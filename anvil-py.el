;;; anvil-py.el --- Python structural locators via tree-sitter  -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2026 zawatton

;;; Commentary:

;; Phase 1a of Doc 21 (docs/design/21-treesit-edits.org).  Provides
;; read-only structural locators for Python source files, built on
;; the anvil-treesit shared core (tree-sitter-python grammar).  All
;; tools here are read-only; edit primitives land in Phase 2.
;;
;; Surface (elisp):
;;   (anvil-py-list-imports    FILE)
;;   (anvil-py-list-functions  FILE)
;;   (anvil-py-list-classes    FILE)
;;   (anvil-py-list-methods    FILE CLASS-NAME)
;;   (anvil-py-list-decorators FILE)
;;   (anvil-py-find-definition FILE NAME)
;;   (anvil-py-surrounding-form FILE POINT &key kind)
;;
;; All queries are compiled once and cached via
;; `anvil-treesit-compile-query'.  When the tree-sitter-python grammar
;; is not installed, the handlers signal a structured `grammar-missing'
;; error rather than a backtrace so Claude can relay the install hint.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'anvil-server)
(require 'anvil-treesit)

(defconst anvil-py--server-id "emacs-eval"
  "Server ID under which py-* MCP tools are registered.")

(defconst anvil-py--lang 'python)

;;;; --- queries ------------------------------------------------------------

(defconst anvil-py--queries
  '((functions
     . "(function_definition) @fn")
    (classes
     . "(class_definition) @cls")
    (imports
     . "[(import_statement) @im
        (import_from_statement) @im
        (future_import_statement) @im]")
    (decorators
     . "(decorated_definition
         (decorator) @deco
         [(function_definition)
          (class_definition)]) @whole")
    (methods
     . "(function_definition) @fn"))
  "Tree-sitter query source strings keyed by operation symbol.
Each value is a query source; the compiled form is cached in
`anvil-treesit--query-cache' under (python . OP).

Queries capture only the whole definition node.  The collector
extracts the name from the `name' field and climbs to a
`decorated_definition' parent when present.  This avoids the
cross-pattern ordering hazard of returning both `@name' and `@fn'
captures — `treesit-query-capture' returns captures in tree-order,
not pattern-pair order, so nested definitions interleave captures
in a way that breaks naive pair-grouping state machines.")

(defun anvil-py--query (op)
  "Return the compiled tree-sitter query for OP."
  (let ((src (alist-get op anvil-py--queries)))
    (unless src
      (error "anvil-py: unknown query op %S" op))
    (anvil-treesit-compile-query anvil-py--lang op src)))

;;;; --- capture helpers ----------------------------------------------------

(defun anvil-py--captures-grouped (root query)
  "Run QUERY on ROOT and return captures grouped by match.
Emacs `treesit-query-capture' returns a flat list of (NAME . NODE)
pairs.  This walks the flat list and groups consecutive captures
into per-match alists, which is what the locator body wants for
\"this @fn was named @name\" correlations."
  (let ((all (treesit-query-capture root query))
        groups current seen-names)
    (dolist (cap all)
      (let ((name (car cap)))
        (when (memq name seen-names)
          (when current (push (nreverse current) groups))
          (setq current nil seen-names nil))
        (push name seen-names)
        (push cap current)))
    (when current (push (nreverse current) groups))
    (nreverse groups)))

;;;; --- public locators ----------------------------------------------------

(defun anvil-py-list-imports (file)
  "Return a list of plists describing imports in FILE.
Each entry: (:kind 'import|from :text STR :bounds PLIST)."
  (anvil-treesit-with-root file anvil-py--lang root
    (let ((q (anvil-py--query 'imports))
          results)
      (dolist (cap (treesit-query-capture root q))
        (let* ((node (cdr cap))
               (kind (pcase (treesit-node-type node)
                       ("import_statement" 'import)
                       ("import_from_statement" 'from)
                       ("future_import_statement" 'future)
                       (_ 'other))))
          (push (list :kind kind
                      :text (anvil-treesit-node-text node)
                      :bounds (anvil-treesit-node-bounds node))
                results)))
      (nreverse results))))

(defun anvil-py--async-node-p (function-def-node)
  "Return non-nil when FUNCTION-DEF-NODE is an `async def'.
The python grammar marks async defs either via an `async' field or
via an anonymous `async' keyword child node depending on grammar
version — check both."
  (or (treesit-node-child-by-field-name function-def-node "async")
      (let ((c0 (treesit-node-child function-def-node 0)))
        (and c0 (string= (treesit-node-text c0 t) "async")))))

(defun anvil-py--outer-node (inner)
  "Return INNER's `decorated_definition' parent, or INNER itself."
  (let ((p (treesit-node-parent inner)))
    (if (and p (string= (treesit-node-type p) "decorated_definition"))
        p
      inner)))

(defun anvil-py--node-name (def-node)
  "Return the name text of a function_definition / class_definition node.
Reads the `name' field directly, returning empty string if missing."
  (let ((n (treesit-node-child-by-field-name def-node "name")))
    (if n (anvil-treesit-node-text n) "")))

(defun anvil-py--enclosing-class-name (fn-node)
  "Return the name of the class directly containing FN-NODE, or nil.
Climbs past `decorated_definition', `block', and `class_definition'
as needed.  Returns nil if FN-NODE is not a direct method of any
class (e.g. a module-level function, or a nested def inside another
function)."
  (let* ((outer (anvil-py--outer-node fn-node))
         (parent (treesit-node-parent outer)))
    (when (and parent (string= (treesit-node-type parent) "block"))
      (let ((grand (treesit-node-parent parent)))
        (when (and grand (string= (treesit-node-type grand) "class_definition"))
          (anvil-py--node-name grand))))))

(defun anvil-py--collect-named (root op kind-sym)
  "Collect one plist per definition captured by OP.
Each capture's node is a bare `function_definition' or
`class_definition'; the collector climbs to a `decorated_definition'
parent when present so :bounds includes the decorator block.  The
name is read from the node's `name' field — reliable regardless of
tree-traversal order for nested defs.

`:class-name' is set to the enclosing class's name when the def is
a direct method of a class (a nested function inside a function
keeps :class-name nil — the enclosing class relationship only
crosses one level of block).  This lets callers of
`anvil-py-find-definition' / `anvil-py-list-functions' distinguish
methods from top-level defs without a second walk."
  (let ((q (anvil-py--query op))
        results)
    (dolist (cap (treesit-query-capture root q))
      (let* ((inner (cdr cap))
             (outer (anvil-py--outer-node inner))
             (decorated (not (eq outer inner)))
             (async (and (eq kind-sym 'function)
                         (anvil-py--async-node-p inner)))
             (class-name (and (eq kind-sym 'function)
                              (anvil-py--enclosing-class-name inner))))
        (push (list :kind kind-sym
                    :name (anvil-py--node-name inner)
                    :class-name class-name
                    :decorated decorated
                    :async (and async t)
                    :bounds (anvil-treesit-node-bounds outer))
              results)))
    (nreverse results)))

(defun anvil-py-list-functions (file)
  "Return a list of plists describing function definitions in FILE.
Each entry: (:kind 'function :name STR :decorated BOOL :async BOOL
:bounds PLIST).  Includes both top-level and nested defs, and both
bare def and async def."
  (anvil-treesit-with-root file anvil-py--lang root
    (anvil-py--collect-named root 'functions 'function)))

(defun anvil-py-list-classes (file)
  "Return a list of plists describing class definitions in FILE.
Each entry: (:kind 'class :name STR :decorated BOOL :async BOOL
:bounds PLIST).  The :async key is always nil for classes, kept for
plist shape parity with functions."
  (anvil-treesit-with-root file anvil-py--lang root
    (anvil-py--collect-named root 'classes 'class)))

(defun anvil-py-list-methods (file class-name)
  "Return methods defined directly inside class CLASS-NAME in FILE.
Each entry: (:kind 'method :class-name STR :name STR :decorated BOOL
:async BOOL :bounds PLIST).  Nested classes' methods are excluded;
only the first-level body of a class named CLASS-NAME matches.
Decorator lines above a method are included in :bounds via the
`decorated_definition' climb, matching `anvil-py-list-functions'."
  (anvil-treesit-with-root file anvil-py--lang root
    (let ((q (anvil-py--query 'methods))
          results)
      (dolist (cap (treesit-query-capture root q))
        (when (eq (car cap) 'fn)
          (let* ((inner (cdr cap))
                 (cls (anvil-py--enclosing-class-name inner)))
            (when (and cls (string= cls class-name))
              (let* ((outer (anvil-py--outer-node inner))
                     (decorated (not (eq outer inner)))
                     (async (anvil-py--async-node-p inner)))
                (push (list :kind 'method
                            :class-name cls
                            :name (anvil-py--node-name inner)
                            :decorated decorated
                            :async (and async t)
                            :bounds (anvil-treesit-node-bounds outer))
                      results))))))
      (nreverse results))))

(defun anvil-py--decorated-target (whole-node)
  "Return (:name :kind :bounds) describing WHOLE-NODE's decorated target.
WHOLE-NODE is a `decorated_definition' capture."
  (let* ((def (treesit-node-child-by-field-name whole-node "definition"))
         ;; Fallback: grammar versions that don't expose the field —
         ;; find the first non-decorator child.
         (def (or def
                  (let (found)
                    (dolist (c (treesit-node-children whole-node t))
                      (unless (or found
                                  (string= (treesit-node-type c) "decorator"))
                        (setq found c)))
                    found)))
         (kind (pcase (and def (treesit-node-type def))
                 ("function_definition" 'function)
                 ("class_definition" 'class)
                 (_ 'other))))
    (list :name (if def (anvil-py--node-name def) "")
          :kind kind
          :bounds (and def (anvil-treesit-node-bounds def)))))

(defun anvil-py-list-decorators (file)
  "Return a list of plists describing @decorator usages in FILE.
Each entry: (:decorator STR :target STR :target-kind 'function|'class
:bounds PLIST :target-bounds PLIST :whole-bounds PLIST).  A single
decorated def with multiple @-lines yields one entry per decorator,
all pointing at the same target."
  (anvil-treesit-with-root file anvil-py--lang root
    (let ((q (anvil-py--query 'decorators))
          ;; Collect one decorated_definition node per @whole capture.
          wholes)
      (dolist (cap (treesit-query-capture root q))
        (when (eq (car cap) 'whole)
          (cl-pushnew (cdr cap) wholes :test #'equal)))
      (let (results)
        (dolist (whole (nreverse wholes))
          (let* ((target (anvil-py--decorated-target whole))
                 (whole-bounds (anvil-treesit-node-bounds whole))
                 ;; Each decorator child of `whole' gets its own entry.
                 (decos (cl-remove-if-not
                         (lambda (c)
                           (string= (treesit-node-type c) "decorator"))
                         (treesit-node-children whole t))))
            (dolist (d decos)
              (push (list :decorator (anvil-treesit-node-text d)
                          :target (plist-get target :name)
                          :target-kind (plist-get target :kind)
                          :bounds (anvil-treesit-node-bounds d)
                          :target-bounds (plist-get target :bounds)
                          :whole-bounds whole-bounds)
                    results))))
        (nreverse results)))))

(defun anvil-py-find-definition (file name)
  "Return the first function or class named NAME in FILE, or nil.
Shape: (:kind 'function|'class :name STR :decorated BOOL :async BOOL
:bounds PLIST).  When both a function and a class share a name, the
first one encountered in source order wins — caller can use
`anvil-py-list-functions' / `-list-classes' for exhaustive results."
  (cl-some (lambda (entry)
             (and (string= (plist-get entry :name) name) entry))
           (append (anvil-py-list-functions file)
                   (anvil-py-list-classes file))))

(cl-defun anvil-py-surrounding-form (file point &key kind)
  "Return the enclosing def / class containing POINT in FILE.
KIND (optional) restricts to `function' or `class'; nil matches either.
Returns nil when POINT is outside any def / class.  Shape:
(:kind 'function|'class :name STR :decorated BOOL :async BOOL
:bounds PLIST)."
  (let* ((fns (anvil-py-list-functions file))
         (cls (anvil-py-list-classes file))
         (candidates (pcase kind
                       ('function fns)
                       ('class cls)
                       (_ (append fns cls))))
         (enclosing (cl-remove-if-not
                     (lambda (e)
                       (let* ((b (plist-get e :bounds))
                              (s (plist-get b :start))
                              (en (plist-get b :end)))
                         (and s en (<= s point en))))
                     candidates)))
    ;; The innermost enclosing form has the latest :start — tree-sitter
    ;; captures arrive in document order but nesting makes the child's
    ;; :start strictly greater than the parent's, so max-by :start picks
    ;; the tightest wrap.
    (when enclosing
      (car (sort enclosing
                 (lambda (a b)
                   (> (plist-get (plist-get a :bounds) :start)
                      (plist-get (plist-get b :bounds) :start))))))))

;;;; --- edit helpers (Phase 2a) --------------------------------------------

(defun anvil-py--import-insertion-point (root)
  "Return the 1-based point after which new imports should be inserted.
Picks the end of the last existing import statement, or the start
of the first non-docstring / non-future-import statement if no
regular imports are present."
  (let ((last-import-end nil)
        (first-non-import-start nil)
        (c (treesit-node-child root 0)))
    (while c
      (let ((type (treesit-node-type c)))
        (pcase type
          ((or "import_statement" "import_from_statement" "future_import_statement")
           (setq last-import-end (treesit-node-end c)))
          ("expression_statement"
           ;; Module docstring is an expression_statement with a string
           ;; child — skip it if it's the very first node.
           (unless (or last-import-end first-non-import-start)
             (let ((inner (treesit-node-child c 0)))
               (unless (and inner
                            (string= (treesit-node-type inner) "string"))
                 (setq first-non-import-start (treesit-node-start c))))))
          (_ (unless first-non-import-start
               (setq first-non-import-start (treesit-node-start c))))))
      (setq c (treesit-node-next-sibling c)))
    (or last-import-end
        first-non-import-start
        ;; Empty file.
        (point-min))))

(defun anvil-py--from-import-names (node)
  "Return the list of imported names in a `from X import ...' NODE.
Each entry is the raw source text of a `dotted_name' / `aliased_import'
child — aliases such as \"Workbook as W\" are preserved intact.  The
module's own `dotted_name' (reached via the `module_name' field) is
filtered out by start-position comparison, which works even when the
tree-sitter node wrapper produces a fresh object per field lookup."
  (let* ((mod-node (treesit-node-child-by-field-name node "module_name"))
         (mod-start (and mod-node (treesit-node-start mod-node)))
         names)
    (dolist (c (treesit-node-children node t))
      (let ((type (treesit-node-type c)))
        (when (and (member type '("dotted_name" "aliased_import"))
                   (not (and mod-start
                             (= (treesit-node-start c) mod-start))))
          (push (anvil-treesit-node-text c) names))))
    (nreverse names)))

(defun anvil-py--from-import-module (node)
  "Return the module name of a `from X import ...' NODE as a string."
  (let ((m (treesit-node-child-by-field-name node "module_name")))
    (and m (anvil-treesit-node-text m))))

(defun anvil-py--find-from-import (root module)
  "Return the first `from MODULE import ...' node in ROOT, or nil."
  (let ((q (anvil-py--query 'imports))
        found)
    (dolist (cap (treesit-query-capture root q))
      (unless found
        (let ((n (cdr cap)))
          (when (and (string= (treesit-node-type n) "import_from_statement")
                     (string= (anvil-py--from-import-module n) module))
            (setq found n)))))
    found))

(defun anvil-py--find-import (root module &optional alias)
  "Return the first `import MODULE[ as ALIAS]' node in ROOT, or nil."
  (let ((q (anvil-py--query 'imports))
        found)
    (dolist (cap (treesit-query-capture root q))
      (unless found
        (let ((n (cdr cap)))
          (when (string= (treesit-node-type n) "import_statement")
            (dolist (c (treesit-node-children n t))
              (let ((type (treesit-node-type c)))
                (cond
                 ((and (null alias)
                       (string= type "dotted_name")
                       (string= (anvil-treesit-node-text c) module))
                  (setq found n))
                 ((and alias
                       (string= type "aliased_import"))
                  (let* ((name-node (treesit-node-child-by-field-name
                                     c "name"))
                         (alias-node (treesit-node-child-by-field-name
                                      c "alias")))
                    (when (and name-node alias-node
                               (string= (anvil-treesit-node-text name-node)
                                        module)
                               (string= (anvil-treesit-node-text alias-node)
                                        alias))
                      (setq found n)))))))))))
    found))

(defun anvil-py--render-from-import (module names)
  "Render `from MODULE import NAME, NAME, ...' for NAMES (list of strings).
NAMES is used in the order given — callers decide sort."
  (format "from %s import %s" module
          (mapconcat #'identity names ", ")))

(defun anvil-py--render-import (module &optional alias)
  "Render `import MODULE' or `import MODULE as ALIAS'."
  (if alias
      (format "import %s as %s" module alias)
    (format "import %s" module)))

(defun anvil-py--spec-kind (spec)
  "Return the :kind of SPEC, defaulting to `import' when omitted."
  (or (plist-get spec :kind) 'import))

(cl-defun anvil-py--plan-add-import (file spec)
  "Build an edit plan adding SPEC to FILE.
Returns a plan plist (see `anvil-treesit-make-plan').  When SPEC is
already satisfied (bare import present, or every requested
from-import name already present), returns a no-op plan."
  (anvil-treesit-with-root file anvil-py--lang root
    (pcase (anvil-py--spec-kind spec)
      ('from
       (let* ((module (plist-get spec :from))
              (wanted (plist-get spec :names))
              (existing (anvil-py--find-from-import root module)))
         (unless (and module wanted)
           (user-error "anvil-py-add-import: :from and :names required for from-import"))
         (if existing
             (let* ((current (anvil-py--from-import-names existing))
                    (current-bare (mapcar (lambda (n)
                                            (car (split-string n " as ")))
                                          current))
                    (missing (cl-remove-if
                              (lambda (w)
                                (member (car (split-string w " as "))
                                        current-bare))
                              wanted)))
               (if (null missing)
                   (anvil-treesit-make-noop-plan
                    file (format "add-import from %s" module))
                 (let* ((merged (append current missing))
                        (new-text (anvil-py--render-from-import
                                   module merged))
                        (beg (treesit-node-start existing))
                        (end (treesit-node-end existing)))
                   (anvil-treesit-make-plan
                    file beg end new-text
                    (format "add-import from %s (merge %d name%s)"
                            module (length missing)
                            (if (= 1 (length missing)) "" "s"))))))
           ;; No existing from-import for this module — insert a new one.
           (let* ((ins (anvil-py--import-insertion-point root))
                  (new-line (concat (if (> ins (point-min)) "\n" "")
                                    (anvil-py--render-from-import
                                     module wanted)
                                    (if (= ins (point-min)) "\n" ""))))
             (anvil-treesit-make-plan
              file ins ins new-line
              (format "add-import from %s (new)" module))))))
      ('import
       (let* ((module (plist-get spec :module))
              (alias (plist-get spec :alias))
              (existing (anvil-py--find-import root module alias)))
         (unless module
           (user-error "anvil-py-add-import: :module required for import"))
         (if existing
             (anvil-treesit-make-noop-plan
              file (format "add-import import %s" module))
           (let* ((ins (anvil-py--import-insertion-point root))
                  (new-line (concat (if (> ins (point-min)) "\n" "")
                                    (anvil-py--render-import module alias)
                                    (if (= ins (point-min)) "\n" ""))))
             (anvil-treesit-make-plan
              file ins ins new-line
              (format "add-import import %s" module)))))))))

(cl-defun anvil-py--plan-remove-import (file spec)
  "Build an edit plan removing SPEC from FILE.
For `:kind 'from', removes only the requested names; if that leaves
the statement empty, the whole statement is deleted.  For `:kind
'import', removes the whole statement when module (and alias, if
specified) matches.  No-op when the import is already absent."
  (anvil-treesit-with-root file anvil-py--lang root
    (pcase (anvil-py--spec-kind spec)
      ('from
       (let* ((module (plist-get spec :from))
              (wanted (plist-get spec :names))
              (existing (anvil-py--find-from-import root module)))
         (unless (and module wanted)
           (user-error "anvil-py-remove-import: :from and :names required"))
         (if (null existing)
             (anvil-treesit-make-noop-plan
              file (format "remove-import from %s" module))
           (let* ((current (anvil-py--from-import-names existing))
                  (kept (cl-remove-if
                         (lambda (n)
                           (member (car (split-string n " as "))
                                   wanted))
                         current))
                  (beg (treesit-node-start existing))
                  (end (treesit-node-end existing)))
             (cond
              ((equal current kept)
               (anvil-treesit-make-noop-plan
                file (format "remove-import from %s" module)))
              ((null kept)
               ;; Drop the whole statement — also eat the trailing newline.
               (let ((end+1 (with-temp-buffer
                              (insert-file-contents file)
                              (goto-char end)
                              (if (eq (char-after) ?\n) (1+ end) end))))
                 (anvil-treesit-make-plan
                  file beg end+1 ""
                  (format "remove-import from %s (drop whole statement)"
                          module))))
              (t
               (anvil-treesit-make-plan
                file beg end
                (anvil-py--render-from-import module kept)
                (format "remove-import from %s (drop %d name%s)"
                        module
                        (- (length current) (length kept))
                        (if (= 1 (- (length current) (length kept)))
                            "" "s")))))))))
      ('import
       (let* ((module (plist-get spec :module))
              (alias (plist-get spec :alias))
              (existing (anvil-py--find-import root module alias)))
         (unless module
           (user-error "anvil-py-remove-import: :module required"))
         (if (null existing)
             (anvil-treesit-make-noop-plan
              file (format "remove-import import %s" module))
           (let* ((beg (treesit-node-start existing))
                  (end (treesit-node-end existing))
                  (end+1 (with-temp-buffer
                           (insert-file-contents file)
                           (goto-char end)
                           (if (eq (char-after) ?\n) (1+ end) end))))
             (anvil-treesit-make-plan
              file beg end+1 ""
              (format "remove-import import %s" module)))))))))

;;;; --- wrap-expr helpers (Phase 2c) ---------------------------------------

(defconst anvil-py-wrap-placeholder "|anvil-hole|"
  "Token substituted with the wrapped text inside a `py-wrap-expr' wrapper.")

(defun anvil-py--count-substring (needle haystack)
  "Return the number of non-overlapping occurrences of NEEDLE in HAYSTACK."
  (if (string-empty-p needle)
      0
    (let ((i 0) (n 0) (len (length needle)))
      (while (and (< i (length haystack))
                  (setq i (string-match-p (regexp-quote needle) haystack i)))
        (cl-incf n)
        (setq i (+ i len)))
      n)))

(defun anvil-py--validate-wrapper (wrapper)
  "Signal a user-error unless WRAPPER has exactly one placeholder.
Returns WRAPPER unchanged when valid."
  (let ((n (anvil-py--count-substring anvil-py-wrap-placeholder wrapper)))
    (cond
     ((= n 0)
      (user-error "anvil-py-wrap-expr: wrapper missing `%s' placeholder"
                  anvil-py-wrap-placeholder))
     ((> n 1)
      (user-error "anvil-py-wrap-expr: wrapper has %d `%s' placeholders \
(want exactly 1)" n anvil-py-wrap-placeholder)))
    wrapper))

(defun anvil-py--node-at-exact-range (start end)
  "Return the tree-sitter node whose range is exactly [START, END), or nil.
Walks from the smallest covering node outward until the bounds match;
if no node has that exact range we return nil so the caller can
surface a precise error rather than accepting a misaligned wrap."
  (let ((n (treesit-node-on start end 'python)))
    (catch 'found
      (while n
        (when (and (= (treesit-node-start n) start)
                   (= (treesit-node-end n) end))
          (throw 'found n))
        (setq n (treesit-node-parent n)))
      nil)))

;;;; --- rename-import helpers (Phase 2c) -----------------------------------

(defun anvil-py--split-name-alias (entry)
  "Return (cons BARE-NAME ALIAS-OR-NIL) for a from-import entry string.
An ENTRY of \"X\" yields (\"X\" . nil), \"X as Y\" yields (\"X\" . \"Y\")."
  (let ((parts (split-string entry "[ \t]+as[ \t]+")))
    (cons (car parts) (cadr parts))))

(defun anvil-py--render-name-entry (bare alias)
  "Render a single from-import entry from BARE name and optional ALIAS."
  (if (and alias (not (string-empty-p alias)))
      (format "%s as %s" bare alias)
    bare))

(defun anvil-py--find-bare-import-node (root module)
  "Return the bare `import MODULE [as X]' statement node in ROOT, or nil.
Matches on the module dotted_name regardless of whether it currently
carries an alias."
  (let ((q (anvil-py--query 'imports))
        found)
    (dolist (cap (treesit-query-capture root q))
      (unless found
        (let ((n (cdr cap)))
          (when (string= (treesit-node-type n) "import_statement")
            (dolist (c (treesit-node-children n t))
              (pcase (treesit-node-type c)
                ("dotted_name"
                 (when (string= (anvil-treesit-node-text c) module)
                   (setq found n)))
                ("aliased_import"
                 (let ((nm (treesit-node-child-by-field-name c "name")))
                   (when (and nm (string= (anvil-treesit-node-text nm)
                                          module))
                     (setq found n))))))))))
    found))

;;;; --- replace-function helpers (Phase 2b) -------------------------------

(defun anvil-py--line-beginning-at (point)
  "Return the 1-based point of the line start that contains POINT.
Does not modify point or match data."
  (save-excursion (goto-char point) (line-beginning-position)))

(defun anvil-py--common-leading-whitespace (lines)
  "Return the longest whitespace prefix common to every non-empty LINE.
LINES is a list of strings; empty / whitespace-only lines are
excluded from the comparison because `textwrap.dedent' semantics
ignore them.  Returns an empty string if no non-empty line exists."
  (let ((candidates (cl-remove-if
                     (lambda (l) (string-empty-p (string-trim l)))
                     lines)))
    (if (null candidates)
        ""
      (cl-reduce
       (lambda (a b)
         (let ((i 0) (len (min (length a) (length b))))
           (while (and (< i len) (eq (aref a i) (aref b i)))
             (cl-incf i))
           (substring a 0 i)))
       (mapcar (lambda (l)
                 (if (string-match "\\`[ \t]*" l)
                     (match-string 0 l)
                   ""))
               candidates)))))

(defun anvil-py--dedent (text)
  "Strip the common leading whitespace from every line of TEXT.
Matches Python's `textwrap.dedent' semantics — returns TEXT with
the longest whitespace prefix shared by all non-empty lines removed
from every line.  Pure; TEXT is not modified."
  (let* ((lines (split-string text "\n"))
         (common (anvil-py--common-leading-whitespace lines)))
    (if (string-empty-p common)
        text
      (mapconcat (lambda (l)
                   (if (string-prefix-p common l)
                       (substring l (length common))
                     l))
                 lines "\n"))))

(defun anvil-py--reindent (text indent)
  "Prepend INDENT to every non-empty line of TEXT.
Empty / whitespace-only lines stay as-is so replacement blocks don't
grow trailing whitespace that editors would flag."
  (if (string-empty-p indent)
      text
    (mapconcat (lambda (l)
                 (if (string-empty-p (string-trim l))
                     l
                   (concat indent l)))
               (split-string text "\n")
               "\n")))

(defun anvil-py--find-replace-target (root name class-filter)
  "Return the function_definition node matching NAME in ROOT.
When CLASS-FILTER is a string, only methods whose enclosing class's
name matches it are considered.  Signals a `user-error' when no
match exists or when the result is ambiguous without a class
filter.  CLASS-FILTER may be nil (no filter) or a string."
  (let ((q (anvil-py--query 'functions))
        candidates)
    (dolist (cap (treesit-query-capture root q))
      (let* ((inner (cdr cap))
             (nm (anvil-py--node-name inner)))
        (when (string= nm name)
          (let ((cls (anvil-py--enclosing-class-name inner)))
            (when (or (null class-filter)
                      (and cls (string= cls class-filter)))
              (push (cons cls inner) candidates))))))
    (cond
     ((null candidates)
      (user-error
       "anvil-py-replace-function: no def named %S%s"
       name (if class-filter (format " in class %S" class-filter) "")))
     ((and (null class-filter) (> (length candidates) 1))
      (user-error
       "anvil-py-replace-function: ambiguous %S (matches: %s); \
pass :class to select"
       name
       (mapconcat (lambda (c)
                    (format "%s" (or (car c) "<top-level>")))
                  candidates ", ")))
     (t (cdar (last candidates))))))  ; candidates were pushed reverse

;;;; --- public edit API (Phase 2a) -----------------------------------------

(cl-defun anvil-py-add-import (file spec &key apply)
  "Add the import specified by SPEC to FILE.
SPEC is a plist:
  (:kind 'from :from MODULE :names (NAME ...))  — `from X import Y, Z'
  (:kind 'import :module MODULE [:alias NAME]) — `import X [as Y]'

Idempotent: if SPEC is already satisfied, returns a no-op plan.
Merges into an existing `from X import ...' when :kind is `from' and
MODULE already has a statement — the new NAMES are appended, de-duped
by bare name (so `X' and `X as Y' both count as having `X').

Returns the edit plan unless APPLY is truthy, in which case the plan
is applied via `anvil-treesit-apply-plan' and the plan with
:applied-at is returned."
  (let ((plan (anvil-py--plan-add-import file spec)))
    (if (anvil-treesit-truthy apply)
        (anvil-treesit-apply-plan plan)
      plan)))

(cl-defun anvil-py-remove-import (file spec &key apply)
  "Remove the import specified by SPEC from FILE.
SPEC shape matches `anvil-py-add-import'.  For `from'-imports,
only the requested NAMES are removed; if the statement becomes
empty, the whole statement is deleted (including its newline).
Idempotent: a missing import returns a no-op plan.

Returns the plan unless APPLY is truthy."
  (let ((plan (anvil-py--plan-remove-import file spec)))
    (if (anvil-treesit-truthy apply)
        (anvil-treesit-apply-plan plan)
      plan)))

(cl-defun anvil-py--plan-wrap-expr (file start end wrapper)
  "Build an edit plan wrapping the expression [START, END) of FILE.
WRAPPER must contain exactly one `|anvil-hole|' placeholder; its
occurrence is replaced by the original text from the file.  Returns
a no-op plan when WRAPPER equals the literal placeholder (wrapping
with nothing is a no-op by definition) and when the computed
replacement matches the current text."
  (anvil-py--validate-wrapper wrapper)
  (anvil-treesit-with-root file anvil-py--lang root
    (unless (and (integerp start) (integerp end) (< start end))
      (user-error "anvil-py-wrap-expr: invalid range (%S . %S)" start end))
    (let* ((node (anvil-py--node-at-exact-range start end))
           (original (buffer-substring-no-properties start end))
           (replacement (replace-regexp-in-string
                         (regexp-quote anvil-py-wrap-placeholder)
                         original wrapper t t))
           (reason (format "wrap-expr [%d,%d)%s" start end
                           (if node
                               (format " (%s)" (treesit-node-type node))
                             " (unaligned!)"))))
      (unless node
        (user-error "anvil-py-wrap-expr: range %d-%d does not align with \
a tree-sitter node — wrapping would produce invalid Python"
                    start end))
      (if (string= original replacement)
          (anvil-treesit-make-noop-plan file reason)
        (anvil-treesit-make-plan file start end replacement reason)))))

(cl-defun anvil-py--plan-rename-import (file spec)
  "Build an edit plan renaming an import alias in FILE.
SPEC shapes are documented on `anvil-py-rename-import'.  Signals a
`user-error' when the import does not exist — unlike add/remove,
renaming requires the target to be present.  Returns a no-op plan
when the new alias already matches the current one."
  (anvil-treesit-with-root file anvil-py--lang root
    (pcase (anvil-py--spec-kind spec)
      ('import
       (let* ((module (plist-get spec :module))
              (new-alias (plist-get spec :new-alias))
              (node (and module (anvil-py--find-bare-import-node root module))))
         (unless module
           (user-error "anvil-py-rename-import: :module required"))
         (unless node
           (user-error
            "anvil-py-rename-import: no `import %s' in %s"
            module (file-name-nondirectory file)))
         (let* ((current-text (anvil-treesit-node-text node))
                (new-text (anvil-py--render-import module new-alias))
                (beg (treesit-node-start node))
                (end (treesit-node-end node)))
           (if (string= current-text new-text)
               (anvil-treesit-make-noop-plan
                file (format "rename-import import %s" module))
             (anvil-treesit-make-plan
              file beg end new-text
              (format "rename-import import %s → %s"
                      module (if new-alias (format "as %s" new-alias)
                               "(bare)")))))))
      ('from
       (let* ((from-mod (plist-get spec :from))
              (target-name (plist-get spec :name))
              (new-alias (plist-get spec :new-alias))
              (node (and from-mod
                         (anvil-py--find-from-import root from-mod))))
         (unless (and from-mod target-name)
           (user-error
            "anvil-py-rename-import: :from and :name required for from-import"))
         (unless node
           (user-error
            "anvil-py-rename-import: no `from %s import' in %s"
            from-mod (file-name-nondirectory file)))
         (let* ((entries (anvil-py--from-import-names node))
                (target-idx (cl-position target-name entries
                                         :test (lambda (want entry)
                                                 (string=
                                                  want
                                                  (car (anvil-py--split-name-alias
                                                        entry)))))))
           (unless target-idx
             (user-error
              "anvil-py-rename-import: %s not in `from %s import'"
              target-name from-mod))
           (let* ((current-entry (nth target-idx entries))
                  (new-entry (anvil-py--render-name-entry target-name new-alias)))
             (if (string= current-entry new-entry)
                 (anvil-treesit-make-noop-plan
                  file (format "rename-import from %s:%s"
                               from-mod target-name))
               (let* ((new-entries (copy-sequence entries))
                      (_ (setf (nth target-idx new-entries) new-entry))
                      (new-text (anvil-py--render-from-import
                                 from-mod new-entries))
                      (beg (treesit-node-start node))
                      (end (treesit-node-end node)))
                 (anvil-treesit-make-plan
                  file beg end new-text
                  (format "rename-import from %s: %s → %s"
                          from-mod current-entry new-entry)))))))))))

(cl-defun anvil-py--plan-replace-function (file name new-source class-filter)
  "Build an edit plan replacing the def named NAME in FILE with NEW-SOURCE.
Dedents NEW-SOURCE and reindents to the column of the existing def.
CLASS-FILTER is the enclosing class name or nil.  The swap covers the
function_definition node only — any decorators attached to it survive.
Replaces [line-beginning-position(fn-start), fn-end) so the existing
leading indent is included in the old range and reindented uniformly."
  (anvil-treesit-with-root file anvil-py--lang root
    (let* ((fn (anvil-py--find-replace-target root name class-filter))
           (fn-start (treesit-node-start fn))
           (fn-end (treesit-node-end fn))
           (line-beg (anvil-py--line-beginning-at fn-start))
           (indent (make-string (- fn-start line-beg) ?\s))
           (dedented (anvil-py--dedent new-source))
           (replacement (anvil-py--reindent dedented indent))
           ;; Compare without trailing whitespace differences — an
           ;; idempotent re-apply (same NEW-SOURCE against an already-
           ;; replaced def) should be a no-op.
           (current (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-substring-no-properties line-beg fn-end)))
           (reason (format "replace-function %s%s" name
                           (if class-filter
                               (format " (class %s)" class-filter) ""))))
      (if (string= current replacement)
          (anvil-treesit-make-noop-plan file reason)
        (anvil-treesit-make-plan file line-beg fn-end replacement reason)))))

(cl-defun anvil-py-wrap-expr (file start end wrapper &key apply)
  "Wrap the expression [START, END) of FILE with WRAPPER.
WRAPPER is a source template containing `anvil-py-wrap-placeholder'
\(the literal string \"|anvil-hole|\") exactly once — that token is
replaced with the original expression text.

Example:
  (anvil-py-wrap-expr FILE (cons 120 130) \"cached(|anvil-hole|)\")
    turns `compute()' at columns 120-130 into `cached(compute())'.

Safety: the range must align to an exact tree-sitter node boundary;
misaligned ranges signal a user-error rather than produce invalid
Python.  WRAPPER must contain the placeholder exactly once.

Returns the plan unless APPLY is truthy.  :apply t writes via
`anvil-treesit-apply-plan' and returns the plan with :applied-at."
  (let ((plan (anvil-py--plan-wrap-expr file start end wrapper)))
    (if (anvil-treesit-truthy apply)
        (anvil-treesit-apply-plan plan)
      plan)))

(cl-defun anvil-py-rename-import (file spec &key apply)
  "Rename the alias of an existing import in FILE.
SPEC is a plist:
  (:kind 'import :module MODULE :new-alias STR-OR-NIL)
    — rewrite `import MODULE [as OLD]' to `import MODULE [as NEW]';
      :new-alias nil drops the alias (makes it bare).
  (:kind 'from :from MODULE :name NAME :new-alias STR-OR-NIL)
    — rewrite the single NAME entry inside `from MODULE import ...'.
      Other names in the statement are preserved verbatim.

Signals a `user-error' when the target import does not exist — this
is an edit, not a create.  Returns a no-op plan when the new alias
already matches the current one.  This is *not* a reference rename:
the identifier's use sites in the code body are not touched (that
needs Phase 3).

Returns the edit plan unless APPLY is truthy, in which case the
plan is applied via `anvil-treesit-apply-plan' and returned with
:applied-at."
  (let ((plan (anvil-py--plan-rename-import file spec)))
    (if (anvil-treesit-truthy apply)
        (anvil-treesit-apply-plan plan)
      plan)))

(cl-defun anvil-py-replace-function (file name new-source &key class apply)
  "Replace the body of the def named NAME in FILE with NEW-SOURCE.
NEW-SOURCE is the full def including signature — e.g.
  \"def foo(a, b):\\n    return a + b\"
and is interpreted as column-0 source: it is dedented (common
leading whitespace stripped) and reindented to match the column of
the existing def, so the caller need not know whether they are
replacing a top-level function or a method.

CLASS (keyword) selects among methods when multiple defs share the
same name.  Without CLASS, a unique name is required — ambiguous
names signal a user-error listing the candidates.

Decorators attached to the def are preserved — only the
function_definition node itself is swapped.  A double-apply with
the identical NEW-SOURCE is a no-op.

Returns the edit plan unless APPLY is truthy, in which case the plan
is applied via `anvil-treesit-apply-plan' and the plan with
:applied-at is returned."
  (let ((plan (anvil-py--plan-replace-function
               file name new-source
               (and class (if (symbolp class) (symbol-name class) class)))))
    (if (anvil-treesit-truthy apply)
        (anvil-treesit-apply-plan plan)
      plan)))

;;;; --- MCP handlers -------------------------------------------------------

(defun anvil-py--tool-list-imports (file)
  "MCP wrapper — list Python imports.

MCP Parameters:
  file - absolute path to the .py file to analyze"
  (anvil-server-with-error-handling
   (anvil-py-list-imports file)))

(defun anvil-py--tool-list-functions (file)
  "MCP wrapper — list Python function definitions (includes async def).

MCP Parameters:
  file - absolute path to the .py file to analyze"
  (anvil-server-with-error-handling
   (anvil-py-list-functions file)))

(defun anvil-py--tool-list-classes (file)
  "MCP wrapper — list Python class definitions.

MCP Parameters:
  file - absolute path to the .py file to analyze"
  (anvil-server-with-error-handling
   (anvil-py-list-classes file)))

(defun anvil-py--tool-list-methods (file class-name)
  "MCP wrapper — list direct methods of CLASS-NAME.

MCP Parameters:
  file       - absolute path to the .py file to analyze
  class-name - class name whose methods to list"
  (anvil-server-with-error-handling
   (anvil-py-list-methods file class-name)))

(defun anvil-py--tool-list-decorators (file)
  "MCP wrapper — list @decorator usages with their targets.

MCP Parameters:
  file - absolute path to the .py file to analyze"
  (anvil-server-with-error-handling
   (anvil-py-list-decorators file)))

(defun anvil-py--tool-find-definition (file name)
  "MCP wrapper — find a function or class by NAME.

MCP Parameters:
  file - absolute path to the .py file to analyze
  name - identifier to find"
  (anvil-server-with-error-handling
   (or (anvil-py-find-definition file name)
       (list :found nil :name name))))

(defun anvil-py--plist-p (x)
  "Return non-nil when X looks like a plist."
  (and (listp x) (keywordp (car-safe x))))

(defun anvil-py--spec-keyword (key)
  "Normalize SPEC KEY into the keyword form used by the planners."
  (intern
   (concat
    ":"
    (replace-regexp-in-string
     "_"
     "-"
     (cond
      ((keywordp key) (substring (symbol-name key) 1))
      ((symbolp key) (symbol-name key))
      ((stringp key) key)
      (t (format "%s" key)))))))

(defun anvil-py--alist-to-plist (alist)
  "Convert ALIST to a plist with normalized keyword keys."
  (let (out)
    (dolist (entry alist)
      (push (anvil-py--spec-keyword (car entry)) out)
      (push (cdr entry) out))
    (nreverse out)))

(defun anvil-py--normalize-plist-keys (plist)
  "Return PLIST with keys normalized for the planners."
  (let (out)
    (while plist
      (push (anvil-py--spec-keyword (pop plist)) out)
      (push (pop plist) out))
    (nreverse out)))

(defun anvil-py--parse-spec-string (spec)
  "Parse string SPEC as JSON or Lisp data and return the resulting object."
  (let ((trimmed (string-trim spec)))
    (unless (string-empty-p trimmed)
      (or
       (condition-case nil
           (json-parse-string trimmed
                              :object-type 'plist
                              :array-type 'list
                              :null-object nil
                              :false-object nil)
         (error nil))
       (condition-case nil
           (pcase-let ((`(,obj . ,idx) (read-from-string trimmed)))
             (when (string-empty-p (string-trim (substring trimmed idx)))
               obj))
         (error nil))
       (user-error "anvil-py: could not parse spec string %S" spec)))))

(defun anvil-py--coerce-spec (spec)
  "Normalize an MCP-bridge SPEC into the elisp plist shape.
MCP tool calls can arrive as a plist, an alist / hash-table decoded
from JSON, or a JSON / Lisp string representation of either.  Return
a plist whose values are ready for the elisp planners."
  (let* ((s (cond
             ((null spec) nil)
             ((anvil-py--plist-p spec)
              (anvil-py--normalize-plist-keys (copy-sequence spec)))
             ((hash-table-p spec)
              (let (out)
                (maphash (lambda (k v)
                           (push (cons k v) out))
                         spec)
                (anvil-py--alist-to-plist (nreverse out))))
             ((and (listp spec) (consp spec) (consp (car spec)))
              (anvil-py--alist-to-plist spec))
             ((stringp spec)
              (anvil-py--coerce-spec (anvil-py--parse-spec-string spec)))
             ((listp spec)
              (copy-sequence spec))
             (t
              (user-error "anvil-py: unsupported spec shape %S" spec))))
         (kind (plist-get s :kind))
         (names (plist-get s :names)))
    (when (stringp kind)
      (setq s (plist-put s :kind (intern kind))))
    (when (vectorp names)
      (setq s (plist-put s :names (append names nil)))
      (setq names (plist-get s :names)))
    (when (stringp names)
      (setq s (plist-put s :names
                         (split-string names "[ ,]+" t))))
    s))

(defun anvil-py--tool-add-import (file spec apply)
  "MCP wrapper — add an import.

MCP Parameters:
  file  - absolute path to the .py file to edit
  spec  - plist describing the import (see `anvil-py-add-import')
  apply - truthy to write; default returns a preview plan"
  (anvil-server-with-error-handling
   (anvil-py-add-import file (anvil-py--coerce-spec spec) :apply apply)))

(defun anvil-py--tool-remove-import (file spec apply)
  "MCP wrapper — remove an import.

MCP Parameters:
  file  - absolute path to the .py file to edit
  spec  - plist describing the import (see `anvil-py-remove-import')
  apply - truthy to write; default returns a preview plan"
  (anvil-server-with-error-handling
   (anvil-py-remove-import file (anvil-py--coerce-spec spec) :apply apply)))

(defun anvil-py--tool-wrap-expr (file start end wrapper apply)
  "MCP wrapper — wrap an expression.

MCP Parameters:
  file    - absolute path to the .py file to edit
  start   - 1-based buffer point at the start of the expression
  end     - 1-based buffer point at the end of the expression
  wrapper - source template containing `|anvil-hole|' exactly once
  apply   - truthy to write; default returns a preview plan"
  (anvil-server-with-error-handling
   (let ((s (if (stringp start) (string-to-number start) start))
         (e (if (stringp end) (string-to-number end) end)))
     (anvil-py-wrap-expr file s e wrapper :apply apply))))

(defun anvil-py--tool-rename-import (file spec apply)
  "MCP wrapper — rename an import alias.

MCP Parameters:
  file  - absolute path to the .py file to edit
  spec  - plist describing the rename (see `anvil-py-rename-import')
  apply - truthy to write; default returns a preview plan"
  (anvil-server-with-error-handling
   (anvil-py-rename-import file (anvil-py--coerce-spec spec) :apply apply)))

(defun anvil-py--tool-replace-function (file name new-source class apply)
  "MCP wrapper — replace a Python function or method.

MCP Parameters:
  file       - absolute path to the .py file to edit
  name       - identifier of the function / method to replace
  new-source - full replacement def (e.g. \"def foo(a): return a\"),
               interpreted as column-0 source and auto-reindented
  class      - optional enclosing class name to disambiguate methods;
               empty / nil means any class (error if ambiguous)
  apply      - truthy to write; default returns a preview plan"
  (anvil-server-with-error-handling
   (let ((cls (cond ((null class) nil)
                    ((and (stringp class) (string-empty-p class)) nil)
                    (t class))))
     (anvil-py-replace-function file name new-source
                                :class cls :apply apply))))

(defun anvil-py--tool-surrounding-form (file point kind)
  "MCP wrapper — return the def / class enclosing POINT.

MCP Parameters:
  file  - absolute path to the .py file to analyze
  point - 1-based buffer point inside the file
  kind  - optional \"function\" or \"class\" to restrict, empty / nil = either"
  (anvil-server-with-error-handling
   (let* ((p (cond ((integerp point) point)
                   ((stringp point) (string-to-number point))
                   (t (user-error "anvil-py: point must be an integer"))))
          (k (pcase kind
               ((pred null) nil)
               ((pred stringp)
                (if (string-empty-p kind) nil (intern kind)))
               ((pred symbolp) kind)
               (_ nil))))
     (or (anvil-py-surrounding-form file p :kind k)
         (list :found nil :point p)))))

;;;; --- module lifecycle ---------------------------------------------------

(defconst anvil-py--tool-ids
  '("py-list-imports"
    "py-list-functions"
    "py-list-classes"
    "py-list-methods"
    "py-list-decorators"
    "py-find-definition"
    "py-surrounding-form"
    "py-add-import"
    "py-remove-import"
    "py-rename-import"
    "py-replace-function"
    "py-wrap-expr")
  "Stable list of MCP tool ids registered by `anvil-py-enable'.")

(defun anvil-py--register-tools ()
  "Register the Phase 1a py-* MCP tools on `anvil-py--server-id'."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-list-imports)
   :id "py-list-imports"
   :intent '(py-read structure)
   :layer 'core
   :server-id anvil-py--server-id
   :description "List every `import' / `from X import Y' statement in a
Python file.  Returns an ordered list of (:kind :text :bounds) plists,
one per statement, text and source range preserved."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-list-functions)
   :id "py-list-functions"
   :intent '(py-read structure)
   :layer 'core
   :server-id anvil-py--server-id
   :description "List every function definition in a Python file,
including async def and decorated def.  Returns (:kind :name :decorated
:async :bounds) per function.  Includes nested defs."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-list-classes)
   :id "py-list-classes"
   :intent '(py-read structure)
   :layer 'core
   :server-id anvil-py--server-id
   :description "List every class definition in a Python file.  Returns
(:kind :name :decorated :async :bounds) per class."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-list-methods)
   :id "py-list-methods"
   :intent '(py-read structure)
   :layer 'core
   :server-id anvil-py--server-id
   :description "List methods defined directly inside the class named
CLASS-NAME.  Nested classes' methods are excluded; only the first-level
body of a class with that exact name matches."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-list-decorators)
   :id "py-list-decorators"
   :intent '(py-read structure)
   :layer 'core
   :server-id anvil-py--server-id
   :description "List every @decorator usage in a Python file with its
target function / class name.  Multi-decorator targets yield one entry
per decorator, all pointing at the same target."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-find-definition)
   :id "py-find-definition"
   :intent '(py-read)
   :layer 'core
   :server-id anvil-py--server-id
   :description "Find the first function or class named NAME in a Python
file.  When both a function and a class share the name, the first in
source order wins; use `py-list-functions' / `py-list-classes' for
exhaustive results."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-surrounding-form)
   :id "py-surrounding-form"
   :intent '(py-read structure)
   :layer 'core
   :server-id anvil-py--server-id
   :description "Return the innermost function or class whose source
range contains the 1-based buffer POINT.  KIND restricts the match to
`function' or `class'; empty / nil matches either."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-add-import)
   :id "py-add-import"
   :intent '(py-edit code-bulk-edit)
   :layer 'core
   :server-id anvil-py--server-id
   :description "Add an import to a Python file.  SPEC is a plist:
(:kind 'from :from MODULE :names (NAME ...)) for `from X import ...',
or (:kind 'import :module MODULE [:alias NAME]) for `import X'.
Idempotent — merges into an existing `from X import ...' for the same
module, and returns a no-op plan when SPEC is already satisfied.
Preview-default; pass :apply t to write via file-batch-across.")

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-remove-import)
   :id "py-remove-import"
   :intent '(py-edit code-bulk-edit)
   :layer 'core
   :server-id anvil-py--server-id
   :description "Remove an import from a Python file.  SPEC shape
matches `py-add-import'.  For `from'-imports, only the requested
names are removed; if the statement is left empty, the whole line
is dropped.  Idempotent: a missing import returns a no-op plan.
Preview-default; pass :apply t to write via file-batch-across.")

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-rename-import)
   :id "py-rename-import"
   :intent '(py-edit code-bulk-edit)
   :layer 'core
   :server-id anvil-py--server-id
   :description "Rename the alias of an existing import statement.
SPEC shape: (:kind 'import :module M :new-alias S-or-nil) for bare
imports, or (:kind 'from :from M :name N :new-alias S-or-nil) for
from-imports (single-name alias rename; other names in the same
statement are preserved).  Errors when the target import is not
present — this is an edit, not a create.  Reference renaming at
use sites is out of scope (Phase 3).  Preview-default.")

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-replace-function)
   :id "py-replace-function"
   :intent '(py-edit code-bulk-edit)
   :layer 'core
   :server-id anvil-py--server-id
   :description "Replace a Python function / method body with
NEW-SOURCE.  NEW-SOURCE is the full def including signature, written
at column 0 — it is auto-dedented and re-indented to the enclosing
column, so the same input works for top-level functions and for
methods.  CLASS disambiguates methods with the same name across
classes; without CLASS, an ambiguous NAME errors with the candidate
list.  Decorators attached to the def are preserved.  Idempotent:
replacing with identical source is a no-op.  Preview-default.")

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-wrap-expr)
   :id "py-wrap-expr"
   :intent '(py-edit code-bulk-edit)
   :layer 'core
   :server-id anvil-py--server-id
   :description "Wrap the expression at [START, END) with WRAPPER.
WRAPPER is a source template containing the placeholder
`|anvil-hole|' exactly once — that token is replaced with the
original expression text from the file.  The range must align to an
exact tree-sitter node boundary, or the tool errors rather than
produce invalid Python.  Example: wrapping `compute()' with
`cached(|anvil-hole|)' yields `cached(compute())'.
Preview-default."))

(defun anvil-py-enable ()
  "Enable the Phase 1a py-* MCP tools."
  (interactive)
  (anvil-py--register-tools))

(defun anvil-py-disable ()
  "Unregister the Phase 1a py-* MCP tools."
  (interactive)
  (dolist (id anvil-py--tool-ids)
    (anvil-server-unregister-tool id anvil-py--server-id)))

(provide 'anvil-py)
;;; anvil-py.el ends here
