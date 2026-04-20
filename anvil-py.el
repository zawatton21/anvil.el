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
    "py-surrounding-form")
  "Stable list of MCP tool ids registered by `anvil-py-enable'.")

(defun anvil-py--register-tools ()
  "Register the Phase 1a py-* MCP tools on `anvil-py--server-id'."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-list-imports)
   :id "py-list-imports"
   :server-id anvil-py--server-id
   :description "List every `import' / `from X import Y' statement in a
Python file.  Returns an ordered list of (:kind :text :bounds) plists,
one per statement, text and source range preserved."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-list-functions)
   :id "py-list-functions"
   :server-id anvil-py--server-id
   :description "List every function definition in a Python file,
including async def and decorated def.  Returns (:kind :name :decorated
:async :bounds) per function.  Includes nested defs."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-list-classes)
   :id "py-list-classes"
   :server-id anvil-py--server-id
   :description "List every class definition in a Python file.  Returns
(:kind :name :decorated :async :bounds) per class."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-list-methods)
   :id "py-list-methods"
   :server-id anvil-py--server-id
   :description "List methods defined directly inside the class named
CLASS-NAME.  Nested classes' methods are excluded; only the first-level
body of a class with that exact name matches."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-list-decorators)
   :id "py-list-decorators"
   :server-id anvil-py--server-id
   :description "List every @decorator usage in a Python file with its
target function / class name.  Multi-decorator targets yield one entry
per decorator, all pointing at the same target."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-find-definition)
   :id "py-find-definition"
   :server-id anvil-py--server-id
   :description "Find the first function or class named NAME in a Python
file.  When both a function and a class share the name, the first in
source order wins; use `py-list-functions' / `py-list-classes' for
exhaustive results."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-py--tool-surrounding-form)
   :id "py-surrounding-form"
   :server-id anvil-py--server-id
   :description "Return the innermost function or class whose source
range contains the 1-based buffer POINT.  KIND restricts the match to
`function' or `class'; empty / nil matches either."
   :read-only t))

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
