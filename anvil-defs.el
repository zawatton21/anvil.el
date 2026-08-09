;;; anvil-defs.el --- SQLite index of elisp definitions and references  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 11 Phase 1 + Phase 2 (filenotify deferred to Phase 3).
;;
;; Persistent SQLite index of every top-level Elisp definition, its
;; references (call / quote / var), and every `require' / `provide'
;; edge, across a set of configured project roots.  Built to answer
;; the recurring "who calls X?" / "where is X defined?" / "what
;; requires module M?" queries in 1 MCP hop rather than the
;; 3-5-hop Grep + Read loop.
;;
;; Schema:
;;   file      (id, path, mtime, size, indexed_at)
;;   defs      (id, file_id, kind, name, line, end_line,
;;              arity_min, arity_max, docstring_head, obsolete_p)
;;   refs      (id, file_id, name, line, context, kind)
;;   features  (id, file_id, feature, kind)  -- kind in {'requires','provides'}
;;   schema_meta (version)
;;
;; The DB lives at `anvil-defs-index-db-path' (default
;; ~/.emacs.d/anvil-defs-index.db); deleting it is safe — a full
;; rebuild is cheap.
;;
;; Scanner reuses `anvil-sexp--read-file' (Doc 12 Phase 1) so the
;; top-level form parser and kind classifier are single-sourced.
;; Only the walk-and-record logic for nested references lives here.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-server)

;; Architecture α (2026-04-25 user signoff): anvil-XXX delegates pure
;; helpers / heavy logic to nelisp-XXX low-level libraries so the
;; substrate investment (Doc 25 Phase 6.5 全 phase) is reused.
;; nelisp src/ must be on `load-path' (anvil dev daemon adds it via
;; Phase 5-E setup; Stage D launcher bundles nelisp src/ alongside
;; anvil.el).  Soft load — defer to require so byte-compile works in
;; both anvil-only and anvil+nelisp environments.
(require 'nelisp-defs-index nil 'noerror)
;; `--excluded-p' rebinds the nelisp-side defvar via `let' to honour
;; user customisation of `anvil-defs-exclude-patterns'.  Declare the
;; symbol as special so byte-compile in an anvil-only environment
;; (nelisp-defs-index not loaded) treats the binding as dynamic
;; rather than emitting "unused lexical variable".
(defvar nelisp-defs-index-exclude-patterns)

;; anvil-sexp is required lazily (inside functions that use its
;; reader helpers) because anvil-sexp also optionally requires us
;; back to pick up the Phase 2b fast path, and a top-level require
;; in both directions produces a "Recursive require" error at load
;; time.  The byte-compiler is kept happy via `declare-function'.
(declare-function anvil-sexp--with-elisp-syntax "anvil-sexp" (fn))
(declare-function anvil-sexp--read-current-buffer "anvil-sexp" ())
(declare-function anvil-sexp--project-root "anvil-sexp" (&optional hint))
(declare-function anvil-sexp--truthy "anvil-sexp" (v))
;; anvil-git is lazily required inside `anvil-defs--git-changed-lines'
;; (Doc 57 Phase 2) so the top-level keeps no build-time dep on it.
(declare-function anvil-git--run "anvil-git" (args repo &optional opts))
(declare-function anvil-git--check "anvil-git" (res context))
;; anvil-treesit-backend is lazily required inside the Python scanner
;; (Doc 57 Phase 3); the functional `…-with-root-fn' avoids a compile-time
;; macro dependency so elisp-only builds never load the treesit backend.
(declare-function anvil-treesit-with-root-fn "anvil-treesit-backend" (file lang fn))
(declare-function anvil-treesit-compile-query "anvil-treesit-backend" (lang op source))
(declare-function anvil-treesit-language-for-file "anvil-treesit-backend" (file))
(declare-function treesit-node-text "treesit" (node &optional no-property))
(defvar anvil-sexp--function-defining-forms)
(defvar anvil-sexp--defining-forms)


;;;; --- group / config -----------------------------------------------------

(defgroup anvil-defs nil
  "SQLite-backed symbol index for Emacs Lisp projects."
  :group 'anvil
  :prefix "anvil-defs-")

(defcustom anvil-defs-index-db-path
  (expand-file-name "anvil-defs-index.db" user-emacs-directory)
  "Path to the anvil-defs SQLite database file."
  :type 'file
  :group 'anvil-defs)

(defcustom anvil-defs-paths nil
  "List of project roots to index.
When nil, `anvil-defs-index-rebuild' defaults to the git ancestor
of `default-directory' via `anvil-sexp--project-root'."
  :type '(repeat directory)
  :group 'anvil-defs)

(defcustom anvil-defs-exclude-patterns
  '("/\\.git/"
    "/\\.claude/"
    "/node_modules/"
    "/vendor/"
    "/tests/fixtures/"
    "/worktrees/"
    "/\\.worktrees/"
    "/dist/"
    "/build/"
    "/target/"
    "\\.elc\\'")
  "Regexps for files that should not be indexed."
  :type '(repeat regexp)
  :group 'anvil-defs)

(defcustom anvil-defs-languages '(elisp)
  "Languages whose files are collected and scanned into the index.
Each symbol enables one extension + scanner:
  `elisp'      -> .el        (the anvil-sexp reader)
  `python'     -> .py        (tree-sitter, Doc 57 Phase 3)
  `javascript' -> .js .jsx   (tree-sitter, Doc 57 Phase 3 JS/TS)
  `typescript' -> .ts .tsx   (tree-sitter, Doc 57 Phase 3 JS/TS)
Default is elisp-only so existing behavior is unchanged; add a language
to index its call edges for `defs-trace-path' / `defs-detect-changes'.
Non-elisp scanning needs a built-in tree-sitter with the matching grammar."
  :type '(set (const elisp) (const python) (const javascript) (const typescript))
  :group 'anvil-defs)

(defcustom anvil-defs-graph-exclude-names
  '("if" "when" "unless" "cond" "and" "or" "not" "while" "progn" "prog1" "prog2"
    "let" "let*" "letrec" "setq" "setq-default" "quote" "function" "lambda"
    "catch" "unwind-protect" "condition-case" "save-excursion" "save-restriction"
    "save-current-buffer" "with-current-buffer" "interactive" "declare"
    "defun" "defmacro" "defvar" "defconst" "defsubst" "defcustom" "defgroup"
    "defface" "define-minor-mode" "define-derived-mode" "cl-defun" "cl-defmacro"
    "require" "provide" "dolist" "dotimes" "push" "pop" "pcase" "cl-loop"
    "ignore" "ignore-errors" "list" "cons" "car" "cdr" "append" "nconc"
    "nreverse" "eq" "eql" "equal" "null" "memq" "member" "funcall" "apply"
    "mapcar" "mapc" "format" "message" "error" "user-error" "concat")
  "Symbol names treated as language / stdlib plumbing in call-graph analytics.
Excluded from `defs-trace-path', `defs-callers-rank', `defs-clusters' /
`defs-architecture' and `defs-dead-code', so a self-hosted runtime that
*defines* core forms (e.g. NeLisp defining `defun' / `when' / `quote')
does not drown the graph in plumbing.  The `refs' table is untouched —
`defs-references' still finds every mention.  Set to nil to disable."
  :type '(repeat string)
  :group 'anvil-defs)

(defconst anvil-defs-schema-version 1
  "Integer migration key written into schema_meta.")

(defconst anvil-defs--server-id "emacs-eval"
  "Server ID under which defs-* MCP tools are registered.")


;;;; --- backend / db helpers ----------------------------------------------

(defvar anvil-defs--backend nil
  "Active SQLite backend symbol, set on first open.")

(defvar anvil-defs--db nil
  "Open database handle, or nil when closed.")

(defun anvil-defs--detect-backend ()
  "Return the best available SQLite backend or signal a `user-error'."
  (cond
   ((and (fboundp 'sqlite-available-p) (sqlite-available-p)) 'builtin)
   ((require 'emacsql nil t) 'emacsql)
   (t
    (user-error
     "anvil-defs: neither built-in sqlite (Emacs 29+) nor emacsql available"))))

(defun anvil-defs--open (path)
  "Open SQLite database at PATH using the active backend."
  (pcase anvil-defs--backend
    ('builtin (sqlite-open path))
    ('emacsql (user-error "anvil-defs: emacsql backend not implemented"))))

(defun anvil-defs--close (db)
  "Close DB if live."
  (pcase anvil-defs--backend
    ('builtin (when (sqlitep db) (sqlite-close db)))
    ('emacsql nil)))

(defun anvil-defs--execute (db sql &optional params)
  "Execute SQL (DDL / INSERT / UPDATE / DELETE) on DB with PARAMS."
  (pcase anvil-defs--backend
    ('builtin (sqlite-execute db sql params))
    ('emacsql (user-error "anvil-defs: emacsql backend not implemented"))))

(defun anvil-defs--select (db sql &optional params)
  "Run SELECT SQL on DB with PARAMS and return a list of rows."
  (pcase anvil-defs--backend
    ('builtin (sqlite-select db sql params))
    ('emacsql (user-error "anvil-defs: emacsql backend not implemented"))))

(defmacro anvil-defs--with-transaction (db &rest body)
  "Run BODY inside an explicit BEGIN/COMMIT/ROLLBACK on DB.
Plain BEGIN/COMMIT keeps this portable across `with-sqlite-transaction'
availability (see memory feedback_sqlite_with_transaction_not_portable)."
  (declare (indent 1) (debug t))
  (let ((d (make-symbol "db")))
    `(let ((,d ,db))
       (anvil-defs--execute ,d "BEGIN")
       (condition-case err
           (prog1 (progn ,@body)
             (anvil-defs--execute ,d "COMMIT"))
         (error
          (ignore-errors (anvil-defs--execute ,d "ROLLBACK"))
          (signal (car err) (cdr err)))))))


;;;; --- schema / ddl ------------------------------------------------------

(defconst anvil-defs--ddl
  '("CREATE TABLE IF NOT EXISTS schema_meta (
       version INTEGER PRIMARY KEY)"

    "CREATE TABLE IF NOT EXISTS file (
       id          INTEGER PRIMARY KEY,
       path        TEXT UNIQUE NOT NULL,
       mtime       INTEGER NOT NULL,
       size        INTEGER NOT NULL,
       indexed_at  INTEGER NOT NULL)"

    "CREATE TABLE IF NOT EXISTS defs (
       id              INTEGER PRIMARY KEY,
       file_id         INTEGER NOT NULL REFERENCES file(id) ON DELETE CASCADE,
       kind            TEXT NOT NULL,
       name            TEXT NOT NULL,
       line            INTEGER NOT NULL,
       end_line        INTEGER,
       arity_min       INTEGER,
       arity_max       INTEGER,
       docstring_head  TEXT,
       obsolete_p      INTEGER NOT NULL DEFAULT 0)"

    "CREATE INDEX IF NOT EXISTS idx_defs_name ON defs(name)"
    "CREATE INDEX IF NOT EXISTS idx_defs_file ON defs(file_id)"
    "CREATE INDEX IF NOT EXISTS idx_defs_kind_name ON defs(kind, name)"

    "CREATE TABLE IF NOT EXISTS refs (
       id       INTEGER PRIMARY KEY,
       file_id  INTEGER NOT NULL REFERENCES file(id) ON DELETE CASCADE,
       name     TEXT NOT NULL,
       line     INTEGER NOT NULL,
       context  TEXT,
       kind     TEXT NOT NULL)"

    "CREATE INDEX IF NOT EXISTS idx_refs_name ON refs(name)"
    "CREATE INDEX IF NOT EXISTS idx_refs_file ON refs(file_id)"
    "CREATE INDEX IF NOT EXISTS idx_refs_kind_name ON refs(kind, name)"
    ;; refs.context is the caller (enclosing def); index it so the
    ;; callees-direction trace-path lookups (WHERE context = ?) and the
    ;; recursive JOIN ON r.context = t.node do not full-scan refs.
    ;; Additive `IF NOT EXISTS' index — applied on every open, so existing
    ;; DBs gain it without a schema-version bump or rebuild.
    "CREATE INDEX IF NOT EXISTS idx_refs_kind_context ON refs(kind, context)"

    "CREATE TABLE IF NOT EXISTS features (
       id       INTEGER PRIMARY KEY,
       file_id  INTEGER NOT NULL REFERENCES file(id) ON DELETE CASCADE,
       feature  TEXT NOT NULL,
       kind     TEXT NOT NULL)"

    "CREATE INDEX IF NOT EXISTS idx_features_feat_kind ON features(feature, kind)")
  "DDL applied on DB open.")

(defun anvil-defs--stored-schema-version (db)
  "Return the integer schema_meta.version stored in DB, or nil."
  (condition-case _err
      (car-safe (car-safe
                 (anvil-defs--select db "SELECT version FROM schema_meta" nil)))
    (error nil)))

(defun anvil-defs--drop-all-tables (db)
  "Drop every anvil-defs-owned table from DB.
Used when the stored `schema_meta.version' does not match
`anvil-defs-schema-version' — a trivial drop-and-rebuild gives
correct behavior without a per-version migration dispatcher."
  (dolist (tbl '("defs" "refs" "features" "file" "schema_meta"))
    (anvil-defs--execute db (format "DROP TABLE IF EXISTS %s" tbl))))

(defun anvil-defs--apply-ddl (db)
  "Apply DDL, pragmas, and schema version row to DB.
When the stored schema version does not match
`anvil-defs-schema-version', drop every owned table first so the
new DDL lands on a clean slate.  Full rebuild after this is the
user's responsibility (and cheap — sub-second on project scale)."
  (anvil-defs--execute db "PRAGMA journal_mode = WAL")
  (anvil-defs--execute db "PRAGMA foreign_keys = ON")
  (let ((stored (anvil-defs--stored-schema-version db)))
    (when (and stored (not (= stored anvil-defs-schema-version)))
      (message "anvil-defs: schema version mismatch (db=%s code=%s); dropping index"
               stored anvil-defs-schema-version)
      (anvil-defs--drop-all-tables db)))
  (dolist (stmt anvil-defs--ddl)
    (anvil-defs--execute db stmt))
  (anvil-defs--execute
   db "INSERT OR IGNORE INTO schema_meta(version) VALUES (?)"
   (list anvil-defs-schema-version)))


;;;; --- file discovery ---------------------------------------------------

(defun anvil-defs--excluded-p (path)
  "Return non-nil when PATH matches one of `anvil-defs-exclude-patterns'.
Delegates to `nelisp-defs-index--excluded-p' when available, with the
nelisp pattern variable temporarily rebound to `anvil-defs-exclude-patterns'
so user customisation on the anvil side wins (the two defvars carry
identical defaults, but anvil exposes a `defcustom' that users may
extend)."
  (if (fboundp 'nelisp-defs-index--excluded-p)
      (let ((nelisp-defs-index-exclude-patterns anvil-defs-exclude-patterns))
        (nelisp-defs-index--excluded-p path))
    (cl-some (lambda (re) (string-match-p re path))
             anvil-defs-exclude-patterns)))

(defun anvil-defs--file-regexp ()
  "Return a filename regexp matching files for `anvil-defs-languages'.
Unknown language symbols are ignored; an empty set falls back to .el."
  (let ((exts (apply #'append
                     (mapcar (lambda (l)
                               ;; fresh `list' per call — never quoted literals,
                               ;; which `append'/`nconc' could corrupt on reuse.
                               (pcase l
                                 ('elisp (list "el"))
                                 ('python (list "py"))
                                 ('javascript (list "js" "jsx"))
                                 ('typescript (list "ts" "tsx"))))
                             anvil-defs-languages))))
    (format "\\.\\(?:%s\\)\\'"
            (mapconcat #'regexp-quote (or exts '("el")) "\\|"))))

(defcustom anvil-defs-auto-root-exclude
  "\\(?:\\.wt-\\|\\.pre-\\|\\.bak\\|-backup\\|-bak\\|-old\\|-cut\\|-spike\\|-snapshot\\)"
  "Regexp matched against dev-repo basenames in `anvil-defs--auto-roots'.
Matching dirs (backups, experiment cuts, snapshots) are skipped so the
auto-derived index is not polluted by duplicate trees.  Git worktrees are
also dropped structurally — their `.git' is a file, not a directory."
  :type 'regexp
  :group 'anvil-defs)

(defun anvil-defs--auto-roots ()
  "Default index roots used when `anvil-defs-paths' is nil.
If <project-root>/dev/ exists, return the real git clones directly under
it — scoping the index to the active dev repos with no hardcoded absolute
paths (portable across machines).  Skips git worktrees (their `.git' is a
file) and dirs matching `anvil-defs-auto-root-exclude' (backups /
snapshots), both of which are duplicate trees that pollute the graph.
Falls back to the project root itself when there is no dev/ dir."
  (require 'anvil-sexp)
  (let* ((root (anvil-sexp--project-root))
         (dev (and root (expand-file-name "dev" root))))
    (or (and dev (file-directory-p dev)
             (let (repos)
               (dolist (d (directory-files dev t "\\`[^.]" t))
                 (when (and (file-directory-p d)
                            ;; a real clone keeps `.git' as a directory;
                            ;; a worktree's `.git' is a gitdir-pointer file.
                            (file-directory-p (expand-file-name ".git" d))
                            (not (string-match-p anvil-defs-auto-root-exclude
                                                 (file-name-nondirectory d))))
                   (push d repos)))
               (nreverse repos)))
        (and root (list root)))))

(defun anvil-defs--collect-files (&optional paths)
  "Return absolute source paths under PATHS (default `anvil-defs-paths').
Extensions are those enabled by `anvil-defs-languages'.  When both PATHS
and `anvil-defs-paths' are nil, falls back to `anvil-defs--auto-roots'
(the dev repos under the project root, else the project root)."
  (require 'anvil-sexp)
  (let* ((roots (or paths anvil-defs-paths (anvil-defs--auto-roots)))
         (regexp (anvil-defs--file-regexp))
         (acc nil))
    (dolist (root roots)
      (let ((abs (expand-file-name root)))
        (when (file-directory-p abs)
          (dolist (f (directory-files-recursively abs regexp nil))
            (unless (anvil-defs--excluded-p f)
              (push (expand-file-name f) acc))))))
    (nreverse (delete-dups acc))))


;;;; --- scanner -----------------------------------------------------------

;; Phase 6.5 (Doc 25) で nelisp-defs-index に同名 helper が port 済。
;; 本実装は nelisp-defs-index が load-path にあれば nelisp 版へ delegate、
;; なければ自前 fallback (anvil 単体動作保証)。下記 5 helpers は purely
;; functional、cache / I/O 状態を持たないので backward compat 100%。

(defun anvil-defs--first-line (s)
  "Return the first line of S, trimmed and clipped to 160 chars.
Delegates to `nelisp-defs-index--first-line' when available."
  (if (fboundp 'nelisp-defs-index--first-line)
      (nelisp-defs-index--first-line s)
    (when (and s (stringp s))
      (let* ((nl (string-search "\n" s))
             (first (if nl (substring s 0 nl) s))
             (trim (string-trim first)))
        (if (> (length trim) 160) (substring trim 0 160) trim)))))

(defun anvil-defs--extract-docstring (sexp)
  "Return the docstring of SEXP, or nil.
Looks at the third element for defun-likes, fourth for defvar-likes.
Delegates to `nelisp-defs-index--extract-docstring' when available."
  (if (fboundp 'nelisp-defs-index--extract-docstring)
      (nelisp-defs-index--extract-docstring sexp)
    (when (consp sexp)
      (let ((op (car sexp))
            (rest (cddr sexp)))
        (cond
         ((memq op '(defun defmacro defsubst cl-defun cl-defmacro
                      cl-defgeneric cl-defmethod
                      define-minor-mode define-derived-mode))
          ;; (OP NAME ARGLIST [DOC] BODY...)
          (when (and (consp rest) (stringp (cadr rest)))
            (cadr rest)))
         ((memq op '(defvar defvar-local defcustom defconst))
          ;; (OP NAME [INIT] [DOC] ...)
          (when (and (consp rest) (consp (cdr rest)) (stringp (cadr rest)))
            (cadr rest))))))))

(defun anvil-defs--extract-arity (arglist)
  "Return (MIN . MAX) arity for ARGLIST.  MAX is nil when &rest present.
Also treats &body as &rest.  &key turns the call variadic for this
purpose (we cannot statically bound keyword call arity).
Delegates to `nelisp-defs-index--extract-arity' when available."
  (if (fboundp 'nelisp-defs-index--extract-arity)
      (nelisp-defs-index--extract-arity arglist)
    (let ((min 0) (max 0) (stage 'req) (ret nil))
      (catch 'done
        (dolist (a arglist)
          (cond
           ((eq a '&optional) (setq stage 'opt))
           ((memq a '(&rest &body &key))
            (setq ret (cons min nil))
            (throw 'done nil))
           ;; Skip modifier keywords that do not count as arguments.
           ((memq a '(&allow-other-keys &aux)) nil)
           ;; Specializer lists in cl-defmethod look like `(x integer)' —
           ;; treat them as one positional arg, matching call arity.
           ((eq stage 'req) (cl-incf min) (cl-incf max))
           ((eq stage 'opt) (cl-incf max))))
        (setq ret (cons min max)))
      ret)))

(defun anvil-defs--obsolete-p (_sexp)
  "Return non-nil when _SEXP is marked obsolete.
Placeholder; full detection (scanning for nearby `make-obsolete'
calls) is deferred."
  nil)

(defun anvil-defs--arglist (sexp)
  "Return the ARGLIST of a defun-like SEXP, or nil.
Handles shapes the naive (caddr sexp) form misses:
  - `cl-defmethod' with keyword qualifiers like `:around',
    whose arglist sits after any :KEYWORD that follows the name
  - `define-minor-mode' / `define-derived-mode' whose third slot
    is a docstring rather than an arglist (returns nil — arity is
    not statically extractable for those macros)
  - `cl-defstruct' whose CADR is `(NAME :option ...)' and has no
    separate arglist (returns nil)
Delegates to `nelisp-defs-index--arglist' when available.

Behavioural divergence (untested, anvil-only): the legacy fallback
recognises `defalias' / `define-globalized-minor-mode' /
`define-obsolete-function-alias' via `anvil-sexp--function-defining-forms'
but the nelisp backend does not list them.  After delegation,
`defalias' arity goes from a misleading `(2 . 2)' (taking the quoted
target symbol shape as a 2-arg arglist) to `nil' — a bug fix, since
defalias-defined symbols inherit their target's arity.  The two
`define-*-mode' / `-obsolete-function-alias' cases already returned
`nil' on both sides."
  (if (fboundp 'nelisp-defs-index--arglist)
      (nelisp-defs-index--arglist sexp)
    (let* ((op (car sexp))
           (tail (cdr sexp))
           (second (car-safe tail)))
      (cond
       ;; Lazy-load `anvil-sexp' for `anvil-sexp--function-defining-forms'
       ;; (inlined from the former `anvil-defs--kinds-function').  The
       ;; primary path delegates to `nelisp-defs-index--arglist' above,
       ;; so this fallback only runs in anvil-only environments where
       ;; nelisp-defs-index is not loaded.
       ((not (progn (require 'anvil-sexp)
                    (memq op anvil-sexp--function-defining-forms)))
        nil)
       ;; (cl-defmethod NAME [KW ...] ARGLIST BODY).  Skip qualifier
       ;; keywords between NAME and the real arglist.
       ((eq op 'cl-defmethod)
        (let ((rest (cdr-safe tail)))
          (while (and rest (keywordp (car rest)))
            (setq rest (cdr rest)))
          (let ((candidate (car-safe rest)))
            (when (listp candidate) candidate))))
       ;; Define-*-mode macros — no statically-known arglist shape.
       ((memq op '(define-minor-mode define-derived-mode
                   define-globalized-minor-mode
                   define-obsolete-function-alias))
        nil)
       ;; Standard (OP NAME ARGLIST ...) shape.
       (t
        (let ((third (car-safe (cdr-safe tail))))
          (when (and (symbolp second) (listp third))
            third)))))))

(defun anvil-defs--walk-each (xs fn)
  "Apply FN to each element of XS, tolerating improper / dotted lists.
`dolist' signals on the dotted tail; callers walking arbitrary
reader output (including literal alists like `(:title . \"x\")')
must not rely on the proper-list invariant.  FN receives each
`car' and, if XS ends in a non-nil atom, that atom itself.
Delegates to `nelisp-defs-index--walk-each' when available."
  (if (fboundp 'nelisp-defs-index--walk-each)
      (nelisp-defs-index--walk-each xs fn)
    (while (consp xs)
      (funcall fn (car xs))
      (setq xs (cdr xs)))
    (when xs (funcall fn xs))))

(defconst anvil-defs--walker-skip-symbols
  '(nil t)
  "Symbols that appear pervasively and carry no reference semantics.
Keeping them out of the index reduces `refs' row count without
losing query value — no project ever asks \"who calls nil?\".")

(defun anvil-defs--walker-ignored-p (sym)
  "Return non-nil when SYM should not appear in the refs table.
Delegates to `nelisp-defs-index--walker-ignored-p' when available;
both implementations consult an equivalent `(nil t)' skip-list, so
the delegation is semantically transparent."
  (if (fboundp 'nelisp-defs-index--walker-ignored-p)
      (nelisp-defs-index--walker-ignored-p sym)
    (or (null sym)
        (keywordp sym)
        (memq sym anvil-defs--walker-skip-symbols))))

(defun anvil-defs--walk-form-refs (sexp context line emit)
  "Walk SEXP recursively, calling EMIT for each reference / feature edge.
EMIT is (KIND NAME LINE CONTEXT).  CONTEXT is the enclosing def's
name (string) or nil.  LINE is the top-level form's starting
line.  The walker records:
  - call sites: (OP ARGS...) with OP a plain symbol
  - quote sites: \\='X / #\\='X / inside (quote X) / (function X)
  - symbol references: bare symbols in value position
  - require / provide edges (via EMIT\\='s \\='require / \\='provide kinds)"
  (cond
   ((consp sexp)
    (let ((op (car sexp)))
      (cond
       ;; '(quote X) or '(function X) on a bare symbol.  Do not recurse
       ;; — we already extracted the relevant reference.
       ((and (memq op '(quote function))
             (consp (cdr sexp))
             (symbolp (cadr sexp))
             (not (anvil-defs--walker-ignored-p (cadr sexp))))
        (funcall emit 'quote (symbol-name (cadr sexp)) line context))
       ;; (require 'SYM ...)
       ((and (eq op 'require)
             (consp (cdr sexp))
             (consp (cadr sexp))
             (eq (car (cadr sexp)) 'quote)
             (symbolp (cadr (cadr sexp))))
        (funcall emit 'require (symbol-name (cadr (cadr sexp)))
                 line context))
       ;; (provide 'SYM ...)
       ((and (eq op 'provide)
             (consp (cdr sexp))
             (consp (cadr sexp))
             (eq (car (cadr sexp)) 'quote)
             (symbolp (cadr (cadr sexp))))
        (funcall emit 'provide (symbol-name (cadr (cadr sexp)))
                 line context))
       ;; (OP ARGS...) — record a call on OP, then recurse into args.
       ((and (symbolp op) (not (anvil-defs--walker-ignored-p op)))
        (funcall emit 'call (symbol-name op) line context)
        (anvil-defs--walk-each
         (cdr sexp)
         (lambda (sub) (anvil-defs--walk-form-refs sub context line emit))))
       ;; non-symbol operator (e.g. ((lambda (x) x) 1)) / dotted alists
       ;; like (:key . "value") — walk every element; the walker
       ;; helper tolerates improper lists.
       (t
        (anvil-defs--walk-each
         sexp
         (lambda (sub) (anvil-defs--walk-form-refs sub context line emit)))))))
   ;; Bare value-position symbol reference (variable read, argument,
   ;; element of a literal list, etc.).  Without this branch, forms
   ;; like `(setq handlers (list anvil-state-set ...))' miss the
   ;; mention of anvil-state-set as a value, which would make
   ;; defs-references under-report compared to the Phase 2a reader
   ;; scan.
   ((and (symbolp sexp) (not (anvil-defs--walker-ignored-p sexp)))
    (funcall emit 'symbol (symbol-name sexp) line context))))

(defun anvil-defs--line-at (pos)
  "Return 1-based line number for buffer position POS."
  (save-excursion
    (goto-char pos)
    (line-number-at-pos (point) t)))

;;;; --- python scanner (Doc 57 Phase 3, tree-sitter) ----------------------

(defun anvil-defs--ts-node-name (node)
  "Return the `name' field text of tree-sitter NODE, or nil."
  (let ((n (treesit-node-child-by-field-name node "name")))
    (and n (treesit-node-text n t))))

(defun anvil-defs--py-enclosing-class (node)
  "Return the class name directly enclosing NODE (one block level), or nil."
  (let ((parent (treesit-node-parent node)))
    (when (and parent (string= (treesit-node-type parent) "block"))
      (let ((grand (treesit-node-parent parent)))
        (when (and grand (string= (treesit-node-type grand) "class_definition"))
          (anvil-defs--ts-node-name grand))))))

(defun anvil-defs--py-enclosing-defun (node)
  "Return the name of the nearest function_definition ancestor of NODE, or nil."
  (let ((p (treesit-node-parent node)) (res nil))
    (while (and p (not res))
      (when (string= (treesit-node-type p) "function_definition")
        (setq res (anvil-defs--ts-node-name p)))
      (setq p (treesit-node-parent p)))
    res))

(defun anvil-defs--py-callee (call-node)
  "Return the callee symbol name of python CALL-NODE, or nil.
A bare `f()' yields \"f\"; an attribute call `x.m()' yields the method
name \"m\".  Resolution is syntactic — a same-named method on a
different class produces a false edge (the Phase 3 textual caveat)."
  (let ((fnode (treesit-node-child-by-field-name call-node "function")))
    (when fnode
      (pcase (treesit-node-type fnode)
        ("identifier" (treesit-node-text fnode t))
        ("attribute"
         (let ((a (treesit-node-child-by-field-name fnode "attribute")))
           (and a (treesit-node-text a t))))
        (_ nil)))))

(defun anvil-defs--scan-python-file (path)
  "Parse Python PATH via tree-sitter; return (:defs :refs :features).
Defs are function / method / class nodes; refs are call sites with
`name' = callee and `context' = the enclosing def (caller), so the same
`trace_path' / `detect_changes' queries span elisp and Python.  No
type inference — cross-module same-named calls can produce false edges.
import / from-import module names become `requires' features, so
`defs-who-requires' answers \"who imports module M\" for Python too."
  (require 'anvil-treesit-backend)
  (anvil-treesit-with-root-fn
   path 'python
   (lambda (root)
     (let ((defs nil) (refs nil) (features nil)
           (fnq (anvil-treesit-compile-query
                 'python 'anvil-defs-py-fn "(function_definition) @x"))
           (clq (anvil-treesit-compile-query
                 'python 'anvil-defs-py-cls "(class_definition) @x"))
           (caq (anvil-treesit-compile-query
                 'python 'anvil-defs-py-call "(call) @x"))
           (impq (anvil-treesit-compile-query
                  'python 'anvil-defs-py-import
                  "[(import_statement name: (dotted_name) @m)
                    (import_statement name: (aliased_import name: (dotted_name) @m))
                    (import_from_statement module_name: (dotted_name) @m)
                    (import_from_statement module_name: (relative_import) @m)]")))
       (dolist (cap (treesit-query-capture root fnq))
         (let* ((node (cdr cap))
                (name (anvil-defs--ts-node-name node)))
           (when (and name (not (string-empty-p name)))
             (push (list :kind (if (anvil-defs--py-enclosing-class node)
                                   "method" "function")
                         :name name
                         :line (line-number-at-pos (treesit-node-start node))
                         :end-line (line-number-at-pos (treesit-node-end node))
                         :arity-min nil :arity-max nil
                         :docstring-head nil :obsolete-p 0)
                   defs))))
       (dolist (cap (treesit-query-capture root clq))
         (let* ((node (cdr cap))
                (name (anvil-defs--ts-node-name node)))
           (when (and name (not (string-empty-p name)))
             (push (list :kind "class" :name name
                         :line (line-number-at-pos (treesit-node-start node))
                         :end-line (line-number-at-pos (treesit-node-end node))
                         :arity-min nil :arity-max nil
                         :docstring-head nil :obsolete-p 0)
                   defs))))
       (dolist (cap (treesit-query-capture root caq))
         (let* ((node (cdr cap))
                (callee (anvil-defs--py-callee node)))
           (when (and callee (not (string-empty-p callee)))
             (push (list :name callee
                         :line (line-number-at-pos (treesit-node-start node))
                         :context (anvil-defs--py-enclosing-defun node)
                         :kind "call")
                   refs))))
       ;; imports -> features (kind "requires"), so `defs-who-requires'
       ;; answers "who imports module M" for Python too.
       (dolist (cap (treesit-query-capture root impq))
         (let ((m (treesit-node-text (cdr cap) t)))
           (when (and m (not (string-empty-p m)))
             (push (list :feature m :kind "requires") features))))
       (list :defs (nreverse defs) :refs (nreverse refs)
             :features (nreverse features))))))

;;;; --- javascript / typescript scanner (Doc 57 Phase 3 JS/TS) -----------

(defun anvil-defs--ts-enclosing-class (node)
  "Return the class name whose body directly contains NODE, or nil."
  (let ((body (treesit-node-parent node)))
    (when (and body (string= (treesit-node-type body) "class_body"))
      (let ((cls (treesit-node-parent body)))
        (when (and cls (string= (treesit-node-type cls) "class_declaration"))
          (anvil-defs--ts-node-name cls))))))

(defun anvil-defs--ts-enclosing-defun (node)
  "Return the name of the nearest *named* JS/TS function enclosing NODE.
Climbs through anonymous arrow / function expressions, picking up the
name from an enclosing `variable_declarator' (=const f = () =>=) or
object `pair' (={ f() {} }=) when the function node itself is unnamed."
  (let ((p (treesit-node-parent node)) (res nil))
    (while (and p (not res))
      (let ((ty (treesit-node-type p)))
        (cond
         ((member ty '("function_declaration" "generator_function_declaration"
                       "method_definition"))
          (let ((nm (anvil-defs--ts-node-name p)))
            (when (and nm (not (string-empty-p nm))) (setq res nm))))
         ((member ty '("arrow_function" "function_expression"))
          (let ((gp (treesit-node-parent p)))
            (when gp
              (pcase (treesit-node-type gp)
                ("variable_declarator"
                 (let ((nm (treesit-node-child-by-field-name gp "name")))
                   (when nm (setq res (treesit-node-text nm t)))))
                ("pair"
                 (let ((k (treesit-node-child-by-field-name gp "key")))
                   (when k (setq res (treesit-node-text k t)))))))))))
      (setq p (treesit-node-parent p)))
    res))

(defun anvil-defs--ts-callee (call-node)
  "Return the callee symbol name of a JS/TS CALL-NODE, or nil.
A bare `f()' yields \"f\"; a member call `a.b()' yields \"b\" (the
property).  Syntactic only — same-named methods collide (Phase 3 caveat)."
  (let ((fnode (treesit-node-child-by-field-name call-node "function")))
    (when fnode
      (pcase (treesit-node-type fnode)
        ("identifier" (treesit-node-text fnode t))
        ("member_expression"
         (let ((p (treesit-node-child-by-field-name fnode "property")))
           (and p (treesit-node-text p t))))
        (_ nil)))))

(defun anvil-defs--scan-ts-file (path)
  "Parse a JS/TS PATH via tree-sitter; return (:defs :refs :features).
Defs: function declarations, named arrow / function-expression bindings
(=const f = () =>=), classes and methods.  Refs: `call_expression' sites
with `name' = callee and `context' = the nearest named enclosing
function — same shape as elisp / Python, so `trace_path' /
`detect_changes' span all three.  Resolution is syntactic (no type
inference); import specifiers become `requires' features (so
`defs-who-requires' answers \"who imports module M\")."
  (require 'anvil-treesit-backend)
  (let ((lang (anvil-treesit-language-for-file path)))
    (unless lang
      (user-error "anvil-defs: no tree-sitter language for %s" path))
    (anvil-treesit-with-root-fn
     path lang
     (lambda (root)
       (let ((defs nil) (refs nil) (features nil)
             (fnq (anvil-treesit-compile-query
                   lang 'anvil-defs-ts-fn
                   "[(function_declaration) @x (generator_function_declaration) @x]"))
             (clq (anvil-treesit-compile-query
                   lang 'anvil-defs-ts-cls "(class_declaration) @x"))
             (mq (anvil-treesit-compile-query
                  lang 'anvil-defs-ts-method "(method_definition) @x"))
             (dq (anvil-treesit-compile-query
                  lang 'anvil-defs-ts-decl "(variable_declarator) @x"))
             (caq (anvil-treesit-compile-query
                   lang 'anvil-defs-ts-call "(call_expression) @x"))
             (imq (anvil-treesit-compile-query
                   lang 'anvil-defs-ts-import
                   "(import_statement source: (string) @m)")))
         (cl-flet ((emit-def (node kind name)
                     (when (and name (not (string-empty-p name)))
                       (push (list :kind kind :name name
                                   :line (line-number-at-pos
                                          (treesit-node-start node))
                                   :end-line (line-number-at-pos
                                              (treesit-node-end node))
                                   :arity-min nil :arity-max nil
                                   :docstring-head nil :obsolete-p 0)
                             defs))))
           (dolist (cap (treesit-query-capture root fnq))
             (emit-def (cdr cap) "function" (anvil-defs--ts-node-name (cdr cap))))
           (dolist (cap (treesit-query-capture root clq))
             (emit-def (cdr cap) "class" (anvil-defs--ts-node-name (cdr cap))))
           (dolist (cap (treesit-query-capture root mq))
             (emit-def (cdr cap) "method" (anvil-defs--ts-node-name (cdr cap))))
           ;; named arrow / function-expression bindings: per-declarator,
           ;; avoiding the two-capture cross-match pairing hazard.
           (dolist (cap (treesit-query-capture root dq))
             (let* ((decl (cdr cap))
                    (val (treesit-node-child-by-field-name decl "value"))
                    (nm (treesit-node-child-by-field-name decl "name")))
               (when (and val nm
                          (member (treesit-node-type val)
                                  '("arrow_function" "function_expression"))
                          (string= (treesit-node-type nm) "identifier"))
                 (emit-def val "function" (treesit-node-text nm t)))))
           (dolist (cap (treesit-query-capture root caq))
             (let* ((node (cdr cap))
                    (callee (anvil-defs--ts-callee node)))
               (when (and callee (not (string-empty-p callee)))
                 (push (list :name callee
                             :line (line-number-at-pos (treesit-node-start node))
                             :context (anvil-defs--ts-enclosing-defun node)
                             :kind "call")
                       refs))))
           ;; import specifiers -> features (kind "requires"); the module
           ;; string is stored with surrounding quotes stripped.
           (dolist (cap (treesit-query-capture root imq))
             (let* ((raw (treesit-node-text (cdr cap) t))
                    (m (if (and (>= (length raw) 2)
                                (memq (aref raw 0) '(?\" ?\' ?\`)))
                           (substring raw 1 (1- (length raw)))
                         raw)))
               (when (and m (not (string-empty-p m)))
                 (push (list :feature m :kind "requires") features)))))
         (list :defs (nreverse defs) :refs (nreverse refs)
               :features (nreverse features)))))))

(defun anvil-defs--scan-file (path)
  "Dispatch PATH to the matching language scanner by extension.
.py -> Python (tree-sitter); .js/.jsx/.ts/.tsx -> JS/TS (tree-sitter);
everything else -> the elisp reader.  All return (:defs :refs :features)
for the shared ingest path."
  (pcase (downcase (or (file-name-extension path) ""))
    ("py" (anvil-defs--scan-python-file path))
    ((or "js" "jsx" "ts" "tsx") (anvil-defs--scan-ts-file path))
    (_ (anvil-defs--scan-elisp-file path))))

(defun anvil-defs--scan-elisp-file (path)
  "Parse elisp PATH and return (:defs LIST :refs LIST :features LIST).
All items are plists suitable for direct insertion by the ingest
path.  Uses `anvil-sexp--read-file' so every caller shares the
same top-level parser."
  (require 'anvil-sexp)
  (let ((defs nil) (refs nil) (features nil)
        (forms nil))
    (anvil-sexp--with-elisp-syntax
     (lambda ()
       (insert-file-contents path)
       (setq forms (anvil-sexp--read-current-buffer))
       (dolist (f forms)
         (let* ((sexp (plist-get f :sexp))
                (kind (plist-get f :kind))
                (name-sym (plist-get f :name))
                (start (plist-get f :form-start))
                (end (plist-get f :end))
                (line-start (anvil-defs--line-at start))
                (line-end (anvil-defs--line-at end))
                (context-str (and name-sym (symbol-name name-sym))))
           ;; Definition row (kind must be a symbol we recognize as defining).
           (when (and name-sym
                      (memq kind anvil-sexp--defining-forms))
             (let* ((docstring (anvil-defs--extract-docstring sexp))
                    (arglist (anvil-defs--arglist sexp))
                    (arity (and arglist
                                (anvil-defs--extract-arity arglist))))
               (push (list :kind (symbol-name kind)
                           :name (symbol-name name-sym)
                           :line line-start
                           :end-line line-end
                           :arity-min (car-safe arity)
                           :arity-max (cdr-safe arity)
                           :docstring-head (anvil-defs--first-line docstring)
                           :obsolete-p (if (anvil-defs--obsolete-p sexp) 1 0))
                     defs)))
           ;; Feature and reference edges.
           (let ((emit (lambda (ekind ename eline ecntx)
                         (cond
                          ((memq ekind '(require provide))
                           (push (list :feature ename
                                       :kind (pcase ekind
                                               ('require "requires")
                                               ('provide "provides")))
                                 features))
                          (t
                           (push (list :name ename
                                       :line eline
                                       :context ecntx
                                       :kind (symbol-name ekind))
                                 refs))))))
             (anvil-defs--walk-form-refs sexp context-str line-start emit))))))
    (list :defs (nreverse defs)
          :refs (nreverse refs)
          :features (nreverse features))))


;;;; --- ingest ------------------------------------------------------------

(defun anvil-defs--file-id (db path)
  "Return the `file.id' for PATH (inserting a new row when absent)."
  (let* ((attrs (file-attributes path 'integer))
         (mtime (and attrs (float-time (file-attribute-modification-time attrs))))
         (size (and attrs (file-attribute-size attrs))))
    (anvil-defs--execute
     db
     "INSERT INTO file(path,mtime,size,indexed_at) VALUES (?,?,?,?)
      ON CONFLICT(path) DO UPDATE SET
        mtime=excluded.mtime, size=excluded.size, indexed_at=excluded.indexed_at"
     (list path (truncate (or mtime 0)) (or size 0) (truncate (float-time))))
    (car (anvil-defs--select
          db "SELECT id FROM file WHERE path = ?" (list path)))))

(defun anvil-defs--delete-file-rows (db file-id)
  "Delete every indexed row for FILE-ID, leaving the file row itself."
  (anvil-defs--execute db "DELETE FROM defs WHERE file_id = ?" (list file-id))
  (anvil-defs--execute db "DELETE FROM refs WHERE file_id = ?" (list file-id))
  (anvil-defs--execute db "DELETE FROM features WHERE file_id = ?" (list file-id)))

(defun anvil-defs--ingest-file (db path)
  "Scan PATH and write its rows into DB (replacing any prior state)."
  (let* ((scan (anvil-defs--scan-file path))
         (file-id (anvil-defs--file-id db path))
         (file-id-n (if (listp file-id) (car file-id) file-id)))
    (anvil-defs--with-transaction db
      (anvil-defs--delete-file-rows db file-id-n)
      (dolist (d (plist-get scan :defs))
        (anvil-defs--execute
         db
         "INSERT INTO defs(file_id,kind,name,line,end_line,
            arity_min,arity_max,docstring_head,obsolete_p)
          VALUES (?,?,?,?,?,?,?,?,?)"
         (list file-id-n
               (plist-get d :kind)
               (plist-get d :name)
               (plist-get d :line)
               (plist-get d :end-line)
               (plist-get d :arity-min)
               (plist-get d :arity-max)
               (plist-get d :docstring-head)
               (plist-get d :obsolete-p))))
      (dolist (r (plist-get scan :refs))
        (anvil-defs--execute
         db
         "INSERT INTO refs(file_id,name,line,context,kind)
          VALUES (?,?,?,?,?)"
         (list file-id-n
               (plist-get r :name)
               (plist-get r :line)
               (plist-get r :context)
               (plist-get r :kind))))
      (dolist (f (plist-get scan :features))
        (anvil-defs--execute
         db
         "INSERT INTO features(file_id,feature,kind) VALUES (?,?,?)"
         (list file-id-n
               (plist-get f :feature)
               (plist-get f :kind)))))
    (list :defs (length (plist-get scan :defs))
          :refs (length (plist-get scan :refs))
          :features (length (plist-get scan :features)))))


;;;; --- ensure-db + refresh -----------------------------------------------

(defun anvil-defs--live-db-p (db)
  "Non-nil when DB is a usable handle for the active backend.
`sqlitep' only exists on Emacs 29+ — the `fboundp' guard keeps
the helper safe when anvil-defs is loaded on an older Emacs that
falls through to the emacsql stub branch."
  (pcase anvil-defs--backend
    ('builtin (and (fboundp 'sqlitep) (sqlitep db)))
    ('emacsql (and db t))
    (_ nil)))

(defun anvil-defs--ensure-db ()
  "Open the backing DB if not already open.  Return the handle."
  (unless anvil-defs--backend
    (setq anvil-defs--backend (anvil-defs--detect-backend)))
  (unless (and anvil-defs--db (anvil-defs--live-db-p anvil-defs--db))
    (make-directory (file-name-directory anvil-defs-index-db-path) t)
    (setq anvil-defs--db (anvil-defs--open anvil-defs-index-db-path))
    (anvil-defs--apply-ddl anvil-defs--db))
  anvil-defs--db)

(defun anvil-defs--indexed-mtime (db path)
  "Return the mtime we last recorded for PATH, or nil."
  (car-safe (car-safe
             (anvil-defs--select
              db "SELECT mtime FROM file WHERE path = ?" (list path)))))

(defun anvil-defs--current-mtime (path)
  "Return PATH's current on-disk mtime as an integer."
  (when (file-readable-p path)
    (truncate (float-time
               (file-attribute-modification-time
                (file-attributes path 'integer))))))


;;;; --- public API --------------------------------------------------------

(defun anvil-defs-index-rebuild (&optional paths)
  "Re-index every .el file under PATHS (default `anvil-defs-paths').
Returns (:files N :defs N :refs N :features N :duration-ms N)."
  (let* ((db (anvil-defs--ensure-db))
         (t0 (current-time))
         (files (anvil-defs--collect-files paths))
         (totals (list :files 0 :defs 0 :refs 0 :features 0)))
    ;; Drop everything.  File rows cascade via ON DELETE CASCADE.
    (anvil-defs--with-transaction db
      (anvil-defs--execute db "DELETE FROM file")
      (anvil-defs--execute db "DELETE FROM defs")
      (anvil-defs--execute db "DELETE FROM refs")
      (anvil-defs--execute db "DELETE FROM features"))
    (dolist (f files)
      (condition-case _
          (let ((r (anvil-defs--ingest-file db f)))
            (cl-incf (nth 1 totals))
            (setcar (nthcdr 3 totals) (+ (nth 3 totals) (plist-get r :defs)))
            (setcar (nthcdr 5 totals) (+ (nth 5 totals) (plist-get r :refs)))
            (setcar (nthcdr 7 totals) (+ (nth 7 totals)
                                         (plist-get r :features))))
        (error nil)))
    (append totals
            (list :duration-ms (truncate (* 1000 (float-time
                                                  (time-subtract
                                                   (current-time) t0))))))))

(defun anvil-defs-refresh-if-stale (&optional file)
  "Re-ingest FILE if its on-disk mtime is newer than the indexed mtime.
With FILE nil, checks every path already in the index.  Returns the
list of files actually re-scanned."
  (let* ((db (anvil-defs--ensure-db))
         (paths (cond (file (list (expand-file-name file)))
                      (t (mapcar #'car (anvil-defs--select
                                         db "SELECT path FROM file")))))
         (touched nil))
    (dolist (p paths)
      (let ((indexed (anvil-defs--indexed-mtime db p))
            (current (anvil-defs--current-mtime p)))
        (when (and current (or (null indexed) (> current indexed)))
          (anvil-defs--ingest-file db p)
          (push p touched))))
    (nreverse touched)))

(defun anvil-defs-search (name &rest plist)
  "Return definition records whose NAME matches.
PLIST keys:
  :kind  - symbol, symbol list, or string; restricts def.kind
  :fuzzy - non-nil enables LIKE %NAME% matching
  :limit - integer, default 50"
  (let* ((db (anvil-defs--ensure-db))
         (kind (plist-get plist :kind))
         (fuzzy (plist-get plist :fuzzy))
         (limit (or (plist-get plist :limit) 50))
         (kind-sql
          (cond
           ((null kind) nil)
           ((listp kind)
            (format " AND kind IN (%s)"
                    (mapconcat (lambda (_) "?") kind ",")))
           (t " AND kind = ?")))
         (kind-params
          (cond
           ((null kind) nil)
           ((listp kind) (mapcar (lambda (k)
                                   (if (symbolp k) (symbol-name k) k))
                                 kind))
           ((symbolp kind) (list (symbol-name kind)))
           (t (list kind))))
         (sql (format "SELECT d.kind, d.name, f.path, d.line, d.end_line,
                             d.arity_min, d.arity_max, d.docstring_head,
                             d.obsolete_p
                      FROM defs d JOIN file f ON d.file_id = f.id
                      WHERE %s%s
                      ORDER BY d.name, f.path
                      LIMIT ?"
                      (if fuzzy "d.name LIKE ?" "d.name = ?")
                      (or kind-sql "")))
         (params (append
                  (list (if fuzzy (format "%%%s%%" name) name))
                  kind-params
                  (list limit))))
    (mapcar (lambda (row)
              (list :kind (nth 0 row)
                    :name (nth 1 row)
                    :file (nth 2 row)
                    :line (nth 3 row)
                    :end-line (nth 4 row)
                    :arity-min (nth 5 row)
                    :arity-max (nth 6 row)
                    :docstring-head (nth 7 row)
                    :obsolete-p (nth 8 row)))
            (anvil-defs--select db sql params))))

(defun anvil-defs-references (symbol &rest plist)
  "Return reference records for SYMBOL.
PLIST keys:
  :kind  - string, symbol, or list to filter ref.kind (call / quote /
           require / provide)
  :limit - integer, default 500"
  (let* ((db (anvil-defs--ensure-db))
         (name (if (symbolp symbol) (symbol-name symbol) symbol))
         (kind (plist-get plist :kind))
         (limit (or (plist-get plist :limit) 500))
         (kind-sql
          (cond
           ((null kind) nil)
           ((listp kind)
            (format " AND kind IN (%s)"
                    (mapconcat (lambda (_) "?") kind ",")))
           (t " AND kind = ?")))
         (kind-params
          (cond
           ((null kind) nil)
           ((listp kind) (mapcar (lambda (k)
                                   (if (symbolp k) (symbol-name k) k))
                                 kind))
           ((symbolp kind) (list (symbol-name kind)))
           (t (list kind))))
         (sql (format "SELECT f.path, r.line, r.context, r.kind, r.name
                      FROM refs r JOIN file f ON r.file_id = f.id
                      WHERE r.name = ?%s
                      ORDER BY f.path, r.line
                      LIMIT ?"
                      (or kind-sql "")))
         (params (append (list name) kind-params (list limit))))
    (mapcar (lambda (row)
              (list :name (nth 4 row)
                    :file (nth 0 row)
                    :line (nth 1 row)
                    :context (nth 2 row)
                    :kind (nth 3 row)))
            (anvil-defs--select db sql params))))

(defun anvil-defs-signature (symbol)
  "Return a signature plist for SYMBOL, or nil if not indexed.
When multiple definitions exist the first encountered is returned."
  (let* ((name (if (symbolp symbol) (symbol-name symbol) symbol))
         (hits (anvil-defs-search name :limit 1)))
    (when hits
      (let ((h (car hits)))
        (list :name (plist-get h :name)
              :kind (plist-get h :kind)
              :arity-min (plist-get h :arity-min)
              :arity-max (plist-get h :arity-max)
              :file (plist-get h :file)
              :line (plist-get h :line)
              :docstring-head (plist-get h :docstring-head))))))

(defun anvil-defs-who-requires (feature)
  "Return a list of file paths that contain `(require ''FEATURE)'."
  (let* ((db (anvil-defs--ensure-db))
         (feat (if (symbolp feature) (symbol-name feature) feature)))
    (mapcar #'car
            (anvil-defs--select
             db
             "SELECT f.path FROM features ff JOIN file f ON ff.file_id = f.id
              WHERE ff.feature = ? AND ff.kind = 'requires'
              ORDER BY f.path"
             (list feat)))))

(defun anvil-defs-trace-path (symbol &rest plist)
  "Trace the elisp call graph around SYMBOL over the indexed call edges.

The defs index already stores caller->callee edges implicitly: every
`refs' row of kind `call' has `context' = the enclosing definition (the
caller) and `name' = the called symbol (the callee).  This walks that
relation with a bounded breadth-first search, so one call answers \"who
(transitively) calls X\" or \"what does X (transitively) call\" instead of
the agent issuing one `anvil-defs-references' hop per level.

PLIST keys:
  :direction      `callers' (who calls SYMBOL — the default, for
                  blast-radius) or `callees' (what SYMBOL calls).
  :depth          Maximum BFS depth, clamped to 1..5 (default 3).
  :internal-only  When non-nil (the default) only symbols that are
                  themselves defined in the index are traversed and
                  returned — drops builtins, special forms, macros and
                  third-party calls.  nil returns the raw graph.
  :limit          Maximum nodes, clamped to 1..2000 (default 200).

Returns a list of plists (:name :depth :defined), nearest depth first
then by name; SYMBOL itself is never included.  :defined is non-nil when
the node resolves to a definition in the index."
  (let* ((db (anvil-defs--ensure-db))
         (name (if (symbolp symbol) (symbol-name symbol) symbol))
         (callees (eq (or (plist-get plist :direction) 'callers) 'callees))
         (internal (if (plist-member plist :internal-only)
                       (plist-get plist :internal-only)
                     t))
         (depth (max 1 (min 5 (or (plist-get plist :depth) 3))))
         (limit (max 1 (min 2000 (or (plist-get plist :limit) 200))))
         ;; refs.context = caller, refs.name = callee.  callees expand on
         ;; context=node -> name; callers on name=node -> context.
         (seed-col   (if callees "name" "context"))
         (seed-key   (if callees "context" "name"))
         (step-match (if callees "r.context = t.node" "r.name = t.node"))
         (step-col   (if callees "r.name" "r.context"))
         ;; callers additionally require the enclosing context be non-null.
         (seed-notnull (if callees "" " AND context IS NOT NULL"))
         (step-notnull (if callees "" " AND r.context IS NOT NULL"))
         (seed-internal (if internal
                            (format " AND %s IN (SELECT name FROM defs)" seed-col)
                          ""))
         (step-internal (if internal
                            (format " AND %s IN (SELECT name FROM defs)" step-col)
                          ""))
         (xspec (anvil-defs--graph-exclude-sql "x.node"))
         (sql (format "WITH RECURSIVE trace(node, depth) AS (
  SELECT DISTINCT %s, 1 FROM refs
   WHERE %s = ? AND kind = 'call'%s%s
  UNION
  SELECT %s, t.depth + 1
    FROM refs r JOIN trace t ON %s
   WHERE r.kind = 'call' AND t.depth < ?%s%s
)
SELECT x.node, x.d,
       (SELECT COUNT(*) FROM defs d WHERE d.name = x.node)
  FROM (SELECT node, MIN(depth) AS d FROM trace
         WHERE node IS NOT NULL GROUP BY node) x
 WHERE x.node <> ?%s
 ORDER BY x.d, x.node
 LIMIT ?"
                      seed-col seed-key seed-notnull seed-internal
                      step-col step-match step-notnull step-internal
                      (car xspec)))
         (params (append (list name depth name) (cdr xspec) (list limit))))
    (mapcar (lambda (row)
              (list :name (nth 0 row)
                    :depth (nth 1 row)
                    :defined (> (or (nth 2 row) 0) 0)))
            (anvil-defs--select db sql params))))

(defun anvil-defs--git-changed-lines (repo rev)
  "Return ((ABS-PATH (S . E) ...) ...) of new-side changed line ranges.
Runs `git diff -U0 REV' in REPO (REV diffed against the working tree)
via `anvil-git--run' and parses the hunk headers.  Deleted files
(+++ /dev/null) are dropped; a zero-count hunk maps to its single
anchor line."
  (require 'anvil-git)
  (let* ((res (anvil-git--run
               (format "diff --src-prefix=a/ --dst-prefix=b/ -U0 %s"
                       (shell-quote-argument (or rev "HEAD")))
               repo '(:max-output 1048576)))
         (_ (anvil-git--check res "diff -U0"))
         (out (or (plist-get res :stdout) ""))
         (root (expand-file-name repo))
         (acc (make-hash-table :test 'equal))
         (order nil)
         (cur nil))
    (dolist (line (split-string out "\n"))
      (cond
       ((string-prefix-p "+++ " line)
        (let ((p (string-trim (substring line 4))))
          (setq cur (cond ((string= p "/dev/null") nil)
                          ((string-prefix-p "b/" p)
                           (expand-file-name (substring p 2) root))
                          (t (expand-file-name p root))))
          (when (and cur (not (gethash cur acc)))
            (puthash cur t acc) (push cur order))))
       ((and cur (string-prefix-p "@@ " line)
             (string-match
              "\\`@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
              line))
        (let* ((s (string-to-number (match-string 1 line)))
               (cnt (if (match-string 2 line)
                        (string-to-number (match-string 2 line))
                      1))
               (e (if (<= cnt 0) s (+ s (1- cnt)))))
          (push (cons s e) (gethash (concat "R:" cur) acc))))))
    (mapcar (lambda (path)
              (cons path (nreverse (gethash (concat "R:" path) acc))))
            (nreverse order))))

(defun anvil-defs--defs-in-range (db file-id s e)
  "Return def plists in FILE-ID whose [line, end_line] overlaps [S, E]."
  (mapcar (lambda (row)
            (list :name (nth 0 row) :kind (nth 1 row)
                  :line (nth 2 row) :end-line (nth 3 row)))
          (anvil-defs--select
           db "SELECT DISTINCT name, kind, line, end_line FROM defs
                WHERE file_id = ? AND line <= ?
                  AND (end_line IS NULL OR end_line >= ?)"
           (list file-id e s))))

(defun anvil-defs--direct-caller-count (db name)
  "Return the number of distinct indexed definitions that call NAME."
  (or (caar (anvil-defs--select
             db "SELECT COUNT(DISTINCT context) FROM refs
                  WHERE name = ? AND kind = 'call' AND context IS NOT NULL
                    AND context IN (SELECT name FROM defs)"
             (list name)))
      0))

(defun anvil-defs--tested-p (db name)
  "Return non-nil when NAME is referenced from a test file."
  (> (or (caar (anvil-defs--select
                db "SELECT COUNT(*) FROM refs r JOIN file f ON r.file_id = f.id
                     WHERE r.name = ?
                       AND (f.path LIKE '%-test.el'
                            OR f.path LIKE '%/tests/%'
                            OR f.path LIKE '%/test/%')"
                (list name)))
        0)
     0))

(defun anvil-defs-detect-changes (&rest plist)
  "Map git changes to the indexed definitions they touch (blast-radius).

For every changed line range, the enclosing indexed definition(s) are
the \"changed symbols\"; their transitive callers (via
`anvil-defs-trace-path') are the blast-radius.  Each changed symbol
carries a coarse risk plist so an agent can triage what a diff endangers
before it edits — one call in place of a diff read plus N
`defs-references' hops.

PLIST keys:
  :repo    Repo directory (default: the git project root).
  :rev     Revision diffed against the working tree (default \"HEAD\",
           i.e. uncommitted changes).  E.g. \"HEAD~3\", a branch, a SHA.
  :depth   Caller-expansion depth for blast-radius, 1..5 (default 1).
  :limit   Max changed symbols reported (default 100).
  :changes Pre-parsed ((ABS-PATH (S . E) ...) ...) override; when given,
           git is not invoked and the index is not refreshed (test seam).

Risk per changed symbol: `high' = public (no `--') with >=1 caller and
no test reference; `medium' = public, or >=3 callers; `low' otherwise.

Returns (:repo :rev :files N :changed (PLIST...) :total-impacted N),
each changed PLIST being (:name :kind :file :public :direct-callers
:impacted :tested :risk :callers-sample)."
  (let* ((db (anvil-defs--ensure-db))
         (override (plist-member plist :changes))
         (repo (or (plist-get plist :repo)
                   (progn (require 'anvil-sexp) (anvil-sexp--project-root))))
         (rev (or (plist-get plist :rev) "HEAD"))
         (depth (max 1 (min 5 (or (plist-get plist :depth) 1))))
         (limit (max 1 (min 1000 (or (plist-get plist :limit) 100))))
         (changes (if override (plist-get plist :changes)
                    (anvil-defs--git-changed-lines repo rev)))
         (impacted-set (make-hash-table :test 'equal))
         (seen (make-hash-table :test 'equal))
         (changed nil))
    ;; Re-ingest changed files so def line ranges match the diffed state.
    (unless override
      (dolist (entry changes)
        (when (car entry) (ignore-errors (anvil-defs-refresh-if-stale (car entry))))))
    (dolist (entry changes)
      (let* ((path (car entry))
             (file-id (caar (and path (anvil-defs--select
                                       db "SELECT id FROM file WHERE path = ?"
                                       (list path))))))
        (when file-id
          (dolist (r (cdr entry))
            (dolist (d (anvil-defs--defs-in-range db file-id (car r) (cdr r)))
              (let ((nm (plist-get d :name)))
                (when (and (not (gethash nm seen)) (< (length changed) limit))
                  (puthash nm t seen)
                  (let* ((public (not (string-match-p "--" nm)))
                         (callers (anvil-defs--direct-caller-count db nm))
                         (tested (anvil-defs--tested-p db nm))
                         (blast (anvil-defs-trace-path nm :direction 'callers
                                                       :depth depth :limit limit))
                         (risk (cond
                                ((and public (>= callers 1) (not tested)) "high")
                                ((or public (>= callers 3)) "medium")
                                (t "low"))))
                    (dolist (b blast)
                      (puthash (plist-get b :name) t impacted-set))
                    (push (list :name nm :kind (plist-get d :kind)
                                :file path :public public
                                :direct-callers callers
                                :impacted (length blast)
                                :tested tested :risk risk
                                :callers-sample
                                (mapcar (lambda (b) (plist-get b :name))
                                        (seq-take blast 8)))
                          changed)))))))))
    (list :repo (expand-file-name repo) :rev rev
          :files (length changes)
          :changed (nreverse changed)
          :total-impacted (hash-table-count impacted-set))))

;;;; --- dead-code + caller-rank (Doc 57 Phase 4) -------------------------

(defconst anvil-defs--callable-kinds
  '("defun" "defmacro" "defsubst" "cl-defun" "cl-defmacro" "cl-defsubst"
    "cl-defmethod" "cl-defgeneric" "define-minor-mode" "define-derived-mode"
    "function" "method")
  "Def kinds treated as callable for dead-code / caller-rank queries.")

(defun anvil-defs--kinds-sql (kinds)
  "Return (SQL-PLACEHOLDERS . PARAMS) for an `IN (...)' over KINDS.
PARAMS is a fresh list so callers may `append' it without aliasing."
  (cons (format "(%s)" (mapconcat (lambda (_) "?") kinds ","))
        (copy-sequence kinds)))

(defun anvil-defs--graph-exclude-sql (col)
  "Return (SQL-FRAGMENT . PARAMS) excluding plumbing names on COL.
COL is a SQL column/expression; the fragment is empty (and PARAMS nil)
when `anvil-defs-graph-exclude-names' is nil.  PARAMS is a fresh list."
  (if anvil-defs-graph-exclude-names
      (cons (format " AND %s NOT IN (%s)" col
                    (mapconcat (lambda (_) "?")
                               anvil-defs-graph-exclude-names ","))
            (copy-sequence anvil-defs-graph-exclude-names))
    (cons "" nil)))

(defun anvil-defs-dead-code (&rest plist)
  "Return callable definitions with no reference anywhere in the index.
A def is reported when no `refs' row names it from *outside itself*
(call / quote / value reference; the name token inside its own defining
form is ignored) — nothing in the indexed tree uses it.  Use outside the
indexed paths is invisible, so results are *candidates*, not proof:
public API, entry points and dynamically dispatched names may appear
\(filter with :name-pattern, e.g. \"%--%\" for elisp privates).

PLIST keys:
  :kind          List of def kinds (default `anvil-defs--callable-kinds').
  :name-pattern  Optional SQL LIKE pattern on the def name.
  :limit         Maximum rows, clamped 1..5000 (default 200).

Returns plists (:name :kind :file :line :public), file/line order."
  (let* ((db (anvil-defs--ensure-db))
         (kinds (or (plist-get plist :kind) anvil-defs--callable-kinds))
         (pattern (plist-get plist :name-pattern))
         (limit (max 1 (min 5000 (or (plist-get plist :limit) 200))))
         (kspec (anvil-defs--kinds-sql kinds))
         (xspec (anvil-defs--graph-exclude-sql "d.name"))
         (sql (format "SELECT d.kind, d.name, f.path, d.line
                       FROM defs d JOIN file f ON d.file_id = f.id
                       WHERE d.kind IN %s AND d.obsolete_p = 0%s%s
                         AND NOT EXISTS (
                           SELECT 1 FROM refs r
                            WHERE r.name = d.name
                              -- ignore the def's own name token inside its
                              -- defining form (elisp records it as a symbol
                              -- ref with context = the def itself).
                              AND (r.context IS NULL OR r.context <> d.name))
                       ORDER BY f.path, d.line
                       LIMIT ?"
                      (car kspec)
                      (if pattern " AND d.name LIKE ?" "")
                      (car xspec)))
         (params (append (cdr kspec) (and pattern (list pattern))
                         (cdr xspec) (list limit))))
    (mapcar (lambda (row)
              (list :kind (nth 0 row) :name (nth 1 row)
                    :file (nth 2 row) :line (nth 3 row)
                    :public (not (string-match-p "--" (nth 1 row)))))
            (anvil-defs--select db sql params))))

(defun anvil-defs-callers-rank (&rest plist)
  "Rank callable definitions by call in-degree (distinct callers).
Use for hotspots (high in-degree) or under-used code (low).  In-degree
counts distinct enclosing definitions that *call* the name (kind=call),
matching `defs-trace-path' direction=callers.

PLIST keys:
  :min    Minimum caller count, inclusive (default 0).
  :max    Maximum caller count, inclusive (default: no upper bound).
  :kind   List of def kinds (default `anvil-defs--callable-kinds').
  :order  `desc' (default, hotspots first) or `asc'.
  :limit  Maximum rows, clamped 1..2000 (default 50).

Returns plists (:name :kind :file :line :callers :public)."
  (let* ((db (anvil-defs--ensure-db))
         (kinds (or (plist-get plist :kind) anvil-defs--callable-kinds))
         (minc (max 0 (or (plist-get plist :min) 0)))
         (maxc (plist-get plist :max))
         (order (if (eq (plist-get plist :order) 'asc) "ASC" "DESC"))
         (limit (max 1 (min 2000 (or (plist-get plist :limit) 50))))
         (kspec (anvil-defs--kinds-sql kinds))
         (xspec (anvil-defs--graph-exclude-sql "d.name"))
         ;; Pre-aggregate in-degree per callee name in ONE pass, then LEFT
         ;; JOIN to defs — a correlated per-def subquery makes SQLite
         ;; full-scan refs once per def (O(defs*refs); ~10 min on a large
         ;; index).  GROUP BY d.name collapses multi-site defs (in-degree
         ;; is per-name), the outer wrap applies the min/max window.
         (sql (format "SELECT kind, name, path, line, callers FROM (
                         SELECT MIN(d.kind) AS kind, d.name AS name,
                                MIN(f.path) AS path, MIN(d.line) AS line,
                                COALESCE(rc.callers, 0) AS callers
                         FROM defs d
                         JOIN file f ON d.file_id = f.id
                         LEFT JOIN (SELECT name, COUNT(DISTINCT context) AS callers
                                      FROM refs
                                     WHERE kind = 'call' AND context IS NOT NULL
                                     GROUP BY name) rc
                           ON rc.name = d.name
                         WHERE d.kind IN %s AND d.obsolete_p = 0%s
                         GROUP BY d.name)
                       WHERE callers >= ?%s
                       ORDER BY callers %s, name
                       LIMIT ?"
                      (car kspec)
                      (car xspec)
                      (if maxc " AND callers <= ?" "")
                      order))
         (params (append (cdr kspec) (cdr xspec)
                         (list minc) (and maxc (list maxc)) (list limit))))
    (mapcar (lambda (row)
              (list :kind (nth 0 row) :name (nth 1 row)
                    :file (nth 2 row) :line (nth 3 row)
                    :callers (nth 4 row)
                    :public (not (string-match-p "--" (nth 1 row)))))
            (anvil-defs--select db sql params))))

;;;; --- clustering / architecture (Doc 57 Phase 5) -----------------------

(defun anvil-defs--call-graph (db file-pattern)
  "Build the inter-file call graph as undirected weighted adjacency.
A node is a FILE path; edge A--B is weighted by the number of calls from
a def in file A to a def in file B (the caller file is the ref's own
file, the callee file is where its target is defined; intra-file calls
are dropped).  Returns (:adj HASH :deg HASH :nodes LIST :m2 FLOAT) — ADJ
maps node -> hash(neighbor -> weight), DEG node -> weighted degree, NODES
the sorted file list, M2 twice the total edge weight.  Operating at file
granularity keeps the graph small (hundreds of nodes) so single-level
Louvain stays sub-second — symbol granularity is O(10k) nodes and far too
slow in elisp.  With FILE-PATTERN both endpoint files must match."
  (let* ((fclause (if file-pattern " AND cf.path LIKE ? AND nf.path LIKE ?" ""))
         (xname (anvil-defs--graph-exclude-sql "r.name"))
         (xctx (anvil-defs--graph-exclude-sql "r.context"))
         (sql (concat
               "SELECT cf.path, nf.path, COUNT(*)
                  FROM refs r
                  JOIN file cf ON cf.id = r.file_id
                  JOIN defs nd ON nd.name = r.name
                  JOIN file nf ON nf.id = nd.file_id
                 WHERE r.kind='call' AND r.context IS NOT NULL
                   AND r.context <> r.name
                   AND r.file_id <> nd.file_id"
               (car xname) (car xctx)
               fclause
               " GROUP BY cf.path, nf.path"))
         (params (append (cdr xname) (cdr xctx)
                         (and file-pattern (list file-pattern file-pattern))))
         (rows (anvil-defs--select db sql params))
         (adj (make-hash-table :test 'equal))
         (deg (make-hash-table :test 'equal)))
    (cl-flet ((edge (a b w)
                (let ((h (or (gethash a adj)
                             (puthash a (make-hash-table :test 'equal) adj))))
                  (puthash b (+ (or (gethash b h) 0) w) h))
                (puthash a (+ (or (gethash a deg) 0) w) deg)))
      (dolist (row rows)
        (let ((a (nth 0 row)) (b (nth 1 row)) (w (nth 2 row)))
          (edge a b w)
          (edge b a w))))
    (let (nodes (m2 0))
      (maphash (lambda (k v) (push k nodes) (setq m2 (+ m2 v))) deg)
      (list :adj adj :deg deg :nodes (sort nodes #'string<) :m2 (float m2)))))

(defun anvil-defs--louvain (graph &optional max-pass)
  "Single-level Louvain community detection on GRAPH (`anvil-defs--call-graph').
Returns a hash node -> community-id (a representative node name).
Deterministic: nodes are visited in sorted order and a node only moves
on a *strictly* greater modularity gain (ties keep the current
community), so the partition is reproducible and convergent."
  (let* ((adj (plist-get graph :adj))
         (deg (plist-get graph :deg))
         (nodes (plist-get graph :nodes))
         (m2 (plist-get graph :m2))
         (comm (make-hash-table :test 'equal))
         (ctot (make-hash-table :test 'equal))
         (maxp (or max-pass 30)))
    (when (> m2 0)
      (dolist (n nodes)
        (puthash n n comm)
        (puthash n (gethash n deg) ctot))
      (let ((pass 0) (moved t))
        (while (and moved (< pass maxp))
          (setq moved nil pass (1+ pass))
          (dolist (n nodes)
            (let* ((cn (gethash n comm))
                   (kn (gethash n deg))
                   (nbc (make-hash-table :test 'equal)))
              (puthash cn (- (gethash cn ctot) kn) ctot)
              (when (gethash n adj)
                (maphash (lambda (nb w)
                           (let ((c (gethash nb comm)))
                             (when c
                               (puthash c (+ (or (gethash c nbc) 0) w) nbc))))
                         (gethash n adj)))
              (let ((best-c cn)
                    (best-gain (- (or (gethash cn nbc) 0)
                                  (/ (* (gethash cn ctot) kn) m2))))
                (maphash (lambda (c kin)
                           (let ((gain (- kin (/ (* (gethash c ctot) kn) m2))))
                             (when (> gain best-gain)
                               (setq best-c c best-gain gain))))
                         nbc)
                (puthash n best-c comm)
                (puthash best-c (+ (gethash best-c ctot) kn) ctot)
                (unless (equal best-c cn) (setq moved t))))))))
    comm))

(defun anvil-defs--cluster-list (db file-pattern min-size)
  "Return communities of the call graph as (ID . SORTED-MEMBERS) pairs,
largest first.  Communities smaller than MIN-SIZE are dropped."
  (let* ((comm (anvil-defs--louvain (anvil-defs--call-graph db file-pattern)))
         (groups (make-hash-table :test 'equal))
         clusters)
    (maphash (lambda (n c) (push n (gethash c groups))) comm)
    (maphash (lambda (c members)
               (when (>= (length members) min-size)
                 (push (cons c (sort members #'string<)) clusters)))
             groups)
    (sort clusters (lambda (a b)
                     (let ((la (length (cdr a))) (lb (length (cdr b))))
                       (if (= la lb) (string< (car a) (car b)) (> la lb)))))))

(defun anvil-defs-clusters (&rest plist)
  "Detect modules / subsystems via community detection on the inter-file
call graph (single-level Louvain).  Files that call into each other
densely cluster together — an approximate module map.  Only files with at
least one cross-file call edge participate.

PLIST keys:
  :file-pattern  Optional SQL LIKE on file path to scope the graph.
  :min-size      Drop clusters smaller than this many files (default 2).
  :members       File basenames sampled per cluster (default 12).
  :limit         Max clusters returned (default 50).

Returns plists (:id :size :members), largest first; :id and :members are
file basenames, :size is the file count."
  (let* ((db (anvil-defs--ensure-db))
         (min-size (or (plist-get plist :min-size) 2))
         (members-n (or (plist-get plist :members) 12))
         (limit (max 1 (min 5000 (or (plist-get plist :limit) 50))))
         (clusters (anvil-defs--cluster-list
                    db (plist-get plist :file-pattern) min-size)))
    (mapcar (lambda (cl)
              (let ((members (cdr cl)))
                (list :id (file-name-nondirectory (car cl))
                      :size (length members)
                      :members (seq-take
                                (mapcar #'file-name-nondirectory members)
                                members-n))))
            (seq-take clusters limit))))

(defun anvil-defs-architecture (&rest plist)
  "Return a one-call architecture overview of the indexed call graph.
Combines index totals, the busiest symbols (hotspots, by call in-degree),
the call-graph community count, and the largest clusters.

PLIST keys:
  :file-pattern  Optional SQL LIKE to scope clusters to a subtree.
  :hotspots      Number of top hotspots (default 10).
  :clusters      Number of top clusters detailed (default 10).

Returns (:totals :hotspots :cluster-count :clusters)."
  (let* ((db (anvil-defs--ensure-db))
         (status (anvil-defs-index-status))
         (hot-n (or (plist-get plist :hotspots) 10))
         (clu-n (or (plist-get plist :clusters) 10))
         (clusters (anvil-defs--cluster-list
                    db (plist-get plist :file-pattern) 2)))
    (list :totals (list :files (plist-get status :files)
                        :defs (plist-get status :defs)
                        :refs (plist-get status :refs)
                        :features (plist-get status :features))
          :cluster-count (length clusters)
          :hotspots (mapcar (lambda (h)
                              (list :name (plist-get h :name)
                                    :callers (plist-get h :callers)))
                            (anvil-defs-callers-rank :order 'desc :limit hot-n))
          :clusters (mapcar (lambda (cl)
                              (let ((members (cdr cl)))
                                (list :id (file-name-nondirectory (car cl))
                                      :size (length members)
                                      :members (seq-take
                                                (mapcar #'file-name-nondirectory
                                                        members)
                                                8))))
                            (seq-take clusters clu-n)))))

(defun anvil-defs-index-status ()
  "Return (:db-path :files :defs :refs :features :schema-version)."
  (let* ((db (anvil-defs--ensure-db))
         (count (lambda (sql)
                  (car-safe (car-safe (anvil-defs--select db sql))))))
    (list :db-path anvil-defs-index-db-path
          :schema-version anvil-defs-schema-version
          :files (or (funcall count "SELECT COUNT(*) FROM file") 0)
          :defs (or (funcall count "SELECT COUNT(*) FROM defs") 0)
          :refs (or (funcall count "SELECT COUNT(*) FROM refs") 0)
          :features (or (funcall count "SELECT COUNT(*) FROM features") 0))))


;;;; --- MCP tool wrappers ------------------------------------------------

(defun anvil-defs--coerce-int (v default)
  "Return integer V or its numeric parse; fall back to DEFAULT."
  (cond ((integerp v) v)
        ((and (stringp v) (string-match "\\`-?[0-9]+\\'" v))
         (string-to-number v))
        (t default)))

(defun anvil-defs--tool-search (name &optional kind fuzzy limit)
  "List indexed definitions matching NAME.
Lazy-loads `anvil-sexp' for the shared truthy helper so the
`anvil-defs' top-level has no build-time dep on `anvil-sexp'.

MCP Parameters:
  name   - Symbol name (string).
  kind   - Optional comma-separated kind filter, e.g. \"defun,cl-defun\".
  fuzzy  - Non-nil truthy string enables LIKE %NAME% matching.
  limit  - Maximum rows (string or integer); default 50."
  (anvil-server-with-error-handling
   (require 'anvil-sexp)
   (let ((k (and (stringp kind) (not (string-empty-p kind))
                 (split-string kind "[ ,]+" t))))
     (anvil-defs-search name
                        :kind k
                        :fuzzy (anvil-sexp--truthy fuzzy)
                        :limit (anvil-defs--coerce-int limit 50)))))

(defun anvil-defs--tool-references (symbol &optional kind limit)
  "List indexed references of SYMBOL.

MCP Parameters:
  symbol - Symbol name (string).
  kind   - Optional comma-separated ref.kind filter, e.g. \"call,quote\".
  limit  - Maximum rows; default 500."
  (anvil-server-with-error-handling
   (let ((k (and (stringp kind) (not (string-empty-p kind))
                 (split-string kind "[ ,]+" t))))
     (anvil-defs-references symbol
                            :kind k
                            :limit (anvil-defs--coerce-int limit 500)))))

(defun anvil-defs--tool-signature (symbol)
  "Return signature metadata for SYMBOL (:name :kind :arity-min :arity-max).

MCP Parameters:
  symbol - Symbol name (string)."
  (anvil-server-with-error-handling
   (anvil-defs-signature symbol)))

(defun anvil-defs--tool-who-requires (feature)
  "Return files that `(require ''FEATURE)'.

MCP Parameters:
  feature - Feature name (string)."
  (anvil-server-with-error-handling
   (anvil-defs-who-requires feature)))

(defun anvil-defs--tool-trace-path (symbol &optional direction depth internal limit)
  "Trace the elisp call graph around SYMBOL over indexed call edges.

MCP Parameters:
  symbol    - Symbol name (string).
  direction - \"callers\" (who calls SYMBOL — default) or \"callees\"
              (what SYMBOL calls).
  depth     - Max BFS depth 1..5 (string or integer); default 3.
  internal  - Truthy by default: traverse only symbols defined in the
              index, dropping builtins / special forms / macros /
              third-party calls.  Pass \"nil\" / \"false\" / \"0\" to include
              them.
  limit     - Maximum nodes (string or integer); default 200."
  (anvil-server-with-error-handling
   (require 'anvil-sexp)
   (let* ((dir (if (and (stringp direction)
                        (member (downcase (string-trim direction))
                                '("callees" "callee" "out" "down")))
                   'callees 'callers))
          ;; internal-only defaults ON; only an explicit falsey string
          ;; disables it (omitted => arg is nil => keep default).
          (internal-only (if (null internal) t (anvil-sexp--truthy internal))))
     (anvil-defs-trace-path symbol
                            :direction dir
                            :depth (anvil-defs--coerce-int depth 3)
                            :internal-only internal-only
                            :limit (anvil-defs--coerce-int limit 200)))))

(defun anvil-defs--tool-detect-changes (&optional repo rev depth limit)
  "Map a git diff to the indexed symbols it touches + their callers.

MCP Parameters:
  repo   - Repo directory (string).  Default: the git project root.
  rev    - Revision diffed against the working tree (default \"HEAD\" =
           uncommitted changes).  E.g. \"HEAD~3\", a branch, a SHA.
  depth  - Blast-radius caller depth 1..5 (string or integer); default 1.
  limit  - Max changed symbols (string or integer); default 100."
  (anvil-server-with-error-handling
   (require 'anvil-sexp)
   (apply #'anvil-defs-detect-changes
          (append
           (and (stringp repo) (not (string-empty-p repo)) (list :repo repo))
           (and (stringp rev) (not (string-empty-p rev)) (list :rev rev))
           (list :depth (anvil-defs--coerce-int depth 1)
                 :limit (anvil-defs--coerce-int limit 100))))))

(defun anvil-defs--tool-dead-code (&optional kind pattern limit)
  "List callable defs with no reference anywhere in the index.

MCP Parameters:
  kind    - Optional comma-separated def kinds (default: callable kinds —
            defun / cl-defun / defmacro / ... / function / method).
  pattern - Optional SQL LIKE on the name, e.g. \"%--%\" for elisp privates.
  limit   - Maximum rows (string or integer); default 200.

Results are dead-code *candidates*: external / dynamic use is invisible."
  (anvil-server-with-error-handling
   (let ((k (and (stringp kind) (not (string-empty-p kind))
                 (split-string kind "[ ,]+" t))))
     (anvil-defs-dead-code
      :kind k
      :name-pattern (and (stringp pattern) (not (string-empty-p pattern)) pattern)
      :limit (anvil-defs--coerce-int limit 200)))))

(defun anvil-defs--tool-callers-rank (&optional min max kind order limit)
  "Rank callable defs by call in-degree (hotspots / under-used code).

MCP Parameters:
  min   - Minimum caller count, inclusive (string or integer); default 0.
  max   - Maximum caller count, inclusive; default unbounded.  Pass 0 with
          a 0 max for dead-ish (use defs-dead-code for true zero-reference).
  kind  - Optional comma-separated def kinds.
  order - \"desc\" (default, hotspots first) or \"asc\".
  limit - Maximum rows (string or integer); default 50."
  (anvil-server-with-error-handling
   (let ((k (and (stringp kind) (not (string-empty-p kind))
                 (split-string kind "[ ,]+" t))))
     (anvil-defs-callers-rank
      :min (anvil-defs--coerce-int min 0)
      :max (and (or (integerp max)
                    (and (stringp max) (not (string-empty-p max))))
                (anvil-defs--coerce-int max 0))
      :kind k
      :order (if (and (stringp order)
                      (string= (downcase (string-trim order)) "asc"))
                 'asc 'desc)
      :limit (anvil-defs--coerce-int limit 50)))))

(defun anvil-defs--tool-clusters (&optional pattern min_size limit)
  "Detect modules / subsystems via call-graph community detection.

MCP Parameters:
  pattern  - Optional SQL LIKE on file path to scope the graph.
  min_size - Drop clusters smaller than this (default 2).
  limit    - Max clusters (string or integer); default 50."
  (anvil-server-with-error-handling
   (anvil-defs-clusters
    :file-pattern (and (stringp pattern) (not (string-empty-p pattern)) pattern)
    :min-size (anvil-defs--coerce-int min_size 2)
    :limit (anvil-defs--coerce-int limit 50))))

(defun anvil-defs--tool-architecture (&optional pattern hotspots clusters)
  "One-call architecture overview: totals, hotspots, clusters.

MCP Parameters:
  pattern  - Optional SQL LIKE on file path to scope clusters.
  hotspots - Top hotspots to include (string or integer); default 10.
  clusters - Top clusters to detail (string or integer); default 10."
  (anvil-server-with-error-handling
   (anvil-defs-architecture
    :file-pattern (and (stringp pattern) (not (string-empty-p pattern)) pattern)
    :hotspots (anvil-defs--coerce-int hotspots 10)
    :clusters (anvil-defs--coerce-int clusters 10))))

(defun anvil-defs--tool-index-rebuild (&optional paths)
  "Rebuild the defs index.

MCP Parameters:
  paths - Optional comma-separated roots.  Default: configured
          `anvil-defs-paths' or the containing git project."
  (anvil-server-with-error-handling
   (let ((p (and (stringp paths) (not (string-empty-p paths))
                 (mapcar #'string-trim (split-string paths ",")))))
     (anvil-defs-index-rebuild p))))

(defun anvil-defs--tool-index-status ()
  "Return (:db-path :files :defs :refs :features :schema-version).

MCP Parameters: (none)"
  (anvil-server-with-error-handling
   (anvil-defs-index-status)))


;;;; --- module lifecycle ------------------------------------------------

(defun anvil-defs--register-tools ()
  "Register defs-* MCP tools."
  (anvil-server-register-tool
   #'anvil-defs--tool-search
   :id "defs-search"
   :intent '(elisp-read)
   :layer 'core
   :server-id anvil-defs--server-id
   :description
   "Layer 2 of anvil progressive disclosure (see `disclosure-help').
Search the elisp symbol index for defining forms (defun, defvar,
defmacro, cl-defun, ...).  Exact match by default; pass fuzzy=t
for substring matching.  Restrict by kind with a comma-separated
list.  Each row carries :file + :line and can be turned into a
`defs://<sha>/SYM' citation URI (see also `defs-index' Layer 1 for a
slim listing).  Escalate to Layer 3 (`elisp-get-function-definition')
once you picked the definition."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-defs--tool-references
   :id "defs-references"
   :intent '(elisp-read)
   :layer 'core
   :server-id anvil-defs--server-id
   :description
   "List every indexed reference of a symbol.  Filter by ref kind
(call / quote / require / provide).  Strings and comments are
excluded at index time, so results are false-positive free.
1 hop for \"who calls X?\"."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-defs--tool-signature
   :id "defs-signature"
   :intent '(elisp-read)
   :layer 'core
   :server-id anvil-defs--server-id
   :description
   "Return arity + first-line docstring + location for a symbol.
Cheaper than `elisp-describe-function'; answers \"how do I call X?\"."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-defs--tool-who-requires
   :id "defs-who-requires"
   :intent '(elisp-read)
   :layer 'core
   :server-id anvil-defs--server-id
   :description
   "Return files that `(require ''FEATURE)'.  Answers module
reverse-dependency questions."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-defs--tool-trace-path
   :id "defs-trace-path"
   :intent '(elisp-read)
   :layer 'core
   :server-id anvil-defs--server-id
   :description
   "Walk the elisp call graph from a symbol over the indexed call edges
(refs.context -> refs.name).  direction=callers answers \"who
(transitively) calls X?\" (blast-radius); direction=callees answers
\"what does X call?\".  Bounded BFS, depth 1..5.  Collapses what would be
one defs-references hop per level into a single call.  internal (default
on) limits results to symbols defined in the index, hiding builtins /
macros / third-party calls."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-defs--tool-detect-changes
   :id "defs-detect-changes"
   :intent '(elisp-read)
   :layer 'core
   :server-id anvil-defs--server-id
   :description
   "Map a git diff to the indexed definitions it touches and their
transitive callers (blast-radius).  rev (default HEAD = uncommitted
changes) is diffed against the working tree; each changed symbol comes
back with direct-caller count, test-coverage flag and a coarse
high/medium/low risk.  Answers \"what does this diff endanger?\" in one
call instead of reading the diff and chasing defs-references per symbol."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-defs--tool-dead-code
   :id "defs-dead-code"
   :intent '(elisp-read)
   :layer 'core
   :server-id anvil-defs--server-id
   :description
   "List callable definitions with zero references anywhere in the index
\(dead-code candidates).  Filter with a name pattern (e.g. \"%--%\" for
elisp private functions).  External / dynamic use is invisible, so treat
hits as candidates — public API and entry points may surface."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-defs--tool-callers-rank
   :id "defs-callers-rank"
   :intent '(elisp-read)
   :layer 'core
   :server-id anvil-defs--server-id
   :description
   "Rank callable definitions by call in-degree (distinct callers).
order=desc surfaces hotspots (most-called); min/max bound the count for
\"used by >= N\" or low-use sweeps.  Complements defs-dead-code and
defs-trace-path with a whole-index degree view."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-defs--tool-clusters
   :id "defs-clusters"
   :intent '(elisp-read)
   :layer 'core
   :server-id anvil-defs--server-id
   :description
   "Group files that call into each other densely into communities
(single-level Louvain over the inter-file call graph) — an approximate
module / subsystem map.  Each cluster reports its file count and a
basename sample.  Scope with a file-path pattern."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-defs--tool-architecture
   :id "defs-architecture"
   :intent '(elisp-read)
   :layer 'core
   :server-id anvil-defs--server-id
   :description
   "One-call architecture overview of the indexed code: totals, the
busiest symbols (call in-degree hotspots), the call-graph community
count and the largest clusters.  A fast orient-yourself snapshot."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-defs--tool-index-rebuild
   :id "defs-index-rebuild"
   :intent '(elisp-read admin)
   :layer 'workflow
   :server-id anvil-defs--server-id
   :description
   "Rebuild the defs index from scratch.  Opt-in — full rebuild
takes seconds on a normal anvil-sized project.")

  (anvil-server-register-tool
   #'anvil-defs--tool-index-status
   :id "defs-index-status"
   :intent '(elisp-read admin)
   :layer 'workflow
   :server-id anvil-defs--server-id
   :description
   "Return index schema version, DB path, and file / def / ref /
feature counts.  Fast."
   :read-only t))

(defun anvil-defs--unregister-tools ()
  "Remove every defs-* MCP tool."
  (dolist (id '("defs-search"
                "defs-references"
                "defs-signature"
                "defs-who-requires"
                "defs-trace-path"
                "defs-detect-changes"
                "defs-dead-code"
                "defs-callers-rank"
                "defs-clusters"
                "defs-architecture"
                "defs-index-rebuild"
                "defs-index-status"))
    (anvil-server-unregister-tool id anvil-defs--server-id)))

;;;###autoload
(defun anvil-defs-enable ()
  "Register defs-* MCP tools and open the backing SQLite DB."
  (interactive)
  (anvil-defs--ensure-db)
  (anvil-defs--register-tools))

(defun anvil-defs-disable ()
  "Unregister defs-* MCP tools and close the DB."
  (interactive)
  (anvil-defs--unregister-tools)
  (when anvil-defs--db
    (anvil-defs--close anvil-defs--db)
    (setq anvil-defs--db nil)))

(provide 'anvil-defs)
;;; anvil-defs.el ends here
