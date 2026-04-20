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

;; anvil-sexp is required lazily (inside functions that use its
;; reader helpers) because anvil-sexp also optionally requires us
;; back to pick up the Phase 2b fast path, and a top-level require
;; in both directions produces a "Recursive require" error at load
;; time.  The byte-compiler is kept happy via `declare-function'.
(declare-function anvil-sexp--with-elisp-syntax "anvil-sexp" (fn))
(declare-function anvil-sexp--read-current-buffer "anvil-sexp" ())
(declare-function anvil-sexp--project-root "anvil-sexp" (&optional hint))
(declare-function anvil-sexp--truthy "anvil-sexp" (v))
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
    "/tests/fixtures/"
    "/worktrees/"
    "/dist/"
    "/build/"
    "\\.elc\\'")
  "Regexps for files that should not be indexed."
  :type '(repeat regexp)
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
  "Return non-nil when PATH matches one of `anvil-defs-exclude-patterns'."
  (cl-some (lambda (re) (string-match-p re path))
           anvil-defs-exclude-patterns))

(defun anvil-defs--collect-files (&optional paths)
  "Return absolute .el paths under PATHS (default `anvil-defs-paths').
Falls back to the nearest git ancestor of `default-directory' when
both are unset."
  (require 'anvil-sexp)
  (let* ((roots (or paths anvil-defs-paths
                    (list (anvil-sexp--project-root))))
         (acc nil))
    (dolist (root roots)
      (let ((abs (expand-file-name root)))
        (when (file-directory-p abs)
          (dolist (f (directory-files-recursively abs "\\.el\\'" nil))
            (unless (anvil-defs--excluded-p f)
              (push (expand-file-name f) acc))))))
    (nreverse (delete-dups acc))))


;;;; --- scanner -----------------------------------------------------------

(defun anvil-defs--kinds-function ()
  "Forms whose name denotes a function or macro for `arity_*' extraction.
Reads `anvil-sexp--function-defining-forms' lazily so the top
level of `anvil-defs' does not depend on `anvil-sexp' loading
(the two modules are mutually optional)."
  (require 'anvil-sexp)
  anvil-sexp--function-defining-forms)

(defun anvil-defs--first-line (s)
  "Return the first line of S, trimmed and clipped to 160 chars."
  (when (and s (stringp s))
    (let* ((nl (string-search "\n" s))
           (first (if nl (substring s 0 nl) s))
           (trim (string-trim first)))
      (if (> (length trim) 160) (substring trim 0 160) trim))))

(defun anvil-defs--extract-docstring (sexp)
  "Return the docstring of SEXP, or nil.
Looks at the third element for defun-likes, fourth for defvar-likes."
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
          (cadr rest)))))))

(defun anvil-defs--extract-arity (arglist)
  "Return (MIN . MAX) arity for ARGLIST.  MAX is nil when &rest present.
Also treats &body as &rest.  &key turns the call variadic for this
purpose (we cannot statically bound keyword call arity)."
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
    ret))

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
    separate arglist (returns nil)."
  (let* ((op (car sexp))
         (tail (cdr sexp))
         (second (car-safe tail)))
    (cond
     ((not (memq op (anvil-defs--kinds-function))) nil)
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
          third))))))

(defun anvil-defs--walk-each (xs fn)
  "Apply FN to each element of XS, tolerating improper / dotted lists.
`dolist' signals on the dotted tail; callers walking arbitrary
reader output (including literal alists like `(:title . \"x\")')
must not rely on the proper-list invariant.  FN receives each
`car' and, if XS ends in a non-nil atom, that atom itself."
  (while (consp xs)
    (funcall fn (car xs))
    (setq xs (cdr xs)))
  (when xs (funcall fn xs)))

(defconst anvil-defs--walker-skip-symbols
  '(nil t)
  "Symbols that appear pervasively and carry no reference semantics.
Keeping them out of the index reduces `refs' row count without
losing query value — no project ever asks \"who calls nil?\".")

(defun anvil-defs--walker-ignored-p (sym)
  "Return non-nil when SYM should not appear in the refs table."
  (or (null sym)
      (keywordp sym)
      (memq sym anvil-defs--walker-skip-symbols)))

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

(defun anvil-defs--scan-file (path)
  "Parse PATH and return (:defs LIST :refs LIST :features LIST).
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
   :server-id anvil-defs--server-id
   :description
   "Layer 2 of anvil progressive disclosure (see `disclosure-help').
Search the elisp symbol index for defining forms (defun, defvar,
defmacro, cl-defun, ...).  Exact match by default; pass fuzzy=t
for substring matching.  Restrict by kind with a comma-separated
list.  Each row carries :file + :line and can be turned into a
`defs://0/SYM' citation URI (see also `defs-index' Layer 1 for a
slim listing).  Escalate to Layer 3 (`elisp-get-function-definition')
once you picked the definition."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-defs--tool-references
   :id "defs-references"
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
   :server-id anvil-defs--server-id
   :description
   "Return arity + first-line docstring + location for a symbol.
Cheaper than `elisp-describe-function'; answers \"how do I call X?\"."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-defs--tool-who-requires
   :id "defs-who-requires"
   :server-id anvil-defs--server-id
   :description
   "Return files that `(require ''FEATURE)'.  Answers module
reverse-dependency questions."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-defs--tool-index-rebuild
   :id "defs-index-rebuild"
   :server-id anvil-defs--server-id
   :description
   "Rebuild the defs index from scratch.  Opt-in — full rebuild
takes seconds on a normal anvil-sized project.")

  (anvil-server-register-tool
   #'anvil-defs--tool-index-status
   :id "defs-index-status"
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
