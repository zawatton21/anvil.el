;;; anvil-sqlite.el --- SQLite query MCP tool for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Exposes generic SQLite read-only queries as an MCP tool so LLM
;; agents can inspect arbitrary SQLite databases (e.g. the anvil
;; org-index, project build artefacts, app state) in a single round
;; trip instead of shelling out to `emacs --batch' with an inline
;; eval form.
;;
;; Requires Emacs 29+ built-in sqlite.  On older Emacs the module
;; loads but the tool signals a clear `user-error' at call time.
;;
;; Intentionally read-only: only SELECT / PRAGMA / EXPLAIN are
;; accepted.  Write operations should go through module-specific
;; APIs that understand their schemas.

;;; Code:

(require 'anvil-server)
(require 'cl-lib)
(require 'json)

(defgroup anvil-sqlite nil
  "Anvil SQLite query tool."
  :group 'anvil
  :prefix "anvil-sqlite-")

(defconst anvil-sqlite--server-id "emacs-eval"
  "Server ID for this module's MCP tools.
Matches the id passed to `anvil-server-process-jsonrpc' by the
stdio shim (--server-id=emacs-eval) so the tool is visible to the
shared MCP connection alongside anvil-file, anvil-org, etc.")

(defcustom anvil-sqlite-max-rows 1000
  "Maximum number of rows returned by `anvil-sqlite-query'.
Queries returning more rows are truncated with a warning in the
response plist.  Prevents accidental blowups of the response."
  :type 'integer
  :group 'anvil-sqlite)

;;; Guardrails

(defun anvil-sqlite--readonly-statement-p (sql)
  "Return non-nil if SQL begins with a read-only statement keyword."
  (let ((trimmed (string-trim (or sql ""))))
    (and (not (string-empty-p trimmed))
         (string-match-p
          "\\`\\(?:SELECT\\|WITH\\|PRAGMA\\|EXPLAIN\\)\\b"
          (upcase trimmed)))))

(defun anvil-sqlite--require-backend ()
  "Signal `user-error' unless built-in sqlite is usable."
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (user-error
     "anvil-sqlite: built-in sqlite not available (Emacs 29+ required)")))

;;; Tool implementation

(defun anvil-sqlite--tool-query (db-path sql &optional params-json)
  "Run a read-only SQL statement against the SQLite DB at DB-PATH.

MCP Parameters:
  db-path     - Absolute path to the SQLite database file (string).
  sql         - SQL statement (string).  Only SELECT, WITH, PRAGMA
                and EXPLAIN are accepted; other statements error.
  params-json - Optional JSON array of bind parameters for the
                statement, e.g. \"[\\\"foo\\\",42]\".  Leave empty
                for no parameters.

Returns a printed plist:
  (:row-count N :truncated BOOL :rows ROWS)
ROWS is a list of row lists.  Large result sets are clipped to
`anvil-sqlite-max-rows' and :truncated becomes t when that
happens.  Errors out of the SQLite layer are bubbled up via
`anvil-server-with-error-handling'."
  (anvil-server-with-error-handling
   (anvil-sqlite--require-backend)
   (unless (anvil-sqlite--readonly-statement-p sql)
     (user-error
      "anvil-sqlite: refusing statement; only SELECT/WITH/PRAGMA/EXPLAIN are allowed"))
   (let* ((path   (expand-file-name db-path))
          (_exist (unless (file-exists-p path)
                    (user-error "anvil-sqlite: %s does not exist" path)))
          (params (cond
                   ((null params-json) nil)
                   ((and (stringp params-json)
                         (string-empty-p (string-trim params-json))) nil)
                   (t (json-parse-string params-json
                                         :array-type 'list))))
          (db     (sqlite-open path))
          (cap    (1+ anvil-sqlite-max-rows))
          truncated rows)
     (unwind-protect
         (let* ((stmt (sqlite-select db sql params 'set))
                (count 0))
           (while (and stmt (sqlite-more-p stmt) (< count cap))
             (let ((row (sqlite-next stmt)))
               (when row
                 (push row rows)
                 (cl-incf count))))
           (when stmt (sqlite-finalize stmt))
           (when (> (length rows) anvil-sqlite-max-rows)
             (setq truncated t)
             (setq rows (cl-subseq rows 0 anvil-sqlite-max-rows))))
       (ignore-errors (sqlite-close db)))
     (let ((ordered (nreverse rows)))
       (format "%S" (list :row-count (length ordered)
                          :truncated (and truncated t)
                          :rows ordered))))))

;;; Module lifecycle

;;;###autoload
(defun anvil-sqlite-enable ()
  "Register the SQLite query MCP tool."
  (anvil-server-register-tool
   #'anvil-sqlite--tool-query
   :id "sqlite-query"
   :intent '(db-read)
   :layer 'core
   :server-id anvil-sqlite--server-id
   :description
   "Run a read-only SQL statement against a SQLite database file
and return the rows as a plist.  Much cheaper than shelling out to
`emacs --batch' with an inline `sqlite-select' form.  Only SELECT /
WITH / PRAGMA / EXPLAIN are accepted.  Requires Emacs 29+ built-in
sqlite.  Large result sets are clipped to `anvil-sqlite-max-rows'."
   :read-only t))

(defun anvil-sqlite-disable ()
  "Unregister the SQLite query MCP tool."
  (anvil-server-unregister-tool "sqlite-query" anvil-sqlite--server-id))

(provide 'anvil-sqlite)
;;; anvil-sqlite.el ends here
