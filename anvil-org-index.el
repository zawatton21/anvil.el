;;; anvil-org-index.el --- Persistent SQLite index for org files -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Persistent SQLite-backed index of headlines, tags, properties and
;; IDs across configured org directories.  Intended to let MCP read
;; tools avoid re-parsing large org files on every call.
;;
;; See docs/design/02-org-index.org for the full design.  This file
;; covers Phase 1 only: schema creation, full rebuild, status, and a
;; pure-regexp scanner.  Filesystem watcher, incremental refresh and
;; MCP tool integration come in later phases.
;;
;; Backend selection at load time:
;;   1. Emacs 29+ with `sqlite-available-p' -> built-in sqlite
;;   2. `emacsql' installed                  -> emacsql (TODO, Phase 1+)
;;   3. otherwise                             -> `user-error'
;;
;; Only the built-in backend is fully implemented in Phase 1.  The
;; emacsql path is sketched but signals a clear error until completed.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-server)

;; Checkpoint is provided by anvil-offload but is a no-op in the main
;; daemon, so we do not hard-require the module — just declare it to
;; keep byte-compile quiet.  Inside the offload REPL the subprocess's
;; own definition takes over and actually emits the message.
(declare-function anvil-preempt-checkpoint "anvil-offload"
                  (value &optional cursor))

;;; Customization

(defgroup anvil-org-index nil
  "Anvil persistent org index."
  :group 'anvil
  :prefix "anvil-org-index-")

(defcustom anvil-org-index-db-path
  (expand-file-name "anvil-org-index.db" user-emacs-directory)
  "Path to the SQLite database file."
  :type 'file
  :group 'anvil-org-index)

(defcustom anvil-org-index-rebuild-checkpoint-every 25
  "Commit-and-checkpoint granularity for `anvil-org-index-rebuild'.
After every N files the outer transaction is committed and — if
running inside `anvil-offload' — `anvil-preempt-checkpoint' emits
the progress to the main daemon.  Lower N reduces lost work on
preempt at the cost of more SQLite commits; higher N is cheaper
but the cursor lags behind.  A value of 0 falls back to the
Phase 1 single-transaction behaviour (no checkpointing)."
  :type 'integer
  :group 'anvil-org-index)

(defcustom anvil-org-index-paths nil
  "List of directories to recursively index.
Each entry is a directory path.  Non-existent entries are skipped
with a warning."
  :type '(repeat directory)
  :group 'anvil-org-index)

(defcustom anvil-org-index-exclude-patterns
  '("/\\.git/" "/archive/" "/\\.cache/")
  "Regex patterns for paths to exclude from indexing."
  :type '(repeat regexp)
  :group 'anvil-org-index)

(defcustom anvil-org-index-todo-keywords
  '("TODO" "NEXT" "WAIT" "PROJECT" "SOMEDAY"
    "DONE" "CANCEL" "CANCELED" "CANCELLED"
    "NOTE" "MEMO")
  "TODO keywords recognized by the regex scanner.
Any leading uppercase word on a headline that matches this list
is treated as the TODO state."
  :type '(repeat string)
  :group 'anvil-org-index)

(defconst anvil-org-index-schema-version 1
  "Current schema version.  Bump on incompatible changes.")

;;; Backend

(defvar anvil-org-index--backend nil
  "Active backend symbol: `builtin' or `emacsql'.
Populated lazily by `anvil-org-index--detect-backend'.")

(defvar anvil-org-index--db nil
  "Open database handle, or nil.")

(defun anvil-org-index--detect-backend ()
  "Return the best available backend symbol, or signal `user-error'."
  (cond
   ((and (fboundp 'sqlite-available-p) (sqlite-available-p))
    'builtin)
   ((require 'emacsql nil t)
    'emacsql)
   (t
    (user-error
     "anvil-org-index: neither built-in sqlite (Emacs 29+) nor emacsql is available"))))

(defun anvil-org-index--open (path)
  "Open database at PATH using the active backend."
  (pcase anvil-org-index--backend
    ('builtin (sqlite-open path))
    ('emacsql
     (user-error "anvil-org-index: emacsql backend not yet implemented"))))

(defun anvil-org-index--close (db)
  "Close DB."
  (pcase anvil-org-index--backend
    ('builtin (when (sqlitep db) (sqlite-close db)))
    ('emacsql nil)))

(defun anvil-org-index--execute (db sql &optional params)
  "Run SQL (DDL or INSERT/UPDATE/DELETE) against DB with PARAMS."
  (pcase anvil-org-index--backend
    ('builtin (sqlite-execute db sql params))
    ('emacsql (user-error "anvil-org-index: emacsql backend not yet implemented"))))

(defun anvil-org-index--select (db sql &optional params)
  "Run SELECT SQL against DB with PARAMS.  Returns list of rows."
  (pcase anvil-org-index--backend
    ('builtin (sqlite-select db sql params))
    ('emacsql (user-error "anvil-org-index: emacsql backend not yet implemented"))))

(defmacro anvil-org-index--with-transaction (db &rest body)
  "Run BODY inside a transaction on DB.
Uses raw BEGIN/COMMIT/ROLLBACK for portability across backends
and Emacs versions (avoids `with-sqlite-transaction' which is not
present on all targets)."
  (declare (indent 1) (debug t))
  (let ((db-sym (make-symbol "db")))
    `(let ((,db-sym ,db))
       (anvil-org-index--execute ,db-sym "BEGIN")
       (condition-case err
           (prog1 (progn ,@body)
             (anvil-org-index--execute ,db-sym "COMMIT"))
         (error
          (ignore-errors (anvil-org-index--execute ,db-sym "ROLLBACK"))
          (signal (car err) (cdr err)))))))

;;; Schema

(defconst anvil-org-index--ddl
  '("CREATE TABLE IF NOT EXISTS schema_meta (
       version INTEGER PRIMARY KEY)"

    "CREATE TABLE IF NOT EXISTS file (
       id          INTEGER PRIMARY KEY,
       path        TEXT UNIQUE NOT NULL,
       mtime       INTEGER NOT NULL,
       size        INTEGER NOT NULL,
       indexed_at  INTEGER NOT NULL,
       schema_ver  INTEGER NOT NULL DEFAULT 1)"

    "CREATE TABLE IF NOT EXISTS headline (
       id          INTEGER PRIMARY KEY,
       file_id     INTEGER NOT NULL REFERENCES file(id) ON DELETE CASCADE,
       parent_id   INTEGER,
       level       INTEGER NOT NULL,
       title       TEXT NOT NULL,
       todo        TEXT,
       priority    TEXT,
       scheduled   TEXT,
       deadline    TEXT,
       closed      TEXT,
       org_id      TEXT,
       position    INTEGER NOT NULL,
       line_start  INTEGER NOT NULL,
       line_end    INTEGER)"

    "CREATE INDEX IF NOT EXISTS idx_headline_file ON headline(file_id)"
    "CREATE INDEX IF NOT EXISTS idx_headline_todo ON headline(todo)"
    "CREATE INDEX IF NOT EXISTS idx_headline_id ON headline(org_id) WHERE org_id IS NOT NULL"
    "CREATE INDEX IF NOT EXISTS idx_headline_sched ON headline(scheduled) WHERE scheduled IS NOT NULL"

    "CREATE TABLE IF NOT EXISTS tag (
       headline_id INTEGER NOT NULL REFERENCES headline(id) ON DELETE CASCADE,
       tag         TEXT NOT NULL,
       PRIMARY KEY (headline_id, tag))"

    "CREATE INDEX IF NOT EXISTS idx_tag_tag ON tag(tag)"

    "CREATE TABLE IF NOT EXISTS property (
       headline_id INTEGER NOT NULL REFERENCES headline(id) ON DELETE CASCADE,
       key         TEXT NOT NULL,
       value       TEXT NOT NULL,
       PRIMARY KEY (headline_id, key))"

    "CREATE INDEX IF NOT EXISTS idx_property_key ON property(key)")
  "List of DDL statements applied on DB open.")

(defun anvil-org-index--apply-ddl (db)
  "Apply DDL + pragmas + schema version record to DB."
  (anvil-org-index--execute db "PRAGMA journal_mode = WAL")
  (anvil-org-index--execute db "PRAGMA foreign_keys = ON")
  (dolist (stmt anvil-org-index--ddl)
    (anvil-org-index--execute db stmt))
  (anvil-org-index--execute
   db "INSERT OR IGNORE INTO schema_meta(version) VALUES (?)"
   (list anvil-org-index-schema-version)))

;;; File discovery

(defun anvil-org-index--excluded-p (path)
  "Return non-nil if PATH matches any `anvil-org-index-exclude-patterns'."
  (cl-some (lambda (re) (string-match-p re path))
           anvil-org-index-exclude-patterns))

(defun anvil-org-index--collect-files (&optional paths)
  "Return absolute paths of all *.org files under PATHS.
PATHS defaults to `anvil-org-index-paths'."
  (let ((roots (or paths anvil-org-index-paths))
        (acc nil))
    (dolist (root roots)
      (let ((abs (expand-file-name root)))
        (if (not (file-directory-p abs))
            (message "anvil-org-index: skipping missing path %s" abs)
          (dolist (f (directory-files-recursively abs "\\.org\\'" nil))
            (unless (anvil-org-index--excluded-p f)
              (push (expand-file-name f) acc))))))
    (nreverse acc)))

;;; Scanner — pure regexp, does not load org-mode

(defun anvil-org-index--parse-headline-title (raw level)
  "Parse RAW headline text (without leading stars) at LEVEL into a plist.
Extracts TODO keyword, priority cookie [#A], trailing tags :t1:t2:,
and the clean title."
  (let ((s raw) todo prio tags)
    (when (string-match "\\`\\([A-Z]+\\(?:-[A-Z]+\\)?\\)[ \t]+\\(.*\\)\\'" s)
      (let ((kw (match-string 1 s)))
        (when (member kw anvil-org-index-todo-keywords)
          (setq todo kw
                s (match-string 2 s)))))
    (when (string-match "\\`\\[#\\([A-Z]\\)\\][ \t]+\\(.*\\)\\'" s)
      (setq prio (match-string 1 s)
            s (match-string 2 s)))
    (when (string-match "[ \t]+\\(:[A-Za-z0-9_@#%:]+:\\)[ \t]*\\'" s)
      ;; Capture match data before calling `split-string' which resets it.
      (let ((beg    (match-beginning 0))
            (tagstr (match-string 1 s)))
        (setq tags (split-string tagstr ":" t))
        (setq s    (substring s 0 beg))))
    (list :level level
          :todo todo
          :priority prio
          :tags tags
          :title (string-trim s))))

(defun anvil-org-index--scan-buffer ()
  "Scan the current buffer as org text.
Return a list of headline plists in document order.  The buffer
is expected to contain org syntax as plain text; org-mode is NOT
loaded."
  (save-excursion
    (goto-char (point-min))
    (let ((results nil)
          (current nil)
          (in-properties nil)
          (line 1))
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (text (buffer-substring-no-properties bol eol)))
          (cond
           ;; headline
           ((string-match "\\`\\(\\*+\\)[ \t]+\\(.*\\)\\'" text)
            (when current
              (setq current (plist-put current :line-end (1- line)))
              (push current results))
            (let* ((level (length (match-string 1 text)))
                   (rest  (match-string 2 text))
                   (parsed (anvil-org-index--parse-headline-title rest level)))
              (setq current (append parsed
                                    (list :position bol
                                          :line-start line
                                          :scheduled nil
                                          :deadline nil
                                          :closed nil
                                          :org-id nil
                                          :properties nil))
                    in-properties nil)))
           ;; :PROPERTIES: start
           ((and current (string-match-p "\\`[ \t]*:PROPERTIES:[ \t]*\\'" text))
            (setq in-properties t))
           ;; :END:
           ((and current in-properties
                 (string-match-p "\\`[ \t]*:END:[ \t]*\\'" text))
            (setq in-properties nil))
           ;; property line
           ((and current in-properties
                 (string-match "\\`[ \t]*:\\([^:\n\t ]+\\):[ \t]+\\(.*\\)\\'" text))
            (let ((key (match-string 1 text))
                  (val (string-trim (match-string 2 text))))
              (setq current
                    (plist-put current :properties
                               (cons (cons key val)
                                     (plist-get current :properties))))
              (when (string-equal (upcase key) "ID")
                (setq current (plist-put current :org-id val)))))
           ;; SCHEDULED / DEADLINE / CLOSED on planning line.
           ;; A single planning line may carry multiple keywords, so
           ;; scan all matches (not just the first).
           ((and current
                 (string-match-p
                  "\\b\\(?:SCHEDULED\\|DEADLINE\\|CLOSED\\):"
                  text))
            (let ((start 0))
              (while (string-match
                      "\\b\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):[ \t]*\\(<[^>\n]+>\\|\\[[^]\n]+\\]\\)"
                      text start)
                (let ((which (upcase (match-string 1 text)))
                      (ts    (match-string 2 text))
                      (end   (match-end 0)))
                  (setq current
                        (plist-put current
                                   (pcase which
                                     ("SCHEDULED" :scheduled)
                                     ("DEADLINE"  :deadline)
                                     ("CLOSED"    :closed))
                                   ts))
                  (setq start end))))))
          (goto-char (1+ eol))
          (cl-incf line)))
      (when current
        (setq current (plist-put current :line-end (1- line)))
        (push current results))
      (nreverse results))))

(defun anvil-org-index--scan-file (path)
  "Return list of headline plists for PATH."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (anvil-org-index--scan-buffer)))

;;; Insertion helpers

(defun anvil-org-index--insert-file-row (db path)
  "Delete existing FILE row for PATH if any, then insert fresh.
Return the new file id."
  (let* ((attrs (file-attributes path))
         (mtime (when attrs (floor (float-time (file-attribute-modification-time attrs)))))
         (size  (when attrs (file-attribute-size attrs)))
         (now   (floor (float-time))))
    (anvil-org-index--execute db "DELETE FROM file WHERE path = ?" (list path))
    (anvil-org-index--execute
     db
     "INSERT INTO file (path, mtime, size, indexed_at, schema_ver) VALUES (?, ?, ?, ?, ?)"
     (list path mtime size now anvil-org-index-schema-version))
    (caar (anvil-org-index--select db "SELECT last_insert_rowid()"))))

(defun anvil-org-index--insert-headline (db file-id parent-id hl)
  "Insert one headline HL under FILE-ID with optional PARENT-ID.
Return the new headline id."
  (anvil-org-index--execute
   db
   "INSERT INTO headline
      (file_id, parent_id, level, title, todo, priority,
       scheduled, deadline, closed, org_id, position, line_start, line_end)
    VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)"
   (list file-id
         parent-id
         (plist-get hl :level)
         (plist-get hl :title)
         (plist-get hl :todo)
         (plist-get hl :priority)
         (plist-get hl :scheduled)
         (plist-get hl :deadline)
         (plist-get hl :closed)
         (plist-get hl :org-id)
         (plist-get hl :position)
         (plist-get hl :line-start)
         (plist-get hl :line-end)))
  (let ((hid (caar (anvil-org-index--select db "SELECT last_insert_rowid()"))))
    (dolist (tag (plist-get hl :tags))
      (anvil-org-index--execute
       db "INSERT OR IGNORE INTO tag (headline_id, tag) VALUES (?, ?)"
       (list hid tag)))
    (dolist (kv (plist-get hl :properties))
      (anvil-org-index--execute
       db "INSERT OR REPLACE INTO property (headline_id, key, value) VALUES (?, ?, ?)"
       (list hid (car kv) (cdr kv))))
    hid))

(defun anvil-org-index--ingest-file (db path)
  "Re-index a single PATH into DB.  Return list (headline-count)."
  (let* ((headlines (anvil-org-index--scan-file path))
         (file-id   (anvil-org-index--insert-file-row db path))
         ;; stack: (level . id) pairs, deepest on top
         (stack nil))
    (dolist (hl headlines)
      (let ((level (plist-get hl :level)))
        (while (and stack (>= (caar stack) level))
          (pop stack))
        (let* ((parent-id (and stack (cdar stack)))
               (hid (anvil-org-index--insert-headline db file-id parent-id hl)))
          (push (cons level hid) stack))))
    (list :headline-count (length headlines))))

;;; Public API — Phase 1

;;;###autoload
(defun anvil-org-index-enable ()
  "Initialize backend, open DB, apply DDL.  Idempotent."
  (interactive)
  (unless anvil-org-index--backend
    (setq anvil-org-index--backend (anvil-org-index--detect-backend)))
  (unless (and anvil-org-index--db
               (or (not (eq anvil-org-index--backend 'builtin))
                   (sqlitep anvil-org-index--db)))
    (let ((dir (file-name-directory anvil-org-index-db-path)))
      (unless (file-directory-p dir) (make-directory dir t)))
    (setq anvil-org-index--db
          (anvil-org-index--open anvil-org-index-db-path))
    (anvil-org-index--apply-ddl anvil-org-index--db))
  (anvil-org-index--register-tools)
  (message "anvil-org-index: enabled (backend=%s db=%s)"
           anvil-org-index--backend
           anvil-org-index-db-path))

(defun anvil-org-index--register-tools ()
  "Register the org-index MCP tools.  Idempotent."
  (anvil-server-register-tool
   #'anvil-org-index--tool-search
   :id "org-index-search"
   :intent '(org-read)
   :layer 'core
   :server-id anvil-org-index--server-id
   :description
   "Layer 2 of anvil progressive disclosure (see `disclosure-help').
Search the org-index for matching headlines (title LIKE, tag
AND-match, TODO IN, scheduled/deadline date bounds, file path
LIKE) and return a printed plist of rows.  Each row carries its
own `org://ID' citation URI under `:org-id' — reuse it as-is in
Layer 3 (`org-read-by-id').  All parameters are optional; omit to
disable that filter.  For a cheaper first pass use `org-index-index'
(Layer 1, ~20 tok/row).  Results clip at
`anvil-org-index-search-hard-limit' (1000) and mark `:truncated t'
when clipped."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-org-index--tool-rebuild
   :id "org-index-rebuild"
   :intent '(org-read admin)
   :layer 'workflow
   :server-id anvil-org-index--server-id
   :description
   "Rebuild the entire org-index from disk.  Runs in an offload
subprocess so the main daemon stays responsive.  Commits in
chunks of `anvil-org-index-rebuild-checkpoint-every' files and
emits `anvil-preempt-checkpoint' at each chunk boundary.  On
budget exceed (`:offload-timeout', default 300s) returns a
partial plist whose :cursor is (:files-processed N :last-path
P); already-committed chunks persist in the DB so a follow-up
call can re-run safely (idempotent per file)."
   :offload t
   :offload-inherit-load-path t
   :resumable t
   :offload-timeout 300))

(defun anvil-org-index--unregister-tools ()
  "Unregister org-index MCP tools.  Safe when not registered."
  (ignore-errors
    (anvil-server-unregister-tool "org-index-search"
                                   anvil-org-index--server-id))
  (ignore-errors
    (anvil-server-unregister-tool "org-index-rebuild"
                                   anvil-org-index--server-id)))

;;;###autoload
(defun anvil-org-index-disable ()
  "Close DB.  Safe to call multiple times."
  (interactive)
  (anvil-org-index--unregister-tools)
  (when anvil-org-index--db
    (anvil-org-index--close anvil-org-index--db)
    (setq anvil-org-index--db nil))
  (message "anvil-org-index: disabled"))

;;;###autoload
(defun anvil-org-index-rebuild (&optional paths)
  "Rebuild the index over PATHS (defaults to `anvil-org-index-paths').
Existing rows for the touched files are replaced.  Files are
committed in chunks of `anvil-org-index-rebuild-checkpoint-every';
when running inside `anvil-offload', each chunk boundary also
calls `anvil-preempt-checkpoint' so a preempted rebuild (via a
`:resumable t' tool) returns a cursor pointing at the last
committed file.  Returns a summary plist."
  (interactive)
  (unless anvil-org-index--db (anvil-org-index-enable))
  (let* ((db          anvil-org-index--db)
         (files       (anvil-org-index--collect-files paths))
         (files-total (length files))
         (start       (float-time))
         (chunk       anvil-org-index-rebuild-checkpoint-every)
         (total       0)
         (files-done  0))
    (anvil-org-index--execute db "BEGIN")
    (condition-case err
        (progn
          (dolist (f files)
            (let ((info (anvil-org-index--ingest-file db f)))
              (cl-incf total (plist-get info :headline-count))
              (cl-incf files-done))
            (when (and (> chunk 0)
                       (zerop (mod files-done chunk))
                       (< files-done files-total))
              (anvil-org-index--execute db "COMMIT")
              (when (fboundp 'anvil-preempt-checkpoint)
                (anvil-preempt-checkpoint
                 (list :files-done       files-done
                       :files-total      files-total
                       :headlines-so-far total)
                 (list :files-processed files-done
                       :last-path       f)))
              (anvil-org-index--execute db "BEGIN")))
          (anvil-org-index--execute db "COMMIT"))
      (error
       (ignore-errors (anvil-org-index--execute db "ROLLBACK"))
       (signal (car err) (cdr err))))
    (let ((elapsed (- (float-time) start)))
      (message "anvil-org-index: rebuild %d file(s), %d headline(s) in %.2fs"
               files-total total elapsed)
      (list :files       files-total
            :headlines   total
            :elapsed-sec elapsed))))

;;; Phase 2 — incremental refresh

(defun anvil-org-index--indexed-mtime (db path)
  "Return mtime recorded in the index for PATH, or nil if not indexed."
  (caar (anvil-org-index--select
         db "SELECT mtime FROM file WHERE path = ?" (list path))))

(defun anvil-org-index--current-mtime (path)
  "Return filesystem mtime of PATH as integer seconds, or nil if missing."
  (let ((attrs (file-attributes path)))
    (when attrs
      (floor (float-time (file-attribute-modification-time attrs))))))

(defun anvil-org-index--refresh-one (path)
  "Refresh a single PATH against the index.
Re-ingests if stale, deletes if the file no longer exists, skips
if mtimes match.  Returns (:path P :outcome SYM :elapsed-sec N)."
  (let* ((path      (expand-file-name path))
         (start     (float-time))
         (disk-mt   (anvil-org-index--current-mtime path))
         (idx-mt    (anvil-org-index--indexed-mtime anvil-org-index--db path))
         (outcome   'unchanged))
    (cond
     ((not disk-mt)
      (when idx-mt
        (anvil-org-index--execute
         anvil-org-index--db "DELETE FROM file WHERE path = ?" (list path))
        (setq outcome 'removed)))
     ((or (null idx-mt) (/= disk-mt idx-mt))
      (anvil-org-index--with-transaction anvil-org-index--db
        (anvil-org-index--ingest-file anvil-org-index--db path))
      (setq outcome (if idx-mt 'reindexed 'added))))
    (list :path path
          :outcome outcome
          :elapsed-sec (- (float-time) start))))

(defun anvil-org-index--refresh-all (&optional paths)
  "Refresh every file under PATHS (defaults to `anvil-org-index-paths').
Adds new files, re-ingests stale ones, and deletes rows for files
that have been removed from disk.  Runs inside a single transaction.
Returns a summary plist."
  (let* ((db          anvil-org-index--db)
         (start       (float-time))
         (disk-files  (anvil-org-index--collect-files paths))
         (disk-set    (let ((h (make-hash-table :test 'equal)))
                        (dolist (f disk-files) (puthash f t h))
                        h))
         (idx-rows    (anvil-org-index--select
                       db "SELECT path, mtime FROM file"))
         (idx-map     (let ((h (make-hash-table :test 'equal)))
                        (dolist (row idx-rows)
                          (puthash (car row) (cadr row) h))
                        h))
         (added 0) (reindexed 0) (removed 0) (unchanged 0))
    (anvil-org-index--with-transaction db
      (dolist (f disk-files)
        (let ((disk-mt (anvil-org-index--current-mtime f))
              (idx-mt  (gethash f idx-map)))
          (cond
           ((null idx-mt)
            (anvil-org-index--ingest-file db f)
            (cl-incf added))
           ((/= disk-mt idx-mt)
            (anvil-org-index--ingest-file db f)
            (cl-incf reindexed))
           (t (cl-incf unchanged)))))
      (dolist (row idx-rows)
        (let ((p (car row)))
          (unless (gethash p disk-set)
            (anvil-org-index--execute
             db "DELETE FROM file WHERE path = ?" (list p))
            (cl-incf removed)))))
    (let ((elapsed (- (float-time) start)))
      (message "anvil-org-index: refresh +%d ~%d -%d =%d in %.2fs"
               added reindexed removed unchanged elapsed)
      (list :scanned (length disk-files)
            :added added
            :reindexed reindexed
            :removed removed
            :unchanged unchanged
            :elapsed-sec elapsed))))

;;;###autoload
(defun anvil-org-index-refresh-if-stale (&optional file)
  "Refresh FILE (or all configured paths) based on mtime comparison.
With FILE, re-ingest only if its mtime differs from the index, or
delete its rows if the file no longer exists on disk.  Without
FILE, refresh every file under `anvil-org-index-paths'.
Much cheaper than `anvil-org-index-rebuild' when only a few files
have changed.  Returns a summary plist."
  (interactive)
  (unless anvil-org-index--db (anvil-org-index-enable))
  (if file
      (anvil-org-index--refresh-one file)
    (anvil-org-index--refresh-all)))

;;; Phase 3 — filesystem watcher

(defcustom anvil-org-index-watch-debounce-seconds 2.0
  "Delay in seconds before acting on a file-change event.
Consecutive events for the same path within this window are
coalesced to a single refresh call."
  :type 'number
  :group 'anvil-org-index)

(defcustom anvil-org-index-periodic-scan-seconds 3600
  "Interval in seconds for the periodic full-index mtime sweep.
0 disables the periodic scan.  Acts as a safety net in case the
underlying filesystem-notification backend drops events (w32-notify
in particular is known to miss events under heavy load)."
  :type 'integer
  :group 'anvil-org-index)

(defvar anvil-org-index--watch-descriptors nil
  "Alist of (DIR . DESCRIPTOR) for active filesystem watches.")

(defvar anvil-org-index--refresh-timer-map
  (make-hash-table :test 'equal)
  "Map from file path to its pending debounce timer.")

(defvar anvil-org-index--periodic-timer nil
  "Timer for the periodic full-index scan, or nil if inactive.")

(defun anvil-org-index--collect-dirs (&optional paths)
  "Return directories under PATHS that contain at least one .org file.
PATHS defaults to `anvil-org-index-paths'.  Excluded paths
honour `anvil-org-index-exclude-patterns'.  Used to seed the
filesystem watcher — watching every org-bearing directory
catches file creation, modification, renaming and deletion even
when the filenotify backend is non-recursive (the common case on
Linux/inotify)."
  (let ((roots (or paths anvil-org-index-paths))
        (dirs nil))
    (dolist (root roots)
      (let ((abs (expand-file-name root)))
        (when (file-directory-p abs)
          (let ((stack (list abs)))
            (while stack
              (let ((dir (pop stack)))
                (unless (anvil-org-index--excluded-p dir)
                  (let ((has-org nil))
                    (dolist (entry (directory-files dir t "\\`[^.]" t))
                      (cond
                       ((file-directory-p entry)
                        (unless (anvil-org-index--excluded-p entry)
                          (push entry stack)))
                       ((string-match-p "\\.org\\'" entry)
                        (setq has-org t))))
                    (when has-org
                      (push (file-name-as-directory dir) dirs))))))))))
    (nreverse dirs)))

(defun anvil-org-index--perform-refresh (path)
  "Run the debounced refresh for PATH and clear its pending timer.
Exposed as a separate function so tests can exercise it directly
without waiting on real `run-with-timer' delays."
  (remhash path anvil-org-index--refresh-timer-map)
  (condition-case err
      (anvil-org-index-refresh-if-stale path)
    (error
     (message "anvil-org-index: refresh failed for %s: %S" path err))))

(defun anvil-org-index--schedule-refresh (path)
  "Schedule a debounced refresh for PATH.
A second event arriving within
`anvil-org-index-watch-debounce-seconds' cancels the prior timer
and restarts the window, so bursty editors (save-with-backup,
VCS operations) still cost only one refresh."
  (let ((existing (gethash path anvil-org-index--refresh-timer-map)))
    (when (timerp existing)
      (cancel-timer existing)))
  (puthash path
           (run-with-timer
            anvil-org-index-watch-debounce-seconds nil
            #'anvil-org-index--perform-refresh path)
           anvil-org-index--refresh-timer-map))

(defun anvil-org-index--on-file-event (event)
  "Dispatch a filesystem EVENT to the refresh scheduler.
EVENT format: (DESCRIPTOR ACTION FILE &optional FILE1).  Only
actions that plausibly affect .org files are acted on; the
backend-specific `attribute-changed' is included because some
Windows tools touch mtime without raising a normal change event."
  (let ((action (nth 1 event))
        (file   (nth 2 event)))
    (when (and (stringp file)
               (memq action '(created changed deleted renamed
                                      attribute-changed))
               (string-match-p "\\.org\\'" file))
      (anvil-org-index--schedule-refresh (expand-file-name file)))))

(defun anvil-org-index--watch-add-one (dir)
  "Add a `file-notify' watch for DIR.  Returns non-nil on success."
  (condition-case err
      (let ((desc (file-notify-add-watch
                   dir '(change attribute-change)
                   #'anvil-org-index--on-file-event)))
        (push (cons dir desc) anvil-org-index--watch-descriptors)
        t)
    (error
     (message "anvil-org-index: cannot watch %s: %S" dir err)
     nil)))

(defun anvil-org-index--watch-dirs-chunked (dirs chunk-size delay)
  "Install file-notify watches for DIRS, CHUNK-SIZE at a time.
After each chunk, yield to the event loop via `run-at-time' with
DELAY seconds so the human never sees a multi-second freeze."
  (if (<= (length dirs) chunk-size)
      ;; Last (or only) chunk — install and finish.
      (progn
        (dolist (dir dirs)
          (anvil-org-index--watch-add-one dir))
        (message "anvil-org-index: watching %d director%s"
                 (length anvil-org-index--watch-descriptors)
                 (if (= 1 (length anvil-org-index--watch-descriptors))
                     "y" "ies")))
    ;; Install this chunk, then defer the rest.
    (let ((chunk (cl-subseq dirs 0 chunk-size))
          (rest  (cl-subseq dirs chunk-size)))
      (dolist (dir chunk)
        (anvil-org-index--watch-add-one dir))
      (run-at-time delay nil
                   #'anvil-org-index--watch-dirs-chunked
                   rest chunk-size delay))))

;;;###autoload
;;; Phase A' — async refresh via subprocess (offload from main daemon)

(defcustom anvil-org-index-async-emacs-bin
  (or (executable-find "emacs") "emacs")
  "Emacs binary used when spawning the async-refresh subprocess."
  :type 'file
  :group 'anvil-org-index)

(defcustom anvil-org-index-async-refresh-log
  (expand-file-name "anvil-org-index-refresh.log" user-emacs-directory)
  "Log file appended with one line per async refresh completion."
  :type 'file
  :group 'anvil-org-index)

(defvar anvil-org-index--async-refresh-proc nil
  "Currently-running async refresh process, or nil.")

(defun anvil-org-index--async-refresh-sentinel (job-id start-time)
  "Return a sentinel lambda that logs completion for JOB-ID."
  (lambda (p _event)
    (when (memq (process-status p) '(exit signal))
      (let* ((status (process-exit-status p))
             (elapsed (float-time
                       (time-subtract (current-time) start-time)))
             (line (format "%s [%s] exit=%d elapsed=%.2fs %s\n"
                           (format-time-string "%Y-%m-%d %H:%M:%S")
                           job-id status elapsed
                           (if (zerop status) "OK" "FAIL"))))
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region line nil anvil-org-index-async-refresh-log
                        'append 'no-message))
        (when (and (zerop status) (buffer-live-p (process-buffer p)))
          (kill-buffer (process-buffer p)))))))

(defun anvil-org-index--async-refresh-spawn ()
  "Spawn the refresh subprocess.  Internal helper."
  (unless anvil-org-index-paths
    (user-error "anvil-org-index-paths is empty — nothing to refresh"))
  (let* ((lib-path (or (locate-library "anvil-org-index")
                       (user-error "anvil-org-index library not found on load-path")))
         (lib-dir (file-name-directory lib-path))
         ;; Quote the literals so the subprocess `setq' receives data,
         ;; not a function call.  `anvil-org-index-paths' is a list; a
         ;; bare `(a b)' form in the --eval string would be evaluated.
         (paths-literal (prin1-to-string anvil-org-index-paths))
         (db-literal (prin1-to-string anvil-org-index-db-path))
         (job-id (format "refresh-%s" (format-time-string "%H%M%S")))
         (start-time (current-time))
         (eval-form
          (format
           "(progn (require 'anvil-org-index) (setq anvil-org-index-paths '%s anvil-org-index-db-path %s) (anvil-org-index-refresh-if-stale))"
           paths-literal db-literal))
         (proc
          (start-process
           job-id
           (get-buffer-create (format " *%s*" job-id))
           anvil-org-index-async-emacs-bin
           "--batch" "-Q"
           "-L" lib-dir
           "--eval" eval-form)))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel
     proc
     (anvil-org-index--async-refresh-sentinel job-id start-time))
    (setq anvil-org-index--async-refresh-proc proc)
    (format "anvil-org-index: async refresh started: %s pid=%d"
            job-id (process-id proc))))

;;;###autoload
(defun anvil-org-index-refresh-async ()
  "Run `anvil-org-index-refresh-if-stale' in a subprocess (fire-and-forget).
Completion is logged to `anvil-org-index-async-refresh-log'.
When a previous refresh is still running this call is a no-op and
returns a \"busy\" marker so concurrent timer ticks don't pile up."
  (interactive)
  (cond
   ((and anvil-org-index--async-refresh-proc
         (process-live-p anvil-org-index--async-refresh-proc))
    (let ((msg (format
                "anvil-org-index: async refresh already running (pid=%d)"
                (process-id anvil-org-index--async-refresh-proc))))
      (message "%s" msg)
      msg))
   (t
    (anvil-org-index--async-refresh-spawn))))

(defun anvil-org-index-periodic-scan-start (&optional interval)
  "Start the periodic mtime scan (no filesystem watches).
INTERVAL overrides `anvil-org-index-periodic-scan-seconds'.
Each tick runs `anvil-org-index-refresh-async' — a `emacs --batch'
subprocess does the mtime sweep so the main daemon never blocks on
stat traffic even with thousands of org files.  Idempotent."
  (interactive)
  (unless anvil-org-index--db (anvil-org-index-enable))
  (let ((secs (or interval anvil-org-index-periodic-scan-seconds)))
    (when (and (> secs 0)
               (not (timerp anvil-org-index--periodic-timer)))
      (setq anvil-org-index--periodic-timer
            (run-with-timer
             secs secs
             (lambda ()
               (condition-case err
                   (anvil-org-index-refresh-async)
                 (error
                  (message "anvil-org-index periodic scan failed: %S"
                           err))))))
      (message "anvil-org-index: periodic scan every %ds (async subprocess)"
               secs))))

;;;###autoload
(defun anvil-org-index-watch (&optional paths)
  "Start watching PATHS for .org file changes; auto-refresh the index.
PATHS defaults to `anvil-org-index-paths'.  Idempotent — a
second call tears down and re-establishes all watches.  Also
installs the periodic safety-net scan.

WARNING: on Windows, `file-notify-add-watch' takes ~1s per
directory.  With 25 directories this freezes the editor for 25+
seconds.  Use `anvil-org-index-periodic-scan-start' instead."
  (interactive)
  (unless anvil-org-index--db (anvil-org-index-enable))
  (require 'filenotify)
  (anvil-org-index-unwatch)
  (let ((dirs (anvil-org-index--collect-dirs paths)))
    ;; Chunked watch setup — 3 dirs per tick, 0.1s yield between chunks
    (anvil-org-index--watch-dirs-chunked dirs 3 0.1)
    ;; Periodic safety-net scan
    (anvil-org-index-periodic-scan-start)
    dirs))

;;;###autoload
(defun anvil-org-index-unwatch ()
  "Tear down all filesystem watches and pending debounce timers."
  (interactive)
  (dolist (cell anvil-org-index--watch-descriptors)
    (ignore-errors (file-notify-rm-watch (cdr cell))))
  (setq anvil-org-index--watch-descriptors nil)
  (maphash (lambda (_k timer)
             (when (timerp timer) (cancel-timer timer)))
           anvil-org-index--refresh-timer-map)
  (clrhash anvil-org-index--refresh-timer-map)
  (when (timerp anvil-org-index--periodic-timer)
    (cancel-timer anvil-org-index--periodic-timer)
    (setq anvil-org-index--periodic-timer nil)))

;;; Phase 4a — fast-path readers backed by the index

(defun anvil-org-index--file-id (db path)
  "Return the file row id for PATH (expanded), or nil."
  (caar (anvil-org-index--select
         db "SELECT id FROM file WHERE path = ?"
         (list (expand-file-name path)))))

(defun anvil-org-index--decode-title-segment (segment)
  "Decode SEGMENT produced by splitting a slash-separated headline path.
`anvil-org' tools require `/' characters that belong to headline
titles to be encoded as %2F; this undoes that encoding so the
decoded string matches the raw title stored in the index."
  (replace-regexp-in-string "%2F" "/" segment t t))

(defun anvil-org-index--read-file-region (path line-start line-end)
  "Return UTF-8 text from PATH between LINE-START and LINE-END inclusive.
Lines are 1-indexed.  When LINE-END is nil or non-positive, read
to end of file."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (goto-char (point-min))
    (forward-line (max 0 (1- line-start)))
    (let ((beg (point)))
      (cond
       ((and line-end (> line-end 0))
        (goto-char (point-min))
        (forward-line line-end)
        (buffer-substring-no-properties beg (point)))
       (t (buffer-substring-no-properties beg (point-max)))))))

(defun anvil-org-index--subtree-range (db headline-id)
  "Return (path line-start line-end) covering HEADLINE-ID's entire subtree.
The stored `line_end' on a headline points at the line before the
very next heading of any level, so it cuts off nested children.
For `org-read-headline'-style semantics we want the full subtree,
which ends just before the next heading of equal-or-lower level
\(i.e. a sibling or uncle), or the end of file.  Computed here via
the index rather than by re-parsing the file."
  (let ((row (car (anvil-org-index--select
                   db
                   "SELECT f.id, f.path, h.level, h.line_start
                      FROM headline h JOIN file f ON f.id = h.file_id
                      WHERE h.id = ?
                      LIMIT 1"
                   (list headline-id)))))
    (when row
      (let* ((file-id  (nth 0 row))
             (path     (nth 1 row))
             (level    (nth 2 row))
             (start    (nth 3 row))
             (next-row (car (anvil-org-index--select
                             db
                             "SELECT MIN(line_start) FROM headline
                                WHERE file_id = ? AND line_start > ? AND level <= ?"
                             (list file-id start level))))
             (next-start (car next-row))
             (end (if (and next-start (numberp next-start))
                      (1- next-start)
                    0)))
        (list path start end)))))

(defun anvil-org-index--lookup-id-subtree (db org-id)
  "Return (path line-start line-end) for ORG-ID's subtree, or nil."
  (let ((hid (caar (anvil-org-index--select
                    db
                    "SELECT id FROM headline WHERE org_id = ? LIMIT 1"
                    (list org-id)))))
    (and hid (anvil-org-index--subtree-range db hid))))

(defun anvil-org-index--find-by-path (db file-id segments)
  "Walk SEGMENTS under FILE-ID, return the matching headline id or nil.
SEGMENTS is a list of already-decoded title strings.  Ambiguous
matches (more than one headline at a given level sharing a title
under the same parent) return nil so the caller can fall back to
a full parse rather than pick the wrong entry."
  (let ((parent nil)
        (current nil)
        (failed nil))
    (catch 'done
      (dolist (segment segments)
        (let ((rows
               (if parent
                   (anvil-org-index--select
                    db
                    "SELECT id FROM headline
                       WHERE file_id = ? AND parent_id = ? AND title = ?"
                    (list file-id parent segment))
                 (anvil-org-index--select
                  db
                  "SELECT id FROM headline
                     WHERE file_id = ? AND parent_id IS NULL AND title = ?"
                  (list file-id segment)))))
          (cond
           ((null rows)        (setq failed t) (throw 'done nil))
           ((cdr rows)         (setq failed t) (throw 'done nil))
           (t (setq current (caar rows)
                    parent  current))))))
    (unless failed current)))

;;;###autoload
(defun anvil-org-index-read-by-id (org-id)
  "Return the text of the entire subtree whose :ID: property is ORG-ID.
Uses the index to locate (file, line-range) in one SELECT, then
reads only that line range of the file.  Signals `user-error' if
ORG-ID is not in the index."
  (unless anvil-org-index--db (anvil-org-index-enable))
  (let ((loc (anvil-org-index--lookup-id-subtree
              anvil-org-index--db org-id)))
    (unless loc
      (user-error "anvil-org-index: ID %s not found in index" org-id))
    (apply #'anvil-org-index--read-file-region loc)))

;;;###autoload
(defun anvil-org-index-read-headline (file headline-path)
  "Return the text of HEADLINE-PATH inside FILE via the index.
HEADLINE-PATH is a slash-separated list of headline titles, with
any literal `/' inside a title encoded as %2F (matching the
`org-read-headline' MCP tool).  Signals `user-error' if the file
is not indexed or the path is ambiguous."
  (unless anvil-org-index--db (anvil-org-index-enable))
  (let ((file-id (anvil-org-index--file-id anvil-org-index--db file)))
    (unless file-id
      (user-error "anvil-org-index: %s is not indexed" file))
    (let* ((segments (mapcar #'anvil-org-index--decode-title-segment
                             (split-string headline-path "/" t)))
           (hid (anvil-org-index--find-by-path
                 anvil-org-index--db file-id segments))
           (range (and hid (anvil-org-index--subtree-range
                            anvil-org-index--db hid))))
      (unless range
        (user-error
         "anvil-org-index: headline %S not uniquely resolvable under %s"
         headline-path file))
      (apply #'anvil-org-index--read-file-region range))))

;;;###autoload
(defun anvil-org-index-read-outline (file &optional max-depth)
  "Return an outline list of FILE straight from the index.
Each entry is a plist: (:level :title :todo :tags :line).  Order
follows the on-disk position of the headlines.  When MAX-DEPTH is
a positive integer the SQL side filters `WHERE level <= ?' so only
headlines at that depth or shallower are returned.  Much cheaper
than re-parsing with org-element for large files that rarely change."
  (unless anvil-org-index--db (anvil-org-index-enable))
  (let ((file-id (anvil-org-index--file-id anvil-org-index--db file)))
    (unless file-id
      (user-error "anvil-org-index: %s is not indexed" file))
    (let ((rows
           (if (and max-depth (integerp max-depth) (> max-depth 0))
               (anvil-org-index--select
                anvil-org-index--db
                "SELECT id, level, title, todo, line_start
                   FROM headline
                   WHERE file_id = ? AND level <= ?
                   ORDER BY position"
                (list file-id max-depth))
             (anvil-org-index--select
              anvil-org-index--db
              "SELECT id, level, title, todo, line_start
                 FROM headline
                 WHERE file_id = ?
                 ORDER BY position"
              (list file-id)))))
      (mapcar
       (lambda (row)
         (let* ((hid   (nth 0 row))
                (tags  (anvil-org-index--select
                        anvil-org-index--db
                        "SELECT tag FROM tag WHERE headline_id = ?"
                        (list hid))))
           (list :level (nth 1 row)
                 :title (nth 2 row)
                 :todo  (nth 3 row)
                 :tags  (mapcar #'car tags)
                 :line  (nth 4 row))))
       rows))))

(defun anvil-org-index--outline-build-tree (flat)
  "Build a hierarchical alist tree from FLAT outline list.
FLAT is the list returned by `anvil-org-index-read-outline', each
element a plist with :level and :title.  Returns a vector of alists
with keys title, level, children (full depth, populated)."
  (let* ((root (list (cons :level 0) (cons :children nil)))
         (stack (list root)))
    (dolist (node flat)
      (let* ((lv (plist-get node :level))
             (entry (list (cons :level lv)
                          (cons :title (plist-get node :title))
                          (cons :children nil))))
        ;; Pop deeper/equal-level frames so the top is a strict parent.
        (while (and (cdr stack)
                    (>= (alist-get :level (car stack)) lv))
          (pop stack))
        (let ((children-cell (assq :children (car stack))))
          (setcdr children-cell
                  (append (cdr children-cell) (list entry))))
        (push entry stack)))
    (anvil-org-index--outline-nodes-to-json
     (alist-get :children root))))

(defun anvil-org-index--outline-nodes-to-json (nodes)
  "Convert internal NODES list to a JSON-ready vector of alists.
Uses symbol keys (title, level, children) compatible with
`anvil-org--generate-outline' output."
  (vconcat
   (mapcar
    (lambda (node)
      `((title    . ,(alist-get :title node))
        (level    . ,(alist-get :level node))
        (children . ,(anvil-org-index--outline-nodes-to-json
                      (alist-get :children node)))))
    nodes)))

;;;###autoload
(defun anvil-org-index-read-outline-json (file &optional max-depth)
  "Return a hierarchical outline of FILE as a JSON string.
Shape matches `anvil-org--generate-outline':
  {\"headings\": [{\"title\":..,\"level\":..,\"children\":[..]}..]}
but populates children at full depth instead of only two levels.
When MAX-DEPTH is a positive integer, only headlines at that level
or shallower are included; the tree is built from the filtered
flat list so no dangling children remain.  Cheap fast-path for
large files already in the index."
  (require 'json)
  (let* ((flat (anvil-org-index-read-outline file max-depth))
         (tree (anvil-org-index--outline-build-tree flat)))
    (json-encode `((headings . ,tree)))))

;;; Phase 4c+: full-text search fast-path

(defconst anvil-org-index--server-id "emacs-eval"
  "Server ID for org-index MCP tools.
Matches the stdio shim `--server-id=emacs-eval' so the search
tool appears alongside the other anvil-* tools.")

(defcustom anvil-org-index-search-default-limit 100
  "Default `:limit' for `anvil-org-index-search' when the caller omits one."
  :type 'integer
  :group 'anvil-org-index)

(defcustom anvil-org-index-search-hard-limit 1000
  "Absolute cap on the number of rows returned by `anvil-org-index-search'.
Protects the MCP channel from accidentally megabyte-sized
replies; the caller may request a smaller limit but never larger."
  :type 'integer
  :group 'anvil-org-index)

(defun anvil-org-index--search-like-wrap (s)
  "Wrap S with SQL LIKE wildcards when it contains no % already."
  (if (or (null s) (string-empty-p s)) nil
    (if (string-match-p "%" s) s
      (concat "%" s "%"))))

(defun anvil-org-index--search-ts-substr (col op)
  "Produce a SQL fragment comparing the YYYY-MM-DD slice of COL via OP."
  (format "substr(%s, 2, 10) %s ?" col op))

(defun anvil-org-index--search-normalise-list (v)
  "Coerce V into a non-empty list of strings, or nil.
Accepts a string (singleton), a list, or a comma-separated string."
  (cond
   ((null v) nil)
   ((and (stringp v) (string-empty-p (string-trim v))) nil)
   ((stringp v)
    (if (string-match-p "," v)
        (mapcar #'string-trim
                (split-string v "," t "[ \t\n\r]+"))
      (list v)))
   ((listp v) (seq-filter (lambda (x) (and x (not (string-empty-p x)))) v))
   (t nil)))

;;;###autoload
(cl-defun anvil-org-index-search
    (&key title-like file-like todo tag
          property-key property-value
          scheduled-before scheduled-after
          deadline-before  deadline-after
          limit offset)
  "Search the headline index and return matching rows as a plist.

Keyword arguments (all optional, combined with AND):
  :title-like        SQL LIKE pattern on `headline.title'; bare
                     strings are auto-wrapped with `%...%'.
  :file-like         SQL LIKE pattern on `file.path'.
  :todo              string or list/CSV; matches any of the given
                     TODO keywords (IN clause).
  :tag               string or list/CSV; *every* tag listed must be
                     attached to the headline (AND of EXISTS).
  :property-key      exact match on PROPERTIES drawer key (case
                     preserved from the org source — typically
                     UPPERCASE like \"ID\", \"CATEGORY\").
  :property-value    SQL LIKE pattern on property value; auto-
                     wrapped with `%...%' when bare.  Combines
                     with :property-key as \"key = ? AND value LIKE ?\"
                     on the SAME row; on its own it matches any key.
  :scheduled-before  ISO date \"YYYY-MM-DD\"; keeps headlines whose
                     SCHEDULED date is <= this value.
  :scheduled-after   same but >=.
  :deadline-before / :deadline-after  — as above but for DEADLINE.
  :limit             integer, default `anvil-org-index-search-default-limit',
                     capped by `anvil-org-index-search-hard-limit'.
  :offset            integer, default 0.

Returns:
  (:count N :truncated BOOL :rows (PLIST ...))
Each row plist carries `:file', `:line', `:level', `:title', `:todo',
`:priority', `:tags', `:scheduled', `:deadline', `:org-id'.

Much cheaper than repeated `org-map-entries' scans because the
whole query runs as a single indexed SQL statement."
  (unless anvil-org-index--db (anvil-org-index-enable))
  (let* ((todos (anvil-org-index--search-normalise-list todo))
         (tags  (anvil-org-index--search-normalise-list tag))
         (lim   (min (or limit anvil-org-index-search-default-limit)
                     anvil-org-index-search-hard-limit))
         (off   (or offset 0))
         (where '())
         (args  '()))
    (when-let* ((p (anvil-org-index--search-like-wrap title-like)))
      (push "h.title LIKE ?" where) (push p args))
    (when-let* ((p (anvil-org-index--search-like-wrap file-like)))
      (push "f.path LIKE ?" where) (push p args))
    (when todos
      (push (format "h.todo IN (%s)"
                    (mapconcat (lambda (_) "?") todos ","))
            where)
      (setq args (append (reverse todos) args)))
    (dolist (tg tags)
      (push "EXISTS (SELECT 1 FROM tag t WHERE t.headline_id = h.id AND t.tag = ?)"
            where)
      (push tg args))
    (let ((pval (anvil-org-index--search-like-wrap property-value)))
      (cond
       ((and property-key pval)
        (push (concat "EXISTS (SELECT 1 FROM property p "
                      "WHERE p.headline_id = h.id "
                      "AND p.key = ? AND p.value LIKE ?)")
              where)
        (push property-key args)
        (push pval args))
       (property-key
        (push (concat "EXISTS (SELECT 1 FROM property p "
                      "WHERE p.headline_id = h.id AND p.key = ?)")
              where)
        (push property-key args))
       (pval
        (push (concat "EXISTS (SELECT 1 FROM property p "
                      "WHERE p.headline_id = h.id AND p.value LIKE ?)")
              where)
        (push pval args))))
    (when scheduled-before
      (push (anvil-org-index--search-ts-substr "h.scheduled" "<=") where)
      (push scheduled-before args))
    (when scheduled-after
      (push (anvil-org-index--search-ts-substr "h.scheduled" ">=") where)
      (push scheduled-after args))
    (when deadline-before
      (push (anvil-org-index--search-ts-substr "h.deadline" "<=") where)
      (push deadline-before args))
    (when deadline-after
      (push (anvil-org-index--search-ts-substr "h.deadline" ">=") where)
      (push deadline-after args))
    (let* ((where-sql (if where
                          (concat "WHERE " (mapconcat #'identity
                                                      (nreverse where)
                                                      " AND "))
                        ""))
           (sql (format
                 "SELECT f.path, h.line_start, h.level, h.title,
                         h.todo, h.priority, h.scheduled, h.deadline,
                         h.org_id,
                         (SELECT GROUP_CONCAT(t.tag, ',')
                            FROM tag t WHERE t.headline_id = h.id)
                    FROM headline h
                    JOIN file f ON h.file_id = f.id
                    %s
                ORDER BY f.path, h.position
                   LIMIT ? OFFSET ?"
                 where-sql))
           (params (append (nreverse args) (list (1+ lim) off)))
           (rows (anvil-org-index--select anvil-org-index--db sql params))
           (truncated (> (length rows) lim))
           (clipped (if truncated (cl-subseq rows 0 lim) rows)))
      (list :count     (length clipped)
            :truncated truncated
            :rows
            (mapcar
             (lambda (r)
               (list :file      (nth 0 r)
                     :line      (nth 1 r)
                     :level     (nth 2 r)
                     :title     (nth 3 r)
                     :todo      (nth 4 r)
                     :priority  (nth 5 r)
                     :scheduled (nth 6 r)
                     :deadline  (nth 7 r)
                     :org-id    (nth 8 r)
                     :tags      (let ((concat (nth 9 r)))
                                  (and concat (split-string concat "," t)))))
             clipped)))))

(defun anvil-org-index--tool-search
    (&optional title-like file-like todo tag
               property-key property-value
               scheduled-before scheduled-after
               deadline-before  deadline-after
               limit offset)
  "MCP wrapper for `anvil-org-index-search'.

All parameters are optional strings; omit or pass empty to skip.

MCP Parameters:
  title-like       - SQL LIKE pattern on headline title (auto-wrap
                     with %..% when no % present).
  file-like        - SQL LIKE pattern on absolute file path.
  todo             - TODO keyword or comma-separated list (OR).
  tag              - Tag or comma-separated list (AND).
  property-key     - Exact match on PROPERTIES drawer key (case
                     preserved — typically UPPERCASE).
  property-value   - SQL LIKE pattern on property value; combines
                     with property-key on the same row.
  scheduled-before - ISO \"YYYY-MM-DD\" — SCHEDULED <= date.
  scheduled-after  - ISO \"YYYY-MM-DD\" — SCHEDULED >= date.
  deadline-before  - ISO \"YYYY-MM-DD\" — DEADLINE  <= date.
  deadline-after   - ISO \"YYYY-MM-DD\" — DEADLINE  >= date.
  limit            - integer string, default 100, hard-capped at 1000.
  offset           - integer string, default 0."
  (anvil-server-with-error-handling
   (let* ((int-or-nil
           (lambda (s)
             (and s (stringp s)
                  (not (string-empty-p (string-trim s)))
                  (string-to-number s))))
          (none-empty
           (lambda (s)
             (and s (stringp s)
                  (not (string-empty-p (string-trim s)))
                  s))))
     (format "%S"
             (anvil-org-index-search
              :title-like       (funcall none-empty title-like)
              :file-like        (funcall none-empty file-like)
              :todo             (funcall none-empty todo)
              :tag              (funcall none-empty tag)
              :property-key     (funcall none-empty property-key)
              :property-value   (funcall none-empty property-value)
              :scheduled-before (funcall none-empty scheduled-before)
              :scheduled-after  (funcall none-empty scheduled-after)
              :deadline-before  (funcall none-empty deadline-before)
              :deadline-after   (funcall none-empty deadline-after)
              :limit            (funcall int-or-nil limit)
              :offset           (funcall int-or-nil offset))))))

(defun anvil-org-index--tool-rebuild (&optional paths)
  "MCP offload handler — rebuild the entire org-index.

Runs in an offload subprocess (`:offload t :resumable t').  Opens
the DB directly, bypassing the full `anvil-org-index-enable' so
the subprocess does not re-register MCP tools in its own dying
process.  Returns the rebuild summary plist serialized via
`format \"%S\"'.

MCP Parameters:
  paths - Optional comma-separated list of directory paths to
          rebuild.  Empty or omitted means every directory in
          the configured `anvil-org-index-paths'."
  (anvil-server-with-error-handling
   (unless anvil-org-index--backend
     (setq anvil-org-index--backend (anvil-org-index--detect-backend)))
   (unless anvil-org-index--db
     (let ((dir (file-name-directory anvil-org-index-db-path)))
       (unless (file-directory-p dir) (make-directory dir t))
       (setq anvil-org-index--db
             (anvil-org-index--open anvil-org-index-db-path))
       (anvil-org-index--apply-ddl anvil-org-index--db)))
   (let ((path-list
          (and paths (stringp paths)
               (not (string-empty-p (string-trim paths)))
               (mapcar #'string-trim (split-string paths ",")))))
     (format "%S" (anvil-org-index-rebuild path-list)))))

;;;###autoload
(defun anvil-org-index-status ()
  "Show a summary of the current index state."
  (interactive)
  (if (not anvil-org-index--db)
      (message "anvil-org-index: not enabled")
    (let* ((files (caar (anvil-org-index--select
                         anvil-org-index--db "SELECT COUNT(*) FROM file")))
           (heads (caar (anvil-org-index--select
                         anvil-org-index--db "SELECT COUNT(*) FROM headline")))
           (tags  (caar (anvil-org-index--select
                         anvil-org-index--db "SELECT COUNT(DISTINCT tag) FROM tag")))
           (size  (if (file-exists-p anvil-org-index-db-path)
                      (file-attribute-size
                       (file-attributes anvil-org-index-db-path))
                    0)))
      (message "anvil-org-index: %d file(s), %d headline(s), %d tag(s), db=%s (%.1fKB)"
               files heads tags
               anvil-org-index-db-path
               (/ size 1024.0)))))

(provide 'anvil-org-index)
;;; anvil-org-index.el ends here
