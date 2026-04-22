;;; anvil-memory.el --- Auto-memory metadata index + TTL audit  -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 29 Phase 1a — metadata index + scan walker + per-type TTL
;; audit + access tracker for Claude Code auto-memory.
;;
;; Motivation (token-savior benchmark): file-based auto-memory
;; silently accumulates stale entries; projects older than 3 months
;; and references with broken URLs still show up in `MEMORY.md' and
;; waste context budget.  A metadata index + TTL policy flags
;; expired / needs-recheck rows so the memory-pruner skill can act
;; on concrete candidates instead of scanning file contents every
;; run.
;;
;; Scope split (Doc 29):
;;
;;   Phase 1a (this commit)  — scan + audit + access + list.
;;                             Non-destructive: the index is a
;;                             SQLite DB at
;;                             `anvil-memory-db-path'; memory
;;                             .md files are only read, never
;;                             written.
;;
;;   Phase 1b (separate)      — FTS5 search (`memory-search'),
;;                             contradiction detection
;;                             (`memory-save-check'), URL HEAD
;;                             check on reference-type rows.
;;
;;   Phase 2 (separate)       — decay-score ordering + promote +
;;                             MDL distillation.
;;
;; Public Elisp API:
;;   (anvil-memory-scan &optional ROOTS)  -> count
;;   (anvil-memory-audit &optional TYPE)  -> list of plists
;;   (anvil-memory-access FILE)           -> new access-count or nil
;;   (anvil-memory-list &optional TYPE)   -> list of plists
;;
;; MCP tools (emacs-eval server):
;;   memory-scan    — (re-)populate the metadata index
;;   memory-audit   — TTL policy flags per row
;;   memory-access  — bump access-count + last-accessed
;;   memory-list    — dump the index (optionally filtered by type)
;;
;; The module lives in `anvil-optional-modules' because it depends
;; on Emacs 29+ built-in SQLite and needs an explicit opt-in from
;; the user; nothing in the core module set references memory
;; metadata.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-server)


;;;; --- group + defcustoms -------------------------------------------------

(defgroup anvil-memory nil
  "Auto-memory metadata index for anvil."
  :group 'anvil
  :prefix "anvil-memory-")

(defconst anvil-memory--server-id "emacs-eval"
  "Server id under which memory-* MCP tools register.")

(defcustom anvil-memory-db-path
  (expand-file-name "anvil-memory-index.db" user-emacs-directory)
  "SQLite file backing the memory metadata index."
  :type 'file
  :group 'anvil-memory)

(defcustom anvil-memory-roots nil
  "List of directories to scan for .md memory files.
When nil, `anvil-memory-scan' auto-detects every memory/ directory
under ~/.claude/projects/*/ so all project-local auto-memories are
indexed in a single DB.  Explicit list overrides auto-detection."
  :type '(choice (const :tag "Auto-detect" nil)
                 (repeat directory))
  :group 'anvil-memory)

(defconst anvil-memory-supported '(scan audit access list)
  "Capability tags this module currently provides.
Tests in tests/anvil-memory-test.el gate their `skip-unless' on
membership here so a half-shipped feature never breaks CI.")

(defconst anvil-memory-ttl-policies
  '((user      . (:hard nil      :soft nil))
    (feedback  . (:hard nil      :soft 15552000))   ; soft 180d
    (project   . (:hard 7776000  :soft 5184000))    ; hard 90d / soft 60d
    (reference . (:hard 2592000  :soft 1814400))    ; hard 30d / soft 21d
    (memo      . (:hard nil      :soft nil)))
  "Per-type TTL policy used by `anvil-memory-audit'.
Each value is a plist with `:hard' (seconds until the memory is
flagged expired) and `:soft' (seconds until it is flagged
needs-recheck).  nil in either slot disables that flag for the
type.  Durations match the Doc 29 Phase 1 policy table.")


;;;; --- sqlite backend ----------------------------------------------------

(defvar anvil-memory--db nil
  "Open handle on `anvil-memory-db-path' (nil when the module is idle).")

(defun anvil-memory--require-sqlite ()
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (user-error
     "anvil-memory: Emacs SQLite backend unavailable (needs Emacs 29+)")))

(defun anvil-memory--open ()
  "Open the backing DB and apply schema."
  (anvil-memory--require-sqlite)
  (unless anvil-memory--db
    (make-directory (file-name-directory anvil-memory-db-path) t)
    (setq anvil-memory--db (sqlite-open anvil-memory-db-path))
    (anvil-memory--ensure-schema))
  anvil-memory--db)

(defun anvil-memory--close ()
  (when anvil-memory--db
    (ignore-errors (sqlite-close anvil-memory--db))
    (setq anvil-memory--db nil)))

(defun anvil-memory--db ()
  (or anvil-memory--db (anvil-memory--open)))

(defun anvil-memory--ensure-schema ()
  (sqlite-execute anvil-memory--db
                  "CREATE TABLE IF NOT EXISTS memory_meta (
                     file TEXT PRIMARY KEY,
                     type TEXT NOT NULL,
                     created INTEGER NOT NULL,
                     last_accessed INTEGER,
                     access_count INTEGER DEFAULT 0,
                     validity_prior REAL DEFAULT 0.9,
                     ttl_policy TEXT,
                     decay_score REAL DEFAULT 0.0,
                     tags TEXT
                   )")
  (sqlite-execute anvil-memory--db
                  "CREATE INDEX IF NOT EXISTS memory_meta_type_idx
                     ON memory_meta(type)"))


;;;; --- type inference + row → plist ---------------------------------------

(defun anvil-memory--infer-type (filename)
  "Return the memory type symbol inferred from FILENAME's prefix."
  (cond
   ((string-prefix-p "feedback_"  filename) 'feedback)
   ((string-prefix-p "project_"   filename) 'project)
   ((string-prefix-p "reference_" filename) 'reference)
   ((string-prefix-p "user_"      filename) 'user)
   (t 'memo)))

(defun anvil-memory--row->plist (row)
  "Convert a 9-column memory_meta ROW into a caller-friendly plist."
  (list :file (nth 0 row)
        :type (intern (nth 1 row))
        :created (nth 2 row)
        :last-accessed (nth 3 row)
        :access-count (or (nth 4 row) 0)
        :validity-prior (or (nth 5 row) 0.9)
        :ttl-policy (and (nth 6 row) (intern (nth 6 row)))
        :decay-score (or (nth 7 row) 0.0)
        :tags (nth 8 row)))


;;;; --- scan ---------------------------------------------------------------

(defun anvil-memory--effective-roots ()
  "Return the list of memory directories to scan.
`anvil-memory-roots' wins when non-nil; otherwise every
`memory/' subdirectory under `~/.claude/projects/*/' is returned."
  (cond
   (anvil-memory-roots
    (cl-remove-if-not #'file-directory-p anvil-memory-roots))
   (t
    (let ((projects-dir (expand-file-name "~/.claude/projects")))
      (when (file-directory-p projects-dir)
        (cl-loop for sub in (directory-files projects-dir t "\\`[^.]")
                 for memdir = (expand-file-name "memory" sub)
                 when (file-directory-p memdir)
                 collect memdir))))))

;;;###autoload
(defun anvil-memory-scan (&optional roots)
  "Walk ROOTS (or `anvil-memory--effective-roots' when omitted) and
(re-)register every .md file under them in the metadata index.
`MEMORY.md' index files are skipped.  Existing rows are preserved
on conflict (so access-count and validity-prior are never reset).
Returns the number of .md files seen."
  (let* ((roots (or roots (anvil-memory--effective-roots)))
         (db (anvil-memory--db))
         (n 0))
    (dolist (root roots)
      (when (file-directory-p root)
        (dolist (path (directory-files root t "\\.md\\'"))
          (let ((base (file-name-nondirectory path)))
            (unless (equal base "MEMORY.md")
              (let* ((type (anvil-memory--infer-type base))
                     (mtime (truncate (float-time
                                       (nth 5 (file-attributes path))))))
                (sqlite-execute
                 db
                 "INSERT INTO memory_meta(file, type, created, ttl_policy)
                    VALUES (?1, ?2, ?3, ?2)
                    ON CONFLICT(file) DO NOTHING"
                 (list path (symbol-name type) mtime))
                (cl-incf n)))))))
    n))


;;;; --- list ---------------------------------------------------------------

;;;###autoload
(defun anvil-memory-list (&optional type)
  "Return every indexed memory row, optionally filtered by TYPE symbol.
Rows are ordered by file path.  Each row is a plist with keys
:file / :type / :created / :last-accessed / :access-count /
:validity-prior / :ttl-policy / :decay-score / :tags."
  (let* ((db (anvil-memory--db))
         (rows (if type
                   (sqlite-select
                    db
                    "SELECT file, type, created, last_accessed,
                            access_count, validity_prior, ttl_policy,
                            decay_score, tags
                       FROM memory_meta
                      WHERE type = ?1
                   ORDER BY file"
                    (list (symbol-name type)))
                 (sqlite-select
                  db
                  "SELECT file, type, created, last_accessed,
                          access_count, validity_prior, ttl_policy,
                          decay_score, tags
                     FROM memory_meta
                 ORDER BY file"))))
    (mapcar #'anvil-memory--row->plist rows)))


;;;; --- access tracker -----------------------------------------------------

;;;###autoload
(defun anvil-memory-access (file)
  "Record a read of memory FILE.
Increments `access_count' and sets `last_accessed' to the current
epoch.  Returns the new access-count, or nil when FILE is not in
the index (never signals — the caller can use the nil return to
decide whether to `anvil-memory-scan' first)."
  (let* ((db (anvil-memory--db))
         (now (truncate (float-time)))
         (rows (sqlite-select
                db
                "SELECT access_count FROM memory_meta WHERE file = ?1"
                (list file))))
    (when rows
      (sqlite-execute
       db
       "UPDATE memory_meta
           SET access_count = access_count + 1,
               last_accessed = ?1
         WHERE file = ?2"
       (list now file))
      (1+ (or (car (car rows)) 0)))))


;;;; --- audit --------------------------------------------------------------

;;;###autoload
(defun anvil-memory-audit (&optional type)
  "Apply `anvil-memory-ttl-policies' to every row (or filtered by TYPE).
Returns a list of plists extending each `anvil-memory-list' row
with:
  :age-days  integer days since `:created'
  :flag      `expired' | `needs-recheck' | nil
  :reason    short string (nil when `:flag' is nil)"
  (let* ((rows (anvil-memory-list type))
         (now (truncate (float-time))))
    (mapcar
     (lambda (row)
       (let* ((typ (plist-get row :type))
              (created (plist-get row :created))
              (age-sec (- now created))
              (age-days (/ age-sec 86400))
              (policy (cdr (assoc typ anvil-memory-ttl-policies)))
              (hard (plist-get policy :hard))
              (soft (plist-get policy :soft))
              flag reason)
         (cond
          ((and hard (> age-sec hard))
           (setq flag 'expired reason "hard-ttl-exceeded"))
          ((and soft (> age-sec soft))
           (setq flag 'needs-recheck reason "soft-window")))
         (append row (list :age-days age-days
                           :flag flag
                           :reason reason))))
     rows)))


;;;; --- MCP tool handlers --------------------------------------------------

(defun anvil-memory--coerce-type (v)
  "Coerce V (string / symbol / nil) to a type symbol or nil."
  (cond ((null v) nil)
        ((and (stringp v) (string-empty-p v)) nil)
        ((stringp v) (intern v))
        ((symbolp v) v)
        (t nil)))

(defun anvil-memory--tool-scan (&optional roots)
  "Scan ROOTS (or the auto-detected ~/.claude/projects/*/memory list).

MCP Parameters:
  roots - Optional colon-separated list of directories.  Empty /
          omitted delegates to the auto-detect path.

Returns (:count N :roots DIRS)."
  (anvil-server-with-error-handling
   (let* ((roots* (cond ((null roots) nil)
                        ((listp roots) roots)
                        ((and (stringp roots) (string-empty-p roots)) nil)
                        ((stringp roots) (split-string roots ":" t))
                        (t nil)))
          (resolved (or roots* (anvil-memory--effective-roots)))
          (n (anvil-memory-scan roots*)))
     (list :count n :roots resolved))))

(defun anvil-memory--tool-audit (&optional type)
  "Run the per-type TTL audit against the index.

MCP Parameters:
  type - Optional memory type symbol (user / feedback / project /
         reference / memo).  Empty / omitted audits every row.

Returns (:entries ROWS) where each ROW extends the list shape
with :age-days / :flag / :reason."
  (anvil-server-with-error-handling
   (let ((typ (anvil-memory--coerce-type type)))
     (list :entries (anvil-memory-audit typ)))))

(defun anvil-memory--tool-access (file)
  "Record an access of the memory identified by FILE.

MCP Parameters:
  file - Absolute path of the memory .md file (as stored in the
         index).

Returns (:file FILE :access-count N :found BOOL).  `:found' is
nil when FILE is not in the index; `:access-count' is then nil."
  (anvil-server-with-error-handling
   (let ((new-count (anvil-memory-access file)))
     (list :file file
           :access-count new-count
           :found (and new-count t)))))

(defun anvil-memory--tool-list (&optional type)
  "Dump the memory metadata index.

MCP Parameters:
  type - Optional memory type to filter by.  Empty / omitted
         returns every row.

Returns (:rows ROWS) ordered by file path."
  (anvil-server-with-error-handling
   (let ((typ (anvil-memory--coerce-type type)))
     (list :rows (anvil-memory-list typ)))))


;;;; --- module lifecycle ---------------------------------------------------

(defconst anvil-memory--tool-specs
  `((,(anvil-server-encode-handler #'anvil-memory--tool-scan)
     :id "memory-scan"
     :description
     "(Re-)populate the auto-memory metadata index.  Walks every memory/
directory under ~/.claude/projects/*/ (or an explicit ROOTS list),
reads each .md file's mtime as `created' when the row is new, and
leaves access_count / validity_prior untouched on conflict so
existing metadata survives a rescan.")

    (,(anvil-server-encode-handler #'anvil-memory--tool-audit)
     :id "memory-audit"
     :description
     "Apply the per-type TTL policy and return flagged rows.  Each entry
carries :age-days plus a :flag field — `expired' (hard-ttl
exceeded) or `needs-recheck' (inside the soft window) or nil."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory--tool-access)
     :id "memory-access"
     :description
     "Bump access_count and last_accessed for a memory .md file.  Returns
the new access count; `:found' is nil when the file is not in
the index (run `memory-scan' first in that case).")

    (,(anvil-server-encode-handler #'anvil-memory--tool-list)
     :id "memory-list"
     :description
     "Dump the metadata index, optionally filtered by TYPE.  Intended as
a Layer-1 slim overview for memory-pruner / audit workflows
rather than for loading memory contents."
     :read-only t))
  "Spec list consumed by `anvil-server-register-tools'.")

(defun anvil-memory--register-tools ()
  (anvil-server-register-tools anvil-memory--server-id
                               anvil-memory--tool-specs))

(defun anvil-memory--unregister-tools ()
  (anvil-server-unregister-tools anvil-memory--server-id
                                 anvil-memory--tool-specs))

;;;###autoload
(defun anvil-memory-enable ()
  "Open the metadata DB and register memory-* MCP tools."
  (interactive)
  (anvil-memory--open)
  (anvil-memory--register-tools))

;;;###autoload
(defun anvil-memory-disable ()
  "Unregister memory-* MCP tools and close the metadata DB."
  (interactive)
  (anvil-memory--unregister-tools)
  (anvil-memory--close))


(provide 'anvil-memory)

;;; anvil-memory.el ends here
