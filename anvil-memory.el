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

(defconst anvil-memory-supported
  '(scan audit access list
         search save-check duplicates audit-urls
         decay promote regenerate reindex-fts)
  "Capability tags this module currently provides.
Tests in tests/anvil-memory-test.el gate their `skip-unless' on
membership here so a half-shipped feature never breaks CI.")

(defcustom anvil-memory-decay-half-life-days 30
  "Days after which the recency component of decay-score decays to ~1/e.
Used by `anvil-memory--compute-decay'.  Defaults to the 30-day
half-life from Doc 29 architecture.  Shortening this biases the
ranking toward very fresh memories; lengthening it keeps long-
lived feedback rows visible longer."
  :type 'integer
  :group 'anvil-memory)

(defcustom anvil-memory-decay-access-saturation 10
  "Access count at which the decay-score access component saturates.
Counts above this cap no longer boost the score; chosen so a
memory referenced 10 sessions in a row reaches the maximum access
contribution."
  :type 'integer
  :group 'anvil-memory)

(defcustom anvil-memory-decay-type-weights
  '((user      . 1.0)
    (feedback  . 0.9)
    (project   . 0.7)
    (reference . 0.5)
    (memo      . 0.4))
  "Per-type weight mixed into `anvil-memory--compute-decay'.
Higher weights buoy long-lived rows (user / feedback) against the
pure recency signal, so rarely-read-but-load-bearing memories do
not drop off the MEMORY.md index."
  :type '(alist :key-type symbol :value-type number)
  :group 'anvil-memory)

(defcustom anvil-memory-fts-tokenizer 'auto
  "Tokenizer used when (re-)creating the `memory_body_fts' table.
Valid values:
  `auto'      — probe the SQLite build: use `trigram' when the FTS5
                trigram extension is available (SQLite 3.34+), else
                fall back to `unicode61'.
  `trigram'   — force the trigram tokenizer (better CJK substring
                hits).  Errors at create time on older SQLite.
  `unicode61' — force the SQLite default tokenizer (legacy).

Changing this value does NOT rebuild the on-disk FTS table on its
own — call `anvil-memory-reindex-fts' (or the `memory-reindex-fts'
MCP tool) to swap tokenizers in-place."
  :type '(choice (const :tag "Auto (trigram if available)" auto)
                 (const :tag "Trigram (SQLite 3.34+)" trigram)
                 (const :tag "unicode61 (default)" unicode61))
  :group 'anvil-memory)

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

(defun anvil-memory--sqlite-supports-trigram-p (db)
  "Return non-nil when DB's SQLite build ships the FTS5 trigram tokenizer.
Probes by creating (and dropping) a throwaway virtual table; any
signalled error is treated as an unsupported build."
  (condition-case nil
      (progn
        (sqlite-execute
         db
         "CREATE VIRTUAL TABLE anvil_memory_trigram_probe
            USING fts5(x, tokenize=trigram)")
        (sqlite-execute db "DROP TABLE anvil_memory_trigram_probe")
        t)
    (error nil)))

(defun anvil-memory--resolve-tokenizer (db)
  "Return the tokenizer symbol (`trigram' / `unicode61') DB should use.
Respects `anvil-memory-fts-tokenizer'; `auto' consults
`anvil-memory--sqlite-supports-trigram-p' to decide."
  (pcase anvil-memory-fts-tokenizer
    ('trigram 'trigram)
    ('unicode61 'unicode61)
    ('auto (if (anvil-memory--sqlite-supports-trigram-p db)
               'trigram
             'unicode61))
    (other (user-error
            "anvil-memory: invalid `anvil-memory-fts-tokenizer': %S"
            other))))

(defun anvil-memory--current-fts-tokenizer (db)
  "Return the tokenizer symbol currently configured on memory_body_fts.
Parses the stored CREATE statement from sqlite_master; returns nil
when the table does not exist."
  (let ((rows (sqlite-select
               db
               "SELECT sql FROM sqlite_master WHERE name = 'memory_body_fts'")))
    (when rows
      (let ((sql (or (nth 0 (car rows)) "")))
        (if (string-match-p "tokenize *= *['\"]?trigram" sql)
            'trigram
          'unicode61)))))

(defun anvil-memory--create-fts-table (db tokenizer)
  "Create memory_body_fts on DB using TOKENIZER (symbol).
Uses `IF NOT EXISTS' so first-open is a no-op when the table is
already present with a matching tokenizer."
  (let ((tok-sql (pcase tokenizer
                   ('trigram   ", tokenize='trigram'")
                   ('unicode61 "")
                   (_ (user-error
                       "anvil-memory: invalid tokenizer: %S" tokenizer)))))
    (sqlite-execute
     db
     (format "CREATE VIRTUAL TABLE IF NOT EXISTS memory_body_fts
                USING fts5(file UNINDEXED, body%s)"
             tok-sql))))

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
                     ON memory_meta(type)")
  ;; FTS5 virtual table carrying memory bodies for full-text
  ;; search / jaccard-based save-check / duplicate detection.
  ;; Phase 2b-i: tokenizer is now resolvable (`auto' picks trigram
  ;; on SQLite 3.34+ for better CJK substring hits; existing
  ;; deployments keep unicode61 until `anvil-memory-reindex-fts'
  ;; rebuilds the table).
  (anvil-memory--create-fts-table
   anvil-memory--db
   (anvil-memory--resolve-tokenizer anvil-memory--db)))


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

(defun anvil-memory--read-body-utf8 (path)
  "Return the UTF-8 decoded body of PATH as a string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents path))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun anvil-memory-scan (&optional roots)
  "Walk ROOTS (or `anvil-memory--effective-roots' when omitted) and
(re-)register every .md file under them in the metadata index.
`MEMORY.md' index files are skipped.  Existing metadata rows are
preserved on conflict (so access_count / validity_prior are never
reset).  The FTS body index is always refreshed per file so
re-scanning a changed .md reflects immediately in `memory-search'.
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
                                       (nth 5 (file-attributes path)))))
                     (body (ignore-errors
                             (anvil-memory--read-body-utf8 path))))
                (sqlite-execute
                 db
                 "INSERT INTO memory_meta(file, type, created, ttl_policy)
                    VALUES (?1, ?2, ?3, ?2)
                    ON CONFLICT(file) DO NOTHING"
                 (list path (symbol-name type) mtime))
                (sqlite-execute
                 db
                 "DELETE FROM memory_body_fts WHERE file = ?1"
                 (list path))
                (sqlite-execute
                 db
                 "INSERT INTO memory_body_fts(file, body) VALUES (?1, ?2)"
                 (list path (or body "")))
                (cl-incf n)))))))
    n))


;;;; --- list ---------------------------------------------------------------

(defun anvil-memory--compute-decay (row)
  "Compute a [0, 1] decay-score for ROW.
Formula (Doc 29 architecture):
  decay = 0.4 * recency(last-accessed)
        + 0.3 * access(access-count, saturation)
        + 0.3 * type-weight(type)
  recency(t) = exp(-days-since(t) / half-life)
  access(n)  = min(1, n / saturation)

An unaccessed row (`:last-accessed' nil) contributes 0 recency —
those rows are deliberately ranked on type-weight alone so the
MEMORY.md index does not spike a brand-new file above a proven
feedback rule."
  (let* ((now (truncate (float-time)))
         (last (plist-get row :last-accessed))
         (count (or (plist-get row :access-count) 0))
         (type (plist-get row :type))
         (half-life (max 1 anvil-memory-decay-half-life-days))
         (sat (max 1 anvil-memory-decay-access-saturation))
         (recency (if last
                      (let ((days (/ (float (- now last)) 86400.0)))
                        (exp (- (/ days (float half-life)))))
                    0.0))
         (access (min 1.0 (/ (float count) (float sat))))
         (tw (or (cdr (assoc type anvil-memory-decay-type-weights)) 0.5)))
    (+ (* 0.4 recency) (* 0.3 access) (* 0.3 tw))))

;;;###autoload
(cl-defun anvil-memory-list (&optional type &key with-decay sort)
  "Return every indexed memory row, optionally filtered by TYPE symbol.
Each row is a plist with keys :file / :type / :created /
:last-accessed / :access-count / :validity-prior / :ttl-policy /
:decay-score / :tags.

  :with-decay   non-nil decorates each row with a freshly-computed
                `:decay-score' float (overrides the stored column
                value, which is never updated in-place by
                `memory-scan').
  :sort         `decay' orders rows by :decay-score descending.
                Any other value (or nil) falls back to file path."
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
                 ORDER BY file")))
         (plists (mapcar #'anvil-memory--row->plist rows)))
    (when with-decay
      ;; `anvil-memory--row->plist' already stashes the stored
      ;; `decay_score' column (default 0.0) under :decay-score.
      ;; Using `append' here would leave the stored 0.0 in front of
      ;; the fresh value and `plist-get' would keep returning the
      ;; stale 0.0.  `plist-put' on a copy updates in-place.
      (setq plists
            (mapcar (lambda (row)
                      (plist-put (copy-sequence row)
                                 :decay-score
                                 (anvil-memory--compute-decay row)))
                    plists)))
    (when (eq sort 'decay)
      (setq plists
            (cl-sort (copy-sequence plists) #'>
                     :key (lambda (r)
                            (or (plist-get r :decay-score) 0.0)))))
    plists))


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

(defun anvil-memory--first-url (body)
  "Return the first http/https URL found in BODY, or nil."
  (when (and body (string-match "\\(https?://[^ \t\n<>\"']+\\)" body))
    (match-string 1 body)))

(defun anvil-memory--check-url-for-row (row)
  "Return `ok' / `broken' / nil for the first URL in ROW's body.
Uses `anvil-http-head' (Doc 09).  Any HTTP status < 400 counts as
`ok', >= 400 counts as `broken', errors / missing backend return
nil."
  (let* ((path (plist-get row :file))
         (body (anvil-memory--get-body path))
         (url (anvil-memory--first-url body)))
    (when (and url (fboundp 'anvil-http-head))
      (let* ((result (ignore-errors (anvil-http-head url)))
             (status (and result (plist-get result :status))))
        (cond
         ((null status) nil)
         ((and (integerp status) (>= status 200) (< status 400)) 'ok)
         ((and (integerp status) (>= status 400)) 'broken)
         (t nil))))))

;;;###autoload
(cl-defun anvil-memory-audit (&optional type &key with-urls)
  "Apply `anvil-memory-ttl-policies' to every row (or filtered by TYPE).
Returns a list of plists extending each `anvil-memory-list' row
with:
  :age-days   integer days since `:created'
  :flag       `expired' | `needs-recheck' | nil
  :reason     short string (nil when `:flag' is nil)
  :url-status `ok' | `broken' | nil   (only when :WITH-URLS non-nil
              AND row is of type `reference'; all other rows leave
              the key absent)"
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
         (let ((extra (list :age-days age-days
                            :flag flag
                            :reason reason)))
           (when (and with-urls (eq typ 'reference))
             (setq extra (append extra
                                 (list :url-status
                                       (anvil-memory--check-url-for-row
                                        row)))))
           (append row extra))))
     rows)))


;;;; --- Phase 1b: FTS5 search / jaccard / duplicates ------------------------

(defun anvil-memory--get-body (path)
  "Return the raw body indexed for PATH, or nil when absent."
  (let* ((db (anvil-memory--db))
         (rows (sqlite-select
                db
                "SELECT body FROM memory_body_fts WHERE file = ?1"
                (list path))))
    (car (car rows))))

(defun anvil-memory--tokens (s)
  "Tokenize S into lower-case words ≥3 chars long.
Splits on any non-alphanumeric run so URL fragments / dashes
contribute their sub-words; tokens shorter than 3 chars are
dropped to cut stopwords without a real stoplist."
  (when (and s (stringp s))
    (cl-delete-if (lambda (tok) (< (length tok) 3))
                  (split-string (downcase s) "[^[:alnum:]]+" t))))

(defun anvil-memory--jaccard (xs ys)
  "Jaccard similarity over the two token lists XS / YS (0.0 – 1.0)."
  (let* ((xs-set (delete-dups (copy-sequence xs)))
         (ys-set (delete-dups (copy-sequence ys)))
         (inter (cl-intersection xs-set ys-set :test #'equal))
         (union (cl-union xs-set ys-set :test #'equal)))
    (if (null union)
        0.0
      (/ (float (length inter)) (length union)))))

(defun anvil-memory--tokens-to-fts-query (tokens &optional cap)
  "Turn TOKENS into an FTS5 \"OR\" query string.
CAP limits the number of unique tokens emitted (default 12) so a
wall-of-text draft does not produce a query SQLite refuses."
  (let* ((cap (or cap 12))
         (uniq (cl-delete-duplicates (copy-sequence tokens)
                                     :test #'equal))
         (slice (seq-take uniq cap)))
    (when slice
      (mapconcat (lambda (tok) (format "\"%s\"" tok)) slice " OR "))))

;;;###autoload
(cl-defun anvil-memory-search (query &key type limit)
  "Full-text search the FTS body index for QUERY.
  :type   restrict to that memory type symbol.
  :limit  max number of rows (default 20).
Returns a list of plists (:file :type :snippet :rank) ordered by
FTS5 rank (best first).  Empty / whitespace-only QUERY returns nil."
  (when (and query (stringp query)
             (not (string-empty-p (string-trim query))))
    (let* ((db (anvil-memory--db))
           (limit (or limit 20))
           (sql (if type
                    "SELECT m.file, m.type,
                            snippet(memory_body_fts, 1, '<<', '>>', '...', 16) AS snip,
                            rank
                       FROM memory_body_fts
                       JOIN memory_meta m ON m.file = memory_body_fts.file
                      WHERE memory_body_fts MATCH ?1
                        AND m.type = ?2
                   ORDER BY rank
                      LIMIT ?3"
                  "SELECT m.file, m.type,
                          snippet(memory_body_fts, 1, '<<', '>>', '...', 16) AS snip,
                          rank
                     FROM memory_body_fts
                     JOIN memory_meta m ON m.file = memory_body_fts.file
                    WHERE memory_body_fts MATCH ?1
                 ORDER BY rank
                    LIMIT ?2"))
           (params (if type
                       (list query (symbol-name type) limit)
                     (list query limit)))
           (rows (ignore-errors (sqlite-select db sql params))))
      (mapcar (lambda (r)
                (list :file (nth 0 r)
                      :type (intern (nth 1 r))
                      :snippet (nth 2 r)
                      :rank (nth 3 r)))
              rows))))

;;;###autoload
(defun anvil-memory-save-check (subject body &optional top-n)
  "Return top-N memories similar to the draft SUBJECT/BODY.
TOP-N defaults to 3.  Uses FTS5 keyword search against
SUBJECT + BODY to shortlist candidates, then ranks them by
jaccard overlap against BODY.  Each result plist:
(:file :type :snippet :similarity)."
  (let* ((top-n (or top-n 3))
         (query (anvil-memory--tokens-to-fts-query
                 (append (anvil-memory--tokens subject)
                         (anvil-memory--tokens body))))
         (candidates (when query
                       (anvil-memory-search query :limit (max 10 top-n))))
         (draft-toks (anvil-memory--tokens body))
         scored)
    (dolist (cand candidates)
      (let* ((path (plist-get cand :file))
             (cand-body (anvil-memory--get-body path))
             (sim (anvil-memory--jaccard
                   draft-toks (anvil-memory--tokens cand-body))))
        (when (> sim 0.0)
          (push (list :file path
                      :type (plist-get cand :type)
                      :snippet (plist-get cand :snippet)
                      :similarity sim)
                scored))))
    (let ((ranked (cl-sort scored #'>
                           :key (lambda (x) (plist-get x :similarity)))))
      (seq-take ranked top-n))))

;;;###autoload
(defun anvil-memory-duplicates (&optional threshold)
  "Return pairs of memories whose body jaccard similarity > THRESHOLD.
THRESHOLD defaults to 0.6.  Each entry is a plist
(:pair (FILE1 FILE2) :similarity FLOAT) ordered by :similarity
descending.  O(N²) in the number of indexed memories — fine up to
a few hundred; defer semantic (embedding-based) duplicate
detection to Phase 2."
  (let* ((threshold (or threshold 0.6))
         (db (anvil-memory--db))
         (rows (sqlite-select
                db "SELECT file, body FROM memory_body_fts ORDER BY file"))
         (entries (mapcar (lambda (r)
                            (cons (nth 0 r)
                                  (anvil-memory--tokens (nth 1 r))))
                          rows))
         pairs)
    (let ((tail entries))
      (while (and tail (cdr tail))
        (let ((a (car tail)))
          (dolist (b (cdr tail))
            (let ((sim (anvil-memory--jaccard (cdr a) (cdr b))))
              (when (> sim threshold)
                (push (list :pair (list (car a) (car b))
                            :similarity sim)
                      pairs)))))
        (setq tail (cdr tail))))
    (cl-sort pairs #'>
             :key (lambda (p) (plist-get p :similarity)))))


;;;; --- Phase 2a: promote + regenerate-index ------------------------------

(defconst anvil-memory--type-prefixes
  '("feedback_" "project_" "reference_" "user_")
  "Filename prefixes stripped / prepended by `anvil-memory-promote'.
Order matches `anvil-memory--infer-type' so a `feedback_' file
cannot be mis-stripped as `user_*'.")

(defun anvil-memory--strip-type-prefix (basename)
  "Return BASENAME with any known type prefix removed once."
  (let ((result basename)
        (stripped nil))
    (dolist (p anvil-memory--type-prefixes)
      (unless stripped
        (when (string-prefix-p p result)
          (setq result (substring result (length p))
                stripped t))))
    result))

;;;###autoload
(defun anvil-memory-promote (old-file new-type)
  "Rename OLD-FILE on disk to reflect NEW-TYPE's filename prefix and
update the metadata + FTS index accordingly.

OLD-FILE must exist and be indexed (run `anvil-memory-scan'
first).  NEW-TYPE is one of the `anvil-memory-ttl-policies'
symbols (user / feedback / project / reference / memo).  The
destination basename is `NEW-TYPE_BASENAME-WITHOUT-OLD-PREFIX'
placed in OLD-FILE's directory; the helper refuses overwrite.

Returns the new absolute path on success.  The memory file's
body is never rewritten — only its filename and index rows are
touched."
  (unless (and old-file (stringp old-file))
    (user-error "anvil-memory-promote: OLD-FILE must be a string"))
  (unless (and new-type (symbolp new-type))
    (user-error "anvil-memory-promote: NEW-TYPE must be a symbol"))
  (unless (file-exists-p old-file)
    (user-error "anvil-memory-promote: source missing: %s" old-file))
  (let* ((db (anvil-memory--db))
         (indexed (sqlite-select
                   db
                   "SELECT 1 FROM memory_meta WHERE file = ?1 LIMIT 1"
                   (list old-file))))
    (unless indexed
      (user-error
       "anvil-memory-promote: file not indexed (run memory-scan): %s"
       old-file))
    (let* ((dir (file-name-directory old-file))
           (stripped (anvil-memory--strip-type-prefix
                      (file-name-nondirectory old-file)))
           (new-base (format "%s_%s" (symbol-name new-type) stripped))
           (new-file (expand-file-name new-base dir)))
      (when (file-exists-p new-file)
        (user-error
         "anvil-memory-promote: destination exists: %s" new-file))
      (rename-file old-file new-file)
      (sqlite-execute
       db
       "UPDATE memory_meta
           SET file = ?1, type = ?2, ttl_policy = ?2
         WHERE file = ?3"
       (list new-file (symbol-name new-type) old-file))
      (sqlite-execute
       db
       "UPDATE memory_body_fts SET file = ?1 WHERE file = ?2"
       (list new-file old-file))
      new-file)))


(defun anvil-memory--parse-frontmatter (body)
  "Extract display metadata from BODY's leading `---' YAML block.
Returns (:name STR :description STR); either can be nil when the
respective key is absent.  No YAML parser is used — a line-level
regexp handles the 4 fields anvil memories actually write."
  (when (and body (string-match "\\`---\n\\([^\0]*?\\)---\n?" body))
    (let* ((fm (match-string 1 body))
           (name nil)
           (desc nil))
      (save-match-data
        (when (string-match "^name:\\s-*\\(.+\\)$" fm)
          (setq name (string-trim (match-string 1 fm))))
        (when (string-match "^description:\\s-*\\(.+\\)$" fm)
          (setq desc (string-trim (match-string 1 fm)))))
      (list :name name :description desc))))

(defun anvil-memory--fallback-display-name (path)
  "Derive a human-friendly title from PATH when frontmatter is absent.
Strips the type prefix + `.md' extension and turns underscores
into spaces."
  (let* ((base (file-name-nondirectory path))
         (stem (file-name-sans-extension base))
         (no-prefix (anvil-memory--strip-type-prefix stem)))
    (replace-regexp-in-string "_" " " no-prefix)))

;;;###autoload
(defun anvil-memory-regenerate-index (root)
  "Return a `MEMORY.md' body listing memories under ROOT.
Rows are ordered by `:decay-score' descending (Phase 2a formula)
so the most load-bearing entries gravitate to the top 200 lines
Claude Code actually loads.  Each line is shaped:

  - [NAME](BASENAME.md) — DESCRIPTION

NAME / DESCRIPTION come from the memory file's YAML frontmatter;
missing fields fall back to a prettified filename / empty string.
The function is read-only: it returns the text — the caller
writes it to disk (the memory-pruner skill owns that step)."
  (unless (and root (stringp root))
    (user-error "anvil-memory-regenerate-index: ROOT must be a string"))
  (let* ((abs-root (file-name-as-directory (expand-file-name root)))
         (rows (anvil-memory-list nil :with-decay t :sort 'decay))
         (scoped (cl-remove-if-not
                  (lambda (r)
                    (string-prefix-p abs-root (plist-get r :file)))
                  rows)))
    (mapconcat
     (lambda (row)
       (let* ((path (plist-get row :file))
              (base (file-name-nondirectory path))
              (body (or (anvil-memory--get-body path) ""))
              (fm (anvil-memory--parse-frontmatter body))
              (name (or (and fm (plist-get fm :name))
                        (anvil-memory--fallback-display-name path)))
              (desc (or (and fm (plist-get fm :description)) "")))
         (if (string-empty-p desc)
             (format "- [%s](%s)" name base)
           (format "- [%s](%s) — %s" name base desc))))
     scoped
     "\n")))


;;;; --- Phase 2b-i: FTS tokenizer reindex ---------------------------------

(defun anvil-memory-reindex-fts (&optional tokenizer)
  "Rebuild `memory_body_fts' with TOKENIZER (symbol).
TOKENIZER nil resolves from `anvil-memory-fts-tokenizer'.  Valid
explicit values: `trigram' / `unicode61'.

Drops the existing FTS virtual table, recreates it, then repopulates
the body index by re-reading each indexed .md file from disk.  Rows
whose underlying file disappeared are silently skipped so a stale
index does not block the rebuild.

Returns (:tokenizer SYM :rebuilt N) — the tokenizer actually
installed and the number of bodies repopulated.  Raises a
`user-error' on invalid TOKENIZER so callers fail fast."
  (let* ((db (anvil-memory--db))
         (tok (cond
               ((null tokenizer) (anvil-memory--resolve-tokenizer db))
               ((memq tokenizer '(trigram unicode61)) tokenizer)
               (t (user-error
                   "anvil-memory-reindex-fts: invalid tokenizer %S"
                   tokenizer))))
         (files (sqlite-select db "SELECT file FROM memory_meta"))
         (rebuilt 0))
    (sqlite-execute db "DROP TABLE IF EXISTS memory_body_fts")
    (anvil-memory--create-fts-table db tok)
    (dolist (row files)
      (let* ((path (nth 0 row))
             (body (and (file-readable-p path)
                        (ignore-errors
                          (anvil-memory--read-body-utf8 path)))))
        (when body
          (sqlite-execute
           db
           "INSERT INTO memory_body_fts(file, body) VALUES (?1, ?2)"
           (list path body))
          (cl-incf rebuilt))))
    (list :tokenizer tok :rebuilt rebuilt)))


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

(defun anvil-memory--coerce-int (v default)
  "Coerce V (integer / digit-string / nil) to an integer, else DEFAULT."
  (cond ((integerp v) v)
        ((and (stringp v) (string-match "\\`[0-9]+\\'" v))
         (string-to-number v))
        (t default)))

(defun anvil-memory--coerce-number (v default)
  "Coerce V (number / numeric-string / nil) to a number, else DEFAULT."
  (cond ((numberp v) v)
        ((and (stringp v)
              (string-match "\\`-?[0-9]+\\(\\.[0-9]+\\)?\\'" v))
         (string-to-number v))
        (t default)))

(defun anvil-memory--tool-audit (&optional type with_urls)
  "Run the per-type TTL audit against the index.

MCP Parameters:
  type      - Optional memory type symbol (user / feedback /
              project / reference / memo).  Empty / omitted audits
              every row.
  with_urls - Truthy (non-nil / non-empty string) opts into
              `anvil-http-head' checks against the first URL in
              each reference-type row; the row gains :url-status
              (`ok' / `broken' / nil).

Returns (:entries ROWS) where each ROW extends the list shape
with :age-days / :flag / :reason, and — when WITH_URLS was on and
the row is a reference — :url-status."
  (anvil-server-with-error-handling
   (let* ((typ (anvil-memory--coerce-type type))
          (urls (cond ((null with_urls) nil)
                      ((and (stringp with_urls)
                            (string-empty-p with_urls))
                       nil)
                      (t t))))
     (list :entries (anvil-memory-audit typ :with-urls urls)))))

(defun anvil-memory--tool-search (query &optional type limit)
  "Full-text search the memory FTS index.

MCP Parameters:
  query - FTS5 query string.  Empty / whitespace returns (:rows nil).
  type  - Optional memory type filter.
  limit - Optional max result count (default 20, digit-string accepted).

Returns (:rows ROWS) ordered by FTS rank (best first)."
  (anvil-server-with-error-handling
   (list :rows
         (anvil-memory-search
          query
          :type (anvil-memory--coerce-type type)
          :limit (anvil-memory--coerce-int limit 20)))))

(defun anvil-memory--tool-save-check (subject body)
  "Return top-N indexed memories similar to a draft SUBJECT/BODY.

MCP Parameters:
  subject - Draft memory subject (or empty).
  body    - Draft memory body.  FTS5 shortlist + jaccard ranking.

Returns (:candidates ROWS), each :file :type :snippet :similarity."
  (anvil-server-with-error-handling
   (list :candidates
         (anvil-memory-save-check (or subject "") (or body "")))))

(defun anvil-memory--tool-duplicates (&optional threshold)
  "Return memory pairs whose body jaccard overlap exceeds THRESHOLD.

MCP Parameters:
  threshold - Float (or numeric string).  Default 0.6.

Returns (:pairs PAIRS) sorted by :similarity descending."
  (anvil-server-with-error-handling
   (list :pairs
         (anvil-memory-duplicates
          (anvil-memory--coerce-number threshold 0.6)))))

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

(defun anvil-memory--tool-list (&optional type with_decay sort)
  "Dump the memory metadata index.

MCP Parameters:
  type       - Optional memory type to filter by.  Empty /
               omitted returns every row.
  with_decay - Truthy (non-nil / non-empty string) attaches a
               freshly-computed :decay-score to each row (Phase
               2a formula).
  sort       - `decay' sorts rows by :decay-score descending.
               Any other value (or empty / omitted) keeps the
               default file-path ordering.

Returns (:rows ROWS)."
  (anvil-server-with-error-handling
   (let* ((typ (anvil-memory--coerce-type type))
          (wd (cond ((null with_decay) nil)
                    ((and (stringp with_decay) (string-empty-p with_decay))
                     nil)
                    (t t)))
          (sort-sym (cond ((null sort) nil)
                          ((and (stringp sort) (string-empty-p sort)) nil)
                          ((stringp sort) (intern sort))
                          ((symbolp sort) sort)
                          (t nil))))
     (list :rows
           (anvil-memory-list typ :with-decay wd :sort sort-sym)))))

(defun anvil-memory--tool-promote (old_file new_type)
  "Rename an indexed memory file and update its type.

MCP Parameters:
  old_file - Absolute path as stored in the index.  Required.
  new_type - Target type symbol / string (user / feedback /
             project / reference / memo).  Required.

Returns (:old-file :new-file :type) on success.  Signals
`user-error' when OLD_FILE is missing, unindexed, or the new
filename already exists — the rename is refused in every
failure mode so the on-disk state stays consistent."
  (anvil-server-with-error-handling
   (let* ((typ (anvil-memory--coerce-type new_type))
          (_ (unless typ
               (user-error
                "anvil-memory-promote: new_type must be a type symbol")))
          (new-path (anvil-memory-promote old_file typ)))
     (list :old-file old_file
           :new-file new-path
           :type typ))))

(defun anvil-memory--tool-regenerate (root)
  "Return a MEMORY.md body for ROOT ordered by decay-score descending.

MCP Parameters:
  root - Directory whose indexed memory files should be listed.
         Only rows whose `:file' lives under ROOT are included.

Returns (:body TEXT) — read-only; the caller writes the file."
  (anvil-server-with-error-handling
   (list :body (anvil-memory-regenerate-index root))))

(defun anvil-memory--tool-reindex-fts (&optional tokenizer)
  "Rebuild memory_body_fts with TOKENIZER (trigram / unicode61).

MCP Parameters:
  tokenizer - Optional string / symbol.  `trigram' forces the
              CJK-friendly trigram tokenizer (SQLite 3.34+);
              `unicode61' forces the legacy default.  Empty /
              omitted resolves from `anvil-memory-fts-tokenizer'
              (`auto' picks trigram when the build supports it).

Returns (:tokenizer SYM :rebuilt N)."
  (anvil-server-with-error-handling
   (let ((tok (cond
               ((null tokenizer) nil)
               ((and (stringp tokenizer) (string-empty-p tokenizer)) nil)
               ((stringp tokenizer) (intern tokenizer))
               ((symbolp tokenizer) tokenizer)
               (t (user-error
                   "tokenizer: expected string / symbol / nil, got %S"
                   tokenizer)))))
     (anvil-memory-reindex-fts tok))))


;;;; --- module lifecycle ---------------------------------------------------

(defconst anvil-memory--tool-specs
  `((,(anvil-server-encode-handler #'anvil-memory--tool-scan)
     :id "memory-scan"
     :description
     "(Re-)populate the auto-memory metadata index.  Walks every memory/
directory under ~/.claude/projects/*/ (or an explicit ROOTS list),
reads each .md file's mtime as `created' when the row is new, and
leaves access_count / validity_prior untouched on conflict so
existing metadata survives a rescan.  Phase 1b also refreshes the
FTS5 body index for every file encountered.")

    (,(anvil-server-encode-handler #'anvil-memory--tool-audit)
     :id "memory-audit"
     :description
     "Apply the per-type TTL policy and return flagged rows.  Each entry
carries :age-days plus a :flag field — `expired' (hard-ttl
exceeded) or `needs-recheck' (inside the soft window) or nil.
Pass with_urls=true to opt into `anvil-http-head' checks on the
first URL of every reference-type row; the row gains :url-status
(`ok' / `broken' / nil)."
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
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory--tool-search)
     :id "memory-search"
     :description
     "Full-text search the memory FTS5 index.  Returns file / type /
snippet / rank per hit, ordered by FTS5 rank (best first).  Use
this as Layer 2 before `memory-get'-ish full-body reads."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory--tool-save-check)
     :id "memory-save-check"
     :description
     "Return top-N indexed memories similar to a draft SUBJECT/BODY.
FTS5 candidate shortlist plus jaccard overlap scoring (Phase 1b
keyword / Levenshtein baseline; Phase 2 will add LLM
contradiction classification).  Each row carries :similarity in
the 0.0–1.0 range — callers flag >=0.7 as a duplicate / merge
candidate."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory--tool-duplicates)
     :id "memory-duplicates"
     :description
     "Return every memory pair whose body jaccard similarity exceeds
THRESHOLD (default 0.6).  O(N^2) over the index — fine up to a
few hundred memories, defer semantic (embedding) duplicates to
Phase 2."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory--tool-promote)
     :id "memory-promote"
     :description
     "Rename an indexed memory file to reflect NEW_TYPE's prefix and
update metadata + FTS index accordingly.  Refuses overwrite and
missing / unindexed sources so on-disk state stays consistent.
Use this to promote a `MEMO'-style note to `feedback' once it
has proven itself across multiple sessions.")

    (,(anvil-server-encode-handler #'anvil-memory--tool-regenerate)
     :id "memory-regenerate-index"
     :description
     "Return a `MEMORY.md' body listing memories under ROOT, ordered
by :decay-score descending.  Lines are `- [NAME](FILE) — DESC'
using YAML frontmatter when present (fallback: prettified file
stem).  Read-only — the caller (memory-pruner skill) writes the
returned body to disk after human review."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory--tool-reindex-fts)
     :id "memory-reindex-fts"
     :description
     "Rebuild the memory_body_fts virtual table with a chosen tokenizer.
Phase 2b-i: `trigram' (SQLite 3.34+) is CJK-friendly — Japanese
substring queries (3+ chars) that miss under `unicode61' start
matching.  Omit tokenizer to use `anvil-memory-fts-tokenizer'
(defaults to `auto', which picks trigram when available).  Returns
:tokenizer / :rebuilt."))
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
