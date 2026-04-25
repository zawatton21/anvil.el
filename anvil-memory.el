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

;; Phase 2b-ii: the LLM verdict path calls
;; `anvil-orchestrator-submit-and-collect' only when the caller
;; opts in with `:with-llm t'.  The module itself does not require
;; anvil-orchestrator so the no-LLM code path works on an installation
;; without the orchestrator loaded.
(declare-function anvil-orchestrator-submit-and-collect "anvil-orchestrator")

;; Phase 3b: the live TCP server JSON-encodes memory rows via the
;; built-in `json' library.  Loaded eagerly because every
;; `/api/...' route needs it and the dep is ~free (core Emacs).
(require 'json)


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
When nil, `anvil-memory-scan' auto-detects the memory directories
declared in `anvil-memory-provider-paths' (Claude Code, Codex CLI,
Gemini CLI, Continue, Aider, Cline, Cursor, …) so auto-memory from
any provider the user has installed lands in a single DB.  An
explicit list overrides auto-detection."
  :type '(choice (const :tag "Auto-detect" nil)
                 (repeat directory))
  :group 'anvil-memory)

(defcustom anvil-memory-provider-paths
  '((claude   . "~/.claude/projects/*/memory")
    (codex    . "~/.codex/memories")
    (gemini   . "~/.gemini/memories")
    (continue . "~/.continue/memories")
    (aider    . "~/.aider/memories")
    (cline    . "~/.cline/memories")
    (cursor   . "~/.cursor/memories")
    (windsurf . "~/.windsurf/memories")
    (zed      . "~/.config/zed/memories"))
  "Per-provider memory directory patterns used when
`anvil-memory-roots' is nil.  Each entry is a cons (PROVIDER .
PATTERN).  PATTERN may be an absolute path or contain `*' globs
(e.g. ~/.claude/projects/*/memory); `~' is expanded.  Patterns
that do not match any existing directory are skipped silently, so
adding a provider the user has not installed costs nothing.
Providers added later can be registered by pushing a new pair
onto this alist; built-ins stay in the list for defaults."
  :type '(alist :key-type symbol :value-type string)
  :group 'anvil-memory)

(defconst anvil-memory-supported
  '(scan audit access list
         search save-check duplicates audit-urls
         decay promote regenerate reindex-fts llm-verdict
         mdl-distill export-html serve contradictions)
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

(defcustom anvil-memory-llm-provider "claude"
  "Default provider id passed to `anvil-orchestrator-submit-and-collect'
when `anvil-memory-save-check' is called with `:with-llm t' and no
explicit provider.  Override per-call with `:provider'."
  :type 'string
  :group 'anvil-memory)

(defcustom anvil-memory-llm-model nil
  "Default model slug for the LLM verdict classifier (nil means let
the orchestrator pick the provider's default).  Override per-call
with `:model'."
  :type '(choice (const :tag "Provider default" nil) string)
  :group 'anvil-memory)

(defcustom anvil-memory-llm-timeout-sec 30
  "Per-candidate wall-clock cap passed as :collect-timeout-sec when
`anvil-memory-save-check' dispatches a verdict classification.
Each call also forwards this as :timeout-sec to the provider.
Kept short because the expected output is a single keyword."
  :type 'integer
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
   (anvil-memory--resolve-tokenizer anvil-memory--db))
  ;; Phase 3c: persistent contradiction edges.  Pair ordering is
  ;; canonicalised on write (string< first) so (A, B) and (B, A) do
  ;; not both get rows — the (file_a, file_b) composite PK enforces
  ;; that invariant across rescans.
  (sqlite-execute anvil-memory--db
                  "CREATE TABLE IF NOT EXISTS memory_contradictions (
                     file_a TEXT NOT NULL,
                     file_b TEXT NOT NULL,
                     verdict TEXT NOT NULL,
                     score REAL,
                     checked_at INTEGER,
                     PRIMARY KEY (file_a, file_b)
                   )"))


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

(defun anvil-memory--expand-provider-pattern (pattern)
  "Return the list of existing directories matched by PATTERN.
PATTERN is a string that may contain `*' globs (e.g.
~/.claude/projects/*/memory); `~' is expanded via
`expand-file-name'.  Non-existent expansions are dropped so
unused providers cost nothing at scan time."
  (let ((expanded (expand-file-name pattern)))
    (if (string-match-p "\\*" expanded)
        (cl-remove-if-not #'file-directory-p
                          (file-expand-wildcards expanded))
      (and (file-directory-p expanded) (list expanded)))))

(defun anvil-memory--effective-roots ()
  "Return the list of memory directories to scan.
`anvil-memory-roots' wins when non-nil; otherwise every directory
matched by `anvil-memory-provider-paths' is returned.  Results are
deduplicated while preserving provider order so two providers
sharing a symlinked directory are indexed once."
  (cond
   (anvil-memory-roots
    (cl-remove-if-not #'file-directory-p anvil-memory-roots))
   (t
    (cl-delete-duplicates
     (cl-loop for (_provider . pattern) in anvil-memory-provider-paths
              append (anvil-memory--expand-provider-pattern pattern))
     :test #'equal
     :from-end t))))

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
(cl-defun anvil-memory-save-check
    (subject body &optional top-n
             &key with-llm provider model timeout-sec)
  "Return top-N memories similar to the draft SUBJECT/BODY.
TOP-N defaults to 3.  Uses FTS5 keyword search against
SUBJECT + BODY to shortlist candidates, then ranks them by
jaccard overlap against BODY.  Each result plist carries at
least (:file :type :snippet :similarity).

Phase 2b-ii: when WITH-LLM is non-nil every returned candidate
is additionally classified via `anvil-orchestrator-submit-and-
collect' and the result grows one of:
  :verdict       — `duplicate' / `contradicting' / `orthogonal'
  :verdict-raw   — model text if the verdict word was missing
  :verdict-error — string when the orchestrator failed / timed out

PROVIDER / MODEL / TIMEOUT-SEC default to
`anvil-memory-llm-provider' / `anvil-memory-llm-model' /
`anvil-memory-llm-timeout-sec'.  Zero candidates skips the
orchestrator entirely so the LLM is never paid for a miss."
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
    (let ((ranked (seq-take
                   (cl-sort scored #'>
                            :key (lambda (x) (plist-get x :similarity)))
                   top-n)))
      (if with-llm
          (mapcar (lambda (cand)
                    (anvil-memory--classify-candidate
                     cand subject body
                     (or provider anvil-memory-llm-provider)
                     (or model anvil-memory-llm-model)
                     (or timeout-sec anvil-memory-llm-timeout-sec)))
                  ranked)
        ranked))))

(defun anvil-memory--verdict-prompt (subject body cand-body)
  "Build the one-word verdict prompt for CAND-BODY vs SUBJECT/BODY."
  (format "Classify the relationship between the NEW memory draft and \
the EXISTING memory below.  Reply with exactly one of these words, \
lowercase, no punctuation:
  duplicate      — same rule / fact, redundant
  contradicting  — asserts something incompatible with EXISTING
  orthogonal     — different topic / compatible

NEW memory draft:
Subject: %s
Body:
%s

EXISTING memory:
%s

Verdict (one word):"
          subject body cand-body))

(defun anvil-memory--parse-verdict (text)
  "Return `duplicate' / `contradicting' / `orthogonal' found in TEXT, else nil.
Match is case-insensitive and tolerates surrounding punctuation /
labels (`Verdict: DUPLICATE.')."
  (let ((s (downcase (or text ""))))
    (cond
     ((string-match-p "\\bcontradict\\(ing\\|s\\|ed\\|ion\\)?\\b" s)
      'contradicting)
     ((string-match-p "\\bduplicate\\b" s) 'duplicate)
     ((string-match-p "\\borthogonal\\b" s) 'orthogonal))))

(defun anvil-memory--classify-candidate (cand subject body provider model timeout)
  "Run the LLM verdict classifier on CAND (a save-check plist).
PROVIDER / MODEL / TIMEOUT are passed through to
`anvil-orchestrator-submit-and-collect'.  Returns CAND enriched
with :verdict / :verdict-raw / :verdict-error per the result."
  (let* ((cand-body (or (anvil-memory--get-body (plist-get cand :file)) ""))
         (prompt (anvil-memory--verdict-prompt subject body cand-body))
         (result (condition-case err
                     (anvil-orchestrator-submit-and-collect
                      :provider provider
                      :prompt prompt
                      :model model
                      :timeout-sec timeout
                      :collect-timeout-sec timeout)
                   (error (list :status 'failed
                                :error (error-message-string err)
                                :pending nil))))
         (status (plist-get result :status))
         (pending (plist-get result :pending))
         (summary (plist-get result :summary))
         (err-msg (plist-get result :error)))
    (cond
     (pending
      (append cand (list :verdict nil
                         :verdict-error "orchestrator pending (timeout)")))
     ((eq status 'done)
      (let ((v (anvil-memory--parse-verdict summary)))
        (if v
            (append cand (list :verdict v))
          (append cand (list :verdict nil
                             :verdict-raw (or summary ""))))))
     (t
      (append cand (list :verdict nil
                         :verdict-error
                         (format "orchestrator %s: %s"
                                 (or status 'unknown)
                                 (or err-msg "no error message"))))))))

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


;;;; --- Phase 2b-iii: MDL distillation ------------------------------------

(defun anvil-memory--distill-prompt (files bodies)
  "Build the MDL distillation prompt from FILES and BODIES (aligned lists).
Each source is emitted as a labelled `--- FILE: path ---' block so
the parser / the LLM can attribute the draft back to the inputs."
  (let ((blocks
         (cl-loop for f in files
                  for b in bodies
                  concat (format "--- FILE: %s ---\n%s\n" f (or b "")))))
    (format "You are asked to propose ONE \"convention memory\" file that \
unifies the %d existing auto-memory entries below.

Output requirements:
- Start with a YAML frontmatter block containing at least:
    name         (short title)
    description  (one line, <= 140 chars)
    type         (user / feedback / project / reference)
- Follow the frontmatter with a concise body (<=50 lines) that captures
  the common principle.
- Do NOT emit anything except the proposed memory file content.
- The draft must be general enough to replace all sources without losing
  load-bearing detail.  Only include what the sources collectively justify.
- Preserve the dominant language of the sources (Japanese OK).

Sources (%d):
%s
Proposed convention memory:"
            (length files) (length files) blocks)))

(defun anvil-memory--file-indexed-p (db path)
  "Non-nil when PATH appears in memory_meta on DB."
  (and path
       (stringp path)
       (let ((rows (sqlite-select
                    db "SELECT 1 FROM memory_meta WHERE file = ?1"
                    (list path))))
         (and rows t))))

;;;###autoload
(cl-defun anvil-memory-mdl-distill
    (files &key provider model timeout-sec)
  "Propose a unified convention memory that covers FILES.
FILES is a non-empty list of memory .md paths, each of which must
already be indexed (`memory-scan' first).  Each file's body is
read fresh off disk and sent — alongside a path label — to
`anvil-orchestrator-submit-and-collect'.  Defaults come from
`anvil-memory-llm-provider' / `-llm-model' / `-llm-timeout-sec'.

Returns a plist:
  :draft    — proposed memory content (frontmatter + body) on success
  :sources  — FILES verbatim (caller preserves order)
  :error    — string when the orchestrator failed / timed out

The function is read-only: it never writes to disk, rescans the
index, or mutates the sources.  The caller (memory-pruner skill /
human) decides whether to accept the draft."
  (unless (and files (listp files))
    (user-error "anvil-memory-mdl-distill: FILES must be a non-empty list"))
  (let* ((db (anvil-memory--db)))
    (dolist (f files)
      (unless (anvil-memory--file-indexed-p db f)
        (user-error
         "anvil-memory-mdl-distill: %S is not in the memory index" f))))
  (let* ((bodies (mapcar (lambda (f)
                           (or (anvil-memory--get-body f)
                               (and (file-readable-p f)
                                    (anvil-memory--read-body-utf8 f))
                               ""))
                         files))
         (prompt (anvil-memory--distill-prompt files bodies))
         (prov (or provider anvil-memory-llm-provider))
         (mdl  (or model anvil-memory-llm-model))
         (tout (or timeout-sec anvil-memory-llm-timeout-sec))
         (result (condition-case err
                     (anvil-orchestrator-submit-and-collect
                      :provider prov
                      :prompt prompt
                      :model mdl
                      :timeout-sec tout
                      :collect-timeout-sec tout)
                   (error (list :status 'failed
                                :error (error-message-string err)
                                :pending nil))))
         (status (plist-get result :status))
         (pending (plist-get result :pending))
         (summary (plist-get result :summary))
         (err-msg (plist-get result :error)))
    (cond
     (pending
      (list :draft nil
            :sources files
            :error "orchestrator pending (timeout)"))
     ((eq status 'done)
      (list :draft (or summary "")
            :sources files
            :error nil))
     (t
      (list :draft nil
            :sources files
            :error (format "orchestrator %s: %s"
                           (or status 'unknown)
                           (or err-msg "no error message")))))))


;;;; --- Phase 3a: HTML export (static viewer) -----------------------------

(defconst anvil-memory--html-escape-map
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;")
    ("\"" . "&quot;")
    ("'" . "&#39;"))
  "Literal → entity substitutions for `anvil-memory--html-escape'.
Applied in order; `&' must come first so later ampersands coming
from prior replacements (there aren't any here, but the ordering
rule is still the defensible default).")

(defun anvil-memory--html-escape (s)
  "Return S with the 5 core HTML entities escaped.
Returns \"\" on nil input so callers can feed it `plist-get'
results without a guard."
  (let ((out (or s "")))
    (dolist (pair anvil-memory--html-escape-map)
      (setq out (replace-regexp-in-string
                 (regexp-quote (car pair)) (cdr pair) out t t)))
    out))

(defconst anvil-memory--html-css
  "body{font-family:system-ui,-apple-system,sans-serif;\
max-width:1100px;margin:2rem auto;padding:0 1rem;color:#222}
h1{margin-bottom:.2rem}
h2{margin-top:2rem;text-transform:capitalize;\
border-bottom:1px solid #ddd;padding-bottom:.2rem}
.summary{color:#666;font-size:.9rem;margin-bottom:1rem}
table{width:100%;border-collapse:collapse;margin-top:.5rem}
th,td{text-align:left;padding:.3rem .5rem;border-bottom:1px solid #eee;\
vertical-align:top}
th{font-weight:600;background:#fafafa;font-size:.9rem}
.bar{display:inline-block;height:.6em;border-radius:3px;\
background:#ccc;vertical-align:middle}
.bar.hot{background:#3c5}
.bar.warm{background:#dd3}
.bar.cold{background:#d73}
.name{font-weight:600}
.desc{color:#444}
.muted{color:#888;font-size:.85rem}
.file{font-family:ui-monospace,monospace;font-size:.8rem;color:#555}
tr.empty td{color:#888;font-style:italic;text-align:center}
"
  "Self-contained CSS embedded in `anvil-memory-export-html' output.
Kept minimal + offline-openable — no external fonts / CDNs.")

(defconst anvil-memory--html-type-order
  '(user feedback project reference memo)
  "Stable render order for memory type sections.
User / feedback first (long-lived rules), then project (scoped),
reference (TTL'd links), memo (misc catch-all).")

(defun anvil-memory--group-by-type (rows)
  "Return alist ((TYPE . ROWS) ...) grouped from decay-sorted ROWS.
Key order matches `anvil-memory--html-type-order'; input row
order within each type is preserved (so the caller's decay-desc
sort survives the split)."
  (let (buckets)
    (dolist (row rows)
      (let* ((type (plist-get row :type))
             (cell (assq type buckets)))
        (if cell
            (setcdr cell (append (cdr cell) (list row)))
          (push (cons type (list row)) buckets))))
    (let (ordered)
      (dolist (type anvil-memory--html-type-order)
        (let ((cell (assq type buckets)))
          (when cell
            (push cell ordered)
            (setq buckets (delq cell buckets)))))
      (append (nreverse ordered) buckets))))

(defun anvil-memory--decay-bar-class (decay)
  "Map DECAY (0.0-1.0) onto a bar CSS class."
  (cond
   ((>= decay 0.66) "bar hot")
   ((>= decay 0.33) "bar warm")
   (t               "bar cold")))

(defun anvil-memory--render-row (row)
  "Return one <tr> string for ROW (a memory-list plist)."
  (let* ((path (plist-get row :file))
         (base (file-name-nondirectory path))
         (body (or (anvil-memory--get-body path) ""))
         (fm (anvil-memory--parse-frontmatter body))
         (name (or (and fm (plist-get fm :name))
                   (anvil-memory--fallback-display-name path)))
         (desc (or (and fm (plist-get fm :description)) ""))
         (decay (or (plist-get row :decay-score) 0.0))
         (count (or (plist-get row :access-count) 0))
         (last (plist-get row :last-accessed))
         (last-str (if last
                       (format-time-string "%Y-%m-%d" last)
                     "—")))
    (format "    <tr data-decay=\"%.3f\">\
<td><div class=\"name\">%s</div>\
<div class=\"desc\">%s</div>\
<div class=\"file\">%s</div></td>\
<td><span class=\"%s\" style=\"width:%.0fpx\"></span> \
<span class=\"muted\">%.2f</span></td>\
<td class=\"muted\">%d</td><td class=\"muted\">%s</td></tr>\n"
            decay
            (anvil-memory--html-escape name)
            (anvil-memory--html-escape desc)
            (anvil-memory--html-escape base)
            (anvil-memory--decay-bar-class decay)
            (* 80 decay)
            decay
            count
            last-str)))

(defun anvil-memory--render-section (type rows)
  "Return a <section> string for TYPE with already-sorted ROWS."
  (let ((heading (anvil-memory--html-escape (symbol-name type))))
    (concat
     (format "<section data-type=\"%s\">\n" heading)
     (format "  <h2>%s <span class=\"muted\">(%d)</span></h2>\n"
             heading (length rows))
     "  <table>\n"
     "    <thead><tr><th>memory</th><th>decay</th>\
<th>access</th><th>last</th></tr></thead>\n"
     "    <tbody>\n"
     (mapconcat #'anvil-memory--render-row rows "")
     "    </tbody>\n"
     "  </table>\n"
     "</section>\n")))

(defun anvil-memory--render-type-filter (groups)
  "Return a per-type checkbox fieldset + inline JS that toggles sections.
Only emits controls for TYPEs present in GROUPS."
  (if (null groups) ""
    (concat
     "<fieldset class=\"facets\">\n"
     "  <legend>Types</legend>\n"
     (mapconcat
      (lambda (group)
        (let ((name (anvil-memory--html-escape (symbol-name (car group)))))
          (format "  <label><input type=\"checkbox\" class=\"facet-filter\" \
data-facet-type=\"%s\" checked> %s</label>\n" name name)))
      groups "")
     "</fieldset>\n"
     ;; Inline script — Phase 3a fixture only forbids `<script src>'.
     "<script>\n(function(){\n\
var boxes=document.querySelectorAll('.facet-filter');\n\
boxes.forEach(function(cb){cb.addEventListener('change',function(e){\n\
  var t=e.target.getAttribute('data-facet-type');\n\
  var sec=document.querySelector('section[data-type=\"'+t+'\"]');\n\
  if(sec){sec.style.display=e.target.checked?'':'none';}\n\
});});\n})();\n</script>\n")))

(defun anvil-memory--render-contradictions (rows)
  "Return the contradictions <section> for stored ROWS, empty when none."
  (if (null rows) ""
    (concat
     "<section data-section=\"contradictions\">\n"
     (format "  <h2>Contradictions <span class=\"muted\">(%d)</span></h2>\n"
             (length rows))
     "  <ul class=\"contradictions\">\n"
     (mapconcat
      (lambda (r)
        (let* ((a (anvil-memory--html-escape
                   (file-name-nondirectory (plist-get r :file-a))))
               (b (anvil-memory--html-escape
                   (file-name-nondirectory (plist-get r :file-b))))
               (v (anvil-memory--html-escape (plist-get r :verdict)))
               (score (or (plist-get r :score) 0.0)))
          (format "    <li data-verdict=\"%s\">\
<code>%s</code> <span class=\"arrow\">↔</span> <code>%s</code> \
<span class=\"muted\">%s · %.2f</span></li>\n"
                  v a b v score)))
      rows "")
     "  </ul>\n"
     "</section>\n")))

(defun anvil-memory--render-page (abs-root groups title)
  "Return the full HTML document for ABS-ROOT + GROUPS (:by-type alist)."
  (let* ((total (apply #'+ (mapcar (lambda (g) (length (cdr g))) groups)))
         (contradictions (ignore-errors (anvil-memory-contradictions)))
         ;; Only include contradiction rows whose both endpoints live under
         ;; ABS-ROOT — the page is scoped to a single root and showing edges
         ;; to out-of-scope files would confuse the reader.
         (scoped (cl-remove-if-not
                  (lambda (r)
                    (and (string-prefix-p abs-root (plist-get r :file-a))
                         (string-prefix-p abs-root (plist-get r :file-b))))
                  contradictions))
         (hdr (or title (format "anvil memory — %s"
                                (abbreviate-file-name abs-root)))))
    (concat
     "<!DOCTYPE html>\n<html lang=\"en\"><head>\n"
     "  <meta charset=\"utf-8\">\n"
     "  <meta name=\"generator\" content=\"anvil-memory / Doc 29 Phase 3\">\n"
     "  <title>" (anvil-memory--html-escape hdr) "</title>\n"
     "  <style>\n" anvil-memory--html-css "  </style>\n"
     "</head><body>\n"
     "<h1>anvil memory</h1>\n"
     (format "<div class=\"summary\">%s · %d memor%s</div>\n"
             (anvil-memory--html-escape (abbreviate-file-name abs-root))
             total
             (if (= total 1) "y" "ies"))
     (anvil-memory--render-type-filter groups)
     (anvil-memory--render-contradictions scoped)
     (if (zerop total)
         "<p class=\"muted\">No indexed memories under this root.</p>\n"
       (mapconcat (lambda (group)
                    (anvil-memory--render-section (car group) (cdr group)))
                  groups ""))
     "</body></html>\n")))

;;;###autoload
(cl-defun anvil-memory-export-html (root &key out-path title)
  "Return a self-contained HTML snapshot of memories under ROOT.
Only rows whose `:file' lives under ROOT are included.  Within
each type section, rows are decay-score descending so the most
load-bearing entries surface at the top of the page.

:out-path NON-NIL writes the HTML to that file (UTF-8) in addition
to returning the string.  :title overrides the default <title>.

Render surface: name / description (YAML frontmatter), filename,
decay-score (numeric + colour bar), access-count, last-accessed
date.  Memory bodies are NOT embedded so a stray .html file never
leaks session text."
  (unless (and root (stringp root))
    (user-error "anvil-memory-export-html: ROOT must be a string"))
  (let* ((abs (file-name-as-directory (expand-file-name root)))
         (rows (anvil-memory-list nil :with-decay t :sort 'decay))
         (scoped (cl-remove-if-not
                  (lambda (r) (string-prefix-p abs (plist-get r :file)))
                  rows))
         (groups (anvil-memory--group-by-type scoped))
         (html (anvil-memory--render-page abs groups title)))
    (when out-path
      (make-directory (file-name-directory out-path) t)
      (let ((coding-system-for-write 'utf-8))
        (with-temp-file out-path
          (insert html))))
    html))


;;;; --- Phase 3b: live TCP server ------------------------------------------

(defcustom anvil-memory-serve-host "127.0.0.1"
  "Host the memory viewer server binds to.
Loopback-only is the strongly recommended default — there is no
authentication, so exposing the socket to the network (e.g. by
setting this to `0.0.0.0') lets anyone on the subnet read the
metadata index.  Change only if you know what you are doing."
  :type 'string
  :group 'anvil-memory)

(defcustom anvil-memory-serve-port 8729
  "Default port for `anvil-memory-serve-start'.
Pass `:port t' to let the kernel pick a free ephemeral port
(useful for tests and for avoiding collisions with other services)."
  :type '(choice integer (const :tag "Ephemeral (kernel-picked)" t))
  :group 'anvil-memory)

(defvar anvil-memory--serve-proc nil
  "Running server process, or nil when the viewer is not live.")
(defvar anvil-memory--serve-port nil
  "Port the running server is bound to, or nil when idle.")
(defvar anvil-memory--serve-host nil
  "Host the running server is bound to, or nil when idle.")
(defvar anvil-memory--serve-root nil
  "Directory the running server scopes its listings to, or nil for all.")

(defun anvil-memory--serve-running-p ()
  "Non-nil when the memory viewer server is live."
  (and anvil-memory--serve-proc
       (process-live-p anvil-memory--serve-proc)))

(defun anvil-memory--http-status-line (code)
  "Return \"HTTP/1.0 CODE REASON\\r\\n\" for CODE."
  (let ((reason (pcase code
                  (200 "OK")
                  (400 "Bad Request")
                  (404 "Not Found")
                  (405 "Method Not Allowed")
                  (500 "Internal Server Error")
                  (_   "Status"))))
    (format "HTTP/1.0 %d %s\r\n" code reason)))

(defun anvil-memory--http-response (status content-type body)
  "Build an HTTP/1.0 response string from STATUS / CONTENT-TYPE / BODY."
  (let ((bytes (string-bytes body)))
    (concat (anvil-memory--http-status-line status)
            (format "Content-Type: %s\r\n" content-type)
            (format "Content-Length: %d\r\n" bytes)
            "Connection: close\r\n"
            "Cache-Control: no-store\r\n"
            "\r\n"
            body)))

(defun anvil-memory--parse-request-line (line)
  "Parse the first line of an HTTP request into (METHOD PATH), else nil."
  (when (and line (string-match
                   "\\`\\([A-Z]+\\) \\([^ ]+\\) HTTP/[0-9.]+\\'" line))
    (cons (match-string 1 line) (match-string 2 line))))

(defun anvil-memory--serve-list-rows (&optional scope-root)
  "Return the indexed-memory plists under SCOPE-ROOT (or all when nil)."
  (let ((rows (anvil-memory-list nil :with-decay t :sort 'decay)))
    (if (and scope-root (stringp scope-root))
        (let ((abs (file-name-as-directory (expand-file-name scope-root))))
          (cl-remove-if-not
           (lambda (r) (string-prefix-p abs (plist-get r :file)))
           rows))
      rows)))

(defun anvil-memory--rows-to-alist (rows)
  "Coerce memory plists ROWS to alists the `json' library can encode."
  (mapcar
   (lambda (r)
     `((file . ,(plist-get r :file))
       (type . ,(symbol-name (plist-get r :type)))
       (access_count . ,(or (plist-get r :access-count) 0))
       (last_accessed . ,(or (plist-get r :last-accessed) :null))
       (decay_score . ,(or (plist-get r :decay-score) 0.0))))
   rows))

(defun anvil-memory--heatmap-aggregate (rows)
  "Aggregate ROWS into per-type (count, mean decay) alist entries."
  (let (buckets)
    (dolist (r rows)
      (let* ((type (plist-get r :type))
             (decay (or (plist-get r :decay-score) 0.0))
             (cell (assq type buckets)))
        (if cell
            (setcdr cell (list (1+ (car (cdr cell)))
                               (+ (cadr (cdr cell)) decay)))
          (push (cons type (list 1 decay)) buckets))))
    (mapcar
     (lambda (cell)
       (let* ((type (car cell))
              (count (car (cdr cell)))
              (sum (cadr (cdr cell))))
         `((type . ,(symbol-name type))
           (count . ,count)
           (mean_decay . ,(if (zerop count) 0.0 (/ sum (float count)))))))
     (sort buckets
           (lambda (a b)
             (let ((ao (cl-position (car a) anvil-memory--html-type-order))
                   (bo (cl-position (car b) anvil-memory--html-type-order)))
               (< (or ao 99) (or bo 99))))))))

(defun anvil-memory--serve-dispatch (method path)
  "Dispatch HTTP METHOD + PATH to a response string."
  (cond
   ((not (equal method "GET"))
    (anvil-memory--http-response
     405 "text/plain; charset=utf-8"
     "Only GET is supported by the anvil-memory viewer.\n"))
   ((equal path "/")
    (let* ((root (or anvil-memory--serve-root
                     (car (anvil-memory--effective-roots))
                     ""))
           (html (if (and root (stringp root) (not (string-empty-p root)))
                     (anvil-memory-export-html root)
                   "<!DOCTYPE html><html><body><p>No indexed \
memory roots — run <code>memory-scan</code> first.</p></body></html>")))
      (anvil-memory--http-response 200 "text/html; charset=utf-8" html)))
   ((equal path "/api/list")
    (let* ((rows (anvil-memory--serve-list-rows anvil-memory--serve-root))
           (json-encoding-pretty-print nil)
           (body (json-encode (anvil-memory--rows-to-alist rows))))
      (anvil-memory--http-response 200 "application/json; charset=utf-8" body)))
   ((equal path "/api/decay-heatmap")
    (let* ((rows (anvil-memory--serve-list-rows anvil-memory--serve-root))
           (body (json-encode (anvil-memory--heatmap-aggregate rows))))
      (anvil-memory--http-response 200 "application/json; charset=utf-8" body)))
   ((equal path "/api/contradictions")
    (let* ((rows (anvil-memory-contradictions))
           (body (json-encode
                  (anvil-memory--contradictions-to-alist rows))))
      (anvil-memory--http-response 200 "application/json; charset=utf-8" body)))
   (t
    (anvil-memory--http-response
     404 "text/plain; charset=utf-8"
     (format "Not Found: %s\n" path)))))

(defun anvil-memory--serve-handle-buffer (buf)
  "Parse BUF (the accumulated request bytes) and return an HTTP response.
Returns nil when the request is incomplete (no `\\r\\n\\r\\n' yet)
so the filter can wait for more data."
  (let ((split (string-match "\r\n\r\n" buf)))
    (when split
      (let* ((head (substring buf 0 split))
             (lines (split-string head "\r\n" t))
             (parsed (anvil-memory--parse-request-line (car lines))))
        (if parsed
            (anvil-memory--serve-dispatch (car parsed) (cdr parsed))
          (anvil-memory--http-response
           400 "text/plain; charset=utf-8"
           "Bad Request\n"))))))

(defun anvil-memory--serve-filter (proc data)
  "Per-connection filter: buffer bytes, respond, close when request complete."
  (let ((buf (concat (or (process-get proc :request-buffer) "") data)))
    (process-put proc :request-buffer buf)
    (let ((response
           (condition-case err
               (anvil-memory--serve-handle-buffer buf)
             (error
              (anvil-memory--http-response
               500 "text/plain; charset=utf-8"
               (format "Server error: %s\n" (error-message-string err)))))))
      (when response
        (ignore-errors (process-send-string proc response))
        (ignore-errors (delete-process proc))))))

(defun anvil-memory--serve-sentinel (_proc _event)
  "Sentinel for accepted client processes.  No-op: filter owns teardown."
  nil)

;;;###autoload
(cl-defun anvil-memory-serve-start (&key host port root)
  "Start the local HTTP memory viewer and return its (:port :host :process).
HOST defaults to `anvil-memory-serve-host' (loopback).  PORT is
either an integer or `t' (let the kernel pick a free port);
defaults to `anvil-memory-serve-port'.  ROOT scopes the served
listings — nil includes every indexed memory root.

Signals `user-error' when a server is already running; call
`anvil-memory-serve-stop' first if you want to rebind."
  (when (anvil-memory--serve-running-p)
    (user-error
     "anvil-memory-serve: already running on %s:%s"
     anvil-memory--serve-host anvil-memory--serve-port))
  (let* ((h (or host anvil-memory-serve-host))
         (p (or port anvil-memory-serve-port))
         (proc (make-network-process
                :name "anvil-memory-serve"
                :server t
                :host h
                :service p
                :family 'ipv4
                :coding 'binary
                :noquery t
                :filter #'anvil-memory--serve-filter
                :sentinel #'anvil-memory--serve-sentinel))
         (assigned (process-contact proc :service)))
    (setq anvil-memory--serve-proc proc
          anvil-memory--serve-port (if (integerp assigned) assigned p)
          anvil-memory--serve-host h
          anvil-memory--serve-root (and root (stringp root) root))
    (list :port anvil-memory--serve-port
          :host h
          :process proc
          :root anvil-memory--serve-root)))

;;;###autoload
(defun anvil-memory-serve-stop ()
  "Stop the running memory viewer server, if any.
Returns the port number it was bound to, or nil when nothing was
running (so the caller can use the return value to distinguish
`was-running' from `was-idle').  Always safe to call."
  (interactive)
  (let ((was anvil-memory--serve-port))
    (when (and anvil-memory--serve-proc
               (process-live-p anvil-memory--serve-proc))
      (ignore-errors (delete-process anvil-memory--serve-proc)))
    (setq anvil-memory--serve-proc nil
          anvil-memory--serve-port nil
          anvil-memory--serve-host nil
          anvil-memory--serve-root nil)
    was))


;;;; --- Phase 3c: contradiction store + graph -----------------------------

(defun anvil-memory--canonical-pair (a b)
  "Return (cons A B) / (cons B A) with string< ordering so (a,b)=(b,a)."
  (if (string-lessp a b) (cons a b) (cons b a)))

(defun anvil-memory--upsert-contradiction (db a b verdict score ts)
  "Store / refresh a single contradiction row (canonically-ordered pair)."
  (sqlite-execute
   db
   "INSERT INTO memory_contradictions(file_a, file_b, verdict, score, checked_at)
    VALUES (?1, ?2, ?3, ?4, ?5)
    ON CONFLICT(file_a, file_b) DO UPDATE SET
      verdict=excluded.verdict,
      score=excluded.score,
      checked_at=excluded.checked_at"
   (list a b verdict score ts)))

;;;###autoload
(cl-defun anvil-memory-scan-contradictions
    (&key (threshold 0.5) (mode 'keyword)
          provider model timeout-sec)
  "Populate `memory_contradictions' from the current memory index.
THRESHOLD is the jaccard-similarity floor passed to
`anvil-memory-duplicates' to shortlist candidate pairs.

MODE controls how candidates become stored rows:
  `keyword' — every candidate is persisted verbatim with
              verdict=\"candidate\" and score=jaccard.
  `llm'     — each candidate is handed to `anvil-orchestrator-
              submit-and-collect' for a verdict; only pairs flagged
              `contradicting' are stored (verdict=\"contradicting\",
              score=jaccard).  PROVIDER / MODEL / TIMEOUT-SEC
              default to `anvil-memory-llm-*'.

Rows are upserted by (file_a, file_b) so re-running is idempotent.
Returns (:scanned N :stored M :mode STR :threshold FLOAT)."
  (let* ((db (anvil-memory--db))
         (candidates (anvil-memory-duplicates threshold))
         (now (truncate (float-time)))
         (stored 0))
    (pcase mode
      ('keyword
       (dolist (c candidates)
         (let* ((pair (plist-get c :pair))
                (sim (plist-get c :similarity))
                (canon (anvil-memory--canonical-pair
                        (nth 0 pair) (nth 1 pair))))
           (anvil-memory--upsert-contradiction
            db (car canon) (cdr canon) "candidate" sim now)
           (cl-incf stored))))
      ('llm
       (let ((prov (or provider anvil-memory-llm-provider))
             (mdl  (or model anvil-memory-llm-model))
             (tout (or timeout-sec anvil-memory-llm-timeout-sec)))
         (dolist (c candidates)
           (let* ((pair (plist-get c :pair))
                  (sim (plist-get c :similarity))
                  (canon (anvil-memory--canonical-pair
                          (nth 0 pair) (nth 1 pair)))
                  (a (car canon))
                  (b (cdr canon))
                  (body-a (or (anvil-memory--get-body a) ""))
                  (body-b (or (anvil-memory--get-body b) ""))
                  (prompt (anvil-memory--verdict-prompt
                           (file-name-nondirectory a) body-a body-b))
                  (result (condition-case err
                              (anvil-orchestrator-submit-and-collect
                               :provider prov
                               :prompt prompt
                               :model mdl
                               :timeout-sec tout
                               :collect-timeout-sec tout)
                            (error (list :status 'failed
                                         :error (error-message-string err)
                                         :pending nil))))
                  (status (plist-get result :status))
                  (summary (plist-get result :summary))
                  (verdict (and (eq status 'done)
                                (anvil-memory--parse-verdict summary))))
             (when (eq verdict 'contradicting)
               (anvil-memory--upsert-contradiction
                db a b "contradicting" sim now)
               (cl-incf stored))))))
      (other
       (user-error
        "anvil-memory-scan-contradictions: unknown mode %S" other)))
    (list :scanned (length candidates)
          :stored stored
          :mode (symbol-name mode)
          :threshold threshold)))

;;;###autoload
(defun anvil-memory-contradictions ()
  "Return every stored contradiction / candidate edge as plists.
Sorted by verdict (contradicting / candidate / others) then by
score descending.  Result plist keys:
  :file-a / :file-b / :verdict / :score / :checked-at"
  (let* ((db (anvil-memory--db))
         (rows (sqlite-select
                db "SELECT file_a, file_b, verdict, score, checked_at
                    FROM memory_contradictions
                    ORDER BY verdict ASC, score DESC")))
    (mapcar (lambda (row)
              (list :file-a (nth 0 row)
                    :file-b (nth 1 row)
                    :verdict (nth 2 row)
                    :score (or (nth 3 row) 0.0)
                    :checked-at (nth 4 row)))
            rows)))

(defun anvil-memory--contradictions-to-alist (rows)
  "Coerce `anvil-memory-contradictions' ROWS to JSON-serialisable alists."
  (mapcar (lambda (r)
            `((file_a . ,(plist-get r :file-a))
              (file_b . ,(plist-get r :file-b))
              (verdict . ,(plist-get r :verdict))
              (score . ,(or (plist-get r :score) 0.0))
              (checked_at . ,(or (plist-get r :checked-at) :null))))
          rows))

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

(defun anvil-memory--coerce-bool (v)
  "Coerce V (nil / bool / string) to `t' / nil.
Empty string / \"false\" / \"nil\" / \"0\" → nil; anything else
non-nil → t."
  (cond
   ((null v) nil)
   ((eq v t) t)
   ((stringp v)
    (not (member (downcase v) '("" "false" "nil" "0" "no" "off"))))
   (t t)))

(defun anvil-memory--tool-save-check (subject body
                                              &optional with_llm provider model)
  "Return top-N indexed memories similar to a draft SUBJECT/BODY.

MCP Parameters:
  subject  - Draft memory subject (or empty).
  body     - Draft memory body.  FTS5 shortlist + jaccard ranking.
  with_llm - Optional truthy flag (`true' / `1' / `t').  When set,
             each candidate carries an LLM verdict
             (`duplicate' / `contradicting' / `orthogonal') via
             `anvil-orchestrator-submit-and-collect'.  Opt-in —
             unset stays keyword-only like Phase 1b.
  provider - Optional orchestrator provider id (defaults to
             `anvil-memory-llm-provider').
  model    - Optional model slug (defaults to
             `anvil-memory-llm-model').

Returns (:candidates ROWS).  Each row carries :file / :type /
:snippet / :similarity plus, when WITH_LLM was set, one of
:verdict / :verdict-raw / :verdict-error."
  (anvil-server-with-error-handling
   (let* ((llm-p (anvil-memory--coerce-bool with_llm))
          (prov (cond ((null provider) nil)
                      ((and (stringp provider) (string-empty-p provider)) nil)
                      (t provider)))
          (mdl (cond ((null model) nil)
                     ((and (stringp model) (string-empty-p model)) nil)
                     (t model))))
     (list :candidates
           (anvil-memory-save-check
            (or subject "") (or body "") nil
            :with-llm llm-p
            :provider prov
            :model mdl)))))

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

(defun anvil-memory--tool-mdl-distill (files &optional provider model)
  "Propose a unified convention memory that covers FILES.

MCP Parameters:
  files    - Colon-separated absolute paths of indexed memory .md
             files (`:' chosen for POSIX path safety).  Each path
             must be in the metadata index (scan first).
  provider - Optional orchestrator provider id (defaults to
             `anvil-memory-llm-provider').
  model    - Optional model slug (defaults to
             `anvil-memory-llm-model').

Returns (:draft STR :sources LIST :error STR-OR-NIL).  Read-only:
the proposal is returned, never written to disk — callers (the
memory-pruner skill / a human reviewer) decide adoption."
  (anvil-server-with-error-handling
   (let* ((paths (cond ((null files) nil)
                       ((listp files) files)
                       ((and (stringp files) (string-empty-p files)) nil)
                       ((stringp files) (split-string files ":" t))
                       (t nil)))
          (prov (cond ((null provider) nil)
                      ((and (stringp provider) (string-empty-p provider)) nil)
                      (t provider)))
          (mdl (cond ((null model) nil)
                     ((and (stringp model) (string-empty-p model)) nil)
                     (t model))))
     (anvil-memory-mdl-distill paths :provider prov :model mdl))))

(defun anvil-memory--tool-export-html (root &optional out_path title)
  "Render a self-contained HTML snapshot of memories under ROOT.

MCP Parameters:
  root     - Directory whose indexed memory files should be listed.
             Only rows whose `:file' lives under ROOT are included.
  out_path - Optional absolute path.  When set, the HTML is also
             written there; the parent dir is created if missing.
  title    - Optional <title> override.  Empty / omitted keeps the
             default `anvil memory — <root>' heading.

Returns (:html STR :written PATH-OR-NIL).  Offline-openable —
no external CSS / JS references."
  (anvil-server-with-error-handling
   (let* ((out (cond ((null out_path) nil)
                     ((and (stringp out_path) (string-empty-p out_path)) nil)
                     (t out_path)))
          (ttl (cond ((null title) nil)
                     ((and (stringp title) (string-empty-p title)) nil)
                     (t title)))
          (html (anvil-memory-export-html root :out-path out :title ttl)))
     (list :html html :written out))))

(defun anvil-memory--tool-serve-start (&optional host port root)
  "Start the local HTTP memory viewer.

MCP Parameters:
  host - Optional interface to bind (default \"127.0.0.1\").  Avoid
         binding to a non-loopback address — the server has no
         authentication and exposes the metadata index.
  port - Optional port: digit string or `0' (= ephemeral).  Empty /
         omitted uses `anvil-memory-serve-port'.
  root - Optional directory to scope listings to.  Empty / omitted
         serves every indexed memory root.

Returns (:port :host :root :running t)."
  (anvil-server-with-error-handling
   (let* ((h (cond ((null host) nil)
                   ((and (stringp host) (string-empty-p host)) nil)
                   (t host)))
          (p-raw (cond ((null port) nil)
                       ((integerp port) port)
                       ((and (stringp port) (string-empty-p port)) nil)
                       ((stringp port) (string-to-number port))
                       (t nil)))
          (p (cond ((null p-raw) nil)
                   ((and (numberp p-raw) (zerop p-raw)) t)
                   (t p-raw)))
          (r (cond ((null root) nil)
                   ((and (stringp root) (string-empty-p root)) nil)
                   (t root)))
          (info (anvil-memory-serve-start :host h :port p :root r)))
     (list :port (plist-get info :port)
           :host (plist-get info :host)
           :root (plist-get info :root)
           :running t))))

(defun anvil-memory--tool-serve-stop ()
  "Stop the running memory viewer server (no-op when idle).

Returns (:stopped PORT-OR-NIL) — nil indicates the server was
already idle so the caller can detect no-ops."
  (anvil-server-with-error-handling
   (list :stopped (anvil-memory-serve-stop))))

(defun anvil-memory--tool-scan-contradictions (&optional threshold mode
                                                         provider model)
  "Populate memory_contradictions from the current memory index.

MCP Parameters:
  threshold - Optional jaccard floor (number or digit-string).
              Defaults to 0.5.
  mode      - `keyword' (default) or `llm'.  `llm' calls
              `anvil-orchestrator' per candidate and only keeps
              contradicting pairs.
  provider  - Optional orchestrator provider id (llm mode only).
  model     - Optional model slug (llm mode only).

Returns (:scanned :stored :mode :threshold)."
  (anvil-server-with-error-handling
   (let* ((th (anvil-memory--coerce-number threshold 0.5))
          (m-raw (cond ((null mode) nil)
                       ((and (stringp mode) (string-empty-p mode)) nil)
                       ((stringp mode) (intern mode))
                       ((symbolp mode) mode)
                       (t nil)))
          (m (or m-raw 'keyword))
          (prov (cond ((null provider) nil)
                      ((and (stringp provider) (string-empty-p provider)) nil)
                      (t provider)))
          (mdl (cond ((null model) nil)
                     ((and (stringp model) (string-empty-p model)) nil)
                     (t model))))
     (anvil-memory-scan-contradictions
      :threshold th :mode m :provider prov :model mdl))))

(defun anvil-memory--tool-contradictions ()
  "Return every stored contradiction / candidate edge.

Returns (:rows ROWS).  Each row carries :file-a / :file-b /
:verdict / :score / :checked-at — read-only."
  (anvil-server-with-error-handling
   (list :rows (anvil-memory-contradictions))))


;;;; --- module lifecycle ---------------------------------------------------

(defconst anvil-memory--tool-specs
  `((,(anvil-server-encode-handler #'anvil-memory--tool-scan)
     :id "memory-scan"
     :intent '(memory admin)
     :layer 'workflow
     :description
     "(Re-)populate the auto-memory metadata index.  Walks every memory/
directory under ~/.claude/projects/*/ (or an explicit ROOTS list),
reads each .md file's mtime as `created' when the row is new, and
leaves access_count / validity_prior untouched on conflict so
existing metadata survives a rescan.  Phase 1b also refreshes the
FTS5 body index for every file encountered.")

    (,(anvil-server-encode-handler #'anvil-memory--tool-audit)
     :id "memory-audit"
     :intent '(memory admin)
     :layer 'workflow
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
     :intent '(memory)
     :layer 'workflow
     :description
     "Bump access_count and last_accessed for a memory .md file.  Returns
the new access count; `:found' is nil when the file is not in
the index (run `memory-scan' first in that case).")

    (,(anvil-server-encode-handler #'anvil-memory--tool-list)
     :id "memory-list"
     :intent '(memory)
     :layer 'workflow
     :description
     "Dump the metadata index, optionally filtered by TYPE.  Intended as
a Layer-1 slim overview for memory-pruner / audit workflows
rather than for loading memory contents."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory--tool-search)
     :id "memory-search"
     :intent '(memory)
     :layer 'workflow
     :description
     "Full-text search the memory FTS5 index.  Returns file / type /
snippet / rank per hit, ordered by FTS5 rank (best first).  Use
this as Layer 2 before `memory-get'-ish full-body reads."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory--tool-save-check)
     :id "memory-save-check"
     :intent '(memory)
     :layer 'workflow
     :description
     "Return top-N indexed memories similar to a draft SUBJECT/BODY.
FTS5 candidate shortlist plus jaccard overlap scoring (Phase 1b
keyword baseline).  Each row carries :similarity in the 0.0–1.0
range — callers flag >=0.7 as a duplicate / merge candidate.
Phase 2b-ii: pass with_llm=true (plus optional provider / model)
to enrich every candidate with a one-word LLM verdict
(duplicate / contradicting / orthogonal) via anvil-orchestrator.
The LLM path is opt-in — omitting with_llm keeps the response
network-free."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory--tool-duplicates)
     :id "memory-duplicates"
     :intent '(memory admin)
     :layer 'workflow
     :description
     "Return every memory pair whose body jaccard similarity exceeds
THRESHOLD (default 0.6).  O(N^2) over the index — fine up to a
few hundred memories, defer semantic (embedding) duplicates to
Phase 2."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory--tool-promote)
     :id "memory-promote"
     :intent '(memory admin)
     :layer 'workflow
     :description
     "Rename an indexed memory file to reflect NEW_TYPE's prefix and
update metadata + FTS index accordingly.  Refuses overwrite and
missing / unindexed sources so on-disk state stays consistent.
Use this to promote a `MEMO'-style note to `feedback' once it
has proven itself across multiple sessions.")

    (,(anvil-server-encode-handler #'anvil-memory--tool-regenerate)
     :id "memory-regenerate-index"
     :intent '(memory admin)
     :layer 'workflow
     :description
     "Return a `MEMORY.md' body listing memories under ROOT, ordered
by :decay-score descending.  Lines are `- [NAME](FILE) — DESC'
using YAML frontmatter when present (fallback: prettified file
stem).  Read-only — the caller (memory-pruner skill) writes the
returned body to disk after human review."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory--tool-reindex-fts)
     :id "memory-reindex-fts"
     :intent '(memory admin)
     :layer 'workflow
     :description
     "Rebuild the memory_body_fts virtual table with a chosen tokenizer.
Phase 2b-i: `trigram' (SQLite 3.34+) is CJK-friendly — Japanese
substring queries (3+ chars) that miss under `unicode61' start
matching.  Omit tokenizer to use `anvil-memory-fts-tokenizer'.
Default is `auto': use `trigram' when available, otherwise fall
back to `unicode61'.  Returns :tokenizer / :rebuilt.")

    (,(anvil-server-encode-handler #'anvil-memory--tool-mdl-distill)
     :id "memory-mdl-distill"
     :intent '(memory admin)
     :layer 'workflow
     :description
     "Propose a unified \"convention memory\" from N indexed memory files.
Phase 2b-iii: the orchestrator receives each source's body as a
labelled block and returns a draft (frontmatter + body) that
covers them.  Read-only — the draft is returned, never written to
disk, so the memory-pruner skill / a human decides whether to
accept it.  Pass files as a colon-separated absolute-path list;
every entry must already be in the metadata index (run
`memory-scan' first).  Returns :draft / :sources / :error."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory--tool-export-html)
     :id "memory-export-html"
     :intent '(memory admin)
     :layer 'workflow
     :description
     "Render a self-contained HTML snapshot of memories under ROOT.
Phase 3a: static viewer — no TCP server, no external resources, so
the output file opens offline via file://.  Rows are grouped by
type (user / feedback / project / reference / memo) and sorted by
decay-score descending within each section.  Only metadata
(frontmatter name / description, filename, decay, access-count,
last-accessed) is emitted; memory bodies stay on disk so a stray
.html file never leaks session text.  Pass `out_path' to also
persist the page alongside the returned HTML string.  Returns
:html + :written."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-memory--tool-serve-start)
     :id "memory-serve-start"
     :intent '(memory admin)
     :layer 'workflow
     :description
     "Start the local HTTP memory viewer on 127.0.0.1:<port>.
Phase 3b: serves the Phase 3a HTML at `/', JSON at
`/api/list' and `/api/decay-heatmap'.  Opt-in — the server has no
authentication, bind only to loopback.  `host' / `port' / `root'
are all optional and coerced from strings.  Returns :port / :host /
:root / :running.")

    (,(anvil-server-encode-handler #'anvil-memory--tool-serve-stop)
     :id "memory-serve-stop"
     :intent '(memory admin)
     :layer 'workflow
     :description
     "Stop the running memory viewer server.  Idempotent — the MCP
call always succeeds; :stopped is the port number that was being
served, or nil when the server was already idle.")

    (,(anvil-server-encode-handler #'anvil-memory--tool-scan-contradictions)
     :id "memory-scan-contradictions"
     :intent '(memory admin)
     :layer 'workflow
     :description
     "Phase 3c: find memory pairs above a jaccard threshold and persist
them in `memory_contradictions'.  mode=\"keyword\" (default) stores
every candidate verbatim for later review; mode=\"llm\" calls
anvil-orchestrator per candidate and only keeps pairs flagged
`contradicting'.  Idempotent — rows are upserted by (file_a, file_b)
with canonical (sorted) pair ordering.  Returns :scanned / :stored /
:mode / :threshold.")

    (,(anvil-server-encode-handler #'anvil-memory--tool-contradictions)
     :id "memory-contradictions"
     :intent '(memory admin)
     :layer 'workflow
     :description
     "Phase 3c: return every stored contradiction / candidate edge as
plists with :file-a / :file-b / :verdict / :score / :checked-at.
Read-only — rows are populated by `memory-scan-contradictions'."
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
