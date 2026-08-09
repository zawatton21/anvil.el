;;; anvil-harness-telemetry.el --- Runtime harness failure classifier + telemetry (4-class) -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 46 Phase 1.  4-class harness failure telemetry (no-exec /
;; contract-violation / stall / reasoning).  Hooks into
;; `anvil-server-tool-error-hook' (raised from inside
;; `anvil-server-with-error-handling' and from the tools/call dispatcher)
;; to classify each failure with a static rule table and persist the
;; event into a dedicated SQLite table inside the anvil-worklog DB file.
;;
;; Three MCP tools — `harness-telemetry-record', `-stats', `-recent' —
;; expose the recorder API and aggregator queries to other modules.
;;
;; D8: Phase 1 ships Emacs-only; NeLisp dual-target lands once the
;; standalone runtime has the anvil-server / anvil-worklog stack.

;;; anvil-audit: tools-wrapped-at-registration

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-server)
(require 'anvil-worklog)


;;;; --- customization ----------------------------------------------------

(defgroup anvil-harness-telemetry nil
  "Runtime harness failure classifier + telemetry (Doc 46 Phase 1)."
  :group 'anvil
  :prefix "anvil-harness-telemetry-")

(defcustom anvil-harness-telemetry-db-path nil
  "Override path for the harness telemetry SQLite DB.
When nil, the table lives inside the file returned by
`anvil-worklog-effective-db-path' so anvil-worklog and
anvil-harness-telemetry share a single DB file (separate tables).
Tests bind this to a temp file for isolation."
  :type '(choice (const :tag "Share with anvil-worklog" nil) file)
  :group 'anvil-harness-telemetry)

(defcustom anvil-harness-telemetry-confidence-low-threshold 0.5
  "Below this confidence value a recorded event is flagged low-confidence.
The threshold is the gate for the `:low-confidence-count' bucket
returned by `anvil-harness-telemetry-stats' (D4)."
  :type 'number
  :group 'anvil-harness-telemetry)

(defcustom anvil-harness-telemetry-raw-context-max-chars 500
  "Cap on bytes stored per event in the `raw_context' column.
Errors whose `format \"%S\"' is larger than this value are
truncated.  Keeps the DB small and bounds the FTS5 index growth."
  :type 'integer
  :group 'anvil-harness-telemetry)


;;;; --- server id ---------------------------------------------------------

(defconst anvil-harness-telemetry--server-id "emacs-eval"
  "Server ID under which `harness-telemetry-*' MCP tools register.
Must match the `--server-id' the stdio shim uses (memory:
`feedback_anvil_server_id_must_match_stdio').")


;;;; --- classifier rules (D1) --------------------------------------------

(defconst anvil-harness-telemetry-classifier-rules
  '(;; no-exec (実行不能 ~ 33.3%)
    (no-exec
     :error-symbols (void-function
                     error-process-exited-abnormally
                     file-missing
                     file-error)
     :regex ("not callable"
             "Connection refused"
             "No such file or directory"
             "Permission denied"
             "process exited [1-9]"))

    ;; contract-violation (契約違反 ~ 23.3%)
    (contract-violation
     :error-symbols (wrong-type-argument
                     wrong-number-of-arguments)
     :regex ("Missing required parameter"
             "Unexpected parameter"
             "json-schema validation"
             "Invalid argument"
             "InputValidationError"))

    ;; stall (反復停滞 ~ 13.2%) — repetition-detector emit のみ
    (stall :emit-only-from-repetition-detector t)

    ;; reasoning (推論ミス ~ 30%) — fallback
    (reasoning :fallback t))
  "Static rule table consumed by `anvil-harness-telemetry--classify' (Doc 46 D1).
Rules are scanned in order; first hit wins.  `wrong-type-argument' is
stack-depth ambiguous (dispatcher input = contract-violation, tool body =
no-exec) and resolved by the `:source' hint passed to the classifier,
not by the rule table itself.")


;;;; --- classifier --------------------------------------------------------

(defun anvil-harness-telemetry--error-symbol (err)
  "Return the primary symbol of error condition cell ERR, or nil."
  (cond
   ((symbolp err) err)
   ((and (consp err) (symbolp (car err))) (car err))))

(defun anvil-harness-telemetry--error-message (err)
  "Return a human-readable string for error condition cell ERR."
  (cond
   ((stringp err) err)
   ((consp err)
    (condition-case nil
        (error-message-string err)
      (error (format "%S" err))))
   ((null err) "")
   (t (format "%S" err))))

(defun anvil-harness-telemetry--rule-hits-p (rule sym msg)
  "Return non-nil when RULE plist matches SYM / MSG."
  (let ((symbols (plist-get rule :error-symbols))
        (regexes (plist-get rule :regex)))
    (or (and sym symbols (memq sym symbols))
        (and msg regexes
             (cl-some (lambda (re) (string-match-p re msg)) regexes)))))

(cl-defun anvil-harness-telemetry--classify (err &key source)
  "Classify error condition cell ERR.

Return (CLASS . CONFIDENCE).  CLASS is one of `no-exec',
`contract-violation', `stall', `reasoning'.  CONFIDENCE is 1.0
when a rule matched explicitly and 0.3 for the `reasoning'
fallback.

SOURCE hints at the call site for stack-depth-aware rules:
  `dispatcher-validation' — error raised inside the dispatcher's
                            pre-call validation step.  Treated as
                            contract-violation for ambiguous symbols.
  `tool-body'             — error raised inside a tool handler body
                            (via `anvil-server-with-error-handling'
                            or the dispatcher's generic `error'
                            arm).  Treated as no-exec for ambiguous
                            symbols.
  `repetition-detector'   — caller already detected a stall pattern;
                            classification is forced to `stall'.

Stack-depth disambiguation matters because `wrong-type-argument'
raised at the dispatcher input means a schema-level contract
violation, while the same symbol raised inside the tool body
points at an unreachable or mis-coerced internal call."
  (let* ((sym (anvil-harness-telemetry--error-symbol err))
         (msg (anvil-harness-telemetry--error-message err)))
    (cond
     ((eq source 'repetition-detector)
      (cons 'stall 1.0))
     ((and (eq sym 'wrong-type-argument)
           (eq source 'dispatcher-validation))
      (cons 'contract-violation 1.0))
     ((and (eq sym 'wrong-type-argument)
           (eq source 'tool-body))
      (cons 'no-exec 1.0))
     (t
      (catch 'hit
        (dolist (rule-entry anvil-harness-telemetry-classifier-rules)
          (let ((class (car rule-entry))
                (props (cdr rule-entry)))
            (cond
             ((plist-get props :fallback) nil)
             ((plist-get props :emit-only-from-repetition-detector) nil)
             ((anvil-harness-telemetry--rule-hits-p props sym msg)
              (throw 'hit (cons class 1.0))))))
        (cons 'reasoning 0.3))))))


;;;; --- sqlite backend ----------------------------------------------------

(defvar anvil-harness-telemetry--db nil
  "Open SQLite handle on the resolved DB path (nil when idle).")

(defvar anvil-harness-telemetry--resolved-db-path nil
  "Cached path the handle was opened against.
Reset by `anvil-harness-telemetry--close' so a re-open re-runs
detection.")

(defun anvil-harness-telemetry--resolve-db-path ()
  "Return the effective DB path.
When `anvil-harness-telemetry-db-path' is set it wins; otherwise
defer to `anvil-worklog-effective-db-path' so both modules share
one file."
  (or anvil-harness-telemetry-db-path
      (anvil-worklog-effective-db-path)))

(defun anvil-harness-telemetry-effective-db-path ()
  "Return the path the DB is currently (or would be) opened at."
  (or anvil-harness-telemetry--resolved-db-path
      (anvil-harness-telemetry--resolve-db-path)))

(defun anvil-harness-telemetry--require-sqlite ()
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (user-error
     "anvil-harness-telemetry: Emacs SQLite backend unavailable (needs Emacs 29+)")))

(defun anvil-harness-telemetry--trigram-supported-p (db)
  "Return non-nil when DB's SQLite build ships the FTS5 trigram tokenizer."
  (condition-case nil
      (progn
        (sqlite-execute
         db
         "CREATE VIRTUAL TABLE anvil_ht_trigram_probe
            USING fts5(x, tokenize=trigram)")
        (sqlite-execute db "DROP TABLE anvil_ht_trigram_probe")
        t)
    (error nil)))

(defun anvil-harness-telemetry--ensure-schema ()
  "Create harness_failures + harness_failures_fts when missing."
  (let* ((db anvil-harness-telemetry--db)
         (tok-sql (if (anvil-harness-telemetry--trigram-supported-p db)
                      ", tokenize='trigram'"
                    "")))
    (sqlite-execute
     db
     "CREATE TABLE IF NOT EXISTS harness_failures (
        id           INTEGER PRIMARY KEY AUTOINCREMENT,
        timestamp    TEXT NOT NULL,
        class        TEXT NOT NULL,
        confidence   REAL NOT NULL,
        tool         TEXT,
        args_digest  TEXT,
        error_message TEXT,
        provider     TEXT,
        session_id   TEXT,
        machine      TEXT,
        raw_context  TEXT
      )")
    (sqlite-execute
     db
     "CREATE INDEX IF NOT EXISTS harness_failures_class_idx
        ON harness_failures(class)")
    (sqlite-execute
     db
     "CREATE INDEX IF NOT EXISTS harness_failures_ts_idx
        ON harness_failures(timestamp)")
    (sqlite-execute
     db
     "CREATE INDEX IF NOT EXISTS harness_failures_tool_idx
        ON harness_failures(tool)")
    (sqlite-execute
     db
     (format
      "CREATE VIRTUAL TABLE IF NOT EXISTS harness_failures_fts
         USING fts5(error_message, raw_context,
                    content='harness_failures',
                    content_rowid='id'%s)"
      tok-sql))))

(defun anvil-harness-telemetry--open ()
  "Open the backing DB and apply schema."
  (anvil-harness-telemetry--require-sqlite)
  (unless anvil-harness-telemetry--db
    (let ((path (anvil-harness-telemetry--resolve-db-path)))
      (setq anvil-harness-telemetry--resolved-db-path path)
      (make-directory (file-name-directory path) t)
      (setq anvil-harness-telemetry--db (sqlite-open path))
      (anvil-harness-telemetry--ensure-schema)))
  anvil-harness-telemetry--db)

(defun anvil-harness-telemetry--close ()
  "Close the DB and clear cached state."
  (when anvil-harness-telemetry--db
    (ignore-errors (sqlite-close anvil-harness-telemetry--db))
    (setq anvil-harness-telemetry--db nil))
  (setq anvil-harness-telemetry--resolved-db-path nil))

(defun anvil-harness-telemetry--db ()
  (or anvil-harness-telemetry--db (anvil-harness-telemetry--open)))


;;;; --- machine / timestamp helpers ---------------------------------------

(defun anvil-harness-telemetry--current-machine ()
  "Return the conventional `<platform>-<host>' machine token."
  (if (fboundp 'anvil-worklog--current-machine-token)
      (anvil-worklog--current-machine-token)
    (let ((sn (system-name)))
      (if (and (stringp sn) (string-match "\\`\\([^.]+\\)" sn))
          (match-string 1 sn)
        sn))))

(defun anvil-harness-telemetry--now-iso ()
  "Return the current local time as `YYYY-MM-DDTHH:MM:SS±ZZZZ'."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun anvil-harness-telemetry--raw-context (err)
  "Return a length-capped raw representation of ERR."
  (let ((s (format "%S" err)))
    (if (> (length s) anvil-harness-telemetry-raw-context-max-chars)
        (substring s 0 anvil-harness-telemetry-raw-context-max-chars)
      s)))


;;;; --- event recorder ----------------------------------------------------

;;;###autoload
(cl-defun anvil-harness-telemetry-record
    (class &key confidence tool args-digest error-message
           provider session machine raw-context timestamp)
  "Insert one harness failure event row and return its identity plist.

CLASS is one of `no-exec', `contract-violation', `stall',
`reasoning'.  Other fields are optional and default to nil /
sensible values:

  :confidence    — 0.0-1.0 (default 1.0).  Below
                   `anvil-harness-telemetry-confidence-low-threshold'
                   the returned plist carries :low-confidence t.
  :tool          — tool name string.
  :args-digest   — sxhash hex of the tool arguments (used by the
                   repetition-detector in Phase 2).
  :error-message — short human-readable message string.
  :provider      — claude / codex / gemini / etc.
  :session       — session id string.
  :machine       — `<platform>-<host>' token (default: current machine).
  :raw-context   — length-capped raw error representation.
  :timestamp     — ISO datetime (default: current local time).

Returns =(:id ROWID :class CLASS :confidence CONFIDENCE :low-confidence BOOL)=."
  (unless (memq class '(no-exec contract-violation stall reasoning))
    (user-error "anvil-harness-telemetry-record: invalid class %S" class))
  (let* ((db (anvil-harness-telemetry--db))
         (conf (or confidence 1.0))
         (low (< conf anvil-harness-telemetry-confidence-low-threshold))
         (ts (or timestamp (anvil-harness-telemetry--now-iso)))
         (m (or machine (anvil-harness-telemetry--current-machine))))
    (sqlite-execute
     db
     "INSERT INTO harness_failures
        (timestamp, class, confidence, tool, args_digest,
         error_message, provider, session_id, machine, raw_context)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     (list ts (symbol-name class) conf
           tool args-digest error-message provider session m raw-context))
    (let ((rowid (caar (sqlite-select db "SELECT last_insert_rowid()"))))
      (sqlite-execute
       db
       "INSERT INTO harness_failures_fts (rowid, error_message, raw_context)
        VALUES (?, ?, ?)"
       (list rowid (or error-message "") (or raw-context "")))
      (list :id rowid
            :class class
            :confidence conf
            :low-confidence low))))

;;;###autoload
(cl-defun anvil-harness-telemetry-record-from-error
    (err &key source tool provider session args-digest)
  "Classify ERR and persist a harness failure event.

ERR is a raw error condition cell as bound by `condition-case'.
SOURCE is documented on `anvil-harness-telemetry--classify'.

Failures inside the recorder itself are caught and logged (silent
fail per the Doc 46 safety policy) so telemetry never breaks the
main flow.

Returns the same plist as `anvil-harness-telemetry-record', or
nil on internal failure."
  (condition-case telemetry-err
      (let* ((classification (anvil-harness-telemetry--classify
                              err :source source))
             (class (car classification))
             (confidence (cdr classification)))
        (anvil-harness-telemetry-record
         class
         :confidence confidence
         :tool (when tool (format "%s" tool))
         :args-digest args-digest
         :error-message (anvil-harness-telemetry--error-message err)
         :provider provider
         :session session
         :raw-context (anvil-harness-telemetry--raw-context err)))
    (error
     (message "anvil-harness-telemetry: record-from-error swallowed: %s"
              (error-message-string telemetry-err))
     nil)))


;;;; --- aggregation (stats / recent) --------------------------------------

(defun anvil-harness-telemetry--row->plist (row)
  "Convert a harness_failures ROW (11 columns) into a plist."
  (list :id (nth 0 row)
        :timestamp (nth 1 row)
        :class (intern (nth 2 row))
        :confidence (nth 3 row)
        :tool (nth 4 row)
        :args-digest (nth 5 row)
        :error-message (nth 6 row)
        :provider (nth 7 row)
        :session-id (nth 8 row)
        :machine (nth 9 row)
        :raw-context (nth 10 row)))

(defun anvil-harness-telemetry--range-where (since until)
  "Return (WHERE-SQL . ARGS) for SINCE / UNTIL filters."
  (let (parts args)
    (when (and since (not (string-empty-p since)))
      (push "timestamp >= ?" parts) (push since args))
    (when (and until (not (string-empty-p until)))
      (push "timestamp <= ?" parts) (push until args))
    (cons (if parts (mapconcat #'identity (nreverse parts) " AND ") "")
          (nreverse args))))

(defun anvil-harness-telemetry--compose-where (base extra)
  "Return a `WHERE ...' fragment from BASE clause and EXTRA clause.
Either argument may be nil / empty."
  (let ((b (and base (not (string-empty-p base)) base))
        (e (and extra (not (string-empty-p extra)) extra)))
    (cond
     ((and b e) (format " WHERE %s AND %s" b e))
     (b (format " WHERE %s" b))
     (e (format " WHERE %s" e))
     (t ""))))

(cl-defun anvil-harness-telemetry-stats (&key since until)
  "Aggregate harness failure events.

Optional SINCE / UNTIL are ISO timestamps (inclusive) clipping the
timestamp range.

Returns a plist:
  :total                 N
  :by-class              ((CLASS-SYM . COUNT) ...)
  :by-tool               ((TOOL . COUNT) ...)      ; top 10
  :by-provider           ((PROVIDER . COUNT) ...)
  :low-confidence-count  N (rows with confidence below the
                            configurable threshold)
  :by-day                ((YYYY-MM-DD . COUNT) ...) DESC"
  (let* ((db (anvil-harness-telemetry--db))
         (rw (anvil-harness-telemetry--range-where since until))
         (base (car rw))
         (range-args (cdr rw)))
    (cl-flet ((q (sql extra-args)
                 (let ((all (append range-args extra-args)))
                   (if all (sqlite-select db sql all)
                     (sqlite-select db sql)))))
      (let* ((total
              (caar (q (concat "SELECT COUNT(*) FROM harness_failures"
                               (anvil-harness-telemetry--compose-where base nil))
                       nil)))
             (by-class
              (q (concat "SELECT class, COUNT(*) FROM harness_failures"
                         (anvil-harness-telemetry--compose-where base nil)
                         " GROUP BY class")
                 nil))
             (by-tool
              (q (concat "SELECT tool, COUNT(*) FROM harness_failures"
                         (anvil-harness-telemetry--compose-where
                          base "tool IS NOT NULL")
                         " GROUP BY tool"
                         " ORDER BY COUNT(*) DESC LIMIT 10")
                 nil))
             (by-provider
              (q (concat "SELECT provider, COUNT(*) FROM harness_failures"
                         (anvil-harness-telemetry--compose-where
                          base "provider IS NOT NULL")
                         " GROUP BY provider")
                 nil))
             (low-conf
              (caar (q (concat "SELECT COUNT(*) FROM harness_failures"
                               (anvil-harness-telemetry--compose-where
                                base "confidence < ?"))
                       (list anvil-harness-telemetry-confidence-low-threshold))))
             (by-day
              (q (concat
                  "SELECT substr(timestamp,1,10), COUNT(*) FROM harness_failures"
                  (anvil-harness-telemetry--compose-where base nil)
                  " GROUP BY substr(timestamp,1,10)"
                  " ORDER BY substr(timestamp,1,10) DESC")
                 nil)))
        (list :total (or total 0)
              :by-class (mapcar (lambda (r) (cons (intern (nth 0 r))
                                                  (nth 1 r)))
                                by-class)
              :by-tool (mapcar (lambda (r) (cons (nth 0 r) (nth 1 r)))
                               by-tool)
              :by-provider (mapcar (lambda (r) (cons (nth 0 r) (nth 1 r)))
                                   by-provider)
              :low-confidence-count (or low-conf 0)
              :by-day (mapcar (lambda (r) (cons (nth 0 r) (nth 1 r)))
                              by-day))))))

(cl-defun anvil-harness-telemetry-recent (&key limit class)
  "Return recent harness failure rows ordered by timestamp DESC.

Optional LIMIT (default 20) caps the result count.  Optional
CLASS filters by class symbol (`no-exec' / `contract-violation' /
`stall' / `reasoning')."
  (let* ((db (anvil-harness-telemetry--db))
         (lim (or limit 20))
         (where '()) (args '()))
    (when class
      (push "class = ?" where)
      (push (symbol-name class) args))
    (let* ((where-sql (if where
                          (concat " WHERE "
                                  (mapconcat #'identity
                                             (nreverse where) " AND "))
                        ""))
           (args* (nreverse args))
           (sql (concat
                 "SELECT id, timestamp, class, confidence, tool,
                         args_digest, error_message, provider,
                         session_id, machine, raw_context
                    FROM harness_failures"
                 where-sql
                 " ORDER BY timestamp DESC, id DESC LIMIT "
                 (number-to-string lim)))
           (rows (if args*
                     (sqlite-select db sql args*)
                   (sqlite-select db sql))))
      (mapcar #'anvil-harness-telemetry--row->plist rows))))


;;;; --- MCP wrappers ------------------------------------------------------

(defun anvil-harness-telemetry--coerce-int (v default)
  (cond ((integerp v) v)
        ((and (stringp v) (string-match "\\`[0-9]+\\'" v))
         (string-to-number v))
        (t default)))

(defun anvil-harness-telemetry--coerce-string (v)
  (cond ((null v) nil)
        ((and (stringp v) (string-empty-p v)) nil)
        ((stringp v) v)
        (t nil)))

(defun anvil-harness-telemetry--coerce-class (v)
  (let ((s (anvil-harness-telemetry--coerce-string v)))
    (and s (intern s))))

(defun anvil-harness-telemetry--coerce-float (v default)
  (cond ((numberp v) v)
        ((and (stringp v)
              (string-match "\\`[0-9]+\\(\\.[0-9]+\\)?\\'" v))
         (string-to-number v))
        (t default)))

(defun anvil-harness-telemetry--tool-record
    (class &optional confidence tool error_message provider session
           args_digest raw_context)
  "Insert a harness failure event row (internal API for host modules).

MCP Parameters:
  class         - Required class string: no-exec / contract-violation
                  / stall / reasoning.
  confidence    - Optional 0.0-1.0 (default 1.0).
  tool          - Optional tool name.
  error_message - Optional human-readable error string.
  provider      - Optional provider id (claude / codex / gemini / ...).
  session       - Optional session id.
  args_digest   - Optional sxhash digest of tool arguments.
  raw_context   - Optional truncated raw representation.

Returns =(:id ROWID :class CLASS :confidence CONFIDENCE :low-confidence BOOL)=."
  (anvil-server-with-error-handling
   (let ((cls (anvil-harness-telemetry--coerce-class class)))
     (unless cls
       (anvil-server-tool-throw
        "harness-telemetry-record: `class' is required"))
     (anvil-harness-telemetry-record
      cls
      :confidence (anvil-harness-telemetry--coerce-float confidence 1.0)
      :tool (anvil-harness-telemetry--coerce-string tool)
      :error-message (anvil-harness-telemetry--coerce-string error_message)
      :provider (anvil-harness-telemetry--coerce-string provider)
      :session (anvil-harness-telemetry--coerce-string session)
      :args-digest (anvil-harness-telemetry--coerce-string args_digest)
      :raw-context (anvil-harness-telemetry--coerce-string raw_context)))))

(defun anvil-harness-telemetry--tool-stats (&optional since until)
  "Aggregate harness failure events into per-class / -tool / -provider counts.

MCP Parameters:
  since - Optional ISO timestamp lower bound (inclusive).
  until - Optional ISO timestamp upper bound (inclusive).

Returns the plist documented on `anvil-harness-telemetry-stats'."
  (anvil-server-with-error-handling
   (anvil-harness-telemetry-stats
    :since (anvil-harness-telemetry--coerce-string since)
    :until (anvil-harness-telemetry--coerce-string until))))

(defun anvil-harness-telemetry--tool-recent (&optional limit class)
  "Return recent harness failure rows ordered by timestamp DESC.

MCP Parameters:
  limit - Optional max result count (default 20).
  class - Optional class filter (no-exec / contract-violation /
          stall / reasoning).

Returns =(:rows ROWS)=."
  (anvil-server-with-error-handling
   (list :rows
         (anvil-harness-telemetry-recent
          :limit (anvil-harness-telemetry--coerce-int limit 20)
          :class (anvil-harness-telemetry--coerce-class class)))))


;;;; --- dispatcher hook ---------------------------------------------------

(defun anvil-harness-telemetry--dispatcher-hook (err &optional tool-name source)
  "Hook function added to `anvil-server-tool-error-hook'.

ERR is a newly constructed condition cell containing only an allowlisted
symbol and bounded text.  TOOL-NAME is the fixed-grammar active tool id or
`<oversized-tool-id>'.  SOURCE is the call-site hint documented on
`anvil-harness-telemetry--classify'."
  (anvil-harness-telemetry-record-from-error
   err
   :source (or source 'tool-body)
   :tool tool-name))


;;;; --- module lifecycle --------------------------------------------------

(defconst anvil-harness-telemetry--tool-specs
  `((,(anvil-server-encode-handler #'anvil-harness-telemetry--tool-record)
     :id "harness-telemetry-record"
     :intent (observability admin)
     :layer workflow
     :description
     "Insert one harness failure event row (Doc 46 Phase 1).  The
dispatcher hook installed by `anvil-harness-telemetry-enable' is the
primary write path; this tool is for host modules that already
classified a failure on their own (anvil-claude-watchdog stale
detection, anvil-orchestrator provider error parsing, etc.) and want
to persist it directly.  Required `class' (no-exec / contract-violation /
stall / reasoning).  Optional `confidence' (default 1.0) — below the
configured low-confidence threshold the row is flagged for manual
review.")

    (,(anvil-server-encode-handler #'anvil-harness-telemetry--tool-stats)
     :id "harness-telemetry-stats"
     :intent (observability)
     :layer workflow
     :description
     "Aggregate harness failure events into per-class / per-tool /
per-provider / per-day counters plus a low-confidence bucket.  Use
for measuring the effect of Doc 43-47 improvements over the 1-2
week baseline observation period (Doc 46 D6).  Optional `since' /
`until' ISO timestamps clip the range."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-harness-telemetry--tool-recent)
     :id "harness-telemetry-recent"
     :intent (observability)
     :layer workflow
     :description
     "Return recent harness failure events ordered by timestamp DESC.
Pair with `harness-telemetry-stats' to drill into a spike — once an
aggregate count looks off, pull the latest rows for that class with
this tool.  Optional `limit' (default 20) and `class' filter."
     :read-only t))
  "Spec list consumed by `anvil-server-register-tools'.")

(defun anvil-harness-telemetry--register-tools ()
  (anvil-server-register-tools anvil-harness-telemetry--server-id
                               anvil-harness-telemetry--tool-specs))

(defun anvil-harness-telemetry--unregister-tools ()
  (anvil-server-unregister-tools anvil-harness-telemetry--server-id
                                 anvil-harness-telemetry--tool-specs))

;;;###autoload
(defun anvil-harness-telemetry-enable ()
  "Open the telemetry DB, register MCP tools, and install the dispatcher hook."
  (interactive)
  (anvil-harness-telemetry--open)
  (anvil-harness-telemetry--register-tools)
  (add-hook 'anvil-server-tool-error-hook
            #'anvil-harness-telemetry--dispatcher-hook))

;;;###autoload
(defun anvil-harness-telemetry-disable ()
  "Remove the dispatcher hook, unregister MCP tools, and close the DB."
  (interactive)
  (remove-hook 'anvil-server-tool-error-hook
               #'anvil-harness-telemetry--dispatcher-hook)
  (anvil-harness-telemetry--unregister-tools)
  (anvil-harness-telemetry--close))

(provide 'anvil-harness-telemetry)
;;; anvil-harness-telemetry.el ends here
