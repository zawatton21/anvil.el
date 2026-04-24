;;; anvil-session.el --- Session snapshots + Claude Code hooks  -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;;; Commentary:

;; Doc 17 — Session snapshot / resume / Claude Code lifecycle hooks.
;;
;; This module answers "how does a resuming Claude pick up where the
;; previous session left off without re-reading half the project?"
;;
;; The surface has three layers (Doc 17 Phases 1 / 3 / 4):
;;
;;   Phase 1 (primitives):  `anvil-session-snapshot' / -resume / -list
;;                          / -delete.  Plist records stored in
;;                          anvil-state ns="session", TTL 14 days.
;;
;;   Phase 3 (hooks):       `anvil-session-hook-dispatch' routes
;;                          Claude Code lifecycle events (SessionStart
;;                          / PreCompact / PostToolUse /
;;                          UserPromptSubmit / SessionEnd) to snapshot
;;                          + event-index primitives.  Events land in
;;                          anvil-state ns="session-events" for
;;                          per-session audit + search.
;;
;;   Phase 4 (opt-in):      `anvil-session-hooks-enabled' defcustom +
;;                          `anvil-hook-install-settings' command that
;;                          writes the hook bindings into
;;                          `~/.claude/settings.json' (dry-run, install,
;;                          uninstall, preserves existing hooks).
;;
;; Snapshot payload shape (plist):
;;   :name STR :created-at FLOAT-TIME
;;   :branch STR :base-branch STR
;;   :task-summary STR :notes LIST
;;   :preamble-suggested STR
;;
;; Event row shape (plist):
;;   :session-id STR :ts FLOAT-TIME :kind STR :tool STR :summary STR
;;
;; Both are round-trippable through `prin1' / `read' so anvil-state's
;; serializer handles them with no extra ceremony.

;;; Code:

(require 'cl-lib)
(require 'anvil-state)
(require 'anvil-server)


;;;; --- group + constants --------------------------------------------------

(defgroup anvil-session nil
  "Session snapshot / resume / Claude Code hook integration (Doc 17)."
  :group 'anvil
  :prefix "anvil-session-")

(defconst anvil-session--server-id "emacs-eval"
  "Server ID under which session-* MCP tools are registered.")

(defconst anvil-session--snapshot-ns "session"
  "anvil-state namespace for snapshot storage.")

(defconst anvil-session--events-ns "session-events"
  "anvil-state namespace for per-session event log rows.
Row keys use the shape \"<session-id>/<zero-padded-seq>-<ts>\" so
`anvil-state-list-keys' returns events in insertion order for a
given session, and wildcards like \"SESSION/\" act as session
prefixes for search.")

(defcustom anvil-session-ttl-sec (* 14 24 60 60)
  "TTL (seconds) for snapshot rows before anvil-state purges them.
Default 14 days.  Event rows use `anvil-session-events-ttl-sec'."
  :type 'integer
  :group 'anvil-session)

(defcustom anvil-session-events-ttl-sec (* 14 24 60 60)
  "TTL (seconds) for event-log rows (`session-events' namespace).
Default 14 days."
  :type 'integer
  :group 'anvil-session)

(defcustom anvil-session-event-summary-max-chars 200
  "Upper bound on `:summary' chars recorded per event.
Matches the Doc 17 design target — keeps the FTS surface tight
without needing full payload replay."
  :type 'integer
  :group 'anvil-session)

(defcustom anvil-session-default-base-branch "main"
  "Fallback `:base-branch' for snapshots when the caller omits it."
  :type 'string
  :group 'anvil-session)

(defcustom anvil-session-hooks-enabled nil
  "Non-nil to opt into Claude Code lifecycle hook dispatch (Phase 4).
The defcustom itself does nothing — it exists for
`anvil-hook-install-settings' to short-circuit when the user has
not explicitly opted in, matching the Doc 17 safety-rail design."
  :type 'boolean
  :group 'anvil-session)

(defcustom anvil-session-hook-script
  (expand-file-name "scripts/anvil-hook"
                    (or (and load-file-name
                             (file-name-directory load-file-name))
                        default-directory))
  "Absolute path to the `anvil-hook' shell wrapper.
Used by `anvil-hook-install-settings' to emit the command line that
binds to each Claude Code lifecycle event.  On Windows swap this to
the `.bat' sibling."
  :type 'file
  :group 'anvil-session)

(defcustom anvil-session-claude-settings-path
  (expand-file-name "~/.claude/settings.json")
  "Path to the Claude Code user-settings JSON file.
`anvil-hook-install-settings' deep-merges an `hooks' block into this
file, preserving every other top-level key.  Override per-machine
(e.g. `project/.claude/settings.json' for per-project hooks)."
  :type 'file
  :group 'anvil-session)


;;;; --- Phase 1: snapshot primitives ---------------------------------------

(defun anvil-session--git-branch ()
  "Return the current git branch name as a string, or nil.
Uses a plain `process-file' / `call-process' shell-out rather than
requiring magit — anvil-session has to work on machines without
heavy git helpers loaded (e.g. a fresh worktree in CI)."
  (let ((out (with-temp-buffer
               (when (zerop (or (ignore-errors
                                  (call-process
                                   "git" nil t nil
                                   "rev-parse" "--abbrev-ref" "HEAD"))
                                1))
                 (string-trim (buffer-string))))))
    (and (stringp out) (not (string-empty-p out)) out)))

(defun anvil-session--format-preamble (snap)
  "Build a resume-prompt string from SNAP's plist.
The Doc 17 contract: one preamble block that Claude can drop into
its first response without ≥N `Read' calls to rebuild context.
Only non-empty fields are surfaced; the caller may always fall
back to the raw plist via `anvil-session-resume'."
  (let ((name    (plist-get snap :name))
        (created (plist-get snap :created-at))
        (branch  (plist-get snap :branch))
        (base    (plist-get snap :base-branch))
        (task    (plist-get snap :task-summary))
        (notes   (plist-get snap :notes)))
    (concat
     (format "Resuming session %S (last active %s).\n"
             (or name "anon")
             (if (numberp created)
                 (format-time-string "%Y-%m-%d %H:%M" created)
               "?"))
     (format "Branch: %s (base %s).\n"
             (or branch "?") (or base "?"))
     (when (and (stringp task) (not (string-empty-p task)))
       (format "Last task: %s\n" task))
     (when notes
       (concat "Recent notes:\n"
               (mapconcat (lambda (n) (format "  - %s" n))
                          (if (listp notes) notes (list notes))
                          "\n")
               "\n")))))

;;;###autoload
(cl-defun anvil-session-snapshot (name &key branch base-branch
                                         task-summary notes)
  "Capture current state into a named snapshot NAME, persist, return plist.

Keyword arguments:
  :branch        — override autodetected git branch.
  :base-branch   — override `anvil-session-default-base-branch'.
  :task-summary  — human-readable \"what was I doing\" sentence.
  :notes         — list of bullet strings; single string wraps to list.

Writes into anvil-state namespace `session' with TTL
`anvil-session-ttl-sec'.  Returns the stored plist (including the
auto-generated `:preamble-suggested')."
  (unless (and (stringp name) (not (string-empty-p name)))
    (user-error "anvil-session-snapshot: NAME must be a non-empty string"))
  (let* ((snap (list :name name
                     :created-at (float-time)
                     :branch (or branch (anvil-session--git-branch))
                     :base-branch (or base-branch
                                      anvil-session-default-base-branch)
                     :task-summary task-summary
                     :notes (cond ((null notes) nil)
                                  ((stringp notes) (list notes))
                                  ((listp notes) notes)
                                  (t (list (format "%s" notes))))))
         (snap (plist-put snap :preamble-suggested
                          (anvil-session--format-preamble snap))))
    (anvil-state-set name snap
                     :ns anvil-session--snapshot-ns
                     :ttl anvil-session-ttl-sec)
    snap))

;;;###autoload
(defun anvil-session-resume (name)
  "Return the snapshot plist stored under NAME, or nil when missing.
Does not mutate live state; the caller composes the resume flow
(e.g. feeding `:preamble-suggested' to `orchestrator-preamble-set')."
  (anvil-state-get name :ns anvil-session--snapshot-ns))

;;;###autoload
(defun anvil-session-list ()
  "Return a list of snapshot descriptor plists for every live row.
Each descriptor carries `:name', `:created-at', `:branch', and
`:task-summary-head' (first 80 chars of the stored summary, or
nil).  The full payload remains accessible via `session-resume'."
  (let (out)
    (dolist (k (anvil-state-list-keys :ns anvil-session--snapshot-ns))
      (let ((s (anvil-state-get k :ns anvil-session--snapshot-ns)))
        (when s
          (push (list :name (plist-get s :name)
                      :created-at (plist-get s :created-at)
                      :branch (plist-get s :branch)
                      :task-summary-head
                      (let ((t-sum (plist-get s :task-summary)))
                        (and (stringp t-sum)
                             (substring t-sum 0
                                        (min 80 (length t-sum))))))
                out))))
    (nreverse out)))

;;;###autoload
(defun anvil-session-delete (name)
  "Purge snapshot NAME from anvil-state; return t when a row was deleted."
  (unless (and (stringp name) (not (string-empty-p name)))
    (user-error "anvil-session-delete: NAME must be a non-empty string"))
  (anvil-state-delete name :ns anvil-session--snapshot-ns))


;;;; --- Phase 3: event log primitives --------------------------------------

(defun anvil-session--event-key (session-id ts)
  "Build a lexicographically-sortable key for SESSION-ID + TS.
The `us' suffix is integer microseconds so `anvil-state-list-keys'
returns rows in chronological order for a given session without
needing a separate index."
  (format "%s/%017d" session-id (truncate (* ts 1e6))))

(defun anvil-session--truncate-summary (s)
  "Clamp S to `anvil-session-event-summary-max-chars' (nil stays nil)."
  (cond
   ((null s) nil)
   ((not (stringp s)) (anvil-session--truncate-summary (format "%s" s)))
   ((> (length s) anvil-session-event-summary-max-chars)
    (substring s 0 anvil-session-event-summary-max-chars))
   (t s)))

;;;###autoload
(cl-defun anvil-session-log-event (session-id kind
                                              &key tool summary ts)
  "Append one event row to the Phase 3 session event log.

SESSION-ID is the Claude Code session identifier (hook env var
`CLAUDE_SESSION_ID' in practice).
KIND is a short symbol or string, e.g. `tool-use' / `user-prompt'
/ `error' / `decision'.

Keyword arguments:
  :tool      — optional tool name (for `tool-use' kind).
  :summary   — ≤200 chars digest of the event (enforced via
               `anvil-session-event-summary-max-chars').
  :ts        — override timestamp (defaults to `float-time').

Returns the stored row plist.  Rows live in anvil-state ns
`session-events' with TTL `anvil-session-events-ttl-sec'."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (user-error "anvil-session-log-event: SESSION-ID required"))
  (let* ((ts* (or ts (float-time)))
         (kind-str (cond ((stringp kind) kind)
                         ((symbolp kind) (symbol-name kind))
                         (t (format "%s" kind))))
         (row (list :session-id session-id
                    :ts ts*
                    :kind kind-str
                    :tool tool
                    :summary (anvil-session--truncate-summary summary)))
         (key (anvil-session--event-key session-id ts*)))
    (anvil-state-set key row
                     :ns anvil-session--events-ns
                     :ttl anvil-session-events-ttl-sec)
    row))

(defun anvil-session--all-events ()
  "Return every live event row, ordered by key (≈ chronological).
Used as the common pool for `--events-search' / `--events-recent'."
  (let (out)
    (dolist (k (sort (copy-sequence
                      (anvil-state-list-keys
                       :ns anvil-session--events-ns))
                     #'string-lessp))
      (let ((r (anvil-state-get k :ns anvil-session--events-ns)))
        (when r (push r out))))
    (nreverse out)))

(defun anvil-session--session-events (session-id)
  "Return live events for SESSION-ID, chronologically ordered."
  (let ((prefix (concat session-id "/"))
        out)
    (dolist (k (sort (copy-sequence
                      (anvil-state-list-keys
                       :ns anvil-session--events-ns))
                     #'string-lessp))
      (when (string-prefix-p prefix k)
        (let ((r (anvil-state-get k :ns anvil-session--events-ns)))
          (when r (push r out)))))
    (nreverse out)))

;;;###autoload
(cl-defun anvil-session-events-recent (&key session-id limit)
  "Return the last LIMIT events (default 20), newest last.
SESSION-ID scopes the result to one session; nil spans all sessions."
  (let* ((limit (or limit 20))
         (pool (if session-id
                   (anvil-session--session-events session-id)
                 (anvil-session--all-events))))
    (if (> (length pool) limit)
        (nthcdr (- (length pool) limit) pool)
      pool)))

;;;###autoload
(cl-defun anvil-session-events-search (query &key session-id limit)
  "Return events whose `:summary' or `:tool' contains QUERY (case-insensitive).
With SESSION-ID, restrict to that session; otherwise search all.
LIMIT caps the result size (default 20); oldest matches are dropped
first so the return is the newest LIMIT hits in chronological order.

This is the anvil analog of context-mode's PostToolUse / user-prompt
full-text search.  Doc 17 design used FTS5 but the anvil-state
simple-LIKE path is adequate for the ≤500-event/session volume
anticipated; swap in a dedicated FTS5 DB later if hot."
  (let* ((limit (or limit 20))
         (pool (if session-id
                   (anvil-session--session-events session-id)
                 (anvil-session--all-events)))
         (q (and (stringp query) (not (string-empty-p query))
                 (downcase query)))
         matches)
    (dolist (r pool)
      (let ((summary (plist-get r :summary))
            (tool    (plist-get r :tool)))
        (when (or (null q)
                  (and (stringp summary)
                       (string-match-p (regexp-quote q) (downcase summary)))
                  (and (stringp tool)
                       (string-match-p (regexp-quote q) (downcase tool))))
          (push r matches))))
    (setq matches (nreverse matches))
    (if (> (length matches) limit)
        (nthcdr (- (length matches) limit) matches)
      matches)))


;;;; --- Phase 3: Claude Code hook dispatch ---------------------------------

(defconst anvil-session--auto-snapshot-prefix "auto/pre-compact/"
  "Key prefix for snapshots created by the PreCompact hook.
SessionStart filters on this prefix + session-id to pick up the
freshest auto-capture of the previous session run.")

(defun anvil-session--auto-snapshot-name (session-id)
  "Return a unique snapshot NAME for SESSION-ID's PreCompact capture.
Shape: `auto/pre-compact/<session-id>/<ISO-ts>' — human readable in
`anvil-state-list-keys' output and sortable-by-recency."
  (format "%s%s/%s"
          anvil-session--auto-snapshot-prefix
          (or session-id "unknown")
          (format-time-string "%Y%m%dT%H%M%S")))

(defun anvil-session--most-recent-auto-snapshot (session-id)
  "Return the newest auto-snapshot plist for SESSION-ID, or nil."
  (let* ((prefix (format "%s%s/"
                         anvil-session--auto-snapshot-prefix
                         session-id))
         (names
          (cl-remove-if-not
           (lambda (k) (string-prefix-p prefix k))
           (anvil-state-list-keys :ns anvil-session--snapshot-ns)))
         (newest (car (sort names #'string-greaterp))))
    (and newest (anvil-session-resume newest))))

;;;###autoload
(defun anvil-session-hook-dispatch (event &rest args)
  "Route a Claude Code lifecycle EVENT to the right session primitive.

EVENT is a symbol or string:
  `pre-compact'    ARGS = (SESSION-ID &optional TASK-SUMMARY NOTES).
                   Creates an auto-snapshot under
                   `auto/pre-compact/SESSION-ID/ISO-TS' so the next
                   SessionStart can resume seamlessly.
  `session-start'  ARGS = (SESSION-ID).
                   Returns the `:preamble-suggested' of the newest
                   auto-snapshot for SESSION-ID, or an empty string.
                   The hook wrapper forwards this to stdout; the
                   caller pastes it into the new conversation.
  `post-tool-use'  ARGS = (SESSION-ID TOOL &optional SUMMARY).
                   Logs a `tool-use' event.
  `user-prompt'    ARGS = (SESSION-ID PROMPT-EXCERPT).
                   Logs a `user-prompt' event with PROMPT-EXCERPT
                   as the summary (truncated).
  `session-end'    ARGS = (SESSION-ID).
                   Logs a `session-end' event.

Unknown events return `(:error \"...\")' rather than signalling so
the shell wrapper does not crash the Claude hook pipeline."
  (let ((event* (cond ((symbolp event) event)
                      ((stringp event) (intern event))
                      (t event))))
    (pcase event*
      ('pre-compact
       (let* ((session-id (or (nth 0 args) "unknown"))
              (task-summary (nth 1 args))
              (notes (nth 2 args))
              (name (anvil-session--auto-snapshot-name session-id))
              (snap (anvil-session-snapshot name
                                            :task-summary task-summary
                                            :notes notes)))
         (when (fboundp 'anvil-compact-on-pre-compact)
           (funcall (intern "anvil-compact-on-pre-compact") session-id))
         snap))
      ('post-compact
       (let* ((session-id (or (nth 0 args) "unknown")))
         (if (fboundp 'anvil-compact-on-post-compact)
             (funcall (intern "anvil-compact-on-post-compact") session-id)
           (list :decision :compact-not-loaded))))
      ('stop
       (let* ((session-id (or (nth 0 args) "unknown"))
              (transcript-path (nth 1 args)))
         (if (fboundp 'anvil-compact-on-stop)
             (funcall (intern "anvil-compact-on-stop")
                      session-id
                      :transcript-path transcript-path)
           (list :decision :compact-not-loaded))))
      ('session-start
       (let* ((session-id (or (nth 0 args) "unknown"))
              (snap (anvil-session--most-recent-auto-snapshot session-id))
              (session-preamble (if snap
                                    (or (plist-get snap :preamble-suggested) "")
                                  ""))
              (compact-preamble
               (when (fboundp 'anvil-compact-on-session-start)
                 (funcall (intern "anvil-compact-on-session-start")
                          session-id))))
         (if (and (stringp compact-preamble)
                  (not (string-empty-p compact-preamble)))
             compact-preamble
           session-preamble)))
      ('post-tool-use
       (let* ((session-id (or (nth 0 args) "unknown"))
              (tool (nth 1 args))
              (summary (nth 2 args)))
         (anvil-session-log-event session-id 'tool-use
                                  :tool tool :summary summary)))
      ('user-prompt
       (let* ((session-id (or (nth 0 args) "unknown"))
              (prompt (nth 1 args)))
         (anvil-session-log-event session-id 'user-prompt
                                  :summary prompt)
         (let ((nudge (when (fboundp 'anvil-compact-on-user-prompt)
                        (funcall (intern "anvil-compact-on-user-prompt")
                                 session-id))))
           (when (and (stringp nudge) (not (string-empty-p nudge)))
             nudge))))
      ('session-end
       (let ((session-id (or (nth 0 args) "unknown")))
         (anvil-session-log-event session-id 'session-end
                                  :summary "session ended")))
      (_ (list :error
               (format "anvil-session-hook-dispatch: unknown event %S"
                       event))))))


;;;; --- Phase 4: install-settings command ----------------------------------

(defconst anvil-session--hook-events
  '(("PreCompact"       . "pre-compact")
    ("PostCompact"      . "post-compact")
    ("Stop"             . "stop")
    ("SessionStart"     . "session-start")
    ("PostToolUse"      . "post-tool-use")
    ("UserPromptSubmit" . "user-prompt")
    ("SessionEnd"       . "session-end"))
  "Alist of Claude Code hook names to their `anvil-hook' event
sub-commands.  The car is the JSON key written into
`~/.claude/settings.json'; the cdr is the first argv of the
wrapper script.")

(defun anvil-session--hook-command-for (event-cli script)
  "Return the command string bound to EVENT-CLI in Claude settings.
SCRIPT is the absolute path to `anvil-hook'.  Sub-commands like
`post-tool-use' forward Claude-provided env vars as trailing argv
so the dispatcher can extract session-id, tool name, and prompt
excerpt.  This is a plain string rather than an argv list because
Claude Code's hooks schema is shell-like."
  (pcase event-cli
    ("pre-compact"    (format "%s pre-compact $CLAUDE_SESSION_ID"
                              script))
    ("post-compact"   (format "%s post-compact $CLAUDE_SESSION_ID"
                              script))
    ("stop"           (format
                       "%s stop $CLAUDE_SESSION_ID $CLAUDE_TRANSCRIPT_PATH"
                       script))
    ("session-start"  (format "%s session-start $CLAUDE_SESSION_ID"
                              script))
    ("post-tool-use"  (format
                       "%s post-tool-use $CLAUDE_SESSION_ID $CLAUDE_TOOL_NAME"
                       script))
    ("user-prompt"    (format
                       "%s user-prompt $CLAUDE_SESSION_ID $CLAUDE_PROMPT"
                       script))
    ("session-end"    (format "%s session-end $CLAUDE_SESSION_ID"
                              script))
    (_ (error "anvil-session: unknown hook sub-command %S" event-cli))))

(defun anvil-session--read-settings (path)
  "Return the JSON object at PATH as a hash-table, or an empty one.
Missing file → empty table so the install path never aborts on a
fresh machine.  JSON parse errors signal — the caller promoted the
risk of corrupting the user's settings."
  (if (and (stringp path) (file-exists-p path))
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (json-parse-buffer :object-type 'hash-table
                           :array-type 'array
                           :null-object :null
                           :false-object :false))
    (make-hash-table :test 'equal)))

(defun anvil-session--write-settings (path settings)
  "Serialize SETTINGS (hash-table) to PATH with two-space indent.
Ensures the parent directory exists.  Overwrites atomically via
`write-region' so a mid-write crash does not leave the file
truncated."
  (let ((dir (file-name-directory path)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (with-temp-file path
    (insert (json-serialize settings
                            :false-object :false
                            :null-object :null))
    ;; Pretty-print — Claude users edit this file by hand.
    (goto-char (point-min))
    (when (fboundp 'json-pretty-print-buffer)
      (ignore-errors (json-pretty-print-buffer)))))

(defun anvil-session--settings-plan (existing-hooks script op)
  "Return `(NEW-HOOKS . DIFF)' describing OP applied to EXISTING-HOOKS.
OP is `install' or `uninstall'.  SCRIPT is the anvil-hook path.
DIFF is a list of (ACTION KEY VALUE) tuples for the dry-run
printout: ACTION ∈ {`add', `update', `remove', `keep'}."
  (let ((new (copy-hash-table existing-hooks))
        (diff nil))
    (pcase op
      ('install
       (dolist (pair anvil-session--hook-events)
         (let* ((claude-key (car pair))
                (sub-cmd (cdr pair))
                (desired (anvil-session--hook-command-for sub-cmd script))
                (existing (gethash claude-key existing-hooks)))
           (cond
            ((null existing)
             (puthash claude-key desired new)
             (push (list 'add claude-key desired) diff))
            ((equal existing desired)
             (push (list 'keep claude-key desired) diff))
            (t
             (puthash claude-key desired new)
             (push (list 'update claude-key desired) diff))))))
      ('uninstall
       (dolist (pair anvil-session--hook-events)
         (let* ((claude-key (car pair))
                (existing (gethash claude-key existing-hooks)))
           (when existing
             (remhash claude-key new)
             (push (list 'remove claude-key existing) diff))))))
    (cons new (nreverse diff))))

(defun anvil-session--format-diff (diff)
  "Render DIFF from `--settings-plan' as a human-readable string."
  (if (null diff)
      "(no changes)"
    (mapconcat
     (lambda (entry)
       (pcase (car entry)
         ('add    (format "+ %s: %s"    (nth 1 entry) (nth 2 entry)))
         ('update (format "~ %s: %s"    (nth 1 entry) (nth 2 entry)))
         ('remove (format "- %s (was: %s)" (nth 1 entry) (nth 2 entry)))
         ('keep   (format "= %s"        (nth 1 entry)))))
     diff "\n")))

;;;###autoload
(cl-defun anvil-hook-install-settings (&key dry-run uninstall path script)
  "Install the Doc 17 Claude Code hook bindings into settings JSON.

Keyword arguments:
  :dry-run   — preview the additions / updates, don't write.  Never
               mutates the file; always returns the diff string.
  :uninstall — remove the anvil hooks from the settings; non-anvil
               hooks are preserved.
  :path      — override `anvil-session-claude-settings-path'.
  :script    — override `anvil-session-hook-script'.

Returns a plist `(:path PATH :diff STR :applied BOOL)'.  `applied'
is nil when DRY-RUN is non-nil or the diff is empty.

The function is non-destructive: every JSON key outside `hooks.*'
stays verbatim, hooks we do not install (user's PreToolUse etc.)
stay verbatim, and UPDATE actions only fire when the existing
binding targets a different anvil-hook invocation."
  (let* ((path* (or path anvil-session-claude-settings-path))
         (script* (or script anvil-session-hook-script))
         (settings (anvil-session--read-settings path*))
         (hooks (or (gethash "hooks" settings)
                    (make-hash-table :test 'equal)))
         (plan (anvil-session--settings-plan
                hooks script* (if uninstall 'uninstall 'install)))
         (new-hooks (car plan))
         (diff (cdr plan))
         (diff-str (anvil-session--format-diff diff))
         (applied nil))
    (puthash "hooks" new-hooks settings)
    (unless (or dry-run (null diff))
      (anvil-session--write-settings path* settings)
      (setq applied t))
    (list :path path* :diff diff-str :applied applied)))


;;;; --- MCP tool wrappers --------------------------------------------------

(defun anvil-session--tool-snapshot (name &optional task-summary notes
                                          branch base-branch)
  "MCP wrapper for `anvil-session-snapshot'.

MCP Parameters:
  name          - Non-empty snapshot identifier (string).
  task-summary  - Optional human-readable \"what was I doing\".
  notes         - Optional list of bullet strings (JSON array); a
                  single string wraps to `(list …)' automatically.
  branch        - Optional override for autodetected git branch.
  base-branch   - Optional base branch (default
                  `anvil-session-default-base-branch')."
  (let ((notes-list
         (cond ((null notes) nil)
               ((stringp notes)
                (if (string-empty-p notes) nil (list notes)))
               ((vectorp notes) (append notes nil))
               ((listp notes) notes)
               (t (list (format "%s" notes))))))
    (anvil-session-snapshot
     name
     :branch (and (stringp branch) (not (string-empty-p branch)) branch)
     :base-branch (and (stringp base-branch)
                       (not (string-empty-p base-branch)) base-branch)
     :task-summary (and (stringp task-summary)
                        (not (string-empty-p task-summary)) task-summary)
     :notes notes-list)))

(defun anvil-session--tool-resume (name)
  "MCP wrapper for `anvil-session-resume'.

MCP Parameters:
  name - Snapshot identifier to fetch.  Returns nil when absent
         (MCP layer turns nil into a JSON null)."
  (or (anvil-session-resume name)
      (list :error (format "anvil-session: no snapshot named %S" name))))

(defun anvil-session--tool-list ()
  "MCP wrapper for `anvil-session-list'.  Returns a vector of
descriptor plists so the JSON encoder renders an array rather
than an object.

MCP Parameters: none."
  (apply #'vector (anvil-session-list)))

(defun anvil-session--tool-delete (name)
  "MCP wrapper for `anvil-session-delete'.

MCP Parameters:
  name - Snapshot identifier to purge.  Returns {:deleted t} on
         success, {:deleted :false} when the row did not exist."
  (list :deleted (if (anvil-session-delete name) t :false)
        :name name))

(defun anvil-session--tool-events-search (query &optional session-id limit)
  "MCP wrapper for `anvil-session-events-search'.

MCP Parameters:
  query      - Case-insensitive substring to match in `:summary'
               or `:tool'.  Empty / nil returns the LIMIT most
               recent events (see session-events-recent).
  session-id - Optional Claude session id to restrict the hunt.
  limit      - Optional cap on rows returned (default 20).  Oldest
               matches are dropped first."
  (apply #'vector
         (anvil-session-events-search
          query
          :session-id (and (stringp session-id)
                           (not (string-empty-p session-id))
                           session-id)
          :limit (cond ((integerp limit) limit)
                       ((and (stringp limit) (not (string-empty-p limit)))
                        (string-to-number limit))
                       (t nil)))))

(defun anvil-session--tool-events-recent (&optional session-id limit)
  "MCP wrapper for `anvil-session-events-recent'.

MCP Parameters:
  session-id - Optional session scope (see session-events-search).
  limit      - Optional cap (default 20)."
  (apply #'vector
         (anvil-session-events-recent
          :session-id (and (stringp session-id)
                           (not (string-empty-p session-id))
                           session-id)
          :limit (cond ((integerp limit) limit)
                       ((and (stringp limit) (not (string-empty-p limit)))
                        (string-to-number limit))
                       (t nil)))))


;;;; --- module lifecycle ---------------------------------------------------

(defun anvil-session--register-tools ()
  "Register every session-* MCP tool under the shared server-id."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-session--tool-snapshot)
   :id "session-snapshot"
   :intent '(session)
   :layer 'workflow
   :server-id anvil-session--server-id
   :description
   "Capture the current session state (branch + task-summary + notes)
into a named plist under anvil-state ns=session (TTL 14d).  Returns
the stored snapshot including `preamble-suggested' — a self-
contained resume block callers can drop into orchestrator-preamble
or an LLM re-entry prompt.  Write tool.")
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-session--tool-resume)
   :id "session-resume"
   :intent '(session)
   :layer 'workflow
   :server-id anvil-session--server-id
   :description
   "Return the stored snapshot plist for NAME, or an error envelope
when none exists.  Read-only — does not mutate running state."
   :read-only t)
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-session--tool-list)
   :id "session-list"
   :intent '(session)
   :layer 'workflow
   :server-id anvil-session--server-id
   :description
   "Return a JSON array of descriptor plists for every live snapshot
(name + created-at + branch + task-summary-head, head capped at 80
chars).  Use session-resume for the full payload."
   :read-only t)
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-session--tool-delete)
   :id "session-delete"
   :intent '(session admin)
   :layer 'workflow
   :server-id anvil-session--server-id
   :description
   "Purge the snapshot stored under NAME.  Returns {deleted: bool,
name}.  Write tool.")
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-session--tool-events-search)
   :id "session-events-search"
   :intent '(session)
   :layer 'workflow
   :server-id anvil-session--server-id
   :description
   "Case-insensitive substring hunt across the event log's `summary'
+ `tool' fields.  Use for \"which tool did I use last time I hit the
worker retry-cap bug?\".  Scopes to SESSION-ID when given; LIMIT caps
the result (default 20, oldest matches dropped first).  Read-only."
   :read-only t)
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-session--tool-events-recent)
   :id "session-events-recent"
   :intent '(session)
   :layer 'workflow
   :server-id anvil-session--server-id
   :description
   "Return the last LIMIT events (default 20), newest last.  Optional
SESSION-ID narrows to a single Claude session.  Cheaper than
session-events-search when the caller just wants recent activity.
Read-only."
   :read-only t))

(defun anvil-session--unregister-tools ()
  "Remove every session-* MCP tool from the shared server."
  (dolist (id '("session-snapshot" "session-resume"
                "session-list"     "session-delete"
                "session-events-search" "session-events-recent"))
    (ignore-errors
      (anvil-server-unregister-tool id anvil-session--server-id))))

;;;###autoload
(defun anvil-session-enable ()
  "Register session-* MCP tools and open the anvil-state backing store."
  (interactive)
  (anvil-state-enable)
  (anvil-session--register-tools))

(defun anvil-session-disable ()
  "Unregister session-* MCP tools."
  (interactive)
  (anvil-session--unregister-tools))


(provide 'anvil-session)
;;; anvil-session.el ends here
