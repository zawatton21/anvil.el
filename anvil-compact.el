;;; anvil-compact.el --- Autonomous /compact orchestration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  zawatton

;; Author: zawatton
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1") (anvil-server "0.1") (anvil-state "0.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; anvil-compact provides an aggressive auto-`/compact' orchestration
;; layer on top of Claude Code's hook system (Doc 36 Phase 1).
;;
;; Claude Code 2026-04 runs its built-in auto-compact at a hardcoded
;; ~83.5% context usage.  For autonomous / long-running development
;; sessions that is too late — the last ~30% is consumed at premium
;; tokens/turn before compaction kicks in.  This module adds a
;; user-configurable trigger (default 45%) that fires at a segment
;; boundary (end-of-turn + no in-progress task) via the Stop hook,
;; parks a snapshot of working state in `anvil-state', and tells the
;; next user-prompt turn to invoke `/compact' itself — since hooks
;; cannot trigger slash commands directly, the model must.
;;
;; Since emitting `additionalContext' requires structured JSON output
;; from the hook, this module provides the JSON-formatting helpers so
;; the `scripts/anvil-hook' wrapper can just forward whatever this
;; module returns.
;;
;; Minimal wiring:
;;   - Stop hook           → `anvil-compact-on-stop'
;;   - UserPromptSubmit    → `anvil-compact-on-user-prompt'
;;   - SessionStart        → `anvil-compact-on-session-start'
;;
;; State lives in `anvil-state' under namespace `compact'.  Keys are
;; scoped by SESSION-ID so multiple concurrent Claude Code sessions do
;; not collide.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'anvil-server)
(require 'anvil-state)

(defgroup anvil-compact nil
  "Autonomous `/compact' orchestration for Claude Code sessions."
  :group 'anvil
  :prefix "anvil-compact-")

(defcustom anvil-compact-trigger-percent 45
  "Context-usage percent at which auto-compact becomes eligible.
Below this threshold the Stop hook returns silently.  The default
45 is aggressive compared with the harness's hardcoded ~83.5%
auto-compact; it targets long autonomous development sessions
where staying below 50% keeps per-turn token cost predictable.
Raise for looser (closer-to-harness) behaviour, lower for even
more aggressive compaction (risk: snapshot overhead dominates)."
  :type 'integer
  :group 'anvil-compact)

(defcustom anvil-compact-cooldown-percent 25
  "Minimum percent growth between consecutive auto-compact triggers.
Prevents thrashing right after a compact: if the previous compact
landed at `last-compact-percent', the next trigger requires the
current percent to exceed `last-compact-percent +
anvil-compact-cooldown-percent'.  With defaults (45% trigger, 25%
cooldown) a normal cycle is: compact at ~45% → settle at ~15%
after compact → grow to ~40% (15 + 25) before another trigger
can fire."
  :type 'integer
  :group 'anvil-compact)

(defcustom anvil-compact-context-tokens-max 200000
  "Assumed context window size in tokens for percent calculation.
Claude Sonnet 4.6 / Opus 4.7 = 200K by default.  The 1M-context
variants should set this to 1000000."
  :type 'integer
  :group 'anvil-compact)

(defcustom anvil-compact-bytes-per-token 4.0
  "Approximate bytes-per-token used to estimate transcript tokens.
The JSONL transcript is mostly ASCII + JSON overhead; 4 bytes per
token is a conservative first-order estimate.  Real token counts
vary by ~20%; callers who need exact counts should swap in a
tokenizer in Phase 2.  Float so users can tune per-locale
(Japanese-heavy prompts may want 2.5)."
  :type 'number
  :group 'anvil-compact)

(defcustom anvil-compact-snapshot-include-files t
  "When non-nil, `anvil-compact-snapshot-capture' records recently
touched file paths in the snapshot so post-compact restore can
hint the continuation."
  :type 'boolean
  :group 'anvil-compact)

(defcustom anvil-compact-snapshot-include-todos t
  "When non-nil, `anvil-compact-snapshot-capture' records the task
list summary in the snapshot."
  :type 'boolean
  :group 'anvil-compact)

(defcustom anvil-compact-include-event-log t
  "When non-nil, snapshots embed the most recent anvil-session
events for the session (loads only when `anvil-session' is
available — falls back to nil otherwise, non-fatal).  Recent
events help Claude reconstruct `what tools I just ran' context
after /compact wipes the detailed history."
  :type 'boolean
  :group 'anvil-compact)

(defcustom anvil-compact-event-log-limit 10
  "Maximum number of recent events to embed in a snapshot."
  :type 'integer
  :group 'anvil-compact)

(defcustom anvil-compact-semantic-summary-enabled nil
  "When non-nil, snapshots also embed an LLM-generated 1-2
paragraph summary of the session via
`anvil-orchestrator-submit-and-collect'.  Opt-in because each
summary costs an orchestrator round-trip; leave nil until the
plain-text snapshot proves insufficient in practice.  Doc 36
Phase 2 ships the defcustom + hook points but the actual
orchestrator call is reserved for a follow-up so Phase 2 stays
self-contained."
  :type 'boolean
  :group 'anvil-compact)

(defconst anvil-compact--server-id "emacs-eval"
  "Server ID for the compact-* MCP tools.")

(defconst anvil-compact--state-ns "compact"
  "`anvil-state' namespace for compact state (flags + snapshots).")


;;;; --- state helpers -------------------------------------------------------

(defun anvil-compact--state-key (session-id kind)
  "Return the `anvil-state' key for KIND under SESSION-ID.
SESSION-ID defaults to \"unknown\" when nil or empty so state is
never silently discarded on a malformed hook invocation."
  (let ((sid (if (and session-id (stringp session-id)
                      (not (string-empty-p session-id)))
                 session-id
               "unknown")))
    (format "%s/%s" sid kind)))

(defun anvil-compact--state-put (session-id kind value)
  "Store VALUE at the state key for SESSION-ID + KIND.
Values are `prin1'-serialised by `anvil-state' so arbitrary Lisp
structures (plists / alists / strings) round-trip cleanly."
  (anvil-state-set (anvil-compact--state-key session-id kind) value
                   :ns anvil-compact--state-ns))

(defun anvil-compact--state-get (session-id kind &optional default)
  "Fetch the stored value for SESSION-ID + KIND, or DEFAULT.
`anvil-state' returns DEFAULT when the key is absent or expired."
  (anvil-state-get (anvil-compact--state-key session-id kind)
                   :ns anvil-compact--state-ns
                   :default default))

(defun anvil-compact--state-clear-flag (session-id)
  "Clear the pending-nudge flag for SESSION-ID (idempotent)."
  (anvil-state-delete (anvil-compact--state-key session-id "pending-nudge")
                      :ns anvil-compact--state-ns))

(defun anvil-compact--queue-push (session-id value)
  "Append VALUE to the restore-queue for SESSION-ID.
The queue is FIFO — `--queue-pop' returns the oldest item first.
Idempotent with respect to existing queue contents."
  (let ((q (anvil-compact--state-get session-id "restore-queue" '())))
    (anvil-compact--state-put session-id "restore-queue"
                              (append q (list value)))))

(defun anvil-compact--queue-pop (session-id)
  "Pop and return the oldest value from the restore-queue for
SESSION-ID, or nil when the queue is empty / absent.  Deletes the
queue key when the last element is removed."
  (let ((q (anvil-compact--state-get session-id "restore-queue")))
    (when (and q (listp q) (not (null q)))
      (let ((head (car q))
            (rest (cdr q)))
        (if rest
            (anvil-compact--state-put session-id "restore-queue" rest)
          (anvil-state-delete
           (anvil-compact--state-key session-id "restore-queue")
           :ns anvil-compact--state-ns))
        head))))

(defun anvil-compact--queue-length (session-id)
  "Return the current restore-queue length for SESSION-ID."
  (let ((q (anvil-compact--state-get session-id "restore-queue" '())))
    (if (listp q) (length q) 0)))


;;;; --- token / percent estimation ------------------------------------------

(defun anvil-compact--valid-path-p (path)
  "Return non-nil when PATH is a non-empty string pointing at a regular file.
Guards against empty strings (which `file-readable-p' treats as
the current directory) and directories (which also pass
`file-readable-p' but must not be read as transcripts)."
  (and path (stringp path)
       (not (string-empty-p path))
       (file-regular-p path)))

(defun anvil-compact--file-size-bytes (path)
  "Return the byte size of PATH, or 0 when PATH is not a regular file.
Never signals — missing transcript degrades to zero tokens so the
trigger decision returns :below-threshold rather than erroring out."
  (if (anvil-compact--valid-path-p path)
      (or (file-attribute-size (file-attributes path)) 0)
    0))

(defun anvil-compact--count-turns (path)
  "Return the number of JSONL records in PATH (one per line).
Cheap line-count approximation of turn count.  Returns 0 when PATH
is not a regular file.  Does not parse the JSON; the transcript
schema varies between Claude Code versions and a line count is a
stable first-order signal of session length."
  (if (anvil-compact--valid-path-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (count-lines (point-min) (point-max)))
    0))

(cl-defun anvil-compact-estimate (&key transcript-path
                                       bytes-per-token
                                       context-max)
  "Estimate context usage from TRANSCRIPT-PATH.
Returns a plist `(:bytes BYTES :tokens TOKENS :percent PERCENT
:turns TURNS)'.  BYTES-PER-TOKEN defaults to
`anvil-compact-bytes-per-token'.  CONTEXT-MAX defaults to
`anvil-compact-context-tokens-max'.  PERCENT is an integer 0..100
rounded down; it is never negative and is clamped at 100 so
callers can compare against thresholds without further clean-up.
Missing / unreadable TRANSCRIPT-PATH yields
`(:bytes 0 :tokens 0 :percent 0 :turns 0)'."
  (let* ((bpt    (or bytes-per-token anvil-compact-bytes-per-token))
         (cmax   (or context-max     anvil-compact-context-tokens-max))
         (bytes  (anvil-compact--file-size-bytes transcript-path))
         (tokens (if (and (numberp bpt) (> bpt 0))
                     (truncate (/ (float bytes) bpt))
                   0))
         (turns  (anvil-compact--count-turns transcript-path))
         (pct    (if (and (numberp cmax) (> cmax 0))
                     (min 100 (max 0 (truncate (* 100.0
                                                  (/ (float tokens)
                                                     cmax)))))
                   0)))
    (list :bytes bytes :tokens tokens :percent pct :turns turns)))


;;;; --- trigger decision ----------------------------------------------------

(cl-defun anvil-compact-should-trigger (&key percent
                                             task-in-progress
                                             last-compact-percent
                                             trigger-at
                                             cooldown)
  "Decide whether a new compact cycle is eligible.
PERCENT is the current context-usage estimate.
TASK-IN-PROGRESS is the number of in-progress tasks; compact is
suppressed when this is non-zero so mid-reasoning state is not
lost.
LAST-COMPACT-PERCENT is the percent at which the last compact
fired; defaults to 0 when absent (= no prior compact).
TRIGGER-AT defaults to `anvil-compact-trigger-percent'.
COOLDOWN defaults to `anvil-compact-cooldown-percent'.

Returns a plist `(:trigger BOOL :reason SYM :percent P)'.  REASON
is one of `:below-threshold', `:in-progress', `:cooldown',
`:trigger'.  Callers should key on `:trigger' for the binary
decision and log REASON for observability."
  (let* ((pct      (or percent 0))
         (inprog   (or task-in-progress 0))
         (last     (or last-compact-percent 0))
         (thresh   (or trigger-at anvil-compact-trigger-percent))
         (cool     (or cooldown anvil-compact-cooldown-percent))
         (reason
          (cond
           ((< pct thresh)                         :below-threshold)
           ((> inprog 0)                           :in-progress)
           ((< (- pct last) cool)                  :cooldown)
           (t                                      :trigger))))
    (list :trigger (eq reason :trigger)
          :reason  reason
          :percent pct)))


;;;; --- snapshot ------------------------------------------------------------

(defun anvil-compact--collect-events (session-id explicit)
  "Return the event list to embed in a snapshot for SESSION-ID.
EXPLICIT, when non-nil, is used verbatim (caller supplied).
Otherwise, when `anvil-compact-include-event-log' is non-nil and
`anvil-session-events-recent' is available, the most recent
events (up to `anvil-compact-event-log-limit') are returned.
Degrades to nil whenever anvil-session is absent — the caller
treats nil as `no event log' without raising."
  (cond
   (explicit explicit)
   ((and anvil-compact-include-event-log
         (fboundp 'anvil-session-events-recent))
    (condition-case nil
        (funcall (intern "anvil-session-events-recent")
                 :session-id session-id
                 :limit anvil-compact-event-log-limit)
      (error nil)))
   (t nil)))

(cl-defun anvil-compact-snapshot-capture (session-id
                                          &key
                                          task-summary
                                          files
                                          todos
                                          branch
                                          percent
                                          events)
  "Capture a snapshot plist for SESSION-ID and store it in state.
TASK-SUMMARY is a short free-text string summarising the current
logical work unit — this is what the post-compact continuation
hint will lean on, so callers should provide one when possible.
FILES is a list of file-path strings touched recently.
TODOS is a free-form string or list describing pending tasks.
BRANCH is the current git branch.
PERCENT is the measured context percent at snapshot time.
EVENTS, when supplied, is the explicit event list to embed.
When nil, Phase 2 auto-collects the latest anvil-session events
if `anvil-compact-include-event-log' is on and the session
module is loaded.

Respects `anvil-compact-snapshot-include-files' and
`anvil-compact-snapshot-include-todos' — when those are nil the
corresponding fields are dropped to minimise snapshot size.

Returns the stored snapshot plist."
  (let* ((collected (anvil-compact--collect-events session-id events))
         (snap
          (list :captured-at (format-time-string "%Y-%m-%dT%H:%M:%S%z")
                :percent      (or percent 0)
                :task-summary (or task-summary "")
                :branch       (or branch "")
                :files        (when anvil-compact-snapshot-include-files
                                (or files '()))
                :todos        (when anvil-compact-snapshot-include-todos
                                (or todos ""))
                :events       (when anvil-compact-include-event-log
                                collected))))
    (anvil-compact--state-put session-id "snapshot" snap)
    snap))

(defun anvil-compact-snapshot-get (session-id)
  "Fetch the latest snapshot plist for SESSION-ID, or nil."
  (anvil-compact--state-get session-id "snapshot"))

(defun anvil-compact-snapshot-format (snap)
  "Render SNAP as a human-readable continuation string.
The returned string is designed to be pasted into the model's
context directly — it reads as an `[anvil-compact restore]'
preamble that the model can use to pick up where /compact left
off.  Missing / empty fields are skipped rather than rendered as
blank labels."
  (when (and snap (listp snap))
    (let* ((ts     (plist-get snap :captured-at))
           (pct    (plist-get snap :percent))
           (task   (plist-get snap :task-summary))
           (branch (plist-get snap :branch))
           (files  (plist-get snap :files))
           (todos  (plist-get snap :todos))
           parts)
      (push (format "[anvil-compact restore @ %s, %d%% used]"
                    (or ts "?") (or pct 0))
            parts)
      (when (and (stringp task) (not (string-empty-p task)))
        (push (format "  task: %s" task) parts))
      (when (and (stringp branch) (not (string-empty-p branch)))
        (push (format "  branch: %s" branch) parts))
      (when (and files (listp files))
        (push (format "  files: %s"
                      (mapconcat #'identity files ", "))
              parts))
      (when (and todos
                 (or (and (stringp todos) (not (string-empty-p todos)))
                     (and (listp todos) todos)))
        (push (format "  todos: %s"
                      (if (listp todos)
                          (mapconcat (lambda (x) (format "%s" x)) todos "; ")
                        todos))
              parts))
      (let ((events (plist-get snap :events)))
        (when (and events (listp events))
          (push (format "  recent events (%d):" (length events)) parts)
          (dolist (e events)
            (let ((kind (and (listp e) (plist-get e :kind)))
                  (summary (and (listp e) (plist-get e :summary))))
              (push (format "    - %s: %s"
                            (or kind "?")
                            (or summary ""))
                    parts)))))
      (mapconcat #'identity (nreverse parts) "\n"))))


;;;; --- additionalContext JSON output ---------------------------------------

(defun anvil-compact--json-additional-context (event body)
  "Return a JSON string for Claude Code `additionalContext' output.
EVENT is the Claude hook-event name (string, e.g.
\"UserPromptSubmit\" or \"SessionStart\").  BODY is the text to
inject.  The result is a single-line JSON object matching Claude
Code 2026-04's hook-output contract."
  (json-encode
   `((hookSpecificOutput
      .
      ((hookEventName . ,event)
       (additionalContext . ,body))))))


;;;; --- hook entries --------------------------------------------------------

(defun anvil-compact-on-pre-compact (session-id &rest _args)
  "PreCompact-hook entry point — refresh the parked snapshot.
Runs just before the harness executes `/compact'.  Re-times the
existing Stop-time snapshot so the post-compact restore carries
the most recent captured-at + any event-log updates since Stop.
Returns the refreshed snapshot plist or nil when no snapshot is
parked."
  (let ((existing (anvil-compact-snapshot-get session-id)))
    (when existing
      (anvil-compact-snapshot-capture
       session-id
       :task-summary (plist-get existing :task-summary)
       :branch (plist-get existing :branch)
       :files (plist-get existing :files)
       :todos (plist-get existing :todos)
       :percent (plist-get existing :percent)))))

(defun anvil-compact-on-post-compact (session-id)
  "PostCompact-hook entry point — enqueue snapshot for next turn.
Also records `last-compact-percent' and `last-compact-time' so
`anvil-compact-should-trigger' can honour cooldown against the
just-completed compact cycle.  Returns the enqueued snapshot or
nil when no snapshot was parked (which would be unusual but not
an error — e.g. the harness auto-compact ran without a prior
anvil-compact trigger)."
  (let ((snap (anvil-compact-snapshot-get session-id)))
    (when snap
      (anvil-compact--queue-push session-id snap)
      (anvil-compact--state-put session-id "last-compact-percent"
                                (or (plist-get snap :percent) 0))
      (anvil-compact--state-put session-id "last-compact-time"
                                (float-time))
      snap)))

(cl-defun anvil-compact-on-stop (session-id
                                 &key
                                 transcript-path
                                 task-in-progress
                                 task-summary
                                 branch
                                 files
                                 todos)
  "Stop-hook entry point — decide + flag + snapshot.
Called when Claude Code finishes a reply.  Reads transcript size,
runs `anvil-compact-should-trigger', and if the decision is
`:trigger', sets a `pending-nudge' flag and captures a snapshot
so the next UserPromptSubmit can emit the /compact nudge.

Returns a plist `(:decision DECISION :percent P)' for logging;
the Stop hook itself does not need to emit additional context."
  (let* ((est     (anvil-compact-estimate
                   :transcript-path transcript-path))
         (pct     (plist-get est :percent))
         (last    (or (anvil-compact--state-get
                       session-id "last-compact-percent")
                      0))
         (dec     (anvil-compact-should-trigger
                   :percent pct
                   :task-in-progress task-in-progress
                   :last-compact-percent last)))
    (when (plist-get dec :trigger)
      (anvil-compact--state-put session-id "pending-nudge" t)
      (anvil-compact-snapshot-capture session-id
                                      :task-summary task-summary
                                      :files files
                                      :todos todos
                                      :branch branch
                                      :percent pct))
    (list :decision (plist-get dec :reason)
          :percent  pct)))

(defun anvil-compact-on-user-prompt (session-id)
  "UserPromptSubmit-hook entry point — emit nudge or queued restore.
Priority (highest first):

1. If the `pending-nudge' flag is set, emit the /compact nudge
   (Phase 1 behaviour): cleared idempotently, JSON additionalContext
   tells the model to invoke `/compact'.
2. Else if the restore-queue has a pending item (PostCompact put it
   there), pop it and emit as restore preamble so the model sees
   the pre-compact state even though /compact wiped history.
3. Else return empty string (hook caller forwards verbatim)."
  (let ((flag (anvil-compact--state-get session-id "pending-nudge")))
    (cond
     (flag
      (anvil-compact--state-clear-flag session-id)
      (let* ((snap (anvil-compact-snapshot-get session-id))
             (preamble (or (anvil-compact-snapshot-format snap)
                           "[anvil-compact restore]"))
             (pct (if (listp snap) (or (plist-get snap :percent) 0) 0))
             (body
              (format (concat "[auto-compact] context usage ~%d%% — "
                              "run `/compact 続行: %s' now to stay "
                              "under budget.  The snapshot above "
                              "has already been parked in "
                              "anvil-state; continue with that "
                              "context after compaction.")
                      pct
                      (or (and (stringp (plist-get snap :task-summary))
                               (plist-get snap :task-summary))
                          "現タスク"))))
        (anvil-compact--json-additional-context
         "UserPromptSubmit"
         (concat preamble "\n\n" body))))
     (t
      (let ((queued (anvil-compact--queue-pop session-id)))
        (if (not queued)
            ""
          (let ((preamble (or (anvil-compact-snapshot-format queued)
                              "[anvil-compact restore]")))
            (if (string-empty-p preamble)
                ""
              (anvil-compact--json-additional-context
               "UserPromptSubmit" preamble)))))))))

(defun anvil-compact-on-session-start (session-id)
  "SessionStart-hook entry point — emit parked snapshot as preamble.
Prefers a queued restore (PostCompact-enqueued) over the plain
latest snapshot so cross-compact continuation wins when both are
present.  Returns JSON additionalContext or an empty string."
  (let* ((queued (anvil-compact--queue-pop session-id))
         (snap   (or queued (anvil-compact-snapshot-get session-id))))
    (if (not snap)
        ""
      (let ((preamble (anvil-compact-snapshot-format snap)))
        (if (or (null preamble) (string-empty-p preamble))
            ""
          (anvil-compact--json-additional-context
           "SessionStart" preamble))))))


;;;; --- MCP tools -----------------------------------------------------------

(defun anvil-compact--tool-estimate (transcript_path)
  "Estimate context usage from TRANSCRIPT_PATH.

MCP Parameters:
  transcript_path - absolute path to the Claude Code JSONL
                    transcript.  Empty string returns zero usage."
  (anvil-server-with-error-handling
    (let ((path (and (stringp transcript_path)
                     (not (string-empty-p transcript_path))
                     transcript_path)))
      (anvil-compact-estimate :transcript-path path))))

(defun anvil-compact--tool-should-trigger (percent task_in_progress
                                                   last_compact_percent)
  "Apply the trigger decision for PERCENT + TASK_IN_PROGRESS.

MCP Parameters:
  percent                - current context-usage percent (0..100)
  task_in_progress       - count of currently in-progress tasks
  last_compact_percent   - percent at which the last compact fired
                           (pass 0 when there has been no prior
                           compact in this session)."
  (anvil-server-with-error-handling
    (let ((pct  (if (numberp percent) percent
                  (and (stringp percent) (string-to-number percent))))
          (inp  (if (numberp task_in_progress) task_in_progress
                  (and (stringp task_in_progress)
                       (string-to-number task_in_progress))))
          (last (if (numberp last_compact_percent) last_compact_percent
                  (and (stringp last_compact_percent)
                       (string-to-number last_compact_percent)))))
      (anvil-compact-should-trigger
       :percent pct
       :task-in-progress inp
       :last-compact-percent last))))

(defun anvil-compact--tool-snapshot (session_id task_summary)
  "Capture a snapshot for SESSION_ID with TASK_SUMMARY.

MCP Parameters:
  session_id    - Claude Code session identifier
  task_summary  - short free-text summary of current work.  Empty
                  string is accepted (no summary recorded)."
  (anvil-server-with-error-handling
    (anvil-compact-snapshot-capture session_id
                                    :task-summary task_summary)))

(defun anvil-compact--tool-restore (session_id)
  "Return the rendered snapshot preamble for SESSION_ID.

MCP Parameters:
  session_id - Claude Code session identifier"
  (anvil-server-with-error-handling
    (or (anvil-compact-snapshot-format
         (anvil-compact-snapshot-get session_id))
        "")))

(defun anvil-compact--tool-hook (session_id stage transcript_path)
  "Unified hook entry: dispatch STAGE for SESSION_ID.

MCP Parameters:
  session_id      - Claude Code session identifier
  stage           - one of \"stop\", \"user-prompt\",
                    \"session-start\", \"pre-compact\",
                    \"post-compact\".  Other values return the
                    empty string.
  transcript_path - absolute path to the JSONL transcript (only
                    consulted for stage=\"stop\"; ignored
                    otherwise)."
  (anvil-server-with-error-handling
    (pcase stage
      ("stop"           (prin1-to-string
                         (anvil-compact-on-stop
                          session_id
                          :transcript-path transcript_path)))
      ("user-prompt"    (anvil-compact-on-user-prompt session_id))
      ("session-start"  (anvil-compact-on-session-start session_id))
      ("pre-compact"    (prin1-to-string
                         (anvil-compact-on-pre-compact session_id)))
      ("post-compact"   (prin1-to-string
                         (anvil-compact-on-post-compact session_id)))
      (_ ""))))


;;;; --- module lifecycle ----------------------------------------------------

;;;###autoload
(defun anvil-compact-enable ()
  "Register the compact-* MCP tools and ensure state backend is up."
  (anvil-state-enable)
  (anvil-server-register-tool
   #'anvil-compact--tool-estimate
   :id "compact-estimate"
   :intent '(session compact observe)
   :layer 'core
   :server-id anvil-compact--server-id
   :description
   "Estimate Claude Code context usage from a JSONL transcript
path.  Returns :bytes, :tokens, :percent, :turns as a plist; used
by the Stop hook to decide whether an auto-compact cycle is
eligible."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-compact--tool-should-trigger
   :id "compact-should-trigger"
   :intent '(session compact decide)
   :layer 'core
   :server-id anvil-compact--server-id
   :description
   "Apply the auto-compact trigger decision given current percent,
in-progress task count, and last-compact percent.  Pure function;
does not read or write state."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-compact--tool-snapshot
   :id "compact-snapshot"
   :intent '(session compact snapshot)
   :layer 'core
   :server-id anvil-compact--server-id
   :description
   "Capture a session snapshot (task-summary, branch, files, todos)
into anvil-state ns=compact so the post-compact restore hint can
continue the work across a /compact boundary."
   :read-only nil)
  (anvil-server-register-tool
   #'anvil-compact--tool-restore
   :id "compact-restore"
   :intent '(session compact restore)
   :layer 'core
   :server-id anvil-compact--server-id
   :description
   "Return the rendered snapshot preamble for SESSION_ID, suitable
for injection into additionalContext at SessionStart or
PostCompact.  Empty string when no snapshot is parked."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-compact--tool-hook
   :id "compact-hook"
   :intent '(session compact hook)
   :layer 'core
   :server-id anvil-compact--server-id
   :description
   "Unified hook dispatcher for the three Claude Code events
relevant to auto-compact (stop / user-prompt / session-start).
Hook wrappers (scripts/anvil-hook) can forward any of these
events here instead of wiring each one individually."
   :read-only nil))

;;;###autoload
(defun anvil-compact-disable ()
  "Unregister the compact-* MCP tools."
  (anvil-server-unregister-tool "compact-estimate"
                                anvil-compact--server-id)
  (anvil-server-unregister-tool "compact-should-trigger"
                                anvil-compact--server-id)
  (anvil-server-unregister-tool "compact-snapshot"
                                anvil-compact--server-id)
  (anvil-server-unregister-tool "compact-restore"
                                anvil-compact--server-id)
  (anvil-server-unregister-tool "compact-hook"
                                anvil-compact--server-id))

(provide 'anvil-compact)
;;; anvil-compact.el ends here
