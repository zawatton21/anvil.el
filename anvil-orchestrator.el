;;; anvil-orchestrator.el --- Parallel AI CLI dispatcher for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Multi-provider AI CLI orchestrator — spawn several agentic CLI
;; subprocesses (claude, aider, gemini, …) in parallel and expose a
;; terse submit / status / collect API so the parent Claude session
;; only consumes task IDs and final summaries, not intermediate
;; tool-call traffic.
;;
;; Design doc: docs/design/10-orchestrator.org.
;;
;; Phase 1a scope (this file):
;;   * Provider abstraction (cl-defstruct) + built-in `claude' provider
;;   * `anvil-orchestrator-submit' / `-status' / `-collect' / `-cancel'
;;     / `-retry' Elisp API + matching MCP tools (server-id emacs-eval)
;;   * Pool with global + per-provider concurrency caps, FIFO queue,
;;     wall-clock timeout enforcement
;;   * stdout / stderr redirected to per-task disk files
;;   * Parsing of `claude --output-format stream-json' summary + usage
;;     + total_cost_usd
;;   * State persistence via `anvil-state' ns="orchestrator" — queued /
;;     running tasks survive daemon restart (running tasks are re-marked
;;     as failed with a "daemon restart" error on reload)
;;   * Tabulated-list dashboard (`M-x anvil-orchestrator-dashboard')
;;
;; Phase 1b+ (not in this file): git worktree isolation, `:depends-on'
;; DAG, stdout overflow head/tail slicing, aider / gemini / ollama
;; providers, anvil-cron integration.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'tabulated-list)
(require 'anvil-server)
(require 'anvil-state)
(require 'anvil-git)

;;;; --- configuration ------------------------------------------------------

(defgroup anvil-orchestrator nil
  "Parallel AI CLI dispatcher for anvil."
  :group 'anvil
  :prefix "anvil-orchestrator-")

(defconst anvil-orchestrator--server-id "emacs-eval"
  "MCP server-id orchestrator tools register under.")

(defconst anvil-orchestrator--state-ns "orchestrator"
  "anvil-state namespace used for persisted task records.")

(defcustom anvil-orchestrator-concurrency 3
  "Global cap on simultaneously running tasks.
Chosen to stay well under Claude MAX plan's 5-hour window when
combined with the default timeout and batch size.  Raise
explicitly (`let' or `setq') for large nightly batches."
  :type 'integer
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-per-provider-concurrency
  '((claude . 3) (gemini . 4) (ollama . 2) (aider . 2) (codex . 3))
  "Per-provider concurrency cap alist.
Providers not listed fall back to `anvil-orchestrator-concurrency'."
  :type '(alist :key-type symbol :value-type integer)
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-timeout-sec-default 600
  "Default per-task wall-clock cap in seconds.
Tasks running longer are killed and marked `failed' with a
`timeout' error.  Override per task with `:timeout-sec'."
  :type 'integer
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-batch-max-tasks 20
  "Maximum number of tasks accepted in a single `submit' call.
A protective ceiling so a stray programmatic caller cannot
accidentally queue thousands of tasks."
  :type 'integer
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-budget-usd-default 1.00
  "Per-task backstop budget in USD.
Passed through to providers that support `--max-budget-usd'
(Claude CLI).  Subscription users may see the limit ignored;
it still serves as a programmatic sanity check against the
batch total."
  :type 'number
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-batch-budget-usd-total 10.00
  "Hard cap on the sum of per-task budgets inside one `submit' call.
Submits exceeding this value are refused with `user-error'."
  :type 'number
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-work-dir
  (expand-file-name "anvil-orchestrator" user-emacs-directory)
  "Parent directory for per-task stdout / stderr files.
Created on demand.  Kept out of the repo so transient scratch
output never lands in git status."
  :type 'directory
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-system-prompt-append
  "You are running as a sub-task inside anvil-orchestrator.
Complete the requested work with the minimum number of tool calls.
Produce a short final summary (<= 300 chars) of what you did and
the git SHA of any commit you made, then stop."
  "Default --append-system-prompt injected into every sub-task.
Individual tasks may override via `:system-prompt-append'."
  :type 'string
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-auto-retry-on '(429 500 502 503 504 network)
  "HTTP status codes (or the symbol `network') that trigger an auto-retry.
Parsed from the failed task's stderr / stream-json result when
available.  Other failures must be retried manually via
`anvil-orchestrator-retry'."
  :type '(repeat (choice integer symbol))
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-auto-retry-max 2
  "Maximum number of automatic retries per task."
  :type 'integer
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-auto-retry-base-ms 200
  "Base delay (ms) for the exponential-backoff schedule.
Actual delay = base-ms * 2^retry-count, capped by
`anvil-orchestrator-auto-retry-max-delay-ms' and optionally jittered by
`anvil-orchestrator-auto-retry-jitter-pct'."
  :type 'integer
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-auto-retry-max-delay-ms 30000
  "Upper bound (ms) on any single retry delay.
Applied before jitter so the post-jitter value may slightly exceed this."
  :type 'integer
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-auto-retry-jitter-pct 25
  "Full-jitter percentage applied to each computed retry delay.
Final delay is uniformly sampled from [raw*(1-pct/100), raw*(1+pct/100)].
Set to 0 to disable jitter.
Clamped internally to [0, 100]."
  :type 'integer
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-per-provider-backoff
  nil
  "Optional per-provider override of the backoff schedule.
Alist of PROVIDER → plist with any of keys :base-ms :max-delay-ms :jitter-pct.
Missing keys fall back to the global `anvil-orchestrator-auto-retry-*' defcustoms.
Providers not listed use the globals entirely."
  :type '(alist :key-type symbol :value-type plist)
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-poll-interval-sec 1.0
  "Timer period for the pool pump + timeout scan (seconds)."
  :type 'number
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-summary-max-chars 4000
  "Maximum length of the summary kept on each task record.
The original 300-char ceiling forced callers to re-read
`:stdout-path' every time they needed the real agent message;
4000 is roughly the upper tail of a single-turn AI answer in
practice so `orchestrator-extract-result' can return the text
directly in the common case without a follow-up file read."
  :type 'integer
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-permission-mode "acceptEdits"
  "Default `--permission-mode' passed to providers supporting it.
Set to \"plan\" for dry-run, \"bypassPermissions\" for fully
autonomous (use with care)."
  :type 'string
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-worktree-auto t
  "When non-nil, new tasks whose `:cwd' is inside a git repository
get a dedicated worktree — either via the provider's native flag
(claude's `--worktree') or via an `anvil-git worktree add' under
`anvil-orchestrator-work-dir' when the provider lacks native
support.  Override per task with `:no-worktree t'."
  :type 'boolean
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-output-size-cap (* 1024 1024)
  "Per-task stdout / stderr byte ceiling before head + tail truncation."
  :type 'integer
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-output-head-bytes (* 256 1024)
  "Head bytes kept intact when an output file exceeds the cap."
  :type 'integer
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-output-tail-bytes (* 256 1024)
  "Tail bytes kept intact when an output file exceeds the cap."
  :type 'integer
  :group 'anvil-orchestrator)

;;;; --- provider abstraction ----------------------------------------------

(cl-defstruct anvil-orchestrator-provider
  "Descriptor for a backend AI CLI provider."
  id                       ; symbol, e.g. 'claude
  cli                      ; string, binary name for `executable-find'
  version-check            ; (lambda () t-or-error-string)
  build-cmd                ; (lambda (task) (list CLI ARG ARG ...)) — full command
  parse-output             ; (lambda (stdout-path stderr-path exit-code) plist)
  supports-tool-use
  supports-worktree
  supports-budget
  supports-system-prompt-append
  default-model
  cost-estimator)          ; (lambda (prompt) usd-float) — pre-submit estimate

(defvar anvil-orchestrator--providers (make-hash-table :test 'eq)
  "Symbol → `anvil-orchestrator-provider' descriptor.")

;;;###autoload
(defun anvil-orchestrator-register-provider (id &rest props)
  "Register a provider descriptor under ID.
PROPS is a plist of slot keyword → value matching
`anvil-orchestrator-provider' slots (without the `:' on symbols
that are already keywords)."
  (puthash id
           (apply #'make-anvil-orchestrator-provider :id id props)
           anvil-orchestrator--providers)
  id)

(defun anvil-orchestrator--provider (id)
  "Return the registered provider for ID, or signal `user-error'."
  (or (gethash id anvil-orchestrator--providers)
      (user-error "anvil-orchestrator: unknown provider %S (registered: %S)"
                  id (hash-table-keys anvil-orchestrator--providers))))

;;;; --- provider-common helpers -------------------------------------------

(defconst anvil-orchestrator--retry-http-re
  "\\b\\(429\\|500\\|502\\|503\\|504\\)\\b"
  "Regex matching retryable HTTP status codes on stderr.
First capture group is the numeric code.")

(defconst anvil-orchestrator--retry-network-re
  "\\b\\(network\\|ECONNRESET\\|ETIMEDOUT\\|EAI_AGAIN\\)\\b"
  "Regex matching generic network-failure keywords on stderr.")

(defun anvil-orchestrator--stderr-retry-code (stderr-path exit-code)
  "Scan STDERR-PATH (first 8 KiB) and return a retry-code symbol or nil.

Returns an integer for HTTP status matches (429 / 5xx), the
symbol `network' for generic network errors, or nil when no known
retryable pattern is present.  Reads nothing when EXIT-CODE is
zero or the path is not readable, so callers can unconditionally
plug this into their parse-output epilogue."
  (when (and (integerp exit-code)
             (not (zerop exit-code))
             (stringp stderr-path)
             (file-readable-p stderr-path))
    (with-temp-buffer
      (insert-file-contents stderr-path nil 0 8192)
      (goto-char (point-min))
      (cond
       ((re-search-forward anvil-orchestrator--retry-http-re nil t)
        (string-to-number (match-string 1)))
       ((progn (goto-char (point-min))
               (re-search-forward anvil-orchestrator--retry-network-re
                                  nil t))
        'network)))))

(defun anvil-orchestrator--stderr-retry-after-ms (stderr-path exit-code)
  "Return retry delay from STDERR-PATH as integer milliseconds, or nil.
Reads STDERR-PATH only when EXIT-CODE is a non-zero integer and the file is
readable; otherwise reads nothing and returns nil.  Scans the first 8192 bytes
for a Retry-After hint and returns the delay in milliseconds, clamped to the
range [0, 600000]."
  (when (and (integerp exit-code)
             (not (zerop exit-code))
             (file-readable-p stderr-path))
    (with-temp-buffer
      (insert-file-contents stderr-path nil 0 8192)
      (let ((case-fold-search t))
        (cond
         ((re-search-forward "[Rr]etry-[Aa]fter[ :]+\\([0-9]+\\)" nil t)
          (max 0 (min 600000 (* 1000 (string-to-number (match-string 1))))))
         ((progn
            (goto-char (point-min))
            (re-search-forward "\"retry_after\"[ :]*\\([0-9]+\\)" nil t))
          (max 0 (min 600000 (* 1000 (string-to-number (match-string 1)))))))))))

(defun anvil-orchestrator--truncate-summary (summary)
  "Trim SUMMARY and clamp it to `anvil-orchestrator-summary-max-chars'.
Appends an ellipsis when truncation happened.  Returns nil when
SUMMARY is nil or an empty string so callers can treat the result
as the canonical `:summary' slot value."
  (when (and (stringp summary) (not (string-empty-p summary)))
    (let ((trimmed (string-trim summary)))
      (cond
       ((string-empty-p trimmed) nil)
       ((> (length trimmed) anvil-orchestrator-summary-max-chars)
        (concat (substring trimmed 0 anvil-orchestrator-summary-max-chars)
                "…"))
       (t trimmed)))))

(defun anvil-orchestrator--argv-append-when (cmd test &rest items)
  "Return CMD with ITEMS appended iff TEST is non-nil.
Collapses the `(setq cmd (append cmd (list ...)))' pattern that
every provider build-cmd repeats.  Does not mutate CMD."
  (if test (append cmd items) cmd))

;;;; --- claude provider (built-in) -----------------------------------------

(defconst anvil-orchestrator--claude-price-table
  '(("opus"   . (:input 0.000015 :output 0.000075 :cache-read 0.0000015))
    ("sonnet" . (:input 0.000003 :output 0.000015 :cache-read 0.0000003))
    ("haiku"  . (:input 0.0000008 :output 0.000004 :cache-read 0.00000008)))
  "Static per-token USD price table for fallback cost estimation.
Values are approximate 2026-04 published rates; override by
supplying a different `cost-estimator' at provider registration.")

(defun anvil-orchestrator--claude-model-prices (model)
  "Return (:input :output :cache-read) prices per token for MODEL."
  (let ((key (cond
              ((null model) "sonnet")
              ((string-match-p "opus"   model) "opus")
              ((string-match-p "haiku"  model) "haiku")
              (t                                "sonnet"))))
    (cdr (assoc key anvil-orchestrator--claude-price-table))))

(defun anvil-orchestrator--claude-cost (prompt &optional model)
  "Rough pre-submit USD estimate for PROMPT under MODEL."
  (let* ((prices (anvil-orchestrator--claude-model-prices model))
         (in-tok (/ (length (or prompt "")) 4.0))
         ;; assume 2× response length; crude but conservative
         (out-tok (* 2.0 in-tok)))
    (+ (* in-tok  (or (plist-get prices :input)  0.000003))
       (* out-tok (or (plist-get prices :output) 0.000015)))))

(defun anvil-orchestrator--claude-check ()
  "Return t when the `claude' CLI is on `exec-path'."
  (if (executable-find "claude") t
    (error "anvil-orchestrator: `claude' CLI not found on exec-path")))

(defun anvil-orchestrator--claude-build-cmd (task)
  "Build the `claude' command line for TASK plist.
Includes streaming JSON output, budget, append-system-prompt,
permission mode, optional native --worktree, and finally the
prompt.  The native worktree flag is emitted when the task has
`:_worktree-name' (set by `--spawn' when `--should-worktree'
decides the task should run in an isolated worktree and the
provider natively supports it)."
  (let* ((model    (or (plist-get task :model) "sonnet"))
         (prompt   (plist-get task :prompt))
         (budget   (or (plist-get task :budget-usd)
                       anvil-orchestrator-budget-usd-default))
         (append-p (or (plist-get task :system-prompt-append)
                       anvil-orchestrator-system-prompt-append))
         (perm     (or (plist-get task :permission-mode)
                       anvil-orchestrator-permission-mode))
         (allowed  (plist-get task :allowed-tools))
         (bare     (plist-get task :bare))
         (wt-name  (plist-get task :_worktree-name))
         (cmd      (list (executable-find "claude")
                         "--print"
                         "--output-format" "stream-json"
                         "--verbose"
                         "--model" model
                         "--permission-mode" perm
                         "--max-budget-usd" (format "%s" budget))))
    (setq cmd (anvil-orchestrator--argv-append-when
               cmd append-p "--append-system-prompt" append-p))
    (setq cmd (anvil-orchestrator--argv-append-when
               cmd allowed  "--allowedTools" allowed))
    (setq cmd (anvil-orchestrator--argv-append-when
               cmd bare     "--bare"))
    (setq cmd (anvil-orchestrator--argv-append-when
               cmd wt-name  "--worktree" wt-name))
    ;; Prompt is the positional argument; CLI treats remaining argv as prompt.
    (append cmd (list prompt))))

(defun anvil-orchestrator--claude-parse-output (stdout-path stderr-path exit-code)
  "Parse STDOUT-PATH (stream-json NDJSON) into a summary plist.
Returns (:summary STR :cost-usd FLOAT :cost-tokens PLIST
:commit-sha NIL :auto-retry-code SYM) — missing fields are nil."
  (let (summary cost-usd tokens)
    (condition-case _err
        (with-temp-buffer
          (insert-file-contents stdout-path)
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((line (buffer-substring-no-properties
                          (point) (line-end-position)))
                   (obj  (condition-case _ (json-parse-string
                                            line
                                            :object-type 'plist
                                            :array-type  'list
                                            :null-object nil
                                            :false-object nil)
                           (error nil))))
              (when (and obj (equal (plist-get obj :type) "result"))
                (let* ((res   (plist-get obj :result))
                       (usage (plist-get obj :usage))
                       (cost  (plist-get obj :total_cost_usd)))
                  (when res        (setq summary res))
                  (when (numberp cost) (setq cost-usd cost))
                  (when usage
                    (setq tokens
                          (list :input       (plist-get usage :input_tokens)
                                :output      (plist-get usage :output_tokens)
                                :cache-read  (plist-get usage :cache_read_input_tokens)))))))
            (forward-line 1)))
      (error nil))
    (list :summary         (anvil-orchestrator--truncate-summary summary)
          :cost-usd        cost-usd
          :cost-tokens     tokens
          :commit-sha      nil
          :auto-retry-code (and (not summary)
                                (anvil-orchestrator--stderr-retry-code
                                 stderr-path exit-code)))))

(anvil-orchestrator-register-provider
 'claude
 :cli "claude"
 :version-check #'anvil-orchestrator--claude-check
 :build-cmd     #'anvil-orchestrator--claude-build-cmd
 :parse-output  #'anvil-orchestrator--claude-parse-output
 :supports-tool-use             t
 :supports-worktree             t
 :supports-budget               t
 :supports-system-prompt-append t
 :default-model                 "sonnet"
 :cost-estimator (lambda (task) (anvil-orchestrator--claude-cost
                                 (plist-get task :prompt)
                                 (plist-get task :model))))

;;;; --- aider provider (Phase 2, built-in) --------------------------------

(defcustom anvil-orchestrator-aider-default-model "openai/gpt-4o-mini"
  "Default `aider --model' value when a task omits :model.
Format is `<vendor>/<model>', e.g. `openai/gpt-4o',
`anthropic/claude-3-5-sonnet', `gemini/gemini-2.0-flash'."
  :type 'string
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-aider-extra-args nil
  "Extra argv appended verbatim to every `aider' invocation.
Useful for persistent flags like `--no-check-update' or
`--read .aider.conf.yml' that are already non-default here."
  :type '(repeat string)
  :group 'anvil-orchestrator)

(defun anvil-orchestrator--aider-check ()
  "Return t when the `aider' CLI is on `exec-path'."
  (if (executable-find "aider") t
    (error "anvil-orchestrator: `aider' CLI not found on exec-path")))

(defun anvil-orchestrator--aider-cost (task)
  "Rough pre-submit USD estimate for TASK under the aider model.
Uses a coarse per-vendor price table.  Returns 0 for local
providers (ollama / any model matching `local')."
  (let* ((model  (or (plist-get task :model)
                     anvil-orchestrator-aider-default-model))
         (prompt (or (plist-get task :prompt) ""))
         (in-tok (/ (length prompt) 4.0))
         (out-tok (* 2.0 in-tok))
         (prices
          (cond
           ((string-match-p "gpt-4o-mini"         model) '(:i 0.00000015 :o 0.0000006))
           ((string-match-p "gpt-4o"              model) '(:i 0.0000025  :o 0.00001))
           ((string-match-p "\\`o[13]\\|o1-mini"  model) '(:i 0.0000025  :o 0.00001))
           ((string-match-p "claude-3-5-haiku"    model) '(:i 0.0000008  :o 0.000004))
           ((string-match-p "claude-3-5-sonnet"   model) '(:i 0.000003   :o 0.000015))
           ((string-match-p "claude-3-opus"       model) '(:i 0.000015   :o 0.000075))
           ((string-match-p "gemini-2\\.0-flash"  model) '(:i 0.0000001  :o 0.0000004))
           ((string-match-p "gemini"              model) '(:i 0.00000035 :o 0.00000105))
           ((string-match-p "ollama/\\|/local"    model) '(:i 0          :o 0))
           (t                                            '(:i 0.000003   :o 0.000015)))))
    (+ (* in-tok  (plist-get prices :i))
       (* out-tok (plist-get prices :o)))))

(defun anvil-orchestrator--aider-build-cmd (task)
  "Build the `aider' command line for TASK plist.
Uses `--message' for single-shot non-interactive invocation,
`--yes-always' to skip prompts, `--no-stream' for deterministic
stdout, and `--no-check-update' to avoid network pings.  Files
under `:files' become positional args; `:read-only-files' become
`--read FILE' pairs; `:subtree-only', `:no-auto-commits',
`:no-pretty', `:no-gitignore' are honoured.  Extra argv from
`anvil-orchestrator-aider-extra-args' is appended after the
flags and before any positional file list."
  (let* ((model     (or (plist-get task :model)
                        anvil-orchestrator-aider-default-model))
         (prompt    (plist-get task :prompt))
         (files     (plist-get task :files))
         (read-only (plist-get task :read-only-files))
         (no-auto   (plist-get task :no-auto-commits))
         (subtree   (plist-get task :subtree-only))
         (cmd       (list (executable-find "aider")
                          "--model" model
                          "--message" prompt
                          "--yes-always"
                          "--no-stream"
                          "--no-check-update"
                          "--no-show-release-notes"
                          "--no-analytics"
                          "--no-pretty")))
    (setq cmd (anvil-orchestrator--argv-append-when
               cmd no-auto "--no-auto-commits"))
    (setq cmd (anvil-orchestrator--argv-append-when
               cmd subtree "--subtree-only"))
    (dolist (ro read-only)
      (setq cmd (anvil-orchestrator--argv-append-when cmd ro "--read" ro)))
    (setq cmd (if anvil-orchestrator-aider-extra-args
                  (append cmd anvil-orchestrator-aider-extra-args)
                cmd))
    (append cmd files)))

(defconst anvil-orchestrator--aider-skip-prefix-re
  (concat "\\`\\(?:"
          "[+-]\\{1,3\\}\\| \\|@@\\|"
          "diff \\|index \\|---\\|\\+\\+\\+\\|"
          "\\$ \\|>>>>>>>\\|<<<<<<<\\|=======\\|"
          "Applied edit\\|Added \\|Dropped \\|Files\\|Repo\\|"
          "Main model\\|Weak model\\|Editor model\\|"
          "Tokens:\\|Cost:\\|Commit \\|"
          "Aider v\\|Model: \\|Git repo:\\|Using \\|"
          "Scanning repo\\|Repo-map:"
          "\\)")
  "Prefixes identifying non-summary aider output lines.
Used by `anvil-orchestrator--aider-parse-output' to drop
setup / diff / stat noise when extracting the summary tail.")

(defun anvil-orchestrator--aider-parse-output (stdout-path stderr-path exit-code)
  "Parse aider STDOUT-PATH into a summary plist.
Extracts the last commit SHA (from `^Commit [0-9a-f]+ '), takes
the tail non-diff paragraph as the summary, and heuristically
maps stderr HTTP / network errors to auto-retry codes."
  (let (summary commit-sha)
    (condition-case _err
        (when (file-readable-p stdout-path)
          (with-temp-buffer
            (insert-file-contents stdout-path)
            (goto-char (point-min))
            (while (re-search-forward
                    "^Commit \\([0-9a-f]\\{7,40\\}\\)\\b" nil t)
              (setq commit-sha (match-string 1)))
            (let* ((sz (buffer-size))
                   (tail-start (max (point-min) (- (point-max)
                                                   (min sz 4096))))
                   (lines nil))
              (goto-char tail-start)
              (while (not (eobp))
                (let ((line (buffer-substring-no-properties
                             (point) (line-end-position))))
                  (unless (or (string-empty-p (string-trim line))
                              (string-match-p
                               anvil-orchestrator--aider-skip-prefix-re
                               line))
                    (push line lines)))
                (forward-line 1))
              (when lines
                (setq summary (mapconcat #'identity
                                         (nreverse lines) "\n"))))))
      (error nil))
    (list :summary         (anvil-orchestrator--truncate-summary summary)
          :cost-usd        nil
          :cost-tokens     nil
          :commit-sha      commit-sha
          :auto-retry-code (and (not summary)
                                (anvil-orchestrator--stderr-retry-code
                                 stderr-path exit-code)))))

(anvil-orchestrator-register-provider
 'aider
 :cli "aider"
 :version-check #'anvil-orchestrator--aider-check
 :build-cmd     #'anvil-orchestrator--aider-build-cmd
 :parse-output  #'anvil-orchestrator--aider-parse-output
 :supports-tool-use             t
 :supports-worktree             nil
 :supports-budget               nil
 :supports-system-prompt-append nil
 :default-model                 "openai/gpt-4o-mini"
 :cost-estimator #'anvil-orchestrator--aider-cost)

;;;; --- gemini provider (Phase 3, built-in) -------------------------------

(defcustom anvil-orchestrator-gemini-default-model "gemini-2.5-flash"
  "Default `gemini --model' value when a task omits :model.
Google gemini-cli model names: `gemini-2.5-pro', `gemini-2.5-flash',
`gemini-2.0-flash'."
  :type 'string
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-gemini-extra-args nil
  "Extra argv appended verbatim to every `gemini' invocation."
  :type '(repeat string)
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-gemini-yolo nil
  "When non-nil, append `--yolo' to auto-accept tool invocations.
Equivalent to raising claude's permission mode to `bypassPermissions'.
Off by default — enable explicitly when dispatching tool-using tasks."
  :type 'boolean
  :group 'anvil-orchestrator)

(defun anvil-orchestrator--gemini-check ()
  "Return t when the `gemini' CLI is on `exec-path'."
  (if (executable-find "gemini") t
    (error "anvil-orchestrator: `gemini' CLI not found on exec-path")))

(defun anvil-orchestrator--gemini-cost (task)
  "Rough pre-submit USD estimate for TASK under the gemini model.
Gemini pricing is per-1M-tokens and much cheaper than Claude / GPT-4.
Falls back to a flash-tier rate for unrecognised model strings."
  (let* ((model  (or (plist-get task :model)
                     anvil-orchestrator-gemini-default-model))
         (prompt (or (plist-get task :prompt) ""))
         (in-tok (/ (length prompt) 4.0))
         (out-tok (* 2.0 in-tok))
         (prices
          (cond
           ((string-match-p "2\\.5-pro"   model) '(:i 0.00000125 :o 0.00001))
           ((string-match-p "2\\.5-flash" model) '(:i 0.0000003  :o 0.0000025))
           ((string-match-p "2\\.0-flash" model) '(:i 0.0000001  :o 0.0000004))
           ((string-match-p "1\\.5-pro"   model) '(:i 0.00000125 :o 0.000005))
           ((string-match-p "1\\.5-flash" model) '(:i 0.000000075 :o 0.0000003))
           (t                                    '(:i 0.0000003  :o 0.0000025)))))
    (+ (* in-tok  (plist-get prices :i))
       (* out-tok (plist-get prices :o)))))

(defun anvil-orchestrator--gemini-build-cmd (task)
  "Build the `gemini' command line for TASK plist.
Uses `-p PROMPT' for single-shot non-interactive invocation and
`-m MODEL' to select the model.  When
`anvil-orchestrator-gemini-yolo' (or TASK `:yolo') is non-nil,
`--yolo' is appended to auto-accept tool invocations.  Extra argv
from `anvil-orchestrator-gemini-extra-args' follows."
  (let* ((model   (or (plist-get task :model)
                      anvil-orchestrator-gemini-default-model))
         (prompt  (plist-get task :prompt))
         (yolo    (or (plist-get task :yolo)
                      anvil-orchestrator-gemini-yolo))
         (cmd     (list (executable-find "gemini")
                        "-m" model
                        "-p" prompt)))
    (setq cmd (anvil-orchestrator--argv-append-when
               cmd yolo "--yolo"))
    (if anvil-orchestrator-gemini-extra-args
        (append cmd anvil-orchestrator-gemini-extra-args)
      cmd)))

(defconst anvil-orchestrator--gemini-skip-prefix-re
  (concat "\\`\\(?:"
          "Loaded cached credentials\\|"
          "Data collection is\\|"
          "Using model:\\|"
          "Authenticating\\|"
          "\\[DEBUG\\]\\|\\[INFO\\]"
          "\\)")
  "Prefixes identifying non-content gemini CLI log lines.
Stripped by `anvil-orchestrator--gemini-parse-output' when
building the tail summary.")

(defun anvil-orchestrator--gemini-parse-output (stdout-path stderr-path exit-code)
  "Parse gemini STDOUT-PATH plain text into a summary plist.
Returns (:summary STR :cost-usd NIL :cost-tokens NIL
:commit-sha NIL :auto-retry-code SYM).  The gemini CLI emits
plain markdown; we take the tail up to ~4 KB, dropping empty
lines and a small set of known log prefixes."
  (let (summary)
    (condition-case _err
        (when (file-readable-p stdout-path)
          (with-temp-buffer
            (insert-file-contents stdout-path)
            (let* ((sz (buffer-size))
                   (tail-start (max (point-min) (- (point-max)
                                                   (min sz 4096))))
                   (lines nil))
              (goto-char tail-start)
              (while (not (eobp))
                (let ((line (buffer-substring-no-properties
                             (point) (line-end-position))))
                  (unless (or (string-empty-p (string-trim line))
                              (string-match-p
                               anvil-orchestrator--gemini-skip-prefix-re
                               line))
                    (push line lines)))
                (forward-line 1))
              (when lines
                (setq summary (mapconcat #'identity
                                         (nreverse lines) "\n"))))))
      (error nil))
    (list :summary         (anvil-orchestrator--truncate-summary summary)
          :cost-usd        nil
          :cost-tokens     nil
          :commit-sha      nil
          :auto-retry-code (and (not summary)
                                (anvil-orchestrator--stderr-retry-code
                                 stderr-path exit-code)))))

(anvil-orchestrator-register-provider
 'gemini
 :cli "gemini"
 :version-check #'anvil-orchestrator--gemini-check
 :build-cmd     #'anvil-orchestrator--gemini-build-cmd
 :parse-output  #'anvil-orchestrator--gemini-parse-output
 :supports-tool-use             t
 :supports-worktree             nil
 :supports-budget               nil
 :supports-system-prompt-append nil
 :default-model                 "gemini-2.5-flash"
 :cost-estimator #'anvil-orchestrator--gemini-cost)

;;;; --- ollama provider (Phase 3b, built-in) ------------------------------

(defcustom anvil-orchestrator-ollama-default-model "llama3.2"
  "Default `ollama run' model name when a task omits :model.
Must reference a model already pulled via `ollama pull MODEL'."
  :type 'string
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-ollama-extra-args nil
  "Extra argv appended verbatim to every `ollama run' invocation.
Useful for flags like `--keepalive 5m' or `--verbose'."
  :type '(repeat string)
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-ollama-strip-thinking t
  "When non-nil, strip `<think>...</think>' blocks (case-insensitive, multi-line) from ollama stdout before building the task summary.  Set to nil to pass thinking output through verbatim."
  :type 'boolean
  :group 'anvil-orchestrator)

(defun anvil-orchestrator--ollama-check ()
  "Return t when the `ollama' CLI is on `exec-path'."
  (if (executable-find "ollama") t
    (error "anvil-orchestrator: `ollama' CLI not found on exec-path")))

(defun anvil-orchestrator--ollama-cost (_task)
  "Return 0 — Ollama runs locally, no per-token billing."
  0.0)

(defun anvil-orchestrator--ollama-build-cmd (task)
  "Build the `ollama run' command line for TASK plist.
Uses `ollama run MODEL PROMPT' for single-shot non-interactive
invocation.  Extra argv from
`anvil-orchestrator-ollama-extra-args' follows after positional
args."
  (let* ((model   (or (plist-get task :model)
                      anvil-orchestrator-ollama-default-model))
         (prompt  (plist-get task :prompt))
         (cmd     (list (executable-find "ollama")
                        "run" model prompt)))
    (if anvil-orchestrator-ollama-extra-args
        (append cmd anvil-orchestrator-ollama-extra-args)
      cmd)))

(defconst anvil-orchestrator--ollama-skip-prefix-re
  (concat "\\`\\(?:"
          "pulling manifest\\|"
          "pulling \\|"
          "verifying\\|"
          "writing manifest\\|"
          "success\\|"
          "total duration:\\|"
          "load duration:\\|"
          "prompt eval count:\\|"
          "prompt eval duration:\\|"
          "prompt eval rate:\\|"
          "eval count:\\|"
          "eval duration:\\|"
          "eval rate:"
          "\\)")
  "Prefixes identifying non-content ollama CLI log / stats lines.
Stripped by `anvil-orchestrator--ollama-parse-output' when
building the tail summary.")

(defun anvil-orchestrator--ollama-strip-thinking-block (text)
  "Strip ollama thinking-mode preamble blocks from TEXT.

When `anvil-orchestrator-ollama-strip-thinking' is non-nil, remove:
  (a) `<think>...</think>' XML-style blocks (some reasoning models); and
  (b) `Thinking...' ... `...done thinking.' plain-text blocks (gemma4
      on ollama emits this rather than XML tags).

Both patterns match case-insensitive and multi-line.  After stripping,
the result is trimmed via `string-trim'.  Returns TEXT unchanged when
TEXT is nil or stripping is disabled.  Returns nil when the stripped
and trimmed result is empty."
  (if (or (null text)
          (not anvil-orchestrator-ollama-strip-thinking))
      text
    (let* ((case-fold-search t)
           (stripped
            (replace-regexp-in-string
             (concat
              "<think>\\(?:.\\|\n\\)*?</think>"
              "\\|"
              "Thinking\\.\\.\\.\\(?:.\\|\n\\)*?\\.\\.\\.done thinking\\.")
             ""
             text)))
      (let ((trimmed (string-trim stripped)))
        (unless (string-empty-p trimmed)
          trimmed)))))

(defun anvil-orchestrator--ollama-parse-output (stdout-path stderr-path exit-code)
  "Parse ollama STDOUT-PATH plain text into a summary plist.
Returns (:summary STR :cost-usd NIL :cost-tokens NIL
:commit-sha NIL :auto-retry-code SYM).  Strips `ollama run
--verbose' eval stats footer + any pull-progress prefixes."
  (let (summary)
    (condition-case _err
        (when (file-readable-p stdout-path)
          (with-temp-buffer
            (insert-file-contents stdout-path)
            (let* ((sz (buffer-size))
                   (tail-start (max (point-min) (- (point-max)
                                                   (min sz 4096))))
                   (lines nil))
              (goto-char tail-start)
              (while (not (eobp))
                (let ((line (buffer-substring-no-properties
                             (point) (line-end-position))))
                  (unless (or (string-empty-p (string-trim line))
                              (string-match-p
                               anvil-orchestrator--ollama-skip-prefix-re
                               line))
                    (push line lines)))
                (forward-line 1))
              (when lines
                (setq summary (mapconcat #'identity
                                         (nreverse lines) "\n"))))))
      (error nil))
    (list :summary         (anvil-orchestrator--truncate-summary
                            (anvil-orchestrator--ollama-strip-thinking-block summary))
          :cost-usd        nil
          :cost-tokens     nil
          :commit-sha      nil
          :auto-retry-code (and (not summary)
                                (anvil-orchestrator--stderr-retry-code
                                 stderr-path exit-code)))))

(anvil-orchestrator-register-provider
 'ollama
 :cli "ollama"
 :version-check #'anvil-orchestrator--ollama-check
 :build-cmd     #'anvil-orchestrator--ollama-build-cmd
 :parse-output  #'anvil-orchestrator--ollama-parse-output
 :supports-tool-use             nil
 :supports-worktree             nil
 :supports-budget               nil
 :supports-system-prompt-append nil
 :default-model                 "llama3.2"
 :cost-estimator #'anvil-orchestrator--ollama-cost)

;;;; --- codex native provider ---------------------------------------------

(defcustom anvil-orchestrator-codex-default-model "gpt-5.4"
  "Default `-m MODEL' value when a task omits `:model'.
The codex CLI's own default (as of 0.121) is also \"gpt-5.4\".
Set to \"gpt-5-codex\" for code-heavy tasks."
  :type 'string
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-codex-extra-args nil
  "Global argv appended to every `codex exec' invocation.
Useful for e.g. `-c \"model_reasoning_effort=high\"' overrides.
Must not include `--sandbox' or `--json' as those are already
passed by the build-cmd."
  :type '(repeat string)
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-codex-sandbox "read-only"
  "Default sandbox mode for `codex exec' tasks.
Orchestrator submissions are short single-shot Q&A or
non-mutating code review, so read-only is the safe default.
Override per task with `:sandbox' in the task plist."
  :type '(choice (const "read-only")
                 (const "workspace-write")
                 (const "danger-full-access"))
  :group 'anvil-orchestrator)

(defun anvil-orchestrator--codex-check ()
  "Return non-nil when the codex CLI is on PATH."
  (and (executable-find "codex") t))

(defun anvil-orchestrator--codex-cost (_task)
  "Codex via ChatGPT Plus OAuth has no per-token billing."
  0.0)

(defun anvil-orchestrator--codex-build-cmd (task)
  "Build the `codex exec' command line for TASK plist.
Uses `--json' (JSONL events) so parse-output picks
`agent_message.text' off each completed turn.  Accepts
`:sandbox' on the task plist to override the default."
  (let* ((model   (or (plist-get task :model)
                      anvil-orchestrator-codex-default-model))
         (prompt  (plist-get task :prompt))
         (sandbox (or (plist-get task :sandbox)
                      anvil-orchestrator-codex-sandbox))
         (cmd     (list (executable-find "codex")
                        "exec"
                        "--skip-git-repo-check"
                        "--sandbox" sandbox
                        "--json"
                        "-m" model)))
    (if anvil-orchestrator-codex-extra-args
        (append cmd anvil-orchestrator-codex-extra-args
                (list prompt))
      (append cmd (list prompt)))))

(defun anvil-orchestrator--codex-parse-output (stdout-path stderr-path exit-code)
  "Parse codex STDOUT-PATH JSONL into a summary plist.
Scans every line, parses each as JSON, and keeps the last
`agent_message' text produced by `item.completed' events.
`turn.completed' carries the token usage object.  Silent on
malformed lines so a partial stream does not poison the summary."
  (let (summary tokens)
    (condition-case _err
        (when (file-readable-p stdout-path)
          (with-temp-buffer
            (insert-file-contents stdout-path)
            (goto-char (point-min))
            (while (not (eobp))
              (let* ((line (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))
                     (j (and (not (string-empty-p line))
                             (ignore-errors
                               (json-parse-string
                                line
                                :object-type 'plist
                                :null-object nil
                                :false-object nil)))))
                (cond
                 ((and j (equal "item.completed" (plist-get j :type)))
                  (let* ((item  (plist-get j :item))
                         (itype (and item (plist-get item :type)))
                         (text  (and item (plist-get item :text))))
                    (when (and (equal "agent_message" itype)
                               (stringp text))
                      (setq summary text))))
                 ((and j (equal "turn.completed" (plist-get j :type)))
                  (setq tokens (plist-get j :usage)))))
              (forward-line 1))))
      (error nil))
    (list :summary         (anvil-orchestrator--truncate-summary summary)
          :cost-usd        nil
          :cost-tokens     tokens
          :commit-sha      nil
          :auto-retry-code (and (not summary)
                                (anvil-orchestrator--stderr-retry-code
                                 stderr-path exit-code)))))

(anvil-orchestrator-register-provider
 'codex
 :cli "codex"
 :version-check #'anvil-orchestrator--codex-check
 :build-cmd     #'anvil-orchestrator--codex-build-cmd
 :parse-output  #'anvil-orchestrator--codex-parse-output
 :supports-tool-use             t
 :supports-worktree             nil
 :supports-budget               nil
 :supports-system-prompt-append nil
 :default-model                 "gpt-5.4"
 :cost-estimator #'anvil-orchestrator--codex-cost)

;;;; --- UUID + internal state ---------------------------------------------

(defun anvil-orchestrator--uuid ()
  "Return a random 32-hex-char UUID-like identifier."
  (format "%08x-%04x-%04x-%04x-%012x"
          (random #x100000000) (random #x10000)
          (random #x10000)     (random #x10000)
          (random #x1000000000000)))

(defvar anvil-orchestrator--tasks (make-hash-table :test 'equal)
  "Hash table id → task plist.")

(defvar anvil-orchestrator--queue nil
  "FIFO list of queued task ids (head = next to run).")

(defvar anvil-orchestrator--running (make-hash-table :test 'equal)
  "Hash table id → process object for currently running tasks.")

(defvar anvil-orchestrator--batches (make-hash-table :test 'equal)
  "Hash table batch-id → list of task-ids in submission order.")

(defvar anvil-orchestrator--pump-timer nil
  "Timer running the pool pump + timeout scan.")

;;;; --- state persistence -------------------------------------------------

(defun anvil-orchestrator--persist (task)
  "Write TASK plist to `anvil-state'.  Returns TASK."
  (let ((id (plist-get task :id)))
    (when id
      (ignore-errors
        (anvil-state-set id task :ns anvil-orchestrator--state-ns))
      (puthash id task anvil-orchestrator--tasks))
    task))

(defun anvil-orchestrator--task-get (id)
  "Return the live task plist for ID, or nil."
  (or (gethash id anvil-orchestrator--tasks)
      (ignore-errors
        (anvil-state-get id :ns anvil-orchestrator--state-ns))))

(defun anvil-orchestrator--task-update (id &rest updates)
  "Apply keyword UPDATES to task ID and persist."
  (let ((task (anvil-orchestrator--task-get id)))
    (when task
      (let ((tail updates))
        (while tail
          (setq task (plist-put task (car tail) (cadr tail)))
          (setq tail (cddr tail))))
      (anvil-orchestrator--persist task))))

(defun anvil-orchestrator--restore-from-state ()
  "Re-hydrate task records from anvil-state into memory.
Tasks that were `running' at shutdown are marked failed because
their OS pids no longer belong to us, but also flagged
`:interrupted t' + `:interrupted-at TS' so
`anvil-orchestrator-resume-interrupted' can distinguish
daemon-restart orphans from genuine exit-code failures and
re-queue them without breaking `:depends-on' chains."
  (anvil-state-enable)
  (clrhash anvil-orchestrator--tasks)
  (clrhash anvil-orchestrator--batches)
  (setq anvil-orchestrator--queue nil)
  (condition-case _err
      (let ((rows (ignore-errors
                    (sqlite-select
                     (anvil-state--db)
                     "SELECT v FROM kv WHERE ns = ?1"
                     (list anvil-orchestrator--state-ns)))))
        (dolist (row rows)
          (let* ((serialized (car row))
                 (task (ignore-errors
                         (anvil-state--deserialize serialized))))
            (when (and task (listp task) (plist-get task :id))
              (pcase (plist-get task :status)
                ('running
                 (setq task (plist-put task :status 'failed))
                 (setq task (plist-put task :error
                                       "anvil-orchestrator: daemon restart interrupted task"))
                 (setq task (plist-put task :interrupted t))
                 (setq task (plist-put task :interrupted-at (float-time)))
                 (anvil-orchestrator--persist task))
                ('queued
                 (puthash (plist-get task :id) task anvil-orchestrator--tasks)
                 (push (plist-get task :id) anvil-orchestrator--queue))
                (_
                 (puthash (plist-get task :id) task anvil-orchestrator--tasks)))
              (let* ((bid (plist-get task :batch-id))
                     (cur (gethash bid anvil-orchestrator--batches)))
                (when bid
                  (puthash bid
                           (append cur (list (plist-get task :id)))
                           anvil-orchestrator--batches)))))))
    (error nil))
  (setq anvil-orchestrator--queue (nreverse anvil-orchestrator--queue)))

;;;; --- worktree isolation -------------------------------------------------

(defun anvil-orchestrator--repo-root (cwd)
  "Return the git top-level directory for CWD, or nil.
Thin wrapper over `anvil-git-repo-root' that additionally guards
against non-strings / non-directories so caller sites can branch
on the return value without pre-validating CWD."
  (when (and (stringp cwd) (file-directory-p cwd))
    (ignore-errors (anvil-git-repo-root cwd))))

(defun anvil-orchestrator--worktree-name-for (task)
  "Return a sanitised worktree name for TASK (safe for git / CLI)."
  (let* ((raw  (or (plist-get task :worktree-name)
                   (plist-get task :name)))
         (safe (replace-regexp-in-string "[^A-Za-z0-9._-]" "-" raw))
         (trim (if (> (length safe) 48) (substring safe 0 48) safe)))
    (concat "anvil-orch-" trim)))

(defun anvil-orchestrator--should-worktree (task provider)
  "Return non-nil when worktree isolation should be applied to TASK."
  (and anvil-orchestrator-worktree-auto
       (not (plist-get task :no-worktree))
       (anvil-orchestrator-provider-supports-worktree provider)
       (anvil-orchestrator--repo-root
        (or (plist-get task :cwd) default-directory))))

(defun anvil-orchestrator--anvil-side-worktree-path (task repo-root)
  "Return an absolute worktree path for TASK under the orchestrator work-dir."
  (expand-file-name
   (format "wt-%s" (or (plist-get task :id) "anon"))
   (expand-file-name "worktrees" anvil-orchestrator-work-dir))
  (ignore repo-root))

(defun anvil-orchestrator--create-anvil-worktree (task repo-root)
  "Create a git worktree for TASK and return its absolute path.
Delegates to `anvil-git-worktree-add', which signals a descriptive
error on non-zero exit."
  (anvil-git-worktree-add
   (anvil-orchestrator--anvil-side-worktree-path task repo-root)
   repo-root))

(defun anvil-orchestrator--apply-worktree (task)
  "Resolve worktree decisions for TASK; returns the possibly-modified plist.
For providers that natively support worktree (e.g. claude), writes
`:_worktree-name' so the build-cmd emits its own flag.  For other
providers, creates an `anvil-git worktree add' under
`anvil-orchestrator-work-dir' and rewrites `:cwd' to that path."
  (let* ((provider (anvil-orchestrator--provider (plist-get task :provider))))
    (if (not (anvil-orchestrator--should-worktree task provider))
        task
      (let* ((native (anvil-orchestrator-provider-supports-worktree provider))
             (name   (anvil-orchestrator--worktree-name-for task)))
        (cond
         ((and native (memq (plist-get task :provider) '(claude)))
          ;; Claude (and any future provider we opt-in here) owns the
          ;; worktree lifecycle via its own --worktree flag — just
          ;; advertise the name to build-cmd.
          (plist-put (copy-sequence task) :_worktree-name name))
         (t
          ;; Generic path: anvil creates the worktree and rewrites :cwd.
          (let* ((root (anvil-orchestrator--repo-root
                        (or (plist-get task :cwd) default-directory)))
                 (wt   (anvil-orchestrator--create-anvil-worktree task root))
                 (new  (copy-sequence task)))
            (setq new (plist-put new :cwd wt))
            (setq new (plist-put new :_worktree-path wt))
            new)))))))

;;;; --- stdout / stderr overflow -------------------------------------------

(defun anvil-orchestrator--truncate-output-file (path)
  "If PATH is over the cap, rewrite it as head + marker + tail.
Returns the original size (number) when truncation happened, else nil.
Binary-safe: reads / writes via `set-buffer-multibyte nil' so
stream-json NDJSON survives round-trip."
  (let* ((attrs (and (file-exists-p path) (file-attributes path)))
         (size  (and attrs (file-attribute-size attrs)))
         (cap   anvil-orchestrator-output-size-cap)
         (head  anvil-orchestrator-output-head-bytes)
         (tail  anvil-orchestrator-output-tail-bytes))
    (when (and size (> size cap))
      (let* ((omitted (max 0 (- size (+ head tail))))
             (tmp (concat path ".trunc"))
             (marker (format "\n---\n[anvil-orchestrator: %d bytes truncated (%d kept as head, %d as tail)]\n---\n"
                             omitted head tail))
             (coding-system-for-read  'binary)
             (coding-system-for-write 'binary))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally path nil 0 head)
          ;; `insert-file-contents-literally' leaves point at the start of
          ;; the inserted region, so move to end before appending the marker.
          (goto-char (point-max))
          (insert marker)
          (write-region (point-min) (point-max) tmp nil 'silent))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally path nil (- size tail) size)
          (write-region (point-min) (point-max) tmp t 'silent))
        (rename-file tmp path t)
        size))))

;;;; --- spawn + sentinel --------------------------------------------------

(defun anvil-orchestrator--ensure-work-dir ()
  "Create `anvil-orchestrator-work-dir' if missing."
  (unless (file-directory-p anvil-orchestrator-work-dir)
    (make-directory anvil-orchestrator-work-dir t)))

(defun anvil-orchestrator--stdout-path (task)
  (expand-file-name (format "%s.stdout" (plist-get task :id))
                    anvil-orchestrator-work-dir))

(defun anvil-orchestrator--stderr-path (task)
  (expand-file-name (format "%s.stderr" (plist-get task :id))
                    anvil-orchestrator-work-dir))

(defun anvil-orchestrator--spawn (task)
  "Spawn TASK's provider CLI and record the process in the running table."
  (anvil-orchestrator--ensure-work-dir)
  (let* ((provider (anvil-orchestrator--provider (plist-get task :provider)))
         ;; Worktree decision runs first and may rewrite :cwd / add
         ;; :_worktree-name that build-cmd reads.
         (task     (anvil-orchestrator--apply-worktree task))
         (cmd      (funcall (anvil-orchestrator-provider-build-cmd provider)
                            task))
         (stdout   (anvil-orchestrator--stdout-path task))
         (stderr   (anvil-orchestrator--stderr-path task))
         (out-buf  (generate-new-buffer (format " *anvil-orch-%s*"
                                                (plist-get task :id))))
         (err-buf  (generate-new-buffer (format " *anvil-orch-err-%s*"
                                                (plist-get task :id))))
         (env      (append (plist-get task :env)
                           (list "ANVIL_IN_ORCHESTRATOR=1")
                           process-environment))
         (default-directory
          (or (plist-get task :cwd) default-directory))
         (process-environment env)
         (proc (make-process
                :name     (format "anvil-orch-%s" (plist-get task :id))
                :buffer   out-buf
                :stderr   err-buf
                :command  cmd
                :connection-type 'pipe
                :coding   'utf-8-unix
                :noquery  t
                :sentinel #'anvil-orchestrator--sentinel)))
    ;; Providers pass the prompt via argv, not stdin; ollama / claude
    ;; wait on the input pipe indefinitely if we leave it open, so the
    ;; task hangs until the wall-clock kills it.  Close stdin eagerly.
    ;; Opt out with :keep-stdin t in the task plist.
    (unless (plist-get task :keep-stdin)
      (ignore-errors (process-send-eof proc)))
    (process-put proc 'anvil-task-id       (plist-get task :id))
    (process-put proc 'anvil-stdout-path   stdout)
    (process-put proc 'anvil-stderr-path   stderr)
    (process-put proc 'anvil-stdout-buffer out-buf)
    (process-put proc 'anvil-stderr-buffer err-buf)
    (process-put proc 'anvil-started-at    (float-time))
    (puthash (plist-get task :id) proc anvil-orchestrator--running)
    (anvil-orchestrator--task-update
     (plist-get task :id)
     :status        'running
     :started-at    (float-time)
     :stdout-path   stdout
     :stderr-path   stderr
     :pid           (process-id proc)
     :cwd           (plist-get task :cwd)
     :_worktree-name (plist-get task :_worktree-name)
     :_worktree-path (plist-get task :_worktree-path))
    proc))

(defun anvil-orchestrator--finalize (proc status)
  "Flush stdout/stderr from PROC buffers, parse, and mark the task done/failed."
  (let* ((id       (process-get proc 'anvil-task-id))
         (out-buf  (process-get proc 'anvil-stdout-buffer))
         (err-buf  (process-get proc 'anvil-stderr-buffer))
         (stdout   (process-get proc 'anvil-stdout-path))
         (stderr   (process-get proc 'anvil-stderr-path))
         (task     (anvil-orchestrator--task-get id))
         (exit     (process-exit-status proc)))
    (when (buffer-live-p out-buf)
      (with-current-buffer out-buf
        (write-region (point-min) (point-max) stdout nil 'silent))
      (kill-buffer out-buf))
    (when (buffer-live-p err-buf)
      (with-current-buffer err-buf
        (write-region (point-min) (point-max) stderr nil 'silent))
      (kill-buffer err-buf))
    (let ((out-orig (and stdout
                         (anvil-orchestrator--truncate-output-file stdout)))
          (err-orig (and stderr
                         (anvil-orchestrator--truncate-output-file stderr))))
      (when (or out-orig err-orig)
        (anvil-orchestrator--task-update
         id
         :stdout-bytes-original out-orig
         :stderr-bytes-original err-orig)))
    (remhash id anvil-orchestrator--running)
    (when task
      (let* ((provider (anvil-orchestrator--provider (plist-get task :provider)))
             (parsed   (ignore-errors
                         (funcall (anvil-orchestrator-provider-parse-output
                                   provider)
                                  stdout stderr exit)))
             (now      (float-time))
             (final    (cond
                        ((eq status 'cancelled) 'cancelled)
                        ((eq status 'timeout)   'failed)
                        ((and (integerp exit) (zerop exit)) 'done)
                        (t 'failed)))
             (error-msg
              (cond
               ((eq status 'timeout) "anvil-orchestrator: wall-clock timeout")
               ((eq status 'cancelled) "anvil-orchestrator: cancelled by user")
               ((eq final 'failed)
                (format "anvil-orchestrator: exit %s" exit))
               (t nil))))
        (anvil-orchestrator--task-update
         id
         :status       final
         :finished-at  now
         :elapsed-ms   (and (plist-get task :started-at)
                            (round (* 1000 (- now (plist-get task :started-at)))))
         :exit-code    exit
         :summary      (plist-get parsed :summary)
         :cost-usd     (plist-get parsed :cost-usd)
         :cost-tokens  (plist-get parsed :cost-tokens)
         :commit-sha   (plist-get parsed :commit-sha)
         :error        error-msg
         :auto-retry-code (plist-get parsed :auto-retry-code)
         :retry-after-ms (and (eq final 'failed)
                              (anvil-orchestrator--stderr-retry-after-ms stderr exit)))
        (anvil-orchestrator--maybe-auto-retry id)
        (anvil-orchestrator--pump)))))

(defun anvil-orchestrator--sentinel (proc event)
  "Sentinel invoked by Emacs on process state transitions."
  (when (memq (process-status proc) '(exit signal))
    (let ((cancelled (process-get proc 'anvil-cancel-reason)))
      (anvil-orchestrator--finalize proc (or cancelled 'normal))))
  (ignore event))

;;;; --- timeout + pump -----------------------------------------------------

(defun anvil-orchestrator--check-timeouts ()
  "SIGTERM (then SIGKILL) processes exceeding their wall-clock cap."
  (let ((now (float-time)))
    (maphash
     (lambda (id proc)
       (when (process-live-p proc)
         (let* ((task (anvil-orchestrator--task-get id))
                (cap  (or (plist-get task :timeout-sec)
                          anvil-orchestrator-timeout-sec-default))
                (started (process-get proc 'anvil-started-at)))
           (when (and started (> (- now started) cap))
             (process-put proc 'anvil-cancel-reason 'timeout)
             (ignore-errors (signal-process proc 'SIGTERM))
             (run-at-time 2 nil
                          (lambda ()
                            (when (process-live-p proc)
                              (ignore-errors
                                (signal-process proc 'SIGKILL)))))))))
     anvil-orchestrator--running)))

(defun anvil-orchestrator--running-count (&optional provider)
  "Return count of tasks whose live process is running (optionally for PROVIDER)."
  (let ((n 0))
    (maphash
     (lambda (id proc)
       (when (process-live-p proc)
         (if provider
             (let ((t0 (anvil-orchestrator--task-get id)))
               (when (eq (plist-get t0 :provider) provider)
                 (cl-incf n)))
           (cl-incf n))))
     anvil-orchestrator--running)
    n))

(defun anvil-orchestrator--per-provider-cap (provider)
  "Return the concurrency cap for PROVIDER."
  (or (alist-get provider anvil-orchestrator-per-provider-concurrency)
      anvil-orchestrator-concurrency))

(defun anvil-orchestrator--dep-tasks (task)
  "Return the list of task plists TASK `:depends-on' resolves to.
Unknown dep names silently drop out (they should already have
been caught by `--validate-batch')."
  (let* ((bid (plist-get task :batch-id))
         (ids (and bid (gethash bid anvil-orchestrator--batches))))
    (delq nil
          (mapcar
           (lambda (dep-name)
             (cl-some
              (lambda (id)
                (let ((t0 (anvil-orchestrator--task-get id)))
                  (and t0 (equal (plist-get t0 :name) dep-name) t0)))
              ids))
           (plist-get task :depends-on)))))

(defun anvil-orchestrator--classify-for-pump (task)
  "Return how to handle TASK at this pump tick.
One of `drop' / `fail-dep' / `skip-dep' / `skip-prov' / `run'.
Also returns (pcase `(fail-dep . DEP-NAME)') when a dep has
already failed so the caller can record the propagation cause."
  (cond
   ((or (null task) (not (eq (plist-get task :status) 'queued))) 'drop)
   (t
    (let* ((deps (anvil-orchestrator--dep-tasks task))
           (failed (cl-find-if
                    (lambda (d)
                      (memq (plist-get d :status) '(failed cancelled)))
                    deps)))
      (cond
       (failed (cons 'fail-dep (plist-get failed :name)))
       ((cl-some (lambda (d) (not (eq (plist-get d :status) 'done))) deps)
        'skip-dep)
       ((let ((prov (plist-get task :provider)))
          (and prov
               (>= (anvil-orchestrator--running-count prov)
                   (anvil-orchestrator--per-provider-cap prov))))
        'skip-prov)
       (t 'run))))))

(defun anvil-orchestrator--pump ()
  "Start as many queued tasks as concurrency caps and the DAG allow.
FIFO-ish: tasks blocked on deps or provider caps are held back,
everything else runs when global concurrency permits."
  (let ((global-cap anvil-orchestrator-concurrency)
        (held       nil))
    (while (and anvil-orchestrator--queue
                (< (anvil-orchestrator--running-count) global-cap))
      (let* ((id      (pop anvil-orchestrator--queue))
             (task    (anvil-orchestrator--task-get id))
             (action  (anvil-orchestrator--classify-for-pump task)))
        (pcase action
          ('drop nil)
          ((or 'skip-dep 'skip-prov) (push id held))
          (`(fail-dep . ,dep-name)
           (anvil-orchestrator--task-update
            id
            :status      'failed
            :finished-at (float-time)
            :error       (format "anvil-orchestrator: dependency %s did not succeed"
                                 dep-name)))
          ('run
           (condition-case err
               (anvil-orchestrator--spawn task)
             (error
              (anvil-orchestrator--task-update
               id
               :status      'failed
               :finished-at (float-time)
               :error       (format "anvil-orchestrator: spawn failed: %s"
                                    (error-message-string err)))))))))
    (setq anvil-orchestrator--queue
          (append (nreverse held) anvil-orchestrator--queue))))

(defun anvil-orchestrator--pump-tick ()
  "Timer callback: timeout scan + pool pump."
  (condition-case _err
      (progn
        (anvil-orchestrator--check-timeouts)
        (anvil-orchestrator--pump))
    (error nil)))

(defun anvil-orchestrator--ensure-pump-timer ()
  "Start the pump timer if not already running."
  (unless (and anvil-orchestrator--pump-timer
               (memq anvil-orchestrator--pump-timer timer-list))
    (setq anvil-orchestrator--pump-timer
          (run-at-time anvil-orchestrator-poll-interval-sec
                       anvil-orchestrator-poll-interval-sec
                       #'anvil-orchestrator--pump-tick))))

(defun anvil-orchestrator--cancel-pump-timer ()
  (when anvil-orchestrator--pump-timer
    (cancel-timer anvil-orchestrator--pump-timer)
    (setq anvil-orchestrator--pump-timer nil)))

;;;; --- auto-retry ---------------------------------------------------------

(defun anvil-orchestrator--resolve-backoff (provider)
  "Return the effective backoff plist for PROVIDER.
The result has shape `(:base-ms N :max-delay-ms M :jitter-pct P)'.
If `anvil-orchestrator-per-provider-backoff' contains an override plist for
PROVIDER, each of keys `:base-ms', `:max-delay-ms', and `:jitter-pct' uses the
override when its value is a number.  Missing or non-numeric override values
fall back to the corresponding global defcustoms:
`anvil-orchestrator-auto-retry-base-ms',
`anvil-orchestrator-auto-retry-max-delay-ms', and
`anvil-orchestrator-auto-retry-jitter-pct'.  If no override exists for
PROVIDER, all values come from the globals."
  (let* ((override (alist-get provider anvil-orchestrator-per-provider-backoff))
         (base-ms (plist-get override :base-ms))
         (max-delay-ms (plist-get override :max-delay-ms))
         (jitter-pct (plist-get override :jitter-pct)))
    (list :base-ms (if (numberp base-ms)
                       base-ms
                     anvil-orchestrator-auto-retry-base-ms)
          :max-delay-ms (if (numberp max-delay-ms)
                            max-delay-ms
                          anvil-orchestrator-auto-retry-max-delay-ms)
          :jitter-pct (if (numberp jitter-pct)
                          jitter-pct
                        anvil-orchestrator-auto-retry-jitter-pct))))

(defun anvil-orchestrator--compute-retry-delay-ms (tries &optional provider retry-after-ms)
  "Return retry delay in milliseconds for TRIES, optionally using PROVIDER and RETRY-AFTER-MS.
Resolve backoff settings with `anvil-orchestrator--resolve-backoff' using
PROVIDER, where a nil PROVIDER falls back to the global settings.  The raw
delay is `min(cap, base * 2^tries)', where `base' is `:base-ms' and `cap' is
`:max-delay-ms' from the resolved plist.  Jitter percentage is taken from
`:jitter-pct' and clamped to the range 0..100.

When jitter is zero, return the raw delay.  Otherwise compute a symmetric
inclusive jitter range around the raw delay using `spread = raw * pct / 100',
`low = raw - spread', and `high = raw + spread', then return a random integer
in the inclusive range `[low, high]'.

When RETRY-AFTER-MS is a number, return the greater of RETRY-AFTER-MS and the
computed backoff delay.  Otherwise return the computed backoff delay."
  (let* ((backoff (anvil-orchestrator--resolve-backoff provider))
         (base (plist-get backoff :base-ms))
         (cap (plist-get backoff :max-delay-ms))
         (raw (min cap (* base (expt 2 tries))))
         (pct (max 0 (min 100 (plist-get backoff :jitter-pct))))
         (computed
          (if (= pct 0)
              raw
            (let* ((spread (/ (* raw pct) 100))
                   (low (- raw spread))
                   (high (+ raw spread)))
              (+ low (random (1+ (- high low))))))))
    (if (numberp retry-after-ms)
        (max retry-after-ms computed)
      computed)))

(defun anvil-orchestrator--maybe-auto-retry (id)
  "Spawn an auto-retry of task ID when its failure matches policy."
  (let* ((task (anvil-orchestrator--task-get id))
         (code (plist-get task :auto-retry-code))
         (tries (or (plist-get task :retry-count) 0))
         (auto-ok (if (plist-member task :auto-retry)
                      (plist-get task :auto-retry) t)))
    (when (and auto-ok
               code
               (memq code anvil-orchestrator-auto-retry-on)
               (< tries anvil-orchestrator-auto-retry-max)
               (eq (plist-get task :status) 'failed))
      (let* ((delay-ms (anvil-orchestrator--compute-retry-delay-ms
                        tries
                        (plist-get task :provider)
                        (plist-get task :retry-after-ms)))
             (new-id   (anvil-orchestrator--uuid))
             (clone    (copy-sequence task)))
        (setq clone (plist-put clone :id new-id))
        (setq clone (plist-put clone :status 'queued))
        (setq clone (plist-put clone :started-at nil))
        (setq clone (plist-put clone :finished-at nil))
        (setq clone (plist-put clone :elapsed-ms nil))
        (setq clone (plist-put clone :exit-code nil))
        (setq clone (plist-put clone :summary nil))
        (setq clone (plist-put clone :error nil))
        (setq clone (plist-put clone :submitted-at (float-time)))
        (setq clone (plist-put clone :retry-count (1+ tries)))
        (setq clone (plist-put clone :retry-of id))
        (setq clone (plist-put clone :retry-reason code))
        (anvil-orchestrator--persist clone)
        (let* ((bid (plist-get clone :batch-id))
               (cur (gethash bid anvil-orchestrator--batches)))
          (when bid
            (puthash bid (append cur (list new-id))
                     anvil-orchestrator--batches)))
        (run-at-time (/ delay-ms 1000.0) nil
                     (lambda ()
                       (setq anvil-orchestrator--queue
                             (append anvil-orchestrator--queue (list new-id)))
                       (anvil-orchestrator--pump)))))))

;;;; --- validation + public API -------------------------------------------

(defun anvil-orchestrator--coerce-task (input)
  "Normalise INPUT (plist or alist) to a task plist with defaults.
Signals `user-error' on missing / malformed fields."
  (let ((task (cond
               ((and (listp input) (keywordp (car input))) input)
               ((listp input)
                ;; alist with string or symbol keys
                (apply #'append
                       (mapcar (lambda (cell)
                                 (let ((k (car cell))
                                       (v (cdr cell)))
                                   (list (intern
                                          (concat ":"
                                                  (cond ((keywordp k)
                                                         (substring (symbol-name k) 1))
                                                        ((symbolp k) (symbol-name k))
                                                        (t (format "%s" k)))))
                                         v)))
                               input)))
               (t (user-error "anvil-orchestrator: task must be a list, got %S"
                              input)))))
    (unless (and (stringp (plist-get task :name))
                 (not (string-empty-p (plist-get task :name))))
      (user-error "anvil-orchestrator: task :name missing or empty"))
    (let ((prov (plist-get task :provider)))
      (unless prov
        (user-error "anvil-orchestrator: task :provider missing (%s)"
                    (plist-get task :name)))
      (when (stringp prov)
        (setq task (plist-put task :provider (intern prov)))))
    (unless (and (stringp (plist-get task :prompt))
                 (not (string-empty-p (plist-get task :prompt))))
      (user-error "anvil-orchestrator: task :prompt missing (%s)"
                  (plist-get task :name)))
    ;; defaults
    (unless (plist-get task :budget-usd)
      (setq task (plist-put task :budget-usd
                            anvil-orchestrator-budget-usd-default)))
    (unless (plist-get task :timeout-sec)
      (setq task (plist-put task :timeout-sec
                            anvil-orchestrator-timeout-sec-default)))
    task))

(defun anvil-orchestrator--validate-deps (tasks)
  "Check every `:depends-on' entry names an existing sibling task.
Also detects cycles.  Signals `user-error' on violation."
  (let ((names (mapcar (lambda (t0) (plist-get t0 :name)) tasks))
        (adj   (make-hash-table :test 'equal)))
    ;; forward edges
    (dolist (t0 tasks)
      (puthash (plist-get t0 :name)
               (or (plist-get t0 :depends-on) nil)
               adj))
    ;; unknown names
    (dolist (t0 tasks)
      (dolist (dep (plist-get t0 :depends-on))
        (unless (member dep names)
          (user-error "anvil-orchestrator: task %s :depends-on unknown %s"
                      (plist-get t0 :name) dep))))
    ;; cycle detection (iterative DFS)
    (let ((visiting (make-hash-table :test 'equal))
          (visited  (make-hash-table :test 'equal)))
      (cl-labels
          ((dfs (node stack)
             (cond
              ((gethash node visiting)
               (user-error "anvil-orchestrator: cyclic :depends-on: %s"
                           (mapconcat #'identity (reverse (cons node stack))
                                      " -> ")))
              ((gethash node visited) nil)
              (t
               (puthash node t visiting)
               (dolist (d (gethash node adj))
                 (dfs d (cons node stack)))
               (remhash node visiting)
               (puthash node t visited)))))
        (dolist (t0 tasks)
          (dfs (plist-get t0 :name) nil))))))

(defun anvil-orchestrator--validate-batch (tasks)
  "Sanity-check the TASKS list before enqueueing.
Signals `user-error' on any violation."
  (when (null tasks)
    (user-error "anvil-orchestrator: empty task list"))
  (when (> (length tasks) anvil-orchestrator-batch-max-tasks)
    (user-error "anvil-orchestrator: batch size %d > cap %d"
                (length tasks) anvil-orchestrator-batch-max-tasks))
  (let ((names (mapcar (lambda (t0) (plist-get t0 :name)) tasks)))
    (unless (= (length names) (length (delete-dups (copy-sequence names))))
      (user-error "anvil-orchestrator: duplicate :name within batch: %S"
                  names)))
  (dolist (t0 tasks)
    (anvil-orchestrator--provider (plist-get t0 :provider)))
  (anvil-orchestrator--validate-deps tasks)
  (let ((total (apply #'+ (mapcar (lambda (t0)
                                    (or (plist-get t0 :budget-usd) 0))
                                  tasks))))
    (when (> total anvil-orchestrator-batch-budget-usd-total)
      (user-error "anvil-orchestrator: batch budget $%.2f exceeds cap $%.2f"
                  total anvil-orchestrator-batch-budget-usd-total))))

;;;###autoload
(defun anvil-orchestrator-submit (tasks)
  "Submit TASKS (list of plists) as a single batch.
Returns the new batch-id UUID.  Tasks run asynchronously; poll
via `anvil-orchestrator-status' or block via
`anvil-orchestrator-collect'."
  (anvil-state-enable)
  (anvil-orchestrator--ensure-work-dir)
  (let* ((coerced (mapcar #'anvil-orchestrator--coerce-task tasks))
         (_valid  (anvil-orchestrator--validate-batch coerced))
         (batch-id (anvil-orchestrator--uuid))
         (now     (float-time))
         (ids     nil))
    (dolist (t0 coerced)
      (let* ((id (anvil-orchestrator--uuid))
             ;; `copy-sequence' disconnects any shared tail that `append'
             ;; left behind -- without it, plist-put on a new key uses
             ;; setcdr to extend the tail in place, so tasks that shared
             ;; a defaults list (e.g. consensus fan-out) clobber each
             ;; other's elapsed-ms / cost-usd / started-at on finalize.
             (task (copy-sequence
                    (append (list :id           id
                                  :batch-id     batch-id
                                  :submitted-at now
                                  :status       'queued)
                            t0))))
        (anvil-orchestrator--persist task)
        (push id ids)
        (setq anvil-orchestrator--queue
              (append anvil-orchestrator--queue (list id)))))
    (puthash batch-id (nreverse ids) anvil-orchestrator--batches)
    (anvil-orchestrator--ensure-pump-timer)
    (anvil-orchestrator--pump)
    batch-id))

(defun anvil-orchestrator--task-summary-plist (task)
  "Return the public slim plist for TASK (omit prompt, paths)."
  (list :id          (plist-get task :id)
        :name        (plist-get task :name)
        :batch-id    (plist-get task :batch-id)
        :provider    (plist-get task :provider)
        :model       (plist-get task :model)
        :status      (plist-get task :status)
        :elapsed-ms  (plist-get task :elapsed-ms)
        :cost-usd    (plist-get task :cost-usd)
        :summary     (plist-get task :summary)
        :commit-sha  (plist-get task :commit-sha)
        :error       (plist-get task :error)
        :exit-code   (plist-get task :exit-code)
        :retry-of    (plist-get task :retry-of)
        :retry-count (plist-get task :retry-count)))

;;;###autoload
(defun anvil-orchestrator-status (id)
  "Return the status plist for task or batch ID.
For a batch: (:batch-id :total :queued :running :done :failed
:cancelled :tasks LIST).  For a task: slim task plist."
  (let ((batch-ids (gethash id anvil-orchestrator--batches)))
    (if batch-ids
        (let* ((tasks (mapcar #'anvil-orchestrator--task-get batch-ids))
               (summary (mapcar #'anvil-orchestrator--task-summary-plist tasks)))
          (list :batch-id  id
                :total     (length tasks)
                :queued    (cl-count 'queued    tasks
                                     :key (lambda (t0) (plist-get t0 :status)))
                :running   (cl-count 'running   tasks
                                     :key (lambda (t0) (plist-get t0 :status)))
                :done      (cl-count 'done      tasks
                                     :key (lambda (t0) (plist-get t0 :status)))
                :failed    (cl-count 'failed    tasks
                                     :key (lambda (t0) (plist-get t0 :status)))
                :cancelled (cl-count 'cancelled tasks
                                     :key (lambda (t0) (plist-get t0 :status)))
                :tasks     summary))
      (let ((task (anvil-orchestrator--task-get id)))
        (and task (anvil-orchestrator--task-summary-plist task))))))

(defun anvil-orchestrator--batch-terminal-p (batch-id)
  "Non-nil when every task in BATCH-ID has reached a terminal state."
  (let ((ids (gethash batch-id anvil-orchestrator--batches)))
    (and ids
         (cl-every (lambda (id)
                     (memq (plist-get (anvil-orchestrator--task-get id) :status)
                           '(done failed cancelled)))
                   ids))))

;;;###autoload
(cl-defun anvil-orchestrator-collect (batch-id &key wait (poll 1.0) (max-wait-sec 1800))
  "Return the list of slim task plists for BATCH-ID.
With :wait non-nil, block until every task reaches a terminal
state (or MAX-WAIT-SEC elapses) using an `accept-process-output'
loop so the Emacs UI stays responsive."
  (let ((ids (gethash batch-id anvil-orchestrator--batches)))
    (unless ids
      (user-error "anvil-orchestrator: unknown batch-id %s" batch-id))
    (when wait
      (let ((deadline (+ (float-time) max-wait-sec)))
        (while (and (not (anvil-orchestrator--batch-terminal-p batch-id))
                    (< (float-time) deadline))
          (accept-process-output nil poll))))
    (mapcar (lambda (id)
              (anvil-orchestrator--task-summary-plist
               (anvil-orchestrator--task-get id)))
            ids)))

;;;###autoload
(defun anvil-orchestrator-cancel (task-id)
  "SIGTERM the running TASK-ID (SIGKILL after 2s).  Returns t on success."
  (let* ((task (anvil-orchestrator--task-get task-id))
         (proc (gethash task-id anvil-orchestrator--running)))
    (cond
     ((null task)
      (user-error "anvil-orchestrator: unknown task-id %s" task-id))
     ((memq (plist-get task :status) '(done failed cancelled))
      nil)
     ((eq (plist-get task :status) 'queued)
      (setq anvil-orchestrator--queue
            (delete task-id anvil-orchestrator--queue))
      (anvil-orchestrator--task-update
       task-id :status 'cancelled :finished-at (float-time)
       :error "anvil-orchestrator: cancelled before start")
      t)
     ((and proc (process-live-p proc))
      (process-put proc 'anvil-cancel-reason 'cancelled)
      (ignore-errors (signal-process proc 'SIGTERM))
      (run-at-time 2 nil
                   (lambda ()
                     (when (process-live-p proc)
                       (ignore-errors (signal-process proc 'SIGKILL)))))
      t)
     (t nil))))

;;;###autoload
(defun anvil-orchestrator-retry (task-id)
  "Re-submit TASK-ID as a new task (same batch, new UUID).  Returns new id."
  (let ((task (anvil-orchestrator--task-get task-id)))
    (unless task
      (user-error "anvil-orchestrator: unknown task-id %s" task-id))
    (let* ((new-id (anvil-orchestrator--uuid))
           (clone  (copy-sequence task)))
      (setq clone (plist-put clone :id new-id))
      (setq clone (plist-put clone :status 'queued))
      (setq clone (plist-put clone :submitted-at (float-time)))
      (setq clone (plist-put clone :started-at nil))
      (setq clone (plist-put clone :finished-at nil))
      (setq clone (plist-put clone :elapsed-ms nil))
      (setq clone (plist-put clone :exit-code nil))
      (setq clone (plist-put clone :summary nil))
      (setq clone (plist-put clone :error nil))
      (setq clone (plist-put clone :retry-of task-id))
      (anvil-orchestrator--persist clone)
      (let* ((bid (plist-get clone :batch-id))
             (cur (gethash bid anvil-orchestrator--batches)))
        (when bid
          (puthash bid (append cur (list new-id))
                   anvil-orchestrator--batches)))
      (setq anvil-orchestrator--queue
            (append anvil-orchestrator--queue (list new-id)))
      (anvil-orchestrator--ensure-pump-timer)
      (anvil-orchestrator--pump)
      new-id)))

;;;; --- Phase 4: cross-model consensus --------------------------------------

(defcustom anvil-orchestrator-consensus-threshold 0.5
  "Minimum pairwise Jaccard similarity for a consensus to be `unanimous'.
When any pairwise similarity drops below this, the verdict becomes
`divergent'.  Range: 0.0 (completely different) to 1.0 (identical)."
  :type 'number
  :group 'anvil-orchestrator)

(defconst anvil-orchestrator--consensus-state-ns "orchestrator-consensus"
  "anvil-state namespace used for persisted consensus metadata.")

(defvar anvil-orchestrator--consensus-groups (make-hash-table :test 'equal)
  "Map consensus-id -> plist (:batch-id :providers :task-ids :created-at).")

(defun anvil-orchestrator--consensus-persist (consensus-id meta)
  "Persist META for CONSENSUS-ID to anvil-state."
  (anvil-state-set consensus-id meta
                   :ns anvil-orchestrator--consensus-state-ns))

(defun anvil-orchestrator--restore-consensus-from-state ()
  "Re-hydrate consensus metadata from anvil-state into memory."
  (clrhash anvil-orchestrator--consensus-groups)
  (condition-case _err
      (let ((rows (ignore-errors
                    (sqlite-select
                     (anvil-state--db)
                     "SELECT k, v FROM kv WHERE ns = ?1"
                     (list anvil-orchestrator--consensus-state-ns)))))
        (dolist (row rows)
          (let* ((cid  (car row))
                 (meta (ignore-errors
                         (anvil-state--deserialize (cadr row)))))
            (when (and cid meta (listp meta))
              (puthash cid meta anvil-orchestrator--consensus-groups)))))
    (error nil)))

(defun anvil-orchestrator--jaccard-similarity (text-a text-b)
  "Return Jaccard similarity (0.0-1.0) between TEXT-A and TEXT-B.
Compared as the set of trimmed non-empty lines.  Both empty
returns 1.0 (vacuously identical); exactly one empty returns 0.0."
  (cl-labels ((lineset (s)
                (let (acc)
                  (dolist (ln (split-string (or s "") "\n"))
                    (let ((t0 (string-trim ln)))
                      (unless (string-empty-p t0)
                        (push t0 acc))))
                  (delete-dups acc))))
    (let ((a (lineset text-a))
          (b (lineset text-b)))
      (cond
       ((and (null a) (null b)) 1.0)
       ((or  (null a) (null b)) 0.0)
       (t
        (let* ((inter (cl-count-if (lambda (x) (member x b)) a))
               (union (- (+ (length a) (length b)) inter)))
          (if (zerop union) 1.0 (/ (float inter) union))))))))

(defun anvil-orchestrator--consensus-build-tasks (prompt providers overrides defaults)
  "Build a fan-out task list from PROMPT across PROVIDERS.
OVERRIDES is an alist ((provider . (:key val ...))).  DEFAULTS
is a plist of fields (e.g. :timeout-sec) applied as trailing
fallbacks per task (each per-provider override wins over a default)."
  (let (tasks)
    (dolist (p providers)
      (let* ((per-p (cdr (assq p overrides)))
             (task  (append (list :name (format "consensus-%s" p)
                                  :provider p
                                  :prompt prompt)
                            per-p defaults)))
        (push task tasks)))
    (nreverse tasks)))

;;;###autoload
(cl-defun anvil-orchestrator-submit-consensus
    (&key prompt providers overrides timeout-sec budget-usd)
  "Dispatch PROMPT to each provider in PROVIDERS and return a plist.
Result: (:consensus-id STR :batch-id STR :providers (SYM...) :task-ids (STR...)).

PROVIDERS must be a list of registered provider symbols with
length >= 2.  OVERRIDES is an alist ((provider . (:key val ...)))
whose per-provider plist is merged into the corresponding task
(e.g. per-provider :model).  TIMEOUT-SEC and BUDGET-USD apply as
defaults for every fan-out task unless already set in OVERRIDES."
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "consensus: prompt must be a non-empty string"))
  (unless (and (listp providers) (>= (length providers) 2))
    (user-error "consensus: providers must be a list of length >= 2"))
  (dolist (p providers)
    (unless (anvil-orchestrator--provider p)
      (user-error "consensus: unknown provider %S" p)))
  (let* ((consensus-id (anvil-orchestrator--uuid))
         (defaults (append
                    (and timeout-sec (list :timeout-sec timeout-sec))
                    (and budget-usd  (list :budget-usd budget-usd))))
         (tasks    (anvil-orchestrator--consensus-build-tasks
                    prompt providers overrides defaults))
         (batch-id (anvil-orchestrator-submit tasks))
         (task-ids (gethash batch-id anvil-orchestrator--batches))
         (meta     (list :batch-id batch-id
                         :providers providers
                         :task-ids task-ids
                         :created-at (float-time))))
    (puthash consensus-id meta anvil-orchestrator--consensus-groups)
    (anvil-orchestrator--consensus-persist consensus-id meta)
    (list :consensus-id consensus-id
          :batch-id batch-id
          :providers providers
          :task-ids task-ids)))

(defun anvil-orchestrator--consensus-similarity-matrix (tasks)
  "Return an alist ((\"PROV_A x PROV_B\" . SCORE) ...) for TASKS.
TASKS is a list of slim task plists.  Each unordered pair is
compared by summary text; pairs with identical provider symbols
still emit one entry using their positional index."
  (let (result
        (n (length tasks))
        (v (apply #'vector tasks)))
    (cl-loop
     for i below n do
     (cl-loop
      for j from (1+ i) below n do
      (let* ((a (aref v i))
             (b (aref v j))
             (pa (plist-get a :provider))
             (pb (plist-get b :provider))
             (key (if (eq pa pb)
                      (format "%s[%d] x %s[%d]" pa i pb j)
                    (format "%s x %s" pa pb)))
             (score (anvil-orchestrator--jaccard-similarity
                     (or (plist-get a :summary) "")
                     (or (plist-get b :summary) ""))))
        (push (cons key score) result))))
    (nreverse result)))

;;;###autoload
(cl-defun anvil-orchestrator-consensus-collect
    (consensus-id &key wait (max-wait-sec 1800))
  "Collect all tasks for CONSENSUS-ID and return a verdict plist.
Result: (:consensus-id STR :batch-id STR :verdict SYM
         :min-similarity FLOAT :matrix ALIST :tasks (PLIST...)).

VERDICT is `unanimous' when every pairwise Jaccard similarity is
>= `anvil-orchestrator-consensus-threshold' AND all tasks reached
`done'; otherwise `divergent'.  Single-provider consensus (not
allowed at submit time) would emit `unanimous' vacuously."
  (let ((meta (gethash consensus-id anvil-orchestrator--consensus-groups)))
    (unless meta
      (user-error "consensus: unknown consensus-id %s" consensus-id))
    (let* ((batch-id (plist-get meta :batch-id))
           (results  (anvil-orchestrator-collect
                      batch-id :wait wait :max-wait-sec max-wait-sec))
           (matrix   (anvil-orchestrator--consensus-similarity-matrix results))
           (min-sim  (if matrix (apply #'min (mapcar #'cdr matrix)) 1.0))
           (all-done (cl-every (lambda (r) (eq (plist-get r :status) 'done))
                               results))
           (verdict  (if (and all-done
                              (>= min-sim anvil-orchestrator-consensus-threshold))
                         'unanimous
                       'divergent)))
      (list :consensus-id consensus-id
            :batch-id batch-id
            :verdict verdict
            :min-similarity min-sim
            :matrix matrix
            :tasks results))))

;;;; --- Phase 4b: meta-LLM judge --------------------------------------------

(defcustom anvil-orchestrator-judge-default-provider 'claude
  "Default provider used by `anvil-orchestrator-judge-consensus'.
Must be a registered provider symbol.  Pick one capable of
synthesis (claude, gemini).  Ollama is viable but plain-text
summaries may be lower quality."
  :type 'symbol
  :group 'anvil-orchestrator)

(defcustom anvil-orchestrator-judge-prompt-template
  "You are adjudicating responses from multiple AI assistants to a single question.

ORIGINAL QUESTION:
%s

CANDIDATE RESPONSES:
%s

Produce a single synthesized answer that captures the best of these candidates.  Begin with the answer itself, then add a brief rationale (max 3 sentences) noting agreement and disagreement between candidates."
  "Template for the judge prompt.
Two %s placeholders are filled in order: (1) the original prompt,
(2) the formatted candidate block.  Override to change tone or to
request a structured output (e.g. JSON)."
  :type 'string
  :group 'anvil-orchestrator)

(defun anvil-orchestrator--judge-format-candidates (tasks)
  "Format TASKS (slim plist list) into a numbered candidate block."
  (let (acc (i 0))
    (dolist (t0 tasks)
      (setq i (1+ i))
      (push (format "%d. [provider: %s, status: %s]\n%s"
                    i
                    (or (plist-get t0 :provider) "?")
                    (or (plist-get t0 :status) "?")
                    (or (plist-get t0 :summary)
                        (plist-get t0 :error)
                        "(no output)"))
            acc))
    (mapconcat #'identity (nreverse acc) "\n\n")))

(defun anvil-orchestrator--judge-build-prompt (original-prompt tasks)
  "Build the full judge prompt from ORIGINAL-PROMPT + TASKS."
  (format anvil-orchestrator-judge-prompt-template
          (or original-prompt "")
          (anvil-orchestrator--judge-format-candidates tasks)))

(defun anvil-orchestrator--judge-original-prompt (meta)
  "Return the original prompt for the consensus META, or nil.
Walks the fan-out task ids until one yields a :prompt."
  (let ((task-ids (plist-get meta :task-ids))
        found)
    (catch 'done
      (dolist (id task-ids)
        (let ((task (anvil-orchestrator--task-get id)))
          (when (and task (plist-get task :prompt))
            (setq found (plist-get task :prompt))
            (throw 'done t)))))
    found))

;;;###autoload
(cl-defun anvil-orchestrator-judge-consensus
    (consensus-id &key judge judge-model extra wait
                  (max-wait-sec 1800) timeout-sec)
  "Submit a meta-LLM judge task for CONSENSUS-ID.
Requires the fan-out batch to be terminal (pass :WAIT t to block
until then).  :JUDGE defaults to
`anvil-orchestrator-judge-default-provider'.  :EXTRA, when
non-nil, is appended after the rendered candidate block to adjust
adjudication instructions without redefining the template.

Returns (:consensus-id STR :judge-task-id STR :judge-batch-id STR
         :judge-provider SYM :judge-model STRING-or-NIL)."
  (let ((meta (gethash consensus-id anvil-orchestrator--consensus-groups)))
    (unless meta
      (user-error "judge: unknown consensus-id %s" consensus-id))
    (let* ((batch-id (plist-get meta :batch-id))
           (results  (anvil-orchestrator-collect
                      batch-id :wait wait :max-wait-sec max-wait-sec)))
      (unless (anvil-orchestrator--batch-terminal-p batch-id)
        (user-error
         "judge: consensus %s batch %s is not terminal (pass :wait t)"
         consensus-id batch-id))
      (let* ((prov   (or judge anvil-orchestrator-judge-default-provider))
             (_check (unless (anvil-orchestrator--provider prov)
                       (user-error "judge: unknown provider %S" prov)))
             (orig   (anvil-orchestrator--judge-original-prompt meta))
             (core   (anvil-orchestrator--judge-build-prompt orig results))
             (prompt (if (and extra (stringp extra)
                              (not (string-empty-p extra)))
                         (concat core "\n\n" extra)
                       core))
             (task   (append
                      (list :name (format "consensus-judge-%s"
                                          (substring consensus-id 0 8))
                            :provider prov
                            :prompt prompt)
                      (and judge-model (list :model judge-model))
                      (and timeout-sec (list :timeout-sec timeout-sec))))
             (new-batch (anvil-orchestrator-submit (list task)))
             (task-id   (car (gethash new-batch
                                      anvil-orchestrator--batches)))
             (updated   meta))
        (ignore _check)
        (setq updated (plist-put updated :judge-task-id task-id))
        (setq updated (plist-put updated :judge-batch-id new-batch))
        (setq updated (plist-put updated :judge-provider prov))
        (setq updated (plist-put updated :judge-model judge-model))
        (setq updated (plist-put updated :judge-submitted-at (float-time)))
        (puthash consensus-id updated anvil-orchestrator--consensus-groups)
        (anvil-orchestrator--consensus-persist consensus-id updated)
        (list :consensus-id   consensus-id
              :judge-task-id  task-id
              :judge-batch-id new-batch
              :judge-provider prov
              :judge-model    judge-model)))))

;;;###autoload
(cl-defun anvil-orchestrator-judge-collect
    (consensus-id &key wait (max-wait-sec 1800))
  "Return the judge task plist for CONSENSUS-ID.
Result: (:consensus-id :judge-task-id :judge-provider :judge PLIST).
Errors when no judge has been submitted for the consensus."
  (let ((meta (gethash consensus-id anvil-orchestrator--consensus-groups)))
    (unless meta
      (user-error "judge-collect: unknown consensus-id %s" consensus-id))
    (let ((judge-id    (plist-get meta :judge-task-id))
          (judge-batch (plist-get meta :judge-batch-id)))
      (unless judge-id
        (user-error
         "judge-collect: no judge submitted for consensus %s" consensus-id))
      (when (and wait judge-batch)
        (let ((deadline (+ (float-time) max-wait-sec)))
          (while (and (not (anvil-orchestrator--batch-terminal-p judge-batch))
                      (< (float-time) deadline))
            (accept-process-output nil 0.1))))
      (let ((task (anvil-orchestrator--task-get judge-id)))
        (list :consensus-id   consensus-id
              :judge-task-id  judge-id
              :judge-provider (plist-get meta :judge-provider)
              :judge          (and task
                                   (anvil-orchestrator--task-summary-plist
                                    task)))))))

;;;; --- MCP tool handlers -------------------------------------------------

(defun anvil-orchestrator--parse-tasks-json (tasks-json)
  "Convert MCP string TASKS-JSON into the Elisp task list."
  (unless (and (stringp tasks-json) (not (string-empty-p tasks-json)))
    (user-error "orchestrator-submit: tasks must be a non-empty JSON array string"))
  (let ((parsed (json-parse-string tasks-json
                                   :object-type 'plist
                                   :array-type  'list
                                   :null-object nil
                                   :false-object nil)))
    (unless (listp parsed)
      (user-error "orchestrator-submit: tasks JSON must parse to an array"))
    parsed))

(defun anvil-orchestrator--tool-submit (tasks)
  "Submit TASKS (JSON array string) and return the batch id.

MCP Parameters:
  tasks - JSON array of task objects.  Each object must have
          `name' (string, unique within batch), `provider'
          (e.g. \"claude\"), and `prompt' (string).  Optional
          keys: `model', `cwd', `budget_usd', `timeout_sec',
          `system_prompt_append', `allowed_tools',
          `permission_mode'.

Returns a plist (:batch-id STR :total N)."
  (anvil-server-with-error-handling
   (let* ((parsed    (anvil-orchestrator--parse-tasks-json tasks))
          (batch-id  (anvil-orchestrator-submit parsed)))
     (list :batch-id batch-id :total (length parsed)))))

(defun anvil-orchestrator--tool-status (id)
  "Return orchestrator status for task or batch ID.

MCP Parameters:
  id - Batch id or task id string."
  (anvil-server-with-error-handling
   (or (anvil-orchestrator-status id)
       (user-error "orchestrator-status: unknown id %s" id))))

(defun anvil-orchestrator--tool-collect (batch_id &optional wait)
  "Collect task summaries for BATCH_ID.

MCP Parameters:
  batch_id - Batch id returned by orchestrator-submit.
  wait     - When truthy (\"t\" / \"true\" / non-empty), block until
             every task reaches a terminal state."
  (anvil-server-with-error-handling
   (let ((w (and wait (not (member wait '("" "nil" "false" "0"))))))
     (anvil-orchestrator-collect batch_id :wait w))))

(defun anvil-orchestrator--tool-cancel (task_id)
  "Cancel running task TASK_ID.

MCP Parameters:
  task_id - Task id string to cancel."
  (anvil-server-with-error-handling
   (list :cancelled (and (anvil-orchestrator-cancel task_id) t))))

(defun anvil-orchestrator--tool-retry (task_id)
  "Re-submit task TASK_ID under a new id.

MCP Parameters:
  task_id - Task id string to retry."
  (anvil-server-with-error-handling
   (list :new-task-id (anvil-orchestrator-retry task_id))))

(defun anvil-orchestrator--parse-providers-json (providers-json)
  "Parse PROVIDERS-JSON (JSON string array) into a list of provider symbols."
  (unless (and (stringp providers-json) (not (string-empty-p providers-json)))
    (user-error "orchestrator-consensus-submit: providers must be a non-empty JSON array string"))
  (let ((parsed (json-parse-string providers-json
                                   :array-type 'list
                                   :object-type 'plist
                                   :null-object nil
                                   :false-object nil)))
    (unless (listp parsed)
      (user-error "orchestrator-consensus-submit: providers JSON must parse to an array"))
    (mapcar (lambda (s)
              (cond
               ((symbolp s) s)
               ((stringp s) (intern s))
               (t (user-error "orchestrator-consensus-submit: provider %S must be string or symbol" s))))
            parsed)))

(defun anvil-orchestrator--parse-overrides-json (overrides-json)
  "Parse OVERRIDES-JSON (object of provider->plist) into an alist."
  (when (and overrides-json (stringp overrides-json)
             (not (string-empty-p overrides-json)))
    (let ((parsed (json-parse-string overrides-json
                                     :object-type 'plist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil)))
      (cl-loop for (k v) on parsed by #'cddr
               collect (cons (intern (substring (symbol-name k) 1)) v)))))

(defun anvil-orchestrator--tool-consensus-submit
    (prompt providers &optional overrides timeout_sec budget_usd)
  "Dispatch PROMPT to each provider and return a consensus plist.

MCP Parameters:
  prompt      - Prompt string sent to every provider.
  providers   - JSON array of provider names (e.g. [\"claude\",\"gemini\"]).
  overrides   - Optional JSON object of per-provider plist fields, e.g.
                {\"claude\":{\"model\":\"claude-opus-4-7\"},\"gemini\":{}}.
  timeout_sec - Optional per-task timeout seconds (string).
  budget_usd  - Optional per-task budget USD (string).

Returns (:consensus-id STR :batch-id STR :providers LIST :task-ids LIST)."
  (anvil-server-with-error-handling
   (let* ((provs (anvil-orchestrator--parse-providers-json providers))
          (over  (anvil-orchestrator--parse-overrides-json overrides))
          (tsec  (and timeout_sec (stringp timeout_sec)
                      (not (string-empty-p timeout_sec))
                      (string-to-number timeout_sec)))
          (busd  (and budget_usd  (stringp budget_usd)
                      (not (string-empty-p budget_usd))
                      (string-to-number budget_usd))))
     (anvil-orchestrator-submit-consensus
      :prompt prompt :providers provs :overrides over
      :timeout-sec tsec :budget-usd busd))))

(defun anvil-orchestrator--tool-consensus-collect (consensus_id &optional wait)
  "Collect verdict for CONSENSUS_ID.

MCP Parameters:
  consensus_id - Consensus id string returned by orchestrator-consensus-submit.
  wait         - When truthy (\"t\"/\"true\"/non-empty), block until all
                 fan-out tasks reach a terminal state."
  (anvil-server-with-error-handling
   (let ((w (and wait (not (member wait '("" "nil" "false" "0"))))))
     (anvil-orchestrator-consensus-collect consensus_id :wait w))))

(defun anvil-orchestrator--tool-consensus-judge
    (consensus_id &optional judge judge_model extra wait timeout_sec)
  "Submit a meta-LLM judge task for CONSENSUS_ID and return the id.

MCP Parameters:
  consensus_id - Consensus id returned by orchestrator-consensus-submit.
  judge        - Judge provider symbol-name string (e.g. \"claude\").
                 Defaults to `anvil-orchestrator-judge-default-provider'.
  judge_model  - Optional model override for the judge task.
  extra        - Optional instruction appended after the rendered
                 candidate block (e.g. \"Answer in <= 120 words.\").
  wait         - When truthy (\"t\"/\"true\"/non-empty), block until
                 the fan-out batch reaches terminal state before
                 dispatching the judge.
  timeout_sec  - Optional judge-task wall-clock cap in seconds.

Returns (:consensus-id :judge-task-id :judge-batch-id
         :judge-provider :judge-model)."
  (anvil-server-with-error-handling
   (let* ((judge-sym (and judge (stringp judge)
                          (not (string-empty-p judge))
                          (intern judge)))
          (model     (and judge_model (stringp judge_model)
                          (not (string-empty-p judge_model))
                          judge_model))
          (extra-str (and extra (stringp extra)
                          (not (string-empty-p extra))
                          extra))
          (w         (and wait (not (member wait '("" "nil" "false" "0")))))
          (tsec      (and timeout_sec (stringp timeout_sec)
                          (not (string-empty-p timeout_sec))
                          (string-to-number timeout_sec))))
     (anvil-orchestrator-judge-consensus
      consensus_id :judge judge-sym :judge-model model
      :extra extra-str :wait w :timeout-sec tsec))))

(defun anvil-orchestrator--tool-consensus-judge-collect
    (consensus_id &optional wait)
  "Return judge task summary for CONSENSUS_ID.

MCP Parameters:
  consensus_id - Consensus id string.
  wait         - When truthy, block until judge task reaches a
                 terminal state."
  (anvil-server-with-error-handling
   (let ((w (and wait (not (member wait '("" "nil" "false" "0"))))))
     (anvil-orchestrator-judge-collect consensus_id :wait w))))

;;;; --- observability -------------------------------------------------------

(defun anvil-orchestrator--percentile (sorted-values pct)
  "Return the PCT percentile from SORTED-VALUES.
SORTED-VALUES must already be sorted ascending.  PCT is an integer
between 0 and 100 inclusive.  Return nil when SORTED-VALUES is empty.
Otherwise return the value at index
`(floor (* (/ pct 100.0) (1- n)))', where N is the list length."
  (when sorted-values
    (let* ((n (length sorted-values))
           (idx (floor (* (/ pct 100.0) (1- n)))))
      (nth idx sorted-values))))

(defun anvil-orchestrator-stats (&rest args)
  "Aggregate observability statistics from `anvil-orchestrator--tasks'.

Accepted keyword ARGS: `:since' (float-time cutoff) and `:provider'
(provider symbol).  Both filter tasks; see helpers for semantics.
Returns a plist with keys :total, :by-status, :by-provider,
:elapsed-ms-avg, :elapsed-ms-p50, :elapsed-ms-p95, :total-cost-usd,
:since, :provider."
  (let* ((since (plist-get args :since))
         (provider-filter (plist-get args :provider))
         (total 0)
         (done 0)
         (failed 0)
         (cancelled 0)
         (running 0)
         (queued 0)
         (elapsed-values nil)
         (elapsed-sum 0)
         (elapsed-count 0)
         (total-cost 0.0)
         (provider-stats (make-hash-table :test 'eq)))
    (cl-labels
        ((round-cost (value)
           (/ (round (* value 1000000)) 1000000.0))
         (mean-or-nil (sum count)
           (when (> count 0)
             (round (/ (float sum) count)))))
      (maphash
       (lambda (_id task)
         (let ((task-provider (plist-get task :provider))
               (task-finished-at (plist-get task :finished-at)))
           (when (and (or (null provider-filter)
                          (eq task-provider provider-filter))
                      (or (not (numberp since))
                          (and (numberp task-finished-at)
                               (>= task-finished-at since))))
             (cl-incf total)
             (pcase (plist-get task :status)
               ('done (cl-incf done))
               ('failed (cl-incf failed))
               ('cancelled (cl-incf cancelled))
               ('running (cl-incf running))
               ('queued (cl-incf queued)))
             (let ((elapsed-ms (plist-get task :elapsed-ms)))
               (when (integerp elapsed-ms)
                 (push elapsed-ms elapsed-values)
                 (cl-incf elapsed-sum elapsed-ms)
                 (cl-incf elapsed-count)))
             (let ((cost-usd (plist-get task :cost-usd)))
               (when (numberp cost-usd)
                 (setq total-cost (+ total-cost cost-usd))))
             (let* ((bucket (or (gethash task-provider provider-stats)
                                (list :total 0
                                      :done 0
                                      :failed 0
                                      :elapsed-ms-sum 0
                                      :elapsed-ms-count 0
                                      :cost-usd-total 0.0)))
                    (bucket-elapsed-ms (plist-get task :elapsed-ms))
                    (bucket-cost-usd (plist-get task :cost-usd)))
               (setq bucket (plist-put bucket :total (1+ (plist-get bucket :total))))
               (when (eq (plist-get task :status) 'done)
                 (setq bucket (plist-put bucket :done (1+ (plist-get bucket :done)))))
               (when (eq (plist-get task :status) 'failed)
                 (setq bucket (plist-put bucket :failed (1+ (plist-get bucket :failed)))))
               (when (integerp bucket-elapsed-ms)
                 (setq bucket
                       (plist-put bucket :elapsed-ms-sum
                                  (+ (plist-get bucket :elapsed-ms-sum) bucket-elapsed-ms)))
                 (setq bucket
                       (plist-put bucket :elapsed-ms-count
                                  (1+ (plist-get bucket :elapsed-ms-count)))))
               (when (numberp bucket-cost-usd)
                 (setq bucket
                       (plist-put bucket :cost-usd-total
                                  (+ (plist-get bucket :cost-usd-total) bucket-cost-usd))))
               (puthash task-provider bucket provider-stats)))))
       anvil-orchestrator--tasks)
      (let* ((sorted-elapsed (sort elapsed-values #'<))
             (by-provider nil))
        (maphash
         (lambda (prov bucket)
           (push
            (cons prov
                  (list :total (plist-get bucket :total)
                        :done (plist-get bucket :done)
                        :failed (plist-get bucket :failed)
                        :elapsed-ms-avg
                        (mean-or-nil (plist-get bucket :elapsed-ms-sum)
                                     (plist-get bucket :elapsed-ms-count))
                        :cost-usd-total
                        (round-cost (plist-get bucket :cost-usd-total))))
            by-provider))
         provider-stats)
        (setq by-provider
              (sort by-provider
                    (lambda (a b)
                      (string< (symbol-name (car a))
                               (symbol-name (car b))))))
        (list :total total
              :by-status (list :done done
                               :failed failed
                               :cancelled cancelled
                               :running running
                               :queued queued)
              :by-provider by-provider
              :elapsed-ms-avg (mean-or-nil elapsed-sum elapsed-count)
              :elapsed-ms-p50 (anvil-orchestrator--percentile sorted-elapsed 50)
              :elapsed-ms-p95 (anvil-orchestrator--percentile sorted-elapsed 95)
              :total-cost-usd (round-cost total-cost)
              :since since
              :provider provider-filter)))))

(defun anvil-orchestrator--format-stats-text (stats)
  "Return a human-readable multi-line stats summary for STATS.
Nil numeric values render as \"-\".  Cost values render with four
decimal places, or \"-\" when nil or zero."
  (cl-labels
      ((fmt-num (v)
         (if (numberp v) (number-to-string v) "-"))
       (fmt-cost (v)
         (if (and (numberp v) (/= v 0.0))
             (format "%.4f" v)
           "-"))
       (provider-line (entry)
         (let* ((provider (car entry))
                (plist (cdr entry))
                (cost (or (plist-get plist :cost-usd-total)
                          (plist-get plist :total-cost-usd))))
           (format "  %s  total=%s  done=%s  failed=%s  avg-ms=%s  cost=%s"
                   provider
                   (fmt-num (plist-get plist :total))
                   (fmt-num (plist-get plist :done))
                   (fmt-num (plist-get plist :failed))
                   (fmt-num (plist-get plist :elapsed-ms-avg))
                   (fmt-cost cost)))))
    (let* ((by-status (plist-get stats :by-status))
           (by-provider (plist-get stats :by-provider))
           (lines (list
                   "Anvil Orchestrator stats"
                   "------------------------"
                   (format "total:      %s"
                           (fmt-num (plist-get stats :total)))
                   (format "by-status:  done=%s failed=%s cancelled=%s running=%s queued=%s"
                           (fmt-num (plist-get by-status :done))
                           (fmt-num (plist-get by-status :failed))
                           (fmt-num (plist-get by-status :cancelled))
                           (fmt-num (plist-get by-status :running))
                           (fmt-num (plist-get by-status :queued)))
                   (format "elapsed-ms: avg=%s p50=%s p95=%s"
                           (fmt-num (plist-get stats :elapsed-ms-avg))
                           (fmt-num (plist-get stats :elapsed-ms-p50))
                           (fmt-num (plist-get stats :elapsed-ms-p95)))
                   (format "cost-usd:   %s"
                           (fmt-cost (plist-get stats :total-cost-usd)))
                   ""
                   "by-provider:")))
      (when by-provider
        (setq lines
              (append lines
                      (mapcar #'provider-line by-provider))))
      (string-join lines "\n"))))

;;;###autoload
(defun anvil-orchestrator-stats-dashboard ()
  "Show the orchestrator stats dashboard in a reusable special-mode buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Anvil Orchestrator Stats*"))
        (text (anvil-orchestrator--format-stats-text
               (anvil-orchestrator-stats))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

(defun anvil-orchestrator--tool-stats ()
  "MCP wrapper for `anvil-orchestrator-stats'.
The server layer performs JSON serialisation; this function
returns the stats plist verbatim."
  (anvil-orchestrator-stats))

;;;; --- DAG resume (Phase 6C'') ---------------------------------------------
;;
;; When the main daemon dies mid-task, every `running' task's subprocess
;; becomes a detached orphan that anvil can no longer collect stdout
;; from.  `--restore-from-state' flips those tasks to `failed' with an
;; `:interrupted t' flag so the distinction between "genuine exit-code
;; failure" and "daemon restart orphan" survives rehydration.  This
;; section adds the resume operation: re-queue interrupted tasks in
;; place (preserving their task-id + `:depends-on' so downstream work
;; reconnects to the resumed upstream) and let the existing DAG-aware
;; pump restart them in topological order.

(defun anvil-orchestrator--task-interrupted-p (task)
  "Return non-nil when TASK was orphaned by a daemon restart and not yet resumed.
Requires status = `failed' + `:interrupted t' + no `:resumed-at'.
Status check is defensive against inconsistent state where only
the flag survives a manual patch."
  (and task
       (eq 'failed (plist-get task :status))
       (plist-get task :interrupted)
       (not (plist-get task :resumed-at))))

;;;###autoload
(defun anvil-orchestrator-list-interrupted (&optional batch-id)
  "Return the list of task plists orphaned by a daemon restart.
When BATCH-ID is non-nil, restrict to that batch; otherwise scan
the whole in-memory task table."
  (let (out)
    (if batch-id
        (dolist (id (gethash batch-id anvil-orchestrator--batches))
          (let ((task (anvil-orchestrator--task-get id)))
            (when (anvil-orchestrator--task-interrupted-p task)
              (push task out))))
      (maphash (lambda (_id task)
                 (when (anvil-orchestrator--task-interrupted-p task)
                   (push task out)))
               anvil-orchestrator--tasks))
    (nreverse out)))

(defun anvil-orchestrator--resume-one (task)
  "Flip TASK from `:interrupted' `failed' to a fresh `queued' record.
Clears the `:error' / `:finished-at' / `:elapsed-ms' /
`:exit-code' / `:summary' / `:interrupted*' slots so the task
looks like a first-time queue entry; keeps `:id' so inbound
`:depends-on' references stay valid."
  (let ((id (plist-get task :id)))
    (anvil-orchestrator--task-update
     id
     :status         'queued
     :error          nil
     :finished-at    nil
     :elapsed-ms     nil
     :exit-code      nil
     :summary        nil
     :cost-usd       nil
     :cost-tokens    nil
     :started-at     nil
     :interrupted    nil
     :interrupted-at nil
     :resumed-at     (float-time))
    (setq anvil-orchestrator--queue
          (append anvil-orchestrator--queue (list id)))
    id))

;;;###autoload
(defun anvil-orchestrator-resume-interrupted (&optional batch-id)
  "Re-queue every interrupted (daemon-restart orphan) task.
When BATCH-ID is non-nil, restrict the resume to that batch.

Each target task is flipped from `:interrupted' `failed' back
to `queued' in place — its task-id is preserved so inbound
`:depends-on' references stay valid and the existing DAG-aware
pump (`--classify-for-pump') handles topological ordering.

Returns a plist `(:resumed IDS :already-blocked IDS :total N)':
  :resumed         — task-ids re-queued and immediately DAG-ready
  :already-blocked — task-ids re-queued but waiting on a dep that
                     is still `failed' / `interrupted'; the pump
                     will fail them unless the dep is resumed too
  :total           — count of tasks acted on"
  (interactive)
  (let* ((targets (anvil-orchestrator-list-interrupted batch-id))
         resumed blocked)
    (dolist (task targets)
      (let ((id (anvil-orchestrator--resume-one task)))
        (let* ((reloaded (anvil-orchestrator--task-get id)))
          (if (anvil-orchestrator--dag-ready-p reloaded)
              (push id resumed)
            (push id blocked)))))
    (when targets
      (anvil-orchestrator--ensure-pump-timer)
      (anvil-orchestrator--pump))
    (list :resumed         (nreverse resumed)
          :already-blocked (nreverse blocked)
          :total           (length targets))))

(defun anvil-orchestrator--dag-ready-p (task)
  "Return non-nil when every TASK `:depends-on' dep is already `done'."
  (let ((deps (plist-get task :depends-on)))
    (or (null deps)
        (cl-every (lambda (dep-id-or-name)
                    ;; deps are stored as names within a batch; resolve
                    ;; to the actual task record
                    (let ((dep (or (anvil-orchestrator--task-get dep-id-or-name)
                                   (anvil-orchestrator--find-dep-by-name
                                    dep-id-or-name
                                    (plist-get task :batch-id)))))
                      (and dep (eq 'done (plist-get dep :status)))))
                  deps))))

(defun anvil-orchestrator--find-dep-by-name (name batch-id)
  "Return the task record whose `:name' = NAME within BATCH-ID.
`:depends-on' stores sibling names; callers that want status
resolution have to map back to the task record via the batch."
  (when (and name batch-id)
    (cl-some (lambda (id)
               (let ((t0 (anvil-orchestrator--task-get id)))
                 (and t0
                      (equal name (plist-get t0 :name))
                      t0)))
             (gethash batch-id anvil-orchestrator--batches))))

(defun anvil-orchestrator--tool-list-interrupted (&optional batch_id)
  "MCP wrapper — list interrupted (daemon-restart orphan) tasks.

MCP Parameters:
  batch_id - Optional batch-id filter; omit to scan all tasks."
  (anvil-server-with-error-handling
    (list :tasks
          (mapcar #'anvil-orchestrator--task-summary-plist
                  (anvil-orchestrator-list-interrupted
                   (and batch_id
                        (not (string-empty-p batch_id))
                        batch_id))))))

(defun anvil-orchestrator--tool-resume-interrupted (&optional batch_id)
  "MCP wrapper — re-queue interrupted tasks.

MCP Parameters:
  batch_id - Optional batch-id scope; omit to resume every
             interrupted task in the orchestrator."
  (anvil-server-with-error-handling
    (anvil-orchestrator-resume-interrupted
     (and batch_id
          (not (string-empty-p batch_id))
          batch_id))))

;;;; --- glue helpers (dogfood friction reducers) ---------------------------

;;;###autoload
(defun anvil-orchestrator-extract-result (task-id &optional full)
  "Return the result plist for TASK-ID.

Produces the minimum fields a caller needs to act on a finished
task without opening `:stdout-path' by hand: `:summary' (the
provider-parsed final message), `:cost-usd', `:tokens',
`:commit-sha', `:exit-code', and `:error'.

When FULL is non-nil the provider's `parse-output' hook is
re-invoked on the raw stream files with
`anvil-orchestrator-summary-max-chars' bound to
`most-positive-fixnum', so the caller receives the untruncated
agent message.  FULL is the right knob for rare long answers
(codex long explanations, consensus judge narratives); the
default summary is already 4000 chars which fits the vast
majority of single-turn responses."
  (let* ((task (anvil-orchestrator--task-get task-id))
         (_    (unless task
                 (user-error "extract-result: unknown task id %s" task-id)))
         (base (list :task-id    task-id
                     :status     (plist-get task :status)
                     :provider   (plist-get task :provider)
                     :cost-usd   (plist-get task :cost-usd)
                     :tokens     (plist-get task :cost-tokens)
                     :commit-sha (plist-get task :commit-sha)
                     :exit-code  (plist-get task :exit-code)
                     :error      (plist-get task :error))))
    (if (not full)
        (append base (list :summary (plist-get task :summary)))
      (let* ((provider (plist-get task :provider))
             (prov-rec (and provider (anvil-orchestrator--provider provider)))
             (parser   (and prov-rec
                            (anvil-orchestrator-provider-parse-output prov-rec)))
             (stdout   (plist-get task :stdout-path))
             (stderr   (plist-get task :stderr-path))
             (exit     (plist-get task :exit-code)))
        (unless (and parser stdout (file-readable-p stdout))
          (user-error "extract-result: cannot re-parse (provider=%s, stdout-readable=%s)"
                      provider (and stdout (file-readable-p stdout))))
        (let* ((anvil-orchestrator-summary-max-chars most-positive-fixnum)
               (reparsed (funcall parser stdout stderr exit)))
          (append base (list :summary (plist-get reparsed :summary))))))))

;;;###autoload
(cl-defun anvil-orchestrator-submit-one
    (&key provider prompt model name cwd budget-usd timeout-sec)
  "Submit a single task and return its task-id (not the batch-id).

A light convenience over `anvil-orchestrator-submit' for the
common case where the caller has exactly one prompt and does
not need DAG / consensus / multi-task semantics.  All keyword
arguments except :provider and :prompt are optional.  Behaviour
is identical to passing a one-element list through `submit':
the task enters the global pool with the same concurrency,
retry, and budget rules as any other batch."
  (unless (and provider prompt)
    (user-error "submit-one: :provider and :prompt are required"))
  (let* ((auto-name (or name
                        (format "one-%s-%s"
                                (if (symbolp provider)
                                    (symbol-name provider)
                                  provider)
                                (format-time-string "%H%M%S"))))
         (task (append (list :name auto-name
                             :provider provider
                             :prompt prompt)
                       (when model       (list :model model))
                       (when cwd         (list :cwd cwd))
                       (when budget-usd  (list :budget-usd budget-usd))
                       (when timeout-sec (list :timeout-sec timeout-sec))))
         (batch-id (anvil-orchestrator-submit (list task)))
         (ids      (gethash batch-id anvil-orchestrator--batches)))
    (car ids)))

;;;###autoload
(cl-defun anvil-orchestrator-submit-and-collect
    (&key provider prompt model name cwd budget-usd timeout-sec
          (collect-timeout-sec 180) (poll-interval-sec 0.5) full)
  "Submit a single task and wait (non-blocking) for its result.

A thin composition of `anvil-orchestrator-submit-one' + internal
poll + `anvil-orchestrator-extract-result' so short-running tasks
(30 s to a few minutes) can be dispatched in one call without the
submit → poll → tail → extract hop chain.

:provider / :prompt are forwarded to `submit-one'; :model / :name
/ :cwd / :budget-usd / :timeout-sec likewise when non-nil.

:collect-timeout-sec (default 180) is the wall-clock cap on the
poll loop.  :poll-interval-sec (default 0.5) tunes the
`accept-process-output' cadence.  :full is passed through to
`extract-result' when the task completes.

Returns a plist.  When the task reaches a terminal state the
plist is `extract-result' s output plus `:pending nil'; fields
include `:task-id', `:status' (`done' / `failed' / `cancelled'),
`:summary', `:error', etc.  When the collect timeout elapses
first, returns `(:task-id ID :status running :pending t)' and
the task is deliberately not cancelled so the caller can
re-collect via `extract-result' later."
  (let* ((task-id (anvil-orchestrator-submit-one
                   :provider provider
                   :prompt prompt
                   :model model
                   :name name
                   :cwd cwd
                   :budget-usd budget-usd
                   :timeout-sec timeout-sec))
         (deadline (+ (float-time) collect-timeout-sec)))
    (while (let ((task (anvil-orchestrator--task-get task-id)))
             (and task
                  (not (memq (plist-get task :status)
                             '(done failed cancelled)))
                  (< (float-time) deadline)))
      (accept-process-output nil poll-interval-sec))
    (let* ((task (anvil-orchestrator--task-get task-id))
           (status (and task (plist-get task :status))))
      (cond
       ((memq status '(done failed cancelled))
        (append (anvil-orchestrator-extract-result task-id full)
                (list :pending nil)))
       (t
        (list :task-id task-id
              :status  (or status 'unknown)
              :pending t))))))

;;;###autoload
(cl-defun anvil-orchestrator-tail (task-id &key stream bytes)
  "Return the last BYTES bytes of task TASK-ID's STREAM file.

STREAM is either `:stdout' / \"stdout\" (default) or
`:stderr' / \"stderr\".  BYTES defaults to 8192 and is clamped
to the file size.  Returns the raw tail string, or nil when
the stream file is missing or empty.  Unlike
`extract-result :full t' this is provider-agnostic: it reads
raw bytes straight off disk without re-running any parser, so
it is the right primitive for watching codex / claude partial
output mid-run or for inspecting stderr after a crash."
  (let* ((task   (anvil-orchestrator--task-get task-id))
         (_      (unless task
                   (user-error "tail: unknown task id %s" task-id)))
         (sym    (cond ((eq stream :stderr) :stderr-path)
                       ((equal stream "stderr") :stderr-path)
                       (t :stdout-path)))
         (path   (plist-get task sym))
         (nbytes (or bytes 8192)))
    (when (and path (file-readable-p path))
      (let ((size (file-attribute-size (file-attributes path))))
        (when (and size (> size 0))
          (with-temp-buffer
            (insert-file-contents path nil
                                  (max 0 (- size nbytes))
                                  size)
            (buffer-string)))))))

(defun anvil-orchestrator--coerce-truthy-string (s)
  "Coerce MCP string S to nil / t using the same rules as tool-collect."
  (and s (not (member s '("" "nil" "false" "0" "no" "False" "NIL")))))

(defun anvil-orchestrator--tool-extract-result (task_id &optional full)
  "MCP wrapper for `anvil-orchestrator-extract-result'.

MCP Parameters:
  task_id - Task id string as returned by orchestrator-submit* tools.
  full    - When truthy (\"t\" / \"true\" / non-empty), bypass
            summary truncation and re-parse the full stdout."
  (anvil-server-with-error-handling
    (anvil-orchestrator-extract-result
     task_id
     (anvil-orchestrator--coerce-truthy-string full))))

(defun anvil-orchestrator--tool-submit-one (provider prompt
                                            &optional model name)
  "MCP wrapper for `anvil-orchestrator-submit-one'.

MCP Parameters:
  provider - Provider name string (e.g. \"claude\", \"codex\",
             \"gemini\", \"aider\", \"ollama\").  Coerced to symbol.
  prompt   - Prompt text string.  Required.
  model    - Optional provider-specific model id override.
  name     - Optional human-readable task label; auto-generated
             from provider + HHMMSS when omitted."
  (anvil-server-with-error-handling
    (unless (and (stringp provider) (not (string-empty-p provider)))
      (user-error "submit-one: provider required"))
    (unless (and (stringp prompt) (not (string-empty-p prompt)))
      (user-error "submit-one: prompt required"))
    (let ((task-id (anvil-orchestrator-submit-one
                    :provider (intern provider)
                    :prompt   prompt
                    :model    (and model (not (string-empty-p model)) model)
                    :name     (and name  (not (string-empty-p name))  name))))
      (list :task-id task-id))))

(defun anvil-orchestrator--tool-submit-and-collect
    (provider prompt
     &optional model name cwd budget_usd timeout_sec
     collect_timeout_sec full)
  "MCP wrapper for `anvil-orchestrator-submit-and-collect'.

MCP Parameters:
  provider - Provider name string (e.g. \"claude\", \"codex\").
  prompt   - Prompt text string.  Required.
  model    - Optional provider-specific model id override.
  name     - Optional human-readable task label.
  cwd      - Optional working directory.
  budget_usd           - Optional per-task budget as a string
                         (numeric).  Empty or nil = use default.
  timeout_sec          - Optional provider kill timeout (string,
                         integer).
  collect_timeout_sec  - Optional wall-clock collect timeout
                         (string, integer, default 180).  When the
                         task is still running at the deadline the
                         wrapper returns :pending t without killing
                         the task.
  full                 - Truthy string (\"t\" / \"true\") asks
                         extract-result to re-parse without
                         summary truncation."
  (anvil-server-with-error-handling
    (unless (and (stringp provider) (not (string-empty-p provider)))
      (user-error "submit-and-collect: provider required"))
    (unless (and (stringp prompt) (not (string-empty-p prompt)))
      (user-error "submit-and-collect: prompt required"))
    (cl-labels ((num (s)
                  (and s (stringp s) (not (string-empty-p s))
                       (let ((n (string-to-number s)))
                         (and (numberp n) (> n 0) n)))))
      (anvil-orchestrator-submit-and-collect
       :provider (intern provider)
       :prompt   prompt
       :model    (and model (not (string-empty-p model)) model)
       :name     (and name  (not (string-empty-p name))  name)
       :cwd      (and cwd   (not (string-empty-p cwd))   cwd)
       :budget-usd          (num budget_usd)
       :timeout-sec         (num timeout_sec)
       :collect-timeout-sec (or (num collect_timeout_sec) 180)
       :full (anvil-orchestrator--coerce-truthy-string full)))))

(defun anvil-orchestrator--tool-tail (task_id &optional stream bytes)
  "MCP wrapper for `anvil-orchestrator-tail'.

MCP Parameters:
  task_id - Task id string.
  stream  - Optional \"stdout\" (default) or \"stderr\".
  bytes   - Optional integer (as string) cap, default 8192."
  (anvil-server-with-error-handling
    (let ((n (or (and bytes
                      (stringp bytes)
                      (not (string-empty-p bytes))
                      (string-to-number bytes))
                 8192)))
      (when (<= n 0) (setq n 8192))
      (list :tail (or (anvil-orchestrator-tail
                       task_id
                       :stream (or stream "stdout")
                       :bytes n)
                      "")))))

;;;; --- dashboard ----------------------------------------------------------

(defvar anvil-orchestrator-dashboard-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g")   #'anvil-orchestrator-dashboard-refresh)
    (define-key m (kbd "RET") #'anvil-orchestrator-dashboard-show-stdout)
    (define-key m (kbd "k")   #'anvil-orchestrator-dashboard-cancel)
    (define-key m (kbd "r")   #'anvil-orchestrator-dashboard-retry)
    m)
  "Keymap for `anvil-orchestrator-dashboard-mode'.")

(define-derived-mode anvil-orchestrator-dashboard-mode tabulated-list-mode
  "OrchDash"
  "Major mode for the anvil-orchestrator dashboard."
  (setq tabulated-list-format
        [("Name"     18 t)
         ("Status"   10 t)
         ("Prov"      8 t)
         ("Model"    10 t)
         ("Elap(ms)" 10 t :right-align t)
         ("Cost"      8 t :right-align t)
         ("Batch"    12 t)
         ("Summary"  40 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun anvil-orchestrator--dashboard-entries ()
  "Return tabulated-list entries from the in-memory task table."
  (let (entries)
    (maphash
     (lambda (id task)
       (let* ((status (format "%s" (or (plist-get task :status) "")))
              (retry-count (plist-get task :retry-count))
              (status-cell (if (and (integerp retry-count) (> retry-count 0))
                               (format "%s×%d" status retry-count)
                             status))
              (row (vector
                    (or (plist-get task :name) "")
                    status-cell
                    (format "%s" (or (plist-get task :provider) ""))
                    (or (plist-get task :model) "")
                    (if (plist-get task :elapsed-ms)
                        (number-to-string (plist-get task :elapsed-ms))
                      "")
                    (if (plist-get task :cost-usd)
                        (format "$%.4f" (plist-get task :cost-usd))
                      "")
                    (substring (or (plist-get task :batch-id) "") 0
                               (min 8 (length (or (plist-get task :batch-id) ""))))
                    (or (plist-get task :summary)
                        (or (plist-get task :error) "")))))
         (push (list id row) entries)))
     anvil-orchestrator--tasks)
    entries))

;;;###autoload
(defun anvil-orchestrator-dashboard ()
  "Open the orchestrator dashboard in a new buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Anvil Orchestrator*")))
    (with-current-buffer buf
      (anvil-orchestrator-dashboard-mode)
      (setq tabulated-list-entries
            (anvil-orchestrator--dashboard-entries))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(defun anvil-orchestrator-dashboard-refresh ()
  "Refresh the orchestrator dashboard."
  (interactive)
  (when (derived-mode-p 'anvil-orchestrator-dashboard-mode)
    (setq tabulated-list-entries
          (anvil-orchestrator--dashboard-entries))
    (tabulated-list-print t)))

(defun anvil-orchestrator-dashboard-show-stdout ()
  "Visit the stdout file of the task at point."
  (interactive)
  (let* ((id   (tabulated-list-get-id))
         (task (anvil-orchestrator--task-get id))
         (path (plist-get task :stdout-path)))
    (cond
     ((null task)  (message "No task at point"))
     ((and path (file-readable-p path)) (find-file-other-window path))
     (t (message "No stdout file for %s" id)))))

(defun anvil-orchestrator-dashboard-cancel ()
  "Cancel the task at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (anvil-orchestrator-cancel id)
      (anvil-orchestrator-dashboard-refresh))))

(defun anvil-orchestrator-dashboard-retry ()
  "Retry the task at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (anvil-orchestrator-retry id)
      (anvil-orchestrator-dashboard-refresh))))

;;;; --- lifecycle ----------------------------------------------------------

(defun anvil-orchestrator--register-tools ()
  "Register orchestrator-* MCP tools under the emacs-eval server."
  (anvil-server-register-tool
   #'anvil-orchestrator--tool-submit
   :id "orchestrator-submit"
   :server-id anvil-orchestrator--server-id
   :description
   "Submit a batch of AI CLI tasks (claude today, more providers
coming) and return a batch id.  Tasks run asynchronously in a
pool with global and per-provider concurrency caps; poll via
`orchestrator-status' or `orchestrator-collect'.  Designed to
keep the parent session's context small — only task ids and
final summaries (<= `anvil-orchestrator-summary-max-chars',
default 4000) come back by default.")

  (anvil-server-register-tool
   #'anvil-orchestrator--tool-status
   :id "orchestrator-status"
   :server-id anvil-orchestrator--server-id
   :description
   "Return the status of a batch or a single task.  For a batch:
counts and slim task summaries (no full stdout / prompt)."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-orchestrator--tool-collect
   :id "orchestrator-collect"
   :server-id anvil-orchestrator--server-id
   :description
   "Return the slim task plist list for a batch.  Pass wait=\"t\" to
block until every task reaches a terminal state (done / failed
/ cancelled)."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-orchestrator--tool-cancel
   :id "orchestrator-cancel"
   :server-id anvil-orchestrator--server-id
   :description
   "Cancel a running or queued task (SIGTERM, then SIGKILL after
2s grace).  Already terminal tasks are left alone.")

  (anvil-server-register-tool
   #'anvil-orchestrator--tool-retry
   :id "orchestrator-retry"
   :server-id anvil-orchestrator--server-id
   :description
   "Re-submit a task under a new id (same batch, same prompt /
provider / model).  Useful after a manual cancel or a
non-auto-retryable failure.")

  (anvil-server-register-tool
   #'anvil-orchestrator--tool-consensus-submit
   :id "orchestrator-consensus-submit"
   :server-id anvil-orchestrator--server-id
   :description
   "Dispatch the same prompt to >= 2 providers (cross-model
consensus).  Returns a consensus id + batch id + task ids.  Poll
via `orchestrator-consensus-collect' to get a verdict (unanimous
/ divergent) based on pairwise Jaccard similarity of summaries.")

  (anvil-server-register-tool
   #'anvil-orchestrator--tool-consensus-collect
   :id "orchestrator-consensus-collect"
   :server-id anvil-orchestrator--server-id
   :description
   "Return the consensus verdict for a consensus id.  Includes
:verdict (unanimous / divergent), :min-similarity, pairwise
:matrix, and the per-task slim plists.  Pass wait=\"t\" to block
until every fan-out task reaches a terminal state."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-orchestrator--tool-consensus-judge
   :id "orchestrator-consensus-judge"
   :server-id anvil-orchestrator--server-id
   :description
   "Run a meta-LLM judge (Phase 4b) over a consensus batch.  The
fan-out batch must be terminal (or pass wait=\"t\" to block
first).  The judge prompt is built from the original question +
each provider's summary and submitted as a single new task.
Returns the judge-task-id; poll with
`orchestrator-consensus-judge-collect'.")

  (anvil-server-register-tool
   #'anvil-orchestrator--tool-consensus-judge-collect
   :id "orchestrator-consensus-judge-collect"
   :server-id anvil-orchestrator--server-id
   :description
   "Return the judge task summary for a consensus id.  Pass
wait=\"t\" to block until the judge task reaches a terminal
state."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-orchestrator--tool-stats
   :id "orchestrator-stats"
   :server-id anvil-orchestrator--server-id
   :description
   "Return aggregated task statistics from the in-memory task table:
totals, per-status counts, per-provider breakdown, elapsed-ms avg/
p50/p95, and total cost-usd sum.  Read-only snapshot."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-orchestrator--tool-extract-result
   :id "orchestrator-extract-result"
   :server-id anvil-orchestrator--server-id
   :description
   "Return the minimum result plist for a task — summary, cost,
tokens, exit code, error — without exposing the raw prompt or
paths.  Pass full=\"t\" to bypass summary truncation and
re-parse the full agent message from stdout.  Replaces the old
three-tool glue dance (Read stdout-path + python3 json extract
+ Edit) with a single call."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-orchestrator--tool-submit-one
   :id "orchestrator-submit-one"
   :server-id anvil-orchestrator--server-id
   :description
   "Submit a single task and return its task-id.  A convenience
wrapper over orchestrator-submit for one-off prompts; identical
pool / retry / budget semantics.  Use orchestrator-submit for
DAGs, consensus, or multi-task batches.")

  (anvil-server-register-tool
   #'anvil-orchestrator--tool-submit-and-collect
   :id "orchestrator-submit-and-collect"
   :server-id anvil-orchestrator--server-id
   :description
   "Submit a single task and block until it finishes (or
collect_timeout_sec elapses — default 180).  Returns the
extract-result plist (:summary / :cost-usd / :status / ...) when
the task reaches a terminal state.  On timeout the task is kept
alive and the wrapper returns :pending t + :task-id, so a later
orchestrator-extract-result picks it up.  Phase 7a: replaces the
submit-one / poll / tail / extract-result 4-hop chain with a
single call for short-running tasks.")

  (anvil-server-register-tool
   #'anvil-orchestrator--tool-tail
   :id "orchestrator-tail"
   :server-id anvil-orchestrator--server-id
   :description
   "Return the last N bytes of a task's stdout or stderr stream.
Provider-agnostic raw-byte tail: no parsing, no truncation at
summary granularity.  Use this to watch in-flight codex /
claude partial output, or to inspect stderr after a failure."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-orchestrator--tool-list-interrupted
   :id "orchestrator-list-interrupted"
   :server-id anvil-orchestrator--server-id
   :description
   "List tasks orphaned by a daemon restart (status=failed,
interrupted=t, not yet resumed).  Optional batch_id filter.
Phase 6C'' DAG resume diagnostic."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-orchestrator--tool-resume-interrupted
   :id "orchestrator-resume-interrupted"
   :server-id anvil-orchestrator--server-id
   :description
   "Re-queue every interrupted (daemon-restart orphan) task in
place, preserving task-ids + :depends-on so downstream work
reconnects to resumed upstream.  Optional batch_id scope.
Returns counts of immediately-DAG-ready vs dep-blocked ids.
Phase 6C'' DAG resume."))

(defun anvil-orchestrator--unregister-tools ()
  (dolist (id '("orchestrator-submit" "orchestrator-status"
                "orchestrator-collect" "orchestrator-cancel"
                "orchestrator-retry"
                "orchestrator-consensus-submit"
                "orchestrator-consensus-collect"
                "orchestrator-consensus-judge"
                "orchestrator-consensus-judge-collect"
                "orchestrator-stats"
                "orchestrator-extract-result"
                "orchestrator-submit-one"
                "orchestrator-submit-and-collect"
                "orchestrator-tail"
                "orchestrator-list-interrupted"
                "orchestrator-resume-interrupted"))
    (anvil-server-unregister-tool id anvil-orchestrator--server-id)))

;;;###autoload
(defun anvil-orchestrator-enable ()
  "Enable the orchestrator module: register MCP tools, rehydrate state."
  (interactive)
  (anvil-state-enable)
  (anvil-orchestrator--restore-from-state)
  (anvil-orchestrator--restore-consensus-from-state)
  (anvil-orchestrator--register-tools)
  (anvil-orchestrator--ensure-pump-timer))

(defun anvil-orchestrator-disable ()
  "Disable the module: unregister tools, cancel the pump timer."
  (interactive)
  (anvil-orchestrator--cancel-pump-timer)
  (anvil-orchestrator--unregister-tools))

(provide 'anvil-orchestrator)
;;; anvil-orchestrator.el ends here
