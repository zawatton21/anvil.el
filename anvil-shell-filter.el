;;; anvil-shell-filter.el --- Per-command shell output compression + tee  -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 27 Phase 1 — per-command output filters + tee to `anvil-state'.
;;
;; Motivation (rtk benchmark): a 30-min Claude Code session often drops
;; 118k → 24k tokens (-80%) when verbose shell output is compressed
;; per-command before reaching the LLM.  Raw stdout for `git status',
;; `git diff', `rg', and friends is mostly boilerplate; a dedicated
;; filter per command can collapse it 5-10x while preserving the
;; signal the LLM actually consumes.
;;
;; Strategy (4 buckets from rtk README):
;;   * Smart filtering  — drop noise (Loading/Compiling/gcs-done)
;;   * Grouping         — collapse by file / directory
;;   * Truncation       — cap per-file and per-hunk repetition
;;   * Deduplication    — (Phase 2) collapse identical log lines
;;
;; Public Elisp API:
;;   (anvil-shell-filter-apply NAME RAW)        — pure string transform
;;   (anvil-shell-filter-lookup CMD-STRING)     — first-token → tag
;;   (anvil-shell-filter-run CMD &rest OPTS)    — shell + filter + tee
;;   (anvil-shell-filter-tee-get TEE-ID)        — retrieve raw text
;;   (anvil-shell-filter-gain &optional DAYS)   — summary plist
;;
;; MCP tools (emacs-eval server):
;;   shell-run       — run a shell command, filter + tee the output
;;   shell-filter    — apply a filter to a string, no shell call
;;   shell-tee-get   — fetch raw text by tee-id
;;   shell-gain      — cumulative savings summary (last N days)
;;
;; A bounded raw prefix is stored under the `shell-tee' namespace with a
;; TTL so the LLM can inspect captured bytes when compression hides something
;; material.  The result's byte count and truncation flag disclose omissions.  `anvil-state'
;; handles TTL pruning; no separate sweep is required.
;;
;; The module lives in `anvil-optional-modules' because it depends on
;; `anvil-state' (Emacs 29+ SQLite) and adds several user-facing MCP
;; tools that should not be advertised to every session by default.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'anvil-server)
(require 'anvil-state)
(require 'anvil-host)


;;;; --- group + defcustoms -------------------------------------------------

(defgroup anvil-shell-filter nil
  "Per-command shell output compression for anvil."
  :group 'anvil
  :prefix "anvil-shell-filter-")

(defconst anvil-shell-filter--server-id "emacs-eval"
  "Server id under which shell-* MCP tools register.")

(defconst anvil-shell-filter--tee-ns "shell-tee"
  "`anvil-state' namespace storing raw tee blobs.")

(defconst anvil-shell-filter--gain-ns "shell-gain"
  "`anvil-state' namespace holding per-day gain entries.")

(defconst anvil-shell-filter--trace-ns "shell-trace"
  "`anvil-state' namespace holding per-phase trace events for
`anvil-shell-filter-run' invocations.  Populated only when
`anvil-shell-filter-trace-events' is non-nil.")

(defconst anvil-shell-filter-supported
  '(git-status git-log git-diff rg find ls pytest ert-batch
               emacs-batch make dispatch tee gain
               gh git-log-graph pip-install npm-install
               docker-ps docker-logs kubectl-get aws-s3-ls
               prettier ruff phase2a-dispatch tee-grep
               register taco-critical)
  "Capability tags this module currently provides.
Tests in tests/anvil-shell-filter-test.el gate their `skip-unless'
on membership here so a half-shipped filter never breaks CI.  The
pseudo-tags `dispatch', `tee', `gain' describe infrastructure
behaviours rather than concrete command names — they live on the
same list so each test can self-describe its capability gate.")

(defcustom anvil-shell-tee-ttl-sec 3600
  "Seconds raw output is retained under the shell-tee namespace.
`anvil-state' prunes expired rows lazily on `GET' and eagerly on
`anvil-state-vacuum'; callers do not need to sweep."
  :type 'integer
  :group 'anvil-shell-filter)

(defcustom anvil-shell-tee-max-bytes (* 4 1024 1024)
  "Maximum stdout bytes captured and stored per shell-run invocation.
Larger output is discarded at the process filter before decoding, then
represented by a bounded marker in `anvil-state'.  The marker reports the
exact omitted byte count whenever the cap can hold it; the smallest caps use
an ASCII ellipsis.  This caps both root-Emacs memory and database growth for
noisy commands.  Set to nil to
allow explicitly unbounded capture.  For genuinely large output, redirect to
a file instead."
  :type '(choice integer (const :tag "No cap" nil))
  :group 'anvil-shell-filter)

(defcustom anvil-shell-filter-trace-events nil
  "When non-nil, emit per-phase trace rows for each
`anvil-shell-filter-run' invocation into `anvil-state' under
namespace `shell-trace'.

Each call produces five rows keyed by a unique trace id —
`<id>-start', `<id>-exec-done', `<id>-filter-done',
`<id>-tee-done', `<id>-return' — recording phase name, absolute
Unix timestamp, and elapsed milliseconds since the run start.
TTL = `anvil-shell-trace-ttl-sec'.

Opt-in observability.  When nil the run path adds zero state
writes (zero-cost).  Inspect by querying
  SELECT k, v FROM kv WHERE ns = \\='shell-trace\\='
during or after a suspected hang to learn which phase blocked."
  :type 'boolean
  :group 'anvil-shell-filter)

(defcustom anvil-shell-trace-ttl-sec 600
  "TTL (seconds) for trace events emitted when
`anvil-shell-filter-trace-events' is non-nil.  Defaults to 10
minutes — long enough to inspect a recent hang, short enough
that traces do not accumulate."
  :type 'integer
  :group 'anvil-shell-filter)

(defcustom anvil-shell-filter-rg-matches-per-file 3
  "Max matches per file retained by the rg filter before `(N more)'."
  :type 'integer
  :group 'anvil-shell-filter)

(defcustom anvil-shell-filter-diff-hunks 3
  "Max hunks retained by the git-diff filter before the omitted footer."
  :type 'integer
  :group 'anvil-shell-filter)

(defcustom anvil-shell-filter-ls-threshold 10
  "ls filter collapses to a count summary when line count exceeds this."
  :type 'integer
  :group 'anvil-shell-filter)

(defcustom anvil-shell-filter-default-timeout 60
  "Timeout (seconds) for `anvil-shell-filter-run' shell invocations."
  :type 'integer
  :group 'anvil-shell-filter)

(defcustom anvil-shell-filter-max-sync-timeout 120
  "Maximum timeout accepted by synchronous shell MCP tools.

Longer work must use native execution or external asynchronous evaluation
so the root event loop remains inside its watchdog and client envelopes."
  :type 'integer
  :group 'anvil-shell-filter)

(defcustom anvil-shell-tee-grep-default-max-line-bytes 200
  "Per-line truncation cap used by `anvil-shell-filter-tee-grep'.
Lines longer than this are wrapped with a `…(N bytes elided)' sentinel
so the compressed output stays predictable for downstream parsers."
  :type 'integer
  :group 'anvil-shell-filter)

(defcustom anvil-shell-tee-grep-default-tail-fallback 50
  "When an `anvil-shell-filter-tee-grep' regex matches zero lines, return
the last N lines of stdout as a fallback rather than an empty result.
Set to 0 to disable the fallback."
  :type 'integer
  :group 'anvil-shell-filter)

(defcustom anvil-shell-filter-tokens-per-char 0.25
  "Approximate tokens-per-character ratio used for gain summaries.
rtk and OpenAI both publish ~4 chars-per-token for English/code,
so the default 0.25 tracks that.  Purely an advisory number — the
byte counts are exact."
  :type 'number
  :group 'anvil-shell-filter)

(defcustom anvil-shell-filter-taco-critical-fallback t
  "When non-nil, unknown `shell-run' commands get a safe critical keeper.
This is Doc 45 / TACO Phase 1: known static filters still win, but
unknown verbose commands with error-like lines are compressed to the
critical lines plus small context.  Commands without critical signals
remain raw passthrough."
  :type 'boolean
  :group 'anvil-shell-filter)

(defcustom anvil-shell-filter-taco-critical-min-lines 80
  "Minimum stdout line count before TACO critical fallback can compress.
Non-zero exits bypass this threshold because their tail is often useful."
  :type 'integer
  :group 'anvil-shell-filter)

(defcustom anvil-shell-filter-taco-critical-context-lines 2
  "Number of surrounding stdout lines kept around each critical line."
  :type 'integer
  :group 'anvil-shell-filter)

(defcustom anvil-shell-filter-taco-critical-tail-lines 20
  "Number of final stdout lines kept for unknown commands that exit non-zero."
  :type 'integer
  :group 'anvil-shell-filter)

(defcustom anvil-shell-filter-taco-critical-patterns
  '("\\berror\\b"
    "\\bfatal:"
    "\\bfailed\\b"
    "Traceback"
    "Exception"
    "AssertionError"
    "panic"
    "segmentation fault"
    "^E[[:space:]]+"
    "^[[:space:]]*at .*(.*:[0-9]+)")
  "Case-insensitive regexps whose matching stdout lines are always kept.
These patterns are intentionally conservative.  `shell-run' still tees its
bounded raw prefix and reports whether the complete output was truncated."
  :type '(repeat regexp)
  :group 'anvil-shell-filter)


;;;; --- pure per-command filters -------------------------------------------

(defun anvil-shell-filter--git-status (raw)
  "Compress `git status --short --branch' RAW to a one-line summary.
Output shape: \"branch:NAME[ +AHEAD][ -BEHIND] CODE:N CODE:N ...\".
Status codes are the 2-char porcelain prefix (` M' → `M', `??'
stays `??') so the summary is grep-friendly."
  (let* ((lines (split-string raw "\n" t))
         (branch-line (car lines))
         (rest (cdr lines))
         (branch "?")
         (ahead 0) (behind 0)
         (counts (make-hash-table :test 'equal))
         (order nil))
    (when (and branch-line (string-prefix-p "## " branch-line))
      (let* ((tail (substring branch-line 3))
             (dots (string-match "\\.\\.\\." tail))
             (brack (string-match " \\[" tail))
             (end (cond ((and dots brack) (min dots brack))
                        (dots dots) (brack brack)
                        (t (length tail)))))
        (setq branch (string-trim (substring tail 0 end))))
      (save-match-data
        (when (string-match "\\[ahead \\([0-9]+\\)" branch-line)
          (setq ahead (string-to-number (match-string 1 branch-line))))
        (when (string-match "behind \\([0-9]+\\)" branch-line)
          (setq behind (string-to-number (match-string 1 branch-line))))))
    (dolist (line rest)
      (when (>= (length line) 2)
        (let ((code (string-trim (substring line 0 2))))
          (unless (string-empty-p code)
            (unless (gethash code counts)
              (push code order))
            (puthash code (1+ (gethash code counts 0)) counts)))))
    (let (parts)
      (push (format "branch:%s" branch) parts)
      (when (> ahead 0)  (push (format "+%d" ahead) parts))
      (when (> behind 0) (push (format "-%d" behind) parts))
      (dolist (code (nreverse order))
        (push (format "%s:%d" code (gethash code counts)) parts))
      (string-join (nreverse parts) " "))))

(defun anvil-shell-filter--git-log (raw)
  "Compress verbose `git log' RAW to `HASH SUBJECT' lines (7-char hashes)."
  (let ((lines (split-string raw "\n"))
        results)
    (while lines
      (let ((line (car lines)))
        (if (string-match "^commit \\([a-f0-9]+\\)" line)
            (let* ((hash (substring (match-string 1 line)
                                    0 (min 7 (length (match-string 1 line)))))
                   (subject nil))
              (setq lines (cdr lines))
              (while (and lines (not subject))
                (if (string-match "^    \\(.*\\)" (car lines))
                    (setq subject (match-string 1 (car lines)))
                  (setq lines (cdr lines))))
              (when subject
                (push (format "%s %s" hash subject) results))
              (when lines (setq lines (cdr lines))))
          (setq lines (cdr lines)))))
    (string-join (nreverse results) "\n")))

(defun anvil-shell-filter--git-diff (raw)
  "Compress unified-diff RAW: keep ≤`anvil-shell-filter-diff-hunks' hunks.
Remaining hunks are replaced by a `[... N more hunks omitted]'
footer.  File-header lines (diff / --- / +++ / index) are always
retained so the diff remains contextually addressable."
  (let* ((lines (split-string raw "\n"))
         (cap anvil-shell-filter-diff-hunks)
         (hunks 0)
         output in-overflow)
    (dolist (line lines)
      (cond
       ((string-prefix-p "@@ " line)
        (setq hunks (1+ hunks))
        (setq in-overflow (> hunks cap))
        (unless in-overflow (push line output)))
       ((or (string-prefix-p "diff --git" line)
            (string-prefix-p "index " line)
            (string-prefix-p "--- " line)
            (string-prefix-p "+++ " line))
        (setq in-overflow nil)
        (push line output))
       (t (unless in-overflow (push line output)))))
    (when (> hunks cap)
      (push (format "[... %d more hunks omitted]" (- hunks cap)) output))
    (string-join (nreverse output) "\n")))

(defun anvil-shell-filter--rg (raw)
  "Group rg RAW by filename; keep first N matches per file, suffix `(M more)'."
  (let ((lines (split-string raw "\n" t))
        (cap anvil-shell-filter-rg-matches-per-file)
        (groups (make-hash-table :test 'equal))
        order)
    (dolist (line lines)
      (when (string-match "\\`\\([^:\n]+\\):" line)
        (let ((path (match-string 1 line)))
          (unless (gethash path groups)
            (push path order))
          (puthash path (cons line (gethash path groups)) groups))))
    (let (output)
      (dolist (path (nreverse order))
        (let* ((matches (nreverse (gethash path groups)))
               (total (length matches))
               (shown (seq-take matches cap))
               (rest (- total cap)))
          (push (string-join shown "\n") output)
          (when (> rest 0)
            (push (format "  (%d more in %s)" rest path) output))))
      (string-join (nreverse output) "\n"))))

(defun anvil-shell-filter--find (raw)
  "Compress find RAW to per-directory counts."
  (let ((lines (split-string raw "\n" t))
        (groups (make-hash-table :test 'equal))
        order)
    (dolist (line lines)
      (let ((dir (or (file-name-directory line) "./")))
        (unless (gethash dir groups)
          (push dir order))
        (puthash dir (1+ (gethash dir groups 0)) groups)))
    (let (output)
      (dolist (dir (nreverse order))
        (push (format "%s: %d" dir (gethash dir groups)) output))
      (string-join (nreverse output) "\n"))))

(defun anvil-shell-filter--ls (raw)
  "Collapse ls RAW to an `N entries' line once the threshold is crossed."
  (let* ((lines (split-string raw "\n" t))
         (count (length lines)))
    (if (> count anvil-shell-filter-ls-threshold)
        (format "%d entries" count)
      raw)))

(defun anvil-shell-filter--pytest (raw)
  "Compress pytest RAW: pass-only keeps summary; failures keep FAILURES block."
  (let ((lines (split-string raw "\n"))
        output
        (in-section nil))
    (dolist (line lines)
      (cond
       ((string-match-p "=\\{3,\\}.*\\(FAILURES\\|ERRORS\\).*=\\{3,\\}" line)
        (setq in-section t)
        (push line output))
       ((and in-section
             (string-match-p "=\\{3,\\}.*[0-9]+ \\(passed\\|failed\\|error\\)" line))
        (setq in-section nil)
        (push line output))
       (in-section
        (push line output))
       ((string-match-p " FAILED\\|=\\{3,\\}.*[0-9]+ \\(passed\\|failed\\|error\\)" line)
        (push line output))))
    (string-join (nreverse output) "\n")))

(defun anvil-shell-filter--ert-batch (raw)
  "Compress ERT batch RAW: keep FAILED blocks and the final `Ran N tests' line."
  (let ((lines (split-string raw "\n"))
        output
        (keep-block nil))
    (dolist (line lines)
      (cond
       ((or (string-match-p "^\\s-*FAILED " line)
            (string-match-p "^Test .* condition:" line))
        (setq keep-block t)
        (push line output))
       ((and keep-block
             (or (string-empty-p line)
                 (string-prefix-p "Ran " line)))
        (setq keep-block nil)
        (push line output))
       (keep-block
        (push line output))
       ((string-match-p "^Ran [0-9]+ tests" line)
        (push line output))))
    (string-join (nreverse output) "\n")))

(defun anvil-shell-filter--emacs-batch (raw)
  "Drop `Compiling .../done', `Loading ...', and `gcs-done' lines."
  (let ((lines (split-string raw "\n"))
        output)
    (dolist (line lines)
      (unless (or (string-match-p "\\`Compiling .*\\.\\.\\.\\(done\\)?\\'" line)
                  (string-match-p "\\`Loading " line)
                  (string-match-p "\\`gcs-done\\'" line))
        (push line output)))
    (string-join (nreverse output) "\n")))

(defun anvil-shell-filter--make (raw)
  "Keep only `warning', `error', and `make: ***' lines from make RAW."
  (let ((lines (split-string raw "\n"))
        output)
    (dolist (line lines)
      (when (or (string-match-p "\\b\\(warning\\|error\\|Error\\|FAIL\\|Fatal\\)\\b" line)
                (string-match-p "\\`make\\(\\[[0-9]+\\]\\)?: \\*\\*\\*" line))
        (push line output)))
    (string-join (nreverse output) "\n")))


;;;; --- Phase 2a filters ---------------------------------------------------

(defun anvil-shell-filter--gh (raw)
  "Reduce `gh pr list' / `gh issue list' RAW to `#NUM TITLE' oneliners."
  (let ((lines (split-string raw "\n"))
        output)
    (dolist (line lines)
      (cond
       ((string-match "^\\(#[0-9]+\\)\\s-+\\(\\S-.*?\\)\\s-\\{2,\\}" line)
        (push (format "%s %s" (match-string 1 line)
                      (string-trim-right (match-string 2 line)))
              output))
       ((string-match-p "^\\(error\\|Error\\|gh: \\)" line)
        (push line output))))
    (string-join (nreverse output) "\n")))

(defun anvil-shell-filter--git-log-graph (raw)
  "Compress `git log --graph' RAW to `<graph> <hash> <subject>' lines.
Preserves the leading tree characters so the merge topology is
still visible; hashes are truncated to 7 chars and Author / Date /
blank commit metadata is dropped."
  (let ((lines (split-string raw "\n"))
        results)
    (while lines
      (let ((line (car lines)))
        (if (string-match
             "^\\(\\(?:[*|/\\\\ ]\\)+\\)commit \\([a-f0-9]+\\)" line)
            (let* ((graph (match-string 1 line))
                   (hash-full (match-string 2 line))
                   (hash (substring hash-full 0 (min 7 (length hash-full))))
                   subject)
              (setq lines (cdr lines))
              (while (and lines (not subject))
                (if (string-match "^[*|/\\\\ ]*    \\(.+\\)" (car lines))
                    (setq subject (match-string 1 (car lines)))
                  (setq lines (cdr lines))))
              (when subject
                (push (format "%s%s %s" graph hash subject) results))
              (when lines (setq lines (cdr lines))))
          (setq lines (cdr lines)))))
    (string-join (nreverse results) "\n")))

(defun anvil-shell-filter--pip-install (raw)
  "Drop pip progress / cache noise.
Keeps `Successfully installed' plus ERROR / WARNING lines."
  (let ((lines (split-string raw "\n"))
        output)
    (dolist (line lines)
      (when (or (string-match-p "^Successfully installed\\|^ERROR:\\|^WARNING:" line)
                (string-match-p "\\bFATAL\\b\\|not found\\|failed" line))
        (push line output)))
    (string-join (nreverse output) "\n")))

(defun anvil-shell-filter--npm-install (raw)
  "Keep npm summary (added / removed / changed / audited / vulnerabilities)
and any `npm ERR' / `npm WARN' lines; drop http / notice / progress."
  (let ((lines (split-string raw "\n"))
        output)
    (dolist (line lines)
      (when (or (string-match-p "^\\(added\\|removed\\|changed\\) [0-9]+" line)
                (string-match-p "\\baudited [0-9]+" line)
                (string-match-p "\\bvulnerabilit\\(y\\|ies\\)" line)
                (string-match-p "^npm ERR\\|^npm WARN" line))
        (push line output)))
    (string-join (nreverse output) "\n")))

(defun anvil-shell-filter--docker-ps (raw)
  "Collapse `docker ps' rows to a count when they exceed the ls threshold.
Keeps the header row so callers can still see column semantics."
  (let* ((lines (split-string raw "\n" t))
         (header (car lines))
         (rows (cdr lines))
         (n (length rows)))
    (if (> n anvil-shell-filter-ls-threshold)
        (format "%s\n... %d containers" header n)
      raw)))

(defun anvil-shell-filter--docker-logs (raw)
  "Collapse consecutive duplicate `docker logs' lines (ignoring timestamp).
Lines that differ only by their leading ISO-8601 timestamp are
merged and the count is emitted as `... (xN)' on the first
occurrence's original line."
  (let ((lines (split-string raw "\n" t))
        output
        (prev-norm nil)
        (prev-raw nil)
        (count 0))
    (cl-labels
        ((strip-ts (s)
                   (if (string-match "^[-0-9T:.+]+\\s-+\\(.*\\)" s)
                       (match-string 1 s)
                     s))
         (emit ()
               (when prev-raw
                 (push (if (> count 1)
                           (format "%s (x%d)" prev-raw count)
                         prev-raw)
                       output))))
      (dolist (line lines)
        (let ((norm (strip-ts line)))
          (if (equal norm prev-norm)
              (setq count (1+ count))
            (emit)
            (setq prev-norm norm prev-raw line count 1))))
      (emit))
    (string-join (nreverse output) "\n")))

(defun anvil-shell-filter--kubectl-get (raw)
  "Collapse `kubectl get' rows to a count when rows exceed the ls threshold."
  (let* ((lines (split-string raw "\n" t))
         (header (car lines))
         (rows (cdr lines))
         (n (length rows)))
    (if (> n anvil-shell-filter-ls-threshold)
        (format "%s\n... %d resources" header n)
      raw)))

(defun anvil-shell-filter--aws-s3-ls (raw)
  "Collapse `aws s3 ls' output to a count when entries exceed the ls threshold."
  (let* ((lines (split-string raw "\n" t))
         (n (length lines)))
    (if (> n anvil-shell-filter-ls-threshold)
        (format "... %d s3 entries" n)
      raw)))

(defun anvil-shell-filter--prettier (raw)
  "Keep `[error]' / `[warn]' / `SyntaxError' / `Error:' lines from prettier RAW."
  (let ((lines (split-string raw "\n"))
        output)
    (dolist (line lines)
      (when (string-match-p "\\[error\\]\\|\\[warn\\]\\|SyntaxError\\|Error:" line)
        (push line output)))
    (string-join (nreverse output) "\n")))

(defun anvil-shell-filter--ruff (raw)
  "Group ruff violations by rule code; cap 3 occurrences per code + `(N more)'.
The trailing `Found N errors.' summary is preserved when present."
  (let ((lines (split-string raw "\n"))
        (groups (make-hash-table :test 'equal))
        order summary)
    (dolist (line lines)
      (cond
       ((string-match "^Found [0-9]+ error" line)
        (setq summary line))
       ((string-match ": \\([A-Z][0-9]+\\)\\b" line)
        (let ((code (match-string 1 line)))
          (unless (gethash code groups)
            (push code order))
          (puthash code (cons line (gethash code groups)) groups)))))
    (let (output)
      (dolist (code (nreverse order))
        (let* ((matches (nreverse (gethash code groups)))
               (shown (seq-take matches 3))
               (rest (- (length matches) 3)))
          (push (string-join shown "\n") output)
          (when (> rest 0)
            (push (format "  (%d more %s)" rest code) output))))
      (when summary (push summary output))
      (string-join (nreverse output) "\n"))))

(defun anvil-shell-filter--taco-critical-line-p (line)
  "Return non-nil when LINE matches a configured critical pattern."
  (let ((case-fold-search t))
    (cl-some (lambda (re) (string-match-p re line))
             anvil-shell-filter-taco-critical-patterns)))

(defun anvil-shell-filter--taco-mark-range (keep start end)
  "Mark KEEP vector indexes from START through END, clamped to bounds."
  (let ((i (max 0 start))
        (last (min (1- (length keep)) end)))
    (while (<= i last)
      (aset keep i t)
      (setq i (1+ i)))))

(defun anvil-shell-filter--taco-critical-render (raw &optional stderr exit)
  "Return TACO Phase 1 critical-keeper output for RAW.
STDERR is not embedded because `shell-run' returns it separately; it
only affects the header.  EXIT controls non-zero tail retention."
  (let* ((lines (split-string (or raw "") "\n"))
         (line-count (length lines))
         (context (max 0 anvil-shell-filter-taco-critical-context-lines))
         (tail (max 0 anvil-shell-filter-taco-critical-tail-lines))
         (nonzero (and exit (not (zerop exit))))
         (keep (make-vector line-count nil))
         (critical-count 0))
    (cl-loop for line in lines
             for i from 0
             when (anvil-shell-filter--taco-critical-line-p line)
             do (setq critical-count (1+ critical-count))
             and do (anvil-shell-filter--taco-mark-range
                     keep (- i context) (+ i context)))
    (when nonzero
      (anvil-shell-filter--taco-mark-range
       keep (- line-count tail) (1- line-count)))
    (let ((kept (cl-loop for i below line-count count (aref keep i)))
          (out nil)
          (gap 0))
      (push (format "[anvil-taco: kept %d/%d stdout lines; critical=%d%s%s]"
                    kept line-count critical-count
                    (if nonzero (format "; exit=%s" exit) "")
                    (if (and stderr (not (string-empty-p stderr)))
                        "; stderr returned separately"
                      ""))
            out)
      (cl-loop for line in lines
               for i from 0
               do (if (aref keep i)
                      (progn
                        (when (> gap 0)
                          (push (format "...[%d stdout lines omitted]" gap)
                                out)
                          (setq gap 0))
                        (push line out))
                    (setq gap (1+ gap))))
      (when (> gap 0)
        (push (format "...[%d stdout lines omitted]" gap) out))
      (string-join (nreverse out) "\n"))))

(defun anvil-shell-filter--taco-critical-maybe (raw &optional stderr exit)
  "Return critical-keeper output for RAW, or nil when passthrough is safer."
  (when (and anvil-shell-filter-taco-critical-fallback
             (stringp raw)
             (not (string-empty-p raw)))
    (let* ((lines (split-string raw "\n"))
           (line-count (length lines))
           (nonzero (and exit (not (zerop exit))))
           (has-critical
            (cl-some #'anvil-shell-filter--taco-critical-line-p lines)))
      (when (and (or nonzero
                     (>= line-count anvil-shell-filter-taco-critical-min-lines))
                 (or has-critical nonzero))
        (let ((compressed
               (anvil-shell-filter--taco-critical-render raw stderr exit)))
          (when (< (length compressed) (length raw))
            compressed))))))

(defun anvil-shell-filter--taco-critical (raw)
  "Apply TACO Phase 1 critical keeping to RAW, or return RAW unchanged."
  (or (anvil-shell-filter--taco-critical-maybe raw nil nil)
      raw))


;;;; --- dispatch / lookup --------------------------------------------------

(defvar anvil-shell-filter-handlers
  `((git-status     . ,#'anvil-shell-filter--git-status)
    (git-log        . ,#'anvil-shell-filter--git-log)
    (git-log-graph  . ,#'anvil-shell-filter--git-log-graph)
    (git-diff       . ,#'anvil-shell-filter--git-diff)
    (rg             . ,#'anvil-shell-filter--rg)
    (find           . ,#'anvil-shell-filter--find)
    (ls             . ,#'anvil-shell-filter--ls)
    (pytest         . ,#'anvil-shell-filter--pytest)
    (ert-batch      . ,#'anvil-shell-filter--ert-batch)
    (emacs-batch    . ,#'anvil-shell-filter--emacs-batch)
    (make           . ,#'anvil-shell-filter--make)
    (gh             . ,#'anvil-shell-filter--gh)
    (pip-install    . ,#'anvil-shell-filter--pip-install)
    (npm-install    . ,#'anvil-shell-filter--npm-install)
    (docker-ps      . ,#'anvil-shell-filter--docker-ps)
    (docker-logs    . ,#'anvil-shell-filter--docker-logs)
    (kubectl-get    . ,#'anvil-shell-filter--kubectl-get)
    (aws-s3-ls      . ,#'anvil-shell-filter--aws-s3-ls)
    (prettier       . ,#'anvil-shell-filter--prettier)
    (ruff           . ,#'anvil-shell-filter--ruff)
    (taco-critical  . ,#'anvil-shell-filter--taco-critical))
  "Alist mapping filter tag → pure `(RAW) -> COMPRESSED' function.
Phase 3 adds `anvil-shell-filter-register' as the public way to install
additional declarative filters; the alist is still publicly readable
for legacy callers that walked the entries directly.")

(defvar anvil-shell-filter--match-command-table nil
  "Alist of (REGEX . TAG) populated by `anvil-shell-filter-register'.
Consulted by `anvil-shell-filter-lookup' as a fallback when the
hand-written cond-tree returns nil.  Order = registration order;
first match wins so later registrations shadow earlier ones with
the same regex.  Each `anvil-shell-filter-register' call removes
any prior cell whose cdr equals TAG before pushing the fresh entry,
so re-registering a tag never leaves orphan rows.")

(defun anvil-shell-filter-lookup (cmd)
  "Return the filter tag for the first token(s) of shell command CMD, or nil.
Multi-token commands (`git SUB', `docker SUB', `kubectl SUB',
`aws SUB SUB2') dispatch on the relevant sub-command so each
variant lands on its own filter; `git log --graph' upgrades to
`git-log-graph'.  Unknown commands return nil and the caller
is expected to fall through to raw passthrough."
  (when (and cmd (stringp cmd))
    (let* ((tokens (split-string (string-trim cmd) "\\s-+" t))
           (first (car tokens))
           (second (cadr tokens))
           (third (nth 2 tokens)))
      (cond
       ((equal first "git")
        (cond
         ((and (equal second "log") (member "--graph" tokens)) 'git-log-graph)
         ((equal second "status") 'git-status)
         ((equal second "log")    'git-log)
         ((equal second "diff")   'git-diff)
         (t nil)))
       ((equal first "gh") 'gh)
       ((member first '("pip" "pip3"))
        (and (equal second "install") 'pip-install))
       ((equal first "npm")
        (and (member second '("install" "i" "ci")) 'npm-install))
       ((equal first "docker")
        (cond ((equal second "ps")   'docker-ps)
              ((equal second "logs") 'docker-logs)
              (t nil)))
       ((equal first "kubectl")
        (and (equal second "get") 'kubectl-get))
       ((equal first "aws")
        (and (equal second "s3") (equal third "ls") 'aws-s3-ls))
       ((equal first "prettier") 'prettier)
       ((equal first "ruff") 'ruff)
       ((member first '("rg" "ag")) 'rg)
       ((equal first "find") 'find)
       ((equal first "ls") 'ls)
       ((equal first "pytest") 'pytest)
       ((equal first "make") 'make)
       (t (cl-loop for (regex . tag) in anvil-shell-filter--match-command-table
                   when (string-match-p regex cmd)
                   return tag))))))

(defun anvil-shell-filter-apply (name raw)
  "Apply filter tag NAME to string RAW.
NAME nil returns RAW unchanged.  Unknown NAMEs also passthrough
so `shell-run' can fall back gracefully when `anvil-shell-filter-lookup'
returns a tag that hasn't been implemented yet."
  (cond
   ((null name) raw)
   ((not (stringp raw)) raw)
   (t
    (let ((fn (alist-get name anvil-shell-filter-handlers)))
      (if (functionp fn)
          (funcall fn raw)
        raw)))))


;;;; --- declarative register API (Phase 3) ---------------------------------

;; Doc 27 Phase 3 — register API + 8-stage pipeline.  Lets a filter be
;; declared as data (a plist of regex / int / string knobs) instead of
;; a hand-coded `(defun ...)'.  The seed corpus is the rtk TOML import
;; (vendor/rtk-filters/), but the same API serves user-defined filters
;; in `~/.emacs.d/anvil-shell-filters.el'.

;; rtk specs author regex against Rust's `regex' crate; Elisp's regex
;; dialect differs (capturing groups need `\\(', alternation needs
;; `\\|', `\\s' / `\\d' / `\\w' need POSIX class equivalents).  The
;; translator below covers the small subset rtk filters actually use,
;; so vendored TOMLs stay byte-identical to upstream and only the
;; in-Emacs registration translates.

(defun anvil-shell-filter--rust-regex-to-elisp (pattern)
  "Translate a (small subset of) Rust regex PATTERN into Elisp regex.

Handles the idioms rtk filters use:

  \\s → [[:space:]]      \\S → [^[:space:]]
  \\d → [0-9]            \\D → [^0-9]
  \\w → [A-Za-z0-9_]     \\W → [^A-Za-z0-9_]

Capturing / alternation differences (outside character classes):

  (        → \\(
  )        → \\)
  |        → \\|

Non-capturing groups `(?:' become Elisp's `\\(?:'.  Named captures
`(?P<name>...)' are downgraded to `\\(?:...\\)' — the captured text
is unused by the filter pipeline.  `\\b' / `\\n' / `\\t' / `\\\\'
pass through unchanged."
  (if (or (null pattern) (not (stringp pattern)) (string-empty-p pattern))
      pattern
    (let ((out (make-string 0 0))
          (i 0)
          (n (length pattern))
          (in-class nil))
      (while (< i n)
        (let ((c (aref pattern i)))
          (cond
           ;; Inside [...] keep everything verbatim except an escape pair.
           (in-class
            (cond
             ((eq c ?\\)
              (when (< (1+ i) n)
                (setq out (concat out (string c (aref pattern (1+ i))))))
              (cl-incf i 2))
             ((eq c ?\])
              (setq out (concat out "]"))
              (setq in-class nil)
              (cl-incf i))
             (t
              (setq out (concat out (string c)))
              (cl-incf i))))
           ;; Backslash escapes outside char class.
           ((eq c ?\\)
            (let ((nx (and (< (1+ i) n) (aref pattern (1+ i)))))
              (cond
               ((eq nx ?s) (setq out (concat out "[[:space:]]"))   (cl-incf i 2))
               ((eq nx ?S) (setq out (concat out "[^[:space:]]"))  (cl-incf i 2))
               ((eq nx ?d) (setq out (concat out "[0-9]"))         (cl-incf i 2))
               ((eq nx ?D) (setq out (concat out "[^0-9]"))        (cl-incf i 2))
               ((eq nx ?w) (setq out (concat out "[A-Za-z0-9_]"))  (cl-incf i 2))
               ((eq nx ?W) (setq out (concat out "[^A-Za-z0-9_]")) (cl-incf i 2))
               ;; Passthrough: \b \n \t \\ \. \( \) \| etc.
               ;; In particular \( / \) / \| stay as literal escapes.
               (t (setq out (concat out (string c (or nx ?\\))))
                  (cl-incf i (if nx 2 1))))))
           ((eq c ?\[)
            (setq out (concat out "["))
            (setq in-class t)
            (cl-incf i))
           ;; Group open: detect (?: and (?P<name>
           ((eq c ?\()
            (cond
             ;; (?:...
             ((and (< (+ i 2) n)
                   (eq (aref pattern (1+ i)) ?\?)
                   (eq (aref pattern (+ i 2)) ?:))
              (setq out (concat out "\\(?:"))
              (cl-incf i 3))
             ;; (?P<name>... → downgrade to (?: by skipping past ">"
             ((and (< (+ i 3) n)
                   (eq (aref pattern (1+ i)) ?\?)
                   (eq (aref pattern (+ i 2)) ?P)
                   (eq (aref pattern (+ i 3)) ?<))
              (let ((close (cl-position ?> pattern :start (+ i 4))))
                (setq out (concat out "\\(?:"))
                (setq i (if close (1+ close) (+ i 4)))))
             (t
              (setq out (concat out "\\("))
              (cl-incf i))))
           ((eq c ?\))
            (setq out (concat out "\\)"))
            (cl-incf i))
           ((eq c ?|)
            (setq out (concat out "\\|"))
            (cl-incf i))
           (t
            (setq out (concat out (string c)))
            (cl-incf i)))))
      out)))

(defun anvil-shell-filter--xlat-cell-pattern (cell)
  "Translate the car of CELL (a (PATTERN . X) cons) via the Rust→Elisp helper."
  (cons (anvil-shell-filter--rust-regex-to-elisp (car cell)) (cdr cell)))

(defconst anvil-shell-filter--ansi-csi-re
  "\x1b\\[[0-9;?]*[a-zA-Z]"
  "Regex matching ANSI CSI escape sequences (SGR + cursor moves).")

(defun anvil-shell-filter--strip-ansi (raw)
  "Strip ANSI CSI / SGR escape sequences from RAW string."
  (replace-regexp-in-string anvil-shell-filter--ansi-csi-re "" raw))

(defun anvil-shell-filter--apply-replace (raw replace-list)
  "Apply REPLACE-LIST `((PATTERN . REPLACEMENT) ...)' to RAW in order."
  (let ((out raw))
    (dolist (cell replace-list out)
      (setq out (replace-regexp-in-string (car cell) (cdr cell) out)))))

(defun anvil-shell-filter--apply-strip-lines (raw regex-list)
  "Drop every line of RAW matching ANY regex in REGEX-LIST.
A nil REGEX-LIST returns RAW unchanged."
  (if (null regex-list)
      raw
    (let* ((lines (split-string raw "\n"))
           (kept  (cl-remove-if
                   (lambda (line)
                     (cl-some (lambda (re) (string-match-p re line))
                              regex-list))
                   lines)))
      (string-join kept "\n"))))

(defun anvil-shell-filter--apply-keep-lines (raw regex-list)
  "Keep only lines of RAW matching at least one regex in REGEX-LIST.
A nil REGEX-LIST returns RAW unchanged (= no filtering)."
  (if (null regex-list)
      raw
    (let* ((lines (split-string raw "\n"))
           (kept  (cl-remove-if-not
                   (lambda (line)
                     (cl-some (lambda (re) (string-match-p re line))
                              regex-list))
                   lines)))
      (string-join kept "\n"))))

(defun anvil-shell-filter--apply-truncate (raw n)
  "Right-trim each line of RAW to N chars; lines longer get `...' appended.
Nil or non-positive N returns RAW unchanged."
  (if (or (not (numberp n)) (<= n 0))
      raw
    (string-join
     (mapcar (lambda (line)
               (if (> (length line) n)
                   (concat (substring line 0 (max 0 (- n 3))) "...")
                 line))
             (split-string raw "\n"))
     "\n")))

(defun anvil-shell-filter--apply-tail-lines (raw n)
  "Keep only the last N lines of RAW.  Nil / non-positive N returns RAW."
  (if (or (not (numberp n)) (<= n 0))
      raw
    (let ((lines (split-string raw "\n")))
      (string-join (last lines n) "\n"))))

(defun anvil-shell-filter--apply-max-lines (raw n)
  "Keep only the first N lines of RAW.  Nil / non-positive N returns RAW."
  (if (or (not (numberp n)) (<= n 0))
      raw
    (let* ((lines (split-string raw "\n"))
           (cut   (cl-subseq lines 0 (min n (length lines)))))
      (string-join cut "\n"))))

(defun anvil-shell-filter--match-output-short-circuit (raw match-list)
  "Return MESSAGE for the first regex in MATCH-LIST that matches RAW, else nil.
MATCH-LIST is `((PATTERN . MESSAGE) ...)'."
  (cl-loop for (pat . msg) in match-list
           when (string-match-p pat raw)
           return msg))

;;;###autoload
(cl-defun anvil-shell-filter-register
    (tag &key match-command strip-ansi filter-stderr replace match-output
         strip-lines-matching keep-lines-matching truncate-lines-at
         max-lines tail-lines on-empty description)
  "Register a declarative filter under TAG (a symbol).

The pipeline runs in this order on every call:

  1. strip-ansi          (when non-nil — drops ANSI CSI / SGR escapes)
  2. replace             ((PATTERN . REPLACEMENT) cells, in order)
  3. match-output        ((PATTERN . MESSAGE) cells; first match short-circuits)
  4. strip-lines-matching (drop any line matching ANY regex in the list)
  5. keep-lines-matching  (keep only lines matching ANY regex; nil = no-op)
  6. truncate-lines-at   (right-trim each line to N chars; appends `...')
  7. tail-lines          (keep only last N lines)
  8. max-lines           (keep only first N lines AFTER tail)
  9. on-empty fallback   (return :on-empty verbatim when result trims empty)

Existing entries with the same TAG are replaced; the previous match-command
entry (if any) is removed from `anvil-shell-filter--match-command-table'
before the new one is pushed, so re-registering never leaves orphans.

DESCRIPTION and FILTER-STDERR are accepted for spec parity with the rtk
TOML schema but are currently advisory only — stderr is already merged
into stdout by `anvil-shell-filter-run'.

Returns TAG.

Every regex in PROPS (`:match-command' / `:strip-lines-matching' /
`:keep-lines-matching' / patterns in `:replace' and `:match-output')
is run through `anvil-shell-filter--rust-regex-to-elisp' before
storage so vendored rtk TOMLs (Rust regex dialect) work as-is."
  (ignore filter-stderr description)
  (let* ((match-command (anvil-shell-filter--rust-regex-to-elisp match-command))
         (replace (mapcar #'anvil-shell-filter--xlat-cell-pattern replace))
         (match-output (mapcar #'anvil-shell-filter--xlat-cell-pattern match-output))
         (strip-lines-matching
          (mapcar #'anvil-shell-filter--rust-regex-to-elisp strip-lines-matching))
         (keep-lines-matching
          (mapcar #'anvil-shell-filter--rust-regex-to-elisp keep-lines-matching))
         (closure
         (lambda (raw)
           (let* ((s (if strip-ansi
                         (anvil-shell-filter--strip-ansi raw)
                       raw))
                  (s (anvil-shell-filter--apply-replace s replace))
                  (mo (and match-output
                           (anvil-shell-filter--match-output-short-circuit
                            s match-output))))
             (if mo
                 mo
               (let* ((s (anvil-shell-filter--apply-strip-lines
                          s strip-lines-matching))
                      (s (anvil-shell-filter--apply-keep-lines
                          s keep-lines-matching))
                      (s (anvil-shell-filter--apply-truncate
                          s truncate-lines-at))
                      (s (anvil-shell-filter--apply-tail-lines
                          s tail-lines))
                      (s (anvil-shell-filter--apply-max-lines
                          s max-lines)))
                 (if (and on-empty (string-empty-p (string-trim s)))
                     on-empty
                   s)))))))
    (setf (alist-get tag anvil-shell-filter-handlers) closure)
    (when match-command
      (setq anvil-shell-filter--match-command-table
            (cl-remove-if (lambda (cell) (eq (cdr cell) tag))
                          anvil-shell-filter--match-command-table))
      (push (cons match-command tag)
            anvil-shell-filter--match-command-table))
    tag))

;;;###autoload
(defun anvil-shell-filter-register-from-spec (spec)
  "Register a filter from SPEC, a plist with :tag plus the same keys
accepted by `anvil-shell-filter-register'.

Used by the auto-generated `anvil-shell-filter-builtin.el' bridge so
each rtk-derived call site stays one line.  SPEC must contain :tag;
all other keywords are forwarded verbatim."
  (let ((tag (plist-get spec :tag))
        (kw  (cl-loop for (k v) on spec by #'cddr
                      unless (eq k :tag)
                      append (list k v))))
    (unless tag
      (error "anvil-shell-filter-register-from-spec: SPEC missing :tag — %S" spec))
    (apply #'anvil-shell-filter-register tag kw)))


;;;; --- tee + gain statistics ----------------------------------------------

(defun anvil-shell-filter--new-id ()
  "Generate a short tee-id (`t-<epoch>-<rand>')."
  (format "t-%x-%x"
          (truncate (float-time))
          (random #x1000000)))

(defun anvil-shell-filter--trace-new-id ()
  "Generate a short trace-id (`tr-<epoch>-<rand>')."
  (format "tr-%x-%x"
          (truncate (float-time))
          (random #x1000000)))

(defun anvil-shell-filter--trace (id phase start-time)
  "Record a phase trace row for PHASE under trace ID.
START-TIME is the timestamp captured when the run began; the
record carries the elapsed milliseconds between START-TIME and
now.  No-op when `anvil-shell-filter-trace-events' is nil."
  (when anvil-shell-filter-trace-events
    (ignore-errors
      (anvil-state-set
       (format "%s-%s" id phase)
       (list :phase phase
             :at (truncate (float-time))
             :elapsed-ms (truncate (* 1000 (float-time
                                            (time-since start-time)))))
       :ns anvil-shell-filter--trace-ns
       :ttl anvil-shell-trace-ttl-sec))))

(defun anvil-shell-filter--byte-prefix (string max-bytes)
  "Return the longest whole-character prefix of STRING within MAX-BYTES."
  (unless (and (integerp max-bytes) (>= max-bytes 0))
    (error "anvil-shell-filter: byte cap must be a nonnegative integer"))
  (let ((low 0)
        (high (length string)))
    (while (< low high)
      (let ((mid (/ (+ low high 1) 2)))
        (if (<= (string-bytes (substring string 0 mid)) max-bytes)
            (setq low mid)
          (setq high (1- mid)))))
    (substring string 0 low)))

(defun anvil-shell-filter--raw-byte-character-p (character)
  "Return non-nil when CHARACTER is an undecoded source byte."
  (and (> character 255)
       (>= (multibyte-char-to-unibyte character) 0)))

(defun anvil-shell-filter--tee-marker (omitted cap)
  "Return an omission marker for OMITTED source bytes within CAP.
Use the descriptive marker whenever it fits.  Small caps receive a compact
exact-count marker; impossibly small caps receive its bounded prefix."
  (if (<= omitted 0)
      ""
    (let* ((full
            (format "\n…[anvil-shell-tee: truncated %d bytes]" omitted))
           (compact (format "[+%dB]" omitted)))
      (cond
       ((or (null cap) (<= (string-bytes full) cap)) full)
       ((<= (string-bytes compact) cap) compact)
       (t (anvil-shell-filter--byte-prefix "..." cap))))))

(defun anvil-shell-filter--tee-candidate
    (raw source-bytes coding total-bytes characters cap)
  "Build a bounded tee candidate from the first CHARACTERS of RAW.
SOURCE-BYTES is the captured unibyte stream, CODING decodes it, and
TOTAL-BYTES is the complete source-stream size.  Return (TEXT . KEPT)
only when the decoded prefix round-trips to the exact source prefix."
  (condition-case nil
      (let* ((prefix (substring raw 0 characters))
             (encoded (encode-coding-string prefix coding))
             (kept (string-bytes encoded)))
        (when (and (<= kept (string-bytes source-bytes))
                   (equal encoded (substring source-bytes 0 kept))
                   (<= kept total-bytes))
          (let* ((marker
                  (anvil-shell-filter--tee-marker
                   (- total-bytes kept) cap))
                 (candidate (concat prefix marker)))
            (when (or (null cap)
                      (<= (string-bytes candidate) cap))
              (cons candidate kept)))))
    (error nil)))

(defun anvil-shell-filter--prefix-mappable-coding-p (coding)
  "Return non-nil when CODING supports independent prefix re-encoding."
  (memq (coding-system-get coding :coding-type)
        '(charset no-conversion raw-text utf-8)))

(defun anvil-shell-filter--bounded-tee-value
    (raw source-bytes coding total-bytes cap)
  "Return RAW plus any exact omission marker within byte CAP.
Prefix selection counts TOTAL-BYTES and SOURCE-BYTES in the source encoding,
but applies CAP to the plain text stored by `anvil-state'."
  (let* ((raw* (substring-no-properties raw))
         (captured-bytes (string-bytes source-bytes))
         (complete (= total-bytes captured-bytes))
         (raw-byte-position
          (cl-position-if
           #'anvil-shell-filter--raw-byte-character-p raw*)))
    (cond
     ((and complete
           (null raw-byte-position)
           (or (null cap)
               (<= (string-bytes raw*) cap)))
      raw*)
     (t
      (let ((full
             (and
              (null raw-byte-position)
              (anvil-shell-filter--tee-candidate
               raw* source-bytes coding total-bytes (length raw*) cap))))
        (cond
         (full (car full))
         ((not (anvil-shell-filter--prefix-mappable-coding-p coding))
          (car
           (anvil-shell-filter--tee-candidate
            raw* source-bytes coding total-bytes 0 cap)))
         (t
          (let ((low 0)
                (high (or raw-byte-position (length raw*)))
                best)
            (while (<= low high)
              (let* ((middle (/ (+ low high) 2))
                     (candidate
                      (anvil-shell-filter--tee-candidate
                       raw* source-bytes coding total-bytes middle cap)))
                (if candidate
                    (setq best (car candidate)
                          low (1+ middle))
                  (setq high (1- middle)))))
            (or best
                (car
                 (anvil-shell-filter--tee-candidate
                  raw* source-bytes coding total-bytes 0 cap))
                "")))))))))

(defun anvil-shell-filter--tee-put
    (raw &optional total-bytes source-bytes coding)
  "Store a bounded RAW prefix under a fresh tee id and return that id.
TOTAL-BYTES is the complete source-stream size.  SOURCE-BYTES is the captured
unibyte stream decoded as CODING.  The stored text, including its exact or
compact omission marker, never exceeds `anvil-shell-tee-max-bytes'."
  (let* ((id (anvil-shell-filter--new-id))
         (cap anvil-shell-tee-max-bytes)
         (coding* (or coding 'utf-8))
         (source*
          (or source-bytes
              (encode-coding-string raw coding*)))
         (total (or total-bytes (string-bytes source*)))
         (bounded
          (anvil-shell-filter--bounded-tee-value
           raw source* coding* total cap)))
    (anvil-state-set id bounded
                     :ns anvil-shell-filter--tee-ns
                     :ttl anvil-shell-tee-ttl-sec)
    id))

;;;###autoload
(defun anvil-shell-filter-tee-get (tee-id)
  "Return the raw text stored under TEE-ID, or nil when absent / expired.
Never signals; missing ids return nil so the caller can retry with
a fresh `shell-run' invocation."
  (when (and tee-id (stringp tee-id) (not (string-empty-p tee-id)))
    (anvil-state-get tee-id :ns anvil-shell-filter--tee-ns)))

(defun anvil-shell-filter--gain-record (name raw-size compressed-size)
  "Append a gain entry for NAME (RAW-SIZE→COMPRESSED-SIZE bytes).
Entries are keyed by UTC date under the `shell-gain' namespace so
`shell-gain' can aggregate across days.  Returns the newly-recorded
plist entry."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (existing (or (anvil-state-get today :ns anvil-shell-filter--gain-ns)
                       '()))
         (entry (list :cmd (format "%s" name)
                      :raw raw-size
                      :compressed compressed-size
                      :saved (- raw-size compressed-size)
                      :at (truncate (float-time)))))
    (anvil-state-set today (cons entry existing)
                     :ns anvil-shell-filter--gain-ns)
    entry))

;;;###autoload
(defun anvil-shell-filter-gain (&optional days)
  "Return cumulative filter savings over the last DAYS days (default 7).
Result plist:
  :days              — window size in days
  :entries           — number of filter invocations in the window
  :raw-total         — total raw bytes
  :compressed-total  — total compressed bytes
  :saved-total       — raw − compressed bytes
  :saved-tokens      — approximate token savings (4 chars/token)
  :pct               — saved / raw × 100, float"
  (let* ((days (or days 7))
         (all-keys (ignore-errors
                     (anvil-state-list-keys :ns anvil-shell-filter--gain-ns)))
         (recent (if (> (length all-keys) days)
                     (cl-subseq all-keys (- (length all-keys) days))
                   all-keys))
         (entries (cl-loop for k in recent
                           append (or (anvil-state-get
                                       k :ns anvil-shell-filter--gain-ns)
                                      '())))
         (raw-total (cl-loop for e in entries sum (or (plist-get e :raw) 0)))
         (compressed-total (cl-loop for e in entries
                                    sum (or (plist-get e :compressed) 0)))
         (saved (- raw-total compressed-total)))
    (list :days days
          :entries (length entries)
          :raw-total raw-total
          :compressed-total compressed-total
          :saved-total saved
          :saved-tokens (truncate (* saved anvil-shell-filter-tokens-per-char))
          :pct (if (> raw-total 0) (/ (* 100.0 saved) raw-total) 0.0))))


;;;; --- shell-run orchestrator ---------------------------------------------

(defun anvil-shell-filter--bounded-sync-timeout (timeout)
  "Return TIMEOUT when it fits the synchronous MCP deadline.

Larger values would pin the root event loop beyond its watchdog and
client envelopes, so reject them before a child process is started."
  (unless (and (numberp timeout) (>= timeout 0))
    (user-error "shell-run timeout must be a non-negative number"))
  (when (and (numberp anvil-shell-filter-max-sync-timeout)
             (> timeout anvil-shell-filter-max-sync-timeout))
    (user-error
     "shell-run timeout %ss exceeds synchronous cap %ss; use native execution or emacs-eval-async"
     timeout anvil-shell-filter-max-sync-timeout))
  timeout)

;;;###autoload
(defun anvil-shell-filter-run (cmd &rest opts)
  "Run shell CMD, apply a filter, tee the raw output to `anvil-state'.
OPTS is a plist:
  :filter   `auto' (default), a filter tag symbol, or nil to skip.
  :timeout  seconds, defaults to `anvil-shell-filter-default-timeout'.
  :cwd      working directory for the shell invocation.

Returns a plist:
  :exit             shell exit status
  :filter           resolved filter tag (or nil on passthrough)
  :compressed       compressed output string
  :raw-size         complete stdout size before capture limiting (bytes)
  :compressed-size  compressed output size (bytes)
  :tee-id           id under which bounded raw stdout was saved
  :stderr           bounded stderr with any presentation truncation marker
  :truncated        non-nil when `anvil-shell' truncated the buffers

Stdout capture and the stored tee value, including its omission marker, are
bounded by `anvil-shell-tee-max-bytes'.  Redirect genuinely large output to
a file."
  (let* ((trace-id (and anvil-shell-filter-trace-events
                        (anvil-shell-filter--trace-new-id)))
         (trace-start (and trace-id (current-time)))
         (filter-opt
          (if (plist-member opts :filter)
              (plist-get opts :filter)
            'auto))
         (timeout
          (anvil-shell-filter--bounded-sync-timeout
           (or (plist-get opts :timeout)
               anvil-shell-filter-default-timeout)))
         (cwd (plist-get opts :cwd))
         (resolved0 (cond
                     ((eq filter-opt 'auto) (anvil-shell-filter-lookup cmd))
                     ((null filter-opt) nil)
                     (t filter-opt))))
    (when trace-id
      (anvil-shell-filter--trace trace-id "start" trace-start))
    (let* ((result
            (anvil-shell
             cmd
             (list :timeout timeout :cwd cwd
                   :max-output anvil-shell-tee-max-bytes
                   :include-source-bytes t)))
           (_ (when trace-id
                (anvil-shell-filter--trace trace-id "exec-done" trace-start)))
           (exit (plist-get result :exit))
           (raw (or (plist-get result :stdout-captured)
                    (plist-get result :stdout)
                    ""))
           (stderr-captured
            (or (plist-get result :stderr-captured)
                (plist-get result :stderr)
                ""))
           (stderr
            (or (plist-get result :stderr)
                stderr-captured))
           (source-bytes
            (plist-get result :stdout-captured-source-bytes))
           (coding (or (plist-get result :coding) 'utf-8))
           (truncated (plist-get result :truncated))
           (taco-compressed
            (and (eq filter-opt 'auto)
                 (null resolved0)
                 (anvil-shell-filter--taco-critical-maybe
                  raw stderr-captured exit)))
           (resolved (if taco-compressed 'taco-critical resolved0))
           (compressed (or taco-compressed
                           (anvil-shell-filter-apply resolved raw)))
           (_ (when trace-id
                (anvil-shell-filter--trace trace-id "filter-done" trace-start)))
           (raw-size
            (or (plist-get result :stdout-total-bytes)
                (if source-bytes
                    (string-bytes source-bytes)
                  (string-bytes raw))))
           (compressed-size (string-bytes compressed))
           (tee-id
            (anvil-shell-filter--tee-put
             raw raw-size source-bytes coding))
           (_ (when trace-id
                (anvil-shell-filter--trace trace-id "tee-done" trace-start))))
      (when resolved
        (anvil-shell-filter--gain-record resolved raw-size compressed-size))
      (when trace-id
        (anvil-shell-filter--trace trace-id "return" trace-start))
      (list :exit exit
            :filter resolved
            :compressed compressed
            :raw-size raw-size
            :compressed-size compressed-size
            :tee-id tee-id
            :stderr stderr
            :truncated truncated))))


;;;; --- tee-grep: regex line filter + per-line truncate -------------------

(defun anvil-shell-filter--truncate-line (line max-bytes)
  "Return LINE within MAX-BYTES, including any elision sentinel.
Nil or zero preserves LINE.  A tiny positive cap that cannot hold the
exact-count sentinel receives a compact ASCII elision token instead."
  (unless (or (null max-bytes)
              (and (integerp max-bytes) (>= max-bytes 0)))
    (user-error "tee-grep max-line-bytes must be a nonnegative integer"))
  (let ((total (string-bytes line)))
    (cond
     ((or (null max-bytes) (zerop max-bytes) (<= total max-bytes))
      line)
     (t
      (let ((marker (format "…(%d bytes elided)" total)))
        (if (> (string-bytes marker) max-bytes)
            (anvil-shell-filter--byte-prefix "..." max-bytes)
          (let ((prefix "")
                (previous-kept -1))
            (while (/= previous-kept (string-bytes prefix))
              (setq previous-kept (string-bytes prefix)
                    prefix
                    (anvil-shell-filter--byte-prefix
                     line
                     (max 0 (- max-bytes (string-bytes marker))))
                    marker
                    (format "…(%d bytes elided)"
                            (- total (string-bytes prefix)))))
            (concat prefix marker))))))))

(defun anvil-shell-filter--grep-lines (raw regex max-line-bytes tail-fallback)
  "Return lines from RAW that match REGEX, each truncated to MAX-LINE-BYTES.
When zero lines match, return the last TAIL-FALLBACK lines instead."
  (let* ((lines (split-string (or raw "") "\n" nil))
         (matches (cl-remove-if-not
                   (lambda (l) (string-match-p regex l))
                   lines))
         (used-fallback nil)
         (selected
          (if (and (null matches) (> tail-fallback 0))
              (progn (setq used-fallback t)
                     (let ((n (length lines)))
                       (if (<= n tail-fallback)
                           lines
                         (nthcdr (- n tail-fallback) lines))))
            matches)))
    (cons used-fallback
          (mapconcat (lambda (l)
                       (anvil-shell-filter--truncate-line l max-line-bytes))
                     selected
                     "\n"))))

;;;###autoload
(defun anvil-shell-filter-tee-grep (cmd &rest opts)
  "Run shell CMD, return only stdout lines matching `:grep' regex.
Each retained line is truncated to `:max-line-bytes' (default
`anvil-shell-tee-grep-default-max-line-bytes').  A bounded raw prefix is
tee'd; `:raw-size' and `:truncated' disclose omitted output.

OPTS plist:
  :grep             regex; lines that don't match are dropped (required)
  :max-line-bytes   per-line truncation cap (default 200)
  :tail-fallback    when zero lines match, return last N lines instead
                    (default 50; pass 0 to disable)
  :timeout          seconds, defaults to `--default-timeout'
  :cwd              working directory

Returns a plist:
  :exit             shell exit code
  :compressed       filtered output (newline-joined)
  :raw-size         complete source-stream stdout size
  :compressed-size  compressed output size in bytes
  :match-count      number of lines selected
  :used-fallback    t when tail-fallback was triggered
  :tee-id           id under which bounded raw stdout was saved
  :stderr           bounded stderr with any presentation truncation marker
  :truncated        non-nil when shell buffers truncated"
  (let* ((grep (or (plist-get opts :grep)
                   (error "anvil-shell-filter-tee-grep: :grep is required")))
         (max-line-bytes
          (let ((value
                 (or (plist-get opts :max-line-bytes)
                     anvil-shell-tee-grep-default-max-line-bytes)))
            (unless (and (integerp value) (>= value 0))
              (user-error
               "tee-grep max-line-bytes must be a nonnegative integer"))
            value))
         (tail-fallback (let ((v (plist-get opts :tail-fallback)))
                          (if (numberp v) v
                            anvil-shell-tee-grep-default-tail-fallback)))
         (timeout
          (anvil-shell-filter--bounded-sync-timeout
           (or (plist-get opts :timeout)
               anvil-shell-filter-default-timeout)))
         (cwd (plist-get opts :cwd))
         (result
          (anvil-shell
           cmd
           (list :timeout timeout :cwd cwd
                 :max-output anvil-shell-tee-max-bytes
                 :include-source-bytes t)))
         (exit (plist-get result :exit))
         (raw (or (plist-get result :stdout-captured)
                  (plist-get result :stdout)
                  ""))
         (stderr-captured
          (or (plist-get result :stderr-captured)
              (plist-get result :stderr)
              ""))
         (stderr
          (or (plist-get result :stderr)
              stderr-captured))
         (source-bytes
          (plist-get result :stdout-captured-source-bytes))
         (coding (or (plist-get result :coding) 'utf-8))
         (truncated (plist-get result :truncated))
         (grep-result (anvil-shell-filter--grep-lines
                       raw grep max-line-bytes tail-fallback))
         (used-fallback (car grep-result))
         (compressed (cdr grep-result))
         (raw-size
          (or (plist-get result :stdout-total-bytes)
              (if source-bytes
                  (string-bytes source-bytes)
                (string-bytes raw))))
         (compressed-size (string-bytes compressed))
         (tee-id
          (anvil-shell-filter--tee-put
           raw raw-size source-bytes coding))
         (match-count (length (split-string compressed "\n" nil))))
    (anvil-shell-filter--gain-record 'tee-grep raw-size compressed-size)
    (list :exit exit
          :compressed compressed
          :raw-size raw-size
          :compressed-size compressed-size
          :match-count (if (string-empty-p compressed) 0 match-count)
          :used-fallback used-fallback
          :tee-id tee-id
          :stderr stderr
          :truncated truncated)))


;;;; --- MCP tool handlers --------------------------------------------------

(defun anvil-shell-filter--coerce-int (v default)
  "Coerce V (integer, digit-string, or nil) to an integer, else DEFAULT."
  (cond ((integerp v) v)
        ((and (stringp v) (string-match "\\`-?[0-9]+\\'" v))
         (string-to-number v))
        (t default)))

(defun anvil-shell-filter--tool-shell-run (cmd &optional filter timeout_sec cwd)
  "Run shell CMD, filter, and tee; return the result plist.

MCP Parameters:
  cmd         - Shell command line to execute.  Required.
  filter      - `auto' (default), a filter tag name (e.g. `git-status'),
                or empty string / nil to skip compression.
  timeout_sec - Optional timeout override in seconds.
  cwd         - Optional working directory for the shell invocation.

Returns (:exit :filter :compressed :raw-size :compressed-size
:tee-id :stderr :truncated).  A bounded raw stdout prefix is saved under
the tee namespace; `:raw-size' and `:truncated' disclose omissions."
  (anvil-server-with-error-handling
   (let* ((filter-tag (cond
                       ((null filter) 'auto)
                       ((and (stringp filter) (string-empty-p filter)) nil)
                       ((and (stringp filter) (equal filter "auto")) 'auto)
                       ((stringp filter) (intern filter))
                       ((symbolp filter) filter)
                       (t 'auto)))
          (timeout (anvil-shell-filter--coerce-int
                    timeout_sec anvil-shell-filter-default-timeout))
          (cwd* (and (stringp cwd) (not (string-empty-p cwd)) cwd)))
     (anvil-shell-filter-run cmd :filter filter-tag :timeout timeout :cwd cwd*))))

(defun anvil-shell-filter--tool-shell-filter (filter raw)
  "Apply FILTER to RAW without running a shell command.

MCP Parameters:
  filter - Filter tag name (e.g. `git-status').  Empty / nil = passthrough.
  raw    - Raw text to compress.

Returns (:filter :compressed :raw-size :compressed-size).  Exists so
callers that already have the raw output (from a different tool or
a prior tee-get) can re-compress without re-running the shell."
  (anvil-server-with-error-handling
   (let* ((filter-tag (cond
                       ((null filter) nil)
                       ((and (stringp filter) (string-empty-p filter)) nil)
                       ((stringp filter) (intern filter))
                       ((symbolp filter) filter)
                       (t nil)))
          (raw* (or raw ""))
          (compressed (anvil-shell-filter-apply filter-tag raw*)))
     (list :filter filter-tag
           :compressed compressed
           :raw-size (string-bytes raw*)
           :compressed-size (string-bytes compressed)))))

(defun anvil-shell-filter--tool-shell-tee-get (tee_id)
  "Retrieve raw stdout previously stored under TEE_ID.

MCP Parameters:
  tee_id - Identifier returned by `shell-run'.

Returns (:tee-id :raw :found).  `:found' is nil when the id is
unknown or its entry expired; `:raw' is the empty string in that
case."
  (anvil-server-with-error-handling
   (let* ((raw (anvil-shell-filter-tee-get tee_id)))
     (list :tee-id tee_id
           :raw (or raw "")
           :found (and raw t)))))

(defun anvil-shell-filter--tool-shell-tee-grep
    (cmd grep &optional max_line_bytes tail_fallback timeout_sec cwd)
  "Run shell CMD, return only stdout lines matching GREP regex.

MCP Parameters:
  cmd             - Shell command line.  Required.
  grep            - Regex; lines that don't match are dropped.  Required.
  max_line_bytes  - Per-line truncation cap (default 200).
  tail_fallback   - When zero lines match, return last N lines instead
                    (default 50; pass 0 to disable).
  timeout_sec     - Optional timeout override in seconds.
  cwd             - Optional working directory.

Returns (:exit :compressed :raw-size :compressed-size :match-count
:used-fallback :tee-id :stderr :truncated).  A bounded raw stdout prefix
is saved under the tee namespace; `:raw-size' and `:truncated' disclose
omissions."
  (anvil-server-with-error-handling
   (let* ((max-line (anvil-shell-filter--coerce-int
                     max_line_bytes
                     anvil-shell-tee-grep-default-max-line-bytes))
          (tail (anvil-shell-filter--coerce-int
                 tail_fallback
                 anvil-shell-tee-grep-default-tail-fallback))
          (timeout (anvil-shell-filter--coerce-int
                    timeout_sec anvil-shell-filter-default-timeout))
          (cwd* (and (stringp cwd) (not (string-empty-p cwd)) cwd)))
     (anvil-shell-filter-tee-grep cmd
                                  :grep grep
                                  :max-line-bytes max-line
                                  :tail-fallback tail
                                  :timeout timeout
                                  :cwd cwd*))))

(defun anvil-shell-filter--tool-shell-gain (&optional days)
  "Return cumulative filter savings over the last DAYS days.

MCP Parameters:
  days - Window size (default 7).  Accepts integer or digit string."
  (anvil-server-with-error-handling
   (let ((d (anvil-shell-filter--coerce-int days 7)))
     (anvil-shell-filter-gain d))))


;;;; --- module lifecycle ---------------------------------------------------

(defconst anvil-shell-filter--tool-specs
  `((,(anvil-server-encode-handler #'anvil-shell-filter--tool-shell-run)
     :id "shell-run"
     :intent '(shell)
     :layer 'io
     :description
     "Run a shell command, compress its stdout through a per-command filter
(git-status, git-log, git-diff, rg, find, ls, pytest, ert-batch,
emacs-batch, make), and save a bounded raw prefix to the tee namespace
for later retrieval via `shell-tee-get'.  `filter=auto' picks a
handler from the first token of CMD; `filter=\"\"` disables
compression.")

    (,(anvil-server-encode-handler #'anvil-shell-filter--tool-shell-filter)
     :id "shell-filter"
     :intent '(shell)
     :layer 'io
     :description
     "Apply a named filter to a string without running a shell command.  Lets
callers re-compress output they already have (from a prior
`shell-tee-get' or a foreign tool)."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-shell-filter--tool-shell-tee-get)
     :id "shell-tee-get"
     :intent '(shell)
     :layer 'io
     :description
     "Fetch the bounded raw stdout prefix captured by `shell-run' under TEE_ID.
Retention is governed by `anvil-shell-tee-ttl-sec' (default 1h);
expired ids return :found nil."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-shell-filter--tool-shell-tee-grep)
     :id "shell-tee-grep"
     :intent '(shell)
     :layer 'io
     :description
     "Run a shell command, drop stdout lines that don't match GREP regex,
truncate each retained line to MAX_LINE_BYTES (default 200), and tee
a bounded raw stdout prefix for later retrieval via `shell-tee-get'.  When zero
lines match GREP, falls back to the last TAIL_FALLBACK lines (default
50) so the caller never gets an empty result by accident.  Designed
for `make bench-actual', `cargo test', `pytest', etc — extract just
the OVERALL / gate / fail-summary lines without parsing on the
client side.")

    (,(anvil-server-encode-handler #'anvil-shell-filter--tool-shell-gain)
     :id "shell-gain"
     :intent '(shell admin)
     :layer 'io
     :description
     "Summarise cumulative raw→compressed savings over the last DAYS days
(default 7).  Returns raw / compressed / saved byte totals plus an
approximate token count and percentage saved."
     :read-only t))
  "Spec list consumed by `anvil-server-register-tools'.")

(defun anvil-shell-filter--register-tools ()
  (anvil-server-register-tools anvil-shell-filter--server-id
                               anvil-shell-filter--tool-specs))

(defun anvil-shell-filter--unregister-tools ()
  (anvil-server-unregister-tools anvil-shell-filter--server-id
                                 anvil-shell-filter--tool-specs))

;;;###autoload
(defun anvil-shell-filter-enable ()
  "Register Doc 27 shell-* MCP tools and open the state backing store."
  (interactive)
  (anvil-state-enable)
  (anvil-shell-filter--register-tools))

;;;###autoload
(defun anvil-shell-filter-disable ()
  "Unregister shell-* MCP tools."
  (interactive)
  (anvil-shell-filter--unregister-tools))


(provide 'anvil-shell-filter)

;;; anvil-shell-filter.el ends here
