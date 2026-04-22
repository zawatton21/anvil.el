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
;; Raw output is always stored (win or lose) under the `shell-tee'
;; namespace with a TTL so the LLM can reach back for the full bytes
;; when the compressed form hides something material.  `anvil-state'
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

(defconst anvil-shell-filter-supported
  '(git-status git-log git-diff rg find ls pytest ert-batch
               emacs-batch make dispatch tee gain
               gh git-log-graph pip-install npm-install
               docker-ps docker-logs kubectl-get aws-s3-ls
               prettier ruff phase2a-dispatch)
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

(defcustom anvil-shell-filter-tokens-per-char 0.25
  "Approximate tokens-per-character ratio used for gain summaries.
rtk and OpenAI both publish ~4 chars-per-token for English/code,
so the default 0.25 tracks that.  Purely an advisory number — the
byte counts are exact."
  :type 'number
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
    (ruff           . ,#'anvil-shell-filter--ruff))
  "Alist mapping filter tag → pure `(RAW) -> COMPRESSED' function.
Users can `setf' new entries to register additional filters before
Phase 3 adds a public register API.")

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
       (t nil)))))

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


;;;; --- tee + gain statistics ----------------------------------------------

(defun anvil-shell-filter--new-id ()
  "Generate a short tee-id (`t-<epoch>-<rand>')."
  (format "t-%x-%x"
          (truncate (float-time))
          (random #x1000000)))

(defun anvil-shell-filter--tee-put (raw)
  "Store RAW under a fresh tee-id, return the id string."
  (let ((id (anvil-shell-filter--new-id)))
    (anvil-state-set id raw
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
  :raw-size         length of raw stdout (bytes)
  :compressed-size  length of compressed output (bytes)
  :tee-id           id under which the raw stdout was saved
  :stderr           raw stderr (never compressed)
  :truncated        non-nil when `anvil-shell' truncated the buffers

Raw stdout is always tee'd so callers can fetch the full output
via `anvil-shell-filter-tee-get' when compression hid material
detail."
  (let* ((filter-opt (or (plist-get opts :filter) 'auto))
         (timeout (or (plist-get opts :timeout)
                      anvil-shell-filter-default-timeout))
         (cwd (plist-get opts :cwd))
         (resolved (cond
                    ((eq filter-opt 'auto) (anvil-shell-filter-lookup cmd))
                    ((null filter-opt) nil)
                    (t filter-opt)))
         (result (anvil-shell cmd (list :timeout timeout :cwd cwd
                                        :max-output nil)))
         (exit (plist-get result :exit))
         (raw (or (plist-get result :stdout) ""))
         (stderr (or (plist-get result :stderr) ""))
         (truncated (plist-get result :truncated))
         (compressed (anvil-shell-filter-apply resolved raw))
         (raw-size (length raw))
         (compressed-size (length compressed))
         (tee-id (anvil-shell-filter--tee-put raw)))
    (when resolved
      (anvil-shell-filter--gain-record resolved raw-size compressed-size))
    (list :exit exit
          :filter resolved
          :compressed compressed
          :raw-size raw-size
          :compressed-size compressed-size
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
:tee-id :stderr :truncated).  Raw stdout is always saved under
the tee namespace so a follow-up `shell-tee-get' can recover it."
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
           :raw-size (length raw*)
           :compressed-size (length compressed)))))

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
     :description
     "Run a shell command, compress its stdout through a per-command filter
(git-status, git-log, git-diff, rg, find, ls, pytest, ert-batch,
emacs-batch, make), and save the raw output to the tee namespace
for later retrieval via `shell-tee-get'.  `filter=auto' picks a
handler from the first token of CMD; `filter=\"\"` disables
compression.")

    (,(anvil-server-encode-handler #'anvil-shell-filter--tool-shell-filter)
     :id "shell-filter"
     :description
     "Apply a named filter to a string without running a shell command.  Lets
callers re-compress output they already have (from a prior
`shell-tee-get' or a foreign tool)."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-shell-filter--tool-shell-tee-get)
     :id "shell-tee-get"
     :description
     "Fetch raw stdout previously captured by `shell-run' under TEE_ID.
Retention is governed by `anvil-shell-tee-ttl-sec' (default 1h);
expired ids return :found nil."
     :read-only t)

    (,(anvil-server-encode-handler #'anvil-shell-filter--tool-shell-gain)
     :id "shell-gain"
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
