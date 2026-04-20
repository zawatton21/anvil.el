;;; anvil-bisect.el --- Test-driven git bisect for ERT regressions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 13 Phase 1 — a thin, ERT-native, worktree-isolated wrapper
;; over `git bisect' that localizes a failing ERT test to the
;; commit that introduced it.
;;
;; Public entry point:
;;   (anvil-bisect-test TEST-NAME &key good bad test-file
;;                                  timeout-total timeout-per-step)
;; MCP tools:
;;   bisect-test, bisect-cancel, bisect-last-result
;;
;; Each step runs in an isolated emacs --batch subprocess against a
;; dedicated `git worktree' under `temporary-file-directory', so the
;; caller's working tree is undisturbed.  A per-step timer kills any
;; run that exceeds `:timeout-per-step' (default 60s).
;;
;; This module is opinionated:
;;   - ERT only (no buttercup / ecukes)
;;   - bad = HEAD, good = origin/main by default
;;   - single-test bisect (caller already knows what is failing)
;;
;; For longer-running bisects over large ranges, Phase 2 will add
;; `anvil-bisect-expr' and optional `:offload t' dispatch.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-server)


;;;; --- group + constants ------------------------------------------------

(defgroup anvil-bisect nil
  "Test-driven git bisect for Emacs Lisp projects."
  :group 'anvil
  :prefix "anvil-bisect-")

(defconst anvil-bisect--server-id "emacs-eval"
  "Server ID under which bisect-* MCP tools are registered.")

(defcustom anvil-bisect-default-good "origin/main"
  "Default ref considered good when `:good' is omitted.
Set to whatever stable baseline your project tracks — \"main\",
\"master\", or a release tag are all reasonable."
  :type 'string
  :group 'anvil-bisect)

(defcustom anvil-bisect-default-bad "HEAD"
  "Default ref considered bad when `:bad' is omitted."
  :type 'string
  :group 'anvil-bisect)

(defcustom anvil-bisect-default-timeout-total 600
  "Default overall timeout (seconds) for a single bisect run."
  :type 'integer
  :group 'anvil-bisect)

(defcustom anvil-bisect-default-timeout-per-step 60
  "Default per-step timeout (seconds).  A step that exceeds this is
killed and the bisect step is treated as `skip' (git bisect's
non-decisive outcome)."
  :type 'integer
  :group 'anvil-bisect)

(defcustom anvil-bisect-worktree-root temporary-file-directory
  "Parent directory under which per-run worktrees are created."
  :type 'directory
  :group 'anvil-bisect)

(defcustom anvil-bisect-emacs-program (or invocation-name "emacs")
  "Emacs executable used for subprocess steps.  Defaults to the
running Emacs's path so the bisect steps run the same version as
the caller."
  :type 'file
  :group 'anvil-bisect)

(defcustom anvil-bisect-extra-load-path nil
  "Extra directories to prepend to the subprocess load-path via -L.
The current project root and its `tests/' subdir are always
included."
  :type '(repeat directory)
  :group 'anvil-bisect)


;;;; --- runtime state ----------------------------------------------------

(defvar anvil-bisect--active-worktree nil
  "Path to the worktree of the currently-running bisect, or nil.")

(defvar anvil-bisect--active-process nil
  "Current bisect step subprocess, or nil when idle.")

(defvar anvil-bisect--last-result nil
  "Plist describing the most recent bisect outcome.")

(defvar anvil-bisect--cancelled nil
  "Set by `anvil-bisect-cancel' to interrupt the control loop.")

(defvar anvil-bisect--debug
  ;; Default to t in CI environments (the `CI' env var is set by
  ;; GitHub Actions, GitLab, CircleCI, Jenkins, etc.).  CI runs are
  ;; the case where the verbose log actually helps: failures there
  ;; are notoriously hard to reproduce locally, and the per-step
  ;; output is the fastest path to "why did this commit skip?".
  ;; Interactive / local runs stay quiet.
  (and (getenv "CI") t)
  "When non-nil, log step stdout/stderr and exit codes to `*Messages*'.
Useful for test diagnosis; never left on in production except under
CI where the debug output is the primary regression triage signal.")


;;;; --- git helpers ------------------------------------------------------

(defun anvil-bisect--git (dir &rest args)
  "Run git with ARGS in DIR.  Return (EXIT . OUTPUT) where OUTPUT is
the combined stdout/stderr trimmed of trailing newline."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory dir))
           (exit (apply #'call-process "git" nil t nil args)))
      (cons exit (string-trim (buffer-string))))))

(defun anvil-bisect--git-check (dir &rest args)
  "Run git with ARGS in DIR.  Signal on non-zero exit.  Return stdout."
  (let ((r (apply #'anvil-bisect--git dir args)))
    (unless (zerop (car r))
      (error "anvil-bisect: git %s failed in %s: %s"
             (mapconcat #'identity args " ") dir (cdr r)))
    (cdr r)))

(defun anvil-bisect--resolve-ref (repo ref)
  "Resolve REF in REPO to a 40-hex SHA, or signal."
  (anvil-bisect--git-check repo "rev-parse" "--verify" ref))

(defun anvil-bisect--commit-metadata (repo sha)
  "Return (:sha :subject :author :date) for SHA in REPO."
  (let* ((fmt "%H%n%s%n%an%n%aI")
         (raw (anvil-bisect--git-check
               repo "show" "-s" (concat "--format=" fmt) sha))
         (lines (split-string raw "\n" nil)))
    (list :sha (nth 0 lines)
          :subject (nth 1 lines)
          :author (nth 2 lines)
          :date (nth 3 lines))))

(defun anvil-bisect--diff-files (repo sha)
  "Return list of file paths changed in SHA."
  ;; --name-only already suppresses the diff body when paired with
  ;; `--pretty=format:'; adding `--no-patch' would conflict.
  (split-string
   (anvil-bisect--git-check repo "show" "--name-only"
                            "--pretty=format:" sha)
   "\n" t "\\s-+"))


;;;; --- worktree lifecycle ----------------------------------------------

(defun anvil-bisect--make-worktree-path ()
  "Allocate a fresh worktree path under `anvil-bisect-worktree-root'."
  (let ((dir (expand-file-name
              (format "anvil-bisect-%s-%05d"
                      (format-time-string "%s")
                      (random 100000))
              anvil-bisect-worktree-root)))
    ;; Never reuse an existing path; `random' collision would be
    ;; catastrophic on a shared tmpdir.
    (if (file-exists-p dir)
        (anvil-bisect--make-worktree-path)
      dir)))

(defun anvil-bisect--create-worktree (repo ref)
  "Create a detached worktree in REPO pointing at REF.  Return its path."
  (anvil-bisect--git-check repo "worktree" "prune")
  (let ((path (anvil-bisect--make-worktree-path)))
    (anvil-bisect--git-check repo "worktree" "add" "--detach" path ref)
    path))

(defun anvil-bisect--destroy-worktree (repo path)
  "Remove the worktree at PATH from REPO.  Idempotent."
  (when (and path (file-directory-p path))
    ;; `git worktree remove' refuses while bisect is active — reset it.
    (ignore-errors (anvil-bisect--git repo "-C" path "bisect" "reset"))
    (anvil-bisect--git repo "worktree" "remove" "--force" path)
    (when (file-directory-p path)
      (delete-directory path t))))


;;;; --- single-step runner ----------------------------------------------

(defun anvil-bisect--entry-path ()
  "Return absolute path to the subprocess entry file loaded on -l.
Pins to the .el source (never a stale .elc) so the subprocess
always loads the same version the caller is running.  Resolved
against the live source checkout's `load-path', not against the
worktree — the worktree is a historical checkout that may
pre-date this file existing on disk."
  (or (locate-file "anvil-bisect-entry.el" load-path)
      (error "anvil-bisect: cannot locate anvil-bisect-entry.el on load-path")))

(defun anvil-bisect--step-command (worktree test-name test-file)
  "Return the argv list for one bisect step.
TEST-FILE is a path relative to WORKTREE (e.g. \"tests/foo-test.el\")."
  (let ((entry (anvil-bisect--entry-path))
        (extra-L nil))
    (dolist (d anvil-bisect-extra-load-path)
      (setq extra-L (nconc extra-L (list "-L" d))))
    (append
     (list anvil-bisect-emacs-program
           "-Q" "--batch"
           "-L" worktree
           "-L" (expand-file-name "tests" worktree))
     extra-L
     (list "-l" entry
           "--eval"
           (format "(anvil-bisect-entry %S %S)"
                   (if (stringp test-name) test-name
                     (symbol-name test-name))
                   test-file)))))

(defun anvil-bisect--run-step (worktree test-name test-file timeout)
  "Run one bisect step.  Return one of:
  `pass'     — test passed (exit 0)
  `fail'     — test failed (exit 1)
  `skip'     — test could not be loaded / other inconclusive (exit 125)
  `timeout'  — subprocess exceeded TIMEOUT
  `error'    — unexpected exit status; details in
               `anvil-bisect--last-result' :last-step-stderr"
  (let* ((cmd (anvil-bisect--step-command worktree test-name test-file))
         (buf (generate-new-buffer " *anvil-bisect-step*"))
         (outcome nil)
         (timer nil))
    (unwind-protect
        ;; Run the step with its cwd pinned to the worktree so the
        ;; relative TEST-FILE argument resolves correctly inside the
        ;; subprocess (and so `(expand-file-name ...)' on the entry
        ;; side means "relative to the worktree checkout").
        (let* ((default-directory (file-name-as-directory worktree))
               (proc (apply #'start-process
                            "anvil-bisect-step" buf cmd)))
          (setq anvil-bisect--active-process proc)
          (setq timer
                (run-at-time
                 timeout nil
                 (lambda ()
                   (when (process-live-p proc)
                     (setq outcome 'timeout)
                     (interrupt-process proc)
                     (run-at-time
                      1 nil (lambda ()
                              (when (process-live-p proc)
                                (kill-process proc))))))))
          ;; Block until the process exits.  `accept-process-output' yields
          ;; so timer and process sentinel both run.
          (while (process-live-p proc)
            (accept-process-output proc 0.2))
          (unless outcome
            (let ((exit (process-exit-status proc)))
              (when anvil-bisect--debug
                (message "[anvil-bisect] exit=%s out=%s"
                         exit (with-current-buffer buf (buffer-string))))
              (setq outcome
                    (pcase exit
                      (0 'pass)
                      (1 'fail)
                      (125 'skip)
                      (_ 'error)))))
          outcome)
      (when timer (cancel-timer timer))
      (setq anvil-bisect--active-process nil)
      (when (buffer-live-p buf) (kill-buffer buf)))))


;;;; --- control loop ----------------------------------------------------

(defun anvil-bisect--collect-bisect-log (worktree)
  "Return the contents of WORKTREE's BISECT_LOG as a string, or nil.
Works both for a normal checkout and for a linked worktree
(whose git state lives at .git/worktrees/<name>/BISECT_LOG).
Older git versions print a relative `--git-dir' path, which must
be resolved against WORKTREE (not the caller's default-directory)
or the lookup silently reads the wrong repository."
  (let* ((worktree-abs (file-name-as-directory
                        (expand-file-name worktree)))
         (candidate
          (cond
           ((file-directory-p (expand-file-name ".git" worktree-abs))
            (expand-file-name ".git/BISECT_LOG" worktree-abs))
           (t
            (let* ((gitdir (car-safe
                            (split-string
                             (cdr (anvil-bisect--git worktree-abs "rev-parse"
                                                     "--git-dir"))
                             "\n" t))))
              (and gitdir
                   (expand-file-name
                    "BISECT_LOG"
                    ;; Force absolute: if git returned a relative
                    ;; path, anchor it to the worktree.
                    (expand-file-name gitdir worktree-abs))))))))
    (when (and candidate (file-exists-p candidate))
      (with-temp-buffer
        (insert-file-contents candidate)
        (buffer-string)))))

(defun anvil-bisect--first-bad-sha (log)
  "Extract the final \"first bad commit\" SHA from a bisect log string.
Prefers the terminating `# first bad commit' header that git
writes when the range collapses.  When that header is absent
(unusual — skipped-commit layouts), returns the SHA of the
*last* `# bad:' entry in the log rather than the first; the
first `# bad:' is the initial bad ref, not the introducing
commit."
  (when log
    (cond
     ((string-match
       "# first bad commit: \\[\\([0-9a-f]+\\)\\]"
       log)
      (match-string 1 log))
     (t
      (let ((pat "# bad: \\[\\([0-9a-f]+\\)\\]")
            (start 0) (last-sha nil))
        (while (string-match pat log start)
          (setq last-sha (match-string 1 log))
          (setq start (match-end 0)))
        last-sha)))))

(defun anvil-bisect--drive (repo worktree test-name test-file
                                 good bad timeout-total timeout-per-step)
  "Drive a bisect loop inside WORKTREE.  Returns the final outcome plist.
REPO is the main git directory; WORKTREE is the isolated copy
where `git bisect' runs."
  (let* ((t0 (current-time))
         (steps 0)
         (outcome nil)
         (last-step nil)
         (deadline (+ (float-time) timeout-total)))
    ;; Start bisect.  `git bisect start BAD GOOD' declares the range
    ;; in a single invocation and leaves the worktree at the first
    ;; midpoint ready to test.
    (anvil-bisect--git-check worktree "bisect" "start" bad good)
    (catch 'done
      (while t
        (when anvil-bisect--cancelled
          (setq outcome 'cancelled)
          (throw 'done nil))
        (when (> (float-time) deadline)
          (setq outcome 'timeout)
          (throw 'done nil))
        (cl-incf steps)
        (setq last-step
              (anvil-bisect--run-step
               worktree test-name test-file timeout-per-step))
        ;; If cancel arrived *during* the step, its SIGINT will have
        ;; made `--run-step' report `error' (signal-exited process).
        ;; Interpret the cancel flag first so the outer status
        ;; reflects user intent rather than a post-hoc error code.
        (when anvil-bisect--cancelled
          (setq outcome 'cancelled)
          (throw 'done nil))
        (pcase last-step
          ('pass (anvil-bisect--git-check worktree "bisect" "good"))
          ('fail (anvil-bisect--git-check worktree "bisect" "bad"))
          ('skip (anvil-bisect--git-check worktree "bisect" "skip"))
          ('timeout
           ;; Treat as skip so bisect can make progress elsewhere.
           (anvil-bisect--git-check worktree "bisect" "skip"))
          (_
           (setq outcome 'error)
           (throw 'done nil)))
        ;; `git bisect' prints a terminal "first bad commit" line when
        ;; the range collapses to a single commit.  Detect via rev-parse.
        (let* ((log (anvil-bisect--collect-bisect-log worktree)))
          (when (and log (string-match-p "first bad commit" log))
            (setq outcome 'found)
            (throw 'done nil)))))
    (let* ((log (anvil-bisect--collect-bisect-log worktree))
           (sha (and (eq outcome 'found)
                     (anvil-bisect--first-bad-sha log)))
           (meta (and sha (anvil-bisect--commit-metadata repo sha)))
           (elapsed (float-time (time-subtract (current-time) t0))))
      (list :status outcome
            :breaking-sha sha
            :breaking-subject (plist-get meta :subject)
            :breaking-author (plist-get meta :author)
            :breaking-date (plist-get meta :date)
            :steps steps
            :range (format "%s..%s" good bad)
            :elapsed-sec elapsed
            :log log
            :diff-files (and sha (anvil-bisect--diff-files repo sha))))))


;;;; --- public API ------------------------------------------------------

(defun anvil-bisect--repo-root (&optional hint)
  "Return the top-level git directory containing HINT (or `default-directory')."
  (let* ((dir (or hint default-directory))
         (r (anvil-bisect--git dir "rev-parse" "--show-toplevel")))
    (unless (zerop (car r))
      (error "anvil-bisect: %s is not inside a git repository" dir))
    (cdr r)))

(cl-defun anvil-bisect-test (test-name &key good bad test-file
                                            timeout-total timeout-per-step)
  "Bisect the current repo for the commit that broke TEST-NAME.
TEST-NAME is an ERT test symbol or its string name.

Keyword arguments:
  :good   - known-good ref (default `anvil-bisect-default-good')
  :bad    - known-bad ref (default `anvil-bisect-default-bad')
  :test-file - path to the test file that defines TEST-NAME,
               relative to the repo root (e.g. \"tests/foo-test.el\").
               Required — we do not auto-discover test files.
  :timeout-total     - overall deadline in seconds (default 600)
  :timeout-per-step  - per-step deadline in seconds (default 60)

Returns a plist as specified in Doc 13 (:status :breaking-sha
:breaking-subject :breaking-author :breaking-date :steps :range
:elapsed-sec :log :diff-files)."
  (unless test-file
    (error "anvil-bisect-test: :test-file is required"))
  (let* ((repo (anvil-bisect--repo-root))
         (good-ref (or good anvil-bisect-default-good))
         (bad-ref (or bad anvil-bisect-default-bad))
         (t-total (or timeout-total anvil-bisect-default-timeout-total))
         (t-step (or timeout-per-step anvil-bisect-default-timeout-per-step))
         (worktree nil))
    (when anvil-bisect--active-worktree
      (error "anvil-bisect: another bisect is already running in %s"
             anvil-bisect--active-worktree))
    (setq anvil-bisect--cancelled nil)
    (unwind-protect
        (progn
          (anvil-bisect--resolve-ref repo good-ref)
          (anvil-bisect--resolve-ref repo bad-ref)
          (setq worktree (anvil-bisect--create-worktree repo bad-ref))
          (setq anvil-bisect--active-worktree worktree)
          (let ((result (anvil-bisect--drive
                         repo worktree test-name test-file
                         good-ref bad-ref t-total t-step)))
            (setq anvil-bisect--last-result result)
            result))
      (setq anvil-bisect--active-worktree nil)
      (when worktree
        (anvil-bisect--destroy-worktree repo worktree)))))

(defun anvil-bisect-last-result ()
  "Return the plist describing the most recent bisect run, or nil."
  anvil-bisect--last-result)

(defun anvil-bisect-cancel ()
  "Cancel a running bisect.  Sets a flag the control loop polls on
its next iteration; also interrupts the running step subprocess."
  (setq anvil-bisect--cancelled t)
  (when (and anvil-bisect--active-process
             (process-live-p anvil-bisect--active-process))
    (interrupt-process anvil-bisect--active-process))
  t)


;;;; --- MCP tool wrappers -----------------------------------------------

(defun anvil-bisect--coerce-int (v default)
  "Return V as integer with DEFAULT fallback."
  (cond ((integerp v) v)
        ((and (stringp v) (string-match "\\`-?[0-9]+\\'" v))
         (string-to-number v))
        (t default)))

(defun anvil-bisect--tool-test (test_name test_file
                                &optional good bad
                                timeout_total timeout_per_step)
  "Bisect the current repo for the breaking commit of TEST_NAME.

MCP Parameters:
  test_name          - ERT test symbol name (string).
  test_file          - Path to the ERT test file (relative or absolute).
  good               - Optional good ref (default origin/main).
  bad                - Optional bad ref (default HEAD).
  timeout_total      - Optional overall timeout (seconds, default 600).
  timeout_per_step   - Optional per-step timeout (seconds, default 60).

Returns a Doc 13 result plist — :status is one of `found', `timeout',
`cancelled', or `error'."
  (anvil-server-with-error-handling
   (anvil-bisect-test test_name
                      :good (and (stringp good)
                                 (not (string-empty-p good)) good)
                      :bad (and (stringp bad)
                                (not (string-empty-p bad)) bad)
                      :test-file test_file
                      :timeout-total (anvil-bisect--coerce-int
                                      timeout_total
                                      anvil-bisect-default-timeout-total)
                      :timeout-per-step (anvil-bisect--coerce-int
                                         timeout_per_step
                                         anvil-bisect-default-timeout-per-step))))

(defun anvil-bisect--tool-cancel ()
  "Cancel any currently-running bisect.

MCP Parameters: (none)"
  (anvil-server-with-error-handling
   (anvil-bisect-cancel)))

(defun anvil-bisect--tool-last-result ()
  "Return the most recent bisect plist, or nil.

MCP Parameters: (none)"
  (anvil-server-with-error-handling
   (anvil-bisect-last-result)))


;;;; --- module lifecycle -----------------------------------------------

(defun anvil-bisect--register-tools ()
  "Register bisect-* MCP tools."
  (anvil-server-register-tool
   #'anvil-bisect--tool-test
   :id "bisect-test"
   :server-id anvil-bisect--server-id
   :description
   "Bisect the current git repo for the commit that broke an ERT
test.  Runs each step in an isolated git worktree + emacs --batch
subprocess so the caller's working tree is untouched.  Returns
the breaking commit's SHA, subject, author, date, and the files
changed.  Synchronous and can block for minutes; expect long
latency for wide ranges.")

  (anvil-server-register-tool
   #'anvil-bisect--tool-cancel
   :id "bisect-cancel"
   :server-id anvil-bisect--server-id
   :description
   "Cancel the in-flight bisect run, if any.  Interrupts the
current step subprocess and releases its worktree.")

  (anvil-server-register-tool
   #'anvil-bisect--tool-last-result
   :id "bisect-last-result"
   :server-id anvil-bisect--server-id
   :description
   "Return the plist describing the most recent bisect run, or nil
when none has happened yet in this Emacs session."
   :read-only t))

(defun anvil-bisect--unregister-tools ()
  "Remove every bisect-* MCP tool."
  (dolist (id '("bisect-test" "bisect-cancel" "bisect-last-result"))
    (anvil-server-unregister-tool id anvil-bisect--server-id)))

;;;###autoload
(defun anvil-bisect-enable ()
  "Register bisect-* MCP tools."
  (interactive)
  (anvil-bisect--register-tools))

(defun anvil-bisect-disable ()
  "Unregister bisect-* MCP tools."
  (interactive)
  (anvil-bisect--unregister-tools))

(provide 'anvil-bisect)
;;; anvil-bisect.el ends here
