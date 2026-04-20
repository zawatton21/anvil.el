;;; anvil-bisect-entry.el --- Subprocess entry point for anvil-bisect  -*- lexical-binding: t; -*-

;;; Commentary:

;; Invoked by `anvil-bisect--run-step' inside `emacs --batch'.  Loads
;; a single ERT test file, runs one named test, and exits:
;;   0    pass
;;   1    fail / unexpected result
;;   125  skip (test symbol missing, file could not be loaded — the
;;         git-bisect convention for "cannot decide on this commit")
;;
;; Keep this file free of anvil requires so a commit lacking the
;; module we are bisecting can still run the entry cleanly.

;;; Code:

(require 'ert)

(defun anvil-bisect-entry (test-name test-file)
  "Run ERT test TEST-NAME defined in TEST-FILE.
TEST-FILE is interpreted relative to `default-directory' (which
is the worktree when called from `anvil-bisect').  Exits the
Emacs process with 0 on pass, 1 on fail, 125 on skip.

Every non-pass path prints a `[entry]' diagnostic line to stderr
so `anvil-bisect--run-step' (with `--debug' on) can say WHY a
commit skipped.  Without this, the CI log surfaces only git's own
`only skip'ped commits left to test' error, which does not say
what the subprocess actually hit."
  (condition-case err
      (let ((path (expand-file-name test-file))
            (sym  (cond ((symbolp test-name) test-name)
                        ((stringp test-name) (intern test-name))
                        (t (error "test-name must be symbol or string")))))
        (unless (file-readable-p path)
          (message "[entry] skip: cannot read %s (cwd=%s)"
                   path default-directory)
          (kill-emacs 125))
        (load path nil t)
        (let ((test (ignore-errors (ert-get-test sym))))
          (unless test
            (message "[entry] skip: test %S not defined after load of %s"
                     sym path)
            (kill-emacs 125))
          ;; `ert-run-test' runs the test and stores the outcome on
          ;; the test's most-recent-result slot; the function itself
          ;; returns the test struct, not a result object, so we fetch
          ;; the result explicitly before classifying.
          ;;
          ;; Emacs-version-specific hazard: on Emacs 29.4 (and some
          ;; 29.x builds) `ert-run-test' *signals* on test failure
          ;; instead of quietly storing the result.  On Emacs 30.1
          ;; the signal path was removed.  If the call signals here
          ;; we must treat it as "the test ran and failed" (exit 1 —
          ;; definitive bad commit for git bisect), NOT as "can't
          ;; tell" (exit 125 — skip).  Otherwise every bisect step
          ;; on an Emacs-29 CI runner returns skip and git bisect
          ;; converges on "only 'skip'ped commits left to test".
          ;;
          ;; Note: `ert-test-result-type-p' with a quoted symbol
          ;; argument hits a `pcase-exhaustive' mismatch on
          ;; Emacs 30.1 — use the direct `ert-test-passed-p'
          ;; predicate which is stable across versions.
          (condition-case run-err
              (ert-run-test test)
            (error
             (message "[entry] fail: ert-run-test signalled (%s)"
                      (error-message-string run-err))
             (kill-emacs 1)))
          (let ((result (ert-test-most-recent-result test)))
            (if (ert-test-passed-p result)
                (kill-emacs 0)
              (message "[entry] fail: test %S did not pass (result=%S)"
                       sym (and result (type-of result)))
              (kill-emacs 1)))))
    (error
     (message "[entry] skip (error): %s" (error-message-string err))
     (kill-emacs 125))))

(provide 'anvil-bisect-entry)
;;; anvil-bisect-entry.el ends here
