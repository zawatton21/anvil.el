;;; anvil-host-test.el --- Tests for anvil-host -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression / correctness tests for `anvil-shell' and related
;; `anvil-host' helpers.  Focused on behaviours that are easy to get
;; wrong at the `make-process' boundary — most notably that background
;; / disowned descendants spawned from inside the shell must survive
;; the wrapper shell exiting (issue #10).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-host)

;;;; --- issue #10 guard -----------------------------------------------------

(ert-deftest anvil-host-test-shell-preserves-disowned-children ()
  "Regression guard for issue #10.

A background / disowned descendant spawned inside the shell command
must survive `anvil-shell' returning.  Under the Emacs default (PTY
connection), the spawned shell becomes a session leader and the
kernel sends SIGHUP to every session member when it exits — which
kills `wl-copy's background clipboard owner and every `sleep 30 &'
variant, breaking Wayland clipboard support.  `anvil-host--run'
forces `:connection-type 'pipe' so this cannot happen.

The test works by scheduling a short-delayed background `touch' and
asserting the marker file appears after the shell has already
returned.  If PTY SIGHUP propagation is ever re-introduced, the
background process dies before it can touch the file and the test
fails."
  (skip-unless (eq system-type 'gnu/linux))
  (let* ((marker (make-temp-file "anvil-host-test-detach-"))
         (cmd (format "(sleep 0.5; touch %s) & disown"
                      (shell-quote-argument marker))))
    ;; make-temp-file creates the file; delete so the test can
    ;; observe a real re-creation by the detached child.
    (delete-file marker)
    (unwind-protect
        (progn
          (let ((res (anvil-shell cmd '(:timeout 5))))
            (should (eql 0 (plist-get res :exit))))
          (should-not (file-exists-p marker))  ; background not done yet
          (let ((deadline (+ (float-time) 3.0)))
            (while (and (not (file-exists-p marker))
                        (< (float-time) deadline))
              (sleep-for 0.05)))
          (should (file-exists-p marker)))
      (ignore-errors (delete-file marker)))))

;;;; --- stdout / stderr hygiene --------------------------------------------

(ert-deftest anvil-host-test-shell-does-not-leak-sentinel-status ()
  "`Process anvil-host-shell finished' must not appear in captured
:stdout / :stderr.  Emacs's default process sentinel writes that
line on exit; the wrapper silences it with `:sentinel #'ignore'
(and the same for the `:stderr' pipe's own sentinel)."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let ((res (anvil-shell "echo hello; echo world >&2" '(:timeout 3))))
    (should (eql 0 (plist-get res :exit)))
    (let ((out (plist-get res :stdout))
          (err (plist-get res :stderr)))
      (should-not (string-match-p "Process anvil-host-shell" out))
      (should-not (string-match-p "Process anvil-host-shell" err))
      (should (string-match-p "hello" out))
      (should (string-match-p "world" err)))))

;;;; --- basic exit / output semantics --------------------------------------

(ert-deftest anvil-host-test-shell-nonzero-exit-reported ()
  "A non-zero shell exit is reported in :exit (not raised)."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let ((res (anvil-shell "exit 7" '(:timeout 3))))
    (should (eql 7 (plist-get res :exit)))))

(provide 'anvil-host-test)
;;; anvil-host-test.el ends here
