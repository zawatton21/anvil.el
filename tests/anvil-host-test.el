;;; anvil-host-test.el --- Tests for anvil-host -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression / correctness tests for `anvil-shell' and related
;; `anvil-host' helpers.  Focused on behaviours that are easy to get
;; wrong at the `make-process' boundary — most notably that detached
;; descendants spawned from inside the shell must survive
;; the wrapper shell exiting (issue #10).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-host)

;;;; --- issue #10 guard -----------------------------------------------------

(ert-deftest anvil-host-test-shell-preserves-disowned-children ()
  "Regression guard for issue #10.

A detached descendant spawned inside the shell command must survive
`anvil-shell' returning.  `anvil-host--run' gives the shell pipe
streams and closes its process-input pipe immediately because the
API has no stdin channel.  On Linux, `setsid --fork' creates the new
session needed for a descendant to outlive the wrapper shell.  This
is the same mechanism clipboard helpers such as `wl-copy' and
`xclip' use for their owner daemons.

The test schedules a short-delayed background `touch' via
`setsid --fork' and asserts the marker file appears after the shell
has already returned.  If `anvil-host--run' ever regresses to a mode
that reaps the whole subprocess tree on exit, the setsid grandchild
dies before it can touch the file."
  (skip-unless (eq system-type 'gnu/linux))
  (skip-unless (executable-find "setsid"))
  (let* ((marker (make-temp-file "anvil-host-test-detach-"))
         (cmd (format "setsid --fork sh -c 'sleep 0.5; touch %s'"
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

(ert-deftest anvil-host-test-shell-stdin-is-eof ()
  "Shell commands that read stdin must observe immediate EOF."
  (skip-unless (memq system-type '(gnu/linux darwin windows-nt)))
  (let* ((command
          (if (eq system-type 'windows-nt)
              "more >NUL & echo stdin-eof"
            "cat >/dev/null; printf 'stdin-eof\\n'"))
         (res (anvil-shell command '(:timeout 3))))
    (should (eql 0 (plist-get res :exit)))
    (should (equal "stdin-eof\n" (plist-get res :stdout)))
    (should (equal "" (plist-get res :stderr)))))

(ert-deftest anvil-host-test-shell-bounds-output-before-decoding ()
  "A finite :max-output must cap lexical capture, not only returned text."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (skip-unless (executable-find "dd"))
  (let ((decoded-byte-counts nil)
        (decode-output (symbol-function 'anvil-host--decode-output)))
    (cl-letf (((symbol-function 'anvil-host--decode-output)
               (lambda (bytes coding)
                 (push (string-bytes bytes) decoded-byte-counts)
                 (funcall decode-output bytes coding))))
      (let* ((limit 1024)
             (res
              (anvil-shell
               "dd if=/dev/zero bs=1048576 count=1 2>/dev/null"
               (list :timeout 5 :max-output limit))))
        (should (eql 0 (plist-get res :exit)))
        (should (eq t (plist-get res :truncated)))
        (should (= 1048576 (plist-get res :stdout-total-bytes)))
        (should (= 0 (plist-get res :stderr-total-bytes)))
        (should decoded-byte-counts)
        (should
         (cl-every (lambda (size) (<= size limit))
                   decoded-byte-counts))
        (should
         (string-match-p
          "anvil-host: truncated, 1047552 more bytes"
          (plist-get res :stdout)))))))

(ert-deftest anvil-host-test-source-byte-fields-are-opt-in ()
  "Exact source-byte fields are absent unless explicitly requested."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (dolist (opts
           '((:timeout 3)
             (:timeout 3 :include-source-bytes nil)))
    (let ((res (anvil-shell "printf output" opts)))
      (should-not
       (plist-member res :stdout-captured-source-bytes))
      (should-not
       (plist-member res :stderr-captured-source-bytes)))))

(ert-deftest anvil-host-test-source-byte-fields-are-exact-unibyte ()
  "The opt-in fields preserve exact bounded bytes in the source encoding."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (skip-unless (coding-system-p 'cp932-dos))
  (let* ((res
          (anvil-shell
           "printf '\\202\\240X'; printf '\\202\\242Y' >&2"
           '(:timeout 3 :max-output 2 :coding cp932-dos
             :include-source-bytes t)))
         (stdout-source
          (plist-get res :stdout-captured-source-bytes))
         (stderr-source
          (plist-get res :stderr-captured-source-bytes)))
    (should (plist-member res :stdout-captured-source-bytes))
    (should (plist-member res :stderr-captured-source-bytes))
    (should (equal "あ" (plist-get res :stdout-captured)))
    (should (equal "い" (plist-get res :stderr-captured)))
    (should (= 3 (plist-get res :stdout-total-bytes)))
    (should (= 3 (plist-get res :stderr-total-bytes)))
    (should (equal (unibyte-string #x82 #xa0) stdout-source))
    (should (equal (unibyte-string #x82 #xa2) stderr-source))
    (should-not (multibyte-string-p stdout-source))
    (should-not (multibyte-string-p stderr-source))
    (should (= 2 (string-bytes stdout-source)))
    (should (= 2 (string-bytes stderr-source)))))

(ert-deftest anvil-host-test-child-bindings-are-spawn-local ()
  "Dedicated child bindings apply at spawn without leaking into the root."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((root-environment process-environment)
         (root-exec-path exec-path)
         (root-shell shell-file-name)
         (root-switch shell-command-switch)
         (child-environment
          (cons "ANVIL_HOST_CHILD_SCOPE=child"
                (copy-sequence process-environment)))
         (child-exec-path (reverse (copy-sequence exec-path)))
         (child-shell (or (executable-find "sh") shell-file-name))
         (anvil-host-child-process-environment child-environment)
         (anvil-host-child-exec-path child-exec-path)
         (anvil-host-child-shell-file-name child-shell)
         (anvil-host-child-shell-command-switch "-c")
         (original-make-process anvil-host--make-process-primitive)
         observed)
    (cl-letf ((anvil-host--make-process-primitive
               (lambda (&rest args)
                 (setq observed
                       (list process-environment exec-path
                             shell-file-name shell-command-switch))
                 (apply original-make-process args))))
      (let ((result
             (anvil-host--run
              "printf %s \"$ANVIL_HOST_CHILD_SCOPE\""
              'utf-8 temporary-file-directory 3)))
        (should
         (equal
          (list 0 "child" "" 5 0
                (encode-coding-string "child" 'utf-8)
                (encode-coding-string "" 'utf-8))
          result))))
    (should (equal child-environment (nth 0 observed)))
    (should (equal child-exec-path (nth 1 observed)))
    (should (equal child-shell (nth 2 observed)))
    (should (equal "-c" (nth 3 observed)))
    (should (eq root-environment process-environment))
    (should (eq root-exec-path exec-path))
    (should (eq root-shell shell-file-name))
    (should (eq root-switch shell-command-switch))))

(ert-deftest anvil-host-test-shell-nonzero-exit-reported ()
  "A non-zero shell exit is reported in :exit (not raised)."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let ((res (anvil-shell "exit 7" '(:timeout 3))))
    (should (eql 7 (plist-get res :exit)))))

;;;; --- §7.2 late stderr capture (stderr-pipe drain) ----------------------

(ert-deftest anvil-host-test-shell-captures-late-stderr ()
  "Stderr emitted just before exit must be fully captured.
Regression for the race between proc and stderr-pipe-proc
lifecycles: the kernel pipe may still hold bytes when proc dies,
and the pipe-proc only reads them once Emacs services it.  The
post-exit drain captures those late bytes."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let ((res (anvil-shell
              "for i in 1 2 3 4 5; do echo line$i >&2; done; exit 0"
              '(:timeout 5))))
    (should (eql 0 (plist-get res :exit)))
    (let ((err (plist-get res :stderr)))
      (should (string-match-p "line1" err))
      (should (string-match-p "line5" err)))))

(provide 'anvil-host-test)
;;; anvil-host-test.el ends here
