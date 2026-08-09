;;; anvil-eval-server-execute-test.el --- ERT for server-execute cleanup advice -*- lexical-binding: t; -*-

;;; Commentary:

;; A non-local exit during `server-execute' — `(top-level)' chosen in
;; the debugger, or any `throw' unwinding past the eval body — skips
;; the `-print' reply and `server-delete-client' teardown at the end
;; of `server-execute'.  The emacsclient process then blocks in
;; `recvfrom' forever on a connection that will never answer, hanging
;; the stdio bridge that owns it.
;;
;; `anvil-eval--server-execute-cleanup-advice' wraps `server-execute'
;; in `unwind-protect': on abnormal unwind of a wait-for-reply client
;; (dontkill nil, connection still open) it sends an `-error' reply
;; and deletes the connection.  These tests pin that contract using a
;; real localhost socket pair so `process-status' is genuinely `open'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'server)
(require 'anvil)
(require 'anvil-eval)

(defmacro anvil-eval-server-execute-test--with-open-process (var &rest body)
  "Bind VAR to a live network process with status `open' around BODY."
  (declare (indent 1))
  `(let* ((srv (make-network-process
                :name "anvil-cleanup-test-srv" :server t
                :host 'local :service t :family 'ipv4 :noquery t))
          (,var (make-network-process
                 :name "anvil-cleanup-test-cli"
                 :host 'local :service (process-contact srv :service)
                 :family 'ipv4 :noquery t)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-process ,var))
       (ignore-errors (delete-process srv)))))

(ert-deftest anvil-eval-server-execute-test-abnormal-exit-replies ()
  "Abnormal unwind sends an -error reply and deletes the connection."
  (anvil-eval-server-execute-test--with-open-process proc
    (let (sent deleted)
      (cl-letf (((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s)))
                ((symbol-function 'delete-process)
                 (lambda (p) (setq deleted p))))
        (catch 'abort
          (anvil-eval--server-execute-cleanup-advice
           (lambda (&rest _) (throw 'abort nil))
           proc nil nil nil nil nil nil)))
      (should sent)
      (should (string-prefix-p "-error " sent))
      (should (eq deleted proc)))))

(ert-deftest anvil-eval-server-execute-test-dontkill-left-alone ()
  "Abnormal unwind of a dontkill client leaves the connection alone."
  (anvil-eval-server-execute-test--with-open-process proc
    (let (sent deleted)
      (cl-letf (((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s)))
                ((symbol-function 'delete-process)
                 (lambda (p) (setq deleted p))))
        (catch 'abort
          (let ((args (if (>= emacs-major-version 30)
                          (list proc nil nil nil nil t nil)
                        (list proc nil nil nil t nil nil))))
            (apply #'anvil-eval--server-execute-cleanup-advice
                   (lambda (&rest _) (throw 'abort nil))
                   args))))
      (should-not sent)
      (should-not deleted))))

(ert-deftest anvil-eval-server-execute-test-normal-exit-untouched ()
  "When server-execute completes (and tears down itself), advice no-ops."
  (anvil-eval-server-execute-test--with-open-process proc
    (let (sent)
      (cl-letf (((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s))))
        ;; Real server-execute replies and deletes the client itself;
        ;; simulate that teardown in the wrapped function.
        (anvil-eval--server-execute-cleanup-advice
         (lambda (&rest _) (delete-process proc) 'done)
         proc nil nil nil nil nil nil))
      (should-not sent))))

(provide 'anvil-eval-server-execute-test)
;;; anvil-eval-server-execute-test.el ends here
