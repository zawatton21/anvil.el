;;; anvil-eval-test.el --- Tests for anvil-eval helpers + NeLisp backend -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite covering two anvil-eval responsibilities:
;;
;;   1. Async eval result formatting (queue wait vs runtime split,
;;      job lifecycle plist fields).
;;
;;   2. The pure Elisp NeLisp evaluator backend
;;      (`anvil-eval--nelisp' + `anvil-eval--locate-nelisp-source-directory' +
;;      the `nelisp-eval' MCP tool registration).
;;
;; NeLisp tests skip cleanly when the pure Elisp NeLisp source directory
;; cannot be located.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-eval)


;;;; --- async result formatting -------------------------------------------

(defun anvil-eval-test--time-ago (seconds)
  "Return a timestamp SECONDS before now."
  (time-subtract (current-time) (seconds-to-time seconds)))

(ert-deftest anvil-eval-test-result-reports-runtime-for-finished-job ()
  "Finished async jobs report runtime separately from job age."
  (let ((anvil-eval--async-jobs (make-hash-table :test 'equal)))
    (puthash
     "job-1"
     (let* ((start (anvil-eval-test--time-ago 12))
            (run-start (time-add start (seconds-to-time 10)))
            (finish (time-add run-start (seconds-to-time 0.4))))
       (list :status 'done
             :result "\"ok\""
             :expression "(progn \"ok\")"
             :start-time start
             :run-start-time run-start
             :finish-time finish
             :queue-wait-sec 10.0
             :runtime-sec 0.4))
     anvil-eval--async-jobs)
    (let ((out (anvil-eval--result "job-1")))
      (should (string-match-p "^status: done" out))
      (should (string-match-p "^elapsed: [0-9.]+s" out))
      (should (string-match-p "^age: [0-9.]+s" out))
      (should (string-match-p "^queue-wait: 10.0s" out))
      (should (string-match-p "^runtime: 0.4s" out))
      (should (string-match-p "^result: \"ok\"" out)))))

(ert-deftest anvil-eval-test-result-running-before-start-has-no-runtime ()
  "Queued async jobs have age and queue wait, but no runtime."
  (let ((anvil-eval--async-jobs (make-hash-table :test 'equal)))
    (puthash
     "job-1"
     (list :status 'running
           :result nil
           :expression "(sleep-for 1)"
           :start-time (anvil-eval-test--time-ago 3))
     anvil-eval--async-jobs)
    (let ((out (anvil-eval--result "job-1")))
      (should (string-match-p "^status: running" out))
      (should (string-match-p "^queue-wait: [0-9.]+s" out))
      (should (string-match-p "^runtime: N/A" out))
      (should (string-match-p "^result: N/A" out)))))

(ert-deftest anvil-eval-test-result-running-after-start-reports-runtime ()
  "Running async jobs report runtime since their run-start time."
  (let ((anvil-eval--async-jobs (make-hash-table :test 'equal)))
    (puthash
     "job-1"
     (let* ((start (anvil-eval-test--time-ago 4))
            (run-start (time-add start (seconds-to-time 2))))
       (list :status 'running
             :result nil
             :expression "(sleep-for 10)"
             :start-time start
             :run-start-time run-start))
     anvil-eval--async-jobs)
    (let ((out (anvil-eval--result "job-1")))
      (should (string-match-p "^status: running" out))
      (should (string-match-p "^queue-wait: 2.0s" out))
      (should (string-match-p "^runtime: [0-9.]+s" out))
      (should-not (string-match-p "^runtime: N/A" out)))))

(ert-deftest anvil-eval-test-async-records-runtime-fields ()
  "The isolated async path records queue wait and runtime fields."
  (let ((anvil-eval--async-jobs (make-hash-table :test 'equal))
        (anvil-eval--async-counter 0)
        (anvil-eval--async-queue nil)
        (anvil-eval--async-active-count 0)
        (anvil-eval--async-pump-timer nil)
        (anvil-eval--async-shutting-down nil))
    (unwind-protect
        (let* ((started (anvil-eval--async "(+ 1 2)"))
               (job-id
                (replace-regexp-in-string "\\`Job started: " "" started))
               (deadline (+ (float-time) 5.0)))
          (while (and (< (float-time) deadline)
                      (eq 'running
                          (plist-get
                           (gethash job-id anvil-eval--async-jobs)
                           :status)))
            (accept-process-output nil 0.01))
          (let ((job (gethash job-id anvil-eval--async-jobs)))
            (should (eq 'done (plist-get job :status)))
            (should (numberp (plist-get job :queue-wait-sec)))
            (should (numberp (plist-get job :runtime-sec)))
            (should (equal "3" (plist-get job :result))))
          (let ((out (anvil-eval--result job-id)))
            (should (string-match-p "^queue-wait: [0-9.]+s" out))
            (should (string-match-p "^runtime: [0-9.]+s" out))))
      (anvil-eval--async-cancel-all)
      (anvil-offload-stop-repl))))


;;;; --- guards -------------------------------------------------------------

(defun anvil-eval-test--nelisp-source-or-skip ()
  "Return the located pure Elisp NeLisp source directory, or skip."
  (let ((anvil-eval--nelisp-source-cache 'unset))
    (or (anvil-eval--locate-nelisp-source-directory)
        (ert-skip "pure Elisp nelisp source directory not found"))))


;;;; --- locate-nelisp-source-directory ------------------------------------

(defun anvil-eval-test--make-source-dir ()
  "Create a temporary directory that looks like a NeLisp source dir."
  (let ((dir (make-temp-file "anvil-eval-test-nelisp-src-" t)))
    (with-temp-file (expand-file-name "nelisp-read.el" dir))
    (with-temp-file (expand-file-name "nelisp-eval.el" dir))
    (with-temp-file (expand-file-name "nelisp-macro.el" dir))
    dir))

(ert-deftest anvil-eval-test-locate-source-honours-explicit-override ()
  "When `anvil-eval-nelisp-source-directory' points at a source dir,
the resolver returns it without consulting env vars."
  (let* ((tmp (anvil-eval-test--make-source-dir)))
    (unwind-protect
        (let ((anvil-eval-nelisp-source-directory tmp)
              (anvil-eval--nelisp-source-cache 'unset)
              (process-environment
               (cons "NELISP_ELISP_DIR=/nonexistent/should-not-be-used"
                     process-environment)))
          (should (equal (expand-file-name tmp)
                         (anvil-eval--locate-nelisp-source-directory))))
      (when (file-directory-p tmp) (delete-directory tmp t)))))

(ert-deftest anvil-eval-test-locate-source-honours-env-var ()
  "When `anvil-eval-nelisp-source-directory' is nil and $NELISP_ELISP_DIR
points at a source dir, the resolver picks it up."
  (let* ((tmp (anvil-eval-test--make-source-dir)))
    (unwind-protect
        (let ((anvil-eval-nelisp-source-directory nil)
              (anvil-eval--nelisp-source-cache 'unset)
              (process-environment
               (cons (concat "NELISP_ELISP_DIR=" tmp) process-environment)))
          (should (equal (expand-file-name tmp)
                         (anvil-eval--locate-nelisp-source-directory))))
      (when (file-directory-p tmp) (delete-directory tmp t)))))

(ert-deftest anvil-eval-test-locate-source-caches-result ()
  "Second call returns the same source directory without re-probing."
  (anvil-eval-test--nelisp-source-or-skip)
  (let ((first (anvil-eval--locate-nelisp-source-directory))
        (second (anvil-eval--locate-nelisp-source-directory)))
    (should (equal first second))
    (should (stringp first))))


;;;; --- anvil-eval--nelisp -------------------------------------------------

(ert-deftest anvil-eval-test-nelisp-arithmetic ()
  "`(+ 1 2)' on the pure Elisp NeLisp evaluator returns \"3\"."
  (anvil-eval-test--nelisp-source-or-skip)
  (let ((anvil-eval-nelisp-reset-before-eval t))
    (should (equal "3" (anvil-eval--nelisp "(+ 1 2)")))))

(ert-deftest anvil-eval-test-nelisp-keeps-repl-state ()
  "`nelisp-eval' keeps NeLisp definitions across calls by default."
  (anvil-eval-test--nelisp-source-or-skip)
  (let ((anvil-eval-nelisp-reset-before-eval t))
    (should (equal "nelisp-eval-test-repl-x"
                   (anvil-eval--nelisp "(defvar nelisp-eval-test-repl-x 41)"))))
  (let ((anvil-eval-nelisp-reset-before-eval nil))
    (should (equal "42" (anvil-eval--nelisp "(+ nelisp-eval-test-repl-x 1)")))))

(ert-deftest anvil-eval-test-nelisp-keyword-self-eval ()
  "Keywords self-evaluate on the pure Elisp NeLisp evaluator."
  (anvil-eval-test--nelisp-source-or-skip)
  (let ((anvil-eval-nelisp-reset-before-eval t))
    (should (equal ":foo" (anvil-eval--nelisp ":foo")))))

(ert-deftest anvil-eval-test-nelisp-string-len ()
  "Round-trip a string operation through the pure Elisp evaluator."
  (anvil-eval-test--nelisp-source-or-skip)
  (let ((anvil-eval-nelisp-reset-before-eval t))
    (should (equal "5" (anvil-eval--nelisp "(length \"hello\")")))))

(ert-deftest anvil-eval-test-nelisp-missing-source-errors ()
  "When no source directory can be located, the eval helper signals an error."
  (let ((anvil-eval--nelisp-source-cache nil))
    (should-error (anvil-eval--nelisp "(+ 1 2)") :type 'error)))


;;;; --- tool registration --------------------------------------------------

(ert-deftest anvil-eval-test-nelisp-tool-registered ()
  "`anvil-eval-enable' registers a `nelisp-eval' tool that
`anvil-eval-disable' removes again.  Probes via the registry hash
table directly since anvil-server has no public `tool-registered-p'."
  (unwind-protect
      (progn
        (anvil-eval-enable)
        (should (gethash "nelisp-eval"
                         (anvil-server--get-server-tools
                          anvil-eval--server-id))))
    (anvil-eval-disable)
    (should-not (gethash "nelisp-eval"
                         (anvil-server--get-server-tools
                          anvil-eval--server-id)))))


(provide 'anvil-eval-test)
;;; anvil-eval-test.el ends here
