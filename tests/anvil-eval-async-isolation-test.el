;;; anvil-eval-async-isolation-test.el --- Isolated async eval regressions -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression coverage for issue #53's root-Emacs freeze.  The first test runs
;; the historical non-yielding reproducer in a disposable outer child, so an
;; unfixed implementation fails by a bounded timeout instead of wedging ERT.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'anvil-eval)

(defconst anvil-eval-async-isolation-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst anvil-eval-async-isolation-test--root
  (file-name-directory
   (directory-file-name anvil-eval-async-isolation-test--directory)))

(defconst anvil-eval-async-isolation-test--emacs
  (expand-file-name invocation-name invocation-directory))

(defconst anvil-eval-async-isolation-test--async-timeout 8
  "Seconds allowed for the non-yielding isolated job.")

(defconst anvil-eval-async-isolation-test--start-timeout 6
  "Seconds allowed for the non-yielding child to publish its PID.")

(defconst anvil-eval-async-isolation-test--settle-timeout 10
  "Seconds allowed for the timed job to reach terminal state.")

(defconst anvil-eval-async-isolation-test--death-timeout 5
  "Seconds allowed for the killed exact child to disappear.")

(defconst anvil-eval-async-isolation-test--good-timeout 10
  "Seconds allowed for the post-timeout control job.")

(defconst anvil-eval-async-isolation-test--root-timeout
  (+ anvil-eval-async-isolation-test--start-timeout
     anvil-eval-async-isolation-test--settle-timeout
     anvil-eval-async-isolation-test--death-timeout
     anvil-eval-async-isolation-test--good-timeout
     5)
  "Hard outer cap covering every child phase plus five seconds of margin.")

(defun anvil-eval-async-isolation-test--job-id (reply)
  "Extract a job ID from async REPLY."
  (string-remove-prefix "Job started: " reply))

(defun anvil-eval-async-isolation-test--wait (job-id &optional timeout)
  "Wait up to TIMEOUT seconds for JOB-ID and return its job plist."
  (let ((deadline (+ (float-time) (or timeout 10))))
    (while (let ((job (gethash job-id anvil-eval--async-jobs)))
             (and job
                  (eq 'running (plist-get job :status))
                  (< (float-time) deadline)))
      (accept-process-output nil 0.02))
    (gethash job-id anvil-eval--async-jobs)))

(defun anvil-eval-async-isolation-test--assert-offload-clean ()
  "Refuse to discard ownership state before subprocess cleanup converges."
  (let ((deadline (+ (float-time) 1.0)))
    (while (and (hash-table-p anvil-offload--pending)
                (> (hash-table-count anvil-offload--pending) 0)
                (< (float-time) deadline))
      (accept-process-output nil 0.01)))
  (let (owned pending)
    (dolist (table (anvil-offload--registered-ownership-tables))
      (maphash
       (lambda (proc value)
         (push (list table proc value) owned))
       table))
    (when (hash-table-p anvil-offload--pending)
      (maphash (lambda (id _future) (push id pending))
               anvil-offload--pending))
    (when (or (anvil-offload--cleanup-active-p)
              (anvil-offload--submission-active-p)
              anvil-offload--pool
              anvil-offload--pool-retiring-p
              anvil-offload--pool-cleanup-active-p
              anvil-offload--submission-active-p
              anvil-offload--stop-retiring-p
              anvil-offload--retired-pools
              owned
              pending)
      (error
       (concat
        "async isolation cleanup did not converge: "
        "pool=%S pool-retiring=%S cleanup-active=%S submission-active=%S stop-retiring=%S "
        "retired=%S owned=%S pending=%S")
       anvil-offload--pool
       anvil-offload--pool-retiring-p
       anvil-offload--pool-cleanup-active-p
       anvil-offload--submission-active-p
       anvil-offload--stop-retiring-p
       anvil-offload--retired-pools
       owned
       pending))))

(defun anvil-eval-async-isolation-test--reset ()
  "Cancel jobs and reset all async/offload state after custody converges."
  (anvil-eval--async-cancel-all)
  (anvil-offload-stop-repl)
  (anvil-eval-async-isolation-test--assert-offload-clean)
  (setq anvil-eval--async-shutting-down nil
        anvil-eval--async-jobs (make-hash-table :test 'equal)
        anvil-eval--async-queue nil
        anvil-eval--async-active-count 0
        anvil-eval--async-pump-timer nil
        anvil-eval--async-counter 0
        anvil-offload--pool nil
        anvil-offload--round-robin 0
        anvil-offload--pending (make-hash-table :test 'eql)
        anvil-offload--isolated-processes (make-hash-table :test 'eq)
        anvil-offload--pool-retiring-p nil
        anvil-offload--pool-cleanup-active-p nil
        anvil-offload--submission-active-p nil
        anvil-offload--transaction-state (vector nil nil)
        anvil-offload--stop-retiring-p nil
        anvil-offload--retired-pools nil
        anvil-offload--ownership-table-registry nil
        anvil-offload--fallback-processes (make-hash-table :test 'eq)
        anvil-offload--next-id 0)
  (accept-process-output nil 0.05))

(defmacro anvil-eval-async-isolation-test--with-clean-state (&rest body)
  "Run BODY with bounded isolated-eval defaults and unconditional cleanup."
  (declare (indent 0))
  `(let ((anvil-offload--transaction-state (vector nil nil))
         (anvil-offload-emacs-bin
          anvil-eval-async-isolation-test--emacs)
         (anvil-eval-async-max-active 2)
         (anvil-eval-async-max-queued 4)
         (anvil-eval-async-timeout 8)
         (anvil-eval-async-max-timeout 10)
         (anvil-eval-async-retention-seconds 30)
         (anvil-eval-async-max-result-bytes 1024)
         (anvil-offload-max-frame-bytes (* 2 1024 1024)))
     (unwind-protect
         (progn (anvil-eval-async-isolation-test--reset) ,@body)
       (anvil-eval-async-isolation-test--reset))))

(defun anvil-eval-async-isolation-test--run-disposable-root ()
  "Run the non-yielding reproducer in a bounded disposable Emacs."
  (let* ((buffer (generate-new-buffer " *anvil-async-root-regression*"))
         (started-file (make-temp-file "anvil-async-started-"))
         (_deleted (delete-file started-file))
         (loop-expression
          (format
           "(progn (with-temp-file %S (insert (number-to-string (emacs-pid)))) (while t))"
           started-file))
         (child-form
          `(progn
             (require 'anvil-eval)
             (setq anvil-offload-emacs-bin
                   ,anvil-eval-async-isolation-test--emacs
                   anvil-eval-async-max-active 1
                   anvil-eval-async-max-queued 1
                   anvil-eval-async-timeout
                   ,anvil-eval-async-isolation-test--async-timeout
                   anvil-eval-async-max-timeout 10
                   anvil-eval-async-retention-seconds 30)
             (let* ((bad-reply (anvil-eval--async ,loop-expression))
                    (bad-id (string-remove-prefix
                             "Job started: " bad-reply))
                    (start-deadline
                     (+ (float-time)
                        ,anvil-eval-async-isolation-test--start-timeout)))
               (while (and (not (file-exists-p ,started-file))
                           (< (float-time) start-deadline)
                           (eq 'running
                               (plist-get
                                (gethash bad-id anvil-eval--async-jobs)
                                :status)))
                 (accept-process-output nil 0.02))
               (unless (file-exists-p ,started-file)
                 (error
                  "non-yielding child never wrote its start marker: %S"
                  (gethash bad-id anvil-eval--async-jobs)))
               (let* ((bad-job (gethash bad-id anvil-eval--async-jobs))
                      (bad-future (plist-get bad-job :future))
                      (bad-proc
                       (and bad-future
                            (anvil-future--process bad-future)))
                      (recorded-pid
                       (string-to-number
                        (with-temp-buffer
                          (insert-file-contents ,started-file)
                          (buffer-string))))
                      (marker nil)
                      (_timer
                       (run-at-time 0.1 nil
                                    (lambda () (setq marker t))))
                      (deadline
                       (+ (float-time)
                          ,anvil-eval-async-isolation-test--settle-timeout)))
                 (unless (and (anvil-future-p bad-future)
                              (processp bad-proc)
                              (= recorded-pid (process-id bad-proc)))
                   (error "start marker did not identify the exact child"))
                 (while (and (< (float-time) deadline)
                             (eq 'running
                                 (plist-get
                                  (gethash bad-id anvil-eval--async-jobs)
                                  :status)))
                   (accept-process-output nil 0.02))
                 (setq bad-job (gethash bad-id anvil-eval--async-jobs))
                 (unless marker
                   (error "root marker timer did not run during child loop"))
                 (unless (and (eq 'error (plist-get bad-job :status))
                              (eq 'timeout (plist-get bad-job :reason)))
                   (error "non-yielding job did not time out: %S" bad-job))
                 (let ((death-deadline
                        (+ (float-time)
                           ,anvil-eval-async-isolation-test--death-timeout)))
                   (while (and (process-live-p bad-proc)
                               (< (float-time) death-deadline))
                     (accept-process-output bad-proc 0.02 nil t)))
                 (when (process-live-p bad-proc)
                   (error "timed-out exact child survived")))
               (let* ((good-reply (anvil-eval--async "(+ 1 2)"))
                      (good-id (string-remove-prefix
                                "Job started: " good-reply))
                      (deadline
                       (+ (float-time)
                          ,anvil-eval-async-isolation-test--good-timeout)))
                 (while (and (< (float-time) deadline)
                             (eq 'running
                                 (plist-get
                                  (gethash good-id anvil-eval--async-jobs)
                                  :status)))
                   (accept-process-output nil 0.02))
                 (unless (equal "3"
                                (plist-get
                                 (gethash good-id anvil-eval--async-jobs)
                                 :result))
                   (error "fresh isolated job failed")))
               (princ "ANVIL-ASYNC-ISOLATION-OK"))))
         (proc
          (make-process
           :name "anvil-async-root-regression"
           :buffer buffer
           :command
           (list anvil-eval-async-isolation-test--emacs
                 "-Q" "--batch"
                 "-L" anvil-eval-async-isolation-test--root
                 "--eval" (prin1-to-string child-form))
           :connection-type 'pipe
           :noquery t
           :sentinel #'ignore))
         (deadline
          (+ (float-time)
             anvil-eval-async-isolation-test--root-timeout)))
    (unwind-protect
        (progn
          (while (and (process-live-p proc) (< (float-time) deadline))
            (accept-process-output proc 0.05 nil t))
          (when (process-live-p proc)
            (delete-process proc)
            (ert-fail "disposable root froze on non-yielding async eval"))
          (let ((output (with-current-buffer buffer (buffer-string)))
                (status (process-exit-status proc)))
            (unless (= 0 status)
              (ert-fail
               (format "disposable root exited %s:\n%s" status output)))
            (should (string-match-p "ANVIL-ASYNC-ISOLATION-OK" output))))
      (when (process-live-p proc) (delete-process proc))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-exists-p started-file)
        ;; The marker is written by the exact non-yielding child.  Reap that
        ;; PID even when the disposable root crashes before its own cleanup;
        ;; a failing regression must never orphan the process it diagnoses.
        (let ((pid
               (string-to-number
                (with-temp-buffer
                  (insert-file-contents started-file)
                  (buffer-string)))))
          (when (> pid 0)
            (ignore-errors (signal-process pid 9))))
        (delete-file started-file)))))

(defun anvil-eval-async-isolation-test--run-without-emacs-on-path ()
  "Run a trivial async job when only the absolute outer Emacs is usable."
  (let* ((empty-path (make-temp-file "anvil-async-empty-path-" t))
         (buffer (generate-new-buffer " *anvil-async-no-path*"))
         (child-form
          `(progn
             (require 'anvil-eval)
             (unless (and (file-name-absolute-p anvil-offload-emacs-bin)
                          (file-executable-p anvil-offload-emacs-bin))
               (error "offload default is not an executable absolute path: %S"
                      anvil-offload-emacs-bin))
             (when (executable-find "emacs")
               (error "test PATH unexpectedly contains Emacs"))
             (setq anvil-eval-async-max-active 1
                   anvil-eval-async-max-queued 1
                   anvil-eval-async-max-timeout 5
                   anvil-eval-async-retention-seconds 30)
             (unwind-protect
                 (let* ((reply (anvil-eval--async "(+ 20 22)" 3))
                        (id (string-remove-prefix "Job started: " reply))
                        (deadline (+ (float-time) 5)))
                   (while (and (< (float-time) deadline)
                               (eq 'running
                                   (plist-get
                                    (gethash id anvil-eval--async-jobs)
                                    :status)))
                     (accept-process-output nil 0.02))
                   (let ((job (gethash id anvil-eval--async-jobs)))
                     (unless (eq 'done (plist-get job :status))
                       (error "no-PATH async job failed: %S" job))
                     (unless (equal "42" (plist-get job :result))
                       (error "no-PATH async result mismatch: %S" job)))
                   (princ "ANVIL-ASYNC-NO-PATH-OK"))
               (anvil-eval--async-cancel-all))))
         proc
         (deadline (+ (float-time) 8)))
    (unwind-protect
        (progn
          (let ((process-environment (copy-sequence process-environment)))
            (setenv "PATH" empty-path)
            (setq proc
                  (make-process
                   :name "anvil-async-no-path"
                   :buffer buffer
                   :command
                   (list anvil-eval-async-isolation-test--emacs
                         "-Q" "--batch"
                         "-L" anvil-eval-async-isolation-test--root
                         "--eval" (prin1-to-string child-form))
                   :connection-type 'pipe
                   :noquery t
                   :sentinel #'ignore)))
          (while (and (process-live-p proc) (< (float-time) deadline))
            (accept-process-output proc 0.05 nil t))
          (when (process-live-p proc)
            (delete-process proc)
            (ert-fail "async eval did not finish without Emacs on PATH"))
          (let ((output (with-current-buffer buffer (buffer-string))))
            (should (= 0 (process-exit-status proc)))
            (should (string-match-p "ANVIL-ASYNC-NO-PATH-OK" output))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory empty-path t))))

(ert-deftest anvil-eval-async-isolation-nonyielding-does-not-block-root ()
  "A tight loop cannot block the root event loop or poison the next job."
  (anvil-eval-async-isolation-test--run-disposable-root))

(ert-deftest anvil-eval-async-isolation-default-emacs-needs-no-path ()
  "The running Emacs absolute path is the default child executable."
  (anvil-eval-async-isolation-test--run-without-emacs-on-path))

(ert-deftest anvil-eval-async-isolation-mcp-timeout-contract ()
  "The MCP schema is numeric, while cached string callers remain compatible."
  (let* ((schema
          (anvil-server--generate-schema-from-function 'anvil-eval--async))
         (properties (alist-get 'properties schema))
         (timeout-schema (cdr (assoc "timeout" properties)))
         (required (append (alist-get 'required schema) nil)))
    (should (equal "number" (alist-get 'type timeout-schema)))
    (should-not (member "timeout" required)))
  (anvil-eval-async-isolation-test--with-clean-state
    (let* ((reply (anvil-eval--async "(+ 20 22)" "5"))
           (id (anvil-eval-async-isolation-test--job-id reply))
           (job (anvil-eval-async-isolation-test--wait id)))
      (should (eq 'done (plist-get job :status)))
      (should (equal "42" (plist-get job :result))))
    (should-error (anvil-eval--async "(+ 1 2)" "5 seconds")
                  :type 'anvil-server-tool-error)))

(ert-deftest anvil-eval-async-isolation-submission-never-reads-user-source ()
  "The accepting root treats the expression as opaque text."
  (anvil-eval-async-isolation-test--with-clean-state
    (let (reply)
      (cl-letf (((symbol-function 'read-from-string)
                 (lambda (&rest _args)
                   (error "root attempted to read user source"))))
        (setq reply (anvil-eval--async "(+ 20 22)" 8)))
      (let* ((id (anvil-eval-async-isolation-test--job-id reply))
             (job (anvil-eval-async-isolation-test--wait id)))
        (should (eq 'done (plist-get job :status)))
        (should (equal "42" (plist-get job :result)))))))

(ert-deftest anvil-eval-async-isolation-cancel-does-not-affect-peer ()
  "Cancelling one job kills only its child while a peer completes."
  (anvil-eval-async-isolation-test--with-clean-state
    (let* ((slow-id
            (anvil-eval-async-isolation-test--job-id
             (anvil-eval--async "(sleep-for 30)" 8)))
           (peer-id
            (anvil-eval-async-isolation-test--job-id
             (anvil-eval--async "(progn (sleep-for 0.1) 'peer)" 8)))
           (start-deadline (+ (float-time) 3)))
      (while (and (< (float-time) start-deadline)
                  (or (null
                       (plist-get
                        (gethash slow-id anvil-eval--async-jobs) :future))
                      (null
                       (plist-get
                        (gethash peer-id anvil-eval--async-jobs) :future))))
        (accept-process-output nil 0.02))
      (let* ((slow-future
              (plist-get (gethash slow-id anvil-eval--async-jobs) :future))
             (peer-future
              (plist-get (gethash peer-id anvil-eval--async-jobs) :future)))
        (should (anvil-future-p slow-future))
        (should (anvil-future-p peer-future))
        (should-not (eq (anvil-future--process slow-future)
                        (anvil-future--process peer-future))))
      (should (string-match-p "Job cancelled"
                              (anvil-eval--cancel slow-id)))
      (let ((slow (gethash slow-id anvil-eval--async-jobs))
            (peer (anvil-eval-async-isolation-test--wait peer-id)))
        (should (eq 'error (plist-get slow :status)))
        (should (eq 'cancelled (plist-get slow :reason)))
        (should (eq 'done (plist-get peer :status)))
        (should (equal "peer" (plist-get peer :result)))))))

(ert-deftest anvil-eval-async-isolation-results-are-readable-and-bounded ()
  "Unreadable, circular, and oversized values settle as bounded strings."
  (anvil-eval-async-isolation-test--with-clean-state
    (let ((anvil-eval-async-max-result-bytes 64))
      (dolist (case '(("(current-buffer)" . "#<buffer")
                      ("(let ((x (list 'a))) (setcdr x x) x)" . "#1=")
                      ("(make-string 1000 ?x)" . "truncated")))
        (let* ((id
                (anvil-eval-async-isolation-test--job-id
                 (anvil-eval--async (car case) 8)))
               (job (anvil-eval-async-isolation-test--wait id))
               (result (plist-get job :result)))
          (should (eq 'done (plist-get job :status)))
          (should (string-match-p (regexp-quote (cdr case)) result))
          (should (<= (string-bytes result) 64)))))))

(ert-deftest anvil-eval-async-isolation-escape-heavy-result-fits-wire ()
  "Escape-heavy output is truncated to a successful bounded wire frame."
  (anvil-eval-async-isolation-test--with-clean-state
    (let ((anvil-eval-async-max-result-bytes (* 1024 1024))
          (anvil-offload-max-frame-bytes (* 2 1024 1024)))
      (let* ((id
              (anvil-eval-async-isolation-test--job-id
               (anvil-eval--async "(make-string 700000 92)" 8)))
             (start-deadline (+ (float-time) 3))
             future)
        (while (and (null future) (< (float-time) start-deadline))
          (setq future
                (plist-get (gethash id anvil-eval--async-jobs) :future))
          (unless future (accept-process-output nil 0.02)))
        (should (anvil-future-p future))
        (let* ((child (anvil-future--process future))
               (job (anvil-eval-async-isolation-test--wait id 12))
               (result (plist-get job :result))
               (wire
                (concat
                 anvil-offload--frame-prefix
                 (anvil-offload--frame-encode-payload
                  (prin1-to-string
                   (list :id (anvil-future--id future) :ok result)))
                 "\n")))
          (should (eq 'done (plist-get job :status)))
          (should (stringp result))
          (should (string-match-p "truncated" result))
          (should (<= (string-bytes result)
                      anvil-eval-async-max-result-bytes))
          (should (<= (string-bytes wire)
                      anvil-offload-max-frame-bytes))
          (let ((death-deadline (+ (float-time) 2)))
            (while (and (process-live-p child)
                        (< (float-time) death-deadline))
              (accept-process-output child 0.02))
            (should-not (process-live-p child))))))))

(ert-deftest anvil-eval-async-isolation-preserves-request-context ()
  "The child sees request-local environment and directory bindings."
  (anvil-eval-async-isolation-test--with-clean-state
    (let* ((tmp (make-temp-file "anvil-eval-async-context-" t))
           (process-environment (copy-sequence process-environment))
           (default-directory (file-name-as-directory tmp)))
      (unwind-protect
          (progn
            (setenv "ANVIL_ASYNC_CONTEXT" "project-value")
            (let* ((id
                    (anvil-eval-async-isolation-test--job-id
                     (anvil-eval--async
                      "(list (getenv \"ANVIL_ASYNC_CONTEXT\") default-directory)"
                      8)))
                   (job (anvil-eval-async-isolation-test--wait id)))
              (should (eq 'done (plist-get job :status)))
              (should (equal
                       (format "(%S %S)" "project-value" default-directory)
                       (plist-get job :result)))))
        (delete-directory tmp t)))))

(ert-deftest anvil-eval-async-isolation-queue-is-bounded ()
  "Active plus queued limits reject excess jobs without spawning them."
  (anvil-eval-async-isolation-test--with-clean-state
    (let ((anvil-eval-async-max-active 1)
          (anvil-eval-async-max-queued 1))
      (anvil-eval--async "(sleep-for 30)" 8)
      (anvil-eval--async "(sleep-for 30)" 8)
      (should-error (anvil-eval--async "(+ 1 2)" 3)
                    :type 'anvil-server-tool-error))))

(ert-deftest anvil-eval-async-isolation-polling-format-compatible ()
  "Job IDs and the established polling fields remain compatible."
  (anvil-eval-async-isolation-test--with-clean-state
    (let* ((reply (anvil-eval--async "(+ 2 3)" 8))
           (id (anvil-eval-async-isolation-test--job-id reply)))
      (should (string-match-p "\\`job-[0-9]+-[0-9]+\\'" id))
      (anvil-eval-async-isolation-test--wait id)
      (let ((result (anvil-eval--result id)))
        (dolist (line '("status: done" "elapsed:" "age:" "queue-wait:"
                        "runtime:" "result: 5"))
          (should (string-match-p (regexp-quote line) result)))))))

(defun anvil-eval-async-isolation-test--idle-process (name)
  "Return a quiet subprocess named NAME for protocol-filter tests."
  (make-process
   :name name
   :command (list shell-file-name shell-command-switch "sleep 30")
   :connection-type 'pipe
   :noquery t
   :sentinel #'ignore))

(defun anvil-eval-async-isolation-test--cleanup-owned-processes
    (&rest processes)
  "Hard-delete PROCESSES and prove all dynamically bound custody converged."
  (dolist (proc processes)
    (when (processp proc)
      (when (process-live-p proc)
        (set-process-sentinel proc #'anvil-offload--sentinel))
      (anvil-offload--hard-delete-process proc)))
  (anvil-offload-stop-repl)
  (anvil-eval-async-isolation-test--assert-offload-clean))

(defun anvil-eval-async-isolation-test--frame (message)
  "Encode MESSAGE as one complete offload protocol frame."
  (concat anvil-offload--frame-prefix
          (anvil-offload--frame-encode-payload
           (prin1-to-string message))
          "\n"))

(ert-deftest
    anvil-eval-async-isolation-terminal-reply-hard-deletes-child ()
  "A successful child cannot survive by blocking its own exit hooks."
  (anvil-eval-async-isolation-test--with-clean-state
    (let* ((reply
            (anvil-eval--async
             "(progn (add-hook 'kill-emacs-hook (lambda () (while t))) 42)"
             8))
           (id (anvil-eval-async-isolation-test--job-id reply))
           (start-deadline (+ (float-time) 3))
           future)
      (while (and (null future) (< (float-time) start-deadline))
        (setq future
              (plist-get (gethash id anvil-eval--async-jobs) :future))
        (unless future (accept-process-output nil 0.02)))
      (should (anvil-future-p future))
      (let ((child (anvil-future--process future)))
        (unwind-protect
            (let ((job (anvil-eval-async-isolation-test--wait id)))
              (should (eq 'done (plist-get job :status)))
              (should (equal "42" (plist-get job :result)))
              (let ((death-deadline (+ (float-time) 2)))
                (while (and (process-live-p child)
                            (< (float-time) death-deadline))
                  (accept-process-output child 0.02 nil t)))
              (should-not (process-live-p child)))
          (when (process-live-p child)
            (delete-process child)))))))

(ert-deftest
    anvil-eval-async-isolation-terminal-cleanup-survives-nonlocal-callback ()
  "A settling callback cannot strand its exact isolated child."
  (anvil-eval-async-isolation-test--with-clean-state
    (let* ((future
            (anvil-offload-isolated
             '(progn
                (add-hook 'kill-emacs-hook (lambda () (while t)))
                42)
             :on-settle
             (lambda (_future)
               (throw 'anvil-async-test-nonlocal 'escaped))))
           (child (anvil-future--process future)))
      (unwind-protect
          (progn
            (should
             (eq 'escaped
                 (catch 'anvil-async-test-nonlocal
                   (anvil-future-await future 5)
                   'did-not-escape)))
            (should (eq 'done (anvil-future-status future)))
            (should (equal 42 (anvil-future-value future)))
            (let ((death-deadline (+ (float-time) 2)))
              (while (and (process-live-p child)
                          (< (float-time) death-deadline))
                (accept-process-output child 0.02 nil t)))
            (should-not (process-live-p child)))
        (when (process-live-p child)
          (delete-process child))))))

(ert-deftest anvil-eval-async-isolation-callback-quit-is-contained ()
  "A settling callback's quit cannot escape the process filter."
  (anvil-eval-async-isolation-test--with-clean-state
    (let ((future
           (anvil-offload-isolated
            '(+ 40 2)
            :on-settle (lambda (_future) (signal 'quit nil)))))
      (should (anvil-future-await future 5))
      (should (eq 'done (anvil-future-status future)))
      (should (equal 42 (anvil-future-value future))))))

(ert-deftest
    anvil-eval-async-isolation-start-callback-nonlocal-kills-child ()
  "A nonlocal start callback cannot strand a pending isolated child."
  (anvil-eval-async-isolation-test--with-clean-state
    (let* ((future
            (anvil-offload-isolated
             '(while t)
             :on-start
             (lambda (_future)
               (throw 'anvil-async-start-nonlocal 'escaped))))
           (child (anvil-future--process future)))
      (unwind-protect
          (progn
            (should
             (eq 'escaped
                 (catch 'anvil-async-start-nonlocal
                   (anvil-future-await future 5)
                   'did-not-escape)))
            (should (eq 'killed (anvil-future-status future)))
            (should-not
             (gethash (anvil-future--id future)
                      (anvil-offload--ensure-pending)))
            (let ((death-deadline (+ (float-time) 2)))
              (while (and (process-live-p child)
                          (< (float-time) death-deadline))
                (accept-process-output child 0.02 nil t)))
            (should-not (process-live-p child)))
        (when (process-live-p child)
          (delete-process child))))))

(ert-deftest
    anvil-eval-async-isolation-hard-kill-survives-nonlocal-callback ()
  "A nonlocal settle callback cannot bypass an explicit hard kill."
  (anvil-eval-async-isolation-test--with-clean-state
    (let* ((future
            (anvil-offload-isolated
             '(while t)
             :on-settle
             (lambda (_future)
               (throw 'anvil-async-kill-nonlocal 'escaped))))
           (child (anvil-future--process future))
           (start-deadline (+ (float-time) 3)))
      (unwind-protect
          (progn
            (while (and (null (anvil-future--started-at future))
                        (< (float-time) start-deadline))
              (accept-process-output child 0.02 nil t))
            (should (anvil-future--started-at future))
            (should
             (eq 'escaped
                 (catch 'anvil-async-kill-nonlocal
                   (anvil-future-kill future 'test-kill)
                   'did-not-escape)))
            (should (eq 'killed (anvil-future-status future)))
            (should-not
             (gethash (anvil-future--id future)
                      (anvil-offload--ensure-pending)))
            (let ((death-deadline (+ (float-time) 2)))
              (while (and (process-live-p child)
                          (< (float-time) death-deadline))
                (accept-process-output child 0.02 nil t)))
            (should-not (process-live-p child)))
        (when (process-live-p child)
          (delete-process child))))))

(ert-deftest
    anvil-eval-async-isolation-pump-kills-terminalized-spawn ()
  "A job terminalized during spawn cannot orphan its fresh child."
  (anvil-eval-async-isolation-test--with-clean-state
    (let ((proc
           (anvil-eval-async-isolation-test--idle-process
            "anvil-async-stale-spawn"))
          job-id
          future)
      (unwind-protect
          (cl-letf
              (((symbol-function 'anvil-eval--async-schedule-pump)
                #'ignore)
               ((symbol-function 'anvil-offload-isolated)
                (lambda (_form &rest keys)
                  (setq future
                        (make-anvil-future
                         :id 701
                         :process proc
                         :status 'pending
                         :isolated-p t
                         :on-start (plist-get keys :on-start)
                         :on-settle (plist-get keys :on-settle)))
                  (puthash 701 future (anvil-offload--ensure-pending))
                  (puthash proc t
                           (anvil-offload--ensure-isolated-processes))
                  (anvil-eval--async-terminalize
                   job-id 'error "Error: cancelled during spawn" 'cancelled)
                  future)))
            (setq job-id
                  (anvil-eval-async-isolation-test--job-id
                   (anvil-eval--async "(while t)" 8)))
            (anvil-eval--async-pump)
            (let ((job (gethash job-id anvil-eval--async-jobs)))
              (should (eq 'error (plist-get job :status)))
              (should (eq 'cancelled (plist-get job :reason)))
              (should (eq 'killed (anvil-future-status future)))
              (should (eq 'stale-job
                          (anvil-future--terminal-reason future)))
              (should-not
               (gethash 701 (anvil-offload--ensure-pending)))
              (should-not
               (gethash proc (anvil-offload--ensure-isolated-processes)))
              (should-not (process-live-p proc))
              (should (= 0 anvil-eval--async-active-count))))
        (when (process-live-p proc)
          (delete-process proc))))))

(ert-deftest
    anvil-eval-async-isolation-hard-delete-preserves-live-tracking ()
  "Failed hard deletion cannot untrack a still-live owned child."
  (let* ((proc
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-hard-delete-tracking"))
         (anvil-offload--pool (vector proc))
         (anvil-offload--pool-retiring-p nil)
         (anvil-offload--transaction-state (vector nil nil))
         (anvil-offload--pool-cleanup-active-p nil)
         (anvil-offload--isolated-processes (make-hash-table :test 'eq))
         (anvil-offload--submission-active-p nil)
         (anvil-offload--stop-retiring-p nil)
         (anvil-offload--retired-pools nil)
         (anvil-offload--ownership-table-registry nil)
         (anvil-offload--fallback-processes (make-hash-table :test 'eq))
         (anvil-offload--pending (make-hash-table :test 'eql)))
    (puthash proc t anvil-offload--isolated-processes)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'kill-process) #'ignore)
                    ((symbol-function 'delete-process) #'ignore))
            (anvil-offload--hard-delete-process proc))
          (should (process-live-p proc))
          (should (eq proc (aref anvil-offload--pool 0)))
          (should (gethash proc anvil-offload--isolated-processes))
          (cl-letf
              (((symbol-function 'kill-process)
                (lambda (_proc) (error "kill refused")))
               ((symbol-function 'delete-process)
                (lambda (_proc) (error "delete refused"))))
            (anvil-offload--hard-delete-process proc))
          (should (process-live-p proc))
          (should (eq proc (aref anvil-offload--pool 0)))
          (should (gethash proc anvil-offload--isolated-processes))
          (should (anvil-offload--hard-delete-process proc))
          (should-not (process-live-p proc))
          (should-not (aref anvil-offload--pool 0))
          (should-not (gethash proc anvil-offload--isolated-processes)))
      (anvil-eval-async-isolation-test--cleanup-owned-processes proc))))

(ert-deftest
    anvil-eval-async-isolation-pool-resize-preserves-live-tracking ()
  "Partial retirement fails closed until every old pooled child exits."
  (let* ((stubborn
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-resize-stubborn"))
         (disposable
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-resize-disposable"))
         (pool (vector disposable stubborn))
         (anvil-offload--pool pool)
         (anvil-offload--pool-retiring-p nil)
         (anvil-offload--transaction-state (vector nil nil))
         (anvil-offload--pool-cleanup-active-p nil)
         (anvil-offload--submission-active-p nil)
         (anvil-offload--stop-retiring-p nil)
         (anvil-offload--retired-pools nil)
         (anvil-offload--ownership-table-registry nil)
         (anvil-offload--fallback-processes (make-hash-table :test 'eq))
         (anvil-offload-pool-size 1)
         (real-delete (symbol-function 'delete-process))
         spawned)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'kill-process) #'ignore)
               ((symbol-function 'delete-process)
                (lambda (proc)
                  (unless (eq proc stubborn)
                    (funcall real-delete proc))))
               ((symbol-function 'anvil-offload--spawn-process)
                (lambda (_idx)
                  (setq spawned t)
                  (error "replacement spawned"))))
            (should-error
             (anvil-offload--ensure-pool-vector)
             :type 'error)
            (should anvil-offload--pool-retiring-p)
            (should-not anvil-offload--pool-cleanup-active-p)
            (should (eq pool anvil-offload--pool))
            (should-not (aref anvil-offload--pool 0))
            (should (eq stubborn (aref anvil-offload--pool 1)))
            (should-error (anvil-offload--pick-worker) :type 'error))
          (should-not spawned)
          (should (process-live-p stubborn))
          (delete-process stubborn)
          (should (= 1 (length (anvil-offload--ensure-pool-vector))))
          (should-not anvil-offload--pool-retiring-p)
          (should-not (aref anvil-offload--pool 0)))
      (anvil-eval-async-isolation-test--cleanup-owned-processes
       stubborn disposable))))

(ert-deftest
    anvil-eval-async-isolation-retirement-reentrancy-never-spawns ()
  "Pool retirement marks its generation and rejects reentrant dispatch."
  (let* ((first
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-retire-reentrant-first"))
         (second
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-retire-reentrant-second"))
         (pool (vector first second))
         (anvil-offload--pool pool)
         (anvil-offload--pool-retiring-p nil)
         (anvil-offload--transaction-state (vector nil nil))
         (anvil-offload--pool-cleanup-active-p nil)
         (anvil-offload--submission-active-p nil)
         (anvil-offload--stop-retiring-p nil)
         (anvil-offload--retired-pools nil)
         (anvil-offload--ownership-table-registry nil)
         (anvil-offload--fallback-processes (make-hash-table :test 'eq))
         (anvil-offload-pool-size 1)
         observed
         reentrant-error
         spawned)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'kill-process)
                (lambda (_proc)
                  (unless observed
                    (setq observed
                          (list anvil-offload--pool-retiring-p
                                anvil-offload--pool-cleanup-active-p
                                (process-get first 'anvil-offload-retiring)
                                (process-get second 'anvil-offload-retiring))
                          reentrant-error
                          (should-error
                           (anvil-offload--pick-worker)
                           :type 'error)))))
               ((symbol-function 'delete-process) #'ignore)
               ((symbol-function 'anvil-offload--spawn-process)
                (lambda (_idx)
                  (setq spawned t)
                  (error "replacement spawned"))))
            (should-error
             (anvil-offload--ensure-pool-vector)
             :type 'error))
          (should (equal observed '(t t t t)))
          (should (string-match-p
                   "cleanup already active"
                   (error-message-string reentrant-error)))
          (should-not spawned)
          (should (eq pool anvil-offload--pool))
          (should anvil-offload--pool-retiring-p)
          (should-not anvil-offload--pool-cleanup-active-p))
      (anvil-eval-async-isolation-test--cleanup-owned-processes
       first second))))

(ert-deftest
    anvil-eval-async-isolation-protocol-replacement-preserves-live-tracking ()
  "A failed slot-local upgrade retains its child and leaves peers usable."
  (let* ((old
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-protocol-tracking"))
         (peer
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-protocol-peer"))
         (pool (vector old peer))
         (anvil-offload--pool pool)
         (anvil-offload--pool-retiring-p nil)
         (anvil-offload--transaction-state (vector nil nil))
         (anvil-offload--pool-cleanup-active-p nil)
         (anvil-offload--submission-active-p nil)
         (anvil-offload--stop-retiring-p nil)
         (anvil-offload--retired-pools nil)
         (anvil-offload--ownership-table-registry nil)
         (anvil-offload--fallback-processes (make-hash-table :test 'eq))
         (anvil-offload-pool-size 2)
         (anvil-offload--next-id 41)
         (peer-future
          (make-anvil-future :id 706 :process peer :status 'pending))
         spawned
         isolated-spawned
         reentrant-error
         peer-reentrant-error)
    (process-put old 'anvil-offload-protocol-version
                 (1- anvil-offload--protocol-version))
    (process-put peer 'anvil-offload-protocol-version
                 anvil-offload--protocol-version)
    (puthash 706 peer-future (anvil-offload--ensure-pending))
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'kill-process)
                (lambda (_proc)
                  (unless reentrant-error
                    (setq reentrant-error
                          (should-error
                           (anvil-offload '(+ 1 2) :isolated t)
                           :type 'error)
                          peer-reentrant-error
                          (should-error
                           (anvil-offload--ensure-slot 1)
                           :type 'error)))))
               ((symbol-function 'delete-process) #'ignore)
               ((symbol-function 'anvil-offload--spawn-isolated-process)
                (lambda (_id)
                  (setq isolated-spawned t)
                  (error "isolated replacement spawned")))
               ((symbol-function 'anvil-offload--spawn-process)
                (lambda (_idx)
                  (setq spawned t)
                  (error "replacement spawned"))))
            (should-error (anvil-offload--ensure-slot 0) :type 'error))
          (should-not spawned)
          (should-not isolated-spawned)
          (should (= 41 anvil-offload--next-id))
          (dolist (condition (list reentrant-error peer-reentrant-error))
            (should (string-match-p
                     "cleanup already active"
                     (error-message-string condition))))
          (should (eq pool anvil-offload--pool))
          (should (eq old (aref anvil-offload--pool 0)))
          (should (process-get old 'anvil-offload-retiring))
          (should (process-live-p old))
          (should-not anvil-offload--pool-retiring-p)
          (should-not anvil-offload--pool-cleanup-active-p)
          (should (eq peer (anvil-offload--ensure-slot 1)))
          (should (eq peer-future
                      (gethash 706 (anvil-offload--ensure-pending)))))
      (remhash 706 (anvil-offload--ensure-pending))
      (anvil-eval-async-isolation-test--cleanup-owned-processes
       old peer))))

(defun anvil-eval-async-isolation-test--assert-slot-replacement (dead)
  "Prove one successful slot replacement preserves a pending healthy peer.
When DEAD is non-nil, replace a dead current-protocol slot; otherwise
replace a live protocol-incompatible slot."
  (let* ((target
          (anvil-eval-async-isolation-test--idle-process
           (if dead "anvil-async-dead-target" "anvil-async-old-target")))
         (peer
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-replacement-peer"))
         (replacement
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-replacement-new"))
         (pool (vector target peer))
         (ownership (make-hash-table :test 'eq))
         (anvil-offload--pool pool)
         (anvil-offload--pool-retiring-p nil)
         (anvil-offload--transaction-state (vector nil nil))
         (anvil-offload--pool-cleanup-active-p nil)
         (anvil-offload--submission-active-p nil)
         (anvil-offload--stop-retiring-p nil)
         (anvil-offload--retired-pools nil)
         (anvil-offload--ownership-table-registry nil)
         (anvil-offload--fallback-processes (make-hash-table :test 'eq))
         (anvil-offload-pool-size 2)
         (anvil-offload--isolated-processes ownership)
         (peer-future
          (make-anvil-future :id 707 :process peer :status 'pending))
         (real-remhash anvil-offload--remhash-primitive)
         spawn-inhibit
         publication-observed)
    (process-put target 'anvil-offload-protocol-version
                 (if dead
                     anvil-offload--protocol-version
                   (1- anvil-offload--protocol-version)))
    (process-put peer 'anvil-offload-protocol-version
                 anvil-offload--protocol-version)
    (process-put replacement 'anvil-offload-protocol-version
                 anvil-offload--protocol-version)
    (when dead
      (delete-process target))
    (puthash 707 peer-future (anvil-offload--ensure-pending))
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'anvil-offload--spawn-process)
                (lambda (_idx)
                  (setq spawn-inhibit inhibit-quit)
                  replacement))
               (anvil-offload--remhash-primitive
                (lambda (key table)
                  (when (and (eq key replacement)
                             (eq table ownership))
                    (setq publication-observed
                          (list (eq replacement (aref pool 0))
                                (eq :slot-staging
                                    (gethash replacement ownership))
                                inhibit-quit)))
                  (funcall real-remhash key table))))
            (should (eq replacement (anvil-offload--ensure-slot 0))))
          (should spawn-inhibit)
          (should (equal publication-observed '(t t t)))
          (should (eq pool anvil-offload--pool))
          (should (eq replacement (aref pool 0)))
          (should (eq peer (aref pool 1)))
          (should (eq peer-future
                      (gethash 707 (anvil-offload--ensure-pending))))
          (should-not (gethash replacement ownership))
          (should-not
           (process-get replacement 'anvil-offload-ownership-tables))
          (should-not
           (process-get replacement 'anvil-offload-staged-slot))
          (should-not anvil-offload--pool-retiring-p)
          (should-not anvil-offload--pool-cleanup-active-p))
      (remhash 707 (anvil-offload--ensure-pending))
      (anvil-eval-async-isolation-test--cleanup-owned-processes
       target peer replacement))))

(ert-deftest
    anvil-eval-async-isolation-protocol-replacement-preserves-peer ()
  "Successful protocol replacement is slot-local."
  (anvil-eval-async-isolation-test--assert-slot-replacement nil))

(ert-deftest
    anvil-eval-async-isolation-dead-slot-replacement-preserves-peer ()
  "Successful dead-slot replacement is slot-local."
  (anvil-eval-async-isolation-test--assert-slot-replacement t))

(ert-deftest
    anvil-eval-async-isolation-slot-spawn-rollback-retains-ownership ()
  "Rollback restages ownership across table swaps until natural death."
  (let* ((staged
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-slot-staging"))
         (pool (vector nil))
         (replacement (vector nil))
         (initial-table (make-hash-table :test 'eq))
         (replacement-table (make-hash-table :test 'eq))
         (anvil-offload--pool pool)
         (anvil-offload--pool-retiring-p nil)
         (anvil-offload--transaction-state (vector nil nil))
         (anvil-offload--pool-cleanup-active-p nil)
         (anvil-offload--submission-active-p nil)
         (anvil-offload--stop-retiring-p nil)
         (anvil-offload--retired-pools nil)
         (anvil-offload--ownership-table-registry nil)
         (anvil-offload--fallback-processes (make-hash-table :test 'eq))
         (anvil-offload-pool-size 1)
         (anvil-offload--isolated-processes initial-table)
         spawn-inhibit)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'anvil-offload--spawn-process)
                (lambda (_idx)
                  (setq spawn-inhibit inhibit-quit
                        anvil-offload--pool replacement
                        anvil-offload--isolated-processes replacement-table)
                  staged))
               ((symbol-function 'kill-process) #'ignore)
               ((symbol-function 'delete-process) #'ignore))
            (should-error (anvil-offload--ensure-slot 0) :type 'error))
          (should spawn-inhibit)
          (should (eq replacement anvil-offload--pool))
          (should (eq replacement-table
                      anvil-offload--isolated-processes))
          (should (process-live-p staged))
          (dolist (table (list initial-table replacement-table))
            (should (eq :slot-staging (gethash staged table))))
          (let ((tables
                 (process-get staged 'anvil-offload-ownership-tables))
                (expected
                 (list initial-table replacement-table
                       anvil-offload--fallback-processes)))
            (should-not
             (cl-set-exclusive-or
              (copy-sequence tables) expected :test #'eq)))
          (should (eql 0 (process-get staged 'anvil-offload-staged-slot)))
          (should-not anvil-offload--pool-cleanup-active-p)
          ;; Exercise the sentinel path, not hard-delete's synchronous release.
          (set-process-sentinel staged #'anvil-offload--sentinel)
          (delete-process staged)
          (let ((deadline (+ (float-time) 1.0)))
            (while (and (process-live-p staged)
                        (< (float-time) deadline))
              (accept-process-output staged 0.01 nil t)))
          (accept-process-output staged 0.01 nil t)
          (should-not (process-live-p staged))
          (dolist (table (list initial-table replacement-table))
            (should-not (gethash staged table)))
          (should-not
           (process-get staged 'anvil-offload-ownership-tables))
          (should-not
           (process-get staged 'anvil-offload-staged-slot)))
      (anvil-eval-async-isolation-test--cleanup-owned-processes
       staged))))

(ert-deftest
    anvil-eval-async-isolation-slot-spawn-collision-preserves-peer ()
  "A post-spawn slot collision cannot overwrite a peer or orphan the child."
  (let* ((staged
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-slot-collision-staged"))
         (peer
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-slot-collision-peer"))
         (pool (vector nil))
         (ownership (make-hash-table :test 'eq))
         (anvil-offload--pool pool)
         (anvil-offload--pool-retiring-p nil)
         (anvil-offload--transaction-state (vector nil nil))
         (anvil-offload--pool-cleanup-active-p nil)
         (anvil-offload--submission-active-p nil)
         (anvil-offload--stop-retiring-p nil)
         (anvil-offload--retired-pools nil)
         (anvil-offload--ownership-table-registry nil)
         (anvil-offload--fallback-processes (make-hash-table :test 'eq))
         (anvil-offload-pool-size 1)
         (anvil-offload--isolated-processes ownership)
         spawn-inhibit)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'anvil-offload--spawn-process)
                (lambda (_idx)
                  (setq spawn-inhibit inhibit-quit)
                  (aset pool 0 peer)
                  staged))
               ((symbol-function 'kill-process) #'ignore)
               ((symbol-function 'delete-process) #'ignore))
            (should-error (anvil-offload--ensure-slot 0) :type 'error))
          (should spawn-inhibit)
          (should (eq peer (aref pool 0)))
          (should (process-live-p peer))
          (should (process-live-p staged))
          (should (eq :slot-staging (gethash staged ownership)))
          (should (memq ownership
                        (process-get
                         staged 'anvil-offload-ownership-tables)))
          (should-not anvil-offload--pool-cleanup-active-p)
          (anvil-offload--hard-delete-process staged)
          (should-not (process-live-p staged))
          (should-not (gethash staged ownership)))
      (anvil-eval-async-isolation-test--cleanup-owned-processes
       staged peer))))

(ert-deftest
    anvil-eval-async-isolation-stop-repl-preserves-live-tracking ()
  "A failed stop retires retained children and prevents their reuse."
  (let* ((pooled
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-stop-pooled"))
         (isolated
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-stop-isolated"))
         (anvil-offload--pool (vector pooled))
         (anvil-offload--pool-retiring-p nil)
         (anvil-offload--transaction-state (vector nil nil))
         (anvil-offload--pool-cleanup-active-p nil)
         (anvil-offload--submission-active-p nil)
         (anvil-offload--stop-retiring-p nil)
         (anvil-offload--retired-pools nil)
         (anvil-offload--ownership-table-registry nil)
         (anvil-offload--fallback-processes (make-hash-table :test 'eq))
         (anvil-offload-pool-size 1)
         (anvil-offload--next-id 41)
         (anvil-offload--isolated-processes (make-hash-table :test 'eq))
         spawned
         isolated-spawned
         reentrant-error)
    (process-put pooled 'anvil-offload-protocol-version
                 anvil-offload--protocol-version)
    (puthash isolated t anvil-offload--isolated-processes)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'kill-process)
                (lambda (_proc)
                  (unless reentrant-error
                    (setq reentrant-error
                          (should-error
                           (anvil-offload '(+ 1 2) :isolated t)
                           :type 'error)))))
               ((symbol-function 'delete-process) #'ignore)
               ((symbol-function 'anvil-offload--spawn-isolated-process)
                (lambda (_id)
                  (setq isolated-spawned t)
                  (error "isolated replacement spawned")))
               ((symbol-function 'anvil-offload--spawn-process)
                (lambda (_idx)
                  (setq spawned t)
                  (error "replacement spawned"))))
            (anvil-offload-stop-repl)
            (should (string-match-p
                     "cleanup already active"
                     (error-message-string reentrant-error)))
            (should-not isolated-spawned)
            (should (= 41 anvil-offload--next-id))
            (should anvil-offload--pool-retiring-p)
            (should-not anvil-offload--pool-cleanup-active-p)
            (should (process-get pooled 'anvil-offload-retiring))
            (should (process-live-p pooled))
            (should (eq pooled (aref anvil-offload--pool 0)))
            (should (process-live-p isolated))
            (should (gethash isolated anvil-offload--isolated-processes))
            (should-error (anvil-offload--pick-worker) :type 'error))
          (should-not spawned)
          (should (eq pooled (aref anvil-offload--pool 0))))
      (anvil-eval-async-isolation-test--cleanup-owned-processes
       pooled isolated))))

(ert-deftest anvil-eval-async-isolation-rejects-cross-process-frames ()
  "Started, checkpoint, and terminal frames cannot settle a peer process."
  (let* ((proc-a
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-frame-owner-a"))
         (proc-b
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-frame-owner-b"))
         (future-b
          (make-anvil-future :id 702 :process proc-b :status 'pending)))
    (unwind-protect
        (progn
          (puthash 702 future-b (anvil-offload--ensure-pending))
          (anvil-offload--filter
           proc-a
           (concat
            (anvil-eval-async-isolation-test--frame
             '(:id 702 :started t))
            (anvil-eval-async-isolation-test--frame
             '(:id 702 :checkpoint (:value forged :cursor peer)))
            (anvil-eval-async-isolation-test--frame
             '(:id 702 :ok forged))))
          (should (eq 'pending (anvil-future-status future-b)))
          (should-not (anvil-future--started-at future-b))
          (should-not (anvil-future-checkpoint future-b))
          (anvil-offload--filter
           proc-b
           (anvil-eval-async-isolation-test--frame
            '(:id 702 :ok owned)))
          (should (eq 'done (anvil-future-status future-b)))
          (should (eq 'owned (anvil-future-value future-b))))
      (remhash 702 (anvil-offload--ensure-pending))
      (when (process-live-p proc-a) (delete-process proc-a))
      (when (process-live-p proc-b) (delete-process proc-b)))))

(ert-deftest
    anvil-eval-async-isolation-frame-overflow-survives-nonlocal-callback ()
  "A frame failure callback cannot strand the offending subprocess."
  (let* ((proc
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-frame-overflow"))
         (future
          (make-anvil-future
           :id 704
           :process proc
           :status 'pending
           :isolated-p t
           :on-settle
           (lambda (_future)
             (throw 'anvil-async-frame-nonlocal 'escaped))))
         (anvil-offload-max-frame-bytes 1)
         (anvil-offload--pool nil)
         (anvil-offload--pool-retiring-p nil)
         (anvil-offload--transaction-state (vector nil nil))
         (anvil-offload--pool-cleanup-active-p nil)
         (anvil-offload--submission-active-p nil)
         (anvil-offload--stop-retiring-p nil)
         (anvil-offload--retired-pools nil)
         (anvil-offload--ownership-table-registry nil)
         (anvil-offload--fallback-processes (make-hash-table :test 'eq))
         (anvil-offload--isolated-processes (make-hash-table :test 'eq))
         (anvil-offload--pending (make-hash-table :test 'eql)))
    (unwind-protect
        (progn
          (puthash 704 future (anvil-offload--ensure-pending))
          (should
           (eq 'escaped
               (catch 'anvil-async-frame-nonlocal
                 (anvil-offload--filter proc "xx")
                 'did-not-escape)))
          (should (eq 'error (anvil-future-status future)))
          (should (string-match-p "frame exceeded"
                                  (anvil-future-error future)))
          (should-not (gethash 704 (anvil-offload--ensure-pending)))
          (should-not (process-live-p proc)))
      (remhash 704 (anvil-offload--ensure-pending))
      (anvil-eval-async-isolation-test--cleanup-owned-processes proc))))

(ert-deftest
    anvil-eval-async-isolation-dead-process-settles-all-after-nonlocal ()
  "One callback cannot leave sibling futures pending on a dead process."
  (let* ((proc
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-dead-process-futures"))
         (table (anvil-offload--ensure-pending))
         futures
         ids)
    (unwind-protect
        (progn
          (delete-process proc)
          (dotimes (i 5)
            (let* ((id (+ 710 i))
                   (future
                    (make-anvil-future
                     :id id
                     :process proc
                     :status 'pending
                     :on-settle
                     (lambda (_future)
                       (throw 'anvil-async-dead-nonlocal 'escaped)))))
              (push id ids)
              (push future futures)
              (puthash id future table)))
          (should
           (eq 'escaped
               (catch 'anvil-async-dead-nonlocal
                 (anvil-offload--finalize-dead-process proc "dead child")
                 'did-not-escape)))
          (dolist (future futures)
            (should (eq 'error (anvil-future-status future)))
            (should (equal "dead child" (anvil-future-error future)))
            (should-not (gethash (anvil-future--id future) table))))
      (dolist (id ids)
        (remhash id table))
      (when (process-live-p proc)
        (anvil-offload--hard-delete-process proc)))))

(ert-deftest anvil-eval-async-isolation-coalesced-frames-use-per-frame-cap ()
  "Several valid frames in one filter chunk are not one oversized frame."
  (let* ((proc
          (anvil-eval-async-isolation-test--idle-process
           "anvil-async-coalesced-frames"))
         (future
          (make-anvil-future :id 703 :process proc :status 'pending))
         (started
          (anvil-eval-async-isolation-test--frame
           '(:id 703 :started t)))
         (terminal
          (anvil-eval-async-isolation-test--frame
           (list :id 703 :ok (make-string 20 ?x))))
         (cap (max (string-bytes started) (string-bytes terminal)))
         (anvil-offload-max-frame-bytes cap))
    (unwind-protect
        (progn
          (should (> (+ (string-bytes started) (string-bytes terminal))
                     cap))
          (puthash 703 future (anvil-offload--ensure-pending))
          (anvil-offload--filter proc (concat started terminal))
          (should (eq 'done (anvil-future-status future)))
          (should (numberp (anvil-future--started-at future)))
          (should (equal (make-string 20 ?x)
                         (anvil-future-value future))))
      (remhash 703 (anvil-offload--ensure-pending))
      (when (process-live-p proc) (delete-process proc)))))

(ert-deftest anvil-eval-async-isolation-preserves-load-path-precedence ()
  "Duplicate libraries resolve according to the captured root load-path."
  (anvil-eval-async-isolation-test--with-clean-state
    (let* ((tmp (make-temp-file "anvil-async-load-path-" t))
           (first-dir (expand-file-name "first" tmp))
           (second-dir (expand-file-name "second" tmp))
           (suffix (format "%d" (random most-positive-fixnum)))
           (feature (intern (format "anvil-async-library-%s" suffix)))
           (origin (intern (format "anvil-async-origin-%s" suffix)))
           (file-name (concat (symbol-name feature) ".el")))
      (unwind-protect
          (progn
            (make-directory first-dir)
            (make-directory second-dir)
            (with-temp-file (expand-file-name file-name first-dir)
              (insert (format "(setq %s 'first)\n(provide '%s)\n"
                              origin feature)))
            (with-temp-file (expand-file-name file-name second-dir)
              (insert (format "(setq %s 'second)\n(provide '%s)\n"
                              origin feature)))
            (let* ((load-path
                    (append (list first-dir second-dir) load-path))
                   (expression
                    (prin1-to-string
                     `(progn (require ',feature) ,origin)))
                   (id
                    (anvil-eval-async-isolation-test--job-id
                     (anvil-eval--async expression 8)))
                   (job (anvil-eval-async-isolation-test--wait id)))
              (should (eq 'done (plist-get job :status)))
              (should (equal "first" (plist-get job :result)))))
        (delete-directory tmp t)))))

(ert-deftest anvil-eval-async-isolation-retains-from-finish-time ()
  "A long job receives the full result-retention window after completion."
  (anvil-eval-async-isolation-test--with-clean-state
    (let* ((anvil-eval-async-retention-seconds 0.1)
           (id
            (anvil-eval-async-isolation-test--job-id
             (anvil-eval--async "(progn (sleep-for 0.2) 7)" 8)))
           (job (anvil-eval-async-isolation-test--wait id)))
      (should (eq 'done (plist-get job :status)))
      (should (string-match-p "status: done" (anvil-eval--result id)))
      (should (gethash id anvil-eval--async-jobs))
      (should (string-match-p "result: 7" (anvil-eval--result id))))))

(ert-deftest anvil-eval-async-isolation-explicit-initializer-contract ()
  "Initializer files restore child features; live root definitions do not."
  (anvil-eval-async-isolation-test--with-clean-state
    (let* ((suffix (format "%d" (random most-positive-fixnum)))
           (root-only (intern (format "anvil-async-root-only-%s" suffix)))
           (initialized
            (intern (format "anvil-async-initialized-%s" suffix)))
           (init-file (make-temp-file "anvil-async-init-" nil ".el")))
      (unwind-protect
          (progn
            (fset root-only (lambda () 'root-only))
            (with-temp-file init-file
              (insert (format "(defun %s () 'initializer-ok)\n"
                              initialized)))
            (let* ((anvil-offload-init-files (list init-file))
                   (expression
                    (prin1-to-string
                     `(list (fboundp ',root-only) (,initialized))))
                   (id
                    (anvil-eval-async-isolation-test--job-id
                     (anvil-eval--async expression 8)))
                   (job (anvil-eval-async-isolation-test--wait id)))
              (should (eq 'done (plist-get job :status)))
              (should (equal "(nil initializer-ok)"
                             (plist-get job :result)))))
        (when (fboundp root-only) (fmakunbound root-only))
        (when (file-exists-p init-file) (delete-file init-file))))))

(provide 'anvil-eval-async-isolation-test)
;;; anvil-eval-async-isolation-test.el ends here
