;;; anvil-fusion-longrun-async-test.el --- tests for the async quest runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Phase 3 tests.  The orchestrator is stubbed via cl-letf so the full
;; submit -> poll -> done lifecycle runs with no real model: submit
;; records each batch's task name, status reports terminal, and
;; extract-result returns canned step / distill output (distill output
;; carries the STATUS marker that drives termination).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'anvil-orchestrator)
(require 'anvil-fusion-ask)
(require 'anvil-fusion-longrun-async)

(defconst anvil-fusion-longrun-async-test--clrhash-primitive
  (symbol-function 'clrhash)
  "Unadvised hash-table cleanup primitive for fixture teardown.")

(defvar anvil-fusion-longrun-async-test--databases (cons nil nil)
  "Stable state whose cdr owns temporary database handles and paths.")

(defun anvil-fusion-longrun-async-test--cleanup-databases ()
  "Close and remove every temporary database owned by the current test."
  (let ((entries (cdr anvil-fusion-longrun-async-test--databases))
        (inhibit-quit t))
    (unwind-protect
        (dolist (entry entries)
          (when (car entry)
            (condition-case nil
                (sqlite-close (car entry))
              ((error quit) nil)))
          (dolist (suffix '("" "-wal" "-shm" "-journal"))
            (condition-case nil
                (delete-file (concat (cdr entry) suffix))
              ((error quit) nil))))
      (setcdr anvil-fusion-longrun-async-test--databases nil))))

(defmacro anvil-fusion-longrun-async-test--with-resources (&rest body)
  "Run BODY with isolated jobs and deterministic database cleanup."
  (declare (indent 0) (debug t))
  `(let ((anvil-fusion-longrun-async-test--databases (cons nil nil))
         (anvil-fusion-longrun--jobs (make-hash-table :test 'equal)))
     (unwind-protect
         (progn ,@body)
       (let ((inhibit-quit t))
         (unwind-protect
             (condition-case nil
                 (funcall anvil-fusion-longrun-async-test--clrhash-primitive
                          anvil-fusion-longrun--jobs)
               ((error quit) nil))
           (anvil-fusion-longrun-async-test--cleanup-databases))))))

(defun anvil-fusion-longrun-async-test--tmpdb ()
  "Stage, open, and publish a fresh temporary quest store."
  (let (path entry db)
    (let ((inhibit-quit t))
      (setq path (make-temp-file "alr-async-" nil ".db")
            entry (cons nil path))
      (setcdr anvil-fusion-longrun-async-test--databases
              (cons entry
                    (cdr anvil-fusion-longrun-async-test--databases)))
      (setq db (anvil-fusion-longrun-store-open path))
      (setcar entry db))
    db))

(defun anvil-fusion-longrun-async-test--drain (job-id &optional cap)
  "Poll JOB-ID until terminal (or CAP polls); return the last status."
  (let ((cap (or cap 60)) r)
    (while (and (> cap 0)
                (setq r (anvil-fusion-longrun-status job-id))
                (not (memq (plist-get r :status) '(done error))))
      (cl-decf cap))
    r))

(defmacro anvil-fusion-longrun-async-test--with-stub (status-verdict &rest body)
  "Run BODY with the orchestrator stubbed.
STATUS-VERDICT is evaluated for each `anvil-orchestrator-status'
call; `distill-status' controls the canned trailing STATUS word.
BODY may inspect `--submitted', `--asks', and `--tasks'."
  (declare (indent 1))
  `(let ((--counter 0)
         (--names (make-hash-table :test 'equal))
         (--tasks (make-hash-table :test 'equal))
         (--submitted nil)
         (--asks nil))
     (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                (lambda (tasks)
                  (cl-incf --counter)
                  (let ((bid (format "batch-%d" --counter)))
                    (puthash bid (plist-get (car tasks) :name) --names)
                    (puthash bid (car tasks) --tasks)
                    (push (car tasks) --submitted)
                    bid)))
               ((symbol-function 'anvil-orchestrator-status)
                (lambda (_id) ,status-verdict))
               ((symbol-function 'anvil-fusion--batch-first-task-id)
                (lambda (bid) bid))
               ((symbol-function 'anvil-fusion-ask)
                (lambda (prompt &rest args)
                  (push (list :prompt prompt :args args) --asks)
                  (list :answer "PANEL-OUT")))
               ((symbol-function 'anvil-orchestrator-extract-result)
                (lambda (bid &optional _full)
                  (let ((name (gethash bid --names)))
                    (if (string-prefix-p "lr-distill" name)
                        ;; distinct digest per step so convergence does not
                        ;; trigger accidentally in the generic stub path.
                        (let* ((k (string-to-number
                                   (or (car (last (split-string name "-"))) "0")))
                               (words '("alpha-apple" "bravo-river" "charlie-tower"
                                        "delta-cloud" "echo-stone" "foxtrot-maple"))
                               (w (nth (mod k (length words)) words)))
                          (list :summary
                                (format "DIGEST-%s\nSTATUS: %s" w distill-status)))
                      (list :summary (format "stepout-%s" name)))))))
       ,@body)))

(ert-deftest anvil-fusion-longrun-async-test-fixture-cleans-nonlocal-exit ()
  "The fixture must survive open failure, error, quit, and cleanup quit."
  (let (failed-path database path jobs
                    quit-database quit-path quit-jobs quit-seen)
    (cl-letf (((symbol-function 'anvil-fusion-longrun-store-open)
               (lambda (candidate)
                 (setq failed-path candidate)
                 (error "Injected async store open failure"))))
      (should-error
       (anvil-fusion-longrun-async-test--with-resources
        (anvil-fusion-longrun-async-test--tmpdb))))
    (dolist (suffix '("" "-wal" "-shm" "-journal"))
      (should-not (file-exists-p (concat failed-path suffix))))
    (should-error
     (anvil-fusion-longrun-async-test--with-resources
      (setq database (anvil-fusion-longrun-async-test--tmpdb)
            path
            (cdar (cdr anvil-fusion-longrun-async-test--databases))
            jobs anvil-fusion-longrun--jobs)
      (puthash "retained" (list :db database) jobs)
      (error "Fixture nonlocal exit")))
    (should (hash-table-p jobs))
    (should (= 0 (hash-table-count jobs)))
    (should-error (sqlite-select database "SELECT 1"))
    (dolist (suffix '("" "-wal" "-shm" "-journal"))
      (should-not (file-exists-p (concat path suffix))))
    (setq quit-seen
          (condition-case nil
              (progn
                (anvil-fusion-longrun-async-test--with-resources
                 (setq quit-database
                       (anvil-fusion-longrun-async-test--tmpdb)
                       quit-path
                       (cdar
                        (cdr anvil-fusion-longrun-async-test--databases))
                       quit-jobs anvil-fusion-longrun--jobs)
                 (puthash "quit" (list :db quit-database) quit-jobs)
                 (signal 'quit nil))
                nil)
            (quit t)))
    (should quit-seen)
    (should (= 0 (hash-table-count quit-jobs)))
    (should-error (sqlite-select quit-database "SELECT 1"))
    (dolist (suffix '("" "-wal" "-shm" "-journal"))
      (should-not (file-exists-p (concat quit-path suffix))))
    (let* ((anvil-fusion-longrun-async-test--databases (cons nil nil))
           (cleanup-path (make-temp-file "alr-async-cleanup-" nil ".db")))
      (setcdr anvil-fusion-longrun-async-test--databases
              (list (cons :fake cleanup-path)))
      (cl-letf (((symbol-function 'sqlite-close)
                 (lambda (_db) (signal 'quit nil))))
        (anvil-fusion-longrun-async-test--cleanup-databases))
      (should-not (cdr anvil-fusion-longrun-async-test--databases))
      (should-not (file-exists-p cleanup-path)))))

(ert-deftest anvil-fusion-longrun-async-test-lifecycle-budget ()
  (anvil-fusion-longrun-async-test--with-resources (let ((distill-status "CONTINUE")
        (db (anvil-fusion-longrun-async-test--tmpdb)))
    (anvil-fusion-longrun-async-test--with-stub
        (list :running 0 :queued 0)
      (let* ((s (anvil-fusion-longrun-start-async
                 "G" :provider 'ollama :max-steps 2 :db db))
             (job-id (plist-get s :job-id))
             (r (anvil-fusion-longrun-async-test--drain job-id)))
        (should (eq (plist-get s :stage) 'step-ready))
        (should (eq (plist-get r :status) 'done))
        (should (= (plist-get r :steps) 2))
        (should (= (plist-get r :panel-steps) 0))
        (should (eq (plist-get r :stopped) 'budget))
        (should (string-prefix-p "DIGEST-" (plist-get r :answer)))
        (let ((q (anvil-fusion-longrun-store-get db (plist-get s :quest-id))))
          (should (= (plist-get q :step) 2))
          (should (equal (plist-get q :status) "budget")))
        (should (= (length (anvil-fusion-longrun-store-steps
                            db (plist-get s :quest-id)))
                   2)))))))

(ert-deftest anvil-fusion-longrun-async-test-early-done ()
  (anvil-fusion-longrun-async-test--with-resources (let ((distill-status "DONE")
        (db (anvil-fusion-longrun-async-test--tmpdb)))
    (anvil-fusion-longrun-async-test--with-stub
        (list :running 0 :queued 0)
      (let* ((s (anvil-fusion-longrun-start-async
                 "G" :provider 'ollama :max-steps 9 :db db))
             (r (anvil-fusion-longrun-async-test--drain (plist-get s :job-id))))
        (should (eq (plist-get r :status) 'done))
        (should (= (plist-get r :steps) 1))
        (should (eq (plist-get r :stopped) 'done))
        (should (equal (plist-get (anvil-fusion-longrun-store-get
                                   db (plist-get s :quest-id))
                                  :status)
                       "done")))))))

(ert-deftest anvil-fusion-longrun-async-test-running-does-not-advance ()
  (anvil-fusion-longrun-async-test--with-resources (let ((distill-status "CONTINUE")
        (db (anvil-fusion-longrun-async-test--tmpdb))
        (polls 0))
    (anvil-fusion-longrun-async-test--with-stub
        (progn
          (cl-incf polls)
          (if (= polls 1) (list :running 1 :queued 0)
            (list :running 0 :queued 0)))
      (let* ((s (anvil-fusion-longrun-start-async
                 "G" :provider 'ollama :max-steps 1 :db db))
             (r1 (anvil-fusion-longrun-status (plist-get s :job-id))))
        (should (eq (plist-get r1 :status) 'running))
        (should (eq (plist-get r1 :stage) 'step-running))
        (let ((r (anvil-fusion-longrun-async-test--drain (plist-get s :job-id))))
          (should (eq (plist-get r :status) 'done))
          (should (= (plist-get r :steps) 1))))))))

(ert-deftest anvil-fusion-longrun-async-test-resume ()
  (anvil-fusion-longrun-async-test--with-resources (let ((distill-status "CONTINUE")
        (db (anvil-fusion-longrun-async-test--tmpdb)))
    (anvil-fusion-longrun-async-test--with-stub
        (list :running 0 :queued 0)
      (let* ((a (anvil-fusion-longrun-start-async
                 "G" :provider 'ollama :max-steps 2 :db db))
             (qid (plist-get a :quest-id)))
        (anvil-fusion-longrun-async-test--drain (plist-get a :job-id))
        (should (= (plist-get (anvil-fusion-longrun-store-get db qid) :step) 2))
        (let* ((b (anvil-fusion-longrun-resume-async qid :max-steps 4 :db db))
               (rb (anvil-fusion-longrun-async-test--drain (plist-get b :job-id))))
          (should (= (plist-get b :resumed-from) 2))
          (should (eq (plist-get b :stage) 'step-ready))
          (should (eq (plist-get rb :status) 'done))
          (should (= (plist-get rb :steps) 4))
          (should (= (length (anvil-fusion-longrun-store-steps db qid)) 4))))))))

(ert-deftest anvil-fusion-longrun-async-test-unknown-job ()
  (anvil-fusion-longrun-async-test--with-resources (should-error (anvil-fusion-longrun-status "nope"))))

(ert-deftest anvil-fusion-longrun-async-test-resume-unknown-quest ()
  (anvil-fusion-longrun-async-test--with-resources (let ((db (anvil-fusion-longrun-async-test--tmpdb)))
    (should-error (anvil-fusion-longrun-resume-async "nope" :db db)))))

(ert-deftest anvil-fusion-longrun-async-test-converges ()
  (anvil-fusion-longrun-async-test--with-resources (let ((db (anvil-fusion-longrun-async-test--tmpdb))
        (--counter 0)
        (--names (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (tasks)
                 (cl-incf --counter)
                 (let ((bid (format "batch-%d" --counter)))
                   (puthash bid (plist-get (car tasks) :name) --names)
                   bid)))
              ((symbol-function 'anvil-orchestrator-status)
               (lambda (_id) (list :running 0 :queued 0)))
              ((symbol-function 'anvil-fusion--batch-first-task-id)
               (lambda (bid) bid))
              ((symbol-function 'anvil-orchestrator-extract-result)
               (lambda (bid &optional _full)
                 (if (string-prefix-p "lr-distill" (gethash bid --names))
                     (list :summary "STABLE STATE\nSTATUS: CONTINUE")
                   (list :summary "stepout")))))
      (let* ((s (anvil-fusion-longrun-start-async
                 "G" :provider 'ollama :max-steps 9 :db db))
             (r (anvil-fusion-longrun-async-test--drain (plist-get s :job-id))))
        (should (eq (plist-get r :status) 'done))
        (should (eq (plist-get r :stopped) 'converged))
        (should (= (plist-get r :steps) 3)))))))

(ert-deftest anvil-fusion-longrun-async-test-hermetic-allowed-tools ()
  (anvil-fusion-longrun-async-test--with-resources (let ((db (anvil-fusion-longrun-async-test--tmpdb))
        captured)
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (tasks)
                 (push (car tasks) captured)
                 "batch-x"))
              ((symbol-function 'anvil-orchestrator-status)
               (lambda (_id) (list :running 0 :queued 0)))
              ((symbol-function 'anvil-fusion--batch-first-task-id)
               (lambda (bid) bid))
              ((symbol-function 'anvil-orchestrator-extract-result)
               (lambda (_bid &optional _full)
                 (list :summary "stepout"))))
      (let ((s (anvil-fusion-longrun-start-async
                "G" :provider 'claude :max-steps 3 :db db :hermetic t)))
        (should-not captured)
        (anvil-fusion-longrun-status (plist-get s :job-id)))
      (let ((task (car captured)))
        (should (string-match-p "mcp__emacs-eval-ultra__file-outline"
                                (plist-get task :allowed-tools)))
        (should (string-match-p "読み取り専用" (plist-get task :prompt))))))))

(ert-deftest anvil-fusion-longrun-async-test-panel-hard-only-routes-next-step-via-ask ()
  (anvil-fusion-longrun-async-test--with-resources (let ((distill-status "CONTINUE")
        (db (anvil-fusion-longrun-async-test--tmpdb)))
    (anvil-fusion-longrun-async-test--with-stub
        (list :running 0 :queued 0)
      (cl-letf (((symbol-function 'anvil-orchestrator-extract-result)
                 (lambda (bid &optional _full)
                   (let* ((task (gethash bid --tasks))
                          (name (plist-get task :name)))
                     (if (string-prefix-p "lr-distill" name)
                         (let ((step-n (string-to-number
                                        (or (car (last (split-string name "-"))) "0"))))
                           (list :summary
                                 (format "DIGEST-%d\n%s\nSTATUS: %s"
                                         step-n
                                         (if (= step-n 1)
                                             "NEXT-HARD: yes"
                                           "NEXT-HARD: no")
                                         distill-status)))
                       (list :summary (format "stepout-%s" name)))))))
        (let* ((s (anvil-fusion-longrun-start-async
                   "G" :provider 'ollama :max-steps 3 :db db
                   :step-panel 'opus-solo))
               (job-id (plist-get s :job-id))
               (r1 (anvil-fusion-longrun-status job-id))
               (r2 (anvil-fusion-longrun-status job-id))
               (r (anvil-fusion-longrun-async-test--drain job-id)))
          (should (eq (plist-get r1 :stage) 'step-running))
          (should (eq (plist-get r2 :stage) 'distill-running))
          (should (= (length --asks) 1))
          (should (eq (plist-get r :status) 'done))
          (should (= (plist-get r :panel-steps) 1))
          (let* ((submitted (reverse --submitted))
                 (ask-call (car --asks))
                 (distill-2 (seq-find (lambda (task)
                                        (equal (plist-get task :name) "lr-distill-2"))
                                      submitted)))
            (should (equal (mapcar (lambda (task) (plist-get task :name))
                                   submitted)
                           '("lr-step-1" "lr-distill-1" "lr-distill-2"
                             "lr-step-3" "lr-distill-3")))
            (should (string-match-p "DIGEST-1" (plist-get ask-call :prompt)))
            (should (string-match-p "PANEL-OUT" (plist-get distill-2 :prompt))))))))))

(ert-deftest anvil-fusion-longrun-async-test-panel-budget-cap-falls-back ()
  (anvil-fusion-longrun-async-test--with-resources (let ((distill-status "CONTINUE")
        (db (anvil-fusion-longrun-async-test--tmpdb))
        (anvil-fusion-longrun-max-panel-steps 1))
    (anvil-fusion-longrun-async-test--with-stub
        (list :running 0 :queued 0)
      (cl-letf (((symbol-function 'anvil-orchestrator-extract-result)
                 (lambda (bid &optional _full)
                   (let ((name (gethash bid --names)))
                     (if (string-prefix-p "lr-distill" name)
                         (let ((step-n (string-to-number
                                        (or (car (last (split-string name "-"))) "0"))))
                           (list :summary
                                 (format "DIGEST-%d\n%s\nSTATUS: %s"
                                         step-n
                                         (if (< step-n 3)
                                             "NEXT-HARD: yes"
                                           "NEXT-HARD: no")
                                         distill-status)))
                       (list :summary (format "stepout-%s" name)))))))
        (let* ((s (anvil-fusion-longrun-start-async
                   "G" :provider 'ollama :max-steps 3 :db db
                   :step-panel 'opus-solo))
               (r (anvil-fusion-longrun-async-test--drain (plist-get s :job-id))))
          (should (eq (plist-get r :status) 'done))
          (should (= (plist-get r :panel-steps) 1))
          (should (= (length --asks) 1))
          (should (member "lr-step-3"
                          (mapcar (lambda (task) (plist-get task :name))
                                  --submitted)))))))))

(ert-deftest anvil-fusion-longrun-async-test-verify-retry-then-pass ()
  (anvil-fusion-longrun-async-test--with-resources (let ((db (anvil-fusion-longrun-async-test--tmpdb))
        (--counter 0)
        (--order nil)
        (--tasks (make-hash-table :test 'equal))
        (distill-counts (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (tasks)
                 (cl-incf --counter)
                 (let ((bid (format "batch-%d" --counter))
                       (task (car tasks)))
                   (puthash bid task --tasks)
                   (push bid --order)
                   bid)))
              ((symbol-function 'anvil-orchestrator-status)
               (lambda (_id) (list :running 0 :queued 0)))
              ((symbol-function 'anvil-fusion--batch-first-task-id)
               (lambda (bid) bid))
              ((symbol-function 'anvil-orchestrator-extract-result)
               (lambda (bid &optional _full)
                 (let* ((task (gethash bid --tasks))
                        (name (plist-get task :name)))
                   (if (string-prefix-p "lr-distill" name)
                       (let ((seen (1+ (gethash name distill-counts 0))))
                         (puthash name seen distill-counts)
                         (list :summary
                               (pcase (list name seen)
                                 (`("lr-distill-1" 1) "DIGEST-FIRST\nSTATUS: CONTINUE")
                                 (`("lr-distill-1" 2) "DIGEST-RETRY\nSTATUS: CONTINUE")
                                 (_ "DIGEST-DONE\nSTATUS: DONE"))))
                     (list :summary (format "stepout-%s" name)))))))
      (let* ((s (anvil-fusion-longrun-start-async
                 "G" :provider 'ollama :max-steps 2 :db db
                 :verify-steps t
                 :verify-fn (lambda (_question digest)
                              (if (equal digest "DIGEST-FIRST")
                                  (list (list :claim "wrong fact"
                                              :evidence "proof"
                                              :verdict 'refuted))
                                nil))))
             (r (anvil-fusion-longrun-async-test--drain (plist-get s :job-id))))
        (should (eq (plist-get r :status) 'done))
        (should (= (plist-get r :steps) 2))
        (should (= (plist-get r :gate-failures) 0))
        (should (equal (plist-get r :answer) "DIGEST-DONE"))
        (let* ((order (nreverse --order))
               (submitted
                (mapcar (lambda (bid) (plist-get (gethash bid --tasks) :name))
                        order))
               (step1-prompts
                (mapcar (lambda (bid) (plist-get (gethash bid --tasks) :prompt))
                        (seq-filter
                         (lambda (bid)
                           (equal (plist-get (gethash bid --tasks) :name) "lr-step-1"))
                         order))))
          (should (= (length step1-prompts) 2))
          (should (equal submitted
                         '("lr-step-1" "lr-distill-1" "lr-step-1"
                           "lr-distill-1" "lr-step-2" "lr-distill-2")))
          (should (string-match-p "前回試行への反証" (cadr step1-prompts)))
          (should (string-match-p "wrong fact" (cadr step1-prompts)))))))))

(ert-deftest anvil-fusion-longrun-async-test-verify-retry-then-fail ()
  (anvil-fusion-longrun-async-test--with-resources (let ((db (anvil-fusion-longrun-async-test--tmpdb))
        (--counter 0)
        (--order nil)
        (--tasks (make-hash-table :test 'equal))
        (distill-counts (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'anvil-orchestrator-submit)
               (lambda (tasks)
                 (cl-incf --counter)
                 (let ((bid (format "batch-%d" --counter))
                       (task (car tasks)))
                   (puthash bid task --tasks)
                   (push bid --order)
                   bid)))
              ((symbol-function 'anvil-orchestrator-status)
               (lambda (_id) (list :running 0 :queued 0)))
              ((symbol-function 'anvil-fusion--batch-first-task-id)
               (lambda (bid) bid))
              ((symbol-function 'anvil-orchestrator-extract-result)
               (lambda (bid &optional _full)
                 (let* ((task (gethash bid --tasks))
                        (name (plist-get task :name)))
                   (if (string-prefix-p "lr-distill" name)
                       (let ((seen (1+ (gethash name distill-counts 0))))
                         (puthash name seen distill-counts)
                         (list :summary
                               (pcase (list name seen)
                                 (`("lr-distill-1" 1) "DIGEST-FIRST\nSTATUS: CONTINUE")
                                 (`("lr-distill-1" 2) "DIGEST-STILL-BAD\nSTATUS: CONTINUE")
                                 (_ "DIGEST-DONE\nSTATUS: DONE"))))
                     (list :summary (format "stepout-%s" name)))))))
      (let* ((s (anvil-fusion-longrun-start-async
                 "G" :provider 'ollama :max-steps 2 :db db
                 :verify-steps t
                 :verify-fn (lambda (_question digest)
                              (if (member digest '("DIGEST-FIRST" "DIGEST-STILL-BAD"))
                                  (list (list :claim "still wrong"
                                              :evidence "source"
                                              :verdict 'refuted))
                                nil))))
             (r (anvil-fusion-longrun-async-test--drain (plist-get s :job-id))))
        (should (eq (plist-get r :status) 'done))
        (should (= (plist-get r :steps) 2))
        (should (= (plist-get r :gate-failures) 1))
        (should (equal (plist-get r :answer) "DIGEST-DONE"))
        (should (equal (mapcar (lambda (bid) (plist-get (gethash bid --tasks) :name))
                               (nreverse --order))
                       '("lr-step-1" "lr-distill-1" "lr-step-1"
                         "lr-distill-1" "lr-step-2" "lr-distill-2")))
        (should (= (length (anvil-fusion-longrun-store-steps
                            db (plist-get s :quest-id)))
                   2)))))))

(provide 'anvil-fusion-longrun-async-test)
;;; anvil-fusion-longrun-async-test.el ends here
