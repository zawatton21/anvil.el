;;; anvil-offload-ownership-test.el --- Transactional custody regressions -*- lexical-binding: t; -*-

;;; Commentary:

;; Adversarial regressions for issue #53.  Every fixture dynamically binds all
;; ownership generations and proves cleanup convergence before scope exit.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'anvil-offload)

(defconst anvil-offload-ownership-test--prefix "anvil-ownership-")

(defun anvil-offload-ownership-test--idle-process (name &optional buffer)
  "Return a quiet process named NAME, optionally using BUFFER."
  (make-process
   :name name
   :buffer buffer
   :command (list shell-file-name shell-command-switch "sleep 30")
   :connection-type 'pipe
   :noquery t
   :sentinel #'anvil-offload--sentinel))

(defun anvil-offload-ownership-test--compatible (proc)
  "Tag PROC with the current offload protocol and return it."
  (process-put proc 'anvil-offload-protocol-version
               anvil-offload--protocol-version)
  proc)

(defun anvil-offload-ownership-test--owned-p (proc)
  "Return non-nil when any registered table owns PROC."
  (cl-some
   (lambda (table) (gethash proc table))
   (anvil-offload--registered-ownership-tables)))

(defun anvil-offload-ownership-test--assert-clean ()
  "Assert that every bound ownership channel has converged."
  (let ((deadline (+ (float-time) 1.0)))
    (while (and (hash-table-p anvil-offload--pending)
                (> (hash-table-count anvil-offload--pending) 0)
                (< (float-time) deadline))
      (accept-process-output nil 0.01)))
  (let (owned pending)
    (dolist (table (anvil-offload--registered-ownership-tables))
      (maphash
       (lambda (proc value) (push (list table proc value) owned))
       table))
    (when (hash-table-p anvil-offload--pending)
      (maphash (lambda (id _future) (push id pending))
               anvil-offload--pending))
    (should-not anvil-offload--pool)
    (should-not anvil-offload--pool-retiring-p)
    (should-not (anvil-offload--cleanup-active-p))
    (should-not (anvil-offload--submission-active-p))
    (should-not anvil-offload--pool-cleanup-active-p)
    (should-not anvil-offload--submission-active-p)
    (should-not anvil-offload--stop-retiring-p)
    (should-not anvil-offload--retired-pools)
    (should-not owned)
    (should-not pending)))

(defun anvil-offload-ownership-test--cleanup ()
  "Delete test children and prove bound ownership convergence."
  (dolist (proc (process-list))
    (when (string-prefix-p anvil-offload-ownership-test--prefix
                           (process-name proc))
      (set-process-sentinel proc #'anvil-offload--sentinel)
      (anvil-offload--hard-delete-process proc)))
  (anvil-offload-stop-repl)
  (anvil-offload-ownership-test--assert-clean))

(defmacro anvil-offload-ownership-test--with-state (&rest body)
  "Run BODY with dynamically isolated offload ownership state."
  (declare (indent 0))
  `(let ((anvil-offload--pool nil)
         (anvil-offload--pool-retiring-p nil)
         (anvil-offload--pool-cleanup-active-p nil)
         (anvil-offload--submission-active-p nil)
         (anvil-offload--transaction-state (vector nil nil))
         (anvil-offload--stop-retiring-p nil)
         (anvil-offload--retired-pools nil)
         (anvil-offload--ownership-table-registry nil)
         (anvil-offload--fallback-processes (make-hash-table :test 'eq))
         (anvil-offload--isolated-processes (make-hash-table :test 'eq))
         (anvil-offload--pending (make-hash-table :test 'eql))
         (anvil-offload--round-robin 0)
         (anvil-offload--next-id 0)
         (anvil-offload-pool-size 1)
         (anvil-offload-spawn-environment-function nil))
     (unwind-protect
         (progn ,@body)
       (anvil-offload-ownership-test--cleanup))))

(defun anvil-offload-ownership-test--frame (message)
  "Encode MESSAGE as one complete offload frame."
  (concat anvil-offload--frame-prefix
          (anvil-offload--frame-encode-payload
           (prin1-to-string message))
          "\n"))

(defun anvil-offload-ownership-test--future (id proc &rest keys)
  "Return a production-shaped registered future for ID owned by PROC."
  (apply
   #'make-anvil-future
   :id id
   :process proc
   :status 'pending
   :registration-p t
   :registered-id id
   :registered-process proc
   :registered-status 'pending
   :registered-isolated-p (and (plist-get keys :isolated-p) t)
   keys))

(ert-deftest anvil-offload-ownership-junk-prefix-strip-is-linear ()
  "Two-megabyte prompt chatter allocates only one final substring."
  (let* ((prefix (car anvil-offload--ignored-junk-prefixes))
         (minimum-bytes (* 2 1024 1024))
         (count (ceiling (/ minimum-bytes (float (string-bytes prefix)))))
         (payload "ANVIL-OFFLOAD payload")
         (input
          (with-temp-buffer
            (dotimes (_ count)
              (insert prefix))
            (insert payload)
            (buffer-string)))
         (real-substring anvil-offload--substring-primitive)
         (substring-calls 0)
         (materialized-bytes 0)
         result)
    (let ((anvil-offload--substring-primitive
           (lambda (string &rest args)
             (cl-incf substring-calls)
             (cl-incf materialized-bytes (string-bytes string))
             (apply real-substring string args))))
      (setq result (anvil-offload--strip-ignored-junk-prefixes input)))
    (should (>= (string-bytes input) minimum-bytes))
    (should (equal payload result))
    (should (= 1 substring-calls))
    (should (= (string-bytes input) materialized-bytes))))

(ert-deftest
    anvil-offload-ownership-isolated-fallback-does-not-retire-healthy-pool ()
  "Failed isolated cleanup cannot sweep a healthy pooled worker."
  (anvil-offload-ownership-test--with-state
    (let* ((healthy
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-healthy-pool")))
           (isolated
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-stubborn-isolated"))
           (pool (vector healthy))
           (anvil-offload--pool pool)
           (future
            (anvil-offload-ownership-test--future 901 healthy))
           killed)
      (puthash 901 future anvil-offload--pending)
      (anvil-offload--track-process-ownership
       isolated t anvil-offload--isolated-processes)
      (cl-letf (((symbol-function 'kill-process)
                 (lambda (proc) (push proc killed)))
                ((symbol-function 'delete-process) #'ignore))
        (should-not (anvil-offload--hard-delete-process isolated))
        (should (eq :submission-active
                    (gethash isolated anvil-offload--fallback-processes)))
        (setq killed nil)
        (should (eq pool (anvil-offload--ensure-pool-vector)))
        (should-not killed)
        (should (eq healthy (aref pool 0)))
        (should (process-live-p healthy))
        (should (process-live-p isolated))
        (should (eq 'pending (anvil-future-status future)))
        (should (eq future (gethash 901 anvil-offload--pending)))))))

(ert-deftest anvil-offload-ownership-retired-retry-sweeps-all-generations ()
  "A later resize cannot publish while an older retired child survives."
  (anvil-offload-ownership-test--with-state
    (let* ((stubborn
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-retired-stubborn"))
           (old (vector stubborn))
           (replacement (vector nil))
           (anvil-offload--pool old)
           (anvil-offload-pool-size 2)
           swapped)
      (cl-letf (((symbol-function 'kill-process)
                 (lambda (_proc)
                   (unless swapped
                     (setq swapped t
                           anvil-offload--pool replacement))))
                ((symbol-function 'delete-process) #'ignore))
        (should-error (anvil-offload--ensure-pool-vector))
        (should (eq replacement anvil-offload--pool))
        (should (memq old anvil-offload--retired-pools))
        (should anvil-offload--pool-retiring-p)
        (should-error (anvil-offload--ensure-pool-vector))
        (should (eq replacement anvil-offload--pool))
        (should (memq old anvil-offload--retired-pools))
        (should (process-live-p stubborn)))
      (let ((fresh (anvil-offload--ensure-pool-vector)))
        (should (= 2 (length fresh)))
        (should-not (eq fresh replacement))
        (should-not anvil-offload--retired-pools)
        (should-not anvil-offload--pool-retiring-p)
        (should-not (process-live-p stubborn))))))

(ert-deftest anvil-offload-ownership-dead-slot-delete-must-converge ()
  "A dead registered child blocks resize until deletion removes its object."
  (skip-unless (memq system-type '(darwin gnu/linux)))
  (anvil-offload-ownership-test--with-state
    (let* ((delete-exited-processes nil)
           (proc
            (make-process
             :name "anvil-ownership-dead-resize"
             :buffer nil
             :command (list shell-file-name shell-command-switch "exit 0")
             :connection-type 'pipe
             :noquery t
             :sentinel #'ignore))
           (old (vector proc))
           (anvil-offload--pool old)
           (anvil-offload-pool-size 2)
           (deadline (+ (float-time) 3))
           (spawned 0))
      (while (and (process-live-p proc) (< (float-time) deadline))
        (accept-process-output proc 0.02))
      (should-not (process-live-p proc))
      (should
       (memq proc (funcall anvil-offload--process-list-primitive)))
      (cl-letf (((symbol-function 'kill-process) #'ignore)
                ((symbol-function 'delete-process) #'ignore)
                ((symbol-function 'anvil-offload--spawn-process)
                 (lambda (_slot)
                   (cl-incf spawned)
                   (error "dead resize unexpectedly spawned"))))
        (should-error (anvil-offload--ensure-pool-vector))
        (should (= 0 spawned))
        (should (eq old anvil-offload--pool))
        (should (eq proc (aref old 0)))
        (should (memq old anvil-offload--retired-pools))
        (should anvil-offload--pool-retiring-p)
        (should (process-get proc 'anvil-offload-retiring))
        (should (anvil-offload-ownership-test--owned-p proc)))
      (let ((fresh (anvil-offload--ensure-pool-vector)))
        (should (= 2 (length fresh)))
        (should-not (eq fresh old))
        (should-not (aref old 0))
        (should-not anvil-offload--retired-pools)
        (should-not anvil-offload--pool-retiring-p)
        (should-not
         (memq proc (funcall anvil-offload--process-list-primitive)))
        (should-not (anvil-offload-ownership-test--owned-p proc))))))

(ert-deftest anvil-offload-ownership-retiring-flag-watcher-keeps-child ()
  "A retiring-flag watcher cannot orphan the captured primary child."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-retiring-flag")))
           (old (vector proc))
           (replacement (vector nil))
           (anvil-offload--pool old)
           (anvil-offload-pool-size 2)
           fired watcher)
      (setq watcher
            (lambda (_symbol _new operation _where)
              (when (and (eq operation 'set) (not fired))
                (setq fired t
                      anvil-offload--pool replacement)
                (error "retiring flag watcher"))))
      (add-variable-watcher 'anvil-offload--pool-retiring-p watcher)
      (unwind-protect
          (should-error (anvil-offload--ensure-pool-vector))
        (remove-variable-watcher
         'anvil-offload--pool-retiring-p watcher))
      (should fired)
      (should (eq replacement anvil-offload--pool))
      (should (eq proc (aref old 0)))
      (should (process-get proc 'anvil-offload-retiring))
      (should (gethash proc anvil-offload--fallback-processes))
      (should (anvil-offload-ownership-test--owned-p proc))
      (anvil-offload-stop-repl)
      (should-not (process-live-p proc))
      (should-not (anvil-offload-ownership-test--owned-p proc)))))

(ert-deftest anvil-offload-ownership-retired-list-watcher-keeps-child ()
  "A retired-list watcher cannot orphan a child after swapping the primary."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-retired-list")))
           (old (vector proc))
           (replacement (vector nil))
           (anvil-offload--pool old)
           (anvil-offload-pool-size 2)
           fired watcher)
      (setq watcher
            (lambda (_symbol _new operation _where)
              (when (and (eq operation 'set) (not fired))
                (setq fired t
                      anvil-offload--pool replacement)
                (error "retired list watcher"))))
      (add-variable-watcher 'anvil-offload--retired-pools watcher)
      (unwind-protect
          (should-error (anvil-offload--ensure-pool-vector))
        (remove-variable-watcher
         'anvil-offload--retired-pools watcher))
      (should fired)
      (should (eq replacement anvil-offload--pool))
      (should (eq proc (aref old 0)))
      (should anvil-offload--pool-retiring-p)
      (should (gethash proc anvil-offload--fallback-processes))
      (should (anvil-offload-ownership-test--owned-p proc))
      (anvil-offload-stop-repl)
      (should-not (process-live-p proc))
      (should-not (anvil-offload-ownership-test--owned-p proc)))))

(ert-deftest anvil-offload-ownership-pool-watcher-signal-keeps-old-custody ()
  "A signaling pool watcher sees the guard and prevents publication."
  (anvil-offload-ownership-test--with-state
    (let* ((old (vector nil))
           (anvil-offload--pool old)
           (anvil-offload-pool-size 2)
           (anvil-offload--next-id 17)
           (count 0)
           active reentrant)
      (let ((watcher
             (lambda (_symbol _new operation _where)
               (when (eq operation 'set)
                 (cl-incf count)
                 (setq active anvil-offload--pool-cleanup-active-p)
                 (condition-case err
                     (anvil-offload '(+ 1 2) :isolated t)
                   (error (setq reentrant err)))
                 (error "ownership watcher stopped publication")))))
        (add-variable-watcher 'anvil-offload--pool watcher)
        (unwind-protect
            (should-error (anvil-offload--ensure-pool-vector))
          (remove-variable-watcher 'anvil-offload--pool watcher)))
      (should (= 1 count))
      (should active)
      (should (string-match-p "already active"
                              (error-message-string reentrant)))
      (should (= 17 anvil-offload--next-id))
      (should (eq old anvil-offload--pool))
      (should anvil-offload--pool-retiring-p)
      (should-not anvil-offload--retired-pools)
      (should (= 2 (length (anvil-offload--ensure-pool-vector)))))))

(ert-deftest anvil-offload-ownership-slot-swap-retains-stubborn-exact-child ()
  "A slot delete callback cannot orphan its exact surviving child."
  (anvil-offload-ownership-test--with-state
    (let* ((cur
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-slot-stubborn"))
           (peer
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-slot-peer")))
           (old (vector cur peer))
           (replacement (vector peer nil))
           (anvil-offload--pool old)
           (anvil-offload-pool-size 2)
           swapped)
      (process-put cur 'anvil-offload-protocol-version
                   (1- anvil-offload--protocol-version))
      (cl-letf (((symbol-function 'kill-process)
                 (lambda (_proc)
                   (unless swapped
                     (setq swapped t
                           anvil-offload--pool replacement))))
                ((symbol-function 'delete-process) #'ignore))
        (should-error (anvil-offload--ensure-slot 0)))
      (should (eq replacement anvil-offload--pool))
      (should (process-live-p cur))
      (should (process-live-p peer))
      (should (anvil-offload-ownership-test--owned-p cur))
      (anvil-offload-stop-repl)
      (should-not (process-live-p cur))
      (should-not (process-live-p peer))
      (should-not (anvil-offload-ownership-test--owned-p cur)))))

(ert-deftest anvil-offload-ownership-isolated-only-stop-remains-fail-closed ()
  "An auxiliary-only survivor keeps stop-retiring until a later retry."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-isolated-stop"))
           (other (make-hash-table :test 'eq))
           (anvil-offload--ownership-table-registry
            (list other anvil-offload--isolated-processes))
           (anvil-offload--next-id 23)
           spawned)
      (puthash proc t other)
      (cl-letf (((symbol-function 'kill-process) #'ignore)
                ((symbol-function 'delete-process) #'ignore)
                ((symbol-function 'anvil-offload--spawn-isolated-process)
                 (lambda (_id) (setq spawned t) proc)))
        (anvil-offload-stop-repl)
        (should anvil-offload--stop-retiring-p)
        (should anvil-offload--pool-retiring-p)
        (should (gethash proc other))
        (should-error (anvil-offload '(+ 1 2) :isolated t))
        (should-error (anvil-offload--ensure-pool-vector))
        (should (= 23 anvil-offload--next-id))
        (should-not spawned))
      (anvil-offload-stop-repl)
      (should-not anvil-offload--stop-retiring-p)
      (should-not anvil-offload--pool-retiring-p)
      (should-not (gethash proc other))
      (should-not (process-live-p proc)))))

(ert-deftest anvil-offload-ownership-stop-signal-retains-auxiliary-snapshot ()
  "A signaling first-stop watcher cannot hide an auxiliary child from retry."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-stop-aux-snapshot"))
           (old-table anvil-offload--isolated-processes)
           (anvil-offload--next-id 83)
           fired watcher stop-error rejection spawned)
      (anvil-offload--track-process-ownership proc t old-table)
      (setq watcher
            (lambda (_symbol _new operation _where)
              (when (and (eq operation 'set) (not fired))
                (setq fired t
                      anvil-offload--ownership-table-registry nil
                      anvil-offload--isolated-processes
                      (make-hash-table :test 'eq))
                (error "stop auxiliary registry watcher"))))
      (add-variable-watcher 'anvil-offload--stop-retiring-p watcher)
      (unwind-protect
          (setq stop-error (should-error (anvil-offload-stop-repl)))
        (remove-variable-watcher
         'anvil-offload--stop-retiring-p watcher))
      (should fired)
      (should
       (string-match-p
        "stop auxiliary registry watcher"
        (error-message-string stop-error)))
      (should (process-live-p proc))
      (should (gethash proc old-table))
      (should (gethash proc anvil-offload--fallback-processes))
      (should (process-get proc 'anvil-offload-stop-retiring))
      (should (anvil-offload--fallback-stop-retirement-p))
      (should-not anvil-offload--stop-retiring-p)
      (cl-letf
          (((symbol-function 'anvil-offload--spawn-isolated-process)
            (lambda (_id)
              (setq spawned t)
              (error "unexpected isolated spawn"))))
        (setq rejection
              (should-error
               (anvil-offload '(+ 1 2) :isolated t))))
      (should-not spawned)
      (should (= 83 anvil-offload--next-id))
      (should
       (string-match-p
        "cleanup already active"
        (error-message-string rejection)))
      (anvil-offload-stop-repl)
      (should-not (process-live-p proc))
      (should-not (memq proc (process-list)))
      (should-not (gethash proc old-table))
      (should-not (gethash proc anvil-offload--fallback-processes))
      (should-not (process-get proc 'anvil-offload-stop-retiring))
      (should-not (anvil-offload--fallback-stop-retirement-p))
      (should-not (anvil-offload-ownership-test--owned-p proc))
      (should-not anvil-offload--stop-retiring-p)
      (should-not anvil-offload--pool-retiring-p))))

(ert-deftest anvil-offload-ownership-stop-captures-callback-installed-auxiliary ()
  "Whole-stop cleanup captures auxiliary custody installed by pool callbacks."
  (anvil-offload-ownership-test--with-state
    (let* ((pool-proc
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-stop-callback-pool")))
           (late-proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-stop-callback-late"))
           (late-table (make-hash-table :test 'eq))
           (anvil-offload--pool (vector pool-proc))
           (real-kill (symbol-function 'kill-process))
           fired)
      (cl-letf
          (((symbol-function 'kill-process)
            (lambda (proc)
              (when (and (eq proc pool-proc) (not fired))
                (setq fired t)
                (puthash late-proc t late-table)
                (push late-table
                      anvil-offload--ownership-table-registry))
              (funcall real-kill proc))))
        (anvil-offload-stop-repl))
      (should fired)
      (should-not (process-live-p pool-proc))
      (should-not (process-live-p late-proc))
      (should-not (memq pool-proc (process-list)))
      (should-not (memq late-proc (process-list)))
      (should (zerop (hash-table-count late-table)))
      (should-not (anvil-offload-ownership-test--owned-p pool-proc))
      (should-not (anvil-offload-ownership-test--owned-p late-proc))
      (should (zerop
               (hash-table-count anvil-offload--fallback-processes)))
      (should-not anvil-offload--pool)
      (should-not anvil-offload--stop-retiring-p)
      (should-not anvil-offload--pool-retiring-p))))

(ert-deftest anvil-offload-ownership-submission-guard-blocks-callback-stop ()
  "Environment callbacks cannot stop or recursively submit mid-publication."
  (anvil-offload-ownership-test--with-state
    (let (stop-error reentrant-error observed)
      (setq anvil-offload-spawn-environment-function
            (lambda ()
              (setq observed anvil-offload--submission-active-p)
              (condition-case err
                  (anvil-offload-stop-repl)
                (error (setq stop-error err)))
              (condition-case err
                  (anvil-offload '(+ 4 5) :isolated t)
                (error (setq reentrant-error err)))
              (copy-sequence process-environment)))
      (cl-letf (((symbol-function 'anvil-offload--process-command)
                 (lambda ()
                   (list shell-file-name shell-command-switch "sleep 30"))))
        (let ((future (anvil-offload '(+ 1 2) :isolated t)))
          (should observed)
          (should (string-match-p "submission already active"
                                  (error-message-string stop-error)))
          (should (string-match-p "submission already active"
                                  (error-message-string reentrant-error)))
          (should (= 1 anvil-offload--next-id))
          (anvil-future-kill future 'test-cleanup))))))

(ert-deftest anvil-offload-ownership-cleanup-cell-precedes-mirror-watcher ()
  "Cleanup reentry is rejected before the observable mirror is bound."
  (anvil-offload-ownership-test--with-state
    (let* ((old (vector nil))
           (anvil-offload--pool old)
           (anvil-offload-pool-size 2)
           fired authoritative mirror
           ensure-error stop-error submit-error
           (spawn-count 0)
           watcher)
      (setq watcher
            (lambda (_symbol _new operation _where)
              (when (and (eq operation 'let) (not fired))
                (setq fired t
                      authoritative (anvil-offload--cleanup-active-p)
                      mirror anvil-offload--pool-cleanup-active-p)
                (condition-case err
                    (anvil-offload--ensure-pool-vector)
                  (error (setq ensure-error err)))
                (condition-case err
                    (anvil-offload-stop-repl)
                  (error (setq stop-error err)))
                (condition-case err
                    (anvil-offload '(+ 1 2) :isolated t)
                  (error (setq submit-error err))))))
      (add-variable-watcher
       'anvil-offload--pool-cleanup-active-p watcher)
      (unwind-protect
          (cl-letf (((symbol-function 'anvil-offload--spawn-isolated-process)
                     (lambda (_id)
                       (cl-incf spawn-count)
                       (error "isolated spawn escaped cleanup guard"))))
            (should (= 2 (length (anvil-offload--ensure-pool-vector)))))
        (remove-variable-watcher
         'anvil-offload--pool-cleanup-active-p watcher))
      (should fired)
      (should authoritative)
      (should-not mirror)
      (dolist (err (list ensure-error stop-error submit-error))
        (should err)
        (should (string-match-p "cleanup already active"
                                (error-message-string err))))
      (should (= 0 spawn-count))
      (should (= 0 anvil-offload--next-id))
      (should-not (anvil-offload--cleanup-active-p))
      (should-not anvil-offload--pool-cleanup-active-p))))

(ert-deftest anvil-offload-ownership-submission-cell-precedes-mirror-watcher ()
  "Submission reentry is rejected before allocation or process spawn."
  (anvil-offload-ownership-test--with-state
    (let ((spawn-count 0)
          fired authoritative mirror
          stop-error recursive-error watcher)
      (setq watcher
            (lambda (_symbol _new operation _where)
              (when (and (eq operation 'let) (not fired))
                (setq fired t
                      authoritative (anvil-offload--submission-active-p)
                      mirror anvil-offload--submission-active-p)
                (condition-case err
                    (anvil-offload-stop-repl)
                  (error (setq stop-error err)))
                (condition-case err
                    (anvil-offload '(+ 3 4) :isolated t)
                  (error (setq recursive-error err)))
                (error "submission mirror watcher"))))
      (add-variable-watcher 'anvil-offload--submission-active-p watcher)
      (unwind-protect
          (cl-letf (((symbol-function 'anvil-offload--spawn-isolated-process)
                     (lambda (_id)
                       (cl-incf spawn-count)
                       (error "isolated spawn escaped submission guard"))))
            (should-error (anvil-offload '(+ 1 2) :isolated t)))
        (remove-variable-watcher
         'anvil-offload--submission-active-p watcher))
      (should fired)
      (should authoritative)
      (should-not mirror)
      (should stop-error)
      (should recursive-error)
      (should (string-match-p "submission already active"
                              (error-message-string stop-error)))
      (should (string-match-p "submission already active"
                              (error-message-string recursive-error)))
      (should (= 0 spawn-count))
      (should (= 0 anvil-offload--next-id))
      (should-not (anvil-offload--submission-active-p))
      (should-not anvil-offload--submission-active-p))))

(ert-deftest anvil-offload-ownership-first-table-watcher-prevents-spawn ()
  "A first-table watcher signal occurs before any child is created."
  (anvil-offload-ownership-test--with-state
    (setq anvil-offload--isolated-processes nil)
    (let ((make-count 0)
          watcher)
      (setq watcher
            (lambda (_symbol _new operation _where)
              (when (eq operation 'set)
                (error "ownership table watcher"))))
      (add-variable-watcher 'anvil-offload--isolated-processes watcher)
      (unwind-protect
          (cl-letf ((anvil-offload--make-process-primitive
                     (lambda (&rest _args)
                       (cl-incf make-count)
                       (error "make-process unexpectedly called"))))
            (should-error (anvil-offload--spawn-isolated-process 1)))
        (remove-variable-watcher 'anvil-offload--isolated-processes watcher))
      (should (= 0 make-count)))))

(ert-deftest anvil-offload-ownership-registered-table-is-not-republished ()
  "Post-spawn custody does not reassign an already registered watched table."
  (anvil-offload-ownership-test--with-state
    (let ((real-make anvil-offload--make-process-primitive)
          created
          result
          (registry-sets 0)
          watcher)
      (setq watcher
            (lambda (_symbol _new operation _where)
              (when (eq operation 'set)
                (cl-incf registry-sets)
                (when (and (processp created)
                           (process-live-p created))
                  (error "registry watcher after spawn")))))
      (add-variable-watcher
       'anvil-offload--ownership-table-registry watcher)
      (unwind-protect
          (cl-letf
              (((symbol-function 'anvil-offload--process-command)
                (lambda ()
                  (list shell-file-name shell-command-switch "sleep 30")))
               (anvil-offload--make-process-primitive
                (lambda (&rest args)
                  (setq created (apply real-make args)))))
            (setq result
                  (anvil-offload--spawn-named-process
                   "anvil-ownership-registry-watcher" t)))
        (remove-variable-watcher
         'anvil-offload--ownership-table-registry watcher))
      (should (eq created result))
      (should (= 1 registry-sets))
      (should (process-live-p result))
      (should (anvil-offload-ownership-test--owned-p result))
      (should (anvil-offload--hard-delete-process result))
      (should-not (process-live-p result)))))

(ert-deftest anvil-offload-ownership-sentinel-retains-bounded-junk-diagnostic ()
  "A dead child reports its last bounded startup line to the owning future."
  (anvil-offload-ownership-test--with-state
    (let* ((id 301)
           (proc
            (make-process
             :name "anvil-ownership-dead-diagnostic"
             :buffer nil
             :command
             (list shell-file-name shell-command-switch "cat >/dev/null")
             :connection-type 'pipe
             :noquery t
             :filter #'anvil-offload--filter
             :sentinel #'ignore))
           (future
            (anvil-offload-ownership-test--future
             id proc :isolated-p t))
           (deadline (+ (float-time) 3))
           diagnostic)
      (process-put proc 'anvil-offload-isolated t)
      (anvil-offload--track-process-ownership
       proc t anvil-offload--isolated-processes)
      (puthash id future anvil-offload--pending)
      (anvil-offload--filter
       proc (concat "startup failure: " (make-string 300 ?x) "\n"))
      (setq diagnostic
            (process-get proc 'anvil-offload-last-junk-reply))
      (should (string-prefix-p "startup failure:" diagnostic))
      (should (<= (length diagnostic) 120))
      (set-process-sentinel proc #'anvil-offload--sentinel)
      (process-send-eof proc)
      (while (and (< (float-time) deadline)
                  (eq 'pending (anvil-future-status future)))
        (accept-process-output nil 0.02))
      (should (eq 'error (anvil-future-status future)))
      (should (string-match-p
               (regexp-quote diagnostic)
               (anvil-future-error future)))
      (should (< (length (anvil-future-error future)) 256))
      (should-not (gethash id anvil-offload--pending)))))

(ert-deftest anvil-offload-ownership-sentinel-preserves-actionable-junk-prefix ()
  "Later backtrace junk must not overwrite the actionable startup failure."
  (anvil-offload-ownership-test--with-state
    (let* ((id 302)
           (proc
            (make-process
             :name "anvil-ownership-dead-diagnostic-prefix"
             :buffer nil
             :command
             (list shell-file-name shell-command-switch "cat >/dev/null")
             :connection-type 'pipe
             :noquery t
             :filter #'anvil-offload--filter
             :sentinel #'ignore))
           (future
            (anvil-offload-ownership-test--future
             id proc :isolated-p t))
           (deadline (+ (float-time) 3))
           (first
            (concat "Actionable startup failure: " (make-string 180 ?x)))
           (last
            (concat (make-string 180 ?y) " normal-top-level()"))
           (first-preview (anvil-offload--line-preview first))
           (last-preview (anvil-offload--line-preview last))
           diagnostic)
      (process-put proc 'anvil-offload-isolated t)
      (anvil-offload--track-process-ownership
       proc t anvil-offload--isolated-processes)
      (puthash id future anvil-offload--pending)
      (anvil-offload--filter proc (format "%s\n%s\n" first last))
      (should
       (equal first-preview
              (process-get proc 'anvil-offload-first-junk-reply)))
      (should
       (equal last-preview
              (process-get proc 'anvil-offload-last-junk-reply)))
      (setq diagnostic (anvil-offload--junk-diagnostic proc))
      (should (string-prefix-p "Actionable startup failure:" diagnostic))
      (should (string-suffix-p "normal-top-level()" diagnostic))
      (should (<= (length diagnostic)
                  anvil-offload--junk-diagnostic-max-chars))
      (should
       (equal
        (mapcar
         (lambda (limit)
           (anvil-offload--bounded-ends "abcdef" limit))
         '(0 1 2 3))
        '("" "." ".." "...")))
      (should
       (equal
        (mapcar
         (lambda (length)
           (length
            (anvil-offload--exit-reason
             proc (make-string length ?b))))
         '(249 250 251 252))
        '(255 255 255 252)))
      (set-process-sentinel proc #'anvil-offload--sentinel)
      (process-send-eof proc)
      (while (and (< (float-time) deadline)
                  (eq 'pending (anvil-future-status future)))
        (accept-process-output nil 0.02))
      (should (eq 'error (anvil-future-status future)))
      (should
       (string-match-p
        (regexp-quote "Actionable startup failure:")
        (anvil-future-error future)))
      (should
       (string-match-p
        (regexp-quote "normal-top-level()")
        (anvil-future-error future)))
      (should (<= (length (anvil-future-error future))
                  anvil-offload--exit-reason-max-chars))
      (should-not (gethash id anvil-offload--pending)))))

(ert-deftest anvil-offload-ownership-sentinel-includes-late-final-filter-junk ()
  "The deferred death finalizer samples junk after the last filter callback."
  (anvil-offload-ownership-test--with-state
    (let* ((delete-exited-processes nil)
           (id 303)
           (settle-count 0)
           (proc
            (make-process
             :name "anvil-ownership-late-death-diagnostic"
             :buffer nil
             :command
             (list shell-file-name shell-command-switch "exit 1")
             :connection-type 'pipe
             :noquery t
             :filter #'anvil-offload--filter
             :sentinel #'ignore))
           (future
            (anvil-offload-ownership-test--future
             id proc :isolated-p t
             :on-settle (lambda (_future) (cl-incf settle-count))))
           (deadline (+ (float-time) 3))
           (late "late actionable diagnostic"))
      (process-put proc 'anvil-offload-isolated t)
      (anvil-offload--track-process-ownership
       proc t anvil-offload--isolated-processes)
      (puthash id future anvil-offload--pending)
      (while (and (process-live-p proc) (< (float-time) deadline))
        (accept-process-output proc 0.02))
      (should-not (process-live-p proc))
      (anvil-offload--sentinel proc "exited abnormally with code 1\n")
      (should (eq 'pending (anvil-future-status future)))
      (anvil-offload--filter proc (concat late "\n"))
      (while (and (< (float-time) deadline)
                  (eq 'pending (anvil-future-status future)))
        (accept-process-output nil 0.02))
      (should (eq 'error (anvil-future-status future)))
      (should (string-match-p
               (regexp-quote late)
               (anvil-future-error future)))
      (should (= settle-count 1))
      (should-not (gethash id anvil-offload--pending)))))

(ert-deftest anvil-offload-ownership-hard-delete-forgets-dead-child ()
  "Hard delete must remove an already-exited exact child from Emacs."
  (anvil-offload-ownership-test--with-state
    (let* ((delete-exited-processes nil)
           (proc
            (make-process
             :name "anvil-ownership-dead-hard-delete"
             :buffer nil
             :command
             (list shell-file-name shell-command-switch "exit 0")
             :connection-type 'pipe
             :noquery t
             :sentinel #'ignore))
           (deadline (+ (float-time) 2)))
      (anvil-offload--track-process-ownership
       proc :isolated anvil-offload--isolated-processes)
      (while (and (process-live-p proc)
                  (< (float-time) deadline))
        (accept-process-output proc 0.02))
      (should-not (process-live-p proc))
      (should (eq (process-status proc) 'exit))
      (should (memq proc (process-list)))
      (should (anvil-offload-ownership-test--owned-p proc))
      (should (anvil-offload--hard-delete-process proc))
      (should-not (memq proc (process-list)))
      (should-not (anvil-offload-ownership-test--owned-p proc)))))

(ert-deftest anvil-offload-ownership-sentinel-forgets-dead-child ()
  "Natural death settles first, then explicitly deletes the exact process."
  (skip-unless (memq system-type '(darwin gnu/linux)))
  (anvil-offload-ownership-test--with-state
    (let* ((delete-exited-processes nil)
           (id 302)
           (real-delete (symbol-function 'delete-process))
           (delete-count 0)
           settle-delete-count
           (proc
            (make-process
             :name "anvil-ownership-dead-sentinel"
             :buffer nil
             :command
             (list shell-file-name shell-command-switch "cat >/dev/null")
             :connection-type 'pipe
             :noquery t
             :sentinel #'anvil-offload--sentinel))
           (future
            (anvil-offload-ownership-test--future
             id proc :isolated-p t
             :on-settle
             (lambda (_future)
               (setq settle-delete-count delete-count))))
           (deadline (+ (float-time) 3)))
      (process-put proc 'anvil-offload-isolated t)
      (anvil-offload--track-process-ownership
       proc t anvil-offload--isolated-processes)
      (puthash id future anvil-offload--pending)
      (cl-letf (((symbol-function 'delete-process)
                 (lambda (candidate)
                   (when (eq candidate proc)
                     (cl-incf delete-count))
                   (funcall real-delete candidate))))
        (process-send-eof proc)
        (while (and (< (float-time) deadline)
                    (or (process-live-p proc)
                        (memq proc (process-list))
                        (eq 'pending (anvil-future-status future))))
          (accept-process-output nil 0.02))
        (should (eq 'error (anvil-future-status future)))
        (should (string-match-p "offload REPL exited"
                                (anvil-future-error future)))
        (should (equal 0 settle-delete-count))
        (should (> delete-count 0))
        (should-not (gethash id anvil-offload--pending))
        (should-not (memq proc (process-list)))
        (should-not
         (anvil-offload-ownership-test--owned-p proc))))))

(ert-deftest anvil-offload-ownership-hard-delete-marks-before-table-watcher ()
  "A first-table watcher cannot orphan a child while replacing its pool."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-delete-watcher")))
           (old (vector proc))
           (replacement (vector nil))
           (anvil-offload--pool old)
           (anvil-offload--isolated-processes nil)
           fired observed watcher)
      (setq watcher
            (lambda (_symbol _new operation _where)
              (when (and (eq operation 'set) (not fired))
                (setq fired t
                      observed
                      (process-get proc 'anvil-offload-retiring)
                      anvil-offload--pool replacement)
                (error "hard-delete table watcher"))))
      (add-variable-watcher 'anvil-offload--isolated-processes watcher)
      (unwind-protect
          (cl-letf (((symbol-function 'kill-process) #'ignore)
                    ((symbol-function 'delete-process) #'ignore))
            (should-error (anvil-offload--hard-delete-process proc)))
        (remove-variable-watcher
         'anvil-offload--isolated-processes watcher))
      (should fired)
      (should observed)
      (should (process-live-p proc))
      (should (eq replacement anvil-offload--pool))
      (should (eq proc (aref old 0)))
      (should (gethash proc anvil-offload--fallback-processes))
      (should (anvil-offload-ownership-test--owned-p proc))
      (anvil-offload-stop-repl)
      (should-not (process-live-p proc))
      (should-not (anvil-offload-ownership-test--owned-p proc)))))

(ert-deftest anvil-offload-ownership-stop-watcher-keeps-fail-closed-state ()
  "A signaling final-publication watcher leaves stop fail closed."
  (anvil-offload-ownership-test--with-state
    (let* ((old (vector nil))
           (anvil-offload--pool old)
           (anvil-offload--next-id 71)
           fired observed reentrant watcher)
      (setq watcher
            (lambda (_symbol _new operation _where)
              (when (and (eq operation 'set) (not fired))
                (setq fired t
                      observed
                      (list anvil-offload--pool-cleanup-active-p
                            anvil-offload--pool-retiring-p
                            anvil-offload--stop-retiring-p))
                (condition-case err
                    (anvil-offload '(+ 1 2) :isolated t)
                  (error (setq reentrant err)))
                (error "stop pool watcher"))))
      (add-variable-watcher 'anvil-offload--pool watcher)
      (unwind-protect
          (should-error (anvil-offload-stop-repl))
        (remove-variable-watcher 'anvil-offload--pool watcher))
      (should fired)
      (should (equal observed '(t t t)))
      (should (string-match-p "cleanup already active"
                              (error-message-string reentrant)))
      (should (= 71 anvil-offload--next-id))
      (should (eq old anvil-offload--pool))
      (should anvil-offload--pool-retiring-p)
      (should anvil-offload--stop-retiring-p)
      (anvil-offload-stop-repl)
      (should-not anvil-offload--pool)
      (should-not anvil-offload--pool-retiring-p)
      (should-not anvil-offload--stop-retiring-p))))

(ert-deftest anvil-offload-ownership-spawn-unwind-prefers-exact-local-proc ()
  "Custody primitives preserve the original exit and exact new child."
  (anvil-offload-ownership-test--with-state
    (let ((real-make-process anvil-offload--make-process-primitive)
          created advice-called outcome)
      (cl-letf
          (((symbol-function 'anvil-offload--process-command)
            (lambda ()
              (list shell-file-name shell-command-switch "sleep 30")))
           ((symbol-function 'process-put)
            (lambda (&rest _args)
              (setq advice-called t
                    anvil-offload--isolated-processes
                    (make-hash-table :test 'eq)
                    anvil-offload--ownership-table-registry nil)
              (throw 'anvil-ownership-replacement-exit :replaced)))
           (anvil-offload--make-process-primitive
            (lambda (&rest args)
              (setq created (apply real-make-process args))
              (throw 'anvil-ownership-original-exit :original)))
           ((symbol-function 'kill-process) #'ignore)
           ((symbol-function 'delete-process) #'ignore))
        (setq outcome
              (catch 'anvil-ownership-original-exit
                (catch 'anvil-ownership-replacement-exit
                  (anvil-offload--spawn-named-process
                   "anvil-ownership-exact-spawn" t)
                  :returned))))
      (should (eq outcome :original))
      (should-not advice-called)
      (should (process-live-p created))
      (should (anvil-offload-ownership-test--owned-p created)))))

(ert-deftest anvil-offload-ownership-spawn-quit-retains-exact-custody ()
  "Quit after child creation leaves the exact child globally retriable."
  (anvil-offload-ownership-test--with-state
    (let ((real-make anvil-offload--make-process-primitive)
          created quit-seen)
      (cl-letf (((symbol-function 'anvil-offload--process-command)
                 (lambda ()
                   (list shell-file-name shell-command-switch "sleep 30")))
                (anvil-offload--make-process-primitive
                 (lambda (&rest args)
                   (setq created (apply real-make args))
                   (signal 'quit nil)))
                ((symbol-function 'kill-process) #'ignore)
                ((symbol-function 'delete-process) #'ignore))
        (condition-case nil
            (anvil-offload--spawn-named-process
             "anvil-ownership-spawn-quit" t)
          (quit (setq quit-seen t))))
      (should quit-seen)
      (should (process-live-p created))
      (should (anvil-offload-ownership-test--owned-p created)))))

(ert-deftest anvil-offload-ownership-spawn-bypasses-puthash-advice ()
  "Runtime hash advice cannot interrupt first child custody publication."
  (anvil-offload-ownership-test--with-state
    (let ((real-puthash (symbol-function 'puthash))
          proc
          (process-key-calls 0))
      (cl-letf (((symbol-function 'anvil-offload--process-command)
                 (lambda ()
                   (list shell-file-name shell-command-switch "sleep 30")))
                ((symbol-function 'puthash)
                 (lambda (key value table)
                   (if (processp key)
                       (progn
                         (cl-incf process-key-calls)
                         (error "process-key puthash refused"))
                     (funcall real-puthash key value table)))))
        (setq proc
              (anvil-offload--spawn-named-process
               "anvil-ownership-puthash-advice" t)))
      (should (process-live-p proc))
      (should (= 0 process-key-calls))
      (should (anvil-offload-ownership-test--owned-p proc)))))

(ert-deftest anvil-offload-ownership-spawn-recovers-moved-buffer-child ()
  "A child moved before make-process throws remains globally owned."
  (anvil-offload-ownership-test--with-state
    (let ((real-make anvil-offload--make-process-primitive)
          (other-buffer (get-buffer-create " *anvil-ownership-moved-child*"))
          created outcome)
      (cl-letf (((symbol-function 'anvil-offload--process-command)
                 (lambda ()
                   (list shell-file-name shell-command-switch "sleep 30")))
                (anvil-offload--make-process-primitive
                 (lambda (&rest args)
                   (setq created (apply real-make args))
                   (set-process-buffer created other-buffer)
                   (throw 'anvil-ownership-moved-exit 'moved-exit)))
                ((symbol-function 'kill-process) #'ignore)
                ((symbol-function 'delete-process) #'ignore))
        (setq outcome
              (catch 'anvil-ownership-moved-exit
            (anvil-offload--spawn-named-process
                 "anvil-ownership-moved-spawn" t)
                'returned)))
      (should (eq 'moved-exit outcome))
      (should (process-live-p created))
      (should (eq other-buffer (process-buffer created)))
      (should (anvil-offload-ownership-test--owned-p created)))))

(defun anvil-offload-ownership-test--exercise-send-exit (isolated exit)
  "Exercise public send EXIT for an ISOLATED or pooled submission."
  (let* ((proc
          (anvil-offload-ownership-test--compatible
           (anvil-offload-ownership-test--idle-process
            (format "anvil-ownership-send-%s-%s" isolated exit))))
         (table anvil-offload--isolated-processes)
         (real-maker (symbol-function 'make-anvil-future))
         future callback-called pending-seen outcome)
    (unless isolated
      (setq anvil-offload--pool (vector proc)))
    (cl-letf
        (((symbol-function 'make-anvil-future)
          (lambda (&rest args)
            (setq future (apply real-maker args))))
         ((symbol-function 'anvil-offload--pick-worker)
          (lambda () proc))
         ((symbol-function 'anvil-offload--spawn-isolated-process)
          (lambda (_id)
            (process-put proc 'anvil-offload-isolated t)
            (anvil-offload--track-process-ownership proc t table)
            proc))
         ((symbol-function 'process-send-string)
          (lambda (_proc _string)
            (setq pending-seen
                  (= 1 (hash-table-count anvil-offload--pending)))
            (pcase exit
              ('error (error "deterministic send error"))
              ('quit (signal 'quit nil))
              ('throw (throw 'anvil-ownership-send-exit 'throw))))))
      (setq outcome
            (catch 'anvil-ownership-send-exit
              (condition-case _err
                  (progn
                    (anvil-offload
                     '(+ 1 2) :isolated isolated
                     :on-settle (lambda (_future)
                                  (setq callback-called t)))
                    'returned)
                (quit 'quit)
                (error 'error)))))
    (should (eq exit outcome))
    (should pending-seen)
    (should-not callback-called)
    (should (anvil-future-p future))
    (should (eq 'error (anvil-future-status future)))
    (should (eq 'submission-aborted
                (anvil-future--terminal-reason future)))
    (should (= 0 (hash-table-count anvil-offload--pending)))
    (should (process-get proc 'anvil-offload-retiring))
    (should (anvil-offload-ownership-test--owned-p proc))
    (let ((deadline (+ (float-time) 1.0)))
      (while (and (process-live-p proc) (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should-not (process-live-p proc))
    (should-not (anvil-offload-ownership-test--owned-p proc))
    (when (and (not isolated) anvil-offload--pool)
      (should-not (aref anvil-offload--pool 0)))))

(ert-deftest anvil-offload-ownership-pooled-send-error-cleans-transaction ()
  (anvil-offload-ownership-test--with-state
    (anvil-offload-ownership-test--exercise-send-exit nil 'error)))

(ert-deftest anvil-offload-ownership-isolated-send-error-cleans-transaction ()
  (anvil-offload-ownership-test--with-state
    (anvil-offload-ownership-test--exercise-send-exit t 'error)))

(ert-deftest anvil-offload-ownership-pooled-send-quit-cleans-transaction ()
  (anvil-offload-ownership-test--with-state
    (anvil-offload-ownership-test--exercise-send-exit nil 'quit)))

(ert-deftest anvil-offload-ownership-isolated-send-quit-cleans-transaction ()
  (anvil-offload-ownership-test--with-state
    (anvil-offload-ownership-test--exercise-send-exit t 'quit)))

(ert-deftest anvil-offload-ownership-pooled-send-throw-cleans-transaction ()
  (anvil-offload-ownership-test--with-state
    (anvil-offload-ownership-test--exercise-send-exit nil 'throw)))

(ert-deftest anvil-offload-ownership-isolated-send-throw-cleans-transaction ()
  (anvil-offload-ownership-test--with-state
    (anvil-offload-ownership-test--exercise-send-exit t 'throw)))

(ert-deftest anvil-offload-ownership-abort-preserves-original-cleanup-throw ()
  "Fallible custody advice cannot replace the original transport throw."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-abort-cleanup")))
           (anvil-offload--pool (vector proc))
           (real-puthash (symbol-function 'puthash))
           (process-key-calls 0)
           (pending-key-calls 0)
           (timer-calls 0)
           outcome)
      (cl-letf
          (((symbol-function 'anvil-offload--pick-worker)
            (lambda () proc))
           ((symbol-function 'puthash)
            (lambda (key value table)
              (if (processp key)
                  (progn
                    (cl-incf process-key-calls)
                    (throw 'anvil-ownership-custody-fault 'custody-fault))
                (cl-incf pending-key-calls)
                (funcall real-puthash key value table))))
           ((symbol-function 'process-send-string)
            (lambda (_proc _string)
              (throw 'anvil-ownership-original-send 'original-send)))
           ((symbol-function 'run-at-time)
            (lambda (&rest _args)
              (cl-incf timer-calls)
              (throw 'anvil-ownership-cleanup-timer 'cleanup-timer))))
        ;; Prove the installed process-key fault really would throw if the
        ;; abort path called the advised symbol instead of its captured
        ;; primitive.
        (should
         (eq 'custody-fault
             (catch 'anvil-ownership-custody-fault
               (puthash proc t (make-hash-table :test 'eq))
               'returned)))
        (setq process-key-calls 0)
        (setq outcome
              (catch 'anvil-ownership-original-send
                (anvil-offload '(+ 1 2))
                'returned)))
      (should (eq 'original-send outcome))
      (should (= 0 process-key-calls))
      (should (= 1 pending-key-calls))
      (should (= 0 timer-calls))
      (should (= 0 (hash-table-count anvil-offload--pending)))
      (should (process-get proc 'anvil-offload-retiring))
      (should (anvil-offload-ownership-test--owned-p proc))
      (let ((deadline (+ (float-time) 1.0)))
        (while (and (process-live-p proc) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should-not (process-live-p proc)))))

(ert-deftest anvil-offload-ownership-future-kill-retires-before-callback ()
  "A kill callback cannot reacquire the exact compatible pooled child."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-future-kill")))
           (anvil-offload--pool (vector proc))
           observed reentrant
           (future
            (make-anvil-future
             :id 41 :process proc :status 'pending
             :on-settle
             (lambda (_future)
               (setq observed
                     (process-get proc 'anvil-offload-retiring))
               (condition-case err
                   (anvil-offload--ensure-slot 0)
                 (error (setq reentrant err)))))))
      (puthash 41 future anvil-offload--pending)
      (cl-letf (((symbol-function 'kill-process) #'ignore)
                ((symbol-function 'delete-process) #'ignore))
        (anvil-future-kill future 'test-kill))
      (should observed)
      (should reentrant)
      (should (eq proc (aref anvil-offload--pool 0)))
      (should (process-live-p proc))
      (should (anvil-offload-ownership-test--owned-p proc))
      (should-not (gethash 41 anvil-offload--pending)))))

(ert-deftest
    anvil-offload-ownership-future-kill-callback-exit-defers-cleanup ()
  "Future-kill preserves a callback throw across first-table publication."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-future-kill-exit")))
           (anvil-offload--pool (vector proc))
           (future
            (anvil-offload-ownership-test--future
             402 proc
             :on-settle
             (lambda (_future)
               (throw 'anvil-ownership-kill-exit 'escaped))))
           watcher
           watcher-fired
           outcome)
      (puthash 402 future anvil-offload--pending)
      (setq anvil-offload--isolated-processes nil
            watcher
            (lambda (_symbol _new operation _where)
              (when (eq operation 'set)
                (setq watcher-fired t)
                (error "kill cleanup table watcher"))))
      (add-variable-watcher 'anvil-offload--isolated-processes watcher)
      (unwind-protect
          (setq outcome
                (catch 'anvil-ownership-kill-exit
                  (anvil-future-kill future 'test-kill)
                  'returned))
        (remove-variable-watcher
         'anvil-offload--isolated-processes watcher))
      (should (eq 'escaped outcome))
      (should-not watcher-fired)
      (should (eq 'killed (anvil-future-status future)))
      (should-not (gethash 402 anvil-offload--pending))
      (let ((deadline (+ (float-time) 1.0)))
        (while (and (process-live-p proc) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should-not (process-live-p proc))
      (should-not (aref anvil-offload--pool 0)))))

(ert-deftest anvil-offload-ownership-frame-overflow-retires-before-callback ()
  "A frame failure callback cannot reacquire the exact pooled child."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-frame-overflow")))
           (anvil-offload--pool (vector proc))
           (anvil-offload-max-frame-bytes 1)
           observed reentrant
           (future
            (make-anvil-future
             :id 42 :process proc :status 'pending
             :on-settle
             (lambda (_future)
               (setq observed
                     (process-get proc 'anvil-offload-retiring))
               (condition-case err
                   (anvil-offload--ensure-slot 0)
                 (error (setq reentrant err)))))))
      (puthash 42 future anvil-offload--pending)
      (cl-letf (((symbol-function 'kill-process) #'ignore)
                ((symbol-function 'delete-process) #'ignore))
        (anvil-offload--filter proc "xx"))
      (should observed)
      (should reentrant)
      (should (eq 'error (anvil-future-status future)))
      (should (eq proc (aref anvil-offload--pool 0)))
      (should (process-live-p proc))
      (should (anvil-offload-ownership-test--owned-p proc)))))

(ert-deftest anvil-offload-ownership-settle-snapshots-isolated-child ()
  "A settling callback preserves its exit and cannot redirect exact cleanup."
  (anvil-offload-ownership-test--with-state
    (let* ((child
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-settle-child"))
           (peer
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-settle-peer"))
           (future
            (make-anvil-future
             :id 51 :process child :status 'pending :isolated-p t
             :on-settle
             (lambda (settled)
               (setf (anvil-future--process settled) peer
                     (anvil-future--isolated-p settled) nil)
               (throw 'anvil-ownership-settle-exit 'escaped))))
           watcher
           watcher-fired
           outcome)
      (anvil-offload--track-process-ownership
       child t anvil-offload--isolated-processes)
      (puthash 51 future anvil-offload--pending)
      (setq anvil-offload--isolated-processes nil
            watcher
            (lambda (_symbol _new operation _where)
              (when (eq operation 'set)
                (setq watcher-fired t)
                (error "settle cleanup table watcher"))))
      (add-variable-watcher 'anvil-offload--isolated-processes watcher)
      (unwind-protect
          (setq outcome
                (catch 'anvil-ownership-settle-exit
                  (anvil-offload--settle-reply child future 'done 9 'done)
                  'returned))
        (remove-variable-watcher
         'anvil-offload--isolated-processes watcher))
      (should (eq 'escaped outcome))
      (should-not watcher-fired)
      (let ((deadline (+ (float-time) 1.0)))
        (while (and (process-live-p child) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should-not (process-live-p child))
      (should (process-live-p peer))
      (should-not (gethash 51 anvil-offload--pending)))))

(ert-deftest anvil-offload-ownership-start-snapshots-exact-owner ()
  "A start callback preserves its exit and cannot redirect exact cleanup."
  (anvil-offload-ownership-test--with-state
    (let* ((child
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-start-child"))
           (peer
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-start-peer"))
           (future
            (make-anvil-future
             :id 52 :process child :status 'pending :isolated-p t
             :on-start
             (lambda (started)
               (setf (anvil-future--id started) 999
                     (anvil-future--process started) peer
                     (anvil-future--isolated-p started) nil)
               (throw 'anvil-ownership-start-exit 'escaped))))
           watcher
           watcher-fired
           outcome)
      (anvil-offload--track-process-ownership
       child t anvil-offload--isolated-processes)
      (puthash 52 future anvil-offload--pending)
      (setq anvil-offload--isolated-processes nil
            watcher
            (lambda (_symbol _new operation _where)
              (when (eq operation 'set)
                (setq watcher-fired t)
                (error "start cleanup table watcher"))))
      (add-variable-watcher 'anvil-offload--isolated-processes watcher)
      (unwind-protect
          (setq outcome
                (catch 'anvil-ownership-start-exit
                  (anvil-offload--mark-started future)
                  'returned))
        (remove-variable-watcher
         'anvil-offload--isolated-processes watcher))
      (should (eq 'escaped outcome))
      (should-not watcher-fired)
      (let ((deadline (+ (float-time) 1.0)))
        (while (and (process-live-p child) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should-not (process-live-p child))
      (should (process-live-p peer))
      (should (eq child (anvil-future--process future)))
      (should (= 52 (anvil-future--id future)))
      (should (anvil-future--isolated-p future))
      (should (eq 'killed (anvil-future-status future)))
      (should-not (gethash 52 anvil-offload--pending)))))

(ert-deftest
    anvil-offload-ownership-materialize-exit-preserves-all-fragments ()
  "Materialization errors and throws leave every queued fragment replayable."
  (anvil-offload-ownership-test--with-state
    (dolist (exit-kind '(error throw))
      (let* ((proc
              (anvil-offload-ownership-test--idle-process
               (format "anvil-ownership-materialize-%s" exit-kind)))
             (state (anvil-offload--filter-queue proc))
             outcome)
        (anvil-offload--filter-queue-append state "first-")
        (anvil-offload--filter-queue-append state "second")
        (cl-letf (((symbol-function 'mapconcat)
                   (lambda (&rest _args)
                     (pcase exit-kind
                       ('error (error "materialize error"))
                       ('throw
                        (throw 'anvil-ownership-materialize-exit
                               'escaped))))))
          (setq outcome
                (pcase exit-kind
                  ('error
                   (condition-case _err
                       (progn
                         (anvil-offload--filter-queue-materialize state)
                         'returned)
                     (error 'escaped)))
                  ('throw
                   (catch 'anvil-ownership-materialize-exit
                     (anvil-offload--filter-queue-materialize state)
                     'returned)))))
        (should (eq 'escaped outcome))
        (should (equal "first-second"
                       (anvil-offload--pending-bytes-string proc)))))))

(ert-deftest anvil-offload-ownership-fragmented-frame-materializes-once ()
  "A near-limit frame arriving in tiny chunks settles exactly once linearly."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-fragmented-frame"))
           (payload (make-string (* 1024 1024) ?z))
           (frame
            (anvil-offload-ownership-test--frame
             (list :id 601 :ok payload)))
           (anvil-offload-max-frame-bytes (1+ (string-bytes frame)))
           (settle-count 0)
           (future
            (anvil-offload-ownership-test--future
             601 proc :on-settle
             (lambda (_future) (cl-incf settle-count))))
           (real-materialize
            (symbol-function 'anvil-offload--filter-queue-materialize))
           (chunks
            (let ((position 0)
                  parts)
              (while (< position (length frame))
                (let ((end (min (length frame) (+ position 512))))
                  (push (substring frame position end) parts)
                  (setq position end)))
              (nreverse parts)))
           (materializations 0)
           (materialized-bytes 0))
      (puthash 601 future anvil-offload--pending)
      (cl-letf
          (((symbol-function 'anvil-offload--filter-queue-materialize)
            (lambda (state)
              (cl-incf materializations)
              (cl-incf materialized-bytes (aref state 5))
              (funcall real-materialize state))))
        (dolist (chunk chunks)
          (anvil-offload--filter proc chunk)))
      (should (= 1 materializations))
      (should (<= materialized-bytes (* 2 (string-bytes frame))))
      (should (= 1 settle-count))
      (should (eq 'done (anvil-future-status future)))
      (should (equal payload (anvil-future-value future)))
      (should-not (gethash 601 anvil-offload--pending))
      (should (= 0 (anvil-offload--filter-queue-total-bytes proc)))
      (should (string-empty-p
               (anvil-offload--pending-bytes-string proc))))))

(ert-deftest anvil-offload-ownership-coalesced-throw-replays-later-frame ()
  "A tagged first callback exit cannot discard a later coalesced frame."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-coalesced"))
           (first-count 0)
           (second-count 0)
           (timer-calls 0)
           (first
            (make-anvil-future
             :id 61 :process proc :status 'pending
             :on-settle
             (lambda (_future)
               (cl-incf first-count)
               (throw 'anvil-ownership-frame-exit 'escaped))))
           (second
            (make-anvil-future
             :id 62 :process proc :status 'pending
             :on-settle (lambda (_future) (cl-incf second-count))))
           (second-frame
            (anvil-offload-ownership-test--frame '(:id 62 :ok second)))
           (chunk
            (concat
             (anvil-offload-ownership-test--frame '(:id 61 :ok first))
             second-frame)))
      (puthash 61 first anvil-offload--pending)
      (puthash 62 second anvil-offload--pending)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (cl-incf timer-calls)
                   (throw 'anvil-ownership-wrong-timer 'wrong-timer))))
        (should
         (eq 'escaped
             (catch 'anvil-ownership-frame-exit
               (anvil-offload--filter proc chunk)
               'returned))))
      (should (= 0 timer-calls))
      (should (eq 'done (anvil-future-status first)))
      (should (eq 'pending (anvil-future-status second)))
      (should (equal second-frame (anvil-offload--pending-bytes-string proc)))
      (let ((deadline (+ (float-time) 1.0)))
        (while (and (eq 'pending (anvil-future-status second))
                    (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (eq 'done (anvil-future-status second)))
      (should (eq 'second (anvil-future-value second)))
      (should (= 1 first-count))
      (should (= 1 second-count))
      (should (string-empty-p
               (or (anvil-offload--pending-bytes-string proc) "")))
      (should (= 0 (hash-table-count anvil-offload--pending))))))

(ert-deftest anvil-offload-ownership-sibling-mutation-cannot-redirect-reply ()
  "One callback cannot redirect or terminalize a sibling registration."
  (anvil-offload-ownership-test--with-state
    (let* ((owner
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-sibling-owner"))
           (peer
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-sibling-peer"))
           second
           (first
            (anvil-offload-ownership-test--future
             81 owner
             :on-settle
             (lambda (_future)
               (setf (anvil-future--id second) 999
                     (anvil-future--process second) peer
                     (anvil-future--status second) 'done
                     (anvil-future--isolated-p second) t))))
           (second-count 0))
      (setq second
            (anvil-offload-ownership-test--future
             82 owner :on-settle
             (lambda (_future) (cl-incf second-count))))
      (puthash 81 first anvil-offload--pending)
      (puthash 82 second anvil-offload--pending)
      (anvil-offload--filter
       owner
       (concat
        (anvil-offload-ownership-test--frame '(:id 81 :ok first))
        (anvil-offload-ownership-test--frame '(:id 82 :ok second))))
      (should (eq 'done (anvil-future-status second)))
      (should (eq 'second (anvil-future-value second)))
      (should (= 82 (anvil-future--id second)))
      (should (eq owner (anvil-future--process second)))
      (should-not (anvil-future--isolated-p second))
      (should (= 1 second-count))
      (should (= 0 (hash-table-count anvil-offload--pending)))
      (should (process-live-p peer)))))

(ert-deftest anvil-offload-ownership-start-public-kill-uses-registration ()
  "A start callback kill cannot be redirected through visible fields."
  (anvil-offload-ownership-test--with-state
    (let* ((owner
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-start-kill-owner"))
           (peer
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-start-kill-peer"))
           future)
      (setq future
            (anvil-offload-ownership-test--future
             83 owner :on-start
             (lambda (started)
               (setf (anvil-future--id started) 999
                     (anvil-future--process started) peer)
               (anvil-future-kill started 'callback-kill))))
      (puthash 83 future anvil-offload--pending)
      (anvil-offload--mark-started future)
      (should (eq 'killed (anvil-future-status future)))
      (should (= 83 (anvil-future--id future)))
      (should (eq owner (anvil-future--process future)))
      (should-not (gethash 83 anvil-offload--pending))
      (should-not (process-live-p owner))
      (should (process-live-p peer)))))

(ert-deftest anvil-offload-ownership-start-status-mutation-is-reconciled ()
  "A contained start callback error cannot forge terminal state."
  (anvil-offload-ownership-test--with-state
    (let* ((owner
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-start-status"))
           (future
            (anvil-offload-ownership-test--future
             84 owner :on-start
             (lambda (started)
               (setf (anvil-future--status started) 'done)
               (error "contained start error")))))
      (puthash 84 future anvil-offload--pending)
      (anvil-offload--mark-started future)
      (should (eq 'pending (anvil-future-status future)))
      (should (eq future (gethash 84 anvil-offload--pending)))
      (should (process-live-p owner))
      (anvil-future-kill future 'test-cleanup))))

(ert-deftest anvil-offload-ownership-start-await-cancel-use-registration ()
  "Mutated visible owner/id cannot redirect await or cancel a sibling key."
  (anvil-offload-ownership-test--with-state
    (let* ((owner
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-await-cancel-owner"))
           (dead-peer
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-await-cancel-dead"))
           (sibling (anvil-offload-ownership-test--future 94 owner))
           awaited
           future)
      (delete-process dead-peer)
      (setq future
            (anvil-offload-ownership-test--future
             93 owner :on-start
             (lambda (started)
               (setf (anvil-future--id started) 94
                     (anvil-future--process started) dead-peer)
               (setq awaited (anvil-future-await started 0))
               (anvil-future-cancel started))))
      (puthash 93 future anvil-offload--pending)
      (puthash 94 sibling anvil-offload--pending)
      (anvil-offload--mark-started future)
      (should-not awaited)
      (should (eq 'cancelled (anvil-future-status future)))
      (should (= 93 (anvil-future--id future)))
      (should (eq owner (anvil-future--process future)))
      (should-not (gethash 93 anvil-offload--pending))
      (should (eq sibling (gethash 94 anvil-offload--pending)))
      (should (process-live-p owner))
      (anvil-future-kill sibling 'test-cleanup))))

(ert-deftest anvil-offload-ownership-finalize-snapshots-all-siblings ()
  "Dead-process finalization is independent of sibling callback mutation."
  (anvil-offload-ownership-test--with-state
    (let* ((owner
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-finalize-owner"))
           (peer
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-finalize-peer"))
           second
           (first
            (anvil-offload-ownership-test--future
             85 owner :on-settle
             (lambda (_future)
               (setf (anvil-future--id second) 999
                     (anvil-future--process second) peer
                     (anvil-future--status second) 'done)
               (throw 'anvil-ownership-finalize-exit 'escaped)))))
      (setq second (anvil-offload-ownership-test--future 86 owner))
      (puthash 85 first anvil-offload--pending)
      (puthash 86 second anvil-offload--pending)
      (delete-process owner)
      (should
       (eq 'escaped
           (catch 'anvil-ownership-finalize-exit
             (anvil-offload--finalize-dead-process owner "dead owner")
             'returned)))
      (dolist (future (list first second))
        (should (eq 'error (anvil-future-status future)))
        (should (equal "dead owner" (anvil-future-error future))))
      (should (= 86 (anvil-future--id second)))
      (should (eq owner (anvil-future--process second)))
      (should (= 0 (hash-table-count anvil-offload--pending)))
      (should (process-live-p peer)))))

(ert-deftest anvil-offload-ownership-recursive-filter-preserves-partial-frame ()
  "Recursive filter input appends behind the outer wire-order queue."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-recursive-filter"))
           (third-frame
            (anvil-offload-ownership-test--frame '(:id 89 :ok third)))
           (split (/ (length third-frame) 2))
           (third-first (substring third-frame 0 split))
           (third-rest (substring third-frame split))
           (first
            (anvil-offload-ownership-test--future
             87 proc :on-settle
             (lambda (_future)
               (anvil-offload--filter proc third-first))))
           (second (anvil-offload-ownership-test--future 88 proc))
           (third (anvil-offload-ownership-test--future 89 proc)))
      (puthash 87 first anvil-offload--pending)
      (puthash 88 second anvil-offload--pending)
      (puthash 89 third anvil-offload--pending)
      (anvil-offload--filter
       proc
       (concat
        (anvil-offload-ownership-test--frame '(:id 87 :ok first))
        (anvil-offload-ownership-test--frame '(:id 88 :ok second))))
      (should (eq 'done (anvil-future-status first)))
      (should (eq 'done (anvil-future-status second)))
      (should (eq 'pending (anvil-future-status third)))
      (should (equal third-first (anvil-offload--pending-bytes-string proc)))
      (anvil-offload--filter proc third-rest)
      (should (eq 'done (anvil-future-status third)))
      (should (eq 'third (anvil-future-value third)))
      (should (= 0 (hash-table-count anvil-offload--pending)))
      (should (string-empty-p
               (or (anvil-offload--pending-bytes-string proc) ""))))))

(ert-deftest anvil-offload-ownership-callback-can-await-later-frame ()
  "A callback can service and await a later registered frame reentrantly."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-callback-await"))
           (second (anvil-offload-ownership-test--future 92 proc))
           awaited
           (first
            (anvil-offload-ownership-test--future
             91 proc :on-settle
             (lambda (_future)
               (run-at-time
                0 nil #'anvil-offload--filter proc
                (anvil-offload-ownership-test--frame
                 '(:id 92 :ok awaited)))
               (setq awaited (anvil-future-await second 0.5))))))
      (puthash 91 first anvil-offload--pending)
      (puthash 92 second anvil-offload--pending)
      (anvil-offload--filter
       proc (anvil-offload-ownership-test--frame '(:id 91 :ok first)))
      (should awaited)
      (should (eq 'done (anvil-future-status second)))
      (should (eq 'awaited (anvil-future-value second)))
      (should (= 0 (hash-table-count anvil-offload--pending))))))

(ert-deftest anvil-offload-ownership-callback-awaits-coalesced-sibling ()
  "A callback can await a sibling already queued in the same input chunk."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-coalesced-await"))
           (second (anvil-offload-ownership-test--future 94 proc))
           awaited
           callback-status
           (first
            (anvil-offload-ownership-test--future
             93 proc :on-settle
             (lambda (_future)
               (setq awaited (anvil-future-await second 0.5)
                     callback-status (anvil-future-status second))))))
      (puthash 93 first anvil-offload--pending)
      (puthash 94 second anvil-offload--pending)
      (anvil-offload--filter
       proc
       (concat
        (anvil-offload-ownership-test--frame '(:id 93 :ok first))
        (anvil-offload-ownership-test--frame '(:id 94 :ok second))))
      (should awaited)
      (should (eq 'done callback-status))
      (should (eq 'done (anvil-future-status second)))
      (should (eq 'second (anvil-future-value second)))
      (should (= 0 (hash-table-count anvil-offload--pending)))
      (should (string-empty-p
               (or (anvil-offload--pending-bytes-string proc) ""))))))

(defun anvil-offload-ownership-test--assert-bad-frame-terminal
    (name payload)
  "Assert framed PAYLOAD terminalizes NAME's owned request."
  (let* ((proc (anvil-offload-ownership-test--idle-process name))
         (future (anvil-offload-ownership-test--future 90 proc)))
    (puthash 90 future anvil-offload--pending)
    (anvil-offload--filter
     proc (concat anvil-offload--frame-prefix payload "\n"))
    (should (eq 'error (anvil-future-status future)))
    (should (string-match-p "reply frame" (anvil-future-error future)))
    (should-not (gethash 90 anvil-offload--pending))
    (should-not (process-live-p proc))))

(ert-deftest anvil-offload-ownership-malformed-frames-fail-owned-process ()
  "Malformed reserved frames cannot leave futures pending forever."
  (anvil-offload-ownership-test--with-state
    (anvil-offload-ownership-test--assert-bad-frame-terminal
     "anvil-ownership-bad-base64" "%%%not-base64%%%")
    (anvil-offload-ownership-test--assert-bad-frame-terminal
     "anvil-ownership-bad-shape"
     (anvil-offload--frame-encode-payload "(:id 90 :unknown t)"))
    (anvil-offload-ownership-test--assert-bad-frame-terminal
     "anvil-ownership-missing-start-value"
     (anvil-offload--frame-encode-payload "(:id 90 :started)"))
    (anvil-offload-ownership-test--assert-bad-frame-terminal
     "anvil-ownership-missing-checkpoint-value"
     (anvil-offload--frame-encode-payload "(:id 90 :checkpoint)"))
    (anvil-offload-ownership-test--assert-bad-frame-terminal
     "anvil-ownership-duplicate-key"
     (anvil-offload--frame-encode-payload
      "(:id 90 :id 90 :ok duplicate)"))))

(ert-deftest anvil-offload-ownership-unreadable-result-is-readable-error ()
  "An unreadable child value errors cleanly and leaves its pool reusable."
  (anvil-offload-ownership-test--with-state
    (let* ((future
            (anvil-offload
             '(progn
                (setq print-unreadable-function
                      (lambda (&rest _args) "nil"))
                (current-buffer))))
           (owner (anvil-offload--registered-process future)))
      (should (anvil-future-await future 5))
      (should (eq 'error (anvil-future-status future)))
      (should (string-match-p
               "unreadable offload reply" (anvil-future-error future)))
      (should (process-live-p owner))
      (let ((bare
             (anvil-offload
              '(progn
                 (setq print-symbols-bare t)
                 (car (read-positioning-symbols "(wire-position)"))))))
        (should (anvil-future-await bare 5))
        (should (eq 'error (anvil-future-status bare)))
        (should (string-match-p
                 "unreadable offload reply" (anvil-future-error bare))))
      (let ((next (anvil-offload '(+ 1 2))))
        (should (eq owner (anvil-offload--registered-process next)))
        (should (anvil-future-await next 5))
        (should (eq 3 (anvil-future-value next)))))))

(ert-deftest anvil-offload-ownership-normalizes-remote-error-diagnostics ()
  "Hostile child printer state cannot truncate or replace error details."
  (anvil-offload-ownership-test--with-state
    (let ((future
           (anvil-offload
            '(progn
               (setq print-length 1
                     print-level 1
                     print-unreadable-function
                     (lambda (&rest _args) "nil")
                     print-continuous-numbering t
                     print-number-table nil
                     print-symbols-bare t)
               (signal 'error (list "wire boom" '(a b c)))))))
      (should (anvil-future-await future 5))
      (should (eq 'error (anvil-future-status future)))
      (should
       (equal "(error \"wire boom\" (a b c))"
              (anvil-future-error future))))
    (let ((unreadable
           (anvil-offload
            '(signal 'error (list "buffer boom" (current-buffer))))))
      (should (anvil-future-await unreadable 5))
      (should (eq 'error (anvil-future-status unreadable)))
      (should (string-match-p
               "buffer boom.*#<.*buffer"
               (anvil-future-error unreadable))))
    (let ((next (anvil-offload '(+ 1 2))))
      (should (anvil-future-await next 5))
      (should (= 3 (anvil-future-value next))))))

(ert-deftest anvil-offload-ownership-unreadable-checkpoint-aborts-eval ()
  "An unreadable checkpoint cannot terminalize while its worker stays busy."
  (anvil-offload-ownership-test--with-state
    (let* ((future
            (anvil-offload
             '(progn
                (anvil-preempt-checkpoint (current-buffer))
                (while t))))
           (owner (anvil-offload--registered-process future)))
      (should (anvil-future-await future 5))
      (should (eq 'error (anvil-future-status future)))
      (should-not (anvil-future-checkpoint future))
      (let ((next (anvil-offload '(+ 1 2))))
        (should (eq owner (anvil-offload--registered-process next)))
        (should (anvil-future-await next 5))
        (should (= 3 (anvil-future-value next)))))))

(ert-deftest anvil-offload-ownership-circular-results-preserve-topology ()
  "Readable circular and shared values survive the framed child boundary."
  (anvil-offload-ownership-test--with-state
    (let ((circular
           (anvil-offload
            '(let ((value (list 'node)))
               (setcdr value value)
               value))))
      (should (anvil-future-await circular 5))
      (let ((value (anvil-future-value circular)))
        (should (eq value (cdr value)))))
    (let ((shared
           (anvil-offload
            '(let ((value (list 'shared)))
               (list value value)))))
      (should (anvil-future-await shared 5))
      (let ((value (anvil-future-value shared)))
        (should (eq (car value) (cadr value)))))))

(ert-deftest anvil-offload-ownership-normalizes-wire-printer-state ()
  "Caller and pooled-child printer globals cannot corrupt wire objects."
  (anvil-offload-ownership-test--with-state
    (let ((print-length 3)
          (print-level 1)
          (print-circle nil)
          (print-gensym nil)
          (float-output-format "%.1f")
          (future (anvil-offload '(+ 1 2) :isolated t)))
      (should (anvil-future-await future 5))
      (should (= 3 (anvil-future-value future))))
    (let ((poison
           (anvil-offload
            '(progn
               (setq print-length 5
                     print-level 2
                     print-circle nil
                     print-gensym nil
                     print-unreadable-function
                     (lambda (&rest _args) "nil")
                     print-continuous-numbering t
                     print-number-table nil
                     print-symbols-bare t
                     print-quoted nil
                     print-integers-as-characters t
                     float-output-format "%.1f")
               'configured))))
      (should (anvil-future-await poison 5))
      (should (eq 'configured (anvil-future-value poison))))
    (let ((complete
           (anvil-offload
            '(list (number-sequence 1 10) 1.23456789))))
      (should (anvil-future-await complete 5))
      (should
       (equal '((1 2 3 4 5 6 7 8 9 10) 1.23456789)
              (anvil-future-value complete))))
    (let ((property
           (anvil-offload
            '(progn
               (setq print-charset-text-property nil)
               (propertize "x" 'charset 'latin-iso8859-1)))))
      (should (anvil-future-await property 5))
      (should
       (eq 'latin-iso8859-1
           (get-text-property
            0 'charset (anvil-future-value property)))))
    (let* ((name "anvil-ownership-uninterned-wire-symbol")
           (gensym (anvil-offload `(make-symbol ,name)))
           value)
      (should (anvil-future-await gensym 5))
      (setq value (anvil-future-value gensym))
      (should (symbolp value))
      (should (equal name (symbol-name value)))
      (should-not (intern-soft name anvil-offload--canonical-obarray)))
    (let* ((shared (list 'shared-wire-value))
           (form (list 'list (list 'quote shared) (list 'quote shared)))
           (print-continuous-numbering t)
           (print-number-table nil))
      (dotimes (_ 2)
        (let ((future (anvil-offload form)))
          (should (anvil-future-await future 5))
          (let ((value (anvil-future-value future)))
            (should (eq (car value) (cadr value)))))))
    (let* ((text (propertize "x" 'charset 'latin-iso8859-1))
           (print-charset-text-property nil)
           (future (anvil-offload (list 'identity text))))
      (should (anvil-future-await future 5))
      (should
       (eq 'latin-iso8859-1
           (get-text-property
            0 'charset (anvil-future-value future)))))))

(ert-deftest anvil-offload-ownership-unreadable-request-aborts-cleanly ()
  "An unreadable FORM is rejected before transport and strands no custody."
  (anvil-offload-ownership-test--with-state
    (let ((print-unreadable-function (lambda (&rest _args) "nil")))
      (should-error
       (anvil-offload (list 'identity (current-buffer)))))
    (let ((print-symbols-bare t))
      (should-error
       (anvil-offload
        (list 'identity
              (car (read-positioning-symbols "(wire-position)"))))))
    (should (= 0 (hash-table-count anvil-offload--pending)))
    (let ((future (anvil-offload '(+ 1 2))))
      (should (anvil-future-await future 5))
      (should (= 3 (anvil-future-value future))))))

(ert-deftest anvil-offload-ownership-normalizes-wire-reader-state ()
  "Reader shorthands, circle handling, and obarrays stay protocol-local."
  (anvil-offload-ownership-test--with-state
    (let ((poison
           (anvil-offload
            '(progn
               (setq read-circle nil
                     read-symbol-shorthands '(("wire-" . "poison-"))
                     obarray (make-vector 1511 0))
               'configured))))
      (should (anvil-future-await poison 5))
      (should (eq 'configured (anvil-future-value poison))))
    (let* ((circle (list 'wire-value))
           (_ (setcdr circle circle))
           (form (list 'car (list 'quote circle)))
           (future (anvil-offload form)))
      (should (anvil-future-await future 5))
      (should (eq 'wire-value (anvil-future-value future))))
    (let ((read-symbol-shorthands '(("wire-" . "poison-")))
          (obarray (make-vector 1511 0))
          (future (anvil-offload '(quote wire-value))))
      (should (anvil-future-await future 5))
      (should (eq 'wire-value (anvil-future-value future))))))

(ert-deftest anvil-offload-ownership-normalizes-noninteractive-reader-state ()
  "A child request cannot poison stdin handling for later pooled requests."
  (anvil-offload-ownership-test--with-state
    (let ((anvil-offload-max-frame-bytes 512)
          owner)
      (let ((poison
             (anvil-offload
              '(progn
                 (setq read-hide-char ?*)
                 'hide-char-poisoned))))
        (setq owner (anvil-offload--registered-process poison))
        (should (anvil-future-await poison 5))
        (should (eq 'hide-char-poisoned
                    (anvil-future-value poison))))
      (let ((large
             (anvil-offload
              (list 'progn (make-string 1000 ?x) 3))))
        (should (eq owner (anvil-offload--registered-process large)))
        (should (anvil-future-await large 5))
        (should (= 3 (anvil-future-value large))))
      (let ((poison
             (anvil-offload
              '(progn
                 (setq inhibit-interaction t)
                 (setq executing-kbd-macro t)
                 (setq noninteractive nil)
                 (setq minibuffer-local-map 17)
                 (sleep-for 0.2)
                 'interaction-poisoned))))
        (should (eq owner (anvil-offload--registered-process poison)))
        (should (anvil-future-await poison 5))
        (should (eq 'interaction-poisoned
                    (anvil-future-value poison))))
      (let ((next (anvil-offload '(+ 1 2))))
        (should (eq owner (anvil-offload--registered-process next)))
        (should (anvil-future-await next 5))
        (should (= 3 (anvil-future-value next)))))))

(ert-deftest anvil-offload-ownership-retirement-rebuilds-local-survivors ()
  "A cleanup callback cannot erase locally captured survivor custody."
  (anvil-offload-ownership-test--with-state
    (let* ((stubborn
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-local-survivor"))
           (old (vector stubborn))
           (anvil-offload--pool old)
           (anvil-offload-pool-size 2))
      (cl-letf (((symbol-function 'kill-process)
                 (lambda (_proc)
                   (setq anvil-offload--retired-pools nil)))
                ((symbol-function 'delete-process) #'ignore))
        (should-error (anvil-offload--ensure-pool-vector)))
      (should (eq old anvil-offload--pool))
      (should (memq old anvil-offload--retired-pools))
      (should anvil-offload--pool-retiring-p)
      (should (process-live-p stubborn)))))

(ert-deftest anvil-offload-ownership-spawn-rejects-preexisting-return ()
  "Pooled and isolated spawn never claim or mutate a preexisting peer."
  (anvil-offload-ownership-test--with-state
    (let* ((peer
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-preexisting-spawn-peer"))
           (original-filter (process-filter peer)))
      (dolist (isolated '(nil t))
        (let (condition)
          (cl-letf ((anvil-offload--make-process-primitive
                     (lambda (&rest _args) peer)))
            (setq condition
                  (should-error
                   (anvil-offload--spawn-named-process
                    (format
                     "anvil-ownership-preexisting-return-%s" isolated)
                    isolated))))
          (should
           (string-match-p
            "constructor returned a preexisting process"
            (error-message-string condition)))
          (should (process-live-p peer))
          (should (eq (process-filter peer) original-filter))
          (should-not
           (process-get peer 'anvil-offload-protocol-version))
          (should-not (process-get peer 'anvil-offload-isolated))
          (should-not (process-get peer 'anvil-offload-retiring))
          (should-not (anvil-offload-ownership-test--owned-p peer)))))))

(ert-deftest anvil-offload-ownership-spawn-cleans-duplicate-success ()
  "A valid returned child cannot orphan a matching constructor sibling."
  (anvil-offload-ownership-test--with-state
    (let ((real-make-process anvil-offload--make-process-primitive)
          first second returned)
      (cl-letf
          (((symbol-function 'anvil-offload--process-command)
            (lambda ()
              (list shell-file-name shell-command-switch "exec sleep 30")))
           (anvil-offload--make-process-primitive
            (lambda (&rest args)
              (setq first (apply real-make-process args)
                    second (apply real-make-process args))
              first)))
        (setq returned
              (anvil-offload--spawn-named-process
               "anvil-ownership-duplicate-success")))
      (should (eq returned first))
      (should (process-live-p first))
      (should-not (process-live-p second))
      (should (anvil-offload-ownership-test--owned-p first))
      (should-not (anvil-offload-ownership-test--owned-p second))
      (should
       (= anvil-offload--protocol-version
          (process-get first 'anvil-offload-protocol-version)))
      (anvil-offload--hard-delete-process first)
      (should-not (process-live-p first))
      (should-not (anvil-offload-ownership-test--owned-p first)))))

(ert-deftest anvil-offload-ownership-spawn-cleans-duplicate-throw ()
  "A constructor throw gives custody of every new child before deletion."
  (anvil-offload-ownership-test--with-state
    (let ((real-make-process anvil-offload--make-process-primitive)
          created outcome)
      (cl-letf
          (((symbol-function 'anvil-offload--process-command)
            (lambda ()
              (list shell-file-name shell-command-switch "exec sleep 30")))
           (anvil-offload--make-process-primitive
            (lambda (&rest args)
              (push (apply real-make-process args) created)
              (push (apply real-make-process args) created)
              (throw 'anvil-ownership-spawn-exit :original))))
        (setq outcome
              (catch 'anvil-ownership-spawn-exit
                (anvil-offload--spawn-named-process
                 "anvil-ownership-duplicate-throw")
                :returned)))
      (should (eq outcome :original))
      (should (= 2 (length created)))
      (dolist (proc created)
        (should-not (process-live-p proc))
        (should-not (anvil-offload-ownership-test--owned-p proc))))))

(ert-deftest anvil-offload-ownership-spawn-invalid-return-cleans-delta ()
  "A new unrelated return cannot hide the exact requested child."
  (anvil-offload-ownership-test--with-state
    (let ((real-make-process anvil-offload--make-process-primitive)
          requested helper condition)
      (cl-letf
          (((symbol-function 'anvil-offload--process-command)
            (lambda ()
              (list shell-file-name shell-command-switch "exec sleep 30")))
           (anvil-offload--make-process-primitive
            (lambda (&rest args)
              (setq requested (apply real-make-process args)
                    helper
                    (funcall
                     real-make-process
                     :name "anvil-ownership-unrelated-return"
                     :buffer nil
                     :command
                     (list shell-file-name shell-command-switch
                           "exec sleep 30")
                     :connection-type 'pipe
                     :noquery t
                     :filter #'ignore
                     :sentinel #'ignore))
              helper)))
        (setq condition
              (should-error
               (anvil-offload--spawn-named-process
                "anvil-ownership-invalid-new-return"))))
      (should
       (string-match-p
        "constructor returned an invalid process"
        (error-message-string condition)))
      (dolist (proc (list requested helper))
        (should (processp proc))
        (should-not (process-live-p proc))
        (should-not (anvil-offload-ownership-test--owned-p proc))))))

(ert-deftest
    anvil-offload-ownership-live-hard-delete-keeps-tagged-finalizer ()
  "A tagged scheduler exit cannot recursively delete a live owned child."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-live-sentinel-throw"))
           (id 303)
           (settle-count 0)
           (future
            (anvil-offload-ownership-test--future
             id proc :isolated-p t
             :on-settle
             (lambda (_future)
               (cl-incf settle-count))))
           outcome
           (deadline (+ (float-time) 3)))
      (set-process-sentinel proc #'anvil-offload--sentinel)
      (process-put proc 'anvil-offload-isolated t)
      (anvil-offload--track-process-ownership
       proc t anvil-offload--isolated-processes)
      (puthash id future anvil-offload--pending)
      (let ((anvil-offload--run-at-time-function
             (lambda (&rest _args)
               (throw 'anvil-ownership-finalizer-exit :original))))
        (cl-letf (((symbol-function 'kill-process) #'ignore))
          (setq outcome
                (catch 'anvil-ownership-finalizer-exit
                  (anvil-offload--hard-delete-process proc)
                  :returned))
          (while (and (< (float-time) deadline)
                      (or (memq proc (process-list))
                          (eq 'pending (anvil-future-status future))
                          (anvil-offload-ownership-test--owned-p proc)))
            (accept-process-output nil 0.02))))
      (should (eq outcome :original))
      (should (eq 'error (anvil-future-status future)))
      (should (= 1 settle-count))
      (should-not (gethash id anvil-offload--pending))
      (should-not (memq proc (process-list)))
      (should-not (anvil-offload-ownership-test--owned-p proc)))))

(ert-deftest anvil-offload-ownership-sentinel-exit-always-releases ()
  "Scheduler errors and tagged exits cannot skip sentinel resource cleanup."
  (anvil-offload-ownership-test--with-state
    (dolist (exit-kind '(error throw))
      (let* ((buffer
              (generate-new-buffer
               (format " *anvil-ownership-sentinel-%s*" exit-kind)))
             (proc
              (anvil-offload-ownership-test--idle-process
               (format "anvil-ownership-sentinel-%s" exit-kind)
               buffer))
             outcome)
        (anvil-offload--track-process-ownership
         proc t anvil-offload--isolated-processes)
        (funcall anvil-offload--process-put-primitive
                 proc 'anvil-offload-isolated t)
        (funcall anvil-offload--process-put-primitive
                 proc 'anvil-offload-owned-buffer buffer)
        (set-process-sentinel proc #'ignore)
        (delete-process proc)
        (let ((anvil-offload--run-at-time-function
               (lambda (&rest _args)
                 (pcase exit-kind
                   ('error (error "sentinel timer error"))
                   ('throw
                    (throw
                     'anvil-ownership-sentinel-exit :original))))))
          (pcase exit-kind
            ('error
             (anvil-offload--sentinel proc "finished")
             (setq outcome :returned))
            ('throw
             (setq outcome
                   (catch 'anvil-ownership-sentinel-exit
                     (anvil-offload--sentinel proc "finished")
                     :returned)))))
        (should
         (eq outcome (if (eq exit-kind 'error) :returned :original)))
        (let ((deadline (+ (float-time) 1.0)))
          (while (and
                  (or (anvil-offload-ownership-test--owned-p proc)
                      (buffer-live-p buffer))
                  (< (float-time) deadline))
            (accept-process-output nil 0.01)))
        (should-not (anvil-offload-ownership-test--owned-p proc))
        (should-not (memq proc (process-list)))
        (should-not (buffer-live-p buffer))))))

(ert-deftest anvil-offload-ownership-spawn-preserves-shared-peer-buffer ()
  "Invalid-return cleanup cannot kill a preexisting peer's target buffer."
  (anvil-offload-ownership-test--with-state
    (let* ((name "anvil-ownership-shared-buffer")
           (buffer (get-buffer-create (format " *%s*" name)))
           (peer
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-shared-buffer-peer" buffer))
           (real-make-process anvil-offload--make-process-primitive)
           child condition)
      (cl-letf
          (((symbol-function 'anvil-offload--process-command)
            (lambda ()
              (list shell-file-name shell-command-switch "exec sleep 30")))
           (anvil-offload--make-process-primitive
            (lambda (&rest args)
              (setq child (apply real-make-process args))
              peer)))
        (setq condition
              (should-error
               (anvil-offload--spawn-named-process name))))
      (should
       (string-match-p
        "constructor returned a preexisting process"
        (error-message-string condition)))
      (should (process-live-p peer))
      (should (buffer-live-p buffer))
      (should (eq (process-buffer peer) buffer))
      (should (processp child))
      (should-not (process-live-p child))
      (should-not (anvil-offload-ownership-test--owned-p child))
      (should-not (anvil-offload-ownership-test--owned-p peer)))))

(ert-deftest anvil-offload-ownership-sentinel-nil-scheduler-terminalizes-pending ()
  "A nil scheduler result cannot leave a dead process's future pending."
  (anvil-offload-ownership-test--with-state
    (let* ((buffer
            (generate-new-buffer " *anvil-ownership-sentinel-pending*"))
           (proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-sentinel-pending" buffer))
           (future
            (anvil-offload-ownership-test--future
             201 proc :isolated-p t)))
      (anvil-offload--track-process-ownership
       proc t anvil-offload--isolated-processes)
      (funcall anvil-offload--process-put-primitive
               proc 'anvil-offload-isolated t)
      (funcall anvil-offload--process-put-primitive
               proc 'anvil-offload-owned-buffer buffer)
      (puthash 201 future anvil-offload--pending)
      (set-process-sentinel proc #'ignore)
      (delete-process proc)
      (let ((anvil-offload--run-at-time-function
             (lambda (&rest _args) nil)))
        (anvil-offload--sentinel proc "finished"))
      (let ((deadline (+ (float-time) 1.0)))
        (while (and (eq 'pending (anvil-future-status future))
                    (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (eq 'error (anvil-future-status future)))
      (should-not (gethash 201 anvil-offload--pending))
      (should-not (anvil-offload-ownership-test--owned-p proc))
      (should-not (buffer-live-p buffer)))))

(ert-deftest anvil-offload-ownership-sentinel-owned-skips-property-read ()
  "Registered isolated ownership bypasses the legacy isolated property."
  (anvil-offload-ownership-test--with-state
    (let* ((buffer
            (generate-new-buffer " *anvil-ownership-sentinel-property*"))
           (proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-sentinel-property" buffer))
           (real-process-get anvil-offload--process-get-primitive)
           isolated-property-read outcome)
      (anvil-offload--track-process-ownership
       proc t anvil-offload--isolated-processes)
      (funcall anvil-offload--process-put-primitive
               proc 'anvil-offload-isolated t)
      (funcall anvil-offload--process-put-primitive
               proc 'anvil-offload-owned-buffer buffer)
      (set-process-sentinel proc #'ignore)
      (delete-process proc)
      (let ((anvil-offload--process-get-primitive
             (lambda (process property)
               (if (eq property 'anvil-offload-isolated)
                   (progn
                     (setq isolated-property-read t)
                     (throw 'anvil-ownership-property-exit :unexpected))
                 (funcall real-process-get process property)))))
        (setq outcome
              (catch 'anvil-ownership-property-exit
                (anvil-offload--sentinel proc "finished")
                :returned)))
      (should (eq outcome :returned))
      (should-not isolated-property-read)
      (let ((deadline (+ (float-time) 1.0)))
        (while (and
                (or (anvil-offload-ownership-test--owned-p proc)
                    (buffer-live-p buffer))
                (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should-not (anvil-offload-ownership-test--owned-p proc))
      (should-not (memq proc (process-list)))
      (should-not (buffer-live-p buffer)))))

(ert-deftest anvil-offload-ownership-abort-scheduler-fallback-preserves-exit ()
  "Abort cleanup bypasses a tagged scheduler and preserves transport exit."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--compatible
             (anvil-offload-ownership-test--idle-process
              "anvil-ownership-abort-scheduler")))
           (anvil-offload--pool (vector proc))
           scheduler-called outcome)
      (cl-letf
          (((symbol-function 'anvil-offload--pick-worker)
            (lambda () proc))
           ((symbol-function 'process-send-string)
            (lambda (&rest _args)
              (throw 'anvil-ownership-send-exit :original))))
        (let ((anvil-offload--run-at-time-function
               (lambda (&rest _args)
                 (setq scheduler-called t)
                 (throw 'anvil-ownership-abort-scheduler :replaced))))
          (setq outcome
                (catch 'anvil-ownership-send-exit
                  (anvil-offload '(+ 1 2))
                  :returned))))
      (should (eq outcome :original))
      (should-not scheduler-called)
      (should (= 0 (hash-table-count anvil-offload--pending)))
      (let ((deadline (+ (float-time) 1.0)))
        (while (and (process-live-p proc)
                    (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should-not (process-live-p proc))
      (should-not (anvil-offload-ownership-test--owned-p proc)))))

(ert-deftest anvil-offload-ownership-process-list-advice-cannot-orphan ()
  "Persistent process-list advice cannot interrupt spawn unwind custody."
  (anvil-offload-ownership-test--with-state
    (let ((real-make-process anvil-offload--make-process-primitive)
          created outcome)
      (cl-letf
          (((symbol-function 'anvil-offload--process-command)
            (lambda ()
              (list shell-file-name shell-command-switch "exec sleep 30")))
           ((symbol-function 'process-list)
            (lambda ()
              (error "persistent process-list failure")))
           (anvil-offload--make-process-primitive
            (lambda (&rest args)
              (setq created (apply real-make-process args))
              (throw 'anvil-ownership-process-list-exit :original))))
        (setq outcome
              (catch 'anvil-ownership-process-list-exit
                (anvil-offload--spawn-named-process
                 "anvil-ownership-process-list" t)
                :returned)))
      (should (eq outcome :original))
      (should (processp created))
      (should-not (process-live-p created))
      (should-not (anvil-offload-ownership-test--owned-p created)))))

(ert-deftest anvil-offload-ownership-valid-shared-buffer-preserves-peer ()
  "A valid isolated child never owns a preexisting peer buffer."
  (anvil-offload-ownership-test--with-state
    (let* ((name "anvil-ownership-valid-shared-buffer")
           (buffer (get-buffer-create (format " *%s*" name)))
           (peer
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-valid-shared-peer" buffer))
           child)
      (unwind-protect
          (progn
            (cl-letf
                (((symbol-function 'anvil-offload--process-command)
                  (lambda ()
                    (list shell-file-name shell-command-switch
                          "exec sleep 30"))))
              (setq child
                    (anvil-offload--spawn-named-process name t)))
            (should (process-live-p child))
            (should (process-live-p peer))
            (should
             (eq (funcall anvil-offload--process-buffer-primitive child)
                 buffer))
            (should-not
             (funcall anvil-offload--process-get-primitive
                      child 'anvil-offload-owned-buffer))
            (anvil-offload--hard-delete-process child)
            (should-not (process-live-p child))
            (should (process-live-p peer))
            (should (buffer-live-p buffer))
            (should (eq (process-buffer peer) buffer)))
        (when (and (processp child) (process-live-p child))
          (anvil-offload--hard-delete-process child))
        (when (and (processp peer) (process-live-p peer))
          (set-process-sentinel peer #'ignore)
          (delete-process peer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest anvil-offload-ownership-constructor-advice-cannot-yield-output ()
  "Runtime constructor advice cannot rewrite a child before custody exists."
  (anvil-offload-ownership-test--with-state
    (let* ((name "anvil-ownership-advised-constructor")
           (real-symbol-make-process (symbol-function 'make-process))
           (foreign-buffer
            (generate-new-buffer " *anvil-ownership-advised-foreign*"))
           (canary
            (format "anvil-offload-yield-canary-%s"
                    (random most-positive-fixnum)))
           advice-called created seen child)
      (unwind-protect
          (cl-letf
              (((symbol-function 'anvil-offload--process-command)
                (lambda ()
                  (list shell-file-name shell-command-switch
                        "exec sleep 30")))
               ((symbol-function 'make-process)
                (lambda (&rest args)
                  (setq advice-called t)
                  (setq args (plist-put args :buffer foreign-buffer)
                        args
                        (plist-put
                         args :filter
                         (lambda (_process chunk)
                           (setq seen (concat seen chunk))))
                        args
                        (plist-put
                         args :command
                         (list shell-file-name shell-command-switch
                               (format "printf %s; sleep 30"
                                       (shell-quote-argument canary)))))
                  (setq created (apply real-symbol-make-process args))
                  (accept-process-output created 0.2)
                  created)))
            (setq child
                  (anvil-offload--spawn-named-process name t))
            (should-not advice-called)
            (should-not seen)
            (should
             (eq (funcall anvil-offload--process-filter-primitive child)
                 #'anvil-offload--filter))
            (should-not
             (eq (funcall anvil-offload--process-buffer-primitive child)
                 foreign-buffer))
            (should (string-empty-p
                     (with-current-buffer foreign-buffer
                       (buffer-string))))
            (anvil-offload--hard-delete-process child)
            (should-not (process-live-p child)))
        (when (and (processp created) (process-live-p created))
          (delete-process created))
        (when (and (processp child) (process-live-p child))
          (anvil-offload--hard-delete-process child))
        (when (buffer-live-p foreign-buffer)
          (kill-buffer foreign-buffer))))))

(ert-deftest anvil-offload-ownership-rejects-rewritten-transport ()
  "A returned child with a foreign filter or buffer is invalid and retired."
  (anvil-offload-ownership-test--with-state
    (let* ((name "anvil-ownership-rewritten-transport")
           (real-make-process anvil-offload--make-process-primitive)
           (foreign-buffer
            (generate-new-buffer " *anvil-ownership-rewritten-foreign*"))
           created condition)
      (unwind-protect
          (cl-letf
              (((symbol-function 'anvil-offload--process-command)
                (lambda ()
                  (list shell-file-name shell-command-switch
                        "exec sleep 30")))
               (anvil-offload--make-process-primitive
                (lambda (&rest args)
                  (setq created (apply real-make-process args))
                  (set-process-buffer created foreign-buffer)
                  (set-process-filter created #'ignore)
                  created)))
            (setq condition
                  (should-error
                   (anvil-offload--spawn-named-process name t)))
            (should
             (string-match-p
              "constructor returned an invalid process"
              (error-message-string condition)))
            (should (processp created))
            (should-not (process-live-p created))
            (should-not
             (anvil-offload-ownership-test--owned-p created))
            (should (buffer-live-p foreign-buffer)))
        (when (and (processp created) (process-live-p created))
          (delete-process created))
        (when (buffer-live-p foreign-buffer)
          (kill-buffer foreign-buffer))))))

(ert-deftest anvil-offload-ownership-custody-bypasses-get-and-remove-advice ()
  "Property and hash advice cannot replace a constructor's original exit."
  (anvil-offload-ownership-test--with-state
    (let ((real-make-process anvil-offload--make-process-primitive)
          created process-get-called remhash-called outcome)
      (cl-letf
          (((symbol-function 'anvil-offload--process-command)
            (lambda ()
              (list shell-file-name shell-command-switch
                    "exec sleep 30")))
           (anvil-offload--make-process-primitive
            (lambda (&rest args)
              (setq created (apply real-make-process args))
              (throw 'anvil-ownership-constructor-exit :original)))
           ((symbol-function 'process-get)
            (lambda (&rest _args)
              (setq process-get-called t)
              (throw 'anvil-ownership-process-get-exit :replaced-get)))
           ((symbol-function 'remhash)
            (lambda (&rest _args)
              (setq remhash-called t)
              (throw 'anvil-ownership-remhash-exit :replaced-remove))))
        (setq outcome
              (catch 'anvil-ownership-constructor-exit
                (catch 'anvil-ownership-process-get-exit
                  (catch 'anvil-ownership-remhash-exit
                    (anvil-offload--spawn-named-process
                     "anvil-ownership-custody-advice" t)
                    :returned)))))
      (should (eq outcome :original))
      (should-not process-get-called)
      (should-not remhash-called)
      (should (processp created))
      (should-not (process-live-p created))
      (should-not (anvil-offload-ownership-test--owned-p created)))))

(ert-deftest anvil-offload-ownership-nil-scheduler-replays-later-frame ()
  "A nil timer result cannot strand a later coalesced reply."
  (anvil-offload-ownership-test--with-state
    (let* ((proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-nil-scheduler"))
           (first-count 0)
           (second-count 0)
           (scheduler-calls 0)
           (first
            (make-anvil-future
             :id 211 :process proc :status 'pending
             :on-settle
             (lambda (_future)
               (cl-incf first-count)
               (throw 'anvil-ownership-nil-frame-exit :original))))
           (second
            (make-anvil-future
             :id 212 :process proc :status 'pending
             :on-settle
             (lambda (_future) (cl-incf second-count))))
           (second-frame
            (anvil-offload-ownership-test--frame
             '(:id 212 :ok second)))
           (chunk
            (concat
             (anvil-offload-ownership-test--frame
              '(:id 211 :ok first))
             second-frame)))
      (puthash 211 first anvil-offload--pending)
      (puthash 212 second anvil-offload--pending)
      (let ((anvil-offload--run-at-time-function
             (lambda (&rest _args)
               (cl-incf scheduler-calls)
               nil)))
        (should
         (eq :original
             (catch 'anvil-ownership-nil-frame-exit
               (anvil-offload--filter proc chunk)
               :returned))))
      (should (= 1 scheduler-calls))
      (should (eq 'done (anvil-future-status first)))
      (should (eq 'pending (anvil-future-status second)))
      (should
       (equal
        second-frame
        (anvil-offload--pending-bytes-string proc)))
      (let ((deadline (+ (float-time) 1.0)))
        (while (and (eq 'pending (anvil-future-status second))
                    (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (eq 'done (anvil-future-status second)))
      (should (eq 'second (anvil-future-value second)))
      (should (= 1 first-count))
      (should (= 1 second-count))
      (should (= 0 (hash-table-count anvil-offload--pending)))
      (should
       (string-empty-p
        (or
         (anvil-offload--pending-bytes-string proc)
         ""))))))

(ert-deftest anvil-offload-ownership-sentinel-preserves-foreign-timer ()
  "A preexisting foreign timer cannot strand dead-child finalization."
  (anvil-offload-ownership-test--with-state
    (let* ((peer
            (run-at-time 30 30 #'ignore 'peer-argument))
           (peer-function (timer--function peer))
           (peer-repeat (timer--repeat-delay peer))
           (peer-args (copy-tree (timer--args peer)))
           (buffer
            (generate-new-buffer " *anvil-ownership-sentinel-peer-timer*"))
           (proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-sentinel-peer-timer" buffer))
           (future
            (anvil-offload-ownership-test--future
             221 proc :isolated-p t)))
      (unwind-protect
          (progn
            (anvil-offload--track-process-ownership
             proc t anvil-offload--isolated-processes)
            (funcall anvil-offload--process-put-primitive
                     proc 'anvil-offload-isolated t)
            (funcall anvil-offload--process-put-primitive
                     proc 'anvil-offload-owned-buffer buffer)
            (puthash 221 future anvil-offload--pending)
            (set-process-sentinel proc #'ignore)
            (delete-process proc)
            (let ((anvil-offload--run-at-time-function
                   (lambda (&rest _args) peer)))
              (anvil-offload--sentinel proc "finished"))
            (let ((deadline (+ (float-time) 1.0)))
              (while (and (eq 'pending (anvil-future-status future))
                          (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (should (eq 'error (anvil-future-status future)))
            (should-not (gethash 221 anvil-offload--pending))
            (should (memq peer timer-list))
            (should (eq (timer--function peer) peer-function))
            (should (equal (timer--repeat-delay peer) peer-repeat))
            (should (equal (timer--args peer) peer-args))
            (should-not (buffer-live-p buffer)))
        (when (and (timerp peer)
                   (or (memq peer timer-list)
                       (memq peer timer-idle-list)))
          (cancel-timer peer))))))

(ert-deftest anvil-offload-ownership-filter-preserves-foreign-timer ()
  "A preexisting foreign timer cannot strand a coalesced reply."
  (anvil-offload-ownership-test--with-state
    (let* ((peer
            (run-at-time 30 30 #'ignore 'peer-argument))
           (peer-function (timer--function peer))
           (peer-repeat (timer--repeat-delay peer))
           (peer-args (copy-tree (timer--args peer)))
           (proc
            (anvil-offload-ownership-test--idle-process
             "anvil-ownership-filter-peer-timer"))
           (first
            (make-anvil-future
             :id 222 :process proc :status 'pending
             :on-settle
             (lambda (_future)
               (throw 'anvil-ownership-peer-timer-exit :original))))
           (second
            (make-anvil-future
             :id 223 :process proc :status 'pending))
           (second-frame
            (anvil-offload-ownership-test--frame
             '(:id 223 :ok second)))
           (chunk
            (concat
             (anvil-offload-ownership-test--frame
              '(:id 222 :ok first))
             second-frame)))
      (unwind-protect
          (progn
            (puthash 222 first anvil-offload--pending)
            (puthash 223 second anvil-offload--pending)
            (let ((anvil-offload--run-at-time-function
                   (lambda (&rest _args) peer)))
              (should
               (eq :original
                   (catch 'anvil-ownership-peer-timer-exit
                     (anvil-offload--filter proc chunk)
                     :returned))))
            (let ((deadline (+ (float-time) 1.0)))
              (while (and (eq 'pending (anvil-future-status second))
                          (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (should (eq 'done (anvil-future-status second)))
            (should (eq 'second (anvil-future-value second)))
            (should (= 0 (hash-table-count anvil-offload--pending)))
            (should (memq peer timer-list))
            (should (eq (timer--function peer) peer-function))
            (should (equal (timer--repeat-delay peer) peer-repeat))
            (should (equal (timer--args peer) peer-args)))
        (when (and (timerp peer)
                   (or (memq peer timer-list)
                       (memq peer timer-idle-list)))
          (cancel-timer peer))))))

(ert-deftest anvil-offload-ownership-scheduler-collapses-exact-duplicates ()
  "One exact callback timer survives while foreign helpers stay unchanged."
  (let* ((real-run-at-time (symbol-function 'run-at-time))
         (callback-count 0)
         (callback (lambda (_argument) (cl-incf callback-count)))
         first second helper selected
         helper-function helper-repeat helper-args)
    (unwind-protect
        (let ((anvil-offload--run-at-time-function
               (lambda (_time _repeat function &rest args)
                 (setq first
                       (apply real-run-at-time 30 30 function args)
                       second
                       (apply real-run-at-time 30 30 function args)
                       helper
                       (funcall real-run-at-time
                                30 30 #'ignore 'foreign-helper)
                       helper-function (timer--function helper)
                       helper-repeat (timer--repeat-delay helper)
                       helper-args (copy-tree (timer--args helper)))
                 first)))
          (setq selected
                (anvil-offload--schedule-zero-delay
                 callback 'exact-argument))
          (should (eq selected first))
          (should (anvil-offload--timer-scheduled-p first))
          (should-not (timer--repeat-delay first))
          (should-not (anvil-offload--timer-scheduled-p second))
          (should (eq (timer--function first) callback))
          (should (equal (timer--args first) '(exact-argument)))
          (should (anvil-offload--timer-scheduled-p helper))
          (should (eq (timer--function helper) helper-function))
          (should (equal (timer--repeat-delay helper) helper-repeat))
          (should (equal (timer--args helper) helper-args))
          (let ((deadline (+ (float-time) 1.0)))
            (while (and (zerop callback-count)
                        (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (should (= 1 callback-count))
          (accept-process-output nil 0.05)
          (should (= 1 callback-count))
          (should (anvil-offload--timer-scheduled-p helper))
          (should (eq (timer--function helper) helper-function))
          (should (equal (timer--repeat-delay helper) helper-repeat))
          (should (equal (timer--args helper) helper-args)))
      (dolist (timer (list selected first second helper))
        (when (anvil-offload--timer-scheduled-p timer)
          (cancel-timer timer))))))

(ert-deftest anvil-offload-ownership-capture-strips-preexisting-advice ()
  "Primitive capture unwraps advice already installed on the target symbol."
  (let ((wrapper
         (lambda (original &rest args)
           (apply original args))))
    (advice-add 'make-process :around wrapper)
    (unwind-protect
        (should
         (eq (anvil-offload--capture-primitive 'make-process)
             anvil-offload--make-process-primitive))
      (advice-remove 'make-process wrapper))))

(ert-deftest anvil-offload-ownership-zz-global-state-remains-clean ()
  "Focused and full runs must leave no hidden global ownership state."
  (should-not anvil-offload--pool)
  (should-not anvil-offload--pool-retiring-p)
  (should-not (anvil-offload--cleanup-active-p))
  (should-not (anvil-offload--submission-active-p))
  (should-not anvil-offload--pool-cleanup-active-p)
  (should-not anvil-offload--submission-active-p)
  (should-not anvil-offload--stop-retiring-p)
  (should-not anvil-offload--retired-pools)
  (dolist (table (anvil-offload--registered-ownership-tables))
    (should (= 0 (hash-table-count table))))
  (when (hash-table-p anvil-offload--pending)
    (should (= 0 (hash-table-count anvil-offload--pending)))))

(provide 'anvil-offload-ownership-test)
;;; anvil-offload-ownership-test.el ends here
