;;; anvil-host-reentrancy-test.el --- Host transaction regressions -*- lexical-binding: t; -*-

;;; Commentary:

;; Regressions for issue #53 and the host subprocess transaction.  These tests
;; exercise real children and make callback ordering deterministic with files.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'anvil-host)

(defun anvil-host-reentrancy-test--result (exit stdout stderr)
  "Return the seven-field host transaction result for expected output."
  (list exit stdout stderr
        (string-bytes stdout) (string-bytes stderr)
        (encode-coding-string stdout 'utf-8)
        (encode-coding-string stderr 'utf-8)))

(defun anvil-host-reentrancy-test--force-clean (processes buffers)
  "Converge PROCESSES, BUFFERS, and dynamically bound host state."
  (dolist (process processes)
    (when (and (processp process) (process-live-p process))
      (ignore-errors (delete-process process))))
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (ignore-errors (kill-buffer buffer))))
  (ignore-errors (anvil-host--retry-retired-processes))
  (anvil-host-reentrancy-test--wait-for-cleanup 1)
  (when (or (> (anvil-host--retired-count) 0)
            (anvil-host--cleanup-active-p)
            (aref (anvil-host--resource-state) 3))
    (error "host test cleanup did not converge"))
  (ignore-errors (anvil-host--cancel-cleanup-timer))
  (when (anvil-host--cleanup-timer-scheduled-p)
    (error "host test cleanup timer remains scheduled"))
  (setcdr anvil-host--cleanup-state nil)
  (setcar anvil-host--cleanup-state nil)
  (setq anvil-host--cleanup-active nil))

(defun anvil-host-reentrancy-test--assert-clean (processes buffers)
  "Assert exact PROCESSES, BUFFERS, and global host custody are clean."
  (dolist (process processes)
    (should (processp process))
    (should-not (process-live-p process)))
  (dolist (buffer buffers)
    (should-not (buffer-live-p buffer)))
  (should (zerop (anvil-host--retired-count)))
  (should-not (anvil-host--cleanup-active-p))
  (should-not (aref (anvil-host--resource-state) 2))
  (should-not (aref (anvil-host--resource-state) 3))
  (should-not anvil-host--cleanup-active)
  (should-not (timerp anvil-host--cleanup-timer))
  (should-not (anvil-host--cleanup-timer-scheduled-p)))

(defun anvil-host-reentrancy-test--wait-for-cleanup (seconds)
  "Service timers for at most SECONDS while host custody is nonempty."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (or (> (anvil-host--retired-count) 0)
                    (anvil-host--cleanup-timer-scheduled-p))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))))

(ert-deftest anvil-host-reentrancy-callback-sees-root-context ()
  "A callback sees root state while the child gets its private state."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((project (make-temp-file "anvil-host-project-" t))
         (started (expand-file-name "started" project))
         (baseline-directory default-directory)
         (baseline-coding (copy-tree process-coding-system-alist))
         (child-environment
          (cons "ANVIL_HOST_PRIVATE_MARKER=secret"
                (copy-sequence process-environment)))
         (anvil-host--cleanup-state (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         timer observed)
    (unwind-protect
        (progn
          (setq timer
                (run-at-time
                 0.01 0.01
                 (lambda ()
                   (when (file-exists-p started)
                     (setq observed
                           (list default-directory
                                 anvil-host-child-process-environment
                                 anvil-host-child-exec-path
                                 anvil-host-child-shell-file-name
                                 anvil-host-child-shell-command-switch
                                 (getenv "ANVIL_HOST_PRIVATE_MARKER")
                                 (copy-tree process-coding-system-alist)))
                     (when (timerp timer)
                       (cancel-timer timer)
                       (setq timer nil))))))
          (let ((anvil-host-child-process-environment child-environment)
                (anvil-host-child-exec-path (copy-sequence exec-path))
                (anvil-host-child-shell-file-name shell-file-name)
                (anvil-host-child-shell-command-switch shell-command-switch))
            (should
             (equal
              (anvil-host-reentrancy-test--result 0
                    (format "%s\nsecret\ncafé"
                            (directory-file-name (file-truename project)))
                    "")
              (anvil-host--run
               (format
                (concat ": > %s; sleep 0.20; "
                        "printf '%%s\\n%%s\\ncafé' \"$(pwd -P)\" "
                        "\"$ANVIL_HOST_PRIVATE_MARKER\"")
                (shell-quote-argument started))
               'utf-8-unix project 2))))
          (should observed)
          (should (equal baseline-directory (nth 0 observed)))
          (should-not (nth 1 observed))
          (should-not (nth 2 observed))
          (should-not (nth 3 observed))
          (should-not (nth 4 observed))
          (should-not (nth 5 observed))
          (should (equal baseline-coding (nth 6 observed))))
      (when (timerp timer)
        (cancel-timer timer))
      (anvil-host-reentrancy-test--force-clean nil nil)
      (delete-directory project t))))

(ert-deftest anvil-host-reentrancy-foreign-filter-releases-child ()
  "Foreign process filters remain live while a host child is running."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((directory (make-temp-file "anvil-host-foreign-filter-" t))
         (started (expand-file-name "started" directory))
         (release (expand-file-name "release" directory))
         (baseline-directory default-directory)
         (child-environment
          (cons "ANVIL_HOST_PRIVATE_MARKER=secret"
                (copy-sequence process-environment)))
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         helper timer observed)
    (unwind-protect
        (progn
          (should (zerop (call-process "mkfifo" nil nil nil release)))
          ;; Spawn before the host transaction so the helper is foreign to its
          ;; identity snapshot.  A timer releases its blocking pipe only after
          ;; the owned child has opened the FIFO, without polling in a shell.
          (setq helper
                (make-process
                 :name "anvil-host-foreign-release-helper"
                 :buffer nil
                 :command
                 (list shell-file-name shell-command-switch
                       (concat "IFS= read -r ignored || exit; printf x; "
                               "IFS= read -r -t 2 ignored"))
                 :connection-type 'pipe
                 :coding 'binary
                 :noquery t
                 :sentinel #'ignore
                 :filter
                 (lambda (_process _chunk)
                   (setq observed
                         (list
                          default-directory
                          anvil-host-child-process-environment
                          anvil-host-child-exec-path
                          anvil-host-child-shell-file-name
                          anvil-host-child-shell-command-switch
                          (getenv "ANVIL_HOST_PRIVATE_MARKER")))
                   (write-region "\n" nil release nil 'silent))))
          (setq timer
                (run-at-time
                 0.05 0.05
                 (lambda ()
                   (when (and (file-exists-p started)
                              (processp helper)
                              (process-live-p helper))
                     (process-send-string helper "\n")
                     (cancel-timer timer)
                     (setq timer nil)))))
          (let ((anvil-host-child-process-environment child-environment)
                (anvil-host-child-exec-path (copy-sequence exec-path))
                (anvil-host-child-shell-file-name shell-file-name)
                (anvil-host-child-shell-command-switch shell-command-switch))
            (should
             (equal
              (anvil-host-reentrancy-test--result 0 "secret" "")
              (anvil-host--run
               (format
                (concat "exec 9<> %s; : > %s; "
                        "IFS= read -r -t 2 ignored <&9 || exit 99; "
                        "printf '%%s' \"$ANVIL_HOST_PRIVATE_MARKER\"")
                (shell-quote-argument release)
                (shell-quote-argument started))
               'utf-8-unix directory 3))))
          (should
           (equal (list baseline-directory nil nil nil nil nil) observed))
          (should (file-exists-p started))
          ;; The owned transaction must not sweep its preexisting helper.
          (should (process-live-p helper))
          (anvil-host-reentrancy-test--assert-clean nil nil))
      (when (timerp timer)
        (cancel-timer timer))
      (when (and (processp helper) (process-live-p helper))
        (delete-process helper))
      (anvil-host-reentrancy-test--force-clean nil nil)
      (delete-directory directory t))))

(ert-deftest anvil-host-reentrancy-output-is-not-buffer-visible ()
  "Callbacks cannot read filtered stdout or stderr from Emacs buffers."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((directory (make-temp-file "anvil-host-output-" t))
         (filter-seen (expand-file-name "filter-seen" directory))
         (release (expand-file-name "release" directory))
         (stdout-canary
          (format "anvil-private-out-%s" (random most-positive-fixnum)))
         (stderr-canary
          (format "anvil-private-err-%s" (random most-positive-fixnum)))
         (real-make-process anvil-host--make-process-primitive)
         (real-make-pipe-process anvil-host--make-pipe-process-primitive)
         (anvil-host--cleanup-state (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         processes timer scanned exposed post-exposed)
    (unwind-protect
        (cl-labels
            ((visible-canary-buffer
              ()
              (seq-find
               (lambda (buffer)
                 (with-current-buffer buffer
                   (save-restriction
                     (widen)
                     (or (string-match-p
                          (regexp-quote stdout-canary)
                          (buffer-string))
                         (string-match-p
                          (regexp-quote stderr-canary)
                          (buffer-string))))))
               (buffer-list))))
          (should (zerop (call-process "mkfifo" nil nil nil release)))
          (setq timer
                (run-at-time
                 0.05 0.05
                 (lambda ()
                   (when (file-exists-p filter-seen)
                     (setq scanned t
                           exposed (visible-canary-buffer))
                     (write-region "\n" nil release nil 'silent)
                     (when (timerp timer)
                       (cancel-timer timer)
                       (setq timer nil))))))
          (cl-letf
              ((anvil-host--make-pipe-process-primitive
                (lambda (&rest args)
                  (let ((process (apply real-make-pipe-process args)))
                    (push process processes)
                    process)))
               (anvil-host--make-process-primitive
                (lambda (&rest args)
                  (let* ((name (plist-get args :name))
                         (process (apply real-make-process args)))
                    (when (and (stringp name)
                               (string-prefix-p "anvil-host-shell-" name))
                      (push process processes))
                    process))))
            (let* ((command
                    (format
                     (concat "exec 9<> %s; "
                             "printf %s; printf %s >&2; sleep 0.1; "
                             ": > %s; "
                             "IFS= read -r -t 4 ignored <&9 || exit 99")
                     (shell-quote-argument release)
                     (shell-quote-argument stdout-canary)
                     (shell-quote-argument stderr-canary)
                     (shell-quote-argument filter-seen)))
                   (result
                    (anvil-host--run command 'utf-8-unix directory 6)))
              (should
               (equal (anvil-host-reentrancy-test--result 0 stdout-canary stderr-canary) result))))
          (setq post-exposed (visible-canary-buffer))
          (should scanned)
          (should-not exposed)
          (should-not post-exposed)
          (anvil-host-reentrancy-test--assert-clean processes nil))
      (when (timerp timer)
        (cancel-timer timer))
      (anvil-host-reentrancy-test--force-clean processes nil)
      (delete-directory directory t))))


(ert-deftest anvil-host-reentrancy-spawn-nonlocal-exits-kill-exact-child ()
  "Every constructor exit kills its detached child and preserves a peer."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let ((case-count 0))
    (dolist (spawn-kind '(stderr main))
      (dolist (exit-kind '(error quit throw))
        (let* ((real-make-process anvil-host--make-process-primitive)
               (real-make-pipe-process
                anvil-host--make-pipe-process-primitive)
               (peer
                (make-process
                 :name (symbol-name (gensym "anvil-host-peer-"))
                 :buffer nil
                 :command (list shell-file-name shell-command-switch
                                "exec sleep 30")
                 :connection-type 'pipe
                 :noquery t
                 :sentinel #'ignore))
               (anvil-host--cleanup-state
                (cons nil (make-hash-table :test #'eq)))
               (anvil-host--cleanup-timer nil)
               (anvil-host--cleanup-active nil)
               (anvil-host-cleanup-timeout 0.2)
               processes condition exited)
          (unwind-protect
              (cl-labels
                  ((exit-now
                    ()
                    (pcase exit-kind
                      ('error (error "original spawn error"))
                      ('quit (signal 'quit nil))
                      ('throw
                       (throw 'anvil-host-spawn-exit :original)))))
                (cl-letf
                    ((anvil-host--make-pipe-process-primitive
                      (lambda (&rest args)
                        (let ((process
                               (apply real-make-pipe-process args)))
                          (push process processes)
                          (set-process-buffer process nil)
                          (when (eq spawn-kind 'stderr)
                            (exit-now))
                          process)))
                     (anvil-host--make-process-primitive
                      (lambda (&rest args)
                        (let ((process (apply real-make-process args)))
                          (push process processes)
                          (set-process-buffer process nil)
                          (when (eq spawn-kind 'main)
                            (exit-now))
                          process))))
                  (pcase exit-kind
                    ('error
                     (setq condition
                           (should-error
                            (anvil-host--run
                             "exec sleep 30" 'utf-8-unix nil 2)))
                     (should
                      (string-match-p
                       "original spawn error"
                       (error-message-string condition))))
                    ('quit
                     (setq condition
                           (condition-case caught
                               (progn
                                 (anvil-host--run
                                  "exec sleep 30" 'utf-8-unix nil 2)
                                 (ert-fail "expected constructor quit"))
                             (quit caught)))
                     (should (eq (car-safe condition) 'quit)))
                    ('throw
                     (setq exited
                           (catch 'anvil-host-spawn-exit
                             (anvil-host--run
                              "exec sleep 30" 'utf-8-unix nil 2)
                             :returned))
                     (should (eq exited :original))))
                  (should (= (length processes)
                             (if (eq spawn-kind 'stderr) 1 2)))
                  (should (process-live-p peer))
                  (anvil-host-reentrancy-test--assert-clean
                   processes nil)
                  (setq case-count (1+ case-count))))
            (when (and (processp peer) (process-live-p peer))
              (delete-process peer))
            (anvil-host-reentrancy-test--force-clean
             processes nil)))))
    (should (= case-count 6))))

(ert-deftest anvil-host-reentrancy-failed-cleanup-timer-converges ()
  "A failed first cleanup retains all custody until its timer converges."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let ((real-make-process anvil-host--make-process-primitive)
        (real-make-pipe-process anvil-host--make-pipe-process-primitive)
        (anvil-host--cleanup-state
         (cons nil (make-hash-table :test #'eq)))
        (anvil-host--cleanup-timer nil)
        (anvil-host--cleanup-active nil)
        (anvil-host-cleanup-timeout 0.01)
        processes)
    (unwind-protect
        (progn
          (cl-letf
              ((anvil-host--make-pipe-process-primitive
                (lambda (&rest args)
                  (let ((process (apply real-make-pipe-process args)))
                    (push process processes)
                    process)))
               (anvil-host--make-process-primitive
                (lambda (&rest args)
                  (let ((process (apply real-make-process args)))
                    (push process processes)
                    process)))
               ((symbol-function 'kill-process)
                (lambda (&rest _args) nil))
               ((symbol-function 'delete-process)
                (lambda (&rest _args) nil)))
            (should-error
             (anvil-host--run
              "exec sleep 30" 'utf-8-unix nil 0.01))
            (should (= (length processes) 2))
            (dolist (process processes)
              (should (process-live-p process)))
            (should (= 1 (anvil-host--retired-count)))
            (should (timerp anvil-host--cleanup-timer)))
          ;; The mocks are gone: prove the real one-shot retry, rather than a
          ;; direct helper call, kills both exact processes and releases state.
          (anvil-host-reentrancy-test--wait-for-cleanup 3)
          (anvil-host-reentrancy-test--assert-clean processes nil))
      (anvil-host-reentrancy-test--force-clean processes nil))))

(ert-deftest anvil-host-reentrancy-cleanup-is-not-recursive ()
  "A cleanup callback cannot recursively sweep the retirement table."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((custody (generate-new-buffer " *anvil-host-test-custody*"))
         (context-directory (make-temp-file "anvil-host-context-" t))
         (child
          (make-process
           :name "anvil-host-retry-child"
           :buffer custody
           :command (list shell-file-name shell-command-switch
                          "exec sleep 30")
           :connection-type 'pipe
           :noquery t
           :sentinel #'ignore))
         (metadata (list :process child
                         :stderr-process nil
                         :custody-buffer custody
                         :stderr-custody-buffer nil))
         (real-terminate
          (symbol-function 'anvil-host--terminate-exact-process))
         (anvil-host--cleanup-state (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         (depth 0) (max-depth 0) (calls 0) observed-directory)
    (unwind-protect
        (progn
          (with-current-buffer custody
            (setq-local default-directory context-directory))
          (puthash child metadata (anvil-host--retired-table))
          (cl-letf
              (((symbol-function 'anvil-host--terminate-exact-process)
                (lambda (process)
                  (setq calls (1+ calls)
                        depth (1+ depth)
                        max-depth (max max-depth depth)
                        observed-directory default-directory)
                  (unwind-protect
                      (progn
                        (anvil-host--retry-retired-processes)
                        (funcall real-terminate process))
                    (setq depth (1- depth))))))
            (anvil-host--retry-retired-processes))
          (should (= calls 1))
          (should (= max-depth 1))
          (should (equal observed-directory context-directory))
          (anvil-host-reentrancy-test--assert-clean
           (list child) (list custody)))
      (anvil-host-reentrancy-test--force-clean
       (list child) (list custody))
      (delete-directory context-directory t))))

(ert-deftest anvil-host-reentrancy-tagged-cleanup-retains-custody ()
  "A tagged cleanup exit still publishes custody and schedules retry."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((custody (generate-new-buffer " *anvil-host-tagged-custody*"))
         (child
          (make-process
           :name "anvil-host-tagged-child"
           :buffer custody
           :command (list shell-file-name shell-command-switch
                          "exec sleep 30")
           :connection-type 'pipe
           :noquery t
           :sentinel #'ignore))
         (anvil-host--cleanup-state (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         exited)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'kill-process)
                     (lambda (&rest _args)
                       (throw 'anvil-host-cleanup-exit :original))))
            (setq exited
                  (catch 'anvil-host-cleanup-exit
                    (anvil-host--retire-process child nil custody nil)
                    :returned)))
          (should (eq exited :original))
          (should (process-live-p child))
          (should (= 1 (anvil-host--retired-count)))
          (should (buffer-live-p custody))
          (should (timerp anvil-host--cleanup-timer))
          (anvil-host--retry-retired-processes)
          (anvil-host-reentrancy-test--assert-clean
           (list child) (list custody)))
      (anvil-host-reentrancy-test--force-clean
       (list child) (list custody)))))

(ert-deftest anvil-host-reentrancy-release-failure-retains-custody ()
  "A resource callback failure cannot remove the ownership entry early."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((custody (generate-new-buffer " *anvil-host-release-custody*"))
         (child
          (make-process
           :name "anvil-host-release-child"
           :buffer custody
           :command (list shell-file-name shell-command-switch "exit 0")
           :connection-type 'pipe
           :noquery t
           :sentinel #'ignore))
         (metadata (list :process child
                         :stderr-process nil
                         :custody-buffer custody
                         :stderr-custody-buffer nil))
         (real-kill-buffer (symbol-function 'kill-buffer))
         (anvil-host--cleanup-state (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil))
    (unwind-protect
        (progn
          (while (process-live-p child)
            (accept-process-output child 0.05))
          (puthash child metadata (anvil-host--retired-table))
          (cl-letf (((symbol-function 'kill-buffer)
                     (lambda (buffer-or-name)
                       (if (eq (get-buffer buffer-or-name) custody)
                           (error "injected release failure")
                         (funcall real-kill-buffer buffer-or-name)))))
            (should-not
             (anvil-host--release-process-resources child metadata)))
          (should (eq (gethash child (anvil-host--retired-table))
                      metadata))
          (should (buffer-live-p custody))
          (anvil-host--retry-retired-processes)
          (anvil-host-reentrancy-test--assert-clean
           (list child) (list custody)))
      (anvil-host-reentrancy-test--force-clean
       (list child) (list custody)))))


(ert-deftest anvil-host-reentrancy-initial-retire-guards-buffer-hooks ()
  "A buffer hook cannot enter a nested sweep during initial retirement."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((custody (generate-new-buffer " *anvil-host-hook-custody*"))
         (child
          (make-process
           :name "anvil-host-hook-child"
           :buffer custody
           :command (list shell-file-name shell-command-switch "exit 0")
           :connection-type 'pipe
           :noquery t
           :sentinel #'ignore))
         (anvil-host--cleanup-state (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         (calls 0)
         seen-active)
    (unwind-protect
        (progn
          (while (process-live-p child)
            (accept-process-output child 0.05))
          (with-current-buffer custody
            (add-hook
             'kill-buffer-hook
             (lambda ()
               (setq calls (1+ calls)
                     seen-active
                     (cons anvil-host--cleanup-active seen-active))
               (anvil-host--retry-retired-processes))
             nil t))
          (anvil-host--retire-process child nil custody nil)
          (should (= calls 1))
          (should (equal seen-active '(t)))
          (anvil-host-reentrancy-test--assert-clean
           (list child) (list custody)))
      (anvil-host-reentrancy-test--force-clean
       (list child) (list custody)))))

(ert-deftest anvil-host-reentrancy-buffer-only-failure-retains-custody ()
  "Buffer-only error, quit, and throw cleanup remains timer-owned."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let ((case-count 0))
    (dolist (exit-kind '(error quit throw))
      (let ((custody
             (generate-new-buffer " *anvil-host-buffer-only-custody*"))
            (anvil-host--cleanup-state
             (cons nil (make-hash-table :test #'eq)))
            (anvil-host--cleanup-timer nil)
            (anvil-host--cleanup-active nil)
            exited)
        (unwind-protect
            (progn
              (cl-letf
                  (((symbol-function 'kill-buffer)
                    (lambda (&rest _args)
                      (pcase exit-kind
                        ('error (error "buffer cleanup error"))
                        ('quit (signal 'quit nil))
                        ('throw
                         (throw 'anvil-host-buffer-cleanup :original))))))
                (setq exited
                      (catch 'anvil-host-buffer-cleanup
                        (anvil-host--retire-process
                         nil nil custody nil)
                        :returned)))
              (if (eq exit-kind 'throw)
                  (should (eq exited :original))
                (should (eq exited :returned)))
              (should (buffer-live-p custody))
              (should (= 1 (anvil-host--retired-count)))
              (should (timerp anvil-host--cleanup-timer))
              (should-not (anvil-host--cleanup-active-p))
              (anvil-host--retry-retired-processes)
              (anvil-host-reentrancy-test--assert-clean
               nil (list custody))
              (setq case-count (1+ case-count)))
          (anvil-host-reentrancy-test--force-clean
           nil (list custody)))))
    (should (= case-count 3))))

(ert-deftest anvil-host-reentrancy-active-watcher-cannot-enter-retry ()
  "A watcher on observable cleanup state sees the stable guard already set."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((custody (generate-new-buffer " *anvil-host-watcher-custody*"))
         (child
          (make-process
           :name "anvil-host-watcher-child"
           :buffer custody
           :command (list shell-file-name shell-command-switch "exit 0")
           :connection-type 'pipe
           :noquery t
           :sentinel #'ignore))
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         seen
         watcher)
    (unwind-protect
        (progn
          (while (process-live-p child)
            (accept-process-output child 0.05))
          (setq watcher
                (lambda (_symbol _new-value _operation _where)
                  (push (anvil-host--cleanup-active-p) seen)
                  (anvil-host--retry-retired-processes)))
          (add-variable-watcher 'anvil-host--cleanup-active watcher)
          (anvil-host--retire-process child nil custody nil)
          (remove-variable-watcher 'anvil-host--cleanup-active watcher)
          (setq watcher nil)
          (should seen)
          (should (seq-every-p #'identity seen))
          (anvil-host-reentrancy-test--assert-clean
           (list child) (list custody)))
      (when watcher
        (remove-variable-watcher 'anvil-host--cleanup-active watcher))
      (anvil-host-reentrancy-test--force-clean
       (list child) (list custody)))))


(ert-deftest anvil-host-reentrancy-watcher-error-retains-public-run ()
  "A watched cleanup binding cannot precede public-run resource custody."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let ((real-make-process anvil-host--make-process-primitive)
        (real-make-pipe-process anvil-host--make-pipe-process-primitive)
        (anvil-host--cleanup-state
         (cons nil (make-hash-table :test #'eq)))
        (anvil-host--cleanup-timer nil)
        (anvil-host--cleanup-active nil)
        processes watcher condition)
    (unwind-protect
        (progn
          (setq watcher
                (lambda (_symbol new-value _operation _where)
                  (when new-value
                    (error "watcher blocked publication"))))
          (add-variable-watcher 'anvil-host--cleanup-active watcher)
          (cl-letf
              ((anvil-host--make-pipe-process-primitive
                (lambda (&rest args)
                  (let ((process (apply real-make-pipe-process args)))
                    (push process processes)
                    process)))
               (anvil-host--make-process-primitive
                (lambda (&rest args)
                  (let ((process (apply real-make-process args)))
                    (push process processes)
                    process))))
            (setq condition
                  (should-error
                   (anvil-host--run
                    "exec sleep 30" 'utf-8-unix nil 0.01))))
          (should
           (string-match-p
            "watcher blocked publication"
            (error-message-string condition)))
          (should (= (length processes) 2))
          (should (seq-every-p #'process-live-p processes))
          (should (= 1 (anvil-host--retired-count)))
          (should (timerp anvil-host--cleanup-timer))
          (should-not (anvil-host--cleanup-active-p))
          (remove-variable-watcher 'anvil-host--cleanup-active watcher)
          (setq watcher nil)
          (anvil-host--retry-retired-processes)
          (anvil-host-reentrancy-test--assert-clean processes nil))
      (when watcher
        (remove-variable-watcher 'anvil-host--cleanup-active watcher))
      (anvil-host-reentrancy-test--force-clean processes nil))))

(ert-deftest anvil-host-reentrancy-name-collision-keeps-old-peer ()
  "A constructor collision retires its whole new family, not an old peer."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (dolist (spawn-kind '(stderr main))
    (let* ((real-make-process anvil-host--make-process-primitive)
           (real-make-pipe-process
            anvil-host--make-pipe-process-primitive)
           (shell-base "anvil-host-collision-shell")
           (stderr-base "anvil-host-collision-stderr")
           (target-base
            (if (eq spawn-kind 'main) shell-base stderr-base))
           (peer-name (concat target-base "<1>"))
           (peer
            (if (eq spawn-kind 'main)
                (make-process
                 :name peer-name :buffer nil
                 :command (list shell-file-name shell-command-switch
                                "exec sleep 30")
                 :connection-type 'pipe :noquery t :sentinel #'ignore)
              (make-pipe-process
               :name peer-name :buffer (current-buffer)
               :noquery t :sentinel #'ignore :filter #'ignore)))
           (preexisting (process-list))
           (anvil-host--cleanup-state
            (cons nil (make-hash-table :test #'eq)))
           (anvil-host--cleanup-timer nil)
           (anvil-host--cleanup-active nil)
           created exited)
      (when (eq spawn-kind 'stderr)
        (set-process-buffer peer nil))
      (unwind-protect
          (progn
            (cl-letf
                (((symbol-function 'anvil-host--unique-process-name)
                  (lambda (prefix)
                    (if (string-prefix-p "anvil-host-shell-" prefix)
                        shell-base
                      stderr-base)))
                 (anvil-host--make-pipe-process-primitive
                  (lambda (&rest args)
                    (if (eq spawn-kind 'stderr)
                        (let ((decoy
                               (apply real-make-pipe-process args))
                              actual)
                          (setq actual
                                (apply real-make-pipe-process args)
                                created (list decoy actual))
                          (set-process-buffer decoy nil)
                          (set-process-buffer actual nil)
                          (throw 'anvil-host-name-collision :original))
                      (apply real-make-pipe-process args))))
                 (anvil-host--make-process-primitive
                  (lambda (&rest args)
                    (if (eq spawn-kind 'main)
                        (let ((decoy (apply real-make-process args))
                              actual)
                          (setq actual (apply real-make-process args)
                                created (list decoy actual))
                          (set-process-buffer decoy nil)
                          (set-process-buffer actual nil)
                          (throw 'anvil-host-name-collision :original))
                      (apply real-make-process args)))))
              (setq exited
                    (catch 'anvil-host-name-collision
                      (anvil-host--run
                       "exec sleep 30" 'utf-8-unix nil 2)
                      :returned)))
            (should (eq exited :original))
            (should (= (length created) 2))
            (should (process-live-p peer))
            (should
             (seq-every-p
              (lambda (process) (not (process-live-p process)))
              created))
            (dolist (base (list shell-base stderr-base))
              (should-not
               (seq-some
                #'process-live-p
                (anvil-host--new-processes-in-name-family
                 base preexisting))))
            (anvil-host-reentrancy-test--assert-clean created nil))
        (when (and (processp peer) (process-live-p peer))
          (delete-process peer))
        (anvil-host-reentrancy-test--force-clean created nil)))))

(ert-deftest anvil-host-reentrancy-batch-stages-before-first-cleanup ()
  "A tagged first deletion leaves every batch member globally retriable."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((children
          (mapcar
           (lambda (index)
             (make-process
              :name (format "anvil-host-batch-child-%d" index)
              :buffer nil
              :command (list shell-file-name shell-command-switch
                             "exec sleep 30")
              :connection-type 'pipe :noquery t :sentinel #'ignore))
           '(1 2)))
         (metadata
          (mapcar
           (lambda (process)
             (list :process process :stderr-process nil
                   :custody-buffer nil :stderr-custody-buffer nil))
           children))
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         exited)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'kill-process)
                     (lambda (&rest _args)
                       (throw 'anvil-host-batch-exit :original))))
            (setq exited
                  (catch 'anvil-host-batch-exit
                    (anvil-host--retire-resources metadata)
                    :returned)))
          (should (eq exited :original))
          (should (= 2 (anvil-host--retired-count)))
          (should (timerp anvil-host--cleanup-timer))
          (should (seq-every-p #'process-live-p children))
          (anvil-host--retry-retired-processes)
          (anvil-host-reentrancy-test--assert-clean children nil))
      (anvil-host-reentrancy-test--force-clean children nil))))

(ert-deftest anvil-host-reentrancy-puthash-exit-retains-staged-custody ()
  "An after-insertion hash exit cannot strand an unstaged live child."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((child
          (make-process
           :name "anvil-host-puthash-exit" :buffer nil
           :command (list shell-file-name shell-command-switch
                          "exec sleep 30")
           :connection-type 'pipe :noquery t :sentinel #'ignore))
         (real-puthash (symbol-function 'puthash))
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         fired exited)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'puthash)
                     (lambda (key value table)
                       (prog1 (funcall real-puthash key value table)
                         (unless fired
                           (setq fired t)
                           (throw 'anvil-host-puthash-exit :original))))))
            (setq exited
                  (catch 'anvil-host-puthash-exit
                    (anvil-host--retire-process child nil nil nil)
                    :returned)))
          (should (eq exited :original))
          (should (process-live-p child))
          (should (= 1 (anvil-host--retired-count)))
          (should (timerp anvil-host--cleanup-timer))
          (should-not (anvil-host--cleanup-active-p))
          (anvil-host--retry-retired-processes)
          (anvil-host-reentrancy-test--assert-clean (list child) nil))
      (anvil-host-reentrancy-test--force-clean (list child) nil))))

(ert-deftest anvil-host-reentrancy-timer-publication-watcher-is-finite ()
  "A timer mirror watcher cannot create an anonymous repeating timer."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((child
          (make-process
           :name "anvil-host-timer-publication" :buffer nil
           :command (list shell-file-name shell-command-switch
                          "exec sleep 30")
           :connection-type 'pipe :noquery t :sentinel #'ignore))
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         (anvil-host-cleanup-timeout 0.01)
         watcher timer condition)
    (unwind-protect
        (progn
          (setq watcher
                (lambda (_symbol new-value _operation _where)
                  (when (timerp new-value)
                    (error "timer publication blocked"))))
          (add-variable-watcher 'anvil-host--cleanup-timer watcher)
          (cl-letf (((symbol-function 'kill-process)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'delete-process)
                     (lambda (&rest _args) nil)))
            (setq condition
                  (should-error
                   (anvil-host--retire-process child nil nil nil))))
          (should
           (string-match-p
            "timer publication blocked"
            (error-message-string condition)))
          (setq timer (aref (anvil-host--resource-state) 2))
          (should (timerp timer))
          (should (anvil-host--cleanup-timer-scheduled-p timer))
          (should-not anvil-host--cleanup-timer)
          (should (process-live-p child))
          (should (= 1 (anvil-host--retired-count)))
          (remove-variable-watcher 'anvil-host--cleanup-timer watcher)
          (setq watcher nil)
          (anvil-host-reentrancy-test--wait-for-cleanup 2)
          (should-not (memq timer timer-list))
          (anvil-host-reentrancy-test--assert-clean (list child) nil))
      (when watcher
        (remove-variable-watcher 'anvil-host--cleanup-timer watcher))
      (anvil-host-reentrancy-test--force-clean (list child) nil))))

(ert-deftest anvil-host-reentrancy-stale-timer-mirror-cannot-block-retry ()
  "A blocked mirror clear cannot suppress the next exact retry timer."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((children
          (mapcar
           (lambda (index)
             (make-process
              :name (format "anvil-host-timer-clear-%d" index)
              :buffer nil
              :command (list shell-file-name shell-command-switch
                             "exec sleep 30")
              :connection-type 'pipe :noquery t :sentinel #'ignore))
           '(1 2)))
         (child1 (nth 0 children))
         (child2 (nth 1 children))
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         (anvil-host-cleanup-timeout 0.01)
         watcher timer1 timer2)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'kill-process)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'delete-process)
                     (lambda (&rest _args) nil)))
            (anvil-host--retire-process child1 nil nil nil))
          (setq timer1 (aref (anvil-host--resource-state) 2))
          (should (anvil-host--cleanup-timer-scheduled-p timer1))
          (setq watcher
                (lambda (_symbol new-value _operation _where)
                  (when (null new-value)
                    (error "timer clear blocked"))))
          (add-variable-watcher 'anvil-host--cleanup-timer watcher)
          (anvil-host--retry-retired-processes)
          (should-not (process-live-p child1))
          (should (timerp anvil-host--cleanup-timer))
          (should-not (anvil-host--cleanup-timer-scheduled-p timer1))
          (should-not (aref (anvil-host--resource-state) 2))
          (cl-letf (((symbol-function 'kill-process)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'delete-process)
                     (lambda (&rest _args) nil)))
            (anvil-host--retire-process child2 nil nil nil))
          (setq timer2 (aref (anvil-host--resource-state) 2))
          (should (timerp timer2))
          (should-not (eq timer1 timer2))
          (should (anvil-host--cleanup-timer-scheduled-p timer2))
          (remove-variable-watcher 'anvil-host--cleanup-timer watcher)
          (setq watcher nil)
          (anvil-host-reentrancy-test--wait-for-cleanup 2)
          (anvil-host-reentrancy-test--assert-clean children nil))
      (when watcher
        (remove-variable-watcher 'anvil-host--cleanup-timer watcher))
      (anvil-host-reentrancy-test--force-clean children nil))))
(ert-deftest anvil-host-reentrancy-constructor-cannot-submit-recursively ()
  "A constructor callback is rejected before a nested process allocation."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let ((real-make-pipe-process
         anvil-host--make-pipe-process-primitive)
        (anvil-host--cleanup-state
         (cons nil (make-hash-table :test #'eq)))
        (anvil-host--cleanup-timer nil)
        (anvil-host--cleanup-active nil)
        attempted nested-spawns nested-condition)
    (unwind-protect
        (cl-letf
            ((anvil-host--make-pipe-process-primitive
              (lambda (&rest args)
                (if attempted
                    (progn
                      (setq nested-spawns (1+ (or nested-spawns 0)))
                      (apply real-make-pipe-process args))
                  (setq attempted t
                        nested-condition
                        (should-error
                         (anvil-host--run
                          "printf nested" 'utf-8-unix nil 1)))
                  (apply real-make-pipe-process args)))))
          (should
           (equal (anvil-host-reentrancy-test--result 0 "outer" "")
                  (anvil-host--run
                   "printf outer" 'utf-8-unix nil 1)))
          (should
           (string-match-p
            "submission already active"
            (error-message-string nested-condition)))
          (should-not nested-spawns)
          (anvil-host-reentrancy-test--assert-clean nil nil))
      (anvil-host-reentrancy-test--force-clean nil nil))))
(ert-deftest anvil-host-reentrancy-public-run-rejects-cleanup-snapshots ()
  "Public host submission cannot spawn during active or retained cleanup."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let ((anvil-host--cleanup-state
         (cons t (make-hash-table :test #'eq)))
        (anvil-host--cleanup-timer nil)
        (anvil-host--cleanup-active nil)
        (spawn-count 0)
        condition)
    (unwind-protect
        (cl-letf ((anvil-host--make-pipe-process-primitive
                   (lambda (&rest _args)
                     (setq spawn-count (1+ spawn-count))))
                  (anvil-host--make-process-primitive
                   (lambda (&rest _args)
                     (setq spawn-count (1+ spawn-count)))))
          (setq condition
                (should-error
                 (anvil-host--run "printf no" 'utf-8-unix nil 1)))
          (should
           (string-match-p
            "cleanup already active"
            (error-message-string condition)))
          (should (zerop spawn-count)))
      (setcar anvil-host--cleanup-state nil)
      (anvil-host-reentrancy-test--force-clean nil nil)))
  (let ((custody
         (generate-new-buffer " *anvil-host-pending-custody*"))
        (anvil-host--cleanup-state
         (cons nil (make-hash-table :test #'eq)))
        (anvil-host--cleanup-timer nil)
        (anvil-host--cleanup-active nil)
        (spawn-count 0)
        condition)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'kill-buffer)
                     (lambda (&rest _args)
                       (error "pending cleanup"))))
            (anvil-host--retire-process nil nil custody nil))
          (cl-letf (((symbol-function 'kill-buffer)
                     (lambda (&rest _args)
                       (error "pending cleanup")))
                    (anvil-host--make-pipe-process-primitive
                     (lambda (&rest _args)
                       (setq spawn-count (1+ spawn-count))))
                    (anvil-host--make-process-primitive
                     (lambda (&rest _args)
                       (setq spawn-count (1+ spawn-count)))))
            (setq condition
                  (should-error
                   (anvil-host--run "printf no" 'utf-8-unix nil 1))))
          (should
           (string-match-p
            "retained cleanup has not converged"
            (error-message-string condition)))
          (should (zerop spawn-count))
          (should (buffer-live-p custody))
          (anvil-host--retry-retired-processes)
          (anvil-host-reentrancy-test--assert-clean nil (list custody)))
      (anvil-host-reentrancy-test--force-clean nil (list custody)))))

(ert-deftest anvil-host-reentrancy-rewritten-constructor-recovers-by-snapshot ()
  "Advice cannot hide a child by rewriting both its name and filter."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (dolist (spawn-kind '(stderr main))
    (let ((real-make-process anvil-host--make-process-primitive)
          (real-make-pipe-process anvil-host--make-pipe-process-primitive)
          (anvil-host--cleanup-state
           (cons nil (make-hash-table :test #'eq)))
          (anvil-host--cleanup-timer nil)
          (anvil-host--cleanup-active nil)
          created exited)
      (unwind-protect
          (progn
            (cl-letf
                ((anvil-host--make-pipe-process-primitive
                  (lambda (&rest args)
                    (if (eq spawn-kind 'stderr)
                        (progn
                          (setq args
                                (plist-put
                                 (plist-put args :name
                                            "rewritten-stderr")
                                 :filter (lambda (&rest _ignored) nil)))
                          (setq created
                                (apply real-make-pipe-process args))
                          (set-process-buffer created nil)
                          (throw 'anvil-host-rewritten-name :original))
                      (apply real-make-pipe-process args))))
                 (anvil-host--make-process-primitive
                  (lambda (&rest args)
                    (if (eq spawn-kind 'main)
                        (progn
                          (setq args
                                (plist-put
                                 (plist-put args :name "rewritten-main")
                                 :filter (lambda (&rest _ignored) nil)))
                          (setq created (apply real-make-process args))
                          (set-process-buffer created nil)
                          (throw 'anvil-host-rewritten-name :original))
                      (apply real-make-process args)))))
              (setq exited
                    (catch 'anvil-host-rewritten-name
                      (anvil-host--run
                       "exec sleep 30" 'utf-8-unix nil 2)
                      :returned)))
            (should (eq exited :original))
            (should (processp created))
            (anvil-host-reentrancy-test--assert-clean
             (list created) nil))
        (anvil-host-reentrancy-test--force-clean
         (list created) nil)))))

(ert-deftest anvil-host-reentrancy-returned-peer-is-never-touched ()
  "A constructor-returned preexisting peer survives without interaction."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (dolist (spawn-kind '(stderr main))
    (let* ((real-make-process anvil-host--make-process-primitive)
           (real-make-pipe-process
            anvil-host--make-pipe-process-primitive)
           (real-send-eof anvil-host--process-send-eof-primitive)
           (peer-buffer
            (generate-new-buffer " *anvil-host-returned-peer*"))
           (peer
            (if (eq spawn-kind 'main)
                (make-process
                 :name "anvil-host-returned-main-peer"
                 :buffer peer-buffer
                 :command (list shell-file-name shell-command-switch
                                "exec cat")
                 :connection-type 'pipe :noquery t :sentinel #'ignore)
              (make-pipe-process
               :name "anvil-host-returned-stderr-peer"
               :buffer peer-buffer :noquery t :sentinel #'ignore
               :filter #'ignore)))
           (anvil-host--cleanup-state
            (cons nil (make-hash-table :test #'eq)))
           (anvil-host--cleanup-timer nil)
           (anvil-host--cleanup-active nil)
           actual condition peer-eof-seen)
      (unwind-protect
          (progn
            (cl-letf
                ((anvil-host--make-pipe-process-primitive
                  (lambda (&rest args)
                    (if (eq spawn-kind 'stderr)
                        (progn
                          (setq args
                                (plist-put
                                 (plist-put args :name
                                            "hidden-returned-stderr")
                                 :filter (lambda (&rest _ignored) nil)))
                          (setq actual
                                (apply real-make-pipe-process args))
                          peer)
                      (apply real-make-pipe-process args))))
                 (anvil-host--make-process-primitive
                  (lambda (&rest args)
                    (if (eq spawn-kind 'main)
                        (progn
                          (setq args
                                (plist-put
                                 (plist-put args :name
                                            "hidden-returned-main")
                                 :filter (lambda (&rest _ignored) nil)))
                          (setq actual (apply real-make-process args))
                          peer)
                      (apply real-make-process args))))
                 (anvil-host--process-send-eof-primitive
                  (lambda (process)
                    (when (eq process peer)
                      (setq peer-eof-seen t))
                    (funcall real-send-eof process))))
              (setq condition
                    (should-error
                     (anvil-host--run
                      "exec sleep 30" 'utf-8-unix nil 2))))
            (should
             (string-match-p
              "constructor returned a preexisting process"
              (error-message-string condition)))
            (should (process-live-p peer))
            (should-not peer-eof-seen)
            ;; For main, a mistaken EOF would terminate `cat'.  For stderr,
            ;; a mistaken detach would remove this exact buffer association.
            (should (eq (process-buffer peer) peer-buffer))
            (should (processp actual))
            (anvil-host-reentrancy-test--assert-clean
             (list actual) nil))
        (when (and (processp peer) (process-live-p peer))
          (delete-process peer))
        (when (buffer-live-p peer-buffer)
          (kill-buffer peer-buffer))
        (anvil-host-reentrancy-test--force-clean
         (list actual) nil)))))

(ert-deftest anvil-host-reentrancy-valid-constructor-preserves-helper ()
  "A successful constructor cannot donate an unrelated helper to cleanup."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (dolist (spawn-kind '(stderr main))
    (let ((real-make-process anvil-host--make-process-primitive)
          (real-make-pipe-process anvil-host--make-pipe-process-primitive)
          (anvil-host--cleanup-state
           (cons nil (make-hash-table :test #'eq)))
          (anvil-host--cleanup-timer nil)
          (anvil-host--cleanup-active nil)
          helper target)
      (unwind-protect
          (progn
            (cl-letf
                ((anvil-host--make-pipe-process-primitive
                  (lambda (&rest args)
                    (if (eq spawn-kind 'stderr)
                        (progn
                          (setq helper
                                (funcall
                                 real-make-process
                                 :name "anvil-host-unrelated-helper-stderr"
                                 :buffer nil
                                 :command
                                 (list shell-file-name shell-command-switch
                                       "exec sleep 30")
                                 :connection-type 'pipe
                                 :noquery t
                                 :sentinel #'ignore)
                                target
                                (apply real-make-pipe-process args))
                          target)
                      (apply real-make-pipe-process args))))
                 (anvil-host--make-process-primitive
                  (lambda (&rest args)
                    (if (eq spawn-kind 'main)
                        (progn
                          (setq helper
                                (funcall
                                 real-make-process
                                 :name "anvil-host-unrelated-helper-main"
                                 :buffer nil
                                 :command
                                 (list shell-file-name shell-command-switch
                                       "exec sleep 30")
                                 :connection-type 'pipe
                                 :noquery t
                                 :sentinel #'ignore)
                                target
                                (apply real-make-process args))
                          target)
                      (apply real-make-process args)))))
              (should
               (equal (anvil-host-reentrancy-test--result 0 "tracked" "")
                      (anvil-host--run
                       "printf tracked" 'utf-8-unix nil 2))))
            (should (processp target))
            (should-not (process-live-p target))
            (should (processp helper))
            (should (process-live-p helper))
            (anvil-host-reentrancy-test--assert-clean
             (list target) nil))
        (when (and (processp helper) (process-live-p helper))
          (delete-process helper))
        (anvil-host-reentrancy-test--force-clean
         (list target) nil)))))

(ert-deftest anvil-host-reentrancy-timer-clear-watcher-cannot-submit ()
  "The retry timer acquires its stable guard before clearing its mirror."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((real-make-process anvil-host--make-process-primitive)
         (real-make-pipe-process anvil-host--make-pipe-process-primitive)
         (child
          (make-process
           :name "anvil-host-timer-reentry-child"
           :buffer nil
           :command
           (list shell-file-name shell-command-switch "exec sleep 30")
           :connection-type 'pipe
           :noquery t
           :sentinel #'ignore))
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         (anvil-host-cleanup-timeout 0.01)
         watcher attempted condition nested-returned (spawn-count 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'kill-process)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'delete-process)
                     (lambda (&rest _args) nil)))
            (anvil-host--retire-process child nil nil nil))
          (should (process-live-p child))
          (should (anvil-host--cleanup-timer-scheduled-p))
          (setq watcher
                (lambda (_symbol new-value _operation _where)
                  (when (and (null new-value) (not attempted))
                    (setq attempted t)
                    (condition-case caught
                        (progn
                          (anvil-host--run
                           "printf nested" 'utf-8-unix nil 1)
                          (setq nested-returned t))
                      (error (setq condition caught))))))
          (add-variable-watcher 'anvil-host--cleanup-timer watcher)
          (cl-letf
              ((anvil-host--make-pipe-process-primitive
                (lambda (&rest args)
                  (setq spawn-count (1+ spawn-count))
                  (apply real-make-pipe-process args)))
               (anvil-host--make-process-primitive
                (lambda (&rest args)
                  (setq spawn-count (1+ spawn-count))
                  (apply real-make-process args))))
            (anvil-host-reentrancy-test--wait-for-cleanup 3))
          (should attempted)
          (should condition)
          (should
           (string-match-p
            "cleanup already active" (error-message-string condition)))
          (should-not nested-returned)
          (should (zerop spawn-count))
          (remove-variable-watcher 'anvil-host--cleanup-timer watcher)
          (setq watcher nil)
          (anvil-host-reentrancy-test--assert-clean (list child) nil))
      (when watcher
        (remove-variable-watcher 'anvil-host--cleanup-timer watcher))
      (anvil-host-reentrancy-test--force-clean (list child) nil))))

(ert-deftest anvil-host-reentrancy-cross-constructor-peer-is-preserved ()
  "Main construction cannot claim a helper created by stderr construction."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let ((real-make-process anvil-host--make-process-primitive)
        (real-make-pipe-process anvil-host--make-pipe-process-primitive)
        (anvil-host--cleanup-state
         (cons nil (make-hash-table :test #'eq)))
        (anvil-host--cleanup-timer nil)
        (anvil-host--cleanup-active nil)
        helper stderr-target condition)
    (unwind-protect
        (progn
          (cl-letf
              ((anvil-host--make-pipe-process-primitive
                (lambda (&rest args)
                  (setq helper
                        (funcall
                         real-make-process
                         :name "anvil-host-cross-constructor-helper"
                         :buffer nil
                         :command
                         (list shell-file-name shell-command-switch
                               "exec cat")
                         :connection-type 'pipe
                         :noquery t
                         :sentinel #'ignore)
                        stderr-target
                        (apply real-make-pipe-process args))
                  stderr-target))
               (anvil-host--make-process-primitive
                (lambda (&rest args)
                  ;; Match the requested main filter, but return the live
                  ;; helper that existed before this constructor began.
                  (set-process-filter helper (plist-get args :filter))
                  helper)))
            (setq condition
                  (should-error
                   (anvil-host--run
                    "printf unreachable" 'utf-8-unix nil 2))))
          (should
           (string-match-p
            "shell constructor returned a preexisting process"
            (error-message-string condition)))
          (should (process-live-p helper))
          (should (processp stderr-target))
          (anvil-host-reentrancy-test--assert-clean
           (list stderr-target) nil))
      (when (and (processp helper) (process-live-p helper))
        (delete-process helper))
      (anvil-host-reentrancy-test--force-clean
       (list stderr-target) nil))))

(ert-deftest anvil-host-reentrancy-timer-constructor-preserves-peer ()
  "Cleanup owns the exact one-shot even when its constructor returns a peer."
  (let* ((real-run-at-time anvil-host--run-at-time-function)
         (peer (run-at-time 30 30 #'ignore))
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         actual)
    (unwind-protect
        (progn
          (cl-letf ((anvil-host--run-at-time-function
                     (lambda (&rest args)
                       (setq actual (apply real-run-at-time args))
                       peer)))
            (anvil-host--ensure-cleanup-timer))
          (should (timerp actual))
          (should (eq (aref (anvil-host--resource-state) 2) actual))
          (should (eq anvil-host--cleanup-timer actual))
          (should (memq actual timer-list))
          (should (memq peer timer-list))
          (anvil-host--cancel-cleanup-timer)
          (should-not (memq actual timer-list))
          (should (memq peer timer-list))
          (should-not (aref (anvil-host--resource-state) 2))
          (should-not anvil-host--cleanup-timer))
      (when (and (timerp actual) (memq actual timer-list))
        (cancel-timer actual))
      (when (memq peer timer-list)
        (cancel-timer peer))
      (anvil-host-reentrancy-test--force-clean nil nil))))

(ert-deftest anvil-host-reentrancy-timer-constructor-tagged-exit-is-owned ()
  "A one-shot created before a tagged constructor exit remains cancellable."
  (let ((real-run-at-time anvil-host--run-at-time-function)
        (anvil-host--cleanup-state
         (cons nil (make-hash-table :test #'eq)))
        (anvil-host--cleanup-timer nil)
        (anvil-host--cleanup-active nil)
        actual exited)
    (unwind-protect
        (progn
          (cl-letf ((anvil-host--run-at-time-function
                     (lambda (&rest args)
                       (setq actual (apply real-run-at-time args))
                       (throw 'anvil-host-timer-constructor-exit :original))))
            (setq exited
                  (catch 'anvil-host-timer-constructor-exit
                    (anvil-host--ensure-cleanup-timer)
                    :returned)))
          (should (eq exited :original))
          (should (timerp actual))
          (should (eq (aref (anvil-host--resource-state) 2) actual))
          (should-not anvil-host--cleanup-timer)
          (should (memq actual timer-list))
          (anvil-host--cancel-cleanup-timer)
          (should-not (memq actual timer-list))
          (should-not (aref (anvil-host--resource-state) 2)))
      (when (and (timerp actual) (memq actual timer-list))
        (cancel-timer actual))
      (anvil-host-reentrancy-test--force-clean nil nil))))

(ert-deftest anvil-host-reentrancy-timer-constructor-normalizes-shape ()
  "Constructor repeat delays and callback arguments cannot defeat retry."
  (dolist (mutation '(repeat arguments))
    (let ((real-run-at-time anvil-host--run-at-time-function)
          (anvil-host--cleanup-state
           (cons nil (make-hash-table :test #'eq)))
          (anvil-host--cleanup-timer nil)
          (anvil-host--cleanup-active nil)
          actual)
      (unwind-protect
          (progn
            (cl-letf ((anvil-host--run-at-time-function
                       (lambda (&rest args)
                         (pcase mutation
                           ('repeat (setf (nth 1 args) 1))
                           ('arguments
                            (setq args (append args (list :bogus)))))
                         (setq actual (apply real-run-at-time args)))))
              (anvil-host--ensure-cleanup-timer))
            (should (timerp actual))
            (should (eq (aref (anvil-host--resource-state) 2) actual))
            (should (eq anvil-host--cleanup-timer actual))
            (should (memq actual timer-list))
            (should-not (timer--repeat-delay actual))
            (should-not (timer--args actual))
            (anvil-host--cancel-cleanup-timer)
            (should-not (memq actual timer-list)))
        (when (and (timerp actual) (memq actual timer-list))
          (cancel-timer actual))
        (anvil-host-reentrancy-test--force-clean nil nil)))))

(ert-deftest anvil-host-reentrancy-timer-constructor-discards-duplicate ()
  "Only one exact callback timer survives duplicate constructor output."
  (let ((real-run-at-time anvil-host--run-at-time-function)
        (anvil-host--cleanup-state
         (cons nil (make-hash-table :test #'eq)))
        (anvil-host--cleanup-timer nil)
        (anvil-host--cleanup-active nil)
        first second)
    (unwind-protect
        (progn
          (cl-letf ((anvil-host--run-at-time-function
                     (lambda (&rest args)
                       (setq first (apply real-run-at-time args)
                             second (apply real-run-at-time args))
                       first)))
            (anvil-host--ensure-cleanup-timer))
          (should (eq (aref (anvil-host--resource-state) 2) first))
          (should (memq first timer-list))
          (should-not (memq second timer-list))
          (anvil-host--cancel-cleanup-timer)
          (should-not (memq first timer-list)))
      (dolist (timer (list first second))
        (when (and (timerp timer) (memq timer timer-list))
          (cancel-timer timer)))
      (anvil-host-reentrancy-test--force-clean nil nil))))

(ert-deftest anvil-host-reentrancy-early-timer-fire-rearms-exact-retry ()
  "A one-shot firing before advised construction returns is not lost."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((real-run-at-time anvil-host--run-at-time-function)
         (child
          (make-process
           :name "anvil-host-early-timer-child"
           :buffer nil
           :command
           (list shell-file-name shell-command-switch "exec sleep 30")
           :connection-type 'pipe
           :noquery t
           :sentinel #'ignore))
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         actual observed-during-constructor)
    (unwind-protect
        (progn
          (cl-letf ((anvil-host--run-at-time-function
                     (lambda (&rest args)
                       (setq actual (apply real-run-at-time args))
                       ;; Deliver the exact one-shot before its constructor
                       ;; returns, independent of host scheduler latency.
                       (let ((timer-event-last actual))
                         (funcall (nth 2 args)))
                       (setq observed-during-constructor
                             (eq
                              (aref (anvil-host--resource-state) 2)
                              actual))
                       actual))
                    ((symbol-function 'kill-process)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'delete-process)
                     (lambda (&rest _args) nil)))
            (anvil-host--retire-process child nil nil nil))
          (should observed-during-constructor)
          (should (process-live-p child))
          (should (eq (aref (anvil-host--resource-state) 2) actual))
          (should (anvil-host--cleanup-timer-scheduled-p actual))
          (anvil-host-reentrancy-test--wait-for-cleanup 3)
          (anvil-host-reentrancy-test--assert-clean (list child) nil))
      (anvil-host-reentrancy-test--force-clean (list child) nil))))

(ert-deftest anvil-host-reentrancy-stale-last-timer-preserves-peer ()
  "Synchronous callback advice cannot mistake a stale last timer for its own."
  (let* ((real-run-at-time anvil-host--run-at-time-function)
         (peer (run-at-time 30 30 #'ignore 'peer-argument))
         (peer-function (timer--function peer))
         (peer-repeat (timer--repeat-delay peer))
         (peer-args (copy-tree (timer--args peer)))
         (peer-time (copy-tree (timer--time peer)))
         (anvil-host--cleanup-state
          (cons t (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         actual)
    (unwind-protect
        (progn
          (cl-letf ((anvil-host--run-at-time-function
                     (lambda (&rest args)
                       (setq actual (apply real-run-at-time args))
                       (let ((timer-event-last peer))
                         (funcall (nth 2 args)))
                       actual)))
            (anvil-host--ensure-cleanup-timer))
          (setcar anvil-host--cleanup-state nil)
          (should (eq (aref (anvil-host--resource-state) 2) actual))
          (should (eq (timer--function peer) peer-function))
          (should (equal (timer--repeat-delay peer) peer-repeat))
          (should (equal (timer--args peer) peer-args))
          (should (equal (timer--time peer) peer-time))
          (should (memq peer timer-list))
          (anvil-host--cancel-cleanup-timer)
          (should (memq peer timer-list)))
      (setcar anvil-host--cleanup-state nil)
      (when (and (timerp actual) (memq actual timer-list))
        (cancel-timer actual))
      (when (memq peer timer-list)
        (cancel-timer peer))
      (anvil-host-reentrancy-test--force-clean nil nil))))

(ert-deftest anvil-host-reentrancy-wrapped-timer-is-normalized ()
  "A returned callback wrapper is owned instead of left repeating."
  (let ((real-run-at-time anvil-host--run-at-time-function)
        (before (copy-sequence timer-list))
        (anvil-host--cleanup-state
         (cons nil (make-hash-table :test #'eq)))
        (anvil-host--cleanup-timer nil)
        (anvil-host--cleanup-active nil)
        actual wrapper)
    (unwind-protect
        (progn
          (cl-letf ((anvil-host--run-at-time-function
                     (lambda (&rest args)
                       (let ((callback (nth 2 args)))
                         (setq wrapper
                               (lambda (&rest callback-args)
                                 (apply callback callback-args)))
                         (setf (nth 1 args) 30
                               (nth 2 args) wrapper)
                         (setq args (append args (list :bogus)))
                         (setq actual (apply real-run-at-time args))))))
            (anvil-host--ensure-cleanup-timer))
          (should (eq (aref (anvil-host--resource-state) 2) actual))
          (should (memq actual timer-list))
          (should-not (eq (timer--function actual) wrapper))
          (should-not (timer--repeat-delay actual))
          (should-not (timer--args actual))
          (should
           (equal
            (list actual)
            (cl-remove-if
             (lambda (timer) (memq timer before))
             timer-list)))
          (anvil-host--cancel-cleanup-timer)
          (should-not (memq actual timer-list)))
      (when (and (timerp actual) (memq actual timer-list))
        (cancel-timer actual))
      (anvil-host-reentrancy-test--force-clean nil nil))))

(ert-deftest anvil-host-reentrancy-filter-rewrite-cannot-leak-output ()
  "A successful name match cannot replace the private output filter."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let ((real-make-process anvil-host--make-process-primitive)
        (visible (generate-new-buffer " *anvil-host-visible-output*"))
        (anvil-host--cleanup-state
         (cons nil (make-hash-table :test #'eq)))
        (anvil-host--cleanup-timer nil)
        (anvil-host--cleanup-active nil)
        created condition)
    (unwind-protect
        (progn
          (cl-letf
              ((anvil-host--make-process-primitive
                (lambda (&rest args)
                  (if
                      (string-prefix-p
                       "anvil-host-shell-" (plist-get args :name))
                      (progn
                        (setq args
                              (plist-put
                               (plist-put args :buffer visible)
                               :filter nil))
                        (setq created (apply real-make-process args)))
                    (apply real-make-process args)))))
            (setq condition
                  (should-error
                   (anvil-host--run
                    "printf process-custody-canary"
                    'utf-8-unix nil 2))))
          (should
           (string-match-p
            "shell constructor returned an invalid process"
            (error-message-string condition)))
          (should (processp created))
          (should-not (process-live-p created))
          (should (buffer-live-p visible))
          (should (string-empty-p
                   (with-current-buffer visible (buffer-string))))
          (anvil-host-reentrancy-test--assert-clean (list created) nil))
      (when (buffer-live-p visible)
        (kill-buffer visible))
      (anvil-host-reentrancy-test--force-clean (list created) nil))))

(ert-deftest anvil-host-reentrancy-process-list-advice-cannot-orphan ()
  "Persistent process-list advice cannot interrupt exact child recovery."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let ((real-make-process anvil-host--make-process-primitive)
        (anvil-host--cleanup-state
         (cons nil (make-hash-table :test #'eq)))
        (anvil-host--cleanup-timer nil)
        (anvil-host--cleanup-active nil)
        created outcome)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'process-list)
                (lambda ()
                  (error "persistent process-list failure")))
               (anvil-host--make-process-primitive
                (lambda (&rest args)
                  (setq created (apply real-make-process args))
                  (throw 'anvil-host-process-list-exit :original))))
            (setq outcome
                  (catch 'anvil-host-process-list-exit
                    (anvil-host--run
                     "exec sleep 30" 'utf-8-unix nil 2)
                    :returned)))
          (should (eq outcome :original))
          (should (processp created))
          (anvil-host-reentrancy-test--assert-clean (list created) nil))
      (anvil-host-reentrancy-test--force-clean (list created) nil))))

(ert-deftest anvil-host-reentrancy-adopts-legacy-repeating-timer ()
  "Live state upgrade adopts and cancels an old scheduled timer mirror."
  (let* ((legacy (run-at-time 30 30 #'ignore))
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer legacy)
         (anvil-host--cleanup-active nil)
         (resources (anvil-host--resource-state)))
    (unwind-protect
        (progn
          (should (eq (aref resources 2) legacy))
          (should (anvil-host--cleanup-timer-scheduled-p legacy))
          (anvil-host--cancel-cleanup-timer)
          (should-not (memq legacy timer-list))
          (should-not (aref resources 2))
          (should-not anvil-host--cleanup-timer))
      (when (memq legacy timer-list)
        (cancel-timer legacy))
      (anvil-host-reentrancy-test--force-clean nil nil))))
(ert-deftest anvil-host-reentrancy-cold-table-failure-spawns-nothing ()
  "Cold retirement-table failure precedes every resource constructor."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let ((real-make-hash-table (symbol-function 'make-hash-table))
        (anvil-host--cleanup-state (cons nil nil))
        (anvil-host--cleanup-timer nil)
        (anvil-host--cleanup-active nil)
        (spawn-count 0)
        condition)
    (unwind-protect
        (cl-letf
            (((symbol-function 'make-hash-table)
              (lambda (&rest args)
                (if (and
                     (null (aref (anvil-host--resource-state) 0))
                     (eq (plist-get args :test) #'eq))
                    (error "cold table failure")
                  (apply real-make-hash-table args))))
             (anvil-host--make-pipe-process-primitive
              (lambda (&rest _args)
                (setq spawn-count (1+ spawn-count))
                (error "unexpected pipe spawn")))
             (anvil-host--make-process-primitive
              (lambda (&rest _args)
                (setq spawn-count (1+ spawn-count))
                (error "unexpected process spawn"))))
          (setq condition
                (should-error
                 (anvil-host--run
                  "printf never" 'utf-8-unix nil 2)))
          (should
           (string-match-p
            "cold table failure" (error-message-string condition)))
          (should (zerop spawn-count))
          (should-not (aref (anvil-host--resource-state) 0))
          (should (anvil-host--retired-empty-p))
          (should-not (anvil-host--cleanup-active-p))
          (should-not (timerp anvil-host--cleanup-timer)))
      (anvil-host-reentrancy-test--force-clean nil nil))))
(ert-deftest anvil-host-reentrancy-detached-stderr-is-bounded ()
  "A background stderr holder cannot extend cleanup to the child timeout."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((directory (make-temp-file "anvil-host-detached-" t))
         (finished (expand-file-name "finished" directory))
         (anvil-host--cleanup-state (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         (anvil-host-cleanup-timeout 1.5)
         (started (float-time))
         result elapsed)
    (unwind-protect
        (progn
          (setq result
                (anvil-host--run
                 ;; A bounded kernel sleep outlives the cleanup deadline while
                 ;; guaranteeing self-termination if the test runner dies.
                 (format "(sleep 2; : > %s) &"
                         (shell-quote-argument finished))
                 'utf-8-unix directory 2)
                elapsed (- (float-time) started))
          (should (equal (anvil-host-reentrancy-test--result 0 "" "") result))
          (should-not (file-exists-p finished))
          ;; The configured child cleanup timeout is 1.5s.  Pipe closure must
          ;; remain near the documented 0.2s drain instead of waiting for it.
          (should (< elapsed 0.8))
          (let ((deadline (+ (float-time) 3)))
            (while (and (not (file-exists-p finished))
                        (< (float-time) deadline))
              (sleep-for 0.01)))
          (should (file-exists-p finished))
          (anvil-host-reentrancy-test--assert-clean nil nil))
      (anvil-host-reentrancy-test--force-clean nil nil)
      (delete-directory directory t))))

(ert-deftest anvil-host-reentrancy-idle-timer-peer-is-preserved ()
  "A returned preexisting idle timer is never adopted or mutated."
  (let* ((real-run-at-time anvil-host--run-at-time-function)
         (peer (run-with-idle-timer 30 t #'ignore 'peer-argument))
         (peer-function (timer--function peer))
         (peer-repeat (timer--repeat-delay peer))
         (peer-args (copy-tree (timer--args peer)))
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         actual)
    (unwind-protect
        (progn
          (cl-letf ((anvil-host--run-at-time-function
                     (lambda (&rest args)
                       (setq actual (apply real-run-at-time args))
                       peer)))
            (anvil-host--ensure-cleanup-timer))
          (should (eq (aref (anvil-host--resource-state) 2) actual))
          (should (memq actual timer-list))
          (should (memq peer timer-idle-list))
          (should-not (memq peer timer-list))
          (should (eq (timer--function peer) peer-function))
          (should (equal (timer--repeat-delay peer) peer-repeat))
          (should (equal (timer--args peer) peer-args))
          (anvil-host--cancel-cleanup-timer)
          (should-not (anvil-host--timer-scheduled-p actual))
          (should (memq peer timer-idle-list)))
      (when (anvil-host--timer-scheduled-p actual)
        (cancel-timer actual))
      (when (anvil-host--timer-scheduled-p peer)
        (cancel-timer peer))
      (anvil-host-reentrancy-test--force-clean nil nil))))

(ert-deftest anvil-host-reentrancy-tagged-idle-timer-is-recovered ()
  "A tagged constructor exit cannot orphan a repeating idle cleanup timer."
  (let ((anvil-host--cleanup-state
         (cons nil (make-hash-table :test #'eq)))
        (anvil-host--cleanup-timer nil)
        (anvil-host--cleanup-active nil)
        idle outcome)
    (unwind-protect
        (progn
          (cl-letf ((anvil-host--run-at-time-function
                     (lambda (&rest args)
                       (setq idle
                             (apply #'run-with-idle-timer
                                    30 t (nth 2 args) (nthcdr 3 args)))
                       (throw 'anvil-host-idle-constructor-exit :original))))
            (setq outcome
                  (catch 'anvil-host-idle-constructor-exit
                    (anvil-host--ensure-cleanup-timer)
                    :returned)))
          (should (eq outcome :original))
          (should (timerp idle))
          (should (eq (aref (anvil-host--resource-state) 2) idle))
          (should (memq idle timer-list))
          (should-not (memq idle timer-idle-list))
          (should-not (timer--repeat-delay idle))
          (should-not (timer--args idle))
          (anvil-host--cancel-cleanup-timer)
          (should-not (anvil-host--timer-scheduled-p idle)))
      (when (anvil-host--timer-scheduled-p idle)
        (cancel-timer idle))
      (anvil-host-reentrancy-test--force-clean nil nil))))

(ert-deftest anvil-host-reentrancy-constructor-advice-cannot-yield-output ()
  "Runtime constructor advice cannot rewrite filters before custody exists."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((real-symbol-make-process (symbol-function 'make-process))
         (foreign-buffer
          (generate-new-buffer " *anvil-host-advised-constructor*"))
         (canary
          (format "anvil-host-yield-canary-%s"
                  (random most-positive-fixnum)))
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         advice-called created leaked)
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq advice-called t
                           created (apply real-symbol-make-process args))
                     (set-process-buffer created foreign-buffer)
                     (set-process-filter
                      created
                      (lambda (_process chunk)
                        (setq leaked (concat leaked chunk))))
                     (accept-process-output created 0.2)
                     created)))
          (let ((result
                 (anvil-host--run
                  (format "printf %s" (shell-quote-argument canary))
                  'utf-8-unix nil 2)))
            (should (equal result (anvil-host-reentrancy-test--result 0 canary "")))
            (should-not advice-called)
            (should-not leaked)
            (should (string-empty-p
                     (with-current-buffer foreign-buffer
                       (buffer-string))))
            (anvil-host-reentrancy-test--assert-clean nil nil)))
      (when (and (processp created) (process-live-p created))
        (delete-process created))
      (when (buffer-live-p foreign-buffer)
        (kill-buffer foreign-buffer))
      (anvil-host-reentrancy-test--force-clean
       (and (processp created) (list created)) nil))))

(ert-deftest anvil-host-reentrancy-new-timer-helper-is-preserved ()
  "An exact callback timer does not confer custody over a returned helper."
  (let* ((real-run-at-time anvil-host--run-at-time-function)
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         actual helper helper-function helper-repeat helper-args)
    (unwind-protect
        (progn
          (cl-letf
              ((anvil-host--run-at-time-function
                (lambda (&rest args)
                  (setq actual (apply real-run-at-time args)
                        helper
                        (funcall real-run-at-time
                                 30 30 #'ignore 'helper-argument)
                        helper-function (timer--function helper)
                        helper-repeat (timer--repeat-delay helper)
                        helper-args (copy-tree (timer--args helper)))
                  helper)))
            (anvil-host--ensure-cleanup-timer))
          (should (eq (aref (anvil-host--resource-state) 2) actual))
          (should (anvil-host--timer-scheduled-p actual))
          (should (anvil-host--timer-scheduled-p helper))
          (should (eq (timer--function helper) helper-function))
          (should (equal (timer--repeat-delay helper) helper-repeat))
          (should (equal (timer--args helper) helper-args))
          (anvil-host--cancel-cleanup-timer)
          (should-not (anvil-host--timer-scheduled-p actual))
          (should (anvil-host--timer-scheduled-p helper)))
      (when (anvil-host--timer-scheduled-p actual)
        (cancel-timer actual))
      (when (anvil-host--timer-scheduled-p helper)
        (cancel-timer helper))
      (anvil-host-reentrancy-test--force-clean nil nil))))

(ert-deftest anvil-host-reentrancy-eof-advice-cannot-yield-output ()
  "Runtime EOF advice cannot reopen the private-output window."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((real-symbol-eof (symbol-function 'process-send-eof))
         (foreign-buffer
          (generate-new-buffer " *anvil-host-eof-advice*"))
         (canary
          (format "anvil-host-eof-canary-%s"
                  (random most-positive-fixnum)))
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         called seen result)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'process-send-eof)
                (lambda (process)
                  (setq called t)
                  (set-process-buffer process foreign-buffer)
                  (set-process-filter
                   process
                   (lambda (_process chunk)
                     (setq seen (concat seen chunk))))
                  (prog1
                      (funcall real-symbol-eof process)
                    (accept-process-output process 0.2)))))
            (setq result
                  (anvil-host--run
                   (format "cat >/dev/null; printf %s"
                           (shell-quote-argument canary))
                   'utf-8-unix nil 2)))
          (should (equal result (anvil-host-reentrancy-test--result 0 canary "")))
          (should-not called)
          (should-not seen)
          (should (string-empty-p
                   (with-current-buffer foreign-buffer
                     (buffer-string))))
          (anvil-host-reentrancy-test--assert-clean nil nil))
      (when (buffer-live-p foreign-buffer)
        (kill-buffer foreign-buffer))
      (anvil-host-reentrancy-test--force-clean nil nil))))

(ert-deftest anvil-host-reentrancy-wait-advice-cannot-yield-output ()
  "Runtime wait advice cannot replace filters during private execution."
  (skip-unless (memq system-type '(gnu/linux darwin)))
  (let* ((real-symbol-accept (symbol-function 'accept-process-output))
         (foreign-buffer
          (generate-new-buffer " *anvil-host-wait-advice*"))
         (canary
          (format "anvil-host-wait-canary-%s"
                  (random most-positive-fixnum)))
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         called seen result)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'accept-process-output)
                (lambda (&rest args)
                  (let ((process (car args)))
                    (when (processp process)
                      (setq called t)
                      (set-process-buffer process foreign-buffer)
                      (set-process-filter
                       process
                       (lambda (_process chunk)
                         (setq seen (concat seen chunk)))))
                    (apply real-symbol-accept args)))))
            (setq result
                  (anvil-host--run
                   (format "sleep 0.05; printf %s"
                           (shell-quote-argument canary))
                   'utf-8-unix nil 2)))
          (should (equal result (anvil-host-reentrancy-test--result 0 canary "")))
          (should-not called)
          (should-not seen)
          (should (string-empty-p
                   (with-current-buffer foreign-buffer
                     (buffer-string))))
          (anvil-host-reentrancy-test--assert-clean nil nil))
      (when (buffer-live-p foreign-buffer)
        (kill-buffer foreign-buffer))
      (anvil-host-reentrancy-test--force-clean nil nil))))

(ert-deftest anvil-host-reentrancy-runtime-timer-advice-is-bypassed ()
  "Runtime advice cannot create opaque repeating cleanup wrappers."
  (let* ((real-run-at-time (symbol-function 'run-at-time))
         (before (anvil-host--scheduled-timers))
         (anvil-host--cleanup-state
          (cons nil (make-hash-table :test #'eq)))
         (anvil-host--cleanup-timer nil)
         (anvil-host--cleanup-active nil)
         advice-called first second actual)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'run-at-time)
                (lambda (_time _repeat callback &rest args)
                  (let ((first-wrapper
                         (lambda (&rest callback-args)
                           (apply callback callback-args)))
                        (second-wrapper
                         (lambda (&rest callback-args)
                           (apply callback callback-args))))
                    (setq advice-called t
                          first
                          (apply real-run-at-time
                                 30 30 first-wrapper args)
                          second
                          (apply real-run-at-time
                                 30 30 second-wrapper args))
                    first))))
            (anvil-host--ensure-cleanup-timer))
          (setq actual (aref (anvil-host--resource-state) 2))
          (should-not advice-called)
          (should (timerp actual))
          (should (anvil-host--timer-scheduled-p actual))
          (should
           (equal
            (list actual)
            (cl-remove-if
             (lambda (timer) (memq timer before))
             (anvil-host--scheduled-timers))))
          (anvil-host--cancel-cleanup-timer)
          (should-not (anvil-host--timer-scheduled-p actual)))
      (dolist (timer (list actual first second))
        (when (anvil-host--timer-scheduled-p timer)
          (cancel-timer timer)))
      (anvil-host-reentrancy-test--force-clean nil nil))))

(ert-deftest anvil-host-reentrancy-capture-strips-preexisting-advice ()
  "Primitive capture unwraps advice already installed on target symbols."
  (let ((wrapper
         (lambda (original &rest args)
           (apply original args))))
    (advice-add 'make-process :around wrapper)
    (advice-add 'run-at-time :around wrapper)
    (unwind-protect
        (progn
          (should
           (eq (anvil-host--capture-primitive 'make-process)
               anvil-host--make-process-primitive))
          (should
           (eq (anvil-host--capture-primitive 'run-at-time)
               anvil-host--run-at-time-function)))
      (advice-remove 'make-process wrapper)
      (advice-remove 'run-at-time wrapper))))

(ert-deftest anvil-host-reentrancy-zz-global-state-remains-clean ()
  "Focused and full runs leave no hidden host custody state."
  (should (zerop (anvil-host--retired-count)))
  (should-not (anvil-host--cleanup-active-p))
  (should-not (aref (anvil-host--resource-state) 2))
  (should-not (aref (anvil-host--resource-state) 3))
  (should-not anvil-host--cleanup-active)
  (should-not anvil-host--cleanup-timer)
  (should-not (anvil-host--cleanup-timer-scheduled-p)))

(provide 'anvil-host-reentrancy-test)
;;; anvil-host-reentrancy-test.el ends here
