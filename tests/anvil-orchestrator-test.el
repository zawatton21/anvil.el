;;; anvil-orchestrator-test.el --- Tests for anvil-orchestrator -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for `anvil-orchestrator'.  A lightweight `test'
;; provider spawned via /bin/sh + printf emits fake stream-json so
;; no real claude CLI is needed.  One live smoke test at the bottom
;; exercises the real claude CLI under ANVIL_ALLOW_LIVE=1.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'anvil-state)
(require 'anvil-orchestrator)
(require 'anvil-test-fixtures)

;;;; --- fixtures -----------------------------------------------------------

(defvar anvil-orchestrator-test--stream-json
  "{\"type\":\"assistant\",\"message\":{\"content\":\"working\"}}\n{\"type\":\"result\",\"result\":\"stub task done\",\"total_cost_usd\":0.00042,\"usage\":{\"input_tokens\":123,\"output_tokens\":45,\"cache_read_input_tokens\":0}}"
  "Pre-baked stream-json output for the `test' provider.")

(defun anvil-orchestrator-test--register-stub (&optional stream-json exit-code)
  "Register / overwrite the `test' provider with STREAM-JSON output.
EXIT-CODE overrides the sh exit status (default 0).  The argv is
built by `anvil-test-fixtures-stub-cmd'."
  (let ((payload (or stream-json anvil-orchestrator-test--stream-json)))
    (anvil-orchestrator-register-provider
     'test
     :cli "sh"
     :version-check (lambda () t)
     :build-cmd (lambda (_task)
                  (anvil-test-fixtures-stub-cmd payload exit-code))
     :parse-output #'anvil-orchestrator--claude-parse-output
     :supports-tool-use             t
     :supports-worktree             nil
     :supports-budget               nil
     :supports-system-prompt-append nil
     :default-model                 "stub"
     :cost-estimator (lambda (_task) 0.0))))

(defun anvil-orchestrator-test--register-slow (sleep-sec)
  "Register a slow `slow' provider that sleeps SLEEP-SEC before printing."
  (anvil-orchestrator-register-provider
   'slow
   :cli "sh"
   :version-check (lambda () t)
   :build-cmd (lambda (_task)
                (anvil-test-fixtures-sleep-stub-cmd
                 sleep-sec anvil-orchestrator-test--stream-json))
   :parse-output #'anvil-orchestrator--claude-parse-output
   :default-model "slow"))

(defmacro anvil-orchestrator-test--with-fresh (&rest body)
  "Run BODY inside a freshly initialised orchestrator + state DB.
Uses `anvil-test-fixtures-kill-processes' to SIGKILL any stub
subprocess left behind by BODY before removing the work dir."
  (declare (indent 0))
  `(let ((anvil-state-db-path (make-temp-file "anvil-orch-st-" nil ".db"))
         (anvil-state--db nil)
         (anvil-orchestrator-work-dir
          (make-temp-file "anvil-orch-work-" t))
         (anvil-orchestrator--tasks (make-hash-table :test 'equal))
         (anvil-orchestrator--running (make-hash-table :test 'equal))
         (anvil-orchestrator--batches (make-hash-table :test 'equal))
         (anvil-orchestrator--queue nil)
         (anvil-orchestrator--pump-timer nil)
         (anvil-orchestrator-poll-interval-sec 0.1))
     (unwind-protect
         (progn
           (anvil-state-enable)
           (anvil-orchestrator-test--register-stub)
           ,@body)
       (anvil-orchestrator--cancel-pump-timer)
       (anvil-test-fixtures-kill-processes
        anvil-orchestrator--running)
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path))
       (ignore-errors
         (delete-directory anvil-orchestrator-work-dir t)))))

(defun anvil-orchestrator-test--wait-batch (batch-id &optional max-sec)
  "Block until BATCH-ID is terminal or MAX-SEC (default 10s) elapses."
  (let ((deadline (+ (float-time) (or max-sec 10))))
    (while (and (not (anvil-orchestrator--batch-terminal-p batch-id))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))))

;;;; --- provider registration ---------------------------------------------

(ert-deftest anvil-orchestrator-test-claude-provider-registered ()
  "Built-in `claude' provider is registered on load."
  (let ((prov (anvil-orchestrator--provider 'claude)))
    (should (anvil-orchestrator-provider-p prov))
    (should (equal "claude" (anvil-orchestrator-provider-cli prov)))
    (should (anvil-orchestrator-provider-supports-tool-use prov))))

(ert-deftest anvil-orchestrator-test-unknown-provider-errors ()
  (should-error (anvil-orchestrator--provider 'no-such)
                :type 'user-error))

;;;; --- validation --------------------------------------------------------

(ert-deftest anvil-orchestrator-test-submit-rejects-empty ()
  (anvil-orchestrator-test--with-fresh
    (should-error (anvil-orchestrator-submit nil) :type 'user-error)))

(ert-deftest anvil-orchestrator-test-submit-rejects-duplicate-name ()
  (anvil-orchestrator-test--with-fresh
    (should-error
     (anvil-orchestrator-submit
      (list (list :name "a" :provider 'test :prompt "hi")
            (list :name "a" :provider 'test :prompt "yo")))
     :type 'user-error)))

(ert-deftest anvil-orchestrator-test-submit-rejects-missing-prompt ()
  (anvil-orchestrator-test--with-fresh
    (should-error
     (anvil-orchestrator-submit
      (list (list :name "a" :provider 'test)))
     :type 'user-error)))

(ert-deftest anvil-orchestrator-test-submit-rejects-batch-over-cap ()
  (anvil-orchestrator-test--with-fresh
    (let ((anvil-orchestrator-batch-max-tasks 2))
      (should-error
       (anvil-orchestrator-submit
        (list (list :name "a" :provider 'test :prompt "x")
              (list :name "b" :provider 'test :prompt "y")
              (list :name "c" :provider 'test :prompt "z")))
       :type 'user-error))))

(ert-deftest anvil-orchestrator-test-submit-rejects-batch-budget ()
  (anvil-orchestrator-test--with-fresh
    (let ((anvil-orchestrator-batch-budget-usd-total 0.50))
      (should-error
       (anvil-orchestrator-submit
        (list (list :name "a" :provider 'test :prompt "x" :budget-usd 0.40)
              (list :name "b" :provider 'test :prompt "y" :budget-usd 0.40)))
       :type 'user-error))))

;;;; --- happy path --------------------------------------------------------

(ert-deftest anvil-orchestrator-test-submit-and-collect-one-task ()
  "Single task runs to done and surfaces summary + cost."
  (anvil-orchestrator-test--with-fresh
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "single" :provider 'test
                               :prompt "hello"))))
           (_     (anvil-orchestrator-test--wait-batch batch))
           (results (anvil-orchestrator-collect batch)))
      (should (= 1 (length results)))
      (let ((r (car results)))
        (should (eq 'done (plist-get r :status)))
        (should (equal "single" (plist-get r :name)))
        (should (equal "stub task done" (plist-get r :summary)))
        (should (= 0.00042 (plist-get r :cost-usd)))))))

(ert-deftest anvil-orchestrator-test-status-batch-counts ()
  "`orchestrator-status' on a batch id reports counts + slim task list."
  (anvil-orchestrator-test--with-fresh
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "a" :provider 'test :prompt "p")
                         (list :name "b" :provider 'test :prompt "q")))))
      (anvil-orchestrator-test--wait-batch batch)
      (let ((st (anvil-orchestrator-status batch)))
        (should (equal batch (plist-get st :batch-id)))
        (should (= 2 (plist-get st :total)))
        (should (= 2 (plist-get st :done)))
        (should (= 0 (plist-get st :failed)))
        (should (= 2 (length (plist-get st :tasks))))))))

(ert-deftest anvil-orchestrator-test-collect-wait-blocks-until-done ()
  (anvil-orchestrator-test--with-fresh
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "a" :provider 'test :prompt "p"))))
           (results (anvil-orchestrator-collect batch :wait t
                                                :max-wait-sec 10)))
      (should (eq 'done (plist-get (car results) :status))))))

;;;; --- failure / retry ---------------------------------------------------

(ert-deftest anvil-orchestrator-test-non-zero-exit-marks-failed ()
  "Provider exit != 0 surfaces as status=failed with error string."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-stub "" 3)
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "bad" :provider 'test :prompt "p")))))
      (anvil-orchestrator-test--wait-batch batch)
      (let ((r (car (anvil-orchestrator-collect batch))))
        (should (eq 'failed (plist-get r :status)))
        (should (string-match-p "exit" (plist-get r :error)))))))

(ert-deftest anvil-orchestrator-test-retry-creates-new-task ()
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-stub "" 1)
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "flaky" :provider 'test
                               :prompt "p" :auto-retry nil)))))
      (anvil-orchestrator-test--wait-batch batch)
      (let* ((orig (car (anvil-orchestrator-collect batch)))
             (new-id (anvil-orchestrator-retry (plist-get orig :id))))
        (should (stringp new-id))
        (should-not (equal new-id (plist-get orig :id)))
        (let ((new-task (anvil-orchestrator--task-get new-id)))
          (should (equal (plist-get orig :id) (plist-get new-task :retry-of))))))))

;;;; --- concurrency + queuing ---------------------------------------------

(ert-deftest anvil-orchestrator-test-concurrency-cap-respected ()
  "With cap=1, only one task runs at a time."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-slow 0.3)
    (let ((anvil-orchestrator-concurrency 1)
          (anvil-orchestrator-per-provider-concurrency
           '((slow . 1))))
      (let ((batch (anvil-orchestrator-submit
                    (list (list :name "a" :provider 'slow :prompt "x")
                          (list :name "b" :provider 'slow :prompt "y")))))
        ;; give the pump a tick
        (accept-process-output nil 0.2)
        (should (<= (anvil-orchestrator--running-count) 1))
        (anvil-orchestrator-test--wait-batch batch 15)
        (let ((st (anvil-orchestrator-status batch)))
          (should (= 2 (plist-get st :done))))))))

;;;; --- cancel ------------------------------------------------------------

(ert-deftest anvil-orchestrator-test-cancel-queued-task ()
  "Cancel before spawn sets status=cancelled without spawning."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-slow 5)
    (let ((anvil-orchestrator-concurrency 1))
      ;; Running one slow task to block the pool
      (let* ((batch (anvil-orchestrator-submit
                     (list (list :name "blocker" :provider 'slow :prompt "x")
                           (list :name "waits"   :provider 'slow :prompt "y"))))
             (ids (gethash batch anvil-orchestrator--batches))
             (queued-id (cadr ids)))
        (accept-process-output nil 0.1)
        (should (anvil-orchestrator-cancel queued-id))
        (should (eq 'cancelled
                    (plist-get (anvil-orchestrator--task-get queued-id)
                               :status)))
        ;; cleanup: cancel the running one too so fresh macro can clean up
        (anvil-orchestrator-cancel (car ids))
        (anvil-orchestrator-test--wait-batch batch 10)))))

;;;; --- state persistence -------------------------------------------------

(ert-deftest anvil-orchestrator-test-state-round-trip ()
  "Completed tasks read back through anvil-state."
  (anvil-orchestrator-test--with-fresh
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "persist" :provider 'test :prompt "p")))))
      (anvil-orchestrator-test--wait-batch batch)
      ;; blow away in-memory state, keep on-disk DB
      (clrhash anvil-orchestrator--tasks)
      (clrhash anvil-orchestrator--batches)
      (setq anvil-orchestrator--queue nil)
      (anvil-orchestrator--restore-from-state)
      (let ((id (car (gethash batch anvil-orchestrator--batches))))
        (should id)
        (should (eq 'done (plist-get (anvil-orchestrator--task-get id)
                                     :status)))))))

(ert-deftest anvil-orchestrator-test-state-running-becomes-failed-on-restore ()
  "Running tasks are reset to failed on restore (daemon restart interrupt)."
  (anvil-orchestrator-test--with-fresh
    (let* ((id (anvil-orchestrator--uuid))
           (task (list :id id :batch-id "b1" :name "orphan"
                       :provider 'test :prompt "x" :status 'running
                       :submitted-at (float-time))))
      (anvil-orchestrator--persist task)
      (clrhash anvil-orchestrator--tasks)
      (anvil-orchestrator--restore-from-state)
      (let ((restored (anvil-orchestrator--task-get id)))
        (should (eq 'failed (plist-get restored :status)))
        (should (string-match-p "daemon restart"
                                (plist-get restored :error)))))))

;;;; --- provider-common helpers ------------------------------------------

(ert-deftest anvil-orchestrator-test-truncate-summary-clamps ()
  "Summary over the cap is clamped + suffixed with an ellipsis."
  (let* ((anvil-orchestrator-summary-max-chars 10)
         (result (anvil-orchestrator--truncate-summary
                  (make-string 25 ?x))))
    (should (equal (concat (make-string 10 ?x) "…") result))))

(ert-deftest anvil-orchestrator-test-truncate-summary-trims-whitespace ()
  (should (equal "hello"
                 (anvil-orchestrator--truncate-summary
                  "  hello  \n"))))

(ert-deftest anvil-orchestrator-test-truncate-summary-handles-nil-and-empty ()
  (should-not (anvil-orchestrator--truncate-summary nil))
  (should-not (anvil-orchestrator--truncate-summary ""))
  (should-not (anvil-orchestrator--truncate-summary "   \n\t  ")))

(ert-deftest anvil-orchestrator-test-stderr-retry-code-http ()
  "HTTP 429 / 5xx on stderr maps to an integer retry code."
  (let ((err (make-temp-file "anvil-retry-")))
    (unwind-protect
        (progn
          (write-region "HTTP 502 Bad Gateway\n" nil err nil 'silent)
          (should (equal 502
                         (anvil-orchestrator--stderr-retry-code err 1))))
      (delete-file err))))

(ert-deftest anvil-orchestrator-test-stderr-retry-code-network ()
  "Generic network keywords on stderr map to `network'."
  (let ((err (make-temp-file "anvil-retry-")))
    (unwind-protect
        (progn
          (write-region "connect ECONNRESET something\n" nil err nil 'silent)
          (should (equal 'network
                         (anvil-orchestrator--stderr-retry-code err 1))))
      (delete-file err))))

(ert-deftest anvil-orchestrator-test-stderr-retry-code-none ()
  "No retryable pattern → nil.  Also nil for exit 0 or missing path."
  (let ((err (make-temp-file "anvil-retry-")))
    (unwind-protect
        (progn
          (write-region "some unrelated message\n" nil err nil 'silent)
          (should-not (anvil-orchestrator--stderr-retry-code err 1))
          ;; Exit 0 short-circuits the scan.
          (write-region "HTTP 503 should be ignored\n" nil err nil 'silent)
          (should-not (anvil-orchestrator--stderr-retry-code err 0)))
      (delete-file err)))
  ;; Missing path is a silent nil.
  (should-not (anvil-orchestrator--stderr-retry-code
               "/does/not/exist/stderr.txt" 1)))

(ert-deftest anvil-orchestrator-test-argv-append-when ()
  "Appends ITEMS only when the predicate is non-nil; does not mutate CMD."
  (let ((cmd '("a" "b")))
    (should (equal '("a" "b" "c")
                   (anvil-orchestrator--argv-append-when cmd t "c")))
    (should (equal '("a" "b")
                   (anvil-orchestrator--argv-append-when cmd nil "c")))
    ;; Original intact.
    (should (equal '("a" "b") cmd)))
  (should (equal '("x" "y" "z")
                 (anvil-orchestrator--argv-append-when '("x") t "y" "z"))))

;;;; --- parser -------------------------------------------------------------

(ert-deftest anvil-orchestrator-test-parse-stream-json-extracts-usage ()
  (let* ((tmp-out (make-temp-file "anvil-orch-out-" nil ".jsonl"))
         (tmp-err (make-temp-file "anvil-orch-err-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file tmp-out
            (insert anvil-orchestrator-test--stream-json))
          (let ((parsed (anvil-orchestrator--claude-parse-output
                         tmp-out tmp-err 0)))
            (should (equal "stub task done" (plist-get parsed :summary)))
            (should (= 0.00042 (plist-get parsed :cost-usd)))
            (should (= 123 (plist-get (plist-get parsed :cost-tokens)
                                      :input)))))
      (delete-file tmp-out)
      (delete-file tmp-err))))

(ert-deftest anvil-orchestrator-test-parse-stream-json-truncates-long-summary ()
  (let* ((tmp-out (make-temp-file "anvil-orch-out-" nil ".jsonl"))
         (tmp-err (make-temp-file "anvil-orch-err-" nil ".txt"))
         (anvil-orchestrator-summary-max-chars 20)
         (long (make-string 500 ?a)))
    (unwind-protect
        (progn
          (with-temp-file tmp-out
            (insert (json-encode
                     `(:type "result" :result ,long
                             :total_cost_usd 0.01))))
          (let* ((parsed (anvil-orchestrator--claude-parse-output
                          tmp-out tmp-err 0))
                 (s (plist-get parsed :summary)))
            (should (= 21 (length s)))))
      (delete-file tmp-out)
      (delete-file tmp-err))))

;;;; --- Phase 1b: worktree isolation --------------------------------------

(ert-deftest anvil-orchestrator-test-claude-build-cmd-adds-worktree ()
  "`:_worktree-name' surfaces as `--worktree NAME' in the claude cmd."
  (let* ((task (list :name "demo" :provider 'claude
                     :prompt "hi" :_worktree-name "anvil-orch-demo"))
         (cmd  (anvil-orchestrator--claude-build-cmd task)))
    (should (member "--worktree" cmd))
    (should (member "anvil-orch-demo" cmd))))

(ert-deftest anvil-orchestrator-test-claude-build-cmd-no-worktree-by-default ()
  (let* ((task (list :name "demo" :provider 'claude :prompt "hi"))
         (cmd  (anvil-orchestrator--claude-build-cmd task)))
    (should-not (member "--worktree" cmd))))

(ert-deftest anvil-orchestrator-test-should-worktree-requires-git-repo ()
  "Non-git cwd → never triggers worktree handling."
  (let* ((tmp (make-temp-file "anvil-orch-no-git-" t))
         (task (list :name "x" :provider 'claude :prompt "p" :cwd tmp)))
    (unwind-protect
        (should-not
         (anvil-orchestrator--should-worktree
          task (anvil-orchestrator--provider 'claude)))
      (delete-directory tmp t))))

(ert-deftest anvil-orchestrator-test-should-worktree-opt-out ()
  "`:no-worktree t' suppresses worktree handling even in a repo."
  (let* ((repo (anvil-orchestrator-test--make-repo))
         (task (list :name "x" :provider 'claude :prompt "p"
                     :cwd repo :no-worktree t)))
    (unwind-protect
        (should-not
         (anvil-orchestrator--should-worktree
          task (anvil-orchestrator--provider 'claude)))
      (delete-directory repo t))))

(ert-deftest anvil-orchestrator-test-should-worktree-detects-repo ()
  "cwd inside a git repo + claude provider → worktree handling on."
  (let* ((repo (anvil-orchestrator-test--make-repo))
         (task (list :name "x" :provider 'claude :prompt "p" :cwd repo)))
    (unwind-protect
        (should (anvil-orchestrator--should-worktree
                 task (anvil-orchestrator--provider 'claude)))
      (delete-directory repo t))))

(ert-deftest anvil-orchestrator-test-worktree-name-sanitises ()
  (should (equal "anvil-orch-foo-bar-baz"
                 (anvil-orchestrator--worktree-name-for
                  (list :name "foo/bar baz")))))

(defalias 'anvil-orchestrator-test--make-repo
  'anvil-test-fixtures-make-repo
  "Use the shared test fixture helper.")

;;;; --- Phase 1b: :depends-on DAG -----------------------------------------

(ert-deftest anvil-orchestrator-test-submit-rejects-unknown-dep ()
  (anvil-orchestrator-test--with-fresh
    (should-error
     (anvil-orchestrator-submit
      (list (list :name "a" :provider 'test :prompt "x"
                  :depends-on '("ghost"))))
     :type 'user-error)))

(ert-deftest anvil-orchestrator-test-submit-rejects-dep-cycle ()
  (anvil-orchestrator-test--with-fresh
    (should-error
     (anvil-orchestrator-submit
      (list (list :name "a" :provider 'test :prompt "x"
                  :depends-on '("b"))
            (list :name "b" :provider 'test :prompt "y"
                  :depends-on '("a"))))
     :type 'user-error)))

(ert-deftest anvil-orchestrator-test-dep-runs-after-parent-done ()
  "Child task waits in queue until parent reaches `done'."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-slow 0.3)
    ;; Register parent as 'slow' + child as 'test'
    (let ((batch (anvil-orchestrator-submit
                  (list (list :name "parent" :provider 'slow :prompt "p")
                        (list :name "child"  :provider 'test :prompt "c"
                              :depends-on '("parent"))))))
      (anvil-orchestrator-test--wait-batch batch 15)
      (let* ((st (anvil-orchestrator-status batch))
             (child (cl-find-if
                     (lambda (t0) (equal "child" (plist-get t0 :name)))
                     (plist-get st :tasks))))
        (should (eq 'done (plist-get child :status)))
        (should (= 2 (plist-get st :done)))))))

(ert-deftest anvil-orchestrator-test-dep-propagates-failure ()
  "If a dep fails, its dependents also flip to `failed'."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-stub "" 2) ; failing test provider
    (let ((batch (anvil-orchestrator-submit
                  (list (list :name "bad"   :provider 'test :prompt "p"
                              :auto-retry nil)
                        (list :name "child" :provider 'test :prompt "c"
                              :auto-retry nil
                              :depends-on '("bad"))))))
      (anvil-orchestrator-test--wait-batch batch 10)
      (let* ((st (anvil-orchestrator-status batch))
             (child (cl-find-if
                     (lambda (t0) (equal "child" (plist-get t0 :name)))
                     (plist-get st :tasks))))
        (should (eq 'failed (plist-get child :status)))
        (should (string-match-p "dependency bad" (plist-get child :error)))))))

;;;; --- Phase 1b: stdout / stderr overflow --------------------------------

(ert-deftest anvil-orchestrator-test-overflow-leaves-small-files-alone ()
  (let* ((path (make-temp-file "anvil-orch-small-"))
         (anvil-orchestrator-output-size-cap 1024))
    (unwind-protect
        (progn
          (with-temp-file path (insert (make-string 200 ?a)))
          (should-not (anvil-orchestrator--truncate-output-file path))
          (should (= 200 (file-attribute-size (file-attributes path)))))
      (delete-file path))))

(ert-deftest anvil-orchestrator-test-overflow-truncates-large-files ()
  (let* ((path (make-temp-file "anvil-orch-big-"))
         (anvil-orchestrator-output-size-cap 400)
         (anvil-orchestrator-output-head-bytes 100)
         (anvil-orchestrator-output-tail-bytes 100))
    (unwind-protect
        (progn
          (with-temp-file path
            (dotimes (i 10)
              (insert (format "%05d" i) (make-string 95 ?a) "\n")))
          (let* ((orig (file-attribute-size (file-attributes path)))
                 (returned (anvil-orchestrator--truncate-output-file path)))
            (should (equal orig returned))
            (let ((body (with-temp-buffer
                          (insert-file-contents-literally path)
                          (buffer-string))))
              (should (string-match-p "truncated" body))
              (should (string-match-p "\\`00000" body))
              (should (< (length body)
                         (+ 100 100 400 4096))))))
      (delete-file path))))

;;;; --- Phase 2: aider provider -------------------------------------------

(ert-deftest anvil-orchestrator-test-aider-provider-registered ()
  "Built-in `aider' provider is registered on load."
  (let ((prov (anvil-orchestrator--provider 'aider)))
    (should (anvil-orchestrator-provider-p prov))
    (should (equal "aider" (anvil-orchestrator-provider-cli prov)))
    (should (anvil-orchestrator-provider-supports-tool-use prov))
    (should-not (anvil-orchestrator-provider-supports-worktree prov))
    (should-not (anvil-orchestrator-provider-supports-budget prov))
    (should (equal "openai/gpt-4o-mini"
                   (anvil-orchestrator-provider-default-model prov)))))

(ert-deftest anvil-orchestrator-test-aider-build-cmd-basic ()
  "`aider' build-cmd emits model, message, and non-interactive flags."
  (let* ((anvil-orchestrator-aider-default-model "openai/gpt-4o-mini")
         (cl-letf-bind
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (_) "/usr/bin/aider")))
            (anvil-orchestrator--aider-build-cmd
             (list :prompt "refactor foo"
                   :model "anthropic/claude-3-5-sonnet-latest")))))
    (should (equal "/usr/bin/aider"     (nth 0 cl-letf-bind)))
    (should (member "--model"           cl-letf-bind))
    (should (member "anthropic/claude-3-5-sonnet-latest" cl-letf-bind))
    (should (member "--message"         cl-letf-bind))
    (should (member "refactor foo"      cl-letf-bind))
    (should (member "--yes-always"      cl-letf-bind))
    (should (member "--no-stream"       cl-letf-bind))
    (should (member "--no-check-update" cl-letf-bind))
    (should (member "--no-pretty"       cl-letf-bind))
    (should-not (member "--no-auto-commits" cl-letf-bind))
    (should-not (member "--subtree-only"    cl-letf-bind))))

(ert-deftest anvil-orchestrator-test-aider-build-cmd-default-model ()
  "`aider' build-cmd falls back to `anvil-orchestrator-aider-default-model'."
  (let ((anvil-orchestrator-aider-default-model "openai/gpt-4o-mini"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) "/usr/bin/aider")))
      (let ((cmd (anvil-orchestrator--aider-build-cmd
                  (list :prompt "x"))))
        (should (member "openai/gpt-4o-mini" cmd))))))

(ert-deftest anvil-orchestrator-test-aider-build-cmd-files-and-readonly ()
  "`:files' become positional args; `:read-only-files' become `--read' pairs."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_) "/usr/bin/aider")))
    (let ((cmd (anvil-orchestrator--aider-build-cmd
               (list :prompt "p"
                     :model  "openai/gpt-4o"
                     :files  '("a.el" "b.el")
                     :read-only-files '("CLAUDE.md")
                     :subtree-only t
                     :no-auto-commits t))))
      (should (member "--subtree-only"    cmd))
      (should (member "--no-auto-commits" cmd))
      ;; --read <path> pairs
      (let ((tail (member "--read" cmd)))
        (should tail)
        (should (equal "CLAUDE.md" (cadr tail))))
      ;; File list sits at the end
      (should (equal '("a.el" "b.el") (last cmd 2))))))

(ert-deftest anvil-orchestrator-test-aider-build-cmd-extra-args ()
  "`anvil-orchestrator-aider-extra-args' is appended before positionals."
  (let ((anvil-orchestrator-aider-extra-args '("--verbose" "--no-gitignore")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) "/usr/bin/aider")))
      (let ((cmd (anvil-orchestrator--aider-build-cmd
                 (list :prompt "x" :model "openai/gpt-4o"
                       :files '("f.el")))))
        (should (member "--verbose"       cmd))
        (should (member "--no-gitignore"  cmd))
        ;; Positional still last.
        (should (equal "f.el" (car (last cmd))))))))

(ert-deftest anvil-orchestrator-test-aider-parse-output-commit-and-summary ()
  "Parser extracts last commit SHA + tail summary from aider stdout."
  (let ((path (make-temp-file "aider-out-")))
    (unwind-protect
        (progn
          (write-region
           (concat
            "Aider v0.77.0\n"
            "Model: openai/gpt-4o-mini\n"
            "Git repo: .git with 5 files\n"
            "Tokens: 1.2k sent, 0.3k received\n"
            "Cost: $0.0002 message, $0.0015 session\n"
            "\n"
            "Applied edit to src/foo.el\n"
            "diff --git a/src/foo.el b/src/foo.el\n"
            "--- a/src/foo.el\n"
            "+++ b/src/foo.el\n"
            "@@ -1,1 +1,1 @@\n"
            "-old\n"
            "+new\n"
            "Commit abc1234 refactor foo\n"
            "\n"
            "I renamed the helper and updated the caller.\n"
            "The change is backward compatible.\n")
           nil path nil 'silent)
          (let ((parsed (anvil-orchestrator--aider-parse-output path nil 0)))
            (should (equal "abc1234" (plist-get parsed :commit-sha)))
            (should (stringp (plist-get parsed :summary)))
            (should (string-match-p "renamed the helper"
                                    (plist-get parsed :summary)))
            (should-not (string-match-p "diff --git"
                                        (plist-get parsed :summary)))
            (should-not (string-match-p "Tokens:"
                                        (plist-get parsed :summary)))
            (should-not (plist-get parsed :cost-usd))))
      (delete-file path))))

(ert-deftest anvil-orchestrator-test-aider-parse-output-takes-last-commit ()
  "Parser keeps the last `Commit <sha>' when multiple appear."
  (let ((path (make-temp-file "aider-out-")))
    (unwind-protect
        (progn
          (write-region
           "Commit aaaaaaa first change\n\ndescription one\nCommit bbbbbbb later\n\nfinal description\n"
           nil path nil 'silent)
          (let ((parsed (anvil-orchestrator--aider-parse-output path nil 0)))
            (should (equal "bbbbbbb" (plist-get parsed :commit-sha)))
            (should (string-match-p "final description"
                                    (plist-get parsed :summary)))))
      (delete-file path))))

(ert-deftest anvil-orchestrator-test-aider-parse-output-retry-on-429 ()
  "Parser maps 429 on stderr to :auto-retry-code 429 when no summary."
  (let ((out (make-temp-file "aider-out-"))
        (err (make-temp-file "aider-err-")))
    (unwind-protect
        (progn
          (write-region "" nil out nil 'silent)
          (write-region "HTTP 429 Too Many Requests\n" nil err nil 'silent)
          (let ((parsed (anvil-orchestrator--aider-parse-output out err 1)))
            (should-not (plist-get parsed :summary))
            (should (equal 429 (plist-get parsed :auto-retry-code)))))
      (delete-file out)
      (delete-file err))))

(ert-deftest anvil-orchestrator-test-aider-cost-estimator ()
  "`anvil-orchestrator--aider-cost' returns numbers in sensible ranges."
  (let ((small (anvil-orchestrator--aider-cost
                (list :prompt "hi" :model "openai/gpt-4o-mini")))
        (big   (anvil-orchestrator--aider-cost
                (list :prompt (make-string 4000 ?x)
                      :model "anthropic/claude-3-opus-latest")))
        (local (anvil-orchestrator--aider-cost
                (list :prompt "hi" :model "ollama/llama3"))))
    (should (and (numberp small) (>= small 0) (< small 0.01)))
    (should (and (numberp big)   (> big small)))
    (should (= 0 local))))

(ert-deftest anvil-orchestrator-test-aider-end-to-end-stub ()
  "End-to-end: stub aider via sh routed through the aider parser.
Registered under a distinct provider symbol so the built-in
`aider' descriptor isn't overwritten for the rest of the suite."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-register-provider
     'aider-stub
     :cli "sh"
     :version-check (lambda () t)
     :build-cmd (lambda (_task)
                  (list "sh" "-c"
                        (concat
                         "printf 'Aider v0.77.0\\n"
                         "Model: openai/gpt-4o-mini\\n"
                         "Tokens: 1k sent\\n"
                         "Applied edit to f.el\\n"
                         "Commit deadbee stub change\\n\\n"
                         "Made the requested tweak to f.el.\\n'")))
     :parse-output #'anvil-orchestrator--aider-parse-output
     :supports-tool-use t
     :supports-worktree nil
     :default-model "openai/gpt-4o-mini"
     :cost-estimator #'anvil-orchestrator--aider-cost)
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "aider-stub"
                               :provider 'aider-stub
                               :prompt   "tweak f.el"
                               :no-worktree t)))))
      (anvil-orchestrator-test--wait-batch batch 5)
      (let ((r (car (anvil-orchestrator-collect batch))))
        (should (eq 'done (plist-get r :status)))
        (should (equal "deadbee" (plist-get r :commit-sha)))
        (should (stringp (plist-get r :summary)))
        (should (string-match-p "requested tweak"
                                (plist-get r :summary)))))))

;;;; --- Phase 3: gemini provider ------------------------------------------

(ert-deftest anvil-orchestrator-test-gemini-provider-registered ()
  "Built-in `gemini' provider is registered on load."
  (let ((prov (anvil-orchestrator--provider 'gemini)))
    (should (anvil-orchestrator-provider-p prov))
    (should (equal "gemini" (anvil-orchestrator-provider-cli prov)))
    (should (anvil-orchestrator-provider-supports-tool-use prov))
    (should-not (anvil-orchestrator-provider-supports-worktree prov))
    (should-not (anvil-orchestrator-provider-supports-budget prov))
    (should-not (anvil-orchestrator-provider-supports-system-prompt-append
                 prov))
    (should (equal "gemini-2.5-flash"
                   (anvil-orchestrator-provider-default-model prov)))))

(ert-deftest anvil-orchestrator-test-gemini-build-cmd-basic ()
  "`gemini' build-cmd emits -m MODEL and -p PROMPT."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_) "/usr/bin/gemini")))
    (let ((cmd (anvil-orchestrator--gemini-build-cmd
                (list :prompt "summarize readme"
                      :model "gemini-2.5-pro"))))
      (should (equal "/usr/bin/gemini"  (nth 0 cmd)))
      (should (equal "-m"               (nth 1 cmd)))
      (should (equal "gemini-2.5-pro"   (nth 2 cmd)))
      (should (equal "-p"               (nth 3 cmd)))
      (should (equal "summarize readme" (nth 4 cmd)))
      (should-not (member "--yolo" cmd)))))

(ert-deftest anvil-orchestrator-test-gemini-build-cmd-default-model ()
  "`gemini' build-cmd falls back to `anvil-orchestrator-gemini-default-model'."
  (let ((anvil-orchestrator-gemini-default-model "gemini-2.5-flash"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) "/usr/bin/gemini")))
      (let ((cmd (anvil-orchestrator--gemini-build-cmd
                  (list :prompt "x"))))
        (should (member "gemini-2.5-flash" cmd))))))

(ert-deftest anvil-orchestrator-test-gemini-build-cmd-yolo ()
  "`:yolo' in task (or defcustom) adds `--yolo' to argv."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_) "/usr/bin/gemini")))
    (let ((cmd (anvil-orchestrator--gemini-build-cmd
                (list :prompt "p" :model "gemini-2.5-flash" :yolo t))))
      (should (member "--yolo" cmd)))
    (let ((anvil-orchestrator-gemini-yolo t))
      (let ((cmd (anvil-orchestrator--gemini-build-cmd
                  (list :prompt "p" :model "gemini-2.5-flash"))))
        (should (member "--yolo" cmd))))))

(ert-deftest anvil-orchestrator-test-gemini-build-cmd-extra-args ()
  "`anvil-orchestrator-gemini-extra-args' is appended after flags."
  (let ((anvil-orchestrator-gemini-extra-args '("--include-dir" "src/")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) "/usr/bin/gemini")))
      (let ((cmd (anvil-orchestrator--gemini-build-cmd
                  (list :prompt "p" :model "gemini-2.5-flash"))))
        (should (member "--include-dir" cmd))
        (should (member "src/"          cmd))))))

(ert-deftest anvil-orchestrator-test-gemini-parse-output-plain-text ()
  "Parser extracts tail summary from gemini plain-text stdout."
  (let ((path (make-temp-file "gemini-out-")))
    (unwind-protect
        (progn
          (write-region
           (concat
            "Loaded cached credentials from ~/.gemini/oauth.json\n"
            "Using model: gemini-2.5-flash\n"
            "\n"
            "The README describes an Emacs MCP orchestrator.\n"
            "Key points: multi-provider, worktree isolation, cost tracking.\n")
           nil path nil 'silent)
          (let ((parsed (anvil-orchestrator--gemini-parse-output path nil 0)))
            (should (stringp (plist-get parsed :summary)))
            (should (string-match-p "orchestrator"
                                    (plist-get parsed :summary)))
            (should-not (string-match-p "Loaded cached"
                                        (plist-get parsed :summary)))
            (should-not (string-match-p "Using model:"
                                        (plist-get parsed :summary)))
            (should-not (plist-get parsed :cost-usd))
            (should-not (plist-get parsed :commit-sha))))
      (delete-file path))))

(ert-deftest anvil-orchestrator-test-gemini-parse-output-retry-on-429 ()
  "Parser maps 429 on stderr to :auto-retry-code 429 when no summary."
  (let ((out (make-temp-file "gemini-out-"))
        (err (make-temp-file "gemini-err-")))
    (unwind-protect
        (progn
          (write-region "" nil out nil 'silent)
          (write-region "HTTP 429 Too Many Requests\n" nil err nil 'silent)
          (let ((parsed (anvil-orchestrator--gemini-parse-output out err 1)))
            (should-not (plist-get parsed :summary))
            (should (equal 429 (plist-get parsed :auto-retry-code)))))
      (delete-file out)
      (delete-file err))))

(ert-deftest anvil-orchestrator-test-gemini-cost-estimator ()
  "`anvil-orchestrator--gemini-cost' returns sensible per-model numbers."
  (let ((small (anvil-orchestrator--gemini-cost
                (list :prompt "hi" :model "gemini-2.5-flash")))
        (big   (anvil-orchestrator--gemini-cost
                (list :prompt (make-string 4000 ?x)
                      :model "gemini-2.5-pro")))
        (flash (anvil-orchestrator--gemini-cost
                (list :prompt (make-string 4000 ?x)
                      :model "gemini-2.0-flash"))))
    (should (and (numberp small) (>= small 0) (< small 0.01)))
    (should (and (numberp big)   (> big small)))
    ;; 2.0-flash is strictly cheaper than 2.5-pro on same input.
    (should (< flash big))))

(ert-deftest anvil-orchestrator-test-gemini-end-to-end-stub ()
  "End-to-end: stub gemini via sh routed through the gemini parser.
Registered under a distinct provider symbol so the built-in
`gemini' descriptor isn't overwritten for the rest of the suite."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-register-provider
     'gemini-stub
     :cli "sh"
     :version-check (lambda () t)
     :build-cmd (lambda (_task)
                  (list "sh" "-c"
                        (concat
                         "printf 'Loaded cached credentials\\n"
                         "Using model: gemini-2.5-flash\\n\\n"
                         "Here is my summary.\\n"
                         "It mentions the orchestrator module.\\n'")))
     :parse-output #'anvil-orchestrator--gemini-parse-output
     :supports-tool-use t
     :supports-worktree nil
     :default-model "gemini-2.5-flash"
     :cost-estimator #'anvil-orchestrator--gemini-cost)
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "gemini-stub"
                               :provider 'gemini-stub
                               :prompt   "summarize the readme"
                               :no-worktree t)))))
      (anvil-orchestrator-test--wait-batch batch 5)
      (let ((r (car (anvil-orchestrator-collect batch))))
        (should (eq 'done (plist-get r :status)))
        (should (stringp (plist-get r :summary)))
        (should (string-match-p "orchestrator module"
                                (plist-get r :summary)))
        (should-not (string-match-p "Loaded cached"
                                    (plist-get r :summary)))))))

;;;; --- cron integration (Phase 2b) ---------------------------------------

(require 'anvil-cron)

(defvar anvil-orchestrator-test--cron-captured-batch nil)

(defvar anvil-orchestrator-test--cron-hook-calls nil)

(defun anvil-orchestrator-test--cron-dispatcher ()
  "Dispatcher for `anvil-cron' `:fn'.  Submits one stub task."
  (let ((batch (anvil-orchestrator-submit
                (list (list :name "nightly"
                            :provider 'test
                            :prompt "hello from cron")))))
    (setq anvil-orchestrator-test--cron-captured-batch batch)
    batch))

(defmacro anvil-orchestrator-test--with-cron (&rest body)
  "Run BODY with a fresh orchestrator + isolated cron state.
Forces in-process cron execution so the dispatcher runs on the
main daemon where the orchestrator state lives."
  (declare (indent 0))
  `(let ((anvil-cron-use-worker nil)
         (anvil-cron--tasks (make-hash-table :test 'eq))
         (anvil-cron-after-run-functions nil)
         (anvil-orchestrator-test--cron-captured-batch nil)
         (anvil-orchestrator-test--cron-hook-calls nil))
     (anvil-orchestrator-test--with-fresh
       ,@body)))

(ert-deftest anvil-orchestrator-test-cron-dispatches-batch ()
  "A cron `:fn' that calls `anvil-orchestrator-submit' dispatches a
real batch to the stub provider and the batch runs to completion."
  (anvil-orchestrator-test--with-cron
    (anvil-cron-register
     :id 'nightly-test
     :interval 3600
     :fn #'anvil-orchestrator-test--cron-dispatcher)
    (anvil-cron-run-now 'nightly-test)
    ;; cron defers via run-with-timer 0 — spin until the dispatcher ran.
    (let ((deadline (+ (float-time) 10)))
      (while (and (not anvil-orchestrator-test--cron-captured-batch)
                  (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should anvil-orchestrator-test--cron-captured-batch)
    (anvil-orchestrator-test--wait-batch
     anvil-orchestrator-test--cron-captured-batch 10)
    (let ((results (anvil-orchestrator-collect
                    anvil-orchestrator-test--cron-captured-batch)))
      (should (= 1 (length results)))
      (should (eq 'done (plist-get (car results) :status)))
      (should (equal "stub task done"
                     (plist-get (car results) :summary))))))

(ert-deftest anvil-orchestrator-test-cron-after-run-hook-sees-batch-id ()
  "`anvil-cron-after-run-functions' receives the dispatcher's return
value (the batch-id) as its RESULT argument."
  (anvil-orchestrator-test--with-cron
    (add-hook 'anvil-cron-after-run-functions
              (lambda (id status result elapsed)
                (push (list :id id :status status
                            :result result :elapsed elapsed)
                      anvil-orchestrator-test--cron-hook-calls)))
    (anvil-cron-register
     :id 'nightly-test
     :interval 3600
     :fn #'anvil-orchestrator-test--cron-dispatcher)
    (anvil-cron-run-now 'nightly-test)
    (let ((deadline (+ (float-time) 10)))
      (while (and (not anvil-orchestrator-test--cron-hook-calls)
                  (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should (= 1 (length anvil-orchestrator-test--cron-hook-calls)))
    (let* ((call (car anvil-orchestrator-test--cron-hook-calls))
           (result (plist-get call :result)))
      (should (eq 'nightly-test (plist-get call :id)))
      (should (eq 'ok (plist-get call :status)))
      (should (stringp result))
      ;; batch-id is a 36-char UUID, round-tripped through `%S' → stringified.
      (should (string-match-p
               "[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}"
               result)))))

;;;; --- live smoke test ---------------------------------------------------

(ert-deftest anvil-orchestrator-test-live-claude ()
  "Submit a tiny task via the real claude CLI (ANVIL_ALLOW_LIVE=1)."
  (skip-unless (and (getenv "ANVIL_ALLOW_LIVE")
                    (executable-find "claude")))
  (anvil-orchestrator-test--with-fresh
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "live-smoke"
                               :provider 'claude
                               :model "haiku"
                               :prompt "Respond with exactly: PONG")))))
      (anvil-orchestrator-test--wait-batch batch 120)
      (let ((r (car (anvil-orchestrator-collect batch))))
        (should (eq 'done (plist-get r :status)))
        (should (stringp (plist-get r :summary)))))))

(provide 'anvil-orchestrator-test)
;;; anvil-orchestrator-test.el ends here
