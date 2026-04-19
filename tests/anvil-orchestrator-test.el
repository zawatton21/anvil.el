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

;;;; --- Phase 3b: ollama provider -----------------------------------------

(ert-deftest anvil-orchestrator-test-ollama-provider-registered ()
  "Built-in `ollama' provider is registered on load."
  (let ((prov (anvil-orchestrator--provider 'ollama)))
    (should (anvil-orchestrator-provider-p prov))
    (should (equal "ollama" (anvil-orchestrator-provider-cli prov)))
    (should-not (anvil-orchestrator-provider-supports-tool-use prov))
    (should-not (anvil-orchestrator-provider-supports-worktree prov))
    (should-not (anvil-orchestrator-provider-supports-budget prov))
    (should-not (anvil-orchestrator-provider-supports-system-prompt-append
                 prov))
    (should (equal "llama3.2"
                   (anvil-orchestrator-provider-default-model prov)))))

(ert-deftest anvil-orchestrator-test-ollama-build-cmd-basic ()
  "`ollama' build-cmd emits `run MODEL PROMPT' positionals."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_) "/usr/bin/ollama")))
    (let ((cmd (anvil-orchestrator--ollama-build-cmd
                (list :prompt "summarize readme"
                      :model "qwen2.5-coder"))))
      (should (equal "/usr/bin/ollama"   (nth 0 cmd)))
      (should (equal "run"               (nth 1 cmd)))
      (should (equal "qwen2.5-coder"     (nth 2 cmd)))
      (should (equal "summarize readme"  (nth 3 cmd))))))

(ert-deftest anvil-orchestrator-test-ollama-build-cmd-default-model ()
  "`ollama' build-cmd falls back to `anvil-orchestrator-ollama-default-model'."
  (let ((anvil-orchestrator-ollama-default-model "llama3.2"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) "/usr/bin/ollama")))
      (let ((cmd (anvil-orchestrator--ollama-build-cmd
                  (list :prompt "x"))))
        (should (member "llama3.2" cmd))))))

(ert-deftest anvil-orchestrator-test-ollama-build-cmd-extra-args ()
  "`anvil-orchestrator-ollama-extra-args' is appended after positionals."
  (let ((anvil-orchestrator-ollama-extra-args '("--keepalive" "5m")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) "/usr/bin/ollama")))
      (let ((cmd (anvil-orchestrator--ollama-build-cmd
                  (list :prompt "p" :model "llama3.2"))))
        (should (member "--keepalive" cmd))
        (should (member "5m"          cmd))))))

(ert-deftest anvil-orchestrator-test-ollama-parse-output-plain-text ()
  "Parser extracts tail summary from ollama plain-text stdout,
dropping --verbose eval stats and any pull-progress prefixes."
  (let ((path (make-temp-file "ollama-out-")))
    (unwind-protect
        (progn
          (write-region
           (concat
            "pulling manifest\n"
            "pulling abcdef...    \n"
            "verifying sha256 digest\n"
            "writing manifest\n"
            "success\n"
            "\n"
            "The repository defines an Emacs orchestrator package.\n"
            "Key ideas: provider abstraction, worktree isolation, cost tracking.\n"
            "\n"
            "total duration:       2.341s\n"
            "load duration:        120ms\n"
            "prompt eval count:    42 tokens\n"
            "prompt eval duration: 321ms\n"
            "prompt eval rate:     130 tokens/s\n"
            "eval count:           85 tokens\n"
            "eval duration:        1.2s\n"
            "eval rate:            71 tokens/s\n")
           nil path nil 'silent)
          (let ((parsed (anvil-orchestrator--ollama-parse-output path nil 0)))
            (should (stringp (plist-get parsed :summary)))
            (should (string-match-p "orchestrator package"
                                    (plist-get parsed :summary)))
            (should-not (string-match-p "pulling manifest"
                                        (plist-get parsed :summary)))
            (should-not (string-match-p "eval count:"
                                        (plist-get parsed :summary)))
            (should-not (string-match-p "total duration:"
                                        (plist-get parsed :summary)))
            (should-not (plist-get parsed :cost-usd))
            (should-not (plist-get parsed :commit-sha))))
      (delete-file path))))

(ert-deftest anvil-orchestrator-test-ollama-cost-always-zero ()
  "`anvil-orchestrator--ollama-cost' returns 0 for every task (local)."
  (should (= 0 (anvil-orchestrator--ollama-cost
                (list :prompt "hi" :model "llama3.2"))))
  (should (= 0 (anvil-orchestrator--ollama-cost
                (list :prompt (make-string 10000 ?x)
                      :model "qwen2.5-coder")))))

(ert-deftest anvil-orchestrator-test-ollama-end-to-end-stub ()
  "End-to-end: stub ollama via sh routed through the ollama parser.
Registered under a distinct provider symbol so the built-in
`ollama' descriptor isn't overwritten for the rest of the suite."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-register-provider
     'ollama-stub
     :cli "sh"
     :version-check (lambda () t)
     :build-cmd (lambda (_task)
                  (list "sh" "-c"
                        (concat
                         "printf 'pulling manifest\\n"
                         "success\\n\\n"
                         "Your summary here.\\n"
                         "It references the orchestrator module.\\n\\n"
                         "total duration: 1.5s\\n"
                         "eval count: 42 tokens\\n'")))
     :parse-output #'anvil-orchestrator--ollama-parse-output
     :supports-tool-use nil
     :supports-worktree nil
     :default-model "llama3.2"
     :cost-estimator #'anvil-orchestrator--ollama-cost)
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "ollama-stub"
                               :provider 'ollama-stub
                               :prompt   "summarize"
                               :no-worktree t)))))
      (anvil-orchestrator-test--wait-batch batch 5)
      (let ((r (car (anvil-orchestrator-collect batch))))
        (should (eq 'done (plist-get r :status)))
        (should (stringp (plist-get r :summary)))
        (should (string-match-p "orchestrator module"
                                (plist-get r :summary)))
        (should-not (string-match-p "pulling manifest"
                                    (plist-get r :summary)))
        (should-not (string-match-p "total duration"
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

;;;; --- Phase 5: codex native provider ------------------------------------

(ert-deftest anvil-orchestrator-test-codex-provider-registered ()
  "Built-in `codex' provider is registered on load."
  (let ((prov (anvil-orchestrator--provider 'codex)))
    (should (anvil-orchestrator-provider-p prov))
    (should (equal "codex" (anvil-orchestrator-provider-cli prov)))
    (should (anvil-orchestrator-provider-supports-tool-use prov))
    (should-not (anvil-orchestrator-provider-supports-worktree prov))
    (should-not (anvil-orchestrator-provider-supports-budget prov))
    (should-not (anvil-orchestrator-provider-supports-system-prompt-append
                 prov))
    (should (equal "gpt-5.4"
                   (anvil-orchestrator-provider-default-model prov)))))

(ert-deftest anvil-orchestrator-test-codex-build-cmd-basic ()
  "codex build-cmd emits exec + required flags + prompt."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_) "/usr/bin/codex")))
    (let ((cmd (anvil-orchestrator--codex-build-cmd
                (list :prompt "summarize readme"
                      :model "gpt-5-codex"))))
      (should (equal "/usr/bin/codex"   (nth 0 cmd)))
      (should (equal "exec"             (nth 1 cmd)))
      (should (member "--skip-git-repo-check" cmd))
      (should (member "--json"                cmd))
      (should (member "-m"                    cmd))
      (should (member "gpt-5-codex"           cmd))
      ;; Prompt is the final positional.
      (should (equal "summarize readme" (car (last cmd)))))))

(ert-deftest anvil-orchestrator-test-codex-build-cmd-default-model ()
  "build-cmd falls back to `anvil-orchestrator-codex-default-model'."
  (let ((anvil-orchestrator-codex-default-model "gpt-5.4"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) "/usr/bin/codex")))
      (let ((cmd (anvil-orchestrator--codex-build-cmd
                  (list :prompt "x"))))
        (should (member "gpt-5.4" cmd))))))

(ert-deftest anvil-orchestrator-test-codex-build-cmd-default-sandbox ()
  "`--sandbox' defaults to `anvil-orchestrator-codex-sandbox'."
  (let ((anvil-orchestrator-codex-sandbox "read-only"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) "/usr/bin/codex")))
      (let ((cmd (anvil-orchestrator--codex-build-cmd
                  (list :prompt "x" :model "m"))))
        (should (member "--sandbox"  cmd))
        (should (member "read-only"  cmd))))))

(ert-deftest anvil-orchestrator-test-codex-build-cmd-sandbox-override ()
  "Task `:sandbox' beats the defcustom default."
  (let ((anvil-orchestrator-codex-sandbox "read-only"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) "/usr/bin/codex")))
      (let ((cmd (anvil-orchestrator--codex-build-cmd
                  (list :prompt "x" :model "m"
                        :sandbox "workspace-write"))))
        (should (member "workspace-write" cmd))
        (should-not (member "read-only"    cmd))))))

(ert-deftest anvil-orchestrator-test-codex-build-cmd-extra-args ()
  "Global extra-args sit between required flags and the prompt."
  (let ((anvil-orchestrator-codex-extra-args
         '("-c" "model_reasoning_effort=\"high\"")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) "/usr/bin/codex")))
      (let ((cmd (anvil-orchestrator--codex-build-cmd
                  (list :prompt "the prompt" :model "m"))))
        (should (member "-c" cmd))
        (should (member "model_reasoning_effort=\"high\"" cmd))
        (should (equal "the prompt" (car (last cmd))))))))

(ert-deftest anvil-orchestrator-test-codex-parse-output-agent-message ()
  "Parser returns the last `agent_message' text and the usage plist."
  (let ((path (make-temp-file "codex-out-")))
    (unwind-protect
        (progn
          (write-region
           (concat
            "{\"type\":\"thread.started\",\"thread_id\":\"t1\"}\n"
            "{\"type\":\"turn.started\"}\n"
            "{\"type\":\"item.completed\",\"item\":{"
            "\"id\":\"i0\",\"type\":\"agent_message\","
            "\"text\":\"Rust enforces memory safety at compile time.\"}}\n"
            "{\"type\":\"turn.completed\",\"usage\":{"
            "\"input_tokens\":11954,\"cached_input_tokens\":10624,"
            "\"output_tokens\":18}}\n")
           nil path nil 'silent)
          (let ((parsed (anvil-orchestrator--codex-parse-output path nil 0)))
            (should (stringp (plist-get parsed :summary)))
            (should (string-match-p "Rust enforces memory safety"
                                    (plist-get parsed :summary)))
            (should (plist-get parsed :cost-tokens))
            (should (equal 11954
                           (plist-get (plist-get parsed :cost-tokens)
                                      :input_tokens)))
            (should-not (plist-get parsed :cost-usd))
            (should-not (plist-get parsed :commit-sha))))
      (delete-file path))))

(ert-deftest anvil-orchestrator-test-codex-parse-output-picks-last ()
  "When the stream contains multiple `agent_message' events, the last wins."
  (let ((path (make-temp-file "codex-out-")))
    (unwind-protect
        (progn
          (write-region
           (concat
            "{\"type\":\"item.completed\",\"item\":{"
            "\"type\":\"agent_message\",\"text\":\"first draft\"}}\n"
            "{\"type\":\"item.completed\",\"item\":{"
            "\"type\":\"agent_message\",\"text\":\"final answer\"}}\n"
            "{\"type\":\"turn.completed\"}\n")
           nil path nil 'silent)
          (let ((parsed (anvil-orchestrator--codex-parse-output path nil 0)))
            (should (equal "final answer" (plist-get parsed :summary)))))
      (delete-file path))))

(ert-deftest anvil-orchestrator-test-codex-parse-output-empty ()
  "Empty stdout yields nil summary (and no crash)."
  (let ((path (make-temp-file "codex-out-")))
    (unwind-protect
        (progn
          (write-region "" nil path nil 'silent)
          (let ((parsed (anvil-orchestrator--codex-parse-output path nil 0)))
            (should-not (plist-get parsed :summary))))
      (delete-file path))))

(ert-deftest anvil-orchestrator-test-codex-parse-output-skips-non-agent-items ()
  "Non agent_message `item.completed' events do not contribute to summary."
  (let ((path (make-temp-file "codex-out-")))
    (unwind-protect
        (progn
          (write-region
           (concat
            "{\"type\":\"item.completed\",\"item\":{"
            "\"type\":\"tool_call\",\"text\":\"ls /tmp\"}}\n"
            "{\"type\":\"item.completed\",\"item\":{"
            "\"type\":\"agent_message\",\"text\":\"The answer is X.\"}}\n"
            "{\"type\":\"turn.completed\"}\n")
           nil path nil 'silent)
          (let ((parsed (anvil-orchestrator--codex-parse-output path nil 0)))
            (should (equal "The answer is X." (plist-get parsed :summary)))))
      (delete-file path))))

(ert-deftest anvil-orchestrator-test-codex-cost-always-zero ()
  "codex cost-estimator returns 0 (Plus OAuth, no per-token billing)."
  (should (= 0 (anvil-orchestrator--codex-cost
                (list :prompt "hi" :model "gpt-5.4"))))
  (should (= 0 (anvil-orchestrator--codex-cost
                (list :prompt (make-string 10000 ?x)
                      :model "gpt-5-codex")))))

(ert-deftest anvil-orchestrator-test-codex-end-to-end-stub ()
  "End-to-end: stub codex via sh emitting a JSONL event stream through
the codex parser.  Registered under a distinct provider symbol so
the built-in `codex' descriptor isn't overwritten for the rest of
the suite."
  (anvil-orchestrator-test--with-fresh
    (let ((payload
           (concat
            "{\"type\":\"thread.started\",\"thread_id\":\"t1\"}\n"
            "{\"type\":\"turn.started\"}\n"
            "{\"type\":\"item.completed\",\"item\":{"
            "\"id\":\"i0\",\"type\":\"agent_message\","
            "\"text\":\"stub codex summary\"}}\n"
            "{\"type\":\"turn.completed\",\"usage\":{\"input_tokens\":5}}")))
      (anvil-orchestrator-register-provider
       'codex-stub
       :cli "sh"
       :version-check (lambda () t)
       :build-cmd (lambda (_task)
                    (anvil-test-fixtures-stub-cmd payload 0))
       :parse-output #'anvil-orchestrator--codex-parse-output
       :supports-tool-use             t
       :supports-worktree             nil
       :supports-budget               nil
       :supports-system-prompt-append nil
       :default-model                 "gpt-5.4"
       :cost-estimator #'anvil-orchestrator--codex-cost))
    (let* ((batch (anvil-orchestrator-submit
                   (list (list :name "codex-stub"
                               :provider 'codex-stub
                               :prompt   "probe")))))
      (anvil-orchestrator-test--wait-batch batch 5)
      (let ((r (car (anvil-orchestrator-collect batch))))
        (should (eq 'done (plist-get r :status)))
        (should (equal "stub codex summary"
                       (plist-get r :summary)))))))

;;;; --- Phase 4: consensus -------------------------------------------------

(defun anvil-orchestrator-test--consensus-stream-json (summary)
  "Build a stream-json payload whose `:result' decodes to SUMMARY."
  (format
   "{\"type\":\"assistant\",\"message\":{\"content\":\"working\"}}\n{\"type\":\"result\",\"result\":%s,\"total_cost_usd\":0.001,\"usage\":{\"input_tokens\":10,\"output_tokens\":5,\"cache_read_input_tokens\":0}}"
   (json-encode summary)))

(defun anvil-orchestrator-test--register-named (name summary)
  "Register a fake provider NAME whose output is SUMMARY (plain text)."
  (let ((payload (anvil-orchestrator-test--consensus-stream-json summary)))
    (anvil-orchestrator-register-provider
     name
     :cli "sh"
     :version-check (lambda () t)
     :build-cmd (lambda (_task)
                  (anvil-test-fixtures-stub-cmd payload 0))
     :parse-output #'anvil-orchestrator--claude-parse-output
     :supports-tool-use             t
     :supports-worktree             nil
     :supports-budget               nil
     :supports-system-prompt-append nil
     :default-model                 "stub"
     :cost-estimator (lambda (_task) 0.0))))

(ert-deftest anvil-orchestrator-test-jaccard-identical ()
  "Identical line-sets yield similarity 1.0."
  (should (= 1.0 (anvil-orchestrator--jaccard-similarity
                  "alpha\nbeta\ngamma"
                  "alpha\nbeta\ngamma"))))

(ert-deftest anvil-orchestrator-test-jaccard-disjoint ()
  "Completely disjoint line-sets yield similarity 0.0."
  (should (= 0.0 (anvil-orchestrator--jaccard-similarity
                  "alpha\nbeta"
                  "x\ny\nz"))))

(ert-deftest anvil-orchestrator-test-jaccard-partial ()
  "Overlap of 2 / union 4 → 0.5."
  (should (= 0.5 (anvil-orchestrator--jaccard-similarity
                  "alpha\nbeta\ngamma"
                  "alpha\ngamma\nepsilon"))))

(ert-deftest anvil-orchestrator-test-jaccard-both-empty-is-one ()
  "Vacuously identical empties collapse to 1.0."
  (should (= 1.0 (anvil-orchestrator--jaccard-similarity "" ""))))

(ert-deftest anvil-orchestrator-test-jaccard-one-empty-is-zero ()
  "Exactly one empty side is 0.0."
  (should (= 0.0 (anvil-orchestrator--jaccard-similarity "alpha\nbeta" "")))
  (should (= 0.0 (anvil-orchestrator--jaccard-similarity "" "alpha\nbeta"))))

(ert-deftest anvil-orchestrator-test-jaccard-ignores-blank-lines ()
  "Trims & drops empty lines; duplicate lines collapsed."
  (should (= 1.0 (anvil-orchestrator--jaccard-similarity
                  "  alpha \n\n  beta  "
                  "alpha\nbeta\nalpha"))))

(ert-deftest anvil-orchestrator-test-consensus-submit-rejects-empty-prompt ()
  (anvil-orchestrator-test--with-fresh
    (should-error
     (anvil-orchestrator-submit-consensus
      :prompt "" :providers '(test test))
     :type 'user-error)))

(ert-deftest anvil-orchestrator-test-consensus-submit-rejects-single ()
  "Require >= 2 providers (else consensus is moot)."
  (anvil-orchestrator-test--with-fresh
    (should-error
     (anvil-orchestrator-submit-consensus
      :prompt "hi" :providers '(test))
     :type 'user-error)))

(ert-deftest anvil-orchestrator-test-consensus-submit-rejects-unknown-provider ()
  (anvil-orchestrator-test--with-fresh
    (should-error
     (anvil-orchestrator-submit-consensus
      :prompt "hi" :providers '(test no-such))
     :type 'user-error)))

(ert-deftest anvil-orchestrator-test-consensus-submit-fans-out ()
  "Consensus submit creates N tasks with identical prompt + one batch."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-named 'tprov-a "alpha")
    (anvil-orchestrator-test--register-named 'tprov-b "beta")
    (let* ((res (anvil-orchestrator-submit-consensus
                 :prompt "explain foo" :providers '(tprov-a tprov-b)))
           (consensus-id (plist-get res :consensus-id))
           (batch-id     (plist-get res :batch-id))
           (task-ids     (plist-get res :task-ids)))
      (should (stringp consensus-id))
      (should (stringp batch-id))
      (should (= 2 (length task-ids)))
      ;; Both fan-out tasks share the same prompt.
      (dolist (tid task-ids)
        (let ((task (anvil-orchestrator--task-get tid)))
          (should (equal "explain foo" (plist-get task :prompt)))))
      ;; Consensus metadata was persisted.
      (should (gethash consensus-id
                       anvil-orchestrator--consensus-groups)))))

(ert-deftest anvil-orchestrator-test-consensus-submit-applies-overrides ()
  "Per-provider :model overrides flow into the corresponding task."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-named 'tprov-a "alpha")
    (anvil-orchestrator-test--register-named 'tprov-b "beta")
    (let* ((res (anvil-orchestrator-submit-consensus
                 :prompt "x"
                 :providers '(tprov-a tprov-b)
                 :overrides '((tprov-a . (:model "m-a"))
                              (tprov-b . (:model "m-b")))))
           (task-ids (plist-get res :task-ids))
           (tasks    (mapcar #'anvil-orchestrator--task-get task-ids)))
      (let ((a (cl-find 'tprov-a tasks
                        :key (lambda (t0) (plist-get t0 :provider))))
            (b (cl-find 'tprov-b tasks
                        :key (lambda (t0) (plist-get t0 :provider)))))
        (should (equal "m-a" (plist-get a :model)))
        (should (equal "m-b" (plist-get b :model)))))))

(ert-deftest anvil-orchestrator-test-consensus-collect-unanimous ()
  "Identical summaries → verdict 'unanimous and min-similarity 1.0."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-named 'tprov-a "same result")
    (anvil-orchestrator-test--register-named 'tprov-b "same result")
    (let* ((res (anvil-orchestrator-submit-consensus
                 :prompt "q" :providers '(tprov-a tprov-b))))
      (anvil-orchestrator-test--wait-batch
       (plist-get res :batch-id) 10)
      (let ((v (anvil-orchestrator-consensus-collect
                (plist-get res :consensus-id))))
        (should (eq 'unanimous (plist-get v :verdict)))
        (should (= 1.0 (plist-get v :min-similarity)))
        (should (= 1 (length (plist-get v :matrix))))
        (should (= 2 (length (plist-get v :tasks))))))))

(ert-deftest anvil-orchestrator-test-consensus-collect-divergent ()
  "Disjoint summaries → verdict 'divergent."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-named 'tprov-a "apple pear")
    (anvil-orchestrator-test--register-named 'tprov-b "carrot onion")
    (let* ((res (anvil-orchestrator-submit-consensus
                 :prompt "q" :providers '(tprov-a tprov-b))))
      (anvil-orchestrator-test--wait-batch
       (plist-get res :batch-id) 10)
      (let ((v (anvil-orchestrator-consensus-collect
                (plist-get res :consensus-id))))
        (should (eq 'divergent (plist-get v :verdict)))
        (should (= 0.0 (plist-get v :min-similarity)))))))

(ert-deftest anvil-orchestrator-test-consensus-collect-unknown ()
  (anvil-orchestrator-test--with-fresh
    (should-error
     (anvil-orchestrator-consensus-collect "no-such-id")
     :type 'user-error)))

(ert-deftest anvil-orchestrator-test-consensus-mcp-submit ()
  "MCP wrapper parses providers JSON + overrides JSON and fans out."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-named 'tprov-a "same")
    (anvil-orchestrator-test--register-named 'tprov-b "same")
    (let* ((out (anvil-orchestrator--tool-consensus-submit
                 "ping"
                 "[\"tprov-a\",\"tprov-b\"]"
                 "{\"tprov-a\":{\"model\":\"m-a\"}}"))
           (consensus-id (plist-get out :consensus-id))
           (batch-id     (plist-get out :batch-id)))
      (should (stringp consensus-id))
      (should (stringp batch-id))
      (should (= 2 (length (plist-get out :task-ids))))
      (let* ((task-ids (plist-get out :task-ids))
             (a (cl-find 'tprov-a
                         (mapcar #'anvil-orchestrator--task-get task-ids)
                         :key (lambda (t0) (plist-get t0 :provider)))))
        (should (equal "m-a" (plist-get a :model)))))))

;;;; --- shared-tail regression ---------------------------------------------

(ert-deftest anvil-orchestrator-test-submit-breaks-shared-tail ()
  "Tasks built via a shared `defaults' tail (consensus fan-out) must
not clobber each other's per-task fields when plist-put appends a
new key to the internal plist.  Guard: submit deep-enough-copies
so post-finalize :elapsed-ms of task A does not appear on task B."
  (anvil-orchestrator-test--with-fresh
    (let* ((defaults '(:timeout-sec 60 :budget-usd 0.10))
           (ids (anvil-orchestrator-submit
                 (list (append (list :name "alpha" :provider 'test
                                     :prompt "hi")
                               defaults)
                       (append (list :name "beta"  :provider 'test
                                     :prompt "yo")
                               defaults)))))
      (should (stringp ids))
      (let* ((task-ids (gethash ids anvil-orchestrator--batches))
             (a (anvil-orchestrator--task-get (car task-ids)))
             (b (anvil-orchestrator--task-get (cadr task-ids))))
        ;; Mutate A's plist by adding a novel key.
        (anvil-orchestrator--task-update (car task-ids)
                                         :probe-only-on-a "A")
        (let ((a2 (anvil-orchestrator--task-get (car task-ids)))
              (b2 (anvil-orchestrator--task-get (cadr task-ids))))
          (should (equal "A" (plist-get a2 :probe-only-on-a)))
          (should (null (plist-get b2 :probe-only-on-a))))
        (ignore a b)))))

;;;; --- Phase 4b: meta-LLM judge -------------------------------------------

(ert-deftest anvil-orchestrator-test-judge-format-candidates ()
  "`--judge-format-candidates' numbers entries and labels provider."
  (let* ((tasks (list
                 (list :provider 'claude :status 'done :summary "A1")
                 (list :provider 'gemini :status 'done :summary "B1")))
         (out (anvil-orchestrator--judge-format-candidates tasks)))
    (should (string-match-p "1\\. \\[provider: claude" out))
    (should (string-match-p "2\\. \\[provider: gemini" out))
    (should (string-match-p "A1" out))
    (should (string-match-p "B1" out))))

(ert-deftest anvil-orchestrator-test-judge-format-handles-no-output ()
  "`(no output)' fallback when both :summary and :error are nil."
  (let ((out (anvil-orchestrator--judge-format-candidates
              (list (list :provider 'x :status 'failed)))))
    (should (string-match-p "(no output)" out))))

(ert-deftest anvil-orchestrator-test-judge-build-prompt-fills-template ()
  "Default template substitutes prompt then candidates."
  (let* ((out (anvil-orchestrator--judge-build-prompt
               "What is 1 + 1?"
               (list (list :provider 'claude :status 'done :summary "2"))))
         (qidx (string-match-p "What is 1 \\+ 1\\?" out))
         (cidx (string-match-p "1\\. \\[provider: claude" out)))
    (should qidx)
    (should cidx)
    ;; Question must appear before the candidate block.
    (should (< qidx cidx))))

(ert-deftest anvil-orchestrator-test-judge-unknown-consensus ()
  (anvil-orchestrator-test--with-fresh
    (should-error
     (anvil-orchestrator-judge-consensus "no-such-id")
     :type 'user-error)))

(ert-deftest anvil-orchestrator-test-judge-rejects-non-terminal ()
  "Judge errors when the fan-out batch is not terminal and :wait is nil."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-slow 5)
    (anvil-orchestrator-test--register-named 'tprov-b "beta")
    (let* ((res (anvil-orchestrator-submit-consensus
                 :prompt "slow q" :providers '(slow tprov-b))))
      ;; slow provider sleeps 5s → not terminal yet.
      (should-error
       (anvil-orchestrator-judge-consensus
        (plist-get res :consensus-id))
       :type 'user-error))))

(ert-deftest anvil-orchestrator-test-judge-submits-new-task ()
  "Happy path: after fan-out completes, judge submits a new single-task batch."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-named 'tprov-a "alpha answer")
    (anvil-orchestrator-test--register-named 'tprov-b "beta answer")
    ;; Register a judge provider whose summary we can assert on.
    (anvil-orchestrator-test--register-named 'tjudge "synth result")
    (let* ((res (anvil-orchestrator-submit-consensus
                 :prompt "unify" :providers '(tprov-a tprov-b))))
      (anvil-orchestrator-test--wait-batch
       (plist-get res :batch-id) 10)
      (let* ((j (anvil-orchestrator-judge-consensus
                 (plist-get res :consensus-id) :judge 'tjudge))
             (jid (plist-get j :judge-task-id))
             (jb  (plist-get j :judge-batch-id)))
        (should (stringp jid))
        (should (stringp jb))
        (should (eq 'tjudge (plist-get j :judge-provider)))
        (anvil-orchestrator-test--wait-batch jb 10)
        (let* ((coll (anvil-orchestrator-judge-collect
                      (plist-get res :consensus-id)))
               (jplist (plist-get coll :judge)))
          (should (eq 'done (plist-get jplist :status)))
          (should (equal "synth result" (plist-get jplist :summary)))
          (should (eq 'tjudge (plist-get coll :judge-provider))))))))

(ert-deftest anvil-orchestrator-test-judge-prompt-embeds-summaries ()
  "Judge task's prompt contains each candidate summary verbatim."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-named 'tprov-a "alpha answer")
    (anvil-orchestrator-test--register-named 'tprov-b "beta answer")
    (anvil-orchestrator-test--register-named 'tjudge "synth")
    (let* ((res (anvil-orchestrator-submit-consensus
                 :prompt "explain the diff" :providers '(tprov-a tprov-b))))
      (anvil-orchestrator-test--wait-batch
       (plist-get res :batch-id) 10)
      (let* ((j (anvil-orchestrator-judge-consensus
                 (plist-get res :consensus-id) :judge 'tjudge))
             (jt (anvil-orchestrator--task-get
                  (plist-get j :judge-task-id)))
             (prompt (plist-get jt :prompt)))
        (should (string-match-p "explain the diff" prompt))
        (should (string-match-p "alpha answer" prompt))
        (should (string-match-p "beta answer"  prompt))))))

(ert-deftest anvil-orchestrator-test-judge-extra-appended ()
  "`:extra' is appended verbatim after the rendered candidate block."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-named 'tprov-a "a")
    (anvil-orchestrator-test--register-named 'tprov-b "b")
    (anvil-orchestrator-test--register-named 'tjudge "s")
    (let* ((res (anvil-orchestrator-submit-consensus
                 :prompt "q" :providers '(tprov-a tprov-b))))
      (anvil-orchestrator-test--wait-batch
       (plist-get res :batch-id) 10)
      (let* ((j (anvil-orchestrator-judge-consensus
                 (plist-get res :consensus-id)
                 :judge 'tjudge
                 :extra "ANSWER IN JSON"))
             (jt (anvil-orchestrator--task-get
                  (plist-get j :judge-task-id))))
        (should (string-match-p "ANSWER IN JSON"
                                (plist-get jt :prompt)))))))

(ert-deftest anvil-orchestrator-test-judge-collect-without-submit ()
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-named 'tprov-a "a")
    (anvil-orchestrator-test--register-named 'tprov-b "b")
    (let* ((res (anvil-orchestrator-submit-consensus
                 :prompt "q" :providers '(tprov-a tprov-b))))
      (should-error
       (anvil-orchestrator-judge-collect
        (plist-get res :consensus-id))
       :type 'user-error))))

(ert-deftest anvil-orchestrator-test-judge-mcp-submit-collect ()
  "MCP wrappers drive the full judge cycle."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-named 'tprov-a "x")
    (anvil-orchestrator-test--register-named 'tprov-b "y")
    (anvil-orchestrator-test--register-named 'tjudge "judged")
    (let* ((sub (anvil-orchestrator--tool-consensus-submit
                 "q" "[\"tprov-a\",\"tprov-b\"]" nil))
           (cid (plist-get sub :consensus-id)))
      (anvil-orchestrator-test--wait-batch
       (plist-get sub :batch-id) 10)
      (let* ((jret (anvil-orchestrator--tool-consensus-judge
                    cid "tjudge" nil nil nil nil))
             (jb (plist-get jret :judge-batch-id)))
        (should (stringp (plist-get jret :judge-task-id)))
        (anvil-orchestrator-test--wait-batch jb 10)
        (let* ((coll (anvil-orchestrator--tool-consensus-judge-collect cid))
               (jplist (plist-get coll :judge)))
          (should (equal "judged" (plist-get jplist :summary))))))))

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

;; --- Phase 6C MVP tests ---

(ert-deftest anvil-orchestrator--phase-6c-compute-delay-default-within-jitter-range ()
  (let ((values nil))
    (dotimes (_ 100)
      (push (anvil-orchestrator--compute-retry-delay-ms 0) values))
    (dolist (v values)
      (should (and (>= v 150) (<= v 250))))))

(ert-deftest anvil-orchestrator--phase-6c-compute-delay-cap-enforced ()
  (let ((result (anvil-orchestrator--compute-retry-delay-ms 20)))
    (should (<= result 37500))))

(ert-deftest anvil-orchestrator--phase-6c-compute-delay-no-jitter-deterministic ()
  (let ((anvil-orchestrator-auto-retry-jitter-pct 0))
    (should (= 1600
               (anvil-orchestrator--compute-retry-delay-ms 3)))))

(ert-deftest anvil-orchestrator--phase-6c-auto-retry-on-includes-network ()
  (should (memq 'network
                (default-value 'anvil-orchestrator-auto-retry-on))))

(ert-deftest anvil-orchestrator--phase-6c-maybe-auto-retry-sets-retry-reason ()
  (let ((anvil-orchestrator--tasks (make-hash-table :test 'equal))
        (anvil-orchestrator--batches (make-hash-table :test 'equal)))
    (let* ((id (anvil-orchestrator--uuid))
           (task (list :status 'failed
                       :auto-retry-code 429
                       :retry-count 0
                       :id id
                       :batch-id "test-batch"))
           clone-id
           clone)
      (puthash id task anvil-orchestrator--tasks)
      (puthash "test-batch" (list id) anvil-orchestrator--batches)
      (unwind-protect
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'anvil-orchestrator--persist)
                     (lambda (tk)
                       (puthash (plist-get tk :id) tk
                                anvil-orchestrator--tasks)
                       tk)))
            (anvil-orchestrator--maybe-auto-retry id)
            (maphash
             (lambda (task-id task-plist)
               (when (equal (plist-get task-plist :retry-of) id)
                 (setq clone-id task-id)
                 (setq clone task-plist)))
             anvil-orchestrator--tasks)
            (should clone)
            (should (equal 429 (plist-get clone :retry-reason))))
        (remhash id anvil-orchestrator--tasks)
        (when clone-id
          (remhash clone-id anvil-orchestrator--tasks))))))

(ert-deftest anvil-orchestrator--phase-6c-dashboard-entry-shows-retry-count ()
  (let ((anvil-orchestrator--tasks (make-hash-table :test 'equal)))
    (let* ((id (anvil-orchestrator--uuid))
           (task (list :retry-count 2
                       :status 'done
                       :name "t"
                       :provider 'claude
                       :id id
                       :batch-id "b"))
           entry)
      (puthash id task anvil-orchestrator--tasks)
      (unwind-protect
          (progn
            (setq entry
                  (cl-find-if (lambda (it) (equal (car it) id))
                              (anvil-orchestrator--dashboard-entries)))
            (should entry)
            (should (equal "done×2" (aref (cadr entry) 1))))
        (remhash id anvil-orchestrator--tasks)))))

;; --- Phase 6C' tests ---
(ert-deftest anvil-orchestrator--phase-6cp-stderr-retry-after-parses-http-header ()
  (let ((path (make-temp-file "anvil-orchestrator-stderr-")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "some log\nRetry-After: 45\nmore"))
          (should (= 45000
                     (anvil-orchestrator--stderr-retry-after-ms path 1))))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest anvil-orchestrator--phase-6cp-stderr-retry-after-parses-json ()
  (let ((path (make-temp-file "anvil-orchestrator-stderr-")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "{\"error\":\"rate limited\",\"retry_after\":12}"))
          (should (= 12000
                     (anvil-orchestrator--stderr-retry-after-ms path 1))))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest anvil-orchestrator--phase-6cp-stderr-retry-after-nil-on-clean-exit ()
  (let ((path (make-temp-file "anvil-orchestrator-stderr-")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "Retry-After: 30"))
          (should (null
                   (anvil-orchestrator--stderr-retry-after-ms path 0))))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest anvil-orchestrator--phase-6cp-resolve-backoff-uses-globals-for-unknown ()
  (let ((anvil-orchestrator-per-provider-backoff nil))
    (let ((backoff (anvil-orchestrator--resolve-backoff 'claude)))
      (should (= (plist-get backoff :base-ms)
                 anvil-orchestrator-auto-retry-base-ms))
      (should (= (plist-get backoff :max-delay-ms)
                 anvil-orchestrator-auto-retry-max-delay-ms))
      (should (= (plist-get backoff :jitter-pct)
                 anvil-orchestrator-auto-retry-jitter-pct)))))

(ert-deftest anvil-orchestrator--phase-6cp-resolve-backoff-honours-override ()
  (let ((anvil-orchestrator-per-provider-backoff '((claude :base-ms 500 :jitter-pct 10))))
    (let ((claude-backoff (anvil-orchestrator--resolve-backoff 'claude))
          (gemini-backoff (anvil-orchestrator--resolve-backoff 'gemini)))
      (should (= (plist-get claude-backoff :base-ms) 500))
      (should (= (plist-get claude-backoff :jitter-pct) 10))
      (should (= (plist-get claude-backoff :max-delay-ms)
                 anvil-orchestrator-auto-retry-max-delay-ms))
      (should (= (plist-get gemini-backoff :base-ms)
                 anvil-orchestrator-auto-retry-base-ms))
      (should (= (plist-get gemini-backoff :max-delay-ms)
                 anvil-orchestrator-auto-retry-max-delay-ms))
      (should (= (plist-get gemini-backoff :jitter-pct)
                 anvil-orchestrator-auto-retry-jitter-pct)))))

(ert-deftest anvil-orchestrator--phase-6cp-compute-delay-retry-after-overrides-computed ()
  (let ((anvil-orchestrator-auto-retry-jitter-pct 0))
    (should (= 60000
               (anvil-orchestrator--compute-retry-delay-ms 2 nil 60000)))))

;; --- gemma4 thinking-mode guard tests ---

(ert-deftest anvil-orchestrator--gemma4-strip-removes-single-block ()
  (let ((anvil-orchestrator-ollama-strip-thinking t))
    (should
     (equal "Actual answer."
            (anvil-orchestrator--ollama-strip-thinking-block
             "<think>reasoning here</think>\nActual answer.")))))

(ert-deftest anvil-orchestrator--gemma4-strip-case-insensitive-multiline ()
  (let ((anvil-orchestrator-ollama-strip-thinking t))
    (should
     (equal "Real answer."
            (anvil-orchestrator--ollama-strip-thinking-block
             "<THINK>line one\nline two</Think>Real answer.")))))

(ert-deftest anvil-orchestrator--gemma4-strip-bypass-when-disabled ()
  (let ((anvil-orchestrator-ollama-strip-thinking nil)
        (input "<think>meta</think>answer"))
    (should
     (equal input
            (anvil-orchestrator--ollama-strip-thinking-block input)))))

(ert-deftest anvil-orchestrator--gemma4-strip-plain-text-delimiter ()
  "Strip the `Thinking... ... ...done thinking.' plain-text form emitted by gemma4:e4b on ollama."
  (let ((anvil-orchestrator-ollama-strip-thinking t))
    (should
     (equal "Final Answer: 42"
            (anvil-orchestrator--ollama-strip-thinking-block
             "Thinking...\nStep 1: 7+5=12\nStep 2: 1+1+2=4\n...done thinking.\n\nFinal Answer: 42")))))

;; --- observability stats tests ---

(ert-deftest anvil-orchestrator--obs-stats-empty-table-zero-totals ()
  (let ((anvil-orchestrator--tasks (make-hash-table :test 'equal)))
    (let* ((stats (anvil-orchestrator-stats))
           (by-status (plist-get stats :by-status)))
      (should (= 0 (plist-get stats :total)))
      (should (= 0.0 (plist-get stats :total-cost-usd)))
      (should (null (plist-get stats :elapsed-ms-avg)))
      (should (null (plist-get stats :elapsed-ms-p50)))
      (should (null (plist-get stats :elapsed-ms-p95)))
      (should (= 0 (plist-get by-status :done)))
      (should (= 0 (plist-get by-status :failed)))
      (should (= 0 (plist-get by-status :cancelled)))
      (should (= 0 (plist-get by-status :running)))
      (should (= 0 (plist-get by-status :queued)))
      (should (null (plist-get stats :by-provider))))))

(ert-deftest anvil-orchestrator--obs-stats-mixed-populated ()
  (let ((anvil-orchestrator--tasks (make-hash-table :test 'equal)))
    (let ((finished-at 1000.0))
      (puthash (anvil-orchestrator--uuid)
               (list :status 'done :provider 'claude :elapsed-ms 100
                     :cost-usd 0.01 :finished-at finished-at)
               anvil-orchestrator--tasks)
      (puthash (anvil-orchestrator--uuid)
               (list :status 'done :provider 'claude :elapsed-ms 300
                     :cost-usd 0.02 :finished-at finished-at)
               anvil-orchestrator--tasks)
      (puthash (anvil-orchestrator--uuid)
               (list :status 'failed :provider 'codex :elapsed-ms 200
                     :cost-usd nil :finished-at finished-at)
               anvil-orchestrator--tasks)
      (puthash (anvil-orchestrator--uuid)
               (list :status 'cancelled :provider 'codex :elapsed-ms nil
                     :cost-usd nil :finished-at finished-at)
               anvil-orchestrator--tasks))
    (let* ((stats (anvil-orchestrator-stats))
           (by-status (plist-get stats :by-status))
           (by-provider (plist-get stats :by-provider))
           (claude (cdr (assoc 'claude by-provider)))
           (codex (cdr (assoc 'codex by-provider))))
      (should (= 4 (plist-get stats :total)))
      (should (= 2 (plist-get by-status :done)))
      (should (= 1 (plist-get by-status :failed)))
      (should (= 1 (plist-get by-status :cancelled)))
      (should (= 200 (plist-get stats :elapsed-ms-avg)))
      (should (= 200 (plist-get stats :elapsed-ms-p50)))
      (should (= 200 (plist-get stats :elapsed-ms-p95)))
      (should (= 0.03 (plist-get stats :total-cost-usd)))
      (should claude)
      (should (= 2 (plist-get claude :total)))
      (should (= 200 (plist-get claude :elapsed-ms-avg)))
      (should (= 0.03 (plist-get claude :cost-usd-total)))
      (should codex)
      (should (= 2 (plist-get codex :total)))
      (should (= 200 (plist-get codex :elapsed-ms-avg)))
      (should (= 0.0 (plist-get codex :cost-usd-total))))))

(ert-deftest anvil-orchestrator--obs-stats-provider-filter ()
  (let ((anvil-orchestrator--tasks (make-hash-table :test 'equal)))
    (let ((finished-at 1000.0))
      (puthash (anvil-orchestrator--uuid)
               (list :status 'done :provider 'claude :elapsed-ms 100
                     :cost-usd 0.01 :finished-at finished-at)
               anvil-orchestrator--tasks)
      (puthash (anvil-orchestrator--uuid)
               (list :status 'done :provider 'claude :elapsed-ms 300
                     :cost-usd 0.02 :finished-at finished-at)
               anvil-orchestrator--tasks)
      (puthash (anvil-orchestrator--uuid)
               (list :status 'failed :provider 'codex :elapsed-ms 200
                     :cost-usd nil :finished-at finished-at)
               anvil-orchestrator--tasks)
      (puthash (anvil-orchestrator--uuid)
               (list :status 'cancelled :provider 'codex :elapsed-ms nil
                     :cost-usd nil :finished-at finished-at)
               anvil-orchestrator--tasks))
    (let* ((stats (anvil-orchestrator-stats :provider 'claude))
           (by-status (plist-get stats :by-status))
           (by-provider (plist-get stats :by-provider)))
      (should (= 2 (plist-get stats :total)))
      (should (= 1 (length by-provider)))
      (should (eq 'claude (caar by-provider)))
      (should (= 2 (plist-get by-status :done)))
      (should (= 0 (plist-get by-status :failed)))
      (should (= 0 (plist-get by-status :cancelled)))
      (should (= 0 (plist-get by-status :running)))
      (should (= 0 (plist-get by-status :queued))))))

;; --- observability UI + MCP tests ---

(ert-deftest anvil-orchestrator--obs-ui-format-stats-text-shape ()
  (let ((text
         (anvil-orchestrator--format-stats-text
          '(:total 4
            :by-status (:done 2 :failed 1 :cancelled 1 :running 0 :queued 0)
            :by-provider ((claude :total 2 :done 2 :failed 0
                                  :elapsed-ms-avg 200 :cost-usd-total 0.03)
                          (codex :total 2 :done 0 :failed 1
                                 :elapsed-ms-avg 200 :cost-usd-total 0.0))
            :elapsed-ms-avg 200 :elapsed-ms-p50 200 :elapsed-ms-p95 300
            :total-cost-usd 0.03 :since nil :provider nil))))
    (should (stringp text))
    (should-not (string-empty-p text))
    (should (string-match-p (regexp-quote "Anvil Orchestrator stats") text))
    (should (string-match-p (regexp-quote "total:") text))
    (should (string-match-p (regexp-quote "claude") text))
    (should (string-match-p (regexp-quote "codex") text))))

(ert-deftest anvil-orchestrator--obs-ui-format-stats-text-nil-numerics ()
  (let ((text
         (anvil-orchestrator--format-stats-text
          '(:total nil
            :by-status (:done nil :failed nil :cancelled nil :running nil :queued nil)
            :by-provider nil
            :elapsed-ms-avg nil :elapsed-ms-p50 nil :elapsed-ms-p95 nil
            :total-cost-usd nil :since nil :provider nil))))
    (should (stringp text))
    (should (string-match-p (regexp-quote "-") text))))

(ert-deftest anvil-orchestrator--obs-ui-tool-stats-returns-stats ()
  (cl-letf (((symbol-function 'anvil-orchestrator-stats)
             (lambda (&rest _args)
               '(:total 42))))
    (should (equal '(:total 42)
                   (anvil-orchestrator--tool-stats)))))

;;;; --- glue helpers (extract-result / submit-one / tail) ------------------

(defmacro anvil-orchestrator-test--with-task (task-plist &rest body)
  "Run BODY with a single TASK-PLIST registered in the in-memory task table.
Cleans up the task hash afterwards so the ambient state stays pristine."
  (declare (indent 1))
  `(let* ((_tasks (copy-hash-table anvil-orchestrator--tasks))
          (task   (copy-sequence ,task-plist))
          (id     (plist-get task :id)))
     (unwind-protect
         (progn
           (puthash id task anvil-orchestrator--tasks)
           ,@body)
       (setq anvil-orchestrator--tasks _tasks))))

(ert-deftest anvil-orchestrator--extract-result-returns-summary-and-cost ()
  "extract-result returns a slim plist for a finished task without FULL."
  (anvil-orchestrator-test--with-task
      '(:id "tid-1" :status done :provider codex
        :summary "short answer" :cost-usd 0.01
        :cost-tokens (:input 10 :output 5)
        :commit-sha "abc1234" :exit-code 0)
    (let ((r (anvil-orchestrator-extract-result "tid-1")))
      (should (equal "tid-1"         (plist-get r :task-id)))
      (should (equal 'done           (plist-get r :status)))
      (should (equal "short answer"  (plist-get r :summary)))
      (should (equal 0.01            (plist-get r :cost-usd)))
      (should (equal '(:input 10 :output 5)
                     (plist-get r :tokens)))
      (should (equal "abc1234"       (plist-get r :commit-sha)))
      (should (equal 0               (plist-get r :exit-code))))))

(ert-deftest anvil-orchestrator--extract-result-errors-on-unknown-id ()
  (should-error (anvil-orchestrator-extract-result "no-such-id")
                :type 'user-error))

(ert-deftest anvil-orchestrator--extract-result-full-bypasses-truncation ()
  "With FULL non-nil, re-parse stdout-path without clamping summary."
  (let* ((tmp (make-temp-file "orch-extract-" nil ".jsonl")))
    (unwind-protect
        (progn
          ;; Write one codex-shaped agent_message event with a long body.
          (let ((long-text (make-string 8000 ?x)))
            (with-temp-file tmp
              (insert
               (json-serialize
                (list :type "item.completed"
                      :item (list :type "agent_message"
                                  :text long-text))
                :null-object nil :false-object nil)
               "\n")))
          (anvil-orchestrator-test--with-task
              (list :id "tid-full"
                    :status 'done :provider 'codex
                    :stdout-path tmp
                    :stderr-path nil
                    :exit-code 0
                    ;; Stored summary was clamped on finalize.
                    :summary (make-string 4000 ?x))
            ;; default full=nil returns stored (4k) summary
            (let ((r (anvil-orchestrator-extract-result "tid-full")))
              (should (= 4000 (length (plist-get r :summary)))))
            ;; full=t re-parses and delivers the entire 8k body
            (let ((r (anvil-orchestrator-extract-result "tid-full" t)))
              (should (= 8000 (length (plist-get r :summary)))))))
      (delete-file tmp))))

(ert-deftest anvil-orchestrator--submit-one-returns-task-id ()
  "submit-one returns the (single) task-id, not the batch-id."
  (cl-letf* ((submitted nil)
             ((symbol-function 'anvil-orchestrator-submit)
              (lambda (tasks)
                (setq submitted tasks)
                (let ((batch "batch-xyz")
                      (task-id "task-abc"))
                  (puthash batch (list task-id)
                           anvil-orchestrator--batches)
                  batch))))
    (let ((id (anvil-orchestrator-submit-one
               :provider 'codex
               :prompt "do a thing"
               :model "gpt-5-codex"
               :name "unit-one")))
      (should (equal "task-abc" id))
      (should (= 1 (length submitted)))
      (let ((t0 (car submitted)))
        (should (equal 'codex        (plist-get t0 :provider)))
        (should (equal "do a thing"  (plist-get t0 :prompt)))
        (should (equal "gpt-5-codex" (plist-get t0 :model)))
        (should (equal "unit-one"    (plist-get t0 :name)))))))

(ert-deftest anvil-orchestrator--submit-one-auto-names-when-omitted ()
  "submit-one auto-generates :name when caller does not supply one."
  (cl-letf* ((submitted nil)
             ((symbol-function 'anvil-orchestrator-submit)
              (lambda (tasks)
                (setq submitted tasks)
                (puthash "batch" (list "tid")
                         anvil-orchestrator--batches)
                "batch")))
    (anvil-orchestrator-submit-one :provider 'claude :prompt "x")
    (let ((name (plist-get (car submitted) :name)))
      (should (stringp name))
      (should (string-match-p "\\`one-claude-" name)))))

(ert-deftest anvil-orchestrator--submit-one-requires-provider-and-prompt ()
  (should-error (anvil-orchestrator-submit-one :prompt "x") :type 'user-error)
  (should-error (anvil-orchestrator-submit-one :provider 'claude)
                :type 'user-error))

(ert-deftest anvil-orchestrator--tail-reads-last-bytes ()
  "tail returns the last N bytes of stdout when smaller than file."
  (let ((tmp (make-temp-file "orch-tail-")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (dotimes (_i 10) (insert "0123456789\n"))) ; 110 bytes
          (anvil-orchestrator-test--with-task
              (list :id "tid-tail" :status 'done
                    :provider 'codex
                    :stdout-path tmp :stderr-path nil)
            (let ((text (anvil-orchestrator-tail "tid-tail" :bytes 20)))
              (should (stringp text))
              (should (= 20 (length text))))))
      (delete-file tmp))))

(ert-deftest anvil-orchestrator--tail-returns-nil-when-empty ()
  "tail returns nil when the stdout file is empty."
  (let ((tmp (make-temp-file "orch-tail-empty-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert ""))
          (anvil-orchestrator-test--with-task
              (list :id "tid-empty" :status 'done
                    :provider 'codex
                    :stdout-path tmp :stderr-path nil)
            (should (null (anvil-orchestrator-tail "tid-empty")))))
      (delete-file tmp))))

(ert-deftest anvil-orchestrator--tail-honours-stderr-stream ()
  "tail :stream :stderr reads from :stderr-path instead of :stdout-path."
  (let ((out (make-temp-file "orch-out-"))
        (err (make-temp-file "orch-err-")))
    (unwind-protect
        (progn
          (with-temp-file out (insert "OUTPUT\n"))
          (with-temp-file err (insert "STDERR_CONTENT\n"))
          (anvil-orchestrator-test--with-task
              (list :id "tid-stream" :status 'done
                    :provider 'codex
                    :stdout-path out :stderr-path err)
            (should (equal "OUTPUT\n"
                           (anvil-orchestrator-tail "tid-stream")))
            (should (equal "STDERR_CONTENT\n"
                           (anvil-orchestrator-tail
                            "tid-stream" :stream :stderr)))))
      (delete-file out)
      (delete-file err))))

(ert-deftest anvil-orchestrator--tool-tail-coerces-bytes-from-string ()
  "tool-tail accepts bytes as a numeric string (MCP pass-through)."
  (let ((tmp (make-temp-file "orch-tool-tail-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert (make-string 50 ?X)))
          (anvil-orchestrator-test--with-task
              (list :id "tid-tt" :status 'done
                    :provider 'codex
                    :stdout-path tmp :stderr-path nil)
            (let ((r (anvil-orchestrator--tool-tail "tid-tt" "stdout" "10")))
              (should (equal 10 (length (plist-get r :tail)))))))
      (delete-file tmp))))

(ert-deftest anvil-orchestrator--summary-default-is-4000 ()
  "Default `anvil-orchestrator-summary-max-chars' is 4000 after bump."
  (let ((anvil-orchestrator-summary-max-chars
         (default-value 'anvil-orchestrator-summary-max-chars)))
    (should (equal 4000 anvil-orchestrator-summary-max-chars))))

;;;; --- Phase 7a: submit-and-collect ---------------------------------------

(defmacro anvil-orchestrator-test--with-mocked-submit (task-plist &rest body)
  "Run BODY with `anvil-orchestrator-submit' mocked to return a batch
whose sole task plist is TASK-PLIST (already installed into the task
hash).  Cleans the task + batch tables on exit."
  (declare (indent 1))
  `(let ((_tasks   (copy-hash-table anvil-orchestrator--tasks))
         (_batches (copy-hash-table anvil-orchestrator--batches)))
     (unwind-protect
         (let* ((task (copy-sequence ,task-plist))
                (id   (plist-get task :id)))
           (puthash id task anvil-orchestrator--tasks)
           (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                      (lambda (_tasks)
                        (let ((batch "sac-batch"))
                          (puthash batch (list id)
                                   anvil-orchestrator--batches)
                          batch))))
             ,@body))
       (setq anvil-orchestrator--tasks   _tasks)
       (setq anvil-orchestrator--batches _batches))))

(ert-deftest anvil-orchestrator--submit-and-collect-returns-done-result ()
  "When the task is already done the wrapper returns extract-result."
  (anvil-orchestrator-test--with-mocked-submit
      '(:id "sac-done" :status done :provider codex
        :summary "ok" :cost-usd 0.02
        :cost-tokens (:input 5 :output 7)
        :commit-sha "abc" :exit-code 0)
    (let ((r (anvil-orchestrator-submit-and-collect
              :provider 'codex :prompt "do"
              :collect-timeout-sec 2.0
              :poll-interval-sec 0.05)))
      (should (equal "sac-done" (plist-get r :task-id)))
      (should (equal 'done      (plist-get r :status)))
      (should (equal "ok"       (plist-get r :summary)))
      (should (equal 0.02       (plist-get r :cost-usd)))
      (should-not (plist-get r :pending)))))

(ert-deftest anvil-orchestrator--submit-and-collect-surfaces-failed-error ()
  "A failed task yields :status failed + :error + :pending nil."
  (anvil-orchestrator-test--with-mocked-submit
      '(:id "sac-fail" :status failed :provider codex
        :error "boom" :exit-code 2 :summary nil
        :cost-tokens nil)
    (let ((r (anvil-orchestrator-submit-and-collect
              :provider 'codex :prompt "do"
              :collect-timeout-sec 2.0
              :poll-interval-sec 0.05)))
      (should (equal 'failed (plist-get r :status)))
      (should (equal "boom"  (plist-get r :error)))
      (should-not (plist-get r :pending)))))

(ert-deftest anvil-orchestrator--submit-and-collect-times-out-non-destructive ()
  "Collect timeout returns :pending t + :task-id without killing the task."
  (anvil-orchestrator-test--with-mocked-submit
      '(:id "sac-run" :status running :provider codex)
    (let ((r (anvil-orchestrator-submit-and-collect
              :provider 'codex :prompt "do"
              :collect-timeout-sec 0.2
              :poll-interval-sec 0.05)))
      (should (equal "sac-run" (plist-get r :task-id)))
      (should (plist-get r :pending))
      (should (equal 'running (plist-get r :status))))
    ;; task is NOT cleaned up
    (should (gethash "sac-run" anvil-orchestrator--tasks))))

(ert-deftest anvil-orchestrator--tool-submit-and-collect-coerces-numeric-strings ()
  "MCP wrapper accepts collect_timeout_sec as a numeric string."
  (anvil-orchestrator-test--with-mocked-submit
      '(:id "sac-mcp" :status done :provider claude
        :summary "m" :cost-usd 0 :cost-tokens nil :exit-code 0)
    (let ((r (anvil-orchestrator--tool-submit-and-collect
              "claude" "hi" nil nil nil nil nil "5")))
      (should (equal 'done (plist-get r :status)))
      (should (equal "m"   (plist-get r :summary))))
    ;; empty string collect_timeout_sec falls back to default 180
    (let ((r (anvil-orchestrator--tool-submit-and-collect
              "claude" "hi" nil nil nil nil nil "")))
      (should (equal 'done (plist-get r :status))))))

;;;; --- Phase 7b: context preamble registry -------------------------------

(defmacro anvil-orchestrator-test--with-preamble-store (&rest body)
  "Run BODY against a fresh anvil-state DB so preamble keys start empty."
  (declare (indent 0))
  `(let ((anvil-state-db-path (make-temp-file "anvil-pre-" nil ".db"))
         (anvil-state--db nil))
     (unwind-protect
         (progn
           (anvil-state-enable)
           ,@body)
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path)))))

(ert-deftest anvil-orchestrator--preamble-set-and-get-roundtrip ()
  "preamble-set stores text that preamble-get returns verbatim."
  (anvil-orchestrator-test--with-preamble-store
    (anvil-orchestrator-preamble-set "style" "Be concise.")
    (should (equal "Be concise."
                   (anvil-orchestrator-preamble-get "style")))
    (should-not (anvil-orchestrator-preamble-get "unknown"))
    (should-error (anvil-orchestrator-preamble-set "" "x")
                  :type 'user-error)
    (should-error (anvil-orchestrator-preamble-set "k" 42)
                  :type 'user-error)))

(ert-deftest anvil-orchestrator--preamble-list-sorted-with-sizes ()
  "preamble-list returns (:key :chars) plists sorted ascending by key."
  (anvil-orchestrator-test--with-preamble-store
    (anvil-orchestrator-preamble-set "zzz" "xx")
    (anvil-orchestrator-preamble-set "aaa" "hello")
    (let ((rows (anvil-orchestrator-preamble-list)))
      (should (= 2 (length rows)))
      (should (equal "aaa" (plist-get (nth 0 rows) :key)))
      (should (equal 5    (plist-get (nth 0 rows) :chars)))
      (should (equal "zzz" (plist-get (nth 1 rows) :key)))
      (should (equal 2    (plist-get (nth 1 rows) :chars))))))

(ert-deftest anvil-orchestrator--preamble-delete-removes-entry ()
  "preamble-delete removes the row and reports deletion status."
  (anvil-orchestrator-test--with-preamble-store
    (anvil-orchestrator-preamble-set "k" "v")
    (should (equal t (anvil-orchestrator-preamble-delete "k")))
    (should-not (anvil-orchestrator-preamble-get "k"))
    (should-not (anvil-orchestrator-preamble-delete "k"))))

(ert-deftest anvil-orchestrator--resolve-preamble-ref-forms ()
  "resolve-preamble-ref handles nil / string / list / unknown / bad type."
  (anvil-orchestrator-test--with-preamble-store
    (anvil-orchestrator-preamble-set "a" "ALPHA")
    (anvil-orchestrator-preamble-set "b" "BETA")
    (should-not (anvil-orchestrator--resolve-preamble-ref nil))
    (should (equal "ALPHA"
                   (anvil-orchestrator--resolve-preamble-ref "a")))
    (should (equal "ALPHA\n\nBETA"
                   (anvil-orchestrator--resolve-preamble-ref '("a" "b"))))
    (should-error (anvil-orchestrator--resolve-preamble-ref "missing")
                  :type 'user-error)
    (should-error (anvil-orchestrator--resolve-preamble-ref '("a" "missing"))
                  :type 'user-error)
    (should-error (anvil-orchestrator--resolve-preamble-ref 42)
                  :type 'user-error)))

(ert-deftest anvil-orchestrator--coerce-task-resolves-preamble-ref ()
  "Coerced task gets the preamble prepended to :prompt and loses
:preamble-ref so a later retry does not double-apply the text."
  (anvil-orchestrator-test--with-preamble-store
    (anvil-orchestrator-preamble-set "style" "Be concise.")
    (let* ((raw (list :name "t1" :provider 'claude
                      :prompt "Do thing."
                      :preamble-ref "style"))
           (coerced (anvil-orchestrator--coerce-task raw)))
      (should (equal "Be concise.\n\nDo thing."
                     (plist-get coerced :prompt)))
      (should-not (plist-get coerced :preamble-ref)))))

(ert-deftest anvil-orchestrator--preamble-set-from-file-stores-whole-file ()
  "`preamble-set-from-file' without slicing stores the entire file."
  (anvil-orchestrator-test--with-preamble-store
    (let ((path (make-temp-file "anvil-pre-file-")))
      (unwind-protect
          (progn
            (with-temp-file path
              (insert "line-one\nline-two\nline-three\n"))
            (let ((r (anvil-orchestrator-preamble-set-from-file
                      "doc" path)))
              (should (equal "doc" (plist-get r :key)))
              (should (> (plist-get r :chars) 20))
              (should (equal (expand-file-name path)
                             (plist-get r :path))))
            (should (equal "line-one\nline-two\nline-three\n"
                           (anvil-orchestrator-preamble-get "doc"))))
        (ignore-errors (delete-file path))))))

(ert-deftest anvil-orchestrator--preamble-set-from-file-offset-limit-slices ()
  "`:offset' and `:limit' carve an inclusive line range out of PATH."
  (anvil-orchestrator-test--with-preamble-store
    (let ((path (make-temp-file "anvil-pre-slice-")))
      (unwind-protect
          (progn
            (with-temp-file path
              (insert "one\ntwo\nthree\nfour\nfive\n"))
            (anvil-orchestrator-preamble-set-from-file
             "slice" path :offset 2 :limit 2)
            (should (equal "two\nthree\n"
                           (anvil-orchestrator-preamble-get "slice")))
            (anvil-orchestrator-preamble-set-from-file
             "tail" path :offset 4)
            (should (equal "four\nfive\n"
                           (anvil-orchestrator-preamble-get "tail")))
            (should-error (anvil-orchestrator-preamble-set-from-file
                           "bad" "/nonexistent/path.txt")
                          :type 'user-error)
            (should-error (anvil-orchestrator-preamble-set-from-file
                           "bad" path :offset 0)
                          :type 'user-error))
        (ignore-errors (delete-file path))))))

;;;; --- Phase 7c: live streaming -------------------------------------------

(ert-deftest anvil-orchestrator--stream-opt-in-default-off ()
  "A task without :stream t does not accumulate stream events."
  (anvil-orchestrator-test--with-fresh
    (let ((batch (anvil-orchestrator-submit
                  '((:name "no-stream" :provider test :prompt "x")))))
      (anvil-orchestrator-test--wait-batch batch)
      (let* ((id (car (gethash batch anvil-orchestrator--batches)))
             (task (anvil-orchestrator--task-get id)))
        (should-not (plist-get task :stream-events))
        (should-not (anvil-orchestrator-stream id))))))

(ert-deftest anvil-orchestrator--stream-event-push-appends ()
  "A streaming task accumulates one event per stream-json line."
  (anvil-orchestrator-test--with-fresh
    (let ((batch (anvil-orchestrator-submit
                  '((:name "s1" :provider test :prompt "x" :stream t)))))
      (anvil-orchestrator-test--wait-batch batch)
      (let* ((id (car (gethash batch anvil-orchestrator--batches)))
             (events (anvil-orchestrator-stream id)))
        ;; Default stub emits assistant + result (2 JSON lines).
        (should (>= (length events) 2))
        (should (equal "assistant" (plist-get (nth 0 events) :type)))
        (should (equal "result"    (plist-get (nth 1 events) :type)))
        (should (plist-get (nth 0 events) :data))
        ;; Seq counter begins at 0 and increments monotonically.
        (should (= 0 (plist-get (nth 0 events) :seq)))
        (should (= 1 (plist-get (nth 1 events) :seq)))))))

(ert-deftest anvil-orchestrator--stream-compress-overflow ()
  "Overflow compresses oldest uncompressed events; count/seq/ts preserved."
  (let* ((now (float-time))
         (events (cl-loop for i below 10
                          collect (list :seq i :ts now :type "x"
                                        :data `(:i ,i) :line "body")))
         (capped (anvil-orchestrator--stream-compress-overflow events 3)))
    (should (= 10 (length capped)))
    ;; Oldest 7 are compressed, newest 3 keep bodies.
    (should (plist-get (nth 0 capped) :compressed))
    (should (plist-get (nth 6 capped) :compressed))
    (should-not (plist-get (nth 7 capped) :compressed))
    (should-not (plist-get (nth 9 capped) :compressed))
    ;; Seq + ts survive.
    (should (= 0 (plist-get (nth 0 capped) :seq)))
    (should (= 9 (plist-get (nth 9 capped) :seq)))
    (should (eq now (plist-get (nth 0 capped) :ts)))
    ;; Body gone on compressed entries.
    (should-not (plist-get (nth 0 capped) :data))
    (should-not (plist-get (nth 0 capped) :line))
    ;; A second pass is a no-op (already at cap).
    (let ((re-capped (anvil-orchestrator--stream-compress-overflow capped 3)))
      (should (equal capped re-capped)))))

(ert-deftest anvil-orchestrator--stream-since-seq-filters ()
  "anvil-orchestrator-stream :since-seq returns only strictly-newer events."
  (anvil-orchestrator-test--with-fresh
    (let ((task (list :id "sid" :batch-id "b" :status 'done :stream t
                      :stream-events (list (list :seq 0 :ts 1.0 :type "a")
                                           (list :seq 1 :ts 1.0 :type "b")
                                           (list :seq 2 :ts 1.0 :type "c")))))
      (puthash "sid" task anvil-orchestrator--tasks)
      (should (= 3 (length (anvil-orchestrator-stream "sid"))))
      (should (= 2 (length (anvil-orchestrator-stream "sid" :since-seq 0))))
      (should (= 1 (length (anvil-orchestrator-stream "sid" :since-seq 1))))
      (should (= 0 (length (anvil-orchestrator-stream "sid" :since-seq 2))))
      ;; :since-seq nil falls through to unfiltered.
      (should (= 3 (length (anvil-orchestrator-stream "sid" :since-seq nil)))))))

(ert-deftest anvil-orchestrator--stream-tool-wrapper-shape ()
  "MCP wrapper returns :events / :last-seq / :task-status (string coerce)."
  (anvil-orchestrator-test--with-fresh
    (let ((task (list :id "tid" :batch-id "b" :status 'done :stream t
                      :stream-events (list (list :seq 0 :ts 1.0 :type "a")
                                           (list :seq 1 :ts 1.0 :type "b")))))
      (puthash "tid" task anvil-orchestrator--tasks)
      (let ((r (anvil-orchestrator--tool-stream "tid")))
        (should (= 2 (length (plist-get r :events))))
        (should (= 1 (plist-get r :last-seq)))
        (should (eq 'done (plist-get r :task-status))))
      ;; since_seq as numeric string drops older events.
      (let ((r (anvil-orchestrator--tool-stream "tid" "0")))
        (should (= 1 (length (plist-get r :events))))
        (should (= 1 (plist-get r :last-seq))))
      ;; Empty since_seq = no filter.
      (let ((r (anvil-orchestrator--tool-stream "tid" "")))
        (should (= 2 (length (plist-get r :events))))))))

(ert-deftest anvil-orchestrator--stream-on-event-callback-invoked ()
  "`:on-event' fires once per pushed event with (task-id event-plist)."
  (anvil-orchestrator-test--with-fresh
    (let* ((calls nil)
           (cb (lambda (id ev)
                 (push (cons id (plist-get ev :seq)) calls)))
           (batch (anvil-orchestrator-submit
                   `((:name "cb1" :provider test :prompt "x"
                            :stream t :on-event ,cb)))))
      (anvil-orchestrator-test--wait-batch batch)
      (should (>= (length calls) 2))
      (should (cl-every (lambda (c) (integerp (cdr c))) calls))
      (should (cl-every (lambda (c) (stringp (car c))) calls)))))

(ert-deftest anvil-orchestrator--stream-non-json-line-recorded-raw ()
  "Non-JSON output still becomes an event with :line set, :data nil."
  (anvil-orchestrator-test--with-fresh
    (anvil-orchestrator-test--register-stub "hello world")
    (let ((batch (anvil-orchestrator-submit
                  '((:name "raw1" :provider test :prompt "x" :stream t)))))
      (anvil-orchestrator-test--wait-batch batch)
      (let* ((id (car (gethash batch anvil-orchestrator--batches)))
             (events (anvil-orchestrator-stream id)))
        (should (>= (length events) 1))
        (should-not (plist-get (car events) :data))
        (should (equal "hello world" (plist-get (car events) :line)))))))

(ert-deftest anvil-orchestrator--persist-strips-volatile-fields ()
  "sanitize-for-persist drops :stream-events / :on-event but keeps core."
  (let* ((task (list :id "x1" :name "n" :provider 'test :prompt "p"
                     :stream-events (list (list :seq 0))
                     :on-event #'identity
                     :status 'done))
         (sanitized (anvil-orchestrator--sanitize-for-persist task)))
    (should (plist-get sanitized :id))
    (should (plist-get sanitized :provider))
    (should (plist-get sanitized :status))
    (should-not (plist-get sanitized :stream-events))
    (should-not (plist-get sanitized :on-event))))

(ert-deftest anvil-orchestrator--attach-on-event-sets-callback ()
  "`attach-on-event' stores CB on a running task, refuses terminal ones."
  (anvil-orchestrator-test--with-fresh
    (let ((running (list :id "run1" :batch-id "b" :status 'running
                         :stream t))
          (done    (list :id "done1" :batch-id "b" :status 'done
                         :stream t)))
      (puthash "run1"  running anvil-orchestrator--tasks)
      (puthash "done1" done    anvil-orchestrator--tasks)
      (let ((cb (lambda (_ _) 'noop)))
        (should (anvil-orchestrator-attach-on-event "run1" cb))
        (should (eq cb (plist-get
                        (gethash "run1" anvil-orchestrator--tasks)
                        :on-event)))
        ;; terminal task rejected
        (should-not (anvil-orchestrator-attach-on-event "done1" cb))
        ;; unknown id rejected
        (should-not (anvil-orchestrator-attach-on-event "nope" cb))))))

(ert-deftest anvil-orchestrator--dashboard-autofollow-rejects-bad-buffer ()
  "`dashboard-autofollow' errors when BUF is not in dashboard-mode."
  (anvil-orchestrator-test--with-fresh
    (let ((task (list :id "r2" :batch-id "b" :status 'running
                      :stream t)))
      (puthash "r2" task anvil-orchestrator--tasks))
    (with-temp-buffer
      (should-error (anvil-orchestrator-dashboard-autofollow
                     "r2" (current-buffer))
                    :type 'user-error))))

;;;; --- DAG resume (Phase 6C'') --------------------------------------------

(defmacro anvil-orchestrator-test--with-pool (tasks &rest body)
  "Install TASKS (list of plists) into the task + batch tables for BODY.
Cleans up afterwards so the ambient orchestrator state is pristine."
  (declare (indent 1))
  `(let ((saved-tasks   (copy-hash-table anvil-orchestrator--tasks))
         (saved-batches (copy-hash-table anvil-orchestrator--batches))
         (saved-queue   (copy-sequence anvil-orchestrator--queue)))
     (unwind-protect
         (progn
           (clrhash anvil-orchestrator--tasks)
           (clrhash anvil-orchestrator--batches)
           (setq anvil-orchestrator--queue nil)
           (dolist (t0 ,tasks)
             (puthash (plist-get t0 :id) t0 anvil-orchestrator--tasks)
             (let ((bid (plist-get t0 :batch-id)))
               (when bid
                 (puthash bid
                          (append (gethash bid anvil-orchestrator--batches)
                                  (list (plist-get t0 :id)))
                          anvil-orchestrator--batches))))
           ,@body)
       (setq anvil-orchestrator--tasks saved-tasks
             anvil-orchestrator--batches saved-batches
             anvil-orchestrator--queue saved-queue))))

(ert-deftest anvil-orchestrator--interrupted-predicate ()
  "`:interrupted t' + no `:resumed-at' ⇒ interrupted."
  (should (anvil-orchestrator--task-interrupted-p
           '(:id "a" :status failed :interrupted t)))
  (should-not (anvil-orchestrator--task-interrupted-p
               '(:id "a" :status failed :interrupted t :resumed-at 1.0)))
  (should-not (anvil-orchestrator--task-interrupted-p
               '(:id "a" :status failed)))
  (should-not (anvil-orchestrator--task-interrupted-p
               '(:id "a" :status done :interrupted t))))

(ert-deftest anvil-orchestrator--list-interrupted-scans-all-and-batch ()
  "list-interrupted surfaces the right subset with and without batch-id."
  (anvil-orchestrator-test--with-pool
      (list
       (list :id "a1" :batch-id "b1" :status 'failed :interrupted t)
       (list :id "a2" :batch-id "b1" :status 'done)
       (list :id "b1" :batch-id "b2" :status 'failed :interrupted t)
       (list :id "c1" :batch-id "b2" :status 'failed
             :interrupted t :resumed-at 5.0))
    (let* ((all (anvil-orchestrator-list-interrupted))
           (all-ids (mapcar (lambda (t0) (plist-get t0 :id)) all)))
      (should (= 2 (length all)))
      (should (member "a1" all-ids))
      (should (member "b1" all-ids)))
    (let ((only-b1 (anvil-orchestrator-list-interrupted "b1")))
      (should (= 1 (length only-b1)))
      (should (equal "a1" (plist-get (car only-b1) :id))))))

(ert-deftest anvil-orchestrator--resume-flips-status-preserves-id ()
  "resume-interrupted keeps the task-id and flips status to queued."
  (anvil-orchestrator-test--with-pool
      (list
       (list :id "iid" :batch-id "b" :name "unit"
             :provider 'codex :prompt "x"
             :status 'failed :interrupted t
             :interrupted-at 1.0
             :error "anvil-orchestrator: daemon restart interrupted task"))
    (cl-letf (((symbol-function 'anvil-orchestrator--ensure-pump-timer)
               (lambda () nil))
              ((symbol-function 'anvil-orchestrator--pump)
               (lambda () nil)))
      (let ((r (anvil-orchestrator-resume-interrupted)))
        (should (member "iid" (plist-get r :resumed)))
        (should (= 1 (plist-get r :total)))
        (let ((t0 (anvil-orchestrator--task-get "iid")))
          (should (eq 'queued (plist-get t0 :status)))
          (should (null (plist-get t0 :interrupted)))
          (should (null (plist-get t0 :error)))
          (should (plist-get t0 :resumed-at)))
        (should (member "iid" anvil-orchestrator--queue))))))

(ert-deftest anvil-orchestrator--resume-reports-blocked-when-dep-failed ()
  "Task B resumed but its dep A is still failed ⇒ :already-blocked."
  (anvil-orchestrator-test--with-pool
      (list
       ;; Dep A is a real hard failure (no :interrupted)
       (list :id "a" :batch-id "b" :name "A"
             :status 'failed :error "real failure")
       ;; B was orphaned by daemon restart; dep-on A
       (list :id "b" :batch-id "b" :name "B"
             :provider 'codex :prompt "p"
             :depends-on '("A")
             :status 'failed :interrupted t))
    (cl-letf (((symbol-function 'anvil-orchestrator--ensure-pump-timer)
               (lambda () nil))
              ((symbol-function 'anvil-orchestrator--pump)
               (lambda () nil)))
      (let ((r (anvil-orchestrator-resume-interrupted "b")))
        (should (member "b" (plist-get r :already-blocked)))
        (should-not (member "b" (plist-get r :resumed)))))))

(ert-deftest anvil-orchestrator--resume-handles-dep-on-interrupted-peer ()
  "When both A and B are interrupted, both flip to queued; DAG-ready
status is checked against the *post-flip* state."
  (anvil-orchestrator-test--with-pool
      (list
       (list :id "ax" :batch-id "bx" :name "A"
             :provider 'codex :prompt "pa"
             :status 'failed :interrupted t)
       (list :id "bx" :batch-id "bx" :name "B"
             :provider 'codex :prompt "pb"
             :depends-on '("A")
             :status 'failed :interrupted t))
    (cl-letf (((symbol-function 'anvil-orchestrator--ensure-pump-timer)
               (lambda () nil))
              ((symbol-function 'anvil-orchestrator--pump)
               (lambda () nil)))
      (let ((r (anvil-orchestrator-resume-interrupted "bx")))
        (should (= 2 (plist-get r :total)))
        ;; A had no deps so it's immediately ready; B depends on A which
        ;; is now :queued (not :done) so B is reported as blocked.
        (should (member "ax" (plist-get r :resumed)))
        (should (member "bx" (plist-get r :already-blocked)))))))

(ert-deftest anvil-orchestrator--dag-ready-p-honours-depends-on ()
  (anvil-orchestrator-test--with-pool
      (list
       (list :id "x" :batch-id "bb" :name "X" :status 'done)
       (list :id "y" :batch-id "bb" :name "Y" :status 'running))
    (let ((ok (list :id "ok" :batch-id "bb" :depends-on '("X")))
          (bad (list :id "bad" :batch-id "bb" :depends-on '("Y"))))
      (should (anvil-orchestrator--dag-ready-p ok))
      (should-not (anvil-orchestrator--dag-ready-p bad))
      (should (anvil-orchestrator--dag-ready-p
               '(:id "nodep" :batch-id "bb"))))))

(ert-deftest anvil-orchestrator--tool-resume-interrupted-coerces-empty ()
  "tool wrapper treats empty batch_id string as nil (whole-pool)."
  (anvil-orchestrator-test--with-pool
      (list
       (list :id "t1" :batch-id "b1" :name "A"
             :provider 'codex :prompt "p"
             :status 'failed :interrupted t))
    (cl-letf (((symbol-function 'anvil-orchestrator--ensure-pump-timer)
               (lambda () nil))
              ((symbol-function 'anvil-orchestrator--pump)
               (lambda () nil)))
      (let ((r (anvil-orchestrator--tool-resume-interrupted "")))
        (should (= 1 (plist-get r :total))))
      ;; re-run: now already resumed, so nothing to do
      (let ((r (anvil-orchestrator--tool-resume-interrupted nil)))
        (should (= 0 (plist-get r :total)))))))

(provide 'anvil-orchestrator-test)
;;; anvil-orchestrator-test.el ends here
