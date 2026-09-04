;;; run.el --- Doc 61 Phase 8e end-to-end eval runner (SCRATCH ONLY, never committed)  -*- lexical-binding: t; -*-

;; Drives ONE (task, arm) quest via anvil-fusion-longrun-run against a
;; scratch git-repo copy of a doc61-phase8e fixture, checkpointing
;; per-step, and writes the final digest + a metadata sexp (cost/steps/
;; panel-steps/gate-failures/wall/trace) under RAW-DIR.  Scoring
;; (make check / score.sh) is deliberately done by the bash caller
;; AFTER this process exits (or is killed by a wall-clock timeout) so
;; a killed run still gets scored against whatever files it left
;; behind.
;;
;; Usage:
;;   emacs -Q --batch -L /path/to/anvil.el \
;;     --eval '(setq anvil-state-db-path ".../state.db")' \
;;     --eval '(setq anvil-orchestrator-work-dir ".../workdir")' \
;;     -l /path/to/scratch/run.el \
;;     -- TASK_NUM ARM TASKDIR RAW_DIR \
;;   > LOG 2>&1
;;
;; TASK_NUM: 1|2|3   ARM: baseline|full

(require 'cl-lib)
(require 'anvil-orchestrator)
(require 'anvil-fusion)
(require 'anvil-fusion-panels)
(require 'anvil-fusion-verify)
(require 'anvil-fusion-ask)
(require 'anvil-fusion-longrun)

;; Harness fix after smoke attempt 1 (task1/baseline, 2026-07-04): the
;; default --permission-mode acceptEdits lets the child edit files but
;; BLOCKS Bash (`make check` -> "requires approval"), so the quest
;; burned steps 3..8 re-verifying statically and could never reach
;; STATUS: DONE.  The eval REQUIRES the agent to run `make check`
;; itself, so run children with bypassPermissions.  Config-only change
;; (defcustom in this batch process); no module file touched.  Safe
;; here: each child's cwd is a throwaway scratch fixture copy.
(setq anvil-orchestrator-permission-mode "bypassPermissions")

;;;; --- cost-capture advice (same technique as benchmarks/doc61-phase6e/run.el) ----

(defvar anvil-b8e--captured nil
  "Accumulator of cost-bearing plists seen during one quest run.")

(defun anvil-b8e--capture (result)
  (push result anvil-b8e--captured)
  result)
(advice-add 'anvil-orchestrator-extract-result :filter-return #'anvil-b8e--capture)

(defun anvil-b8e--capture-fusion-ask (result)
  "Panel member fan-out cost never crosses `anvil-orchestrator-extract-result'
\(members are read straight off `anvil-orchestrator-status' inside
`anvil-fusion-ask'\) -- recover it here from the returned :candidates."
  (let* ((cands (plist-get result :candidates))
         (sum 0.0) (any nil))
    (dolist (c cands)
      (let ((v (plist-get c :cost-usd)))
        (when (numberp v) (setq any t sum (+ sum v)))))
    (when any
      (push (list :cost-usd sum :source 'fusion-ask-candidates) anvil-b8e--captured)))
  result)
(advice-add 'anvil-fusion-ask :filter-return #'anvil-b8e--capture-fusion-ask)

(defun anvil-b8e--total-cost ()
  (let ((sum 0.0) (any nil))
    (dolist (r anvil-b8e--captured)
      (let ((v (plist-get r :cost-usd)))
        (when (numberp v) (setq any t sum (+ sum v)))))
    (if any sum nil)))

;;;; --- file helpers ---------------------------------------------------------

(defun anvil-b8e--write (path text)
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-buffer
      (insert (or text ""))
      (write-region (point-min) (point-max) path nil 'quiet))))

(defun anvil-b8e--write-sexp (path obj)
  (anvil-b8e--write
   path
   (let ((print-length nil) (print-level nil) (print-circle nil))
     (pp-to-string obj))))

;;;; --- entry point -----------------------------------------------------------

(let* ((args (delete "--" (copy-sequence command-line-args-left))))
  (setq command-line-args-left nil)
  (let* ((task-num (nth 0 args))
         (arm      (nth 1 args))
         (taskdir  (expand-file-name (nth 2 args)))
         (raw-dir  (expand-file-name (nth 3 args)))
         (run-key  (format "task%s-%s" task-num arm))
         (task-md  (expand-file-name "TASK.md" taskdir))
         (task-md-text (with-temp-buffer
                          (insert-file-contents task-md)
                          (buffer-string)))
         (goal (concat
                (format "作業ディレクトリは %s。ファイルを直接編集してよい。完了条件は make check が通ること。各ステップで実際に編集・実行を進めること。\n\n"
                        taskdir)
                task-md-text))
         (on-step-fn
          (lambda (id step digest done meta)
            (anvil-b8e--write-sexp
             (expand-file-name (format "%s.checkpoint.sexp" run-key) raw-dir)
             (list :id id :task task-num :arm arm :step step :digest digest
                   :done done :meta meta :cost-so-far (anvil-b8e--total-cost)))))
         (t0 (float-time))
         result wall errored error-msg)
    (message "anvil-b8e: %s starting (goal %d chars)" run-key (length goal))
    (condition-case err
        (setq result
              (if (string= arm "full")
                  (anvil-fusion-longrun-run
                   goal :provider 'claude :max-steps 8 :timeout-sec 240
                   :cwd taskdir :on-step on-step-fn
                   :step-panel 'opus-solo :step-panel-mode 'hard-only
                   :verify-steps t)
                (anvil-fusion-longrun-run
                 goal :provider 'claude :max-steps 8 :timeout-sec 240
                 :cwd taskdir :on-step on-step-fn)))
      (error
       (setq errored t error-msg (error-message-string err))
       (message "anvil-b8e: %s ERRORED: %s" run-key error-msg)))
    (setq wall (- (float-time) t0))
    (anvil-b8e--write
     (expand-file-name (format "%s.digest.txt" run-key) raw-dir)
     (or (plist-get result :digest) ""))
    (anvil-b8e--write-sexp
     (expand-file-name (format "%s.meta.sexp" run-key) raw-dir)
     (list :task task-num :arm arm
           :steps (plist-get result :steps)
           :stopped (plist-get result :stopped)
           :panel-steps (plist-get result :panel-steps)
           :gate-failures (plist-get result :gate-failures)
           :digest-chars (plist-get result :digest-chars)
           :wall-sec wall
           :cost-usd (anvil-b8e--total-cost)
           :errored errored
           :error-msg error-msg
           :trace (plist-get result :trace)))
    (message "anvil-b8e: %s done steps=%s stopped=%s panel-steps=%s gate-failures=%s wall=%.1fs cost=%s errored=%s"
             run-key (plist-get result :steps) (plist-get result :stopped)
             (plist-get result :panel-steps) (plist-get result :gate-failures)
             wall (anvil-b8e--total-cost) errored)))

;;; run.el ends here
