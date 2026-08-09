;;; anvil-eval.el --- Elisp evaluation tools for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Provides sync and async Elisp evaluation as MCP tools.
;; - `emacs-eval' — synchronous evaluation (< timeout)
;; - `emacs-eval-async' — fire-and-forget, returns job ID
;; - `emacs-eval-result' — poll async job result
;; - `emacs-eval-jobs' — list all async jobs
;;
;; Also integrates tool-call timeout, org-mode fast-mode, and
;; request serialization mutex into the server core.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-server)
(require 'anvil-offload)

(declare-function org-agenda-files "org")

;;; Customization

(defgroup anvil-eval nil
  "Anvil Elisp evaluation tools."
  :group 'anvil
  :prefix "anvil-eval-")

(defcustom anvil-eval-timeout 30
  "Hard timeout in seconds for synchronous MCP tool calls.
Tools exceeding this are killed.  Use async variant for longer ops."
  :type 'integer
  :group 'anvil-eval)

(defcustom anvil-eval-org-fast-mode t
  "When non-nil, minimize `org-mode' setup during MCP tool calls.
Skips mode hooks, disables font-lock and org-element-cache.
Improves performance ~4x on large org files."
  :type 'boolean
  :group 'anvil-eval)

(defcustom anvil-eval-nelisp-timeout 30
  "Hard timeout in seconds for synchronous `nelisp-eval' tool calls."
  :type 'integer
  :group 'anvil-eval)

(defcustom anvil-eval-nelisp-source-directory nil
  "Directory containing pure Elisp NeLisp sources, or nil for auto-detect.
The directory must contain `nelisp-read.el', `nelisp-eval.el', and
`nelisp-macro.el'."
  :type '(choice (const :tag "Auto-detect" nil) directory)
  :group 'anvil-eval)

(defcustom anvil-eval-nelisp-reset-before-eval nil
  "When non-nil, reset pure Elisp NeLisp state before each `nelisp-eval' call.
The default nil keeps MCP calls REPL-like: definitions from previous
calls remain available in NeLisp's own global/function tables."
  :type 'boolean
  :group 'anvil-eval)

;;; Tool timeout

(defun anvil-eval--timeout-advice (orig-fn &rest args)
  "Wrap MCP dispatch in a cooperative `with-timeout' safety net.
Emacs timers run only when Lisp reaches an event-loop yield point, so this
cannot preempt a non-yielding synchronous form.  `emacs-eval' deliberately
retains root-Emacs semantics for live buffers and loaded state; deployments
that require hard containment must run a dedicated daemon behind an external
bridge watchdog."
  (with-timeout
      (anvil-eval-timeout
       (anvil-server-tool-throw
        (format "MCP tool call exceeded %ds timeout — use async variant"
                anvil-eval-timeout)))
    (apply orig-fn args)))

;;; Org-mode fast mode

(defvar anvil-eval--in-org-fast-mode nil
  "Non-nil when MCP tool is running with fast org-mode.
Set automatically for the duration of each MCP tool call.")

(defun anvil-eval--org-mode-fast-advice (orig-fn &rest args)
  "Make `org-mode' setup minimal when called inside an MCP tool."
  (if (and anvil-eval-org-fast-mode anvil-eval--in-org-fast-mode
           ;; Skip the fast path for user agenda files: those buffers
           ;; outlive the MCP tool call, and `(font-lock-mode -1)' on
           ;; them sets `font-lock-mode-set-explicitly' so
           ;; `global-font-lock-mode' never re-enables fontification —
           ;; the buffer stays uncoloured for the rest of the session.
           (not (and buffer-file-name
                     (member (expand-file-name buffer-file-name)
                             (mapcar #'expand-file-name
                                     (org-agenda-files t t))))))
      (progn
        ;; setq-local, not let: Emacs 30's define-derived-mode calls
        ;; (make-local-variable 'delay-mode-hooks) inside org-mode, which
        ;; errors if the variable is already let-bound.  Making it
        ;; buffer-local first turns that call into a harmless no-op.
        (setq-local delay-mode-hooks t)
        (apply orig-fn args)
        (when (fboundp 'font-lock-mode)
          (font-lock-mode -1))
        (when (boundp 'org-element-use-cache)
          (setq-local org-element-use-cache nil)))
    (apply orig-fn args)))

(defun anvil-eval--org-fast-mode-wrapper (orig-fn &rest args)
  "Enable fast org-mode for the duration of any MCP tool call."
  (let ((anvil-eval--in-org-fast-mode t))
    (apply orig-fn args)))

;;; Request serialization mutex

(defvar anvil-eval--request-mutex
  (when (fboundp 'make-mutex)
    (make-mutex "anvil-request"))
  "Mutex serializing MCP request dispatch.
Prevents state corruption from re-entrant calls via yield points.
Only available in Emacs with thread support.")

(defun anvil-eval--request-mutex-advice (orig-fn &rest args)
  "Serialize MCP request dispatch via a mutex."
  (if anvil-eval--request-mutex
      (with-mutex anvil-eval--request-mutex
        (apply orig-fn args))
    (apply orig-fn args)))

;;; Sync eval tool

(defun anvil-eval--sync (expression)
  "Evaluate EXPRESSION as Emacs Lisp and return the result as string.

  Synchronous evaluation preserves live root-Emacs buffers,
  definitions, and loaded state.  Its timeout is cooperative and therefore
  cannot interrupt non-yielding Lisp.  For isolated or long-running work,
  use emacs-eval-async; hard containment of this live-state tool requires a
  dedicated daemon with an external bridge watchdog.

  MCP Parameters:
    expression - Emacs Lisp expression to evaluate (string, required)
                 Example: \"(elfeed-update)\"
                 Example: \"(buffer-list)\""
  (anvil-server-with-error-handling
   (let* ((form (car (read-from-string expression)))
          (result (eval form t)))
     (format "%S" result))))

;;; NeLisp eval tool (pure Elisp REPL)

(defvar anvil-eval--nelisp-source-cache 'unset
  "Cached result of `anvil-eval--locate-nelisp-source-directory'.")

(defvar anvil-eval--nelisp-source-cache-key nil
  "Candidate directory list used for `anvil-eval--nelisp-source-cache'.")

(defvar anvil-eval--nelisp-loaded nil
  "Non-nil once the pure Elisp NeLisp evaluator has been loaded.")

(declare-function nelisp--reset "nelisp-eval" ())
(declare-function nelisp-eval-string "nelisp-eval" (str))

(defun anvil-eval--nelisp-source-candidates ()
  "Return candidate pure Elisp NeLisp source directories."
  (list anvil-eval-nelisp-source-directory
        (getenv "NELISP_ELISP_DIR")
        (expand-file-name "Cowork/Notes/dev/nelisp/src"
                          (or (getenv "HOME") "~"))
        (expand-file-name "Notes/dev/nelisp/src"
                          (or (getenv "HOME") "~"))))

(defun anvil-eval--nelisp-source-directory-p (dir)
  "Return non-nil when DIR contains the pure Elisp NeLisp evaluator."
  (let ((expanded (and dir (expand-file-name dir))))
    (and expanded
         (file-readable-p (expand-file-name "nelisp-read.el" expanded))
         (file-readable-p (expand-file-name "nelisp-eval.el" expanded))
         (file-readable-p (expand-file-name "nelisp-macro.el" expanded))
         expanded)))

(defun anvil-eval-nelisp-clear-source-cache ()
  "Clear cached pure Elisp NeLisp source discovery state."
  (interactive)
  (setq anvil-eval--nelisp-source-cache 'unset)
  (setq anvil-eval--nelisp-source-cache-key nil))

(defun anvil-eval--locate-nelisp-source-directory ()
  "Return directory containing pure Elisp NeLisp sources, or nil."
  (let ((candidates (anvil-eval--nelisp-source-candidates)))
    (if (and (not (eq anvil-eval--nelisp-source-cache 'unset))
             (equal candidates anvil-eval--nelisp-source-cache-key))
        anvil-eval--nelisp-source-cache
      (let ((found (cl-some #'anvil-eval--nelisp-source-directory-p
                            candidates)))
        (setq anvil-eval--nelisp-source-cache-key candidates)
        (setq anvil-eval--nelisp-source-cache found)))))

(defun anvil-eval--ensure-nelisp ()
  "Load the pure Elisp NeLisp evaluator and initialize REPL state."
  (let ((src-dir (anvil-eval--locate-nelisp-source-directory)))
    (unless src-dir
      (error "pure Elisp nelisp sources not found — set anvil-eval-nelisp-source-directory or NELISP_ELISP_DIR"))
    (let ((load-path (cons src-dir load-path)))
      (require 'nelisp-eval)
      (require 'nelisp-macro))
    (unless anvil-eval--nelisp-loaded
      (nelisp--reset)
      (setq anvil-eval--nelisp-loaded t))))

(defun anvil-eval--nelisp-host (expression)
  "Evaluate EXPRESSION through the pure Elisp NeLisp evaluator."
  (anvil-eval--ensure-nelisp)
  (when (and anvil-eval-nelisp-reset-before-eval
             (fboundp 'nelisp--reset))
    (nelisp--reset))
  (prin1-to-string (nelisp-eval-string expression)))

(defun anvil-eval--reset-nelisp-host ()
  "Reset the pure Elisp NeLisp evaluator and keep it loaded."
  (anvil-eval--ensure-nelisp)
  (nelisp--reset)
  (setq anvil-eval--nelisp-loaded t)
  "nelisp-eval reset")

(defun anvil-eval--nelisp (expression)
  "Evaluate EXPRESSION with the pure Elisp NeLisp evaluator.

This runs inside the host Emacs daemon, but through NeLisp's own
reader/evaluator and NeLisp-owned global/function tables.  Calls are
stateful by default so the MCP tool behaves like a practical REPL.

MCP Parameters:
  expression - NeLisp / Elisp expression to evaluate (string, required)
               Example: \"(+ 1 2 3)\"
               Example: \"(length \\\"hello\\\")\""
  (anvil-server-with-error-handling
   (with-timeout (anvil-eval-nelisp-timeout
                  (error "nelisp-eval timed out after %ds"
                         anvil-eval-nelisp-timeout))
     (anvil-eval--nelisp-host expression))))

(defun anvil-eval--nelisp-reset ()
  "Reset the stateful pure Elisp NeLisp REPL.

Use this when a previous `nelisp-eval' call intentionally changed
globals/functions or when recovering from a bad exploratory session."
  (anvil-server-with-error-handling
   (with-timeout (anvil-eval-nelisp-timeout
                  (error "nelisp-eval reset timed out after %ds"
                         anvil-eval-nelisp-timeout))
     (anvil-eval--reset-nelisp-host))))

;;; Async eval tools

(defvar anvil-eval--async-jobs (make-hash-table :test 'equal)
  "Hash table mapping job-id to async job plists.
Each job records :status, :result, :expression and :start-time.
Once its isolated child starts evaluating, :run-start-time is set;
completed jobs also carry :finish-time, :queue-wait-sec and
:runtime-sec.")

(defvar anvil-eval--async-counter 0
  "Counter for generating unique job IDs.")

(defcustom anvil-eval-async-max-active 4
  "Maximum number of isolated async evaluation children."
  :type 'integer
  :group 'anvil-eval)

(defcustom anvil-eval-async-max-queued 32
  "Maximum number of accepted async jobs waiting for a child."
  :type 'integer
  :group 'anvil-eval)

(defcustom anvil-eval-async-timeout 300
  "Default wall-clock timeout in seconds for an async job."
  :type 'number
  :group 'anvil-eval)

(defcustom anvil-eval-async-max-timeout 3600
  "Largest per-job timeout accepted by `emacs-eval-async'."
  :type 'number
  :group 'anvil-eval)

(defcustom anvil-eval-async-retention-seconds 600
  "Seconds to retain a terminal async job for result polling."
  :type 'number
  :group 'anvil-eval)

(defcustom anvil-eval-async-max-expression-bytes (* 1024 1024)
  "Maximum UTF-8 byte size accepted for an async expression."
  :type 'integer
  :group 'anvil-eval)

(defcustom anvil-eval-async-max-result-bytes (* 1024 1024)
  "Maximum UTF-8 byte size returned by an async expression.
Formatting and truncation happen in the isolated child."
  :type 'integer
  :group 'anvil-eval)

(defvar anvil-eval--async-queue nil
  "FIFO of async job IDs waiting for isolated children.")

(defvar anvil-eval--async-active-count 0
  "Number of async jobs that currently own isolated children.")

(defvar anvil-eval--async-pump-timer nil
  "Zero-delay timer scheduled to drain `anvil-eval--async-queue'.")

(defvar anvil-eval--async-shutting-down nil
  "Non-nil while async jobs are being cancelled during disable.")

(defun anvil-eval--async-context ()
  "Capture request-local execution context for an isolated child."
  (list :process-environment (copy-sequence process-environment)
        :exec-path (copy-sequence exec-path)
        :default-directory default-directory
        :shell-file-name shell-file-name
        :shell-command-switch shell-command-switch
        :load-path (copy-sequence load-path)))

(defun anvil-eval--async-child-form (expression)
  "Return a child-side form that reads and evaluates EXPRESSION safely."
  `(let* ((print-circle t)
          (print-gensym t)
          (read-result (read-from-string ,expression))
          (value (eval (car read-result) t))
          (text (format "%S" value))
          (limit ,anvil-eval-async-max-result-bytes)
          (frame-limit ,anvil-offload-max-frame-bytes)
          (marker "\n... [truncated by emacs-eval-async]")
          (fits
           (lambda (candidate)
             (and
              (<= (string-bytes candidate) limit)
              (<=
               (string-bytes
                (concat
                 anvil-offload--frame-prefix
                 (base64-encode-string
                  (encode-coding-string
                   (prin1-to-string
                    (list :id anvil-offload--repl-current-id
                          :ok candidate))
                   'utf-8-unix)
                  t)
                 "\n"))
               frame-limit)))))
     (if (funcall fits text)
         text
       ;; First retain as much of the truncation marker as both public and
       ;; wire limits permit, then fill the remaining frame with result text.
       ;; Measuring the complete emitted frame is necessary because Lisp
       ;; string escaping and base64 can expand escape-heavy results by more
       ;; than the public result cap alone predicts.
       (let ((suffix marker)
             (low 0)
             (high (length marker)))
         (unless (funcall fits suffix)
           (while (< low high)
             (let ((middle (/ (+ low high 1) 2)))
               (if (funcall fits (substring marker 0 middle))
                   (setq low middle)
                 (setq high (1- middle)))))
           (setq suffix (substring marker 0 low)))
         (setq low 0
               high (length text))
         (while (< low high)
           (let ((middle (/ (+ low high 1) 2)))
             (if (funcall fits
                          (concat (substring text 0 middle) suffix))
                 (setq low middle)
               (setq high (1- middle)))))
         (concat (substring text 0 low) suffix)))))

(defun anvil-eval--async-terminal-p (job)
  "Return non-nil when JOB has a public terminal status."
  (memq (plist-get job :status) '(done error)))

(defun anvil-eval--async-remove-from-queue (job-id)
  "Remove JOB-ID from the pending FIFO."
  (setq anvil-eval--async-queue
        (delete job-id anvil-eval--async-queue)))

(defun anvil-eval--async-cleanup-job (job-id)
  "Remove terminal JOB-ID and release its cleanup timer."
  (let ((job (gethash job-id anvil-eval--async-jobs)))
    (when (and job (anvil-eval--async-terminal-p job))
      (remhash job-id anvil-eval--async-jobs))))

(defun anvil-eval--async-schedule-pump ()
  "Schedule one event-loop turn that starts queued jobs."
  (unless (or anvil-eval--async-shutting-down
              (timerp anvil-eval--async-pump-timer))
    (setq anvil-eval--async-pump-timer
          (run-at-time
           0 nil
           (lambda ()
             (setq anvil-eval--async-pump-timer nil)
             (anvil-eval--async-pump))))))

(defun anvil-eval--async-terminalize (job-id status result reason)
  "Settle JOB-ID once with public STATUS, RESULT, and REASON."
  (let ((job (gethash job-id anvil-eval--async-jobs)))
    (when (and job (not (anvil-eval--async-terminal-p job)))
      (let* ((finish (current-time))
             (start (plist-get job :start-time))
             (run-start (plist-get job :run-start-time))
             (deadline-timer (plist-get job :deadline-timer)))
        (when (timerp deadline-timer)
          (cancel-timer deadline-timer))
        (anvil-eval--async-remove-from-queue job-id)
        (when (plist-get job :active-counted)
          (setq anvil-eval--async-active-count
                (max 0 (1- anvil-eval--async-active-count))))
        (setq job (plist-put job :status status))
        (setq job (plist-put job :phase 'terminal))
        (setq job (plist-put job :reason reason))
        (setq job (plist-put job :result result))
        (setq job (plist-put job :finish-time finish))
        (setq job
              (plist-put
               job :queue-wait-sec
               (or (plist-get job :queue-wait-sec)
                   (float-time
                    (time-subtract (or run-start finish) start)))))
        (when run-start
          (setq job
                (plist-put
                 job :runtime-sec
                 (float-time (time-subtract finish run-start)))))
        (setq job (plist-put job :deadline-timer nil))
        (setq job (plist-put job :active-counted nil))
        (setq job (plist-put job :future nil))
        (setq job (plist-put job :context nil))
        (setq job
              (plist-put
               job :cleanup-timer
               (unless anvil-eval--async-shutting-down
                 (run-at-time anvil-eval-async-retention-seconds nil
                              #'anvil-eval--async-cleanup-job job-id))))
        (puthash job-id job anvil-eval--async-jobs)
        (anvil-eval--async-schedule-pump)
        t))))

(defun anvil-eval--async-started (job-id future)
  "Record the child start transition for JOB-ID and FUTURE."
  (let ((job (gethash job-id anvil-eval--async-jobs)))
    (when (and job
               (not (anvil-eval--async-terminal-p job))
               (or (null (plist-get job :future))
                   (eq future (plist-get job :future)))
               (not (plist-get job :run-start-time)))
      (let ((run-start (current-time)))
        (setq job (plist-put job :phase 'executing))
        (setq job (plist-put job :run-start-time run-start))
        (setq job
              (plist-put
               job :queue-wait-sec
               (float-time
                (time-subtract run-start (plist-get job :start-time)))))
        (puthash job-id job anvil-eval--async-jobs)))))

(defun anvil-eval--async-settled (job-id future)
  "Translate terminal FUTURE state into JOB-ID's public result."
  (pcase (anvil-future-status future)
    ('done
     (anvil-eval--async-terminalize
      job-id 'done (anvil-future-value future) 'done))
    ('error
     (anvil-eval--async-terminalize
      job-id 'error
      (format "Error: %s" (or (anvil-future-error future)
                              "isolated child failed"))
      (or (anvil-future--terminal-reason future) 'remote-error)))
    ('killed
     (anvil-eval--async-terminalize
      job-id 'error
      (format "Error: %s" (or (anvil-future-error future)
                              "isolated child was killed"))
      (or (anvil-future--terminal-reason future) 'killed)))
    ('cancelled
     (anvil-eval--async-terminalize
      job-id 'error "Error: cancelled" 'cancelled))))

(defun anvil-eval--async-timeout (job-id)
  "Hard-stop JOB-ID after its finite wall-clock deadline."
  (let* ((job (gethash job-id anvil-eval--async-jobs))
         (future (and job (plist-get job :future))))
    (when (and job (not (anvil-eval--async-terminal-p job)))
      (if (and future (eq 'pending (anvil-future-status future)))
          (anvil-future-kill future 'timeout)
        (anvil-eval--async-terminalize
         job-id 'error "Error: timeout before isolated child started"
         'timeout))
      (let ((remaining (gethash job-id anvil-eval--async-jobs)))
        (when (and remaining (not (anvil-eval--async-terminal-p remaining)))
          (anvil-eval--async-terminalize
           job-id 'error "Error: async timeout cleanup failed" 'timeout))))))

(defun anvil-eval--async-cancel-job (job-id)
  "Cancel queued or active JOB-ID, returning its resulting job plist."
  (let* ((job (gethash job-id anvil-eval--async-jobs))
         (future (and job (plist-get job :future))))
    (when (and job (not (anvil-eval--async-terminal-p job)))
      (if (and future (eq 'pending (anvil-future-status future)))
          (anvil-future-kill future 'cancelled)
        (anvil-eval--async-terminalize
         job-id 'error "Error: cancelled" 'cancelled)))
    (gethash job-id anvil-eval--async-jobs)))

(defun anvil-eval--async-pump ()
  "Start queued jobs while isolated-child capacity is available."
  (while (and (not anvil-eval--async-shutting-down)
              (< anvil-eval--async-active-count
                 (max 1 anvil-eval-async-max-active))
              anvil-eval--async-queue)
    (let* ((job-id (pop anvil-eval--async-queue))
           (job (gethash job-id anvil-eval--async-jobs)))
      (when (and job
                 (eq 'running (plist-get job :status))
                 (eq 'queued (plist-get job :phase)))
        (setq anvil-eval--async-active-count
              (1+ anvil-eval--async-active-count))
        (setq job (plist-put job :phase 'starting))
        (setq job (plist-put job :active-counted t))
        (puthash job-id job anvil-eval--async-jobs)
        (let ((context (plist-get job :context)))
          (condition-case err
              (let ((future
                     (anvil-offload-isolated
                      (anvil-eval--async-child-form
                       (plist-get job :expression))
                      :exact-load-path
                      (plist-get context :load-path)
                      :process-environment
                      (plist-get context :process-environment)
                      :exec-path (plist-get context :exec-path)
                      :default-directory
                      (plist-get context :default-directory)
                      :shell-file-name
                      (plist-get context :shell-file-name)
                      :shell-command-switch
                      (plist-get context :shell-command-switch)
                      :on-start
                      (lambda (future)
                        (anvil-eval--async-started job-id future))
                      :on-settle
                      (lambda (future)
                        (anvil-eval--async-settled job-id future)))))
                (setq job (gethash job-id anvil-eval--async-jobs))
                (if (and job (not (anvil-eval--async-terminal-p job)))
                    (progn
                      (setq job (plist-put job :future future))
                      (setq job (plist-put job :context nil))
                      (puthash job-id job anvil-eval--async-jobs))
                  ;; Cancellation or timeout may terminalize the job while the
                  ;; child is spawning.  Never leave that freshly owned child
                  ;; alive without a job that can cancel or collect it.
                  (anvil-future-kill future 'stale-job)))
            (error
             (anvil-eval--async-terminalize
              job-id 'error
              (format "Error: could not start isolated child: %s"
                      (error-message-string err))
              'spawn-error))))))))

(defun anvil-eval--async (expression &optional timeout)
  "Queue EXPRESSION for isolated asynchronous evaluation.
Returns a job ID immediately.  User source is read, evaluated, and
printed only in a dedicated one-shot subprocess.  The child receives the
captured request environment, directory, shell, executable path, and exact
load-path ordering, but it does not clone live root definitions, features,
buffers, or buffer-local state.  Configure `anvil-offload-init-files' when
expressions require explicit child initialization.

MCP Parameters:
  expression - Emacs Lisp expression to evaluate asynchronously
               (string, required)
  timeout    - (number) Optional finite wall-clock timeout in seconds"
  (anvil-server-with-error-handling
    (unless (stringp expression)
      (error "expression must be a string"))
    (when (> (string-bytes expression)
             anvil-eval-async-max-expression-bytes)
      (error "expression exceeds %d-byte limit"
             anvil-eval-async-max-expression-bytes))
    (let* ((timeout (or timeout anvil-eval-async-timeout))
           (timeout
            (if (and (stringp timeout)
                     (string-match-p
                      "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" timeout))
                (string-to-number timeout)
              timeout)))
      (unless (and (numberp timeout)
                   (> timeout 0)
                   (<= timeout anvil-eval-async-max-timeout))
        (error "timeout must be positive and no greater than %s seconds"
               anvil-eval-async-max-timeout))
      (let ((accepted 0))
        (maphash
         (lambda (_id job)
           (unless (anvil-eval--async-terminal-p job)
             (setq accepted (1+ accepted))))
         anvil-eval--async-jobs)
        (when (>= accepted
                  (+ (max 1 anvil-eval-async-max-active)
                     (max 0 anvil-eval-async-max-queued)))
          (error "async evaluation queue is full (%d accepted)" accepted)))
      (let* ((job-id (format "job-%d-%d"
                             (setq anvil-eval--async-counter
                                   (1+ anvil-eval--async-counter))
                             (truncate (float-time))))
             (job (list :status 'running
                        :phase 'queued
                        :reason nil
                        :result nil
                        :expression expression
                        :start-time (current-time)
                        :context (anvil-eval--async-context))))
        (puthash job-id job anvil-eval--async-jobs)
        (setq job
              (plist-put
               job :deadline-timer
               (run-at-time timeout nil #'anvil-eval--async-timeout job-id)))
        (puthash job-id job anvil-eval--async-jobs)
        (setq anvil-eval--async-queue
              (append anvil-eval--async-queue (list job-id)))
        (anvil-eval--async-schedule-pump)
        (format "Job started: %s" job-id)))))

(defun anvil-eval--format-seconds (seconds)
  "Format SECONDS as a compact duration string, or return \"N/A\"."
  (if (numberp seconds)
      (format "%.1fs" seconds)
    "N/A"))

(defun anvil-eval--cancel (job-id)
  "Cancel an async JOB-ID and hard-stop its isolated child.

MCP Parameters:
  job-id - The job ID returned by emacs-eval-async (string, required)"
  (anvil-server-with-error-handling
    (let ((job (gethash job-id anvil-eval--async-jobs)))
      (cond
       ((not job) (format "Job not found: %s" job-id))
       ((anvil-eval--async-terminal-p job)
        (format "Job already %s: %s" (plist-get job :status) job-id))
       (t
        (anvil-eval--async-cancel-job job-id)
        (format "Job cancelled: %s" job-id))))))

(defun anvil-eval--async-cancel-all ()
  "Hard-stop all active jobs and clear all async scheduling state."
  (let ((anvil-eval--async-shutting-down t)
        ids)
    (when (timerp anvil-eval--async-pump-timer)
      (cancel-timer anvil-eval--async-pump-timer))
    (setq anvil-eval--async-pump-timer nil)
    (maphash (lambda (id _job) (push id ids)) anvil-eval--async-jobs)
    (dolist (id ids)
      (let ((job (gethash id anvil-eval--async-jobs)))
        (if (and job (not (anvil-eval--async-terminal-p job)))
            (anvil-eval--async-cancel-job id)
          (when-let ((timer (and job (plist-get job :cleanup-timer))))
            (when (timerp timer) (cancel-timer timer))))))
    (setq anvil-eval--async-queue nil
          anvil-eval--async-active-count 0)
    (clrhash anvil-eval--async-jobs)))

(defun anvil-eval--result (job-id)
  "Get the result of an async job.
Returns status, age, queue wait, runtime and result.  Completed
results auto-clean after 10 minutes.

MCP Parameters:
  job-id - The job ID returned by emacs-eval-async (string, required)
           Example: \"job-1-1711843200\""
  (anvil-server-with-error-handling
    (let ((job (gethash job-id anvil-eval--async-jobs)))
      (if (not job)
          (format "Job not found: %s (may have been cleaned up)" job-id)
        (let ((status (plist-get job :status))
              (result (plist-get job :result))
              (now (current-time)))
          (let* ((start-time (plist-get job :start-time))
                 (run-start (plist-get job :run-start-time))
                 (finish (plist-get job :finish-time))
                 (elapsed (float-time
                           (time-subtract now start-time)))
                 (queue-wait
                  (or (plist-get job :queue-wait-sec)
                      (and run-start
                           (float-time
                            (time-subtract run-start start-time)))
                      elapsed))
                 (runtime
                  (or (plist-get job :runtime-sec)
                      (and run-start
                           (float-time
                            (time-subtract (or finish now)
                                           run-start)))))
                 (terminal-age
                  (and finish
                       (float-time (time-subtract now finish)))))
          (when (and terminal-age
                     (> terminal-age
                        anvil-eval-async-retention-seconds))
            (when-let ((timer (plist-get job :cleanup-timer)))
              (when (timerp timer) (cancel-timer timer)))
            (remhash job-id anvil-eval--async-jobs))
          (format
           (concat "status: %s\n"
                   "elapsed: %.1fs\n"
                   "age: %.1fs\n"
                   "queue-wait: %s\n"
                   "runtime: %s\n"
                   "result: %s")
           status elapsed elapsed
           (anvil-eval--format-seconds queue-wait)
           (anvil-eval--format-seconds runtime)
           (or result "N/A"))))))))

(defun anvil-eval--jobs ()
  "List all async jobs and their statuses.

MCP Parameters: (none)"
  (anvil-server-with-error-handling
    (let ((jobs '()))
      (maphash
       (lambda (id job)
         (push (format "%s: %s (%.1fs) - %s"
                       id
                       (plist-get job :status)
                       (float-time
                        (time-subtract (current-time)
                                       (plist-get job :start-time)))
                       (truncate-string-to-width
                        (or (plist-get job :expression) "") 50))
               jobs))
       anvil-eval--async-jobs)
      (if jobs
          (mapconcat #'identity (nreverse jobs) "\n")
        "No async jobs."))))

;;; Module enable/disable

(defvar anvil-eval--server-id "anvil"
  "Server ID used when registering eval tools.")

(declare-function server-quote-arg "server")

(defun anvil-eval--server-execute-cleanup-advice (orig-fn &rest args)
  "Guarantee emacsclient teardown when `server-execute' exits abnormally.
Without this, a non-local exit during the eval body — e.g.
`(top-level)' selected in the debugger, or any unwound `throw' —
skips the reply / `server-delete-client' step at the end of
`server-execute'.  The emacsclient process is then blocked in
`recvfrom' on a still-open server connection that will never
receive its `-print' reply, hanging whatever owns it (for the MCP
stdio transport, the anvil-stdio.sh bridge and its client).

We wrap the call in an `unwind-protect': on abnormal unwind, if
this looks like a wait-for-reply client (`dontkill' is nil and the
connection is still open), synthesise an `-error' reply and tear
the connection down so emacsclient can exit cleanly.  When
`dontkill' is non-nil — `-window-system' / `-tty' / `-resume' /
`-suspend' clients — we leave the connection alone, matching
upstream `server-execute' behaviour.

`dontkill' sits at a different ARGS index depending on the Emacs
version: Emacs 30 inserted `evalexprs' ahead of it (Bug#65902,
commit 683efb8de5), shifting `dontkill' from index 4 to index 5."
  (let* ((proc (nth 0 args))
         (dontkill-index (if (>= emacs-major-version 30) 5 4))
         (dontkill (nth dontkill-index args)))
    (unwind-protect
        (apply orig-fn args)
      (when (and (null dontkill)
                 (processp proc)
                 (process-live-p proc)
                 (eq (process-status proc) 'open))
        (ignore-errors
          (process-send-string
           proc
           (concat "-error "
                   (server-quote-arg
                    "anvil: server-execute aborted by non-local exit")
                   "\n")))
        (ignore-errors (delete-process proc))))))

(defun anvil-eval-enable ()
  "Register eval tools and install advice."
  (setq anvil-eval--async-shutting-down nil)
  (setq anvil-eval--server-id
        (or (and (boundp 'anvil-server-id) anvil-server-id) "anvil"))
  ;; Install advice
  (advice-add 'anvil-server--handle-tools-call
              :around #'anvil-eval--timeout-advice)
  (advice-add 'anvil-server--handle-tools-call
              :around #'anvil-eval--org-fast-mode-wrapper)
  (advice-add 'org-mode :around #'anvil-eval--org-mode-fast-advice)
  (advice-add 'anvil-server-process-jsonrpc
              :around #'anvil-eval--request-mutex-advice)
  (advice-add 'server-execute
              :around #'anvil-eval--server-execute-cleanup-advice)
  ;; Register tools
  (anvil-server-register-tool
   #'anvil-eval--sync
   :id "emacs-eval"
   :intent '(eval)
   :layer 'dev
   :description
   "Evaluate Emacs Lisp in the live root session for quick state queries and
small edits. Its timeout is cooperative and cannot preempt non-yielding Lisp;
use emacs-eval-async for isolated work. Hard containment requires a dedicated
daemon behind an external bridge watchdog."
   :server-id anvil-eval--server-id)
  (anvil-server-register-tool
   #'anvil-eval--async
   :id "emacs-eval-async"
   :intent '(eval)
   :layer 'dev
   :description
   "Evaluate Emacs Lisp in a process-isolated asynchronous job.
Returns a job ID immediately; user source is never read or evaluated in the
root Emacs. The child receives request environment and path context but not
live root definitions, loaded features, buffers, or buffer-local state;
configure anvil-offload-init-files for required child initialization. Use for
durable long-running operations. Unsaved child-buffer changes do not survive.
Retrieve the result with emacs-eval-result using the returned job ID."
   :server-id anvil-eval--server-id)
  (anvil-server-register-tool
   #'anvil-eval--result
   :id "emacs-eval-result"
   :intent '(eval admin)
   :layer 'dev
   :description
   "Get the result of an async job started by emacs-eval-async.
Returns status (running/done/error), age, queue wait, runtime, and result.
Poll this until status is 'done' or 'error'."
   :server-id anvil-eval--server-id)
  (anvil-server-register-tool
   #'anvil-eval--cancel
   :id "emacs-eval-cancel"
   :intent '(eval admin)
   :layer 'dev
   :description
   "Cancel an async evaluation job and hard-stop its isolated subprocess.
Already-terminal and unknown job IDs return stable informational messages."
   :server-id anvil-eval--server-id)
  (anvil-server-register-tool
   #'anvil-eval--jobs
   :id "emacs-eval-jobs"
   :intent '(eval admin)
   :layer 'dev
   :description
   "List all async jobs and their statuses.
Useful for checking what's running or debugging stuck jobs."
   :server-id anvil-eval--server-id)
  (anvil-server-register-tool
   #'anvil-eval--nelisp
   :id "nelisp-eval"
   :intent '(eval)
   :layer 'dev
   :description
   "Evaluate expression with the pure Elisp NeLisp evaluator and
return the printed result.  Uses NeLisp's reader/evaluator and keeps
NeLisp globals across calls, so it behaves as a practical MCP REPL."
   :server-id anvil-eval--server-id)
  (anvil-server-register-tool
   #'anvil-eval--nelisp-reset
   :id "nelisp-eval-reset"
   :intent '(eval admin)
   :layer 'dev
   :description
   "Reset the stateful pure Elisp NeLisp REPL used by nelisp-eval."
   :server-id anvil-eval--server-id))

(defun anvil-eval-disable ()
  "Unregister eval tools and remove advice."
  (anvil-eval--async-cancel-all)
  (anvil-server-unregister-tool "emacs-eval" anvil-eval--server-id)
  (anvil-server-unregister-tool "emacs-eval-async" anvil-eval--server-id)
  (anvil-server-unregister-tool "emacs-eval-result" anvil-eval--server-id)
  (anvil-server-unregister-tool "emacs-eval-cancel" anvil-eval--server-id)
  (anvil-server-unregister-tool "emacs-eval-jobs" anvil-eval--server-id)
  (anvil-server-unregister-tool "nelisp-eval" anvil-eval--server-id)
  (anvil-server-unregister-tool "nelisp-eval-reset" anvil-eval--server-id)
  (advice-remove 'anvil-server--handle-tools-call
                 #'anvil-eval--timeout-advice)
  (advice-remove 'anvil-server--handle-tools-call
                 #'anvil-eval--org-fast-mode-wrapper)
  (advice-remove 'org-mode #'anvil-eval--org-mode-fast-advice)
  (advice-remove 'anvil-server-process-jsonrpc
                 #'anvil-eval--request-mutex-advice)
  (advice-remove 'server-execute
                 #'anvil-eval--server-execute-cleanup-advice))

(provide 'anvil-eval)
;;; anvil-eval.el ends here
