;;; anvil-eval.el --- Elisp evaluation tools for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Fujisawa Electric Management Office

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

(require 'anvil-server)

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

;;; Tool timeout

(defun anvil-eval--timeout-advice (orig-fn &rest args)
  "Wrap MCP tool call dispatch with `with-timeout' as a safety net."
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
  (if (and anvil-eval-org-fast-mode anvil-eval--in-org-fast-mode)
      (let ((delay-mode-hooks t))
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

  Synchronous evaluation with timeout protection.
  For long-running operations, use emacs-eval-async instead.

  MCP Parameters:
    expression - Emacs Lisp expression to evaluate (string, required)
                 Example: \"(elfeed-update)\"
                 Example: \"(buffer-list)\""
  (anvil-server-with-error-handling
   (let* ((form (car (read-from-string expression)))
          (result (eval form t)))
     (format "%S" result))))

;;; Async eval tools

(defvar anvil-eval--async-jobs (make-hash-table :test 'equal)
  "Hash table mapping job-id to plist (:status :result :start-time).")

(defvar anvil-eval--async-counter 0
  "Counter for generating unique job IDs.")

(defun anvil-eval--async (expression)
  "Evaluate EXPRESSION asynchronously via run-with-timer.
Returns a job ID immediately.  The expression runs in the next
event loop iteration, so Emacs remains responsive.
Use emacs-eval-result to retrieve the result.

MCP Parameters:
  expression - Emacs Lisp expression to evaluate asynchronously (string, required)
               Example: \"(byte-compile-file \\\"~/.emacs.d/init.el\\\")\"
               Use for long-running operations that would timeout with emacs-eval."
  (anvil-server-with-error-handling
    (let* ((job-id (format "job-%d-%d"
                           (setq anvil-eval--async-counter
                                 (1+ anvil-eval--async-counter))
                           (truncate (float-time))))
           (form (car (read-from-string expression))))
      (puthash job-id
               (list :status 'running
                     :result nil
                     :expression expression
                     :start-time (current-time))
               anvil-eval--async-jobs)
      (run-with-timer 0 nil
        (lambda ()
          (let ((job (gethash job-id anvil-eval--async-jobs)))
            (condition-case err
                (let ((result (eval form t)))
                  (plist-put job :status 'done)
                  (plist-put job :result (format "%S" result)))
              (error
               (plist-put job :status 'error)
               (plist-put job :result (format "Error: %S" err)))))))
      (format "Job started: %s" job-id))))

(defun anvil-eval--result (job-id)
  "Get the result of an async job.
Returns status and result.  Completed results auto-clean after 10 minutes.

MCP Parameters:
  job-id - The job ID returned by emacs-eval-async (string, required)
           Example: \"job-1-1711843200\""
  (anvil-server-with-error-handling
    (let ((job (gethash job-id anvil-eval--async-jobs)))
      (if (not job)
          (format "Job not found: %s (may have been cleaned up)" job-id)
        (let ((status (plist-get job :status))
              (result (plist-get job :result))
              (elapsed (float-time
                        (time-subtract (current-time)
                                       (plist-get job :start-time)))))
          (when (and (not (eq status 'running))
                     (> elapsed 600))
            (remhash job-id anvil-eval--async-jobs))
          (format "status: %s\nelapsed: %.1fs\nresult: %s"
                  status elapsed (or result "N/A")))))))

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

(defun anvil-eval-enable ()
  "Register eval tools and install advice."
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
  ;; Register tools
  (anvil-server-register-tool
   #'anvil-eval--sync
   :id "emacs-eval"
   :description
   "Evaluate Emacs Lisp expression synchronously and return the result.
Use for quick operations (< 30s): querying state, small edits,
reading data. For heavy operations use emacs-eval-async instead."
   :server-id anvil-eval--server-id)
  (anvil-server-register-tool
   #'anvil-eval--async
   :id "emacs-eval-async"
   :description
   "Evaluate Emacs Lisp expression asynchronously.
Returns a job ID immediately. Emacs remains responsive during execution.
Use for long-running operations: git clone, byte-compile, package install.
Retrieve result with emacs-eval-result tool using the returned job ID."
   :server-id anvil-eval--server-id)
  (anvil-server-register-tool
   #'anvil-eval--result
   :id "emacs-eval-result"
   :description
   "Get the result of an async job started by emacs-eval-async.
Returns status (running/done/error), elapsed time, and result.
Poll this until status is 'done' or 'error'."
   :server-id anvil-eval--server-id)
  (anvil-server-register-tool
   #'anvil-eval--jobs
   :id "emacs-eval-jobs"
   :description
   "List all async jobs and their statuses.
Useful for checking what's running or debugging stuck jobs."
   :server-id anvil-eval--server-id))

(defun anvil-eval-disable ()
  "Unregister eval tools and remove advice."
  (anvil-server-unregister-tool "emacs-eval" anvil-eval--server-id)
  (anvil-server-unregister-tool "emacs-eval-async" anvil-eval--server-id)
  (anvil-server-unregister-tool "emacs-eval-result" anvil-eval--server-id)
  (anvil-server-unregister-tool "emacs-eval-jobs" anvil-eval--server-id)
  (advice-remove 'anvil-server--handle-tools-call
                 #'anvil-eval--timeout-advice)
  (advice-remove 'anvil-server--handle-tools-call
                 #'anvil-eval--org-fast-mode-wrapper)
  (advice-remove 'org-mode #'anvil-eval--org-mode-fast-advice)
  (advice-remove 'anvil-server-process-jsonrpc
                 #'anvil-eval--request-mutex-advice))

(provide 'anvil-eval)
;;; anvil-eval.el ends here
