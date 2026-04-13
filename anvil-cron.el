;;; anvil-cron.el --- Scheduled task runner for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Fujisawa Electric Management Office

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A lightweight cron-like scheduler for Emacs.  Tasks execute on
;; worker daemons (via anvil-worker) to avoid blocking the main
;; Emacs session.
;;
;; Quick start:
;;
;;   ;; Schedule a task at 07:00 daily
;;   (anvil-cron-register
;;    :id 'morning-lint
;;    :time "07:00"
;;    :fn #'anvil-lint-wiki-run)
;;
;;   ;; Schedule every 2 hours
;;   (anvil-cron-register
;;    :id 'health-check
;;    :interval (* 2 60 60)
;;    :fn #'my-health-check)
;;
;;   ;; Start the scheduler
;;   (anvil-cron-enable)
;;
;; Tasks can also be created at runtime via the MCP tool:
;;
;;   cron-list    — list all scheduled tasks
;;   cron-run     — run a task immediately
;;   cron-status  — show next run times and last results
;;
;; Execution model:
;;
;;   Timer fires → (anvil-worker-call EXPRESSION) on worker daemon
;;                → result stored in task record
;;                → anvil-cron-after-run-functions hook called
;;
;;   If anvil-worker is not available, falls back to
;;   run-with-timer async in the main daemon.

;;; Code:

(require 'cl-lib)
(require 'anvil-server)

;;; Customization

(defgroup anvil-cron nil
  "Scheduled task runner for anvil."
  :group 'anvil
  :prefix "anvil-cron-")

(defcustom anvil-cron-use-worker t
  "When non-nil, execute tasks on worker daemons.
When nil or when anvil-worker is unavailable, execute in the
main Emacs process via `run-with-timer'."
  :type 'boolean)

(defcustom anvil-cron-log-buffer-name "*anvil-cron-log*"
  "Buffer name for the cron execution log."
  :type 'string)

(defcustom anvil-cron-after-run-functions nil
  "Hook called after a task runs.
Each function receives (TASK-ID STATUS RESULT ELAPSED-SECS)."
  :type 'hook)

;;; Task registry

(defvar anvil-cron--tasks (make-hash-table :test #'eq)
  "Hash table of registered tasks.  Key: task ID (symbol).
Value: plist with keys:
  :id          Symbol
  :description String
  :time        \"HH:MM\" for daily schedule, or nil
  :interval    Seconds for recurring, or nil (daily if :time set)
  :fn          Function to call (no arguments)
  :expression  Elisp string for worker execution (auto-generated)
  :enabled     Boolean
  :timer       Timer object (internal)
  :last-run    Time of last execution
  :last-status Symbol: ok, error, running
  :last-result String")

(cl-defun anvil-cron-register (&key id description time interval fn enabled)
  "Register a scheduled task.

ID is a symbol identifying the task.
DESCRIPTION is a human-readable string.
TIME is \"HH:MM\" for daily execution at that local time.
INTERVAL is seconds between executions (overrides TIME if both set).
FN is a function (no args) to execute.
ENABLED defaults to t."
  (unless id (error "anvil-cron: :id required"))
  (unless fn (error "anvil-cron: :fn required"))
  (unless (or time interval)
    (error "anvil-cron: :time or :interval required"))
  ;; Stop existing timer if re-registering
  (anvil-cron--stop-task id)
  (puthash id
           (list :id id
                 :description (or description (symbol-name id))
                 :time time
                 :interval interval
                 :fn fn
                 :expression (format "(%s)" (symbol-name fn))
                 :enabled (if (eq enabled nil) t enabled)
                 :timer nil
                 :last-run nil
                 :last-status nil
                 :last-result nil)
           anvil-cron--tasks)
  ;; Auto-start if scheduler is running
  (when anvil-cron--running
    (anvil-cron--start-task id)))

(defun anvil-cron-unregister (id)
  "Remove task ID from the scheduler."
  (anvil-cron--stop-task id)
  (remhash id anvil-cron--tasks))

;;; Timer computation

(defun anvil-cron--next-daily-delay (time-str)
  "Compute seconds until next TIME-STR (\"HH:MM\") occurrence."
  (let* ((parts (split-string time-str ":"))
         (hour (string-to-number (nth 0 parts)))
         (minute (string-to-number (nth 1 parts)))
         (now (current-time))
         (today (decode-time now))
         (target (encode-time 0 minute hour
                              (decoded-time-day today)
                              (decoded-time-month today)
                              (decoded-time-year today)))
         (delay (float-time (time-subtract target now))))
    (if (> delay 0) delay
      (+ delay (* 24 60 60)))))

;;; Task execution

(defun anvil-cron--execute-task (id)
  "Execute task ID.  Dispatch to worker or run in-process."
  (let ((task (gethash id anvil-cron--tasks)))
    (when (and task (plist-get task :enabled))
      (plist-put task :last-run (current-time))
      (plist-put task :last-status 'running)
      (let ((fn (plist-get task :fn))
            (expr (plist-get task :expression))
            (start (float-time)))
        (if (and anvil-cron-use-worker
                 (fboundp 'anvil-worker-call))
            ;; Worker execution (non-blocking for main daemon)
            (run-with-timer 0 nil
              (lambda ()
                (condition-case err
                    (let ((result (anvil-worker-call expr)))
                      (anvil-cron--record-result id 'ok result start))
                  (error
                   (anvil-cron--record-result
                    id 'error (format "%S" err) start)))))
          ;; Fallback: in-process async
          (run-with-timer 0 nil
            (lambda ()
              (condition-case err
                  (let ((result (funcall fn)))
                    (anvil-cron--record-result
                     id 'ok (format "%S" result) start))
                (error
                 (anvil-cron--record-result
                  id 'error (format "%S" err) start))))))))))

(defun anvil-cron--record-result (id status result start-time)
  "Record execution result for task ID."
  (when-let ((task (gethash id anvil-cron--tasks)))
    (let ((elapsed (- (float-time) start-time)))
      (plist-put task :last-status status)
      (plist-put task :last-result result)
      (anvil-cron--log id status result elapsed)
      (run-hook-with-args 'anvil-cron-after-run-functions
                          id status result elapsed))))

(defun anvil-cron--log (id status result elapsed)
  "Append execution log entry."
  (let ((buf (get-buffer-create anvil-cron-log-buffer-name)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "[%s] %s: %s (%.1fs) %s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      id status elapsed
                      (truncate-string-to-width (or result "") 80))))))

;;; Timer management

(defvar anvil-cron--running nil
  "Non-nil when the scheduler is active.")

(defun anvil-cron--start-task (id)
  "Start the timer for task ID."
  (let ((task (gethash id anvil-cron--tasks)))
    (when (and task (plist-get task :enabled))
      ;; Cancel existing timer
      (when-let ((old (plist-get task :timer)))
        (when (timerp old) (cancel-timer old)))
      (let* ((interval (plist-get task :interval))
             (time-str (plist-get task :time))
             (delay (cond
                     (interval interval)
                     (time-str (anvil-cron--next-daily-delay time-str))
                     (t 3600)))
             (repeat (or interval (* 24 60 60)))
             (timer (run-with-timer delay repeat
                      (lambda () (anvil-cron--execute-task id)))))
        (plist-put task :timer timer)))))

(defun anvil-cron--stop-task (id)
  "Stop the timer for task ID."
  (when-let ((task (gethash id anvil-cron--tasks)))
    (when-let ((timer (plist-get task :timer)))
      (when (timerp timer) (cancel-timer timer)))
    (plist-put task :timer nil)))

(defun anvil-cron-start ()
  "Start all enabled tasks."
  (interactive)
  (setq anvil-cron--running t)
  (let ((count 0))
    (maphash (lambda (id _task)
               (anvil-cron--start-task id)
               (cl-incf count))
             anvil-cron--tasks)
    (message "anvil-cron: started %d task%s" count (if (= count 1) "" "s"))))

(defun anvil-cron-stop ()
  "Stop all task timers."
  (interactive)
  (maphash (lambda (id _task) (anvil-cron--stop-task id))
           anvil-cron--tasks)
  (setq anvil-cron--running nil)
  (message "anvil-cron: stopped"))

;;; Query functions

(defun anvil-cron-list ()
  "Return a formatted string listing all tasks."
  (let (entries)
    (maphash
     (lambda (_id task)
       (push (format "%-20s %s  sched:%-8s  status:%-7s  %s"
                     (plist-get task :id)
                     (if (plist-get task :enabled) "ON " "OFF")
                     (or (plist-get task :time)
                         (format "%ds" (or (plist-get task :interval) 0)))
                     (or (plist-get task :last-status) '--)
                     (or (plist-get task :description) ""))
             entries))
     anvil-cron--tasks)
    (if entries
        (mapconcat #'identity (nreverse entries) "\n")
      "(no tasks registered)")))

(defun anvil-cron-run-now (id)
  "Execute task ID immediately, ignoring schedule."
  (if (gethash id anvil-cron--tasks)
      (progn (anvil-cron--execute-task id)
             (format "Task %s dispatched" id))
    (format "Task not found: %s" id)))

;;; MCP tools

(defun anvil-cron--tool-list (_args)
  "List all scheduled tasks.

MCP Parameters:
  (none)"
  (anvil-cron-list))

(defun anvil-cron--tool-run (args)
  "Run a scheduled task immediately.

MCP Parameters:
  task_id - Task ID to run (string, required)
            Example: \"morning-lint\""
  (anvil-server-with-error-handling
    (let* ((id-str (or (plist-get args :task_id)
                       (error "task_id required")))
           (id (intern id-str)))
      (anvil-cron-run-now id))))

(defun anvil-cron--tool-status (_args)
  "Show detailed status of all tasks.

MCP Parameters:
  (none)"
  (let (entries)
    (maphash
     (lambda (_id task)
       (push (format "%s:\n  schedule: %s\n  enabled: %s\n  last-run: %s\n  status: %s\n  result: %s"
                     (plist-get task :id)
                     (or (plist-get task :time)
                         (format "every %ds" (or (plist-get task :interval) 0)))
                     (plist-get task :enabled)
                     (if (plist-get task :last-run)
                         (format-time-string "%Y-%m-%d %H:%M:%S"
                                            (plist-get task :last-run))
                       "never")
                     (or (plist-get task :last-status) "pending")
                     (truncate-string-to-width
                      (or (plist-get task :last-result) "N/A") 100))
             entries))
     anvil-cron--tasks)
    (if entries
        (mapconcat #'identity (nreverse entries) "\n\n")
      "(no tasks)")))

;;; Enable / Disable

(defvar anvil-cron--server-id "emacs-eval"
  "MCP server ID for tool registration.")

;;;###autoload
(defun anvil-cron-enable ()
  "Register MCP tools and start the scheduler."
  (interactive)
  (anvil-server-register-tool
   #'anvil-cron--tool-list
   :id "cron-list"
   :description "List all scheduled tasks with status"
   :read-only t
   :server-id anvil-cron--server-id)
  (anvil-server-register-tool
   #'anvil-cron--tool-run
   :id "cron-run"
   :description "Run a scheduled task immediately by ID"
   :server-id anvil-cron--server-id)
  (anvil-server-register-tool
   #'anvil-cron--tool-status
   :id "cron-status"
   :description "Show detailed status of all scheduled tasks"
   :read-only t
   :server-id anvil-cron--server-id)
  (anvil-cron-start))

;;;###autoload
(defun anvil-cron-disable ()
  "Stop scheduler and unregister tools."
  (interactive)
  (anvil-cron-stop))

(provide 'anvil-cron)
;;; anvil-cron.el ends here
