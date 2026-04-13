;;; anvil-worker.el --- Worker daemon for anvil — protect the human's Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Fujisawa Electric Management Office

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Emacs is the human's tool first.  AI should never freeze the
;; editor.  This module spawns an isolated sub-Emacs daemon (the
;; "worker") and routes MCP tool calls through it by default.
;;
;; Architecture:
;;
;;   AI client (Claude, GPT, local LLM, ...)
;;     └─ anvil-stdio.sh
;;          └─ emacsclient → main Emacs daemon
;;               └─ anvil-worker delegates to worker daemon
;;                    └─ emacsclient -f worker-server → worker Emacs
;;
;; The worker daemon:
;; - Runs with -Q (no user init) + anvil-worker-init.el
;; - Crashes don't affect the main Emacs
;; - Auto-respawns on death (health check timer)
;; - Has no org-mode, no GUI hooks — lightweight compute pool
;;
;; The main daemon remains responsive for the human at all times.

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup anvil-worker nil
  "Anvil worker daemon — isolated execution for AI tool calls."
  :group 'anvil
  :prefix "anvil-worker-")

(defcustom anvil-worker-server-name "anvil-worker"
  "Daemon name for the worker (used with --fg-daemon=NAME)."
  :type 'string
  :group 'anvil-worker)

(defcustom anvil-worker-emacs-bin (or (executable-find "emacs") "emacs")
  "Path to the Emacs binary used to spawn the worker daemon."
  :type 'file
  :group 'anvil-worker)

(defcustom anvil-worker-init-file nil
  "Init file loaded into the worker daemon.
If nil, anvil generates a minimal init that registers the eval tool.
Set this to a custom file if you want additional tools in the worker."
  :type '(choice (const :tag "Auto-generate" nil) file)
  :group 'anvil-worker)

(defcustom anvil-worker-health-check-interval 30
  "Seconds between worker health checks."
  :type 'integer
  :group 'anvil-worker)

(defcustom anvil-worker-call-timeout 60
  "Default timeout in seconds for worker calls."
  :type 'integer
  :group 'anvil-worker)

(defcustom anvil-worker-spawn-wait 5
  "Max seconds to wait for a freshly spawned worker to become reachable."
  :type 'integer
  :group 'anvil-worker)

(defcustom anvil-worker-lifecycle-log
  (expand-file-name "anvil-worker.log" user-emacs-directory)
  "Append-only log of worker lifecycle events."
  :type 'file
  :group 'anvil-worker)

;;; State

(defvar anvil-worker--server-file nil
  "Resolved path to the worker daemon's server file.")

(defvar anvil-worker--health-timer nil
  "Repeating timer for health checks.")

(defvar anvil-worker--last-state nil
  "Last observed worker state: alive, dead, or nil (unknown).")

;;; Internal helpers

(defun anvil-worker--server-file ()
  "Return the server file path for the worker daemon, cached."
  (or anvil-worker--server-file
      (setq anvil-worker--server-file
            (expand-file-name
             (concat "server/" anvil-worker-server-name)
             user-emacs-directory))))

(defun anvil-worker--log (event &optional details)
  "Append EVENT with optional DETAILS to the lifecycle log."
  (let ((line (format "%s [%s]%s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      event
                      (if details (concat " " details) ""))))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region line nil anvil-worker-lifecycle-log 'append 'no-message))))

(defun anvil-worker--generate-init-file ()
  "Generate a temporary init file for the worker daemon.
Returns the path to the generated file."
  (let ((init-file (expand-file-name "anvil-worker-init.el"
                                     user-emacs-directory)))
    (unless (file-exists-p init-file)
      (let ((anvil-dir (file-name-directory (locate-library "anvil-server"))))
        (with-temp-buffer
          (insert (format ";;; anvil-worker-init.el --- Auto-generated worker init -*- lexical-binding: t; -*-\n\n"))
          (insert (format "(add-to-list 'load-path %S)\n" anvil-dir))
          (insert "(require 'anvil-server)\n")
          (insert "(require 'anvil-server-commands)\n\n")
          (insert "(defun anvil-worker--eval (expression)\n")
          (insert "  \"Evaluate EXPRESSION on the worker daemon.\n\n")
          (insert "MCP Parameters:\n")
          (insert "  expression - Emacs Lisp expression as a string\"\n")
          (insert "  (anvil-server-with-error-handling\n")
          (insert "    (let ((result (eval (read expression) t)))\n")
          (insert "      (format \\\"%S\\\" result))))\n\n")
          (insert (format "(anvil-server-register-tool #'anvil-worker--eval\n"))
          (insert (format "  :id \"eval\"\n"))
          (insert (format "  :description \"Evaluate Emacs Lisp on the isolated worker daemon\"\n"))
          (insert (format "  :server-id \"%s\")\n\n" anvil-worker-server-name))
          (insert "(anvil-server-start)\n")
          (insert (format "(message \"[%s] ready\")\n" anvil-worker-server-name))
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (point-min) (point-max) init-file nil 'silent)))))
    init-file))

;;; Public API

(defun anvil-worker-alive-p ()
  "Return non-nil if the worker daemon is reachable."
  (let ((server-file (anvil-worker--server-file)))
    (and (file-exists-p server-file)
         (= 0 (call-process "emacsclient" nil nil nil
                            "-f" server-file
                            "-e" "t")))))

;;;###autoload
(defun anvil-worker-spawn ()
  "Spawn the worker daemon if not already running.  Idempotent."
  (interactive)
  (if (anvil-worker-alive-p)
      (progn
        (message "Anvil worker already alive")
        (anvil-worker--log 'spawn-skipped "already alive"))
    (let* ((init-file (or anvil-worker-init-file
                         (anvil-worker--generate-init-file)))
           (proc (start-process
                  "anvil-worker-spawn"
                  (get-buffer-create " *anvil-worker-spawn*")
                  anvil-worker-emacs-bin
                  (concat "--fg-daemon=" anvil-worker-server-name)
                  "-Q"
                  "-l" init-file)))
      (set-process-query-on-exit-flag proc nil)
      (anvil-worker--log 'spawn (format "pid=%d init=%s" (process-id proc) init-file))
      (message "Anvil worker spawned: pid=%d" (process-id proc))
      proc)))

;;;###autoload
(defun anvil-worker-call (expression &optional timeout)
  "Run EXPRESSION on the worker daemon.
TIMEOUT defaults to `anvil-worker-call-timeout' seconds.
Returns the worker's result as a string.
Auto-spawns the worker if it's not running."
  (let ((timeout (or timeout anvil-worker-call-timeout)))
    ;; Ensure worker is alive
    (unless (anvil-worker-alive-p)
      (anvil-worker-spawn)
      (let ((deadline (+ (float-time) anvil-worker-spawn-wait)))
        (while (and (not (anvil-worker-alive-p))
                    (< (float-time) deadline))
          (sit-for 0.1))))
    (unless (anvil-worker-alive-p)
      (error "Anvil worker failed to start within %ds" anvil-worker-spawn-wait))
    ;; Call via emacsclient
    (let ((buf (get-buffer-create " *anvil-worker-call*")))
      (with-current-buffer buf (erase-buffer))
      (let* ((proc (start-process
                    "anvil-worker-call" buf
                    "emacsclient"
                    "-f" (anvil-worker--server-file)
                    "-e" expression))
             (start (float-time)))
        (set-process-query-on-exit-flag proc nil)
        (while (and (process-live-p proc)
                    (< (- (float-time) start) timeout))
          (accept-process-output proc 0.1))
        (when (process-live-p proc)
          (kill-process proc)
          (error "Anvil worker call timeout (%ds)" timeout))
        (string-trim (with-current-buffer buf (buffer-string)))))))

;;;###autoload
(defun anvil-worker-kill ()
  "Kill the worker daemon."
  (interactive)
  (when (anvil-worker-alive-p)
    (ignore-errors
      (call-process "emacsclient" nil nil nil
                    "-f" (anvil-worker--server-file)
                    "-e" "(kill-emacs)"))
    (anvil-worker--log 'killed)
    (message "Anvil worker killed")))

;;; Health check

(defun anvil-worker--health-check ()
  "Check worker reachability; respawn on death.  Logs only transitions."
  (let ((alive (anvil-worker-alive-p)))
    (cond
     ((null anvil-worker--last-state)
      (setq anvil-worker--last-state (if alive 'alive 'dead))
      (unless alive
        (anvil-worker--log 'startup-dead "respawning")
        (anvil-worker-spawn)))
     ((and (eq anvil-worker--last-state 'alive) (not alive))
      (anvil-worker--log 'death "respawning")
      (setq anvil-worker--last-state 'dead)
      (anvil-worker-spawn))
     ((and (eq anvil-worker--last-state 'dead) alive)
      (anvil-worker--log 'recovered)
      (setq anvil-worker--last-state 'alive)))))

;;;###autoload
(defun anvil-worker-health-timer-start ()
  "Start periodic worker health checks with auto-respawn."
  (interactive)
  (when (timerp anvil-worker--health-timer)
    (cancel-timer anvil-worker--health-timer))
  (setq anvil-worker--last-state nil)
  (setq anvil-worker--health-timer
        (run-with-timer anvil-worker-health-check-interval
                        anvil-worker-health-check-interval
                        #'anvil-worker--health-check))
  (anvil-worker--log 'timer-started
                     (format "interval=%ds" anvil-worker-health-check-interval))
  (message "Anvil worker health timer started (%ds)"
           anvil-worker-health-check-interval))

(defun anvil-worker-health-timer-stop ()
  "Stop periodic worker health checks."
  (interactive)
  (when (timerp anvil-worker--health-timer)
    (cancel-timer anvil-worker--health-timer)
    (setq anvil-worker--health-timer nil)
    (anvil-worker--log 'timer-stopped)
    (message "Anvil worker health timer stopped")))

;;; Module enable/disable

(defun anvil-worker-enable ()
  "Spawn the worker daemon and start health monitoring."
  (anvil-worker-spawn)
  (anvil-worker-health-timer-start))

(defun anvil-worker-disable ()
  "Stop health monitoring (does not kill the worker)."
  (anvil-worker-health-timer-stop))

(provide 'anvil-worker)
;;; anvil-worker.el ends here
