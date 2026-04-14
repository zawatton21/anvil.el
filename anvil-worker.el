;;; anvil-worker.el --- Worker pool for anvil — protect the human's Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Fujisawa Electric Management Office

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Emacs is the human's tool first.  AI should never freeze the
;; editor.  This module spawns a pool of isolated sub-Emacs daemons
;; and routes MCP tool calls through them.
;;
;; Architecture:
;;
;;   AI client (Claude, GPT, local LLM, ...)
;;     └─ anvil-stdio.sh
;;          └─ emacsclient → main Emacs daemon
;;               └─ anvil-worker-call → least-busy worker
;;                    └─ emacsclient -f worker-N → worker Emacs N
;;
;; Worker pool:
;; - N workers (default 2), each a separate Emacs process
;; - Round-robin dispatch with busy tracking
;; - Auto-respawn crashed workers
;; - Heavy-op detection routes to dedicated heavy worker
;;
;; Heavy-op detection:
;; - Patterns like byte-compile, insert-file-contents, call-process
;;   are auto-detected in emacs-eval expressions
;; - Heavy ops get longer timeout + dedicated worker slot
;; - Light ops use the regular pool with short timeout

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup anvil-worker nil
  "Anvil worker pool — isolated execution for AI tool calls."
  :group 'anvil
  :prefix "anvil-worker-")

(defcustom anvil-worker-pool-size 2
  "Number of worker daemons in the pool.
More workers = more parallel AI sessions without blocking.
Each worker is a separate Emacs process (~30MB RAM)."
  :type 'integer
  :group 'anvil-worker)

(defcustom anvil-worker-emacs-bin (or (executable-find "emacs") "emacs")
  "Path to the Emacs binary used to spawn worker daemons."
  :type 'file
  :group 'anvil-worker)

(defcustom anvil-worker-init-file nil
  "Init file loaded into worker daemons.
If nil, anvil generates a minimal init that registers the eval tool."
  :type '(choice (const :tag "Auto-generate" nil) file)
  :group 'anvil-worker)

(defcustom anvil-worker-health-check-interval 30
  "Seconds between worker health checks."
  :type 'integer
  :group 'anvil-worker)

(defcustom anvil-worker-alive-check-timeout 2.0
  "Seconds to wait for the `emacsclient -e t' liveness probe.
If the probe does not return within this window, the worker is
treated as dead and the probe process is killed.  This guards
against stale server files whose TCP port has been reused by a
process that accepts the connection but never replies to the
Emacs server auth protocol — which would otherwise hang the
probing daemon indefinitely."
  :type 'number
  :group 'anvil-worker)

(defcustom anvil-worker-call-timeout 60
  "Default timeout in seconds for normal worker calls."
  :type 'integer
  :group 'anvil-worker)

(defcustom anvil-worker-heavy-timeout 300
  "Timeout in seconds for heavy operations (byte-compile, etc)."
  :type 'integer
  :group 'anvil-worker)

(defcustom anvil-worker-spawn-wait 5
  "Max seconds to wait for a freshly spawned worker."
  :type 'integer
  :group 'anvil-worker)

(defcustom anvil-worker-lifecycle-log
  (expand-file-name "anvil-worker.log" user-emacs-directory)
  "Append-only log of worker lifecycle events."
  :type 'file
  :group 'anvil-worker)

;;; Heavy-op detection

(defcustom anvil-worker-heavy-patterns
  '(("byte-compile"         . "(byte-compile")
    ("insert-file-contents" . "(insert-file-contents")
    ("call-process"         . "(call-process\\b")
    ("shell-command"        . "(shell-command")
    ("org-babel-tangle"     . "(org-babel-tangle")
    ("elfeed-update"        . "(elfeed-update")
    ("package-install"      . "(package-install")
    ("url-retrieve-sync"    . "(url-retrieve-synchronously")
    ("make-network-process" . "(make-network-process")
    ("find-file"            . "(find-file[- ]"))
  "Alist of (LABEL . REGEX) for heavy operation detection.
Expressions matching these patterns get routed with longer timeout."
  :type '(alist :key-type string :value-type string)
  :group 'anvil-worker)

;;; Pool state

(defvar anvil-worker--pool nil
  "Vector of worker state plists.
Each entry: (:name STRING :server-file STRING :busy BOOLEAN :last-state SYMBOL)")

(defvar anvil-worker--health-timer nil
  "Repeating timer for pool health checks.")

(defvar anvil-worker--dispatch-index 0
  "Round-robin index for worker dispatch.")

;;; Internal helpers

(defun anvil-worker--name (index)
  "Return daemon name for worker at INDEX."
  (format "anvil-worker-%d" (1+ index)))

(defun anvil-worker--server-file (index)
  "Return server file path for worker at INDEX."
  (expand-file-name
   (concat "server/" (anvil-worker--name index))
   user-emacs-directory))

(defun anvil-worker--log (event &optional details)
  "Append EVENT with optional DETAILS to the lifecycle log."
  (let ((line (format "%s [%s]%s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      event
                      (if details (concat " " details) "")))
        (coding-system-for-write 'utf-8-unix))
    (write-region line nil anvil-worker-lifecycle-log 'append 'no-message)))

(defun anvil-worker--generate-init-file ()
  "Generate a minimal init file for worker daemons.
Always regenerates.  A cached file can go stale when anvil itself
moves on disk (the previous path gets baked into `load-path'), so
we unconditionally rewrite from the current `locate-library'
result.  Signals if `anvil-server' cannot be located."
  (let* ((init-file (expand-file-name "anvil-worker-init.el"
                                      user-emacs-directory))
         (located (locate-library "anvil-server"))
         (anvil-dir (and located (file-name-directory located))))
    (unless anvil-dir
      (error "anvil-worker: cannot locate anvil-server in load-path"))
    (with-temp-buffer
      (insert ";;; anvil-worker-init.el --- Auto-generated -*- lexical-binding: t; -*-\n\n")
      (insert (format "(add-to-list 'load-path %S)\n" anvil-dir))
      (insert "(require 'anvil-server)\n")
      (insert "(require 'anvil-server-commands)\n\n")
      (insert "(defun anvil-worker--eval (expression)\n")
      (insert "  \"Evaluate EXPRESSION on the worker daemon.\n\n")
      (insert "MCP Parameters:\n")
      (insert "  expression - Emacs Lisp expression as a string\"\n")
      (insert "  (anvil-server-with-error-handling\n")
      (insert "    (let ((result (eval (read expression) t)))\n")
      (insert "      (format \"%S\" result))))\n\n")
      ;; Register with a generic server-id; the actual name doesn't matter
      ;; because the worker is reached via emacsclient, not MCP stdio
      (insert "(anvil-server-register-tool #'anvil-worker--eval\n")
      (insert "  :id \"eval\"\n")
      (insert "  :description \"Evaluate Emacs Lisp on the isolated worker\"\n")
      (insert "  :server-id \"worker\")\n\n")
      (insert "(anvil-server-start)\n")
      (insert "(message \"[anvil-worker] ready\")\n")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max) init-file nil 'silent)))
    init-file))

(defun anvil-worker--init-pool ()
  "Initialize the pool state vector."
  (setq anvil-worker--pool
        (make-vector anvil-worker-pool-size nil))
  (dotimes (i anvil-worker-pool-size)
    (aset anvil-worker--pool i
          (list :name (anvil-worker--name i)
                :server-file (anvil-worker--server-file i)
                :busy nil
                :last-state nil))))

;;; Worker lifecycle

(defun anvil-worker--probe-emacsclient (server-file)
  "Return non-nil if `emacsclient -f SERVER-FILE -e t' exits 0 in time.
The probe is run asynchronously via `make-process' and hard-killed
after `anvil-worker-alive-check-timeout' seconds.  This prevents a
stale SERVER-FILE pointing at a reused TCP port from blocking the
caller indefinitely."
  (let* ((buf (generate-new-buffer " *anvil-worker-probe*"))
         (proc (make-process
                :name "anvil-worker-probe"
                :buffer buf
                :command (list "emacsclient" "-f" server-file "-e" "t")
                :noquery t
                :connection-type 'pipe))
         (deadline (+ (float-time) anvil-worker-alive-check-timeout))
         (alive nil))
    (unwind-protect
        (progn
          (while (and (process-live-p proc)
                      (< (float-time) deadline))
            (accept-process-output proc 0.1 nil t))
          (if (process-live-p proc)
              (progn
                (delete-process proc)
                (anvil-worker--log
                 'probe-timeout
                 (format "%s >%.1fs"
                         (file-name-nondirectory server-file)
                         anvil-worker-alive-check-timeout)))
            (setq alive (= 0 (process-exit-status proc)))))
      (when (buffer-live-p buf) (kill-buffer buf)))
    alive))

(defun anvil-worker--server-file-pid (server-file)
  "Return the PID recorded on the first line of SERVER-FILE.
Returns nil if the file cannot be read or the PID cannot be parsed.
Emacs server files start with `HOST:PORT PID' on line one."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents server-file nil 0 256)
      (goto-char (point-min))
      (when (re-search-forward "^\\S-+ \\([0-9]+\\)" (line-end-position) t)
        (string-to-number (match-string 1))))))

(defun anvil-worker--server-file-stale-p (server-file)
  "Return non-nil if SERVER-FILE's recorded PID is no longer running.
A missing PID (unparseable file) is treated as stale too."
  (let ((pid (anvil-worker--server-file-pid server-file)))
    (or (null pid)
        (null (process-attributes pid)))))

(defun anvil-worker-alive-p (&optional index)
  "Return non-nil if worker at INDEX (default 0) is reachable.
Performs three checks in order: file-exists, PID still alive, and
a bounded `emacsclient' probe.  The PID check catches the common
stale case (daemon crashed but socket file remained) cheaply —
without opening a TCP connection that might hang if the port has
since been reused.  A stale server file is deleted as a side
effect so the next spawn starts cleanly."
  (let* ((idx (or index 0))
         (server-file (anvil-worker--server-file idx)))
    (cond
     ((not (file-exists-p server-file)) nil)
     ((anvil-worker--server-file-stale-p server-file)
      (ignore-errors (delete-file server-file))
      (anvil-worker--log
       'stale-server-file
       (file-name-nondirectory server-file))
      nil)
     (t (anvil-worker--probe-emacsclient server-file)))))

(defun anvil-worker--spawn-one (index)
  "Spawn worker at INDEX if not already running."
  (if (anvil-worker-alive-p index)
      (anvil-worker--log 'spawn-skipped
                         (format "%s already alive" (anvil-worker--name index)))
    (let* ((init-file (or anvil-worker-init-file
                         (anvil-worker--generate-init-file)))
           (name (anvil-worker--name index))
           (proc (start-process
                  (format "anvil-worker-spawn-%d" index)
                  (get-buffer-create (format " *anvil-worker-%d*" index))
                  anvil-worker-emacs-bin
                  (concat "--fg-daemon=" name)
                  "-Q"
                  "-l" init-file)))
      (set-process-query-on-exit-flag proc nil)
      (anvil-worker--log 'spawn (format "%s pid=%d" name (process-id proc)))
      proc)))

;;;###autoload
(defun anvil-worker-spawn ()
  "Spawn all workers in the pool.  Idempotent."
  (interactive)
  (unless anvil-worker--pool
    (anvil-worker--init-pool))
  (dotimes (i anvil-worker-pool-size)
    (anvil-worker--spawn-one i))
  (message "Anvil worker pool: %d workers spawned" anvil-worker-pool-size))

;;;###autoload
(defun anvil-worker-kill ()
  "Kill all worker daemons."
  (interactive)
  (dotimes (i anvil-worker-pool-size)
    (when (anvil-worker-alive-p i)
      (ignore-errors
        (call-process "emacsclient" nil nil nil
                      "-f" (anvil-worker--server-file i)
                      "-e" "(kill-emacs)"))
      (anvil-worker--log 'killed (anvil-worker--name i))))
  (message "Anvil worker pool: all workers killed"))

;;; Dispatch — pick a worker

(defun anvil-worker--pick-worker ()
  "Pick the next available worker index.  Round-robin with busy skip."
  (unless anvil-worker--pool
    (anvil-worker--init-pool))
  (let ((start anvil-worker--dispatch-index)
        (size anvil-worker-pool-size)
        (chosen nil))
    ;; Try to find a non-busy, alive worker
    (dotimes (_ size)
      (let* ((idx (% (+ start _) size))
             (state (aref anvil-worker--pool idx)))
        (when (and (not chosen)
                   (not (plist-get state :busy))
                   (anvil-worker-alive-p idx))
          (setq chosen idx))))
    ;; Fallback: any alive worker (even busy)
    (unless chosen
      (dotimes (_ size)
        (let ((idx (% (+ start _) size)))
          (when (and (not chosen) (anvil-worker-alive-p idx))
            (setq chosen idx)))))
    ;; Last resort: try to spawn and use worker 0
    (unless chosen
      (anvil-worker--spawn-one 0)
      (let ((deadline (+ (float-time) anvil-worker-spawn-wait)))
        (while (and (not (anvil-worker-alive-p 0))
                    (< (float-time) deadline))
          (sit-for 0.1)))
      (when (anvil-worker-alive-p 0)
        (setq chosen 0)))
    (when chosen
      (setq anvil-worker--dispatch-index (% (1+ chosen) size)))
    chosen))

;;; Heavy-op detection

(defun anvil-worker-detect-heavy (expression)
  "Return list of matched heavy-op labels in EXPRESSION, or nil."
  (when (stringp expression)
    (let (matches)
      (dolist (p anvil-worker-heavy-patterns)
        (when (string-match-p (cdr p) expression)
          (push (car p) matches)))
      (nreverse matches))))

;;; Call worker

;;;###autoload
(defun anvil-worker-call (expression &optional timeout)
  "Run EXPRESSION on a worker daemon from the pool.
TIMEOUT defaults based on heavy-op detection.
Returns the worker's result as a string.
Auto-spawns workers if needed."
  (let* ((heavy (anvil-worker-detect-heavy expression))
         (timeout (or timeout
                      (if heavy anvil-worker-heavy-timeout
                        anvil-worker-call-timeout)))
         (idx (anvil-worker--pick-worker)))
    (unless idx
      (error "Anvil: no worker available (pool size %d)" anvil-worker-pool-size))
    (let ((state (aref anvil-worker--pool idx))
          (server-file (anvil-worker--server-file idx)))
      ;; Mark busy
      (plist-put state :busy t)
      (when heavy
        (anvil-worker--log 'heavy-dispatch
                           (format "%s patterns=%s"
                                   (anvil-worker--name idx)
                                   (mapconcat #'identity heavy ","))))
      (unwind-protect
          (let ((buf (get-buffer-create
                      (format " *anvil-worker-call-%d*" idx))))
            (with-current-buffer buf (erase-buffer))
            (let* ((proc (start-process
                          "anvil-worker-call" buf
                          "emacsclient"
                          "-f" server-file
                          "-e" expression))
                   (start (float-time)))
              (set-process-query-on-exit-flag proc nil)
              (while (and (process-live-p proc)
                          (< (- (float-time) start) timeout))
                (accept-process-output proc 0.1))
              (when (process-live-p proc)
                (kill-process proc)
                (error "Anvil worker %s timeout (%ds)"
                       (anvil-worker--name idx) timeout))
              (string-trim (with-current-buffer buf (buffer-string)))))
        ;; Mark not busy
        (plist-put state :busy nil)))))

;;; Pool status

;;;###autoload
(defun anvil-worker-status ()
  "Display pool status."
  (interactive)
  (unless anvil-worker--pool
    (anvil-worker--init-pool))
  (let ((lines '()))
    (dotimes (i anvil-worker-pool-size)
      (let* ((state (aref anvil-worker--pool i))
             (alive (anvil-worker-alive-p i))
             (busy (plist-get state :busy)))
        (push (format "  %s: %s%s"
                      (anvil-worker--name i)
                      (if alive "alive" "dead")
                      (if busy " [busy]" ""))
              lines)))
    (message "Anvil worker pool (%d):\n%s"
             anvil-worker-pool-size
             (mapconcat #'identity (nreverse lines) "\n"))))

;;; Health check

(defun anvil-worker--health-check ()
  "Check all workers; respawn dead ones.  Logs only transitions."
  (unless anvil-worker--pool
    (anvil-worker--init-pool))
  (dotimes (i anvil-worker-pool-size)
    (let* ((state (aref anvil-worker--pool i))
           (alive (anvil-worker-alive-p i))
           (last (plist-get state :last-state)))
      (cond
       ((null last)
        (plist-put state :last-state (if alive 'alive 'dead))
        (unless alive
          (anvil-worker--log 'startup-dead
                             (format "%s respawning" (anvil-worker--name i)))
          (anvil-worker--spawn-one i)))
       ((and (eq last 'alive) (not alive))
        (anvil-worker--log 'death
                           (format "%s respawning" (anvil-worker--name i)))
        (plist-put state :last-state 'dead)
        (anvil-worker--spawn-one i))
       ((and (eq last 'dead) alive)
        (anvil-worker--log 'recovered (anvil-worker--name i))
        (plist-put state :last-state 'alive))))))

;;;###autoload
(defun anvil-worker-health-timer-start ()
  "Start periodic pool health checks with auto-respawn."
  (interactive)
  (when (timerp anvil-worker--health-timer)
    (cancel-timer anvil-worker--health-timer))
  (setq anvil-worker--health-timer
        (run-with-timer anvil-worker-health-check-interval
                        anvil-worker-health-check-interval
                        #'anvil-worker--health-check))
  (anvil-worker--log 'timer-started
                     (format "pool=%d interval=%ds"
                             anvil-worker-pool-size
                             anvil-worker-health-check-interval))
  (message "Anvil worker health timer started (%ds, %d workers)"
           anvil-worker-health-check-interval
           anvil-worker-pool-size))

(defun anvil-worker-health-timer-stop ()
  "Stop periodic pool health checks."
  (interactive)
  (when (timerp anvil-worker--health-timer)
    (cancel-timer anvil-worker--health-timer)
    (setq anvil-worker--health-timer nil)
    (anvil-worker--log 'timer-stopped)
    (message "Anvil worker health timer stopped")))

;;; Module enable/disable

(defun anvil-worker-enable ()
  "Initialize pool, spawn workers, start health monitoring."
  (anvil-worker--init-pool)
  (anvil-worker-spawn)
  (anvil-worker-health-timer-start))

(defun anvil-worker-disable ()
  "Stop health monitoring (does not kill workers)."
  (anvil-worker-health-timer-stop))

(provide 'anvil-worker)
;;; anvil-worker.el ends here
