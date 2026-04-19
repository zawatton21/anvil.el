;;; anvil-pty-broker.el --- node-pty TCP broker client -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 1 MVP of the pty-broker (docs/design/04-pty-broker.org).
;;
;; A thin Elisp client for `pty-broker/anvil-pty-broker.js', a Node.js
;; process that runs `node-pty' and exposes a JSON-line TCP protocol.
;; The broker lets anvil drive TUI programs (bash, claude, aider, ...)
;; without putting any PTY code inside the Emacs process itself — so
;; filter starvation (feedback_eat_daemon_filter_starvation.md) and
;; ConPTY stdin quirks (feedback_conpty_headless_stdin_unidirectional.md)
;; stop mattering.
;;
;; Phase 1 scope:
;;   * auto-spawn the Node broker on `anvil-pty-broker-enable'
;;   * TCP auth via token (random, never persisted)
;;   * Elisp API: `anvil-pty-spawn', `anvil-pty-send', `anvil-pty-kill',
;;     `anvil-pty-list', `anvil-pty-read'
;;   * MCP tools: pty-spawn / pty-send / pty-kill / pty-list / pty-read
;;   * timer-only I/O — network filter just appends to a string buffer;
;;     all parsing runs in a `run-at-time' tick
;;   * command allowlist — empty by default (no `spawn' possible)
;;
;; Deferred to Phase 2+: resize, rich render (ANSI), multi-client,
;; persistent sessions, headless JSONL mode for claude.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'anvil-server)

(defgroup anvil-pty-broker nil
  "node-pty broker client for anvil."
  :group 'anvil
  :prefix "anvil-pty-broker-")

(defconst anvil-pty-broker--server-id "emacs-eval"
  "MCP server id the pty-broker tools register under.")

(defcustom anvil-pty-broker-node-binary "node"
  "Name or absolute path of the Node.js binary."
  :type 'string
  :group 'anvil-pty-broker)

(defcustom anvil-pty-broker-script
  (expand-file-name "pty-broker/anvil-pty-broker.js"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "Absolute path to `anvil-pty-broker.js'."
  :type 'file
  :group 'anvil-pty-broker)

(defcustom anvil-pty-broker-port 0
  "TCP port to ask the broker to listen on.  0 means dynamic."
  :type 'integer
  :group 'anvil-pty-broker)

(defcustom anvil-pty-broker-host "127.0.0.1"
  "Host the broker listens on; keep localhost-only."
  :type 'string
  :group 'anvil-pty-broker)

(defcustom anvil-pty-broker-allowed-commands nil
  "Commands the broker may spawn.  Empty list = no spawning permitted.
Each element is a program name as passed to `anvil-pty-spawn' (e.g.
\"bash\", \"claude\")."
  :type '(repeat string)
  :group 'anvil-pty-broker)

(defcustom anvil-pty-broker-poll-interval 0.05
  "Seconds between timer-driven drains of the TCP receive buffer."
  :type 'number
  :group 'anvil-pty-broker)

(defcustom anvil-pty-broker-startup-timeout 5.0
  "Seconds to wait for the Node broker to print its ready line."
  :type 'number
  :group 'anvil-pty-broker)

(defcustom anvil-pty-broker-auth-timeout 3.0
  "Seconds to wait for the broker to acknowledge our auth frame."
  :type 'number
  :group 'anvil-pty-broker)

(defvar anvil-pty-broker--node-proc nil)
(defvar anvil-pty-broker--node-stderr-buf nil)
(defvar anvil-pty-broker--net-proc nil)
(defvar anvil-pty-broker--port nil)
(defvar anvil-pty-broker--token nil)
(defvar anvil-pty-broker--authed nil)
(defvar anvil-pty-broker--recv-buf ""
  "Raw bytes received from the broker; drained by the tick timer.")
(defvar anvil-pty-broker--timer nil)
(defvar anvil-pty-broker--ptys nil
  "Hash-table: pty-id → plist (:pid :exit :output STRING :events LIST).")

(defun anvil-pty-broker--gen-token ()
  "Return a 32-char URL-safe random token."
  (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
        (out (make-string 32 ?x)))
    (dotimes (i 32)
      (aset out i (aref chars (random (length chars)))))
    out))

(defun anvil-pty-broker--encode-frame (plist)
  "Return JSON + LF for PLIST."
  (concat (json-encode plist) "\n"))

(defun anvil-pty-broker--send-frame (plist)
  "Send PLIST as one JSON line to the broker."
  (unless (and anvil-pty-broker--net-proc
               (process-live-p anvil-pty-broker--net-proc))
    (error "anvil-pty-broker: not connected"))
  (process-send-string anvil-pty-broker--net-proc
                       (anvil-pty-broker--encode-frame plist)))

(defun anvil-pty-broker--await-ready (proc timeout)
  "Scan PROC's buffer for the broker ready line; return port or nil."
  (let ((deadline (+ (float-time) timeout))
        port)
    (while (and (not port) (< (float-time) deadline) (process-live-p proc))
      (with-current-buffer (process-buffer proc)
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward
                 "\"ready\"\\s-*:\\s-*true.*?\"port\"\\s-*:\\s-*\\([0-9]+\\)"
                 nil t)
            (setq port (string-to-number (match-string 1))))))
      (unless port
        (accept-process-output proc 0.1)))
    port))

(defun anvil-pty-broker--tcp-filter (_proc chunk)
  "Starvation-safe filter: just append CHUNK to `recv-buf'."
  (setq anvil-pty-broker--recv-buf
        (concat anvil-pty-broker--recv-buf chunk)))

(defun anvil-pty-broker--tcp-sentinel (proc event)
  "Log and clean up when the TCP connection goes away."
  (when (memq (process-status proc) '(closed failed exit signal))
    (message "anvil-pty-broker: TCP %s (%s)" (process-status proc)
             (string-trim event))))

(defun anvil-pty-broker--drain-tick ()
  "Parse pending frames from `recv-buf' and update pty state."
  (let ((buf anvil-pty-broker--recv-buf))
    (while (let ((idx (string-match "\n" buf)))
             (when idx
               (let ((line (substring buf 0 idx)))
                 (setq buf (substring buf (1+ idx)))
                 (unless (string-empty-p line)
                   (condition-case err
                       (anvil-pty-broker--handle-frame
                        (let ((json-object-type 'plist)
                              (json-array-type 'list)
                              (json-key-type 'keyword))
                          (json-read-from-string line)))
                     (error
                      (message "anvil-pty-broker: bad frame %s: %s"
                               line err))))
                 t))))
    (setq anvil-pty-broker--recv-buf buf)))

(defun anvil-pty-broker--get-pty (id)
  "Return the plist for pty ID, creating an empty row if absent."
  (or (gethash id anvil-pty-broker--ptys)
      (puthash id (list :output "" :events nil)
               anvil-pty-broker--ptys)))

(defun anvil-pty-broker--record-event (id ev-plist)
  "Append EV-PLIST to ID's :events list."
  (let ((row (anvil-pty-broker--get-pty id)))
    (plist-put row :events (append (plist-get row :events) (list ev-plist)))))

(defun anvil-pty-broker--handle-frame (plist)
  "Dispatch one parsed broker event PLIST."
  (let ((ev (plist-get plist :ev)))
    (cond
     ((equal ev "authed")
      (setq anvil-pty-broker--authed t))
     ((equal ev "spawned")
      (let* ((id (plist-get plist :id))
             (row (anvil-pty-broker--get-pty id)))
        (plist-put row :pid (plist-get plist :pid))))
     ((equal ev "output")
      (let* ((id (plist-get plist :id))
             (row (anvil-pty-broker--get-pty id))
             (b64 (or (plist-get plist :data) "")))
        (plist-put row :output
                   (concat (plist-get row :output)
                           (base64-decode-string b64)))))
     ((equal ev "exit")
      (let* ((id (plist-get plist :id))
             (row (anvil-pty-broker--get-pty id)))
        (plist-put row :exit (plist-get plist :code))
        (anvil-pty-broker--record-event id plist)))
     ((equal ev "list")
      (anvil-pty-broker--record-event :meta plist))
     ((equal ev "error")
      (let ((id (or (plist-get plist :id) :meta)))
        (anvil-pty-broker--record-event id plist)))
     (t
      (anvil-pty-broker--record-event :meta plist)))))

(defun anvil-pty-broker--wait-for (predicate timeout)
  "Run the drain tick until PREDICATE returns non-nil or TIMEOUT seconds pass."
  (let ((deadline (+ (float-time) timeout))
        ok)
    (while (and (not (setq ok (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output anvil-pty-broker--net-proc 0.05)
      (anvil-pty-broker--drain-tick))
    ok))

;;;; --- public API ---------------------------------------------------------

;;;###autoload
(defun anvil-pty-broker-enable ()
  "Start the Node broker, connect, auth, and register MCP tools."
  (interactive)
  (when (and anvil-pty-broker--net-proc
             (process-live-p anvil-pty-broker--net-proc))
    (error "anvil-pty-broker: already enabled"))
  (unless (executable-find anvil-pty-broker-node-binary)
    (error "anvil-pty-broker: node not found (%s)" anvil-pty-broker-node-binary))
  (unless (file-exists-p anvil-pty-broker-script)
    (error "anvil-pty-broker: script not found: %s" anvil-pty-broker-script))
  (setq anvil-pty-broker--ptys (make-hash-table :test 'equal))
  (setq anvil-pty-broker--recv-buf ""
        anvil-pty-broker--authed nil
        anvil-pty-broker--port nil
        anvil-pty-broker--token (anvil-pty-broker--gen-token))
  (let* ((stdout-buf (generate-new-buffer " *anvil-pty-broker-stdout*"))
         (stderr-buf (generate-new-buffer " *anvil-pty-broker-stderr*"))
         (args (append
                (list anvil-pty-broker-script
                      "--port" (number-to-string anvil-pty-broker-port)
                      "--token" anvil-pty-broker--token)
                (cl-loop for c in anvil-pty-broker-allowed-commands
                         append (list "--allow" c))))
         (proc (make-process
                :name "anvil-pty-broker-node"
                :buffer stdout-buf
                :stderr stderr-buf
                :command (cons anvil-pty-broker-node-binary args)
                :noquery t)))
    (setq anvil-pty-broker--node-proc proc
          anvil-pty-broker--node-stderr-buf stderr-buf)
    (let ((port (anvil-pty-broker--await-ready
                 proc anvil-pty-broker-startup-timeout)))
      (unless port
        (let ((err (with-current-buffer stderr-buf (buffer-string))))
          (ignore-errors (delete-process proc))
          (setq anvil-pty-broker--node-proc nil)
          (error "anvil-pty-broker: broker did not become ready%s"
                 (if (string-empty-p err) ""
                   (format "; stderr=%s" (string-trim err))))))
      (setq anvil-pty-broker--port port))
    (let ((net (make-network-process
                :name "anvil-pty-broker-tcp"
                :host anvil-pty-broker-host
                :service anvil-pty-broker--port
                :filter #'anvil-pty-broker--tcp-filter
                :sentinel #'anvil-pty-broker--tcp-sentinel
                :coding 'no-conversion
                :noquery t)))
      (setq anvil-pty-broker--net-proc net))
    (anvil-pty-broker--send-frame
     `(:op "auth" :token ,anvil-pty-broker--token))
    (unless (anvil-pty-broker--wait-for
             (lambda () anvil-pty-broker--authed)
             anvil-pty-broker-auth-timeout)
      (anvil-pty-broker--teardown)
      (error "anvil-pty-broker: auth failed"))
    (setq anvil-pty-broker--timer
          (run-with-timer anvil-pty-broker-poll-interval
                          anvil-pty-broker-poll-interval
                          #'anvil-pty-broker--drain-tick))
    (anvil-pty-broker--register-tools)
    (message "anvil-pty-broker: ready on port %d" anvil-pty-broker--port)
    anvil-pty-broker--port))

;;;###autoload
(defun anvil-pty-broker-disable ()
  "Shut the broker down and unregister MCP tools."
  (interactive)
  (anvil-pty-broker--unregister-tools)
  (anvil-pty-broker--teardown)
  (message "anvil-pty-broker: disabled"))

(defun anvil-pty-broker--teardown ()
  "Close TCP, kill the Node process, cancel the timer."
  (when anvil-pty-broker--timer
    (cancel-timer anvil-pty-broker--timer)
    (setq anvil-pty-broker--timer nil))
  (when (and anvil-pty-broker--net-proc
             (process-live-p anvil-pty-broker--net-proc))
    (ignore-errors (delete-process anvil-pty-broker--net-proc)))
  (setq anvil-pty-broker--net-proc nil
        anvil-pty-broker--authed nil
        anvil-pty-broker--recv-buf "")
  (when (and anvil-pty-broker--node-proc
             (process-live-p anvil-pty-broker--node-proc))
    (ignore-errors (delete-process anvil-pty-broker--node-proc)))
  (setq anvil-pty-broker--node-proc nil
        anvil-pty-broker--port nil
        anvil-pty-broker--token nil))

(defun anvil-pty-spawn (cmd &rest opts)
  "Spawn CMD via the broker.  Returns the pty-id string.
OPTS is a plist accepting :args :cwd :cols :rows :id."
  (unless anvil-pty-broker--authed
    (error "anvil-pty-broker: not authed (call anvil-pty-broker-enable)"))
  (let ((id (or (plist-get opts :id)
                (format "pty-%06d" (random 1000000)))))
    (anvil-pty-broker--send-frame
     (list :op "spawn"
           :id id
           :cmd cmd
           :args (or (plist-get opts :args) [])
           :cwd (or (plist-get opts :cwd) default-directory)
           :cols (or (plist-get opts :cols) 80)
           :rows (or (plist-get opts :rows) 24)))
    id))

(defun anvil-pty-send (id text)
  "Send TEXT (as base64) to pty ID."
  (anvil-pty-broker--send-frame
   (list :op "input" :id id :b64 t
         :data (base64-encode-string (encode-coding-string text 'utf-8) t))))

(defun anvil-pty-kill (id &optional signal)
  "Kill pty ID with optional SIGNAL (default SIGTERM)."
  (anvil-pty-broker--send-frame
   (list :op "kill" :id id :signal (or signal "SIGTERM"))))

(defun anvil-pty-list ()
  "Ask the broker for active pty ids; return the list (after a brief wait)."
  (let ((meta-row (gethash :meta anvil-pty-broker--ptys)))
    (when meta-row (plist-put meta-row :events nil)))
  (anvil-pty-broker--send-frame '(:op "list"))
  (anvil-pty-broker--wait-for
   (lambda ()
     (let ((row (gethash :meta anvil-pty-broker--ptys)))
       (when row
         (cl-some (lambda (ev) (equal (plist-get ev :ev) "list"))
                  (plist-get row :events)))))
   1.0)
  (let* ((row (gethash :meta anvil-pty-broker--ptys))
         (ev (cl-find-if (lambda (e) (equal (plist-get e :ev) "list"))
                         (plist-get row :events))))
    (plist-get ev :ids)))

(defun anvil-pty-read (id &optional consume)
  "Return the accumulated output for pty ID.
When CONSUME is non-nil, clear the buffer after reading."
  (let* ((row (gethash id anvil-pty-broker--ptys))
         (out (and row (plist-get row :output))))
    (when (and row consume)
      (plist-put row :output ""))
    (or out "")))

;;;; --- MCP tool wrappers --------------------------------------------------

(defun anvil-pty-broker--tool-spawn (cmd &optional args cwd cols rows)
  "MCP wrapper for `anvil-pty-spawn'.

MCP Parameters:
  cmd  - Program to run (string).  Must appear in
         `anvil-pty-broker-allowed-commands' or the broker rejects.
  args - Optional list of argv strings.  Elisp caller: list, MCP
         caller: JSON array of strings.
  cwd  - Optional working directory.
  cols - Optional TTY columns (default 80).
  rows - Optional TTY rows (default 24)."
  (let ((arg-list
         (cond ((null args) nil)
               ((listp args) args)
               ((vectorp args) (append args nil))
               ((stringp args)
                (if (string-empty-p args) nil
                  (split-string args "\\s-+" t)))
               (t (signal 'wrong-type-argument (list 'list-or-vector args))))))
    (anvil-pty-spawn cmd
                     :args arg-list
                     :cwd (and (stringp cwd) (not (string-empty-p cwd)) cwd)
                     :cols (or cols 80)
                     :rows (or rows 24))))

(defun anvil-pty-broker--tool-send (id text)
  "MCP wrapper for `anvil-pty-send'.

MCP Parameters:
  id   - pty id returned from pty-spawn.
  text - UTF-8 text to write into the pty's stdin.  Append \"\\n\"
         yourself if the command needs a newline."
  (anvil-pty-send id text)
  (list :sent (length text)))

(defun anvil-pty-broker--tool-kill (id &optional signal)
  "MCP wrapper for `anvil-pty-kill'.

MCP Parameters:
  id     - pty id.
  signal - Optional signal name (default SIGTERM)."
  (anvil-pty-kill id (and (stringp signal)
                          (not (string-empty-p signal))
                          signal))
  (list :killed id))

(defun anvil-pty-broker--tool-list ()
  "MCP wrapper for `anvil-pty-list'."
  (list :ids (or (anvil-pty-list) nil)))

(defun anvil-pty-broker--tool-read (id &optional consume)
  "MCP wrapper for `anvil-pty-read'.

MCP Parameters:
  id      - pty id.
  consume - When non-nil (t/non-empty string), clear the output
            buffer after reading."
  (let* ((consume-p
          (cond ((null consume) nil)
                ((eq consume t) t)
                ((stringp consume)
                 (not (member (downcase consume) '("" "nil" "false" "0"))))
                (t t))))
    (list :id id
          :output (anvil-pty-read id consume-p)
          :consumed consume-p)))

(defun anvil-pty-broker--register-tools ()
  "Register pty-broker MCP tools.  Idempotent."
  (anvil-server-register-tool
   #'anvil-pty-broker--tool-spawn
   :id "pty-spawn"
   :server-id anvil-pty-broker--server-id
   :description "Spawn a program under the node-pty broker. Requires
the program to appear in `anvil-pty-broker-allowed-commands'. Returns
the pty-id string to use with pty-send / pty-read / pty-kill.")
  (anvil-server-register-tool
   #'anvil-pty-broker--tool-send
   :id "pty-send"
   :server-id anvil-pty-broker--server-id
   :description "Write TEXT into an existing pty's stdin.")
  (anvil-server-register-tool
   #'anvil-pty-broker--tool-kill
   :id "pty-kill"
   :server-id anvil-pty-broker--server-id
   :description "Send a signal (default SIGTERM) to an existing pty.")
  (anvil-server-register-tool
   #'anvil-pty-broker--tool-list
   :id "pty-list"
   :server-id anvil-pty-broker--server-id
   :description "Return the list of active pty ids."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-pty-broker--tool-read
   :id "pty-read"
   :server-id anvil-pty-broker--server-id
   :description "Return buffered output for a pty; consume=t clears
the buffer so the next read only sees new bytes."
   :read-only t))

(defun anvil-pty-broker--unregister-tools ()
  "Unregister the pty-broker MCP tools.  Safe when not registered."
  (dolist (id '("pty-spawn" "pty-send" "pty-kill" "pty-list" "pty-read"))
    (ignore-errors
      (anvil-server-unregister-tool id anvil-pty-broker--server-id))))

(provide 'anvil-pty-broker)

;;; anvil-pty-broker.el ends here
