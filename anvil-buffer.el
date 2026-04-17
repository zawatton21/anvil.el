;;; anvil-buffer.el --- Explicit buffer-* MCP tools for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Implements the Phase 3 buffer-first MCP tools of the Disk-First
;; Policy design (see docs/design/05-disk-first.org):
;;
;;   - `buffer-read'           — read the live text of a named buffer
;;   - `buffer-save'           — persist the buffer to disk, refusing
;;                                when the file has been rewritten
;;                                since the buffer last visited it
;;   - `buffer-list-modified'  — enumerate all file-visiting, modified
;;                                buffers with their divergence status
;;
;; Rationale: `file-*' tools intentionally ignore visited buffers and
;; always read/write disk.  When an AI genuinely needs to peek at
;; unsaved editor state (debug views, human-in-the-loop workflows) or
;; needs to commit live edits to disk with a human-style `save-buffer',
;; it must opt into the `buffer-*' variant so the contract is explicit.
;;
;; Saves bypass `save-buffer' hooks by design: the MCP endpoint must be
;; deterministic and non-interactive, so we use `write-region' + mark
;; the buffer unmodified.  Callers that need hooks (gofmt-on-save etc.)
;; should invoke the editor's own `save-buffer' interactively.

;;; Code:

(require 'anvil-server)
(require 'anvil-disk)
(require 'cl-lib)

(defgroup anvil-buffer nil
  "Anvil buffer-* MCP tools."
  :group 'anvil
  :prefix "anvil-buffer-")

(defconst anvil-buffer--server-id "emacs-eval"
  "Server ID for this module's MCP tools.")

;;;; --- internals -----------------------------------------------------------

(defun anvil-buffer--require-live (name)
  "Return the live buffer named NAME or signal `user-error'."
  (let ((buf (get-buffer name)))
    (unless (and buf (buffer-live-p buf))
      (user-error "anvil-buffer: no live buffer named %S" name))
    buf))

(defun anvil-buffer--truthy-arg (v)
  "Treat MCP string arg V as boolean.
Nil, empty string, \"false\", \"nil\", \"0\" are false.  Anything
else (\"t\", \"true\", \"1\") is true."
  (cond
   ((null v) nil)
   ((stringp v)
    (let ((s (string-trim (downcase v))))
      (not (member s '("" "nil" "false" "0")))))
   (t (and v t))))

(defun anvil-buffer--describe (buf)
  "Return a plist describing BUF: name, file, size, modified flag, status."
  (with-current-buffer buf
    (let* ((file (buffer-file-name))
           (div  (and file (anvil-disk-buffer-divergence file))))
      (list :buffer   (buffer-name buf)
            :file     file
            :size     (buffer-size)
            :modified (and (buffer-modified-p buf) t)
            :status   (if div (plist-get div :status) 'no-file)))))

;;;; --- buffer-read ---------------------------------------------------------

(defun anvil-buffer-read (name)
  "Return a plist with the content of the live buffer NAME.
Signals `user-error' when no such buffer exists.  Includes any
divergence :warnings for callers comparing against disk."
  (let* ((buf      (anvil-buffer--require-live name))
         (info     (anvil-buffer--describe buf))
         (file     (plist-get info :file))
         (warnings (and file (anvil-file-warn-if-diverged file)))
         (content  (with-current-buffer buf (buffer-string))))
    (append info (list :content content :warnings warnings))))

(defun anvil-buffer--tool-read (name)
  "MCP wrapper for `anvil-buffer-read'.

MCP Parameters:
  name - The buffer name to read (string, required).  Must match
         an existing live buffer."
  (anvil-server-with-error-handling
   (format "%S" (anvil-buffer-read name))))

;;;; --- buffer-save ---------------------------------------------------------

(defun anvil-buffer-save (name &optional force)
  "Persist the live buffer NAME to its visited file.
When the buffer does not visit a file, signal `user-error'.  When
the status is `disk-newer' (file rewritten externally since the
buffer visited it), refuse unless FORCE is non-nil.

Returns a plist:
  (:buffer NAME :file FILE :bytes-written N :forced BOOL :status STATUS
   :saved BOOL)
`:saved' is nil when the buffer was already unmodified (noop)."
  (let* ((buf  (anvil-buffer--require-live name))
         (file (buffer-file-name buf)))
    (unless file
      (user-error "anvil-buffer: buffer %S does not visit a file" name))
    (let* ((div    (anvil-disk-buffer-divergence file))
           (status (and div (plist-get div :status))))
      (when (and (not force)
                 (memq status '(disk-newer both-modified)))
        (user-error
         "anvil-buffer-save: %S would clobber disk-newer content at %s \
(status %s); pass force=t to override"
         name file status))
      (let ((modified (buffer-modified-p buf))
            bytes)
        (if (not modified)
            (list :buffer name :file file :bytes-written 0
                  :forced (and force t) :status status :saved nil)
          (with-current-buffer buf
            (let ((coding-system-for-write 'utf-8-unix))
              ;; Divergence already checked + force guard applied above;
              ;; Emacs' supersession prompt is redundant and errors out in
              ;; batch mode.  Silence BOTH the public and the internal
              ;; userlock helper — Emacs 29+ routes `write-region' through
              ;; `userlock--ask-user-about-supersession-threat' which throws
              ;; "Cannot resolve conflict in batch mode" directly.
              (cl-letf (((symbol-function 'ask-user-about-supersession-threat)
                         #'ignore)
                        ((symbol-function 'userlock--ask-user-about-supersession-threat)
                         #'ignore))
                (write-region (point-min) (point-max) file nil 'silent)))
            (set-buffer-modified-p nil)
            (set-visited-file-modtime)
            (setq bytes (- (position-bytes (point-max))
                           (position-bytes (point-min)))))
          (list :buffer name :file file :bytes-written bytes
                :forced (and force t) :status status :saved t))))))

(defun anvil-buffer--tool-save (name &optional force)
  "MCP wrapper for `anvil-buffer-save'.

MCP Parameters:
  name  - Buffer name to save (string, required).
  force - When truthy (\"t\" / \"true\" / \"1\"), override the
          refusal that fires when the file on disk has been
          rewritten since the buffer last visited it (status
          `disk-newer' or `both-modified')."
  (anvil-server-with-error-handling
   (format "%S" (anvil-buffer-save name (anvil-buffer--truthy-arg force)))))

;;;; --- buffer-list-modified ------------------------------------------------

(defun anvil-buffer-list-modified ()
  "Return a plist listing every file-visiting modified buffer.
Shape:
  (:count N :buffers (PLIST ...))
where each entry plist carries `:buffer', `:file', `:size',
`:modified', `:status'.  Unsaved scratch / *Messages* style
buffers are omitted because they have no visited file."
  (let (entries)
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (buffer-file-name buf)
                 (buffer-modified-p buf))
        (push (anvil-buffer--describe buf) entries)))
    (setq entries (nreverse entries))
    (list :count (length entries) :buffers entries)))

(defun anvil-buffer--tool-list-modified ()
  "MCP wrapper for `anvil-buffer-list-modified'.

MCP Parameters: none.  Returns a printed plist with every
file-visiting modified buffer plus its divergence status — useful
to answer \"what does the human have open and unsaved right
now?\" without scanning disk."
  (anvil-server-with-error-handling
   (format "%S" (anvil-buffer-list-modified))))

;;;; --- module lifecycle ----------------------------------------------------

;;;###autoload
(defun anvil-buffer-enable ()
  "Register the buffer-* MCP tools."
  (anvil-server-register-tool
   #'anvil-buffer--tool-read
   :id "buffer-read"
   :server-id anvil-buffer--server-id
   :description
   "Read the live text of a named Emacs buffer.  Unlike `file-read',
this returns whatever the editor currently holds — including
unsaved edits — so the buffer's `:modified' flag and divergence
`:status' are meaningful.  Errors when the buffer does not exist."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-buffer--tool-save
   :id "buffer-save"
   :server-id anvil-buffer--server-id
   :description
   "Write the named buffer's content to its visited file.  Refuses
when the disk content has been rewritten since the buffer last
visited the file (status `disk-newer' or `both-modified') unless
`force' is truthy.  Uses `write-region' + unmodified flag, so
`save-buffer' hooks (auto-formatters etc.) are NOT fired.")
  (anvil-server-register-tool
   #'anvil-buffer--tool-list-modified
   :id "buffer-list-modified"
   :server-id anvil-buffer--server-id
   :description
   "List every file-visiting buffer that has unsaved edits, with
each buffer's visited file, character count, modified flag, and
disk/buffer divergence status.  Meant to answer \"what does the
human have open and unsaved right now?\""
   :read-only t))

(defun anvil-buffer-disable ()
  "Unregister the buffer-* MCP tools."
  (dolist (id '("buffer-read" "buffer-save" "buffer-list-modified"))
    (anvil-server-unregister-tool id anvil-buffer--server-id)))

(provide 'anvil-buffer)
;;; anvil-buffer.el ends here
