;;; anvil-disk.el --- Disk/buffer divergence guards for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Implements the Phase 1 helpers of the "Disk-First Policy" design
;; (see docs/design/05-disk-first.org).  Phase 1 is pure addition:
;;
;;   - `anvil-disk-buffer-divergence' — classify the state between a
;;     file on disk and its visited buffer, if any.
;;   - `anvil-file-warn-if-diverged'  — list of human-readable warnings.
;;   - `anvil-file-require-in-sync'   — signal when not in-sync.
;;   - `anvil-file-safe-write'        — write to disk but refuse when
;;     a visited buffer has unsaved edits, unless :force is passed.
;;
;; No MCP tool is registered here; the helpers are the shared
;; primitive that Phase 2 (file-* tool audit) and Phase 3 (buffer-*
;; opt-in variants) build on.

;;; Code:

(require 'cl-lib)

(defgroup anvil-disk nil
  "Anvil disk/buffer divergence guards."
  :group 'anvil
  :prefix "anvil-disk-")

;;;; --- divergence classification ------------------------------------------

(defun anvil-disk--modtime-unknown-p (modtime)
  "Return non-nil when MODTIME is a \"no recorded time\" sentinel.
Emacs returns 0 for buffers with no visited file and the
integer/list zero sentinels for intentionally stale buffers (see
`feedback_init_org_buffer_divergence.md').  Treating these as
unknown keeps the decision matrix from silently reporting
\"in-sync\"."
  (or (null modtime)
      (and (numberp modtime) (zerop modtime))
      (and (listp modtime)
           (cl-every (lambda (x) (and (numberp x) (zerop x))) modtime))))

(defun anvil-disk-buffer-divergence (file)
  "Return a divergence plist for FILE or nil if no visited buffer.
The plist has keys:
  :buffer        — the visited buffer object
  :disk-mtime    — current on-disk modification time, or nil
  :buffer-mtime  — buffer's recorded `visited-file-modtime'
  :disk-size     — current on-disk size in bytes, or nil
  :buffer-size   — visited buffer's size in characters
  :status        — one of
                   `in-sync'       (disk matches buffer's last visit, buffer unmodified)
                   `buffer-newer'  (buffer modified, disk unchanged since visit)
                   `disk-newer'    (disk changed since visit, buffer unmodified)
                   `both-modified' (buffer modified AND disk changed)
                   `unknown'       (buffer has no recorded modtime — intentional stale)

Returns nil when no buffer is currently visiting FILE.  The
function does not touch disk beyond a single `file-attributes'
call and does not modify the visited buffer."
  (let* ((abs (expand-file-name file)))
    (when-let* ((buf (find-buffer-visiting abs)))
      (let* ((modified (buffer-modified-p buf))
             (visited  (with-current-buffer buf (visited-file-modtime)))
             (attrs    (file-attributes abs))
             (disk-mt  (and attrs (file-attribute-modification-time attrs)))
             (disk-sz  (and attrs (file-attribute-size attrs)))
             (buf-sz   (with-current-buffer buf (buffer-size)))
             (status
              (cond
               ((anvil-disk--modtime-unknown-p visited) 'unknown)
               ((not (with-current-buffer buf
                       (verify-visited-file-modtime buf)))
                (if modified 'both-modified 'disk-newer))
               (modified 'buffer-newer)
               (t 'in-sync))))
        (list :buffer       buf
              :disk-mtime   disk-mt
              :buffer-mtime visited
              :disk-size    disk-sz
              :buffer-size  buf-sz
              :status       status)))))

;;;; --- warn / require ------------------------------------------------------

(defun anvil-file-warn-if-diverged (file)
  "Return a list of human-readable warning strings for FILE.
Empty list when FILE is in-sync or has no visited buffer.  Meant
to be embedded into tool response plists under `:warnings'."
  (let* ((div (anvil-disk-buffer-divergence file))
         (status (and div (plist-get div :status))))
    (cond
     ((or (null div) (eq status 'in-sync)) nil)
     ((eq status 'unknown)
      (list (format "anvil-disk: buffer %s has no recorded modtime \
(intentional stale buffer, disk read preferred)"
                    (buffer-name (plist-get div :buffer)))))
     (t
      (list (format "anvil-disk: %s — buffer=%s disk-size=%s buffer-size=%s"
                    status
                    (buffer-name (plist-get div :buffer))
                    (plist-get div :disk-size)
                    (plist-get div :buffer-size)))))))

(defun anvil-file-require-in-sync (file)
  "Signal `user-error' unless FILE is in-sync or has no visited buffer."
  (let* ((div (anvil-disk-buffer-divergence file))
         (status (and div (plist-get div :status))))
    (when (and div (not (eq status 'in-sync)))
      (user-error "anvil-file-require-in-sync: %s (status %s)"
                  file status))))

;;;; --- safe write ----------------------------------------------------------

(cl-defun anvil-file-safe-write (file content &key force)
  "Write CONTENT (string) to FILE, refusing when a visited buffer conflicts.

A buffer whose status is `buffer-newer' or `both-modified' has
unsaved edits that would be lost if the write proceeded without a
refresh.  In that case the function signals `user-error' unless
FORCE is non-nil.

`disk-newer' and `unknown' are not treated as a conflict: the
buffer has no pending changes (or the modtime is intentionally
zero), and a disk-authoritative overwrite is the documented policy
of the disk-first design.

Returns a plist:
  (:file ABS :status STATUS :forced t-or-nil :bytes-written N)"
  (let* ((abs (expand-file-name file))
         (div (anvil-disk-buffer-divergence abs))
         (status (and div (plist-get div :status))))
    (when (and (not force)
               (memq status '(buffer-newer both-modified)))
      (user-error
       "anvil-file-safe-write: buffer %s has unsaved edits for %s (status %s); \
pass :force t to override"
       (buffer-name (plist-get div :buffer)) abs status))
    (let ((coding-system-for-write 'utf-8-unix))
      ;; Divergence already checked + force guard applied above; Emacs'
      ;; supersession prompt is redundant and errors out in batch mode.
      ;; Silence BOTH the public and the internal userlock helper —
      ;; Emacs 29+ routes `write-region' through the internal variant
      ;; which throws "Cannot resolve conflict in batch mode" directly.
      (cl-letf (((symbol-function 'ask-user-about-supersession-threat)
                 #'ignore)
                ((symbol-function 'userlock--ask-user-about-supersession-threat)
                 #'ignore))
        (write-region content nil abs nil 'silent)))
    (list :file          abs
          :status        (or status 'no-buffer)
          :forced        (and force t)
          :bytes-written (string-bytes content))))

(provide 'anvil-disk)
;;; anvil-disk.el ends here
