;;; anvil-emacs.el --- Emacs buffer state inspection for anvil -*- lexical-binding: t; -*-

;; Author: zawatton21
;; Keywords: tools, mcp, claude, emacs

;;; Commentary:

;; anvil-helpers の姉妹ファイル。Claude Code が emacs-eval 経由で
;; Emacs server 内部の buffer 状態を 1 call で取得するための helper 群。
;;
;; 主要動機:
;;   memory `feedback_init_org_buffer_divergence.md' で頻発する
;;   「init.org buffer は disk と意図的 divergence」罠を恒久化するため、
;;   buffer/disk 不整合を 1 call で診断できるようにする。
;;
;; 戻り値:
;;   plist。buffer が見つからなければ nil。失敗時は errors しない。

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; --- internal -----------------------------------------------------------

(defun anvil-emacs--resolve-buffer (name-or-path)
  "Return a buffer object matching NAME-OR-PATH, or nil.
Tries: exact buffer name, visiting file (expanded), basename match."
  (or (and (stringp name-or-path) (get-buffer name-or-path))
      (and (stringp name-or-path)
           (find-buffer-visiting (expand-file-name name-or-path)))
      (and (stringp name-or-path)
           (let ((base (file-name-nondirectory name-or-path)))
             (cl-find-if (lambda (b)
                           (let ((bf (buffer-file-name b)))
                             (and bf (string= base (file-name-nondirectory bf)))))
                         (buffer-list))))))

(defun anvil-emacs--vmt-recorded-p (buf)
  "Return non-nil if BUF has a recorded visited-file-modtime.
The 0 / (0 . 0) form indicates `not recorded' (e.g. file changed
but buffer kept; visited-file-modtime explicitly cleared)."
  (with-current-buffer buf
    (let ((vmt (visited-file-modtime)))
      (cond
       ((eql vmt 0) nil)
       ((and (consp vmt) (numberp (car vmt)) (zerop (car vmt))
             (or (null (cdr vmt))
                 (and (numberp (cdr vmt)) (zerop (cdr vmt)))
                 (consp (cdr vmt)))) ; (0 0 ...) legacy form
        ;; (0 0 ...) means not recorded
        (and (consp (cdr vmt))
             (let ((rest (cdr vmt)))
               (cl-some (lambda (n) (and (numberp n) (not (zerop n))))
                        rest))))
       (t t)))))

;;;; --- public API ---------------------------------------------------------

(defun anvil-emacs-buffer-info (name-or-path)
  "Return diagnostic info for the buffer matching NAME-OR-PATH, or nil.

NAME-OR-PATH is matched in this order:
  1. exact buffer name (`get-buffer')
  2. visiting file (`find-buffer-visiting' on absolute path)
  3. basename match across `buffer-list'

Returned plist:
  (:name STR :file STR-or-nil :size N :mode SYMBOL :modified BOOL
   :vmt-recorded BOOL :on-disk-size N-or-nil :size-mismatch BOOL
   :verified BOOL :divergent BOOL)

  :verified      = `verify-visited-file-modtime' (nil if no file)
  :divergent     = the buffer disagrees with disk by any means
                   (verified=nil OR vmt-recorded=nil OR size-mismatch)
  :size-mismatch = on-disk-size != buffer-size (only meaningful when file exists)

This exists primarily to diagnose the init.org buffer divergence
trap (memory: feedback_init_org_buffer_divergence.md)."
  (let ((buf (anvil-emacs--resolve-buffer name-or-path)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (let* ((file (buffer-file-name))
               (size (buffer-size))
               (verified (and file (verify-visited-file-modtime buf)))
               (vmt-recorded (anvil-emacs--vmt-recorded-p buf))
               (on-disk (and file (file-exists-p file)
                             (file-attribute-size (file-attributes file))))
               (size-mismatch (and on-disk (/= size on-disk)))
               (divergent (or (and file (not verified))
                              (and file (not vmt-recorded))
                              size-mismatch)))
          (list :name           (buffer-name)
                :file           file
                :size           size
                :mode           major-mode
                :modified       (buffer-modified-p)
                :vmt-recorded   vmt-recorded
                :on-disk-size   on-disk
                :size-mismatch  (and size-mismatch t)
                :verified       (and verified t)
                :divergent      (and divergent t)))))))

(defun anvil-emacs-buffer-list (&optional filter)
  "Return brief info on all live buffers as a list of plists.

FILTER plist:
  :file-only t   only buffers visiting a file
  :match REGEXP  buffer name regexp
  :limit N       cap result count

Each element:
  (:name STR :file STR-or-nil :size N :mode SYMBOL :modified BOOL)"
  (let* ((file-only (plist-get filter :file-only))
         (regex     (plist-get filter :match))
         (limit     (plist-get filter :limit))
         results)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (or (not file-only) (buffer-file-name))
                   (or (not regex)
                       (string-match-p regex (buffer-name))))
          (push (list :name     (buffer-name)
                      :file     (buffer-file-name)
                      :size     (buffer-size)
                      :mode     major-mode
                      :modified (buffer-modified-p))
                results))))
    (setq results (nreverse results))
    (if (and limit (> (length results) limit))
        (seq-take results limit)
      results)))

;;;; --- discoverability ----------------------------------------------------

(defun anvil-emacs-helpers-list ()
  "Return a list of all anvil-emacs* user-facing function names."
  (let (results)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (let ((n (symbol-name sym)))
                    (and (string-prefix-p "anvil-emacs" n)
                         (not (string-match-p "--" n)))))
         (push sym results))))
    (sort results (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(provide 'anvil-emacs)
;;; anvil-emacs.el ends here
