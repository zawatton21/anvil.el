;;; anvil-fs.el --- Filesystem tools for anvil -*- lexical-binding: t; -*-

;; Author: zawatton21
;; Keywords: tools, mcp, claude, files

;;; Commentary:

;; anvil-helpers の姉妹ファイル。Claude Code が emacs-eval 経由で
;; ファイルシステムの一覧/差分検出を OS 非依存に行う helper 群。
;;
;; 主要動機:
;;   photo-manager / scansnap-pipeline / receipt-ocr の 3 skill が
;;   「カメラロール / Inbox に新着あるか」を毎回 ad-hoc に書いていた
;;   差分検出を 1 call で完結させる。
;;
;; 戻り値: file plist
;;   (:path STR-abs :mtime TIME :size N)
;; mtime は Emacs time value (`time-less-p' でそのまま比較可)。

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)

;;;; --- internal -----------------------------------------------------------

(defun anvil-fs--parse-since (since)
  "Convert SINCE to an Emacs time value, or nil.
Accepts: nil, number (seconds-since-epoch), string parseable by
`date-to-time' (e.g. \"2026-04-12\", \"2026-04-12T09:00:00\"), or
a time value passed through unchanged."
  (cond
   ((null since) nil)
   ((numberp since) (seconds-to-time since))
   ((stringp since)
    (condition-case nil
        (date-to-time since)
      (error
       ;; date-to-time refuses bare YYYY-MM-DD on some Emacsen
       (condition-case nil
           (date-to-time (concat since " 00:00:00"))
         (error nil)))))
   ((consp since) since)
   (t nil)))

(defun anvil-fs--collect (dir regex recursive)
  "Return a list of absolute file paths matching REGEX under DIR.
RECURSIVE non-nil walks subdirectories. REGEX matches basenames."
  (let ((expanded (expand-file-name dir)))
    (if recursive
        (directory-files-recursively expanded (or regex "") nil)
      (let ((files (directory-files expanded t (or regex nil) nil)))
        (cl-remove-if (lambda (f)
                        (or (string-suffix-p "/." f)
                            (string-suffix-p "/.." f)))
                      files)))))

;;;; --- public API ---------------------------------------------------------

(defun anvil-fs-find-recent (dir &optional opts)
  "Return regular files under DIR sorted by mtime descending.

OPTS plist:
  :since      time / number / \"YYYY-MM-DD\" — only files newer than this
  :limit N    cap result count (default 100, nil or 0 = no cap)
  :glob STR   shell wildcard on basename, e.g. \"*.org\"
  :recursive  walk subdirectories
  :regexp R   raw regex on basename (overrides :glob)

Each element:
  (:path STR-abs :mtime TIME :size N)

mtime is an Emacs time value comparable with `time-less-p'."
  (let* ((since-time (anvil-fs--parse-since (plist-get opts :since)))
         (limit (if (plist-member opts :limit)
                    (plist-get opts :limit)
                  100))
         (glob (plist-get opts :glob))
         (raw-regex (plist-get opts :regexp))
         (regex (or raw-regex
                    (and glob (wildcard-to-regexp glob))))
         (recursive (plist-get opts :recursive))
         (paths (anvil-fs--collect dir regex recursive))
         results)
    (dolist (f paths)
      (when (file-regular-p f)
        (let* ((attrs (file-attributes f))
               (mtime (file-attribute-modification-time attrs))
               (size  (file-attribute-size attrs)))
          (when (or (null since-time)
                    (time-less-p since-time mtime))
            (push (list :path f :mtime mtime :size size) results)))))
    (setq results
          (sort results
                (lambda (a b)
                  (time-less-p (plist-get b :mtime)
                               (plist-get a :mtime)))))
    (if (and limit (> limit 0) (> (length results) limit))
        (seq-take results limit)
      results)))

;;;; --- anvil-fs-stat ------------------------------------------------------

(defun anvil-fs-stat (path)
  "Return file/directory metadata for PATH as a plist, or nil if missing.

Returned plist:
  (:path STR-abs :exists BOOL :type SYM :size N
   :mtime TIME :atime TIME :ctime TIME
   :mode-string STR :mode-octal STR :uid N :gid N)

:type is one of 'file 'dir 'symlink 'other.

Designed to satisfy CLAUDE.md's `wc -c でサイズ確認' rule from
inside Emacs without spawning a shell. Symlinks are NOT followed."
  (let ((expanded (expand-file-name path)))
    (if (not (file-exists-p expanded))
        (list :path expanded :exists nil)
      (let* ((attrs (file-attributes expanded 'integer))
             (type-raw (file-attribute-type attrs))
             (type (cond ((eq type-raw t) 'dir)
                         ((stringp type-raw) 'symlink)
                         ((null type-raw) 'file)
                         (t 'other)))
             (mode-string (file-attribute-modes attrs))
             ;; Convert "drwxr-xr-x" → "755" / "100644" via file-modes
             (mode-int (file-modes expanded))
             (mode-octal (and mode-int (format "%o" mode-int))))
        (list :path        expanded
              :exists      t
              :type        type
              :size        (file-attribute-size attrs)
              :mtime       (file-attribute-modification-time attrs)
              :atime       (file-attribute-access-time attrs)
              :ctime       (file-attribute-status-change-time attrs)
              :mode-string mode-string
              :mode-octal  mode-octal
              :uid         (file-attribute-user-id attrs)
              :gid         (file-attribute-group-id attrs))))))

;;;; --- anvil-fs-tree ------------------------------------------------------

(defun anvil-fs--tree-walk (dir depth max-depth glob-regex show-files)
  "Internal recursive walker for `anvil-fs-tree'."
  (let* ((expanded (expand-file-name dir))
         (entries (and (file-directory-p expanded)
                       (cl-remove-if (lambda (n) (or (string= n ".") (string= n "..")))
                                     (directory-files expanded nil nil t))))
         dirs files)
    (dolist (name entries)
      (let ((full (expand-file-name name expanded)))
        (cond
         ((file-directory-p full)
          (push name dirs))
         ((file-regular-p full)
          (when (and show-files
                     (or (null glob-regex)
                         (string-match-p glob-regex name)))
            (push name files))))))
    (let ((children
           (append
            (mapcar (lambda (d)
                      (if (and max-depth (>= depth max-depth))
                          (list :name d :type 'dir :children nil :truncated t)
                        (anvil-fs--tree-walk
                         (expand-file-name d expanded)
                         (1+ depth) max-depth glob-regex show-files)))
                    (sort dirs #'string<))
            (mapcar (lambda (f)
                      (let* ((full (expand-file-name f expanded))
                             (size (file-attribute-size (file-attributes full))))
                        (list :name f :type 'file :size size)))
                    (sort files #'string<)))))
      (list :name (file-name-nondirectory (directory-file-name expanded))
            :path expanded
            :type 'dir
            :children children))))

(defun anvil-fs-tree (dir &optional opts)
  "Return a nested tree of DIR as a plist.

OPTS plist:
  :max-depth N      walk only N levels (nil = unlimited)
  :glob STR         shell wildcard on file basename (\"*.org\")
  :show-files BOOL  include regular files (default t)

Returned plist (recursive):
  (:name STR :path STR-abs :type 'dir
   :children ((:name :type 'dir :children ..)
              (:name :type 'file :size N) ..))

Subdirectories at the depth limit get :truncated t and :children nil.
Symlinks are NOT followed (file-attributes default)."
  (let* ((max-depth (plist-get opts :max-depth))
         (glob (plist-get opts :glob))
         (show-files (if (plist-member opts :show-files)
                         (plist-get opts :show-files)
                       t))
         (regex (and glob (wildcard-to-regexp glob))))
    (anvil-fs--tree-walk dir 0 max-depth regex show-files)))

;;;; --- anvil-fs-disk-usage -----------------------------------------------

(defun anvil-fs-disk-usage (dir &optional opts)
  "Return disk usage statistics for DIR as a plist.

OPTS plist:
  :top-n N      include the N largest files (default 10)
  :glob STR     restrict to file basename matching this wildcard

Returned plist:
  (:dir STR-abs :total-bytes N :total-mb N :file-count N :dir-count N
   :largest ((:path :size) ..))

Walks recursively. Symlinks are NOT followed."
  (let* ((expanded (expand-file-name dir))
         (top-n (or (plist-get opts :top-n) 10))
         (glob  (plist-get opts :glob))
         (regex (and glob (wildcard-to-regexp glob)))
         (total 0)
         (file-count 0)
         (dir-count 0)
         largest)
    (unless (file-directory-p expanded)
      (error "anvil-fs: not a directory: %s" dir))
    (let ((files (directory-files-recursively expanded (or regex "") t)))
      (dolist (path files)
        (cond
         ((file-directory-p path)
          (cl-incf dir-count))
         ((file-regular-p path)
          (cl-incf file-count)
          (let ((size (file-attribute-size (file-attributes path))))
            (when size
              (cl-incf total size)
              (push (cons path size) largest)))))))
    (setq largest
          (let ((sorted (sort largest (lambda (a b) (> (cdr a) (cdr b))))))
            (mapcar (lambda (cell) (list :path (car cell) :size (cdr cell)))
                    (seq-take sorted top-n))))
    (list :dir         expanded
          :total-bytes total
          :total-mb    (round (/ total 1048576.0))
          :file-count  file-count
          :dir-count   dir-count
          :largest     largest)))

;;;; --- discoverability ----------------------------------------------------

(defun anvil-fs-helpers-list ()
  "Return a list of all anvil-fs* user-facing function names."
  (let (results)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (let ((n (symbol-name sym)))
                    (and (string-prefix-p "anvil-fs" n)
                         (not (string-match-p "--" n)))))
         (push sym results))))
    (sort results (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(provide 'anvil-fs)
;;; anvil-fs.el ends here
