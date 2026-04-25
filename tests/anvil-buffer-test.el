;;; anvil-buffer-test.el --- Tests for anvil-buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Covers the three buffer-first MCP helpers:
;; `anvil-buffer-read', `anvil-buffer-save', `anvil-buffer-list-modified'.
;; All tests work on temporary files + `find-file-noselect' visited
;; buffers; no MCP registration is exercised.

;;; Code:

(require 'ert)
(require 'anvil-buffer)

(defmacro anvil-buffer-test--with-file (vars content &rest body)
  "Bind (FILE BUF) to a seeded temp file and its visited buffer."
  (declare (indent 2))
  (let ((file-var (car vars))
        (buf-var  (cadr vars)))
    `(let* ((,file-var (make-temp-file "anvil-buffer-" nil ".txt"))
            (,buf-var nil))
       (unwind-protect
           (progn
             (let ((coding-system-for-write 'utf-8-unix))
               (write-region ,content nil ,file-var nil 'silent))
             (setq ,buf-var (find-file-noselect ,file-var))
             ,@body)
         (when (buffer-live-p ,buf-var)
           (with-current-buffer ,buf-var
             (set-buffer-modified-p nil))
           (kill-buffer ,buf-var))
         (ignore-errors (delete-file ,file-var))))))

(defun anvil-buffer-test--bump-disk-mtime (file)
  "Advance FILE's mtime by 10 seconds."
  (set-file-times file (time-add (current-time) 10)))

(defun anvil-buffer-test--read-disk (file)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (buffer-string)))

;;;; --- buffer-read --------------------------------------------------------

(ert-deftest anvil-buffer-test-read-clean ()
  "Reading a clean visited buffer returns its content + in-sync status."
  (anvil-buffer-test--with-file (f buf) "hello\nworld\n"
    (let ((res (anvil-buffer-read (buffer-name buf))))
      (should (equal "hello\nworld\n" (plist-get res :content)))
      (should (equal f (plist-get res :file)))
      (should (null (plist-get res :modified)))
      (should (eq 'in-sync (plist-get res :status)))
      (should (null (plist-get res :warnings))))))

(ert-deftest anvil-buffer-test-read-dirty ()
  "A modified buffer reports :modified t and emits a warning."
  (anvil-buffer-test--with-file (f buf) "hello\n"
    (with-current-buffer buf (insert "DIRTY"))
    (let ((res (anvil-buffer-read (buffer-name buf))))
      (should (eq t (plist-get res :modified)))
      (should (eq 'buffer-newer (plist-get res :status)))
      (should (= 1 (length (plist-get res :warnings)))))))

(ert-deftest anvil-buffer-test-read-missing-errors ()
  "Unknown buffer name signals user-error."
  (should-error (anvil-buffer-read "no-such-buffer-zzz") :type 'user-error))

;;;; --- buffer-save --------------------------------------------------------

(ert-deftest anvil-buffer-test-save-dirty-writes-disk ()
  "Saving a dirty buffer writes to disk and clears the modified flag."
  (anvil-buffer-test--with-file (f buf) "start\n"
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "appended\n"))
    (let ((res (anvil-buffer-save (buffer-name buf))))
      (should (eq t (plist-get res :saved)))
      (should (> (plist-get res :bytes-written) 0))
      (should (null (buffer-modified-p buf)))
      (should (equal "start\nappended\n"
                     (anvil-buffer-test--read-disk f))))))

(ert-deftest anvil-buffer-test-save-clean-is-noop ()
  "Saving an unmodified buffer is a noop and reports :saved nil."
  (anvil-buffer-test--with-file (f buf) "clean\n"
    (let ((res (anvil-buffer-save (buffer-name buf))))
      (should (null (plist-get res :saved)))
      (should (= 0 (plist-get res :bytes-written)))
      (should (equal "clean\n" (anvil-buffer-test--read-disk f))))))

(ert-deftest anvil-buffer-test-save-refuses-disk-newer ()
  "Saving a modified buffer onto a disk-newer file refuses without force."
  (anvil-buffer-test--with-file (f buf) "v1\n"
    (with-current-buffer buf (insert "local"))
    ;; External disk rewrite + mtime bump.
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region "v2-rewritten\n" nil f nil 'silent))
    (anvil-buffer-test--bump-disk-mtime f)
    (should-error (anvil-buffer-save (buffer-name buf)) :type 'user-error)
    ;; Disk still has v2-rewritten — buffer did NOT win.
    (should (equal "v2-rewritten\n" (anvil-buffer-test--read-disk f)))))

(ert-deftest anvil-buffer-test-save-force-overrides-disk-newer ()
  "force=t overwrites even when status is both-modified / disk-newer."
  (anvil-buffer-test--with-file (f buf) "v1\n"
    (with-current-buffer buf (insert "local"))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region "v2-rewritten\n" nil f nil 'silent))
    (anvil-buffer-test--bump-disk-mtime f)
    (let ((res (anvil-buffer-save (buffer-name buf) t)))
      (should (eq t (plist-get res :forced)))
      (should (eq t (plist-get res :saved))))
    ;; Disk now reflects the buffer content.
    (should (string-match-p "local" (anvil-buffer-test--read-disk f)))))

(ert-deftest anvil-buffer-test-save-requires-visited-file ()
  "A buffer without a visited file cannot be saved."
  (let ((buf (generate-new-buffer "*anvil-buffer-transient*")))
    (unwind-protect
        (should-error (anvil-buffer-save (buffer-name buf))
                      :type 'user-error)
      (kill-buffer buf))))

;;;; --- buffer-list-modified -----------------------------------------------

(ert-deftest anvil-buffer-test-list-modified-picks-up-dirty-buffer ()
  "A freshly dirtied buffer shows up in the modified list."
  (anvil-buffer-test--with-file (f buf) "hello\n"
    (with-current-buffer buf (insert "X"))
    (let* ((res (anvil-buffer-list-modified))
           (names (mapcar (lambda (b) (plist-get b :buffer))
                          (plist-get res :buffers))))
      (should (>= (plist-get res :count) 1))
      (should (member (buffer-name buf) names)))))

(ert-deftest anvil-buffer-test-list-modified-excludes-clean ()
  "A clean visited buffer is not listed."
  (anvil-buffer-test--with-file (f buf) "hello\n"
    (let* ((res   (anvil-buffer-list-modified))
           (names (mapcar (lambda (b) (plist-get b :buffer))
                          (plist-get res :buffers))))
      (should-not (member (buffer-name buf) names)))))

;;; anvil-buffer-test.el ends here
