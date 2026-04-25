;;; anvil-disk-test.el --- Tests for anvil-disk divergence helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Covers the five divergence states classified by
;; `anvil-disk-buffer-divergence' plus the refusal and :force
;; semantics of `anvil-file-safe-write'.  Each test seeds a real
;; file, optionally visits it with `find-file-noselect', and
;; tears everything down in `unwind-protect'.

;;; Code:

(require 'ert)
(require 'anvil-disk)

;;;; --- fixtures -----------------------------------------------------------

(defmacro anvil-disk-test--with-file (vars content &rest body)
  "Bind (FILE) and (FILE BUF) in VARS to a seeded temp file and run BODY.
VARS is (FILE) for no-buffer tests or (FILE BUF) when a visiting
buffer is required.  CONTENT is the initial disk text.  The buffer
is killed and the file deleted on exit."
  (declare (indent 2))
  (let ((file-var (car vars))
        (buf-var  (cadr vars)))
    `(let* ((,file-var (make-temp-file "anvil-disk-" nil ".txt"))
            ,@(when buf-var `((,buf-var nil))))
       (unwind-protect
           (progn
             (let ((coding-system-for-write 'utf-8-unix))
               (write-region ,content nil ,file-var nil 'silent))
             ,@(when buf-var
                 `((setq ,buf-var (find-file-noselect ,file-var))))
             ,@body)
         ,@(when buf-var
             `((when (buffer-live-p ,buf-var)
                 (with-current-buffer ,buf-var
                   (set-buffer-modified-p nil))
                 (kill-buffer ,buf-var))))
         (ignore-errors (delete-file ,file-var))))))

(defun anvil-disk-test--bump-disk-mtime (file)
  "Advance FILE's mtime by 10 seconds so it outruns any visited modtime.
Avoids flaky 1-second NTFS / 2-second FAT resolution in tests."
  (let ((future (time-add (current-time) 10)))
    (set-file-times file future)))

;;;; --- classification -----------------------------------------------------

(ert-deftest anvil-disk-test-no-visited-buffer-returns-nil ()
  "A file nobody visits produces nil, not a spurious in-sync plist."
  (anvil-disk-test--with-file (f) "hello\n"
    (should (null (anvil-disk-buffer-divergence f)))))

(ert-deftest anvil-disk-test-in-sync ()
  "Freshly visited file with no edits reports :status in-sync."
  (anvil-disk-test--with-file (f buf) "hello\n"
    (let ((div (anvil-disk-buffer-divergence f)))
      (should div)
      (should (eq buf (plist-get div :buffer)))
      (should (eq 'in-sync (plist-get div :status))))))

(ert-deftest anvil-disk-test-buffer-newer ()
  "Modifying the buffer without saving reports :status buffer-newer."
  (anvil-disk-test--with-file (f buf) "hello\n"
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "world\n"))
    (should (eq 'buffer-newer
                (plist-get (anvil-disk-buffer-divergence f) :status)))))

(ert-deftest anvil-disk-test-disk-newer ()
  "An external disk bump with unmodified buffer reports disk-newer."
  (anvil-disk-test--with-file (f _buf) "hello\n"
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region "rewritten\n" nil f nil 'silent))
    (anvil-disk-test--bump-disk-mtime f)
    (should (eq 'disk-newer
                (plist-get (anvil-disk-buffer-divergence f) :status)))))

(ert-deftest anvil-disk-test-both-modified ()
  "Modified buffer plus external disk bump reports both-modified."
  (anvil-disk-test--with-file (f buf) "hello\n"
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "edit\n"))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region "rewritten\n" nil f nil 'silent))
    (anvil-disk-test--bump-disk-mtime f)
    (should (eq 'both-modified
                (plist-get (anvil-disk-buffer-divergence f) :status)))))

(ert-deftest anvil-disk-test-unknown-modtime ()
  "A buffer whose visited-file-modtime is zeroed reports unknown."
  (anvil-disk-test--with-file (f buf) "hello\n"
    (with-current-buffer buf
      (set-visited-file-modtime 0))
    (should (eq 'unknown
                (plist-get (anvil-disk-buffer-divergence f) :status)))))

;;;; --- warn / require -----------------------------------------------------

(ert-deftest anvil-disk-test-warn-in-sync-empty ()
  "warn-if-diverged is empty for in-sync and no-buffer cases."
  (anvil-disk-test--with-file (f) "hello\n"
    (should (null (anvil-file-warn-if-diverged f))))
  (anvil-disk-test--with-file (f _buf) "hello\n"
    (should (null (anvil-file-warn-if-diverged f)))))

(ert-deftest anvil-disk-test-warn-buffer-newer-nonempty ()
  "warn-if-diverged yields one message for a modified buffer."
  (anvil-disk-test--with-file (f buf) "hello\n"
    (with-current-buffer buf (insert "X"))
    (let ((ws (anvil-file-warn-if-diverged f)))
      (should (= 1 (length ws)))
      (should (string-match-p "buffer-newer" (car ws))))))

(ert-deftest anvil-disk-test-require-in-sync-signals ()
  "require-in-sync signals when the buffer is modified."
  (anvil-disk-test--with-file (f buf) "hello\n"
    (with-current-buffer buf (insert "X"))
    (should-error (anvil-file-require-in-sync f) :type 'user-error))
  (anvil-disk-test--with-file (f _buf) "hello\n"
    ;; In-sync — no error.
    (should (null (anvil-file-require-in-sync f)))))

;;;; --- safe-write ---------------------------------------------------------

(ert-deftest anvil-disk-test-safe-write-refuses-on-buffer-newer ()
  "Safe-write refuses when the visited buffer has unsaved edits."
  (anvil-disk-test--with-file (f buf) "hello\n"
    (with-current-buffer buf (insert "X"))
    (should-error (anvil-file-safe-write f "override\n") :type 'user-error)
    ;; Disk content untouched.
    (should (equal "hello\n"
                   (with-temp-buffer
                     (let ((coding-system-for-read 'utf-8))
                       (insert-file-contents f))
                     (buffer-string))))))

(ert-deftest anvil-disk-test-safe-write-force-overrides ()
  "With :force t the write proceeds even when buffer-newer."
  (anvil-disk-test--with-file (f buf) "hello\n"
    (with-current-buffer buf (insert "X"))
    (let ((res (anvil-file-safe-write f "forced\n" :force t)))
      (should (eq t (plist-get res :forced)))
      (should (eq 'buffer-newer (plist-get res :status))))
    (should (equal "forced\n"
                   (with-temp-buffer
                     (let ((coding-system-for-read 'utf-8))
                       (insert-file-contents f))
                     (buffer-string))))))

(ert-deftest anvil-disk-test-safe-write-allows-disk-newer ()
  "disk-newer is not a conflict: safe-write proceeds and reports status."
  (anvil-disk-test--with-file (f _buf) "hello\n"
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region "externally rewritten\n" nil f nil 'silent))
    (anvil-disk-test--bump-disk-mtime f)
    (let ((res (anvil-file-safe-write f "new content\n")))
      (should (eq 'disk-newer (plist-get res :status)))
      (should (null (plist-get res :forced))))))

(ert-deftest anvil-disk-test-safe-write-no-buffer ()
  "Without a visited buffer, safe-write reports status no-buffer."
  (anvil-disk-test--with-file (f) "hello\n"
    (let ((res (anvil-file-safe-write f "whatever\n")))
      (should (eq 'no-buffer (plist-get res :status))))))

;;; anvil-disk-test.el ends here
