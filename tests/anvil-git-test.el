;;; anvil-git-test.el --- Tests for anvil-git -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercises the new repo-root / head-sha / branch / diff / worktree
;; helpers added on top of the pre-existing anvil-git.el module.
;; Each test builds a disposable git repo via `call-process' so no
;; network or pre-existing checkout is required.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-git)
(require 'anvil-test-fixtures)

;;;; --- fixtures -----------------------------------------------------------

(defun anvil-git-test--run (dir &rest args)
  "Run git ARGS in DIR, signaling on non-zero exit.
Kept as a thin wrapper for tests that mutate an existing repo
(checkout, commit) where the shared fixtures library has no
equivalent helper."
  (let ((rc (apply #'call-process "git" nil nil nil
                   "-C" dir args)))
    (unless (eql rc 0)
      (error "anvil-git-test: git %s in %s -> exit %s"
             (mapconcat #'identity args " ") dir rc))))

(defmacro anvil-git-test--with-repo (sym &rest body)
  "Bind SYM to a fresh repo path (via shared fixture), run BODY,
then delete the repo."
  (declare (indent 1))
  `(let ((,sym (anvil-test-fixtures-make-repo)))
     (unwind-protect (progn ,@body)
       (anvil-test-fixtures-destroy-repo ,sym))))

;;;; --- repo-root ----------------------------------------------------------

(ert-deftest anvil-git-test-repo-root-finds-toplevel ()
  (anvil-git-test--with-repo repo
    (should (equal (file-name-as-directory
                    (file-truename repo))
                   (file-name-as-directory
                    (file-truename (anvil-git-repo-root repo)))))
    ;; nested directory
    (let ((sub (expand-file-name "sub" repo)))
      (make-directory sub)
      (should (equal (file-name-as-directory (file-truename repo))
                     (file-name-as-directory
                      (file-truename (anvil-git-repo-root sub))))))))

(ert-deftest anvil-git-test-repo-root-returns-nil-outside-repo ()
  (let ((tmp (make-temp-file "anvil-git-nope-" t)))
    (unwind-protect
        (should-not (anvil-git-repo-root tmp))
      (delete-directory tmp t))))

(ert-deftest anvil-git-test-repo-root-errors-on-file-path ()
  (anvil-git-test--with-repo repo
    (let ((file (expand-file-name "tracked.txt" repo)))
      (with-temp-file file
        (insert "hello\n"))
      (should-error (anvil-git-repo-root file)
                    :type 'user-error))))

;;;; --- head-sha / branch --------------------------------------------------

(ert-deftest anvil-git-test-head-sha-returns-40-hex ()
  (anvil-git-test--with-repo repo
    (let ((full (anvil-git-head-sha repo)))
      (should (stringp full))
      (should (= 40 (length full)))
      (should (string-match-p "\\`[0-9a-f]\\{40\\}\\'" full)))))

(ert-deftest anvil-git-test-head-sha-short-is-shorter ()
  (anvil-git-test--with-repo repo
    (let ((full  (anvil-git-head-sha repo))
          (short (anvil-git-head-sha repo t)))
      (should (stringp short))
      (should (< (length short) (length full)))
      (should (string-prefix-p short full)))))

(ert-deftest anvil-git-test-branch-current ()
  (anvil-git-test--with-repo repo
    (should (equal "main" (anvil-git-branch-current repo)))
    ;; detach HEAD
    (anvil-git-test--run repo "checkout" "-q" "--detach")
    (should-not (anvil-git-branch-current repo))))

;;;; --- diff-names ---------------------------------------------------------

(ert-deftest anvil-git-test-diff-names-unstaged-vs-head ()
  (anvil-git-test--with-repo repo
    (write-region "changed\n" nil (expand-file-name "README" repo))
    (let ((names (anvil-git-diff-names repo)))
      (should (member "README" names)))))

(ert-deftest anvil-git-test-diff-names-empty-on-clean-tree ()
  (anvil-git-test--with-repo repo
    (should-not (anvil-git-diff-names repo))))

;;;; --- worktree list / add / remove --------------------------------------

(ert-deftest anvil-git-test-worktree-list-has-primary ()
  (anvil-git-test--with-repo repo
    (let* ((entries (anvil-git-worktree-list repo))
           (first   (car entries)))
      (should (listp first))
      (should (plist-get first :path))
      (should (plist-get first :head)))))

(ert-deftest anvil-git-test-worktree-add-and-remove ()
  (anvil-git-test--with-repo repo
    (let* ((target (expand-file-name "wt-target"
                                     (make-temp-file "anvil-git-wt-" t)))
           (returned (anvil-git-worktree-add target repo)))
      (unwind-protect
          (progn
            (should (equal target returned))
            (should (file-directory-p target))
            ;; It should now show in worktree-list.
            (let ((paths (mapcar (lambda (p) (plist-get p :path))
                                 (anvil-git-worktree-list repo))))
              (should (cl-some (lambda (p)
                                 (equal (file-truename p)
                                        (file-truename target)))
                               paths)))
            ;; Remove cleanly.
            (should (anvil-git-worktree-remove target repo))
            (should-not (file-directory-p target)))
        (ignore-errors (delete-directory target t))
        (ignore-errors (delete-directory (file-name-directory target) t))))))

(ert-deftest anvil-git-test-worktree-add-with-new-branch ()
  (anvil-git-test--with-repo repo
    (let ((target (expand-file-name "wt-branch"
                                    (make-temp-file "anvil-git-wt-" t))))
      (unwind-protect
          (progn
            (anvil-git-worktree-add target repo
                                    :branch "feat/x"
                                    :ref    "HEAD")
            (should (file-directory-p target))
            (should (equal "feat/x" (anvil-git-branch-current target))))
        (ignore-errors (anvil-git-worktree-remove target repo :force t))
        (ignore-errors (delete-directory target t))
        (ignore-errors (delete-directory (file-name-directory target) t))))))

(ert-deftest anvil-git-test-worktree-add-errors-when-not-in-repo ()
  (let ((tmp (make-temp-file "anvil-git-nope-" t)))
    (unwind-protect
        (should-error
         (anvil-git-worktree-add (expand-file-name "wt" tmp) tmp))
      (delete-directory tmp t))))

;;;; --- enable / disable round-trip ---------------------------------------

(ert-deftest anvil-git-test-enable-registers-tools ()
  "`anvil-git-enable' makes tools visible; `anvil-git-disable' removes them."
  (ignore-errors (anvil-git-disable))
  (anvil-git-enable)
  (unwind-protect
      (let ((ids (anvil-test-fixtures-registered-tool-ids
                  anvil-git--server-id)))
        (should (member "git-repo-root"     ids))
        (should (member "git-head-sha"      ids))
        (should (member "git-worktree-list" ids)))
    (anvil-git-disable))
  (should-not (member "git-repo-root"
                      (anvil-test-fixtures-registered-tool-ids
                       anvil-git--server-id))))

(provide 'anvil-git-test)
;;; anvil-git-test.el ends here
