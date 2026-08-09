;;; anvil-fusion-exec-test.el --- ERT for execution verification -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for Doc 61 Phase 6c execution-based verification.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'anvil-fusion)
(require 'anvil-fusion-exec)

(defconst anvil-fusion-exec-test--patch-fenced
  "before\n```diff\n--- a/base.txt\n+++ b/base.txt\n@@ -1 +1 @@\n-old\n+new\n```\nafter\n")

(defconst anvil-fusion-exec-test--patch-raw
  "intro\n--- a/base.txt\n+++ b/base.txt\n@@ -1 +1 @@\n-old\n+new\ntext\n")

(defmacro anvil-fusion-exec-test--with-temp-repo (&rest body)
  "Create a committed temp git repo and run BODY there."
  (declare (indent 0) (debug t))
  `(let* ((repo (make-temp-file "anvil-fusion-exec-repo-" t))
          (default-directory repo))
     (unwind-protect
         (progn
           (should (eq 0 (call-process "git" nil nil nil "init" "-q" repo)))
           (should (eq 0 (call-process "git" nil nil nil "-C" repo "config" "user.name" "Test User")))
           (should (eq 0 (call-process "git" nil nil nil "-C" repo "config" "user.email" "test@example.com")))
           (should (eq 0 (call-process "git" nil nil nil "-C" repo "config" "commit.gpgsign" "false")))
           (with-temp-file (expand-file-name "base.txt" repo)
             (insert "old\n"))
           (should (eq 0 (call-process "git" nil nil nil "-C" repo "add" "base.txt")))
           (should (eq 0 (call-process "git" nil nil nil "-C" repo "commit" "-q" "-m" "init")))
           ,@body)
       (ignore-errors (delete-directory repo t)))))

(defun anvil-fusion-exec-test--worktree-lines (repo)
  "Return `git worktree list --porcelain' lines for REPO."
  (with-temp-buffer
    (should (eq 0 (call-process "git" nil t nil "-C" repo "worktree" "list" "--porcelain")))
    (split-string (buffer-string) "\n" t)))

(defun anvil-fusion-exec-test--base-file-patch (repo new-body)
  "Return a fenced diff patch changing REPO's base.txt to NEW-BODY."
  (let ((file (expand-file-name "base.txt" repo)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert new-body))
          (with-temp-buffer
            (should (eq 0 (call-process "git" nil t nil "-C" repo "diff" "--" "base.txt")))
            (concat "```diff\n" (buffer-string) "```\n")))
      (should (eq 0 (call-process "git" nil nil nil "-C" repo "checkout" "--" "base.txt"))))))

(ert-deftest anvil-fusion-exec-test-extract-patch-fenced ()
  (should (equal "--- a/base.txt\n+++ b/base.txt\n@@ -1 +1 @@\n-old\n+new"
                 (anvil-fusion-exec--extract-patch
                  anvil-fusion-exec-test--patch-fenced))))

(ert-deftest anvil-fusion-exec-test-extract-patch-raw ()
  (should (equal "--- a/base.txt\n+++ b/base.txt\n@@ -1 +1 @@\n-old\n+new"
                 (anvil-fusion-exec--extract-patch
                  anvil-fusion-exec-test--patch-raw))))

(ert-deftest anvil-fusion-exec-test-extract-patch-mixed ()
  (let ((text (concat
               "```patch\n--- a/base.txt\n+++ b/base.txt\n@@ -1 +1 @@\n-old\n+new\n```\n"
               "middle\n"
               "--- a/other.txt\n+++ b/other.txt\n@@ -1 +1 @@\n-a\n+b\nend\n")))
    (should
     (equal
      (concat
       "--- a/base.txt\n+++ b/base.txt\n@@ -1 +1 @@\n-old\n+new\n\n"
       "--- a/other.txt\n+++ b/other.txt\n@@ -1 +1 @@\n-a\n+b")
      (anvil-fusion-exec--extract-patch text)))))

(ert-deftest anvil-fusion-exec-test-extract-patch-none ()
  (should-not (anvil-fusion-exec--extract-patch "no diff here")))

(ert-deftest anvil-fusion-exec-test-run-candidate-pass-and-cleanup ()
  (anvil-fusion-exec-test--with-temp-repo
    (let ((temp-root (make-temp-file "anvil-fusion-exec-tmp-" t)))
      (unwind-protect
          (let ((temporary-file-directory (file-name-as-directory temp-root)))
            (let* ((candidate `(:name "passer" :summary
                                      ,(anvil-fusion-exec-test--base-file-patch repo "new\n")))
                   (result (anvil-fusion-exec-run-candidate candidate repo "grep -qx 'new' base.txt"))
                   (verified (anvil-fusion-exec-verify-candidates
                              (list candidate)
                              :repo-root repo
                              :check-cmd "grep -qx 'new' base.txt")))
              (should (eq 'pass (plist-get result :status)))
              (should (equal "exit 0" (plist-get result :detail)))
              (should (equal '((:name "passer" :status pass :detail "exit 0"))
                             (plist-get verified :results)))
              (should (equal
                       '((:claim "候補 passer のパッチは検証コマンドに合格した"
                          :kind code
                          :candidates ("passer")
                          :verdict confirmed
                          :evidence "exit 0"))
                       (plist-get verified :claims)))
              (should (equal nil (directory-files temp-root nil directory-files-no-dot-files-regexp)))
              (should (= 1 (cl-count-if (lambda (line) (string-prefix-p "worktree " line))
                                        (anvil-fusion-exec-test--worktree-lines repo))))))
        (ignore-errors (delete-directory temp-root t))))))

(ert-deftest anvil-fusion-exec-test-run-candidate-check-fail ()
  (anvil-fusion-exec-test--with-temp-repo
    (let* ((patch (anvil-fusion-exec-test--base-file-patch repo "new\n"))
           (result (anvil-fusion-exec-run-candidate
                    `(:name "f" :summary ,patch)
                    repo
                    "false")))
      (should (eq 'fail (plist-get result :status)))
      (should (string-match-p "check failed with exit 1" (plist-get result :detail))))))

(ert-deftest anvil-fusion-exec-test-run-candidate-apply-fail ()
  (anvil-fusion-exec-test--with-temp-repo
    (let* ((patch (anvil-fusion-exec-test--base-file-patch repo "changed\n"))
           (bad-patch (replace-regexp-in-string "^-old$" "-missing" patch))
           (result (anvil-fusion-exec-run-candidate
                    `(:name "bad" :summary ,bad-patch)
                    repo
                    "true")))
      (should (eq 'fail (plist-get result :status)))
      (should (string-match-p "patch does not apply:" (plist-get result :detail))))))

(ert-deftest anvil-fusion-exec-test-run-candidate-timeout ()
  (anvil-fusion-exec-test--with-temp-repo
    (let* ((patch (anvil-fusion-exec-test--base-file-patch repo "new\n"))
           (result (anvil-fusion-exec-run-candidate
                    `(:name "slow" :summary ,patch)
                    repo
                    "sleep 5"
                    :timeout-sec 1)))
      (should (eq 'fail (plist-get result :status)))
      (should (equal "check timed out after 1 s" (plist-get result :detail))))))

(ert-deftest anvil-fusion-exec-test-run-candidate-no-patch ()
  (anvil-fusion-exec-test--with-temp-repo
    (let* ((candidate '(:name "plain" :summary "just prose"))
           (result (anvil-fusion-exec-run-candidate candidate repo "true"))
           (verified (anvil-fusion-exec-verify-candidates
                      (list candidate) :repo-root repo :check-cmd "true")))
      (should (eq 'no-patch (plist-get result :status)))
      (should (equal "no patch block" (plist-get result :detail)))
      (should (equal
               '((:claim "候補 plain の回答には検証可能なパッチが見つからなかった"
                  :kind code
                  :candidates ("plain")
                  :verdict unverified
                  :evidence "no patch block"))
               (plist-get verified :claims))))))

(provide 'anvil-fusion-exec-test)
;;; anvil-fusion-exec-test.el ends here
