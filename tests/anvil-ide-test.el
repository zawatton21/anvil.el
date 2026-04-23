;;; anvil-ide-test.el --- Regression tests for anvil-ide -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused regression tests for xref scoping.  The xref tools should keep
;; results inside the project associated with the context file instead of
;; mixing in hits from installed packages or other unrelated directories.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'xref)
(require 'anvil-ide)

(defun anvil-ide-test--write-file (path content)
  "Write CONTENT to PATH, creating parent directories as needed."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert content)))

(defun anvil-ide-test--make-xref (file line summary)
  "Return an xref item for FILE at LINE with SUMMARY."
  (xref-make summary
             (xref-make-file-location file line 1)))

(defun anvil-ide-test--with-temp-xref-layout (fn)
  "Create a temporary project/external layout and call FN with its files."
  (let* ((dir (make-temp-file "anvil-ide-test-" t))
         (project-root (expand-file-name "project" dir))
         (target-file (expand-file-name "src/main.el" project-root))
         (project-hit (expand-file-name "lib/project-hit.el" project-root))
         (external-hit (expand-file-name "elpa/pkg/external-hit.el" dir)))
    (unwind-protect
        (progn
          (anvil-ide-test--write-file
           target-file
           ";;; main.el\n\n(defun sample ()\n  nil)\n")
          (anvil-ide-test--write-file
           project-hit
           ";;; project-hit.el\n\n(sample)\n")
          (anvil-ide-test--write-file
           external-hit
           ";;; external-hit.el\n\n(sample)\n")
          (funcall fn project-root target-file project-hit external-hit))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest anvil-ide-test-xref-find-references-filters-to-project-root ()
  "xref references should exclude matches outside the current project."
  (anvil-ide-test--with-temp-xref-layout
   (lambda (project-root target-file project-hit external-hit)
     (cl-letf (((symbol-function 'xref-find-backend)
                (lambda () 'fake-backend))
               ((symbol-function 'xref-backend-references)
                (lambda (_backend _identifier)
                  (list
                   (anvil-ide-test--make-xref project-hit 3 "project reference")
                   (anvil-ide-test--make-xref external-hit 3 "external reference"))))
               ((symbol-function 'anvil-ide--project-for-buffer)
                (lambda (_buffer) :fake-project))
               ((symbol-function 'project-root)
                (lambda (_project) project-root)))
       (let ((result (anvil-ide--xref-find-references "sample" target-file)))
         (should (string-match-p
                  (regexp-quote project-hit)
                  result))
         (should (string-match-p "project reference" result))
         (should-not (string-match-p
                      (regexp-quote external-hit)
                      result))
         (should-not (string-match-p "external reference" result)))))))

(ert-deftest anvil-ide-test-xref-find-apropos-filters-to-project-root ()
  "xref apropos should exclude matches outside the current project."
  (anvil-ide-test--with-temp-xref-layout
   (lambda (project-root target-file project-hit external-hit)
     (cl-letf (((symbol-function 'xref-find-backend)
                (lambda () 'fake-backend))
               ((symbol-function 'xref-backend-apropos)
                (lambda (_backend _pattern)
                  (list
                   (anvil-ide-test--make-xref project-hit 3 "project apropos")
                   (anvil-ide-test--make-xref external-hit 3 "external apropos"))))
               ((symbol-function 'anvil-ide--project-for-buffer)
                (lambda (_buffer) :fake-project))
               ((symbol-function 'project-root)
                (lambda (_project) project-root)))
       (let ((result (anvil-ide--xref-find-apropos "sample" target-file)))
         (should (string-match-p
                  (regexp-quote project-hit)
                  result))
         (should (string-match-p "project apropos" result))
         (should-not (string-match-p
                      (regexp-quote external-hit)
                      result))
         (should-not (string-match-p "external apropos" result)))))))

(ert-deftest anvil-ide-test-xref-find-references-elisp-no-project-falls-back-to-directory-search ()
  "Non-project Elisp references should avoid the interactive project prompt."
  (anvil-ide-test--with-temp-xref-layout
   (lambda (_project-root target-file project-hit external-hit)
     (let ((expected-root
            (file-name-as-directory
             (file-name-directory (expand-file-name target-file)))))
       (cl-letf (((symbol-function 'xref-find-backend)
                  (lambda () 'elisp))
                 ((symbol-function 'anvil-ide--project-for-buffer)
                  (lambda (_buffer) nil))
                 ((symbol-function 'xref-backend-references)
                  (lambda (&rest _)
                    (signal 'quit '(minibuffer-quit))))
                 ((symbol-function 'xref-references-in-directory)
                  (lambda (_identifier dir)
                    (should (equal expected-root
                                   (file-name-as-directory dir)))
                    (list
                     (anvil-ide-test--make-xref target-file 3 "local reference")
                     (anvil-ide-test--make-xref external-hit 3 "external reference")))))
         (let ((result (anvil-ide--xref-find-references "sample" target-file)))
           (should (string-match-p
                    (regexp-quote target-file)
                    result))
           (should (string-match-p "local reference" result))
           (should-not (string-match-p
                        (regexp-quote external-hit)
                        result))
           (should-not (string-match-p "external reference" result))))))))

(provide 'anvil-ide-test)
;;; anvil-ide-test.el ends here
