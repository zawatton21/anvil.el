;;; anvil-elisp-test.el --- Tests for anvil-elisp -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused regression tests for Elisp definition lookup, including the
;; native-comp case where a file-backed Elisp function appears as `subr'
;; at runtime.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'anvil-elisp)

(defun anvil-elisp-test--parse-json (text)
  "Parse TEXT as a plist-shaped JSON object."
  (json-parse-string text
                     :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun anvil-elisp-test--require-native-compiler ()
  "Skip unless this runner can actually produce native-compiled functions."
  (unless (and (fboundp 'native-compile)
               (fboundp 'native-comp-function-p)
               (fboundp 'native-comp-available-p)
               (native-comp-available-p))
    (ert-skip "native compilation is unavailable on this runner")))

(defun anvil-elisp-test--native-compile-symbol-or-skip (sym)
  "Native-compile SYM or skip if the runner cannot do so reliably."
  (anvil-elisp-test--require-native-compiler)
  (condition-case err
      (let ((compiled (native-compile sym)))
        (unless (native-comp-function-p compiled)
          (ert-skip
           (format "native-compile did not return a native function for %s"
                   sym)))
        (fset sym compiled))
    (error
     (ert-skip
      (format "native compilation failed on this runner: %s"
              (error-message-string err))))))

(defun anvil-elisp-test--with-temp-source (fn)
  "Write a temporary Elisp fixture and call FN with its path."
  (let* ((dir (make-temp-file "anvil-elisp-test-" t))
         (file (expand-file-name "fixture.el" dir))
         (feature 'anvil-elisp-test-fixture)
         (symbols '(anvil-elisp-test-fixture-fn
                    anvil-elisp-test-fixture-alias)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; fixture.el --- temp -*- lexical-binding: t; -*-\n\n"
                    ";; Header for temp source fn\n"
                    "(defun anvil-elisp-test-fixture-fn (x)\n"
                    "  \"Return X plus one.\"\n"
                    "  (+ x 1))\n\n"
                    "(defalias 'anvil-elisp-test-fixture-alias\n"
                    "  #'anvil-elisp-test-fixture-fn\n"
                    "  \"Alias doc.\")\n\n"
                    "(provide 'anvil-elisp-test-fixture)\n"))
          (load-file file)
          (funcall fn file))
      (ignore-errors
        (when (featurep feature)
          (unload-feature feature t)))
      (dolist (sym symbols)
        (ignore-errors (fmakunbound sym)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(ert-deftest anvil-elisp-test-get-function-definition-c-primitive ()
  "True C primitives still report the C-function shape."
  (let ((res (anvil-elisp-test--parse-json
              (anvil-elisp--get-function-definition "car"))))
    (should (plist-get res :is-c-function))
    (should (equal "car" (plist-get res :function-name)))))

(ert-deftest anvil-elisp-test-dispatch-prefers-source-file-over-subr ()
  "A file-backed `subr' must go through the source-file path."
  (cl-letf (((symbol-function 'find-lisp-object-file-name)
             (lambda (_sym _type) "/tmp/fake-source.el"))
            ((symbol-function 'anvil-elisp--get-function-definition-from-file)
             (lambda (function sym func-file is-alias aliased-to)
               (list :from-file function sym func-file is-alias aliased-to)))
            ((symbol-function 'anvil-elisp--get-function-definition-c-function)
             (lambda (&rest _args)
               :c-function)))
    (should
     (equal
      '(:from-file "car" car "/tmp/fake-source.el" nil nil)
      (anvil-elisp--get-function-definition-dispatch
       "car" 'car (list (symbol-function 'car) nil nil))))))

(ert-deftest anvil-elisp-test-native-compiled-file-backed-function-uses-source ()
  "Native-compiled Elisp functions with a source file must not be treated as C."
  (anvil-elisp-test--require-native-compiler)
  (anvil-elisp-test--with-temp-source
   (lambda (file)
     (anvil-elisp-test--native-compile-symbol-or-skip
      'anvil-elisp-test-fixture-fn)
     (should (subrp (symbol-function 'anvil-elisp-test-fixture-fn)))
     (should-not
      (and (fboundp 'subr-primitive-p)
           (subr-primitive-p (symbol-function 'anvil-elisp-test-fixture-fn))))
     (let ((res (anvil-elisp-test--parse-json
                 (anvil-elisp--get-function-definition
                  "anvil-elisp-test-fixture-fn"))))
       (should-not (plist-member res :is-c-function))
       (should (equal file (plist-get res :file-path)))
       (should
        (string-match-p
         "(defun anvil-elisp-test-fixture-fn"
         (plist-get res :source)))))))

(ert-deftest anvil-elisp-test-native-compiled-no-source-returns-synthetic-stub ()
  "A native-compiled function without source should return a synthetic stub."
  (anvil-elisp-test--require-native-compiler)
  (let ((sym 'anvil-elisp-test-native-no-source))
    (unwind-protect
        (progn
          (defalias sym
            (lambda (x y)
              "Standalone native doc."
              (+ x y 3)))
          (anvil-elisp-test--native-compile-symbol-or-skip sym)
          (let ((res (anvil-elisp-test--parse-json
                      (anvil-elisp--get-function-definition
                       (symbol-name sym)))))
            (should-not (plist-member res :is-c-function))
            (should (plist-get res :source-unavailable))
            (should (equal "native-compiled-no-source"
                           (plist-get res :reason)))
            (should (equal "<native-compiled>"
                           (plist-get res :file-path)))
            (should (string-match-p
                     "Synthetic stub"
                     (plist-get res :source)))
            (should (string-match-p
                     "(defun anvil-elisp-test-native-no-source"
                     (plist-get res :source)))
            (should (string-match-p
                     "Standalone native doc."
                     (plist-get res :source)))))
      (ignore-errors (fmakunbound sym)))))

(provide 'anvil-elisp-test)
;;; anvil-elisp-test.el ends here
