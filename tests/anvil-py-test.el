;;; anvil-py-test.el --- Tests for anvil-py  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 21 Phase 1a — read-only Python locators via tree-sitter-python.
;;
;; Most tests assert against `tests/fixtures/py-sample/app.py'.  Tests
;; that actually run a query ~= skip themselves when the
;; tree-sitter-python grammar is not installed on the runner (the CI
;; matrix doesn't have a pinned grammar install yet); the pure
;; unit-shaped tests — extension dispatch, grammar-missing error
;; shape, query-cache mechanics — run unconditionally.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-treesit)
(require 'anvil-py)

(defconst anvil-py-test--fixture
  (expand-file-name "fixtures/py-sample/app.py"
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defun anvil-py-test--grammar-ready ()
  "Return non-nil when the Python tree-sitter grammar is loadable.
Tests that actually query the tree skip when this returns nil so
the suite stays green on runners that have not yet installed the
grammar.  Locally / in CI with the grammar baked in, all tests
run."
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p 'python)))

(defmacro anvil-py-test--requires-grammar (&rest body)
  "Skip the test body unless the Python grammar is available."
  (declare (indent 0))
  `(if (anvil-py-test--grammar-ready)
       (progn ,@body)
     (ert-skip "tree-sitter-python grammar not installed on this runner")))

;;;; --- pure / grammar-independent -----------------------------------------

(ert-deftest anvil-py-test-dispatch-py-extension ()
  (should (eq 'python (anvil-treesit-language-for-file "/x/app.py")))
  (should (eq 'python (anvil-treesit-language-for-file "/x/app.pyi")))
  (should (null (anvil-treesit-language-for-file "/x/unknown.xyz"))))

(ert-deftest anvil-py-test-grammar-missing-error-shape ()
  "The structured error carries the install hint and source URL."
  (let ((e (anvil-treesit-grammar-missing-error 'python)))
    (should (eq 'grammar-missing (plist-get e :kind)))
    (should (eq 'python (plist-get e :lang)))
    (should (string-match-p "treesit-install-language-grammar"
                            (plist-get e :install-hint)))
    (should (string-match-p "tree-sitter-python"
                            (plist-get e :source-url)))))

(ert-deftest anvil-py-test-query-cache-stable-handle ()
  "Identical (lang . op) calls return the same compiled query object."
  (anvil-py-test--requires-grammar
    (let* ((q1 (anvil-py--query 'functions))
           (q2 (anvil-py--query 'functions)))
      (should (eq q1 q2)))))

;;;; --- locators vs fixture ------------------------------------------------

(ert-deftest anvil-py-test-list-imports-count ()
  (anvil-py-test--requires-grammar
    (let ((ims (anvil-py-list-imports anvil-py-test--fixture)))
      ;; Fixture has: __future__ import, `import os', `import sys as
      ;; system', `from pathlib import Path', `from openpyxl import
      ;; Workbook, load_workbook', `from typing import (...)'.
      (should (= 6 (length ims)))
      ;; Distinguish bare import vs from-import.
      (should (cl-find 'import ims :key (lambda (p) (plist-get p :kind))))
      (should (cl-find 'from   ims :key (lambda (p) (plist-get p :kind)))))))

(ert-deftest anvil-py-test-list-functions-covers-top-level-nested-async-decorated ()
  (anvil-py-test--requires-grammar
    (let* ((fns (anvil-py-list-functions anvil-py-test--fixture))
           (names (mapcar (lambda (p) (plist-get p :name)) fns)))
      (should (member "plain_func" names))
      (should (member "fetch_value" names))
      (should (member "static_helper" names))
      (should (member "multi_decorated" names))
      (should (member "outer" names))
      (should (member "inner" names))      ; nested def
      (should (member "describe" names))   ; method (list-functions is not filtered)
      (let ((fv (cl-find "fetch_value" fns :key
                         (lambda (p) (plist-get p :name))
                         :test #'string=)))
        (should (plist-get fv :async))
        (should (not (plist-get fv :decorated))))
      (let ((sh (cl-find "static_helper" fns :key
                         (lambda (p) (plist-get p :name))
                         :test #'string=)))
        (should (plist-get sh :decorated))
        (should (not (plist-get sh :async)))))))

(ert-deftest anvil-py-test-list-classes ()
  (anvil-py-test--requires-grammar
    (let* ((cls (anvil-py-list-classes anvil-py-test--fixture))
           (names (mapcar (lambda (p) (plist-get p :name)) cls)))
      (should (member "ReportWriter" names))
      (should (member "Config" names))
      (let ((cfg (cl-find "Config" cls :key
                          (lambda (p) (plist-get p :name))
                          :test #'string=)))
        (should (plist-get cfg :decorated))))))  ; @dataclass

(ert-deftest anvil-py-test-list-methods-reportwriter ()
  (anvil-py-test--requires-grammar
    (let* ((ms (anvil-py-list-methods anvil-py-test--fixture "ReportWriter"))
           (names (mapcar (lambda (p) (plist-get p :name)) ms)))
      (should (member "__init__" names))
      (should (member "write" names))
      (should (member "default_name" names))   ; decorated method
      (should (member "write_async" names))    ; async method
      (should (= 4 (length ms)))
      (let ((wa (cl-find "write_async" ms :key
                         (lambda (p) (plist-get p :name))
                         :test #'string=)))
        (should (plist-get wa :async)))
      (let ((dn (cl-find "default_name" ms :key
                         (lambda (p) (plist-get p :name))
                         :test #'string=)))
        (should (plist-get dn :decorated))))))

(ert-deftest anvil-py-test-list-methods-config ()
  (anvil-py-test--requires-grammar
    (let* ((ms (anvil-py-list-methods anvil-py-test--fixture "Config"))
           (names (mapcar (lambda (p) (plist-get p :name)) ms)))
      (should (equal names '("describe"))))))

(ert-deftest anvil-py-test-list-methods-nonexistent-class ()
  (anvil-py-test--requires-grammar
    (should (null (anvil-py-list-methods anvil-py-test--fixture "DoesNotExist")))))

(ert-deftest anvil-py-test-list-decorators-targets ()
  (anvil-py-test--requires-grammar
    (let* ((decos (anvil-py-list-decorators anvil-py-test--fixture))
           (by-target (mapcar (lambda (d)
                                (cons (plist-get d :decorator)
                                      (plist-get d :target)))
                              decos)))
      (should (member (cons "@staticmethod" "static_helper") by-target))
      (should (member (cons "@classmethod" "multi_decorated") by-target))
      (should (member (cons "@property" "multi_decorated") by-target))
      (should (member (cons "@staticmethod" "default_name") by-target))
      (should (member (cons "@dataclass" "Config") by-target))
      (let ((cfg-deco (cl-find "Config" decos :key
                               (lambda (d) (plist-get d :target))
                               :test #'string=)))
        (should (eq 'class (plist-get cfg-deco :target-kind)))))))

(ert-deftest anvil-py-test-find-definition-function ()
  (anvil-py-test--requires-grammar
    (let ((d (anvil-py-find-definition anvil-py-test--fixture "plain_func")))
      (should d)
      (should (eq 'function (plist-get d :kind)))
      (should (string= "plain_func" (plist-get d :name))))))

(ert-deftest anvil-py-test-find-definition-class ()
  (anvil-py-test--requires-grammar
    (let ((d (anvil-py-find-definition anvil-py-test--fixture "ReportWriter")))
      (should d)
      (should (eq 'class (plist-get d :kind)))
      (should (string= "ReportWriter" (plist-get d :name))))))

(ert-deftest anvil-py-test-find-definition-missing-returns-nil ()
  (anvil-py-test--requires-grammar
    (should (null (anvil-py-find-definition anvil-py-test--fixture "no_such")))))

(ert-deftest anvil-py-test-surrounding-form-inside-method ()
  (anvil-py-test--requires-grammar
    (let* ((wr (anvil-py-find-definition anvil-py-test--fixture "write"))
           (b  (plist-get wr :bounds))
           (mid (+ (plist-get b :start) 8))
           (encl (anvil-py-surrounding-form anvil-py-test--fixture mid)))
      (should encl)
      (should (string= "write" (plist-get encl :name))))))

(ert-deftest anvil-py-test-surrounding-form-outside-returns-nil ()
  (anvil-py-test--requires-grammar
    (should (null (anvil-py-surrounding-form anvil-py-test--fixture 1)))))

(ert-deftest anvil-py-test-surrounding-form-kind-filter ()
  (anvil-py-test--requires-grammar
    (let* ((cls (anvil-py-find-definition anvil-py-test--fixture "ReportWriter"))
           (b   (plist-get cls :bounds))
           ;; Point inside the class header but also inside its body —
           ;; the class bounds cover everything.
           (mid (+ (plist-get b :start) 8)))
      ;; :kind 'class returns the class.
      (let ((hit (anvil-py-surrounding-form
                  anvil-py-test--fixture mid :kind 'class)))
        (should hit)
        (should (string= "ReportWriter" (plist-get hit :name)))))))

(ert-deftest anvil-py-test-list-functions-captures-nested-inner ()
  "Regression for capture-ordering bug: nested defs inside another
def must appear in the result even though treesit returns captures
in tree order, which breaks naive pair-grouping state machines."
  (anvil-py-test--requires-grammar
    (let ((names (mapcar (lambda (p) (plist-get p :name))
                         (anvil-py-list-functions anvil-py-test--fixture))))
      (should (member "outer" names))
      (should (member "inner" names))
      (should-not (member "" names)))))

(ert-deftest anvil-py-test-list-functions-sets-class-name-on-methods ()
  "A method's entry in `py-list-functions' carries the enclosing
class via :class-name; a top-level function carries nil."
  (anvil-py-test--requires-grammar
    (let* ((fns (anvil-py-list-functions anvil-py-test--fixture))
           (by-name (lambda (n)
                      (cl-find n fns
                               :key (lambda (p) (plist-get p :name))
                               :test #'string=))))
      (should (null (plist-get (funcall by-name "plain_func") :class-name)))
      (should (null (plist-get (funcall by-name "outer") :class-name)))
      ;; Nested def inside a function is not a method — no enclosing
      ;; class crosses the function boundary.
      (should (null (plist-get (funcall by-name "inner") :class-name)))
      (should (string= "ReportWriter"
                       (plist-get (funcall by-name "write") :class-name)))
      (should (string= "ReportWriter"
                       (plist-get (funcall by-name "default_name") :class-name)))
      (should (string= "Config"
                       (plist-get (funcall by-name "describe") :class-name))))))

(ert-deftest anvil-py-test-find-definition-method-has-class-name ()
  "py-find-definition returning a method carries :class-name."
  (anvil-py-test--requires-grammar
    (let ((d (anvil-py-find-definition anvil-py-test--fixture "describe")))
      (should d)
      (should (string= "Config" (plist-get d :class-name))))))

(ert-deftest anvil-py-test-list-methods-entry-uses-class-name ()
  "Methods list carries :class-name (not :class) for plist shape
parity with `py-list-functions'."
  (anvil-py-test--requires-grammar
    (let ((ms (anvil-py-list-methods anvil-py-test--fixture "ReportWriter")))
      (should (cl-every (lambda (m)
                          (string= "ReportWriter" (plist-get m :class-name)))
                        ms))
      ;; Legacy :class key is gone.
      (should (cl-every (lambda (m) (null (plist-get m :class))) ms)))))

;;;; --- error paths --------------------------------------------------------

(ert-deftest anvil-py-test-list-imports-errors-on-missing-file ()
  (anvil-py-test--requires-grammar
    (should-error (anvil-py-list-imports "/tmp/definitely-not-there.py")
                  :type 'user-error)))

(ert-deftest anvil-py-test-list-imports-errors-on-nil-file ()
  (should-error (anvil-py-list-imports nil) :type 'user-error))

(ert-deftest anvil-py-test-grammar-missing-signals-structured-user-error ()
  "With the grammar stubbed as unavailable, callers see a
user-error whose message is a `read'-able grammar-missing plist."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_lang) nil)))
    (condition-case err
        (progn (anvil-treesit-ensure-grammar 'python)
               (should nil))  ; unreachable
      (user-error
       (let* ((msg (error-message-string err))
              (plist (read msg)))
         (should (eq 'grammar-missing (plist-get plist :kind)))
         (should (eq 'python (plist-get plist :lang))))))))

(provide 'anvil-py-test)
;;; anvil-py-test.el ends here
