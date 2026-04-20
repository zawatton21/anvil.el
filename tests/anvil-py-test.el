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

;;;; --- edit primitives (Phase 2a) ----------------------------------------

(defun anvil-py-test--temp-copy (path)
  "Copy PATH to a fresh temp file and return the temp path.
Tests that mutate the fixture use this so the in-tree fixture stays
pristine across runs."
  (let ((tmp (make-temp-file "anvil-py-edit-" nil ".py")))
    (copy-file path tmp t)
    tmp))

(defmacro anvil-py-test--with-edit-copy (var &rest body)
  "Bind VAR to a fresh temp copy of the fixture, run BODY, delete the copy."
  (declare (indent 1))
  `(let ((,var (anvil-py-test--temp-copy anvil-py-test--fixture)))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file ,var)))))

(ert-deftest anvil-py-test-add-import-from-merges-into-existing ()
  "Adding a new name to an existing from-import merges in place,
producing a single replacement op, and a double-apply is a no-op."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (let ((plan (anvil-py-add-import
                   f '(:kind from :from "openpyxl" :names ("Cell")))))
        (should (= 1 (length (plist-get plan :ops))))
        (should (string-match-p "from openpyxl import .*Cell"
                                (plist-get
                                 (car (plist-get plan :ops)) :replacement))))
      ;; Apply, then a second preview should be a no-op.
      (anvil-py-add-import
       f '(:kind from :from "openpyxl" :names ("Cell")) :apply t)
      (let ((re-plan (anvil-py-add-import
                      f '(:kind from :from "openpyxl" :names ("Cell")))))
        (should (null (plist-get re-plan :ops)))
        (should (string-match-p "no-op" (plist-get re-plan :summary)))))))

(ert-deftest anvil-py-test-add-import-from-new-module ()
  "Adding a from-import for a module not yet present inserts a new
line after the existing import block."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (anvil-py-add-import
       f '(:kind from :from "json" :names ("loads" "dumps")) :apply t)
      (let ((text (with-temp-buffer
                    (insert-file-contents f) (buffer-string))))
        (should (string-match-p "^from json import loads, dumps$" text))
        ;; Must not appear before other imports.
        (should (string-match-p "from typing import" text))))))

(ert-deftest anvil-py-test-add-import-bare-new ()
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (anvil-py-add-import f '(:kind import :module "subprocess") :apply t)
      (let ((text (with-temp-buffer
                    (insert-file-contents f) (buffer-string))))
        (should (string-match-p "^import subprocess$" text))))))

(ert-deftest anvil-py-test-add-import-bare-existing-is-noop ()
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (let ((plan (anvil-py-add-import f '(:kind import :module "os"))))
        (should (null (plist-get plan :ops)))))))

(ert-deftest anvil-py-test-add-import-alias ()
  "`import X as Y' when module matches but alias differs is treated
as distinct — adding `import json as j' when only `import json' exists
adds a new statement."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (let ((plan (anvil-py-add-import
                   f '(:kind import :module "sys" :alias "system"))))
        ;; `import sys as system' is already in the fixture → no-op.
        (should (null (plist-get plan :ops))))
      (let ((plan (anvil-py-add-import
                   f '(:kind import :module "sys"))))
        ;; Bare `import sys' is NOT present — only the aliased form is.
        (should (= 1 (length (plist-get plan :ops))))))))

(ert-deftest anvil-py-test-add-import-errors-when-spec-incomplete ()
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (should-error
       (anvil-py-add-import f '(:kind from :from "openpyxl"))
       :type 'user-error)
      (should-error
       (anvil-py-add-import f '(:kind import))
       :type 'user-error))))

(ert-deftest anvil-py-test-remove-import-drops-single-name ()
  "Removing one name from a multi-name from-import leaves the others.
Assertions check imports via `anvil-py-list-imports' rather than
string-matching the file text, because `load_workbook' is also used
as an identifier in the fixture body."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (anvil-py-remove-import
       f '(:kind from :from "openpyxl" :names ("load_workbook")) :apply t)
      (let* ((ims (anvil-py-list-imports f))
             (openpyxl (cl-find-if (lambda (i)
                                     (string-match-p "openpyxl"
                                                     (plist-get i :text)))
                                   ims)))
        (should openpyxl)
        (should (string-match-p "Workbook" (plist-get openpyxl :text)))
        (should-not (string-match-p "load_workbook" (plist-get openpyxl :text)))))))

(ert-deftest anvil-py-test-remove-import-drops-last-name-deletes-statement ()
  "Removing the last remaining name drops the whole statement including
its trailing newline.  Asserts via `anvil-py-list-imports' so the
fixture docstring mentioning \"openpyxl\" doesn't false-positive."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (anvil-py-remove-import
       f '(:kind from :from "openpyxl"
           :names ("Workbook" "load_workbook")) :apply t)
      (let ((ims (anvil-py-list-imports f)))
        (should-not (cl-find-if (lambda (i)
                                  (string-match-p "openpyxl"
                                                  (plist-get i :text)))
                                ims))
        ;; Sibling imports unaffected.
        (should (cl-find-if (lambda (i)
                              (string-match-p "pathlib"
                                              (plist-get i :text)))
                            ims))))))

(ert-deftest anvil-py-test-remove-import-bare ()
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (anvil-py-remove-import f '(:kind import :module "os") :apply t)
      (let ((text (with-temp-buffer
                    (insert-file-contents f) (buffer-string))))
        (should-not (string-match-p "^import os$" text))))))

(ert-deftest anvil-py-test-remove-import-missing-is-noop ()
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (let ((plan (anvil-py-remove-import
                   f '(:kind import :module "nonexistent"))))
        (should (null (plist-get plan :ops)))))))

(ert-deftest anvil-py-test-add-then-remove-round-trip ()
  "Adding an import and then removing it returns the file to byte
identity with the original fixture."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (let ((before (with-temp-buffer
                      (insert-file-contents f) (buffer-string))))
        (anvil-py-add-import
         f '(:kind from :from "openpyxl" :names ("Cell")) :apply t)
        (anvil-py-remove-import
         f '(:kind from :from "openpyxl" :names ("Cell")) :apply t)
        (let ((after (with-temp-buffer
                       (insert-file-contents f) (buffer-string))))
          (should (string= before after)))))))

(ert-deftest anvil-py-test-edit-plan-has-diff-preview ()
  "Non-noop plans carry a :diff-preview string suitable for display."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (let ((plan (anvil-py-add-import
                   f '(:kind from :from "openpyxl" :names ("Cell")))))
        (should (stringp (plist-get plan :diff-preview)))
        (should (string-match-p "^---" (plist-get plan :diff-preview)))
        (should (string-match-p "\\+from openpyxl" (plist-get plan :diff-preview)))))))

;;;; --- replace-function (Phase 2b) ----------------------------------------

(ert-deftest anvil-py-test-replace-function-top-level ()
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (anvil-py-replace-function
       f "plain_func" "def plain_func(a, b):\n    return a * b" :apply t)
      ;; The body change must survive round-trip through list-functions.
      (let* ((fns (anvil-py-list-functions f))
             (pf (cl-find "plain_func" fns :key
                          (lambda (p) (plist-get p :name))
                          :test #'string=)))
        (should pf)
        (should (string-match-p "return a \\* b"
                                (with-temp-buffer
                                  (insert-file-contents f)
                                  (goto-char (plist-get (plist-get pf :bounds)
                                                        :start))
                                  (buffer-substring (point) (min (point-max)
                                                                 (+ (point) 100))))))))))

(ert-deftest anvil-py-test-replace-function-method-reindents ()
  "Replacing a method with column-0 source re-indents to the method's
column; the surrounding class body stays structurally intact."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (anvil-py-replace-function
       f "describe"
       "def describe(self):\n    return f'renamed {self.name}'"
       :apply t)
      (let ((text (with-temp-buffer
                    (insert-file-contents f) (buffer-string))))
        (should (string-match-p "    def describe(self):" text))
        (should (string-match-p "        return f'renamed" text))
        ;; Sibling class members unchanged.
        (should (string-match-p "@dataclass" text))))))

(ert-deftest anvil-py-test-replace-function-with-class-filter ()
  "When a method name also appears elsewhere, :class resolves it."
  (anvil-py-test--requires-grammar
    (let ((tmp (make-temp-file "anvil-py-ambig-" nil ".py")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (insert "def shared():\n    return 1\n\n"
                      "class A:\n"
                      "    def shared(self):\n        return 2\n\n"
                      "class B:\n"
                      "    def shared(self):\n        return 3\n"))
            ;; Without :class, the name resolves to 3 candidates → error.
            (should-error (anvil-py-replace-function
                           tmp "shared" "def shared():\n    return 9")
                          :type 'user-error)
            ;; With :class A, only one candidate.
            (anvil-py-replace-function
             tmp "shared" "def shared(self):\n    return 99"
             :class "A" :apply t)
            (let ((text (with-temp-buffer
                          (insert-file-contents tmp) (buffer-string))))
              (should (string-match-p "class A:\n    def shared(self):\n        return 99" text))
              ;; B unchanged.
              (should (string-match-p "class B:\n    def shared(self):\n        return 3" text))))
        (ignore-errors (delete-file tmp))))))

(ert-deftest anvil-py-test-replace-function-missing-errors ()
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (should-error
       (anvil-py-replace-function f "no_such" "def no_such(): pass")
       :type 'user-error))))

(ert-deftest anvil-py-test-replace-function-idempotent-identical ()
  "Replacing with identical source is a no-op plan."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      ;; Fixture's plain_func is exactly `def plain_func(a, b):\n    return a + b'.
      (let ((plan (anvil-py-replace-function
                   f "plain_func" "def plain_func(a, b):\n    return a + b")))
        (should (null (plist-get plan :ops)))
        (should (string-match-p "no-op" (plist-get plan :summary)))))))

(ert-deftest anvil-py-test-replace-function-round-trip-byte-identity ()
  "Replace with new body, replace back with original, file byte-identical."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (let ((before (with-temp-buffer
                      (insert-file-contents f) (buffer-string))))
        (anvil-py-replace-function
         f "plain_func" "def plain_func(a, b):\n    return a * b" :apply t)
        (anvil-py-replace-function
         f "plain_func" "def plain_func(a, b):\n    return a + b" :apply t)
        (let ((after (with-temp-buffer
                       (insert-file-contents f) (buffer-string))))
          (should (string= before after)))))))

(ert-deftest anvil-py-test-replace-function-preserves-decorators ()
  "Decorators on a replaced def stay untouched — only the
function_definition node itself is swapped."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      ;; static_helper is @staticmethod-decorated in the fixture.
      (anvil-py-replace-function
       f "static_helper"
       "def static_helper(x):\n    return x + 99"
       :apply t)
      (let ((text (with-temp-buffer
                    (insert-file-contents f) (buffer-string))))
        ;; Decorator line intact.
        (should (string-match-p "^@staticmethod\ndef static_helper" text))
        (should (string-match-p "return x \\+ 99" text))))))

;;;; --- rename-import (Phase 2c) ------------------------------------------

(ert-deftest anvil-py-test-rename-import-bare-alias-rename ()
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (anvil-py-rename-import
       f '(:kind import :module "sys" :new-alias "sy") :apply t)
      (let ((text (with-temp-buffer
                    (insert-file-contents f) (buffer-string))))
        (should (string-match-p "^import sys as sy$" text))
        (should-not (string-match-p "^import sys as system$" text))))))

(ert-deftest anvil-py-test-rename-import-drops-alias-on-nil ()
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (anvil-py-rename-import
       f '(:kind import :module "sys" :new-alias nil) :apply t)
      (let ((text (with-temp-buffer
                    (insert-file-contents f) (buffer-string))))
        (should (string-match-p "^import sys$" text))
        (should-not (string-match-p "^import sys as" text))))))

(ert-deftest anvil-py-test-rename-import-from-preserves-siblings ()
  "Renaming one name in a multi-name from-import must not touch
the other names."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (anvil-py-rename-import
       f '(:kind from :from "openpyxl" :name "Workbook" :new-alias "WB")
       :apply t)
      (let* ((ims (anvil-py-list-imports f))
             (openpyxl (cl-find-if (lambda (i)
                                     (string-match-p "openpyxl"
                                                     (plist-get i :text)))
                                   ims))
             (text (plist-get openpyxl :text)))
        (should (string-match-p "Workbook as WB" text))
        (should (string-match-p "load_workbook" text))))))

(ert-deftest anvil-py-test-rename-import-same-alias-is-noop ()
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (let ((plan (anvil-py-rename-import
                   f '(:kind import :module "sys" :new-alias "system"))))
        (should (null (plist-get plan :ops)))
        (should (string-match-p "no-op" (plist-get plan :summary)))))))

(ert-deftest anvil-py-test-rename-import-missing-module-errors ()
  "Renaming an import that doesn't exist errors — rename is an edit,
not a create (use `py-add-import' for that)."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (should-error
       (anvil-py-rename-import
        f '(:kind import :module "nonexistent" :new-alias "no"))
       :type 'user-error)
      (should-error
       (anvil-py-rename-import
        f '(:kind from :from "openpyxl" :name "NotPresent" :new-alias "NP"))
       :type 'user-error))))

(ert-deftest anvil-py-test-rename-import-spec-validation ()
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (should-error (anvil-py-rename-import f '(:kind import))
                    :type 'user-error)
      (should-error (anvil-py-rename-import f '(:kind from :from "openpyxl"))
                    :type 'user-error))))

;;;; --- wrap-expr (Phase 2c) -----------------------------------------------

(ert-deftest anvil-py-test-wrap-expr-wraps-integer-literal ()
  "Wrapping the integer `1000' of `MAX_ROWS = 1000' with int() produces
`int(1000)'."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (let* ((text (with-temp-buffer (insert-file-contents f) (buffer-string)))
             (idx (string-match "1000\n" text))
             (start (1+ idx))
             (end (+ start 4)))
        (anvil-py-wrap-expr f start end "int(|anvil-hole|)" :apply t)
        (let ((after (with-temp-buffer
                       (insert-file-contents f) (buffer-string))))
          (should (string-match-p "MAX_ROWS = int(1000)" after)))))))

(ert-deftest anvil-py-test-wrap-expr-missing-placeholder-errors ()
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (should-error
       (anvil-py-wrap-expr f 100 110 "no_hole_here()")
       :type 'user-error))))

(ert-deftest anvil-py-test-wrap-expr-duplicate-placeholder-errors ()
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (should-error
       (anvil-py-wrap-expr f 100 110 "f(|anvil-hole|, |anvil-hole|)")
       :type 'user-error))))

(ert-deftest anvil-py-test-wrap-expr-misaligned-range-errors ()
  "A range that doesn't align to a tree-sitter node boundary signals
a user-error rather than producing invalid Python."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      ;; Pick an obviously misaligned range: [500, 503) lands mid-identifier.
      (should-error
       (anvil-py-wrap-expr f 500 503 "f(|anvil-hole|)")
       :type 'user-error))))

(ert-deftest anvil-py-test-wrap-expr-invalid-range-errors ()
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (should-error (anvil-py-wrap-expr f 0 0 "f(|anvil-hole|)")
                    :type 'user-error)
      (should-error (anvil-py-wrap-expr f 10 5 "f(|anvil-hole|)")
                    :type 'user-error))))

(ert-deftest anvil-py-test-wrap-expr-placeholder-only-is-noop ()
  "Wrapping with just the placeholder (no surrounding text) replaces
the range with itself — a no-op plan."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (let* ((text (with-temp-buffer (insert-file-contents f) (buffer-string)))
             (idx (string-match "1000\n" text))
             (start (1+ idx))
             (end (+ start 4))
             (plan (anvil-py-wrap-expr f start end "|anvil-hole|")))
        (should (null (plist-get plan :ops)))))))

(ert-deftest anvil-py-test-replace-function-handles-leading-indent-in-input ()
  "Caller source with leading indent (e.g. copied from inside a
class) is dedented before being re-indented to the target column.
This lets AI callers paste source unchanged without column-0 care."
  (anvil-py-test--requires-grammar
    (anvil-py-test--with-edit-copy f
      (anvil-py-replace-function
       f "plain_func"
       "        def plain_func(a, b):\n            return a ** b"
       :apply t)
      (let ((text (with-temp-buffer
                    (insert-file-contents f) (buffer-string))))
        ;; plain_func is top-level: 0-column indent after dedent.
        (should (string-match-p "^def plain_func(a, b):\n    return a \\*\\* b"
                                text))))))

(provide 'anvil-py-test)
;;; anvil-py-test.el ends here
