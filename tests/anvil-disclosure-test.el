;;; anvil-disclosure-test.el --- Tests for anvil-disclosure -*- lexical-binding: t; -*-

;;; Commentary:

;; Layer-1 projection + budget tests for Doc 28 Phase 1.  Tests stub
;; `anvil-org-index-search' via `cl-letf' so the heavy SQLite backend
;; does not need to be opened.  Budget test uses a synthetic 50-row
;; corpus to verify per-row token cost stays under
;; `anvil-disclosure-layer1-budget-tokens'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-disclosure)
;; Pre-load the real backend so the handler's internal `require' is a
;; no-op and does not clobber our `cl-letf' stub on first test run.
(require 'anvil-org-index)

(defun anvil-disclosure-test--fake-rows (n)
  "Generate N synthetic org-index rows (plists)."
  (cl-loop for i from 1 to n
           collect (list :file      (format "/home/u/notes/file-%02d.org" i)
                         :line      (* i 10)
                         :level     2
                         :title     (format "Example headline %d for %dth" i i)
                         :todo      "TODO"
                         :priority  nil
                         :scheduled nil
                         :deadline  nil
                         :org-id    (format "id-%08x" i)
                         :tags      '("project" "demo"))))

(defmacro anvil-disclosure-test--with-fake-search (rows &rest body)
  "Stub `anvil-org-index-search' to return ROWS inside BODY."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'anvil-org-index-search)
              (lambda (&rest _args)
                (list :count (length ,rows)
                      :truncated nil
                      :rows ,rows))))
     ,@body))

;;;; --- Layer-1 projection -------------------------------------------------

(ert-deftest anvil-disclosure-test-project-rows-basic ()
  (let* ((rows (anvil-disclosure-test--fake-rows 3))
         (pointers (anvil-disclosure--project-rows rows))
         (first (car pointers)))
    (should (= 3 (length pointers)))
    (should (string-prefix-p "org://id-" (plist-get first :id)))
    (should (equal "Example headline 1 for 1th"
                   (plist-get first :title)))
    (should (string-match-p "file-01\\.org\\'"
                            (plist-get first :path)))))

(ert-deftest anvil-disclosure-test-project-drops-rows-without-id ()
  "Rows without :org-id are silently dropped from the Layer-1 output."
  (let* ((rows (list (list :file "/a.org" :title "t1" :org-id nil)
                     (list :file "/b.org" :title "t2" :org-id "")
                     (list :file "/c.org" :title "t3" :org-id "x")))
         (pointers (anvil-disclosure--project-rows rows)))
    (should (= 1 (length pointers)))
    (should (equal "org://x" (plist-get (car pointers) :id)))))

;;;; --- handler orchestration ----------------------------------------------

(ert-deftest anvil-disclosure-test-handler-clamps-limit ()
  (anvil-disclosure-test--with-fake-search
      (anvil-disclosure-test--fake-rows 5)
    (let ((res (anvil-disclosure-org-index-index nil nil "99999")))
      ;; Returned count equals stubbed rows (5), regardless of limit.
      (should (= 5 (plist-get res :count))))))

(ert-deftest anvil-disclosure-test-handler-empty-match ()
  (anvil-disclosure-test--with-fake-search '()
    (let ((res (anvil-disclosure-org-index-index "nothing-matches")))
      (should (= 0 (plist-get res :count)))
      (should (null (plist-get res :rows))))))

(ert-deftest anvil-disclosure-test-tool-returns-printed-plist ()
  "The MCP wrapper serialises its plist with `format %S'."
  (anvil-disclosure-test--with-fake-search
      (anvil-disclosure-test--fake-rows 2)
    (let ((s (anvil-disclosure--tool-org-index-index nil nil "50")))
      (should (stringp s))
      (should (string-match-p ":count 2" s))
      (should (string-match-p "org://id-00000001" s)))))

;;;; --- budget -------------------------------------------------------------

(defun anvil-disclosure-test--tokens-per-row (rows)
  "Approximate tokens / pointer by serialising each and dividing chars by 4."
  (let* ((pointers (anvil-disclosure--project-rows rows))
         (total (apply #'+ (mapcar (lambda (p)
                                     (max 1 (/ (length (format "%S" p)) 4)))
                                   pointers))))
    (/ (float total) (max 1 (length pointers)))))

(ert-deftest anvil-disclosure-test-budget-layer1 ()
  "Synthetic 50-row corpus stays under the Layer-1 token budget."
  (let* ((rows (anvil-disclosure-test--fake-rows 50))
         (avg  (anvil-disclosure-test--tokens-per-row rows)))
    (should (< avg anvil-disclosure-layer1-budget-tokens))))

;;;; --- help text ----------------------------------------------------------

(ert-deftest anvil-disclosure-test-help-mentions-all-schemes ()
  "disclosure-help enumerates every registered URI scheme."
  (let ((help (anvil-disclosure-help-handler)))
    (dolist (kind '("org://" "defs://" "file://"
                    "journal://" "http-cache://"))
      (should (string-match-p (regexp-quote kind) help)))))

(ert-deftest anvil-disclosure-test-help-mentions-layers ()
  (let ((help (anvil-disclosure-help-handler)))
    (should (string-match-p "Layer 1" help))
    (should (string-match-p "Layer 2" help))
    (should (string-match-p "Layer 3" help))))

(provide 'anvil-disclosure-test)
;;; anvil-disclosure-test.el ends here
