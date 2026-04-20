;;; anvil-disclosure-test.el --- Tests for anvil-disclosure -*- lexical-binding: t; -*-

;;; Commentary:

;; Layer-1 projection + budget tests for Doc 28 Phase 1 + 2.  Tests
;; stub `anvil-org-index-search' / `anvil-defs-search' via `cl-letf' so
;; the heavy SQLite backends do not need to be opened.  Budget test
;; uses a synthetic 50-row corpus to verify per-row token cost stays
;; under `anvil-disclosure-layer1-budget-tokens'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-disclosure)
;; Pre-load the real backends so the handlers' internal `require' is a
;; no-op and does not clobber our `cl-letf' stubs on first test run.
(require 'anvil-org-index)
(require 'anvil-defs)
(require 'anvil-file)
(require 'anvil-uri)

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

(ert-deftest anvil-disclosure-test-help-mentions-phase2-tools ()
  "disclosure-help lists the Phase 2 tools."
  (let ((help (anvil-disclosure-help-handler)))
    (should (string-match-p "file-read-snippet" help))
    (should (string-match-p "defs-index" help))
    (should (string-match-p "file-outline" help))))

;;;; --- Phase 2: file-read-snippet (Layer 2) ------------------------------

(defmacro anvil-disclosure-test--with-temp-file (vars content &rest body)
  "Bind (FILE) to a temp file pre-populated with CONTENT."
  (declare (indent 2))
  (let ((file-var (car vars)))
    `(let ((,file-var (make-temp-file "anvil-disclosure-" nil ".txt")))
       (unwind-protect
           (progn
             (let ((coding-system-for-write 'utf-8-unix))
               (write-region ,content nil ,file-var nil 'silent))
             ,@body)
         (ignore-errors (delete-file ,file-var))))))

(ert-deftest anvil-disclosure-test-snippet-bounds-middle ()
  (let ((pr (anvil-disclosure--snippet-bounds 50 20 1000)))
    (should (= 40 (car pr)))
    (should (= 59 (cdr pr)))))

(ert-deftest anvil-disclosure-test-snippet-bounds-top-edge ()
  "Window near the top is clamped so we still fill the requested size."
  (let ((pr (anvil-disclosure--snippet-bounds 3 20 1000)))
    (should (= 1 (car pr)))
    (should (= 20 (cdr pr)))))

(ert-deftest anvil-disclosure-test-snippet-bounds-bottom-edge ()
  "Window near the bottom shifts start up so we still fill WINDOW lines.
Symmetric to the top-edge case — the bounds helper maximises context
rather than leaving a short tail."
  (let ((pr (anvil-disclosure--snippet-bounds 95 20 100)))
    (should (= 81 (car pr)))
    (should (= 100 (cdr pr)))))

(ert-deftest anvil-disclosure-test-snippet-bounds-short-file ()
  "When the file is shorter than WINDOW, bounds are clamped and
:truncated will be reported by the handler."
  (let ((pr (anvil-disclosure--snippet-bounds 2 20 5)))
    (should (= 1 (car pr)))
    (should (= 5 (cdr pr)))))

(ert-deftest anvil-disclosure-test-clamp-window ()
  (should (= 20 (anvil-disclosure--clamp-window nil)))
  (should (= 5  (anvil-disclosure--clamp-window "5")))
  (should (= 1  (anvil-disclosure--clamp-window "0")))
  (should (= anvil-disclosure-snippet-window-max
             (anvil-disclosure--clamp-window "9999"))))

(ert-deftest anvil-disclosure-test-file-read-snippet-basic ()
  (anvil-disclosure-test--with-temp-file (f)
      (mapconcat (lambda (i) (format "line-%02d" i))
                 (number-sequence 1 40) "\n")
    (let* ((res (anvil-disclosure-file-read-snippet f 20 10)))
      (should (equal (expand-file-name f) (plist-get res :path)))
      (should (string-match-p "^file:///" (plist-get res :id)))
      (should (string-match-p "#L[0-9]+-[0-9]+\\'"
                              (plist-get res :id)))
      (should (= 10 (1+ (- (plist-get res :line-end)
                           (plist-get res :line-start)))))
      (should (string-match-p "line-20" (plist-get res :body)))
      (should (null (plist-get res :truncated))))))

(ert-deftest anvil-disclosure-test-file-read-snippet-truncated-bottom ()
  "When the file is smaller than the window, :truncated is t."
  (anvil-disclosure-test--with-temp-file (f)
      (mapconcat #'identity '("a" "b" "c" "d" "e") "\n")
    (let ((res (anvil-disclosure-file-read-snippet f 3 20)))
      (should (eq t (plist-get res :truncated)))
      (should (= 5 (plist-get res :total-lines))))))

(ert-deftest anvil-disclosure-test-tool-file-read-snippet-printable ()
  (anvil-disclosure-test--with-temp-file (f)
      "one\ntwo\nthree\nfour\nfive\n"
    (let ((s (anvil-disclosure--tool-file-read-snippet f "3" "3")))
      (should (stringp s))
      (should (string-match-p ":body" s))
      (should (string-match-p "file:///" s)))))

;;;; --- Phase 2: defs-index (Layer 1) -------------------------------------

(defun anvil-disclosure-test--fake-defs-rows (n)
  "Generate N synthetic anvil-defs-search row plists."
  (cl-loop for i from 1 to n
           collect (list :kind "defun"
                         :name (format "my-fn-%02d" i)
                         :file (format "/src/module-%02d.el" i)
                         :line (* i 13)
                         :end-line (1+ (* i 13))
                         :arity-min 0
                         :arity-max 0
                         :docstring-head ""
                         :obsolete-p nil)))

(defmacro anvil-disclosure-test--with-fake-defs-search (rows &rest body)
  (declare (indent 1))
  `(cl-letf (((symbol-function 'anvil-defs-search)
              (lambda (&rest _args) ,rows)))
     ,@body))

(ert-deftest anvil-disclosure-test-defs-project-basic ()
  (let* ((rows (anvil-disclosure-test--fake-defs-rows 2))
         (pointers (anvil-disclosure--defs-project-rows rows))
         (first (car pointers)))
    (should (= 2 (length pointers)))
    (should (equal "defs://0/my-fn-01" (plist-get first :id)))
    (should (equal "defun" (plist-get first :kind)))))

(ert-deftest anvil-disclosure-test-defs-index-handler ()
  (anvil-disclosure-test--with-fake-defs-search
      (anvil-disclosure-test--fake-defs-rows 3)
    (let ((res (anvil-disclosure-defs-index "my")))
      (should (= 3 (plist-get res :count)))
      (should (equal "defs://0/my-fn-01"
                     (plist-get (car (plist-get res :rows)) :id))))))

(ert-deftest anvil-disclosure-test-defs-index-empty-query ()
  "An empty/blank query returns an empty row list without hitting the DB."
  (cl-letf (((symbol-function 'anvil-defs-search)
             (lambda (&rest _)
               (error "defs-search must not run on blank query"))))
    (let ((res (anvil-disclosure-defs-index "")))
      (should (= 0 (plist-get res :count))))))

(ert-deftest anvil-disclosure-test-defs-index-budget ()
  "Layer-1 defs pointer budget: avg tokens/row < budget."
  (let* ((rows (anvil-disclosure-test--fake-defs-rows 50))
         (pointers (anvil-disclosure--defs-project-rows rows))
         (total (apply #'+ (mapcar (lambda (p)
                                     (max 1 (/ (length (format "%S" p))
                                               4)))
                                   pointers)))
         (avg   (/ (float total) (max 1 (length pointers)))))
    (should (< avg anvil-disclosure-layer1-budget-tokens))))

;;;; --- Phase 2: Layer-3 URI auto-strip -----------------------------------

(ert-deftest anvil-disclosure-test-file-read-strip-uri-plain-path ()
  "A plain absolute path is passed through unchanged."
  (should (equal (list "/a/b" "3" "5")
                 (anvil-file--tool-read--strip-uri "/a/b" "3" "5"))))

(ert-deftest anvil-disclosure-test-file-read-strip-uri-with-range ()
  "A file:// URI with a line range seeds offset/limit when unset."
  (pcase-let ((`(,p ,o ,l)
               (anvil-file--tool-read--strip-uri
                "file:///tmp/x.el#L10-14" nil nil)))
    (should (equal "/tmp/x.el" p))
    (should (equal "9" o))
    (should (equal "5" l))))

(ert-deftest anvil-disclosure-test-file-read-strip-uri-caller-wins ()
  "Explicit offset/limit override the URI-embedded range."
  (pcase-let ((`(,_p ,o ,l)
               (anvil-file--tool-read--strip-uri
                "file:///tmp/x.el#L10-14" "0" "3")))
    (should (equal "0" o))
    (should (equal "3" l))))

(ert-deftest anvil-disclosure-test-file-read-strip-uri-no-range ()
  "A file:// URI without a range just strips the scheme."
  (pcase-let ((`(,p ,o ,l)
               (anvil-file--tool-read--strip-uri
                "file:///tmp/x.el" nil nil)))
    (should (equal "/tmp/x.el" p))
    (should (null o))
    (should (null l))))

(require 'anvil-elisp)

(ert-deftest anvil-disclosure-test-elisp-strip-defs-uri ()
  "defs://SHA/SYM → SYM; plain names pass through."
  (should (equal "anvil-server-register-tool"
                 (anvil-elisp--strip-defs-uri
                  "defs://0/anvil-server-register-tool")))
  (should (equal "plain-name"
                 (anvil-elisp--strip-defs-uri "plain-name")))
  (should (equal "cl-lib/reduce"
                 (anvil-elisp--strip-defs-uri
                  "defs://abc/cl-lib/reduce"))))

(provide 'anvil-disclosure-test)
;;; anvil-disclosure-test.el ends here
