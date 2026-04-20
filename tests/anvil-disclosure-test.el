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
(require 'anvil-org)
(require 'anvil-elisp)
(require 'anvil-http)
(require 'anvil-state)

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
  "Each pointer carries a `defs://<sha>/name' URI; sha is deterministic."
  (let* ((rows (anvil-disclosure-test--fake-defs-rows 2))
         (pointers (anvil-disclosure--defs-project-rows rows))
         (first (car pointers))
         (expected-sha (anvil-disclosure--defs-row-sha (car rows))))
    (should (= 2 (length pointers)))
    (should (equal (format "defs://%s/my-fn-01" expected-sha)
                   (plist-get first :id)))
    (should (equal "defun" (plist-get first :kind)))))

(ert-deftest anvil-disclosure-test-defs-index-handler ()
  (anvil-disclosure-test--with-fake-defs-search
      (anvil-disclosure-test--fake-defs-rows 3)
    (let* ((res (anvil-disclosure-defs-index "my"))
           (first (car (plist-get res :rows)))
           (uri (plist-get first :id)))
      (should (= 3 (plist-get res :count)))
      (should (string-match-p "\\`defs://[0-9a-f]+/my-fn-01\\'" uri)))))

(ert-deftest anvil-disclosure-test-defs-row-sha-deterministic ()
  "Same row → same sha; different rows → different sha."
  (let ((r1 (list :name "foo" :file "/a.el" :line 10))
        (r2 (list :name "foo" :file "/a.el" :line 10))
        (r3 (list :name "foo" :file "/b.el" :line 10)))
    (should (equal (anvil-disclosure--defs-row-sha r1)
                   (anvil-disclosure--defs-row-sha r2)))
    (should-not (equal (anvil-disclosure--defs-row-sha r1)
                       (anvil-disclosure--defs-row-sha r3)))))

(ert-deftest anvil-disclosure-test-defs-row-sha-same-name-different-file ()
  "Same-name functions in different files get distinct URIs."
  (let* ((r1 (list :name "f" :file "/a.el" :line 1))
         (r2 (list :name "f" :file "/b.el" :line 1))
         (p1 (anvil-disclosure--defs-row-to-pointer r1))
         (p2 (anvil-disclosure--defs-row-to-pointer r2)))
    (should-not (equal (plist-get p1 :id) (plist-get p2 :id)))))

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
                 (anvil-file--read-normalize-uri-args "/a/b" "3" "5"))))

(ert-deftest anvil-disclosure-test-file-read-strip-uri-with-range ()
  "A file:// URI with a line range seeds offset/limit when unset."
  (pcase-let ((`(,p ,o ,l)
               (anvil-file--read-normalize-uri-args
                "file:///tmp/x.el#L10-14" nil nil)))
    (should (equal "/tmp/x.el" p))
    (should (equal "9" o))
    (should (equal "5" l))))

(ert-deftest anvil-disclosure-test-file-read-strip-uri-caller-wins ()
  "Explicit offset/limit override the URI-embedded range."
  (pcase-let ((`(,_p ,o ,l)
               (anvil-file--read-normalize-uri-args
                "file:///tmp/x.el#L10-14" "0" "3")))
    (should (equal "0" o))
    (should (equal "3" l))))

(ert-deftest anvil-disclosure-test-file-read-strip-uri-no-range ()
  "A file:// URI without a range just strips the scheme."
  (pcase-let ((`(,p ,o ,l)
               (anvil-file--read-normalize-uri-args
                "file:///tmp/x.el" nil nil)))
    (should (equal "/tmp/x.el" p))
    (should (null o))
    (should (null l))))

;;;; --- Phase 3: cross-layer uri-fetch ------------------------------------

(defmacro anvil-disclosure-test--with-stubbed-layer3 (&rest body)
  "Stub every Layer-3 handler so `anvil-uri-fetch' can be exercised
without touching the real org / file / elisp backends.  Returns the
plist the handler was called with inside :body so the test can
assert on routing + arguments."
  `(cl-letf (((symbol-function 'anvil-org--tool-read-by-id)
              (lambda (uuid) (list :stub 'org :uuid uuid)))
             ((symbol-function 'anvil-file-read)
              (lambda (path &optional off lim)
                (list :stub 'file :path path :off off :lim lim)))
             ((symbol-function 'anvil-elisp--get-function-definition)
              (lambda (fn) (list :stub 'elisp :fn fn))))
     ,@body))

(ert-deftest anvil-disclosure-test-uri-fetch-org ()
  (anvil-disclosure-test--with-stubbed-layer3
    (let ((res (anvil-disclosure-uri-fetch "org://abc-123")))
      (should (eq 'org (plist-get res :scheme)))
      (should (eq 'org-read-by-id (plist-get res :resolver)))
      (should (equal (list :stub 'org :uuid "abc-123")
                     (plist-get res :body))))))

(ert-deftest anvil-disclosure-test-uri-fetch-journal ()
  "journal:// routes to the same resolver as org:// (ID after the year)."
  (anvil-disclosure-test--with-stubbed-layer3
    (let ((res (anvil-disclosure-uri-fetch "journal://2026/id-x")))
      (should (eq 'journal (plist-get res :scheme)))
      (should (eq 'org-read-by-id (plist-get res :resolver)))
      (should (equal "id-x"
                     (plist-get (plist-get res :body) :uuid))))))

(ert-deftest anvil-disclosure-test-uri-fetch-file-with-range ()
  "file:// URI range is converted to 0-based offset + inclusive limit."
  (anvil-disclosure-test--with-stubbed-layer3
    (let ((res (anvil-disclosure-uri-fetch
                "file:///tmp/foo.el#L10-14")))
      (should (eq 'file (plist-get res :scheme)))
      (should (eq 'file-read (plist-get res :resolver)))
      (let ((body (plist-get res :body)))
        (should (equal "/tmp/foo.el" (plist-get body :path)))
        (should (= 9 (plist-get body :off)))
        (should (= 5 (plist-get body :lim)))))))

(ert-deftest anvil-disclosure-test-uri-fetch-file-no-range ()
  "file:// URI without a range passes nil offset/limit through."
  (anvil-disclosure-test--with-stubbed-layer3
    (let* ((res (anvil-disclosure-uri-fetch "file:///etc/hosts"))
           (body (plist-get res :body)))
      (should (equal "/etc/hosts" (plist-get body :path)))
      (should (null (plist-get body :off)))
      (should (null (plist-get body :lim))))))

(ert-deftest anvil-disclosure-test-uri-fetch-defs ()
  (anvil-disclosure-test--with-stubbed-layer3
    (let* ((res (anvil-disclosure-uri-fetch
                 "defs://abcd1234/anvil-server-register-tool"))
           (body (plist-get res :body)))
      (should (eq 'defs (plist-get res :scheme)))
      (should (eq 'elisp-get-function-definition
                  (plist-get res :resolver)))
      (should (equal "anvil-server-register-tool"
                     (plist-get body :fn))))))

(ert-deftest anvil-disclosure-test-uri-fetch-http-cache-errors ()
  "http-cache:// with no matching cache entry signals an error.
In Phase 3 this errored because the scheme was unsupported; Phase 4
now dispatches to `anvil-disclosure-http-cache-get' which itself
errors when the sha does not resolve."
  (anvil-disclosure-test--with-stubbed-layer3
    (cl-letf (((symbol-function 'anvil-http-cache-get)
               (lambda (_sha) nil)))
      (should-error (anvil-disclosure-uri-fetch
                     "http-cache://deadbeef")))))

(ert-deftest anvil-disclosure-test-uri-fetch-unknown-scheme ()
  (should-error (anvil-disclosure-uri-fetch "gopher://old")))

(ert-deftest anvil-disclosure-test-tool-uri-fetch-printable ()
  (anvil-disclosure-test--with-stubbed-layer3
    (let ((s (anvil-disclosure--tool-uri-fetch "org://abc-123")))
      (should (stringp s))
      (should (string-match-p ":scheme org" s))
      (should (string-match-p ":resolver org-read-by-id" s)))))

;;;; --- Phase 4: journal-index -------------------------------------------

(defun anvil-disclosure-test--fake-journal-rows (n)
  "Generate N synthetic journal rows under capture/journals-2026.org."
  (cl-loop for i from 1 to n
           collect (list :file (format "/notes/capture/journals-2026.org")
                         :line (* i 10)
                         :level 2
                         :title (format "NOTE 2026-04-%02d" i)
                         :todo "NOTE"
                         :org-id (format "j-%08x" i)
                         :tags '())))

(ert-deftest anvil-disclosure-test-journal-extract-year ()
  (should (equal "2026" (anvil-disclosure--journal-extract-year
                         "/notes/capture/journals-2026.org")))
  (should (null (anvil-disclosure--journal-extract-year
                 "/notes/capture/inbox.org"))))

(ert-deftest anvil-disclosure-test-journal-row-to-pointer ()
  (let* ((row (car (anvil-disclosure-test--fake-journal-rows 1)))
         (p (anvil-disclosure--journal-row-to-pointer row)))
    (should (equal "journal://2026/j-00000001" (plist-get p :id)))
    (should (equal "2026" (plist-get p :year)))))

(ert-deftest anvil-disclosure-test-journal-row-skips-non-journal ()
  "Rows whose path does not match `journals-YYYY' are dropped."
  (should (null (anvil-disclosure--journal-row-to-pointer
                 (list :file "/a/random.org"
                       :org-id "x"
                       :title "t")))))

(ert-deftest anvil-disclosure-test-journal-index-filters-year ()
  "When YEAR is supplied the underlying :file-like carries the year."
  (let (captured)
    (cl-letf (((symbol-function 'anvil-org-index-search)
               (lambda (&rest args)
                 (setq captured args)
                 (list :count 2 :truncated nil
                       :rows (anvil-disclosure-test--fake-journal-rows 2)))))
      (let ((res (anvil-disclosure-journal-index "2026" nil "10")))
        (should (= 2 (plist-get res :count)))
        (should (string-match-p "journals-2026"
                                (plist-get captured :file-like)))
        (should (equal "journal://2026/j-00000001"
                       (plist-get (car (plist-get res :rows)) :id)))))))

(ert-deftest anvil-disclosure-test-journal-index-int-year ()
  "Integer YEAR is formatted as a 4-digit string."
  (let (captured)
    (cl-letf (((symbol-function 'anvil-org-index-search)
               (lambda (&rest args)
                 (setq captured args)
                 (list :count 0 :truncated nil :rows nil))))
      (anvil-disclosure-journal-index 2025)
      (should (string-match-p "journals-2025"
                              (plist-get captured :file-like))))))

;;;; --- Phase 4: http-cache-index / http-cache-get ----------------------

(defmacro anvil-disclosure-test--with-fake-http-cache (entries &rest body)
  "Stub `anvil-http-cache-list' / `anvil-http-cache-get' to return
ENTRIES (a list of cache-list plists with :url :sha :body etc)."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'anvil-http-cache-list)
              (lambda (&rest _) ,entries))
             ((symbol-function 'anvil-http-cache-get)
              (lambda (sha)
                (let ((target (if (string-prefix-p "http-cache://" sha)
                                  (substring sha (length "http-cache://"))
                                sha)))
                  (seq-find (lambda (e)
                              (equal target (plist-get e :sha)))
                            ,entries)))))
     ,@body))

(defun anvil-disclosure-test--fake-http-entries ()
  "Three synthetic cache entries with stable shas."
  (list (list :url "https://a.example/" :sha "aaa111"
              :status 200 :fetched-at 1 :body-length 5
              :content-type "text/plain"
              :body "body-a" :headers '())
        (list :url "https://b.example/" :sha "bbb222"
              :status 200 :fetched-at 2 :body-length 6
              :content-type "application/json"
              :body "body-b" :headers '())
        (list :url "https://c.example/" :sha "ccc333"
              :status 404 :fetched-at 3 :body-length 0
              :content-type nil
              :body "" :headers '())))

(ert-deftest anvil-disclosure-test-http-cache-index-all ()
  (anvil-disclosure-test--with-fake-http-cache
      (anvil-disclosure-test--fake-http-entries)
    (let* ((res (anvil-disclosure-http-cache-index))
           (first (car (plist-get res :rows))))
      (should (= 3 (plist-get res :count)))
      (should (equal "http-cache://aaa111" (plist-get first :id)))
      (should (= 200 (plist-get first :status))))))

(ert-deftest anvil-disclosure-test-http-cache-index-filter ()
  (anvil-disclosure-test--with-fake-http-cache
      (anvil-disclosure-test--fake-http-entries)
    (let ((res (anvil-disclosure-http-cache-index "b.example")))
      (should (= 1 (plist-get res :count)))
      (should (equal "http-cache://bbb222"
                     (plist-get (car (plist-get res :rows)) :id))))))

(ert-deftest anvil-disclosure-test-http-cache-get-hit ()
  (anvil-disclosure-test--with-fake-http-cache
      (anvil-disclosure-test--fake-http-entries)
    (let ((entry (anvil-disclosure-http-cache-get
                  "http-cache://bbb222")))
      (should (equal "https://b.example/" (plist-get entry :url)))
      (should (equal "body-b" (plist-get entry :body))))))

(ert-deftest anvil-disclosure-test-http-cache-get-miss ()
  (anvil-disclosure-test--with-fake-http-cache
      (anvil-disclosure-test--fake-http-entries)
    (should-error (anvil-disclosure-http-cache-get "deadbeef"))))

(ert-deftest anvil-disclosure-test-uri-fetch-http-cache-resolves ()
  "Phase 4 swaps the Phase-3 placeholder — dispatch now works."
  (anvil-disclosure-test--with-fake-http-cache
      (anvil-disclosure-test--fake-http-entries)
    (let* ((res (anvil-disclosure-uri-fetch "http-cache://aaa111"))
           (body (plist-get res :body)))
      (should (eq 'http-cache (plist-get res :scheme)))
      (should (eq 'http-cache-get (plist-get res :resolver)))
      (should (equal "body-a" (plist-get body :body))))))

;;;; --- Phase 4: real sha round-trip ------------------------------------

(ert-deftest anvil-disclosure-test-http-url-sha-stable ()
  "The helper produces a deterministic 64-hex sha256 string."
  (let ((sha1 (anvil-http--url-sha "https://EXAMPLE.com/p"))
        (sha2 (anvil-http--url-sha "https://example.com/p")))
    (should (= 64 (length sha1)))
    ;; normalised → same digest despite capitalisation.
    (should (equal sha1 sha2))))

(ert-deftest anvil-disclosure-test-help-mentions-phase4-tools ()
  (let ((help (anvil-disclosure-help-handler)))
    (should (string-match-p "journal-index" help))
    (should (string-match-p "http-cache-index" help))
    (should (string-match-p "http-cache-get" help))))

(ert-deftest anvil-disclosure-test-elisp-strip-defs-uri ()
  "defs://SHA/SYM → SYM; plain names pass through; Phase-3 real SHAs OK."
  (should (equal "anvil-server-register-tool"
                 (anvil-elisp--strip-defs-uri
                  "defs://0/anvil-server-register-tool")))
  (should (equal "anvil-server-register-tool"
                 (anvil-elisp--strip-defs-uri
                  "defs://abcd1234/anvil-server-register-tool")))
  (should (equal "plain-name"
                 (anvil-elisp--strip-defs-uri "plain-name")))
  (should (equal "cl-lib/reduce"
                 (anvil-elisp--strip-defs-uri
                  "defs://abc/cl-lib/reduce"))))

(provide 'anvil-disclosure-test)
;;; anvil-disclosure-test.el ends here
