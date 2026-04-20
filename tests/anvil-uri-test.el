;;; anvil-uri-test.el --- Tests for anvil-uri -*- lexical-binding: t; -*-

;;; Commentary:

;; Round-trip + parse-only cases for each citation URI scheme defined
;; in `anvil-uri'.  All pure — no temp files, no MCP.

;;; Code:

(require 'ert)
(require 'anvil-uri)

;;;; --- org:// ------------------------------------------------------------

(ert-deftest anvil-uri-test-org-roundtrip ()
  "org:// URIs round-trip through format + parse."
  (let* ((uri (anvil-uri-org "abc-123"))
         (parsed (anvil-uri-parse uri)))
    (should (equal uri "org://abc-123"))
    (should (eq 'org (plist-get parsed :scheme)))
    (should (equal "abc-123" (plist-get parsed :id)))))

(ert-deftest anvil-uri-test-org-empty-id-errors ()
  "anvil-uri-org refuses empty / non-string IDs."
  (should-error (anvil-uri-org ""))
  (should-error (anvil-uri-org nil)))

;;;; --- defs:// -----------------------------------------------------------

(ert-deftest anvil-uri-test-defs-roundtrip ()
  (let* ((uri (anvil-uri-defs "d41d8c" "anvil-uri-parse"))
         (parsed (anvil-uri-parse uri)))
    (should (equal uri "defs://d41d8c/anvil-uri-parse"))
    (should (eq 'defs (plist-get parsed :scheme)))
    (should (equal "d41d8c" (plist-get parsed :sha)))
    (should (equal "anvil-uri-parse" (plist-get parsed :symbol)))))

(ert-deftest anvil-uri-test-defs-symbol-with-slashes ()
  "Symbol segments may contain / (e.g., namespaced names)."
  (let* ((uri (anvil-uri-defs "sha" "cl-lib/reduce"))
         (parsed (anvil-uri-parse uri)))
    (should (equal "sha" (plist-get parsed :sha)))
    (should (equal "cl-lib/reduce" (plist-get parsed :symbol)))))

;;;; --- file:// -----------------------------------------------------------

(ert-deftest anvil-uri-test-file-posix-roundtrip ()
  (let* ((uri (anvil-uri-file "/home/user/foo.el" 10 20))
         (parsed (anvil-uri-parse uri)))
    (should (equal uri "file:///home/user/foo.el#L10-20"))
    (should (eq 'file (plist-get parsed :scheme)))
    (should (equal "/home/user/foo.el" (plist-get parsed :path)))
    (should (= 10 (plist-get parsed :line-start)))
    (should (= 20 (plist-get parsed :line-end)))))

(ert-deftest anvil-uri-test-file-single-line ()
  (let ((uri (anvil-uri-file "/tmp/a" 5)))
    (should (equal uri "file:///tmp/a#L5"))
    (should (= 5 (plist-get (anvil-uri-parse uri) :line-end)))))

(ert-deftest anvil-uri-test-file-no-range ()
  (let* ((uri (anvil-uri-file "/etc/hosts"))
         (parsed (anvil-uri-parse uri)))
    (should (equal uri "file:///etc/hosts"))
    (should (null (plist-get parsed :line-start)))
    (should (null (plist-get parsed :line-end)))))

(ert-deftest anvil-uri-test-file-windows-drive ()
  "Windows-style drive-letter paths are preserved verbatim in the URI."
  (let* ((uri (anvil-uri-file "C:\\Users\\a.txt" 1 3))
         (parsed (anvil-uri-parse uri)))
    (should (equal uri "file:///C:/Users/a.txt#L1-3"))
    (should (equal "C:/Users/a.txt" (plist-get parsed :path)))))

(ert-deftest anvil-uri-test-file-equal-start-end ()
  "Equal start and end collapse to a single-line `#Ln' form."
  (should (equal (anvil-uri-file "/a/b" 7 7) "file:///a/b#L7")))

;;;; --- journal:// --------------------------------------------------------

(ert-deftest anvil-uri-test-journal-year-int ()
  (let* ((uri (anvil-uri-journal 2026 "id-abc"))
         (parsed (anvil-uri-parse uri)))
    (should (equal uri "journal://2026/id-abc"))
    (should (equal "2026" (plist-get parsed :year)))
    (should (equal "id-abc" (plist-get parsed :id)))))

(ert-deftest anvil-uri-test-journal-year-string ()
  (should (equal (anvil-uri-journal "2025" "x") "journal://2025/x")))

;;;; --- http-cache:// -----------------------------------------------------

(ert-deftest anvil-uri-test-http-cache-roundtrip ()
  (let* ((sha "abc123deadbeef")
         (uri (anvil-uri-http-cache sha))
         (parsed (anvil-uri-parse uri)))
    (should (equal uri (concat "http-cache://" sha)))
    (should (eq 'http-cache (plist-get parsed :scheme)))
    (should (equal sha (plist-get parsed :sha)))))

(ert-deftest anvil-uri-test-http-cache-prefix-unambiguous ()
  "Real http:// URLs do not match the anvil-uri-kind registry.
The distinct `http-cache://' prefix keeps real HTTP URLs unambiguous."
  (should (null (anvil-uri-kind "http://example.com/")))
  (should (eq 'http-cache (anvil-uri-kind "http-cache://x"))))

;;;; --- generic parser ----------------------------------------------------

(ert-deftest anvil-uri-test-parse-returns-nil-on-unknown ()
  (should (null (anvil-uri-parse "gopher://old")))
  (should (null (anvil-uri-parse "plain string")))
  (should (null (anvil-uri-parse ""))))

(ert-deftest anvil-uri-test-scheme-registry-coverage ()
  "Every scheme in `anvil-uri-schemes' has a formatter and round-trips."
  (dolist (entry anvil-uri-schemes)
    (let* ((kind (car entry))
           (sample (pcase kind
                     ('org (anvil-uri-org "id"))
                     ('defs (anvil-uri-defs "sha" "sym"))
                     ('file (anvil-uri-file "/a"))
                     ('journal (anvil-uri-journal "2026" "id"))
                     ('http-cache (anvil-uri-http-cache "h")))))
      (should (eq kind (anvil-uri-kind sample)))
      (should (plist-get (anvil-uri-parse sample) :scheme)))))

(provide 'anvil-uri-test)
;;; anvil-uri-test.el ends here
