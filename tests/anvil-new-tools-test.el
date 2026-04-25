;;; anvil-new-tools-test.el --- Tests for ert-run, byte-compile, outline, sqlite -*- lexical-binding: t; -*-

;;; Commentary:

;; Covers the four token-efficiency MCP tools added 2026-04-15:
;;   elisp-ert-run, elisp-byte-compile-file, file-outline, sqlite-query.
;; Each test writes a minimal temp fixture and calls the private
;; tool function directly (skipping MCP registration) so the tests
;; run under plain ERT.

;;; Code:

(require 'ert)
(require 'anvil-elisp)
(require 'anvil-file)
(require 'anvil-sqlite)

(defun anvil-new-tools-test--write (path content)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil path nil 'silent)))

(defun anvil-new-tools-test--read-plist (str)
  "Parse STR (output of `format \"%S\"' on a plist) back to a plist."
  (car (read-from-string str)))

;;;; --- elisp-ert-run --------------------------------------------------------

(ert-deftest anvil-tools-test-ert-run-all-pass ()
  "A file of passing tests reports :failed=0 and a matching :passed."
  (let* ((tmp (make-temp-file "anvil-ert-" nil ".el")))
    (unwind-protect
        (progn
          (anvil-new-tools-test--write
           tmp
           "(require 'ert)
(ert-deftest anvil-tools-tmp-a () (should (= 1 1)))
(ert-deftest anvil-tools-tmp-b () (should-not (= 1 2)))
")
          (let* ((out (anvil-elisp--ert-run tmp nil))
                 (res (anvil-new-tools-test--read-plist out)))
            (should (= 2 (plist-get res :passed)))
            (should (= 0 (plist-get res :failed)))
            (should (numberp (plist-get res :elapsed-sec)))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest anvil-tools-test-ert-run-captures-failure ()
  "A failing test is reported with :condition text."
  (let* ((tmp (make-temp-file "anvil-ert-" nil ".el")))
    (unwind-protect
        (progn
          (anvil-new-tools-test--write
           tmp
           "(require 'ert)
(ert-deftest anvil-tools-tmp-fail () (should (= 1 2)))
")
          (let* ((out (anvil-elisp--ert-run tmp nil))
                 (res (anvil-new-tools-test--read-plist out))
                 (fails (plist-get res :failures)))
            (should (= 1 (plist-get res :failed)))
            (should (= 1 (length fails)))
            (should (stringp (plist-get (car fails) :condition)))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest anvil-tools-test-ert-run-adds-test-file-directory-to-load-path ()
  "Test files can `require' sibling helpers from their own directory."
  (let* ((dir (make-temp-file "anvil-ert-path-" t))
         (helper (expand-file-name "tmp-helper.el" dir))
         (test-file (expand-file-name "tmp-helper-test.el" dir)))
    (unwind-protect
        (progn
          (anvil-new-tools-test--write
           helper
           "(provide 'tmp-helper)\n(defun tmp-helper-answer () 42)\n")
          (anvil-new-tools-test--write
           test-file
           "(require 'ert)\n(require 'tmp-helper)\n(ert-deftest anvil-tools-tmp-helper-test () (should (= 42 (tmp-helper-answer))))\n")
          (let* ((out (anvil-elisp--ert-run test-file nil))
                 (res (anvil-new-tools-test--read-plist out)))
            (should (= 1 (plist-get res :passed)))
            (should (= 0 (plist-get res :failed)))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest anvil-tools-test-ert-fresh-feature-strips-test-suffix ()
  "Helper infers a feature symbol by stripping the trailing `-test'."
  (should (eq 'anvil-worker
              (anvil-elisp--ert-fresh-feature
               "/tmp/tests/anvil-worker-test.el")))
  (should (eq 'anvil-org-index
              (anvil-elisp--ert-fresh-feature
               "c:/x/tests/anvil-org-index-test.el"))))

(ert-deftest anvil-tools-test-ert-fresh-feature-rejects-non-test-file ()
  "No `-test' suffix means the inference is a no-op (returns nil)."
  (should (null (anvil-elisp--ert-fresh-feature "/tmp/anvil-worker.el"))))

(ert-deftest anvil-tools-test-ert-run-fresh-truthy-string-invokes-cache-invalidation ()
  "Passing `\"t\"' as :fresh triggers the cache invalidation helper."
  (let* ((tmp (make-temp-file "anvil-ert-fresh-" nil "-test.el"))
         (seen nil))
    (unwind-protect
        (progn
          (anvil-new-tools-test--write
           tmp "(require 'ert)\n(ert-deftest anvil-tools-fresh-a () t)\n")
          (cl-letf (((symbol-function 'anvil-elisp--ert-invalidate-cache)
                     (lambda (feat path) (push (list feat path) seen))))
            (anvil-elisp--ert-run tmp nil "t"))
          (should (= 1 (length seen)))
          ;; Because the temp file basename ends with `-test', the helper
          ;; should have inferred the companion feature (an interned
          ;; symbol of the stripped basename) — not nil.
          (should (symbolp (car (car seen)))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest anvil-tools-test-ert-run-fresh-nil-string-skips-invalidation ()
  "Falsy string (\"nil\" / empty) leaves cache alone."
  (let* ((tmp (make-temp-file "anvil-ert-fresh-" nil "-test.el"))
         (called 0))
    (unwind-protect
        (progn
          (anvil-new-tools-test--write
           tmp "(require 'ert)\n(ert-deftest anvil-tools-fresh-b () t)\n")
          (cl-letf (((symbol-function 'anvil-elisp--ert-invalidate-cache)
                     (lambda (&rest _) (cl-incf called))))
            (anvil-elisp--ert-run tmp nil ""))
          (should (= 0 called)))
      (ignore-errors (delete-file tmp)))))

;;;; --- elisp-byte-compile-file ---------------------------------------------

(ert-deftest anvil-tools-test-byte-compile-clean ()
  "A clean file compiles with :ok t."
  (let ((tmp (make-temp-file "anvil-bc-" nil ".el")))
    (unwind-protect
        (progn
          (anvil-new-tools-test--write
           tmp
           ";;; tmp --- x -*- lexical-binding: t; -*-
;;; Commentary: tmp
;;; Code:
(defun anvil-tools-tmp-clean () \"A clean function.\" 1)
(provide 'tmp)
;;; tmp ends here
")
          (let* ((out (anvil-elisp--byte-compile-file tmp))
                 (res (anvil-new-tools-test--read-plist out)))
            (should (eq t (plist-get res :ok)))
            (should (null (plist-get res :errors)))))
      (ignore-errors (delete-file tmp))
      (ignore-errors (delete-file (concat (file-name-sans-extension tmp) ".elc"))))))

(ert-deftest anvil-tools-test-byte-compile-fatal-error ()
  "A malformed file reports :ok nil and a populated :errors list."
  (let ((tmp (make-temp-file "anvil-bc-bad-" nil ".el")))
    (unwind-protect
        (progn
          (anvil-new-tools-test--write
           tmp
           ";;; tmp --- x -*- lexical-binding: t; -*-
;;; Commentary: tmp
;;; Code:
(defun anvil-tools-tmp-bad (
(provide 'tmp)
;;; tmp ends here
")
          (let* ((out (anvil-elisp--byte-compile-file tmp))
                 (res (anvil-new-tools-test--read-plist out)))
            (should-not (plist-get res :ok))
            (should (cl-find-if
                     (lambda (msg)
                       (string-match-p "End of file during parsing" msg))
                     (plist-get res :errors)))))
      (ignore-errors (delete-file tmp))
      (ignore-errors (delete-file (concat (file-name-sans-extension tmp) ".elc"))))))

(ert-deftest anvil-tools-test-byte-compile-offload-e2e ()
  "elisp-byte-compile-file goes through the offload REPL end-to-end.
Register the real tool via `anvil-elisp-enable', call it over the
dispatcher, and confirm the .elc appears on disk.  Load-path must
be inherited so the target file's `(require)'s resolve."
  (require 'anvil-elisp)
  (require 'anvil-offload)
  (let ((tmp (make-temp-file "anvil-bc-offload-" nil ".el")))
    (unwind-protect
        (progn
          (anvil-new-tools-test--write
           tmp
           ";;; tmp --- x -*- lexical-binding: t; -*-
;;; Commentary: tmp
;;; Code:
(defun anvil-tools-tmp-offload () \"offload-path clean function.\" 7)
(provide 'anvil-tools-tmp-offload)
;;; tmp ends here
")
          (anvil-elisp-enable)
          (let* ((params `((name . "elisp-byte-compile-file")
                           (arguments . ((file . ,tmp)))))
                 (resp (anvil-server--handle-tools-call
                        "t-bc-off" params
                        (make-anvil-server-metrics) "emacs-eval"))
                 (decoded (json-read-from-string resp))
                 (result (alist-get 'result decoded))
                 (is-error (alist-get 'isError result))
                 (text (alist-get 'text
                                  (aref (alist-get 'content result) 0)))
                 (plist (anvil-new-tools-test--read-plist text)))
            (should (eq :json-false is-error))
            (should (eq t (plist-get plist :ok)))
            (should (file-exists-p
                     (concat (file-name-sans-extension tmp) ".elc")))))
      (anvil-server-unregister-tool "elisp-byte-compile-file" "emacs-eval")
      (ignore-errors (anvil-offload-stop-repl))
      (ignore-errors (delete-file tmp))
      (ignore-errors (delete-file (concat (file-name-sans-extension tmp) ".elc"))))))

(ert-deftest anvil-tools-test-org-index-rebuild-offload-e2e ()
  "`org-index-rebuild' dispatches through offload, writes the DB,
and returns a serialized summary plist to the MCP client.

Scopes the test to a tmp directory with a handful of synthetic
org files so the rebuild completes well within the timeout; the
checkpoint protocol itself is covered by the anvil-org-index
unit tests.  What this test validates is the full wiring:
`:offload t :offload-inherit-load-path t :resumable t' plus the
subprocess's direct DB open path."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (require 'anvil-org-index)
  (require 'anvil-offload)
  (let* ((tmpdir (make-temp-file "anvil-idx-e2e-" t))
         (dbfile (expand-file-name "e2e.db" tmpdir))
         (anvil-org-index-db-path dbfile)
         (anvil-org-index-paths (list tmpdir))
         (anvil-org-index-rebuild-checkpoint-every 2)
         (anvil-org-index--backend nil)
         (anvil-org-index--db nil))
    (unwind-protect
        (progn
          (dotimes (i 3)
            (anvil-new-tools-test--write
             (expand-file-name (format "e%d.org" i) tmpdir)
             (format "* Heading %d\n** Child %d\n" i i)))
          (anvil-org-index-enable)
          (let* ((params `((name . "org-index-rebuild")
                           (arguments . ((paths . ,tmpdir)))))
                 (resp (anvil-server--handle-tools-call
                        "t-idx-rebuild" params
                        (make-anvil-server-metrics) "emacs-eval"))
                 (decoded (json-read-from-string resp))
                 (result (alist-get 'result decoded))
                 (is-error (alist-get 'isError result))
                 (text (alist-get 'text
                                  (aref (alist-get 'content result) 0)))
                 (plist (anvil-new-tools-test--read-plist text)))
            (should (eq :json-false is-error))
            (should (= 3 (plist-get plist :files)))
            (should (= 6 (plist-get plist :headlines)))
            (should (numberp (plist-get plist :elapsed-sec)))))
      (ignore-errors (anvil-org-index-disable))
      (ignore-errors (anvil-offload-stop-repl))
      (ignore-errors (delete-directory tmpdir t)))))

;;;; --- file-outline ---------------------------------------------------------

(ert-deftest anvil-tools-test-outline-elisp ()
  "Outline of an Elisp file lists its def-forms."
  (let ((tmp (make-temp-file "anvil-outline-" nil ".el")))
    (unwind-protect
        (progn
          (anvil-new-tools-test--write
           tmp
           "(defvar anvil-tools-tmp-v 1)
(defun anvil-tools-tmp-fn () 2)
(defmacro anvil-tools-tmp-m () 3)
(defcustom anvil-tools-tmp-c 4 \"doc\" :type 'integer)
")
          (let* ((out (anvil-file--tool-outline tmp nil))
                 (res (anvil-new-tools-test--read-plist out))
                 (items (plist-get res :items))
                 (names (mapcar (lambda (it) (plist-get it :name)) items)))
            (should (equal "elisp" (plist-get res :format)))
            (should (= 4 (plist-get res :count)))
            (should (member "anvil-tools-tmp-v" names))
            (should (member "anvil-tools-tmp-fn" names))
            (should (member "anvil-tools-tmp-m" names))
            (should (member "anvil-tools-tmp-c" names))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest anvil-tools-test-outline-org ()
  "Outline of an org file lists its headlines with h1/h2 kinds."
  (let ((tmp (make-temp-file "anvil-outline-" nil ".org")))
    (unwind-protect
        (progn
          (anvil-new-tools-test--write
           tmp
           "* Alpha
** Beta
* Gamma
")
          (let* ((out (anvil-file--tool-outline tmp nil))
                 (res (anvil-new-tools-test--read-plist out))
                 (items (plist-get res :items)))
            (should (equal "org" (plist-get res :format)))
            (should (= 3 (plist-get res :count)))
            (should (equal "h1" (plist-get (nth 0 items) :kind)))
            (should (equal "Alpha" (plist-get (nth 0 items) :name)))
            (should (equal "h2" (plist-get (nth 1 items) :kind)))
            (should (equal "Gamma" (plist-get (nth 2 items) :name)))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest anvil-tools-test-outline-markdown ()
  "Outline of a Markdown file lists its hash headings."
  (let ((tmp (make-temp-file "anvil-outline-" nil ".md")))
    (unwind-protect
        (progn
          (anvil-new-tools-test--write
           tmp
           "# One
## Two
### Three
")
          (let* ((out (anvil-file--tool-outline tmp nil))
                 (res (anvil-new-tools-test--read-plist out))
                 (items (plist-get res :items)))
            (should (equal "markdown" (plist-get res :format)))
            (should (= 3 (plist-get res :count)))
            (should (equal "h1" (plist-get (nth 0 items) :kind)))
            (should (equal "h3" (plist-get (nth 2 items) :kind)))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest anvil-tools-test-outline-unknown-extension-errors ()
  "Outline raises an MCP tool error when format cannot be inferred."
  (let ((tmp (make-temp-file "anvil-outline-" nil ".xyz")))
    (unwind-protect
        (progn
          (anvil-new-tools-test--write tmp "whatever\n")
          (should-error (anvil-file--tool-outline tmp nil)
                        :type 'anvil-server-tool-error))
      (ignore-errors (delete-file tmp)))))

;;;; --- sqlite-query ---------------------------------------------------------

(defun anvil-tools-test--have-sqlite ()
  (and (fboundp 'sqlite-available-p) (sqlite-available-p)))

(ert-deftest anvil-tools-test-sqlite-query-select ()
  "A SELECT against a seeded DB returns rows."
  (skip-unless (anvil-tools-test--have-sqlite))
  (let* ((tmp (make-temp-file "anvil-sql-" nil ".db"))
         (db (sqlite-open tmp)))
    (unwind-protect
        (progn
          (sqlite-execute db "CREATE TABLE t (id INTEGER, name TEXT)")
          (sqlite-execute db "INSERT INTO t VALUES (1, 'a'), (2, 'b')")
          (sqlite-close db)
          (let* ((out (anvil-sqlite--tool-query
                       tmp "SELECT id, name FROM t ORDER BY id" nil))
                 (res (anvil-new-tools-test--read-plist out)))
            (should (= 2 (plist-get res :row-count)))
            (should (equal '((1 "a") (2 "b")) (plist-get res :rows)))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest anvil-tools-test-sqlite-query-rejects-write ()
  "Non-SELECT statements are rejected via `anvil-server-tool-error'."
  (skip-unless (anvil-tools-test--have-sqlite))
  (let ((tmp (make-temp-file "anvil-sql-" nil ".db")))
    (unwind-protect
        (progn
          (let ((db (sqlite-open tmp)))
            (sqlite-execute db "CREATE TABLE t (id INTEGER)")
            (sqlite-close db))
          (should-error
           (anvil-sqlite--tool-query tmp "INSERT INTO t VALUES (1)" nil)
           :type 'anvil-server-tool-error))
      (ignore-errors (delete-file tmp)))))

(ert-deftest anvil-tools-test-sqlite-query-with-params ()
  "Parameterized queries accept a JSON array of bind values."
  (skip-unless (anvil-tools-test--have-sqlite))
  (let ((tmp (make-temp-file "anvil-sql-" nil ".db")))
    (unwind-protect
        (progn
          (let ((db (sqlite-open tmp)))
            (sqlite-execute db "CREATE TABLE t (id INTEGER, name TEXT)")
            (sqlite-execute db "INSERT INTO t VALUES (1, 'foo'), (2, 'bar')")
            (sqlite-close db))
          (let* ((out (anvil-sqlite--tool-query
                       tmp "SELECT name FROM t WHERE id = ?" "[2]"))
                 (res (anvil-new-tools-test--read-plist out)))
            (should (= 1 (plist-get res :row-count)))
            (should (equal '(("bar")) (plist-get res :rows)))))
      (ignore-errors (delete-file tmp)))))

;;; anvil-new-tools-test.el ends here
