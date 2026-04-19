;;; anvil-dev-test.el --- Tests for anvil-dev -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercises `anvil-self-sync-check' and its helpers.  The `git'
;; subprocess is stubbed via `cl-letf' so the tests do not depend
;; on the filesystem containing a real anvil worktree.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-dev)

(defun anvil-dev-test--call-process-stub-output (buf text)
  "Insert TEXT into BUF, respecting the call-process conventions for t / nil."
  (let ((dest (cond ((eq buf t) (current-buffer))
                    ((bufferp buf) buf)
                    ((stringp buf) (get-buffer buf))
                    (t nil))))
    (when dest
      (with-current-buffer dest (insert text)))))

(defmacro anvil-dev-test--with-git (replies &rest body)
  "Run BODY with `call-process' (of git) returning preset REPLIES.
REPLIES is a list of (ARGS-MATCHER . OUTPUT) pairs.  First hit wins.
Non-git `call-process' calls still signal exit-status 1."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'call-process)
              (lambda (prog &optional _infile buf _disp &rest args)
                (if (equal prog "git")
                    (let ((match (cl-some (lambda (pair)
                                            (and (funcall (car pair) args)
                                                 pair))
                                          ,replies)))
                      (anvil-dev-test--call-process-stub-output
                       buf (or (cdr match) ""))
                      (if match 0 1))
                  1))))
     ,@body))

(defun anvil-dev-test--args-has (substr)
  "Predicate: true when the CALL-PROCESS args contain SUBSTR."
  (lambda (args) (and (member substr args) t)))

(defun anvil-dev-test--make-dir ()
  (let ((d (make-temp-file "anvil-dev-" t)))
    d))

;;;; --- --git-at -----------------------------------------------------------

(ert-deftest anvil-dev-test-git-at-returns-trimmed-stdout ()
  (let ((d (anvil-dev-test--make-dir)))
    (unwind-protect
        (anvil-dev-test--with-git
            (list (cons (anvil-dev-test--args-has "HEAD")
                        "abcdef1234\n"))
          (should (equal "abcdef1234"
                         (anvil-dev--git-at d "rev-parse" "HEAD"))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-git-at-returns-nil-on-failure ()
  (let ((d (anvil-dev-test--make-dir)))
    (unwind-protect
        (anvil-dev-test--with-git '()
          (should (null (anvil-dev--git-at d "rev-parse" "HEAD"))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-git-at-rejects-non-directory ()
  (should (null (anvil-dev--git-at "/no/such/dir" "rev-parse" "HEAD")))
  (should (null (anvil-dev--git-at nil "rev-parse" "HEAD"))))

;;;; --- --short-sha --------------------------------------------------------

(ert-deftest anvil-dev-test-short-sha-truncates ()
  (should (equal "abcdef1" (anvil-dev--short-sha "abcdef1234567")))
  (should (equal "abc" (anvil-dev--short-sha "abc")))
  (should (null (anvil-dev--short-sha nil))))

;;;; --- self-sync-check integration ---------------------------------------

(ert-deftest anvil-dev-test-self-sync-check-reports-in-sync ()
  "When installed HEAD equals dev HEAD, :in-sync t and :warning nil."
  (let ((installed (anvil-dev-test--make-dir))
        (dev       (anvil-dev-test--make-dir))
        (anvil-dev-source-path nil))
    (unwind-protect
        (let ((anvil-dev-source-path dev))
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (&rest _)
                       (expand-file-name "anvil-server.el" installed))))
            (anvil-dev-test--with-git
                (list
                 (cons (lambda (args) (member "--porcelain" args)) "")
                 (cons (anvil-dev-test--args-has "--abbrev-ref") "master")
                 (cons (anvil-dev-test--args-has "HEAD")
                       "1111111111111111111111111111111111111111"))
              (let ((res (anvil-self-sync-check)))
                (should (eq t   (plist-get res :in-sync)))
                (should (null   (plist-get res :warning)))
                (should (equal  "1111111111111111111111111111111111111111"
                                (plist-get res :installed-head)))
                (should (equal  "1111111111111111111111111111111111111111"
                                (plist-get res :dev-head)))
                (should (equal  0 (plist-get res :installed-dirty-count)))))))
      (delete-directory installed t)
      (delete-directory dev t))))

(ert-deftest anvil-dev-test-self-sync-check-flags-head-mismatch ()
  "Differing installed / dev HEADs set :in-sync nil and fill :warning."
  (let ((installed (anvil-dev-test--make-dir))
        (dev       (anvil-dev-test--make-dir)))
    (unwind-protect
        (let ((anvil-dev-source-path dev))
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (&rest _)
                       (expand-file-name "anvil-server.el" installed)))
                    ;; Track which dir is asked so we can return
                    ;; different HEADs for installed vs dev.
                    ((symbol-function 'call-process)
                     (lambda (prog &optional _i buf _d &rest args)
                       (when (equal prog "git")
                         (let ((text
                                (cond
                                 ((member "--porcelain" args) "")
                                 ((member "--abbrev-ref" args) "master")
                                 ((equal default-directory
                                         (file-name-as-directory installed))
                                  "aaaaaaa1111111111111111111111111111111111")
                                 (t
                                  "bbbbbbb2222222222222222222222222222222222"))))
                           (anvil-dev-test--call-process-stub-output buf text)
                           0)))))
            (let ((res (anvil-self-sync-check)))
              (should (null (plist-get res :in-sync)))
              (should (stringp (plist-get res :warning)))
              (should (string-match-p "HEAD" (plist-get res :warning))))))
      (delete-directory installed t)
      (delete-directory dev t))))

(ert-deftest anvil-dev-test-self-sync-check-handles-missing-locate ()
  "When anvil-server is not on `load-path' the warning flags it."
  (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) nil)))
    (let* ((anvil-dev-source-path nil)
           (res (anvil-self-sync-check)))
      (should (null (plist-get res :installed-dir)))
      (should (string-match-p "not located" (plist-get res :warning))))))

(ert-deftest anvil-dev-test-self-sync-check-no-dev-path-is-in-sync ()
  "Without `anvil-dev-source-path' we compare nothing, so :in-sync t."
  (let ((installed (anvil-dev-test--make-dir)))
    (unwind-protect
        (let ((anvil-dev-source-path nil))
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (&rest _)
                       (expand-file-name "anvil-server.el" installed))))
            (anvil-dev-test--with-git
                (list
                 (cons (lambda (args) (member "--porcelain" args)) "")
                 (cons (anvil-dev-test--args-has "--abbrev-ref") "master")
                 (cons (anvil-dev-test--args-has "HEAD") "ccccccc"))
              (let ((res (anvil-self-sync-check)))
                (should (eq t (plist-get res :in-sync)))
                (should (null (plist-get res :dev-head)))
                (should (null (plist-get res :warning)))))))
      (delete-directory installed t))))

;;;; --- parse-ert-summary --------------------------------------------------

(ert-deftest anvil-dev-test-parse-ert-summary-unskipped ()
  (let ((s (anvil-dev--parse-ert-summary
            "Ran 8 tests, 8 results as expected, 0 unexpected (2026-04-17 ...)")))
    (should (equal 8 (plist-get s :total)))
    (should (equal 8 (plist-get s :passed)))
    (should (equal 0 (plist-get s :failed)))
    (should (equal 0 (plist-get s :skipped)))))

(ert-deftest anvil-dev-test-parse-ert-summary-with-skipped ()
  (let ((s (anvil-dev--parse-ert-summary
            "Ran 12 tests, 10 results as expected, 1 unexpected, 1 skipped")))
    (should (equal 12 (plist-get s :total)))
    (should (equal 10 (plist-get s :passed)))
    (should (equal 1  (plist-get s :failed)))
    (should (equal 1  (plist-get s :skipped)))))

(ert-deftest anvil-dev-test-parse-ert-summary-unmatched ()
  (should (null (anvil-dev--parse-ert-summary "noise — no ERT line here"))))

;;;; --- discover-test-files -------------------------------------------------

(ert-deftest anvil-dev-test-discover-empty-dir ()
  (let ((d (anvil-dev-test--make-dir)))
    (unwind-protect
        (should (null (anvil-dev--discover-test-files d)))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-discover-finds-and-sorts ()
  "Returns only matching files, sorted."
  (let ((d (anvil-dev-test--make-dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "tests" d))
          (dolist (f '("anvil-zzz-test.el" "anvil-aaa-test.el"
                       "notes.el" "anvil-test.el" "helper-test.el"))
            (with-temp-file (expand-file-name (format "tests/%s" f) d)
              (insert "")))
          (let* ((files (anvil-dev--discover-test-files d))
                 (names (mapcar #'file-name-nondirectory files)))
            (should (equal '("anvil-aaa-test.el" "anvil-test.el" "anvil-zzz-test.el")
                           names))))
      (delete-directory d t))))

;;;; --- scaffold-module -----------------------------------------------------

(ert-deftest anvil-dev-test-scaffold-rejects-bad-names ()
  (let ((d (anvil-dev-test--make-dir)))
    (unwind-protect
        (progn
          (should-error (anvil-dev-scaffold-module "" "desc" d) :type 'user-error)
          (should-error (anvil-dev-scaffold-module "Bad-Case" "d" d) :type 'user-error)
          (should-error (anvil-dev-scaffold-module "has space" "d" d) :type 'user-error)
          (should-error (anvil-dev-scaffold-module "x" "" d) :type 'user-error))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-scaffold-writes-both-files ()
  "Happy path: both files created, feature + test-feature match the name."
  (let ((d (anvil-dev-test--make-dir)))
    (unwind-protect
        (let* ((res (anvil-dev-scaffold-module "foo" "Foo module demo" d))
               (mod  (plist-get res :module-file))
               (test (plist-get res :test-file)))
          (should (file-exists-p mod))
          (should (file-exists-p test))
          (let ((content (with-temp-buffer
                           (insert-file-contents mod)
                           (buffer-string))))
            (should (string-match-p "anvil-foo\\.el --- Foo module demo" content))
            (should (string-match-p "(provide 'anvil-foo)" content))
            (should (string-match-p "(defun anvil-foo-enable ()" content))
            ;; Placeholders must all be substituted — no stray %NAME%.
            (should-not (string-match-p "%NAME%" content))
            (should-not (string-match-p "%DESC%" content))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-scaffold-refuses-overwrite ()
  (let ((d (anvil-dev-test--make-dir)))
    (unwind-protect
        (progn
          (anvil-dev-scaffold-module "bar" "first" d)
          (should-error (anvil-dev-scaffold-module "bar" "second" d)
                        :type 'user-error))
      (delete-directory d t))))

;;;; --- run-one-test-file (integration, tiny) -------------------------------

(ert-deftest anvil-dev-test-run-one-file-parses-counts ()
  "End-to-end: write a trivial test file, run it, assert parsed counts."
  (let ((d (anvil-dev-test--make-dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "tests" d))
          (with-temp-file (expand-file-name "tests/anvil-xyz-test.el" d)
            (insert "(require 'ert)\n"
                    "(ert-deftest anvil-xyz-dummy-pass () (should t))\n"
                    "(ert-deftest anvil-xyz-dummy-pass2 () (should (= 2 (+ 1 1))))\n"))
          (let ((r (anvil-dev--run-one-test-file
                    (expand-file-name "tests/anvil-xyz-test.el" d)
                    d)))
            (should (plist-get r :ok))
            (should (equal 2 (plist-get r :total)))
            (should (equal 2 (plist-get r :passed)))
            (should (equal 0 (plist-get r :failed)))))
      (delete-directory d t))))

;;;; --- release audit -----------------------------------------------------

(defun anvil-dev-test--audit-write (file content)
  "Create FILE with CONTENT, making parent directories as needed."
  (let ((dir (file-name-directory file)))
    (when dir (make-directory dir t)))
  (with-temp-file file (insert content)))

(defun anvil-dev-test--audit-make-root ()
  "Return a fresh temp directory set up like an anvil checkout."
  (let ((d (make-temp-file "anvil-dev-audit-" t)))
    (make-directory (expand-file-name "docs/design" d) t)
    d))

(ert-deftest anvil-dev-test-audit-flags-arglist-strip ()
  "A wrapper with `(_args)` is reported as an arglist-strip hazard."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-foo.el" d)
           (concat
            "(defun anvil-foo--tool-probe (_args)\n"
            "  \"docstring.\n\nMCP Parameters:\n  (none)\"\n"
            "  (ignore _args) \"ok\")\n"))
          (let* ((r (anvil-dev-release-audit d))
                 (hits (plist-get r :arglist-strip)))
            (should (= 1 (length hits)))
            (should (equal "anvil-foo.el"
                           (plist-get (car hits) :file)))
            (should (equal "anvil-foo--tool-probe"
                           (plist-get (car hits) :defun)))
            (should-not (plist-get r :clean-p))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-allows-empty-arglist ()
  "A wrapper with `()` is NOT an arglist-strip hazard."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-foo.el" d)
           (concat
            "(defun anvil-foo--tool-probe ()\n"
            "  \"no-arg tool.\" \"ok\")\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :arglist-strip)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-flags-missing-params-section ()
  "A wrapper with real args but no `MCP Parameters:' is flagged."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-foo.el" d)
           (concat
            "(defun anvil-foo--tool-run (task-id)\n"
            "  \"Run something.  Yes, this has an arg but no section.\"\n"
            "  task-id)\n"))
          (let* ((r (anvil-dev-release-audit d))
                 (hits (plist-get r :missing-params)))
            (should (= 1 (length hits)))
            (should (equal "anvil-foo--tool-run"
                           (plist-get (car hits) :defun)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-accepts-documented-params ()
  "A wrapper with real args and an `MCP Parameters:' section is clean."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-foo.el" d)
           (concat
            "(defun anvil-foo--tool-run (task-id)\n"
            "  \"Run task.\n\nMCP Parameters:\n  task-id - Task identifier\"\n"
            "  task-id)\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :missing-params)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-ignores-error-helpers ()
  "Helpers named `*--tool-*-error' are not MCP wrappers and must not be audited."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-foo.el" d)
           (concat
            "(defun anvil-foo--tool-validation-error (message &rest args)\n"
            "  \"Throw validation error MESSAGE with ARGS.\"\n"
            "  (error message args))\n"
            "(defun anvil-foo--tool-file-access-error (locator)\n"
            "  \"Throw file access error for LOCATOR.\"\n"
            "  (error \"%s\" locator))\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :arglist-strip)))
            (should (null (plist-get r :missing-params)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-ignores-single-dash-tool-names ()
  "Only `--tool-' (double-dash) names are MCP wrappers; `-tool-' are not."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-foo.el" d)
           (concat
            "(defun anvil-foo-process-tool-response (response)\n"
            "  \"Process a response coming back from a tool.\"\n"
            "  response)\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :missing-params)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-skips-anvil-dev-module ()
  "The audit must skip `anvil-dev.el' even when it lives in the root."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          ;; Put something that would match if scanned.
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-dev.el" d)
           (concat
            "(defun anvil-dev--tool-probe (_args) \"x\" \"ok\")\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :arglist-strip)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-flags-non-shipped-design-doc ()
  "A design org whose STATUS lacks `SHIPPED' is reported."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "docs/design/04-pty.org" d)
           (concat
            "#+title: PTY\n"
            "* STATUS\n"
            "~DRAFT~ — レビュー前\n"))
          (let* ((r (anvil-dev-release-audit d))
                 (docs (plist-get r :non-shipped-docs)))
            (should (= 1 (length docs)))
            (should (equal "04-pty.org" (plist-get (car docs) :file)))
            (should (string-match-p "DRAFT"
                                    (plist-get (car docs) :status)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-accepts-shipped-design-doc ()
  "A design org whose STATUS contains `SHIPPED' is silent."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "docs/design/01-worker.org" d)
           (concat
            "#+title: Worker\n"
            "* STATUS\n"
            "~Phase 1+2+3 SHIPPED 2026-04-16~\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :non-shipped-docs)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-skips-properties-drawer ()
  "The status extractor must skip the :PROPERTIES: drawer before
picking up the status line itself."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "docs/design/02-ix.org" d)
           (concat
            "* STATUS\n"
            "  :PROPERTIES:\n"
            "  :ID:       abc\n"
            "  :END:\n"
            "~Phase 1 SHIPPED~\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :non-shipped-docs)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-clean-report-all-green ()
  "A tidy tree reports `:clean-p' t and no findings."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-foo.el" d)
           "(defun anvil-foo--tool-probe () \"no-arg.\" \"ok\")\n")
          (anvil-dev-test--audit-write
           (expand-file-name "docs/design/01-ok.org" d)
           "* STATUS\n~SHIPPED~\n")
          (let ((r (anvil-dev-release-audit d)))
            (should (plist-get r :clean-p))
            (should (null (plist-get r :arglist-strip)))
            (should (null (plist-get r :missing-params)))
            (should (null (plist-get r :non-shipped-docs)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-formatted-report-has-root-and-time ()
  "Formatter includes root + audited-at headers regardless of state."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (let* ((r (anvil-dev-release-audit d))
               (text (anvil-dev--audit-format-report r)))
          (should (stringp text))
          (should (string-match-p "anvil release audit — " text))
          (should (string-match-p "root: " text)))
      (delete-directory d t))))

;;;; --- bundle: release-audit :scope + test-run-all :minimal --------------

(ert-deftest anvil-dev-test-release-audit-scope-limits-to-one-file ()
  "`:scope FILE' filters source scanners to that file only."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          ;; Dirty file (arglist-strip hit).
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-bad.el" d)
           "(defun anvil-bad--tool-foo (_x)\n  \"no MCP Parameters.\"\n  \"ok\")\n")
          ;; Clean file.
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-good.el" d)
           "(defun anvil-good--tool-probe () \"no-arg.\" \"ok\")\n")
          (anvil-dev-test--audit-write
           (expand-file-name "docs/design/01-ok.org" d)
           "* STATUS\n~SHIPPED~\n")
          ;; Whole-tree audit sees the hit.
          (let ((r (anvil-dev-release-audit d)))
            (should (plist-get r :arglist-strip))
            (should-not (plist-get r :clean-p)))
          ;; Scope to the clean file — hit disappears.
          (let* ((scope (expand-file-name "anvil-good.el" d))
                 (r (anvil-dev-release-audit d :scope scope)))
            (should (null (plist-get r :arglist-strip)))
            (should (null (plist-get r :missing-params)))
            (should (equal scope (plist-get r :scope))))
          ;; Scope to the bad file — hit re-appears but narrowly.
          (let* ((scope (expand-file-name "anvil-bad.el" d))
                 (r (anvil-dev-release-audit d :scope scope)))
            (should (= 1 (length (plist-get r :arglist-strip))))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-release-audit-scope-nonexistent-returns-empty ()
  "`:scope' to a path that does not exist finds nothing in source."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-bad.el" d)
           "(defun anvil-bad--tool-foo (_x) \"no docs.\" \"ok\")\n")
          (let* ((scope (expand-file-name "nope.el" d))
                 (r (anvil-dev-release-audit d :scope scope)))
            (should (null (plist-get r :arglist-strip)))
            (should (null (plist-get r :missing-params)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-test-run-all-minimal-omits-per-file ()
  "With `:minimal t' the return plist has no :per-file key."
  (cl-letf* ((files '("/tmp/anvil-fake/tests/anvil-x-test.el"))
             ((symbol-function 'anvil-dev--discover-test-files)
              (lambda (_root) files))
             ((symbol-function 'anvil-dev--run-one-test-file)
              (lambda (_file _root)
                (list :file "anvil-x-test.el" :ok t
                      :total 3 :passed 3 :failed 0 :skipped 0
                      :elapsed-ms 42))))
    (let ((default-directory
           (make-temp-file "anvil-dev-minimal-" t)))
      (unwind-protect
          (progn
            (let ((r (anvil-dev-test-run-all default-directory)))
              (should (plist-member r :per-file))
              (should (= 1 (length (plist-get r :per-file)))))
            (let ((r (anvil-dev-test-run-all default-directory
                                             :minimal t)))
              (should-not (plist-member r :per-file))
              (should (= 3 (plist-get r :total)))
              (should (= 3 (plist-get r :passed)))))
        (delete-directory default-directory t)))))

;;; anvil-dev-test.el ends here
