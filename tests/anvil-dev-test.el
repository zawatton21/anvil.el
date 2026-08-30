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

;;;; --- codex efficiency check --------------------------------------------

(defun anvil-dev-test--write-codex-efficiency-fixtures (codex-home root)
  "Create a complete Codex efficiency fixture under CODEX-HOME and ROOT."
  (make-directory codex-home t)
  (make-directory (expand-file-name "skills" codex-home) t)
  (with-temp-file (expand-file-name "config.toml" codex-home)
    (insert "[mcp_servers.emacs-eval]\n"
            "command = \"/tmp/anvil\"\n\n"
            "[mcp_servers.serena]\n"
            "command = \"/tmp/uvx\"\n\n"
            "[mcp_servers.context7]\n"
            "command = \"/tmp/npx\"\n"))
  (dolist (skill anvil-dev--codex-efficiency-required-skills)
    (let ((dir (expand-file-name (format "skills/%s" skill) codex-home)))
      (make-directory dir t)
      (with-temp-file (expand-file-name "SKILL.md" dir)
        (insert "---\nname: " skill "\ndescription: test\n---\n"))))
  (make-directory (expand-file-name ".serena" root) t)
  (with-temp-file (expand-file-name ".serena/project.yml" root)
    (insert "project_name: Test\n"))
  (make-directory (expand-file-name ".claude/reference" root) t)
  (with-temp-file (expand-file-name ".claude/reference/codex-efficiency-setup.md" root)
    (insert "# Codex Efficiency Setup\n")))

(ert-deftest anvil-dev-test-codex-efficiency-check-all-green ()
  "A complete fixture reports :ok t and no warnings."
  (let ((codex-home (anvil-dev-test--make-dir))
        (root (anvil-dev-test--make-dir)))
    (unwind-protect
        (progn
          (anvil-dev-test--write-codex-efficiency-fixtures codex-home root)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (name)
                       (cond
                        ((equal name "uvx") "/bin/uvx")
                        ((equal name "npx") "/bin/npx")
                        (t nil)))))
            (let ((r (anvil-codex-efficiency-check codex-home root)))
              (should (plist-get r :ok))
              (should (null (plist-get r :warnings)))
              (should (equal t (cdr (assoc "serena"
                                           (plist-get r :mcp-servers)))))
              (should (equal t (cdr (assoc "notes-development"
                                           (plist-get r :skills))))))))
      (delete-directory codex-home t)
      (delete-directory root t))))

(ert-deftest anvil-dev-test-codex-efficiency-check-warns-on-missing-parts ()
  "Missing MCP sections, executable, skill, and project files are warned."
  (let ((codex-home (anvil-dev-test--make-dir))
        (root (anvil-dev-test--make-dir)))
    (unwind-protect
        (progn
          (make-directory codex-home t)
          (with-temp-file (expand-file-name "config.toml" codex-home)
            (insert "[mcp_servers.emacs-eval]\ncommand = \"/tmp/anvil\"\n"))
          (make-directory (expand-file-name "skills/anvil-memory-worklog"
                                            codex-home)
                          t)
          (with-temp-file (expand-file-name
                           "skills/anvil-memory-worklog/SKILL.md"
                           codex-home)
            (insert "---\nname: anvil-memory-worklog\n---\n"))
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (name)
                       (and (equal name "npx") "/bin/npx"))))
            (let* ((r (anvil-codex-efficiency-check codex-home root))
                   (warnings (plist-get r :warnings))
                   (joined (mapconcat #'identity warnings "\n")))
              (should-not (plist-get r :ok))
              (should (string-match-p "Missing MCP server section: serena"
                                      joined))
              (should (string-match-p "Missing MCP server section: context7"
                                      joined))
              (should (string-match-p "Missing executable.*uvx" joined))
              (should (string-match-p "Missing Codex skill: notes-development"
                                      joined))
              (should (string-match-p "Missing Serena project config"
                                      joined))
              (should (string-match-p "Missing Codex recovery reference"
                                      joined)))))
      (delete-directory codex-home t)
      (delete-directory root t))))

;;;; --- Claude limits report analysis --------------------------------------

(defconst anvil-dev-test--claude-limits-sample
  "what's contributing to your limits usage?

91% of your usage was at >150k context
 longer sessions are more expensive even when cached. /compact mid-task, /clear
 when switching to new tasks.

56% of your usage came from subagent-heavy sessions
 each subagent runs its own requests.

42% of your usage came from sessions active for 8+ hours
 these are often background/loop sessions.

16% of your usage came from /loop

73% of your usage came from mcp server \"emacs-eval\"
 mcp tool results stay in context for the rest of the session.

skills                  % of usage
/loop                          16%

mcp servers             % of usage
emacs-eval                     73%
"
  "Stable sample of Claude Code limits output.")

(ert-deftest anvil-dev-test-claude-limits-analyze-extracts-main-metrics ()
  "The analyzer extracts the day-level metrics from copied limits text."
  (let* ((r (anvil-claude-limits-analyze
             anvil-dev-test--claude-limits-sample))
         (metrics (plist-get r :metrics))
         (top (plist-get r :top-actions)))
    (should (equal 91 (cdr (assq 'high-context metrics))))
    (should (equal 56 (cdr (assq 'subagent-heavy metrics))))
    (should (equal 42 (cdr (assq 'long-sessions metrics))))
    (should (equal 16 (cdr (assq 'loop metrics))))
    (should (equal 73 (cdr (assq 'emacs-eval metrics))))
    (should (eq 'high-context (plist-get (car top) :metric)))
    (should (eq 'critical (plist-get (car top) :severity)))))

(ert-deftest anvil-dev-test-claude-limits-analyze-falls-back-to-table-rows ()
  "When prose is TUI-corrupted, table rows still recover key metrics."
  (let* ((r (anvil-claude-limits-analyze
             "skills % of usage\n/loop 28%\n\nmcp servers % of usage\nemacs-eval 34%\n"))
         (metrics (plist-get r :metrics)))
    (should (null (cdr (assq 'high-context metrics))))
    (should (equal 28 (cdr (assq 'loop metrics))))
    (should (equal 34 (cdr (assq 'emacs-eval metrics))))))

(ert-deftest anvil-dev-test-claude-limits-analyze-rejects-empty-report ()
  (should-error (anvil-claude-limits-analyze "") :type 'user-error))

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

(ert-deftest anvil-dev-test-audit-accepts-shipped-on-later-line ()
  "STATUS first line `~DRAFT~ → ~SHIPPED~' style passes — keyword
appears anywhere in the body, not only on the first line."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "docs/design/17-session.org" d)
           (concat
            "* STATUS\n"
            "~DRAFT~ (2026-04-19, extended 2026-04-20 with Phase 3-4) →\n"
            "~SHIPPED~ (2026-04-22, develop branch).\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :non-shipped-docs)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-accepts-deferred-as-non-blocker ()
  "DEFERRED specs are intentional non-blockers and must not appear
in the master-merge gate report."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "docs/design/30-acp.org" d)
           (concat
            "* STATUS\n"
            "~DRAFT~ (2026-04-21) → ~DEFERRED~ (2026-04-22 after research).\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :non-shipped-docs)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-accepts-audit-research-memo ()
  "AUDIT memos are research-only, treated as non-blockers."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "docs/design/32-rhblind.org" d)
           "* STATUS\n~AUDIT~ (2026-04-22) — research memo only.\n")
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :non-shipped-docs)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-rejects-lowercase-shipped-narrative ()
  "Lowercase `shipped' inside narrative prose must not pass the gate.
Only the all-caps status keyword counts."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "docs/design/20-pty2.org" d)
           (concat
            "* STATUS\n"
            "~DRAFT~ (2026-04-19)\n\n"
            "Extends the shipped Phase 1 broker with TUI semantics.\n"))
          (let* ((r (anvil-dev-release-audit d))
                 (docs (plist-get r :non-shipped-docs)))
            (should (= 1 (length docs)))
            (should (equal "20-pty2.org" (plist-get (car docs) :file)))))
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

;;;; --- plist-return scanner (v0.3.1-class regression guard) --------------

(ert-deftest anvil-dev-test-audit-flags-plist-return ()
  "A wrapper whose body ends with `(list :K ...)' is flagged."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-foo.el" d)
           (concat
            "(defun anvil-foo--tool-send (id text)\n"
            "  \"Send.\n\nMCP Parameters:\n  id - x\n  text - y\"\n"
            "  (foo id text)\n"
            "  (list :sent (length text)))\n"))
          (let* ((r (anvil-dev-release-audit d))
                 (hits (plist-get r :plist-return)))
            (should (= 1 (length hits)))
            (should (equal "anvil-foo--tool-send"
                           (plist-get (car hits) :defun)))
            (should-not (plist-get r :clean-p))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-allows-string-return ()
  "A wrapper whose body ends with a string is NOT flagged."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-foo.el" d)
           (concat
            "(defun anvil-foo--tool-echo (text)\n"
            "  \"Echo.\n\nMCP Parameters:\n  text - x\"\n"
            "  (format \"received: %s\" text))\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :plist-return)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-allows-json-encode-return ()
  "A wrapper ending in `(json-encode ...)' is NOT flagged."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-foo.el" d)
           (concat
            "(defun anvil-foo--tool-probe (id)\n"
            "  \"Probe.\n\nMCP Parameters:\n  id - x\"\n"
            "  (json-encode `((id . ,id) (ok . t))))\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :plist-return)))) )
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-respects-plist-return-exemption-marker ()
  "A file tagged `tools-wrapped-at-registration' is not scanned for plist return."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-foo.el" d)
           (concat
            ";;; anvil-foo.el --- test -*- lexical-binding: t; -*-\n"
            ";;; anvil-audit: tools-wrapped-at-registration\n\n"
            "(defun anvil-foo--tool-send (id text)\n"
            "  \"Send.\n\nMCP Parameters:\n  id - x\n  text - y\"\n"
            "  (list :sent (length text)))\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :plist-return)))) )
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-plist-return-ignores-non-terminal-list ()
  "A `(list :K ...)' form that is NOT the last expression is not flagged."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-foo.el" d)
           (concat
            "(defun anvil-foo--tool-submit (payload)\n"
            "  \"Submit.\n\nMCP Parameters:\n  payload - x\"\n"
            "  (let ((slim (list :payload payload :ok t)))\n"
            "    (foo slim)\n"
            "    (format \"submitted %S\" slim)))\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :plist-return)))) )
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-plist-return-ignores-nested-in-format ()
  "A `(list :K ...)' nested as an arg of `format' is not flagged
— the defun's terminal value is the `format' string, not the list."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-foo.el" d)
           (concat
            "(defun anvil-foo--tool-outline (path)\n"
            "  \"Outline.\n\nMCP Parameters:\n  path - x\"\n"
            "  (anvil-server-with-error-handling\n"
            "   (let ((items (foo path)))\n"
            "     (format \"%S\" (list :path path :items items)))))\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :plist-return)))) )
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-plist-return-catches-wrapped-let ()
  "A `(let (...) (list :K ...))' wrapped in `anvil-server-with-error-handling'
must still be flagged — sequencing forms are unwrapped before the
terminal check so the real plist return is visible."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-foo.el" d)
           (concat
            "(defun anvil-foo--tool-run (id)\n"
            "  \"Run.\n\nMCP Parameters:\n  id - x\"\n"
            "  (anvil-server-with-error-handling\n"
            "   (let* ((raw (foo id)))\n"
            "     (list :id id :raw raw))))\n"))
          (let* ((r (anvil-dev-release-audit d))
                 (hits (plist-get r :plist-return)))
            (should (= 1 (length hits)))
            (should (equal "anvil-foo--tool-run"
                           (plist-get (car hits) :defun)))
            (should-not (plist-get r :clean-p))) )
      (delete-directory d t))))

;;;; --- issue-fix-no-test scanner (37fcc52-class guard) ------------------

(defun anvil-dev-test--audit-make-git-repo ()
  "Return a fresh temp dir initialised as a git repo with an empty tests/ dir."
  (let ((d (make-temp-file "anvil-dev-audit-git-" t)))
    (make-directory (expand-file-name "docs/design" d) t)
    (make-directory (expand-file-name "tests" d) t)
    (let ((default-directory (file-name-as-directory d)))
      (call-process "git" nil nil nil "init" "-q" "-b" "master")
      (call-process "git" nil nil nil "config" "user.email" "t@example.com")
      (call-process "git" nil nil nil "config" "user.name"  "Tester")
      (call-process "git" nil nil nil "config" "commit.gpgsign" "false"))
    d))

(defun anvil-dev-test--audit-git-commit (d message &rest files)
  "Create FILES with `path→content' pairs inside D and commit with MESSAGE."
  (let ((default-directory (file-name-as-directory d)))
    (while files
      (let ((path (pop files))
            (content (pop files)))
        (let ((abs (expand-file-name path d)))
          (make-directory (file-name-directory abs) t)
          (with-temp-file abs (insert content)))))
    (call-process "git" nil nil nil "add" "-A")
    (call-process "git" nil nil nil "commit" "-q" "--allow-empty" "-m" message)))

(ert-deftest anvil-dev-test-audit-issue-fix-without-test-flags-hit ()
  "A commit `Fixes #N' that touches only source (not tests/) is flagged."
  (let ((d (anvil-dev-test--audit-make-git-repo)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-git-commit
           d "initial"
           "anvil-foo.el" "(provide 'anvil-foo)\n")
          (anvil-dev-test--audit-git-commit
           d "fix(foo): guard against NPE\n\nFixes #42"
           "anvil-foo.el" ";; edited\n(provide 'anvil-foo)\n")
          (let* ((r (anvil-dev-release-audit d))
                 (hits (plist-get r :issue-fix-no-test)))
            (should (= 1 (length hits)))
            (should (= 42 (plist-get (car hits) :issue)))
            (should-not (plist-get r :clean-p))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-issue-fix-with-test-is-clean ()
  "A commit `Fixes #N' that touches tests/ is NOT flagged."
  (let ((d (anvil-dev-test--audit-make-git-repo)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-git-commit
           d "initial" "anvil-foo.el" "(provide 'anvil-foo)\n")
          (anvil-dev-test--audit-git-commit
           d "fix(foo): guard against NPE\n\nFixes #42"
           "anvil-foo.el" ";; fix\n(provide 'anvil-foo)\n"
           "tests/anvil-foo-test.el"
           "(require 'ert)\n(ert-deftest anvil-foo-guards-npe () (should t))\n")
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :issue-fix-no-test)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-issue-fix-ignores-regular-commits ()
  "Commits without `Fixes|Closes|Resolves #N' are never flagged, even
if they don't touch tests."
  (let ((d (anvil-dev-test--audit-make-git-repo)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-git-commit
           d "initial" "anvil-foo.el" "(provide 'anvil-foo)\n")
          (anvil-dev-test--audit-git-commit
           d "feat(foo): a feature without tests, not an issue fix"
           "anvil-foo.el" ";; feature\n(provide 'anvil-foo)\n")
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :issue-fix-no-test)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-issue-fix-depth-0-disables-scan ()
  "Setting `anvil-dev-audit-issue-fix-commit-depth' to 0 turns the scan off."
  (let ((d (anvil-dev-test--audit-make-git-repo)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-git-commit
           d "initial" "anvil-foo.el" "(provide 'anvil-foo)\n")
          (anvil-dev-test--audit-git-commit
           d "fix(foo): Fixes #7"
           "anvil-foo.el" ";; fix\n(provide 'anvil-foo)\n")
          (let ((anvil-dev-audit-issue-fix-commit-depth 0))
            (let ((r (anvil-dev-release-audit d)))
              (should (null (plist-get r :issue-fix-no-test))))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-issue-fix-non-git-root-is-safe ()
  "A directory that is not a git worktree simply returns an empty list."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (let ((r (anvil-dev-release-audit d)))
          (should (null (plist-get r :issue-fix-no-test))))
      (delete-directory d t))))


;;;; --- Phase C: :unused-since scanner (Doc 34) ----------------------------

(require 'anvil-state)
(require 'anvil-discovery)

(ert-deftest anvil-dev-test-audit-unused-since-omitted-is-nil ()
  "Without :unused-since the Phase C scanner does not run."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (let ((r (anvil-dev-release-audit d)))
          (should (null (plist-get r :unused-tools)))
          (should (null (plist-get r :unused-since))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-unused-since-reports-stale-and-never ()
  "With :unused-since the scanner reports stale counters and
never-called tools against the current registry."
  (let ((d (anvil-dev-test--audit-make-root))
        (anvil-server--tools (make-hash-table :test #'equal))
        (anvil-state-db-path (make-temp-file "anvil-dev-audit-" nil ".db"))
        (anvil-state--db nil))
    (unwind-protect
        (progn
          (anvil-state-enable)
          (anvil-server-register-tool
           (lambda () "ok") :id "recent-tool"
           :description "r" :intent '(file-edit) :layer 'core)
          (anvil-server-register-tool
           (lambda () "ok") :id "stale-tool"
           :description "s" :intent '(file-edit) :layer 'core)
          (anvil-server-register-tool
           (lambda () "ok") :id "never-tool"
           :description "n" :intent '(file-edit) :layer 'core)
          (let ((now (truncate (float-time))))
            (anvil-state-set
             "recent-tool"
             (list :count 1 :last-called now :first-seen now
                   :server-id "default")
             :ns anvil-discovery--usage-ns)
            (anvil-state-set
             "stale-tool"
             (list :count 3
                   :last-called (- now (* 45 86400))
                   :first-seen (- now (* 45 86400))
                   :server-id "default")
             :ns anvil-discovery--usage-ns))
          (let* ((r (anvil-dev-release-audit d :unused-since 30))
                 (found (plist-get r :unused-tools))
                 (ids (mapcar (lambda (p) (plist-get p :id)) found))
                 (reasons (mapcar (lambda (p) (cons (plist-get p :id)
                                                   (plist-get p :reason)))
                                  found)))
            (should (equal 30 (plist-get r :unused-since)))
            (should-not (member "recent-tool" ids))
            (should (member "stale-tool" ids))
            (should (member "never-tool" ids))
            (should (eq 'stale (cdr (assoc "stale-tool" reasons))))
            (should (eq 'never-called (cdr (assoc "never-tool" reasons))))
            (should (plist-get r :clean-p))))
      (anvil-discovery-usage-clear)
      (anvil-state-disable)
      (ignore-errors (delete-file anvil-state-db-path))
      (delete-directory d t))))

;;;; --- secret-scan (gitleaks) --------------------------------------------

(ert-deftest anvil-dev-test-audit-secrets-nil-when-disabled ()
  "Scanner returns nil when `anvil-dev-audit-secrets-enabled' is nil,
without ever shelling out."
  (let ((d (anvil-dev-test--make-dir))
        (anvil-dev-audit-secrets-enabled nil))
    (unwind-protect
        (cl-letf (((symbol-function 'call-process)
                   (lambda (&rest _args)
                     (error "call-process must not be invoked when disabled"))))
          (should (null (anvil-dev--audit-scan-secrets d))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-secrets-nil-when-binary-missing ()
  "Scanner returns nil (no signal) when `gitleaks' is not on PATH."
  (let ((d (anvil-dev-test--make-dir)))
    (make-directory (expand-file-name ".git" d))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (prog) (cond ((equal prog "gitleaks") nil)
                                        ((equal prog "git") "/usr/bin/git")
                                        (t nil))))
                  ((symbol-function 'call-process)
                   (lambda (&rest _args)
                     (error "call-process must not be invoked without gitleaks"))))
          (should (null (anvil-dev--audit-scan-secrets d))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-secrets-nil-when-not-git ()
  "Scanner returns nil when ROOT is not a git worktree."
  (let ((d (anvil-dev-test--make-dir)))
    (unwind-protect
        (progn
          ;; no .git inside d
          (should (null (anvil-dev--audit-scan-secrets d))))
      (delete-directory d t))))

(defun anvil-dev-test--executable-find-stub (prog)
  "Return fake paths for `gitleaks' / `git', nil otherwise.
Used in cl-letf binding to avoid recursing into the real
`executable-find' during scanner tests."
  (cond ((equal prog "gitleaks") "/fake/gitleaks")
        ((equal prog "git")      "/usr/bin/git")
        (t nil)))

(ert-deftest anvil-dev-test-audit-secrets-parses-json-findings ()
  "Scanner parses gitleaks JSON report into finding plists."
  (let ((d (anvil-dev-test--make-dir)))
    (make-directory (expand-file-name ".git" d))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   #'anvil-dev-test--executable-find-stub)
                  ((symbol-function 'call-process)
                   (lambda (_prog _in _buf _disp &rest args)
                     (let ((rpath (cadr (member "--report-path" args))))
                       (when rpath
                         (with-temp-file rpath
                           (insert "[{\"RuleID\":\"github-pat\","
                                   "\"StartLine\":1,\"File\":\"s.txt\","
                                   "\"Match\":\"REDACTED\","
                                   "\"Fingerprint\":\"fp1\"}]"))))
                     0)))
          (let ((findings (anvil-dev--audit-scan-secrets d)))
            (should (= 1 (length findings)))
            (let ((f (car findings)))
              (should (equal "s.txt" (plist-get f :file)))
              (should (= 1 (plist-get f :line)))
              (should (equal "github-pat" (plist-get f :rule)))
              (should (equal "fp1" (plist-get f :fingerprint))))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-secrets-survives-malformed-json ()
  "Scanner degrades to nil on malformed JSON without signaling."
  (let ((d (anvil-dev-test--make-dir)))
    (make-directory (expand-file-name ".git" d))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   #'anvil-dev-test--executable-find-stub)
                  ((symbol-function 'call-process)
                   (lambda (_prog _in _buf _disp &rest args)
                     (let ((rpath (cadr (member "--report-path" args))))
                       (when rpath
                         (with-temp-file rpath
                           (insert "this is not json"))))
                     0)))
          (should (null (anvil-dev--audit-scan-secrets d))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-secrets-empty-report-returns-nil ()
  "Empty report file (no leaks) yields nil, not an error."
  (let ((d (anvil-dev-test--make-dir)))
    (make-directory (expand-file-name ".git" d))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   #'anvil-dev-test--executable-find-stub)
                  ((symbol-function 'call-process)
                   (lambda (_prog _in _buf _disp &rest args)
                     (let ((rpath (cadr (member "--report-path" args))))
                       (when rpath
                         (with-temp-file rpath (insert ""))))
                     0)))
          (should (null (anvil-dev--audit-scan-secrets d))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-clean-p-reflects-secrets ()
  "`:clean-p' becomes nil when the secret scan reports a finding."
  (let ((d (anvil-dev-test--make-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'anvil-dev--audit-scan-secrets)
                   (lambda (_root)
                     (list (list :file "x.el" :line 1
                                 :rule "github-pat" :fingerprint "fp1")))))
          (let ((r (anvil-dev-release-audit d)))
            (should (equal 1 (length (plist-get r :secrets))))
            (should-not (plist-get r :clean-p))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-audit-report-renders-secret-finding ()
  "`anvil-dev--audit-format-report' emits a FAIL block for secrets."
  (let* ((result (list :arglist-strip nil
                       :missing-params nil
                       :plist-return nil
                       :issue-fix-no-test nil
                       :non-shipped-docs nil
                       :secrets (list (list :file "s.txt" :line 3
                                            :rule "aws" :fingerprint "fp9"))
                       :unused-tools nil
                       :clean-p nil
                       :root "/tmp/x/"
                       :scope nil :unused-since nil
                       :audited-at "2026-01-01 00:00:00"))
         (text (anvil-dev--audit-format-report result)))
    (should (string-match-p "FAIL gitleaks detected secrets" text))
    (should (string-match-p "s.txt:3" text))
    (should (string-match-p "aws" text))
    (should (string-match-p "fp9" text))))

;;;; --- T92 fixture realism scanner ---------------------------------------
;;
;; T70 cross-cutting #3 follow-up.  These tests exercise the
;; production-shape-vs-test-shape mismatch detector that catches the
;; class of false PASS where a `let' binds a hash-table-shaped global
;; (e.g. `anvil-orchestrator--providers') to a quoted alist literal.

(ert-deftest anvil-dev-test-realism-classify-init-shapes ()
  "`anvil-dev--audit-realism-classify-init' recognises tracked init forms."
  (should (eq :hash-table
              (anvil-dev--audit-realism-classify-init
               '(make-hash-table :test 'eq))))
  (should (eq :vector
              (anvil-dev--audit-realism-classify-init
               '(make-vector 8 nil))))
  (should (eq :vector
              (anvil-dev--audit-realism-classify-init
               '(vector 1 2 3))))
  (should (eq :vector
              (anvil-dev--audit-realism-classify-init [1 2 3])))
  (should (null (anvil-dev--audit-realism-classify-init '(list 1 2 3))))
  (should (null (anvil-dev--audit-realism-classify-init nil)))
  (should (null (anvil-dev--audit-realism-classify-init "string"))))

(ert-deftest anvil-dev-test-realism-classify-value-shapes ()
  "`anvil-dev--audit-realism-classify-value' classifies the high-precision set."
  (should (eq :alist
              (anvil-dev--audit-realism-classify-value
               '(quote ((a . 1) (b . 2))))))
  (should (eq :alist
              ;; `(let ((x '((a) (b)))))' — alist of cons-headed lists.
              (anvil-dev--audit-realism-classify-value
               '(quote ((a) (b))))))
  (should (eq :hash-table
              (anvil-dev--audit-realism-classify-value
               '(make-hash-table :test 'eq))))
  (should (eq :vector
              (anvil-dev--audit-realism-classify-value
               '(make-vector 4 nil))))
  (should (eq :vector
              (anvil-dev--audit-realism-classify-value [1 2 3])))
  (should (eq :alist-runtime
              (anvil-dev--audit-realism-classify-value
               '(list (cons 'a 1) (cons 'b 2)))))
  (should (eq :nil (anvil-dev--audit-realism-classify-value nil)))
  (should (eq :nil (anvil-dev--audit-realism-classify-value '(quote nil))))
  ;; Function-call delegate (the post-T70-fix pattern) — UNKNOWN, no flag.
  (should (eq :unknown
              (anvil-dev--audit-realism-classify-value
               '(anvil-orchestrator-routing-test--make-providers
                 '(claude codex)))))
  ;; A flat quoted list of symbols is NOT an alist — must be :unknown.
  (should (eq :unknown
              (anvil-dev--audit-realism-classify-value
               '(quote (a b c))))))

(ert-deftest anvil-dev-test-realism-shapes-conflict-matrix ()
  "Conflict matrix: hash-table↔alist & vector↔alist flag, same shapes do not."
  (should (anvil-dev--audit-realism-shapes-conflict-p
           :hash-table :alist))
  (should (anvil-dev--audit-realism-shapes-conflict-p
           :hash-table :alist-runtime))
  (should (anvil-dev--audit-realism-shapes-conflict-p
           :hash-table :vector))
  (should (anvil-dev--audit-realism-shapes-conflict-p
           :vector :alist))
  (should (anvil-dev--audit-realism-shapes-conflict-p
           :vector :hash-table))
  ;; Same shape — no conflict.
  (should-not (anvil-dev--audit-realism-shapes-conflict-p
               :hash-table :hash-table))
  (should-not (anvil-dev--audit-realism-shapes-conflict-p
               :vector :vector))
  ;; Unknown / nil — never flag (precision over recall).
  (should-not (anvil-dev--audit-realism-shapes-conflict-p
               :hash-table :unknown))
  (should-not (anvil-dev--audit-realism-shapes-conflict-p
               :hash-table :nil))
  (should-not (anvil-dev--audit-realism-shapes-conflict-p
               :vector :unknown)))

(ert-deftest anvil-dev-test-realism-detects-alist-vs-hash ()
  "T70 reproducer: production = (make-hash-table), test = quoted alist."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          ;; Production module declares a hash-table-shaped registry.
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-orchestrator.el" d)
           (concat
            "(defvar anvil-orchestrator--providers"
            " (make-hash-table :test 'eq)"
            "  \"Provider registry.\")\n"))
          (make-directory (expand-file-name "tests" d) t)
          ;; Test let-binds the same var to a quoted alist — the bug.
          (anvil-dev-test--audit-write
           (expand-file-name "tests/anvil-orchestrator-test.el" d)
           (concat
            "(ert-deftest x ()\n"
            "  (let ((anvil-orchestrator--providers"
            "         '((claude) (codex) (ollama))))\n"
            "    (should t)))\n"))
          (let* ((r (anvil-dev-release-audit d))
                 (hits (plist-get r :fixture-realism)))
            (should (= 1 (length hits)))
            (should (equal "anvil-orchestrator-test.el"
                           (plist-get (car hits) :file)))
            (should (eq 'anvil-orchestrator--providers
                        (plist-get (car hits) :var)))
            (should (eq :hash-table (plist-get (car hits) :declared)))
            (should (eq :alist      (plist-get (car hits) :bound)))
            (should-not (plist-get r :clean-p))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-realism-skips-real-hash-fixture ()
  "Test that builds the fixture via a helper-call (post-T70 pattern) is clean."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-orchestrator.el" d)
           (concat
            "(defvar anvil-orchestrator--providers"
            " (make-hash-table :test 'eq)"
            "  \"Registry.\")\n"))
          (make-directory (expand-file-name "tests" d) t)
          (anvil-dev-test--audit-write
           (expand-file-name "tests/anvil-orchestrator-test.el" d)
           (concat
            "(ert-deftest x ()\n"
            "  (let ((anvil-orchestrator--providers"
            "         (anvil-orchestrator-test--make-providers"
            "          '(claude codex))))\n"
            "    (should t)))\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :fixture-realism)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-realism-skips-matching-hash-fixture ()
  "Same shape on both sides — `(make-hash-table)' bind is clean."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-x.el" d)
           "(defvar anvil-x--reg (make-hash-table :test 'equal) \"reg\")\n")
          (make-directory (expand-file-name "tests" d) t)
          (anvil-dev-test--audit-write
           (expand-file-name "tests/anvil-x-test.el" d)
           (concat
            "(ert-deftest y ()\n"
            "  (let ((anvil-x--reg (make-hash-table :test 'equal)))\n"
            "    (should t)))\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :fixture-realism)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-realism-detects-alist-vs-vector ()
  "Production = `(make-vector ...)', test = quoted alist — flagged."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-vec.el" d)
           "(defvar anvil-vec--slots (make-vector 8 nil) \"slots\")\n")
          (make-directory (expand-file-name "tests" d) t)
          (anvil-dev-test--audit-write
           (expand-file-name "tests/anvil-vec-test.el" d)
           (concat
            "(ert-deftest z ()\n"
            "  (let ((anvil-vec--slots '((a . 1) (b . 2))))\n"
            "    (should t)))\n"))
          (let* ((r (anvil-dev-release-audit d))
                 (hits (plist-get r :fixture-realism)))
            (should (= 1 (length hits)))
            (should (eq :vector (plist-get (car hits) :declared)))
            (should (eq :alist  (plist-get (car hits) :bound)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-realism-handles-cl-letf ()
  "`cl-letf' / `cl-letf*' bindings of a tracked var are inspected too."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-orchestrator.el" d)
           "(defvar anvil-orchestrator--providers (make-hash-table :test 'eq))\n")
          (make-directory (expand-file-name "tests" d) t)
          (anvil-dev-test--audit-write
           (expand-file-name "tests/anvil-orchestrator-test.el" d)
           (concat
            "(ert-deftest q ()\n"
            "  (cl-letf* ((anvil-orchestrator--providers"
            "              '((claude) (codex))))\n"
            "    (should t)))\n"))
          (let* ((r (anvil-dev-release-audit d))
                 (hits (plist-get r :fixture-realism)))
            (should (= 1 (length hits)))
            (should (eq 'anvil-orchestrator--providers
                        (plist-get (car hits) :var)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-realism-ignores-unknown-vars ()
  "A let-bind of a symbol that is NOT in the production shape map is ignored."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          ;; No production defvar — shape map stays empty.
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-x.el" d)
           "(defun anvil-x-fn () nil)\n")
          (make-directory (expand-file-name "tests" d) t)
          (anvil-dev-test--audit-write
           (expand-file-name "tests/anvil-x-test.el" d)
           (concat
            "(ert-deftest q ()\n"
            "  (let ((some-random-var '((a . 1))))\n"
            "    (should t)))\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :fixture-realism)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-realism-detects-runtime-alist-list-cons ()
  "`(list (cons ...) ...)' against a hash-table production is flagged."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-y.el" d)
           "(defvar anvil-y--reg (make-hash-table :test 'eq))\n")
          (make-directory (expand-file-name "tests" d) t)
          (anvil-dev-test--audit-write
           (expand-file-name "tests/anvil-y-test.el" d)
           (concat
            "(ert-deftest q ()\n"
            "  (let ((anvil-y--reg"
            "         (list (cons 'a 1) (cons 'b 2))))\n"
            "    (should t)))\n"))
          (let* ((r (anvil-dev-release-audit d))
                 (hits (plist-get r :fixture-realism)))
            (should (= 1 (length hits)))
            (should (eq :alist-runtime (plist-get (car hits) :bound)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-realism-defconst-also-tracked ()
  "`defconst' init forms contribute to the production shape map too."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-z.el" d)
           "(defconst anvil-z--lookup (make-hash-table :test 'eq))\n")
          (make-directory (expand-file-name "tests" d) t)
          (anvil-dev-test--audit-write
           (expand-file-name "tests/anvil-z-test.el" d)
           (concat
            "(ert-deftest q ()\n"
            "  (let ((anvil-z--lookup '((x . 1))))\n"
            "    (should t)))\n"))
          (let* ((r (anvil-dev-release-audit d))
                 (hits (plist-get r :fixture-realism)))
            (should (= 1 (length hits)))
            (should (eq :hash-table (plist-get (car hits) :declared)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-realism-empty-tests-dir-clean ()
  "Repo with no tests/ directory: scanner returns nil cleanly."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-x.el" d)
           "(defvar anvil-x--reg (make-hash-table))\n")
          ;; Note: no tests/ subdir.
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :fixture-realism)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-realism-report-renders-finding ()
  "`anvil-dev--audit-format-report' emits a FAIL block for fixture-realism."
  (let* ((result
          (list :arglist-strip nil
                :missing-params nil
                :plist-return nil
                :issue-fix-no-test nil
                :non-shipped-docs nil
                :secrets nil
                :fixture-realism
                (list (list :file "anvil-orchestrator-test.el"
                            :var 'anvil-orchestrator--providers
                            :declared :hash-table
                            :bound :alist))
                :unused-tools nil
                :clean-p nil
                :root "/tmp/x/"
                :scope nil :unused-since nil
                :audited-at "2026-01-01 00:00:00"))
         (text (anvil-dev--audit-format-report result)))
    (should (string-match-p "FAIL fixture realism" text))
    (should (string-match-p "anvil-orchestrator-test\\.el" text))
    (should (string-match-p "anvil-orchestrator--providers" text))
    (should (string-match-p "declared=:hash-table" text))
    (should (string-match-p "bound=:alist" text))))

(ert-deftest anvil-dev-test-realism-respects-file-exempt-marker ()
  "A test file carrying the `fixture-realism-exempt' marker is skipped."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-orchestrator.el" d)
           "(defvar anvil-orchestrator--providers (make-hash-table :test 'eq))\n")
          (make-directory (expand-file-name "tests" d) t)
          ;; Same shape mismatch as the canonical T70 reproducer, but
          ;; this file declares the exemption marker in its header.
          (anvil-dev-test--audit-write
           (expand-file-name "tests/anvil-orchestrator-test.el" d)
           (concat
            ";;; anvil-orchestrator-test.el --- compat -*- lexical-binding: t; -*-\n"
            ";;; anvil-audit: fixture-realism-exempt\n"
            "(ert-deftest x ()\n"
            "  (let ((anvil-orchestrator--providers"
            "         '((claude) (codex))))\n"
            "    (should t)))\n"))
          (let ((r (anvil-dev-release-audit d)))
            (should (null (plist-get r :fixture-realism)))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-realism-clean-p-gates-on-realism ()
  "`:clean-p' becomes nil when only the realism scanner has a hit."
  (let ((d (anvil-dev-test--audit-make-root)))
    (unwind-protect
        (progn
          (anvil-dev-test--audit-write
           (expand-file-name "anvil-x.el" d)
           "(defvar anvil-x--reg (make-hash-table :test 'eq))\n")
          (make-directory (expand-file-name "tests" d) t)
          (anvil-dev-test--audit-write
           (expand-file-name "tests/anvil-x-test.el" d)
           (concat
            "(ert-deftest q ()\n"
            "  (let ((anvil-x--reg '((a) (b))))\n"
            "    (should t)))\n"))
          ;; No design docs, no source defun hits, no commits; only
          ;; realism finding should drive `:clean-p' to nil.
          (let ((r (anvil-dev-release-audit d)))
            (should (plist-get r :fixture-realism))
            (should-not (plist-get r :clean-p))))
      (delete-directory d t))))

;;; anvil-dev-test.el ends here
