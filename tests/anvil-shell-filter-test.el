;;; anvil-shell-filter-test.el --- Tests for anvil-shell-filter -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for Doc 27 Phase 1 — per-command shell output
;; compression + tee + gain statistics.  Each filter is a pure
;; string-to-string function so the unit tests feed canned raw
;; output and assert a shape-locked compressed form.
;;
;; Tests that need the `anvil-state' KV (tee round-trip, gain
;; accumulation) isolate themselves with a temp DB via
;; `anvil-shell-filter-test--with-state' so the real DB is never
;; touched.
;;
;; Every test gates itself with `skip-unless' on
;; `anvil-shell-filter-supported' membership so the TDD-lock commit
;; can land ahead of the impl without red CI.

;;; Code:

(require 'ert)
(require 'cl-lib)
;; Non-fatal: the TDD-lock commit lands before the module, so
;; `anvil-shell-filter' is intentionally absent on first load.
;; `skip-unless' below gates every real assertion on the
;; `anvil-shell-filter-supported' capability list and turns red
;; only once the impl declares it has landed.
(require 'anvil-shell-filter nil t)
(require 'anvil-state)


;;;; --- fixture helpers ----------------------------------------------------

(defmacro anvil-shell-filter-test--with-state (&rest body)
  "Run BODY with a fresh temp `anvil-state' DB."
  (declare (indent 0))
  `(let ((anvil-state-db-path (make-temp-file "anvil-shf-" nil ".db"))
         (anvil-state--db nil))
     (unwind-protect
         (progn (anvil-state-enable) ,@body)
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path)))))

(defun anvil-shell-filter-test--supported-p (tag)
  "Return non-nil when TAG is in `anvil-shell-filter-supported'."
  (and (boundp 'anvil-shell-filter-supported)
       (memq tag anvil-shell-filter-supported)))


;;;; --- per-command filter snapshots ---------------------------------------

(ert-deftest anvil-shell-filter-test/git-status-porcelain-summary ()
  "`git status --short --branch' output compresses to one-line summary."
  (skip-unless (anvil-shell-filter-test--supported-p 'git-status))
  (let* ((raw (concat
               "## main...origin/main [ahead 2]\n"
               " M anvil-file.el\n"
               " M anvil-git.el\n"
               "?? .worktrees/\n"
               "?? notes.org\n"
               "?? scratch.el\n"
               "A  anvil-shell-filter.el\n"
               "D  obsolete.el\n"))
         (out (anvil-shell-filter-apply 'git-status raw)))
    (should (stringp out))
    (should (string-match-p "\\`branch:main" out))
    (should (string-match-p "\\+2" out))
    (should (string-match-p "M:2" out))
    (should (string-match-p "\\?\\?:3" out))
    (should (string-match-p "A:1" out))
    (should (string-match-p "D:1" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/git-status-detached-head ()
  "Detached-HEAD `## HEAD (no branch)' line still parses."
  (skip-unless (anvil-shell-filter-test--supported-p 'git-status))
  (let* ((raw "## HEAD (no branch)\n M foo.el\n")
         (out (anvil-shell-filter-apply 'git-status raw)))
    (should (string-match-p "\\`branch:" out))
    (should (string-match-p "M:1" out))))

(ert-deftest anvil-shell-filter-test/git-log-oneline ()
  "Verbose `git log' reduces to `hash subject' per commit."
  (skip-unless (anvil-shell-filter-test--supported-p 'git-log))
  (let* ((raw (concat
               "commit deadbeefcafebabe1234567890abcdef12345678\n"
               "Author: Zaw <z@example.com>\n"
               "Date:   Wed Apr 22 11:00:00 2026 +0900\n"
               "\n"
               "    feat(27): shell filter skeleton\n"
               "\n"
               "commit feedfacec001d00d9876543210abcdef87654321\n"
               "Author: Zaw <z@example.com>\n"
               "Date:   Wed Apr 22 10:30:00 2026 +0900\n"
               "\n"
               "    test(27): shape-lock filters\n"))
         (out (anvil-shell-filter-apply 'git-log raw)))
    (should (stringp out))
    (let ((lines (split-string out "\n" t)))
      (should (= 2 (length lines)))
      (should (string-match-p "^deadbee[a-f0-9]* feat(27): shell filter skeleton" (nth 0 lines)))
      (should (string-match-p "^feedfac[a-f0-9]* test(27): shape-lock filters" (nth 1 lines))))))

(ert-deftest anvil-shell-filter-test/git-diff-hunk-cap ()
  "Long unified diff keeps ≤3 hunks + omitted-hunks footer."
  (skip-unless (anvil-shell-filter-test--supported-p 'git-diff))
  (let* ((raw (mapconcat
               #'identity
               (append
                (list "diff --git a/foo.el b/foo.el"
                      "--- a/foo.el"
                      "+++ b/foo.el")
                (cl-loop for i from 1 to 6
                         append (list (format "@@ -%d,3 +%d,3 @@" i i)
                                      " context-before"
                                      "-old-line"
                                      "+new-line"
                                      " context-after")))
               "\n"))
         (out (anvil-shell-filter-apply 'git-diff raw)))
    (should (stringp out))
    ;; hunk-header count must be 3
    (let ((hunks 0)
          (start 0))
      (while (string-match "^@@ " out start)
        (setq hunks (1+ hunks)
              start (match-end 0)))
      (should (= 3 hunks)))
    (should (string-match-p "3 more hunks omitted" out))))

(ert-deftest anvil-shell-filter-test/git-diff-short-passthrough ()
  "Diff with ≤3 hunks is returned without omitted-hunks footer."
  (skip-unless (anvil-shell-filter-test--supported-p 'git-diff))
  (let* ((raw (concat "diff --git a/foo.el b/foo.el\n"
                      "@@ -1,1 +1,1 @@\n"
                      "-a\n"
                      "+b\n"))
         (out (anvil-shell-filter-apply 'git-diff raw)))
    (should (stringp out))
    (should-not (string-match-p "hunks omitted" out))))

(ert-deftest anvil-shell-filter-test/rg-grouped-by-file ()
  "rg output groups by file, caps at 3 matches, appends `(N more)'."
  (skip-unless (anvil-shell-filter-test--supported-p 'rg))
  (let* ((raw (concat
               "src/foo.el:10:  (foo 1)\n"
               "src/foo.el:20:  (foo 2)\n"
               "src/foo.el:30:  (foo 3)\n"
               "src/foo.el:40:  (foo 4)\n"
               "src/foo.el:50:  (foo 5)\n"
               "src/bar.el:5:   (foo 6)\n"))
         (out (anvil-shell-filter-apply 'rg raw)))
    (should (stringp out))
    (should (string-match-p "src/foo.el" out))
    (should (string-match-p "src/bar.el" out))
    (should (string-match-p "2 more" out))))

(ert-deftest anvil-shell-filter-test/find-grouped-by-dir ()
  "find output groups by directory with a count suffix."
  (skip-unless (anvil-shell-filter-test--supported-p 'find))
  (let* ((raw (concat
               "./src/a.el\n./src/b.el\n./src/c.el\n./src/d.el\n./src/e.el\n"
               "./tests/a-test.el\n./tests/b-test.el\n"))
         (out (anvil-shell-filter-apply 'find raw)))
    (should (stringp out))
    (should (string-match-p "./src/" out))
    (should (string-match-p "./tests/" out))
    (should (string-match-p "5" out))
    (should (string-match-p "2" out))))

(ert-deftest anvil-shell-filter-test/ls-counts ()
  "ls output collapses to counts when entries > threshold."
  (skip-unless (anvil-shell-filter-test--supported-p 'ls))
  (let* ((raw (concat
               (mapconcat (lambda (i) (format "file%02d.el" i))
                          (number-sequence 1 15) "\n")
               "\n"))
         (out (anvil-shell-filter-apply 'ls raw)))
    (should (stringp out))
    (should (string-match-p "15" out))))

(ert-deftest anvil-shell-filter-test/pytest-pass-count-only ()
  "Pytest passing run keeps summary line, drops verbose per-test output."
  (skip-unless (anvil-shell-filter-test--supported-p 'pytest))
  (let* ((raw (concat
               "============================= test session starts ==============================\n"
               "collected 42 items\n"
               "tests/test_a.py ..........                                                  [ 24%]\n"
               "tests/test_b.py ................                                            [ 62%]\n"
               "tests/test_c.py ................                                            [100%]\n"
               "\n"
               "============================== 42 passed in 1.23s ==============================\n"))
         (out (anvil-shell-filter-apply 'pytest raw)))
    (should (stringp out))
    (should (string-match-p "42 passed" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/pytest-fail-keeps-traceback ()
  "Pytest failing run preserves the FAILED block + traceback."
  (skip-unless (anvil-shell-filter-test--supported-p 'pytest))
  (let* ((raw (concat
               "tests/test_a.py::test_ok PASSED\n"
               "tests/test_b.py::test_bad FAILED\n"
               "=================================== FAILURES ===================================\n"
               "________________________________ test_bad _____________________________________\n"
               "    def test_bad():\n"
               ">       assert 1 == 2\n"
               "E       assert 1 == 2\n"
               "tests/test_b.py:3: AssertionError\n"
               "========================= 1 failed, 1 passed in 0.01s =========================\n"))
         (out (anvil-shell-filter-apply 'pytest raw)))
    (should (string-match-p "FAILED" out))
    (should (string-match-p "test_bad" out))
    (should (string-match-p "AssertionError" out))))

(ert-deftest anvil-shell-filter-test/ert-batch-pass-count ()
  "ERT batch pass-only run reduces to summary."
  (skip-unless (anvil-shell-filter-test--supported-p 'ert-batch))
  (let* ((raw (concat
               "Running 10 tests (2026-04-22 12:00:00, selector `t')\n"
               "   passed  1/10  anvil-shell-filter-test/git-status\n"
               "   passed  2/10  anvil-shell-filter-test/git-log\n"
               "   passed  3/10  anvil-shell-filter-test/git-diff\n"
               "   passed  4/10  anvil-shell-filter-test/rg\n"
               "   passed  5/10  anvil-shell-filter-test/find\n"
               "   passed  6/10  anvil-shell-filter-test/ls\n"
               "   passed  7/10  anvil-shell-filter-test/pytest\n"
               "   passed  8/10  anvil-shell-filter-test/emacs-batch\n"
               "   passed  9/10  anvil-shell-filter-test/make\n"
               "   passed 10/10  anvil-shell-filter-test/ert-batch\n"
               "\n"
               "Ran 10 tests, 10 results as expected, 0 unexpected (0.123456 sec)\n"))
         (out (anvil-shell-filter-apply 'ert-batch raw)))
    (should (string-match-p "Ran 10 tests" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/ert-batch-fail-keeps-detail ()
  "ERT batch with failures keeps the FAILED blocks."
  (skip-unless (anvil-shell-filter-test--supported-p 'ert-batch))
  (let* ((raw (concat
               "Running 2 tests\n"
               "   passed  1/2  t-ok\n"
               "   FAILED  2/2  t-bad\n"
               "Test t-bad condition:\n"
               "    (ert-test-failed\n"
               "     ((should (equal 1 2)) :form (equal 1 2) :value nil))\n"
               "\n"
               "Ran 2 tests, 1 results as expected, 1 unexpected\n"))
         (out (anvil-shell-filter-apply 'ert-batch raw)))
    (should (string-match-p "FAILED" out))
    (should (string-match-p "t-bad" out))
    (should (string-match-p "ert-test-failed" out))))

(ert-deftest anvil-shell-filter-test/emacs-batch-compile-squash ()
  "emacs --batch byte-compile output squashes `Compiling ...done' lines."
  (skip-unless (anvil-shell-filter-test--supported-p 'emacs-batch))
  (let* ((raw (concat
               "Compiling /tmp/a.el...\n"
               "Compiling /tmp/a.el...done\n"
               "Compiling /tmp/b.el...\n"
               "Compiling /tmp/b.el...done\n"
               "Compiling /tmp/c.el...\n"
               "Warning: foo bar\n"
               "Compiling /tmp/c.el...done\n"
               "gcs-done\n"))
         (out (anvil-shell-filter-apply 'emacs-batch raw)))
    (should (string-match-p "Warning:" out))
    (should-not (string-match-p "gcs-done" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/make-keeps-errors ()
  "make output keeps error/warning lines and the final summary."
  (skip-unless (anvil-shell-filter-test--supported-p 'make))
  (let* ((raw (concat
               "cc -c foo.c\n"
               "foo.c:10:5: warning: unused variable\n"
               "cc -c bar.c\n"
               "cc -o prog foo.o bar.o\n"
               "make: *** [Makefile:15: prog] Error 1\n"))
         (out (anvil-shell-filter-apply 'make raw)))
    (should (string-match-p "warning" out))
    (should (string-match-p "Error 1" out))))


;;;; --- dispatch / lookup --------------------------------------------------

(ert-deftest anvil-shell-filter-test/lookup-by-first-token ()
  "`anvil-shell-filter-lookup' maps a command string to a handler tag."
  (skip-unless (anvil-shell-filter-test--supported-p 'dispatch))
  (should (eq 'git-status (anvil-shell-filter-lookup "git status --short")))
  (should (eq 'git-log    (anvil-shell-filter-lookup "git log --oneline -5")))
  (should (eq 'git-diff   (anvil-shell-filter-lookup "git diff HEAD~1")))
  (should (eq 'rg         (anvil-shell-filter-lookup "rg --no-heading foo")))
  (should (null (anvil-shell-filter-lookup "totally-unknown-cmd"))))

(ert-deftest anvil-shell-filter-test/apply-nil-filter-passthrough ()
  "A nil filter tag returns RAW unchanged."
  (skip-unless (anvil-shell-filter-test--supported-p 'dispatch))
  (let ((raw "random text\nsecond line\n"))
    (should (equal raw (anvil-shell-filter-apply nil raw)))))


;;;; --- tee round-trip + gain stats ---------------------------------------

(ert-deftest anvil-shell-filter-test/tee-roundtrip ()
  "`anvil-shell-filter--tee-put' stores raw; `tee-get' retrieves it."
  (skip-unless (anvil-shell-filter-test--supported-p 'tee))
  (anvil-shell-filter-test--with-state
    (let* ((raw "raw bytes\nline 2\n")
           (id (anvil-shell-filter--tee-put raw)))
      (should (stringp id))
      (should (equal raw (anvil-shell-filter-tee-get id))))))

(ert-deftest anvil-shell-filter-test/tee-get-missing-returns-nil ()
  "Unknown tee-id returns nil, never errors."
  (skip-unless (anvil-shell-filter-test--supported-p 'tee))
  (anvil-shell-filter-test--with-state
    (should (null (anvil-shell-filter-tee-get "no-such-id")))))

(ert-deftest anvil-shell-filter-test/gain-accumulates ()
  "`anvil-shell-filter--gain-record' accumulates savings; `gain' summarises."
  (skip-unless (anvil-shell-filter-test--supported-p 'gain))
  (anvil-shell-filter-test--with-state
    (anvil-shell-filter--gain-record 'git-status 3000 600)
    (anvil-shell-filter--gain-record 'git-diff   10000 2500)
    (let ((summary (anvil-shell-filter-gain)))
      (should (plist-get summary :entries))
      (should (>= (plist-get summary :entries) 2))
      (should (= 13000 (plist-get summary :raw-total)))
      (should (= 3100  (plist-get summary :compressed-total)))
      (should (= 9900  (plist-get summary :saved-total))))))


;;;; --- Phase 2a filter snapshots (10 additional handlers) ----------------

(ert-deftest anvil-shell-filter-test/gh-pr-list-oneline ()
  "`gh pr list' output reduces to `#NUM TITLE' one-liners."
  (skip-unless (anvil-shell-filter-test--supported-p 'gh))
  (let* ((raw (concat
               "Showing 3 of 3 open pull requests in owner/repo\n"
               "\n"
               "ID   TITLE                           BRANCH             CREATED AT\n"
               "#42  feat: shell filter              feat/shf           about 1 hour ago\n"
               "#41  fix: cache expiry               fix/cache          about 2 hours ago\n"
               "#40  docs: update README             docs/readme        about 3 hours ago\n"))
         (out (anvil-shell-filter-apply 'gh raw)))
    (should (stringp out))
    (should (string-match-p "#42 feat: shell filter" out))
    (should (string-match-p "#41 fix: cache expiry" out))
    (should (string-match-p "#40 docs: update README" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/git-log-graph-preserves-tree-chars ()
  "`git log --graph' graph characters survive the graph filter."
  (skip-unless (anvil-shell-filter-test--supported-p 'git-log-graph))
  (let* ((raw (concat
               "* commit deadbeefcafebabe1234567890abcdef12345678\n"
               "|\\  Merge: feedface deadbeef\n"
               "| | Author: Zaw <z@example.com>\n"
               "| | Date:   Wed Apr 22 11:00:00 2026 +0900\n"
               "| |\n"
               "| |     feat: shell filter skeleton\n"
               "| |\n"
               "| * commit feedfacec001d00d9876543210abcdef87654321\n"
               "|/  Author: Zaw <z@example.com>\n"
               "|   Date:   Wed Apr 22 10:30:00 2026 +0900\n"
               "|\n"
               "|       test: shape-lock filters\n"))
         (out (anvil-shell-filter-apply 'git-log-graph raw)))
    (should (stringp out))
    ;; At least one graph char must survive
    (should (string-match-p "[*|/\\\\]" out))
    (should (string-match-p "deadbee" out))
    (should (string-match-p "shell filter skeleton" out))))

(ert-deftest anvil-shell-filter-test/pip-install-keeps-installed-and-errors ()
  "`pip install' filter drops Collecting/Downloading lines, keeps results."
  (skip-unless (anvil-shell-filter-test--supported-p 'pip-install))
  (let* ((raw (concat
               "Collecting numpy\n"
               "  Downloading numpy-1.26.4-cp311.whl (18.3 MB)\n"
               "     |████████████████████████████████| 18.3 MB 12.3 MB/s\n"
               "Using cached numpy-1.26.4-cp311.whl (18.3 MB)\n"
               "Installing collected packages: numpy\n"
               "Successfully installed numpy-1.26.4\n"
               "WARNING: You are using pip version 22.0\n"))
         (out (anvil-shell-filter-apply 'pip-install raw)))
    (should (string-match-p "Successfully installed numpy-1.26.4" out))
    (should (string-match-p "WARNING" out))
    (should-not (string-match-p "Downloading" out))
    (should-not (string-match-p "Using cached" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/npm-install-summary-and-errors ()
  "`npm install' filter keeps `added N' summary and `npm ERR'/`npm WARN'."
  (skip-unless (anvil-shell-filter-test--supported-p 'npm-install))
  (let* ((raw (concat
               "npm notice Beginning install\n"
               "npm http fetch GET 200 https://registry.npmjs.org/react\n"
               "npm http fetch GET 200 https://registry.npmjs.org/react-dom\n"
               "npm WARN deprecated coffee-script@1.12.7\n"
               "\n"
               "added 1234 packages, and audited 1235 packages in 12s\n"
               "\n"
               "42 packages are looking for funding\n"
               "\n"
               "5 vulnerabilities (2 moderate, 3 high)\n"))
         (out (anvil-shell-filter-apply 'npm-install raw)))
    (should (string-match-p "added 1234 packages" out))
    (should (string-match-p "npm WARN" out))
    (should (string-match-p "vulnerabilities" out))
    (should-not (string-match-p "npm http fetch" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/docker-ps-summary-when-many ()
  "`docker ps' collapses to a row count when rows exceed the threshold."
  (skip-unless (anvil-shell-filter-test--supported-p 'docker-ps))
  (let* ((raw (concat
               "CONTAINER ID   IMAGE     COMMAND   CREATED   STATUS    PORTS   NAMES\n"
               (mapconcat (lambda (i)
                            (format "abc%03d       alpine    sh        1 min     Up 1 min          cont%d"
                                    i i))
                          (number-sequence 1 15) "\n")
               "\n"))
         (out (anvil-shell-filter-apply 'docker-ps raw)))
    (should (stringp out))
    (should (string-match-p "15" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/docker-logs-dedup ()
  "`docker logs' collapses consecutive duplicate lines with `(xN)'."
  (skip-unless (anvil-shell-filter-test--supported-p 'docker-logs))
  (let* ((raw (concat
               "2026-04-22T10:00:00 GET /api/health 200\n"
               "2026-04-22T10:00:01 GET /api/health 200\n"
               "2026-04-22T10:00:02 GET /api/health 200\n"
               "2026-04-22T10:00:03 POST /api/login 401\n"
               "2026-04-22T10:00:04 GET /api/health 200\n"))
         (out (anvil-shell-filter-apply 'docker-logs raw)))
    ;; 3× consecutive health-check must collapse
    (should (string-match-p "(x3)\\|×3\\|x3" out))
    (should (string-match-p "POST /api/login 401" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/kubectl-get-summary-when-many ()
  "`kubectl get pods' collapses to header + count when rows exceed threshold."
  (skip-unless (anvil-shell-filter-test--supported-p 'kubectl-get))
  (let* ((raw (concat
               "NAME          READY   STATUS    RESTARTS   AGE\n"
               (mapconcat (lambda (i)
                            (format "pod-%02d       1/1     Running   0          1d" i))
                          (number-sequence 1 15) "\n")
               "\n"))
         (out (anvil-shell-filter-apply 'kubectl-get raw)))
    (should (string-match-p "NAME\\s-+READY" out))
    (should (string-match-p "15" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/aws-s3-ls-summary-when-many ()
  "`aws s3 ls' collapses to count when entries exceed threshold."
  (skip-unless (anvil-shell-filter-test--supported-p 'aws-s3-ls))
  (let* ((raw (concat
               (mapconcat
                (lambda (i)
                  (format "2026-04-22 10:00:00      1024 file-%02d.txt" i))
                (number-sequence 1 20) "\n")
               "\n"))
         (out (anvil-shell-filter-apply 'aws-s3-ls raw)))
    (should (stringp out))
    (should (string-match-p "20" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/prettier-drops-checked-keeps-errors ()
  "`prettier' drops unchanged file lines, keeps errors."
  (skip-unless (anvil-shell-filter-test--supported-p 'prettier))
  (let* ((raw (concat
               "checking formatting...\n"
               "src/a.js 12ms\n"
               "src/b.js 8ms\n"
               "src/c.js 4ms (unchanged)\n"
               "[error] src/bad.js: SyntaxError: Unexpected token (5:3)\n"
               "All matched files use Prettier code style!\n"))
         (out (anvil-shell-filter-apply 'prettier raw)))
    (should (string-match-p "SyntaxError" out))
    (should-not (string-match-p "src/a.js 12ms" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/ruff-groups-by-code ()
  "`ruff' groups violations by rule code with a cap per code."
  (skip-unless (anvil-shell-filter-test--supported-p 'ruff))
  (let* ((raw (concat
               "src/a.py:10:5: E501 line too long\n"
               "src/a.py:12:5: E501 line too long\n"
               "src/a.py:14:5: E501 line too long\n"
               "src/a.py:16:5: E501 line too long\n"
               "src/a.py:18:5: E501 line too long\n"
               "src/a.py:20:5: E501 line too long\n"
               "src/b.py:5:1: F401 imported but unused\n"
               "src/c.py:7:1: W291 trailing whitespace\n"
               "Found 8 errors.\n"))
         (out (anvil-shell-filter-apply 'ruff raw)))
    (should (stringp out))
    (should (string-match-p "E501" out))
    (should (string-match-p "F401" out))
    (should (string-match-p "W291" out))
    (should (string-match-p "Found 8 errors" out))
    ;; All 6 E501 occurrences must NOT appear verbatim — filter caps them
    (should (< (length out) (length raw)))))


;;;; --- Phase 2a dispatch extensions ---------------------------------------

(ert-deftest anvil-shell-filter-test/lookup-phase2a-tokens ()
  "New first-token commands map to their Phase 2a filters."
  (skip-unless (anvil-shell-filter-test--supported-p 'phase2a-dispatch))
  (should (eq 'gh            (anvil-shell-filter-lookup "gh pr list")))
  (should (eq 'gh            (anvil-shell-filter-lookup "gh pr view 42")))
  (should (eq 'git-log-graph (anvil-shell-filter-lookup "git log --graph --oneline")))
  (should (eq 'pip-install   (anvil-shell-filter-lookup "pip install numpy")))
  (should (eq 'pip-install   (anvil-shell-filter-lookup "pip3 install requests")))
  (should (eq 'npm-install   (anvil-shell-filter-lookup "npm install")))
  (should (eq 'docker-ps     (anvil-shell-filter-lookup "docker ps -a")))
  (should (eq 'docker-logs   (anvil-shell-filter-lookup "docker logs my-container")))
  (should (eq 'kubectl-get   (anvil-shell-filter-lookup "kubectl get pods")))
  (should (eq 'aws-s3-ls     (anvil-shell-filter-lookup "aws s3 ls s3://bucket/")))
  (should (eq 'prettier      (anvil-shell-filter-lookup "prettier --write .")))
  (should (eq 'ruff          (anvil-shell-filter-lookup "ruff check src/"))))


;;;; --- tee-grep regex line filter ----------------------------------------

(ert-deftest anvil-shell-filter-test/tee-grep-keeps-matching-lines ()
  "tee-grep returns only lines matching the regex."
  (skip-unless (anvil-shell-filter-test--supported-p 'tee-grep))
  (let* ((raw "alpha line\nbeta line\ngamma line\nalpha tail\n")
         (out (anvil-shell-filter--grep-lines raw "alpha" 200 0)))
    (should (equal (cdr out) "alpha line\nalpha tail"))
    (should (null (car out)))))

(ert-deftest anvil-shell-filter-test/tee-grep-truncates-long-lines ()
  "tee-grep truncates per-line beyond max-line-bytes with sentinel."
  (skip-unless (anvil-shell-filter-test--supported-p 'tee-grep))
  (let* ((long (concat "OVERALL: " (make-string 500 ?x)))
         (raw (concat long "\n"))
         (out (cdr (anvil-shell-filter--grep-lines raw "OVERALL" 80 0))))
    (should (< (length out) 100))
    (should (string-match-p "…(\\([0-9]+\\) bytes elided)" out))))

(ert-deftest anvil-shell-filter-test/tee-grep-tail-fallback-on-zero-match ()
  "tee-grep falls back to last N lines when regex yields zero matches."
  (skip-unless (anvil-shell-filter-test--supported-p 'tee-grep))
  (let* ((raw "line1\nline2\nline3\nline4\nline5\n")
         (cell (anvil-shell-filter--grep-lines raw "ZZZ-no-match" 200 2))
         (used (car cell))
         (out (cdr cell)))
    (should used)
    (should (string-match-p "line5" out))
    (should-not (string-match-p "line1" out))))

(ert-deftest anvil-shell-filter-test/tee-grep-tail-fallback-disabled ()
  "tee-grep returns empty string when fallback is 0 + zero matches."
  (skip-unless (anvil-shell-filter-test--supported-p 'tee-grep))
  (let* ((raw "line1\nline2\n")
         (cell (anvil-shell-filter--grep-lines raw "ZZZ" 200 0)))
    (should (null (car cell)))
    (should (string-empty-p (cdr cell)))))

(ert-deftest anvil-shell-filter-test/tee-grep-truncate-line-helper ()
  "`--truncate-line' wraps oversized strings with elision sentinel."
  (skip-unless (anvil-shell-filter-test--supported-p 'tee-grep))
  (should (equal (anvil-shell-filter--truncate-line "abc" 10) "abc"))
  (let ((out (anvil-shell-filter--truncate-line (make-string 200 ?y) 60)))
    (should (< (length out) 80))
    (should (string-match-p "…(140 bytes elided)\\'" out))))

(ert-deftest anvil-shell-filter-test/tee-grep-end-to-end ()
  "End-to-end: shell echo + grep + tee + match-count + raw retrieval."
  (skip-unless (anvil-shell-filter-test--supported-p 'tee-grep))
  (anvil-shell-filter-test--with-state
   (let* ((cmd "printf 'fib-30 PASS 12.4x\\nfact-iter FAIL 0.3x\\nOVERALL: PASS\\n'")
          (result (anvil-shell-filter-tee-grep cmd
                                               :grep "OVERALL\\|PASS\\|FAIL"
                                               :max-line-bytes 200
                                               :tail-fallback 0)))
     (should (= 0 (plist-get result :exit)))
     (should (= 3 (plist-get result :match-count)))
     (should (null (plist-get result :used-fallback)))
     (should (string-match-p "OVERALL: PASS" (plist-get result :compressed)))
     (should (< (plist-get result :compressed-size)
                (plist-get result :raw-size)))
     (should (stringp (plist-get result :tee-id)))
     (let ((raw (anvil-shell-filter-tee-get (plist-get result :tee-id))))
       (should (string-match-p "fib-30 PASS" raw))))))



;;;; --- Phase 3 register API ----------------------------------------------

;; Each test isolates state by binding `anvil-shell-filter-handlers' and
;; `anvil-shell-filter--match-command-table' to fresh local copies so a
;; registered tag never bleeds across tests.

(defmacro anvil-shell-filter-test--with-clean-registry (&rest body)
  "Run BODY with cloned filter registry + match-command-table.
Restores both globals on exit so registered test tags do not leak."
  (declare (indent 0))
  `(let ((anvil-shell-filter-handlers
          (copy-sequence anvil-shell-filter-handlers))
         (anvil-shell-filter--match-command-table
          (copy-sequence anvil-shell-filter--match-command-table)))
     ,@body))

(ert-deftest anvil-shell-filter-test/register-rust-regex-translator-basic ()
  "Translator handles \\s / \\d / capturing groups / alternation."
  (skip-unless (anvil-shell-filter-test--supported-p 'register))
  (let ((xlat #'anvil-shell-filter--rust-regex-to-elisp))
    (should (equal (funcall xlat "^df(\\s|$)")
                   "^df\\([[:space:]]\\|$\\)"))
    (should (equal (funcall xlat "^brew\\s+(install|upgrade)\\b")
                   "^brew[[:space:]]+\\(install\\|upgrade\\)\\b"))
    (should (equal (funcall xlat "Audited \\d+ package")
                   "Audited [0-9]+ package"))
    ;; Empty / nil pass through.
    (should (null (funcall xlat nil)))
    (should (equal (funcall xlat "") ""))))

(ert-deftest anvil-shell-filter-test/register-rust-regex-translator-noncap-and-class ()
  "Translator preserves non-capturing groups + leaves char class contents alone."
  (skip-unless (anvil-shell-filter-test--supported-p 'register))
  (let ((xlat #'anvil-shell-filter--rust-regex-to-elisp))
    ;; (?:foo|bar) → \(?:foo\|bar\)
    (should (equal (funcall xlat "(?:^|/)liquibase(?:\\s|$)")
                   "\\(?:^\\|/\\)liquibase\\(?:[[:space:]]\\|$\\)"))
    ;; Char class contents stay verbatim — `(' / `|' inside [...] are literal.
    (should (equal (funcall xlat "[a-z(|)]+")
                   "[a-z(|)]+"))
    ;; \b is preserved verbatim — Elisp shares this escape.
    (should (string-match-p "\\\\b" (funcall xlat "^foo\\b")))))

(ert-deftest anvil-shell-filter-test/register-rust-regex-end-to-end-df ()
  "After register, df.toml-style match_command actually matches `df -h'."
  (skip-unless (anvil-shell-filter-test--supported-p 'register))
  (anvil-shell-filter-test--with-clean-registry
    (anvil-shell-filter-register
     'df-spec :match-command "^df(\\s|$)" :max-lines 5)
    (should (eq (anvil-shell-filter-lookup "df -h") 'df-spec))
    (should (eq (anvil-shell-filter-lookup "df") 'df-spec))
    ;; Doesn't overreach to `dfu' or `differ'.
    (should (null (anvil-shell-filter-lookup "differ a b")))))

(ert-deftest anvil-shell-filter-test/register-pipeline-order ()
  "Verify the 8-stage pipeline runs every stage in the documented order.

Pipeline order under test:
  1. strip-ansi  : `\\x1b[31mFOO line\\x1b[0m' → `FOO line'
  2. replace     : `FOO' → `FUU'
  3. match-output: NEVER-MATCH regex → no short-circuit
  4. strip-lines : drops `^noise'
  5. keep-lines  : keeps `keep' / `FUU' lines
  6. truncate    : 12-char wrap with `...'
  7. tail-lines  : last 5
  8. max-lines   : first 3 of those
  9. on-empty    : not triggered (output non-empty)"
  (skip-unless (anvil-shell-filter-test--supported-p 'register))
  (anvil-shell-filter-test--with-clean-registry
    (anvil-shell-filter-register
     'pipeline-order
     :strip-ansi t
     :replace '(("FOO" . "FUU"))
     :match-output '(("NEVER-MATCH" . "short-circuit-msg"))
     :strip-lines-matching '("^noise")
     :keep-lines-matching '("keep" "FUU")
     :truncate-lines-at 12
     :tail-lines 5
     :max-lines 3
     :on-empty "EMPTY")
    (let* ((esc (char-to-string 27))
           (raw (concat esc "[31mFOO line" esc "[0m\n"
                        "noise line drop\n"
                        "keep one\n"
                        "this is a very long keep line\n"
                        "keep three\n"
                        "keep four\n"
                        "keep five\n"))
           (out (anvil-shell-filter-apply 'pipeline-order raw))
           (lines (split-string out "\n")))
      ;; strip-ansi + replace ran and were observable in the output.
      (should-not (string-match-p (regexp-quote esc) out)) ; ANSI stripped
      (should-not (string-match-p "FOO" out))  ; replaced FOO → FUU
      (should-not (string-match-p "noise" out)) ; strip-lines dropped it
      ;; truncate to 12 chars produced at least one trimmed line.
      (should (cl-some (lambda (l) (and (> (length l) 0)
                                        (string-suffix-p "..." l)))
                       lines))
      (dolist (l lines) (should (<= (length l) 12)))
      ;; tail 5 then max 3 → at most 3 lines.
      (should (<= (length lines) 3)))))

(ert-deftest anvil-shell-filter-test/register-strip-ansi-only ()
  "strip-ansi alone removes CSI sequences without altering other content."
  (skip-unless (anvil-shell-filter-test--supported-p 'register))
  (anvil-shell-filter-test--with-clean-registry
    (anvil-shell-filter-register 'ansi-only :strip-ansi t)
    (let* ((esc (char-to-string 27))
           (raw (concat esc "[1;31merror:" esc "[0m bad"))
           (out (anvil-shell-filter-apply 'ansi-only raw)))
      (should (equal out "error: bad")))))

(ert-deftest anvil-shell-filter-test/register-replace-multiple ()
  "Multiple replace cells apply in order."
  (skip-unless (anvil-shell-filter-test--supported-p 'register))
  (anvil-shell-filter-test--with-clean-registry
    (anvil-shell-filter-register
     'rep-multi :replace '(("foo" . "bar") ("bar" . "baz")))
    ;; foo → bar, then bar → baz, so result is "baz baz" (both substitutions
    ;; applied in order means the first foo→bar feeds into bar→baz).
    (should (equal (anvil-shell-filter-apply 'rep-multi "foo bar") "baz baz"))))

(ert-deftest anvil-shell-filter-test/register-match-output-short-circuit ()
  "First match in match-output returns its message and skips later stages."
  (skip-unless (anvil-shell-filter-test--supported-p 'register))
  (anvil-shell-filter-test--with-clean-registry
    (anvil-shell-filter-register
     'short-circuit
     :match-output '(("FATAL" . "fatal-detected")
                     ("WARN"  . "warn-detected"))
     :max-lines 1)  ; would hide "FATAL" if pipeline continued
    (should (equal (anvil-shell-filter-apply
                    'short-circuit "line1\nFATAL boom\nline3")
                   "fatal-detected"))
    (should (equal (anvil-shell-filter-apply
                    'short-circuit "line1\nWARN soft\nline3")
                   "warn-detected"))
    ;; No match → falls through to max-lines.
    (should (equal (anvil-shell-filter-apply
                    'short-circuit "line1\nline2\nline3")
                   "line1"))))

(ert-deftest anvil-shell-filter-test/register-strip-and-keep-combined ()
  "strip-lines drops noise, keep-lines retains only signal."
  (skip-unless (anvil-shell-filter-test--supported-p 'register))
  (anvil-shell-filter-test--with-clean-registry
    (anvil-shell-filter-register
     'strip-keep
     :strip-lines-matching '("^DEBUG")
     :keep-lines-matching  '("ERROR" "WARN"))
    (let ((out (anvil-shell-filter-apply
                'strip-keep
                (concat "DEBUG x\nERROR y\nINFO z\nWARN w\nDEBUG q"))))
      (should (string-match-p "ERROR y" out))
      (should (string-match-p "WARN w" out))
      (should-not (string-match-p "DEBUG" out))
      (should-not (string-match-p "INFO" out)))))

(ert-deftest anvil-shell-filter-test/register-truncate-lines-at ()
  "truncate-lines-at right-trims long lines and appends `...'."
  (skip-unless (anvil-shell-filter-test--supported-p 'register))
  (anvil-shell-filter-test--with-clean-registry
    (anvil-shell-filter-register 'trunc :truncate-lines-at 10)
    (let ((out (anvil-shell-filter-apply
                'trunc "short\nthis line is way too long")))
      (should (equal (car (split-string out "\n")) "short"))
      (should (string-suffix-p "..."
                               (cadr (split-string out "\n"))))
      (should (= (length (cadr (split-string out "\n"))) 10)))))

(ert-deftest anvil-shell-filter-test/register-tail-then-max ()
  "tail-lines runs first, then max-lines selects the head of the tail window."
  (skip-unless (anvil-shell-filter-test--supported-p 'register))
  (anvil-shell-filter-test--with-clean-registry
    (anvil-shell-filter-register 'tm :tail-lines 4 :max-lines 2)
    ;; 6 input lines → tail 4 = lines 3-6 → max 2 of those = lines 3-4.
    (let ((out (anvil-shell-filter-apply
                'tm "L1\nL2\nL3\nL4\nL5\nL6")))
      (should (equal out "L3\nL4")))))

(ert-deftest anvil-shell-filter-test/register-on-empty-fallback ()
  "on-empty fallback returned when post-pipeline result trims to empty."
  (skip-unless (anvil-shell-filter-test--supported-p 'register))
  (anvil-shell-filter-test--with-clean-registry
    (anvil-shell-filter-register
     'oe :keep-lines-matching '("NEVER-MATCHES") :on-empty "OK-NOOP")
    (should (equal (anvil-shell-filter-apply 'oe "a\nb\nc") "OK-NOOP"))
    ;; Whitespace-only post-pipeline also trips on-empty.
    (should (equal (anvil-shell-filter-apply 'oe "")  "OK-NOOP"))))

(ert-deftest anvil-shell-filter-test/register-lookup-falls-through-to-match-command-table ()
  "Unknown first-token falls through to match-command-table regex."
  (skip-unless (anvil-shell-filter-test--supported-p 'register))
  (anvil-shell-filter-test--with-clean-registry
    (anvil-shell-filter-register 'mytool :match-command "^mytool\\b")
    (should (eq (anvil-shell-filter-lookup "mytool --foo bar") 'mytool))
    ;; Existing first-token rule still wins for known cmds.
    (should (eq (anvil-shell-filter-lookup "git status") 'git-status))
    ;; Truly unrelated cmds still return nil.
    (should (null (anvil-shell-filter-lookup "uname -a")))))

(ert-deftest anvil-shell-filter-test/register-from-spec-passthrough ()
  "register-from-spec forwards every keyword and returns the tag."
  (skip-unless (anvil-shell-filter-test--supported-p 'register))
  (anvil-shell-filter-test--with-clean-registry
    (let ((tag (anvil-shell-filter-register-from-spec
                '(:tag spec-tag
                       :match-command "^spec-tag\\b"
                       :max-lines 2))))
      (should (eq tag 'spec-tag))
      (should (eq (anvil-shell-filter-lookup "spec-tag args")
                  'spec-tag))
      (should (equal (anvil-shell-filter-apply 'spec-tag "L1\nL2\nL3")
                     "L1\nL2")))
    ;; Missing :tag signals an error (caller bug, not silent fallthrough).
    (should-error (anvil-shell-filter-register-from-spec
                   '(:match-command "^x\\b" :max-lines 1)))))

(ert-deftest anvil-shell-filter-test/register-replaces-existing-tag ()
  "Re-registering a tag replaces the handler and removes prior match-cmd row."
  (skip-unless (anvil-shell-filter-test--supported-p 'register))
  (anvil-shell-filter-test--with-clean-registry
    (anvil-shell-filter-register 'reapp :match-command "^old-cmd\\b" :max-lines 1)
    (anvil-shell-filter-register 'reapp :match-command "^new-cmd\\b" :max-lines 5)
    ;; New regex wins, old regex no longer maps to this tag.
    (should (eq (anvil-shell-filter-lookup "new-cmd args") 'reapp))
    (should (null (anvil-shell-filter-lookup "old-cmd args")))
    ;; Match-command-table has exactly ONE cell pointing to 'reapp.
    (should (= 1 (cl-count 'reapp anvil-shell-filter--match-command-table
                           :key #'cdr)))))


;;;; --- §7.3 tee-put cap (regression for state.db bloat) ----------------

(ert-deftest anvil-shell-filter-test/tee-put-respects-max-bytes ()
  "Tee-put truncates raw when it exceeds `anvil-shell-tee-max-bytes'."
  (skip-unless (anvil-shell-filter-test--supported-p 'tee))
  (anvil-shell-filter-test--with-state
    (let* ((anvil-shell-tee-max-bytes 32)
           (raw (concat (make-string 200 ?x) "TAIL"))
           (id (anvil-shell-filter--tee-put raw))
           (got (anvil-shell-filter-tee-get id)))
      (should (stringp got))
      (should (string-match-p "anvil-shell-tee: truncated" got))
      (should (< (length got) (length raw))))))

(ert-deftest anvil-shell-filter-test/tee-put-cap-nil-keeps-full ()
  "Setting `anvil-shell-tee-max-bytes' to nil disables capping."
  (skip-unless (anvil-shell-filter-test--supported-p 'tee))
  (anvil-shell-filter-test--with-state
    (let* ((anvil-shell-tee-max-bytes nil)
           (raw (make-string 1000 ?y))
           (id (anvil-shell-filter--tee-put raw))
           (got (anvil-shell-filter-tee-get id)))
      (should (equal raw got)))))


;;;; --- §7.1 trace events ------------------------------------------------

(ert-deftest anvil-shell-filter-test/trace-events-disabled-no-rows ()
  "When `anvil-shell-filter-trace-events' is nil, no trace rows accrue."
  (skip-unless (anvil-shell-filter-test--supported-p 'tee))
  (anvil-shell-filter-test--with-state
    (let ((anvil-shell-filter-trace-events nil))
      (anvil-shell-filter--trace "tr-test" "start" (current-time))
      (should (null (anvil-state-list-keys
                     :ns anvil-shell-filter--trace-ns))))))

(ert-deftest anvil-shell-filter-test/trace-events-enabled-records-phase ()
  "When enabled, `--trace' writes a phase row with elapsed-ms."
  (skip-unless (anvil-shell-filter-test--supported-p 'tee))
  (anvil-shell-filter-test--with-state
    (let* ((anvil-shell-filter-trace-events t)
           (start (current-time)))
      (anvil-shell-filter--trace "tr-x" "start" start)
      (let ((row (anvil-state-get
                  "tr-x-start" :ns anvil-shell-filter--trace-ns)))
        (should (listp row))
        (should (equal "start" (plist-get row :phase)))
        (should (integerp (plist-get row :at)))
        (should (integerp (plist-get row :elapsed-ms)))))))


;;;; --- Doc 45 / TACO Phase 1 critical fallback --------------------------

(ert-deftest anvil-shell-filter-test/taco-critical-keeps-error-context ()
  "TACO critical fallback keeps error-like lines and omits distant noise."
  (skip-unless (anvil-shell-filter-test--supported-p 'taco-critical))
  (let* ((anvil-shell-filter-taco-critical-min-lines 5)
         (anvil-shell-filter-taco-critical-context-lines 1)
         (raw (string-join
               (append (cl-loop for i from 1 to 20 collect (format "noise-%02d" i))
                       '("before" "Traceback (most recent call last):"
                         "AssertionError: bad value" "after")
                       (cl-loop for i from 21 to 40 collect (format "noise-%02d" i)))
               "\n"))
         (out (anvil-shell-filter-apply 'taco-critical raw)))
    (should (string-match-p "anvil-taco" out))
    (should (string-match-p "Traceback" out))
    (should (string-match-p "AssertionError" out))
    (should (string-match-p "stdout lines omitted" out))
    (should-not (string-match-p "noise-01" out))
    (should-not (string-match-p "noise-40" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/taco-critical-auto-for-unknown-run ()
  "Unknown failing commands use TACO critical fallback under filter=auto."
  (skip-unless (anvil-shell-filter-test--supported-p 'taco-critical))
  (anvil-shell-filter-test--with-state
    (let* ((anvil-shell-filter-taco-critical-min-lines 5)
           (anvil-shell-filter-taco-critical-tail-lines 2)
           (raw (string-join
                 (append (cl-loop for i from 1 to 80
                                  collect (format "step-%02d ................................" i))
                         '("FAILED: build target" "tail-a" "tail-b"))
                 "\n")))
      (cl-letf (((symbol-function 'anvil-shell)
                 (lambda (_cmd _opts)
                   (list :exit 2 :stdout raw :stderr "stderr detail"))))
        (let ((result (anvil-shell-filter-run "unknown-tool --verbose")))
          (should (eq 'taco-critical (plist-get result :filter)))
          (should (equal 2 (plist-get result :exit)))
          (should (equal "stderr detail" (plist-get result :stderr)))
          (should (string-match-p "FAILED: build target"
                                  (plist-get result :compressed)))
          (should (string-match-p "tail-b" (plist-get result :compressed)))
          (should (equal raw (anvil-shell-filter-tee-get
                              (plist-get result :tee-id)))))))))

(ert-deftest anvil-shell-filter-test/taco-critical-short-clean-passthrough ()
  "Short clean unknown output stays raw passthrough under filter=auto."
  (skip-unless (anvil-shell-filter-test--supported-p 'taco-critical))
  (anvil-shell-filter-test--with-state
    (cl-letf (((symbol-function 'anvil-shell)
               (lambda (_cmd _opts)
                 (list :exit 0 :stdout "one\ntwo\nthree" :stderr ""))))
      (let ((result (anvil-shell-filter-run "unknown-tool")))
        (should-not (plist-get result :filter))
        (should (equal "one\ntwo\nthree" (plist-get result :compressed)))))))


(provide 'anvil-shell-filter-test)

;;; anvil-shell-filter-test.el ends here
