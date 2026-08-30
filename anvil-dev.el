;;; anvil-dev.el --- Developer / ops helpers for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Small tools that are *about* anvil rather than part of its
;; operation.  Currently:
;;
;;   - `anvil-self-sync-check'
;;     — inspect the git state of the anvil clone that Emacs loaded,
;;       and optionally compare it to a separate dev checkout so the
;;       "installed tree silently diverged from the dev tree" bug
;;       (2026-04-16) is caught in one MCP call.
;;
;;   - `anvil-dev-test-run-all'
;;     — run every tests/anvil-*-test.el in per-file `emacs --batch'
;;       subprocesses and aggregate the ERT counts.  Covers the gap
;;       where CI only exercises `anvil-test.el'.  Exposes a matching
;;       MCP tool and a `-batch' entry point for Makefile use.
;;
;;   - `anvil-dev-scaffold-module'
;;     — emit a new `anvil-NAME.el' and `tests/anvil-NAME-test.el'
;;       with standard headers, enable/disable stubs, and a passing
;;       smoke test so a new module compiles + runs out of the box.
;;
;;   - `anvil-codex-efficiency-check'
;;     — inspect a Codex setup for the token-saving baseline:
;;       Serena / Context7 MCP server entries, required executables,
;;       local Codex skills, and the project-side Serena config /
;;       recovery reference note.
;;
;;   - `anvil-claude-limits-analyze'
;;     — turn Claude Code's "what's contributing to your limits usage?"
;;       text into metrics + Anvil-focused mitigation actions.
;;
;; Enable via `(add-to-list 'anvil-optional-modules 'dev)' in init.

;;; Code:

(require 'cl-lib)
(require 'anvil-server)

(defgroup anvil-dev nil
  "Developer / ops helpers for anvil."
  :group 'anvil
  :prefix "anvil-dev-")

(defcustom anvil-dev-source-path nil
  "Optional path to a separate dev checkout of anvil.el.
When non-nil, `anvil-self-sync-check' fetches its HEAD and
compares it against the installed clone's HEAD so `git pull'
gaps surface before the next daemon restart."
  :type '(choice (const :tag "No dev clone" nil) directory)
  :group 'anvil-dev)

(defcustom anvil-dev-emacs-bin (or (executable-find "emacs") "emacs")
  "Emacs binary used to spawn test subprocesses from the test runner."
  :type 'file
  :group 'anvil-dev)

(defcustom anvil-dev-test-file-pattern "\\`anvil-.*test\\.el\\'"
  "Regex matching test files under `tests/' for `anvil-dev-test-run-all'.
The default accepts both the per-module form `anvil-MOD-test.el'
and the core aggregator `anvil-test.el'."
  :type 'regexp
  :group 'anvil-dev)

(defconst anvil-dev--server-id "emacs-eval"
  "Server ID for the dev-* MCP tools.")

(defconst anvil-dev--codex-efficiency-required-skills
  '("anvil-memory-worklog"
    "notes-org-editing"
    "notes-development"
    "nelisp-development"
    "web-article-archive"
    "notes-report-pipeline")
  "Codex skill directories expected by the Notes efficiency setup.")

;;;; --- internal ------------------------------------------------------------

(defun anvil-dev--git-at (dir &rest args)
  "Run `git ARGS' inside DIR and return trimmed stdout, or nil on failure."
  (when (and dir (file-directory-p dir))
    (with-temp-buffer
      (let* ((default-directory (file-name-as-directory dir))
             (status (apply #'call-process "git" nil t nil args)))
        (when (and (integerp status) (zerop status))
          (string-trim (buffer-string)))))))

(defun anvil-dev--short-sha (sha)
  "Return the first 7 chars of SHA, or SHA itself if shorter / nil."
  (and sha (stringp sha)
       (if (> (length sha) 7) (substring sha 0 7) sha)))

(defun anvil-dev--git-state (dir)
  "Return a plist describing the git state of DIR (nil if not a worktree).
Keys: :head :branch :dirty-count."
  (let ((head (anvil-dev--git-at dir "rev-parse" "HEAD")))
    (when head
      (let* ((branch (anvil-dev--git-at dir "rev-parse" "--abbrev-ref" "HEAD"))
             (porc (anvil-dev--git-at dir "status" "--porcelain"))
             (dirty (if (and porc (not (string-empty-p porc)))
                        (length (split-string porc "\n" t))
                      0)))
        (list :head head :branch branch :dirty-count dirty)))))

(defun anvil-dev--derive-warning (src-dir installed-state dev-dir dev-state)
  "Produce a human-readable warning string, or nil when all is well."
  (cond
   ((null src-dir)
    "anvil-server not located — library not loaded")
   ((null installed-state)
    (format "installed dir %s is not a git worktree" src-dir))
   ((and dev-dir (null dev-state))
    (format "dev-source-path %s is not a git worktree" dev-dir))
   ((and dev-state
         (not (equal (plist-get installed-state :head)
                     (plist-get dev-state :head))))
    (format "installed HEAD %s ≠ dev HEAD %s — run `git pull' in %s"
            (anvil-dev--short-sha (plist-get installed-state :head))
            (anvil-dev--short-sha (plist-get dev-state :head))
            src-dir))))

;;;; --- public --------------------------------------------------------------

;;;###autoload
(defun anvil-self-sync-check ()
  "Report anvil's installed git state, and mismatch vs the dev checkout.

Returns a plist:
  :installed-dir         where `anvil-server' was loaded from
  :installed-head        HEAD SHA of that worktree (nil if not a git repo)
  :installed-branch      current branch of the installed clone
  :installed-dirty-count number of modified / untracked files
  :dev-dir               `anvil-dev-source-path' (or nil)
  :dev-head              HEAD SHA of the dev clone (nil / not set)
  :dev-branch            branch of the dev clone
  :in-sync               t when HEADs match OR when no dev-dir is configured
  :warning               short human string when something is off (nil = OK)

Motivation: 2026-04-16 reproduced the \"old anvil-worker loaded\"
trap where the running daemon read an outdated default because a
second clone (`external-packages/anvil.el/`) stayed behind the
dev tree.  One call to this helper now surfaces that mismatch."
  (let* ((src-file (locate-library "anvil-server"))
         (src-dir  (and src-file (file-name-directory src-file)))
         (installed (and src-dir (anvil-dev--git-state src-dir)))
         (dev-dir  anvil-dev-source-path)
         (dev-st   (and dev-dir (anvil-dev--git-state dev-dir)))
         (in-sync  (or (null dev-dir)
                       (and installed dev-st
                            (equal (plist-get installed :head)
                                   (plist-get dev-st :head)))))
         (warning  (anvil-dev--derive-warning
                    src-dir installed dev-dir dev-st)))
    (list :installed-dir         src-dir
          :installed-head        (plist-get installed :head)
          :installed-branch      (plist-get installed :branch)
          :installed-dirty-count (or (plist-get installed :dirty-count) 0)
          :dev-dir               dev-dir
          :dev-head              (plist-get dev-st :head)
          :dev-branch            (plist-get dev-st :branch)
          :in-sync               (and in-sync t)
          :warning               warning)))

(defun anvil-dev--tool-self-sync-check ()
  "MCP wrapper for `anvil-self-sync-check'.

MCP Parameters: none.  Returns a printed plist comparing the
installed anvil clone's git HEAD with `anvil-dev-source-path'."
  (anvil-server-with-error-handling
   (format "%S" (anvil-self-sync-check))))

;;;; --- codex efficiency check ---------------------------------------------

(defun anvil-dev--codex-home ()
  "Return the effective Codex home directory."
  (or (getenv "CODEX_HOME")
      (expand-file-name "~/.codex")))

(defun anvil-dev--read-file-string (file)
  "Return FILE contents as a string, or nil when unreadable."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun anvil-dev--toml-has-section-p (content section)
  "Return non-nil when TOML CONTENT has a top-level SECTION table."
  (and content
       (string-match-p
        (format "^\\[mcp_servers\\.%s\\][ \t]*$"
                (regexp-quote section))
        content)))

(defun anvil-dev--skill-present-p (skills-dir skill)
  "Return non-nil when SKILLS-DIR contains SKILL with a readable SKILL.md."
  (file-readable-p (expand-file-name (format "%s/SKILL.md" skill)
                                     skills-dir)))

(defun anvil-dev--plist-bool-alist (items predicate)
  "Return an alist mapping ITEMS to t/nil according to PREDICATE."
  (mapcar (lambda (item) (cons item (and (funcall predicate item) t)))
          items))

;;;###autoload
(defun anvil-codex-efficiency-check (&optional codex-home project-root)
  "Inspect the local Codex token-saving setup.

CODEX-HOME defaults to `$CODEX_HOME' or `~/.codex'.  PROJECT-ROOT
defaults to `default-directory'.  The check is read-only and
validates the baseline used by this workspace:
  - `~/.codex/config.toml' is readable;
  - `emacs-eval', `serena', and `context7' MCP server sections exist;
  - `uvx' and `npx' executables resolve;
  - required local Codex skill directories contain `SKILL.md';
  - project-side `.serena/project.yml' and the recovery reference
    note exist.

Returns a plist with :ok and :warnings so callers can fail fast
without reading every config file by hand."
  (let* ((home (file-name-as-directory
                (expand-file-name (or codex-home (anvil-dev--codex-home)))))
         (root (file-name-as-directory
                (expand-file-name (or project-root default-directory))))
         (config-file (expand-file-name "config.toml" home))
         (config-content (anvil-dev--read-file-string config-file))
         (skills-dir (expand-file-name "skills" home))
         (required-mcp '("emacs-eval" "serena" "context7"))
         (mcp-present
          (anvil-dev--plist-bool-alist
           required-mcp
           (lambda (server)
             (anvil-dev--toml-has-section-p config-content server))))
         (executables
          (list (cons "uvx" (executable-find "uvx"))
                (cons "npx" (executable-find "npx"))))
         (skills-present
          (anvil-dev--plist-bool-alist
           anvil-dev--codex-efficiency-required-skills
           (lambda (skill)
             (anvil-dev--skill-present-p skills-dir skill))))
         (serena-project (expand-file-name ".serena/project.yml" root))
         (reference-file
          (expand-file-name ".claude/reference/codex-efficiency-setup.md"
                            root))
         warnings)
    (unless config-content
      (push (format "Codex config is not readable: %s" config-file)
            warnings))
    (dolist (server mcp-present)
      (unless (cdr server)
        (push (format "Missing MCP server section: %s" (car server))
              warnings)))
    (dolist (exe executables)
      (unless (cdr exe)
        (push (format "Missing executable on exec-path: %s" (car exe))
              warnings)))
    (dolist (skill skills-present)
      (unless (cdr skill)
        (push (format "Missing Codex skill: %s" (car skill))
              warnings)))
    (unless (file-readable-p serena-project)
      (push (format "Missing Serena project config: %s" serena-project)
            warnings))
    (unless (file-readable-p reference-file)
      (push (format "Missing Codex recovery reference: %s" reference-file)
            warnings))
    (list :ok (null warnings)
          :codex-home home
          :project-root root
          :config-file config-file
          :mcp-servers mcp-present
          :executables executables
          :skills-dir skills-dir
          :skills skills-present
          :serena-project serena-project
          :reference-file reference-file
          :warnings (nreverse warnings))))

(defun anvil-dev--tool-codex-efficiency-check (&optional codex-home project-root)
  "MCP wrapper for `anvil-codex-efficiency-check'.

MCP Parameters:
  codex-home   - Optional Codex home directory.  Empty string uses
                 `$CODEX_HOME' or `~/.codex'.
  project-root - Optional project root containing `.serena/' and
                 `.claude/reference/'.  Empty string uses
                 `default-directory'."
  (anvil-server-with-error-handling
   (let ((home (and codex-home (stringp codex-home)
                    (not (string-empty-p codex-home))
                    codex-home))
         (root (and project-root (stringp project-root)
                    (not (string-empty-p project-root))
                    project-root)))
     (format "%S" (anvil-codex-efficiency-check home root)))))

;;;; --- Claude limits report analysis --------------------------------------

(defun anvil-dev--limits-percent-after (report marker)
  "Return the first integer percent before MARKER in REPORT.
Matches lines like \"91% of your usage was at >150k context\"."
  (when (and (stringp report) (stringp marker)
             (string-match
              (format "\\([0-9]+\\)%%[^\n]*%s" (regexp-quote marker))
              report))
    (string-to-number (match-string 1 report))))

(defun anvil-dev--limits-table-percent (report label)
  "Return integer percent from a simple LABEL table row in REPORT.
Matches lines like \"emacs-eval                     73%\"."
  (when (and (stringp report) (stringp label))
    (catch 'found
      (dolist (line (split-string report "\n"))
        (let ((trimmed (string-trim-left line)))
          (when (and (string-prefix-p label trimmed)
                     (or (= (length trimmed) (length label))
                         (member (aref trimmed (length label))
                                 '(?\s ?\t))))
            (when (string-match "\\([0-9]+\\)%" trimmed)
              (throw 'found (string-to-number (match-string 1 trimmed))))))))))

(defun anvil-dev--limits-metric (report key)
  "Extract metric KEY from Claude limits REPORT."
  (pcase key
    ('high-context
     (anvil-dev--limits-percent-after report ">150k context"))
    ('subagent-heavy
     (anvil-dev--limits-percent-after report "subagent-heavy sessions"))
    ('long-sessions
     (anvil-dev--limits-percent-after report "sessions active for 8+ hours"))
    ('loop
     (or (anvil-dev--limits-percent-after report "came from /loop")
         (anvil-dev--limits-table-percent report "/loop")))
    ('emacs-eval
     (or (anvil-dev--limits-percent-after report "mcp server \"emacs-eval\"")
         (anvil-dev--limits-table-percent report "emacs-eval")))))

(defun anvil-dev--limits-severity (percent)
  "Return severity symbol for PERCENT."
  (cond
   ((null percent) 'unknown)
   ((>= percent 70) 'critical)
   ((>= percent 40) 'high)
   ((>= percent 20) 'medium)
   ((> percent 0) 'low)
   (t 'none)))

(defun anvil-dev--limits-action (metric percent)
  "Return an action plist for METRIC at PERCENT."
  (let ((severity (anvil-dev--limits-severity percent)))
    (pcase metric
      ('high-context
       (list :metric metric :percent percent :severity severity
             :action "Trigger /compact mid-task and /clear when switching tasks; prefer session-context packs over carrying raw history."))
      ('subagent-heavy
       (list :metric metric :percent percent :severity severity
             :action "Gate subagent spawning; use Serena/context tools first and route simple exploration to cheaper or local helpers."))
      ('long-sessions
       (list :metric metric :percent percent :severity severity
             :action "Add session-age/context-pressure checks to Stop/UserPrompt hooks; warn after long background loops."))
      ('loop
       (list :metric metric :percent percent :severity severity
             :action "Scope /loop skills down, cap iterations, and emit a compact reminder before loop continuation."))
      ('emacs-eval
       (list :metric metric :percent percent :severity severity
             :action "Reduce emacs-eval result size: prefer outline/symbol tools, filtered shell output, minimal modes, and disable unused MCP surfaces.")))))

;;;###autoload
(defun anvil-claude-limits-analyze (report)
  "Analyze Claude Code limits REPORT and return Anvil mitigation guidance.

REPORT is the pasted text from Claude Code's \"what's contributing
to your limits usage?\" screen.  The parser is intentionally
tolerant of copied TUI corruption: it looks for stable percent
markers and table rows, then returns the extracted metrics plus
ranked actions.

Returns a plist with :metrics, :actions, and :top-actions."
  (unless (and (stringp report) (not (string-empty-p report)))
    (user-error "anvil-claude-limits-analyze: REPORT must be non-empty text"))
  (let* ((metrics (mapcar (lambda (key)
                            (cons key (anvil-dev--limits-metric report key)))
                          '(high-context subagent-heavy long-sessions
                            loop emacs-eval)))
         (actions (mapcar (lambda (cell)
                            (anvil-dev--limits-action (car cell) (cdr cell)))
                          metrics))
         (ranked (sort (copy-sequence actions)
                       (lambda (a b)
                         (> (or (plist-get a :percent) -1)
                            (or (plist-get b :percent) -1))))))
    (list :metrics metrics
          :actions actions
          :top-actions (cl-subseq ranked 0 (min 3 (length ranked))))))

(defun anvil-dev--tool-claude-limits-analyze (report)
  "MCP wrapper for `anvil-claude-limits-analyze'.

MCP Parameters:
  report - Text copied from Claude Code's limits usage breakdown."
  (anvil-server-with-error-handling
   (format "%S" (anvil-claude-limits-analyze report))))

;;;; --- test-run-all --------------------------------------------------------

(defun anvil-dev--project-root ()
  "Best-effort project root detection.
Priority: `anvil-dev-source-path', then the directory of the
loaded `anvil-server' library."
  (or (and anvil-dev-source-path (file-directory-p anvil-dev-source-path)
           (expand-file-name anvil-dev-source-path))
      (let ((src-file (locate-library "anvil-server")))
        (and src-file (file-name-directory src-file)))))

(defun anvil-dev--discover-test-files (dir)
  "Return sorted list of `tests/anvil-*-test.el' absolute paths in DIR."
  (let ((tests-dir (expand-file-name "tests" dir)))
    (and (file-directory-p tests-dir)
         (sort (directory-files tests-dir t anvil-dev-test-file-pattern)
               #'string<))))

(defun anvil-dev--parse-ert-summary (output)
  "Extract ERT summary counts from batch OUTPUT, or nil.
Handles both the unskipped and skipped forms of the summary line."
  (cond
   ((string-match
     "Ran \\([0-9]+\\) tests?, \\([0-9]+\\) results? as expected, \\([0-9]+\\) unexpected, \\([0-9]+\\) skipped"
     output)
    (list :total   (string-to-number (match-string 1 output))
          :passed  (string-to-number (match-string 2 output))
          :failed  (string-to-number (match-string 3 output))
          :skipped (string-to-number (match-string 4 output))))
   ((string-match
     "Ran \\([0-9]+\\) tests?, \\([0-9]+\\) results? as expected, \\([0-9]+\\) unexpected"
     output)
    (list :total   (string-to-number (match-string 1 output))
          :passed  (string-to-number (match-string 2 output))
          :failed  (string-to-number (match-string 3 output))
          :skipped 0))))

(defun anvil-dev--run-one-test-file (file root)
  "Run FILE under ERT in a `emacs --batch' rooted at ROOT.
Returns a result plist with :file :ok :exit :elapsed-ms :total
:passed :failed :skipped :output."
  (let* ((start (float-time))
         (buf (generate-new-buffer " *anvil-dev-test-out*"))
         (tests-dir (expand-file-name "tests" root))
         (exit (let ((default-directory (file-name-as-directory root)))
                 (apply #'call-process
                        anvil-dev-emacs-bin nil buf nil
                        (append
                         (list "--batch"
                               "--eval" "(setq load-prefer-newer t)"
                               "-L" root)
                         (and (file-directory-p tests-dir)
                              (list "-L" tests-dir))
                         (list "-l" "ert" "-l" file
                               "-f" "ert-run-tests-batch-and-exit")))))
         (elapsed (- (float-time) start))
         (output (with-current-buffer buf (buffer-string))))
    (kill-buffer buf)
    (let ((summary (anvil-dev--parse-ert-summary output)))
      (list :file       (file-name-nondirectory file)
            :exit       exit
            :ok         (and (integerp exit) (zerop exit))
            :elapsed-ms (round (* elapsed 1000))
            :total      (or (plist-get summary :total) 0)
            :passed     (or (plist-get summary :passed) 0)
            :failed     (or (plist-get summary :failed) 0)
            :skipped    (or (plist-get summary :skipped) 0)
            :output     output))))

;;;###autoload
(cl-defun anvil-dev-test-run-all (&optional project-dir &key minimal)
  "Run every tests/anvil-*-test.el in PROJECT-DIR via `emacs --batch'.
Each file runs in its own subprocess so one file's load error
cannot mask failures in others.  Returns an aggregated plist:
  :project-dir ROOT :file-count N :total T :passed P :failed F
  :skipped S :elapsed-ms MS :failed-files (FILE...) :per-file (PLIST...)

With non-nil :MINIMAL, the per-file list is omitted entirely,
cutting the return value down to the aggregate counters plus
`:failed-files'.  Useful for high-frequency polling (CI dashboards,
dogfooding loops) where the per-file breakdown costs orders of
magnitude more bytes than the aggregate it summarises; failure
diagnosis can always fall back to a non-minimal run."
  (interactive)
  (let ((root (or project-dir (anvil-dev--project-root))))
    (unless (and root (file-directory-p root))
      (user-error "anvil-dev-test-run-all: project root not found"))
    (let ((files (anvil-dev--discover-test-files root)))
      (unless files
        (user-error "anvil-dev-test-run-all: no test files in %s/tests" root))
      (let* ((results (mapcar (lambda (f) (anvil-dev--run-one-test-file f root))
                              files))
             (sum (lambda (key)
                    (apply #'+ (mapcar (lambda (r) (plist-get r key)) results))))
             (total  (funcall sum :total))
             (passed (funcall sum :passed))
             (failed (funcall sum :failed))
             (skipped (funcall sum :skipped))
             (elapsed-ms (funcall sum :elapsed-ms))
             (bad (cl-remove-if (lambda (r) (plist-get r :ok)) results))
             (base (list :project-dir  root
                         :file-count   (length files)
                         :total        total
                         :passed       passed
                         :failed       failed
                         :skipped      skipped
                         :elapsed-ms   elapsed-ms
                         :failed-files (mapcar (lambda (r) (plist-get r :file)) bad))))
        (when (called-interactively-p 'any)
          (message "Anvil test-run-all: %d files · %d/%d tests · %d failed · %.1fs"
                   (length files) passed total failed
                   (/ elapsed-ms 1000.0)))
        (if minimal
            base
          (append base (list :per-file results)))))))

;;;###autoload
(defun anvil-dev-test-run-all-batch ()
  "Batch entry point: run all tests and exit 0 on green, 1 on any failure.
Invoke as `emacs --batch -L . -l anvil-dev -f anvil-dev-test-run-all-batch'."
  (let ((result (anvil-dev-test-run-all default-directory)))
    (message "\n== anvil test-run-all ==")
    (dolist (r (plist-get result :per-file))
      (message "  %-38s %d/%d  %s  %dms"
               (plist-get r :file)
               (plist-get r :passed)
               (plist-get r :total)
               (if (plist-get r :ok) "OK  " "FAIL")
               (plist-get r :elapsed-ms)))
    (message "-- totals: %d files, %d/%d tests, %d failed, %.1fs --"
             (plist-get result :file-count)
             (plist-get result :passed)
             (plist-get result :total)
             (plist-get result :failed)
             (/ (plist-get result :elapsed-ms) 1000.0))
    (kill-emacs (if (zerop (plist-get result :failed)) 0 1))))

(defun anvil-dev--tool-test-run-all (&optional project-dir minimal)
  "MCP wrapper for `anvil-dev-test-run-all'.

Drops the per-file :output (noisy ERT logs) from the response so
a green run stays compact; failure output is still reachable via
the interactive command.  With MINIMAL truthy, the per-file list
is omitted entirely — cuts the response to ~10% of the default
size, ideal for dogfooding / polling loops where only the
aggregate counters matter.

MCP Parameters:
  project-dir - Optional anvil checkout root (defaults to installed clone)
  minimal     - Optional truthy string (\"t\" / \"true\" / \"1\") to
                omit the per-file breakdown.  Failed-files list is
                still returned so a red run can still be diagnosed
                at a glance."
  (anvil-server-with-error-handling
   (let* ((dir (and project-dir (stringp project-dir)
                    (not (string-empty-p project-dir))
                    project-dir))
          (mini (and minimal (stringp minimal)
                     (not (member minimal
                                  '("" "nil" "false" "0" "no" "False" "NIL")))))
          (result (anvil-dev-test-run-all dir :minimal mini)))
     (if mini
         (format "%S" result)
       (let* ((lean-per-file
               (mapcar (lambda (r)
                         (list :file (plist-get r :file)
                               :ok (plist-get r :ok)
                               :total (plist-get r :total)
                               :passed (plist-get r :passed)
                               :failed (plist-get r :failed)
                               :skipped (plist-get r :skipped)
                               :elapsed-ms (plist-get r :elapsed-ms)))
                       (plist-get result :per-file)))
              (compact (plist-put (copy-sequence result) :per-file lean-per-file)))
         (format "%S" compact))))))

;;;; --- module scaffold -----------------------------------------------------

(defconst anvil-dev--module-template
  ";;; anvil-%NAME%.el --- %DESC% -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; TODO: describe the purpose of this module.

;;; Code:

(require 'cl-lib)
(require 'anvil-server)

(defgroup anvil-%NAME% nil
  \"%DESC%\"
  :group 'anvil
  :prefix \"anvil-%NAME%-\")

(defconst anvil-%NAME%--server-id \"emacs-eval\"
  \"Server ID under which anvil-%NAME% MCP tools are registered.\")

;;;###autoload
(defun anvil-%NAME%-enable ()
  \"Register the anvil-%NAME% MCP tools.\"
  ;; TODO: register tools via `anvil-server-register-tool'.
  t)

(defun anvil-%NAME%-disable ()
  \"Unregister the anvil-%NAME% MCP tools.\"
  ;; TODO: unregister tools via `anvil-server-unregister-tool'.
  t)

(provide 'anvil-%NAME%)
;;; anvil-%NAME%.el ends here
"
  "Template for a new module file.  %NAME% / %DESC% are substituted.")

(defconst anvil-dev--test-template
  ";;; anvil-%NAME%-test.el --- Tests for anvil-%NAME% -*- lexical-binding: t; -*-

(require 'ert)
(require 'anvil-%NAME%)

(ert-deftest anvil-%NAME%-test-feature-provided ()
  \"The module's feature symbol is provided after load.\"
  (should (featurep 'anvil-%NAME%)))

(ert-deftest anvil-%NAME%-test-enable-disable-callable ()
  \"Enable and disable stubs exist and return without error.\"
  (should (fboundp 'anvil-%NAME%-enable))
  (should (fboundp 'anvil-%NAME%-disable)))

(provide 'anvil-%NAME%-test)
;;; anvil-%NAME%-test.el ends here
"
  "Template for a new test file.  %NAME% is substituted.")

(defun anvil-dev--valid-module-name-p (name)
  "Non-nil if NAME is a plausible anvil module name (lowercase, hyphenated).
Binds `case-fold-search' to nil so \"Bad-Case\" fails — the default
in many buffers is t, which would let `[a-z]' match uppercase."
  (and (stringp name)
       (> (length name) 0)
       (let ((case-fold-search nil))
         (string-match-p "\\`[a-z][a-z0-9-]*\\'" name))))

(defun anvil-dev--substitute-template (template name desc)
  "Replace %NAME%/%DESC% placeholders in TEMPLATE."
  (let ((out template))
    (setq out (replace-regexp-in-string "%NAME%" name out t t))
    (setq out (replace-regexp-in-string "%DESC%" desc out t t))
    out))

;;;###autoload
(defun anvil-dev-scaffold-module (name description &optional project-dir)
  "Create anvil-NAME.el and tests/anvil-NAME-test.el in PROJECT-DIR.
DESCRIPTION is a one-line summary inserted into the first line of
the new module.  Returns a plist with :module-file and :test-file
paths.  Errors if either target already exists."
  (interactive
   (list (read-string "Module name (lowercase, hyphens ok): ")
         (read-string "One-line description: ")))
  (unless (anvil-dev--valid-module-name-p name)
    (user-error "anvil-dev-scaffold-module: invalid module name: %s" name))
  (when (or (null description) (string-empty-p description))
    (user-error "anvil-dev-scaffold-module: description must not be empty"))
  (let* ((root (or project-dir (anvil-dev--project-root)))
         (_ (unless (and root (file-directory-p root))
              (user-error "anvil-dev-scaffold-module: project root not found")))
         (tests-dir (expand-file-name "tests" root))
         (mod-file  (expand-file-name (format "anvil-%s.el" name) root))
         (test-file (expand-file-name (format "anvil-%s-test.el" name) tests-dir)))
    (when (file-exists-p mod-file)
      (user-error "Module file already exists: %s" mod-file))
    (when (file-exists-p test-file)
      (user-error "Test file already exists: %s" test-file))
    (unless (file-directory-p tests-dir)
      (make-directory tests-dir t))
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file mod-file
        (insert (anvil-dev--substitute-template
                 anvil-dev--module-template name description)))
      (with-temp-file test-file
        (insert (anvil-dev--substitute-template
                 anvil-dev--test-template name description))))
    (when (called-interactively-p 'any)
      (message "Scaffolded %s and %s"
               (file-name-nondirectory mod-file)
               (file-name-nondirectory test-file)))
    (list :module-file mod-file :test-file test-file)))

(defun anvil-dev--tool-scaffold-module (name description)
  "MCP wrapper for `anvil-dev-scaffold-module'.

MCP Parameters:
  name - Module name (lowercase letters + digits + hyphens, e.g. \"offload\")
  description - One-line summary used in the -*- header"
  (anvil-server-with-error-handling
   (format "%S" (anvil-dev-scaffold-module name description))))

;;;; --- release audit -------------------------------------------------------
;;
;; v0.2.0 shipped with an Emacs 30 arglist-strip bug (issue #9) because
;; the pre-release review missed `(_args)`-style MCP wrappers in three
;; modules even though an earlier fix had documented the pattern.  This
;; audit scans the anvil tree for classes of defect that are cheap to
;; detect and painful to discover in production:
;;
;;   1. `_`-prefix argument on an MCP tool wrapper (Emacs 30 strips the
;;      underscore; the schema validator then rejects the tool).
;;   2. An MCP tool wrapper takes a real argument but has no
;;      `MCP Parameters:' section in its docstring — the schema generator
;;      can't describe the parameter and Claude can't call the tool.
;;   3. Any docs/design/*.org whose `* STATUS' text does not contain
;;      `SHIPPED' — informational surface of the master-integration gate.

(defun anvil-dev--audit-default-root ()
  "Return the directory that should be audited.
Prefers the dev checkout (`anvil-dev-source-path') when set; falls
back to the installed anvil directory (containing `anvil-dev.el')."
  (or (and anvil-dev-source-path
           (file-directory-p anvil-dev-source-path)
           (file-name-as-directory anvil-dev-source-path))
      (when-let* ((lib (locate-library "anvil-dev")))
        (file-name-directory lib))))

(defun anvil-dev--audit-module-files (root)
  "Return the anvil-*.el source files in ROOT, excluding tests and anvil-dev.el.
The audit skips anvil-dev.el because its own scanners match its own
regexes and would report spurious hits on its template strings."
  (let ((files (directory-files root t "\\`anvil-[^/]+\\.el\\'" t)))
    (cl-remove-if (lambda (f)
                    (let ((base (file-name-nondirectory f)))
                      (or (string-match-p "-test\\.el\\'" base)
                          (equal base "anvil-dev.el"))))
                  files)))

(defconst anvil-dev--audit-wrapper-regex
  "^(defun \\([a-zA-Z0-9-]+--tool-[a-zA-Z0-9-]+\\) (\\([^)]*\\))"
  "Matches a top-level MCP tool wrapper `defun'.
Group 1 captures the defun name (must use the `--tool-' private
prefix convention).  Group 2 captures the literal argument list,
possibly empty.

The `--tool-' double-dash prefix is treated as authoritative; a
function named `foo--tool-bar-error' is still caught structurally
and filtered out separately via `anvil-dev--audit-wrapper-name-p'
because it is idiomatic to name error helpers alongside real
wrappers.")

(defun anvil-dev--audit-wrapper-name-p (name)
  "Return non-nil when NAME is a real MCP tool wrapper (not a helper).
Names ending in `-error' are error-helper siblings of real
wrappers (e.g. `anvil-org--tool-validation-error') and must not
be audited."
  (not (string-suffix-p "-error" name)))

(defun anvil-dev--audit-scan-arglist-strip-in-files (files)
  "Scan FILES (explicit list of paths) for arglist-strip hits.
Used both by `anvil-dev--audit-scan-arglist-strip' (auto-discovery
path) and `anvil-dev-release-audit' (scope-limited path).  Passing
an empty list short-circuits cleanly to no findings — that is how
a scope filter that matches zero files becomes a zero-finding
scan instead of reverting to full-tree discovery."
  (let (findings)
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward anvil-dev--audit-wrapper-regex nil t)
          (let ((name  (match-string 1))
                (args  (match-string 2))
                (line  (line-number-at-pos (match-beginning 0))))
            (when (and (anvil-dev--audit-wrapper-name-p name)
                       (string-match-p "\\`_\\|[ \t]_" args))
              (push (list :file (file-name-nondirectory file)
                          :line line
                          :defun name
                          :args args)
                    findings))))))
    (nreverse findings)))

(defun anvil-dev--audit-scan-arglist-strip (root)
  "Scan ROOT for MCP tool wrappers with `(_arg)`-style arguments.
Returns a list of plists `(:file NAME :line N :defun SYM :args ARGS)'
for every wrapper whose first argument begins with an underscore
— that is the exact Emacs 30 arglist-strip regression that
broke v0.2.0 (issue #9)."
  (anvil-dev--audit-scan-arglist-strip-in-files
   (anvil-dev--audit-module-files root)))

(defun anvil-dev--audit-wrapper-has-real-args-p (args)
  "Return non-nil when the wrapper ARG-STRING describes a real parameter.
Empty, `&rest', `&optional _x', and `_foo' forms are treated as
no-arg; anything else counts as a real argument that ought to be
documented in an `MCP Parameters:' section."
  (let ((trimmed (string-trim args)))
    (cond
     ((string-empty-p trimmed) nil)
     ((string-match-p "\\`&rest[ \t]" trimmed) nil)
     ((string-match-p "\\`_" trimmed) nil)
     ((string-match-p "\\`&optional[ \t]+_" trimmed) nil)
     (t t))))

(defun anvil-dev--audit-scan-missing-params-section-in-files (files)
  "Scan explicit FILES list for wrappers with real args but no
`MCP Parameters:' docstring section.  See the kind-mate
`-arglist-strip-in-files' for the rationale behind accepting an
explicit list instead of auto-discovering from a project root."
  (let (findings)
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward anvil-dev--audit-wrapper-regex nil t)
          (let* ((name  (match-string 1))
                 (args  (match-string 2))
                 (start (match-beginning 0))
                 (line  (line-number-at-pos start)))
            (when (and (anvil-dev--audit-wrapper-name-p name)
                       (anvil-dev--audit-wrapper-has-real-args-p args))
              (let* ((form-end
                      (save-excursion
                        (goto-char start)
                        (condition-case nil
                            (progn (forward-sexp) (point))
                          (error (point-max))))))
                (unless (save-excursion
                          (goto-char start)
                          (re-search-forward "MCP Parameters:" form-end t))
                  (push (list :file (file-name-nondirectory file)
                              :line line
                              :defun name
                              :args args)
                        findings))))))))
    (nreverse findings)))

(defun anvil-dev--audit-scan-missing-params-section (root)
  "Scan ROOT for MCP tool wrappers that take real args but lack
an `MCP Parameters:' docstring section.  Returns a list of
plists `(:file NAME :line N :defun SYM :args ARGS)'."
  (anvil-dev--audit-scan-missing-params-section-in-files
   (anvil-dev--audit-module-files root)))

(defconst anvil-dev--audit-plist-return-exempt-marker
  ";;; anvil-audit: tools-wrapped-at-registration"
  "File-header comment that exempts a file from the plist-return scanner.
Modules that intentionally return plists from `--tool-*' bodies and
wrap them at registration time (e.g. anvil-orchestrator's
`anvil-orchestrator--encode-handler' pattern) must include this
line in the first 2 KB of the file.  See the scanner docstring for
the one-line comment format.")

(defun anvil-dev--audit-file-exempts-plist-return-p (file)
  "Return non-nil when FILE carries the plist-return exemption marker.
The marker must appear within the first 2 KB of the file on a line
starting with `anvil-dev--audit-plist-return-exempt-marker'."
  (with-temp-buffer
    (insert-file-contents file nil 0 2000)
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote anvil-dev--audit-plist-return-exempt-marker))
     nil t)))

(defconst anvil-dev--audit-sequencing-forms
  '(let let* progn
    save-excursion save-restriction save-match-data save-current-buffer
    unwind-protect
    when unless
    with-current-buffer with-temp-buffer with-temp-file
    anvil-server-with-error-handling)
  "Forms whose logical terminal value is the last of their body.
Used by `anvil-dev--audit-terminal-form' to unwrap trivial
sequencing constructs when deciding whether a `--tool-*' defun
actually returns a plist literal.")

(defun anvil-dev--audit-terminal-form (form)
  "Return the logical terminal value of FORM.
Unwraps `let' / `let*' / `progn' / `save-*' / `unwind-protect' /
`when' / `unless' / `anvil-server-with-error-handling' /
`with-current-buffer' / `with-temp-{buffer,file}' recursively so
that e.g. `(let (...) (list :K ...))' resolves to `(list :K ...)'.
Non-sequencing cons forms return themselves; atoms return themselves."
  (if (and (consp form)
           (memq (car form) anvil-dev--audit-sequencing-forms))
      (anvil-dev--audit-terminal-form (car (last form)))
    form))

(defun anvil-dev--audit-defun-terminal-form (defun-form)
  "Return the logical terminal form of DEFUN-FORM's body.
DEFUN-FORM is a parsed `(defun NAME ARGS ...BODY)'.  The leading
docstring and any `(declare ...)' / `(interactive ...)' directives
are skipped, then `anvil-dev--audit-terminal-form' is applied to
the last remaining body form."
  (let ((body (cdddr defun-form)))
    (while (and body
                (let ((f (car body)))
                  (or (stringp f)
                      (and (consp f)
                           (memq (car f) '(declare interactive))))))
      (setq body (cdr body)))
    (anvil-dev--audit-terminal-form (car (last body)))))

(defun anvil-dev--audit-plist-return-form-p (form)
  "Non-nil when FORM is a `(list :KEYWORD ...)' literal.
Used as the end-criterion in the plist-return scanner."
  (and (consp form)
       (eq (car form) 'list)
       (keywordp (cadr form))))

(defun anvil-dev--audit-scan-plist-return-in-files (files)
  "Scan FILES for `--tool-*' wrappers whose body terminates in a plist literal.

An MCP tool handler must return a string or nil per the
`anvil-server' contract; returning a raw Lisp plist trips a
runtime error on every MCP call (the class of bug v0.3.1 shipped
a hotfix for).  The scanner reads each `--tool-*' defun as a
sexp, unwraps common sequencing forms (let / progn / save-* /
with-error-handling / with-temp-buffer / ...) and flags the defun
when the logical terminal form is `(list :KEYWORD ...)'.

Files tagged with `anvil-dev--audit-plist-return-exempt-marker' in
their first 2 KB are skipped wholesale — the exemption is the
supported way for a module to declare that its tool bodies
intentionally return rich plists because something else encodes
them at registration time.

Returns a list of plists `(:file NAME :line N :defun SYM)'."
  (let (findings)
    (dolist (file files)
      (unless (anvil-dev--audit-file-exempts-plist-return-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward
                  "^(defun \\([a-zA-Z0-9-]+--tool-[a-zA-Z0-9-]+\\)"
                  nil t)
            (let ((name  (match-string 1))
                  (start (match-beginning 0))
                  (line  (line-number-at-pos (match-beginning 0))))
              (when (anvil-dev--audit-wrapper-name-p name)
                (let ((form (save-excursion
                              (goto-char start)
                              (ignore-errors (read (current-buffer))))))
                  (when (and form (consp form) (eq (car form) 'defun))
                    (let ((term (anvil-dev--audit-defun-terminal-form form)))
                      (when (anvil-dev--audit-plist-return-form-p term)
                        (push (list :file (file-name-nondirectory file)
                                    :line line
                                    :defun name)
                              findings)))))))))))
    (nreverse findings)))

(defun anvil-dev--audit-scan-plist-return (root)
  "Scan ROOT for MCP tool wrappers that terminate with a plist literal.
Returns a list of plists `(:file NAME :line N :defun SYM)'."
  (anvil-dev--audit-scan-plist-return-in-files
   (anvil-dev--audit-module-files root)))

(defcustom anvil-dev-audit-issue-fix-commit-depth 20
  "How many recent commits `anvil-dev--audit-scan-issue-fix-without-test' inspects.
20 commits covers a typical two-week anvil push cadence.  Set to a
larger value before the release cut, or to 0 to disable the scan
(useful in environments where git history is shallow or absent, e.g.
a fresh CI clone with fetch-depth=1)."
  :type 'integer
  :group 'anvil-dev)

(defconst anvil-dev--audit-issue-fix-regex
  "\\(?:Fixes\\|Closes\\|Resolves\\) +#\\([0-9]+\\)"
  "Regex that detects an issue-closing commit-message keyword.
Case-sensitive on purpose — matches the GitHub-canonical verbs
that auto-close the referenced issue.  Lower-cased variants
(\"fix #12\") do not auto-close on GitHub and are intentionally
not flagged; anvil commits that truly close an issue spell the
verb in title case by convention.")

(defun anvil-dev--audit--git-available-p (root)
  "Return non-nil when ROOT looks like a git worktree with history."
  (and (file-directory-p (expand-file-name ".git" root))
       (executable-find "git")))

(defun anvil-dev--audit--git-commit-info (root sha)
  "Return (:sha :subject :body :files) for SHA inside ROOT.
Uses two `git show' calls so parsing stays format-agnostic:
`--format=%s%n%b' for the human-readable message and a separate
`--name-only' call for the file list.  Slightly chattier than one
big `git log' invocation but much easier to keep correct when
commit messages contain arbitrary text (including blank lines)."
  (let ((default-directory (file-name-as-directory root)))
    (let ((msg (with-temp-buffer
                 (when (eq 0 (call-process "git" nil t nil "show" "-s"
                                           "--format=%s%n%b" sha))
                   (buffer-string))))
          (files (with-temp-buffer
                   (when (eq 0 (call-process "git" nil t nil "show"
                                             "--name-only" "--format=" sha))
                     (split-string (buffer-string) "\n" t "[ \t]+")))))
      (when msg
        (let* ((lines (split-string msg "\n" nil))
               (subject (car lines))
               (body (mapconcat #'identity (cdr lines) "\n")))
          (list :sha sha
                :subject (or subject "")
                :body (or body "")
                :files files))))))

(defun anvil-dev--audit--git-log-commits (root depth)
  "Return DEPTH most recent commits as a list of (:sha :subject :body :files).
Returns nil when ROOT is not a usable git worktree or DEPTH <= 0."
  (when (and (anvil-dev--audit--git-available-p root)
             (integerp depth) (> depth 0))
    (let ((default-directory (file-name-as-directory root)))
      (let ((shas (with-temp-buffer
                    (when (eq 0 (call-process "git" nil t nil "log"
                                              (format "-n%d" depth)
                                              "--format=%H"))
                      (split-string (buffer-string) "\n" t "[ \t]+")))))
        (delq nil
              (mapcar (lambda (sha)
                        (anvil-dev--audit--git-commit-info root sha))
                      shas))))))

(defun anvil-dev--audit-scan-issue-fix-without-test (root)
  "Scan the last N commits (N=`anvil-dev-audit-issue-fix-commit-depth').
Flag any commit whose subject or body contains `Fixes #N', `Closes
#N', or `Resolves #N' but did not add / modify any file under
`tests/'.  Catches the shape of the v0.3.1 hotfix (37fcc52) where a
hastily-shipped fix skipped the regression guard.  Does not catch the
harder case where a test was added but misses the reporter's
scenario — that one needs human review.

Returns a list of plists `(:sha SHA :issue N :subject STR)'."
  (let ((commits (anvil-dev--audit--git-log-commits
                  root anvil-dev-audit-issue-fix-commit-depth))
        findings)
    (dolist (c commits)
      (let* ((sha (plist-get c :sha))
             (subject (plist-get c :subject))
             (body (plist-get c :body))
             (text (concat subject "\n" body))
             (files (plist-get c :files))
             (issue (when (and (stringp text)
                               (string-match anvil-dev--audit-issue-fix-regex
                                             text))
                      (string-to-number (match-string 1 text))))
             (has-test (cl-some (lambda (f)
                                  (and (stringp f)
                                       (string-match-p "\\`tests/" f)))
                                files)))
        (when (and issue (not has-test))
          (push (list :sha (substring sha 0 (min 10 (length sha)))
                      :issue issue
                      :subject subject)
                findings))))
    (nreverse findings)))

(defun anvil-dev--audit-design-doc-status (file)
  "Return cons (FIRST-LINE . FULL-BODY) of FILE's `* STATUS' section.
FIRST-LINE is the first non-blank, non-drawer line trimmed and stripped of
`~...~' verbatim delimiters (used as the human-readable summary in
reports).  FULL-BODY is the whole STATUS section text used to detect
status keywords that may appear on a later line such as
`DRAFT → SHIPPED' progressions.  Returns nil when the file has no
STATUS heading."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward "^\\* STATUS" nil t)
      (forward-line 1)
      (let* ((start (point))
             (end (save-excursion
                    (or (and (re-search-forward "^\\* " nil t)
                             (match-beginning 0))
                        (point-max))))
             (body (buffer-substring-no-properties start end))
             first-line)
        (save-excursion
          (goto-char start)
          (catch 'found
            (while (< (point) end)
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
                (unless (or (string-match-p "\\`[[:space:]]*\\'" line)
                            (string-match-p "\\`[[:space:]]*:PROPERTIES:" line)
                            (string-match-p "\\`[[:space:]]*:ID:" line)
                            (string-match-p "\\`[[:space:]]*:END:" line))
                  (setq first-line
                        (string-trim
                         (replace-regexp-in-string
                          "\\`[[:space:]]*~\\|~[[:space:]]*\\'" "" line)))
                  (throw 'found nil)))
              (forward-line 1))))
        (when first-line (cons first-line body))))))

(defun anvil-dev--audit-status-shipped-p (body)
  "Return non-nil when STATUS BODY counts as non-blocking for master gate.
Looks for the all-caps status keywords `SHIPPED' (work merged),
`DEFERRED' (postponed by decision), or `AUDIT' (research-only memo)
anywhere in the body, with case-sensitive matching so that incidental
narrative words like \"the shipped Phase 1 broker\" do not satisfy
the gate.  Multi-step progressions like `~DRAFT~ → ~APPROVED~ →
~SHIPPED~' therefore pass once the final keyword appears anywhere
in the body."
  (and body
       (let ((case-fold-search nil))
         (string-match-p "\\bSHIPPED\\b\\|\\bDEFERRED\\b\\|\\bAUDIT\\b" body))))

(defun anvil-dev--audit-scan-design-docs (root)
  "Scan ROOT/docs/design/*.org for STATUS sections that block master merge.
A doc is blocking when its STATUS body contains none of `SHIPPED',
`DEFERRED', or `AUDIT' (i.e. still DRAFT / APPROVED / mid-phase).
Returns a list of plists `(:file NAME :status LINE)' in lexical
order, where LINE is the human-readable first line of STATUS used
in reports."
  (let ((design-dir (expand-file-name "docs/design" root))
        findings)
    (when (file-directory-p design-dir)
      (dolist (file (sort (directory-files design-dir t "\\.org\\'" t)
                          #'string-lessp))
        (let ((base (file-name-nondirectory file)))
          (unless (equal base "README.org")
            (let ((status (anvil-dev--audit-design-doc-status file)))
              (cond
               ((null status)
                (push (list :file base :status "(no STATUS section)")
                      findings))
               ((not (anvil-dev--audit-status-shipped-p (cdr status)))
                (push (list :file base :status (car status)) findings))))))))
    (nreverse findings)))

(defun anvil-dev--audit-filter-by-scope (files scope)
  "Restrict FILES to entries under SCOPE (file path or directory).
SCOPE nil returns FILES unchanged.  Non-existent SCOPE returns nil."
  (cond
   ((null scope) files)
   ((not (file-exists-p scope)) nil)
   ((file-regular-p scope)
    (let ((abs (expand-file-name scope)))
      (cl-remove-if-not (lambda (f) (equal (expand-file-name f) abs))
                        files)))
   ((file-directory-p scope)
    (let ((dir (file-name-as-directory (expand-file-name scope))))
      (cl-remove-if-not (lambda (f)
                          (string-prefix-p dir (expand-file-name f)))
                        files)))
   (t nil)))

(defun anvil-dev--audit-scan-unused-tools (days)
  "Return tool-ids whose last dispatch was more than DAYS ago.
Reads the Doc 34 Phase C counter from `anvil-state' namespace
`discovery-usage'.  Returns a list of plists
`(:id :reason :last-called :days-ago :count)'.  :reason is
`never-called' when the tool has no counter row, `stale' when
the counter exists but is older than DAYS.  Returns nil (no
findings) when anvil-state / anvil-discovery are not loaded so
the audit still runs on machines where Phase C is not enabled."
  (when (and (featurep 'anvil-state)
             (featurep 'anvil-discovery)
             (fboundp 'anvil-state-get))
    (condition-case nil
        (let* ((ns      (bound-and-true-p anvil-discovery--usage-ns))
               (now     (truncate (float-time)))
               (cutoff  (- now (* days 86400)))
               registered results)
          (when (hash-table-p (bound-and-true-p anvil-server--tools))
            (maphash
             (lambda (_sid table)
               (when (hash-table-p table)
                 (maphash (lambda (id _t) (push id registered)) table)))
             anvil-server--tools))
          (setq registered (cl-remove-duplicates registered :test #'equal))
          (dolist (id registered)
            (let ((entry (and ns (anvil-state-get id :ns ns :default nil))))
              (cond
               ((null entry)
                (push (list :id id :reason 'never-called) results))
               (t
                (let ((last (plist-get entry :last-called)))
                  (when (and (numberp last) (< last cutoff))
                    (push (list :id id
                                :reason 'stale
                                :last-called last
                                :days-ago (/ (- now last) 86400)
                                :count (or (plist-get entry :count) 0))
                          results)))))))
          (sort results
                (lambda (a b)
                  (string-lessp (plist-get a :id)
                                (plist-get b :id)))))
      (error nil))))

(defcustom anvil-dev-audit-secrets-enabled t
  "When non-nil, `anvil-dev-release-audit' runs a gitleaks secret scan.
Requires the `gitleaks' binary (https://github.com/gitleaks/gitleaks)
on `PATH'.  The scan degrades to nil when the binary is unavailable,
so the audit still runs on machines without gitleaks installed."
  :type 'boolean
  :group 'anvil-dev)

(defun anvil-dev--audit--gitleaks-available-p (root)
  "Return non-nil when ROOT is a git worktree and `gitleaks' is on PATH.
Honours `anvil-dev-audit-secrets-enabled'."
  (and anvil-dev-audit-secrets-enabled
       (anvil-dev--audit--git-available-p root)
       (executable-find "gitleaks")))

(defun anvil-dev--audit-scan-secrets (root)
  "Run `gitleaks detect' in ROOT and return a list of finding plists.
Each plist: (:file REL :line INT :rule ID :fingerprint ID).  The
scan passes `--redact' so the actual secret value never enters the
audit output — only the rule-id and fingerprint (which `gitleaks'
uses for its allowlist mechanism) are retained.

Returns nil when gitleaks is unavailable or no leaks are found.
Never signals — invocation or parse failures degrade to nil so a
broken gitleaks install cannot block the rest of the audit."
  (when (anvil-dev--audit--gitleaks-available-p root)
    (let ((default-directory (file-name-as-directory root))
          (report-file (make-temp-file "anvil-gitleaks-" nil ".json")))
      (unwind-protect
          (progn
            (call-process "gitleaks" nil nil nil
                          "detect"
                          "--no-banner"
                          "--exit-code" "0"
                          "--redact"
                          "--report-format" "json"
                          "--report-path" report-file)
            (condition-case nil
                (let ((findings
                       (with-temp-buffer
                         (insert-file-contents report-file)
                         (when (> (buffer-size) 0)
                           (json-parse-buffer :object-type 'plist
                                              :array-type 'list
                                              :null-object nil
                                              :false-object nil)))))
                  (when (listp findings)
                    (mapcar
                     (lambda (f)
                       (list :file        (or (plist-get f :File) "")
                             :line        (or (plist-get f :StartLine) 0)
                             :rule        (or (plist-get f :RuleID) "")
                             :fingerprint (or (plist-get f :Fingerprint) "")))
                     findings)))
              (error nil)))
        (ignore-errors (delete-file report-file))))))

;;;; --- fixture realism scanner --------------------------------------------
;;
;; T70 (anvil-orchestrator-routing) shipped with a "test fixture realism"
;; bug: production stored its provider registry in
;; `(make-hash-table :test 'eq)' but the test let-bound the same variable
;; to a quoted alist `'((claude) (codex) (ollama))'.  ERT passed because
;; the routing code-path under test happened to also use `mapcar #'car'
;; — the real production caller signalled `wrong-type-argument'.  This
;; was a false PASS caused by a fixture whose *shape* did not mirror the
;; production data structure.
;;
;; This scanner is the structured guard against that class of bug.  It
;; reads each anvil-*.el source file, classifies the *declared shape* of
;; each `defvar' / `defconst' (only when the init form is one of a small
;; high-confidence set: `make-hash-table' / `make-vector' / vector
;; literal), then walks every tests/anvil-*-test.el looking for `let' /
;; `let*' / `cl-letf' / `cl-letf*' bindings of those same symbols whose
;; bound value form classifies to a different shape.  Function-call
;; values (the post-T70-fix pattern of binding via a helper that builds
;; the right shape) classify as `:unknown' and are intentionally NOT
;; flagged — the scanner aims for high precision so it can be a CI gate.

(defconst anvil-dev--audit-realism-tracked-init-cars
  '(make-hash-table make-vector vector)
  "`car' symbols whose `defvar' / `defconst' init forms we classify.
Any other init form leaves the symbol unrecorded — the scanner only
fires on symbols whose production shape we are confident about.")

(defconst anvil-dev--audit-realism-exempt-marker
  ";;; anvil-audit: fixture-realism-exempt"
  "File-header comment that exempts a whole test file from the realism scan.
Tests that intentionally exercise a back-compat branch by binding a
production var to a non-canonical shape (e.g. T70's
`candidates-from-alist-fixture' which proves the alist fallback in
`anvil-orchestrator-routing--candidates') must include this line in
the first 2 KB of the file.  Coarse by design — opt-out is per-file
rather than per-binding because per-binding requires preserving
comments through `read', which `read' does not do.")

(defun anvil-dev--audit-realism-file-exempt-p (file)
  "Return non-nil when FILE carries the realism-exemption marker.
The marker must appear within the first 2 KB of the file on a line
starting with `anvil-dev--audit-realism-exempt-marker'."
  (with-temp-buffer
    (insert-file-contents file nil 0 2000)
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote anvil-dev--audit-realism-exempt-marker))
     nil t)))

(defun anvil-dev--audit-realism-classify-init (form)
  "Classify a `defvar' / `defconst' INIT FORM into a shape symbol.
Returns one of `:hash-table', `:vector', or nil (unknown / not a
shape we track).  See `anvil-dev--audit-realism-tracked-init-cars'."
  (cond
   ((and (consp form) (eq (car form) 'make-hash-table)) :hash-table)
   ((and (consp form) (eq (car form) 'make-vector))     :vector)
   ((and (consp form) (eq (car form) 'vector))          :vector)
   ((vectorp form)                                       :vector)
   (t nil)))

(defun anvil-dev--audit-realism-classify-value (form)
  "Classify a let-binding VALUE FORM into a runtime shape symbol.
Returns one of:
  :alist          - quoted list whose first element is a cons-pair
                    `(SYM . X)' or a cons-headed list (the alist
                    literal pattern that broke T70).
  :alist-runtime  - `(list (cons ...) ...)' — the constructor form
                    of an alist.
  :hash-table     - `(make-hash-table ...)'.
  :vector         - vector literal `[...]' or `(make-vector ...)'
                    or `(vector ...)'.
  :nil            - the literal nil (or `(quote nil)') — empty bind.
  :unknown        - everything else (function calls, symbols, numbers,
                    strings, ...).  NOT flagged.

Quoted-form recognition handles both the `'(...)' reader macro and
the explicit `(quote ...)' form, since `read' normalises them.

The classifier is deliberately conservative: anything not in the
high-confidence set returns `:unknown' so that fixture builders
that legitimately delegate to a helper (like the post-T70 fix's
`anvil-orchestrator-routing-test--make-providers') do not produce
false positives."
  (cond
   ((null form) :nil)
   ((and (consp form) (eq (car form) 'quote))
    (let ((q (cadr form)))
      (cond
       ((null q) :nil)
       ((vectorp q) :vector)
       ((and (consp q)
             ;; A list whose every element is a cons (or a cons-headed
             ;; list, since `(SYM VAL)` reads as `(SYM . (VAL))') is
             ;; an alist literal.  Single-element non-cons lists
             ;; (like `'(x y z)') are `:unknown' to keep precision.
             (cl-every (lambda (e) (consp e)) q))
        :alist)
       (t :unknown))))
   ((vectorp form) :vector)
   ((and (consp form) (eq (car form) 'make-hash-table)) :hash-table)
   ((and (consp form) (eq (car form) 'make-vector))     :vector)
   ((and (consp form) (eq (car form) 'vector))          :vector)
   ((and (consp form) (eq (car form) 'list)
         (cl-every (lambda (e)
                     (and (consp e) (eq (car e) 'cons)))
                   (cdr form)))
    :alist-runtime)
   (t :unknown)))

(defun anvil-dev--audit-realism-shapes-conflict-p (declared bound)
  "Non-nil when DECLARED production shape conflicts with BOUND test shape.
DECLARED is one of `:hash-table' / `:vector'.  BOUND is one of
the keywords returned by `anvil-dev--audit-realism-classify-value'.

Conflicts (== flagged):
  hash-table ↔ alist        (the T70 case)
  hash-table ↔ alist-runtime
  hash-table ↔ vector
  vector ↔ alist
  vector ↔ alist-runtime
  vector ↔ hash-table

Non-conflicts (== clean):
  same shape on both sides
  bound is :unknown (function-call delegate)
  bound is :nil (empty bind, harmless)"
  (cond
   ((memq bound '(:unknown :nil)) nil)
   ((eq declared bound) nil)
   ((and (eq declared :hash-table)
         (memq bound '(:alist :alist-runtime :vector))) t)
   ((and (eq declared :vector)
         (memq bound '(:alist :alist-runtime :hash-table))) t)
   (t nil)))

(defun anvil-dev--audit-realism-read-forms (file)
  "Read every top-level form in FILE and return a list of them.
Forms that fail to `read' (e.g. unbalanced sexps in a partial file)
are dropped silently — the scanner is read-only and must never
abort the audit on a syntactically broken sibling test file."
  (let (forms)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil)
        (error nil)))
    (nreverse forms)))

(defun anvil-dev--audit-realism-collect-shape-map-from-forms (forms shape-map)
  "Walk top-level FORMS, recording defvar/defconst shapes into SHAPE-MAP.
SHAPE-MAP is a hash-table keyed by symbol, value = shape keyword.
Only `defvar' / `defconst' forms with a tracked init form get an
entry — see `anvil-dev--audit-realism-classify-init'.  When the
same symbol appears more than once with different shapes the first
entry wins (matches Emacs's own `defvar' precedence: a later
re-defvar in the same file is unusual, and we want the canonical
init expression to set policy)."
  (dolist (form forms)
    (when (and (consp form)
               (memq (car form) '(defvar defconst))
               (>= (length form) 3))
      (let* ((sym   (nth 1 form))
             (init  (nth 2 form))
             (shape (anvil-dev--audit-realism-classify-init init)))
        (when (and shape (symbolp sym) (not (gethash sym shape-map)))
          (puthash sym shape shape-map))))))

(defun anvil-dev--audit-realism-build-shape-map (source-files)
  "Return a hash-table mapping SOURCE-FILES' production var → declared shape.
SOURCE-FILES is the production module list (i.e. the same list
`anvil-dev--audit-module-files' returns).  Test files are not
scanned here — only real anvil-*.el source files contribute to the
shape map."
  (let ((shape-map (make-hash-table :test 'eq)))
    (dolist (file source-files)
      (let ((forms (anvil-dev--audit-realism-read-forms file)))
        (anvil-dev--audit-realism-collect-shape-map-from-forms
         forms shape-map)))
    shape-map))

(defconst anvil-dev--audit-realism-let-heads
  '(let let* cl-letf cl-letf*)
  "Forms whose first list-of-bindings argument we inspect for shape mismatch.
`cl-letf' / `cl-letf*' are included because anvil tests routinely
use them to monkey-patch globals; the binding shape rules are the
same as plain `let'.")

(defun anvil-dev--audit-realism-binding-pair (binding)
  "Return (SYM . VALUE-FORM) for BINDING in a let-style binding list.
BINDING may be a symbol (no value form — bound to nil), or a
two-element list `(SYM VALUE)'.  cl-letf binding shapes that wrap
the head in a `(symbol-function ...)' or `((function ...))' form
do not match a tracked production variable (which is always a plain
symbol) and therefore harmlessly classify as `nil' — they never
generate findings."
  (cond
   ((symbolp binding) (cons binding nil))
   ((and (consp binding) (symbolp (car binding)))
    (cons (car binding) (cadr binding)))
   (t (cons nil nil))))

(defun anvil-dev--audit-realism-walk-form (form shape-map findings-cell file)
  "Recursively walk FORM looking for shape-mismatched let-bindings.
SHAPE-MAP is the production var → declared shape hash-table built
by `anvil-dev--audit-realism-build-shape-map'.  FINDINGS-CELL is a
mutable cons whose `car' accumulates findings.  FILE is the
basename used to label findings.

Findings are pushed as plists `(:file FILE :var SYM :declared SHAPE
:bound SHAPE)'.  Line numbers are NOT recorded — the scanner reads
forms via `read' which discards source positions, and the
trade-off (precision over location) is acceptable since the
mismatch is uniquely identified by `(file, var, shapes)' triple.

Recursion is safe for improper / dotted lists (e.g. cons-pair
literals `(a . 1)' read via `read'): the walker descends only
through the `car' / `cdr' chain manually instead of using
`dolist', which would signal `wrong-type-argument' on a non-list
tail.  The walker also stops at `quote' forms — the contents of
a quoted literal are data, not code, and cannot contain a runtime
let-binding."
  (when (consp form)
    (let ((head (car form)))
      (cond
       ;; Quoted data — never contains a runtime let.  Skip the body
       ;; entirely so we do not waste cycles or trip on dotted pairs.
       ((eq head 'quote) nil)
       (t
        (when (memq head anvil-dev--audit-realism-let-heads)
          (let ((bindings (cadr form)))
            (when (listp bindings)
              (dolist (b bindings)
                (let* ((pair  (anvil-dev--audit-realism-binding-pair b))
                       (sym   (car pair))
                       (value (cdr pair))
                       (declared (and sym (gethash sym shape-map))))
                  (when declared
                    (let ((bound (anvil-dev--audit-realism-classify-value
                                  value)))
                      (when (anvil-dev--audit-realism-shapes-conflict-p
                             declared bound)
                        (push (list :file file
                                    :var sym
                                    :declared declared
                                    :bound bound)
                              (car findings-cell))))))))))
        ;; Manually walk the cons spine so improper / dotted lists do
        ;; not blow up `dolist'.
        (let ((cell form))
          (while (consp cell)
            (anvil-dev--audit-realism-walk-form
             (car cell) shape-map findings-cell file)
            (setq cell (cdr cell)))
          ;; Handle a final non-nil non-cons cdr (the dotted tail).
          (when (and cell (not (consp cell)))
            (anvil-dev--audit-realism-walk-form
             cell shape-map findings-cell file))))))))

(defun anvil-dev--audit-realism-test-files (root)
  "Return the tests/anvil-*-test.el files under ROOT.
Returns nil when ROOT/tests does not exist."
  (let ((dir (expand-file-name "tests" root)))
    (when (file-directory-p dir)
      (directory-files dir t "\\`anvil-.*-test\\.el\\'" t))))

(defun anvil-dev--audit-scan-fixture-realism (root)
  "Scan ROOT for test fixtures whose let-bind shape conflicts with production.
Returns a list of plists `(:file NAME :var SYM :declared SHAPE
:bound SHAPE)'.  The `:declared' shape comes from a `defvar' /
`defconst' init form in some `anvil-*.el' under ROOT; the
`:bound' shape is the classification of the test-side value
form.  The classifier is conservative: helper-call values
(`:unknown') are never flagged.

Test files tagged with `anvil-dev--audit-realism-exempt-marker' in
their first 2 KB are skipped wholesale — the supported way for a
back-compat-fixture file to opt out (see the marker docstring for
the rationale)."
  (let* ((source-files (anvil-dev--audit-module-files root))
         (test-files   (anvil-dev--audit-realism-test-files root))
         (shape-map    (anvil-dev--audit-realism-build-shape-map
                        source-files))
         (findings-cell (cons nil nil)))
    (dolist (file test-files)
      (unless (anvil-dev--audit-realism-file-exempt-p file)
        (let ((forms (anvil-dev--audit-realism-read-forms file))
              (base  (file-name-nondirectory file)))
          (dolist (form forms)
            (anvil-dev--audit-realism-walk-form
             form shape-map findings-cell base)))))
    (nreverse (car findings-cell))))

(cl-defun anvil-dev-release-audit (&optional project-dir &key scope unused-since)
  "Audit the anvil tree at PROJECT-DIR for pre-release hazards.

Runs seven cheap scanners (see the `release audit' section for
details) and returns a plist:

  :arglist-strip    — list of wrapper plists hit by the Emacs 30
                      underscore-strip regression
  :missing-params   — list of wrapper plists with real args but
                      no `MCP Parameters:' docstring section
  :plist-return     — list of wrapper plists whose body ends with
                      a `(list :K ...)' form (MCP contract violation
                      unless the file opts out via the
                      `tools-wrapped-at-registration' marker)
  :issue-fix-no-test — list of commit plists whose message closes
                      an issue but whose diff does NOT touch
                      `tests/' (guards against the 37fcc52 pattern —
                      ship a fix without a regression test).  Skipped
                      when git history is unavailable (shallow clone).
  :non-shipped-docs — list of `(:file :status)' plists for design
                      docs whose STATUS line lacks `SHIPPED'
  :secrets          — list of `(:file :line :rule :fingerprint)'
                      plists for secrets detected by `gitleaks'.
                      Skipped when gitleaks is not installed or
                      `anvil-dev-audit-secrets-enabled' is nil.
  :fixture-realism  — list of `(:file :var :declared :bound)'
                      plists for tests that let-bind a production
                      variable to a value of the wrong shape (T70
                      hash-table-vs-alist false-PASS pattern).
  :unused-tools     — Doc 34 Phase C.  Populated only when
                      UNUSED-SINCE is a positive integer; list of
                      plists `(:id :reason :last-called :days-ago
                      :count)' for tools whose counter is stale
                      (>UNUSED-SINCE days) or absent.  Requires
                      `anvil-state' + `anvil-discovery' loaded.
                      Advisory — does not affect `:clean-p'.
  :clean-p          — t iff the seven release-blocker scans returned
                      empty (unused-tools excluded by design)
  :root             — absolute directory that was audited
  :scope            — SCOPE value when supplied, else nil
  :unused-since     — UNUSED-SINCE value when supplied, else nil
  :audited-at       — ISO timestamp when the scan ran

:SCOPE limits the source scanners (arglist-strip / missing-params /
plist-return) to a single file or directory path.  When SCOPE names
a regular file, only that file is audited; when it names a
directory, files under it are audited.  The design-docs, issue-fix,
fixture-realism, and unused-tools scanners always run against the
whole project / registry state.

:UNUSED-SINCE N (integer days) activates the Phase C unused-tools
scanner.  Typical values: 14 (biweekly review) or 30 (monthly
deprecation candidate list).  Omit or pass nil to skip."
  (interactive)
  (let* ((root (or project-dir (anvil-dev--audit-default-root)))
         (_    (unless (and root (file-directory-p root))
                 (error "anvil-dev-release-audit: no anvil root found (tried %S)"
                        root)))
         (all-files      (anvil-dev--audit-module-files root))
         (scanner-files  (if scope
                             (anvil-dev--audit-filter-by-scope
                              all-files scope)
                           all-files))
         (arglist-strip  (anvil-dev--audit-scan-arglist-strip-in-files
                          scanner-files))
         (missing-params (anvil-dev--audit-scan-missing-params-section-in-files
                          scanner-files))
         (plist-return   (anvil-dev--audit-scan-plist-return-in-files
                          scanner-files))
         (issue-fix-no-test (anvil-dev--audit-scan-issue-fix-without-test root))
         (non-shipped    (anvil-dev--audit-scan-design-docs root))
         (secrets        (anvil-dev--audit-scan-secrets root))
         (fixture-realism (anvil-dev--audit-scan-fixture-realism root))
         (unused-tools   (when (and (integerp unused-since) (> unused-since 0))
                           (anvil-dev--audit-scan-unused-tools unused-since)))
         (clean-p        (and (null arglist-strip)
                              (null missing-params)
                              (null plist-return)
                              (null issue-fix-no-test)
                              (null non-shipped)
                              (null secrets)
                              (null fixture-realism)))
         (result
          (list :arglist-strip arglist-strip
                :missing-params missing-params
                :plist-return plist-return
                :issue-fix-no-test issue-fix-no-test
                :non-shipped-docs non-shipped
                :secrets secrets
                :fixture-realism fixture-realism
                :unused-tools unused-tools
                :clean-p clean-p
                :root (file-name-as-directory (expand-file-name root))
                :scope scope
                :unused-since unused-since
                :audited-at (format-time-string "%Y-%m-%d %H:%M:%S"))))
    (when (called-interactively-p 'any)
      (message "%s" (anvil-dev--audit-format-report result)))
    result))

(defun anvil-dev--audit-format-findings (header findings formatter)
  "Render FINDINGS as an indented bullet list with HEADER.
FORMATTER is a function called with each finding plist; it must
return a one-line string."
  (when findings
    (concat
     (format "%s (%d):\n" header (length findings))
     (mapconcat (lambda (f) (format "    %s" (funcall formatter f)))
                findings
                "\n")
     "\n\n")))

(defun anvil-dev--audit-format-report (result)
  "Render RESULT as a human-readable release audit report."
  (let ((arglist (plist-get result :arglist-strip))
        (params  (plist-get result :missing-params))
        (plists  (plist-get result :plist-return))
        (issue-fix (plist-get result :issue-fix-no-test))
        (docs    (plist-get result :non-shipped-docs))
        (secrets (plist-get result :secrets))
        (realism (plist-get result :fixture-realism))
        (clean-p (plist-get result :clean-p)))
    (concat
     (format "anvil release audit — %s\n" (plist-get result :audited-at))
     (format "root: %s\n\n"                (plist-get result :root))
     (if clean-p
         "OK — no hazards found, release-ready from this audit's POV.\n"
       (concat
        (anvil-dev--audit-format-findings
         "FAIL arglist-strip (Emacs 30 `_arg' underscore-strip)"
         arglist
         (lambda (f)
           (format "%s:%d  %s  (args=(%s))"
                   (plist-get f :file)
                   (plist-get f :line)
                   (plist-get f :defun)
                   (plist-get f :args))))
        (anvil-dev--audit-format-findings
         "FAIL missing `MCP Parameters:' section"
         params
         (lambda (f)
           (format "%s:%d  %s  (args=(%s))"
                   (plist-get f :file)
                   (plist-get f :line)
                   (plist-get f :defun)
                   (plist-get f :args))))
        (anvil-dev--audit-format-findings
         "FAIL plist-return (tool body returns `(list :K ...)'; wrap at\n     registration or opt-out via\n     `;;; anvil-audit: tools-wrapped-at-registration' header)"
         plists
         (lambda (f)
           (format "%s:%d  %s"
                   (plist-get f :file)
                   (plist-get f :line)
                   (plist-get f :defun))))
        (anvil-dev--audit-format-findings
         "FAIL issue-fix shipped without a test change (Fixes / Closes /\n     Resolves #N but no file under `tests/' in the same commit)"
         issue-fix
         (lambda (f)
           (format "%s  #%d  %s"
                   (plist-get f :sha)
                   (plist-get f :issue)
                   (plist-get f :subject))))
        (anvil-dev--audit-format-findings
         "FAIL gitleaks detected secrets in tracked content (value redacted;\n     use fingerprint with `gitleaks' allowlist if false positive)"
         secrets
         (lambda (f)
           (format "%s:%d  %s  [%s]"
                   (plist-get f :file)
                   (plist-get f :line)
                   (plist-get f :rule)
                   (plist-get f :fingerprint))))
        (anvil-dev--audit-format-findings
         "FAIL fixture realism: test let-binds production var to wrong shape\n     (T70 false-PASS pattern — production = hash-table, test = alist)"
         realism
         (lambda (f)
           (format "%s  %s  declared=%s  bound=%s"
                   (plist-get f :file)
                   (plist-get f :var)
                   (plist-get f :declared)
                   (plist-get f :bound))))
        (anvil-dev--audit-format-findings
         "WARN design docs not yet SHIPPED (master-gate informational)"
         docs
         (lambda (f)
           (format "%s — %s"
                   (plist-get f :file)
                   (plist-get f :status)))))))))

(defun anvil-dev--tool-release-audit (&optional scope)
  "Audit the anvil tree for pre-release hazards and return a formatted report.

MCP Parameters:
  scope - Optional file-or-directory path.  When supplied, the
          source scanners (arglist-strip / missing-params /
          plist-return) are restricted to that path; the
          design-docs, issue-fix, and fixture-realism scans still
          cover the whole tree.  Empty string = full audit."
  (anvil-server-with-error-handling
    (let ((sc (and scope (stringp scope) (not (string-empty-p scope))
                   scope)))
      (anvil-dev--audit-format-report
       (anvil-dev-release-audit nil :scope sc)))))

;;;; --- module lifecycle ----------------------------------------------------

;;;###autoload
(defun anvil-dev-enable ()
  "Register the dev-* MCP tools."
  (anvil-server-register-tool
   #'anvil-dev--tool-self-sync-check
   :id "anvil-self-sync-check"
   :intent '(dev audit)
   :layer 'dev
   :server-id anvil-dev--server-id
   :description
   "Report the installed anvil clone's git HEAD + branch + dirty
count, and (when `anvil-dev-source-path' is set) compare against
the dev checkout to flag unpushed / unpulled divergence before it
causes a silent \"old code loaded\" bug after a daemon restart."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-dev--tool-test-run-all
   :id "anvil-test-run-all"
   :intent '(dev test)
   :layer 'dev
   :server-id anvil-dev--server-id
   :description
   "Run every tests/anvil-*-test.el in the anvil checkout via
`emacs --batch' subprocesses and return aggregated ERT counts.
CI only exercises the smoke suite — use this for pre-commit
verification across every test file."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-dev--tool-codex-efficiency-check
   :id "anvil-codex-efficiency-check"
   :intent '(dev audit codex)
   :layer 'dev
   :server-id anvil-dev--server-id
   :description
   "Check the local Codex token-saving setup in one read-only call:
Codex config readability, emacs-eval / Serena / Context7 MCP
server entries, uvx / npx executables, required local Codex
skills, project-side Serena config, and the recovery reference
note.  Returns :ok plus actionable :warnings."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-dev--tool-claude-limits-analyze
   :id "anvil-claude-limits-analyze"
   :intent '(dev audit claude usage)
   :layer 'dev
   :server-id anvil-dev--server-id
   :description
   "Parse Claude Code's limits usage breakdown text and return
Anvil-focused mitigation guidance for high-context sessions,
subagent-heavy use, 8h+ sessions, /loop usage, and emacs-eval MCP
result pressure.  Read-only; useful after copying the day/week
limits screen from Claude Code."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-dev--tool-scaffold-module
   :id "anvil-scaffold-module"
   :intent '(dev scaffold)
   :layer 'dev
   :server-id anvil-dev--server-id
   :description
   "Create a new anvil-NAME.el module plus tests/anvil-NAME-test.el
with standard GPL header, defgroup, enable/disable stubs, and two
passing smoke tests so the new module compiles and runs before
any real code lives in it."
   :read-only nil)
  (anvil-server-register-tool
   #'anvil-dev--tool-release-audit
   :id "anvil-release-audit"
   :intent '(dev audit)
   :layer 'dev
   :server-id anvil-dev--server-id
   :description
   "Scan the anvil tree for seven pre-release hazard classes:
Emacs 30 `_arg' arglist-strip regressions in MCP tool wrappers,
wrappers with real args but no `MCP Parameters:' docstring
section, wrappers whose body ends with a `(list :K ...)' plist
literal (MCP string-or-nil contract violation, unless the file
opts out via `;;; anvil-audit: tools-wrapped-at-registration'),
issue-closing commits whose diff does not touch `tests/' (the
37fcc52 pattern — ship a fix with no regression guard),
docs/design/*.org files whose STATUS text lacks `SHIPPED',
secrets detected by `gitleaks' in tracked content (skipped when
the binary is unavailable), and test fixtures that let-bind a
production variable to a wrong-shape value (the T70 false-PASS
hash-table-vs-alist pattern).  Returns a formatted report;
`clean' when empty."
   :read-only t))

(defun anvil-dev-disable ()
  "Unregister the dev-* MCP tools."
  (anvil-server-unregister-tool "anvil-self-sync-check"
                                anvil-dev--server-id)
  (anvil-server-unregister-tool "anvil-test-run-all"
                                anvil-dev--server-id)
  (anvil-server-unregister-tool "anvil-codex-efficiency-check"
                                anvil-dev--server-id)
  (anvil-server-unregister-tool "anvil-claude-limits-analyze"
                                anvil-dev--server-id)
  (anvil-server-unregister-tool "anvil-scaffold-module"
                                anvil-dev--server-id)
  (anvil-server-unregister-tool "anvil-release-audit"
                                anvil-dev--server-id))

(provide 'anvil-dev)
;;; anvil-dev.el ends here
