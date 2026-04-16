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
         (exit (let ((default-directory (file-name-as-directory root)))
                 (call-process
                  anvil-dev-emacs-bin nil buf nil
                  "--batch" "-L" root
                  "-l" "ert" "-l" file
                  "-f" "ert-run-tests-batch-and-exit")))
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
(defun anvil-dev-test-run-all (&optional project-dir)
  "Run every tests/anvil-*-test.el in PROJECT-DIR via `emacs --batch'.
Each file runs in its own subprocess so one file's load error
cannot mask failures in others.  Returns an aggregated plist:
  :project-dir ROOT :file-count N :total T :passed P :failed F
  :skipped S :elapsed-ms MS :failed-files (FILE...) :per-file (PLIST...)"
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
             (bad (cl-remove-if (lambda (r) (plist-get r :ok)) results)))
        (when (called-interactively-p 'any)
          (message "Anvil test-run-all: %d files · %d/%d tests · %d failed · %.1fs"
                   (length files) passed total failed
                   (/ elapsed-ms 1000.0)))
        (list :project-dir  root
              :file-count   (length files)
              :total        total
              :passed       passed
              :failed       failed
              :skipped      skipped
              :elapsed-ms   elapsed-ms
              :failed-files (mapcar (lambda (r) (plist-get r :file)) bad)
              :per-file     results)))))

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

(defun anvil-dev--tool-test-run-all (&optional project-dir)
  "MCP wrapper for `anvil-dev-test-run-all'.

Drops the per-file :output (noisy ERT logs) from the response so
a green run stays compact; failure output is still reachable via
the interactive command.

MCP Parameters:
  project-dir - Optional anvil checkout root (defaults to installed clone)"
  (anvil-server-with-error-handling
   (let* ((dir (and project-dir (stringp project-dir)
                    (not (string-empty-p project-dir))
                    project-dir))
          (result (anvil-dev-test-run-all dir))
          (lean-per-file
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
     (format "%S" compact))))

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

;;;; --- module lifecycle ----------------------------------------------------

;;;###autoload
(defun anvil-dev-enable ()
  "Register the dev-* MCP tools."
  (anvil-server-register-tool
   #'anvil-dev--tool-self-sync-check
   :id "anvil-self-sync-check"
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
   :server-id anvil-dev--server-id
   :description
   "Run every tests/anvil-*-test.el in the anvil checkout via
`emacs --batch' subprocesses and return aggregated ERT counts.
CI only exercises the smoke suite — use this for pre-commit
verification across every test file."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-dev--tool-scaffold-module
   :id "anvil-scaffold-module"
   :server-id anvil-dev--server-id
   :description
   "Create a new anvil-NAME.el module plus tests/anvil-NAME-test.el
with standard GPL header, defgroup, enable/disable stubs, and two
passing smoke tests so the new module compiles and runs before
any real code lives in it."
   :read-only nil))

(defun anvil-dev-disable ()
  "Unregister the dev-* MCP tools."
  (anvil-server-unregister-tool "anvil-self-sync-check"
                                anvil-dev--server-id)
  (anvil-server-unregister-tool "anvil-test-run-all"
                                anvil-dev--server-id)
  (anvil-server-unregister-tool "anvil-scaffold-module"
                                anvil-dev--server-id))

(provide 'anvil-dev)
;;; anvil-dev.el ends here
