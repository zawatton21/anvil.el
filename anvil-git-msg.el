;;; anvil-git-msg.el --- Commit / PR message synthesis  -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 15 Phase 1 — structured Conventional-Commits draft generator
;; and PR-body draft generator driven by the actual staged diff / commit
;; log.  The tools are deterministic: they do NOT call an LLM.  Claude
;; (or any caller) still writes the subject line itself; the module's
;; job is to hand back the scaffolding — type / scope / body bullets /
;; trailers / suggested-full template — so the caller spends tokens on
;; the human-readable summary instead of re-reading every diff.
;;
;; Public Elisp API:
;;   (anvil-git-msg-commit &key repo scope-override)
;;     -> (:type :scope :subject :body :trailers :suggested-full :files)
;;
;;   (anvil-git-msg-pr &key base head repo)
;;     -> (:title :summary :test-plan :commits :suggested-body)
;;
;; MCP tools (emacs-eval server):
;;   git-commit-message   — draft for the staged diff of the current repo
;;   git-pr-body          — draft summarising commits between BASE and HEAD
;;
;; Both are read-only — no writes to git history.  Co-Authored-By is
;; appended to the commit draft automatically per memory
;; `feedback_commit_co_authored_by.md'.  Callers can disable that via
;; `anvil-git-msg-co-authored-by' set to nil.
;;
;; Phase 2 (git-why blame, git-commit-split hunk clustering) is still
;; DRAFT in docs/design/15-git-msg.org.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'anvil-server)
(require 'anvil-git)


;;;; --- group + defcustoms ---------------------------------------------------

(defgroup anvil-git-msg nil
  "Commit / PR message synthesis for anvil."
  :group 'anvil
  :prefix "anvil-git-msg-")

(defconst anvil-git-msg--server-id "emacs-eval"
  "Server id under which git-msg-* MCP tools register.")

(defcustom anvil-git-msg-co-authored-by
  "Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
  "Identity appended as the `Co-Authored-By' trailer on every commit draft.
Set to nil to skip the trailer entirely (humans not collaborating
with Claude).  Per memory `feedback_commit_co_authored_by.md'
this is mandatory for Claude-mediated commits."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'anvil-git-msg)

(defcustom anvil-git-msg-subject-placeholder "<describe>"
  "Placeholder emitted in `:subject' when the module cannot infer one.
The caller replaces this with a human-written summary before
actually committing."
  :type 'string
  :group 'anvil-git-msg)

(defcustom anvil-git-msg-test-file-regexp
  "\\(?:\\`\\|/\\)tests?/\\|-test\\.el\\'\\|_test\\.el\\'"
  "Path regexp used to classify a file as a test."
  :type 'regexp
  :group 'anvil-git-msg)

(defcustom anvil-git-msg-doc-file-regexp
  "\\.\\(?:org\\|md\\|markdown\\|rst\\|txt\\)\\'\\|\\(?:\\`\\|/\\)docs?/"
  "Path regexp used to classify a file as documentation."
  :type 'regexp
  :group 'anvil-git-msg)

(defcustom anvil-git-msg-max-body-files 20
  "Maximum files to enumerate in the body bullets.
Larger diffs collapse to an ellipsis with an overflow count."
  :type 'integer
  :group 'anvil-git-msg)

(defcustom anvil-git-msg-pr-base-default "master"
  "Default base branch for `anvil-git-msg-pr'.
Override per call with :base."
  :type 'string
  :group 'anvil-git-msg)

(defcustom anvil-git-msg-pr-max-commits 40
  "Maximum commits to include in a PR-body draft."
  :type 'integer
  :group 'anvil-git-msg)


;;;; --- staged-file classification -----------------------------------------

(defun anvil-git-msg--test-file-p (path)
  (and (stringp path)
       (string-match-p anvil-git-msg-test-file-regexp path)))

(defun anvil-git-msg--doc-file-p (path)
  (and (stringp path)
       (string-match-p anvil-git-msg-doc-file-regexp path)))

(defun anvil-git-msg--parse-status (letter)
  "Translate a name-status letter into a symbol."
  (pcase letter
    ("A" 'added) ("M" 'modified) ("D" 'deleted)
    ("R" 'renamed) ("C" 'copied) ("T" 'typechange)
    ("U" 'unmerged) (_ 'other)))

(defun anvil-git-msg--collect-staged (repo)
  "Return per-file plists for the staged diff in REPO.
Each plist carries :path :added :deleted :status.
Returns nil when nothing is staged."
  (let* ((name-status
          (plist-get
           (anvil-git--run "diff --cached --name-status" repo
                           '(:max-output 131072))
           :stdout))
         (numstat
          (plist-get
           (anvil-git--run "diff --cached --numstat" repo
                           '(:max-output 131072))
           :stdout))
         (status-map (make-hash-table :test 'equal))
         files)
    (dolist (line (split-string name-status "\n" t))
      (let ((parts (split-string line "\t")))
        (when (>= (length parts) 2)
          (puthash (car (last parts))
                   (anvil-git-msg--parse-status
                    (substring (car parts) 0 1))
                   status-map))))
    (dolist (line (split-string numstat "\n" t))
      (let ((parts (split-string line "\t")))
        (when (= (length parts) 3)
          (let* ((added   (string-to-number (nth 0 parts)))
                 (deleted (string-to-number (nth 1 parts)))
                 (path    (nth 2 parts))
                 (status  (gethash path status-map 'modified)))
            (push (list :path path
                        :added added
                        :deleted deleted
                        :status status)
                  files)))))
    (nreverse files)))


;;;; --- type + scope inference ---------------------------------------------

(defun anvil-git-msg--infer-type (files)
  "Classify FILES into a Conventional-Commits type string.
Default heuristics: all-test -> test, all-doc -> docs, any-added
file -> feat, otherwise fix.  Returns a string; caller may override."
  (cond
   ((null files) "chore")
   ((cl-every (lambda (f) (anvil-git-msg--test-file-p (plist-get f :path)))
              files)
    "test")
   ((cl-every (lambda (f) (anvil-git-msg--doc-file-p (plist-get f :path)))
              files)
    "docs")
   ((cl-some (lambda (f) (eq (plist-get f :status) 'added)) files)
    "feat")
   (t "fix")))

(defun anvil-git-msg--module-name-of (path)
  "Return the `module' portion of PATH used for scope inference.
Rules (applied in order):
- tests/NAME-test.el         -> NAME
- docs/design/NN-NAME.org    -> NAME (drops leading digit hyphen)
- docs/*                     -> \"docs\"
- .github/*                  -> \"ci\"
- benchmarks/*               -> \"benchmarks\"
- pty-broker/*               -> \"pty-broker\"
- anvil-PKG.el /-*.el        -> PKG (strip `anvil-' prefix)
- top-level FILE.el          -> basename minus extension
Returns nil when no useful scope can be extracted."
  (when (and (stringp path) (not (string-empty-p path)))
    (let ((base (file-name-nondirectory path)))
      (cond
       ((string-match-p "\\`docs/" path)
        (if (string-match "\\`docs/design/[0-9]+-\\(.+\\)\\.org\\'" path)
            (match-string 1 path)
          "docs"))
       ((string-match-p "\\`\\.github/\\|\\`\\.gitlab/\\|\\`\\.circleci/" path)
        "ci")
       ((string-match-p "\\`benchmarks/" path) "benchmarks")
       ((string-match-p "\\`pty-broker/" path) "pty-broker")
       ((string-match "\\`\\(?:tests?/\\)?anvil-\\([a-zA-Z0-9-]+\\?\\)\\(?:-test\\)?\\.el\\'" base)
        (match-string 1 base))
       ((string-match "\\`anvil-\\([a-zA-Z0-9-]+?\\)\\.el\\'" base)
        (match-string 1 base))
       ((string-match "\\`\\(.+?\\)\\(?:-test\\)?\\.el\\'" base)
        (match-string 1 base))
       (t nil)))))

(defun anvil-git-msg--common-prefix (strings)
  "Return the longest common hyphen-delimited prefix of STRINGS.
\(\"orchestrator-routing\" \"orchestrator\") -> \"orchestrator\".
Returns nil when the list is empty or the first element is nil."
  (when (and strings (car strings))
    (let* ((segs (mapcar (lambda (s) (and s (split-string s "-"))) strings))
           (first (car segs))
           (common nil))
      (when (cl-every #'identity segs)
        (catch 'done
          (dolist (seg first)
            (if (cl-every (lambda (other)
                            (and other
                                 (string= seg (car other))))
                          segs)
                (progn (push seg common)
                       (setq segs (mapcar #'cdr segs)))
              (throw 'done nil))))
        (when common
          (mapconcat #'identity (nreverse common) "-"))))))

(defun anvil-git-msg--infer-scope (files)
  "Return the Conventional-Commits scope for FILES, or nil.
Maps every file to its module name (see `--module-name-of'),
then folds to the longest common hyphen prefix.  Mixed modules
yield nil — the caller should leave the scope field empty."
  (let* ((modules (delq nil
                        (mapcar (lambda (f)
                                  (anvil-git-msg--module-name-of
                                   (plist-get f :path)))
                                files)))
         (unique (delete-dups (copy-sequence modules))))
    (cond
     ((null unique) nil)
     ((= 1 (length unique)) (car unique))
     (t (anvil-git-msg--common-prefix unique)))))


;;;; --- body assembly ------------------------------------------------------

(defun anvil-git-msg--status-verb (status)
  (pcase status
    ('added    "add")
    ('deleted  "remove")
    ('renamed  "rename")
    ('copied   "copy")
    ('typechange "change type of")
    (_ "update")))

(defun anvil-git-msg--file-bullet (file)
  (let ((path (plist-get file :path))
        (added (plist-get file :added))
        (deleted (plist-get file :deleted))
        (status (plist-get file :status)))
    (format "- %s %s (+%d / -%d)"
            (anvil-git-msg--status-verb status)
            path added deleted)))

(defun anvil-git-msg--build-body (files)
  "Return a string with bullet lines summarising FILES."
  (let* ((max anvil-git-msg-max-body-files)
         (shown (seq-take files max))
         (overflow (- (length files) (length shown)))
         (lines (mapcar #'anvil-git-msg--file-bullet shown)))
    (when (> overflow 0)
      (setq lines
            (append lines
                    (list (format "- ... (+%d more files)" overflow)))))
    (mapconcat #'identity lines "\n")))


;;;; --- suggested-full template --------------------------------------------

(defun anvil-git-msg--format-trailers (trailers)
  "Format TRAILERS (alist) into a string ready to append to a commit body."
  (mapconcat (lambda (pair)
               (format "%s: %s" (car pair) (cdr pair)))
             trailers "\n"))

(defun anvil-git-msg--suggested-full (type scope subject body trailers)
  "Assemble the final commit-message template."
  (let* ((header (if (and scope (not (string-empty-p scope)))
                     (format "%s(%s): %s" type scope subject)
                   (format "%s: %s" type subject)))
         (parts (list header)))
    (when (and body (not (string-empty-p body)))
      (setq parts (append parts (list "" body))))
    (when trailers
      (setq parts (append parts
                          (list "" (anvil-git-msg--format-trailers trailers)))))
    (mapconcat #'identity parts "\n")))


;;;; --- public API: commit -------------------------------------------------

;;;###autoload
(cl-defun anvil-git-msg-commit (&key repo scope-override)
  "Return a structured commit-message draft for the staged diff of REPO.

REPO defaults to `default-directory'.  SCOPE-OVERRIDE, when
non-nil, replaces the inferred Conventional-Commits scope.

Returned plist:
  :type             Conventional-Commits type string (e.g. \"feat\").
  :scope            Scope string or nil.
  :subject          Placeholder \"<describe>\" the caller replaces.
  :body             Multi-line bullet list summarising each file.
  :trailers         Alist of trailer name -> value (includes
                    Co-Authored-By when `anvil-git-msg-co-authored-by'
                    is non-nil).
  :files            Raw file plists as returned by the staged
                    collector (:path :added :deleted :status).
  :suggested-full   Full template ready to pipe into
                    `git commit -F -' after swapping the subject.

Returns nil when nothing is staged."
  (let ((files (anvil-git-msg--collect-staged repo)))
    (when files
      (let* ((type (anvil-git-msg--infer-type files))
             (scope (or scope-override
                        (anvil-git-msg--infer-scope files)))
             (subject anvil-git-msg-subject-placeholder)
             (body (anvil-git-msg--build-body files))
             (trailers
              (when anvil-git-msg-co-authored-by
                (list (cons "Co-Authored-By"
                            anvil-git-msg-co-authored-by))))
             (full (anvil-git-msg--suggested-full
                    type scope subject body trailers)))
        (list :type type
              :scope scope
              :subject subject
              :body body
              :trailers trailers
              :files files
              :suggested-full full)))))


;;;; --- public API: PR body ------------------------------------------------

(defun anvil-git-msg--log-range (base head repo limit)
  "Return commit plists between BASE and HEAD, up to LIMIT entries."
  (let* ((range (format "%s..%s"
                        (shell-quote-argument base)
                        (shell-quote-argument head)))
         (res (anvil-git--run
               (format "log -n %d --pretty=format:%%H%%x09%%aI%%x09%%an%%x09%%s %s"
                       limit range)
               repo '(:max-output 1048576))))
    (when (eql 0 (plist-get res :exit))
      (let (commits)
        (dolist (line (split-string (plist-get res :stdout) "\n" t))
          (let ((fields (split-string line "\t")))
            (when (>= (length fields) 4)
              (push (list :hash (nth 0 fields)
                          :date (nth 1 fields)
                          :author (nth 2 fields)
                          :subject (string-join (nthcdr 3 fields) "\t"))
                    commits))))
        (nreverse commits)))))

(defun anvil-git-msg--pr-test-plan-items (commits)
  "Derive a minimal manual test-plan checklist from COMMITS."
  (let ((types (delete-dups
                (delq nil
                      (mapcar
                       (lambda (c)
                         (when (string-match
                                "\\`\\(feat\\|fix\\|refactor\\|test\\|docs\\|chore\\|perf\\|build\\|ci\\)\\(?:(\\([^)]+\\))\\)?:"
                                (plist-get c :subject))
                           (match-string 1 (plist-get c :subject))))
                       commits))))
        items)
    (when (member "feat" types)
      (push "- [ ] Exercise every new public command / tool at least once"
            items))
    (when (member "fix" types)
      (push "- [ ] Reproduce the original bug and confirm the fix"
            items))
    (when (member "perf" types)
      (push "- [ ] Spot-check no regression via bench-compare"
            items))
    (unless (member "docs" types)
      (push "- [ ] make test-all green on develop" items))
    (push "- [ ] Review diff for Co-Authored-By trailer presence" items)
    (nreverse items)))

;;;###autoload
(cl-defun anvil-git-msg-pr (&key base head repo)
  "Return a PR-body draft summarising commits between BASE and HEAD.

BASE defaults to `anvil-git-msg-pr-base-default' (e.g. \"master\").
HEAD defaults to the current branch (`anvil-git-branch-current').
Up to `anvil-git-msg-pr-max-commits' commits are included.

Returned plist:
  :title            Placeholder subject line derived from the last commit.
  :summary          Short prose paragraph listing commit count / types.
  :test-plan        List of checklist bullets, one per detected type.
  :commits          Raw commit plists (hash / date / author / subject).
  :suggested-body   Ready-to-paste markdown body.

Returns nil when the range is empty."
  (let* ((base (or base anvil-git-msg-pr-base-default))
         (head (or head (anvil-git-branch-current repo) "HEAD"))
         (commits (anvil-git-msg--log-range
                   base head repo anvil-git-msg-pr-max-commits)))
    (when commits
      (let* ((count (length commits))
             (last-subject (plist-get (car commits) :subject))
             (title last-subject)
             (summary (format "%d commit%s on %s since %s."
                              count
                              (if (= count 1) "" "s")
                              head base))
             (test-plan (anvil-git-msg--pr-test-plan-items commits))
             (body
              (string-join
               (list "## Summary"
                     summary
                     ""
                     "## Commits"
                     (mapconcat
                      (lambda (c)
                        (format "- `%s` %s"
                                (substring (plist-get c :hash) 0 7)
                                (plist-get c :subject)))
                      commits "\n")
                     ""
                     "## Test Plan"
                     (mapconcat #'identity test-plan "\n"))
               "\n")))
        (list :title title
              :summary summary
              :test-plan test-plan
              :commits commits
              :suggested-body body)))))


;;;; --- MCP handlers -------------------------------------------------------

(defun anvil-git-msg--coerce-optional-string (s)
  (and (stringp s) (not (string-empty-p s)) s))

(defun anvil-git-msg--tool-commit-message (&optional scope repo)
  "MCP wrapper for `anvil-git-msg-commit'.

MCP Parameters:
  scope - Optional Conventional-Commits scope override.  Empty
          string = let the module infer it.
  repo  - Optional repository path; defaults to the daemon's
          current `default-directory'."
  (anvil-server-with-error-handling
    (or (anvil-git-msg-commit
         :scope-override (anvil-git-msg--coerce-optional-string scope)
         :repo (anvil-git-msg--coerce-optional-string repo))
        (list :staged nil
              :message "git-commit-message: nothing is staged"))))

(defun anvil-git-msg--tool-pr-body (&optional base head repo)
  "MCP wrapper for `anvil-git-msg-pr'.

MCP Parameters:
  base - Optional base ref (default `anvil-git-msg-pr-base-default').
  head - Optional head ref (default current branch).
  repo - Optional repository path."
  (anvil-server-with-error-handling
    (or (anvil-git-msg-pr
         :base (anvil-git-msg--coerce-optional-string base)
         :head (anvil-git-msg--coerce-optional-string head)
         :repo (anvil-git-msg--coerce-optional-string repo))
        (list :commits nil
              :message "git-pr-body: range is empty"))))


;;;; --- lifecycle ----------------------------------------------------------

(defun anvil-git-msg--register-tools ()
  "Register git-msg-* MCP tools under `anvil-git-msg--server-id'."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-git-msg--tool-commit-message)
   :id "git-commit-message"
   :intent '(git compose)
   :layer 'workflow
   :server-id anvil-git-msg--server-id
   :description
   "Generate a Conventional-Commits draft from the staged diff.
Returns a plist with :type / :scope / :subject / :body / :trailers
/ :files / :suggested-full.  The :subject is a placeholder the
caller replaces with human-readable text; :suggested-full is the
full template (header + body + Co-Authored-By trailer) ready to
pipe into `git commit -F -' after the subject swap.  Read-only —
never writes to git history."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-git-msg--tool-pr-body)
   :id "git-pr-body"
   :intent '(git compose)
   :layer 'workflow
   :server-id anvil-git-msg--server-id
   :description
   "Generate a PR-body draft summarising commits between BASE and
HEAD.  Returns a plist with :title / :summary / :test-plan
/ :commits / :suggested-body.  Up to 40 commits are included.
Read-only — never creates PRs (use gh CLI or mcp__github for
that)."
   :read-only t))

(defun anvil-git-msg--unregister-tools ()
  (dolist (id '("git-commit-message" "git-pr-body"))
    (ignore-errors
      (anvil-server-unregister-tool id anvil-git-msg--server-id))))

;;;###autoload
(defun anvil-git-msg-enable ()
  "Register the Doc 15 commit / PR-body MCP tools."
  (interactive)
  (anvil-git-msg--register-tools))

;;;###autoload
(defun anvil-git-msg-disable ()
  "Unregister the Doc 15 commit / PR-body MCP tools."
  (interactive)
  (anvil-git-msg--unregister-tools))

(provide 'anvil-git-msg)

;;; anvil-git-msg.el ends here
