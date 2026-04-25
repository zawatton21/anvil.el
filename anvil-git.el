;;; anvil-git.el --- Git operations for anvil -*- lexical-binding: t; -*-

;; Author: zawatton
;; Keywords: tools, mcp, claude, git, vc

;;; Commentary:

;; anvil-host.el の姉妹ファイル。Claude Code が emacs-eval 経由で
;; git リポジトリの状態を OS 非依存に取得する helper 群。
;;
;; 構成:
;;   Layer 1 — `anvil-shell' (anvil-host が所有)
;;   Layer 2 — `anvil-git-status' / `anvil-git-log' / `anvil-git-diff-stats'
;;
;; 設計方針:
;;   - 全 git 呼び出しに `-c core.quotepath=false' を強制 → 日本語パス安全
;;   - REPO 引数は :cwd 経由で anvil-shell に渡す
;;   - 戻り値は plist。失敗時は (error "anvil-git: ...") を投げる
;;
;; なぜ今 helper 化したか:
;;   handoff doc では「2 回以上 ad-hoc parse したら作る」予定だったが、
;;   先に整備して self-reinforcing loop を起動する方を選んだ
;;   (memory: feedback_my_cc_helpers_proposal.md / user collaboration philosophy)。

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'anvil-host)
(require 'anvil-server)

(defconst anvil-git--server-id "emacs-eval"
  "MCP server id that git-* tools register under.")

;;;; --- MCP serialization --------------------------------------------------
;;
;; All git helpers return elisp-native shapes (strings, plists, lists of
;; plists, keywords like :null / :empty-array).  anvil-server requires
;; tool handlers to return `string' or `nil' — anything else trips
;; `Tool handler must return string or nil'.  The small encoder below
;; converts our shapes to JSON, keeping :null / :empty-array sentinels
;; semantically meaningful (→ JSON null / empty array respectively).

(defun anvil-git--plist-p (x)
  "Return non-nil when X looks like a plist (keyword head)."
  (and (listp x) (keywordp (car-safe x))))

(defun anvil-git--to-json-value (x)
  "Recursively convert X into a shape `json-encode' renders sensibly.
Plists become alists with symbol keys (leading colon stripped).
Plain lists (non-plist) become vectors so they emit as JSON arrays.
The sentinels :null and :empty-array map to nil / empty vector."
  (cond
   ((eq x :null) nil)
   ((eq x :empty-array) (vector))
   ((or (null x) (eq x t) (numberp x) (stringp x)) x)
   ((keywordp x) (substring (symbol-name x) 1))
   ((symbolp x) (symbol-name x))
   ((vectorp x)
    (vconcat (mapcar #'anvil-git--to-json-value x)))
   ((anvil-git--plist-p x)
    (cl-loop for (k v) on x by #'cddr
             collect (cons (intern (substring (symbol-name k) 1))
                           (anvil-git--to-json-value v))))
   ((and (consp x) (not (proper-list-p x)))
    ;; Dotted pair — emit as [car, cdr] to survive improper-list mapcar.
    (vector (anvil-git--to-json-value (car x))
            (anvil-git--to-json-value (cdr x))))
   ((listp x)
    (vconcat (mapcar #'anvil-git--to-json-value x)))
   (t (format "%S" x))))

(defun anvil-git--mcp-encode (value)
  "Encode VALUE as a JSON string for MCP return.
Strings and nil pass through — both are valid direct returns."
  (cond
   ((null value) nil)
   ((stringp value) value)
   (t (json-encode (anvil-git--to-json-value value)))))

;;;; --- internal: shell wrapper --------------------------------------------

(defun anvil-git--run (args repo &optional opts)
  "Run `git -c core.quotepath=false ARGS' in REPO. Returns anvil-shell plist.
ARGS is a single string already shell-quoted by the caller. OPTS is
merged into the anvil-shell call (cwd is forced to REPO if non-nil).
`--no-pager' is added unconditionally so pager-configured repos
never inject terminal escapes into captured stdout."
  (let* ((cmd (concat "git --no-pager -c core.quotepath=false " args))
         (merged (append (and repo (list :cwd (expand-file-name repo)))
                         (list :coding 'utf-8)
                         opts)))
    (anvil-shell cmd merged)))

(defun anvil-git--check (res context)
  "Error if RES (anvil-shell plist) reports non-zero exit. CONTEXT is a string."
  (let ((exit (plist-get res :exit)))
    (unless (eql exit 0)
      (error "anvil-git: %s failed (exit %s): %s"
             context exit (string-trim (plist-get res :stderr))))))

(defun anvil-git--require-directory (path context)
  "Return PATH expanded, or signal when PATH is not an existing directory.
Nil PATH is returned as nil so callers can still default to
`default-directory'.  CONTEXT names the public helper for error text."
  (cond
   ((null path) nil)
   ((file-directory-p path) (expand-file-name path))
   ((file-exists-p path)
    (user-error "%s: PATH must be a directory: %s" context path))
   (t
    (user-error "%s: directory does not exist: %s" context path))))

;;;; --- anvil-git-status ---------------------------------------------------

(defun anvil-git--parse-branch-line (line)
  "Parse a `## ...' header line from `git status --porcelain=v1 -b'.
Returns (BRANCH UPSTREAM AHEAD BEHIND)."
  (let (branch upstream ahead behind)
    (cond
     ;; ## branch...upstream [ahead N, behind M]
     ((string-match
       "\\`## \\([^.]+?\\)\\(?:\\.\\.\\.\\([^ ]+\\)\\(?: \\[\\(.*\\)\\]\\)?\\)?\\'"
       line)
      (setq branch   (match-string 1 line)
            upstream (match-string 2 line))
      (let ((extras (match-string 3 line)))
        (when extras
          (when (string-match "ahead \\([0-9]+\\)" extras)
            (setq ahead (string-to-number (match-string 1 extras))))
          (when (string-match "behind \\([0-9]+\\)" extras)
            (setq behind (string-to-number (match-string 1 extras)))))))
     ;; HEAD is detached / no commits / etc.
     ((string-match "\\`## \\(.*\\)\\'" line)
      (setq branch (match-string 1 line))))
    (list branch upstream ahead behind)))

(defun anvil-git--parse-status-entry (line)
  "Parse one body line from `git status --porcelain=v1 -b'.
Returns (KIND PATH MODE) where KIND is one of 'staged 'modified
'untracked 'ignored 'unmerged. MODE is the 2-char XY string."
  (cond
   ((string-prefix-p "## " line) nil)
   ((< (length line) 4) nil)
   (t
    (let* ((xy   (substring line 0 2))
           (path (substring line 3))
           (x    (aref xy 0))
           (y    (aref xy 1)))
      (cond
       ((string= xy "??") (list 'untracked path xy))
       ((string= xy "!!") (list 'ignored   path xy))
       ((or (= x ?U) (= y ?U) (string= xy "AA") (string= xy "DD"))
        (list 'unmerged path xy))
       (t
        (let (kinds)
          (unless (= x ?\s) (push 'staged kinds))
          (unless (= y ?\s) (push 'modified kinds))
          ;; First record under each applicable bucket — but the
          ;; current interface returns one kind per entry. We pick
          ;; staged when X is set, modified when only Y is set, so
          ;; that the caller's :staged / :modified buckets stay
          ;; non-overlapping for the common case.
          (list (if (= x ?\s) 'modified 'staged) path xy))))))))

(defun anvil-git-status (&optional repo)
  "Return git status as a plist:
  (:branch STR :upstream STR-or-nil :ahead N-or-nil :behind N-or-nil
   :staged ((PATH MODE) ..) :modified ((PATH MODE) ..)
   :untracked ((PATH MODE) ..) :unmerged ((PATH MODE) ..))

REPO is the repository directory; defaults to `default-directory'."
  (let* ((res (anvil-git--run "status --porcelain=v1 -b" repo
                              '(:max-output 524288))))
    (anvil-git--check res "status")
    (let ((lines (split-string (plist-get res :stdout) "\n" t))
          branch upstream ahead behind
          staged modified untracked unmerged)
      (dolist (line lines)
        (if (string-prefix-p "## " line)
            (let ((parsed (anvil-git--parse-branch-line line)))
              (setq branch   (nth 0 parsed)
                    upstream (nth 1 parsed)
                    ahead    (nth 2 parsed)
                    behind   (nth 3 parsed)))
          (let ((entry (anvil-git--parse-status-entry line)))
            (when entry
              (let ((kind (nth 0 entry))
                    (rec  (list (nth 1 entry) (nth 2 entry))))
                (pcase kind
                  ('staged    (push rec staged))
                  ('modified  (push rec modified))
                  ('untracked (push rec untracked))
                  ('unmerged  (push rec unmerged))))))))
      (list :branch    branch
            :upstream  upstream
            :ahead     ahead
            :behind    behind
            :staged    (nreverse staged)
            :modified  (nreverse modified)
            :untracked (nreverse untracked)
            :unmerged  (nreverse unmerged)))))

;;;; --- anvil-git-log ------------------------------------------------------

(defun anvil-git-log (&optional repo limit)
  "Return the most recent LIMIT (default 20) commits as a list of plists:
  ((:hash STR :date STR-iso :author STR :subject STR) ..)

REPO is the repository directory; defaults to `default-directory'."
  (let* ((n   (or limit 20))
         (res (anvil-git--run
               (format "log -n %d --pretty=format:%%H%%x09%%aI%%x09%%an%%x09%%s" n)
               repo
               '(:max-output 524288))))
    (anvil-git--check res "log")
    (let ((lines (split-string (plist-get res :stdout) "\n" t))
          results)
      (dolist (line lines)
        (let ((fields (split-string line "\t")))
          (when (>= (length fields) 4)
            (push (list :hash    (nth 0 fields)
                        :date    (nth 1 fields)
                        :author  (nth 2 fields)
                        :subject (string-join (nthcdr 3 fields) "\t"))
                  results))))
      (nreverse results))))

;;;; --- anvil-git-diff-stats -----------------------------------------------

(defun anvil-git-diff-stats (&optional repo rev)
  "Return diff shortstat as a plist (:files N :insertions N :deletions N).
With no REV, compares the worktree against HEAD (unstaged + staged).
With REV (e.g. \"HEAD~1\" or \"main..feature\"), passes it to git diff."
  (let* ((args (if rev
                   (format "diff --shortstat %s" (shell-quote-argument rev))
                 "diff HEAD --shortstat"))
         (res (anvil-git--run args repo '(:max-output 4096))))
    (anvil-git--check res "diff --shortstat")
    (let ((line (string-trim (plist-get res :stdout)))
          (files 0) (ins 0) (del 0))
      (when (string-match "\\([0-9]+\\) files? changed" line)
        (setq files (string-to-number (match-string 1 line))))
      (when (string-match "\\([0-9]+\\) insertions?" line)
        (setq ins (string-to-number (match-string 1 line))))
      (when (string-match "\\([0-9]+\\) deletions?" line)
        (setq del (string-to-number (match-string 1 line))))
      (list :files files :insertions ins :deletions del))))

;;;; --- anvil-git-blame-line -----------------------------------------------

(defun anvil-git--parse-blame-porcelain (output)
  "Parse `git blame -L N,N --porcelain' OUTPUT into a single plist."
  (let ((lines (split-string output "\n" t))
        hash author author-mail author-time
        committer committer-time summary filename line-content)
    (when lines
      ;; First line: <hash> <orig> <final> <count>
      (when (string-match "\\`\\([0-9a-f]+\\) " (car lines))
        (setq hash (match-string 1 (car lines))))
      (dolist (line (cdr lines))
        (cond
         ((string-match "\\`author \\(.*\\)" line)
          (setq author (match-string 1 line)))
         ((string-match "\\`author-mail \\(.*\\)" line)
          (setq author-mail (match-string 1 line)))
         ((string-match "\\`author-time \\([0-9]+\\)" line)
          (setq author-time (string-to-number (match-string 1 line))))
         ((string-match "\\`committer \\(.*\\)" line)
          (setq committer (match-string 1 line)))
         ((string-match "\\`committer-time \\([0-9]+\\)" line)
          (setq committer-time (string-to-number (match-string 1 line))))
         ((string-match "\\`summary \\(.*\\)" line)
          (setq summary (match-string 1 line)))
         ((string-match "\\`filename \\(.*\\)" line)
          (setq filename (match-string 1 line)))
         ((string-prefix-p "\t" line)
          (setq line-content (substring line 1))))))
    (list :hash hash
          :author author
          :author-mail author-mail
          :author-date (and author-time
                            (format-time-string "%Y-%m-%dT%H:%M:%S"
                                                (seconds-to-time author-time)))
          :committer committer
          :committer-date (and committer-time
                               (format-time-string "%Y-%m-%dT%H:%M:%S"
                                                   (seconds-to-time committer-time)))
          :summary summary
          :filename filename
          :line line-content)))

(defun anvil-git-blame-line (file line &optional repo)
  "Return blame info for LINE of FILE as a plist.

FILE is relative to REPO (or `default-directory'). LINE is 1-based.
Returns:
  (:hash :author :author-mail :author-date :committer :committer-date
   :summary :filename :line)

Errors if FILE / LINE is invalid."
  (let* ((args (format "blame -L %d,%d --porcelain %s"
                       line line (shell-quote-argument file)))
         (res  (anvil-git--run args repo '(:max-output 16384))))
    (anvil-git--check res "blame")
    (anvil-git--parse-blame-porcelain (plist-get res :stdout))))

;;;; --- anvil-git-show-file ------------------------------------------------

(defun anvil-git-show-file (rev path &optional repo opts)
  "Return the contents of PATH at REV as a string.

REV is anything `git show' accepts (commit hash, tag, branch, HEAD~N).
PATH is relative to REPO. OPTS is forwarded to `anvil-shell'
(use :max-output to cap large blobs; default 262144).

Returns nil if the path does not exist at that revision."
  (let* ((merged (append (list :max-output 262144) opts))
         (args (format "show %s:%s"
                       (shell-quote-argument rev)
                       (shell-quote-argument path)))
         (res (anvil-git--run args repo merged)))
    (if (eql (plist-get res :exit) 0)
        (plist-get res :stdout)
      ;; missing path → exit 128, return nil instead of erroring
      (let ((err (string-trim (plist-get res :stderr))))
        (cond
         ((string-match-p "exists on disk, but not in" err) nil)
         ((string-match-p "does not exist" err) nil)
         (t (error "anvil-git: show failed: %s" err)))))))

;;;; --- anvil-git-toplevel -------------------------------------------------

(defun anvil-git-toplevel (&optional dir)
  "Return the absolute toplevel of the git repository containing DIR.
DIR defaults to `default-directory'. Returns nil if DIR is not inside
a git repository (does not error)."
  (let ((res (anvil-git--run "rev-parse --show-toplevel" dir
                             '(:max-output 4096))))
    (when (eql (plist-get res :exit) 0)
      (string-trim (plist-get res :stdout)))))

;;;; --- anvil-git-staged-diff ----------------------------------------------

(defun anvil-git-staged-diff (&optional repo opts)
  "Return the staged diff as a string. Empty string if nothing is staged.

REPO defaults to `default-directory'.
OPTS plist:
  :max-output N    cap output bytes (default 65536)
  :stat-only BOOL  use --stat instead of full patch"
  (let* ((stat-only (plist-get opts :stat-only))
         (args (concat "diff --staged --no-color"
                       (if stat-only " --stat" "")))
         (max-out (or (plist-get opts :max-output) 65536))
         (res (anvil-git--run args repo
                              (list :max-output max-out :coding 'utf-8))))
    (anvil-git--check res "diff --staged")
    (plist-get res :stdout)))

;;;; --- anvil-git-repo-root ------------------------------------------------

(defun anvil-git-repo-root (&optional dir)
  "Return the absolute top-level directory of the git repo for DIR.
Cheap probe; every worktree / orchestrator helper calls this
first.  Never signals — returns nil when DIR is not in a repo.
DIR, when non-nil, must name an existing directory.  Behaves
identically to `anvil-git-toplevel'; the `repo-root' name matches
the orchestrator / other-module naming convention."
  (anvil-git-toplevel (anvil-git--require-directory dir "anvil-git-repo-root")))

;;;; --- anvil-git-head-sha -------------------------------------------------

(defun anvil-git-head-sha (&optional repo short)
  "Return the HEAD commit SHA for REPO (default `default-directory').
SHORT non-nil returns the abbreviated form.  Returns nil when REPO
has no commits or is not a repo."
  (let* ((args (if short "rev-parse --short HEAD" "rev-parse HEAD"))
         (res  (anvil-git--run args repo '(:max-output 4096))))
    (when (eql (plist-get res :exit) 0)
      (let ((s (string-trim (plist-get res :stdout))))
        (and (not (string-empty-p s)) s)))))

;;;; --- anvil-git-branch-current -------------------------------------------

(defun anvil-git-branch-current (&optional repo)
  "Return the current branch name for REPO, or nil for detached HEAD."
  (let ((res (anvil-git--run "symbolic-ref --quiet --short HEAD" repo
                             '(:max-output 4096))))
    (when (eql (plist-get res :exit) 0)
      (let ((s (string-trim (plist-get res :stdout))))
        (and (not (string-empty-p s)) s)))))

;;;; --- anvil-git-diff-names -----------------------------------------------

(defun anvil-git-diff-names (&optional repo from to)
  "Return paths changed between FROM and TO in REPO as a list.
FROM and TO default to unstaged-vs-HEAD when both omitted.  Pass
FROM alone for `git diff FROM' (FROM..worktree); pass both for
`FROM..TO'."
  (let* ((args (concat "diff --name-only"
                       (when from (concat " " (shell-quote-argument from)))
                       (when to   (concat " " (shell-quote-argument to)))))
         (res  (anvil-git--run args repo '(:max-output 524288))))
    (anvil-git--check res "diff --name-only")
    (split-string (plist-get res :stdout) "\n" t)))

;;;; --- anvil-git-worktree-list --------------------------------------------

(defun anvil-git--parse-worktree-porcelain (output)
  "Parse `git worktree list --porcelain' OUTPUT into a list of plists.
Each entry is a normal (:key VAL ...) plist."
  (let (entries current)
    (dolist (line (split-string output "\n"))
      (cond
       ((string-empty-p line)
        (when current
          (push current entries)
          (setq current nil)))
       ((string-prefix-p "worktree " line)
        (setq current (list :path (substring line (length "worktree ")))))
       ((string-prefix-p "HEAD " line)
        (when current
          (setq current (plist-put current :head
                                   (substring line (length "HEAD "))))))
       ((string-prefix-p "branch " line)
        (when current
          (setq current (plist-put current :branch
                                   (substring line (length "branch "))))))
       ((equal line "bare")
        (when current (setq current (plist-put current :bare t))))
       ((equal line "detached")
        (when current (setq current (plist-put current :detached t))))))
    (when current (push current entries))
    (nreverse entries)))

(defun anvil-git-worktree-list (&optional repo)
  "Return the list of worktrees attached to REPO.
Each entry is a plist (:path :head :branch :bare :detached).
Bare / detached flags are only present when true."
  (let ((res (anvil-git--run "worktree list --porcelain" repo
                             '(:max-output 65536))))
    (anvil-git--check res "worktree list")
    (anvil-git--parse-worktree-porcelain (plist-get res :stdout))))

;;;; --- anvil-git-worktree-add / remove (Elisp only) -----------------------

(cl-defun anvil-git-worktree-add (path &optional repo &key ref branch force)
  "Create a git worktree at PATH in REPO.
Returns the absolute PATH on success; signals otherwise.
Keys: :ref (commit-ish to check out, default \"HEAD\"),
      :branch (create a new branch at REF),
      :force (pass `--force')."
  (let* ((target (expand-file-name path))
         (parent (file-name-directory target))
         (args (concat "worktree add"
                       (when force  " --force")
                       (when branch (concat " -b " (shell-quote-argument branch)))
                       " " (shell-quote-argument target)
                       " " (shell-quote-argument (or ref "HEAD")))))
    (unless (file-directory-p parent) (make-directory parent t))
    (anvil-git--check (anvil-git--run args repo nil) "worktree add")
    target))

(cl-defun anvil-git-worktree-remove (path &optional repo &key force)
  "Remove the worktree at PATH from REPO.  Returns t on success."
  (let ((args (concat "worktree remove"
                      (when force " --force")
                      " " (shell-quote-argument (expand-file-name path)))))
    (anvil-git--check (anvil-git--run args repo nil) "worktree remove")
    t))

;;;; --- MCP tool wrappers --------------------------------------------------

(defun anvil-git--tool-repo-root (path)
  "Return the git repo top-level dir for PATH, or nil.

MCP Parameters:
  path - Directory inside the repo."
  (anvil-server-with-error-handling
   (or (anvil-git-repo-root path) "null")))

(defun anvil-git--tool-head-sha (path &optional short)
  "Return HEAD SHA for the repo containing PATH.

MCP Parameters:
  path  - Directory inside the repo.
  short - Truthy → return the abbreviated SHA."
  (anvil-server-with-error-handling
   (or (anvil-git-head-sha path (and short (not (equal short ""))))
       "null")))

(defun anvil-git--tool-branch-current (path)
  "Return the current branch for PATH, or nil when detached.

MCP Parameters:
  path - Directory inside the repo."
  (anvil-server-with-error-handling
   (or (anvil-git-branch-current path) "null")))

(defun anvil-git--tool-log (path &optional limit)
  "Return recent commits for repo at PATH.

MCP Parameters:
  path  - Directory inside the repo.
  limit - Max commits to return (integer or numeric string)."
  (anvil-server-with-error-handling
   (let ((n (cond ((integerp limit) limit)
                  ((and (stringp limit)
                        (string-match "\\`[0-9]+\\'" limit))
                   (string-to-number limit))
                  (t nil))))
     (or (anvil-git-log path n) "[]"))))

(defun anvil-git--tool-diff-names (path &optional from to)
  "Return changed paths between FROM and TO in repo at PATH.

MCP Parameters:
  path - Directory inside the repo.
  from - Base revision (optional).
  to   - Target revision (optional)."
  (anvil-server-with-error-handling
   (let ((f  (and (stringp from) (not (string-empty-p from)) from))
         (t* (and (stringp to)   (not (string-empty-p to))   to)))
     (or (anvil-git-diff-names path f t*) "[]"))))

(defun anvil-git--tool-diff-stats (path &optional rev)
  "Return structured diff counts for repo at PATH.

MCP Parameters:
  path - Directory inside the repo.
  rev  - Optional revision (e.g. \"HEAD~1\" or \"main..HEAD\")."
  (anvil-git--mcp-encode
   (anvil-server-with-error-handling
    (let ((r (and (stringp rev) (not (string-empty-p rev)) rev)))
      (anvil-git-diff-stats path r)))))

(defun anvil-git--tool-status (path)
  "Return porcelain status + branch info for repo at PATH.

MCP Parameters:
  path - Directory inside the repo."
  (anvil-git--mcp-encode
   (anvil-server-with-error-handling
    (anvil-git-status path))))

(defun anvil-git--tool-worktree-list (path)
  "Return attached worktrees for repo at PATH.

MCP Parameters:
  path - Directory inside the repo."
  (anvil-server-with-error-handling
   (or (anvil-git-worktree-list path) "[]")))

;;;; --- module lifecycle ---------------------------------------------------

(defun anvil-git--register-tools ()
  "Register git-* MCP tools under `anvil-git--server-id'."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-git--tool-repo-root)
   :id "git-repo-root"
   :intent '(git read)
   :layer 'workflow
   :server-id anvil-git--server-id
   :description
   "Return the git top-level directory for PATH, or nil when PATH is
not inside a repository.  PATH must be a directory."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-git--tool-head-sha)
   :id "git-head-sha"
   :intent '(git read)
   :layer 'workflow
   :server-id anvil-git--server-id
   :description
   "Return the HEAD commit SHA for the repo containing PATH.  Pass
short=1 for the abbreviated form."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-git--tool-branch-current)
   :id "git-branch-current"
   :intent '(git read)
   :layer 'workflow
   :server-id anvil-git--server-id
   :description
   "Return the current branch name, or nil when HEAD is detached."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-git--tool-log)
   :id "git-log"
   :intent '(git read)
   :layer 'workflow
   :server-id anvil-git--server-id
   :description
   "Return recent commits as (hash, date, author, subject) plists.
limit defaults to 20."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-git--tool-diff-names)
   :id "git-diff-names"
   :intent '(git read)
   :layer 'workflow
   :server-id anvil-git--server-id
   :description
   "Return the paths differing between FROM and TO (defaults
unstaged-vs-HEAD)."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-git--tool-diff-stats)
   :id "git-diff-stats"
   :intent '(git read)
   :layer 'workflow
   :server-id anvil-git--server-id
   :description
   "Return structured diff counts (files, insertions, deletions)
for REV or unstaged-vs-HEAD."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-git--tool-status)
   :id "git-status"
   :intent '(git read)
   :layer 'workflow
   :server-id anvil-git--server-id
   :description
   "Return porcelain status + branch/upstream/ahead/behind counts
as one plist.  Buckets: staged / modified / untracked / unmerged."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-git--tool-worktree-list)
   :id "git-worktree-list"
   :intent '(git read)
   :layer 'workflow
   :server-id anvil-git--server-id
   :description
   "Return attached git worktrees as plists (path, head, branch,
bare, detached)."
   :read-only t))

(defun anvil-git--unregister-tools ()
  "Remove every git-* MCP tool from the shared server."
  (dolist (id '("git-repo-root" "git-head-sha" "git-branch-current"
                "git-log" "git-diff-names" "git-diff-stats"
                "git-status" "git-worktree-list"))
    (anvil-server-unregister-tool id anvil-git--server-id)))

;;;###autoload
(defun anvil-git-enable ()
  "Register git-* MCP tools."
  (interactive)
  (anvil-git--register-tools))

(defun anvil-git-disable ()
  "Unregister git-* MCP tools."
  (interactive)
  (anvil-git--unregister-tools))

;;;; --- discoverability ----------------------------------------------------

(defun anvil-git-helpers-list ()
  "Return a list of all anvil-git* user-facing function names."
  (let (results)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (let ((n (symbol-name sym)))
                    (and (string-prefix-p "anvil-git" n)
                         (not (string-match-p "--" n)))))
         (push sym results))))
    (sort results (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(provide 'anvil-git)
;;; anvil-git.el ends here
