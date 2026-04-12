;;; anvil-git.el --- Git operations for anvil -*- lexical-binding: t; -*-

;; Author: zawatton21
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
(require 'subr-x)
(require 'anvil-host)

;;;; --- internal: shell wrapper --------------------------------------------

(defun anvil-git--run (args repo &optional opts)
  "Run `git -c core.quotepath=false ARGS' in REPO. Returns anvil-shell plist.
ARGS is a single string already shell-quoted by the caller. OPTS is
merged into the anvil-shell call (cwd is forced to REPO if non-nil)."
  (let* ((cmd (concat "git -c core.quotepath=false " args))
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
