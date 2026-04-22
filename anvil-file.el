;;; anvil-file.el --- Safe file and org editing for anvil -*- lexical-binding: t; -*-

;; Author: zawatton
;; Keywords: tools, mcp, claude

;;; Commentary:

;; Claude Code が emacs-eval (mcp__org-mcp__emacs-eval) 経由で呼び出す
;; ための helper 群。目的:
;;
;;   1. マウント層を経由せず大ファイル (>1.2MB) を安全に編集
;;      (init.org / journals-*.org のような巨大 org ファイル)
;;   2. 同一ファイル内の複数編集を 1 call で完結 → Claude のトークン削減
;;   3. Claude が毎回 with-temp-buffer + insert-file-contents を書かずに済む
;;
;; 全ての file-* helper は `with-temp-buffer' + `insert-file-contents' +
;; `write-region' パターン (buffer 副作用なし、auto-revert への影響なし)。
;;
;; 命名規約:
;;   anvil-file-*  汎用ファイル編集 (任意の text file)
;;   anvil-org-*   org 構造編集 (allowed-files 制限なし)
;;
;; 戻り値: 成功時は plist (例: (:replaced 3 :file "/abs/path"))。
;;         Claude は plist の件数フィールドで成否を確認できる。
;; 失敗時: (error "my-cc: ...") を投げる。emacs-eval は文字列化して返す。
;;
;; API リファレンス: ~/Cowork/Notes/.claude/anvil-helpers-reference.org
;;
;; Claude Code 側の使用方針 (CLAUDE.md 参照):
;;   - 単発の局所編集     → Edit tool (こちらが軽い)
;;   - 1.2MB 超のファイル → anvil-file-* / org-mcp
;;   - regex 一括置換     → anvil-file-replace-regexp
;;   - 行範囲挿入/削除    → anvil-file-insert-at-line / delete-lines
;;   - 同一ファイル 3+ 編集 → anvil-file-* (1 call にまとめる)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'anvil-disk)
(require 'anvil-server)   ; provides `anvil-server-with-error-handling' macro

;;;; --- internal -----------------------------------------------------------

(defun anvil--prepare-path (path)
  "Expand PATH and verify it exists as a regular file. Return absolute path."
  (let ((abs (expand-file-name path)))
    (unless (file-exists-p abs)
      (error "my-cc: file not found: %s" abs))
    (unless (file-regular-p abs)
      (error "my-cc: not a regular file: %s" abs))
    abs))

(defun anvil--write-current-buffer-to (path)
  "Write current temp buffer content to PATH atomically.
Uses `write-region' with VISIT nil so no buffer is created."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (point-min) (point-max) path nil 'silent)))

(defun anvil--insert-file (path)
  "Insert PATH into current buffer with UTF-8 forced.
Auto-detection in `insert-file-contents' silently falls back to
raw-text on some inputs (notably journals-2026.org), which then
breaks string comparison against literal Unicode in source code
because the buffer holds 8-bit raw bytes instead of decoded
codepoints. Forcing utf-8 here makes string-equal predictable for
all anvil-file-* and anvil-org-* helpers."
  (let ((coding-system-for-read 'utf-8))
    (insert-file-contents path)))

;;;; --- file: read ---------------------------------------------------------

(defun anvil-file-read (path &optional offset limit)
  "Read file PATH and return its content as a string.
OFFSET is the 0-based line offset to start from (default 0).
LIMIT is the maximum number of lines to return (default all).
Returns (:file PATH :content STR :total-lines N :offset OFFSET
         :lines-returned N :warnings LIST).
:warnings is nil when the file has no visited buffer or is in-sync;
otherwise a list of human-readable strings flagging divergence
(see `anvil-file-warn-if-diverged')."
  (let* ((abs (anvil--prepare-path path))
         (warnings (anvil-file-warn-if-diverged abs)))
    (with-temp-buffer
      (anvil--insert-file abs)
      (let ((total (count-lines (point-min) (point-max)))
            (off (or offset 0))
            (lim limit))
        (goto-char (point-min))
        (forward-line off)
        (let* ((beg (point))
               (end (if lim
                        (progn (forward-line lim) (point))
                      (point-max)))
               (content (buffer-substring-no-properties beg end))
               (lines-returned (count-lines beg end)))
          (list :file abs :content content
                :total-lines total :offset off
                :lines-returned lines-returned
                :warnings warnings))))))

(defun anvil-file-read-region (path start-line end-line)
  "Read PATH from START-LINE to END-LINE inclusive (1-indexed).
Returns the text as a string."
  (let ((abs (anvil--prepare-path path)))
    (with-temp-buffer
      (anvil--insert-file abs)
      (goto-char (point-min))
      (forward-line (1- start-line))
      (let ((beg (point)))
        (forward-line (1+ (- end-line start-line)))
        (buffer-substring-no-properties beg (point))))))

(defun anvil-file-read-regexp (path pattern &optional context)
  "Search PATH for PATTERN (regexp) and return matches.
CONTEXT is the number of context lines around each match (default 0).
Returns a list of plists: ((:line N :match \"...\" :context \"...\") ...).
Returns nil if no matches found (does not error)."
  (let ((abs (anvil--prepare-path path))
        (ctx (or context 0))
        (results '()))
    (with-temp-buffer
      (anvil--insert-file abs)
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
        (let* ((line-num (line-number-at-pos (match-beginning 0)))
               (match-text (match-string-no-properties 0))
               (ctx-beg (save-excursion
                          (goto-char (match-beginning 0))
                          (forward-line (- ctx))
                          (line-beginning-position)))
               (ctx-end (save-excursion
                          (goto-char (match-end 0))
                          (forward-line ctx)
                          (line-end-position))))
          (push (list :line line-num
                      :match match-text
                      :context (buffer-substring-no-properties ctx-beg ctx-end))
                results))
        ;; Avoid infinite loop on zero-width matches.
        (when (= (match-beginning 0) (match-end 0))
          (if (eobp) (goto-char (point-max)) (forward-char 1)))))
    (nreverse results)))

(defun anvil-file-line-count (path)
  "Return line count of PATH."
  (let ((abs (anvil--prepare-path path)))
    (with-temp-buffer
      (anvil--insert-file abs)
      (count-lines (point-min) (point-max)))))

;;;; --- file: write --------------------------------------------------------

(defun anvil-file-replace-regexp (path pattern replacement &optional max-count)
  "In PATH, replace occurrences of PATTERN (regexp) with REPLACEMENT.
If MAX-COUNT is non-nil, stop after that many replacements.
REPLACEMENT may use \\1 \\2 etc. for capture groups.
Returns (:replaced N :file PATH :warnings LIST).  Errors if 0
replacements were made.  :warnings surfaces pre-write divergence
with any visited buffer (see `anvil-file-warn-if-diverged')."
  (let* ((abs (anvil--prepare-path path))
         (warnings (anvil-file-warn-if-diverged abs))
         (count 0))
    (with-temp-buffer
      (anvil--insert-file abs)
      (goto-char (point-min))
      (while (and (re-search-forward pattern nil t)
                  (or (null max-count) (< count max-count)))
        (replace-match replacement nil nil)
        (cl-incf count))
      (when (zerop count)
        (error "my-cc: pattern not found in %s: %s" abs pattern))
      (anvil--write-current-buffer-to abs))
    (list :replaced count :file abs :warnings warnings)))

(defun anvil-file-replace-string (path old-string new-string &optional max-count)
  "In PATH, replace literal OLD-STRING with NEW-STRING.
If MAX-COUNT is non-nil, stop after that many replacements.
Returns (:replaced N :file PATH :warnings LIST).  Errors if 0
replacements were made.  Pass MAX-COUNT 1 to assert exactly-one
match (will still error on 0).

:warnings is computed *before* the disk write from
`anvil-file-warn-if-diverged'.  It flags the case where a visited
buffer has unsaved edits, so the caller can choose to refresh the
buffer or abort a follow-up action; the disk write itself is not
refused (disk-first contract)."
  (let* ((abs (anvil--prepare-path path))
         (warnings (anvil-file-warn-if-diverged abs))
         (count 0))
    (with-temp-buffer
      (anvil--insert-file abs)
      (goto-char (point-min))
      (while (and (search-forward old-string nil t)
                  (or (null max-count) (< count max-count)))
        (replace-match new-string t t)
        (cl-incf count))
      (when (zerop count)
        (error "my-cc: string not found in %s: %s" abs old-string))
      (anvil--write-current-buffer-to abs))
    (list :replaced count :file abs :warnings warnings)))

(defun anvil-file-insert-at-line (path line content)
  "Insert CONTENT into PATH at LINE (1-indexed). LINE 1 = before first line.
A trailing newline is added to CONTENT if not present.
Returns (:line LINE :inserted-bytes N :file PATH :warnings LIST)."
  (let* ((abs (anvil--prepare-path path))
         (warnings (anvil-file-warn-if-diverged abs))
         (text (if (string-suffix-p "\n" content)
                   content
                 (concat content "\n"))))
    (with-temp-buffer
      (anvil--insert-file abs)
      (goto-char (point-min))
      (forward-line (1- line))
      (insert text)
      (anvil--write-current-buffer-to abs))
    (list :line line :inserted-bytes (length text)
          :file abs :warnings warnings)))

(defun anvil-file-delete-lines (path start-line end-line)
  "Delete lines START-LINE through END-LINE inclusive (1-indexed) from PATH.
Returns (:deleted N :file PATH :warnings LIST)."
  (when (> start-line end-line)
    (error "my-cc: start-line %d > end-line %d" start-line end-line))
  (let* ((abs (anvil--prepare-path path))
         (warnings (anvil-file-warn-if-diverged abs))
         (deleted (1+ (- end-line start-line))))
    (with-temp-buffer
      (anvil--insert-file abs)
      (goto-char (point-min))
      (forward-line (1- start-line))
      (let ((beg (point)))
        (forward-line deleted)
        (delete-region beg (point)))
      (anvil--write-current-buffer-to abs))
    (list :deleted deleted :file abs :warnings warnings)))

(defun anvil-file-append (path content)
  "Append CONTENT to end of PATH.
A leading newline is added if file does not end with one.
Returns (:appended-bytes N :file PATH :warnings LIST)."
  (let* ((abs (anvil--prepare-path path))
         (warnings (anvil-file-warn-if-diverged abs))
         bytes)
    (with-temp-buffer
      (anvil--insert-file abs)
      (goto-char (point-max))
      (unless (or (= (point) (point-min))
                  (eq (char-before) ?\n))
        (insert "\n"))
      (let ((before (point)))
        (insert content)
        (setq bytes (- (point) before)))
      (anvil--write-current-buffer-to abs))
    (list :appended-bytes bytes :file abs :warnings warnings)))

(defun anvil-file-prepend (path content)
  "Prepend CONTENT to beginning of PATH.
A trailing newline is added to CONTENT if not present.
Returns (:prepended-bytes N :file PATH :warnings LIST)."
  (let* ((abs (anvil--prepare-path path))
         (warnings (anvil-file-warn-if-diverged abs))
         (text (if (string-suffix-p "\n" content)
                   content
                 (concat content "\n"))))
    (with-temp-buffer
      (anvil--insert-file abs)
      (goto-char (point-min))
      (insert text)
      (anvil--write-current-buffer-to abs))
    (list :prepended-bytes (length text) :file abs :warnings warnings)))

(defun anvil-file-create (path content &optional overwrite)
  "Create new file at PATH with CONTENT.
Errors if file exists unless OVERWRITE is non-nil.
Parent directory must exist (will not be created automatically).
Returns (:created PATH :bytes N)."
  (let ((abs (expand-file-name path)))
    (when (and (file-exists-p abs) (not overwrite))
      (error "my-cc: file exists: %s (pass overwrite to replace)" abs))
    (let ((dir (file-name-directory abs)))
      (unless (file-directory-p dir)
        (error "my-cc: parent directory does not exist: %s" dir)))
    (with-temp-buffer
      (insert content)
      (anvil--write-current-buffer-to abs))
    (list :created abs :bytes (length content))))

;;;; --- org: structural edits ----------------------------------------------

;; org-mcp の allowed-files 制限外 (estimates/, invoices/, capture/web/ 等)
;; の org ファイル編集用。allowed-files 内 (todo.org / journals-*.org /
;; init.org) は org-mcp の write tools を優先する。

(defun anvil--org-find-heading (heading-path)
  "In current org buffer, move point to heading matching HEADING-PATH.
HEADING-PATH is a list of heading title strings from outermost to innermost.
Returns t if found, nil otherwise."
  (require 'org)
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (re-search-forward org-heading-regexp nil t))
      (when (equal (org-get-outline-path t) heading-path)
        (setq found t)
        (beginning-of-line)))
    found))

(defun anvil-org-edit-body (path heading-path new-body)
  "In org file PATH, replace the body of heading at HEADING-PATH with NEW-BODY.
HEADING-PATH is a list of heading title strings from outermost to innermost,
e.g. (\"Top Level\" \"Section\" \"Leaf\").
The body is the content between the heading line (and its property drawer,
if any) and the next heading. Subheadings are preserved.
Returns (:file PATH :heading HEADING-PATH)."
  (require 'org)
  (let ((abs (anvil--prepare-path path)))
    (with-temp-buffer
      (anvil--insert-file abs)
      (delay-mode-hooks (org-mode))
      (unless (anvil--org-find-heading heading-path)
        (error "my-cc: heading not found in %s: %S" abs heading-path))
      ;; point is at heading start. Move past heading line + meta data.
      (forward-line 1)
      (org-end-of-meta-data t)
      (let* ((body-beg (point))
             ;; Body ends at next heading (any level) or EOF.
             (body-end (save-excursion
                         (if (re-search-forward org-heading-regexp nil t)
                             (line-beginning-position)
                           (point-max)))))
        (delete-region body-beg body-end)
        (goto-char body-beg)
        (insert new-body)
        (unless (string-suffix-p "\n" new-body)
          (insert "\n")))
      (anvil--write-current-buffer-to abs))
    (list :file abs :heading heading-path)))

(defun anvil-org-append-to-heading (path heading-path content)
  "In org file PATH, append CONTENT after the existing body of heading at
HEADING-PATH (before any subheadings). Useful for adding new MEMO entries
to a NOTE section without disturbing existing content.
Returns (:file PATH :heading HEADING-PATH :appended-bytes N)."
  (require 'org)
  (let ((abs (anvil--prepare-path path))
        bytes)
    (with-temp-buffer
      (anvil--insert-file abs)
      (delay-mode-hooks (org-mode))
      (unless (anvil--org-find-heading heading-path)
        (error "my-cc: heading not found in %s: %S" abs heading-path))
      (forward-line 1)
      (org-end-of-meta-data t)
      ;; Skip existing body up to (but not including) the next heading.
      (let ((body-end (save-excursion
                        (if (re-search-forward org-heading-regexp nil t)
                            (line-beginning-position)
                          (point-max)))))
        (goto-char body-end))
      ;; Ensure separation.
      (unless (or (bobp) (eq (char-before) ?\n))
        (insert "\n"))
      (let ((before (point)))
        (insert content)
        (unless (string-suffix-p "\n" content)
          (insert "\n"))
        (setq bytes (- (point) before)))
      (anvil--write-current-buffer-to abs))
    (list :file abs :heading heading-path :appended-bytes bytes)))

(defun anvil-journal--default-file (date)
  "Return the default journals-YYYY.org path for DATE (\"YYYY-MM-DD\")."
  (expand-file-name
   (format "~/Cowork/Notes/capture/journals-%s.org" (substring date 0 4))))

(defun anvil-journal--day-suffix (date)
  "Return English 3-letter weekday for DATE string (e.g. \"Sun\")."
  (let ((system-time-locale "C"))
    (format-time-string "%a" (date-to-time (concat date " 00:00:00")))))

(defun anvil-journal-append-memo (memo-title body &optional date file)
  "Append a MEMO entry under today's 作業ログ section in a journal file.

DATE defaults to today (string \"YYYY-MM-DD\").
FILE defaults to ~/Cowork/Notes/capture/journals-<year>.org for DATE.

The function:
  1. Locates the top-level headline `* DATE-...' (e.g. `* 2026-04-12-Sunday').
     Errors if missing.
  2. Within that section, locates `** NOTE 作業ログ <DATE ...>' by date prefix.
     If missing, creates one as `** NOTE 作業ログ <DATE Wkd>' where Wkd is
     the English 3-letter abbreviation. Existing headlines preserve their
     own day-of-week tag (Japanese or English).
  3. Appends a `*** MEMO AI: TITLE <DATE Wkd>' subheading at the end of
     the inner section's body, with BODY inserted verbatim afterwards.

BODY is inserted unchanged. Convention: indent each bullet line with
4 spaces (org list style). The helper does not auto-indent.

Returns:
  (:file PATH :date DATE :memo-title TITLE :inner-section-existed BOOL
   :appended-bytes N)

Errors with `(error \"my-cc: ...\")' on missing top-level headline."
  (require 'org)
  (let* ((date (or date (format-time-string "%Y-%m-%d")))
         (path (or file (anvil-journal--default-file date)))
         (abs (anvil--prepare-path path))
         (default-suffix (anvil-journal--day-suffix date))
         ;; Build the final MEMO chunk only after inner-suffix is known.
         ;; Pre-formatting and patching with `format' is unsafe because
         ;; BODY may contain literal `%' characters.
         (build-memo
          (lambda (suffix)
            (concat
             (format "*** MEMO AI: %s <%s %s>\n" memo-title date suffix)
             body
             (if (string-suffix-p "\n" body) "" "\n"))))
         inner-existed inner-suffix bytes)
    (with-temp-buffer
      (anvil--insert-file abs)
      (delay-mode-hooks (org-mode))
      ;; --- locate top-level `* DATE-...' headline ---
      (goto-char (point-min))
      (unless (re-search-forward
               (format "^\\* %s-" (regexp-quote date)) nil t)
        (error "my-cc: top-level headline `* %s-...' not found in %s"
               date abs))
      (forward-line 0)
      (let ((top-end (save-excursion
                       (forward-line 1)
                       (if (re-search-forward "^\\* " nil t)
                           (line-beginning-position)
                         (point-max)))))
        ;; --- locate inner `** NOTE 作業ログ <DATE ...>' headline ---
        (forward-line 1)
        (cond
         ((re-search-forward
           (format "^\\*\\* NOTE 作業ログ <%s \\([^>]+\\)>"
                   (regexp-quote date))
           top-end t)
          (setq inner-existed t
                inner-suffix (match-string 1))
          ;; Walk forward to body end (next ** or higher heading, or top-end).
          (forward-line 1)
          (let ((body-end (save-excursion
                            (if (re-search-forward "^\\*\\{1,2\\} " top-end t)
                                (line-beginning-position)
                              top-end))))
            (goto-char body-end)
            (unless (or (bobp) (eq (char-before) ?\n))
              (insert "\n"))
            (let ((before (point)))
              (insert (funcall build-memo inner-suffix))
              (setq bytes (- (point) before)))))
         (t
          ;; Create the inner section, then insert the memo inside it.
          (setq inner-suffix default-suffix)
          (goto-char top-end)
          (unless (eq (char-before) ?\n) (insert "\n"))
          (let ((before (point)))
            (insert (format "** NOTE 作業ログ <%s %s>\n" date inner-suffix))
            (insert (funcall build-memo inner-suffix))
            (setq bytes (- (point) before))))))
      (anvil--write-current-buffer-to abs))
    (list :file                  abs
          :date                  date
          :memo-title            memo-title
          :inner-section-existed (and inner-existed t)
          :inner-suffix          inner-suffix
          :appended-bytes        bytes)))

(defun anvil-org-headlines (path &optional filter)
  "Return a list of headline plists for org file PATH.

FILTER plist:
  :level N         exact level only
  :max-depth N     levels 1..N only
  :state STR-or-LIST  TODO state(s) — string or list of strings
  :tag TAG         must include this tag
  :match REGEXP    headline title regexp

Each element:
  (:level N :title STR :tags (..) :todo STR-or-nil
   :scheduled STR-or-nil :deadline STR-or-nil :path-list (..))

:path-list contains ancestor titles only (excluding the headline
itself); use :title for the leaf. The walk uses temp-buffer + scope
nil so the file does not need to be visited."
  (require 'org)
  (let* ((abs (anvil--prepare-path path))
         (level     (plist-get filter :level))
         (max-depth (plist-get filter :max-depth))
         (state-f   (plist-get filter :state))
         (state-list (cond ((null state-f) nil)
                           ((stringp state-f) (list state-f))
                           ((listp state-f) state-f)))
         (tag-f     (plist-get filter :tag))
         (match-f   (plist-get filter :match))
         results)
    (with-temp-buffer
      (anvil--insert-file abs)
      (delay-mode-hooks (org-mode))
      (org-map-entries
       (lambda ()
         (let* ((cur-level (org-current-level))
                (todo (org-get-todo-state))
                (tags (mapcar #'substring-no-properties (org-get-tags)))
                (heading (substring-no-properties
                          (or (org-get-heading t t t t) "")))
                (sched (org-entry-get nil "SCHEDULED"))
                (deadl (org-entry-get nil "DEADLINE"))
                (path-list (mapcar #'substring-no-properties
                                   (org-get-outline-path nil))))
           (when (and (or (null level) (and cur-level (= level cur-level)))
                      (or (null max-depth)
                          (and cur-level (<= cur-level max-depth)))
                      (or (null state-list)
                          (and todo (member todo state-list)))
                      (or (null tag-f) (member tag-f tags))
                      (or (null match-f) (string-match-p match-f heading)))
             (push (list :level     cur-level
                         :title     heading
                         :tags      tags
                         :todo      todo
                         :scheduled sched
                         :deadline  deadl
                         :path-list path-list)
                   results))))
       t nil))
    (nreverse results)))

(defun anvil-org-property-search (path property value-or-regexp &optional opts)
  "Search org file PATH for entries with PROPERTY matching VALUE-OR-REGEXP.

VALUE-OR-REGEXP is a regexp matched against the property value
(use `regexp-quote' for literal match).

OPTS plist:
  :inherit BOOL    if t, use `org-entry-get' inheritance (default nil)
  :state STR-or-LIST  filter by TODO state too
  :limit N         cap result count

Each result element:
  (:title STR :level N :todo STR-or-nil :path-list (..)
   :property-value STR :all-properties ((KEY . VALUE) ..))

For point-of-interest queries on inspection trackers / 見積管理 etc."
  (require 'org)
  (let* ((abs (anvil--prepare-path path))
         (inherit (plist-get opts :inherit))
         (state-f (plist-get opts :state))
         (state-list (cond ((null state-f) nil)
                           ((stringp state-f) (list state-f))
                           ((listp state-f) state-f)))
         (limit (plist-get opts :limit))
         results)
    (with-temp-buffer
      (anvil--insert-file abs)
      (delay-mode-hooks (org-mode))
      (org-map-entries
       (lambda ()
         (let* ((val (org-entry-get nil property inherit))
                (todo (org-get-todo-state)))
           (when (and val
                      (string-match-p value-or-regexp val)
                      (or (null state-list)
                          (and todo (member todo state-list))))
             (push (list :title (substring-no-properties
                                 (or (org-get-heading t t t t) ""))
                         :level (org-current-level)
                         :todo todo
                         :path-list (mapcar #'substring-no-properties
                                            (org-get-outline-path nil))
                         :property-value val
                         :all-properties (org-entry-properties nil 'standard))
                   results))))
       t nil))
    (let ((sorted (nreverse results)))
      (if (and limit (> (length sorted) limit))
          (seq-take sorted limit)
        sorted))))

(defun anvil-org-todo-summary (path &optional filter)
  "Return a TODO summary for org file PATH as a plist.

FILTER plist:
  :state STATES   list of TODO keyword strings to include (default: all)
  :tags TAGS      list of tag strings, ALL must be present on the entry

Returned plist:
  (:total N
   :by-state ((STATE . N) ..)            sorted by count desc
   :overdue ((:heading :scheduled :state) ..)
   :today ((:heading :scheduled :state) ..)
   :upcoming-7d ((:heading :scheduled :state) ..))

Scheduled buckets are derived from org SCHEDULED timestamps. ISO
date string comparison is used for today / overdue (timezone-safe);
upcoming-7d uses Emacs time math against the next 7 days.

This exists primarily so agenda-dashboard / inspection-monitor
skills can drop their ad-hoc org-map-entries loops."
  (require 'org)
  (let* ((abs (anvil--prepare-path path))
         (state-filter (plist-get filter :state))
         (tags-filter  (plist-get filter :tags))
         (today-str (format-time-string "%Y-%m-%d"))
         (week-end (time-add (current-time) (days-to-time 7)))
         (total 0)
         by-state overdue today-list upcoming-list)
    (with-temp-buffer
      (anvil--insert-file abs)
      (delay-mode-hooks (org-mode))
      (org-map-entries
       (lambda ()
         (let ((todo (org-get-todo-state))
               (tags (org-get-tags))
               (sched-str (org-entry-get nil "SCHEDULED")))
           (when (and todo
                      (or (null state-filter) (member todo state-filter))
                      (or (null tags-filter)
                          (cl-every (lambda (tag) (member tag tags)) tags-filter)))
             (cl-incf total)
             (let ((cell (assoc todo by-state)))
               (if cell
                   (setcdr cell (1+ (cdr cell)))
                 (push (cons todo 1) by-state)))
             (when (and sched-str
                        (string-match
                         "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
                         sched-str))
               (let* ((sched-day (match-string 1 sched-str))
                      (heading (substring-no-properties
                                (or (org-get-heading t t t t) "")))
                      (entry (list :heading heading
                                   :scheduled sched-day
                                   :state todo)))
                 (cond
                  ((string< sched-day today-str)
                   (push entry overdue))
                  ((string= sched-day today-str)
                   (push entry today-list))
                  ((let ((sched-time
                          (ignore-errors (org-time-string-to-time sched-str))))
                     (and sched-time (time-less-p sched-time week-end)))
                   (push entry upcoming-list))))))))
       t nil))
    (list :total       total
          :by-state    (sort by-state (lambda (a b) (> (cdr a) (cdr b))))
          :overdue     (nreverse overdue)
          :today       (nreverse today-list)
          :upcoming-7d (nreverse upcoming-list))))

;;;; --- org: read/write (org-mcp equivalent, no allowed-files restriction) --

(defun anvil--org-extract-heading-data ()
  "At current org heading, extract structured data as plist.
Point must be on a heading line.  Returns:
  (:title :level :todo :tags :body :properties :children-titles)"
  (let* ((title (org-get-heading t t t t))
         (level (org-current-level))
         (todo  (org-get-todo-state))
         (tags  (org-get-tags nil t))
         (props (org-entry-properties nil 'standard))
         ;; body: after heading line + meta-data, before first child or subtree end
         (body-beg (save-excursion
                     (forward-line 1)
                     (org-end-of-meta-data t)
                     (point)))
         (subtree-end (save-excursion (org-end-of-subtree t t) (point)))
         (first-child-pos (save-excursion
                            (and (org-goto-first-child) (point))))
         (body-end (or first-child-pos subtree-end))
         (body (string-trim-right
                (buffer-substring-no-properties body-beg body-end)))
         ;; children titles: headings at exactly level+1 within subtree
         (children
          (let (kids (child-level (1+ level)))
            (save-excursion
              (when first-child-pos
                (goto-char first-child-pos)
                (push (org-get-heading t t t t) kids)
                (while (org-get-next-sibling)
                  (when (= (org-current-level) child-level)
                    (push (org-get-heading t t t t) kids)))))
            (nreverse kids))))
    (list :title title :level level :todo todo :tags tags
          :body body :properties props :children-titles children)))

(defun anvil-org-read-outline (path &optional max-depth)
  "Return hierarchical heading structure of org file PATH as nested plists.
Each node: (:title :level :todo :tags :children (...)).
MAX-DEPTH limits heading levels returned (nil = unlimited).
Returns (:file PATH :outline <nested-list>)."
  (require 'org)
  (let ((abs (anvil--prepare-path path))
        flat)
    (with-temp-buffer
      (anvil--insert-file abs)
      (delay-mode-hooks (org-mode))
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (beginning-of-line)
        (let ((lv (org-current-level)))
          (when (or (null max-depth) (<= lv max-depth))
            (push (list :title (org-get-heading t t t t)
                        :level lv
                        :todo  (org-get-todo-state)
                        :tags  (org-get-tags nil t)
                        :children nil)
                  flat)))
        (forward-line 1)))
    ;; Build tree from flat list (reverse to restore order)
    (setq flat (nreverse flat))
    (let ((root (list :title "root" :level 0 :children nil))
          (stack nil))
      (push root stack)
      (dolist (node flat)
        (let ((lv (plist-get node :level)))
          ;; Pop stack until parent level < current level
          (while (and (cdr stack)
                      (>= (plist-get (car stack) :level) lv))
            (pop stack))
          ;; Append as child of stack top
          (let ((parent (car stack)))
            (plist-put parent :children
                       (append (plist-get parent :children) (list node))))
          (push node stack)))
      (list :file abs :outline (plist-get root :children)))))

(defun anvil-org-read-headline (path heading-path)
  "Read heading at HEADING-PATH in org file PATH.
HEADING-PATH is a list of title strings from outermost to innermost.
Returns plist: (:file :title :level :todo :tags :body :properties :children-titles)."
  (require 'org)
  (let ((abs (anvil--prepare-path path)))
    (with-temp-buffer
      (anvil--insert-file abs)
      (delay-mode-hooks (org-mode))
      (unless (anvil--org-find-heading heading-path)
        (error "my-cc: heading not found in %s: %S" abs heading-path))
      (let ((data (anvil--org-extract-heading-data)))
        (append (list :file abs) data)))))

(defun anvil-org-read-by-id (path id)
  "Read heading with :ID: property ID in org file PATH.
Returns same plist format as `anvil-org-read-headline'."
  (require 'org)
  (let ((abs (anvil--prepare-path path)))
    (with-temp-buffer
      (anvil--insert-file abs)
      (delay-mode-hooks (org-mode))
      (let ((pos (org-find-property "ID" id)))
        (unless pos
          (error "my-cc: ID %s not found in %s" id abs))
        (goto-char pos)
        (let ((data (anvil--org-extract-heading-data)))
          (append (list :file abs :id id) data))))))

(defun anvil-org-add-headline (path parent-path title &optional opts)
  "Add a new headline TITLE under PARENT-PATH in org file PATH.
PARENT-PATH is a list of ancestor title strings, or nil for top-level.
OPTS plist: :todo STATE, :tags (list of strings), :body TEXT.
Returns (:file PATH :heading-path <list> :title TITLE)."
  (require 'org)
  (let ((abs (anvil--prepare-path path))
        new-level heading-path)
    (with-temp-buffer
      (anvil--insert-file abs)
      (delay-mode-hooks (org-mode))
      (if parent-path
          (progn
            (unless (anvil--org-find-heading parent-path)
              (error "my-cc: parent heading not found in %s: %S" abs parent-path))
            (setq new-level (1+ (org-current-level)))
            (org-end-of-subtree t t))
        ;; Top-level: insert at end of file
        (setq new-level 1)
        (goto-char (point-max))
        (skip-chars-backward " \t\n"))
      ;; Ensure preceding newline
      (unless (bolp) (insert "\n"))
      (insert "\n" (make-string new-level ?*) " " title "\n")
      ;; Apply options
      (forward-line -1)
      (when (plist-get opts :todo)
        (let ((org-after-todo-state-change-hook nil)
              (org-log-done nil)
              (org-log-repeat nil))
          (org-todo (plist-get opts :todo))))
      (when (plist-get opts :tags)
        (org-set-tags (plist-get opts :tags)))
      (when (plist-get opts :body)
        (end-of-line)
        (insert "\n" (plist-get opts :body))
        (unless (string-suffix-p "\n" (plist-get opts :body))
          (insert "\n")))
      (setq heading-path (if parent-path
                             (append parent-path (list title))
                           (list title)))
      (anvil--write-current-buffer-to abs))
    (list :file abs :heading-path heading-path :title title)))

(defun anvil-org-rename-headline (path heading-path new-title)
  "Rename heading at HEADING-PATH to NEW-TITLE in org file PATH.
Returns (:file PATH :old-title OLD :new-title NEW-TITLE)."
  (require 'org)
  (let ((abs (anvil--prepare-path path)))
    (with-temp-buffer
      (anvil--insert-file abs)
      (delay-mode-hooks (org-mode))
      (unless (anvil--org-find-heading heading-path)
        (error "my-cc: heading not found in %s: %S" abs heading-path))
      (let ((old-title (org-get-heading t t t t)))
        (org-edit-headline new-title)
        (anvil--write-current-buffer-to abs)
        (list :file abs :old-title old-title :new-title new-title)))))

(defun anvil-org-update-todo-state (path heading-path new-state)
  "Set TODO state of heading at HEADING-PATH to NEW-STATE in org file PATH.
NEW-STATE is a string like \"TODO\", \"DONE\", or \"\" to clear.
Returns (:file PATH :heading TITLE :old-state OLD :new-state NEW-STATE)."
  (require 'org)
  (let ((abs (anvil--prepare-path path)))
    (with-temp-buffer
      (anvil--insert-file abs)
      (delay-mode-hooks (org-mode))
      (unless (anvil--org-find-heading heading-path)
        (error "my-cc: heading not found in %s: %S" abs heading-path))
      (let ((old-state (org-get-todo-state))
            (title (org-get-heading t t t t)))
        (let ((org-after-todo-state-change-hook nil)
              (org-log-done nil)
              (org-log-repeat nil))
          (org-todo (if (string-empty-p new-state) 'none new-state)))
        (anvil--write-current-buffer-to abs)
        (list :file abs :heading title
              :old-state old-state :new-state new-state)))))

;;;; --- elisp dev helpers --------------------------------------------------

(defun anvil-elisp-describe (name)
  "Describe function or variable NAME (string).
Auto-detects whether NAME is a function, variable, or both, and returns
the relevant documentation.  Returns a plist with :type, :name, and :doc."
  (let* ((sym (intern name))
         (is-fn (fboundp sym))
         (is-var (boundp sym))
         result)
    (unless (or is-fn is-var)
      (error "my-cc: symbol %s is neither a function nor a variable" name))
    (when is-fn
      (push (list :type "function"
                  :name name
                  :doc (with-temp-buffer
                         (let ((standard-output (current-buffer)))
                           (describe-function-1 sym)
                           (buffer-string))))
            result))
    (when is-var
      (let ((doc (documentation-property sym 'variable-documentation t)))
        (push (list :type "variable"
                    :name name
                    :doc (or doc "(no documentation)")
                    :custom-p (custom-variable-p sym)
                    :local-p (local-variable-if-set-p sym)
                    :source (ignore-errors
                              (find-lisp-object-file-name
                               sym 'defvar)))
              result)))
    (if (= 1 (length result))
        (car result)
      result)))

(defun anvil-elisp-definition (function-name)
  "Return the source code definition of FUNCTION-NAME (string).
Returns a plist with :name, :file, :line, and :source.
For C primitives, :source will be nil and :c-function t."
  (let* ((sym (intern function-name))
         (def (symbol-function sym)))
    (unless (fboundp sym)
      (error "my-cc: function %s is void" function-name))
    (if (subrp def)
        (list :name function-name :c-function t
              :doc (documentation sym t))
      (let ((loc (find-function-noselect sym t)))
        (unless (and loc (car loc))
          (error "my-cc: cannot locate source for %s" function-name))
        (with-current-buffer (car loc)
          (goto-char (cdr loc))
          (let ((start (point))
                (file (buffer-file-name))
                (line (line-number-at-pos)))
            (forward-sexp 1)
            (list :name function-name
                  :file file
                  :line line
                  :source (buffer-substring-no-properties start (point)))))))))

(defun anvil-elisp-info (symbol-name)
  "Look up SYMBOL-NAME in the Elisp Info manual.
Returns a plist with :symbol, :found, :node, :manual, and :content.
If not found, :found is nil and :content contains the error message."
  (let ((sym (intern symbol-name)))
    (condition-case err
        (save-window-excursion
          (with-temp-buffer
            (emacs-lisp-mode)
            (info-lookup-symbol sym)
            (let* ((info-buf (get-buffer "*info*"))
                   node manual content)
              (unless info-buf
                (error "Info buffer not created"))
              (with-current-buffer info-buf
                (goto-char (point-min))
                (when (re-search-forward
                       "^File: \\([^,]+\\),\\s-*Node: \\([^\n,]+\\)"
                       nil t)
                  (setq manual (match-string 1)
                        node (match-string 2)))
                (forward-line 1)
                (let ((start (point))
                      (end (or (save-excursion
                                 (when (re-search-forward "\n\^_" nil t)
                                   (match-beginning 0)))
                               (point-max))))
                  (setq content (buffer-substring-no-properties start end))))
              (list :symbol symbol-name
                    :found t
                    :node node
                    :manual manual
                    :content content))))
      (error
       (list :symbol symbol-name
             :found nil
             :content (format "Not found: %s" (error-message-string err)))))))

(defun anvil-elisp-byte-compile-check (file)
  "Byte-compile FILE and return warnings/errors without writing .elc.
Returns a plist with :file, :ok, :warnings (list of strings), and :error.
Parses *Compile-Log* buffer for warnings since byte-compile-log-warning-function
is unreliable in Emacs 30."
  (let* ((abs (expand-file-name file))
         (log-buf (get-buffer-create "*Compile-Log*"))
         marker warnings)
    (unless (file-exists-p abs)
      (error "my-cc: file not found: %s" abs))
    (with-current-buffer log-buf
      (goto-char (point-max))
      (setq marker (point)))
    (condition-case err
        (save-window-excursion
          (let* ((tmp (make-temp-file "bytecomp-" nil ".elc"))
                 (byte-compile-dest-file-function (lambda (_f) tmp))
                 (byte-compile-warnings t)
                 (result (byte-compile-file abs)))
            (when (file-exists-p tmp) (delete-file tmp))
            (with-current-buffer log-buf
              (let ((new-output (buffer-substring-no-properties
                                 marker (point-max))))
                (dolist (line (split-string new-output "\n" t))
                  (let ((trimmed (string-trim line)))
                    (when (string-match-p "\\(?:Warning\\|Error\\):" trimmed)
                      (push trimmed warnings))))))
            (setq warnings (nreverse warnings))
            (list :file abs
                  :ok (and result (null warnings))
                  :warnings warnings)))
      (error
       (list :file abs
             :ok nil
             :warnings (nreverse warnings)
             :error (error-message-string err))))))

;;;; --- discoverability ----------------------------------------------------

(defun anvil-helpers-list ()
  "Return a list of all anvil-* helper function names (as symbols)."
  (let (results)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (string-prefix-p "anvil-" (symbol-name sym))
                  (not (string-prefix-p "anvil--" (symbol-name sym))))
         (push sym results))))
    (sort results (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

;;;; --- MCP tool wrappers --------------------------------------------------
;;
;; These thin wrappers adapt anvil-file-* functions for direct MCP tool
;; registration.  MCP JSON-RPC delivers all parameters as strings, so
;; numeric arguments (line numbers, counts) need string-to-number
;; conversion.  Registering these as dedicated MCP tools eliminates the
;; shell + elisp-reader double-escaping problem that plagues emacs-eval
;; calls with backslash-heavy content (Windows paths, org source blocks).
;;
;; The wrapper docstrings include "MCP Parameters:" sections so that
;; anvil-server--generate-schema-from-function can extract descriptions.

(defvar anvil-file--server-id "emacs-eval"
  "Server ID under which anvil-file MCP tools are registered.")

(defun anvil-file--tool-replace-string (path old-string new-string &optional max-count)
  "Replace literal OLD-STRING with NEW-STRING in file at PATH.
Returns the number of replacements made.  Errors if no match found.

MCP Parameters:
  path - Absolute path to the file to edit
  old-string - The exact text to search for (literal, not regexp)
  new-string - The replacement text
  max-count - Maximum replacements to make (optional, e.g. \"1\" for single match)"
  (anvil-server-with-error-handling
    (let ((count (if (and max-count (not (string-empty-p max-count)))
                     (string-to-number max-count)
                   nil)))
      (format "%S" (anvil-file-replace-string path old-string new-string count)))))

(defun anvil-file--tool-replace-regexp (path pattern replacement &optional max-count)
  "Replace PATTERN (regexp) with REPLACEMENT in file at PATH.
REPLACEMENT may use \\\\1 \\\\2 etc. for capture groups.
Returns the number of replacements made.  Errors if no match found.

MCP Parameters:
  path - Absolute path to the file to edit
  pattern - Regular expression to search for
  replacement - Replacement string (may use \\\\1 \\\\2 for capture groups)
  max-count - Maximum replacements to make (optional)"
  (anvil-server-with-error-handling
    (let ((count (if (and max-count (not (string-empty-p max-count)))
                     (string-to-number max-count)
                   nil)))
      (format "%S" (anvil-file-replace-regexp path pattern replacement count)))))

(defun anvil-file--tool-insert-at-line (path line content)
  "Insert CONTENT into file at PATH before LINE (1-indexed).
A trailing newline is added if not present.

MCP Parameters:
  path - Absolute path to the file to edit
  line - Line number to insert before (1-indexed, e.g. \"1\" for start of file)
  content - Text to insert"
  (anvil-server-with-error-handling
    (format "%S" (anvil-file-insert-at-line path (string-to-number line) content))))

(defun anvil-file--tool-delete-lines (path start-line end-line)
  "Delete lines START-LINE through END-LINE (inclusive, 1-indexed) from PATH.

MCP Parameters:
  path - Absolute path to the file to edit
  start-line - First line to delete (1-indexed)
  end-line - Last line to delete (1-indexed, inclusive)"
  (anvil-server-with-error-handling
    (format "%S" (anvil-file-delete-lines path
                                          (string-to-number start-line)
                                          (string-to-number end-line)))))

(defun anvil-file--read-normalize-uri-args (path offset limit)
  "Return (PATH OFFSET LIMIT) with `file://' citation URI expanded.
When PATH is a `file://PATH[#L<s>[-<e>]]' URI the scheme is stripped,
and if the URI carries a line range it seeds OFFSET (start-1) and
LIMIT (end-start+1) when the caller did not supply explicit values."
  (if (and (stringp path) (string-prefix-p "file://" path))
      (let* ((parsed (and (fboundp 'anvil-uri-parse)
                          (anvil-uri-parse path))))
        (if parsed
            (let* ((p  (plist-get parsed :path))
                   (s  (plist-get parsed :line-start))
                   (e  (plist-get parsed :line-end))
                   (o  (or offset
                           (and s (number-to-string (max 0 (1- s))))))
                   (l  (or limit
                           (and s e
                                (number-to-string
                                 (max 1 (1+ (- e s))))))))
              (list p o l))
          ;; Fallback: naive prefix strip.
          (list (substring path (length "file://")) offset limit)))
    (list path offset limit)))

(defun anvil-file--tool-read (path &optional offset limit)
  "Read file at PATH and return its content.
Supports optional line-based pagination.  PATH may also be a
`file://PATH[#L<s>-<e>]' citation URI emitted by the disclosure
Layer-1 / Layer-2 tools; the embedded line range seeds offset/limit
when the caller did not supply them.

MCP Parameters:
  path - Absolute path to the file to read (or `file://' citation URI)
  offset - Lines to skip from start (optional, 0-indexed, e.g. \"100\")
  limit - Maximum lines to return (optional, e.g. \"50\")"
  (anvil-server-with-error-handling
    (require 'anvil-uri nil t)
    (pcase-let ((`(,p ,off-str ,lim-str)
                 (anvil-file--read-normalize-uri-args path offset limit)))
      (let ((off (if (and off-str (not (string-empty-p off-str)))
                     (string-to-number off-str)
                   nil))
            (lim (if (and lim-str (not (string-empty-p lim-str)))
                     (string-to-number lim-str)
                   nil)))
        (format "%S" (anvil-file-read p off lim))))))

(defun anvil-file--tool-append (path content)
  "Append CONTENT to end of file at PATH.

MCP Parameters:
  path - Absolute path to the file
  content - Text to append"
  (anvil-server-with-error-handling
    (format "%S" (anvil-file-append path content))))

;;;; --- batch operations ----------------------------------------------------
;;
;; Execute multiple file operations in a single MCP call.  This is the
;; most token-efficient way to perform bulk edits: instead of N round
;; trips through MCP, one call with a JSON array of operations.
;;
;; Operations JSON format:
;;   [
;;     {"op": "replace", "old": "text", "new": "replacement"},
;;     {"op": "replace-regexp", "pattern": "re", "replacement": "str"},
;;     {"op": "insert-at-line", "line": 10, "content": "text"},
;;     {"op": "delete-lines", "start": 5, "end": 8},
;;     {"op": "append", "content": "text"},
;;     {"op": "prepend", "content": "text"}
;;   ]
;;
;; All operations run sequentially on the same temp buffer, so line
;; numbers in later operations reflect changes made by earlier ones.
;; The file is written once at the end (atomic).

(defun anvil-file--batch-get (op key)
  "Get KEY from operation alist OP, trying both symbol and string keys."
  (or (alist-get key op)
      (alist-get (symbol-name key) op nil nil #'equal)))

(defun anvil-file--batch-get-number (op key)
  "Get KEY from OP and coerce to number if string."
  (let ((v (anvil-file--batch-get op key)))
    (when v (if (stringp v) (string-to-number v) v))))

(defun anvil-file-batch (path operations)
  "Execute a list of OPERATIONS on file at PATH in a single pass.
OPERATIONS is a list of alists, each with an `op' key and operation-specific
keys.  All operations run on the same buffer; the file is written once at end.
Returns (:ok t :operations N :file PATH :warnings LIST) on success.
:warnings surfaces pre-write divergence with any visited buffer."
  (let* ((abs (anvil--prepare-path path))
         (warnings (anvil-file-warn-if-diverged abs))
         (op-count 0))
    (with-temp-buffer
      (anvil--insert-file abs)
      (dolist (op operations)
        (let ((type (anvil-file--batch-get op 'op)))
          (pcase type
            ("replace"
             (let ((old (anvil-file--batch-get op 'old))
                   (new (anvil-file--batch-get op 'new))
                   (limit (anvil-file--batch-get-number op 'max-count)))
               (goto-char (point-min))
               (let ((count 0))
                 (while (and (search-forward old nil t)
                             (or (null limit) (< count limit)))
                   (replace-match new t t)
                   (cl-incf count))
                 (when (zerop count)
                   (error "batch: replace not found: %s"
                          (truncate-string-to-width old 80))))))
            ("replace-regexp"
             (let ((pattern (anvil-file--batch-get op 'pattern))
                   (replacement (anvil-file--batch-get op 'replacement))
                   (limit (anvil-file--batch-get-number op 'max-count)))
               (goto-char (point-min))
               (let ((count 0))
                 (while (and (re-search-forward pattern nil t)
                             (or (null limit) (< count limit)))
                   (replace-match replacement nil nil)
                   (cl-incf count))
                 (when (zerop count)
                   (error "batch: regexp not found: %s" pattern)))))
            ("insert-at-line"
             (let ((line (anvil-file--batch-get-number op 'line))
                   (content (anvil-file--batch-get op 'content)))
               (goto-char (point-min))
               (forward-line (1- line))
               (insert (if (string-suffix-p "\n" content)
                           content
                         (concat content "\n")))))
            ("delete-lines"
             (let ((start (anvil-file--batch-get-number op 'start))
                   (end (anvil-file--batch-get-number op 'end)))
               (goto-char (point-min))
               (forward-line (1- start))
               (let ((beg (point)))
                 (forward-line (1+ (- end start)))
                 (delete-region beg (point)))))
            ("append"
             (goto-char (point-max))
             (unless (bolp) (insert "\n"))
             (insert (anvil-file--batch-get op 'content)))
            ("prepend"
             (goto-char (point-min))
             (let ((c (anvil-file--batch-get op 'content)))
               (insert (if (string-suffix-p "\n" c) c (concat c "\n")))))
            (_
             (error "batch: unknown op: %s" type))))
        (cl-incf op-count))
      (anvil--write-current-buffer-to abs))
    (list :ok t :operations op-count :file abs :warnings warnings)))

(defun anvil-file--tool-batch (path operations)
  "Execute multiple file operations on PATH in a single call.
OPERATIONS is a JSON array of operation objects.  Each object must have
an \"op\" field and operation-specific fields.

Supported operations:
  {\"op\": \"replace\", \"old\": \"text\", \"new\": \"replacement\", \"max-count\": 1}
  {\"op\": \"replace-regexp\", \"pattern\": \"re\", \"replacement\": \"str\"}
  {\"op\": \"insert-at-line\", \"line\": 10, \"content\": \"text to insert\"}
  {\"op\": \"delete-lines\", \"start\": 5, \"end\": 8}
  {\"op\": \"append\", \"content\": \"text to add at end\"}
  {\"op\": \"prepend\", \"content\": \"text to add at start\"}

All operations run sequentially on the same buffer.  Line numbers in
later operations reflect changes from earlier ones.  The file is
written atomically once at the end.

MCP Parameters:
  path - Absolute path to the file to edit
  operations - JSON array of operation objects (as a string)"
  (anvil-server-with-error-handling
    (let ((ops (json-parse-string operations :object-type 'alist :array-type 'list)))
      (format "%S" (anvil-file-batch path ops)))))

;;;; --- JSON object editing -------------------------------------------------
;;
;; Bulk-add key/value pairs to a top-level JSON object, preserving
;; existing formatting.  Designed for i18n workflows where hundreds of
;; translations need to be appended in a single MCP call.  Edit tool
;; alternatives require a long anchor old_string for each batch, wasting
;; tokens; this helper only needs the PAIRS payload.

(defun anvil--json-escape-string (s)
  "Return S escaped for use inside a JSON string literal.
Does NOT add surrounding quotes."
  (replace-regexp-in-string
   "[\"\\\\\b\f\n\r\t]"
   (lambda (m)
     (pcase (aref m 0)
       (?\" "\\\"")
       (?\\ "\\\\")
       (?\b "\\b")
       (?\f "\\f")
       (?\n "\\n")
       (?\r "\\r")
       (?\t "\\t")))
   s t t))

(defun anvil--json-format-kv (key value indent-cols)
  "Return formatted \"KEY\": \"VALUE\" line with INDENT-COLS leading spaces.
Both KEY and VALUE are escaped as JSON strings."
  (format "%s\"%s\": \"%s\""
          (make-string indent-cols ?\s)
          (anvil--json-escape-string key)
          (anvil--json-escape-string value)))

(defun anvil--json-detect-indent (buffer-content default)
  "Inspect BUFFER-CONTENT and return the indent width used for entries.
Falls back to DEFAULT (usually 2) if no indented string entry is found."
  (if (string-match "^\\( +\\)\"" buffer-content)
      (length (match-string 1 buffer-content))
    default))

(defun anvil--json-normalize-pairs (pairs)
  "Normalize PAIRS into list of (KEY . VALUE) cons cells.
Accepts cons cells, two-element lists, or alists.  Both KEY and VALUE
must be strings; raises otherwise."
  (mapcar
   (lambda (p)
     (let ((k nil) (v nil))
       (cond
        ((and (consp p) (stringp (car p)) (stringp (cdr p)))
         (setq k (car p) v (cdr p)))
        ((and (consp p) (stringp (car p)) (consp (cdr p)) (null (cddr p))
              (stringp (cadr p)))
         (setq k (car p) v (cadr p)))
        ((and (vectorp p) (= (length p) 2)
              (stringp (aref p 0)) (stringp (aref p 1)))
         (setq k (aref p 0) v (aref p 1)))
        (t (error "anvil-json: invalid pair (need string key+value): %S" p)))
       (cons k v)))
   pairs))

(defun anvil-json-object-add (path pairs &optional opts)
  "Add PAIRS to the top-level JSON object at PATH.

PAIRS is a list of (KEY . VALUE) cons cells or two-element lists.
Both KEY and VALUE must be strings.  Non-string JSON types are not
supported (would require typed encoding).

OPTS is a plist:
  :on-duplicate SYMBOL  `skip' (default) | `overwrite' | `error'.
  :indent NUMBER        Indentation width.  If omitted, detected from
                        the file's first entry line; falls back to 2.

The file's existing formatting is preserved.  New entries are inserted
just before the outermost closing `}'.  Comma handling: a missing
trailing comma on the previous last entry is added automatically; new
entries get trailing commas except the last one (to conform with strict
JSON parsers).

Returns plist (:added N :skipped M :overwritten K :file PATH)."
  (let* ((abs (anvil--prepare-path path))
         (on-dup (or (plist-get opts :on-duplicate) 'skip))
         (explicit-indent (plist-get opts :indent))
         (normalized (anvil--json-normalize-pairs pairs))
         (added 0) (skipped 0) (overwritten 0)
         (keys-to-insert '()))
    (with-temp-buffer
      (anvil--insert-file abs)
      (let* ((content (buffer-string))
             (indent (or explicit-indent
                         (anvil--json-detect-indent content 2)))
             (existing (ignore-errors
                         (json-parse-string content
                                            :object-type 'hash-table
                                            :null-object nil
                                            :false-object nil))))
        (unless (hash-table-p existing)
          (error "anvil-json: file is not a JSON object: %s" abs))
        ;; Classify each pair
        (dolist (p normalized)
          (let ((k (car p)) (v (cdr p)))
            (cond
             ((gethash k existing)
              (pcase on-dup
                ('skip (cl-incf skipped))
                ('error (error "anvil-json: duplicate key: %s" k))
                ('overwrite
                 ;; In-place replace: find the existing entry's line
                 ;; and rewrite the value portion.  We match the key
                 ;; literally (after escaping for regex) and capture
                 ;; everything up through the colon+space prefix.
                 (let* ((esc-key (regexp-quote
                                  (anvil--json-escape-string k)))
                        (pat (concat
                              "^\\([[:space:]]*\"" esc-key
                              "\"[[:space:]]*:[[:space:]]*\\)"
                              "\\(\"\\(?:\\\\.\\|[^\"\\\\]\\)*\"\\|"
                              "[-+0-9.eE]+\\|true\\|false\\|null\\)"))
                        (new-value-literal
                         (concat "\"" (anvil--json-escape-string v) "\"")))
                   (goto-char (point-min))
                   (if (re-search-forward pat nil t)
                       (progn
                         (replace-match (concat "\\1"
                                                (replace-regexp-in-string
                                                 "\\\\" "\\\\\\\\"
                                                 new-value-literal))
                                        t nil)
                         (cl-incf overwritten))
                     (error "anvil-json: overwrite failed for key %s" k))))))
             (t
              (push p keys-to-insert)))))
        (setq keys-to-insert (nreverse keys-to-insert))
        ;; Insert new pairs before closing }
        (when keys-to-insert
          (goto-char (point-max))
          (unless (re-search-backward "}[[:space:]\n]*\\'" nil t)
            (error "anvil-json: no closing '}' found"))
          (let ((close-point (point)))
            ;; Ensure previous entry has trailing comma.
            (save-excursion
              (skip-chars-backward " \t\n" (point-min))
              (when (and (> (point) (point-min))
                         (not (eq (char-before) ?{))
                         (not (eq (char-before) ?\,)))
                (insert ",")
                (cl-incf close-point)))
            (goto-char close-point)
            ;; Insert the new lines.  Newline before so we're at column 0.
            (unless (bolp) (insert "\n"))
            (let* ((n (length keys-to-insert))
                   (i 0))
              (dolist (p keys-to-insert)
                (insert (anvil--json-format-kv (car p) (cdr p) indent))
                (when (< i (1- n))
                  (insert ","))
                (insert "\n")
                (cl-incf i))))
          (setq added (length keys-to-insert))))
      (anvil--write-current-buffer-to abs))
    (list :added added :skipped skipped :overwritten overwritten :file abs)))

;;;; --- import line idempotent insert ---------------------------------------

(defun anvil-file-ensure-import (path import-line &optional opts)
  "Ensure IMPORT-LINE exists as a top-level line in PATH.

Idempotent.  If the line already matches verbatim anywhere in the file,
no change is made.

OPTS plist:
  :after-regex REGEXP  Insert after the last line matching REGEXP.
                       Default: \"^import \" (covers TS/JS/Python-ish
                       import blocks; use nil to always insert at top).
  :position SYMBOL     `after-last-match' (default) | `before-first-match'
                       | `top' (very first line) | `bottom'.

Returns plist (:inserted BOOL :line N :already-present BOOL :file PATH :warnings LIST)."
  (let* ((abs (anvil--prepare-path path))
         (warnings (anvil-file-warn-if-diverged abs))
         (after-regex (if (plist-member opts :after-regex)
                          (plist-get opts :after-regex)
                        "^import "))
         (position (or (plist-get opts :position) 'after-last-match)))
    (with-temp-buffer
      (anvil--insert-file abs)
      ;; Already present?  Match full line (leading/trailing whitespace
      ;; tolerated via forced anchoring).
      (goto-char (point-min))
      (let ((target-line (string-trim-right import-line)))
        (if (re-search-forward (concat "^"
                                       (regexp-quote target-line)
                                       "$")
                               nil t)
            (list :inserted nil :already-present t
                  :line (line-number-at-pos) :file abs
                  :warnings warnings)
          (let ((insert-line nil))
            (pcase position
              ('top (setq insert-line 1))
              ('bottom (setq insert-line nil))  ; special: append
              ('before-first-match
               (when after-regex
                 (goto-char (point-min))
                 (when (re-search-forward after-regex nil t)
                   (setq insert-line (line-number-at-pos
                                      (line-beginning-position))))))
              (_ ; after-last-match
               (when after-regex
                 (goto-char (point-max))
                 (when (re-search-backward after-regex nil t)
                   (setq insert-line (1+ (line-number-at-pos)))))))
            (cond
             ((eq position 'bottom)
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert target-line "\n")
              (let ((ln (1- (line-number-at-pos))))
                (anvil--write-current-buffer-to abs)
                (list :inserted t :already-present nil
                      :line ln :file abs :warnings warnings)))
             (insert-line
              (goto-char (point-min))
              (forward-line (1- insert-line))
              (insert target-line "\n")
              (anvil--write-current-buffer-to abs)
              (list :inserted t :already-present nil
                    :line insert-line :file abs :warnings warnings))
             (t
              ;; No matches: insert at top (line 1)
              (goto-char (point-min))
              (insert target-line "\n")
              (anvil--write-current-buffer-to abs)
              (list :inserted t :already-present nil
                    :line 1 :file abs :warnings warnings)))))))))

;;;; --- code transformation tools ----------------------------------------
;; Tool 1: anvil-code-extract-pattern  — read structured records from text
;; Tool 2: anvil-code-add-field-by-map — bulk-add a field to TS/JS objects

;;;; --- code-extract-pattern ------------------------------------------------

(defun anvil-code--brace-balance-end (start limit)
  "From START scan forward to find a matching `{...}' block.
Walk to the first `{' (not past LIMIT); return position just after the
matching `}', or nil if unbalanced.  Skips characters inside double-quoted
strings (`\\\\' and `\\\"' escapes honored).  LIMIT may be nil for no cap."
  (save-excursion
    (goto-char start)
    (when (re-search-forward "{" limit t)
      ;; Point now sits past the opening `{'.  Depth = 1.
      (let ((depth 1)
            (cap (or limit (point-max))))
        (catch 'found
          (while (and (> depth 0) (< (point) cap))
            (let ((c (char-after)))
              (cond
               ((eq c ?\")
                (forward-char 1)
                (while (and (< (point) cap)
                            (not (eq (char-after) ?\")))
                  (if (eq (char-after) ?\\)
                      (forward-char 2)
                    (forward-char 1)))
                (when (and (< (point) cap) (eq (char-after) ?\"))
                  (forward-char 1)))
               ((eq c ?{)
                (cl-incf depth)
                (forward-char 1))
               ((eq c ?})
                (cl-decf depth)
                (forward-char 1)
                (when (= depth 0)
                  (throw 'found (point))))
               (t (forward-char 1)))))
          nil)))))

(defun anvil-code--find-block-end (block-end match-end next-start)
  "Compute the end position for a block.

BLOCK-END is one of:
  `next-block-start' (default) — return NEXT-START or `point-max'
  `brace-balance' — find matching `{...}' starting at MATCH-END,
                    bounded by NEXT-START (when set) or whole buffer
  REGEXP STRING   — search forward from MATCH-END; return the start
                    position of the first match, or NEXT-START / EOB

Returns a buffer position, or nil if no end could be determined."
  (cond
   ((or (null block-end) (eq block-end 'next-block-start))
    (or next-start (point-max)))
   ((eq block-end 'brace-balance)
    (anvil-code--brace-balance-end match-end next-start))
   ((stringp block-end)
    (save-excursion
      (goto-char match-end)
      (if (re-search-forward block-end (or next-start nil) t)
          (match-beginning 0)
        (or next-start (point-max)))))
   (t (error "anvil-code: invalid :block-end %S" block-end))))

(defun anvil-code--validate-fields (fields)
  "Validate FIELDS is a non-empty list of well-formed field plists."
  (unless (and (listp fields) fields)
    (error "anvil-code: :fields must be a non-empty list"))
  (dolist (f fields)
    (unless (listp f)
      (error "anvil-code: each field must be a plist (got %S)" f))
    (let ((name (plist-get f :name))
          (re   (plist-get f :regexp)))
      (unless (stringp name)
        (error "anvil-code: field missing :name string: %S" f))
      (unless (stringp re)
        (error "anvil-code: field missing :regexp string: %S" f)))))

(defun anvil-code-extract-pattern (path spec)
  "Extract structured records from PATH by matching repeating patterns.

For each match of `:block-start' at PATH, determine the block's body via
`:block-end', then run each `:fields' regexp inside that body and capture
group 1 as the field's value.  Returns the list of records as plain data
without modifying the file.

PATH is the absolute path to the file.

SPEC is a plist:
  :block-start    REGEXP — Required.  Marks the start of each block.
                  If group 1 is present, its capture becomes the block's :id.
  :block-end      One of:
                    `next-block-start' (default) — block ends right before
                      the next `:block-start' match (or EOF).
                    `brace-balance' — track `{}' depth from `:block-start';
                      end at the matching closing `}'.  Strings are skipped.
                    REGEXP string — block ends at the start of the first
                      regexp match after `:block-start'.
  :fields         List of field plists.  Each plist:
                    :name STRING   — output key (required)
                    :regexp REGEXP — must capture value in group 1 (required)
                    :required BOOL — when t, missing field skips the block
                                     (or errors, per :on-missing-required)
  :max-blocks     NUMBER — stop after returning this many records (optional)
  :on-missing-required SYMBOL — `skip-block' (default) | `error'

Returns plist:
  :matches LIST   — list of plists, each:
                    :id STRING      — capture group 1 of :block-start, or nil
                    :start-line N   — line of the block-start match
                    :end-line N     — line at the block-end position
                    :fields ((NAME . VALUE) ...)  — captured fields, in order
  :total NUMBER     — total :block-start matches detected
  :returned NUMBER  — number of records in :matches
  :skipped NUMBER   — blocks dropped because a :required field was missing
  :file PATH
  :warnings LIST"
  (let* ((abs (anvil--prepare-path path))
         (warnings (anvil-file-warn-if-diverged abs))
         (block-start (or (plist-get spec :block-start)
                          (error "anvil-code: :block-start is required")))
         (block-end (or (plist-get spec :block-end) 'next-block-start))
         (fields (plist-get spec :fields))
         (max-blocks (plist-get spec :max-blocks))
         (on-missing-required (or (plist-get spec :on-missing-required)
                                  'skip-block))
         (matches nil)
         (total 0)
         (skipped 0))
    (anvil-code--validate-fields fields)
    (unless (memq on-missing-required '(skip-block error))
      (error "anvil-code: :on-missing-required must be skip-block|error \
(got %S)" on-missing-required))
    (with-temp-buffer
      (anvil--insert-file abs)
      ;; Pass 1: locate all block-start matches.
      (let ((positions nil))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward block-start nil t)
            (push (list :start (match-beginning 0)
                        :match-end (match-end 0)
                        :id (and (match-beginning 1)
                                 (match-string-no-properties 1)))
                  positions)))
        (setq positions (nreverse positions))
        (setq total (length positions))
        ;; Pass 2: process each block.
        (let ((idx 0)
              (rest positions))
          (catch 'done
            (while rest
              (let* ((cur (car rest))
                     (next (cadr rest))
                     (start (plist-get cur :start))
                     (m-end (plist-get cur :match-end))
                     (id (plist-get cur :id))
                     (next-start (and next (plist-get next :start)))
                     (end (anvil-code--find-block-end
                           block-end m-end next-start)))
                (when end
                  (let ((body (buffer-substring-no-properties m-end end))
                        (field-acc nil)
                        (missing-required nil))
                    (dolist (f fields)
                      (let* ((name (plist-get f :name))
                             (re (plist-get f :regexp))
                             (required (plist-get f :required))
                             (matched (string-match re body))
                             (value (and matched (match-string 1 body))))
                        (cond
                         (value
                          (push (cons name value) field-acc))
                         (required
                          (push name missing-required)))))
                    (cond
                     (missing-required
                      (pcase on-missing-required
                        ('skip-block (cl-incf skipped))
                        ('error
                         (error "anvil-code-extract: required field(s) \
missing %S in block at line %d (file %s)"
                                (nreverse missing-required)
                                (line-number-at-pos start) abs))))
                     (t
                      (push (list :id id
                                  :start-line (line-number-at-pos start)
                                  :end-line (line-number-at-pos end)
                                  :fields (nreverse field-acc))
                            matches)
                      (cl-incf idx)
                      (when (and max-blocks (>= idx max-blocks))
                        (throw 'done nil)))))))
              (setq rest (cdr rest)))))))
    (let ((reversed (nreverse matches)))
      (list :matches reversed
            :total total
            :returned (length reversed)
            :skipped skipped
            :file abs
            :warnings warnings))))

;;;; --- code-add-field-by-map (TS/JS object literal bulk add) --------------

(defun anvil-file--map-normalize (map)
  "Normalize MAP into an alist of (LOOKUP . ADD) string pairs.
Accepts alist, list of two-element lists, or hash-table."
  (cond
   ((hash-table-p map)
    (let (acc)
      (maphash (lambda (k v) (push (cons k v) acc)) map)
      (nreverse acc)))
   ((listp map)
    (mapcar
     (lambda (p)
       (cond
        ((and (consp p) (stringp (car p)) (stringp (cdr p)))
         (cons (car p) (cdr p)))
        ((and (consp p) (stringp (car p)) (consp (cdr p))
              (null (cddr p)) (stringp (cadr p)))
         (cons (car p) (cadr p)))
        (t (error "anvil-code: invalid map pair (need string→string): %S" p))))
     map))
   (t (error "anvil-code: map must be alist or hash-table: %S" map))))

(defun anvil-file--ts-string-end (start)
  "Return position just past the closing `\"' of a TS string starting at START.
START must point at the opening `\"'.  Handles `\\\\' and `\\\"' escapes.
Returns nil if the string is unterminated within the buffer."
  (save-excursion
    (goto-char (1+ start))
    (catch 'done
      (while (not (eobp))
        (let ((c (char-after)))
          (cond
           ((eq c ?\\)
            (forward-char 2))
           ((eq c ?\")
            (forward-char 1)
            (throw 'done (point)))
           (t (forward-char 1)))))
      nil)))

(defun anvil-file--ts-block-bounds (pos)
  "Return (BRACE-START . BRACE-END) of the single-line `{...}' block at POS.
POS must lie inside the block.  Returns nil if no single-line block contains
POS, or if the block spans multiple lines.  The bounds are character positions
of `{' and the character after `}'."
  (let* ((line-bol (line-beginning-position))
         (line-eol (line-end-position))
         (brace-start (save-excursion
                        (goto-char pos)
                        (when (search-backward "{" line-bol t)
                          (point))))
         (brace-end (save-excursion
                      (goto-char pos)
                      (when (search-forward "}" line-eol t)
                        (point)))))
    (when (and brace-start brace-end
               (< brace-start pos) (>= brace-end pos))
      (cons brace-start brace-end))))

(defun anvil-file--collect-scope-regions (scope-regex)
  "Return list of (START . END) substring regions matching SCOPE-REGEX.
When SCOPE-REGEX is nil, returns one region covering the whole buffer."
  (if (null scope-regex)
      (list (cons (point-min) (point-max)))
    (let ((acc nil))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward scope-regex nil t)
          (push (cons (match-beginning 0) (match-end 0)) acc)
          ;; Avoid infinite loop on zero-width matches.
          (when (= (match-beginning 0) (match-end 0))
            (forward-char 1))))
      (nreverse acc))))

(defun anvil-code-add-field-by-map (path spec)
  "Add a field to TS/JS object literals by mapping from another field's value.

For each occurrence of `LOOKUP-KEY: \"VALUE\"' inside a single-line `{...}'
block at PATH, look up VALUE in MAP and insert `ADD-KEY: \"MAPPED-VALUE\"'
before the block's closing `}'.

PATH is the absolute path to the file.

SPEC is a plist:
  :lookup-key    String — existing field name (e.g. \"ja\")
  :add-key       String — field name to add (e.g. \"en\")
  :map           Alist of (LOOKUP-VALUE . ADD-VALUE) string pairs, or
                 hash-table with the same shape
  :syntax        Symbol — currently only `ts' (default)
  :on-existing   Symbol — `error' (default) | `skip' | `overwrite'
                 Action when ADD-KEY is already present in the block
  :on-missing    Symbol — `skip' (default) | `error'
                 Action when LOOKUP-VALUE is not in MAP
  :scope-regex   String — only edit substrings matching this regexp
                 (each match becomes one independent region; nil = whole file)
  :apply         Boolean — when nil (default), preview only; when t, write disk

Returns plist:
  :added N            new ADD-KEY insertions made (or would-be in dry-run)
  :skipped N          matches skipped (existing ADD-KEY under `skip', or
                       lookup-value not in MAP under `skip')
  :overwritten N      existing ADD-KEY values replaced
  :missing ((V . COUNT) ...)  lookup-values seen in source but absent from MAP
  :total-matches N    total LOOKUP-KEY occurrences inspected
  :dry-run BOOL       t when :apply was nil
  :preview ((LINE BEFORE-LINE AFTER-LINE) ...)  always populated
  :file PATH
  :warnings LIST

Implementation notes:
  - Only single-line `{...}' blocks are matched (the i18n shape this targets).
  - String values must be double-quoted; `\\\\' and `\\\"' escapes are honored
    when scanning the lookup string, but the captured value is the raw text
    between the quotes (escapes pass through verbatim into MAP lookups).
  - The added field is inserted immediately before the closing `}', preceded
    by a comma if the block is non-empty after the lookup-key entry."
  (let* ((abs (anvil--prepare-path path))
         (warnings (anvil-file-warn-if-diverged abs))
         (lookup-key (or (plist-get spec :lookup-key)
                         (error "anvil-code: :lookup-key is required")))
         (add-key (or (plist-get spec :add-key)
                      (error "anvil-code: :add-key is required")))
         (map-norm (anvil-file--map-normalize
                    (or (plist-get spec :map)
                        (error "anvil-code: :map is required"))))
         (syntax (or (plist-get spec :syntax) 'ts))
         (on-existing (or (plist-get spec :on-existing) 'error))
         (on-missing (or (plist-get spec :on-missing) 'skip))
         (scope-regex (plist-get spec :scope-regex))
         (apply-p (plist-get spec :apply))
         (added 0) (skipped 0) (overwritten 0) (total 0)
         (missing-counts (make-hash-table :test 'equal))
         (preview nil)
         ;; List of edits as (REGION-INDEX BLOCK-END KIND VALUE INSERT-TEXT)
         ;; KIND ∈ 'insert | 'overwrite-value-end (we instead recompute on apply).
         ;; We collect (POS-INSERT INSERT-STRING DELETE-RANGE) tuples for apply.
         (edits nil))
    (unless (eq syntax 'ts)
      (error "anvil-code: only :syntax 'ts is supported (got %S)" syntax))
    (unless (memq on-existing '(error skip overwrite))
      (error "anvil-code: :on-existing must be error|skip|overwrite (got %S)"
             on-existing))
    (unless (memq on-missing '(skip error))
      (error "anvil-code: :on-missing must be skip|error (got %S)" on-missing))
    (with-temp-buffer
      (anvil--insert-file abs)
      (let* ((lookup-pat
              (concat "\\b" (regexp-quote lookup-key)
                      "[ \t]*:[ \t]*\""))
             (add-key-pat
              (concat "\\b" (regexp-quote add-key) "[ \t]*:")))
        (dolist (region (anvil-file--collect-scope-regions scope-regex))
          (save-excursion
            (goto-char (car region))
            (while (re-search-forward lookup-pat (cdr region) t)
              (let* ((quote-open (1- (match-end 0)))
                     (value-end (anvil-file--ts-string-end quote-open)))
                (when value-end
                  (let* ((value (buffer-substring-no-properties
                                 (1+ quote-open) (1- value-end)))
                         (block (anvil-file--ts-block-bounds quote-open)))
                    (when block
                      (cl-incf total)
                      (let* ((brace-start (car block))
                             (brace-end (cdr block))
                             (block-text (buffer-substring-no-properties
                                          brace-start brace-end))
                             (mapping (assoc value map-norm)))
                        (cond
                         ((null mapping)
                          (puthash value
                                   (1+ (gethash value missing-counts 0))
                                   missing-counts)
                          (pcase on-missing
                            ('skip (cl-incf skipped))
                            ('error
                             (error "anvil-code: lookup-value not in map: %S \
(file %s, line %d)"
                                    value abs
                                    (line-number-at-pos quote-open)))))
                         (t
                          (let* ((mapped (cdr mapping))
                                 (line (line-number-at-pos quote-open))
                                 (before-line
                                  (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position))))
                            (cond
                             ;; ADD-KEY already present in this block.
                             ((string-match-p add-key-pat block-text)
                              (pcase on-existing
                                ('skip (cl-incf skipped))
                                ('error
                                 (error "anvil-code: %s already present \
in block (file %s, line %d) — pass :on-existing 'skip or 'overwrite"
                                        add-key abs line))
                                ('overwrite
                                 (let* ((existing-pat
                                         (concat "\\b"
                                                 (regexp-quote add-key)
                                                 "[ \t]*:[ \t]*\""))
                                        (find-pos
                                         (save-excursion
                                           (goto-char brace-start)
                                           (when (re-search-forward
                                                  existing-pat brace-end t)
                                             (point)))))
                                   (when find-pos
                                     (let* ((existing-end
                                             (anvil-file--ts-string-end
                                              (1- find-pos))))
                                       (when existing-end
                                         (let* ((after-line
                                                 (concat
                                                  (substring before-line 0
                                                             (- find-pos
                                                                (line-beginning-position)))
                                                  (anvil--json-escape-string mapped)
                                                  "\""
                                                  (substring before-line
                                                             (- existing-end
                                                                (line-beginning-position))))))
                                           (push (list line before-line after-line)
                                                 preview)
                                           (push (list 'overwrite
                                                       find-pos
                                                       existing-end
                                                       (concat
                                                        (anvil--json-escape-string mapped)
                                                        "\""))
                                                 edits)
                                           (cl-incf overwritten)))))))))
                             ;; ADD-KEY not present — insert before closing `}'.
                             (t
                              (let* ((inner-end
                                      (save-excursion
                                        (goto-char (1- brace-end))
                                        (skip-chars-backward " \t"
                                                             (1+ brace-start))
                                        (point)))
                                     (between-text
                                      (string-trim
                                       (buffer-substring-no-properties
                                        (1+ brace-start) (1- brace-end))))
                                     (sep (if (string-empty-p between-text) "" ", "))
                                     (insert-text
                                      (concat sep add-key ": \""
                                              (anvil--json-escape-string mapped)
                                              "\""))
                                     (after-line
                                      (concat
                                       (substring before-line 0
                                                  (- inner-end
                                                     (line-beginning-position)))
                                       insert-text
                                       (substring before-line
                                                  (- inner-end
                                                     (line-beginning-position))))))
                                (push (list line before-line after-line) preview)
                                (push (list 'insert inner-end insert-text) edits)
                                (cl-incf added)))))))))))
                ;; Always advance past the lookup match to avoid re-scanning.
                (when value-end (goto-char value-end))))))
        ;; Apply edits if requested.  Apply in reverse position order so that
        ;; earlier positions remain valid after later edits.
        (when (and apply-p edits)
          (dolist (e (sort (copy-sequence edits)
                           (lambda (a b)
                             (> (nth 1 a) (nth 1 b)))))
            (pcase (car e)
              ('insert
               (goto-char (nth 1 e))
               (insert (nth 2 e)))
              ('overwrite
               (let ((from (nth 1 e)) (to (nth 2 e)))
                 (delete-region (1- from) to)
                 (goto-char (1- from))
                 (insert "\"" (nth 3 e))))))
          (anvil--write-current-buffer-to abs))
        (let ((missing-list nil))
          (maphash (lambda (k v) (push (cons k v) missing-list))
                   missing-counts)
          (list :added added
                :skipped skipped
                :overwritten overwritten
                :missing (nreverse missing-list)
                :total-matches total
                :dry-run (not apply-p)
                :preview (nreverse preview)
                :file abs
                :warnings warnings))))))

;;;; --- batch across multiple files -----------------------------------------

(defun anvil-file-batch-across (file-ops)
  "Run `anvil-file-batch' across multiple files in one call.

FILE-OPS is a list of alists each with:
  `path'        Absolute path to the file.
  `operations'  List of op alists (same format as `anvil-file-batch').

All files are processed sequentially.  If one fails, the error is
captured and processing continues for the rest; the caller sees a
per-file status in the returned list.

Returns plist (:files N :succeeded M :failed K :results ((PATH . PLIST) ...))."
  (let ((succeeded 0) (failed 0) (results '()))
    (dolist (fo file-ops)
      (let ((path (or (alist-get 'path fo)
                      (alist-get "path" fo nil nil #'equal)))
            (ops (or (alist-get 'operations fo)
                     (alist-get "operations" fo nil nil #'equal))))
        (unless (and path ops)
          (error "anvil-file-batch-across: missing path or operations in %S" fo))
        (condition-case err
            (let ((res (anvil-file-batch path ops)))
              (cl-incf succeeded)
              (push (cons path res) results))
          (error
           (cl-incf failed)
           (push (cons path (list :error (error-message-string err)))
                 results)))))
    (list :files (length file-ops)
          :succeeded succeeded
          :failed failed
          :results (nreverse results))))

;;;; --- MCP tool wrappers (new helpers) -------------------------------------

(defun anvil-file--tool-json-object-add (path pairs-json &optional on-duplicate indent)
  "Add key/value pairs to the top-level JSON object at PATH.

PAIRS-JSON is a JSON array of two-element arrays or objects.  Examples:
  [[\"key1\",\"val1\"], [\"key2\",\"val2\"]]
  [{\"key\":\"a\",\"value\":\"b\"}, ...]
ON-DUPLICATE is \"skip\"|\"overwrite\"|\"error\" (default: skip).
INDENT is a string that parses as a number (default: auto-detect).

MCP Parameters:
  path - Absolute path to the JSON file to edit
  pairs-json - JSON array of [key, value] pairs or {key, value} objects
  on-duplicate - \"skip\" (default), \"overwrite\", or \"error\"
  indent - Optional indentation width as a string (e.g. \"2\"); auto-detected if omitted"
  (anvil-server-with-error-handling
   (let* ((parsed (json-parse-string pairs-json
                                     :object-type 'alist
                                     :array-type 'list))
          (pairs (mapcar
                  (lambda (p)
                    (cond
                     ((and (listp p) (= (length p) 2)
                           (stringp (car p)) (stringp (cadr p)))
                      (cons (car p) (cadr p)))
                     ((listp p)  ; alist from JSON object
                      (let ((k (or (alist-get 'key p)
                                   (alist-get "key" p nil nil #'equal)))
                            (v (or (alist-get 'value p)
                                   (alist-get "value" p nil nil #'equal))))
                        (unless (and k v)
                          (error "pair needs {\"key\":...,\"value\":...}: %S" p))
                        (cons k v)))
                     (t (error "unsupported pair shape: %S" p))))
                  parsed))
          (opts (append
                 (when on-duplicate
                   (list :on-duplicate (intern on-duplicate)))
                 (when (and indent (stringp indent) (not (string-empty-p indent)))
                   (list :indent (string-to-number indent))))))
     (format "%S" (anvil-json-object-add path pairs opts)))))

(defun anvil-file--tool-ensure-import (path import-line &optional after-regex position)
  "Idempotently ensure IMPORT-LINE exists as a line in PATH.

If the line is already present, no change is made.  Otherwise inserts
after the last line matching AFTER-REGEX (default: \"^import \").
POSITION overrides insertion location.

MCP Parameters:
  path - Absolute path to the file to edit
  import-line - The full line of text to ensure (e.g. \"import x from 'y';\")
  after-regex - Optional regexp; insert after the last matching line (default: \"^import \")
  position - Optional: \"after-last-match\" (default), \"before-first-match\", \"top\", or \"bottom\""
  (anvil-server-with-error-handling
   (let ((opts (append
                (when after-regex (list :after-regex after-regex))
                (when position (list :position (intern position))))))
     (format "%S" (anvil-file-ensure-import path import-line opts)))))

(defun anvil-file--tool-batch-across (file-ops-json)
  "Apply `anvil-file-batch' to multiple files in a single call.

FILE-OPS-JSON is a JSON array of objects, each with a `path' and an
`operations' array (same format as file-batch).  Failures in one file
do not abort the rest; per-file results are returned.

MCP Parameters:
  file-ops-json - JSON array of {\"path\":...,\"operations\":[...]} objects"
  (anvil-server-with-error-handling
   (let ((file-ops (json-parse-string file-ops-json
                                      :object-type 'alist
                                      :array-type 'list)))
     (format "%S" (anvil-file-batch-across file-ops)))))

(defun anvil-file--tool-code-extract-pattern (path spec-json)
  "Extract repeating structured records from PATH driven by SPEC-JSON.

SPEC-JSON is a JSON object describing the extraction.  Keys mirror the
elisp `anvil-code-extract-pattern' SPEC plist (kebab-case in elisp,
snake_case or kebab-case accepted in JSON):

  {
    \"block-start\":  \"if \\\\(.*\\\\) \\\\{\",
    \"block-end\":    \"brace-balance\",
    \"fields\": [
      {\"name\": \"name\",  \"regexp\": \"name *= *\\\"(.*)\\\"\"},
      {\"name\": \"price\", \"regexp\": \"price *= *(\\\\d+)\", \"required\": true}
    ],
    \"max-blocks\": 100,
    \"on-missing-required\": \"skip-block\"
  }

The `block-end' value is one of `\"next-block-start\"' (default),
`\"brace-balance\"', or any other string treated as a regexp.

This tool is read-only — the file is never modified.

MCP Parameters:
  path - Absolute path to the file to scan
  spec-json - JSON object specifying block-start, block-end, fields, etc."
  (anvil-server-with-error-handling
   (let* ((parsed (json-parse-string spec-json
                                     :object-type 'alist
                                     :array-type 'list))
          (get (lambda (key)
                 (or (alist-get (intern key) parsed)
                     (alist-get key parsed nil nil #'equal))))
          (block-start (funcall get "block-start"))
          (block-end-raw (funcall get "block-end"))
          (block-end (cond
                      ((null block-end-raw) 'next-block-start)
                      ((member block-end-raw
                               '("next-block-start" "brace-balance"))
                       (intern block-end-raw))
                      ((stringp block-end-raw) block-end-raw)
                      (t (error
                          "anvil-code: invalid block-end %S"
                          block-end-raw))))
          (fields-raw (funcall get "fields"))
          (fields
           (mapcar
            (lambda (f)
              (let ((name (or (alist-get 'name f)
                              (alist-get "name" f nil nil #'equal)))
                    (re   (or (alist-get 'regexp f)
                              (alist-get "regexp" f nil nil #'equal)))
                    (req  (or (alist-get 'required f)
                              (alist-get "required" f nil nil #'equal))))
                (append (list :name name :regexp re)
                        (when (and req (not (eq req :null)))
                          (list :required t)))))
            fields-raw))
          (max-blocks (funcall get "max-blocks"))
          (on-missing-raw (funcall get "on-missing-required"))
          (spec (append
                 (list :block-start block-start
                       :block-end block-end
                       :fields fields)
                 (when (numberp max-blocks)
                   (list :max-blocks max-blocks))
                 (when (and on-missing-raw (stringp on-missing-raw)
                            (not (string-empty-p on-missing-raw)))
                   (list :on-missing-required (intern on-missing-raw))))))
     (format "%S" (anvil-code-extract-pattern path spec)))))

(defun anvil-file--tool-code-add-field-by-map
    (path lookup-key add-key map-json
          &optional on-existing on-missing scope-regex apply)
  "Add ADD-KEY to single-line `{...}' object literals in PATH by mapping LOOKUP-KEY values.

For each occurrence of `LOOKUP-KEY: \"VALUE\"' inside a `{...}' block,
look up VALUE in MAP-JSON and insert `ADD-KEY: \"MAPPED-VALUE\"' before
the closing `}'.  Default is preview-only — pass APPLY=\"t\" to write.

MAP-JSON accepts either a JSON object {\"v1\":\"t1\",\"v2\":\"t2\"} or a
JSON array of two-element arrays [[\"v1\",\"t1\"],[\"v2\",\"t2\"]].

MCP Parameters:
  path - Absolute path to the TS/JS file to edit
  lookup-key - Existing field name to look up (e.g. \"ja\")
  add-key - Field name to add (e.g. \"en\")
  map-json - JSON object or array of [lookup-value, add-value] pairs
  on-existing - \"error\" (default), \"skip\", or \"overwrite\"
  on-missing - \"skip\" (default) or \"error\"
  scope-regex - Optional regexp; only edit within substrings matching it
  apply - \"t\" to write the file; otherwise (default) preview only"
  (anvil-server-with-error-handling
   (let* ((parsed (json-parse-string map-json
                                     :object-type 'alist
                                     :array-type 'list))
          (map (cond
                ;; alist form (parsed JSON object)
                ((and parsed (consp (car parsed))
                      (or (symbolp (caar parsed)) (stringp (caar parsed))))
                 (mapcar (lambda (kv)
                           (let ((k (car kv)) (v (cdr kv)))
                             (cons (if (symbolp k) (symbol-name k) k) v)))
                         parsed))
                ;; list-of-lists form (parsed JSON array of arrays)
                (t parsed)))
          (apply-p (and apply (member apply '("t" "true" "1" "yes"))))
          (spec (append
                 (list :lookup-key lookup-key
                       :add-key add-key
                       :map map)
                 (when (and on-existing (not (string-empty-p on-existing)))
                   (list :on-existing (intern on-existing)))
                 (when (and on-missing (not (string-empty-p on-missing)))
                   (list :on-missing (intern on-missing)))
                 (when (and scope-regex (not (string-empty-p scope-regex)))
                   (list :scope-regex scope-regex))
                 (when apply-p (list :apply t)))))
     (format "%S" (anvil-code-add-field-by-map path spec)))))

;;;; --- outline --------------------------------------------------------------

(defun anvil-file--outline-elisp ()
  "Scan the current buffer as Elisp; return a list of outline entries."
  (let ((items nil))
    (goto-char (point-min))
    (while (re-search-forward
            "^(\\(cl-def\\(?:un\\|method\\|generic\\|macro\\|struct\\)\\|def\\(?:un\\|macro\\|var\\|const\\|custom\\|group\\|subst\\|alias\\|face\\|theme\\|advice-add\\)\\)[ \t\n]+\\([^ \t\n()]+\\)"
            nil t)
      (push (list :kind (match-string-no-properties 1)
                  :name (match-string-no-properties 2)
                  :line (line-number-at-pos (match-beginning 0)))
            items))
    (nreverse items)))

(defun anvil-file--outline-org ()
  "Scan the current buffer as org; return headline outline entries."
  (let ((items nil))
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\)[ \t]+\\(.*\\)$" nil t)
      (push (list :kind (format "h%d" (length (match-string-no-properties 1)))
                  :name (string-trim (match-string-no-properties 2))
                  :line (line-number-at-pos (match-beginning 0)))
            items))
    (nreverse items)))

(defun anvil-file--outline-markdown ()
  "Scan the current buffer as Markdown; return heading outline entries."
  (let ((items nil))
    (goto-char (point-min))
    (while (re-search-forward "^\\(#+\\)[ \t]+\\(.*\\)$" nil t)
      (push (list :kind (format "h%d" (length (match-string-no-properties 1)))
                  :name (string-trim (match-string-no-properties 2))
                  :line (line-number-at-pos (match-beginning 0)))
            items))
    (nreverse items)))

(defun anvil-file--tool-outline (path &optional format)
  "Return a compact outline of PATH without sending the whole file.

MCP Parameters:
  path   - Path to the file to scan (string).
  format - Optional format override: \"elisp\", \"org\", \"markdown\".
           When omitted the format is inferred from the file extension
           (.el / .org / .md|.markdown).

Returns a printed plist:
  (:path P :format F :count N :items ((:kind K :name N :line L) ...))

Kinds:
  elisp   : defun, defmacro, defvar, defcustom, defconst, defgroup,
            defsubst, defalias, defface, deftheme, defmethod, defgeneric,
            defstruct, advice-add
  org     : h1 .. h6 (star count)
  md      : h1 .. h6 (hash count)

Ideal for orienting in large files before any Read call — saves
the full body from the response."
  (anvil-server-with-error-handling
   (let* ((abs (expand-file-name path))
          (fmt (or (and format (not (string-empty-p format))
                        (downcase format))
                   (pcase (downcase (or (file-name-extension abs) ""))
                     ((or "el" "elc") "elisp")
                     ("org" "org")
                     ((or "md" "markdown") "markdown")
                     (_ nil))))
          (items
           (with-temp-buffer
             (let ((coding-system-for-read 'utf-8-unix))
               (insert-file-contents abs))
             (pcase fmt
               ("elisp"    (anvil-file--outline-elisp))
               ("org"      (anvil-file--outline-org))
               ("markdown" (anvil-file--outline-markdown))
               (_ (error "Unknown format for %s (pass format= to override)"
                         abs))))))
     (format "%S" (list :path abs
                        :format fmt
                        :count (length items)
                        :items items)))))

;;;; --- module enable/disable -----------------------------------------------

(defun anvil-file-enable ()
  "Register anvil-file MCP tools for direct JSON-RPC access.
This eliminates the need to call file operations through emacs-eval,
avoiding shell + elisp-reader double-escaping of backslashes."
  (anvil-server-register-tool
   #'anvil-file--tool-replace-string
   :id "file-replace-string"
   :intent '(file-edit)
   :layer 'core
   :description
   "Replace literal text in a file.  Operates on the raw file via
temp-buffer + write-region (no mount-layer issues on Windows).
Safe for files over 1.2MB.  Errors if the old text is not found.
Pass max-count \"1\" to assert exactly one match."
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-replace-regexp
   :id "file-replace-regexp"
   :intent '(file-edit)
   :layer 'core
   :description
   "Replace regexp matches in a file.  The replacement string may use
\\\\1 \\\\2 for capture groups.  Errors if no match found.
Safe for files over 1.2MB."
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-insert-at-line
   :id "file-insert-at-line"
   :intent '(file-edit)
   :layer 'core
   :description
   "Insert text at a specific line number in a file (1-indexed).
Line 1 inserts before the first line.  Safe for files over 1.2MB."
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-delete-lines
   :id "file-delete-lines"
   :intent '(file-edit)
   :layer 'core
   :description
   "Delete a range of lines (inclusive, 1-indexed) from a file.
Safe for files over 1.2MB."
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-read
   :id "file-read"
   :intent '(file-read structure)
   :layer 'core
   :description
   "Layer 3 of anvil progressive disclosure (see `disclosure-help').
Read file contents with optional line-based pagination.  Accepts
either a plain absolute path or a `file://PATH[#L<start>[-<end>]]'
citation URI emitted by Layer 1 (`file-outline') / Layer 2
(`file-read-snippet') — the URI's line range becomes the default
offset/limit.  Returns the file content as a string.  For large
files, use `file-read-snippet' (Layer 2) or pass offset/limit to
read specific sections."
   :read-only t
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-append
   :id "file-append"
   :intent '(file-edit)
   :layer 'core
   :description
   "Append text to the end of a file.  A leading newline is added
if the file does not end with one.  Safe for files over 1.2MB."
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-batch
   :id "file-batch"
   :intent '(file-edit batch)
   :layer 'core
   :description
   "Execute multiple file operations in a single call.  Most token-efficient
way to perform bulk edits: N operations in 1 round trip instead of N calls.
The operations parameter is a JSON array string.  Supported ops:
replace, replace-regexp, insert-at-line, delete-lines, append, prepend.
All operations run sequentially on the same buffer and the file is written
once atomically at the end.  Safe for files over 1.2MB."
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-json-object-add
   :id "json-object-add"
   :intent '(json-edit config)
   :layer 'core
   :description
   "Add key-value pairs to a top-level JSON object while preserving
existing formatting.  Designed for i18n dictionaries and config files
where hundreds of entries must be appended without re-emitting the
whole file.  Pairs are supplied as a JSON array, e.g.
\"[[\\\"a\\\",\\\"1\\\"],[\\\"b\\\",\\\"2\\\"]]\".  The function detects
existing keys and handles duplicates via on-duplicate
(skip/overwrite/error).  Indentation is auto-detected from the file.
Trailing-comma and closing-brace handling is automatic.  Only string
values are supported; for numeric/boolean values fall back to
file-insert-at-line."
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-ensure-import
   :id "file-ensure-import"
   :intent '(file-edit)
   :layer 'core
   :description
   "Idempotently ensure an import (or any header) line exists in a file.
If the line already appears verbatim, returns already-present without
modifying the file.  Otherwise inserts after the last line matching
after-regex (default: \"^import \", matching TS/JS/Python imports).
Position can be overridden: \"after-last-match\" (default),
\"before-first-match\", \"top\", or \"bottom\"."
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-batch-across
   :id "file-batch-across"
   :intent '(file-edit batch)
   :layer 'core
   :description
   "Apply anvil-file-batch to multiple files in a single MCP call.
The argument is a JSON array where each element has a path and an
operations array: [{\"path\":\"/a.el\",\"operations\":[...]},...].
Failures in one file do not abort the rest; per-file results are
returned.  Use this for bulk docstring updates, import additions, or
coordinated multi-file refactors."
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-outline
   :id "file-outline"
   :intent '(file-read structure)
   :layer 'core
   :description
   "Layer 1 of anvil progressive disclosure (see `disclosure-help').
Return a compact structural outline of a file without reading its
body.  Infers format from extension (.el / .org / .md) or accepts a
format= override.  Emits (:kind :name :line) entries for Elisp
def-forms, org headlines, or Markdown headings.  Use this FIRST
to orient in large files before deciding whether to escalate to
Layer 2 (`file-read-snippet') or Layer 3 (`file-read').  Tool
descriptions and disclosure-help cover the full contract."
   :read-only t
   :offload t
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-code-extract-pattern
   :id "code-extract-pattern"
   :intent '(code-bulk-edit)
   :layer 'core
   :description
   "Extract repeating structured records from a file using regexp patterns.
For each match of `block-start' the tool finds the block's body via
`block-end' (`next-block-start', `brace-balance', or a regexp), then
runs each `fields' regexp inside the body and captures group 1 as the
field's value.  Read-only — the file is never modified.  Returns plist
with :matches (each :id :start-line :end-line :fields) :total :returned
:skipped.  Targets legacy code migration / data extraction where reading
the entire file would be wasteful.  Brace-balance skips strings."
   :read-only t
   :offload t
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-code-add-field-by-map
   :id "code-add-field-by-map"
   :intent '(code-bulk-edit)
   :layer 'core
   :description
   "Add a field to TS/JS object literals by mapping from another field's value.
For each occurrence of `LOOKUP-KEY: \"VALUE\"' inside a single-line `{...}'
block at PATH, look up VALUE in MAP-JSON and insert `ADD-KEY: \"MAPPED-VALUE\"'
before the closing `}'.  Targets bulk i18n / schema-extension workflows where
Read+Write of whole files would dominate token cost.  Default is preview-only;
pass apply=\"t\" to write the file.  on-existing controls behavior when
ADD-KEY already exists (error|skip|overwrite, default error).  scope-regex
restricts edits to substrings matching the pattern.  Returns plist with
:added :skipped :overwritten :missing :total-matches :dry-run :preview."
   :server-id anvil-file--server-id))

(defun anvil-file-disable ()
  "Unregister anvil-file MCP tools."
  (anvil-server-unregister-tool "file-replace-string" anvil-file--server-id)
  (anvil-server-unregister-tool "file-replace-regexp" anvil-file--server-id)
  (anvil-server-unregister-tool "file-insert-at-line" anvil-file--server-id)
  (anvil-server-unregister-tool "file-delete-lines" anvil-file--server-id)
  (anvil-server-unregister-tool "file-read" anvil-file--server-id)
  (anvil-server-unregister-tool "file-outline" anvil-file--server-id)
  (anvil-server-unregister-tool "file-append" anvil-file--server-id)
  (anvil-server-unregister-tool "file-batch" anvil-file--server-id)
  (anvil-server-unregister-tool "json-object-add" anvil-file--server-id)
  (anvil-server-unregister-tool "file-ensure-import" anvil-file--server-id)
  (anvil-server-unregister-tool "file-batch-across" anvil-file--server-id)
  (anvil-server-unregister-tool "code-add-field-by-map" anvil-file--server-id)
  (anvil-server-unregister-tool "code-extract-pattern" anvil-file--server-id))

(provide 'anvil-helpers)
(provide 'anvil-file)
;;; anvil-file.el ends here
