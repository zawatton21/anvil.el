;;; anvil-file.el --- Safe file and org editing for anvil -*- lexical-binding: t; -*-

;; Author: zawatton21
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
Returns (:file PATH :content STR :total-lines N :offset OFFSET :lines-returned N)."
  (let ((abs (anvil--prepare-path path)))
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
                :lines-returned lines-returned))))))

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
Returns (:replaced N :file PATH). Errors if 0 replacements were made."
  (let ((abs (anvil--prepare-path path))
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
    (list :replaced count :file abs)))

(defun anvil-file-replace-string (path old-string new-string &optional max-count)
  "In PATH, replace literal OLD-STRING with NEW-STRING.
If MAX-COUNT is non-nil, stop after that many replacements.
Returns (:replaced N :file PATH). Errors if 0 replacements were made.
Pass MAX-COUNT 1 to assert exactly-one match (will still error on 0)."
  (let ((abs (anvil--prepare-path path))
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
    (list :replaced count :file abs)))

(defun anvil-file-insert-at-line (path line content)
  "Insert CONTENT into PATH at LINE (1-indexed). LINE 1 = before first line.
A trailing newline is added to CONTENT if not present.
Returns (:line LINE :inserted-bytes N :file PATH)."
  (let ((abs (anvil--prepare-path path))
        (text (if (string-suffix-p "\n" content)
                  content
                (concat content "\n"))))
    (with-temp-buffer
      (anvil--insert-file abs)
      (goto-char (point-min))
      (forward-line (1- line))
      (insert text)
      (anvil--write-current-buffer-to abs))
    (list :line line :inserted-bytes (length text) :file abs)))

(defun anvil-file-delete-lines (path start-line end-line)
  "Delete lines START-LINE through END-LINE inclusive (1-indexed) from PATH.
Returns (:deleted N :file PATH)."
  (when (> start-line end-line)
    (error "my-cc: start-line %d > end-line %d" start-line end-line))
  (let ((abs (anvil--prepare-path path))
        (deleted (1+ (- end-line start-line))))
    (with-temp-buffer
      (anvil--insert-file abs)
      (goto-char (point-min))
      (forward-line (1- start-line))
      (let ((beg (point)))
        (forward-line deleted)
        (delete-region beg (point)))
      (anvil--write-current-buffer-to abs))
    (list :deleted deleted :file abs)))

(defun anvil-file-append (path content)
  "Append CONTENT to end of PATH.
A leading newline is added if file does not end with one.
Returns (:appended-bytes N :file PATH)."
  (let ((abs (anvil--prepare-path path))
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
    (list :appended-bytes bytes :file abs)))

(defun anvil-file-prepend (path content)
  "Prepend CONTENT to beginning of PATH.
A trailing newline is added to CONTENT if not present.
Returns (:prepended-bytes N :file PATH)."
  (let ((abs (anvil--prepare-path path))
        (text (if (string-suffix-p "\n" content)
                  content
                (concat content "\n"))))
    (with-temp-buffer
      (anvil--insert-file abs)
      (goto-char (point-min))
      (insert text)
      (anvil--write-current-buffer-to abs))
    (list :prepended-bytes (length text) :file abs)))

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

(provide 'anvil-helpers)
;;; anvil-helpers.el ends here
