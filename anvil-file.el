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

(defun anvil-file--tool-read (path &optional offset limit)
  "Read file at PATH and return its content.
Supports optional line-based pagination.

MCP Parameters:
  path - Absolute path to the file to read
  offset - Lines to skip from start (optional, 0-indexed, e.g. \"100\")
  limit - Maximum lines to return (optional, e.g. \"50\")"
  (anvil-server-with-error-handling
    (let ((off (if (and offset (not (string-empty-p offset)))
                   (string-to-number offset)
                 nil))
          (lim (if (and limit (not (string-empty-p limit)))
                   (string-to-number limit)
                 nil)))
      (anvil-file-read path off lim))))

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
Returns (:ok t :operations N :file PATH) on success."
  (let ((abs (anvil--prepare-path path))
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
    (list :ok t :operations op-count :file abs)))

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

Returns plist (:inserted BOOL :line N :already-present BOOL :file PATH)."
  (let* ((abs (anvil--prepare-path path))
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
                  :line (line-number-at-pos) :file abs)
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
                      :line ln :file abs)))
             (insert-line
              (goto-char (point-min))
              (forward-line (1- insert-line))
              (insert target-line "\n")
              (anvil--write-current-buffer-to abs)
              (list :inserted t :already-present nil
                    :line insert-line :file abs))
             (t
              ;; No matches: insert at top (line 1)
              (goto-char (point-min))
              (insert target-line "\n")
              (anvil--write-current-buffer-to abs)
              (list :inserted t :already-present nil
                    :line 1 :file abs)))))))))

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

;;;; --- module enable/disable -----------------------------------------------

(defun anvil-file-enable ()
  "Register anvil-file MCP tools for direct JSON-RPC access.
This eliminates the need to call file operations through emacs-eval,
avoiding shell + elisp-reader double-escaping of backslashes."
  (anvil-server-register-tool
   #'anvil-file--tool-replace-string
   :id "file-replace-string"
   :description
   "Replace literal text in a file.  Operates on the raw file via
temp-buffer + write-region (no mount-layer issues on Windows).
Safe for files over 1.2MB.  Errors if the old text is not found.
Pass max-count \"1\" to assert exactly one match."
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-replace-regexp
   :id "file-replace-regexp"
   :description
   "Replace regexp matches in a file.  The replacement string may use
\\\\1 \\\\2 for capture groups.  Errors if no match found.
Safe for files over 1.2MB."
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-insert-at-line
   :id "file-insert-at-line"
   :description
   "Insert text at a specific line number in a file (1-indexed).
Line 1 inserts before the first line.  Safe for files over 1.2MB."
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-delete-lines
   :id "file-delete-lines"
   :description
   "Delete a range of lines (inclusive, 1-indexed) from a file.
Safe for files over 1.2MB."
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-read
   :id "file-read"
   :description
   "Read file contents with optional line-based pagination.
Returns the file content as a string.  For large files, use offset
and limit to read specific sections."
   :read-only t
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-append
   :id "file-append"
   :description
   "Append text to the end of a file.  A leading newline is added
if the file does not end with one.  Safe for files over 1.2MB."
   :server-id anvil-file--server-id)

  (anvil-server-register-tool
   #'anvil-file--tool-batch
   :id "file-batch"
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
   :description
   "Apply anvil-file-batch to multiple files in a single MCP call.
The argument is a JSON array where each element has a path and an
operations array: [{\"path\":\"/a.el\",\"operations\":[...]},...].
Failures in one file do not abort the rest; per-file results are
returned.  Use this for bulk docstring updates, import additions, or
coordinated multi-file refactors."
   :server-id anvil-file--server-id))

(defun anvil-file-disable ()
  "Unregister anvil-file MCP tools."
  (anvil-server-unregister-tool "file-replace-string" anvil-file--server-id)
  (anvil-server-unregister-tool "file-replace-regexp" anvil-file--server-id)
  (anvil-server-unregister-tool "file-insert-at-line" anvil-file--server-id)
  (anvil-server-unregister-tool "file-delete-lines" anvil-file--server-id)
  (anvil-server-unregister-tool "file-read" anvil-file--server-id)
  (anvil-server-unregister-tool "file-append" anvil-file--server-id)
  (anvil-server-unregister-tool "file-batch" anvil-file--server-id)
  (anvil-server-unregister-tool "json-object-add" anvil-file--server-id)
  (anvil-server-unregister-tool "file-ensure-import" anvil-file--server-id)
  (anvil-server-unregister-tool "file-batch-across" anvil-file--server-id))

(provide 'anvil-helpers)
(provide 'anvil-file)
;;; anvil-file.el ends here
