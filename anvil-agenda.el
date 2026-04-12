;;; anvil-agenda.el --- Agenda with rokuyo calendar for anvil -*- lexical-binding: t; -*-

;; Author: zawatton21
;; Keywords: tools, mcp, claude, org, agenda, rokuyo

;;; Commentary:

;; anvil-helpers の姉妹ファイル。Claude Code が emacs-eval 経由で
;; 「指定期間のアジェンダ (六曜 + scheduled + deadline + journal)」を
;; 1 call で plist list として取得するための helper。
;;
;; 設計方針:
;;   - 六曜は計算しない。journals-YYYY.org に既存運用されている
;;     `** ROKUYO 大安 <2026-01-01 Thu>' 形式の見出しから抽出するだけ
;;     (太陰太陽暦計算は不要 — agenda-dashboard skill が同フォーマットを採用済)。
;;   - 複数年跨ぎの date range に対応 (start-year .. end-year の journals を walk)。
;;   - pure regexp scanner (org-mode 不要) — org-map-entries は 650KB journals で
;;     MCP 30s timeout を超えるため廃止。regexp は ~100x 高速。
;;   - 日付抽出は YYYY-MM-DD regexp (繰り返し記法 +1w / ++1y を含む文字列でも安全)。
;;   - 戻り値は date 順 plist list。空 day は plist は返すが list が空。
;;
;; Public API:
;;   (anvil-agenda &optional OPTS)         — 期間アジェンダ取得
;;   (anvil-agenda-rokuyo DATE &optional FILE) — 単一日付の rokuyo
;;   (anvil-agenda-overdue &optional OPTS) — overdue (今日以前 + 未完了)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-file)

;;;; --- internal: date helpers ---------------------------------------------

(defun anvil-agenda--today ()
  "Return today's date as ISO YYYY-MM-DD string."
  (format-time-string "%Y-%m-%d"))

(defun anvil-agenda--iso->time (iso)
  "Parse ISO YYYY-MM-DD string ISO into a time value (00:00:00 local)."
  (date-to-time (concat iso " 00:00:00")))

(defun anvil-agenda--add-days (iso n)
  "Return ISO date that is N days after ISO."
  (format-time-string "%Y-%m-%d"
                      (time-add (anvil-agenda--iso->time iso)
                                (days-to-time n))))

(defun anvil-agenda--extract-iso (str)
  "Extract first YYYY-MM-DD from STR, or nil."
  (when (and str (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" str))
    (match-string 1 str)))

(defun anvil-agenda--in-range-p (date start end)
  "Return non-nil if DATE (ISO) is in [START, END] inclusive (string compare)."
  (and (stringp date)
       (not (string< date start))
       (not (string< end date))))

(defun anvil-agenda--parse-repeater (ts-str)
  "Parse org timestamp TS-STR for repeater info.
Returns (BASE-DATE . REPEATER) where REPEATER is (N . UNIT) or nil.
UNIT is one of 'd 'w 'm 'y. BASE-DATE is ISO YYYY-MM-DD."
  (let ((date (anvil-agenda--extract-iso ts-str))
        repeater)
    (when (and date ts-str
               (string-match "\\+\\+?\\([0-9]+\\)\\([dwmy]\\)" ts-str))
      (let ((n (string-to-number (match-string 1 ts-str)))
            (u (intern (match-string 2 ts-str))))
        (setq repeater (cons n u))))
    (cons date repeater)))

(defun anvil-agenda--advance-date (iso n unit)
  "Advance ISO date by N UNITs. UNIT is one of 'd 'w 'm 'y."
  (let ((time (anvil-agenda--iso->time iso)))
    (pcase unit
      ('d (format-time-string "%Y-%m-%d" (time-add time (days-to-time n))))
      ('w (format-time-string "%Y-%m-%d" (time-add time (days-to-time (* n 7)))))
      ('m (let* ((decoded (decode-time time))
                 (new-month (+ (nth 4 decoded) n))
                 (new-year (+ (nth 5 decoded) (/ (1- new-month) 12)))
                 (adj-month (1+ (mod (1- new-month) 12))))
            (format "%04d-%02d-%02d" new-year adj-month (nth 3 decoded))))
      ('y (let ((decoded (decode-time time)))
            (format "%04d-%02d-%02d" (+ (nth 5 decoded) n)
                    (nth 4 decoded) (nth 3 decoded)))))))

(defun anvil-agenda--repeater-dates (base-date repeater start end)
  "Return all occurrences of BASE-DATE + REPEATER within [START, END].
If REPEATER is nil, return (BASE-DATE) if in range, else nil."
  (if (null repeater)
      (when (anvil-agenda--in-range-p base-date start end)
        (list base-date))
    (let* ((n (car repeater))
           (unit (cdr repeater))
           (cur base-date)
           results)
      ;; Advance past start if base-date is before start
      (while (string< cur start)
        (setq cur (anvil-agenda--advance-date cur n unit)))
      ;; Collect all occurrences in [start, end]
      (while (not (string< end cur))
        (push cur results)
        (setq cur (anvil-agenda--advance-date cur n unit)))
      (nreverse results))))

(defun anvil-agenda--day-list (start end)
  "Return ((DATE . WEEKDAY) ..) for every day in [START, END] inclusive.
WEEKDAY is C-locale 3-letter abbreviation (Mon..Sun)."
  (let ((cur (anvil-agenda--iso->time start))
        (last (anvil-agenda--iso->time end))
        results)
    (while (not (time-less-p last cur))
      (let ((d (format-time-string "%Y-%m-%d" cur))
            (w (let ((system-time-locale "C"))
                 (format-time-string "%a" cur))))
        (push (cons d w) results))
      (setq cur (time-add cur (days-to-time 1))))
    (nreverse results)))

;;;; --- internal: range / file resolution ----------------------------------

(defun anvil-agenda--resolve-range (opts)
  "Resolve OPTS :start / :end into (START-ISO . END-ISO)."
  (let* ((start (or (plist-get opts :start) 'today))
         (start-str (cond ((eq start 'today) (anvil-agenda--today))
                          ((stringp start) start)
                          (t (error "anvil-agenda: invalid :start (%S)" start))))
         (end (or (plist-get opts :end) 14))
         (end-str (cond ((stringp end) end)
                        ((numberp end)
                         (anvil-agenda--add-days start-str (1- end)))
                        (t (error "anvil-agenda: invalid :end (%S)" end)))))
    (when (string< end-str start-str)
      (error "anvil-agenda: end (%s) is before start (%s)" end-str start-str))
    (cons start-str end-str)))

(defun anvil-agenda--default-files (start end)
  "Default files for date range [START, END]: todo.org + journals-YYYY.org."
  (let* ((sy (string-to-number (substring start 0 4)))
         (ey (string-to-number (substring end   0 4)))
         (todo (expand-file-name "~/Cowork/Notes/capture/todo.org"))
         (journals
          (mapcar (lambda (y)
                    (expand-file-name
                     (format "~/Cowork/Notes/capture/journals-%d.org" y)))
                  (number-sequence sy ey))))
    (cl-remove-if-not #'file-exists-p (cons todo journals))))

;;;; --- internal: bucket helpers -------------------------------------------

(defun anvil-agenda--bucket-get (buckets date)
  "Return (or create) the day bucket plist for DATE in BUCKETS hash table."
  (or (gethash date buckets)
      (let ((b (list :rokuyo nil
                     :scheduled nil
                     :deadlines nil
                     :journal-entries nil)))
        (puthash date b buckets)
        b)))

(defun anvil-agenda--bucket-push (buckets date key entry)
  "Push ENTRY onto KEY list of BUCKETS[DATE]. Creates bucket if missing."
  (let ((b (anvil-agenda--bucket-get buckets date)))
    (plist-put b key (cons entry (plist-get b key)))))

(defun anvil-agenda--bucket-set-rokuyo (buckets date rokuyo)
  "Set :rokuyo of BUCKETS[DATE] to ROKUYO (last write wins, but unique per date)."
  (plist-put (anvil-agenda--bucket-get buckets date) :rokuyo rokuyo))

;;;; --- internal: regexp heading parser ------------------------------------

;; org heading line: `*+ (STATE )TITLE( :tag1:tag2:)`
;; We skip org-mode entirely — pure regexp is ~100x faster on large files.

(defconst anvil-agenda--heading-re
  "^\\(\\*+\\) +\\(.*\\)$"
  "Regexp matching an org heading line. Group 1=stars, 2=rest.")

(defconst anvil-agenda--tags-re
  "\\(:[a-zA-Z0-9_@#%:]+:\\) *$"
  "Regexp matching trailing org tags in a heading.")

(defun anvil-agenda--parse-heading (rest)
  "Parse the REST of a heading (after stars+space) into (TODO TITLE TAGS).
TODO is a string or nil. TITLE is the bare text. TAGS is a list of strings."
  (let (todo title tags)
    ;; Extract trailing tags
    (when (string-match anvil-agenda--tags-re rest)
      (let ((tag-str (match-string 1 rest)))
        (setq tags (split-string tag-str ":" t)
              rest (string-trim-right (substring rest 0 (match-beginning 0))))))
    ;; Check for leading TODO keyword
    (when (string-match
           "\\`\\(TODO\\|DONE\\|NEXT\\|WAIT\\|CANCEL\\|NOTE\\|MEMO\\|ROKUYO\\|SCHEDULED\\|SOMEDAY\\) +"
           rest)
      (setq todo (match-string 1 rest)
            rest (substring rest (match-end 0))))
    (setq title (string-trim rest))
    (list todo title tags)))

(defun anvil-agenda--scan-sched-deadline (limit)
  "From point, scan forward (up to LIMIT) for SCHEDULED/DEADLINE lines.
Returns (SCHED-TS . DEADLINE-TS) where each is the raw timestamp string
(e.g. \"<2026-03-22 Sun +1w>\") or nil."
  (let (sched dead)
    (while (and (< (point) limit)
                (not (and sched dead))
                (not (looking-at "^\\*")))
      (cond
       ((looking-at "^[ \t]*SCHEDULED: +\\(<[^>]+>\\)")
        (setq sched (match-string 1)))
       ((looking-at "^[ \t]*DEADLINE: +\\(<[^>]+>\\)")
        (setq dead (match-string 1))))
      (forward-line 1))
    (cons sched dead)))

;;;; --- internal: file walker (regexp-based) -------------------------------

(defun anvil-agenda--walk-file (path buckets start end opts)
  "Walk org PATH with regexp scanner and populate BUCKETS for [START, END].
OPTS controls which entry kinds to collect. No org-mode initialization —
pure text scanning, ~100x faster than org-map-entries on large files."
  (let* ((inc-rokuyo    (if (plist-member opts :include-rokuyo)
                            (plist-get opts :include-rokuyo) t))
         (inc-scheduled (if (plist-member opts :include-scheduled)
                            (plist-get opts :include-scheduled) t))
         (inc-deadlines (if (plist-member opts :include-deadlines)
                            (plist-get opts :include-deadlines) t))
         (inc-journal   (plist-get opts :include-journal))
         (state-f       (or (plist-get opts :state) '("TODO" "NEXT" "WAIT")))
         (state-list    (cond ((null state-f) nil)
                              ((stringp state-f) (list state-f))
                              ((listp state-f) state-f)))
         ;; path-stack: vector indexed by level, holds heading title at each depth
         (path-stack (make-vector 20 nil)))
    (with-temp-buffer
      (anvil--insert-file path)
      (goto-char (point-min))
      (while (re-search-forward anvil-agenda--heading-re nil t)
        (let* ((stars (length (match-string 1)))
               (rest  (match-string 2))
               (parsed (anvil-agenda--parse-heading rest))
               (kw     (nth 0 parsed))
               (title  (nth 1 parsed))
               (tags   (nth 2 parsed)))
          ;; Update path-stack: clear deeper levels
          (when (< stars (length path-stack))
            (aset path-stack stars title)
            (cl-loop for i from (1+ stars) below (length path-stack)
                     do (aset path-stack i nil)))
          ;; Build path-list from stack (levels 1..stars-1)
          (let ((path-list
                 (cl-loop for i from 1 below stars
                          when (aref path-stack i)
                          collect (aref path-stack i))))
            (cond
             ;; --- ROKUYO
             ((and inc-rokuyo (equal kw "ROKUYO"))
              (let ((date (anvil-agenda--extract-iso title)))
                (when (and date (anvil-agenda--in-range-p date start end))
                  ;; title is "大安 <2026-01-01 Thu>" → extract rokuyo name
                  (let ((rname (if (string-match "\\`\\(.+?\\) <" title)
                                   (match-string 1 title)
                                 title)))
                    (anvil-agenda--bucket-set-rokuyo buckets date rname)))))
             ;; --- TODO/NEXT/WAIT with SCHEDULED or DEADLINE
             ((and kw (or (null state-list) (member kw state-list))
                   (or inc-scheduled inc-deadlines))
              (save-excursion
                (forward-line 1)
                (let* ((limit (min (+ (point) 500) (point-max)))
                       (sd (anvil-agenda--scan-sched-deadline limit))
                       (entry-base (list :title title
                                         :file path
                                         :state kw
                                         :tags tags
                                         :path-list path-list)))
                  (when (and inc-scheduled (car sd))
                    (let* ((pr (anvil-agenda--parse-repeater (car sd)))
                           (dates (anvil-agenda--repeater-dates
                                   (car pr) (cdr pr) start end)))
                      (dolist (d dates)
                        (anvil-agenda--bucket-push
                         buckets d :scheduled entry-base))))
                  (when (and inc-deadlines (cdr sd))
                    (let* ((pr (anvil-agenda--parse-repeater (cdr sd)))
                           (dates (anvil-agenda--repeater-dates
                                   (car pr) (cdr pr) start end)))
                      (dolist (d dates)
                        (anvil-agenda--bucket-push
                         buckets d :deadlines entry-base)))))))
             ;; --- NOTE / MEMO journal entries
             ((and inc-journal (member kw '("NOTE" "MEMO")))
              (let ((date (anvil-agenda--extract-iso title)))
                (when (and date (anvil-agenda--in-range-p date start end))
                  (anvil-agenda--bucket-push
                   buckets date :journal-entries
                   (list :title title
                         :file path
                         :kind kw
                         :tags tags
                         :path-list path-list))))))))))))

;;;; --- public API ---------------------------------------------------------

(defun anvil-agenda (&optional opts)
  "Return a date-range agenda as an ordered list of day plists.

OPTS plist:
  :start    \"YYYY-MM-DD\" or 'today (default 'today)
  :end      \"YYYY-MM-DD\" or N (number of days from start, default 14)
  :files    list of org file paths (default: todo.org + journals-YYYY.org)
  :state    TODO state filter (string or list, default '(\"TODO\" \"NEXT\" \"WAIT\"))
  :include-rokuyo BOOL    (default t)
  :include-scheduled BOOL (default t)
  :include-deadlines BOOL (default t)
  :include-journal BOOL   (default nil)

Each day plist:
  (:date \"YYYY-MM-DD\" :weekday \"Mon\" :rokuyo STR-or-nil
   :scheduled (ENTRY-PLIST ..) :deadlines (..) :journal-entries (..))"
  (let* ((range (anvil-agenda--resolve-range opts))
         (start (car range))
         (end   (cdr range))
         (files (or (plist-get opts :files)
                    (anvil-agenda--default-files start end)))
         (buckets (make-hash-table :test 'equal)))
    (dolist (f files)
      (anvil-agenda--walk-file f buckets start end opts))
    ;; Reverse pushed lists to chronological order of insertion.
    (maphash (lambda (_d b)
               (dolist (k '(:scheduled :deadlines :journal-entries))
                 (plist-put b k (nreverse (plist-get b k)))))
             buckets)
    ;; Build result for every day in range, even if empty.
    (mapcar (lambda (dw)
              (let* ((d (car dw))
                     (w (cdr dw))
                     (b (gethash d buckets)))
                (list :date d
                      :weekday w
                      :rokuyo (and b (plist-get b :rokuyo))
                      :scheduled (and b (plist-get b :scheduled))
                      :deadlines (and b (plist-get b :deadlines))
                      :journal-entries (and b (plist-get b :journal-entries)))))
            (anvil-agenda--day-list start end))))

(defun anvil-agenda-rokuyo (date &optional file)
  "Return the rokuyo string for DATE (ISO) by reading FILE.
FILE defaults to journals-YYYY.org for DATE's year. Returns nil if
no ROKUYO entry exists for DATE."
  (let* ((year (substring date 0 4))
         (path (expand-file-name
                (or file
                    (format "~/Cowork/Notes/capture/journals-%s.org" year)))))
    (unless (file-exists-p path)
      (error "anvil-agenda-rokuyo: file not found: %s" path))
    (let ((buckets (make-hash-table :test 'equal)))
      (anvil-agenda--walk-file path buckets date date
                               '(:include-rokuyo t
                                 :include-scheduled nil
                                 :include-deadlines nil))
      (let ((b (gethash date buckets)))
        (and b (plist-get b :rokuyo))))))

(defun anvil-agenda-overdue (&optional opts)
  "Return overdue TODO entries as a flat list of entry plists.

Definition of overdue: TODO/NEXT/WAIT with SCHEDULED or DEADLINE
strictly before today.

OPTS plist:
  :files    list of org file paths (default: todo.org + journals-2026.org)
  :state    TODO state filter (default '(\"TODO\" \"NEXT\" \"WAIT\"))
  :since    \"YYYY-MM-DD\" earliest scan date (default \"1970-01-01\")

Each entry plist:
  (:date :kind (scheduled|deadline) :title :file :state :tags :path-list)"
  (let* ((today (anvil-agenda--today))
         (start (or (plist-get opts :since) "1970-01-01"))
         (end (anvil-agenda--add-days today -1))  ; strictly before today
         (state-f (or (plist-get opts :state) '("TODO" "NEXT" "WAIT")))
         (files (or (plist-get opts :files)
                    (anvil-agenda--default-files start today)))
         (buckets (make-hash-table :test 'equal)))
    (when (not (string< end start))
      (dolist (f files)
        (anvil-agenda--walk-file f buckets start end
                                 (list :include-rokuyo nil
                                       :include-scheduled t
                                       :include-deadlines t
                                       :state state-f))))
    (let (results)
      (maphash
       (lambda (date b)
         (dolist (e (nreverse (plist-get b :scheduled)))
           (push (append (list :date date :kind 'scheduled) e) results))
         (dolist (e (nreverse (plist-get b :deadlines)))
           (push (append (list :date date :kind 'deadline) e) results)))
       buckets)
      (sort results (lambda (a b)
                      (string< (plist-get a :date)
                               (plist-get b :date)))))))

;;;; --- discoverability ----------------------------------------------------

(defun anvil-agenda-helpers-list ()
  "Return a list of all anvil-agenda* user-facing function names."
  (let (results)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (let ((n (symbol-name sym)))
                    (and (string-prefix-p "anvil-agenda" n)
                         (not (string-match-p "--" n)))))
         (push sym results))))
    (sort results (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(provide 'anvil-agenda)
;;; anvil-agenda.el ends here
