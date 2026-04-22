;;; anvil-memory-test.el --- Tests for anvil-memory engine -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for Doc 29 Phase 1a — memory-engine metadata index,
;; scan walker, TTL audit, access tracker, list.
;;
;; Every test isolates itself with a fresh `anvil-memory-db-path'
;; temp SQLite file and a temp "memory root" directory populated
;; with .md fixtures — the user's real ~/.claude/projects/*/memory/
;; layout is never touched.
;;
;; Tests `skip-unless' their capability tag is in
;; `anvil-memory-supported' so the TDD-lock commit can land before
;; the impl.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
;; Non-fatal: TDD-lock lands before the impl.
(require 'anvil-memory nil t)


;;;; --- fixture helpers ----------------------------------------------------

(defun anvil-memory-test--supported-p (tag)
  "Return non-nil when TAG is in `anvil-memory-supported'."
  (and (boundp 'anvil-memory-supported)
       (memq tag anvil-memory-supported)))

(defun anvil-memory-test--write (path content &optional mtime)
  "Write CONTENT to PATH; optionally set the file mtime (unix epoch)."
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert content))
  (when mtime
    (set-file-times path (seconds-to-time mtime))))

(defmacro anvil-memory-test--with-env (&rest body)
  "Run BODY with a fresh temp DB + fresh temp memory root directory.
Binds:
  `anvil-memory-db-path'  — fresh temp .db
  `anvil-memory-roots'    — list of the single temp memory dir
  root                    — the temp dir path (as a let-local var)"
  (declare (indent 0))
  `(let* ((root (make-temp-file "anvil-memtest-" t))
          (anvil-memory-db-path (make-temp-file "anvil-memidx-" nil ".db"))
          (anvil-memory--db nil)
          (anvil-memory-roots (list root)))
     (unwind-protect
         (progn
           (when (fboundp 'anvil-memory-enable)
             (anvil-memory-enable))
           ,@body)
       (when (fboundp 'anvil-memory-disable)
         (anvil-memory-disable))
       (ignore-errors (delete-file anvil-memory-db-path))
       (ignore-errors (delete-directory root t)))))

(defun anvil-memory-test--seed-mixed (root)
  "Populate ROOT with one fixture per memory type + MEMORY.md index."
  (anvil-memory-test--write
   (expand-file-name "MEMORY.md" root)
   "- [user role](user_role.md) — short\n")
  (anvil-memory-test--write
   (expand-file-name "user_role.md" root)
   "---\ntype: user\n---\nI am a dev.\n")
  (anvil-memory-test--write
   (expand-file-name "feedback_commit_rules.md" root)
   "---\ntype: feedback\n---\nAlways co-authored-by.\n")
  (anvil-memory-test--write
   (expand-file-name "project_anvil_phase9.md" root)
   "---\ntype: project\n---\nPhase 9 scope.\n")
  (anvil-memory-test--write
   (expand-file-name "reference_some_link.md" root)
   "---\ntype: reference\n---\nhttps://example.com\n"))


;;;; --- scan ---------------------------------------------------------------

(ert-deftest anvil-memory-test/scan-populates-four-types ()
  "`anvil-memory-scan' walks ROOT and creates one row per .md file
(MEMORY.md is skipped)."
  (skip-unless (anvil-memory-test--supported-p 'scan))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-mixed root)
    (let ((count (anvil-memory-scan)))
      (should (= 4 count)))
    (let ((rows (anvil-memory-list)))
      (should (= 4 (length rows))))))

(ert-deftest anvil-memory-test/scan-infers-type-from-prefix ()
  "Type is derived from the filename prefix."
  (skip-unless (anvil-memory-test--supported-p 'scan))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-mixed root)
    (anvil-memory-scan)
    (let* ((rows (anvil-memory-list))
           (by-file (mapcar (lambda (r)
                              (cons (file-name-nondirectory (plist-get r :file))
                                    (plist-get r :type)))
                            rows)))
      (should (equal 'user      (cdr (assoc "user_role.md" by-file))))
      (should (equal 'feedback  (cdr (assoc "feedback_commit_rules.md" by-file))))
      (should (equal 'project   (cdr (assoc "project_anvil_phase9.md" by-file))))
      (should (equal 'reference (cdr (assoc "reference_some_link.md" by-file)))))))

(ert-deftest anvil-memory-test/scan-skips-memory-md ()
  "The MEMORY.md index file itself is never indexed."
  (skip-unless (anvil-memory-test--supported-p 'scan))
  (anvil-memory-test--with-env
    (anvil-memory-test--write (expand-file-name "MEMORY.md" root) "index\n")
    (anvil-memory-test--write
     (expand-file-name "feedback_x.md" root) "body\n")
    (anvil-memory-scan)
    (let ((files (mapcar (lambda (r)
                           (file-name-nondirectory (plist-get r :file)))
                         (anvil-memory-list))))
      (should (member "feedback_x.md" files))
      (should-not (member "MEMORY.md" files)))))

(ert-deftest anvil-memory-test/scan-idempotent ()
  "Running scan twice does not duplicate rows."
  (skip-unless (anvil-memory-test--supported-p 'scan))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-mixed root)
    (anvil-memory-scan)
    (anvil-memory-scan)
    (should (= 4 (length (anvil-memory-list))))))

(ert-deftest anvil-memory-test/scan-handles-empty-dir ()
  "An empty memory root produces zero rows, never errors."
  (skip-unless (anvil-memory-test--supported-p 'scan))
  (anvil-memory-test--with-env
    (should (= 0 (anvil-memory-scan)))
    (should (null (anvil-memory-list)))))

(ert-deftest anvil-memory-test/scan-assigns-memo-type-fallback ()
  "A .md file whose prefix does not match any known type gets
`memo' as the fallback."
  (skip-unless (anvil-memory-test--supported-p 'scan))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "unstructured_notes.md" root) "body\n")
    (anvil-memory-scan)
    (let ((row (car (anvil-memory-list))))
      (should (eq 'memo (plist-get row :type))))))


;;;; --- audit (TTL policy) -------------------------------------------------

(ert-deftest anvil-memory-test/audit-user-never-expires ()
  "`user' memories never raise an expiry flag by default."
  (skip-unless (anvil-memory-test--supported-p 'audit))
  (anvil-memory-test--with-env
    (let ((old (- (truncate (float-time)) (* 365 86400))))
      (anvil-memory-test--write
       (expand-file-name "user_ancient.md" root) "x\n" old))
    (anvil-memory-scan)
    (let* ((report (anvil-memory-audit))
           (flags (mapcar (lambda (e) (plist-get e :flag)) report)))
      (should (or (null flags)
                  (cl-every (lambda (f) (null f)) flags))))))

(ert-deftest anvil-memory-test/audit-project-older-than-hard-ttl-expired ()
  "A `project' memory older than 90 days is flagged expired."
  (skip-unless (anvil-memory-test--supported-p 'audit))
  (anvil-memory-test--with-env
    (let ((old (- (truncate (float-time)) (* 100 86400))))
      (anvil-memory-test--write
       (expand-file-name "project_old.md" root) "x\n" old))
    (anvil-memory-scan)
    (let* ((report (anvil-memory-audit))
           (entry (car report)))
      (should (eq 'expired (plist-get entry :flag))))))

(ert-deftest anvil-memory-test/audit-project-soft-window-flags-needs-recheck ()
  "A `project' memory 60-90d old is flagged needs-recheck."
  (skip-unless (anvil-memory-test--supported-p 'audit))
  (anvil-memory-test--with-env
    (let ((old (- (truncate (float-time)) (* 70 86400))))
      (anvil-memory-test--write
       (expand-file-name "project_midaged.md" root) "x\n" old))
    (anvil-memory-scan)
    (let* ((report (anvil-memory-audit))
           (entry (car report)))
      (should (eq 'needs-recheck (plist-get entry :flag))))))

(ert-deftest anvil-memory-test/audit-reference-30d-expired ()
  "A `reference' memory older than 30 days is flagged expired."
  (skip-unless (anvil-memory-test--supported-p 'audit))
  (anvil-memory-test--with-env
    (let ((old (- (truncate (float-time)) (* 35 86400))))
      (anvil-memory-test--write
       (expand-file-name "reference_dead.md" root) "x\n" old))
    (anvil-memory-scan)
    (let* ((report (anvil-memory-audit))
           (entry (car report)))
      (should (eq 'expired (plist-get entry :flag))))))

(ert-deftest anvil-memory-test/audit-filter-by-type ()
  "`audit' restricts to TYPE when given."
  (skip-unless (anvil-memory-test--supported-p 'audit))
  (anvil-memory-test--with-env
    (let ((old (- (truncate (float-time)) (* 100 86400))))
      (anvil-memory-test--write
       (expand-file-name "project_a.md" root) "x\n" old)
      (anvil-memory-test--write
       (expand-file-name "reference_b.md" root) "x\n" old))
    (anvil-memory-scan)
    (let* ((report (anvil-memory-audit 'project))
           (types (mapcar (lambda (e) (plist-get e :type)) report)))
      (should (cl-every (lambda (ty) (eq ty 'project)) types))
      (should (= 1 (length report))))))

(ert-deftest anvil-memory-test/audit-feedback-under-soft-window-clean ()
  "A `feedback' memory inside the 180-day soft window is flagged nil."
  (skip-unless (anvil-memory-test--supported-p 'audit))
  (anvil-memory-test--with-env
    (let ((recent (- (truncate (float-time)) (* 30 86400))))
      (anvil-memory-test--write
       (expand-file-name "feedback_recent.md" root) "x\n" recent))
    (anvil-memory-scan)
    (let* ((report (anvil-memory-audit))
           (entry (car report)))
      (should (null (plist-get entry :flag))))))


;;;; --- access -------------------------------------------------------------

(ert-deftest anvil-memory-test/access-bumps-count ()
  "`anvil-memory-access' increments access-count."
  (skip-unless (anvil-memory-test--supported-p 'access))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_x.md" root) "x\n")
    (anvil-memory-scan)
    (let* ((before (plist-get (car (anvil-memory-list)) :access-count)))
      (anvil-memory-access
       (expand-file-name "feedback_x.md" root))
      (anvil-memory-access
       (expand-file-name "feedback_x.md" root))
      (let ((after (plist-get (car (anvil-memory-list)) :access-count)))
        (should (= (+ 2 (or before 0)) after))))))

(ert-deftest anvil-memory-test/access-updates-last-accessed ()
  "`anvil-memory-access' sets last-accessed to the current epoch."
  (skip-unless (anvil-memory-test--supported-p 'access))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_x.md" root) "x\n")
    (anvil-memory-scan)
    (let ((before (truncate (float-time))))
      (anvil-memory-access
       (expand-file-name "feedback_x.md" root))
      (let ((la (plist-get (car (anvil-memory-list)) :last-accessed)))
        (should (integerp la))
        (should (>= la before))))))

(ert-deftest anvil-memory-test/access-unknown-file-returns-nil ()
  "Accessing a file that was not indexed returns nil, never errors."
  (skip-unless (anvil-memory-test--supported-p 'access))
  (anvil-memory-test--with-env
    (anvil-memory-scan)
    (should (null (anvil-memory-access
                   (expand-file-name "feedback_ghost.md" root))))))


;;;; --- list ---------------------------------------------------------------

(ert-deftest anvil-memory-test/list-filters-by-type ()
  "`anvil-memory-list' with TYPE filter returns only matching rows."
  (skip-unless (anvil-memory-test--supported-p 'list))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-mixed root)
    (anvil-memory-scan)
    (let ((feedback (anvil-memory-list 'feedback))
          (reference (anvil-memory-list 'reference)))
      (should (= 1 (length feedback)))
      (should (= 1 (length reference)))
      (should (eq 'feedback  (plist-get (car feedback) :type)))
      (should (eq 'reference (plist-get (car reference) :type))))))

(ert-deftest anvil-memory-test/list-returns-plist-with-expected-keys ()
  "Every list row includes the metadata keys Phase 1a commits to."
  (skip-unless (anvil-memory-test--supported-p 'list))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_x.md" root) "x\n")
    (anvil-memory-scan)
    (let ((row (car (anvil-memory-list))))
      (dolist (k '(:file :type :created :last-accessed
                         :access-count :validity-prior))
        (should (plist-member row k))))))


;;;; --- Phase 1b: FTS5 search -----------------------------------------------

(ert-deftest anvil-memory-test/search-returns-matches ()
  "FTS5 search returns rows whose bodies contain the query token."
  (skip-unless (anvil-memory-test--supported-p 'search))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_widgets.md" root)
     "---\ntype: feedback\n---\nwidgets always foo the bar\n")
    (anvil-memory-test--write
     (expand-file-name "feedback_plumbing.md" root)
     "---\ntype: feedback\n---\npipes and drains only\n")
    (anvil-memory-scan)
    (let* ((hits (anvil-memory-search "widgets"))
           (files (mapcar (lambda (h)
                            (file-name-nondirectory (plist-get h :file)))
                          hits)))
      (should (member "feedback_widgets.md" files))
      (should-not (member "feedback_plumbing.md" files)))))

(ert-deftest anvil-memory-test/search-result-has-snippet ()
  "Each search result carries :file / :type / :snippet keys."
  (skip-unless (anvil-memory-test--supported-p 'search))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "project_alpha.md" root)
     "---\ntype: project\n---\nalpha milestone snapshot details\n")
    (anvil-memory-scan)
    (let ((row (car (anvil-memory-search "alpha"))))
      (dolist (k '(:file :type :snippet))
        (should (plist-member row k)))
      (should (stringp (plist-get row :snippet))))))

(ert-deftest anvil-memory-test/search-filter-by-type ()
  "`:type' restricts results to rows of that type."
  (skip-unless (anvil-memory-test--supported-p 'search))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_foo.md" root)
     "foo bar baz\n")
    (anvil-memory-test--write
     (expand-file-name "project_foo.md" root)
     "foo bar baz\n")
    (anvil-memory-scan)
    (let ((hits (anvil-memory-search "foo" :type 'feedback)))
      (should (= 1 (length hits)))
      (should (eq 'feedback (plist-get (car hits) :type))))))

(ert-deftest anvil-memory-test/search-respects-limit ()
  "`:limit' caps the result count."
  (skip-unless (anvil-memory-test--supported-p 'search))
  (anvil-memory-test--with-env
    (dotimes (i 5)
      (anvil-memory-test--write
       (expand-file-name (format "feedback_n%d.md" i) root)
       "widget widget widget\n"))
    (anvil-memory-scan)
    (let ((hits (anvil-memory-search "widget" :limit 2)))
      (should (= 2 (length hits))))))

(ert-deftest anvil-memory-test/search-empty-query-returns-empty ()
  "Empty / whitespace-only query returns empty list, never errors."
  (skip-unless (anvil-memory-test--supported-p 'search))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_x.md" root) "x\n")
    (anvil-memory-scan)
    (should (null (anvil-memory-search "")))
    (should (null (anvil-memory-search "   ")))))

(ert-deftest anvil-memory-test/search-reflects-body-update ()
  "Re-scanning a changed file updates the FTS index."
  (skip-unless (anvil-memory-test--supported-p 'search))
  (anvil-memory-test--with-env
    (let ((path (expand-file-name "feedback_x.md" root)))
      (anvil-memory-test--write path "original content alpha\n")
      (anvil-memory-scan)
      (should (anvil-memory-search "alpha"))
      (should-not (anvil-memory-search "beta"))
      (anvil-memory-test--write path "revised content beta\n")
      (anvil-memory-scan)
      (should-not (anvil-memory-search "alpha"))
      (should (anvil-memory-search "beta")))))


;;;; --- Phase 1b: save-check (top-N similar) --------------------------------

(ert-deftest anvil-memory-test/save-check-returns-similar-candidates ()
  "`save-check' returns top-N memories whose body overlaps the draft."
  (skip-unless (anvil-memory-test--supported-p 'save-check))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_existing.md" root)
     "Always sign commits with Co-Authored-By trailer for Claude sessions.\n")
    (anvil-memory-test--write
     (expand-file-name "project_unrelated.md" root)
     "Inventory tracker for the warehouse project.\n")
    (anvil-memory-scan)
    (let* ((hits (anvil-memory-save-check
                  "commit rules"
                  "Always sign commits with Co-Authored-By when Claude helped."))
           (files (mapcar (lambda (h)
                            (file-name-nondirectory (plist-get h :file)))
                          hits)))
      (should (member "feedback_existing.md" files))
      (should-not (member "project_unrelated.md" files)))))

(ert-deftest anvil-memory-test/save-check-no-match-returns-empty ()
  "A unique draft with no indexed overlap returns nil."
  (skip-unless (anvil-memory-test--supported-p 'save-check))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_a.md" root)
     "completely different topic about databases\n")
    (anvil-memory-scan)
    (should (null (anvil-memory-save-check
                   "something else entirely"
                   "xyz qrs tuv unrelated keywords")))))

(ert-deftest anvil-memory-test/save-check-result-has-similarity-score ()
  "Each candidate carries :similarity (0-1 float)."
  (skip-unless (anvil-memory-test--supported-p 'save-check))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_x.md" root)
     "shared words common keywords important phrase\n")
    (anvil-memory-scan)
    (let ((hit (car (anvil-memory-save-check
                     "x" "shared words common keywords important"))))
      (should hit)
      (should (numberp (plist-get hit :similarity)))
      (should (>= (plist-get hit :similarity) 0.0))
      (should (<= (plist-get hit :similarity) 1.0)))))


;;;; --- Phase 1b: duplicates ------------------------------------------------

(ert-deftest anvil-memory-test/duplicates-finds-high-overlap-pair ()
  "Two memories whose body overlap exceeds the threshold appear as a pair."
  (skip-unless (anvil-memory-test--supported-p 'duplicates))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_a.md" root)
     "alpha beta gamma delta epsilon zeta eta theta\n")
    (anvil-memory-test--write
     (expand-file-name "feedback_b.md" root)
     "alpha beta gamma delta epsilon zeta eta theta iota\n")
    (anvil-memory-test--write
     (expand-file-name "feedback_c.md" root)
     "xi omicron pi rho sigma tau upsilon phi chi psi\n")
    (anvil-memory-scan)
    (let* ((pairs (anvil-memory-duplicates 0.6))
           (files-in-pairs
            (cl-loop for p in pairs
                     collect (cons (file-name-nondirectory
                                    (car (plist-get p :pair)))
                                   (file-name-nondirectory
                                    (cadr (plist-get p :pair)))))))
      (should (cl-find-if
               (lambda (f)
                 (or (equal f '("feedback_a.md" . "feedback_b.md"))
                     (equal f '("feedback_b.md" . "feedback_a.md"))))
               files-in-pairs))
      (should-not (cl-find-if
                   (lambda (f)
                     (or (equal (car f) "feedback_c.md")
                         (equal (cdr f) "feedback_c.md")))
                   files-in-pairs)))))

(ert-deftest anvil-memory-test/duplicates-respects-threshold ()
  "Higher threshold shrinks the result set."
  (skip-unless (anvil-memory-test--supported-p 'duplicates))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_a.md" root) "foo bar baz qux\n")
    (anvil-memory-test--write
     (expand-file-name "feedback_b.md" root) "foo bar quux corge\n")
    (anvil-memory-scan)
    (let ((pairs-loose (anvil-memory-duplicates 0.2))
          (pairs-tight (anvil-memory-duplicates 0.9)))
      (should (>= (length pairs-loose) (length pairs-tight)))
      (should (null pairs-tight)))))

(ert-deftest anvil-memory-test/duplicates-pair-has-similarity ()
  "Each duplicate entry exposes :pair and :similarity."
  (skip-unless (anvil-memory-test--supported-p 'duplicates))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_a.md" root) "alpha beta gamma\n")
    (anvil-memory-test--write
     (expand-file-name "feedback_b.md" root) "alpha beta gamma\n")
    (anvil-memory-scan)
    (let ((entry (car (anvil-memory-duplicates 0.5))))
      (should entry)
      (should (plist-member entry :pair))
      (should (plist-member entry :similarity))
      (should (consp (plist-get entry :pair)))
      (should (= 2 (length (plist-get entry :pair)))))))


;;;; --- Phase 1b: audit URL HEAD ------------------------------------------

(ert-deftest anvil-memory-test/audit-urls-flags-reference-status ()
  "When `:with-urls' is set, reference-type rows gain a :url-status field."
  (skip-unless (anvil-memory-test--supported-p 'audit-urls))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "reference_ok.md" root)
     "See https://example.com for details.\n")
    (anvil-memory-scan)
    (cl-letf* (((symbol-function 'anvil-http-head)
                (lambda (&rest _) '(:status 200))))
      (let ((row (car (anvil-memory-audit nil :with-urls t))))
        (should (eq 'ok (plist-get row :url-status)))))))

(ert-deftest anvil-memory-test/audit-urls-detects-404 ()
  "A non-2xx HTTP status on the first URL marks :url-status as `broken'."
  (skip-unless (anvil-memory-test--supported-p 'audit-urls))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "reference_dead.md" root)
     "https://example.com/gone is now 404\n")
    (anvil-memory-scan)
    (cl-letf* (((symbol-function 'anvil-http-head)
                (lambda (&rest _) '(:status 404))))
      (let ((row (car (anvil-memory-audit nil :with-urls t))))
        (should (eq 'broken (plist-get row :url-status)))))))

(ert-deftest anvil-memory-test/audit-urls-skips-non-reference-types ()
  "URL HEAD is only attempted for reference-type rows."
  (skip-unless (anvil-memory-test--supported-p 'audit-urls))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_x.md" root)
     "Avoid https://example.com/doc link-rotting.\n")
    (anvil-memory-scan)
    (let ((calls 0))
      (cl-letf* (((symbol-function 'anvil-http-head)
                  (lambda (&rest _)
                    (setq calls (1+ calls))
                    '(:status 200))))
        (anvil-memory-audit nil :with-urls t)
        (should (= 0 calls))))))

(ert-deftest anvil-memory-test/audit-default-skips-url-check ()
  "Omitting `:with-urls' never touches `anvil-http-head'."
  (skip-unless (anvil-memory-test--supported-p 'audit-urls))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "reference_a.md" root)
     "https://example.com lives\n")
    (anvil-memory-scan)
    (let ((calls 0))
      (cl-letf* (((symbol-function 'anvil-http-head)
                  (lambda (&rest _)
                    (setq calls (1+ calls))
                    '(:status 200))))
        (anvil-memory-audit)
        (should (= 0 calls))))))


;;;; --- Phase 2a: decay-score ------------------------------------------------

(ert-deftest anvil-memory-test/decay-score-key-present-with-opt ()
  "`anvil-memory-list :with-decay t' decorates each row with a
fresh :decay-score float."
  (skip-unless (anvil-memory-test--supported-p 'decay))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_x.md" root) "x\n")
    (anvil-memory-scan)
    (let ((row (car (anvil-memory-list nil :with-decay t))))
      (should (plist-member row :decay-score))
      (should (numberp (plist-get row :decay-score))))))

(ert-deftest anvil-memory-test/decay-recent-access-outranks-stale ()
  "Two same-type memories differ in decay: recent+accessed > stale+unaccessed."
  (skip-unless (anvil-memory-test--supported-p 'decay))
  (anvil-memory-test--with-env
    (let ((old (- (truncate (float-time)) (* 30 86400))))
      (anvil-memory-test--write
       (expand-file-name "feedback_stale.md" root) "s\n" old))
    (anvil-memory-test--write
     (expand-file-name "feedback_hot.md" root) "h\n")
    (anvil-memory-scan)
    (anvil-memory-access (expand-file-name "feedback_hot.md" root))
    (anvil-memory-access (expand-file-name "feedback_hot.md" root))
    (let* ((rows (anvil-memory-list nil :with-decay t))
           (hot (cl-find "feedback_hot.md" rows
                         :key (lambda (r)
                                (file-name-nondirectory (plist-get r :file)))
                         :test #'equal))
           (stale (cl-find "feedback_stale.md" rows
                           :key (lambda (r)
                                  (file-name-nondirectory
                                   (plist-get r :file)))
                           :test #'equal)))
      (should (> (plist-get hot :decay-score)
                 (plist-get stale :decay-score))))))

(ert-deftest anvil-memory-test/decay-type-weight-user-beats-reference ()
  "With identical recency / access, user type outranks reference type."
  (skip-unless (anvil-memory-test--supported-p 'decay))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "user_a.md" root) "a\n")
    (anvil-memory-test--write
     (expand-file-name "reference_a.md" root) "a\n")
    (anvil-memory-scan)
    (let* ((rows (anvil-memory-list nil :with-decay t))
           (u (cl-find 'user rows :key (lambda (r) (plist-get r :type))))
           (r (cl-find 'reference rows
                       :key (lambda (r) (plist-get r :type)))))
      (should (> (plist-get u :decay-score)
                 (plist-get r :decay-score))))))

(ert-deftest anvil-memory-test/decay-null-last-accessed-returns-number ()
  "An unaccessed row still produces a valid (non-nil) decay score."
  (skip-unless (anvil-memory-test--supported-p 'decay))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_x.md" root) "x\n")
    (anvil-memory-scan)
    (let ((row (car (anvil-memory-list nil :with-decay t))))
      (should (numberp (plist-get row :decay-score)))
      (should (>= (plist-get row :decay-score) 0.0))
      (should (<= (plist-get row :decay-score) 1.0)))))

(ert-deftest anvil-memory-test/list-sort-by-decay-desc ()
  "`anvil-memory-list :sort 'decay' orders rows by :decay-score descending."
  (skip-unless (anvil-memory-test--supported-p 'decay))
  (anvil-memory-test--with-env
    (let ((old (- (truncate (float-time)) (* 60 86400))))
      (anvil-memory-test--write
       (expand-file-name "feedback_stale.md" root) "s\n" old))
    (anvil-memory-test--write
     (expand-file-name "feedback_hot.md" root) "h\n")
    (anvil-memory-scan)
    (anvil-memory-access (expand-file-name "feedback_hot.md" root))
    (let* ((rows (anvil-memory-list nil :with-decay t :sort 'decay))
           (files (mapcar (lambda (r)
                            (file-name-nondirectory (plist-get r :file)))
                          rows)))
      (should (equal "feedback_hot.md" (car files)))
      (should (equal "feedback_stale.md" (cadr files))))))


;;;; --- Phase 2a: memory-promote -------------------------------------------

(ert-deftest anvil-memory-test/promote-renames-file-on-disk ()
  "Promoting a MEMO row to feedback renames the file on disk."
  (skip-unless (anvil-memory-test--supported-p 'promote))
  (anvil-memory-test--with-env
    (let ((old (expand-file-name "notes.md" root)))
      (anvil-memory-test--write old "body\n")
      (anvil-memory-scan)
      (anvil-memory-promote old 'feedback)
      (should-not (file-exists-p old))
      (should (file-exists-p (expand-file-name "feedback_notes.md" root))))))

(ert-deftest anvil-memory-test/promote-updates-metadata-row ()
  "Promote updates the index so `memory-list' reflects new path + type."
  (skip-unless (anvil-memory-test--supported-p 'promote))
  (anvil-memory-test--with-env
    (let ((old (expand-file-name "notes.md" root)))
      (anvil-memory-test--write old "body\n")
      (anvil-memory-scan)
      (anvil-memory-promote old 'feedback))
    (let* ((rows (anvil-memory-list))
           (row (car rows)))
      (should (= 1 (length rows)))
      (should (equal 'feedback (plist-get row :type)))
      (should (string-match-p "/feedback_notes\\.md\\'"
                              (plist-get row :file))))))

(ert-deftest anvil-memory-test/promote-strips-existing-prefix ()
  "Promote strips the old type prefix before prefixing the new type."
  (skip-unless (anvil-memory-test--supported-p 'promote))
  (anvil-memory-test--with-env
    (let ((old (expand-file-name "project_alpha.md" root)))
      (anvil-memory-test--write old "body\n")
      (anvil-memory-scan)
      (anvil-memory-promote old 'feedback))
    (should (file-exists-p (expand-file-name "feedback_alpha.md" root)))))

(ert-deftest anvil-memory-test/promote-errors-on-missing-source ()
  "Promoting an unknown file signals a user-error, disk untouched."
  (skip-unless (anvil-memory-test--supported-p 'promote))
  (anvil-memory-test--with-env
    (anvil-memory-scan)
    (should-error
     (anvil-memory-promote (expand-file-name "nope.md" root) 'feedback)
     :type 'user-error)))

(ert-deftest anvil-memory-test/promote-errors-on-destination-exists ()
  "Promote refuses when the destination filename already exists."
  (skip-unless (anvil-memory-test--supported-p 'promote))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "project_alpha.md" root) "a\n")
    (anvil-memory-test--write
     (expand-file-name "feedback_alpha.md" root) "b\n")
    (anvil-memory-scan)
    (should-error
     (anvil-memory-promote (expand-file-name "project_alpha.md" root)
                           'feedback)
     :type 'user-error)))

(ert-deftest anvil-memory-test/promote-refreshes-fts-body ()
  "Promote rewrites the FTS row so search by the new path works."
  (skip-unless (anvil-memory-test--supported-p 'promote))
  (anvil-memory-test--with-env
    (let ((old (expand-file-name "notes.md" root)))
      (anvil-memory-test--write old "uniquetoken\n")
      (anvil-memory-scan)
      (anvil-memory-promote old 'feedback))
    (let* ((hits (anvil-memory-search "uniquetoken"))
           (files (mapcar (lambda (h)
                            (file-name-nondirectory (plist-get h :file)))
                          hits)))
      (should (equal '("feedback_notes.md") files)))))


;;;; --- Phase 2a: memory-regenerate-index ----------------------------------

(ert-deftest anvil-memory-test/regenerate-returns-bullet-list ()
  "regenerate-index emits `- [NAME](FILE) — DESC' lines for every row."
  (skip-unless (anvil-memory-test--supported-p 'regenerate))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_a.md" root)
     "---\nname: Alpha\ndescription: The alpha rule\ntype: feedback\n---\nbody\n")
    (anvil-memory-test--write
     (expand-file-name "project_b.md" root)
     "---\nname: Beta\ndescription: The beta project\ntype: project\n---\nbody\n")
    (anvil-memory-scan)
    (let ((body (anvil-memory-regenerate-index root)))
      (should (string-match-p
               "^- \\[Alpha\\](feedback_a\\.md) — The alpha rule" body))
      (should (string-match-p
               "^- \\[Beta\\](project_b\\.md) — The beta project" body)))))

(ert-deftest anvil-memory-test/regenerate-sorts-by-decay-desc ()
  "The regenerated list is ordered by :decay-score descending."
  (skip-unless (anvil-memory-test--supported-p 'regenerate))
  (anvil-memory-test--with-env
    (let ((old (- (truncate (float-time)) (* 90 86400))))
      (anvil-memory-test--write
       (expand-file-name "feedback_stale.md" root)
       "---\nname: Stale\ndescription: old\n---\n" old))
    (anvil-memory-test--write
     (expand-file-name "feedback_hot.md" root)
     "---\nname: Hot\ndescription: fresh\n---\n")
    (anvil-memory-scan)
    (anvil-memory-access (expand-file-name "feedback_hot.md" root))
    (let* ((body (anvil-memory-regenerate-index root))
           (hot-pos (string-match "Hot\\](" body))
           (stale-pos (string-match "Stale\\](" body)))
      (should hot-pos)
      (should stale-pos)
      (should (< hot-pos stale-pos)))))

(ert-deftest anvil-memory-test/regenerate-filters-by-root ()
  "Only rows under ROOT contribute to the output."
  (skip-unless (anvil-memory-test--supported-p 'regenerate))
  (anvil-memory-test--with-env
    (let ((other (make-temp-file "anvil-memother-" t)))
      (unwind-protect
          (progn
            (anvil-memory-test--write
             (expand-file-name "feedback_here.md" root)
             "---\nname: Here\ndescription: local\n---\n")
            (anvil-memory-test--write
             (expand-file-name "feedback_there.md" other)
             "---\nname: There\ndescription: other\n---\n")
            (let ((anvil-memory-roots (list root other)))
              (anvil-memory-scan))
            (let ((body (anvil-memory-regenerate-index root)))
              (should (string-match-p "feedback_here\\.md" body))
              (should-not (string-match-p "feedback_there\\.md" body))))
        (ignore-errors (delete-directory other t))))))

(ert-deftest anvil-memory-test/regenerate-fallback-name-from-filename ()
  "Missing frontmatter name falls back to the filename without prefix/ext."
  (skip-unless (anvil-memory-test--supported-p 'regenerate))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_something_cool.md" root)
     "no frontmatter here\n")
    (anvil-memory-scan)
    (let ((body (anvil-memory-regenerate-index root)))
      (should (string-match-p "\\[something cool\\]" body)))))

(ert-deftest anvil-memory-test/regenerate-empty-root-returns-empty-string ()
  "A root with no indexed rows produces the empty string, not nil."
  (skip-unless (anvil-memory-test--supported-p 'regenerate))
  (anvil-memory-test--with-env
    (anvil-memory-scan)
    (should (equal "" (anvil-memory-regenerate-index root)))))


;;;; --- Phase 2b-i trigram tokenizer ---------------------------------------

(ert-deftest anvil-memory-test/trigram-supports-probe ()
  "Probe reports non-nil when the SQLite build ships FTS5 trigram."
  (skip-unless (anvil-memory-test--supported-p 'reindex-fts))
  (anvil-memory-test--with-env
    (let ((db (anvil-memory--db)))
      ;; Probe is allowed to return nil on older SQLite — we assert
      ;; only that it is a boolean (non-signaling).
      (should (memq (and (anvil-memory--sqlite-supports-trigram-p db) t)
                    '(t nil))))))

(ert-deftest anvil-memory-test/trigram-resolve-auto-prefers-trigram ()
  "`auto' resolves to trigram on an SQLite that supports it."
  (skip-unless (anvil-memory-test--supported-p 'reindex-fts))
  (anvil-memory-test--with-env
    (let* ((db (anvil-memory--db))
           (anvil-memory-fts-tokenizer 'auto))
      (when (anvil-memory--sqlite-supports-trigram-p db)
        (should (eq 'trigram (anvil-memory--resolve-tokenizer db)))))))

(ert-deftest anvil-memory-test/trigram-resolve-forced-values ()
  "`trigram' / `unicode61' are returned verbatim when forced."
  (skip-unless (anvil-memory-test--supported-p 'reindex-fts))
  (anvil-memory-test--with-env
    (let ((db (anvil-memory--db)))
      (let ((anvil-memory-fts-tokenizer 'trigram))
        (should (eq 'trigram (anvil-memory--resolve-tokenizer db))))
      (let ((anvil-memory-fts-tokenizer 'unicode61))
        (should (eq 'unicode61 (anvil-memory--resolve-tokenizer db)))))))

(ert-deftest anvil-memory-test/trigram-resolve-invalid-errors ()
  "Unknown tokenizer symbol raises a user-error rather than silently booting."
  (skip-unless (anvil-memory-test--supported-p 'reindex-fts))
  (anvil-memory-test--with-env
    (let ((db (anvil-memory--db))
          (anvil-memory-fts-tokenizer 'bogus))
      (should-error (anvil-memory--resolve-tokenizer db)))))

(ert-deftest anvil-memory-test/trigram-ensure-schema-honours-auto ()
  "Fresh DB picks the auto-resolved tokenizer when creating the FTS table."
  (skip-unless (anvil-memory-test--supported-p 'reindex-fts))
  (anvil-memory-test--with-env
    (let* ((db (anvil-memory--db))
           (current (anvil-memory--current-fts-tokenizer db)))
      (if (anvil-memory--sqlite-supports-trigram-p db)
          (should (eq 'trigram current))
        (should (eq 'unicode61 current))))))

(ert-deftest anvil-memory-test/trigram-reindex-switches-tokenizer ()
  "Reindex swaps the FTS table in-place; current-tokenizer reflects it."
  (skip-unless (anvil-memory-test--supported-p 'reindex-fts))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_a.md" root) "---\ntype: feedback\n---\nhello world\n")
    (anvil-memory-scan)
    (let ((db (anvil-memory--db)))
      (anvil-memory-reindex-fts 'unicode61)
      (should (eq 'unicode61 (anvil-memory--current-fts-tokenizer db)))
      (when (anvil-memory--sqlite-supports-trigram-p db)
        (anvil-memory-reindex-fts 'trigram)
        (should (eq 'trigram (anvil-memory--current-fts-tokenizer db)))))))

(ert-deftest anvil-memory-test/trigram-reindex-preserves-body ()
  "Reindex rebuilds the FTS body index; previously-indexed rows stay searchable."
  (skip-unless (anvil-memory-test--supported-p 'reindex-fts))
  (anvil-memory-test--with-env
    (let ((db (anvil-memory--db)))
      (skip-unless (anvil-memory--sqlite-supports-trigram-p db))
      (anvil-memory-test--write
       (expand-file-name "feedback_b.md" root) "---\ntype: feedback\n---\nalpha bravo charlie\n")
      (anvil-memory-scan)
      (anvil-memory-reindex-fts 'trigram)
      (let ((hits (anvil-memory-search "bravo")))
        (should (> (length hits) 0))
        (should (plist-get (car hits) :file))))))

(ert-deftest anvil-memory-test/trigram-reindex-returns-report ()
  "Reindex returns a plist carrying the tokenizer and rebuilt count."
  (skip-unless (anvil-memory-test--supported-p 'reindex-fts))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_c.md" root) "---\ntype: feedback\n---\nseed one\n")
    (anvil-memory-test--write
     (expand-file-name "feedback_d.md" root) "---\ntype: feedback\n---\nseed two\n")
    (anvil-memory-scan)
    (let ((report (anvil-memory-reindex-fts 'unicode61)))
      (should (eq 'unicode61 (plist-get report :tokenizer)))
      (should (= 2 (plist-get report :rebuilt))))))

(ert-deftest anvil-memory-test/trigram-reindex-rejects-invalid ()
  "Explicit invalid tokenizer symbol raises before touching the DB."
  (skip-unless (anvil-memory-test--supported-p 'reindex-fts))
  (anvil-memory-test--with-env
    (anvil-memory-scan)
    (should-error (anvil-memory-reindex-fts 'bogus-tokenizer))))

(ert-deftest anvil-memory-test/trigram-japanese-substring-matches ()
  "Trigram indexing lets a 3-char Japanese substring find a longer body."
  (skip-unless (anvil-memory-test--supported-p 'reindex-fts))
  (anvil-memory-test--with-env
    (let ((db (anvil-memory--db)))
      (skip-unless (anvil-memory--sqlite-supports-trigram-p db))
      (anvil-memory-test--write
       (expand-file-name "project_jp.md" root)
       "---\ntype: project\n---\n藤澤電気管理事務所の月次点検\n")
      (anvil-memory-scan)
      (anvil-memory-reindex-fts 'trigram)
      (let ((hits (anvil-memory-search "澤電気")))
        (should (> (length hits) 0))))))

(ert-deftest anvil-memory-test/trigram-mcp-tool-roundtrip ()
  "`memory-reindex-fts' MCP handler returns an encoded :tokenizer / :rebuilt plist."
  (skip-unless (anvil-memory-test--supported-p 'reindex-fts))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_e.md" root) "---\ntype: feedback\n---\nplist check\n")
    (anvil-memory-scan)
    (let ((result (anvil-memory--tool-reindex-fts "unicode61")))
      ;; The MCP handler goes through `anvil-server-encode-handler' at
      ;; registration, so here we call the underlying plain function and
      ;; assert the plist shape — registration wrapping is covered by the
      ;; existing release-audit marker.
      (should (eq 'unicode61 (plist-get result :tokenizer)))
      (should (integerp (plist-get result :rebuilt))))))


;;;; --- Phase 2b-ii: LLM save-check verdict --------------------------------

(defmacro anvil-memory-test--with-llm-mock (response-plists &rest body)
  "Run BODY with `anvil-orchestrator-submit-and-collect' stubbed.
RESPONSE-PLISTS is an expression evaluating to a list of plists;
each orchestrator call pops one off a shared queue.  A dynamic
variable `mock-calls' is let-bound and records the plists of
forwarded keyword args so tests can assert call counts / contents."
  (declare (indent 1))
  `(let* ((mock-queue (copy-sequence ,response-plists))
          (mock-calls nil))
     (cl-letf (((symbol-function 'anvil-orchestrator-submit-and-collect)
                (lambda (&rest kwargs)
                  (push kwargs mock-calls)
                  (or (pop mock-queue)
                      (list :status 'done :summary "orthogonal" :pending nil)))))
       ,@body)))

(defun anvil-memory-test--seed-duplicate-pair (root)
  "Populate ROOT with two similar feedback memories for save-check."
  (anvil-memory-test--write
   (expand-file-name "feedback_commit.md" root)
   "---\ntype: feedback\n---\nAlways include Co-Authored-By in every commit.\n")
  (anvil-memory-test--write
   (expand-file-name "feedback_commit_rules.md" root)
   "---\ntype: feedback\n---\nAlways include Co-Authored-By tag.\n"))

(ert-deftest anvil-memory-test/llm-verdict-without-flag-unchanged ()
  "`save-check' without :with-llm returns the Phase 1b shape (no verdict)."
  (skip-unless (anvil-memory-test--supported-p 'llm-verdict))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-duplicate-pair root)
    (anvil-memory-scan)
    (let ((hits (anvil-memory-save-check
                 "Co-Authored-By rule"
                 "Always include Co-Authored-By in every commit."
                 3)))
      (should (> (length hits) 0))
      (dolist (h hits)
        (should-not (plist-member h :verdict))
        (should-not (plist-member h :verdict-raw))
        (should-not (plist-member h :verdict-error))))))

(ert-deftest anvil-memory-test/llm-verdict-duplicate-parsed ()
  "LLM response \"duplicate\" is parsed into :verdict 'duplicate."
  (skip-unless (anvil-memory-test--supported-p 'llm-verdict))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-duplicate-pair root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'done :summary "duplicate" :pending nil)
              (list :status 'done :summary "duplicate" :pending nil))
      (let ((hits (anvil-memory-save-check
                   "Co-Authored-By rule"
                   "Always include Co-Authored-By in every commit."
                   3 :with-llm t)))
        (should (> (length hits) 0))
        (dolist (h hits)
          (should (eq 'duplicate (plist-get h :verdict))))))))

(ert-deftest anvil-memory-test/llm-verdict-contradicting-parsed ()
  "LLM response \"contradicting\" maps to 'contradicting."
  (skip-unless (anvil-memory-test--supported-p 'llm-verdict))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-duplicate-pair root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'done :summary "contradicting" :pending nil)
              (list :status 'done :summary "contradicting" :pending nil))
      (let ((hits (anvil-memory-save-check
                   "Co-Authored-By rule"
                   "Never include Co-Authored-By."
                   3 :with-llm t)))
        (should (> (length hits) 0))
        (should (eq 'contradicting (plist-get (car hits) :verdict)))))))

(ert-deftest anvil-memory-test/llm-verdict-orthogonal-parsed ()
  "LLM response \"orthogonal\" maps to 'orthogonal."
  (skip-unless (anvil-memory-test--supported-p 'llm-verdict))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-duplicate-pair root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'done :summary "orthogonal" :pending nil)
              (list :status 'done :summary "orthogonal" :pending nil))
      (let ((hits (anvil-memory-save-check
                   "Co-Authored-By rule" "Commit body 123"
                   3 :with-llm t)))
        (should (> (length hits) 0))
        (dolist (h hits)
          (should (eq 'orthogonal (plist-get h :verdict))))))))

(ert-deftest anvil-memory-test/llm-verdict-case-punctuation-robust ()
  "Verdict parser is case-insensitive and tolerates surrounding punctuation."
  (skip-unless (anvil-memory-test--supported-p 'llm-verdict))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-duplicate-pair root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'done :summary "Verdict: DUPLICATE." :pending nil)
              (list :status 'done :summary "The answer is: CONTRADICTING." :pending nil))
      (let ((hits (anvil-memory-save-check
                   "Co-Authored-By rule"
                   "Always include Co-Authored-By in every commit."
                   3 :with-llm t)))
        (should (memq (plist-get (car hits) :verdict)
                      '(duplicate contradicting orthogonal)))))))

(ert-deftest anvil-memory-test/llm-verdict-unknown-response ()
  "Unparseable LLM response yields :verdict nil + :verdict-raw."
  (skip-unless (anvil-memory-test--supported-p 'llm-verdict))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-duplicate-pair root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'done :summary "maybe perhaps" :pending nil)
              (list :status 'done :summary "something else" :pending nil))
      (let ((hits (anvil-memory-save-check
                   "Co-Authored-By rule"
                   "Always include Co-Authored-By in every commit."
                   3 :with-llm t)))
        (should (> (length hits) 0))
        (dolist (h hits)
          (should (null (plist-get h :verdict)))
          (should (stringp (plist-get h :verdict-raw))))))))

(ert-deftest anvil-memory-test/llm-verdict-failed-status ()
  "Orchestrator :status failed propagates to :verdict-error."
  (skip-unless (anvil-memory-test--supported-p 'llm-verdict))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-duplicate-pair root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'failed :error "boom" :pending nil)
              (list :status 'failed :error "boom" :pending nil))
      (let ((hits (anvil-memory-save-check
                   "Co-Authored-By rule"
                   "Always include Co-Authored-By in every commit."
                   3 :with-llm t)))
        (should (> (length hits) 0))
        (dolist (h hits)
          (should (null (plist-get h :verdict)))
          (should (stringp (plist-get h :verdict-error))))))))

(ert-deftest anvil-memory-test/llm-verdict-pending-timeout ()
  "Orchestrator :pending t collapses into :verdict-error."
  (skip-unless (anvil-memory-test--supported-p 'llm-verdict))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-duplicate-pair root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'running :pending t)
              (list :status 'running :pending t))
      (let ((hits (anvil-memory-save-check
                   "Co-Authored-By rule"
                   "Always include Co-Authored-By in every commit."
                   3 :with-llm t)))
        (should (> (length hits) 0))
        (dolist (h hits)
          (should (null (plist-get h :verdict)))
          (should (stringp (plist-get h :verdict-error))))))))

(ert-deftest anvil-memory-test/llm-verdict-called-per-candidate ()
  "One orchestrator call per candidate returned (N candidates → N calls)."
  (skip-unless (anvil-memory-test--supported-p 'llm-verdict))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-duplicate-pair root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'done :summary "duplicate" :pending nil)
              (list :status 'done :summary "duplicate" :pending nil)
              (list :status 'done :summary "duplicate" :pending nil))
      (let ((hits (anvil-memory-save-check
                   "Co-Authored-By rule"
                   "Always include Co-Authored-By in every commit."
                   5 :with-llm t)))
        (should (= (length hits) (length mock-calls)))))))

(ert-deftest anvil-memory-test/llm-verdict-no-candidates-no-call ()
  "Zero candidates → orchestrator is never called."
  (skip-unless (anvil-memory-test--supported-p 'llm-verdict))
  (anvil-memory-test--with-env
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock nil
      (let ((hits (anvil-memory-save-check "nothing" "matches" 3 :with-llm t)))
        (should (null hits))
        (should (null mock-calls))))))

(ert-deftest anvil-memory-test/llm-verdict-mcp-tool-roundtrip ()
  "MCP `memory-save-check' forwards with_llm/provider/model through the handler."
  (skip-unless (anvil-memory-test--supported-p 'llm-verdict))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-duplicate-pair root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'done :summary "duplicate" :pending nil)
              (list :status 'done :summary "duplicate" :pending nil))
      (let* ((result (anvil-memory--tool-save-check
                      "Co-Authored-By rule"
                      "Always include Co-Authored-By in every commit."
                      "true" "claude" "sonnet-4-6"))
             (cands (plist-get result :candidates)))
        (should (> (length cands) 0))
        (dolist (c cands)
          (should (eq 'duplicate (plist-get c :verdict))))
        ;; provider / model were forwarded to the mock.
        (should (equal "claude"      (plist-get (car mock-calls) :provider)))
        (should (equal "sonnet-4-6"  (plist-get (car mock-calls) :model)))))))


;;;; --- Phase 2b-iii: MDL distillation ------------------------------------

(defun anvil-memory-test--seed-trio (root)
  "Populate ROOT with three feedback memories that share a common rule."
  (anvil-memory-test--write
   (expand-file-name "feedback_commit_one.md" root)
   "---\ntype: feedback\n---\nAlways include Co-Authored-By in every commit.\n")
  (anvil-memory-test--write
   (expand-file-name "feedback_commit_two.md" root)
   "---\ntype: feedback\n---\nAlways attach Co-Authored-By tag.\n")
  (anvil-memory-test--write
   (expand-file-name "feedback_commit_three.md" root)
   "---\ntype: feedback\n---\nCommits need Co-Authored-By trailer.\n"))

(defconst anvil-memory-test--mdl-draft
  "---\nname: commit-co-authored-by rule\ndescription: always include Co-Authored-By\ntype: feedback\n---\nEvery commit authored with Claude must carry a Co-Authored-By trailer.\n"
  "Canned MDL draft returned by the mock orchestrator.")

(ert-deftest anvil-memory-test/mdl-distill-empty-files-errors ()
  "Empty FILES list raises user-error before calling the orchestrator."
  (skip-unless (anvil-memory-test--supported-p 'mdl-distill))
  (anvil-memory-test--with-env
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock nil
      (should-error (anvil-memory-mdl-distill nil))
      (should (null mock-calls)))))

(ert-deftest anvil-memory-test/mdl-distill-unindexed-file-errors ()
  "A path not in memory_meta raises user-error; no orchestrator call."
  (skip-unless (anvil-memory-test--supported-p 'mdl-distill))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-trio root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock nil
      (should-error
       (anvil-memory-mdl-distill
        (list (expand-file-name "feedback_missing.md" root))))
      (should (null mock-calls)))))

(ert-deftest anvil-memory-test/mdl-distill-single-memory-returns-draft ()
  "A one-element FILES list still distils (caller may reuse the loop)."
  (skip-unless (anvil-memory-test--supported-p 'mdl-distill))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-trio root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'done :summary anvil-memory-test--mdl-draft
                    :pending nil))
      (let* ((files (list (expand-file-name "feedback_commit_one.md" root)))
             (res (anvil-memory-mdl-distill files)))
        (should (stringp (plist-get res :draft)))
        (should (equal files (plist-get res :sources)))
        (should (null (plist-get res :error)))
        (should (= 1 (length mock-calls)))))))

(ert-deftest anvil-memory-test/mdl-distill-multiple-memories ()
  "N inputs → one orchestrator call; :sources preserves caller order."
  (skip-unless (anvil-memory-test--supported-p 'mdl-distill))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-trio root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'done :summary anvil-memory-test--mdl-draft
                    :pending nil))
      (let* ((files (list (expand-file-name "feedback_commit_one.md" root)
                          (expand-file-name "feedback_commit_two.md" root)
                          (expand-file-name "feedback_commit_three.md" root)))
             (res (anvil-memory-mdl-distill files)))
        (should (= 1 (length mock-calls)))
        (should (equal files (plist-get res :sources)))
        (should (stringp (plist-get res :draft)))))))

(ert-deftest anvil-memory-test/mdl-distill-prompt-includes-bodies ()
  "Orchestrator prompt carries each source's body as a labelled block."
  (skip-unless (anvil-memory-test--supported-p 'mdl-distill))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-trio root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'done :summary anvil-memory-test--mdl-draft
                    :pending nil))
      (let ((files (list (expand-file-name "feedback_commit_one.md" root)
                         (expand-file-name "feedback_commit_two.md" root))))
        (anvil-memory-mdl-distill files)
        (let ((prompt (plist-get (car mock-calls) :prompt)))
          (should (stringp prompt))
          (should (string-match-p "Co-Authored-By" prompt))
          (should (string-match-p "Co-Authored-By tag" prompt))
          (should (string-match-p "feedback_commit_one" prompt))
          (should (string-match-p "feedback_commit_two" prompt)))))))

(ert-deftest anvil-memory-test/mdl-distill-failed-status-error ()
  "Orchestrator :status failed surfaces as :error, :draft is nil."
  (skip-unless (anvil-memory-test--supported-p 'mdl-distill))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-trio root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'failed :error "boom" :pending nil))
      (let* ((files (list (expand-file-name "feedback_commit_one.md" root)))
             (res (anvil-memory-mdl-distill files)))
        (should (null (plist-get res :draft)))
        (should (stringp (plist-get res :error)))
        (should (equal files (plist-get res :sources)))))))

(ert-deftest anvil-memory-test/mdl-distill-pending-timeout-error ()
  "Orchestrator :pending t surfaces as :error, :draft nil."
  (skip-unless (anvil-memory-test--supported-p 'mdl-distill))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-trio root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'running :pending t))
      (let* ((files (list (expand-file-name "feedback_commit_one.md" root)))
             (res (anvil-memory-mdl-distill files)))
        (should (null (plist-get res :draft)))
        (should (stringp (plist-get res :error)))))))

(ert-deftest anvil-memory-test/mdl-distill-forwards-provider-model ()
  "`:provider' / `:model' reach the orchestrator call."
  (skip-unless (anvil-memory-test--supported-p 'mdl-distill))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-trio root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'done :summary anvil-memory-test--mdl-draft
                    :pending nil))
      (anvil-memory-mdl-distill
       (list (expand-file-name "feedback_commit_one.md" root))
       :provider "codex" :model "gemma4")
      (should (equal "codex"  (plist-get (car mock-calls) :provider)))
      (should (equal "gemma4" (plist-get (car mock-calls) :model))))))

(ert-deftest anvil-memory-test/mdl-distill-does-not-write-disk ()
  "Distillation is read-only: no source / MEMORY.md file is modified."
  (skip-unless (anvil-memory-test--supported-p 'mdl-distill))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-trio root)
    (anvil-memory-scan)
    (let* ((src (expand-file-name "feedback_commit_one.md" root))
           (before (nth 5 (file-attributes src))))
      (anvil-memory-test--with-llm-mock
          (list (list :status 'done :summary anvil-memory-test--mdl-draft
                      :pending nil))
        (anvil-memory-mdl-distill (list src)))
      (let ((after (nth 5 (file-attributes src))))
        (should (equal before after))))))

(ert-deftest anvil-memory-test/mdl-distill-mcp-tool-roundtrip ()
  "MCP `memory-mdl-distill' accepts a colon-separated files string."
  (skip-unless (anvil-memory-test--supported-p 'mdl-distill))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-trio root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'done :summary anvil-memory-test--mdl-draft
                    :pending nil))
      (let* ((f1 (expand-file-name "feedback_commit_one.md" root))
             (f2 (expand-file-name "feedback_commit_two.md" root))
             (joined (concat f1 ":" f2))
             (res (anvil-memory--tool-mdl-distill joined "claude" "sonnet")))
        (should (stringp (plist-get res :draft)))
        (should (equal (list f1 f2) (plist-get res :sources)))
        (should (equal "claude" (plist-get (car mock-calls) :provider)))
        (should (equal "sonnet" (plist-get (car mock-calls) :model)))))))


;;;; --- Phase 3a: HTML export (static viewer) -----------------------------

(defun anvil-memory-test--seed-typed-mix (root)
  "Populate ROOT with one memory per type for HTML export smoke tests."
  (anvil-memory-test--write
   (expand-file-name "user_role.md" root)
   "---\nname: User Role\ndescription: I am a senior electrical engineer.\n---\n")
  (anvil-memory-test--write
   (expand-file-name "feedback_commits.md" root)
   "---\nname: Commit rule\ndescription: Co-Authored-By every time.\n---\n")
  (anvil-memory-test--write
   (expand-file-name "project_anvil.md" root)
   "---\nname: anvil\ndescription: anvil.el development.\n---\n")
  (anvil-memory-test--write
   (expand-file-name "reference_linear.md" root)
   "---\nname: Linear\ndescription: Bug tracker entry point.\n---\n"))

(ert-deftest anvil-memory-test/export-html-empty-root-returns-valid-html ()
  "Zero memories → valid HTML stub with no row."
  (skip-unless (anvil-memory-test--supported-p 'export-html))
  (anvil-memory-test--with-env
    (anvil-memory-scan)
    (let ((html (anvil-memory-export-html root)))
      (should (stringp html))
      (should (string-match-p "<!DOCTYPE html>" html))
      (should (string-match-p "</html>" html))
      (should-not (string-match-p "feedback_" html)))))

(ert-deftest anvil-memory-test/export-html-includes-memory-files ()
  "Each indexed memory's basename appears in the emitted HTML."
  (skip-unless (anvil-memory-test--supported-p 'export-html))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-typed-mix root)
    (anvil-memory-scan)
    (let ((html (anvil-memory-export-html root)))
      (should (string-match-p "user_role\\.md" html))
      (should (string-match-p "feedback_commits\\.md" html))
      (should (string-match-p "project_anvil\\.md" html))
      (should (string-match-p "reference_linear\\.md" html)))))

(ert-deftest anvil-memory-test/export-html-groups-by-type ()
  "Each memory type gets its own labelled section."
  (skip-unless (anvil-memory-test--supported-p 'export-html))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-typed-mix root)
    (anvil-memory-scan)
    (let ((html (anvil-memory-export-html root)))
      (should (string-match-p "data-type=\"user\"" html))
      (should (string-match-p "data-type=\"feedback\"" html))
      (should (string-match-p "data-type=\"project\"" html))
      (should (string-match-p "data-type=\"reference\"" html)))))

(ert-deftest anvil-memory-test/export-html-has-decay-attribute ()
  "Every memory row carries a numeric data-decay attribute."
  (skip-unless (anvil-memory-test--supported-p 'export-html))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-typed-mix root)
    (anvil-memory-scan)
    (let ((html (anvil-memory-export-html root)))
      (should (string-match-p "data-decay=\"[0-9.]+\"" html)))))

(ert-deftest anvil-memory-test/export-html-orders-by-decay-desc-within-type ()
  "Within one type, higher decay-score rows precede lower ones."
  (skip-unless (anvil-memory-test--supported-p 'export-html))
  (anvil-memory-test--with-env
    (let* ((low (expand-file-name "feedback_low.md" root))
           (high (expand-file-name "feedback_high.md" root)))
      (anvil-memory-test--write low "---\nname: low\n---\n")
      (anvil-memory-test--write high "---\nname: high\n---\n")
      (anvil-memory-scan)
      ;; Bump access on `high' so decay-score surpasses `low'.
      (dotimes (_ 10) (anvil-memory-access high))
      (let* ((html (anvil-memory-export-html root))
             (hi-pos (string-match "feedback_high\\.md" html))
             (lo-pos (string-match "feedback_low\\.md" html)))
        (should hi-pos)
        (should lo-pos)
        (should (< hi-pos lo-pos))))))

(ert-deftest anvil-memory-test/export-html-escapes-entities ()
  "Frontmatter containing < / > / & is HTML-escaped in the rendered page."
  (skip-unless (anvil-memory-test--supported-p 'export-html))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_escape.md" root)
     "---\nname: A<B & C>D\ndescription: one & two\n---\n")
    (anvil-memory-scan)
    (let ((html (anvil-memory-export-html root)))
      (should (string-match-p "A&lt;B &amp; C&gt;D" html))
      (should (string-match-p "one &amp; two" html))
      ;; Raw markup must not leak.
      (should-not (string-match-p "A<B &" html)))))

(ert-deftest anvil-memory-test/export-html-writes-file-when-out-path ()
  "OUT-PATH writes the HTML to disk and the returned string equals the file."
  (skip-unless (anvil-memory-test--supported-p 'export-html))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-typed-mix root)
    (anvil-memory-scan)
    (let* ((out (expand-file-name "out.html" root))
           (html (anvil-memory-export-html root :out-path out)))
      (should (file-exists-p out))
      (should (equal html (with-temp-buffer
                            (insert-file-contents out)
                            (buffer-string)))))))

(ert-deftest anvil-memory-test/export-html-is-self-contained ()
  "Output references no external CSS / JS — opens offline via file://."
  (skip-unless (anvil-memory-test--supported-p 'export-html))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-typed-mix root)
    (anvil-memory-scan)
    (let ((html (anvil-memory-export-html root)))
      (should-not (string-match-p "<link " html))
      (should-not (string-match-p "<script[^>]+src=" html)))))

(ert-deftest anvil-memory-test/export-html-honours-title ()
  "TITLE replaces the default <title> text."
  (skip-unless (anvil-memory-test--supported-p 'export-html))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-typed-mix root)
    (anvil-memory-scan)
    (let ((html (anvil-memory-export-html root :title "custom-title-x")))
      (should (string-match-p "<title>custom-title-x</title>" html)))))

(ert-deftest anvil-memory-test/export-html-mcp-tool-roundtrip ()
  "MCP `memory-export-html' returns :html + :written (nil without out-path)."
  (skip-unless (anvil-memory-test--supported-p 'export-html))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-typed-mix root)
    (anvil-memory-scan)
    (let* ((res1 (anvil-memory--tool-export-html root nil))
           (out (expand-file-name "out.html" root))
           (res2 (anvil-memory--tool-export-html root out)))
      (should (stringp (plist-get res1 :html)))
      (should (null (plist-get res1 :written)))
      (should (stringp (plist-get res2 :html)))
      (should (equal out (plist-get res2 :written)))
      (should (file-exists-p out)))))


;;;; --- Phase 3b: live TCP server ----------------------------------------

(defun anvil-memory-test--http-get (port path)
  "Open a TCP client to 127.0.0.1:PORT, GET PATH, return a parsed plist.
Plist keys: :status (int), :headers (alist, lower-cased names), :body (str).
Blocks up to 3s then times out returning :status -1."
  (let* ((buf "")
         (done nil)
         (proc (make-network-process
                :name "anvil-memory-http-test-client"
                :host "127.0.0.1"
                :service port
                :family 'ipv4
                :coding 'binary
                :noquery t
                :filter (lambda (_p s) (setq buf (concat buf s)))
                :sentinel (lambda (p _e)
                            (when (memq (process-status p)
                                        '(closed failed exit signal))
                              (setq done t))))))
    (unwind-protect
        (progn
          (process-send-string
           proc (format "GET %s HTTP/1.0\r\nHost: 127.0.0.1\r\n\r\n" path))
          (with-timeout (3 nil)
            (while (not done)
              (accept-process-output proc 0.1))))
      (when (process-live-p proc) (delete-process proc)))
    (if (string-match "\\`HTTP/[0-9.]+ \\([0-9]+\\) " buf)
        (let* ((status (string-to-number (match-string 1 buf)))
               (split (string-match "\r\n\r\n" buf))
               (head (substring buf 0 (or split (length buf))))
               (body (if split (substring buf (+ split 4)) ""))
               (headers
                (let (acc)
                  (dolist (line (cdr (split-string head "\r\n" t)))
                    (when (string-match "\\([^:]+\\):\\s-*\\(.*\\)" line)
                      (push (cons (downcase (match-string 1 line))
                                  (match-string 2 line))
                            acc)))
                  (nreverse acc))))
          (list :status status :headers headers :body body))
      (list :status -1 :headers nil :body buf))))

(defun anvil-memory-test--http-raw (port request-bytes)
  "Open TCP client to 127.0.0.1:PORT, write REQUEST-BYTES, return full reply."
  (let* ((buf "")
         (done nil)
         (proc (make-network-process
                :name "anvil-memory-http-test-raw"
                :host "127.0.0.1"
                :service port
                :family 'ipv4
                :coding 'binary
                :noquery t
                :filter (lambda (_p s) (setq buf (concat buf s)))
                :sentinel (lambda (p _e)
                            (when (memq (process-status p)
                                        '(closed failed exit signal))
                              (setq done t))))))
    (unwind-protect
        (progn
          (process-send-string proc request-bytes)
          (with-timeout (3 nil)
            (while (not done)
              (accept-process-output proc 0.1))))
      (when (process-live-p proc) (delete-process proc)))
    buf))

(defmacro anvil-memory-test--with-server (root-var &rest body)
  "Run BODY with a memory server bound to an ephemeral port.
ROOT-VAR is the temp memory root.  Binds `port' (int) and `info'
(the start plist)."
  (declare (indent 1))
  `(let* ((info (anvil-memory-serve-start :host "127.0.0.1" :port t
                                          :root ,root-var))
          (port (plist-get info :port)))
     (unwind-protect (progn ,@body)
       (anvil-memory-serve-stop))))

(ert-deftest anvil-memory-test/serve-start-returns-port-and-process ()
  "Start returns :port / :host / :process; the process is live."
  (skip-unless (anvil-memory-test--supported-p 'serve))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-typed-mix root)
    (anvil-memory-scan)
    (anvil-memory-test--with-server root
      (should (integerp port))
      (should (> port 0))
      (should (equal "127.0.0.1" (plist-get info :host)))
      (should (process-live-p (plist-get info :process))))))

(ert-deftest anvil-memory-test/serve-double-start-errors ()
  "Starting while already running raises `user-error'."
  (skip-unless (anvil-memory-test--supported-p 'serve))
  (anvil-memory-test--with-env
    (anvil-memory-scan)
    (anvil-memory-test--with-server root
      (should-error (anvil-memory-serve-start :host "127.0.0.1" :port t
                                              :root root)))))

(ert-deftest anvil-memory-test/serve-stop-idempotent ()
  "Stop when no server is running returns nil without signalling."
  (skip-unless (anvil-memory-test--supported-p 'serve))
  (anvil-memory-test--with-env
    (should (null (anvil-memory-serve-stop)))
    (should (null (anvil-memory-serve-stop)))))

(ert-deftest anvil-memory-test/serve-get-root-returns-html ()
  "GET / answers 200 with Content-Type: text/html."
  (skip-unless (anvil-memory-test--supported-p 'serve))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-typed-mix root)
    (anvil-memory-scan)
    (anvil-memory-test--with-server root
      (let* ((res (anvil-memory-test--http-get port "/"))
             (ct (assoc "content-type" (plist-get res :headers))))
        (should (= 200 (plist-get res :status)))
        (should ct)
        (should (string-match-p "text/html" (cdr ct)))
        (should (string-match-p "<!DOCTYPE html>" (plist-get res :body)))))))

(ert-deftest anvil-memory-test/serve-get-root-contains-memories ()
  "The served HTML lists every indexed memory's basename."
  (skip-unless (anvil-memory-test--supported-p 'serve))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-typed-mix root)
    (anvil-memory-scan)
    (anvil-memory-test--with-server root
      (let ((body (plist-get (anvil-memory-test--http-get port "/") :body)))
        (should (string-match-p "user_role\\.md" body))
        (should (string-match-p "feedback_commits\\.md" body))
        (should (string-match-p "project_anvil\\.md" body))))))

(ert-deftest anvil-memory-test/serve-get-api-list-returns-json ()
  "GET /api/list returns 200 + application/json + parseable array."
  (skip-unless (anvil-memory-test--supported-p 'serve))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-typed-mix root)
    (anvil-memory-scan)
    (anvil-memory-test--with-server root
      (let* ((res (anvil-memory-test--http-get port "/api/list"))
             (ct (assoc "content-type" (plist-get res :headers))))
        (should (= 200 (plist-get res :status)))
        (should (string-match-p "application/json" (cdr ct)))
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (parsed (json-read-from-string (plist-get res :body))))
          (should (listp parsed)))))))

(ert-deftest anvil-memory-test/serve-get-api-heatmap-returns-json ()
  "GET /api/decay-heatmap returns 200 + per-type aggregate JSON."
  (skip-unless (anvil-memory-test--supported-p 'serve))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-typed-mix root)
    (anvil-memory-scan)
    (anvil-memory-test--with-server root
      (let* ((res (anvil-memory-test--http-get port "/api/decay-heatmap"))
             (json-object-type 'alist)
             (json-array-type 'list)
             (parsed (and (= 200 (plist-get res :status))
                          (json-read-from-string (plist-get res :body)))))
        (should parsed)
        (should (listp parsed))
        (dolist (entry parsed)
          (should (assq 'type entry))
          (should (assq 'count entry))
          (should (assq 'mean_decay entry)))))))

(ert-deftest anvil-memory-test/serve-unknown-path-404 ()
  "Unknown paths answer 404."
  (skip-unless (anvil-memory-test--supported-p 'serve))
  (anvil-memory-test--with-env
    (anvil-memory-scan)
    (anvil-memory-test--with-server root
      (let ((res (anvil-memory-test--http-get port "/does-not-exist")))
        (should (= 404 (plist-get res :status)))))))

(ert-deftest anvil-memory-test/serve-non-get-405 ()
  "Non-GET methods answer 405 Method Not Allowed."
  (skip-unless (anvil-memory-test--supported-p 'serve))
  (anvil-memory-test--with-env
    (anvil-memory-scan)
    (anvil-memory-test--with-server root
      (let ((raw (anvil-memory-test--http-raw
                  port "POST / HTTP/1.0\r\nHost: x\r\n\r\n")))
        (should (string-match-p "HTTP/1\\.[01] 405 " raw))))))

(ert-deftest anvil-memory-test/serve-sequential-requests-ok ()
  "Three back-to-back GETs each succeed; the server survives all."
  (skip-unless (anvil-memory-test--supported-p 'serve))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-typed-mix root)
    (anvil-memory-scan)
    (anvil-memory-test--with-server root
      (dotimes (_ 3)
        (let ((res (anvil-memory-test--http-get port "/")))
          (should (= 200 (plist-get res :status))))))))

(ert-deftest anvil-memory-test/serve-mcp-tool-lifecycle ()
  "MCP `memory-serve-start' / `memory-serve-stop' round-trip."
  (skip-unless (anvil-memory-test--supported-p 'serve))
  (anvil-memory-test--with-env
    (anvil-memory-scan)
    (let* ((started (anvil-memory--tool-serve-start "127.0.0.1" "0" root))
           (port (plist-get started :port)))
      (unwind-protect
          (progn
            (should (integerp port))
            (should (plist-get started :running))
            (let ((res (anvil-memory-test--http-get port "/")))
              (should (= 200 (plist-get res :status)))))
        (let ((stopped (anvil-memory--tool-serve-stop)))
          (should (plist-member stopped :stopped)))))))


;;;; --- Phase 3c: contradiction store + graph ----------------------------

(defun anvil-memory-test--seed-contradicting-pair (root)
  "Populate ROOT with two memories whose bodies overlap (candidate pair)."
  (anvil-memory-test--write
   (expand-file-name "feedback_commit_always.md" root)
   "---\nname: commit rule\n---\nAlways include Co-Authored-By in every commit message trailer.\n")
  (anvil-memory-test--write
   (expand-file-name "feedback_commit_never.md" root)
   "---\nname: commit rule\n---\nNever include Co-Authored-By in any commit message trailer.\n"))

(ert-deftest anvil-memory-test/scan-contradictions-keyword-mode-stores-candidates ()
  "Keyword mode stores every jaccard-overlapping pair as verdict=candidate."
  (skip-unless (anvil-memory-test--supported-p 'contradictions))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-contradicting-pair root)
    (anvil-memory-scan)
    (let ((res (anvil-memory-scan-contradictions :threshold 0.3 :mode 'keyword)))
      (should (plist-member res :scanned))
      (should (>= (plist-get res :stored) 1))
      (let ((rows (anvil-memory-contradictions)))
        (should (>= (length rows) 1))
        (dolist (r rows)
          (should (equal "candidate" (plist-get r :verdict)))
          (should (numberp (plist-get r :score))))))))

(ert-deftest anvil-memory-test/scan-contradictions-respects-threshold ()
  "Pairs whose jaccard similarity < THRESHOLD are not stored."
  (skip-unless (anvil-memory-test--supported-p 'contradictions))
  (anvil-memory-test--with-env
    (anvil-memory-test--write
     (expand-file-name "feedback_alpha.md" root)
     "---\nname: alpha\n---\nalpha bravo charlie delta echo foxtrot golf\n")
    (anvil-memory-test--write
     (expand-file-name "feedback_zulu.md" root)
     "---\nname: zulu\n---\nzulu yankee xray whiskey victor uniform tango\n")
    (anvil-memory-scan)
    ;; The two bodies are fully disjoint; frontmatter `name:' contributes
    ;; the lone shared token "name" (jaccard ≈ 0.06).  A threshold of
    ;; 0.3 is comfortably above that noise floor so nothing should store.
    (let ((res (anvil-memory-scan-contradictions :threshold 0.3 :mode 'keyword)))
      (should (= 0 (plist-get res :stored))))))

(ert-deftest anvil-memory-test/scan-contradictions-llm-mode-stores-contradicting ()
  "LLM mode verifies candidates; only pairs the model flags as contradicting stay."
  (skip-unless (anvil-memory-test--supported-p 'contradictions))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-contradicting-pair root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        ;; Each candidate pair triggers 1 mock call.
        (list (list :status 'done :summary "contradicting" :pending nil)
              (list :status 'done :summary "contradicting" :pending nil))
      (let ((res (anvil-memory-scan-contradictions
                  :threshold 0.3 :mode 'llm)))
        (should (>= (plist-get res :stored) 1))
        (dolist (r (anvil-memory-contradictions))
          (should (equal "contradicting" (plist-get r :verdict))))))))

(ert-deftest anvil-memory-test/scan-contradictions-llm-skips-non-contradicting ()
  "LLM mode drops pairs the model labels `orthogonal' / `duplicate'."
  (skip-unless (anvil-memory-test--supported-p 'contradictions))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-contradicting-pair root)
    (anvil-memory-scan)
    (anvil-memory-test--with-llm-mock
        (list (list :status 'done :summary "orthogonal" :pending nil)
              (list :status 'done :summary "orthogonal" :pending nil))
      (let ((res (anvil-memory-scan-contradictions
                  :threshold 0.3 :mode 'llm)))
        (should (= 0 (plist-get res :stored)))
        (should (null (anvil-memory-contradictions)))))))

(ert-deftest anvil-memory-test/scan-contradictions-empty-index-noop ()
  "Zero memories → scan is a no-op; read returns nil."
  (skip-unless (anvil-memory-test--supported-p 'contradictions))
  (anvil-memory-test--with-env
    (anvil-memory-scan)
    (let ((res (anvil-memory-scan-contradictions :mode 'keyword)))
      (should (= 0 (plist-get res :stored))))
    (should (null (anvil-memory-contradictions)))))

(ert-deftest anvil-memory-test/scan-contradictions-rescan-upserts ()
  "A second scan with the same inputs replaces (not duplicates) stored rows."
  (skip-unless (anvil-memory-test--supported-p 'contradictions))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-contradicting-pair root)
    (anvil-memory-scan)
    (anvil-memory-scan-contradictions :threshold 0.3 :mode 'keyword)
    (let ((first (length (anvil-memory-contradictions))))
      (anvil-memory-scan-contradictions :threshold 0.3 :mode 'keyword)
      (should (= first (length (anvil-memory-contradictions)))))))

(ert-deftest anvil-memory-test/contradictions-read-empty-returns-nil ()
  "Read API on a fresh index returns nil, not an error."
  (skip-unless (anvil-memory-test--supported-p 'contradictions))
  (anvil-memory-test--with-env
    (anvil-memory-scan)
    (should (null (anvil-memory-contradictions)))))

(ert-deftest anvil-memory-test/api-contradictions-returns-json ()
  "GET /api/contradictions returns 200 + application/json array."
  (skip-unless (anvil-memory-test--supported-p 'contradictions))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-contradicting-pair root)
    (anvil-memory-scan)
    (anvil-memory-scan-contradictions :threshold 0.3 :mode 'keyword)
    (anvil-memory-test--with-server root
      (let* ((res (anvil-memory-test--http-get port "/api/contradictions"))
             (ct (assoc "content-type" (plist-get res :headers))))
        (should (= 200 (plist-get res :status)))
        (should (string-match-p "application/json" (cdr ct)))
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (parsed (json-read-from-string (plist-get res :body))))
          (should (listp parsed))
          (should (> (length parsed) 0))
          (dolist (entry parsed)
            (should (assq 'file_a entry))
            (should (assq 'file_b entry))
            (should (assq 'verdict entry))))))))

(ert-deftest anvil-memory-test/serve-html-has-contradictions-section-when-present ()
  "When contradictions are stored the viewer HTML lists them."
  (skip-unless (anvil-memory-test--supported-p 'contradictions))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-contradicting-pair root)
    (anvil-memory-scan)
    (anvil-memory-scan-contradictions :threshold 0.3 :mode 'keyword)
    (let ((html (anvil-memory-export-html root)))
      (should (string-match-p "data-section=\"contradictions\"" html))
      (should (string-match-p "feedback_commit_always\\.md" html))
      (should (string-match-p "feedback_commit_never\\.md" html)))))

(ert-deftest anvil-memory-test/serve-html-omits-contradictions-section-when-empty ()
  "With no stored contradictions the section header is absent."
  (skip-unless (anvil-memory-test--supported-p 'contradictions))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-typed-mix root)
    (anvil-memory-scan)
    (let ((html (anvil-memory-export-html root)))
      (should-not (string-match-p "data-section=\"contradictions\"" html)))))

(ert-deftest anvil-memory-test/serve-html-has-type-filter-controls ()
  "The viewer HTML now exposes CSS-filterable per-type checkboxes."
  (skip-unless (anvil-memory-test--supported-p 'contradictions))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-typed-mix root)
    (anvil-memory-scan)
    (let ((html (anvil-memory-export-html root)))
      (should (string-match-p "class=\"facet-filter\"" html))
      ;; Inline script is OK, external <script src> is not (Phase 3a rule).
      (should (string-match-p "<script>" html))
      (should-not (string-match-p "<script[^>]+src=" html)))))

(ert-deftest anvil-memory-test/mcp-scan-contradictions-roundtrip ()
  "MCP `memory-scan-contradictions' accepts string args and returns a plist."
  (skip-unless (anvil-memory-test--supported-p 'contradictions))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-contradicting-pair root)
    (anvil-memory-scan)
    (let ((res (anvil-memory--tool-scan-contradictions "0.3" "keyword" nil nil)))
      (should (plist-member res :stored))
      (should (plist-member res :scanned))
      (should (equal "keyword" (plist-get res :mode))))))

(ert-deftest anvil-memory-test/mcp-contradictions-roundtrip ()
  "MCP `memory-contradictions' returns rows as :rows alist entries."
  (skip-unless (anvil-memory-test--supported-p 'contradictions))
  (anvil-memory-test--with-env
    (anvil-memory-test--seed-contradicting-pair root)
    (anvil-memory-scan)
    (anvil-memory-scan-contradictions :threshold 0.3 :mode 'keyword)
    (let* ((res (anvil-memory--tool-contradictions))
           (rows (plist-get res :rows)))
      (should (listp rows))
      (should (> (length rows) 0))
      (dolist (r rows)
        (should (plist-member r :file-a))
        (should (plist-member r :file-b))
        (should (plist-member r :verdict))))))


(provide 'anvil-memory-test)

;;; anvil-memory-test.el ends here
