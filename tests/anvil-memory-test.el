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


(provide 'anvil-memory-test)

;;; anvil-memory-test.el ends here
