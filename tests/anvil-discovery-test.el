;;; anvil-discovery-test.el --- Tests for anvil-discovery Doc 34 Phase A + C -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the Doc 34 Phase A intent-based tool discovery:
;; metadata propagation through anvil-server-register-tool, intent /
;; layer / query / stability filtering, and deterministic sort order.
;;
;; Phase C coverage: usage-counter dispatch hook, report shape,
;; never-called / unused-since-days classification.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-server)
(require 'anvil-state)
(require 'anvil-discovery)


;;;; --- fixture ------------------------------------------------------------

(defmacro anvil-discovery-test--with-registry (&rest body)
  "Run BODY against a fresh in-memory anvil-server tools table.
Registers a controlled set of fixture tools spanning multiple
intents / layers / stabilities, plus one tool with no metadata so
default-value fallbacks are exercised."
  (declare (indent 0))
  `(let ((anvil-server--tools (make-hash-table :test #'equal))
         (anvil-server-tool-filter-function nil))
     (unwind-protect
         (progn
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-file-batch"
            :description "batch edits in a single file"
            :read-only nil
            :intent '(file-edit batch)
            :layer 'core
            :stability 'stable)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-org-read"
            :description "read org section by path"
            :read-only t
            :intent '(org-read)
            :layer 'core
            :stability 'stable)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-http-fetch"
            :description "fetch URL with cache"
            :read-only t
            :intent '(http)
            :layer 'io
            :stability 'stable)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-bench"
            :description "benchmark runner"
            :read-only t
            :intent '(bench)
            :layer 'dev
            :stability 'stable)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-experimental"
            :description "wip handler"
            :read-only nil
            :intent '(file-edit)
            :layer 'core
            :stability 'experimental)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-deprecated"
            :description "legacy"
            :read-only t
            :intent '(file-edit)
            :layer 'core
            :stability 'deprecated)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-untagged"
            :description "no discovery metadata")
           ,@body))))

(defun anvil-discovery-test--ids (result)
  "Extract :id list from a handler RESULT plist."
  (mapcar (lambda (tool) (plist-get tool :id))
          (plist-get result :tools)))


;;;; --- metadata propagation ----------------------------------------------

(ert-deftest anvil-discovery-test-metadata-stored ()
  "register-tool passes :intent / :layer / :stability through to the
tool plist kept in `anvil-server--tools'."
  (anvil-discovery-test--with-registry
    (let* ((table (gethash "default" anvil-server--tools))
           (tool  (gethash "stub-file-batch" table)))
      (should (equal '(file-edit batch) (plist-get tool :intent)))
      (should (eq 'core (plist-get tool :layer)))
      (should (eq 'stable (plist-get tool :stability))))))

(ert-deftest anvil-discovery-test-metadata-defaults-applied ()
  "Tools registered without discovery keys resolve to default values
via the discovery accessors."
  (anvil-discovery-test--with-registry
    (let* ((table (gethash "default" anvil-server--tools))
           (tool  (gethash "stub-untagged" table)))
      (should (equal '(general)
                     (anvil-discovery--tool-intent tool)))
      (should (eq 'core
                  (anvil-discovery--tool-layer tool)))
      (should (eq 'stable
                  (anvil-discovery--tool-stability tool))))))


;;;; --- arg parsing --------------------------------------------------------

(ert-deftest anvil-discovery-test-parse-intent-csv ()
  (should (equal '(file-edit org-read)
                 (anvil-discovery--parse-intent-arg "file-edit, org-read"))))

(ert-deftest anvil-discovery-test-parse-intent-empty-is-nil ()
  (should (null (anvil-discovery--parse-intent-arg "")))
  (should (null (anvil-discovery--parse-intent-arg nil))))

(ert-deftest anvil-discovery-test-parse-layer ()
  (should (eq 'core (anvil-discovery--parse-layer-arg "core")))
  (should (null (anvil-discovery--parse-layer-arg ""))))


;;;; --- intent filter ------------------------------------------------------

(ert-deftest anvil-discovery-test-intent-filter-hits ()
  (anvil-discovery-test--with-registry
    (let ((ids (anvil-discovery-test--ids
                (anvil-discovery-tools-by-intent
                 "" "file-edit" "" ""))))
      (should (member "stub-file-batch" ids))
      (should-not (member "stub-org-read" ids)))))

(ert-deftest anvil-discovery-test-intent-filter-csv ()
  (anvil-discovery-test--with-registry
    (let ((ids (anvil-discovery-test--ids
                (anvil-discovery-tools-by-intent
                 "" "file-edit,org-read" "" ""))))
      (should (member "stub-file-batch" ids))
      (should (member "stub-org-read" ids))
      (should-not (member "stub-http-fetch" ids)))))

(ert-deftest anvil-discovery-test-no-filter-returns-all-stable ()
  "With no filters every non-deprecated, non-experimental tool is
returned, including the untagged fixture (default intent general)."
  (anvil-discovery-test--with-registry
    (let ((ids (anvil-discovery-test--ids
                (anvil-discovery-tools-by-intent "" "" "" ""))))
      (should (member "stub-untagged" ids))
      (should (member "stub-file-batch" ids))
      (should-not (member "stub-deprecated" ids))
      (should-not (member "stub-experimental" ids)))))


;;;; --- layer filter -------------------------------------------------------

(ert-deftest anvil-discovery-test-layer-filter-io ()
  (anvil-discovery-test--with-registry
    (let ((ids (anvil-discovery-test--ids
                (anvil-discovery-tools-by-intent "" "" "io" ""))))
      (should (equal '("stub-http-fetch") ids)))))

(ert-deftest anvil-discovery-test-layer-filter-dev ()
  (anvil-discovery-test--with-registry
    (let ((ids (anvil-discovery-test--ids
                (anvil-discovery-tools-by-intent "" "" "dev" ""))))
      (should (equal '("stub-bench") ids)))))


;;;; --- query regex filter -------------------------------------------------

(ert-deftest anvil-discovery-test-query-matches-id ()
  (anvil-discovery-test--with-registry
    (let ((ids (anvil-discovery-test--ids
                (anvil-discovery-tools-by-intent "file-batch" "" "" ""))))
      (should (equal '("stub-file-batch") ids)))))

(ert-deftest anvil-discovery-test-query-matches-description ()
  (anvil-discovery-test--with-registry
    (let ((ids (anvil-discovery-test--ids
                (anvil-discovery-tools-by-intent "benchmark" "" "" ""))))
      (should (equal '("stub-bench") ids)))))

(ert-deftest anvil-discovery-test-query-invalid-regex-errors ()
  (anvil-discovery-test--with-registry
    (should-error (anvil-discovery-tools-by-intent "[unclosed" "" "" "")
                  :type 'anvil-server-tool-error)))


;;;; --- stability gating ---------------------------------------------------

(ert-deftest anvil-discovery-test-deprecated-always-hidden ()
  "Deprecated tools are excluded even when include-experimental=true."
  (anvil-discovery-test--with-registry
    (let ((ids (anvil-discovery-test--ids
                (anvil-discovery-tools-by-intent "" "" "" "t"))))
      (should-not (member "stub-deprecated" ids)))))

(ert-deftest anvil-discovery-test-experimental-requires-optin ()
  (anvil-discovery-test--with-registry
    (let ((default-ids (anvil-discovery-test--ids
                        (anvil-discovery-tools-by-intent "" "" "" "")))
          (optin-ids   (anvil-discovery-test--ids
                        (anvil-discovery-tools-by-intent "" "" "" "true"))))
      (should-not (member "stub-experimental" default-ids))
      (should (member "stub-experimental" optin-ids)))))


;;;; --- sort order ---------------------------------------------------------

(ert-deftest anvil-discovery-test-sort-layer-before-id ()
  "Core-layer tools come before io before dev regardless of id order."
  (anvil-discovery-test--with-registry
    (let* ((ids (anvil-discovery-test--ids
                 (anvil-discovery-tools-by-intent "" "" "" "")))
           (layers (mapcar
                    (lambda (id)
                      (cond
                       ((member id '("stub-file-batch" "stub-org-read"
                                     "stub-untagged"))
                        'core)
                       ((equal id "stub-http-fetch") 'io)
                       ((equal id "stub-bench") 'dev)))
                    ids))
           (ranks (mapcar #'anvil-discovery--layer-rank layers)))
      ;; Ranks must be non-decreasing.
      (should (equal ranks (sort (copy-sequence ranks) #'<=))))))

(ert-deftest anvil-discovery-test-sort-intent-overlap-score ()
  "When layers tie, higher intent overlap wins."
  (anvil-discovery-test--with-registry
    ;; stub-file-batch has two intents (file-edit batch); the untagged
    ;; fixture matches none.  Asking for file-edit,batch gives the
    ;; former a higher score.
    (let ((ids (anvil-discovery-test--ids
                (anvil-discovery-tools-by-intent
                 "" "file-edit,batch" "core" ""))))
      (should (member "stub-file-batch" ids))
      (should (equal "stub-file-batch" (car ids))))))


;;;; --- response shape -----------------------------------------------------

(ert-deftest anvil-discovery-test-result-count-matches-tools ()
  (anvil-discovery-test--with-registry
    (let* ((res (anvil-discovery-tools-by-intent "" "" "" ""))
           (tools (plist-get res :tools)))
      (should (= (plist-get res :count) (length tools))))))

(ert-deftest anvil-discovery-test-result-strips-internal-keys ()
  (anvil-discovery-test--with-registry
    (let* ((res (anvil-discovery-tools-by-intent "" "" "" ""))
           (tool (car (plist-get res :tools))))
      (should-not (plist-member tool :_layer-rank))
      (should-not (plist-member tool :_intent-score))
      (should (plist-get tool :id))
      (should (plist-get tool :layer))
      (should (plist-get tool :intent))
      (should (plist-get tool :stability)))))


;;;; --- Phase C: usage counter --------------------------------------------

(defmacro anvil-discovery-test--with-usage (&rest body)
  "Run BODY with a fresh tool registry AND a fresh anvil-state DB so
usage-counter tests are hermetic."
  (declare (indent 0))
  `(let ((anvil-server--tools (make-hash-table :test #'equal))
         (anvil-server-tool-filter-function nil)
         (anvil-server-tool-dispatch-hook nil)
         (anvil-state-db-path (make-temp-file
                               "anvil-discovery-usage-" nil ".db"))
         (anvil-state--db nil))
     (unwind-protect
         (progn
           (anvil-state-enable)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-a"
            :description "a" :intent '(file-edit) :layer 'core)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-b"
            :description "b" :intent '(file-read) :layer 'core)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-c"
            :description "c" :intent '(http) :layer 'io)
           ,@body)
       (anvil-discovery-usage-clear)
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path)))))

(ert-deftest anvil-discovery-test-record-call-increments-counter ()
  "Each record-call bumps :count and refreshes :last-called."
  (anvil-discovery-test--with-usage
    (anvil-discovery--record-call "stub-a" "default")
    (anvil-discovery--record-call "stub-a" "default")
    (anvil-discovery--record-call "stub-a" "default")
    (let ((entry (anvil-discovery--usage-entry "stub-a")))
      (should (= 3 (plist-get entry :count)))
      (should (numberp (plist-get entry :last-called)))
      (should (numberp (plist-get entry :first-seen)))
      (should (equal "default" (plist-get entry :server-id))))))

(ert-deftest anvil-discovery-test-record-call-no-state-silent ()
  "Without anvil-state loaded the hook is a silent no-op."
  (let ((anvil-server--tools (make-hash-table :test #'equal))
        (anvil-server-tool-dispatch-hook nil))
    ;; Simulate `anvil-state' feature missing: shadow `featurep' so the
    ;; record-call helper takes the no-state branch.
    (cl-letf (((symbol-function 'featurep)
               (lambda (f &rest _) (not (eq f 'anvil-state)))))
      (should-not (condition-case err
                      (progn
                        (anvil-discovery--record-call "missing" "default")
                        nil)
                    (error err))))))

(ert-deftest anvil-discovery-test-parse-days-arg ()
  (should (= 14 (anvil-discovery--parse-days-arg nil)))
  (should (= 14 (anvil-discovery--parse-days-arg "")))
  (should (= 7  (anvil-discovery--parse-days-arg "7")))
  (should (= 14 (anvil-discovery--parse-days-arg "-3")))
  (should (= 30 (anvil-discovery--parse-days-arg 30))))

(ert-deftest anvil-discovery-test-usage-report-classifies-tools ()
  "After seeding one recent + one stale + leaving one untouched,
the report returns each in the correct bucket."
  (anvil-discovery-test--with-usage
    (let ((now (truncate (float-time))))
      ;; stub-a called now (recent)
      (anvil-state-set
       "stub-a"
       (list :count 5 :last-called now :first-seen now
             :server-id "default")
       :ns anvil-discovery--usage-ns)
      ;; stub-b called 30 days ago (stale against 14-day threshold)
      (anvil-state-set
       "stub-b"
       (list :count 1
             :last-called (- now (* 30 86400))
             :first-seen  (- now (* 30 86400))
             :server-id "default")
       :ns anvil-discovery--usage-ns))
    (let ((report (anvil-discovery-usage-report "14")))
      (should (= 14 (plist-get report :days-threshold)))
      (should (= 3 (plist-get report :total-registered)))
      (should (= 2 (plist-get report :with-usage)))
      (should (member "stub-c" (plist-get report :never-called)))
      (should (member "stub-b" (plist-get report :unused-since-days)))
      (should-not (member "stub-a"
                          (plist-get report :unused-since-days)))
      (let ((per-tool (plist-get report :per-tool)))
        ;; stub-a (recent) sorts before stub-b (stale)
        (should (equal "stub-a" (plist-get (car per-tool) :id)))))))

(ert-deftest anvil-discovery-test-usage-report-empty-registry ()
  "No registered tools → zeros, nil lists, no error."
  (let ((anvil-server--tools (make-hash-table :test #'equal))
        (anvil-state-db-path (make-temp-file
                              "anvil-discovery-empty-" nil ".db"))
        (anvil-state--db nil))
    (unwind-protect
        (progn
          (anvil-state-enable)
          (let ((r (anvil-discovery-usage-report "7")))
            (should (= 0 (plist-get r :total-registered)))
            (should (= 0 (plist-get r :with-usage)))
            (should (null (plist-get r :never-called)))
            (should (null (plist-get r :unused-since-days)))
            (should (null (plist-get r :per-tool)))))
      (anvil-state-disable)
      (ignore-errors (delete-file anvil-state-db-path)))))


;;;; --- dispatch hook end-to-end ------------------------------------------

(ert-deftest anvil-discovery-test-enable-installs-dispatch-hook ()
  "`anvil-discovery-enable' adds `anvil-discovery--record-call' to
the dispatch hook; disable removes it."
  (let ((anvil-server--tools (make-hash-table :test #'equal))
        (anvil-server-tool-dispatch-hook nil))
    (anvil-discovery-enable)
    (unwind-protect
        (should (memq #'anvil-discovery--record-call
                      anvil-server-tool-dispatch-hook))
      (anvil-discovery-disable))
    (should-not (memq #'anvil-discovery--record-call
                      anvil-server-tool-dispatch-hook))))

(provide 'anvil-discovery-test)
;;; anvil-discovery-test.el ends here
