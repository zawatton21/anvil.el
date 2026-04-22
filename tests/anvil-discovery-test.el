;;; anvil-discovery-test.el --- Tests for anvil-discovery Doc 34 Phase A -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the Doc 34 Phase A intent-based tool discovery:
;; metadata propagation through anvil-server-register-tool, intent /
;; layer / query / stability filtering, and deterministic sort order.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-server)
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

(provide 'anvil-discovery-test)
;;; anvil-discovery-test.el ends here
