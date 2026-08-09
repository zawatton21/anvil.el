;;; anvil-manifest-test.el --- Tests for anvil-manifest -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the Doc 26 Phase 1 manifest-profile filter:
;; profile toolset dispatch, tools/list filtering, and the
;; manifest-cost handler.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-server)
(require 'anvil-manifest)

;;;; --- fixture ------------------------------------------------------------

(defmacro anvil-manifest-test--with-server (&rest body)
  "Run BODY against a fresh in-memory anvil-server tools table.
Registers three stub tools (`stub-ultra', `stub-nav', `stub-hidden')
plus the real `manifest-cost' handler, then cleans up.  Server-id
aliases and per-server-id profile overrides are also cleared for
isolation from any outer daemon state."
  (declare (indent 0))
  `(let ((anvil-server--tools (make-hash-table :test #'equal))
         (anvil-server-tool-filter-function nil)
         (anvil-server-id-aliases nil)
         (anvil-manifest-profile 'full)
         (anvil-manifest-server-profiles nil))
     (unwind-protect
         (progn
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-ultra"
            :description "hot tool" :read-only t)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-nav"
            :description "nav tool" :read-only t)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-hidden"
            :description "heavy tool" :read-only nil)
           ,@body)
       (setq anvil-server-tool-filter-function nil))))

(defun anvil-manifest-test--tools-list-names (&optional server-id)
  "Return the list of tool names currently advertised by tools/list.
SERVER-ID defaults to \"default\" and is resolved through
`anvil-server-id-aliases' by the handler."
  (let* ((sid (or server-id "default"))
         ;; bc8da89 added a per-server-id JSON cache.  Tests share the
         ;; top-level cache table (the fixture only lets `anvil-server--tools')
         ;; and may rebind `anvil-server-tool-filter-function' or
         ;; `anvil-server-id-aliases' between calls; full clrhash so each
         ;; call rebuilds from current dynamic state.
         (_ (anvil-server--tools-list-cache-invalidate))
         (response-string (anvil-server--handle-tools-list 1 sid))
         (response (json-read-from-string response-string))
         (result (alist-get 'result response))
         (tools (alist-get 'tools result)))
    (mapcar (lambda (entry) (alist-get 'name entry))
            (append tools nil))))

;;;; --- profile-toolset dispatch ------------------------------------------

(ert-deftest anvil-manifest-test-toolset-full-is-sentinel ()
  "`full' profile returns the `:all' sentinel (no enumerable list)."
  (should (eq :all (anvil-manifest--profile-toolset 'full))))

(ert-deftest anvil-manifest-test-toolset-ultra-is-subset-of-nav ()
  "Every ultra entry appears in nav (nav is ultra ∪ extras)."
  (dolist (t-id anvil-manifest-profile-ultra)
    (should (member t-id anvil-manifest-profile-nav))))

(ert-deftest anvil-manifest-test-toolset-nav-is-subset-of-core ()
  "Every nav entry appears in core (core is nav ∪ extras)."
  (dolist (t-id anvil-manifest-profile-nav)
    (should (member t-id anvil-manifest-profile-core))))

(ert-deftest anvil-manifest-test-unknown-profile-errors ()
  "Unknown profile signals user-error rather than silently failing open."
  (should-error (anvil-manifest--profile-toolset 'nonsense)
                :type 'user-error))

;;;; --- visibility predicate ----------------------------------------------

(ert-deftest anvil-manifest-test-visibility-full-shows-everything ()
  "Under `full' every tool id is visible regardless of registration."
  (let ((anvil-manifest-profile 'full))
    (should (anvil-manifest--visible-p "stub-ultra" nil))
    (should (anvil-manifest--visible-p "stub-hidden" nil))
    (should (anvil-manifest--visible-p "totally-unknown" nil))))

(ert-deftest anvil-manifest-test-visibility-ultra-hides-non-listed ()
  "Under `ultra' only listed tool ids are visible."
  (let ((anvil-manifest-profile 'ultra))
    (should (anvil-manifest--visible-p "file-read" nil))
    (should-not (anvil-manifest--visible-p "stub-hidden" nil))
    (should-not (anvil-manifest--visible-p "orchestrator-submit" nil))))

;;;; --- tools/list filter round-trip --------------------------------------

(ert-deftest anvil-manifest-test-tools-list-no-filter-shows-all ()
  "With filter function nil, every registered tool is advertised."
  (anvil-manifest-test--with-server
    (let ((names (anvil-manifest-test--tools-list-names)))
      (should (member "stub-ultra" names))
      (should (member "stub-nav" names))
      (should (member "stub-hidden" names)))))

(ert-deftest anvil-manifest-test-tools-list-filter-hides-unlisted ()
  "Filter function restricts tools/list to matching ids."
  (anvil-manifest-test--with-server
    (let ((anvil-server-tool-filter-function
           (lambda (tool-id _tool _server-id)
             (member tool-id '("stub-ultra" "stub-nav")))))
      (let ((names (anvil-manifest-test--tools-list-names)))
        (should (member "stub-ultra" names))
        (should (member "stub-nav" names))
        (should-not (member "stub-hidden" names))))))

(ert-deftest anvil-manifest-test-enable-activates-profile-filter ()
  "`anvil-manifest-enable' installs the profile filter and adds manifest-cost."
  (anvil-manifest-test--with-server
    (let ((anvil-manifest-profile 'ultra))
      (anvil-manifest-enable)
      (unwind-protect
          (progn
            (should (eq anvil-server-tool-filter-function
                        #'anvil-manifest--visible-p))
            (let ((names (anvil-manifest-test--tools-list-names)))
              (should (member "manifest-cost" names))
              (should-not (member "stub-hidden" names))))
        (anvil-manifest-disable)))))

(ert-deftest anvil-manifest-test-disable-removes-filter ()
  "`anvil-manifest-disable' clears the filter and unregisters the tool."
  (anvil-manifest-test--with-server
    (anvil-manifest-enable)
    (anvil-manifest-disable)
    (should (null anvil-server-tool-filter-function))
    (let ((names (anvil-manifest-test--tools-list-names)))
      (should-not (member "manifest-cost" names)))))

;;;; --- manifest-cost handler ---------------------------------------------

(ert-deftest anvil-manifest-test-cost-full-counts-all-tools ()
  "Under `full', advertised-count equals registered-count."
  (anvil-manifest-test--with-server
    (let* ((anvil-manifest-profile 'full)
           (result (anvil-manifest-cost-handler)))
      (should (eq (plist-get result :profile) 'full))
      (should (= (plist-get result :advertised-count)
                 (plist-get result :registered-count)))
      (should (> (plist-get result :approx-tokens) 0)))))

(ert-deftest anvil-manifest-test-cost-ultra-shrinks-count ()
  "Under `ultra', advertised-count is smaller than registered-count."
  (anvil-manifest-test--with-server
    (let* ((anvil-manifest-profile 'ultra)
           (result (anvil-manifest-cost-handler)))
      (should (eq (plist-get result :profile) 'ultra))
      (should (<= (plist-get result :advertised-count)
                  (plist-get result :registered-count)))
      ;; Only stub-hidden/stub-nav/stub-ultra exist; ultra matches none
      ;; of them (IDs don't intersect), so advertised-count is 0.
      (should (= 0 (plist-get result :advertised-count))))))

(ert-deftest anvil-manifest-test-cost-lists-available-profiles ()
  "Handler advertises every profile name, including Doc44 dynamic."
  (anvil-manifest-test--with-server
    (let ((result (anvil-manifest-cost-handler)))
      (should (equal '(full core nav ultra lean agent edit headless dynamic)
                     (plist-get result :profiles-available))))))

;;;; --- Phase 1a: per-server-id profile + alias ---------------------------

(ert-deftest anvil-manifest-test-server-id-alias-resolution ()
  "`anvil-server--resolve-id' follows `anvil-server-id-aliases'."
  (let ((anvil-server-id-aliases '(("v-ultra" . "real")
                                   ("v-nav"   . "real"))))
    (should (equal "real"    (anvil-server--resolve-id "v-ultra")))
    (should (equal "real"    (anvil-server--resolve-id "v-nav")))
    (should (equal "missing" (anvil-server--resolve-id "missing")))))

(ert-deftest anvil-manifest-test-tools-list-via-alias-sees-same-pool ()
  "tools/list under a virtual server-id reads the real id's tool table."
  (anvil-manifest-test--with-server
    (let ((anvil-server-id-aliases '(("default-alias" . "default")))
          (anvil-server-tool-filter-function nil))
      (let ((names (anvil-manifest-test--tools-list-names "default-alias")))
        (should (member "stub-ultra" names))
        (should (member "stub-hidden" names))))))

(ert-deftest anvil-manifest-test-filter-sees-original-server-id ()
  "Filter function receives the original (virtual) server-id, not resolved."
  (anvil-manifest-test--with-server
    (let ((seen-ids '()))
      (let ((anvil-server-id-aliases '(("virt" . "default")))
            (anvil-server-tool-filter-function
             (lambda (_tool-id _tool server-id)
               (push server-id seen-ids)
               t)))
        (anvil-manifest-test--tools-list-names "virt")
        (should (cl-every (lambda (sid) (equal sid "virt")) seen-ids))
        (should (>= (length seen-ids) 3))))))

(ert-deftest anvil-manifest-test-per-server-id-profile-overrides-global ()
  "`anvil-manifest-server-profiles' overrides `anvil-manifest-profile' per id."
  (anvil-manifest-test--with-server
    (let ((anvil-manifest-profile 'full)
          (anvil-manifest-server-profiles '(("slim" . ultra))))
      ;; Default server-id: no override, uses global `full'
      (should (anvil-manifest--visible-p "stub-hidden" nil "default"))
      ;; Virtual server-id with `ultra' override: stub-hidden is hidden
      (should-not (anvil-manifest--visible-p "stub-hidden" nil "slim"))
      ;; An ultra tool id (e.g. `file-read') would pass under `slim'
      (should (anvil-manifest--visible-p "file-read" nil "slim")))))

(ert-deftest anvil-manifest-test-legacy-two-arg-filter-still-works ()
  "Calling `anvil-manifest--visible-p' with the pre-Phase-1a 2-arg
signature continues to use the global profile.  Keeps third-party
filter subclasses from breaking during the upgrade."
  (let ((anvil-manifest-profile 'full)
        (anvil-manifest-server-profiles nil))
    (should (anvil-manifest--visible-p "anything" nil))))

(ert-deftest anvil-manifest-test-enable-installs-default-aliases ()
  "`anvil-manifest-enable' adds `anvil-manifest--default-aliases'."
  (anvil-manifest-test--with-server
    (let ((anvil-server-id-aliases nil))
      (anvil-manifest-enable)
      (unwind-protect
          (dolist (alias anvil-manifest--default-aliases)
            (should (member alias anvil-server-id-aliases)))
        (anvil-manifest-disable)))))

(ert-deftest anvil-manifest-test-disable-removes-default-aliases ()
  "`anvil-manifest-disable' cleans up the aliases it installed."
  (anvil-manifest-test--with-server
    (let ((anvil-server-id-aliases '(("user-alias" . "real"))))
      (anvil-manifest-enable)
      (anvil-manifest-disable)
      (should (member '("user-alias" . "real") anvil-server-id-aliases))
      (dolist (alias anvil-manifest--default-aliases)
        (should-not (member alias anvil-server-id-aliases))))))

(ert-deftest anvil-manifest-test-alias-plus-ultra-profile-filters ()
  "End-to-end: orchestrator subprocess connects as `emacs-eval-ultra'
and sees only ultra tools while `emacs-eval' still shows everything."
  (anvil-manifest-test--with-server
    ;; Add the canonical ultra tools so they match the profile set.
    (anvil-server-register-tool
     (lambda () "ok") :id "file-read"
     :description "canonical ultra tool" :read-only t)
    (anvil-server-register-tool
     (lambda () "ok") :id "git-status"
     :description "canonical ultra tool" :read-only t)
    (anvil-manifest-enable)
    (unwind-protect
        (progn
          ;; Real id -> full profile, sees every stub + canonical tool.
          (let ((names (anvil-manifest-test--tools-list-names "default")))
            (should (member "stub-ultra" names))
            (should (member "stub-hidden" names))
            (should (member "manifest-cost" names)))
          ;; Install an alias for this test's `default' server-id so
          ;; the virtual ultra id routes to the same table.
          (let ((anvil-server-id-aliases
                 (cons '("default-ultra" . "default")
                       anvil-server-id-aliases))
                (anvil-manifest-server-profiles
                 '(("default-ultra" . ultra))))
            (let ((names
                   (anvil-manifest-test--tools-list-names
                    "default-ultra")))
              (should (member "file-read" names))
              (should (member "git-status" names))
              (should (member "manifest-cost" names))
              (should-not (member "stub-hidden" names))
              (should-not (member "stub-ultra" names)))))
      (anvil-manifest-disable))))

;;;; --- manifest snapshot (regression baseline) ---------------------------

(ert-deftest anvil-manifest-test-ultra-has-manifest-cost ()
  "`manifest-cost' must always be visible so Claude can introspect."
  (should (member "manifest-cost" anvil-manifest-profile-ultra)))

(ert-deftest anvil-manifest-test-ultra-has-manifest-attention ()
  "`manifest-attention' must be visible for Doc44 self-inspection."
  (should (member "manifest-attention" anvil-manifest-profile-ultra)))

(ert-deftest anvil-manifest-test-ultra-has-recovery-suggest ()
  "`manifest-recovery-suggest' must be visible for Doc44 recovery."
  (should (member "manifest-recovery-suggest" anvil-manifest-profile-ultra)))

(ert-deftest anvil-manifest-test-ultra-has-docstring-candidates ()
  "`manifest-docstring-candidates' must be visible for Doc44 Phase 2d."
  (should (member "manifest-docstring-candidates"
                  anvil-manifest-profile-ultra)))

(ert-deftest anvil-manifest-test-ultra-under-20-tools ()
  "ultra stays hot-path only (guard against drift)."
  (should (< (length anvil-manifest-profile-ultra) 25)))

(ert-deftest anvil-manifest-test-no-duplicates-in-any-profile ()
  "Profile lists must not contain duplicate tool ids."
  (dolist (profile (list anvil-manifest-profile-ultra
                         anvil-manifest-profile-nav
                         anvil-manifest-profile-core))
    (should (= (length profile)
               (length (cl-remove-duplicates profile :test #'equal))))))


;;;; --- Phase B: intent-based profiles (agent / edit) ----------------------

(defmacro anvil-manifest-test--with-intent-tools (&rest body)
  "Run BODY against a registry with a controlled set of tagged stubs."
  (declare (indent 0))
  `(let ((anvil-server--tools (make-hash-table :test #'equal))
         (anvil-server-tool-filter-function nil)
         (anvil-server-id-aliases nil)
         (anvil-manifest-profile 'full)
         (anvil-manifest-server-profiles nil))
     (unwind-protect
         (progn
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-file-edit"
            :description "edit file"
            :intent '(file-edit) :layer 'core :stability 'stable)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-org-edit"
            :description "edit org"
            :intent '(org-edit) :layer 'core :stability 'stable)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-git-read"
            :description "read git"
            :intent '(git read) :layer 'workflow :stability 'stable)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-http"
            :description "http fetch"
            :intent '(http) :layer 'io :stability 'stable)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-bench"
            :description "bench"
            :intent '(bench) :layer 'dev :stability 'stable)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-wip"
            :description "wip"
            :intent '(file-edit) :layer 'core
            :stability 'experimental)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-dead"
            :description "dead"
            :intent '(file-edit) :layer 'core
            :stability 'deprecated)
           (anvil-server-register-tool
            (lambda () "ok") :id "stub-untagged"
            :description "legacy")
           ,@body)
       (setq anvil-server-tool-filter-function nil))))

(ert-deftest anvil-manifest-test-toolset-agent-is-filter-spec ()
  "`agent' profile returns a `(:filter ...)' plist, not an ID list."
  (let ((set (anvil-manifest--profile-toolset 'agent)))
    (should (consp set))
    (should (eq (car set) :filter))))

(ert-deftest anvil-manifest-test-toolset-edit-is-filter-spec ()
  (let ((set (anvil-manifest--profile-toolset 'edit)))
    (should (consp set))
    (should (eq (car set) :filter))))

(ert-deftest anvil-manifest-test-toolset-dynamic-is-attention-spec ()
  "Doc44 dynamic profile returns an attention selector spec."
  (let ((set (anvil-manifest--profile-toolset 'dynamic)))
    (should (consp set))
    (should (eq (car set) :attention))))

(ert-deftest anvil-manifest-test-filter-match-intent-include ()
  "Filter matches when tool intent intersects the include list."
  (let ((filter '(:filter t :intent-include (file-edit))))
    (should (anvil-manifest--filter-match-p
             filter '(:intent (file-edit) :layer core :stability stable)))
    (should-not (anvil-manifest--filter-match-p
                 filter '(:intent (org-read) :layer core :stability stable)))))

(ert-deftest anvil-manifest-test-filter-match-layer-include ()
  (let ((filter '(:filter t :layer-include (core))))
    (should (anvil-manifest--filter-match-p
             filter '(:intent (any) :layer core :stability stable)))
    (should-not (anvil-manifest--filter-match-p
                 filter '(:intent (any) :layer io :stability stable)))))

(ert-deftest anvil-manifest-test-filter-excludes-deprecated ()
  (let ((filter '(:filter t :stability-exclude (deprecated))))
    (should-not (anvil-manifest--filter-match-p
                 filter '(:intent (any) :layer core :stability deprecated)))
    (should (anvil-manifest--filter-match-p
             filter '(:intent (any) :layer core :stability stable)))))

(ert-deftest anvil-manifest-test-filter-nil-tool-uses-defaults ()
  "A nil tool plist is treated as intent=general / layer=core / stability=stable."
  ;; Default-tagged (general) does not intersect (file-edit), so it fails
  ;; the agent filter's intent clause.
  (should-not (anvil-manifest--filter-match-p
               '(:filter t :intent-include (file-edit))
               nil))
  ;; No clauses => pass-all, even for nil plist.
  (should (anvil-manifest--filter-match-p '(:filter t) nil)))

(ert-deftest anvil-manifest-test-filter-missing-clause-is-pass-all ()
  "Absent filter clauses do not restrict the match."
  (let ((tool '(:intent (anything) :layer dev :stability stable)))
    ;; Only layer-include set: intent / stability are unrestricted.
    (should (anvil-manifest--filter-match-p
             '(:filter t :layer-include (dev)) tool))
    ;; No clauses at all: anything passes (including deprecated, since
    ;; stability-exclude is absent).
    (should (anvil-manifest--filter-match-p '(:filter t) tool))))

(ert-deftest anvil-manifest-test-visible-agent-profile ()
  "Under agent: file-edit / org-edit / git-read pass; http / bench / deprecated fail."
  (anvil-manifest-test--with-intent-tools
    (let* ((anvil-manifest-profile 'agent)
           (visible (lambda (id)
                      (let ((tool (gethash id
                                           (gethash "default"
                                                    anvil-server--tools))))
                        (anvil-manifest--visible-p id tool nil)))))
      (should (funcall visible "stub-file-edit"))
      (should (funcall visible "stub-org-edit"))
      (should (funcall visible "stub-git-read"))
      (should-not (funcall visible "stub-http"))
      (should-not (funcall visible "stub-bench"))
      (should-not (funcall visible "stub-dead")))))

(ert-deftest anvil-manifest-test-visible-edit-profile ()
  "Under edit: layer=core / intent=edit pass; workflow / io / dev fail."
  (anvil-manifest-test--with-intent-tools
    (let* ((anvil-manifest-profile 'edit)
           (visible (lambda (id)
                      (let ((tool (gethash id
                                           (gethash "default"
                                                    anvil-server--tools))))
                        (anvil-manifest--visible-p id tool nil)))))
      (should (funcall visible "stub-file-edit"))
      (should (funcall visible "stub-org-edit"))
      (should-not (funcall visible "stub-git-read"))
      (should-not (funcall visible "stub-http"))
      (should-not (funcall visible "stub-bench")))))

(ert-deftest anvil-manifest-test-visible-id-list-profiles-unchanged ()
  "Legacy ID-list profiles keep working after Phase B."
  (let ((anvil-manifest-profile 'ultra))
    ;; file-read is in ultra list; stub-untagged is not.
    (should (anvil-manifest--visible-p "file-read" nil))
    (should-not (anvil-manifest--visible-p "stub-untagged" nil))))

(ert-deftest anvil-manifest-test-visible-full-profile-unchanged ()
  "`full' profile always returns t regardless of metadata."
  (let ((anvil-manifest-profile 'full))
    (should (anvil-manifest--visible-p "anything" nil))
    (should (anvil-manifest--visible-p "anything"
                                       '(:intent (x) :layer dev
                                         :stability deprecated)))))

;;;; --- Doc 44 Phase 2a: Tool Attention IsO-lite --------------------------

(ert-deftest anvil-manifest-test-attention-rank-query ()
  "manifest-attention ranks tools by local lexical query relevance."
  (anvil-manifest-test--with-intent-tools
    (let* ((ranked (anvil-manifest--attention-rank "edit file" "default" 3))
           (ids (mapcar (lambda (row) (plist-get row :id)) ranked)))
      (should (member "stub-file-edit" ids))
      (should-not (member "stub-http" ids))
      (should (> (plist-get (car ranked) :score) 0)))))

(ert-deftest anvil-manifest-test-attention-state-filter ()
  "Doc44 state-filter drops tools whose preconditions do not match."
  (anvil-manifest-test--with-intent-tools
    (anvil-server-register-tool
     (lambda () "ok") :id "stub-workspace-edit"
     :description "edit workspace file"
     :intent '(file-edit) :layer 'core :stability 'stable
     :preconditions '(:requires (workspace) :excludes (readonly)))
    (let* ((anvil-manifest-state-tags nil)
           (none (mapcar (lambda (row) (plist-get row :id))
                         (anvil-manifest--attention-rank
                          "workspace edit" "default" 10)))
           (anvil-manifest-state-tags '(workspace))
           (ok (mapcar (lambda (row) (plist-get row :id))
                       (anvil-manifest--attention-rank
                        "workspace edit" "default" 10)))
           (anvil-manifest-state-tags '(workspace readonly))
           (blocked (mapcar (lambda (row) (plist-get row :id))
                            (anvil-manifest--attention-rank
                             "workspace edit" "default" 10))))
      (should-not (member "stub-workspace-edit" none))
      (should (member "stub-workspace-edit" ok))
      (should-not (member "stub-workspace-edit" blocked)))))

(ert-deftest anvil-manifest-test-attention-handler-shape ()
  "manifest-attention handler returns a JSON-encodable plist shape."
  (anvil-manifest-test--with-intent-tools
    (let ((res (anvil-manifest-attention-handler
                "http fetch" "default" "2")))
      (should (equal "http fetch" (plist-get res :query)))
      (should (equal "default" (plist-get res :server-id)))
      (should (vectorp (plist-get res :state-tags)))
      (should (= 2 (plist-get res :limit)))
      (should (> (plist-get res :count) 0))
      (should (vectorp (plist-get res :tools))))))

(ert-deftest anvil-manifest-test-attention-embedding-backend ()
  "Doc44 Phase 2a full uses embedding vectors when a backend is configured."
  (anvil-manifest-test--with-intent-tools
    (let ((anvil-manifest-embedding-backend 'function)
          (anvil-manifest--summary-index-cache
           (make-hash-table :test #'equal))
          (anvil-manifest-embedding-function
           (lambda (text)
             (cond
              ((string-match-p "semantic network" text) [1.0 0.0])
              ((string-match-p "http fetch" text) [0.95 0.05])
              ((string-match-p "edit file" text) [0.0 1.0])
              (t [0.1 0.1])))))
      (let* ((ranked (anvil-manifest--attention-rank
                      "semantic network" "default" 2))
             (top (car ranked))
             (res (anvil-manifest-attention-handler
                   "semantic network" "default" 2)))
        (should (equal "stub-http" (plist-get top :id)))
        (should (equal "embedding" (plist-get top :score-backend)))
        (should (equal "embedding" (plist-get res :ranking-backend)))
        (should (> (plist-get res :summary-index-count) 0))))))

(ert-deftest anvil-manifest-test-visible-dynamic-profile-query ()
  "Under dynamic, query hits keep full focus while other tools stay visible trimmed."
  (anvil-manifest-test--with-intent-tools
    (let ((anvil-manifest-profile 'dynamic)
          (anvil-manifest-attention-query "http fetch")
          (anvil-manifest-attention-top-k 1))
      (let ((http (gethash "stub-http" (gethash "default" anvil-server--tools)))
            (edit (gethash "stub-file-edit" (gethash "default" anvil-server--tools))))
        (should (anvil-manifest--visible-p "stub-http" http "default"))
        (should (anvil-manifest--focused-tool-p "stub-http" "default"))
        (should (anvil-manifest--visible-p "stub-file-edit" edit "default"))
        (should-not (anvil-manifest--focused-tool-p
                     "stub-file-edit" "default"))
        (should (anvil-manifest--visible-p
                 "manifest-attention" nil "default"))
        (should (anvil-manifest--visible-p
                 "manifest-recovery-suggest" nil "default"))))))

(ert-deftest anvil-manifest-test-visible-dynamic-can-disable-trim-expansion ()
  "When schema trimming is disabled, dynamic profile falls back to Top-K only."
  (anvil-manifest-test--with-intent-tools
    (let ((anvil-manifest-profile 'dynamic)
          (anvil-manifest-attention-query "http fetch")
          (anvil-manifest-attention-top-k 1)
          (anvil-manifest-schema-trim-enabled nil))
      (let ((http (gethash "stub-http" (gethash "default" anvil-server--tools)))
            (edit (gethash "stub-file-edit" (gethash "default" anvil-server--tools))))
        (should (anvil-manifest--visible-p "stub-http" http "default"))
        (should-not (anvil-manifest--visible-p
                     "stub-file-edit" edit "default"))))))

(ert-deftest anvil-manifest-test-visible-dynamic-empty-query-fallback ()
  "Dynamic profile with empty query falls back to the safe agent profile."
  (anvil-manifest-test--with-intent-tools
    (let ((anvil-manifest-profile 'dynamic)
          (anvil-manifest-attention-query ""))
      (let ((edit (gethash "stub-file-edit" (gethash "default" anvil-server--tools)))
            (http (gethash "stub-http" (gethash "default" anvil-server--tools))))
        (should (anvil-manifest--visible-p "stub-file-edit" edit "default"))
        (should-not (anvil-manifest--visible-p "stub-http" http "default"))))))

(ert-deftest anvil-manifest-test-docstring-candidates-doc43-bridge ()
  "Doc44 Phase 2d surfaces Doc43 docstring rewrite candidates."
  (anvil-manifest-test--with-intent-tools
    (let* ((res (anvil-manifest-docstring-candidates-handler
                 "edit file" "default" 3))
           (rows (append (plist-get res :candidates) nil))
           (top (car rows)))
      (should (equal (plist-get res :query) "edit file"))
      (should (<= (length rows) 3))
      (should (member "stub-file-edit"
                      (mapcar (lambda (row) (plist-get row :id))
                              rows)))
      (should (>= (plist-get top :manifest-score)
                  (plist-get top :score)))
      (should (equal (plist-get top :rewrite-tool)
                     "autoresearch-docstring-propose"))
      (should (equal (plist-get top :suggested-action)
                     "propose-docstring-rewrite-for-tool-attention")))))

(ert-deftest anvil-manifest-test-docstring-candidates-query-filters ()
  "Docstring candidate query narrows candidates by attention relevance."
  (anvil-manifest-test--with-intent-tools
    (let* ((res (anvil-manifest-docstring-candidates-handler
                 "http fetch" "default" 5))
           (rows (append (plist-get res :candidates) nil))
           (ids (mapcar (lambda (row) (plist-get row :id)) rows)))
      (should (member "stub-http" ids))
      (should-not (member "stub-file-edit" ids)))))

(ert-deftest anvil-manifest-test-schema-trimmer-removes-param-docs ()
  "Doc44 Phase 2c trims non-focused dynamic tool schemas."
  (let* ((tool
          '(:description "Long description first line\nsecond"
            :schema ((type . "object")
                     (properties
                      ("path" . ((type . "string")
                                 (description . "Long parameter description")))))))
         (default (anvil-server--build-tool-fragment
                   "stub-wide" (plist-get tool :description)
                   (plist-get tool :schema)))
         (anvil-manifest-profile 'dynamic)
         (anvil-manifest-attention-query "http fetch")
         (anvil-manifest-schema-trim-enabled t)
         (trimmed (anvil-manifest--tool-fragment
                   "stub-wide" tool "default" default)))
    (should (string-match-p "\"name\":\"stub-wide\"" trimmed))
    (should (string-match-p "\"path\"" trimmed))
    (should-not (string-match-p "Long parameter description" trimmed))
    (should-not (string-match-p "second" trimmed))))

(ert-deftest anvil-manifest-test-lru-warms-recent-tool ()
  "Doc44 Phase 2c keeps recently used tools focused."
  (let ((anvil-manifest--warm-tools nil)
        (anvil-manifest-attention-query "")
        (anvil-manifest-warm-tool-limit 2))
    (anvil-manifest--record-tool-dispatch "a" "default")
    (anvil-manifest--record-tool-dispatch "b" "default")
    (anvil-manifest--record-tool-dispatch "c" "default")
    (should (equal '("c" "b") anvil-manifest--warm-tools))
    (should (anvil-manifest--focused-tool-p "c" "default"))
    (should-not (anvil-manifest--focused-tool-p "a" "default"))))

(ert-deftest anvil-manifest-test-recovery-suggest-handler ()
  "Doc44 Phase 2c recovery handler reports visibility and focus reasons."
  (anvil-manifest-test--with-intent-tools
    (let ((anvil-manifest-profile 'dynamic)
          (anvil-manifest-attention-query "http fetch")
          (anvil-manifest-attention-top-k 1)
          (anvil-manifest--warm-tools nil))
      (let* ((res (anvil-manifest-recovery-suggest-handler
                   "http fetch" "default" 3))
             (rows (append (plist-get res :suggestions) nil))
             (http (cl-find "stub-http" rows
                            :key (lambda (row)
                                   (plist-get row :id))
                            :test #'equal)))
        (should (equal "http fetch" (plist-get res :query)))
        (should http)
        (should (eq t (plist-get http :visible)))
        (should (eq t (plist-get http :focused)))
        (should (equal "focused-full-schema"
                       (plist-get http :reason)))))))

(ert-deftest anvil-manifest-test-unknown-profile-still-errors-phase-b ()
  "Unknown profile continues to signal user-error (no regression)."
  (should-error (anvil-manifest--profile-toolset 'nonsense)
                :type 'user-error))

(ert-deftest anvil-manifest-test-default-aliases-include-agent-edit ()
  "Doc 34 Phase B: agent / edit virtual server-ids ship in the
default alias table so orchestrator injection works without the
user editing `anvil-server-id-aliases' by hand."
  (should (member '("emacs-eval-agent" . "emacs-eval")
                  anvil-manifest--default-aliases))
  (should (member '("emacs-eval-edit" . "emacs-eval")
                  anvil-manifest--default-aliases))
  (should (member '("emacs-eval-dynamic" . "emacs-eval")
                  anvil-manifest--default-aliases)))

(ert-deftest anvil-manifest-test-default-server-profiles-include-agent-edit ()
  "Doc 34 Phase B: virtual server-ids map to the right profile
symbols in the default `anvil-manifest-server-profiles'."
  (should (eq 'agent
              (cdr (assoc "emacs-eval-agent"
                          (default-value 'anvil-manifest-server-profiles)))))
  (should (eq 'edit
              (cdr (assoc "emacs-eval-edit"
                          (default-value 'anvil-manifest-server-profiles)))))
  (should (eq 'dynamic
              (cdr (assoc "emacs-eval-dynamic"
                          (default-value 'anvil-manifest-server-profiles))))))

;;;; Stage D (Doc 18) — headless profile for non-Emacs user distribution

(ert-deftest anvil-manifest-test-headless-profile-is-id-list ()
  "headless profile is an ID-list (non-empty list of strings)."
  (let ((toolset (anvil-manifest--profile-toolset 'headless)))
    (should (listp toolset))
    (should (> (length toolset) 0))
    (should (cl-every #'stringp toolset))))

(ert-deftest anvil-manifest-test-core-covers-async-eval-lifecycle ()
  "The core profile exposes every operation needed to manage async eval jobs."
  (let ((toolset (anvil-manifest--profile-toolset 'core)))
    (dolist (id '("emacs-eval" "emacs-eval-async" "emacs-eval-result"
                  "emacs-eval-jobs" "emacs-eval-cancel"))
      (should (member id toolset)))))

(ert-deftest anvil-manifest-test-headless-profile-covers-phase5e-7-tools ()
  "Stage D Phase 6.1 MVP は NeLisp Phase 5-E の 7 tool を必ず expose する。"
  (let ((toolset (anvil-manifest--profile-toolset 'headless)))
    (dolist (id '("file-read" "file-outline"
                  "git-log" "git-status"
                  "http-fetch"
                  "data-get-path" "data-set-path"))
      (should (member id toolset)))))

(ert-deftest anvil-manifest-test-headless-excludes-ui-dependent ()
  "headless は TUI / browser / orchestrator / worker-admin 等の UI 依存 tool を含まない。"
  (let ((toolset (anvil-manifest--profile-toolset 'headless))
        (ui-deps '("anvil-browser-open" "anvil-browser-close"
                   "orchestrator-submit" "orchestrator-stream"
                   "anvil-worker-probe" "anvil-worker-reset-pool"
                   "bisect-test" "cron-run"
                   "emacs-eval" "emacs-eval-async"
                   "pty-spawn" "pty-read-filtered")))
    (dolist (id ui-deps)
      (should-not (member id toolset)))))

(ert-deftest anvil-manifest-test-headless-alias-and-server-profile ()
  "emacs-eval-headless virtual server-id が default alias + server-profile の両方に含まれる。"
  (should (member '("emacs-eval-headless" . "emacs-eval")
                  anvil-manifest--default-aliases))
  (should (eq 'headless
              (cdr (assoc "emacs-eval-headless"
                          (default-value 'anvil-manifest-server-profiles))))))

(provide 'anvil-manifest-test)
;;; anvil-manifest-test.el ends here
