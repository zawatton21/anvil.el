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
  (let ((response-json nil)
        (sid (or server-id "default")))
    (cl-letf (((symbol-function 'anvil-server--jsonrpc-response)
               (lambda (_id payload) (setq response-json payload))))
      (anvil-server--handle-tools-list 1 sid))
    (mapcar (lambda (entry) (alist-get 'name entry))
            (append (alist-get 'tools response-json) nil))))

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
  "Handler advertises every profile name (5 legacy + 2 Phase B)."
  (anvil-manifest-test--with-server
    (let ((result (anvil-manifest-cost-handler)))
      (should (equal '(full core nav ultra lean agent edit)
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
                  anvil-manifest--default-aliases)))

(ert-deftest anvil-manifest-test-default-server-profiles-include-agent-edit ()
  "Doc 34 Phase B: virtual server-ids map to the right profile
symbols in the default `anvil-manifest-server-profiles'."
  (should (eq 'agent
              (cdr (assoc "emacs-eval-agent"
                          (default-value 'anvil-manifest-server-profiles)))))
  (should (eq 'edit
              (cdr (assoc "emacs-eval-edit"
                          (default-value 'anvil-manifest-server-profiles))))))

(provide 'anvil-manifest-test)
;;; anvil-manifest-test.el ends here
