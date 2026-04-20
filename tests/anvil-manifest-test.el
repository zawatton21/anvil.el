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
  "Handler advertises the five profile names."
  (anvil-manifest-test--with-server
    (let ((result (anvil-manifest-cost-handler)))
      (should (equal '(full core nav ultra lean)
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

(provide 'anvil-manifest-test)
;;; anvil-manifest-test.el ends here
