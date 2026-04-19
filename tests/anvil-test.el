;;; anvil-test.el --- Tests for anvil.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic smoke tests for the anvil package.

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil)
(require 'anvil-server)
(require 'anvil-server-metrics)
;; `anvil-offload-stub' provides tiny fixture handlers (pid / boom) so
;; the `:offload' dispatch tests can load them into the subprocess by
;; feature name.  Optional: if the stub is absent the `:offload' tests
;; simply fail their registration step and everyone else keeps working.
(require 'anvil-offload-stub nil 'noerror)
(require 'anvil-test-fixtures)

(ert-deftest anvil-test-feature-provided ()
  "Verify that anvil feature is provided."
  (should (featurep 'anvil)))

(ert-deftest anvil-test-customization-group ()
  "Verify customization variables exist."
  (should (boundp 'anvil-modules))
  (should (boundp 'anvil-optional-modules))
  (should (boundp 'anvil-server-id)))

(ert-deftest anvil-test-initial-state ()
  "Verify initial state is disabled."
  (should-not anvil--enabled)
  (should-not anvil--loaded-modules))

(ert-deftest anvil-test-describe-setup-command ()
  "Verify describe-setup is callable."
  (should (fboundp 'anvil-describe-setup)))

;;;; --- MCP parameter validator ------------------------------------------

(ert-deftest anvil-test-parser-accepts-underscore-params ()
  "Parameters prefixed with `_' (Elisp unused-arg convention) must not
trigger the \"missing from MCP Parameters section\" error.  This is the
regression fix for the 2026-04-16 `anvil-cron' optional-module skip."
  ;; Zero-arg tool with no Parameters section is fine.
  (defun anvil-test--ignored-tool (_args)
    "A tool that takes no real parameters.

MCP Parameters:
  (none)"
    "ok")
  (should
   (listp
    (anvil-server--extract-param-descriptions
     (documentation 'anvil-test--ignored-tool)
     '(_args)))))

(ert-deftest anvil-test-parser-still-rejects-undocumented-real-param ()
  "Non-underscore parameters must still be required in the docstring."
  (defun anvil-test--broken-tool (arg)
    "No Parameters section here, but arg is real."
    arg)
  (should-error
   (anvil-server--extract-param-descriptions
    (documentation 'anvil-test--broken-tool)
    '(arg))))

(ert-deftest anvil-test-schema-hides-underscore-params ()
  "JSON schema must not expose `_'-prefixed args to MCP clients."
  (defun anvil-test--tool-no-args (_args)
    "Zero-parameter tool.

MCP Parameters:
  (none)"
    "ok")
  (let ((schema (anvil-server--generate-schema-from-function
                 'anvil-test--tool-no-args)))
    ;; All-underscore arglist collapses to the no-args schema shape.
    (should (equal '((type . "object")) schema))))

(ert-deftest anvil-test-schema-mixed-underscore-and-real ()
  "When arglist has both `_'-prefixed and real params, hide only the `_' ones."
  (defun anvil-test--tool-mixed (_args task_id)
    "Mixed _ and real.

MCP Parameters:
  task_id - Task identifier (string, required)"
    task_id)
  (let* ((schema (anvil-server--generate-schema-from-function
                  'anvil-test--tool-mixed))
         (props (alist-get 'properties schema))
         (required (alist-get 'required schema)))
    (should (assoc "task_id" props))
    (should-not (assoc "_args" props))
    (should (equal ["task_id"] required))))

(ert-deftest anvil-test-dispatch-tolerates-stale-underscore-args ()
  "A client with a stale schema that still sends `_args' must not error.
The dispatcher silently drops `_'-prefixed provided params so a mid-flight
schema change does not break in-flight clients."
  (defun anvil-test--tool-legacy (_args)
    "Zero-parameter tool.

MCP Parameters:
  (none)"
    "ok-legacy")
  (unwind-protect
      (progn
        (anvil-server-register-tool
         #'anvil-test--tool-legacy
         :id "anvil-test-legacy"
         :description "test"
         :server-id "anvil-test")
        ;; Client sends {_args: "stale"} — must NOT error.
        (let* ((params '((name . "anvil-test-legacy")
                         (arguments . ((_args . "stale")))))
               (resp (anvil-server--handle-tools-call
                      "t2" params
                      (make-anvil-server-metrics) "anvil-test"))
               (decoded (json-read-from-string resp)))
          (should (alist-get 'result decoded))
          (let* ((result (alist-get 'result decoded))
                 (content (alist-get 'content result))
                 (first (aref content 0))
                 (text (alist-get 'text first)))
            (should (equal "ok-legacy" text)))))
    (anvil-server-unregister-tool "anvil-test-legacy" "anvil-test")))

(ert-deftest anvil-test-dispatch-accepts-empty-args-for-underscore-tool ()
  "tools/call with empty arguments must succeed for `_'-only handlers.
This is the dispatcher side of the fix — schema hides `_args', client
sends `{}', dispatcher fills `_args' with nil and calls the handler."
  (defun anvil-test--tool-underscore-only (_args)
    "Zero-parameter tool.

MCP Parameters:
  (none)"
    "ok-empty")
  (unwind-protect
      (progn
        (anvil-server-register-tool
         #'anvil-test--tool-underscore-only
         :id "anvil-test-underscore"
         :description "test"
         :server-id "anvil-test")
        (let* ((params '((name . "anvil-test-underscore")
                         (arguments . ())))
               (resp (anvil-server--handle-tools-call
                      "t1" params
                      (make-anvil-server-metrics) "anvil-test"))
               (decoded (json-read-from-string resp)))
          (should (alist-get 'result decoded))
          (let* ((result (alist-get 'result decoded))
                 (content (alist-get 'content result))
                 (first (aref content 0))
                 (text (alist-get 'text first)))
            (should (equal "ok-empty" text)))))
    (anvil-server-unregister-tool "anvil-test-underscore" "anvil-test")))

(ert-deftest anvil-test-schema-includes-real-params ()
  "JSON schema must still include non-underscore args."
  (defun anvil-test--tool-with-arg (task_id)
    "Tool with a real arg.

MCP Parameters:
  task_id - Task identifier (string, required)"
    task_id)
  (let* ((schema (anvil-server--generate-schema-from-function
                  'anvil-test--tool-with-arg))
         (props (alist-get 'properties schema))
         (required (alist-get 'required schema)))
    (should (assoc "task_id" props))
    (should (equal ["task_id"] required))))

;;; :offload dispatch (Doc 03 Phase 2b) ------------------------------

(defun anvil-test--offload-stub-dir ()
  "Return the tests/ directory so the subprocess can load the stub."
  (file-name-directory
   (or load-file-name buffer-file-name (expand-file-name "tests/"))))

(ert-deftest anvil-test-offload-dispatch-runs-in-subprocess ()
  "A tool registered with `:offload t' executes in a batch subprocess.
The PID returned must differ from the main daemon's PID."
  (require 'anvil-offload)
  (unwind-protect
      (progn
        (anvil-server-register-tool
         #'anvil-offload-stub-pid-tool
         :id "anvil-test-offload"
         :description "test offload"
         :server-id "anvil-test"
         :offload t
         :offload-load-path (list (anvil-test--offload-stub-dir))
         :offload-require 'anvil-offload-stub
         :offload-timeout 30)
        (let* ((params '((name . "anvil-test-offload")
                         (arguments . ((tag . "hi")))))
               (resp (anvil-server--handle-tools-call
                      "t-offload" params
                      (make-anvil-server-metrics) "anvil-test"))
               (decoded (json-read-from-string resp))
               (result (alist-get 'result decoded))
               (content (alist-get 'content result))
               (first (aref content 0))
               (text (alist-get 'text first)))
          (should (string-match "\\`pid:\\([0-9]+\\) tag:hi\\'" text))
          (let ((remote-pid (string-to-number (match-string 1 text))))
            (should (integerp remote-pid))
            (should-not (= remote-pid (emacs-pid))))))
    (anvil-server-unregister-tool "anvil-test-offload" "anvil-test")
    (ignore-errors (anvil-offload-stop-repl))))

(ert-deftest anvil-test-offload-remote-error-becomes-tool-error ()
  "Remote errors from the offload REPL surface as `isError': t."
  (require 'anvil-offload)
  (unwind-protect
      (progn
        (anvil-server-register-tool
         #'anvil-offload-stub-boom
         :id "anvil-test-offload-boom"
         :description "boom"
         :server-id "anvil-test"
         :offload t
         :offload-load-path (list (anvil-test--offload-stub-dir))
         :offload-require 'anvil-offload-stub
         :offload-timeout 30)
        (let* ((params '((name . "anvil-test-offload-boom")
                         (arguments . ((_ignored . "x")))))
               (resp (anvil-server--handle-tools-call
                      "t-boom" params
                      (make-anvil-server-metrics) "anvil-test"))
               (decoded (json-read-from-string resp))
               (result (alist-get 'result decoded))
               (is-error (alist-get 'isError result)))
          (should (eq t is-error))))
    (anvil-server-unregister-tool "anvil-test-offload-boom" "anvil-test")
    (ignore-errors (anvil-offload-stop-repl))))

(ert-deftest anvil-test-offload-auto-derive-from-symbol-file ()
  "With only `:offload t' the dispatcher derives :require / :load-path.
The stub handler lives in tests/anvil-offload-stub.el which provides
`anvil-offload-stub' — `symbol-file' gets us both the feature name
\(basename) and its directory."
  (require 'anvil-offload)
  (unwind-protect
      (progn
        (anvil-server-register-tool
         #'anvil-offload-stub-pid-tool
         :id "anvil-test-offload-auto"
         :description "auto-derive test"
         :server-id "anvil-test"
         :offload t
         :offload-timeout 30)
        (let* ((params '((name . "anvil-test-offload-auto")
                         (arguments . ((tag . "auto")))))
               (resp (anvil-server--handle-tools-call
                      "t-auto" params
                      (make-anvil-server-metrics) "anvil-test"))
               (decoded (json-read-from-string resp))
               (result (alist-get 'result decoded))
               (content (alist-get 'content result))
               (text (alist-get 'text (aref content 0))))
          (should (string-match "\\`pid:\\([0-9]+\\) tag:auto\\'" text))
          (let ((remote-pid (string-to-number (match-string 1 text))))
            (should-not (= remote-pid (emacs-pid))))))
    (anvil-server-unregister-tool "anvil-test-offload-auto" "anvil-test")
    (ignore-errors (anvil-offload-stop-repl))))

(ert-deftest anvil-test-offload-auto-derive-helper ()
  "`anvil-server--offload-auto-derive' returns (FEATURE . (DIR)) for a loaded fn."
  (let ((pair (anvil-server--offload-auto-derive
               'anvil-offload-stub-pid-tool)))
    (should (consp pair))
    (should (eq 'anvil-offload-stub (car pair)))
    (should (stringp (car (cdr pair))))
    (should (file-directory-p (car (cdr pair)))))
  ;; Undefined / fresh symbol has no source file → returns nil.
  (let ((sym (make-symbol "anvil-test--never-defined")))
    (should-not (anvil-server--offload-auto-derive sym))))

(ert-deftest anvil-test-offload-inherit-load-path-adds-daemon-entries ()
  "`:offload-inherit-load-path t' grows the subprocess's `load-path'.
Compare the same handler invoked with and without the flag — the
inheriting call must report a strictly larger `load-path'."
  (require 'anvil-offload)
  (let (len-inherit len-plain)
    (unwind-protect
        (progn
          (anvil-server-register-tool
           #'anvil-offload-stub-load-path-size
           :id "anvil-test-lp-inherit"
           :description "inherit"
           :server-id "anvil-test"
           :offload t
           :offload-inherit-load-path t
           :offload-timeout 30)
          (let* ((params '((name . "anvil-test-lp-inherit")
                           (arguments . ((_ignored . "x")))))
                 (resp (anvil-server--handle-tools-call
                        "t-lp-i" params
                        (make-anvil-server-metrics) "anvil-test"))
                 (decoded (json-read-from-string resp))
                 (result (alist-get 'result decoded))
                 (text (alist-get 'text
                                  (aref (alist-get 'content result) 0))))
            (setq len-inherit (string-to-number text))))
      (anvil-server-unregister-tool "anvil-test-lp-inherit" "anvil-test")
      (ignore-errors (anvil-offload-stop-repl)))
    (unwind-protect
        (progn
          (anvil-server-register-tool
           #'anvil-offload-stub-load-path-size
           :id "anvil-test-lp-plain"
           :description "plain"
           :server-id "anvil-test"
           :offload t
           :offload-timeout 30)
          (let* ((params '((name . "anvil-test-lp-plain")
                           (arguments . ((_ignored . "x")))))
                 (resp (anvil-server--handle-tools-call
                        "t-lp-p" params
                        (make-anvil-server-metrics) "anvil-test"))
                 (decoded (json-read-from-string resp))
                 (result (alist-get 'result decoded))
                 (text (alist-get 'text
                                  (aref (alist-get 'content result) 0))))
            (setq len-plain (string-to-number text))))
      (anvil-server-unregister-tool "anvil-test-lp-plain" "anvil-test")
      (ignore-errors (anvil-offload-stop-repl)))
    (should (integerp len-inherit))
    (should (integerp len-plain))
    (should (> len-inherit len-plain))))

(ert-deftest anvil-test-offload-timeout-surfaces-as-tool-error ()
  "A tool that exceeds `:offload-timeout' signals an MCP tool error."
  (require 'anvil-offload)
  (unwind-protect
      (progn
        (anvil-server-register-tool
         #'anvil-offload-stub-sleep
         :id "anvil-test-offload-slow"
         :description "slow"
         :server-id "anvil-test"
         :offload t
         :offload-load-path (list (anvil-test--offload-stub-dir))
         :offload-require 'anvil-offload-stub
         :offload-timeout 0.5)
        (let* ((params '((name . "anvil-test-offload-slow")
                         (arguments . ((_ignored . "x")))))
               (resp (anvil-server--handle-tools-call
                      "t-slow" params
                      (make-anvil-server-metrics) "anvil-test"))
               (decoded (json-read-from-string resp))
               (result (alist-get 'result decoded))
               (is-error (alist-get 'isError result))
               (content (alist-get 'content result))
               (text (alist-get 'text (aref content 0))))
          (should (eq t is-error))
          (should (string-match-p "budget exceeded" text))))
    (anvil-server-unregister-tool "anvil-test-offload-slow" "anvil-test")
    (ignore-errors (anvil-offload-stop-repl))))

(ert-deftest anvil-test-offload-timeout-kills-and-respawns-slot ()
  "Budget-exceeded kill actually terminates the subprocess slot.
The PID observed on the second dispatch (after kill) must differ
from the PID that ran the first call."
  (require 'anvil-offload)
  (let (pid-before pid-after)
    (unwind-protect
        (progn
          (anvil-server-register-tool
           #'anvil-offload-stub-pid-tool
           :id "anvil-test-pid-probe"
           :description "pid"
           :server-id "anvil-test"
           :offload t
           :offload-timeout 30)
          (anvil-server-register-tool
           #'anvil-offload-stub-sleep
           :id "anvil-test-sleeper"
           :description "slow"
           :server-id "anvil-test"
           :offload t
           :offload-timeout 0.4)
          (let* ((metrics (make-anvil-server-metrics))
                 (decode-pid (lambda (resp)
                               (let* ((r (alist-get 'result
                                                    (json-read-from-string resp)))
                                      (txt (alist-get 'text
                                                      (aref (alist-get 'content r) 0))))
                                 (string-match "pid:\\([0-9]+\\)" txt)
                                 (string-to-number (match-string 1 txt))))))
            (setq pid-before
                  (funcall decode-pid
                           (anvil-server--handle-tools-call
                            "t-pid1"
                            '((name . "anvil-test-pid-probe")
                              (arguments . ((tag . "before"))))
                            metrics "anvil-test")))
            ;; Fire the sleeper — should budget-exceed, killing the slot.
            (let* ((sleep-resp
                    (anvil-server--handle-tools-call
                     "t-slp"
                     '((name . "anvil-test-sleeper")
                       (arguments . ((_ignored . "x"))))
                     metrics "anvil-test"))
                   (sleep-result (alist-get 'result
                                            (json-read-from-string sleep-resp))))
              (should (eq t (alist-get 'isError sleep-result))))
            ;; Next probe call must land on a FRESH slot — PID differs.
            (setq pid-after
                  (funcall decode-pid
                           (anvil-server--handle-tools-call
                            "t-pid2"
                            '((name . "anvil-test-pid-probe")
                              (arguments . ((tag . "after"))))
                            metrics "anvil-test")))))
      (anvil-server-unregister-tool "anvil-test-pid-probe" "anvil-test")
      (anvil-server-unregister-tool "anvil-test-sleeper" "anvil-test")
      (ignore-errors (anvil-offload-stop-repl)))
    (should (integerp pid-before))
    (should (integerp pid-after))
    (should-not (= pid-before pid-after))))

(ert-deftest anvil-test-offload-resumable-returns-partial-on-budget ()
  "A `:resumable t' tool converts budget-exceeded into a partial plist
instead of `isError: t'.  The MCP content carries :status 'partial
with a `:consumed-sec' number and `:reason' budget-exceeded."
  (require 'anvil-offload)
  (unwind-protect
      (progn
        (anvil-server-register-tool
         #'anvil-offload-stub-sleep
         :id "anvil-test-resumable-slow"
         :description "slow-resumable"
         :server-id "anvil-test"
         :offload t
         :resumable t
         :offload-timeout 0.3)
        (let* ((params '((name . "anvil-test-resumable-slow")
                         (arguments . ((_ignored . "x")))))
               (resp (anvil-server--handle-tools-call
                      "t-resume" params
                      (make-anvil-server-metrics) "anvil-test"))
               (decoded (json-read-from-string resp))
               (result (alist-get 'result decoded))
               (is-error (alist-get 'isError result))
               (text (alist-get 'text
                                (aref (alist-get 'content result) 0)))
               (plist (car (read-from-string text))))
          (should (eq :json-false is-error))
          (should (eq 'partial (plist-get plist :status)))
          (should (eq 'budget-exceeded (plist-get plist :reason)))
          (should (numberp (plist-get plist :consumed-sec)))
          (should (>= (plist-get plist :consumed-sec) 0.25))))
    (anvil-server-unregister-tool "anvil-test-resumable-slow" "anvil-test")
    (ignore-errors (anvil-offload-stop-repl))))

(ert-deftest anvil-test-offload-resumable-folds-in-checkpoint ()
  "With `:resumable t', the partial plist carries the latest
checkpoint's `:value' and `:cursor' when the handler called
`anvil-preempt-checkpoint' before running out of budget."
  (require 'anvil-offload)
  (unwind-protect
      (progn
        (anvil-server-register-tool
         #'anvil-offload-stub-checkpoint-then-sleep
         :id "anvil-test-resumable-ckpt"
         :description "resumable with checkpoint"
         :server-id "anvil-test"
         :offload t
         :offload-require 'anvil-offload-stub
         ;; Subprocess needs tests/ on its load-path to find the stub,
         ;; and enough time to actually run before budget fires so the
         ;; checkpoint reaches the daemon — if this test ever flakes,
         ;; bump :offload-timeout before doubting the assertions.
         :offload-inherit-load-path t
         :resumable t
         :offload-timeout 2.0)
        (let* ((params '((name . "anvil-test-resumable-ckpt")
                         (arguments . ((tag . "run-A")))))
               (resp (anvil-server--handle-tools-call
                      "t-ckpt" params
                      (make-anvil-server-metrics) "anvil-test"))
               (decoded (json-read-from-string resp))
               (result (alist-get 'result decoded))
               (is-error (alist-get 'isError result))
               (text (alist-get 'text
                                (aref (alist-get 'content result) 0)))
               (plist (car (read-from-string text))))
          (should (eq :json-false is-error))
          (should (eq 'partial (plist-get plist :status)))
          (should (eq 'budget-exceeded (plist-get plist :reason)))
          (should (equal "value:run-A"  (plist-get plist :value)))
          (should (equal "cursor:run-A" (plist-get plist :cursor)))))
    (anvil-server-unregister-tool "anvil-test-resumable-ckpt" "anvil-test")
    (ignore-errors (anvil-offload-stop-repl))))


;;;; --- bundle: anvil-server-register-tools / -unregister-tools -----------

(defvar anvil-test--spec-handler-a-called nil)
(defvar anvil-test--spec-handler-b-called nil)

(defun anvil-test--spec-handler-a ()
  "Test tool A."
  (setq anvil-test--spec-handler-a-called t)
  (list :ok "a"))

(defun anvil-test--spec-handler-b ()
  "Test tool B."
  (setq anvil-test--spec-handler-b-called t)
  (list :ok "b"))

(ert-deftest anvil-test-server-register-tools-registers-all ()
  "`anvil-server-register-tools' enrols every spec under SERVER-ID."
  (let ((specs `((,#'anvil-test--spec-handler-a
                  :id "anvil-test-spec-a"
                  :description "spec-a")
                 (,#'anvil-test--spec-handler-b
                  :id "anvil-test-spec-b"
                  :description "spec-b"
                  :read-only t))))
    (unwind-protect
        (let ((ids (anvil-server-register-tools "anvil-test" specs))
              (registered (anvil-test-fixtures-registered-tool-ids
                           "anvil-test")))
          (should (equal '("anvil-test-spec-a" "anvil-test-spec-b") ids))
          (should (member "anvil-test-spec-a" registered))
          (should (member "anvil-test-spec-b" registered)))
      (anvil-server-unregister-tools "anvil-test" specs))))

(ert-deftest anvil-test-server-unregister-tools-removes-all ()
  "`anvil-server-unregister-tools' removes every :id from SPECS."
  (let ((specs `((,#'anvil-test--spec-handler-a
                  :id "anvil-test-unreg-a"
                  :description "a")
                 (,#'anvil-test--spec-handler-b
                  :id "anvil-test-unreg-b"
                  :description "b"))))
    (anvil-server-register-tools "anvil-test" specs)
    (let ((results (anvil-server-unregister-tools "anvil-test" specs))
          (leftover (anvil-test-fixtures-registered-tool-ids "anvil-test")))
      (should (= 2 (length results)))
      (should-not (member "anvil-test-unreg-a" leftover))
      (should-not (member "anvil-test-unreg-b" leftover)))))

(ert-deftest anvil-test-server-register-tools-overrides-stale-server-id ()
  "Even if a spec carries :server-id, the argument SERVER-ID wins."
  (let ((specs `((,#'anvil-test--spec-handler-a
                  :id "anvil-test-override"
                  :description "override"
                  :server-id "some-stale-server"))))
    (unwind-protect
        (progn
          (anvil-server-register-tools "anvil-test" specs)
          (should (member "anvil-test-override"
                          (anvil-test-fixtures-registered-tool-ids
                           "anvil-test")))
          (should-not (member "anvil-test-override"
                              (anvil-test-fixtures-registered-tool-ids
                               "some-stale-server"))))
      (anvil-server-unregister-tools "anvil-test" specs))))

(ert-deftest anvil-test-server-register-tools-rejects-bad-spec ()
  "Malformed specs abort registration with a clear error."
  (should-error (anvil-server-register-tools
                 "anvil-test"
                 '(("not-a-function" :id "x" :description "x")))
                :type 'error)
  (should-error (anvil-server-register-tools
                 "anvil-test"
                 `((,#'anvil-test--spec-handler-a :id "x")))
                :type 'error))

;;; anvil-test.el ends here
