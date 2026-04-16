;;; anvil-test.el --- Tests for anvil.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic smoke tests for the anvil package.

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil)
(require 'anvil-server)
(require 'anvil-server-metrics)

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

;;; anvil-test.el ends here
