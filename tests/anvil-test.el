;;; anvil-test.el --- Tests for anvil.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic smoke tests for the anvil package.

;;; Code:

(require 'ert)
(require 'anvil)
(require 'anvil-server)

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

;;; anvil-test.el ends here
