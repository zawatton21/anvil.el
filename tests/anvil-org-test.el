;;; anvil-org-test.el --- Tests for anvil-org -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused contract tests for org-mode MCP tool wrappers.

;;; Code:

(require 'ert)
(require 'anvil-org)

(ert-deftest anvil-org-test-tool-read-by-id-rejects-org-id-resource-uri ()
  "The Layer-3 tool accepts org:// citations, not org-id:// resources."
  (let ((err
         (should-error
          (anvil-org--tool-read-by-id
           "org-id://550e8400-e29b-41d4-a716-446655440000")
          :type 'anvil-server-tool-error)))
    (should
     (string-match-p
      "Parameter uuid does not accept org-id:// resource URIs"
      (cadr err)))
    (should
     (string-match-p
      "Use the raw UUID"
      (cadr err)))
    (should
     (string-match-p
      "org://550e8400-e29b-41d4-a716-446655440000"
      (cadr err)))))

;;; anvil-org-test.el ends here
