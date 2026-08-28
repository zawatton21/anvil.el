;;; anvil-reader-escape-test.el --- Numeric reader escape regressions -*- lexical-binding: t; -*-

;;; Commentary:

;; Guards numeric string escapes used by anvil sources with expectations
;; built from integer character codes.  This prevents the source literal and
;; its test fixture from failing in the same way if reader escape handling
;; regresses.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-browser)
(require 'anvil-claude-watchdog)
(require 'anvil-fusion-verify)
(require 'anvil-memory)
(require 'anvil-semantic)
(require 'anvil-wl-imap)
(require 'anvil-wl-smtp)

(ert-deftest anvil-reader-escape-test-browser-cache-key-nul-separators ()
  "Browser cache keys contain NUL separators."
  (let ((nul (char-to-string 0)))
    (should
     (equal (concat "url" nul "selector" nul "profile" nul "agent")
            (anvil-browser--cache-key
             "url" "selector" '(:profile "profile" :user-agent "agent"))))))

(ert-deftest anvil-reader-escape-test-semantic-digest-nul-separators ()
  "Semantic digests hash NUL-delimited fields."
  (let ((nul (char-to-string 0)))
    (should
     (equal (secure-hash 'sha1 (concat "file" nul "head" nul "text"))
            (anvil-semantic--digest "file" "head" "text")))))

(ert-deftest anvil-reader-escape-test-fusion-cache-key-nul-separator ()
  "Fusion cache keys hash claim text and kind with a NUL separator."
  (let* ((claim '(:claim "claim text" :kind fact))
         (expected
          (secure-hash
           'sha1 (concat "claim text" (char-to-string 0) "fact"))))
    (should (equal expected (anvil-fusion-verify--cache-key claim)))))

(ert-deftest anvil-reader-escape-test-memory-frontmatter-stops-at-nul ()
  "Frontmatter scanning does not cross a NUL byte."
  (let ((body (concat "---\nname: before"
                      (char-to-string 0)
                      "\n---\nbody\n")))
    (should-not (anvil-memory--parse-frontmatter body))))

(ert-deftest anvil-reader-escape-test-watchdog-cmdline-nul-separators ()
  "Watchdog command lines replace procfs NUL separators with spaces."
  (let ((nul (char-to-string 0)))
    (cl-letf (((symbol-function 'anvil-claude-watchdog--read-file)
               (lambda (_path) (concat "claude" nul "--flag" nul))))
      (should (equal "claude --flag"
                     (anvil-claude-watchdog--proc-cmdline 1))))))

(ert-deftest anvil-reader-escape-test-imap-xoauth2-soh-separators ()
  "IMAP XOAUTH2 payloads contain SOH separators."
  (let* ((soh (char-to-string 1))
         (decoded
          (base64-decode-string
           (anvil-wl-imap--xoauth2-initial-response "user" "token"))))
    (should (equal (concat "user=user" soh "auth=Bearer token" soh soh)
                   decoded))))

(ert-deftest anvil-reader-escape-test-smtp-plain-nul-separators ()
  "SMTP AUTH PLAIN payloads contain leading and embedded NULs."
  (let (sent)
    (cl-letf (((symbol-function 'anvil-wl-smtp--expect)
               (lambda (_conn line _prefix)
                 (setq sent line)
                 t)))
      (anvil-wl-smtp-login 'conn "user" "pass"))
    (let* ((prefix "AUTH PLAIN ")
           (payload (base64-decode-string (substring sent (length prefix))))
           (nul (char-to-string 0)))
      (should (string-prefix-p prefix sent))
      (should (equal (concat nul "user" nul "pass") payload)))))

(provide 'anvil-reader-escape-test)
;;; anvil-reader-escape-test.el ends here
