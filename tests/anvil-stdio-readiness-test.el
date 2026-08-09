;;; anvil-stdio-readiness-test.el --- Headless readiness regression -*- lexical-binding: t; -*-

;;; Commentary:

;; Run the standalone bridge regression that proves exact nil readiness is
;; retried under a bounded deadline, malformed output and invalid settings fail
;; closed, and a daemon replacement cannot enter JSON-RPC, init, or stop
;; handlers after the final same-event readiness guard.  Large staged requests
;; must be gone before a live reply and after direct bridge termination; the
;; exact bounded helper process must terminate with its parent.

;;; Code:

(require 'ert)

(defconst anvil-stdio-readiness-test--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the stdio readiness regression helper.")

(ert-deftest anvil-stdio-readiness-is-final-and-atomic ()
  "Headless readiness and staging custody must remain final and bounded."
  (let* ((root
          (file-name-directory
           (directory-file-name anvil-stdio-readiness-test--directory)))
         (helper
          (expand-file-name
           "anvil-stdio-readiness-test.py"
           anvil-stdio-readiness-test--directory))
         (bridge (expand-file-name "anvil-stdio.sh" root))
         (python (executable-find "python3"))
         (emacs (expand-file-name invocation-name invocation-directory))
         (timeout (or (executable-find "timeout")
                      (executable-find "gtimeout")))
         (bashes
          (delete-dups
           (mapcar
            #'file-truename
            (delq nil
                  (list
                   (executable-find "bash")
                   (and (file-executable-p "/bin/bash") "/bin/bash")))))))
    (dolist (program (append (list python emacs timeout) bashes))
      (should program))
    (should (file-readable-p bridge))
    (dolist (bridge-bash bashes)
      (with-temp-buffer
        (let ((status
               (call-process
                timeout nil t nil
                "--kill-after=2" "45"
                python "-I" "-B" "-u"
                helper bridge bridge-bash emacs)))
          (unless (and (integerp status) (zerop status))
            (ert-fail
             (format
              "Readiness regression failed for %s (status %S):\n%s"
              bridge-bash status (buffer-string))))
          (should
           (string-match-p
            (concat "stdio-readiness-ok bash="
                    (regexp-quote bridge-bash))
            (buffer-string))))))))

(ert-deftest anvil-stdio-readiness-contract-is-explicit ()
  "Every bridge phase uses the configured exact readiness predicate."
  (let* ((root
          (file-name-directory
           (directory-file-name anvil-stdio-readiness-test--directory)))
         (bridge (expand-file-name "anvil-stdio.sh" root)))
    (with-temp-buffer
      (insert-file-contents bridge)
      (let ((source (buffer-string)))
        (should (= 3
                   (how-many
                    (regexp-quote
                     "-e \"$ANVIL_EMACSCLIENT_READY_EXPR\"")
                    (point-min) (point-max))))
        (should-not
         (string-match-p
          (regexp-quote "-e t)")
          source))
        (should
         (string-match-p
          (regexp-quote "PROBE-NOT-READY")
          source))
        (should
         (string-match-p
          (regexp-quote "ANVIL_MCP_NOT_READY_SENTINEL")
          source))))))

(provide 'anvil-stdio-readiness-test)

;;; anvil-stdio-readiness-test.el ends here
