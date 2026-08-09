;;; anvil-stdio-large-response-test.el --- Real large response E2E -*- lexical-binding: t; -*-

;;; Commentary:

;; Launch a fresh foreground Emacs daemon and drive the real stdio bridge plus
;; real emacsclient through an exact multibyte 3 MB JSON-RPC response after
;; deliberately losing its marker transport.  Daemon counters prove exactly
;; one handler call; the same bridge must then serve a second request, preserve
;; generation-bound private custody,
;; and remove the transaction on EOF.  Run under both macOS Bash 3.2 and the
;; PATH Bash when they differ.

;;; Code:

(require 'ert)

(defconst anvil-stdio-large-response-test--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the real-daemon large-response regression.")

(ert-deftest anvil-stdio-large-response-real-daemon-round-trip ()
  "A large response must survive real daemon and emacsclient transport."
  (let* ((root
          (file-name-directory
           (directory-file-name anvil-stdio-large-response-test--directory)))
         (helper
          (expand-file-name
           "anvil-stdio-large-response-test.py"
           anvil-stdio-large-response-test--directory))
         (bridge (expand-file-name "anvil-stdio.sh" root))
         (python (executable-find "python3"))
         (timeout (or (executable-find "timeout")
                      (executable-find "gtimeout")))
         (emacs (expand-file-name invocation-name invocation-directory))
         (emacsclient (expand-file-name "emacsclient" invocation-directory))
         (bashes
          (delete-dups
           (mapcar
            #'file-truename
            (delq nil
                  (list
                   (executable-find "bash")
                   (and (file-executable-p "/bin/bash")
                        "/bin/bash")))))))
    (dolist (program (append (list python timeout emacs emacsclient) bashes))
      (should (and program (file-executable-p program))))
    (should (file-readable-p bridge))
    (should (file-readable-p helper))
    (dolist (bridge-bash bashes)
      (with-temp-buffer
        (let ((status
               (call-process
                timeout nil t nil
                "--kill-after=3" "120"
                python "-I" "-B" "-u"
                helper bridge bridge-bash emacs emacsclient)))
          (unless (and (integerp status) (zerop status))
            (ert-fail
             (format
              "Real large-response regression failed for %s (status %S):\n%s"
              bridge-bash status (buffer-string))))
          (should
           (string-match-p
            (concat "stdio-large-response-ok bash="
                    (regexp-quote bridge-bash))
            (buffer-string))))))))

(provide 'anvil-stdio-large-response-test)

;;; anvil-stdio-large-response-test.el ends here
