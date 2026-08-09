;;; anvil-stdio-postdispatch-test.el --- Bridge response regression -*- lexical-binding: t; -*-

;;; Commentary:

;; Run the deterministic stdio post-dispatch regression under a hard outer
;; deadline.  The Python helper owns the negative control, same-pipe,
;; pre-dispatch loader freeze, at-most-once, UTF-8/uppercase framing,
;; partial-byte/NUL input, large Unicode staging, pipelined framing,
;; interrupted response-stage cleanup, authenticated final/proof custody,
;; concurrent cleanup snapshots,
;; oversized advisory markers,
;; guarded cumulative-frame-budget, truncated-frame,
;; guard-loader, runner/owner-death,
;; same-pipe recovery, debug-path race,
;; sink-FD, and process-group cleanup assertions.  Run both
;; the PATH Bash and macOS /bin/bash when they differ, because Bash 3.2 exposed
;; response and timeout bugs hidden by Bash 5.

;;; Code:

(require 'ert)
(require 'seq)

(defconst anvil-stdio-postdispatch-test--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the stdio post-dispatch regression helper.")

(ert-deftest anvil-stdio-postdispatch-response-path-is-process-free ()
  "A completed dispatch must not depend on another executable starting."
  (let* ((root
          (file-name-directory
           (directory-file-name anvil-stdio-postdispatch-test--directory)))
         (helper
          (expand-file-name
           "anvil-stdio-postdispatch-test.py"
           anvil-stdio-postdispatch-test--directory))
         (bridge (expand-file-name "anvil-stdio.sh" root))
         (python (executable-find "python3"))
         (timeout (or (executable-find "timeout")
                      (executable-find "gtimeout")))
         (bashes
          (delete-dups
           (mapcar
            #'file-truename
            (delq nil
                  (list
                   (executable-find "bash")
                   (and (file-executable-p "/bin/bash")
                        "/bin/bash")))))))
    (dolist (program (append (list python timeout) bashes))
      (should program))
    (should (file-readable-p bridge))
    (dolist (bridge-bash bashes)
      (with-temp-buffer
        (let ((status
               (call-process
                timeout nil t nil
                "--kill-after=2" "90"
                python "-I" "-B" "-u"
                helper bridge bridge-bash)))
          (unless (and (integerp status) (zerop status))
            (ert-fail
             (format
              "Post-dispatch regression failed for %s (status %S):\n%s"
              bridge-bash status (buffer-string))))
          (should
           (string-match-p
            (concat "stdio-postdispatch-ok bash="
                    (regexp-quote bridge-bash))
            (buffer-string))))))))

(ert-deftest anvil-stdio-postdispatch-transport-contract-is-explicit ()
  "The bridge keeps fail-closed editor and bounded lifecycle contracts."
  (let* ((root
          (file-name-directory
           (directory-file-name anvil-stdio-postdispatch-test--directory)))
         (bridge (expand-file-name "anvil-stdio.sh" root)))
    (with-temp-buffer
      (insert-file-contents bridge)
      (let ((source (buffer-string)))
        (should (string-match-p "unset ALTERNATE_EDITOR" source))
        (should (= 1
                   (how-many
                    (regexp-quote
                     "-t \"$ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT\"")
                    (point-min) (point-max))))
        (should
         (string-match-p
          (regexp-quote
           "local grace=\"${ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT:-1}\"")
          source))
        (should (= 2
                   (how-many
                    (regexp-quote "-t \"$grace\"")
                    (point-min) (point-max))))
        (should (= 2
                   (how-many
                    (regexp-quote
                     "anvil_mcp_converge_runner \"$runner\"")
                    (point-min) (point-max))))
        (should
         (string-match-p
          (regexp-quote
           "ANVIL_MCP_REQUEST_PARSE_TIMEOUT=${ANVIL_MCP_REQUEST_PARSE_TIMEOUT:-10}")
          source))
        (should
         (string-match-p
          (regexp-quote "\"$ANVIL_MCP_REQUEST_PARSE_TIMEOUT\" 10")
          source))
        (should
         (string-match-p
          (regexp-quote "readonly ANVIL_MCP_INLINE_REQUEST_BYTES=16384")
          source))
        (should (string-match-p "anvil_mcp_stage_request()" source))
        (should
         (string-match-p
          (regexp-quote
           "_anvil_response_basename=\"response.$ANVIL_MCP_REQUEST_SEQUENCE.json\"")
          source))
        (should (string-match-p "anvil_mcp_prepare_response()" source))
        (should
         (string-match-p
          (regexp-quote
           "_anvil_response_proof_path=\"$ANVIL_MCP_TRANSACTION_DIRECTORY/proof.$ANVIL_MCP_REQUEST_SEQUENCE.json\"")
          source))
        (should
         (string-match-p
          (regexp-quote "exec 6<\"$_anvil_response_stage_path\"")
          source))
        (should
         (string-match-p "anvil_mcp_validate_response_fd()" source))
        (should
         (string-match-p "anvil_mcp_read_staged_response" source))
        (should
         (string-match-p
          (regexp-quote "exec 6<&- 7<&- 9>&-")
          source))
        (should (string-match-p "anvil_mcp_retire_staged_request()" source))
        (should-not
         (string-match-p
          (regexp-quote ": >|\"$_anvil_request_path\"")
          source))
        (should-not
         (string-match-p "anvil_mcp_reset_response_placeholder" source))
        (should-not (string-match-p "ANVIL_MCP_HEAD" source))
        (should
         (string-match-p
          (regexp-quote "mcp_debug_log \"INIT-RC\" \"$INIT_RC\"")
          source))
        (should
         (string-match-p
          (regexp-quote "mcp_debug_log \"STOP-RC\" \"$STOP_RC\"")
          source))))))

(provide 'anvil-stdio-postdispatch-test)

;;; anvil-stdio-postdispatch-test.el ends here
