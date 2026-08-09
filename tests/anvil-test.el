;;; anvil-test.el --- Tests for anvil.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic smoke tests for the anvil package.

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil)
(require 'anvil-server)
(require 'anvil-server-commands)
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

(ert-deftest anvil-test-server-status-command ()
  "`anvil-server-status' is bound, interactive, and reports the
running flag in the format the README documents (`Running' /
`Stopped')."
  (should (fboundp 'anvil-server-status))
  (should (commandp 'anvil-server-status))
  (let ((anvil-server--running t))
    (should (string-match-p "Running"
                            (let ((inhibit-message t))
                              (anvil-server-status)))))
  (let ((anvil-server--running nil))
    (should (string-match-p "Stopped"
                            (let ((inhibit-message t))
                              (anvil-server-status))))))

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

(ert-deftest anvil-test-schema-cache-reuses-identical-schema-and-fragment ()
  "A warm schema cache must skip schema and fragment generation."
  (defun anvil-test--schema-cache-tool (path &optional mode)
    "Read PATH using MODE.

MCP Parameters:
  path - File path to read
  mode - Optional read mode"
    (list path mode))
  (let* ((id "anvil-test-schema-cache")
         (description "Read a file for schema cache testing")
         (server-id "anvil-test-schema-cache-server")
         (cache-file (make-temp-file "anvil-schema-cache-test-"))
         (arglist (help-function-arglist
                   'anvil-test--schema-cache-tool t))
         (fresh-schema
          (anvil-server--generate-schema-from-function
           'anvil-test--schema-cache-tool arglist))
         (fresh-fragment
          (anvil-server--build-tool-fragment
           id description fresh-schema)))
    (unwind-protect
        (let ((anvil-server-schema-cache-file cache-file)
              (anvil-server--schema-cache nil)
              (anvil-server--schema-cache-loaded nil))
          (anvil-server-unregister-tool id server-id)
          (anvil-server-register-tool
           #'anvil-test--schema-cache-tool
           :id id
           :description description
           :server-id server-id)
          (let* ((tools-table (anvil-server--get-server-tools server-id))
                 (tool (gethash id tools-table)))
            (should (equal fresh-schema (plist-get tool :schema)))
            (should (equal fresh-fragment (plist-get tool :json-fragment))))
          (anvil-server-unregister-tool id server-id)
          ;; Force the next registration to reload the on-disk cache.
          (setq anvil-server--schema-cache nil)
          (setq anvil-server--schema-cache-loaded nil)
          (let ((schema-calls 0)
                (fragment-calls 0))
            (cl-letf (((symbol-function
                        'anvil-server--generate-schema-from-function)
                       (lambda (&rest _args)
                         (setq schema-calls (1+ schema-calls))
                         (error "schema generation was not skipped")))
                      ((symbol-function
                        'anvil-server--build-tool-fragment)
                       (lambda (&rest _args)
                         (setq fragment-calls (1+ fragment-calls))
                         (error "fragment generation was not skipped"))))
              (anvil-server-register-tool
               #'anvil-test--schema-cache-tool
               :id id
               :description description
               :server-id server-id))
            (let* ((tools-table (anvil-server--get-server-tools server-id))
                   (tool (gethash id tools-table)))
              (should (= 0 schema-calls))
              (should (= 0 fragment-calls))
              (should (equal fresh-schema (plist-get tool :schema)))
              (should (equal fresh-fragment
                             (plist-get tool :json-fragment))))))
      (anvil-server-unregister-tool id server-id)
      (when (file-exists-p cache-file)
        (delete-file cache-file)))))

(ert-deftest anvil-test-schema-cache-signature-does-not-read-docstring ()
  "Signature construction must be stable before cache lookup."
  (cl-letf (((symbol-function 'documentation)
             (lambda (&rest _args)
               (error "documentation must not be called"))))
    (should (string-match-p
             ":doc nil"
             (anvil-server--schema-cache-signature
              'anvil-test--schema-cache-tool
              '(path &optional mode)
              "Read a file for schema cache testing"
              nil)))))

(ert-deftest anvil-test-schema-cache-registers-lazy-tool-fragments ()
  "Cached fragments can advertise tools before their modules are loaded."
  (let* ((server-id "anvil-test-lazy-fragments")
         (cache-file (make-temp-file "anvil-lazy-fragments-"))
         (fragment "{\"name\":\"lazy-tool\",\"description\":\"Lazy\",\"inputSchema\":{\"type\":\"object\"}}")
         (loader-called nil))
    (unwind-protect
        (progn
          (write-region
           (concat
            "(setq anvil-server--schema-cache-file-data '"
            (prin1-to-string
             (list :version anvil-server--schema-cache-version
                   :entries
                   (list
                    (list :id "lazy-tool"
                          :signature "sig"
                          :schema '((type . "object"))
                          :fragment fragment))))
            ")\n")
           nil cache-file)
          (let ((anvil-server-schema-cache-file cache-file)
                (anvil-server--schema-cache nil)
                (anvil-server--schema-cache-loaded nil))
            (should
             (= 1
                (anvil-server-register-cached-tool-fragments
                 server-id
                 `(("lazy-tool" .
                    ,(lambda (&rest _)
                       (setq loader-called t)))))))
            (let* ((tools-table (anvil-server--get-server-tools server-id))
                   (tool (gethash "lazy-tool" tools-table)))
              (should (plist-get tool :lazy-placeholder))
              (should (equal fragment (plist-get tool :json-fragment)))
              (should-not loader-called))))
      (remhash server-id anvil-server--tools)
      (when (file-exists-p cache-file)
        (delete-file cache-file)))))

(ert-deftest anvil-test-lazy-placeholder-is-replaced-by-real-registration ()
  "Loading a module may replace its cached placeholder with a real tool."
  (defun anvil-test--lazy-real-tool ()
    "Return ok."
    "ok")
  (let* ((server-id "anvil-test-lazy-replace")
         (tools-table (anvil-server--get-server-tools server-id)))
    (unwind-protect
        (progn
          (puthash "lazy-real"
                   (list :id "lazy-real"
                         :json-fragment "{}"
                         :lazy-placeholder t)
                   tools-table)
          (anvil-server-register-tool
           #'anvil-test--lazy-real-tool
           :id "lazy-real"
           :description "Real lazy tool"
           :server-id server-id)
          (let ((tool (gethash "lazy-real" tools-table)))
            (should-not (plist-get tool :lazy-placeholder))
            (should (eq 'anvil-test--lazy-real-tool
                        (plist-get tool :handler)))))
      (remhash server-id anvil-server--tools))))

(ert-deftest anvil-test-tools-call-loads-lazy-placeholder ()
  "tools/call loads a lazy placeholder before dispatching."
  (defun anvil-test--lazy-call-tool ()
    "Return ok."
    "ok")
  (let* ((server-id "anvil-test-lazy-call")
         (tools-table (anvil-server--get-server-tools server-id))
         (loader-called nil))
    (unwind-protect
        (progn
          (puthash "lazy-call"
                   (list :id "lazy-call"
                         :json-fragment "{}"
                         :lazy-placeholder t
                         :lazy-loader
                         (lambda (&rest _)
                           (setq loader-called t)
                           (anvil-server-register-tool
                            #'anvil-test--lazy-call-tool
                            :id "lazy-call"
                            :description "Lazy call tool"
                            :server-id server-id)))
                   tools-table)
          (let* ((resp
                  (anvil-server--handle-tools-call
                   "lazy-call-id"
                   '((name . "lazy-call") (arguments . ()))
                   (make-anvil-server-metrics)
                   server-id))
                 (decoded (json-read-from-string resp))
                 (result (alist-get 'result decoded))
                 (content (alist-get 'content result))
                 (text (alist-get 'text (aref content 0))))
            (should loader-called)
            (should (equal "ok" text))))
      (remhash server-id anvil-server--tools))))

(ert-deftest anvil-test-scan-int-after-tolerates-whitespace ()
  "Standalone scanner must accept normal JSON whitespace before numbers."
  (should (equal 42 (anvil-server--scan-int-after "{\"id\": 42,}" "\"id\":")))
  (should (equal 42 (anvil-server--scan-int-after "{\"id\":42,}" "\"id\":")))
  (should (equal 7 (anvil-server--scan-int-after "{\"id\":\t7 }" "\"id\":")))
  (should (equal -3 (anvil-server--scan-int-after "{\"id\": -3 }" "\"id\":"))))

(ert-deftest anvil-test-scan-string-after-tolerates-whitespace ()
  "Standalone scanner must accept normal JSON whitespace before strings."
  (should (equal "tools/list"
                 (anvil-server--scan-string-after
                  "{\"method\": \"tools/list\"}" "\"method\":")))
  (should (equal "tools/list"
                 (anvil-server--scan-string-after
                  "{\"method\":\"tools/list\"}" "\"method\":")))
  (should (equal "a\"b\\c"
                 (anvil-server--scan-string-after
                  "{\"name\": \"a\\\"b\\\\c\"}" "\"name\":"))))

(ert-deftest anvil-test-scan-json-value-after-tolerates-string-id ()
  "JSON-RPC ids may be strings as well as numbers."
  (should (equal "abc"
                 (anvil-server--scan-json-value-after
                  "{\"id\": \"abc\"}" "\"id\":")))
  (should (equal 17
                 (anvil-server--scan-json-value-after
                  "{\"id\": 17}" "\"id\":"))))

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

(ert-deftest anvil-test-dispatch-converts-quit-to-tool-error ()
  "A wrapped tool must turn `quit' into an MCP tool error result."
  (defun anvil-test--tool-quit (_args)
    "Signal quit through the helper wrapper.

MCP Parameters:
  (none)"
    (anvil-server-with-error-handling
      (signal 'quit '(minibuffer-quit))))
  (unwind-protect
      (progn
        (anvil-server-register-tool
         #'anvil-test--tool-quit
         :id "anvil-test-tool-quit"
         :description "test"
         :server-id "anvil-test")
          (let* ((params '((name . "anvil-test-tool-quit")
                         (arguments . ())))
               (resp (anvil-server--handle-tools-call
                      "t-quit" params
                      (make-anvil-server-metrics) "anvil-test"))
               (decoded (json-read-from-string resp))
               (result (alist-get 'result decoded))
               (text (alist-get 'text
                                (aref (alist-get 'content result) 0))))
          (should (eq t (alist-get 'isError result)))
          (should (string-match-p "Interrupted (quit)" text))
          (should (string-match-p "C-g" text))))
    (anvil-server-unregister-tool "anvil-test-tool-quit" "anvil-test")))

(ert-deftest anvil-test-process-jsonrpc-catches-raw-quit ()
  "Top-level JSON-RPC dispatch must serialize uncaught `quit' as JSON."
  (defun anvil-test--raw-quit (_args)
    "Signal quit without wrapping.

MCP Parameters:
  (none)"
    (signal 'quit '(minibuffer-quit)))
  (unwind-protect
      (progn
        (anvil-server-register-tool
         #'anvil-test--raw-quit
         :id "anvil-test-raw-quit"
         :description "test"
         :server-id "anvil-test")
        (let* ((anvil-server--running t)
               (request
                (anvil-server-create-tools-call-request
                 "anvil-test-raw-quit" 77 nil))
               (resp (anvil-server-process-jsonrpc request "anvil-test"))
               (decoded (json-read-from-string resp))
               (rpc-error (alist-get 'error decoded)))
          (should rpc-error)
          (should (= anvil-server-jsonrpc-error-internal
                     (alist-get 'code rpc-error)))
          (should (string-match-p "Quit"
                                  (alist-get 'message rpc-error)))))
    (anvil-server-unregister-tool "anvil-test-raw-quit" "anvil-test")))

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

;;;; --- encoded registration wrappers -------------------------------------

(ert-deftest anvil-test-register-tool-normalizes-encoded-handler ()
  "`anvil-server-register-tool' must introspect wrapped tools via the raw handler."
  (defun anvil-test--wrapped-schema-tool (path &optional mode)
    "Return PATH and MODE in a plist.

MCP Parameters:
  path - Absolute path to inspect.
  mode - Optional mode string."
    (list :path path :mode mode))
  (let ((tool-id "anvil-test-wrapped-schema")
        (server-id "anvil-test")
        (wrapped
         (anvil-server-encode-handler #'anvil-test--wrapped-schema-tool)))
    (unwind-protect
        (progn
          (anvil-server-register-tool
           wrapped
           :id tool-id
           :description "wrapped schema test"
           :server-id server-id)
          (let* ((tool (gethash tool-id
                                (anvil-server--get-server-tools server-id)))
                 (schema (plist-get tool :schema))
                 (props (alist-get 'properties schema))
                 (required (alist-get 'required schema)))
            (should (eq 'anvil-test--wrapped-schema-tool
                        (plist-get tool :handler)))
            (should (equal '(path &optional mode)
                           (plist-get tool :arglist)))
            (should (plist-get tool :encode-result))
            (should (assoc "path" props))
            (should (assoc "mode" props))
            (should (equal ["path"] required))))
      (anvil-server-unregister-tool tool-id server-id))))

(ert-deftest anvil-test-dispatch-encodes-wrapped-handler-result ()
  "`tools/call' must validate args against the raw signature, then encode the result."
  (defun anvil-test--wrapped-dispatch-tool (task_id &optional mode)
    "Echo TASK_ID and MODE as a plist.

MCP Parameters:
  task_id - Task identifier string.
  mode - Optional execution mode string."
    (list :task_id task_id :mode mode))
  (let ((tool-id "anvil-test-wrapped-dispatch")
        (server-id "anvil-test")
        (wrapped
         (anvil-server-encode-handler #'anvil-test--wrapped-dispatch-tool)))
    (unwind-protect
        (progn
          (anvil-server-register-tool
           wrapped
           :id tool-id
           :description "wrapped dispatch test"
           :server-id server-id)
          (let* ((params `((name . ,tool-id)
                           (arguments . ((task_id . "task-7")
                                         (mode . "fast")))))
                 (resp (anvil-server--handle-tools-call
                        "t-wrapped" params
                        (make-anvil-server-metrics) server-id))
                 (decoded (json-read-from-string resp))
                 (result (alist-get 'result decoded))
                 (text (alist-get 'text
                                  (aref (alist-get 'content result) 0)))
                 (payload (json-parse-string text :object-type 'plist)))
            (should (eq :json-false (alist-get 'isError result)))
            (should (stringp text))
            (should (equal "task-7" (plist-get payload :task_id)))
            (should (equal "fast" (plist-get payload :mode)))))
      (anvil-server-unregister-tool tool-id server-id))))

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
           ;; macOS GitHub Actions runners can spend ~400ms just
           ;; dispatching an IPC round-trip; a 0.4s budget races
           ;; with that overhead and the kill misses, leaving the
           ;; second call landing on the SAME slot (PID equal).
           ;; Stub sleeps 30s so any value well under that still
           ;; exercises the budget-exceed path.
           :offload-timeout 2.0)
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


;;;; --- bundle: anvil-server-encode-handler / --to-json-value ------------

(defun anvil-test--encode-handler-fixture-handler (a &optional b)
  "Fixture returning a plist with a dotted-pair alist inside.

MCP Parameters:
  a - First argument (string).
  b - Optional second argument (string)."
  (list :a a :b (or b "default")
        :matrix (list (cons "k1" 1.0) (cons "k2" 2.0))))

(ert-deftest anvil-test-encode-handler-exposes-raw-handler ()
  "PR #12 switched `anvil-server-encode-handler' to a symbol-backed
wrapper that records the raw handler on the `anvil-server-raw-handler'
symbol property and flags `anvil-server-encode-result' t.  Registration
normalizes via those properties, so schema generation sees the raw
handler's arglist even though the wrapper itself is an
`apply-partially' closure with `(&rest args)' shape."
  (let ((wrapped (anvil-server-encode-handler
                  #'anvil-test--encode-handler-fixture-handler)))
    (should (symbolp wrapped))
    (should (eq #'anvil-test--encode-handler-fixture-handler
                (get wrapped 'anvil-server-raw-handler)))
    (should (get wrapped 'anvil-server-encode-result))
    ;; Schema generation from the RAW handler (via the normalize helper)
    ;; must yield the expected arg shape.
    (let* ((meta (anvil-server--normalize-tool-handler wrapped))
           (raw (plist-get meta :handler)))
      (should (eq raw #'anvil-test--encode-handler-fixture-handler))
      (should (equal '(a &optional b) (plist-get meta :arglist)))
      (should (anvil-server--generate-schema-from-function raw)))))

(ert-deftest anvil-test-encode-handler-registers-with-register-tool ()
  "Wrapped handler must pass the full register-tool path (schema gen)."
  (let ((wrapped (anvil-server-encode-handler
                  #'anvil-test--encode-handler-fixture-handler)))
    (unwind-protect
        (progn
          (anvil-server-register-tool
           wrapped
           :id "anvil-test-encode-handler-tool"
           :server-id "anvil-test"
           :description "encode-handler round-trip fixture")
          (should (member "anvil-test-encode-handler-tool"
                          (anvil-test-fixtures-registered-tool-ids
                           "anvil-test"))))
      (ignore-errors
        (anvil-server-unregister-tool "anvil-test-encode-handler-tool"
                                      "anvil-test")))))

(ert-deftest anvil-test-to-json-value-handles-dotted-pair ()
  "`anvil-server--to-json-value' must emit [car, cdr] for dotted pairs
that slip through plist detection (alist entries), otherwise
`mapcar' on the improper list crashes with listp error."
  (let* ((out (anvil-server--to-json-value (cons "k" 1.0))))
    (should (vectorp out))
    (should (equal "k" (aref out 0)))
    (should (equal 1.0 (aref out 1))))
  ;; Via encode-for-mcp + round-trip to JSON.
  (let* ((result (anvil-test--encode-handler-fixture-handler "hi"))
         (json (anvil-server-encode-for-mcp result))
         (parsed (let ((json-object-type 'alist)
                       (json-array-type 'list))
                   (json-read-from-string json))))
    (should (stringp json))
    (should (equal "hi" (cdr (assq 'a parsed))))
    (should (equal "default" (cdr (assq 'b parsed))))
    (should (listp (cdr (assq 'matrix parsed))))))

;;;; --- bounded inline tool results --------------------------------------

(defvar anvil-test--inline-payload nil)

(defun anvil-test--inline-payload-tool ()
  "Return the current inline-boundary fixture payload."
  anvil-test--inline-payload)

(defun anvil-test--inline-macro-error-tool ()
  "Signal the current fixture payload through the public error wrapper."
  (anvil-server-with-error-handling
    (error "%s" anvil-test--inline-payload)))

(defun anvil-test--inline-direct-error-tool ()
  "Signal the current fixture payload as an explicit MCP tool error."
  (anvil-server-tool-throw anvil-test--inline-payload))

(defun anvil-test--inline-generic-error-tool ()
  "Signal the current fixture payload as an ordinary error."
  (error "%s" anvil-test--inline-payload))

(defun anvil-test--inline-wrong-type-tool ()
  "Signal an allowlisted condition with request-owned non-string data."
  (signal 'wrong-type-argument
          (list 'numberp (list anvil-test--inline-payload))))

(defun anvil-test--inline-quit-tool ()
  "Signal quit with the current fixture payload."
  (signal 'quit (list anvil-test--inline-payload)))

(defun anvil-test--inline-spoof-overflow-tool ()
  "Spoof the internal overflow condition with the fixture payload."
  (signal 'anvil-server-inline-result-too-large
          (list anvil-test--inline-payload)))

(defun anvil-test--inline-token-aware-spoof-overflow-tool ()
  "Spoof the overflow condition using any published marker or a lookalike."
  (signal 'anvil-server-inline-result-too-large
          (list (if (boundp 'anvil-server--inline-result-overflow-token)
                    (symbol-value
                     'anvil-server--inline-result-overflow-token)
                  (make-symbol "anvil-server-inline-result-overflow"))
                anvil-test--inline-payload)))

(defun anvil-test--inline-property-error-tool ()
  "Signal an ordinary error whose text carries fixture properties."
  (signal 'error (list anvil-test--inline-payload)))

(defun anvil-test--response-text (response)
  "Return the first MCP text item from decoded RESPONSE."
  (let* ((decoded (json-read-from-string response))
         (result (alist-get 'result decoded))
         (content (and result (alist-get 'content result))))
    (and content (alist-get 'text (aref content 0)))))

(defun anvil-test--response-error-code (response)
  "Return the JSON-RPC error code from RESPONSE, or nil."
  (alist-get 'code
             (alist-get 'error (json-read-from-string response))))

(ert-deftest anvil-test-inline-result-limit-end-to-end ()
  "Oversized results fail before disclosure, metrics, hooks, or encoding."
  (let* ((server-id "anvil-test-inline-boundary")
         (tool-id "inline.escape-heavy")
         (anvil-test--inline-payload
          (concat "INLINE-SENTINEL-" (make-string 64 ?\")))
         (anvil-server-max-inline-result-bytes 32)
         (anvil-server--running t)
         (hook-values nil)
         (payload-values nil))
    (unwind-protect
        (progn
          (anvil-server-register-tool
           #'anvil-test--inline-payload-tool
           :id tool-id :description "inline fixture" :server-id server-id)
          (let ((anvil-server-tool-error-hook
                 (list (lambda (&rest values) (push values hook-values)))))
            (cl-letf (((symbol-function
                        'anvil-server-metrics--track-tool-payload)
                       (lambda (_tool _args response)
                         (push response payload-values)))
                      ((symbol-function 'anvil-disclosure-budget-apply)
                       (lambda (&rest _)
                         (ert-fail "oversized raw result reached disclosure"))))
              (let* ((request
                      (json-encode
                       `((jsonrpc . "2.0") (id . 91)
                         (method . "tools/call")
                         (params . ((name . ,tool-id)
                                    (arguments . ()))))))
                     (response
                      (anvil-server-process-jsonrpc request server-id))
                     (decoded (json-read-from-string response))
                     (result (alist-get 'result decoded))
                     (text (anvil-test--response-text response)))
                (should (eq t (alist-get 'isError result)))
                (should
                 (equal
                  (concat
                   "Tool inline.escape-heavy result exceeds the inline "
                   "response limit (raw-bytes=80 "
                   "projected-bytes-at-least=34 limit=32). Use a paginated, "
                   "filtered, tee, or asynchronous interface.")
                  text))
                (should (string-match-p "inline response limit" text))
                (should (string-match-p "limit=32" text))
                (should-not (string-match-p "INLINE-SENTINEL" response))
                (should-not hook-values)
                (should-not payload-values))))
          ;; A disclosure transform is an untrusted expansion seam and gets
          ;; the same guard before payload metrics or MCP wrapping.
          (setq anvil-test--inline-payload "safe")
          (cl-letf (((symbol-function 'anvil-disclosure-budget-apply)
                     (lambda (_tool _text)
                       (concat "DISCLOSURE-SENTINEL-"
                               (make-string 64 ?\\))))
                    ((symbol-function
                      'anvil-server-metrics--track-tool-payload)
                     (lambda (&rest _)
                       (ert-fail "expanded disclosure reached metrics"))))
            (let ((response
                   (anvil-server--handle-tools-call
                    92 `((name . ,tool-id) (arguments . ()))
                    (make-anvil-server-metrics) server-id)))
              (should
               (equal
                (concat
                 "Tool inline.escape-heavy result exceeds the inline "
                 "response limit (raw-bytes=84 "
                 "projected-bytes-at-least=34 limit=32). Use a paginated, "
                 "filtered, tee, or asynchronous interface.")
                (anvil-test--response-text response)))
              (should (string-match-p "inline response limit" response))
              (should-not (string-match-p "DISCLOSURE-SENTINEL" response)))))
      (ignore-errors (anvil-server-unregister-tool tool-id server-id)))))

(ert-deftest anvil-test-inline-result-limit-error-paths ()
  "Tool-derived error cells and labels are reconstructed before hooks."
  (let* ((server-id "anvil-test-inline-errors")
         (tool-id "inline.macro-error")
         (anvil-test--inline-payload
          (concat "ERROR-SENTINEL-" (make-string 80 ?x)))
         (anvil-server-max-inline-result-bytes 32)
         (anvil-server--running t)
         (hook-values nil))
    (unwind-protect
        (progn
          (anvil-server-register-tool
           #'anvil-test--inline-macro-error-tool
           :id tool-id :description "error fixture" :server-id server-id)
          (anvil-server-register-tool
           #'anvil-test--inline-direct-error-tool
           :id "inline.direct-error" :description "error fixture"
           :server-id server-id)
          (anvil-server-register-tool
           #'anvil-test--inline-generic-error-tool
           :id "inline.generic-error" :description "error fixture"
           :server-id server-id)
          (anvil-server-register-tool
           #'anvil-test--inline-wrong-type-tool
           :id "inline.wrong-type" :description "error fixture"
           :server-id server-id)
          (anvil-server-register-tool
           #'anvil-test--inline-quit-tool
           :id "inline.quit" :description "error fixture"
           :server-id server-id)
          ;; A macro-wrapped generic failure hooks exactly once and is then
          ;; transported as an MCP isError result without the rejected text.
          (let ((anvil-server-tool-error-hook
                 (list (lambda (&rest values) (push values hook-values)))))
            (let* ((response
                    (anvil-server--handle-tools-call
                     93 `((name . ,tool-id) (arguments . ()))
                     (make-anvil-server-metrics) server-id))
                   (text (anvil-test--response-text response)))
              (should (string-match-p "inline response limit" text))
              (should-not (string-match-p "ERROR-SENTINEL" response))
              (should (= 1 (length hook-values)))
              (pcase-let ((`(,condition ,label ,source)
                           (car hook-values)))
                (should (consp condition))
                (should (eq 'error (car condition)))
                (should (equal tool-id label))
                (should (eq 'tool-body source))
                (should-not
                 (equal (cadr condition) anvil-test--inline-payload)))))
          ;; Direct tool errors remain MCP isError results and are unhooked.
          (setq hook-values nil)
          (let ((anvil-server-tool-error-hook
                 (list (lambda (&rest values) (push values hook-values)))))
            (let ((response
                   (anvil-server--handle-tools-call
                    94 '((name . "inline.direct-error") (arguments . ()))
                    (make-anvil-server-metrics) server-id)))
              (should (string-match-p "inline response limit" response))
              (should-not (string-match-p "ERROR-SENTINEL" response))
              (should-not hook-values)))
          ;; Dispatcher generic errors keep the -32603 envelope and hook once.
          (setq hook-values nil)
          (let ((anvil-server-tool-error-hook
                 (list (lambda (&rest values) (push values hook-values)))))
            (let ((response
                   (anvil-server--handle-tools-call
                    95 '((name . "inline.generic-error") (arguments . ()))
                    (make-anvil-server-metrics) server-id)))
              (should (= anvil-server-jsonrpc-error-internal
                         (anvil-test--response-error-code response)))
              (should-not (string-match-p "ERROR-SENTINEL" response))
              (should (= 1 (length hook-values)))))
          ;; Quit keeps the -32603 envelope and does not run the error hook.
          (setq hook-values nil)
          (let ((anvil-server-tool-error-hook
                 (list (lambda (&rest values) (push values hook-values)))))
            (let ((response
                   (anvil-server--handle-tools-call
                    96 '((name . "inline.quit") (arguments . ()))
                    (make-anvil-server-metrics) server-id)))
              (should (= anvil-server-jsonrpc-error-internal
                         (anvil-test--response-error-code response)))
              (should-not (string-match-p "ERROR-SENTINEL" response))
              (should-not hook-values)))
          ;; Malformed params are contained by the real JSON-RPC path, hook
          ;; exactly once, retain -32602, and never reach the outer formatter.
          (setq hook-values nil)
          (cl-letf (((symbol-function 'anvil-server--handle-error)
                     (lambda (&rest _)
                       (ert-fail "tool error escaped to outer handler"))))
            (let ((anvil-server-tool-error-hook
                   (list (lambda (&rest values) (push values hook-values)))))
              (let* ((request
                      (json-encode
                       `((jsonrpc . "2.0") (id . 97)
                         (method . "tools/call")
                         (params . [,(concat "PARAMS-SENTINEL-"
                                             (make-string 80 ?p))]))))
                     (response
                      (anvil-server-process-jsonrpc request server-id)))
                (should (= anvil-server-jsonrpc-error-invalid-params
                           (anvil-test--response-error-code response)))
                (should-not (string-match-p "PARAMS-SENTINEL" response))
                (should (= 1 (length hook-values))))))
          ;; An oversized unexpected-parameter name is sanitized after the
          ;; dispatcher's inner validation and retains -32602 plus one hook.
          (setq hook-values nil)
          (let* ((unexpected
                  (intern (concat "UNEXPECTED-SENTINEL-"
                                  (make-string 80 ?u))))
                 (params
                  `((name . "inline.macro-error")
                    (arguments . ((,unexpected . "value"))))))
            (let ((anvil-server-tool-error-hook
                   (list (lambda (&rest values) (push values hook-values)))))
              (let ((response
                     (anvil-server--handle-tools-call
                      951 params (make-anvil-server-metrics) server-id)))
                (should (= anvil-server-jsonrpc-error-invalid-params
                           (anvil-test--response-error-code response)))
                (should-not
                 (string-match-p "UNEXPECTED-SENTINEL" response))
                (should (= 1 (length hook-values))))))
          ;; Lazy loader failures are covered by the same outer boundary.
          (let ((table (anvil-server--get-server-tools server-id)))
            (puthash
             "inline.lazy-generic"
             (list :id "inline.lazy-generic" :json-fragment "{}"
                   :lazy-placeholder t
                   :lazy-loader (lambda (&rest _)
                                  (error "%s" anvil-test--inline-payload)))
             table)
            (puthash
             "inline.lazy-tool-error"
             (list :id "inline.lazy-tool-error" :json-fragment "{}"
                   :lazy-placeholder t
                   :lazy-loader (lambda (&rest _)
                                  (anvil-server-tool-throw
                                   anvil-test--inline-payload)))
             table))
          (setq hook-values nil)
          (let ((anvil-server-tool-error-hook
                 (list (lambda (&rest values) (push values hook-values)))))
            (let ((response
                   (anvil-server--handle-tools-call
                    98 '((name . "inline.lazy-generic") (arguments . ()))
                    (make-anvil-server-metrics) server-id)))
              (should (= anvil-server-jsonrpc-error-internal
                         (anvil-test--response-error-code response)))
              (should-not (string-match-p "ERROR-SENTINEL" response))
              (should (= 1 (length hook-values)))))
          (setq hook-values nil)
          (let ((anvil-server-tool-error-hook
                 (list (lambda (&rest values) (push values hook-values)))))
            (let ((response
                   (anvil-server--handle-tools-call
                    99 '((name . "inline.lazy-tool-error") (arguments . ()))
                    (make-anvil-server-metrics) server-id)))
              (should (string-match-p "inline response limit" response))
              (should-not (string-match-p "ERROR-SENTINEL" response))
              (should-not hook-values)))
          ;; Lookup misses use only the placeholder and stay unhooked.
          (setq hook-values nil)
          (let ((anvil-server-tool-error-hook
                 (list (lambda (&rest values) (push values hook-values)))))
            (let ((response
                   (anvil-server--handle-tools-call
                    100
                    '((name . "MISSING SENTINEL unsafe!") (arguments . ()))
                    (make-anvil-server-metrics) server-id)))
              (should (= anvil-server-jsonrpc-error-invalid-request
                         (anvil-test--response-error-code response)))
              (should (string-match-p "<oversized-tool-id>" response))
              (should-not (string-match-p "MISSING SENTINEL" response))
              (should-not hook-values)))
          ;; The production harness hook receives the sanitized symbol and
          ;; label and retains its classifier behavior at the final recorder.
          (require 'anvil-harness-telemetry)
          (let (recorded)
            (cl-letf (((symbol-function 'anvil-harness-telemetry-record)
                       (lambda (class &rest keys)
                         (setq recorded (cons class keys))
                         '(:id 1))))
              (let ((anvil-server-tool-error-hook
                     '(anvil-harness-telemetry--dispatcher-hook)))
                (let ((response
                       (anvil-server--handle-tools-call
                        101 '((name . "inline.wrong-type") (arguments . ()))
                        (make-anvil-server-metrics) server-id)))
                  (should (= anvil-server-jsonrpc-error-internal
                             (anvil-test--response-error-code response)))
                  (should-not (string-match-p "ERROR-SENTINEL" response)))))
            (should (eq 'no-exec (car recorded)))
            (should (equal "inline.wrong-type"
                           (plist-get (cdr recorded) :tool)))
            (should-not (string-match-p
                         "ERROR-SENTINEL" (format "%S" recorded))))
          ;; Unknown condition names are canonicalized before persistence.
          (let* ((sentinel-symbol
                  (intern "request-owned-condition-ERROR-SENTINEL"))
                 (sanitized
                  (anvil-server--sanitize-tool-error
                   "unsafe tool name!" 'generic
                   (list sentinel-symbol anvil-test--inline-payload))))
            (should (eq 'error (car (plist-get sanitized :condition))))
            (should (equal "<oversized-tool-id>"
                           (plist-get sanitized :tool)))
            (should-not (string-match-p
                         "ERROR-SENTINEL" (plist-get sanitized :text)))))
      (dolist (id (list tool-id "inline.direct-error" "inline.generic-error"
                        "inline.wrong-type" "inline.quit"
                        "inline.lazy-generic" "inline.lazy-tool-error"))
        (ignore-errors (anvil-server-unregister-tool id server-id))))))

(ert-deftest anvil-test-inline-result-limit-boundaries ()
  "Projected sizes match Emacs JSON escaping without using the encoder."
  (let* ((controls (apply #'string (number-sequence 0 31)))
         (unibyte-high
          (apply #'unibyte-string (number-sequence #x80 #xff)))
         (cases (list "plain" "\"\\" controls "éλ中" "😀" unibyte-high))
         (oracles
          (mapcar
           (lambda (text)
             (- (string-bytes (json-encode-string text)) 2))
           cases)))
    (cl-letf (((symbol-function 'json-encode-string)
               (lambda (&rest _) (ert-fail "projector used json encoder")))
              ((symbol-function 'json-encode)
               (lambda (&rest _) (ert-fail "projector used json encoder"))))
      (cl-mapc
       (lambda (text expected)
         (should (= expected
                    (anvil-server--projected-json-string-bytes text nil))))
       cases oracles)))
  (should (= 172
             (anvil-server--projected-json-string-bytes
              (apply #'string (number-sequence 0 31)) nil)))
  (should (= 640
             (anvil-server--projected-json-string-bytes
              (apply #'unibyte-string (number-sequence #x80 #xff)) nil)))
  (dolist (label (list "a" "A0._/-" "tool/name.with-127-safe-chars"
                       (make-string 128 ?a)))
    (should (equal label (anvil-server--safe-tool-label label))))
  (dolist (label (list nil "" " unsafe" "tool!" "é"
                       (make-string 129 ?a)))
    (should (equal "<oversized-tool-id>"
                   (anvil-server--safe-tool-label label))))
  (let ((anvil-server-max-inline-result-bytes 32))
    (should (equal (make-string 32 ?a)
                   (anvil-server--enforce-inline-result-limit
                    "tool" (make-string 32 ?a))))
    (should-error
     (anvil-server--enforce-inline-result-limit
      "tool" (make-string 33 ?a))
     :type 'anvil-server-inline-result-too-large)
    ;; Seven raw unibyte octets project to 35 bytes through Emacs octal
    ;; escaping, so raw length alone is not a safe acceptance test.
    (should-error
     (anvil-server--enforce-inline-result-limit
      "tool" (apply #'unibyte-string (make-list 7 #x80)))
     :type 'anvil-server-inline-result-too-large)
    (should (equal (make-string 8 #x1f600)
                   (anvil-server--enforce-inline-result-limit
                    "tool" (make-string 8 #x1f600))))
    (should-error
     (anvil-server--enforce-inline-result-limit
      "tool" (make-string 9 #x1f600))
     :type 'anvil-server-inline-result-too-large))
  (dolist (disabled '(nil 0 -1))
    (let ((anvil-server-max-inline-result-bytes disabled))
      (should (equal "legacy"
                     (anvil-server--enforce-inline-result-limit
                      "tool" "legacy")))))
  ;; Disabled caps preserve legacy success/error text and envelopes, while
  ;; the hook still receives a fresh condition cell and fixed-grammar label.
  (let* ((server-id "anvil-test-inline-disabled")
         (anvil-test--inline-payload "legacy-payload"))
    (unwind-protect
        (progn
          (anvil-server-register-tool
           #'anvil-test--inline-payload-tool
           :id "inline.legacy-success" :description "legacy fixture"
           :server-id server-id)
          (anvil-server-register-tool
           #'anvil-test--inline-direct-error-tool
           :id "inline.legacy-error" :description "legacy fixture"
           :server-id server-id)
          (anvil-server-register-tool
           #'anvil-test--inline-macro-error-tool
           :id "inline.legacy-macro" :description "legacy fixture"
           :server-id server-id)
          (dolist (disabled '(nil 0 -1))
            (let ((anvil-server-max-inline-result-bytes disabled))
              (should
               (equal
                "legacy-payload"
                (anvil-test--response-text
                 (anvil-server--handle-tools-call
                  102
                  '((name . "inline.legacy-success") (arguments . ()))
                  (make-anvil-server-metrics) server-id))))
              (should
               (equal
                "legacy-payload"
                (anvil-test--response-text
                 (anvil-server--handle-tools-call
                  103
                  '((name . "inline.legacy-error") (arguments . ()))
                  (make-anvil-server-metrics) server-id))))))
          (let ((anvil-server-max-inline-result-bytes nil)
                hook-condition hook-label)
            (let ((anvil-server-tool-error-hook
                   (list (lambda (condition label _source)
                           (setq hook-condition condition
                                 hook-label label)))))
              (let ((response
                     (anvil-server--handle-tools-call
                      104
                      '((name . "inline.legacy-macro") (arguments . ()))
                      (make-anvil-server-metrics) server-id)))
                (should (string-match-p "legacy-payload" response)))
              (should (consp hook-condition))
              (should (eq 'error (car hook-condition)))
              (should (string-match-p "legacy-payload"
                                      (cadr hook-condition)))
              (should (equal "inline.legacy-macro" hook-label)))))
      (dolist (id '("inline.legacy-success" "inline.legacy-error"
                    "inline.legacy-macro"))
        (ignore-errors (anvil-server-unregister-tool id server-id)))))
  ;; The sanitizer is a fail-closed, non-signaling boundary even if one of
  ;; its helpers is unexpectedly broken.
  (cl-letf (((symbol-function 'anvil-server--safe-tool-label)
             (lambda (&rest _) (error "helper fault"))))
    (should
     (equal '(:condition (error "") :text ""
               :tool "<oversized-tool-id>")
            (anvil-server--sanitize-tool-error
             "request-owned" 'generic '(error "request-owned")))))
  (cl-letf (((symbol-function
              'anvil-server--projected-json-string-bytes)
             (lambda (&rest _) (error "counter fault"))))
    (should
     (equal '(:condition (error "") :text ""
               :tool "<oversized-tool-id>")
            (anvil-server--sanitize-tool-error
             "request-owned" 'generic '(error "request-owned"))))))

(ert-deftest anvil-test-inline-result-limit-malformed-config-fails-closed ()
  "Malformed inline limits fail closed across every tool error boundary."
  (let* ((server-id "anvil-test-inline-malformed-limit")
         (anvil-test--inline-payload
          (concat "MALFORMED-LIMIT-REQUEST-SENTINEL-"
                  (make-string 4096 ?x)))
         (anvil-server--running t)
         (fallback '(:condition (error "") :text ""
                     :tool "<oversized-tool-id>"))
         (tool-ids
          '("inline.malformed-success"
            "inline.malformed-macro"
            "inline.malformed-tool-error"
            "inline.malformed-generic"
            "inline.malformed-wrong-type"
            "inline.malformed-quit"
            "inline.malformed-spoof"
            "inline.malformed-lazy-generic"
            "inline.malformed-lazy-tool-error")))
    (unwind-protect
        (progn
          (dolist (registration
                   `((anvil-test--inline-payload-tool
                      "inline.malformed-success")
                     (anvil-test--inline-macro-error-tool
                      "inline.malformed-macro")
                     (anvil-test--inline-direct-error-tool
                      "inline.malformed-tool-error")
                     (anvil-test--inline-generic-error-tool
                      "inline.malformed-generic")
                     (anvil-test--inline-wrong-type-tool
                      "inline.malformed-wrong-type")
                     (anvil-test--inline-quit-tool
                      "inline.malformed-quit")
                     (anvil-test--inline-spoof-overflow-tool
                      "inline.malformed-spoof")))
            (anvil-server-register-tool
             (car registration)
             :id (cadr registration) :description "malformed limit fixture"
             :server-id server-id))
          (let ((table (anvil-server--get-server-tools server-id)))
            (puthash
             "inline.malformed-lazy-generic"
             (list :id "inline.malformed-lazy-generic" :json-fragment "{}"
                   :lazy-placeholder t
                   :lazy-loader
                   (lambda (&rest _)
                     (error "%s" anvil-test--inline-payload)))
             table)
            (puthash
             "inline.malformed-lazy-tool-error"
             (list :id "inline.malformed-lazy-tool-error"
                   :json-fragment "{}" :lazy-placeholder t
                   :lazy-loader
                   (lambda (&rest _)
                     (anvil-server-tool-throw anvil-test--inline-payload)))
             table))
          (dolist (malformed '(t 1.5 "not-an-integer"))
            (let ((anvil-server-max-inline-result-bytes malformed))
              (should-error
               (anvil-server--enforce-inline-result-limit
                "inline.malformed-success" anvil-test--inline-payload)
               :type 'anvil-server-invalid-inline-result-limit)
              (dolist (class '(inline-result macro invalid-params tool-error
                                             quit generic not-found))
                (should
                 (equal
                  fallback
                  (anvil-server--sanitize-tool-error
                   anvil-test--inline-payload class
                   (list 'request-owned-condition
                         anvil-test--inline-payload)))))
              (dolist
                  (case
                   `(("success"
                      ((name . "inline.malformed-success") (arguments . ()))
                      ,anvil-server-jsonrpc-error-internal)
                     ("macro"
                      ((name . "inline.malformed-macro") (arguments . ()))
                      nil)
                     ("tool-error"
                      ((name . "inline.malformed-tool-error")
                       (arguments . ()))
                      nil)
                     ("generic"
                      ((name . "inline.malformed-generic") (arguments . ()))
                      ,anvil-server-jsonrpc-error-internal)
                     ("wrong-type"
                      ((name . "inline.malformed-wrong-type")
                       (arguments . ()))
                      ,anvil-server-jsonrpc-error-internal)
                     ("quit"
                      ((name . "inline.malformed-quit") (arguments . ()))
                      ,anvil-server-jsonrpc-error-internal)
                     ("spoofed-overflow"
                      ((name . "inline.malformed-spoof") (arguments . ()))
                      nil)
                     ("inner-invalid-params"
                      ((name . "inline.malformed-success")
                       (arguments
                        . ((unexpected . ,anvil-test--inline-payload))))
                      ,anvil-server-jsonrpc-error-invalid-params)
                     ("outer-invalid-params"
                      [,anvil-test--inline-payload]
                      ,anvil-server-jsonrpc-error-invalid-params)
                     ("lazy-generic"
                      ((name . "inline.malformed-lazy-generic")
                       (arguments . ()))
                      ,anvil-server-jsonrpc-error-internal)
                     ("lazy-tool-error"
                      ((name . "inline.malformed-lazy-tool-error")
                       (arguments . ()))
                      nil)
                     ("not-found"
                      ((name . ,anvil-test--inline-payload) (arguments . ()))
                      ,anvil-server-jsonrpc-error-invalid-request)))
                (ert-info ((format "limit type %S, path %s"
                                   (type-of malformed) (car case)))
                  (let* ((hook-values nil)
                         (anvil-server-tool-error-hook
                          (list
                           (lambda (&rest values)
                             (push values hook-values))))
                         (response
                          (anvil-server--handle-tools-call
                           105 (cadr case) (make-anvil-server-metrics)
                           server-id))
                         (decoded (json-read-from-string response))
                         (expected-code (caddr case)))
                    (should (< (string-bytes response) 1024))
                    (should-not
                     (string-match-p "MALFORMED-LIMIT-REQUEST-SENTINEL"
                                     response))
                    (if expected-code
                        (let ((error-object (alist-get 'error decoded)))
                          (should (= expected-code
                                     (alist-get 'code error-object)))
                          (should (equal ""
                                         (alist-get 'message error-object))))
                      (let ((result (alist-get 'result decoded)))
                        (should (eq t (alist-get 'isError result)))
                        (should (equal ""
                                       (anvil-test--response-text response)))))
                    (dolist (values hook-values)
                      (should-not
                       (string-match-p
                        "MALFORMED-LIMIT-REQUEST-SENTINEL"
                        (format "%S" values))))))))))
      (dolist (tool-id tool-ids)
        (ignore-errors (anvil-server-unregister-tool tool-id server-id))))))

(ert-deftest anvil-test-inline-result-projects-wide-and-raw-characters ()
  "The projector matches Emacs's five-byte extended character encoding."
  (let* ((below-threshold (string #x1fffff))
         (at-threshold (string #x200000))
         (multibyte-raw
          (string-make-multibyte (unibyte-string #x80 #xff)))
         (cases (list below-threshold at-threshold multibyte-raw))
         (expected
          (mapcar
           (lambda (text)
             (- (string-bytes (json-encode-string text)) 2))
           cases)))
    (cl-letf (((symbol-function 'json-encode-string)
               (lambda (&rest _) (ert-fail "projector used JSON encoder")))
              ((symbol-function 'json-encode)
               (lambda (&rest _) (ert-fail "projector used JSON encoder"))))
      (cl-mapc
       (lambda (text projected)
         (should
          (= projected
             (anvil-server--projected-json-string-bytes text nil))))
       cases expected)))
  (should (= 4 (anvil-server--projected-json-string-bytes
                (string #x1fffff))))
  (should (= 5 (anvil-server--projected-json-string-bytes
                (string #x200000))))
  (should (= 10 (anvil-server--projected-json-string-bytes
                 (string-make-multibyte (unibyte-string #x80 #xff))))))

(ert-deftest anvil-test-inline-result-spoofed-condition-is-bounded ()
  "Handler, lazy-loader, and disclosure overflow spoofs cannot bypass the cap."
  (let* ((server-id "anvil-test-inline-spoof")
         (anvil-server-max-inline-result-bytes 512)
         (anvil-server--running t)
         (anvil-test--inline-payload
          (concat "SPOOFED-OVERFLOW-SENTINEL-" (make-string 4096 ?x))))
    (unwind-protect
        (progn
          (anvil-server-register-tool
           #'anvil-test--inline-spoof-overflow-tool
           :id "inline.spoof-handler" :description "spoof fixture"
           :server-id server-id)
          (anvil-server-register-tool
           #'anvil-test--inline-token-aware-spoof-overflow-tool
           :id "inline.spoof-token-aware" :description "spoof fixture"
           :server-id server-id)
          (anvil-server-register-tool
           #'anvil-test--inline-payload-tool
           :id "inline.spoof-disclosure" :description "spoof fixture"
           :server-id server-id)
          (puthash
           "inline.spoof-loader"
           (list :id "inline.spoof-loader" :json-fragment "{}"
                 :lazy-placeholder t
                 :lazy-loader
                 (lambda (&rest _)
                   (signal 'anvil-server-inline-result-too-large
                           (list anvil-test--inline-payload))))
           (anvil-server--get-server-tools server-id))
          (dolist (tool-id '("inline.spoof-handler" "inline.spoof-token-aware"
                            "inline.spoof-loader"))
            (let* ((response
                    (anvil-server--handle-tools-call
                     105 `((name . ,tool-id) (arguments . ()))
                     (make-anvil-server-metrics) server-id))
                   (text (anvil-test--response-text response)))
              (should (stringp text))
              (should-not
               (string-match-p "SPOOFED-OVERFLOW-SENTINEL" response))
              (if (equal tool-id "inline.spoof-token-aware")
                  (should (equal "Tool error contained non-string data" text))
                (should
                 (string-match-p
                  "inline-result error text exceeds" text)))
              (should-not (string-match-p " result exceeds" text))
              (should
               (<= (anvil-server--projected-json-string-bytes text)
                   anvil-server-max-inline-result-bytes))))
          (setq anvil-test--inline-payload "safe")
          (cl-letf (((symbol-function 'anvil-disclosure-budget-apply)
                     (lambda (&rest _)
                       (signal 'anvil-server-inline-result-too-large
                               (list
                                (concat
                                 "SPOOFED-OVERFLOW-SENTINEL-"
                                 (make-string 4096 ?d)))))))
            (let* ((response
                    (anvil-server--handle-tools-call
                     106
                     '((name . "inline.spoof-disclosure") (arguments . ()))
                     (make-anvil-server-metrics) server-id))
                   (text (anvil-test--response-text response)))
              (should (stringp text))
              (should-not
               (string-match-p "SPOOFED-OVERFLOW-SENTINEL" response))
              (should
               (string-match-p "inline-result error text exceeds" text))
              (should-not (string-match-p " result exceeds" text))
              (should
               (<= (anvil-server--projected-json-string-bytes text)
                   anvil-server-max-inline-result-bytes)))))
      (dolist (id '("inline.spoof-handler" "inline.spoof-loader"
                    "inline.spoof-token-aware" "inline.spoof-disclosure"))
        (ignore-errors (anvil-server-unregister-tool id server-id))))))

(ert-deftest anvil-test-inline-result-malformed-internal-count-is-bounded ()
  "An impossible internal overflow count must use the ordinary error path."
  (let* ((server-id "anvil-test-inline-malformed-internal")
         (tool-id "inline.malformed-internal")
         (anvil-server-max-inline-result-bytes 512)
         (anvil-server--running t)
         (anvil-test--inline-payload "safe")
         (impossible-count (expt 10 1000))
         (malformed-counts
          (list
           (list impossible-count 513)
           (list 80 (1+ most-positive-fixnum))
           (list 0 513)
           (list 80 512)
           (list 80 519)
           (list 80 513 'extra)
           (list "80" 513))))
    (unwind-protect
        (progn
          (anvil-server-register-tool
           #'anvil-test--inline-payload-tool
           :id tool-id :description "malformed internal fixture"
           :server-id server-id)
          (dolist (counts malformed-counts)
            (cl-letf (((symbol-function
                        'anvil-server--enforce-inline-result-limit)
                       (lambda (&rest _)
                         (signal 'anvil-server-inline-result-too-large
                                 counts))))
              (let* ((response
                      (anvil-server--handle-tools-call
                       107 `((name . ,tool-id) (arguments . ()))
                       (make-anvil-server-metrics) server-id))
                     (decoded (json-read-from-string response))
                     (error-object (alist-get 'error decoded))
                     (message (alist-get 'message error-object)))
                (should (= -32603 (alist-get 'code error-object)))
                (should
                 (equal "Internal error executing tool with non-string data"
                        message))
                (should-not
                 (string-match-p
                  (number-to-string impossible-count) response))
                (should
                 (<= (anvil-server--projected-json-string-bytes message)
                     anvil-server-max-inline-result-bytes))))))
      (ignore-errors (anvil-server-unregister-tool tool-id server-id)))))

(ert-deftest anvil-test-inline-result-error-hook-strips-properties ()
  "The real dispatcher hook receives plain sanitized error text."
  (let* ((server-id "anvil-test-inline-properties")
         (tool-id "inline.property-error")
         (hidden-object (list :request-owned-object))
         (request-tool-id
          (propertize (copy-sequence tool-id)
                      'request-owned hidden-object))
         (anvil-test--inline-payload
          (propertize "visible error" 'request-owned hidden-object))
         (anvil-server-max-inline-result-bytes 512)
         (anvil-server--running t)
         hook-label
         recorded)
    (unwind-protect
        (progn
          (require 'anvil-harness-telemetry)
          (anvil-server-register-tool
           #'anvil-test--inline-property-error-tool
           :id tool-id :description "property fixture" :server-id server-id)
          (cl-letf (((symbol-function 'anvil-harness-telemetry-record)
                     (lambda (class &rest keys)
                       (setq recorded (cons class keys))
                       '(:id 1))))
            (let ((anvil-server-tool-error-hook
                   (list
                    (lambda (err label source)
                      (setq hook-label label)
                      (anvil-harness-telemetry--dispatcher-hook
                       err label source)))))
              (let ((response
                     (anvil-server--handle-tools-call
                      107 `((name . ,request-tool-id) (arguments . ()))
                      (make-anvil-server-metrics) server-id)))
                (should (= anvil-server-jsonrpc-error-internal
                           (anvil-test--response-error-code response))))))
          (should (consp recorded))
          (should (equal tool-id hook-label))
          (should-not (eq request-tool-id hook-label))
          (should-not (text-properties-at 0 hook-label))
          (should
           (stringp (plist-get (cdr recorded) :error-message)))
          (should-not
           (text-property-not-all
            0 (length (plist-get (cdr recorded) :error-message))
            'request-owned nil
            (plist-get (cdr recorded) :error-message)))
          (should-not
           (string-match-p
            "request-owned-object"
            (plist-get (cdr recorded) :raw-context))))
      (ignore-errors (anvil-server-unregister-tool tool-id server-id)))))

(ert-deftest anvil-test-inline-result-projects-error-before-concat ()
  "Oversized generic error data is projected before any combined copy."
  (let* ((anvil-server-max-inline-result-bytes 32)
         (payload (make-string 4096 ?x))
         (original-join
          (symbol-function 'anvil-server--join-error-segments))
         (copied-request-data nil)
         sanitized)
    (cl-letf (((symbol-function 'anvil-server--join-error-segments)
               (lambda (segments)
                 (when (memq payload segments)
                   (setq copied-request-data t))
                 (funcall original-join segments))))
      (setq sanitized
            (anvil-server--sanitize-tool-error
             "inline.generic" 'generic (list 'error payload))))
    (should-not copied-request-data)
    (should
     (string-match-p "inline response limit"
                     (plist-get sanitized :text)))))

(ert-deftest anvil-test-inline-result-fallback-values-are-fresh ()
  "Each sanitizer fault returns independent mutable fallback values."
  (dolist (failure '(error quit))
    (cl-letf (((symbol-function 'anvil-server--safe-tool-label)
               (lambda (&rest _)
                 (if (eq failure 'quit)
                     (signal 'quit nil)
                   (error "helper fault")))))
      (let* ((first
              (anvil-server--sanitize-tool-error
               "request-owned" 'generic '(error "request-owned")))
             (first-condition (plist-get first :condition))
             (first-label (plist-get first :tool)))
        (unwind-protect
            (progn
              (setcar first :mutated)
              (setcar first-condition 'file-error)
              (aset first-label 0 ?X)
              (let* ((second
                      (anvil-server--sanitize-tool-error
                       "request-owned" 'generic '(error "request-owned")))
                     (second-condition (plist-get second :condition))
                     (second-label (plist-get second :tool)))
                (should (eq :condition (car second)))
                (should (eq 'error (car second-condition)))
                (should (equal "<oversized-tool-id>" second-label))
                (should-not (eq first second))
                (should-not (eq first-condition second-condition))
                (should-not (eq first-label second-label))))
          (setcar first :condition)
          (setcar first-condition 'error)
          (aset first-label 0 ?<))))))

(ert-deftest anvil-test-inline-result-rejected-labels-are-fresh ()
  "Rejected request labels cannot mutate a later sanitizer result."
  (let* ((first
          (anvil-server--sanitize-tool-error
           "!invalid" 'generic '(error "request-owned")))
         (first-label (plist-get first :tool)))
    (unwind-protect
        (progn
          (aset first-label 0 ?X)
          (let* ((second
                  (anvil-server--sanitize-tool-error
                   "!invalid" 'generic '(error "request-owned")))
                 (second-label (plist-get second :tool)))
            (should (equal "<oversized-tool-id>" second-label))
            (should-not (eq first-label second-label))))
      (aset first-label 0 ?<))))

(ert-deftest anvil-test-tools-call-allows-nil-json-object-key ()
  "A JSON object key decoded as symbol nil remains a valid alist entry."
  (let* ((server-id "anvil-test-nil-json-key")
         (tool-id "inline.nil-json-key")
         (anvil-test--inline-payload "nil-key accepted")
         (anvil-server--running t)
         (request
          (concat
           "{\"jsonrpc\":\"2.0\",\"id\":108,\"method\":\"tools/call\","
           "\"params\":{\"name\":\"inline.nil-json-key\","
           "\"arguments\":{},\"nil\":\"ignored\"}}")))
    (unwind-protect
        (progn
          (anvil-server-register-tool
           #'anvil-test--inline-payload-tool
           :id tool-id :description "nil-key fixture" :server-id server-id)
          (let* ((decoded (json-read-from-string request))
                 (params (alist-get 'params decoded)))
            (should (consp (assq nil params))))
          (let ((response (anvil-server-process-jsonrpc request server-id)))
            (should-not (anvil-test--response-error-code response))
            (should
             (equal anvil-test--inline-payload
                    (anvil-test--response-text response)))))
      (ignore-errors (anvil-server-unregister-tool tool-id server-id)))))

;;; anvil-test.el ends here
