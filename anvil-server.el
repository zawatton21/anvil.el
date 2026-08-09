;;; anvil-server.el --- Anvil MCP protocol core -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Author: Laurynas Biveinis (original mcp-server-lib), zawatton (anvil fork)
;; Keywords: comm, tools
;; Version: 0.2.1
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/zawatton/anvil.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library enables Emacs packages to expose their functionality to AI
;; applications via the Model Context Protocol (MCP).
;;
;; For users of MCP-enabled Emacs packages:
;; 1. Run M-x anvil-server-install to install the stdio transport script
;; 2. Run M-x anvil-server-start to start the MCP server
;; 3. Register your MCP server with an AI client using the installed script
;;    (see your specific MCP server's documentation for details)
;;
;; Additional commands:
;; - M-x anvil-server-stop: Stop the MCP server
;; - M-x anvil-server-describe-setup: View registered tools and resources
;; - M-x anvil-server-show-metrics: View usage statistics
;; - M-x anvil-server-uninstall: Remove the stdio transport script
;;
;; The library handles JSON-RPC 2.0 communication, manages tool and resource
;; registration, and provides error handling suitable for LLM interactions.
;;
;; See https://modelcontextprotocol.io/ for the protocol specification.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'anvil-server-metrics)

;; `anvil-offload' is an optional module — loaded on demand in
;; `anvil-server--offload-apply'.  Declare to silence byte-compile warnings.
(declare-function anvil-server-start "anvil-server-commands" ())
(declare-function anvil-offload "anvil-offload" (form &rest keys))
(declare-function anvil-future-await "anvil-offload" (future &optional timeout))
(declare-function anvil-future-cancel "anvil-offload" (future))
(declare-function anvil-future-kill "anvil-offload" (future))
(declare-function anvil-future-elapsed "anvil-offload" (future))
(declare-function anvil-future-status "anvil-offload" (future))
(declare-function anvil-future-value "anvil-offload" (future))
(declare-function anvil-future-error "anvil-offload" (future))
(declare-function anvil-future-checkpoint "anvil-offload" (future))

(defvar anvil-server--debug-trace nil
  "When non-nil, emit `[STDIO]/[PJ]/[VAD]/[JR]/[TL]/[REG-*]/[STEP]'
diagnostic trace lines via `nelisp--write-stderr-line' from the
standalone-nelisp dispatch chain.  Default nil for production
(silent).  Bisect / debug sessions can set non-nil before loading
the substrate to trace per-stage timing.")

;;; Customization variables

(defgroup anvil-server nil
  "Anvil MCP server core."
  :group 'comm
  :prefix "anvil-server-")

(defcustom anvil-server-log-io nil
  "If non-nil, log all JSON-RPC messages to the *anvil-server-log* buffer."
  :group 'anvil-server
  :type 'boolean)

(defcustom anvil-server-install-directory user-emacs-directory
  "Directory where anvil-stdio.sh will be installed.
Defaults to `user-emacs-directory' but can be customized."
  :type 'directory
  :group 'anvil-server)

(defcustom anvil-server-schema-cache-file
  (expand-file-name
   "anvil-schema-cache.el"
   (expand-file-name "anvil-runtime" temporary-file-directory))
  "File used to persist precomputed MCP tool schemas and JSON fragments."
  :type 'file
  :group 'anvil-server)

(defcustom anvil-server-autostart-on-request nil
  "When non-nil, restart a stopped server instead of erroring.
If `anvil-server-process-jsonrpc' is reached while
`anvil-server--running' is nil (the server was stopped for any
reason), call `anvil-server-start' transparently rather than
signalling.  For stdio transports the signalled error surfaces as
empty emacsclient output, which clients cannot match to their
request — effectively a hang.  Nil preserves the historical
behaviour of signalling an error."
  :type 'boolean
  :group 'anvil-server)

(defcustom anvil-server-dispatch-timeout nil
  "Seconds before an in-flight request is answered with a timeout error.
When non-nil, request dispatch in `anvil-server-process-jsonrpc' is
wrapped in `with-timeout': a handler that exceeds the limit yields a
proper JSON-RPC internal error (carrying the request id) instead of
blocking the transport indefinitely.  Note `with-timeout' only fires
while the Emacs event loop is processing; it cannot interrupt a
tight non-yielding loop.  Nil (default) imposes no limit."
  :type '(choice (const :tag "No timeout" nil) number)
  :group 'anvil-server)

;;; Public Constants

(defconst anvil-server-name "anvil"
  "Name of the MCP server.")

(defconst anvil-server-protocol-version "2025-03-26"
  "Current MCP protocol version supported by this server.")

;;; Public API - JSON-RPC 2.0 Error Codes

(defconst anvil-server-jsonrpc-error-parse -32700
  "JSON-RPC 2.0 Parse Error code.")

(defconst anvil-server-jsonrpc-error-invalid-request -32600
  "JSON-RPC 2.0 Invalid Request error code.")

(defconst anvil-server-jsonrpc-error-method-not-found -32601
  "JSON-RPC 2.0 Method Not Found error code.")

(defconst anvil-server-jsonrpc-error-invalid-params -32602
  "JSON-RPC 2.0 Invalid Params error code.")

(defconst anvil-server-jsonrpc-error-internal -32603
  "JSON-RPC 2.0 Internal Error code.")

;;; Internal Constants

(defconst anvil-server--uri-scheme-regex
  "[a-zA-Z][a-zA-Z0-9+.-]*://"
  "Regex pattern matching URI scheme according to RFC 3986.
Matches scheme names that start with a letter followed by any combination
of letters, digits, plus, period, or hyphen, ending with ://")

(defconst anvil-server--uri-scheme-start-regex
  (concat "^" anvil-server--uri-scheme-regex)
  "Regex pattern matching URI scheme at start of string.")

(defconst anvil-server--uri-with-scheme-regex
  (concat "\\`" anvil-server--uri-scheme-regex ".+")
  "Regex pattern matching complete URI starting with scheme.")

(defconst anvil-server--param-indent-min 2
  "Minimum indentation (in spaces) for parameter definitions.
This follows standard Emacs docstring conventions for list items,
where list items are typically indented 2-4 spaces.")

(defconst anvil-server--param-indent-max 4
  "Maximum indentation (in spaces) for parameter definitions.
This follows standard Emacs docstring conventions for list items,
where list items are typically indented 2-4 spaces.")

(defconst anvil-server--continuation-indent-min 6
  "Minimum indentation (in spaces) for continuation lines.
This value MUST be greater than `anvil-server--param-indent-max'
to ensure continuation lines can be distinguished from new parameter
definitions in the parser state machine.")

;;; Internal global state variables

(defvar anvil-server--running nil
  "Whether the MCP server is currently running.")

(defun anvil-server-running-p ()
  "Return non-nil if the Anvil MCP server is currently running."
  (bound-and-true-p anvil-server--running))

(defvar anvil-server--tools (make-hash-table :test 'equal)
  "Hash table of registered MCP tools by server.
Keys are server-id strings, values are hash tables of tool-id to tool-data.")

(defvar anvil-server--resources (make-hash-table :test 'equal)
  "Hash table of registered MCP resources by server.
Keys are server-id strings, values are hash tables of URI to resource-data.")

(defvar anvil-server--resource-templates
  (make-hash-table :test 'equal)
  "Hash table of registered MCP resource templates by server.
Keys are server-id strings, values are hash tables of template to template-data.")

(defvar anvil-server--schema-cache-version 1
  "Version marker for `anvil-server-schema-cache-file'.")

(defvar anvil-server--schema-cache nil
  "In-memory list of persisted schema cache entries.")

(defvar anvil-server--schema-cache-loaded nil
  "Non-nil once `anvil-server-schema-cache-file' has been read.")

;;; Core helpers

(defun anvil-server--get-server-tools (server-id)
  "Get tools hash table for SERVER-ID, creating if needed.
Returns a hash table mapping tool-id to tool-data."
  (or (gethash server-id anvil-server--tools)
      (let ((tools-table (make-hash-table :test 'equal)))
        (puthash server-id tools-table anvil-server--tools)
        tools-table)))

(defun anvil-server--get-server-resources (server-id)
  "Get resources hash table for SERVER-ID, creating if needed.
Returns a hash table mapping URI to resource-data."
  (or (gethash server-id anvil-server--resources)
      (let ((resources-table (make-hash-table :test 'equal)))
        (puthash server-id resources-table anvil-server--resources)
        resources-table)))

(defun anvil-server--get-server-templates (server-id)
  "Get templates hash table for SERVER-ID, creating if needed.
Returns a hash table mapping template to template-data."
  (or (gethash server-id anvil-server--resource-templates)
      (let ((templates-table (make-hash-table :test 'equal)))
        (puthash
         server-id templates-table anvil-server--resource-templates)
        templates-table)))

(defun anvil-server--js-string (s)
  "Encode S as a JSON string literal (including surrounding quotes).
Hand-built char-by-char escape that avoids `json-encode' (which has
cumulative degenerate performance on the pure-elisp interpreter
post-2026-05-17: stalls after ~4-5 calls in the same process)."
  (let ((parts (list "\""))
        (i 0) (n (length s)))
    (while (< i n)
      (let ((c (aref s i)))
        (cond
         ((eq c ?\") (push "\\\"" parts))
         ((eq c ?\\) (push "\\\\" parts))
         ((eq c ?\n) (push "\\n" parts))
         ((eq c ?\r) (push "\\r" parts))
         ((eq c ?\t) (push "\\t" parts))
         ((< c 32) (push (format "\\u%04x" c) parts))
         (t (push (char-to-string c) parts))))
      (setq i (1+ i)))
    (push "\"" parts)
    (apply #'concat (nreverse parts))))

(defun anvil-server--js-schema (schema)
  "Encode an anvil-generated MCP input SCHEMA alist to JSON.
Handles the bounded shape produced by
`anvil-server--generate-schema-from-function':
  `((type . \"object\") (properties . PROPS) (required . VEC))'.
Hand-built to avoid `json-encode' cumulative degradation."
  (if (null schema)
      "{\"type\":\"object\"}"
    (let ((parts (list "{"))
          (first t))
      (dolist (pair schema)
        (let ((key (car pair))
              (val (cdr pair)))
          (unless first (push "," parts))
          (setq first nil)
          (push (anvil-server--js-string (symbol-name key)) parts)
          (push ":" parts)
          (cond
           ((stringp val)
            (push (anvil-server--js-string val) parts))
           ((vectorp val)
            (push "[" parts)
            (let ((vfirst t) (j 0) (vn (length val)))
              (while (< j vn)
                (unless vfirst (push "," parts))
                (setq vfirst nil)
                (push (anvil-server--js-string (aref val j)) parts)
                (setq j (1+ j))))
            (push "]" parts))
           ((eq key 'properties)
            ;; alist of (NAME-STRING . SUB-ALIST)
            (push "{" parts)
            (let ((pfirst t))
              (dolist (prop val)
                (unless pfirst (push "," parts))
                (setq pfirst nil)
                (push (anvil-server--js-string (car prop)) parts)
                (push ":" parts)
                (push (anvil-server--js-schema (cdr prop)) parts)))
            (push "}" parts))
           ((and (consp val) (consp (car val)))
            ;; Nested schema alist (e.g. an array's `items') — recurse.
            ;; Provided :schema values reach here; the generator's own
            ;; bounded shape never nests below `properties'.
            (push (anvil-server--js-schema val) parts))
           (t
            ;; Fallback: stringify
            (push (anvil-server--js-string (format "%s" val)) parts)))))
      (push "}" parts)
      (apply #'concat (nreverse parts)))))

(defun anvil-server--build-tool-fragment (id description schema)
  "Build a tools/list JSON fragment for one tool.
Hand-built; never calls `json-encode'."
  (concat "{\"name\":" (anvil-server--js-string id)
          ",\"description\":" (anvil-server--js-string description)
          ",\"inputSchema\":" (anvil-server--js-schema schema)
          "}"))

(defun anvil-server--prin1-to-string (object)
  "Return the `prin1' representation of OBJECT as a string.
Uses the builtin `prin1-to-string' (a pure string operation) rather
than `prin1' into a temp buffer: standalone NeLisp ignores a buffer
stream argument to `prin1' and writes to stdout instead, which both
corrupts the MCP framing channel and leaves the buffer empty."
  (let ((print-length nil)
        (print-level nil))
    (prin1-to-string object)))

(defun anvil-server--schema-cache-signature
    (raw-handler arglist description provided-schema)
  "Return a persistent cache signature for one tool registration.
Keys on exactly the inputs the generated schema depends on: the
handler's ARGLIST, the tool DESCRIPTION, and any
explicitly-PROVIDED-SCHEMA.

Deliberately does NOT serialize the live function object.  On
standalone NeLisp the handler is a `(closure ENV ARGS BODY)' whose
captured ENV (a) overflows `prin1' into a stack-exhaustion SIGSEGV
\(observed: hard crash on the first tool registration) and (b) is
re-consed every session, so a definition-keyed signature could never
hit the cache there anyway.

Also deliberately avoids `(documentation RAW-HANDLER t)'.  Standalone
NeLisp can return unstable values or hang while resolving docstrings for
some handlers, and this function runs before cache lookup.  Cache misses
still parse docstrings during schema generation when user-visible
parameters exist."
  (let ((handler-name (and (symbolp raw-handler) raw-handler)))
    (anvil-server--prin1-to-string
     (list :handler handler-name
           :arglist arglist
           :description description
           ;; Preserve the old standalone cache signature shape.  Earlier
           ;; standalone-generated cache files already stored `:doc nil'.
           :doc nil
           :schema provided-schema))))

(defvar anvil-server--schema-cache-file-data nil
  "Scratch binding set by loading `anvil-server-schema-cache-file'.
The cache file is a self-loading `(setq ...)' form; see
`anvil-server--schema-cache-write'.")

(defun anvil-server--schema-cache-load ()
  "Read `anvil-server-schema-cache-file' once into memory via `load'.
Standalone NeLisp cannot `read' from a buffer (`insert-file-contents'
yields an empty buffer and `(read (current-buffer))' signals \"no
STREAM\"), so the cache file is a self-loading `(setq ...)' form that we
`load' and then validate.  `load' behaves identically on host Emacs, so
this is a single portable path."
  (unless anvil-server--schema-cache-loaded
    (setq anvil-server--schema-cache-loaded t)
    (setq anvil-server--schema-cache
          (condition-case nil
              (progn
                (setq anvil-server--schema-cache-file-data nil)
                ;; NOERROR + NOMESSAGE: a missing cache file just yields nil.
                (load anvil-server-schema-cache-file t t)
                (let ((data anvil-server--schema-cache-file-data))
                  (if (and (listp data)
                           (eq (plist-get data :version)
                               anvil-server--schema-cache-version)
                           (listp (plist-get data :entries)))
                      (plist-get data :entries)
                    nil)))
            (error nil)))))

(defun anvil-server--schema-cache-write ()
  "Persist `anvil-server--schema-cache' to disk as a self-loading form.
Writes `(setq anvil-server--schema-cache-file-data (quote DATA))' via
`write-region' on a STRING.  Standalone NeLisp does not honour a buffer
stream argument to `prin1' (it leaks to stdout and leaves the buffer
empty), so the cache round-trips purely through string ops + `load'
\(see `anvil-server--schema-cache-load')."
  (condition-case nil
      (let ((dir (file-name-directory anvil-server-schema-cache-file)))
        (when (and dir (not (file-directory-p dir)))
          (make-directory dir t))
        (write-region
         (concat
          "(setq anvil-server--schema-cache-file-data '"
          (anvil-server--prin1-to-string
           (list :version anvil-server--schema-cache-version
                 :entries anvil-server--schema-cache))
          ")\n")
         nil anvil-server-schema-cache-file))
    (error nil)))

(defun anvil-server--schema-cache-get (id signature)
  "Return cached schema entry for ID and SIGNATURE, or nil."
  (anvil-server--schema-cache-load)
  (catch 'found
    (dolist (entry anvil-server--schema-cache)
      (when (and (equal id (plist-get entry :id))
                 (equal signature (plist-get entry :signature))
                 (plist-member entry :schema)
                 (stringp (plist-get entry :fragment)))
        (throw 'found entry)))
    nil))

(defun anvil-server--schema-cache-put (id signature schema fragment)
  "Persist SCHEMA and FRAGMENT for ID and SIGNATURE."
  (anvil-server--schema-cache-load)
  (setq anvil-server--schema-cache
        (cons (list :id id
                    :signature signature
                    :schema schema
                    :fragment fragment)
              (cl-remove-if
               (lambda (entry)
                 (and (equal id (plist-get entry :id))
                      (equal signature (plist-get entry :signature))))
               anvil-server--schema-cache)))
  (anvil-server--schema-cache-write))

(defvar anvil-server--tools-list-cache)

(defun anvil-server-register-cached-tool-fragments
    (server-id lazy-loaders)
  "Register cached tool-list fragments for SERVER-ID.
LAZY-LOADERS is an alist mapping tool id strings to loader functions.
Each cached entry becomes a placeholder tool that can advertise itself
in `tools/list' immediately.  The real module is loaded on first
`tools/call' through its loader."
  (let ((tools-table (anvil-server--get-server-tools server-id))
        (fragments nil)
        (count 0))
    (anvil-server--schema-cache-load)
    (dolist (entry anvil-server--schema-cache)
      (let* ((id (plist-get entry :id))
             (fragment (plist-get entry :fragment))
             (loader (cdr (assoc id lazy-loaders))))
        (when (and (stringp id)
                   (stringp fragment)
                   loader
                   (not (gethash id tools-table)))
          (puthash id
                   (list :id id
                         :description ""
                         :schema (plist-get entry :schema)
                         :json-fragment fragment
                         :lazy-placeholder t
                         :lazy-loader loader)
                   tools-table)
          (push fragment fragments)
          (setq count (1+ count)))))
    (when (> count 0)
      (puthash server-id
               (concat "{\"tools\":["
                       (mapconcat #'identity (nreverse fragments) ",")
                       "]}")
               anvil-server--tools-list-cache))
    count))

(defun anvil-server--scan-substr-pos (s needle)
  "Return start index of NEEDLE in S, or nil.
Pure char-by-char scan — does not depend on `string-match' which
returns nil on raw-byte strings from `read-stdin-bytes' under
standalone nelisp."
  (let* ((s-len (length s))
         (n-len (length needle))
         (limit (- s-len n-len))
         (i 0)
         (found nil))
    (while (and (<= i limit) (not found))
      (let ((j 0) (ok t))
        (while (and ok (< j n-len))
          (if (eq (aref s (+ i j)) (aref needle j))
              (setq j (1+ j))
            (setq ok nil)))
        (if ok (setq found i)
          (setq i (1+ i)))))
    found))

(defun anvil-server--scan-int-after (s needle)
  "Return integer after NEEDLE in S, tolerating whitespace, or nil."
  (let ((pos (anvil-server--scan-substr-pos s needle)))
    (when pos
      (let* ((s-len (length s))
             (i (+ pos (length needle)))
             (digits nil)
             (saw-digit nil))
        (while (and (< i s-len)
                    (let ((c (aref s i)))
                      (or (eq c ?\s) (eq c ?\t) (eq c ?\n) (eq c ?\r))))
          (setq i (1+ i)))
        (when (and (< i s-len) (eq (aref s i) ?-))
          (push (aref s i) digits)
          (setq i (1+ i)))
        (while (and (< i s-len)
                    (let ((c (aref s i)))
                      (and (>= c ?0) (<= c ?9))))
          (push (aref s i) digits)
          (setq saw-digit t)
          (setq i (1+ i)))
        (when saw-digit
          (string-to-number (apply #'string (nreverse digits))))))))

(defun anvil-server--scan-string-after (s needle)
  "Return JSON string after NEEDLE in S, tolerating whitespace, or nil.
NEEDLE should end at the colon before the value."
  (let ((pos (anvil-server--scan-substr-pos s needle)))
    (when pos
      (let* ((s-len (length s))
             (i (+ pos (length needle)))
             (chars nil))
        (while (and (< i s-len)
                    (let ((c (aref s i)))
                      (or (eq c ?\s) (eq c ?\t) (eq c ?\n) (eq c ?\r))))
          (setq i (1+ i)))
        (when (and (< i s-len) (eq (aref s i) ?\"))
          (setq i (1+ i))
          (while (and (< i s-len) (not (eq (aref s i) ?\")))
            (if (eq (aref s i) ?\\)
                (progn
                  (setq i (1+ i))
                  (when (< i s-len)
                    (push (aref s i) chars)
                    (setq i (1+ i))))
              (push (aref s i) chars)
              (setq i (1+ i))))
          (apply #'string (nreverse chars)))))))

(defun anvil-server--scan-json-value-after (s needle)
  "Return a string or integer JSON value after NEEDLE in S, or nil."
  (let ((pos (anvil-server--scan-substr-pos s needle)))
    (when pos
      (let* ((s-len (length s))
             (i (+ pos (length needle))))
        (while (and (< i s-len)
                    (let ((c (aref s i)))
                      (or (eq c ?\s) (eq c ?\t) (eq c ?\n) (eq c ?\r))))
          (setq i (1+ i)))
        (cond
         ((and (< i s-len) (eq (aref s i) ?\"))
          (anvil-server--scan-string-after s needle))
         ((and (< i s-len)
               (let ((c (aref s i)))
                 (or (eq c ?-) (and (>= c ?0) (<= c ?9)))))
          (anvil-server--scan-int-after s needle))
         (t nil))))))

(defun anvil-server--scan-flat-object-after (s needle)
  "Parse a flat JSON object that begins right after NEEDLE in S.
Returns an alist `((SYMBOL . STRING-OR-INT) ...)' or nil if no
match.  Handles the bounded shape used by MCP tool arguments:
`{\"key1\":\"val1\",\"key2\":42,\"key3\":\"val3\"}'.  Bypasses
json-read-from-string (broken on read-stdin-bytes strings under
standalone nelisp)."
  (let ((pos (anvil-server--scan-substr-pos s needle)))
    (when pos
      (let* ((s-len (length s))
             (i (+ pos (length needle)))
             ;; Skip optional whitespace + opening `{'.
             (_skip-open
              (progn
                (while (and (< i s-len)
                            (let ((c (aref s i)))
                              (or (eq c ?\s) (eq c ?\t) (eq c ?\n))))
                  (setq i (1+ i)))
                (when (and (< i s-len) (eq (aref s i) ?\{))
                  (setq i (1+ i)))))
             (result nil)
             (done nil))
        (while (and (not done) (< i s-len))
          ;; Skip whitespace and commas
          (while (and (< i s-len)
                      (let ((c (aref s i)))
                        (or (eq c ?\s) (eq c ?\t) (eq c ?\n) (eq c ?,))))
            (setq i (1+ i)))
          (cond
           ((>= i s-len) (setq done t))
           ((eq (aref s i) ?\})
            (setq done t))
           ((eq (aref s i) ?\")
            ;; Read key string
            (setq i (1+ i))
            (let ((key-chars nil))
              (while (and (< i s-len) (not (eq (aref s i) ?\")))
                (push (aref s i) key-chars)
                (setq i (1+ i)))
              (when (< i s-len) (setq i (1+ i))) ; consume closing "
              ;; Skip whitespace + colon
              (while (and (< i s-len)
                          (let ((c (aref s i)))
                            (or (eq c ?\s) (eq c ?\t) (eq c ?:))))
                (setq i (1+ i)))
              ;; Read value
              (let ((key (intern (apply #'string (nreverse key-chars))))
                    (val nil))
                (cond
                 ((and (< i s-len) (eq (aref s i) ?\"))
                  (setq i (1+ i))
                  (let ((val-chars nil))
                    (while (and (< i s-len) (not (eq (aref s i) ?\")))
                      ;; Minimal escape handling: skip a single backslash
                      ;; and copy the next char verbatim (covers `\"' and
                      ;; `\\' for typical MCP args).
                      (if (eq (aref s i) ?\\)
                          (progn
                            (setq i (1+ i))
                            (when (< i s-len)
                              (push (aref s i) val-chars)
                              (setq i (1+ i))))
                        (push (aref s i) val-chars)
                        (setq i (1+ i))))
                    (when (< i s-len) (setq i (1+ i))) ; consume closing "
                    (setq val (apply #'string (nreverse val-chars)))))
                 ((and (< i s-len)
                       (let ((c (aref s i)))
                         (or (eq c ?-)
                             (and (>= c ?0) (<= c ?9)))))
                  (let ((num-chars nil))
                    (while (and (< i s-len)
                                (let ((c (aref s i)))
                                  (or (eq c ?-)
                                      (eq c ?.)
                                      (and (>= c ?0) (<= c ?9)))))
                      (push (aref s i) num-chars)
                      (setq i (1+ i)))
                    (setq val
                          (string-to-number
                           (apply #'string (nreverse num-chars))))))
                 (t (setq done t)))
                (push (cons key val) result))))
           (t (setq i (1+ i)))))
        (nreverse result)))))

(defun anvil-server--jsonrpc-response (id result)
  "Create a JSON-RPC response with ID and RESULT."
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[JR] enter"))
  (let* ((t0 (float-time))
         (form `((jsonrpc . "2.0") (id . ,id) (result . ,result)))
         (_ (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
              (nelisp--write-stderr-line
               (format "[JR] form-built %.4fs" (- (float-time) t0)))))
         (t1 (float-time))
         (out (json-encode form)))
    (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
      (nelisp--write-stderr-line
       (format "[JR] json-encode %.4fs len=%d"
               (- (float-time) t1) (length out))))
    out))

;; ---- tools/list response cache (perf: 2026-05-11) ----
;;
;; MCP `tools/list' returns a ~20KB JSON payload for the standard
;; ~30-tool bundle.  json-encode on NeLisp standalone is O(N²) inside
;; the native `mapconcat' primitive (`lisp/nelisp-stdlib-plist-str.el'
;; line ~1037, `(setq acc (concat acc joiner (car cur)))' in a while
;; loop), driving response time to 35-120 s and overrunning Claude
;; Code's 30 s /mcp reconnect timeout.  The response content is pure
;; data — registry × filter — so caching the JSON-encoded `result'
;; object per server-id and skipping json-encode on subsequent calls
;; is safe.  Invalidated by `anvil-server-register-tool' /
;; `anvil-server-unregister-tool' (which call the helper directly).
;;
;; We cache only the `result' fragment (= the JSON for `((tools . [...]))'),
;; not the full JSON-RPC wrapper, because the wrapper's `id' varies per
;; call.  The wrapper concat is constant-time string assembly.
(defvar anvil-server--tools-list-cache (make-hash-table :test 'equal)
  "Hash table mapping (VIRTUAL-ID . RESOLVED-ID) to cached JSON.
The stored string is the JSON-encoded `((tools . [...]))' alist, with no
outer JSON-RPC wrapper.  Cleared for every alias of a changed server-id by
`anvil-server--tools-list-cache-invalidate' when tools register /
unregister.")

(defun anvil-server--tools-list-cache-invalidate (&optional server-id)
  "Drop tools/list cache entries involving SERVER-ID, or every entry if nil.
SERVER-ID may be either a virtual or resolved registry id.  Invalidating a
resolved id also removes every cached virtual alias of that registry."
  (if server-id
      (let (stale-keys)
        (maphash
         (lambda (key _value)
           (when (or (equal key server-id)
                     (and (consp key)
                          (or (equal (car key) server-id)
                              (equal (cdr key) server-id))))
             (push key stale-keys)))
         anvil-server--tools-list-cache)
        (dolist (key stale-keys)
          (remhash key anvil-server--tools-list-cache)))
    (clrhash anvil-server--tools-list-cache)))

(defun anvil-server--jsonrpc-response-from-result-json (id result-json)
  "Build a JSON-RPC response by wrapping a pre-encoded RESULT-JSON string.
ID is encoded inline.  Used by the tools/list cache fast path to skip
a full json-encode of the (potentially ~20KB) result object."
  (concat "{\"jsonrpc\":\"2.0\",\"id\":"
          (cond
           ((null id) "null")
           ((integerp id) (number-to-string id))
           ((stringp id) (json-encode id))
           (t (json-encode id)))
          ",\"result\":"
          result-json
          "}"))


(defun anvil-server--param-name-matches-arg-p (param-name arg)
  "Return t if PARAM-NAME matches ARG symbol name."
  (string= param-name (symbol-name arg)))

(defun anvil-server--validate-param-for-saving
    (param-name arglist descriptions)
  "Validate that PARAM-NAME can be saved to DESCRIPTIONS.
Check that PARAM-NAME is not already in DESCRIPTIONS and that it
exists in ARGLIST.  Signal an error if validation fails."
  (when (assoc param-name descriptions)
    (error "Duplicate parameter '%s' in MCP Parameters" param-name))
  (unless (cl-member
           param-name
           arglist
           :test #'anvil-server--param-name-matches-arg-p)
    (error
     "Parameter '%s' in MCP Parameters not in function args %S"
     param-name
     arglist)))

(defun anvil-server--extract-param-descriptions (docstring arglist)
  "Extract parameter descriptions from DOCSTRING based on ARGLIST.

The docstring should contain an \"MCP Parameters:\" section with parameters
formatted using indentation-based syntax:

  Parameter definitions (2-4 spaces):
    param-name - description text

  Continuation lines (6+ spaces):
      additional description text
      can span multiple lines

Example:
  \"Function docstring.

  MCP Parameters:
    location - city, address, or coordinates
    verbose - whether to include detailed info
        Set to t for extended weather data
        including wind speed and humidity\"

ARGLIST should be the function's argument list.
Returns an alist mapping parameter names to their descriptions.
Signals an error if a parameter is described multiple times,
doesn't match function arguments, or if any parameter is not documented."
  ;; Validate that all arglist members are symbols
  (dolist (arg arglist)
    (unless (symbolp arg)
      (error "Non-symbol in function arglist: %S" arg)))

  (let ((descriptions nil))
    (when docstring
      (when
          (string-match
           "MCP Parameters:[\n\r]+\\(\\(?:[ \t]+[^ \t\n\r].*[\n\r]*\\)*\\)"
           docstring)
        (let
            ((params-text (match-string 1 docstring))
             ;; Match param definitions: spaces (min-max), name, whitespace, hyphen
             (param-regex
              (format
               "^[ ]\\{%d,%d\\}\\([^ \t\n\r]+\\)[ \t]*-[ \t]*\\(.*\\)$"
               anvil-server--param-indent-min
               anvil-server--param-indent-max))
             ;; Match continuation lines: 6+ spaces
             (continuation-regex
              (format "^[ ]\\{%d,\\}\\(.*\\)$"
                      anvil-server--continuation-indent-min))
             (current-param nil)
             (current-desc nil))
          (with-temp-buffer
            (insert params-text)
            (goto-char (point-min))
            (while (not (eobp))
              (cond
               ;; Parameter definition line
               ((looking-at param-regex)
                ;; Save previous parameter if exists
                (when current-param
                  (anvil-server--validate-param-for-saving
                   current-param arglist descriptions)
                  (push (cons
                         current-param (string-trim current-desc))
                        descriptions))
                ;; Start new parameter
                (setq current-param (match-string 1))
                (setq current-desc (match-string 2))
                (forward-line))
               ;; Continuation line
               ((looking-at continuation-regex)
                (if current-param
                    (setq current-desc
                          (concat current-desc " " (match-string 1)))
                  (error
                   "Continuation line without parameter at line %d: %S"
                   (line-number-at-pos)
                   (string-trim (match-string 0))))
                (forward-line))
               ;; Empty or other line
               (t
                (forward-line))))
            ;; Save last parameter
            (when current-param
              (anvil-server--validate-param-for-saving
               current-param arglist descriptions)
              (push (cons current-param (string-trim current-desc))
                    descriptions)))))
      ;; Check that all function parameters have descriptions
      (dolist (arg arglist)
        (let ((arg-name (symbol-name arg)))
          ;; Skip &optional / &rest markers and `_'-prefixed parameters
          ;; (the Elisp convention for intentionally unused parameters —
          ;; these have no user-facing meaning and need no MCP docs).
          (unless (or (string= arg-name "&optional")
                      (string= arg-name "&rest")
                      (string-prefix-p "_" arg-name)
                      (assoc arg-name descriptions))
            (error
             "Function parameter '%s' missing from MCP Parameters section"
             arg-name)))))
    descriptions))

(defun anvil-server--function-param-descriptions (func arglist)
  "Return MCP parameter descriptions for FUNC using ARGLIST.
For symbol handlers, cache the parsed descriptions on the symbol so repeated
registrations do not re-fetch and re-parse the same docstring."
  (if (symbolp func)
      (let ((cached (get func 'anvil-server-param-descriptions))
            (definition (symbol-function func)))
        (if (and cached
                 (eq definition (plist-get cached :definition))
                 (equal arglist (plist-get cached :arglist)))
            (plist-get cached :descriptions)
          (let* (;; Use RAW=t to prevent substitute-command-keys from
                 ;; converting apostrophes to fancy quotes, preserving exact
                 ;; documentation text.
                 (docstring (documentation func t))
                 (descriptions
                  (anvil-server--extract-param-descriptions
                   docstring arglist)))
            (put func 'anvil-server-param-descriptions
                 (list :arglist arglist
                       :definition definition
                       :descriptions descriptions))
            descriptions)))
    (anvil-server--extract-param-descriptions (documentation func t) arglist)))

(defun anvil-server--generate-schema-from-function (func &optional arglist)
  "Generate JSON schema by analyzing FUNC's signature.
Returns a schema object suitable for tool registration.
Extracts parameter descriptions from the docstring if available.
Parameters prefixed with `_' (the Elisp unused-arg convention) are
hidden from the client-facing schema — `anvil-server--handle-tools-call'
fills them with nil at dispatch time.
If ARGLIST is provided, reuse it instead of calling
`help-function-arglist'.

A description in the docstring's \"MCP Parameters:\" section may start
with a \"(TYPE) \" prefix — one of (boolean), (integer), (number),
(array), (object) — to set the parameter's JSON schema type.  The
prefix is stripped from the client-facing description.  Without a
recognised prefix the type defaults to \"string\"."
  (let ((arglist (or arglist (help-function-arglist func t))))
    (when (memq '&rest arglist)
      (error "MCP tool handlers do not support &rest parameters"))
    (let ((visible-arglist
           (cl-remove-if
            (lambda (a)
              (string-prefix-p "_" (symbol-name a)))
            arglist)))
      (if visible-arglist
          ;; One or more user-visible arguments case
          (let ((param-descriptions
                 (anvil-server--function-param-descriptions
                  func arglist))
                (properties '())
                (required '())
                (seen-optional nil))
            (dolist (arg visible-arglist)
              (let ((param-name (symbol-name arg)))
                (if (string= param-name "&optional")
                    ;; Mark that we've seen &optional
                    (setq seen-optional t)
                  ;; Regular parameter - add to properties
                  (let* ((raw-description
                          (cdr (assoc param-name param-descriptions)))
                         ;; Infer the JSON type from a "(TYPE) ..." prefix
                         ;; in the docstring description.  Recognised:
                         ;; boolean integer number array object.  Absent
                         ;; or unrecognised prefixes keep type "string".
                         (type-match
                          (and raw-description
                               (string-match
                                "\\`(\\(boolean\\|integer\\|number\\|array\\|object\\)) "
                                raw-description)))
                         (json-type
                          (if type-match
                              (match-string 1 raw-description)
                            "string"))
                         (description
                          (if type-match
                              (substring raw-description (match-end 0))
                            raw-description))
                         (property-schema `((type . ,json-type))))
                    ;; Add description if provided
                    (when description
                      (setq property-schema
                            (cons
                             `(description . ,description)
                             property-schema)))
                    ;; Add to properties with original parameter name
                    (push
                     (cons param-name property-schema) properties)
                    ;; Add to required list only if before &optional
                    (unless seen-optional
                      (push param-name required))))))
            `((type . "object")
              (properties . ,(nreverse properties))
              (required . ,(vconcat (nreverse required)))))
        ;; No user-visible arguments case (all args are `_'-prefixed or empty)
        '((type . "object"))))))

(defun anvil-server--normalize-tool-handler (handler)
  "Return registration metadata derived from HANDLER.
Wrappers created by `anvil-server-encode-handler' (or compatible helpers
that set the `anvil-server-raw-handler' / `anvil-server-encode-result'
symbol properties) keep the original handler as the source of truth for
schema extraction and argument binding while transport encoding happens
after execution."
  (let* ((raw-handler
          (if-let ((wrapped (and (symbolp handler)
                                 (get handler 'anvil-server-raw-handler))))
              wrapped
            handler))
         (encode-result
          (and (symbolp handler)
               (get handler 'anvil-server-encode-result))))
    (list :handler raw-handler
          :arglist (help-function-arglist raw-handler t)
          :encode-result encode-result)))

(defun anvil-server--ref-counted-register (key item table)
  "Register ITEM with KEY in TABLE with reference counting.
If KEY already exists, increment its reference count.
Otherwise, add ITEM to TABLE with :ref-count 1.
Returns nil."
  (if-let* ((existing (gethash key table)))
    ;; Item already exists - increment ref count
    (let ((ref-count (or (plist-get existing :ref-count) 1)))
      (plist-put existing :ref-count (1+ ref-count)))
    ;; New item - ensure it has ref-count = 1
    (plist-put item :ref-count 1)
    (puthash key item table))
  nil)

(defun anvil-server--ref-counted-unregister (key table)
  "Unregister item with KEY from TABLE using reference counting.
If reference count > 1, decrement it.
Otherwise, remove the item from TABLE.
Returns t if item was found, nil otherwise."
  (if-let* ((item (gethash key table))
            (ref-count (or (plist-get item :ref-count) 1)))
    (if (> ref-count 1)
        ;; Decrement ref count
        (progn
          (plist-put item :ref-count (1- ref-count))
          t)
      ;; Last reference - remove the item
      (remhash key table)
      t)))

(defun anvil-server--jsonrpc-error (id code message)
  "Create a JSON-RPC error response with ID, error CODE and MESSAGE."
  (json-encode
   `((jsonrpc . "2.0")
     (id . ,id)
     (error . ((code . ,code) (message . ,message))))))

(defun anvil-server--append-optional-fields (alist &rest pairs)
  "Append optional field PAIRS to ALIST.

PAIRS should be alternating keys and values.
Only adds a key-value pair if the value is non-nil.

Example:
  (anvil-server--append-optional-fields
   \\='((uri . \"test://resource\") (name . \"Test\"))
   \\='description description-var
   \\='mimeType mime-type-var)

This adds description only if description-var is non-nil,
and mimeType only if mime-type-var is non-nil."
  (let ((additions nil))
    (while pairs
      (let ((key (pop pairs))
            (value (pop pairs)))
        (when value
          (push (cons key value) additions))))
    (append alist additions)))

(defun anvil-server--respond-with-result
    (request-context result-data)
  "Send RESULT-DATA as response to the client through REQUEST-CONTEXT.

Arguments:
  REQUEST-CONTEXT  The MCP request context from the handler
  RESULT-DATA      The data to return to the client (any Elisp value)

The RESULT-DATA will be automatically converted to JSON-compatible format:
  - Strings, numbers, booleans are sent as-is
  - Symbols are converted to strings
  - Lists are converted to JSON arrays
  - Alists with string keys are converted to JSON objects
  - Other Elisp types are stringified appropriately

Example:
  ;; In a tool handler:
  (anvil-server--respond-with-result
   context
   \\='((status . \"success\")
     (files . [\"file1.txt\" \"file2.txt\"])
     (count . 2)))"
  (let ((id (plist-get request-context :id)))
    (anvil-server--jsonrpc-response id result-data)))

(defun anvil-server--log-json-rpc (direction json-message server-id)
  "Log JSON-RPC message in DIRECTION with JSON-MESSAGE for SERVER-ID.
DIRECTION should be \"in\" for incoming, \"out\" for outgoing.
SERVER-ID must be non-nil and already resolved to a string."
  (when anvil-server-log-io
    (let ((buffer (get-buffer-create "*anvil-server-log*"))
          (direction-prefix
           (if (string= direction "in")
               "->"
             "<-"))
          (direction-name
           (if (string= direction "in")
               "(request)"
             "(response)")))
      (with-current-buffer buffer
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (view-mode 1)
          (insert
           (format "%s %s [server:%s] [%s]\n"
                   direction-prefix
                   direction-name
                   server-id
                   json-message)))))))

(defmacro anvil-server--metrics-bump (place)
  "Increment metrics PLACE, swallowing setf failures on standalone NeLisp.
The `cl-defstruct' setf machinery is not yet wired in standalone NeLisp,
so `(cl-incf (anvil-server-metrics-calls m))' raises `wrong-type-argument:
symbol' there.  Metrics are observability — losing a count is acceptable;
losing dispatch is not.  This macro keeps the host-Emacs path incrementing
normally while standalone silently no-ops."
  (declare (debug t))
  `(condition-case nil (cl-incf ,place) (error nil)))

(defvar anvil-server--current-request-id nil
  "Request id of the JSON-RPC message currently being dispatched.
Bound by `anvil-server-process-jsonrpc' around request dispatch so
that fallback error responses can carry the id of the request that
failed.  JSON-RPC 2.0 requires error responses to echo the request
id; a client cannot match an `id: null' error against its pending
request and may wait forever.")

(defun anvil-server--handle-error (err)
  "Handle error ERR in MCP process by logging and creating an error response.
Returns a JSON-RPC error response string for internal errors."
  (anvil-server--jsonrpc-error
   anvil-server--current-request-id
   anvil-server-jsonrpc-error-internal
   (format "Internal error: %s" (error-message-string err))))

(defun anvil-server--validate-and-dispatch-request
    (request server-id)
  "Process a JSON-RPC REQUEST object and validate JSON-RPC 2.0 compliance.

REQUEST is a parsed JSON object (alist) containing the JSON-RPC request fields.
SERVER-ID is required and identifies which server registry to use.

The function performs JSON-RPC 2.0 validation, checking:
- Protocol version (must be \"2.0\")
- ID field presence (required for regular requests, forbidden for notifications)
- Method field presence (always required)

If validation succeeds, dispatches the request to the appropriate handler.
Returns a JSON-RPC formatted response string, or nil for notifications."
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[VAD] enter"))
  (let* ((t-ag (float-time))
         (jsonrpc (alist-get 'jsonrpc request))
         (_ (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
              (nelisp--write-stderr-line
               (format "[VAD] alist-get-jsonrpc %.4fs val=%S" (- (float-time) t-ag) jsonrpc))))
         (id (alist-get 'id request))
         (method (alist-get 'method request))
         (params (alist-get 'params request))
         (_ (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
              (nelisp--write-stderr-line
               (format "[VAD] 4-alist-get total %.4fs id=%S method=%S"
                       (- (float-time) t-ag) id method))))
         (is-notification
          (and method (string-prefix-p "notifications/" method))))
    ;; Check for JSON-RPC 2.0 compliance first
    (cond
     ;; Return error for non-2.0 requests
     ((not (equal jsonrpc "2.0"))
      (anvil-server--jsonrpc-error
       id
       anvil-server-jsonrpc-error-invalid-request
       "Invalid Request: Not JSON-RPC 2.0"))

     ;; Check if id is present for notifications/* methods
     ((and id is-notification)
      (anvil-server--jsonrpc-error
       nil
       anvil-server-jsonrpc-error-invalid-request
       "Invalid Request: Notifications must not include 'id' field"))
     ;; Check if id is missing
     ((and (not id) (not is-notification))
      (anvil-server--jsonrpc-error
       nil
       anvil-server-jsonrpc-error-invalid-request
       "Invalid Request: Missing required 'id' field"))
     ;; Check if method is missing
     ((not method)
      (anvil-server--jsonrpc-error
       id
       anvil-server-jsonrpc-error-invalid-request
       "Invalid Request: Missing required 'method' field"))

     ;; Process valid request
     (t
      (anvil-server--dispatch-jsonrpc-method
       id method params server-id)))))

;;; Resource Template Support

(defun anvil-server--match-uri-template (uri parsed-template)
  "Match URI against PARSED-TEMPLATE.
Returns:
- alist of captured parameters if URI matches and has variables
- \\='match-no-params if URI matches but template has no variables
- nil if URI doesn't match template"
  (let ((segments (plist-get parsed-template :segments))
        (pos 0)
        (params '())
        (uri-len (length uri)))
    (catch 'no-match
      ;; Handle first segment with scheme - it's always literal
      (let* ((first-segment (car segments))
             (value (plist-get first-segment :value))
             (val-len (length value)))
        ;; First literal segment always contains scheme - handle
        ;; case-insensitive matching
        (unless (<= val-len uri-len)
          (throw 'no-match nil))
        (let* ((uri-part (substring uri 0 val-len))
               (scheme-end (string-match "://" value))
               (scheme-len (+ scheme-end 3)))
          (unless (and (>= (length uri-part) val-len)
                       (>= val-len scheme-len)
                       (let ((val-scheme
                              (substring value 0 scheme-len))
                             (uri-scheme
                              (substring uri-part 0 scheme-len)))
                         (string=
                          (downcase val-scheme)
                          (downcase uri-scheme)))
                       (string=
                        (substring value scheme-len)
                        (substring uri-part scheme-len)))
            (throw 'no-match nil))
          (setq pos val-len)
          (setq segments (cdr segments))))

      ;; Process remaining segments
      (dolist (segment segments)
        (let ((seg-type (plist-get segment :type)))
          (cond
           ;; Literal segment - must match exactly (case-sensitive)
           ((eq seg-type 'literal)
            (let* ((value (plist-get segment :value))
                   (val-len (length value)))
              (unless (and (<= (+ pos val-len) uri-len)
                           (string=
                            value
                            (substring uri pos (+ pos val-len))))
                (throw 'no-match nil))
              (setq pos (+ pos val-len))))
           ;; Variable segment - capture value
           ((eq seg-type 'variable)
            (let* ((var-name (plist-get segment :name))
                   ;; Find next segment to determine delimiter
                   (segment-index (seq-position segments segment))
                   (next-segment
                    (when (< (1+ segment-index) (length segments))
                      (nth (1+ segment-index) segments)))
                   (delimiter
                    (when (and next-segment
                               (eq
                                'literal
                                (plist-get next-segment :type)))
                      (plist-get next-segment :value))))
              (if delimiter
                  ;; Variable followed by literal - match until delimiter
                  (let ((end-pos
                         (string-match (regexp-quote delimiter) uri
                                       pos)))
                    (unless end-pos
                      (throw 'no-match nil))
                    (push (cons var-name (substring uri pos end-pos))
                          params)
                    (setq pos end-pos))
                ;; Variable at end - consume rest
                (push (cons var-name (substring uri pos)) params)
                (setq pos uri-len)))))))
      ;; Check we consumed entire URI
      (when (= pos uri-len)
        ;; Return params or 'match-no-params to distinguish from no match
        (if params
            (nreverse params)
          'match-no-params)))))

(defun anvil-server--find-matching-template (uri templates-table)
  "Find a resource template that matches URI in TEMPLATES-TABLE.
TEMPLATES-TABLE is required and can be nil.
Returns cons of (template . params) or nil if no match found."
  (when templates-table
    (catch 'found
      (maphash
       (lambda (_template-pattern template-data)
         (let* ((parsed (plist-get template-data :parsed))
                (match-result
                 (anvil-server--match-uri-template uri parsed)))
           ;; match-result is nil for no match, 'match-no-params or alist
           (when match-result
             (throw 'found
                    (cons
                     template-data
                     (if (eq match-result 'match-no-params)
                         nil
                       match-result))))))
       templates-table)
      nil)))

(defun anvil-server--execute-resource-handler
    (resource-data uri handler-params id method-metrics)
  "Execute a resource handler and return the response.
RESOURCE-DATA is the plist containing handler and metadata.
URI is the resource URI.
HANDLER-PARAMS are parameters to pass to the handler (nil for direct resources).
ID is the request ID.
METHOD-METRICS is used to track errors."
  (condition-case err
      (let* ((handler (plist-get resource-data :handler))
             (mime-type (plist-get resource-data :mime-type))
             (content
              (if handler-params
                  (funcall handler handler-params)
                (funcall handler)))
             (content-entry
              (anvil-server--append-optional-fields
               `((uri . ,uri) (text . ,content))
               'mimeType mime-type)))
        (anvil-server--jsonrpc-response
         id `((contents . ,(vector content-entry)))))
    ;; Handle resource-specific errors with custom error codes
    (anvil-server-resource-error
     (anvil-server--metrics-bump (anvil-server-metrics-errors method-metrics))
     (let ((code (car (cdr err)))
           (message (cadr (cdr err))))
       (anvil-server--jsonrpc-error id code message)))
    (quit
     (cl-incf (anvil-server-metrics-errors method-metrics))
     (anvil-server--jsonrpc-error
      id anvil-server-jsonrpc-error-internal
      (format "Resource handler quit for %s: %S" uri err)))
    ;; Handle any other error from the handler
    (error
     (anvil-server--metrics-bump (anvil-server-metrics-errors method-metrics))
     (anvil-server--jsonrpc-error
      id anvil-server-jsonrpc-error-internal
      (format "Error reading resource %s: %s"
              uri (error-message-string err))))))

(defun anvil-server--handle-resources-read
    (id params method-metrics server-id)
  "Handle resources/read request with ID and PARAMS for SERVER-ID.
METHOD-METRICS is used to track errors for this method.
SERVER-ID is resolved through `anvil-server-id-aliases'."
  (let* ((uri (alist-get 'uri params))
         (resolved-id (anvil-server--resolve-id server-id))
         (resources-table
          (anvil-server--get-server-resources resolved-id))
         (templates-table
          (anvil-server--get-server-templates resolved-id))
         (resource (gethash uri resources-table))
         (template-match
          (unless resource
            (anvil-server--find-matching-template
             uri templates-table))))
    (cond
     ;; Direct resource found
     (resource
      (anvil-server--execute-resource-handler
       resource uri nil id method-metrics))
     ;; Resource template match found
     (template-match
      (let ((template-data (car template-match))
            (params (cdr template-match)))
        (anvil-server--execute-resource-handler
         template-data uri params id method-metrics)))
     ;; No resource or resource template found
     (t
      (anvil-server--jsonrpc-error
       id
       anvil-server-jsonrpc-error-invalid-params
       (format "Resource not found: %s" uri))))))

(defun anvil-server--dispatch-jsonrpc-method
    (id method params server-id)
  "Dispatch a JSON-RPC request to the appropriate handler for SERVER-ID.
ID is the JSON-RPC request ID to use in response.
METHOD is the JSON-RPC method name to dispatch.
PARAMS is the JSON-RPC params object from the request.
Returns a JSON-RPC response string for the request."
  (let ((method-metrics (anvil-server-metrics--get method)))
    (anvil-server--metrics-bump (anvil-server-metrics-calls method-metrics))

    (cond
     ((equal method "initialize")
      (anvil-server--handle-initialize id server-id))
     ((equal method "notifications/initialized")
      (anvil-server--handle-initialized)
      nil)
     ((equal method "notifications/cancelled")
      nil)
     ((equal method "tools/list")
      (anvil-server--handle-tools-list id server-id))
     ((equal method "resources/list")
      (anvil-server--handle-resources-list id server-id))
     ((equal method "resources/templates/list")
      (anvil-server--handle-resources-templates-list id server-id))
     ((equal method "resources/read")
      (anvil-server--handle-resources-read
       id params method-metrics server-id))
     ((equal method "tools/call")
      (anvil-server--handle-tools-call
       id params method-metrics server-id))
     (t
      (anvil-server--jsonrpc-error
       id
       anvil-server-jsonrpc-error-method-not-found
       (format "Method not found: %s" method))))))

;;; Notification handlers

(defun anvil-server--handle-initialize (id server-id)
  "Handle initialize request with ID for SERVER-ID.
This implements the MCP initialize handshake, which negotiates protocol
version and capabilities between the client and server.  SERVER-ID
is resolved through `anvil-server-id-aliases' for capability lookup."
  (let ((capabilities '())
        (resolved-id (anvil-server--resolve-id server-id)))
    (let ((tools-table (gethash resolved-id anvil-server--tools))
          (resources-table
           (gethash resolved-id anvil-server--resources))
          (templates-table
           (gethash resolved-id anvil-server--resource-templates)))
      (when (and tools-table (> (hash-table-count tools-table) 0))
        (push `(tools . ,(make-hash-table)) capabilities))
      (when (or (and resources-table
                     (> (hash-table-count resources-table) 0))
                (and templates-table
                     (> (hash-table-count templates-table) 0)))
        (push `(resources . ,(make-hash-table)) capabilities))
      (if (fboundp 'nelisp--write-stderr-line)
          ;; Standalone NeLisp can spend tens of seconds in `json-encode'
          ;; even for this tiny fixed response.  Build the result JSON
          ;; directly and wrap it with the existing pre-encoded helper.
          (let ((cap-json
                 (concat "{"
                         (mapconcat
                          #'identity
                          (delq nil
                                (list
                                 (when (assoc 'tools capabilities)
                                   "\"tools\":{}")
                                 (when (assoc 'resources capabilities)
                                   "\"resources\":{}")))
                          ",")
                         "}")))
            (anvil-server--jsonrpc-response-from-result-json
             id
             (concat "{\"protocolVersion\":"
                     (anvil-server--js-string anvil-server-protocol-version)
                     ",\"serverInfo\":{\"name\":"
                     (anvil-server--js-string anvil-server-name)
                     ",\"version\":"
                     (anvil-server--js-string anvil-server-protocol-version)
                     "},\"capabilities\":" cap-json "}")))
        (anvil-server--jsonrpc-response
         id
         `((protocolVersion . ,anvil-server-protocol-version)
           (serverInfo
            .
            ((name . ,anvil-server-name)
             (version . ,anvil-server-protocol-version)))
           (capabilities . ,capabilities)))))))

(defun anvil-server--handle-initialized ()
  "Handle initialized notification from client.

This is called after successful initialization to complete the handshake.
The client sends this notification to acknowledge the server's response
to the initialize request.")

(defvar anvil-server-tool-filter-function nil
  "Optional filter applied to tools/list advertisements.
When bound to a function, it is called with (TOOL-ID TOOL-PLIST
SERVER-ID) for each registered tool and must return non-nil to keep
the tool in the response.  The SERVER-ID argument is the original
(possibly virtual) server-id the client asked for — useful when
filtering per connection, not per daemon.

Handlers remain live either way, so hidden tools stay callable via
explicit tools/call.

Set by `anvil-manifest' (Doc 26) to implement ANVIL_PROFILE.")

(defvar anvil-server-tool-fragment-function nil
  "Optional transformer for tools/list JSON fragments.
When non-nil, called with (TOOL-ID TOOL-PLIST SERVER-ID
DEFAULT-FRAGMENT) for every visible tool.  It must return a JSON
object fragment string, or nil to omit the tool from the response.

Set by `anvil-manifest' Doc 44 Phase 2c to advertise trimmed schemas
for non-focused dynamic-profile tools while keeping full schemas for
Top-K and warm LRU tools.")

(defvar anvil-server-tool-dispatch-hook nil
  "Abnormal hook run after a tool handler returns successfully.
Functions on this hook are called with (TOOL-ID SERVER-ID); the
SERVER-ID is the virtual id the client used (before alias
resolution).  Used by Doc 34 Phase C (`anvil-discovery') to track
per-tool usage counters without coupling anvil-server to anvil-state.

Errors raised by hook functions are caught and logged; they do not
mask or modify the handler's result.  Keep hook functions cheap —
they run on every successful dispatch.")

(defvar anvil-server-tool-error-hook nil
  "Abnormal hook run when a tool handler error is observed.
Functions are called with (ERR TOOL-NAME SOURCE):
  ERR        — the raw error condition cell from `condition-case'.
  TOOL-NAME  — the active tool id, or nil when the hook fires from
               `anvil-server-with-error-handling' without a tool
               name in scope.
  SOURCE     — call-site hint symbol:
                 `tool-body'             from inside the macro or
                                         the dispatcher's generic
                                         `error' arm,
                 `dispatcher-validation' from the dispatcher's
                                         `anvil-server-invalid-params'
                                         arm.

Used by `anvil-harness-telemetry' (Doc 46) to classify failures into
the 4 runtime-harness classes (no-exec / contract-violation / stall /
reasoning) and persist them for measurement of Doc 43-47 effects.

Errors raised by hook functions are caught and logged; they do not
mask the original error.")

(defvar anvil-server--current-tool-name nil
  "Dynamically bound to the active tool name during dispatch.
Lets `anvil-server-tool-error-hook' callbacks attribute errors to
the correct tool even when fired from inside
`anvil-server-with-error-handling', which has no lexical access to
the dispatcher-level name.")

(defun anvil-server--run-tool-error-hook (err tool-name source)
  "Run `anvil-server-tool-error-hook' with ERR / TOOL-NAME / SOURCE.
Hook errors are caught and logged so telemetry never breaks the
main dispatch flow."
  (when anvil-server-tool-error-hook
    (condition-case hook-err
        (run-hook-with-args 'anvil-server-tool-error-hook
                            err tool-name source)
      (error
       (message "anvil-server: tool-error-hook failed: %s"
                (error-message-string hook-err))))))

(defvar anvil-server-id-aliases nil
  "Alist mapping virtual server-ids to real server-ids.
Entries are (VIRTUAL . REAL) string pairs.  Used to advertise the
same set of tool handlers under multiple server-ids, typically so
`anvil-manifest' (Doc 26) can apply a per-connection profile filter
without duplicating the tool registrations.

Example: ((\"emacs-eval-ultra\" . \"emacs-eval\")) makes tools/list
for `emacs-eval-ultra' look up the `emacs-eval' tools table while
the filter function still sees the original virtual server-id and
can pick a different profile for it.")

(defun anvil-server--resolve-id (server-id)
  "Return the real server-id SERVER-ID resolves to.
Follows `anvil-server-id-aliases'.  Safe to call on a real id (it
becomes a no-op)."
  (or (cdr (assoc server-id anvil-server-id-aliases)) server-id))

(defun anvil-server--handle-tools-list (id server-id)
  "Handle tools/list request with ID for SERVER-ID.
Returns a list of registered tools with their metadata, minus any
filtered out by `anvil-server-tool-filter-function'.  SERVER-ID may
be a virtual id registered in `anvil-server-id-aliases'; in that
case tool lookup uses the resolved real id but the filter function
still sees the original virtual id for profile selection.

Perf (2026-05-11): the JSON-encoded `result' object is cached per
server-id in `anvil-server--tools-list-cache' and skips full
json-encode on subsequent calls.  The cache is invalidated by
register / unregister."
  (let* ((resolved-id (anvil-server--resolve-id server-id))
         ;; Filters receive the unresolved virtual id and may intentionally
         ;; expose a different profile per alias, so the cache key retains
         ;; both virtual and resolved ids.  The invalidator removes every key
         ;; whose resolved component changes.
         (cache-key (cons server-id resolved-id))
         ;; Fragment transformers can depend on dynamic query / LRU /
         ;; state-filter context, so cached result JSON would be stale.
         (cached (and (null anvil-server-tool-fragment-function)
                      (gethash cache-key anvil-server--tools-list-cache))))
    (if cached
        (anvil-server--jsonrpc-response-from-result-json id cached)
      ;; Fast path (2026-05-24): collect pre-encoded :json-fragment
      ;; cached on each tool plist at registration time.  Per-request
      ;; concat is O(n) and avoids json-encode entirely.  Original code
      ;; called json-encode on a vector-of-entries which degenerated to
      ;; stuck after 1-2 entries on the pure-elisp interpreter post-
      ;; 2026-05-17.
      (let* ((t0 (float-time))
             (fragments nil))
        (when-let* ((tools-table
                     (gethash resolved-id anvil-server--tools)))
          (maphash
           (lambda (tool-id tool)
             (when (or (null anvil-server-tool-filter-function)
                       (funcall anvil-server-tool-filter-function
                                tool-id tool server-id))
               (let* ((default-frag (plist-get tool :json-fragment))
                      (frag (if anvil-server-tool-fragment-function
                                (funcall anvil-server-tool-fragment-function
                                         tool-id tool server-id
                                         default-frag)
                              default-frag)))
                 (when frag (push frag fragments)))))
           tools-table))
        (let* ((joined (mapconcat #'identity (nreverse fragments) ","))
               (result-json (concat "{\"tools\":[" joined "]}")))
          (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
            (nelisp--write-stderr-line
             (format "[TL] fast-path %.4fs n=%d len=%d"
                     (- (float-time) t0) (length fragments)
                     (length result-json))))
          ;; Standalone already stores per-tool JSON fragments; rebuilding
          ;; this small wrapper is cheap.  Avoid mutating this request cache
          ;; there because post-2026-05-17 NeLisp can stall in hash-table
          ;; mutation after the large tools/list string is built.
          (unless (or anvil-server-tool-fragment-function
                      (fboundp 'nelisp--write-stderr-line))
            (puthash cache-key result-json anvil-server--tools-list-cache))
          (anvil-server--jsonrpc-response-from-result-json id result-json))))))

(defun anvil-server--build-resource-entry
    (uri-or-template resource-data is-template)
  "Build a resource entry for resources/list response.
URI-OR-TEMPLATE is the URI (for direct resources) or URI template.
RESOURCE-DATA is the plist containing resource metadata.
IS-TEMPLATE indicates whether this is a template resource."
  (let* ((name (plist-get resource-data :name))
         (description (plist-get resource-data :description))
         (mime-type (plist-get resource-data :mime-type))
         (uri-field
          (if is-template
              'uriTemplate
            'uri))
         (base-entry
          `((,uri-field . ,uri-or-template) (name . ,name))))
    (anvil-server--append-optional-fields base-entry
                                            'description
                                            description
                                            'mimeType
                                            mime-type)))

(defun anvil-server--collect-resources-from-hash
    (hash-table is-template)
  "Collect resource entries from HASH-TABLE.
IS-TEMPLATE indicates whether these are template resources.
Returns a vector of resource entries."
  (let ((entries (vector)))
    (maphash
     (lambda (uri-or-template resource-data)
       (setq entries
             (vconcat
              entries
              (vector
               (anvil-server--build-resource-entry
                uri-or-template resource-data is-template)))))
     hash-table)
    entries))

(defun anvil-server--handle-resources-list (id server-id)
  "Handle resources/list request with ID for SERVER-ID.
Returns a list of all registered resources with their metadata.
SERVER-ID is resolved through `anvil-server-id-aliases'."
  (let* ((resolved-id (anvil-server--resolve-id server-id))
         (resources-table
          (gethash resolved-id anvil-server--resources))
         (resource-list
          (if resources-table
              (anvil-server--collect-resources-from-hash
               resources-table nil)
            (vector))))
    (anvil-server--jsonrpc-response
     id `((resources . ,resource-list)))))

(defun anvil-server--handle-resources-templates-list (id server-id)
  "Handle resources/templates/list request with ID for SERVER-ID.
Returns a list of all registered resource templates.
SERVER-ID is resolved through `anvil-server-id-aliases'."
  (let* ((resolved-id (anvil-server--resolve-id server-id))
         (templates-table
          (gethash resolved-id anvil-server--resource-templates))
         (template-list
          (if templates-table
              (anvil-server--collect-resources-from-hash
               templates-table t)
            (vector))))
    (anvil-server--jsonrpc-response
     id `((resourceTemplates . ,template-list)))))

(defun anvil-server--handle-tools-call
    (id params method-metrics server-id)
  "Handle tools/call request with ID and PARAMS for SERVER-ID.
METHOD-METRICS is used to track errors for this method.  SERVER-ID
is resolved through `anvil-server-id-aliases' before tool lookup so
virtual server-ids share the same handler pool."
  (let* ((tool-name (alist-get 'name params))
         (resolved-id (anvil-server--resolve-id server-id))
         (tools-table (gethash resolved-id anvil-server--tools))
         (tool
          (when tools-table
            (gethash tool-name tools-table)))
         (tool-args (alist-get 'arguments params)))
    (when (and tool
               (plist-get tool :lazy-placeholder)
               (functionp (plist-get tool :lazy-loader)))
      (funcall (plist-get tool :lazy-loader) tool-name resolved-id)
      (setq tools-table (gethash resolved-id anvil-server--tools))
      (setq tool
            (when tools-table
              (gethash tool-name tools-table))))
    (if tool
        (let ((handler (plist-get tool :handler))
              (context (list :id id)))
          (condition-case err
              (let*
                  ((arglist
                    (progn
                      (unless (or (functionp handler)
                                  (and (symbolp handler) (fboundp handler)))
                        (signal 'void-function (list handler)))
                      ;; Keep dispatch aligned with registration-time
                      ;; schema extraction, including transport-only
                      ;; wrappers created by `anvil-server-encode-handler'.
                      (or (plist-get tool :arglist)
                          (help-function-arglist handler t))))
                   (expected-params '())
                   (required-params '())
                   (provided-params '())
                   (arg-values '())
                   (seen-optional nil)
                   (raw-result
                    (progn
                      ;; Collect expected and required parameter names.
                      ;; `_'-prefixed args follow the Elisp unused-arg
                      ;; convention — they are hidden from the client-facing
                      ;; schema (see `anvil-server--generate-schema-from-function')
                      ;; and must be skipped here too.
                      (dolist (param arglist)
                        (let ((param-name (symbol-name param)))
                          (cond
                           ((string= param-name "&optional")
                            (setq seen-optional t))
                           ((string-prefix-p "_" param-name)
                            nil)
                           (t
                            (push (intern param-name) expected-params)
                            (unless seen-optional
                              (push (intern param-name)
                                    required-params))))))
                      ;; Collect provided parameter names
                      (dolist (arg tool-args)
                        (push (car arg) provided-params))
                      ;; Check for missing required parameters
                      (dolist (required required-params)
                        (unless (memq required provided-params)
                          (signal
                           'anvil-server-invalid-params
                           (list
                            (format
                             (concat
                              "Missing required parameter: %s. "
                              "Each parameter must be a separate field "
                              "in the JSON input object — do not embed "
                              "XML-style tags such as </%s> or "
                              "<parameter name=\"%s\"> inside another "
                              "parameter's string value.")
                             required required required)))))
                      ;; Check for unexpected parameters.  `_'-prefixed
                      ;; names are silently accepted even when not in
                      ;; `expected-params' — a stale client that still has
                      ;; a pre-fix schema may send them.  We just drop them.
                      (dolist (provided provided-params)
                        (unless
                            (or (string-prefix-p
                                 "_" (symbol-name provided))
                                (memq provided expected-params))
                          (signal
                           'anvil-server-invalid-params
                           (list
                            (format "Unexpected parameter: %s"
                                    provided)))))
                      ;; All validation passed, collect values and call handler.
                      ;; `_'-prefixed args always receive nil (client can't send them).
                      (dolist (param arglist)
                        (let ((param-name (symbol-name param)))
                          (unless (string= param-name "&optional")
                            (let ((value
                                   (if (string-prefix-p "_" param-name)
                                       nil
                                     (alist-get
                                      (intern param-name) tool-args))))
                              (push value arg-values)))))
                      (let ((final-args (nreverse arg-values))
                            (anvil-server--current-tool-name tool-name))
                        (if (plist-get tool :offload)
                            (anvil-server--offload-apply
                             tool handler final-args)
                          (apply handler final-args)))))
                   ;; `anvil-server-encode-handler' marks tools that want
                   ;; transport-level JSON encoding while their raw
                   ;; handler keeps returning rich Lisp data.
                   (result
                    (if (plist-get tool :encode-result)
                        (anvil-server-encode-for-mcp raw-result)
                      raw-result))
                   ;; Coerce result to string for MCP transport.
                   ;;
                   ;; Contract widen (T118 / T97-FOLLOWUP-1):
                   ;;   string / nil  → pass through (legacy)
                   ;;   plist / list / cons / hash-table / vector
                   ;;                 → JSON-encode via
                   ;;                   `anvil-server-encode-for-mcp'
                   ;;   symbol / function / buffer / process / marker
                   ;;                 → reject (cannot be wire-encoded)
                   ;;
                   ;; This removes the "string or nil" hard reject that
                   ;; surfaced as `defs-index-status` etc returning a
                   ;; plist (= boot-smoke WARN, T97 M1 / T110 M2).  The
                   ;; existing `:encode-result' wrapper path stays the
                   ;; preferred long-form (it round-trips through the
                   ;; raw handler for ERT), but un-wrapped handlers no
                   ;; longer need to hand-stringify.
                   (result-text
                    (cond
                     ((null result)
                      "")
                     ((stringp result)
                      result)
                     ((or (consp result)        ; plist / alist / list / dotted
                          (hash-table-p result)
                          (vectorp result))
                      (anvil-server-encode-for-mcp result))
                     (t
                      (signal
                       'anvil-server-invalid-params
                       (list
                        (format
                         (concat
                          "Tool handler must return a JSON-serialisable"
                          " value (string / nil / plist / list /"
                          " hash-table / vector), got: %s")
                         (type-of result)))))))
                   (result-text
                    (if (fboundp 'anvil-disclosure-budget-apply)
                        (anvil-disclosure-budget-apply tool-name result-text)
                      result-text))
                   ;; Wrap the handler result in the MCP format
                   (formatted-result
                    `((content
                       .
                       ,(vector
                         `((type . "text") (text . ,result-text))))
                      (isError . :json-false))))
                (anvil-server-metrics--track-tool-payload
                 tool-name tool-args result-text)
                (anvil-server-metrics--track-tool-call tool-name)
                (condition-case hook-err
                    (run-hook-with-args
                     'anvil-server-tool-dispatch-hook
                     tool-name server-id)
                  (error
                   (message "anvil-server: dispatch-hook error on %s: %s"
                            tool-name
                            (error-message-string hook-err))))
                (anvil-server--respond-with-result
                 context formatted-result))
            ;; Handle invalid parameter errors
            (anvil-server-invalid-params
             (anvil-server-metrics--track-tool-call tool-name t)
             (anvil-server--metrics-bump (anvil-server-metrics-errors method-metrics))
             (anvil-server--run-tool-error-hook
              err tool-name 'dispatcher-validation)
             (anvil-server--jsonrpc-error
              id
              anvil-server-jsonrpc-error-invalid-params
              (cadr err)))
            ;; Handle tool-specific errors thrown with
            ;; anvil-server-tool-throw
            (anvil-server-tool-error
             (anvil-server-metrics--track-tool-call tool-name t)
             (anvil-server--metrics-bump (anvil-server-metrics-errors method-metrics))
             (let ((formatted-error
                    `((content
                       .
                       ,(vector
                         `((type . "text") (text . ,(cadr err)))))
                      (isError . t))))
               (anvil-server--respond-with-result
                context formatted-error)))
            (quit
             (anvil-server-metrics--track-tool-call tool-name t)
             (cl-incf (anvil-server-metrics-errors method-metrics))
             (anvil-server--jsonrpc-error
              id anvil-server-jsonrpc-error-internal
              (format "Tool handler quit: %S" err)))
            ;; Keep existing handling for all other errors
            (error
             (anvil-server-metrics--track-tool-call tool-name t)
             (anvil-server--metrics-bump (anvil-server-metrics-errors method-metrics))
             (anvil-server--run-tool-error-hook
              err tool-name 'tool-body)
             (anvil-server--jsonrpc-error
              id anvil-server-jsonrpc-error-internal
              (format "Internal error executing tool: %s"
                      (error-message-string err))))))
      (anvil-server-metrics--track-tool-call tool-name t)
      (anvil-server--metrics-bump (anvil-server-metrics-errors method-metrics))
      (anvil-server--jsonrpc-error
       id
       anvil-server-jsonrpc-error-invalid-request
       (format "Tool not found: %s" tool-name)))))

;;; Offload dispatch (Doc 03 Phase 2b)

(defcustom anvil-server-offload-default-timeout 60
  "Default wall-clock timeout (seconds) for offloaded tool calls.
Overridden per-tool by `:offload-timeout' on `anvil-server-register-tool'."
  :type 'integer
  :group 'anvil-server)

(defun anvil-server--offload-auto-derive (handler)
  "Return (FEATURE . (DIR)) derived from HANDLER's `symbol-file', or nil.
Used by `anvil-server--offload-apply' as a fallback when the tool
was registered without `:offload-require' / `:offload-load-path'.
The feature name is the basename of the source file, matching
the anvil convention (`anvil-X.el' provides `anvil-X')."
  (when-let* ((file (ignore-errors (symbol-file handler 'defun)))
              (dir (file-name-directory file))
              (base (file-name-base file)))
    (cons (intern base) (list dir))))

(defun anvil-server--offload-apply (tool handler args)
  "Run HANDLER with ARGS in the offload REPL, return its value.
TOOL is the registered tool plist (for :offload-require etc.).
When the tool omits both :offload-require and :offload-load-path,
auto-derive them from the handler's source file so the common case
\(pure tool in an anvil-* module) works with just `:offload t'.
Signals `anvil-server-tool-error' on timeout or remote error."
  (require 'anvil-offload)
  (unless (symbolp handler)
    (signal 'anvil-server-tool-error
            (list (format "Offload requires a symbol handler, got %S"
                          handler))))
  (let* ((form `(apply #',handler ',args))
         (explicit-req (plist-get tool :offload-require))
         (explicit-lp  (plist-get tool :offload-load-path))
         (inherit-lp   (plist-get tool :offload-inherit-load-path))
         (auto (unless (or explicit-req explicit-lp)
                 (anvil-server--offload-auto-derive handler)))
         (requires (or explicit-req (car auto)))
         (base-lp (or explicit-lp (cdr auto)))
         ;; When :offload-inherit-load-path is set, snapshot the daemon's
         ;; `load-path' at dispatch time and hand it to the subprocess so
         ;; handlers that `(require)' arbitrary user code — byte-compile
         ;; being the motivating case — resolve the same way as in the
         ;; daemon.  Pure tools should leave this off so pool slots stay
         ;; lean.
         (extra-load-path (if inherit-lp (append base-lp load-path) base-lp))
         (timeout (or (plist-get tool :offload-timeout)
                      anvil-server-offload-default-timeout))
         (resumable (plist-get tool :resumable))
         (future (anvil-offload form
                                :require requires
                                :load-path extra-load-path)))
    ;; A quit, error, or timeout must never abandon a pending future.  The
    ;; unwind cleanup clears and hard-kills its pool slot synchronously so
    ;; the next request cannot queue behind the interrupted REPL form.
    (unwind-protect
        (if (not (anvil-future-await future timeout))
            ;; Budget exceeded — snapshot the latest checkpoint BEFORE the
            ;; hard-kill (kill clears pending state and we want the last
            ;; observed partial state), then kill so the subprocess slot
            ;; does not stay wedged by the runaway call.  Either surface a
            ;; `partial' plist (for `:resumable t' tools, folding in the
            ;; checkpoint's :value / :cursor when present) or an error.
            (let* ((elapsed (anvil-future-elapsed future))
                   (cp (and resumable (anvil-future-checkpoint future))))
              (anvil-future-kill future)
              (if resumable
                  (format "%S" (list :status 'partial
                                     :value (plist-get cp :value)
                                     :cursor (plist-get cp :cursor)
                                     :consumed-sec elapsed
                                     :reason 'budget-exceeded))
                (signal 'anvil-server-tool-error
                        (list (format "Offload budget exceeded after %.2fs"
                                      elapsed)))))
          (pcase (anvil-future-status future)
            ('done (anvil-future-value future))
            ('error (signal 'anvil-server-tool-error
                            (list (format "Offload error: %s"
                                          (anvil-future-error future)))))
            ('killed (signal 'anvil-server-tool-error
                             (list (format "Offload killed: %s"
                                           (anvil-future-error future)))))
            (status (signal 'anvil-server-tool-error
                            (list (format "Offload unexpected status: %s"
                                          status))))))
      (when (eq 'pending (anvil-future-status future))
        (let ((inhibit-quit t))
          (anvil-future-kill future))))))

;;; Error handling helpers

(defmacro anvil-server-with-error-handling (&rest body)
  "Execute BODY with automatic error handling for MCP tools.

Any error that occurs during BODY execution is caught and converted to
an MCP tool error using `anvil-server-tool-throw'.  This ensures
consistent error reporting to LLM clients.

Arguments:
  BODY  Forms to execute with error handling

Returns the result of BODY execution if successful.

Example:
  (defun my-tool-handler (path)
    \"Read and process a file at PATH.\"
    (anvil-server-with-error-handling
      ;; Any errors here will be caught and reported properly
      (with-temp-buffer
        (insert-file-contents path)
        (process-buffer-contents))))

See also: `anvil-server-tool-throw'"
  `(condition-case err
       (progn
         ,@body)
     (quit
      (anvil-server-tool-throw
       "Interrupted (quit) — the user pressed C-g.  Avoid running code that blocks the Emacs UI."))
     (error
      (anvil-server--run-tool-error-hook
       err anvil-server--current-tool-name 'tool-body)
      (anvil-server-tool-throw (format "Error: %S" err)))))

;;; Tool helpers

;;; API - Server


;;; API - Transport

(defun anvil-server-process-jsonrpc (json-string server-id)
  "Process a JSON-RPC message JSON-STRING for SERVER-ID and return the response.
This is the main entry point for stdio transport in MCP.

The function accepts a JSON-RPC 2.0 message string and returns
a JSON-RPC response string suitable for returning to clients via stdout.

When using the MCP server with emacsclient, invoke this function like:
emacsclient -e \\='(anvil-server-process-jsonrpc
                  \"[JSON-RPC message]\" \"my-server\")\\='

See also: `anvil-server-process-jsonrpc-parsed'"
  (unless anvil-server--running
    (if anvil-server-autostart-on-request
        (anvil-server-start)
      (error
       "No active MCP server, start server with `anvil-server-start' first")))

  (anvil-server--log-json-rpc "in" json-string server-id)

  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line
     (format "[PJ] in t=%.4f json-len=%d" (float-time) (length json-string))))

  ;; Step 1: Try to parse the JSON, handle parsing errors
  (let ((json-object nil)
        (response nil))
    (let ((t0 (float-time))
          (decoded nil))
      (condition-case json-err
          (progn
            ;; Bypass decode-coding-string on standalone nelisp:
            ;; read-stdin-bytes already returns a UTF-8 string, and
            ;; decode-coding-string on it produces a multibyte string
            ;; whose internal representation makes json-read-from-string
            ;; pathologically slow (> 600 s observed for 150-byte body).
            (setq decoded
                  (if (fboundp 'nelisp--write-stderr-line)
                      ;; Standalone nelisp: read-stdin-bytes already returns
                      ;; UTF-8 text; decode-coding-string would make json-read
                      ;; pathologically slow, so pass through unchanged.
                      json-string
                    ;; Host Emacs: the JSON-RPC line arrives as a unibyte
                    ;; (raw UTF-8 byte) string, so decode to multibyte before
                    ;; json-read.  Otherwise CJK argument values come back
                    ;; unibyte and `search-forward' / `re-search-forward'
                    ;; never match the multibyte file buffer (manifested as
                    ;; file-replace-string "string not found" on Japanese
                    ;; while pure-ASCII args were unaffected).
                    (if (and (fboundp 'multibyte-string-p)
                             (multibyte-string-p json-string))
                        json-string
                      (decode-coding-string json-string 'utf-8 t))))
            (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
              (nelisp--write-stderr-line
               (format "[PJ] decode %.4fs decoded-len=%d"
                       (- (float-time) t0) (length decoded))))
            ;; Standalone nelisp: json-read-from-string and string-match
            ;; both return nil on raw-byte strings from read-stdin-bytes,
            ;; so use a char-by-char scan that bypasses both.
            ;; Host Emacs: json-read works fine and is needed because
            ;; the scan-extract path drops `params' (real tool dispatch
            ;; needs full alist).  Gate on a standalone-only fboundp.
            (let ((t1 (float-time)))
              (if (fboundp 'nelisp--write-stderr-line)
                  ;; standalone path
                  (let* ((sx-id (anvil-server--scan-json-value-after
                                 decoded "\"id\":"))
                         (sx-method (anvil-server--scan-string-after
                                     decoded "\"method\":"))
                         ;; For `tools/call' the handler needs
                         ;; `(name . X) (arguments . ALIST)' in params.
                         ;; Extract both with a flat-object scanner.
                         (sx-params
                          (when (equal sx-method "tools/call")
                            (let ((nm (anvil-server--scan-string-after
                                       decoded "\"name\":"))
                                  (args (anvil-server--scan-flat-object-after
                                         decoded "\"arguments\":")))
                              `((name . ,nm) (arguments . ,args))))))
                    (setq json-object
                          `((jsonrpc . "2.0")
                            (id . ,sx-id)
                            (method . ,sx-method)
                            (params . ,sx-params)))
                    (when anvil-server--debug-trace
                      (nelisp--write-stderr-line
                       (format "[PJ] scan-extract %.4fs id=%S method=%S name=%S"
                               (- (float-time) t1) sx-id sx-method
                               (and sx-params (alist-get 'name sx-params))))))
                ;; host Emacs path — normal json-read
                (setq json-object (json-read-from-string decoded)))))
        (json-error
         (setq response
               (anvil-server--jsonrpc-error
                nil anvil-server-jsonrpc-error-parse
                (format "Parse error: %s"
                        (error-message-string json-err)))))))
    (unless response
      (let ((t0 (float-time)))
        (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
          (nelisp--write-stderr-line "[PJ] dispatch-start"))
        (let ((anvil-server--current-request-id
               (when json-object (cdr (assq 'id json-object))))
              (dispatch
               (lambda ()
                 (condition-case err
                     (setq response
                           (anvil-server--validate-and-dispatch-request
                            json-object server-id))
                   (quit
                    (setq response (anvil-server--handle-error err)))
                   (error
                    (setq response (anvil-server--handle-error err)))))))
          (if anvil-server-dispatch-timeout
              (with-timeout (anvil-server-dispatch-timeout
                             (setq response
                                   (anvil-server--jsonrpc-error
                                    (when json-object
                                      (cdr (assq 'id json-object)))
                                    anvil-server-jsonrpc-error-internal
                                    (format "Request timed out after %s seconds"
                                            anvil-server-dispatch-timeout))))
                (funcall dispatch))
            (funcall dispatch)))
        (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
          (nelisp--write-stderr-line
           (format "[PJ] validate+dispatch %.4fs resp-len=%d"
                   (- (float-time) t0)
                   (if response (length response) -1))))))
    (when response
      (anvil-server--log-json-rpc "out" response server-id))
    (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
      (nelisp--write-stderr-line
       (format "[PJ] out t=%.4f" (float-time))))
    response))

(defun anvil-server-process-jsonrpc-to-file
    (json-string server-id response-stage sequence max-bytes
                 expected-device expected-inode)
  "Process JSON-STRING once and publish its response from RESPONSE-STAGE.
RESPONSE-STAGE is a precommitted zero-length 0600 inode in a canonical
0700 directory owned by the current user.  EXPECTED-DEVICE and
EXPECTED-INODE bind that inode before stateful dispatch.  SEQUENCE is a
positive bridge-local request number and MAX-BYTES is a positive hard
limit.

After dispatch, write the exact stage inode and publish two no-clobber hard
links, response.SEQUENCE.json and proof.SEQUENCE.json.  Both names must share
the precommitted identity.  Return a small sequence and byte-count marker.
The bridge retains an open descriptor to the stage inode and treats the
authenticated link pair as authoritative even if marker transport is lost."
  (unless (and (stringp response-stage)
               (file-name-absolute-p response-stage))
    (error "Anvil response stage must be an absolute path"))
  (unless (and (integerp sequence) (> sequence 0))
    (error "Anvil response sequence must be a positive integer"))
  (unless (and (integerp max-bytes) (> max-bytes 0))
    (error "Anvil response maximum must be a positive integer"))
  (unless (and (integerp expected-device) (>= expected-device 0)
               (integerp expected-inode) (> expected-inode 0))
    (error "Anvil response identity must contain device and inode integers"))
  (let* ((expanded-stage (expand-file-name response-stage))
         (stage-basename (file-name-nondirectory expanded-stage))
         (directory
          (file-name-as-directory
           (file-name-directory expanded-stage)))
         (canonical-directory
          (file-name-as-directory (file-truename directory)))
         (canonical-stage
          (expand-file-name stage-basename canonical-directory))
         (final-file
          (expand-file-name
           (format "response.%d.json" sequence)
           canonical-directory))
         (proof-file
          (expand-file-name
           (format "proof.%d.json" sequence)
           canonical-directory))
         (directory-attributes
          (file-attributes canonical-directory 'integer))
         (stage-attributes
          (file-attributes canonical-stage 'integer)))
    (cl-labels
        ((same-identity-p
          (attributes)
          (and attributes
               (equal
                (file-attribute-device-number attributes)
                expected-device)
               (equal
                (file-attribute-inode-number attributes)
                expected-inode)))
         (parent-safe-p
          ()
          (let ((attributes
                 (file-attributes canonical-directory 'integer)))
            (and attributes
                 (eq t (file-attribute-type attributes))
                 (equal
                  (file-attribute-user-id attributes)
                  (user-uid))
                 (= #o700 (file-modes canonical-directory))
                 (equal
                  (file-attribute-device-number attributes)
                  (file-attribute-device-number directory-attributes))
                 (equal
                  (file-attribute-inode-number attributes)
                  (file-attribute-inode-number directory-attributes)))))
         (response-file-safe-p
          (path link-numbers size)
          (let ((attributes (file-attributes path 'integer)))
            (and (same-identity-p attributes)
                 (null (file-attribute-type attributes))
                 (not (file-symlink-p path))
                 (equal
                  (file-attribute-user-id attributes)
                  (user-uid))
                 (memq
                  (file-attribute-link-number attributes)
                  link-numbers)
                 (= #o600 (file-modes path))
                 (= size (file-attribute-size attributes)))))
         (name-absent-p
          (path)
          (and (not (file-exists-p path))
               (not (file-symlink-p path))))
         (delete-owned-name
          (path)
          (let ((attributes (file-attributes path 'integer)))
            (when (same-identity-p attributes)
              (ignore-errors (delete-file path)))))
         (published-pair-p
          (size)
          (and (parent-safe-p)
               (response-file-safe-p final-file '(2 3) size)
               (response-file-safe-p proof-file '(2 3) size))))
      (unless
          (and (equal directory canonical-directory)
               (equal expanded-stage canonical-stage)
               (string-match-p
                (format "^\\.response-tmp\\.%d\\.[^/]+$" sequence)
                stage-basename)
               directory-attributes
               (eq t (file-attribute-type directory-attributes))
               (equal
                (file-attribute-user-id directory-attributes)
                (user-uid))
               (= #o700 (file-modes canonical-directory))
               (same-identity-p stage-attributes)
               (null (file-attribute-type stage-attributes))
               (not (file-symlink-p canonical-stage))
               (equal
                (file-attribute-user-id stage-attributes)
                (user-uid))
               (= 1 (file-attribute-link-number stage-attributes))
               (= #o600 (file-modes canonical-stage))
               (= 0 (file-attribute-size stage-attributes))
               (name-absent-p final-file)
               (name-absent-p proof-file))
        (error "Unsafe Anvil response stage: %s" response-stage))
      (let ((response
             (or (anvil-server-process-jsonrpc json-string server-id) "")))
        (let ((inhibit-quit t)
              (file-name-handler-alist nil)
              bytes
              byte-count
              committed)
          (unwind-protect
              (progn
                (setq bytes (encode-coding-string response 'utf-8 t))
                (when (> (string-bytes bytes) max-bytes)
                  (setq response
                        (condition-case nil
                            (let* ((json-object-type 'alist)
                                   (json-key-type 'symbol)
                                   (request
                                    (json-read-from-string json-string))
                                   (id-cell
                                    (and (listp request)
                                         (assq 'id request))))
                              (if id-cell
                                  (anvil-server--jsonrpc-error
                                   (cdr id-cell)
                                   anvil-server-jsonrpc-error-internal
                                   (format
                                    (concat
                                     "Response exceeded bridge maximum "
                                     "of %d bytes")
                                    max-bytes))
                                ""))
                          (error "")))
                  (setq bytes
                        (encode-coding-string response 'utf-8 t))
                  (when (> (string-bytes bytes) max-bytes)
                    (setq response ""
                          bytes "")))
                (setq byte-count (string-bytes bytes))
                (unless
                    (and (parent-safe-p)
                         (response-file-safe-p
                          canonical-stage '(1) 0)
                         (name-absent-p final-file)
                         (name-absent-p proof-file))
                  (error
                   "Anvil response transaction changed during dispatch"))
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert bytes)
                  (let ((backup-inhibited t)
                        (create-lockfiles nil)
                        (coding-system-for-write 'no-conversion)
                        (write-region-annotate-functions nil)
                        (write-region-post-annotation-function nil)
                        (write-region-inhibit-fsync nil))
                    (write-region
                     (point-min) (point-max)
                     canonical-stage nil 'silent)))
                (set-file-modes canonical-stage #o600)
                (unless
                    (and (parent-safe-p)
                         (response-file-safe-p
                          canonical-stage '(1) byte-count)
                         (name-absent-p final-file)
                         (name-absent-p proof-file))
                  (error "Anvil response stage changed while writing"))
                (add-name-to-file canonical-stage final-file)
                (unless
                    (and (parent-safe-p)
                         (response-file-safe-p
                          canonical-stage '(2) byte-count)
                         (response-file-safe-p
                          final-file '(2) byte-count)
                         (name-absent-p proof-file))
                  (error "Anvil response final link changed identity"))
                (add-name-to-file canonical-stage proof-file)
                (unless
                    (and (parent-safe-p)
                         (response-file-safe-p
                          canonical-stage '(3) byte-count)
                         (response-file-safe-p
                          final-file '(3) byte-count)
                         (response-file-safe-p
                          proof-file '(3) byte-count))
                  (error "Anvil response proof link changed identity"))
                (delete-file canonical-stage)
                (unless
                    (and (parent-safe-p)
                         (response-file-safe-p
                          final-file '(2) byte-count)
                         (response-file-safe-p
                          proof-file '(2) byte-count))
                  (error "Anvil response publication did not converge"))
                (setq committed t)
                (format "anvil-mcp-response-staged:%d:%d"
                        sequence byte-count))
            (unless committed
              (when (and byte-count
                         (published-pair-p byte-count))
                (delete-owned-name canonical-stage)
                (setq committed
                      (and
                       (response-file-safe-p
                        final-file '(2) byte-count)
                       (response-file-safe-p
                        proof-file '(2) byte-count))))
              (unless committed
                (delete-owned-name proof-file)
                (delete-owned-name final-file)
                (delete-owned-name canonical-stage)))))))))

(defun anvil-server-process-jsonrpc-parsed (request server-id)
  "Send REQUEST to the MCP server and return parsed response for SERVER-ID.
REQUEST should be a JSON string containing a valid JSON-RPC 2.0 request.

Call `anvil-server-process-jsonrpc' and return its result as a parsed alist."
  (json-read-from-string
   (anvil-server-process-jsonrpc request server-id)))

;;; API - MCP Content-Length framing (T71)
;;
;; Standard MCP stdio transport uses HTTP-like framing:
;;
;;     Content-Length: <N>\r\n
;;     \r\n
;;     <N bytes of UTF-8 JSON body>
;;
;; The legacy line-delimited JSON path (one JSON object per line) is kept
;; as a backward-compatibility / dev-mode fallback when no
;; `Content-Length:' header is detected.  See
;; `anvil-server-run-batch-stdio' for the dispatcher that auto-detects
;; mode.

(defconst anvil-server-mcp-framing-header-name "Content-Length"
  "Header name used for MCP stdio Content-Length framing.")

(defun anvil-server-mcp-frame-encode (json-string)
  "Encode JSON-STRING as an MCP Content-Length framed message.

Returns a unibyte string of the form

    Content-Length: <N>\\r\\n\\r\\n<body>

where N is the UTF-8 byte length of JSON-STRING.  JSON-STRING is
encoded to UTF-8 before length is computed so that multi-byte
characters report the correct on-the-wire byte count.

Raises `wrong-type-argument' if JSON-STRING is not a string."
  (unless (stringp json-string)
    (signal 'wrong-type-argument (list 'stringp json-string)))
  (let* ((body (encode-coding-string json-string 'utf-8 t))
         (n (length body))
         (header (format "%s: %d\r\n\r\n"
                         anvil-server-mcp-framing-header-name n)))
    ;; Header is ASCII, body is already a unibyte UTF-8 string.
    (concat (encode-coding-string header 'utf-8 t) body)))

(defun anvil-server-mcp-parse-content-length-header (header-block)
  "Parse HEADER-BLOCK and return the Content-Length integer or nil.

HEADER-BLOCK is the raw text up to (but not including) the empty
line that terminates the header section — i.e. CR-LF separated
lines, no trailing CRLF CRLF.

Header names are matched case-insensitively (RFC 7230 §3.2).  Returns
nil if the header is absent or its value is not a non-negative
integer."
  (when (stringp header-block)
    (let ((case-fold-search t)
          (re (format "^[ \t]*%s[ \t]*:[ \t]*\\([0-9]+\\)[ \t]*\\(?:\r\\)?$"
                      (regexp-quote anvil-server-mcp-framing-header-name))))
      (when (string-match re header-block)
        (string-to-number (match-string 1 header-block))))))

(defun anvil-server-mcp-frame-parse-string (input)
  "Parse a single MCP framed message from INPUT string.

Returns a plist (:body BODY :consumed N) where BODY is the decoded
UTF-8 JSON string and N is the number of bytes consumed from INPUT,
or nil if INPUT does not yet contain a complete framed message.

Signals an error tagged `anvil-mcp-frame-error' on malformed
framing (e.g. missing Content-Length header)."
  (unless (stringp input)
    (signal 'wrong-type-argument (list 'stringp input)))
  (let* ((unibyte (if (multibyte-string-p input)
                      (encode-coding-string input 'utf-8 t)
                    input))
         ;; \r\n\r\n separator between header section and body.
         (sep "\r\n\r\n")
         (sep-pos (string-match (regexp-quote sep) unibyte)))
    (cond
     ((null sep-pos)
      ;; Header section incomplete.
      nil)
     (t
      (let* ((header-block (substring unibyte 0 sep-pos))
             (body-start (+ sep-pos (length sep)))
             (n (anvil-server-mcp-parse-content-length-header
                 header-block)))
        (cond
         ((null n)
          (signal 'anvil-mcp-frame-error
                  (list "Missing or invalid Content-Length header"
                        header-block)))
         ((< (- (length unibyte) body-start) n)
          ;; Body not yet fully buffered.
          nil)
         (t
          (let* ((body-bytes (substring unibyte body-start
                                        (+ body-start n)))
                 (body (decode-coding-string body-bytes 'utf-8 t))
                 (consumed (+ body-start n)))
            (list :body body :consumed consumed)))))))))

(define-error 'anvil-mcp-frame-error
  "Malformed MCP Content-Length framing")

(defun anvil-server-mcp-detect-framing-p (initial)
  "Return non-nil if INITIAL bytes look like MCP Content-Length framing.

A heuristic: the first non-whitespace byte starts the
`Content-Length:' header (case-insensitive).  This lets the batch
stdio loop sniff the very first line without consuming it."
  (when (stringp initial)
    (let ((case-fold-search t)
          (trimmed (string-trim-left initial)))
      (string-prefix-p
       (concat anvil-server-mcp-framing-header-name ":")
       trimmed
       t))))

;;; API - Utilities

(defun anvil-server-create-tools-list-request (&optional id)
  "Create a tools/list JSON-RPC request with optional ID.
If ID is not provided, it defaults to 1."
  (json-encode
   `(("jsonrpc" . "2.0")
     ("method" . "tools/list")
     ("id" . ,(or id 1)))))

(defun anvil-server-create-tools-call-request
    (tool-name &optional id args)
  "Create a tools/call JSON-RPC request for TOOL-NAME.
Optional ID and ARGS are also supported.
TOOL-NAME is the registered identifier of the tool to call.
ID is the JSON-RPC request ID, defaults to 1 if not provided.
ARGS is an association list of arguments to pass to the tool.

Example:
  (anvil-server-create-tools-call-request
   \"list-files\" 42 \\='((\"path\" . \"/tmp\")))"
  (json-encode
   `(("jsonrpc" . "2.0")
     ("method" . "tools/call") ("id" . ,(or id 1))
     ("params" .
      (("name" . ,tool-name) ("arguments" . ,(or args '())))))))

(defun anvil-server-create-resources-list-request (&optional id)
  "Create a resources/list JSON-RPC request with optional ID.
If ID is not provided, it defaults to 1."
  (json-encode
   `(("jsonrpc" . "2.0")
     ("method" . "resources/list")
     ("id" . ,(or id 1)))))

(defun anvil-server-create-resources-templates-list-request
    (&optional id)
  "Create a resources/templates/list JSON-RPC request with optional ID.
If ID is not provided, it defaults to 1."
  (json-encode
   `(("jsonrpc" . "2.0")
     ("method" . "resources/templates/list")
     ("id" . ,(or id 1)))))

(defun anvil-server-create-resources-read-request (uri &optional id)
  "Create a resources/read JSON-RPC request for URI with optional ID.
If ID is not provided, it defaults to 1.

Arguments:
  URI    Resource URI to read
  ID     Optional request ID (defaults to 1)

Example:
  (anvil-server-create-resources-read-request \"test://resource\" 42)"
  (json-encode
   `(("jsonrpc" . "2.0")
     ("method" . "resources/read")
     ("id" . ,(or id 1))
     ("params" . (("uri" . ,uri))))))

;;; API - Tools

(defun anvil-server-register-tool (handler &rest properties)
  "Register a tool with the MCP server.

Arguments:
  HANDLER          Function to handle tool invocations
  PROPERTIES       Property list with tool attributes

Required properties:
  :id              String identifier for the tool (e.g., \"list-files\")
  :description     String describing what the tool does

Optional properties:
  :title           User-friendly display name for the tool
  :schema          Precomputed MCP input schema; skips handler introspection
                   for schema generation when provided
  :read-only       If true, indicates tool doesn't modify its environment
  :server-id       Server identifier (defaults to \"default\")

The HANDLER function's signature determines its input schema.
When HANDLER is wrapped by `anvil-server-encode-handler' (or a
compatible wrapper), registration still derives schema and
parameter validation from the underlying raw handler and only
encodes the result at the MCP transport boundary.
Currently only no-argument and single-argument handlers are supported.

Example:
  ;; Simple tool with no arguments
  (anvil-server-register-tool #\\='my-org-files-handler
    :id \"org-list-files\"
    :description \"Lists all available Org mode files for task management\")

  ;; With optional properties
  (anvil-server-register-tool #\\='my-org-files-handler
    :id \"org-list-files\"
    :description \"Lists all available Org mode files for task management\"
    :title \"List Org Files\"
    :read-only t)

  ;; Tool with one argument - parameter description in docstring
  (defun my-file-reader (path)
    \"Read file at PATH.

MCP Parameters:
  path - absolute path to the file to read\"
    (anvil-server-with-error-handling
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))))
  (anvil-server-register-tool #\\='my-file-reader
    :id \"read-file\"
    :description \"Read contents of a file\")

See also:
  `anvil-server-unregister-tool' - Remove a registered tool
  `anvil-server-with-error-handling' - Error handling for tool handlers
  `anvil-server-tool-throw' - Signal errors from tool handlers"
  (let* ((id (plist-get properties :id))
         (description (plist-get properties :description))
         (title (plist-get properties :title))
         (provided-schema (plist-get properties :schema))
         (read-only (plist-get properties :read-only))
         (server-id (or (plist-get properties :server-id) "default"))
         (tools-table (anvil-server--get-server-tools server-id)))
    ;; Error checking for required properties.
    ;; Standalone nelisp's `functionp' returns nil for symbols whose
    ;; function cell was installed via `fset' (= encoded handler
    ;; wrappers from `anvil-server-encode-handler').  Accept any
    ;; symbol whose function cell is bound as a fallback.
    (unless (or (functionp handler)
                (and (symbolp handler) (fboundp handler)))
      (error "Tool registration requires handler function"))
    (unless id
      (error "Tool registration requires :id property"))
    (unless description
      (error "Tool registration requires :description property"))
    ;; Check for existing registration
    (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
      (nelisp--write-stderr-line (concat "[REG-IN] " id)))
    (if-let* ((existing (gethash id tools-table)))
      (if (plist-get existing :lazy-placeholder)
          (progn
            (remhash id tools-table)
            (apply #'anvil-server-register-tool handler properties))
        (anvil-server--ref-counted-register id existing tools-table))
      (let* ((_p1 (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
                    (nelisp--write-stderr-line (concat "[REG-1 normalize] " id))))
             (handler-meta
              (anvil-server--normalize-tool-handler handler))
             (_p2 (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
                    (nelisp--write-stderr-line (concat "[REG-2 normalize-done] " id))))
             (raw-handler (plist-get handler-meta :handler))
             (arglist (plist-get handler-meta :arglist))
             (encode-result (plist-get handler-meta :encode-result))
             (schema-cache-signature
              (anvil-server--schema-cache-signature
               raw-handler arglist description provided-schema))
             (schema-cache-entry
              (anvil-server--schema-cache-get id schema-cache-signature))
             (_cache-log
              (when (and anvil-server--debug-trace
                         (fboundp 'nelisp--write-stderr-line))
                (nelisp--write-stderr-line
                 (concat "[REG-2 cache-"
                         (if schema-cache-entry "hit] " "miss] ")
                         id))))
             (_p3 (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
                    (nelisp--write-stderr-line (concat "[REG-3 schema-start] " id))))
             (schema
              (if schema-cache-entry
                  (plist-get schema-cache-entry :schema)
                (or provided-schema
                    (anvil-server--generate-schema-from-function
                     raw-handler arglist))))
             (_p4 (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
                    (nelisp--write-stderr-line (concat "[REG-4 schema-done] " id))))
             (tool
              (list
               :id id
               :description description
               :handler raw-handler
               :arglist arglist
               :schema schema)))
        ;; Add optional properties if provided
        (when title
          (setq tool (plist-put tool :title title)))
        (when encode-result
          (setq tool (plist-put tool :encode-result t)))
        ;; Always include :read-only if it was specified, even if nil
        (when (plist-member properties :read-only)
          (setq tool (plist-put tool :read-only read-only)))
        ;; Offload routing (Doc 03 Phase 2b).  :offload t routes the
        ;; handler call through `anvil-offload' so it runs in a batch
        ;; subprocess instead of the main daemon.  The handler must be a
        ;; symbol (not a lambda) because the subprocess identifies it by
        ;; name after loading the features listed in :offload-require.
        ;; Encoded registration wrappers are normalized back to the raw
        ;; handler before offload metadata is stored.
        (dolist (k '(:offload :offload-require :offload-load-path
                              :offload-inherit-load-path
                              :offload-timeout
                              :resumable
                              ;; Discovery metadata (Doc 34 Phase A):
                              ;; :intent is a list of symbols naming
                              ;; the use-cases the tool serves;
                              ;; :layer classifies the tool as
                              ;; core / io / workflow / dev;
                              ;; :stability is stable / experimental /
                              ;; deprecated.  Handlers ignore these
                              ;; values; `anvil-discovery' reads them.
                              ;; :preconditions is Doc 44 state-filter
                              ;; metadata consumed by `anvil-manifest'.
                              :intent :layer :stability :preconditions))
          (when (plist-member properties k)
            (setq tool (plist-put tool k (plist-get properties k)))))
        ;; Pre-build the tools/list JSON fragment with a hand-rolled
        ;; encoder that avoids `json-encode' entirely.  On post-2026-
        ;; 05-17 nelisp the pure-elisp `json-encode' degrades after
        ;; 4-5 calls — calling it 6 times during substrate registration
        ;; was hitting the wall and stalling cold-load.  The hand-built
        ;; encoder uses only `concat' + `format' and stays O(N) regardless.
        (let ((fragment
               (or (and schema-cache-entry
                        (plist-get schema-cache-entry :fragment))
                   (anvil-server--build-tool-fragment
                    id description (plist-get tool :schema)))))
          (setq tool (plist-put tool :json-fragment fragment)))
        (unless schema-cache-entry
          (anvil-server--schema-cache-put
           id schema-cache-signature
           (plist-get tool :schema)
           (plist-get tool :json-fragment)))
        ;; Register the tool
        (anvil-server--ref-counted-register id tool tools-table)
        ;; Perf (2026-05-11): tools/list response is cached per server-id;
        ;; drop the cached entry so the next /list rebuilds with the new tool.
        (anvil-server--tools-list-cache-invalidate server-id)))))

(defun anvil-server-unregister-tool (tool-id &optional server-id)
  "Unregister a tool with ID TOOL-ID from the MCP server with SERVER-ID.

Returns t if the tool was found and removed, nil otherwise.

See also: `anvil-server-register-tool'"
  (let* ((effective-id (or server-id "default"))
         (tools-table
          (anvil-server--get-server-tools effective-id))
         (removed
          (anvil-server--ref-counted-unregister tool-id tools-table)))
    ;; Perf (2026-05-11): drop the cached tools/list response so the
    ;; next /list rebuilds without the removed tool.
    (when removed
      (anvil-server--tools-list-cache-invalidate effective-id))
    removed))

(defun anvil-server-register-tools (server-id specs)
  "Register every (HANDLER . PROPS) SPEC under SERVER-ID in one call.

SPECS is a list whose entries each look like (HANDLER :id ID
:description DESC [:read-only t] [:title STR] ...).  SERVER-ID is
auto-filled into every PROPS plist, overriding any stray
`:server-id' already in there (so a spec list authored once can
be re-used across servers without editing).

Signals `error' on structurally-bad specs; otherwise returns the
list of registered tool-id strings in input order.  Use with the
companion `anvil-server-unregister-tools' to keep register /
unregister paths consistent — the common module pattern becomes:

  (defconst my-mod--tool-specs `((,#\\='my-mod--tool-foo :id \"foo\"
                                  :description \"...\")))
  (defun my-mod--register-tools () (anvil-server-register-tools
                                    SERVER-ID my-mod--tool-specs))
  (defun my-mod--unregister-tools () (anvil-server-unregister-tools
                                      SERVER-ID my-mod--tool-specs))

Collapses the verbose per-tool register boilerplate (four-line
call + matching id string in an unregister dolist) into a single
spec list maintained in one place."
  (let (ids)
    (dolist (spec specs)
      (unless (and (consp spec) (functionp (car spec))
                   (cl-evenp (length (cdr spec))))
        (error "anvil-server-register-tools: malformed spec %S" spec))
      (let* ((handler (car spec))
             (props   (cl-loop for (k v) on (cdr spec) by #'cddr
                               unless (eq k :server-id)
                               append (list k v)))
             (final   (append (list :server-id server-id) props))
             (id      (plist-get final :id)))
        (apply #'anvil-server-register-tool handler final)
        (push id ids)))
    (nreverse ids)))

(defun anvil-server-unregister-tools (server-id specs)
  "Unregister every :id referenced in SPECS from SERVER-ID.
Bad specs and missing :id entries are silently skipped so the
function is safe to call even when partial registration left the
table in an intermediate state.  Returns the list of tool ids
that were unregistered (t = removed, nil = absent)."
  (let (results)
    (dolist (spec specs)
      (when (consp spec)
        (let ((id (plist-get (cdr spec) :id)))
          (when id
            (push (cons id
                        (anvil-server-unregister-tool id server-id))
                  results)))))
    (nreverse results)))

;; Custom error type for tool errors
(define-error 'anvil-server-tool-error "MCP tool error" 'user-error)
(define-error 'anvil-server-resource-error "MCP resource error")
(define-error 'anvil-server-invalid-params "MCP invalid parameters")

(defun anvil-server-tool-throw (error-message)
  "Signal a tool error with ERROR-MESSAGE.
The error will be properly formatted and sent to the client.
This should be used within tool handlers to indicate failures.

Arguments:
  ERROR-MESSAGE  String describing the error

Example:
  (defun my-tool-handler (path)
    \"List files in PATH.

MCP Parameters:
  path - directory path to list\"
    (unless (file-directory-p path)
      (anvil-server-tool-throw
       (format \"Not a directory: %s\" path)))
    ;; ... rest of implementation ...)

See also: `anvil-server-with-error-handling'"
  (signal 'anvil-server-tool-error (list error-message)))

(defun anvil-server--parse-uri-template (template)
  "Parse URI TEMPLATE into segments.
Returns plist with :segments, :variables, and :pattern.
Supports RFC 6570 simple variables {var} and reserved expansion {+var}."
  ;; First check for balanced braces
  (let ((open-count 0)
        (close-count 0)
        (i 0))
    (while (< i (length template))
      (cond
       ((eq (aref template i) ?{)
        (setq open-count (1+ open-count)))
       ((eq (aref template i) ?})
        (setq close-count (1+ close-count))))
      (setq i (1+ i)))
    ;; Check for balanced braces after processing entire string
    (unless (= open-count close-count)
      (error "Unbalanced braces in resource template: %s" template)))

  (let ((segments '())
        (variables '())
        (pos 0)
        (len (length template)))
    ;; Process template character by character
    (while (< pos len)
      (if-let ((var-start (string-match "{" template pos)))
        ;; Found variable start
        (progn
          ;; Add literal segment before variable if any
          (when (> var-start pos)
            (push (list
                   :type 'literal
                   :value (substring template pos var-start))
                  segments))
          ;; Find variable end (guaranteed to exist due to balance check)
          (let* ((var-end (string-match "}" template var-start))
                 ;; Extract variable content
                 (var-content
                  (substring template (1+ var-start) var-end))
                 (reserved
                  (and (> (length var-content) 0)
                       (eq (aref var-content 0) ?+)))
                 (var-name
                  (if reserved
                      (substring var-content 1)
                    var-content)))
            ;; Validate variable name
            ;; RFC 6570: Variable names must start with ALPHA / "_"
            ;; and contain only ALPHA / DIGIT / "_" / pct-encoded
            (unless (string-match-p
                     "\\`[A-Za-z_][A-Za-z0-9_]*\\'" var-name)
              (error
               "Invalid variable name '%s' in resource template: %s"
               var-name
               template))
            ;; Add variable segment
            (push (list
                   :type 'variable
                   :name var-name
                   :reserved reserved)
                  segments)
            (push var-name variables)
            (setq pos (1+ var-end))))
        ;; No more variables, add remaining literal
        (when (< pos len)
          (push (list :type 'literal :value (substring template pos))
                segments))
        (setq pos len)))
    ;; Return parsed structure
    (list
     :segments (nreverse segments)
     :variables (nreverse variables)
     :pattern template)))

(defun anvil-server--register-resource-internal
    (uri handler properties hash-table extra-props)
  "Internal helper for resource registration.
URI is the resource URI.
HANDLER is the handler function.
PROPERTIES is the plist of properties.
HASH-TABLE is either `anvil-server--resources' or
`anvil-server--resource-templates'.
EXTRA-PROPS is a plist of additional properties to include (e.g., :parsed)."
  (let ((name (plist-get properties :name))
        (description (plist-get properties :description))
        (mime-type (plist-get properties :mime-type)))
    ;; Error checking for required properties
    (unless (functionp handler)
      (error "Resource registration requires handler function"))
    (unless name
      (error "Resource registration requires :name property"))

    (if-let* ((existing (gethash uri hash-table)))
      (anvil-server--ref-counted-register uri existing hash-table)
      (let ((entry
             (append (list :handler handler :name name) extra-props)))
        (when description
          (setq entry (plist-put entry :description description)))
        (when mime-type
          (setq entry (plist-put entry :mime-type mime-type)))
        (anvil-server--ref-counted-register
         uri entry hash-table)))))

;;; API - Resources

(defun anvil-server-register-resource (uri handler &rest properties)
  "Register a resource with the MCP server.

Automatically detects whether URI is a template based on presence of {}.

Arguments:
  URI              Resource URI or URI template (e.g., \"org://projects.org\"
                   or \"org://{filename}/outline\")
  HANDLER          Function that returns the content
                   - For static resources: takes no arguments
                   - For templates: takes params alist argument
  PROPERTIES       Property list with resource attributes

Required properties:
  :name            Human-readable name for the resource

Optional properties:
  :description     Description of the resource
  :mime-type       MIME type (default: \"text/plain\")
  :server-id       Server identifier (defaults to \"default\")

Examples:
  ;; Static resource
  (anvil-server-register-resource
   \"org://projects.org\"
   (lambda ()
     (with-temp-buffer
       (insert-file-contents \"~/org/projects.org\")
       (buffer-string)))
   :name \"Projects\"
   :description \"Current project list\"
   :mime-type \"text/plain\")

  ;; Template resource
  (anvil-server-register-resource
   \"org://{filename}/outline\"
   (lambda (params)
     (generate-outline
       (alist-get \"filename\" params nil nil #\\='string=)))
   :name \"Org file outline\"
   :description \"Hierarchical outline of an Org file\")

See also: `anvil-server-unregister-resource'"
  (let* ((server-id (or (plist-get properties :server-id) "default"))
         (resources-table
          (anvil-server--get-server-resources server-id))
         (templates-table
          (anvil-server--get-server-templates server-id)))
    ;; Check for proper URI structure: scheme://path
    (unless (string-match-p anvil-server--uri-with-scheme-regex uri)
      (error
       "Resource URI must have format 'scheme://path': '%s'" uri))
    ;; Check for unmatched }
    (when (and (string-match-p "}" uri)
               (not (string-match-p "{" uri)))
      (error "Unmatched '}' in resource URI: %s" uri))
    ;; Check if this is a template by looking for unescaped {
    (if (string-match-p "{" uri)
        ;; It's a template - delegate to template logic
        (let ((parsed (anvil-server--parse-uri-template uri)))
          (anvil-server--register-resource-internal
           uri
           handler
           properties
           templates-table
           (list :parsed parsed)))
      ;; It's a static resource
      (anvil-server--register-resource-internal
       uri handler properties resources-table nil))))

(defun anvil-server-unregister-resource (uri &optional server-id)
  "Unregister a resource by its URI.

Automatically detects whether URI is a template based on presence of {}.

Arguments:
  URI       The URI or URI template to unregister
  SERVER-ID Server identifier (optional, defaults to \"default\")

Returns t if the resource was found and removed, nil otherwise.

Examples:
  (anvil-server-unregister-resource \"org://projects.org\")
  (anvil-server-unregister-resource \"org://{filename}/outline\")

See also: `anvil-server-register-resource'"
  (let* ((server-id (or server-id "default"))
         (resources-table
          (anvil-server--get-server-resources server-id))
         (templates-table
          (anvil-server--get-server-templates server-id)))
    ;; Check if this is a template by looking for unescaped {
    (if (string-match-p "{" uri)
        ;; It's a template
        (anvil-server--ref-counted-unregister uri templates-table)
      ;; It's a static resource
      (anvil-server--ref-counted-unregister uri resources-table))))

(defun anvil-server-resource-signal-error (code message)
  "Signal a JSON-RPC error from a resource handler.

CODE is the JSON-RPC error code constant (e.g.,
`anvil-server-jsonrpc-error-invalid-params' or
`anvil-server-jsonrpc-error-internal').
MESSAGE is the error message string.

This function does not return - it signals an error condition that
will be caught by the resource handler infrastructure."
  (signal 'anvil-server-resource-error (list code message)))

;;;; --- MCP return value encoder -------------------------------------------
;;
;; The MCP transport contract (see `anvil-server--tool-call') requires
;; handlers to return a string or nil.  Modules that want to return rich
;; Lisp data (plists, nested lists) have two options:
;;
;;   1. Wrap the return expression in `anvil-server-encode-for-mcp' so the
;;      result becomes a JSON string at the boundary.  Works per-call and
;;      keeps the bug class visible to `anvil-release-audit' — the scanner
;;      flags any `(list :K ...)' that is still the terminal form.
;;
;;   2. Tag the file with the `;;; anvil-audit: tools-wrapped-at-registration'
;;      header and wrap each registered handler with
;;      `anvil-server-encode-handler' at registration time.  The wrapper is
;;      transport-only: registration still inspects the underlying raw
;;      handler and applies JSON encoding after execution (the pattern
;;      anvil-orchestrator uses for its ~25 tools).
;;
;; Option 1 is the smaller, per-tool fix; option 2 is the whole-module
;; pattern.  Both produce the same wire format and pass the scanner.

(defun anvil-server--plist-p (x)
  "Return non-nil when X is a plist (keyword-keyed list)."
  (and (consp x) (keywordp (car x))))

(defun anvil-server--list-to-json-array (xs)
  "Recursively convert proper or improper list XS into a JSON array.
A dotted tail becomes the final array element, so `(1 . 2)' becomes
`[1, 2]' on the wire."
  (let (out)
    (while (consp xs)
      (push (anvil-server--to-json-value (car xs)) out)
      (setq xs (cdr xs)))
    (when xs
      (push (anvil-server--to-json-value xs) out))
    (vconcat (nreverse out))))

(defun anvil-server--to-json-value (x)
  "Recursively convert X into a value that `json-encode' renders sensibly.
Plists become alists with symbol keys (leading colon stripped).
Plain lists become vectors so they emit as JSON arrays.  Scalars pass
through unchanged.  Keywords serialize as their bare name (no colon)."
  (cond
   ((or (null x) (eq x t) (numberp x)) x)
   ((stringp x) x)
   ((keywordp x) (substring (symbol-name x) 1))
   ((symbolp x) (symbol-name x))
   ((vectorp x)
    (vconcat (mapcar #'anvil-server--to-json-value x)))
   ((anvil-server--plist-p x)
    (cl-loop for (k v) on x by #'cddr
             collect (cons (intern (substring (symbol-name k) 1))
                           (anvil-server--to-json-value v))))
   ((and (consp x) (not (proper-list-p x)))
    (vector (anvil-server--to-json-value (car x))
            (anvil-server--to-json-value (cdr x))))
   ((listp x)
    (anvil-server--list-to-json-array x))
   (t x)))

(defun anvil-server-encode-for-mcp (value)
  "Encode VALUE (plist / list / scalar) as a JSON string for MCP transport.
Strings and nil pass through unchanged; other shapes round-trip through
`anvil-server--to-json-value' then `json-encode'."
  (cond
   ((or (null value) (stringp value)) value)
   (t (json-encode (anvil-server--to-json-value value)))))

(defun anvil-server--encoded-handler-dispatch (encoder handler &rest args)
  "Apply HANDLER to ARGS, then pass the result through ENCODER."
  (funcall encoder (apply handler args)))

(defun anvil-server--make-encoded-handler (handler encoder)
  "Return a symbol-backed transport wrapper around HANDLER using ENCODER.
The wrapper records the underlying raw handler on the
`anvil-server-raw-handler' symbol property and marks
`anvil-server-encode-result' so registration can preserve the raw
signature/docstring for schema extraction and dispatch."
  (let* ((name (if (symbolp handler)
                   (symbol-name handler)
                 "handler"))
         (sym (make-symbol (format "anvil-encoded:%s" name))))
    (put sym 'anvil-server-raw-handler handler)
    (put sym 'anvil-server-encode-result t)
    (put sym 'function-documentation (documentation handler t))
    (fset sym
          (apply-partially #'anvil-server--encoded-handler-dispatch
                           encoder handler))
    sym))

(defun anvil-server-encode-handler (handler)
  "Return a wrapper around HANDLER that JSON-encodes its result.
Lets the underlying tool body keep returning a rich plist for direct
Elisp / ERT callers while the MCP transport receives a JSON string.
Schema extraction and MCP argument binding still use HANDLER's raw
signature/docstring via `anvil-server-register-tool'."
  (anvil-server--make-encoded-handler
   handler #'anvil-server-encode-for-mcp))

(provide 'anvil-server)
;;; anvil-server.el ends here
