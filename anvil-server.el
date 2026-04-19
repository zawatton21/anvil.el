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

(require 'json)
(require 'anvil-server-metrics)

;; `anvil-offload' is an optional module — loaded on demand in
;; `anvil-server--offload-apply'.  Declare to silence byte-compile warnings.
(declare-function anvil-offload "anvil-offload" (form &rest keys))
(declare-function anvil-future-await "anvil-offload" (future &optional timeout))
(declare-function anvil-future-cancel "anvil-offload" (future))
(declare-function anvil-future-kill "anvil-offload" (future))
(declare-function anvil-future-elapsed "anvil-offload" (future))
(declare-function anvil-future-status "anvil-offload" (future))
(declare-function anvil-future-value "anvil-offload" (future))
(declare-function anvil-future-error "anvil-offload" (future))
(declare-function anvil-future-checkpoint "anvil-offload" (future))

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

(defun anvil-server--jsonrpc-response (id result)
  "Create a JSON-RPC response with ID and RESULT."
  (json-encode `((jsonrpc . "2.0") (id . ,id) (result . ,result))))


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

(defun anvil-server--generate-schema-from-function (func)
  "Generate JSON schema by analyzing FUNC's signature.
Returns a schema object suitable for tool registration.
Extracts parameter descriptions from the docstring if available.
Parameters prefixed with `_' (the Elisp unused-arg convention) are
hidden from the client-facing schema — `anvil-server--handle-tools-call'
fills them with nil at dispatch time."
  (let ((arglist (help-function-arglist func t)))
    (when (memq '&rest arglist)
      (error "MCP tool handlers do not support &rest parameters"))
    (let*
        ( ;; Use RAW=t to prevent substitute-command-keys from converting
         ;; apostrophes to fancy quotes, preserving exact documentation text
         (docstring (documentation func t))
         (param-descriptions
          (anvil-server--extract-param-descriptions
           docstring arglist))
         ;; Filter out `_'-prefixed args for the user-visible schema.
         (visible-arglist
          (cl-remove-if
           (lambda (a)
             (string-prefix-p "_" (symbol-name a)))
           arglist)))
      (if visible-arglist
          ;; One or more user-visible arguments case
          (let ((properties '())
                (required '())
                (seen-optional nil))
            (dolist (arg visible-arglist)
              (let ((param-name (symbol-name arg)))
                (if (string= param-name "&optional")
                    ;; Mark that we've seen &optional
                    (setq seen-optional t)
                  ;; Regular parameter - add to properties
                  (let* ((description
                          (cdr (assoc param-name param-descriptions)))
                         (property-schema `((type . "string"))))
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

(defun anvil-server--handle-error (err)
  "Handle error ERR in MCP process by logging and creating an error response.
Returns a JSON-RPC error response string for internal errors."
  (anvil-server--jsonrpc-error
   nil
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
  (let* ((jsonrpc (alist-get 'jsonrpc request))
         (id (alist-get 'id request))
         (method (alist-get 'method request))
         (params (alist-get 'params request))
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
     (cl-incf (anvil-server-metrics-errors method-metrics))
     (let ((code (car (cdr err)))
           (message (cadr (cdr err))))
       (anvil-server--jsonrpc-error id code message)))
    ;; Handle any other error from the handler
    (error
     (cl-incf (anvil-server-metrics-errors method-metrics))
     (anvil-server--jsonrpc-error
      id anvil-server-jsonrpc-error-internal
      (format "Error reading resource %s: %s"
              uri (error-message-string err))))))

(defun anvil-server--handle-resources-read
    (id params method-metrics server-id)
  "Handle resources/read request with ID and PARAMS for SERVER-ID.
METHOD-METRICS is used to track errors for this method."
  (let* ((uri (alist-get 'uri params))
         (resources-table
          (anvil-server--get-server-resources server-id))
         (templates-table
          (anvil-server--get-server-templates server-id))
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
    (cl-incf (anvil-server-metrics-calls method-metrics))

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
version and capabilities between the client and server."
  (let ((capabilities '()))
    (let ((tools-table (gethash server-id anvil-server--tools))
          (resources-table
           (gethash server-id anvil-server--resources))
          (templates-table
           (gethash server-id anvil-server--resource-templates)))
      (when (and tools-table (> (hash-table-count tools-table) 0))
        (push `(tools . ,(make-hash-table)) capabilities))
      (when (or (and resources-table
                     (> (hash-table-count resources-table) 0))
                (and templates-table
                     (> (hash-table-count templates-table) 0)))
        (push `(resources . ,(make-hash-table)) capabilities))
      (anvil-server--jsonrpc-response
       id
       `((protocolVersion . ,anvil-server-protocol-version)
         (serverInfo
          .
          ((name . ,anvil-server-name)
           (version . ,anvil-server-protocol-version)))
         (capabilities . ,capabilities))))))

(defun anvil-server--handle-initialized ()
  "Handle initialized notification from client.

This is called after successful initialization to complete the handshake.
The client sends this notification to acknowledge the server's response
to the initialize request.")

(defun anvil-server--handle-tools-list (id server-id)
  "Handle tools/list request with ID for SERVER-ID.
Returns a list of all registered tools with their metadata."
  (let ((tool-list (vector)))
    (when-let* ((tools-table
                 (gethash server-id anvil-server--tools)))
      (maphash
       (lambda (tool-id tool)
         (let* ((tool-description (plist-get tool :description))
                (tool-title (plist-get tool :title))
                (tool-read-only (plist-get tool :read-only))
                (tool-schema
                 (or (plist-get tool :schema) '((type . "object"))))
                (tool-entry
                 `((name . ,tool-id)
                   (description . ,tool-description)
                   (inputSchema . ,tool-schema)))
                (annotations nil))
           ;; Collect annotations if present
           (when tool-title
             (push (cons 'title tool-title) annotations))
           ;; Add readOnlyHint when :read-only is explicitly provided (both t
           ;; and nil)
           (when (plist-member tool :read-only)
             (let ((annot-value
                    (if tool-read-only
                        t
                      :json-false)))
               (push (cons 'readOnlyHint annot-value) annotations)))
           ;; Add annotations to tool entry if any exist
           (when annotations
             (setq tool-entry
                   (append
                    tool-entry `((annotations . ,annotations)))))
           (setq tool-list (vconcat tool-list (vector tool-entry)))))
       tools-table))
    (anvil-server--jsonrpc-response id `((tools . ,tool-list)))))

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
Returns a list of all registered resources with their metadata."
  (let* ((resources-table
          (gethash server-id anvil-server--resources))
         (resource-list
          (if resources-table
              (anvil-server--collect-resources-from-hash
               resources-table nil)
            (vector))))
    (anvil-server--jsonrpc-response
     id `((resources . ,resource-list)))))

(defun anvil-server--handle-resources-templates-list (id server-id)
  "Handle resources/templates/list request with ID for SERVER-ID.
Returns a list of all registered resource templates."
  (let* ((templates-table
          (gethash server-id anvil-server--resource-templates))
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
METHOD-METRICS is used to track errors for this method."
  (let* ((tool-name (alist-get 'name params))
         (tools-table (gethash server-id anvil-server--tools))
         (tool
          (when tools-table
            (gethash tool-name tools-table)))
         (tool-args (alist-get 'arguments params)))
    (if tool
        (let ((handler (plist-get tool :handler))
              (context (list :id id)))
          (condition-case err
              (let*
                  ;; Check if handler is defined before trying to get arglist
                  ((arglist
                    (if (fboundp handler)
                        (help-function-arglist handler t)
                      ;; If undefined, signal early with proper error
                      (signal 'void-function (list handler))))
                   (expected-params '())
                   (required-params '())
                   (provided-params '())
                   (arg-values '())
                   (seen-optional nil)
                   (result
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
                            (format "Missing required parameter: %s"
                                    required)))))
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
                      (let ((final-args (nreverse arg-values)))
                        (if (plist-get tool :offload)
                            (anvil-server--offload-apply
                             tool handler final-args)
                          (apply handler final-args)))))
                   ;; Ensure result is string or nil, error on other types
                   (result-text
                    (cond
                     ((null result)
                      "")
                     ((stringp result)
                      result)
                     (t
                      (signal
                       'anvil-server-invalid-params
                       (list
                        (format
                         "Tool handler must return string or nil, got: %s"
                         (type-of result)))))))
                   ;; Wrap the handler result in the MCP format
                   (formatted-result
                    `((content
                       .
                       ,(vector
                         `((type . "text") (text . ,result-text))))
                      (isError . :json-false))))
                (anvil-server-metrics--track-tool-call tool-name)
                (anvil-server--respond-with-result
                 context formatted-result))
            ;; Handle invalid parameter errors
            (anvil-server-invalid-params
             (anvil-server-metrics--track-tool-call tool-name t)
             (cl-incf (anvil-server-metrics-errors method-metrics))
             (anvil-server--jsonrpc-error
              id
              anvil-server-jsonrpc-error-invalid-params
              (cadr err)))
            ;; Handle tool-specific errors thrown with
            ;; anvil-server-tool-throw
            (anvil-server-tool-error
             (anvil-server-metrics--track-tool-call tool-name t)
             (cl-incf (anvil-server-metrics-errors method-metrics))
             (let ((formatted-error
                    `((content
                       .
                       ,(vector
                         `((type . "text") (text . ,(cadr err)))))
                      (isError . t))))
               (anvil-server--respond-with-result
                context formatted-error)))
            ;; Keep existing handling for all other errors
            (error
             (anvil-server-metrics--track-tool-call tool-name t)
             (cl-incf (anvil-server-metrics-errors method-metrics))
             (anvil-server--jsonrpc-error
              id anvil-server-jsonrpc-error-internal
              (format "Internal error executing tool: %s"
                      (error-message-string err))))))
      (anvil-server-metrics--track-tool-call tool-name t)
      (cl-incf (anvil-server-metrics-errors method-metrics))
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
                                      status))))))))

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
     (error
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
    (error
     "No active MCP server, start server with `anvil-server-start' first"))

  (anvil-server--log-json-rpc "in" json-string server-id)

  ;; Step 1: Try to parse the JSON, handle parsing errors
  (let ((json-object nil)
        (response nil))
    ;; Attempt to parse the JSON
    (condition-case json-err
        (setq json-object
              (json-read-from-string
               (decode-coding-string json-string 'utf-8 t)))
      (json-error
       ;; If JSON parsing fails, create a parse error response
       (setq response
             (anvil-server--jsonrpc-error
              nil anvil-server-jsonrpc-error-parse
              (format "Parse error: %s"
                      (error-message-string json-err))))))
    ;; Step 2: Process the request if JSON parsing succeeded
    (unless response
      (condition-case err
          (setq response
                (anvil-server--validate-and-dispatch-request
                 json-object server-id))
        (error
         (setq response (anvil-server--handle-error err)))))

    ;; Only log and return responses when they exist (not for notifications)
    (when response
      (anvil-server--log-json-rpc "out" response server-id))
    response))

(defun anvil-server-process-jsonrpc-parsed (request server-id)
  "Send REQUEST to the MCP server and return parsed response for SERVER-ID.
REQUEST should be a JSON string containing a valid JSON-RPC 2.0 request.

Call `anvil-server-process-jsonrpc' and return its result as a parsed alist."
  (json-read-from-string
   (anvil-server-process-jsonrpc request server-id)))

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
  :read-only       If true, indicates tool doesn't modify its environment
  :server-id       Server identifier (defaults to \"default\")

The HANDLER function's signature determines its input schema.
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
         (read-only (plist-get properties :read-only))
         (server-id (or (plist-get properties :server-id) "default"))
         (tools-table (anvil-server--get-server-tools server-id)))
    ;; Error checking for required properties
    (unless (functionp handler)
      (error "Tool registration requires handler function"))
    (unless id
      (error "Tool registration requires :id property"))
    (unless description
      (error "Tool registration requires :description property"))
    ;; Check for existing registration
    (if-let* ((existing (gethash id tools-table)))
      (anvil-server--ref-counted-register id existing tools-table)
      (let* ((schema
              (anvil-server--generate-schema-from-function handler))
             (tool
              (list
               :id id
               :description description
               :handler handler
               :schema schema)))
        ;; Add optional properties if provided
        (when title
          (setq tool (plist-put tool :title title)))
        ;; Always include :read-only if it was specified, even if nil
        (when (plist-member properties :read-only)
          (setq tool (plist-put tool :read-only read-only)))
        ;; Offload routing (Doc 03 Phase 2b).  :offload t routes the
        ;; handler call through `anvil-offload' so it runs in a batch
        ;; subprocess instead of the main daemon.  The handler must be a
        ;; symbol (not a lambda) because the subprocess identifies it by
        ;; name after loading the features listed in :offload-require.
        (dolist (k '(:offload :offload-require :offload-load-path
                              :offload-inherit-load-path
                              :offload-timeout
                              :resumable))
          (when (plist-member properties k)
            (setq tool (plist-put tool k (plist-get properties k)))))
        ;; Register the tool
        (anvil-server--ref-counted-register id tool tools-table)))))

(defun anvil-server-unregister-tool (tool-id &optional server-id)
  "Unregister a tool with ID TOOL-ID from the MCP server with SERVER-ID.

Returns t if the tool was found and removed, nil otherwise.

See also: `anvil-server-register-tool'"
  (let ((tools-table
         (anvil-server--get-server-tools (or server-id "default"))))
    (anvil-server--ref-counted-unregister tool-id tools-table)))

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

(provide 'anvil-server)
;;; anvil-server.el ends here
