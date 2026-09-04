;;; anvil-server-commands.el --- User commands for anvil server -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; This file is part of anvil-server.el.

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

;; This file provides interactive user commands for the MCP server library.

;;; Code:

(require 'anvil-server)
(require 'anvil-server-metrics)

;;;###autoload
(defun anvil-server-start ()
  "Start the MCP server and begin handling client requests.

This function starts the MCP server that can process JSON-RPC
requests via `anvil-server-process-jsonrpc'.  Once started, the server
will dispatch incoming requests to the appropriate tool
handlers that have been registered with `anvil-server-register-tool'.

Resets all metrics when starting.

See also: `anvil-server-stop'"
  (interactive)
  (when anvil-server--running
    (error "MCP server is already running"))

  (clrhash anvil-server-metrics--table)
  (when (called-interactively-p 'any)
    (message "Emacs starting handling MCP requests"))
  (setq anvil-server--running t))

;;;###autoload
(defun anvil-server-stop ()
  "Stop the MCP server from processing client requests.

Sets the server state to stopped, which prevents further processing of
client requests.  Note that this does not release any resources or unregister
tools, it simply prevents `anvil-server-process-jsonrpc' from accepting new
requests.

See also: `anvil-server-start'"
  (interactive)
  (unless anvil-server--running
    (error "MCP server is not running"))

  (when (called-interactively-p 'any)
    (message "Emacs stopping handling MCP requests"))
  ;; Mark server as not running
  (setq anvil-server--running nil)
  ;; Show metrics summary if there are any
  (when (> (hash-table-count anvil-server-metrics--table) 0)
    (message "%s" (anvil-server-metrics-summary)))
  t)

;;;###autoload
(defun anvil-server-status ()
  "Display the main MCP server's running state.
Prints `Running' or `Stopped' via `message', mirroring
`anvil-worker-status' for the worker pool.

For full setup details (registered tools, resources, client config
snippet), use `anvil-server-describe-setup'."
  (interactive)
  (message "MCP server: %s" (if anvil-server--running "Running" "Stopped")))

(defun anvil-server--batch-write-stdout (s)
  "Write S to stdout, flushing when the standalone primitive is present."
  (cond
   ((and (fboundp 'nelisp--write-stdout-bytes) (stringp s))
    (nelisp--write-stdout-bytes s))
   (t
    (princ s))))

(defun anvil-server--batch-emit-response (resp framing-p)
  "Emit RESP to stdout, framed if FRAMING-P is non-nil.

Framed responses use MCP Content-Length framing:

    Content-Length: <N>\\r\\n\\r\\n<body>

Legacy responses are emitted as a single line terminated by `terpri'."
  (when (and resp (stringp resp) (not (string-empty-p resp)))
    (cond
     (framing-p
      ;; Encode via the canonical framing helper so byte length matches
      ;; the UTF-8 wire representation.
      (let ((frame (anvil-server-mcp-frame-encode resp)))
        ;; Standalone NeLisp's direct byte writer flushes per call.  Host
        ;; Emacs falls back to `princ' for ordinary --batch tests.
        (let ((coding-system-for-write 'utf-8))
          (anvil-server--batch-write-stdout frame))))
     (t
      ;; MCP stdio is UTF-8 on every platform.  In particular, native
      ;; Windows Emacs otherwise writes non-ASCII characters as CP932.
      (let ((coding-system-for-write 'utf-8))
        (anvil-server--batch-write-stdout (concat resp "\n")))))))

(defun anvil-server--batch-skip-blank-lines ()
  "Drain consecutive blank lines from STDIN and return next non-blank.

Returns the next non-empty string read from `read-from-minibuffer',
or nil on EOF.  Each `\\r' byte read in batch mode shows up as an
extra empty line under `read-from-minibuffer'; this helper skips
all of them transparently so a CRLF-separated MCP framed message
parses cleanly under `--batch'."
  (let ((line ""))
    (while (and (stringp line) (string-empty-p line))
      (setq line (ignore-errors (read-from-minibuffer ""))))
    line))

(defun anvil-server--batch-read-framed-message ()
  "Read one MCP Content-Length framed message from STDIN.

Returns the JSON body string, or nil on EOF / malformed framing.
Header lines are read with `read-from-minibuffer' (line-based,
EOF-tolerant); the body is read by accumulating subsequent lines
until the announced byte length is satisfied.  CRLF separators are
handled by treating empty reads as blanks.  This is best-effort
under `--batch': Emacs' line reader is the only practical primitive
for stdin in batch mode, so a JSON body that contains literal
newlines is reassembled by re-inserting `\\n' between read chunks
and trusting Content-Length to delimit the message.  In practice
MCP clients emit single-line JSON bodies."
  (let* ((first-header (anvil-server--batch-skip-blank-lines)))
    (when first-header
      (anvil-server--batch-read-framed-with-prefix first-header))))

;;;###autoload
(defun anvil-server-run-batch-stdio (&optional server-id)
  "Read JSON-RPC requests from STDIN, emit responses to STDOUT.

The loop auto-detects MCP Content-Length framing on the very first
line: if the first non-empty line starts with `Content-Length:'
(case-insensitive), all subsequent traffic — input and output — is
treated as framed; otherwise the legacy line-delimited JSON path is
used.  See `anvil-server-mcp-frame-encode' /
`anvil-server-mcp-frame-parse-string' for the framing helpers.

Loops until state is cleared or EOF is seen.  Intended for batch
subprocess invocation (Stage D `anvil mcp serve' launcher), not for
daemon emacsclient transport.

SERVER-ID defaults to \"default\".  Use the alias `emacs-eval-headless'
to enable the Stage D headless tool profile via `anvil-manifest'.

Example:
  emacs --batch -Q -L /path/to/anvil.el \\
    -l anvil -l anvil-server-commands -l anvil-manifest \\
    --eval \"(anvil-manifest-enable)\" \\
    --eval \"(anvil-server-run-batch-stdio \\\"emacs-eval-headless\\\")\""
  (let ((server-id (or server-id "default"))
        (framing-p nil)
        (sniffed-line nil)
        (first-iter t))
    (anvil-server-start)
    (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
      (nelisp--write-stderr-line
       (format "[STDIO] start loop t=%.3f" (float-time))))
    (while anvil-server--running
      (let* ((req
              (cond
               ;; First iteration: sniff one line to decide framing.
               (first-iter
                (setq first-iter nil)
                (let* ((t0 (float-time))
                       (peek (ignore-errors
                               (read-from-minibuffer "")))
                       (t1 (float-time)))
                  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
                    (nelisp--write-stderr-line
                     (format "[STDIO] peek %.3fs len=%d" (- t1 t0)
                             (if peek (length peek) -1))))
                  (cond
                   ((null peek) nil) ; EOF before any input
                   ((anvil-server-mcp-detect-framing-p peek)
                    (setq framing-p t)
                    (let* ((tA (float-time))
                           (body (anvil-server--batch-read-framed-with-prefix peek))
                           (tB (float-time)))
                      (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
                        (nelisp--write-stderr-line
                         (format "[STDIO] framed-read-with-prefix %.3fs len=%d"
                                 (- tB tA)
                                 (if body (length body) -1))))
                      body))
                   (t peek))))
               (framing-p
                (let* ((tA (float-time))
                       (body (anvil-server--batch-read-framed-message))
                       (tB (float-time)))
                  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
                    (nelisp--write-stderr-line
                     (format "[STDIO] framed-read %.3fs len=%d"
                             (- tB tA)
                             (if body (length body) -1))))
                  body))
               (t
                (ignore-errors (read-from-minibuffer ""))))))
        (cond
         ((null req)
          (setq anvil-server--running nil))
         ((and (stringp req) (not (string-empty-p req)))
          (let* ((tC (float-time))
                 (resp (anvil-server-process-jsonrpc req server-id))
                 (tD (float-time)))
            (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
              (nelisp--write-stderr-line
               (format "[STDIO] process-jsonrpc %.3fs resp-len=%d"
                       (- tD tC)
                       (if resp (length resp) -1))))
            (let ((tE (float-time)))
              (anvil-server--batch-emit-response resp framing-p)
              (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
                (nelisp--write-stderr-line
                 (format "[STDIO] emit-response %.3fs"
                         (- (float-time) tE)))))))
         (t nil))))
    (when anvil-server--running
      (anvil-server-stop))
    (ignore sniffed-line)))

(defun anvil-server--batch-read-framed-with-prefix (first-header-line)
  "Continue reading a framed MCP message after FIRST-HEADER-LINE.

Used on the very first iteration of `anvil-server-run-batch-stdio'
when sniffing detects framing — the sniffed line is the first
header rather than a JSON body, so we keep reading subsequent
headers + body.  Uses `anvil-server--batch-skip-blank-lines' to
tolerate CRLF expansion under `--batch' line-based stdin reads."
  (let ((header-lines (list (replace-regexp-in-string
                             "\r\\'" "" first-header-line))))
    ;; Read remaining header lines.  A blank line marks the end of
    ;; the header section, BUT a single CRLF in the input shows up
    ;; as an empty `read-from-minibuffer' result, then \r as another
    ;; empty, etc.  We stop at the first empty read and let
    ;; `anvil-server--batch-skip-blank-lines' below absorb any
    ;; extra CRs before the body.
    (let ((seen-blank nil))
      (catch 'done
        (while (not seen-blank)
          (let ((line (ignore-errors (read-from-minibuffer ""))))
            (cond
             ((null line) (throw 'done nil))
             ((string-empty-p
               (replace-regexp-in-string "\r\\'" "" line))
              (setq seen-blank t))
             (t (push (replace-regexp-in-string "\r\\'" "" line)
                      header-lines)))))))
    (let* ((header-block (mapconcat #'identity
                                    (nreverse header-lines) "\r\n"))
           (n (anvil-server-mcp-parse-content-length-header
               header-block)))
      (when (and n (>= n 0))
        ;; Body: skip any extra blank reads that the CRLF separator
        ;; produced under batch line-input.  Then accumulate until
        ;; N UTF-8 bytes are collected.
        (let* ((first-body-line
                (anvil-server--batch-skip-blank-lines))
               (acc "")
               (need n)
               (first t))
          (when first-body-line
            (let* ((piece-bytes
                    (encode-coding-string first-body-line 'utf-8 t))
                   (piece-len (length piece-bytes)))
              (cond
               ((<= piece-len need)
                (setq acc (concat acc first-body-line))
                (setq need (- need piece-len)))
               (t
                (let ((trimmed-bytes (substring piece-bytes 0 need)))
                  (setq acc
                        (concat acc
                                (decode-coding-string
                                 trimmed-bytes 'utf-8 t)))
                  (setq need 0))))
              (setq first nil)))
          (while (> need 0)
            (let ((chunk (ignore-errors (read-from-minibuffer ""))))
              (unless chunk
                (setq need 0))
              (when chunk
                (let* ((piece (if first chunk (concat "\n" chunk)))
                       (piece-bytes
                        (encode-coding-string piece 'utf-8 t))
                       (piece-len (length piece-bytes)))
                  (setq first nil)
                  (cond
                   ((<= piece-len need)
                    (setq acc (concat acc piece))
                    (setq need (- need piece-len)))
                   (t
                    (let ((trimmed-bytes
                           (substring piece-bytes 0 need)))
                      (setq acc
                            (concat acc
                                    (decode-coding-string
                                     trimmed-bytes 'utf-8 t)))
                      (setq need 0))))))))
          (and (not (string-empty-p acc)) acc))))))

(defun anvil-server--stage-d-env-optional-modules ()
  "Return optional modules requested through `ANVIL_OPTIONAL_MODULES'."
  (let ((raw (getenv "ANVIL_OPTIONAL_MODULES"))
        modules)
    (when raw
      (dolist (part (split-string raw "[,[:space:]]+" t))
        (when (string-match-p "\\`[A-Za-z0-9_-]+\\'" part)
          (push (intern part) modules))))
    (nreverse modules)))

;;;###autoload
(defun anvil-server-stage-d-headless-run ()
  "Stage D launcher entry: load anvil + headless profile, run batch stdio.

Convenience wrapper that:
  1. Loads `anvil' (which calls `anvil-enable' to register all modules)
  2. Activates `anvil-manifest' headless profile filter
  3. Runs the batch stdio loop with server-id `emacs-eval-headless'

Used by `bin/anvil mcp serve' when anvil.el is bundled in the
Stage D distribution.  Equivalent shell invocation:

  emacs --batch -Q -L /path/to/anvil.el \\
    --eval \"(require \\='anvil)\" \\
    --eval \"(anvil-enable)\" \\
    --eval \"(require \\='anvil-manifest)\" \\
    --eval \"(anvil-manifest-enable)\" \\
    --eval \"(anvil-server-run-batch-stdio \\\"emacs-eval-headless\\\")\""
  (require 'anvil)
  ;; Stage D needs http + state to expose the architecture α delegate
  ;; chain (anvil-http → nelisp-http etc).  Honour any prior user
  ;; customisation but ensure the two are present.  memory + worklog
  ;; carry the DB-direct cross-AI interchange surface and are required
  ;; from the headless launcher's first invocation (Doc 29 Phase 5 +
  ;; Doc 42 Phase 2).
  (when (boundp 'anvil-optional-modules)
    (dolist (m (append (anvil-server--stage-d-env-optional-modules)
                       '(http state memory worklog)))
      (cl-pushnew m anvil-optional-modules)))
  (when (fboundp 'anvil-enable)
    (anvil-enable))
  (require 'anvil-manifest)
  (when (fboundp 'anvil-manifest-enable)
    (anvil-manifest-enable))
  (anvil-server-run-batch-stdio "emacs-eval-headless"))

;;; Script Installation

(defun anvil-server--package-script-path ()
  "Return the path to anvil-stdio.sh in the package directory.
Returns nil if not found."
  (let* ((library-path (locate-library "anvil-server"))
         (package-dir
          (and library-path (file-name-directory library-path)))
         (script-path
          (and package-dir
               (expand-file-name "anvil-stdio.sh" package-dir))))
    (when (and script-path (file-exists-p script-path))
      script-path)))

(defun anvil-server--installed-script-path ()
  "Return the path where the script should be installed."
  (expand-file-name "anvil-stdio.sh"
                    anvil-server-install-directory))

;;;###autoload
(defun anvil-server-install ()
  "Install anvil-stdio.sh to `anvil-server-install-directory'."
  (interactive)
  (let ((source (anvil-server--package-script-path))
        (target (anvil-server--installed-script-path)))
    (unless source
      (error "Cannot find anvil-stdio.sh in package directory"))
    (when (file-exists-p target)
      (unless (yes-or-no-p
               (format "File already exists at %s. Overwrite? "
                       target))
        (user-error "Installation cancelled")))
    (make-directory (file-name-directory target) t)
    (copy-file source target t)
    (set-file-modes target #o755)
    (message "Script installed to: %s" target)))

;;;###autoload
(defun anvil-server-uninstall ()
  "Remove installed anvil-stdio.sh from `anvil-server-install-directory'."
  (interactive)
  (let ((target (anvil-server--installed-script-path)))
    (unless (file-exists-p target)
      (user-error "No script found at: %s" target))
    (when (yes-or-no-p (format "Remove script at %s? " target))
      (delete-file target)
      (message "Script removed from: %s" target))))

;;; Metrics commands

;;;###autoload
(defun anvil-server-reset-metrics ()
  "Reset all metrics."
  (interactive)
  (clrhash anvil-server-metrics--table)
  (message "MCP metrics reset"))

(defun anvil-server--get-handler-name (handler)
  "Get a human-readable name for HANDLER function.
Returns the function name for symbols, \"lambda\" for lambdas,
\"closure\" for closures, \"compiled-function\" for byte-code, or
\"unknown\" for unrecognized types."
  (cond
   ((symbolp handler)
    (symbol-name handler))
   ((byte-code-function-p handler)
    (let ((name (aref handler 2)))
      (if (symbolp name)
          (symbol-name name)
        "compiled-function")))
   ;; In Emacs 30.1+, interpreted lambdas with lexical binding are
   ;; represented as interpreted-function objects, not lists
   ((and (fboundp 'interpreted-function-p)
         (interpreted-function-p handler))
    "closure")
   ;; In Emacs 30.1+, closurep covers both interpreted and compiled closures
   ((and (fboundp 'closurep) (closurep handler))
    "closure")
   ;; For older Emacs versions, check list-based representations
   ((and (listp handler) (eq (car handler) 'lambda))
    "lambda")
   ((and (listp handler) (eq (car handler) 'closure))
    "closure")
   (t
    "unknown")))

(defun anvil-server--insert-usage-metrics (metrics &optional indent)
  "Insert usage statistics for METRICS into the current buffer.
METRICS should be a anvil-server-metrics struct or nil.
INDENT is an optional string prepended to the line (defaults to \"  \").
If METRICS is provided, inserts the call count and error count.
If METRICS is nil, inserts \"0 calls\" as the usage."
  (let ((prefix (or indent "  ")))
    (if metrics
        (let ((calls (anvil-server-metrics-calls metrics))
              (errors (anvil-server-metrics-errors metrics)))
          (insert
           (format "%s  Usage: %d calls, %d errors\n"
                   prefix
                   calls
                   errors)))
      (insert (format "%s  Usage: 0 calls\n" prefix)))))

(defun anvil-server--insert-item-properties
    (name description &optional extra-props indent)
  "Insert item properties NAME, DESCRIPTION, and EXTRA-PROPS.
NAME is always inserted (required property).
DESCRIPTION is only inserted if non-nil (optional property).
EXTRA-PROPS is an optional plist of additional properties to display.
INDENT is an optional string prepended to each line (defaults to \"  \").
Properties in EXTRA-PROPS with nil values are skipped."
  (let ((prefix (or indent "  ")))
    (insert (format "%s  Name: %s\n" prefix name))
    (when description
      (insert (format "%s  Description: %s\n" prefix description)))
    (let ((props extra-props))
      (while props
        (let ((key (car props))
              (value (cadr props)))
          (when value
            (insert
             (format "%s  %s: %s\n"
                     prefix
                     (capitalize (substring (symbol-name key) 1))
                     value)))
          (setq props (cddr props)))))))

(defun anvil-server--entry-key-lessp (a b)
  "Compare alist entries A and B by their keys alphabetically.
Returns t if the key of A is lexicographically less than the key of B."
  (string< (car a) (car b)))

(defun anvil-server--hash-table-to-sorted-alist (hash-table)
  "Convert HASH-TABLE to a sorted alist of (KEY . VALUE) pairs.
Returns nil if HASH-TABLE is nil.
The returned alist is sorted alphabetically by key."
  (when hash-table
    (let ((alist nil))
      (maphash
       (lambda (key value) (push (cons key value) alist)) hash-table)
      (sort alist #'anvil-server--entry-key-lessp))))

(defmacro anvil-server--with-hash-table-entries
    (hash-table header &rest body)
  "Iterate over HASH-TABLE entries in sorted order.
Prints HEADER before iterating if the table is not empty.
Executes BODY for each entry with `entry' bound to (KEY . VALUE) cons cell.
Entries are processed in alphabetical order by key.
Does nothing if HASH-TABLE is nil or empty."
  (declare (indent 2) (debug (form form body)))
  `(when (and ,hash-table (> (hash-table-count ,hash-table) 0))
     (insert ,header)
     (dolist (entry
              (anvil-server--hash-table-to-sorted-alist
               ,hash-table))
       ,@body)))

(defun anvil-server--format-metrics-with-errors
    (key metrics &optional indent)
  "Format metrics KEY with METRICS including error rate.
Optional INDENT adds spaces before the key."
  (let* ((total (anvil-server-metrics-calls metrics))
         (errors (anvil-server-metrics-errors metrics))
         (rate (anvil-server-metrics--error-rate total errors))
         (formatted-key
          (if indent
              (format "  %-38s" key)
            (format "%-40s" key))))
    (format "%s %6d %7d %9.1f%%\n" formatted-key total errors rate)))

;;;###autoload
(defun anvil-server-describe-setup ()
  "Show the current MCP server setup including registered tools and resources."
  (interactive)
  (with-current-buffer (get-buffer-create "*MCP Server Setup*")
    (erase-buffer)
    (insert "MCP Server Setup\n\n")
    (insert
     (format "Status: %s\n\n"
             (if anvil-server--running
                 "Running"
               "Stopped")))
    ;; Only show Tools section if there are actual tools
    (when (and anvil-server--tools
               (> (hash-table-count anvil-server--tools) 0)
               (cl-some
                (lambda (entry)
                  (let ((tools-table (cdr entry)))
                    (and tools-table
                         (> (hash-table-count tools-table) 0))))
                (anvil-server--hash-table-to-sorted-alist
                 anvil-server--tools)))
      (insert "Tools:\n"))
    (anvil-server--with-hash-table-entries anvil-server--tools ""
      (let* ((server-id (car entry))
             (tools-table (cdr entry))
             (multi-server
              (> (hash-table-count anvil-server--tools) 1)))
        (when multi-server
          (insert (format "  Server: %s\n" server-id)))
        (anvil-server--with-hash-table-entries tools-table ""
          (let* ((id (car entry))
                 (tool (cdr entry))
                 (description (plist-get tool :description))
                 (handler (plist-get tool :handler))
                 (handler-name
                  (anvil-server--get-handler-name handler))
                 (metrics-key (format "tools/call:%s" id))
                 (metrics
                  (gethash metrics-key anvil-server-metrics--table))
                 (indent
                  (if multi-server
                      "    "
                    "  ")))
            (insert (format "%s%s\n" indent id))
            (insert
             (format "%s  Description: %s\n" indent description))
            (when (plist-member tool :title)
              (insert
               (format "%s  Title: %s\n"
                       indent
                       (plist-get tool :title))))
            (when (plist-member tool :read-only)
              (insert
               (format "%s  Read-only: %s\n"
                       indent
                       (plist-get tool :read-only))))
            (insert (format "%s  Handler: %s\n" indent handler-name))
            (anvil-server--insert-usage-metrics metrics indent)
            (insert "\n")))))
    ;; Only show Resources section if there are actual resources
    (when (and anvil-server--resources
               (> (hash-table-count anvil-server--resources) 0)
               (cl-some
                (lambda (entry)
                  (let ((resources-table (cdr entry)))
                    (and resources-table
                         (> (hash-table-count resources-table) 0))))
                (anvil-server--hash-table-to-sorted-alist
                 anvil-server--resources)))
      (insert "\nResources:\n"))
    (anvil-server--with-hash-table-entries anvil-server--resources
        ""
      (let* ((server-id (car entry))
             (resources-table (cdr entry))
             (multi-server
              (> (hash-table-count anvil-server--resources) 1)))
        (when multi-server
          (insert (format "  Server: %s\n" server-id)))
        (anvil-server--with-hash-table-entries resources-table ""
          (let* ((uri (car entry))
                 (resource (cdr entry))
                 (name (plist-get resource :name))
                 (description (plist-get resource :description))
                 (mime-type
                  (or (plist-get resource :mime-type) "text/plain"))
                 (handler (plist-get resource :handler))
                 (handler-name
                  (anvil-server--get-handler-name handler))
                 (metrics-key (format "resources/read:%s" uri))
                 (metrics
                  (gethash metrics-key anvil-server-metrics--table))
                 (indent
                  (if multi-server
                      "    "
                    "  ")))
            (insert (format "%s%s\n" indent uri))
            (anvil-server--insert-item-properties
             name description
             (list :mime-type mime-type)
             indent)
            (insert (format "%s  Handler: %s\n" indent handler-name))
            (anvil-server--insert-usage-metrics metrics indent)
            (insert "\n")))))
    ;; Only show resource templates if there are any
    (when (and anvil-server--resource-templates
               (> (hash-table-count
                   anvil-server--resource-templates)
                  0)
               (cl-some
                (lambda (entry)
                  (let ((templates-table (cdr entry)))
                    (and templates-table
                         (> (hash-table-count templates-table) 0))))
                (anvil-server--hash-table-to-sorted-alist
                 anvil-server--resource-templates)))
      (insert "\nResource Templates:\n")
      (anvil-server--with-hash-table-entries
          anvil-server--resource-templates
          ""
        (let* ((server-id (car entry))
               (templates-table (cdr entry))
               (multi-server
                (> (hash-table-count
                    anvil-server--resource-templates)
                   1)))
          (when multi-server
            (insert (format "  Server: %s\n" server-id)))
          (anvil-server--with-hash-table-entries templates-table ""
            (let* ((uri (car entry))
                   (template (cdr entry))
                   (name (plist-get template :name))
                   (description (plist-get template :description))
                   (mime-type
                    (or (plist-get template :mime-type) "text/plain"))
                   (handler (plist-get template :handler))
                   (handler-name
                    (anvil-server--get-handler-name handler))
                   (indent
                    (if multi-server
                        "    "
                      "  ")))
              (insert (format "%s%s\n" indent uri))
              (anvil-server--insert-item-properties
               name description
               (list :mime-type mime-type)
               indent)
              (insert
               (format "%s  Handler: %s\n" indent handler-name))
              (insert "\n"))))))
    (display-buffer (current-buffer))))

;;;###autoload
(defun anvil-server-show-metrics ()
  "Display metrics in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*MCP Metrics*")
    (erase-buffer)
    (insert "MCP Usage Metrics\n")
    (insert "=================\n\n")
    (insert (format-time-string "Session started: %F %T\n\n"))

    ;; Separate into three categories
    (let ((method-metrics nil)
          (notification-metrics nil)
          (tool-metrics nil))
      (maphash
       (lambda (key metrics)
         (cond
          ((string-match-p ":" key)
           (push (cons key metrics) tool-metrics))
          ((string-prefix-p "notifications/" key)
           (push (cons key metrics) notification-metrics))
          (t
           (push (cons key metrics) method-metrics))))
       anvil-server-metrics--table)

      ;; Display method-level metrics
      (when method-metrics
        (insert "Method Calls:\n")
        (insert
         "---------------------------------------- ------ ------- ----------\n")
        (dolist (entry
                 (sort method-metrics
                       #'anvil-server--entry-key-lessp))
          (insert
           (anvil-server--format-metrics-with-errors
            (car entry) (cdr entry))))
        (insert "\n"))

      ;; Display notifications
      (when notification-metrics
        (insert "Notifications:\n")
        (insert "---------------------------------------- ------\n")
        (dolist (entry
                 (sort notification-metrics
                       #'anvil-server--entry-key-lessp))
          (let* ((key (car entry))
                 (metrics (cdr entry))
                 (total (anvil-server-metrics-calls metrics)))
            ;; Notifications don't have errors, so simpler display
            (insert (format "%-40s %6d\n" key total))))
        (insert "\n"))

      ;; Display tool-specific metrics
      (when tool-metrics
        (insert "Tool Usage:\n")
        (insert
         "---------------------------------------- ------ ------- ----------\n")
        (dolist (entry
                 (sort tool-metrics
                       #'anvil-server--entry-key-lessp))
          (let* ((key (car entry))
                 (metrics (cdr entry))
                 (display-name
                  (if (string-match "tools/call:\\(.*\\)" key)
                      (match-string 1 key)
                    key)))
            (insert
             (anvil-server--format-metrics-with-errors
              display-name metrics
              t)))))

      ;; Summary with totals
      (let ((method-total 0)
            (method-errors 0)
            (tool-total 0)
            (tool-errors 0))
        ;; Calculate totals
        (dolist (entry method-metrics)
          (let ((metrics (cdr entry)))
            (cl-incf
             method-total (anvil-server-metrics-calls metrics))
            (cl-incf
             method-errors (anvil-server-metrics-errors metrics))))
        (dolist (entry tool-metrics)
          (let ((metrics (cdr entry)))
            (cl-incf
             tool-total (anvil-server-metrics-calls metrics))
            (cl-incf
             tool-errors (anvil-server-metrics-errors metrics))))

        ;; Display summary
        (insert "\nSummary:\n")
        (insert "--------\n")
        (insert
         (format "Methods: %d calls, %d errors (%.1f%%)\n"
                 method-total method-errors
                 (anvil-server-metrics--error-rate
                  method-total method-errors)))
        (insert
         (format "Tools: %d calls, %d errors (%.1f%%)\n"
                 tool-total tool-errors
                 (anvil-server-metrics--error-rate
                  tool-total tool-errors)))))
    (display-buffer (current-buffer))))

(provide 'anvil-server-commands)

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; package-lint-main-file: "anvil-server.el"
;; End:

;;; anvil-server-commands.el ends here
