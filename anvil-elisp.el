;;; anvil-elisp.el --- Elisp development tools for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 anvil-elisp.el contributors

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Laurynas Biveinis
;; Version: 1.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, development
;; URL: https://github.com/zawatton/anvil.el

;;; Commentary:

;; This package provides an MCP server for agentic Elisp development.

;;; Code:

(require 'anvil-server)
(require 'help-fns)
(require 'pp)
(require 'info-look)
(require 'cl-lib)
(require 'ert)
(require 'bytecomp)


;;; System Directory Setup

(defvar anvil-elisp--system-lisp-dir
  (let* ((data-parent
          (file-name-directory (directory-file-name data-directory)))
         (lisp-dir (expand-file-name "lisp/" data-parent)))
    (when (file-directory-p lisp-dir)
      lisp-dir))
  "System Lisp directory for Emacs installation.
Computed once at package load time from `data-directory'.")

(defconst anvil-elisp--server-id "emacs-eval"
  "Server ID for this MCP server.
Matches the id passed to `anvil-server-process-jsonrpc' by the
stdio shim (--server-id=emacs-eval) so tools are visible to the
shared MCP connection alongside anvil-file, anvil-org, etc.")

(defgroup anvil-elisp nil
  "MCP server for agentic Elisp development."
  :group 'tools
  :prefix "anvil-elisp-")

(defcustom anvil-elisp-additional-allowed-dirs nil
  "Additional directories to allow for elisp-read-source-file.
List of directory paths that should be accessible in addition to
the default Emacs system and ELPA directories.

This is useful for users of alternative package managers like
straight.el, elpaca, or custom package setups.

Example:
  (setq anvil-elisp-additional-allowed-dirs
        \\='(\"~/.emacs.d/straight/build/\"
           \"~/.emacs.d/straight/repos/\"
           \"~/my-elisp-packages/\"))

Security note: Only add directories you trust, as this allows
the MCP server to read any .el files in these locations."
  :type '(repeat directory)
  :group 'anvil-elisp
  :safe (lambda (val) (and (listp val) (cl-every #'stringp val))))

;;; Utility Functions

(defun anvil-elisp--non-empty-docstring-p (doc)
  "Return t if DOC is a non-empty documentation string, nil otherwise."
  (and doc (not (string-empty-p doc))))

(defmacro anvil-elisp--with-auto-compression (&rest body)
  "Execute BODY with `auto-compression-mode' temporarily enabled.
Restores the original mode state after BODY completes."
  (declare (indent 0) (debug t))
  `(let ((anvil-elisp--was-enabled auto-compression-mode))
     (unwind-protect
         (progn
           (unless anvil-elisp--was-enabled
             (auto-compression-mode 1))
           ,@body)
       (unless anvil-elisp--was-enabled
         (auto-compression-mode -1)))))

;;; JSON Response Helpers

(defun anvil-elisp--json-encode-source-location
    (source file-path start-line end-line)
  "Encode a source location response as JSON.
SOURCE is the source code string.
FILE-PATH is the absolute path to the source file.
START-LINE and END-LINE are 1-based line numbers."
  (json-encode
   `((source . ,source)
     (file-path . ,file-path)
     (start-line . ,start-line)
     (end-line . ,end-line))))

(defun anvil-elisp--json-encode-not-found (symbol message)
  "Encode a not-found response as JSON.
SYMBOL is the symbol that was looked up.
MESSAGE is the error or not-found message."
  (json-encode
   `((found . :json-false) (symbol . ,symbol) (message . ,message))))

(defun anvil-elisp--validate-symbol (name type &optional intern-p)
  "Validate that NAME is a non-empty string suitable for a symbol.
TYPE is a string describing the symbol type for error messages.
If INTERN-P is non-nil, return the interned symbol, otherwise just validate.
Throws an error if validation fails."
  (unless (stringp name)
    (anvil-server-tool-throw (format "Invalid %s name" type)))
  (when (string-empty-p name)
    (anvil-server-tool-throw (format "Empty %s name" type)))
  (when intern-p
    (intern name)))

;;; Property Collection

(defun anvil-elisp--extract-function-properties (sym)
  "Extract all properties for function symbol SYM.
Returns an alist of properties or nil if not a function."
  (when (fboundp sym)
    (let* ((fn (symbol-function sym))
           (is-alias (symbolp fn))
           (aliased-to (and is-alias fn)))
      `((function . ,fn)
        (is-alias . ,is-alias)
        (aliased-to . ,aliased-to)
        (is-subr
         .
         ,(subrp
           (if is-alias
               aliased-to
             fn)))
        (doc . ,(documentation sym))
        (file . ,(find-lisp-object-file-name sym 'defun))))))

(defun anvil-elisp--json-bool (value)
  "Convert Elisp boolean VALUE to JSON boolean representation.
In Elisp, nil is false and everything else is true.
For JSON encoding, returns t for truthy values and :json-false for nil.
This ensures proper JSON boolean serialization."
  (if value
      t
    :json-false))

(defun anvil-elisp--variable-exists-p (props)
  "Check if variable exists based on its PROPS.
A variable exists if it is bound, documented, defined in a file,
is a custom variable, is obsolete, or is an alias."
  (or (alist-get 'bound-p props)
      (alist-get 'doc props)
      (alist-get 'file props)
      (alist-get 'custom-p props)
      (alist-get 'obsolete props)
      (alist-get 'is-alias props)))

;;; Tool Implementations

(defun anvil-elisp--describe-function (function)
  "Get full documentation for Emacs Lisp FUNCTION.

MCP Parameters:
  function - The name of the function to describe"
  (anvil-server-with-error-handling
   (let ((sym (anvil-elisp--validate-symbol function "function" t)))
     (if (fboundp sym)
         (with-temp-buffer
           (let ((standard-output (current-buffer)))
             (describe-function-1 sym)
             (buffer-string)))
       (anvil-server-tool-throw
        (format "Function %s is void" function))))))

;;; Function Definition Helpers

(defun anvil-elisp--process-alias-source
    (source function aliased-to file-path start-line end-line)
  "Post-process SOURCE for function aliases to return a useful defalias form.
If source is just a quoted symbol, replace it with a synthetic defalias form.
Returns a JSON encoded response with enhanced alias information.

FUNCTION is the alias function name.
ALIASED-TO is the target function name.
FILE-PATH, START-LINE, and END-LINE specify source location information."
  (let ((doc (or (documentation (intern-soft function)) "")))
    (if (and source
             (string-match-p (format "['']%s\\>" function) source)
             (not (string-match-p "defalias" source)))
        ;; Generate synthetic defalias form
        (let ((func-def
               (format "(defalias '%s #'%s %S)"
                       function
                       aliased-to
                       doc)))
          (anvil-elisp--json-encode-source-location
           func-def file-path start-line end-line))
      ;; Pass through original source
      (anvil-elisp--json-encode-source-location
       source file-path start-line end-line))))

(defun anvil-elisp--get-function-definition-c-function (fn-name)
  "Return response for C-implemented FN-NAME in get-function-definition."
  (json-encode
   `((is-c-function . t)
     (function-name . ,fn-name)
     (message .
              ,(format
                "Function `%s` is implemented in C source code. \
Use elisp-describe-function tool to get its docstring."
                fn-name)))))

(defun anvil-elisp--extract-function-body (fn has-doc)
  "Extract body from function object FN.
HAS-DOC indicates whether the function has a docstring.
Returns nil if FN is not a function."
  (if (not (functionp fn))
      nil
    (cond
     ;; Emacs 30+ interpreted-function objects
     ((eq (type-of fn) 'interpreted-function)
      ;; Extract body from interpreted-function
      ;; Format: #[args body env bytecode doc]
      (aref fn 1))
     ;; Emacs 29 and earlier cons-based functions
     ((consp fn)
      ;; Function format: (closure ENV ARGS [DOCSTRING] . BODY)
      ;; or: (lambda ARGS [DOCSTRING] . BODY)
      ;; Skip: car (closure/lambda), cadr (env/args), caddr (args/docstring)
      ;; If has docstring, body starts at position 3 (0-indexed)
      ;; If no docstring, body starts at position 2 (0-indexed)
      (nthcdr
       (if has-doc
           3 ; Skip closure/lambda, env/args, and docstring
         2) ; Skip closure/lambda and args only
       fn))
     ;; Fallback for other types
     (t
      (anvil-server-tool-throw
       (format "Don't know how to extract body from function type: %s"
               (type-of fn)))))))

(defun anvil-elisp--reconstruct-function-definition
    (fn-name args doc body)
  "Reconstruct a function definition from its runtime components.
This is used for interactively defined functions where the source file
is not available.  Creates a synthetic defun form.

FN-NAME is the function name as a string.
ARGS is the argument list.
DOC is the documentation string (can be empty).
BODY is the list of body expressions."
  (unless body
    (anvil-server-tool-throw
     (format "Failed to extract body for function %s" fn-name)))
  (let ((defun-form
         `(defun ,(intern fn-name) ,(or args '())
            ,@
            (when (anvil-elisp--non-empty-docstring-p doc)
              (list doc))
            ,@body)))
    (pp-to-string defun-form)))

(defun anvil-elisp--get-function-definition-interactive
    (fn-name sym fn)
  "Handle interactively defined function FN-NAME.
SYM is the function symbol, FN is the function object.
Returns JSON response for an interactively defined function."
  (let* ((args (help-function-arglist sym t))
         (doc (documentation sym))
         (body
          (anvil-elisp--extract-function-body
           fn (anvil-elisp--non-empty-docstring-p doc)))
         (func-def
          (anvil-elisp--reconstruct-function-definition
           fn-name args doc body)))
    (anvil-elisp--json-encode-source-location
     func-def "<interactively defined>" 1 1)))

;;; Variable Helpers

(defun anvil-elisp--find-custom-group (sym)
  "Find the custom group that contain variable SYM.
Returns the group name as a string, or nil if not found."
  (catch 'found
    (mapatoms
     (lambda (group-sym)
       (when (get group-sym 'custom-group)
         (dolist (member (get group-sym 'custom-group))
           (when (and (eq (car member) sym)
                      (eq (cadr member) 'custom-variable))
             (throw 'found (symbol-name group-sym)))))))
    nil))

(defun anvil-elisp--find-header-comment-start (point)
  "Find the start of header comments preceding POINT.
Returns the position of the first comment line, or POINT if no comments found."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (forward-line -1)

    ;; Check if there's a header comment
    (if (looking-at "^[ \t]*;;")
        (progn
          ;; Find first line of the consecutive comment block
          (while (and (looking-at "^[ \t]*;;")
                      (> (forward-line -1) -1)))
          ;; We went one line too far back
          (forward-line 1)
          (point))
      point)))

(defun anvil-elisp--extract-source-region (start-point end-point)
  "Extract source code between START-POINT and END-POINT.
Returns a list of (source start-line end-line)."
  (list
   (buffer-substring-no-properties start-point end-point)
   (line-number-at-pos start-point)
   (line-number-at-pos end-point)))

(defun anvil-elisp--extract-variable-properties (sym)
  "Extract all properties for variable symbol SYM.
Returns an alist of properties."
  (let* ((doc (documentation-property sym 'variable-documentation))
         (file (find-lisp-object-file-name sym 'defvar))
         (custom-p (custom-variable-p sym))
         (obsolete (get sym 'byte-obsolete-variable))
         (bound-p (boundp sym))
         (alias-target (indirect-variable sym))
         (is-alias (not (eq sym alias-target)))
         (is-special (special-variable-p sym))
         (custom-group
          (when custom-p
            (anvil-elisp--find-custom-group sym)))
         (custom-type
          (when custom-p
            (get sym 'custom-type))))
    `((doc . ,doc)
      (file . ,file)
      (custom-p . ,custom-p)
      (obsolete . ,obsolete)
      (bound-p . ,bound-p)
      (alias-target . ,alias-target)
      (is-alias . ,is-alias)
      (is-special . ,is-special)
      (custom-group . ,custom-group)
      (custom-type . ,custom-type))))

(defun anvil-elisp--build-variable-json-response (variable props)
  "Build JSON response for VARIABLE using collected PROPS.
VARIABLE is the variable name string, PROPS is an alist of properties."
  (let ((bound-p (alist-get 'bound-p props))
        (doc (alist-get 'doc props))
        (file (alist-get 'file props))
        (custom-p (alist-get 'custom-p props))
        (obsolete (alist-get 'obsolete props))
        (is-alias (alist-get 'is-alias props))
        (alias-target (alist-get 'alias-target props))
        (is-special (alist-get 'is-special props))
        (custom-group (alist-get 'custom-group props))
        (custom-type (alist-get 'custom-type props)))
    (json-encode
     `((name . ,variable)
       (bound . ,(anvil-elisp--json-bool bound-p))
       ,@
       (when bound-p
         `((value-type
            .
            ,(symbol-name
              (type-of (symbol-value (intern variable)))))))
       (documentation . ,doc)
       (source-file . ,(or file "<interactively defined>"))
       (is-custom . ,(anvil-elisp--json-bool custom-p))
       ,@
       (when custom-group
         `((custom-group . ,custom-group)))
       ,@
       (when custom-type
         `((custom-type . ,(format "%S" custom-type))))
       (is-obsolete . ,(anvil-elisp--json-bool obsolete))
       (is-alias . ,(anvil-elisp--json-bool is-alias))
       ,@
       (when obsolete
         `((obsolete-since . ,(nth 2 obsolete))
           (obsolete-replacement . ,(nth 0 obsolete))))
       (is-special . ,(anvil-elisp--json-bool is-special))
       ,@
       (when is-alias
         `((alias-target . ,(symbol-name alias-target))))))))

(defun anvil-elisp--describe-variable (variable)
  "Get information about Emacs Lisp VARIABLE without exposing its value.

MCP Parameters:
  variable - The name of the variable to describe"
  (let* ((sym (anvil-elisp--validate-symbol variable "variable" t))
         (props (anvil-elisp--extract-variable-properties sym)))
    (if (anvil-elisp--variable-exists-p props)
        (anvil-elisp--build-variable-json-response variable props)
      (anvil-server-tool-throw
       (format "Variable %s is not bound" variable)))))

;;; File-based Function Extraction

(defun anvil-elisp--get-function-definition-from-file
    (fn-name sym func-file is-alias aliased-to)
  "Extract function definition for FN-NAME from FUNC-FILE.
SYM is the function symbol.
IS-ALIAS and ALIASED-TO are used for special handling of aliases."
  (anvil-elisp--with-auto-compression
    (let ((actual-file
           (cond
            ((file-exists-p func-file)
             func-file)
            ((file-exists-p (concat func-file ".gz"))
             (concat func-file ".gz"))
            (t
             (anvil-server-tool-throw
              (format "File not found: %s (tried .el and .el.gz)"
                      func-file))))))
      (with-temp-buffer
        (insert-file-contents actual-file)
        (goto-char (point-min))
        (let ((def-pos
               (find-function-search-for-symbol sym nil func-file)))
          (unless def-pos
            (anvil-server-tool-throw
             (format "Could not locate definition for %s" fn-name)))
          (goto-char (cdr def-pos))

          ;; Find the start point including any header comments
          (let* ((func-point (point))
                 (start-point
                  (anvil-elisp--find-header-comment-start
                   func-point))
                 (end-point
                  (progn
                    (goto-char func-point)
                    (forward-sexp)
                    (point)))
                 (source-info
                  (anvil-elisp--extract-source-region
                   start-point end-point)))

            ;; Return the result, with special handling for aliases
            (if is-alias
                (anvil-elisp--process-alias-source
                 (nth 0 source-info)
                 fn-name
                 aliased-to
                 func-file
                 (nth 1 source-info)
                 (nth 2 source-info))
              (anvil-elisp--json-encode-source-location
               (nth 0 source-info)
               func-file
               (nth 1 source-info)
               (nth 2 source-info)))))))))

(defun anvil-elisp--extract-function-info (sym)
  "Extract function information for symbol SYM.
Returns (fn is-alias aliased-to) or nil if not a function."
  (when (fboundp sym)
    (let* ((fn (symbol-function sym))
           (is-alias (symbolp fn))
           (aliased-to (and is-alias (symbol-name fn))))
      (list fn is-alias aliased-to))))

(defun anvil-elisp--get-function-definition-dispatch
    (function sym fn-info)
  "Dispatch to appropriate handler based on function type.
FUNCTION is the function name string.
SYM is the function symbol.
FN-INFO is the result from `anvil-elisp--extract-function-info`."
  (let ((fn (nth 0 fn-info))
        (is-alias (nth 1 fn-info))
        (aliased-to (nth 2 fn-info)))
    (cond
     ;; C-implemented function
     ((subrp fn)
      (anvil-elisp--get-function-definition-c-function function))

     ;; Has source file
     ((find-lisp-object-file-name sym 'defun)
      (anvil-elisp--get-function-definition-from-file
       function
       sym
       (find-lisp-object-file-name sym 'defun)
       is-alias
       aliased-to))

     ;; Interactive alias
     (is-alias
      (anvil-elisp--process-alias-source
       (format "'%s" function)
       function
       aliased-to
       "<interactively defined>"
       1
       1))

     ;; Interactive function
     (t
      (anvil-elisp--get-function-definition-interactive
       function sym fn)))))

(defun anvil-elisp--strip-defs-uri (fn)
  "Return FN with a leading `defs://SHA/' citation prefix stripped."
  (if (and (stringp fn) (string-prefix-p "defs://" fn))
      (let ((rest (substring fn (length "defs://"))))
        ;; defs://<sha>/<symbol> — take everything after the first /.
        (if (string-match "\\`[^/]+/\\(.+\\)\\'" rest)
            (match-string 1 rest)
          rest))
    fn))

(defun anvil-elisp--get-function-definition (function)
  "Get the source code definition for Emacs Lisp FUNCTION.
FUNCTION may also be a `defs://SHA/SYMBOL' citation URI from the
disclosure Layer-1 / Layer-2 tools; the symbol part is extracted
transparently.

MCP Parameters:
  function - The name of the function to retrieve
             (or `defs://SHA/SYMBOL' citation URI)"
  (let* ((name (anvil-elisp--strip-defs-uri function))
         (sym (anvil-elisp--validate-symbol name "function" t))
         (fn-info (anvil-elisp--extract-function-info sym)))
    (unless fn-info
      (anvil-server-tool-throw
       (format "Function %s is not found" name)))
    (anvil-elisp--get-function-definition-dispatch
     name sym fn-info)))

;;; Info Documentation Helpers

(defun anvil-elisp--extract-info-node-content ()
  "Extract the complete content of the current Info node.
Assumes we're in an Info buffer at the correct node."
  (let ((start nil)
        (end nil))
    ;; Find the start of actual content (after the node header)
    (goto-char (point-min))
    (when (re-search-forward "^File: [^,]+,  Node: [^,\n]+.*\n" nil t)
      (setq start (point)))

    ;; Find the end of content
    (when start
      (goto-char start)
      ;; Look for the next node boundary or end of buffer
      (if (re-search-forward "^\^_" nil t)
          (setq end (match-beginning 0))
        (setq end (point-max))))

    ;; Extract and clean up the content
    (when (and start end)
      (anvil-elisp--clean-info-content
       (buffer-substring-no-properties start end)))))

(defun anvil-elisp--clean-info-content (content)
  "Clean up Info formatting from CONTENT.
Removes navigation markers while preserving documentation structure."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))

    ;; Remove footnote references like (*note ...)
    (while (re-search-forward "\\*[Nn]ote[ \n][^:]*::" nil t)
      (replace-match "[See: \\&]"))

    ;; Return cleaned content
    (buffer-string)))

(defun anvil-elisp--perform-info-lookup (symbol)
  "Perform the actual Info lookup for SYMBOL.
Returns an alist with lookup results or nil if not found."
  (condition-case nil
      (with-temp-buffer
        ;; Set up for info-lookup
        (let ((mode 'emacs-lisp-mode)
              (info-buf nil)
              (node nil)
              (manual nil)
              (content nil))

          ;; info-lookup-symbol needs a buffer in the right mode
          (emacs-lisp-mode)

          ;; Perform the lookup - this will open an Info buffer
          (save-window-excursion
            (info-lookup-symbol symbol mode)

            ;; Get the Info buffer that was opened
            (setq info-buf (get-buffer "*info*"))

            (when info-buf
              (with-current-buffer info-buf
                ;; Extract node information
                (goto-char (point-min))
                (when (re-search-forward
                       "^File: \\([^,]+\\),  Node: \\([^,\n]+\\)"
                       nil t)
                  (setq manual (match-string 1))
                  (setq node (match-string 2))
                  ;; Remove .info extension if present
                  (when (string-match "\\.info\\'" manual)
                    (setq manual
                          (substring manual 0 (match-beginning 0)))))

                ;; Extract content
                (setq content
                      (anvil-elisp--extract-info-node-content)))))

          ;; Return results if we found something
          (when (and node content)
            `((found . t)
              (symbol . ,symbol)
              (node . ,node)
              (manual . ,manual)
              (content . ,content)
              (info-ref . ,(format "(%s)%s" manual node))))))
    ;; If lookup fails, return nil
    (error
     nil)))

(defun anvil-elisp--info-lookup-symbol (symbol)
  "Look up SYMBOL in Elisp Info documentation.

MCP Parameters:
  symbol - The symbol to look up (string)"
  (anvil-server-with-error-handling
   ;; Validate input
   (anvil-elisp--validate-symbol symbol "symbol")
   ;; Perform lookup
   (let ((result (anvil-elisp--perform-info-lookup symbol)))
     (if result
         (json-encode result)
       (anvil-elisp--json-encode-not-found
        symbol
        (format "Symbol '%s' not found in Elisp Info documentation"
                symbol))))))

(defun anvil-elisp--library-name-p (name)
  "Return non-nil if NAME is a library name, not a file path.
Library names are non-blank strings without path separators.
Strings containing '/' or '\\\\' are treated as paths, not library names."
  (and (stringp name)
       (not (string-blank-p name))
       (not (file-name-absolute-p name))
       (not (string-match-p "[/\\]" name))))

(defun anvil-elisp--resolve-library-to-source-path (library-name)
  "Resolve LIBRARY-NAME to its source file path that exists on disk.

Uses `locate-library' to find the library, then:
  - Converts .elc → .el (source, not bytecode)
  - Checks if .el exists, otherwise tries .el.gz

Returns the actual file path (.el or .el.gz) that exists on disk.
Throws an error if the library or source file is not found.

Example transformations:
  `locate-library' returns /path/file.el     → /path/file.el
  `locate-library' returns /path/file.el.gz  → /path/file.el.gz
  `locate-library' returns /path/file.elc    → /path/file.el (or .el.gz)
  `locate-library' returns /path/file.elc.gz → /path/file.el (or .el.gz)"
  (let ((library-path (locate-library library-name)))
    (unless library-path
      (anvil-server-tool-throw
       (format "Library not found: %s" library-name)))
    ;; Remove .gz extension first if present
    ;; This must be done before .elc conversion to handle .elc.gz correctly
    (when (string-suffix-p ".gz" library-path)
      (setq library-path (file-name-sans-extension library-path)))
    ;; Convert .elc to .el if needed (after .gz removal)
    (when (string-suffix-p ".elc" library-path)
      (setq library-path
            (concat (file-name-sans-extension library-path) ".el")))
    ;; Find the actual source file that exists on disk
    (let ((actual-file
           (cond
            ((file-exists-p library-path)
             library-path)
            ((file-exists-p (concat library-path ".gz"))
             (concat library-path ".gz"))
            (t
             nil))))
      (unless actual-file
        (anvil-server-tool-throw
         (format
          "Source file not found for library %s (tried %s and %s.gz)"
          library-name library-path library-path)))
      actual-file)))

(defun anvil-elisp--read-source-file (library-or-path)
  "Read Elisp source file from allowed locations.
Accepts either a library name or absolute path via LIBRARY-OR-PATH.

Library names (e.g., \"subr\", \"mcp-server-lib\") are resolved via
`locate-library' and validated against the allowed directories.

Absolute paths must be as returned by other elisp-dev tools.

Handles both .el and .el.gz files transparently.

MCP Parameters:
  library-or-path - Library name (e.g., \"subr\") or absolute .el path"
  (anvil-server-with-error-handling
   ;; 1. Resolve library name to absolute path if needed
   (let ((file-path
          (if (anvil-elisp--library-name-p library-or-path)
              (anvil-elisp--resolve-library-to-source-path
               library-or-path)
            library-or-path)))

     ;; 2. Validate input format
     (unless (and (stringp file-path)
                  (file-name-absolute-p file-path)
                  (or (string-suffix-p ".el" file-path)
                      (string-suffix-p ".el.gz" file-path)))
       (anvil-server-tool-throw
        "Invalid path format: must be absolute path ending in .el or .el.gz"))

     ;; 3. Check for path traversal
     (when (string-match-p "\\.\\." file-path)
       (anvil-server-tool-throw
        "Path contains illegal '..' traversal"))

     ;; 4. Resolve symlinks and validate location
     (let* ((true-path (file-truename file-path))
            ;; Build list of allowed package directories
            (allowed-dirs
             (append
              ;; Current package-user-dir
              (when (boundp 'package-user-dir)
                (list
                 (file-truename
                  (file-name-as-directory package-user-dir))))
              ;; All dirs from package-directory-list
              (mapcar
               (lambda (dir)
                 (file-truename (file-name-as-directory dir)))
               package-directory-list)
              ;; System lisp directory
              (when anvil-elisp--system-lisp-dir
                (list
                 (file-truename
                  (file-name-as-directory
                   anvil-elisp--system-lisp-dir))))
              ;; User-configured additional directories
              (mapcar
               (lambda (dir)
                 (file-truename (file-name-as-directory dir)))
               anvil-elisp-additional-allowed-dirs)))
            ;; Check if file is under any allowed directory
            (allowed-p
             (cl-some
              (lambda (dir)
                (and dir (string-prefix-p dir true-path)))
              allowed-dirs)))

       (unless allowed-p
         (anvil-server-tool-throw
          "Access denied: path outside allowed directories"))

       ;; 5. Verify file exists and read contents
       (unless (file-exists-p true-path)
         (anvil-server-tool-throw
          (format "File not found: %s" library-or-path)))

       (anvil-elisp--with-auto-compression
         (with-temp-buffer
           (insert-file-contents true-path)
           (buffer-string)))))))

;;; ERT test runner — compact result for LLM consumption

(defun anvil-elisp--ert-registered-names ()
  "Return a list of every symbol currently holding an ERT test."
  (let (acc)
    (mapatoms
     (lambda (sym)
       (when (get sym 'ert--test)
         (push sym acc))))
    acc))

(defun anvil-elisp--ert-fresh-feature (file)
  "Infer the feature symbol to unload before FILE is reloaded.
Convention: a test file named `FOO-test.el' tests the feature
`FOO'.  Strips the trailing `-test' from the file's basename and
returns an interned symbol.  Returns nil when the pattern does
not match (caller should then pass an explicit feature name)."
  (let ((base (file-name-base file)))
    (when (string-match "\\`\\(.+\\)-test\\'" base)
      (intern (match-string 1 base)))))

(defun anvil-elisp--ert-invalidate-cache (feature test-file)
  "Unload FEATURE (if loaded) and delete stale .elc files.
Removes both the companion source .elc (derived from FEATURE's
source location) and TEST-FILE's own .elc so the next `load'
picks up the freshest bytes."
  (when (and feature (featurep feature))
    (unload-feature feature t))
  (let ((test-elc (concat (file-name-sans-extension test-file) ".elc")))
    (when (file-exists-p test-elc)
      (ignore-errors (delete-file test-elc))))
  (when feature
    (let* ((src (locate-library (symbol-name feature)))
           (src-elc (and src
                         (concat (file-name-sans-extension src) ".elc"))))
      (when (and src-elc (file-exists-p src-elc))
        (ignore-errors (delete-file src-elc))))))

(defun anvil-elisp--ert-run (file &optional selector fresh)
  "Run ERT tests from FILE and return a compact result plist.

MCP Parameters:
  file     - Path to an .el file that defines ERT tests (string).
             The file is `load'ed into the current Emacs session so
             its `ert-deftest' forms register with the global test
             registry.
  selector - Optional ERT selector as an Elisp-readable string
             (e.g. \"t\" for all, \"\\\"my-test-name\\\"\" for one,
             or \"(tag :integration)\" for tagged).  Defaults to t.
  fresh    - Optional cache invalidation hint.  When the string is
             truthy (\"t\", \"true\", \"1\") or names a feature
             symbol, unload that feature and delete its companion
             `.elc' plus the test file's own `.elc' before loading.
             Empty / \"nil\" / \"false\" / \"0\" skip invalidation.
             \"t\" auto-infers the feature from FILE's basename
             (strips a trailing `-test' — so
             `tests/anvil-worker-test.el' reloads `anvil-worker').

Returns a printed plist:
  (:passed N :failed M :skipped S :elapsed-sec T :failures F)
where F is a list of (:name STR :condition STR :backtrace STR).
Backtraces are truncated to keep the response small.

Runs tests synchronously in the current process, so side effects
from the test file persist after the call.  Intended for tight
feedback loops during development — use a batch subprocess when
isolation matters."
  (let* ((path (expand-file-name file))
         (sel  (cond
                ((null selector) t)
                ((and (stringp selector) (string-empty-p selector)) t)
                ((and (stringp selector) (string= selector "t")) t)
                ((stringp selector)
                 (condition-case nil (read selector) (error t)))
                (t t)))
         (_invalidate
          (when fresh
            (let* ((truthy (cond
                            ((symbolp fresh) fresh)
                            ((stringp fresh)
                             (let ((s (string-trim (downcase fresh))))
                               (cond
                                ((member s '("" "nil" "false" "0")) nil)
                                ((member s '("t" "true" "1")) t)
                                (t (intern fresh)))))
                            (t fresh)))
                   (feat (cond
                          ((null truthy) nil)
                          ((eq truthy t) (anvil-elisp--ert-fresh-feature path))
                          ((symbolp truthy) truthy))))
              (when truthy
                (anvil-elisp--ert-invalidate-cache feat path)))))
         (start (float-time))
         (passed 0) (failed 0) (skipped 0)
         (failures nil)
         (before (anvil-elisp--ert-registered-names)))
    (load path nil t)
    (let* ((after   (anvil-elisp--ert-registered-names))
           (added   (cl-remove-if (lambda (n) (memq n before)) after))
           (tests
            (cond
             ((or (eq sel t) (null sel))
              (mapcar (lambda (n) (get n 'ert--test)) added))
             ((stringp sel)
              (mapcar (lambda (n) (get n 'ert--test))
                      (cl-remove-if-not
                       (lambda (n) (string-match-p sel (symbol-name n)))
                       added)))
             ((symbolp sel)
              (when (memq sel added) (list (get sel 'ert--test))))
             (t (mapcar (lambda (n) (get n 'ert--test)) added)))))
      (dolist (test tests)
        (let ((result (ert-run-test test)))
          (cond
           ((ert-test-passed-p result) (cl-incf passed))
           ((ert-test-skipped-p result) (cl-incf skipped))
           (t
            (cl-incf failed)
            (let* ((cond-obj (and (ert-test-result-with-condition-p result)
                                  (ert-test-result-with-condition-condition
                                   result)))
                   (cond-str (if cond-obj
                                 (let ((s (prin1-to-string cond-obj)))
                                   (if (> (length s) 400)
                                       (concat (substring s 0 400) " …")
                                     s))
                               "unknown failure"))
                   (bt-obj   (and (ert-test-result-with-condition-p result)
                                  (ert-test-result-with-condition-backtrace
                                   result)))
                   (bt-str   (if bt-obj
                                 (let ((raw (prin1-to-string bt-obj)))
                                   (if (> (length raw) 800)
                                       (concat (substring raw 0 800) " …")
                                     raw))
                               "")))
              (push (list :name      (symbol-name (ert-test-name test))
                          :condition cond-str
                          :backtrace bt-str)
                    failures)))))))
    (format "%S" (list :passed passed
                       :failed failed
                       :skipped skipped
                       :elapsed-sec (- (float-time) start)
                       :failures (nreverse failures)))))

;;; Byte-compile — compact result

(defun anvil-elisp--byte-compile-file (file)
  "Byte-compile FILE and return a compact result plist.

MCP Parameters:
  file - Path to an .el file to byte-compile (string).

Returns a printed plist:
  (:ok BOOL :output PATH :warnings (...) :errors (...))
Warnings and errors are parsed out of the byte-compile log so the
caller does not have to scan it."
  (let* ((path (expand-file-name file))
         (log-buf (get-buffer-create " *anvil-bc-log*"))
         (byte-compile-log-buffer (buffer-name log-buf))
         (warnings nil)
         (errors nil)
         (result nil))
    (with-current-buffer log-buf
      (let ((inhibit-read-only t)) (erase-buffer)))
    (condition-case err
        (setq result (byte-compile-file path))
      (error (push (error-message-string err) errors)))
    (with-current-buffer log-buf
      (goto-char (point-min))
      (while (re-search-forward
              "^\\(?:.*?:\\)?\\(?:[0-9]+:[0-9]+: ?\\)?\\(Warning\\|Error\\): \\(.*\\)$"
              nil t)
        (let ((kind (match-string 1))
              (msg  (match-string 2)))
          (if (equal kind "Warning")
              (push msg warnings)
            (push msg errors)))))
    (format "%S" (list :ok (and result (null errors))
                       :output (concat (file-name-sans-extension path) ".elc")
                       :warnings (nreverse warnings)
                       :errors (nreverse errors)))))

;;;###autoload
(defun anvil-elisp-enable ()
  "Enable the Elisp development MCP tools."
  (anvil-server-register-tool
   #'anvil-elisp--ert-run
   :id "elisp-ert-run"
   :server-id anvil-elisp--server-id
   :description
   "Run ERT tests from a file and return a compact plist instead of
the chatty output `emacs --batch ... ert-run-tests-batch-and-exit'
produces.  Returns :passed :failed :skipped :elapsed-sec :failures,
with each failure carrying a truncated condition and backtrace.
Intended for tight test/fix loops during development — far cheaper
in tokens than shelling out and parsing stdout."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-elisp--byte-compile-file
   :id "elisp-byte-compile-file"
   :server-id anvil-elisp--server-id
   :description
   "Byte-compile a single .el file and return a plist:
\(:ok BOOL :output PATH :warnings (...) :errors (...)).  Replaces
shelling out to `emacs --batch -f batch-byte-compile' when you
just need a clean yes/no plus the list of diagnostics.  Errors
and warnings are parsed out of the log buffer for you."
   :read-only nil
   :offload t
   :offload-inherit-load-path t)
  (anvil-server-register-tool
   #'anvil-elisp--describe-function
   :id "elisp-describe-function"
   :server-id anvil-elisp--server-id
   :description
   "Get documentation for an Emacs Lisp function or check if it exists. Returns
function documentation from the current running Emacs environment, including all
currently loaded packages and libraries.

Supports:
- Regular functions (defun), macros (defmacro), inline functions (defsubst)
- Function aliases (shows both alias info and target function docs)
- Built-in C functions (subr)
- Byte-compiled functions
- Functions with or without documentation

Returns formatted documentation including:
- Function signature with argument names
- Full docstring with parameter descriptions
- Source file location
- Function type (closure, macro, subr, etc.)

Error cases:
- Non-existent functions return 'Function X is void'
- Invalid input types return 'Error: ...'"
   :read-only t)
  (anvil-server-register-tool
   #'anvil-elisp--get-function-definition
   :id "elisp-get-function-definition"
   :server-id anvil-elisp--server-id
   :description
   "Layer 3 of anvil progressive disclosure (see `disclosure-help').
Get the source code definition of an Emacs Lisp function with any header
comments. Accepts either a bare symbol name or a `defs://SHA/SYMBOL'
citation URI emitted by Layer 1 (`defs-index') / Layer 2 (`defs-search').
Returns source code with file path and 1-based line numbers. For
functions defined in C, returns a suggestion to call elisp-describe-function
tool instead.

Returns JSON with:
- source: Complete function definition including header comments
- file-path: Absolute path to source file or '<interactively defined>'
- start-line: Line number where definition starts (1-based)
- end-line: Line number where definition ends

Special handling:
- Function aliases: Returns the defalias form with docstring
- C functions: Returns is-c-function=true with suggestion message
- Interactive functions: Reconstructs defun from runtime representation
- Byte-compiled functions: Retrieves original source if available

Error cases:
- Non-existent functions return 'Function X is not found'
- Non-string input returns 'Invalid function name'

Use this tool when you need to:
- View or analyze function implementation
- Extract function source for modification
- Understand function structure with comments"
   :read-only t)
  (anvil-server-register-tool
   #'anvil-elisp--describe-variable
   :id "elisp-describe-variable"
   :server-id anvil-elisp--server-id
   :description
   "Get comprehensive information about an Emacs Lisp variable without
exposing its value. Essential for understanding variable definitions,
types, and relationships in Elisp code.

Parameters:
  variable - Variable name as a string (e.g., \"load-path\", \"custom-file\")

Returns JSON object with these fields:
  name - Variable name (string, always present)
  bound - Whether variable has a value (boolean, always present)
  value-type - Type of the current value like \"string\", \"cons\", \"integer\",
               \"symbol\" (string, only when bound is true)
  documentation - Variable's docstring (string or null, always present)
  source-file - File where defined, or \"<interactively defined>\"
                (string, always present)
  is-custom - Whether it's a defcustom variable (boolean, always present)
  custom-group - Which customization group it belongs to
                 (string, only when is-custom is true)
  custom-type - Type specification for customization like \"string\" or
                complex types (string, only when is-custom is true)
  is-obsolete - Whether marked as obsolete (boolean, always present)
  obsolete-since - Version when obsoleted
                   (string, only when is-obsolete is true)
  obsolete-replacement - Suggested replacement
                         (string, only when is-obsolete is true)
  is-alias - Whether this is an alias to another variable
             (boolean, always present)
  alias-target - The actual variable this aliases to
                 (string, only when is-alias is true)
  is-special - Whether it's a special/dynamic variable in lexical-binding
               context (boolean, always present)

Common use cases:
- Check if a configuration variable exists before using it
- Understand variable relationships (aliases, obsolescence)
- Verify variable types before setting values
- Find documentation for Emacs configuration options
- Discover which customization group a setting belongs to

Security: Never exposes actual values to prevent leaking sensitive data
like API keys, passwords, or personal information. Use this instead of
eval when exploring variables.

Error cases return error messages for:
- Non-string input
- Completely undefined variables (no binding, no documentation, no properties)"
   :read-only t)
  (anvil-server-register-tool
   #'anvil-elisp--info-lookup-symbol
   :id "elisp-info-lookup-symbol"
   :server-id anvil-elisp--server-id
   :description
   "Look up Elisp symbols in Info documentation and return the complete
documentation node. Returns the full content of the Info node containing
the symbol's documentation from the Emacs Lisp Reference Manual.

Parameters:
  symbol - The Elisp symbol to look up (string)

Returns JSON with:
  found - Whether documentation was found (boolean)
  symbol - The symbol that was looked up (string)
  node - The Info node name containing the documentation (string, when found)
  manual - The Info manual name, typically 'elisp' (string, when found)
  content - The complete Info node content including all examples,
            cross-references, and related information (string, when found)
  info-ref - Info reference like '(elisp)Node Name' for direct access
             (string, when found)
  message - Error or not-found message (string, when not found)

The content field contains the entire Info node, ensuring you have full
context including:
- Complete function/variable descriptions
- All code examples and usage patterns
- Cross-references to related concepts
- Any warnings, notes, or special considerations

Common symbols that can be looked up:
- Special forms: defun, defvar, let, if, cond, lambda
- Functions: mapcar, apply, funcall, concat
- Macros: when, unless, dolist, defmacro
- Variables: load-path, emacs-version
- Concepts: 'lexical binding', 'dynamic binding'

Error cases:
- Symbol not found in documentation
- Invalid symbol name
- Info system unavailable"
   :read-only t)
  (anvil-server-register-tool
   #'anvil-elisp--read-source-file
   :id "elisp-read-source-file"
   :server-id anvil-elisp--server-id
   :description
   "Read Elisp source files from Emacs system directories or ELPA packages.
Accepts either library names or absolute file paths.

Parameters:
  library-or-path - Library name (e.g., \\='subr', \\='mcp-server-lib') or
                    absolute path to .el file (string)

Input modes:
1. Library names (recommended for built-in and installed packages):
   - Simple names without path separators (e.g., \\='subr', \\='files')
   - Resolved via Emacs locate-library function
   - Examples: \\='subr', \\='mcp-server-lib', \\='org'

2. Absolute paths (for compatibility with other elisp-dev tools):
   - Full paths ending in .el (e.g., \\='/path/to/file.el')
   - Returned by elisp-get-function-definition
   - Examples: \\='/opt/homebrew/.../lisp/subr.el'

Security:
- Only reads from Emacs system lisp directories and ELPA directories
- Rejects paths with \"..\" traversal
- Resolves symlinks to prevent escaping allowed directories
- Library names must resolve to paths within allowed directories

Features:
- Transparently handles .el.gz compressed files
- Works with both built-in Emacs libraries and installed packages
- Returns complete file contents as string

Error cases:
- Library not found (locate-library returns nil)
- Invalid path format (paths must be absolute and end in .el)
- Path traversal attempts
- Access outside allowed directories
- File not found"
   :read-only t))

;;;###autoload
(defun anvil-elisp-disable ()
  "Disable the Elisp development MCP tools."
  (anvil-server-unregister-tool
   "elisp-ert-run" anvil-elisp--server-id)
  (anvil-server-unregister-tool
   "elisp-byte-compile-file" anvil-elisp--server-id)
  (anvil-server-unregister-tool
   "elisp-describe-function" anvil-elisp--server-id)
  (anvil-server-unregister-tool
   "elisp-get-function-definition" anvil-elisp--server-id)
  (anvil-server-unregister-tool
   "elisp-describe-variable" anvil-elisp--server-id)
  (anvil-server-unregister-tool
   "elisp-info-lookup-symbol" anvil-elisp--server-id)
  (anvil-server-unregister-tool
   "elisp-read-source-file" anvil-elisp--server-id))

(provide 'anvil-elisp)
;;; anvil-elisp.el ends here
