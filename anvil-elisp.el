;;; anvil-elisp.el --- Elisp development tools for anvil -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

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
    (source file-path start-line end-line &optional extra-fields)
  "Encode a source location response as JSON.
SOURCE is the source code string.
FILE-PATH is the absolute path to the source file.
START-LINE and END-LINE are 1-based line numbers.
EXTRA-FIELDS, when non-nil, is an alist appended to the response."
  (json-encode
   (append
    `((source . ,source)
      (file-path . ,file-path)
      (start-line . ,start-line)
      (end-line . ,end-line))
    extra-fields)))

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

(defun anvil-elisp--describe-function-portable (sym)
  "Render a portable description for SYM using `documentation' +
`help-function-arglist'.  Returns a plain string compatible with
both Emacs and NeLisp runtimes (no `describe-function-1' usage)."
  (let* ((fn (symbol-function sym))
         (is-alias (symbolp fn))
         (aliased-to (and is-alias fn))
         (is-subr (subrp (if is-alias aliased-to fn)))
         (arglist (condition-case nil
                      (help-function-arglist sym t)
                    (error nil)))
         (doc (or (documentation sym) ""))
         (file (and (fboundp 'find-lisp-object-file-name)
                    (find-lisp-object-file-name sym 'defun)))
         (sig (cond
               (arglist (format "(%s%s)" sym
                                (if arglist
                                    (concat " "
                                            (mapconcat
                                             (lambda (a) (format "%S" a))
                                             arglist " "))
                                  "")))
               (t (format "(%s ...)" sym))))
         (kind (cond
                (is-alias (format "alias for `%s'" aliased-to))
                (is-subr "built-in function")
                (t "Lisp function"))))
    (format "%s\n\n%s%s%s%s"
            sig
            (format "  %s.\n\n" kind)
            (if file (format "Defined in `%s'.\n\n" file) "")
            (if is-alias
                (format "Aliased to `%s'.\n\n" aliased-to)
              "")
            doc)))

(defun anvil-elisp--describe-function (function)
  "Get full documentation for Emacs Lisp FUNCTION.

Uses `documentation' + `help-function-arglist' so the renderer is
portable across Emacs and NeLisp runtimes.  `describe-function-1'
(an Emacs-internal *Help* buffer formatter) is intentionally not
used here.

MCP Parameters:
  function - The name of the function to describe"
  (anvil-server-with-error-handling
   (let ((sym (anvil-elisp--validate-symbol function "function" t)))
     (if (fboundp sym)
         (anvil-elisp--describe-function-portable sym)
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

(defun anvil-elisp--strip-runtime-signature (doc)
  "Return DOC without a trailing runtime-added \"(fn ...)\" signature."
  (if (and (stringp doc)
           (string-match "\\`\\(.+?\\)\\(?:\n\n(fn [^)]+)\\)?\\'" doc))
      (match-string 1 doc)
    doc))

(defun anvil-elisp--get-function-definition-native-no-source
    (fn-name sym)
  "Return a synthetic definition for native-compiled FN-NAME without source.
SYM is the function symbol.  This path is used when runtime metadata
confirms native compilation but there is no recoverable source file."
  (let* ((args (or (help-function-arglist sym t) '(&rest args)))
         (doc (anvil-elisp--strip-runtime-signature
               (or (documentation sym t) "")))
         (message
          (format
           "Original body unavailable: `%s' is native-compiled and no source file is recorded."
           fn-name))
         (defun-form
          `(defun ,sym ,args
             ,@(when (anvil-elisp--non-empty-docstring-p doc)
                 (list doc))
             (error ,message)))
         (source
          (concat
           (format
            ";; Synthetic stub: `%s' is native-compiled and has no recoverable source file.\n"
            fn-name)
           (pp-to-string defun-form)))
         (end-line (max 1 (length (string-lines source)))))
    (anvil-elisp--json-encode-source-location
     source
     "<native-compiled>"
     1
     end-line
     `((source-unavailable . t)
       (reason . "native-compiled-no-source")
       (message . ,message)))))

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
        (aliased-to (nth 2 fn-info))
        ;; Native-compiled Elisp functions also satisfy `subrp', so prefer
        ;; the recorded source file when one exists before classifying a
        ;; function as C-implemented.
        (func-file (find-lisp-object-file-name sym 'defun)))
    (cond
     ;; Has source file
     (func-file
      (anvil-elisp--get-function-definition-from-file
       function
       sym
       func-file
       is-alias
       aliased-to))

     ;; Native-compiled Elisp function without a recoverable source file.
     ((and (fboundp 'native-comp-function-p)
           (native-comp-function-p fn))
      (anvil-elisp--get-function-definition-native-no-source
       function sym))

     ;; C-implemented function
     ((subrp fn)
      (anvil-elisp--get-function-definition-c-function function))

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
;; Doc 38 Phase C — info-lookup-symbol moved to anvil-ide-elisp.el
;; (= depends on Emacs `info-look', not portable to NeLisp runtime).

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

(defun anvil-elisp--truthy-arg (value)
  "Return non-nil when VALUE is a truthy MCP argument."
  (cond
   ((null value) nil)
   ((or (eq value :json-false) (eq value :false)) nil)
   ((stringp value)
    (not (member (string-trim (downcase value))
                 '("" "nil" "false" "0"))))
   (t value)))

(defun anvil-elisp--listify-arg (value)
  "Return VALUE as a list.
Vectors are converted to lists; nil stays nil; scalars become a
single-element list."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun anvil-elisp--ensure-string-list (value key)
  "Normalize VALUE into a list of strings or signal for KEY."
  (let ((items (anvil-elisp--listify-arg value)))
    (unless items
      (signal 'anvil-server-tool-error
              (list (format "%s is required" key))))
    (dolist (item items)
      (unless (stringp item)
        (signal 'anvil-server-tool-error
                (list (format "%s must be a string list" key)))))
    items))

(defun anvil-elisp--repo-root-for-file (file)
  "Return the repo root for FILE, or its directory if no git root exists."
  (let* ((path (expand-file-name file))
         (root (locate-dominating-file path ".git")))
    (expand-file-name (or root (file-name-directory path) default-directory))))

(defun anvil-elisp--expand-path-under-root (path root)
  "Expand PATH under ROOT unless PATH is already absolute."
  (if (file-name-absolute-p path)
      (expand-file-name path)
    (expand-file-name path root)))

(defun anvil-elisp--ert-distilled-command (files load-path-dirs selector)
  "Build an `emacs --batch -Q' command for FILES, LOAD-PATH-DIRS, and SELECTOR."
  (let* ((emacs-bin
          (if (and (stringp invocation-name)
                   (stringp invocation-directory))
              (expand-file-name invocation-name invocation-directory)
            invocation-name))
         (command (list emacs-bin "--batch" "-Q")))
    (dolist (dir load-path-dirs)
      (setq command (append command (list "-L" dir))))
    (setq command (append command (list "-l" "ert")))
    (dolist (file files)
      (setq command (append command (list "-l" file))))
    (if selector
        (append command
                (list "--eval"
                      (format "(ert-run-tests-batch-and-exit %S)"
                              (read selector))))
      (append command (list "-f" "ert-run-tests-batch-and-exit")))))

(defun anvil-elisp--process-shell-command (argv log-path)
  "Return a shell command string that runs ARGV and writes to LOG-PATH."
  (format "exec %s >%s 2>&1"
          (mapconcat #'shell-quote-argument argv " ")
          (shell-quote-argument log-path)))

(defun anvil-elisp--process-exit-code (process)
  "Return PROCESS exit code, using 128+signal for signalled exits."
  (if (eq (process-status process) 'signal)
      (+ 128 (process-exit-status process))
    (process-exit-status process)))

(defun anvil-elisp--read-file-string (path)
  "Return PATH contents as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun anvil-elisp--truncate-tail (text limit &optional from-start)
  "Return TEXT truncated to LIMIT chars.
When FROM-START is non-nil, keep the head; otherwise keep the tail."
  (if (<= (length text) limit)
      text
    (if from-start
        (substring text 0 limit)
      (substring text (- (length text) limit)))))

(defun anvil-elisp--ert-distilled-summary (output)
  "Parse the stable batch ERT summary line from OUTPUT."
  (when (string-match
         (concat
          "Ran \\([0-9]+\\) tests, [0-9]+ results as expected, "
          "\\([0-9]+\\) unexpected"
          "\\(?:, \\([0-9]+\\) skipped\\)?"
          " ([^,]+, \\([0-9.]+\\) sec)")
         output)
    (list :ran (string-to-number (match-string 1 output))
          :unexpected (string-to-number (match-string 2 output))
          :skipped (string-to-number (or (match-string 3 output) "0"))
          :duration-sec (string-to-number (match-string 4 output)))))

(defun anvil-elisp--ert-distilled-parse-failures (output)
  "Parse failing test names, condition heads, and first backtrace from OUTPUT."
  (let ((lines (split-string output "\n"))
        (failures nil)
        (order nil)
        (first-backtrace nil)
        (full-backtrace nil)
        collect-backtrace
        collect-condition
        current-backtrace)
    (dolist (line lines)
      (cond
       ((string-match "^Test \\([^[:space:]]+\\) backtrace:$" line)
        (setq collect-backtrace t
              collect-condition nil
              current-backtrace nil))
       ((and collect-backtrace
             (string-match "^Test \\([^[:space:]]+\\) condition:$" line))
        (let ((name (match-string 1 line))
              (joined (string-join (nreverse current-backtrace) "\n")))
          (setq collect-backtrace nil
                collect-condition name)
          (when (and joined (not (string-empty-p joined))
                     (null first-backtrace))
            (setq first-backtrace
                  (anvil-elisp--truncate-tail joined 600 t)
                  full-backtrace
                  (anvil-elisp--truncate-tail joined 8000 t)))))
       (collect-backtrace
        (push line current-backtrace))
       ((and collect-condition
             (not (string-empty-p (string-trim line))))
        (unless (assoc collect-condition failures)
          (push collect-condition order)
          (push (cons collect-condition (string-trim line)) failures))
        (setq collect-condition nil))
       ((string-match "^   FAILED  [0-9]+/[0-9]+  \\([^[:space:]]+\\)" line)
        (let ((name (match-string 1 line)))
          (unless (assoc name failures)
            (push name order)
            (push (cons name nil) failures))))))
    (list :failed
          (mapcar (lambda (name)
                    (list :name name
                          :error (or (cdr (assoc name failures))
                                     "unknown failure")))
                  (nreverse order))
          :first-backtrace first-backtrace
          :full-backtrace full-backtrace)))

(defun anvil-elisp--ert-run-distilled
    (files &optional load_path root selector timeout_sec full)
  "Run FILES in a fresh batch Emacs and return a fixed-shape digest.

MCP Parameters:
  files       - Required list of ERT test files to load.
  load_path   - Optional list of `-L' directories.  Defaults to the
                repo root inferred from the first file plus its `tests/'
                directory.
  root        - Optional working directory.  Defaults to the repo root
                inferred from the first file.
  selector    - Optional ERT selector string passed to
                `ert-run-tests-batch-and-exit'.
  timeout_sec - Optional timeout in seconds (default 300).
  full        - Optional truthy flag; when set, include the bounded raw
                backtrace section under `:backtrace'.

Returns a printed plist digest rather than the full ERT transcript.
The raw log is always written to `:log-path'.  Timeout or crash returns
`(:exit-code CODE :error STRING :tail STRING :log-path PATH)'."
  (let* ((file-list (anvil-elisp--ensure-string-list files "files"))
         (base-root (anvil-elisp--repo-root-for-file (car file-list)))
         (root-dir (expand-file-name (or root base-root)))
         (expanded-files
          (mapcar (lambda (file)
                    (anvil-elisp--expand-path-under-root file root-dir))
                  file-list))
         (expanded-load-path
          (mapcar (lambda (dir)
                    (anvil-elisp--expand-path-under-root dir root-dir))
                  (or (anvil-elisp--listify-arg load_path)
                      (list root-dir (expand-file-name "tests" root-dir)))))
         (timeout (if timeout_sec
                      (max 1 (truncate (if (numberp timeout_sec)
                                           timeout_sec
                                         (string-to-number timeout_sec))))
                    300))
         (log-path (make-temp-file "anvil-ert-distilled-" nil ".log"))
         (result nil)
         (argv nil)
         (process nil)
         (output "")
         (exit-code 0))
    (condition-case err
        (setq argv (anvil-elisp--ert-distilled-command
                    expanded-files expanded-load-path selector))
      (error
       (setq result
             (list :exit-code 2
                   :error (format "Invalid ert selector: %s"
                                  (error-message-string err))
                   :tail ""
                   :log-path log-path))))
    (unless result
      (let ((default-directory root-dir)
            (deadline (+ (float-time) timeout)))
        (setq process
              (make-process
               :name "anvil-ert-distilled"
               :command
               (list shell-file-name shell-command-switch
                     (anvil-elisp--process-shell-command argv log-path))
               :noquery t))
        (while (and (process-live-p process)
                    (< (float-time) deadline))
          (accept-process-output process 0.1))
        (when (process-live-p process)
          (ignore-errors (kill-process process))
          (while (process-live-p process)
            (accept-process-output process 0.05))
          (setq exit-code 124
                result
                (list :exit-code 124
                      :error (format "ERT subprocess timed out after %s seconds"
                                     timeout)
                      :tail ""
                      :log-path log-path))))
      (setq output (anvil-elisp--read-file-string log-path))
      (when (and result (plist-member result :tail))
        (setf (plist-get result :tail)
              (anvil-elisp--truncate-tail output 1000)))
      (unless result
        (setq exit-code (anvil-elisp--process-exit-code process))
        (let ((summary (anvil-elisp--ert-distilled-summary output)))
          (if (null summary)
              (setq result
                    (list :exit-code exit-code
                          :error (format "ERT subprocess failed without a parsable summary (exit %s)"
                                         exit-code)
                          :tail (anvil-elisp--truncate-tail output 1000)
                          :log-path log-path))
            (let ((failure-data
                   (anvil-elisp--ert-distilled-parse-failures output)))
              (setq result
                    (list :ran (plist-get summary :ran)
                          :unexpected (plist-get summary :unexpected)
                          :skipped (plist-get summary :skipped)
                          :duration-sec (plist-get summary :duration-sec)
                          :exit-code exit-code
                          :log-path log-path))
              (when (> (plist-get summary :unexpected) 0)
                (setq result
                      (append result
                              (list :failed (plist-get failure-data :failed)
                                    :first-backtrace
                                    (plist-get failure-data :first-backtrace))))
                (when (anvil-elisp--truthy-arg full)
                  (setq result
                        (append result
                                (list :backtrace
                                      (plist-get failure-data :full-backtrace)))))))))))
    (format "%S" result)))

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
         (path-dir (file-name-directory path))
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
    (let ((load-path (if (and path-dir
                              (not (member path-dir load-path)))
                         (cons path-dir load-path)
                       load-path)))
      (load path nil t))
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

(defun anvil-elisp--byte-compile-file--nelisp (path)
  "Compile every top-level lambda in PATH via nelisp-cc.
Reads top-level forms from PATH (must already exist on disk) and
feeds each `(defun ...)' / `(defmacro ...)'  body as a lambda
form into `nelisp-cc-runtime-compile-and-allocate'.  Returns a
plist of (:ok BOOL :output PATH :warnings (...) :errors (...)).

Top-level forms that are not function definitions are skipped
(this matches `byte-compile-file' which compiles them as part of
the load-time stream but does not produce per-form artefacts that
nelisp-cc cares about).  Errors raised by the nelisp-cc pipeline
are captured into the :errors list rather than bubbled up.

This delegate path is only taken when
`nelisp-cc-runtime-compile-and-allocate' is `fboundp'; otherwise
the caller falls back to the Emacs `byte-compile-file' renderer."
  (let ((errors nil)
        (warnings nil)
        (compiled 0)
        (forms nil))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (let ((sexp t))
            (while sexp
              (setq sexp (condition-case _
                             (read (current-buffer))
                           (end-of-file nil)
                           (error nil)))
              (when (and (consp sexp)
                         (memq (car sexp) '(defun defmacro)))
                (push sexp forms)))))
      (error (push (error-message-string err) errors)))
    (dolist (form (nreverse forms))
      ;; (defun NAME ARGS [DOC] BODY...) -> (lambda ARGS [DOC] BODY...)
      (let ((lambda-form `(lambda ,@(cddr form))))
        (condition-case err
            (progn
              (funcall (intern "nelisp-cc-runtime-compile-and-allocate")
                       lambda-form)
              (cl-incf compiled))
          (error
           (push (format "%s: %s"
                         (cadr form) (error-message-string err))
                 warnings)))))
    (format "%S" (list :ok (null errors)
                       :output (concat (file-name-sans-extension path)
                                       ".elc")
                       :backend 'nelisp-cc
                       :compiled-forms compiled
                       :warnings (nreverse warnings)
                       :errors (nreverse errors)))))

(defun anvil-elisp--byte-compile-file (file)
  "Byte-compile FILE and return a compact result plist.

When `nelisp-cc-runtime-compile-and-allocate' is bound (NeLisp
runtime), each top-level `defun' / `defmacro' is fed through that
form-based JIT; the response is annotated `:backend nelisp-cc'.
Otherwise the Emacs `byte-compile-file' path runs as before and
the `:backend' key is omitted.  Both paths return the same shape:

MCP Parameters:
  file - Path to an .el file to byte-compile (string).

Returns a printed plist:
  (:ok BOOL :output PATH :warnings (...) :errors (...))
Warnings and errors are parsed out of the byte-compile log so the
caller does not have to scan it."
  (let ((path (expand-file-name file)))
    (if (fboundp 'nelisp-cc-runtime-compile-and-allocate)
        (anvil-elisp--byte-compile-file--nelisp path)
      (let* ((log-buf (get-buffer-create " *anvil-bc-log*"))
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
          (while (not (eobp))
            (let ((line-text (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position))))
              (cond
               ((string-match
                 "^\\(?:.*?\\):\\([0-9]+\\):\\([0-9]+\\):\\s-*\\(Warning\\|Error\\):\\s-*\\(.*\\)$"
                 line-text)
                (let ((kind (match-string 3 line-text))
                      (msg  (match-string 4 line-text)))
                  (if (equal kind "Warning")
                      (push msg warnings)
                    (push msg errors))))
               ((string-match
                 "^\\(?:.*?\\):\\s-*\\(Warning\\|Error\\):\\s-*\\(.*\\)$"
                 line-text)
                (let ((kind (match-string 1 line-text))
                      (msg  (match-string 2 line-text)))
                  (if (equal kind "Warning")
                      (push msg warnings)
                    (push msg errors)))))
              (forward-line 1))))
        (format "%S" (list :ok (and result (null errors))
                           :output (concat (file-name-sans-extension path)
                                           ".elc")
                           :warnings (nreverse warnings)
                           :errors (nreverse errors)))))))

;;;###autoload
(defun anvil-elisp-enable ()
  "Enable the Elisp development MCP tools."
  (anvil-server-register-tool
   #'anvil-elisp--ert-run
   :id "elisp-ert-run"
   :intent '(elisp-test)
   :layer 'dev
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
   #'anvil-elisp--ert-run-distilled
   :id "ert-run-distilled"
   :intent '(elisp-test)
   :layer 'dev
   :server-id anvil-elisp--server-id
   :description
   "Run batch ERT in a fresh `emacs --batch -Q' subprocess and return
a fixed-shape digest:
\(:ran N :unexpected N :skipped N :duration-sec F :exit-code N
 :log-path PATH ...).  Unexpected runs add failing test names plus the
first backtrace head; `full' truthy includes the bounded raw backtrace
section too.  Timeouts and crashes return only `:exit-code', `:error',
`:tail', and `:log-path', so the response stays small even for large
suites."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-elisp--byte-compile-file
   :id "elisp-byte-compile-file"
   :intent '(elisp-build)
   :layer 'dev
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
   :intent '(elisp-read)
   :layer 'core
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
   :intent '(elisp-read)
   :layer 'core
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
   :intent '(elisp-read)
   :layer 'core
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
  ;; Doc 38 Phase C — `elisp-info-lookup-symbol' lives in
  ;; `anvil-ide-elisp.el' (= IDE-only, depends on Emacs info-look).
  (anvil-server-register-tool
   #'anvil-elisp--read-source-file
   :id "elisp-read-source-file"
   :intent '(elisp-read file-read)
   :layer 'core
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
   "ert-run-distilled" anvil-elisp--server-id)
  (anvil-server-unregister-tool
   "elisp-byte-compile-file" anvil-elisp--server-id)
  (anvil-server-unregister-tool
   "elisp-describe-function" anvil-elisp--server-id)
  (anvil-server-unregister-tool
   "elisp-get-function-definition" anvil-elisp--server-id)
  (anvil-server-unregister-tool
   "elisp-describe-variable" anvil-elisp--server-id)
  ;; Doc 38 Phase C — `elisp-info-lookup-symbol' is unregistered by
  ;; `anvil-ide-elisp-disable'.
  (anvil-server-unregister-tool
   "elisp-read-source-file" anvil-elisp--server-id))

(provide 'anvil-elisp)
;;; anvil-elisp.el ends here
