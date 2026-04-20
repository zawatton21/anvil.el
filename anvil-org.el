;;; anvil-org.el --- Anvil org-mode MCP tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Author: Laurynas Biveinis <laurynas.biveinis@gmail.com>
;; Keywords: convenience, files, matching, outlines
;; Version: 0.9.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/zawatton/anvil.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements a Model Context Protocol (MCP) server for
;; Org-mode.

;;; Code:

(require 'cl-lib)
(require 'anvil-server)
(require 'org)
(require 'org-id)
(require 'url-util)

(defcustom anvil-org-allowed-files nil
  "List of absolute paths to Org files that can be accessed via MCP."
  :type '(repeat file)
  :group 'anvil-org)

(defcustom anvil-org-use-index t
  "When non-nil, `org-read-*' tools try `anvil-org-index' first.
A hit returns straight from the SQLite-backed index (ms-scale).
On a miss — index not loaded, entry absent, path ambiguous, or
any error inside the fast-path — the handler falls back to the
existing org-element-based implementation.  Set to nil to
disable the fast-path entirely (e.g. while comparing behaviour)."
  :type 'boolean
  :group 'anvil-org)

(defcustom anvil-org-allowed-files-enabled t
  "When non-nil, restrict file access to `anvil-org-allowed-files'.
Set to nil to allow access to any file."
  :type 'boolean
  :group 'anvil-org)

(defconst anvil-org--server-id "emacs-eval"
  "Server ID for MCP server registration.
Historically named anvil-org, renamed to emacs-eval to reflect
its role as a general-purpose Emacs evaluation server.")

(defconst anvil-org--uri-headline-prefix "org-headline://"
  "URI prefix for headline resources.")

(defconst anvil-org--uri-id-prefix "org-id://"
  "URI prefix for ID-based resources.")

(defun anvil-org--extract-uri-suffix (uri prefix)
  "Extract suffix from URI after PREFIX.
Returns the suffix string if URI starts with PREFIX, nil otherwise."
  (when (string-prefix-p prefix uri)
    (substring uri (length prefix))))

;; Error handling helpers

(defun anvil-org--headline-not-found-error (headline-path)
  "Throw error for HEADLINE-PATH not found."
  (anvil-server-tool-throw
   (format "Cannot find headline: %s"
           (mapconcat #'identity headline-path "/"))))

(defun anvil-org--id-not-found-error (id)
  "Throw error for ID not found."
  (anvil-server-tool-throw (format "Cannot find ID '%s'" id)))

(defun anvil-org--tool-validation-error (message &rest args)
  "Throw validation error MESSAGE with ARGS for tool operations."
  (anvil-server-tool-throw (apply #'format message args)))

(defun anvil-org--resource-validation-error (message &rest args)
  "Signal validation error MESSAGE with ARGS for resource operations."
  (anvil-server-resource-signal-error
   anvil-server-jsonrpc-error-invalid-params
   (apply #'format message args)))

(defun anvil-org--state-mismatch-error (expected found context)
  "Throw state mismatch error.
EXPECTED is the expected value, FOUND is the actual value,
CONTEXT describes what is being compared."
  (anvil-server-tool-throw
   (format "%s mismatch: expected '%s', found '%s'"
           context expected found)))

(defun anvil-org--resource-not-found-error (resource-type identifier)
  "Signal resource not found error.
RESOURCE-TYPE is the type of resource,
IDENTIFIER is the resource identifier."
  (anvil-server-resource-signal-error
   anvil-server-jsonrpc-error-invalid-params
   (format "Cannot find %s: '%s'" resource-type identifier)))

(defun anvil-org--tool-file-access-error (locator)
  "Throw file access error for tool operations.
LOCATOR is the resource identifier (file path or ID) that was
denied access."
  (anvil-server-tool-throw
   (format "'%s': the referenced file not in allowed list" locator)))

(defun anvil-org--resource-file-access-error (locator)
  "Signal file access error for resource operations.
LOCATOR is the resource identifier (file path or ID) that was
denied access."
  (anvil-server-resource-signal-error
   anvil-server-jsonrpc-error-invalid-params
   (format "'%s': the referenced file not in allowed list" locator)))

;; Helpers

(defun anvil-org--read-file (file-path)
  "Read and return the contents of FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun anvil-org--paths-equal-p (path1 path2)
  "Return t if PATH1 and PATH2 refer to the same file.
Handles symlinks and path variations by normalizing both paths."
  (string= (file-truename path1) (file-truename path2)))

(defun anvil-org--find-allowed-file (filename)
  "Find FILENAME in `anvil-org-allowed-files'.
Returns the expanded path if found, nil if not in the allowed list.
When `anvil-org-allowed-files-enabled' is nil, always returns the expanded path."
  (if (not anvil-org-allowed-files-enabled)
      (expand-file-name filename)
    (when-let* ((found
                 (cl-find
                  (file-truename filename)
                  anvil-org-allowed-files
                  :test #'anvil-org--paths-equal-p)))
      (expand-file-name found))))

(defun anvil-org--refresh-file-buffers (file-path)
  "Refresh all buffers visiting FILE-PATH.
Preserves narrowing state across the refresh operation."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when-let* ((buf-file (buffer-file-name)))
        (when (string= buf-file file-path)
          (let ((was-narrowed (buffer-narrowed-p))
                (narrow-start nil)
                (narrow-end nil))
            ;; Save narrowing markers if narrowed
            (when was-narrowed
              (setq narrow-start (point-min-marker))
              (setq narrow-end (point-max-marker)))
            (condition-case err
                (unwind-protect
                    (progn
                      (revert-buffer t t t)
                      ;; Check if buffer was modified by hooks
                      (when (buffer-modified-p)
                        (anvil-org--tool-validation-error
                         "Buffer for file %s was modified during \
refresh.  Check your `after-revert-hook' for functions that modify \
the buffer"
                         file-path)))
                  ;; Restore narrowing even if revert fails
                  (when was-narrowed
                    (narrow-to-region narrow-start narrow-end)))
              (error
               (anvil-org--tool-validation-error
                "Failed to refresh buffer for file %s: %s. \
Check your Emacs hooks (`before-revert-hook', \
`after-revert-hook', `revert-buffer-function')"
                file-path (error-message-string err))))))))))

(defun anvil-org--complete-and-save (file-path response-alist)
  "Create ID if needed, save FILE-PATH, return JSON.
Creates or gets an Org ID for the current headline and returns it.
FILE-PATH is the path to save the buffer contents to.
RESPONSE-ALIST is an alist of response fields."
  (let ((id (org-id-get-create)))
    (write-region (point-min) (point-max) file-path)
    (anvil-org--refresh-file-buffers file-path)
    (json-encode
     (append
      `((success . t))
      response-alist
      `((uri . ,(concat anvil-org--uri-id-prefix id)))))))

(defun anvil-org--fail-if-modified (file-path operation)
  "Check if FILE-PATH has unsaved change in any buffer.
OPERATION is a string describing the operation for error messages."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (string= (buffer-file-name) file-path)
                 (buffer-modified-p))
        (anvil-org--tool-validation-error
         "Cannot %s: file has unsaved changes in buffer"
         operation)))))

(defmacro anvil-org--with-org-file (file-path &rest body)
  "Execute BODY in a temp Org buffer with file at FILE-PATH."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert-file-contents ,file-path)
     (org-mode)
     (goto-char (point-min))
     ,@body))

(defmacro anvil-org--modify-and-save
    (file-path operation response-alist &rest body)
  "Execute BODY to modify Org file at FILE-PATH, then save result.
First validates that FILE-PATH has no unsaved changes (using
OPERATION for error messages).  Then executes BODY in a temp buffer
set up for the Org file.  After BODY executes, creates an Org ID if
needed, saves the buffer, refreshes any visiting buffers, and
returns the result of `anvil-org--complete-and-save' with FILE-PATH
and RESPONSE-ALIST.
BODY can access FILE-PATH, OPERATION, and RESPONSE-ALIST as
variables."
  (declare (indent 3) (debug (form form form body)))
  `(progn
     (anvil-org--fail-if-modified ,file-path ,operation)
     (with-temp-buffer
       (set-visited-file-name ,file-path t)
       (insert-file-contents ,file-path)
       (org-mode)
       (goto-char (point-min))
       ,@body
       (anvil-org--complete-and-save ,file-path ,response-alist))))

(defun anvil-org--find-allowed-file-with-id (id)
  "Find an allowed file containing the Org ID.
First looks up in the org-id database, then validates the file is in
the allowed list.
Returns the expanded file path if found and allowed.
Throws a tool error if ID exists but file is not allowed, or if ID
is not found."
  (if-let* ((id-file (org-id-find-id-file id)))
    ;; ID found in database, check if file is allowed
    (if-let* ((allowed-file (anvil-org--find-allowed-file id-file)))
      allowed-file
      (anvil-org--tool-file-access-error id))
    ;; ID not in database - might not exist or DB is stale
    ;; Fall back to searching allowed files manually (only when restriction is on)
    (if (not anvil-org-allowed-files-enabled)
        (anvil-org--id-not-found-error id)
      (let ((found-file nil))
        (dolist (allowed-file anvil-org-allowed-files)
          (unless found-file
            (when (file-exists-p allowed-file)
              (anvil-org--with-org-file allowed-file
                (when (org-find-property "ID" id)
                  (setq found-file (expand-file-name allowed-file)))))))
        (or found-file (anvil-org--id-not-found-error id))))))

(defmacro anvil-org--with-uri-prefix-dispatch
    (uri headline-body id-body)
  "Dispatch tool URI handling based on prefix.
URI is the URI string to dispatch on.
HEADLINE-BODY is executed when URI starts with
`anvil-org--uri-headline-prefix', with the URI after the prefix bound
to `headline'.
ID-BODY is executed when URI starts with `anvil-org--uri-id-prefix',
with the URI after the prefix bound to `id'.
Throws an error if neither prefix matches."
  (declare (indent 1))
  `(if-let* ((id
              (anvil-org--extract-uri-suffix
               ,uri anvil-org--uri-id-prefix)))
     ,id-body
     (if-let* ((headline
                (anvil-org--extract-uri-suffix
                 ,uri anvil-org--uri-headline-prefix)))
       ,headline-body
       (anvil-org--tool-validation-error
        "Invalid resource URI format: %s"
        ,uri))))

(defun anvil-org--validate-file-access (filename)
  "Validate that FILENAME is in the allowed list.
FILENAME must be an absolute path.
Returns the full path if allowed, signals an error otherwise."
  (unless (file-name-absolute-p filename)
    (anvil-org--resource-validation-error "Path must be absolute: %s"
                                        filename))
  (let ((allowed-file (anvil-org--find-allowed-file filename)))
    (unless allowed-file
      (anvil-org--resource-file-access-error filename))
    allowed-file))

(defun anvil-org--extract-children (target-level)
  "Extract children at TARGET-LEVEL until next lower level heading."
  (let ((children '()))
    (save-excursion
      (while (and (re-search-forward "^\\*+ " nil t)
                  (>= (org-current-level) target-level))
        (when (= (org-current-level) target-level)
          (let* ((title (org-get-heading t t t t))
                 (child
                  `((title . ,title)
                    (level . ,target-level)
                    (children . []))))
            (push child children)))))
    (vconcat (nreverse children))))

(defun anvil-org--extract-headings ()
  "Extract heading structure from current org buffer."
  (let ((result '()))
    (goto-char (point-min))
    (while (re-search-forward "^\\* " nil t) ; Find level 1 headings
      (let* ((title (org-get-heading t t t t))
             ;; Get level 2 children
             (children (anvil-org--extract-children 2))
             (heading
              `((title . ,title) (level . 1) (children . ,children))))
        (push heading result)))
    (vconcat (nreverse result))))

(defun anvil-org--generate-outline (file-path)
  "Generate JSON outline structure for FILE-PATH."
  (anvil-org--with-org-file file-path
    (let ((headings (anvil-org--extract-headings)))
      `((headings . ,headings)))))

(defun anvil-org--decode-file-path (encoded-path)
  "Decode special characters from ENCODED-PATH.
Specifically decodes %23 back to #."
  (replace-regexp-in-string "%23" "#" encoded-path))

(defun anvil-org--split-headline-uri (path-after-protocol)
  "Split PATH-AFTER-PROTOCOL into (file-path . headline-path).
PATH-AFTER-PROTOCOL is the part after `org-headline://'.
Returns (FILE . HEADLINE) where FILE is the decoded file path and
HEADLINE is the part after the fragment separator.
File paths with # characters should be encoded as %23."
  (if-let* ((hash-pos (string-match "#" path-after-protocol)))
    (cons
     (anvil-org--decode-file-path
      (substring path-after-protocol 0 hash-pos))
     (substring path-after-protocol (1+ hash-pos)))
    (cons (anvil-org--decode-file-path path-after-protocol) nil)))

(defun anvil-org--parse-resource-uri (uri)
  "Parse URI and return (file-path . headline-path).
Validates file access and returns expanded file path."
  (let (file-path
        headline-path)
    (anvil-org--with-uri-prefix-dispatch
        uri
      ;; Handle org-headline:// URIs
      (let* ((split-result (anvil-org--split-headline-uri headline))
             (filename (car split-result))
             (headline-path-str (cdr split-result))
             (allowed-file (anvil-org--validate-file-access filename)))
        (setq file-path (expand-file-name allowed-file))
        (setq headline-path
              (when headline-path-str
                (mapcar
                 #'url-unhex-string
                 (split-string headline-path-str "/")))))
      ;; Handle org-id:// URIs
      (progn
        (setq file-path (anvil-org--find-allowed-file-with-id id))
        (setq headline-path (list id))))
    (cons file-path headline-path)))

(defun anvil-org--navigate-to-headline (headline-path)
  "Navigate to headline in HEADLINE-PATH.
HEADLINE-PATH is a list of headline titles forming a path.
Returns t if found, nil otherwise.  Point is left at the headline."
  (catch 'not-found
    (let ((search-start (point-min))
          (search-end (point-max))
          (current-level 0)
          (found nil)
          (path-index 0))
      (dolist (target-title headline-path)
        (setq found nil)
        (goto-char search-start)
        (while (and (not found)
                    (re-search-forward "^\\*+ " search-end t))
          (let ((title (org-get-heading t t t t))
                (level (org-current-level)))
            (when (and (string= title target-title)
                       (or (= current-level 0)
                           (= level (1+ current-level))))
              (setq found t)
              (setq current-level level)
              ;; Limit search to this subtree for nesting
              (when (< (1+ path-index) (length headline-path))
                (setq search-start (point))
                (setq search-end
                      (save-excursion
                        (org-end-of-subtree t t)
                        (point)))))))
        (unless found
          (throw 'not-found nil))
        (setq path-index (1+ path-index))))
    t))

(defun anvil-org--extract-headline-content ()
  "Extract content of current headline including the headline itself.
Point should be at the headline."
  (let ((start (line-beginning-position)))
    (org-end-of-subtree t t)
    ;; Remove trailing newline if present
    (when (and (> (point) start) (= (char-before) ?\n))
      (backward-char))
    (buffer-substring-no-properties start (point))))

(defun anvil-org--get-headline-content (file-path headline-path)
  "Get content for headline at HEADLINE-PATH in FILE-PATH.
HEADLINE-PATH is a list of headline titles to traverse.
Returns the content string or nil if not found."
  (anvil-org--with-org-file file-path
    (when (anvil-org--navigate-to-headline headline-path)
      (anvil-org--extract-headline-content))))

(defun anvil-org--goto-headline-from-uri (headline-path is-id)
  "Navigate to headline based on HEADLINE-PATH and IS-ID flag.
If IS-ID is non-nil, treats HEADLINE-PATH as containing an ID.
Otherwise, navigates using HEADLINE-PATH as title hierarchy."
  (if is-id
      ;; ID case - headline-path contains single ID
      (if-let* ((pos (org-find-property "ID" (car headline-path))))
        (goto-char pos)
        (anvil-org--id-not-found-error (car headline-path)))
    ;; Path case - headline-path contains title hierarchy
    (unless (anvil-org--navigate-to-headline headline-path)
      (anvil-org--headline-not-found-error headline-path))))

(defun anvil-org--get-content-by-id (file-path id)
  "Get content for org node with ID in FILE-PATH.
Returns the content string or nil if not found."
  (anvil-org--with-org-file file-path
    (when-let* ((pos (org-find-property "ID" id)))
      (goto-char pos)
      (anvil-org--extract-headline-content))))

(defun anvil-org--validate-todo-state (state)
  "Validate STATE is a valid TODO keyword."
  (let ((valid-states
         (delete
          "|"
          (org-remove-keyword-keys
           (apply #'append (mapcar #'cdr org-todo-keywords))))))
    (unless (member state valid-states)
      (anvil-org--tool-validation-error
       "Invalid TODO state: '%s' - valid states: %s"
       state (mapconcat #'identity valid-states ", ")))))

(defun anvil-org--validate-and-normalize-tags (tags)
  "Validate and normalize TAGS.
TAGS can be a single tag string or list of tag strings.
Returns normalized tag list.
Validates:
- Tag names follow Org rules (alphanumeric, underscore, at-sign)
- Tags are in configured tag alist (if configured)
- Tags don't violate mutual exclusivity groups
Signals error for invalid tags."
  (let ((tag-list (anvil-org--normalize-tags-to-list tags))
        (allowed-tags
         (append
          (mapcar
           #'anvil-org--extract-tag-from-alist-entry org-tag-alist)
          (mapcar
           #'anvil-org--extract-tag-from-alist-entry
           org-tag-persistent-alist))))
    ;; Remove special keywords like :startgroup
    (setq allowed-tags
          (cl-remove-if
           #'anvil-org--is-tag-group-keyword-p allowed-tags))
    ;; If tag alists are configured, validate against them
    (when allowed-tags
      (dolist (tag tag-list)
        (unless (member tag allowed-tags)
          (anvil-org--tool-validation-error
           "Tag not in configured tag alist: %s"
           tag))))
    ;; Always validate tag names follow Org's rules
    (dolist (tag tag-list)
      (unless (string-match "^[[:alnum:]_@]+$" tag)
        (anvil-org--tool-validation-error
         "Invalid tag name (must be alphanumeric, _, or @): %s"
         tag)))
    ;; Validate mutual exclusivity if tag-alist is configured
    (when org-tag-alist
      (anvil-org--validate-mutex-tag-groups tag-list org-tag-alist))
    (when org-tag-persistent-alist
      (anvil-org--validate-mutex-tag-groups
       tag-list org-tag-persistent-alist))
    tag-list))

(defun anvil-org--extract-tag-from-alist-entry (entry)
  "Extract tag name from an `org-tag-alist' ENTRY.
ENTRY can be a string or a cons cell (tag . key)."
  (if (consp entry)
      (car entry)
    entry))

(defun anvil-org--is-tag-group-keyword-p (tag)
  "Check if symbol TAG is a special keyword like :startgroup."
  (and (symbolp tag) (string-match "^:" (symbol-name tag))))

(defun anvil-org--parse-mutex-tag-groups (tag-alist)
  "Parse mutually exclusive tag groups from TAG-ALIST.
Returns a list of lists, where each inner list contains tags
that are mutually exclusive with each other."
  (let ((groups '())
        (current-group nil)
        (in-group nil))
    (dolist (entry tag-alist)
      (cond
       ;; Start of a mutex group
       ((eq entry :startgroup)
        (setq in-group t)
        (setq current-group '()))
       ;; End of a mutex group
       ((eq entry :endgroup)
        (when (and in-group current-group)
          (push current-group groups))
        (setq in-group nil)
        (setq current-group nil))
       ;; Inside a group - collect tags
       (in-group
        (let ((tag (anvil-org--extract-tag-from-alist-entry entry)))
          (when (and tag (not (anvil-org--is-tag-group-keyword-p tag)))
            (push tag current-group))))))
    groups))

(defun anvil-org--validate-mutex-tag-groups (tags tag-alist)
  "Validate that TAGS don't violate mutex groups in TAG-ALIST.
TAGS is a list of tag strings.
Errors if multiple tags from same mutex group."
  (let ((mutex-groups (anvil-org--parse-mutex-tag-groups tag-alist)))
    (dolist (group mutex-groups)
      (let ((tags-in-group
             (cl-intersection tags group :test #'string=)))
        (when (> (length tags-in-group) 1)
          (anvil-org--tool-validation-error
           "Tags %s are mutually exclusive (cannot use together)"
           (mapconcat (lambda (tag) (format "'%s'" tag)) tags-in-group
                      ", ")))))))

(defun anvil-org--validate-headline-title (title)
  "Validate that TITLE is not empty or whitespace-only.
Throws an MCP tool error if validation fails."
  (when (or (string-empty-p title)
            (string-match-p "^[[:space:]]*$" title)
            ;; Explicitly match NBSP for Emacs 27.2 compatibility
            ;; In Emacs 27.2, [[:space:]] doesn't match NBSP (U+00A0)
            (string-match-p "^[\u00A0]*$" title))
    (anvil-org--tool-validation-error
     "Headline title cannot be empty or contain only whitespace"))
  (when (string-match-p "[\n\r]" title)
    (anvil-org--tool-validation-error
     "Headline title cannot contain newlines")))

(defun anvil-org--validate-body-no-headlines (body level)
  "Validate that BODY doesn't contain headlines at LEVEL or higher.
LEVEL is the Org outline level (1 for *, 2 for **, etc).
Throws an MCP tool error if invalid headlines are found."
  ;; Build regex to match headlines at the current level or higher
  ;; For level 3, this matches ^*, ^**, or ^***
  ;; Matches asterisks + space/tab (headlines need content)
  (let ((regex (format "^\\*\\{1,%d\\}[ \t]" level)))
    (when (string-match regex body)
      (anvil-org--tool-validation-error
       "Body cannot contain headlines at level %d or higher"
       level))))

(defun anvil-org--validate-body-no-unbalanced-blocks (body)
  "Validate that BODY doesn't contain unbalanced blocks.
Uses a state machine: tracks if we're in a block, and which one.
Text inside blocks is literal and doesn't start/end other blocks.
Throws an MCP tool error if unbalanced blocks are found."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (let
        ((current-block nil)) ; Current block type or nil
      ;; Scan forward for all block markers
      ;; Block names can be any non-whitespace chars
      (while (re-search-forward
              "^#\\+\\(BEGIN\\|END\\|begin\\|end\\)_\\(\\S-+\\)"
              nil t)
        (let ((marker-type (upcase (match-string 1)))
              (block-type (upcase (match-string 2))))
          (cond
           ;; Found BEGIN
           ((string= marker-type "BEGIN")
            (if current-block
                ;; Already in block - BEGIN is literal
                nil
              ;; Not in a block - enter this block
              (setq current-block block-type)))
           ;; Found END
           ((string= marker-type "END")
            (cond
             ;; Not in any block - this END is orphaned
             ((null current-block)
              (anvil-org--tool-validation-error
               "Orphaned END_%s without BEGIN_%s"
               block-type block-type))
             ;; In matching block - exit the block
             ((string= current-block block-type)
              (setq current-block nil))
             ;; In different block - this END is just literal text
             (t
              nil))))))
      ;; After scanning, check if we're still in a block
      (when current-block
        (anvil-org--tool-validation-error
         "Body contains unclosed %s block"
         current-block)))))

(defun anvil-org--normalize-tags-to-list (tags)
  "Normalize TAGS parameter to a list format.
TAGS can be:
- nil or empty list -> returns nil
- vector (JSON array) -> converts to list
- string -> wraps in list
- list -> returns as-is
Throws error for invalid types."
  (cond
   ((null tags)
    nil) ; No tags (nil or empty list)
   ((vectorp tags)
    (append tags nil)) ; Convert JSON array (vector) to list
   ((listp tags)
    tags) ; Already a list
   ((stringp tags)
    (list tags)) ; Single tag string
   (t
    (anvil-org--tool-validation-error "Invalid tags format: %s" tags))))

(defun anvil-org--navigate-to-parent-or-top (parent-path parent-id)
  "Navigate to parent headline or top of file.
PARENT-PATH is a list of headline titles (or nil for top-level).
PARENT-ID is an ID string (or nil).
Returns parent level (integer) if parent exists, nil for top-level.
Assumes point is in an Org buffer."
  (if (or parent-path parent-id)
      (progn
        (anvil-org--goto-headline-from-uri
         (or (and parent-id (list parent-id)) parent-path) parent-id)
        ;; Save parent level before moving point
        ;; Ensure we're at the beginning of headline
        (org-back-to-heading t)
        (org-current-level))
    ;; No parent specified - top level
    ;; Skip past any header comments (#+TITLE, #+AUTHOR, etc.)
    (while (and (not (eobp)) (looking-at "^#\\+"))
      (forward-line))
    ;; Position correctly: if blank line after headers,
    ;; skip it; if headline immediately after, stay
    (when (and (not (eobp)) (looking-at "^[ \t]*$"))
      ;; On blank line after headers, skip
      (while (and (not (eobp)) (looking-at "^[ \t]*$"))
        (forward-line)))
    nil))

(defun anvil-org--position-for-new-child (after-uri parent-end)
  "Position point for inserting a new child under current heading.
AFTER-URI is an optional org-id:// URI of a sibling to insert after.
PARENT-END is the end position of the parent's subtree.
Assumes point is at parent heading.
If AFTER-URI is non-nil, positions after that sibling.
If nil, positions at end of parent's subtree.
Throws validation error if AFTER-URI is invalid or sibling not found."
  (if (and after-uri (not (string-empty-p after-uri)))
      (progn
        ;; Parse afterUri to get the ID
        (let ((after-id
               (anvil-org--extract-uri-suffix
                after-uri anvil-org--uri-id-prefix))
              (found nil))
          (unless after-id
            (anvil-org--tool-validation-error
             "Field after_uri is not %s: %s"
             anvil-org--uri-id-prefix after-uri))
          ;; Find the sibling with the specified ID
          (org-back-to-heading t) ;; At parent
          ;; Search sibling in parent's subtree
          ;; Move to first child
          (if (org-goto-first-child)
              (progn
                ;; Now search among siblings
                (while (and (not found) (< (point) parent-end))
                  (let ((current-id (org-entry-get nil "ID")))
                    (when (string= current-id after-id)
                      (setq found t)
                      ;; Move to sibling end
                      (org-end-of-subtree t t)))
                  (unless found
                    ;; Move to next sibling
                    (unless (org-get-next-sibling)
                      ;; No more siblings
                      (goto-char parent-end)))))
            ;; No children
            (goto-char parent-end))
          (unless found
            (anvil-org--tool-validation-error
             "Sibling with ID %s not found under parent"
             after-id))))
    ;; No after_uri - insert at end of parent's subtree
    (org-end-of-subtree t t)
    ;; If we're at the start of a sibling, go back one char
    ;; to be at the end of parent's content
    (when (looking-at "^\\*+ ")
      (backward-char 1))))

(defun anvil-org--ensure-newline ()
  "Ensure there is a newline or buffer start before point."
  (unless (or (bobp) (looking-back "\n" 1))
    (insert "\n")))

(defun anvil-org--insert-heading (title parent-level)
  "Insert a new Org heading at the appropriate level.
TITLE is the headline text to insert.
PARENT-LEVEL is the parent's heading level (integer) if inserting
as a child, or nil if inserting at top-level.
Assumes point is positioned where the heading should be inserted.
After insertion, point is left on the heading line at end-of-line."
  (if parent-level
      ;; We're inside a parent
      (progn
        (anvil-org--ensure-newline)
        ;; Insert heading manually at parent level + 1
        ;; We don't use `org-insert-heading' because when parent has
        ;; no children, it creates a sibling of the parent instead of
        ;; a child
        (let ((heading-start (point)))
          (insert (make-string (1+ parent-level) ?*) " " title "\n")
          ;; Set point to heading for `org-todo' and `org-set-tags'
          (goto-char heading-start)
          (end-of-line)))
    ;; Top-level heading
    ;; Check if there are no headlines yet (empty buffer or only
    ;; headers before us)
    (let ((has-headline
           (save-excursion
             (goto-char (point-min))
             (re-search-forward "^\\*+ " nil t))))
      (if (not has-headline)
          (progn
            (anvil-org--ensure-newline)
            (insert "* "))
        ;; Has headlines - use `org-insert-heading'
        ;; Ensure proper spacing before inserting
        (anvil-org--ensure-newline)
        (org-insert-heading nil nil t))
      (insert title))))

(defun anvil-org--replace-body-content
    (old-body new-body body-content replace-all body-begin body-end)
  "Replace body content in the current buffer.
OLD-BODY is the substring to replace.
NEW-BODY is the replacement text.
BODY-CONTENT is the current body content string.
REPLACE-ALL if non-nil, replace all occurrences.
BODY-BEGIN is the buffer position where body starts.
BODY-END is the buffer position where body ends."
  (let ((new-body-content
         (cond
          ;; Special case: empty oldBody with empty body
          ((and (string= old-body "")
                (string-match-p "\\`[[:space:]]*\\'" body-content))
           new-body)
          ;; Normal replacement with replaceAll
          (replace-all
           (replace-regexp-in-string
            (regexp-quote old-body) new-body body-content
            t t))
          ;; Normal single replacement
          (t
           (let ((pos
                  (string-match
                   (regexp-quote old-body) body-content)))
             (if pos
                 (concat
                  (substring body-content 0 pos)
                  new-body
                  (substring body-content (+ pos (length old-body))))
               body-content))))))

    ;; Replace the body content
    (if (< body-begin body-end)
        (delete-region body-begin body-end)
      ;; Empty body - ensure we're at the right position
      (goto-char body-begin))
    (insert new-body-content)))

;; Tool handlers

(defun anvil-org--tool-get-todo-config ()
  "Return the TODO keyword configuration."
  (let ((seq-list '())
        (sem-list '()))
    (dolist (seq org-todo-keywords)
      (let* ((type (car seq))
             (keywords (cdr seq))
             (type-str (symbol-name type))
             (keyword-vec [])
             (before-bar t))
        (dolist (kw keywords)
          (if (string= kw "|")
              (setq before-bar nil)
            ;; Check if this is the last keyword and no "|" seen
            (let ((is-last-no-bar
                   (and before-bar (equal kw (car (last keywords))))))
              (when is-last-no-bar
                (setq keyword-vec (vconcat keyword-vec ["|"])))
              (push `((state
                       .
                       ,(car (org-remove-keyword-keys (list kw))))
                      (isFinal
                       . ,(or is-last-no-bar (not before-bar)))
                      (sequenceType . ,type-str))
                    sem-list)))
          (setq keyword-vec (vconcat keyword-vec (vector kw))))
        (push
         `((type . ,type-str) (keywords . ,keyword-vec)) seq-list)))
    (json-encode
     `((sequences . ,(vconcat (nreverse seq-list)))
       (semantics . ,(vconcat (nreverse sem-list)))))))

(defun anvil-org--tool-get-tag-config ()
  "Return the tag configuration as literal Elisp strings."
  (json-encode
   `((org-use-tag-inheritance
      .
      ,(prin1-to-string org-use-tag-inheritance))
     (org-tags-exclude-from-inheritance
      . ,(prin1-to-string org-tags-exclude-from-inheritance))
     (org-tag-alist . ,(prin1-to-string org-tag-alist))
     (org-tag-persistent-alist
      . ,(prin1-to-string org-tag-persistent-alist)))))

(defun anvil-org--tool-get-allowed-files ()
  "Return the list of allowed Org files."
  (json-encode `((files . ,(vconcat anvil-org-allowed-files)))))

(defun anvil-org--tool-update-todo-state (uri current_state new_state)
  "Update the TODO state of a headline at URI.
Creates an Org ID for the headline if one doesn't exist.
Returns the ID-based URI for the updated headline.
CURRENT_STATE is the current TODO state (empty string for no state).
NEW_STATE is the new TODO state to set.

MCP Parameters:
  uri - URI of the headline
        Formats:
          - org-headline://{absolute-path}#{headline-path}
          - org-id://{id}
  current_state - Current TODO state (empty string for no state)
  new_state - New TODO state (must be in `org-todo-keywords')"
  (let* ((parsed (anvil-org--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed)))
    (anvil-org--validate-todo-state new_state)
    (anvil-org--modify-and-save file-path "update"
                              `((previous_state
                                 .
                                 ,(or current_state ""))
                                (new_state . ,new_state))
      (anvil-org--goto-headline-from-uri
       headline-path (string-prefix-p anvil-org--uri-id-prefix uri))

      ;; Check current state matches
      (beginning-of-line)
      (let ((actual-state (org-get-todo-state)))
        (unless (string= actual-state current_state)
          (anvil-org--state-mismatch-error
           (or current_state "(no state)")
           (or actual-state "(no state)") "State")))

      ;; Update the state
      (org-todo new_state))))

(defun anvil-org--tool-add-todo
    (title todo_state tags body parent_uri &optional after_uri)
  "Add a new TODO item to an Org file.
Creates an Org ID for the new headline and returns its ID-based URI.
TITLE is the headline text.
TODO_STATE is the TODO state from `org-todo-keywords'.
TAGS is a single tag string or list of tag strings.
BODY is optional body text.
PARENT_URI is the URI of the parent item.
AFTER_URI is optional URI of sibling to insert after.

MCP Parameters:
  title - The headline text
  todo_state - TODO state from `org-todo-keywords'
  tags - Tags to add (single string or array of strings)
  body - Optional body text content
  parent_uri - Parent item URI
               Formats:
                 - org-headline://{absolute-path}#{headline-path}
                 - org-id://{id}
  after_uri - Sibling to insert after (optional)
              Formats:
                - org-headline://{absolute-path}#{headline-path}
                - org-id://{id}"
  (anvil-org--validate-headline-title title)
  (anvil-org--validate-todo-state todo_state)
  (let* ((tag-list (anvil-org--validate-and-normalize-tags tags))
         file-path
         parent-path
         parent-id)

    ;; Parse parent URI once to extract file-path and parent location
    (anvil-org--with-uri-prefix-dispatch
        parent_uri
      ;; Handle org-headline:// URIs
      (let* ((split-result (anvil-org--split-headline-uri headline))
             (filename (car split-result))
             (path-str (cdr split-result))
             (allowed-file (anvil-org--validate-file-access filename)))
        (setq file-path (expand-file-name allowed-file))
        (when (and path-str (> (length path-str) 0))
          (setq parent-path
                (mapcar
                 #'url-unhex-string (split-string path-str "/")))))
      ;; Handle org-id:// URIs
      (progn
        (setq file-path (anvil-org--find-allowed-file-with-id id))
        (setq parent-id id)))

    ;; Add the TODO item
    (anvil-org--modify-and-save file-path "add TODO"
                              `((file
                                 .
                                 ,(file-name-nondirectory file-path))
                                (title . ,title))
      (let ((parent-level
             (anvil-org--navigate-to-parent-or-top
              parent-path parent-id)))

        ;; Handle positioning after navigation to parent
        (when (or parent-path parent-id)
          (let ((parent-end
                 (save-excursion
                   (org-end-of-subtree t t)
                   (point))))
            (anvil-org--position-for-new-child after_uri parent-end)))

        ;; Validate body before inserting heading
        ;; Calculate the target level for validation
        (let ((target-level
               (if (or parent-path parent-id)
                   ;; Child heading - parent level + 1
                   (1+ (or parent-level 0))
                 ;; Top-level heading
                 1)))

          ;; Validate body content if provided
          (when body
            (anvil-org--validate-body-no-headlines body target-level)
            (anvil-org--validate-body-no-unbalanced-blocks body)))

        ;; Insert the new heading
        (anvil-org--insert-heading title parent-level)

        (org-todo todo_state)

        (when tag-list
          (org-set-tags tag-list))

        ;; Add body if provided
        (if body
            (progn
              (end-of-line)
              (insert "\n" body)
              (unless (string-suffix-p "\n" body)
                (insert "\n"))
              ;; Move back to the heading for org-id-get-create
              ;; org-id-get-create requires point to be on a heading
              (org-back-to-heading t))
          ;; No body - ensure newline after heading
          (end-of-line)
          (unless (looking-at "\n")
            (insert "\n")))))))

;; Resource handlers

(defun anvil-org--handle-outline-resource (params)
  "Handler for org://{filename}/outline template.
PARAMS is an alist containing the filename parameter."
  (let* ((filename (alist-get "filename" params nil nil #'string=))
         (allowed-file (anvil-org--validate-file-access filename))
         (outline
          (anvil-org--generate-outline
           (expand-file-name allowed-file))))
    (json-encode outline)))

(defun anvil-org--handle-file-resource (params)
  "Handler for org://{filename} template.
PARAMS is an alist containing the filename parameter."
  (let* ((filename (alist-get "filename" params nil nil #'string=))
         (allowed-file (anvil-org--validate-file-access filename)))
    (anvil-org--read-file (expand-file-name allowed-file))))

(defun anvil-org--handle-headline-resource (params)
  "Handler for org-headline://{filename} template.
PARAMS is an alist containing the filename parameter.
The filename parameter includes both file and headline path."
  (let* ((full-path (alist-get "filename" params nil nil #'string=))
         (split-result (anvil-org--split-headline-uri full-path))
         (filename (car split-result))
         (allowed-file (anvil-org--validate-file-access filename))
         (headline-path-str (cdr split-result))
         ;; Parse the path (URL-encoded headline path)
         (headline-path
          (when headline-path-str
            (mapcar
             #'url-unhex-string
             (split-string headline-path-str "/")))))
    (if headline-path
        (let ((content
               (anvil-org--get-headline-content
                allowed-file headline-path)))
          (unless content
            (anvil-org--resource-not-found-error
             "headline" (mapconcat #'identity headline-path "/")))
          content)
      ;; No headline path means get entire file
      (anvil-org--read-file allowed-file))))

(defun anvil-org--handle-id-resource (params)
  "Handler for org-id://{uuid} template.
PARAMS is an alist containing the uuid parameter."
  (let* ((id (alist-get "uuid" params nil nil #'string=))
         (file-path (org-id-find-id-file id)))
    (unless file-path
      (anvil-org--resource-not-found-error "ID" id))
    (let ((allowed-file (anvil-org--find-allowed-file file-path)))
      (unless allowed-file
        (anvil-org--resource-file-access-error id))
      (anvil-org--get-content-by-id allowed-file id))))

(defun anvil-org--tool-rename-headline (uri current_title new_title)
  "Rename headline title at URI from CURRENT_TITLE to NEW_TITLE.
Preserves the current TODO state and tags, creates an Org ID for the
headline if one doesn't exist.
Returns the ID-based URI for the renamed headline.

MCP Parameters:
  uri - URI of the headline
        Formats:
          - org-headline://{absolute-path}#{headline-path}
          - org-id://{id}
  current_title - Current title without TODO state or tags
  new_title - New title without TODO state or tags"
  (anvil-org--validate-headline-title new_title)

  (let* ((parsed (anvil-org--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed)))

    ;; Rename the headline in the file
    (anvil-org--modify-and-save file-path "rename"
                              `((previous_title . ,current_title)
                                (new_title . ,new_title))
      ;; Navigate to the headline
      (anvil-org--goto-headline-from-uri
       headline-path (string-prefix-p anvil-org--uri-id-prefix uri))

      ;; Verify current title matches
      (beginning-of-line)
      (let ((actual-title (org-get-heading t t t t)))
        (unless (string= actual-title current_title)
          (anvil-org--state-mismatch-error
           current_title actual-title "Title")))

      (org-edit-headline new_title))))

(defun anvil-org--tool-edit-body
    (resource_uri old_body new_body replace_all)
  "Edit body content of an Org node using partial string replacement.
RESOURCE_URI is the URI of the node to edit.
OLD_BODY is the substring to search for within the node's body.
         Use empty string \"\" to add content to an empty node.
NEW_BODY is the replacement text.
REPLACE_ALL if non-nil, replace all occurrences.

MCP Parameters:
  resource_uri - URI of the node
                 Formats:
                   - org-headline://{absolute-path}#{headline-path}
                   - org-id://{id}
  old_body - Substring to replace within the body (must be unique
             unless replace_all).  Use \"\" to add to empty nodes
  new_body - Replacement text
  replace_all - Replace all occurrences (optional, default false)"
  ;; Normalize JSON false to nil for proper boolean handling
  ;; JSON false can arrive as :false (keyword) or "false" (string)
  (let ((replace_all
         (cond
          ((eq replace_all :false)
           nil)
          ((equal replace_all "false")
           nil)
          (t
           replace_all))))
    (anvil-org--validate-body-no-unbalanced-blocks new_body)

    (let* ((parsed (anvil-org--parse-resource-uri resource_uri))
           (file-path (car parsed))
           (headline-path (cdr parsed)))

      (anvil-org--modify-and-save file-path "edit body" nil
        (anvil-org--goto-headline-from-uri
         headline-path
         (string-prefix-p anvil-org--uri-id-prefix resource_uri))

        (anvil-org--validate-body-no-headlines
         new_body (org-current-level))

        ;; Skip past headline and properties
        (org-end-of-meta-data t)

        ;; Get body boundaries
        (let ((body-begin (point))
              (body-end nil)
              (body-content nil)
              (occurrence-count 0))

          ;; Find end of body (before next headline or end of subtree)
          (save-excursion
            (if (org-goto-first-child)
                ;; Has children - body ends before first child
                (setq body-end (point))
              ;; No children - body extends to end of subtree
              (org-end-of-subtree t)
              (setq body-end (point))))

          ;; Extract body content
          (setq body-content
                (buffer-substring-no-properties body-begin body-end))

          ;; Trim leading newline if present
          ;; (`org-end-of-meta-data' includes it)
          (when (and (> (length body-content) 0)
                     (= (aref body-content 0) ?\n))
            (setq body-content (substring body-content 1))
            (setq body-begin (1+ body-begin)))

          ;; Check if body is empty
          (when (string-match-p "\\`[[:space:]]*\\'" body-content)
            ;; Empty oldBody + empty body -> add content
            (if (string= old_body "")
                ;; Treat as single replacement
                (setq occurrence-count 1)
              (anvil-org--tool-validation-error
               "Node has no body content")))

          ;; Count occurrences (unless already handled above)
          (unless (= occurrence-count 1)
            ;; Empty oldBody with non-empty body is an error
            (if (and (string= old_body "")
                     (not
                      (string-match-p
                       "\\`[[:space:]]*\\'" body-content)))
                (anvil-org--tool-validation-error
                 "Cannot use empty old_body with non-empty body")
              ;; Normal occurrence counting
              (let ((case-fold-search nil)
                    (search-pos 0))
                (while (string-match
                        (regexp-quote old_body) body-content
                        search-pos)
                  (setq occurrence-count (1+ occurrence-count))
                  (setq search-pos (match-end 0))))))

          ;; Validate occurrences
          (cond
           ((= occurrence-count 0)
            (anvil-org--tool-validation-error "Body text not found: %s"
                                            old_body))
           ((and (> occurrence-count 1) (not replace_all))
            (anvil-org--tool-validation-error
             (concat "Text appears %d times (use replace_all)")
             occurrence-count)))

          ;; Perform replacement
          (anvil-org--replace-body-content
           old_body
           new_body
           body-content
           replace_all
           body-begin
           body-end))))))

;; Tools duplicating resource templates

(defun anvil-org--tool-read-file (file)
  "Tool wrapper for org://{filename} resource template.
FILE is the absolute path to an Org file.

MCP Parameters:
  file - Absolute path to an Org file"
  (anvil-org--handle-file-resource `(("filename" . ,file))))

(defun anvil-org--tool-read-outline (file &optional max_depth)
  "Tool wrapper for org-outline://{filename} resource template.
FILE is the absolute path to an Org file.  When MAX_DEPTH is a
string that parses as a positive integer, only headlines at that
level or shallower are returned (index fast-path side); the
org-element fallback ignores MAX_DEPTH and continues to return
its historical 2-level structure.

MCP Parameters:
  file      - Absolute path to an Org file
  max_depth - Optional integer string (e.g. \"1\", \"3\") capping
              the outline depth; omit for full depth."
  (let ((depth (and max_depth
                    (stringp max_depth)
                    (not (string-empty-p (string-trim max_depth)))
                    (string-to-number max_depth))))
    (when (and depth (<= depth 0)) (setq depth nil))
    (or (anvil-org--try-index-read-outline file depth)
        (anvil-org--handle-outline-resource `(("filename" . ,file))))))

(declare-function anvil-org-index-read-by-id       "anvil-org-index" (org-id))
(declare-function anvil-org-index-read-headline    "anvil-org-index" (file path))
(declare-function anvil-org-index-read-outline-json "anvil-org-index" (file &optional max-depth))

(defun anvil-org--index-available-p ()
  "Return non-nil when the `anvil-org-index' fast-path can be tried.
Checks the feature-flag, the library itself, and the live DB
handle so callers do not need to repeat those guards."
  (and anvil-org-use-index
       (featurep 'anvil-org-index)
       (boundp 'anvil-org-index--db)
       anvil-org-index--db))

(defun anvil-org--try-index-read-by-id (uuid)
  "Try `anvil-org-index-read-by-id' for UUID; return the body or nil.
A nil return means fall back to the org-element handler."
  (when (anvil-org--index-available-p)
    (condition-case _err
        (anvil-org-index-read-by-id uuid)
      (error nil))))

(defun anvil-org--try-index-read-headline (file headline-path)
  "Try `anvil-org-index-read-headline'; return the body or nil."
  (when (anvil-org--index-available-p)
    (condition-case _err
        (anvil-org-index-read-headline file headline-path)
      (error nil))))

(defun anvil-org--try-index-read-outline (file &optional max-depth)
  "Try `anvil-org-index-read-outline-json'; return JSON string or nil.
MAX-DEPTH, when non-nil, caps the outline at that heading level."
  (when (anvil-org--index-available-p)
    (condition-case _err
        (anvil-org-index-read-outline-json file max-depth)
      (error nil))))

(defun anvil-org--tool-read-headline (file headline_path)
  "Tool wrapper for org-headline://{filename}#{path} resource.
FILE is the absolute path to an Org file.
HEADLINE_PATH is the non-empty slash-separated path to
headline.

MCP Parameters:
  file - Absolute path to an Org file
  headline_path - Non-empty slash-separated path to headline
                  (string)
                  Only slashes in headline titles must be
                  encoded as %2F
                  Example: \"Project/Planning\" for nested headlines
                  Example: \"A%2FB Testing\" for headline titled
                  \"A/B Testing\"
                  To read entire files, use org-read-file
                  instead"
  (unless (stringp headline_path)
    (anvil-org--tool-validation-error
     "Parameter headline_path must be a string, got: %S (type: %s)"
     headline_path (type-of headline_path)))
  (when (string-empty-p headline_path)
    (anvil-org--tool-validation-error
     "Parameter headline_path must be non-empty; use \
org-read-file tool to read entire files"))
  (or (anvil-org--try-index-read-headline file headline_path)
      (let ((full-path (concat file "#" headline_path)))
        (anvil-org--handle-headline-resource `(("filename" . ,full-path))))))

(defun anvil-org--tool-read-by-id (uuid)
  "Tool wrapper for org-id://{uuid} resource template.
UUID is the UUID from headline's ID property.  Accepts either the
raw UUID or an `org://UUID' citation URI emitted by the
progressive-disclosure Layer-1 / Layer-2 tools.

MCP Parameters:
  uuid - UUID (or org://UUID citation URI) from headline's ID property"
  (let ((id (if (and (stringp uuid)
                     (string-prefix-p "org://" uuid))
                (substring uuid (length "org://"))
              uuid)))
    (or (anvil-org--try-index-read-by-id id)
        (anvil-org--handle-id-resource `(("uuid" . ,id))))))

(defun anvil-org-enable ()
  "Enable the anvil-org module."
  (anvil-server-register-tool
   #'anvil-org--tool-get-todo-config
   :id "org-get-todo-config"
   :description
   "Get the TODO keyword configuration from the current Emacs
Org-mode settings.  Returns information about task state sequences
and their semantics.

Parameters: None

Returns JSON object with two arrays:
  sequences - Array of TODO keyword sequences, each containing:
    - type: Sequence type (e.g., \"sequence\", \"type\")
    - keywords: Array of keywords including \"|\" separator between
active and done states
  semantics - Array of keyword semantics, each containing:
    - state: The TODO keyword (e.g., \"TODO\", \"DONE\")
    - isFinal: Whether this is a final (done) state (boolean)
    - sequenceType: The sequence type this keyword belongs to

The \"|\" separator in sequences marks the boundary between active
states (before) and done states (after).  If no \"|\" is present,
the last keyword is treated as the done state.

Use this tool to understand the available task states in the Org
configuration before creating or updating TODO items."
   :read-only t
   :server-id anvil-org--server-id)

  (anvil-server-register-tool
   #'anvil-org--tool-get-tag-config
   :id "org-get-tag-config"
   :description
   "Get tag-related configuration from the current Emacs Org-mode
settings.  Returns literal Elisp variable values as strings for tag
configuration introspection.

Parameters: None

Returns JSON object with literal Elisp expressions (as strings) for:
  org-use-tag-inheritance - Controls tag inheritance behavior
  org-tags-exclude-from-inheritance - Tags that don't inherit
  org-tag-alist - List of allowed tags with optional key bindings and
                  groups
  org-tag-persistent-alist - Additional persistent tags (or nil)

The org-tag-alist format includes:
  - Simple tags: (\"tagname\" . key-char)
  - Group markers: :startgroup, :endgroup for mutually exclusive tags
  - Grouptags: :startgrouptag, :grouptags, :endgrouptag for tag
hierarchies

Use this tool to understand:
  - Which tags are allowed
  - Tag inheritance rules
  - Mutually exclusive tag groups
  - Tag hierarchy relationships

This helps validate tag usage and understand tag semantics before
adding or modifying tags on TODO items."
   :read-only t
   :server-id anvil-org--server-id)

  (anvil-server-register-tool
   #'anvil-org--tool-get-allowed-files
   :id "org-get-allowed-files"
   :description
   "Get the list of Org files accessible through the anvil-org
server.  Returns the configured allowed files exactly as specified in
anvil-org-allowed-files.

Parameters: None

Returns JSON object containing:
  files (array of strings): Absolute paths of allowed Org files

Example response:
  {
    \"files\": [
      \"/home/user/org/tasks.org\",
      \"/home/user/org/projects.org\",
      \"/home/user/notes/daily.org\"
    ]
  }

Empty configuration returns:
  {
    \"files\": []
  }

Use cases:
  - Discovery: What Org files can I access through MCP?
  - URI Construction: I need to build an org-headline:// URI - what's
    the exact path?
  - Access Troubleshooting: Why is my file access failing?
  - Configuration Verification: Did my anvil-org-allowed-files setting
    work correctly?"
   :read-only t
   :server-id anvil-org--server-id)

  (anvil-server-register-tool
   #'anvil-org--tool-update-todo-state
   :id "org-update-todo-state"
   :description
   "Update the TODO state of an Org headline.  Changes the task state
while preserving the headline title, tags, and other properties.
Creates an Org ID property for the headline if one doesn't exist.

Parameters:
  uri - URI of the headline to update (string, required)
        Formats:
          - org-headline://{absolute-path}#{url-encoded-path}
          - org-id://{uuid}
  current_state - Expected current TODO state (string, required)
                  Use empty string \"\" if headline has no TODO state
                  Must match actual state or tool will error
  new_state - New TODO state to set (string, required)
              Must be valid keyword from org-todo-keywords

Returns JSON object:
  success - Always true on success (boolean)
  previous_state - The previous TODO state (string, empty for none)
  new_state - The new TODO state that was set (string)
  uri - ID-based URI (org-id://{uuid}) for the updated headline"
   :read-only nil
   :server-id anvil-org--server-id)

  (anvil-server-register-tool
   #'anvil-org--tool-add-todo
   :id "org-add-todo"
   :description
   "Add a new TODO item to an Org file at a specified location.
Creates the headline with TODO state, tags, and optional body content.
Automatically creates an Org ID property for the new headline.

Parameters:
  title - Headline text without TODO state or tags (string, required)
          Cannot be empty or whitespace-only
          Cannot contain newlines
  todo_state - TODO keyword from org-todo-keywords (string, required)
  tags - Tags for the headline (string or array, required)
         Single tag: \"urgent\"
         Multiple tags: [\"work\", \"urgent\"]
         Validated against org-tag-alist if configured
         Must follow Org tag rules (alphanumeric, _, @)
         Respects mutually exclusive tag groups
  body - Body content under the headline (string, optional)
         Cannot contain headlines at same or higher level as new item
         If #+BEGIN/#+END blocks are present, they must be balanced
  parent_uri - Parent location (string, required)
               For top-level: org-headline://{absolute-path}
               For child: org-headline://{path}#{parent-path}
                         or org-id://{parent-uuid}
  after_uri - Sibling to insert after (string, optional)
              Must be org-id://{uuid} format
              If omitted, appends as last child of parent

Returns JSON object:
  success - Always true on success (boolean)
  uri - ID-based URI (org-id://{uuid}) for the new headline
  file - Filename (not full path) where item was added
  title - The headline title that was created

Positioning behavior:
  - With parent_uri only: Appends as last child of parent
  - With parent_uri + after_uri: Inserts immediately after specified
sibling
  - Top-level (parent_uri with no fragment): Adds at end of file."
   :read-only nil
   :server-id anvil-org--server-id)

  (anvil-server-register-tool
   #'anvil-org--tool-rename-headline
   :id "org-rename-headline"
   :description
   "Rename an Org headline's title while preserving its TODO state,
tags, properties, and body content.  Creates an Org ID property for
the headline if one doesn't exist.

Parameters:
  uri - URI of the headline to rename (string, required)
        Formats:
          - org-headline://{absolute-path}#{url-encoded-path}
          - org-id://{uuid}
  current_title - Expected current title without TODO/tags (string,
required)
                  Must match actual title or tool will error
                  Used to prevent race conditions
  new_title - New title without TODO state or tags (string, required)
              Cannot be empty or whitespace-only
              Cannot contain newlines

Returns JSON object:
  success - Always true on success (boolean)
  previous_title - The previous headline title (string)
  new_title - The new title that was set (string)
  uri - ID-based URI (org-id://{uuid}) for the renamed headline"
   :read-only nil
   :server-id anvil-org--server-id)

  (anvil-server-register-tool
   #'anvil-org--tool-edit-body
   :id "org-edit-body"
   :description
   "Edit the body content of an Org headline using partial string
replacement.  Finds and replaces a substring within the headline's
body text.  Creates an Org ID property for the headline if one doesn't
exist.

Parameters:
  resource_uri - URI of the headline to edit (string, required)
                 Formats:
                   - org-headline://{absolute-path}#{url-encoded-path}
                   - org-id://{uuid}
  old_body - Substring to find and replace (string, required)
             Must appear exactly once unless replace_all is true
             Use empty string \"\" only for adding to empty nodes
  new_body - Replacement text (string, required)
             Cannot introduce headlines at same or higher level
             Must maintain balanced #+BEGIN/#+END blocks
  replace_all - Replace all occurrences (boolean, optional, default
                false). When false, old_body must be unique in the
                body.

Returns JSON object:
  success - Always true on success (boolean)
  uri - ID-based URI (org-id://{uuid}) for the edited headline

Special behavior - Empty old_body:
  When old_body is \"\", the tool adds content to empty nodes:
  - Only works if node body is empty or whitespace-only
  - Error if node already has content
  - Useful for adding initial content to newly created headlines"
   :read-only nil
   :server-id anvil-org--server-id)

  (anvil-server-register-tool
   #'anvil-org--tool-read-file
   :id "org-read-file"
   :description
   "Read complete raw content of an Org file. Returns entire file as
plain text with all formatting, properties, and structure preserved.
File must be in anvil-org-allowed-files.

Parameters:
  file - Absolute path to Org file (string, required)

Returns: Plain text content of the entire Org file"
   :read-only t
   :server-id anvil-org--server-id)

  (anvil-server-register-tool
   #'anvil-org--tool-read-outline
   :id "org-read-outline"
   :description
   "Get hierarchical structure of Org file as JSON outline. Returns
   all headline titles and nesting relationships at full depth. File
   must be in anvil-org-allowed-files.

Parameters:
  file - Absolute path to Org file (string, required)

Returns: JSON object with hierarchical outline structure"
   :read-only t
   :server-id anvil-org--server-id)

  (anvil-server-register-tool
   #'anvil-org--tool-read-headline
   :id "org-read-headline"
   :description
   "Read specific Org headline by hierarchical path. Returns headline
   with TODO state, tags, properties, body text, and all nested
   subheadings. File must be in anvil-org-allowed-files.

Parameters:
  file - Absolute path to Org file (string, required)
  headline_path - Non-empty slash-separated path to headline (string,
                  required). Only slashes (/) in  headline titles must
                  be encoded as %2F
                  Example: \"Project/Planning\" for nested headlines
                  Example: \"A%2FB Testing\" for headline titled
                  \"A/B Testing\"
                  To read entire files, use org-read-file instead

Returns: Plain text content of the headline and its subtree"
   :read-only t
   :server-id anvil-org--server-id)

  (anvil-server-register-tool
   #'anvil-org--tool-read-by-id
   :id "org-read-by-id"
   :description
   "Layer 3 of anvil progressive disclosure (see `disclosure-help').
Read Org headline by its unique ID property.  More stable than
path-based access since IDs don't change when headlines are renamed
or moved.  Accepts the raw UUID directly or the `org://UUID' citation
URI returned by Layer 1 (`org-index-index') / Layer 2
(`org-index-search').  File containing the ID must be in
anvil-org-allowed-files.

Parameters:
  uuid - UUID (or org://UUID citation URI) from headline's ID
         property (string, required)

Returns: Plain text content of the headline and its subtree"
   :read-only t
   :server-id anvil-org--server-id)

  ;; Register template resources for org files
  (anvil-server-register-resource
   "org://{filename}" #'anvil-org--handle-file-resource
   :name "Org file"
   :description
   "Access the complete raw content of an Org file.  Returns the
entire file as plain text, preserving all formatting, properties, and
structure.

URI format: org://{filename}
  filename - Absolute path to the Org file (required)

Returns: Plain text content of the entire Org file"
   :mime-type "text/plain"
   :server-id anvil-org--server-id)

  (anvil-server-register-resource
   "org-outline://{filename}" #'anvil-org--handle-outline-resource
   :name "Org file outline"
   :description
   "Get the hierarchical structure of an Org file as a JSON
outline.  Extracts headline titles and their nesting relationships up
to 2 levels deep.

URI format: org-outline://{filename}
  filename - Absolute path to the Org file (required)

Returns: JSON object with structure:
  {
    \"headings\": [
      {
        \"title\": \"Top-level heading\",
        \"level\": 1,
        \"children\": [
          {
            \"title\": \"Subheading\",
            \"level\": 2,
            \"children\": []
          }
        ]
      }
    ]
  }

Depth limitation:
  - Level 1 headings (top-level) are extracted
  - Level 2 headings (direct children) are included
  - Deeper levels are not included (children arrays are empty)

Example URIs:
  org-outline:///home/user/notes/tasks.org
  org-outline:///Users/name/Documents/projects.org

Use this resource to:
  - Get document structure overview
  - Understand file organization without reading full content"
   :mime-type "application/json"
   :server-id anvil-org--server-id)

  (anvil-server-register-resource
   (concat anvil-org--uri-headline-prefix "{filename}")
   #'anvil-org--handle-headline-resource
   :name "Org headline content"
   :description
   "Access content of a specific Org headline by its path in the
file hierarchy.  Returns the headline and all its subheadings as
plain text.

URI format: org-headline://{filename}#{headline-path}
  filename - Absolute path (# characters must be encoded as %23)
  # - Fragment separator (literal #, not encoded)
  headline-path - URL-encoded headline titles separated by /

URI encoding rules:
  - File path # → %23 (e.g., file#1.org → file%231.org)
  - Fragment separator → # (literal, marks start of headline path)
  - Headline title spaces → %20
  - Headline title # → %23 (e.g., Task #5 → Task%20%2345)
  - Path separator → / (literal, between nested headlines)

Encoding limitations:
  - ONLY # is encoded in file paths (minimal encoding for readability)
  - File paths with % characters should be avoided
  - Files named with %XX patterns (e.g., \"100%23done.org\") will fail
  - For such files, rename them or use org-id:// URIs instead
  - Headline paths use full URL encoding (all special chars encoded)

Returns: Plain text content including:
  - The headline itself with TODO state and tags
  - All properties drawer content
  - Body text
  - All nested subheadings (complete subtree)

Example URIs:
  org-headline:///home/user/tasks.org#Project%20Alpha
    → Top-level \"Project Alpha\" heading

  org-headline:///home/user/tasks.org#Project%20Alpha/Planning
    → \"Planning\" subheading under \"Project Alpha\"

  org-headline:///home/user/tasks.org#Issue%20%2342
    → Heading titled \"Issue #42\"

  org-headline:///home/user/file%231.org#Task%20%235
    → \"Task #5\" from file named \"file#1.org\"

  org-headline:///home/user/tasks.org
    → Entire file (no fragment means whole file)

Use this resource to:
  - Read specific sections of an Org file
  - Access headline content by hierarchical path
  - Get complete subtree including all children"
   :mime-type "text/plain"
   :server-id anvil-org--server-id)

  (anvil-server-register-resource
   (concat anvil-org--uri-id-prefix "{uuid}")
   #'anvil-org--handle-id-resource
   :name "Org node by ID"
   :description
   "Access content of an Org headline by its unique ID property.
More stable than path-based access since IDs don't change when
headlines are renamed or moved.

URI format: org-id://{uuid}
  uuid - Value of the headline's ID property (required)

How IDs work in Org:
  Headlines can have an ID property:
    * My Headline
    :PROPERTIES:
    :ID: 550e8400-e29b-41d4-a716-446655440000
    :END:

  The ID provides permanent, unique identification regardless of:
    - Headline title changes
    - Headline moving to different locations in file
    - File renaming or moving

Security and access:
  - The file containing the ID must be in anvil-org-allowed-files
  - Uses org-id database for ID-to-file lookup
  - Falls back to searching allowed files if database is stale

Returns: Plain text content including:
  - The headline itself with TODO state and tags
  - All properties drawer content
  - Body text
  - All nested subheadings (complete subtree)

Example URIs:
  org-id://550e8400-e29b-41d4-a716-446655440000
    → Headline with that ID property

Use this resource to:
  - Access headlines by stable identifier
  - Reference content that may be renamed or moved
  - Build cross-references between Org nodes"
   :mime-type "text/plain"
   :server-id anvil-org--server-id))

(defun anvil-org-disable ()
  "Disable the anvil-org module."
  (anvil-server-unregister-tool
   "org-get-todo-config" anvil-org--server-id)
  (anvil-server-unregister-tool
   "org-get-tag-config" anvil-org--server-id)
  (anvil-server-unregister-tool
   "org-get-allowed-files" anvil-org--server-id)
  (anvil-server-unregister-tool
   "org-update-todo-state" anvil-org--server-id)
  (anvil-server-unregister-tool "org-add-todo" anvil-org--server-id)
  (anvil-server-unregister-tool
   "org-rename-headline" anvil-org--server-id)
  (anvil-server-unregister-tool "org-edit-body" anvil-org--server-id)
  ;; Unregister workaround tools
  (anvil-server-unregister-tool "org-read-file" anvil-org--server-id)
  (anvil-server-unregister-tool
   "org-read-outline" anvil-org--server-id)
  (anvil-server-unregister-tool
   "org-read-headline" anvil-org--server-id)
  (anvil-server-unregister-tool "org-read-by-id" anvil-org--server-id)
  ;; Unregister template resources
  (anvil-server-unregister-resource
   "org://{filename}" anvil-org--server-id)
  (anvil-server-unregister-resource
   "org-outline://{filename}" anvil-org--server-id)
  (anvil-server-unregister-resource
   (concat
    anvil-org--uri-headline-prefix "{filename}")
   anvil-org--server-id)
  (anvil-server-unregister-resource
   (concat anvil-org--uri-id-prefix "{uuid}") anvil-org--server-id))

(provide 'anvil-org)
;;; anvil-org.el ends here
