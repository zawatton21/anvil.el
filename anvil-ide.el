;;; anvil-ide.el --- IDE tools for anvil: xref, diagnostics, imenu, tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Yoav Orot (original claude-code-ide-emacs-tools)
;; Copyright (C) 2025-2026 zawatton (anvil fork)

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; IDE tools extracted from claude-code-ide-emacs-tools.el and
;; claude-code-ide-diagnostics.el, made LLM-agnostic.
;;
;; Tools:
;; - xref_find_references: find references to an identifier
;; - xref_find_apropos: search symbols by pattern
;; - project_info: project overview
;; - imenu_list_symbols: list symbols in a file
;; - treesit_info: tree-sitter AST inspection
;; - diagnostics: flycheck/flymake diagnostics

;;; Code:

(require 'anvil-server)
(require 'apropos)
(require 'xref)
(require 'cl-lib)
(require 'imenu)
(require 'json)

;; Optional
(require 'project nil t)
(require 'flycheck nil t)
(require 'flymake nil t)

;; Tree-sitter declarations
(declare-function treesit-available-p "treesit" ())
(declare-function treesit-node-at "treesit" (pos &optional parser-or-lang named))
(declare-function treesit-node-text "treesit" (node &optional no-property))
(declare-function treesit-node-field-name "treesit" (node))
(declare-function treesit-node-type "treesit" (node))
(declare-function treesit-node-start "treesit" (node))
(declare-function treesit-node-end "treesit" (node))
(declare-function treesit-node-parent "treesit" (node))
(declare-function treesit-node-child "treesit" (node n))
(declare-function treesit-node-child-count "treesit" (node))
(declare-function treesit-node-check "treesit" (node property))
(declare-function treesit-parser-list "treesit" (&optional buffer language))
(declare-function treesit-parser-create "treesit" (language &optional buffer no-reuse tag))
(declare-function treesit-parser-root-node "treesit" (parser))
(autoload 'anvil-treesit-language-for-file "anvil-treesit" nil nil)
(autoload 'anvil-treesit-ensure-grammar "anvil-treesit" nil nil)

;; Flycheck declarations
(defvar flycheck-current-errors)
(declare-function flycheck-error-line "flycheck" (err))
(declare-function flycheck-error-column "flycheck" (err))
(declare-function flycheck-error-end-line "flycheck" (err))
(declare-function flycheck-error-end-column "flycheck" (err))
(declare-function flycheck-error-level "flycheck" (err))
(declare-function flycheck-error-checker "flycheck" (err))
(declare-function flycheck-error-message "flycheck" (err))

;;; Customization

(defgroup anvil-ide nil
  "Anvil IDE tools."
  :group 'anvil
  :prefix "anvil-ide-")

(defcustom anvil-ide-diagnostics-backend 'auto
  "Backend to use for diagnostics collection.
- `auto': detect flycheck or flymake automatically
- `flycheck': use flycheck
- `flymake': use flymake"
  :type '(choice (const :tag "Automatic" auto)
                 (const :tag "Flycheck" flycheck)
                 (const :tag "Flymake" flymake))
  :group 'anvil-ide)

;;; xref tools

(defun anvil-ide--xref-find-references (identifier file-path)
  "Find references to IDENTIFIER using FILE-PATH as context.

MCP Parameters:
  identifier - the identifier to find references for
  file-path - file path to use as context for the search"
  (anvil-server-with-error-handling
   (let ((target-buffer (or (find-buffer-visiting file-path)
                            (find-file-noselect file-path))))
     (with-current-buffer target-buffer
       (let ((backend (xref-find-backend)))
         (if (not backend)
             (format "No xref backend available for %s" file-path)
           (let ((xref-items (xref-backend-references backend identifier)))
             (if xref-items
                 (mapconcat
                  (lambda (item)
                    (let* ((location (xref-item-location item))
                           (file (xref-location-group location))
                           (marker (xref-location-marker location))
                           (line (with-current-buffer (marker-buffer marker)
                                   (save-excursion
                                     (goto-char marker)
                                     (line-number-at-pos))))
                           (summary (xref-item-summary item)))
                      (format "%s:%d: %s" file line summary)))
                  xref-items "\n")
               (format "No references found for '%s'" identifier)))))))))

(defun anvil-ide--xref-find-apropos (pattern file-path)
  "Find symbols matching PATTERN using FILE-PATH as context.

MCP Parameters:
  pattern - the pattern to search for symbols
  file-path - file path to use as context for the search"
  (anvil-server-with-error-handling
   (let ((target-buffer (or (find-buffer-visiting file-path)
                            (find-file-noselect file-path))))
     (with-current-buffer target-buffer
       (let ((backend (xref-find-backend)))
         (cond
          ((not backend)
           (format "No xref backend available for %s" file-path))
          ((and (eq backend 'etags)
                (not (or (and (boundp 'tags-file-name) tags-file-name
                              (file-exists-p tags-file-name))
                         (and (boundp 'tags-table-list) tags-table-list
                              (cl-some #'file-exists-p tags-table-list)))))
           (format "No tags table available for %s" file-path))
          (t
           (let ((xref-items (xref-backend-apropos backend pattern)))
             (if xref-items
                 (mapconcat
                  (lambda (item)
                    (let* ((location (xref-item-location item))
                           (file (xref-location-group location))
                           (marker (xref-location-marker location))
                           (line (with-current-buffer (marker-buffer marker)
                                   (save-excursion
                                     (goto-char marker)
                                     (line-number-at-pos))))
                           (summary (xref-item-summary item)))
                      (format "%s:%d: %s" file line summary)))
                  xref-items "\n")
               (format "No symbols found matching '%s'" pattern))))))))))

;;; Project info

(defun anvil-ide--project-for-buffer (buffer)
  "Return the project object for BUFFER, or nil."
  (when (and (featurep 'project)
             (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (and (stringp default-directory)
                 (file-directory-p default-directory))
        (ignore-errors (project-current nil))))))

(defun anvil-ide--current-project ()
  "Return the most relevant current project, or nil."
  (or (anvil-ide--project-for-buffer
       (window-buffer (selected-window)))
      (anvil-ide--project-for-buffer (current-buffer))
      (cl-loop for buffer in (buffer-list)
               when (or (buffer-file-name buffer)
                        (buffer-local-value 'default-directory buffer))
               thereis (anvil-ide--project-for-buffer buffer))))

(defun anvil-ide--project-info ()
  "Get information about the current project.

MCP Parameters: (none)"
  (anvil-server-with-error-handling
   (if-let* ((proj (anvil-ide--current-project)))
       (let ((root (project-root proj)))
         (format "Project: %s\nFiles: %d"
                 root
                 (length (project-files proj))))
     "No project detected")))

;;; imenu

(defun anvil-ide--imenu-list-symbols (file-path)
  "List all symbols in FILE-PATH using imenu.

MCP Parameters:
  file-path - path to the file to analyze for symbols"
  (anvil-server-with-error-handling
   (let ((target-buffer (or (find-buffer-visiting file-path)
                            (find-file-noselect file-path))))
     (with-current-buffer target-buffer
       (imenu--make-index-alist)
       (if imenu--index-alist
           (let ((results '()))
             (dolist (item imenu--index-alist)
               (cond
                ((string-match-p "^\\*" (car item)) nil)
                ((or (markerp (cdr item)) (numberp (cdr item)))
                 (let* ((pos (if (markerp (cdr item))
                                 (marker-position (cdr item))
                               (cdr item)))
                        (line (line-number-at-pos pos)))
                   (push (format "%s:%d: %s" file-path line (car item))
                         results)))
                ((listp (cdr item))
                 (let ((category (car item)))
                   (dolist (subitem (cdr item))
                     (when (and (consp subitem)
                                (or (markerp (cdr subitem))
                                    (numberp (cdr subitem))))
                       (let* ((pos (if (markerp (cdr subitem))
                                       (marker-position (cdr subitem))
                                     (cdr subitem)))
                              (line (line-number-at-pos pos)))
                         (push (format "%s:%d: [%s] %s"
                                       file-path line category (car subitem))
                               results))))))))
             (if results
                 (mapconcat #'identity (nreverse results) "\n")
               (format "No symbols found in %s" file-path)))
         (format "No imenu support or no symbols found in %s" file-path))))))

;;; Tree-sitter

(defun anvil-ide--treesit-format-tree (node level max-depth)
  "Format NODE and its children as a tree string.
LEVEL is indentation level, MAX-DEPTH is the limit."
  (if (or (not node) (>= level max-depth))
      ""
    (let* ((indent (make-string (* level 2) ?\s))
           (type (treesit-node-type node))
           (named (if (treesit-node-check node 'named) " (named)" ""))
           (start (treesit-node-start node))
           (end (treesit-node-end node))
           (field-name (treesit-node-field-name node))
           (field-str (if field-name (format " [%s]" field-name) ""))
           (text (treesit-node-text node t))
           (text-preview (if (and (< (length text) 40)
                                  (not (string-match-p "\n" text)))
                             (format " \"%s\"" text)
                           ""))
           (result (format "%s%s%s%s (%d-%d)%s\n"
                           indent type named field-str start end text-preview))
           (child-count (treesit-node-child-count node)))
      (dotimes (i child-count)
        (when-let ((child (treesit-node-child node i)))
          (setq result (concat result
                               (anvil-ide--treesit-format-tree
                                child (1+ level) max-depth)))))
      result)))

(defun anvil-ide--treesit-parse-integer (value name minimum)
  "Parse VALUE as an integer MCP parameter named NAME.
VALUE may already be an integer or a decimal string.
MINIMUM is the inclusive lower bound."
  (let ((n (cond
            ((null value) nil)
            ((integerp value) value)
            ((and (stringp value)
                  (string-match-p "\\`[0-9]+\\'" value))
             (string-to-number value))
            (t
             (user-error "%s must be an integer, got %S" name value)))))
    (when (and n (< n minimum))
      (user-error "%s must be >= %d, got %S" name minimum value))
    n))

(defun anvil-ide--treesit-ensure-parser (file-path)
  "Return a tree-sitter parser for the current buffer visiting FILE-PATH.
If no parser exists yet, infer the language from FILE-PATH and create
one after verifying the grammar is installed."
  (or (car (treesit-parser-list))
      (let ((lang (anvil-treesit-language-for-file file-path)))
        (when lang
          (anvil-treesit-ensure-grammar lang)
          (treesit-parser-create lang)))))

(defun anvil-ide--treesit-info (file-path &optional line column whole_file include_ancestors include_children)
  "Get tree-sitter syntax tree information for FILE-PATH.

MCP Parameters:
  file-path - path to the file to analyze
  line - line number, 1-based
  column - column number, 0-based
  whole_file - show the entire file syntax tree
  include_ancestors - include parent node hierarchy
  include_children - include child nodes"
  (anvil-server-with-error-handling
   (if (not (and (fboundp 'treesit-available-p)
                 (treesit-available-p)))
       "Tree-sitter is not available in this Emacs build"
     (let* ((line-number (anvil-ide--treesit-parse-integer line "line" 1))
            (column-number (anvil-ide--treesit-parse-integer column "column" 0))
            (target-buffer (or (find-buffer-visiting file-path)
                               (find-file-noselect file-path))))
       (with-current-buffer target-buffer
         (let ((parser (anvil-ide--treesit-ensure-parser file-path)))
           (if (not parser)
               (format "No tree-sitter parser available for %s" file-path)
             (let* ((root-node (treesit-parser-root-node parser))
                    (pos (cond (whole_file nil)
                               (line-number (save-excursion
                                              (goto-char (point-min))
                                              (forward-line (1- line-number))
                                              (move-to-column (or column-number 0))
                                              (point)))
                               (t (point))))
                    (node (if whole_file root-node (treesit-node-at pos parser)))
                    (results '()))
               (if (not node)
                   "No tree-sitter node found"
                 (if whole_file
                     (anvil-ide--treesit-format-tree root-node 0 20)
                   (push (format "Node Type: %s" (treesit-node-type node)) results)
                   (push (format "Range: %d-%d"
                                 (treesit-node-start node) (treesit-node-end node)) results)
                   (push (format "Text: %s"
                                 (truncate-string-to-width
                                  (treesit-node-text node t) 80 nil nil "...")) results)
                   (when (treesit-node-check node 'named)
                     (push "Named: yes" results))
                   (when-let ((field-name (treesit-node-field-name node)))
                     (push (format "Field: %s" field-name) results))
                   (when include_ancestors
                     (push "\nAncestors:" results)
                     (let ((parent (treesit-node-parent node))
                           (level 1))
                       (while (and parent (< level 10))
                         (push (format "  %s[%d] %s (%d-%d)"
                                       (make-string level ?-)
                                       level
                                       (treesit-node-type parent)
                                       (treesit-node-start parent)
                                       (treesit-node-end parent))
                               results)
                         (setq parent (treesit-node-parent parent))
                         (cl-incf level))))
                   (when include_children
                     (push "\nChildren:" results)
                     (let ((child-count (treesit-node-child-count node))
                           (i 0))
                       (if (= child-count 0)
                           (push "  (no children)" results)
                         (while (< i (min child-count 20))
                           (when-let ((child (treesit-node-child node i)))
                             (push (format "  [%d] %s%s (%d-%d)"
                                           i (treesit-node-type child)
                                           (if (treesit-node-check child 'named) " (named)" "")
                                           (treesit-node-start child) (treesit-node-end child))
                                   results))
                           (cl-incf i))
                         (when (> child-count 20)
                           (push (format "  ... and %d more children" (- child-count 20))
                                 results)))))
                   (string-join (nreverse results) "\n")))))))))))

;;; Diagnostics

(defun anvil-ide--severity-to-string (severity)
  "Convert diagnostic SEVERITY to human-readable string."
  (pcase severity
    ('error "Error") ('warning "Warning") ('info "Information") ('hint "Hint")
    ('flymake-error "Error") (':error "Error")
    ('flymake-warning "Warning") (':warning "Warning")
    ('flymake-note "Information") (':note "Information")
    (_ "Information")))

(defun anvil-ide--flycheck-diagnostics (buffer)
  "Get Flycheck diagnostics for BUFFER."
  (when (featurep 'flycheck)
    (with-current-buffer buffer
      (when (bound-and-true-p flycheck-mode)
        (mapcar (lambda (err)
                  (format "%s:%d:%d: [%s] %s (%s)"
                          (buffer-file-name)
                          (flycheck-error-line err)
                          (or (flycheck-error-column err) 1)
                          (anvil-ide--severity-to-string (flycheck-error-level err))
                          (flycheck-error-message err)
                          (or (flycheck-error-checker err) "flycheck")))
                flycheck-current-errors)))))

(defun anvil-ide--flymake-diagnostics (buffer)
  "Get Flymake diagnostics for BUFFER."
  (when (featurep 'flymake)
    (with-current-buffer buffer
      (when (bound-and-true-p flymake-mode)
        (mapcar (lambda (diag)
                  (save-excursion
                    (goto-char (flymake-diagnostic-beg diag))
                    (let ((line (line-number-at-pos))
                          (col (current-column)))
                      (format "%s:%d:%d: [%s] %s (%s)"
                              (buffer-file-name)
                              line col
                              (anvil-ide--severity-to-string
                               (flymake-diagnostic-type diag))
                              (flymake-diagnostic-text diag)
                              (symbol-name (or (flymake-diagnostic-backend diag) 'flymake))))))
                (flymake-diagnostics))))))

(defun anvil-ide--diagnostics (&optional file-path)
  "Get diagnostics for FILE-PATH or all project buffers.

MCP Parameters:
  file-path - file to get diagnostics for (optional, all if omitted)"
  (anvil-server-with-error-handling
   (let ((backend anvil-ide-diagnostics-backend)
         (results '()))
     (if (and file-path (not (string-empty-p file-path)))
         ;; Single file
         (when-let ((buffer (or (get-file-buffer (expand-file-name file-path))
                                (find-file-noselect
                                 (expand-file-name file-path)))))
           (when (eq backend 'auto)
             (setq backend (cond
                            ((and (featurep 'flycheck)
                                  (with-current-buffer buffer
                                    (bound-and-true-p flycheck-mode))) 'flycheck)
                            ((and (featurep 'flymake)
                                  (with-current-buffer buffer
                                    (bound-and-true-p flymake-mode))) 'flymake))))
           (setq results (pcase backend
                           ('flycheck (anvil-ide--flycheck-diagnostics buffer))
                           ('flymake (anvil-ide--flymake-diagnostics buffer)))))
       ;; All buffers
       (dolist (buffer (buffer-list))
         (when (buffer-file-name buffer)
           (let ((diags (or (anvil-ide--flycheck-diagnostics buffer)
                            (anvil-ide--flymake-diagnostics buffer))))
             (when diags
               (setq results (append results diags)))))))
     (if results
         (mapconcat #'identity results "\n")
       "No diagnostics found"))))

;;; Module enable/disable

(defvar anvil-ide--server-id "anvil"
  "Server ID for IDE tool registration.")

(defun anvil-ide-enable ()
  "Register IDE tools with the MCP server."
  (setq anvil-ide--server-id
        (or (and (boundp 'anvil-server-id) anvil-server-id) "anvil"))
  (anvil-server-register-tool
   #'anvil-ide--xref-find-references
   :id "xref_find_references"
   :intent '(ide)
   :layer 'dev
   :description "Find where a function, variable, or class is used throughout the codebase"
   :read-only t
   :server-id anvil-ide--server-id)
  (anvil-server-register-tool
   #'anvil-ide--xref-find-apropos
   :id "xref_find_apropos"
   :intent '(ide)
   :layer 'dev
   :description "Search for functions, variables, or classes by name pattern across the project"
   :read-only t
   :server-id anvil-ide--server-id)
  (anvil-server-register-tool
   #'anvil-ide--project-info
   :id "project_info"
   :intent '(ide)
   :layer 'dev
   :description "Get project directory and file count overview"
   :read-only t
   :server-id anvil-ide--server-id)
  (anvil-server-register-tool
   #'anvil-ide--imenu-list-symbols
   :id "imenu_list_symbols"
   :intent '(ide structure)
   :layer 'dev
   :description "List all functions, classes, and variables in a file with their locations"
   :read-only t
   :server-id anvil-ide--server-id)
  (anvil-server-register-tool
   #'anvil-ide--treesit-info
   :id "treesit_info"
   :intent '(ide)
   :layer 'dev
   :description "Get tree-sitter syntax tree info for a file — node types, ranges, hierarchy"
   :read-only t
   :server-id anvil-ide--server-id)
  (anvil-server-register-tool
   #'anvil-ide--diagnostics
   :id "diagnostics"
   :intent '(ide)
   :layer 'dev
   :description "Get flycheck/flymake diagnostics for a file or all project buffers"
   :read-only t
   :server-id anvil-ide--server-id))

(defun anvil-ide-disable ()
  "Unregister IDE tools."
  (dolist (id '("xref_find_references" "xref_find_apropos" "project_info"
                "imenu_list_symbols" "treesit_info" "diagnostics"))
    (anvil-server-unregister-tool id anvil-ide--server-id)))

(provide 'anvil-ide)
;;; anvil-ide.el ends here
