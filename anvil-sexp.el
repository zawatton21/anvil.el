;;; anvil-sexp.el --- Reader-based structural edits for elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 1 of Doc 12 (docs/design/12-sexp-edits.org).  AST-aware
;; structural editing for Emacs Lisp, built on the native reader
;; rather than tree-sitter.  All MCP tools default to preview mode;
;; writing requires the explicit `:apply t' argument.

;;; Code:

(require 'anvil-server)
(require 'cl-lib)
(require 'pp)
(require 'subr-x)
(require 'bytecomp)

;; Dynamic variables owned by `checkdoc'; declared so that the
;; byte compiler treats the `let' bindings in
;; `anvil-sexp--run-checkdoc' as dynamic (they are read by
;; `checkdoc-file' internally).  The feature itself is loaded
;; lazily inside the function to avoid a top-level recursive
;; require when users load anvil-sexp during an active checkdoc
;; session.
(defvar checkdoc-diagnostic-buffer)
(defvar checkdoc-autofix-flag)
(defvar checkdoc-spellcheck-documentation-flag)
(defvar checkdoc-verb-check-experimental-flag)


;;;; --- group + constants ---------------------------------------------------

(defgroup anvil-sexp nil
  "Reader-based structural edits for Emacs Lisp."
  :group 'anvil
  :prefix "anvil-sexp-")

(defconst anvil-sexp--server-id "emacs-eval"
  "Server ID under which anvil-sexp MCP tools are registered.")

(defconst anvil-sexp-placeholder "|anvil-sexp-hole|"
  "Token substituted with the original sexp in `anvil-sexp-wrap-form' wrappers.")

(defconst anvil-sexp--defining-forms
  '(defun defmacro defvar defcustom defconst defsubst defalias
    cl-defun cl-defmacro cl-defgeneric cl-defmethod cl-defstruct
    define-minor-mode define-derived-mode define-globalized-minor-mode
    defgroup deftheme defface define-obsolete-function-alias
    define-error)
  "Top-level forms recognized as carrying a name in their second position.")


;;;; --- form inspection helpers --------------------------------------------

(defun anvil-sexp--form-kind (sexp)
  "Return the operator symbol of top-level SEXP, or nil if not a list."
  (and (consp sexp) (car sexp)))

(defun anvil-sexp--form-name (sexp)
  "Return the defined name carried by SEXP, or nil if none.
Recognizes the operators in `anvil-sexp--defining-forms'."
  (when (and (consp sexp)
             (memq (car sexp) anvil-sexp--defining-forms)
             (consp (cdr sexp)))
    (let ((name (cadr sexp)))
      (cond ((symbolp name) name)
            ((stringp name) (intern name))
            (t nil)))))


;;;; --- reader primitives --------------------------------------------------

(defun anvil-sexp--with-elisp-syntax (fn)
  "Call FN in a temp buffer using the elisp syntax table.
Ensures `;' comments and string escapes behave correctly during
`read' and `forward-sexp'."
  (with-temp-buffer
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (funcall fn)))

(defun anvil-sexp--skip-to-form ()
  "Move point past whitespace and comments.  Return point or nil at eof."
  (forward-comment most-positive-fixnum)
  (if (eobp) nil (point)))

(defun anvil-sexp--read-current-buffer ()
  "Return list of top-level form plists for the current buffer.
Each element is (:sexp SEXP :start POS :end POS :kind SYMBOL
:name SYMBOL-OR-NIL).  Positions are 1-based buffer points."
  (goto-char (point-min))
  (let (forms)
    (while (anvil-sexp--skip-to-form)
      (let* ((start (point))
             (sexp (condition-case err
                       (read (current-buffer))
                     (end-of-file (signal (car err) (cdr err)))
                     (invalid-read-syntax
                      (error "Invalid elisp at position %d: %s"
                             start (error-message-string err)))))
             (end (point)))
        (push (list :sexp sexp
                    :start start
                    :end end
                    :kind (anvil-sexp--form-kind sexp)
                    :name (anvil-sexp--form-name sexp))
              forms)))
    (nreverse forms)))

(defun anvil-sexp--read-file (path)
  "Return list of top-level form plists for PATH."
  (anvil-sexp--with-elisp-syntax
   (lambda ()
     (insert-file-contents path)
     (anvil-sexp--read-current-buffer))))

(defun anvil-sexp--find-form-by-name (path name &optional kind)
  "Return form plist in PATH whose :name equals NAME.
When KIND is non-nil restrict to forms whose :kind matches.
Returns nil when no form matches."
  (let ((target (if (symbolp name) name (intern name))))
    (cl-find-if (lambda (f)
                  (and (eq (plist-get f :name) target)
                       (or (null kind)
                           (eq (plist-get f :kind)
                               (if (symbolp kind) kind (intern kind))))))
                (anvil-sexp--read-file path))))

(defun anvil-sexp--file-substring (path beg end)
  "Return the byte-for-byte substring of PATH between BEG and END (1-based)."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-substring-no-properties beg end)))


;;;; --- plan helpers -------------------------------------------------------

(defun anvil-sexp--unified-diff (file beg end replacement)
  "Build a small unified-diff-style preview for a single-range edit."
  (let* ((old (anvil-sexp--file-substring file beg end))
         (lines-old (split-string old "\n"))
         (lines-new (split-string replacement "\n"))
         (out (list (format "--- %s" file)
                    (format "+++ %s" file)
                    (format "@@ range %d-%d @@" beg end))))
    (dolist (l lines-old)
      (push (concat "-" l) out))
    (dolist (l lines-new)
      (push (concat "+" l) out))
    (mapconcat #'identity (nreverse out) "\n")))

(defun anvil-sexp--make-plan (file beg end replacement reason)
  "Build an edit plan for a single file / single range."
  (list :ops (list (list :file file
                         :range (cons beg end)
                         :replacement replacement
                         :reason reason))
        :summary (format "%s: 1 op on %s" reason (file-name-nondirectory file))
        :diff-preview (anvil-sexp--unified-diff file beg end replacement)))

(defun anvil-sexp--apply-plan (plan)
  "Write every op in PLAN to disk.  Return PLAN with :applied-at added.
Ops on the same file are applied back-to-front so earlier ranges stay
valid after later replacements shift text."
  (let* ((ops (plist-get plan :ops))
         (by-file (make-hash-table :test 'equal)))
    (dolist (op ops)
      (push op (gethash (plist-get op :file) by-file)))
    (maphash
     (lambda (file file-ops)
       (let ((sorted (sort (copy-sequence file-ops)
                           (lambda (a b)
                             (> (car (plist-get a :range))
                                (car (plist-get b :range)))))))
         (with-temp-buffer
           (insert-file-contents file)
           (dolist (op sorted)
             (let ((r (plist-get op :range)))
               (delete-region (car r) (cdr r))
               (goto-char (car r))
               (insert (plist-get op :replacement))))
           (write-region (point-min) (point-max) file nil 'silent))))
     by-file)
    (append plan (list :applied-at (format-time-string "%FT%T%z")))))

(defun anvil-sexp--truthy (v)
  "Non-nil when V is a non-empty truthy MCP value.
Treats the strings \"nil\", \"false\", \"0\", \"\", and the elisp
symbol nil as falsy; everything else is truthy.  Used to unify
the apply/byte_compile/checkdoc/all argument shapes coming over
the MCP wire."
  (cond ((null v) nil)
        ((stringp v)
         (not (member (downcase v) '("" "nil" "false" "0"))))
        (t t)))

(defun anvil-sexp--maybe-apply (plan apply)
  "Apply PLAN when APPLY is truthy; otherwise return preview PLAN."
  (if (anvil-sexp--truthy apply)
      (anvil-sexp--apply-plan plan)
    plan))


;;;; --- form plist serialization ------------------------------------------

(defun anvil-sexp--form-public (f)
  "Return the JSON-friendly view of a form plist F.
Drops the raw :sexp (returns its printed form as :text) and
converts symbols to strings so downstream consumers can treat the
output uniformly."
  (list :kind (and (plist-get f :kind) (symbol-name (plist-get f :kind)))
        :name (and (plist-get f :name) (symbol-name (plist-get f :name)))
        :start (plist-get f :start)
        :end (plist-get f :end)
        :text (prin1-to-string (plist-get f :sexp))))


;;;; --- read-only tool implementations -------------------------------------

(defun anvil-sexp--tool-read-file (file)
  "Return the list of top-level forms in FILE.

MCP Parameters:
  file  - Absolute path to an .el source file.

Returns a plist list; each entry carries :kind, :name, :start,
:end, and :text (the form's printed representation).  Strings and
comments are skipped by the reader; their positions are not
reported in this phase."
  (anvil-server-with-error-handling
   (unless (file-readable-p file)
     (error "anvil-sexp-read-file: cannot read %s" file))
   (mapcar #'anvil-sexp--form-public (anvil-sexp--read-file file))))

(defun anvil-sexp--tool-surrounding-form (file point &optional kind)
  "Return the innermost top-level form in FILE containing POINT.

MCP Parameters:
  file  - Absolute path to an .el source file.
  point - Buffer point (1-based integer, passed as integer or string).
  kind  - Optional operator name (string, e.g. \"defun\") to restrict
          the match.  When omitted, returns whichever top-level form
          encloses the point.

Returns the same plist shape as `sexp-read-file' entries, or nil
when the point is between top-level forms."
  (anvil-server-with-error-handling
   (unless (file-readable-p file)
     (error "anvil-sexp-surrounding-form: cannot read %s" file))
   (let* ((pos (cond ((integerp point) point)
                     ((and (stringp point) (string-match "\\`[0-9]+\\'" point))
                      (string-to-number point))
                     (t (error "point must be an integer, got %S" point))))
          (want-kind (and (stringp kind) (not (string-empty-p kind)) (intern kind)))
          (forms (anvil-sexp--read-file file))
          (hit (cl-find-if
                (lambda (f) (and (<= (plist-get f :start) pos)
                                 (<= pos (plist-get f :end))
                                 (or (null want-kind)
                                     (eq want-kind (plist-get f :kind)))))
                forms)))
     (and hit (anvil-sexp--form-public hit)))))

(defun anvil-sexp--tool-macroexpand (file name &optional all)
  "Macroexpand the form named NAME in FILE.

MCP Parameters:
  file - Absolute path to an .el source file.
  name - Name (string) of a top-level defining form.
  all  - Non-nil (any truthy string) to run `macroexpand-all'
         instead of the default single-step `macroexpand-1'.

Returns (:name :expanded :original).  The expansion is produced
in the ambient Emacs process — macros that are not yet loaded
will pass through unchanged.  Callers needing full expansion
against a fresh environment should first load the file's
dependencies."
  (anvil-server-with-error-handling
   (let* ((form (anvil-sexp--find-form-by-name file name)))
     (unless form
       (error "anvil-sexp-macroexpand: no form named %s in %s" name file))
     (let* ((sexp (plist-get form :sexp))
            (expanded (if (anvil-sexp--truthy all)
                          (macroexpand-all sexp)
                        (macroexpand-1 sexp))))
       (list :name (symbol-name (plist-get form :name))
             :expanded (with-output-to-string
                         (let ((print-level nil)
                               (print-length nil))
                           (pp expanded)))
             :original (prin1-to-string sexp))))))


;;;; --- edit tool implementations ------------------------------------------

(defun anvil-sexp--parse-sexp-string (s label)
  "Read S as a single elisp form.  LABEL is used in the error message."
  (condition-case err
      (let* ((res (read-from-string s))
             (obj (car res))
             (consumed (cdr res))
             (trim (substring s consumed))
             (rest (string-trim trim)))
        (unless (string-empty-p rest)
          (error "%s: unexpected trailing input after position %d: %S"
                 label consumed rest))
        obj)
    (end-of-file
     (error "%s: unterminated form in %S" label s))
    (invalid-read-syntax
     (error "%s: invalid syntax — %s" label (error-message-string err)))))

(defun anvil-sexp--tool-replace-defun (file name new_form &optional apply)
  "Replace the top-level form named NAME in FILE with NEW_FORM.

MCP Parameters:
  file     - Absolute path to an .el source file.
  name     - Name of the form to replace (string).
  new_form - Replacement form as elisp source text (string).  Must
             parse as exactly one sexp.  It need not be a defun —
             any top-level form is accepted.
  apply    - Non-nil (any truthy string) to write the change to
             disk.  Default preview-only.

Returns an edit plan plist (:ops :summary :diff-preview), with
:applied-at set when the change was written.  Refuses when no
form with NAME is found."
  (anvil-server-with-error-handling
   (unless (file-readable-p file)
     (error "anvil-sexp-replace-defun: cannot read %s" file))
   (let* ((form (anvil-sexp--find-form-by-name file name)))
     (unless form
       (error "anvil-sexp-replace-defun: no form named %s in %s" name file))
     ;; Validate replacement parses as one sexp
     (anvil-sexp--parse-sexp-string new_form "replace-defun new_form")
     (let* ((beg (plist-get form :start))
            (end (plist-get form :end))
            (plan (anvil-sexp--make-plan
                   file beg end new_form
                   (format "replace-defun %s" name))))
       (anvil-sexp--maybe-apply plan apply)))))

(defun anvil-sexp--sexp-bounds-at (point)
  "Return (BEG . END) of the sexp surrounding POINT in the current buffer.
Uses `bounds-of-thing-at-point' and falls back to `up-list' when
POINT lies directly on whitespace between forms."
  (save-excursion
    (goto-char point)
    (or (bounds-of-thing-at-point 'sexp)
        (progn
          (ignore-errors (backward-up-list))
          (when (looking-at-p "(")
            (let ((beg (point)))
              (forward-sexp)
              (cons beg (point))))))))

(defun anvil-sexp--tool-wrap-form (file point wrapper &optional apply)
  "Wrap the sexp at POINT in FILE with WRAPPER.

MCP Parameters:
  file    - Absolute path to an .el source file.
  point   - Buffer point (1-based) inside the sexp to wrap.
  wrapper - Elisp source text containing the placeholder token
            `|anvil-sexp-hole|'.  Example:
              \"(when cond |anvil-sexp-hole|)\"
            The original sexp text is substituted for the placeholder.
  apply   - Non-nil (any truthy string) to write the change.

Returns an edit plan plist.  Refuses when the wrapper does not
contain the placeholder, when the wrapper does not parse, or when
POINT lies outside any sexp."
  (anvil-server-with-error-handling
   (unless (file-readable-p file)
     (error "anvil-sexp-wrap-form: cannot read %s" file))
   (unless (string-match-p (regexp-quote anvil-sexp-placeholder) wrapper)
     (error "anvil-sexp-wrap-form: wrapper must contain %s"
            anvil-sexp-placeholder))
   (anvil-sexp--parse-sexp-string
    (replace-regexp-in-string
     (regexp-quote anvil-sexp-placeholder) "nil" wrapper t t)
    "wrap-form wrapper")
   (let ((pos (cond ((integerp point) point)
                    ((and (stringp point) (string-match "\\`[0-9]+\\'" point))
                     (string-to-number point))
                    (t (error "point must be an integer, got %S" point)))))
     (anvil-sexp--with-elisp-syntax
      (lambda ()
        (insert-file-contents file)
        (let ((bounds (anvil-sexp--sexp-bounds-at pos)))
          (unless bounds
            (error "anvil-sexp-wrap-form: no sexp at point %d" pos))
          (let* ((beg (car bounds))
                 (end (cdr bounds))
                 (original (buffer-substring-no-properties beg end))
                 (replacement (replace-regexp-in-string
                               (regexp-quote anvil-sexp-placeholder)
                               original wrapper t t))
                 (plan (anvil-sexp--make-plan
                        file beg end replacement
                        (format "wrap-form at %d" pos))))
            (anvil-sexp--maybe-apply plan apply))))))))


;;;; --- verify -------------------------------------------------------------

(defun anvil-sexp--parse-bc-log (log-text file)
  "Parse byte-compile LOG-TEXT and return a list of diagnostic plists.
Filters entries by FILE's basename so logs from prior runs don't
leak in."
  (let ((base (file-name-nondirectory file))
        (diags '()))
    (with-temp-buffer
      (insert log-text)
      (goto-char (point-min))
      (while (re-search-forward
              "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\):\\s-*\\(Warning\\|Error\\):\\s-*\\(.*\\)$"
              nil t)
        (let ((path (match-string 1))
              (line (string-to-number (match-string 2)))
              (col  (string-to-number (match-string 3)))
              (kind-str (match-string 4))
              (msg (match-string 5)))
          (when (string-equal (file-name-nondirectory path) base)
            (push (list :kind (if (equal kind-str "Error") 'error 'warning)
                        :source 'byte-compile
                        :line line :column col :message msg)
                  diags)))))
    (nreverse diags)))

(defun anvil-sexp--run-byte-compile (file)
  "Byte-compile FILE to a temp .elc, return (:ok :diagnostics)."
  (let* ((tmp-elc (make-temp-file "anvil-sexp-bc-" nil ".elc"))
         (log-buf (get-buffer-create "*anvil-sexp-bc*"))
         (ok nil))
    (unwind-protect
        (progn
          (with-current-buffer log-buf (erase-buffer))
          (let ((byte-compile-log-buffer (buffer-name log-buf))
                (byte-compile-dest-file-function (lambda (_) tmp-elc))
                (byte-compile-verbose nil))
            (condition-case err
                (setq ok (byte-compile-file file))
              (error
               (with-current-buffer log-buf
                 (goto-char (point-max))
                 (insert (format "\n%s:1:1:Error: %s\n"
                                 file (error-message-string err)))))))
          (let ((diags (anvil-sexp--parse-bc-log
                        (with-current-buffer log-buf (buffer-string))
                        file)))
            (list :ok (and ok
                           (null (cl-find-if
                                  (lambda (d) (eq (plist-get d :kind) 'error))
                                  diags)))
                  :diagnostics diags)))
      (when (file-exists-p tmp-elc) (delete-file tmp-elc))
      (when (buffer-live-p log-buf) (kill-buffer log-buf)))))

(defun anvil-sexp--run-checkdoc (file)
  "Run `checkdoc-file' on FILE, return list of diagnostic plists."
  (require 'checkdoc)
  (let ((warn-buf (get-buffer-create "*anvil-sexp-cd*"))
        (diags '()))
    (unwind-protect
        (progn
          (with-current-buffer warn-buf (erase-buffer))
          (let ((checkdoc-diagnostic-buffer (buffer-name warn-buf))
                (checkdoc-autofix-flag 'never)
                (checkdoc-spellcheck-documentation-flag nil)
                (checkdoc-verb-check-experimental-flag nil))
            (condition-case _
                (checkdoc-file file)
              (error nil)))
          (with-current-buffer warn-buf
            (goto-char (point-min))
            (while (re-search-forward
                    "^\\(.*?\\):\\([0-9]+\\):\\s-*\\(.*\\)$"
                    nil t)
              (let ((path (match-string 1))
                    (line (string-to-number (match-string 2)))
                    (msg (match-string 3)))
                (when (string-equal (file-name-nondirectory path)
                                    (file-name-nondirectory file))
                  (push (list :kind 'warning
                              :source 'checkdoc
                              :line line :message msg)
                        diags))))))
      (when (buffer-live-p warn-buf) (kill-buffer warn-buf)))
    (nreverse diags)))

(defun anvil-sexp--tool-verify (file &optional byte_compile checkdoc)
  "Run diagnostic checks on FILE and return aggregated warnings / errors.

MCP Parameters:
  file          - Absolute path to an .el source file.
  byte_compile  - Include byte-compile diagnostics.  Default t;
                  pass \"nil\" to skip.
  checkdoc      - Include checkdoc diagnostics.  Default t; pass
                  \"nil\" to skip.

Returns (:file :passed :diagnostics).  `:passed' is non-nil when
no entry has :kind \\='error.  This Phase-1 implementation does not
perform sandboxed loading; use `anvil-offload' integration in a
later phase when needed."
  (anvil-server-with-error-handling
   (unless (file-readable-p file)
     (error "anvil-sexp-verify: cannot read %s" file))
   (let* (;; Default ON for both checks; opt out with a falsy string.
          (do-bc (or (null byte_compile) (anvil-sexp--truthy byte_compile)))
          (do-cd (or (null checkdoc) (anvil-sexp--truthy checkdoc)))
          (diags '())
          (bc-ok t))
     (when do-bc
       (let ((bc (anvil-sexp--run-byte-compile file)))
         (setq bc-ok (plist-get bc :ok))
         (setq diags (append diags (plist-get bc :diagnostics)))))
     (when do-cd
       (setq diags (append diags (anvil-sexp--run-checkdoc file))))
     (list :file file
           :passed (and bc-ok
                        (null (cl-find-if
                               (lambda (d) (eq (plist-get d :kind) 'error))
                               diags)))
           :diagnostics diags))))


;;;; --- module lifecycle ---------------------------------------------------

(defun anvil-sexp--register-tools ()
  "Register sexp-* MCP tools under `anvil-sexp--server-id'."
  (anvil-server-register-tool
   #'anvil-sexp--tool-read-file
   :id "sexp-read-file"
   :server-id anvil-sexp--server-id
   :description
   "List every top-level form in an elisp source file.  Each entry
carries :kind (operator name), :name (defined symbol if any),
:start, :end, and :text (the form's printed representation).  The
reader skips strings and comments, so no substring in a docstring
or comment can leak into the result."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-sexp--tool-surrounding-form
   :id "sexp-surrounding-form"
   :server-id anvil-sexp--server-id
   :description
   "Return the innermost top-level form whose range contains the
given point.  Optional kind filter restricts the match to one
operator (e.g. \"defun\")."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-sexp--tool-macroexpand
   :id "sexp-macroexpand"
   :server-id anvil-sexp--server-id
   :description
   "Macroexpand a named top-level form and return its expansion as a
pretty-printed string.  Default is macroexpand-1; pass all=t for
macroexpand-all.  Expansion uses the ambient Emacs process, so
macros not yet loaded pass through unchanged."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-sexp--tool-verify
   :id "sexp-verify"
   :server-id anvil-sexp--server-id
   :description
   "Run byte-compile and checkdoc on an elisp file and return
structured diagnostics.  :passed is non-nil when no entry has
:kind \\='error.  Pass byte_compile=nil or checkdoc=nil to skip a
check."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-sexp--tool-replace-defun
   :id "sexp-replace-defun"
   :server-id anvil-sexp--server-id
   :description
   "Replace the top-level form named NAME in FILE with NEW_FORM
source text.  Returns an edit plan (:ops :summary :diff-preview);
writing requires apply=t explicitly.  Refuses when the new form
does not parse or when no form matches the given name.")

  (anvil-server-register-tool
   #'anvil-sexp--tool-wrap-form
   :id "sexp-wrap-form"
   :server-id anvil-sexp--server-id
   :description
   "Wrap the sexp surrounding POINT in FILE with WRAPPER source text.
WRAPPER must contain the placeholder token `|anvil-sexp-hole|';
the original sexp replaces the placeholder.  Preview by default;
apply=t writes to disk."))

(defun anvil-sexp--unregister-tools ()
  "Remove every sexp-* MCP tool from the shared server."
  (dolist (id '("sexp-read-file"
                "sexp-surrounding-form"
                "sexp-macroexpand"
                "sexp-verify"
                "sexp-replace-defun"
                "sexp-wrap-form"))
    (anvil-server-unregister-tool id anvil-sexp--server-id)))

;;;###autoload
(defun anvil-sexp-enable ()
  "Register sexp-* MCP tools."
  (interactive)
  (anvil-sexp--register-tools))

(defun anvil-sexp-disable ()
  "Unregister sexp-* MCP tools."
  (interactive)
  (anvil-sexp--unregister-tools))

(provide 'anvil-sexp)
;;; anvil-sexp.el ends here
