;;; anvil-sexp.el --- Reader-based structural edits for elisp  -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

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
(require 'anvil-defs nil t)

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

(defconst anvil-sexp--function-defining-forms
  '(defun defmacro defsubst defalias
    cl-defun cl-defmacro cl-defgeneric cl-defmethod
    define-minor-mode define-derived-mode define-globalized-minor-mode
    define-obsolete-function-alias)
  "Subset of defining forms whose name refers to a function or macro.
`anvil-sexp-replace-defun' restricts matching to this set so a
caller asking to replace foo the function does not accidentally
hit foo the variable.")


;;;; --- form inspection helpers --------------------------------------------

(defun anvil-sexp--form-kind (sexp)
  "Return the operator symbol of top-level SEXP, or nil if not a list."
  (and (consp sexp) (car sexp)))

(defun anvil-sexp--form-name (sexp)
  "Return the defined name carried by SEXP, or nil if none.
Recognizes the operators in `anvil-sexp--defining-forms'.  Also
handles `cl-defstruct' and similar forms whose name position may
be a list of the shape (NAME :option value ...), returning the
leading NAME."
  (when (and (consp sexp)
             (memq (car sexp) anvil-sexp--defining-forms)
             (consp (cdr sexp)))
    (let ((name (cadr sexp)))
      (cond ((symbolp name) name)
            ((stringp name) (intern name))
            ((and (consp name) (symbolp (car name))) (car name))
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

(defun anvil-sexp--cookie-start-before (form-start)
  "Return start-of-line of the topmost `;;;###' cookie block before FORM-START.
Returns nil when no cookie line directly precedes FORM-START on
its own line.  A cookie block is any run of consecutive lines each
beginning with `;;;###' (e.g. `;;;###autoload', `;;;###tramp-autoload')
whose last line ends immediately before FORM-START's line."
  (save-excursion
    (goto-char form-start)
    (beginning-of-line)
    ;; If form-start is not at column 0 we can't attribute a cookie
    ;; above — the form shares its line with something else.
    (when (= (point) form-start)
      (let ((cookie-top nil))
        (while (and (> (point) (point-min))
                    (save-excursion
                      (forward-line -1)
                      (looking-at-p "^;;;###")))
          (forward-line -1)
          (setq cookie-top (point)))
        cookie-top))))

(defun anvil-sexp--read-current-buffer ()
  "Return list of top-level form plists for the current buffer.
Each element is (:sexp SEXP :start POS :end POS :kind SYMBOL
:name SYMBOL-OR-NIL :form-start POS).  :start includes any
`;;;###...' autoload cookies directly above the form; :form-start
points at the opening paren itself."
  (goto-char (point-min))
  (let (forms)
    (while (anvil-sexp--skip-to-form)
      (let* ((form-start (point))
             (sexp (condition-case err
                       (read (current-buffer))
                     (end-of-file (signal (car err) (cdr err)))
                     (invalid-read-syntax
                      (error "Invalid elisp at position %d: %s"
                             form-start (error-message-string err)))))
             (end (point))
             (cookie-start (anvil-sexp--cookie-start-before form-start))
             (start (or cookie-start form-start)))
        (push (list :sexp sexp
                    :start start
                    :form-start form-start
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
KIND may be a single symbol, a string, or a list of symbols
(matches if the form's kind is `memq'-in the list)."
  (let ((target (if (symbolp name) name (intern name)))
        (kinds (cond ((null kind) nil)
                     ((listp kind) kind)
                     ((symbolp kind) (list kind))
                     ((stringp kind) (list (intern kind))))))
    (cl-find-if (lambda (f)
                  (and (eq (plist-get f :name) target)
                       (or (null kinds)
                           (memq (plist-get f :kind) kinds))))
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

(defun anvil-sexp--assert-ops-non-overlapping (ops)
  "Signal an error when any two OPS on the same file overlap.
Ops that abut exactly (a-end == b-start) are allowed; any strict
overlap is refused so back-to-front application cannot silently
use stale coordinates."
  (let ((by-file (make-hash-table :test 'equal)))
    (dolist (op ops)
      (push op (gethash (plist-get op :file) by-file)))
    (maphash
     (lambda (file file-ops)
       (let ((sorted (sort (copy-sequence file-ops)
                           (lambda (a b) (< (car (plist-get a :range))
                                            (car (plist-get b :range)))))))
         (cl-loop for (a b) on sorted while b do
                  (let ((a-end (cdr (plist-get a :range)))
                        (b-beg (car (plist-get b :range))))
                    (when (> a-end b-beg)
                      (error "anvil-sexp: overlapping ops in %s: %S vs %S"
                             file (plist-get a :range)
                             (plist-get b :range)))))))
     by-file)))

(defun anvil-sexp--apply-plan (plan)
  "Write every op in PLAN to disk.  Return PLAN with :applied-at added.
Ops on the same file are applied back-to-front so earlier ranges stay
valid after later replacements shift text.  Refuses when any two
ops on the same file overlap."
  (let* ((ops (plist-get plan :ops))
         (by-file (make-hash-table :test 'equal)))
    (anvil-sexp--assert-ops-non-overlapping ops)
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
  "Non-nil when V is a truthy MCP value.
Every shape a JSON/elisp bridge can produce for \"false\" is
treated as falsy:
  - elisp nil
  - :json-false / :false (json.el / other JSON readers)
  - strings \"\", \"nil\", \"false\", \"0\" (case-insensitive,
    surrounding whitespace trimmed)
Any other value is truthy.  Used to unify the
apply/byte_compile/checkdoc/all argument shapes coming across the
MCP wire."
  (cond ((null v) nil)
        ((or (eq v :json-false) (eq v :false)) nil)
        ((stringp v)
         (let ((norm (downcase (string-trim v))))
           (not (member norm '("" "nil" "false" "0")))))
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
   (let* ((form (anvil-sexp--find-form-by-name
                 file name anvil-sexp--function-defining-forms)))
     (unless form
       (error "anvil-sexp-replace-defun: no function-like form named %s in %s (defvar/defcustom are not targets of this tool)"
              name file))
     ;; Validate replacement parses as one sexp
     (anvil-sexp--parse-sexp-string new_form "replace-defun new_form")
     (let* ((beg (plist-get form :start))
            (end (plist-get form :end))
            (plan (anvil-sexp--make-plan
                   file beg end new_form
                   (format "replace-defun %s" name))))
       (anvil-sexp--maybe-apply plan apply)))))

(defun anvil-sexp--sexp-bounds-at (point)
  "Return (BEG . END) of the innermost sexp at POINT in the current buffer.
Uses `bounds-of-thing-at-point' and falls back to the enclosing list
when POINT lies directly on whitespace between forms."
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
  "Wrap the innermost sexp at POINT in FILE with WRAPPER.
When POINT lies on inter-form whitespace, fall back to the enclosing list.

MCP Parameters:
  file    - Absolute path to an .el source file.
  point   - Buffer point (1-based) at or inside the sexp to wrap.
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


;;;; --- phase 2a: project-scope rename / replace-call -----------------------

;; Reader-only backend.  Each .el file in scope is parsed via
;; `anvil-sexp--read-file' to get the top-level forms with their byte
;; ranges, and each range is text-scanned for the target symbol with
;; `syntax-ppss' filtering out strings and comments.  Phase 2b
;; (committed 2026-04-20) adds a Doc 11 `anvil-defs' fast path: when
;; an index is available it narrows the candidate file set before the
;; reader scan, turning O(project) into O(touched-files); each
;; candidate file still falls through to the reader so the emitted
;; plan is byte-equal with or without the index.

(defgroup anvil-sexp-project nil
  "Project-scope settings for `anvil-sexp' Phase 2a rename."
  :group 'anvil-sexp)

(defcustom anvil-sexp-project-exclude-patterns
  '("/\\.git/"
    "/\\.claude/"
    "/node_modules/"
    "/tests/fixtures/"
    "/worktrees/"
    "/dist/"
    "/build/"
    "\\.elc\\'")
  "Regexps of file paths to exclude from project-wide sexp scans."
  :type '(repeat regexp)
  :group 'anvil-sexp-project)

(defcustom anvil-sexp-use-index-backend t
  "When non-nil, use `anvil-defs' to narrow project rename scans.
The index only chooses candidate files; each chosen file is still
rescanned via the reader backend so the resulting edit plan stays
byte-for-byte identical to the full reader walk.  Set to nil to
force the pure Phase 2a path (useful for backend-parity testing
and when debugging index staleness)."
  :type 'boolean
  :group 'anvil-sexp-project)

(defun anvil-sexp--project-root (&optional hint)
  "Return the project root containing HINT (or `default-directory').
Uses the nearest `.git' ancestor; falls back to HINT itself."
  (let ((d (expand-file-name (or hint default-directory))))
    (or (ignore-errors
          (locate-dominating-file d ".git"))
        (file-name-directory d))))

(defun anvil-sexp--index-refresh-project ()
  "Ingest / re-ingest every .el file under the project whose on-disk
mtime differs from the indexed mtime.  Also ingests files the
index has never seen.  Safe to call repeatedly; touches only files
whose state has actually changed.  Returns the list of files that
were (re-)ingested.

This is the guard against codex's stale-index hazard: if a file
started mentioning the target symbol since the last rebuild, its
row is absent from `refs' and the narrowing query would skip it.
Walking every .el on disk with a cheap mtime compare closes the
gap while preserving the fast path for the common case where
nothing has changed (O(project) stat calls is ~ms)."
  (when (and (featurep 'anvil-defs)
             (fboundp 'anvil-defs--ensure-db)
             (fboundp 'anvil-defs--collect-files)
             (fboundp 'anvil-defs--indexed-mtime)
             (fboundp 'anvil-defs--current-mtime)
             (fboundp 'anvil-defs--ingest-file))
    (let* ((db (anvil-defs--ensure-db))
           (root (anvil-sexp--project-root))
           (files (anvil-defs--collect-files (list root)))
           (touched nil))
      (dolist (f files)
        (let ((indexed (anvil-defs--indexed-mtime db f))
              (current (anvil-defs--current-mtime f)))
          (when (and current (or (null indexed) (> current indexed)))
            (anvil-defs--ingest-file db f)
            (push f touched))))
      (nreverse touched))))

(defun anvil-sexp--index-candidate-files (sym)
  "Return the set of .el files the Doc 11 index reports as touching SYM.
Returns nil when `anvil-defs' is not loaded, the index is empty,
or `anvil-sexp-use-index-backend' is non-nil but false.  Includes
files from both `anvil-defs-references' (call / quote / symbol /
var sites) and `anvil-defs-search' (definition name sites) so a
never-referenced defun-name is still surfaced.  Before querying,
a project-wide mtime refresh ensures files modified (or newly
added) since the last rebuild are re-ingested — otherwise the
query would silently miss them."
  (when (and anvil-sexp-use-index-backend
             (featurep 'anvil-defs)
             (fboundp 'anvil-defs-index-status)
             (fboundp 'anvil-defs-references)
             (fboundp 'anvil-defs-search)
             (> (or (plist-get (anvil-defs-index-status) :files) 0) 0))
    (anvil-sexp--index-refresh-project)
    (let* ((name (if (symbolp sym) (symbol-name sym) sym))
           (files
            (delete-dups
             (delq nil
                   (append
                    (mapcar (lambda (h) (plist-get h :file))
                            (anvil-defs-references name :limit 100000))
                    (mapcar (lambda (h) (plist-get h :file))
                            (anvil-defs-search name :limit 1000)))))))
      files)))

(defun anvil-sexp--project-el-files (root)
  "List .el source files under ROOT, respecting
`anvil-sexp-project-exclude-patterns'."
  (let ((all (directory-files-recursively root "\\.el\\'" nil)))
    (cl-remove-if
     (lambda (f)
       (cl-some (lambda (re) (string-match-p re f))
                anvil-sexp-project-exclude-patterns))
     all)))

(defun anvil-sexp--scan-symbol-refs (file name)
  "Return a list of reference plists for NAME in FILE.
Each entry is (:kind KIND :start POS :end POS) where KIND is one
of `defun-name', `call', `quote', or `symbol'.  Strings and
comments are skipped via `syntax-ppss'.  Phase 2a approximation:
`var' and `defun-name' collapse into either `defun-name' (when
the site is the second slot of a known defining form — detected
via the form's parsed metadata) or `symbol' otherwise; Phase 2b
will refine via the Doc 11 index."
  (let ((sym-name (if (symbolp name) (symbol-name name) name))
        (target-sym (if (symbolp name) name (intern name))))
    (anvil-sexp--with-elisp-syntax
     (lambda ()
       (insert-file-contents file)
       (let* ((forms (anvil-sexp--read-current-buffer))
              (regex (concat "\\_<" (regexp-quote sym-name) "\\_>"))
              (defun-name-positions (make-hash-table :test 'eql))
              (hits nil))
         ;; Pass 1: mark defun-name positions via the sexp metadata.
         ;; Restricted to function-like forms — variable-binding forms
         ;; (defvar / defcustom / etc) must fall through to Pass 2 so
         ;; their name sites are classified as `var' by the context
         ;; walker, matching Doc 12 Phase 2a's four-kind taxonomy.
         (dolist (f forms)
           (when (and (eq (plist-get f :name) target-sym)
                      (memq (plist-get f :kind)
                            anvil-sexp--function-defining-forms))
             (save-excursion
               (goto-char (plist-get f :form-start))
               (when (looking-at-p "(")
                 (forward-char 1)
                 (forward-comment most-positive-fixnum)
                 ;; Skip operator token.
                 (when (ignore-errors (forward-sexp) t)
                   (forward-comment most-positive-fixnum)
                   (let ((tok-start (point)))
                     ;; The name may be either bare SYMBOL or a
                     ;; (SYMBOL :option ...) list for cl-defstruct-
                     ;; style forms.  Accept either prefix.
                     (when (or (looking-at (regexp-quote sym-name))
                               (and (looking-at-p "(")
                                    (save-excursion
                                      (forward-char 1)
                                      (forward-comment most-positive-fixnum)
                                      (looking-at (regexp-quote sym-name)))))
                       (let ((match-beg (if (looking-at-p "(")
                                            (save-excursion
                                              (forward-char 1)
                                              (forward-comment
                                               most-positive-fixnum)
                                              (point))
                                          tok-start)))
                         (puthash match-beg t defun-name-positions)
                         (push (list :kind 'defun-name
                                     :start match-beg
                                     :end (+ match-beg (length sym-name)))
                               hits)))))))))
         ;; Pass 2: general text scan, classify via char-before and
         ;; enclosing-list car.  Skip positions already counted as
         ;; defun-name.
         ;;
         ;; NOTE: `syntax-ppss' with an explicit POS moves point to
         ;; POS as a side effect (the underlying `parse-partial-sexp'
         ;; leaves point wherever it stopped), which would rewind the
         ;; `re-search-forward' walker and produce an infinite loop.
         ;; The `save-excursion' guard is load-bearing — do not drop.
         (goto-char (point-min))
         (while (re-search-forward regex nil t)
           (let* ((beg (match-beginning 0))
                  (end (match-end 0))
                  (ppss (save-excursion (syntax-ppss beg))))
             (unless (or (nth 3 ppss)   ; in string
                         (nth 4 ppss)   ; in comment
                         (gethash beg defun-name-positions))
               (push (list :kind (save-excursion
                                   (anvil-sexp--classify-ref-at beg))
                           :start beg
                           :end end)
                     hits))))
         (sort hits (lambda (a b) (< (plist-get a :start)
                                     (plist-get b :start)))))))))

(defconst anvil-sexp--var-binding-forms
  '(defvar defvar-local defcustom defconst defconst-local
    setq setq-local setf cl-letf cl-letf*)
  "Forms whose second (or alternating) slot is a variable-name binding.
Used by `anvil-sexp--classify-ref-at' to surface the `var' kind
required by Doc 12 Phase 2a.")

(defun anvil-sexp--parent-ctx (pos)
  "Return a plist describing POS's position inside its enclosing list.
Result: (:car OP-SYM :index N) or nil when POS is at top level.
OP-SYM is the first sexp of the enclosing list (nil if it is not
a bare symbol); N is the 0-based sexp index, where 0 means POS
sits on the operator token itself."
  (save-excursion
    (goto-char pos)
    (let ((ppss (syntax-ppss)))
      (when (> (nth 0 ppss) 0)
        (let* ((open (nth 1 ppss))
               (car-sym nil)
               (index 0))
          ;; Read the head of the enclosing list.
          (goto-char (1+ open))
          (forward-comment most-positive-fixnum)
          (ignore-errors
            (let ((s (read (current-buffer))))
              (when (symbolp s) (setq car-sym s))))
          ;; Count sexps strictly before POS within the list.  Each
          ;; skipped sexp advances INDEX; the first skip moves us off
          ;; the operator, so a POS on the operator returns INDEX=0.
          (goto-char (1+ open))
          (forward-comment most-positive-fixnum)
          (while (and (< (point) pos)
                      (let ((p (point)))
                        (ignore-errors (forward-sexp))
                        (forward-comment most-positive-fixnum)
                        (> (point) p)))
            (when (<= (point) pos)
              (setq index (1+ index))))
          (list :car car-sym :index index))))))

(defun anvil-sexp--classify-ref-at (pos)
  "Classify the elisp symbol occurrence starting at POS.
Returns one of `defun-name', `call', `quote', `var', or `symbol'.
Call only on positions already known not to be inside a string or
comment.  The implementation combines cheap `char-before' hints
with `anvil-sexp--parent-ctx' so that operator sites with
whitespace or comments between `(' and the head (for example
`( foo 1)' or `(;; hi\\n foo 1)') are still classified as
`call'."
  (let* ((prev (and (> pos (point-min)) (char-before pos)))
         (prev2 (and (> pos (1+ (point-min))) (char-before (1- pos)))))
    (cond
     ;; Reader-quote shorthand: 'foo or #'foo.
     ((and (eql prev ?') (eql prev2 ?#)) 'quote)
     ((eql prev ?') 'quote)
     (t
      (let* ((ctx (anvil-sexp--parent-ctx pos))
             (car-sym (plist-get ctx :car))
             (index (plist-get ctx :index)))
        (cond
         ((null ctx) 'symbol)
         ((memq car-sym '(quote function)) 'quote)
         ((eql index 0) 'call)
         ((and (eql index 1)
               (memq car-sym anvil-sexp--var-binding-forms))
          'var)
         (t 'symbol)))))))

(defun anvil-sexp--enclosing-list-car (pos)
  "Return the operator symbol of the list immediately enclosing POS.
Kept as a thin wrapper around `anvil-sexp--parent-ctx' for
backward compatibility with early Phase 2a call sites."
  (plist-get (anvil-sexp--parent-ctx pos) :car))

(defun anvil-sexp--resolve-scope (scope)
  "Return a list of .el file paths matching SCOPE.
SCOPE may be:
  - nil or \"project\"  -> every .el under the current project root
  - a directory path   -> every .el under that directory
  - a file path        -> that single file
  - a comma-separated string of file paths -> each as given
  - a list of paths    -> each as given"
  (cond
   ((or (null scope) (equal scope "") (equal scope "project"))
    (anvil-sexp--project-el-files (anvil-sexp--project-root)))
   ((listp scope) scope)
   ((and (stringp scope) (string-match-p "," scope))
    (mapcar #'string-trim (split-string scope ",")))
   ((stringp scope)
    (cond
     ((file-directory-p scope) (anvil-sexp--project-el-files scope))
     ((file-regular-p scope) (list scope))
     (t (error "anvil-sexp: scope %S does not exist" scope))))
   (t (error "anvil-sexp: unsupported scope shape %S" scope))))

(defun anvil-sexp--rename-plan (scope old new &optional kinds)
  "Build an edit plan renaming OLD to NEW across SCOPE.
KINDS restricts reference kinds (list of symbols; nil = all).
When SCOPE is the project default, prefer the Doc 11 index to
narrow the file set before the reader scan; results are
byte-identical either way since each candidate file still goes
through `anvil-sexp--scan-symbol-refs'."
  (let* ((old-name (if (symbolp old) (symbol-name old) old))
         (new-name (if (symbolp new) (symbol-name new) new))
         (project-scope-p (or (null scope)
                              (equal scope "")
                              (equal scope "project")))
         (index-files (and project-scope-p
                           (anvil-sexp--index-candidate-files old-name)))
         (files (or index-files (anvil-sexp--resolve-scope scope)))
         (kinds-filter (cond ((null kinds) nil)
                             ((listp kinds) kinds)
                             ((stringp kinds)
                              (mapcar #'intern
                                      (split-string kinds "[ ,]+" t)))))
         (ops nil)
         (touched-files 0)
         (total-hits 0))
    (unless (and (not (string-empty-p old-name))
                 (not (string-empty-p new-name)))
      (error "anvil-sexp-rename-symbol: OLD and NEW must be non-empty"))
    (dolist (file files)
      (let ((file-hits (anvil-sexp--scan-symbol-refs file old-name))
            (emitted 0))
        (dolist (h file-hits)
          (when (or (null kinds-filter)
                    (memq (plist-get h :kind) kinds-filter))
            (cl-incf emitted)
            (push (list :file file
                        :range (cons (plist-get h :start)
                                     (plist-get h :end))
                        :replacement new-name
                        :reason (format "rename-symbol %s->%s [%s]"
                                        old-name new-name
                                        (plist-get h :kind)))
                  ops)))
        (when (> emitted 0)
          (cl-incf touched-files)
          (cl-incf total-hits emitted))))
    (list :ops (nreverse ops)
          :summary (format "rename-symbol %s -> %s: %d sites across %d files"
                           old-name new-name total-hits touched-files)
          :diff-preview
          (format "# rename-symbol %s -> %s\n# %d sites, %d files\n"
                  old-name new-name total-hits touched-files))))

(defun anvil-sexp--tool-rename-symbol (old new &optional kinds scope apply)
  "Rename OLD to NEW across SCOPE.

MCP Parameters:
  old   - Current symbol name (string).
  new   - New symbol name (string).
  kinds - Optional space/comma list of kinds to include: any
          of \"defun-name\", \"call\", \"quote\", \"symbol\".
          Default is all.
  scope - \"project\" (default), a directory path, a file path,
          or a comma-separated list of file paths.
  apply - Truthy to write to disk.  Default preview.

Returns an edit plan plist."
  (anvil-server-with-error-handling
   (anvil-sexp--maybe-apply
    (anvil-sexp--rename-plan scope old new kinds)
    apply)))

(defun anvil-sexp--render-call-template (template args)
  "Substitute %1 %2 ... in TEMPLATE string with the printed forms of ARGS.
Each %N is replaced simultaneously rather than sequentially so a
value printed for %1 whose text contains `%2' is not rewritten
when %2's substitution pass runs.  Missing positional references
signal an error.  %10+ is supported — longer digit runs bind
tighter than shorter prefixes."
  (let ((max-arg 0))
    (let ((start 0))
      (while (string-match "%\\([0-9]+\\)" template start)
        (let ((n (string-to-number (match-string 1 template))))
          (when (> n max-arg) (setq max-arg n))
          (setq start (match-end 0)))))
    (when (> max-arg (length args))
      (error "replace-call template uses %%%d but only %d arg(s) available"
             max-arg (length args)))
    ;; Single-pass replacement: scan once, emit literal runs and
    ;; the N-th arg's printed form on each %<digits> match.
    (let ((i 0) (len (length template)) (out ""))
      (while (< i len)
        (let ((ch (aref template i)))
          (if (and (eq ch ?%)
                   (< (1+ i) len)
                   (let ((d (aref template (1+ i))))
                     (and (>= d ?0) (<= d ?9))))
              (let ((j (1+ i)))
                (while (and (< j len)
                            (let ((d (aref template j)))
                              (and (>= d ?0) (<= d ?9))))
                  (setq j (1+ j)))
                (let* ((n (string-to-number (substring template (1+ i) j)))
                       (val (prin1-to-string (nth (1- n) args))))
                  (setq out (concat out val))
                  (setq i j)))
            (setq out (concat out (char-to-string ch)))
            (setq i (1+ i)))))
      out)))

(defun anvil-sexp--replace-call-plan (scope fn-name new-form-template)
  "Build an edit plan replacing calls to FN-NAME with NEW-FORM-TEMPLATE.
Records sites that were skipped (parse failure, unreadable call,
arity mismatch with the template) so the caller can see partial
results in the plan summary rather than being silently given a
count that hides the misses."
  (let* ((files (anvil-sexp--resolve-scope scope))
         (fn-name-str (if (symbolp fn-name) (symbol-name fn-name) fn-name))
         (ops nil)
         (skips nil)
         (touched-files 0)
         (total-hits 0))
    (dolist (file files)
      (let ((emitted 0))
        (anvil-sexp--with-elisp-syntax
         (lambda ()
           (insert-file-contents file)
           (dolist (h (anvil-sexp--scan-symbol-refs file fn-name-str))
             (when (eq (plist-get h :kind) 'call)
               (let* ((tok-start (plist-get h :start))
                      (call-beg
                       (save-excursion
                         (goto-char tok-start)
                         (ignore-errors (backward-up-list))
                         (and (looking-at-p "(") (point))))
                      (call-end
                       (and call-beg
                            (save-excursion
                              (goto-char call-beg)
                              (ignore-errors (forward-sexp) (point))))))
                 (cond
                  ((or (null call-beg) (null call-end))
                   (push (list :file file :start tok-start
                               :reason "could not delimit call form")
                         skips))
                  (t
                   (let* ((call-text
                           (buffer-substring-no-properties call-beg call-end))
                          (call-sexp
                           (condition-case _
                               (car (read-from-string call-text))
                             (error nil))))
                     (cond
                      ((not (consp call-sexp))
                       (push (list :file file :start call-beg
                                   :reason "call text failed to read as sexp")
                             skips))
                      (t
                       (let* ((args (cdr call-sexp))
                              (render
                               (condition-case err
                                   (anvil-sexp--render-call-template
                                    new-form-template args)
                                 (error
                                  (cons 'err (error-message-string err))))))
                         (cond
                          ((and (consp render) (eq (car render) 'err))
                           (push (list :file file :start call-beg
                                       :reason (cdr render))
                                 skips))
                          (t
                           (cl-incf emitted)
                           (push (list :file file
                                       :range (cons call-beg call-end)
                                       :replacement render
                                       :reason (format "replace-call %s"
                                                       fn-name-str))
                                 ops))))))))))))))
        (when (> emitted 0)
          (cl-incf touched-files)
          (cl-incf total-hits emitted))))
    (list :ops (nreverse ops)
          :skipped (nreverse skips)
          :summary (format "replace-call %s: %d sites across %d files (%d skipped)"
                           fn-name-str total-hits touched-files (length skips))
          :diff-preview
          (format "# replace-call %s -> %s\n# %d sites, %d files, %d skipped\n"
                  fn-name-str new-form-template
                  total-hits touched-files (length skips)))))

(defun anvil-sexp--tool-replace-call (fn_name new_form &optional scope apply)
  "Replace every call to FN_NAME with NEW_FORM across SCOPE.

MCP Parameters:
  fn_name  - Name of the function / macro to rewrite (string).
  new_form - Source template with %1 %2 ... positional slots
             (string).  Each %N is substituted with the printed
             form of the Nth argument at each call site.
  scope    - As in `sexp-rename-symbol'.  Default project.
  apply    - Truthy to write.  Default preview.

Returns an edit plan plist."
  (anvil-server-with-error-handling
   (anvil-sexp--maybe-apply
    (anvil-sexp--replace-call-plan scope fn_name new_form)
    apply)))


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
      (while (not (eobp))
        (let ((line-text (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))))
          (cond
           ((string-match
             "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\):\\s-*\\(Warning\\|Error\\):\\s-*\\(.*\\)$"
             line-text)
            (let ((path (match-string 1 line-text))
                  (line (string-to-number (match-string 2 line-text)))
                  (col  (string-to-number (match-string 3 line-text)))
                  (kind-str (match-string 4 line-text))
                  (msg (match-string 5 line-text)))
              (when (string-equal (file-name-nondirectory path) base)
                (push (list :kind (if (equal kind-str "Error") 'error 'warning)
                            :source 'byte-compile
                            :line line :column col :message msg)
                      diags))))
           ((string-match
             "^\\(.*?\\):\\s-*\\(Warning\\|Error\\):\\s-*\\(.*\\)$"
             line-text)
            (let ((path (match-string 1 line-text))
                  (kind-str (match-string 2 line-text))
                  (msg (match-string 3 line-text)))
              (when (string-equal (file-name-nondirectory path) base)
                (push (list :kind (if (equal kind-str "Error") 'error 'warning)
                            :source 'byte-compile
                            :line 0 :column 0 :message msg)
                      diags)))))
        (forward-line 1))))
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
            (condition-case err
                (checkdoc-file file)
              (error
               (push (list :kind 'error :source 'checkdoc :line 0
                           :message (error-message-string err))
                     diags))))
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


;;;; --- public elisp API (Doc 12 Phase 1) ---------------------------------

;; Thin keyword-style wrappers over the flat `--tool-*' handlers.
;; The flat form is what `anvil-server-register-tool' expects from
;; an MCP client; elisp callers get the nicer shape described in
;; Doc 12's Public API section.

(cl-defun anvil-sexp-read-file (path &key _include)
  "List top-level forms in PATH.  See `sexp-read-file' MCP tool.
`:include' is accepted for forward compatibility with Doc 12
Phase 1+ but is currently ignored; the reader already skips
strings and comments by construction."
  (anvil-sexp--tool-read-file path))

(cl-defun anvil-sexp-surrounding-form (file point &key kind)
  "Return the top-level form containing POINT in FILE.  See
`sexp-surrounding-form' MCP tool for argument semantics."
  (anvil-sexp--tool-surrounding-form file point
                                     (and kind (symbol-name kind))))

(cl-defun anvil-sexp-replace-defun (file name new-form &key apply)
  "Replace the function-like form named NAME in FILE with NEW-FORM.
NEW-FORM is an elisp source string.  See `sexp-replace-defun' MCP
tool; defaults to preview unless APPLY is non-nil."
  (anvil-sexp--tool-replace-defun file name new-form (when apply "t")))

(cl-defun anvil-sexp-wrap-form (file point wrapper &key apply)
  "Wrap the innermost sexp at POINT in FILE with WRAPPER.
When POINT lies on inter-form whitespace, fall back to the enclosing list.
WRAPPER is source text containing `|anvil-sexp-hole|'.  See
`sexp-wrap-form' MCP tool."
  (anvil-sexp--tool-wrap-form file point wrapper (when apply "t")))

(cl-defun anvil-sexp-macroexpand (file name &key all)
  "Macroexpand the form named NAME in FILE.  See `sexp-macroexpand'
MCP tool.  When ALL is non-nil uses `macroexpand-all'."
  (anvil-sexp--tool-macroexpand file name (when all "t")))

(cl-defun anvil-sexp-verify (file &key (byte-compile t) (checkdoc t))
  "Run byte-compile and checkdoc on FILE and return diagnostics.
See `sexp-verify' MCP tool.  Pass :byte-compile nil or :checkdoc
nil to skip a check."
  (anvil-sexp--tool-verify file
                           (if byte-compile "t" "nil")
                           (if checkdoc "t" "nil")))


;;;; --- public elisp API (Doc 12 Phase 2a) --------------------------------

(cl-defun anvil-sexp-rename-symbol (old new &key kinds scope apply)
  "Rename OLD symbol to NEW across SCOPE.  See `sexp-rename-symbol'
MCP tool.  KINDS is a list of symbol kinds to include (any of
defun-name / call / quote / symbol); default all.  SCOPE is
`project' (default), a directory path, a file path, or a list of
file paths."
  (anvil-sexp--tool-rename-symbol
   old new
   (when kinds (mapconcat #'symbol-name kinds ","))
   (cond ((null scope) "project")
         ((symbolp scope) (symbol-name scope))
         ((listp scope) (mapconcat #'identity scope ","))
         (t scope))
   (when apply "t")))

(cl-defun anvil-sexp-replace-call (fn-name new-form &key scope apply)
  "Replace every call to FN-NAME with NEW-FORM template across SCOPE.
NEW-FORM may reference positional arguments via %1 %2 etc.  See
`sexp-replace-call' MCP tool."
  (anvil-sexp--tool-replace-call
   fn-name new-form
   (cond ((null scope) "project")
         ((symbolp scope) (symbol-name scope))
         ((listp scope) (mapconcat #'identity scope ","))
         (t scope))
   (when apply "t")))


;;;; --- module lifecycle ---------------------------------------------------

(defun anvil-sexp--register-tools ()
  "Register sexp-* MCP tools under `anvil-sexp--server-id'."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-sexp--tool-read-file)
   :id "sexp-read-file"
   :intent '(elisp-read structure)
   :layer 'core
   :server-id anvil-sexp--server-id
   :description
   "List every top-level form in an elisp source file.  Each entry
carries :kind (operator name), :name (defined symbol if any),
:start, :end, and :text (the form's printed representation).  The
reader skips strings and comments, so no substring in a docstring
or comment can leak into the result."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-sexp--tool-surrounding-form)
   :id "sexp-surrounding-form"
   :intent '(elisp-read structure)
   :layer 'core
   :server-id anvil-sexp--server-id
   :description
   "Return the innermost top-level form whose range contains the
given point.  Optional kind filter restricts the match to one
operator (e.g. \"defun\")."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-sexp--tool-macroexpand)
   :id "sexp-macroexpand"
   :intent '(elisp-read)
   :layer 'core
   :server-id anvil-sexp--server-id
   :description
   "Macroexpand a named top-level form and return its expansion as a
pretty-printed string.  Default is macroexpand-1; pass all=t for
macroexpand-all.  Expansion uses the ambient Emacs process, so
macros not yet loaded pass through unchanged."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-sexp--tool-verify)
   :id "sexp-verify"
   :intent '(elisp-read)
   :layer 'core
   :server-id anvil-sexp--server-id
   :description
   "Run byte-compile and checkdoc on an elisp file and return
structured diagnostics.  :passed is non-nil when no entry has
:kind \\='error.  Pass byte_compile=nil or checkdoc=nil to skip a
check."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-sexp--tool-replace-defun)
   :id "sexp-replace-defun"
   :intent '(code-bulk-edit)
   :layer 'core
   :server-id anvil-sexp--server-id
   :description
   "Replace the top-level form named NAME in FILE with NEW_FORM
source text.  Returns an edit plan (:ops :summary :diff-preview);
writing requires apply=t explicitly.  Refuses when the new form
does not parse or when no form matches the given name.")

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-sexp--tool-wrap-form)
   :id "sexp-wrap-form"
   :intent '(code-bulk-edit)
   :layer 'core
   :server-id anvil-sexp--server-id
   :description
   "Wrap the innermost sexp at POINT in FILE with WRAPPER source text.
When POINT lies on inter-form whitespace, the tool falls back to the
enclosing list.  WRAPPER must contain the placeholder token
`|anvil-sexp-hole|'; the original sexp replaces the placeholder.
Preview by default; apply=t writes to disk.")

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-sexp--tool-rename-symbol)
   :id "sexp-rename-symbol"
   :intent '(code-bulk-edit)
   :layer 'core
   :server-id anvil-sexp--server-id
   :description
   "Rename every reference of OLD to NEW across SCOPE.  Strings
and comments are skipped via syntax-ppss.  KINDS restricts to
one or more of defun-name / call / quote / symbol.  SCOPE is
\"project\" (default), a directory, a file, or a comma-separated
list of files.  Preview by default; apply=t writes.
(Doc 12 Phase 2a reader-only backend.)")

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-sexp--tool-replace-call)
   :id "sexp-replace-call"
   :intent '(code-bulk-edit)
   :layer 'core
   :server-id anvil-sexp--server-id
   :description
   "Replace every (FN_NAME ARGS...) call with NEW_FORM template
across SCOPE.  NEW_FORM may reference arguments via positional
slots %1 %2 etc.  Strings and comments are skipped.  Preview by
default; apply=t writes."))

(defun anvil-sexp--unregister-tools ()
  "Remove every sexp-* MCP tool from the shared server."
  (dolist (id '("sexp-read-file"
                "sexp-surrounding-form"
                "sexp-macroexpand"
                "sexp-verify"
                "sexp-replace-defun"
                "sexp-wrap-form"
                "sexp-rename-symbol"
                "sexp-replace-call"))
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
