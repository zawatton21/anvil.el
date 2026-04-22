;;; anvil-sexp-cst.el --- Tree-sitter CST + runtime inspect tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;;; Commentary:

;; Doc 31 Phase 1a chunk 1 — skeleton + scalar-type `anvil-inspect-object'.
;;
;; This module eventually carries two families of tools:
;;
;;   1. runtime introspection — `anvil-inspect-object' (this chunk)
;;      A lazy, depth-capped view into any live Lisp value.  Designed
;;      for LLM consumption: token-bounded JSON, drill cursor for
;;      paging into subtrees, typed error envelope for hostile inputs.
;;      Independent of tree-sitter.
;;
;;   2. CST-aware edits — `anvil-sexp-cst-read' / `-edit' (Phase 2a+)
;;      Comment-preserving structural reads and rewrites driven by
;;      tree-sitter-elisp.  These *do* need a working grammar.
;;
;; Phase 1a chunk 1 registers `inspect-object' for the scalar types
;; listed in `anvil-sexp-cst-supported-types'.  Each subsequent chunk
;; extends that list; the shape-lock tests in
;; tests/anvil-sexp-cst-test.el skip per-type, so chunked shipping is
;; safe.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'anvil-server)


;;;; --- group + constants --------------------------------------------------

(defgroup anvil-sexp-cst nil
  "Tree-sitter CST + runtime inspect tools for Emacs Lisp."
  :group 'anvil
  :prefix "anvil-sexp-cst-")

(defconst anvil-sexp-cst--server-id "emacs-eval"
  "Server ID under which sexp-cst-* MCP tools are registered.")

(defconst anvil-sexp-cst-supported-types
  '(integer nil symbol)
  "Type tags that `anvil-inspect-object' renders in the current build.
Phase 1a chunks extend this list; tests in
tests/anvil-sexp-cst-test.el gate their `skip-unless' on membership
here so a half-shipped chunk never breaks CI.")

(defcustom anvil-sexp-cst-cap-bytes 4096
  "Soft cap on `anvil-inspect-object' JSON output in bytes.
When exceeded the output is truncated and a `cursor' pointer is
emitted; the cursor value lives in the anvil-state KV under the
`inspect-object/<client-id>/' namespace with 24h TTL."
  :type 'integer
  :group 'anvil-sexp-cst)

(defcustom anvil-sexp-cst-top-limit 10
  "Number of top-level children surfaced per `anvil-inspect-object' call.
Beyond this the output is truncated with a drill cursor."
  :type 'integer
  :group 'anvil-sexp-cst)


;;;; --- type classifier ---------------------------------------------------

(defun anvil-sexp-cst--type-tag (v)
  "Return a short symbol tag for V's runtime type.
Order matters: char-table predicate must precede vector because
`(vectorp (make-char-table …))' is nil but char-tables still satisfy
`sequencep'; record must precede list because records inherit
`sequencep' too."
  (cond
   ((null v) 'nil)
   ((eq v t) 'bool)
   ((integerp v) 'integer)
   ((floatp v) 'float)
   ((stringp v) 'string)
   ((keywordp v) 'symbol)
   ((symbolp v) 'symbol)
   ((char-table-p v) 'char-table)
   ((hash-table-p v) 'hash-table)
   ((recordp v) 'record)
   ((vectorp v) 'vector)
   ((and (consp v)
         (keywordp (car-safe v)))
    'plist)
   ((and (consp v)
         (consp (car-safe v))
         (atom (cdr-safe (car-safe v))))
    'alist)
   ((consp v)
    (if (proper-list-p v) 'list 'cons))
   (t 'other)))


;;;; --- JSON output encoder -----------------------------------------------

(defun anvil-sexp-cst--encode (type &rest props)
  "Encode TYPE + PROPS as the shape-locked JSON output string.

PROPS is a plist understood keys:
  :length    INTEGER  — optional, omitted when nil
  :entries   VECTOR   — optional, omitted when nil
  :truncated BOOLEAN  — emitted as JSON false/true; nil defaults to false
  :cursor    STRING   — optional, omitted when nil
  :error     ALIST    — optional, omitted when nil (typed error envelope)

Keys with nil values are not emitted; the test suite's `json-parse-string'
walks the resulting hash-table and requires absent keys to stay absent
rather than round-tripping to `:null'."
  (let* ((length    (plist-get props :length))
         (entries   (plist-get props :entries))
         (truncated (plist-get props :truncated))
         (cursor    (plist-get props :cursor))
         (err       (plist-get props :error))
         (payload   (list :type (symbol-name type))))
    (when length
      (setq payload (nconc payload (list :length length))))
    (when entries
      (setq payload (nconc payload (list :entries entries))))
    (setq payload
          (nconc payload
                 (list :truncated (if truncated t :false))))
    (when cursor
      (setq payload (nconc payload (list :cursor cursor))))
    (when err
      (setq payload (nconc payload (list :error err))))
    (json-serialize payload
                    :false-object :false
                    :null-object :null)))


;;;; --- scalar handlers (chunk 1) -----------------------------------------

(defun anvil-sexp-cst--inspect-integer (v)
  "Render integer V as shape-lock JSON.
Scalars omit `length' — size is not a sequence measure and
letting the key default to absent keeps the contract terse."
  (anvil-sexp-cst--encode
   'integer
   :entries (vector (list :k "=" :v (number-to-string v)))))

(defun anvil-sexp-cst--inspect-nil ()
  "Render the `nil' constant as shape-lock JSON."
  (anvil-sexp-cst--encode 'nil))

(defun anvil-sexp-cst--inspect-symbol (sym)
  "Render symbol SYM as shape-lock JSON."
  (let ((name (symbol-name sym)))
    (anvil-sexp-cst--encode
     'symbol
     :length (length name)
     :entries (vector (list :k "=" :v name)))))


;;;; --- dispatcher --------------------------------------------------------

(defun anvil-sexp-cst--inspect-dispatch (value)
  "Return JSON for VALUE, dispatching on `anvil-sexp-cst--type-tag'.
Only the tags listed in `anvil-sexp-cst-supported-types' have a
live handler in chunk 1; others signal `anvil-server-tool-error'
so the MCP client sees a typed error rather than undefined shape."
  (let ((tag (anvil-sexp-cst--type-tag value)))
    (unless (memq tag anvil-sexp-cst-supported-types)
      (signal 'anvil-server-tool-error
              (list (format "inspect-object: type %S not implemented in this build (supported: %S)"
                            tag anvil-sexp-cst-supported-types))))
    (pcase tag
      ('integer (anvil-sexp-cst--inspect-integer value))
      ('nil     (anvil-sexp-cst--inspect-nil))
      ('symbol  (anvil-sexp-cst--inspect-symbol value))
      (_
       ;; Unreachable — guard above rejects unlisted tags.
       (signal 'anvil-server-tool-error
               (list (format "inspect-object: no handler for tag %S" tag)))))))


;;;; --- MCP tool ----------------------------------------------------------

;;;###autoload
(defun anvil-inspect-object (value &optional client-id)
  "Return a shape-locked JSON inspection of VALUE.

CLIENT-ID (optional) is used to namespace any drill cursors
emitted on truncation.  It defaults to \"local\" when omitted so
direct Elisp callers do not have to care.

Output shape (Doc 31 Phase 1a):
  {type, length?, entries?, truncated, cursor?, error?}

MCP Parameters:
  VALUE     Lisp value to inspect (any type)
  CLIENT-ID Optional string used to namespace drill cursors"
  (ignore client-id)                    ; consumed in chunk 2+ for truncation
  (anvil-server-with-error-handling
   (anvil-sexp-cst--inspect-dispatch value)))


;;;; --- module lifecycle --------------------------------------------------

(defun anvil-sexp-cst--abi-ready-p ()
  "Return non-nil when tree-sitter-elisp is usable by this Emacs.
Used only to drive the user-facing warning — the Phase 1a
`anvil-inspect-object' does not need tree-sitter and stays
available either way."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (fboundp 'treesit-ready-p)
       (ignore-errors (treesit-ready-p 'elisp t))))

(defvar anvil-sexp-cst--abi-warning-shown nil
  "Non-nil once the ABI-mismatch warning has been emitted in this session.")

(defun anvil-sexp-cst--maybe-warn-abi ()
  "Emit a one-shot warning when the tree-sitter-elisp grammar is absent.
The Phase 1a `anvil-inspect-object' still registers; the warning
exists so users can run `benchmarks/tools/regenerate-abi14.sh'
before trying the later CST tools."
  (unless (or anvil-sexp-cst--abi-warning-shown
              (anvil-sexp-cst--abi-ready-p))
    (setq anvil-sexp-cst--abi-warning-shown t)
    (message
     "anvil-sexp-cst: tree-sitter-elisp grammar not ready; run benchmarks/tools/regenerate-abi14.sh for future CST tools (inspect-object works regardless)")))

(defun anvil-sexp-cst--register-tools ()
  "Register every sexp-cst-* MCP tool."
  (anvil-server-register-tool
   #'anvil-inspect-object
   :id "sexp-cst-inspect-object"
   :server-id anvil-sexp-cst--server-id
   :description
   "Inspect any live Elisp value and return a token-bounded JSON summary
(type, length, top entries, truncation cursor).  Supported types today:
integer / nil / symbol.  Later chunks extend coverage.  The CLIENT-ID
argument namespaces drill cursors so concurrent clients do not collide."
   :read-only t))

(defun anvil-sexp-cst--unregister-tools ()
  "Remove every sexp-cst-* MCP tool."
  (dolist (id '("sexp-cst-inspect-object"))
    (anvil-server-unregister-tool id anvil-sexp-cst--server-id)))

;;;###autoload
(defun anvil-sexp-cst-enable ()
  "Register sexp-cst-* MCP tools.
Emits a one-shot warning when the tree-sitter-elisp grammar is
missing or ABI-incompatible (does not block registration)."
  (interactive)
  (anvil-sexp-cst--maybe-warn-abi)
  (anvil-sexp-cst--register-tools))

(defun anvil-sexp-cst-disable ()
  "Unregister sexp-cst-* MCP tools."
  (interactive)
  (anvil-sexp-cst--unregister-tools))

(provide 'anvil-sexp-cst)
;;; anvil-sexp-cst.el ends here
