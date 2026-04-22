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
  '(integer nil symbol string list alist plist hash-table vector cons)
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


;;;; --- sequence helpers (shared by chunk 2+) -----------------------------

(defconst anvil-sexp-cst--repr-cap 60
  "Per-value character cap on `prin1'-style reprs emitted in entries.
Entry values are already a terse summary; this second cap keeps a
single deeply-nested child from blowing the 4 KB output budget.")

(defun anvil-sexp-cst--repr (v)
  "Return a short printed representation of V, capped at
`anvil-sexp-cst--repr-cap' characters with a trailing ellipsis when cut."
  (let ((s (format "%S" v)))
    (if (> (length s) anvil-sexp-cst--repr-cap)
        (concat (substring s 0 (- anvil-sexp-cst--repr-cap 3)) "...")
      s)))


;;;; --- string / list / alist / plist handlers (chunk 2) ------------------

(defun anvil-sexp-cst--inspect-string (s)
  "Render string S as shape-lock JSON.
`length' is the character count.  A single `slice' entry carries
up to 40 leading characters so a downstream LLM sees actual content."
  (anvil-sexp-cst--encode
   'string
   :length (length s)
   :entries
   (vector
    (list :k "slice"
          :v (anvil-sexp-cst--repr
              (if (> (length s) 40) (substring s 0 40) s))))))

(defun anvil-sexp-cst--inspect-list (lst)
  "Render proper list LST as shape-lock JSON.
Entries use the index as key and `anvil-sexp-cst--repr' for value.
Caps at `anvil-sexp-cst-top-limit' elements; chunk 4 wires the
remainder to a drill cursor."
  (let* ((len (length lst))
         (take (min len anvil-sexp-cst-top-limit))
         (head (cl-subseq lst 0 take))
         (entries (cl-loop for cell in head
                           for i from 0
                           collect (list :k (number-to-string i)
                                         :v (anvil-sexp-cst--repr cell)))))
    (anvil-sexp-cst--encode
     'list
     :length len
     :entries (apply #'vector entries))))

(defun anvil-sexp-cst--inspect-alist (al)
  "Render alist AL as shape-lock JSON.
Each entry is ((KEY-REPR . VAL-REPR)) → {k, v}; keys are run
through `anvil-sexp-cst--repr' so symbols surface readably
while still round-tripping via `read'."
  (let* ((len (length al))
         (take (min len anvil-sexp-cst-top-limit))
         (head (cl-subseq al 0 take))
         (entries (cl-loop for cell in head
                           collect (list :k (anvil-sexp-cst--repr (car-safe cell))
                                         :v (anvil-sexp-cst--repr (cdr-safe cell))))))
    (anvil-sexp-cst--encode
     'alist
     :length len
     :entries (apply #'vector entries))))

(defun anvil-sexp-cst--inspect-plist (pl)
  "Render plist PL as shape-lock JSON.
Plist `length' is the raw element count (not pair count) — matches
`length' semantics the shape-lock test enforces.  Entries are paired
with the plist key as JSON `k' and the following value's repr as `v'."
  (let* ((len (length pl))
         (pairs (cl-loop for (k v) on pl by #'cddr
                         collect (cons k v)))
         (take (min (length pairs) anvil-sexp-cst-top-limit))
         (head (cl-subseq pairs 0 take))
         (entries (cl-loop for cell in head
                           collect (list :k (anvil-sexp-cst--repr (car cell))
                                         :v (anvil-sexp-cst--repr (cdr cell))))))
    (anvil-sexp-cst--encode
     'plist
     :length len
     :entries (apply #'vector entries))))


;;;; --- hash-table / vector / cons handlers (chunk 3) --------------------

(defun anvil-sexp-cst--inspect-hash-table (h)
  "Render hash-table H as shape-lock JSON.
Key ordering is undefined by `maphash', so the first
`anvil-sexp-cst-top-limit' entries seen are taken; chunk 4 tags
the remainder with a drill cursor for deterministic paging."
  (let* ((len (hash-table-count h))
         (limit anvil-sexp-cst-top-limit)
         (count 0)
         entries)
    (catch 'cap
      (maphash
       (lambda (k v)
         (when (>= count limit) (throw 'cap nil))
         (push (list :k (anvil-sexp-cst--repr k)
                     :v (anvil-sexp-cst--repr v))
               entries)
         (cl-incf count))
       h))
    (anvil-sexp-cst--encode
     'hash-table
     :length len
     :entries (apply #'vector (nreverse entries)))))

(defun anvil-sexp-cst--inspect-vector (vec)
  "Render vector VEC as shape-lock JSON.
Index-keyed entries up to `anvil-sexp-cst-top-limit'."
  (let* ((len (length vec))
         (take (min len anvil-sexp-cst-top-limit))
         (entries (cl-loop for i below take
                           collect (list :k (number-to-string i)
                                         :v (anvil-sexp-cst--repr (aref vec i))))))
    (anvil-sexp-cst--encode
     'vector
     :length len
     :entries (apply #'vector entries))))

(defun anvil-sexp-cst--inspect-cons (c)
  "Render an improper cons C as shape-lock JSON.
Emits the pair as two fixed-key entries `car' / `cdr'.  Length
is omitted — a pair has no meaningful sequence length."
  (anvil-sexp-cst--encode
   'cons
   :entries (vector
             (list :k "car" :v (anvil-sexp-cst--repr (car c)))
             (list :k "cdr" :v (anvil-sexp-cst--repr (cdr c))))))


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
      ('string     (anvil-sexp-cst--inspect-string value))
      ('list       (anvil-sexp-cst--inspect-list value))
      ('alist      (anvil-sexp-cst--inspect-alist value))
      ('plist      (anvil-sexp-cst--inspect-plist value))
      ('hash-table (anvil-sexp-cst--inspect-hash-table value))
      ('vector     (anvil-sexp-cst--inspect-vector value))
      ('cons       (anvil-sexp-cst--inspect-cons value))
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
