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
  '(integer nil symbol string list alist plist hash-table vector cons
            record truncation circular drill)
  "Type tags + behaviors that `anvil-inspect-object' handles today.
Phase 1a chunks extend this list; tests in
tests/anvil-sexp-cst-test.el gate their `skip-unless' on membership
here so a half-shipped chunk never breaks CI.  The pseudo-tags
`truncation', `circular', and `drill' describe emitted *behaviors*
rather than Emacs runtime types — they live on the same list so
each test can self-describe its capability gate.")

(defconst anvil-sexp-cst--real-type-tags
  '(integer nil symbol string list alist plist hash-table vector cons record)
  "Runtime type tags returned by `anvil-sexp-cst--type-tag'.
Subset of `anvil-sexp-cst-supported-types' that the dispatcher
can match with a live handler.")

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
`anvil-sexp-cst--repr-cap' characters with a trailing ellipsis when cut.
Callers are expected to bind `print-circle' to t before invoking repr
on anything that may be part of a cycle."
  (let ((s (format "%S" v)))
    (if (> (length s) anvil-sexp-cst--repr-cap)
        (concat (substring s 0 (- anvil-sexp-cst--repr-cap 3)) "...")
      s)))


;;;; --- cycle detection + cursor generation (chunk 4) ---------------------

(defvar anvil-sexp-cst--current-client-id nil
  "Client-id in scope for the current `anvil-inspect-object' call.
Handlers that emit a drill cursor read this to namespace the
cursor string.  Defaults to \"local\" when nil.")

(defvar anvil-sexp-cst--window-offset 0
  "Top-level child offset for the current inspect dispatch (Phase 1b-a).
Bound by `anvil-inspect-object-drill' when paging past the first
window; the fresh-inspect path leaves it at 0 so Phase 1a behavior
is preserved exactly.  Container handlers read it to select which
slice of their payload to emit as `entries'.")

(defun anvil-sexp-cst--clamp-offset (len)
  "Return `anvil-sexp-cst--window-offset' clamped to [0, LEN]."
  (min (max 0 anvil-sexp-cst--window-offset) len))

(defun anvil-sexp-cst--circular-list-p (v)
  "Return non-nil when V is a cons that contains a cycle reachable via cdr.
Uses Floyd's tortoise-and-hare so this terminates even on deeply
cyclic structures.  Returns nil for proper lists, improper
non-circular pairs, and non-cons values."
  (and (consp v)
       (let ((slow v) (fast v))
         (catch 'found
           (while (and (consp fast) (consp (cdr fast)))
             (setq slow (cdr slow)
                   fast (cddr fast))
             (when (eq slow fast)
               (throw 'found t)))
           nil))))

(defun anvil-sexp-cst--gen-uuid ()
  "Return a short slash-free token suitable for a cursor suffix.
Deliberately lightweight — 16 hex chars, ~64 bits of entropy,
enough for per-session uniqueness inside the
`inspect-object/<client-id>/' namespace."
  (format "%08x%08x"
          (random (expt 2 32))
          (logand #xffffffff
                  (truncate (* (float-time) 1000000)))))

(defun anvil-sexp-cst--make-cursor (value)
  "Return a drill cursor for VALUE and (best-effort) persist it.
Cursor namespace: `inspect-object/<client-id>/<uuid>' per Doc 31
consensus critique.  Storage goes through anvil-state Doc 08 when
that module is loaded; callers get a valid cursor even if storage
is unavailable (drill resolution lands in Phase 1b)."
  (let* ((cid (or anvil-sexp-cst--current-client-id "local"))
         (uuid (anvil-sexp-cst--gen-uuid))
         (ns (format "inspect-object/%s" cid))
         (cursor (format "%s/%s" ns uuid)))
    (when (fboundp 'anvil-state-set)
      (condition-case _err
          (funcall (symbol-function 'anvil-state-set)
                   uuid value :ns ns :ttl 86400)
        (error nil)))
    cursor))

(defun anvil-sexp-cst--window-truncation-info (full-len offset taken value)
  "Return (TRUNCATED . CURSOR-OR-NIL) for a window over VALUE.
TRUNCATED is t when OFFSET+TAKEN is still short of FULL-LEN — i.e.
content after the emitted slice remains un-reported.  Mints a drill
cursor in that case (Phase 1b-a).  When the window covers the
whole structure, truncated is nil and no cursor is allocated."
  (if (> full-len (+ offset taken))
      (cons t (anvil-sexp-cst--make-cursor value))
    (cons nil nil)))

(defun anvil-sexp-cst--encode-error (type kind message)
  "Encode TYPE plus a typed error envelope (no entries)."
  (anvil-sexp-cst--encode
   type
   :error (list :kind kind :message message)))


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
Entries use the index as key and `anvil-sexp-cst--repr' for value;
if LST length exceeds `anvil-sexp-cst-top-limit' a drill cursor is
emitted under the `inspect-object/<client-id>/' namespace.  When
`anvil-sexp-cst--window-offset' is non-zero the window shifts to
that offset — this drives Phase 1b-a pagination."
  (let* ((len (length lst))
         (off (anvil-sexp-cst--clamp-offset len))
         (take (min (max 0 (- len off)) anvil-sexp-cst-top-limit))
         (head (cl-subseq lst off (+ off take)))
         (entries (cl-loop for cell in head
                           for i from off
                           collect (list :k (number-to-string i)
                                         :v (anvil-sexp-cst--repr cell))))
         (ti (anvil-sexp-cst--window-truncation-info len off take lst)))
    (anvil-sexp-cst--encode
     'list
     :length len
     :entries (apply #'vector entries)
     :truncated (car ti)
     :cursor (cdr ti))))

(defun anvil-sexp-cst--inspect-alist (al)
  "Render alist AL as shape-lock JSON.
Each entry is ((KEY-REPR . VAL-REPR)) → {k, v}; keys are run
through `anvil-sexp-cst--repr' so symbols surface readably while
still round-tripping via `read'.  Honors
`anvil-sexp-cst--window-offset' so drill callers can page past the
first top-limit cells."
  (let* ((len (length al))
         (off (anvil-sexp-cst--clamp-offset len))
         (take (min (max 0 (- len off)) anvil-sexp-cst-top-limit))
         (head (cl-subseq al off (+ off take)))
         (entries (cl-loop for cell in head
                           collect (list :k (anvil-sexp-cst--repr (car-safe cell))
                                         :v (anvil-sexp-cst--repr (cdr-safe cell)))))
         (ti (anvil-sexp-cst--window-truncation-info len off take al)))
    (anvil-sexp-cst--encode
     'alist
     :length len
     :entries (apply #'vector entries)
     :truncated (car ti)
     :cursor (cdr ti))))

(defun anvil-sexp-cst--inspect-plist (pl)
  "Render plist PL as shape-lock JSON.
Plist `length' is the raw element count (not pair count) — matches
`length' semantics the shape-lock test enforces.  Truncation fires
on pair count past the top limit.  `anvil-sexp-cst--window-offset'
is interpreted as a *pair* offset (skip N pairs), matching the
drill contract."
  (let* ((len (length pl))
         (pairs (cl-loop for (k v) on pl by #'cddr
                         collect (cons k v)))
         (pair-count (length pairs))
         (off (anvil-sexp-cst--clamp-offset pair-count))
         (take (min (max 0 (- pair-count off)) anvil-sexp-cst-top-limit))
         (head (cl-subseq pairs off (+ off take)))
         (entries (cl-loop for cell in head
                           collect (list :k (anvil-sexp-cst--repr (car cell))
                                         :v (anvil-sexp-cst--repr (cdr cell)))))
         (ti (anvil-sexp-cst--window-truncation-info pair-count off take pl)))
    (anvil-sexp-cst--encode
     'plist
     :length len
     :entries (apply #'vector entries)
     :truncated (car ti)
     :cursor (cdr ti))))


;;;; --- hash-table / vector / cons handlers (chunk 3) --------------------

(defun anvil-sexp-cst--inspect-hash-table (h)
  "Render hash-table H as shape-lock JSON.
Hash-table iteration order is implementation-defined so
`anvil-sexp-cst--window-offset' does not give stable pagination
here — a later chunk may materialise a sorted key list, but for
1b-a drill still returns a valid typed response (first `top-limit'
entries maphash yields from an arbitrary start)."
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
    (let ((ti (anvil-sexp-cst--window-truncation-info len 0 count h)))
      (anvil-sexp-cst--encode
       'hash-table
       :length len
       :entries (apply #'vector (nreverse entries))
       :truncated (car ti)
       :cursor (cdr ti)))))

(defun anvil-sexp-cst--inspect-vector (vec)
  "Render vector VEC as shape-lock JSON with drill cursor past cap.
Honors `anvil-sexp-cst--window-offset' for drill pagination."
  (let* ((len (length vec))
         (off (anvil-sexp-cst--clamp-offset len))
         (take (min (max 0 (- len off)) anvil-sexp-cst-top-limit))
         (entries (cl-loop for i from off below (+ off take)
                           collect (list :k (number-to-string i)
                                         :v (anvil-sexp-cst--repr (aref vec i)))))
         (ti (anvil-sexp-cst--window-truncation-info len off take vec)))
    (anvil-sexp-cst--encode
     'vector
     :length len
     :entries (apply #'vector entries)
     :truncated (car ti)
     :cursor (cdr ti))))

(defun anvil-sexp-cst--inspect-cons (c)
  "Render an improper cons C as shape-lock JSON.
Emits the pair as two fixed-key entries `car' / `cdr'.  Length
is omitted — a pair has no meaningful sequence length."
  (anvil-sexp-cst--encode
   'cons
   :entries (vector
             (list :k "car" :v (anvil-sexp-cst--repr (car c)))
             (list :k "cdr" :v (anvil-sexp-cst--repr (cdr c))))))

(defun anvil-sexp-cst--inspect-record (rec)
  "Render record REC as a Phase 1a stub: type tag + slot count only.
Slot 0 holds the record type, so `length' is `(length rec) - 1'
to match the user-visible slot count.  Drill into individual
slots lands in Phase 1b."
  (anvil-sexp-cst--encode
   'record
   :length (max 0 (1- (length rec)))))


;;;; --- drill cursor resolution (chunk 1b-a) ------------------------------

(defun anvil-sexp-cst--parse-cursor (cursor)
  "Parse CURSOR into (CID . UUID) or nil on malformed input.
Expected format: `inspect-object/<client-id>/<uuid>'.  Neither
component may be empty and neither may contain a slash."
  (and (stringp cursor)
       (let ((parts (split-string cursor "/")))
         (and (= (length parts) 3)
              (string= (nth 0 parts) "inspect-object")
              (> (length (nth 1 parts)) 0)
              (> (length (nth 2 parts)) 0)
              (cons (nth 1 parts) (nth 2 parts))))))

(defun anvil-sexp-cst--coerce-int (v default)
  "Return V as an integer, or DEFAULT if V is nil / empty / non-numeric.
Accepts either a live integer (direct Elisp callers) or an
all-digit string (MCP callers, where every arg arrives as text)."
  (cond
   ((integerp v) v)
   ((and (stringp v) (not (string-empty-p v))
         (string-match-p "\\`[0-9]+\\'" v))
    (string-to-number v))
   (t default)))


;;;; --- dispatcher --------------------------------------------------------

(defun anvil-sexp-cst--inspect-dispatch (value)
  "Return JSON for VALUE, dispatching on `anvil-sexp-cst--type-tag'.
Tags not listed in `anvil-sexp-cst--real-type-tags' signal
`anvil-server-tool-error' so the MCP client sees a typed error
rather than undefined shape."
  (let ((tag (anvil-sexp-cst--type-tag value)))
    (unless (memq tag anvil-sexp-cst--real-type-tags)
      (signal 'anvil-server-tool-error
              (list (format "inspect-object: type %S not implemented in this build (supported: %S)"
                            tag anvil-sexp-cst--real-type-tags))))
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
      ('record     (anvil-sexp-cst--inspect-record value))
      (_
       ;; Unreachable — guard above rejects unlisted tags.
       (signal 'anvil-server-tool-error
               (list (format "inspect-object: no handler for tag %S" tag)))))))


;;;; --- MCP tool ----------------------------------------------------------

;;;###autoload
(defun anvil-inspect-object (value &optional client-id)
  "Return a shape-locked JSON inspection of VALUE.

CLIENT-ID (optional) is used to namespace drill cursors emitted on
truncation.  It defaults to \"local\" when omitted so direct Elisp
callers do not have to care.

Circular structures are detected before dispatch and returned as
a typed error envelope (`inspect-object/circular-reference');
`print-circle' is bound while rendering so repr on deeper cyclic
children emits `#N=' notation rather than looping.

Output shape (Doc 31 Phase 1a):
  {type, length?, entries?, truncated, cursor?, error?}

MCP Parameters:
  VALUE     Lisp value to inspect (any type)
  CLIENT-ID Optional string used to namespace drill cursors"
  (let ((print-circle t)
        (anvil-sexp-cst--current-client-id client-id))
    (anvil-server-with-error-handling
     (cond
      ((anvil-sexp-cst--circular-list-p value)
       (anvil-sexp-cst--encode-error
        'list
        "inspect-object/circular-reference"
        "Value is a circular structure; full traversal suppressed."))
      (t
       (anvil-sexp-cst--inspect-dispatch value))))))


;;;###autoload
(defun anvil-inspect-object-drill (cursor &optional client-id offset limit)
  "Resolve CURSOR through anvil-state and re-inspect the stored value.

CURSOR is the `inspect-object/<cid>/<uuid>' string emitted by a
previous `anvil-inspect-object' call whose output was truncated.

CLIENT-ID, when given, must match the <cid> embedded in CURSOR.
A mismatch returns an `inspect-object/cursor-forbidden' typed
error.  Omitting CLIENT-ID skips the check — intended for direct
Elisp callers; MCP callers always receive their client-id.

OFFSET and LIMIT select a window into the stored value's
top-level children.  OFFSET defaults to 0, LIMIT to
`anvil-sexp-cst-top-limit'.  Both accept integers or digit
strings so the same entry point backs both Elisp and MCP callers.

Returns the same shape-locked JSON as `anvil-inspect-object', or
a typed error envelope keyed by `inspect-object/…' for
malformed, expired, forbidden, or state-unavailable cursors.
Hash-table pagination is best-effort: `maphash' order is
implementation-defined (see that handler's docstring).

MCP Parameters:
  CURSOR    Cursor string from a prior inspect-object truncation
  CLIENT-ID Optional owner client-id (enforced when provided)
  OFFSET    Optional window start (default 0)
  LIMIT     Optional window width (default top-limit)"
  (let ((print-circle t))
    (anvil-server-with-error-handling
     (let ((parsed (anvil-sexp-cst--parse-cursor cursor)))
       (cond
        ((null parsed)
         (anvil-sexp-cst--encode-error
          'error
          "inspect-object/cursor-malformed"
          (format "Cursor %S does not match inspect-object/<cid>/<uuid>"
                  cursor)))
        ((and client-id (stringp client-id) (not (string-empty-p client-id))
              (not (string= client-id (car parsed))))
         (anvil-sexp-cst--encode-error
          'error
          "inspect-object/cursor-forbidden"
          "Cursor was minted for a different client-id."))
        ((not (fboundp 'anvil-state-get))
         (anvil-sexp-cst--encode-error
          'error
          "inspect-object/state-unavailable"
          "anvil-state module is not loaded; cursor cannot be resolved."))
        (t
         (let* ((cid (car parsed))
                (uuid (cdr parsed))
                (ns (format "inspect-object/%s" cid))
                (sentinel (make-symbol "anvil-sexp-cst-drill-miss"))
                (value (funcall (symbol-function 'anvil-state-get)
                                uuid :ns ns :default sentinel)))
           (if (eq value sentinel)
               (anvil-sexp-cst--encode-error
                'error
                "inspect-object/cursor-expired"
                (format "No state row for cursor %S (expired or never minted)."
                        cursor))
             (let* ((off-int (anvil-sexp-cst--coerce-int offset 0))
                    (lim-int (anvil-sexp-cst--coerce-int
                              limit anvil-sexp-cst-top-limit))
                    (anvil-sexp-cst--window-offset off-int)
                    (anvil-sexp-cst-top-limit lim-int)
                    (anvil-sexp-cst--current-client-id cid))
               (anvil-sexp-cst--inspect-dispatch value))))))))))


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
integer / nil / symbol / string / list / alist / plist / hash-table /
vector / cons / record (stub).  The CLIENT-ID argument namespaces
drill cursors so concurrent clients do not collide.  Pair with
sexp-cst-inspect-object-drill to page past top-limit."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-inspect-object-drill
   :id "sexp-cst-inspect-object-drill"
   :server-id anvil-sexp-cst--server-id
   :description
   "Resolve an inspect-object cursor and re-inspect the stored value
under a caller-supplied window (OFFSET / LIMIT).  Typed errors cover
malformed cursors, expired state rows, cross-client attempts, and the
case where anvil-state is not loaded.  Read-only."
   :read-only t))

(defun anvil-sexp-cst--unregister-tools ()
  "Remove every sexp-cst-* MCP tool."
  (dolist (id '("sexp-cst-inspect-object"
                "sexp-cst-inspect-object-drill"))
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
