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
            record char-table truncation circular drill byte-cap
            eieio purge cst-read cst-edit cst-edit-write cst-repair)
  "Type tags + behaviors that `anvil-inspect-object' handles today.
Phase 1a/1b/2/3 chunks extend this list; tests in
tests/anvil-sexp-cst-test.el gate their `skip-unless' on membership
here so a half-shipped chunk never breaks CI.  The pseudo-tags
`truncation', `circular', `drill', `byte-cap', `eieio', `purge',
`cst-read', `cst-edit', `cst-edit-write', `cst-repair' describe
emitted *behaviors* rather than Emacs runtime types — they live on
the same list so each test can self-describe its capability gate.")

(defconst anvil-sexp-cst--real-type-tags
  '(integer nil symbol string list alist plist hash-table vector cons
            record char-table)
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

(defcustom anvil-sexp-cst-read-max-depth 4
  "Default recursion cap for `anvil-sexp-cst-read' (Phase 2a).
Nodes at this depth are emitted with their raw source `text' rather
than recursing into `children'.  Deeper drill requires a separate
call with a smaller POSITION window or a larger MAX-DEPTH."
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

(defun anvil-sexp-cst--build-payload (type length entries truncated cursor err)
  "Return a plist payload ready for `json-serialize'.
Absent keys stay absent (tests require this to distinguish `no
length' from `length = null')."
  (let ((p (list :type (symbol-name type))))
    (when length (setq p (nconc p (list :length length))))
    (when entries (setq p (nconc p (list :entries entries))))
    (setq p (nconc p (list :truncated (if truncated t :false))))
    (when cursor (setq p (nconc p (list :cursor cursor))))
    (when err (setq p (nconc p (list :error err))))
    p))

(defun anvil-sexp-cst--serialize (payload)
  "Serialize PAYLOAD plist as JSON with false-/null-object set."
  (json-serialize payload :false-object :false :null-object :null))

(defun anvil-sexp-cst--encode (type &rest props)
  "Encode TYPE + PROPS as the shape-locked JSON output string.

PROPS is a plist understood keys:
  :length        INTEGER  — optional, omitted when nil
  :entries       VECTOR   — optional, omitted when nil
  :truncated     BOOLEAN  — emitted as JSON false/true; nil → false
  :cursor        STRING   — optional, omitted when nil
  :error         ALIST    — optional, omitted when nil (typed error)
  :cursor-source VALUE    — optional.  When set, the byte-cap
                            backoff (Phase 1b-b) may drop entries
                            from the tail until the serialised bytes
                            fit `anvil-sexp-cst-cap-bytes'.  Each
                            drop sets `:truncated' to t; a new
                            cursor is minted from this value only
                            when `:cursor' was not already supplied.

Keys with nil values are not emitted; the test suite's `json-parse-string'
walks the resulting hash-table and requires absent keys to stay absent
rather than round-tripping to `:null'."
  (let* ((length    (plist-get props :length))
         (entries   (plist-get props :entries))
         (truncated (plist-get props :truncated))
         (cursor    (plist-get props :cursor))
         (err       (plist-get props :error))
         (source    (plist-get props :cursor-source))
         (cap       anvil-sexp-cst-cap-bytes)
         (enc       (anvil-sexp-cst--serialize
                     (anvil-sexp-cst--build-payload
                      type length entries truncated cursor err))))
    (cond
     ;; Happy path: under cap or byte-cap disabled for this call.
     ((or (null source) (<= (string-bytes enc) cap))
      enc)
     ;; Byte cap fires: drop entries from the tail until the payload fits.
     (t
      (let ((vec (cond ((vectorp entries) entries)
                       ((listp entries) (apply #'vector entries))
                       (t (vector))))
            (cur (or cursor (anvil-sexp-cst--make-cursor source)))
            (fit nil))
        (while (and (null fit) (> (length vec) 0))
          (setq vec (cl-subseq vec 0 (1- (length vec))))
          (let ((candidate (anvil-sexp-cst--serialize
                            (anvil-sexp-cst--build-payload
                             type length
                             (and (> (length vec) 0) vec)
                             t cur err))))
            (when (<= (string-bytes candidate) cap)
              (setq fit candidate))))
        (or fit
            ;; Minimal envelope: drop entries entirely.
            (anvil-sexp-cst--serialize
             (anvil-sexp-cst--build-payload
              type length nil t cur err))))))))


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
     :cursor (cdr ti)
     :cursor-source lst)))

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
     :cursor (cdr ti)
     :cursor-source al)))

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
     :cursor (cdr ti)
     :cursor-source pl)))


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
       :cursor (cdr ti)
       :cursor-source h))))

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
     :cursor (cdr ti)
     :cursor-source vec)))

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
  "Render record REC as shape-lock JSON.
EIEIO objects dispatch to `--inspect-eieio-object' which surfaces
class slot names and values.  Non-EIEIO records (bare records,
cl-defstruct) retain the Phase 1a stub: type tag + slot count only.

`cl-defstruct' introspection (type tag + named slots) lands in a
later chunk; 1b-c keeps it at the stub rather than growing scope."
  (cond
   ((and (fboundp 'eieio-object-p) (eieio-object-p rec))
    (anvil-sexp-cst--inspect-eieio-object rec))
   (t
    (anvil-sexp-cst--encode
     'record
     :length (max 0 (1- (length rec)))))))

(defun anvil-sexp-cst--eieio-slot-names (class)
  "Return the list of slot-name symbols defined on EIEIO CLASS.
Handles the Emacs 26+ `eieio-class-slots' API (vector of
`cl-slot-descriptor' structs) while staying tolerant of older
representations (plain vectors / name symbols) so a future Emacs
does not silently break introspection."
  (when (fboundp 'eieio-class-slots)
    (mapcar (lambda (sd)
              (cond
               ((fboundp 'cl--slot-descriptor-name)
                (cl--slot-descriptor-name sd))
               ((vectorp sd) (elt sd 1))
               (t sd)))
            (eieio-class-slots class))))

(defun anvil-sexp-cst--inspect-eieio-object (obj)
  "Render EIEIO OBJ as shape-lock JSON (type=eieio-object).
Entry keys are slot-name strings; values go through
`anvil-sexp-cst--repr'.  Unbound slots surface as `:unbound'
so the caller can tell them apart from an explicit nil."
  (let* ((class (eieio-object-class obj))
         (slot-names (anvil-sexp-cst--eieio-slot-names class))
         (total (length slot-names))
         (off (anvil-sexp-cst--clamp-offset total))
         (take (min (max 0 (- total off)) anvil-sexp-cst-top-limit))
         (window (cl-subseq slot-names off (+ off take)))
         (entries (cl-loop for sn in window
                           collect (list :k (symbol-name sn)
                                         :v (anvil-sexp-cst--repr
                                             (condition-case _err
                                                 (slot-value obj sn)
                                               (error :unbound))))))
         (ti (anvil-sexp-cst--window-truncation-info total off take obj)))
    (anvil-sexp-cst--encode
     'eieio-object
     :length total
     :entries (apply #'vector entries)
     :truncated (car ti)
     :cursor (cdr ti)
     :cursor-source obj)))

(defun anvil-sexp-cst--inspect-char-table (ct)
  "Render char-table CT as shape-lock JSON.
Enumerates via `map-char-table' and stops at
`anvil-sexp-cst-top-limit' ranges.  Entry keys are single char
codes (\"97\") or `FROM-TO' ranges (\"97-122\").  Drill on a
char-table returns the same first-N window: `map-char-table'
order is deterministic (ascending code points) but cannot be
cheaply resumed, so offset pagination is out of scope for 1b-c."
  (let* ((limit anvil-sexp-cst-top-limit)
         (count 0)
         entries)
    (catch 'cap
      (map-char-table
       (lambda (k v)
         (when (>= count limit) (throw 'cap nil))
         (let ((key-str (cond
                         ((integerp k) (format "%d" k))
                         ((consp k) (format "%d-%d" (car k) (cdr k)))
                         (t (format "%S" k)))))
           (push (list :k key-str :v (anvil-sexp-cst--repr v))
                 entries)
           (cl-incf count)))
       ct))
    (let* ((truncated (>= count limit))
           (cursor (and truncated (anvil-sexp-cst--make-cursor ct))))
      (anvil-sexp-cst--encode
       'char-table
       :length count
       :entries (apply #'vector (nreverse entries))
       :truncated truncated
       :cursor cursor
       :cursor-source ct))))


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
      ('char-table (anvil-sexp-cst--inspect-char-table value))
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


;;;###autoload
(defun anvil-inspect-object-purge (client-id)
  "Delete every inspect-object cursor stored for CLIENT-ID.
Drops every row in the `inspect-object/CLIENT-ID' namespace of
anvil-state (Doc 08).  Returns the integer deletion count (0
when the namespace was empty or anvil-state is not loaded).

Use at session end so stale state rows do not accumulate across
long-running daemons.  Signals `user-error' when CLIENT-ID is
missing or empty.

MCP Parameters:
  CLIENT-ID  Required client-id whose cursor namespace to drop"
  (anvil-server-with-error-handling
   (cond
    ((not (and client-id (stringp client-id)
               (not (string-empty-p client-id))))
     (user-error
      "anvil-inspect-object-purge: CLIENT-ID must be a non-empty string"))
    ((not (fboundp 'anvil-state-delete-ns)) 0)
    (t
     (let ((n (funcall (symbol-function 'anvil-state-delete-ns)
                       (format "inspect-object/%s" client-id))))
       (if (integerp n) n 0))))))


;;;; --- sexp-cst-read (Phase 2a) ------------------------------------------

(defun anvil-sexp-cst--grammar-usable-p ()
  "Return non-nil when tree-sitter-elisp can actually parse in this Emacs.
Same check `--maybe-warn-abi' uses but callable at handler time
so the read tool can return a typed error rather than crashing
when the grammar is missing."
  (and (fboundp 'treesit-available-p) (treesit-available-p)
       (fboundp 'treesit-ready-p)
       (ignore-errors (treesit-ready-p 'elisp t))))

(defun anvil-sexp-cst--pick-node (parser root position)
  "Walk from POSITION up to the nearest named node; fall back to ROOT.
When POSITION is nil, returns ROOT unchanged.  `treesit-node-at'
may land on an anonymous token (paren, whitespace) so the upward
walk is required to surface a structurally meaningful node."
  (if (null position)
      root
    (let ((node (ignore-errors (treesit-node-at position parser))))
      (while (and node (not (treesit-node-check node 'named)))
        (setq node (treesit-node-parent node)))
      (or node root))))

(defun anvil-sexp-cst--node-to-plist (node depth max-depth)
  "Serialize treesit NODE into a shape-locked plist.
DEPTH is the current recursion level (0 at the entry node); once
DEPTH reaches MAX-DEPTH the node emits its raw source text and an
empty children array with `:truncated' = t when it had children
that got pruned.  Leaf nodes (zero children) emit text regardless
of depth."
  (let* ((type (treesit-node-type node))
         (start (treesit-node-start node))
         (end (treesit-node-end node))
         (named (and (treesit-node-check node 'named) t))
         (child-count (treesit-node-child-count node))
         (at-cap (>= depth max-depth)))
    (cond
     ((or (zerop child-count) at-cap)
      (list :type type :start start :end end
            :named (if named t :false)
            :text (treesit-node-text node t)
            :children (vector)
            :truncated (if (and at-cap (> child-count 0)) t :false)))
     (t
      (let (kids)
        (dotimes (i child-count)
          (push (anvil-sexp-cst--node-to-plist
                 (treesit-node-child node i)
                 (1+ depth) max-depth)
                kids))
        (list :type type :start start :end end
              :named (if named t :false)
              :children (apply #'vector (nreverse kids))
              :truncated :false))))))

;;;###autoload
(defun anvil-sexp-cst-read (file &optional position max-depth)
  "Return a comment-preserving CST for FILE as shape-locked JSON.

FILE        Path to an `.el' source file (absolute, or relative to
            `default-directory').
POSITION    Optional 1-based point offset; when given, the innermost
            named node enclosing POSITION is returned.  Omit to get
            the whole `source_file' root.
MAX-DEPTH   Optional recursion cap (integer or digit string); defaults
            to `anvil-sexp-cst-read-max-depth'.

Output shape:
  {type, start, end, named, text?, children[], truncated}

Typed errors (envelope = {type:\"error\", truncated:false,
error:{kind, message}}):
  sexp-cst/missing-file        — FILE is nil or empty
  sexp-cst/file-not-found      — path does not resolve
  sexp-cst/grammar-unavailable — tree-sitter-elisp not loadable

MCP Parameters:
  FILE        Required elisp source path
  POSITION    Optional 1-based point offset
  MAX-DEPTH   Optional recursion cap"
  (anvil-server-with-error-handling
   (cond
    ((not (and file (stringp file) (not (string-empty-p file))))
     (anvil-sexp-cst--encode-error
      'error "sexp-cst/missing-file"
      "FILE argument is required."))
    ((not (file-exists-p file))
     (anvil-sexp-cst--encode-error
      'error "sexp-cst/file-not-found"
      (format "No such file: %s" file)))
    ((not (anvil-sexp-cst--grammar-usable-p))
     (anvil-sexp-cst--encode-error
      'error "sexp-cst/grammar-unavailable"
      "tree-sitter-elisp grammar is not loadable in this Emacs session."))
    (t
     (let* ((pos (anvil-sexp-cst--coerce-int position nil))
            (depth-cap (anvil-sexp-cst--coerce-int
                        max-depth anvil-sexp-cst-read-max-depth)))
       (with-temp-buffer
         (insert-file-contents file)
         (let* ((parser (treesit-parser-create 'elisp))
                (root (treesit-parser-root-node parser))
                (node (anvil-sexp-cst--pick-node parser root pos))
                (plist (anvil-sexp-cst--node-to-plist node 0 depth-cap)))
           (json-serialize plist :false-object :false :null-object :null))))))))


;;;; --- sexp-cst-edit (Phase 2b-a, dry-run) -------------------------------

(defun anvil-sexp-cst--reparse-and-locate (content start-0based)
  "Re-parse CONTENT with tree-sitter-elisp and describe the named
node at START-0BASED.  Returns either `(ok . PLIST)' with the
node info or `(error . MESSAGE)' when the re-parsed tree contains
any ERROR node.  Called from the Phase 2b-a edit dry-run path to
verify a proposed replacement keeps the whole file structurally
valid."
  (with-temp-buffer
    (insert content)
    (let* ((parser (treesit-parser-create 'elisp))
           (root (treesit-parser-root-node parser)))
      (cond
       ((treesit-node-check root 'has-error)
        (cons 'error
              "Re-parsed file contains tree-sitter ERROR nodes; replacement rejected."))
       (t
        (let* ((new-pos (1+ start-0based))
               (node (anvil-sexp-cst--pick-node parser root new-pos)))
          (cons 'ok
                (list :type (treesit-node-type node)
                      :start (treesit-node-start node)
                      :end (treesit-node-end node)
                      :text (treesit-node-text node t)))))))))

(defun anvil-sexp-cst--edit-compute (file pos new-text)
  "Internal: read FILE, splice NEW-TEXT over the node at POS, verify.
Returns a plist tagged with `:status :ok' + `:plist EDIT-RESULT' on
success, or `:status :error' + `:kind' + `:message' on any
failure reachable from here (no-node-at-position /
parse-error-in-replacement).  Factored out so both the dry-run
`anvil-sexp-cst-edit' and the on-disk `anvil-sexp-cst-edit-write'
can share the validation path without re-implementing it."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((parser (treesit-parser-create 'elisp))
           (root (treesit-parser-root-node parser))
           (node (anvil-sexp-cst--pick-node parser root pos))
           (before-text (buffer-string))
           (before-bytes (string-bytes before-text)))
      (cond
       ((or (null node) (eq node root))
        (list :status :error
              :kind "sexp-cst/no-node-at-position"
              :message (format "No named node at position %d" pos)))
       (t
        (let* ((old-start (treesit-node-start node))
               (old-end (treesit-node-end node))
               (old-text (treesit-node-text node t))
               (new-content
                (concat (substring before-text 0 (1- old-start))
                        new-text
                        (substring before-text (1- old-end))))
               (reparse (anvil-sexp-cst--reparse-and-locate
                         new-content (1- old-start))))
          (if (eq (car reparse) 'error)
              (list :status :error
                    :kind "sexp-cst/parse-error-in-replacement"
                    :message (cdr reparse))
            (let* ((after-plist (cdr reparse))
                   (before-plist (list :type (treesit-node-type node)
                                       :start old-start
                                       :end old-end
                                       :text old-text))
                   (after-bytes (string-bytes new-content)))
              (list :status :ok
                    :plist (list :type "edit-result"
                                 :before before-plist
                                 :after after-plist
                                 :file-before-bytes before-bytes
                                 :file-after-bytes after-bytes
                                 :new-content new-content))))))))))

(defun anvil-sexp-cst--edit-dry-run (file pos new-text)
  "Thin JSON wrapper around `--edit-compute' for `anvil-sexp-cst-edit'."
  (let ((r (anvil-sexp-cst--edit-compute file pos new-text)))
    (if (eq (plist-get r :status) :error)
        (anvil-sexp-cst--encode-error
         'error (plist-get r :kind) (plist-get r :message))
      (json-serialize (plist-get r :plist)
                      :false-object :false
                      :null-object :null))))

;;;###autoload
(defun anvil-sexp-cst-edit (file position new-text)
  "Replace the named node at POSITION in FILE with NEW-TEXT (dry-run).

Does NOT modify FILE on disk — the caller reads `new-content' from
the returned JSON and applies it via `file-replace-string' or an
equivalent tool.  The point of 2b-a's dry-run is to keep edit
tooling MCP-safe: a tool call never rewrites a file the caller
did not intend to commit.

Validates the proposed result by re-parsing the full patched
content through tree-sitter-elisp.  Any ERROR node in the reparse
(unbalanced parens, stray quotes, …) rejects the edit with
`sexp-cst/parse-error-in-replacement' so LLMs that emit half-
written forms cannot silently corrupt the source.

Output shape:
  {type:\"edit-result\",
   before:{type, start, end, text},
   after: {type, start, end, text},
   file-before-bytes, file-after-bytes, new-content}

Typed errors:
  sexp-cst/missing-file             — FILE nil / empty
  sexp-cst/file-not-found           — path absent
  sexp-cst/grammar-unavailable      — treesit-elisp not loadable
  sexp-cst/position-required        — POSITION missing / non-positive
  sexp-cst/missing-new-text         — NEW-TEXT missing
  sexp-cst/no-node-at-position      — nothing named encloses POSITION
  sexp-cst/parse-error-in-replacement — re-parse failed

MCP Parameters:
  FILE      Required elisp source path
  POSITION  Required 1-based point offset
  NEW-TEXT  Required replacement text"
  (anvil-server-with-error-handling
   (let ((pos (anvil-sexp-cst--coerce-int position nil)))
     (cond
      ((not (and file (stringp file) (not (string-empty-p file))))
       (anvil-sexp-cst--encode-error
        'error "sexp-cst/missing-file" "FILE argument is required."))
      ((not (file-exists-p file))
       (anvil-sexp-cst--encode-error
        'error "sexp-cst/file-not-found"
        (format "No such file: %s" file)))
      ((not (anvil-sexp-cst--grammar-usable-p))
       (anvil-sexp-cst--encode-error
        'error "sexp-cst/grammar-unavailable"
        "tree-sitter-elisp grammar is not loadable in this Emacs session."))
      ((or (null pos) (not (integerp pos)) (< pos 1))
       (anvil-sexp-cst--encode-error
        'error "sexp-cst/position-required"
        "POSITION must be a positive 1-based point offset."))
      ((not (and new-text (stringp new-text)))
       (anvil-sexp-cst--encode-error
        'error "sexp-cst/missing-new-text"
        "NEW-TEXT argument is required."))
      (t
       (anvil-sexp-cst--edit-dry-run file pos new-text))))))


;;;; --- sexp-cst-edit-write (Phase 2b-b, on-disk apply) -------------------

(defun anvil-sexp-cst--make-backup-path (file suffix)
  "Return a unique backup path for FILE using SUFFIX (or timestamp default).
Increments a trailing counter when a collision occurs so concurrent
calls never clobber each other's backups."
  (let* ((suf (or (and (stringp suffix) (not (string-empty-p suffix)) suffix)
                  (format-time-string ".%Y%m%dT%H%M%S.bak")))
         (base (concat file suf))
         (path base)
         (n 1))
    (while (file-exists-p path)
      (setq path (format "%s.%d" base n))
      (cl-incf n))
    path))

;;;###autoload
(defun anvil-sexp-cst-edit-write (file position new-text &optional backup-suffix)
  "Apply the node replacement from `anvil-sexp-cst-edit' to disk.

Runs the same validation as the dry-run variant — including the
full-file re-parse that rejects replacements leaving ERROR nodes
anywhere in the source.  On success, copies FILE to a backup path
(default suffix `.<iso-timestamp>.bak', overridable via
BACKUP-SUFFIX) and writes the patched content back to FILE.  On
validation failure NOTHING is written — the file, backup, and
surrounding directory are left untouched.

Returns a JSON string whose top-level type is `\"edit-write\"' on
success; shape extends the dry-run envelope with a `:backup-path'
field so callers can rollback via a simple rename.

Typed errors (envelope same as dry-run plus):
  sexp-cst/backup-failed  — `copy-file' signalled
  sexp-cst/write-failed   — disk write signalled

MCP Parameters:
  FILE            Required elisp source path
  POSITION        Required 1-based point offset
  NEW-TEXT        Required replacement text
  BACKUP-SUFFIX   Optional deterministic suffix for the backup file"
  (anvil-server-with-error-handling
   (let ((pos (anvil-sexp-cst--coerce-int position nil)))
     (cond
      ((not (and file (stringp file) (not (string-empty-p file))))
       (anvil-sexp-cst--encode-error
        'error "sexp-cst/missing-file" "FILE argument is required."))
      ((not (file-exists-p file))
       (anvil-sexp-cst--encode-error
        'error "sexp-cst/file-not-found"
        (format "No such file: %s" file)))
      ((not (anvil-sexp-cst--grammar-usable-p))
       (anvil-sexp-cst--encode-error
        'error "sexp-cst/grammar-unavailable"
        "tree-sitter-elisp grammar is not loadable in this Emacs session."))
      ((or (null pos) (not (integerp pos)) (< pos 1))
       (anvil-sexp-cst--encode-error
        'error "sexp-cst/position-required"
        "POSITION must be a positive 1-based point offset."))
      ((not (and new-text (stringp new-text)))
       (anvil-sexp-cst--encode-error
        'error "sexp-cst/missing-new-text"
        "NEW-TEXT argument is required."))
      (t
       (let ((r (anvil-sexp-cst--edit-compute file pos new-text)))
         (if (eq (plist-get r :status) :error)
             (anvil-sexp-cst--encode-error
              'error (plist-get r :kind) (plist-get r :message))
           (let* ((plist (plist-get r :plist))
                  (new-content (plist-get plist :new-content))
                  (backup-path (anvil-sexp-cst--make-backup-path
                                file backup-suffix)))
             (condition-case err
                 (progn
                   (copy-file file backup-path nil t t)
                   (with-temp-file file
                     (set-buffer-file-coding-system 'utf-8-unix)
                     (insert new-content))
                   (let* ((out (copy-sequence plist))
                          (out (plist-put out :type "edit-write"))
                          (out (plist-put out :backup-path backup-path)))
                     (json-serialize out
                                     :false-object :false
                                     :null-object :null)))
               (error
                ;; Best-effort cleanup: if backup exists but write
                ;; failed, restore from the backup.
                (when (file-exists-p backup-path)
                  (ignore-errors (copy-file backup-path file t t t)))
                (anvil-sexp-cst--encode-error
                 'error "sexp-cst/write-failed"
                 (format "Disk write failed: %S" err))))))))))))


;;;; --- sexp-cst-repair (Phase 3a, close-paren balancing) -----------------

(defun anvil-sexp-cst--paren-delta (content)
  "Return signed paren imbalance for CONTENT.
Positive N = N unclosed `(' (need N closes appended).
Negative N = N excess `)' (need |N| opens prepended).
Tracks strings (\") and line comments (;) manually so `parse-
partial-sexp' cannot abort mid-scan on a broken input — the whole
point of the repair tool is to accept inputs the real parser
rejects."
  (let ((delta 0)
        (i 0)
        (n (length content))
        (in-string nil)
        (in-comment nil))
    (while (< i n)
      (let ((c (aref content i)))
        (cond
         (in-comment
          (when (eq c ?\n) (setq in-comment nil))
          (cl-incf i))
         (in-string
          (cond
           ((eq c ?\\)
            (setq i (min n (+ i 2))))
           ((eq c ?\")
            (setq in-string nil) (cl-incf i))
           (t (cl-incf i))))
         (t
          (cond
           ((eq c ?\;)
            (setq in-comment t) (cl-incf i))
           ((eq c ?\")
            (setq in-string t) (cl-incf i))
           ((eq c ?\?)
            ;; Character literal like ?( or ?\( : skip the `?' plus
            ;; its payload so its paren does not count.
            (cl-incf i)
            (when (and (< i n) (eq (aref content i) ?\\))
              (cl-incf i))
            (when (< i n) (cl-incf i)))
           ((eq c ?\()
            (cl-incf delta) (cl-incf i))
           ((eq c ?\))
            (cl-decf delta) (cl-incf i))
           (t (cl-incf i)))))))
    delta))

(defun anvil-sexp-cst--content-has-error-p (content)
  "Return non-nil when tree-sitter-elisp finds ERROR nodes in CONTENT."
  (with-temp-buffer
    (insert content)
    (let* ((parser (treesit-parser-create 'elisp))
           (root (treesit-parser-root-node parser)))
      (treesit-node-check root 'has-error))))

(defun anvil-sexp-cst--repair-compute (file)
  "Read FILE, try to balance parens, return a plist.
On success: (:status :ok :plist PAYLOAD).
On failure: (:status :error :kind KIND :message MSG).  The only
`kind' this phase emits is `sexp-cst/repair-failed' — other error
kinds (missing-file, grammar-unavailable) are checked by the
caller before we get here."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((content (buffer-string))
           (bytes-before (string-bytes content))
           (has-error-before (anvil-sexp-cst--content-has-error-p content))
           (delta (anvil-sexp-cst--paren-delta content)))
      (cond
       ((and (zerop delta) (not has-error-before))
        (list :status :ok
              :plist (list :type "repair-result"
                           :before (list :bytes bytes-before
                                         :paren-delta 0
                                         :has-error :false)
                           :after (list :bytes bytes-before
                                        :paren-delta 0
                                        :has-error :false)
                           :fix (list :kind "noop" :added 0 :position 0)
                           :repaired-content content)))
       ((> delta 0)
        (let* ((repaired (concat content (make-string delta ?\))))
               (still-error (anvil-sexp-cst--content-has-error-p repaired)))
          (if still-error
              (list :status :error
                    :kind "sexp-cst/repair-failed"
                    :message
                    (format "Appended %d close-paren(s) but tree-sitter still reports ERROR nodes; damage beyond paren imbalance." delta))
            (list :status :ok
                  :plist (list :type "repair-result"
                               :before (list :bytes bytes-before
                                             :paren-delta delta
                                             :has-error (if has-error-before t :false))
                               :after (list :bytes (string-bytes repaired)
                                            :paren-delta 0
                                            :has-error :false)
                               :fix (list :kind "close-paren-added"
                                          :added delta
                                          :position (1+ (length content)))
                               :repaired-content repaired)))))
       ((< delta 0)
        (let* ((missing (- delta))
               (repaired (concat (make-string missing ?\() content))
               (still-error (anvil-sexp-cst--content-has-error-p repaired)))
          (if still-error
              (list :status :error
                    :kind "sexp-cst/repair-failed"
                    :message
                    (format "Prepended %d open-paren(s) but tree-sitter still reports ERROR nodes; damage beyond paren imbalance." missing))
            (list :status :ok
                  :plist (list :type "repair-result"
                               :before (list :bytes bytes-before
                                             :paren-delta delta
                                             :has-error (if has-error-before t :false))
                               :after (list :bytes (string-bytes repaired)
                                            :paren-delta 0
                                            :has-error :false)
                               :fix (list :kind "open-paren-prepended"
                                          :added missing
                                          :position 1)
                               :repaired-content repaired)))))
       (t
        (list :status :error
              :kind "sexp-cst/repair-failed"
              :message
              "Parens are balanced but tree-sitter still reports ERROR nodes; close-paren balancing cannot help."))))))

;;;###autoload
(defun anvil-sexp-cst-repair (file)
  "Close-paren balance FILE; return a dry-run JSON describing the fix.

Does NOT modify FILE on disk — the caller reads `repaired-content'
from the returned JSON and applies it via `file-replace-string',
`sexp-cst-edit-write' (whole-file overwrite), or an equivalent
tool.  Phase 3a deliberately avoids an on-disk variant so LLM
tool calls cannot silently rewrite source that failed to parse.

Strategy (parinfer fallback, close-paren balancing only):
  1. Count `(' vs `)' outside strings/comments/char-literals.
  2. If delta > 0, append `delta' close-parens at EOF.
  3. If delta < 0, prepend |delta| open-parens at BOF.
  4. Re-parse with tree-sitter-elisp.  If ERROR nodes persist,
     return `sexp-cst/repair-failed' — damage is beyond our scope.

Output shape:
  {type:\"repair-result\",
   before:{bytes, paren-delta, has-error},
   after: {bytes, paren-delta, has-error},
   fix:   {kind:\"noop\"|\"close-paren-added\"|\"open-paren-prepended\",
           added, position},
   repaired-content}

Typed errors:
  sexp-cst/missing-file        — FILE nil / empty
  sexp-cst/file-not-found      — path absent
  sexp-cst/grammar-unavailable — tree-sitter-elisp not loadable
  sexp-cst/repair-failed       — paren balancing did not converge

MCP Parameters:
  FILE   Required elisp source path (may contain unbalanced parens)"
  (anvil-server-with-error-handling
   (cond
    ((not (and file (stringp file) (not (string-empty-p file))))
     (anvil-sexp-cst--encode-error
      'error "sexp-cst/missing-file" "FILE argument is required."))
    ((not (file-exists-p file))
     (anvil-sexp-cst--encode-error
      'error "sexp-cst/file-not-found"
      (format "No such file: %s" file)))
    ((not (anvil-sexp-cst--grammar-usable-p))
     (anvil-sexp-cst--encode-error
      'error "sexp-cst/grammar-unavailable"
      "tree-sitter-elisp grammar is not loadable in this Emacs session."))
    (t
     (let ((r (anvil-sexp-cst--repair-compute file)))
       (if (eq (plist-get r :status) :error)
           (anvil-sexp-cst--encode-error
            'error (plist-get r :kind) (plist-get r :message))
         (json-serialize (plist-get r :plist)
                         :false-object :false
                         :null-object :null)))))))


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
   :read-only t)
  (anvil-server-register-tool
   #'anvil-inspect-object-purge
   :id "sexp-cst-inspect-object-purge"
   :server-id anvil-sexp-cst--server-id
   :description
   "Delete every inspect-object cursor stored for CLIENT-ID.  Returns the
deletion count.  Intended for session-end cleanup so stale state rows do
not accumulate across long-running daemons.  Write tool.")
  (anvil-server-register-tool
   #'anvil-sexp-cst-read
   :id "sexp-cst-read"
   :server-id anvil-sexp-cst--server-id
   :description
   "Parse FILE with tree-sitter-elisp and return a comment-preserving CST
as JSON.  Output shape: {type, start, end, named, text?, children[],
truncated}.  POSITION zooms to the smallest named node enclosing that
point offset; omit for the whole file.  MAX-DEPTH caps recursion (deeper
children are replaced with text + truncated=t).  Typed errors:
sexp-cst/missing-file, sexp-cst/file-not-found, sexp-cst/grammar-unavailable.
Read-only."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-sexp-cst-edit
   :id "sexp-cst-edit"
   :server-id anvil-sexp-cst--server-id
   :description
   "Replace the named node at POSITION in FILE with NEW-TEXT (dry-run).
Does NOT modify FILE — returns the proposed new content in `new-content'
for the caller to apply via `file-replace-string'.  Validates that the
re-parsed full file contains no ERROR nodes.  Output shape:
{before, after, file-before-bytes, file-after-bytes, new-content}.
Typed errors cover missing input, missing files, grammar unavailability,
no-node-at-position, and parse-error-in-replacement.  Read-only (does
not touch disk)."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-sexp-cst-edit-write
   :id "sexp-cst-edit-write"
   :server-id anvil-sexp-cst--server-id
   :description
   "Apply `sexp-cst-edit' to disk with backup.  Runs the same validation
(re-parsed file must be free of ERROR nodes); on success copies FILE to
a backup path (default `.<iso-timestamp>.bak', override via
BACKUP-SUFFIX) then writes the patched content to FILE.  On validation
failure the file, backup, and directory are left untouched.  Returns a
JSON object with `type=\"edit-write\"' + `backup-path' plus the dry-run
fields (before/after/new-content).  Write tool.")
  (anvil-server-register-tool
   #'anvil-sexp-cst-repair
   :id "sexp-cst-repair"
   :server-id anvil-sexp-cst--server-id
   :description
   "Close-paren balance an elisp FILE that fails to parse.  Counts `(' /
`)' outside strings/comments/char-literals, appends missing close-parens
at EOF (or prepends opens at BOF when closes exceed opens), re-parses
with tree-sitter-elisp to verify ERROR nodes are gone.  Returns a
dry-run JSON: {type:\"repair-result\", before, after, fix{kind,added,
position}, repaired-content}; caller applies the patched content via a
separate write tool.  Typed error sexp-cst/repair-failed when balancing
does not converge (damage beyond paren imbalance).  Read-only."
   :read-only t))

(defun anvil-sexp-cst--unregister-tools ()
  "Remove every sexp-cst-* MCP tool."
  (dolist (id '("sexp-cst-inspect-object"
                "sexp-cst-inspect-object-drill"
                "sexp-cst-inspect-object-purge"
                "sexp-cst-read"
                "sexp-cst-edit"
                "sexp-cst-edit-write"
                "sexp-cst-repair"))
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
