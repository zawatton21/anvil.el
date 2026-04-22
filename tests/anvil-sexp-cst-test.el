;;; anvil-sexp-cst-test.el --- Shape-locked tests for anvil-sexp-cst  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; These tests lock in the JSON output contract of `anvil-inspect-object'
;; (Doc 31 Phase 1a) before the module ships.  Codex's TDD-lite review
;; recommendation: freeze the output shape in a test, hand the test to
;; the implementer, let it drive the code.
;;
;; Until `anvil-sexp-cst' is in place every test here `skip-unless' the
;; symbol is `fboundp', so they show up as "skipped" in batch runs rather
;; than erroring.  The moment Phase 1a lands, the skip clears and every
;; assertion becomes enforceable.  Removing or relaxing a shape lock is
;; *only* acceptable as a deliberate spec change, reviewed alongside the
;; Doc 31 update.
;;
;; What is locked
;; --------------
;;   * top-level JSON keys (`type' / optional `length' / `entries' /
;;     `truncated' / optional `cursor' / optional `error')
;;   * entry object shape (`k' and `v', both strings)
;;   * type-tag spelling for the core 6-8 types
;;   * stub contract for `record' (type + length, empty entries)
;;   * truncation signal: `truncated' = t AND `cursor' present AND
;;     cursor namespace begins with `inspect-object/<client-id>/'
;;   * typed error envelope for circular references
;;
;; What is not locked (yet)
;; ------------------------
;;   * exact bytes / key ordering of the JSON encoder
;;   * depth-N drill semantics (Phase 1b)
;;   * char-table / EIEIO representation (Phase 1b)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'anvil-sexp-cst nil t)

;;;; --- fixture helpers ----------------------------------------------------

(defconst anvil-sexp-cst-test--client-id "test-client-abc123"
  "Stable client-id used by every shape-lock test.
The locked cursor namespace is
  `inspect-object/<client-id>/<uuid>' (Doc 31, consensus critique).
Using a constant here lets us assert the prefix exactly.")

(defun anvil-sexp-cst-test--available-p (&optional type)
  "Return non-nil when `anvil-inspect-object' is implemented.
With TYPE (a symbol like `integer' / `alist' / …) also require the
module to have declared it in `anvil-sexp-cst-supported-types'.
Chunks of Phase 1a can ship type families in separate commits —
tests for unshipped types stay skipped until the module adds
their tag to the list."
  (and (fboundp 'anvil-inspect-object)
       (or (null type)
           (not (boundp 'anvil-sexp-cst-supported-types))
           (memq type anvil-sexp-cst-supported-types))))

(defun anvil-sexp-cst-test--invoke (value)
  "Call `anvil-inspect-object' on VALUE with the stable client-id.
Return the decoded JSON as a hash-table (object) or nil if the
tool returned no string.  Signals if the tool returns something
other than a JSON string."
  (let* ((raw (anvil-inspect-object value anvil-sexp-cst-test--client-id))
         (str (cond ((stringp raw) raw)
                    ;; Some implementations might return a plist and let the
                    ;; MCP layer encode — accept that too by re-encoding here.
                    ((listp raw) (json-encode raw))
                    (t (error "inspect-object returned non-string/list: %S" raw)))))
    (json-parse-string str :object-type 'hash-table
                       :array-type 'array
                       :null-object :null
                       :false-object :false)))

(defun anvil-sexp-cst-test--get (obj &rest keys)
  "Walk OBJ (hash-table) following KEYS, missing keys yield nil."
  (let ((cur obj))
    (while (and cur keys)
      (setq cur (and (hash-table-p cur)
                     (gethash (pop keys) cur))))
    cur))

(defun anvil-sexp-cst-test--entries-as-alist (obj)
  "Return OBJ's `entries' array as a ((K . V) ...) alist of strings.
Errors if any entry is missing either `k' or `v'."
  (let ((entries (anvil-sexp-cst-test--get obj "entries"))
        out)
    (unless (or (vectorp entries) (listp entries))
      (error "entries not array-like: %S" entries))
    (seq-doseq (e entries)
      (let ((k (anvil-sexp-cst-test--get e "k"))
            (v (anvil-sexp-cst-test--get e "v")))
        (unless (and (stringp k) (stringp v))
          (error "entry shape broken: %S" e))
        (push (cons k v) out)))
    (nreverse out)))


;;;; --- core type shape locks ---------------------------------------------

(ert-deftest anvil-sexp-cst-test-integer-shape ()
  "An integer round-trips as type=integer, length absent, scalar entry."
  (skip-unless (anvil-sexp-cst-test--available-p 'integer))
  (let ((obj (anvil-sexp-cst-test--invoke 42)))
    (should (equal (anvil-sexp-cst-test--get obj "type") "integer"))
    ;; scalars must not expose length (size is not a sequence measure).
    (should (null (anvil-sexp-cst-test--get obj "length")))
    (should (eq (anvil-sexp-cst-test--get obj "truncated") :false))
    (should (null (anvil-sexp-cst-test--get obj "cursor")))
    (should (null (anvil-sexp-cst-test--get obj "error")))))

(ert-deftest anvil-sexp-cst-test-nil-shape ()
  "nil reports as type=nil with no length or entries."
  (skip-unless (anvil-sexp-cst-test--available-p 'nil))
  (let ((obj (anvil-sexp-cst-test--invoke nil)))
    (should (equal (anvil-sexp-cst-test--get obj "type") "nil"))
    (should (null (anvil-sexp-cst-test--get obj "length")))
    (should (eq (anvil-sexp-cst-test--get obj "truncated") :false))))

(ert-deftest anvil-sexp-cst-test-symbol-shape ()
  "A symbol reports type=symbol; length MAY be present (name length)."
  (skip-unless (anvil-sexp-cst-test--available-p 'symbol))
  (let ((obj (anvil-sexp-cst-test--invoke 'alpha-beta-gamma)))
    (should (equal (anvil-sexp-cst-test--get obj "type") "symbol"))
    (should (eq (anvil-sexp-cst-test--get obj "truncated") :false))))

(ert-deftest anvil-sexp-cst-test-string-shape ()
  "A string reports type=string, length=char count, not truncated at 5 chars."
  (skip-unless (anvil-sexp-cst-test--available-p 'string))
  (let ((obj (anvil-sexp-cst-test--invoke "hello")))
    (should (equal (anvil-sexp-cst-test--get obj "type") "string"))
    (should (equal (anvil-sexp-cst-test--get obj "length") 5))
    (should (eq (anvil-sexp-cst-test--get obj "truncated") :false))))

(ert-deftest anvil-sexp-cst-test-list-shape ()
  "A proper list reports type=list with length and N entries."
  (skip-unless (anvil-sexp-cst-test--available-p 'list))
  (let* ((obj (anvil-sexp-cst-test--invoke '(1 2 3)))
         (entries (anvil-sexp-cst-test--entries-as-alist obj)))
    (should (equal (anvil-sexp-cst-test--get obj "type") "list"))
    (should (equal (anvil-sexp-cst-test--get obj "length") 3))
    (should (= (length entries) 3))
    (should (equal (cdr (nth 0 entries)) "1"))
    (should (equal (cdr (nth 1 entries)) "2"))
    (should (equal (cdr (nth 2 entries)) "3"))))

(ert-deftest anvil-sexp-cst-test-alist-shape ()
  "An alist reports type=alist, length = cell count, keys preserved as SEXP repr."
  (skip-unless (anvil-sexp-cst-test--available-p 'alist))
  (let* ((val '((a . 1) (b . 2) (c . 3)))
         (obj (anvil-sexp-cst-test--invoke val))
         (entries (anvil-sexp-cst-test--entries-as-alist obj)))
    (should (equal (anvil-sexp-cst-test--get obj "type") "alist"))
    (should (equal (anvil-sexp-cst-test--get obj "length") 3))
    (should (= (length entries) 3))
    ;; Keys must include the three symbols; do not pin exact quoting so
    ;; `a' vs `"a"' representation stays an implementation choice, but
    ;; we require the payload to be recoverable.
    (should (member "a" (mapcar (lambda (p) (replace-regexp-in-string "[\"' ]" "" (car p))) entries)))
    (should (member "b" (mapcar (lambda (p) (replace-regexp-in-string "[\"' ]" "" (car p))) entries)))
    (should (member "c" (mapcar (lambda (p) (replace-regexp-in-string "[\"' ]" "" (car p))) entries)))))

(ert-deftest anvil-sexp-cst-test-plist-shape ()
  "A plist (keyword-tagged) reports type=plist with paired keys/values."
  (skip-unless (anvil-sexp-cst-test--available-p 'plist))
  (let* ((val '(:x 1 :y 2 :z 3))
         (obj (anvil-sexp-cst-test--invoke val)))
    ;; Either "plist" (preferred by Doc 31 spec) or "list" is acceptable
    ;; in 1a if the implementation cannot distinguish — lock the wider
    ;; contract but fail on anything exotic.
    (should (member (anvil-sexp-cst-test--get obj "type") '("plist" "list")))
    (should (= (anvil-sexp-cst-test--get obj "length") 6))))

(ert-deftest anvil-sexp-cst-test-hash-table-shape ()
  "A hash-table reports type=hash-table, length = entry count."
  (skip-unless (anvil-sexp-cst-test--available-p 'hash-table))
  (let* ((h (make-hash-table :test 'equal))
         (_ (puthash "a" 1 h))
         (_ (puthash "b" 2 h))
         (_ (puthash "c" 3 h))
         (obj (anvil-sexp-cst-test--invoke h)))
    (should (equal (anvil-sexp-cst-test--get obj "type") "hash-table"))
    (should (equal (anvil-sexp-cst-test--get obj "length") 3))
    (should (eq (anvil-sexp-cst-test--get obj "truncated") :false))
    (should (= (length (anvil-sexp-cst-test--entries-as-alist obj)) 3))))

(ert-deftest anvil-sexp-cst-test-vector-shape ()
  "A vector reports type=vector, length = cell count."
  (skip-unless (anvil-sexp-cst-test--available-p 'vector))
  (let* ((obj (anvil-sexp-cst-test--invoke (vector 10 20 30))))
    (should (equal (anvil-sexp-cst-test--get obj "type") "vector"))
    (should (equal (anvil-sexp-cst-test--get obj "length") 3))
    (should (= (length (anvil-sexp-cst-test--entries-as-alist obj)) 3))))

(ert-deftest anvil-sexp-cst-test-cons-shape ()
  "A proper two-element cons (not an alist cell by itself) reports a pair-ish type."
  (skip-unless (anvil-sexp-cst-test--available-p 'cons))
  ;; Cons of scalars: we don't pin type to 'cons because the prototype
  ;; classifies '(a . b) as 'list'.  What we lock is: must not crash,
  ;; must return `type' string, `error' must be absent.
  (let ((obj (anvil-sexp-cst-test--invoke '(a . b))))
    (should (stringp (anvil-sexp-cst-test--get obj "type")))
    (should (null (anvil-sexp-cst-test--get obj "error")))))


;;;; --- stub contract (Phase 1a: type + length only) -----------------------

(cl-defstruct anvil-sexp-cst-test-sample name quota)

(ert-deftest anvil-sexp-cst-test-record-stub-shape ()
  "cl-struct records report type=record with length, empty entries (1a stub)."
  (skip-unless (anvil-sexp-cst-test--available-p 'record))
  (let* ((rec (make-anvil-sexp-cst-test-sample :name "top" :quota 42))
         (obj (anvil-sexp-cst-test--invoke rec)))
    (should (equal (anvil-sexp-cst-test--get obj "type") "record"))
    (should (integerp (anvil-sexp-cst-test--get obj "length")))
    ;; Phase 1a stub: entries array present but empty (drill lands in 1b).
    (let ((entries (anvil-sexp-cst-test--get obj "entries")))
      (should (or (null entries)
                  (and (vectorp entries) (= 0 (length entries)))
                  (and (listp entries)   (= 0 (length entries))))))))


;;;; --- truncation + cursor namespace --------------------------------------

(ert-deftest anvil-sexp-cst-test-truncation-emits-cursor ()
  "When truncation fires, cursor must be namespaced
`inspect-object/<client-id>/<uuid>' per Doc 31 consensus critique."
  (skip-unless (anvil-sexp-cst-test--available-p 'truncation))
  (let* ((huge (cl-loop for i below 5000
                        collect (cons (intern (format "long-key-%05d" i))
                                      (make-string 50 ?z))))
         (obj (anvil-sexp-cst-test--invoke huge))
         (truncated (anvil-sexp-cst-test--get obj "truncated"))
         (cursor    (anvil-sexp-cst-test--get obj "cursor")))
    (should (equal (anvil-sexp-cst-test--get obj "type") "alist"))
    (should (eq truncated t))
    (should (stringp cursor))
    ;; Namespace shape is the shipping contract, hard-lock it.
    (should (string-prefix-p
             (format "inspect-object/%s/" anvil-sexp-cst-test--client-id)
             cursor))
    ;; UUID component must be non-empty (no trailing slash alone).
    (let ((suffix (substring cursor
                             (length (format "inspect-object/%s/"
                                             anvil-sexp-cst-test--client-id)))))
      (should (> (length suffix) 0))
      (should-not (string-match-p "/" suffix)))))


;;;; --- typed error envelope (circular reference) -------------------------

(ert-deftest anvil-sexp-cst-test-circular-returns-typed-error ()
  "A self-referencing list must be reported as a typed error, not a crash."
  (skip-unless (anvil-sexp-cst-test--available-p 'circular))
  (let* ((cell (list 1 2 3))
         (_ (setcdr (last cell) cell))  ;; close the loop
         (obj (anvil-sexp-cst-test--invoke cell))
         (err (anvil-sexp-cst-test--get obj "error")))
    (should (hash-table-p err))
    (should (member (anvil-sexp-cst-test--get err "kind")
                    '("inspect-object/circular-reference"
                      "circular-reference")))
    (should (stringp (anvil-sexp-cst-test--get err "message")))))


;;;; --- meta-test: shape lock file is loaded and TDD-lite gate active -----

(ert-deftest anvil-sexp-cst-test-meta-locks-loaded ()
  "Shape-lock infrastructure itself must load regardless of impl state.
The whole point of the TDD-lite gate is that this file is valid,
evaluable, and discoverable by `ert-run-tests-batch-and-exit' even
when `anvil-sexp-cst' is entirely absent.  If this test does not
run, the suite is broken and every per-type shape-lock result
below should be treated as unverified."
  (should (fboundp 'anvil-sexp-cst-test--available-p))
  (should (fboundp 'anvil-sexp-cst-test--invoke))
  (should (fboundp 'anvil-sexp-cst-test--get))
  (should (stringp anvil-sexp-cst-test--client-id)))


(provide 'anvil-sexp-cst-test)
;;; anvil-sexp-cst-test.el ends here
