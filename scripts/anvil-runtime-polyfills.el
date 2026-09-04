;;; anvil-runtime-polyfills.el --- Substrate polyfills for standalone NeLisp -*- lexical-binding: t; -*-

;; Phase 7 (= 2026-05-10 follow-up to project_anvil_standalone_tools_wired):
;; Provide the substrate bits that GREEN-bucket anvil-* modules expect
;; but standalone NeLisp doesn't ship.  Loaded by
;; `scripts/anvil-runtime-shell-loop.el' between the emacs-init/stub
;; bootstrap and the tool-module load chain so that registrations and
;; tool-call handler bodies see a healthy substrate.
;;
;; Audited gaps (= host Emacs has it, NeLisp standalone doesn't):
;;   - anvil-discovery `tools-by-intent' / `usage-report'
;;       cl-copy-list, cl-remove-duplicates
;;   - anvil-sqlite `sqlite-query'
;;       cl-subseq + sqlite cursor protocol
;;       (sqlite-more-p / sqlite-next / sqlite-finalize)
;;       + `sqlite-select' return-shape override (= cursor on `'set')
;;   - anvil-bench (= deferred to Stage 2)
;;       benchmark-call / benchmark-elapse / benchmark-progn,
;;       profiler-start / profiler-stop / profiler-reset / profiler-cpu-log

;;; Code:

;; --- pre-vendor substrate (= primitives vendor *.el reaches at load time) --

;; Vendor `profiler.el' calls `(define-hash-table-test 'profiler-function-equal
;; ...)' at top level — without the C primitive its load aborts.  anvil callers
;; don't actually need the registered test to be wired into hash creation, so
;; a no-op recorder is enough.
(unless (fboundp 'define-hash-table-test)
  (defvar anvil-runtime-polyfills--hash-table-tests (make-hash-table)
    "Stub registry for `define-hash-table-test' polyfill.")
  (defun define-hash-table-test (name test-fn hash-fn)
    "Polyfill: record TEST-FN / HASH-FN under NAME (no wiring)."
    (puthash name (list test-fn hash-fn)
             anvil-runtime-polyfills--hash-table-tests)
    name))


;; --- vendor library bulk-load --------------------------------------

;; Directive (2026-05-10 user, two messages): substrate libraries
;; should come from `nelisp-emacs/vendor/emacs-lisp/` whenever possible
;; — that's where upstream Emacs `subr-x.el' / `seq.el' / `cl-*.el'
;; / `benchmark.el' / `profiler.el' / `url/' / `sqlite.el' / `json.el'
;; / `auth-source.el' / `jsonrpc.el' all live.  Prepend the relevant
;; vendor dirs to load-path and force-load the libraries anvil-* (and
;; its transitive deps) reach for.  Each load is wrapped in
;; condition-case so a failure on one library doesn't abort the rest;
;; `unless (featurep ...)' / `unless (fboundp ...)' gates further down
;; in this file remain as fallbacks for whichever vendor load fails.

(let* ((nelisp-emacs-root
        ;; Resolution order:
        ;;   1. `anvil-runtime-bootstrap-nelisp-emacs-dir' from bootstrap.el
        ;;      (= primary channel, set by bin/anvil-runtime before any load).
        ;;   2. explicit override (= caller `setq's the defvar).
        ;;   3. NELISP_EMACS_DIR env var (= getenv works after emacs-init).
        ;;   4. sibling-checkout fallback (= ../nelisp-emacs relative to
        ;;      anvil.el's repo root, which is one level above scripts/).
        (or (and (boundp 'anvil-runtime-bootstrap-nelisp-emacs-dir)
                 anvil-runtime-bootstrap-nelisp-emacs-dir)
            (and (boundp 'anvil-runtime-polyfills-nelisp-emacs-dir)
                 anvil-runtime-polyfills-nelisp-emacs-dir)
            (and (fboundp 'getenv)
                 (let ((env (getenv "NELISP_EMACS_DIR")))
                   (and env (> (length env) 0) env)))
            (and (boundp 'load-file-name) load-file-name
                 (concat (file-name-directory
                          (directory-file-name
                           (file-name-directory
                            (directory-file-name
                             (file-name-directory load-file-name)))))
                         "nelisp-emacs"))
            "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs"))
       (vendor-base (concat nelisp-emacs-root "/vendor/emacs-lisp"))
       (vendor-dirs (list vendor-base
                          (concat vendor-base "/emacs-lisp")
                          (concat vendor-base "/url"))))
  (dolist (dir vendor-dirs)
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

(dolist (lib '("subr-x" "seq" "cl-extra" "cl-seq" "benchmark" "profiler"))
  ;; Under post-2026-05-17 nelisp the pure-elisp interpreter allocates
  ;; ~1 MB/s walking each of these vendor files and never reclaims it,
  ;; so unconditional `load' burns substrate memory unboundedly.  The
  ;; caller (= shell-loop.el / server-loop.el) pre-provides the 6
  ;; feature names before loading us; skip the load when the feature is
  ;; already provided so the burn does not happen.  In host Emacs the
  ;; vendor files load fast and the pre-provide isn't done, so the
  ;; original path is preserved there.
  (unless (featurep (intern lib))
    (condition-case anvil-runtime-polyfills--vendor-err
        (load lib nil t)
      (error
       (when (fboundp 'nelisp--write-stderr-line)
         (nelisp--write-stderr-line
          (concat "[anvil-runtime-polyfills] vendor load `" lib "' failed: "
                  (format "%S" anvil-runtime-polyfills--vendor-err))))))))

;; emacs-stub-bulk binds many subr-x bits unconditionally even when
;; vendor subr-x didn't load — `provide' ensures `(require 'subr-x)'
;; from anvil-* downstreams still satisfies regardless.
(unless (featurep 'subr-x)
  (provide 'subr-x))


;; --- tabulated-list.el stub (UI module, not used in MCP tool path) ---

;; `anvil-orchestrator' does `(require 'tabulated-list)' at load time so
;; it can render the interactive `anvil-orchestrator-status' dashboard
;; buffer.  Standalone NeLisp's reader cannot yet parse multibyte char
;; literals like `?▼' (line 46 col 50 of the vendor file), so loading
;; the real `tabulated-list.el' fails.  The MCP tool surface
;; (`orchestrator-submit', `orchestrator-status' returning JSON, etc.)
;; never reaches the UI render path, so a `provide'-only stub suffices.
(unless (featurep 'tabulated-list)
  (provide 'tabulated-list)
  ;; Minimal mode shell so any top-level form referencing the symbol
  ;; (e.g. `(define-derived-mode foo tabulated-list-mode ...)') resolves.
  (unless (fboundp 'tabulated-list-mode)
    (defun tabulated-list-mode () nil))
  (unless (boundp 'tabulated-list-format)
    (defvar tabulated-list-format nil))
  (unless (boundp 'tabulated-list-entries)
    (defvar tabulated-list-entries nil)))


;; --- subr.el cherry-pick polyfills ----------------------------------

;; `apply-partially' lives in vendor/emacs-lisp/subr.el but loading the
;; whole subr.el pulls many primitives standalone NeLisp can't satisfy.
;; Cherry-pick the function — it's a 2-liner — so that
;; `anvil-server--make-encoded-handler' (= the registration path that
;; wraps every `(anvil-server-register-tools ...)' handler) doesn't trip
;; on `void-function: apply-partially' and silently leave every tool
;; unregistered.  Reason this matters: the symbol returned from
;; `--make-encoded-handler' is what gets passed into
;; `anvil-server-register-tool', whose `(functionp handler)' gate then
;; rejects it because `fset' never ran.
(unless (fboundp 'apply-partially)
  (defun apply-partially (fun &rest args)
    "Return a function that is a partial application of FUN to ARGS.
ARGS is a list of the first N arguments to pass to FUN.
The result is a new function which does the same as FUN, except that
the first N arguments are fixed at the values with which this function
was called."
    (lambda (&rest args2)
      (apply fun (append args args2)))))

;; `functionp' on standalone NeLisp's `lisp/nelisp-stdlib.el' only
;; recognises cons-form lambdas (`(lambda ...)' / `(closure ...)' /
;; `(builtin ...)') and returns nil for any symbol — including
;; uninterned symbols whose function cell is bound via `fset'.
;; `anvil-server--make-encoded-handler' returns precisely such an
;; uninterned symbol and `anvil-server-register-tool''s
;; `(unless (functionp handler) (error ...))' gate rejects it,
;; so every `:server-id'-scoped tool batch silently fails.
;;
;; Override unconditionally (= shadow the stdlib defun) so symbol
;; handlers register correctly.  `defun' is a no-op on host Emacs where
;; the native `functionp' already handles symbols, but we still run
;; through `unless (anvil-runtime-polyfills--functionp-ok-p)' so the
;; host implementation stays canonical.
(defun anvil-runtime-polyfills--functionp-ok-p ()
  "Return non-nil when the running `functionp' already recognises symbols
with bound function cells (= host Emacs / future NeLisp with the fix)."
  (let ((s (make-symbol "anvil-runtime-polyfills--probe")))
    (fset s (lambda (x) x))
    (functionp s)))
(unless (anvil-runtime-polyfills--functionp-ok-p)
  (defun functionp (x)
    "Return t if X is callable.  Polyfill (2026-05-11): extends the
standalone NeLisp `lisp/nelisp-stdlib.el' impl to recognise any
symbol whose function cell is bound (interned or uninterned)."
    (cond
     ((and (consp x) (memq (car x) '(lambda closure builtin))) t)
     ;; `fboundp' signals `wrong-type-argument' for nil even though
     ;; `(symbolp nil) = t', so guard explicitly.
     ((and x (symbolp x) (fboundp x)) t)
     (t nil))))


;; --- env info vars --------------------------------------------------

;; anvil-bench reports `emacs-version' / `system-type' in tool result
;; env info.  Standalone NeLisp does not declare them, so fill with
;; sensible identifiers — runs on real Emacs are unaffected because the
;; defvar gates on `boundp'.

(unless (boundp 'emacs-version)
  (defvar emacs-version "30.0 (nelisp-standalone)"
    "Polyfill: identifier string used by anvil-bench env reporting."))

(unless (boundp 'system-type)
  (defvar system-type 'gnu/linux
    "Polyfill: rough OS family for env reporting.  Real value comes
from a NeLisp-side primitive when one is exposed."))

(unless (boundp 'gc-cons-threshold)
  (defvar gc-cons-threshold 800000
    "Polyfill: Emacs GC threshold knob.  anvil-bench let-binds this to
`most-positive-fixnum' to disable GC during measurement; the binding
is harmless on standalone NeLisp because there's no GC interlock to
disable, but the symbol must exist."))

(unless (boundp 'most-positive-fixnum)
  (defvar most-positive-fixnum (1- (lsh 1 61))
    "Polyfill: large-fixnum sentinel used by anvil-bench's
`gc-cons-threshold' override.  Approximation = 2^61-1 (= 64-bit fixnum
ceiling on most platforms)."))


;; --- math primitive gaps -------------------------------------------

;; `expt' (= base ^ exp) is a C builtin in real Emacs but missing on
;; standalone NeLisp.  anvil-bench's stddev calculation uses
;; `(expt diff 2)' so we need at least integer-exponent semantics.
(unless (fboundp 'expt)
  (defun expt (base exponent)
    "Polyfill: integer-exponent power.
Negative exponents return 1/base^|exponent|.  Non-integer exponents
are not yet supported (= signals an error rather than silently
returning a wrong result)."
    (cond
     ((not (integerp exponent))
      (error "expt polyfill: non-integer exponent %S unsupported"
             exponent))
     ((zerop exponent) 1)
     ((< exponent 0)
      (/ 1.0 (expt base (- exponent))))
     (t
      (let ((result 1) (i 0))
        (while (< i exponent)
          (setq result (* result base))
          (setq i (1+ i)))
        result)))))

(unless (fboundp 'sqrt)
  (defun sqrt (x)
    "Polyfill: Newton-Raphson sqrt for non-negative numbers.
Used by anvil-bench's stddev (sqrt of variance)."
    (cond
     ((zerop x) 0)
     ((< x 0) (error "sqrt polyfill: negative input %S" x))
     (t
      (let ((guess (float x)) (epsilon 1e-12))
        (let ((prev (1+ guess)))
          (while (> (abs (- guess prev)) epsilon)
            (setq prev guess)
            (setq guess (/ (+ guess (/ x guess)) 2.0))))
        guess)))))


;; --- cl-lib gaps (hand-rolled fallback) ----------------------------
;; Vendor cl-extra/cl-seq are loaded above; these `unless fboundp'
;; gates only fire if vendor load failed for some reason.

(unless (fboundp 'cl-copy-list)
  (defun cl-copy-list (list)
    "Polyfill: standalone NeLisp's cl-lib does not ship cl-copy-list.
`copy-sequence' on a list returns a fresh top-level cons chain
preserving any dotted-pair tail, which matches the `cl-copy-list'
contract."
    (copy-sequence list)))

(unless (fboundp 'cl-remove-duplicates)
  (defun cl-remove-duplicates (list &rest _ignored-keys)
    "Polyfill: first-occurrence-wins via `equal'.
`:test' / `:key' / `:from-end' keyword args are ignored — the only
anvil callers (= anvil-discovery aggregation) are content with
default semantics."
    (let ((seen nil) (out nil))
      (dolist (x list)
        (unless (member x seen)
          (push x seen)
          (push x out)))
      (nreverse out))))

(unless (fboundp 'cl-subseq)
  (defun cl-subseq (seq start &optional end)
    "Polyfill: subsequence on lists / vectors / strings.
For lists, walks linearly.  For vectors, builds a fresh vector via
`aset'.  For strings, delegates to `substring'."
    (let ((n (length seq)))
      (let ((e (or end n)))
        (cond
         ((listp seq)
          (let ((i 0) (cur seq) (res nil))
            (while (and cur (< i e))
              (when (>= i start)
                (push (car cur) res))
              (setq i (1+ i))
              (setq cur (cdr cur)))
            (nreverse res)))
         ((vectorp seq)
          (let ((len (- e start))
                (i 0))
            (let ((v (make-vector len nil)))
              (while (< i len)
                (aset v i (aref seq (+ start i)))
                (setq i (1+ i)))
              v)))
         ((stringp seq)
          (substring seq start e))
         (t (error "cl-subseq: unsupported sequence type")))))))

(unless (fboundp 'cl-evenp)
  (defun cl-evenp (n) "Polyfill: t when N is an even integer." (zerop (mod n 2))))

(unless (fboundp 'cl-oddp)
  (defun cl-oddp (n) "Polyfill: t when N is an odd integer." (= 1 (mod n 2))))

(unless (fboundp 'cl-mapcar)
  (defun cl-mapcar (function &rest sequences)
    "Polyfill: like `mapcar' but accepts multiple SEQUENCES.
Walks them in lock-step using `nth' lookup; minimal length wins."
    (cond
     ((null sequences) nil)
     ((null (cdr sequences)) (mapcar function (car sequences)))
     (t
      (let* ((lens (mapcar #'length sequences))
             (min-len (apply #'min lens))
             (out nil)
             (i 0))
        (while (< i min-len)
          (let ((args (mapcar (lambda (s) (nth i s)) sequences)))
            (push (apply function args) out))
          (setq i (1+ i)))
        (nreverse out))))))

(unless (fboundp 'cl-reduce)
  (defun cl-reduce (function sequence &rest keys)
    "Polyfill: simplified fold.
Honors `:initial-value' and `:from-end' keyword args; ignores
`:key' / `:start' / `:end' (= anvil callers don't use them)."
    (let ((init-pair (member :initial-value keys))
          (from-end (cadr (member :from-end keys))))
      (let* ((lst (cond
                   ((listp sequence) sequence)
                   ((vectorp sequence)
                    (let ((acc nil) (i (length sequence)))
                      (while (> i 0) (setq i (1- i))
                             (setq acc (cons (aref sequence i) acc)))
                      acc))
                   ((stringp sequence)
                    (let ((acc nil) (i (length sequence)))
                      (while (> i 0) (setq i (1- i))
                             (setq acc (cons (aref sequence i) acc)))
                      acc))
                   (t (error "cl-reduce: unsupported sequence type"))))
             (lst (if from-end (reverse lst) lst))
             (acc (if init-pair (cadr init-pair)
                    (prog1 (car lst) (setq lst (cdr lst))))))
        (dolist (x lst)
          (setq acc (if from-end
                        (funcall function x acc)
                      (funcall function acc x))))
        acc))))


;; --- seq.el gaps ----------------------------------------------------

;; anvil-bench uses `seq-take' to clip the per-call timing list to TOP
;; entries when reporting profile results.  seq.el is not bundled on
;; standalone NeLisp, so polyfill the minimum surface using the cl-*
;; primitives already provided above.
(unless (fboundp 'seq-take)
  (defun seq-take (sequence n)
    "Polyfill: return the first N elements of SEQUENCE.
Delegates to `cl-subseq', clamping N to (length SEQUENCE)."
    (cl-subseq sequence 0 (min n (length sequence)))))


;; --- callproc / process polyfills ---------------------------------

;; anvil-org-index has a defcustom `anvil-org-index-async-emacs-bin'
;; whose default form evaluates `(or (executable-find "emacs") "emacs")'
;; at load time.  Standalone NeLisp ships no `executable-find'; the load
;; aborts on the bare `(void-function executable-find)'.  Stub it as a
;; PATH-aware lookup that falls back to PROGRAM when env / fs primitives
;; are not available.  The async-refresh codepath that actually consumes
;; the value is not exercised under standalone (= no subprocess spawn),
;; so a value good enough for the defcustom load is sufficient.
(unless (fboundp 'executable-find)
  (defun executable-find (program &optional _remote)
    "Polyfill: PATH-aware executable lookup.
Returns the first absolute path P in $PATH whose basename matches
PROGRAM and `file-executable-p'; falls back to PROGRAM itself when
$PATH is empty / `getenv' is stubbed (= NeLisp Phase 1.6) or no
filesystem primitive is available."
    (cond
     ((not (stringp program)) nil)
     ((and (> (length program) 0)
           (eq (aref program 0) ?/))
      (when (and (fboundp 'file-executable-p) (file-executable-p program))
        program))
     (t
      (let* ((path (and (fboundp 'getenv) (getenv "PATH")))
             (sep (if (and (boundp 'system-type) (eq system-type 'windows-nt))
                      ";" ":"))
             (dirs (and (stringp path) (> (length path) 0)
                        (split-string path sep t)))
             (hit nil))
        (when dirs
          (let ((tail dirs))
            (while (and tail (not hit))
              (let* ((d (car tail))
                     (p (concat (if (and (> (length d) 0)
                                         (eq (aref d (1- (length d))) ?/))
                                    d
                                  (concat d "/"))
                                program)))
                (when (and (fboundp 'file-executable-p)
                           (file-executable-p p))
                  (setq hit p)))
              (setq tail (cdr tail)))))
        hit)))))


;; --- sqlite FFI wire-up via emacs-sqlite-ffi + vendor sqlite.el ------

;; Standalone NeLisp ships:
;;   - `emacs-sqlite-ffi.el' (in nelisp-emacs/src/) — since 2026-09-04
;;     (NeLisp v1.2.0 + Doc 138 SQLite arm) builds the Emacs `sqlite-*'
;;     API on the reader's name-dispatch `nl-ffi-call' sqlite3 rows
;;     (winsqlite3.dll on windows-x86_64, libsqlite3.so.0 on the dynamic
;;     Linux reader), including a real `set' cursor with
;;     `sqlite-next' / `sqlite-more-p' / `sqlite-finalize'.  Before
;;     that it spoke to the retired `libnelisp_runtime.so'.
;;   - `vendor/emacs-lisp/sqlite.el' — upstream Emacs sqlite.el, ships
;;     the `with-sqlite-transaction' macro and `(provide 'sqlite)' so
;;     `(require 'sqlite)' from anvil-* downstreams resolves.
;;
;; emacs-init.el unconditionally loads `emacs-sqlite' (= the forwarder
;; layer) which leaves `sqlite-*' bound to thin shims that call
;; `nelisp-sqlite-*' (= unbound on standalone, so available-p returns
;; nil and the forwarders error).  We `fmakunbound' those names so
;; emacs-sqlite-ffi's `unless fboundp' gates evaluate true and the FFI
;; implementations land.

;; Vendor paths.  Directive (2026-05-10 user): substrate polyfills must
;; prefer the upstream Emacs `*.el' shipped under
;; `nelisp-emacs/vendor/emacs-lisp/' over re-implementing or pulling
;; from sibling NeLisp packages.  We prepend the vendor dirs to
;; load-path so `(require 'sqlite)' / `(require 'url)' / `(require
;; 'jsonrpc)' etc resolve to the vendored Emacs sources.
(let* ((nelisp-emacs-root
        ;; Same chain as the vendor bulk-load §39 above.
        (or (and (boundp 'anvil-runtime-bootstrap-nelisp-emacs-dir)
                 anvil-runtime-bootstrap-nelisp-emacs-dir)
            (and (boundp 'anvil-runtime-polyfills-nelisp-emacs-dir)
                 anvil-runtime-polyfills-nelisp-emacs-dir)
            (and (fboundp 'getenv)
                 (let ((env (getenv "NELISP_EMACS_DIR")))
                   (and env (> (length env) 0) env)))
            (and (boundp 'load-file-name) load-file-name
                 (concat (file-name-directory
                          (directory-file-name
                           (file-name-directory
                            (directory-file-name
                             (file-name-directory load-file-name)))))
                         "nelisp-emacs"))
            "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs"))
       (vendor-base (concat nelisp-emacs-root "/vendor/emacs-lisp"))
       (vendor-dirs (list vendor-base
                          (concat vendor-base "/url"))))
  (dolist (dir vendor-dirs)
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

;; Drop the forwarder-layer bindings so emacs-sqlite-ffi's gated
;; defuns can win.  `sqlite-pragma' / `sqlite-transaction' /
;; `sqlite-commit' / `sqlite-rollback' have no FFI counterpart yet —
;; leave them on the forwarder layer (= they'll error if called, which
;; is more honest than a silent miss).
(dolist (sym '(sqlite-available-p sqlite-open sqlite-close
               sqlite-execute sqlite-select sqlitep))
  (when (fboundp sym)
    (fmakunbound sym)))

(condition-case anvil-runtime-polyfills--sqlite-load-err
    (require 'emacs-sqlite-ffi)
  (error
   (when (fboundp 'nelisp--write-stderr-line)
     (nelisp--write-stderr-line
      (concat "[anvil-runtime-polyfills] emacs-sqlite-ffi require failed: "
              (format "%S" anvil-runtime-polyfills--sqlite-load-err)
              " (sqlite-query handler will error gracefully)")))))

;; Optional: vendor `sqlite.el' provides `with-sqlite-transaction' and
;; `(provide 'sqlite)'.  Anvil callers don't currently `require 'sqlite'
;; directly, but loading is cheap and makes future requires resolve.
(condition-case nil (require 'sqlite) (error nil))

;; anvil-sqlite drives the Emacs 30 cursor protocol:
;;
;;   (let* ((stmt (sqlite-select db sql params 'set)))
;;     (while (and stmt (sqlite-more-p stmt) (< count cap))
;;       (let ((row (sqlite-next stmt))) ...))
;;     (when stmt (sqlite-finalize stmt)))
;;
;; The retired emacs-sqlite-ffi.el returned the full row list regardless
;; of RETURN-TYPE, so the wrapper below re-shaped it into a list-backed
;; cursor.  The 2026-09-04 adapter implements the Emacs cursor protocol
;; itself (`sqlite-more-p' is defined), in which case the wrapper and the
;; list-backed helpers must stay out of the way: they only install when
;; the substrate's `sqlite-select' comes without `sqlite-more-p'.

(defvar anvil-runtime-polyfills--sqlite-select-impl
  (and (fboundp 'sqlite-select)
       (not (fboundp 'sqlite-more-p))
       (symbol-function 'sqlite-select))
  "Captured legacy emacs-sqlite-ffi `sqlite-select' implementation.
Nil when the substrate already speaks the Emacs cursor protocol.  Used
by the cursor-aware override below so the wrapper delegates to the real
FFI without infinite recursion.")

(when anvil-runtime-polyfills--sqlite-select-impl
  (defun sqlite-select (db query &optional values return-type)
    "Cursor-aware override delegating to the captured FFI `sqlite-select'.
When RETURN-TYPE is `'set' or `'full', wrap the row list as a
`(:anvil-sqlite-cursor . REMAINING-ROWS)' cons.  Otherwise return
rows inline (= matches the Emacs 30 builtin's default shape)."
    (let ((rows (funcall anvil-runtime-polyfills--sqlite-select-impl
                         db query values return-type)))
      (cond
       ((memq return-type '(set full))
        (cons :anvil-sqlite-cursor (or rows nil)))
       (t rows)))))

(unless (fboundp 'sqlite-more-p)
  (defun sqlite-more-p (cursor)
    "Return non-nil while CURSOR has un-consumed rows."
    (and (consp cursor)
         (eq (car cursor) :anvil-sqlite-cursor)
         (cdr cursor))))

(unless (fboundp 'sqlite-next)
  (defun sqlite-next (cursor)
    "Pop and return the next row from CURSOR, advancing internal state."
    (when (and (consp cursor)
               (eq (car cursor) :anvil-sqlite-cursor)
               (cdr cursor))
      (let ((row (cadr cursor)))
        (setcdr cursor (cddr cursor))
        row))))

(unless (fboundp 'sqlite-finalize)
  (defun sqlite-finalize (cursor)
    "Release CURSOR.  For the list-backed polyfill this just clears
the remaining-rows tail."
    (when (and (consp cursor)
               (eq (car cursor) :anvil-sqlite-cursor))
      (setcdr cursor nil))))


;; --- benchmark / profiler stubs ------------------------------------

;; anvil-bench.el `(require 'benchmark) (require 'profiler)' fails
;; because the modules don't ship in standalone NeLisp.  We `provide'
;; the features and stub the surface anvil-bench actually calls.
;; benchmark-* are real timing wrappers, profiler-* are no-ops returning
;; empty results — sampling profiler is a NeLisp-side feature gap.

(unless (featurep 'benchmark)
  (defmacro benchmark-elapse (&rest body)
    "Polyfill: time BODY using `current-time'."
    `(let ((anvil-runtime-polyfills--bench-start (current-time)))
       ,@body
       (float-time (time-subtract (current-time)
                                  anvil-runtime-polyfills--bench-start))))
  (defun benchmark-call (function &optional repetitions)
    "Polyfill: call FUNCTION REPETITIONS times, return (elapsed gc-elapsed gc-count).
Elapsed-seconds is real, the GC fields are zero stubs since standalone
NeLisp doesn't expose `gcs-done' / `gc-elapsed' separately."
    (let* ((reps (or repetitions 1))
           (start (current-time))
           (i 0))
      (while (< i reps)
        (funcall function)
        (setq i (1+ i)))
      (list (float-time (time-subtract (current-time) start)) 0 0)))
  (defmacro benchmark-progn (&rest body)
    "Polyfill: time BODY and message the elapsed seconds."
    `(let ((anvil-runtime-polyfills--bp-start (current-time)))
       (prog1 (progn ,@body)
         (message "Elapsed: %fs"
                  (float-time
                   (time-subtract (current-time)
                                  anvil-runtime-polyfills--bp-start))))))
  (provide 'benchmark))

(unless (featurep 'profiler)
  ;; Sampling profiler is a NeLisp-side feature gap; stub returns an
  ;; empty cpu-log so anvil-bench's profile-driven tools degrade
  ;; gracefully (= empty :top, no crash).
  (defun profiler-start (&optional _mode) nil)
  (defun profiler-stop () nil)
  (defun profiler-reset () nil)
  (defun profiler-cpu-log () (make-hash-table :test 'equal))
  (provide 'profiler))

;; --- anvil-server cl-loop workaround --------------------------------

;; `anvil-server-register-tools' / `-unregister-tools' use
;;   (cl-loop for (k v) on (cdr spec) by #'cddr unless (eq k :server-id)
;;            append (list k v))
;; to filter the plist.  Standalone NeLisp's `cl-loop' destructures
;; `(k v)' incorrectly under `by #'cddr' (= every key/value pair drops
;; through the filter), so `(plist-get final :id)' is nil and the
;; inner singular `register-tool' aborts with "requires :id".  Replace
;; both functions with hand-rolled plist walks.  Done at polyfill load
;; time (anvil-server is loaded before polyfills in the driver) so the
;; override is in place before any tool module's `(enable)' iterates.

(when (featurep 'anvil-server)
  (defun anvil-server-register-tools (server-id specs)
    "Polyfill override: hand-rolled plist walk replaces cl-loop."
    (let (ids)
      (dolist (spec specs)
        (unless (and (consp spec) (functionp (car spec))
                     (zerop (mod (length (cdr spec)) 2)))
          (error "anvil-server-register-tools: malformed spec %S" spec))
        (let* ((handler (car spec))
               (raw     (cdr spec))
               (props   nil))
          (let ((tail raw))
            (while tail
              (let ((k (car tail))
                    (v (cadr tail)))
                (unless (eq k :server-id)
                  (setq props (append props (list k v))))
                (setq tail (cddr tail)))))
          (let* ((final (append (list :server-id server-id) props))
                 (id    (plist-get final :id)))
            (apply #'anvil-server-register-tool handler final)
            (push id ids))))
      (nreverse ids)))

  (defun anvil-server-unregister-tools (server-id specs)
    "Polyfill override: hand-rolled plist walk replaces cl-loop."
    (let (results)
      (dolist (spec specs)
        (when (and (consp spec) (functionp (car spec))
                   (zerop (mod (length (cdr spec)) 2)))
          (let* ((raw (cdr spec))
                 (id  nil))
            (let ((tail raw))
              (while (and tail (null id))
                (when (eq (car tail) :id)
                  (setq id (cadr tail)))
                (setq tail (cddr tail))))
            (when id
              (push (anvil-server-unregister-tool id server-id)
                    results)))))
      (nreverse results)))

  ;; `anvil-server--to-json-value' converts plists to alists for JSON
  ;; encoding via:
  ;;   (cl-loop for (k v) on x by #'cddr
  ;;            collect (cons (intern ...) (--to-json-value v)))
  ;; The same NeLisp `cl-loop` destructuring bug drops every key/value
  ;; pair, so the alist is nil → `json-encode' emits "null".  This
  ;; manifests in tools/call as `text:"null"' for every plist-returning
  ;; handler (= memory-list / memory-search / worklog-list / etc).
  ;; Replace the plist branch with a hand-rolled walk; also replace
  ;; `proper-list-p' detection with a hand-rolled cdr walk because
  ;; NeLisp's `proper-list-p' has been observed misclassifying proper
  ;; lists containing plists as improper (= ((:file ...)) was hitting
  ;; the dotted-pair branch and emitting `[obj, null]' tail) — the
  ;; resulting `[..., null]' surfaced as a phantom trailing entry on
  ;; every list-of-plists response.
  (when (and (featurep 'anvil-server)
             (fboundp 'anvil-server--plist-p)
             (fboundp 'anvil-server--list-to-json-array))
    (defun anvil-server--to-json-value (x)
      "Polyfill override: plist branch + proper/improper detection are
hand-rolled to side-step NeLisp's `cl-loop' destructuring + brittle
`proper-list-p' returning nil for proper plist-bearing lists."
      (cond
       ((or (null x) (eq x t) (numberp x)) x)
       ((stringp x) x)
       ((keywordp x) (substring (symbol-name x) 1))
       ((symbolp x) (symbol-name x))
       ((vectorp x)
        (vconcat (mapcar #'anvil-server--to-json-value x)))
       ((anvil-server--plist-p x)
        (let ((tail x)
              (alist nil))
          (while tail
            (let* ((k (car tail))
                   (v (cadr tail))
                   (kn (substring (symbol-name k) 1)))
              (push (cons (intern kn)
                          (anvil-server--to-json-value v))
                    alist))
            (setq tail (cddr tail)))
          (nreverse alist)))
       ((consp x)
        ;; Walk the cdr-chain manually — when we exhaust to nil it's a
        ;; proper list, otherwise it's a dotted pair.  Avoids NeLisp's
        ;; misbehaving `proper-list-p'.
        (let ((tail x))
          (while (consp tail) (setq tail (cdr tail)))
          (if (null tail)
              (anvil-server--list-to-json-array x)
            (vector (anvil-server--to-json-value (car x))
                    (anvil-server--to-json-value (cdr x))))))
       (t x)))))


;; --- post-load patches (anvil-* module compat) ---------------------

;; Fixes that depend on anvil-* having been loaded; called by the driver
;; after the ANVIL_TOOL_MODULES load+enable chain.

(defun anvil-runtime-polyfills-apply-post-load-patches ()
  "Apply fixups to anvil-* modules that need substrate-aware overrides.
Idempotent: each branch checks featurep / fboundp before redefining."

  ;; anvil-sqlite uses `(string-match-p \"\\\\`\\\\(?:SELECT\\\\|WITH\\\\|...\\\\)\\\\b\" ...)`
  ;; for its readonly guard.  Standalone NeLisp's regex engine does not
  ;; accept `\\(?:` non-capturing groups + `\\b` word-boundary in the same
  ;; pattern (= returns nil unconditionally), so every legitimate read
  ;; statement gets rejected.  Replace with a regex-free prefix check.
  (when (featurep 'anvil-sqlite)
    (defun anvil-sqlite--readonly-statement-p (sql)
      "Polyfill override: regex-free read-only-statement detector.
Returns non-nil if SQL begins (after upcase + trim) with one of
SELECT / WITH / PRAGMA / EXPLAIN.  Standalone NeLisp's regex
substrate cannot evaluate the original `(?:...)|\\b' pattern."
      (let* ((trimmed (string-trim (or sql "")))
             (up (upcase trimmed)))
        (and (not (string-empty-p up))
             (or (string-prefix-p "SELECT"  up)
                 (string-prefix-p "WITH"    up)
                 (string-prefix-p "PRAGMA"  up)
                 (string-prefix-p "EXPLAIN" up)))))))


(provide 'anvil-runtime-polyfills)
;;; anvil-runtime-polyfills.el ends here
