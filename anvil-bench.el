;;; anvil-bench.el --- Performance measurement primitives  -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 14 Phase 1 — thin AI-friendly wrappers around `benchmark-call'
;; and `profiler-*' that give Claude (and human operators) a shared
;; measurement surface for perf hypotheses.
;;
;; Public Elisp API:
;;   (anvil-bench-compare THUNK-A THUNK-B &key n warmup label-a label-b)
;;     -> plist (:a-mean-ms :a-stddev-ms :b-mean-ms :b-stddev-ms
;;               :ratio :winner :n :warmup :env ...)
;;
;;   (anvil-bench-profile THUNK &key duration-sec top)
;;     -> plist (:top ((:function :samples :pct) ...)
;;               :total-samples :iterations :duration-sec :env ...)
;;
;;   (anvil-bench-last-report)
;;     -> last plist stored by either of the above, or nil.
;;
;; MCP tools (emacs-eval server):
;;   bench-compare        — a-expr / b-expr strings, optional n / warmup
;;   bench-profile-expr   — expr string, optional duration-sec / top
;;   bench-last           — zero-arg dry-poll of the last report
;;
;; Design doc: docs/design/14-bench-profile.org

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'benchmark)
(require 'profiler)
(require 'anvil-server)


;;;; --- group + defcustoms ---------------------------------------------------

(defgroup anvil-bench nil
  "Performance measurement primitives for anvil."
  :group 'anvil
  :prefix "anvil-bench-")

(defconst anvil-bench--server-id "emacs-eval"
  "Server id under which bench-* MCP tools register.")

(defcustom anvil-bench-default-n 1000
  "Default measured iterations for `anvil-bench-compare'.
Each thunk is called this many times in each A/B slot after warmup."
  :type 'integer
  :group 'anvil-bench)

(defcustom anvil-bench-default-warmup 100
  "Default warmup iterations before measurement.
Warmup calls are not timed and exist so JIT / byte-compilation /
cache effects do not pollute the first measured run."
  :type 'integer
  :group 'anvil-bench)

(defcustom anvil-bench-disable-gc t
  "When non-nil, disable garbage collection during measured runs.
Achieved by let-binding `gc-cons-threshold' to `most-positive-fixnum'
and `gc-cons-percentage' to 1.0 for the duration of the measurement
(warmup runs included)."
  :type 'boolean
  :group 'anvil-bench)

(defcustom anvil-bench-tie-threshold 0.05
  "Maximum |1 - ratio| at which `anvil-bench-compare' declares a tie.
Default 0.05 means a <5 % difference is noise."
  :type 'number
  :group 'anvil-bench)

(defcustom anvil-bench-profile-default-duration-sec 3
  "Default wall-clock seconds for `anvil-bench-profile'.
The thunk is called in a tight loop until the deadline elapses."
  :type 'number
  :group 'anvil-bench)

(defcustom anvil-bench-profile-default-top 20
  "Default number of top hot functions returned by `anvil-bench-profile'."
  :type 'integer
  :group 'anvil-bench)


;;;; --- state ---------------------------------------------------------------

(defvar anvil-bench--last-report nil
  "Last plist returned by `anvil-bench-compare' or `anvil-bench-profile'.
Accessible via `anvil-bench-last-report' or the `bench-last' MCP tool.")


;;;; --- env + stats helpers -------------------------------------------------

(defun anvil-bench--byte-compiled-p (thunk)
  "Return t when THUNK appears to be a byte-compiled function."
  (or (byte-code-function-p thunk)
      (and (fboundp 'native-comp-function-p)
           (native-comp-function-p thunk))))

(defun anvil-bench--env (&optional extra)
  "Return the shared :env plist for a report.
EXTRA is merged as a plist suffix when provided."
  (append
   (list :emacs-version emacs-version
         :system-type (symbol-name system-type)
         :gc-cons-threshold gc-cons-threshold
         :disable-gc anvil-bench-disable-gc)
   extra))

(defun anvil-bench--mean (xs)
  "Arithmetic mean of XS (list of numbers).  Returns 0.0 on empty."
  (if (null xs) 0.0
    (/ (float (apply #'+ xs)) (length xs))))

(defun anvil-bench--stddev (xs)
  "Population standard deviation of XS.  Returns 0.0 on empty or single."
  (if (or (null xs) (null (cdr xs))) 0.0
    (let* ((m (anvil-bench--mean xs))
           (sq (mapcar (lambda (x) (expt (- x m) 2)) xs)))
      (sqrt (/ (apply #'+ sq) (float (length xs)))))))

(defun anvil-bench--time-thunk-n (thunk n)
  "Invoke THUNK N times; return list of elapsed seconds per call."
  (let (samples)
    (dotimes (_ n)
      (let ((t0 (float-time)))
        (funcall thunk)
        (push (- (float-time) t0) samples)))
    (nreverse samples)))

(defun anvil-bench--with-gc-disabled (fn)
  "Invoke FN with `gc-cons-threshold' pegged high and GC %-threshold 1.0.
Restores both after FN returns.  When `anvil-bench-disable-gc' is
nil, FN is invoked directly."
  (if anvil-bench-disable-gc
      (let ((gc-cons-threshold most-positive-fixnum)
            (gc-cons-percentage 1.0))
        (funcall fn))
    (funcall fn)))


;;;; --- bench-compare -------------------------------------------------------

;;;###autoload
(cl-defun anvil-bench-compare (thunk-a thunk-b
                                       &key
                                       (n anvil-bench-default-n)
                                       (warmup anvil-bench-default-warmup)
                                       (label-a "a")
                                       (label-b "b"))
  "Measure THUNK-A vs THUNK-B and return a comparison report.

Both arguments must be zero-argument callables (functions or
closures).  Each is invoked WARMUP times (untimed), then N times
with per-call wallclock measured via `float-time'.

Keyword arguments:
  :n       Iterations (default `anvil-bench-default-n', 1000).
  :warmup  Untimed warmup iterations (default 100).
  :label-a / :label-b  Display labels carried in the report.

Returns a plist:
  (:a-mean-ms :a-stddev-ms :a-total-ms
   :b-mean-ms :b-stddev-ms :b-total-ms
   :ratio      ;; a-mean / b-mean — >1 means b is faster
   :winner     ;; `a' / `b' / `tie'
   :speedup    ;; human-friendly \"a is 2.1x faster\" or similar
   :n :warmup :label-a :label-b
   :env (:emacs-version ... :a-byte-compiled BOOL :b-byte-compiled BOOL))

The report is also stored in `anvil-bench--last-report'."
  (unless (and (functionp thunk-a) (functionp thunk-b))
    (user-error "anvil-bench-compare: thunk-a and thunk-b must be functions"))
  (let* ((a-times
          (anvil-bench--with-gc-disabled
           (lambda ()
             (dotimes (_ warmup) (funcall thunk-a))
             (anvil-bench--time-thunk-n thunk-a n))))
         (b-times
          (anvil-bench--with-gc-disabled
           (lambda ()
             (dotimes (_ warmup) (funcall thunk-b))
             (anvil-bench--time-thunk-n thunk-b n))))
         (a-mean (anvil-bench--mean a-times))
         (b-mean (anvil-bench--mean b-times))
         (a-stddev (anvil-bench--stddev a-times))
         (b-stddev (anvil-bench--stddev b-times))
         (ratio (if (> b-mean 0) (/ a-mean b-mean) 1.0))
         (winner
          (cond ((< (abs (- ratio 1.0)) anvil-bench-tie-threshold) 'tie)
                ((> ratio 1.0) 'b)
                (t 'a)))
         (speedup
          (cond ((eq winner 'tie)
                 (format "%s and %s are within %.1f%% (tie)"
                         label-a label-b
                         (* 100 anvil-bench-tie-threshold)))
                ((eq winner 'b)
                 (format "%s is %.2fx faster than %s" label-b ratio label-a))
                (t
                 (format "%s is %.2fx faster than %s"
                         label-a (if (> ratio 0) (/ 1.0 ratio) 0) label-b))))
         (report
          (list :a-mean-ms   (* 1000.0 a-mean)
                :a-stddev-ms (* 1000.0 a-stddev)
                :a-total-ms  (* 1000.0 (apply #'+ a-times))
                :b-mean-ms   (* 1000.0 b-mean)
                :b-stddev-ms (* 1000.0 b-stddev)
                :b-total-ms  (* 1000.0 (apply #'+ b-times))
                :ratio ratio
                :winner winner
                :speedup speedup
                :n n
                :warmup warmup
                :label-a label-a
                :label-b label-b
                :env (anvil-bench--env
                      (list :a-byte-compiled
                            (anvil-bench--byte-compiled-p thunk-a)
                            :b-byte-compiled
                            (anvil-bench--byte-compiled-p thunk-b))))))
    (setq anvil-bench--last-report report)
    report))


;;;; --- bench-profile -------------------------------------------------------

(defun anvil-bench--extract-leaf (backtrace)
  "Return the leaf (innermost) frame from BACKTRACE.
Emacs' `profiler-cpu-log' keys are vectors whose frames are laid
out from the innermost outward; we grab index 0 when present."
  (when (and (vectorp backtrace) (> (length backtrace) 0))
    (aref backtrace 0)))

(defun anvil-bench--aggregate-profile-log (log)
  "Reduce LOG (hash-table: backtrace -> count) to an alist.
Each result entry has shape (FUNCTION-SEXP . SAMPLE-COUNT).
LOG is nil-safe."
  (let ((counts (make-hash-table :test 'equal)))
    (when (hash-table-p log)
      (maphash
       (lambda (backtrace count)
         (let ((leaf (anvil-bench--extract-leaf backtrace)))
           (when leaf
             (puthash leaf
                      (+ count (gethash leaf counts 0))
                      counts))))
       log))
    (let (acc)
      (maphash (lambda (fn n) (push (cons fn n) acc)) counts)
      acc)))

(defun anvil-bench--format-profile-entries (entries top total)
  "Format the top TOP ENTRIES as a list of plists.
TOTAL is the grand sum for percentage calculation."
  (let ((sorted (sort (copy-sequence entries)
                      (lambda (a b) (> (cdr a) (cdr b))))))
    (mapcar
     (lambda (pair)
       (list :function (format "%S" (car pair))
             :samples (cdr pair)
             :pct (if (> total 0)
                      (/ (* 100.0 (cdr pair)) total)
                    0.0)))
     (seq-take sorted top))))

;;;###autoload
(cl-defun anvil-bench-profile (thunk
                               &key
                               (duration-sec anvil-bench-profile-default-duration-sec)
                               (top anvil-bench-profile-default-top))
  "Sample-profile THUNK for DURATION-SEC seconds; return top hot functions.

THUNK is called repeatedly inside an outer loop bounded by the
deadline (`float-time' + DURATION-SEC).  CPU profiling is enabled
via `profiler-start' \\='cpu during the loop and stopped afterwards.

Keyword arguments:
  :duration-sec  Wall-clock seconds to sample (default 3).
  :top           Number of hot functions to report (default 20).

Returns a plist:
  (:top ((:function STR :samples INT :pct FLOAT) ...)
   :total-samples INT
   :iterations INT       ;; how many times THUNK was invoked
   :duration-sec FLOAT
   :env (:emacs-version ... :thunk-byte-compiled BOOL))

The report is also stored in `anvil-bench--last-report'.

On platforms where the sampling profiler is not available
(e.g. emacs built without profiler support) :top is an empty
list and :total-samples is 0; :iterations still reflects the
true thunk invocation count."
  (unless (functionp thunk)
    (user-error "anvil-bench-profile: thunk must be a function"))
  (let* ((iter 0)
         (deadline (+ (float-time) duration-sec))
         (started (ignore-errors (profiler-start 'cpu) t))
         log)
    (unwind-protect
        (while (< (float-time) deadline)
          (funcall thunk)
          (cl-incf iter))
      (when started
        (ignore-errors (profiler-stop))
        (setq log (ignore-errors (profiler-cpu-log)))
        (ignore-errors (profiler-reset))))
    (let* ((entries (anvil-bench--aggregate-profile-log log))
           (total (apply #'+ (mapcar #'cdr entries)))
           (report
            (list :top (anvil-bench--format-profile-entries entries top total)
                  :total-samples total
                  :iterations iter
                  :duration-sec (float duration-sec)
                  :env (anvil-bench--env
                        (list :thunk-byte-compiled
                              (anvil-bench--byte-compiled-p thunk))))))
      (setq anvil-bench--last-report report)
      report)))


;;;; --- last-report ---------------------------------------------------------

;;;###autoload
(defun anvil-bench-last-report ()
  "Return the most recent bench / profile report, or nil."
  anvil-bench--last-report)


;;;; --- MCP handlers --------------------------------------------------------

(defun anvil-bench--parse-pos-int (s default)
  "Parse S (string) as a positive integer; fall back to DEFAULT."
  (if (and (stringp s) (not (string-empty-p s)))
      (let ((n (string-to-number s)))
        (if (and (integerp n) (> n 0)) n default))
    default))

(defun anvil-bench--parse-pos-number (s default)
  "Parse S (string) as a positive number; fall back to DEFAULT."
  (if (and (stringp s) (not (string-empty-p s)))
      (let ((n (string-to-number s)))
        (if (> n 0) n default))
    default))

(defun anvil-bench--read-expr (s label)
  "Read S as a single elisp expression; signal `user-error' on failure."
  (unless (and (stringp s) (not (string-empty-p s)))
    (user-error "anvil-bench: %s: empty expression" label))
  (condition-case err
      (let ((res (read-from-string s)))
        (car res))
    (error
     (user-error "anvil-bench: %s: failed to read: %s"
                 label (error-message-string err)))))

(defun anvil-bench--thunk-from-expr (expr)
  "Wrap EXPR in a zero-argument lambda and eval to produce a thunk."
  (eval `(lambda () ,expr) t))

(defun anvil-bench--tool-compare (a_expr b_expr
                                  &optional n warmup label_a label_b)
  "MCP wrapper for `anvil-bench-compare'.

MCP Parameters:
  a_expr  - Elisp source of the A thunk body (string).  Wrapped in
            a 0-arg lambda before measurement.
  b_expr  - Source of the B thunk body (string).
  n       - Optional measured-iteration count (string integer).
            Default `anvil-bench-default-n'.
  warmup  - Optional untimed warmup count (string integer).
            Default `anvil-bench-default-warmup'.
  label_a - Optional display label for A; defaults to \"a\".
  label_b - Optional display label for B; defaults to \"b\"."
  (anvil-server-with-error-handling
    (let* ((a-form (anvil-bench--read-expr a_expr "a_expr"))
           (b-form (anvil-bench--read-expr b_expr "b_expr"))
           (n-i    (anvil-bench--parse-pos-int n anvil-bench-default-n))
           (w-i    (anvil-bench--parse-pos-int warmup anvil-bench-default-warmup))
           (la     (if (and (stringp label_a) (not (string-empty-p label_a)))
                       label_a "a"))
           (lb     (if (and (stringp label_b) (not (string-empty-p label_b)))
                       label_b "b")))
      (anvil-bench-compare
       (anvil-bench--thunk-from-expr a-form)
       (anvil-bench--thunk-from-expr b-form)
       :n n-i :warmup w-i :label-a la :label-b lb))))

(defun anvil-bench--tool-profile-expr (expr &optional duration_sec top)
  "MCP wrapper for `anvil-bench-profile'.

MCP Parameters:
  expr         - Elisp source of the thunk body (string).
  duration_sec - Optional sampling window (string number, default 3).
  top          - Optional top-K function count (string integer, default 20)."
  (anvil-server-with-error-handling
    (let* ((form (anvil-bench--read-expr expr "expr"))
           (d (anvil-bench--parse-pos-number
               duration_sec anvil-bench-profile-default-duration-sec))
           (k (anvil-bench--parse-pos-int
               top anvil-bench-profile-default-top)))
      (anvil-bench-profile
       (anvil-bench--thunk-from-expr form)
       :duration-sec d :top k))))

(defun anvil-bench--tool-last ()
  "MCP wrapper for `anvil-bench-last-report'.

Returns the last bench / profile report plist or nil when none
has been recorded in this session."
  (anvil-server-with-error-handling
    (or (anvil-bench-last-report) nil)))


;;;; --- lifecycle -----------------------------------------------------------

(defun anvil-bench--register-tools ()
  "Register bench-* MCP tools under `anvil-bench--server-id'."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-bench--tool-compare)
   :id "bench-compare"
   :intent '(bench)
   :layer 'dev
   :server-id anvil-bench--server-id
   :description
   "Measure two elisp expressions head-to-head.  Each expression is
wrapped in a 0-arg lambda, warmed up, then run N times with wall-
clock per call.  Returns mean / stddev (ms) for each side, the A/B
ratio, a `winner' field (`a' / `b' / `tie'), and env info for
reproducibility.  GC is disabled during measurement.  Use this
before claiming that refactor X is faster than refactor Y — \"it
feels faster\" is not enough."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-bench--tool-profile-expr)
   :id "bench-profile-expr"
   :intent '(bench)
   :layer 'dev
   :server-id anvil-bench--server-id
   :description
   "Sample-profile an elisp expression for DURATION_SEC seconds.
Expression is wrapped in a 0-arg lambda and called in a tight loop
while Emacs' built-in CPU profiler samples.  Returns the top TOP
functions by sample count, each with :function / :samples / :pct,
plus :total-samples / :iterations / :duration-sec / :env.  Long
durations (>10s) should be dispatched via offload."
   :read-only t)

  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-bench--tool-last)
   :id "bench-last"
   :intent '(bench)
   :layer 'dev
   :server-id anvil-bench--server-id
   :description
   "Return the last bench-compare or bench-profile-expr report
recorded in this Emacs session, or nil when none has run.  Handy
for a dashboard to poll the most recent measurement without
re-running it."
   :read-only t))

(defun anvil-bench--unregister-tools ()
  (dolist (id '("bench-compare" "bench-profile-expr" "bench-last"))
    (ignore-errors
      (anvil-server-unregister-tool id anvil-bench--server-id))))

;;;###autoload
(defun anvil-bench-enable ()
  "Register the Doc 14 bench + profile MCP tools."
  (interactive)
  (anvil-bench--register-tools))

;;;###autoload
(defun anvil-bench-disable ()
  "Unregister the Doc 14 bench + profile MCP tools."
  (interactive)
  (anvil-bench--unregister-tools))

(provide 'anvil-bench)

;;; anvil-bench.el ends here
