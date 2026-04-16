;;; anvil-worker-test.el --- Phase 1 lane-split tests for anvil-worker -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercises the data-model and dispatch logic introduced by
;; Doc 01 Phase 1 (lane split + :kind keyword).  No real worker
;; daemons are spawned: `anvil-worker--worker-alive-p' is stubbed
;; so dispatch can be verified without `emacs --fg-daemon'
;; processes (which would be expensive on Windows and rely on
;; emacsclient being on PATH).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-worker)

(defmacro anvil-worker-test--with-pool (sizes alive-set &rest body)
  "Run BODY with a fresh pool of SIZES and stubbed liveness.

SIZES is a plist `(:read N :write N :batch N)'.  ALIVE-SET is a
list of worker NAMES to treat as alive; everything else is dead.
Disable health timer + spawning so no real subprocesses start."
  (declare (indent 2))
  `(let* ((anvil-worker-read-pool-size  (or (plist-get ,sizes :read) 0))
          (anvil-worker-write-pool-size (or (plist-get ,sizes :write) 0))
          (anvil-worker-batch-pool-size (or (plist-get ,sizes :batch) 0))
          (anvil-worker--pool nil)
          (anvil-worker--dispatch-index (list :read 0 :write 0 :batch 0))
          (anvil-worker--alive-set ,alive-set))
     (cl-letf (((symbol-function 'anvil-worker--worker-alive-p)
                (lambda (worker)
                  (and worker
                       (member (plist-get worker :name)
                               anvil-worker--alive-set))))
               ((symbol-function 'anvil-worker--spawn-worker)
                (lambda (_worker) nil))
               ((symbol-function 'anvil-worker--log)
                (lambda (&rest _) nil)))
       (anvil-worker--init-pool)
       ,@body)))

;;;; --- pool init ---------------------------------------------------------

(ert-deftest anvil-worker-test-init-pool-shape ()
  "Pool plist holds a vector per lane, sized from the defcustoms."
  (anvil-worker-test--with-pool '(:read 2 :write 1 :batch 0) nil
    (should (= 2 (length (anvil-worker--lane-pool :read))))
    (should (= 1 (length (anvil-worker--lane-pool :write))))
    (should (= 0 (length (anvil-worker--lane-pool :batch))))))

(ert-deftest anvil-worker-test-name-includes-lane ()
  "Worker names embed the lane and a 1-based index."
  (anvil-worker-test--with-pool '(:read 2 :write 1) nil
    (should (equal "anvil-worker-read-1"
                   (plist-get (anvil-worker--worker :read 0) :name)))
    (should (equal "anvil-worker-read-2"
                   (plist-get (anvil-worker--worker :read 1) :name)))
    (should (equal "anvil-worker-write-1"
                   (plist-get (anvil-worker--worker :write 0) :name)))))

(ert-deftest anvil-worker-test-server-file-includes-lane ()
  "Server-file path mirrors the lane-aware worker name."
  (anvil-worker-test--with-pool '(:read 1 :write 1) nil
    (should (string-match-p "anvil-worker-read-1\\'"
                            (plist-get (anvil-worker--worker :read 0)
                                       :server-file)))
    (should (string-match-p "anvil-worker-write-1\\'"
                            (plist-get (anvil-worker--worker :write 0)
                                       :server-file)))))

;;;; --- pick-worker -------------------------------------------------------

(ert-deftest anvil-worker-test-pick-read-only-honours-lane ()
  ":kind :read picks from :read even when :write is also alive."
  (anvil-worker-test--with-pool
      '(:read 1 :write 1)
      '("anvil-worker-read-1" "anvil-worker-write-1")
    (let ((w (anvil-worker--pick-worker :read)))
      (should (eq :read (plist-get w :lane)))
      (should (equal "anvil-worker-read-1" (plist-get w :name))))))

(ert-deftest anvil-worker-test-pick-write-only-honours-lane ()
  ":kind :write must not return a :read worker even if :read alive."
  (anvil-worker-test--with-pool
      '(:read 1 :write 1)
      '("anvil-worker-read-1" "anvil-worker-write-1")
    (let ((w (anvil-worker--pick-worker :write)))
      (should (eq :write (plist-get w :lane))))))

(ert-deftest anvil-worker-test-pick-auto-without-expression-uses-fallback ()
  ":kind :auto with no expression hits the classifier fallback (default :write)."
  (anvil-worker-test--with-pool
      '(:read 1 :write 1)
      '("anvil-worker-read-1" "anvil-worker-write-1")
    (let ((w (anvil-worker--pick-worker :auto)))
      (should (eq :write (plist-get w :lane))))))

(ert-deftest anvil-worker-test-pick-auto-falls-through ()
  "When :read has no alive workers :auto descends to :write."
  (anvil-worker-test--with-pool
      '(:read 1 :write 1)
      '("anvil-worker-write-1")
    (let ((w (anvil-worker--pick-worker :auto)))
      (should (eq :write (plist-get w :lane))))))

(ert-deftest anvil-worker-test-pick-round-robins-within-lane ()
  "Successive picks within a lane advance the round-robin cursor."
  (anvil-worker-test--with-pool
      '(:read 2)
      '("anvil-worker-read-1" "anvil-worker-read-2")
    (let ((first  (anvil-worker--pick-worker :read))
          (second (anvil-worker--pick-worker :read)))
      (should-not (equal (plist-get first  :name)
                         (plist-get second :name))))))

(ert-deftest anvil-worker-test-pick-skips-busy ()
  "A worker marked busy is skipped over while a non-busy peer exists."
  (anvil-worker-test--with-pool
      '(:read 2)
      '("anvil-worker-read-1" "anvil-worker-read-2")
    (plist-put (anvil-worker--worker :read 0) :busy t)
    (let ((w (anvil-worker--pick-worker :read)))
      (should (equal "anvil-worker-read-2" (plist-get w :name))))))

(ert-deftest anvil-worker-test-pick-busy-fallback ()
  "If every alive worker is busy, dispatch returns one anyway (fallback)."
  (anvil-worker-test--with-pool
      '(:read 2)
      '("anvil-worker-read-1" "anvil-worker-read-2")
    (plist-put (anvil-worker--worker :read 0) :busy t)
    (plist-put (anvil-worker--worker :read 1) :busy t)
    (let ((w (anvil-worker--pick-worker :read)))
      (should w)
      (should (eq :read (plist-get w :lane))))))

;;;; --- arg parsing -------------------------------------------------------

(ert-deftest anvil-worker-test-parse-call-args-empty ()
  "No args ⇒ :auto with default timeout."
  (let ((p (anvil-worker--parse-call-args nil)))
    (should (eq :auto (plist-get p :kind)))
    (should (null (plist-get p :timeout)))))

(ert-deftest anvil-worker-test-parse-call-args-positional-timeout ()
  "Legacy `(EXPR TIMEOUT)' form is accepted as :auto + that timeout."
  (let ((p (anvil-worker--parse-call-args '(120))))
    (should (eq :auto (plist-get p :kind)))
    (should (= 120 (plist-get p :timeout)))))

(ert-deftest anvil-worker-test-parse-call-args-positional-nil-timeout ()
  "A single nil argument is treated as the legacy form, not a keyword."
  (let ((p (anvil-worker--parse-call-args '(nil))))
    (should (eq :auto (plist-get p :kind)))
    (should (null  (plist-get p :timeout)))))

(ert-deftest anvil-worker-test-parse-call-args-keyword-form ()
  "Keyword form binds :kind and :timeout."
  (let ((p (anvil-worker--parse-call-args '(:kind :write :timeout 30))))
    (should (eq :write (plist-get p :kind)))
    (should (= 30     (plist-get p :timeout)))))

(ert-deftest anvil-worker-test-parse-call-args-rejects-unknown ()
  "Unknown keyword raises so typos surface immediately."
  (should-error (anvil-worker--parse-call-args '(:kind :read :nope 1))))

;;;; --- back-compat alias -------------------------------------------------

(ert-deftest anvil-worker-test-pool-size-alias ()
  "Old `anvil-worker-pool-size' setq propagates to read-pool-size."
  (let ((anvil-worker-read-pool-size 99))
    (should (= 99 anvil-worker-pool-size))
    (setq anvil-worker-pool-size 7)
    (should (= 7 anvil-worker-read-pool-size))))

;;;; --- classifier (Doc 01 Phase 2) -----------------------------------------

(defmacro anvil-worker-test--with-fresh-classifier-metrics (&rest body)
  "Reset classifier metrics around BODY so each test starts at zero."
  (declare (indent 0))
  `(let ((anvil-worker--metrics-classify
          (list :read 0 :write 0 :batch 0 :unknown-fallback 0)))
     ,@body))

(ert-deftest anvil-worker-test-classify-read-patterns ()
  "Read-only org/file/buffer/index calls land on :read."
  (anvil-worker-test--with-fresh-classifier-metrics
    (should (eq :read (anvil-worker--classify "(org-read-file \"x.org\")")))
    (should (eq :read (anvil-worker--classify "(file-read \"x.el\")")))
    (should (eq :read (anvil-worker--classify "(buffer-read \"buf\")")))
    (should (eq :read (anvil-worker--classify "(org-index-search :tag \"x\")")))
    (should (eq :read (anvil-worker--classify "(sqlite-query \"SELECT 1\")")))))

(ert-deftest anvil-worker-test-classify-write-patterns ()
  "Mutating file-* / buffer-save / write-region land on :write."
  (anvil-worker-test--with-fresh-classifier-metrics
    (should (eq :write (anvil-worker--classify
                        "(file-replace-string \"x\" \"a\" \"b\")")))
    (should (eq :write (anvil-worker--classify "(file-batch \"x\" '(...))")))
    (should (eq :write (anvil-worker--classify "(buffer-save \"buf\")")))
    (should (eq :write (anvil-worker--classify "(save-buffer)")))
    (should (eq :write (anvil-worker--classify
                        "(write-region 1 2 \"x\" nil 'silent)")))))

(ert-deftest anvil-worker-test-classify-batch-needs-pool ()
  "Batch patterns route to :batch only when batch-pool-size > 0."
  (anvil-worker-test--with-fresh-classifier-metrics
    (let ((anvil-worker-batch-pool-size 0))
      (should (eq :write
                  (anvil-worker--classify "(byte-compile-file \"x.el\")"))))
    (let ((anvil-worker-batch-pool-size 2))
      (should (eq :batch
                  (anvil-worker--classify "(byte-compile-file \"x.el\")"))))))

(ert-deftest anvil-worker-test-classify-write-beats-read ()
  "An expression mixing read+write keywords is escalated to :write."
  (anvil-worker-test--with-fresh-classifier-metrics
    ;; Contains both file-read and file-replace-string.
    (let ((expr "(progn (file-read \"x\") (file-replace-string \"x\" \"a\" \"b\"))"))
      (should (eq :write (anvil-worker--classify expr))))))

(ert-deftest anvil-worker-test-classify-unknown-falls-back-to-write ()
  "Unrecognised expressions hit the safe-side `:write' fallback."
  (anvil-worker-test--with-fresh-classifier-metrics
    (should (eq :write (anvil-worker--classify "(my-mystery-fn 1 2 3)")))
    (should (= 1 (plist-get anvil-worker--metrics-classify :unknown-fallback)))
    (should (= 1 (plist-get anvil-worker--metrics-classify :write)))))

(ert-deftest anvil-worker-test-classify-fallback-customisable ()
  "Changing the fallback custom moves unknown expressions elsewhere."
  (anvil-worker-test--with-fresh-classifier-metrics
    (let ((anvil-worker-classify-unknown-fallback :read))
      (should (eq :read (anvil-worker--classify "(unknown-call)"))))))

(ert-deftest anvil-worker-test-classify-anvil-prefix-accepted ()
  "Patterns accept the `anvil-' prefix as well as the bare name."
  (anvil-worker-test--with-fresh-classifier-metrics
    (should (eq :read  (anvil-worker--classify "(anvil-file-read \"x\")")))
    (should (eq :write (anvil-worker--classify
                        "(anvil-file-replace-string \"x\" \"a\" \"b\")")))))

(ert-deftest anvil-worker-test-classify-non-string-returns-fallback ()
  "Non-string EXPRESSION (nil, list) hits the fallback bucket."
  (anvil-worker-test--with-fresh-classifier-metrics
    (should (eq :write (anvil-worker--classify nil)))
    (should (= 1 (plist-get anvil-worker--metrics-classify :unknown-fallback)))))

(ert-deftest anvil-worker-test-metrics-bump-counts-each-call ()
  "Every classify call bumps exactly one bucket (plus fallback when miss)."
  (anvil-worker-test--with-fresh-classifier-metrics
    (anvil-worker--classify "(file-read \"x\")")
    (anvil-worker--classify "(file-replace-string \"x\" \"a\" \"b\")")
    (anvil-worker--classify "(unknown-thing)")
    (should (= 1 (plist-get anvil-worker--metrics-classify :read)))
    (should (= 2 (plist-get anvil-worker--metrics-classify :write)))
    (should (= 1 (plist-get anvil-worker--metrics-classify :unknown-fallback)))))

;;;; --- pick-worker integration with classifier ----------------------------

(ert-deftest anvil-worker-test-pick-auto-uses-classifier-read ()
  ":kind :auto on a read expression picks the :read lane."
  (anvil-worker-test--with-pool
      '(:read 1 :write 1)
      '("anvil-worker-read-1" "anvil-worker-write-1")
    (anvil-worker-test--with-fresh-classifier-metrics
      (let ((w (anvil-worker--pick-worker :auto "(file-read \"x\")")))
        (should (eq :read (plist-get w :lane)))))))

(ert-deftest anvil-worker-test-pick-auto-uses-classifier-write ()
  ":kind :auto on a write expression picks the :write lane."
  (anvil-worker-test--with-pool
      '(:read 1 :write 1)
      '("anvil-worker-read-1" "anvil-worker-write-1")
    (anvil-worker-test--with-fresh-classifier-metrics
      (let ((w (anvil-worker--pick-worker
                :auto "(file-replace-string \"x\" \"a\" \"b\")")))
        (should (eq :write (plist-get w :lane)))))))

(ert-deftest anvil-worker-test-pick-explicit-kind-overrides-classifier ()
  ":kind :read forces the read lane even for an obvious write expression."
  (anvil-worker-test--with-pool
      '(:read 1 :write 1)
      '("anvil-worker-read-1" "anvil-worker-write-1")
    (anvil-worker-test--with-fresh-classifier-metrics
      (let ((w (anvil-worker--pick-worker
                :read "(file-replace-string \"x\" \"a\" \"b\")")))
        (should (eq :read (plist-get w :lane)))
        ;; Explicit kind must NOT touch classifier metrics.
        (should (= 0 (plist-get anvil-worker--metrics-classify :write)))))))

(ert-deftest anvil-worker-test-pick-auto-falls-through-to-write ()
  "When classifier picks :read but the read lane is dead, fall back to :write."
  (anvil-worker-test--with-pool
      '(:read 1 :write 1)
      '("anvil-worker-write-1")  ; only write alive
    (anvil-worker-test--with-fresh-classifier-metrics
      (let ((w (anvil-worker--pick-worker :auto "(file-read \"x\")")))
        (should (eq :write (plist-get w :lane)))))))

;;;; --- batch warmup (Doc 01 Phase 3) --------------------------------------

(ert-deftest anvil-worker-test-batch-pool-default-is-one ()
  "Phase 3 enabled the batch lane by default (size 1)."
  (should (= 1 (default-value 'anvil-worker-batch-pool-size))))

(ert-deftest anvil-worker-test-batch-warmup-defaults-non-empty ()
  "Default warmup expression list pre-loads at least one library."
  (let ((defaults (default-value 'anvil-worker-batch-warmup-expressions)))
    (should (> (length defaults) 0))
    (should (cl-every #'stringp defaults))))

(ert-deftest anvil-worker-test-send-warmup-noop-when-dead ()
  "`--send-warmup' returns nil and emits no calls when worker is dead."
  (let* ((worker (list :lane :batch :index 0
                       :name "anvil-worker-batch-1"
                       :server-file "/tmp/no-such"
                       :busy nil :last-state nil))
         (calls 0))
    (cl-letf (((symbol-function 'anvil-worker--worker-alive-p)
               (lambda (_) nil))
              ((symbol-function 'call-process)
               (lambda (&rest _) (cl-incf calls) 0))
              ((symbol-function 'anvil-worker--log)
               (lambda (&rest _) nil)))
      (should-not (anvil-worker--send-warmup worker))
      (should (= 0 calls)))))

(ert-deftest anvil-worker-test-send-warmup-fires-each-expression ()
  "Each warmup expression issues exactly one fire-and-forget emacsclient call."
  (let* ((worker (list :lane :batch :index 0
                       :name "anvil-worker-batch-1"
                       :server-file "/tmp/sf"
                       :busy nil :last-state nil))
         (anvil-worker-batch-warmup-expressions
          '("(require 'org)" "(require 'cl-lib)"))
         (recorded '()))
    (cl-letf (((symbol-function 'anvil-worker--worker-alive-p)
               (lambda (_) t))
              ((symbol-function 'call-process)
               (lambda (&rest args) (push args recorded) 0))
              ((symbol-function 'anvil-worker--log)
               (lambda (&rest _) nil)))
      (let ((sent (anvil-worker--send-warmup worker)))
        (should (= 2 (length sent)))
        (should (= 2 (length recorded)))
        ;; Every dispatch goes through emacsclient -n -f SERVER-FILE -e EXPR.
        (dolist (call recorded)
          (should (equal "emacsclient" (nth 0 call)))
          (should (member "-n" call))
          (should (member "-f" call))
          (should (member "/tmp/sf" call)))))))

(ert-deftest anvil-worker-test-maybe-schedule-warmup-only-batch ()
  "Read- and write-lane spawns must NOT trigger warmup scheduling."
  (let (scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest args) (push args scheduled) nil)))
      (anvil-worker--maybe-schedule-warmup
       (list :lane :read :name "r" :server-file "/x"))
      (anvil-worker--maybe-schedule-warmup
       (list :lane :write :name "w" :server-file "/x"))
      (should (null scheduled))
      (anvil-worker--maybe-schedule-warmup
       (list :lane :batch :name "b" :server-file "/x"))
      (should (= 1 (length scheduled))))))

(ert-deftest anvil-worker-test-maybe-schedule-warmup-honours-empty-config ()
  "An empty `batch-warmup-expressions' suppresses scheduling."
  (let ((scheduled 0)
        (anvil-worker-batch-warmup-expressions '()))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) (cl-incf scheduled) nil)))
      (anvil-worker--maybe-schedule-warmup
       (list :lane :batch :name "b" :server-file "/x"))
      (should (= 0 scheduled)))))

(ert-deftest anvil-worker-test-pick-batch-lane-with-pool ()
  "With batch-pool-size > 0 the dispatcher routes :kind :batch correctly."
  (anvil-worker-test--with-pool
      '(:read 1 :write 1 :batch 1)
      '("anvil-worker-batch-1")
    (let ((w (anvil-worker--pick-worker :batch)))
      (should w)
      (should (eq :batch (plist-get w :lane))))))

(ert-deftest anvil-worker-test-classify-batch-routes-when-pool-set ()
  "With batch-pool-size > 0 the classifier returns :batch for matching exprs."
  (anvil-worker-test--with-fresh-classifier-metrics
    (let ((anvil-worker-batch-pool-size 1))
      (should (eq :batch
                  (anvil-worker--classify "(byte-compile-file \"x.el\")"))))))

;;;; --- latency metrics (Doc 01 Phase 4a) ----------------------------------

(defmacro anvil-worker-test--with-fresh-latency (&rest body)
  "Run BODY against an empty per-lane latency bucket plist."
  (declare (indent 0))
  `(let ((anvil-worker--metrics-latency nil))
     (anvil-worker--latency-init)
     ,@body))

(ert-deftest anvil-worker-test-latency-init-shape ()
  "Init creates an empty bucket per lane with all running sums at zero."
  (anvil-worker-test--with-fresh-latency
    (dolist (lane '(:read :write :batch))
      (let ((b (plist-get anvil-worker--metrics-latency lane)))
        (should (= 0 (plist-get b :samples)))
        (should (= 0.0 (plist-get b :spawn-ms-sum)))
        (should (= 0.0 (plist-get b :wait-ms-sum)))
        (should (= 0.0 (plist-get b :total-ms-sum)))
        (should (null (plist-get b :totals)))))))

(ert-deftest anvil-worker-test-latency-record-accumulates ()
  "`--latency-record' bumps the count and sums per lane."
  (anvil-worker-test--with-fresh-latency
    (anvil-worker--latency-record :read 10.0 90.0)
    (anvil-worker--latency-record :read 20.0 80.0)
    (let ((b (plist-get anvil-worker--metrics-latency :read)))
      (should (= 2 (plist-get b :samples)))
      (should (= 30.0 (plist-get b :spawn-ms-sum)))
      (should (= 170.0 (plist-get b :wait-ms-sum)))
      (should (= 200.0 (plist-get b :total-ms-sum)))
      (should (equal '(100.0 100.0) (plist-get b :totals))))))

(ert-deftest anvil-worker-test-latency-isolates-lanes ()
  "A read sample never leaks into the write bucket."
  (anvil-worker-test--with-fresh-latency
    (anvil-worker--latency-record :read  5.0 25.0)
    (anvil-worker--latency-record :write 7.0 33.0)
    (should (= 1 (plist-get (plist-get anvil-worker--metrics-latency :read)
                            :samples)))
    (should (= 1 (plist-get (plist-get anvil-worker--metrics-latency :write)
                            :samples)))
    (should (= 0 (plist-get (plist-get anvil-worker--metrics-latency :batch)
                            :samples)))))

(ert-deftest anvil-worker-test-latency-ring-buffer-cap ()
  "Sample ring drops oldest entries past `anvil-worker-latency-sample-cap'."
  (anvil-worker-test--with-fresh-latency
    (let ((anvil-worker-latency-sample-cap 3))
      (dotimes (i 5)
        (anvil-worker--latency-record :read 1.0 (float i)))
      (let* ((b (plist-get anvil-worker--metrics-latency :read))
             (totals (plist-get b :totals)))
        (should (= 5 (plist-get b :samples)))
        (should (= 3 (length totals)))
        ;; Most recent first; oldest two (1+0, 1+1) dropped.
        (should (equal (list 5.0 4.0 3.0) totals))))))

(ert-deftest anvil-worker-test-latency-cap-zero-keeps-sums-only ()
  "Cap=0 stops sample retention but lets running sums keep counting."
  (anvil-worker-test--with-fresh-latency
    (let ((anvil-worker-latency-sample-cap 0))
      (anvil-worker--latency-record :write 1.0 9.0)
      (anvil-worker--latency-record :write 2.0 8.0)
      (let ((b (plist-get anvil-worker--metrics-latency :write)))
        (should (= 2  (plist-get b :samples)))
        (should (= 20.0 (plist-get b :total-ms-sum)))
        (should (null (plist-get b :totals)))))))

(ert-deftest anvil-worker-test-percentile-empty-returns-nil ()
  (should (null (anvil-worker--percentile nil 50)))
  (should (null (anvil-worker--percentile nil 99))))

(ert-deftest anvil-worker-test-percentile-single-element ()
  (should (= 42 (anvil-worker--percentile '(42) 50)))
  (should (= 42 (anvil-worker--percentile '(42) 99))))

(ert-deftest anvil-worker-test-percentile-bounds ()
  "p0 returns the min, p100 returns the max."
  (let ((s '(10 20 30 40 50 60 70 80 90 100)))
    (should (= 10  (anvil-worker--percentile s 0)))
    (should (= 100 (anvil-worker--percentile s 100)))))

(ert-deftest anvil-worker-test-percentile-mid ()
  "p50 lands somewhere in the middle of a sorted sample."
  (let* ((s '(1 2 3 4 5 6 7 8 9 10))
         (p50 (anvil-worker--percentile s 50)))
    (should (and (>= p50 4) (<= p50 6)))))

(ert-deftest anvil-worker-test-latency-reset-clears-everything ()
  "`anvil-worker-latency-metrics-reset' returns to an empty state."
  (anvil-worker-test--with-fresh-latency
    (anvil-worker--latency-record :read 5.0 5.0)
    (anvil-worker-latency-metrics-reset)
    (should (= 0 (plist-get (plist-get anvil-worker--metrics-latency :read)
                            :samples)))
    (should (null (plist-get (plist-get anvil-worker--metrics-latency :read)
                             :totals)))))

;;;; --- server-eval-at transport (Doc 01 Phase 4c) -------------------------

(ert-deftest anvil-worker-test-connection-method-default-is-emacsclient ()
  "Phase 4c shipped `server-eval-at' as an OPT-IN transport; the default
stays on `emacsclient' because the new path reproduced a hard main-daemon
deadlock on Windows (2026-04-16) that could not be broken with
`with-timeout'."
  (should (eq 'emacsclient
              (default-value 'anvil-worker-connection-method))))

(ert-deftest anvil-worker-test-dispatch-server-eval-at-roundtrips-value ()
  "`--dispatch-via-server-eval-at' returns prin1 of the server's result."
  (anvil-worker-test--with-fresh-latency
    (cl-letf (((symbol-function 'server-eval-at)
               (lambda (name form)
                 (should (equal "anvil-worker-read-1" name))
                 (should (equal '(emacs-pid) form))
                 12345)))
      (let* ((worker (list :lane :read :name "anvil-worker-read-1"))
             (res (anvil-worker--dispatch-via-server-eval-at
                   worker "(emacs-pid)" 10)))
        (should (equal "12345" res))))))

(ert-deftest anvil-worker-test-dispatch-server-eval-at-wraps-errors ()
  "A signal inside the remote form becomes an *ERROR* string."
  (anvil-worker-test--with-fresh-latency
    (cl-letf (((symbol-function 'server-eval-at)
               (lambda (_name _form) (error "boom from remote"))))
      (let* ((worker (list :lane :read :name "w"))
             (res (anvil-worker--dispatch-via-server-eval-at
                   worker "(whatever)" 10)))
        (should (string-match-p "\\`\\*ERROR\\*" res))
        (should (string-match-p "boom from remote" res))))))

(ert-deftest anvil-worker-test-dispatch-server-eval-at-records-latency ()
  "server-eval-at path records a sample with spawn-ms=0 (no child proc)."
  (anvil-worker-test--with-fresh-latency
    (cl-letf (((symbol-function 'server-eval-at)
               (lambda (_name _form) 'ok)))
      (let ((worker (list :lane :write :name "anvil-worker-write-1")))
        (anvil-worker--dispatch-via-server-eval-at worker "(x)" 10))
      (let ((b (plist-get anvil-worker--metrics-latency :write)))
        (should (= 1 (plist-get b :samples)))
        (should (= 0.0 (plist-get b :spawn-ms-sum)))
        (should (>= (plist-get b :wait-ms-sum) 0.0))))))

(ert-deftest anvil-worker-test-call-honours-connection-method ()
  "Setting anvil-worker-connection-method picks the matching dispatcher."
  (anvil-worker-test--with-pool
      '(:read 1 :write 1)
      '("anvil-worker-read-1" "anvil-worker-write-1")
    (anvil-worker-test--with-fresh-classifier-metrics
      (anvil-worker-test--with-fresh-latency
        (let (dispatched)
          (cl-letf (((symbol-function 'anvil-worker--dispatch-via-server-eval-at)
                     (lambda (&rest _) (push 'server dispatched) "s"))
                    ((symbol-function 'anvil-worker--dispatch-via-emacsclient)
                     (lambda (&rest _) (push 'client dispatched) "c")))
            (let ((anvil-worker-connection-method 'server-eval-at))
              (anvil-worker-call "(emacs-pid)" :kind :read :timeout 1))
            (let ((anvil-worker-connection-method 'emacsclient))
              (anvil-worker-call "(emacs-pid)" :kind :read :timeout 1))
            (should (equal '(client server) dispatched))))))))

;;; anvil-worker-test.el ends here
