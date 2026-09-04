;;; anvil-fusion-route-test.el --- ERT for anvil-fusion-route -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for Doc 62 Phase 2's hard-prompt router.  The
;; orchestrator seam is faked with scripted single-task results by task
;; name; escalation is tested by stubbing `anvil-fusion-ask' directly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(or (require 'anvil-orchestrator nil t)
    (provide 'anvil-orchestrator))
(require 'anvil-fusion-route)

(defmacro anvil-fusion-route-test--with-fake-orchestrator
    (script &rest body)
  "Run BODY with a scripted fake single-task orchestrator.
SCRIPT is an alist NAME -> plist describing the task result.  Each
plist may contain :status, :summary, :error, or :signal.  Binds
`submitted' to the newest-first submitted task plists."
  (declare (indent 1) (debug t))
  `(let ((submitted '())
         (batches '())
         (batch-counter 0))
     (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                (lambda (tasks)
                  (let* ((task (car tasks))
                         (batch (format "b%d" (setq batch-counter (1+ batch-counter))))
                         (name (plist-get task :name)))
                    (push task submitted)
                    (push (cons batch (list :task-id (format "%s-id" name)
                                            :script (cdr (assoc name ,script))))
                          batches)
                    batch)))
               ((symbol-function 'anvil-orchestrator-collect)
                (lambda (&rest _) t))
               ((symbol-function 'anvil-orchestrator-status)
                (lambda (id)
                  (let* ((entry (assoc id batches))
                         (task-id (plist-get (cdr entry) :task-id)))
                    (list :tasks (and task-id (list (list :id task-id)))))))
               ((symbol-function 'anvil-orchestrator-extract-result)
                (lambda (task-id full)
                  (should full)
                  (let* ((entry (cl-find-if
                                 (lambda (cell)
                                   (equal task-id (plist-get (cdr cell) :task-id)))
                                 batches))
                         (script-plist (plist-get (cdr entry) :script)))
                    (when (plist-get script-plist :signal)
                      (signal 'error (list (plist-get script-plist :signal))))
                    (append (list :status (or (plist-get script-plist :status) 'done))
                            (and (plist-member script-plist :summary)
                                 (list :summary (plist-get script-plist :summary)))
                            (and (plist-member script-plist :error)
                                 (list :error (plist-get script-plist :error))))))))
       ,@body)))

(defun anvil-fusion-route-test--entry (tier &optional outcome)
  "Return one synthetic routing log entry for TIER and OUTCOME."
  (append (list :time 0.0
                :prompt-head "Q"
                :risk (pcase tier
                        ('single 'low)
                        (_ 'high))
                :reason ""
                :tier tier
                :panel (and (eq tier 'panel) 'opus-solo)
                :verify (eq tier 'panel))
          (and outcome (list :outcome outcome))))

(defun anvil-fusion-route-test--repeat-entry (n tier &optional outcome-fn)
  "Return N synthetic entries for TIER.
OUTCOME-FN receives the zero-based index and returns the outcome plist."
  (let (entries)
    (dotimes (idx n (nreverse entries))
      (push (anvil-fusion-route-test--entry
             tier
             (and outcome-fn (funcall outcome-fn idx)))
            entries))))

(ert-deftest anvil-fusion-route-test-parse-risk-exact ()
  (should (equal '(:risk low :reason "looks fine")
                 (anvil-fusion-route--parse-risk
                  "RISK: LOW\nREASON: looks fine"))))

(ert-deftest anvil-fusion-route-test-parse-risk-lowercase-and-prose ()
  (should (equal '(:risk high :reason "many constraints")
                 (anvil-fusion-route--parse-risk
                  "audit follows\nrisk: high\nreason: many constraints\nthanks"))))

(ert-deftest anvil-fusion-route-test-parse-risk-garbage-falls-to-medium ()
  (should (equal '(:risk medium :reason "")
                 (anvil-fusion-route--parse-risk "nonsense only"))))

(ert-deftest anvil-fusion-route-test-parse-risk-missing-reason-empty ()
  (should (equal '(:risk medium :reason "")
                 (anvil-fusion-route--parse-risk "RISK: medium"))))

(ert-deftest anvil-fusion-route-test-decide-defaults ()
  (should (equal '(:tier single)
                 (anvil-fusion-route--decide 'low)))
  (should (equal '(:tier panel :panel claude-pair :verify nil)
                 (anvil-fusion-route--decide 'medium)))
  (should (equal '(:tier panel :panel opus-solo :verify t)
                 (anvil-fusion-route--decide 'high)))
  (should (equal '(:tier panel :panel claude-pair :verify nil)
                 (anvil-fusion-route--decide 'weird))))

(ert-deftest anvil-fusion-route-test-single-path-low-risk-skips-ask ()
  (let ((anvil-fusion-route--log nil)
        ask-called)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest _) (setq ask-called t) (error "must not run"))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status done :summary "SINGLE-ANSWER")
            ("fusion-route-probe" :status done :summary "RISK: LOW\nREASON: stable"))
        (let ((result (anvil-fusion-route-ask "Q")))
          (should (equal "SINGLE-ANSWER" (plist-get result :answer)))
          (should (eq 'single (plist-get (plist-get result :route) :tier)))
          (should (eq 'low (plist-get (plist-get result :route) :risk)))
          (should-not ask-called)
          (should (= 1 (length (anvil-fusion-route-log)))))))))

(ert-deftest anvil-fusion-route-test-medium-path-escalates-to-claude-pair ()
  (let ((anvil-fusion-route--log nil)
        ask-args)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest args)
                 (setq ask-args args)
                 '(:answer "PANEL-MED" :panel claude-pair :verify nil :extra-tag t))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status done :summary "SINGLE-ANSWER")
            ("fusion-route-probe" :status done :summary "RISK: MEDIUM\nREASON: maybe wrong"))
        (let ((result (anvil-fusion-route-ask "Q" :extra "X")))
          (should (equal "PANEL-MED" (plist-get result :answer)))
          (should (eq 'claude-pair (plist-get (cdr ask-args) :panel)))
          (should-not (plist-get (cdr ask-args) :verify))
          (should (equal "SINGLE-ANSWER"
                         (plist-get (plist-get result :route) :single-answer)))
          (should (eq 'panel (plist-get (plist-get result :route) :tier))))))))

(ert-deftest anvil-fusion-route-test-high-path-escalates-to-opus-solo-verify ()
  (let (ask-args)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest args)
                 (setq ask-args args)
                 '(:answer "PANEL-HIGH"))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status done :summary "SINGLE-ANSWER")
            ("fusion-route-probe" :status done :summary "RISK: HIGH\nREASON: critical"))
        (let ((result (anvil-fusion-route-ask "Q")))
          (should (equal "PANEL-HIGH" (plist-get result :answer)))
          (should (eq 'opus-solo (plist-get (cdr ask-args) :panel)))
          (should (plist-get (cdr ask-args) :verify))
          (should (eq 'high (plist-get (plist-get result :route) :risk))))))))

(ert-deftest anvil-fusion-route-test-forced-path-skips-single-and-probe ()
  (let ((anvil-fusion-route-force-hard-regexps '("CRITICAL"))
        ask-args)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest args)
                 (setq ask-args args)
                 '(:answer "FORCED"))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status done :summary "SHOULD-NOT-RUN")
            ("fusion-route-probe" :status done :summary "SHOULD-NOT-RUN"))
        (let ((result (anvil-fusion-route-ask "CRITICAL: do the hard thing")))
          (should (equal "FORCED" (plist-get result :answer)))
          (should (eq 'forced (plist-get (plist-get result :route) :risk)))
          (should (eq 'opus-solo (plist-get (cdr ask-args) :panel)))
          (should-not (cl-find-if
                       (lambda (task)
                         (equal "fusion-route-single" (plist-get task :name)))
                       submitted)))))))

(ert-deftest anvil-fusion-route-test-single-ask-failure-escalates-high ()
  (let (ask-args)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest args)
                 (setq ask-args args)
                 '(:answer "FALLBACK"))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status failed :error "boom"))
        (let ((result (anvil-fusion-route-ask "Q")))
          (should (equal "FALLBACK" (plist-get result :answer)))
          (should (eq 'opus-solo (plist-get (cdr ask-args) :panel)))
          (should (plist-get (cdr ask-args) :verify))
          (should (eq 'high (plist-get (plist-get result :route) :risk)))
          (should (string-match-p "failed"
                                  (plist-get (plist-get result :route) :reason))))))))

(ert-deftest anvil-fusion-route-test-probe-failure-falls-to-medium ()
  (let (ask-args)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest args)
                 (setq ask-args args)
                 '(:answer "PROBE-FAIL"))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status done :summary "SINGLE-ANSWER")
            ("fusion-route-probe" :signal "probe broke"))
        (let ((result (anvil-fusion-route-ask "Q")))
          (should (equal "PROBE-FAIL" (plist-get result :answer)))
          (should (eq 'claude-pair (plist-get (cdr ask-args) :panel)))
          (should-not (plist-get (cdr ask-args) :verify))
          (should (eq 'medium (plist-get (plist-get result :route) :risk))))))))

(ert-deftest anvil-fusion-route-test-probe-garbage-falls-to-medium ()
  (let (ask-args)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest args)
                 (setq ask-args args)
                 '(:answer "PROBE-GARBAGE"))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status done :summary "SINGLE-ANSWER")
            ("fusion-route-probe" :status done :summary "unstructured prose"))
        (let ((result (anvil-fusion-route-ask "Q")))
          (should (equal "PROBE-GARBAGE" (plist-get result :answer)))
          (should (eq 'claude-pair (plist-get (cdr ask-args) :panel)))
          (should (eq 'medium (plist-get (plist-get result :route) :risk))))))))

(ert-deftest anvil-fusion-route-test-log-ring-clamps-to-size ()
  (let ((anvil-fusion-route--log nil)
        (anvil-fusion-route-log-size 2))
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (&rest _) '(:answer "PANEL"))))
      (anvil-fusion-route-test--with-fake-orchestrator
          '(("fusion-route-single" :status done :summary "SINGLE-ANSWER")
            ("fusion-route-probe" :status done :summary "RISK: LOW\nREASON: ok"))
        (anvil-fusion-route-ask "Q1")
        (anvil-fusion-route-ask "Q2")
        (anvil-fusion-route-ask "Q3")
        (let ((log (anvil-fusion-route-log)))
          (should (= 2 (length log)))
          (should (equal "Q3" (plist-get (car log) :prompt-head)))
          (should (equal "Q2" (plist-get (cadr log) :prompt-head))))))))

(ert-deftest anvil-fusion-route-test-annotate-outcome-attaches ()
  (let ((anvil-fusion-route--log
         (list (anvil-fusion-route-test--entry 'single)
               (anvil-fusion-route-test--entry 'panel))))
    (anvil-fusion-route-annotate-outcome
     0 '(:verified-refuted-count 2 :reask t :cost-usd 1.5))
    (should (equal '(:verified-refuted-count 2 :reask t :cost-usd 1.5)
                   (plist-get (car anvil-fusion-route--log) :outcome)))))

(ert-deftest anvil-fusion-route-test-annotate-outcome-out-of-range-noop ()
  (let* ((entry (anvil-fusion-route-test--entry 'single))
         (anvil-fusion-route--log (list (copy-tree entry))))
    (anvil-fusion-route-annotate-outcome 5 '(:reask t))
    (should (equal (list entry) anvil-fusion-route--log))))

(ert-deftest anvil-fusion-route-test-annotate-outcome-nil-log-safe ()
  (let ((anvil-fusion-route--log nil))
    (should-not (anvil-fusion-route-annotate-outcome 0 '(:reask t)))
    (should-not anvil-fusion-route--log)))

(ert-deftest anvil-fusion-route-test-calibrate-empty-log ()
  (let ((report (anvil-fusion-route-calibrate '())))
    (should (= 0 (plist-get report :n)))
    (should-not (plist-get report :proposals))
    (should (member "No routing decisions available; calibration proposals suppressed."
                    (plist-get report :notes)))))

(ert-deftest anvil-fusion-route-test-calibrate-proposes-tighten-low ()
  (let* ((log (anvil-fusion-route-test--repeat-entry
               25
               'single
               (lambda (idx)
                 (list :reask (< idx 6)
                       :cost-usd 0.1))))
         (report (let ((anvil-fusion-route-calib-min-samples 20)
                       (anvil-fusion-route-calib-low-reask-max 0.2))
                   (anvil-fusion-route-calibrate log)))
         (proposal (car (plist-get report :proposals)))
         (evidence (plist-get proposal :evidence)))
    (should (= 25 (plist-get report :n)))
    (should (eq 'tighten-low (plist-get proposal :change)))
    (should (= 6 (plist-get evidence :reask-count)))
    (should (= 25 (plist-get evidence :reask-samples)))
    (should (= 25 (plist-get evidence :count)))
    (should (> (plist-get evidence :reask-rate) 0.2))))

(ert-deftest anvil-fusion-route-test-calibrate-proposes-relax-high ()
  (let* ((log (anvil-fusion-route-test--repeat-entry
               22
               'panel
               (lambda (_idx)
                 (list :verified-refuted-count 0
                       :cost-usd 0.8))))
         (report (let ((anvil-fusion-route-calib-min-samples 20)
                       (anvil-fusion-route-calib-high-refuted-min 0.1))
                   (anvil-fusion-route-calibrate log)))
         (proposal (car (plist-get report :proposals)))
         (evidence (plist-get proposal :evidence)))
    (should (eq 'relax-high (plist-get proposal :change)))
    (should (= 0 (plist-get evidence :verified-refuted-sum)))
    (should (= 22 (plist-get evidence :verified-refuted-samples)))
    (should (= 22 (plist-get evidence :count)))
    (should (= 0.0 (plist-get evidence :mean-verified-refuted-count)))))

(ert-deftest anvil-fusion-route-test-calibrate-below-min-samples-noted ()
  (let* ((log (anvil-fusion-route-test--repeat-entry
               5
               'single
               (lambda (idx)
                 (list :reask (zerop idx)
                       :cost-usd 0.1))))
         (report (let ((anvil-fusion-route-calib-min-samples 20))
                   (anvil-fusion-route-calibrate log))))
    (should-not (plist-get report :proposals))
    (should (cl-some (lambda (note)
                       (string-match-p "LOW tier omitted from proposals: 5/20 re-ask samples\\." note))
                     (plist-get report :notes)))))

(ert-deftest anvil-fusion-route-test-calibrate-report-string-manual-only ()
  (let* ((anvil-fusion-route-escalation
          '((low . (:tier single))
            (medium . (:tier panel :panel claude-pair :verify nil))
            (high . (:tier panel :panel opus-solo :verify t))))
         (before (copy-tree anvil-fusion-route-escalation))
         (log (append
               (anvil-fusion-route-test--repeat-entry
                25 'single
                (lambda (idx)
                  (list :reask (< idx 6) :cost-usd 0.1)))
               (anvil-fusion-route-test--repeat-entry
                22 'panel
                (lambda (_idx)
                  (list :verified-refuted-count 0 :cost-usd 0.8)))))
         (report-string (let ((anvil-fusion-route-calib-min-samples 20))
                          (anvil-fusion-route-calibrate-report-string log))))
    (should (string-match-p "適用は手動: anvil-fusion-route-escalation を編集"
                            report-string))
    (should (equal before anvil-fusion-route-escalation))))

(provide 'anvil-fusion-route-test)
;;; anvil-fusion-route-test.el ends here
