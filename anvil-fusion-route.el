;;; anvil-fusion-route.el --- Hard-prompt router over single ask and Fusion -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; Doc 62 §4 Phase 2 ("hard-prompt router") and §6's probe-cost bound:
;; answer cheaply first, risk-check THAT answer cheaply, then pay for a
;; heavier panel only when the answer looks risky.  The motivation is
;; empirical, not aesthetic: Doc 61 Phase 6e's 2026-07-03 battery
;; measured a strong single model saturated on easy prompts (Tier A
;; plain judge 10/10) while fusion+verify cost ~4x and only paid for
;; harder multi-constraint prompts (Tier B 79% -> 100%).
;;
;; This module implements that cascade:
;;
;;   single ask -> cheap probe -> optional escalation
;;
;; Tier 1 and the probe each submit exactly ONE orchestrator task using
;; the same public-API submit -> collect -> first-task-id ->
;; extract-result pattern Phase 6a uses for claim extraction.  The
;; escalation step reuses `anvil-fusion-ask' unchanged.
;;
;; The module is load-time pure: it eagerly requires only
;; `anvil-fusion', `anvil-fusion-verify', and `anvil-fusion-ask', all of
;; which keep their orchestrator dependency lazy.  `anvil-orchestrator'
;; is required only at call time inside the thin runtime wrapper.
;;
;; Future optimization deliberately NOT in v1: when escalation happens,
;; the tier-1 answer is returned under :route :single-answer for later
;; comparison, but is not injected into the panel prompt itself.
;;
;; Wiring into optional-module registration and MCP exposure is deferred
;; by the task request; this file only provides the standalone router.

;;; Code:

(require 'cl-lib)
(require 'anvil-fusion)
(require 'anvil-fusion-verify)
(require 'anvil-fusion-ask)

(declare-function anvil-orchestrator-submit "anvil-orchestrator" (tasks))
(declare-function anvil-orchestrator-collect "anvil-orchestrator" (batch-id &rest _))
(declare-function anvil-orchestrator-extract-result "anvil-orchestrator" (task-id &optional full))

(defgroup anvil-fusion nil
  "Fusion judge harness layered over anvil-orchestrator."
  :group 'tools
  :prefix "anvil-fusion-")

(defcustom anvil-fusion-route-single-provider 'claude
  "Provider used for the tier-1 single ask."
  :type 'symbol
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-single-model nil
  "Model used for the tier-1 single ask, or nil for the provider default.
Unlike the probe tier, this is passed through as-is when non-nil."
  :type '(choice (const :tag "Provider default" nil) string)
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-probe-provider 'claude
  "Provider used for the cheap answer-risk probe."
  :type 'symbol
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-probe-model "haiku"
  "Model used for the cheap answer-risk probe when the provider is `claude'.
This is a claude-scoped alias, so it is applied only when the
effective probe provider is `claude'."
  :type '(choice (const :tag "Provider default" nil) string)
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-probe-template
  "あなたは回答の危険度を判定する監査役です。原問と回答を見て、(a) 独立した専門家が回答中の結論を左右する主張に異を唱える見込み、(b) 検証されていない重要な数値・条文・API 名の存在、(c) 多制約の取りこぼしの兆候、を評価してください。
Output EXACTLY two lines:
RISK: LOW|MEDIUM|HIGH
REASON: <one line>

# 原問
%s

# 回答
%s"
  "Probe prompt template for routing.
Contains exactly two `%s' slots: (1) the original prompt, (2) the
tier-1 single answer."
  :type 'string
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-escalation
  '((low    . (:tier single))
    (medium . (:tier panel :panel claude-pair :verify nil))
    (high   . (:tier panel :panel opus-solo  :verify t)))
  "Risk-to-action calibration knob for `anvil-fusion-route-ask'."
  :type '(alist :key-type symbol :value-type plist)
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-force-hard-regexps nil
  "Regexps whose match forces direct execution of the `high' action.
Intended for domain-critical prompt markers wired by the user in init."
  :type '(repeat regexp)
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-log-size 100
  "Maximum number of routing decisions retained in `anvil-fusion-route--log'."
  :type 'integer
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-calib-min-samples 20
  "Minimum outcome-bearing samples required before a tier yields proposals."
  :type 'integer
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-calib-low-reask-max 0.2
  "Maximum tolerated LOW-tier re-ask rate before proposing tighter routing."
  :type 'number
  :group 'anvil-fusion)

(defcustom anvil-fusion-route-calib-high-refuted-min 0.1
  "Minimum tolerated HIGH-tier mean refuted-claim count.
When the measured mean falls below this threshold over enough
outcome-bearing HIGH samples, calibration proposes relaxing the
MEDIUM-to-HIGH bar."
  :type 'number
  :group 'anvil-fusion)

(defconst anvil-fusion-route--risk-line-regexp
  "risk[ \t]*:[ \t]*\\([[:alpha:]]+\\)"
  "Case-insensitive regexp matching a probe `RISK:' line anywhere.")

(defconst anvil-fusion-route--reason-line-regexp
  "reason[ \t]*:[ \t]*\\(.*\\)"
  "Case-insensitive regexp matching a probe `REASON:' line anywhere.")

(defvar anvil-fusion-route--log nil
  "Newest-first routing decision log.
Each entry is a plist:
  (:time FLOAT :prompt-head STRING :risk SYMBOL :reason STRING
   :tier SYMBOL :panel SYMBOL-OR-NIL :verify BOOL
   [:outcome (:verified-refuted-count INT :reask BOOL :cost-usd FLOAT)])")

(defun anvil-fusion-route--probe-prompt (question answer)
  "Build the routing probe prompt from QUESTION and ANSWER."
  (format anvil-fusion-route-probe-template
          (or question "")
          (or answer "")))

(defun anvil-fusion-route--parse-risk (text)
  "Tolerantly parse probe TEXT into (:risk SYM :reason STR).
Searches for `RISK:' / `REASON:' labels anywhere, case-insensitively.
Unknown or absent risk normalizes to `medium'; absent reason to \"\"."
  (let ((case-fold-search t)
        (risk 'medium)
        (reason ""))
    (when (stringp text)
      (when (string-match anvil-fusion-route--risk-line-regexp text)
        (let ((parsed (intern-soft (downcase (match-string 1 text)))))
          (when (memq parsed '(low medium high))
            (setq risk parsed))))
      (when (string-match anvil-fusion-route--reason-line-regexp text)
        (setq reason (string-trim (match-string 1 text)))))
    (list :risk risk :reason reason)))

(defun anvil-fusion-route--decide (risk)
  "Return the action plist for RISK from `anvil-fusion-route-escalation'.
Unknown RISK keys fall back to the `medium' action."
  (copy-tree
   (or (cdr (assq risk anvil-fusion-route-escalation))
       (cdr (assq 'medium anvil-fusion-route-escalation)))))

(defun anvil-fusion-route--force-hard-p (prompt)
  "Return non-nil when PROMPT matches any force-hard regexp."
  (and (stringp prompt)
       (cl-some (lambda (re) (string-match-p re prompt))
                anvil-fusion-route-force-hard-regexps)))

(defun anvil-fusion-route--probe-model-for (provider)
  "Return the effective probe model for PROVIDER."
  (and (eq provider 'claude) anvil-fusion-route-probe-model))

(defun anvil-fusion-route--prompt-head (prompt)
  "Return the first 80 chars of PROMPT for logging."
  (substring (or prompt "") 0 (min 80 (length (or prompt "")))))

(defun anvil-fusion-route--push-log (prompt risk reason action)
  "Record one routing decision for PROMPT / RISK / REASON / ACTION."
  (let ((entry (list :time (float-time)
                     :prompt-head (anvil-fusion-route--prompt-head prompt)
                     :risk risk
                     :reason (or reason "")
                     :tier (plist-get action :tier)
                     :panel (plist-get action :panel)
                     :verify (and (plist-get action :verify) t))))
    (push entry anvil-fusion-route--log)
    (when (> (length anvil-fusion-route--log) anvil-fusion-route-log-size)
      (setq anvil-fusion-route--log
            (cl-subseq anvil-fusion-route--log 0 anvil-fusion-route-log-size)))
    (message "anvil-fusion-route: risk=%s tier=%s panel=%s verify=%s reason=%s"
             risk
             (plist-get action :tier)
             (or (plist-get action :panel) "-")
             (if (plist-get action :verify) "t" "nil")
             (or reason ""))
    entry))

(defun anvil-fusion-route-log ()
  "Return a copy of the routing decision log."
  (copy-tree anvil-fusion-route--log))

(defun anvil-fusion-route--sanitize-outcome (outcome-plist)
  "Return a normalized outcome plist from OUTCOME-PLIST.
Outcome attachment is best-effort only; absent keys are left absent so
offline calibration degrades gracefully when signals are missing."
  (let (outcome)
    (when (plist-member outcome-plist :verified-refuted-count)
      (let ((value (plist-get outcome-plist :verified-refuted-count)))
        (when (integerp value)
          (setq outcome
                (append outcome (list :verified-refuted-count value))))))
    (when (plist-member outcome-plist :reask)
      (setq outcome
            (append outcome
                    (list :reask (and (plist-get outcome-plist :reask) t)))))
    (when (plist-member outcome-plist :cost-usd)
      (let ((value (plist-get outcome-plist :cost-usd)))
        (when (numberp value)
          (setq outcome
                (append outcome (list :cost-usd (float value)))))))
    outcome))

(defun anvil-fusion-route-annotate-outcome (index outcome-plist)
  "Best-effort attach OUTCOME-PLIST to routing log entry at INDEX.
INDEX counts newest-first from zero.  OUTCOME-PLIST may contain
`:verified-refuted-count', `:reask', and `:cost-usd'.  Out-of-range
indexes, missing log state, and malformed payloads are ignored; this
annotation is opportunistic and never required for the router itself."
  (condition-case nil
      (let ((cell (and (natnump index) (nthcdr index anvil-fusion-route--log)))
            (outcome (anvil-fusion-route--sanitize-outcome outcome-plist)))
        (when (and cell outcome)
          (setcar cell (plist-put (copy-tree (car cell)) :outcome outcome))))
    (error nil))
  nil)

(defun anvil-fusion-route--calib-stats-init ()
  "Return a fresh calibration accumulator plist."
  (list :count 0
        :cost-sum 0.0
        :cost-count 0
        :reask-count 0
        :reask-samples 0
        :refuted-sum 0
        :refuted-samples 0))

(defun anvil-fusion-route--calib-stats-update (stats entry)
  "Return STATS updated with ENTRY."
  (let ((outcome (plist-get entry :outcome)))
    (setq stats (plist-put stats :count (1+ (plist-get stats :count))))
    (when (numberp (plist-get outcome :cost-usd))
      (setq stats (plist-put stats :cost-sum
                             (+ (plist-get stats :cost-sum)
                                (plist-get outcome :cost-usd))))
      (setq stats (plist-put stats :cost-count
                             (1+ (plist-get stats :cost-count)))))
    (when (plist-member outcome :reask)
      (setq stats (plist-put stats :reask-samples
                             (1+ (plist-get stats :reask-samples))))
      (when (plist-get outcome :reask)
        (setq stats (plist-put stats :reask-count
                               (1+ (plist-get stats :reask-count))))))
    (when (integerp (plist-get outcome :verified-refuted-count))
      (setq stats (plist-put stats :refuted-sum
                             (+ (plist-get stats :refuted-sum)
                                (plist-get outcome :verified-refuted-count))))
      (setq stats (plist-put stats :refuted-samples
                             (1+ (plist-get stats :refuted-samples))))))
  stats)

(defun anvil-fusion-route--calib-tier-signal (tier stats)
  "Return report signal plist for TIER from STATS."
  (let ((cost-count (plist-get stats :cost-count))
        (reask-samples (plist-get stats :reask-samples))
        (refuted-samples (plist-get stats :refuted-samples)))
    (append
     (list :count (plist-get stats :count)
           :cost-samples cost-count
           :mean-cost-usd (and (> cost-count 0)
                               (/ (plist-get stats :cost-sum) cost-count)))
     (when (eq tier 'low)
       (list :reask-count (plist-get stats :reask-count)
             :reask-samples reask-samples
             :reask-rate (and (> reask-samples 0)
                              (/ (float (plist-get stats :reask-count))
                                 reask-samples))))
     (when (eq tier 'high)
       (list :verified-refuted-sum (plist-get stats :refuted-sum)
             :verified-refuted-samples refuted-samples
             :mean-verified-refuted-count
             (and (> refuted-samples 0)
                  (/ (float (plist-get stats :refuted-sum))
                     refuted-samples)))))))

(defun anvil-fusion-route--calib-format-float (value)
  "Format numeric VALUE compactly for calibration text."
  (if (numberp value)
      (format "%.2f" (float value))
    "n/a"))

(defun anvil-fusion-route--calib-low-proposal (signal)
  "Return a LOW-tier proposal plist from SIGNAL, or nil."
  (let ((samples (plist-get signal :reask-samples))
        (reask-count (plist-get signal :reask-count))
        (rate (plist-get signal :reask-rate)))
    (when (and (>= samples anvil-fusion-route-calib-min-samples)
               (numberp rate)
               (> rate anvil-fusion-route-calib-low-reask-max))
      (list :change 'tighten-low
            :summary "Lower the LOW->MEDIUM bar so more borderline prompts escalate."
            :why "LOW-tier same-session re-asks exceed the configured ceiling."
            :evidence (list :tier 'low
                            :count (plist-get signal :count)
                            :reask-count reask-count
                            :reask-samples samples
                            :reask-rate rate
                            :threshold anvil-fusion-route-calib-low-reask-max
                            :mean-cost-usd (plist-get signal :mean-cost-usd))))))

(defun anvil-fusion-route--calib-high-proposal (signal)
  "Return a HIGH-tier proposal plist from SIGNAL, or nil."
  (let ((samples (plist-get signal :verified-refuted-samples))
        (mean-refuted (plist-get signal :mean-verified-refuted-count)))
    (when (and (>= samples anvil-fusion-route-calib-min-samples)
               (numberp mean-refuted)
               (< mean-refuted anvil-fusion-route-calib-high-refuted-min))
      (list :change 'relax-high
            :summary "Raise the MEDIUM->HIGH bar so fewer prompts pay for verify-heavy escalation."
            :why "HIGH-tier escalations rarely produce refuted claims in follow-up verification."
            :evidence (list :tier 'high
                            :count (plist-get signal :count)
                            :verified-refuted-sum (plist-get signal :verified-refuted-sum)
                            :verified-refuted-samples samples
                            :mean-verified-refuted-count mean-refuted
                            :threshold anvil-fusion-route-calib-high-refuted-min
                            :mean-cost-usd (plist-get signal :mean-cost-usd))))))

(defun anvil-fusion-route-calibrate (&optional log)
  "Analyze LOG and return an offline calibration report plist.
LOG defaults to `anvil-fusion-route-log'.  The report is descriptive
only and never mutates `anvil-fusion-route-escalation'."
  (let* ((entries (copy-tree (or log (anvil-fusion-route-log))))
         (stats-by-tier nil)
         (stats-by-risk nil)
         (by-tier nil)
         proposals
         notes)
    (dolist (entry entries)
      (let* ((tier (plist-get entry :tier))
             (risk (plist-get entry :risk))
             (tier-stats (or (alist-get tier stats-by-tier)
                        (anvil-fusion-route--calib-stats-init))))
        (setq tier-stats (anvil-fusion-route--calib-stats-update tier-stats entry))
        (setf (alist-get tier stats-by-tier) tier-stats)
        (when (memq risk '(low medium high))
          (let ((risk-stats (or (alist-get risk stats-by-risk)
                                (anvil-fusion-route--calib-stats-init))))
            (setq risk-stats
                  (anvil-fusion-route--calib-stats-update risk-stats entry))
            (setf (alist-get risk stats-by-risk) risk-stats)))))
    (setq by-tier
          (mapcar (lambda (tier)
                    (cons tier (plist-get (alist-get tier stats-by-tier) :count)))
                  '(single panel)))
    (let* ((low-signal (anvil-fusion-route--calib-tier-signal
                        'low
                        (or (alist-get 'low stats-by-risk)
                            (anvil-fusion-route--calib-stats-init))))
           (medium-signal (anvil-fusion-route--calib-tier-signal
                           'medium
                           (or (alist-get 'medium stats-by-risk)
                            (anvil-fusion-route--calib-stats-init))))
           (high-signal (anvil-fusion-route--calib-tier-signal
                         'high
                         (or (alist-get 'high stats-by-risk)
                             (anvil-fusion-route--calib-stats-init)))))
      (when (zerop (length entries))
        (setq notes (list "No routing decisions available; calibration proposals suppressed.")))
      (unless (>= (plist-get low-signal :reask-samples) anvil-fusion-route-calib-min-samples)
        (push (format "LOW tier omitted from proposals: %d/%d re-ask samples."
                      (plist-get low-signal :reask-samples)
                      anvil-fusion-route-calib-min-samples)
              notes))
      (unless (>= (plist-get high-signal :verified-refuted-samples)
                  anvil-fusion-route-calib-min-samples)
        (push (format "HIGH tier omitted from proposals: %d/%d verify-outcome samples."
                      (plist-get high-signal :verified-refuted-samples)
                      anvil-fusion-route-calib-min-samples)
              notes))
      (setq proposals (delq nil (list (anvil-fusion-route--calib-low-proposal low-signal)
                                      (anvil-fusion-route--calib-high-proposal high-signal))))
      (list :n (length entries)
            :by-tier by-tier
            :signals (list (cons 'low low-signal)
                           (cons 'medium medium-signal)
                           (cons 'high high-signal))
            :proposals proposals
            :notes (nreverse notes)))))

(defun anvil-fusion-route-calibrate-report-string (&optional log)
  "Return a compact multi-line offline calibration report for LOG."
  (let* ((report (anvil-fusion-route-calibrate log))
         (signals (plist-get report :signals))
         (low (alist-get 'low signals))
         (high (alist-get 'high signals))
         (proposals (plist-get report :proposals))
         (notes (plist-get report :notes))
         (lines
          (list
           (format "Router calibration: n=%d single=%d panel=%d"
                   (plist-get report :n)
                   (or (alist-get 'single (plist-get report :by-tier)) 0)
                   (or (alist-get 'panel (plist-get report :by-tier)) 0))
           (format "LOW  reask=%s/%s rate=%s mean-cost=%s"
                   (or (plist-get low :reask-count) 0)
                   (or (plist-get low :reask-samples) 0)
                   (anvil-fusion-route--calib-format-float
                    (plist-get low :reask-rate))
                   (anvil-fusion-route--calib-format-float
                    (plist-get low :mean-cost-usd)))
           (format "HIGH refuted=%s/%s mean=%s mean-cost=%s"
                   (or (plist-get high :verified-refuted-sum) 0)
                   (or (plist-get high :verified-refuted-samples) 0)
                   (anvil-fusion-route--calib-format-float
                    (plist-get high :mean-verified-refuted-count))
                   (anvil-fusion-route--calib-format-float
                    (plist-get high :mean-cost-usd))))))
    (if proposals
        (cl-loop for proposal in proposals
                 for idx from 1
                 do (push (format "%d. %s [%s]"
                                  idx
                                  (plist-get proposal :summary)
                                  (plist-get proposal :change))
                          lines)
                 do (push (format "   why: %s evidence=%S"
                                  (plist-get proposal :why)
                                  (plist-get proposal :evidence))
                          lines))
      (push "0. No calibration proposals." lines))
    (dolist (note notes)
      (push (format "note: %s" note) lines))
    (push "適用は手動: anvil-fusion-route-escalation を編集" lines)
    (mapconcat #'identity (nreverse lines) "\n")))

(cl-defun anvil-fusion-route--run-single-task
    (name prompt provider &key model cwd timeout-sec (max-wait-sec 1800))
  "Submit one orchestrator task and return its full result plist.
NAME, PROMPT, and PROVIDER define the task.  MODEL / CWD /
TIMEOUT-SEC / MAX-WAIT-SEC are forwarded when present."
  (require 'anvil-orchestrator)
  (let* ((task (append (list :name name :provider provider :prompt prompt)
                       (and model (list :model model))
                       (and cwd (list :cwd cwd))
                       (and timeout-sec (list :timeout-sec timeout-sec))))
         (batch (anvil-orchestrator-submit (list task))))
    (anvil-orchestrator-collect batch :wait t :max-wait-sec max-wait-sec)
    (anvil-orchestrator-extract-result
     (anvil-fusion--batch-first-task-id batch) t)))

(defun anvil-fusion-route--single-failure-route (prompt reason action ask-args)
  "Escalate on tier-1 failure using ACTION and ASK-ARGS."
  (let* ((route (list :risk 'high
                      :reason reason
                      :tier (plist-get action :tier)
                      :panel (plist-get action :panel)
                      :verify (and (plist-get action :verify) t)
                      :single-answer nil))
         (result (apply #'anvil-fusion-ask prompt
                        :panel (plist-get action :panel)
                        :verify (plist-get action :verify)
                        ask-args)))
    (anvil-fusion-route--push-log prompt 'high reason action)
    (append result (list :route route))))

(cl-defun anvil-fusion-route-ask
    (prompt &key cwd timeout-sec (max-wait-sec 1800) verify-args lenses extra)
  "Answer PROMPT via a routed cascade.
Run a single ask first, probe that answer cheaply, then escalate
only when the probe says the answer is risky.

Returns a plist whose :answer is the final answer and whose :route
describes the routing decision:
  (:risk SYM :reason STR :tier SYM :panel SYMBOL-OR-NIL
   :verify BOOL :single-answer STR-OR-NIL)."
  (let* ((high-action (anvil-fusion-route--decide 'high))
         (ask-args (append (and cwd (list :cwd cwd))
                           (and timeout-sec (list :timeout-sec timeout-sec))
                           (list :max-wait-sec max-wait-sec)
                           (and verify-args (list :verify-args verify-args))
                           (and lenses (list :lenses lenses))
                           (and extra (list :extra extra)))))
    (if (anvil-fusion-route--force-hard-p prompt)
        (let* ((route (list :risk 'forced
                            :reason "force-hard regexp matched"
                            :tier (plist-get high-action :tier)
                            :panel (plist-get high-action :panel)
                            :verify (and (plist-get high-action :verify) t)
                            :single-answer nil))
               (result (apply #'anvil-fusion-ask prompt
                              :panel (plist-get high-action :panel)
                              :verify (plist-get high-action :verify)
                              ask-args)))
          (anvil-fusion-route--push-log prompt 'forced "force-hard regexp matched" high-action)
          (append result (list :route route)))
      (condition-case nil
          (let* ((single-result
                  (anvil-fusion-route--run-single-task
                   "fusion-route-single"
                   prompt
                   anvil-fusion-route-single-provider
                   :model anvil-fusion-route-single-model
                   :cwd cwd
                   :timeout-sec timeout-sec
                   :max-wait-sec max-wait-sec))
                 (single-status (plist-get single-result :status))
                 (single-answer (plist-get single-result :summary)))
            (if (or (not (eq single-status 'done))
                    (not (stringp single-answer)))
                (anvil-fusion-route--single-failure-route
                 prompt
                 "single ask failed"
                 high-action
                 ask-args)
              (let* ((probe-result
                      (condition-case nil
                          (anvil-fusion-route--run-single-task
                           "fusion-route-probe"
                           (anvil-fusion-route--probe-prompt prompt single-answer)
                           anvil-fusion-route-probe-provider
                           :model (anvil-fusion-route--probe-model-for
                                   anvil-fusion-route-probe-provider)
                           :cwd cwd
                           :timeout-sec timeout-sec
                           :max-wait-sec max-wait-sec)
                        (error nil)))
                     (probe-status (and probe-result (plist-get probe-result :status)))
                     (probe-summary (and probe-result (plist-get probe-result :summary)))
                     (probe-parsed (if (and probe-result (eq probe-status 'done))
                                       (anvil-fusion-route--parse-risk probe-summary)
                                     (list :risk 'medium :reason "probe failed")))
                     (action (anvil-fusion-route--decide
                              (plist-get probe-parsed :risk)))
                     (route (list :risk (plist-get probe-parsed :risk)
                                  :reason (plist-get probe-parsed :reason)
                                  :tier (plist-get action :tier)
                                  :panel (plist-get action :panel)
                                  :verify (and (plist-get action :verify) t)
                                  :single-answer single-answer)))
                (anvil-fusion-route--push-log
                 prompt
                 (plist-get probe-parsed :risk)
                 (plist-get probe-parsed :reason)
                 action)
                (if (eq (plist-get action :tier) 'single)
                    (list :answer single-answer :route route)
                  (append
                   (apply #'anvil-fusion-ask prompt
                          :panel (plist-get action :panel)
                          :verify (plist-get action :verify)
                          ask-args)
                   (list :route route))))))
        (error
         (anvil-fusion-route--single-failure-route
          prompt
          "single ask failed"
          high-action
          ask-args))))))

(provide 'anvil-fusion-route)
;;; anvil-fusion-route.el ends here
