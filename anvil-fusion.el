;;; anvil-fusion.el --- Fusion judge harness over anvil-orchestrator -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; anvil-fusion recovers Fable-5-class output WITHOUT touching the banned
;; model, by fanning a prompt out to a panel of *available* models and
;; having a judge model synthesize one answer (the "Fusion" technique).
;;
;; anvil-orchestrator already implements ~80% of this (consensus fan-out,
;; a meta-LLM judge, latency routing).  anvil-fusion adds the remaining
;; 20%.  This file is Phase 1 of docs/design/01-fusion-harness.org:
;;
;;   * a *structured* judge template (Fusion-style: consensus /
;;     contradictions / partial coverage / unique insights / gaps ->
;;     synthesized answer) instead of orchestrator's thin "best of" prompt;
;;   * a *full-output fidelity* mode that feeds the judge each candidate's
;;     untruncated answer (orchestrator's :summary is clamped at 4000
;;     chars) for code / long-form tasks.
;;
;; The prompt-building layer is pure and depends on nothing at load time,
;; so it is unit-testable in isolation.  `anvil-fusion-judge-consensus'
;; is the thin orchestration wrapper; it lazily requires
;; `anvil-orchestrator' and uses only its public API
;; (`anvil-orchestrator-consensus-collect', `-extract-result',
;; `-submit', `-status').
;;
;; Later phases add the 1-call `anvil-fusion-ask' wrapper, a critique
;; loop, role-lens injection, named panels (quality / sovereign), and a
;; local eval harness.

;;; Code:

(require 'cl-lib)

;; Public anvil-orchestrator functions used only at call time.  Declared
;; (not required) so the pure prompt layer loads without dragging in the
;; whole orchestrator stack.
(declare-function anvil-orchestrator-consensus-collect "anvil-orchestrator" (consensus-id &rest _))
(declare-function anvil-orchestrator-extract-result "anvil-orchestrator" (task-id &optional full))
(declare-function anvil-orchestrator-submit "anvil-orchestrator" (tasks))
(declare-function anvil-orchestrator-status "anvil-orchestrator" (id))

(defgroup anvil-fusion nil
  "Fusion judge harness layered over anvil-orchestrator."
  :group 'tools
  :prefix "anvil-fusion-")

;;;; --- customization -------------------------------------------------------

(defcustom anvil-fusion-judge-template
  "あなたは、ひとつの問いに対する複数の AI アシスタントの回答を統合する判定者です。

# 原問
%s

# 候補回答
%s

# 手順
以下を順に行ってください。
1. 合意点 — 複数の候補が一致して主張している点を列挙する。
2. 矛盾点 — 候補間で食い違う主張を挙げ、どちらがより妥当かを根拠とともに判定する。
3. 部分カバー — 一部の候補だけが触れている重要な点。
4. 独自洞察 — 単一の候補のみが提供した価値ある視点。
5. 見落とし — どの候補も答えていないが、原問が要求している点。

上記の分析を踏まえ、それらを統合した最終回答を作成する。最終回答は、いずれの
候補単体よりも正確かつ網羅的でなければならない。失敗・空・エラーの候補は割り引く。

# 出力形式
## 分析
（手順 1〜5 を簡潔に）
## 最終回答
（原問に対する、統合された単一の回答）"
  "Judge prompt template (Fusion-structured synthesis).
Two %s placeholders are filled in order: (1) the original prompt,
(2) the formatted candidate block.  Unlike orchestrator's thin
\"best of\" template this asks the judge to extract consensus /
contradictions / partial coverage / unique insights / gaps before
synthesizing, which is where the bulk of the ensemble lift comes
from (Fusion: ~3/4 of the lift is synthesis)."
  :type 'string
  :group 'anvil-fusion)

(defcustom anvil-fusion-default-fidelity 'summary
  "Default candidate fidelity passed to the judge.
`summary' uses orchestrator's parsed `:summary' (clamped at
`anvil-orchestrator-summary-max-chars', default 4000) and is the
cheap default that fits most single-turn answers.  `full'
re-parses each candidate's stdout untruncated via
`anvil-orchestrator-extract-result' — the right knob for code or
long-form (report / legal) tasks where truncation loses fidelity."
  :type '(choice (const :tag "Truncated summary (cheap)" summary)
                 (const :tag "Full untruncated output" full))
  :group 'anvil-fusion)

(defcustom anvil-fusion-judge-default-provider 'claude
  "Default provider for the synthesis judge.
Defaults to the strongest *available* model (Opus 4.8 via the
`claude' provider) since Fable 5 is unavailable.  Override per
call with the :judge argument to `anvil-fusion-judge-consensus'."
  :type 'symbol
  :group 'anvil-fusion)

(defcustom anvil-fusion-judge-dedup nil
  "When non-nil, compress near-duplicate candidate answers for the judge.
This is OFF by default so judge prompts remain byte-identical to
the pre-dedup behavior unless explicitly enabled."
  :type 'boolean
  :group 'anvil-fusion)

(defcustom anvil-fusion-judge-dedup-threshold 0.9
  "Minimum Jaccard similarity to cluster two judge candidates together.
Two candidate texts whose `anvil-fusion--jaccard' score meets or
exceeds this threshold are treated as near-duplicates for
judge-input deduplication."
  :type 'float
  :group 'anvil-fusion)

;;;; --- pure prompt-building layer (no orchestrator load needed) ------------

(defun anvil-fusion--candidate-text (candidate &optional fidelity)
  "Return the answer text for one CANDIDATE slim plist.
CANDIDATE is a slim task plist as returned in the :tasks list of
`anvil-orchestrator-consensus-collect' (keys :id :provider
:status :summary :error).  FIDELITY (defaulting to
`anvil-fusion-default-fidelity') selects `summary' or `full'.  In
`full' mode the candidate's untruncated answer is fetched via
`anvil-orchestrator-extract-result' when that function is
available and the candidate carries an :id; any failure degrades
gracefully back to the truncated :summary.  Returns a non-empty
string, or the candidate :error, or \"(no output)\"."
  (let* ((fid (or fidelity anvil-fusion-default-fidelity))
         (id  (plist-get candidate :id))
         (summary (plist-get candidate :summary))
         (text
          (cond
           ((and (eq fid 'full) id
                 (fboundp 'anvil-orchestrator-extract-result))
            (or (condition-case nil
                    (plist-get (anvil-orchestrator-extract-result id t)
                               :summary)
                  (error nil))
                summary))
           (t summary))))
    (if (and (stringp text) (not (string-empty-p (string-trim text))))
        text
      (or (plist-get candidate :error) "(no output)"))))

(defun anvil-fusion--candidate-label (candidate index)
  "Return the existing judge label string for CANDIDATE at INDEX."
  (format "%d. [provider: %s, status: %s]"
          (1+ index)
          (or (plist-get candidate :provider) "?")
          (or (plist-get candidate :status) "?")))

(defun anvil-fusion--format-labeled-candidate (label text)
  "Return LABEL and TEXT formatted exactly like the normal judge block."
  (format "%s\n%s" label text))

(defun anvil-fusion--candidate-labeled-texts (candidates &optional fidelity)
  "Return CANDIDATES as a list of (LABEL . TEXT) for judge rendering."
  (let ((i 0)
        acc)
    (dolist (candidate candidates)
      (push (cons (anvil-fusion--candidate-label candidate i)
                  (anvil-fusion--candidate-text candidate fidelity))
            acc)
      (setq i (1+ i)))
    (nreverse acc)))

(defun anvil-fusion--judge-dedup-diff (seed-label seed-text member-label member-text)
  "Return a unified diff from SEED-TEXT to MEMBER-TEXT, or nil on failure."
  (let ((old-file (make-temp-file "anvil-fusion-dedup-old-"))
        (new-file (make-temp-file "anvil-fusion-dedup-new-")))
    (unwind-protect
        (progn
          (with-temp-file old-file
            (insert seed-text))
          (with-temp-file new-file
            (insert member-text))
          (with-temp-buffer
            (let ((status (call-process "diff" nil (current-buffer) nil
                                        "--label" seed-label
                                        "--label" member-label
                                        "-u"
                                        old-file
                                        new-file)))
              (when (memq status '(0 1))
                (buffer-string)))))
      (ignore-errors (delete-file old-file))
      (ignore-errors (delete-file new-file)))))

(defun anvil-fusion--dedup-clusters (labeled-texts threshold)
  "Greedily cluster LABELED-TEXTS by seed similarity at THRESHOLD.
LABELED-TEXTS is a list of (LABEL . TEXT).  Each cluster preserves
input order and has the form (:seed (LABEL . TEXT) :members ...),
where :members excludes the seed itself."
  (let ((remaining labeled-texts)
        clusters)
    (while remaining
      (let* ((seed (car remaining))
             (seed-text (cdr seed))
             (rest (cdr remaining))
             members
             next-remaining)
        (dolist (item rest)
          (if (>= (anvil-fusion--jaccard seed-text (cdr item)) threshold)
              (push item members)
            (push item next-remaining)))
        (push (list :seed seed :members (nreverse members)) clusters)
        (setq remaining (nreverse next-remaining))))
    (nreverse clusters)))

(defun anvil-fusion--format-candidates-deduped (candidates &optional fidelity)
  "Format CANDIDATES with near-duplicates collapsed to representative deltas."
  (if (null candidates)
      "(no candidates)"
    (let* ((labeled-texts (anvil-fusion--candidate-labeled-texts candidates fidelity))
           (clusters (anvil-fusion--dedup-clusters
                      labeled-texts
                      anvil-fusion-judge-dedup-threshold)))
      (mapconcat
       (lambda (cluster)
         (let* ((seed (plist-get cluster :seed))
                (seed-label (car seed))
                (seed-text (cdr seed))
                (seed-block (anvil-fusion--format-labeled-candidate
                             seed-label seed-text))
                (member-blocks
                 (mapcar
                  (lambda (member)
                    (let* ((member-label (car member))
                           (member-text (cdr member))
                           (full-block (anvil-fusion--format-labeled-candidate
                                        member-label member-text)))
                      (condition-case nil
                          (let ((diff (anvil-fusion--judge-dedup-diff
                                       seed-label seed-text member-label member-text)))
                            (cond
                             ((null diff) full-block)
                             ((string-empty-p diff)
                              (format
                               "### %s\n（%s とほぼ同一の回答。差分なし・実質同一）"
                               member-label seed-label))
                             ((>= (length diff)
                                  (* 0.8 (max 1 (length member-text))))
                              full-block)
                             (t
                              (format
                               (concat
                                "### %s\n"
                                "（%s とほぼ同一の回答。差分のみ:）\n"
                                "%s")
                               member-label seed-label diff))))
                        (error full-block))))
                  (plist-get cluster :members))))
           (mapconcat #'identity
                      (cons seed-block member-blocks)
                      "\n\n")))
       clusters
       "\n\n"))))

(defun anvil-fusion--format-candidates (candidates &optional fidelity)
  "Format CANDIDATES (slim plist list) into a numbered judge block.
Each entry shows its 1-based index, provider, status and answer
text (per FIDELITY).  Returns \"(no candidates)\" when empty."
  (if (null candidates)
      "(no candidates)"
    (or (and anvil-fusion-judge-dedup
             (>= (length candidates) 2)
             (condition-case nil
                 (anvil-fusion--format-candidates-deduped candidates fidelity)
               (error nil)))
        (let ((i 0) acc)
          (dolist (c candidates)
            (setq i (1+ i))
            (push (format "%d. [provider: %s, status: %s]\n%s"
                          i
                          (or (plist-get c :provider) "?")
                          (or (plist-get c :status) "?")
                          (anvil-fusion--candidate-text c fidelity))
                  acc))
          (mapconcat #'identity (nreverse acc) "\n\n")))))

(cl-defun anvil-fusion-build-judge-prompt
    (original-prompt candidates &key template fidelity extra)
  "Build the full Fusion judge prompt.
ORIGINAL-PROMPT is the question the panel answered.  CANDIDATES is
the slim plist list.  :TEMPLATE overrides
`anvil-fusion-judge-template' (must contain two %s).  :FIDELITY
overrides `anvil-fusion-default-fidelity'.  :EXTRA, when a
non-empty string, is appended after the rendered prompt to adjust
adjudication instructions without redefining the template.  Pure
function — safe to call without anvil-orchestrator loaded (unless
:fidelity is `full', which fetches untruncated output at call
time)."
  (let* ((tmpl (or template anvil-fusion-judge-template))
         (core (format tmpl
                       (or original-prompt "")
                       (anvil-fusion--format-candidates candidates fidelity))))
    (if (and extra (stringp extra) (not (string-empty-p extra)))
        (concat core "\n\n" extra)
      core)))

;;;; --- orchestration wrapper (lazy require of anvil-orchestrator) ----------

(defun anvil-fusion--batch-first-task-id (batch-id)
  "Return the first task id of BATCH-ID via the public status API."
  (let* ((status (anvil-orchestrator-status batch-id))
         (tasks  (plist-get status :tasks)))
    (plist-get (car tasks) :id)))

(cl-defun anvil-fusion-judge-consensus
    (consensus-id &key original-prompt judge judge-model fidelity
                  template extra wait (max-wait-sec 1800) timeout-sec)
  "Submit a Fusion synthesis judge task for CONSENSUS-ID.

Collects the consensus fan-out batch (pass :WAIT t to block until
terminal), builds the structured Fusion judge prompt from the
candidate answers, and submits it as a single new task to the
judge provider.

ORIGINAL-PROMPT is the question the panel answered; it must be
supplied (the slim consensus plists omit it).  As a convenience,
when nil and orchestrator's private
`anvil-orchestrator--judge-original-prompt' is available it is
used as a fallback.  :JUDGE defaults to
`anvil-fusion-judge-default-provider'.  :FIDELITY / :TEMPLATE /
:EXTRA are forwarded to `anvil-fusion-build-judge-prompt'.

Returns (:consensus-id :judge-task-id :judge-batch-id
:judge-provider :judge-model :prompt-chars).  Poll the result
with `anvil-fusion-judge-result'."
  (require 'anvil-orchestrator)
  (let* ((collected (anvil-orchestrator-consensus-collect
                     consensus-id :wait wait :max-wait-sec max-wait-sec))
         (candidates (plist-get collected :tasks))
         (orig (or original-prompt
                   (and (fboundp 'anvil-orchestrator--judge-original-prompt)
                        (fboundp 'anvil-orchestrator--consensus-groups-get)
                        (ignore-errors
                          (anvil-orchestrator--judge-original-prompt
                           (anvil-orchestrator--consensus-groups-get
                            consensus-id))))))
         (prov (or judge anvil-fusion-judge-default-provider)))
    (unless candidates
      (user-error "anvil-fusion: consensus %s has no candidates" consensus-id))
    (unless orig
      (user-error
       "anvil-fusion: original-prompt required (slim consensus plists omit it)"))
    (let* ((prompt (anvil-fusion-build-judge-prompt
                    orig candidates
                    :template template :fidelity fidelity :extra extra))
           (task (append
                  (list :name (format "fusion-judge-%s"
                                      (substring consensus-id 0
                                                 (min 8 (length consensus-id))))
                        :provider prov
                        :prompt prompt)
                  (and judge-model (list :model judge-model))
                  (and timeout-sec (list :timeout-sec timeout-sec))))
           (batch (anvil-orchestrator-submit (list task)))
           (task-id (anvil-fusion--batch-first-task-id batch)))
      (list :consensus-id   consensus-id
            :judge-task-id  task-id
            :judge-batch-id batch
            :judge-provider prov
            :judge-model    judge-model
            :prompt-chars   (length prompt)))))

(cl-defun anvil-fusion-judge-result (judge-task-id &key (full t))
  "Return the synthesized answer plist for JUDGE-TASK-ID.
Thin wrapper over `anvil-orchestrator-extract-result'.  FULL
defaults to t so the (possibly long) synthesis narrative is not
truncated."
  (require 'anvil-orchestrator)
  (anvil-orchestrator-extract-result judge-task-id full))

;;;; --- convergence measure + critique loop (Phase 4) ----------------------

(defcustom anvil-fusion-max-rounds 1
  "Maximum number of *critique* rounds beyond the initial fan-out.
0 disables looping (one fan-out + one judge).  Each extra round
re-runs the whole panel on a critique of the current draft, so
this is a hard cap on cost (`暴走防止')."
  :type 'integer
  :group 'anvil-fusion)

(defcustom anvil-fusion-debate nil
  "When non-nil, critique rounds become bounded multi-round debate.
Nil preserves today's exact critique-loop behavior: one
divergence-triggered re-round whose members see only the critique
prompt.  Non-nil makes each extra round show a member its own prior
answer, the other members' prior answers rendered with the Phase 7
dedup formatter, and the current verified-claims table.  Cost grows
roughly linearly with debate depth: each extra round is another member
fan-out + judge +, when verification is enabled, incremental claim
verification.  Doc 62's router should therefore reserve depth > 1 for
HIGH-tier prompts."
  :type 'boolean
  :group 'anvil-fusion)

(defcustom anvil-fusion-converge-threshold 0.5
  "Minimum pairwise candidate similarity to consider a round converged.
When the least-similar pair of candidate answers scores below
this (Jaccard over character shingles, 0.0-1.0), the panel
disagreed and another critique round is worthwhile (subject to
`anvil-fusion-max-rounds')."
  :type 'float
  :group 'anvil-fusion)

(defcustom anvil-fusion-critique-template
  "原問:
%s

現在の統合草案:
%s

この草案を批判的に検討してください。事実誤り・不足・論理の飛躍・見落としを\
具体的に指摘し、それらを修正した、原問に対するより良い回答を作成してください。\
草案をそのまま繰り返さず、改善した回答そのものを返すこと。"
  "Template for a critique round's member prompt.
Two %s placeholders: (1) the original question, (2) the current
synthesized draft.  Each panel member receives this and returns
an improved answer, which the judge then re-synthesizes."
  :type 'string
  :group 'anvil-fusion)

(defun anvil-fusion--shingles (text)
  "Return the set (deduped list) of character bigrams of TEXT.
Whitespace is collapsed away first so the measure is layout- and
language-agnostic (works for CJK and Latin).  A <2-char string
yields itself (or nil when empty)."
  (let* ((s (replace-regexp-in-string "[ \t\n\r　]+" "" (or text "")))
         (n (length s))
         acc)
    (cond
     ((= n 0) nil)
     ((< n 2) (list s))
     (t (dotimes (i (1- n))
          (push (substring s i (+ i 2)) acc))
        (delete-dups (nreverse acc))))))

(defun anvil-fusion--jaccard (a b)
  "Return the Jaccard similarity (0.0-1.0) of strings A and B."
  (let ((sa (anvil-fusion--shingles a))
        (sb (anvil-fusion--shingles b)))
    (if (and (null sa) (null sb))
        1.0
      (let* ((inter (length (cl-intersection sa sb :test #'equal)))
             (uni   (length (cl-union sa sb :test #'equal))))
        (if (zerop uni) 1.0 (/ (float inter) uni))))))

(defun anvil-fusion--candidate-answers (candidates)
  "Return the non-empty answer texts of done CANDIDATES.
Uses the slim :summary; failed / empty candidates are dropped so
they do not skew the similarity measure."
  (delq nil
        (mapcar (lambda (c)
                  (let ((s (plist-get c :summary)))
                    (and (stringp s)
                         (not (string-empty-p (string-trim s)))
                         s)))
                candidates)))

(defun anvil-fusion-min-pairwise-similarity (texts)
  "Return the minimum pairwise Jaccard similarity among TEXTS.
Returns 1.0 when fewer than two texts are given (nothing to
disagree about)."
  (if (< (length texts) 2)
      1.0
    (let ((m 1.0))
      (cl-loop for (a . rest) on texts do
               (dolist (b rest)
                 (setq m (min m (anvil-fusion--jaccard a b)))))
      m)))

(defun anvil-fusion-should-loop-p (candidates &optional threshold)
  "Return non-nil when CANDIDATES disagree enough to warrant a round.
True when at least two candidates produced answers and their
minimum pairwise similarity is below THRESHOLD (default
`anvil-fusion-converge-threshold')."
  (let ((texts (anvil-fusion--candidate-answers candidates)))
    (and (>= (length texts) 2)
         (< (anvil-fusion-min-pairwise-similarity texts)
            (or threshold anvil-fusion-converge-threshold)))))

(defun anvil-fusion-build-critique-prompt (original draft &optional template)
  "Build a critique-round member prompt from ORIGINAL and DRAFT.
TEMPLATE overrides `anvil-fusion-critique-template' (two %s)."
  (format (or template anvil-fusion-critique-template)
          (or original "") (or draft "")))

(defun anvil-fusion-build-debate-prompt
    (original-prompt prior-answer other-candidates claims-block &optional fidelity)
  "Build a debate-round member prompt from prior round context.
ORIGINAL-PROMPT is the original user question.  PRIOR-ANSWER is the
member's own answer from the previous round.  OTHER-CANDIDATES is the
rest of the prior round's candidate plist list, rendered with
`anvil-fusion--format-candidates-deduped' at FIDELITY.  CLAIMS-BLOCK is
the already-rendered verified-claims table string; nil/empty renders as
\"(なし)\".  Returns a Japanese prompt instructing the member to defend
or revise its answer and never preserve a refuted claim."
  (let ((others (if other-candidates
                    (anvil-fusion--format-candidates-deduped
                     other-candidates fidelity)
                  "(なし)"))
        (claims (if (and (stringp claims-block)
                         (not (string-empty-p claims-block)))
                    claims-block
                  "(なし)")))
    (format
     (concat
      "# 原問\n%s\n\n"
      "# あなたの前回の回答\n%s\n\n"
      "# 他の回答者の回答（要点）\n%s\n\n"
      "# 検証済み主張表\n%s\n\n"
      "# 指示\n"
      "他の回答と検証結果を踏まえ、自分の回答を弁護するか修正せよ。"
      "反証された主張 (REFUTED) を維持してはならない。"
      "改善した最終回答のみを出力せよ。")
     (or original-prompt "")
     (or prior-answer "")
     others
     claims)))

(provide 'anvil-fusion)
;;; anvil-fusion.el ends here
