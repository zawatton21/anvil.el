;;; anvil-fusion-longrun.el --- long-horizon, context-isolated task execution -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion, long-horizon

;;; Commentary:
;;
;; Phase 1 (spike) of docs/design/02-longrun.org.
;;
;; A *quest* runs a long (multi-step) task while carrying a bounded
;; distilled STATE DIGEST instead of the full step history.  Step N sees
;; only (goal + digest_{N-1}), so the per-step context is O(1) no matter
;; how long the task runs -- this is what structurally avoids the
;; "98.5% of tokens is history re-read" overhead documented in
;; capture/yt/yt_20260527_claude_token_10habits.org.
;;
;; Each step is two orchestrator tasks: (1) advance the work one step,
;; (2) distill the new output into an updated, size-capped digest plus a
;; STATUS: DONE/CONTINUE verdict.  Steps run in an isolated subprocess
;; (`:cwd' defaults to /tmp so a nested Claude Code member does not
;; inherit the project's hooks / CLAUDE.md).
;;
;; The caller only invokes `anvil-fusion-longrun-run' and reads the final
;; digest; intermediate step outputs are consumed into the digest and
;; never returned -- the parent session's context stays clean.
;;
;; `step-fn' / `distill-fn' are injectable so ERT exercises the loop and
;; the bounded-context invariant without touching the orchestrator.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'anvil-fusion)                   ; anvil-fusion--batch-first-task-id
(require 'anvil-fusion-panels)            ; member extras whitelist helper

(declare-function anvil-orchestrator-submit "anvil-orchestrator" (tasks))
(declare-function anvil-orchestrator-collect "anvil-orchestrator" (batch-id &rest _))
(declare-function anvil-orchestrator-extract-result "anvil-orchestrator" (task-id &optional full))
(declare-function anvil-fusion-ask "anvil-fusion-ask" (prompt &rest args))
(declare-function anvil-fusion--agentic-extras "anvil-fusion-ask" (agentic cwd))
(declare-function anvil-fusion-verify-extract-claims "anvil-fusion-verify"
                  (&rest args))
(declare-function anvil-fusion-verify-claims "anvil-fusion-verify"
                  (&rest args))
;; Special var from anvil-orchestrator; forward-declare so a hermetic step can
;; dynamically let-bind it to inject a read-only --mcp-config for that step.
(defvar anvil-orchestrator-manifest-profile)
(defvar anvil-fusion-longrun--verify-max-wait-sec nil)

;;;; --- customization -------------------------------------------------------

(defgroup anvil-fusion-longrun nil
  "Long-horizon, context-isolated task execution for anvil-fusion."
  :group 'anvil-fusion
  :prefix "anvil-fusion-longrun-")

(defcustom anvil-fusion-longrun-default-provider 'codex
  "Default orchestrator provider used for each quest step."
  :type 'symbol :group 'anvil-fusion-longrun)

(defcustom anvil-fusion-longrun-max-steps 8
  "Hard cap on quest steps (budget / runaway guard)."
  :type 'integer :group 'anvil-fusion-longrun)

(defcustom anvil-fusion-longrun-digest-max-chars 4000
  "Upper bound on the carried state digest, in characters.
This is the knob that keeps per-step context O(1) regardless of
how long the quest runs."
  :type 'integer :group 'anvil-fusion-longrun)

(defcustom anvil-fusion-longrun-max-panel-steps 3
  "Maximum panel-executed steps per quest.
This is the v1 budget guard for Doc 61 Phase 8b: once this many
steps have run through `anvil-fusion-ask', further panel-eligible
steps fall back to the normal single-provider step function with one
`message'.  Threading a richer Doc 47-style :budget through
`anvil-fusion-ask' is deferred because that API does not expose
`:budget' today."
  :type 'integer :group 'anvil-fusion-longrun)

(defcustom anvil-fusion-longrun-default-cwd "/tmp"
  "Default working directory for quest step tasks.
A neutral dir (e.g. \"/tmp\") keeps a nested Claude Code step from
inheriting the project's hooks / CLAUDE.md."
  :type 'string :group 'anvil-fusion-longrun)

(defcustom anvil-fusion-longrun-converge-threshold 0.9
  "Jaccard-similarity threshold (0.0-1.0) for digest stagnation.
When a step's new digest is at least this similar to the previous
digest, the step counts as stagnant (see
`anvil-fusion-longrun-converge-patience').  Reuses the Fusion
shingle/Jaccard similarity (`anvil-fusion--jaccard')."
  :type 'number :group 'anvil-fusion-longrun)

(defcustom anvil-fusion-longrun-converge-patience 2
  "Consecutive stagnant steps that end a quest early (`converged').
This stops a quest once the distilled state stops meaningfully
changing, instead of burning the rest of the step budget.  Set to 0
to disable convergence detection and rely on STATUS: DONE / the
step budget alone."
  :type 'integer :group 'anvil-fusion-longrun)

(defcustom anvil-fusion-longrun-disclosure-tools
  '("mcp__emacs-eval__file-outline"
    "mcp__emacs-eval__file-read-snippet"
    "mcp__emacs-eval__file-read"
    "mcp__emacs-eval__org-index-search"
    "mcp__emacs-eval__org-index-index"
    "mcp__emacs-eval__defs-search"
    "mcp__emacs-eval__defs-index"
    "mcp__emacs-eval__defs-signature"
    "mcp__emacs-eval__disclosure-help")
  "Read-only progressive-disclosure MCP tools a hermetic step may call.
Passed as a claude `--allowedTools' allow-list so a hermetic step
can ONLY pull information through anvil's 3-layer disclosure path
(outline -> snippet -> read) -- no writes, no shell, no unbounded
native Read.  Note: the FULLY minimal-schema variant additionally
requires a read-only `anvil-orchestrator-manifest-profile' so the
child sees only these tool schemas; the allow-list alone already
makes the step read-only."
  :type '(repeat string) :group 'anvil-fusion-longrun)

(defcustom anvil-fusion-longrun-hermetic-instruction
  "このステップでは書き込み・シェル・ネイティブ Read は使えません。ファイルや知識ベースの内容が必要なときは、読み取り専用ツール file-outline → file-read-snippet → file-read (org は org-index-search、定義は defs-search) を使い、必要な箇所だけを段階的に取得してください。全文の丸読みは避けること。"
  "Instruction appended to a hermetic step prompt.
Nudges the model toward the 3-layer progressive-disclosure path
since native Read / shell are not in the allow-list."
  :type 'string :group 'anvil-fusion-longrun)

(defcustom anvil-fusion-longrun-hermetic-manifest-profile 'ultra
  "anvil-manifest profile injected (via --mcp-config) for a hermetic step.
A hermetic step runs at /tmp and so does not load the project's
.mcp.json; this read-only profile is injected so the step still sees
the disclosure tools regardless of cwd.  `ultra' (read-only: file-outline
/ file-read / org-index-search / org-read-* / git read / ...) is the
tightest profile that carries the disclosure path.  Requires
`anvil-orchestrator-manifest-stdio-command' to be set and anvil-manifest
enabled (server-id aliases registered).  nil disables injection (the
step then needs the tools available some other way)."
  :type 'symbol :group 'anvil-fusion-longrun)

(defcustom anvil-fusion-longrun-hermetic-disallowed-tools
  "Bash,Write,Edit,NotebookEdit,Read,Glob,Grep,WebFetch,WebSearch"
  "Native tools denied for a hermetic step (claude `--disallowedTools').
A hermetic step injects MCP tools with a read-only profile and runs
with `--dangerously-skip-permissions' -- the only way a
non-interactive `claude --print' child will use an injected
--mcp-config server (it cannot answer the interactive trust prompt).
To keep the step read-only despite skip-permissions, native
write/shell/file tools are denied here, forcing the step to pull
context ONLY through the read-only disclosure MCP tools."
  :type 'string :group 'anvil-fusion-longrun)

(defcustom anvil-fusion-longrun-step-template
  "あなたは長期タスクを多段で遂行するエージェントの1ステップを担当します。

# 最終ゴール
%s

# これまでの蒸留された状態 (= ワーキングメモリ。全履歴ではない)
%s

# このステップの指示
上の「状態」だけを前提に、ゴール達成へ向けて次の1ステップ分だけ前進させてください。
状態に既にある事項の単純な繰り返しは避け、新たな前進(調査結果・中間成果・決定)を
簡潔に出力してください。
(現在 %d / 最大 %d ステップ目)"
  "Template for one quest step prompt.
Format args: GOAL, DIGEST, STEP-N, MAX-STEPS."
  :type 'string :group 'anvil-fusion-longrun)

(defcustom anvil-fusion-longrun-distill-template
  "あなたは長期タスクのワーキングメモリ管理者です。

# 最終ゴール
%s

# 直前の状態
%s

# 今ステップの新しい出力
%s

# 指示
「直前の状態」に「今ステップの新しい出力」を統合し、ゴール達成に必要な情報だけを
残した更新後の状態を出力してください。冗長・重複・脱線は削り、%d 文字以内に
圧縮すること。次の一手が、候補間で判断が割れうる難所（設計判断・相反する情報の
裁定・重要な数値判断）だと考える場合のみ、最後の STATUS 行の直前の行に
NEXT-HARD: yes と書いてください。そうでなければその行は省略するか
NEXT-HARD: no と書いてください。
出力の最後の行に、ゴールが完全に達成されたなら STATUS: DONE、まだ続けるべきなら
STATUS: CONTINUE と必ず書いてください。"
  "Template for the per-step distillation prompt.
Format args: GOAL, PREV-DIGEST, STEP-OUTPUT, MAX-CHARS."
  :type 'string :group 'anvil-fusion-longrun)

;;;; --- pure prompt builders ------------------------------------------------

(defun anvil-fusion-longrun-build-step-prompt (goal digest step-n max-steps)
  "Build the step-N prompt from GOAL and DIGEST (the carried state).
DIGEST may be nil for the first step.  Pure."
  (format anvil-fusion-longrun-step-template
          goal (or digest "(まだ無し)") step-n max-steps))

(defun anvil-fusion-longrun-build-distill-prompt (goal prev-digest output max-chars)
  "Build the distillation prompt folding OUTPUT into PREV-DIGEST.
PREV-DIGEST may be nil.  MAX-CHARS bounds the requested digest.  Pure."
  (format anvil-fusion-longrun-distill-template
          goal (or prev-digest "(まだ無し)") output max-chars))

(defun anvil-fusion-longrun--parse-distill (text max-chars)
  "Parse distiller TEXT into (:digest STR :done BOOL :next-hard BOOL).
DONE-P is non-nil only when the *trailing* STATUS marker line is
STATUS: DONE.  NEXT-HARD is non-nil only when the line immediately
before that trailing STATUS line is a trailing NEXT-HARD: yes marker.
Both checks are anchored to the end of TEXT, so STATUS / NEXT-HARD
text that merely appears inside the digest body does not alter the
control flow.  The trailing STATUS and optional NEXT-HARD lines are
stripped, then the digest is trimmed and clamped to MAX-CHARS.  Pure."
  (let* ((case-fold-search t)
         (done   (and (string-match-p "STATUS:[ \t]*DONE[ \t\r\n]*\\'" text) t))
         (body   (replace-regexp-in-string
                  "[ \t\r\n]*STATUS:[ \t]*\\(?:DONE\\|CONTINUE\\)[ \t]*\\'" "" text))
         (next-hard
          (and (string-match-p "NEXT-HARD:[ \t]*yes[ \t\r\n]*\\'" body) t))
         (digest (replace-regexp-in-string
                  "[ \t\r\n]*NEXT-HARD:[ \t]*\\(?:yes\\|no\\)[ \t]*\\'" "" body)))
    (setq digest (string-trim digest))
    (when (> (length digest) max-chars)
      (setq digest (substring digest 0 max-chars)))
    (list :digest digest :done done :next-hard next-hard)))

;;;; --- convergence (digest stagnation) -------------------------------------

(defun anvil-fusion-longrun--streak (streak prev digest threshold patience)
  "Return the updated stagnation STREAK after comparing PREV and DIGEST.
A step is stagnant when PATIENCE > 0, PREV is a non-empty string,
and the Jaccard similarity of PREV and DIGEST is >= THRESHOLD.
Stagnant steps increment STREAK; any change resets it to 0.  Pure."
  (if (and (integerp patience) (> patience 0)
           (stringp prev) (> (length prev) 0)
           (>= (anvil-fusion--jaccard prev (or digest "")) threshold))
      (1+ streak)
    0))

(defun anvil-fusion-longrun--converged-p (streak patience)
  "Return non-nil when STREAK has reached the PATIENCE budget (>0)."
  (and (integerp patience) (> patience 0) (>= streak patience)))

;;;; --- hermetic step config ------------------------------------------------

(defun anvil-fusion-longrun-disclosure-tools-string ()
  "Return `anvil-fusion-longrun-disclosure-tools' as a comma-joined string."
  (mapconcat #'identity anvil-fusion-longrun-disclosure-tools ","))

(defun anvil-fusion-longrun--allowed-tools-plist (allowed-tools)
  "Return (:allowed-tools STR) when ALLOWED-TOOLS is non-empty, else nil.
ALLOWED-TOOLS may be a list of tool names or a comma-joined string.  Pure."
  (let ((s (cond ((null allowed-tools) nil)
                 ((listp allowed-tools)
                  (mapconcat #'identity allowed-tools ","))
                 (t allowed-tools))))
    (and (stringp s) (not (string-empty-p s)) (list :allowed-tools s))))

(defun anvil-fusion-longrun--apply-suffix (prompt suffix)
  "Return PROMPT with SUFFIX appended (blank line) when SUFFIX is non-empty.  Pure."
  (if (and (stringp suffix) (not (string-empty-p suffix)))
      (concat prompt "\n\n" suffix)
    prompt))

(defun anvil-fusion-longrun--apply-suffixes (prompt &rest suffixes)
  "Return PROMPT with each non-empty string in SUFFIXES appended in order.  Pure."
  (dolist (suffix suffixes prompt)
    (setq prompt (anvil-fusion-longrun--apply-suffix prompt suffix))))

(defun anvil-fusion-longrun--resolve-allowed-tools (step-allowed-tools hermetic profile)
  "Return the step allow-list string for hermetic execution, or nil.
Base is STEP-ALLOWED-TOOLS, else (when HERMETIC) the disclosure set.
When PROFILE is non-nil the manifest injects the server as
`emacs-eval-PROFILE', so the tool names are `mcp__emacs-eval-PROFILE__*';
this rewrites the `mcp__emacs-eval__' prefix to match.  Pure."
  (let ((base (or step-allowed-tools
                  (and hermetic (anvil-fusion-longrun-disclosure-tools-string)))))
    (when (and base (listp base))
      (setq base (mapconcat #'identity base ",")))
    (if (and base profile)
        (replace-regexp-in-string
         "mcp__emacs-eval__" (format "mcp__emacs-eval-%s__" profile) base t t)
      base)))

(defun anvil-fusion-longrun--hermetic-suffix (allowed-tools-string)
  "Return the hermetic step instruction naming the exact ALLOWED-TOOLS-STRING.
The model must call the tools by their exact (profile-prefixed) names;
listing them here avoids it guessing the wrong `mcp__emacs-eval__' prefix."
  (concat anvil-fusion-longrun-hermetic-instruction
          (and allowed-tools-string
               (not (string-empty-p allowed-tools-string))
               (concat "\n\n利用可能な読み取り専用ツール(必ずこの正確な名前で呼ぶこと): "
                       allowed-tools-string))))

;;;; --- live runner (one isolated orchestrator task per call) ---------------

(defun anvil-fusion-longrun--run-one (provider prompt name &optional model cwd
                                              timeout-sec max-wait-sec allowed-tools
                                              manifest-profile member-extras)
  "Submit ONE isolated orchestrator task and return its full result text.
ALLOWED-TOOLS (list or comma-joined string) restricts the claude
member to that tool allow-list (hermetic).  Mirrors the batch path
used by `anvil-fusion-ask' but for a single task."
  (require 'anvil-orchestrator)
  (let* ((task  (append (list :provider provider :prompt prompt :name name
                              ;; A quest advances in ONE fixed cwd: each step's
                              ;; output accumulates there and the digest carries
                              ;; state forward.  Auto-worktree would relocate the
                              ;; child into a per-step isolated tree (breaking
                              ;; continuity) and scope file access to that tree,
                              ;; denying reads of absolute paths in the main repo.
                              :no-worktree t)
                        (and model (list :model model))
                        (and cwd (list :cwd cwd))
                        (and timeout-sec (list :timeout-sec timeout-sec))
                        (anvil-fusion-longrun--allowed-tools-plist allowed-tools)
                        (anvil-fusion--member-extras-plist member-extras)
                        (and manifest-profile
                             (list :manifest-profile manifest-profile
                                   :skip-permissions t
                                   :disallowed-tools
                                   anvil-fusion-longrun-hermetic-disallowed-tools))))
         (batch (anvil-orchestrator-submit (list task))))
    (anvil-orchestrator-collect batch :wait t :max-wait-sec (or max-wait-sec 1800))
    (let* ((id     (anvil-fusion--batch-first-task-id batch))
           (result (anvil-orchestrator-extract-result id t)))
      (or (plist-get result :summary) ""))))

(defun anvil-fusion-longrun--default-step-fn (provider model cwd timeout-sec max-wait-sec
                                                      &optional allowed-tools suffix
                                                      manifest-profile member-extras)
  "Return a step-fn closure that runs one isolated orchestrator task.
ALLOWED-TOOLS restricts the step to a tool allow-list; SUFFIX is
appended to each step prompt (hermetic instruction); MANIFEST-PROFILE
injects a read-only --mcp-config for the step."
  (lambda (prompt step-n)
    (anvil-fusion-longrun--run-one
     provider (anvil-fusion-longrun--apply-suffix prompt suffix)
     (format "longrun-step-%d" step-n)
     model cwd timeout-sec max-wait-sec allowed-tools manifest-profile
     member-extras)))

(defun anvil-fusion-longrun--default-distill-fn
    (provider model cwd timeout-sec max-wait-sec &optional member-extras)
  "Return a distill-fn closure that runs one isolated orchestrator task."
  (lambda (prompt step-n)
    (anvil-fusion-longrun--run-one
     provider prompt (format "longrun-distill-%d" step-n)
     model cwd timeout-sec max-wait-sec nil nil member-extras)))

(defun anvil-fusion-longrun--panel-step-p (step-panel step-panel-mode next-hard panel-budget-left)
  "Return non-nil when the current step should execute as a panel step.
STEP-PANEL names the panel; STEP-PANEL-MODE is `hard-only' or
`always'; NEXT-HARD is the previous distiller's hardness signal; and
PANEL-BUDGET-LEFT is the remaining panel-step budget.  Pure."
  (and step-panel
       (> (or panel-budget-left 0) 0)
       (pcase (or step-panel-mode 'hard-only)
         ('always t)
         (_ next-hard))))

(defun anvil-fusion-longrun--default-verify-fn (question digest)
  "Best-effort default verifier for a step DIGEST under GOAL QUESTION.
Returns the annotated claim list, or nil when extraction /
verification fails for any reason."
  (condition-case err
      (progn
        (require 'anvil-fusion-verify)
        (let* ((claims
                (anvil-fusion-verify-extract-claims
                 question
                 (list (list :name "digest" :provider 'digest
                             :status 'done :summary digest))
                 :max-wait-sec (or anvil-fusion-longrun--verify-max-wait-sec 1800))))
          (and claims
               (anvil-fusion-verify-claims
                claims :question question
                :max-wait-sec (or anvil-fusion-longrun--verify-max-wait-sec 1800)))))
    (error
     (message "anvil-fusion-longrun: step verification skipped: %s"
              (error-message-string err))
     nil)))

(defun anvil-fusion-longrun--refuted-claims (claims)
  "Return only the `refuted' entries from annotated CLAIMS.  Pure."
  (seq-filter (lambda (claim) (eq (plist-get claim :verdict) 'refuted)) claims))

(defun anvil-fusion-longrun--gate-result (verify-fn question digest)
  "Run VERIFY-FN on DIGEST for QUESTION and return gate bookkeeping.
The result plist contains :claims, :refuted, and :pass."
  (let* ((claims
          (condition-case err
              (or (funcall verify-fn question digest) nil)
            (error
             (message "anvil-fusion-longrun: step verification skipped: %s"
                      (error-message-string err))
             nil)))
         (refuted (anvil-fusion-longrun--refuted-claims claims)))
    (list :claims claims
          :refuted refuted
          :pass (null refuted))))

(defun anvil-fusion-longrun--retry-feedback-block (refuted-claims)
  "Render the Japanese retry block for REFUTED-CLAIMS.  Pure."
  (concat
   "「# 前回試行への反証」\n"
   (mapconcat
    (lambda (claim)
      (format "- %s\n  根拠: %s"
              (or (plist-get claim :claim) "")
              (or (plist-get claim :evidence) "")))
    refuted-claims
    "\n")
   "\n反証された主張を修正した上で、同じ一手をやり直してください。"))

;;;; --- the loop ------------------------------------------------------------

(cl-defun anvil-fusion-longrun-run
    (goal &key provider model distill-provider distill-model
          max-steps digest-max-chars cwd timeout-sec (max-wait-sec 1800)
          step-fn distill-fn (keep-trace t)
          id resume-step resume-digest on-step on-finish
          hermetic step-allowed-tools step-manifest-profile
          step-panel (step-panel-mode 'hard-only) panel-verify panel-step-fn
          verify-steps verify-fn member-extras agentic)
  "Execute GOAL as a long-horizon quest carrying a bounded state digest.

Each step advances the work (STEP-FN) then distills the new output
into an updated, size-capped digest (DISTILL-FN).  Step N sees only
GOAL plus the digest from step N-1, so per-step context is O(1).

PROVIDER / MODEL pick the step model; DISTILL-PROVIDER /
DISTILL-MODEL default to the step model.  MAX-STEPS caps the loop;
DIGEST-MAX-CHARS bounds the carried state.  CWD defaults to
`anvil-fusion-longrun-default-cwd' (hermetic).  STEP-FN / DISTILL-FN
override the orchestrator-backed defaults -- each is called as
(FN PROMPT STEP-N) and must return the result text (used by ERT to
run the loop without the orchestrator).

ID tags the quest for persistence.  RESUME-STEP / RESUME-DIGEST seed
the loop to continue an interrupted quest from a saved checkpoint.
ON-STEP is called after each step as (ON-STEP ID STEP DIGEST DONE
META); ON-FINISH as (ON-FINISH ID STEP DIGEST STOPPED) -- the store
module (Phase 2) wires these to SQLite checkpoints.

When STEP-PANEL is non-nil, a step may run through
`anvil-fusion-ask' instead of the single-provider STEP-FN.
STEP-PANEL-MODE controls the gate: `always' panels every eligible
step until the panel budget is exhausted; `hard-only' (the default)
uses a panel only when the PREVIOUS distill emitted NEXT-HARD: yes,
so step 1 is never a panel step in `hard-only' mode.  PANEL-VERIFY is
forwarded as `:verify' to `anvil-fusion-ask'.  PANEL-STEP-FN is a
test seam that overrides the real panel call.

MEMBER-EXTRAS is merged into plain single-provider step tasks after
the same whitelist filtering used by `anvil-fusion-panel-tasks';
unknown keys are dropped.  AGENTIC mirrors `anvil-fusion-ask': with a
scratch CWD under `temporary-file-directory' (or AGENTIC = `force'
with non-nil CWD), Claude steps get `bypassPermissions'; otherwise
the preset downgrades to `:allowed-tools \"Bash\"'.  This is the
supported way to run a coding quest that must execute build/tests.
`bypassPermissions' grants the child broad local command/file access
inside the chosen CWD, so keep that CWD scratch/sandboxed.

Returns a plist: :id :goal :answer :digest :steps :stopped :provider
:digest-chars :panel-steps :trace.  Only :answer / :digest are meant to cross
back into the caller's context; intermediate step OUTPUTS are
consumed into the digest and never returned (the parent context
stays clean).  :trace holds per-step metadata only (output-chars /
digest-chars / digest-head), not the full outputs."
  (when agentic
    (require 'anvil-fusion-ask))
  (let* ((prov  (or provider anvil-fusion-longrun-default-provider))
         (dprov (or distill-provider prov))
         (dmod  (or distill-model model))
         (maxn  (or max-steps anvil-fusion-longrun-max-steps))
         (maxc  (or digest-max-chars anvil-fusion-longrun-digest-max-chars))
         (cwd   (or cwd anvil-fusion-longrun-default-cwd))
         (thr   anvil-fusion-longrun-converge-threshold)
         (pat   anvil-fusion-longrun-converge-patience)
         (smp   (or step-manifest-profile
                    (and hermetic anvil-fusion-longrun-hermetic-manifest-profile)))
         (sat   (anvil-fusion-longrun--resolve-allowed-tools
                 step-allowed-tools hermetic smp))
         (suf   (and hermetic (anvil-fusion-longrun--hermetic-suffix sat)))
         (agentic-extras (and agentic (anvil-fusion--agentic-extras agentic cwd)))
         (member-extras
          (append (anvil-fusion--member-extras-plist member-extras)
                  agentic-extras))
         (sfn   (or step-fn
                    (anvil-fusion-longrun--default-step-fn
                     prov model cwd timeout-sec max-wait-sec sat suf smp
                     member-extras)))
         (dfn   (or distill-fn
                    (anvil-fusion-longrun--default-distill-fn
                     dprov dmod cwd timeout-sec max-wait-sec
                     member-extras)))
         (digest resume-digest)
         (trace  nil)
         (step   (or resume-step 0))
         (next-hard nil)
         (panel-budget-left anvil-fusion-longrun-max-panel-steps)
         (panel-steps 0)
         (gate-failures 0)
         (panel-budget-noted nil)
         (streak 0)
         (reason nil)
         (done   nil)
         (vfn    (and verify-steps
                      (or verify-fn #'anvil-fusion-longrun--default-verify-fn))))
    (while (and (< step maxn) (not done))
      (setq step (1+ step))
      (let* ((prev    digest)
             (base-sprompt (anvil-fusion-longrun-build-step-prompt goal digest step maxn))
             (want-panel
              (and step-panel
                   (pcase (or step-panel-mode 'hard-only)
                     ('always t)
                     (_ next-hard))))
             (panelp  (anvil-fusion-longrun--panel-step-p
                       step-panel step-panel-mode next-hard panel-budget-left))
             (run-attempt
              (lambda (retry-feedback consume-panel-budget)
                (let* ((sprompt
                        (anvil-fusion-longrun--apply-suffixes
                         base-sprompt suf retry-feedback))
                       (output
                        (if panelp
                            (progn
                              (require 'anvil-fusion-ask)
                              (when consume-panel-budget
                                (cl-decf panel-budget-left)
                                (cl-incf panel-steps))
                              (if panel-step-fn
                                  (funcall panel-step-fn sprompt step)
                                (plist-get
                                 (anvil-fusion-ask
                                  sprompt :panel step-panel :verify panel-verify
                                  :member-extras member-extras
                                  :agentic agentic
                                  :cwd cwd :timeout-sec timeout-sec
                                  :max-wait-sec max-wait-sec)
                                 :answer)))
                          (progn
                            (when (and consume-panel-budget
                                       want-panel
                                       (<= panel-budget-left 0)
                                       (not panel-budget-noted))
                              (setq panel-budget-noted t)
                              (message "anvil-fusion-longrun: panel-step budget exhausted; falling back to single-provider steps"))
                            (funcall sfn sprompt step))))
                       (dprompt (anvil-fusion-longrun-build-distill-prompt
                                 goal digest output maxc))
                       (draw    (funcall dfn dprompt step))
                       (parsed  (anvil-fusion-longrun--parse-distill draw maxc)))
                  (list :output output :parsed parsed)))))
        (let* ((attempt (funcall run-attempt nil t))
               (parsed  (plist-get attempt :parsed))
               (output  (plist-get attempt :output))
               (gate-state nil)
               (refuted-texts nil))
          (when vfn
            (let* ((gate-1 (anvil-fusion-longrun--gate-result
                            vfn goal (plist-get parsed :digest))))
              (if (plist-get gate-1 :pass)
                  (setq gate-state 'pass)
                (let* ((retry-feedback
                        (anvil-fusion-longrun--retry-feedback-block
                         (plist-get gate-1 :refuted)))
                       (retry-attempt (funcall run-attempt retry-feedback nil))
                       (retry-parsed (plist-get retry-attempt :parsed))
                       (gate-2 (anvil-fusion-longrun--gate-result
                                vfn goal (plist-get retry-parsed :digest))))
                  (setq parsed retry-parsed)
                  (setq output (plist-get retry-attempt :output))
                  (if (plist-get gate-2 :pass)
                      (setq gate-state 'retried-pass)
                    (setq gate-state 'failed)
                    (setq refuted-texts
                          (mapcar (lambda (claim) (plist-get claim :claim))
                                  (plist-get gate-2 :refuted)))
                    (cl-incf gate-failures)
                    (message "anvil-fusion-longrun: step %d verify gate failed after retry"
                             step))))))
          (setq digest (plist-get parsed :digest))
          (setq next-hard (plist-get parsed :next-hard))
          (setq streak (anvil-fusion-longrun--streak streak prev digest thr pat))
          (cond ((plist-get parsed :done) (setq done t reason 'done))
                ((anvil-fusion-longrun--converged-p streak pat)
                 (setq done t reason 'converged)))
          (let ((meta (append
                       (list :step step
                             :panelp panelp
                             :output-chars (length (or output ""))
                             :digest-chars (length (or digest ""))
                             :digest-head (let ((d (or digest "")))
                                            (substring d 0 (min 80 (length d))))
                             :done done)
                       (and vfn (list :gate gate-state))
                       (and (eq gate-state 'failed)
                            (list :refuted refuted-texts)))))
            (when keep-trace (push meta trace))
            (when on-step (funcall on-step id step digest done meta))))))
    (let ((stopped (or reason 'budget)))
      (when on-finish (funcall on-finish id step digest stopped))
      (list :id id
            :goal goal
            :answer digest
            :digest digest
            :steps step
            :stopped stopped
            :provider prov
            :digest-chars (length (or digest ""))
            :gate-failures gate-failures
            :panel-steps panel-steps
            :trace (nreverse trace)))))

(provide 'anvil-fusion-longrun)
;;; anvil-fusion-longrun.el ends here
