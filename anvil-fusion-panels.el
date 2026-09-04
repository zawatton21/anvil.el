;;; anvil-fusion-panels.el --- named model panels for anvil-fusion -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton + Claude
;; Keywords: ai, orchestrator, fusion

;;; Commentary:
;;
;; Phase 2 of docs/design/01-fusion-harness.org: named panels.
;;
;; A *panel* is a named set of models that get fanned out together, plus
;; a judge that synthesizes their answers.  Two panels are first-class
;; equals (user decision 2026-06-15):
;;
;;   quality   — strongest available models (Opus 4.8 + GPT-5.5 +
;;               Gemini 3.1 Pro), judged by Opus 4.8.  US-vendor bound,
;;               max performance, does NOT touch the banned Fable 5.
;;   sovereign — local / self-hosted models only.  Zero content egress,
;;               survives any vendor/government cutoff, satisfies the
;;               "content を外部に出さない" policy by construction.
;;
;; Each panel carries an `egress' flag (`external' | `local-only').  A
;; `local-only' panel is validated to contain ONLY local providers (see
;; `anvil-fusion-local-providers') for every member AND the judge, so
;; sovereignty is enforced mechanically, not by convention.  Phase 3's
;; `anvil-fusion-ask' consults this flag to disable web/browser tools for
;; sovereign runs.
;;
;; Doc 61 §3 adds `opus-solo': a k=3 self-consistency panel using one
;; strong Claude Opus 4.8 model three times under different role lenses,
;; then judged by the same model.  `sonnet-solo' mirrors that structure
;; for a lower-cost sonnet-tier ceiling measurement.  This keeps the
;; panels operational even when cross-vendor auth or wiring is unavailable.
;;
;; This module is pure: it has no load-time dependency on
;; anvil-orchestrator.  `anvil-fusion-panel-tasks' produces orchestrator
;; task plists that Phase 3 feeds to `anvil-orchestrator-submit'.

;;; Code:

(require 'cl-lib)
(require 'anvil-fusion-lens)

(defgroup anvil-fusion nil
  "Fusion judge harness layered over anvil-orchestrator."
  :group 'tools
  :prefix "anvil-fusion-")

;;;; --- customization -------------------------------------------------------

(defcustom anvil-fusion-local-providers '(ollama)
  "Providers that run locally / self-hosted (no content egress).
A panel with `egress' `local-only' must use only these providers
for every member and its judge.  Extend this when you wire up
additional local runtimes."
  :type '(repeat symbol)
  :group 'anvil-fusion)

(defcustom anvil-fusion-panels
  '((quality
     (members . ((claude . "claude-opus-4-8")
                 (codex  . "gpt-5.5")
                 (gemini . "gemini-3.1-pro")))
     (judge   . (claude . "claude-opus-4-8"))
     (egress  . external))
    (sovereign
     (members . ((ollama . "llama3.1:8b")
                 (ollama . "llama3.2:3b")
                 (ollama . "gemma4:e4b")))
     (judge   . (ollama . "llama3.1:8b"))
     (egress  . local-only))
    (budget
     (members . ((gemini . "gemini-3.1-flash")
                 (ollama . "llama3.2:3b")
                 (codex  . "gpt-5.5-mini")))
     (judge   . (claude . "claude-opus-4-8"))
     (egress  . external))
    (diverse
     (members . ((codex) (claude)))
     (judge   . (codex))
     (egress  . external))
    (claude-pair
     (members . ((claude) (claude)))
     (judge   . (claude))
     (egress  . external))
    (opus-solo
     (members . ((claude . "claude-opus-4-8")
                 (claude . "claude-opus-4-8")
                 (claude . "claude-opus-4-8")))
     (judge   . (claude . "claude-opus-4-8"))
     (egress  . external)
     (lenses  . (correctness completeness skeptic)))
    (sonnet-solo
     (members . ((claude . "claude-sonnet-5")
                 (claude . "claude-sonnet-5")
                 (claude . "claude-sonnet-5")))
     (judge   . (claude . "claude-sonnet-5"))
     (egress  . external)
     (lenses  . (correctness completeness skeptic))))
  "Named fusion panels.
An alist NAME -> BODY where BODY is an alist with keys:

  members  list of (PROVIDER . MODEL) conses (>= 2; MODEL may be
           nil to use the provider default).  Duplicate providers
           with different models are allowed (e.g. several ollama
           models) — Phase 3 fans them out as separate tasks.
  judge    a single (PROVIDER . MODEL) cons used to synthesize.
  egress   `external' or `local-only'.  `local-only' is validated
           to contain only `anvil-fusion-local-providers'.
  lenses   optional list of role lenses applied by member position.

The model strings for non-local panels are sensible defaults;
tune them to whatever your account exposes.  The sovereign
defaults (llama3.1:8b + llama3.2:3b + gemma4:e4b, judged by
llama3.1:8b) are validated to run via Ollama on a GTX 1060 6GB /
62GB-RAM box (8b/3b fit VRAM; gemma4:e4b spills to CPU).  Swap
for whatever your local install pulls; see docs/design/01 §9.

`opus-solo' is the k-member single-model self-consistency panel
(Doc 61 Phase 7): one strong model asked k=3 times under
different role lenses, judged by the same model -- no
cross-vendor wiring dependency; pair with the Phase 6d `:verify'
flag for the verifier-grounded judge.  `sonnet-solo' is the
sonnet-tier twin of `opus-solo' for cost-sensitive
self-consistency and for measuring harness uplift on a mid-tier
model; pair with `:verify'."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'anvil-fusion)

(defcustom anvil-fusion-default-panel 'claude-pair
  "Panel used by `anvil-fusion-ask' when none is specified.
Defaults to `claude-pair' — a reliable cloud panel (claude ×2 +
claude judge) that avoids the flaky codex/gemini members of
`quality'.  The local-only `sovereign' mechanism is *retained* and
selectable per call via `:panel \\='sovereign' for zero-egress work,
but small local models proved quality-limited on factual / domain
questions (live: a sovereign run discarded the correct answer for a
hallucinated one; see eval/results-longrun-weakdistill-2026-06-16.org),
so it is no longer the silent default.  Choose `quality' explicitly
for maximum model diversity when external egress and codex/gemini
availability are acceptable."
  :type 'symbol
  :group 'anvil-fusion)

(defconst anvil-fusion--member-extras-keys
  '(:permission-mode :allowed-tools :sandbox :no-worktree :timeout-sec)
  "Whitelisted member task extras merged by `anvil-fusion-panel-tasks'.
Unknown keys in MEMBER-EXTRAS are dropped.")

;;;; --- accessors -----------------------------------------------------------

(defun anvil-fusion-panel-get (name)
  "Return the BODY alist for panel NAME, or signal `user-error'."
  (or (cdr (assq name anvil-fusion-panels))
      (user-error "anvil-fusion: unknown panel %S (known: %s)"
                  name (mapconcat (lambda (p) (symbol-name (car p)))
                                  anvil-fusion-panels ", "))))

(defun anvil-fusion-panel-members (body)
  "Return the (PROVIDER . MODEL) member list from panel BODY."
  (cdr (assq 'members body)))

(defun anvil-fusion-panel-judge (body)
  "Return the (PROVIDER . MODEL) judge cons from panel BODY."
  (cdr (assq 'judge body)))

(defun anvil-fusion-panel-egress (body)
  "Return the egress symbol (`external' / `local-only') from BODY."
  (cdr (assq 'egress body)))

(defun anvil-fusion-provider-local-p (provider)
  "Return non-nil when PROVIDER is a local / self-hosted provider."
  (and (memq provider anvil-fusion-local-providers) t))

(defun anvil-fusion-panel-local-only-p (name)
  "Return non-nil when panel NAME has `local-only' egress."
  (eq (anvil-fusion-panel-egress (anvil-fusion-panel-get name)) 'local-only))

;;;; --- validation (sovereignty enforcement) --------------------------------

(defun anvil-fusion--valid-spec-p (spec)
  "Return non-nil when SPEC is a (SYMBOL . STRING-or-nil) cons."
  (and (consp spec)
       (symbolp (car spec))
       (car spec)                       ; non-nil provider symbol
       (or (null (cdr spec)) (stringp (cdr spec)))))

(defun anvil-fusion-panel-validate (name)
  "Validate panel NAME, signalling `user-error' on any problem.
Checks member/judge shape, the egress value, the >= 2 member
rule, and — for `local-only' panels — that every member provider
AND the judge provider is in `anvil-fusion-local-providers'.
Returns t when the panel is well-formed."
  (let* ((body    (anvil-fusion-panel-get name))
         (members (anvil-fusion-panel-members body))
         (judge   (anvil-fusion-panel-judge body))
         (egress  (anvil-fusion-panel-egress body)))
    (unless (and (listp members) (>= (length members) 2))
      (user-error "anvil-fusion[%s]: need >= 2 members (got %d)"
                  name (length members)))
    (dolist (m members)
      (unless (anvil-fusion--valid-spec-p m)
        (user-error "anvil-fusion[%s]: bad member spec %S" name m)))
    (unless (anvil-fusion--valid-spec-p judge)
      (user-error "anvil-fusion[%s]: bad judge spec %S" name judge))
    (unless (memq egress '(external local-only))
      (user-error "anvil-fusion[%s]: egress must be external/local-only (got %S)"
                  name egress))
    (when (eq egress 'local-only)
      (dolist (m members)
        (unless (anvil-fusion-provider-local-p (car m))
          (user-error
           "anvil-fusion[%s]: local-only panel has non-local member %S"
           name (car m))))
      (unless (anvil-fusion-provider-local-p (car judge))
        (user-error
         "anvil-fusion[%s]: local-only panel has non-local judge %S"
         name (car judge))))
    t))

;;;; --- bridge to the orchestrator (Phase 3 consumes these) -----------------

(defun anvil-fusion--member-extras-plist (member-extras)
  "Return the whitelisted subset of MEMBER-EXTRAS as a plist.
Unknown keys are ignored.  Nil returns nil."
  (let (out)
    (dolist (key anvil-fusion--member-extras-keys out)
      (when (plist-member member-extras key)
        (setq out (plist-put out key (plist-get member-extras key)))))))

(defun anvil-fusion-panel-tasks (body prompt &optional lenses cwd member-extras)
  "Expand panel BODY into orchestrator task plists for PROMPT.
Returns a list of (:provider P :prompt PROMPT [:model M] [:cwd C])
plists, one per member, preserving duplicate providers.  Optional
LENSES is a list of role lenses applied by position: the Nth
member's prompt is prefixed with the Nth lens (see
`anvil-fusion-apply-lens'; nil / missing entries leave the prompt
unchanged).  Optional CWD sets each task's working directory — use
a neutral dir (e.g. \"/tmp\") to keep a nested Claude Code member
from inheriting the project's hooks / CLAUDE.md.  Pure — no
orchestrator call; Phase 3 hands the result to
`anvil-orchestrator-submit'.  Optional MEMBER-EXTRAS is a plist:
only `anvil-fusion--member-extras-keys' are merged; unknown keys are
dropped."
  (let ((i -1)
        (extras (anvil-fusion--member-extras-plist member-extras)))
    (mapcar (lambda (m)
              (setq i (1+ i))
              (let ((p (anvil-fusion-apply-lens prompt (nth i lenses))))
                (append (list :provider (car m)
                              :prompt p
                              :name (format "fusion-member-%d-%s" i (car m)))
                        (and (cdr m) (list :model (cdr m)))
                        (and cwd (list :cwd cwd))
                        extras)))
            (anvil-fusion-panel-members body))))

(provide 'anvil-fusion-panels)
;;; anvil-fusion-panels.el ends here
