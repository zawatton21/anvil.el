;;; anvil-orchestrator-presets.el --- Named consensus provider presets  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 23 Phase 1 — named combinations of providers + judge defaults +
;; per-task knobs for `anvil-orchestrator-submit-consensus'.
;;
;; Callers trade a repeated list + keyword dance like
;;
;;     (anvil-orchestrator-submit-consensus
;;      :prompt  "…"
;;      :providers '(claude codex)
;;      :timeout-sec 120)
;;
;; for a named reference:
;;
;;     (anvil-orchestrator-submit-consensus :prompt "…" :preset 'fast)
;;
;; Built-in presets (overridable via
;; `anvil-orchestrator-consensus-preset-defaults'):
;;
;;   | Name     | Providers                          | Judge  | Threshold |
;;   |----------+------------------------------------+--------+-----------|
;;   | fast     | claude, codex                      | nil    | 0.5       |
;;   | cheap    | codex, ollama                      | nil    | 0.4       |
;;   | broad    | claude, codex, ollama, gemini      | claude | 0.4       |
;;   | rigorous | claude, codex, ollama, gemini      | claude | 0.7       |
;;
;; Phase 1 is in-process only: preset state lives in a module-local
;; hash table seeded from the defcustom on first access.  Phase 2 will
;; persist via `anvil-state' and add MCP tools.  Phase 3 wires an
;; auto-judge opt-in so `(consensus-submit :preset 'broad)' schedules
;; the judge on fan-out termination without a second call.
;;
;; Design:  docs/design/23-consensus-presets.org

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Forward reference — the orchestrator core requires us lazily.
(defvar anvil-orchestrator--providers)


;;;; --- group + defcustoms --------------------------------------------------

(defgroup anvil-orchestrator-presets nil
  "Named consensus provider presets for `anvil-orchestrator'."
  :group 'anvil-orchestrator
  :prefix "anvil-orchestrator-consensus-preset-")

(defcustom anvil-orchestrator-consensus-preset-defaults
  '((fast     :providers (claude codex)
              :judge     nil
              :threshold 0.5)
    (cheap    :providers (codex ollama)
              :judge     nil
              :threshold 0.4)
    (broad    :providers (claude codex ollama gemini)
              :judge     claude
              :threshold 0.4)
    (rigorous :providers (claude codex ollama gemini)
              :judge     claude
              :threshold 0.7))
  "Alist of (NAME . PLIST) default consensus presets.
NAME is a symbol.  PLIST accepts the following keys:

  :providers   (LIST)         — required, >= 2 provider symbols.
  :judge       (SYMBOL/nil)   — optional meta-LLM judge provider.
  :judge-extra (STRING/nil)   — extra instruction appended to the
                                judge prompt template.
  :threshold   (FLOAT 0-1)    — Jaccard verdict cutoff; nil falls
                                back to `anvil-orchestrator-consensus-threshold'.
  :timeout-sec (INT/nil)      — per-task timeout default.
  :budget-usd  (FLOAT/nil)    — per-task budget default.
  :overrides   (ALIST/nil)    — ((PROVIDER . (:key VAL ...)) ...)
                                forwarded to `submit-consensus'.

Users may overwrite or extend this list.  Ship-time built-ins live
here so a freshly enabled orchestrator has recognisable preset names
without persistent state — Phase 2 of Doc 23 adds `anvil-state'
persistence on top."
  :type '(alist :key-type symbol :value-type plist)
  :group 'anvil-orchestrator-presets)


;;;; --- internal state ------------------------------------------------------

(defvar anvil-orchestrator--consensus-presets nil
  "Hash table NAME (symbol) → preset plist.
Populated lazily from `anvil-orchestrator-consensus-preset-defaults'
on first access through the CRUD API.  Users mutate via
`anvil-orchestrator-consensus-preset-set' etc. rather than poking
this variable directly.")

(defun anvil-orchestrator--consensus-presets-table ()
  "Return the preset hash table, seeding from defaults on first call."
  (unless anvil-orchestrator--consensus-presets
    (setq anvil-orchestrator--consensus-presets
          (make-hash-table :test 'eq))
    (dolist (entry anvil-orchestrator-consensus-preset-defaults)
      (puthash (car entry) (copy-sequence (cdr entry))
               anvil-orchestrator--consensus-presets)))
  anvil-orchestrator--consensus-presets)

(defun anvil-orchestrator--consensus-presets-reset ()
  "Drop any user-defined presets; re-seed from defaults.
Exposed for tests — end users should prefer targeted `-set' / `-delete'
operations."
  (setq anvil-orchestrator--consensus-presets nil)
  (anvil-orchestrator--consensus-presets-table))


;;;; --- validation ----------------------------------------------------------

(defun anvil-orchestrator--preset-validate (spec)
  "Signal `user-error' unless SPEC is a well-formed preset plist.
A well-formed preset has a non-empty list of provider symbols in
`:providers' of length >= 2, a nil-or-symbol `:judge', and a nil-or-
float-in-[0,1] `:threshold'.  Other keys are accepted verbatim; the
orchestrator validates provider symbols at submit time."
  (unless (and (listp spec) (keywordp (car spec)))
    (user-error "anvil-orchestrator-consensus-preset: SPEC must be a plist"))
  (let ((providers (plist-get spec :providers))
        (judge     (plist-get spec :judge))
        (threshold (plist-get spec :threshold)))
    (unless (and (listp providers)
                 (>= (length providers) 2)
                 (cl-every #'symbolp providers))
      (user-error "anvil-orchestrator-consensus-preset: :providers must be a list of >= 2 symbols"))
    (when (and judge (not (symbolp judge)))
      (user-error "anvil-orchestrator-consensus-preset: :judge must be a symbol or nil"))
    (when threshold
      (unless (and (numberp threshold) (<= 0 threshold 1))
        (user-error "anvil-orchestrator-consensus-preset: :threshold must be in [0,1]")))
    t))


;;;; --- public CRUD --------------------------------------------------------

;;;###autoload
(defun anvil-orchestrator-consensus-preset-set (name spec)
  "Register or overwrite preset NAME with SPEC.
NAME is a symbol.  SPEC is a plist validated by
`anvil-orchestrator--preset-validate'.  Returns NAME on success."
  (unless (symbolp name)
    (user-error "anvil-orchestrator-consensus-preset-set: NAME must be a symbol"))
  (anvil-orchestrator--preset-validate spec)
  (puthash name (copy-sequence spec)
           (anvil-orchestrator--consensus-presets-table))
  name)

;;;###autoload
(defun anvil-orchestrator-consensus-preset-get (name)
  "Return the preset plist for NAME, or nil if unknown."
  (let ((found (gethash name
                        (anvil-orchestrator--consensus-presets-table))))
    (and found (copy-sequence found))))

;;;###autoload
(defun anvil-orchestrator-consensus-preset-list ()
  "Return a sorted alist ((NAME . PLIST) ...) of every known preset."
  (let (entries)
    (maphash (lambda (k v) (push (cons k (copy-sequence v)) entries))
             (anvil-orchestrator--consensus-presets-table))
    (sort entries
          (lambda (a b) (string< (symbol-name (car a))
                                 (symbol-name (car b)))))))

;;;###autoload
(defun anvil-orchestrator-consensus-preset-delete (name)
  "Remove preset NAME.  Returns t when a preset existed, nil otherwise."
  (let* ((h (anvil-orchestrator--consensus-presets-table))
         (existed (and (gethash name h) t)))
    (when existed (remhash name h))
    existed))


;;;; --- resolution --------------------------------------------------------

(defun anvil-orchestrator--preset-resolve (preset caller-providers
                                                  caller-overrides
                                                  caller-timeout-sec
                                                  caller-budget-usd)
  "Resolve PRESET-NAME + explicit caller keywords into a merged plist.

Returns a plist suitable for direct consumption by
`anvil-orchestrator-submit-consensus':

  (:providers LIST
   :overrides ALIST
   :timeout-sec INT-or-nil
   :budget-usd  FLOAT-or-nil
   :judge       SYM-or-nil
   :judge-extra STR-or-nil
   :threshold   FLOAT-or-nil
   :preset-name SYM)

Caller-supplied keywords win over preset values (`:providers',
`:timeout-sec', `:budget-usd' are authoritative; `:overrides' is
merged per-provider with caller entries winning).  PRESET may be
nil in which case the result is just the caller values."
  (let ((entry (and preset
                    (or (anvil-orchestrator-consensus-preset-get preset)
                        (user-error
                         "consensus-preset: unknown preset %S" preset)))))
    (list :providers   (or caller-providers (plist-get entry :providers))
          :overrides   (anvil-orchestrator--preset-merge-overrides
                        (plist-get entry :overrides) caller-overrides)
          :timeout-sec (or caller-timeout-sec (plist-get entry :timeout-sec))
          :budget-usd  (or caller-budget-usd  (plist-get entry :budget-usd))
          :judge       (plist-get entry :judge)
          :judge-extra (plist-get entry :judge-extra)
          :threshold   (plist-get entry :threshold)
          :preset-name preset)))

(defun anvil-orchestrator--preset-merge-overrides (preset-overrides
                                                   caller-overrides)
  "Merge two override alists.  CALLER entries shadow PRESET entries
at the provider-symbol key level (per-provider plists are not deep-
merged — caller wins the whole plist for that provider)."
  (let ((merged (copy-alist preset-overrides)))
    (dolist (pair caller-overrides merged)
      (setf (alist-get (car pair) merged nil nil #'eq) (cdr pair)))))


(provide 'anvil-orchestrator-presets)

;;; anvil-orchestrator-presets.el ends here
