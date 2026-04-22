;;; anvil-manifest.el --- tools/list profile filter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Per-session tools/list filter: ANVIL_PROFILE env var selects a
;; subset of registered tools to advertise via the MCP manifest.
;; Handlers remain live, so hidden tools stay callable via explicit
;; tools/call.  The goal is shrinking per-turn system-prompt cost for
;; Claude Code sessions that do not need every anvil capability.
;;
;; Design doc: docs/design/26-manifest-profile.org.
;;
;; Profiles (see defconsts below):
;;   ultra - hot daily-driver tools only (~15)
;;   nav   - read-only exploration (~40)
;;   core  - daily coding + edits, no orchestrator/browser/pty (~80)
;;   lean  - core without memory-engine advertising (placeholder until
;;           Doc 29 ships; currently identical to core)
;;   full  - every registered tool (magic sentinel, no filter)
;;
;; Usage:
;;   ANVIL_PROFILE=ultra emacs ...        # env override
;;   (setq anvil-manifest-profile 'nav)   # Lisp override
;;
;; The active profile is locked at `anvil-manifest-enable' time.
;; MCP does not support mid-session manifest swaps — restart to change.

;;; Code:

(require 'cl-lib)
(require 'anvil-server)

;;; Customization

(defgroup anvil-manifest nil
  "tools/list manifest profile filter."
  :group 'anvil
  :prefix "anvil-manifest-")

(defcustom anvil-manifest-profile
  (let ((env (getenv "ANVIL_PROFILE")))
    (if (and env (not (string-empty-p env)))
        (intern env)
      'full))
  "Active manifest profile (global default).
Selects which registered tools are advertised via tools/list when
the incoming server-id has no entry in `anvil-manifest-server-profiles'.
Set via `ANVIL_PROFILE' env var or Lisp before calling
`anvil-manifest-enable'.  Handlers remain live regardless of profile,
so hidden tools stay callable via explicit tools/call.

Legacy ID-list profiles (full / core / nav / ultra / lean) enumerate
tool ids explicitly; intent-based profiles (agent / edit, Doc 34
Phase B) filter by the `:intent' / `:layer' / `:stability' metadata
attached at registration time."
  :type '(choice (const full) (const core) (const nav)
                 (const ultra) (const lean)
                 (const agent) (const edit))
  :group 'anvil-manifest)

(defcustom anvil-manifest-server-profiles
  '(("emacs-eval-ultra" . ultra)
    ("emacs-eval-nav"   . nav)
    ("emacs-eval-core"  . core)
    ("emacs-eval-agent" . agent)
    ("emacs-eval-edit"  . edit))
  "Alist of (SERVER-ID . PROFILE) overriding `anvil-manifest-profile'.
Lets one MCP daemon advertise different tool subsets to different
clients when each client connects under a distinct (virtual) server-id.

The default set assumes `anvil-server-id-aliases' aliases each
virtual id back to the real `emacs-eval' — see
`anvil-manifest--default-aliases'.  To apply `ultra' to an
orchestrator subprocess, point its MCP config's `--server-id=' at
`emacs-eval-ultra'.

Setting a profile to `full' for a given server-id explicitly restores
the unfiltered default for that connection."
  :type '(alist :key-type string :value-type symbol)
  :group 'anvil-manifest)

(defconst anvil-manifest--default-aliases
  '(("emacs-eval-ultra" . "emacs-eval")
    ("emacs-eval-nav"   . "emacs-eval")
    ("emacs-eval-core"  . "emacs-eval")
    ("emacs-eval-agent" . "emacs-eval")
    ("emacs-eval-edit"  . "emacs-eval"))
  "Default `anvil-server-id-aliases' entries installed by
`anvil-manifest-enable'.  Each alias routes a virtual server-id at
the anvil-server tool table for `emacs-eval', so the filter can apply
a per-connection profile without duplicating registrations.

Doc 34 Phase B added `emacs-eval-agent' / `emacs-eval-edit' on top
of the original ultra / nav / core trio.")

;;; Profile definitions

(defconst anvil-manifest-profile-ultra
  '("file-read"
    "file-batch"
    "file-outline"
    "org-read-outline"
    "org-read-headline"
    "org-read-by-id"
    "org-index-search"
    "git-status"
    "git-diff-names"
    "git-log"
    "git-head-sha"
    "http-fetch"
    "bench-compare"
    "bench-last"
    "anvil-test-run-all"
    "manifest-cost")
  "Hot tools only — Claude Code daily driver.
Keep this list tight (~15); items here dominate every system prompt.")

(defconst anvil-manifest-profile-nav
  (append anvil-manifest-profile-ultra
          '("code-extract-pattern"
            "defs-search" "defs-signature" "defs-references"
            "defs-index-status" "defs-who-requires"
            "git-diff-stats" "git-repo-root" "git-worktree-list"
            "git-branch-current"
            "elisp-describe-function" "elisp-describe-variable"
            "elisp-get-function-definition" "elisp-info-lookup-symbol"
            "elisp-read-source-file"
            "py-list-functions" "py-list-classes" "py-list-imports"
            "py-list-methods" "py-list-decorators"
            "py-find-definition" "py-surrounding-form"
            "sexp-read-file" "sexp-surrounding-form"
            "sexp-macroexpand" "sexp-verify"
            "buffer-read" "buffer-list-modified"
            "org-read-file" "org-get-allowed-files"
            "org-get-tag-config" "org-get-todo-config"
            "http-head"
            "sqlite-query"))
  "Read-only exploration — adds nav/describe tools on top of ultra.")

(defconst anvil-manifest-profile-core
  (append anvil-manifest-profile-nav
          '("file-append" "file-batch-across" "file-delete-lines"
            "file-ensure-import" "file-insert-at-line"
            "file-replace-regexp" "file-replace-string"
            "json-object-add" "code-add-field-by-map"
            "sexp-rename-symbol" "sexp-replace-call"
            "sexp-replace-defun" "sexp-wrap-form"
            "py-add-import" "py-remove-import" "py-rename-import"
            "py-replace-function" "py-wrap-expr"
            "org-add-todo" "org-edit-body" "org-update-todo-state"
            "org-rename-headline" "org-index-rebuild"
            "elisp-byte-compile-file" "elisp-ert-run"
            "emacs-eval" "emacs-eval-async" "emacs-eval-result"
            "emacs-eval-jobs"
            "buffer-save"
            "bisect-test" "bisect-cancel" "bisect-last-result"
            "git-commit-message" "git-pr-body"
            "defs-index-rebuild"
            "diagnostics"
            "bench-profile-expr"
            "http-cache-clear"
            "anvil-self-sync-check" "anvil-scaffold-module"
            "anvil-release-audit"))
  "Daily coding + edits — adds write/edit tools on top of nav.
Excludes orchestrator / browser / pty / cron / worker-admin tools.")

(defconst anvil-manifest-profile-lean
  anvil-manifest-profile-core
  "Placeholder — currently identical to core.
After Doc 29 (memory-engine) ships, memory-* tools will be stripped
here to keep the `learning workload' path slim.")

;;; Intent-based profiles (Doc 34 Phase B)
;;
;; These use the `:intent' / `:layer' / `:stability' metadata attached
;; at registration time instead of enumerating tool ids explicitly.
;; The leading `:filter' keyword distinguishes a filter-spec plist
;; from a list-of-strings profile.  Tools without metadata receive
;; default values (intent=general / layer=core / stability=stable)
;; via `anvil-manifest--filter-match-p'.

(defconst anvil-manifest-profile-agent
  '(:filter t
    :intent-include (orchestrator session memory browser
                     file-edit file-read org-read org-edit
                     code-bulk-edit json-edit structure
                     git discovery)
    :layer-include (core workflow)
    :stability-exclude (experimental deprecated))
  "Agent workload: orchestrator-driven long-running tasks.
Intent-filter variant (Doc 34 Phase B) — advertises any tool whose
`:intent' overlaps the include list AND whose `:layer' is core or
workflow AND whose `:stability' is not experimental/deprecated.")

(defconst anvil-manifest-profile-edit
  '(:filter t
    :intent-include (file-edit file-read org-read org-edit
                     code-bulk-edit json-edit structure db-read)
    :layer-include (core)
    :stability-exclude (experimental deprecated))
  "Daily editing: file / code / org, layer=core only.
Intent-filter variant (Doc 34 Phase B) — excludes io (http),
workflow (git, orchestrator), and dev (bench, meta) layers.")

;;; Filter logic

(defun anvil-manifest--profile-toolset (profile)
  "Return the toolset specifier for PROFILE.
Possible return shapes:
  :all                 — full profile, every tool visible
  (\"tool-id\" ...)    — ID-list profile (ultra / nav / core / lean)
  (:filter ...)         — filter-spec profile (agent / edit)"
  (pcase profile
    ('full :all)
    ('core anvil-manifest-profile-core)
    ('nav  anvil-manifest-profile-nav)
    ('ultra anvil-manifest-profile-ultra)
    ('lean anvil-manifest-profile-lean)
    ('agent anvil-manifest-profile-agent)
    ('edit anvil-manifest-profile-edit)
    (_ (user-error "anvil-manifest: unknown profile %S" profile))))

(defun anvil-manifest--profile-for-server-id (server-id)
  "Return the profile symbol that applies to SERVER-ID.
Falls back to the global `anvil-manifest-profile' when the incoming
server-id is absent from `anvil-manifest-server-profiles'."
  (or (cdr (assoc server-id anvil-manifest-server-profiles))
      anvil-manifest-profile))

(defun anvil-manifest--filter-match-p (filter tool-plist)
  "Return non-nil when TOOL-PLIST matches the FILTER spec.
FILTER is a plist whose first element is `:filter'; recognised
clauses are `:intent-include' (tool passes iff its `:intent' list
intersects), `:layer-include' (tool passes iff its `:layer' is a
member), and `:stability-exclude' (tool fails iff its `:stability'
is a member).  Missing clauses pass all tools.  TOOL-PLIST may be
nil, in which case default metadata (intent general / layer core /
stability stable) is assumed."
  (let ((intent-inc (plist-get filter :intent-include))
        (layer-inc (plist-get filter :layer-include))
        (stab-exc (plist-get filter :stability-exclude))
        (tool-intents (or (plist-get tool-plist :intent) '(general)))
        (tool-layer (or (plist-get tool-plist :layer) 'core))
        (tool-stab (or (plist-get tool-plist :stability) 'stable)))
    (and
     (or (null intent-inc) (cl-intersection intent-inc tool-intents))
     (or (null layer-inc) (memq tool-layer layer-inc))
     (not (memq tool-stab stab-exc)))))

(defun anvil-manifest--visible-p (tool-id tool-plist &optional server-id)
  "Return non-nil if TOOL-ID should appear in tools/list for SERVER-ID.
Used as `anvil-server-tool-filter-function' when the module is
enabled.  When called without SERVER-ID (legacy two-argument callers),
the global `anvil-manifest-profile' applies.

Dispatches on the toolset shape: `:all' sentinel passes everything,
a list of strings uses ID membership (ultra / nav / core / lean),
and a `(:filter ...)' plist uses the intent/layer/stability filter
(agent / edit)."
  (let* ((profile (anvil-manifest--profile-for-server-id server-id))
         (set (anvil-manifest--profile-toolset profile)))
    (cond
     ((eq set :all) t)
     ((and (consp set) (eq (car set) :filter))
      (anvil-manifest--filter-match-p set tool-plist))
     (t (member tool-id set)))))

;;; manifest-cost handler

(defun anvil-manifest--schema-token-count (schema)
  "Roughly estimate SCHEMA size in tokens (1 token ≈ 4 chars)."
  (let ((text (format "%S" schema)))
    (max 1 (/ (length text) 4))))

(defun anvil-manifest--entry-token-count (tool-id tool)
  "Estimate the per-tool cost of advertising TOOL-ID with TOOL plist.
Counts the name, description and schema, matching what ends up in
the MCP response."
  (let* ((desc (or (plist-get tool :description) ""))
         (schema (or (plist-get tool :schema) '((type . "object"))))
         (base (+ (length tool-id) (length desc)))
         (schema-tok (anvil-manifest--schema-token-count schema)))
    (+ (max 1 (/ base 4)) schema-tok)))

(defun anvil-manifest-cost-handler ()
  "Return current profile, advertised tool count and approx tokens.

MCP Parameters: (none)"
  (anvil-server-with-error-handling
    (let* ((profile anvil-manifest-profile)
           (server-id "default")
           (tools-table (gethash server-id anvil-server--tools))
           (advertised 0)
           (tokens 0)
           (all 0))
      (when tools-table
        (maphash
         (lambda (tool-id tool)
           (cl-incf all)
           (when (anvil-manifest--visible-p tool-id tool)
             (cl-incf advertised)
             (cl-incf tokens
                      (anvil-manifest--entry-token-count
                       tool-id tool))))
         tools-table))
      (list :profile profile
            :advertised-count advertised
            :registered-count all
            :approx-tokens tokens
            :profiles-available
            '(full core nav ultra lean agent edit)))))

;;; Lifecycle

(defconst anvil-manifest--tool-specs
  `((,#'anvil-manifest-cost-handler
     :id "manifest-cost"
     :intent '(meta token)
     :layer 'dev
     :description "Return the active ANVIL_PROFILE, count of advertised tools, and an approximate token cost of the current tools/list manifest.  Handlers of hidden tools remain callable via explicit tools/call."
     :read-only t
     :title "Manifest Cost"))
  "MCP tool specs provided by `anvil-manifest'.")

;;;###autoload
(defun anvil-manifest-enable ()
  "Activate the tools/list profile filter and register manifest-cost.
Installs `anvil-manifest--default-aliases' into
`anvil-server-id-aliases' so orchestrator subprocesses can connect
as `emacs-eval-ultra' / `emacs-eval-nav' / `emacs-eval-core' and
receive the matching filtered manifest without duplicating tool
registrations."
  (interactive)
  (setq anvil-server-tool-filter-function
        #'anvil-manifest--visible-p)
  (dolist (alias anvil-manifest--default-aliases)
    (cl-pushnew alias anvil-server-id-aliases :test #'equal))
  (anvil-server-register-tools "default" anvil-manifest--tool-specs))

(defun anvil-manifest-disable ()
  "Deactivate the profile filter and unregister manifest-cost.
Also removes `anvil-manifest--default-aliases' from
`anvil-server-id-aliases'."
  (interactive)
  (when (eq anvil-server-tool-filter-function
            #'anvil-manifest--visible-p)
    (setq anvil-server-tool-filter-function nil))
  (dolist (alias anvil-manifest--default-aliases)
    (setq anvil-server-id-aliases
          (delete alias anvil-server-id-aliases)))
  (anvil-server-unregister-tools "default" anvil-manifest--tool-specs))

(provide 'anvil-manifest)
;;; anvil-manifest.el ends here
