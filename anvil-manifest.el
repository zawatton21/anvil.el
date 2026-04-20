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
so hidden tools stay callable via explicit tools/call."
  :type '(choice (const full) (const core) (const nav)
                 (const ultra) (const lean))
  :group 'anvil-manifest)

(defcustom anvil-manifest-server-profiles
  '(("emacs-eval-ultra" . ultra)
    ("emacs-eval-nav"   . nav)
    ("emacs-eval-core"  . core))
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
    ("emacs-eval-core"  . "emacs-eval"))
  "Default `anvil-server-id-aliases' entries installed by
`anvil-manifest-enable'.  Each alias routes a virtual server-id at
the anvil-server tool table for `emacs-eval', so the filter can apply
a per-connection profile without duplicating registrations.")

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

;;; Filter logic

(defun anvil-manifest--profile-toolset (profile)
  "Return the list of tool IDs visible under PROFILE, or `:all' for full."
  (pcase profile
    ('full :all)
    ('core anvil-manifest-profile-core)
    ('nav  anvil-manifest-profile-nav)
    ('ultra anvil-manifest-profile-ultra)
    ('lean anvil-manifest-profile-lean)
    (_ (user-error "anvil-manifest: unknown profile %S" profile))))

(defun anvil-manifest--profile-for-server-id (server-id)
  "Return the profile symbol that applies to SERVER-ID.
Falls back to the global `anvil-manifest-profile' when the incoming
server-id is absent from `anvil-manifest-server-profiles'."
  (or (cdr (assoc server-id anvil-manifest-server-profiles))
      anvil-manifest-profile))

(defun anvil-manifest--visible-p (tool-id _tool-plist &optional server-id)
  "Return non-nil if TOOL-ID should appear in tools/list for SERVER-ID.
Used as `anvil-server-tool-filter-function' when the module is
enabled.  When called without SERVER-ID (legacy two-argument callers),
the global `anvil-manifest-profile' applies."
  (let* ((profile (anvil-manifest--profile-for-server-id server-id))
         (set (anvil-manifest--profile-toolset profile)))
    (or (eq set :all)
        (member tool-id set))))

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
            '(full core nav ultra lean)))))

;;; Lifecycle

(defconst anvil-manifest--tool-specs
  `((,#'anvil-manifest-cost-handler
     :id "manifest-cost"
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
