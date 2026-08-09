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
(require 'subr-x)
(require 'anvil-server)

(declare-function anvil-autoresearch-docstrings "anvil-autoresearch"
                  (&optional query server-id limit))

;;; Customization

(defun anvil-manifest--parse-symbol-list (value)
  "Return VALUE as a list of symbols."
  (cond
   ((null value) nil)
   ((and (consp value) (eq (car value) 'quote))
    (anvil-manifest--parse-symbol-list (cadr value)))
   ((symbolp value) (list value))
   ((stringp value)
    (mapcar #'intern
            (split-string (string-trim value) "[, \t\n\r]+" t)))
   ((vectorp value)
    (anvil-manifest--parse-symbol-list (append value nil)))
   ((listp value)
    (delq nil
          (mapcar (lambda (v)
                    (car (anvil-manifest--parse-symbol-list v)))
                  value)))
   (t nil)))

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
                 (const agent) (const edit)
                 (const headless) (const dynamic))
  :group 'anvil-manifest)

(defcustom anvil-manifest-attention-query
  (or (getenv "ANVIL_ATTENTION_QUERY") "")
  "Query used by the `dynamic' manifest profile.
This is Doc 44 Phase 2a IsO-lite: a local lexical intent search over
registered tool id, description, intent, and layer metadata.  Empty
query falls back to the `agent' profile instead of hiding unknown tools."
  :type 'string
  :group 'anvil-manifest)

(defcustom anvil-manifest-attention-top-k 10
  "Maximum number of tools selected by the `dynamic' manifest profile."
  :type 'integer
  :group 'anvil-manifest)

(defcustom anvil-manifest-state-tags
  (anvil-manifest--parse-symbol-list (getenv "ANVIL_STATE_TAGS"))
  "Current dialog/workspace state tags used by Doc 44 state filtering.
Tools may declare `:preconditions' metadata at registration time.
Supported shapes are a single symbol, a list of required symbols, or a
plist with `:requires', `:excludes', and `:any' tag lists."
  :type '(repeat symbol)
  :group 'anvil-manifest)

(defcustom anvil-manifest-schema-trim-enabled t
  "Non-nil trims non-focused tools/list schemas in the dynamic profile.
Top-K Tool Attention hits, manifest self-inspection tools, and warm LRU
tools keep full schemas.  Other state-compatible dynamic-profile tools
remain advertised with short descriptions and parameter-name/type-only
schemas so recovery remains possible without paying full schema cost."
  :type 'boolean
  :group 'anvil-manifest)

(defcustom anvil-manifest-warm-tool-limit 20
  "Maximum number of recently used tools kept full in dynamic profile."
  :type 'integer
  :group 'anvil-manifest)

(defcustom anvil-manifest-recovery-suggest-limit 8
  "Default number of Tool Attention recovery suggestions."
  :type 'integer
  :group 'anvil-manifest)

(defcustom anvil-manifest-embedding-backend 'lexical
  "Embedding backend for Doc 44 full Tool Attention.
`lexical' keeps the built-in IsO-lite scorer.  `function' calls
`anvil-manifest-embedding-function' with one text argument and expects
a numeric vector/list.  `command' runs
`anvil-manifest-embedding-command' once per text and expects JSON or
whitespace-separated numbers on stdout.  Backend failures fall back to
lexical ranking."
  :type '(choice (const lexical) (const function) (const command))
  :group 'anvil-manifest)

(defcustom anvil-manifest-embedding-command nil
  "Shell command used when `anvil-manifest-embedding-backend' is `command'.
The command receives the text on stdin and must write a JSON array or
whitespace-separated numeric vector to stdout."
  :type '(choice (const nil) string)
  :group 'anvil-manifest)

(defvar anvil-manifest-embedding-function nil
  "Optional function used by the `function' embedding backend.
Called with one text string.  It must return a numeric vector or list,
or nil to fall back to lexical scoring.")

(defcustom anvil-manifest-docstring-candidate-limit 10
  "Default number of Doc 44 docstring rewrite candidates to return."
  :type 'integer
  :group 'anvil-manifest)

(defcustom anvil-manifest-server-profiles
  '(("emacs-eval-ultra" . ultra)
    ("emacs-eval-nav"   . nav)
    ("emacs-eval-core"  . core)
    ("emacs-eval-agent" . agent)
    ("emacs-eval-edit"  . edit)
    ("emacs-eval-dynamic" . dynamic)
    ("emacs-eval-headless" . headless))
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
    ("emacs-eval-edit"  . "emacs-eval")
    ("emacs-eval-dynamic" . "emacs-eval")
    ("emacs-eval-headless" . "emacs-eval"))
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
    "manifest-cost"
    "manifest-attention"
    "manifest-recovery-suggest"
    "manifest-docstring-candidates")
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
            "org-agenda-view" "org-habit-summary"
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
            "org-rename-headline" "org-capture-string"
            "org-index-rebuild"
            "elisp-byte-compile-file" "elisp-ert-run"
            "emacs-eval" "emacs-eval-async" "emacs-eval-result"
            "emacs-eval-jobs" "emacs-eval-cancel"
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

(defconst anvil-manifest-profile-headless
  '(;; Phase 5-E NeLisp MCP server baseline (7 tools) — Stage D MVP
    "file-read" "file-outline"
    "git-log" "git-status"
    "http-fetch"
    "data-get-path" "data-set-path"
    ;; Read / write file ops — headless でも不可欠、UI 依存なし
    "file-batch" "file-batch-across"
    "file-replace-string" "file-replace-regexp"
    "file-insert-at-line" "file-delete-lines" "file-append"
    "file-ensure-import" "file-read-snippet"
    "json-object-add" "code-add-field-by-map"
    "code-extract-pattern"
    ;; Git — subprocess 駆動、TUI 不要
    "git-diff-names" "git-diff-stats" "git-head-sha"
    "git-repo-root" "git-worktree-list" "git-branch-current"
    "git-commit-message" "git-pr-body"
    ;; HTTP — anvil-http Phase 6.2 port 対象、全部 expose
    "http-head" "http-cache-get" "http-cache-clear" "http-cache-index"
    ;; Data / state — Phase 5-F.1 state port で追加予定
    "data-list-keys" "data-delete-path"
    ;; Memory — Doc 29 Phase 5 DB-direct surface (org 依存系は除外)
    "memory-search" "memory-list" "memory-get"
    "memory-add" "memory-save-check" "memory-access"
    "memory-export-md" "memory-regenerate-index"
    ;; Worklog — Doc 42 Phase 2 DB-direct surface (org-scan/export は除外)
    "worklog-add" "worklog-search" "worklog-list" "worklog-get"
    ;; Manifest 自己診断
    "manifest-cost")
  "Stage D (Doc 18) MVP = non-Emacs user distribution。
TUI / transient / dashboard 依存 tool を完全除外、headless 環境で
安全に動く subset のみ。=bin/anvil mcp serve= の default profile と
して Stage D Phase 6.1 launcher から参照される。

nav/core との差:
- orchestrator / browser / pty / worker-admin / bisect / 大量の
  dev tool は含めない (headless 環境で誤爆防止)
- defs-* / elisp-describe-* / py-list-* 等 interactive 探索系も除外
  (non-Emacs user は別手段で IDE 内検索)
- memory-* / worklog-* の DB-direct surface は cross-AI interchange
  に必須なので opt-in (memory-scan / worklog-scan / worklog-export-org
  などの org 依存 tool は引き続き除外)

Phase 5-F.1 state port 完了後、state-* tool 6 本を追加予定。")

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
                     git discovery research)
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

(defconst anvil-manifest--self-tool-ids
  '("manifest-cost"
    "manifest-attention"
    "manifest-recovery-suggest"
    "manifest-docstring-candidates")
  "Tool ids that must stay visible so a filtered profile can inspect itself.")

(defvar anvil-manifest--warm-tools nil
  "Most recently dispatched tool ids, newest first.")

(defvar anvil-manifest--previous-tool-fragment-function nil
  "Tool fragment transformer active before `anvil-manifest-enable'.")

(defvar anvil-manifest--summary-index-cache (make-hash-table :test #'equal)
  "Per-server tool summary index cache for Doc 44 embedding search.")

;;; Filter logic

(defun anvil-manifest--profile-toolset (profile)
  "Return the toolset specifier for PROFILE.
Possible return shapes:
  :all                 — full profile, every tool visible
  (\"tool-id\" ...)    — ID-list profile (ultra / nav / core / lean)
  (:filter ...)         — filter-spec profile (agent / edit)
  (:attention ...)      — dynamic Doc 44 IsO-lite selector"
  (pcase profile
    ('full :all)
    ('core anvil-manifest-profile-core)
    ('nav  anvil-manifest-profile-nav)
    ('ultra anvil-manifest-profile-ultra)
    ('lean anvil-manifest-profile-lean)
    ('agent anvil-manifest-profile-agent)
    ('edit anvil-manifest-profile-edit)
    ('headless anvil-manifest-profile-headless)
    ('dynamic '(:attention t))
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

;;; Tool Attention (Doc 44 Phase 2a IsO-lite)

(defun anvil-manifest--metadata-list (value fallback)
  "Return VALUE as a symbol list, unquoting simple quoted forms."
  (let ((v (or value fallback)))
    (cond
     ((and (consp v) (eq (car v) 'quote)) (cadr v))
     ((listp v) v)
     ((symbolp v) (list v))
     (t fallback))))

(defun anvil-manifest--attention-tokens (text)
  "Return normalized lexical tokens from TEXT."
  (delete-dups
   (cl-loop for word in (split-string
                         (downcase (or text ""))
                         "[^[:alnum:]_-]+" t)
            when (>= (length word) 3)
            collect word)))

(defun anvil-manifest--attention-tool-text (tool-id tool)
  "Return searchable text for TOOL-ID and TOOL."
  (let* ((desc (or (plist-get tool :description) ""))
         (intent (anvil-manifest--metadata-list
                  (plist-get tool :intent) '(general)))
         (layer (or (plist-get tool :layer) 'core)))
    (string-join
     (list tool-id
           (replace-regexp-in-string "[-_]" " " tool-id)
           desc
           (mapconcat #'symbol-name intent " ")
           (symbol-name layer))
     " ")))

(defun anvil-manifest--attention-score (query tool-id tool)
  "Return lexical relevance score for QUERY against TOOL-ID / TOOL."
  (let* ((tokens (anvil-manifest--attention-tokens query))
         (id-text (downcase (replace-regexp-in-string "[-_]" " " tool-id)))
         (desc (downcase (or (plist-get tool :description) "")))
         (meta (downcase
                (anvil-manifest--attention-tool-text tool-id tool)))
         (score 0))
    (dolist (token tokens)
      (when (string-match-p (regexp-quote token) id-text)
        (cl-incf score 5))
      (when (string-match-p (regexp-quote token) desc)
        (cl-incf score 3))
      (when (string-match-p (regexp-quote token) meta)
        (cl-incf score 1)))
    score))

(defun anvil-manifest--coerce-vector (value)
  "Return VALUE as a numeric vector, or nil."
  (let ((xs (cond
             ((vectorp value) (append value nil))
             ((listp value) value)
             (t nil))))
    (when (and xs (cl-every #'numberp xs))
      (vconcat xs))))

(defun anvil-manifest--parse-embedding-output (text)
  "Parse embedding command output TEXT into a numeric vector."
  (or (ignore-errors
        (anvil-manifest--coerce-vector
         (json-parse-string text :array-type 'list)))
      (anvil-manifest--coerce-vector
       (mapcar #'string-to-number
               (split-string (string-trim (or text "")) "[, \t\n\r]+" t)))))

(defun anvil-manifest--command-embedding (text)
  "Return embedding vector for TEXT using `anvil-manifest-embedding-command'."
  (when (and (stringp anvil-manifest-embedding-command)
             (not (string-empty-p anvil-manifest-embedding-command)))
    (let ((outbuf (generate-new-buffer " *anvil-manifest-embedding*"))
          exit out)
      (unwind-protect
          (progn
            (setq exit
                  (with-temp-buffer
                    (insert text)
                    (call-process-region
                     (point-min) (point-max)
                     shell-file-name nil outbuf nil
                     shell-command-switch
                     anvil-manifest-embedding-command)))
            (when (zerop exit)
              (setq out
                    (with-current-buffer outbuf
                      (buffer-substring-no-properties
                       (point-min) (point-max))))
              (anvil-manifest--parse-embedding-output out)))
        (when (buffer-live-p outbuf)
          (kill-buffer outbuf))))))

(defun anvil-manifest--embed-text (text)
  "Return embedding vector for TEXT, or nil to use lexical scoring."
  (pcase anvil-manifest-embedding-backend
    ('function
     (when (functionp anvil-manifest-embedding-function)
       (ignore-errors
         (anvil-manifest--coerce-vector
          (funcall anvil-manifest-embedding-function text)))))
    ('command
     (ignore-errors (anvil-manifest--command-embedding text)))
    (_ nil)))

(defun anvil-manifest--cosine (a b)
  "Return cosine similarity for numeric vectors A and B."
  (when (and (vectorp a) (vectorp b) (= (length a) (length b)))
    (let ((dot 0.0) (aa 0.0) (bb 0.0) (i 0))
      (while (< i (length a))
        (let ((x (float (aref a i)))
              (y (float (aref b i))))
          (setq dot (+ dot (* x y))
                aa (+ aa (* x x))
                bb (+ bb (* y y))))
        (setq i (1+ i)))
      (and (> aa 0.0) (> bb 0.0)
           (/ dot (* (sqrt aa) (sqrt bb)))))))

(defun anvil-manifest--summary-index (server-id)
  "Return cached tool summary rows for SERVER-ID."
  (let* ((sid (or server-id "default"))
         (resolved (anvil-server--resolve-id sid))
         (table (gethash resolved anvil-server--tools))
         (count (if (hash-table-p table) (hash-table-count table) 0))
         (cache (gethash resolved anvil-manifest--summary-index-cache)))
    (if (and cache (= count (plist-get cache :count)))
        (plist-get cache :rows)
      (let (rows)
        (when (hash-table-p table)
          (maphash
           (lambda (tool-id tool)
             (let ((text (anvil-manifest--attention-tool-text
                          tool-id tool)))
               (push (list :id tool-id
                           :server-id sid
                           :resolved-server-id resolved
                           :tool tool
                           :text text
                           :embedding (anvil-manifest--embed-text text))
                     rows)))
           table))
        (setq rows (nreverse rows))
        (puthash resolved (list :count count :rows rows)
                 anvil-manifest--summary-index-cache)
        rows))))

(defun anvil-manifest--attention-row-score (query row query-embedding)
  "Return (SCORE . BACKEND) for QUERY against summary ROW."
  (if-let ((sim (and query-embedding
                     (anvil-manifest--cosine
                      query-embedding
                      (plist-get row :embedding)))))
      (cons (round (* 1000 sim)) "embedding")
    (cons (anvil-manifest--attention-score
           query (plist-get row :id) (plist-get row :tool))
          "lexical")))

(defun anvil-manifest--state-match-p (preconditions &optional state-tags)
  "Return non-nil when PRECONDITIONS match STATE-TAGS.
STATE-TAGS defaults to `anvil-manifest-state-tags'.  PRECONDITIONS
may be nil, a required symbol, a list of required symbols, or a plist
with `:requires', `:excludes', and `:any'."
  (let ((tags (anvil-manifest--parse-symbol-list
               (or state-tags anvil-manifest-state-tags))))
    (cond
     ((null preconditions) t)
     ((symbolp preconditions) (memq preconditions tags))
     ((and (consp preconditions) (keywordp (car preconditions)))
      (let ((requires (anvil-manifest--parse-symbol-list
                       (plist-get preconditions :requires)))
            (excludes (anvil-manifest--parse-symbol-list
                       (plist-get preconditions :excludes)))
            (any (anvil-manifest--parse-symbol-list
                  (plist-get preconditions :any))))
        (and (cl-every (lambda (tag) (memq tag tags)) requires)
             (not (cl-intersection excludes tags))
             (or (null any) (cl-intersection any tags)))))
     ((listp preconditions)
      (cl-every (lambda (tag) (memq tag tags))
                (anvil-manifest--parse-symbol-list preconditions)))
     (t t))))

(defun anvil-manifest--attention-rank (query &optional server-id limit)
  "Return tools ranked by local lexical attention for QUERY."
  (let* ((sid (or server-id "default"))
         (limit (max 1 (or limit anvil-manifest-attention-top-k)))
         (query-embedding (anvil-manifest--embed-text query))
         rows)
    (dolist (row (anvil-manifest--summary-index sid))
      (let* ((tool-id (plist-get row :id))
             (tool (plist-get row :tool))
             (score+backend (anvil-manifest--attention-row-score
                             query row query-embedding))
             (score (car score+backend)))
        (when (and (> score 0)
                   (anvil-manifest--state-match-p
                    (plist-get tool :preconditions)))
          (push (list :id tool-id
                      :server-id sid
                      :resolved-server-id
                      (plist-get row :resolved-server-id)
                      :score score
                      :score-backend (cdr score+backend)
                      :preconditions
                      (or (plist-get tool :preconditions) nil)
                      :layer (or (plist-get tool :layer) 'core)
                      :intent
                      (apply #'vector
                             (anvil-manifest--metadata-list
                              (plist-get tool :intent) '(general)))
                      :description
                      (car (split-string
                            (or (plist-get tool :description) "")
                            "\n")))
                rows))))
    (setq rows
          (sort rows
                (lambda (a b)
                  (let ((sa (plist-get a :score))
                        (sb (plist-get b :score)))
                    (if (= sa sb)
                        (string< (plist-get a :id)
                                 (plist-get b :id))
                      (> sa sb))))))
    (cl-subseq rows 0 (min (length rows) limit))))

(defun anvil-manifest--attention-tool-ids (query &optional server-id)
  "Return Top-K tool ids selected by QUERY for SERVER-ID."
  (mapcar (lambda (row) (plist-get row :id))
          (anvil-manifest--attention-rank
           query server-id anvil-manifest-attention-top-k)))

(defun anvil-manifest--tool-deprecated-p (tool-plist)
  "Return non-nil when TOOL-PLIST declares deprecated stability.

MCP Parameters:
  tool-plist - Internal registered tool plist."
  (eq (plist-get tool-plist :stability) 'deprecated))

(defun anvil-manifest--focused-tool-p (tool-id &optional server-id)
  "Return non-nil when TOOL-ID should keep its full schema."
  (and
   (or (member tool-id anvil-manifest--self-tool-ids)
       (member tool-id anvil-manifest--warm-tools)
       (and (not (string-empty-p
                  (string-trim (or anvil-manifest-attention-query ""))))
            (member tool-id
                    (anvil-manifest--attention-tool-ids
                     anvil-manifest-attention-query server-id))))
   t))

(defun anvil-manifest--attention-visible-p (tool-id tool-plist server-id)
  "Return non-nil when TOOL-ID is visible in the dynamic profile."
  (cond
   ((member tool-id anvil-manifest--self-tool-ids) t)
   ((string-empty-p (string-trim (or anvil-manifest-attention-query "")))
    (and (anvil-manifest--filter-match-p anvil-manifest-profile-agent
                                         tool-plist)
         (anvil-manifest--state-match-p
          (plist-get tool-plist :preconditions))))
   (anvil-manifest-schema-trim-enabled
    (and (not (anvil-manifest--tool-deprecated-p tool-plist))
         (anvil-manifest--state-match-p
          (plist-get tool-plist :preconditions))))
   (t
    (and (anvil-manifest--state-match-p
          (plist-get tool-plist :preconditions))
         (member tool-id
                 (anvil-manifest--attention-tool-ids
                  anvil-manifest-attention-query server-id))))))

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
     ((and (consp set) (eq (car set) :attention))
      (anvil-manifest--attention-visible-p tool-id tool-plist server-id))
     (t (member tool-id set)))))

;;; Schema trimming / LRU / recovery (Doc 44 Phase 2c)

(defun anvil-manifest--record-tool-dispatch (tool-id _server-id)
  "Record TOOL-ID as recently used for dynamic full-schema warming."
  (when (and (stringp tool-id)
             (not (member tool-id anvil-manifest--self-tool-ids)))
    (setq anvil-manifest--warm-tools
          (cons tool-id (delete tool-id anvil-manifest--warm-tools)))
    (when (> (length anvil-manifest--warm-tools)
             anvil-manifest-warm-tool-limit)
      (setcdr (nthcdr (1- anvil-manifest-warm-tool-limit)
                      anvil-manifest--warm-tools)
              nil))))

(defun anvil-manifest--trim-schema (schema)
  "Return a compact parameter-name/type-only copy of SCHEMA."
  (let* ((schema (or schema '((type . "object"))))
         (type (or (cdr (assoc 'type schema))
                   (cdr (assoc "type" schema))
                   "object"))
         (required (or (cdr (assoc 'required schema))
                       (cdr (assoc "required" schema))))
         (props (or (cdr (assoc 'properties schema))
                    (cdr (assoc "properties" schema))))
         out-props)
    (dolist (prop props)
      (let* ((key (car prop))
             (pschema (cdr prop))
             (ptype (or (and (listp pschema)
                             (or (cdr (assoc 'type pschema))
                                 (cdr (assoc "type" pschema))))
                        "string")))
        (push (cons key `((type . ,ptype))) out-props)))
    (delq nil
          `((type . ,type)
            ,@(when out-props
                `((properties . ,(nreverse out-props))))
            ,@(when required
                `((required . ,required)))))))

(defun anvil-manifest--short-description (description)
  "Return compact one-line DESCRIPTION for trimmed tool fragments."
  (truncate-string-to-width
   (or (car (split-string (or description "") "[\n\r]" t)) "")
   180 nil nil t))

(defun anvil-manifest--tool-fragment
    (tool-id tool server-id default-fragment)
  "Return tools/list fragment for TOOL-ID under SERVER-ID.
Dynamic profile keeps full schemas for focused tools and advertises a
trimmed schema for the rest."
  (if (and anvil-manifest-schema-trim-enabled
           (eq (anvil-manifest--profile-for-server-id server-id) 'dynamic)
           (not (anvil-manifest--focused-tool-p tool-id server-id)))
      (anvil-server--build-tool-fragment
       tool-id
       (anvil-manifest--short-description
        (plist-get tool :description))
       (anvil-manifest--trim-schema (plist-get tool :schema)))
    default-fragment))

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
            '(full core nav ultra lean agent edit headless dynamic)))))

(defun anvil-manifest-attention-handler (&optional query server_id limit)
  "Return Doc 44 local Tool Attention ranking for QUERY.

MCP Parameters:
  query     - Natural-language intent query.  Empty / nil uses
              `anvil-manifest-attention-query'.
  server_id - Optional MCP server id whose registered tool table should
              be searched.  Defaults to \"default\" and respects
              `anvil-server-id-aliases'.
  limit     - Optional positive cap.  Defaults to
              `anvil-manifest-attention-top-k'."
  (anvil-server-with-error-handling
    (let* ((q (if (and (stringp query) (not (string-empty-p query)))
                  query
                anvil-manifest-attention-query))
           (sid (if (and (stringp server_id)
                         (not (string-empty-p server_id)))
                    server_id
                  "default"))
           (n (cond
               ((integerp limit) limit)
               ((and (stringp limit) (not (string-empty-p limit)))
                (string-to-number limit))
               (t anvil-manifest-attention-top-k)))
           (ranked (if (string-empty-p (string-trim (or q "")))
                       nil
                     (anvil-manifest--attention-rank
                      q sid (max 1 n)))))
      (list :query (or q "")
            :server-id sid
            :resolved-server-id (anvil-server--resolve-id sid)
            :ranking-backend
            (if (anvil-manifest--embed-text q) "embedding" "lexical")
            :summary-index-count
            (length (anvil-manifest--summary-index sid))
            :state-tags (apply #'vector
                               (anvil-manifest--parse-symbol-list
                                anvil-manifest-state-tags))
            :limit (max 1 n)
            :count (length ranked)
            :tools (apply #'vector ranked)))))

(defun anvil-manifest-recovery-suggest-handler
    (&optional query server_id limit)
  "Return Doc 44 recovery suggestions for QUERY.

MCP Parameters:
  query     - Natural-language task/query that failed to surface a
              useful tool.  Empty / nil uses
              `anvil-manifest-attention-query'.
  server_id - Optional MCP server id whose registered tools should be
              inspected.  Defaults to \"default\".
  limit     - Optional positive cap.  Defaults to
              `anvil-manifest-recovery-suggest-limit'."
  (anvil-server-with-error-handling
    (let* ((q (if (and (stringp query) (not (string-empty-p query)))
                  query
                anvil-manifest-attention-query))
           (sid (if (and (stringp server_id)
                         (not (string-empty-p server_id)))
                    server_id
                  "default"))
           (resolved (anvil-server--resolve-id sid))
           (table (gethash resolved anvil-server--tools))
           (n (anvil-manifest--coerce-limit
               limit anvil-manifest-recovery-suggest-limit))
           rows)
      (when (hash-table-p table)
        (maphash
         (lambda (tool-id tool)
           (let ((score (anvil-manifest--attention-score q tool-id tool)))
             (when (> score 0)
               (let* ((state-ok (anvil-manifest--state-match-p
                                 (plist-get tool :preconditions)))
                      (visible (anvil-manifest--visible-p
                                tool-id tool sid))
                      (focused (anvil-manifest--focused-tool-p
                                tool-id sid)))
                 (push (list :id tool-id
                             :server-id sid
                             :score score
                             :visible visible
                             :focused focused
                             :state-matches state-ok
                             :warm (and (member tool-id
                                                anvil-manifest--warm-tools)
                                        t)
                             :reason
                             (cond
                              ((not state-ok) "blocked-by-preconditions")
                              (focused "focused-full-schema")
                              (visible "visible-trimmed-schema")
                              (t "hidden-by-profile"))
                             :description
                             (car (split-string
                                   (or (plist-get tool :description) "")
                                   "\n")))
                       rows)))))
         table))
      (setq rows
            (sort rows
                  (lambda (a b)
                    (let ((sa (plist-get a :score))
                          (sb (plist-get b :score)))
                      (if (= sa sb)
                          (string< (plist-get a :id)
                                   (plist-get b :id))
                        (> sa sb))))))
      (setq rows (cl-subseq rows 0 (min (length rows) n)))
      (list :query (or q "")
            :server-id sid
            :resolved-server-id resolved
            :warm-tools (apply #'vector anvil-manifest--warm-tools)
            :state-tags (apply #'vector
                               (anvil-manifest--parse-symbol-list
                                anvil-manifest-state-tags))
            :limit n
            :count (length rows)
            :suggestions (apply #'vector rows)))))

(defun anvil-manifest--coerce-limit (limit default)
  "Return LIMIT as a positive integer, falling back to DEFAULT."
  (max 1
       (cond
        ((integerp limit) limit)
        ((and (numberp limit) (> limit 0)) (truncate limit))
        ((and (stringp limit) (not (string-empty-p limit)))
         (string-to-number limit))
        (t default))))

(defun anvil-manifest--candidate-tool (candidate server-id)
  "Return the registered tool plist for CANDIDATE under SERVER-ID."
  (let* ((sid (or (plist-get candidate :server-id) server-id "default"))
         (resolved (anvil-server--resolve-id sid))
         (table (gethash resolved anvil-server--tools)))
    (and (hash-table-p table)
         (gethash (plist-get candidate :id) table))))

(defun anvil-manifest--docstring-candidate-match-p
    (candidate query attention-score)
  "Return non-nil when CANDIDATE is relevant to QUERY."
  (or (string-empty-p (string-trim (or query "")))
      (> attention-score 0)
      (let ((haystack
             (downcase
              (string-join
               (delq nil
                     (list (plist-get candidate :id)
                           (plist-get candidate :description)
                           (mapconcat #'identity
                                      (append
                                       (plist-get candidate :issues)
                                       nil)
                                      " ")))
               " "))))
        (cl-some
         (lambda (token)
           (string-match-p (regexp-quote token) haystack))
         (anvil-manifest--attention-tokens query)))))

(defun anvil-manifest-docstring-candidates-handler
    (&optional query server_id limit)
  "Return Doc 44 docstring rewrite candidates backed by Doc 43.

MCP Parameters:
  query     - Optional natural-language Tool Attention query.  Empty /
              nil uses `anvil-manifest-attention-query'.  When set,
              candidates are filtered and ranked for that query.
  server_id - Optional MCP server id whose registered tool table should
              be inspected.  Defaults to \"default\" and respects
              `anvil-server-id-aliases'.
  limit     - Optional positive cap.  Defaults to
              `anvil-manifest-docstring-candidate-limit'."
  (anvil-server-with-error-handling
    (unless (or (fboundp 'anvil-autoresearch-docstrings)
                (require 'anvil-autoresearch nil t))
      (error "anvil-autoresearch is required for Doc 44 docstring candidates"))
    (let* ((q (if (and (stringp query) (not (string-empty-p query)))
                  query
                anvil-manifest-attention-query))
           (sid (if (and (stringp server_id)
                         (not (string-empty-p server_id)))
                    server_id
                  "default"))
           (n (anvil-manifest--coerce-limit
               limit anvil-manifest-docstring-candidate-limit))
           ;; Over-fetch before query filtering so weak descriptions with
           ;; sparse query vocabulary are not lost too early.
           (raw (anvil-autoresearch-docstrings nil sid (* n 5)))
           (rows nil))
      (dolist (candidate (append (plist-get raw :candidates) nil))
        (let* ((tool (anvil-manifest--candidate-tool candidate sid))
               (attention (if tool
                              (anvil-manifest--attention-score
                               q (plist-get candidate :id) tool)
                            0)))
          (when (anvil-manifest--docstring-candidate-match-p
                 candidate q attention)
            (let ((row (copy-sequence candidate)))
              (setq row
                    (plist-put row :attention-query (or q "")))
              (setq row (plist-put row :attention-score attention))
              (setq row
                    (plist-put row :manifest-score
                               (+ (or (plist-get candidate :score) 0)
                                  attention)))
              (setq row
                    (plist-put row :rewrite-tool
                               "autoresearch-docstring-propose"))
              (setq row
                    (plist-put row :rewrite-args
                               (list :tool-id
                                     (plist-get candidate :id)
                                     :server-id
                                     (plist-get candidate :server-id))))
              (setq row
                    (plist-put
                     row :suggested-action
                     "propose-docstring-rewrite-for-tool-attention"))
              (push row rows)))))
      (setq rows
            (sort rows
                  (lambda (a b)
                    (let ((sa (plist-get a :manifest-score))
                          (sb (plist-get b :manifest-score)))
                      (if (= sa sb)
                          (string< (plist-get a :id)
                                   (plist-get b :id))
                        (> sa sb))))))
      (setq rows (cl-subseq rows 0 (min (length rows) n)))
      (list :query (or q "")
            :server-id sid
            :resolved-server-id (anvil-server--resolve-id sid)
            :limit n
            :count (length rows)
            :candidates (apply #'vector rows)))))

;;; Lifecycle

(defconst anvil-manifest--tool-specs
  `((,#'anvil-manifest-cost-handler
     :id "manifest-cost"
     :intent '(meta token)
     :layer 'dev
     :description "Return the active ANVIL_PROFILE, count of advertised tools, and an approximate token cost of the current tools/list manifest.  Handlers of hidden tools remain callable via explicit tools/call."
     :read-only t
     :title "Manifest Cost")
    (,#'anvil-manifest-attention-handler
     :id "manifest-attention"
     :intent '(meta discovery token)
     :layer 'dev
     :description "Return Doc 44 Tool Attention IsO-lite ranking for a natural-language query over registered MCP tool ids, descriptions, intents, and layers.  Use before choosing a narrow dynamic manifest profile; read-only and local, no embeddings service required."
     :read-only t
     :title "Manifest Attention")
    (,#'anvil-manifest-recovery-suggest-handler
     :id "manifest-recovery-suggest"
     :intent '(meta discovery token)
     :layer 'dev
     :description "Return Doc 44 recovery suggestions when a dynamic manifest profile hides or trims the tool you expected.  Scores registered tools for a natural-language query, reports visibility/focus/precondition status, and shows warm LRU tools.  Read-only."
     :read-only t
     :title "Manifest Recovery Suggest")
    (,#'anvil-manifest-docstring-candidates-handler
     :id "manifest-docstring-candidates"
     :intent '(meta discovery research)
     :layer 'dev
     :description "Return Doc 44 Phase 2d docstring rewrite candidates for Tool Attention quality.  Uses Doc 43 autoresearch-docstrings to rank weak MCP tool descriptions, augments each row with local attention score for the optional query, and points follow-up at autoresearch-docstring-propose."
     :read-only t
     :title "Manifest Docstring Candidates"))
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
  (unless (eq anvil-server-tool-fragment-function
              #'anvil-manifest--tool-fragment)
    (setq anvil-manifest--previous-tool-fragment-function
          anvil-server-tool-fragment-function)
    (setq anvil-server-tool-fragment-function
          #'anvil-manifest--tool-fragment))
  (add-hook 'anvil-server-tool-dispatch-hook
            #'anvil-manifest--record-tool-dispatch)
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
  (when (eq anvil-server-tool-fragment-function
            #'anvil-manifest--tool-fragment)
    (setq anvil-server-tool-fragment-function
          anvil-manifest--previous-tool-fragment-function))
  (remove-hook 'anvil-server-tool-dispatch-hook
               #'anvil-manifest--record-tool-dispatch)
  (dolist (alias anvil-manifest--default-aliases)
    (setq anvil-server-id-aliases
          (delete alias anvil-server-id-aliases)))
  (anvil-server-unregister-tools "default" anvil-manifest--tool-specs))

(provide 'anvil-manifest)
;;; anvil-manifest.el ends here
