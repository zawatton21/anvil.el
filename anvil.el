;;; anvil.el --- An Emacs MCP server — your AI's workbench -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; Author: zawatton
;; Keywords: comm, tools, ai, mcp
;; Version: 1.3.0
;; Package-Requires: ((emacs "28.2"))
;; URL: https://github.com/zawatton21/anvil.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Anvil turns Emacs into an AI-ready workbench via the Model Context
;; Protocol (MCP).  It exposes file editing, org-mode operations, system
;; inspection, and Elisp evaluation as MCP tools that any LLM agent can
;; call — Claude, GPT, local models, or anything that speaks MCP.
;;
;; Key features:
;; - Safe file operations that handle large files (1.2MB+) without data loss
;; - Rich org-mode integration (read, write, refactor headings)
;; - System inspection: git, processes, network, filesystem
;; - Sync and async Elisp evaluation
;; - IDE tools: xref, diagnostics, imenu, tree-sitter
;; - Optional modules: Excel (openpyxl), PDF (pymupdf), cron scheduler
;;
;; Quick start:
;;   (require 'anvil)
;;   (anvil-enable)
;;
;; Then register the MCP server with your AI client using:
;;   M-x anvil-describe-setup
;;
;; See https://modelcontextprotocol.io/ for the MCP specification.

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup anvil nil
  "Anvil — Emacs MCP server for AI agents."
  :group 'tools
  :prefix "anvil-")

(defcustom anvil-modules
  '(worker eval org file host git proc fs emacs text clipboard data net)
  "List of anvil modules to load when `anvil-enable' is called.
Each symbol corresponds to an `anvil-SYMBOL.el' file that provides
a set of MCP tools.  Modules are loaded in the order listed.
The `worker' module should be first — it spawns an isolated sub-Emacs
so AI tool calls never freeze the human's editor."
  :type '(repeat symbol)
  :group 'anvil)

(defcustom anvil-optional-modules nil
  "Optional anvil modules requiring external dependencies.
These are not loaded by default.  Available modules:
- `xlsx'   — Excel read/write (requires Python + openpyxl)
- `pdf'    — PDF extraction (requires Python + pymupdf)
- `ide'    — IDE tools: xref, diagnostics (requires project.el)
- `cron'   — Scheduled task runner with worker dispatch
- `sqlite'    — Read-only SQLite query tool (requires Emacs 29+)
- `elisp'     — Elisp development tools: ERT runner, byte-compile, describe
- `sexp'      — Reader-based structural edits for elisp: sexp-read-file,
                sexp-surrounding-form, sexp-replace-defun, sexp-wrap-form,
                sexp-macroexpand, sexp-verify (Doc 12 Phase 1)
- `org-index' — Persistent SQLite index of org files (requires Emacs 29+)
- `buffer'    — Explicit buffer-* MCP tools (read/save/list-modified)
- `dev'       — Developer helpers: `anvil-self-sync-check' for dev/installed
                git HEAD mismatch detection, `anvil-codex-efficiency-check'
                for Codex token-saving setup audits, and
                `anvil-claude-limits-analyze' for Claude Code limits
                report triage
- `offload'   — Future-based API for running heavy elisp in a batch
                subprocess (Doc 03 Phase 1)
- `browser'   — agent-browser CLI wrapper: fetch / interact / capture
                / screenshot (requires `agent-browser' on PATH,
                Doc 07 Phase A)
- `state'     — Persistent SQLite-backed KV store shared across modules
                (ns / TTL / Lisp prin1 values, requires Emacs 29+,
                Doc 08 Phase 1)
- `session'   — Session snapshot / resume + Claude Code lifecycle
                hook dispatch.  `session-snapshot' captures branch
                + task-summary + notes into anvil-state ns=session
                (TTL 14d) and returns a `preamble-suggested' resume
                block; `session-resume' / -list / -delete round out
                the primitive set.  Phase 3 hooks (PreCompact, Stop,
                SessionStart, PostToolUse, UserPromptSubmit,
                SessionEnd) and the anvil-hook install command ship
                under the same module (Doc 17, requires `state')
- `compact'   — Autonomous /compact orchestration (Doc 36 Phase 1).
                Aggressive auto-compact layer: Stop hook triggers a
                segment-boundary snapshot + nudge when transcript
                crosses a configurable threshold (default 45%,
                cooldown 25%); UserPromptSubmit emits a JSON
                additionalContext telling the model to invoke
                /compact on the next turn; SessionStart restores
                the parked snapshot as preamble.  Complements
                Claude Code's hardcoded ~83.5% auto-compact with a
                user-tunable earlier trigger for long autonomous
                sessions.  Requires `state' and integrates with
                `session' (Stop event).  Seven MCP tools: compact-
                estimate / -should-trigger / -snapshot / -restore /
                -hook / -stats / -pressure-report.
- `harness-telemetry' — Doc 46 Phase 1 runtime-harness failure
                classifier + 4-class SQLite telemetry (no-exec /
                contract-violation / stall / reasoning).  Hooks
                into `anvil-server-tool-error-hook' (raised from
                inside `anvil-server-with-error-handling' and the
                tools/call dispatcher) to classify every failure
                with a static rule table and persist the event
                into a `harness_failures' table inside the
                anvil-worklog DB file.  Three MCP tools —
                harness-telemetry-record / -stats / -recent —
                expose the recorder + aggregator queries so
                later modules (Doc 43-47) can measure their
                effect against a 1-2 week baseline.  Requires
                Emacs 29+ for SQLite; soft-deps on
                `anvil-worklog' for the DB path resolver.
- `autoresearch' — Doc 43 Phase 0 read-only candidate scanner for
                local web / X / YouTube summaries and anvil design
                docs.  Adds `autoresearch-scan', which returns
                ranked implementation candidates with file, line,
                evidence, source kind, score, and suggested action.
                Phase 1a adds `autoresearch-docstrings', a read-only
                evaluator that ranks weak MCP tool description /
                parameter-doc targets for Q1-A docstring
                autoresearch.  Phase 1b adds
                `autoresearch-docstring-plan', a read-only planner
                that returns the current description, scored issues,
                registration-site hint, deterministic draft
                description, and rewrite prompt.  Phase 1c adds
                `autoresearch-docstring-patch', a read-only dry-run
                patch candidate generator for the registered
                `:description' string.  Phase 1d adds
                `autoresearch-docstring-apply', a local deterministic
                apply/eval loop for one description literal: apply
                reviewed rewrite, run optional eval command, and
                revert automatically on eval failure.  Phase 1e adds
                `autoresearch-docstring-propose', which submits one
                bounded orchestrator task with budget, timeout,
                heartbeat timeout, cwd, and sandbox metadata to
                propose a replacement description without applying it.
                Phase 2 adds `holdout_command' to
                `autoresearch-docstring-apply': after a kept inner
                eval, a failing holdout command retroactively restores
                the original file.  Phase 2a adds
                `autoresearch-docstring-run', which tries multiple
                candidate descriptions against eval / holdout gates
                and keeps only the lowest issue-score passing rewrite.
                Phase 2b adds `autoresearch-docstring-harvest', which
                collects successful orchestrator proposal summaries
                and feeds them into that multi-candidate gate.
                Phase 2c adds `autoresearch-docstring-propose-batch',
                which fan-outs N bounded proposal tasks in one
                orchestrator batch for direct harvest.  Phase 2d adds
                explicit `no_worktree' / `worktree_name' controls so
                Doc 43 proposal tasks can opt out of, or name,
                orchestrator worktree isolation.  Phase 2e adds
                `autoresearch-docstring-promote-memory', which stores
                accepted run / harvest results as DB-direct
                anvil-memory rows.  Phase 3 adds
                `autoresearch-routing-evaluate' and
                `autoresearch-routing-plan' for read-only Q1-B
                orchestrator routing autoresearch.  Phase 3a adds
                `autoresearch-routing-propose' to submit one bounded
                routing proposal task without mutating routing
                settings.  Phase 3b adds
                `autoresearch-routing-harvest' to collect and rank
                routing proposal summaries for review without applying
                them.  Phase 3c adds
                `autoresearch-routing-patch-plan' to turn a selected
                proposal into target-file hints, an implementation
                prompt, and verification commands without editing
                source.  Phase 3d adds
                `autoresearch-routing-implement' to submit that
                implementation prompt as one bounded orchestrator task.
                Phase 3e adds
                `autoresearch-routing-implementation-harvest' to
                collect implementation summaries and return a selected
                review candidate plus checklist.  Phase 3f adds
                `autoresearch-routing-implementation-promote-memory'
                to store accepted implementation summaries as
                DB-direct anvil-memory rows.  Phase 5 adds
                `autoresearch-nelisp-codegen-plan',
                `autoresearch-nelisp-codegen-implement', and
                `autoresearch-nelisp-codegen-harvest' to apply the
                same bounded implementation / harvest pattern to
                NeLisp Phase 47 codegen without editing NeLisp from
                the MCP tool itself.  Phase 6 adds
                `anvil-autoresearch-dashboard', a tabulated-list UI
                over current `autoresearch-*' orchestrator tasks.
- `claude-watchdog' — Detect Claude Code (Anthropic CLI) TUI deadlock
                via /proc + jsonl mtime polling.  When wchar stops
                advancing while State=R and the session jsonl goes
                stale, raises a desktop notification + records the
                event under `anvil-state' ns=claude-watchdog-events.
                Phase 1c also detects the State=S spinner variant
                where tracked MCP child processes have zero rchar /
                wchar delta and emits `:hang-variant' through the
                same dispatch path.
                One MCP tool `anvil-claude-watchdog-recent' for
                debugging.  Manual recovery is available as
                `M-x anvil-claude-watchdog-recover': confirmation
                gated SIGKILL plus tmux staging of
                `claude --resume <session-id>' without pressing
                Enter.  Linux only (procfs); requires `state'
                (Doc 40 Phase 1 + 1c + 2).
- `http'      — HTTP client via `url-retrieve-synchronously' with a
                state-backed ETag/TTL cache (Doc 09 Phase 1a)
- `orchestrator' — Parallel AI CLI dispatcher (claude today, more
                   providers in Phase 2+) with concurrency-capped
                   queue, state-persisted status, and a
                   tabulated-list dashboard (Doc 10 Phase 1a)
- `pty-broker' — node-pty TCP broker for TUI programs; moves PTY
                 handling out of the Emacs daemon to avoid filter
                 starvation / ConPTY stdin quirks.  Phase 2b adds
                 `pty-read-filtered' — streaming read that routes
                 pty output through a named shell-filter handler
                 (docker-logs / pytest / …) with a per-pty tail
                 cursor so consecutive calls see only new bytes.
                 Soft-deps on `shell-filter'; raw text is returned
                 when the filter module is not loaded (Doc 04
                 Phase 1 + Doc 27 Phase 2b, requires node +
                 `npm install node-pty')
- `defs'      — SQLite-backed elisp symbol index (defs, refs,
                requires/provides) with defs-search /
                defs-references / defs-signature etc.
                (Doc 11 Phase 1+2, requires Emacs 29+)
- `bisect'    — Test-driven git bisect that pins a failing ERT
                test to the introducing commit via worktree-
                isolated emacs --batch steps (Doc 13 Phase 1)
- `ide-treesit' — Tree-sitter shared core for per-language structural
                editing modules (Doc 21).  Registers no tools
                itself; language modules depend on it.  Renamed
                from `treesit' in Doc 38 Phase C (= IDE layer
                relocation).
- `py'        — Python read-only structural locators: py-list-imports
                / py-list-functions / py-list-classes / py-list-methods
                / py-list-decorators / py-find-definition /
                py-surrounding-form.  Requires Emacs 29+ treesit and
                tree-sitter-python grammar (Doc 21 Phase 1a)
- `ts'        — TypeScript / TSX read-only structural locators:
                ts-list-imports / ts-list-exports / ts-list-functions
                / ts-list-classes / ts-list-methods / ts-list-interfaces
                / ts-list-type-aliases / ts-find-definition /
                ts-surrounding-form.  Dispatches `.ts' to the typescript
                grammar and `.tsx' to the tsx grammar.  Requires the
                tree-sitter-typescript grammar (Doc 21 Phase 1b).
- `js'        — JavaScript / JSX / MJS / CJS read-only structural
                locators: js-list-imports / js-list-exports /
                js-list-functions / js-list-classes / js-list-methods /
                js-find-definition / js-surrounding-form.  Requires the
                tree-sitter-javascript grammar.  Shares internal
                helpers with `ts' so `.ts' must not be handed to JS
                tools and vice versa (Doc 21 Phase 1b).
- `manifest'  — Per-session tools/list filter driven by
                `ANVIL_PROFILE' (ultra / nav / core / lean / full /
                dynamic).
                Handlers remain live regardless of profile; only the
                advertised manifest shrinks.  Opt-in: primarily useful
                for orchestrator child sessions where per-session
                manifest cost dominates (Doc 26 Phase 1).  Doc 44
                Phase 2a adds `manifest-attention' and the `dynamic'
                profile: Tool Attention ranking over tool id,
                description, intent, and layer metadata, driven by
                `ANVIL_ATTENTION_QUERY', with optional function /
                command embedding backends and lexical fallback.
                Doc 44 Phase 2b adds
                state filtering via tool `:preconditions' and
                `ANVIL_STATE_TAGS'.  Doc 44 Phase 2c adds dynamic
                schema trimming, warm LRU full-schema retention, and
                `manifest-recovery-suggest'.  Doc 44 Phase 2d adds
                `manifest-docstring-candidates', a Doc43 bridge that
                ranks weak MCP descriptions for Tool Attention
                quality and points follow-up at
                `autoresearch-docstring-propose'.  Add `manifest'
                last in the module list so it sees every earlier
                registration.
- `discovery' — Intent-based MCP tool discovery.  Adds the
                `anvil-tools-by-intent' tool that answers
                \"which registered tools match intent X / layer Y?\"
                queries without requiring the caller to read
                CLAUDE.md.  Reads `:intent' / `:layer' /
                `:stability' metadata attached by modules at
                register time; tools without metadata fall back
                to default values and still surface (Doc 34
                Phase A)
- `disclosure' — Layer-1 (slim index) + disclosure-help tools that
                formalise the 3-layer read contract documented in
                docs/design/28-progressive-disclosure.org.  Depends on
                `anvil-org-index' for the org-index-index handler
                (Doc 28 Phase 1)
- `shell-filter' — Per-command shell output compression + tee +
                gain statistics.  Adds MCP tools shell-run,
                shell-filter, shell-tee-get, shell-gain that
                transparently filter verbose stdout (git status /
                git log / git diff / rg / find / ls / pytest /
                ert-batch / emacs-batch / make) before returning
                to the caller.  Raw bytes are stashed under the
                `shell-tee' namespace with a TTL so callers can
                recover the full output on demand.  Requires
                `anvil-state' (Emacs 29+ SQLite).  Doc 27 Phase 1.
- `context'   — Headroom-inspired reversible context compression for
                arbitrary tool outputs / RAG snippets / JSON payloads /
                diffs / logs / code excerpts / prose.  Adds
                context-compress, context-retrieve, and context-stats.
                A local content router picks json/diff/log/code/text
                compressors; raw originals are stored in anvil-state
                under a CCR-style namespace so agents can retrieve full
                context on demand.  Requires `anvil-state' (Emacs 29+).
- `sexp-cst'  — Tree-sitter CST + runtime `inspect-object' tool:
                token-bounded JSON view of any live Lisp value.
                Phase 1a ships 9 core types + record stub + truncation
                cursor + circular-reference typed error.  Phase 1b
                adds `inspect-object-drill' (cursor resolution with
                offset/limit pagination), 4 KB byte-cap enforcement,
                char-table + EIEIO handlers, and `inspect-object-purge'
                for session-end namespace cleanup.  Phase 2a ships
                `sexp-cst-read' — comment-preserving CST of an elisp
                file via tree-sitter-elisp grammar, with depth cap
                and typed errors.  Phase 2b-a adds `sexp-cst-edit'
                (dry-run replacement of the node at a point offset;
                re-parsed for structural integrity before returning
                the new file content).  Phase 2b-b adds
                `sexp-cst-edit-write' — same validation plus a
                timestamped backup copy of the original before the
                patched content lands on disk.  Phase 3a adds
                `sexp-cst-repair' — parinfer-less close-paren
                balancing that appends missing `)' at EOF (or
                prepends `(' at BOF when closes exceed opens) and
                re-parses to verify ERROR nodes are gone; returns a
                dry-run `repaired-content' for the caller to apply
                via a separate write tool.  Phase 3c extends repair
                with an unterminated-string detector: when the scan
                ends inside a `\"'-literal, the missing quote is
                appended before paren balancing runs (Doc 31,
                requires Emacs 29+ and the tree-sitter-elisp grammar
                for Phase 2/3 tools)
- `lint'      — Repo hygiene scanner family.  Phase 1 ships a
                pluggable registry plus three org scanners:
                conflict-markers (error) detects unresolved git
                merge markers, orphan-ids (info) flags `:ID:'
                properties no `[[id:...]]' link points at, and
                broken-scheduled (warning) catches SCHEDULED /
                DEADLINE timestamps with unparseable repeaters.
                Run via `lint' / `lint-scanners' MCP tools or
                `M-x anvil-lint' (Doc 16 Phase 1)
- `memory'    — Auto-memory metadata index + per-type TTL audit +
                access tracker.  Opens a SQLite DB at
                `anvil-memory-db-path' and walks every memory/
                directory under ~/.claude/projects/*/ (or
                `anvil-memory-roots') to index ~.md files with
                inferred type (feedback / project / reference /
                user / memo).  Adds MCP tools memory-scan /
                memory-audit / memory-access / memory-list that
                surface stale rows to the memory-pruner skill
                without touching memory file contents (non-
                destructive).  Doc 29 Phase 1a, requires Emacs
                29+.  Phase 1b (FTS5 + contradiction detection +
                URL HEAD) and Phase 2 (decay + promote) stay
                DRAFT.
- `memory-bridge' — Doc 41 Phase 2c localhost JSON API in front of
                `anvil-memory' plus Doc 42 Phase 3 read-only worklog
                endpoints.  Provides `anvil-memory-bridge-start' /
                -stop / -status / -rotate-token; GET endpoints for
                /memory/search, /memory/list, /memory/get/<name>,
                /memory/events, /memory/health, and
                /memory/effective-db-path;
                guarded write endpoints for /memory/save,
                /memory/access, /memory/scan, /memory/prune, and
                DELETE /memory/<name>; plus /worklog/search,
                /worklog/list, /worklog/get, /worklog/health, and
                /worklog/effective-db-path.  Phase 2c adds read-replica
                mode with /memory/replica/status,
                /memory/replica/pull, and
                `anvil-memory-bridge-replica-pull'.  Phase 3 adds
                configured-origin CORS / OPTIONS preflight and a
                side-load Manifest V3 browser extension under
                extensions/memory-bridge for Claude.ai, ChatGPT, and
                Gemini memory injection.  Phase 4 adds a dependency-free
                static mobile thin client under extensions/mobile-bridge
                with localStorage offline save queue replay.  The module
                only loads the bridge; it does not auto-start the
                listener.
- `memory-obs' — Session lifecycle observation capture.  Records
                Claude Code hook events (session-start, user-prompt,
                post-tool-use, stop, session-end) into a separate
                SQLite + FTS5 store at
                `anvil-memory-obs-db-path'.  Body is redacted for
                common secret patterns and an importance heuristic
                runs at insert time.  Opt-in via
                `anvil-memory-obs-enabled' (default nil); when off,
                the integrated `fboundp' guards in `session' make it
                a complete no-op.  Doc 37 Phase 1, requires Emacs
                29+ and `session'.  Phase 2 (AI compression),
                Phase 3 (3-layer search MCP tools), Phase 4
                (auto-inject), Phase 5 (UI), Phase 6 (promote) and
                Phase 7 (vector) stay DRAFT.
- `extend'    — Claude self-extension SDK scaffold (Doc 35 Phase
                A, DRAFT v0).  Adds the
                `anvil-extend-scaffold' MCP tool: a Claude Code
                session can emit `<NAME>.el' (function body) +
                `<NAME>-test.el' (ERT skeleton) +
                `<NAME>-register.el' (MCP register stub) into
                `anvil-extend-storage-dir' (default
                ~/.anvil-extend/) in a single call, then run
                `anvil-extend-load' / `-test' / `-list' / `-remove'
                from the same session to iterate.  Phase A is
                scaffold-only: hot-reload (Phase B), sandbox
                eval (Phase C), rationale auto-record (Phase D),
                NeLisp execute path (Phase E), and ephemeral /
                permanent promotion (Phase F) stay DRAFT until
                Doc 35 reaches LOCKED.
- `wl'        — Wanderlust-oriented Maildir tools.  Adds wl-search /
                wl-list-mails / wl-read-mail / wl-compose-draft /
                wl-send over a locally synced Maildir.  Install the
                `wanderlust' package for its WL UI and FLIM/SEMI MIME
                support; `bin/anvil emacs-package-install-wanderlust'
                installs it into the package dir used by
                `anvil mcp serve'.  Enable the tools with
                `anvil mcp serve --module=wl' or
                ANVIL_OPTIONAL_MODULES=wl.
- `semantic'  — Local search over a configurable corpus of org / text
                / code files, backed by built-in SQLite FTS5 (trigram
                tokenizer for CJK substring matching).  Pure-Lisp and
                fully local by default — content stays local; only the
                caller reads excerpts.  Five MCP tools: `semantic-search'
                (hybrid BM25 + lexical + optional embedding cosine, RRF),
                `notes-lexical-search' (term-overlap + synonym expansion
                for retrieve-then-read), `semantic-reindex',
                `semantic-embed-index' and `semantic-status'.  Retrieval
                unions an FTS5 MATCH pass (>=3-char terms; BM25-ranked)
                with a LIKE-scan pass (2-char CJK compounds) so Japanese
                recall is complete.  An opt-in vector layer
                (`semantic-embed-index'; ollama-local by default,
                gemini / openai opt-in cloud) adds true meaning-based
                cosine via a `chunk_vec' table joined on `digest'.  Set
                `anvil-semantic-roots' then run `semantic-reindex' once;
                run `semantic-embed-index' to enable embeddings.  Doc 18;
                requires Emacs 29+."
  :type '(repeat symbol)
  :group 'anvil)

(defcustom anvil-server-id "anvil"
  "Server ID used for MCP tool registration.
All anvil tools are registered under this server ID."
  :type 'string
  :group 'anvil)

(defcustom anvil-modes-allow-buffer-modify nil
  "List of major modes where anvil edits should modify the live buffer.

When a file being edited has at least one open buffer whose major mode
is derived from a mode in this list, anvil's org-modifying tools
(such as `anvil-org--modify') will:

1. Locate the first buffer visiting that file (following indirect
   buffers to their base buffer).
2. Note whether the buffer already has unsaved modifications.
3. Make the edit directly in the live buffer.
4. Only save the buffer if it did *not* have unsaved modifications
   before the edit — preserving any in-progress user edits.

Set this to \='(fundamental-mode) to enable buffer-first editing for
every file open in Emacs, regardless of its major mode."
  :type '(repeat symbol)
  :group 'anvil)

;;; State

(defvar anvil--enabled nil
  "Non-nil when anvil is active and tools are registered.")

(defvar anvil--loaded-modules nil
  "List of currently loaded and enabled module symbols.")

;;; Module loading

(defun anvil--load-module (name)
  "Load and enable anvil module NAME.
NAME is a symbol like `file', `org', `eval', etc.
The corresponding file `anvil-NAME.el' must provide `anvil-NAME'
and define `anvil-NAME-enable' and `anvil-NAME-disable'."
  (let ((feature (intern (format "anvil-%s" name))))
    (require feature)
    (let ((enable-fn (intern (format "anvil-%s-enable" name))))
      (when (fboundp enable-fn)
        (funcall enable-fn)))
    (cl-pushnew name anvil--loaded-modules)))

(defun anvil--unload-module (name)
  "Disable anvil module NAME."
  (let ((disable-fn (intern (format "anvil-%s-disable" name))))
    (when (fboundp disable-fn)
      (funcall disable-fn)))
  (setq anvil--loaded-modules (delq name anvil--loaded-modules)))

;;; Public API

;;;###autoload
(defun anvil-enable ()
  "Start anvil: load modules and register MCP tools.
Loads all modules listed in `anvil-modules' and `anvil-optional-modules'."
  (interactive)
  (dolist (mod anvil-modules)
    (condition-case err
        (anvil--load-module mod)
      (error (message "Anvil: failed to load module '%s': %s" mod err))))
  (dolist (mod anvil-optional-modules)
    (condition-case err
        (anvil--load-module mod)
      (error (message "Anvil: optional module '%s' skipped: %s" mod err))))
  (setq anvil--enabled t)
  (message "Anvil: enabled with modules: %s"
           (mapconcat #'symbol-name anvil--loaded-modules ", ")))

;;;###autoload
(defun anvil-disable ()
  "Stop anvil: unregister all tools and disable modules."
  (interactive)
  (dolist (mod (reverse anvil--loaded-modules))
    (condition-case err
        (anvil--unload-module mod)
      (error (message "Anvil: error disabling module '%s': %s" mod err))))
  (setq anvil--enabled nil)
  (message "Anvil: disabled"))

;;;###autoload
(defun anvil-describe-setup ()
  "Display anvil setup information for configuring AI clients."
  (interactive)
  (with-help-window "*Anvil Setup*"
    (princ "Anvil — Emacs MCP Server\n")
    (princ "========================\n\n")
    (princ (format "Status: %s\n" (if anvil--enabled "ENABLED" "DISABLED")))
    (princ (format "Server ID: %s\n" anvil-server-id))
    (princ (format "Loaded modules: %s\n\n"
                   (if anvil--loaded-modules
                       (mapconcat #'symbol-name anvil--loaded-modules ", ")
                     "(none)")))
    (princ "Loaded modules:\n")
    (if anvil-modules
        (dolist (mod anvil-modules)
          (princ (format "  - %s %s\n" mod
                         (if (memq mod anvil--loaded-modules) "[active]" "[not loaded]"))))
      (princ "  (none configured)\n"))
    (when anvil-optional-modules
      (princ "\nOptional modules:\n")
      (dolist (mod anvil-optional-modules)
        (princ (format "  - %s %s\n" mod
                       (if (memq mod anvil--loaded-modules) "[active]" "[not loaded]")))))))


;;; Buffer-first modify helpers

(defun anvil--buffer-first-viable-p (file-path)
  "Return non-nil if FILE-PATH should be edited in a live buffer.

Returns (BUF . WAS-MODIFIED) where BUF is the base buffer visiting
FILE-PATH and WAS-MODIFIED is t when that buffer had unsaved edits
before this check.  Returns nil when:
- `anvil-modes-allow-buffer-modify' is nil, or
- no buffer visits FILE-PATH, or
- the visiting buffer's major mode does not derive from any mode
  listed in `anvil-modes-allow-buffer-modify'.

When the visiting buffer is an indirect buffer its base buffer is
used instead.

As a special case, `fundamental-mode' matches any mode — set the
option to \='(fundamental-mode) for buffer-first editing everywhere."
  (when anvil-modes-allow-buffer-modify
    (when-let* ((buf (find-buffer-visiting (expand-file-name file-path))))
      (let ((base (or (buffer-base-buffer buf) buf)))
        (when (with-current-buffer base
                (or (memq 'fundamental-mode anvil-modes-allow-buffer-modify)
                    (cl-some (lambda (m)
                               (provided-mode-derived-p major-mode m))
                             anvil-modes-allow-buffer-modify)))
          (cons base (buffer-modified-p base)))))))

(provide 'anvil)
;;; anvil.el ends here
