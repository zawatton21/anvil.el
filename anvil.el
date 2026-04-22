;;; anvil.el --- An Emacs MCP server — your AI's workbench -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; Author: zawatton
;; Keywords: comm, tools, ai, mcp
;; Version: 0.3.1
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
                git HEAD mismatch detection
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
                the primitive set.  Phase 3 hooks (PreCompact,
                SessionStart, PostToolUse, UserPromptSubmit,
                SessionEnd) and the anvil-hook install command ship
                under the same module (Doc 17, requires `state')
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
- `treesit'   — Tree-sitter shared core for per-language structural
                editing modules (Doc 21).  Registers no tools
                itself; language modules depend on it.
- `py'        — Python read-only structural locators: py-list-imports
                / py-list-functions / py-list-classes / py-list-methods
                / py-list-decorators / py-find-definition /
                py-surrounding-form.  Requires Emacs 29+ treesit and
                tree-sitter-python grammar (Doc 21 Phase 1a)
- `manifest'  — Per-session tools/list filter driven by
                `ANVIL_PROFILE' (ultra / nav / core / lean / full).
                Handlers remain live regardless of profile; only the
                advertised manifest shrinks.  Opt-in: primarily useful
                for orchestrator child sessions where per-session
                manifest cost dominates (Doc 26 Phase 1).  Add
                `manifest' last in the module list so it sees every
                earlier registration.
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
                DRAFT."
  :type '(repeat symbol)
  :group 'anvil)

(defcustom anvil-server-id "anvil"
  "Server ID used for MCP tool registration.
All anvil tools are registered under this server ID."
  :type 'string
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

(provide 'anvil)
;;; anvil.el ends here
