;;; anvil.el --- An Emacs MCP server — your AI's workbench -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; Author: zawatton
;; Keywords: comm, tools, ai, mcp
;; Version: 0.2.0
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
- `http'      — HTTP client via `url-retrieve-synchronously' with a
                state-backed ETag/TTL cache (Doc 09 Phase 1a)"
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
