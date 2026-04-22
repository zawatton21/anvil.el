# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.4.1] - 2026-04-22

Two small fixes released same-day as v0.4.0, both reached the repo
after the v0.4.0 tag was pushed.

### Fixed

- **Release-audit false positive on `anvil-discovery.el`** — four
  private accessors (`--tool-intent` / `--tool-layer` /
  `--tool-stability` / `--tool-summary`) triggered the MCP-tool
  wrapper regex and failed the release-audit gate on master,
  leaving the v0.4.0 CI build red.  Renamed to `-of` suffixes
  (`--intent-of` / `--layer-of` / `--stability-of` / `--summary-of`)
  so the audit no longer treats them as MCP tools.  No public API
  change; the actual `anvil-tools-by-intent` / `anvil-tools-
  usage-report` surfaces are unchanged.
- **`treesit_info` parser initialisation** (@yours57) — the tool
  ran through the TreeSitter node walker without creating a parser
  first, so callers got a stale / empty info payload on buffers
  where treesit hadn't been warmed.  Added `treesit-parser-create`
  + integer-param parse helper + autoload for
  `anvil-treesit-language-for-file` / `-ensure-grammar`.

## [0.4.0] - 2026-04-22

AI agent workbench release.  Fourteen new design documents (Doc 21-34)
worth of primitives turn anvil from a tool-per-task module collection
into a cohesive agent development platform: memory engine, intent-based
tool discovery + profile filter, structural edits for Python / TS / JS /
Elisp-CST, shell output compression, session snapshot + Claude Code
lifecycle hooks.

Test suite: **640 → 1198 passing (+558, +87%)**.  20 new `anvil-*.el`
modules.  +39,343 / -408 lines across 145 files.

### Added — AI agent core

- **`memory` module (Doc 29)** — Bayesian + TTL + FTS5 auto-memory
  engine with 16 MCP tools (`memory-scan` / `memory-audit` /
  `memory-access` / `memory-list` / `memory-search` / `memory-duplicates` /
  `memory-promote` / `memory-serve-start` etc.).  Indexes
  `~/.claude/projects/*/memory/*.md` into SQLite, tracks contradictions
  and URL liveness, surfaces stale rows to `memory-pruner`.  +115 ERT.
- **`session` module (Doc 17)** — session snapshot / resume plus 5
  Claude Code lifecycle hooks (PreCompact / SessionStart / PostToolUse
  / UserPromptSubmit / SessionEnd).  `session-snapshot` captures branch
  + task summary into anvil-state ns=session (14-day TTL), returns a
  `preamble-suggested` resume block; `anvil-hook install` wires the
  hook set into `~/.claude/settings.json`.  +19 ERT.
- **`shell-filter` module (Doc 27)** — shell output compression with
  20 bundled filters (git status / git log / git diff / rg / find /
  pytest / ert-batch / emacs-batch / make / docker-logs / …) + tee +
  gain statistics.  Raw output stashed under `shell-tee` namespace with
  TTL so callers can recover on demand.  Depends on `state`.
- **`disclosure` module (Doc 28)** — 3-layer read contract and citation
  URI scheme.  `file-outline` / `org-index-index` / `defs-index` /
  `journal-index` / `http-cache-index` (Layer 1), `file-read-snippet`
  (Layer 2), `file-read` / `org-read-headline` / `org-read-by-id` /
  `elisp-get-function-definition` / `http-cache-get` (Layer 3).
  `disclosure-help` tool prints the contract; `anvil-uri-fetch` is a
  cross-layer resolver.
- **`discovery` module (Doc 34)** — intent-based MCP tool discovery
  (Phase A) + `agent` / `edit` intent-based profiles (Phase B) +
  orchestrator auto-injection + per-tool usage counter + release-audit
  `:unused-since` scanner (Phase C).  `anvil-tools-by-intent` returns
  tools matching intent CSV / layer / query regex, sorted by layer rank
  and intent overlap.  All 198 registered tools tagged with `:intent` /
  `:layer` / `:stability` metadata.  +27 ERT across discovery / manifest
  / orchestrator / dev.

### Added — Token efficiency

- **`manifest` module (Doc 26)** — per-session `tools/list` profile
  filter driven by `ANVIL_PROFILE`.  Five legacy profiles (`ultra` /
  `nav` / `core` / `lean` / `full`) advertise curated tool subsets to
  shrink the manifest token cost; handlers of hidden tools stay
  callable via explicit `tools/call`.  Phase 1b auto-injects
  `--mcp-config` into orchestrator child sessions.
- **Intent-based profiles** (Doc 34 Phase B) — `agent` (orchestrator /
  session / memory / browser + edit tools, layer=core+workflow) and
  `edit` (file / org / code / json / db only, layer=core).  Filter by
  metadata instead of hand-curated ID lists.

### Added — Language-aware structural edits

- **`sexp` module (Doc 12 Phase 1+2)** — reader-based edits for Elisp:
  `sexp-read-file` / `sexp-surrounding-form` / `sexp-rename-symbol` /
  `sexp-replace-call` / `sexp-replace-defun` / `sexp-wrap-form` /
  `sexp-macroexpand` / `sexp-verify`.  +48 ERT.
- **`sexp-cst` module (Doc 31)** — tree-sitter-elisp CST + runtime
  `inspect-object` tool for any live Lisp value.  `sexp-cst-read`
  (comment-preserving CST), `sexp-cst-edit` + `-write` (point-offset
  replacement with re-parse validation), `sexp-cst-repair` (paren +
  unterminated-string balancing).  +47 ERT.
- **`py` module (Doc 21 Phase 1)** — Python structural locators and
  edits via treesit: `py-list-imports` / `py-list-functions` /
  `py-list-classes` / `py-list-methods` / `py-list-decorators` /
  `py-find-definition` / `py-surrounding-form` plus edit tools
  `py-add-import` / `py-remove-import` / `py-rename-import` /
  `py-replace-function` / `py-wrap-expr`.  +55 ERT.
- **`ts` + `js` modules (Doc 21 Phase 2)** — TS/TSX + JS/JSX locators:
  `ts-list-imports/exports/functions/classes/methods/interfaces/
  type-aliases/find-definition/surrounding-form` and the `js-*`
  mirror.  +25 ERT (ts) + ts-test fixtures.
- **`defs` module (Doc 11)** — SQLite-backed Elisp symbol index.
  `defs-search` / `defs-references` / `defs-signature` /
  `defs-who-requires` / `defs-index-rebuild` / `defs-index-status`.
  +52 ERT.

### Added — Developer workflow

- **`bench` module (Doc 14)** — `bench-compare` / `bench-profile-expr`
  / `bench-last`.
- **`bisect` module (Doc 13)** — test-driven git bisect via worktree-
  isolated `emacs --batch`.  `bisect-test` pins a failing ERT to the
  introducing commit.  +12 ERT.
- **`git-msg` module (Doc 15)** — `git-commit-message` (from staged
  diff) and `git-pr-body` (from branch log).  +17 ERT.
- **`lint` module (Doc 16)** — repo hygiene scanner with pluggable
  registry: `conflict-markers` (error), `orphan-ids` (info),
  `broken-scheduled` (warning).  +13 ERT.
- **`data` module (Doc 33)** — JSON path-based edits with preview-by-
  default: `data-get-path` / `data-set-path` / `data-delete-path` /
  `data-list-keys` for `~/.claude.json`, `package.json`, MCP configs.
  +28 ERT.

### Added — Orchestrator polish

- **Provider latency routing (Doc 22)** — `orchestrator-routing-select`
  for per-provider latency-aware dispatch.  +13 ERT.
- **Consensus presets (Doc 23)** — named provider combinations ship
  with `orchestrator-consensus-*` family.  +16 ERT.
- **Orchestrator submit-and-collect** — one-shot dispatch + wait
  combinator for programmatic callers.
- **Preamble management** — `orchestrator-preamble-set/-get/-list/
  -delete/-set-from-file` for reusable system prompts.
- **Live streaming** — `orchestrator-stream` + `orchestrator-tail`
  forward provider stdout as MCP events while the task runs.
- **Cross-session stats** — `orchestrator-stats` aggregates batch
  history across daemon restarts via anvil-state.

### Added — Tool discovery and counters

- `anvil-tools-by-intent` — intent CSV / layer / query regex / stability
  filter, deprecated always hidden, experimental opt-in, layer-ranked
  output.
- `anvil-tools-usage-report` — per-tool counter summary (days
  threshold, never-called bucket, unused-since bucket, recency
  sort).
- `anvil-dev-release-audit` gains `:unused-since N` scanner —
  advisory-only Phase C hazard: does not flip `:clean-p`.

### Changed

- **`anvil-server-encode-handler` internals** — replaced `eval +
  constructed lambda` with `make-symbol + fset + apply-partially +
  symbol-property` (`anvil-server-raw-handler` /
  `anvil-server-encode-result`).  `anvil-server-register-tool`
  normalizes wrappers back to the raw handler for schema generation
  and docstring validation.  Fixes silent `enable` skip for `sexp` /
  `py` / `bench` / `git-msg` optional modules (PR #12, @yours57).
- **`git` MCP handlers** — sentinels `:null` / `:empty-array` →
  literal strings `"null"` / `"[]"` (visible wire-format change).
  All 8 git tools now wrapped via `anvil-server-encode-handler` at
  registration; 6 dedicated ERT.
- **Worker server files** — path moved from
  `user-emacs-directory/server/` to `server-auth-dir` /
  `server-socket-dir`; liveness check uses `server-running-p` for
  local sockets, PID parsing for TCP auth files.
- **`anvil-server-tool-filter-function`** — now receives
  `(TOOL-ID TOOL-PLIST SERVER-ID)` so filters can branch on metadata.
  `anvil-manifest` uses the full signature for intent-based filtering.
- **`anvil-server-tool-dispatch-hook`** (new abnormal hook) — runs
  after each successful handler return with `(TOOL-ID SERVER-ID)`;
  used by `anvil-discovery` to maintain usage counters without
  coupling `anvil-server` to `anvil-state`.

### Fixed

- **Encoded handler registration bug** — `anvil-server-encode-handler`
  previously returned `(lambda (&rest args) ...)` which
  `anvil-server--generate-schema-from-function` rejected; silently
  skipped `enable` for `sexp` / `py` / `bench` / `git-msg` and made
  every re-register via `unload-feature` break.  Fixed via the
  symbol-backed wrapper path above (PR #12).
- **JSON encoder on dotted pairs** — `anvil-server--to-json-value`
  crashed with `listp` error on alist entries like `(cons "k" 1.0)`.
  New `anvil-server--list-to-json-array` emits `[car, cdr]` for
  improper lists so `mapcar` can't trip.
- **`xref_find_apropos`** — missing `(require 'apropos)` made the
  tool void-function on fresh Emacs installs.
- **Worker liveness** — Unix-socket workers wrongly reported alive
  via file existence alone; now goes through `server-running-p`.

### Documentation

- Fourteen new design docs (Doc 21-34).  Six docs (18 / 19 / 20 / 24
  / 25 / 30) explicitly DEFERRED — out of scope for this release.
  Doc 32 is an audit memo (rhblind/emacs-mcp-server positioning,
  informational only).
- `CLAUDE.md` selection flowchart trimmed (11 rows → 5) — discoverable
  entries moved to `anvil-tools-by-intent` runtime query; only
  size/shape heuristics kept.
- Platform files (`linux.md` / `windows.md`) gained a `tool 探索`
  section documenting `ANVIL_PROFILE=agent` / `edit` usage.

## [0.3.1] - 2026-04-19

Hotfix surfaced by the v0.3.0 orchestrator benchmark
([develop: `benchmarks/results/report-2026-04-19.org`](https://github.com/zawatton/anvil.el/blob/develop/benchmarks/results/report-2026-04-19.org)).
The bench's first programmatic `orchestrator-*` call hit the
cons-return bug; every fix here fell out of the investigation.

### Fixed

- **`orchestrator` MCP wrappers returned plists instead of strings**,
  tripping the `anvil-server` contract and breaking every
  programmatic `orchestrator-*` call from Claude Code / OpenCode /
  other MCP clients with `"Tool handler must return string or nil,
  got: cons"`. Added `anvil-orchestrator--encode-handler` which
  wraps each registered tool at registration time and JSON-encodes
  the plist result. Tool bodies stay unchanged (and still return
  rich plists for direct Elisp / ERT callers).
- Added `anvil-orchestrator--batch-task-ids` accessor + docstring
  note on the `anvil-orchestrator--batches` /
  `anvil-orchestrator--consensus-groups` shape asymmetry, so future
  callers stay decoupled from the storage representation.

### Changed

- Raised the `codex` per-provider concurrency default from **3 → 6**.
  The v0.3.0 ramp benchmark reached 8 concurrent `codex` jobs on
  ChatGPT Plus OAuth with zero 429s; the old default was defensive
  past its evidence. Users on throttled accounts can set it back
  via `anvil-orchestrator-per-provider-concurrency`.

## [0.3.0] - 2026-04-19

Major release covering 10 design documents worth of new primitives:
orchestrator (Doc 10), browser framework (Doc 07), disk-first helpers
(Doc 05), code-transform tools (Doc 06), state KV store (Doc 08), HTTP
client (Doc 09), pty-broker (Doc 04), worker pool v2 (Doc 01), org
index (Doc 02), offload framework (Doc 03). Test suite grew from 292
to 508 passing.

### Added

- **`orchestrator` module (Doc 10)** — parallel AI CLI dispatcher.
  - 5 native providers: `claude`, `aider`, `gemini`, `ollama`, `codex`
    (OpenAI ChatGPT Plus OAuth).
  - Pool with per-provider concurrency caps, state-persisted queue,
    tabulated-list dashboard, git worktree isolation.
  - `:depends-on` DAG with cycle / unknown-name detection and
    propagating failure semantics. DAG resume across daemon restart
    via `orchestrator-resume-interrupted`.
  - Cross-model consensus (Jaccard verdict) and meta-LLM judge
    (synthesized answer via 3rd provider).
  - Auto-retry with exponential backoff + jitter, `Retry-After`
    honouring, per-provider backoff overrides.
  - Live streaming events (Phase 7c) — per-event callbacks without
    SQLite writes, overflow compression.
  - Context preamble registry (Phase 7b) — reusable prompt preludes
    via `:preamble-ref`, auto-resolved at submit.
  - `submit-and-collect` convenience for synchronous fan-out,
    `submit-one` / `extract-result` / `tail` glue helpers.
  - Observability: `orchestrator-stats` with percentile / provider
    filter, `anvil-orchestrator-stats-dashboard` command.
  - `anvil-cron` integration (Phase 2b) via `anvil-cron-register :fn`
    + `examples/nightly-orchestrator.org`.
  - gemma4 thinking-mode guard (XML `<think>` and plain-text
    delimiter).
- **`browser` module (Doc 07)** — agent-browser wrapper.
  - Phase A: `browser-fetch` / `-interact` / `-capture` /
    `-screenshot` / `-close` MCP tools with in-memory cache +
    metrics + `M-x anvil-browser-status`.
  - Phase A': auth primitives (`profile`, `user-agent`,
    `auto-connect`, `session-presets`) for login-walled / anti-bot
    sites.
- **`pty-broker` module (Doc 04 Phase 1)** — Node-pty TCP broker +
  Elisp client + 5 MCP tools, structurally avoids filter-starvation
  and ConPTY headless-stdin limitations.
- **`state` module (Doc 08)** — SQLite-backed KV store with
  namespaces, TTL, and `prin1` value serialization. `anvil-browser`
  cache now persists across daemon restarts.
- **`http` module (Doc 09 Phase 1a)** — `http-fetch` / `-head` /
  `-cache-purge` with ETag/TTL cache via `anvil-state`.
- **`org-index` (Doc 02 Phase 1-4 + 5a)** — SQLite org index,
  filenotify watcher + periodic scan, 177× speed-up on 1500 files /
  71k headlines; `org-read-*` routed through index.
- **Disk-first helpers (Doc 05)** — `:warnings` on every mutating
  `file-*` tool, plus `buffer-read` / `buffer-save` /
  `buffer-list-modified` MCP tools.
- **Code-transform tools (Doc 06)** — `code-extract-pattern`
  (read-only structured block extraction) and
  `code-add-field-by-map` (TS/JS object literal bulk field add for
  i18n).
- **Worker pool v2 (Doc 01)** — 3-lane split with `:kind`
  classifier, batch warmup, latency metrics, optional
  `server-eval-at` transport.
- **Offload framework (Doc 03)** — pipe REPL + pool, `:offload t`,
  hard-kill, handler-side checkpoint for `:value` / `:cursor`
  persistence.
- **`anvil-dev` tools** — `anvil-dev-release-audit` (3 scanners:
  Emacs 30 `_args` arglist-strip hazard, missing
  `MCP Parameters:` docstring sections, non-SHIPPED design docs),
  `anvil-dev-test-run-all :minimal`, `anvil-dev-self-sync-check`,
  `anvil-dev-journal-memo-append`, `anvil-scaffold-module`.
- **Efficiency bundle** — `anvil-server-register-tools` bulk
  registration, `anvil-orchestrator-preamble-set-from-file`,
  `anvil-orchestrator-dashboard-autofollow`,
  `anvil-dev-release-audit :scope`.
- **`anvil-git` extensions** — worktree helpers +
  `git-branch-current` / `git-head-sha` / `git-repo-root` /
  `git-worktree-list` MCP tools.

### Changed

- `anvil-orchestrator-summary-max-chars` default raised 300 → 4000 to
  eliminate the common re-parse round-trip; pass `:full t` to
  `orchestrator-extract-result` for the unbounded form.
- No-arg MCP wrapper convention: write `()` not `(_args)`. Emacs 30
  `help-function-arglist` strips `_`-prefix, which broke MCP schema
  registration for worker, cron, orchestrator, browser, and other
  modules — all audited and fixed.

### Fixed

- `orchestrator`: close stdin after spawn to prevent ollama CLI hang
  (`31d2f3f`).
- `orchestrator`: deep-copy task plist in `submit` so consensus
  fan-out does not share tails — previously caused `:cost-usd` /
  `:elapsed-ms` clobber across providers (`5a2610f`).
- `orchestrator`: gemma4 thinking-strip now handles the plain-text
  `Thinking... ...done thinking.` delimiter in addition to XML
  `<think>` blocks (`86a0e7c`).
- Windows stdio CR robustness — tools/list decode reliably on
  MSYS2 + gawk 5.0.0 + coreutils 8.32 (issue #6).
- Emacs 30 `help-function-arglist` underscore-strip regressions
  across worker, cron, orchestrator (issue #9, v0.2.1) + remaining
  no-arg wrappers audited via `anvil-release-audit`.

## [0.2.1] - 2026-04-18

### Fixed

- Worker + cron module load failure on Emacs 30 due to
  `help-function-arglist` stripping `_args` prefix (issue #9).

## [0.2.0] - 2026-04-17

Initial tagged release — see git history for details.

## [0.1.0] - 2026-04

Project inception.

[0.3.1]: https://github.com/zawatton21/anvil.el/releases/tag/v0.3.1
[0.3.0]: https://github.com/zawatton21/anvil.el/releases/tag/v0.3.0
[0.2.1]: https://github.com/zawatton21/anvil.el/releases/tag/v0.2.1
[0.2.0]: https://github.com/zawatton21/anvil.el/releases/tag/v0.2.0
[0.1.0]: https://github.com/zawatton21/anvil.el/releases/tag/v0.1.0
