# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

[0.3.0]: https://github.com/zawatton21/anvil.el/releases/tag/v0.3.0
[0.2.1]: https://github.com/zawatton21/anvil.el/releases/tag/v0.2.1
[0.2.0]: https://github.com/zawatton21/anvil.el/releases/tag/v0.2.0
[0.1.0]: https://github.com/zawatton21/anvil.el/releases/tag/v0.1.0
