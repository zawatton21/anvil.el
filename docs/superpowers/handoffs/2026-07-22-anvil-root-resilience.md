# Anvil Root Resilience Wiggum Handoff

**Objective:** Implement and deploy the approved Anvil root resilience design.
**Mode:** Autonomous Wiggum continuation.
**Frozen design:** docs/superpowers/specs/2026-07-22-anvil-root-resilience-design.md
**Frozen plan:** docs/superpowers/plans/2026-07-22-anvil-root-resilience.md
**Current repository:** /Users/johnw/src/emacs-lisp/anvil-root-resilience
**Current branch:** fix/anvil-root-resilience
**Nix deployment repository:** /Users/johnw/src/nix
**Nix implementation worktree:** /Users/johnw/src/nix-anvil-root-resilience
**Journal:** not in use
**PAL consensus:** unavailable in the advertised tool surface

## Completed

- Diagnosed false dead worker reporting separately from real 45-second root
  watchdog restarts.
- Verified ai-nix contains no Anvil lifecycle implementation and needs no task
  change.
- Received design approval and pushed initial design commit
  4e10d7e4101610e0ca895c474b568136b11c5694.
- User selected fail-closed pagination for oversized unbounded file reads.
- Activated Wiggum autonomous continuation and drafted the test-first plan.
- First fess audit of b0bd6d7 found incorrect Darwin attributes, broken focused
  selectors, incomplete worker coverage, telemetry gaps, and publication-order
  contradictions.
- Correction commit 9634dba fixed those findings and was independently audited.
- Packaged macOS Emacs experiments proved local Unix sockets survive pathname
  unlink/rebind, omit explicit `:type 'stream`, and are close-on-exec.
- A second audit and focused contract reviews found blocking socket/file I/O,
  unauthenticated event paths, response-builder gaps, fractional-limit bypass,
  TOCTOU file loading, mutating worker probes, JSON escape expansion, stale
  watchdog attribution, unbounded probe helpers, and post-gate rebases.
- The frozen design now removes watchdog-path record files entirely: activity is
  in monitor memory and each launch uses one private nonblocking event pipe.
- The file guard now performs an authoritative maximum-plus-one-byte read, the
  generic cap counts projected JSON-string bytes across every tool error path,
  reporting uses a non-mutating endpoint observer, and the supervisor helper is
  nonblocking, bounded, and timeout-contained.
- The plan now freezes runnable packaged-Emacs commands, named red-green Python
  groups, exact support/launcher provenance, socket-path preflight, startup and
  cached-response phases, per-exit watchdog clearing, revision-bearing version
  output, and no post-gate history mutation.
- A capability-topology audit then closed the final lifecycle gaps: shared-host
  mode is explicitly compatibility-only, parent-guard containment is preserved
  through strict next-launch stale-socket recovery, and one exit finalizer now
  clears watchdog attribution after both natural and intentional daemon exits.
- Final upstream review moved the sanitizer boundary ahead of params/lazy
  loading, protected the persistent harness hook with safe condition/tool
  allowlists, added a second post-disclosure guard, and covered Emacs unibyte
  JSON escaping without materializing encoder output.
- Final cross-repository review replaced unsafe full-loader pagination with a
  chunked, byte-capped, timer-yielding page scan; suppressed semantic duplicate
  activity phases; and made terminal supervisor shutdown invalidate stale
  status on stop, finalizer, or publication failure. Both focused and whole-
  plan audits now report no Critical or Important findings.
- Created the isolated Anvil implementation worktree on
  `fix/anvil-root-resilience` from signed, pushed planning commit `41a0145`.
- Task 0's exact packaged-Emacs baseline passed `make test` 40/40. The broader
  `make test-all` selected 2,507 of 2,745 tests across 109 files and found three
  unexpected results confined to `anvil-stdio-postdispatch-test.el` and
  `anvil-stdio-readiness-test.el`; every other suite passed.
- Task 1's non-vacuous three-test RED run failed for the intended missing
  classifier and legacy blocking/mutating reporting calls. After the minimal
  worker-reporting implementation, the same selector passed 3/3.
- Task 1's full packaged-Emacs verification passed the worker suite 68/68 and
  the worker-pool suite 16/16, both with zero unexpected results.
- Task 1 is committed as `f9b2b9e`, `4f0abf4`, and `a5c400c`; lazy workers
  report cold without forcing initialization, endpoint observation is
  nonblocking, and unexpected reporting errors remain visible.
- Task 2 is committed as `2dd45f6` and `ca7b793`; unbounded whole-file reads
  fail with pagination instructions above one MiB, while paged reads stream a
  maximum-plus-one-byte window without materializing the file. Audit repair
  `b525ef6` stops immediately after a selected page overflows, and `2a4ebbf`
  proves changed-file precedence on that immediate exit.
- Task 3's generic result boundary is committed as `7712a01` and `829f262`;
  projected JSON-string bytes are capped at two MiB across success and error
  paths, request-owned properties are stripped, spoofed overflow conditions
  are sanitized, and every exposed fallback value is fresh. Audit repair
  `72b44aa` rejects malformed cap types on every path, and `2ec574f` documents
  the enabled, disabled, and invalid cases explicitly.
- The broad package-lint compatibility commit was removed from this stacked
  branch to keep the resilience PR scoped. Fresh parent/child package-lint
  diagnostics normalize to the same 439 lines and SHA-256; upstream issue #57
  tracks that inherited repair separately.
- The gate-discovered stdio custody and readiness repairs are committed through
  `56fe0d1`. Runner publication uses an atomic bounded heartbeat and explicit
  ACK custody; readiness cleanup shares caller deadlines, validates ownership,
  and requires two bounded quiet observations. The standalone Bash 3.2 and
  Bash 5.3 suites, delayed-`ps` oracle, timeout-budget audit, and independent
  final readiness audit all pass without weakening a production deadline.
- Emacs 28 file identity fallback is committed as `b8159c0`; authenticated
  inline overflow handling is committed as `a2e3ec3`; and deadline-safe stdio
  cleanup is committed as `56fe0d1`. The inline boundary rejects malformed and
  spoofed conditions while preserving exact genuine 80/34 and 84/34 counts.
- Nix Task 5 now closes every independent audit finding: enum inputs and
  coherent optional deadline pairs are checked, the real clean-wrapper through
  real-Emacs capability chain is proven, duplicate boundary calls emit one
  transition, all eleven telemetry ERTs are gated, and capability keys are
  absent from every descendant class. The focused watchdog set passes 33/33;
  the supervisor capability set passes 5/5, including write-only descriptor
  mode and fresh deterministic run IDs. An independent final review reports no
  correctness or scope blocker; definitive evidence still waits on the new
  upstream source pin.

## 2026-07-23 load-resilience continuation

- Fork CI is green through signed tip `c5b2274`; the stacked PR remains
  intentionally unopened until the final Nix package is deployed and proved on
  Hera.
- A real concurrent dedicated smoke reproduced an ambiguous one-shot dispatch
  while every worker candidate used an independent spawn wait. With the
  deployed 2/1/1 roster, the old selector could spend seven complete waits
  before the tool body and approach the root watchdog deadline.
- Worker selection now uses one aggregate spawn deadline, starts candidates
  only in the highest-priority usable lane, introduces at most one cold worker,
  restarts previously demanded dead peers at most once, clamps full probes to
  the remaining time, and preserves round-robin and cached live fallbacks.
  Deterministic dead-pool, hanging-probe, lazy-lane, busy-fallback,
  lane-priority, and no-redemand regressions pass with the complete worker
  suite.
- Stdio runner READY/ACK control now has its own configured ten-second budget
  rather than inheriting a 240-second dispatch deadline before receiving a
  fresh execution deadline. Bash 3.2 whole-second accounting can add less than
  two seconds to that control phase and to each bounded retirement drain; the
  Nix outer envelope counts both conservative two-second allowances.  The
  ten-second cap preserves the established regression where parent scheduling
  delays ACK publication by more than five seconds.
  Generic operation defaults remain unchanged; the Nix deployment widens only
  the guarded helper budgets and binds the complete outer client envelope.
- Final upstream and Nix gates, the Hera switch, deployed fresh-client proof,
  and the dependent PR are still required. The observed `rc=70` is consistent
  with the repaired startup defects but is not treated as unique incident
  attribution because several fail-closed post-dispatch paths share that code.

## Current state

- The isolated upstream branch is `fix/anvil-root-resilience`; its signed
  implementation tip is `445ff0e`, above the pushed PR #55 planning head
  `41a0145`. The merge base is exactly `41a0145`, and the diff remains scoped
  to Tasks 1-3 plus the required stdio custody/readiness prerequisite. The
  branch is pushed to the fork, but the stacked PR remains intentionally
  unopened until Hera validation succeeds.
- An additional nox CI-shaped ERT run passed all 2,715 tests with exit status
  zero, including both Bash variants of the postdispatch and readiness suites.
  The 52-test smoke suite, warning-fatal five-file core compilation, release
  audit, Eask full compilation, Ruff, Python source compilation, shell syntax,
  and diff hygiene also pass. Eask reports inherited warnings, and literal
  `make lint` remains inherited-red only at package-lint; issue #57 and the
  byte-identical parent/child diagnostic proof are the scoped exception.
- The first fork CI run passed release audit and all six OS/version smoke jobs.
  Its two Linux full-suite jobs exposed one shared test-cleanup defect: an
  unreaped killed daemon leader kept its process group observable. `445ff0e`
  now reaps that leader during bounded group convergence, and the exact real-
  daemon regression passes locally with the packaged Emacs. The fork CI rerun
  is still required. Generated bytecode is absent. Dedicated Anvil reported no
  modified file buffers before Nix edit batches; its root session stayed
  responsive even while every worker lane was dead, reproducing the production
  failure mode.
- The Nix implementation worktree is `fix/anvil-root-resilience` from
  `facb6353740253d76e15d300c65b136f06a675b9`. Task 5 changes are deliberately
  uncommitted until the definitive upstream pin lands.
- The Nix worktree first pinned pushed upstream revision `18f3dfb` in signed
  source-only commit `c0eed8a`, then removed the now-upstreamed test-fixture
  patch in signed commit `49f8e52`; the dedicated package realizes from that
  pin. It must now be repinned to the final post-CI upstream tip. Task 5's fresh
  local generated fixtures pass watchdog 33/33 and its pure event-pipe tests;
  the real-Emacs descendant fixture is being rerun outside the interactive
  host's transient scheduler pressure before the Task 5 commit and full gates.
- `/Users/johnw/src/nix` remains synchronized with `origin/main` at `facb635`;
  its untracked fractal design document is unrelated user work and must remain
  untouched.
- `/Users/johnw/src/ai-nix` remains read-only at `edae388`; its two modified
  planning/design documents are unrelated user work and must remain untouched.
- Task 6 event ingestion, terminal finalization, bounded probe summary,
  revision-bearing version, and forced attribution smoke are not implemented
  yet. No Hera switch or production smoke has occurred.
- Per user direction, the second upstream PR must be stacked on PR #55 and
  opened only after every gate and the deployed Hera proof pass.

## Stop-and-escalate counters

- Repeated failing gate signature: 0 of 3.
- Unusable subagent output: 0 of 2.
- Unresolved rebase conflict: 0.
- Requirement ambiguity: 0 active.
- Destructive action required: no.

## Resume procedure

1. Re-read the Wiggum skill, frozen design, plan, and this handoff.
2. Verify live Anvil, Nix, and ai-nix state; live artifacts override this
   snapshot.
3. Commit this handoff, push the Linux cleanup, and require the fork CI rerun to
   pass before treating the upstream tip as final.
4. Repin the exact final upstream revision, archive hash, committer date, and
   package header version in the Nix worktree without opening the stacked PR.
5. Commit the verified Task 5 capability/telemetry work, then implement Task 6
   event ingestion, sole exit finalization, probe summary/advice, version proof,
   and forced attribution smoke test-first.
6. Run the complete focused and full Nix gates, independently review the whole
   branch, commit/push it, and switch `.#hera` with the explicit local ai-nix
   override.
7. From a fresh client, prove deployed version, watchdog attribution, restart,
   ordinary tool use, worker liveness, and sentinel absence. Only then open the
   second Anvil PR with base `fix/issue-53-interrupted-hangs` (or retarget to
   `master` if PR #55 merged first), clean worktrees/stashes, and hand off.
