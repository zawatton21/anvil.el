# Anvil Root Resilience Wiggum Handoff

**Objective:** Implement and deploy the approved Anvil root resilience design.
**Mode:** Autonomous Wiggum continuation.
**Frozen design:** docs/superpowers/specs/2026-07-22-anvil-root-resilience-design.md
**Frozen plan:** docs/superpowers/plans/2026-07-22-anvil-root-resilience.md
**Current repository:** /Users/johnw/src/emacs-lisp/anvil.el
**Current branch:** fix/issue-53-interrupted-hangs
**Nix deployment repository:** /Users/johnw/src/nix
**Nix implementation worktree:** not created yet
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

## Current state

- No source-code implementation has begun.
- The signed planning HEAD contains exactly the three resilience documents;
  the Anvil parent checkout is clean.
- Dedicated Anvil reported no modified file buffers before the latest edit
  batches; this does not cover the separate interactive development Emacs.
- Precursor planning commits b0bd6d7 and 9634dba plus the signed planning HEAD
  are local after the pushed design commit.
- /Users/johnw/src/nix main is synchronized with origin/main at
  7bf56931bd00c9f546cae8e64147d825661d0da7 and currently has a user-owned
  `config/packages.nix` modification that must remain untouched.
- /Users/johnw/src/ai-nix is read-only for this task at
  0610fd1283cf5ee52a5c71cbc8411a647b37dd7c and now has unrelated user-owned
  modifications/untracked files that must remain untouched.
- Task 0 remains blocked only on the amended commit-bound audit, push, and
  local/remote equality.

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
3. Independently audit the exact signed planning commit object. Fix every
   Critical/Important finding by amending and re-audit until clean.
4. Push the exact audited planning HEAD, then verify local/remote equality.
5. Execute Task 0 and its packaged-Emacs baseline in the isolated Anvil
   worktree.
6. Continue each test-first task with per-commit fess audit, partner-observation
   scan, and progress-ledger update.
7. Keep Nix implementation in /Users/johnw/src/nix-anvil-root-resilience and
   preserve unrelated user state.
