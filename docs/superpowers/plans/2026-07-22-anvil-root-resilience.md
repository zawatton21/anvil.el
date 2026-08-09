# Anvil Root Resilience Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (- [ ]) syntax for tracking.

**Goal:** Correct lazy-worker status reporting, fail safely on oversized inline results, attribute every supervisor-backed per-agent root watchdog kill, and deploy the verified result on Hera.

**Architecture:** Upstream Anvil owns backend-independent worker state and
response-size safety. The Nix deployment owns an in-memory activity channel, a
per-launch nonblocking watchdog-event pipe, supervisor validation, probe
enrichment, packaging, and Hera activation. Large results fail before
expensive response encoding; diagnostics carry only bounded enums, registered
tool identifiers, and timing metadata.

**Tech Stack:** Emacs Lisp with ERT, Python 3 standard library with unittest, Nix, nix-darwin, Git, Anvil MCP.

## Global Constraints

- Worker states are exactly cold, alive, busy, unresponsive, and dead.
- Probing is non-mutating and nonblocking: it never deletes or logs a server
  file, changes worker state, spawns a worker, or calls the full liveness probe.
- anvil-file-max-inline-read-bytes defaults to 1,048,576 bytes and caps both
  unbounded bodies and retained paginated page bytes.
- Only a positive integer limit makes a read bounded; offset alone, fractions,
  zero, and negative values do not.
- An enabled unbounded guard never retains more than maximum-plus-one raw bytes
  and does not trust a separate pathname stat for safety.
- anvil-server-max-inline-result-bytes defaults to 2,097,152 projected
  JSON-string UTF-8 bytes, including escape expansion.
- Rejected file contents, returned tool text, and every tool-derived error text
  never appear in replacement errors, downstream hooks, logs, encoding, or
  telemetry.
- Watchdog telemetry never records arguments, paths, expressions, request IDs,
  raw JSON, results, output, or caller-derived environment values. The
  generated activity-socket path and run ID are the only new telemetry values
  carried into root Emacs and are removed before the child baseline; the
  launcher-only supervised-mode and event-descriptor keys are consumed before root exec. Existing
  synchronization-lease environment values remain unchanged.
- The synchronization lease and activity channel remain separate; activity updates never change dispatch generation.
- Identical consecutive `(phase method tool)` activity tuples are suppressed
  without advancing sequence or phase start time.
- Watchdog schema version is exactly 1; activity frames are at most 1,024 UTF-8
  bytes and event messages at most 512 bytes, no larger than Darwin PIPE_BUF.
- Root Emacs receives a one-shot close-on-exec Unix socket connection and no
  event descriptor. The monitor reports through a per-launch nonblocking pipe
  retained by the supervisor; no watchdog-path file writes or fsyncs exist.
- Existing status entries open with O_NOFOLLOW|O_NONBLOCK before type
  validation; unsafe prelaunch socket entries fail configuration.
- JSON decoders reject duplicate keys, non-finite constants, bool in integer fields, unsafe tool IDs, and unknown keys.
- Telemetry tool IDs are null unless registered and matched by `[A-Za-z0-9][A-Za-z0-9._/-]{0,127}`.
- restart_reason remains daemon-exited:CODE; last_watchdog is accepted only from
  that process's private pipe with matching daemon PID and 32-hex run ID, and a
  later natural or intentional exit without a valid event sets it to null.
- Existing 45-second heartbeat and 225-second dispatch deadlines remain unchanged.
- `anvil-mcp --version` contains the exact full pinned Anvil revision.
- ai-nix is read-only for this task.
- Preserve any unrelated user changes found in either repository.
- Every behavior change follows a witnessed red-green cycle.
- Every work commit receives an independent fess audit; final completion also requires whole-branch review, full gates, an Anvil rebase before the definitive pin, a Nix rebase before its push, Hera switch, and production smoke.

---

### Task 0: Create the isolated Anvil worktree and prove the baseline

**Files:**
- No tracked file changes.

**Interfaces:**
- Produces: /Users/johnw/src/emacs-lisp/anvil-root-resilience on branch fix/anvil-root-resilience.
- Base: the fetched fork/fix/issue-53-interrupted-hangs remote-tracking ref at this audited planning commit.

- [ ] **Step 1: Verify the published planning base**

From /Users/johnw/src/emacs-lisp/anvil.el run:

    git fetch fork
    local_base=$(git rev-parse fix/issue-53-interrupted-hangs)
    remote_base=$(git rev-parse fork/fix/issue-53-interrupted-hangs)
    test "$local_base" = "$remote_base"
    test "$remote_base" = "$(git rev-parse HEAD)"

The three revisions must match. If the remote ref is not this audited planning
commit, push the already-audited parent branch and repeat the check before
creating any child worktree.

- [ ] **Step 2: Detect worktree state and create the child branch**

Confirm neither the target path nor branch already exists. Then run:

    git worktree add /Users/johnw/src/emacs-lisp/anvil-root-resilience \
      -b fix/anvil-root-resilience fork/fix/issue-53-interrupted-hangs

Do not rewrite fix/issue-53-interrupted-hangs. All later Anvil source and test work runs in the new worktree.

- [ ] **Step 3: Verify the clean baseline**

The ordinary shell has no `emacs` executable. From
/Users/johnw/src/emacs-lisp/anvil-root-resilience, evaluate and freeze the exact
packaged runtime used by the deployment:

    ANVIL_TEST_EMACS_STORE=$(direnv exec /Users/johnw/src/nix \
      nix build --no-link --print-out-paths \
      '/Users/johnw/src/nix#packages.aarch64-darwin.anvil-mcp-dedicated.dedicatedRuntimeEmacs')
    export ANVIL_TEST_EMACS="$ANVIL_TEST_EMACS_STORE/bin/emacs"
    test -x "$ANVIL_TEST_EMACS"
    export PATH="$ANVIL_TEST_EMACS_STORE/bin:$PATH"
    "$ANVIL_TEST_EMACS" --batch -Q --eval '(princ emacs-version)'

Record the resolved immutable store path in the progress ledger. Restore these
two exports before every later upstream test or Make target. Then run:

    make test
    make test-all
    make lint
    make byte-compile

Expected: every command exits zero. A failing baseline triggers root-cause investigation before implementation.

- [ ] **Step 4: Initialize durable progress**

Create the ignored .superpowers/sdd/progress.md ledger and record Task 0 complete with the exact baseline command results. The tracked handoff path update belongs to the Task 1 commit, where the handoff is explicitly listed.

---

### Task 1: Truthful nonblocking worker states

**Files:**
- Modify: anvil-worker.el
- Modify: tests/anvil-worker-test.el
- Modify: openspec/specs/worker-pool-spawn-deferred/spec.md
- Modify: docs/superpowers/handoffs/2026-07-22-anvil-root-resilience.md

**Interfaces:**
- Produces: anvil-worker--reported-state WORKER ENDPOINT-ALIVE returning one of cold, alive, busy, unresponsive, or dead.
- Produces: anvil-worker--reporting-endpoint-alive-p WORKER, a non-mutating
  existence/PID/local-connect observation.
- Consumes: worker plist keys :busy, :hung-checks, :demanded, :last-state, :server-file.
- Leaves the recovery-oriented anvil-worker--quick-alive-p unchanged and unused
  by reporting.

- [ ] **Step 1: Add three exact failing worker tests**

Create `anvil-worker-test-reported-state-precedence` with this table:

    (dolist (case
             '(((:demanded nil) nil cold)
               ((:demanded t) nil dead)
               ((:demanded nil) t alive)
               ((:demanded t :hung-checks 1) t unresponsive)
               ((:demanded t :hung-checks 1 :busy t) t busy)))
      (pcase-let ((`(,worker ,endpoint ,expected) case))
        (should (eq expected
                    (anvil-worker--reported-state worker endpoint)))))

Create `anvil-worker-test-status-nonblocking` around the real
`anvil-worker-status`. Install a pool with at least one worker in each
relevant state, count reporting-only check calls by worker, and make
`anvil-worker-spawn`, `anvil-worker--worker-alive-p`,
`anvil-worker--quick-alive-p`, `delete-file`, and `anvil-worker--log` fail
immediately if called. Count
`anvil-worker--reporting-endpoint-alive-p` instead. Assert it is called exactly
once per worker, no worker plist changes, and rendered status contains the
exact `state=`, `demanded=`, and `last=` fields.

Create `anvil-worker-test-probe-rendering` around the real MCP probe. Cover a
cold never-demanded worker, an unresponsive reachable worker with
`probe-failures=1/LIMIT`, and a reachable busy worker that also has stale
probe failures. Assert cold rows are not dead, `last=alive`, `last=dead`, and
`last=unknown` render as required, busy wins over unresponsive, and the
reporting-only check runs exactly once per worker without mutation.

- [ ] **Step 2: Run the focused worker tests and witness RED**

Run this single-quoted, non-vacuous selector:

    "$ANVIL_TEST_EMACS" --batch -Q -L . -L tests -l ert -l tests/anvil-worker-test.el \
      --eval '(let* ((selector "^anvil-worker-test-\\(reported-state-precedence\\|status-nonblocking\\|probe-rendering\\)$")
                     (selected (ert-select-tests selector t)))
                (unless (= 3 (length selected))
                  (error "expected 3 focused worker tests, selected %d"
                         (length selected)))
                (ert-run-tests-batch-and-exit selector))'

Expected failure: `anvil-worker--reported-state` is absent or an existing
status/probe path performs the full liveness check or renders the wrong state.

- [ ] **Step 3: Implement the minimal classifier and formatter**

Add near the existing pool status functions:

    (defun anvil-worker--reported-state (worker endpoint-alive)
      "Return WORKER's nonblocking externally reported state."
      (cond
       ((and endpoint-alive (plist-get worker :busy)) 'busy)
       ((and endpoint-alive
             (> (or (plist-get worker :hung-checks) 0) 0))
        'unresponsive)
       (endpoint-alive 'alive)
       ((not (plist-get worker :demanded)) 'cold)
       (t 'dead)))

Add a formatter shared by `anvil-worker-status` and
`anvil-worker--tool-probe`. It emits state, demanded=yes or no, last=alive,
dead, or unknown, conditional PID, and conditional
probe-failures=N/LIMIT. Implement
`anvil-worker--reporting-endpoint-alive-p` with the existing low-level stale
predicate but without deletion, logging, or mutation. Both callers compute
endpoint-alive exactly once with that reporting-only helper and pass it to the
classifier/formatter.

Update the registered probe description to name all five states. Do not change
health, spawn, ownership, or recovery functions.

- [ ] **Step 4: Update the worker-pool observability requirement and handoff**

Add an acceptance scenario to worker-pool-spawn-deferred/spec.md:

- never-demanded plus absent endpoint reports cold;
- demanded plus absent endpoint reports dead;
- reachable endpoint reports alive, busy, or unresponsive;
- status and probe reporting perform no spawn, full liveness probe, deletion,
  log write, or worker mutation.

Update the handoff current repository/branch to the isolated worktree and record
the Task 0 baseline evidence.

- [ ] **Step 5: Verify GREEN and the full worker suites**

Run the focused command from Step 2, then:

    "$ANVIL_TEST_EMACS" --batch -Q -L . -L tests \
      -l ert -l tests/anvil-worker-test.el \
      -f ert-run-tests-batch-and-exit

    "$ANVIL_TEST_EMACS" --batch -Q -L . -L tests \
      -l ert -l tests/anvil-worker-pool-test.el \
      -f ert-run-tests-batch-and-exit

Expected: all selected tests pass with zero unexpected results.

- [ ] **Step 6: Commit and independently audit**

Stage only the four listed task files and commit:

    git commit -S -m "Report lazy Anvil workers as cold"

Dispatch a read-only fess audit for the commit using the frozen spec and this
task. Verify and fix every real finding before Task 2.

---

### Task 2: Bound inline file reads and paginate safely

**Files:**
- Modify: anvil-file.el
- Modify: tests/anvil-file-test.el

**Interfaces:**
- Produces: anvil-file-max-inline-read-bytes, default 1,048,576.
- Produces: anvil-file--stream-chunk-bytes, fixed at 65,536.
- Produces: anvil-file--stream-yield-chunks, fixed at 16, and
  anvil-file--stream-yield-seconds, fixed at 0.001.
- Produces: anvil-file--validate-read-range OFFSET LIMIT.
- Produces: anvil-file--insert-capped-unbounded TARGET CAP, which inserts at
  most CAP+1 raw bytes into the current buffer, rejects overflow, and decodes a
  successful prefix without reopening TARGET.
- Produces: anvil-file--read-streamed-page TARGET OFFSET LIMIT CAP, which scans
  fixed-size raw chunks, returns exact total lines plus decoded requested text,
  and retains at most one chunk plus CAP+1 selected bytes.
- Defines a bounded request as one with an integer LIMIT greater than zero.
- Accepts OFFSET only when nil or a non-negative integer.
- Parses MCP offset/limit strings as complete ASCII decimal integers; no
  `string-to-number` prefix acceptance.

- [ ] **Step 1: Add three exact failing read-boundary tests**

Create `anvil-file-test-unbounded-read-cap`. With a 16-byte cap, cover a
17-byte regular file and a symlink to it. Make the existing full-body
`anvil--insert-file` fail if called. Assert both calls fail with the configured
maximum plus `offset=0` and `limit=200` guidance, omit a unique payload, and
never request more than 17 raw bytes.

In the same test, make the preliminary attributes report 16 bytes while the
actual target contains 17. The authoritative capped insertion must still reject
without retaining more than 17 bytes. This is the deterministic growth/
replacement regression; a separate pathname stat must not be the safety oracle.

Create `anvil-file-test-read-argument-validation`. Exercise direct
`anvil-file-read` calls and the real `anvil-file--tool-read` wrapper. Reject
fractional, zero, negative, signed, suffix-bearing, and non-decimal limits plus
fractional or negative offsets before either bounded or full body insertion.
Prove decimal `offset=0 limit=1` reaches the read exactly once.

Create `anvil-file-test-read-limit-boundaries`. Prove an exact-cap unbounded
file and a small UTF-8 file are unchanged; cap-plus-one rejects; a large read
with `offset=0 limit=200` and another with positive integer limit 1 return the
right page while a stubbed `anvil--insert-file` fails if called. Instrument raw
chunk insertion and retained-page growth to prove the peak bound, exact
`total-lines`, UTF-8 characters split across chunk boundaries, empty/final
unterminated lines, and offsets beyond EOF. Reject a selected page over the
byte cap without retaining its payload, and fail closed when the target
identity/generation changes or grows during the scan. Prove no read exceeds the
initial byte size and a zero-delay test timer fires through the periodic
bounded yield. Nil, zero, or negative
`anvil-file-max-inline-read-bytes` restores the legacy full-loader path.

- [ ] **Step 2: Run focused file tests and witness RED**

    "$ANVIL_TEST_EMACS" --batch -Q -L . -L tests -l ert -l tests/anvil-file-test.el \
      --eval '(let* ((selector "^anvil-file-test-\\(unbounded-read-cap\\|read-argument-validation\\|read-limit-boundaries\\)$")
                     (selected (ert-select-tests selector t)))
                (unless (= 3 (length selected))
                  (error "expected 3 focused file tests, selected %d"
                         (length selected)))
                (ert-run-tests-batch-and-exit selector))'

Expected failure: the current implementation full-loads before slicing and the
wrapper accepts decimal prefixes through `string-to-number`.

- [ ] **Step 3: Implement strict arguments and capped insertion**

Add the defcustom. Validate direct OFFSET/LIMIT types before preparing the
path. In the MCP wrapper, accept only complete ASCII decimal strings, require
OFFSET >= 0 and LIMIT > 0, and pass integers to the direct function.

For an enabled unbounded request:

1. resolve the regular target once with `file-truename`;
2. use its regular-file attributes only as an early-rejection optimization;
3. otherwise insert bytes 0 through CAP+1 with
   `insert-file-contents-literally` under a unibyte/no-conversion buffer;
4. if more than CAP bytes arrived, erase them and signal the bounded pagination
   error;
5. otherwise decode that already-read byte string as UTF-8 in the result buffer
   and continue without reopening the path.

The error reports an exact stable stat size when early rejection is possible,
or an `at least CAP+1` lower bound when the capped read detects growth. It
never contains a path or file contents.

Positive-integer pagination and an explicitly disabled cap retain their existing
response shape, but enabled pagination uses
`anvil-file--read-streamed-page`, never `anvil--insert-file`. Scan the resolved
target in fixed-size unibyte chunks, count newline boundaries across chunks,
retain only selected raw bytes through CAP+1, and decode only the retained page
as UTF-8. Compare file identity, size, and modification generation before and
after the scan; discard and return a fixed retry error if they differ. Preserve
Emacs `count-lines` semantics for a final unterminated line. If the selected
page exceeds CAP, erase it and instruct the caller to lower `limit` or choose a
filtered/region tool. Peak retained file data must be at most one chunk plus
CAP+1 bytes. Freeze the initial file size as the final byte offset so growth
cannot extend the loop. After every sixteen 65,536-byte chunks, call
`accept-process-output` with a 0.001-second timeout so watchdog timers remain
live during a very large scan.

An explicitly disabled cap retains the legacy full-loader path. Update the
tool docstring and registered description to require a positive integer limit
for large files.

- [ ] **Step 4: Verify GREEN and all file tests**

Run the focused command, then:

    "$ANVIL_TEST_EMACS" --batch -Q -L . -L tests \
      -l ert -l tests/anvil-file-test.el \
      -f ert-run-tests-batch-and-exit

Expected: all tests pass with zero unexpected results.

- [ ] **Step 5: Commit and independently audit**

Commit:

    git commit -S -m "Bound Anvil inline file reads"

Run the per-commit fess audit and resolve every verified finding.

---

### Task 3: Bound every tool-derived text path by projected JSON size

**Files:**
- Modify: anvil-server.el
- Modify: anvil-harness-telemetry.el
- Modify: tests/anvil-test.el
- Modify at closeout: docs/superpowers/handoffs/2026-07-22-anvil-root-resilience.md

**Interfaces:**
- Produces: anvil-server-max-inline-result-bytes, default 2,097,152
  projected JSON-string UTF-8 bytes.
- Produces: anvil-server--projected-json-string-bytes TEXT STOP-AFTER,
  matching `json-encode-string` escaping without materializing it.
- Produces: anvil-server--safe-tool-label TOOL-NAME.
- Produces: anvil-server--safe-condition-symbol CONDITION, returning only a
  fixed classifier/server allowlist member or canonical `error`.
- Produces: anvil-server--enforce-inline-result-limit TOOL-NAME RESULT-TEXT,
  returning RESULT-TEXT or signaling the distinct internal condition
  anvil-server-inline-result-too-large with an already-bounded diagnostic.
- Produces: anvil-server--sanitize-tool-error TOOL-NAME CLASS CONDITION,
  returning `(:condition SANITIZED :text TEXT :tool SAFE-LABEL)` with bounded
  response text, a newly constructed sanitized condition, and the fixed-grammar
  label used by every downstream hook, error metric, and log; never signals.
- Integrates the error helper before hooks/logs/wrapping in tool-error,
  invalid-parameter, quit, generic-error, tool-not-found, and
  `anvil-server-with-error-handling` paths.
- Moves the protected tools/call boundary ahead of params extraction, registry
  lookup, and lazy-placeholder loading.
- Validates params is an object/alist before `alist-get`, signaling a fixed
  content-free `anvil-server-invalid-params` otherwise.

- [ ] **Step 1: Add three exact failing server tests**

Create `anvil-test-inline-result-limit-end-to-end`. Register a temporary tool
that returns a unique escape-heavy payload beyond a dynamically bound 32-byte
projected cap. Send a real `tools/call` through
`anvil-server-process-jsonrpc`. Assert valid JSON-RPC, `isError` true,
bounded diagnostic fields, and total payload absence from disclosure, metrics,
hooks, messages/logs, MCP wrapping, and encoded response. In a second case,
stub disclosure to expand an initially safe result beyond the cap and assert a
post-disclosure guard rejects it before every downstream seam.

Create `anvil-test-inline-result-limit-error-paths`. Exercise oversized
`anvil-server-tool-error`, invalid-parameter, quit, generic-error, and
tool-not-found text. Include non-string condition data and a real registered
handler whose body uses `anvil-server-with-error-handling`. For every branch
assert the helper is non-recursive, the macro and dispatcher hooks receive only
newly constructed sanitized conditions, and the response preserves its
existing envelope while bounding its text. Successful overflow and
`anvil-server-tool-error` remain MCP `isError` results; invalid parameters,
quit, generic errors, and lookup miss remain their current JSON-RPC error
shapes. Neither payload, original condition cell, nor unsafe tool name reaches
a downstream seam. Install the real
`anvil-harness-telemetry--dispatcher-hook` with its database recorder stubbed
at the final write call, and prove that this production persistence path sees
only the sanitized condition and safe label. Update both hook docstrings to
describe that contract. Also send malformed oversized `params` and install a
lazy placeholder whose loader signals an oversized sentinel. Stub
`anvil-server--handle-error` to fail the test if reached; both cases must retain
exact envelopes and expose no sentinel: malformed params is JSON-RPC invalid
params (-32602), while a generic lazy-loader `error` is JSON-RPC internal error
(-32603). Add an explicit lazy-loader `anvil-server-tool-error` subcase that
remains an MCP `isError` result.

Assert exact existing hook cardinality: one call for macro-wrapped, generic,
and invalid-parameter failures; zero for a direct tool-error, quit, and lookup
miss. Through the real harness dispatcher hook, require the sanitized condition
symbol plus source to produce the same expected classifier class as the safe
original while no rejected payload survives. Dynamically define and signal a
condition whose interned name contains the sentinel; require canonical `error`
and total name absence, while `void-function` and `wrong-type-argument` remain
allowlisted and retain their expected classifications.

Create `anvil-test-inline-result-limit-boundaries`. Compare the projected
counter with `json-encode-string` (minus its two quote delimiters) for quotes,
backslashes, every control byte, ASCII, BMP, non-BMP text, and a unibyte string
containing every value 0x80-0xFF. Add a boundary proving Emacs's octal escaping
of unibyte high bytes can reject a raw string well below the cap. Prove exactly
32 projected bytes succeeds, 33 fails, raw text below 32 can fail through
escaping, and multibyte text uses UTF-8 bytes. Prove only the exact ASCII tool
grammar is rendered; all other IDs use `<oversized-tool-id>`. Nil, zero, or negative
configuration restores legacy success and error response text/envelopes while
hooks still receive a reconstructed condition and safe label. Always
unregister fixtures in `unwind-protect`. Rebind `json-encode-string` and
`json-encode` to signal while calling the projected counter; it must still
match precomputed oracle byte counts without materializing encoder output.

Fault-inject the projected counter and safe-label helper while calling
`anvil-server--sanitize-tool-error`, and bind malformed cap values. The
sanitizer must never signal or reach `anvil-server--handle-error`; internal
failure returns a fixed empty-text condition plus `<oversized-tool-id>` without
request-owned data.

- [ ] **Step 2: Run focused server tests and witness RED**

    "$ANVIL_TEST_EMACS" --batch -Q -L . -L tests -l ert -l tests/anvil-test.el \
      --eval '(let* ((selector "^anvil-test-inline-result-limit-\\(end-to-end\\|error-paths\\|boundaries\\)$")
                     (selected (ert-select-tests selector t)))
                (unless (= 3 (length selected))
                  (error "expected 3 focused server tests, selected %d"
                         (length selected)))
                (ert-run-tests-batch-and-exit selector))'

Expected failure: returned or tool-derived error text reaches hooks or encoding,
or JSON escaping expands an accepted raw payload beyond the cap.

- [ ] **Step 3: Implement the projected counter and non-recursive guards**

Implement a streaming projected-byte counter that mirrors Emacs JSON string
escaping, accepts an early-stop bound, and never constructs the escaped string.
Use a fixed-grammar tool label rather than truncating caller-controlled text.

Define `anvil-server-inline-result-too-large` as a distinct internal
condition. Apply the successful guard immediately after `result-text`
construction and before disclosure, payload metrics, hooks, MCP wrapping, and
response encoding. Apply it again to disclosure's returned text before metrics
or wrapping. Its dedicated branch wraps only the prebounded diagnostic as
`isError`.

Move the `condition-case` in `anvil-server--handle-tools-call` around the whole
operation, beginning before `params` extraction, registry lookup, and lazy
loading. Keep a nil-initialized tool label available to the sanitizer if
extraction itself fails. No tool-derived condition may reach
`anvil-server--handle-error`. Before calling `alist-get`, require params to be
an object/alist and signal `anvil-server-invalid-params` with a fixed string
that never formats the rejected object.

Apply the non-signaling error sanitizer before every tool-derived condition hook,
log, or response, including the hook inside
`anvil-server-with-error-handling`. Always pass a newly constructed condition
containing only bounded text and the returned safe tool label to hooks; never
pass the original condition or raw label, even when its text is below the cap.
Use the safe label for error metrics and logs too, especially lookup misses. A
non-string value becomes a fixed message without printing the object. Bound the
tool-not-found branch as well. A nil or non-positive cap preserves legacy
response behavior while still preventing an original condition cell from
entering the persistent hook. Preserve only the fixed classifier-known and
Anvil-server condition-symbol allowlist frozen in the design; canonicalize
every other condition symbol to `error` before any downstream seam.

- [ ] **Step 4: Verify GREEN and full server tests**

Run the focused command, then:

    "$ANVIL_TEST_EMACS" --batch -Q -L . -L tests \
      -l ert -l tests/anvil-test.el \
      -f ert-run-tests-batch-and-exit

Expected: all tests pass with zero unexpected results.

- [ ] **Step 5: Run all upstream Anvil gates**

With the Task 0 packaged Emacs still first on PATH:

    make test
    make test-all
    make lint
    make byte-compile

Expected: every command exits zero with no test failures or compile errors.

- [ ] **Step 6: Commit and independently audit the server guards**

Commit `anvil-server.el`, `anvil-harness-telemetry.el`, and
`tests/anvil-test.el` together so the hook contract and its end-to-end coverage
cannot be stranded:

    git commit -S -m "Reject oversized inline Anvil results"

Run the per-commit fess audit and resolve every verified finding.

- [ ] **Step 7: Finalize Anvil before the Nix pin**

Drain partner observations, run an independent whole-branch review against the
frozen design and plan, fix all Critical and Important findings, rerun all four
gates, and perform a final fess audit of the last work commit.

Update and commit the tracked handoff with the Anvil gate/review evidence. Then
establish the definitive published history exactly once:

    git fetch fork
    git rebase fork/fix/issue-53-interrupted-hangs
    make test
    make test-all
    make lint
    make byte-compile
    git push -u fork fix/anvil-root-resilience
    git status --short --branch

If the branch was already published before this final rebase, use
`git push --force-with-lease` only after verifying the remote still names the
pre-rebase tip. No Anvil history rewrite is allowed after Task 4 computes the
published archive hash.

Record the definitive `git rev-parse HEAD` in the ignored progress ledger for
Task 4.

---

### Task 4: Pin the definitive Anvil revision in an isolated Nix worktree

**Files:**
- Modify: packages/anvil-mcp/source.nix

**Interfaces:**
- Consumes: the published, final Anvil commit from Task 3.
- Produces: matching date, hash, rev, and version metadata in source.nix.
- Provenance: committer ISO timestamp from `%cI`; version from the single
  `;; Version:` header in that revision's `anvil.el`.

- [ ] **Step 1: Create an isolated Nix worktree**

From /Users/johnw/src/nix, inspect existing worktrees, branches, and live status
first. At the latest planning check main equals origin/main at 7bf5693 and has a
user-owned `config/packages.nix` modification; do not assume that snapshot
remains true. Create a sibling linked worktree on branch
fix/anvil-root-resilience based on the current committed local main, preserving
all uncommitted user changes in the original checkout.

    git fetch origin
    nix_base=$(git rev-parse main)
    git worktree add /Users/johnw/src/nix-anvil-root-resilience \
      -b fix/anvil-root-resilience "$nix_base"

Confirm the new worktree is clean and its HEAD equals `$nix_base`. Run every
Nix command through `direnv exec .`.

- [ ] **Step 2: Compute all source metadata from the published commit**

    anvil_repo=/Users/johnw/src/emacs-lisp/anvil-root-resilience
    anvil_rev=$(git -C "$anvil_repo" rev-parse HEAD)
    test "$anvil_rev" = "$(git -C "$anvil_repo" rev-parse fork/fix/anvil-root-resilience)"
    anvil_date=$(git -C "$anvil_repo" show -s --format=%cI "$anvil_rev")
    version_lines=$(git -C "$anvil_repo" show "$anvil_rev:anvil.el" |
      sed -n 's/^;; Version:[[:space:]]*//p')
    test "$(printf '%s\n' "$version_lines" | sed '/^$/d' | wc -l | tr -d ' ')" = 1
    anvil_version=$version_lines
    base32_hash=$(direnv exec . nix-prefetch-url --unpack \
      "https://github.com/jwiegley/anvil.el/archive/${anvil_rev}.tar.gz")
    sri_hash=$(direnv exec . nix hash convert --hash-algo sha256 --to sri "$base32_hash")
    printf 'date=%s\nrev=%s\nversion=%s\nhash=%s\n' \
      "$anvil_date" "$anvil_rev" "$anvil_version" "$sri_hash"

Use the four printed values exactly. Do not bump the package version merely
because the revision changed.

- [ ] **Step 3: Update source.nix and verify the Darwin package**

Replace only `date`, `hash`, `rev`, and `version`; keep owner and repo
unchanged.

    direnv exec . nix eval --raw \
      .#packages.aarch64-darwin.anvil-mcp-dedicated.currentAnvilRev

The output must equal `$anvil_rev`. Then run:

    direnv exec . nix build \
      .#checks.aarch64-darwin.anvil-mcp-dedicated -L

Linux `anvil-mcp-headless` checks are optional here and may be run only when
a Linux builder is explicitly available; they are not Darwin attributes.

- [ ] **Step 4: Commit and independently audit the pin**

Commit only source.nix:

    git commit -S -m "Pin Anvil root resilience changes"

Run an independent fess audit against the pin commit before watchdog work.

---

### Task 5: Attribute watchdog kills through nonblocking capabilities

**Files:**
- Modify: packages/anvil-mcp/default.nix
- Modify: packages/anvil-mcp/watchdog-test.py
- Create: packages/anvil-mcp/watchdog-test-support.py
- Modify: packages/anvil-mcp/timeout-ordering-test.py when generated constants
  require it
- Modify: packages/anvil-mcp/agent-supervisor.py
- Modify: packages/anvil-mcp/agent-supervisor-test.py
- Create: packages/anvil-mcp/watchdog-telemetry-test.el
- Modify: packages/anvil-mcp/headless-smoke.nix

**Interfaces:**
- Produces in the exact generated launcher: strict_json_object,
  validate_activity, select_deadline_cause, and write_watchdog_event.
- Produces a generated `dedicatedTelemetryInit` loaded by production and by the
  exact ERT fixture; no copied Elisp implementation is allowed.
- Produces one mode-0600 `.anvil-root-activity.sock`; no activity/event regular
  file exists.
- Supervisor creates a fresh nonblocking event pipe and 32-hex run ID per
  daemon, retains the read end, passes only a write descriptor above 9 with
  `pass_fds`, and sets launcher-only `ANVIL_EMACS_WATCHDOG_SUPERVISED=1`.
- The unsupervised shared-host launcher accepts only the explicit all-three-
  absent case, generating a run ID and internal discard pipe. Marked supervised
  mode requires both capabilities; every partial or unmarked capability
  combination fails configuration, and per-agent launch never falls back.
- The all-absent shared-host path is compatibility-only and publishes no
  attribution because it has no persistent supervisor/status owner.
- Launcher consumes the supervised-mode and event-descriptor environment keys,
  monitor retains the write end, and root closes it before exec.
- Uses schema version 1, 1,024-byte activity frames, a 512-byte atomic event ceiling,
  and the frozen telemetry grammar.
- Preserves lease generation and all timeout policy values.

- [ ] **Step 1: Add explicit shared-support and named test contracts**

Move AST extraction into `watchdog-test-support.py`. Because tests run with
Python `-I` and the filename is hyphenated, both test programs load it only
through `importlib.util.spec_from_file_location` using
`ANVIL_WATCHDOG_TEST_SUPPORT`. They similarly require
`ANVIL_DEDICATED_LOCK_LAUNCHER`; no fallback or second writer is allowed.

Refactor `watchdog-test.py` into named unittest classes:

- `WatchdogProtocolTests`
- `WatchdogTransportTests`
- `WatchdogLifecycleTests`
- `WatchdogCauseTests`

Extract the Nix-local activity/probe Elisp into `dedicatedTelemetryInit` and
load that exact file from production `dedicatedInit`. Expose the launcher,
support, telemetry init, packaged Anvil, and runtime Emacs store paths in
package passthru. In
`headless-smoke.nix`, pass their exact store paths to watchdog and supervisor
tests and assert the paths inside the tests.

For local focused cycles define this realising refresh after the passthru
attributes exist:

    realize_anvil_attr() {
      direnv exec . nix build --no-link --print-out-paths \
        ".#packages.aarch64-darwin.anvil-mcp-dedicated.$1"
    }
    refresh_watchdog_paths() {
      export ANVIL_MCP_PACKAGE=$(direnv exec . nix build --no-link \
        --print-out-paths '.#packages.aarch64-darwin.anvil-mcp-dedicated')
      export ANVIL_DEDICATED_LOCK_LAUNCHER=$(realize_anvil_attr \
        dedicatedLockLauncher)
      export ANVIL_WATCHDOG_TEST_SUPPORT=$(realize_anvil_attr \
        watchdogTestSupport)
      export ANVIL_DEDICATED_TELEMETRY_INIT=$(realize_anvil_attr \
        dedicatedTelemetryInit)
      export ANVIL_DEDICATED_ANVIL=$(realize_anvil_attr dedicatedAnvil)
      export ANVIL_TEST_EMACS_STORE=$(realize_anvil_attr \
        dedicatedRuntimeEmacs)
      export ANVIL_TEST_EMACS="$ANVIL_TEST_EMACS_STORE/bin/emacs"
      export ANVIL_DEDICATED_AGENT_SUPERVISOR=$(realize_anvil_attr \
        dedicatedAgentSupervisor)
    }
    refresh_watchdog_paths

`nix eval` is not sufficient for these generated paths. Rerun
`refresh_watchdog_paths` after every edit to `default.nix` and before each
focused GREEN command, so the command tests realised artifacts from the
current source. The final Nix check passes and asserts the same exact store
paths.

- [ ] **Step 2: Witness RED and implement strict protocol/cause primitives**

Add protocol/cause tests for exact key sets and enums, duplicate keys through
`object_pairs_hook`, non-finite constants through `parse_constant`, bool
rejection for every integer, strict UTF-8, run ID/PID/sequence, 1,024/512-byte
ceilings, tool grammar, all seven causes, and simultaneous-deadline precedence.
Include control and multibyte registered tool IDs becoming null.

Run:

    direnv exec . python3 -I packages/anvil-mcp/watchdog-test.py \
      WatchdogProtocolTests WatchdogCauseTests

Expected RED: the generated launcher lacks the strict protocol, event writer,
and cause selector. Implement only those pure helpers and rerun until both
classes pass.

- [ ] **Step 3: Witness RED and implement pipe/socket lifecycle**

Add `WatchdogTransportTests`, `WatchdogLifecycleTests`, and
`SupervisorEventPipePlumbingTests` for:

- supervisor creation of nonblocking read/write pipe ends, write descriptor
  above 9, fresh run ID, `pass_fds`, process-object ownership, and cleanup on
  launch failure;
- end-to-end survival of that descriptor through the clean-environment wrapper,
  parent guard, daemon shell, and lock launcher before the monitor consumes it;
- the shared-host all-three-absent compatibility path, the complete marker and
  capability-value matrix, and proof that marked per-agent mode cannot select
  the discard pipe;
- launcher descriptor type/number/nonblocking/write-only validation,
  environment removal, monitor-only retention, and root closure before exec;
- a real Emacs child scanning descriptors 3 through 1023 and finding no event
  pipe or connected activity socket;
- no client, silent client, partial/coalesced/oversized/malformed frames, and
  peer disappearance while deadline and monitor-exit checks continue;
- monitor initialization is exactly `startup/none/null` sequence 0, and a
  silent startup produces `startup-timeout` from the extracted production
  launcher rather than an Elisp fixture;
- one accepted connection, unlink/rebind after acceptance, and delivery only on
  the original connection;
- two launches including failure before accept, unlink after accept,
  best-effort unlink before monitor-initiated kill, and strict next-launch
  stale-socket reclamation after an external exit; prove a hostile replacement
  is preserved and symlink, regular-file, owner/mode/link cases fail closed;
- prospective activity-socket path length at the exact platform boundary,
  including a runtime that fits `/emacs/server` but not the new suffix;
- removal of activity socket/run ID keys before the immutable baseline, with
  shell, host, worker, offload, and arbitrary child environments clean.

Run:

    direnv exec . python3 -I packages/anvil-mcp/watchdog-test.py \
      WatchdogTransportTests WatchdogLifecycleTests
    direnv exec . python3 -I packages/anvil-mcp/agent-supervisor-test.py \
      SupervisorEventPipePlumbingTests

Expected RED: pipe plumbing, socket lifecycle, and path preflight are absent.

Implement `start_daemon` pipe/run creation and cleanup, prospective socket-path
validation for both per-agent and shared-host runtimes, launcher inheritance,
and the nonblocking listener. The monitor starts with in-memory
`startup/none/null` sequence 0. It processes complete frames within the
4,096-byte tick budget while retaining at most one 1,024-byte partial frame.
No regular diagnostic file or fsync is permitted.

- [ ] **Step 4: Witness RED and implement exact root phase telemetry**

In `watchdog-telemetry-test.el`, add seven exact
`anvil-watchdog-telemetry-phase-*` ERT tests covering parse failure, unknown
method, cached `tools/list`, successful tool, direct bounded tool-error,
macro-wrapped bounded error, and non-local exit. Require the frozen sequences
and exact method/tool values. Add disconnect/send-failure cases proving
ordinary requests still succeed and only one constant diagnostic is emitted.
The test loads the exact `dedicatedTelemetryInit` store path and packaged Anvil
path from required environment variables. Python lifecycle coverage, not this
Elisp fixture, owns startup state and startup-timeout attribution.
Within the successful-tool test force both the pre- and post-disclosure guards;
within the macro-error test force both macro and dispatcher sanitizers. Each
must still emit exactly one `result-encode` frame through semantic duplicate
suppression.

Run:

    refresh_watchdog_paths
    "$ANVIL_TEST_EMACS" --batch -Q \
      -L "$ANVIL_DEDICATED_ANVIL/share/emacs/site-lisp" -l ert \
      -l packages/anvil-mcp/watchdog-telemetry-test.el \
      --eval '(let* ((selector "^anvil-watchdog-telemetry-phase-")
                     (selected (ert-select-tests selector t)))
                (unless (= 7 (length selected))
                  (error "expected 7 phase tests, selected %d"
                         (length selected)))
                (ert-run-tests-batch-and-exit selector))'

Expected RED: Nix-local activity connection/writer and phase advice are absent.

Generated Emacs captures and removes only the two new root telemetry keys before
the baseline, connects without `:type`, and disables telemetry on connect/send
failure without propagating the diagnostic error. Add advice at all frozen
boundaries, including all three response builders and the shared upstream
tool-error sanitizer. Cache the last emitted `(phase method tool)` tuple and
suppress an identical consecutive transition without incrementing sequence or
changing the phase-start timestamp. Preserve sanitized method/tool until outer
idle.

- [ ] **Step 5: Witness RED and implement nonblocking kill attribution**

Extend `WatchdogCauseTests` so `write_watchdog_event` verifies the runtime
PIPE_BUF is at least 512 and performs exactly one nonblocking <=512-byte write,
dropping an otherwise-valid long tool ID to null when necessary. Require the
monitor to ignore SIGPIPE explicitly. Then the kill
path calls SIGKILL regardless of
success, partial write, EAGAIN, EPIPE, encoding failure, or injected exception.
Assert no file open/write/fsync occurs. Prove the secret sentinel is absent.

Run the class and witness the new event/kill assertions fail. Refactor
`kill_parent_if` to accept an exact cause/event factory after failure
revalidation. Select the earlier absolute heartbeat/dispatch deadline and name
integrity, monitor-state, durable-refresh, and internal sites explicitly. Event
failure is swallowed only at this diagnostic boundary; SIGKILL is unconditional.

- [ ] **Step 6: Verify GREEN, build the Darwin check, commit, and audit**

Run all four named watchdog classes, the phase ERT selector, the pipe-plumbing
supervisor class, then:

    direnv exec . nix build \
      .#checks.aarch64-darwin.anvil-mcp-dedicated -L

The Nix check must substitute and assert the exact support and launcher store
paths. Commit the listed files:

    git commit -S -m "Record dedicated Anvil watchdog causes"

Run the per-commit fess audit and resolve every verified finding.

---

### Task 6: Validate event pipes and expose bounded restart diagnostics

**Files:**
- Modify: packages/anvil-mcp/agent-supervisor.py
- Modify: packages/anvil-mcp/agent-supervisor-test.py
- Modify: packages/anvil-mcp/agent-supervisor-smoke.py
- Modify: packages/anvil-mcp/default.nix
- Modify: packages/anvil-mcp/watchdog-telemetry-test.el
- Modify: packages/anvil-mcp/headless-smoke.nix

**Interfaces:**
- Produces: read_watchdog_event(PROCESS, EXPECTED_PID, EXPECTED_RUN_ID)
  returning a sanitized dict or None from that process's private pipe.
- Produces: finalize_daemon_exit(PROCESS), the sole natural/intentional exit
  finalizer, returning a valid current event or None after closing descriptors.
- Replaces the always-present last_watchdog on every exit: valid current event
  or null.
- Preserves restart_reason `daemon-exited:CODE`.
- Adds supervisor
  `--probe-summary --runtime-dir PATH --agent-key KEY`, emitting one
  validated bounded ASCII line.
- Changes `anvil-mcp --version` to include the full pinned revision.

- [ ] **Step 1: Witness RED for event ingestion and per-exit status**

Create `SupervisorWatchdogEventTests`. Valid fixtures must call the
AST-extracted production `write_watchdog_event` from the exact launcher path;
mutate raw bytes only for rejection cases. Cover:

- valid adoption from the exited process's read descriptor;
- missing/partial/multiple/oversized records, stale run ID, wrong PID, duplicate
  keys, non-finite constants, unknown keys/enums, bool integers, unsafe tool,
  invalid optional deadlines, and invalid UTF-8;
- read cap 513 and descriptor closure on every path;
- valid watchdog exit followed by an unrelated no-event exit, proving
  last_watchdog is cleared rather than misattributed;
- valid watchdog exit followed by an explicit no-lease stop/reap, proving the
  same finalizer clears last_watchdog without incrementing restart accounting;
- valid watchdog exit followed by externally requested supervisor shutdown,
  proving the outer `finally` finalizes before discarding the process and
  publishes daemon-null/last_watchdog-null status while still holding the lock;
- injected terminal-status publication failure proving the exact stale entry is
  safely invalidated rather than preserved with historical attribution;
- injected second `TimeoutExpired` from `stop_daemon` and injected event-
  finalizer failure, each proving exact-identity status invalidation under the
  still-held lock, descriptor cleanup where possible, and propagation of the
  original lifecycle error;
- unchanged `restart_reason` and sentinel absence.

Run:

    direnv exec . python3 -I packages/anvil-mcp/agent-supervisor-test.py \
      SupervisorWatchdogEventTests

Expected RED: the supervisor does not yet consume the process-specific pipe.

Implement strict nonblocking ingestion in one `finalize_daemon_exit` helper.
Call it after a natural `daemon.poll` exit and after every explicit stop/reap,
always before setting `daemon = None` or starting a replacement root. A single
complete event matching that process's PID/run ID becomes `last_watchdog`;
every exit without one sets the field to null. Intentional stops do not alter
restart count or reason.

Use that same path in the supervisor loop's outer `finally`. While the
supervisor lock is held, stop and finalize the daemon, publish a terminal
daemon-null record with `last_watchdog` null and unchanged restart accounting,
then preserve or clean lifecycle state as today. Bound transient publication
retries; if publication cannot succeed, identity-check and invalidate that
exact status entry so a live trusted record can never retain the older event.
Wrap stop/finalize/publication as one terminal transaction: if stopping or
event finalization raises, invalidate the exact pre-transaction status while
the lock is still held, perform nonblocking descriptor cleanup where possible,
and re-raise the original lifecycle error rather than publishing an unproven
daemon-null record.

- [ ] **Step 2: Witness RED for the strict summary boundary**

Create `SupervisorProbeSummaryTests` for valid current status, no watchdog,
symlink/path replacement, FIFO, socket, wrong owner/mode/link count, duplicate
keys, unsafe strings, dead lifecycle identities, and oversized input. Run FIFO
and socket wrong-type cases in timeout-bounded subprocesses so a blocking open
is itself a failure. Require exactly one ASCII line <=256 bytes or nonzero with
empty stdout/stderr and no raw JSON.

Run the class and witness RED. Implement the CLI using a validated directory FD
and `O_RDONLY|O_NOFOLLOW|O_NONBLOCK` before type validation. Require exact
known status/lifecycle fields and render only validated integers/enums/tool
grammar:

    root-restarts=N cause=CAUSE phase=PHASE tool=TOOL-OR-none

No current watchdog renders `cause=none phase=unknown tool=none`.

- [ ] **Step 3: Witness RED for bounded probe advice and version proof**

Add exact `anvil-watchdog-telemetry-probe-*` ERT tests to
`watchdog-telemetry-test.el` and extend the forced smoke for:

- exact normal summary and exactly one appended line;
- helper nonzero, malformed output, 258-byte output, one stderr byte, and a hung
  helper all yielding only `root-summary=unavailable`;
- registered newline, carriage-return, tab, control, and multibyte tool IDs
  never adding a line or terminal control;
- non-yielding `emacs-eval` recording
  `heartbeat-timeout/tool-call/tools/call/emacs-eval`;
- yielding dispatch overrun recording
  `dispatch-timeout/tool-call/tools/call/emacs-eval`;
- restart_count increasing with `restart_reason=daemon-exited:-9`;
- exact `anvil-mcp VERSION (anvil REV; dedicated Emacs)` with full 40-hex
  package revision;
- sentinel absence from status, probe, diagnostics, and logs.

Create exactly four probe ERT tests (valid summary, invalid output, timeout/
overflow, and adversarial tool label). Rerun `refresh_watchdog_paths`, then run
the exact generated telemetry fixture:

    "$ANVIL_TEST_EMACS" --batch -Q \
      -L "$ANVIL_DEDICATED_ANVIL/share/emacs/site-lisp" -l ert \
      -l packages/anvil-mcp/watchdog-telemetry-test.el \
      --eval '(let* ((selector "^anvil-watchdog-telemetry-probe-")
                     (selected (ert-select-tests selector t)))
                (unless (= 4 (length selected))
                  (error "expected 4 probe tests, selected %d"
                         (length selected)))
                (ert-run-tests-batch-and-exit selector))'

Extend the smoke CLI while retaining its existing full-suite positional form.
The focused form is exactly
`--scenario watchdog-attribution LAUNCHER SUPERVISOR`. Then run the complete
focused invocation:

    refresh_watchdog_paths
    smoke_root=$(mktemp -d /tmp/anvil-watchdog-attribution.XXXXXX)
    trap 'rm -rf -- "$smoke_root"' EXIT
    install -d -m 0700 "$smoke_root/home" "$smoke_root/runtime" \
      "$smoke_root/state"
    smoke_status=0
    direnv exec . env \
      HOME="$smoke_root/home" \
      ANVIL_EMACS_RUNTIME_ROOT="$smoke_root/runtime" \
      ANVIL_EMACS_STATE_ROOT="$smoke_root/state" \
      ANVIL_MCP_CLIENT_STARTUP_SECONDS=330 \
      ANVIL_MCP_CLIENT_TOOL_SECONDS=330 \
      python3 -I packages/anvil-mcp/agent-supervisor-smoke.py \
      --scenario watchdog-attribution \
      "$ANVIL_MCP_PACKAGE/bin/anvil-mcp" \
      "$ANVIL_DEDICATED_AGENT_SUPERVISOR" || smoke_status=$?
    rm -rf -- "$smoke_root"
    trap - EXIT
    test "$smoke_status" -eq 0

Expected RED: helper/advice/version and enriched smoke assertions are absent.

Implement the Nix-local probe advice with the existing
`anvil-headless--run-process-responsive`, calling the packaged Python and
supervisor script with the captured canonical `XDG_RUNTIME_DIR` and validated
directory basename as agent key. Use exactly a two-second timeout, 257-byte
stdout cap, and zero-byte stderr cap. Accept only exit zero plus one
newline-terminated ASCII line <=256 bytes; condition-case every runner failure
to the fixed unavailable line.

Change version output exactly to:

    anvil-mcp ${currentAnvilVersion} (anvil ${currentAnvilRev}; dedicated Emacs)

- [ ] **Step 4: Verify GREEN and complete Nix gates**

Run all focused Task 5/6 classes and the forced smoke, then:

    direnv exec . nix build \
      .#checks.aarch64-darwin.anvil-mcp-dedicated -L

    direnv exec . nix flake check -L

    direnv exec . ./build system

Run Linux headless checks only with an explicit Linux builder. Every applicable
command must exit zero.

- [ ] **Step 5: Commit and independently audit supervisor integration**

Commit:

    git commit -S -m "Expose Anvil watchdog restart diagnostics"

Run the per-commit fess audit and resolve every verified finding.

---

### Task 7: Whole-branch review, publication, deployment, and production proof

**Files:**
- No planned tracked changes; keep final evidence in the ignored progress ledger.
- Create code or test changes only when review or runtime evidence proves them
  necessary.

**Interfaces:**
- Consumes: the already-published definitive Anvil revision and all Nix commits.
- Produces: a pushed Nix branch, switched Hera generation, and direct evidence
  for every acceptance criterion.

- [ ] **Step 1: Drain partner observations**

In each repository, inspect `doc/observations` for regular non-hidden Markdown
files. If present, run partner cleanup before final review and verify its cleanup
commit.

- [ ] **Step 2: Integrate remotes before final review or gates**

Anvil history was finalized before the pin and must not be rewritten. Fetch and
require exact equality:

    git -C /Users/johnw/src/emacs-lisp/anvil-root-resilience fetch fork
    test "$(git -C /Users/johnw/src/emacs-lisp/anvil-root-resilience rev-parse HEAD)" = \
      "$(git -C /Users/johnw/src/emacs-lisp/anvil-root-resilience rev-parse fork/fix/anvil-root-resilience)"

In the Nix worktree, fetch first and record the feature remote's exact prior
state:

    git fetch origin
    nix_remote_before=$(git rev-parse --verify \
      refs/remotes/origin/fix/anvil-root-resilience 2>/dev/null || printf absent)
    if [ "$nix_remote_before" != absent ]; then
      git rebase refs/remotes/origin/fix/anvil-root-resilience
    fi
    git rebase origin/main

Resolve conflicts without guessing intent. No pull or rebase may occur after the
final audits/gates. Verify the evaluated `currentAnvilRev` still equals the
published Anvil tip.

- [ ] **Step 3: Run independent whole-branch reviews and fix findings**

Generate review packages from each merge base to HEAD. Dispatch the strongest
available reviewer against the frozen spec, plan, red-green evidence, generated
launcher, supervisor, and full diffs. Fix every Critical and Important finding
in coherent waves, rerun covering tests, and re-review.

An Anvil fix discovered after Task 4 follows this complete order: commit and
audit it, rerun all Anvil gates, push the new fast-forward Anvil tip, recompute
the archive hash, update and commit the Nix pin, audit that pin, and rerun
covering Nix tests. Never leave `source.nix` naming an unpublished or
superseded revision.

- [ ] **Step 4: Run final fess audits and full gates**

Audit the last work commit in each repository, including explicit checks for
stubs, vacuous tests, fixture drift, error swallowing, suppressions, fallback
smuggling, spec drift, scope creep, documentation drift, verification gaps, and
loose ends. Require clean verdicts.

Then run all four Anvil gates under `ANVIL_TEST_EMACS` and, in Nix:

    direnv exec . nix build \
      .#checks.aarch64-darwin.anvil-mcp-dedicated -L
    direnv exec . nix flake check -L
    direnv exec . ./build system

Any later task change requires a commit, independent fess audit, covering tests,
and the complete affected final gate again.

- [ ] **Step 5: Fetch without mutation, push, and prove synchronization**

For Anvil, fetch only, recheck exact equality and the Nix pin, then perform a
non-rewriting push:

    git -C /Users/johnw/src/emacs-lisp/anvil-root-resilience fetch fork
    test "$(git -C /Users/johnw/src/emacs-lisp/anvil-root-resilience rev-parse HEAD)" = \
      "$(git -C /Users/johnw/src/emacs-lisp/anvil-root-resilience rev-parse fork/fix/anvil-root-resilience)"
    git -C /Users/johnw/src/emacs-lisp/anvil-root-resilience push -u fork \
      fix/anvil-root-resilience

For Nix, fetch and require that neither the feature ref recorded before the
gates nor origin/main changed. If either changed, return to Step 2 and rerun the
review/audit/gate sequence.

    git fetch origin
    nix_remote_now=$(git rev-parse --verify \
      refs/remotes/origin/fix/anvil-root-resilience 2>/dev/null || printf absent)
    test "$nix_remote_now" = "$nix_remote_before"
    git merge-base --is-ancestor origin/main HEAD
    if [ "$nix_remote_before" = absent ]; then
      git push -u origin fix/anvil-root-resilience
    else
      git push -u \
        --force-with-lease=refs/heads/fix/anvil-root-resilience:$nix_remote_before \
        origin fix/anvil-root-resilience
    fi

Fetch once more and require both local tips equal their remote feature refs.
Both statuses must show no task-owned uncommitted changes and zero
ahead/behind. The evaluated Nix pin must equal the pushed Anvil tip.

- [ ] **Step 6: Switch Hera**

From the clean Nix worktree:

    direnv exec . sudo darwin-rebuild switch \
      --flake .#hera \
      --override-input ai-nix /Users/johnw/src/ai-nix

Do not modify ai-nix. Confirm exit zero and record the active generation,
package version, and evaluated Anvil revision.

- [ ] **Step 7: Acquire a fresh bridge and run production smoke**

A pre-existing bridge retains its old generation. Acquire a fresh bridge after
the switch, then prove:

- `anvil-mcp --version` exactly equals
  `anvil-mcp VERSION (anvil REV; dedicated Emacs)`, with independently
  evaluated VERSION/REV and REV equal to the pushed Anvil tip;
- a new lazy pool reports cold, demanded=no, and no false dead rows;
- both status paths use one reporting-only check per worker, mutate nothing, and
  create no worker process;
- an oversized unbounded file-read gives explicit
  `offset=0 limit=200` guidance without a root restart or full-body load;
- the same file with positive integer limit returns a bounded page, while a
  fractional limit fails before body loading;
- a small normal file-read, ordinary `emacs-eval`, and `shell-run` succeed;
- root and child environments contain none of
  `ANVIL_EMACS_WATCHDOG_SUPERVISED`,
  `ANVIL_EMACS_WATCHDOG_EVENT_FD`,
  `ANVIL_EMACS_WATCHDOG_ACTIVITY_SOCKET`, or
  `ANVIL_EMACS_WATCHDOG_RUN_ID`;
- supervisor restart_count stays unchanged during non-faulting smoke;
- the worker probe contains exactly one validated root summary line, while a
  forced hung/noisy helper yields only `root-summary=unavailable`;
- packaged forced heartbeat and dispatch scenarios expose the exact cause plus
  `tool-call/tools/call/emacs-eval`, and invalid event-pipe payloads are
  rejected;
- no secret sentinel appears in retained diagnostics.

- [ ] **Step 8: Final requirement audit and cleanup**

Bind every frozen acceptance criterion to direct evidence in the progress
ledger. Confirm no regular partner observations or task-created stashes remain.
Prune stale remote/worktree metadata. Remove task-created worktrees only after
their branches are pushed and removal cannot discard evidence. Leave unrelated
user state untouched.

Mark the persistent goal complete only after all evidence is present.
