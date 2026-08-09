# Anvil Root Resilience Design

**Date:** 2026-07-22
**Status:** Approved for implementation
**Repositories:** /Users/johnw/src/emacs-lisp/anvil.el and /Users/johnw/src/nix

## Problem

A Hera agent reported that its Anvil worker pool had died while the root Emacs remained responsive. Investigation established two distinct problems:

1. Lazy, never-demanded workers were absent by design, but anvil-worker-probe rendered every absent endpoint as dead. Resetting such a pool merely changed it from a valid cold state to a warm state.
2. The per-agent root Emacs had actually been killed and restarted repeatedly. Calls failing after 46 to 49 seconds aligned with the 45-second root heartbeat watchdog. Root termination removed any child workers, and the replacement root correctly began with a cold pool.

The clearest trigger was an unbounded file-read of a 149,239-line Rust file. Anvil constructed and encoded the entire result synchronously in root Emacs. The watchdog recorded only daemon-exited:-9, so the exact timeout class and root phase had to be inferred after the fact.

## Goals

- Report worker lifecycle state truthfully without spawning workers or performing a blocking liveness check.
- Prevent oversized unbounded file reads from reaching the full-body loader or
  response construction.
- Prevent any oversized inline tool result from entering expensive final JSON response encoding.
- Record the exact watchdog cause and the last sanitized root activity phase
  for supervisor-backed per-agent roots.
- Surface their restart and watchdog information through the existing administrative probe.
- Preserve per-agent isolation, lazy worker startup, current timeout ordering, and existing supervisor compatibility.
- Deploy and production-smoke the result on Hera.

## Non-goals

- Do not make workers eager.
- Do not increase watchdog deadlines to hide root starvation.
- Do not route all file or shell operations through the worker pool.
- Do not return an oversized file automatically in a subprocess; the large result would still need to cross root Emacs and the MCP transport.
- Do not record request arguments, paths, expressions, output, request identifiers, caller-derived environment values, or other potentially sensitive data. The generated non-secret activity-socket pathname and run ID are the only new telemetry values carried into root Emacs; root removes both before taking its child-environment baseline. Launcher-only supervised-mode and event-descriptor keys are consumed before root exec. Existing synchronization-lease environment values remain unchanged.
- Do not change ai-nix; it contains no Anvil worker or watchdog implementation.
- Do not alter unrelated existing changes in /Users/johnw/src/nix/config/packages.nix or /Users/johnw/src/nix/docs/PI-AGENT-WIGGUM-PLAN.md.

## Approach

Use deterministic bounds and structured diagnostics.

Transparent offloading was rejected because it preserves the oversized response and still requires root Emacs to receive and encode it. Eager workers or larger watchdog windows were rejected because they disguise the observed state without fixing root starvation. The selected approach prevents the demonstrated unsafe operation, caps the remaining response boundary, and makes any future watchdog kill directly attributable.

## Component 1: Worker State Reporting

### State model

Add a pure, nonblocking worker-state classifier in anvil-worker.el. It receives
the worker plist and the result of a new reporting-only endpoint check. That
check may inspect existence, PID state, or make and immediately close a local
socket connection, but it never deletes a server file, logs, mutates worker
state, spawns a worker, or runs the full emacsclient probe. The classifier
returns exactly one primary state with this precedence:

1. busy when the endpoint is reachable and the worker is marked busy.
2. unresponsive when the endpoint is reachable, the worker is not busy, and the recorded consecutive full-probe failure count is greater than zero.
3. alive when the endpoint is reachable.
4. cold when the endpoint is absent and the worker has never been demanded.
5. dead when the endpoint is absent and the worker has been demanded.

Endpoint reachability takes precedence over the demanded flag so an inherited or pre-existing reachable worker cannot be described as cold. Busy takes precedence over stale probe-failure evidence. The classifier must not consult last-state as its primary signal because health checking deliberately records intermediate dead observations there.

Both anvil-worker-status and anvil-worker--tool-probe will use this classifier. Probe output will include:

- the primary state;
- demanded=yes or demanded=no;
- last=alive, last=dead, or last=unknown;
- the PID only for a reachable endpoint;
- probe-failures=N/LIMIT only when failures are recorded.

The probe remains non-mutating and nonblocking. Each status path performs the
reporting-only endpoint check exactly once per worker.

### Compatibility

Worker spawning, health recovery, ownership checks, grace periods, and metrics remain unchanged. This is an observability correction, not a second lifecycle state machine.

## Component 2: Bounded File and Tool Results

### Oversized inline file reads

Add `anvil-file-max-inline-read-bytes`, defaulting to 1,048,576 bytes. It
limits both an unbounded inline body and the raw bytes retained for a requested
page.

A bounded request requires a positive integer limit. A supplied fractional,
zero, negative, or otherwise invalid limit, and a non-integer or negative
offset, fail before any file body is loaded.

For an unbounded request with the guard enabled, `anvil-file-read` resolves the
regular target once. A size check may reject an already-oversized stable target
without loading it, but that check is only an optimization. The authoritative
operation inserts at most maximum-plus-one raw bytes from one file open into the
actual result buffer. If more than the configured maximum is observed, it
erases the prefix and signals a tool error containing:

- the exact stable stat size when available, otherwise an observed lower bound;
- the configured inline limit;
- an instruction to retry with offset and limit;
- a concrete first-page example using offset 0 and limit 200.

The rejection never invokes the existing unbounded full-body loader, retains at
most maximum-plus-one bytes, and never includes file contents. The bounded
read, rather than a pathname stat, is the safety boundary, so symlink retarget,
replacement, or growth between inspection and open cannot cause a large body
load.

A caller that supplies a positive integer limit takes a new streaming page
path, never the existing full-body loader. It reads fixed-size raw chunks,
counts newline boundaries across chunks for the exact `total-lines`, and
retains only the requested lines up to maximum-plus-one bytes. It then decodes
only that page as UTF-8. A page over the byte cap is erased and fails with
guidance to lower `limit` or use a filtered/region interface. The scan compares
the resolved target's identity and generation before and after; replacement or
growth that would make the result inconsistent fails with a content-free retry
error. The scan freezes the initial byte size and never reads beyond it, so a
concurrently growing file cannot create an unbounded loop; final generation
validation then rejects the changed result. Every sixteen chunks it yields for
one millisecond through `accept-process-output`, allowing root watchdog timers
to run during a very large count without retaining more data. Peak retained
file data is one chunk plus maximum-plus-one page bytes. The internal constants
are a 65,536-byte chunk, sixteen chunks per yield, and a 0.001-second yield so
tests can assert the bounds exactly.

Supplying offset alone does not bypass the guard because the result remains
unbounded. Small unbounded reads are decoded from the already-bounded prefix
without reopening the file. A nil or non-positive configured limit explicitly
restores the legacy full-loader behavior for installations that require it.

The file-read tool description will state that large files require pagination.

### Generic inline-result boundary

Add `anvil-server-max-inline-result-bytes`, defaulting to 2,097,152 bytes. The
value limits the projected UTF-8 bytes of the result after JSON string escaping,
not merely the raw string. A streaming counter implements the same escaping
rules as Emacs JSON encoding and stops at limit-plus-one without constructing
the expanded string. Tests compare it with `json-encode-string` over control,
quote, backslash, ASCII, multibyte Unicode, and every unibyte high value
0x80-0xFF; the latter use Emacs's octal JSON escaping rather than ordinary
UTF-8 byte counting.

After a successful handler result becomes `result-text`, but before disclosure
processing, metrics payload recording, MCP wrapping, logging, hooks, and final
JSON-RPC encoding, measure its projected JSON-string size. On overflow, discard
it and route a bounded tool error through the ordinary `isError` response path.
Measure the disclosure-transformed text again before any downstream seam, so a
transform cannot expand a previously safe value past the final response bound.

Apply the same non-signaling sanitizer to every tool condition branch—
`anvil-server-tool-error`, invalid parameters, `quit`, and generic errors—and
to tool-not-found responses. It returns both bounded response text and a newly
constructed sanitized condition for downstream hooks. In particular,
`anvil-server-with-error-handling` must sanitize before its persistent
tool-error hook, rather than relying on the later dispatcher branch. No raw
condition cell reaches any hook or log. Non-string condition data becomes a
fixed diagnostic without printing the object. An internal success-overflow
condition has its own branch and cannot recurse through the tool-error
machinery.

Sanitization preserves the call-site source and only an allowlisted condition
symbol needed by harness classification: `void-function`,
`error-process-exited-abnormally`, `file-missing`, `file-error`,
`wrong-type-argument`, `wrong-number-of-arguments`, `error`, and the fixed
classes `anvil-server-tool-error`, `anvil-server-invalid-params`, and
`anvil-server-inline-result-too-large`. Every other symbol becomes canonical `error`,
so a dynamically interned request-derived condition name cannot be persisted.
It preserves no request-owned condition data. It also
preserves hook cardinality: macro-wrapped, generic, and invalid-parameter
failures call the hook exactly once; direct tool-error, quit, and tool-not-found
paths remain unhooked. Internal sanitizer faults collapse to a fixed empty-text
condition and placeholder label without escaping to the outer handler.

The protected `tools/call` boundary begins before parameter extraction,
registry lookup, and lazy-placeholder loading. Malformed or oversized params
and lazy-loader failures therefore enter the same sanitizer-controlled error
branches and cannot fall through to the server-wide internal-error formatter.
Before any `alist-get`, validate that params is an object/alist and signal
`anvil-server-invalid-params` with a fixed, content-free string otherwise. A
generic lazy-loader error preserves the JSON-RPC internal-error envelope; a
loader that explicitly signals `anvil-server-tool-error` preserves the MCP
`isError` envelope.

Every replacement diagnostic contains only:

- the registered tool label when it matches
  `[A-Za-z0-9][A-Za-z0-9._/-]{0,127}`, otherwise the fixed
  `<oversized-tool-id>` placeholder;
- whether the handler returned or which fixed condition class rejected text;
- the observed raw UTF-8 and projected escaped byte counts;
- the configured projected-byte limit;
- guidance to use a paginated, filtered, tee, or asynchronous interface.

Neither rejected success text nor rejected error text may reach disclosure,
metrics payload recording, hooks, logs, MCP wrapping, or final JSON encoding.
A nil or non-positive configured limit disables size rejection and retains
legacy response text and envelopes; error hooks still receive a newly
constructed condition and fixed-grammar tool label rather than original
request-owned objects.

The file-specific guard is intentionally lower and earlier: it prevents the
demonstrated unbounded load. The server boundary protects every tool-derived
text path from escape expansion and expensive final response encoding.

### Shell behavior

Do not reroute shell-run or shell-tee-grep. Their process wait already yields to Emacs, capture is bounded, and filtering operates on the bounded captured prefix. Native execution remains the correct path for long-lived process supervision; the Anvil worker pool is not involved in these shell tools.

## Component 3: Structured Watchdog Attribution

### Per-launch capabilities

Before each daemon start, the supervisor creates a fresh 32-lowercase-hex run
ID and a private pipe. Both pipe ends are nonblocking. The supervisor retains
the read end on that exact process object and passes only the write end through
the trusted launcher chain with `pass_fds`. It also sets the launcher-only
`ANVIL_EMACS_WATCHDOG_SUPERVISED=1` marker. The descriptor is duplicated above
9 so the existing parent guard cannot confuse it with lock descriptors 8 and 9.

The existing shared-host launch path has no process supervisor. When and only
when the supervised marker and both supervisor capability values are absent,
the launcher generates its own run ID and private nonblocking discard pipe so
the shared daemon and watchdog remain functional without publishing
`last_watchdog`. In marked supervised mode, both capability values are
mandatory. Any other partial or unmarked capability combination is a
configuration error. Hera's per-agent path must use the supervisor-owned pipe;
no fallback is allowed there.

This all-absent shared-host path is compatibility-only: it has no persistent
status owner to consume an event or publish attribution. Full shared-host
attribution would require placing a persistent host supervisor around the
daemon and is outside this per-agent failure fix.

For the compatibility pipe, the monitor retains both nonblocking ends and root
inherits neither. At most one atomic event is written before monitor exit, so
the unread discard pipe cannot fill or raise SIGPIPE.

The launcher validates that the inherited descriptor is above 9, nonblocking,
write-only, and the expected open pipe, removes its supervised-mode and
descriptor environment variables, and creates a private mode-0600
Unix-domain activity endpoint in the canonical mode-0700 per-agent runtime
directory. After forking the monitor, the root branch closes the event pipe and
all monitor-owned listener descriptors before exec. The monitor keeps the event
write end; arbitrary root, worker, offload, host, shell, and other subprocesses
inherit neither telemetry descriptor nor the eventual connected activity
socket.

Root Emacs receives exactly two new generated, non-secret telemetry environment
values: `ANVIL_EMACS_WATCHDOG_ACTIVITY_SOCKET` and
`ANVIL_EMACS_WATCHDOG_RUN_ID`. It captures them into private variables and
removes both keys from the current and immutable baseline process environments
before any request-controlled subprocess can start. Existing pulse, lease, and
other synchronization environment values remain unchanged.

### Root activity transport

Root connects once with `make-network-process` using `:family 'local`, the
socket pathname as `:service`, `:coding '(utf-8-unix . utf-8-unix)`,
`:noquery t`, and `:buffer nil`. It omits `:type`; the packaged macOS
Emacs rejects an explicit `stream` while correctly defaulting to a stream. The
resulting descriptor is close-on-exec.

The monitor's listener and accepted connection are nonblocking and are serviced
inside the existing watchdog loop. Each tick performs at most one nonblocking
accept and drains at most 4,096 available bytes. It processes complete newline
frames in order while retaining no more than 1,024 bytes of one partial frame.
No connection, a silent peer, a partial frame, peer disappearance, or multiple
coalesced frames can delay deadline, lock, refresh, parent-liveness, or
monitor-exit checks. Oversized or malformed frames close the peer without
weakening the watchdog. After accepting the one root connection, the monitor
unlinks the socket pathname; later replacement cannot redirect the established
connection.

The monitor initializes its in-memory activity to sequence 0, phase `startup`,
method `none`, and tool null as soon as the root PID is known. Root messages
then use schema version 1 with exactly these JSON keys:

- `schema_version`: integer `1`;
- `run_id`: the launch run ID;
- `daemon_pid`: the positive root daemon PID;
- `sequence`: a non-negative integer strictly greater than the last accepted
  sequence;
- `phase`: one of `startup`, `parse`, `dispatch`, `tool-call`,
  `result-encode`, `response-write`, or `idle`;
- `method`: one of `none`, `initialize`,
  `notifications/initialized`, `ping`, `tools/list`, `tools/call`,
  `resources/list`, `resources/read`, `resources/templates/list`, or
  `other`;
- `tool`: JSON null, or a currently registered identifier matching
  `[A-Za-z0-9][A-Za-z0-9._/-]{0,127}`;
- `phase_started_unix_ms` and `observed_at_unix_ms`: non-negative integer
  Unix milliseconds.

Each newline-terminated message is at most 1,024 UTF-8 bytes. The decoder
rejects duplicate object keys, JSON non-finite constants, bool in every integer
field, unknown keys, invalid UTF-8, and every other schema violation. It accepts
only the expected run ID and daemon PID. Invalid or partial input never replaces
the last valid in-memory activity.

Initial connection failure or a later `process-send-string` failure is
observability-only: root closes the process, disables further activity sends,
emits at most one constant diagnostic, and continues the original MCP request.
It never includes socket paths or exception text.

Root caches the last emitted `(phase method tool)` tuple. An unchanged tuple is
a semantic duplicate: it sends no frame and does not advance sequence or reset
`phase_started_unix_ms`. Thus the pre/post-disclosure result guards and the
macro/dispatcher sanitizer pair still represent one continuous
`result-encode` phase. A tuple changed by an intervening boundary emits
normally.

### Exact phase transitions

Nix-local instrumentation uses these exact boundaries:

| Boundary | Activity emitted before proceeding |
| --- | --- |
| monitor initialization after the root PID is known | `startup`, method `none`, tool null, sequence 0 |
| around `anvil-server-process-jsonrpc` | `parse`, method `none`, tool null; an outer `unwind-protect` always emits `idle` and clears method/tool |
| around `anvil-server--validate-and-dispatch-request` | `dispatch` with the request method mapped through the fixed enum |
| around `anvil-server--handle-tools-call` | `tool-call`, method `tools/call`, and the tool only after registry lookup plus telemetry grammar validation |
| around `anvil-server--enforce-inline-result-limit` and the shared tool-error bounding helper | `result-encode` for successful and every tool-derived error text |
| around `anvil-server--jsonrpc-response`, `anvil-server--jsonrpc-error`, and `anvil-server--jsonrpc-response-from-result-json` | `response-write`, preserving the current sanitized method/tool |

Parse failure therefore emits `parse, response-write, idle`; an unknown method
emits `parse, dispatch, response-write, idle`; a cached `tools/list` response
emits the same sequence with method `tools/list`; and a successful tool or any
tool error emits
`parse, dispatch, tool-call, result-encode, response-write, idle`. A non-local
exit still ends in `idle`. The forced non-yielding and yielding dispatch smoke
scenarios call the registered `emacs-eval` tool, so their last pre-kill
activity is exactly phase `tool-call`, method `tools/call`, tool
`emacs-eval`; their causes differ as `heartbeat-timeout` and
`dispatch-timeout`.

Arguments, request IDs, paths, expressions, raw JSON, results, and
caller-derived environment values are prohibited. Activity remains separate
from the synchronization lease, so updates never advance lease generation or
reset `dispatch_started`.

### Restart-safe socket lifecycle

All stale socket handling is relative to an
`O_DIRECTORY|O_NOFOLLOW` descriptor for the validated runtime directory. The
launcher may unlink only an expected, same-UID, single-link, mode-0600 socket. A
symlink, wrong type, regular file at the socket name, wrong owner, permissive
mode, or unexpected link count is a prelaunch configuration failure.

Before publishing or starting either a per-agent or shared-host runtime, the
supervisor validates the prospective activity pathname against the platform
`sockaddr_un` byte ceiling along with every root and worker Emacs socket. A
path that fits the older `/emacs/server` suffix but not
`.anvil-root-activity.sock` fails before daemon launch.

The launcher uses exclusive bind semantics and removes its owned socket after
acceptance and on startup failure while ownership is still provable. Before a
monitor-initiated SIGKILL it makes one best-effort, identity-checked unlink
attempt before writing the event and killing root. The parent guard can kill
the whole target process group after an external root exit, so no post-exit
monitor cleanup is assumed in that case. A following launch reclaims a stale
socket only after the same exact type, owner, mode, link-count, and pathname
identity checks; it never removes a hostile replacement. Consecutive launches,
including failure before acceptance, must therefore recover without a benign
`EEXIST` failure while retaining process-group containment.

### Watchdog event channel

Immediately after a failure is revalidated and immediately before SIGKILL, the
monitor constructs one schema-version-1 event containing exactly:

- `schema_version`, `run_id`, and `daemon_pid` under the activity
  constraints;
- `cause`: one of `startup-timeout`, `heartbeat-timeout`,
  `dispatch-timeout`, `lock-integrity-failure`,
  `monitor-state-invalid`, `durable-refresh-failure`, or
  `monitor-internal-error`;
- `phase`: an activity phase or `unknown`;
- `method`: one activity method enum value;
- `tool`: null or a telemetry-safe identifier; if including the identifier
  would exceed the atomic event ceiling, the monitor emits null instead;
- `observed_at_unix_ms` and `daemon_uptime_ms`: non-negative integers;
- `heartbeat_age_ms`, `heartbeat_limit_ms`, `dispatch_age_ms`, and
  `dispatch_limit_ms`: non-negative integers when that deadline has an
  anchor, otherwise null.

The event is canonical compact UTF-8 JSON no larger than 512 bytes, the Darwin
pipe atomic-write bound verified at runtime with `fpathconf`. The same
duplicate-key, non-finite-constant, bool, exact-key, enum, and size rules apply.
Durations are integer milliseconds from monotonic time; only
`observed_at_unix_ms` is wall-clock time.

The monitor explicitly ignores SIGPIPE. `write_watchdog_event` performs one
nonblocking write to the private event pipe. A partial write, full/broken pipe,
encoding error, or any other diagnostic failure is discarded; no retry, file
write, or `fsync` occurs in the monitor. The kill path then sends SIGKILL
unconditionally. Thus telemetry cannot block the watchdog or suppress
termination.

When heartbeat and dispatch deadlines have both expired, the cause is whichever
absolute deadline elapsed first. Integrity, refresh, and internal failure sites
use their exact named causes rather than a timeout fallback.

### Supervisor status and probe

The supervisor retains the expected run ID and event read descriptor on each
process object. After that exact daemon exits,
`read_watchdog_event(process, expected_pid, expected_run_id)` reads at most 513
nonblocking bytes from only that process's pipe and validates the complete
strict schema. A missing, partial, oversized, wrong-PID, or wrong-run record is
rejected.

One `finalize_daemon_exit` path owns event ingestion and descriptor closure for
both naturally observed exits and every explicit stop/reap before the process
object is discarded. Each exit replaces the always-present status field: a
valid record becomes `last_watchdog`; no valid record sets it to JSON null.
Historical events never survive to become the apparent cause of a later
unrelated restart. An intentional stop clears `last_watchdog` without
incrementing restart accounting; naturally observed exits retain
`restart_reason` as `daemon-exited:CODE`.

The supervisor's outer shutdown path is included. While it still owns the
supervisor lock, it stops and finalizes the daemon, sets `last_watchdog` to
null, and publishes a terminal daemon-null status with unchanged restart
accounting before preserving or removing lifecycle state. If bounded status
publication fails, it safely invalidates that exact status entry. A propagated
stop timeout or finalizer failure takes the same fail-closed invalidation path
while the lock is still held; the original lifecycle error is not swallowed.
The supervisor never leaves a live, trusted record carrying an older watchdog
event when it could not prove the daemon exit was finalized.

A new read-only supervisor `--probe-summary` mode opens the private runtime
directory and status entry with directory-relative
`O_NOFOLLOW|O_NONBLOCK`, then validates regular type, owner, mode, link count,
pathname identity, bounded strict JSON, lifecycle identities, enums, and tool
grammar. It emits exactly one ASCII line no longer than 256 bytes:

    root-restarts=N cause=CAUSE phase=PHASE tool=TOOL-OR-none

When valid status has no current-exit watchdog, cause is `none`, phase
`unknown`, and tool `none`. Invalid or replaced status fails closed without
printing raw JSON or exception data.

Nix-local probe advice invokes the helper only through the existing responsive
bounded process runner with a two-second wall limit, 257-byte stdout cap, and
zero-byte stderr allowance. It accepts only exit zero and exactly one
newline-terminated ASCII line no longer than 256 bytes; timeout, overflow,
stderr, malformed output, or any other failure appends only
`root-summary=unavailable`. Emacs never parses or renders raw supervisor JSON.

For runtime provenance, `anvil-mcp --version` prints exactly:

    anvil-mcp VERSION (anvil REV; dedicated Emacs)

where REV is the full pinned 40-hex revision. Package tests and production
smoke compare it with `currentAnvilVersion`, `currentAnvilRev`, and the
pushed Anvil tip.

## Security and Privacy

No diagnostic artifact may contain user-controlled argument values. Tests use a
unique sentinel in a path, expression, and request argument and assert that it
is absent from the event pipe, supervisor status, probe output, and diagnostic
logs.

The event pipe is a per-launch capability never inherited by root Emacs, and
the transient activity socket is private and unlinked after acceptance.
Directory-relative `O_NOFOLLOW|O_NONBLOCK` is mandatory before validating any
existing status entry. Duplicate JSON keys, non-finite constants, bool
masquerading as integers, unexpected ownership or mode, wrong daemon PIDs,
stale run identifiers, unsafe tool identifiers, unknown enums, and oversized
fields fail closed.

Unsafe prelaunch directories or stale socket entries are configuration errors
and prevent an unsafe daemon start. After launch, activity connection/send
failure, event-pipe overflow, malformed telemetry, and status-ingestion failure
affect observability only: they cannot abort a valid MCP request, block or
suppress SIGKILL, or disable supervisor restart behavior.

## Testing Strategy

Implementation follows witnessed test-first cycles.

### Anvil tests

In `tests/anvil-worker-test.el`:

- table-test all five states and their precedence;
- invoke both real status paths, fail on spawn, full liveness, deletion, logging,
  or worker mutation, and assert exactly one reporting-only endpoint check per
  worker;
- assert exact state, demanded, last-state, conditional PID, and bounded
  failure-count rendering.

In `tests/anvil-file-test.el`:

- reject an oversized stable file before the full loader runs;
- prove the authoritative read requests at most maximum-plus-one raw bytes and
  rejects growth/replacement even when the preliminary stat is small;
- prove a symlink to an oversized regular target cannot bypass the guard;
- exercise the real registered wrapper with fractional, zero, negative, and
  valid positive-integer limits plus invalid offsets before body loading;
- prove positive-limit reads use fixed chunks, never call the full-body loader,
  retain at most one chunk plus maximum-plus-one page bytes, return exact total
  lines, reject an oversized page, and detect replacement;
- preserve a bounded page, a small unbounded read, and explicitly disabled
  legacy behavior.

In `tests/anvil-test.el`:

- register handlers that return oversized text and signal oversized tool,
  invalid-parameter, quit, and generic error text;
- register a handler using `anvil-server-with-error-handling` and prove its
  persistent telemetry hook receives only the reconstructed sanitized
  condition;
- exercise malformed oversized params and a lazy-placeholder loader that
  signals oversized text, proving neither escapes to the outer internal-error
  formatter;
- assert every response preserves its existing MCP or JSON-RPC envelope with
  bounded text, and the rejected payload is absent from disclosure, metrics,
  hooks, logs, wrapping, and JSON encoding;
- assert exact error-hook cardinality and unchanged harness classification from
  the sanitized condition/source;
- prove a dynamically defined sentinel condition symbol canonicalizes to
  `error`, while known `void-function` and `wrong-type-argument` symbols retain
  their expected classifier result;
- compare projected JSON-string byte counts with `json-encode-string` over an
  escape-heavy oracle corpus, including every unibyte value 0x80-0xFF;
- rebind both JSON encoders to fail while exercising the streaming counter, and
  inject counter/label failures while proving the sanitizer cannot signal;
- stub disclosure to expand an initially safe result and prove the second guard
  rejects it before metrics, hooks, wrapping, or response encoding;
- assert exact projected-byte and multibyte boundaries, safe tool labels,
  non-string condition data, and disabled legacy behavior.

Run each focused selector during its red-green cycle, followed by `make test`,
`make test-all`, `make lint`, and `make byte-compile` under the exact
packaged Emacs path evaluated from the Nix deployment.

### Nix tests

Refactor the generated-launcher tests into named unittest groups so protocol,
transport, lifecycle, and cause behavior each witnesses RED before its
implementation. Exercise Nix-local phase/probe Elisp through a separate ERT
file that loads the exact generated telemetry init. Load the shared hyphenated
Python support script through an explicit store path and `importlib`; assert
support, launcher, telemetry-init, runtime-Emacs, and packaged-Anvil provenance.

Watchdog coverage includes:

- all cause enums, simultaneous-deadline precedence, and startup phase
  attribution;
- one nonblocking event-pipe write followed unconditionally by SIGKILL,
  including full, broken, partial, and encoding-failure cases;
- no client, silent client, partial/coalesced/oversized frames, peer loss, and
  root-side connect/send failure without request failure;
- socket-path boundary preflight, hostile stale entries, two launches, and
  unlink/rebind after acceptance plus safe next-launch recovery after an
  external root exit;
- exact phase sequences for parse failure, unknown method, cached tools/list,
  successful tool, every error path, and non-local exit;
- duplicate-helper cases proving pre/post disclosure and macro/dispatcher
  sanitization emit only one continuous `result-encode` transition;
- strict schema, tool grammar, size ceilings, secret non-retention, and proof
  that root/worker/offload/host/shell/arbitrary children inherit no telemetry
  descriptor or environment key.

Supervisor coverage includes:

- valid per-process event-pipe adoption and missing, partial, stale-run,
  wrong-PID, duplicate-key, bool, enum, grammar, and length rejection;
- valid-watchdog then unrelated natural-exit and intentional-stop clearing,
  with intentional stops leaving restart accounting unchanged;
- externally requested supervisor shutdown finalization and terminal status
  publication/invalidation while the supervisor lock is still held;
- injected stop timeout and event-finalizer failure proving both invalidate the
  exact status identity and propagate the original lifecycle failure;
- status-helper regular-file validation through
  `O_NOFOLLOW|O_NONBLOCK`, including FIFO/socket/symlink/path replacement;
- bounded helper output, hung/noisy helper containment, and exact
  revision-bearing `--version` output.

Forced smoke covers exact heartbeat and dispatch causes with
`tool-call/tools/call/emacs-eval`, exactly one validated probe-summary line,
and absence of a unique secret sentinel. Run focused Python groups and ERT,
the complete Darwin `anvil-mcp-dedicated` check, then `./build system`
before switching.

## Delivery Sequence

1. Commit, independently audit, and push this corrected specification and plan.
2. Create the isolated Anvil worktree from the verified remote planning commit and prove the baseline.
3. Add failing Anvil tests and confirm their expected failures.
4. Implement worker reporting and bounded result behavior; run all Anvil gates.
5. Commit and audit the Anvil changes, rebase once onto the fetched parent, rerun the full gates, and push the definitive Anvil revision.
6. Pin that published revision and archive hash in an isolated Nix worktree, using its committer timestamp and package header version as metadata provenance.
7. Add failing Nix watchdog and supervisor tests, then implement the generated diagnostics.
8. Rebase the Nix branch onto origin/main, run all Anvil-MCP checks and ./build system, audit, and push only task-owned changes.
9. Switch the Hera Darwin configuration.
10. Restart or reacquire an agent bridge so it receives the new generation.
11. Production-smoke cold worker reporting, paginated-read rejection, watchdog status schema, and normal tool execution.
12. Confirm both repositories are clean or contain only the user's pre-existing changes and are up to date with their remotes.

## Acceptance Criteria

- A fresh lazy pool reports cold rather than dead.
- A demanded worker with no reachable endpoint reports dead.
- A reachable worker reports exactly one of alive, busy, or unresponsive.
- Probing never starts workers, performs a blocking full liveness check, deletes
  a server file, logs, or mutates worker state.
- An oversized unbounded file-read never invokes the full-body loader, retains
  at most maximum-plus-one bytes, and gives explicit pagination instructions.
- Only a positive integer limit bypasses the unbounded guard; invalid limit and
  offset values fail before body loading.
- Oversized returned text and every tool-derived error path are rejected by
  projected JSON-string size before downstream hooks, logs, wrapping, or final
  encoding, and never expose their payloads.
- Every watchdog kill writes a validated cause and last sanitized activity when
  the private nonblocking event pipe accepts its one atomic message; telemetry
  failure never delays or suppresses SIGKILL.
- Supervisor status preserves `daemon-exited:CODE`, accepts an event only from
  that process's matching run/PID/schema pipe, and clears historical watchdog
  data on a later exit without a valid event.
- The administrative probe remains one bounded line for every valid or adversarial tool identifier.
- Root activity connection/send failure and a hung/noisy summary helper cannot
  break or starve ordinary MCP requests.
- The deployed `--version` output contains the exact pinned 40-hex Anvil revision.
- No request content or secret sentinel is retained.
- The Anvil and Nix quality gates pass.
- Hera runs the new pinned Anvil generation and production probes demonstrate the corrected behavior.
