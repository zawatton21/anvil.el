# Specification: worker-pool-spawn-deferred

## Purpose

Define non-blocking startup behavior for `anvil-worker`'s subprocess
pool so that enabling anvil on a populated project never stalls the
Emacs daemon's main thread, especially on Windows where
`file-notify-add-watch` and blocking probes amplify startup cost.

## Requirements

### Requirement: `anvil-worker-enable` MUST return promptly

The system SHALL return from `anvil-worker-enable` within a small
constant budget — under one second on a cold machine — regardless of
the configured pool size or the state of any pre-existing worker
server-files. No worker subprocess SHALL be spawned on the caller's
thread before `anvil-worker-enable` returns.

#### Scenario: Enable returns promptly on a populated pool

- **GIVEN** per-lane pool sizes configured so the total pool is five
  or more workers
- **AND** one or more stale worker server-files from a prior killed
  daemon
- **WHEN** `anvil-worker-enable` is called during daemon startup
- **THEN** the call returns in under one second
- **AND** no `start-process` has been issued on the caller's thread

### Requirement: Worker liveness checks on the startup path MUST be non-blocking

The system SHALL determine worker liveness during startup using only
file-existence and OS-level PID checks. It SHALL NOT wait on a
subprocess (via `accept-process-output`, synchronous `call-process`,
or socket round-trip) on the startup path.

#### Scenario: Stale server-file does not hang the startup path

- **GIVEN** a worker server-file whose recorded PID is no longer
  running
- **WHEN** the startup path checks that worker's liveness
- **THEN** the check returns a falsy result within milliseconds
- **AND** the daemon remains responsive

### Requirement: Worker status reporting MUST be truthful and non-mutating

The system SHALL distinguish a never-demanded worker with no endpoint from a
demanded worker whose endpoint is absent.  A reachable endpoint SHALL be
reported according to its recorded busy and full-probe-failure evidence.

#### Scenario: Status and MCP probe report worker lifecycle state

- **GIVEN** a never-demanded worker whose endpoint is absent
- **THEN** status and probe report that worker as `cold`
- **GIVEN** a demanded worker whose endpoint is absent
- **THEN** status and probe report that worker as `dead`
- **GIVEN** a worker whose endpoint is reachable
- **THEN** status and probe report that worker as `alive`, `busy`, or
  `unresponsive`, with `busy` taking precedence over stale probe failures
- **AND** each reporting path observes the endpoint exactly once per worker
- **AND** reporting performs no spawn, full liveness probe, deletion, log
  write, or worker mutation

### Requirement: Filesystem watcher registration MUST NOT run synchronously at startup on Windows

The system SHALL avoid calling `file-notify-add-watch` from the
startup path on Windows, where each call costs on the order of one
second per directory. Features that need to observe file changes
SHALL use periodic `mtime` scanning, or SHALL schedule the watch
registrations deferred so they do not accumulate as a single
synchronous cost.

#### Scenario: Enabling anvil with many watchable directories does not freeze

- **GIVEN** anvil-org-index is configured to watch 25 or more
  directories on a Windows host
- **WHEN** `anvil-enable` is called
- **THEN** the synchronous portion of enable completes in under two
  seconds
- **AND** no `file-notify-add-watch` call is issued on the caller's
  thread before `anvil-enable` returns

## Non-goals

- Does **not** constrain steady-state dispatch latency, pool-size
  defaults, or lane-classifier behavior — those are separate specs.
- Does **not** require any particular post-startup crash-detection
  strategy; only the startup path is constrained.
- Linux/macOS MAY use the same approach but are not required to.
