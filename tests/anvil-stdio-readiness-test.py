#!/usr/bin/env python3
"""Deterministic readiness and atomic-dispatch regressions for anvil-stdio."""

from __future__ import annotations

import json
import os
import selectors
import signal
import stat
import subprocess
import sys
import tempfile
import time
from pathlib import Path
import re
import shutil
from typing import BinaryIO


def make_executable(path: Path, source: str) -> None:
    path.write_text(source, encoding="utf-8")
    path.chmod(path.stat().st_mode | stat.S_IXUSR)


def percent_wire(document: dict[str, object]) -> str:
    return "".join(
        f"%{byte:02X}"
        for byte in json.dumps(
            document, separators=(",", ":"), ensure_ascii=False
        ).encode("utf-8")
    )


def read_count(path: Path) -> int:
    try:
        return int(path.read_text(encoding="utf-8"))
    except (FileNotFoundError, ValueError):
        return 0


def process_alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def wait_process_dead(pid: int, timeout: float = 2.0) -> bool:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if not process_alive(pid):
            return True
        time.sleep(0.02)
    return not process_alive(pid)


def write_fake_emacsclient(path: Path) -> None:
    source = r"""#!__PYTHON__
import base64
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import sys


def bump(name):
    path = Path(os.environ[name])
    try:
        value = int(path.read_text(encoding="utf-8"))
    except FileNotFoundError:
        value = 0
    value += 1
    path.write_text(str(value), encoding="utf-8")
    return value


expression = sys.argv[-1]
if os.environ.get("FAKE_EXPRESSION"):
    Path(os.environ["FAKE_EXPRESSION"]).write_text(expression, encoding="utf-8")
ready_prefix = (
    "(if (and (fboundp 'anvil-headless--ready-p) "
    '(anvil-headless--ready-p "anvil")) '
)
sentinel_suffix = '"anvil-mcp-headless-not-ready")'


def guarded_true_branch():
    if not expression.startswith(ready_prefix):
        return None
    tail = expression[len(ready_prefix) :]
    if tail.endswith(sentinel_suffix):
        branch = tail[: -len(sentinel_suffix)]
        return branch[:-1] if branch.endswith(" ") else None

    cleanup_marker = (
        " (progn (let ((delete-by-moving-to-trash nil)) "
        "(delete-file "
    )
    cleanup_start = tail.rfind(cleanup_marker)
    if cleanup_start < 0 or not tail.endswith("))"):
        return None
    cleanup_branch = tail[cleanup_start + 1 : -1]
    cleanup_pattern = (
        r"\(progn \(let \(\(delete-by-moving-to-trash nil\)\) "
        r"\(delete-file \(decode-coding-string "
        r"\(base64-decode-string \"[A-Za-z0-9+/=]+\"\) "
        r"'utf-8 t\)\)\) "
        r"\"anvil-mcp-headless-not-ready\"\)"
    )
    if re.fullmatch(cleanup_pattern, cleanup_branch) is None:
        return None
    return tail[:cleanup_start]


def execute_file_expression(*, ready=True):
    real_emacs = os.environ["FAKE_REAL_EMACS"]
    response_payload = base64.b64encode(
        os.environ["FAKE_RESPONSE_JSON"].encode("utf-8")
    ).decode("ascii")
    delay = float(os.environ.get("FAKE_REAL_EMACS_DELAY_SEC", "0"))
    program = (
        "(progn "
        + (f"(sleep-for {delay!r}) " if delay else "")
        + f"(fset 'anvil-headless--ready-p (lambda (_server) {'t' if ready else 'nil'})) "
        "(fset 'anvil-server-process-jsonrpc "
        "(lambda (_request _server) "
        f"(decode-coding-string (base64-decode-string "
        f"{json.dumps(response_payload)}) 'utf-8 t))) "
        f"(prin1 {expression}))"
    )
    root = Path(os.environ["FAKE_ANVIL_ROOT"])
    completed = subprocess.run(
        [
            real_emacs,
            "--batch",
            "-Q",
            "-L",
            str(root),
            "-l",
            str(root / "anvil-server.el"),
            "--eval",
            program,
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        check=False,
    )
    if completed.returncode != 0:
        sys.stderr.write(completed.stderr)
        raise SystemExit(completed.returncode)
    sys.stdout.write(completed.stdout)


def spawn_stubborn_descendant():
    descendant = subprocess.Popen(
        [
            sys.executable,
            "-I",
            "-S",
            "-c",
            (
                "import signal; "
                "signal.signal(signal.SIGTERM, signal.SIG_IGN); "
                "signal.signal(signal.SIGHUP, signal.SIG_IGN); "
                "signal.signal(signal.SIGINT, signal.SIG_IGN); "
                "signal.signal(signal.SIGUSR1, signal.SIG_IGN); "
                "signal.pause()"
            ),
        ],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    Path(os.environ["FAKE_DESCENDANT_PID"]).write_text(
        str(descendant.pid), encoding="utf-8"
    )


def atomically_guards(body):
    branch = guarded_true_branch()
    if branch is None:
        return False
    if body != "anvil-server-process-jsonrpc":
        return branch == f'(progn {body} "anvil-mcp-lifecycle-complete")'
    staged_call = "(anvil-server-process-jsonrpc-to-file "
    return (
        branch.count(staged_call) == 1
        and (
            branch.startswith(staged_call)
            or (
                branch.startswith("(let* ")
                and "(anvil-server-process-jsonrpc-to-file anvil-request " in branch
            )
        )
    )


if "anvil-server-process-jsonrpc" in expression:
    if os.environ.get("FAKE_ACTIVE_CHILD"):
        active_child = Path(os.environ["FAKE_ACTIVE_CHILD"])
        active_child_pending = active_child.with_name(
            f".{active_child.name}.{os.getpid()}.pending"
        )
        active_child_pending.write_text(
            f"{os.getpid()} {os.getpgrp()}\n",
            encoding="ascii",
        )
        os.replace(active_child_pending, active_child)
    if os.environ.get("FAKE_ATOMIC_NOT_READY") == "1":
        if not atomically_guards("anvil-server-process-jsonrpc"):
            bump("FAKE_DISPATCH_COUNT")
            Path(os.environ["FAKE_EARLY_DISPATCH"]).write_text(
                "atomic wrapper missing\n", encoding="utf-8"
            )
            print(f'"{os.environ["FAKE_RESPONSE_WIRE"]}"')
        else:
            Path(os.environ["FAKE_GUARD_OBSERVED"]).write_text(
                "jsonrpc\n", encoding="utf-8"
            )
            execute_file_expression(ready=False)
        raise SystemExit(0)
    probes = int(Path(os.environ["FAKE_PROBE_COUNT"]).read_text())
    if probes <= int(os.environ.get("FAKE_NIL_BEFORE", "0")):
        Path(os.environ["FAKE_EARLY_DISPATCH"]).write_text(
            "dispatch before exact readiness\n", encoding="utf-8"
        )
        raise SystemExit(70)
    bump("FAKE_DISPATCH_COUNT")
    if os.environ.get("FAKE_HANG_DISPATCH") == "1":
        if os.environ.get("FAKE_TERM_COUNT"):
            signal.signal(
                signal.SIGTERM,
                lambda _signum, _frame: bump("FAKE_TERM_COUNT"),
            )
        elif os.environ.get("FAKE_IGNORE_TERM") == "1":
            signal.signal(signal.SIGTERM, signal.SIG_IGN)
        if os.environ.get("FAKE_DESCENDANT_PID"):
            spawn_stubborn_descendant()
        Path(os.environ["FAKE_HANG_PID"]).write_text(
            str(os.getpid()), encoding="utf-8"
        )
        if os.environ.get("FAKE_RUNNER_PID"):
            Path(os.environ["FAKE_RUNNER_PID"]).write_text(
                str(os.getppid()), encoding="utf-8"
            )
        if os.environ.get("FAKE_FORGED_RUNNER_RECORD") == "1":
            sys.stdout.buffer.write(b"\0" + b"0\n")
            sys.stdout.buffer.flush()
        while True:
            signal.pause()
    if os.environ.get("FAKE_EXIT_DESCENDANT") == "1":
        spawn_stubborn_descendant()
    if os.environ.get("FAKE_DISPATCH_ERROR") == "1":
        raise SystemExit(70)
    if os.environ.get("FAKE_MALFORMED_OUTPUT") == "1":
        print('"')
    else:
        execute_file_expression()
elif "(test-init)" in expression or "(test-stop)" in expression:
    kind = "init" if "(test-init)" in expression else "stop"
    count_name = "FAKE_INIT_COUNT" if kind == "init" else "FAKE_STOP_COUNT"
    not_ready = os.environ.get(f"FAKE_{kind.upper()}_NOT_READY") == "1"
    malformed = os.environ.get(f"FAKE_{kind.upper()}_MALFORMED") == "1"
    split = os.environ.get(f"FAKE_{kind.upper()}_SPLIT_SENTINEL") == "1"
    guarded = atomically_guards(f"(test-{kind})")
    if not_ready:
        if not guarded:
            bump(count_name)
            print('"anvil-mcp-lifecycle-complete"')
        else:
            Path(os.environ["FAKE_GUARD_OBSERVED"]).write_text(
                f"{kind}\n", encoding="utf-8"
            )
            execute_file_expression(ready=False)
    elif malformed or split:
        if guarded:
            Path(os.environ["FAKE_GUARD_OBSERVED"]).write_text(
                f"{kind}\n", encoding="utf-8"
            )
        bump(count_name)
        if split:
            sys.stdout.write('"anvil-mcp-lifecycle-\ncomplete"\n')
        else:
            print('"')
    else:
        bump(count_name)
        print('"anvil-mcp-lifecycle-complete"')
elif "anvil-headless--ready-p" in expression:
    attempt = bump("FAKE_PROBE_COUNT")
    if os.environ.get("FAKE_INVALID_OUTPUT") == "1":
        output = "t-garbage"
    elif (
        os.environ.get("FAKE_ALWAYS_NIL") == "1"
        or attempt <= int(os.environ.get("FAKE_NIL_BEFORE", "0"))
    ):
        output = "nil"
    else:
        output = "t"
    ending = "\r\n" if os.environ.get("FAKE_CRLF") == "1" else "\n"
    sys.stdout.write(output + ending)
else:
    print(f"unexpected expression: {expression}", file=sys.stderr)
    raise SystemExit(64)
""".replace("__PYTHON__", sys.executable)
    make_executable(path, source)


def write_fast_nil_emacsclient(path: Path, bash: str) -> None:
    """Write a shell-only not-ready probe for one-second budget tests."""
    make_executable(
        path,
        f"""#!{bash}
count=0
if [ -f "$FAKE_PROBE_COUNT" ]; then
    IFS= read -r count < "$FAKE_PROBE_COUNT" || count=0
fi
printf '%s\\n' "$((count + 1))" > "$FAKE_PROBE_COUNT"
printf 'nil\\n'
""",
    )


def write_fast_probe_frontend(path: Path, bash: str) -> None:
    """Answer readiness in shell and delegate stateful expressions to Python."""
    make_executable(
        path,
        f"""#!{bash}
expression=
for expression do :; done
case "$expression" in
*anvil-server-process-jsonrpc*|*test-init*|*test-stop*)
    exec "$FAKE_PYTHON_EMACSCLIENT" "$@"
    ;;
*anvil-headless--ready-p*)
    count=0
    fast_count=0
    if [ -f "$FAKE_PROBE_COUNT" ]; then
        IFS= read -r count < "$FAKE_PROBE_COUNT" || count=0
    fi
    if [ -f "$FAKE_FAST_PROBE_COUNT" ]; then
        IFS= read -r fast_count < "$FAKE_FAST_PROBE_COUNT" || fast_count=0
    fi
    [ "$count" -eq "$fast_count" ] || exit 70
    count=$((count + 1))
    printf '%s\\n' "$count" > "$FAKE_PROBE_COUNT"
    printf '%s\\n' "$count" > "$FAKE_FAST_PROBE_COUNT"
    if [ "${{FAKE_INVALID_OUTPUT:-0}}" = 1 ]; then
        output=t-garbage
    elif [ "${{FAKE_ALWAYS_NIL:-0}}" = 1 ] \
        || [ "$count" -le "${{FAKE_NIL_BEFORE:-0}}" ]; then
        output=nil
    else
        output=t
    fi
    if [ "${{FAKE_CRLF:-0}}" = 1 ]; then
        printf '%s\\r\\n' "$output"
    else
        printf '%s\\n' "$output"
    fi
    ;;
*)
    exec "$FAKE_PYTHON_EMACSCLIENT" "$@"
    ;;
esac
""",
    )


def strict_equal(actual: object, expected: object) -> bool:
    if type(actual) is not type(expected):
        return False
    if isinstance(expected, dict):
        return actual.keys() == expected.keys() and all(
            strict_equal(actual[key], value) for key, value in expected.items()
        )
    if isinstance(expected, list):
        return len(actual) == len(expected) and all(
            strict_equal(left, right) for left, right in zip(actual, expected)
        )
    return actual == expected


HARNESS_MAX_CAPTURE_BYTES = 1_048_576
HARNESS_CHILD_RECORD_MAX_BYTES = 128
HARNESS_SESSION_COLLECT_SECONDS = 4.0
HARNESS_SESSION_REAP_SECONDS = 0.5
HARNESS_SESSION_VALIDATE_SECONDS = 2.0
HARNESS_SESSION_SCHEDULING_GRACE_SECONDS = 0.5
HARNESS_SESSION_SCAN_SECONDS = (
    HARNESS_SESSION_COLLECT_SECONDS
    + HARNESS_SESSION_REAP_SECONDS
    + HARNESS_SESSION_VALIDATE_SECONDS
    + HARNESS_SESSION_SCHEDULING_GRACE_SECONDS
)
HARNESS_TERM_GRACE_SECONDS = 4.0
HARNESS_KILL_GRACE_SECONDS = 2.0
HARNESS_FINAL_VERIFY_SECONDS = 3 * HARNESS_SESSION_SCAN_SECONDS
HARNESS_OBSERVER_MARGIN_SECONDS = 10.0


def bounded_text(path: Path, maximum: int = 65_536) -> str:
    """Return a bounded UTF-8 diagnostic tail from PATH."""
    try:
        with path.open("rb") as stream:
            stream.seek(0, os.SEEK_END)
            size = stream.tell()
            stream.seek(max(0, size - maximum), os.SEEK_SET)
            data = stream.read(maximum)
    except OSError:
        return ""
    return data.decode("utf-8", errors="replace")


class BoundedPipeReader:
    """Read one subprocess pipe without threads or unbounded waits."""

    def __init__(self, stream: BinaryIO) -> None:
        self.stream = stream
        self.buffer = bytearray()
        self.eof = False
        self.selector = selectors.DefaultSelector()
        self.selector.register(stream, selectors.EVENT_READ)

    def fill(self, deadline: float) -> None:
        """Read one bounded chunk before DEADLINE."""
        remaining = deadline - time.monotonic()
        if remaining <= 0 or not self.selector.select(remaining):
            raise TimeoutError("bounded pipe read timed out")
        chunk = os.read(self.stream.fileno(), 64 * 1024)
        if not chunk:
            self.eof = True
            return
        if len(self.buffer) + len(chunk) > HARNESS_MAX_CAPTURE_BYTES:
            raise AssertionError("bridge output exceeded the harness capture limit")
        self.buffer.extend(chunk)

    def line(self, deadline: float) -> bytes:
        """Return one LF-terminated record before DEADLINE."""
        while True:
            newline = self.buffer.find(b"\n")
            if newline >= 0:
                value = bytes(self.buffer[: newline + 1])
                del self.buffer[: newline + 1]
                return value
            if self.eof:
                raise EOFError("bridge closed stdout before a complete reply")
            self.fill(deadline)

    def remainder(self, deadline: float) -> bytes:
        """Return remaining bytes after observing EOF before DEADLINE."""
        while not self.eof:
            self.fill(deadline)
        value = bytes(self.buffer)
        self.buffer.clear()
        return value

    def close(self) -> None:
        """Close only the selector; stream ownership remains with Popen."""
        self.selector.close()


def write_all(stream: BinaryIO, data: bytes, deadline: float) -> None:
    """Write every byte in DATA to STREAM before the absolute DEADLINE."""
    descriptor = stream.fileno()
    was_blocking = os.get_blocking(descriptor)
    selector = selectors.DefaultSelector()
    os.set_blocking(descriptor, False)
    selector.register(stream, selectors.EVENT_WRITE)
    remaining_data = memoryview(data)
    try:
        while remaining_data:
            remaining_time = deadline - time.monotonic()
            if remaining_time <= 0 or not selector.select(remaining_time):
                raise TimeoutError("bounded pipe write timed out")
            try:
                written = os.write(descriptor, remaining_data)
            except BlockingIOError:
                continue
            if written <= 0:
                raise BrokenPipeError("bounded pipe write made no progress")
            remaining_data = remaining_data[written:]
    finally:
        selector.close()
        if not stream.closed:
            os.set_blocking(descriptor, was_blocking)


def bounded_stream_text(stream: BinaryIO | None, timeout: float = 2.0) -> str:
    """Read STREAM to EOF within TIMEOUT and a fixed capture limit."""
    if stream is None:
        return ""
    selector = selectors.DefaultSelector()
    selector.register(stream, selectors.EVENT_READ)
    chunks = bytearray()
    deadline = time.monotonic() + timeout
    try:
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0 or not selector.select(remaining):
                raise AssertionError("subprocess diagnostic pipe did not close")
            chunk = os.read(stream.fileno(), 64 * 1024)
            if not chunk:
                return bytes(chunks).decode("utf-8", errors="replace")
            if len(chunks) + len(chunk) > HARNESS_MAX_CAPTURE_BYTES:
                raise AssertionError("subprocess diagnostic exceeded capture limit")
            chunks.extend(chunk)
    finally:
        selector.close()


class SessionDiscoveryDeadline(AssertionError):
    """A caller-owned process-discovery phase exhausted its deadline."""


def session_processes(
    session_id: int, *, deadline: float | None = None
) -> dict[int, int]:
    """Return live PID-to-PGID entries freshly validated in SESSION_ID.

    When DEADLINE is non-nil, process launch, collection, reaping, and row
    validation all remain inside that caller-owned absolute deadline.
    """
    natural_wait_deadline = time.monotonic() + HARNESS_SESSION_COLLECT_SECONDS
    if deadline is None:
        wait_deadline = natural_wait_deadline
    else:
        wait_deadline = min(
            natural_wait_deadline,
            deadline - HARNESS_SESSION_REAP_SECONDS,
        )
        if wait_deadline <= time.monotonic():
            raise SessionDiscoveryDeadline(
                "process-session discovery phase deadline expired"
            )
    deadline_limited = wait_deadline < natural_wait_deadline
    try:
        with tempfile.TemporaryFile() as capture:
            process = subprocess.Popen(
                ["ps", "-axo", "pid=,stat="],
                stdin=subprocess.DEVNULL,
                stdout=capture,
                stderr=subprocess.DEVNULL,
            )
            try:
                process.wait(timeout=max(0, wait_deadline - time.monotonic()))
            except subprocess.TimeoutExpired as error:
                process.kill()
                reap_deadline = time.monotonic() + 2
                if deadline is not None:
                    reap_deadline = min(reap_deadline, deadline)
                try:
                    process.wait(
                        timeout=max(0, reap_deadline - time.monotonic())
                    )
                except subprocess.TimeoutExpired as reap_error:
                    raise AssertionError(
                        "process-session discovery could not be reaped"
                    ) from reap_error
                if deadline_limited:
                    raise SessionDiscoveryDeadline(
                        "process-session discovery phase deadline expired"
                    ) from error
                raise AssertionError(
                    "process-session discovery did not finish"
                ) from error
            if process.returncode != 0:
                raise AssertionError(
                    f"process-session discovery exited {process.returncode}"
                )
            capture.seek(0, os.SEEK_END)
            size = capture.tell()
            if size > HARNESS_MAX_CAPTURE_BYTES:
                raise AssertionError(
                    "process-session discovery exceeded capture limit"
                )
            capture.seek(0)
            rows = capture.read(size).decode("ascii", errors="strict")
    except AssertionError:
        raise
    except (OSError, UnicodeError, subprocess.SubprocessError) as error:
        raise AssertionError("process-session discovery failed") from error

    processes: dict[int, int] = {}
    validation_deadline = time.monotonic() + HARNESS_SESSION_VALIDATE_SECONDS
    if deadline is not None:
        validation_deadline = min(validation_deadline, deadline)
    for line in rows.splitlines():
        if time.monotonic() >= validation_deadline:
            if deadline is not None and validation_deadline == deadline:
                raise SessionDiscoveryDeadline(
                    "process-session validation phase deadline expired"
                )
            raise AssertionError("process-session validation did not finish")
        pieces = line.split(None, 1)
        if not pieces or any(not piece.isascii() for piece in pieces):
            continue
        pid_text = pieces[0]
        # Darwin may print an empty STAT field for an otherwise live orphan.
        # A zombie is still reported with a leading Z and must not hold cleanup
        # open, but an empty field is not evidence that the process disappeared.
        state = pieces[1] if len(pieces) == 2 else ""
        if not pid_text.isdigit() or state.startswith("Z"):
            continue
        pid = int(pid_text)
        if pid <= 1:
            continue
        try:
            first_session = os.getsid(pid)
            if first_session != session_id:
                continue
            pgid = os.getpgid(pid)
            second_session = os.getsid(pid)
            if first_session == session_id == second_session and pgid > 1:
                processes[pid] = pgid
        except OSError:
            pass
    return processes


def session_process_groups(session_id: int) -> set[int]:
    """Return process groups freshly proven to belong to SESSION_ID."""
    return set(session_processes(session_id).values())


def process_exited_without_reaping(process: subprocess.Popen[bytes]) -> bool:
    """Observe PROCESS exit while retaining its PID/session ownership anchor."""
    if process.returncode is not None:
        return True
    try:
        result = os.waitid(
            os.P_PID,
            process.pid,
            os.WEXITED | os.WNOHANG | os.WNOWAIT,
        )
    except (AttributeError, ChildProcessError, OSError):
        return False
    return result is not None


def wait_process_exit_unreaped(
    process: subprocess.Popen[bytes], timeout: float
) -> bool:
    """Wait boundedly for PROCESS exit without releasing its PID/session."""
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if process_exited_without_reaping(process):
            return True
        time.sleep(0.02)
    return process_exited_without_reaping(process)


def signal_owned_process(pid: int, session_id: int, signum: int) -> bool:
    """Signal PID only while it is still proven to belong to SESSION_ID."""
    if pid <= 1:
        return False
    try:
        if os.getsid(pid) != session_id:
            return False
        os.kill(pid, signum)
    except (PermissionError, ProcessLookupError):
        return False
    return True


def defer_termination_signals() -> tuple[tuple[object, object], list[int]]:
    """Record TERM/INT until one bounded custody transaction completes."""
    previous = (signal.getsignal(signal.SIGTERM), signal.getsignal(signal.SIGINT))
    pending: list[int] = []

    def defer(signum: int, _frame: object) -> None:
        if not pending:
            pending.append(signum)

    signal.signal(signal.SIGTERM, defer)
    signal.signal(signal.SIGINT, defer)
    return previous, pending


def restore_termination_signals(
    state: tuple[tuple[object, object], list[int]],
) -> None:
    """Restore saved handlers, then deliver any deferred termination."""
    handlers, pending = state
    signal.signal(signal.SIGTERM, handlers[0])
    signal.signal(signal.SIGINT, handlers[1])
    for signum in pending:
        handler = handlers[0] if signum == signal.SIGTERM else handlers[1]
        if handler == signal.SIG_IGN:
            continue
        if handler == signal.SIG_DFL:
            raise SystemExit(128 + signum)
        if not callable(handler):
            raise AssertionError("invalid termination handler")
        handler(signum, None)


def recorded_child_group(path: Path, session_id: int) -> int | None:
    """Return PATH's live, self-led child group when owned by SESSION_ID."""
    try:
        with path.open("rb") as stream:
            encoded = stream.read(HARNESS_CHILD_RECORD_MAX_BYTES + 1)
        if len(encoded) > HARNESS_CHILD_RECORD_MAX_BYTES:
            return None
        raw = encoded.decode("ascii", errors="strict")
    except (OSError, UnicodeError):
        return None
    pieces = raw.split()
    if len(pieces) != 2 or any(not piece.isascii() for piece in pieces):
        return None
    if any(not piece.isdigit() for piece in pieces):
        return None
    try:
        pid, pgid = (int(piece) for piece in pieces)
    except ValueError:
        return None
    if pid <= 1 or pid != pgid:
        return None
    try:
        owned = os.getpgid(pid) == pgid and os.getsid(pid) == session_id
        return pgid if owned else None
    except (OSError, OverflowError):
        return None


def signal_group(pgid: int | None, signum: int) -> None:
    """Best-effort signal PGID when it is a validated positive integer."""
    if os.name != "posix" or pgid is None or pgid <= 1:
        return
    try:
        os.killpg(pgid, signum)
    except (PermissionError, ProcessLookupError):
        pass


def close_process_pipes(process: subprocess.Popen[bytes]) -> None:
    """Close PROCESS pipes without reading them."""
    for stream in (process.stdin, process.stdout, process.stderr):
        if stream is not None and not stream.closed:
            try:
                stream.close()
            except OSError:
                pass


def popen_with_termination_deferred(
    *args: object, **kwargs: object
) -> tuple[
    subprocess.Popen[bytes], tuple[tuple[object, object], list[int]]
]:
    """Spawn a process while deferring TERM/INT until cleanup is registered."""
    previous = defer_termination_signals()
    try:
        process = subprocess.Popen(*args, **kwargs)
    except BaseException:
        restore_termination_signals(previous)
        raise
    return process, previous


def bounded_runner_budget(timeout: int, kill_after: int) -> float:
    """Return the conservative wall envelope of one production runner."""
    return float(2 * timeout + 3 * kill_after + 3)


def reply_observer_budget(
    *,
    frame_timeout: int,
    parse_timeout: int,
    readiness_timeout: int,
    dispatch_timeout: int,
    kill_after: int,
    large_request: bool,
) -> float:
    """Outlive every bounded production phase that may precede first EOF."""
    parse_phases = 3 + int(large_request)
    first_reply = (
        bounded_runner_budget(frame_timeout, kill_after)
        + parse_phases * bounded_runner_budget(parse_timeout, kill_after)
        + bounded_runner_budget(readiness_timeout, kill_after)
        + 2
        + bounded_runner_budget(dispatch_timeout, kill_after)
    )
    # An error may exit without a reply, in which case EOF follows as many as
    # two bounded cleanup-staged attempts rather than preceding them.
    return (
        first_reply
        + 2 * bounded_runner_budget(parse_timeout, kill_after)
        + HARNESS_OBSERVER_MARGIN_SECONDS
    )


def shutdown_observer_budget(parse_timeout: int, kill_after: int) -> float:
    """Outlive both production cleanup attempts after stdin reaches EOF."""
    return (
        2 * bounded_runner_budget(parse_timeout, kill_after)
        + HARNESS_OBSERVER_MARGIN_SECONDS
    )


def terminate_bridge(
    process: subprocess.Popen[bytes],
    active_child: Path | None,
    *,
    term_grace: float = HARNESS_TERM_GRACE_SECONDS,
) -> bool:
    """Boundedly retire PROCESS and report a freshly observed orphan group."""
    previous = defer_termination_signals()
    try:
        return _terminate_bridge(
            process,
            active_child,
            term_grace=term_grace,
        )
    finally:
        restore_termination_signals(previous)


def _terminate_bridge(
    process: subprocess.Popen[bytes],
    active_child: Path | None,
    *,
    term_grace: float,
) -> bool:
    """Implement `terminate_bridge` while TERM/INT cannot interrupt custody."""
    if process.stdin is not None and not process.stdin.closed:
        try:
            process.stdin.close()
        except (BrokenPipeError, OSError):
            pass
        process.stdin = None

    if process.returncode is not None:
        close_process_pipes(process)
        return False

    session_id = process.pid
    orphan_group_observed = False
    phase_discovery_errors: list[str] = []
    discovery_errors: list[str] = []

    def discover_groups(
        deadline: float,
    ) -> tuple[set[int], set[int]] | None:
        nonlocal orphan_group_observed
        try:
            processes = session_processes(session_id, deadline=deadline)
        except SessionDiscoveryDeadline:
            return None
        except AssertionError as error:
            message = str(error)
            if message not in discovery_errors:
                discovery_errors.append(message)
            return None
        groups = set(processes.values())
        if any(
            group != session_id and group not in processes for group in groups
        ):
            orphan_group_observed = True
        if active_child is not None:
            child_group = recorded_child_group(active_child, session_id)
            if child_group is not None:
                groups.add(child_group)
        return groups, set(processes)

    # TERM only the bridge leader.  Its production handler owns orderly runner
    # convergence and staged cleanup; signalling newly created cleanup groups
    # here would sabotage the very grace period this harness promises.
    if process.returncode is None:
        signal_owned_process(process.pid, session_id, signal.SIGTERM)

    deadline = time.monotonic() + term_grace
    quiet_exit_scans = 0
    while time.monotonic() < deadline:
        discovered = discover_groups(deadline)
        if discovered is None:
            break
        groups, pids = discovered
        has_children = bool((pids - {process.pid}) or (groups - {session_id}))
        if process_exited_without_reaping(process) and not has_children:
            quiet_exit_scans += 1
            if quiet_exit_scans >= 2:
                break
        else:
            quiet_exit_scans = 0
        time.sleep(0.1)

    # Keep the session leader unreaped until every owned group has received
    # SIGKILL.  That prevents its numeric session/group identity from being
    # reused while a late atomic child publication is still possible.
    signal_group(session_id, signal.SIGKILL)
    deadline = time.monotonic() + HARNESS_KILL_GRACE_SECONDS
    quiet_exit_scans = 0
    while time.monotonic() < deadline:
        discovered = discover_groups(deadline)
        if discovered is None:
            break
        groups, pids = discovered
        for group in groups:
            signal_group(group, signal.SIGKILL)
        has_children = bool((pids - {process.pid}) or (groups - {session_id}))
        if process_exited_without_reaping(process) and not has_children:
            quiet_exit_scans += 1
            if quiet_exit_scans >= 2:
                break
        else:
            quiet_exit_scans = 0
        time.sleep(0.05)

    # One last session-owned escalation occurs while the leader is unreaped.
    signal_group(session_id, signal.SIGKILL)

    retained_processes: dict[int, int] = {}
    verification_deadline = time.monotonic() + HARNESS_KILL_GRACE_SECONDS
    while time.monotonic() < verification_deadline:
        try:
            retained_processes = session_processes(
                session_id, deadline=verification_deadline
            )
        except SessionDiscoveryDeadline as error:
            message = str(error)
            if message not in phase_discovery_errors:
                phase_discovery_errors.append(message)
            break
        except AssertionError as error:
            message = str(error)
            if message not in discovery_errors:
                discovery_errors.append(message)
            break
        retained_processes.pop(process.pid, None)
        if not retained_processes:
            break
        for group in set(retained_processes.values()):
            signal_group(group, signal.SIGKILL)
        time.sleep(0.05)

    leader_exited = process_exited_without_reaping(process)
    if not leader_exited:
        signal_owned_process(process.pid, session_id, signal.SIGKILL)
        leader_exited = wait_process_exit_unreaped(
            process, HARNESS_KILL_GRACE_SECONDS
        )

    # The leader stays unreaped through a final child scan.  This closes the
    # last-fork race between the earlier empty scan and leader termination.
    retained_processes = {}
    verification_deadline = time.monotonic() + HARNESS_FINAL_VERIFY_SECONDS
    quiet_exit_scans = 0
    while time.monotonic() < verification_deadline:
        try:
            retained_processes = session_processes(
                session_id, deadline=verification_deadline
            )
        except SessionDiscoveryDeadline as error:
            message = str(error)
            if message not in discovery_errors:
                discovery_errors.append(message)
            break
        except AssertionError as error:
            message = str(error)
            if message not in discovery_errors:
                discovery_errors.append(message)
            break
        retained_processes.pop(process.pid, None)
        for group in set(retained_processes.values()):
            signal_group(group, signal.SIGKILL)
        leader_exited = process_exited_without_reaping(process)
        if leader_exited and not retained_processes:
            quiet_exit_scans += 1
            if quiet_exit_scans >= 2:
                break
        else:
            quiet_exit_scans = 0
        time.sleep(0.05)

    if quiet_exit_scans < 2:
        discovery_errors.append(
            "harness cleanup did not prove a quiet final session"
        )
    if leader_exited and not retained_processes:
        process.wait(timeout=max(0, verification_deadline - time.monotonic()))
    elif not leader_exited:
        discovery_errors.append("bridge leader could not be reaped")
    close_process_pipes(process)
    if retained_processes:
        raise AssertionError(
            "harness cleanup retained session children "
            f"{sorted(retained_processes)!r}"
        )
    if discovery_errors:
        for message in phase_discovery_errors:
            if message not in discovery_errors:
                discovery_errors.append(message)
        raise AssertionError(
            "harness cleanup session discovery failed: "
            + "; ".join(discovery_errors)
        )
    return orphan_group_observed


def cleanup_fixture_process(
    process: subprocess.Popen[bytes],
    *,
    term_grace: float,
    active_child: Path | None = None,
    reader: BoundedPipeReader | None = None,
    resume: bool = False,
) -> None:
    """Complete one fixture cleanup without asynchronous TERM/INT reentry."""
    previous = defer_termination_signals()
    try:
        if reader is not None:
            reader.close()
        if resume and process.returncode is None:
            signal_owned_process(process.pid, process.pid, signal.SIGCONT)
        if process.returncode is None:
            _terminate_bridge(
                process,
                active_child,
                term_grace=term_grace,
            )
        else:
            close_process_pipes(process)
    finally:
        restore_termination_signals(previous)


def run_bridge_while_open(
    command: list[str],
    request: str,
    environment: dict[str, str],
    root: Path,
    *,
    expect_exit_after_reply: bool = False,
    reply_timeout: float = 60.0,
    shutdown_timeout: float = 60.0,
) -> tuple[str, str]:
    stderr_path = root / "bridge.stderr"
    active_child = Path(
        environment.get("FAKE_ACTIVE_CHILD", str(root / "active-child"))
    )
    try:
        active_child.unlink()
    except FileNotFoundError:
        pass
    options: dict[str, object] = {}
    if os.name == "posix":
        options["start_new_session"] = True
    stderr_handle = stderr_path.open("wb")
    try:
        process, custody_defer = popen_with_termination_deferred(
            command,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=stderr_handle,
            env=environment,
            bufsize=0,
            **options,
        )
    except BaseException:
        stderr_handle.close()
        raise
    reader: BoundedPipeReader | None = None
    try:
        stderr_handle.close()
        if process.stdin is None or process.stdout is None:
            raise AssertionError("bridge pipes were not created")
        reader = BoundedPipeReader(process.stdout)
        previous = custody_defer
        custody_defer = None
        restore_termination_signals(previous)
        reply_deadline = time.monotonic() + reply_timeout
        try:
            write_all(process.stdin, request.encode("utf-8"), reply_deadline)
        except (BrokenPipeError, OSError, TimeoutError) as error:
            orphan_group_observed = terminate_bridge(
                process, active_child, term_grace=shutdown_timeout
            )
            raise AssertionError(
                "bridge did not accept a bounded request: "
                f"rc={process.poll()} "
                f"orphanGroupObserved={orphan_group_observed} "
                f"stderr={bounded_text(stderr_path)!r} "
                f"debug={bounded_text(root / 'debug.log')!r}"
            ) from error

        try:
            first_bytes = reader.line(reply_deadline)
        except EOFError as error:
            terminate_bridge(process, active_child, term_grace=shutdown_timeout)
            raise AssertionError(
                "bridge exited before replying with "
                f"rc={process.poll()} stderr={bounded_text(stderr_path)!r} "
                f"debug={bounded_text(root / 'debug.log')!r}"
            ) from error
        except TimeoutError as error:
            orphan_group_observed = terminate_bridge(
                process, active_child, term_grace=shutdown_timeout
            )
            raise AssertionError(
                "bridge did not return a bounded reply: "
                f"rc={process.poll()} "
                f"orphanGroupObserved={orphan_group_observed} "
                f"stderr={bounded_text(stderr_path)!r} "
                f"debug={bounded_text(root / 'debug.log')!r}"
            ) from error
        try:
            first_line = first_bytes.decode("utf-8", errors="strict")
        except UnicodeDecodeError as error:
            raise AssertionError("bridge reply was not valid UTF-8") from error
        if not expect_exit_after_reply and process_exited_without_reaping(process):
            terminate_bridge(process, active_child, term_grace=shutdown_timeout)
            raise AssertionError(
                "bridge did not remain available after one request: "
                f"rc={process.returncode} stderr={bounded_text(stderr_path)!r} "
                f"debug={bounded_text(root / 'debug.log')!r}"
            )
        if expect_exit_after_reply:
            if not wait_process_exit_unreaped(process, shutdown_timeout):
                terminate_bridge(
                    process, active_child, term_grace=shutdown_timeout
                )
                raise AssertionError(
                    "ambiguous dispatch did not close the bridge"
                )
        transaction_paths = list(root.glob("anvil-mcp.*"))
        staged = {
            path.name: (
                sorted(child.name for child in path.iterdir())
                if path.is_dir()
                else ["<not-a-directory>"]
            )
            for path in transaction_paths
        }
        transaction_ok = len(transaction_paths) <= 1
        if len(transaction_paths) == 1:
            directory = transaction_paths[0]
            children = list(directory.iterdir()) if directory.is_dir() else []

            def generation(child: Path, prefix: str) -> str | None:
                match = re.fullmatch(
                    rf"{re.escape(prefix)}\.([0-9]+)\.json",
                    child.name,
                )
                return match.group(1) if match is not None else None

            def stage_generation(child: Path) -> str | None:
                match = re.fullmatch(
                    r"\.response-tmp\.([0-9]+)\..+",
                    child.name,
                )
                return match.group(1) if match is not None else None

            responses = [
                child for child in children if generation(child, "response") is not None
            ]
            proofs = [
                child for child in children if generation(child, "proof") is not None
            ]
            requests = [
                child for child in children if generation(child, "request") is not None
            ]
            stages = [
                child for child in children if stage_generation(child) is not None
            ]
            directory_info = directory.stat()
            transaction_ok = (
                directory.is_dir()
                and directory_info.st_uid == os.geteuid()
                and stat.S_IMODE(directory_info.st_mode) == 0o700
                and len(requests) <= 1
            )
            unpublished = (
                len(stages) == 1
                and not responses
                and not proofs
                and len(children) == len(stages) + len(requests)
            )
            published = (
                not stages
                and len(responses) == 1
                and len(proofs) == 1
                and len(children) == len(responses) + len(proofs) + len(requests)
            )
            state_generation = (
                stage_generation(stages[0])
                if unpublished
                else generation(responses[0], "response")
                if published
                else None
            )
            if published:
                response_info = responses[0].lstat()
                proof_info = proofs[0].lstat()
                published = (
                    generation(proofs[0], "proof") == state_generation
                    and response_info.st_dev == proof_info.st_dev
                    and response_info.st_ino == proof_info.st_ino
                    and response_info.st_nlink == 2
                    and proof_info.st_nlink == 2
                )
            transaction_ok = transaction_ok and (unpublished or published)
            if requests:
                transaction_ok = (
                    transaction_ok
                    and generation(requests[0], "request") == state_generation
                )
            for child in children:
                info = child.lstat()
                child_ok = (
                    child.is_file()
                    and not child.is_symlink()
                    and info.st_uid == os.geteuid()
                    and stat.S_IMODE(info.st_mode) == 0o600
                )
                if generation(child, "request") is not None:
                    child_ok = (
                        child_ok and info.st_nlink == 1 and info.st_size <= 16_777_216
                    )
                elif stage_generation(child) is not None:
                    child_ok = child_ok and info.st_nlink == 1 and info.st_size == 0
                else:
                    child_ok = (
                        child_ok and info.st_nlink == 2 and info.st_size <= 33_554_432
                    )
                transaction_ok = transaction_ok and child_ok
        if transaction_paths and not transaction_ok:
            debug = bounded_text(root / "debug.log") or "<no debug log>"
            expression_file = root / "expression"
            expression = bounded_text(expression_file, 4096) or "<no expression>"
            guard_file = root / "guard-observed"
            early_file = root / "early-dispatch"
            guard = bounded_text(guard_file, 4096)
            early = bounded_text(early_file, 4096)
            raise AssertionError(
                "bridge retained unsafe generation custody after replying: "
                f"{staged!r}; response={first_line!r}; "
                f"guard={guard!r}; early={early!r}; "
                f"expression-head={expression[:240]!r}; "
                f"expression-tail={expression[-240:]!r}; debug={debug!r}"
            )

        try:
            process.stdin.close()
        except (BrokenPipeError, OSError):
            pass
        process.stdin = None
        if not wait_process_exit_unreaped(process, shutdown_timeout):
            terminate_bridge(process, active_child, term_grace=shutdown_timeout)
            raise AssertionError("bridge did not exit after stdin EOF")
        try:
            retained_processes = session_processes(process.pid)
        except AssertionError as error:
            terminate_bridge(process, active_child, term_grace=0)
            raise AssertionError(
                "bridge session could not be validated before reaping"
            ) from error
        retained_processes.pop(process.pid, None)
        if retained_processes:
            retained = sorted(retained_processes)
            terminate_bridge(process, active_child, term_grace=0)
            raise AssertionError(
                f"bridge exit retained session children {retained!r}"
            )
        process.wait(timeout=HARNESS_KILL_GRACE_SECONDS)
        try:
            remainder_bytes = reader.remainder(time.monotonic() + 2)
            remainder = remainder_bytes.decode("utf-8", errors="strict")
        except (TimeoutError, UnicodeDecodeError) as error:
            terminate_bridge(process, active_child, term_grace=shutdown_timeout)
            raise AssertionError("bridge output did not close cleanly") from error
        stderr = bounded_text(stderr_path)
        stdout = first_line + remainder
        expected_returncode = 74 if expect_exit_after_reply else 0
        if process.returncode != expected_returncode:
            raise AssertionError(
                f"bridge exited {process.returncode}, expected "
                f"{expected_returncode}: stdout={stdout!r} stderr={stderr!r} "
                f"debug={bounded_text(root / 'debug.log')!r}"
            )
        remaining_transactions = list(root.glob("anvil-mcp.*"))
        if remaining_transactions:
            raise AssertionError(
                "bridge exit retained response transaction custody: "
                f"{[path.name for path in remaining_transactions]!r}"
            )
        return stdout, stderr
    finally:
        if not stderr_handle.closed:
            stderr_handle.close()
        cleanup_fixture_process(
            process,
            term_grace=shutdown_timeout,
            active_child=active_child,
            reader=reader,
        )
        if custody_defer is not None:
            restore_termination_signals(custody_defer)


def run_case(
    stdio: Path,
    bash: str,
    *,
    nil_before: int = 0,
    always_nil: bool = False,
    atomic_not_ready: bool = False,
    dispatch_error: bool = False,
    hang_prepare_response: bool = False,
    malformed_output: bool = False,
    invalid_output: bool = False,
    crlf: bool = False,
    readiness_timeout: int = 5,
    probe_timeout: int = 5,
    retry_delay_ms: int = 50,
    retry_max: int = 5,
    dispatch_timeout: int = 15,
    request_parse_timeout: int = 10,
    frame_read_timeout: int = 10,
    real_emacs_delay_sec: float = 0,
    large_request: bool = False,
    exit_descendant: bool = False,
    anvil_root: Path | None = None,
) -> tuple[dict[str, object], int, int, float, str, str]:
    with tempfile.TemporaryDirectory(prefix="anvil-stdio-readiness-") as raw:
        root = Path(raw)
        binary = root / "bin"
        binary.mkdir()
        fake = binary / "emacsclient"
        python_fake: Path | None = None
        if always_nil:
            write_fast_nil_emacsclient(fake, bash)
        else:
            python_fake = binary / "emacsclient-python"
            write_fake_emacsclient(python_fake)
            write_fast_probe_frontend(fake, bash)
        if hang_prepare_response:
            real_python = sys.executable
            make_executable(
                binary / "python3",
                f"""#!{real_python}
import os
import sys
import time

code = "\\n".join(sys.argv[1:])
if (
    os.environ.get("FAKE_HANG_PREPARE_RESPONSE") == "1"
    and 'pieces[0] == "response"' in code
):
    time.sleep(5)
os.execv({real_python!r}, [{real_python!r}, *sys.argv[1:]])
""",
            )
        probe_count = root / "probe-count"
        fast_probe_count = root / "fast-probe-count"
        dispatch_count = root / "dispatch-count"
        early_dispatch = root / "early-dispatch"
        guard_observed = root / "guard-observed"
        expected = {"jsonrpc": "2.0", "id": 17, "result": {"ready": True}}
        request: dict[str, object] = {
            "jsonrpc": "2.0",
            "id": 17,
            "method": "tools/call",
        }
        if large_request:
            request["params"] = {"payload": "x" * 20000}
        environment = os.environ.copy()
        kill_after_timeout = 1
        environment.pop("ANVIL_MCP_PARENT_GUARD", None)
        environment.pop("ANVIL_MCP_PARENT_GUARD_PYTHON", None)
        environment.update(
            {
                "PATH": f"{binary}{os.pathsep}{environment['PATH']}",
                "TMPDIR": str(root),
                "EMACS_MCP_DEBUG_LOG": str(root / "debug.log"),
                "FAKE_PROBE_COUNT": str(probe_count),
                "FAKE_FAST_PROBE_COUNT": str(fast_probe_count),
                "FAKE_PYTHON_EMACSCLIENT": (
                    str(python_fake) if python_fake is not None else ""
                ),
                "FAKE_DISPATCH_COUNT": str(dispatch_count),
                "FAKE_EARLY_DISPATCH": str(early_dispatch),
                "FAKE_GUARD_OBSERVED": str(guard_observed),
                "FAKE_EXPRESSION": str(root / "expression"),
                "FAKE_INIT_COUNT": str(root / "init-count"),
                "FAKE_STOP_COUNT": str(root / "stop-count"),
                "FAKE_RESPONSE_WIRE": percent_wire(expected),
                "FAKE_RESPONSE_JSON": json.dumps(
                    expected, separators=(",", ":"), ensure_ascii=False
                ),
                "FAKE_REAL_EMACS": os.environ["ANVIL_TEST_REAL_EMACS"],
                "FAKE_REAL_EMACS_DELAY_SEC": str(real_emacs_delay_sec),
                "FAKE_ANVIL_ROOT": str(anvil_root or stdio.parent),
                "FAKE_NIL_BEFORE": str(nil_before),
                "FAKE_ALWAYS_NIL": "1" if always_nil else "0",
                "FAKE_ATOMIC_NOT_READY": "1" if atomic_not_ready else "0",
                "FAKE_DISPATCH_ERROR": "1" if dispatch_error else "0",
                "FAKE_MALFORMED_OUTPUT": "1" if malformed_output else "0",
                "FAKE_HANG_DISPATCH": "0",
                "FAKE_HANG_PREPARE_RESPONSE": (
                    "1" if hang_prepare_response else "0"
                ),
                "FAKE_IGNORE_TERM": "0",
                "FAKE_HANG_PID": str(root / "hang-pid"),
                "FAKE_ACTIVE_CHILD": str(root / "active-child"),
                "FAKE_DESCENDANT_PID": str(root / "descendant-pid"),
                "FAKE_EXIT_DESCENDANT": "1" if exit_descendant else "0",
                "FAKE_INVALID_OUTPUT": "1" if invalid_output else "0",
                "FAKE_CRLF": "1" if crlf else "0",
                "ANVIL_MCP_READINESS_MODE": "headless",
                "ANVIL_EMACSCLIENT_PROBE_TIMEOUT": str(probe_timeout),
                "ANVIL_EMACSCLIENT_READINESS_TIMEOUT": str(readiness_timeout),
                "ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT": str(dispatch_timeout),
                "ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT": str(kill_after_timeout),
                "ANVIL_EMACSCLIENT_RETRY_MAX": str(retry_max),
                "ANVIL_EMACSCLIENT_RETRY_DELAY_MS": str(retry_delay_ms),
                "ANVIL_MCP_REQUEST_PARSE_TIMEOUT": str(request_parse_timeout),
                "ANVIL_MCP_FRAME_READ_TIMEOUT": str(frame_read_timeout),
            }
        )
        started = time.monotonic()
        stdout, stderr = run_bridge_while_open(
            [
                bash,
                str(stdio),
                "--socket=/tmp/anvil-readiness-test",
                "--server-id=anvil",
            ],
            json.dumps(request, separators=(",", ":")) + "\n",
            environment,
            root,
            expect_exit_after_reply=hang_prepare_response,
            reply_timeout=reply_observer_budget(
                frame_timeout=frame_read_timeout,
                parse_timeout=request_parse_timeout,
                readiness_timeout=readiness_timeout,
                dispatch_timeout=dispatch_timeout,
                kill_after=kill_after_timeout,
                large_request=large_request,
            ),
            shutdown_timeout=shutdown_observer_budget(
                request_parse_timeout, kill_after_timeout
            ),
        )
        if exit_descendant:
            descendant_path = root / "descendant-pid"
            if not descendant_path.exists():
                raise AssertionError(
                    "successful dispatch did not spawn its stubborn descendant"
                )
            descendant_pid = int(descendant_path.read_text(encoding="utf-8"))
            if not wait_process_dead(descendant_pid):
                raise AssertionError(
                    "successful direct child left a same-group descendant alive"
                )
        elapsed = time.monotonic() - started
        lines = [line for line in stdout.splitlines() if line]
        if len(lines) != 1:
            raise AssertionError(f"bridge returned {len(lines)} replies: {stdout!r}")
        response = json.loads(lines[0])
        if not isinstance(response, dict):
            raise AssertionError(f"bridge returned non-object: {response!r}")
        if early_dispatch.exists():
            raise AssertionError(early_dispatch.read_text(encoding="utf-8"))
        guard = (
            guard_observed.read_text(encoding="utf-8")
            if guard_observed.exists()
            else ""
        )
        debug = bounded_text(root / "debug.log")
        diagnostics = stderr + ("\n" + debug if debug else "")
        probes = read_count(probe_count)
        if not always_nil and probes != read_count(fast_probe_count):
            raise AssertionError(
                "shell/Python probe-count parity failed: "
                f"probes={probes} fast={read_count(fast_probe_count)}"
            )
        return (
            response,
            probes,
            read_count(dispatch_count),
            elapsed,
            diagnostics,
            guard,
        )


def assert_harness_timeout_reaps_separate_child(bash: str) -> None:
    """A reply timeout must not strand a separate child-group writer."""
    minimum_final_verify = 3 * HARNESS_SESSION_SCAN_SECONDS
    if HARNESS_FINAL_VERIFY_SECONDS < minimum_final_verify:
        raise AssertionError(
            "final verification must fund one cleanup scan and two quiet scans"
        )
    with tempfile.TemporaryDirectory(prefix="anvil-stdio-harness-timeout-") as raw:
        root = Path(raw)
        active_child = root / "active-child"
        descendant_child = root / "descendant-child"
        leader_reaped = root / "leader-reaped"
        bridge = root / "bridge.sh"
        slow_bin = root / "slow-bin"
        slow_bin.mkdir()
        real_ps = shutil.which("ps")
        if real_ps is None:
            raise AssertionError("process-table fixture requires ps")
        make_executable(
            slow_bin / "ps",
            (
                f"#!{sys.executable}\n"
                "import os\n"
                "import sys\n"
                "import time\n"
                "time.sleep(1.6)\n"
                f"os.execv({real_ps!r}, [{real_ps!r}, *sys.argv[1:]])\n"
            ),
        )
        make_executable(
            bridge,
            f"""#!{bash}
set -eu
set -m
{json.dumps(sys.executable)} -I -S -c '
import os
from pathlib import Path
import signal
import sys
import time

descendant = os.fork()
if descendant == 0:
    signal.signal(signal.SIGTERM, signal.SIG_IGN)
    signal.signal(signal.SIGHUP, signal.SIG_IGN)
    signal.signal(signal.SIGINT, signal.SIG_IGN)
    while True:
        time.sleep(1)
Path(os.environ["FAKE_DESCENDANT_CHILD"]).write_text(str(descendant))
signal.signal(signal.SIGTERM, lambda *_arguments: sys.exit(0))
while True:
    time.sleep(1)
' &
child=$!
printf '%s %s\n' "$child" "$child" > "$FAKE_ACTIVE_CHILD"
trap 'kill -TERM "$child" 2>/dev/null || :; wait "$child" 2>/dev/null || :; : > "$FAKE_LEADER_REAPED"; exit 143' TERM
while :; do sleep 1; done
""",
        )
        environment = os.environ.copy()
        environment["FAKE_ACTIVE_CHILD"] = str(active_child)
        environment["FAKE_DESCENDANT_CHILD"] = str(descendant_child)
        environment["FAKE_LEADER_REAPED"] = str(leader_reaped)
        started = time.monotonic()
        failure = ""
        original_path = os.environ.get("PATH")
        try:
            os.environ["PATH"] = str(slow_bin) + os.pathsep + (original_path or "")
            run_bridge_while_open(
                [bash, str(bridge)],
                "request\n",
                environment,
                root,
                reply_timeout=2,
                shutdown_timeout=HARNESS_TERM_GRACE_SECONDS,
            )
        except AssertionError as error:
            failure = str(error)
        else:
            raise AssertionError("silent fixture unexpectedly returned a reply")
        finally:
            if original_path is None:
                os.environ.pop("PATH", None)
            else:
                os.environ["PATH"] = original_path
        elapsed = time.monotonic() - started
        if "bridge did not return a bounded reply" not in failure:
            raise AssertionError(f"wrong timeout diagnostic: {failure!r}")
        if "orphanGroupObserved=True" not in failure:
            raise AssertionError(f"orphan group was not observed: {failure!r}")
        if not leader_reaped.exists():
            raise AssertionError("timeout fixture did not reap its child-group leader")
        if not active_child.exists():
            raise AssertionError("timeout fixture never published its child group")
        child_pid = int(active_child.read_text(encoding="ascii").split()[0])
        if not descendant_child.exists():
            raise AssertionError("timeout fixture never published its descendant")
        descendant_pid = int(descendant_child.read_text(encoding="ascii"))
        if not wait_process_dead(child_pid) or not wait_process_dead(descendant_pid):
            raise AssertionError(
                f"timeout retained child group {child_pid}/{descendant_pid}"
            )
        maximum_elapsed = (
            2
            + HARNESS_TERM_GRACE_SECONDS
            + 3 * HARNESS_KILL_GRACE_SECONDS
            + HARNESS_FINAL_VERIFY_SECONDS
            + 1
        )
        if elapsed >= maximum_elapsed:
            raise AssertionError(
                f"harness timeout cleanup exceeded its bound: {elapsed:.3f}s"
            )


def assert_reaped_pid_alias_does_not_extend_wait(
    stdio: Path,
    bash: str,
    expected: dict[str, object],
) -> None:
    """Prove immediate PID reuse hangs the legacy loop but not the fixed one."""
    with tempfile.TemporaryDirectory(prefix="anvil-stdio-reaped-pid-") as raw:
        root = Path(raw)
        alias_marker = root / "alias-observed"
        for source in stdio.parent.glob("*.el"):
            (root / source.name).symlink_to(source)
        text = stdio.read_text(encoding="utf-8")
        function_anchor = "anvil_mcp_job_is_running() {\n"
        child_anchor = "\tchild=$!\n\t# Signals may arrive after ACK but before the exact child PID is assigned.\n"
        fixed_wait = """\twhile :; do
\t\tif wait "$child"; then
\t\t\trc=0
\t\t\tbreak
\t\telse
\t\t\trc=$?
\t\tfi
\t\t# wait can return early when a runner signal trap fires.  Repeat
\t\t# only while Bash still owns the exact job; kill -0 is insufficient
\t\t# because the reaped PID may already identify an unrelated process.
\t\tanvil_mcp_job_is_running "$child" || break
\tdone
"""
        legacy_wait = """\twhile :; do
\t\tif wait "$child"; then rc=0; else rc=$?; fi
\t\tkill -0 "$child" 2>/dev/null || break
\tdone
"""
        if (
            text.count(function_anchor) != 1
            or text.count(child_anchor) != 1
            or text.count(fixed_wait) != 1
        ):
            raise AssertionError("reaped-PID regression injection point drifted")
        kill_override = """kill() {
\tif [ "${1-}" = "-0" ] \\
\t\t&& [ "${2-}" = "${ANVIL_TEST_REAPED_CHILD_PID:-}" ]; then
\t\tprintf alias > %s
\t\treturn 0
\tfi
\tbuiltin kill "$@"
}

""" % json.dumps(str(alias_marker))
        text = text.replace(function_anchor, kill_override + function_anchor, 1)
        text = text.replace(
            child_anchor,
            "\tchild=$!\n\tANVIL_TEST_REAPED_CHILD_PID=$child\n"
            "\t# Signals may arrive after ACK but before the exact child PID is assigned.\n",
            1,
        )
        fixed = root / "anvil-stdio-fixed.sh"
        legacy = root / "anvil-stdio-legacy.sh"
        fixed.write_text(text, encoding="utf-8")
        legacy.write_text(text.replace(fixed_wait, legacy_wait, 1), encoding="utf-8")

        legacy_failure = ""
        try:
            legacy_response, *_ = run_case(
                legacy,
                bash,
                readiness_timeout=1,
                probe_timeout=1,
                dispatch_timeout=2,
                frame_read_timeout=1,
            )
        except AssertionError as error:
            legacy_failure = str(error)
            if "bridge exited before replying with rc=65" not in legacy_failure:
                raise
        else:
            if strict_equal(legacy_response, expected):
                raise AssertionError(
                    "legacy wait loop unexpectedly completed through the PID alias"
                )
        if not alias_marker.exists():
            raise AssertionError(
                "legacy wait loop never exercised the injected PID alias: "
                f"failure={legacy_failure!r}"
            )
        alias_marker.unlink()

        clean_failures: list[str] = []
        for attempt in range(1, 4):
            response, probes, dispatches, elapsed, diagnostics, _guard = run_case(
                fixed,
                bash,
                readiness_timeout=15,
                probe_timeout=5,
                dispatch_timeout=15,
                request_parse_timeout=10,
            )
            if (
                not alias_marker.exists()
                and strict_equal(response, expected)
                and probes == 1
                and dispatches == 1
            ):
                break
            error = response.get("error")
            data = error.get("data") if isinstance(error, dict) else None
            clean_timeout = (
                not alias_marker.exists()
                and dispatches == 0
                and strict_equal(
                    data,
                    {
                        "phase": "readiness",
                        "dispatched": False,
                        "replayed": False,
                        "emacsclientRc": 124,
                    },
                )
                and "MCP-RUNNER-TIMEOUT: phase=execution "
                "operation=emacsclient" in diagnostics
            )
            details = (
                f"attempt={attempt} marker={alias_marker.exists()} "
                f"response={response!r} probes={probes} "
                f"dispatches={dispatches} elapsed={elapsed:.3f} "
                f"diagnostics={diagnostics!r}"
            )
            if clean_timeout:
                clean_failures.append(details)
                continue
            raise AssertionError(
                "fixed wait loop retained the reaped-PID alias: " + details
            )
        else:
            raise AssertionError(
                "fixed wait loop starved before dispatch after three clean "
                "attempts: " + " | ".join(clean_failures)
            )


def assert_readiness_error(response: dict[str, object], rc: int = 75) -> None:
    error = response.get("error")
    data = error.get("data") if isinstance(error, dict) else None
    expected = {
        "phase": "readiness",
        "dispatched": False,
        "replayed": False,
        "emacsclientRc": rc,
    }
    if response.get("id") != 17 or not strict_equal(data, expected):
        raise AssertionError(f"wrong readiness failure: {response!r}")


def assert_dispatch_error(response: dict[str, object], rc: int = 70) -> None:
    error = response.get("error")
    data = error.get("data") if isinstance(error, dict) else None
    expected = {
        "phase": "dispatch",
        "dispatched": True,
        "replayed": False,
        "emacsclientRc": rc,
    }
    if response.get("id") != 17 or not strict_equal(data, expected):
        raise AssertionError(f"wrong dispatch failure: {response!r}")


def assert_lifecycle_guard(
    stdio: Path,
    bash: str,
    kind: str,
    *,
    malformed: bool = False,
    split_sentinel: bool = False,
) -> None:
    with tempfile.TemporaryDirectory(prefix=f"anvil-stdio-{kind}-") as raw:
        root = Path(raw)
        binary = root / "bin"
        binary.mkdir()
        fake = binary / "emacsclient"
        write_fake_emacsclient(fake)
        probe_count = root / "probe-count"
        init_count = root / "init-count"
        stop_count = root / "stop-count"
        guard_observed = root / "guard-observed"
        environment = os.environ.copy()
        environment.pop("ANVIL_MCP_PARENT_GUARD", None)
        environment.pop("ANVIL_MCP_PARENT_GUARD_PYTHON", None)
        environment.update(
            {
                "PATH": f"{binary}{os.pathsep}{environment['PATH']}",
                "TMPDIR": str(root),
                "EMACS_MCP_DEBUG_LOG": str(root / "debug.log"),
                "FAKE_PROBE_COUNT": str(probe_count),
                "FAKE_DISPATCH_COUNT": str(root / "dispatch-count"),
                "FAKE_EARLY_DISPATCH": str(root / "early-dispatch"),
                "FAKE_GUARD_OBSERVED": str(guard_observed),
                "FAKE_INIT_COUNT": str(init_count),
                "FAKE_STOP_COUNT": str(stop_count),
                f"FAKE_{kind.upper()}_NOT_READY": (
                    "0" if malformed or split_sentinel else "1"
                ),
                f"FAKE_{kind.upper()}_MALFORMED": "1" if malformed else "0",
                f"FAKE_{kind.upper()}_SPLIT_SENTINEL": ("1" if split_sentinel else "0"),
                "ANVIL_MCP_READINESS_MODE": "headless",
                "ANVIL_EMACSCLIENT_PROBE_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_READINESS_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT": "1",
                "ANVIL_EMACSCLIENT_RETRY_MAX": "2",
                "ANVIL_EMACSCLIENT_RETRY_DELAY_MS": "0",
                "ANVIL_MCP_REQUEST_PARSE_TIMEOUT": "2",
                "ANVIL_MCP_FRAME_READ_TIMEOUT": "2",
            }
        )
        completed = subprocess.run(
            [
                bash,
                str(stdio),
                f"--{kind}-function=test-{kind}",
                "--socket=/tmp/anvil-readiness-test",
                "--server-id=anvil",
            ],
            input="",
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=environment,
            text=True,
            timeout=8,
            check=False,
        )
        count = read_count(init_count if kind == "init" else stop_count)
        guard = (
            guard_observed.read_text(encoding="utf-8")
            if guard_observed.exists()
            else ""
        )
        debug = bounded_text(root / "debug.log")
        expected_count = 1 if malformed or split_sentinel else 0
        expected_debug = f"MCP-{kind.upper()}-RC: 70"
        if (
            completed.returncode != 0
            or read_count(probe_count) != 1
            or count != expected_count
            or guard != f"{kind}\n"
            or "substring expression" in completed.stderr
            or ((malformed or split_sentinel) and expected_debug not in debug)
        ):
            if split_sentinel:
                mode = "split-sentinel"
            elif malformed:
                mode = "malformed"
            else:
                mode = "not-ready"
            raise AssertionError(
                f"{kind} {mode} lifecycle guard failed: "
                f"rc={completed.returncode} probes={read_count(probe_count)} "
                f"calls={count} guard={guard!r} stdout={completed.stdout!r} "
                f"stderr={completed.stderr!r} debug={debug!r}"
            )


def assert_signal_cleanup(
    stdio: Path,
    bash: str,
    *,
    repeat: bool,
    forged_record: bool = False,
    cleanup_failure: bool = False,
    owner_death_after_hold: bool = False,
) -> None:
    if os.name != "posix":
        return

    if cleanup_failure:
        label = "cleanup-failure"
    elif owner_death_after_hold:
        label = "owner-death-after-hold"
    else:
        label = "forged-record" if forged_record else ("repeat" if repeat else "single")
    with tempfile.TemporaryDirectory(prefix=f"anvil-stdio-signal-{label}-") as raw:
        root = Path(raw)
        binary = root / "bin"
        binary.mkdir()
        fake = binary / "emacsclient"
        python_fake = binary / "emacsclient-python"
        write_fake_emacsclient(python_fake)
        # Keep the readiness checkpoint independent of Python process startup.
        # Stateful dispatch still uses the full fake so the custody regression
        # exercises the real hanging child and same-group descendant paths.
        write_fast_probe_frontend(fake, bash)
        cleanup_attempt_path = root / "cleanup-attempts"
        cleanup_ack_reads_path = root / "cleanup-ack-reads"
        exit_cleanup_path = root / "exit-cleanup-entries"
        bridge = stdio
        source = stdio.read_text(encoding="utf-8")
        if cleanup_failure:
            bridge = root / "anvil-stdio-cleanup-failure.sh"
            needle = "anvil_mcp_cleanup_all_staged() {\n"
            injected = (
                needle
                + '\tif [ -n "${ANVIL_TEST_CLEANUP_FAILURE:-}" ]; then\n'
                + "\t\tattempts=0\n"
                + '\t\tif [ -f "$FAKE_CLEANUP_ATTEMPTS" ]; then\n'
                + '\t\t\tIFS= read -r attempts < "$FAKE_CLEANUP_ATTEMPTS" || :\n'
                + "\t\tfi\n"
                + '\t\tprintf \'%s\\n\' "$((attempts + 1))" '
                + '> "$FAKE_CLEANUP_ATTEMPTS"\n'
                + "\t\treturn 74\n"
                + "\tfi\n"
            )
            if source.count(needle) != 1:
                raise AssertionError("cleanup failure injection point drifted")
            source = source.replace(needle, injected, 1)
            exit_needle = "anvil_mcp_exit_cleanup() {\n"
            exit_injected = (
                exit_needle
                + '\tif [ -n "${ANVIL_TEST_CLEANUP_FAILURE:-}" ]; then\n'
                + "\t\texit_entries=0\n"
                + '\t\tif [ -f "$FAKE_EXIT_CLEANUP_ENTRIES" ]; then\n'
                + '\t\t\tIFS= read -r exit_entries '
                + '< "$FAKE_EXIT_CLEANUP_ENTRIES" || :\n'
                + "\t\tfi\n"
                + '\t\tprintf \'%s\\n\' "$((exit_entries + 1))" '
                + '> "$FAKE_EXIT_CLEANUP_ENTRIES"\n'
                + "\tfi\n"
            )
            if source.count(exit_needle) != 1:
                raise AssertionError("EXIT cleanup injection point drifted")
            make_executable(
                bridge,
                source.replace(exit_needle, exit_injected, 1),
            )
        elif forged_record:
            # The forged result forces retirement followed by staged cleanup.
            # Pause after cleanup READY so at least two atomic heartbeat chunks
            # queue, then fail the test copy if ACK needs more than one read.
            bridge = root / "anvil-stdio-cleanup-ack-backlog.sh"
            setup_anchor = "set -eu -o pipefail\n"
            operation_anchor = '\tlocal operation="$5"\n'
            publication_anchor = (
                "\t# Publish after READY installed every custody trap, "
                "but acknowledge only\n"
            )
            if (
                source.count(setup_anchor) != 1
                or source.count(operation_anchor) != 1
                or source.count(publication_anchor) != 1
            ):
                raise AssertionError("cleanup ACK stress injection point drifted")
            read_guard = r'''
read() {
	local argument= last= count=0
	for argument do last=$argument; done
	if [ "${ANVIL_TEST_ACK_OPERATION:-}" = cleanup-staged ] \
		&& [ "$last" = runner_ack ]; then
		if [ -f "$FAKE_CLEANUP_ACK_READS" ]; then
			IFS= builtin read -r count < "$FAKE_CLEANUP_ACK_READS" || count=0
		fi
		count=$((count + 1))
		printf '%s\n' "$count" > "$FAKE_CLEANUP_ACK_READS"
		[ "$count" -eq 1 ] || return 1
	fi
	builtin read "$@"
}
'''
            source = source.replace(setup_anchor, setup_anchor + read_guard, 1)
            source = source.replace(
                operation_anchor,
                operation_anchor
                + '\tlocal ANVIL_TEST_ACK_OPERATION="$operation"\n',
                1,
            )
            source = source.replace(
                publication_anchor,
                '\tif [ "$operation" = cleanup-staged ]; then\n'
                '\t\t"$ANVIL_MCP_SLEEP" 1\n'
                "\tfi\n\n"
                + publication_anchor,
                1,
            )
            make_executable(bridge, source)
        hang_pid_path = root / "hang-pid"
        descendant_pid_path = root / "descendant-pid"
        runner_pid_path = root / "runner-pid"
        bridge_pid_path = root / "bridge-pid"
        bridge_reaped_path = root / "bridge-reaped"
        term_count_path = root / "term-count"
        probe_count_path = root / "probe-count"
        fast_probe_count_path = root / "fast-probe-count"
        request = {
            "jsonrpc": "2.0",
            "id": 29,
            "method": "tools/call",
            "params": {"payload": "x" * 20000},
        }
        environment = os.environ.copy()
        environment.pop("ANVIL_MCP_PARENT_GUARD", None)
        environment.pop("ANVIL_MCP_PARENT_GUARD_PYTHON", None)
        environment.update(
            {
                "PATH": f"{binary}{os.pathsep}{environment['PATH']}",
                "FAKE_PYTHON_EMACSCLIENT": str(python_fake),
                "TMPDIR": str(root),
                "EMACS_MCP_DEBUG_LOG": str(root / "debug.log"),
                "FAKE_PROBE_COUNT": str(probe_count_path),
                "FAKE_FAST_PROBE_COUNT": str(fast_probe_count_path),
                "FAKE_DISPATCH_COUNT": str(root / "dispatch-count"),
                "FAKE_EARLY_DISPATCH": str(root / "early-dispatch"),
                "FAKE_GUARD_OBSERVED": str(root / "guard-observed"),
                "FAKE_EXPRESSION": str(root / "expression"),
                "FAKE_INIT_COUNT": str(root / "init-count"),
                "FAKE_STOP_COUNT": str(root / "stop-count"),
                "FAKE_RESPONSE_WIRE": percent_wire(
                    {"jsonrpc": "2.0", "id": 29, "result": True}
                ),
                "FAKE_NIL_BEFORE": "0",
                "FAKE_ALWAYS_NIL": "0",
                "FAKE_ATOMIC_NOT_READY": "0",
                "FAKE_DISPATCH_ERROR": "0",
                "FAKE_MALFORMED_OUTPUT": "0",
                "FAKE_HANG_DISPATCH": "1",
                "FAKE_FORGED_RUNNER_RECORD": "1" if forged_record else "0",
                "FAKE_CLEANUP_ATTEMPTS": str(cleanup_attempt_path),
                "FAKE_CLEANUP_ACK_READS": str(cleanup_ack_reads_path),
                "FAKE_EXIT_CLEANUP_ENTRIES": str(exit_cleanup_path),
                "ANVIL_TEST_CLEANUP_FAILURE": "1" if cleanup_failure else "",
                "FAKE_IGNORE_TERM": "0",
                "FAKE_TERM_COUNT": str(term_count_path),
                "FAKE_HANG_PID": str(hang_pid_path),
                "FAKE_RUNNER_PID": str(runner_pid_path),
                "FAKE_DESCENDANT_PID": str(descendant_pid_path),
                "FAKE_BRIDGE_PID": str(bridge_pid_path),
                "FAKE_BRIDGE_REAPED": str(bridge_reaped_path),
                "FAKE_EXIT_DESCENDANT": "0",
                "FAKE_INVALID_OUTPUT": "0",
                "FAKE_CRLF": "0",
                "ANVIL_MCP_READINESS_MODE": "headless",
                "ANVIL_EMACSCLIENT_PROBE_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_READINESS_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT": "1",
                "ANVIL_EMACSCLIENT_RETRY_MAX": "2",
                "ANVIL_EMACSCLIENT_RETRY_DELAY_MS": "0",
                "ANVIL_MCP_REQUEST_PARSE_TIMEOUT": "5",
                "ANVIL_MCP_FRAME_READ_TIMEOUT": "2",
            }
        )
        launch_command = [
            bash,
            str(bridge),
            "--socket=/tmp/anvil-readiness-test",
            "--server-id=anvil",
        ]
        if owner_death_after_hold:
            supervisor = root / "bridge-supervisor.py"
            make_executable(
                supervisor,
                f"""#!{sys.executable}
import os
from pathlib import Path
import signal
import subprocess
import sys


def publish(name, value):
    path = Path(os.environ[name])
    temporary = path.with_name(path.name + f".{{os.getpid()}}.tmp")
    temporary.write_text(str(value), encoding="ascii")
    os.replace(temporary, path)


bridge = subprocess.Popen(sys.argv[1:])
publish("FAKE_BRIDGE_PID", bridge.pid)
returncode = bridge.wait()
publish("FAKE_BRIDGE_REAPED", returncode)
os.kill(os.getpid(), signal.SIGKILL)
""",
            )
            launch_command = [
                sys.executable,
                "-I",
                "-S",
                str(supervisor),
                *launch_command,
            ]
        process, custody_defer = popen_with_termination_deferred(
            launch_command,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=environment,
            bufsize=0,
            start_new_session=True,
        )
        fake_pid: int | None = None
        runner_pid: int | None = None
        descendant_pid: int | None = None
        bridge_pid = process.pid
        bridge_stopped = False
        try:
            if (
                process.stdin is None
                or process.stdout is None
                or process.stderr is None
            ):
                raise AssertionError("signal regression pipes were not created")
            previous = custody_defer
            custody_defer = None
            restore_termination_signals(previous)
            if owner_death_after_hold:
                bridge_deadline = time.monotonic() + 2
                while (
                    not bridge_pid_path.exists()
                    and time.monotonic() < bridge_deadline
                ):
                    if process_exited_without_reaping(process):
                        break
                    time.sleep(0.01)
                bridge_pid_text = bounded_text(bridge_pid_path, 128).strip()
                if (
                    not bridge_pid_text.isascii()
                    or not bridge_pid_text.isdecimal()
                ):
                    raise AssertionError(
                        "owner-death supervisor did not publish its bridge"
                    )
                bridge_pid = int(bridge_pid_text)
                try:
                    bridge_owned = os.getsid(bridge_pid) == process.pid
                except OSError:
                    bridge_owned = False
                if not bridge_owned:
                    raise AssertionError(
                        "owner-death bridge did not remain in its supervisor session"
                    )
            # Allow the existing 10-second dispatch deadline plus both one-
            # second convergence phases and normal bridge unwinding to settle.
            # This observation window does not change any production deadline.
            deadline = time.monotonic() + 15
            write_all(
                process.stdin,
                (json.dumps(request, separators=(",", ":")) + "\n").encode(),
                deadline,
            )
            while time.monotonic() < deadline:
                if process_exited_without_reaping(process):
                    break
                if (
                    hang_pid_path.exists()
                    and runner_pid_path.exists()
                    and descendant_pid_path.exists()
                    and list(root.glob("anvil-mcp.*"))
                ):
                    fake_pid = int(hang_pid_path.read_text(encoding="utf-8"))
                    runner_pid = int(runner_pid_path.read_text(encoding="utf-8"))
                    descendant_pid = int(
                        descendant_pid_path.read_text(encoding="utf-8")
                    )
                    break
                time.sleep(0.02)
            if fake_pid is None or runner_pid is None or descendant_pid is None:
                failure_debug = bounded_text(root / "debug.log")
                details = (
                    "large request did not reach its staged hanging dispatch "
                    "with a same-group descendant: "
                    f"exited={process_exited_without_reaping(process)} "
                    f"debug={failure_debug!r}"
                )
                raise AssertionError(details)

            if forged_record:
                retirement_deadline = time.monotonic() + 5
                while time.monotonic() < retirement_deadline:
                    if (
                        not process_alive(fake_pid)
                        and not process_alive(runner_pid)
                        and not process_alive(descendant_pid)
                    ):
                        break
                    time.sleep(0.02)
                if (
                    process_alive(fake_pid)
                    or process_alive(runner_pid)
                    or process_alive(descendant_pid)
                ):
                    raise AssertionError(
                        "forged child NUL/status suppressed runner escalation"
                    )
                if not signal_owned_process(
                    bridge_pid, process.pid, signal.SIGTERM
                ) and not process_exited_without_reaping(process):
                    raise AssertionError(
                        "bridge lost session ownership before forged-record TERM"
                    )
            elif repeat:
                if not signal_owned_process(
                    runner_pid, process.pid, signal.SIGTERM
                ):
                    raise AssertionError("runner lost session ownership before TERM")
                term_deadline = time.monotonic() + 1
                while (
                    read_count(term_count_path) != 1
                    and time.monotonic() < term_deadline
                ):
                    time.sleep(0.01)
                if read_count(term_count_path) != 1:
                    raise AssertionError(
                        "direct runner TERM did not reach its exact child"
                    )
                time.sleep(0.1)
                if (
                    process_exited_without_reaping(process)
                    or not process_alive(runner_pid)
                    or not process_alive(fake_pid)
                    or not process_alive(descendant_pid)
                ):
                    raise AssertionError(
                        "runner lost exact custody after the first TERM"
                    )

                # Freeze the reader so the status pipe remains open while the
                # exact runner reaps its child and enters the held-writer path.
                if not signal_owned_process(
                    bridge_pid, process.pid, signal.SIGSTOP
                ):
                    raise AssertionError("bridge lost session ownership before STOP")
                bridge_stopped = True
                time.sleep(0.05)
                if not signal_owned_process(
                    runner_pid, process.pid, signal.SIGUSR1
                ):
                    raise AssertionError("runner lost session ownership before USR1")
                kill_deadline = time.monotonic() + 1
                while (
                    (
                        process_alive(fake_pid)
                        or process_alive(descendant_pid)
                    )
                    and time.monotonic() < kill_deadline
                ):
                    time.sleep(0.01)
                if (
                    process_alive(fake_pid)
                    or process_alive(descendant_pid)
                    or not process_alive(runner_pid)
                ):
                    raise AssertionError(
                        "runner did not retain exact custody after TERM then USR1"
                    )

                if owner_death_after_hold:
                    # Kill only the frozen bridge leader.  Its reader vanishes,
                    # and the already-closed runner writer must not spin forever.
                    if not signal_owned_process(
                        bridge_pid, process.pid, signal.SIGKILL
                    ):
                        raise AssertionError(
                            "bridge lost session ownership before owner death"
                        )
                    bridge_stopped = False
                else:
                    # Signals that interrupt the held pipe write must not let the
                    # runner escape before the parent closes its reader.
                    for repeated_signal in (signal.SIGTERM, signal.SIGUSR1):
                        if not signal_owned_process(
                            runner_pid, process.pid, repeated_signal
                        ):
                            raise AssertionError(
                                "runner lost session ownership during held write"
                            )
                        time.sleep(0.05)
                        if not process_alive(runner_pid):
                            raise AssertionError(
                                "signal-interrupted held runner exited before FD7 closed"
                            )

                    # Queue bridge termination while its reader is still frozen;
                    # SIGCONT delivers it before normal response processing resumes.
                    if not signal_owned_process(
                        bridge_pid, process.pid, signal.SIGTERM
                    ) and not process_exited_without_reaping(process):
                        raise AssertionError(
                            "bridge lost session ownership before queued TERM"
                        )
                    if not signal_owned_process(
                        bridge_pid, process.pid, signal.SIGCONT
                    ):
                        raise AssertionError(
                            "bridge lost session ownership before CONT"
                        )
                    bridge_stopped = False
            else:
                if not signal_owned_process(
                    bridge_pid, process.pid, signal.SIGTERM
                ) and not process_exited_without_reaping(process):
                    raise AssertionError(
                        "bridge lost session ownership before direct TERM"
                    )
            if not wait_process_exit_unreaped(process, 5):
                debug = bounded_text(root / "debug.log")
                raise AssertionError(
                    "direct bridge SIGTERM did not produce a bounded exit: "
                    f"child_alive={process_alive(fake_pid)} "
                    f"runner_alive={process_alive(runner_pid)} "
                    f"descendant_alive={process_alive(descendant_pid)} "
                    f"debug={debug!r}"
                )
            if owner_death_after_hold:
                bridge_returncode = bounded_text(
                    bridge_reaped_path, 128
                ).strip()
                if bridge_returncode != str(-signal.SIGKILL):
                    raise AssertionError(
                        "owner-death supervisor did not reap the killed bridge"
                    )

            stage_retained = cleanup_failure or owner_death_after_hold
            deadline = time.monotonic() + 2
            child_alive = True
            runner_alive = True
            descendant_alive = True
            group_alive = True
            session_children: dict[int, int] = {}
            staged = list(root.glob("anvil-mcp.*"))
            while time.monotonic() < deadline:
                session_children = session_processes(process.pid)
                session_children.pop(process.pid, None)
                child_alive = fake_pid in session_children
                runner_alive = runner_pid in session_children
                descendant_alive = descendant_pid in session_children
                group_alive = bool(session_children)
                staged = list(root.glob("anvil-mcp.*"))
                if (
                    not child_alive
                    and not runner_alive
                    and not descendant_alive
                    and not group_alive
                    and (bool(staged) if stage_retained else not staged)
                ):
                    break
                time.sleep(0.02)
            cleanup_attempts = read_count(cleanup_attempt_path)
            cleanup_ack_reads = read_count(cleanup_ack_reads_path)
            exit_cleanup_entries = read_count(exit_cleanup_path)
            expected_returncode = (
                -signal.SIGKILL if owner_death_after_hold else 143
            )
            debug = bounded_text(root / "debug.log")
            group_snapshot = repr(sorted(session_children.items()))
            if (
                child_alive
                or runner_alive
                or descendant_alive
                or group_alive
                or (not staged if stage_retained else bool(staged))
            ):
                raise AssertionError(
                    f"{label} bridge retained custody before reap: "
                    f"child_alive={child_alive} runner_alive={runner_alive} "
                    f"descendant_alive={descendant_alive} group_alive={group_alive} "
                    f"staged={[path.name for path in staged]!r} "
                    f"debug={debug!r} group_snapshot={group_snapshot!r}"
                )
            process.wait(timeout=HARNESS_KILL_GRACE_SECONDS)
            stdout = bounded_stream_text(process.stdout)
            stderr = bounded_stream_text(process.stderr)
            if (
                process.returncode != expected_returncode
                or read_count(term_count_path) != 1
                or read_count(probe_count_path) != 1
                or read_count(fast_probe_count_path) != 1
                or child_alive
                or runner_alive
                or descendant_alive
                or group_alive
                or (not staged if stage_retained else bool(staged))
                or cleanup_attempts != (2 if cleanup_failure else 0)
                or cleanup_ack_reads != (1 if forged_record else 0)
                or exit_cleanup_entries != 0
                or (stdout and not forged_record)
                or "substring expression" in stderr
            ):
                raise AssertionError(
                    f"{label} bridge termination failed custody: "
                    f"rc={process.returncode} terms={read_count(term_count_path)} "
                    f"probes={read_count(probe_count_path)}/"
                    f"{read_count(fast_probe_count_path)} "
                    f"child_alive={child_alive} runner_alive={runner_alive} "
                    f"descendant_alive={descendant_alive} group_alive={group_alive} "
                    f"cleanup_attempts={cleanup_attempts} "
                    f"cleanup_ack_reads={cleanup_ack_reads} "
                    f"exit_cleanup_entries={exit_cleanup_entries} "
                    f"staged={[path.name for path in staged]!r} "
                    f"stdout={stdout!r} stderr={stderr!r} "
                    f"debug={debug!r} group_snapshot={group_snapshot!r}"
                )
        finally:
            if bridge_stopped and bridge_pid != process.pid:
                signal_owned_process(bridge_pid, process.pid, signal.SIGCONT)
                bridge_stopped = False
            cleanup_fixture_process(
                process,
                term_grace=shutdown_observer_budget(5, 1),
                resume=bridge_stopped and bridge_pid == process.pid,
            )
            if custody_defer is not None:
                restore_termination_signals(custody_defer)


def assert_signal_publication(
    stdio: Path,
    bash: str,
    *,
    delivered_ack: bool,
) -> None:
    if os.name != "posix":
        return

    label = "delivered-ack" if delivered_ack else "pre-ack"
    with tempfile.TemporaryDirectory(
        prefix=f"anvil-stdio-publication-{label}-"
    ) as raw:
        root = Path(raw)
        binary = root / "bin"
        binary.mkdir()
        make_executable(
            binary / "emacsclient",
            f"#!{bash}\nprintf 't\\n'\n",
        )
        real_python = sys.executable
        real_sleep = shutil.which("sleep")
        if real_sleep is None:
            raise AssertionError("publication regression requires sleep")
        # Avoid charging Python loader scheduling to the signal-custody timer.
        # The first helper publishes its PID and execs a TERM-ignoring sleeper;
        # later invocations delegate to the real interpreter for cleanup.
        make_executable(
            binary / "python3",
            f"""#!{bash}
if (set -C; : > "$FAKE_PUBLICATION_ONCE") 2>/dev/null; then
    printf '%s' "$$" > "$FAKE_PUBLICATION_READY"
    trap '' TERM
    exec "$FAKE_PUBLICATION_SLEEP" 30
fi
exec "$FAKE_PUBLICATION_REAL_PYTHON" "$@"
""",
        )

        bridge = root / "anvil-stdio-publication.sh"
        source = stdio.read_text(encoding="utf-8")
        needle = (
            "\t\tanvil_mcp_finish_bounded_termination \"$runner\" 0 "
            "\"$runner_owned\"\n"
            "\t\tANVIL_MCP_RUNNER_CRITICAL=1\n"
        )
        if source.count(needle) != 1:
            raise AssertionError("runner publication injection point drifted")
        if delivered_ack:
            injected = (
                needle
                + "\t\tanvil_mcp_test_deliver_ack() {\n"
                + '\t\t\tlocal target="$1" attempts=0 ignored\n'
                + '\t\t\tbuiltin kill -USR2 "$target" || return 1\n'
                + '\t\t\twhile IFS= read -r -t 5 ignored <&7; do\n'
                + '\t\t\t\t[ "$ignored" != "ANVIL-MCP-RUNNER-ACK" ] '
                + '|| break\n'
                + "\t\t\tdone\n"
                + '\t\t\twhile [ ! -e "$FAKE_PUBLICATION_READY" ] '
                + '&& [ "$attempts" -lt 500 ]; do\n'
                + "\t\t\t\tattempts=$((attempts + 1))\n"
                + "\t\t\t\tcommand sleep 0.01\n"
                + "\t\t\tdone\n"
                + '\t\t\t: > "$FAKE_PUBLICATION_SIGNAL_SENT"\n'
                + '\t\t\tbuiltin kill -TERM "$$"\n'
                + "\t\t\treturn 1\n"
                + "\t\t}\n"
                + '\t\tprintf \'%s\' "$runner" '
                + '> "$FAKE_RUNNER_PUBLICATION_READY"\n'
            )
            source = source.replace(needle, injected, 1)
            ack_needle = (
                'kill -USR2 "$ANVIL_MCP_ACTIVE_RUNNER" 2>/dev/null'
            )
            if source.count(ack_needle) != 1:
                raise AssertionError("ACK syscall injection point drifted")
            source = source.replace(
                ack_needle,
                'anvil_mcp_test_deliver_ack "$ANVIL_MCP_ACTIVE_RUNNER"',
                1,
            )
        else:
            injected = (
                needle
                + '\t\tif [ -n "${ANVIL_TEST_SIGNAL_PUBLICATION:-}" ]; then\n'
                + "\t\t\tunset ANVIL_TEST_SIGNAL_PUBLICATION\n"
                + '\t\t\tprintf \'%s\' "$runner" '
                + '> "$FAKE_RUNNER_PUBLICATION_READY"\n'
                + '\t\t\t: > "$FAKE_PUBLICATION_SIGNAL_SENT"\n'
                + '\t\t\tkill -TERM "$$"\n'
                + "\t\tfi\n"
            )
            source = source.replace(needle, injected, 1)
        make_executable(bridge, source)

        ready_path = root / "publication-helper-ready"
        runner_ready_path = root / "publication-runner-ready"
        signal_sent_path = root / "publication-signal-sent"
        environment = os.environ.copy()
        environment.pop("ANVIL_MCP_PARENT_GUARD", None)
        environment.pop("ANVIL_MCP_PARENT_GUARD_PYTHON", None)
        environment.update(
            {
                "PATH": f"{binary}{os.pathsep}{environment['PATH']}",
                "TMPDIR": str(root),
                "EMACS_MCP_DEBUG_LOG": str(root / "debug.log"),
                "ANVIL_TEST_SIGNAL_PUBLICATION": "1",
                "FAKE_PUBLICATION_ONCE": str(root / "publication-once"),
                "FAKE_PUBLICATION_READY": str(ready_path),
                "FAKE_PUBLICATION_REAL_PYTHON": real_python,
                "FAKE_PUBLICATION_SLEEP": real_sleep,
                "FAKE_RUNNER_PUBLICATION_READY": str(runner_ready_path),
                "FAKE_PUBLICATION_SIGNAL_SENT": str(signal_sent_path),
                "ANVIL_MCP_READINESS_MODE": "emacs",
                "ANVIL_EMACSCLIENT_PROBE_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_READINESS_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT": "20",
                "ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT": "150",
                "ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT": "1",
                "ANVIL_EMACSCLIENT_RETRY_MAX": "1",
                "ANVIL_EMACSCLIENT_RETRY_DELAY_MS": "0",
                "ANVIL_MCP_REQUEST_PARSE_TIMEOUT": "2",
                "ANVIL_MCP_FRAME_READ_TIMEOUT": "2",
            }
        )
        process, custody_defer = popen_with_termination_deferred(
            [bash, str(bridge), "--server-id=anvil"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=environment,
            bufsize=0,
            start_new_session=True,
        )
        runner_pid: int | None = None
        helper_pid: int | None = None
        try:
            if process.stdin is None:
                raise AssertionError("publication regression stdin is unavailable")
            previous = custody_defer
            custody_defer = None
            restore_termination_signals(previous)
            deadline = time.monotonic() + 5
            write_all(
                process.stdin,
                b'{"jsonrpc":"2.0","id":41,"method":"tools/call"}\n',
                deadline,
            )
            while time.monotonic() < deadline:
                if runner_ready_path.exists():
                    runner_pid = int(
                        runner_ready_path.read_text(encoding="utf-8")
                    )
                    break
                if process_exited_without_reaping(process):
                    break
                time.sleep(0.02)
            if runner_pid is None:
                raise AssertionError(
                    "pre-ACK runner identity was never published"
                )
            deadline = time.monotonic() + 8
            while time.monotonic() < deadline:
                if signal_sent_path.exists() or process_exited_without_reaping(
                    process
                ):
                    break
                time.sleep(0.01)
            if not signal_sent_path.exists():
                failure_debug = bounded_text(root / "debug.log")
                raise AssertionError(
                    f"{label} publication regression never reached its "
                    "injected signal: "
                    f"exited={process_exited_without_reaping(process)} "
                    f"debug={failure_debug!r}"
                )
            convergence_started = time.monotonic()
            if not wait_process_exit_unreaped(process, 8):
                raise AssertionError(
                    "signal during runner publication did not converge "
                    "independently of the 150-second command deadline"
                )
            convergence_elapsed = time.monotonic() - convergence_started

            helper_started = ready_path.exists()
            if helper_started:
                helper_pid = int(ready_path.read_text(encoding="utf-8"))
            deadline = time.monotonic() + 2
            runner_alive = True
            group_alive = True
            helper_alive = helper_pid is not None
            session_children: dict[int, int] = {}
            while time.monotonic() < deadline:
                session_children = session_processes(process.pid)
                session_children.pop(process.pid, None)
                runner_alive = runner_pid in session_children
                group_alive = bool(session_children)
                helper_alive = helper_pid in session_children
                if not runner_alive and not group_alive and not helper_alive:
                    break
                time.sleep(0.02)
            staged = list(root.glob("anvil-mcp.*"))
            group_snapshot = repr(sorted(session_children.items()))
            if runner_alive or group_alive or helper_alive or staged:
                raise AssertionError(
                    f"{label} publication retained custody before reap: "
                    f"runner_alive={runner_alive} group_alive={group_alive} "
                    f"helper_alive={helper_alive} "
                    f"staged={[path.name for path in staged]!r} "
                    f"group_snapshot={group_snapshot!r}"
                )
            process.wait(timeout=HARNESS_KILL_GRACE_SECONDS)
            stdout = bounded_stream_text(process.stdout)
            stderr = bounded_stream_text(process.stderr)
            if (
                process.returncode != 143
                or runner_alive
                or group_alive
                or helper_alive
                or helper_started != delivered_ack
                or staged
                or stdout
                or "substring expression" in stderr
                or convergence_elapsed >= 3
            ):
                raise AssertionError(
                    f"{label} publication signal lost custody: "
                    f"rc={process.returncode} runner_alive={runner_alive} "
                    f"group_alive={group_alive} helper_alive={helper_alive} "
                    f"helper_started={helper_started} "
                    f"convergence_elapsed={convergence_elapsed:.3f} "
                    f"staged={[path.name for path in staged]!r} "
                    f"stdout={stdout!r} stderr={stderr!r} "
                    f"group_snapshot={group_snapshot!r}"
                )
        finally:
            cleanup_fixture_process(
                process,
                term_grace=shutdown_observer_budget(2, 1),
            )
            if custody_defer is not None:
                restore_termination_signals(custody_defer)


def assert_delayed_ack_preserves_runner(
    stdio: Path,
    bash: str,
    expected: dict[str, object],
) -> None:
    """READY custody survives parent descheduling beyond five seconds."""
    with tempfile.TemporaryDirectory(prefix="anvil-stdio-delayed-ack-") as raw:
        root = Path(raw)
        bridge = root / "anvil-stdio-delayed-ack.sh"
        once = root / "delay-once"
        runner_record = root / "runner"
        source = stdio.read_text(encoding="utf-8")
        needle = (
            "\t# Publish after READY installed every custody trap, but acknowledge only\n"
        )
        if source.count(needle) != 1:
            raise AssertionError("delayed ACK injection point drifted")
        injected = (
            f"\tif [ \"$operation\" = dispatch ] && [ ! -e '{once}' ]; then\n"
            f"\t\t: > '{once}'\n"
            f"\t\tprintf '%s\\n' \"$runner\" > '{runner_record}'\n"
            "\t\t\"$ANVIL_MCP_SLEEP\" 6\n"
            "\tfi\n\n"
            + needle
        )
        make_executable(bridge, source.replace(needle, injected, 1))

        response, probes, dispatches, elapsed, diagnostics, _guard = run_case(
            bridge,
            bash,
            dispatch_timeout=15,
            request_parse_timeout=10,
            anvil_root=stdio.parent,
        )
        if (
            not strict_equal(response, expected)
            or probes != 1
            or dispatches != 1
            or not 6 <= elapsed < 25
            or not runner_record.exists()
        ):
            raise AssertionError(
                "READY runner was not retained across delayed ACK: "
                f"response={response!r} probes={probes} "
                f"dispatches={dispatches} elapsed={elapsed:.3f} "
                f"diagnostics={diagnostics!r}"
            )


def assert_saturated_heartbeat_preserves_ack(stdio: Path, bash: str) -> None:
    """A signal-interrupted heartbeat must still publish ACK and status."""
    source = stdio.read_text(encoding="utf-8")
    start = source.index("anvil_mcp_exec_program() {")
    end = source.index("\n# Drain one bounded retirement phase", start)
    runner_source = source[start:end]
    heartbeat = "printf 'ANVIL-MCP-RUNNER-WAIT:%0234d' 0"
    if runner_source.count(heartbeat) != 1:
        raise AssertionError("heartbeat instrumentation point drifted")
    runner_source = runner_source.replace(
        heartbeat,
        "anvil_mcp_test_heartbeat",
        1,
    )

    with tempfile.TemporaryDirectory(prefix="anvil-stdio-heartbeat-") as raw:
        root = Path(raw)
        before_path = root / "heartbeat-before"
        after_path = root / "heartbeat-after"
        harness = root / "heartbeat-harness.sh"
        true_program = shutil.which("true")
        if true_program is None:
            raise AssertionError("heartbeat regression requires an external true")
        make_executable(
            harness,
            f"""#!{bash}
set -eu -o pipefail

anvil_mcp_test_heartbeat() {{
    local heartbeat_status=0
    printf x >> "$ANVIL_TEST_HEARTBEAT_BEFORE"
    printf '%s' "$ANVIL_TEST_HEARTBEAT_PAYLOAD" \\
        || heartbeat_status=$?
    printf x >> "$ANVIL_TEST_HEARTBEAT_AFTER"
    return "$heartbeat_status"
}}

{runner_source}

ANVIL_MCP_PARENT_GUARD=
ANVIL_MCP_PARENT_GUARD_PYTHON=
ANVIL_MCP_SLEEP="$ANVIL_TEST_TRUE"
exec 7< <(anvil_mcp_run_child "$$" 150 merge null "" "$ANVIL_TEST_TRUE")
runner=$!
if ! IFS= read -r -t 5 ready <&7 \\
    || [ "$ready" != "ANVIL-MCP-RUNNER-READY" ]; then
    exit 71
fi
printf 'READY %s\\n' "$runner"
IFS= read -r command
[ "$command" = go ] || exit 72
kill -USR2 "$runner"
"$ANVIL_TEST_REAL_SLEEP" 0.2
record=
IFS= read -r -t 5 record <&7 || exit 73
[[ "$record" = *ANVIL-MCP-RUNNER-ACK ]] || exit 73
output=
IFS= read -r -d '' output <&7 || exit 74
IFS= read -r status <&7 || exit 75
[ "$status" = 0 ] || exit 76
exec 7<&-
printf 'DONE %s\\n' "$runner"
""",
        )

        environment = os.environ.copy()
        environment.update(
            {
                "ANVIL_TEST_HEARTBEAT_BEFORE": str(before_path),
                "ANVIL_TEST_HEARTBEAT_AFTER": str(after_path),
                # The 256-byte payload is below POSIX's minimum PIPE_BUF of
                # 512.  It therefore preserves the production heartbeat's
                # atomic-write and bounded-pipe semantics in this harness.
                "ANVIL_TEST_HEARTBEAT_PAYLOAD": "W" * 256,
                "ANVIL_TEST_TRUE": true_program,
                "ANVIL_TEST_REAL_SLEEP": subprocess.run(
                    ["sh", "-c", "command -v sleep"],
                    stdout=subprocess.PIPE,
                    text=True,
                    check=True,
                    timeout=2,
                ).stdout.strip(),
            }
        )
        stderr_path = root / "heartbeat.stderr"
        stderr_handle = stderr_path.open("wb")
        try:
            process, custody_defer = popen_with_termination_deferred(
                [bash, str(harness)],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=stderr_handle,
                env=environment,
                bufsize=0,
                start_new_session=True,
            )
        except BaseException:
            stderr_handle.close()
            raise
        runner_pid: int | None = None
        reader: BoundedPipeReader | None = None
        try:
            stderr_handle.close()
            if process.stdin is None or process.stdout is None:
                raise AssertionError("heartbeat regression pipes were not created")
            reader = BoundedPipeReader(process.stdout)
            previous = custody_defer
            custody_defer = None
            restore_termination_signals(previous)
            try:
                ready_line = reader.line(time.monotonic() + 5).decode(
                    "ascii", errors="strict"
                )
            except (EOFError, TimeoutError, UnicodeDecodeError) as error:
                raise AssertionError(
                    "saturated heartbeat runner never published READY"
                ) from error
            pieces = ready_line.strip().split()
            if len(pieces) != 2 or pieces[0] != "READY":
                raise AssertionError(
                    f"invalid saturated heartbeat READY record: {ready_line!r}"
                )
            runner_pid = int(pieces[1])

            saturation_deadline = time.monotonic() + 10
            imbalance_since: float | None = None
            saturated = False
            before_size = 0
            after_size = 0
            while time.monotonic() < saturation_deadline:
                if process_exited_without_reaping(process):
                    break
                before_size = (
                    before_path.stat().st_size if before_path.exists() else 0
                )
                after_size = after_path.stat().st_size if after_path.exists() else 0
                if before_size == after_size + 1:
                    if imbalance_since is None:
                        imbalance_since = time.monotonic()
                    elif time.monotonic() - imbalance_since >= 0.2:
                        saturated = True
                        break
                else:
                    imbalance_since = None
                time.sleep(0.01)
            if not saturated:
                runner_alive = process_alive(runner_pid)
                group_snapshot = repr(sorted(session_process_groups(process.pid)))
                stderr = bounded_text(stderr_path)
                raise AssertionError(
                    "heartbeat pipe did not reach a blocked write: "
                    f"rc={process.returncode} runner_alive={runner_alive} "
                    f"before={before_size} after={after_size} "
                    f"stderr={stderr!r} group_snapshot={group_snapshot!r}"
                )

            write_all(process.stdin, b"go\n", time.monotonic() + 2)
            process.stdin.close()
            process.stdin = None
            reader.close()
            reader = None
            if not wait_process_exit_unreaped(process, 8):
                raise AssertionError(
                    "signal-interrupted saturated heartbeat did not converge"
                )
            deadline = time.monotonic() + 2
            session_children: dict[int, int] = {}
            while time.monotonic() < deadline:
                session_children = session_processes(process.pid)
                session_children.pop(process.pid, None)
                if not session_children:
                    break
                time.sleep(0.02)
            if session_children:
                raise AssertionError(
                    "saturated heartbeat retained custody before reap: "
                    f"runner={runner_pid} children={sorted(session_children)!r}"
                )
            process.wait(timeout=HARNESS_KILL_GRACE_SECONDS)
            stdout = bounded_stream_text(process.stdout)
            stderr = bounded_text(stderr_path)
            if process.returncode != 0 or not stdout.startswith("DONE "):
                raise AssertionError(
                    "signal-interrupted saturated heartbeat lost ACK/status: "
                    f"rc={process.returncode} stdout={stdout!r} stderr={stderr!r}"
                )
        finally:
            if not stderr_handle.closed:
                stderr_handle.close()
            cleanup_fixture_process(
                process,
                term_grace=2,
                reader=reader,
            )
            if custody_defer is not None:
                restore_termination_signals(custody_defer)


def assert_drain_phase_budget(stdio: Path, bash: str) -> None:
    """Each drain retains a full grace period across whole-second ticks."""
    source = stdio.read_text(encoding="utf-8")
    start = source.index("anvil_mcp_drain_runner_output() {")
    end = source.index("\n}\n\n# Converge", start) + 2
    drain_function = source[start:end]
    with tempfile.TemporaryDirectory(prefix="anvil-stdio-drain-budget-") as raw:
        root = Path(raw)
        script = root / "drain-budget.sh"
        make_executable(
            script,
            f"""#!{bash}
set -e
{drain_function}
ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT=1
exec 7< <(
    trap 'exit 0' TERM
    while :; do
        printf '\\0' || exit 0
    done 2>/dev/null
)
writer=$!
SECONDS=0
sleep 0.95
before=$SECONDS
anvil_mcp_drain_runner_output
elapsed=$((SECONDS - before))
kill -KILL "$writer" 2>/dev/null || :
exec 7<&-
printf '%s\\n' "$elapsed"
""",
        )
        completed = subprocess.run(
            [bash, str(script)],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=False,
            timeout=6,
        )
        try:
            elapsed = int(completed.stdout.strip())
        except ValueError as error:
            raise AssertionError(
                f"drain phase emitted invalid timing: {completed.stdout!r}"
            ) from error
        if completed.returncode != 0 or elapsed < 2:
            raise AssertionError(
                "drain phase lost its fractional grace at a SECONDS boundary: "
                f"rc={completed.returncode} elapsed={elapsed} "
                f"stderr={completed.stderr!r}"
            )


def assert_exit_without_staging(stdio: Path, bash: str) -> None:
    with tempfile.TemporaryDirectory(prefix="anvil-stdio-exit-") as raw:
        root = Path(raw)
        binary = root / "bin"
        binary.mkdir()
        cleanup_marker = root / "cleanup-helper"
        real_python = sys.executable
        python_wrapper = f"""#!{real_python}
import os
from pathlib import Path
import sys

code = "\\n".join(sys.argv[1:])
if (
    "def generation_file(name, prefix):" in code
    and "os.rmdir(directory)" in code
):
    Path(os.environ["FAKE_CLEANUP_INVOKED"]).write_text(
        "cleanup helper ran\\n", encoding="utf-8"
    )
os.execv({real_python!r}, [{real_python!r}, *sys.argv[1:]])
"""
        make_executable(binary / "python3", python_wrapper)
        make_executable(
            binary / "emacsclient",
            f"#!{bash}\nprintf 't\\n'\n",
        )
        environment = os.environ.copy()
        environment.pop("ANVIL_MCP_PARENT_GUARD", None)
        environment.pop("ANVIL_MCP_PARENT_GUARD_PYTHON", None)
        environment.update(
            {
                "PATH": f"{binary}{os.pathsep}{environment['PATH']}",
                "TMPDIR": str(root),
                "FAKE_CLEANUP_INVOKED": str(cleanup_marker),
                "ANVIL_MCP_READINESS_MODE": "emacs",
                "ANVIL_EMACSCLIENT_PROBE_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_READINESS_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT": "1",
                "ANVIL_EMACSCLIENT_RETRY_MAX": "1",
                "ANVIL_EMACSCLIENT_RETRY_DELAY_MS": "0",
                "ANVIL_MCP_REQUEST_PARSE_TIMEOUT": "2",
                "ANVIL_MCP_FRAME_READ_TIMEOUT": "2",
            }
        )
        completed = subprocess.run(
            [bash, str(stdio), "--server-id=anvil"],
            input="",
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=environment,
            text=True,
            timeout=5,
            check=False,
        )
        staged = list(root.glob("anvil-mcp.*"))
        if completed.returncode != 0 or cleanup_marker.exists() or staged:
            raise AssertionError(
                "stage-free bridge exit launched cleanup work: "
                f"rc={completed.returncode} marker={cleanup_marker.exists()} "
                f"staged={[path.name for path in staged]!r} "
                f"stdout={completed.stdout!r} stderr={completed.stderr!r}"
            )


def assert_invalid_configuration(
    stdio: Path,
    bash: str,
    name: str,
    value: str,
    *,
    expected_fragment: str | None = None,
    server_id: str = "anvil",
) -> None:
    with tempfile.TemporaryDirectory(prefix="anvil-stdio-config-") as raw:
        binary = Path(raw) / "bin"
        binary.mkdir()
        make_executable(binary / "emacsclient", f"#!{bash}\nexit 0\n")
        environment = os.environ.copy()
        environment.pop("ANVIL_MCP_PARENT_GUARD", None)
        environment.pop("ANVIL_MCP_PARENT_GUARD_PYTHON", None)
        environment.update(
            {
                "PATH": f"{binary}{os.pathsep}{environment['PATH']}",
                "ANVIL_MCP_READINESS_MODE": "emacs",
                "ANVIL_EMACSCLIENT_PROBE_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_READINESS_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT": "1",
                "ANVIL_EMACSCLIENT_RETRY_MAX": "2",
                "ANVIL_EMACSCLIENT_RETRY_DELAY_MS": "0",
                "ANVIL_MCP_REQUEST_PARSE_TIMEOUT": "2",
                "ANVIL_MCP_FRAME_READ_TIMEOUT": "2",
                name: value,
            }
        )
        completed = subprocess.run(
            [bash, str(stdio), f"--server-id={server_id}"],
            input="",
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=environment,
            text=True,
            timeout=3,
            check=False,
        )
        fragment = expected_fragment or name
        if completed.returncode != 64 or fragment not in completed.stderr:
            raise AssertionError(
                f"invalid {name}={value!r} was not rejected cleanly: "
                f"rc={completed.returncode} stderr={completed.stderr!r}"
            )


def main() -> int:
    if len(sys.argv) != 4:
        raise SystemExit(f"usage: {Path(sys.argv[0]).name} ANVIL_STDIO BASH EMACS")
    stdio = Path(sys.argv[1]).resolve()
    bash = str(Path(sys.argv[2]).resolve())
    real_emacs = str(Path(sys.argv[3]).resolve())
    if not os.access(real_emacs, os.X_OK):
        raise AssertionError(f"real Emacs is not executable: {real_emacs}")
    os.environ["ANVIL_TEST_REAL_EMACS"] = real_emacs
    expected = {"jsonrpc": "2.0", "id": 17, "result": {"ready": True}}
    assert_harness_timeout_reaps_separate_child(bash)
    assert_reaped_pid_alias_does_not_extend_wait(stdio, bash, expected)

    success, probes, dispatches, _elapsed, stderr, _guard = run_case(
        stdio, bash, nil_before=2
    )
    if not strict_equal(success, expected) or probes != 3 or dispatches != 1:
        raise AssertionError(
            f"nil-to-ready case failed: response={success!r} "
            f"probes={probes} dispatches={dispatches} stderr={stderr!r}"
        )

    delayed_success, probes, dispatches, elapsed, stderr, _guard = run_case(
        stdio,
        bash,
        real_emacs_delay_sec=5.1,
        dispatch_timeout=20,
    )
    if (
        not strict_equal(delayed_success, expected)
        or probes != 1
        or dispatches != 1
        or not 5 < elapsed < 20
    ):
        raise AssertionError(
            "real Emacs was preempted by a nested harness timeout: "
            f"response={delayed_success!r} probes={probes} "
            f"dispatches={dispatches} elapsed={elapsed:.3f} "
            f"stderr={stderr!r}"
        )

    large_success, probes, dispatches, _elapsed, stderr, _guard = run_case(
        stdio, bash, large_request=True
    )
    if not strict_equal(large_success, expected) or probes != 1 or dispatches != 1:
        raise AssertionError(
            f"large consumed-marker case failed: response={large_success!r} "
            f"probes={probes} dispatches={dispatches} stderr={stderr!r}"
        )

    group_success, probes, dispatches, _elapsed, stderr, _guard = run_case(
        stdio, bash, exit_descendant=True
    )
    if not strict_equal(group_success, expected) or probes != 1 or dispatches != 1:
        raise AssertionError(
            f"same-group descendant cleanup failed: response={group_success!r} "
            f"probes={probes} dispatches={dispatches} stderr={stderr!r}"
        )

    crlf, probes, dispatches, _elapsed, _stderr, _guard = run_case(
        stdio, bash, crlf=True, nil_before=2
    )
    if not strict_equal(crlf, expected) or probes != 3 or dispatches != 1:
        raise AssertionError(
            f"CRLF readiness was not accepted: response={crlf!r} "
            f"probes={probes} dispatches={dispatches}"
        )

    exhausted, probes, dispatches, elapsed, diagnostics, _guard = run_case(
        stdio, bash, always_nil=True, readiness_timeout=1, retry_max=1
    )
    assert_readiness_error(exhausted)
    if (
        probes < 1
        or dispatches != 0
        or not 0 < elapsed < 2.5
        or "MCP-PROBE-WAIT-EXHAUSTED" not in diagnostics
    ):
        raise AssertionError(
            "nil deadline was not bounded and fail-closed: "
            f"probes={probes} dispatches={dispatches} elapsed={elapsed:.3f} "
            f"diagnostics={diagnostics!r}"
        )

    delayed, probes, dispatches, elapsed, diagnostics, _guard = run_case(
        stdio,
        bash,
        always_nil=True,
        readiness_timeout=1,
        retry_delay_ms=5000,
        retry_max=1,
    )
    assert_readiness_error(delayed)
    if (
        probes < 1
        or dispatches != 0
        or not 0 < elapsed < 2.5
        or "MCP-PROBE-WAIT-EXHAUSTED" not in diagnostics
    ):
        raise AssertionError(
            "retry delay exceeded the readiness budget: "
            f"probes={probes} dispatches={dispatches} elapsed={elapsed:.3f} "
            f"diagnostics={diagnostics!r}"
        )

    raced, probes, dispatches, _elapsed, _stderr, guard = run_case(
        stdio, bash, atomic_not_ready=True
    )
    assert_readiness_error(raced)
    if probes != 1 or dispatches != 0 or guard != "jsonrpc\n":
        raise AssertionError(
            "atomic JSON-RPC guard was not observed before dispatch: "
            f"probes={probes} dispatches={dispatches} guard={guard!r}"
        )

    large_raced, probes, dispatches, _elapsed, _stderr, guard = run_case(
        stdio, bash, atomic_not_ready=True, large_request=True
    )
    assert_readiness_error(large_raced)
    if probes != 1 or dispatches != 0 or guard != "jsonrpc\n":
        raise AssertionError(
            "large-request atomic guard was not observed: "
            f"probes={probes} dispatches={dispatches} guard={guard!r}"
        )

    failed, probes, dispatches, _elapsed, _stderr, _guard = run_case(
        stdio, bash, dispatch_error=True, large_request=True
    )
    assert_dispatch_error(failed)
    if probes != 1 or dispatches != 1:
        raise AssertionError(
            "large request dispatch failure was not singular: "
            f"probes={probes} dispatches={dispatches}"
        )

    timed_out, probes, dispatches, _elapsed, diagnostics, _guard = run_case(
        stdio, bash, hang_prepare_response=True, request_parse_timeout=1
    )
    error = timed_out.get("error")
    data = error.get("data") if isinstance(error, dict) else None
    expected_timeout = {
        "phase": "stage",
        "dispatched": False,
        "replayed": False,
        "emacsclientRc": 124,
    }
    if (
        timed_out.get("id") != 17
        or not strict_equal(data, expected_timeout)
        or probes != 1
        or dispatches != 0
        or (
            "MCP-RUNNER-TIMEOUT: phase=execution "
            "operation=prepare-response"
        )
        not in diagnostics
    ):
        raise AssertionError(
            "pre-dispatch timeout lacked a phase-specific diagnostic: "
            f"response={timed_out!r} "
            f"probes={probes} dispatches={dispatches} "
            f"diagnostics={diagnostics!r}"
        )

    malformed, probes, dispatches, _elapsed, stderr, _guard = run_case(
        stdio, bash, malformed_output=True
    )
    assert_dispatch_error(malformed)
    if probes != 1 or dispatches != 1 or "substring expression" in stderr:
        raise AssertionError(
            "one-character dispatch output did not fail closed: "
            f"probes={probes} dispatches={dispatches} stderr={stderr!r}"
        )

    large_malformed, probes, dispatches, _elapsed, stderr, _guard = run_case(
        stdio, bash, malformed_output=True, large_request=True
    )
    assert_dispatch_error(large_malformed)
    if probes != 1 or dispatches != 1 or "substring expression" in stderr:
        raise AssertionError(
            "large malformed dispatch did not cleanly fail: "
            f"probes={probes} dispatches={dispatches} stderr={stderr!r}"
        )

    invalid, probes, dispatches, _elapsed, _stderr, _guard = run_case(
        stdio, bash, invalid_output=True
    )
    assert_readiness_error(invalid)
    if probes != 1 or dispatches != 0:
        raise AssertionError(
            f"invalid probe output was accepted: probes={probes} "
            f"dispatches={dispatches}"
        )

    assert_lifecycle_guard(stdio, bash, "init")
    assert_lifecycle_guard(stdio, bash, "stop")
    assert_lifecycle_guard(stdio, bash, "init", malformed=True)
    assert_lifecycle_guard(stdio, bash, "stop", malformed=True)
    assert_lifecycle_guard(stdio, bash, "init", split_sentinel=True)
    assert_lifecycle_guard(stdio, bash, "stop", split_sentinel=True)
    assert_drain_phase_budget(stdio, bash)
    assert_delayed_ack_preserves_runner(stdio, bash, expected)
    assert_saturated_heartbeat_preserves_ack(stdio, bash)
    assert_signal_publication(stdio, bash, delivered_ack=False)
    assert_signal_publication(stdio, bash, delivered_ack=True)
    assert_signal_cleanup(stdio, bash, repeat=False)
    assert_signal_cleanup(stdio, bash, repeat=True)
    assert_signal_cleanup(stdio, bash, repeat=False, forged_record=True)
    assert_signal_cleanup(stdio, bash, repeat=False, cleanup_failure=True)
    assert_signal_cleanup(
        stdio,
        bash,
        repeat=True,
        owner_death_after_hold=True,
    )
    assert_exit_without_staging(stdio, bash)
    assert_invalid_configuration(stdio, bash, "ANVIL_EMACSCLIENT_RETRY_MAX", "09")
    assert_invalid_configuration(
        stdio, bash, "ANVIL_EMACSCLIENT_READINESS_TIMEOUT", "09"
    )
    assert_invalid_configuration(stdio, bash, "ANVIL_EMACSCLIENT_RETRY_MAX", "9" * 200)
    assert_invalid_configuration(
        stdio,
        bash,
        "ANVIL_EMACSCLIENT_READINESS_TIMEOUT",
        "9" * 200,
    )
    assert_invalid_configuration(
        stdio,
        bash,
        "ANVIL_MCP_READINESS_MODE",
        "unsupported",
        expected_fragment="unsupported readiness mode",
    )
    assert_invalid_configuration(
        stdio,
        bash,
        "ANVIL_MCP_READINESS_MODE",
        "headless",
        expected_fragment="unsafe server id",
        server_id="unsafe id",
    )

    print(f"stdio-readiness-ok bash={bash}")
    return 0


def exit_on_termination(signum: int, _frame: object) -> None:
    """Raise through active fixture cleanup when an outer timeout terminates us."""
    signal.signal(signal.SIGTERM, signal.SIG_IGN)
    signal.signal(signal.SIGINT, signal.SIG_IGN)
    raise SystemExit(128 + signum)


if __name__ == "__main__":
    signal.signal(signal.SIGTERM, exit_on_termination)
    signal.signal(signal.SIGINT, exit_on_termination)
    raise SystemExit(main())
