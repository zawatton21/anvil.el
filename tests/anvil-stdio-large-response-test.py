#!/usr/bin/env python3
"""Real-daemon regression for large Anvil stdio responses."""

from __future__ import annotations

import hashlib
import json
import os
import select
import signal
import stat
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import BinaryIO

LARGE_RESULT_BYTES = 3_000_000


def diagnostics(
    message: str,
    bridge: subprocess.Popen[bytes] | None,
    daemon: subprocess.Popen[bytes] | None,
    bridge_log: Path,
    daemon_log: Path,
) -> AssertionError:
    """Return an assertion carrying bounded bridge and daemon diagnostics."""

    def tail(path: Path) -> str:
        if not path.is_file():
            return "<missing>"
        return path.read_text(encoding="utf-8", errors="replace")[-8000:]

    return AssertionError(
        f"{message}\n"
        f"bridge-rc={None if bridge is None else bridge.poll()}\n"
        f"daemon-rc={None if daemon is None else daemon.poll()}\n"
        f"--- bridge stderr ---\n{tail(bridge_log)}\n"
        f"--- daemon output ---\n{tail(daemon_log)}"
    )


def process_group_alive(group: int) -> bool:
    """Return whether GROUP still has any member."""
    try:
        os.killpg(group, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def terminate_group(process: subprocess.Popen[bytes] | None) -> None:
    """Converge PROCESS and its process group without trusting its leader."""
    if process is None:
        return
    group = process.pid
    for sig, timeout in ((signal.SIGTERM, 2.0), (signal.SIGKILL, 2.0)):
        if process_group_alive(group):
            try:
                os.killpg(group, sig)
            except ProcessLookupError:
                pass
            except PermissionError:
                if process.poll() is None:
                    try:
                        process.send_signal(sig)
                    except ProcessLookupError:
                        pass
        deadline = time.monotonic() + timeout
        while process_group_alive(group) and time.monotonic() < deadline:
            time.sleep(0.02)
        if not process_group_alive(group):
            if process.poll() is None:
                process.wait(timeout=timeout)
            return
    raise AssertionError(f"process group survived SIGKILL: {group}")


def wait_for_daemon(
    daemon: subprocess.Popen[bytes],
    emacsclient: str,
    server_name: str,
    environment: dict[str, str],
    daemon_log: Path,
) -> None:
    """Wait until the exact isolated daemon accepts an evaluation."""
    deadline = time.monotonic() + 20
    last_error = b""
    while time.monotonic() < deadline:
        if daemon.poll() is not None:
            raise diagnostics(
                "Emacs daemon exited during startup",
                None,
                daemon,
                Path("/nonexistent"),
                daemon_log,
            )
        try:
            result = subprocess.run(
                [
                    emacsclient,
                    "-a",
                    "false",
                    "-s",
                    server_name,
                    "-e",
                    "t",
                ],
                env=environment,
                stdin=subprocess.DEVNULL,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                timeout=2,
                check=False,
            )
        except subprocess.TimeoutExpired:
            time.sleep(0.05)
            continue
        last_error = result.stderr
        if result.returncode == 0 and result.stdout.strip() == b"t":
            return
        time.sleep(0.05)
    raise AssertionError(
        "isolated Emacs daemon did not become ready: "
        + last_error.decode("utf-8", errors="replace")
    )


def daemon_eval(
    emacsclient: str,
    server_name: str,
    environment: dict[str, str],
    expression: str,
) -> str:
    """Evaluate EXPRESSION in the exact fixture daemon."""
    result = subprocess.run(
        [
            emacsclient,
            "-a",
            "false",
            "-s",
            server_name,
            "-e",
            expression,
        ],
        env=environment,
        stdin=subprocess.DEVNULL,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        timeout=5,
        check=False,
    )
    if result.returncode != 0:
        raise AssertionError(
            "daemon counter evaluation failed: "
            + result.stderr.decode("utf-8", errors="replace")
        )
    return result.stdout.decode("utf-8", errors="strict").strip()


def send_frame(process: subprocess.Popen[bytes], document: dict[str, object]) -> None:
    """Write one Content-Length framed JSON document."""
    if process.stdin is None:
        raise AssertionError("bridge stdin is unavailable")
    body = json.dumps(
        document,
        ensure_ascii=False,
        separators=(",", ":"),
    ).encode("utf-8")
    process.stdin.write(f"Content-Length: {len(body)}\r\n\r\n".encode("ascii"))
    process.stdin.write(body)
    process.stdin.flush()


def read_some(
    stream: BinaryIO,
    process: subprocess.Popen[bytes],
    deadline: float,
) -> bytes:
    """Read available bridge bytes before DEADLINE."""
    remaining = deadline - time.monotonic()
    if remaining <= 0:
        raise TimeoutError("bridge response deadline expired")
    ready, _, _ = select.select([stream.fileno()], [], [], remaining)
    if not ready:
        raise TimeoutError("bridge response read timed out")
    chunk = os.read(stream.fileno(), 65536)
    if not chunk:
        raise EOFError(f"bridge closed response pipe with rc={process.poll()}")
    return chunk


def read_frame(
    process: subprocess.Popen[bytes],
    timeout: float = 45,
) -> tuple[dict[str, object], bytes]:
    """Read and parse one exact Content-Length response frame."""
    if process.stdout is None:
        raise AssertionError("bridge stdout is unavailable")
    deadline = time.monotonic() + timeout
    buffered = bytearray()
    separator = b"\r\n\r\n"
    while separator not in buffered:
        buffered.extend(read_some(process.stdout, process, deadline))
        if len(buffered) > 65536:
            raise AssertionError("response header exceeded 64 KiB")
    header, body_prefix = bytes(buffered).split(separator, 1)
    content_lengths = []
    for line in header.split(b"\r\n"):
        name, delimiter, value = line.partition(b":")
        if delimiter and name.lower() == b"content-length":
            content_lengths.append(value.strip())
    if len(content_lengths) != 1 or not content_lengths[0].isdigit():
        raise AssertionError(f"invalid response header: {header!r}")
    length = int(content_lengths[0])
    body = bytearray(body_prefix)
    while len(body) < length:
        body.extend(read_some(process.stdout, process, deadline))
    if len(body) != length:
        raise AssertionError("bridge over-read a pipelined response")
    raw = bytes(body)
    parsed = json.loads(raw)
    if not isinstance(parsed, dict):
        raise AssertionError("response is not a JSON object")
    return parsed, raw


def assert_response_custody(
    temp_root: Path,
    sequence: int,
    expected_body: bytes,
) -> None:
    """Assert the exact generation remains privately owned until cleanup."""
    directories = list(temp_root.glob("anvil-mcp.*"))
    if len(directories) != 1:
        raise AssertionError(f"expected one transaction directory: {directories}")
    directory = directories[0]
    directory_stat = directory.stat()
    if (
        not stat.S_ISDIR(directory_stat.st_mode)
        or stat.S_IMODE(directory_stat.st_mode) != 0o700
        or directory_stat.st_uid != os.geteuid()
    ):
        raise AssertionError("unsafe response transaction directory")
    response = directory / f"response.{sequence}.json"
    proof = directory / f"proof.{sequence}.json"
    names = sorted(path.name for path in directory.iterdir())
    expected_names = sorted((response.name, proof.name))
    if names != expected_names:
        raise AssertionError(f"unexpected generation custody: {names}")
    response_stat = response.lstat()
    proof_stat = proof.lstat()
    for path, info in ((response, response_stat), (proof, proof_stat)):
        if (
            not stat.S_ISREG(info.st_mode)
            or path.is_symlink()
            or stat.S_IMODE(info.st_mode) != 0o600
            or info.st_uid != os.geteuid()
            or info.st_nlink != 2
            or path.read_bytes() != expected_body
        ):
            raise AssertionError(f"unsafe or inexact staged response: {path}")
    if (
        response_stat.st_dev != proof_stat.st_dev
        or response_stat.st_ino != proof_stat.st_ino
    ):
        raise AssertionError("response and proof are not one authenticated inode")


def write_daemon_init(path: Path, source_root: Path) -> None:
    """Install a deterministic two-method JSON-RPC responder."""
    root_literal = json.dumps(str(source_root))
    path.write_text(
        f""";;; isolated Anvil large-response fixture -*- lexical-binding: t; -*-
(add-to-list 'load-path {root_literal})
(require 'anvil-server)

(defconst anvil-stdio-large-response-test--prefix "雪🙂")
(defvar anvil-stdio-large-response-test--handler-count 0)
(defvar anvil-stdio-large-response-test--publication-count 0)
(defvar anvil-stdio-large-response-test--marker-loss-count 0)

(defun anvil-stdio-large-response-test--dispatch (json-string _server-id)
  (setq anvil-stdio-large-response-test--handler-count
        (1+ anvil-stdio-large-response-test--handler-count))
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (request (json-read-from-string json-string))
         (identifier (alist-get 'id request))
         (method (alist-get 'method request))
         (result
          (cond
           ((equal method "large")
            (concat
             anvil-stdio-large-response-test--prefix
             (make-string
              (- {LARGE_RESULT_BYTES}
                 (string-bytes
                  anvil-stdio-large-response-test--prefix))
              ?z)))
           ((equal method "small") "recovery-ok")
           (t (error "Unexpected large-response test method: %S" method)))))
    (json-encode
     `((jsonrpc . "2.0") (id . ,identifier) (result . ,result)))))

(fset 'anvil-server-process-jsonrpc
      #'anvil-stdio-large-response-test--dispatch)

(defun anvil-stdio-large-response-test--lose-first-marker
    (original json-string &rest arguments)
  (let ((marker (apply original json-string arguments)))
    (setq anvil-stdio-large-response-test--publication-count
          (1+ anvil-stdio-large-response-test--publication-count))
    (if (= anvil-stdio-large-response-test--publication-count 1)
        (progn
          (setq anvil-stdio-large-response-test--marker-loss-count
                (1+ anvil-stdio-large-response-test--marker-loss-count))
          (error "Intentional response marker transport loss"))
      marker)))

(advice-add
 'anvil-server-process-jsonrpc-to-file
 :around
 #'anvil-stdio-large-response-test--lose-first-marker)
""",
        encoding="utf-8",
    )


def run(stdio: Path, bash: str, emacs: str, emacsclient: str) -> None:
    """Exercise a real daemon, real emacsclient, and one persistent bridge."""
    source_root = stdio.parent.resolve()
    with tempfile.TemporaryDirectory(prefix="anvil-e2e-", dir="/tmp") as raw:
        root = Path(raw)
        home = root / "home"
        temp_root = root / "tmp"
        runtime = root / "runtime"
        for directory in (home, temp_root, runtime):
            directory.mkdir(mode=0o700)
        daemon_init = root / "daemon-init.el"
        daemon_log = root / "daemon.log"
        bridge_log = root / "bridge.log"
        debug_log = root / "bridge-debug.log"
        write_daemon_init(daemon_init, source_root)
        server_name = f"ae{os.getpid():x}{time.monotonic_ns() & 0xFFFF:x}"
        environment = os.environ.copy()
        for name in list(environment):
            if name.startswith(("ANVIL_", "EMACS_MCP_")):
                environment.pop(name)
        environment.update(
            {
                "HOME": str(home),
                "TMPDIR": str(temp_root),
                "XDG_CONFIG_HOME": str(home / ".config"),
                "XDG_CACHE_HOME": str(home / ".cache"),
                "XDG_RUNTIME_DIR": str(runtime),
            }
        )
        environment.pop("ALTERNATE_EDITOR", None)
        environment.pop("EMACSLOADPATH", None)
        environment["PATH"] = os.pathsep.join(
            (
                str(Path(emacsclient).parent),
                str(Path(emacs).parent),
                environment.get("PATH", ""),
            )
        )
        daemon: subprocess.Popen[bytes] | None = None
        bridge: subprocess.Popen[bytes] | None = None
        daemon_output = daemon_log.open("wb")
        bridge_output = bridge_log.open("wb")
        clean = False
        try:
            daemon = subprocess.Popen(
                [
                    emacs,
                    "--quick",
                    f"--fg-daemon={server_name}",
                    "--load",
                    str(daemon_init),
                ],
                env=environment,
                stdin=subprocess.DEVNULL,
                stdout=daemon_output,
                stderr=subprocess.STDOUT,
                start_new_session=True,
            )
            wait_for_daemon(
                daemon,
                emacsclient,
                server_name,
                environment,
                daemon_log,
            )
            bridge_environment = environment.copy()
            bridge_environment.update(
                {
                    "EMACS_MCP_DEBUG_LOG": str(debug_log),
                    "ANVIL_EMACSCLIENT_READINESS_TIMEOUT": "10",
                    "ANVIL_EMACSCLIENT_PROBE_TIMEOUT": "5",
                    "ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT": "60",
                    "ANVIL_MCP_FRAME_READ_TIMEOUT": "10",
                }
            )
            bridge = subprocess.Popen(
                [
                    bash,
                    str(stdio),
                    f"--socket={server_name}",
                    "--server-id=large-response-test",
                ],
                env=bridge_environment,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=bridge_output,
                bufsize=0,
                start_new_session=True,
            )
            if bridge.stdin is None or bridge.stdout is None:
                raise AssertionError("bridge pipes are unavailable")
            original_pid = bridge.pid
            pipe_ids = (
                os.fstat(bridge.stdin.fileno()).st_ino,
                os.fstat(bridge.stdout.fileno()).st_ino,
            )

            send_frame(
                bridge,
                {"jsonrpc": "2.0", "id": "large-real", "method": "large"},
            )
            large_response, large_body = read_frame(bridge)
            prefix = "雪🙂"
            large_result = prefix + (
                "z" * (LARGE_RESULT_BYTES - len(prefix.encode("utf-8")))
            )
            expected_large = {
                "jsonrpc": "2.0",
                "id": "large-real",
                "result": large_result,
            }
            expected_large_body = json.dumps(
                expected_large,
                ensure_ascii=False,
                separators=(",", ":"),
            ).encode("utf-8")
            if (
                large_response != expected_large
                or len(large_result.encode("utf-8")) != LARGE_RESULT_BYTES
                or hashlib.sha256(large_body).digest()
                != hashlib.sha256(expected_large_body).digest()
                or large_body != expected_large_body
            ):
                raise AssertionError("3 MB multibyte response was not exact")
            if daemon_eval(
                emacsclient,
                server_name,
                environment,
                "(list anvil-stdio-large-response-test--handler-count "
                "anvil-stdio-large-response-test--publication-count "
                "anvil-stdio-large-response-test--marker-loss-count)",
            ) != "(1 1 1)":
                raise AssertionError("large request was not dispatched exactly once")
            assert_response_custody(temp_root, 1, large_body)

            send_frame(
                bridge,
                {"jsonrpc": "2.0", "id": "small-real", "method": "small"},
            )
            small_response, small_body = read_frame(bridge)
            if small_response != {
                "jsonrpc": "2.0",
                "id": "small-real",
                "result": "recovery-ok",
            }:
                raise AssertionError(f"same-bridge recovery failed: {small_response!r}")
            if bridge.pid != original_pid or bridge.poll() is not None:
                raise AssertionError("bridge identity changed after the large response")
            if (
                os.fstat(bridge.stdin.fileno()).st_ino,
                os.fstat(bridge.stdout.fileno()).st_ino,
            ) != pipe_ids:
                raise AssertionError("bridge pipes changed after the large response")
            if daemon_eval(
                emacsclient,
                server_name,
                environment,
                "(list anvil-stdio-large-response-test--handler-count "
                "anvil-stdio-large-response-test--publication-count "
                "anvil-stdio-large-response-test--marker-loss-count)",
            ) != "(2 2 1)":
                raise AssertionError("same-pipe request count was not exact")
            assert_response_custody(temp_root, 2, small_body)

            bridge.stdin.close()
            bridge.wait(timeout=10)
            if bridge.returncode != 0:
                raise diagnostics(
                    "bridge failed during EOF cleanup",
                    bridge,
                    daemon,
                    bridge_log,
                    daemon_log,
                )
            if list(temp_root.glob("anvil-mcp.*")):
                raise AssertionError("response transaction survived bridge EOF")
            if process_group_alive(original_pid):
                raise AssertionError("bridge process group survived stdin EOF")
            clean = True
        except BaseException as error:
            if isinstance(error, AssertionError):
                raise
            raise diagnostics(
                f"large-response regression raised {error!r}",
                bridge,
                daemon,
                bridge_log,
                daemon_log,
            ) from error
        finally:
            terminate_group(bridge)
            if daemon is not None and daemon.poll() is None:
                subprocess.run(
                    [
                        emacsclient,
                        "-a",
                        "false",
                        "-s",
                        server_name,
                        "-e",
                        "(kill-emacs 0)",
                    ],
                    env=environment,
                    stdin=subprocess.DEVNULL,
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL,
                    timeout=5,
                    check=False,
                )
            terminate_group(daemon)
            daemon_output.close()
            bridge_output.close()
        if not clean:
            raise AssertionError("large-response regression did not converge")


def main() -> int:
    """Run the regression with the selected bridge and Bash."""
    if len(sys.argv) != 5:
        raise SystemExit(
            f"usage: {Path(sys.argv[0]).name} "
            "ANVIL_STDIO BASH EMACS EMACSCLIENT"
        )
    stdio = Path(sys.argv[1]).resolve()
    bash = str(Path(sys.argv[2]).resolve())
    emacs = str(Path(sys.argv[3]).resolve())
    emacsclient = str(Path(sys.argv[4]).resolve())
    for program in (stdio, Path(bash), Path(emacs), Path(emacsclient)):
        if not program.is_file():
            raise SystemExit(f"not a file: {program}")
    run(stdio, bash, emacs, emacsclient)
    print(f"stdio-large-response-ok bash={bash}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
