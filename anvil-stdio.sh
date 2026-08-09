#!/usr/bin/env bash
# anvil-stdio.sh - Connect to Anvil MCP server via stdio transport
#
# Copyright (C) 2025 Laurynas Biveinis
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

set -eu -o pipefail

# A transport failure must remain a failure.  In particular, never let
# emacsclient reinterpret an Elisp expression as a file for another editor.
unset ALTERNATE_EDITOR

# Default values
INIT_FUNCTION=""
STOP_FUNCTION=""
SOCKET=""
SERVER_ID=""
EMACS_MCP_DEBUG_LOG=${EMACS_MCP_DEBUG_LOG:-""}
ANVIL_MCP_PARENT_GUARD=${ANVIL_MCP_PARENT_GUARD:-""}
ANVIL_MCP_PARENT_GUARD_PYTHON=${ANVIL_MCP_PARENT_GUARD_PYTHON:-""}
ANVIL_MCP_READINESS_MODE=${ANVIL_MCP_READINESS_MODE:-emacs}

# Keep a restrictive mask for a caller-selected debug log.  Stateful stderr is
# discarded through a sink opened once at startup; creating and later cleaning
# one pathname per request would reintroduce loader-dependent helpers.
umask 077
if ! exec 8>/dev/null; then
	echo "anvil-mcp: cannot open dispatch stderr sink" >&2
	exit 70
fi
ANVIL_MCP_RESPONSE_PENDING=0

# Bash 4.2+ can format wall-clock time without execing `date'.  Older Bash
# releases (notably macOS Bash 3.2) retain a process-free relative timestamp.
_anvil_printf_time=0
if printf -v _anvil_timestamp_probe '%(%Y)T' -1 2>/dev/null; then
	_anvil_printf_time=1
fi
unset _anvil_timestamp_probe

# Debug logging setup.  Open one regular, non-symlink destination before any
# request.  During a stateful dispatch logging is suppressed completely, so
# diagnostics can never delay delivery of an already-executed response.
if [ -n "$EMACS_MCP_DEBUG_LOG" ]; then
	if [ -L "$EMACS_MCP_DEBUG_LOG" ] \
		|| { [ -e "$EMACS_MCP_DEBUG_LOG" ] && [ ! -f "$EMACS_MCP_DEBUG_LOG" ]; }; then
		echo "anvil-mcp: debug log must be a regular non-symlink file" >&2
		exit 64
	fi
	if ! : >>"$EMACS_MCP_DEBUG_LOG" || [ ! -f "$EMACS_MCP_DEBUG_LOG" ]; then
		echo "anvil-mcp: cannot create debug log: $EMACS_MCP_DEBUG_LOG" >&2
		exit 70
	fi
	if ! exec 9>>"$EMACS_MCP_DEBUG_LOG"; then
		echo "anvil-mcp: cannot open debug log: $EMACS_MCP_DEBUG_LOG" >&2
		exit 70
	fi

	mcp_debug_log() {
		local direction="$1" message="$2" timestamp
		[ "$ANVIL_MCP_RESPONSE_PENDING" -eq 0 ] || return 0
		message="${message:0:2048}"
		if [ "$_anvil_printf_time" -eq 1 ]; then
			printf -v timestamp '%(%Y-%m-%d %H:%M:%S)T' -1
		else
			printf -v timestamp '+%ss' "$SECONDS"
		fi
		printf '[%s] [%s] MCP-%s: %s\n' \
			"$timestamp" "$$" "$direction" "$message" >&9 2>/dev/null || true
	}

	mcp_debug_log "INFO" "Debug logging enabled"

	# Log the path and resolved steady-state tools without starting diagnostic
	# helpers.  A debug-only git/stat/dirname freeze must not block MCP startup.
	_anvil_script_path="$0"
	mcp_debug_log "INFO" "anvil-stdio.sh path=$_anvil_script_path"
	mcp_debug_log "INFO" \
		"tooling bash=$(command -v bash || printf '?') python3=$(command -v python3 || printf '?') emacsclient=$(command -v emacsclient || printf '?')"
else
	mcp_debug_log() {
		:
	}
fi

# Probe diagnostics are captured before dispatch and may therefore share the
# readiness command's combined output.  No capture pathname is needed.
anvil_mcp_log_probe_stderr() {
	local direction="$1" text="$2"
	[ -n "$text" ] || return 0
	text="${text//$'\n'/ }"
	mcp_debug_log "$direction" "${text:0:4096}"
}

# FD 8 is a process-lifetime stderr sink opened before any stateful dispatch.
# These hooks remain explicit so capture setup failure is still distinguishable
# from an ambiguous post-dispatch failure, without per-request files or reads.
anvil_mcp_capture_begin() {
	: >&8
}

anvil_mcp_capture_finish() {
	:
}

# Run one text-only external program under a Bash-owned deadline.  GNU
# `timeout' cannot bound its own dynamic-loader startup; process substitution
# plus builtin `read -t' can.  The runner discovers its real PID through a
# pre-dispatch Python child because Bash 3.2 reports stale $$/$PPID values in
# subshells.  A delayed guard loader must validate that numeric owner instead
# of adopting a reparent/subreaper.  NUL is forbidden in helper output.
anvil_mcp_exec_program() {
	if [ -n "$ANVIL_MCP_PARENT_GUARD" ]; then
		ANVIL_HEADLESS_BRIDGE_PID="$bridge_pid" \
			ANVIL_HEADLESS_PARENT_PID="$ANVIL_MCP_RUNNER_PID" exec \
			"$ANVIL_MCP_PARENT_GUARD_PYTHON" -I -S \
			"$ANVIL_MCP_PARENT_GUARD" group "$@"
	else
		exec "$@"
	fi
}

anvil_mcp_exec_child() {
	local stderr_mode="$1" input_mode="$2" input="$3"
	shift 3
	exec 6<&- 7<&- 9>&-
	case "$input_mode:$stderr_mode" in
	input:merge)
		anvil_mcp_exec_program "$@" < <(printf '%s' "$input") 2>&1 8>&-
		;;
	input:separate)
		anvil_mcp_exec_program "$@" < <(printf '%s' "$input") 2>&8 8>&-
		;;
	descriptor:merge)
		anvil_mcp_exec_program "$@" <&5 5<&- 2>&1 8>&-
		;;
	descriptor:separate)
		anvil_mcp_exec_program "$@" <&5 5<&- 2>&8 8>&-
		;;
	inherit:merge)
		anvil_mcp_exec_program "$@" 2>&1 8>&-
		;;
	inherit:separate)
		anvil_mcp_exec_program "$@" 2>&8 8>&-
		;;
	null:merge)
		anvil_mcp_exec_program "$@" </dev/null 2>&1 8>&-
		;;
	null:separate)
		anvil_mcp_exec_program "$@" </dev/null 2>&8 8>&-
		;;
	*)
		printf 'anvil-mcp: invalid bounded-run mode: %s/%s' \
			"$input_mode" "$stderr_mode"
		return 64
		;;
	esac
}

anvil_mcp_job_is_running() {
	local target="$1" job_pid
	while IFS= read -r job_pid; do
		[ "$job_pid" = "$target" ] && return 0
	done < <(jobs -pr)
	return 1
}

anvil_mcp_finish_runner() {
	local status="$1" before
	runner_published=1
	while :; do
		before=$signal_epoch
		if printf '\0%s\n' "$status"; then
			break
		fi
		[ "$signal_epoch" -ne "$before" ] || return 0
	done
	# Keep this exact process-substitution runner alive until the parent closes
	# descriptor 7 or performs explicit convergence.  During retirement, close
	# stdout only after exact child wait/reap and group cleanup; kernel EOF then
	# authenticates that no child or runner writer remains.  Hold the original
	# PID until the parent performs its final KILL, so PID reuse is impossible.
	while :; do
		if [ "$retirement_requested" -ne 0 ]; then
			exec 1>&-
			# The parent normally KILLs this exact held PID immediately.  If the
			# bridge itself dies first, leave the hold without waiting forever.
			while kill -0 "$bridge_pid" 2>/dev/null; do :; done
			return 0
		fi
		before=$signal_epoch
		if printf '\0%65536s' '' 2>/dev/null; then
			continue
		fi
		[ "$signal_epoch" -ne "$before" ] || break
	done
}

anvil_mcp_run_child() {
	local bridge_pid="$1" guard_deadline="$2" stderr_mode="$3" input_mode="$4" input="$5"
	local child="" rc=70 runner_pid="" timed_out=0 term_sent=0 kill_sent=0 before=0
	local signal_epoch=0 runner_acked=0 runner_ack_published=0 runner_published=0
	local retirement_requested=0
	shift 5

	# A guarded launch keeps its Python loader in the bridge group until the
	# loaded guard validates the exact runner and moves the target.  Timeout
	# signals cover the pre-group PID and post-group PGID.
	trap '
		retirement_requested=1
		signal_epoch=$((signal_epoch + 1))
		runner_acked=1
		if [ "$term_sent" -eq 0 ]; then
			term_sent=1
			timed_out=1
			if [ -n "$child" ]; then
				kill -TERM -- "-$child" 2>/dev/null \
					|| kill -TERM "$child" 2>/dev/null || :
			fi
		fi
	' TERM
	trap '
		retirement_requested=1
		signal_epoch=$((signal_epoch + 1))
		runner_acked=1
		if [ "$kill_sent" -eq 0 ]; then
			kill_sent=1
			timed_out=1
			if [ -n "$child" ]; then
				kill -KILL -- "-$child" 2>/dev/null \
					|| kill -KILL "$child" 2>/dev/null || :
			fi
		fi
	' USR1
	trap '
		signal_epoch=$((signal_epoch + 1))
		runner_acked=1
	' USR2
	trap '
		if [ "$runner_ack_published" -eq 1 ] \
			&& [ "$runner_published" -eq 0 ]; then
			if [ -n "$child" ]; then
				kill -KILL -- "-$child" 2>/dev/null \
					|| kill -KILL "$child" 2>/dev/null || :
				wait "$child" 2>/dev/null || :
				child=
			fi
			anvil_mcp_finish_runner 70
		fi
	' EXIT
	# READY alone never authorizes the requested command.  Hold this exact writer
	# until the parent publishes its identity and acknowledges it with USR2.  Each
	# 256-byte heartbeat is below POSIX's minimum PIPE_BUF, so it either completes
	# atomically or blocks without a partial chunk.  Heartbeats have no newline:
	# after ACK appends the sole line terminator, the parent drains the finite pipe
	# backlog with one bounded read.  Closing FD7 produces EPIPE without launching.
	if ! printf '%s\n' "ANVIL-MCP-RUNNER-READY"; then
		trap - EXIT TERM USR1 USR2
		return
	fi
	while [ "$runner_acked" -eq 0 ] \
		&& kill -0 "$bridge_pid" 2>/dev/null; do
		before=$signal_epoch
		if ! printf 'ANVIL-MCP-RUNNER-WAIT:%0234d' 0 2>/dev/null; then
			if [ "$signal_epoch" -eq "$before" ]; then
				trap - EXIT TERM USR1 USR2
				return
			fi
			break
		fi
	done
	[ "$runner_acked" -ne 0 ] || {
		trap - EXIT TERM USR1 USR2
		return
	}
	while :; do
		before=$signal_epoch
		if printf '%s\n' "ANVIL-MCP-RUNNER-ACK"; then
			runner_ack_published=1
			break
		fi
		if [ "$signal_epoch" -eq "$before" ]; then
			trap - EXIT TERM USR1 USR2
			return
		fi
	done
	if [ "$timed_out" -ne 0 ]; then
		anvil_mcp_finish_runner 124
		trap - EXIT TERM USR1 USR2
		return
	fi

	if [ -n "$ANVIL_MCP_PARENT_GUARD" ]; then
		[ "$guard_deadline" -le 5 ] || guard_deadline=5
		exec 4< <(exec "$ANVIL_MCP_PARENT_GUARD_PYTHON" -I -S -c \
			'import os; print(os.getppid())' 2>/dev/null)
		child=$!
		if ! IFS= read -r -t "$guard_deadline" runner_pid <&4; then
			exec 4<&-
			kill -KILL "$child" 2>/dev/null || :
			child=
			anvil_mcp_finish_runner 124
			trap - EXIT TERM USR1 USR2
			return
		fi
		exec 4<&-
		case "$runner_pid" in
		""|*[!0-9]*|0|1)
			kill -KILL "$child" 2>/dev/null || :
			child=
			anvil_mcp_finish_runner 70
			trap - EXIT TERM USR1 USR2
			return
			;;
		esac
		ANVIL_MCP_RUNNER_PID=$runner_pid
		child=""
	else
		set -m
	fi

	anvil_mcp_exec_child "$stderr_mode" "$input_mode" "$input" "$@" &
	child=$!
	# Signals may arrive after ACK but before the exact child PID is assigned.
	# Reconcile the strongest recorded intent before entering wait so USR1 can
	# never be downgraded to TERM for a newly published process group.
	if [ "$kill_sent" -ne 0 ]; then
		kill -KILL -- "-$child" 2>/dev/null \
			|| kill -KILL "$child" 2>/dev/null || :
	elif [ "$term_sent" -ne 0 ]; then
		kill -TERM -- "-$child" 2>/dev/null \
			|| kill -TERM "$child" 2>/dev/null || :
	fi
	while :; do
		if wait "$child"; then
			rc=0
			break
		else
			rc=$?
		fi
		# wait can return early when a runner signal trap fires.  Repeat
		# only while Bash still owns the exact job; kill -0 is insufficient
		# because the reaped PID may already identify an unrelated process.
		anvil_mcp_job_is_running "$child" || break
	done
	# Clear trap-visible custody only after wait has reaped the exact child.
	# Retain its group identity long enough to kill daemonized descendants.
	local child_group="$child"
	child=
	# A leader that daemonized must not leave same-group descendants holding
	# the result pipe open after the direct child exits.
	kill -KILL -- "-$child_group" 2>/dev/null || :
	[ "$timed_out" -eq 0 ] || rc=124
	anvil_mcp_finish_runner "$rc"
	trap - EXIT TERM USR1 USR2
}

# Drain one bounded retirement phase.  Bash 3.2 reports both EOF and timeout
# as read rc=1, so callers never infer meaning from that status: authentic EOF
# merely makes this loop return immediately, while timeout advances escalation.
anvil_mcp_drain_runner_output() {
	local grace="${ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT:-1}"
	local started_seconds=$SECONDS elapsed=0 remaining="$grace" ignored=
	while [ "$remaining" -gt 0 ]; do
		ignored=
		IFS= read -r -d '' -n 65536 -t "$remaining" ignored <&7 || break
		elapsed=$((SECONDS - started_seconds))
		remaining=$((grace - elapsed))
		# SECONDS is whole-second on Bash 3.2.  Retain one additional read
		# interval after a tick; the two integer endpoints conservatively permit
		# less than two extra wall seconds for this drain phase.
		[ "$elapsed" -eq 0 ] || remaining=$((remaining + 1))
	done
}

# Converge the exact bounded runner while descriptor 7 remains open.  Never
# trust child-controlled output as a retirement marker: TERM requests orderly
# child/group retirement, USR1 forces it, and only then is the held PID killed.
anvil_mcp_converge_runner() {
	local runner="$1"
	case "$runner" in
	""|*[!0-9]*) return 0 ;;
	esac
	kill -TERM "$runner" 2>/dev/null || :
	anvil_mcp_drain_runner_output
	kill -USR1 "$runner" 2>/dev/null || :
	anvil_mcp_drain_runner_output
	kill -KILL "$runner" 2>/dev/null || :
}

# Results are returned in ANVIL_MCP_RUN_OUTPUT / STATUS.  A missing sentinel
# or malformed private status is always failure; partial output is ignored and
# the still-published runner is converged before the function returns.
anvil_mcp_run_bounded() {
	local deadline="$1" stderr_mode="$2" input_mode="$3" input="$4"
	local operation="$5"
	local runner status="" runner_ready=""
	local runner_ack="" ack_error=0 ack_attempted=0 ack_sent=0
	local runner_owned=0
	local control_deadline=$ANVIL_MCP_RUNNER_CONTROL_TIMEOUT
	local started_seconds=$SECONDS elapsed remaining
	shift 5
	[ "$control_deadline" -le "$deadline" ] || control_deadline=$deadline
	case "$operation" in
	emacsclient|probe-delay|dispatch|frame-line|frame-body|request-metadata|prepare-response|validate-response-fd|stage-request|cleanup-staged)
		;;
	*)
		operation=unknown
		;;
	esac
	ANVIL_MCP_RUN_OUTPUT=""
	ANVIL_MCP_RUN_STATUS=70
	anvil_mcp_finish_pending_termination

	ANVIL_MCP_RUNNER_STARTING=1
	exec 7< <(anvil_mcp_run_child \
		"$$" "$deadline" "$stderr_mode" "$input_mode" "$input" "$@")
	runner=$!
	anvil_mcp_finish_bounded_termination "$runner" 0
	if ! IFS= read -r -t "$control_deadline" runner_ready <&7 \
		|| [ "$runner_ready" != "ANVIL-MCP-RUNNER-READY" ]; then
		mcp_debug_log "RUNNER-TIMEOUT" \
			"phase=ready operation=$operation"
		anvil_mcp_finish_bounded_termination "$runner" 0
		ANVIL_MCP_RUN_STATUS=124
		ANVIL_MCP_RUNNER_STARTING=0
		ANVIL_MCP_ACTIVE_RUNNER=
		# Before a validated READY, numeric runner custody is not published.  The
		# heartbeat observes this close and retires without risking a reused PID.
		exec 7<&-
		anvil_mcp_finish_pending_termination
		return 0
	fi

	# Publish after READY installed every custody trap, but acknowledge only
	# after the exact numeric identity is visible to bridge signal handlers.  A
	# signal at the read/publication boundary either closes FD7 before this
	# critical section or leaves it open for the owning checkpoint below.
	ANVIL_MCP_RUNNER_CRITICAL=1
	if [ -n "$ANVIL_MCP_PENDING_TERMINATION_STATUS" ] \
		|| [ "$ANVIL_MCP_RUNNER_STARTING" -eq 0 ]; then
		ANVIL_MCP_RUNNER_CRITICAL=0
		exec 7<&-
		anvil_mcp_finish_pending_termination
		return 0
	fi
	ANVIL_MCP_ACTIVE_RUNNER=$runner
	ANVIL_MCP_RUNNER_STARTING=0
	runner_owned=1
	ANVIL_MCP_RUNNER_CRITICAL=0
	anvil_mcp_finish_bounded_termination "$runner" 0 "$runner_owned"
	# Bash 3.2 exposes only whole-second SECONDS.  Once it advances, retain one
	# additional read interval instead of timing out immediately after a wall
	# boundary; the two integer endpoints conservatively permit less than two
	# extra wall seconds across READY and ACK.
	elapsed=$((SECONDS - started_seconds))
	remaining=$((control_deadline - elapsed))
	[ "$elapsed" -eq 0 ] || remaining=$((remaining + 1))
	if [ "$remaining" -le 0 ]; then
		ack_error=124
		mcp_debug_log "RUNNER-TIMEOUT" \
			"phase=ack operation=$operation step=budget"
	else
		anvil_mcp_finish_bounded_termination "$runner" 0 "$runner_owned"
		ANVIL_MCP_RUNNER_CRITICAL=1
		if [ -z "$ANVIL_MCP_PENDING_TERMINATION_STATUS" ] \
			&& [ "$ANVIL_MCP_ACTIVE_RUNNER" = "$runner" ]; then
			# Once the USR2 syscall may run, launch is conservatively possible
			# even if a bridge signal makes the compound command report failure.
			ack_attempted=1
			if kill -USR2 "$ANVIL_MCP_ACTIVE_RUNNER" 2>/dev/null; then
				ack_sent=1
			else
				ack_error=70
			fi
		else
			ack_error=70
		fi
		ANVIL_MCP_RUNNER_CRITICAL=0
		anvil_mcp_finish_bounded_termination \
			"$runner" "$ack_attempted" "$runner_owned"
		if [ "$ack_sent" -eq 1 ]; then
			anvil_mcp_finish_bounded_termination \
				"$runner" 1 "$runner_owned"
			elapsed=$((SECONDS - started_seconds))
			remaining=$((control_deadline - elapsed))
			[ "$elapsed" -eq 0 ] || remaining=$((remaining + 1))
			runner_ack=
			if [ "$remaining" -le 0 ] \
				|| ! IFS= read -r -t "$remaining" runner_ack <&7; then
				ack_error=124
				mcp_debug_log "RUNNER-TIMEOUT" \
					"phase=ack operation=$operation step=record"
			elif [[ "$runner_ack" != *ANVIL-MCP-RUNNER-ACK ]]; then
				ack_error=70
			fi
			anvil_mcp_finish_bounded_termination \
				"$runner" 1 "$runner_owned"
		fi
	fi
	anvil_mcp_finish_bounded_termination \
		"$runner" "$ack_attempted" "$runner_owned"
	runner_ack=
	if [ "$ack_error" -ne 0 ]; then
		ANVIL_MCP_RUN_STATUS=$ack_error
		# An attempted ACK may have crossed the requested-command boundary; converge
		# the still-held exact runner before withdrawing its public identity.
		ANVIL_MCP_ACTIVE_RUNNER=
		[ "$runner" = "$ANVIL_MCP_RETIRED_RUNNER" ] \
			|| anvil_mcp_converge_runner "$runner"
		exec 7<&-
		anvil_mcp_finish_pending_termination
		return 0
	fi

	# ACK authorizes the requested command.  Give it its full
	# execution deadline instead of charging READY/ACK setup against it.
	anvil_mcp_finish_bounded_termination "$runner" 1 "$runner_owned"
	remaining=$deadline
	if [ "$remaining" -gt 0 ] \
		&& IFS= read -r -d '' -t "$remaining" ANVIL_MCP_RUN_OUTPUT <&7; then
		anvil_mcp_finish_bounded_termination "$runner" 1 "$runner_owned"
		if IFS= read -r -t "$ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT" status <&7 \
			&& [[ "$status" =~ ^[0-9]+$ ]] \
			&& [ "$status" -le 255 ]; then
			ANVIL_MCP_RUN_STATUS=$status
		else
			ANVIL_MCP_RUN_OUTPUT=""
			ANVIL_MCP_RUN_STATUS=70
		fi
	else
		ANVIL_MCP_RUN_OUTPUT=""
		ANVIL_MCP_RUN_STATUS=124
		mcp_debug_log "RUNNER-TIMEOUT" \
			"phase=execution operation=$operation"
	fi
	anvil_mcp_finish_bounded_termination "$runner" 1 "$runner_owned"
	# Retire synchronously before PID reuse.  Child bytes—including a forged
	# NUL/status record—can never suppress the unconditional escalation.
	ANVIL_MCP_ACTIVE_RUNNER=
	[ "$runner" = "$ANVIL_MCP_RETIRED_RUNNER" ] \
		|| anvil_mcp_converge_runner "$runner"
	# Clear the public numeric identity before closing its reader so no later
	# bridge signal can target the retired PID.
	exec 7<&-
	anvil_mcp_finish_pending_termination
}

# A signal handler must never spawn a new runner: Bash 3.2 contaminates
# process substitutions created while a fatal-signal trap is still active.
# Record the first requested status and finish from the next normal-control
# checkpoint, where bounded staging cleanup is safe again.
ANVIL_MCP_ACTIVE_RUNNER=
ANVIL_MCP_RUNNER_STARTING=0
ANVIL_MCP_PENDING_TERMINATION_STATUS=
ANVIL_MCP_RETIRED_RUNNER=
ANVIL_MCP_RUNNER_CRITICAL=0
ANVIL_MCP_FINALIZING=0
ANVIL_MCP_CLEANUP_READY=0
ANVIL_MCP_CLEANUP_ACTIVE=0
ANVIL_MCP_CLEANUP_DONE=0
anvil_mcp_terminate() {
	local runner="${ANVIL_MCP_ACTIVE_RUNNER:-}"
	[ "$ANVIL_MCP_FINALIZING" -eq 0 ] || return 0
	[ -n "$ANVIL_MCP_PENDING_TERMINATION_STATUS" ] \
		|| ANVIL_MCP_PENDING_TERMINATION_STATUS=$1
	trap ':' HUP INT TERM
	[ "$ANVIL_MCP_CLEANUP_ACTIVE" -eq 0 ] || return 0
	exec 0<&- 2>/dev/null || :
	if [ "$ANVIL_MCP_RUNNER_CRITICAL" -eq 1 ]; then
		# Withdraw publication, but let the owning normal-control transaction
		# decide whether USR2 crossed the launch boundary before convergence.
		ANVIL_MCP_ACTIVE_RUNNER=
		ANVIL_MCP_RUNNER_STARTING=0
		return 0
	fi
	case "$runner" in
	""|*[!0-9]*)
		if [ "$ANVIL_MCP_RUNNER_STARTING" -eq 1 ]; then
			ANVIL_MCP_RUNNER_STARTING=0
			exec 7<&- 2>/dev/null || :
		fi
		;;
	*)
		# Withdraw the identity before callbacks or reads can reenter.  This
		# handler may retire an existing runner, but never launches cleanup.
		ANVIL_MCP_ACTIVE_RUNNER=
		ANVIL_MCP_RUNNER_STARTING=0
		ANVIL_MCP_RETIRED_RUNNER=$runner
		anvil_mcp_converge_runner "$runner"
		exec 7<&- 2>/dev/null || :
		;;
	esac
	return 0
}

anvil_mcp_finish_pending_termination() {
	local status
	[ -n "$ANVIL_MCP_PENDING_TERMINATION_STATUS" ] || return 0
	[ "$ANVIL_MCP_CLEANUP_ACTIVE" -eq 0 ] || return 0
	status=$ANVIL_MCP_PENDING_TERMINATION_STATUS
	ANVIL_MCP_PENDING_TERMINATION_STATUS=
	ANVIL_MCP_RETIRED_RUNNER=
	ANVIL_MCP_FINALIZING=1
	trap ':' HUP INT TERM
	anvil_mcp_cleanup_once || :
	# This is the sole final cleanup transaction.  Do not let EXIT repeat its
	# two bounded attempts after a failure and multiply termination latency.
	trap - EXIT
	exit "$status"
}

anvil_mcp_finish_bounded_termination() {
	local runner="$1" launch_authorized="${2:-0}" runner_owned="${3:-0}"
	[ -n "$ANVIL_MCP_PENDING_TERMINATION_STATUS" ] || return 0
	[ "$ANVIL_MCP_CLEANUP_ACTIVE" -eq 0 ] || return 0
	ANVIL_MCP_ACTIVE_RUNNER=
	ANVIL_MCP_RUNNER_STARTING=0
	if [ "$runner" != "$ANVIL_MCP_RETIRED_RUNNER" ] \
		&& { [ "$launch_authorized" -eq 1 ] \
			|| [ "$runner_owned" -eq 1 ]; }; then
		anvil_mcp_converge_runner "$runner"
	fi
	exec 7<&- 2>/dev/null || :
	anvil_mcp_finish_pending_termination
}

anvil_mcp_cleanup_once() {
	local attempt=0 cleanup_rc=70
	[ "$ANVIL_MCP_CLEANUP_READY" -eq 1 ] || return 0
	[ "$ANVIL_MCP_CLEANUP_DONE" -eq 0 ] || return 0
	[ "$ANVIL_MCP_CLEANUP_ACTIVE" -eq 0 ] || return 0
	if [ -z "$ANVIL_MCP_TRANSACTION_DIRECTORY" ] \
		&& [ ! -e "$ANVIL_MCP_REQUEST_DIRECTORY" ]; then
		ANVIL_MCP_CLEANUP_DONE=1
		return 0
	fi
	ANVIL_MCP_CLEANUP_ACTIVE=1
	while [ "$attempt" -lt 2 ]; do
		attempt=$((attempt + 1))
		set +e
		anvil_mcp_cleanup_all_staged
		cleanup_rc=$?
		set -e
		if [ "$cleanup_rc" -eq 0 ] \
			&& { [ -z "$ANVIL_MCP_TRANSACTION_DIRECTORY" ] \
				|| [ ! -e "$ANVIL_MCP_TRANSACTION_DIRECTORY" ]; } \
			&& [ ! -e "$ANVIL_MCP_REQUEST_DIRECTORY" ]; then
			ANVIL_MCP_CLEANUP_DONE=1
			break
		fi
		[ "$cleanup_rc" -ne 0 ] || cleanup_rc=74
	done
	ANVIL_MCP_CLEANUP_ACTIVE=0
	if [ "$ANVIL_MCP_CLEANUP_DONE" -eq 0 ]; then
		printf 'anvil-mcp: staged cleanup failed after %s attempts (rc=%s)\n' \
			"$attempt" "$cleanup_rc" >&2
		return "$cleanup_rc"
	fi
	return 0
}

anvil_mcp_exit_cleanup() {
	local original_status=$? status
	ANVIL_MCP_FINALIZING=1
	trap ':' HUP INT TERM
	anvil_mcp_cleanup_once || :
	if [ -n "$ANVIL_MCP_PENDING_TERMINATION_STATUS" ]; then
		status=$ANVIL_MCP_PENDING_TERMINATION_STATUS
		ANVIL_MCP_PENDING_TERMINATION_STATUS=
		trap - EXIT
		exit "$status"
	fi
	return "$original_status"
}
trap 'anvil_mcp_terminate 129' HUP
trap 'anvil_mcp_terminate 130' INT
trap 'anvil_mcp_terminate 143' TERM

# --- Retry wrapper for emacsclient ------------------------------------
# Absorbs the ~few-second window where `emacs --daemon' is being
# restarted: emacsclient then fails with "can't find socket" /
# "Connection refused" until the new daemon's server file is ready.
# Retrying silently keeps the MCP pipe alive so upstream Claude Code
# (or whoever is driving this bridge) doesn't see a hard failure for
# a routine daemon bounce.
#
# Configure with env vars (defaults chosen to cover a typical restart):
#   ANVIL_EMACSCLIENT_RETRY_MAX        attempts (default 60)
#   ANVIL_EMACSCLIENT_RETRY_DELAY_MS   delay per attempt in ms (default 100)
# 60 * 100ms = 6 seconds of tolerance.
ANVIL_EMACSCLIENT_RETRY_MAX=${ANVIL_EMACSCLIENT_RETRY_MAX:-60}
ANVIL_EMACSCLIENT_RETRY_DELAY_MS=${ANVIL_EMACSCLIENT_RETRY_DELAY_MS:-100}
# Per-attempt readiness timeout.  Timed-out readiness probes are replayed
# only within the bounded readiness phase; the probe expression is pure.
# Runtime overrides may shorten these values for tests or local policy, but
# may never enlarge the package's cross-client safety envelope.
ANVIL_EMACSCLIENT_PROBE_TIMEOUT=${ANVIL_EMACSCLIENT_PROBE_TIMEOUT:-5}
ANVIL_EMACSCLIENT_READINESS_TIMEOUT=${ANVIL_EMACSCLIENT_READINESS_TIMEOUT:-20}
ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT=${ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT:-20}
ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT=${ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT:-150}
ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT=${ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT:-1}
ANVIL_MCP_REQUEST_PARSE_TIMEOUT=${ANVIL_MCP_REQUEST_PARSE_TIMEOUT:-10}
ANVIL_MCP_FRAME_READ_TIMEOUT=${ANVIL_MCP_FRAME_READ_TIMEOUT:-10}
ANVIL_MCP_RUNNER_CONTROL_TIMEOUT=${ANVIL_MCP_RUNNER_CONTROL_TIMEOUT:-10}
readonly ANVIL_MCP_MAX_REQUEST_BYTES=16777216
# Tool results are staged on disk before they cross emacsclient.  This limit is
# large enough for the worst-case JSON escaping of the 4 MiB shell tee while
# bounding both bridge memory and private staging storage.
readonly ANVIL_MCP_MAX_RESPONSE_BYTES=33554432
# Linux limits each exec argument to 128 KiB even when ARG_MAX is much larger.
# Keep inline request expressions comfortably below that boundary; larger
# requests travel through one private bridge-owned staging directory instead.
readonly ANVIL_MCP_INLINE_REQUEST_BYTES=16384
_anvil_request_tmp=${TMPDIR:-/tmp}
case "$_anvil_request_tmp" in
/) ANVIL_MCP_REQUEST_DIRECTORY="/anvil-mcp.$$.$RANDOM$RANDOM$RANDOM$RANDOM" ;;
*) ANVIL_MCP_REQUEST_DIRECTORY="${_anvil_request_tmp%/}/anvil-mcp.$$.$RANDOM$RANDOM$RANDOM$RANDOM" ;;
esac
readonly ANVIL_MCP_REQUEST_DIRECTORY
unset _anvil_request_tmp
ANVIL_MCP_REQUEST_SEQUENCE=0
ANVIL_MCP_TRANSACTION_DIRECTORY=
_anvil_response_payload=
_anvil_response_stage_path=
_anvil_response_path=
_anvil_response_proof_path=
_anvil_response_device=
_anvil_response_inode=
_anvil_request_path=

anvil_mcp_validate_timeout() {
	local name="$1" value="$2" maximum="$3"
	case "$value" in
	""|*[!0-9]*|0[0-9]*)
		echo "anvil-mcp: $name must be an integer between 1 and $maximum" >&2
		return 64
		;;
	esac
	if [ "${#value}" -gt "${#maximum}" ] \
		|| [ "$value" -lt 1 ] || [ "$value" -gt "$maximum" ]; then
		echo "anvil-mcp: $name must be between 1 and $maximum seconds" >&2
		return 64
	fi
}

anvil_mcp_validate_range() {
	local name="$1" value="$2" minimum="$3" maximum="$4" unit="$5"
	case "$value" in
	""|*[!0-9]*|0[0-9]*)
		echo "anvil-mcp: $name must be an integer" >&2
		return 64
		;;
	esac
	if [ "${#value}" -gt "${#maximum}" ] \
		|| [ "$value" -lt "$minimum" ] || [ "$value" -gt "$maximum" ]; then
		echo "anvil-mcp: $name must be between $minimum and $maximum $unit" >&2
		return 64
	fi
}

anvil_mcp_validate_range ANVIL_EMACSCLIENT_RETRY_MAX \
	"$ANVIL_EMACSCLIENT_RETRY_MAX" 1 1000 attempts
anvil_mcp_validate_range ANVIL_EMACSCLIENT_RETRY_DELAY_MS \
	"$ANVIL_EMACSCLIENT_RETRY_DELAY_MS" 0 60000 milliseconds
anvil_mcp_validate_timeout ANVIL_EMACSCLIENT_PROBE_TIMEOUT \
	"$ANVIL_EMACSCLIENT_PROBE_TIMEOUT" 5
anvil_mcp_validate_timeout ANVIL_EMACSCLIENT_READINESS_TIMEOUT \
	"$ANVIL_EMACSCLIENT_READINESS_TIMEOUT" 20
anvil_mcp_validate_timeout ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT \
	"$ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT" 20
anvil_mcp_validate_timeout ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT \
	"$ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT" 150
anvil_mcp_validate_timeout ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT \
	"$ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT" 1
anvil_mcp_validate_timeout ANVIL_MCP_REQUEST_PARSE_TIMEOUT \
	"$ANVIL_MCP_REQUEST_PARSE_TIMEOUT" 10
anvil_mcp_validate_timeout ANVIL_MCP_FRAME_READ_TIMEOUT \
	"$ANVIL_MCP_FRAME_READ_TIMEOUT" 10
anvil_mcp_validate_timeout ANVIL_MCP_RUNNER_CONTROL_TIMEOUT \
	"$ANVIL_MCP_RUNNER_CONTROL_TIMEOUT" 10

ANVIL_MCP_PYTHON=$(type -P python3 || :)
ANVIL_MCP_EMACSCLIENT=$(type -P emacsclient || :)
ANVIL_MCP_SLEEP=$(type -P sleep || :)
for _anvil_program in \
	ANVIL_MCP_PYTHON ANVIL_MCP_EMACSCLIENT ANVIL_MCP_SLEEP; do
	if [ -z "${!_anvil_program}" ]; then
		echo "anvil-mcp: ${_anvil_program#ANVIL_MCP_} is required" >&2
		exit 69
	fi
done
readonly ANVIL_MCP_PYTHON ANVIL_MCP_EMACSCLIENT ANVIL_MCP_SLEEP
unset _anvil_program
if [ -n "$ANVIL_MCP_PARENT_GUARD" ]; then
	[ -n "$ANVIL_MCP_PARENT_GUARD_PYTHON" ] \
		|| ANVIL_MCP_PARENT_GUARD_PYTHON=$ANVIL_MCP_PYTHON
	case "$ANVIL_MCP_PARENT_GUARD:$ANVIL_MCP_PARENT_GUARD_PYTHON" in
	/*:/*) ;;
	*)
		echo "anvil-mcp: parent guard paths must be absolute" >&2
		exit 64
		;;
	esac
	if [ ! -r "$ANVIL_MCP_PARENT_GUARD" ] \
		|| [ ! -x "$ANVIL_MCP_PARENT_GUARD_PYTHON" ]; then
		echo "anvil-mcp: parent guard or its Python is unavailable" >&2
		exit 69
	fi
elif [ -n "$ANVIL_MCP_PARENT_GUARD_PYTHON" ]; then
	echo "anvil-mcp: parent guard Python was set without a guard" >&2
	exit 64
fi
readonly ANVIL_MCP_PARENT_GUARD ANVIL_MCP_PARENT_GUARD_PYTHON

# Run one emacsclient invocation under a hard timeout.  This helper never
# retries: callers must decide whether the expression is safe to replay.
anvil_emacsclient_once() {
	local timeout_seconds="$1"
	shift
	if [ "${1-}" = "--" ]; then shift; fi

	anvil_mcp_run_bounded "$timeout_seconds" merge null "" emacsclient \
		"$ANVIL_MCP_EMACSCLIENT" -a false "$@"
	return "$ANVIL_MCP_RUN_STATUS"
}

anvil_emacsclient_probe_delay() {
	local started_seconds="$1"
	local delay_ms=$ANVIL_EMACSCLIENT_RETRY_DELAY_MS
	if [ "$ANVIL_EMACSCLIENT_READINESS_TIMEOUT" -ne 0 ]; then
		local elapsed=$((SECONDS - started_seconds))
		local remaining=$((ANVIL_EMACSCLIENT_READINESS_TIMEOUT - elapsed))
		if [ "$remaining" -le 0 ]; then
			return 0
		fi
		local remaining_ms=$((remaining * 1000))
		if [ "$delay_ms" -gt "$remaining_ms" ]; then
			delay_ms=$remaining_ms
		fi
	fi
	if [ "$delay_ms" -gt 0 ]; then
		local delay_sec
		printf -v delay_sec '%d.%03d' \
			"$((delay_ms / 1000))" "$((delay_ms % 1000))"
		local delay_cap=$((delay_ms / 1000 + 1))
		anvil_mcp_run_bounded "$delay_cap" merge null "" probe-delay \
			"$ANVIL_MCP_SLEEP" "$delay_sec"
	fi
}

# Retry only the side-effect-free readiness expression.  A sibling bridge may
# occupy the single Emacs server event loop longer than one probe timeout, so
# timeout exits and exact nil results are replayed until the readiness budget
# expires.  Any other successful output fails closed.
# Missing/refused socket races retain their independent attempt limit.
anvil_emacsclient_probe_retry() {
	if [ "${1-}" = "--" ]; then shift; fi

	local socket_attempt=0 timeout_attempt=0 out="" rc=0
	local started_seconds=$SECONDS this_timeout elapsed remaining
	ANVIL_EMACSCLIENT_PROBE_OUTPUT=
	while :; do
		this_timeout=$ANVIL_EMACSCLIENT_PROBE_TIMEOUT
		if [ "$ANVIL_EMACSCLIENT_READINESS_TIMEOUT" = "0" ]; then
			# A disabled overall timeout means one unbounded readiness wait.
			this_timeout=0
		else
			elapsed=$((SECONDS - started_seconds))
			remaining=$((ANVIL_EMACSCLIENT_READINESS_TIMEOUT - elapsed))
			if [ "$remaining" -le 0 ]; then
				mcp_debug_log "PROBE-WAIT-EXHAUSTED" \
					"timeouts=$timeout_attempt budget=${ANVIL_EMACSCLIENT_READINESS_TIMEOUT}s rc=$rc"
				ANVIL_EMACSCLIENT_PROBE_OUTPUT=$out
				return "$rc"
			fi
			if [ "$this_timeout" -eq 0 ] \
				|| [ "$this_timeout" -gt "$remaining" ]; then
				this_timeout=$remaining
			fi
		fi
		if anvil_emacsclient_once "$this_timeout" -- "$@"; then
			rc=0
		else
			rc=$?
		fi
		out=$ANVIL_MCP_RUN_OUTPUT
		# Bounded capture retains emacsclient's terminal LF; normalize the
		# same trailing line endings that command substitution historically
		# removed before applying the exact readiness contract.
		while [[ "$out" == *$'\n' ]]; do
			out=${out%$'\n'}
		done
		case "$out" in
		*$'\r') out=${out%$'\r'} ;;
		esac
		if [ "$rc" -eq 0 ]; then
			case "$out" in
			t)
				ANVIL_EMACSCLIENT_PROBE_OUTPUT=$out
				return 0
				;;
			nil)
				# Preserve a nonzero status if the overall deadline expires
				# after a sequence of successful-but-not-ready probes.
				rc=75
				mcp_debug_log "PROBE-NOT-READY" \
					"budget=${ANVIL_EMACSCLIENT_READINESS_TIMEOUT}s"
				anvil_emacsclient_probe_delay "$started_seconds"
				continue
				;;
			*)
				mcp_debug_log "PROBE-INVALID-OUTPUT" \
					"rc=0 bytes=${#out}"
				ANVIL_EMACSCLIENT_PROBE_OUTPUT=$out
				return 75
				;;
			esac
		fi
		case "$rc" in
		124|137|142)
			timeout_attempt=$((timeout_attempt + 1))
			mcp_debug_log "PROBE-TIMEOUT" \
				"attempt=$timeout_attempt rc=$rc budget=${ANVIL_EMACSCLIENT_READINESS_TIMEOUT}s"
			continue
			;;
		esac
		if [[ "$out" == *"can't find socket"* \
			|| "$out" == *"Connection refused"* \
			|| "$out" == *server*not*running* \
			|| "$out" == *"No such file or directory"* ]]; then
			socket_attempt=$((socket_attempt + 1))
			if [ "$socket_attempt" -ge "$ANVIL_EMACSCLIENT_RETRY_MAX" ]; then
				mcp_debug_log "PROBE-RETRY-EXHAUSTED" "attempts=$socket_attempt max=$ANVIL_EMACSCLIENT_RETRY_MAX rc=$rc"
				ANVIL_EMACSCLIENT_PROBE_OUTPUT=$out
				return "$rc"
			fi
			if [ "$socket_attempt" -eq 1 ] || [ $((socket_attempt % 10)) -eq 0 ]; then
				local probe_summary="${out//$'\n'/ }"
				mcp_debug_log "PROBE-RETRY" \
					"attempt=$socket_attempt rc=$rc stderr=${probe_summary:0:120}"
			fi
			anvil_emacsclient_probe_delay "$started_seconds"
			continue
		fi
		ANVIL_EMACSCLIENT_PROBE_OUTPUT=$out
		return "$rc"
	done
}

anvil_emacsclient_dispatch_once() {
	local timeout_seconds="$1"
	shift
	if [ "${1-}" = "--" ]; then shift; fi

	anvil_mcp_run_bounded "$timeout_seconds" separate null "" dispatch \
		"$ANVIL_MCP_EMACSCLIENT" -a false "$@"
	return "$ANVIL_MCP_RUN_STATUS"
}

# Parse command line arguments
while [ $# -gt 0 ]; do
	case "$1" in
	--init-function=*)
		INIT_FUNCTION="${1#--init-function=}"
		shift
		;;
	--stop-function=*)
		STOP_FUNCTION="${1#--stop-function=}"
		shift
		;;
	--socket=*)
		SOCKET="${1#--socket=}"
		shift
		;;
	--server-id=*)
		SERVER_ID="${1#--server-id=}"
		shift
		;;
	*)
		echo "Unknown option: $1" >&2
		echo "Usage: $0 [--init-function=name] [--stop-function=name] [--socket=path] [--server-id=id]" >&2
		exit 1
		;;
	esac
done

# Set socket arguments if provided
if [ -n "$SOCKET" ]; then
	readonly SOCKET_OPTIONS=("-s" "$SOCKET")
	mcp_debug_log "INFO" "Using socket: $SOCKET"
else
	readonly SOCKET_OPTIONS=()
fi

# Log init function info if provided
if [ -n "$INIT_FUNCTION" ]; then
	mcp_debug_log "INFO" "Using init function: $INIT_FUNCTION"

	# Derive server-id from init function if not explicitly provided
	# This is a hack for backwards compatibility and will be removed later
	if [ -z "$SERVER_ID" ]; then
		# Extract server-id by removing -mcp-enable suffix
		SERVER_ID="${INIT_FUNCTION%-mcp-enable}"
		mcp_debug_log "INFO" "Derived server-id from init function: $SERVER_ID"
	fi
else
	mcp_debug_log "INFO" "No init function specified"
fi

# Log server-id
if [ -n "$SERVER_ID" ]; then
	mcp_debug_log "INFO" "Using server-id: $SERVER_ID"
else
	# Default to "default" if not specified
	SERVER_ID="default"
	mcp_debug_log "INFO" "Using default server-id: $SERVER_ID"
fi

case "$ANVIL_MCP_READINESS_MODE" in
emacs)
	ANVIL_EMACSCLIENT_READY_EXPR=t
	;;
headless)
	case "$SERVER_ID" in
	""|*[!A-Za-z0-9._-]*)
		echo "anvil-mcp: unsafe server id for headless readiness" >&2
		exit 64
		;;
	esac
	ANVIL_EMACSCLIENT_READY_EXPR="(and (fboundp 'anvil-headless--ready-p) (anvil-headless--ready-p \"$SERVER_ID\"))"
	;;
*)
	echo "anvil-mcp: unsupported readiness mode: $ANVIL_MCP_READINESS_MODE" >&2
	exit 64
	;;
esac
ANVIL_MCP_NOT_READY_SENTINEL=anvil-mcp-headless-not-ready
ANVIL_MCP_LIFECYCLE_COMPLETE=anvil-mcp-lifecycle-complete
ANVIL_MCP_RESPONSE_STAGED_PREFIX=anvil-mcp-response-staged:
readonly ANVIL_MCP_READINESS_MODE ANVIL_EMACSCLIENT_READY_EXPR
readonly ANVIL_MCP_NOT_READY_SENTINEL ANVIL_MCP_LIFECYCLE_COMPLETE
readonly ANVIL_MCP_RESPONSE_STAGED_PREFIX

# Initialize MCP if init function is provided.  Probe readiness with the
# only replayable expression, then invoke the potentially stateful init once.
if [ -n "$INIT_FUNCTION" ]; then
	mcp_debug_log "INIT-CALL" "readiness probe, then one emacsclient -e ($INIT_FUNCTION)"

	init_probe_output=""
	set +e
	anvil_emacsclient_probe_retry -- \
		${SOCKET_OPTIONS[@]+"${SOCKET_OPTIONS[@]}"} \
		-e "$ANVIL_EMACSCLIENT_READY_EXPR"
	INIT_READY_RC=$?
	init_probe_output=$ANVIL_EMACSCLIENT_PROBE_OUTPUT
	INIT_RC=$INIT_READY_RC
	set -e
	if [ "$INIT_READY_RC" -ne 0 ]; then
		anvil_mcp_log_probe_stderr "INIT-PROBE-STDERR" "$init_probe_output"
	fi
	mcp_debug_log "INIT-READY-RC" "$INIT_READY_RC"

	if [ "$INIT_READY_RC" -eq 0 ]; then
		if anvil_mcp_capture_begin; then
			ANVIL_MCP_RESPONSE_PENDING=1
			set +e
			anvil_emacsclient_dispatch_once \
				"$ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT" -- \
				${SOCKET_OPTIONS[@]+"${SOCKET_OPTIONS[@]}"} \
				-e "(if $ANVIL_EMACSCLIENT_READY_EXPR (progn ($INIT_FUNCTION) \"$ANVIL_MCP_LIFECYCLE_COMPLETE\") \"$ANVIL_MCP_NOT_READY_SENTINEL\")"
			INIT_RC=$?
			init_dispatch_output=$ANVIL_MCP_RUN_OUTPUT
			set -e
			anvil_mcp_capture_finish
			ANVIL_MCP_RESPONSE_PENDING=0
			while :; do
				case "$init_dispatch_output" in
				*$'\n') init_dispatch_output=${init_dispatch_output%$'\n'} ;;
				*$'\r') init_dispatch_output=${init_dispatch_output%$'\r'} ;;
				*) break ;;
				esac
			done
			if [ "${#init_dispatch_output}" -ge 2 ] \
				&& [[ "$init_dispatch_output" == \"* && "$init_dispatch_output" == *\" ]]; then
				init_dispatch_output="${init_dispatch_output:1:${#init_dispatch_output}-2}"
			fi
			if [ "$INIT_RC" -eq 0 ] \
				&& [ "$init_dispatch_output" = "$ANVIL_MCP_NOT_READY_SENTINEL" ]; then
				INIT_RC=75
			elif [ "$INIT_RC" -eq 0 ] \
				&& [ "$init_dispatch_output" != "$ANVIL_MCP_LIFECYCLE_COMPLETE" ]; then
				INIT_RC=70
			fi
		else
			INIT_RC=74
			mcp_debug_log "INIT-CAPTURE" "failed before dispatch rc=$INIT_RC"
		fi
	fi
	mcp_debug_log "INIT-RC" "$INIT_RC"
else
	mcp_debug_log "INFO" "Skipping init function call (none provided)"
fi
anvil_mcp_finish_pending_termination

# --- T71: MCP Content-Length framing read helpers --------------------
# The standard MCP stdio transport frames messages as:
#
#     Content-Length: <N>\r\n
#     \r\n
#     <N bytes JSON body>
#
# Older callers may still emit line-delimited JSON.  We sniff the first
# byte of each request: if it is `{', treat as legacy line mode; if it
# is `C' (start of "Content-Length:"), treat as framed.  Mode is
# decided per-request to keep the legacy fallback transparent for
# dev/test invocations.
#
# Output: when input was framed, emit a framed response; otherwise emit
# legacy single-line JSON.

# Read one post-first-byte line without letting Bash normalize wire bytes.
# The helper reads exactly through LF, never reads ahead into the next request,
# rejects NUL before emitting anything, and publishes output only on success.
ANVIL_MCP_BINARY_LINE=""
anvil_mcp_read_binary_line() {
	local frame_deadline="$1" max_bytes="$2" frame_remaining
	ANVIL_MCP_BINARY_LINE=""
	frame_remaining=$((frame_deadline - SECONDS))
	[ "$frame_remaining" -gt 0 ] || return 1
	exec 5<&0
	anvil_mcp_run_bounded "$frame_remaining" merge descriptor "" frame-line \
		"$ANVIL_MCP_PYTHON" -I -S -c '
import os
import sys

limit = int(sys.argv[1])
line = bytearray()
while len(line) <= limit:
    byte = os.read(0, 1)
    if not byte:
        raise SystemExit(1)
    if byte == b"\0":
        raise SystemExit(65)
    if byte == b"\n":
        view = memoryview(line)
        while view:
            written = os.write(1, view)
            if written <= 0:
                raise SystemExit(1)
            view = view[written:]
        raise SystemExit(0)
    line.extend(byte)
raise SystemExit(3)
' "$max_bytes"
	exec 5<&-
	[ "$ANVIL_MCP_RUN_STATUS" -eq 0 ] || return "$ANVIL_MCP_RUN_STATUS"
	ANVIL_MCP_BINARY_LINE=$ANVIL_MCP_RUN_OUTPUT
	return 0
}

# anvil_mcp_read_framed_message
#
# Reads one MCP framed message from STDIN.  Headers are read line by
# line until an empty line; then exactly N bytes of body are read.
# Prints the JSON body on STDOUT (no trailing newline).  Returns 1 on
# EOF or malformed framing, 2 if no Content-Length header found.
anvil_mcp_read_framed_message() {
	local first_line="$1"
	local frame_deadline="$2"
	local header_line content_length="" frame_remaining
	ANVIL_MCP_FRAME_BODY=""

	# Process the already-consumed first line.
	# Strip trailing CR (DOS line endings).
	first_line="${first_line%$'\r'}"
	if [[ "$first_line" =~ ^[Cc][Oo][Nn][Tt][Ee][Nn][Tt]-[Ll][Ee][Nn][Gg][Tt][Hh]:[[:space:]]*([0-9]+)[[:space:]]*$ ]]; then
		content_length="${BASH_REMATCH[1]}"
	fi

	# Read remaining header lines under the same cumulative frame deadline as
	# the body.  A client that stalls with stdin open after Content-Length must
	# not retain this bridge indefinitely before dispatch.
	while :; do
		if ! anvil_mcp_read_binary_line "$frame_deadline" 65536; then
			return 1
		fi
		header_line="${ANVIL_MCP_BINARY_LINE%$'\r'}"
		if [ -z "$header_line" ]; then
			break
		fi
		if [[ "$header_line" =~ ^[Cc][Oo][Nn][Tt][Ee][Nn][Tt]-[Ll][Ee][Nn][Gg][Tt][Hh]:[[:space:]]*([0-9]+)[[:space:]]*$ ]]; then
			content_length="${BASH_REMATCH[1]}"
		fi
	done

	if [ -z "$content_length" ]; then
		return 2
	fi
	if [ "${#content_length}" -gt 8 ] \
		|| [ "$content_length" -gt "$ANVIL_MCP_MAX_REQUEST_BYTES" ]; then
		return 3
	fi

	# Read exactly content_length bytes within the unspent frame budget.  The
	# bounded runner preserves trailing newlines that command substitution would
	# strip.  The unbuffered reader never asks the pipe for more than the frame's
	# remaining bytes; `head -c' may over-read and discard a pipelined request.
	frame_remaining=$((frame_deadline - SECONDS))
	[ "$frame_remaining" -gt 0 ] || return 1
	mcp_debug_log "FRAMING" \
		"body reader start bytes=$content_length remaining=$frame_remaining"
	exec 5<&0
	anvil_mcp_run_bounded "$frame_remaining" merge descriptor "" frame-body \
		"$ANVIL_MCP_PYTHON" -I -S -c '
import os
import sys

remaining = int(sys.argv[1])
while remaining:
    chunk = os.read(0, min(65536, remaining))
    if not chunk:
        raise SystemExit(1)
    if b"\0" in chunk:
        raise SystemExit(65)
    view = memoryview(chunk)
    while view:
        written = os.write(1, view)
        if written <= 0:
            raise SystemExit(1)
        view = view[written:]
    remaining -= len(chunk)
' "$content_length"
	exec 5<&-
	if [ "$ANVIL_MCP_RUN_STATUS" -ne 0 ]; then
		return 1
	fi
	local LC_ALL=C
	if [ "${#ANVIL_MCP_RUN_OUTPUT}" -ne "$content_length" ]; then
		return 1
	fi
	ANVIL_MCP_FRAME_BODY=$ANVIL_MCP_RUN_OUTPUT
	return 0
}

# anvil_mcp_emit_framed_response BODY
#
# Emits BODY framed with Content-Length.  N is computed in bytes
# (not characters) because the MCP spec mandates byte length.
anvil_mcp_emit_framed_response() {
	local body="$1"
	local LC_ALL=C
	local n=${#body}
	printf 'Content-Length: %s\r\n\r\n%s' "$n" "$body"
}

anvil_mcp_emit_response() {
	local framed="$1"
	local body="$2"
	anvil_mcp_finish_pending_termination
	if [ "$framed" = "1" ]; then
		anvil_mcp_emit_framed_response "$body"
	else
		printf '%s\n' "$body"
	fi
	anvil_mcp_finish_pending_termination
	ANVIL_MCP_RESPONSE_PENDING=0
}

# Authenticate FD 6 before dispatch without relying on /dev/fd stat identity.
# macOS Bash 3.2 reports /dev/fd/N on devfs, so -ef rejects a correct descriptor.
# Duplicate the held descriptor to bounded-run stdin; the child closes FD 6
# itself and fstats only descriptor 0.
anvil_mcp_validate_response_fd() {
	local expected_device="$1" expected_inode="$2" status
	if ! exec 5<&6; then
		return 74
	fi
	anvil_mcp_run_bounded "$ANVIL_MCP_REQUEST_PARSE_TIMEOUT" merge descriptor "" \
		validate-response-fd \
		"$ANVIL_MCP_PYTHON" -I -S -c '
import os
import stat
import sys

device = sys.argv[1]
inode = sys.argv[2]
if (
    not device.isascii()
    or not device.isdecimal()
    or not inode.isascii()
    or not inode.isdecimal()
):
    raise SystemExit(64)
info = os.fstat(0)
if (
    not stat.S_ISREG(info.st_mode)
    or info.st_uid != os.geteuid()
    or stat.S_IMODE(info.st_mode) != 0o600
    or info.st_nlink != 1
    or info.st_size != 0
    or info.st_dev != int(device)
    or info.st_ino != int(inode)
):
    raise SystemExit(74)
print("anvil-response-fd-authenticated")
' "$expected_device" "$expected_inode"
	status=$ANVIL_MCP_RUN_STATUS
	exec 5<&-
	[ "$status" -eq 0 ] || return "$status"
	[ "$ANVIL_MCP_RUN_OUTPUT" = "anvil-response-fd-authenticated"$'\n' ] \
		|| return 70
	return 0
}

# Validate the two published names without ever opening either for response
# bytes.  Linux exposes the target device through /dev/fd and gets an exact
# descriptor comparison.  Darwin's devfs does not; its FD was authenticated
# above, while the server validates the expected device/inode around both links.
anvil_mcp_response_names_valid() {
	local path="$1" proof="$2"
	[ -e /dev/fd/6 ] \
		&& [ -f "$path" ] && [ ! -L "$path" ] && [ -O "$path" ] \
		&& [ -f "$proof" ] && [ ! -L "$proof" ] && [ -O "$proof" ] \
		&& [[ "$path" -ef "$proof" ]] \
		|| return 1
	if [ "$ANVIL_MCP_FD_PATH_IDENTITY" -eq 1 ]; then
		[[ "$path" -ef /dev/fd/6 ]] || return 1
	fi
}

# Load one descriptor-authenticated response using Bash 3-compatible builtins.
# FD 6 is opened and authenticated before stateful dispatch.  The published
# names must remain one hard-link pair before and after the bounded read, but
# response bytes always come exclusively from FD 6, never a callback-reachable
# pathname.
ANVIL_MCP_STAGED_RESPONSE=
ANVIL_MCP_STAGED_RESPONSE_BYTES=0
ANVIL_MCP_FD_PATH_IDENTITY=0
anvil_mcp_read_staged_response() {
	local path="$1" proof="$2" expected_size="$3"
	local body="" read_status=0 size
	local LC_ALL=C
	ANVIL_MCP_STAGED_RESPONSE=
	ANVIL_MCP_STAGED_RESPONSE_BYTES=0
	case "$expected_size" in
	"") ;;
	*[!0-9]*|0[0-9]*)
		exec 6<&-
		return 65
		;;
	*)
		if [ "${#expected_size}" -gt "${#ANVIL_MCP_MAX_RESPONSE_BYTES}" ] \
			|| { [ "${#expected_size}" -eq "${#ANVIL_MCP_MAX_RESPONSE_BYTES}" ] \
				&& [[ "$expected_size" > "$ANVIL_MCP_MAX_RESPONSE_BYTES" ]]; }; then
			exec 6<&-
			return 65
		fi
		;;
	esac
	if ! anvil_mcp_response_names_valid "$path" "$proof"; then
		exec 6<&-
		return 66
	fi
	if IFS= read -r -d '' -n "$((ANVIL_MCP_MAX_RESPONSE_BYTES + 1))" body <&6; then
		read_status=0
	else
		read_status=$?
	fi
	if ! anvil_mcp_response_names_valid "$path" "$proof"; then
		exec 6<&-
		return 66
	fi
	exec 6<&-
	if [ "$read_status" -eq 0 ]; then
		return 65
	fi
	size=${#body}
	if [ "$size" -gt "$ANVIL_MCP_MAX_RESPONSE_BYTES" ]; then
		return 65
	fi
	if [ -n "$expected_size" ] && [ "$size" -ne "$expected_size" ]; then
		return 65
	fi
	ANVIL_MCP_STAGED_RESPONSE=$body
	ANVIL_MCP_STAGED_RESPONSE_BYTES=$size
	return 0
}

# Emit only after the entire generation-bound response has been validated.
# The file remains in private custody until the next pre-dispatch cleanup or
# bridge EXIT; never mutate a callback-reachable pathname after dispatch.
anvil_mcp_emit_staged_response() {
	local framed="$1" request_kind="$2"
	anvil_mcp_finish_pending_termination
	case "$request_kind" in
	notification)
		[ "$ANVIL_MCP_STAGED_RESPONSE_BYTES" -eq 0 ] || return 65
		;;
	request)
		[ "$ANVIL_MCP_STAGED_RESPONSE_BYTES" -gt 0 ] || return 65
		if [ "$framed" = "1" ]; then
			printf 'Content-Length: %s\r\n\r\n' \
				"$ANVIL_MCP_STAGED_RESPONSE_BYTES" || return 74
			printf '%s' "$ANVIL_MCP_STAGED_RESPONSE" || return 74
		else
			printf '%s\n' "$ANVIL_MCP_STAGED_RESPONSE" || return 74
		fi
		;;
	*) return 65 ;;
	esac
	anvil_mcp_finish_pending_termination
	ANVIL_MCP_STAGED_RESPONSE=
	ANVIL_MCP_STAGED_RESPONSE_BYTES=0
	ANVIL_MCP_RESPONSE_PENDING=0
	return 0
}

# Erase the exact generation's staged request bytes without starting a helper.
# A delayed Emacs event can only delete or consume this unique generation, never
# a later request.  Bash 3 cannot revalidate mode or link count; Python
# established those inside the private 0700 directory before dispatch.
anvil_mcp_retire_staged_request() {
	local path="$1"
	local expected="$ANVIL_MCP_TRANSACTION_DIRECTORY/request.${ANVIL_MCP_REQUEST_SEQUENCE}.json"
	[ "$path" = "$expected" ] || return 74
	if [ -L "$path" ]; then
		return 74
	fi
	if [ ! -e "$path" ]; then
		return 0
	fi
	# Do not mutate a callback-reachable pathname after validation.  This
	# generation is unique and remains inside the private 0700 directory until
	# the next metadata cleanup or EXIT unlinks the directory entry.
	[ -f "$path" ] && [ -O "$path" ]
}

# Parse only bounded request metadata outside Emacs.  The root may be dead on
# the error path, so correlation cannot depend on another emacsclient call.
# Output is KIND|STARTUP|MODE|PAYLOAD|BYTE-SIZE|JSON-ID.  Small requests use an
# inline base64 payload.  Large requests are marked for bounded staging only
# after the readiness probe succeeds, avoiding both exec argument limits and
# unused files when Emacs is unavailable.
anvil_mcp_request_metadata() {
	local request="$1" transaction_directory="${2:-}"
	anvil_mcp_run_bounded "$ANVIL_MCP_REQUEST_PARSE_TIMEOUT" \
		merge input "$request" request-metadata "$ANVIL_MCP_PYTHON" -I -S -c '
import base64
import json
import math
import os
import stat
import sys

maximum = int(sys.argv[1])
inline_maximum = int(sys.argv[2])
transaction_directory = sys.argv[3]

def generation_file(name, prefix):
    pieces = name.split(".")
    return (
        len(pieces) == 3
        and pieces[0] == prefix
        and pieces[1].isdigit()
        and pieces[2] == "json"
    )

if transaction_directory:
    directory = os.path.abspath(transaction_directory)
    info = os.lstat(directory)
    if (
        os.path.realpath(directory) != directory
        or not stat.S_ISDIR(info.st_mode)
        or info.st_uid != os.geteuid()
        or stat.S_IMODE(info.st_mode) != 0o700
    ):
        raise SystemExit(74)
    while True:
        names = os.listdir(directory)
        if not names:
            break
        for name in names:
            allowed = (
                generation_file(name, "request")
                or generation_file(name, "response")
                or generation_file(name, "proof")
                or name.startswith(".response-tmp.")
            )
            if not allowed:
                raise SystemExit(74)
            path = os.path.join(directory, name)
            try:
                entry = os.lstat(path)
            except FileNotFoundError:
                continue
            if stat.S_ISDIR(entry.st_mode):
                raise SystemExit(74)
            try:
                os.unlink(path)
            except FileNotFoundError:
                pass

raw = sys.stdin.buffer.read(maximum + 1)

def emit(kind, startup, request_id):
    if kind in {"request", "notification"}:
        if len(raw) <= inline_maximum:
            mode = "inline"
            payload = base64.b64encode(raw).decode("ascii")
        else:
            mode = "file"
            payload = ""
    else:
        mode = "none"
        payload = ""
    encoded_id = json.dumps(
        request_id,
        ensure_ascii=True,
        separators=(",", ":"),
    )
    print(
        f"{kind}|{1 if startup else 0}|{mode}|{payload}|{len(raw)}|{encoded_id}"
    )

if len(raw) > maximum:
    emit("parse-error", False, None)
else:
    try:
        document = json.loads(
            raw,
            parse_constant=lambda value: (_ for _ in ()).throw(
                ValueError(value)
            ),
        )
    except (UnicodeDecodeError, ValueError):
        emit("parse-error", False, None)
    else:
        if not isinstance(document, dict):
            emit("invalid-request", False, None)
        else:
            startup = document.get("method") == "initialize"
            if "id" not in document:
                emit("notification", startup, None)
            else:
                request_id = document["id"]
                valid_id = (
                    request_id is None
                    or isinstance(request_id, str)
                    or (
                        not isinstance(request_id, bool)
                        and isinstance(request_id, int)
                    )
                    or (
                        isinstance(request_id, float)
                        and math.isfinite(request_id)
                    )
                )
                if valid_id:
                    emit("request", startup, request_id)
                else:
                    emit("invalid-request", startup, None)
' "$ANVIL_MCP_MAX_REQUEST_BYTES" "$ANVIL_MCP_INLINE_REQUEST_BYTES" \
		"$transaction_directory"
	return "$ANVIL_MCP_RUN_STATUS"
}

# Prepare one generation-bound response inode before stateful dispatch.
# The helper canonicalizes the parent, validates ownership and modes, creates a
# random .response-tmp.SEQUENCE.* inode without following links, and preflights
# same-directory hard links.  It returns
# BASE64-PATH<TAB>CANONICAL-PATH<TAB>DEVICE<TAB>INODE.
anvil_mcp_prepare_response() {
	local requested_directory="$1" basename="$2"
	anvil_mcp_run_bounded "$ANVIL_MCP_REQUEST_PARSE_TIMEOUT" \
		merge null "" prepare-response "$ANVIL_MCP_PYTHON" -I -S -c '
import base64
import os
import signal
import stat
import sys
import tempfile

requested_directory = os.path.abspath(sys.argv[1])
basename = sys.argv[2]
pieces = basename.split(".")
if not (
    len(pieces) == 3
    and pieces[0] == "response"
    and pieces[1].isdigit()
    and pieces[2] == "json"
):
    raise SystemExit(64)

root = os.path.realpath(os.path.dirname(requested_directory))
directory = os.path.join(root, os.path.basename(requested_directory))
root_stat = os.lstat(root)
root_mode = stat.S_IMODE(root_stat.st_mode)
private_root = root_stat.st_uid == os.geteuid() and not (root_mode & 0o022)
sticky_root = bool(root_mode & stat.S_ISVTX) and root_stat.st_uid in {
    0,
    os.geteuid(),
}
if not stat.S_ISDIR(root_stat.st_mode) or not (private_root or sticky_root):
    raise SystemExit(73)

directory_created = False
descriptor = -1
path = ""
probe_path = ""
created = False


def cleanup():
    global descriptor, created, directory_created
    open_descriptor = descriptor
    descriptor = -1
    if open_descriptor >= 0:
        try:
            os.close(open_descriptor)
        except OSError:
            pass
    if probe_path:
        try:
            os.unlink(probe_path)
        except OSError:
            pass
    remove_path = created
    created = False
    if remove_path and path:
        try:
            os.unlink(path)
        except OSError:
            pass
    remove_directory = directory_created
    directory_created = False
    if remove_directory:
        try:
            os.rmdir(directory)
        except OSError:
            pass


def without_term(function):
    if not hasattr(signal, "pthread_sigmask"):
        return function()
    previous = signal.pthread_sigmask(signal.SIG_BLOCK, {signal.SIGTERM})
    try:
        return function()
    finally:
        signal.pthread_sigmask(signal.SIG_SETMASK, previous)


def create_directory():
    global directory_created
    try:
        os.mkdir(directory, 0o700)
        directory_created = True
    except FileExistsError:
        pass


def create_stage():
    global descriptor, path, probe_path, created
    descriptor, path = tempfile.mkstemp(
        prefix=f".response-tmp.{pieces[1]}.",
        dir=directory,
    )
    created = True
    os.fchmod(descriptor, 0o600)
    probe_path = path + ".link-probe"


def interrupted(_signum, _frame):
    signal.signal(signal.SIGTERM, signal.SIG_IGN)
    cleanup()
    raise TimeoutError("response staging interrupted")


signal.signal(signal.SIGTERM, interrupted)
try:
    without_term(create_directory)
    directory_stat = os.lstat(directory)
    if (
        os.path.realpath(directory) != directory
        or not stat.S_ISDIR(directory_stat.st_mode)
        or directory_stat.st_uid != os.geteuid()
        or stat.S_IMODE(directory_stat.st_mode) != 0o700
    ):
        raise SystemExit(73)

    without_term(create_stage)
    info = os.fstat(descriptor)
    if (
        not stat.S_ISREG(info.st_mode)
        or info.st_uid != os.geteuid()
        or info.st_nlink != 1
        or stat.S_IMODE(info.st_mode) != 0o600
        or info.st_size != 0
    ):
        raise PermissionError("unsafe response stage")

    def probe_hardlink():
        os.link(path, probe_path)
        held = os.fstat(descriptor)
        linked = os.lstat(probe_path)
        if (
            not stat.S_ISREG(linked.st_mode)
            or linked.st_uid != os.geteuid()
            or stat.S_IMODE(linked.st_mode) != 0o600
            or linked.st_size != 0
            or held.st_dev != linked.st_dev
            or held.st_ino != linked.st_ino
            or held.st_nlink != 2
            or linked.st_nlink != 2
        ):
            raise PermissionError("unsafe response hard-link probe")
        os.unlink(probe_path)
        held = os.fstat(descriptor)
        original = os.lstat(path)
        if (
            held.st_dev != original.st_dev
            or held.st_ino != original.st_ino
            or held.st_nlink != 1
            or original.st_nlink != 1
        ):
            raise PermissionError("response hard-link probe did not converge")

    without_term(probe_hardlink)
    info = os.fstat(descriptor)
    open_descriptor = descriptor
    descriptor = -1
    os.close(open_descriptor)
    text_path = os.fsdecode(path)
    if any(character in text_path for character in "\t\r\n"):
        raise SystemExit(64)
    encoded = base64.b64encode(os.fsencode(path)).decode("ascii")
    print(f"{encoded}\t{text_path}\t{info.st_dev}\t{info.st_ino}")
except BaseException:
    cleanup()
    raise
' "$requested_directory" "$basename"
	return "$ANVIL_MCP_RUN_STATUS"
}

# Stage one already-validated large request in the canonical transaction
# directory.  The helper is bounded and runs before stateful dispatch.  It
# writes one sequence-unique 0600 file and returns
# BASE64-PATH<TAB>CANONICAL-PATH only after exact-size validation.
anvil_mcp_stage_request() {
	local request="$1" basename="$2" directory="$3"
	anvil_mcp_run_bounded "$ANVIL_MCP_REQUEST_PARSE_TIMEOUT" \
		merge input "$request" stage-request "$ANVIL_MCP_PYTHON" -I -S -c '
import base64
import os
import signal
import stat
import sys

directory = os.path.abspath(sys.argv[1])
basename = sys.argv[2]
maximum = int(sys.argv[3])
raw = sys.stdin.buffer.read(maximum + 1)
if len(raw) > maximum:
    raise SystemExit(65)
pieces = basename.split(".")
if not (
    len(pieces) == 3
    and pieces[0] == "request"
    and pieces[1].isdigit()
    and pieces[2] == "json"
):
    raise SystemExit(64)

directory_stat = os.lstat(directory)
if (
    os.path.realpath(directory) != directory
    or not stat.S_ISDIR(directory_stat.st_mode)
    or directory_stat.st_uid != os.geteuid()
    or stat.S_IMODE(directory_stat.st_mode) != 0o700
):
    raise SystemExit(73)

path = os.path.join(directory, basename)
flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL
if hasattr(os, "O_NOFOLLOW"):
    flags |= os.O_NOFOLLOW
descriptor = -1
created = False

def interrupted(_signum, _frame):
    raise TimeoutError("request staging interrupted")

signal.signal(signal.SIGTERM, interrupted)
try:
    descriptor = os.open(path, flags, 0o600)
    created = True
    file_stat = os.fstat(descriptor)
    if (
        not stat.S_ISREG(file_stat.st_mode)
        or file_stat.st_uid != os.geteuid()
        or file_stat.st_nlink != 1
        or stat.S_IMODE(file_stat.st_mode) != 0o600
        or file_stat.st_size != 0
    ):
        raise PermissionError("unsafe request staging file")
    with os.fdopen(descriptor, "wb") as stream:
        descriptor = -1
        stream.write(raw)
        stream.flush()
        final_stat = os.fstat(stream.fileno())
        if (
            not stat.S_ISREG(final_stat.st_mode)
            or final_stat.st_uid != os.geteuid()
            or final_stat.st_nlink != 1
            or stat.S_IMODE(final_stat.st_mode) != 0o600
            or final_stat.st_size != len(raw)
        ):
            raise PermissionError("request staging size or identity changed")
    text_path = os.fsdecode(path)
    if any(character in text_path for character in "\t\r\n"):
        raise SystemExit(64)
    encoded = base64.b64encode(os.fsencode(path)).decode("ascii")
    print(f"{encoded}\t{text_path}")
except BaseException:
    if descriptor >= 0:
        os.close(descriptor)
    if created:
        try:
            os.unlink(path)
        except OSError:
            pass
    raise
' "$directory" "$basename" "$ANVIL_MCP_MAX_REQUEST_BYTES"
	return "$ANVIL_MCP_RUN_STATUS"
}

anvil_mcp_cleanup_all_staged() {
	local directory="${ANVIL_MCP_TRANSACTION_DIRECTORY:-$ANVIL_MCP_REQUEST_DIRECTORY}"
	anvil_mcp_run_bounded "$ANVIL_MCP_REQUEST_PARSE_TIMEOUT" \
		merge null "" cleanup-staged "$ANVIL_MCP_PYTHON" -I -S -c '
import errno
import os
import stat
import sys

directory = os.path.abspath(sys.argv[1])
try:
    info = os.lstat(directory)
except FileNotFoundError:
    raise SystemExit(0)
if (
    os.path.realpath(directory) != directory
    or not stat.S_ISDIR(info.st_mode)
    or info.st_uid != os.geteuid()
    or stat.S_IMODE(info.st_mode) != 0o700
):
    raise SystemExit(74)

def generation_file(name, prefix):
    pieces = name.split(".")
    return (
        len(pieces) == 3
        and pieces[0] == prefix
        and pieces[1].isdigit()
        and pieces[2] == "json"
    )

while True:
    try:
        names = os.listdir(directory)
    except FileNotFoundError:
        raise SystemExit(0)
    for name in names:
        allowed = (
            generation_file(name, "request")
            or generation_file(name, "response")
            or generation_file(name, "proof")
            or name.startswith(".response-tmp.")
        )
        if not allowed:
            raise SystemExit(74)
        path = os.path.join(directory, name)
        try:
            entry = os.lstat(path)
        except FileNotFoundError:
            continue
        if stat.S_ISDIR(entry.st_mode):
            raise SystemExit(74)
        try:
            os.unlink(path)
        except FileNotFoundError:
            pass
    if names:
        continue
    try:
        os.rmdir(directory)
        break
    except FileNotFoundError:
        break
    except OSError as error:
        if error.errno not in (errno.EEXIST, errno.ENOTEMPTY):
            raise
' "$directory"
	return "$ANVIL_MCP_RUN_STATUS"
}

trap 'anvil_mcp_exit_cleanup' EXIT
ANVIL_MCP_CLEANUP_READY=1

# Emit a correlated at-most-once error.  Notifications remain silent;
# malformed input receives a protocol error with id null.
anvil_mcp_synthetic_error() {
	local request_kind="$1"
	local request_id="$2"
	local framed="$3"
	local phase="$4"
	local dispatched="$5"
	local client_rc="$6"
	local code message response

	case "$request_kind" in
	notification)
		ANVIL_MCP_RESPONSE_PENDING=0
		return 0
		;;
	parse-error)
		request_id=null
		phase=parse
		dispatched=false
		code=-32700
		message="invalid JSON-RPC input before dispatch"
		;;
	invalid-request)
		request_id=null
		phase=parse
		dispatched=false
		code=-32600
		message="invalid JSON-RPC request before dispatch"
		;;
	request)
		code=-32603
		case "$phase" in
		readiness) message="daemon readiness probe failed before dispatch" ;;
		capture) message="bridge capture setup failed before dispatch" ;;
		stage) message="large request staging failed before dispatch" ;;
		*) message="daemon response was ambiguous after one dispatch" ;;
		esac
		;;
	*)
		request_id=null
		phase=parse
		dispatched=false
		code=-32603
		message="bounded JSON-RPC metadata parsing failed"
		;;
	esac
	printf -v response '{"jsonrpc":"2.0","id":%s,"error":{"code":%s,"message":"Bridge synthetic error: %s","data":{"phase":"%s","dispatched":%s,"replayed":false,"emacsclientRc":%s}}}' \
		"$request_id" "$code" "$message" "$phase" "$dispatched" "$client_rc"
	if [ "$dispatched" = "false" ]; then
		mcp_debug_log "SYNTH-ERROR" "id=$request_id phase=$phase dispatched=false replayed=false rc=$client_rc"
	fi
	anvil_mcp_emit_response "$framed" "$response"
}

# Process input and print response.  Idle stdin is intentionally unbounded,
# but one absolute request deadline starts as soon as the first byte arrives.
mcp_debug_log "READY" "stdio request loop"
while :; do
	anvil_mcp_finish_pending_termination
	_anvil_first_byte=""
	if ! LC_ALL=C IFS= read -r -d '' -n 1 _anvil_first_byte; then
		anvil_mcp_finish_pending_termination
		break
	fi
	anvil_mcp_finish_pending_termination
	_anvil_frame_deadline=$((SECONDS + ANVIL_MCP_FRAME_READ_TIMEOUT))
	if [ -z "$_anvil_first_byte" ]; then
		mcp_debug_log "FRAMING-ERROR" "phase=first-byte byte=nul"
		exit 65
	elif [ "$_anvil_first_byte" = $'\n' ]; then
		line=""
	else
		if ! anvil_mcp_read_binary_line \
			"$_anvil_frame_deadline" "$ANVIL_MCP_MAX_REQUEST_BYTES"; then
			mcp_debug_log "FRAMING-ERROR" "phase=first-line"
			exit 65
		fi
		line="${_anvil_first_byte}${ANVIL_MCP_BINARY_LINE}"
	fi

	# T71: detect framing.  An MCP framed request begins with
	# `Content-Length:' (case-insensitive); legacy line-delimited
	# requests begin with `{'.
	# Strip CR for cross-platform safety.
	_anvil_first_line="${line%$'\r'}"
	_anvil_framed=0
	if [[ "$_anvil_first_line" =~ ^[Cc][Oo][Nn][Tt][Ee][Nn][Tt]-[Ll][Ee][Nn][Gg][Tt][Hh]: ]]; then
		_anvil_framed=1
		mcp_debug_log "FRAMING" "Content-Length detected"
		# Re-read full framed message; reuse the already-consumed line.
		if anvil_mcp_read_framed_message \
			"$_anvil_first_line" "$_anvil_frame_deadline"; then
			line=$ANVIL_MCP_FRAME_BODY
		else
			_anvil_frame_rc=$?
			mcp_debug_log "FRAMING-ERROR" \
				"rc=$_anvil_frame_rc first=$_anvil_first_line"
			# The header and some body bytes may already be consumed.  Continuing
			# would reinterpret a delayed remainder as another request.
			exit 65
		fi
	fi

	# Log the incoming request
	mcp_debug_log "REQUEST" "bytes=${#line} head=${line:0:160}"

	# Parse top-level metadata before touching Emacs.  This gives error paths a
	# correct correlation id and lets initialize use its shorter startup cap.
	set +e
	anvil_mcp_request_metadata \
		"$line" "$ANVIL_MCP_TRANSACTION_DIRECTORY"
	_anvil_metadata_rc=$?
	_anvil_metadata=$ANVIL_MCP_RUN_OUTPUT
	_anvil_metadata=${_anvil_metadata%$'\n'}
	_anvil_metadata=${_anvil_metadata%$'\r'}
	set -e
	if [ "$_anvil_metadata_rc" -ne 0 ] \
		&& [ -n "$ANVIL_MCP_TRANSACTION_DIRECTORY" ]; then
		# The current frame is consumed, but retired-generation cleanup did
		# not converge.  Closing is the only fail-closed continuation.
		exit 74
	fi
	if [ "$_anvil_metadata_rc" -ne 0 ]; then
		anvil_mcp_synthetic_error unknown null "$_anvil_framed" \
			parse false "$_anvil_metadata_rc"
		continue
	fi
	# Split the parser record in one linear builtin pass.  Repeated glob-pattern
	# removal (`${record#*|}') is quadratic for large base64 request fields and
	# can peg Bash forever before the dispatch watchdog has started.  `read'
	# assigns all remaining fields to its final variable, preserving `|' inside
	# a JSON string request id.
	_anvil_request_kind=""
	_anvil_startup=""
	_anvil_request_mode=""
	_anvil_request_payload=""
	_anvil_request_size=""
	_anvil_request_id=""
	IFS='|' read -r \
		_anvil_request_kind _anvil_startup \
		_anvil_request_mode _anvil_request_payload \
		_anvil_request_size \
		_anvil_request_id <<<"$_anvil_metadata"
	case "$_anvil_request_kind" in
	request)
		if [ -z "$_anvil_request_id" ]; then
			anvil_mcp_synthetic_error unknown null "$_anvil_framed" \
				parse false 70
			continue
		fi
		;;
	notification)
		_anvil_request_id=null
		;;
	parse-error|invalid-request)
		anvil_mcp_synthetic_error "$_anvil_request_kind" null \
			"$_anvil_framed" parse false 0
		continue
		;;
	*)
		anvil_mcp_synthetic_error unknown null "$_anvil_framed" \
			parse false 70
		continue
		;;
	esac
	case "$_anvil_request_size" in
	""|*[!0-9]*)
		anvil_mcp_synthetic_error unknown null "$_anvil_framed" \
			parse false 70
		continue
		;;
	esac
	case "$_anvil_request_mode" in
	inline)
		case "$_anvil_request_payload" in
		""|*[!A-Za-z0-9+/=]*)
			anvil_mcp_synthetic_error unknown null "$_anvil_framed" \
				parse false 70
			continue
			;;
		esac
		;;
	file) ;;
	*)
		anvil_mcp_synthetic_error unknown null "$_anvil_framed" \
			parse false 70
		continue
		;;
	esac

	# A harmless readiness probe is the only expression this bridge may replay.
	# Once it succeeds, the JSON-RPC expression below is dispatched exactly once.
	probe_output=""
	set +e
	anvil_emacsclient_probe_retry -- \
		${SOCKET_OPTIONS[@]+"${SOCKET_OPTIONS[@]}"} \
		-e "$ANVIL_EMACSCLIENT_READY_EXPR"
	_anvil_probe_rc=$?
	probe_output=$ANVIL_EMACSCLIENT_PROBE_OUTPUT
	set -e
	if [ "$_anvil_probe_rc" -ne 0 ]; then
		anvil_mcp_log_probe_stderr "PROBE-STDERR" "$probe_output"
	fi
	if [ "$_anvil_probe_rc" -ne 0 ]; then
		anvil_mcp_synthetic_error \
			"$_anvil_request_kind" "$_anvil_request_id" \
			"$_anvil_framed" readiness false "$_anvil_probe_rc"
		continue
	fi

	# Dispatch the potentially stateful request exactly once.  Initialize has
	# a shorter cap so daemon readiness plus initialization remains inside the
	# client startup envelope; ordinary tools retain the full dispatch budget.
	_anvil_dispatch_timeout="$ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT"
	if [ "$_anvil_startup" = "1" ]; then
		_anvil_dispatch_timeout="$ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT"
	fi
	if ! anvil_mcp_capture_begin; then
		anvil_mcp_synthetic_error \
			"$_anvil_request_kind" "$_anvil_request_id" \
			"$_anvil_framed" capture false 74
		continue
	fi

	# Each request gets one precommitted response inode.  The parent bridge opens
	# FD 6 before dispatch; bounded children close it, so descendants cannot
	# prolong response custody.
	ANVIL_MCP_REQUEST_SEQUENCE=$((ANVIL_MCP_REQUEST_SEQUENCE + 1))
	_anvil_response_basename="response.$ANVIL_MCP_REQUEST_SEQUENCE.json"
	_anvil_prepare_directory="${ANVIL_MCP_TRANSACTION_DIRECTORY:-$ANVIL_MCP_REQUEST_DIRECTORY}"
	set +e
	anvil_mcp_prepare_response \
		"$_anvil_prepare_directory" "$_anvil_response_basename"
	_anvil_response_prepare_rc=$?
	_anvil_response_record=$ANVIL_MCP_RUN_OUTPUT
	_anvil_response_record=${_anvil_response_record%$'\n'}
	_anvil_response_record=${_anvil_response_record%$'\r'}
	set -e
	# A helper failure is already authoritative.  Do not parse partial output
	# or replace its status with a local record-validation error.
	if [ "$_anvil_response_prepare_rc" -ne 0 ]; then
		exec 6<&- 2>/dev/null || :
		anvil_mcp_capture_finish
		anvil_mcp_synthetic_error \
			"$_anvil_request_kind" "$_anvil_request_id" \
			"$_anvil_framed" stage false "$_anvil_response_prepare_rc"
		exit 74
	fi
	_anvil_response_payload=${_anvil_response_record%%$'\t'*}
	_anvil_response_rest=${_anvil_response_record#*$'\t'}
	_anvil_response_stage_path=${_anvil_response_rest%%$'\t'*}
	_anvil_response_rest=${_anvil_response_rest#*$'\t'}
	_anvil_response_device=${_anvil_response_rest%%$'\t'*}
	_anvil_response_inode=${_anvil_response_rest#*$'\t'}
	if [ "$_anvil_response_payload" = "$_anvil_response_record" ] \
		|| [ "$_anvil_response_stage_path" = "$_anvil_response_rest" ] \
		|| [ "$_anvil_response_device" = "$_anvil_response_rest" ] \
		|| [[ "$_anvil_response_inode" == *$'\t'* ]]; then
		_anvil_response_prepare_rc=70
	fi
	case "$_anvil_response_payload" in
	""|*[!A-Za-z0-9+/=]*) _anvil_response_prepare_rc=70 ;;
	esac
	case "$_anvil_response_device" in
	""|*[!0-9]*|0[0-9]*) _anvil_response_prepare_rc=70 ;;
	esac
	case "$_anvil_response_inode" in
	""|*[!0-9]*|0|0[0-9]*) _anvil_response_prepare_rc=70 ;;
	esac
	_anvil_response_directory=${_anvil_response_stage_path%/*}
	_anvil_response_stage_basename=${_anvil_response_stage_path##*/}
	case "$_anvil_response_stage_basename" in
	".response-tmp.$ANVIL_MCP_REQUEST_SEQUENCE."*) ;;
	*) _anvil_response_prepare_rc=70 ;;
	esac
	if [ -z "$_anvil_response_directory" ] \
		|| [ "$_anvil_response_directory" = "$_anvil_response_stage_path" ]; then
		_anvil_response_prepare_rc=70
	elif [ -z "$ANVIL_MCP_TRANSACTION_DIRECTORY" ]; then
		ANVIL_MCP_TRANSACTION_DIRECTORY=$_anvil_response_directory
	elif [ "$_anvil_response_directory" != "$ANVIL_MCP_TRANSACTION_DIRECTORY" ]; then
		_anvil_response_prepare_rc=74
	fi
	_anvil_response_path="$ANVIL_MCP_TRANSACTION_DIRECTORY/$_anvil_response_basename"
	_anvil_response_proof_path="$ANVIL_MCP_TRANSACTION_DIRECTORY/proof.$ANVIL_MCP_REQUEST_SEQUENCE.json"
	if [ "$_anvil_response_prepare_rc" -eq 0 ] \
		&& { [ "$_anvil_response_directory" != "$ANVIL_MCP_TRANSACTION_DIRECTORY" ] \
			|| [ ! -f "$_anvil_response_stage_path" ] \
			|| [ -L "$_anvil_response_stage_path" ] \
			|| [ ! -O "$_anvil_response_stage_path" ] \
			|| [ -s "$_anvil_response_stage_path" ] \
			|| [ -e "$_anvil_response_path" ] \
			|| [ -L "$_anvil_response_path" ] \
			|| [ -e "$_anvil_response_proof_path" ] \
			|| [ -L "$_anvil_response_proof_path" ]; }; then
		_anvil_response_prepare_rc=74
	fi
	if [ "$_anvil_response_prepare_rc" -eq 0 ]; then
		if ! exec 6<"$_anvil_response_stage_path"; then
			_anvil_response_prepare_rc=74
		else
			set +e
			anvil_mcp_validate_response_fd \
				"$_anvil_response_device" "$_anvil_response_inode"
			_anvil_response_fd_rc=$?
			set -e
			if [ "$_anvil_response_fd_rc" -ne 0 ]; then
				exec 6<&-
				_anvil_response_prepare_rc=$_anvil_response_fd_rc
			else
			case "${OSTYPE-}" in
			linux*) ANVIL_MCP_FD_PATH_IDENTITY=1 ;;
			*) ANVIL_MCP_FD_PATH_IDENTITY=0 ;;
			esac
				if [ "$ANVIL_MCP_FD_PATH_IDENTITY" -eq 1 ] \
					&& ! [[ "$_anvil_response_stage_path" -ef /dev/fd/6 ]]; then
					exec 6<&-
					_anvil_response_prepare_rc=74
				fi
			fi
		fi
	fi
	if [ "$_anvil_response_prepare_rc" -ne 0 ]; then
		exec 6<&- 2>/dev/null || :
		anvil_mcp_capture_finish
		anvil_mcp_synthetic_error \
			"$_anvil_request_kind" "$_anvil_request_id" \
			"$_anvil_framed" stage false "$_anvil_response_prepare_rc"
		exit 74
	fi

	# Process JSON-RPC exactly once and publish its authenticated UTF-8 response.
	# Inline requests stay below exec argument limits; larger requests use the
	# same private transaction directory and are deleted before tool dispatch.
	not_ready_expr="\"$ANVIL_MCP_NOT_READY_SENTINEL\""
	_anvil_request_path=
	case "$_anvil_request_mode" in
	inline)
		base64_input=$_anvil_request_payload
		mcp_debug_log "BASE64-INPUT" "bytes=${#base64_input}"
		elisp_expr="(anvil-server-process-jsonrpc-to-file (base64-decode-string \"$base64_input\") \"$SERVER_ID\" (decode-coding-string (base64-decode-string \"$_anvil_response_payload\") 'utf-8 t) $ANVIL_MCP_REQUEST_SEQUENCE $ANVIL_MCP_MAX_RESPONSE_BYTES $_anvil_response_device $_anvil_response_inode)"
		;;
	file)
		_anvil_request_basename="request.${ANVIL_MCP_REQUEST_SEQUENCE}.json"
		_anvil_expected_request_path="$ANVIL_MCP_TRANSACTION_DIRECTORY/$_anvil_request_basename"
		set +e
		anvil_mcp_stage_request \
			"$line" "$_anvil_request_basename" \
			"$ANVIL_MCP_TRANSACTION_DIRECTORY"
		_anvil_stage_rc=$?
		_anvil_request_record=$ANVIL_MCP_RUN_OUTPUT
		_anvil_request_record=${_anvil_request_record%$'\n'}
		_anvil_request_record=${_anvil_request_record%$'\r'}
		set -e
		_anvil_request_payload=${_anvil_request_record%%$'\t'*}
		_anvil_request_path=${_anvil_request_record#*$'\t'}
		if [ "$_anvil_request_payload" = "$_anvil_request_record" ]; then
			_anvil_stage_rc=70
		fi
		case "$_anvil_request_payload" in
		""|*[!A-Za-z0-9+/=]*) _anvil_stage_rc=70 ;;
		esac
		if [ "$_anvil_request_path" != "$_anvil_expected_request_path" ]; then
			_anvil_stage_rc=70
		fi
		if [ "$_anvil_stage_rc" -ne 0 ]; then
			anvil_mcp_capture_finish
			if ! anvil_mcp_retire_staged_request \
				"$_anvil_expected_request_path"; then
				exit 74
			fi
			anvil_mcp_synthetic_error \
				"$_anvil_request_kind" "$_anvil_request_id" \
				"$_anvil_framed" stage false "$_anvil_stage_rc"
			exec 6<&-
			continue
		fi
		mcp_debug_log "STAGED-INPUT" "bytes=$_anvil_request_size"
		line=
		# A same-event readiness failure must prove exact retirement; deletion
		# failure becomes an ambiguous dispatch rather than a retryable sentinel.
		not_ready_expr="(progn (let ((delete-by-moving-to-trash nil)) (delete-file (decode-coding-string (base64-decode-string \"$_anvil_request_payload\") 'utf-8 t))) $not_ready_expr)"
		elisp_expr="(let* ((anvil-request-file (decode-coding-string (base64-decode-string \"$_anvil_request_payload\") 'utf-8 t)) (anvil-response-file (decode-coding-string (base64-decode-string \"$_anvil_response_payload\") 'utf-8 t)) (delete-by-moving-to-trash nil) anvil-request) (unwind-protect (progn (setq anvil-request (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally anvil-request-file) (unless (= (buffer-size) $_anvil_request_size) (error \"Staged Anvil request size changed\")) (buffer-string))) (delete-file anvil-request-file) (setq anvil-request-file nil) (anvil-server-process-jsonrpc-to-file anvil-request \"$SERVER_ID\" anvil-response-file $ANVIL_MCP_REQUEST_SEQUENCE $ANVIL_MCP_MAX_RESPONSE_BYTES $_anvil_response_device $_anvil_response_inode)) (when anvil-request-file (ignore-errors (delete-file anvil-request-file)))))"
		;;
	esac
	# Close the probe/dispatch race inside the same Emacs server event.  The
	# sentinel proves the stateful handler was not entered.
	elisp_expr="(if $ANVIL_EMACSCLIENT_READY_EXPR $elisp_expr $not_ready_expr)"

	ANVIL_MCP_RESPONSE_PENDING=1
	set +e
	anvil_emacsclient_dispatch_once \
		"$_anvil_dispatch_timeout" -- \
		${SOCKET_OPTIONS[@]+"${SOCKET_OPTIONS[@]}"} \
		-e "$elisp_expr"
	_anvil_client_rc=$?
	dispatch_output=$ANVIL_MCP_RUN_OUTPUT
	set -e
	anvil_mcp_capture_finish

	# Retire sensitive staged request bytes before marker parsing or stdout.
	# Missing is success because Emacs deletes the unique path before callbacks.
	if [ "$_anvil_request_mode" = "file" ]; then
		set +e
		anvil_mcp_retire_staged_request "$_anvil_request_path"
		_anvil_request_retire_rc=$?
		set -e
		if [ "$_anvil_request_retire_rc" -ne 0 ]; then
			exit 74
		fi
	fi

	# The marker is intentionally tiny, but normalize the same line endings and
	# Lisp string quotes accepted by lifecycle calls.
	dispatch_output="${dispatch_output//$'\r'/}"
	dispatch_output="${dispatch_output//$'\n'/}"
	if [ "${#dispatch_output}" -ge 2 ] \
		&& [[ "$dispatch_output" == \"* && "$dispatch_output" == *\" ]]; then
		dispatch_output="${dispatch_output:1:${#dispatch_output}-2}"
	fi

	if [ "$dispatch_output" = "$ANVIL_MCP_NOT_READY_SENTINEL" ]; then
		exec 6<&-
		anvil_mcp_synthetic_error \
			"$_anvil_request_kind" "$_anvil_request_id" \
			"$_anvil_framed" readiness false 75
		continue
	fi

	# The generation-bound response file is authoritative.  A valid marker adds
	# an exact size check; lost marker transport cannot alias another request.
	_anvil_expected_response_size=
	case "$dispatch_output" in
	"$ANVIL_MCP_RESPONSE_STAGED_PREFIX$ANVIL_MCP_REQUEST_SEQUENCE:"*)
		_anvil_expected_response_size=${dispatch_output#"$ANVIL_MCP_RESPONSE_STAGED_PREFIX$ANVIL_MCP_REQUEST_SEQUENCE:"}
		case "$_anvil_expected_response_size" in
		""|*[!0-9]*|0[0-9]*) _anvil_expected_response_size= ;;
		*)
			if [ "${#_anvil_expected_response_size}" -gt \
				"${#ANVIL_MCP_MAX_RESPONSE_BYTES}" ] \
				|| { [ "${#_anvil_expected_response_size}" -eq \
					"${#ANVIL_MCP_MAX_RESPONSE_BYTES}" ] \
					&& [[ "$_anvil_expected_response_size" > \
						"$ANVIL_MCP_MAX_RESPONSE_BYTES" ]]; }; then
				_anvil_expected_response_size=
			fi
			;;
		esac
		;;
	esac
	set +e
	anvil_mcp_read_staged_response \
		"$_anvil_response_path" "$_anvil_response_proof_path" \
		"$_anvil_expected_response_size"
	_anvil_response_read_rc=$?
	set -e
	if [ "$_anvil_response_read_rc" -eq 0 ]; then
		set +e
		anvil_mcp_emit_staged_response \
			"$_anvil_framed" "$_anvil_request_kind"
		_anvil_response_emit_rc=$?
		set -e
		if [ "$_anvil_response_emit_rc" -eq 0 ]; then
			continue
		fi
		# Status 65 is detected before stdout; all other failures may follow a
		# partial frame, so close without appending another protocol object.
		if [ "$_anvil_response_emit_rc" -ne 65 ]; then
			exit 74
		fi
	fi

	_anvil_failure_rc=$_anvil_client_rc
	if [ "$_anvil_failure_rc" -eq 0 ]; then
		_anvil_failure_rc=70
	fi
	anvil_mcp_synthetic_error \
		"$_anvil_request_kind" "$_anvil_request_id" \
		"$_anvil_framed" dispatch true "$_anvil_failure_rc"
	# Stateful dispatch is never replayed.  Unique request bytes are already
	# retired; this generation's response remains private until cleanup.
	continue
done

# Stop MCP if stop function is provided.  As with init, readiness may
# retry but the potentially stateful stop expression is invoked at most once.
anvil_mcp_finish_pending_termination
if [ -n "$STOP_FUNCTION" ]; then
	mcp_debug_log "INFO" "Stopping MCP with function: $STOP_FUNCTION"
	mcp_debug_log "STOP-CALL" "readiness probe, then one emacsclient -e ($STOP_FUNCTION)"

	stop_probe_output=""
	set +e
	anvil_emacsclient_probe_retry -- \
		${SOCKET_OPTIONS[@]+"${SOCKET_OPTIONS[@]}"} \
		-e "$ANVIL_EMACSCLIENT_READY_EXPR"
	STOP_READY_RC=$?
	stop_probe_output=$ANVIL_EMACSCLIENT_PROBE_OUTPUT
	STOP_RC=$STOP_READY_RC
	set -e
	if [ "$STOP_READY_RC" -ne 0 ]; then
		anvil_mcp_log_probe_stderr "STOP-PROBE-STDERR" "$stop_probe_output"
	fi
	mcp_debug_log "STOP-READY-RC" "$STOP_READY_RC"

	if [ "$STOP_READY_RC" -eq 0 ]; then
		if anvil_mcp_capture_begin; then
			ANVIL_MCP_RESPONSE_PENDING=1
			set +e
			anvil_emacsclient_dispatch_once \
				"$ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT" -- \
				${SOCKET_OPTIONS[@]+"${SOCKET_OPTIONS[@]}"} \
				-e "(if $ANVIL_EMACSCLIENT_READY_EXPR (progn ($STOP_FUNCTION) \"$ANVIL_MCP_LIFECYCLE_COMPLETE\") \"$ANVIL_MCP_NOT_READY_SENTINEL\")"
			STOP_RC=$?
			stop_dispatch_output=$ANVIL_MCP_RUN_OUTPUT
			set -e
			anvil_mcp_capture_finish
			ANVIL_MCP_RESPONSE_PENDING=0
			while :; do
				case "$stop_dispatch_output" in
				*$'\n') stop_dispatch_output=${stop_dispatch_output%$'\n'} ;;
				*$'\r') stop_dispatch_output=${stop_dispatch_output%$'\r'} ;;
				*) break ;;
				esac
			done
			if [ "${#stop_dispatch_output}" -ge 2 ] \
				&& [[ "$stop_dispatch_output" == \"* && "$stop_dispatch_output" == *\" ]]; then
				stop_dispatch_output="${stop_dispatch_output:1:${#stop_dispatch_output}-2}"
			fi
			if [ "$STOP_RC" -eq 0 ] \
				&& [ "$stop_dispatch_output" = "$ANVIL_MCP_NOT_READY_SENTINEL" ]; then
				STOP_RC=75
			elif [ "$STOP_RC" -eq 0 ] \
				&& [ "$stop_dispatch_output" != "$ANVIL_MCP_LIFECYCLE_COMPLETE" ]; then
				STOP_RC=70
			fi
		else
			STOP_RC=74
			mcp_debug_log "STOP-CAPTURE" "failed before dispatch rc=$STOP_RC"
		fi
	fi
	mcp_debug_log "STOP-RC" "$STOP_RC"
else
	mcp_debug_log "INFO" "Skipping stop function call (none provided)"
fi
anvil_mcp_finish_pending_termination
