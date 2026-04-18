#!/bin/bash
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

# Default values
INIT_FUNCTION=""
STOP_FUNCTION=""
SOCKET=""
SERVER_ID=""
EMACS_MCP_DEBUG_LOG=${EMACS_MCP_DEBUG_LOG:-""}

# Debug logging setup
if [ -n "$EMACS_MCP_DEBUG_LOG" ]; then
	# Verify log file is writable
	if ! touch "$EMACS_MCP_DEBUG_LOG" 2>/dev/null; then
		echo "Error: Cannot write to debug log file: $EMACS_MCP_DEBUG_LOG" >&2
		exit 1
	fi

	# Helper function for debug logging
	mcp_debug_log() {
		local direction="$1"
		local message="$2"
		local timestamp
		timestamp=$(date "+%Y-%m-%d %H:%M:%S")
		echo "[$timestamp] [$$] MCP-${direction}: ${message}" >>"$EMACS_MCP_DEBUG_LOG"
	}

	mcp_debug_log "INFO" "Debug logging enabled"

	# Log version/path so future failure reports can tell which copy of
	# anvil-stdio.sh was actually executed (opencode/zed MCP configs
	# sometimes point to a checkout other than the user's working copy).
	_anvil_script_path="$0"
	_anvil_script_dir="$(cd "$(dirname "$0")" 2>/dev/null && pwd || echo '?')"
	_anvil_git_sha="$(git -C "$_anvil_script_dir" rev-parse --short HEAD 2>/dev/null || echo 'no-git')"
	_anvil_mtime="$(stat -c %Y "$_anvil_script_path" 2>/dev/null || stat -f %m "$_anvil_script_path" 2>/dev/null || echo 'unknown')"
	mcp_debug_log "INFO" "anvil-stdio.sh path=$_anvil_script_path git=$_anvil_git_sha mtime=$_anvil_mtime"
	mcp_debug_log "INFO" "tooling bash=$(command -v bash || echo '?') sed=$(command -v sed || echo '?') tr=$(command -v tr || echo '?') base64=$(command -v base64 || echo '?') emacsclient=$(command -v emacsclient || echo '?')"
else
	# No-op function when debug logging is disabled
	mcp_debug_log() {
		:
	}
fi

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

# Initialize MCP if init function is provided
if [ -n "$INIT_FUNCTION" ]; then
	# shellcheck disable=SC2124
	readonly INIT_CMD="emacsclient ${SOCKET_OPTIONS[@]+"${SOCKET_OPTIONS[@]}"} -e \"($INIT_FUNCTION)\""

	mcp_debug_log "INIT-CALL" "$INIT_CMD"

	# Execute the command and capture output and return code
	init_stderr_file="/tmp/mcp-init-stderr.$$-$(date +%s%N)"
	mcp_debug_log "INIT-STDERR-FILE" "$init_stderr_file"
	INIT_OUTPUT=$(eval "$INIT_CMD" 2>"$init_stderr_file")
	INIT_RC=$?

	# Log results
	mcp_debug_log "INIT-RC" "$INIT_RC"
	mcp_debug_log "INIT-OUTPUT" "$INIT_OUTPUT"
	if [ -s "$init_stderr_file" ]; then
		mcp_debug_log "INIT-STDERR" "$(cat "$init_stderr_file")"
	fi
	rm -f "$init_stderr_file"
else
	mcp_debug_log "INFO" "Skipping init function call (none provided)"
fi

# Process input and print response
while read -r line; do
	# Log the incoming request
	mcp_debug_log "REQUEST" "$line"

	# Base64 encode the raw JSON to avoid emacsclient transport issues
	# with a specific combination of length, UTF-8 characters, and quoting
	# that occurs in Test 5 with the Lithuanian letter 'ą'
	base64_input=$(echo -n "$line" | base64)
	mcp_debug_log "BASE64-INPUT" "$base64_input"

	# Process JSON-RPC request and return the result with proper UTF-8 encoding
	# Encode the response to base64 to avoid any character encoding issues
	# Handle nil responses from notifications by converting to empty string
	elisp_expr="(base64-encode-string (encode-coding-string (or (anvil-server-process-jsonrpc (base64-decode-string \"$base64_input\") \"$SERVER_ID\") \"\") 'utf-8 t) t)"

	# Get response from emacsclient - capture stderr for debugging
	stderr_file="/tmp/mcp-stderr.$$-$(date +%s%N)"
	base64_response=$(emacsclient "${SOCKET_OPTIONS[@]+"${SOCKET_OPTIONS[@]}"}" -e "$elisp_expr" 2>"$stderr_file")

	# Check for stderr output
	if [ -s "$stderr_file" ]; then
		mcp_debug_log "EMACSCLIENT-STDERR" "$(cat "$stderr_file")"
	fi
	rm -f "$stderr_file"

	mcp_debug_log "BASE64-RESPONSE" "$base64_response"

	# Handle the base64 response - first strip quotes if present
	if [[ "$base64_response" == \"* && "$base64_response" == *\" ]]; then
		# Remove the surrounding quotes
		base64_response="${base64_response:1:${#base64_response}-2}"
		# Unescape any quotes inside
		base64_response="${base64_response//\\\"/\"}"
	fi

	# Repair Windows MSYS frame-boundary corruption.
	# emacsclient.c uses a read buffer of BUFSIZ+1 bytes; on MSYS / mingw
	# stdio.h, BUFSIZ is 512.  The Emacs server's `server-reply-print'
	# splits its output into frames of up to `server-msg-size' (1024 by
	# default), so on Windows every frame larger than ~512 bytes overruns
	# the client's read buffer.  When that happens, the tail of the frame
	# loses its `-print-nonl ' prefix and emacsclient prints it as
	#   *ERROR*: Unknown message: <tail>
	# interleaved with the legitimate base64 payload.  Strip those
	# injection markers with `sed s///g' (POSIX BRE, so `\*' is
	# unambiguously a literal asterisk on every sed implementation --
	# unlike `awk' where `\*' in ERE is treated differently by gawk /
	# mawk / busybox / git-bash-bundled awk and may cause the strip to
	# silently fail).  Then remove ALL CR/LF bytes; frame boundaries
	# leave CRLF behind, and `tr' is trivially portable.  `--ignore-
	# garbage' on the base64 decoder is NOT sufficient on its own: the
	# marker text "ERROR Unknown message" is 15 base64-alphabet bytes
	# that the decoder happily consumes as payload, throwing off the
	# multiple-of-4 requirement and yielding `invalid input' under
	# `set -e -o pipefail'.  Stripping the marker textually, before
	# decoding, is what makes Windows work at all.
	# (No-op on Linux/macOS where one frame fits in one read.)
	base64_response=$(printf '%s' "$base64_response" \
		| sed 's/\*ERROR\*: Unknown message: //g' \
		| tr -d '\r\n')

	# Diagnostic: confirm strip actually happened.  If markers survived,
	# base64 -d will almost certainly fail below, and we want the log to
	# say so out loud rather than just ending at BASE64-RESPONSE.
	_anvil_marker_survivors=$(printf '%s' "$base64_response" | grep -c '\*ERROR\*' || true)
	mcp_debug_log "INFO" "after-strip len=${#base64_response} markers=$_anvil_marker_survivors"

	# Decode the base64 content (lenient against stray non-base64 bytes).
	# Capture rc/stderr explicitly so that a decode failure is logged
	# instead of silently killing the script under `set -e -o pipefail'.
	_anvil_decode_err="/tmp/mcp-decode-err.$$-$(date +%s%N)"
	set +e
	formatted_response=$(printf '%s' "$base64_response" | base64 -d --ignore-garbage 2>"$_anvil_decode_err")
	_anvil_decode_rc=$?
	set -e
	if [ "$_anvil_decode_rc" != 0 ]; then
		mcp_debug_log "DECODE-ERROR" "rc=$_anvil_decode_rc stderr=$(cat "$_anvil_decode_err" 2>/dev/null | tr '\n' ' ') input_head=${base64_response:0:160}"
	fi
	rm -f "$_anvil_decode_err"

	mcp_debug_log "RESPONSE" "$formatted_response"

	# Only output non-empty responses
	if [ -n "$formatted_response" ]; then
		# Output the response
		echo "$formatted_response"
	fi
done

# Stop MCP if stop function is provided
if [ -n "$STOP_FUNCTION" ]; then
	mcp_debug_log "INFO" "Stopping MCP with function: $STOP_FUNCTION"

	# shellcheck disable=SC2124
	readonly STOP_CMD="emacsclient ${SOCKET_OPTIONS[@]+"${SOCKET_OPTIONS[@]}"} -e \"($STOP_FUNCTION)\""

	mcp_debug_log "STOP-CALL" "$STOP_CMD"

	# Execute the command and capture output and return code
	STOP_OUTPUT=$(eval "$STOP_CMD" 2>&1)
	STOP_RC=$?

	# Log results
	mcp_debug_log "STOP-RC" "$STOP_RC"
	mcp_debug_log "STOP-OUTPUT" "$STOP_OUTPUT"
else
	mcp_debug_log "INFO" "Skipping stop function call (none provided)"
fi
