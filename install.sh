#!/usr/bin/env bash
# install.sh --- One-shot installer for anvil.el MCP server (Linux / macOS)
#
# Goal: turn a machine with nothing but a shell into a working
# Claude-Code-to-Emacs MCP bridge in one command.
#
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/zawatton/anvil.el/master/install.sh | bash
#   bash install.sh [--skip-emacs] [--prefix DIR] [--branch REF] [--repo URL]
#                   [--server-id ID] [--dry-run] [--yes]
#
# Options:
#   --skip-emacs    Do not try to install Emacs (user promises it exists).
#   --prefix DIR    Where to clone anvil.el.
#                   Default: $HOME/.emacs.d/external-packages/anvil.el
#   --branch REF    Branch or tag to check out. Default: v0.3.1
#   --repo URL      Override the source repository URL (git clones from it
#                   verbatim — supports local paths and file:// for tests).
#                   Default: https://github.com/zawatton/anvil.el.git
#   --server-id ID  MCP server id registered in ~/.claude.json. Default: anvil
#   --dry-run       Print every action but do not execute.
#   --yes           Non-interactive: answer yes to confirmations.
#   --help          This help.

set -euo pipefail

# ---------------------------------------------------------------------------
# Defaults
# ---------------------------------------------------------------------------

ANVIL_REPO="https://github.com/zawatton/anvil.el.git"
ANVIL_BRANCH="v0.3.1"
ANVIL_PREFIX="$HOME/.emacs.d/external-packages/anvil.el"
ANVIL_SERVER_ID="anvil"
SKIP_EMACS=0
DRY_RUN=0
ASSUME_YES=0

EMACS_DIR="$HOME/.emacs.d"
CLAUDE_CONFIG="$HOME/.claude.json"

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

log()  { printf '\033[1;34m[anvil]\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33m[warn]\033[0m %s\n'  "$*" >&2; }
die()  { printf '\033[1;31m[error]\033[0m %s\n' "$*" >&2; exit 1; }

run() {
  # Run a command, respecting --dry-run.
  if [ "$DRY_RUN" -eq 1 ]; then
    printf '    + %s\n' "$*"
  else
    eval "$@"
  fi
}

confirm() {
  # confirm "prompt" — returns 0 if yes, 1 if no. Auto-yes under --yes.
  if [ "$ASSUME_YES" -eq 1 ]; then return 0; fi
  local reply
  printf '%s [y/N] ' "$1"
  read -r reply
  case "$reply" in
    y|Y|yes|YES) return 0 ;;
    *)           return 1 ;;
  esac
}

usage() {
  sed -n '4,20p' "$0" | sed 's/^# \{0,1\}//'
  exit 0
}

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

while [ $# -gt 0 ]; do
  case "$1" in
    --skip-emacs)   SKIP_EMACS=1; shift ;;
    --prefix)       ANVIL_PREFIX="$2"; shift 2 ;;
    --prefix=*)     ANVIL_PREFIX="${1#--prefix=}"; shift ;;
    --branch)       ANVIL_BRANCH="$2"; shift 2 ;;
    --branch=*)     ANVIL_BRANCH="${1#--branch=}"; shift ;;
    --repo)         ANVIL_REPO="$2"; shift 2 ;;
    --repo=*)       ANVIL_REPO="${1#--repo=}"; shift ;;
    --server-id)    ANVIL_SERVER_ID="$2"; shift 2 ;;
    --server-id=*)  ANVIL_SERVER_ID="${1#--server-id=}"; shift ;;
    --dry-run)      DRY_RUN=1; shift ;;
    --yes|-y)       ASSUME_YES=1; shift ;;
    --help|-h)      usage ;;
    *) die "Unknown argument: $1 (try --help)" ;;
  esac
done

# ---------------------------------------------------------------------------
# Platform detection
# ---------------------------------------------------------------------------

UNAME_S="$(uname -s)"
case "$UNAME_S" in
  Linux*)  OS=linux ;;
  Darwin*) OS=macos ;;
  *) die "Unsupported OS: $UNAME_S (this installer handles Linux/macOS; see install.ps1 for Windows)" ;;
esac

detect_pkg_mgr() {
  if   command -v brew   >/dev/null 2>&1; then echo brew
  elif command -v apt    >/dev/null 2>&1; then echo apt
  elif command -v dnf    >/dev/null 2>&1; then echo dnf
  elif command -v pacman >/dev/null 2>&1; then echo pacman
  elif command -v zypper >/dev/null 2>&1; then echo zypper
  else echo unknown
  fi
}

PKG_MGR="$(detect_pkg_mgr)"

# ---------------------------------------------------------------------------
# Step banner
# ---------------------------------------------------------------------------

log "anvil.el installer"
log "  OS:         $OS"
log "  pkg mgr:    $PKG_MGR"
log "  prefix:     $ANVIL_PREFIX"
log "  repo:       $ANVIL_REPO"
log "  branch:     $ANVIL_BRANCH"
log "  server-id:  $ANVIL_SERVER_ID"
log "  skip-emacs: $SKIP_EMACS"
log "  dry-run:    $DRY_RUN"
echo

# ---------------------------------------------------------------------------
# 1. Prerequisite tools
# ---------------------------------------------------------------------------

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || die "Required command not found: $1"
}

log "step 1/7: checking prerequisites"
require_cmd git
require_cmd python3 || require_cmd python
require_cmd uname

# ---------------------------------------------------------------------------
# 2. Emacs
# ---------------------------------------------------------------------------

install_emacs() {
  case "$PKG_MGR" in
    brew)   run "brew install emacs" ;;
    apt)    run "sudo apt update && sudo apt install -y emacs" ;;
    dnf)    run "sudo dnf install -y emacs" ;;
    pacman) run "sudo pacman -Sy --noconfirm emacs" ;;
    zypper) run "sudo zypper install -y emacs" ;;
    *) die "No supported package manager detected. Install Emacs manually and re-run with --skip-emacs." ;;
  esac
}

check_emacs_version() {
  local v
  v="$(emacs --version 2>/dev/null | head -1 | awk '{print $NF}')"
  case "$v" in
    28.*|29.*|3[0-9].*) log "  emacs $v detected (ok)" ;;
    *) warn "emacs $v is older than 28.2 — some anvil modules will not load" ;;
  esac
}

log "step 2/7: emacs"
if command -v emacs >/dev/null 2>&1; then
  check_emacs_version
elif [ "$SKIP_EMACS" -eq 1 ]; then
  die "Emacs not found and --skip-emacs was given. Install Emacs 28.2+ first."
else
  log "  Emacs not found — installing via $PKG_MGR"
  if ! confirm "Install Emacs via $PKG_MGR now? (may prompt for sudo)"; then
    die "Aborted. Re-run with --skip-emacs after installing Emacs yourself."
  fi
  install_emacs
  command -v emacs >/dev/null 2>&1 || die "Emacs install appeared to succeed but emacs is still not on PATH"
  check_emacs_version
fi

# ---------------------------------------------------------------------------
# 3. Clone anvil.el
# ---------------------------------------------------------------------------

log "step 3/7: clone anvil.el → $ANVIL_PREFIX"
if [ -d "$ANVIL_PREFIX/.git" ]; then
  log "  repo already present, updating"
  run "git -C '$ANVIL_PREFIX' fetch --tags origin"
  run "git -C '$ANVIL_PREFIX' checkout '$ANVIL_BRANCH'"
  run "git -C '$ANVIL_PREFIX' pull --ff-only origin '$ANVIL_BRANCH' || true"
else
  run "mkdir -p '$(dirname "$ANVIL_PREFIX")'"
  # --depth 1 speeds up the real (github) path but is ignored by git for
  # most local clones, which is fine.
  run "git clone --branch '$ANVIL_BRANCH' --depth 1 '$ANVIL_REPO' '$ANVIL_PREFIX'"
fi

# ---------------------------------------------------------------------------
# 4. Minimal init file
# ---------------------------------------------------------------------------

log "step 4/7: bootstrap ~/.emacs.d/anvil-init.el"
run "mkdir -p '$EMACS_DIR'"

ANVIL_INIT="$EMACS_DIR/anvil-init.el"
if [ "$DRY_RUN" -eq 0 ]; then
  cat >"$ANVIL_INIT" <<EOF
;;; anvil-init.el --- minimal anvil bootstrap -*- lexical-binding: t; -*-
;; Generated by install.sh on $(date '+%Y-%m-%d %H:%M:%S')
;; Regenerating is safe — this file carries no user state.

(add-to-list 'load-path "$ANVIL_PREFIX")
(require 'anvil)
;; anvil-server-commands holds anvil-server-start / -stop etc.  It is
;; autoload-cookied, but manual-install (plain add-to-list + require) does
;; not generate loaddefs, so we require it explicitly.
(require 'anvil-server-commands)
(anvil-enable)
(anvil-server-start)

(provide 'anvil-init)
;;; anvil-init.el ends here
EOF
  log "  wrote $ANVIL_INIT"
else
  printf '    + write %s\n' "$ANVIL_INIT"
fi

# Hook into init.el if present, create if not. Idempotent.
#
# Some distros (notably Fedora) pre-seed $HOME/.emacs with a skeleton —
# Emacs then loads ~/.emacs and ignores ~/.emacs.d/init.el.  We therefore
# write the hook into ~/.emacs.d/init.el always, and ALSO into ~/.emacs
# if it already exists (never create it, to respect user intent).
INIT_EL="$EMACS_DIR/init.el"
DOT_EMACS="$HOME/.emacs"
HOOK_LINE='(load (expand-file-name "anvil-init.el" user-emacs-directory))'

ensure_hook() {
  # ensure_hook <target-file> <create-if-missing 0|1>
  local target="$1" create="$2"
  if [ "$DRY_RUN" -eq 1 ]; then
    printf '    + ensure %s loads anvil-init.el\n' "$target"
    return 0
  fi
  if [ ! -f "$target" ]; then
    if [ "$create" = "1" ]; then
      printf ';;; %s --- generated by anvil installer\n%s\n' \
        "$(basename "$target")" "$HOOK_LINE" >"$target"
      log "  created $target"
    fi
    return 0
  fi
  if grep -Fq 'anvil-init.el' "$target"; then
    log "  $target already loads anvil-init (ok)"
  else
    printf '\n;; added by anvil installer\n%s\n' "$HOOK_LINE" >>"$target"
    log "  appended anvil-init hook to $target"
  fi
}

ensure_hook "$INIT_EL"  1   # ~/.emacs.d/init.el: create if missing
ensure_hook "$DOT_EMACS" 0  # ~/.emacs: only touch if the distro seeded it

# ---------------------------------------------------------------------------
# 5. Start / restart daemon
# ---------------------------------------------------------------------------

log "step 5/7: emacs daemon"
if emacsclient -e t >/dev/null 2>&1; then
  log "  daemon already running — loading anvil-init into existing daemon"
  # Non-destructive: require/enable/server-start are all idempotent, so
  # evaluating anvil-init.el in a live daemon is safe even if the user
  # has unrelated state there.
  run "emacsclient -e '(load \"$ANVIL_INIT\")' >/dev/null || true"
else
  log "  starting fresh daemon"
  run "emacs --daemon >/dev/null 2>&1 &"
  run "sleep 2"
fi

if [ "$DRY_RUN" -eq 0 ]; then
  if ! emacsclient -e '(featurep (quote anvil))' 2>/dev/null | grep -q t; then
    warn "anvil did not load in daemon. Inspect logs:"
    warn "  emacsclient -e '(progn (load \"$ANVIL_INIT\") t)'"
  else
    log "  anvil feature loaded in daemon"
  fi
fi

# ---------------------------------------------------------------------------
# 6. stdio bridge script
# ---------------------------------------------------------------------------

log "step 6/7: install stdio bridge"
STDIO_SRC="$ANVIL_PREFIX/anvil-stdio.sh"
STDIO_DST="$EMACS_DIR/anvil-stdio.sh"

# Under --dry-run the clone never happened, so skip the pre-flight check.
if [ "$DRY_RUN" -eq 0 ] && [ ! -f "$STDIO_SRC" ]; then
  die "Missing $STDIO_SRC (repo checkout looks incomplete)"
fi
run "cp '$STDIO_SRC' '$STDIO_DST'"
run "chmod 0755 '$STDIO_DST'"
log "  installed $STDIO_DST"

# ---------------------------------------------------------------------------
# 7. Register with Claude Code (~/.claude.json)
# ---------------------------------------------------------------------------

log "step 7/7: register MCP server in $CLAUDE_CONFIG"

# Use python for robust JSON edit — it ships with every modern macOS / Linux.
PYTHON="$(command -v python3 || command -v python)"

if [ "$DRY_RUN" -eq 0 ]; then
  cp -f "$CLAUDE_CONFIG" "$CLAUDE_CONFIG.anvil-backup" 2>/dev/null || true
  "$PYTHON" - "$CLAUDE_CONFIG" "$ANVIL_SERVER_ID" "$STDIO_DST" <<'PY'
import json, os, sys
cfg_path, server_id, script = sys.argv[1], sys.argv[2], sys.argv[3]
if os.path.exists(cfg_path):
    with open(cfg_path, "r", encoding="utf-8") as f:
        try:
            cfg = json.load(f)
        except json.JSONDecodeError:
            print(f"[error] {cfg_path} is not valid JSON — refusing to overwrite", file=sys.stderr)
            sys.exit(2)
else:
    cfg = {}
cfg.setdefault("mcpServers", {})
cfg["mcpServers"][server_id] = {
    "command": script,
    "args": [f"--server-id={server_id}"],
}
with open(cfg_path, "w", encoding="utf-8") as f:
    json.dump(cfg, f, indent=2, ensure_ascii=False)
    f.write("\n")
print(f"[anvil] registered mcpServers.{server_id} in {cfg_path}")
PY
else
  printf '    + write mcpServers.%s entry in %s\n' "$ANVIL_SERVER_ID" "$CLAUDE_CONFIG"
fi

# ---------------------------------------------------------------------------
# Done
# ---------------------------------------------------------------------------

echo
log "install complete"
log "  restart Claude Code (or run: claude mcp list) to pick up the new server."
log "  verify:  emacsclient -e '(anvil-server-list-tools)'"
log "  logs:    tail -f ~/.emacs.d/anvil-stdio.debug.log  (after setting"
log "           EMACS_MCP_DEBUG_LOG=~/.emacs.d/anvil-stdio.debug.log)"

if [ "$DRY_RUN" -eq 1 ]; then
  echo
  warn "DRY RUN — nothing was actually executed. Re-run without --dry-run."
fi
