#!/usr/bin/env bash
# test-install-container.sh --- runs inside the test container
#
# Invariants this test enforces on install.sh:
#   1. Post-install state is correct (init.el / stdio.sh / daemon feature /
#      ~/.claude.json entry).
#   2. Running install.sh twice is idempotent — no duplicated init.el lines,
#      no disturbed daemon state.
#   3. A pre-existing mcpServers entry in ~/.claude.json is preserved.
#
# Expected environment:
#   /anvil          — read-only mount of the anvil.el repo checkout
#   ANVIL_TEST_BRANCH — the branch to clone (defaults to current HEAD if
#                       the mount is a full repo)
#
# Exit codes:
#   0 all pass
#   1 assertion failed
#   2 setup failure (container env)

set -euo pipefail

# rootless podman / docker-desktop bind-mounts arrive with host UID, which
# trips git's safe.directory check inside the container.  Globally allow
# before any git clone runs.
git config --global --add safe.directory '*' || true

# ---------------------------------------------------------------------------
# Small test helpers
# ---------------------------------------------------------------------------

pass_count=0
fail_count=0

green()  { printf '\033[1;32m%s\033[0m' "$1"; }
red()    { printf '\033[1;31m%s\033[0m' "$1"; }
yellow() { printf '\033[1;33m%s\033[0m' "$1"; }

assert() {
  # assert "<description>" <command...>
  #
  # Runs the command; treats any non-zero exit as failure.  The command
  # must not contain shell metacharacters — wrap with bash -c if needed.
  local desc="$1"; shift
  if "$@" >/tmp/assert.out 2>&1; then
    printf '  %s %s\n' "$(green "pass:")" "$desc"
    pass_count=$((pass_count + 1))
  else
    printf '  %s %s\n' "$(red "FAIL:")" "$desc"
    sed 's/^/      /' /tmp/assert.out
    fail_count=$((fail_count + 1))
  fi
}

section() {
  printf '\n%s %s\n' "$(yellow "==")" "$1"
}

# ---------------------------------------------------------------------------
# Resolve branch
# ---------------------------------------------------------------------------

BRANCH="${ANVIL_TEST_BRANCH:-}"
if [ -z "$BRANCH" ]; then
  if [ -d /anvil/.git ]; then
    BRANCH="$(git -C /anvil rev-parse --abbrev-ref HEAD)"
  else
    BRANCH=master
  fi
fi

echo "test-install-container.sh starting"
echo "  branch: $BRANCH"
echo "  HOME:   $HOME"
echo "  emacs:  $(emacs --version 2>/dev/null | head -1)"

# ---------------------------------------------------------------------------
# Step 1 — clean run
# ---------------------------------------------------------------------------

section 'phase 1 — clean install'

bash /anvil/install.sh \
  --yes --skip-emacs \
  --repo /anvil \
  --branch "$BRANCH" \
  --prefix "$HOME/.emacs.d/external-packages/anvil.el" \
  >/tmp/install.log 2>&1 || {
    red "installer exited non-zero"
    cat /tmp/install.log
    exit 2
  }

assert 'anvil-init.el exists'          test -f "$HOME/.emacs.d/anvil-init.el"
assert 'anvil-stdio.sh exists'         test -f "$HOME/.emacs.d/anvil-stdio.sh"
assert 'anvil-stdio.sh is executable'  test -x "$HOME/.emacs.d/anvil-stdio.sh"
assert 'init.el loads anvil-init'      grep -Fq 'anvil-init.el' "$HOME/.emacs.d/init.el"
assert 'anvil clone present'           test -f "$HOME/.emacs.d/external-packages/anvil.el/anvil.el"
assert 'claude.json exists'            test -f "$HOME/.claude.json"

assert 'daemon responds to emacsclient' \
  bash -c 'emacsclient -e "t" 2>&1 | grep -Fq t'

assert 'anvil feature loaded in daemon' \
  bash -c 'emacsclient -e "(featurep (quote anvil))" 2>/dev/null | grep -Fq t'

# Deeper probe: (featurep anvil) only tells us (require 'anvil) ran, not
# that anvil-server-start was callable.  This one fails loud if the
# autoload-cookie hack in anvil-init.el regresses.
assert 'anvil-server-start function bound' \
  bash -c 'emacsclient -e "(fboundp (quote anvil-server-start))" 2>/dev/null | grep -Fq t'

# Diagnostic dump on first phase (helps when a distro behaves differently)
if [ "${ANVIL_TEST_DEBUG:-0}" = "1" ]; then
  echo "    --- diagnostic dump ---"
  echo "    emacsclient -e t: $(emacsclient -e t 2>&1 || true)"
  echo "    anvil-init.el head:"
  head -20 "$HOME/.emacs.d/anvil-init.el" | sed 's/^/      /'
  echo "    emacs daemon log (last 30 lines):"
  find /tmp -maxdepth 3 -name 'emacs*' -print 2>/dev/null | head
fi

assert 'mcpServers.anvil registered' \
  python3 -c "
import json, sys, pathlib
cfg = json.loads(pathlib.Path('$HOME/.claude.json').read_text())
entry = cfg['mcpServers']['anvil']
assert entry['command'].endswith('anvil-stdio.sh'), entry
assert '--server-id=anvil' in entry['args'], entry['args']
"

# ---------------------------------------------------------------------------
# Step 2 — idempotency
# ---------------------------------------------------------------------------

section 'phase 2 — idempotent re-run'

bash /anvil/install.sh \
  --yes --skip-emacs \
  --repo /anvil \
  --branch "$BRANCH" \
  --prefix "$HOME/.emacs.d/external-packages/anvil.el" \
  >>/tmp/install.log 2>&1 || {
    red "second run exited non-zero"
    tail -30 /tmp/install.log
    exit 2
  }

assert 'init.el not duplicated' \
  bash -c 'test "$(grep -c anvil-init.el "$HOME/.emacs.d/init.el")" = 1'

assert 'anvil still loaded after re-run' \
  bash -c 'emacsclient -e "(featurep (quote anvil))" 2>/dev/null | grep -Fq t'

# ---------------------------------------------------------------------------
# Step 3 — preserve pre-existing mcpServers
# ---------------------------------------------------------------------------

section 'phase 3 — JSON merge preservation'

python3 - <<'PY'
import json, os, pathlib
path = pathlib.Path(os.environ['HOME']) / '.claude.json'
cfg = json.loads(path.read_text())
cfg.setdefault('mcpServers', {})['preexisting'] = {
    'command': '/fake/path', 'args': ['--foo']
}
cfg['user_preference'] = 'keep-me'
path.write_text(json.dumps(cfg, indent=2))
PY

bash /anvil/install.sh \
  --yes --skip-emacs \
  --repo /anvil \
  --branch "$BRANCH" \
  --prefix "$HOME/.emacs.d/external-packages/anvil.el" \
  >>/tmp/install.log 2>&1

assert 'preexisting mcpServers entry preserved' \
  python3 -c "
import json, pathlib
cfg = json.loads(pathlib.Path('$HOME/.claude.json').read_text())
assert 'preexisting' in cfg['mcpServers'], list(cfg['mcpServers'])
assert cfg['mcpServers']['preexisting']['command'] == '/fake/path'
"

assert 'top-level user_preference preserved' \
  python3 -c "
import json, pathlib
cfg = json.loads(pathlib.Path('$HOME/.claude.json').read_text())
assert cfg.get('user_preference') == 'keep-me', cfg
"

# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------

echo
echo "----------------------------------------"
echo "pass: $pass_count  fail: $fail_count"
echo "----------------------------------------"

if [ "$fail_count" -ne 0 ]; then
  echo "installer log (tail):"
  tail -30 /tmp/install.log
  exit 1
fi
