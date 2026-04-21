#!/usr/bin/env bash
# test-install.sh --- orchestrator for install.sh verification
#
# Builds each Dockerfile under scripts/dockerfiles/ into an image, then runs
# scripts/test-install-container.sh inside the container.  The repo
# checkout is bind-mounted read-only at /anvil, so the in-container
# installer clones from a local path (via --repo /anvil) and we can test
# unreleased branches without pushing.
#
# Usage:
#   scripts/test-install.sh              # run all distros
#   scripts/test-install.sh ubuntu       # single distro
#   scripts/test-install.sh ubuntu debian
#   scripts/test-install.sh --list       # list available distros
#   scripts/test-install.sh --no-cache   # force image rebuild
#
# Exit code: 0 if every requested distro passed, 1 otherwise.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
DOCKERFILE_DIR="$SCRIPT_DIR/dockerfiles"

# ---------------------------------------------------------------------------
# Arg parsing
# ---------------------------------------------------------------------------

NO_CACHE=0
LIST_ONLY=0
DISTROS=()

while [ $# -gt 0 ]; do
  case "$1" in
    --no-cache) NO_CACHE=1; shift ;;
    --list)     LIST_ONLY=1; shift ;;
    -h|--help)
      sed -n '3,17p' "$0" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    *) DISTROS+=("$1"); shift ;;
  esac
done

all_distros() {
  find "$DOCKERFILE_DIR" -maxdepth 1 -name '*.Dockerfile' -print \
    | sed -e 's#.*/##' -e 's#\.Dockerfile$##' \
    | sort
}

if [ "$LIST_ONLY" -eq 1 ]; then
  all_distros
  exit 0
fi

if [ "${#DISTROS[@]}" -eq 0 ]; then
  # shellcheck disable=SC2207
  DISTROS=( $(all_distros) )
fi

# ---------------------------------------------------------------------------
# Preflight
# ---------------------------------------------------------------------------

# Allow explicit override via env var, otherwise auto-detect.  podman is
# tried first because it works rootless on Debian/Fedora out of the box,
# whereas docker typically needs systemd unit setup.
CONTAINER_CLI="${CONTAINER_CLI:-}"
if [ -z "$CONTAINER_CLI" ]; then
  if command -v podman >/dev/null 2>&1 && podman info >/dev/null 2>&1; then
    CONTAINER_CLI=podman
  elif command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1; then
    CONTAINER_CLI=docker
  else
    echo "error: neither podman nor docker is reachable." >&2
    echo "hint: install podman (sudo apt install podman) or start the docker daemon." >&2
    exit 2
  fi
fi

BRANCH="$(git -C "$REPO_DIR" rev-parse --abbrev-ref HEAD 2>/dev/null || echo master)"

# If the checkout is a git worktree, its .git is a pointer file that the
# container cannot resolve — the real git dir lives above the mount.
# Materialise a standalone clone in a tmpdir and mount that instead.
#
# --no-hardlinks forces a real file copy (some container storage drivers
# misbehave with hardlinks across the bind boundary).
SRC_DIR="$(mktemp -d -t anvil-installtest-src.XXXXXX)"
trap 'rm -rf "$SRC_DIR"' EXIT

git clone --no-hardlinks --branch "$BRANCH" "$REPO_DIR" "$SRC_DIR/anvil" \
  >/tmp/anvil-install-clone.log 2>&1 || {
    echo "error: failed to snapshot worktree to $SRC_DIR/anvil" >&2
    cat /tmp/anvil-install-clone.log >&2
    exit 2
  }

# Overlay the working tree so uncommitted + untracked files (e.g. a
# test harness being developed right now) are visible to the test.
# tar --exclude=.git preserves the fresh clone's real git dir.
(cd "$REPO_DIR" && tar --exclude='./.git' -cf - .) \
  | tar -C "$SRC_DIR/anvil" -xf -
# Re-stage so `git ls-files` inside the container sees a clean tree —
# untracked files still work via the overlay, but the working tree
# matches the branch tip for consistency in diagnostics.

SNAPSHOT_DIR="$SRC_DIR/anvil"

echo "test-install.sh"
echo "  engine:  $CONTAINER_CLI"
echo "  repo:    $REPO_DIR"
echo "  mount:   $SNAPSHOT_DIR (fresh clone of $BRANCH)"
echo "  branch:  $BRANCH"
echo "  distros: ${DISTROS[*]}"
echo "  cache:   $([ "$NO_CACHE" -eq 1 ] && echo "disabled" || echo "enabled")"
echo

# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------

declare -A RESULTS
overall_rc=0

for distro in "${DISTROS[@]}"; do
  dockerfile="$DOCKERFILE_DIR/$distro.Dockerfile"
  image="anvil-installtest:$distro"

  if [ ! -f "$dockerfile" ]; then
    echo "error: no Dockerfile for '$distro' ($dockerfile)" >&2
    RESULTS["$distro"]=missing
    overall_rc=1
    continue
  fi

  echo "=== $distro ==="
  build_args=( -f "$dockerfile" -t "$image" "$DOCKERFILE_DIR" )
  [ "$NO_CACHE" -eq 1 ] && build_args=( --no-cache "${build_args[@]}" )
  if ! "$CONTAINER_CLI" build "${build_args[@]}"; then
    RESULTS["$distro"]=build-failed
    overall_rc=1
    continue
  fi

  if "$CONTAINER_CLI" run --rm \
      -v "$SNAPSHOT_DIR:/anvil:ro" \
      -e ANVIL_TEST_BRANCH="$BRANCH" \
      "$image"; then
    RESULTS["$distro"]=pass
  else
    RESULTS["$distro"]=fail
    overall_rc=1
  fi
  echo
done

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

echo "=========================================="
echo "summary"
echo "=========================================="
for distro in "${DISTROS[@]}"; do
  result="${RESULTS[$distro]:-skipped}"
  case "$result" in
    pass)          printf '  \033[1;32m%-12s %s\033[0m\n' "$distro" "$result" ;;
    *)             printf '  \033[1;31m%-12s %s\033[0m\n' "$distro" "$result" ;;
  esac
done

exit "$overall_rc"
