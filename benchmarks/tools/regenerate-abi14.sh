#!/usr/bin/env bash
# regenerate-abi14.sh — build tree-sitter-elisp grammar at ABI 14 for Emacs 30.
#
# Upstream Wilfred/tree-sitter-elisp ships grammar.js that generates ABI 15
# with recent tree-sitter CLIs, but Emacs 30.1's libtreesit only supports
# ABI 14. This script drives `tree-sitter generate --abi 14` deterministically
# and installs the resulting shared library into ~/.emacs.d/tree-sitter/.
#
# Usage:
#   regenerate-abi14.sh                   # default source/install paths
#   regenerate-abi14.sh --src DIR         # use existing checkout at DIR
#   regenerate-abi14.sh --install-dir DIR # override install target
#   regenerate-abi14.sh --ref REF         # checkout git ref (branch/tag/sha)
#   regenerate-abi14.sh --no-install      # build only, skip install step
#   regenerate-abi14.sh --verify-only     # run post-install Emacs sanity check
#
# Env overrides: TS_ELISP_REPO, TS_ELISP_REF, TS_INSTALL_DIR, CC

set -euo pipefail

DEFAULT_REPO="https://github.com/Wilfred/tree-sitter-elisp.git"
DEFAULT_SRC="${HOME}/.cache/anvil/tree-sitter-elisp"
DEFAULT_INSTALL="${HOME}/.emacs.d/tree-sitter"
DEFAULT_REF="${TS_ELISP_REF:-main}"
ABI=14

SRC_DIR="${TS_ELISP_SRC:-${DEFAULT_SRC}}"
INSTALL_DIR="${TS_INSTALL_DIR:-${DEFAULT_INSTALL}}"
REPO_URL="${TS_ELISP_REPO:-${DEFAULT_REPO}}"
REF="${DEFAULT_REF}"
DO_INSTALL=1
VERIFY_ONLY=0

while [ $# -gt 0 ]; do
  case "$1" in
    --src) SRC_DIR="$2"; shift 2 ;;
    --install-dir) INSTALL_DIR="$2"; shift 2 ;;
    --ref) REF="$2"; shift 2 ;;
    --no-install) DO_INSTALL=0; shift ;;
    --verify-only) VERIFY_ONLY=1; shift ;;
    -h|--help)
      sed -n '2,17p' "$0" | sed 's/^# \{0,1\}//'
      exit 0 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

log() { printf '[regenerate-abi14] %s\n' "$*" >&2; }
die() { printf '[regenerate-abi14] ERROR: %s\n' "$*" >&2; exit 1; }

verify_install() {
  local so="${INSTALL_DIR}/libtree-sitter-elisp.so"
  [ -f "$so" ] || die "grammar not found at ${so}"
  command -v emacs >/dev/null 2>&1 || { log "emacs not in PATH, skipping runtime check"; return 0; }
  emacs --batch -Q --eval "
(progn
  (require 'treesit nil t)
  (unless (fboundp 'treesit-available-p)
    (message \"treesit not available in this Emacs build\") (kill-emacs 7))
  (unless (treesit-available-p)
    (message \"treesit not compiled in\") (kill-emacs 8))
  (add-to-list 'treesit-extra-load-path \"${INSTALL_DIR}\")
  (unless (treesit-ready-p 'elisp t)
    (message \"treesit-elisp not ready (ABI mismatch or missing grammar)\")
    (kill-emacs 9))
  (with-temp-buffer
    (insert \"(defun f () 1)\")
    (let* ((parser (treesit-parser-create 'elisp))
           (root (treesit-parser-root-node parser)))
      (unless (string= (treesit-node-type root) \"source_file\")
        (message \"unexpected root: %s\" (treesit-node-type root))
        (kill-emacs 10))
      (message \"verify: ok — %s\" (treesit-node-type root)))))
" \
    || die "Emacs verification failed (see output above)"
  log "verify: ok (${so} loads in Emacs, ABI compatible)"
}

if [ "$VERIFY_ONLY" -eq 1 ]; then
  verify_install
  exit 0
fi

# --- Preflight -------------------------------------------------------------

command -v tree-sitter >/dev/null 2>&1 \
  || die "tree-sitter CLI not found. Install via 'cargo install tree-sitter-cli' or 'npm install -g tree-sitter-cli'."
command -v git >/dev/null 2>&1 || die "git not found"
: "${CC:=cc}"
command -v "$CC" >/dev/null 2>&1 || die "compiler '${CC}' not found (set CC env)"

TS_VERSION="$(tree-sitter --version 2>/dev/null || echo unknown)"
log "tree-sitter: ${TS_VERSION}"
log "target ABI: ${ABI}"
log "source:  ${SRC_DIR}"
log "install: ${INSTALL_DIR}"

# --- Fetch / update source -------------------------------------------------

if [ ! -d "${SRC_DIR}/.git" ]; then
  log "cloning ${REPO_URL} into ${SRC_DIR}"
  mkdir -p "$(dirname "$SRC_DIR")"
  git clone --depth 1 "$REPO_URL" "$SRC_DIR"
fi

log "fetching ref ${REF}"
git -C "$SRC_DIR" fetch --depth 1 origin "$REF" || git -C "$SRC_DIR" fetch origin
git -C "$SRC_DIR" checkout "$REF"
git -C "$SRC_DIR" reset --hard "origin/${REF}" 2>/dev/null || git -C "$SRC_DIR" reset --hard "$REF"
HEAD_SHA="$(git -C "$SRC_DIR" rev-parse HEAD)"
log "source HEAD: ${HEAD_SHA}"

# --- Regenerate at ABI 14 --------------------------------------------------

log "tree-sitter generate --abi ${ABI}"
(
  cd "$SRC_DIR"
  tree-sitter generate --abi "$ABI"
)

PARSER_C="${SRC_DIR}/src/parser.c"
[ -f "$PARSER_C" ] || die "generate did not produce ${PARSER_C}"

# Sanity-grep the ABI constant in the generated parser. The symbol name has
# varied across CLI versions (LANGUAGE_VERSION / TREE_SITTER_LANGUAGE_VERSION)
# so match both and accept either.
if ! grep -E '^#define\s+(TREE_SITTER_)?LANGUAGE_VERSION\s+'"${ABI}"'\b' "$PARSER_C" >/dev/null; then
  die "parser.c does not declare LANGUAGE_VERSION ${ABI} — generate may have ignored --abi"
fi
log "parser.c declares LANGUAGE_VERSION ${ABI} ✓"

# --- Compile shared object -------------------------------------------------

BUILD_DIR="$(mktemp -d -t anvil-tselisp-XXXX)"
trap 'rm -rf "$BUILD_DIR"' EXIT

SO_NAME="libtree-sitter-elisp.so"
SO_OUT="${BUILD_DIR}/${SO_NAME}"

CFLAGS_BASE=(-O2 -fPIC -I"${SRC_DIR}/src")
# tree-sitter-elisp has no scanner.c at time of writing, but accept both cases.
SRCS=("${SRC_DIR}/src/parser.c")
if [ -f "${SRC_DIR}/src/scanner.c" ]; then
  SRCS+=("${SRC_DIR}/src/scanner.c")
fi

log "compiling: ${CC} ${CFLAGS_BASE[*]} -shared -o ${SO_OUT} ${SRCS[*]}"
"${CC}" "${CFLAGS_BASE[@]}" -shared -o "$SO_OUT" "${SRCS[@]}"

[ -f "$SO_OUT" ] || die "compile produced no output"
log "built: $(stat -c '%n (%s bytes)' "$SO_OUT")"

# --- Install ---------------------------------------------------------------

if [ "$DO_INSTALL" -eq 0 ]; then
  cp "$SO_OUT" "./${SO_NAME}"
  log "install skipped — copy at ./${SO_NAME}"
  exit 0
fi

mkdir -p "$INSTALL_DIR"
DEST="${INSTALL_DIR}/${SO_NAME}"
if [ -f "$DEST" ]; then
  BACKUP="${DEST}.prev-$(date +%Y%m%d%H%M%S)"
  cp "$DEST" "$BACKUP"
  log "backed up existing grammar: ${BACKUP}"
fi
install -m 0755 "$SO_OUT" "$DEST"
log "installed: ${DEST}"

# --- Post-install verify ---------------------------------------------------

verify_install
log "done"
