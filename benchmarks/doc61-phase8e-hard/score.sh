#!/usr/bin/env bash
# score.sh -- objective scorer for a doc61-phase8e-hard task fixture.
#
# Usage: score.sh TASK_REPO_DIR TASK_NUMBER(1-3)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EMACS="${EMACS:-emacs}"

if [ "$#" -ne 2 ]; then
  echo "usage: $0 TASK_REPO_DIR TASK_NUMBER(1-3)" >&2
  exit 2
fi

TASK_DIR="$1"
TASK_NUM="$2"
HIDDEN_TEST="${SCRIPT_DIR}/hidden/hard${TASK_NUM}-hidden-test.el"

if [ ! -f "$HIDDEN_TEST" ]; then
  echo "FAIL score.sh: no hidden test for task ${TASK_NUM} (${HIDDEN_TEST})" >&2
  exit 2
fi
if [ ! -d "$TASK_DIR" ]; then
  echo "FAIL score.sh: task dir not found: ${TASK_DIR}" >&2
  exit 2
fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

shopt -s nullglob
srcs=("$TASK_DIR"/*.el)
shopt -u nullglob
if [ "${#srcs[@]}" -eq 0 ]; then
  echo "FAIL score.sh: no *.el source files in ${TASK_DIR}" >&2
  exit 2
fi
cp -f "${srcs[@]}" "$WORK"/
cp -f "$HIDDEN_TEST" "$WORK/hidden-test.el"

load_args=()
for f in "$WORK"/*.el; do
  [ "$(basename "$f")" = "hidden-test.el" ] && continue
  load_args+=(-l "$f")
done

OUT="$WORK/out.log"
if "$EMACS" -Q --batch -L "$WORK" \
     "${load_args[@]}" \
     -l "$WORK/hidden-test.el" \
     -f ert-run-tests-batch-and-exit >"$OUT" 2>&1; then
  detail="$(grep -E '^Ran [0-9]+ tests' "$OUT" | tail -1)"
  echo "PASS hard${TASK_NUM} ${TASK_DIR} -- ${detail:-ok}"
  exit 0
else
  detail="$(grep -E '^Ran [0-9]+ tests' "$OUT" | tail -1)"
  echo "FAIL hard${TASK_NUM} ${TASK_DIR} -- ${detail:-see log below}"
  cat "$OUT" >&2
  exit 1
fi
