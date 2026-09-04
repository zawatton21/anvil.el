#!/usr/bin/env bash
# score.sh -- objective scorer for a doc61-phase8e task fixture.
#
# Usage: score.sh TASK_REPO_DIR TASK_NUMBER(1-3)
#
# Copies the HIDDEN acceptance test for TASK_NUMBER into a scratch
# copy of TASK_REPO_DIR (the agent's working copy of fixtures/taskN,
# patched or not) and runs it batch-style with ERT. Prints a single
# PASS/FAIL line plus a one-line detail. Never modifies TASK_REPO_DIR.
#
# This is the objective judge referenced by ../README.org: the agent
# being evaluated never sees hidden/taskN-hidden-test.el, only
# fixtures/taskN/{*.el,tests/visible-test.el,Makefile,TASK.md}.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EMACS="${EMACS:-emacs}"

if [ "$#" -ne 2 ]; then
  echo "usage: $0 TASK_REPO_DIR TASK_NUMBER(1-3)" >&2
  exit 2
fi

TASK_DIR="$1"
TASK_NUM="$2"
HIDDEN_TEST="${SCRIPT_DIR}/hidden/task${TASK_NUM}-hidden-test.el"

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

# Only the top-level library sources travel into the scratch dir --
# never the agent's own tests/ (the hidden test is the sole judge).
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
  echo "PASS task${TASK_NUM} ${TASK_DIR} -- ${detail:-ok}"
  exit 0
else
  detail="$(grep -E '^Ran [0-9]+ tests' "$OUT" | tail -1)"
  echo "FAIL task${TASK_NUM} ${TASK_DIR} -- ${detail:-see log below}"
  cat "$OUT" >&2
  exit 1
fi
