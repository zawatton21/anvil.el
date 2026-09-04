#!/usr/bin/env bash
# run_one.sh TASK_NUM(1-3) ARM(baseline|full)
#
# One full (task, arm) cycle for the Doc 61 Phase 8e end-to-end eval:
#   1. fresh scratch copy of fixtures/taskN + git init (throwaway repo)
#   2. anvil-fusion-longrun-run quest via run.el, in a standalone batch
#      emacs with sandboxed anvil-state-db-path / anvil-orchestrator-work-dir,
#      hard-killed at 25 minutes wall (timeout 1500)
#   3. score: make check (visible) + score.sh (hidden) against whatever
#      the quest left in the scratch repo, run REGARDLESS of whether the
#      quest finished, errored, or was killed by the timeout
#   4. writes raw/<task>-<arm>.score.txt combining run.el's meta.sexp
#      (if it got to write one) with the scoring outcome
#
# Repo files touched: NONE except reading fixtures/taskN + score.sh and
# writing under benchmarks/doc61-phase8e/raw/. All working copies live
# under $SCRATCH.

set -uo pipefail

TASK_NUM="$1"
ARM="$2"

REPO=/home/madblack-21/Cowork/Notes/dev/anvil.el
SCRATCH=/tmp/claude-1000/-home-madblack-21-Cowork-Notes/270690bc-4536-4d11-98da-a9e8806b121a/scratchpad/doc61-phase8e-eval
RAW_DIR="$REPO/benchmarks/doc61-phase8e/raw"
SCORE_SH="$REPO/benchmarks/doc61-phase8e/score.sh"
RUN_KEY="task${TASK_NUM}-${ARM}"
TASKDIR="$SCRATCH/fixtures-work/$RUN_KEY"
ORCH_DIR="$SCRATCH/orch-sandbox/$RUN_KEY"
LOG="$SCRATCH/logs/${RUN_KEY}.log"

echo "=== $RUN_KEY starting $(date -Iseconds) ===" | tee "$LOG"

rm -rf "$TASKDIR" "$ORCH_DIR"
mkdir -p "$TASKDIR" "$ORCH_DIR"
cp -a "$REPO/benchmarks/doc61-phase8e/fixtures/task${TASK_NUM}/." "$TASKDIR/"

(
  cd "$TASKDIR" || exit 1
  git init -q
  git add -A
  git -c user.email="doc61-8e@scratch.local" -c user.name="doc61-8e-runner" \
    commit -q -m init
)

T0=$(date +%s)
timeout 1500 emacs -Q --batch -L "$REPO" \
  --eval "(setq anvil-state-db-path \"$ORCH_DIR/state.db\")" \
  --eval "(setq anvil-orchestrator-work-dir \"$ORCH_DIR/workdir\")" \
  -l "$SCRATCH/run.el" \
  -- "$TASK_NUM" "$ARM" "$TASKDIR" "$RAW_DIR" \
  >>"$LOG" 2>&1
EMACS_EXIT=$?
T1=$(date +%s)
WALL_OUTER=$((T1 - T0))

case "$EMACS_EXIT" in
  0) EMACS_STATUS="completed" ;;
  124|137) EMACS_STATUS="timeout-killed" ;;
  *) EMACS_STATUS="error-exit-${EMACS_EXIT}" ;;
esac
echo "=== $RUN_KEY emacs exit=$EMACS_EXIT status=$EMACS_STATUS outer_wall=${WALL_OUTER}s ===" | tee -a "$LOG"

# --- scoring: always run, regardless of emacs outcome ---
MAKE_OUT=$(cd "$TASKDIR" && make check 2>&1)
MAKE_EXIT=$?
if [ "$MAKE_EXIT" -eq 0 ]; then MAKE_PASS="PASS"; else MAKE_PASS="FAIL"; fi

SCORE_OUT=$("$SCORE_SH" "$TASKDIR" "$TASK_NUM" 2>&1)
SCORE_EXIT=$?
if [ "$SCORE_EXIT" -eq 0 ]; then SCORE_PASS="PASS"; else SCORE_PASS="FAIL"; fi

{
  echo "task=${TASK_NUM} arm=${ARM}"
  echo "emacs_status=${EMACS_STATUS} emacs_exit=${EMACS_EXIT} outer_wall_sec=${WALL_OUTER}"
  echo "visible(make check)=${MAKE_PASS}"
  echo "--- make check output ---"
  echo "$MAKE_OUT"
  echo "hidden(score.sh)=${SCORE_PASS}"
  echo "--- score.sh output ---"
  echo "$SCORE_OUT"
  echo "--- meta.sexp (if present) ---"
  if [ -f "$RAW_DIR/${RUN_KEY}.meta.sexp" ]; then
    cat "$RAW_DIR/${RUN_KEY}.meta.sexp"
  else
    echo "(no meta.sexp -- quest process did not reach its final write; see checkpoint.sexp for partial state)"
    if [ -f "$RAW_DIR/${RUN_KEY}.checkpoint.sexp" ]; then
      echo "--- last checkpoint.sexp ---"
      cat "$RAW_DIR/${RUN_KEY}.checkpoint.sexp"
    fi
  fi
} > "$RAW_DIR/${RUN_KEY}.score.txt"

echo "=== $RUN_KEY SUMMARY: emacs=${EMACS_STATUS} visible=${MAKE_PASS} hidden=${SCORE_PASS} outer_wall=${WALL_OUTER}s ===" | tee -a "$LOG"
echo "RUN_ONE_DONE $RUN_KEY emacs=${EMACS_STATUS} visible=${MAKE_PASS} hidden=${SCORE_PASS} outer_wall=${WALL_OUTER}s"
