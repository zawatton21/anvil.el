#!/usr/bin/env bash
# score.sh -- deterministic rubric scorer for doc61-phase8g answers.
#
# Usage: score.sh ANSWER_FILE qN

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EMACS="${EMACS:-emacs}"

if [ "$#" -ne 2 ]; then
  echo "usage: $0 ANSWER_FILE qN" >&2
  exit 2
fi

ANSWER_FILE="$1"
QUESTION_ID="$2"
RUBRIC_FILE="${SCRIPT_DIR}/rubric/${QUESTION_ID}-rubric.el"

if [ ! -f "$ANSWER_FILE" ]; then
  echo "missing answer file: $ANSWER_FILE" >&2
  exit 2
fi

if [ ! -f "$RUBRIC_FILE" ]; then
  echo "missing rubric file: $RUBRIC_FILE" >&2
  exit 2
fi

"$EMACS" -Q --batch \
  --eval "(let* ((rubric (with-temp-buffer
                           (insert-file-contents \"$RUBRIC_FILE\")
                           (read (current-buffer))))
                 (text (with-temp-buffer
                         (insert-file-contents \"$ANSWER_FILE\")
                         (buffer-string)))
                 (required (plist-get rubric :required))
                 (traps (plist-get rubric :traps))
                 (max-score (plist-get rubric :max-score))
                 (required-hits 0)
                 (trap-hits 0)
                 (score 0))
            (dolist (item required)
              (when (string-match-p (plist-get item :key) text)
                (setq required-hits (1+ required-hits))
                (setq score (+ score (plist-get item :points)))))
            (dolist (item traps)
              (when (string-match-p (plist-get item :key) text)
                (setq trap-hits (1+ trap-hits))
                (setq score (- score (plist-get item :penalty)))))
            (setq score (max 0 score))
            (princ
             (format \"%s score=%d/%d required=%d/%d traps=%d\n\"
                     (plist-get rubric :id)
                     score
                     max-score
                     required-hits
                     (length required)
                     trap-hits))))"
