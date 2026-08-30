;;; anvil-compact-test.el --- Tests for anvil-compact -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercises Doc 36 Phase 1: estimate / should-trigger / snapshot /
;; hook entries / MCP tool shape.  State is isolated via a temp
;; `anvil-state-db-path' so tests do not pollute the user's real DB.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'anvil-state)
(require 'anvil-compact)

(defvar anvil-state--cached-db)

(defun anvil-compact-test--make-db-path ()
  "Return a fresh temp SQLite path for anvil-state in a test."
  (make-temp-file "anvil-compact-test-" nil ".db"))

(defmacro anvil-compact-test--with-fresh-state (&rest body)
  "Run BODY with a private anvil-state DB, cleaned up on exit."
  (declare (indent 0))
  `(let ((path (anvil-compact-test--make-db-path))
         (anvil-state--cached-db nil))
     (unwind-protect
         (let ((anvil-state-db-path path))
           (anvil-state-enable)
           ,@body)
       (anvil-state-disable)
       (ignore-errors (delete-file path)))))

(defun anvil-compact-test--write-jsonl (path lines)
  "Write LINES to PATH, one JSON record per line."
  (with-temp-file path
    (dolist (line lines)
      (insert line "\n"))))


;;;; --- estimate -----------------------------------------------------------

(ert-deftest anvil-compact-test-estimate-missing-path-returns-zero ()
  "Missing / unreadable transcript yields all-zero plist, no signal."
  (let ((r (anvil-compact-estimate :transcript-path nil)))
    (should (= 0 (plist-get r :bytes)))
    (should (= 0 (plist-get r :tokens)))
    (should (= 0 (plist-get r :percent)))
    (should (= 0 (plist-get r :turns)))))

(ert-deftest anvil-compact-test-estimate-empty-string-path ()
  "Empty-string transcript path is treated as missing."
  (let ((r (anvil-compact-estimate :transcript-path "")))
    (should (= 0 (plist-get r :bytes)))))

(ert-deftest anvil-compact-test-estimate-counts-bytes-and-tokens ()
  "A real file should contribute bytes → tokens at the configured ratio."
  (let ((p (make-temp-file "anvil-compact-")))
    (unwind-protect
        (progn
          (anvil-compact-test--write-jsonl
           p '("{\"role\":\"user\",\"content\":\"abc\"}"
               "{\"role\":\"assistant\",\"content\":\"de\"}"))
          (let* ((r (anvil-compact-estimate
                     :transcript-path p
                     :bytes-per-token 4
                     :context-max 1000)))
            (should (> (plist-get r :bytes) 0))
            (should (> (plist-get r :tokens) 0))
            (should (= 2 (plist-get r :turns)))
            ;; Bytes / 4 ≈ tokens
            (should (= (truncate (/ (float (plist-get r :bytes)) 4))
                       (plist-get r :tokens)))))
      (ignore-errors (delete-file p)))))

(ert-deftest anvil-compact-test-estimate-percent-clamped-to-100 ()
  "Huge transcript with tiny context-max should clamp percent at 100."
  (let ((p (make-temp-file "anvil-compact-")))
    (unwind-protect
        (progn
          (with-temp-file p
            (insert (make-string 10000 ?x)))
          (let ((r (anvil-compact-estimate
                    :transcript-path p
                    :bytes-per-token 1
                    :context-max 100)))
            (should (= 100 (plist-get r :percent)))))
      (ignore-errors (delete-file p)))))


;;;; --- should-trigger -----------------------------------------------------

(ert-deftest anvil-compact-test-should-trigger-below-threshold ()
  (let ((r (anvil-compact-should-trigger :percent 20 :trigger-at 45)))
    (should-not (plist-get r :trigger))
    (should (eq :below-threshold (plist-get r :reason)))))

(ert-deftest anvil-compact-test-should-trigger-in-progress-blocks ()
  (let ((r (anvil-compact-should-trigger
            :percent 50 :task-in-progress 1 :trigger-at 45)))
    (should-not (plist-get r :trigger))
    (should (eq :in-progress (plist-get r :reason)))))

(ert-deftest anvil-compact-test-should-trigger-cooldown-blocks ()
  "Growth since last compact < cooldown → blocked."
  (let ((r (anvil-compact-should-trigger
            :percent 50
            :last-compact-percent 40
            :trigger-at 45
            :cooldown 25)))
    (should-not (plist-get r :trigger))
    (should (eq :cooldown (plist-get r :reason)))))

(ert-deftest anvil-compact-test-should-trigger-fires ()
  "Above threshold, no in-progress task, past cooldown → fire."
  (let ((r (anvil-compact-should-trigger
            :percent 50
            :task-in-progress 0
            :last-compact-percent 0
            :trigger-at 45
            :cooldown 25)))
    (should (plist-get r :trigger))
    (should (eq :trigger (plist-get r :reason)))
    (should (= 50 (plist-get r :percent)))))

(ert-deftest anvil-compact-test-should-trigger-at-exact-threshold ()
  "Percent == threshold should fire (the condition is `<', not `<=')."
  (let ((r (anvil-compact-should-trigger
            :percent 45 :trigger-at 45 :last-compact-percent 0
            :cooldown 25)))
    (should (plist-get r :trigger))))

(ert-deftest anvil-compact-test-should-trigger-defaults-apply ()
  "Omitted keys pick up defcustom defaults."
  (let ((anvil-compact-trigger-percent 10)
        (anvil-compact-cooldown-percent 0))
    (let ((r (anvil-compact-should-trigger :percent 15)))
      (should (plist-get r :trigger)))))


;;;; --- pressure report ----------------------------------------------------

(ert-deftest anvil-compact-test-pressure-report-high-context-critical ()
  "High context pressure should surface a critical compact action."
  (let ((p (make-temp-file "anvil-compact-")))
    (unwind-protect
        (progn
          (with-temp-file p
            (insert (make-string 800 ?x)))
          (let ((anvil-compact-trigger-percent 45)
                (anvil-compact-pressure-high-percent 75)
                (anvil-compact-context-tokens-max 1000)
                (anvil-compact-bytes-per-token 1))
            (let ((r (anvil-compact-pressure-report
                      :transcript-path p
                      :task-in-progress 0
                      :last-compact-percent 0)))
              (should (eq :critical (plist-get r :severity)))
              (should (plist-get (plist-get r :compact-decision)
                                 :trigger))
              (should (cl-some
                       (lambda (s)
                         (string-match-p "high-pressure" s))
                       (plist-get r :actions))))))
      (ignore-errors (delete-file p)))))

(ert-deftest anvil-compact-test-pressure-report-long-session-high ()
  "A long-running session should recommend a checkpoint boundary."
  (let ((r (anvil-compact-pressure-report
            :transcript-path nil
            :session-age-hours 8.5)))
    (should (eq :high (plist-get r :severity)))
    (should (cl-some
             (lambda (s)
               (string-match-p "8[+] hours" s))
             (plist-get r :actions)))))


;;;; --- snapshot ------------------------------------------------------------

(ert-deftest anvil-compact-test-snapshot-roundtrip ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact-snapshot-capture
     "sid-1"
     :task-summary "implement Doc 36 Phase 1"
     :branch "feat/doc-36"
     :files '("anvil-compact.el" "tests/anvil-compact-test.el")
     :todos "byte-compile then test-all"
     :percent 50)
    (let ((snap (anvil-compact-snapshot-get "sid-1")))
      (should snap)
      (should (equal "implement Doc 36 Phase 1"
                     (plist-get snap :task-summary)))
      (should (equal "feat/doc-36" (plist-get snap :branch)))
      (should (member "anvil-compact.el" (plist-get snap :files)))
      (should (= 50 (plist-get snap :percent))))))

(ert-deftest anvil-compact-test-snapshot-scoped-by-session ()
  "Two sessions should not see each other's snapshots."
  (anvil-compact-test--with-fresh-state
    (anvil-compact-snapshot-capture "A" :task-summary "A-task" :percent 50)
    (anvil-compact-snapshot-capture "B" :task-summary "B-task" :percent 70)
    (should (equal "A-task" (plist-get (anvil-compact-snapshot-get "A")
                                       :task-summary)))
    (should (equal "B-task" (plist-get (anvil-compact-snapshot-get "B")
                                       :task-summary)))))

(ert-deftest anvil-compact-test-snapshot-include-files-nil-drops-files ()
  (anvil-compact-test--with-fresh-state
    (let ((anvil-compact-snapshot-include-files nil))
      (anvil-compact-snapshot-capture
       "sid" :files '("a.el") :task-summary "x" :percent 50))
    (should (null (plist-get (anvil-compact-snapshot-get "sid")
                             :files)))))

(ert-deftest anvil-compact-test-snapshot-format-skips-empty-fields ()
  (let* ((snap (list :captured-at "2026-04-24T00:00:00+0900"
                     :percent 50
                     :task-summary ""
                     :branch ""
                     :files nil
                     :todos nil))
         (text (anvil-compact-snapshot-format snap)))
    (should (stringp text))
    (should (string-match-p "anvil-compact restore" text))
    (should-not (string-match-p "task:" text))
    (should-not (string-match-p "branch:" text))
    (should-not (string-match-p "files:" text))
    (should-not (string-match-p "todos:" text))))

(ert-deftest anvil-compact-test-snapshot-format-renders-all-fields ()
  (let* ((snap (list :captured-at "2026-04-24T00:00:00+0900"
                     :percent 50
                     :task-summary "write tests"
                     :branch "feat/x"
                     :files '("a.el" "b.el")
                     :todos "compile; test"))
         (text (anvil-compact-snapshot-format snap)))
    (should (string-match-p "task: write tests" text))
    (should (string-match-p "branch: feat/x" text))
    (should (string-match-p "files: a.el, b.el" text))
    (should (string-match-p "todos: compile; test" text))))

(ert-deftest anvil-compact-test-snapshot-format-nil-returns-nil ()
  (should (null (anvil-compact-snapshot-format nil))))


;;;; --- on-stop ------------------------------------------------------------

(ert-deftest anvil-compact-test-on-stop-below-threshold-no-flag ()
  (anvil-compact-test--with-fresh-state
    (let ((r (anvil-compact-on-stop "sid" :transcript-path nil)))
      (should (eq :below-threshold (plist-get r :decision)))
      (should (null (anvil-compact--state-get "sid" "pending-nudge"))))))

(ert-deftest anvil-compact-test-on-stop-trigger-sets-flag-and-snapshot ()
  (anvil-compact-test--with-fresh-state
    (let ((p (make-temp-file "anvil-compact-")))
      (unwind-protect
          (progn
            ;; Arrange: big file to force high percent
            (with-temp-file p
              (insert (make-string 50000 ?x)))
            (let ((anvil-compact-trigger-percent 10)
                  (anvil-compact-cooldown-percent 0)
                  (anvil-compact-context-tokens-max 1000)
                  (anvil-compact-bytes-per-token 1))
              (anvil-compact-on-stop
               "sid"
               :transcript-path p
               :task-summary "do the thing"
               :task-in-progress 0
               :branch "main"))
            (should (equal t (anvil-compact--state-get "sid"
                                                      "pending-nudge")))
            (let ((snap (anvil-compact-snapshot-get "sid")))
              (should snap)
              (should (equal "do the thing"
                             (plist-get snap :task-summary)))))
        (ignore-errors (delete-file p))))))

(ert-deftest anvil-compact-test-on-stop-in-progress-suppresses ()
  (anvil-compact-test--with-fresh-state
    (let ((p (make-temp-file "anvil-compact-")))
      (unwind-protect
          (progn
            (with-temp-file p (insert (make-string 50000 ?x)))
            (let ((anvil-compact-trigger-percent 10)
                  (anvil-compact-context-tokens-max 1000)
                  (anvil-compact-bytes-per-token 1))
              (let ((r (anvil-compact-on-stop
                        "sid"
                        :transcript-path p
                        :task-in-progress 2)))
                (should (eq :in-progress (plist-get r :decision))))))
        (ignore-errors (delete-file p))))))


;;;; --- on-user-prompt -----------------------------------------------------

(ert-deftest anvil-compact-test-on-user-prompt-no-flag-empty ()
  (anvil-compact-test--with-fresh-state
    (should (equal "" (anvil-compact-on-user-prompt "sid")))))

(ert-deftest anvil-compact-test-on-user-prompt-flag-emits-nudge ()
  "Flag-set path emits plain-text preamble + /compact instruction
so the shell hook wrapper can forward it verbatim."
  (anvil-compact-test--with-fresh-state
    (anvil-compact--state-put "sid" "pending-nudge" t)
    (anvil-compact-snapshot-capture "sid"
                                    :task-summary "implement Phase 1"
                                    :percent 50)
    (let ((out (anvil-compact-on-user-prompt "sid")))
      (should (stringp out))
      (should (string-match-p "auto-compact" out))
      (should (string-match-p "/compact" out))
      (should (string-match-p "implement Phase 1" out)))))

(ert-deftest anvil-compact-test-on-user-prompt-clears-flag ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact--state-put "sid" "pending-nudge" t)
    (anvil-compact-on-user-prompt "sid")
    (should (null (anvil-compact--state-get "sid" "pending-nudge")))
    ;; Second call should degrade to empty string
    (should (equal "" (anvil-compact-on-user-prompt "sid")))))


;;;; --- on-session-start ---------------------------------------------------

(ert-deftest anvil-compact-test-on-session-start-no-snapshot-empty ()
  (anvil-compact-test--with-fresh-state
    (should (equal "" (anvil-compact-on-session-start "sid")))))

(ert-deftest anvil-compact-test-on-session-start-emits-preamble ()
  "Session-start emits the snapshot preamble as plain text."
  (anvil-compact-test--with-fresh-state
    (anvil-compact-snapshot-capture "sid"
                                    :task-summary "cross-session task"
                                    :branch "feat/x"
                                    :percent 50)
    (let ((out (anvil-compact-on-session-start "sid")))
      (should (stringp out))
      (should (string-match-p "cross-session task" out))
      (should (string-match-p "feat/x" out)))))


;;;; --- MCP tool wrappers --------------------------------------------------

(ert-deftest anvil-compact-test-tool-estimate-handles-empty-string ()
  (let ((r (anvil-compact--tool-estimate "")))
    (should (listp r))
    (should (= 0 (plist-get r :bytes)))))

(ert-deftest anvil-compact-test-tool-should-trigger-accepts-strings ()
  "MCP transport may deliver numeric args as strings; tool should coerce."
  (let ((r (anvil-compact--tool-should-trigger "60" "0" "0")))
    (should (plist-get r :trigger))))

(ert-deftest anvil-compact-test-tool-snapshot-roundtrip-via-tools ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact--tool-snapshot "sid" "do the thing")
    (let ((out (anvil-compact--tool-restore "sid")))
      (should (stringp out))
      (should (string-match-p "do the thing" out)))))

(ert-deftest anvil-compact-test-tool-hook-unknown-stage-returns-empty ()
  (anvil-compact-test--with-fresh-state
    (should (equal "" (anvil-compact--tool-hook "sid" "unknown" "")))))

(ert-deftest anvil-compact-test-tool-pressure-report-accepts-strings ()
  "MCP transport may deliver optional numeric args as strings."
  (let ((r (anvil-compact--tool-pressure-report "" "" "9" "0" "0")))
    (should (eq :high (plist-get r :severity)))
    (should (> (plist-get r :session-age-hours) 8))))


;;;; --- Phase 2: restore-queue --------------------------------------------

(ert-deftest anvil-compact-test-queue-empty-pop-returns-nil ()
  (anvil-compact-test--with-fresh-state
    (should (null (anvil-compact--queue-pop "sid")))
    (should (= 0 (anvil-compact--queue-length "sid")))))

(ert-deftest anvil-compact-test-queue-fifo ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact--queue-push "sid" 'first)
    (anvil-compact--queue-push "sid" 'second)
    (should (= 2 (anvil-compact--queue-length "sid")))
    (should (eq 'first  (anvil-compact--queue-pop "sid")))
    (should (eq 'second (anvil-compact--queue-pop "sid")))
    (should (null (anvil-compact--queue-pop "sid")))))


;;;; --- Phase 2: on-pre-compact ------------------------------------------

(ert-deftest anvil-compact-test-on-pre-compact-no-snapshot-returns-nil ()
  (anvil-compact-test--with-fresh-state
    (should (null (anvil-compact-on-pre-compact "sid")))))

(ert-deftest anvil-compact-test-on-pre-compact-refreshes-existing ()
  "PreCompact should re-stamp the snapshot so captured-at advances."
  (anvil-compact-test--with-fresh-state
    (anvil-compact-snapshot-capture "sid"
                                    :task-summary "t"
                                    :branch "main"
                                    :percent 50)
    (let ((before (plist-get (anvil-compact-snapshot-get "sid") :captured-at)))
      (sleep-for 1.0)  ; ensure timestamp second bucket advances
      (anvil-compact-on-pre-compact "sid")
      (let ((after (plist-get (anvil-compact-snapshot-get "sid") :captured-at)))
        (should (stringp before))
        (should (stringp after))
        (should-not (equal before after))))))


;;;; --- Phase 2: on-post-compact ------------------------------------------

(ert-deftest anvil-compact-test-on-post-compact-enqueues-and-stamps ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact-snapshot-capture "sid"
                                    :task-summary "post-compact target"
                                    :percent 50)
    (let ((r (anvil-compact-on-post-compact "sid")))
      (should r)
      (should (= 1 (anvil-compact--queue-length "sid")))
      (should (= 50 (anvil-compact--state-get
                     "sid" "last-compact-percent"))))))

(ert-deftest anvil-compact-test-on-post-compact-no-snapshot-nop ()
  (anvil-compact-test--with-fresh-state
    (should (null (anvil-compact-on-post-compact "sid")))
    (should (= 0 (anvil-compact--queue-length "sid")))))


;;;; --- Phase 2: queue consumption by on-session-start / on-user-prompt --

(ert-deftest anvil-compact-test-session-start-prefers-queue ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact-snapshot-capture "sid"
                                    :task-summary "latest snapshot"
                                    :percent 30)
    (anvil-compact--queue-push "sid"
                               (list :captured-at "2026-04-24T00:00:00+0900"
                                     :percent 50
                                     :task-summary "queued restore"
                                     :branch ""
                                     :files nil
                                     :todos nil))
    (let ((out (anvil-compact-on-session-start "sid")))
      (should (string-match-p "queued restore" out))
      ;; Queue was popped, next call falls through to latest snapshot
      (let ((out2 (anvil-compact-on-session-start "sid")))
        (should (string-match-p "latest snapshot" out2))))))

(ert-deftest anvil-compact-test-user-prompt-falls-back-to-queue ()
  "When no pending-nudge flag, UserPromptSubmit should pop a queued
restore (PostCompact path)."
  (anvil-compact-test--with-fresh-state
    (anvil-compact--queue-push "sid"
                               (list :captured-at "2026-04-24T00:00:00+0900"
                                     :percent 50
                                     :task-summary "queued via post-compact"
                                     :branch ""
                                     :files nil
                                     :todos nil))
    (let ((out (anvil-compact-on-user-prompt "sid")))
      (should (string-match-p "queued via post-compact" out)))))


;;;; --- Phase 2: event log embedding -------------------------------------

(ert-deftest anvil-compact-test-snapshot-embeds-explicit-events ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact-snapshot-capture
     "sid"
     :task-summary "x"
     :percent 50
     :events (list (list :kind "tool-use" :summary "file-read")
                   (list :kind "tool-use" :summary "elisp-ert-run")))
    (let* ((snap (anvil-compact-snapshot-get "sid"))
           (events (plist-get snap :events)))
      (should (= 2 (length events)))
      (should (equal "file-read" (plist-get (nth 0 events) :summary))))))

(ert-deftest anvil-compact-test-snapshot-format-renders-events ()
  (let* ((snap (list :captured-at "2026-04-24T00:00:00+0900"
                     :percent 50
                     :task-summary "x"
                     :branch ""
                     :files nil
                     :todos nil
                     :events (list (list :kind "tool-use"
                                         :summary "file-read")
                                   (list :kind "user-prompt"
                                         :summary "next step"))))
         (text (anvil-compact-snapshot-format snap)))
    (should (string-match-p "recent events (2)" text))
    (should (string-match-p "tool-use: file-read" text))
    (should (string-match-p "user-prompt: next step" text))))

(ert-deftest anvil-compact-test-include-event-log-nil-drops-events ()
  (anvil-compact-test--with-fresh-state
    (let ((anvil-compact-include-event-log nil))
      (anvil-compact-snapshot-capture
       "sid" :task-summary "x" :percent 50
       :events (list (list :kind "tool-use" :summary "should-be-dropped"))))
    (let ((snap (anvil-compact-snapshot-get "sid")))
      (should (null (plist-get snap :events))))))


;;;; --- Phase 2: MCP compact-hook new stages -----------------------------

(ert-deftest anvil-compact-test-tool-hook-pre-compact-roundtrip ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact-snapshot-capture "sid" :task-summary "t" :percent 50)
    (let ((out (anvil-compact--tool-hook "sid" "pre-compact" "")))
      (should (stringp out))
      ;; Return is `prin1-to-string' of the refreshed snapshot plist
      (should (string-match-p ":task-summary" out)))))

(ert-deftest anvil-compact-test-tool-hook-post-compact-enqueues ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact-snapshot-capture "sid" :task-summary "t" :percent 50)
    (anvil-compact--tool-hook "sid" "post-compact" "")
    (should (= 1 (anvil-compact--queue-length "sid")))))


;;;; --- Phase 3a: observability events + compact-stats -------------------

(require 'anvil-session)

(defun anvil-compact-test--compact-events (session-id)
  "Return the list of compact.* events for SESSION-ID in chrono order.
Each entry is the event plist stored by `anvil-session-log-event'."
  (cl-remove-if-not
   (lambda (r)
     (let ((k (plist-get r :kind)))
       (and (stringp k) (string-prefix-p "compact." k))))
   (anvil-session--session-events session-id)))

(ert-deftest anvil-compact-test-log-event-no-session-is-nop ()
  "Empty session id should not write an event row."
  (anvil-compact-test--with-fresh-state
    (anvil-compact--log-event "" 'compact.trigger "x")
    (should (null (anvil-session--all-events)))))

(ert-deftest anvil-compact-test-on-stop-trigger-emits-event ()
  (anvil-compact-test--with-fresh-state
    (let ((p (make-temp-file "anvil-compact-")))
      (unwind-protect
          (progn
            (with-temp-file p (insert (make-string 50000 ?x)))
            (let ((anvil-compact-trigger-percent 10)
                  (anvil-compact-cooldown-percent 0)
                  (anvil-compact-context-tokens-max 1000)
                  (anvil-compact-bytes-per-token 1))
              (anvil-compact-on-stop
               "sid-trg" :transcript-path p :task-summary "x")
              (let* ((events (anvil-compact-test--compact-events "sid-trg"))
                     (kinds (mapcar (lambda (e) (plist-get e :kind))
                                    events)))
                (should (member "compact.trigger" kinds))
                (should-not (member "compact.skipped" kinds)))))
        (ignore-errors (delete-file p))))))

(ert-deftest anvil-compact-test-on-stop-below-threshold-emits-skipped ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact-on-stop "sid-skip" :transcript-path nil)
    (let* ((events (anvil-compact-test--compact-events "sid-skip"))
           (kinds (mapcar (lambda (e) (plist-get e :kind)) events))
           (summary (plist-get (car events) :summary)))
      (should (member "compact.skipped" kinds))
      (should-not (member "compact.trigger" kinds))
      (should (string-match-p "reason=below-threshold" summary)))))

(ert-deftest anvil-compact-test-on-user-prompt-nudge-emits-event ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact--state-put "sid-n" "pending-nudge" t)
    (anvil-compact-snapshot-capture "sid-n"
                                    :task-summary "x" :percent 50)
    (anvil-compact-on-user-prompt "sid-n")
    (let ((kinds (mapcar (lambda (e) (plist-get e :kind))
                         (anvil-compact-test--compact-events "sid-n"))))
      (should (member "compact.nudge" kinds)))))

(ert-deftest anvil-compact-test-on-user-prompt-queue-restore-emits-event ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact--queue-push "sid-r"
                               (list :captured-at "t" :percent 50
                                     :task-summary "x" :branch ""
                                     :files nil :todos nil))
    (anvil-compact-on-user-prompt "sid-r")
    (let* ((events (anvil-compact-test--compact-events "sid-r"))
           (kinds (mapcar (lambda (e) (plist-get e :kind)) events)))
      (should (member "compact.restore" kinds)))))

(ert-deftest anvil-compact-test-on-session-start-emits-restore-event ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact-snapshot-capture "sid-s" :task-summary "x" :percent 50)
    (anvil-compact-on-session-start "sid-s")
    (let ((summaries (mapcar (lambda (e) (plist-get e :summary))
                             (anvil-compact-test--compact-events "sid-s"))))
      (should (cl-some (lambda (s) (string-match-p "session-start-fallback" s))
                       summaries)))))

(ert-deftest anvil-compact-test-on-pre-compact-emits-event ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact-snapshot-capture "sid-p" :task-summary "x" :percent 50)
    (anvil-compact-on-pre-compact "sid-p")
    (let ((kinds (mapcar (lambda (e) (plist-get e :kind))
                         (anvil-compact-test--compact-events "sid-p"))))
      (should (member "compact.pre" kinds)))))

(ert-deftest anvil-compact-test-on-post-compact-emits-event ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact-snapshot-capture "sid-pc" :task-summary "x" :percent 50)
    (anvil-compact-on-post-compact "sid-pc")
    (let ((kinds (mapcar (lambda (e) (plist-get e :kind))
                         (anvil-compact-test--compact-events "sid-pc"))))
      (should (member "compact.post" kinds)))))


;;;; --- stats aggregation ------------------------------------------------

(ert-deftest anvil-compact-test-stats-empty-log-returns-zeros ()
  (anvil-compact-test--with-fresh-state
    (let ((s (anvil-compact-stats)))
      (should (= 0 (plist-get s :scanned)))
      (should (= 0 (plist-get s :triggers)))
      (should (null (plist-get s :compliance))))))

(ert-deftest anvil-compact-test-stats-counts-by-kind ()
  (anvil-compact-test--with-fresh-state
    ;; Seed events directly via log-event to avoid cross-function churn
    (anvil-session-log-event "sid" 'compact.trigger :summary "pct=50")
    (anvil-session-log-event "sid" 'compact.trigger :summary "pct=55")
    (anvil-session-log-event "sid" 'compact.skipped
                             :summary "reason=below-threshold pct=10")
    (anvil-session-log-event "sid" 'compact.skipped
                             :summary "reason=cooldown pct=50")
    (anvil-session-log-event "sid" 'compact.nudge :summary "pct=50")
    (anvil-session-log-event "sid" 'compact.restore
                             :summary "source=user-prompt-queue pct=50")
    (let ((s (anvil-compact-stats)))
      (should (= 6 (plist-get s :scanned)))
      (should (= 2 (plist-get s :triggers)))
      (should (= 2 (plist-get s :skipped)))
      (should (= 1 (plist-get s :nudges)))
      (should (= 1 (plist-get s :restores)))
      (should (= 1.0 (plist-get s :compliance)))
      (should (equal 1 (cdr (assoc "below-threshold"
                                   (plist-get s :skip-reasons)))))
      (should (equal 1 (cdr (assoc 50
                                   (plist-get s :trigger-pct))))))))

(ert-deftest anvil-compact-test-stats-scoped-to-session ()
  (anvil-compact-test--with-fresh-state
    (anvil-session-log-event "sid-A" 'compact.trigger :summary "pct=50")
    (anvil-session-log-event "sid-B" 'compact.trigger :summary "pct=60")
    (let ((s (anvil-compact-stats :session-id "sid-A")))
      (should (= 1 (plist-get s :scanned)))
      (should (equal '("sid-A") (plist-get s :session-ids))))))

(ert-deftest anvil-compact-test-stats-since-ts-drops-older-events ()
  (anvil-compact-test--with-fresh-state
    (let ((now (float-time)))
      (anvil-session-log-event "sid" 'compact.trigger
                               :summary "pct=50"
                               :ts (- now 1000))
      (anvil-session-log-event "sid" 'compact.trigger
                               :summary "pct=55"
                               :ts now)
      (let* ((s (anvil-compact-stats :since-ts (- now 10)))
             (last (plist-get s :last-trigger)))
        (should (= 1 (plist-get s :scanned)))
        (should (equal "pct=55" (plist-get last :summary)))
        (should (numberp (plist-get last :ts)))))))

(ert-deftest anvil-compact-test-stats-compliance-nil-when-no-nudges ()
  (anvil-compact-test--with-fresh-state
    (anvil-session-log-event "sid" 'compact.trigger :summary "pct=50")
    (let ((s (anvil-compact-stats)))
      (should (null (plist-get s :compliance))))))


;;;; --- MCP compact-stats tool -------------------------------------------

(ert-deftest anvil-compact-test-tool-stats-accepts-empty-strings ()
  (anvil-compact-test--with-fresh-state
    (anvil-session-log-event "sid" 'compact.trigger :summary "pct=50")
    (let ((s (anvil-compact--tool-stats "" "")))
      (should (= 1 (plist-get s :triggers))))))

(ert-deftest anvil-compact-test-tool-stats-session-scope ()
  (anvil-compact-test--with-fresh-state
    (anvil-session-log-event "sid-A" 'compact.trigger :summary "pct=50")
    (anvil-session-log-event "sid-B" 'compact.trigger :summary "pct=50")
    (let ((s (anvil-compact--tool-stats "sid-A" "")))
      (should (= 1 (plist-get s :triggers))))))

;;; anvil-compact-test.el ends here
