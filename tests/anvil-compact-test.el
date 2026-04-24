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

(ert-deftest anvil-compact-test-on-user-prompt-flag-emits-json ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact--state-put "sid" "pending-nudge" t)
    (anvil-compact-snapshot-capture "sid"
                                    :task-summary "implement Phase 1"
                                    :percent 50)
    (let* ((out (anvil-compact-on-user-prompt "sid"))
           (parsed (json-parse-string out :object-type 'alist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object nil)))
      (let* ((hso (alist-get 'hookSpecificOutput parsed))
             (ctx (alist-get 'additionalContext hso)))
        (should (equal "UserPromptSubmit" (alist-get 'hookEventName hso)))
        (should (string-match-p "auto-compact" ctx))
        (should (string-match-p "/compact" ctx))
        (should (string-match-p "implement Phase 1" ctx))))))

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

(ert-deftest anvil-compact-test-on-session-start-emits-json-preamble ()
  (anvil-compact-test--with-fresh-state
    (anvil-compact-snapshot-capture "sid"
                                    :task-summary "cross-session task"
                                    :branch "feat/x"
                                    :percent 50)
    (let* ((out (anvil-compact-on-session-start "sid"))
           (parsed (json-parse-string out :object-type 'alist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object nil)))
      (let* ((hso (alist-get 'hookSpecificOutput parsed))
             (ctx (alist-get 'additionalContext hso)))
        (should (equal "SessionStart" (alist-get 'hookEventName hso)))
        (should (string-match-p "cross-session task" ctx))
        (should (string-match-p "feat/x" ctx))))))


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
    (let* ((out (anvil-compact-on-session-start "sid"))
           (parsed (json-parse-string out :object-type 'alist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object nil)))
      (let* ((hso (alist-get 'hookSpecificOutput parsed))
             (ctx (alist-get 'additionalContext hso)))
        (should (string-match-p "queued restore" ctx))
        ;; Queue was popped, next call falls through to latest snapshot
        (let* ((out2 (anvil-compact-on-session-start "sid"))
               (parsed2 (json-parse-string out2 :object-type 'alist
                                                :array-type 'list
                                                :null-object nil
                                                :false-object nil)))
          (should (string-match-p "latest snapshot"
                                  (alist-get 'additionalContext
                                             (alist-get 'hookSpecificOutput
                                                        parsed2)))))))))

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
    (let* ((out (anvil-compact-on-user-prompt "sid"))
           (parsed (json-parse-string out :object-type 'alist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object nil)))
      (let ((ctx (alist-get 'additionalContext
                            (alist-get 'hookSpecificOutput parsed))))
        (should (string-match-p "queued via post-compact" ctx))))))


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

;;; anvil-compact-test.el ends here
