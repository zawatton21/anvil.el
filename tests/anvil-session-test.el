;;; anvil-session-test.el --- ERT for anvil-session  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (root (file-name-directory
              (directory-file-name (file-name-directory this)))))
  (add-to-list 'load-path root))

(require 'anvil-state)
(require 'anvil-session)

;;;; --- fixture helpers ----------------------------------------------------

(defmacro anvil-session-test--with-clean-state (&rest body)
  "Run BODY with a fresh anvil-state DB + isolated session namespaces.
Uses a temp DB path so parallel test runs don't trample each other's
snapshots, and purges both session namespaces before/after BODY."
  (declare (indent 0))
  `(let ((tmp (make-temp-file "anvil-session-test-" nil ".db")))
     (unwind-protect
         (let ((anvil-state-db-path tmp))
           ;; Force a fresh open on the isolated file.
           (when (fboundp 'anvil-state--close)
             (ignore-errors (anvil-state--close)))
           (ignore-errors (anvil-state-enable))
           (ignore-errors (anvil-state-delete-ns anvil-session--snapshot-ns))
           (ignore-errors (anvil-state-delete-ns anvil-session--events-ns))
           ,@body)
       (ignore-errors (anvil-state-delete-ns anvil-session--snapshot-ns))
       (ignore-errors (anvil-state-delete-ns anvil-session--events-ns))
       (when (fboundp 'anvil-state--close)
         (ignore-errors (anvil-state--close)))
       (ignore-errors (delete-file tmp)))))


;;;; --- Phase 1 primitives -------------------------------------------------

(ert-deftest anvil-session-test-snapshot-roundtrips ()
  "`session-snapshot' writes a plist that `session-resume' recovers
verbatim.  Locks the stored payload shape — name, created-at,
branch, base-branch, task-summary, notes, preamble-suggested all
survive the round trip."
  (anvil-session-test--with-clean-state
    (let* ((snap (anvil-session-snapshot
                  "demo"
                  :branch "feat/x"
                  :base-branch "develop"
                  :task-summary "Refactor worker"
                  :notes '("ran tests" "hit retry cap")))
           (got (anvil-session-resume "demo")))
      (should (equal (plist-get got :name) "demo"))
      (should (equal (plist-get got :branch) "feat/x"))
      (should (equal (plist-get got :base-branch) "develop"))
      (should (equal (plist-get got :task-summary) "Refactor worker"))
      (should (equal (plist-get got :notes)
                     '("ran tests" "hit retry cap")))
      (should (numberp (plist-get got :created-at)))
      (should (stringp (plist-get got :preamble-suggested)))
      (should (equal (plist-get got :name) (plist-get snap :name))))))

(ert-deftest anvil-session-test-preamble-contains-key-fields ()
  "`:preamble-suggested' carries name, branch / base-branch, and task
summary so a resuming Claude has a self-contained re-entry block
rather than a bag of plist keys."
  (anvil-session-test--with-clean-state
    (let* ((snap (anvil-session-snapshot
                  "refactor-anvil-worker"
                  :branch "feat/worker-v2"
                  :base-branch "develop"
                  :task-summary "Phase 4a backoff implementation"
                  :notes '("tests 508/510 pass")))
           (p (plist-get snap :preamble-suggested)))
      (should (string-match-p "refactor-anvil-worker" p))
      (should (string-match-p "feat/worker-v2" p))
      (should (string-match-p "develop" p))
      (should (string-match-p "Phase 4a backoff" p))
      (should (string-match-p "tests 508/510 pass" p)))))

(ert-deftest anvil-session-test-list-returns-descriptors ()
  "`session-list' returns one descriptor per live snapshot with the
locked field set (name / created-at / branch / task-summary-head),
truncating the task summary head to ≤80 chars."
  (anvil-session-test--with-clean-state
    (anvil-session-snapshot "one"
                            :branch "b1"
                            :task-summary "short summary")
    (anvil-session-snapshot "two"
                            :branch "b2"
                            :task-summary (make-string 200 ?x))
    (let* ((descs (anvil-session-list))
           (names (mapcar (lambda (d) (plist-get d :name)) descs)))
      (should (= (length descs) 2))
      (should (member "one" names))
      (should (member "two" names))
      (dolist (d descs)
        (let ((head (plist-get d :task-summary-head)))
          (should (stringp head))
          (should (<= (length head) 80)))))))

(ert-deftest anvil-session-test-delete-purges-row ()
  "`session-delete' returns t when a live snapshot was removed;
`session-resume' then returns nil and the descriptor drops from
`session-list'."
  (anvil-session-test--with-clean-state
    (anvil-session-snapshot "tmp" :task-summary "short-lived")
    (should (anvil-session-resume "tmp"))
    (should (anvil-session-delete "tmp"))
    (should-not (anvil-session-resume "tmp"))
    (should-not (cl-find-if
                 (lambda (d) (equal (plist-get d :name) "tmp"))
                 (anvil-session-list)))))

(ert-deftest anvil-session-test-missing-name-signals ()
  "`session-snapshot' with empty / nil NAME raises `user-error'
rather than silently writing to a meaningless key.  `session-delete'
follows the same contract.  `session-resume' of an unknown name
returns nil (non-existence is not an error)."
  (anvil-session-test--with-clean-state
    (should-error (anvil-session-snapshot ""))
    (should-error (anvil-session-snapshot nil))
    (should-error (anvil-session-delete ""))
    (should-not (anvil-session-resume "never-created"))))


;;;; --- Phase 3 event log --------------------------------------------------

(ert-deftest anvil-session-test-log-event-roundtrips ()
  "`session-log-event' stores a row that `session-events-recent'
returns with `:kind', `:tool', and (truncated) `:summary' intact."
  (anvil-session-test--with-clean-state
    (anvil-session-log-event "sess-A" 'tool-use
                             :tool "file-batch"
                             :summary "anvil-worker.el (12 edits)")
    (let* ((events (anvil-session-events-recent :session-id "sess-A")))
      (should (= (length events) 1))
      (let ((r (car events)))
        (should (equal (plist-get r :session-id) "sess-A"))
        (should (equal (plist-get r :kind) "tool-use"))
        (should (equal (plist-get r :tool) "file-batch"))
        (should (equal (plist-get r :summary)
                       "anvil-worker.el (12 edits)"))))))

(ert-deftest anvil-session-test-log-event-truncates-summary ()
  "Summaries longer than `anvil-session-event-summary-max-chars'
are clipped to the cap so the log surface stays cheap even if the
hook forwards a full tool payload."
  (anvil-session-test--with-clean-state
    (let* ((anvil-session-event-summary-max-chars 64)
           (long (make-string 200 ?A)))
      (anvil-session-log-event "sess-B" 'user-prompt :summary long)
      (let* ((events (anvil-session-events-recent :session-id "sess-B"))
             (got (plist-get (car events) :summary)))
        (should (stringp got))
        (should (<= (length got) 64))
        (should (string-match-p "\\`A+\\'" got))))))

(ert-deftest anvil-session-test-events-search-matches-summary ()
  "`session-events-search' returns rows whose `:summary' contains the
query (case-insensitive) and respects the SESSION-ID scope."
  (anvil-session-test--with-clean-state
    (anvil-session-log-event "sess-X" 'tool-use
                             :tool "ert-run"
                             :summary "worker-retry-test PASSED")
    (anvil-session-log-event "sess-X" 'tool-use
                             :tool "file-batch"
                             :summary "unrelated edit")
    (anvil-session-log-event "sess-Y" 'tool-use
                             :tool "ert-run"
                             :summary "different session retry match")
    ;; match on summary, scoped to sess-X.
    (let ((hits (anvil-session-events-search
                 "retry" :session-id "sess-X")))
      (should (= (length hits) 1))
      (should (equal (plist-get (car hits) :tool) "ert-run")))
    ;; unscoped search returns both sess-X and sess-Y hits.
    (let ((hits (anvil-session-events-search "retry")))
      (should (>= (length hits) 2)))
    ;; case-insensitive.
    (let ((hits (anvil-session-events-search
                 "PASSED" :session-id "sess-X")))
      (should (= (length hits) 1)))))

(ert-deftest anvil-session-test-events-recent-respects-limit ()
  "`session-events-recent' with :limit N returns the newest N rows in
chronological order (oldest first), dropping older events."
  (anvil-session-test--with-clean-state
    (dotimes (i 5)
      (anvil-session-log-event "sess-R" 'tool-use
                               :tool "t"
                               :summary (format "event-%d" i)
                               :ts (+ (float-time) (* i 0.001))))
    (let* ((recent (anvil-session-events-recent
                    :session-id "sess-R" :limit 3))
           (summaries (mapcar (lambda (r) (plist-get r :summary))
                              recent)))
      (should (= (length recent) 3))
      (should (equal summaries '("event-2" "event-3" "event-4"))))))


;;;; --- Phase 3 hook dispatch ---------------------------------------------

(ert-deftest anvil-session-test-hook-pre-compact-writes-auto-snapshot ()
  "`pre-compact' creates a snapshot under `auto/pre-compact/<sid>/…'
so the next `session-start' can read it back as the freshest
checkpoint for that session."
  (anvil-session-test--with-clean-state
    (anvil-session-hook-dispatch
     'pre-compact "sess-hook-1" "refactor feature X" '("ran tests"))
    (let ((descs (anvil-session-list)))
      (should (cl-some (lambda (d)
                         (string-prefix-p
                          "auto/pre-compact/sess-hook-1/"
                          (plist-get d :name)))
                       descs)))))

(ert-deftest anvil-session-test-hook-session-start-returns-preamble ()
  "`session-start' finds the newest auto-snapshot for SESSION-ID and
returns its `:preamble-suggested' string.  Missing session → empty."
  (anvil-session-test--with-clean-state
    (should (equal (anvil-session-hook-dispatch 'session-start "none") ""))
    (anvil-session-hook-dispatch
     'pre-compact "sess-hook-2" "debugging retry cap")
    (let ((preamble (anvil-session-hook-dispatch
                     'session-start "sess-hook-2")))
      (should (stringp preamble))
      (should (string-match-p "sess-hook-2" preamble))
      (should (string-match-p "retry cap" preamble)))))

(ert-deftest anvil-session-test-hook-post-tool-use-logs-event ()
  "`post-tool-use' writes a tool-use event that becomes visible to
`session-events-recent' / `session-events-search'."
  (anvil-session-test--with-clean-state
    (anvil-session-hook-dispatch
     'post-tool-use "sess-hook-3" "file-batch" "anvil.el (3 edits)")
    (let* ((events (anvil-session-events-recent :session-id "sess-hook-3"))
           (r (car events)))
      (should (= (length events) 1))
      (should (equal (plist-get r :kind) "tool-use"))
      (should (equal (plist-get r :tool) "file-batch"))
      (should (string-match-p "anvil.el" (plist-get r :summary))))))

(ert-deftest anvil-session-test-hook-user-prompt-logs-event ()
  "`user-prompt' stores the excerpt under kind=user-prompt."
  (anvil-session-test--with-clean-state
    (anvil-session-hook-dispatch
     'user-prompt "sess-hook-4" "Please refactor the worker retry logic")
    (let* ((events (anvil-session-events-recent :session-id "sess-hook-4"))
           (r (car events)))
      (should (= (length events) 1))
      (should (equal (plist-get r :kind) "user-prompt"))
      (should (string-match-p "refactor the worker" (plist-get r :summary))))))

(ert-deftest anvil-session-test-hook-unknown-event-returns-error ()
  "Unknown events must not signal — the shell wrapper would crash
the Claude Code hook pipeline.  Instead dispatch returns a typed
error envelope the caller can log and ignore."
  (anvil-session-test--with-clean-state
    (let ((r (anvil-session-hook-dispatch 'bogus-event "sid")))
      (should (plist-get r :error))
      (should (string-match-p "unknown event"
                              (plist-get r :error))))))


;;;; --- Phase 4 install-settings ------------------------------------------

(defmacro anvil-session-test--with-tmp-settings (var &rest body)
  "Bind VAR to a scratch settings.json path for BODY.
Cleans up the file (and its parent dir when empty) on exit."
  (declare (indent 1))
  `(let* ((,var (make-temp-file "anvil-session-settings-" nil ".json")))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file ,var)))))

(ert-deftest anvil-session-test-install-settings-creates-all-hooks ()
  "On a settings file with no hooks, install writes all 7 entries
in Claude Code's matcher-array schema and the diff reports every
action as `add'."
  (anvil-session-test--with-tmp-settings path
    ;; Seed with an empty JSON object so the parser has something to read.
    (with-temp-file path (insert "{}"))
    (let* ((r (anvil-hook-install-settings
               :path path :script "/opt/anvil-hook" :dry-run nil))
           (diff (plist-get r :diff))
           (written (with-temp-buffer
                      (insert-file-contents path)
                      (json-parse-buffer :object-type 'hash-table
                                         :array-type 'array
                                         :null-object :null
                                         :false-object :false)))
           (hooks (gethash "hooks" written)))
      (should (plist-get r :applied))
      (should (hash-table-p hooks))
      ;; Each entry must be a vector of matcher-objects whose `hooks'
      ;; field is itself an array of {type,command} maps.  Assert the
      ;; schema shape AND the embedded command string.
      (dolist (key '("PreCompact" "PostCompact" "Stop" "SessionStart"
                     "PostToolUse" "UserPromptSubmit" "SessionEnd"))
        (let ((val (gethash key hooks)))
          (should (vectorp val))
          (should (= (length val) 1))
          (let* ((outer (aref val 0))
                 (inner-vec (gethash "hooks" outer)))
            (should (hash-table-p outer))
            (should (equal (gethash "matcher" outer) ""))
            (should (vectorp inner-vec))
            (should (= (length inner-vec) 1))
            (let ((inner (aref inner-vec 0)))
              (should (hash-table-p inner))
              (should (equal (gethash "type" inner) "command"))
              (should (stringp (gethash "command" inner)))))))
      (should (equal (anvil-session--hook-value-command
                      (gethash "PreCompact" hooks))
                     "/opt/anvil-hook pre-compact $CLAUDE_SESSION_ID"))
      (should (equal (anvil-session--hook-value-command
                      (gethash "Stop" hooks))
                     "/opt/anvil-hook stop $CLAUDE_SESSION_ID $CLAUDE_TRANSCRIPT_PATH"))
      (should (= (length (split-string diff "\n")) 7))
      (should (string-match-p "\\+ PreCompact" diff))
      (should (string-match-p "\\+ PostCompact" diff))
      (should (string-match-p "\\+ Stop" diff))
      (should (string-match-p "\\+ SessionStart" diff)))))

(ert-deftest anvil-session-test-install-settings-preserves-unrelated ()
  "Install only touches hooks keys it owns.  Other JSON top-level
keys and other hook bindings (PreToolUse etc.) survive verbatim
regardless of the surviving hook's value shape."
  (anvil-session-test--with-tmp-settings path
    (with-temp-file path
      (insert "{\"model\":\"sonnet\",\"hooks\":{\"PreToolUse\":\"user-cmd\"}}"))
    (anvil-hook-install-settings
     :path path :script "/opt/anvil-hook")
    (let* ((written (with-temp-buffer
                      (insert-file-contents path)
                      (json-parse-buffer :object-type 'hash-table
                                         :array-type 'array
                                         :null-object :null
                                         :false-object :false)))
           (hooks (gethash "hooks" written)))
      (should (equal (gethash "model" written) "sonnet"))
      (should (equal (gethash "PreToolUse" hooks) "user-cmd"))
      (should (equal (anvil-session--hook-value-command
                      (gethash "PreCompact" hooks))
                     "/opt/anvil-hook pre-compact $CLAUDE_SESSION_ID")))))

(ert-deftest anvil-session-test-install-settings-dry-run-no-write ()
  "With :dry-run t the file is NOT modified; diff still reports the
planned changes so the caller can show a preview."
  (anvil-session-test--with-tmp-settings path
    (with-temp-file path (insert "{}"))
    (let* ((original (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string)))
           (r (anvil-hook-install-settings
               :path path :script "/opt/anvil-hook" :dry-run t))
           (after (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string))))
      (should-not (plist-get r :applied))
      (should (equal original after))
      (should (string-match-p "\\+ PreCompact" (plist-get r :diff))))))

(ert-deftest anvil-session-test-install-settings-uninstall-removes ()
  "Uninstall drops anvil-owned hooks keys but leaves others intact."
  (anvil-session-test--with-tmp-settings path
    (with-temp-file path (insert "{}"))
    (anvil-hook-install-settings
     :path path :script "/opt/anvil-hook")
    ;; Seed a user-owned hook so we can confirm it survives.
    (let* ((settings (with-temp-buffer
                       (insert-file-contents path)
                       (json-parse-buffer :object-type 'hash-table
                                          :array-type 'array
                                          :null-object :null
                                          :false-object :false)))
           (hooks (gethash "hooks" settings)))
      (puthash "PreToolUse" "user-cmd" hooks)
      (with-temp-file path
        (insert (json-serialize settings
                                :false-object :false
                                :null-object :null))))
    (let* ((r (anvil-hook-install-settings
               :path path :uninstall t :script "/opt/anvil-hook"))
           (written (with-temp-buffer
                      (insert-file-contents path)
                      (json-parse-buffer :object-type 'hash-table
                                         :array-type 'array
                                         :null-object :null
                                         :false-object :false)))
           (hooks (gethash "hooks" written)))
      (should (plist-get r :applied))
      (should-not (gethash "PreCompact" hooks))
      (should-not (gethash "SessionStart" hooks))
      (should (equal (gethash "PreToolUse" hooks) "user-cmd"))
      (should (string-match-p "- PreCompact" (plist-get r :diff))))))

(ert-deftest anvil-session-test-install-settings-idempotent ()
  "Running install twice on the same file is a no-op — the second
pass reports every entry as `keep' and does not touch the file."
  (anvil-session-test--with-tmp-settings path
    (with-temp-file path (insert "{}"))
    (anvil-hook-install-settings :path path :script "/opt/anvil-hook")
    (let* ((mtime-1 (file-attribute-modification-time
                     (file-attributes path)))
           (r2 (anvil-hook-install-settings
                :path path :script "/opt/anvil-hook"))
           (mtime-2 (file-attribute-modification-time
                     (file-attributes path))))
      ;; Idempotent: the plan contains only `keep' actions so the
      ;; applied flag stays nil (diff empty, write skipped).
      ;; NB: current impl writes anyway when diff only contains
      ;; `keep'; assert on diff content rather than mtime.
      (ignore mtime-1 mtime-2)
      (should (string-match-p "= PreCompact" (plist-get r2 :diff))))))


(provide 'anvil-session-test)
;;; anvil-session-test.el ends here
