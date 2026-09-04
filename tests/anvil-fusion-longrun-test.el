;;; anvil-fusion-longrun-test.el --- tests for anvil-fusion-longrun -*- lexical-binding: t; -*-

;;; Commentary:
;; Phase 1 spike tests.  The loop is exercised with injected step-fn /
;; distill-fn so no orchestrator (and no real model) is touched: the
;; tests are fast, deterministic, and focus on the bounded-context
;; handoff invariant and context isolation.

;;; Code:

(require 'ert)
(require 'anvil-fusion-longrun)

;;;; --- pure prompt builders ------------------------------------------------

(ert-deftest anvil-fusion-longrun-test-step-prompt-includes-goal-and-digest ()
  (let ((p (anvil-fusion-longrun-build-step-prompt "GOAL-X" "DIGEST-Y" 2 5)))
    (should (string-match-p "GOAL-X" p))
    (should (string-match-p "DIGEST-Y" p))
    (should (string-match-p "2 / 最大 5" p))))

(ert-deftest anvil-fusion-longrun-test-step-prompt-nil-digest ()
  (let ((p (anvil-fusion-longrun-build-step-prompt "G" nil 1 3)))
    (should (string-match-p "(まだ無し)" p))))

(ert-deftest anvil-fusion-longrun-test-distill-prompt-includes-all ()
  (let ((p (anvil-fusion-longrun-build-distill-prompt "GG" "PREV" "OUT" 1234)))
    (should (string-match-p "GG" p))
    (should (string-match-p "PREV" p))
    (should (string-match-p "OUT" p))
    (should (string-match-p "1234" p))))

;;;; --- distill parsing -----------------------------------------------------

(ert-deftest anvil-fusion-longrun-test-parse-done ()
  (let ((r (anvil-fusion-longrun--parse-distill "state body\nSTATUS: DONE" 100)))
    (should (plist-get r :done))
    (should-not (plist-get r :next-hard))
    (should (equal (plist-get r :digest) "state body"))))

(ert-deftest anvil-fusion-longrun-test-parse-continue ()
  (let ((r (anvil-fusion-longrun--parse-distill "state body\nSTATUS: CONTINUE" 100)))
    (should-not (plist-get r :done))
    (should-not (plist-get r :next-hard))
    (should (equal (plist-get r :digest) "state body"))))

(ert-deftest anvil-fusion-longrun-test-parse-no-status ()
  (let ((r (anvil-fusion-longrun--parse-distill "  just state  " 100)))
    (should-not (plist-get r :done))
    (should-not (plist-get r :next-hard))
    (should (equal (plist-get r :digest) "just state"))))

(ert-deftest anvil-fusion-longrun-test-parse-next-hard-yes ()
  (let ((r (anvil-fusion-longrun--parse-distill
            "state body\nNEXT-HARD: yes\nSTATUS: CONTINUE" 100)))
    (should-not (plist-get r :done))
    (should (plist-get r :next-hard))
    (should (equal (plist-get r :digest) "state body"))))

(ert-deftest anvil-fusion-longrun-test-parse-next-hard-no ()
  (let ((r (anvil-fusion-longrun--parse-distill
            "state body\nNEXT-HARD: no\nSTATUS: CONTINUE" 100)))
    (should-not (plist-get r :done))
    (should-not (plist-get r :next-hard))
    (should (equal (plist-get r :digest) "state body"))))

(ert-deftest anvil-fusion-longrun-test-parse-next-hard-lowercase ()
  (let ((r (anvil-fusion-longrun--parse-distill
            "state body\nnext-hard: yes\nstatus: continue" 100)))
    (should-not (plist-get r :done))
    (should (plist-get r :next-hard))
    (should (equal (plist-get r :digest) "state body"))))

(ert-deftest anvil-fusion-longrun-test-parse-truncates ()
  (let* ((big (concat (make-string 200 ?x) "\nSTATUS: CONTINUE"))
         (r   (anvil-fusion-longrun--parse-distill big 50)))
    (should (= (length (plist-get r :digest)) 50))))

(ert-deftest anvil-fusion-longrun-test-parse-done-only-trailing ()
  "Only a trailing STATUS: DONE terminates; a DONE inside the body does not.
Regression: cataloguing a doc that itself documents the STATUS
protocol put the literal \"STATUS: DONE\" into the digest body and
falsely stopped the quest at step 1."
  ;; mid-body DONE + trailing CONTINUE -> not done, body kept verbatim
  (let ((r (anvil-fusion-longrun--parse-distill
            "課題: 早期 STATUS: DONE 判定\n本文\nSTATUS: CONTINUE" 200)))
    (should-not (plist-get r :done))
    (should (string-match-p "STATUS: DONE" (plist-get r :digest))))
  ;; mid-body DONE, no trailing marker -> not done
  (let ((r (anvil-fusion-longrun--parse-distill
            "本文に STATUS: DONE という語が出るだけ" 200)))
    (should-not (plist-get r :done)))
  ;; trailing DONE still terminates
  (let ((r (anvil-fusion-longrun--parse-distill "本文\nSTATUS: DONE" 200)))
    (should (plist-get r :done))))

(ert-deftest anvil-fusion-longrun-test-parse-next-hard-only-trailing ()
  "Only a trailing NEXT-HARD line sets the hardness flag."
  ;; mid-body NEXT-HARD + trailing no -> false, body kept verbatim
  (let ((r (anvil-fusion-longrun--parse-distill
            "本文に NEXT-HARD: yes が出る\nNEXT-HARD: no\nSTATUS: CONTINUE" 200)))
    (should-not (plist-get r :next-hard))
    (should (string-match-p "NEXT-HARD: yes" (plist-get r :digest))))
  ;; mid-body NEXT-HARD with no trailing status-bound marker -> false
  (let ((r (anvil-fusion-longrun--parse-distill
            "本文に NEXT-HARD: yes という語が出るだけ\nSTATUS: CONTINUE" 200)))
    (should-not (plist-get r :next-hard)))
  ;; trailing yes still sets the flag
  (let ((r (anvil-fusion-longrun--parse-distill
            "本文\nNEXT-HARD: yes\nSTATUS: CONTINUE" 200)))
    (should (plist-get r :next-hard))))

;;;; --- loop: termination ---------------------------------------------------

(defun anvil-fusion-longrun-test--const-distill (body done-step)
  "Return a distill-fn that emits BODY-<n> and DONE at DONE-STEP."
  (lambda (_prompt step-n)
    (format "%s-%d\nSTATUS: %s"
            body step-n
            (if (and done-step (>= step-n done-step)) "DONE" "CONTINUE"))))

(ert-deftest anvil-fusion-longrun-test-run-budget-stop ()
  (let ((r (anvil-fusion-longrun-run
            "g"
            :max-steps 3
            :step-fn (lambda (_p _n) "out")
            :distill-fn (anvil-fusion-longrun-test--const-distill "DIGEST" nil))))
    (should (= (plist-get r :steps) 3))
    (should (eq (plist-get r :stopped) 'budget))
    (should (equal (plist-get r :answer) "DIGEST-3"))))

(ert-deftest anvil-fusion-longrun-test-run-early-done ()
  (let ((r (anvil-fusion-longrun-run
            "g"
            :max-steps 9
            :step-fn (lambda (_p _n) "out")
            :distill-fn (anvil-fusion-longrun-test--const-distill "DIGEST" 2))))
    (should (= (plist-get r :steps) 2))
    (should (eq (plist-get r :stopped) 'done))))

(ert-deftest anvil-fusion-longrun-test-panel-hard-only-gates-on-previous-distill ()
  (let (steps-used panel-steps)
    (let ((r (anvil-fusion-longrun-run
              "g"
              :max-steps 3
              :step-panel 'opus-solo
              :step-fn (lambda (_prompt step-n)
                         (push step-n steps-used)
                         (format "STEP-%d" step-n))
              :panel-step-fn (lambda (_prompt step-n)
                               (push step-n panel-steps)
                               (format "PANEL-%d" step-n))
              :distill-fn (lambda (_prompt step-n)
                            (format "DIGEST-%d\n%s\nSTATUS: CONTINUE"
                                    step-n
                                    (if (= step-n 1)
                                        "NEXT-HARD: yes"
                                      "NEXT-HARD: no"))))))
      (should (equal (nreverse steps-used) '(1 3)))
      (should (equal (nreverse panel-steps) '(2)))
      (should (= (plist-get r :panel-steps) 1)))))

(ert-deftest anvil-fusion-longrun-test-panel-always-mode-uses-budgeted-every-step ()
  (let (panel-steps)
    (let ((r (let ((anvil-fusion-longrun-max-panel-steps 3))
               (anvil-fusion-longrun-run
                "g"
                :max-steps 3
                :step-panel 'opus-solo
                :step-panel-mode 'always
                :step-fn (lambda (_prompt step-n) (format "STEP-%d" step-n))
                :panel-step-fn (lambda (_prompt step-n)
                                 (push step-n panel-steps)
                                 (format "PANEL-%d" step-n))
                :distill-fn (anvil-fusion-longrun-test--const-distill "DIGEST" nil)))))
      (should (equal (nreverse panel-steps) '(1 2 3)))
      (should (= (plist-get r :panel-steps) 3)))))

(ert-deftest anvil-fusion-longrun-test-panel-budget-cap-falls-back ()
  (let ((anvil-fusion-longrun-max-panel-steps 1)
        steps-used
        panel-steps)
    (let ((r (anvil-fusion-longrun-run
              "g"
              :max-steps 3
              :step-panel 'opus-solo
              :step-fn (lambda (_prompt step-n)
                         (push step-n steps-used)
                         (format "STEP-%d" step-n))
              :panel-step-fn (lambda (_prompt step-n)
                               (push step-n panel-steps)
                               (format "PANEL-%d" step-n))
              :distill-fn (lambda (_prompt step-n)
                            (format "DIGEST-%d\n%s\nSTATUS: CONTINUE"
                                    step-n
                                    (if (< step-n 3)
                                        "NEXT-HARD: yes"
                                      "NEXT-HARD: no"))))))
      (should (equal (nreverse panel-steps) '(2)))
      (should (equal (nreverse steps-used) '(1 3)))
      (should (= (plist-get r :panel-steps) 1)))))

(ert-deftest anvil-fusion-longrun-test-panel-off-by-default ()
  (let ((r (anvil-fusion-longrun-run
            "g"
            :max-steps 2
            :step-fn (lambda (_p _n) "out")
            :distill-fn (anvil-fusion-longrun-test--const-distill "DIGEST" nil))))
    (should (= (plist-get r :panel-steps) 0))
    (should (equal (plist-get r :answer) "DIGEST-2"))))

;;;; --- verify gate (Doc 61 Phase 8c) --------------------------------------

(ert-deftest anvil-fusion-longrun-test-verify-gate-pass-through ()
  (let ((r (anvil-fusion-longrun-run
            "goal"
            :max-steps 1
            :verify-steps t
            :step-fn (lambda (_prompt _step) "out")
            :distill-fn (lambda (_prompt _step) "DIGEST-PASS\nSTATUS: DONE")
            :verify-fn (lambda (_question digest)
                         (list (list :claim digest :verdict 'confirmed))))))
    (should (equal (plist-get r :answer) "DIGEST-PASS"))
    (should (= (plist-get r :gate-failures) 0))
    (should (eq (plist-get (car (plist-get r :trace)) :gate) 'pass))))

(ert-deftest anvil-fusion-longrun-test-verify-gate-retry-then-pass ()
  (let (prompts step-nos)
    (let ((r (anvil-fusion-longrun-run
              "goal"
              :max-steps 1
              :verify-steps t
              :step-fn (lambda (prompt step-n)
                         (push prompt prompts)
                         (push step-n step-nos)
                         (if (= (length prompts) 1) "STEP-FIRST" "STEP-RETRY"))
              :distill-fn (lambda (prompt _step)
                            (if (string-match-p "STEP-RETRY" prompt)
                                "DIGEST-RETRY\nSTATUS: DONE"
                              "DIGEST-FIRST\nSTATUS: DONE"))
              :verify-fn (lambda (_question digest)
                           (if (equal digest "DIGEST-FIRST")
                               (list (list :claim "bad fact"
                                           :evidence "proof"
                                           :verdict 'refuted))
                             (list (list :claim digest :verdict 'confirmed)))))))
      (setq prompts (nreverse prompts))
      (setq step-nos (nreverse step-nos))
      (should (equal step-nos '(1 1)))
      (should (equal (plist-get r :answer) "DIGEST-RETRY"))
      (should (eq (plist-get (car (plist-get r :trace)) :gate) 'retried-pass))
      (should (string-match-p "前回試行への反証" (nth 1 prompts)))
      (should (string-match-p "bad fact" (nth 1 prompts))))))

(ert-deftest anvil-fusion-longrun-test-verify-gate-retry-then-fail ()
  (let ((calls 0))
    (let ((r (anvil-fusion-longrun-run
              "goal"
              :max-steps 2
              :verify-steps t
              :step-fn (lambda (_prompt step-n)
                         (cl-incf calls)
                         (format "STEP-%d-%d" step-n calls))
              :distill-fn (lambda (prompt step-n)
                            (if (string-match-p "STEP-1-2" prompt)
                                "DIGEST-FAILED\nSTATUS: CONTINUE"
                              (format "DIGEST-%d\nSTATUS: %s"
                                      step-n (if (= step-n 2) "DONE" "CONTINUE"))))
              :verify-fn (lambda (_question digest)
                           (if (member digest '("DIGEST-1" "DIGEST-FAILED"))
                               (list (list :claim "wrong number"
                                           :evidence "source"
                                           :verdict 'refuted))
                             (list (list :claim digest :verdict 'confirmed)))))))
      (should (= calls 3))
      (should (= (plist-get r :gate-failures) 1))
      (should (equal (plist-get r :answer) "DIGEST-2"))
      (let ((meta (car (plist-get r :trace))))
        (should (eq (plist-get meta :gate) 'failed))
        (should (equal (plist-get meta :refuted) '("wrong number")))))))

(ert-deftest anvil-fusion-longrun-test-verify-gate-off-by-default ()
  (let ((calls 0))
    (let ((r (anvil-fusion-longrun-run
              "goal"
              :max-steps 1
              :step-fn (lambda (_prompt _step) "out")
              :distill-fn (lambda (_prompt _step) "DIGEST\nSTATUS: DONE")
              :verify-fn (lambda (_question _digest)
                           (cl-incf calls)
                           nil))))
      (should (= calls 0))
      (should (= (plist-get r :gate-failures) 0))
      (should-not (plist-member (car (plist-get r :trace)) :gate)))))

(ert-deftest anvil-fusion-longrun-test-verify-gate-panel-retry-preserves-budget ()
  (let ((anvil-fusion-longrun-max-panel-steps 1)
        step-calls
        panel-prompts)
    (let ((r (anvil-fusion-longrun-run
              "goal"
              :max-steps 1
              :verify-steps t
              :step-panel 'opus-solo
              :step-panel-mode 'always
              :step-fn (lambda (_prompt _step)
                         (push 'step step-calls)
                         "STEP")
              :panel-step-fn (lambda (prompt _step)
                               (push prompt panel-prompts)
                               (if (= (length panel-prompts) 1)
                                   "PANEL-FIRST"
                                 "PANEL-RETRY"))
              :distill-fn (lambda (prompt _step)
                            (if (string-match-p "PANEL-RETRY" prompt)
                                "DIGEST-PANEL-RETRY\nSTATUS: DONE"
                              "DIGEST-PANEL-FIRST\nSTATUS: DONE"))
              :verify-fn (lambda (_question digest)
                           (if (equal digest "DIGEST-PANEL-FIRST")
                               (list (list :claim "panel miss"
                                           :evidence "evidence"
                                           :verdict 'refuted))
                             nil)))))
      (setq panel-prompts (nreverse panel-prompts))
      (should-not step-calls)
      (should (= (length panel-prompts) 2))
      (should (= (plist-get r :panel-steps) 1))
      (should (eq (plist-get (car (plist-get r :trace)) :gate) 'retried-pass))
      (should (string-match-p "前回試行への反証" (nth 1 panel-prompts))))))

;;;; --- loop: bounded-context HANDOFF invariant (the core property) ---------

(ert-deftest anvil-fusion-longrun-test-run-handoff-carries-digest ()
  "Step N must receive step N-1's digest, not the full history."
  (let* ((seen-prompts nil)
         (r (anvil-fusion-longrun-run
             "g"
             :max-steps 3
             :step-fn (lambda (prompt _n) (push prompt seen-prompts) "out")
             :distill-fn (anvil-fusion-longrun-test--const-distill "DIGEST" nil))))
    (setq seen-prompts (nreverse seen-prompts))
    ;; step 1 sees no prior digest
    (should (string-match-p "(まだ無し)" (nth 0 seen-prompts)))
    ;; step 2 sees digest produced at step 1
    (should (string-match-p "DIGEST-1" (nth 1 seen-prompts)))
    ;; step 3 sees digest produced at step 2 (and NOT step 1's, i.e. only the
    ;; latest distilled state is carried -- O(1) context, not accumulated)
    (should (string-match-p "DIGEST-2" (nth 2 seen-prompts)))
    (should-not (string-match-p "DIGEST-1" (nth 2 seen-prompts)))
    (ignore r)))

(ert-deftest anvil-fusion-longrun-test-run-step-prompt-bounded ()
  "Per-step prompt size must not grow with step count (O(1) context)."
  (let* ((anvil-fusion-longrun-converge-patience 0) ; identical digests here; isolate budget
         (sizes nil)
         (_ (anvil-fusion-longrun-run
             "g"
             :max-steps 5
             :digest-max-chars 60
             :step-fn (lambda (prompt _n) (push (length prompt) sizes) "out")
             ;; distiller returns a long body that gets clamped to 60 chars
             :distill-fn (lambda (_p n)
                           (format "%s-%d\nSTATUS: CONTINUE"
                                   (make-string 500 ?z) n)))))
    (setq sizes (nreverse sizes))
    ;; later step prompts are not dramatically larger than the first:
    ;; difference is bounded by the clamped digest size, not by accumulation.
    (let ((first (nth 0 sizes))
          (last  (car (last sizes))))
      (should (< (- last first) 200)))))

;;;; --- loop: context isolation (raw outputs do not leak) -------------------

(ert-deftest anvil-fusion-longrun-test-run-isolates-raw-output ()
  "Raw step OUTPUT is consumed into the digest, never returned verbatim."
  (let ((r (anvil-fusion-longrun-run
            "g"
            :max-steps 2
            :step-fn (lambda (_p _n) "SECRET-RAW-OUTPUT-XYZ")
            :distill-fn (anvil-fusion-longrun-test--const-distill "DIGEST" nil))))
    ;; the distiller (stub) kept only "DIGEST-n"; the secret never appears.
    (should-not (string-match-p "SECRET-RAW-OUTPUT-XYZ"
                                (prin1-to-string r)))
    ;; trace entries carry metadata only (no :output key with the text)
    (dolist (e (plist-get r :trace))
      (should (plist-member e :output-chars))
      (should-not (plist-member e :output)))))

;;;; --- convergence (Phase 4) -----------------------------------------------

(ert-deftest anvil-fusion-longrun-test-streak-helper ()
  (should (= (anvil-fusion-longrun--streak 0 "abc def ghi" "abc def ghi" 0.9 2) 1))
  (should (= (anvil-fusion-longrun--streak 1 "abc def ghi" "abc def ghi" 0.9 2) 2))
  (should (= (anvil-fusion-longrun--streak 1 "abc def ghi" "totally other words" 0.9 2) 0))
  (should (= (anvil-fusion-longrun--streak 5 "x" "y" 0.9 0) 0))    ; patience 0 disables
  (should (= (anvil-fusion-longrun--streak 0 nil "x" 0.9 2) 0)))   ; nil prev -> reset

(ert-deftest anvil-fusion-longrun-test-converged-p-helper ()
  (should (anvil-fusion-longrun--converged-p 2 2))
  (should (anvil-fusion-longrun--converged-p 3 2))
  (should-not (anvil-fusion-longrun--converged-p 1 2))
  (should-not (anvil-fusion-longrun--converged-p 5 0)))

(ert-deftest anvil-fusion-longrun-test-run-converges-on-stagnation ()
  "A digest that stops changing ends the quest early (converged)."
  (let ((r (anvil-fusion-longrun-run
            "g"
            :max-steps 9
            :step-fn (lambda (_p _n) "out")
            :distill-fn (lambda (_p _n) "STABLE STATE\nSTATUS: CONTINUE"))))
    (should (eq (plist-get r :stopped) 'converged))
    ;; patience 2: stagnant streak reaches 2 at step 3
    (should (= (plist-get r :steps) 3))))

(ert-deftest anvil-fusion-longrun-test-converge-patience-zero-disables ()
  (let ((anvil-fusion-longrun-converge-patience 0))
    (let ((r (anvil-fusion-longrun-run
              "g"
              :max-steps 3
              :step-fn (lambda (_p _n) "out")
              :distill-fn (lambda (_p _n) "STABLE STATE\nSTATUS: CONTINUE"))))
      (should (eq (plist-get r :stopped) 'budget))
      (should (= (plist-get r :steps) 3)))))

;;;; --- hermetic step (Phase 4b) --------------------------------------------

(ert-deftest anvil-fusion-longrun-test-disclosure-tools-string ()
  (let ((s (anvil-fusion-longrun-disclosure-tools-string)))
    (should (string-match-p "mcp__emacs-eval__file-outline" s))
    (should (string-match-p "mcp__emacs-eval__file-read-snippet" s))
    (should (string-match-p "," s))))

(ert-deftest anvil-fusion-longrun-test-allowed-tools-plist ()
  (should (equal (anvil-fusion-longrun--allowed-tools-plist '("a" "b"))
                 '(:allowed-tools "a,b")))
  (should (equal (anvil-fusion-longrun--allowed-tools-plist "x,y")
                 '(:allowed-tools "x,y")))
  (should-not (anvil-fusion-longrun--allowed-tools-plist nil))
  (should-not (anvil-fusion-longrun--allowed-tools-plist "")))

(ert-deftest anvil-fusion-longrun-test-apply-suffix ()
  (should (equal (anvil-fusion-longrun--apply-suffix "p" "s") "p\n\ns"))
  (should (equal (anvil-fusion-longrun--apply-suffix "p" "") "p"))
  (should (equal (anvil-fusion-longrun--apply-suffix "p" nil) "p")))

(ert-deftest anvil-fusion-longrun-test-hermetic-threads-allowed-tools ()
  "With :hermetic, the default step-fn restricts the step to disclosure
tools and appends the hermetic instruction to the step prompt."
  (let (captured)
    (cl-letf (((symbol-function 'anvil-fusion-longrun--run-one)
               (lambda (_provider prompt _name &optional _model _cwd _ts _mw
                                  allowed-tools manifest-profile _member-extras)
                 (push (list :prompt prompt :allowed allowed-tools
                             :mp manifest-profile)
                       captured)
                 "out")))
      (anvil-fusion-longrun-run
       "g" :hermetic t :max-steps 1
       :distill-fn (lambda (_p _n) "D\nSTATUS: DONE"))
      (let ((c (car captured)))
        (should (equal (plist-get c :allowed)
                       (anvil-fusion-longrun--resolve-allowed-tools
                        nil t anvil-fusion-longrun-hermetic-manifest-profile)))
        (should (string-match-p "mcp__emacs-eval-ultra__file-outline"
                                (plist-get c :allowed)))
        (should (eq (plist-get c :mp)
                    anvil-fusion-longrun-hermetic-manifest-profile))
        (should (string-match-p "読み取り専用" (plist-get c :prompt)))
        (should (string-match-p "file-outline" (plist-get c :prompt)))))))

(ert-deftest anvil-fusion-longrun-test-single-provider-step-receives-merged-extras ()
  "Single-provider longrun steps pass merged whitelisted extras to `--run-one'."
  (let (captured)
    (cl-letf (((symbol-function 'anvil-fusion-longrun--run-one)
               (lambda (_provider _prompt _name &optional _model _cwd _ts _mw
                                  _allowed _manifest member-extras)
                 (setq captured member-extras)
                 "STEP-OUT")))
      (anvil-fusion-longrun-run
       "goal"
       :max-steps 1
       :cwd (expand-file-name "fusion-agentic" temporary-file-directory)
       :agentic t
       :member-extras '(:timeout-sec 91 :bogus "drop")
       :distill-fn (lambda (_prompt _step) "DIGEST\nSTATUS: DONE"))
      (should (equal "bypassPermissions" (plist-get captured :permission-mode)))
      (should (equal "workspace-write" (plist-get captured :sandbox)))
      (should (= 91 (plist-get captured :timeout-sec)))
      (should-not (plist-member captured :bogus)))))

(ert-deftest anvil-fusion-longrun-test-single-provider-step-nil-extras-regression-identical ()
  "Nil extras keep the default single-provider step path unchanged."
  (let (captured)
    (cl-letf (((symbol-function 'anvil-fusion-longrun--run-one)
               (lambda (provider prompt name &optional model cwd timeout-sec _mw
                                  _allowed _manifest member-extras)
                 (setq captured (list :provider provider :prompt prompt :name name
                                      :model model :cwd cwd :timeout-sec timeout-sec
                                      :member-extras member-extras))
                 "STEP-OUT")))
      (anvil-fusion-longrun-run
       "goal"
       :provider 'codex
       :model "gpt-5.5"
       :cwd "/tmp"
       :max-steps 1
       :timeout-sec 30
       :distill-fn (lambda (_prompt _step) "DIGEST\nSTATUS: DONE"))
      (should (equal '(:provider codex
                        :prompt nil
                        :name "longrun-step-1"
                        :model "gpt-5.5"
                        :cwd "/tmp"
                        :timeout-sec 30
                        :member-extras nil)
                     (plist-put captured :prompt nil))))))

(ert-deftest anvil-fusion-longrun-test-distill-task-receives-merged-extras ()
  "Default distill tasks reuse the merged member-extras machinery."
  (let (captured)
    (cl-letf (((symbol-function 'anvil-fusion-longrun--run-one)
               (lambda (_provider _prompt name &optional _model _cwd _ts _mw
                                  _allowed _manifest member-extras)
                 (when (string-match-p "^longrun-distill-" name)
                   (setq captured member-extras))
                 (if (string-match-p "^longrun-step-" name)
                     "STEP-OUT"
                   "DIGEST\nSTATUS: DONE"))))
      (anvil-fusion-longrun-run
       "goal"
       :max-steps 1
       :cwd (expand-file-name "fusion-agentic" temporary-file-directory)
       :agentic t
       :member-extras '(:timeout-sec 64 :bogus "drop")
       :provider 'claude
       :distill-provider 'claude)
      (should (= 64 (plist-get captured :timeout-sec)))
      (should (equal "bypassPermissions" (plist-get captured :permission-mode)))
      (should (equal "workspace-write" (plist-get captured :sandbox)))
      (should-not (plist-member captured :bogus)))))

(ert-deftest anvil-fusion-longrun-test-distill-task-nil-extras-regression-identical ()
  "Nil extras leave the default distill task unchanged."
  (let (captured)
    (cl-letf (((symbol-function 'anvil-fusion-longrun--run-one)
               (lambda (provider prompt name &optional model cwd timeout-sec _mw
                                  allowed-tools manifest-profile member-extras)
                 (when (string-match-p "^longrun-distill-" name)
                   (setq captured (list :provider provider :prompt prompt :name name
                                        :model model :cwd cwd :timeout-sec timeout-sec
                                        :allowed-tools allowed-tools
                                        :manifest-profile manifest-profile
                                        :member-extras member-extras)))
                 (if (string-match-p "^longrun-step-" name)
                     "STEP-OUT"
                   "DIGEST\nSTATUS: DONE"))))
      (anvil-fusion-longrun-run
       "goal"
       :provider 'codex
       :distill-provider 'codex
       :distill-model "gpt-5.5"
       :cwd "/tmp"
       :max-steps 1
       :timeout-sec 30)
      (should (equal '(:provider codex
                        :prompt nil
                        :name "longrun-distill-1"
                        :model "gpt-5.5"
                        :cwd "/tmp"
                        :timeout-sec 30
                        :allowed-tools nil
                        :manifest-profile nil
                        :member-extras nil)
                     (plist-put captured :prompt nil))))))

(ert-deftest anvil-fusion-longrun-test-panel-branch-forwards-member-extras-and-agentic ()
  "Panel steps forward MEMBER-EXTRAS and AGENTIC into `anvil-fusion-ask'."
  (let (ask-args)
    (cl-letf (((symbol-function 'anvil-fusion-ask)
               (lambda (prompt &rest args)
                 (setq ask-args (cons prompt args))
                 (list :answer "PANEL-OUT"))))
      (anvil-fusion-longrun-run
       "goal"
       :max-steps 1
       :cwd (expand-file-name "fusion-agentic" temporary-file-directory)
       :step-panel 'quality
       :step-panel-mode 'always
       :agentic t
       :member-extras '(:timeout-sec 77)
       :distill-fn (lambda (_prompt _step) "DIGEST\nSTATUS: DONE"))
      (should (equal 'quality (plist-get (cdr ask-args) :panel)))
      (should (eq t (plist-get (cdr ask-args) :agentic)))
      (let ((extras (plist-get (cdr ask-args) :member-extras)))
        (should (= 77 (plist-get extras :timeout-sec)))
        (should (equal "bypassPermissions" (plist-get extras :permission-mode)))
        (should (equal "workspace-write" (plist-get extras :sandbox)))))))

(provide 'anvil-fusion-longrun-test)
;;; anvil-fusion-longrun-test.el ends here
