;;; anvil-fusion-ask-test.el --- ERT for anvil-fusion Phase 3 -*- lexical-binding: t; -*-

;;; Commentary:
;; Single-round wiring tests for `anvil-fusion-ask' (pinned with
;; :max-rounds 0 so the Phase 4 critique loop never fires here — looping
;; is covered in anvil-fusion-loop-test.el).  anvil-orchestrator is faked:
;; we `provide' the feature and cl-letf the four public functions the ask
;; path uses, simulating a fan-out + judge without any live model.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'anvil-fusion)
(require 'anvil-fusion-panels)
(or (require 'anvil-orchestrator nil t) ;; load the real module when present (anvil.el)
    (provide 'anvil-orchestrator))  ;; else fake it (fusion standalone repo)
(require 'anvil-fusion-exec)
(require 'anvil-fusion-ask)

(defmacro anvil-fusion-ask-test--with-fake-orchestrator
    (member-candidates &rest body)
  "Run BODY with a faked orchestrator returning MEMBER-CANDIDATES.
Binds `submitted' to the list of submitted task-lists (newest
first)."
  (declare (indent 1) (debug t))
  `(let ((submitted '())
         (calls 0))
     (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                (lambda (tasks)
                  (push tasks submitted)
                  (setq calls (1+ calls))
                  (if (= calls 1) "b-mem" "b-judge")))
               ((symbol-function 'anvil-orchestrator-collect)
                (lambda (&rest _) t))
               ((symbol-function 'anvil-orchestrator-status)
                (lambda (id)
                  (cond
                   ((equal id "b-mem")  (list :tasks ,member-candidates))
                   ((equal id "b-judge")
                    (list :tasks '((:id "j1" :provider ollama :status done))))
                   (t (list :tasks nil)))))
               ((symbol-function 'anvil-orchestrator-extract-result)
                (lambda (id full)
                  (should (equal id "j1"))
                  (should full)
                  (list :summary "FUSED-ANSWER"))))
       ,@body)))

(defmacro anvil-fusion-ask-test--with-scripted-orchestrator
    (script &rest body)
  "Run BODY with a scripted fake orchestrator.
SCRIPT is a submission-order list of plists:
`(:status (:tasks ...) [:extract (:summary ...)])'.  Submitted task
lists are pushed onto `submitted' newest first."
  (declare (indent 1) (debug t))
  `(let ((submitted '())
         (calls 0)
         (queue (copy-tree ,script))
         (batch-map (make-hash-table :test #'equal))
         (result-map (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function 'anvil-orchestrator-submit)
                (lambda (tasks)
                  (let* ((entry (pop queue))
                         (batch-id (format "b-%d" calls)))
                    (should entry)
                    (push tasks submitted)
                    (setq calls (1+ calls))
                    (puthash batch-id entry batch-map)
                    (let* ((status (plist-get entry :status))
                           (first-task (car (plist-get status :tasks)))
                           (task-id (plist-get first-task :id))
                           (extract (plist-get entry :extract)))
                      (when (and task-id extract)
                        (puthash task-id extract result-map)))
                    batch-id)))
               ((symbol-function 'anvil-orchestrator-collect)
                (lambda (&rest _) t))
               ((symbol-function 'anvil-orchestrator-status)
                (lambda (id)
                  (or (plist-get (gethash id batch-map) :status)
                      (list :tasks nil))))
               ((symbol-function 'anvil-orchestrator-extract-result)
                (lambda (id full)
                  (should full)
                  (or (gethash id result-map)
                      (error "Unexpected extract-result id: %S" id)))))
       ,@body)))

(defconst anvil-fusion-ask-test--cands
  '((:id "m1" :provider ollama :status done :summary "候補A: DGR を使う。")
    (:id "m2" :provider ollama :status done :summary "候補B: 地絡方向継電器。")
    (:id "m3" :provider ollama :status done :summary "候補C: OCGR は不可。"))
  "Three simulated sovereign-panel candidate answers.")

(ert-deftest anvil-fusion-ask-test-agentic-extras-safe-cwd ()
  "Scratch temp CWD grants bypassPermissions plus codex sandbox."
  (let ((extras (anvil-fusion--agentic-extras t
                                              (expand-file-name
                                               "fusion-agentic"
                                               temporary-file-directory))))
    (should (equal "bypassPermissions" (plist-get extras :permission-mode)))
    (should-not (plist-member extras :allowed-tools))
    (should (equal "workspace-write" (plist-get extras :sandbox)))))

(ert-deftest anvil-fusion-ask-test-agentic-extras-unsafe-cwd-downgrades ()
  "Unsafe or absent CWD downgrades to a minimal tool grant."
  (dolist (cwd (list nil default-directory))
    (let ((extras (anvil-fusion--agentic-extras t cwd)))
      (should-not (plist-member extras :permission-mode))
      (should (equal "Bash" (plist-get extras :allowed-tools)))
      (should (equal "workspace-write" (plist-get extras :sandbox))))))

(ert-deftest anvil-fusion-ask-test-happy-path-sovereign ()
  "Sovereign panel returns the fused answer + metadata.
Panel is passed explicitly: the default is no longer `sovereign'
(changed to `claude-pair' 2026-06-16), but the sovereign mechanism
is retained and must keep working when selected."
  (anvil-fusion-ask-test--with-fake-orchestrator
      anvil-fusion-ask-test--cands
    (let ((res (anvil-fusion-ask "地絡保護の継電器は？"
                                :panel 'sovereign
                                :fidelity 'summary :max-rounds 0)))
      (should (equal "FUSED-ANSWER" (plist-get res :answer)))
      (should (eq 'sovereign (plist-get res :panel)))
      (should (eq 'local-only (plist-get res :egress)))
      (should (= 0 (plist-get res :rounds)))
      (should (= 3 (length (plist-get res :candidates))))
      (should (> (plist-get res :prompt-chars) 0)))))

(ert-deftest anvil-fusion-ask-test-member-tasks-from-panel ()
  "The fan-out submits exactly the panel's member models."
  (anvil-fusion-ask-test--with-fake-orchestrator
      anvil-fusion-ask-test--cands
    (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 0)
    (let* ((member-list (cadr submitted)))
      (should (= 3 (length member-list)))
      (should (cl-every (lambda (tk) (eq 'ollama (plist-get tk :provider)))
                        member-list))
      (should (equal '("llama3.1:8b" "llama3.2:3b" "gemma4:e4b")
                     (mapcar (lambda (tk) (plist-get tk :model)) member-list)))
      (should (cl-every (lambda (tk) (equal "Q" (plist-get tk :prompt)))
                        member-list)))))

(ert-deftest anvil-fusion-ask-test-judge-prompt-has-candidates ()
  "The judge prompt embeds every candidate answer + the Fusion axes."
  (anvil-fusion-ask-test--with-fake-orchestrator
      anvil-fusion-ask-test--cands
    (anvil-fusion-ask "Q" :panel 'sovereign :fidelity 'summary :max-rounds 0)
    (let* ((judge-task (car (car submitted)))
           (jprompt (plist-get judge-task :prompt)))
      (should (string-match-p "候補A: DGR" jprompt))
      (should (string-match-p "候補B: 地絡方向継電器" jprompt))
      (should (string-match-p "候補C: OCGR は不可" jprompt))
      (should (string-match-p "最終回答" jprompt))
      (should (string-match-p "Q" jprompt)))))

(ert-deftest anvil-fusion-ask-test-judge-provider-from-panel ()
  "The judge task uses the panel's judge provider/model."
  (anvil-fusion-ask-test--with-fake-orchestrator
      anvil-fusion-ask-test--cands
    (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 0)))
      (should (eq 'ollama (plist-get res :judge-provider)))
      (let ((judge-task (car (car submitted))))
        (should (eq 'ollama (plist-get judge-task :provider)))
        (should (equal "llama3.1:8b" (plist-get judge-task :model)))))))

(ert-deftest anvil-fusion-ask-test-extra-appended-to-judge ()
  "EXTRA reaches the judge prompt."
  (anvil-fusion-ask-test--with-fake-orchestrator
      anvil-fusion-ask-test--cands
    (anvil-fusion-ask "Q" :panel 'sovereign :extra "120字以内。" :max-rounds 0)
    (let ((jprompt (plist-get (car (car submitted)) :prompt)))
      (should (string-suffix-p "120字以内。" jprompt)))))

(ert-deftest anvil-fusion-ask-test-sovereignty-guard-blocks-external-judge ()
  "A non-local :judge override on a local-only panel is refused, and
nothing is submitted."
  (anvil-fusion-ask-test--with-fake-orchestrator
      anvil-fusion-ask-test--cands
    (should-error (anvil-fusion-ask "Q" :panel 'sovereign :judge 'claude)
                  :type 'user-error)
    (should (null submitted))))

(ert-deftest anvil-fusion-ask-test-quality-panel-distinct-providers ()
  "Quality panel fans out to its three distinct providers."
  (anvil-fusion-ask-test--with-fake-orchestrator
      '((:id "m1" :provider claude :status done :summary "A")
        (:id "m2" :provider codex  :status done :summary "B")
        (:id "m3" :provider gemini :status done :summary "C"))
    (let ((res (anvil-fusion-ask "Q" :panel 'quality :max-rounds 0)))
      (should (eq 'external (plist-get res :egress)))
      (should (eq 'claude (plist-get res :judge-provider)))
      (let ((member-list (cadr submitted)))
      (should (equal '(claude codex gemini)
                       (mapcar (lambda (tk) (plist-get tk :provider))
                               member-list)))))))

(ert-deftest anvil-fusion-ask-test-member-extras-reach-member-tasks ()
  "MEMBER-EXTRAS are merged into member tasks, not the judge task."
  (anvil-fusion-ask-test--with-fake-orchestrator
      anvil-fusion-ask-test--cands
    (anvil-fusion-ask "Q" :panel 'sovereign
                      :member-extras '(:timeout-sec 42 :sandbox "workspace-write")
                      :max-rounds 0)
    (let ((member-list (cadr submitted))
          (judge-task (car (car submitted))))
      (dolist (task member-list)
        (should (= 42 (plist-get task :timeout-sec)))
        (should (equal "workspace-write" (plist-get task :sandbox))))
      (should-not (plist-member judge-task :timeout-sec))
      (should-not (plist-member judge-task :sandbox)))))

(ert-deftest anvil-fusion-ask-test-agentic-explicit-member-extras-win ()
  "Explicit MEMBER-EXTRAS override the AGENTIC preset on matching keys."
  (anvil-fusion-ask-test--with-fake-orchestrator
      '((:id "m1" :provider claude :status done :summary "A")
        (:id "m2" :provider codex :status done :summary "B")
        (:id "m3" :provider gemini :status done :summary "C"))
    (anvil-fusion-ask "Q"
                      :panel 'quality
                      :agentic t
                      :cwd (expand-file-name "fusion-agentic"
                                             temporary-file-directory)
                      :member-extras '(:sandbox "danger-full-access"
                                       :permission-mode "acceptEdits")
                      :max-rounds 0)
    (let ((member-list (cadr submitted)))
      (dolist (task member-list)
        (should (equal "danger-full-access" (plist-get task :sandbox)))
        (should (equal "acceptEdits" (plist-get task :permission-mode)))))))

(ert-deftest anvil-fusion-ask-test-local-only-refuses-network-egress-tools ()
  "A local-only panel refuses :allowed-tools grants that add egress."
  (anvil-fusion-ask-test--with-fake-orchestrator
      anvil-fusion-ask-test--cands
    (should-error (anvil-fusion-ask "Q" :panel 'sovereign
                                    :member-extras '(:allowed-tools "WebSearch,Bash")
                                    :max-rounds 0)
                  :type 'user-error)
    (should (null submitted))))

(ert-deftest anvil-fusion-ask-test-agentic-member-prompt-appends-instruction ()
  "Agentic runs append the verification instruction to each member prompt."
  (anvil-fusion-ask-test--with-fake-orchestrator
      '((:id "m1" :provider claude :status done :summary "A")
        (:id "m2" :provider codex :status done :summary "B")
        (:id "m3" :provider gemini :status done :summary "C"))
    (anvil-fusion-ask "Q" :panel 'quality :agentic t :max-rounds 0)
    (let ((member-prompt (plist-get (car (cadr submitted)) :prompt)))
      (should (string-match-p "必要なら作業ディレクトリ内でコマンドやツールを実行し" member-prompt)))))

(ert-deftest anvil-fusion-ask-test-exemplars-prepend-member-prompt-only ()
  "EXEMPLARS prepends the retrieved block to member prompts, not the judge question."
  (let ((block "# 過去の検証済み事例（参考。事実確認済みの回答例）\n## 事例: 既存問\n既存答\n"))
    (require 'anvil-fusion-traj)
    (cl-letf (((symbol-function 'anvil-fusion-traj-exemplar-block)
               (lambda (query &rest kwargs)
                 (should (equal "Q" query))
                 (should (= 2 (plist-get kwargs :k)))
                 block)))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--cands
        (anvil-fusion-ask "Q" :panel 'sovereign :exemplars 2 :max-rounds 0)
        (let* ((member-list (cadr submitted))
               (judge-task (car (car submitted)))
               (member-prompt (plist-get (car member-list) :prompt))
               (judge-prompt (plist-get judge-task :prompt)))
          (should (string-prefix-p (concat block "\nQ") member-prompt))
          (should (string-match-p "^# 原問\nQ" judge-prompt))
          (should-not (string-prefix-p block judge-prompt)))))))

(ert-deftest anvil-fusion-ask-test-store-trajectory-best-effort-and-default-off ()
  "STORE-TRAJECTORY stores against the original prompt; default remains off."
  (let (stored-prompt stored-result stored-tags store-called default-called)
    (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (&rest _) anvil-fusion-ask-test--claims-raw))
              ((symbol-function 'anvil-fusion-verify-claims)
               (lambda (&rest _)
                 (list (list :claim "DGR を使う"
                             :kind 'fact
                             :candidates '("A")
                             :verdict 'confirmed
                             :evidence "kb:1"))))
              ((symbol-function 'anvil-fusion-traj-store)
               (lambda (prompt result &rest kwargs)
                 (setq store-called t
                       stored-prompt prompt
                       stored-result result
                       stored-tags (plist-get kwargs :tags))
                 41))
              ((symbol-function 'anvil-fusion-traj-exemplar-block)
               (lambda (&rest _)
                 (setq default-called t)
                 nil)))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--cands
        (let ((res (anvil-fusion-ask "Q" :panel 'sovereign
                                     :verify t
                                     :store-trajectory t
                                     :max-rounds 0)))
          (should store-called)
          (should (equal "Q" stored-prompt))
          (should (equal "FUSED-ANSWER" (plist-get stored-result :answer)))
          (should (equal nil stored-tags))
          (should (= 41 (plist-get res :trajectory-id)))))
      (setq store-called nil
            default-called nil)
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--cands
        (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 0)))
          (should-not store-called)
          (should-not default-called)
          (should-not (plist-member res :trajectory-id)))))))

;;;; ============================================================
;;;; Phase 6d — :verify keyword
;;;; ============================================================

(defconst anvil-fusion-ask-test--claims-raw
  (list (list :claim "DGR を使う" :kind 'fact :candidates '("A" "B"))
        (list :claim "OCGR でも良い" :kind 'fact :candidates '("C")))
  "Two raw (Phase 6a) claims used as the extraction stub's return value.")

(defconst anvil-fusion-ask-test--claims-annotated
  (list (list :claim "DGR を使う" :kind 'fact :candidates '("A" "B")
              :verdict 'confirmed :evidence "a.org:1")
        (list :claim "OCGR でも良い" :kind 'fact :candidates '("C")
              :verdict 'refuted :evidence "反証: OCGR は不可"))
  "Annotated (Phase 6b) claims, one confirmed one refuted.")

(defconst anvil-fusion-ask-test--debate-round0-cands
  '((:id "m1" :name "member-1" :provider ollama :status done :summary "候補A: DGR を使う。")
    (:id "m2" :name "member-2" :provider ollama :status done :summary "候補B: 地絡方向継電器。")
    (:id "m3" :name "member-3" :provider ollama :status done :summary "候補C: OCGR は不可。"))
  "Divergent candidates used to trigger round-1 debate.")

(defconst anvil-fusion-ask-test--debate-round1-cands
  '((:id "m4" :name "member-1" :provider ollama :status done :summary "収束案: DGR を使う。")
    (:id "m5" :name "member-2" :provider ollama :status done :summary "収束案: DGR を使う。")
    (:id "m6" :name "member-3" :provider ollama :status done :summary "収束案: DGR を使う。"))
  "Converged candidates used to stop the loop after one extra round.")

(ert-deftest anvil-fusion-ask-test-deadline-helper-boundary ()
  "The deadline helper is nil-safe and flips at the inclusive boundary."
  (let ((now 10.0))
    (cl-letf (((symbol-function 'float-time) (lambda () now)))
      (should-not (anvil-fusion--deadline-exceeded-p 0 nil))
      (should-not (anvil-fusion--deadline-exceeded-p 0 11))
      (should (anvil-fusion--deadline-exceeded-p 0 10))
      (should (anvil-fusion--deadline-exceeded-p 0 9.5)))))

(ert-deftest anvil-fusion-ask-test-deadline-nil-keyword-overrides-defcustom ()
  "An explicit :deadline-sec nil restores the unbounded path."
  (let ((anvil-fusion-ask-deadline-sec 0)
        (extract-called 0))
    (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (&rest _)
                 (cl-incf extract-called)
                 nil)))
      (anvil-fusion-ask-test--with-scripted-orchestrator
          `((:status (:tasks ,anvil-fusion-ask-test--debate-round0-cands))
            (:status (:tasks ((:id "j1" :provider ollama :status done)))
             :extract (:summary "ROUND0-FUSED"))
            (:status (:tasks ,anvil-fusion-ask-test--debate-round1-cands))
            (:status (:tasks ((:id "j2" :provider ollama :status done)))
             :extract (:summary "ROUND1-FUSED")))
        (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :verify t
                                     :deadline-sec nil :max-rounds 1)))
          (should (= 1 extract-called))
          (should (= 1 (plist-get res :rounds)))
          (should-not (plist-member res :deadline-exceeded)))))))

(ert-deftest anvil-fusion-ask-test-verify-nil-skips-verification ()
  "With :verify nil (the default), extract-claims/verify-claims are
never invoked, and the judge prompt is the normal (un-verified)
template -- existing behavior is untouched."
  (let (extract-called verify-called)
    (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (&rest _) (setq extract-called t) nil))
              ((symbol-function 'anvil-fusion-verify-claims)
               (lambda (&rest _) (setq verify-called t) nil)))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--cands
        (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 0)))
          (should-not extract-called)
          (should-not verify-called)
          (should (null (plist-get res :claims)))
          (let ((jprompt (plist-get (car (car submitted)) :prompt)))
            (should (string-match-p "最終回答" jprompt))
            (should-not (string-match-p "検証済み主張表" jprompt))))))))

(ert-deftest anvil-fusion-ask-test-verify-happy-path ()
  "With :verify t, extraction + verification run, the verified judge
template is used (claims block + the refuted claim present in the
judge prompt), and the return plist carries :claims."
  (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
             (lambda (question _candidates &rest _)
               (should (equal "Q" question))
               anvil-fusion-ask-test--claims-raw))
            ((symbol-function 'anvil-fusion-verify-claims)
             (lambda (claims &rest kwargs)
               (should (equal anvil-fusion-ask-test--claims-raw claims))
               (should (equal "Q" (plist-get kwargs :question)))
               anvil-fusion-ask-test--claims-annotated)))
    (anvil-fusion-ask-test--with-fake-orchestrator
        anvil-fusion-ask-test--cands
      (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :verify t :max-rounds 0)))
        (let ((jprompt (plist-get (car (car submitted)) :prompt)))
          (should (string-match-p "検証済み主張表" jprompt))
          (should (string-match-p "OCGR でも良い" jprompt))
          (should (string-match-p "refuted" jprompt)))
        (should (equal anvil-fusion-ask-test--claims-annotated (plist-get res :claims)))))))

(ert-deftest anvil-fusion-ask-test-verify-extraction-nil-falls-back ()
  "When extraction returns nil, verify-claims is never called, the
normal template is used, the judge still runs, and :claims is nil."
  (let (verify-called)
    (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (&rest _) nil))
              ((symbol-function 'anvil-fusion-verify-claims)
               (lambda (&rest _) (setq verify-called t) nil)))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--cands
        (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :verify t :max-rounds 0)))
          (should-not verify-called)
          (should (equal "FUSED-ANSWER" (plist-get res :answer)))
          (should (null (plist-get res :claims)))
          (let ((jprompt (plist-get (car (car submitted)) :prompt)))
            (should-not (string-match-p "検証済み主張表" jprompt))))))))

(ert-deftest anvil-fusion-ask-test-verify-extraction-nil-uses-base-template-fallback ()
  "Nil extraction uses VERIFY-BASE-TEMPLATE when no explicit TEMPLATE was provided."
  (let ((base "DISTINCT-FALLBACK\n# 原問\n%s\n# 候補回答\n%s\n# 検証済み主張表\n{{CLAIMS}}\n"))
    (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (&rest _) nil)))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--cands
        (anvil-fusion-ask "Q" :panel 'sovereign :verify t
                          :verify-base-template base :max-rounds 0)
        (let ((jprompt (plist-get (car (car submitted)) :prompt)))
          (should (string-match-p "DISTINCT-FALLBACK" jprompt))
          (should (string-match-p "(検証済み主張なし)" jprompt)))))))

(ert-deftest anvil-fusion-ask-test-verify-extraction-nil-explicit-template-wins ()
  "An explicit TEMPLATE overrides the nil-extraction fallback template."
  (let ((base "DISTINCT-FALLBACK\n# 原問\n%s\n# 候補回答\n%s\n# 検証済み主張表\n{{CLAIMS}}\n")
        (explicit "EXPLICIT-TEMPLATE\nQ=%s\nC=%s\n"))
    (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (&rest _) nil)))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--cands
        (anvil-fusion-ask "Q" :panel 'sovereign :verify t
                          :template explicit
                          :verify-base-template base
                          :max-rounds 0)
        (let ((jprompt (plist-get (car (car submitted)) :prompt)))
          (should (string-match-p "EXPLICIT-TEMPLATE" jprompt))
          (should-not (string-match-p "DISTINCT-FALLBACK" jprompt)))))))

(ert-deftest anvil-fusion-ask-test-verify-local-only-threads-panel-judge ()
  "A local-only panel's :verify t threads the panel judge's
provider/model into both extraction and verification, and passes
:egress \\='local-only to verify-claims."
  (let (extract-kw verify-kw)
    (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (_question _candidates &rest kwargs)
                 (setq extract-kw kwargs)
                 anvil-fusion-ask-test--claims-raw))
              ((symbol-function 'anvil-fusion-verify-claims)
               (lambda (_claims &rest kwargs)
                 (setq verify-kw kwargs)
                 anvil-fusion-ask-test--claims-annotated)))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--cands
        (anvil-fusion-ask "Q" :panel 'sovereign :verify t :max-rounds 0)
        (should (eq 'ollama (plist-get extract-kw :provider)))
        (should (equal "llama3.1:8b" (plist-get extract-kw :model)))
        (should (eq 'ollama (plist-get verify-kw :provider)))
        (should (equal "llama3.1:8b" (plist-get verify-kw :model)))
        (should (eq 'local-only (plist-get verify-kw :egress)))))))

(ert-deftest anvil-fusion-ask-test-verify-base-template-forwarded ()
  "A custom VERIFY-BASE-TEMPLATE is used when building the verified judge prompt."
  (let ((base "DISTINCT-PLAN-BASE\n# 原問\n%s\n# 候補回答\n%s\n# 検証済み主張表\n{{CLAIMS}}\n"))
    (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (&rest _) anvil-fusion-ask-test--claims-raw))
              ((symbol-function 'anvil-fusion-verify-claims)
               (lambda (&rest _) anvil-fusion-ask-test--claims-annotated)))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--cands
        (anvil-fusion-ask "Q" :panel 'sovereign :verify t
                          :verify-base-template base :max-rounds 0)
        (let ((jprompt (plist-get (car (car submitted)) :prompt)))
          (should (string-match-p "DISTINCT-PLAN-BASE" jprompt)))))))

(ert-deftest anvil-fusion-ask-test-deadline-skips-verify-but-still-judges ()
  "A tripped deadline after round-0 fan-out skips verification only."
  (let ((now 0.0)
        extract-called
        verify-called
        submitted)
    (cl-letf (((symbol-function 'float-time)
               (lambda () now))
              ((symbol-function 'anvil-orchestrator-submit)
               (lambda (tasks)
                 (push tasks submitted)
                 (if (= (length submitted) 1) "b-mem" "b-judge")))
              ((symbol-function 'anvil-orchestrator-collect)
               (lambda (&rest _) t))
              ((symbol-function 'anvil-orchestrator-status)
               (lambda (id)
                 (cond
                  ((equal id "b-mem")
                   (setq now 25.0)
                   (list :tasks anvil-fusion-ask-test--cands))
                  ((equal id "b-judge")
                   (list :tasks '((:id "j1" :provider ollama :status done))))
                  (t (list :tasks nil)))))
              ((symbol-function 'anvil-orchestrator-extract-result)
               (lambda (_id full)
                 (should full)
                 (list :summary "FUSED-ANSWER")))
              ((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (&rest _)
                 (setq extract-called t)
                 anvil-fusion-ask-test--claims-raw))
              ((symbol-function 'anvil-fusion-verify-claims)
               (lambda (&rest _)
                 (setq verify-called t)
                 anvil-fusion-ask-test--claims-annotated)))
      (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :verify t
                                   :deadline-sec 20 :max-rounds 0)))
        (should-not extract-called)
        (should-not verify-called)
        (should (equal "FUSED-ANSWER" (plist-get res :answer)))
        (should (plist-get res :deadline-exceeded))
        (should (= 2 (length submitted)))))))

(ert-deftest anvil-fusion-ask-test-deadline-stops-before-critique-loop ()
  "A tripped deadline after the round-0 judge prevents later rounds."
  (let ((submitted '())
        (calls 0))
    (cl-letf (((symbol-function 'anvil-fusion--deadline-exceeded-p)
               (lambda (_start _deadline) t))
              ((symbol-function 'anvil-orchestrator-submit)
               (lambda (tasks)
                 (push tasks submitted)
                 (prog1 (format "b-%d" calls)
                   (setq calls (1+ calls)))))
              ((symbol-function 'anvil-orchestrator-collect)
               (lambda (&rest _) t))
              ((symbol-function 'anvil-orchestrator-status)
               (lambda (id)
                 (cond
                  ((equal id "b-0")
                   (list :tasks anvil-fusion-ask-test--debate-round0-cands))
                  ((equal id "b-1")
                   (list :tasks '((:id "j1" :provider ollama :status done))))
                  (t (list :tasks nil)))))
              ((symbol-function 'anvil-orchestrator-extract-result)
               (lambda (id full)
                 (should full)
                 (should (equal "j1" id))
                 (list :summary "ROUND0-FUSED"))))
      (let ((res (anvil-fusion-ask "Q" :panel 'sovereign
                                   :deadline-sec 20 :max-rounds 1)))
        (should (equal "ROUND0-FUSED" (plist-get res :answer)))
        (should (= 0 (plist-get res :rounds)))
        (should (plist-get res :deadline-exceeded))
        (should (= 2 (length submitted)))))))

;;;; ============================================================
;;;; Phase 10 — multi-round debate
;;;; ============================================================

(ert-deftest anvil-fusion-ask-test-critique-loop-legacy-prompt-locked-when-debate-disabled ()
  "With debate disabled, round-1 member prompts remain the exact critique prompt."
  (let ((anvil-fusion-debate nil))
    (anvil-fusion-ask-test--with-scripted-orchestrator
        `((:status (:tasks ,anvil-fusion-ask-test--debate-round0-cands))
          (:status (:tasks ((:id "j1" :provider ollama :status done)))
           :extract (:summary "ROUND0-FUSED"))
          (:status (:tasks ,anvil-fusion-ask-test--debate-round1-cands))
          (:status (:tasks ((:id "j2" :provider ollama :status done)))
           :extract (:summary "ROUND1-FUSED")))
      (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 1)))
        (should (= 1 (plist-get res :rounds)))
        (should (= 0 (plist-get res :debate-rounds)))
        (let* ((submissions (nreverse submitted))
               (round1-member-tasks (nth 2 submissions))
               (expected (anvil-fusion-build-critique-prompt "Q" "ROUND0-FUSED")))
          (should (= 3 (length round1-member-tasks)))
          (dolist (task round1-member-tasks)
            (should (equal expected (plist-get task :prompt)))))))))

(ert-deftest anvil-fusion-ask-test-debate-round-prompts-show-other-candidates ()
  "With debate enabled, round-1 prompts include own answer, other answers, and stop after convergence."
  (let ((anvil-fusion-debate t))
    (anvil-fusion-ask-test--with-scripted-orchestrator
        `((:status (:tasks ,anvil-fusion-ask-test--debate-round0-cands))
          (:status (:tasks ((:id "j1" :provider ollama :status done)))
           :extract (:summary "ROUND0-FUSED"))
          (:status (:tasks ,anvil-fusion-ask-test--debate-round1-cands))
          (:status (:tasks ((:id "j2" :provider ollama :status done)))
           :extract (:summary "ROUND1-FUSED")))
      (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 2)))
        (should (= 1 (plist-get res :rounds)))
        (should (= 1 (plist-get res :debate-rounds)))
        (should (plist-get res :looped))
        (let* ((submissions (nreverse submitted))
               (round1-member-tasks (nth 2 submissions))
               (prompt1 (plist-get (nth 0 round1-member-tasks) :prompt))
               (prompt2 (plist-get (nth 1 round1-member-tasks) :prompt)))
          (should-not (equal (anvil-fusion-build-critique-prompt "Q" "ROUND0-FUSED")
                             prompt1))
          (should (string-match-p "候補A: DGR を使う。" prompt1))
          (should (string-match-p "候補B: 地絡方向継電器。" prompt1))
          (should (string-match-p "候補C: OCGR は不可。" prompt2))
          (should (string-match-p "# 検証済み主張表\n(なし)" prompt1)))))))

(ert-deftest anvil-fusion-ask-test-debate-verify-incremental-new-claims-only ()
  "Debate verification re-checks only newly surfaced claims and merges them into the judge table."
  (let ((anvil-fusion-debate t)
        (extract-calls 0)
        verify-inputs)
    (cl-letf (((symbol-function 'anvil-fusion-verify-extract-claims)
               (lambda (&rest _)
                 (setq extract-calls (1+ extract-calls))
                 (if (= extract-calls 1)
                     (list (list :claim "既存主張" :kind 'fact :candidates '("A")))
                   (list (list :claim "既存主張" :kind 'fact :candidates '("A"))
                         (list :claim "新規主張" :kind 'fact :candidates '("B"))))))
              ((symbol-function 'anvil-fusion-verify-claims)
               (lambda (claims &rest _)
                 (push (mapcar (lambda (claim) (plist-get claim :claim)) claims)
                       verify-inputs)
                 (mapcar
                  (lambda (claim)
                    (list :claim (plist-get claim :claim)
                          :kind (plist-get claim :kind)
                          :candidates (plist-get claim :candidates)
                          :verdict 'confirmed
                          :evidence (format "ev:%s" (plist-get claim :claim))))
                  claims))))
      (anvil-fusion-ask-test--with-scripted-orchestrator
          `((:status (:tasks ,anvil-fusion-ask-test--debate-round0-cands))
            (:status (:tasks ((:id "j1" :provider ollama :status done)))
             :extract (:summary "ROUND0-FUSED"))
            (:status (:tasks ,anvil-fusion-ask-test--debate-round1-cands))
            (:status (:tasks ((:id "j2" :provider ollama :status done)))
             :extract (:summary "ROUND1-FUSED")))
        (let ((res (anvil-fusion-ask "Q" :panel 'sovereign
                                     :verify t
                                     :max-rounds 1)))
          (should (equal '(("既存主張") ("新規主張"))
                         (nreverse verify-inputs)))
          (should (= 1 (plist-get res :debate-rounds)))
          (should (= 2 (length (plist-get res :claims))))
          (let ((jprompt (plist-get (car (car submitted)) :prompt)))
            (should (string-match-p "既存主張" jprompt))
            (should (string-match-p "新規主張" jprompt))))))))

(ert-deftest anvil-fusion-ask-test-debate-max-rounds-zero-skips-debate ()
  "Max-rounds 0 suppresses debate even when the feature flag is on."
  (let ((anvil-fusion-debate t))
    (anvil-fusion-ask-test--with-fake-orchestrator
        anvil-fusion-ask-test--debate-round0-cands
      (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 0)))
        (should (= 0 (plist-get res :rounds)))
        (should (= 0 (plist-get res :debate-rounds)))
        (should (= 2 (length submitted)))))))

;;;; ============================================================
;;;; Phase 6c — :exec-check keyword
;;;; ============================================================

(defconst anvil-fusion-ask-test--exec-cands
  '((:id "m1" :name "pass" :provider ollama :status done :summary "PASSING-CAND")
    (:id "m2" :name "fail" :provider ollama :status done :summary "FAILING-CAND")
    (:id "m3" :name "nopatch" :provider ollama :status done :summary "NOPATCH-CAND"))
  "Three candidates used for :exec-check tests.")

(defconst anvil-fusion-ask-test--exec-claims
  '((:claim "候補 pass のパッチは検証コマンドに合格した"
     :kind code :candidates ("pass") :verdict confirmed :evidence "exit 0")
    (:claim "候補 fail のパッチは検証コマンドに不合格だった"
     :kind code :candidates ("fail") :verdict refuted :evidence "check failed")
    (:claim "候補 nopatch の回答には検証可能なパッチが見つからなかった"
     :kind code :candidates ("nopatch") :verdict unverified :evidence "no patch block"))
  "Execution-verification claims used by stubs.")

(defconst anvil-fusion-ask-test--exec-results
  '((:name "pass" :status pass :detail "exit 0")
    (:name "fail" :status fail :detail "check failed")
    (:name "nopatch" :status no-patch :detail "no patch block"))
  "Execution-verification results used by stubs.")

(ert-deftest anvil-fusion-ask-test-exec-check-nil-skips-exec ()
  "With :exec-check nil, execution verification is not invoked."
  (let (exec-called)
    (cl-letf (((symbol-function 'anvil-fusion-exec-verify-candidates)
               (lambda (&rest _) (setq exec-called t) nil)))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--exec-cands
        (let ((res (anvil-fusion-ask "Q" :panel 'sovereign :max-rounds 0)))
          (should-not exec-called)
          (should (null (plist-get res :exec-results))))))))

(ert-deftest anvil-fusion-ask-test-exec-check-filters-failed-candidates ()
  "Failed candidates are excluded from the first judge call and exec claims merge into the template."
  (cl-letf (((symbol-function 'anvil-fusion-exec-verify-candidates)
             (lambda (_candidates &rest _)
               (list :results anvil-fusion-ask-test--exec-results
                     :claims anvil-fusion-ask-test--exec-claims)))
            ((symbol-function 'anvil-fusion-verify-extract-claims)
             (lambda (&rest _) anvil-fusion-ask-test--claims-raw))
            ((symbol-function 'anvil-fusion-verify-claims)
             (lambda (&rest _) anvil-fusion-ask-test--claims-annotated)))
    (anvil-fusion-ask-test--with-fake-orchestrator
        anvil-fusion-ask-test--exec-cands
      (let ((res (anvil-fusion-ask "Q" :panel 'sovereign
                                   :verify t
                                   :exec-check '(:repo-root "/tmp/repo"
                                                 :check-cmd "make test")
                                   :max-rounds 0)))
        (let ((jprompt (plist-get (car (car submitted)) :prompt)))
          (should (string-match-p "PASSING-CAND" jprompt))
          (should (string-match-p "NOPATCH-CAND" jprompt))
          (should-not (string-match-p "FAILING-CAND" jprompt))
          (should (string-match-p "候補 fail のパッチは検証コマンドに不合格だった" jprompt))
          (should (string-match-p "DGR を使う" jprompt)))
        (should (equal anvil-fusion-ask-test--exec-results
                       (plist-get res :exec-results)))
        (should (equal (append anvil-fusion-ask-test--claims-annotated
                               anvil-fusion-ask-test--exec-claims)
                       (plist-get res :claims)))))))

(ert-deftest anvil-fusion-ask-test-exec-check-all-fail-keeps-all-candidates ()
  "If every candidate fails/errors, exclusion is suppressed."
  (let ((all-fail-results
         '((:name "pass" :status fail :detail "bad")
           (:name "fail" :status error :detail "boom")
           (:name "nopatch" :status fail :detail "bad2")))
        (all-fail-claims
         '((:claim "候補 pass のパッチは検証コマンドに不合格だった"
            :kind code :candidates ("pass") :verdict refuted :evidence "bad")
           (:claim "候補 fail のパッチは実行エラーになった"
            :kind code :candidates ("fail") :verdict refuted :evidence "boom")
           (:claim "候補 nopatch のパッチは検証コマンドに不合格だった"
            :kind code :candidates ("nopatch") :verdict refuted :evidence "bad2"))))
    (cl-letf (((symbol-function 'anvil-fusion-exec-verify-candidates)
               (lambda (&rest _)
                 (list :results all-fail-results :claims all-fail-claims))))
      (anvil-fusion-ask-test--with-fake-orchestrator
          anvil-fusion-ask-test--exec-cands
        (let ((res (anvil-fusion-ask "Q" :panel 'sovereign
                                     :exec-check '(:repo-root "/tmp/repo"
                                                   :check-cmd "make test")
                                     :max-rounds 0)))
          (let ((jprompt (plist-get (car (car submitted)) :prompt)))
            (should (string-match-p "PASSING-CAND" jprompt))
            (should (string-match-p "FAILING-CAND" jprompt))
            (should (string-match-p "NOPATCH-CAND" jprompt))
            (should (string-match-p "候補 fail のパッチは実行エラーになった" jprompt)))
          (should (equal all-fail-results (plist-get res :exec-results)))
          (should (equal all-fail-claims (plist-get res :claims))))))))

(ert-deftest anvil-fusion-ask-test-exec-check-without-verify-still-uses-verified-template ()
  "Exec claims alone still drive the verified judge template when :verify is nil."
  (cl-letf (((symbol-function 'anvil-fusion-exec-verify-candidates)
             (lambda (&rest _)
               (list :results anvil-fusion-ask-test--exec-results
                     :claims anvil-fusion-ask-test--exec-claims))))
    (anvil-fusion-ask-test--with-fake-orchestrator
        anvil-fusion-ask-test--exec-cands
      (let ((res (anvil-fusion-ask "Q" :panel 'sovereign
                                   :exec-check '(:repo-root "/tmp/repo"
                                                 :check-cmd "make test")
                                   :max-rounds 0)))
        (let ((jprompt (plist-get (car (car submitted)) :prompt)))
          (should (string-match-p "検証済み主張表" jprompt))
          (should (string-match-p "候補 pass のパッチは検証コマンドに合格した" jprompt)))
        (should (equal anvil-fusion-ask-test--exec-claims
                       (plist-get res :claims)))))))

(provide 'anvil-fusion-ask-test)
;;; anvil-fusion-ask-test.el ends here
