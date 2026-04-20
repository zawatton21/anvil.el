;;; anvil-orchestrator-presets-test.el --- Tests for anvil-orchestrator-presets -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for Doc 23 Phase 1 — preset CRUD, validation, and the
;; `:preset' resolution path feeding `anvil-orchestrator-submit-consensus'.
;; The submit integration itself is exercised with stub providers borrowed
;; from `anvil-orchestrator-test' fixtures so we never touch a real CLI.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-state)
(require 'anvil-orchestrator)
(require 'anvil-orchestrator-presets)
(require 'anvil-test-fixtures)


;;;; --- fixtures ------------------------------------------------------------

(defmacro anvil-orchestrator-presets-test--with-fresh (&rest body)
  "Run BODY with a clean preset table and ephemeral defaults.
Saves / restores `anvil-orchestrator-consensus-preset-defaults' and
the module-local cache so tests are deterministic."
  (declare (indent 0) (debug t))
  `(let ((saved-defaults anvil-orchestrator-consensus-preset-defaults)
         (saved-table    anvil-orchestrator--consensus-presets))
     (unwind-protect
         (progn
           (setq anvil-orchestrator--consensus-presets nil)
           ,@body)
       (setq anvil-orchestrator-consensus-preset-defaults saved-defaults)
       (setq anvil-orchestrator--consensus-presets saved-table))))


;;;; --- validation ---------------------------------------------------------

(ert-deftest anvil-orchestrator-presets-test-validate-rejects-single-provider ()
  "A preset with fewer than 2 providers is rejected at set time."
  (anvil-orchestrator-presets-test--with-fresh
    (should-error
     (anvil-orchestrator-consensus-preset-set
      'just-ollama '(:providers (ollama))))))

(ert-deftest anvil-orchestrator-presets-test-validate-rejects-non-symbol-providers ()
  "Providers must all be symbols."
  (anvil-orchestrator-presets-test--with-fresh
    (should-error
     (anvil-orchestrator-consensus-preset-set
      'bad '(:providers ("claude" codex))))))

(ert-deftest anvil-orchestrator-presets-test-validate-rejects-bad-threshold ()
  "Threshold must be a number in [0,1]."
  (anvil-orchestrator-presets-test--with-fresh
    (should-error
     (anvil-orchestrator-consensus-preset-set
      'bad '(:providers (claude codex) :threshold 1.5)))
    (should-error
     (anvil-orchestrator-consensus-preset-set
      'bad '(:providers (claude codex) :threshold "0.5")))))

(ert-deftest anvil-orchestrator-presets-test-validate-rejects-non-symbol-judge ()
  "Judge must be nil or a symbol."
  (anvil-orchestrator-presets-test--with-fresh
    (should-error
     (anvil-orchestrator-consensus-preset-set
      'bad '(:providers (claude codex) :judge "claude")))))


;;;; --- CRUD ---------------------------------------------------------------

(ert-deftest anvil-orchestrator-presets-test-set-and-get-roundtrip ()
  "A valid preset round-trips via set / get."
  (anvil-orchestrator-presets-test--with-fresh
    (anvil-orchestrator-consensus-preset-set
     'mine '(:providers (claude codex) :threshold 0.5))
    (let ((got (anvil-orchestrator-consensus-preset-get 'mine)))
      (should (equal '(claude codex) (plist-get got :providers)))
      (should (= 0.5 (plist-get got :threshold))))))

(ert-deftest anvil-orchestrator-presets-test-get-unknown-is-nil ()
  "`-get' on an unregistered name returns nil."
  (anvil-orchestrator-presets-test--with-fresh
    (should (null (anvil-orchestrator-consensus-preset-get 'no-such-preset)))))

(ert-deftest anvil-orchestrator-presets-test-list-is-sorted-by-name ()
  "`-list' returns alist sorted by symbol name."
  (anvil-orchestrator-presets-test--with-fresh
    (setq anvil-orchestrator--consensus-presets (make-hash-table :test 'eq))
    (anvil-orchestrator-consensus-preset-set
     'zebra '(:providers (claude codex)))
    (anvil-orchestrator-consensus-preset-set
     'alpha '(:providers (claude codex)))
    (anvil-orchestrator-consensus-preset-set
     'mike  '(:providers (claude codex)))
    (let ((names (mapcar #'car (anvil-orchestrator-consensus-preset-list))))
      (should (equal '(alpha mike zebra) names)))))

(ert-deftest anvil-orchestrator-presets-test-delete-removes-entry ()
  "`-delete' returns t first time, nil once removed."
  (anvil-orchestrator-presets-test--with-fresh
    (anvil-orchestrator-consensus-preset-set
     'tmp '(:providers (claude codex)))
    (should (anvil-orchestrator-consensus-preset-delete 'tmp))
    (should-not (anvil-orchestrator-consensus-preset-delete 'tmp))
    (should (null (anvil-orchestrator-consensus-preset-get 'tmp)))))

(ert-deftest anvil-orchestrator-presets-test-defaults-are-seeded ()
  "The built-in `fast' / `cheap' / `broad' / `rigorous' presets are present."
  (anvil-orchestrator-presets-test--with-fresh
    (let ((names (mapcar #'car (anvil-orchestrator-consensus-preset-list))))
      (should (memq 'fast names))
      (should (memq 'cheap names))
      (should (memq 'broad names))
      (should (memq 'rigorous names)))
    (let ((fast (anvil-orchestrator-consensus-preset-get 'fast)))
      (should (equal '(claude codex) (plist-get fast :providers)))
      (should (null (plist-get fast :judge))))
    (let ((broad (anvil-orchestrator-consensus-preset-get 'broad)))
      (should (memq 'gemini (plist-get broad :providers)))
      (should (eq 'claude (plist-get broad :judge))))))


;;;; --- resolution ---------------------------------------------------------

(ert-deftest anvil-orchestrator-presets-test-resolve-without-preset-is-noop ()
  "When PRESET is nil, the resolver just passes caller values through."
  (anvil-orchestrator-presets-test--with-fresh
    (let ((r (anvil-orchestrator--preset-resolve
              nil '(claude codex) nil 30 0.5)))
      (should (equal '(claude codex) (plist-get r :providers)))
      (should (= 30 (plist-get r :timeout-sec)))
      (should (= 0.5 (plist-get r :budget-usd)))
      (should (null (plist-get r :preset-name))))))

(ert-deftest anvil-orchestrator-presets-test-resolve-unknown-preset-errors ()
  "Resolving an unknown preset name signals a user-error."
  (anvil-orchestrator-presets-test--with-fresh
    (should-error
     (anvil-orchestrator--preset-resolve
      'nonexistent nil nil nil nil))))

(ert-deftest anvil-orchestrator-presets-test-resolve-preset-fills-defaults ()
  "Preset fills caller's omitted keywords."
  (anvil-orchestrator-presets-test--with-fresh
    (anvil-orchestrator-consensus-preset-set
     'mine '(:providers (claude codex) :threshold 0.6
             :timeout-sec 120))
    (let ((r (anvil-orchestrator--preset-resolve 'mine nil nil nil nil)))
      (should (equal '(claude codex) (plist-get r :providers)))
      (should (= 120 (plist-get r :timeout-sec)))
      (should (= 0.6 (plist-get r :threshold)))
      (should (eq 'mine (plist-get r :preset-name))))))

(ert-deftest anvil-orchestrator-presets-test-resolve-caller-providers-win ()
  "Caller's explicit `:providers' beats the preset's list."
  (anvil-orchestrator-presets-test--with-fresh
    (anvil-orchestrator-consensus-preset-set
     'mine '(:providers (claude codex ollama) :threshold 0.4))
    (let ((r (anvil-orchestrator--preset-resolve
              'mine '(claude gemini) nil nil nil)))
      (should (equal '(claude gemini) (plist-get r :providers)))
      ;; other fields still come from preset
      (should (= 0.4 (plist-get r :threshold))))))

(ert-deftest anvil-orchestrator-presets-test-resolve-caller-timeout-wins ()
  "Caller's `:timeout-sec' beats the preset default."
  (anvil-orchestrator-presets-test--with-fresh
    (anvil-orchestrator-consensus-preset-set
     'mine '(:providers (claude codex) :timeout-sec 600))
    (let ((r (anvil-orchestrator--preset-resolve 'mine nil nil 30 nil)))
      (should (= 30 (plist-get r :timeout-sec))))))

(ert-deftest anvil-orchestrator-presets-test-resolve-merges-overrides ()
  "Overrides are merged per-provider; caller wins on key collision."
  (anvil-orchestrator-presets-test--with-fresh
    (anvil-orchestrator-consensus-preset-set
     'mine '(:providers (claude codex)
             :overrides ((claude . (:model "claude-opus-4-7"))
                         (codex  . (:model "gpt-5.4")))))
    (let* ((r (anvil-orchestrator--preset-resolve
               'mine nil
               '((claude . (:model "claude-haiku-4-5")))
               nil nil))
           (merged (plist-get r :overrides)))
      (should (equal "claude-haiku-4-5"
                     (plist-get (cdr (assq 'claude merged)) :model)))
      (should (equal "gpt-5.4"
                     (plist-get (cdr (assq 'codex merged)) :model))))))


;;;; --- submit-consensus integration --------------------------------------

(defun anvil-orchestrator-presets-test--register-stub (name stdout-text)
  "Register a stub provider NAME that emits STDOUT-TEXT and exits 0."
  (anvil-orchestrator-register-provider
   name
   :build-cmd (lambda (_task)
                (list "/bin/sh" "-c"
                      (format "printf '%%s' %s" (shell-quote-argument stdout-text))))
   :parse-output (lambda (_task stdout _stderr _exit)
                   (list :summary stdout))
   :cost-estimator (lambda (_) nil)))

(defmacro anvil-orchestrator-presets-test--with-fresh-orch (&rest body)
  "Full orchestrator fixture (state + queue + pump) for integration tests.
Duplicates the shape of `anvil-orchestrator-test--with-fresh' because
we can't require that test file without dragging its own tests in."
  (declare (indent 0) (debug t))
  `(let ((anvil-state-db-path (make-temp-file "anvil-orch-pr-st-" nil ".db"))
         (anvil-state--db nil)
         (anvil-orchestrator-work-dir
          (make-temp-file "anvil-orch-pr-work-" t))
         (anvil-orchestrator--tasks (make-hash-table :test 'equal))
         (anvil-orchestrator--running (make-hash-table :test 'equal))
         (anvil-orchestrator--batches (make-hash-table :test 'equal))
         (anvil-orchestrator--consensus-groups (make-hash-table :test 'equal))
         (anvil-orchestrator--queue nil)
         (anvil-orchestrator--pump-timer nil)
         (anvil-orchestrator-poll-interval-sec 0.1))
     (unwind-protect
         (progn
           (anvil-state-enable)
           ,@body)
       (anvil-orchestrator--cancel-pump-timer)
       (anvil-test-fixtures-kill-processes
        anvil-orchestrator--running)
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path))
       (ignore-errors
         (delete-directory anvil-orchestrator-work-dir t)))))

(ert-deftest anvil-orchestrator-presets-test-submit-consensus-with-preset ()
  "`submit-consensus :preset 'P' resolves P and fans out correctly."
  (anvil-orchestrator-presets-test--with-fresh
    (anvil-orchestrator-presets-test--with-fresh-orch
      (anvil-orchestrator-presets-test--register-stub 'tprov-a "a-output")
      (anvil-orchestrator-presets-test--register-stub 'tprov-b "b-output")
      (anvil-orchestrator-consensus-preset-set
       'test-fast '(:providers (tprov-a tprov-b) :threshold 0.5))
      (let* ((result (anvil-orchestrator-submit-consensus
                      :prompt "hello" :preset 'test-fast))
             (task-ids (plist-get result :task-ids)))
        (should (stringp (plist-get result :consensus-id)))
        (should (= 2 (length task-ids)))))))

(provide 'anvil-orchestrator-presets-test)
;;; anvil-orchestrator-presets-test.el ends here
