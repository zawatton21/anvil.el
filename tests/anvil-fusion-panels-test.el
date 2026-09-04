;;; anvil-fusion-panels-test.el --- ERT for anvil-fusion Phase 2 -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure tests for the named-panel layer (registry, accessors, egress
;; sovereignty enforcement, task expansion).  No orchestrator needed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'anvil-fusion-panels)

;;;; --- accessors -----------------------------------------------------------

(ert-deftest anvil-fusion-panels-test-get-quality ()
  "Quality panel resolves with members / judge / egress."
  (let ((body (anvil-fusion-panel-get 'quality)))
    (should (= 3 (length (anvil-fusion-panel-members body))))
    (should (eq 'claude (car (anvil-fusion-panel-judge body))))
    (should (eq 'external (anvil-fusion-panel-egress body)))))

(ert-deftest anvil-fusion-panels-test-get-sovereign ()
  "Sovereign panel is local-only and all-ollama."
  (let ((body (anvil-fusion-panel-get 'sovereign)))
    (should (eq 'local-only (anvil-fusion-panel-egress body)))
    (should (cl-every (lambda (m) (eq 'ollama (car m)))
                      (anvil-fusion-panel-members body)))
    (should (anvil-fusion-panel-local-only-p 'sovereign))))

(ert-deftest anvil-fusion-panels-test-get-unknown ()
  "Unknown panel name signals."
  (should-error (anvil-fusion-panel-get 'nope) :type 'user-error))

(ert-deftest anvil-fusion-panels-test-provider-local-p ()
  "Local classification follows `anvil-fusion-local-providers'."
  (should (anvil-fusion-provider-local-p 'ollama))
  (should-not (anvil-fusion-provider-local-p 'claude))
  (let ((anvil-fusion-local-providers '(ollama mlx)))
    (should (anvil-fusion-provider-local-p 'mlx))))

;;;; --- validation: all defaults pass --------------------------------------

(ert-deftest anvil-fusion-panels-test-defaults-valid ()
  "Every shipped panel validates."
  (dolist (p anvil-fusion-panels)
    (should (anvil-fusion-panel-validate (car p)))))

(ert-deftest anvil-fusion-panels-test-opus-solo-exists ()
  "The Phase 7 Opus self-consistency panel is registered."
  (should (assq 'opus-solo anvil-fusion-panels)))

(ert-deftest anvil-fusion-panels-test-opus-solo-shape ()
  "The Opus self-consistency panel matches the shipped design."
  (let* ((body (anvil-fusion-panel-get 'opus-solo))
         (members (anvil-fusion-panel-members body)))
    (should (= 3 (length members)))
    (should (equal '((claude . "claude-opus-4-8")
                     (claude . "claude-opus-4-8")
                     (claude . "claude-opus-4-8"))
                   members))
    (should (equal '(claude . "claude-opus-4-8")
                   (anvil-fusion-panel-judge body)))
    (should (eq 'external (anvil-fusion-panel-egress body)))
    (should (equal '(correctness completeness skeptic)
                   (cdr (assq 'lenses body))))))

(ert-deftest anvil-fusion-panels-test-opus-solo-validates ()
  "The shipped Opus self-consistency panel validates."
  (should (anvil-fusion-panel-validate 'opus-solo)))

(ert-deftest anvil-fusion-panels-test-sonnet-solo-exists ()
  "The Sonnet self-consistency panel is registered."
  (should (assq 'sonnet-solo anvil-fusion-panels)))

(ert-deftest anvil-fusion-panels-test-sonnet-solo-shape ()
  "The Sonnet self-consistency panel matches the shipped design."
  (let* ((body (anvil-fusion-panel-get 'sonnet-solo))
         (members (anvil-fusion-panel-members body)))
    (should (= 3 (length members)))
    (should (equal '((claude . "claude-sonnet-5")
                     (claude . "claude-sonnet-5")
                     (claude . "claude-sonnet-5"))
                   members))
    (should (equal '(claude . "claude-sonnet-5")
                   (anvil-fusion-panel-judge body)))
    (should (eq 'external (anvil-fusion-panel-egress body)))
    (should (equal '(correctness completeness skeptic)
                   (cdr (assq 'lenses body))))))

(ert-deftest anvil-fusion-panels-test-sonnet-solo-validates ()
  "The shipped Sonnet self-consistency panel validates."
  (should (anvil-fusion-panel-validate 'sonnet-solo)))

;;;; --- validation: sovereignty enforcement --------------------------------

(ert-deftest anvil-fusion-panels-test-local-only-rejects-external-member ()
  "A local-only panel containing an external member is rejected."
  (let ((anvil-fusion-panels
         '((bad (members . ((ollama . "q") (claude . "claude-opus-4-8")))
                (judge . (ollama . "q"))
                (egress . local-only)))))
    (should-error (anvil-fusion-panel-validate 'bad) :type 'user-error)))

(ert-deftest anvil-fusion-panels-test-local-only-rejects-external-judge ()
  "A local-only panel judged by an external provider is rejected."
  (let ((anvil-fusion-panels
         '((bad (members . ((ollama . "q") (ollama . "d")))
                (judge . (claude . "claude-opus-4-8"))
                (egress . local-only)))))
    (should-error (anvil-fusion-panel-validate 'bad) :type 'user-error)))

(ert-deftest anvil-fusion-panels-test-rejects-unknown-egress ()
  "An unrecognized egress value is rejected."
  (let ((anvil-fusion-panels
         '((bad (members . ((ollama . "q") (ollama . "d")))
                (judge . (ollama . "q"))
                (egress . wat)))))
    (should-error (anvil-fusion-panel-validate 'bad) :type 'user-error)))

(ert-deftest anvil-fusion-panels-test-rejects-single-member ()
  "A one-member panel is not a fusion panel."
  (let ((anvil-fusion-panels
         '((bad (members . ((ollama . "q")))
                (judge . (ollama . "q"))
                (egress . local-only)))))
    (should-error (anvil-fusion-panel-validate 'bad) :type 'user-error)))

(ert-deftest anvil-fusion-panels-test-rejects-bad-member-shape ()
  "A malformed member spec is rejected."
  (let ((anvil-fusion-panels
         '((bad (members . ((ollama . "q") "not-a-cons"))
                (judge . (ollama . "q"))
                (egress . local-only)))))
    (should-error (anvil-fusion-panel-validate 'bad) :type 'user-error)))

;;;; --- task expansion ------------------------------------------------------

(ert-deftest anvil-fusion-panels-test-tasks-shape ()
  "Task expansion yields one provider/prompt/model plist per member."
  (let* ((body (anvil-fusion-panel-get 'sovereign))
         (tasks (anvil-fusion-panel-tasks body "PROMPT-X")))
    (should (= 3 (length tasks)))
    (should (eq 'ollama (plist-get (car tasks) :provider)))
    (should (equal "PROMPT-X" (plist-get (car tasks) :prompt)))
    (should (equal "llama3.1:8b" (plist-get (car tasks) :model)))))

(ert-deftest anvil-fusion-panels-test-tasks-have-name ()
  "Every member task carries a non-empty :name (orchestrator requires it)."
  (let ((tasks (anvil-fusion-panel-tasks
                (anvil-fusion-panel-get 'sovereign) "Q")))
    (should (cl-every (lambda (tk)
                        (let ((n (plist-get tk :name)))
                          (and (stringp n) (not (string-empty-p n)))))
                      tasks))
    ;; names are distinct per member
    (should (= 3 (length (delete-dups
                          (mapcar (lambda (tk) (plist-get tk :name)) tasks)))))))

(ert-deftest anvil-fusion-panels-test-tasks-omit-nil-model ()
  "Members with nil model omit the :model key (provider default)."
  (let* ((anvil-fusion-panels
          '((p (members . ((ollama . nil) (ollama . "d")))
               (judge . (ollama . "d"))
               (egress . local-only))))
         (tasks (anvil-fusion-panel-tasks (anvil-fusion-panel-get 'p) "Q")))
    (should-not (plist-member (car tasks) :model))
    (should (equal "d" (plist-get (cadr tasks) :model)))))

(ert-deftest anvil-fusion-panels-test-tasks-preserve-dup-providers ()
  "Duplicate providers are preserved as separate tasks."
  (let ((tasks (anvil-fusion-panel-tasks
                (anvil-fusion-panel-get 'sovereign) "Q")))
    (should (cl-every (lambda (tk) (eq 'ollama (plist-get tk :provider)))
                      tasks))
    (should (equal '("llama3.1:8b" "llama3.2:3b" "gemma4:e4b")
                   (mapcar (lambda (tk) (plist-get tk :model)) tasks)))))

(ert-deftest anvil-fusion-panels-test-opus-solo-tasks ()
  "Opus self-consistency members keep order, names, and lens prefixes."
  (let* ((body (anvil-fusion-panel-get 'opus-solo))
         (prompt "PROMPT-X")
         (lenses (cdr (assq 'lenses body)))
         (tasks (anvil-fusion-panel-tasks body prompt lenses))
         (prefixes (mapcar (lambda (lens) (cdr (assq lens anvil-fusion-lenses)))
                           lenses))
         (names (mapcar (lambda (tk) (plist-get tk :name)) tasks)))
    (should (= 3 (length tasks)))
    (should (cl-every (lambda (tk) (eq 'claude (plist-get tk :provider)))
                      tasks))
    (should (equal '("claude-opus-4-8" "claude-opus-4-8" "claude-opus-4-8")
                   (mapcar (lambda (tk) (plist-get tk :model)) tasks)))
    (should (= 3 (length (delete-dups (copy-sequence names)))))
    (should (string-prefix-p (nth 0 prefixes) (plist-get (nth 0 tasks) :prompt)))
    (should (string-prefix-p (nth 1 prefixes) (plist-get (nth 1 tasks) :prompt)))
    (should (string-prefix-p (nth 2 prefixes) (plist-get (nth 2 tasks) :prompt)))
    (should-not (equal (plist-get (nth 0 tasks) :prompt)
                       (plist-get (nth 1 tasks) :prompt)))
    (should-not (equal (plist-get (nth 1 tasks) :prompt)
                       (plist-get (nth 2 tasks) :prompt)))
    (should-not (equal (plist-get (nth 0 tasks) :prompt)
                       (plist-get (nth 2 tasks) :prompt)))))

(ert-deftest anvil-fusion-panels-test-sonnet-solo-tasks ()
  "Sonnet self-consistency members keep order, names, and lens prefixes."
  (let* ((body (anvil-fusion-panel-get 'sonnet-solo))
         (prompt "PROMPT-X")
         (lenses (cdr (assq 'lenses body)))
         (tasks (anvil-fusion-panel-tasks body prompt lenses))
         (prefixes (mapcar (lambda (lens) (cdr (assq lens anvil-fusion-lenses)))
                           lenses))
         (names (mapcar (lambda (tk) (plist-get tk :name)) tasks)))
    (should (= 3 (length tasks)))
    (should (cl-every (lambda (tk) (eq 'claude (plist-get tk :provider)))
                      tasks))
    (should (equal '("claude-sonnet-5" "claude-sonnet-5" "claude-sonnet-5")
                   (mapcar (lambda (tk) (plist-get tk :model)) tasks)))
    (should (= 3 (length (delete-dups (copy-sequence names)))))
    (should (string-prefix-p (nth 0 prefixes) (plist-get (nth 0 tasks) :prompt)))
    (should (string-prefix-p (nth 1 prefixes) (plist-get (nth 1 tasks) :prompt)))
    (should (string-prefix-p (nth 2 prefixes) (plist-get (nth 2 tasks) :prompt)))
    (should-not (equal (plist-get (nth 0 tasks) :prompt)
                       (plist-get (nth 1 tasks) :prompt)))
    (should-not (equal (plist-get (nth 1 tasks) :prompt)
                       (plist-get (nth 2 tasks) :prompt)))
    (should-not (equal (plist-get (nth 0 tasks) :prompt)
                       (plist-get (nth 2 tasks) :prompt)))))

(ert-deftest anvil-fusion-panels-test-member-extras-whitelist-merged ()
  "Whitelisted MEMBER-EXTRAS keys reach every member; unknown keys are dropped."
  (let* ((tasks (anvil-fusion-panel-tasks
                 (anvil-fusion-panel-get 'sovereign) "Q" nil nil
                 '(:permission-mode "bypassPermissions"
                   :allowed-tools "Bash"
                   :sandbox "workspace-write"
                   :no-worktree t
                   :timeout-sec 55
                   :bogus "nope"))))
    (should (= 3 (length tasks)))
    (dolist (task tasks)
      (should (equal "bypassPermissions" (plist-get task :permission-mode)))
      (should (equal "Bash" (plist-get task :allowed-tools)))
      (should (equal "workspace-write" (plist-get task :sandbox)))
      (should (eq t (plist-get task :no-worktree)))
      (should (= 55 (plist-get task :timeout-sec)))
      (should-not (plist-member task :bogus)))))

(ert-deftest anvil-fusion-panels-test-member-extras-nil-regression-identical ()
  "Nil MEMBER-EXTRAS keeps the exact prior task plist shape."
  (let* ((body (anvil-fusion-panel-get 'sovereign))
         (expected
          '((:provider ollama :prompt "Q" :name "fusion-member-0-ollama"
             :model "llama3.1:8b" :cwd "/tmp")
            (:provider ollama :prompt "Q" :name "fusion-member-1-ollama"
             :model "llama3.2:3b" :cwd "/tmp")
            (:provider ollama :prompt "Q" :name "fusion-member-2-ollama"
             :model "gemma4:e4b" :cwd "/tmp"))))
    (should (equal expected
                   (anvil-fusion-panel-tasks body "Q" nil "/tmp")))
    (should (equal expected
                   (anvil-fusion-panel-tasks body "Q" nil "/tmp" nil)))))

(provide 'anvil-fusion-panels-test)
;;; anvil-fusion-panels-test.el ends here
