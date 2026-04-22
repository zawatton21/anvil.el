;;; dataset.el --- Phase 3b broken-paren cases  -*- lexical-binding: t; -*-

;; 30 synthetic elisp snippets representing typical LLM paren-break
;; failure modes.  Each case is a plist:
;;
;;   :id             integer — stable identifier
;;   :category       string  — one of
;;     "trailing-close" / "leading-excess" / "deep-nested" /
;;     "multi-form" / "string-repair" / "unfixable"
;;   :description    human-readable one-liner
;;   :input          raw elisp string (possibly unparseable)
;;   :expected-kind  one of "close-paren-added" / "open-paren-prepended"
;;                   / "noop" / "sexp-cst/repair-failed"
;;   :expected-added integer — expected `added' count on success,
;;                             or nil for `repair-failed'
;;
;; The categories model the failure modes observed in LLM-emitted
;; elisp (claude / gpt / local models): trailing / leading / deep /
;; multi-form cases should all be fixable by close-paren or open-paren
;; balancing; `unfixable' exists as a honesty check — we must not
;; report false positives when the damage is beyond paren count.
;;
;; To add a case: append to `anvil-bench-31-3b-cases', keep `:id'
;; monotonically increasing, and cross-check `:expected-*' against the
;; actual repair output by re-running the benchmark.

;;; Code:

(defconst anvil-bench-31-3b-cases
  '((:id 1
     :category "trailing-close"
     :description "single form, missing one )"
     :input "(foo"
     :expected-kind "close-paren-added"
     :expected-added 1)
    (:id 2
     :category "trailing-close"
     :description "defun with body, missing one )"
     :input "(defun foo () 42"
     :expected-kind "close-paren-added"
     :expected-added 1)
    (:id 3
     :category "trailing-close"
     :description "defun with arg list, missing two )"
     :input "(defun foo (x) (+ x 1"
     :expected-kind "close-paren-added"
     :expected-added 2)
    (:id 4
     :category "trailing-close"
     :description "let-binding body, missing two )"
     :input "(let ((x 1)) (message \"%s\" x"
     :expected-kind "close-paren-added"
     :expected-added 2)
    (:id 5
     :category "trailing-close"
     :description "cond with multiple arms, missing close"
     :input "(cond ((eq a 1) 'one) ((eq a 2) 'two"
     :expected-kind "close-paren-added"
     :expected-added 2)
    (:id 6
     :category "trailing-close"
     :description "lambda body incomplete"
     :input "(lambda (x) (* x 2"
     :expected-kind "close-paren-added"
     :expected-added 2)
    (:id 7
     :category "trailing-close"
     :description "quoted list literal missing )"
     :input "'(1 2 3 4 5"
     :expected-kind "close-paren-added"
     :expected-added 1)
    (:id 8
     :category "trailing-close"
     :description "nested function call missing inner+outer )"
     :input "(message \"%s\" (format \"x=%d\" (+ 1 2"
     :expected-kind "close-paren-added"
     :expected-added 3)
    (:id 9
     :category "trailing-close"
     :description "defvar with docstring, missing )"
     :input "(defvar my-var 42\n  \"A simple docstring.\""
     :expected-kind "close-paren-added"
     :expected-added 1)
    (:id 10
     :category "trailing-close"
     :description "when form with implicit progn"
     :input "(when cond\n  (do-one)\n  (do-two)"
     :expected-kind "close-paren-added"
     :expected-added 1)

    (:id 11
     :category "leading-excess"
     :description "single stray ) at EOF"
     :input "(foo))"
     :expected-kind "open-paren-prepended"
     :expected-added 1)
    (:id 12
     :category "leading-excess"
     :description "two stray ) at EOF"
     :input "(foo)))"
     :expected-kind "open-paren-prepended"
     :expected-added 2)
    (:id 13
     :category "leading-excess"
     :description "bare excess close"
     :input "x 1)"
     :expected-kind "open-paren-prepended"
     :expected-added 1)
    (:id 14
     :category "leading-excess"
     :description "paranoid over-close on defun"
     :input "(defun foo () 1))"
     :expected-kind "open-paren-prepended"
     :expected-added 1)
    (:id 15
     :category "leading-excess"
     :description "over-closed nested form"
     :input "(let ((x 1)) x)))"
     :expected-kind "open-paren-prepended"
     :expected-added 2)

    (:id 16
     :category "deep-nested"
     :description "4-level nest missing innermost close"
     :input "(a (b (c (d 1"
     :expected-kind "close-paren-added"
     :expected-added 4)
    (:id 17
     :category "deep-nested"
     :description "5-level nest missing two closes"
     :input "(a (b (c (d (e 1)"
     :expected-kind "close-paren-added"
     :expected-added 4)
    (:id 18
     :category "deep-nested"
     :description "condition-case with handler body unclosed"
     :input "(condition-case err\n  (risky-op)\n  (error (message \"failed: %S\" err"
     :expected-kind "close-paren-added"
     :expected-added 3)
    (:id 19
     :category "deep-nested"
     :description "doubly-nested let bindings unclosed"
     :input "(let ((x 1))\n  (let ((y 2))\n    (+ x y"
     :expected-kind "close-paren-added"
     :expected-added 3)
    (:id 20
     :category "deep-nested"
     :description "progn inside when inside defun"
     :input "(defun foo ()\n  (when cond\n    (progn\n      (one)\n      (two)"
     :expected-kind "close-paren-added"
     :expected-added 3)

    (:id 21
     :category "multi-form"
     :description "defvar clean + defun unclosed"
     :input "(defvar x 1)\n(defun foo () (+ x 2"
     :expected-kind "close-paren-added"
     :expected-added 2)
    (:id 22
     :category "multi-form"
     :description "comment + clean form + unclosed form"
     :input ";; header\n(defvar a 1)\n(defun b () (cons a a"
     :expected-kind "close-paren-added"
     :expected-added 2)
    (:id 23
     :category "multi-form"
     :description "three defvars, middle over-closed"
     :input "(defvar a 1)\n(defvar b 2))\n(defvar c 3)"
     :expected-kind "open-paren-prepended"
     :expected-added 1)
    (:id 24
     :category "multi-form"
     :description "provide + require + broken defun"
     :input "(provide 'foo)\n(require 'bar)\n(defun baz () (quux"
     :expected-kind "close-paren-added"
     :expected-added 2)
    (:id 25
     :category "multi-form"
     :description "clean form + clean form — already balanced"
     :input "(defvar x 1)\n(defvar y 2)\n"
     :expected-kind "noop"
     :expected-added 0)

    (:id 26
     :category "string-repair"
     :description "unterminated string inside form"
     :input "(foo \"unterminated"
     :expected-kind "unterminated-string-and-close-paren-added"
     :expected-added 2)
    (:id 27
     :category "string-repair"
     :description "unterminated string with paren-looking chars inside"
     :input "(message \"hello (world"
     :expected-kind "unterminated-string-and-close-paren-added"
     :expected-added 2)
    (:id 28
     :category "unfixable"
     :description "swapped parens (structural, not count)"
     :input ")foo("
     :expected-kind "sexp-cst/repair-failed"
     :expected-added nil)
    (:id 29
     :category "unfixable"
     :description "paren-balanced but half-written symbol"
     :input "(defun foo () (#<void>))"
     :expected-kind "sexp-cst/repair-failed"
     :expected-added nil)
    (:id 30
     :category "unfixable"
     :description "balanced but has stray ` at EOF"
     :input "(defun foo () 1)\n`"
     :expected-kind "sexp-cst/repair-failed"
     :expected-added nil))
  "Phase 3b benchmark cases — 30 synthetic broken-paren elisp inputs.
See top-of-file for schema.")

(provide 'anvil-bench-31-3b-dataset)
;;; dataset.el ends here
