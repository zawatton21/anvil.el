;;; calc-eval.el --- evaluate mini calculator ASTs -*- lexical-binding: t; -*-

;;; Code:

(require 'calc-parse)

(defun calc-eval-ast (ast)
  "Evaluate AST produced by `calc-parse'."
  (pcase ast
    (`(num ,n) n)
    (`(bin "+" ,l ,r) (+ (calc-eval-ast l) (calc-eval-ast r)))
    (`(bin "-" ,l ,r) (- (calc-eval-ast l) (calc-eval-ast r)))
    (`(bin "*" ,l ,r) (* (calc-eval-ast l) (calc-eval-ast r)))
    (`(bin "^" ,l ,r) (expt (calc-eval-ast l) (calc-eval-ast r)))
    (_ (error "Unknown AST node: %S" ast))))

(defun calc-eval-string (expr)
  "Tokenize, parse, and evaluate EXPR."
  (calc-eval-ast (calc-parse (calc-tokenize expr))))

(provide 'calc-eval)
;;; calc-eval.el ends here
