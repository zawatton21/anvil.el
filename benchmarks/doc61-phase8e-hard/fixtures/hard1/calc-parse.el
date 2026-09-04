;;; calc-parse.el --- Pratt parser for mini calculator -*- lexical-binding: t; -*-

;;; Commentary:
;; Parses the token stream from `calc-tokenize' into a small AST:
;;   (num N)
;;   (bin OP LEFT RIGHT)
;;
;; Binary precedence is table-driven.  The obvious place to bolt prefix
;; operators onto this file is *not* the same as the correct one, because
;; prefix and infix operators do not share the same binding-power rule.

;;; Code:

(require 'calc-tokenize)

(defconst calc-operator-precedence
  '(("+" . 10)
    ("-" . 10)
    ("*" . 20)
    ("^" . 40))
  "Binding power for infix operators.")

(defun calc--right-associative-p (op)
  "Return non-nil if OP is right-associative."
  (equal op "^"))

(defun calc--infix-binding-power (op)
  "Return the infix binding power for OP, or nil."
  (cdr (assoc op calc-operator-precedence)))

(defun calc-parse (tokens)
  "Parse TOKENS into an AST."
  (pcase-let ((`(,ast . ,rest) (calc--parse-expression tokens 0)))
    (when rest
      (error "Unexpected trailing tokens: %S" rest))
    ast))

(defun calc--parse-expression (tokens min-bp)
  "Parse TOKENS as an expression with at least MIN-BP binding power."
  (pcase-let ((`(,left . ,rest) (calc--parse-primary tokens)))
    (while (and rest
                (stringp (car rest))
                (calc--infix-binding-power (car rest))
                (>= (calc--infix-binding-power (car rest)) min-bp))
      (let* ((op (car rest))
             (bp (calc--infix-binding-power op))
             (next-min (if (calc--right-associative-p op) bp (1+ bp))))
        (pcase-let ((`(,right . ,tail)
                     (calc--parse-expression (cdr rest) next-min)))
          (setq left (list 'bin op left right))
          (setq rest tail))))
    (cons left rest)))

(defun calc--parse-primary (tokens)
  "Parse a number or parenthesized expression from TOKENS."
  (unless tokens
    (error "Unexpected end of input"))
  (let ((tok (car tokens)))
    (cond
     ((numberp tok)
      (cons (list 'num tok) (cdr tokens)))
     ((equal tok "(")
      (pcase-let ((`(,inner . ,rest) (calc--parse-expression (cdr tokens) 0)))
        (unless (equal (car rest) ")")
          (error "Expected closing parenthesis"))
        (cons inner (cdr rest))))
     (t
      (error "Unexpected token: %S" tok)))))

(provide 'calc-parse)
;;; calc-parse.el ends here
