;;; calc-api.el --- public API for the mini calculator -*- lexical-binding: t; -*-

;;; Code:

(require 'calc-eval)

(defun calc-render-result (expr)
  "Return EXPR's value formatted as a string."
  (number-to-string (calc-eval-string expr)))

(provide 'calc-api)
;;; calc-api.el ends here
