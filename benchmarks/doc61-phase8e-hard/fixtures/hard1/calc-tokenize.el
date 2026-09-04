;;; calc-tokenize.el --- tokenize mini calculator expressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Converts an expression string into a flat token list.  Numbers are
;; emitted as numbers, and each operator/parenthesis should be emitted as
;; its own one-character string token, even when operators are adjacent.

;;; Code:

(defun calc--operator-char-p (ch)
  "Return non-nil when CH is an operator or parenthesis character."
  (memq ch '(?+ ?- ?* ?^ ?\( ?\))))

(defun calc-tokenize (expr)
  "Tokenize EXPR into numbers and one-character operator tokens."
  (let ((i 0)
        (len (length expr))
        (tokens nil))
    (while (< i len)
      (let ((ch (aref expr i)))
        (cond
         ((memq ch '(?\s ?\t ?\n ?\r))
          (setq i (1+ i)))
         ((and (>= ch ?0) (<= ch ?9))
          (let ((start i))
            (while (and (< i len)
                        (let ((digit (aref expr i)))
                          (and (>= digit ?0) (<= digit ?9))))
              (setq i (1+ i)))
            (push (string-to-number (substring expr start i)) tokens)))
         ((calc--operator-char-p ch)
          (let ((start i))
            (setq i (1+ i))
            (while (and (< i len)
                        (calc--operator-char-p (aref expr i)))
              (setq i (1+ i)))
            (push (substring expr start i) tokens)))
         (t
          (error "Unexpected character: %c" ch)))))
    (nreverse tokens)))

(provide 'calc-tokenize)
;;; calc-tokenize.el ends here
