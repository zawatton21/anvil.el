;;; rq-eval.el --- evaluate rq queries -*- lexical-binding: t; -*-

;;; Code:

(require 'seq)
(require 'rq-parse)
(require 'rq-predicate)

(defun rq--dataset (env name)
  "Return dataset NAME from ENV, or signal if missing."
  (or (cdr (assq name env))
      (error "Unknown dataset: %s" name)))

(defun rq--apply-where (rows field value)
  "Filter ROWS to only those with FIELD=VALUE."
  (let ((pred (rq-compile-predicate field value)))
    (seq-filter pred rows)))

(defun rq-run (env query)
  "Run QUERY against ENV and return raw rows."
  (let ((clauses (rq-parse-query query))
        (rows nil))
    (dolist (clause clauses)
      (pcase clause
        (`(from ,name)
         (setq rows (copy-tree (rq--dataset env name))))
        (`(where ,field ,value)
         (setq rows (rq--apply-where rows field value)))
        (_
         (error "Unsupported clause in evaluator: %S" clause))))
    rows))

(provide 'rq-eval)
;;; rq-eval.el ends here
