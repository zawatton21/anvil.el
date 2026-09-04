;;; rq-predicate.el --- predicate helpers for rq -*- lexical-binding: t; -*-

;;; Code:

(require 'rq-data)

(defun rq-compile-predicate (field value)
  "Compile FIELD=VALUE into a predicate over one row."
  (let ((field-sym (rq--field-symbol field)))
    (lambda (row)
      (equal (alist-get field-sym row nil nil #'eq) value))))

(defun rq-compose-predicates (predicates)
  "Compose PREDICATES with logical AND."
  (lambda (row)
    (let ((ok t))
      (while (and predicates ok)
        (setq ok (funcall (car predicates) row))
        (setq predicates (cdr predicates)))
      ok)))

(provide 'rq-predicate)
;;; rq-predicate.el ends here
