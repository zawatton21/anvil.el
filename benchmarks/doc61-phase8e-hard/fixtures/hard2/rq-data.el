;;; rq-data.el --- row helpers for the rq DSL -*- lexical-binding: t; -*-

;;; Code:

(defun rq--field-symbol (field)
  "Normalize FIELD to a symbol.
Joined right-table fields use qualified names such as `users.team'."
  (if (symbolp field) field (intern field)))

(defun rq-row-get (row field)
  "Return FIELD from ROW, or nil if it is absent."
  (alist-get (rq--field-symbol field) row nil nil #'eq))

(defun rq-row-merge-joined (left right dataset)
  "Merge LEFT and RIGHT rows for a join against DATASET.
LEFT keys remain unchanged.  RIGHT keys are re-added under qualified
names like `users.team'."
  (let ((merged (copy-tree left)))
    (dolist (cell right)
      (push (cons (intern (format "%s.%s" dataset (car cell))) (cdr cell))
            merged))
    (nreverse merged)))

(provide 'rq-data)
;;; rq-data.el ends here
