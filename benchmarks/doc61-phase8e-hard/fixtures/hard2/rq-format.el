;;; rq-format.el --- format rq query results -*- lexical-binding: t; -*-

;;; Code:

(require 'rq-eval)

(defun rq--grouped-result-p (result)
  "Return non-nil when RESULT looks like grouped rows."
  (let ((first (car result)))
    (and first (assq :count first))))

(defun rq--sort-groups (groups)
  "Sort GROUPS by count descending, then key alphabetically."
  (sort (copy-sequence groups)
        (lambda (a b)
          (let ((count-a (cdr (assq :count a)))
                (count-b (cdr (assq :count b)))
                (key-a (cdr (assq :key a)))
                (key-b (cdr (assq :key b))))
            (if (/= count-a count-b)
                (> count-a count-b)
              (string< key-a key-b))))))

(defun rq-format-result (result)
  "Format RESULT as display lines."
  (if (rq--grouped-result-p result)
      (mapcar (lambda (group)
                (format "%s => %d"
                        (cdr (assq :key group))
                        (cdr (assq :count group))))
              (rq--sort-groups result))
    (mapcar (lambda (row)
              (format "%s"
                      (mapconcat (lambda (cell)
                                   (format "%s=%s" (car cell) (cdr cell)))
                                 row
                                 " ")))
            result)))

(defun rq-run-lines (env query)
  "Run QUERY against ENV and format the result."
  (rq-format-result (rq-run env query)))

(provide 'rq-format)
;;; rq-format.el ends here
