;;; rq-parse.el --- parse pipe-based rq queries -*- lexical-binding: t; -*-

;;; Code:

(require 'subr-x)

(defun rq--parse-clause (clause)
  "Parse one CLAUSE string into an internal clause form."
  (cond
   ((string-match "\\`from[ \t]+\\([A-Za-z0-9_-]+\\)\\'" clause)
    (list 'from (intern (match-string 1 clause))))
   ((string-match "\\`where[ \t]+\\([A-Za-z0-9_.-]+\\)=\\([^|]+\\)\\'" clause)
    (list 'where
          (match-string 1 clause)
          (string-trim (match-string 2 clause))))
   (t
    (error "Unsupported clause: %s" clause))))

(defun rq-parse-query (query)
  "Parse QUERY into a list of clause forms."
  (mapcar #'rq--parse-clause
          (mapcar #'string-trim
                  (split-string query "|"))))

(provide 'rq-parse)
;;; rq-parse.el ends here
