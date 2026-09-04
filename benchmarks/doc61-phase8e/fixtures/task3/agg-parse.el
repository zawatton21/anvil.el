;;; agg-parse.el --- parse "sensor,value" measurement lines -*- lexical-binding: t; -*-

;;; Commentary:
;; Parses lines of the form "SENSOR,VALUE" where VALUE is either a
;; decimal number or the literal marker "NA" for a missing reading.
;; Malformed lines (not exactly one comma, or an unparsable value
;; that isn't NA) are dropped by returning nil; callers filter the
;; nils out.

;;; Code:

(defun agg-parse-line (line)
  "Parse LINE into a plist (:sensor NAME :value NUMBER-or-nil).
Returns nil if LINE is malformed."
  (let ((parts (split-string line "," t "[ \t]+")))
    (when (= (length parts) 2)
      (let ((sensor (nth 0 parts))
            (raw (nth 1 parts)))
        (cond
         ((string= raw "NA") (list :sensor sensor :value nil))
         ((string-match-p "\\`-?[0-9]+\\(\\.[0-9]+\\)?\\'" raw)
          (list :sensor sensor :value (string-to-number raw)))
         (t nil))))))

(defun agg-parse-lines (lines)
  "Parse LINES (a list of strings), dropping malformed entries."
  (delq nil (mapcar #'agg-parse-line lines)))

(provide 'agg-parse)
;;; agg-parse.el ends here
