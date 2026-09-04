;;; agg-summarize.el --- group and summarize sensor readings -*- lexical-binding: t; -*-

;;; Commentary:
;; Groups parsed records (see `agg-parse.el') by sensor name and
;; computes a per-sensor summary: how many usable (non-NA) readings
;; there were, their sum and average, and how many NA readings were
;; skipped.

;;; Code:

(require 'agg-parse)

(defun agg-group-by-sensor (records)
  "Group RECORDS (plists with :sensor/:value) into an alist SENSOR -> VALUES.
VALUES is the list of :value entries (numbers or nil for NA) for
that sensor, in input order."
  (let ((table nil))
    (dolist (rec records)
      (let* ((sensor (plist-get rec :sensor))
             (cell (assoc sensor table)))
        (if cell
            (setcdr cell (append (cdr cell) (list (plist-get rec :value))))
          (push (cons sensor (list (plist-get rec :value))) table))))
    (nreverse table)))

(defun agg-summarize-group (values)
  "Summarize VALUES (a list of numbers, with nil marking an NA reading).
Returns a plist (:count N :sum SUM :avg AVG :skipped K): N and SUM
only account for non-NA readings; AVG is SUM/N rounded to 2 decimal
places, or nil when there are no non-NA readings; K is how many NA
\(nil) entries were present."
  (let* ((n (length values))
         (sum (apply #'+ (mapcar (lambda (v) (or v 0)) values))))
    (list :count n :sum sum :avg (/ (float sum) n))))

(defun agg-summarize (records)
  "Summarize RECORDS grouped by sensor.
Returns an alist SENSOR -> summary-plist (see `agg-summarize-group')."
  (mapcar (lambda (cell) (cons (car cell) (agg-summarize-group (cdr cell))))
          (agg-group-by-sensor records)))

(provide 'agg-summarize)
;;; agg-summarize.el ends here
