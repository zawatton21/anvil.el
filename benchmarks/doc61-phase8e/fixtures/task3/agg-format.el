;;; agg-format.el --- human-readable sensor summary lines -*- lexical-binding: t; -*-

;;; Commentary:
;; Formats an `agg-summarize' entry into a single display line, e.g.
;; "sensorA: n=3 skipped=1 avg=15.67" or "sensorB: n=0 skipped=2
;; avg=N/A" when every reading for a sensor was NA.

;;; Code:

(require 'agg-summarize)

(defun agg-format-summary (sensor summary)
  "Format SENSOR and its SUMMARY plist (see `agg-summarize-group') as a line."
  (format "%s: n=%d skipped=%d avg=%s"
          sensor
          (plist-get summary :count)
          (plist-get summary :skipped)
          (let ((avg (plist-get summary :avg)))
            (if avg (format "%.2f" avg) "N/A"))))

(defun agg-format-report (records)
  "Format all of RECORDS (see `agg-parse-lines') as sorted report lines."
  (mapcar (lambda (cell) (agg-format-summary (car cell) (cdr cell)))
          (sort (copy-sequence (agg-summarize records))
                (lambda (a b) (string< (car a) (car b))))))

(provide 'agg-format)
;;; agg-format.el ends here
