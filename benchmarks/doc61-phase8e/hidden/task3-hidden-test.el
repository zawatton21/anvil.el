;;; task3-hidden-test.el --- hidden acceptance tests for task 3 (agg) -*- lexical-binding: t; -*-
;;
;; NOT visible to the agent working on fixtures/task3/. Applied at
;; scoring time only (see ../score.sh). Stricter/edge-case coverage
;; beyond fixtures/task3/tests/visible-test.el -- in particular NA
;; handling, all-NA groups, and rounding, none of which the visible
;; NA-free happy-path test can catch.

(require 'ert)
(require 'agg-parse)
(require 'agg-summarize)
(require 'agg-format)

(ert-deftest hidden-agg-summarize-excludes-na-from-avg-and-count ()
  "NA readings must not dilute the count or the average."
  (let* ((records (agg-parse-lines '("sensorA,10" "sensorA,NA" "sensorA,20")))
         (summary (cdr (assoc "sensorA" (agg-summarize records)))))
    (should (= (plist-get summary :count) 2))
    (should (= (plist-get summary :sum) 30))
    (should (= (plist-get summary :avg) 15.0))
    (should (= (plist-get summary :skipped) 1))))

(ert-deftest hidden-agg-summarize-rounds-to-two-decimals ()
  (let* ((records (agg-parse-lines '("sensorA,10" "sensorA,15" "sensorA,22")))
         (summary (cdr (assoc "sensorA" (agg-summarize records)))))
    (should (= (plist-get summary :avg) 15.67))))

(ert-deftest hidden-agg-summarize-all-na-avg-is-nil ()
  "A sensor with only NA readings must report a nil average, not error."
  (let* ((records (agg-parse-lines '("sensorA,NA" "sensorA,NA")))
         (summary (cdr (assoc "sensorA" (agg-summarize records)))))
    (should (= (plist-get summary :count) 0))
    (should (= (plist-get summary :skipped) 2))
    (should (null (plist-get summary :avg)))))

(ert-deftest hidden-agg-summarize-group-empty-input ()
  "Calling the group summarizer directly on no readings must not signal an error."
  (let ((summary (agg-summarize-group nil)))
    (should (= (plist-get summary :count) 0))
    (should (= (plist-get summary :skipped) 0))
    (should (null (plist-get summary :avg)))))

(ert-deftest hidden-agg-format-summary-all-na ()
  (let* ((records (agg-parse-lines '("sensorB,NA")))
         (summary (cdr (assoc "sensorB" (agg-summarize records)))))
    (should (equal (agg-format-summary "sensorB" summary) "sensorB: n=0 skipped=1 avg=N/A"))))

(ert-deftest hidden-agg-summarize-multi-sensor-skipped-independent ()
  (let* ((records (agg-parse-lines '("sensorA,10" "sensorA,NA" "sensorB,5" "sensorB,NA" "sensorB,NA")))
         (summaries (agg-summarize records)))
    (should (= (plist-get (cdr (assoc "sensorA" summaries)) :skipped) 1))
    (should (= (plist-get (cdr (assoc "sensorB" summaries)) :skipped) 2))))

(provide 'task3-hidden-test)
;;; task3-hidden-test.el ends here
