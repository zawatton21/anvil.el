;;; visible-test.el --- visible ERT tests for task3 (agg) -*- lexical-binding: t; -*-

(require 'ert)
(require 'agg-parse)
(require 'agg-summarize)
(require 'agg-format)

(ert-deftest visible-agg-parse-line-basic ()
  (should (equal (agg-parse-line "sensorA,12.5") '(:sensor "sensorA" :value 12.5))))

(ert-deftest visible-agg-parse-line-na ()
  (should (equal (agg-parse-line "sensorA,NA") '(:sensor "sensorA" :value nil))))

(ert-deftest visible-agg-parse-line-malformed ()
  (should-not (agg-parse-line "not-a-valid-line")))

(ert-deftest visible-agg-group-by-sensor ()
  (let ((records (agg-parse-lines '("sensorA,10" "sensorB,5" "sensorA,20"))))
    (should (equal (agg-group-by-sensor records)
                    '(("sensorA" 10 20) ("sensorB" 5))))))

(ert-deftest visible-agg-summarize-no-na ()
  "Averaging a clean group with no NA readings."
  (let* ((records (agg-parse-lines '("sensorA,10" "sensorA,20" "sensorA,30")))
         (summary (cdr (assoc "sensorA" (agg-summarize records)))))
    (should (= (plist-get summary :count) 3))
    (should (= (plist-get summary :sum) 60))
    (should (= (plist-get summary :avg) 20.0))))

(ert-deftest visible-agg-format-summary-basic ()
  (let* ((records (agg-parse-lines '("sensorA,10" "sensorA,20")))
         (summary (cdr (assoc "sensorA" (agg-summarize records)))))
    (should (equal (agg-format-summary "sensorA" summary) "sensorA: n=2 skipped=0 avg=15.00"))))

(provide 'visible-test)
;;; visible-test.el ends here
