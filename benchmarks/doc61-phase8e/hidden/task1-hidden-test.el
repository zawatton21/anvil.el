;;; task1-hidden-test.el --- hidden acceptance tests for task 1 (strutil) -*- lexical-binding: t; -*-
;;
;; NOT visible to the agent working on fixtures/task1/. Applied at
;; scoring time only (see ../score.sh). Stricter/edge-case coverage
;; beyond fixtures/task1/tests/visible-test.el.

(require 'ert)
(require 'strutil-core)
(require 'strutil-format)

(ert-deftest hidden-strutil-truncate-exact-content ()
  "Truncation must reserve room for the ellipsis, not just append it."
  (should (equal (strutil-truncate "hello world" 8) "hello...")))

(ert-deftest hidden-strutil-truncate-small-width ()
  "Very small widths still return a string no longer than N."
  (should (<= (length (strutil-truncate "hello world" 3)) 3)))

(ert-deftest hidden-strutil-display-width-japanese ()
  "Wide (CJK) characters must count as two display columns."
  (should (= (strutil-display-width "ab") 2))
  (should (= (strutil-display-width "あい") 4))
  (should (= (strutil-display-width "aあb") 4)))

(ert-deftest hidden-strutil-pad-display-width-mixed ()
  "Padding must reach the target display width, not the raw char count."
  (should (equal (strutil-pad-display-width "あ" 4) "あ  ")))

(ert-deftest hidden-strutil-join-nonempty-mixed ()
  (should (equal (strutil-join-nonempty '(nil "" "x" "" "y" nil) "-") "x-y")))

(ert-deftest hidden-strutil-join-nonempty-all-empty ()
  (should (equal (strutil-join-nonempty '(nil "" nil) ",") "")))

(ert-deftest hidden-strutil-split-csv-trailing-empty-fields ()
  (should (equal (strutil-split-csv "a,b,,") '("a" "b" "" ""))))

(ert-deftest hidden-strutil-split-csv-escaped-quote ()
  (should (equal (strutil-split-csv "\"he said \"\"hi\"\"\",b")
                  '("he said \"hi\"" "b"))))

(provide 'task1-hidden-test)
;;; task1-hidden-test.el ends here
