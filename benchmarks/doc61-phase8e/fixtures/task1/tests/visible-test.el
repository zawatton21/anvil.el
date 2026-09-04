;;; visible-test.el --- visible ERT tests for task1 (strutil) -*- lexical-binding: t; -*-

(require 'ert)
(require 'strutil-core)
(require 'strutil-format)

(ert-deftest visible-strutil-trim ()
  (should (equal (strutil-trim "  hello  ") "hello"))
  (should (equal (strutil-trim "\tworld\n") "world")))

(ert-deftest visible-strutil-truncate-length ()
  "A truncated string must be exactly N characters long, ellipsis included."
  (should (= (length (strutil-truncate "hello world" 8)) 8)))

(ert-deftest visible-strutil-truncate-noop ()
  (should (equal (strutil-truncate "hi" 8) "hi")))

(ert-deftest visible-strutil-split-csv-basic ()
  (should (equal (strutil-split-csv "a,b,c") '("a" "b" "c"))))

(ert-deftest visible-strutil-split-csv-quoted-comma ()
  (should (equal (strutil-split-csv "\"a,b\",c") '("a,b" "c"))))

(ert-deftest visible-strutil-display-width-ascii ()
  (should (= (strutil-display-width "hello") 5)))

(ert-deftest visible-strutil-join-nonempty ()
  "strutil-join-nonempty must join a list, skipping nil/empty entries."
  (should (equal (strutil-join-nonempty '("a" "" nil "b") ",") "a,b")))

(provide 'visible-test)
;;; visible-test.el ends here
