;;; visible-test.el --- visible tests for hard3 -*- lexical-binding: t; -*-

(require 'ert)
(require 'tt-width)
(require 'tt-layout)
(require 'tt-draw)
(require 'tt)

(defconst visible-tt-columns
  '((:key name :title "Name" :align left)
    (:key score :title "Score" :align right)))

(ert-deftest visible-tt-string-width-ascii ()
  (should (= (tt-string-width "abc") 3)))

(ert-deftest visible-tt-compute-widths-ascii ()
  (should (equal (tt-compute-widths visible-tt-columns
                                    '(((name . "Ann") (score . 7))
                                      ((name . "Bobby") (score . 12))))
                 '(5 5))))

(ert-deftest visible-tt-right-align-body ()
  "The visible suite checks the easy ASCII alignment case."
  (should (equal (tt-render visible-tt-columns
                            '(((name . "Ann") (score . 7))))
                 "+------+-------+\n| Name | Score |\n+------+-------+\n| Ann  |     7 |\n+------+-------+") ))

(ert-deftest visible-tt-center-align-header ()
  (let ((columns '((:key team :title "Team" :align center)
                   (:key n :title "N" :align right))))
    (should (equal (tt-render columns '(((team . "ops") (n . 2))))
                   "+------+---+\n| Team | N |\n+------+---+\n| ops  | 2 |\n+------+---+"))))

(ert-deftest visible-tt-string-width-cjk-direct ()
  (should (= (tt-string-width "あ") 2)))

(provide 'visible-test)
;;; visible-test.el ends here
