;;; hard3-hidden-test.el --- hidden tests for hard3 -*- lexical-binding: t; -*-

(require 'ert)
(require 'tt-width)
(require 'tt-layout)
(require 'tt-draw)
(require 'tt)

(ert-deftest hidden-tt-cjk-widths-contribute-two-columns ()
  (let ((columns '((:key name :title "Name" :align left)
                   (:key score :title "Score" :align right))))
    (should (equal (tt-compute-widths columns
                                      '(((name . "あい") (score . 7))))
                   '(4 5)))))

(ert-deftest hidden-tt-cjk-body-row-renders-with-correct-border ()
  (let ((columns '((:key name :title "Name" :align left)
                   (:key score :title "Score" :align right))))
    (should (equal (tt-render columns '(((name . "あい") (score . 7))))
                   "+------+-------+\n| Name | Score |\n+------+-------+\n| あい |     7 |\n+------+-------+"))))

(ert-deftest hidden-tt-right-align-uses-display-width ()
  (let ((columns '((:key name :title "Name" :align left)
                   (:key score :title "Score" :align right))))
    (should (equal (tt-render columns '(((name . "あ") (score . 12))))
                   "+------+-------+\n| Name | Score |\n+------+-------+\n| あ   |    12 |\n+------+-------+"))))

(ert-deftest hidden-tt-center-align-odd-padding-extra-goes-right ()
  (let ((columns '((:key team :title "Groups" :align center))))
    (should (equal (tt-render columns '(((team . "ops"))))
                   "+--------+\n| Groups |\n+--------+\n|  ops   |\n+--------+"))))

(ert-deftest hidden-tt-header-uses-same-padding-helper ()
  (let ((columns '((:key team :title "部門" :align center)
                   (:key n :title "N" :align right))))
    (should (equal (tt-render columns '(((team . "ops") (n . 2))))
                   "+------+---+\n| 部門 | N |\n+------+---+\n| ops  | 2 |\n+------+---+"))))

(ert-deftest hidden-tt-empty-rows-still-render-header ()
  (let ((columns '((:key name :title "Name" :align left))))
    (should (equal (tt-render columns nil)
                   "+------+\n| Name |\n+------+\n+------+"))))

(ert-deftest hidden-tt-string-width-mixed-cjk ()
  (should (= (tt-string-width "AあB") 4)))

(provide 'hard3-hidden-test)
;;; hard3-hidden-test.el ends here
