;;; hard1-hidden-test.el --- hidden tests for hard1 -*- lexical-binding: t; -*-

(require 'ert)
(require 'calc-tokenize)
(require 'calc-parse)
(require 'calc-eval)
(require 'calc-api)

(ert-deftest hidden-calc-tokenize-adjacent-operators ()
  "Adjacent operators must stay separate once unary minus exists."
  (should (equal (calc-tokenize "1--2") '(1 "-" "-" 2))))

(ert-deftest hidden-calc-unary-minus-vs-exponent ()
  "Unary minus binds looser than exponentiation."
  (should (= (calc-eval-string "-2^2") -4)))

(ert-deftest hidden-calc-parenthesized-negative-base ()
  (should (= (calc-eval-string "(-2)^2") 4)))

(ert-deftest hidden-calc-adjacent-minus-evaluates ()
  (should (= (calc-eval-string "1--2") 3)))

(ert-deftest hidden-calc-negative-exponent ()
  (should (= (calc-eval-string "2^-2") 0.25)))

(ert-deftest hidden-calc-nested-prefix-minus ()
  (should (= (calc-eval-string "-(2 + 3) * 4") -20)))

(ert-deftest hidden-calc-render-adjacent-minus ()
  (should (equal (calc-render-result "10--7") "17")))

(provide 'hard1-hidden-test)
;;; hard1-hidden-test.el ends here
