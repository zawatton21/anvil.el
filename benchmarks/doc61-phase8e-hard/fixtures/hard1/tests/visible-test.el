;;; visible-test.el --- visible tests for hard1 -*- lexical-binding: t; -*-

(require 'ert)
(require 'calc-tokenize)
(require 'calc-parse)
(require 'calc-eval)
(require 'calc-api)

(ert-deftest visible-calc-binary-precedence ()
  (should (= (calc-eval-string "2 + 3 * 4") 14))
  (should (= (calc-eval-string "2 ^ 3 ^ 2") 512)))

(ert-deftest visible-calc-parentheses ()
  (should (= (calc-eval-string "(2 + 3) * 4") 20)))

(ert-deftest visible-calc-unary-minus-basic ()
  "Unary minus should work in a simple prefix position."
  (should (= (calc-eval-string "-3 + 5") 2)))

(ert-deftest visible-calc-unary-minus-after-binary ()
  "The visible suite checks the easy spaced case, not the adjacency edge."
  (should (= (calc-eval-string "2 * -3") -6)))

(ert-deftest visible-calc-render-result ()
  (should (equal (calc-render-result "-4 + 10") "6")))

(provide 'visible-test)
;;; visible-test.el ends here
