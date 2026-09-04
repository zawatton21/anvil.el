;;; visible-test.el --- visible ERT tests for task2 (flow) -*- lexical-binding: t; -*-

(require 'ert)
(require 'flow)
(require 'flow-report)

(ert-deftest visible-flow-happy-path ()
  "new -> open -> resolved -> closed via the direct approve path."
  (let* ((m0 (flow-make))
         (m1 (flow-apply m0 'start))
         (m2 (flow-apply m1 'approve))
         (m3 (flow-apply m2 'close)))
    (should (eq (plist-get m3 :state) 'closed))
    (should (equal (flow-history-summary m3) "new -> open -> resolved -> closed"))))

(ert-deftest visible-flow-invalid-transition-signals ()
  (should-error (flow-apply (flow-make) 'close) :type 'flow-invalid-transition))

(ert-deftest visible-flow-can-fire-p ()
  (should (flow-can-fire-p (flow-make) 'start))
  (should-not (flow-can-fire-p (flow-make) 'close)))

(ert-deftest visible-flow-reopen-after-resolve ()
  "A resolved ticket can be reopened back into the open workflow."
  (let* ((m (flow-apply (flow-apply (flow-make) 'start) 'approve))
         (reopened (flow-apply m 'reopen))
         (back-open (flow-apply reopened 'start)))
    (should (eq (plist-get reopened :state) 'reopened))
    (should (eq (plist-get back-open :state) 'open))))

(provide 'visible-test)
;;; visible-test.el ends here
