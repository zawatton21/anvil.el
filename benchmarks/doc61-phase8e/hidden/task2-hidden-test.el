;;; task2-hidden-test.el --- hidden acceptance tests for task 2 (flow) -*- lexical-binding: t; -*-
;;
;; NOT visible to the agent working on fixtures/task2/. Applied at
;; scoring time only (see ../score.sh). Stricter/edge-case coverage
;; beyond fixtures/task2/tests/visible-test.el -- in particular this
;; catches the pending->approve regression that the visible happy
;; path never exercises (it goes through open->approve instead).

(require 'ert)
(require 'flow)
(require 'flow-report)

(ert-deftest hidden-flow-pending-approve-goes-to-resolved ()
  "Per spec, approving a pending ticket must resolve it, not close it directly."
  (let* ((m (flow-apply (flow-apply (flow-make) 'start) 'hold))
         (approved (flow-apply m 'approve)))
    (should (eq (plist-get approved :state) 'resolved))))

(ert-deftest hidden-flow-pending-path-then-close ()
  (let* ((m (flow-apply (flow-apply (flow-apply (flow-make) 'start) 'hold) 'approve))
         (closed (flow-apply m 'close)))
    (should (eq (plist-get closed :state) 'closed))))

(ert-deftest hidden-flow-reopen-then-resume-cycle ()
  "A reopened ticket can go through hold/resume again before re-resolving."
  (let* ((m (flow-apply (flow-apply (flow-make) 'start) 'approve))
         (reopened (flow-apply m 'reopen))
         (open2 (flow-apply reopened 'start))
         (pending2 (flow-apply open2 'hold))
         (resolved2 (flow-apply pending2 'approve)))
    (should (eq (plist-get resolved2 :state) 'resolved))))

(ert-deftest hidden-flow-closed-is-terminal ()
  (let* ((m (flow-apply (flow-apply (flow-apply (flow-make) 'start) 'approve) 'close)))
    (should (flow-terminal-p (plist-get m :state)))
    (should-not (flow-can-fire-p m 'reopen))))

(ert-deftest hidden-flow-history-summary-through-reopen ()
  (let* ((m (flow-apply (flow-apply (flow-make) 'start) 'approve))
         (reopened (flow-apply m 'reopen)))
    (should (equal (flow-history-summary reopened) "new -> open -> resolved -> reopened"))))

(provide 'task2-hidden-test)
;;; task2-hidden-test.el ends here
