;;; flow.el --- tiny ticket workflow state machine -*- lexical-binding: t; -*-

;;; Commentary:
;; A minimal finite-state machine for a support-ticket workflow.
;; States: new -> open -> pending -> resolved -> closed, with a
;; `reopened' state reachable from `resolved'.
;;
;; Transition table (state . event) -> next-state:
;;   (new      . start)   -> open
;;   (open     . hold)    -> pending
;;   (pending  . resume)  -> open
;;   (open     . approve) -> resolved
;;   (pending  . approve) -> resolved
;;   (resolved . close)   -> closed
;;   (resolved . reopen)  -> reopened
;;   (reopened . start)   -> open
;;
;; `flow-apply' advances a machine plist (:state STATE :history LIST)
;; by one event, signaling `flow-invalid-transition' when STATE/EVENT
;; has no entry in the table.

;;; Code:

(define-error 'flow-invalid-transition "Invalid flow transition")

(defvar flow-transitions
  '(((new      . start)   . open)
    ((open     . hold)    . pending)
    ((pending  . resume)  . open)
    ((open     . approve) . resolved)
    ((pending  . approve) . closed)
    ((resolved . close)   . closed)
    ((reopened . start)   . open))
  "Alist mapping (STATE . EVENT) to the next STATE.")

(defun flow-transition (state event)
  "Return the next state for STATE/EVENT, or nil if not defined."
  (cdr (assoc (cons state event) flow-transitions)))

(defun flow-terminal-p (state)
  "Return non-nil if STATE has no outgoing transitions."
  (null (seq-find (lambda (entry) (eq (caar entry) state)) flow-transitions)))

(defun flow-make (&optional state)
  "Create a new flow machine plist starting at STATE (default `new')."
  (list :state (or state 'new) :history nil))

(defun flow-apply (machine event)
  "Advance MACHINE by EVENT, returning a new machine plist.
Signals `flow-invalid-transition' if EVENT is not valid from the
machine's current state."
  (let* ((state (plist-get machine :state))
         (next (flow-transition state event)))
    (unless next
      (signal 'flow-invalid-transition (list state event)))
    (list :state next
          :history (append (plist-get machine :history)
                            (list (list state event next))))))

(provide 'flow)
;;; flow.el ends here
