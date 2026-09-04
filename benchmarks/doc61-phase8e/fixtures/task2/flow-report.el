;;; flow-report.el --- summarize a flow machine's history -*- lexical-binding: t; -*-

;;; Commentary:
;; Small reporting helpers layered on top of `flow.el': a
;; human-readable history summary and a non-signaling event check.

;;; Code:

(require 'flow)

(defun flow-can-fire-p (machine event)
  "Return non-nil if EVENT is a valid transition from MACHINE's state."
  (and (flow-transition (plist-get machine :state) event) t))

(defun flow-history-summary (machine)
  "Return a human-readable \" -> \"-joined summary of MACHINE's path.
Includes the machine's current state as the final element."
  (let* ((history (plist-get machine :history))
         (states (if history
                     (cons (car (car history))
                           (mapcar (lambda (entry) (nth 2 entry)) history))
                   (list (plist-get machine :state)))))
    (mapconcat #'symbol-name states " -> ")))

(provide 'flow-report)
;;; flow-report.el ends here
