;;; anvil-server-metrics.el --- Metrics collection for anvil server -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Version: 0.2.1
;; URL: https://github.com/zawatton/anvil.el

;; This file is part of anvil.el (vendored from mcp-server-lib).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides metrics collection functionality for the MCP server.
;; It tracks usage statistics for all MCP operations including tools,
;; resources, and prompts.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; anvil-audit: tools-wrapped-at-registration

;; `anvil-server' requires this module, so keep the dependency one-way and
;; only declare the registration helpers we call after `anvil-server' loads.
(declare-function anvil-server-encode-handler "anvil-server" (handler))
(declare-function anvil-server-register-tools "anvil-server" (server-id specs))
(declare-function anvil-server-unregister-tools "anvil-server" (server-id specs))

;;; Data structures

(cl-defstruct
 anvil-server-metrics
 "Metrics for any MCP operation.

This structure tracks usage statistics for MCP operations including
method calls (e.g., \"initialize\", \"tools/list\") and tool-specific
calls (e.g., \"tools/call:my-tool\").

Slots:
  `calls'  - Total number of times the operation was invoked
  `errors' - Number of times the operation resulted in an error"
 (calls 0 :type integer :documentation "Total number of invocations.")
 (errors
  0
  :type integer
  :documentation "Number of failed invocations."))

;;; Global state

(defvar anvil-server-metrics--table (make-hash-table :test 'equal)
  "Metrics for all MCP operations.  Key: method or method:name.")

(defvar anvil-server-metrics--tool-token-table (make-hash-table :test 'equal)
  "Per-tool MCP payload telemetry for the current session.
Keys are tool-id strings.  Values are plists:
  :calls               - successful invocation count
  :request-chars       - cumulative request size estimate
  :response-chars      - cumulative response size
  :max-response-chars  - largest single response seen.

Request char counts use the dispatcher's in-memory arguments object and
measure `(length (format \"%S\" args))' to avoid an extra JSON encode pass.
Response char counts use the exact response text string when the dispatcher
already has it in hand; direct callers may pass a non-string object, in
which case the same `%S' approximation is used.")

(defconst anvil-server-metrics--server-id "emacs-eval"
  "Server id under which the metrics MCP tool registers.")

;;; Core functions

(defun anvil-server-metrics--get (key)
  "Get metrics for KEY, creating if needed."
  (or (gethash key anvil-server-metrics--table)
      (puthash
       key
       (make-anvil-server-metrics)
       anvil-server-metrics--table)))

(defun anvil-server-metrics--error-rate (calls errors)
  "Calculate error rate percentage.
Compute the percentage of ERRORS relative to total CALLS.
Return a float between 0.0 and 100.0."
  (if (zerop calls)
      0.0
    (* 100.0 (/ (float errors) calls))))

(defun anvil-server-metrics--bool-arg (arg default)
  "Coerce optional MCP boolean ARG using DEFAULT.
Explicit JSON false (`:false' or \"false\") becomes nil.  nil means
the caller omitted the value and falls back to DEFAULT.  Any other
explicit value is treated as true."
  (cond
   ((null arg) default)
   ((or (eq arg :false)
        (and (stringp arg)
             (string-equal (downcase arg) "false")))
    nil)
   (t t)))

(defun anvil-server-metrics--coerce-int (value default)
  "Coerce VALUE to an integer, or fall back to DEFAULT."
  (cond
   ((integerp value) value)
   ((and (stringp value)
         (string-match-p "\\`[0-9]+\\'" value))
    (string-to-number value))
   (t default)))

(defun anvil-server-metrics--measure-chars (value)
  "Measure VALUE for token telemetry without extra JSON encoding.
Strings are measured exactly with `length'.  Any non-string value is
approximated with `(length (format \"%S\" value))'."
  (length
   (if (stringp value)
       value
     (format "%S" value))))

(defun anvil-server-metrics--token-entry (tool-id)
  "Return TOOL-ID's token telemetry plist, creating if needed."
  (or (gethash tool-id anvil-server-metrics--tool-token-table)
      (let ((entry (list :calls 0
                         :request-chars 0
                         :response-chars 0
                         :max-response-chars 0)))
        (puthash tool-id entry anvil-server-metrics--tool-token-table)
        entry)))

(defun anvil-server-metrics--clear-token-counters ()
  "Clear all per-tool token telemetry for the current session."
  (clrhash anvil-server-metrics--tool-token-table))

(defun anvil-server-metrics--report-row (tool-id entry)
  "Return a report plist for TOOL-ID from token telemetry ENTRY."
  (let* ((calls (or (plist-get entry :calls) 0))
         (request-chars (or (plist-get entry :request-chars) 0))
         (response-chars (or (plist-get entry :response-chars) 0))
         (max-response-chars (or (plist-get entry :max-response-chars) 0))
         (est-tokens (/ (+ request-chars response-chars) 4)))
    (list :tool tool-id
          :calls calls
          :request-chars request-chars
          :response-chars response-chars
          :max-response-chars max-response-chars
          :est-tokens est-tokens)))

(defun anvil-server-metrics--report-sort-key (row sort-key)
  "Return ROW's numeric sort key for SORT-KEY."
  (pcase sort-key
    ('request (or (plist-get row :request-chars) 0))
    ('calls (or (plist-get row :calls) 0))
    (_ (or (plist-get row :response-chars) 0))))

(defun anvil-server-metrics--normalize-sort (sort)
  "Normalize SORT to one of `response', `request', or `calls'."
  (pcase (and (stringp sort) (downcase sort))
    ("request" 'request)
    ("calls" 'calls)
    (_ 'response)))

(defun anvil-server-metrics--token-report-data (&optional top-n sort)
  "Build the current per-tool token telemetry report.
TOP-N defaults to 10.  SORT defaults to response volume."
  (let* ((top-n* (max 0 (anvil-server-metrics--coerce-int top-n 10)))
         (sort-key (anvil-server-metrics--normalize-sort sort))
         (rows '())
         (total-calls 0)
         (total-request-chars 0)
         (total-response-chars 0))
    (maphash
     (lambda (tool-id entry)
       (let ((calls (or (plist-get entry :calls) 0))
             (request-chars (or (plist-get entry :request-chars) 0))
             (response-chars (or (plist-get entry :response-chars) 0)))
         (cl-incf total-calls calls)
         (cl-incf total-request-chars request-chars)
         (cl-incf total-response-chars response-chars)
         (push (anvil-server-metrics--report-row tool-id entry) rows)))
     anvil-server-metrics--tool-token-table)
    (setq rows
          (sort rows
                (lambda (a b)
                  (let ((ka (anvil-server-metrics--report-sort-key a sort-key))
                        (kb (anvil-server-metrics--report-sort-key b sort-key)))
                    (if (= ka kb)
                        (string-lessp (or (plist-get a :tool) "")
                                      (or (plist-get b :tool) ""))
                      (> ka kb))))))
    (list
     :session-totals
     (list :calls total-calls
           :request-chars total-request-chars
           :response-chars total-response-chars
           :est-tokens (/ (+ total-request-chars total-response-chars) 4))
     :top (cl-subseq rows 0 (min top-n* (length rows)))
     :note "token figures are chars/4 estimates")))

(defun anvil-server-metrics--track-tool-payload (tool-id request response)
  "Record successful TOOL-ID payload telemetry for REQUEST and RESPONSE.
REQUEST is measured from the dispatcher's in-memory arguments object via
`(length (format \"%S\" request))' so the tracker does not trigger a fresh
JSON encode pass.  RESPONSE is measured exactly when it is already a string;
non-string or nil values fall back to the same `%S' approximation."
  (when (stringp tool-id)
    (let* ((entry (anvil-server-metrics--token-entry tool-id))
           (request-chars (anvil-server-metrics--measure-chars request))
           (response-chars (anvil-server-metrics--measure-chars response)))
      (plist-put entry :calls (1+ (or (plist-get entry :calls) 0)))
      (plist-put entry :request-chars
                 (+ (or (plist-get entry :request-chars) 0) request-chars))
      (plist-put entry :response-chars
                 (+ (or (plist-get entry :response-chars) 0) response-chars))
      (plist-put entry :max-response-chars
                 (max (or (plist-get entry :max-response-chars) 0)
                      response-chars))
      entry)))

(defun anvil-server-metrics--track-tool-call
    (tool-name &optional is-error)
  "Track a tool call for TOOL-NAME.
If IS-ERROR is non-nil, also increment the error counter."
  (let ((metrics
         (anvil-server-metrics--get
          (format "tools/call:%s" tool-name))))
    (condition-case nil (cl-incf (anvil-server-metrics-calls metrics)) (error nil))
    (when is-error
      (condition-case nil (cl-incf (anvil-server-metrics-errors metrics)) (error nil)))))

;;; Public functions

(defun anvil-server-metrics-get (operation)
  "Get metrics object for a specific OPERATION.
Return a metrics object with zero values if OPERATION has not been tracked.

Arguments:
  OPERATION - The operation name (e.g., \"tools/list\",
              \"tools/call:my-tool\")

Use `anvil-server-metrics-calls' and `anvil-server-metrics-errors'
to access the metrics values from the returned object."
  (or (gethash operation anvil-server-metrics--table)
      (make-anvil-server-metrics)))

(defun anvil-server-metrics-summary ()
  "Return metrics summary as a string."
  (let ((total-calls 0)
        (total-errors 0))
    (maphash
     (lambda (_key metrics)
       (cl-incf total-calls (anvil-server-metrics-calls metrics))
       (cl-incf total-errors (anvil-server-metrics-errors metrics)))
     anvil-server-metrics--table)
    (format "MCP metrics: %d calls, %d errors (%.1f%% error rate)"
            total-calls total-errors
            (anvil-server-metrics--error-rate
             total-calls total-errors))))

(defun anvil-server-metrics-reset ()
  "Reset method counters and per-tool token telemetry for this session."
  (interactive)
  (clrhash anvil-server-metrics--table)
  (anvil-server-metrics--clear-token-counters))

(defun anvil-server-metrics-token-report (&optional top_n sort reset)
  "Return per-tool MCP payload telemetry for the current session.

MCP Parameters:
  top_n - (integer) Optional number of tools to include in `:top'.
          Default 10.
  sort  - Optional sort key: \"response\" (default), \"request\", or
          \"calls\".
  reset - (boolean) Optional reset flag.  Accepts the repo's usual JSON
          false encodings (`:false' or \"false\").  When true, this
          command returns the report first and only then clears both the
          generic method metrics and the per-tool token counters.

Returns a plist:
  :session-totals - (:calls N :request-chars N :response-chars N
                     :est-tokens N)
  :top            - list of per-tool plists sorted by the requested key
  :note           - \"token figures are chars/4 estimates\""
  (interactive)
  (let ((report (anvil-server-metrics--token-report-data top_n sort)))
    (when (anvil-server-metrics--bool-arg reset nil)
      (anvil-server-metrics-reset))
    report))

(defun anvil-server-metrics--tool-specs ()
  "Return the spec list consumed by `anvil-server-register-tools'."
  `((,(anvil-server-encode-handler #'anvil-server-metrics-token-report)
     :id "metrics-token-report"
     :intent (observability admin)
     :layer workflow
     :description
     "Report per-tool MCP payload telemetry for this session.  Returns
session totals plus the top-N tools sorted by response volume
(default), request volume, or call count.  Request counts use the
dispatcher's in-memory arguments object measured via `%S'; response
counts use the exact transport string when the dispatcher already has
it.  Optional `reset' is report-then-reset.")))

(defun anvil-server-metrics--register-tools ()
  "Register the metrics MCP tools on the emacs-eval server."
  (anvil-server-register-tools anvil-server-metrics--server-id
                               (anvil-server-metrics--tool-specs)))

(defun anvil-server-metrics--unregister-tools ()
  "Unregister the metrics MCP tools from the emacs-eval server."
  (anvil-server-unregister-tools anvil-server-metrics--server-id
                                 (anvil-server-metrics--tool-specs)))

(with-eval-after-load 'anvil-server
  (anvil-server-metrics--register-tools))

(provide 'anvil-server-metrics)

;; Local Variables:
;; package-lint-main-file: "anvil-server.el"
;; End:

;;; anvil-server-metrics.el ends here
