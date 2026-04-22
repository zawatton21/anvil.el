;;; run.el --- Phase 3b benchmark runner  -*- lexical-binding: t; -*-

;; Runs every case in `anvil-bench-31-3b-cases' through
;; `anvil-sexp-cst-repair', classifies the outcome against the
;; expected label, and prints an org-flavored report to stdout.
;;
;; Usage from repo root:
;;   emacs --batch -L . \
;;     -l benchmarks/doc31-phase3b/dataset.el \
;;     -l benchmarks/doc31-phase3b/run.el
;;
;; Exit code is 0 when the run completed — the benchmark does *not*
;; fail on an unexpected outcome, because the dataset is the source of
;; truth for what "expected" means and dataset drift is an interesting
;; signal on its own.  Tally columns make drift obvious.

;;; Code:

(require 'json)
;; `emacs --batch' does not autoload treesit; the `grammar-usable-p'
;; check in anvil-sexp-cst-repair returns nil without this require.
(require 'treesit)
(require 'anvil-sexp-cst)

(defun anvil-bench-31-3b--run-case (case)
  "Execute CASE through `anvil-sexp-cst-repair', return result plist.
Writes `:input' to a temp file, calls repair, parses JSON output,
classifies `:actual-kind' and whether `:matches-expected'."
  (let* ((input (plist-get case :input))
         (tmp (make-temp-file "anvil-bench-31-3b-" nil ".el" input))
         (raw (anvil-sexp-cst-repair tmp))
         (obj (json-parse-string raw :object-type 'alist
                                 :array-type 'list
                                 :null-object nil
                                 :false-object nil))
         (err (alist-get 'error obj))
         actual-kind actual-added)
    (ignore-errors (delete-file tmp))
    (cond
     (err
      (setq actual-kind (or (alist-get 'kind err) "error")))
     (t
      (let ((fix (alist-get 'fix obj)))
        (setq actual-kind (alist-get 'kind fix))
        (setq actual-added (alist-get 'added fix)))))
    (list :id (plist-get case :id)
          :category (plist-get case :category)
          :description (plist-get case :description)
          :expected-kind (plist-get case :expected-kind)
          :expected-added (plist-get case :expected-added)
          :actual-kind actual-kind
          :actual-added actual-added
          :matches-expected
          (and (equal actual-kind (plist-get case :expected-kind))
               (or (null (plist-get case :expected-added))
                   (equal actual-added (plist-get case :expected-added)))))))

(defun anvil-bench-31-3b--tally (results)
  "Aggregate RESULTS by category, return alist of (CAT . plist)."
  (let (tallies)
    (dolist (r results)
      (let* ((cat (plist-get r :category))
             (cell (assoc cat tallies))
             (p (if cell (cdr cell) (list :n 0 :matches 0 :repaired 0 :failed 0))))
        (cl-incf (plist-get p :n))
        (when (plist-get r :matches-expected)
          (cl-incf (plist-get p :matches)))
        (cond
         ((member (plist-get r :actual-kind)
                  '("close-paren-added" "open-paren-prepended" "noop"
                    "unterminated-string-closed"
                    "unterminated-string-and-close-paren-added"
                    "unterminated-string-and-open-paren-prepended"))
          (cl-incf (plist-get p :repaired)))
         (t
          (cl-incf (plist-get p :failed))))
        (if cell (setcdr cell p) (push (cons cat p) tallies))))
    (nreverse tallies)))

(defun anvil-bench-31-3b--print-report (results tallies)
  "Print an org-flavored report to stdout."
  (princ "#+title: Doc 31 Phase 3b — LLM eval (close-paren repair ROI)\n")
  (princ (format "#+date: %s\n"
                 (format-time-string "%Y-%m-%d")))
  (princ "#+author: zawatton + Claude Opus 4.7\n\n")
  (princ "* Run context\n\n")
  (princ (format "- Emacs: %s\n" emacs-version))
  (princ (format "- treesit-elisp grammar ready: %s\n"
                 (if (and (fboundp 'treesit-ready-p)
                          (ignore-errors (treesit-ready-p 'elisp t)))
                     "YES"
                   "NO")))
  (princ (format "- Cases: %d\n" (length results)))
  (princ "- Dataset: [[file:dataset.el][dataset.el]]\n")
  (princ "- Runner: [[file:run.el][run.el]]\n\n")
  (princ "* Aggregate by category\n\n")
  (princ "| Category         | N | Expected-match | Actually repaired | Failed |\n")
  (princ "|------------------+---+----------------+-------------------+--------|\n")
  (dolist (entry tallies)
    (let* ((cat (car entry))
           (p (cdr entry))
           (n (plist-get p :n))
           (m (plist-get p :matches))
           (r (plist-get p :repaired))
           (f (plist-get p :failed)))
      (princ (format "| %-16s | %d | %d/%d (%.0f%%)     | %d/%d (%.0f%%)         | %d/%d  |\n"
                     cat n
                     m n (* 100.0 (/ (float m) n))
                     r n (* 100.0 (/ (float r) n))
                     f n))))
  (let* ((total (length results))
         (matches (cl-count-if (lambda (r) (plist-get r :matches-expected))
                               results))
         (repaired-or-noop
          (cl-count-if
           (lambda (r)
             (member (plist-get r :actual-kind)
                     '("close-paren-added" "open-paren-prepended" "noop"
                       "unterminated-string-closed"
                       "unterminated-string-and-close-paren-added"
                       "unterminated-string-and-open-paren-prepended")))
           results))
         ;; "Retry rate without repair" = how many cases would have
         ;; required the LLM to retry (any case not already clean).
         ;; Our dataset excludes clean-source cases by construction
         ;; except the one `noop' fixture, so retry-without is roughly
         ;; total - (noop cases).
         (noop-count
          (cl-count-if
           (lambda (r) (equal (plist-get r :actual-kind) "noop"))
           results))
         (retry-without (- total noop-count))
         (retry-with (- total repaired-or-noop))
         (reduction (if (zerop retry-without) 0
                      (* 100.0 (/ (float (- retry-without retry-with))
                                  retry-without)))))
    (princ (format "\n*Total expected-match:* %d/%d (%.0f%%)\n"
                   matches total
                   (* 100.0 (/ (float matches) total))))
    (princ (format "*Repair success (close/open/noop):* %d/%d (%.0f%%)\n"
                   repaired-or-noop total
                   (* 100.0 (/ (float repaired-or-noop) total))))
    (princ (format "*Estimated retry-rate reduction:* %.0f%% → %.0f%% (%.0f%% of broken cases rescued)\n\n"
                   (* 100.0 (/ (float retry-without) total))
                   (* 100.0 (/ (float retry-with) total))
                   reduction)))
  (princ "* Per-case detail\n\n")
  (princ "| ID | Category         | Expected kind       | Actual kind         | Exp. added | Act. added | Match | Description |\n")
  (princ "|----+------------------+---------------------+---------------------+------------+------------+-------+-------------|\n")
  (dolist (r results)
    (princ (format "| %2d | %-16s | %-19s | %-19s | %10s | %10s | %s  | %s |\n"
                   (plist-get r :id)
                   (plist-get r :category)
                   (plist-get r :expected-kind)
                   (or (plist-get r :actual-kind) "?")
                   (if (plist-get r :expected-added)
                       (number-to-string (plist-get r :expected-added))
                     "—")
                   (if (plist-get r :actual-added)
                       (number-to-string (plist-get r :actual-added))
                     "—")
                   (if (plist-get r :matches-expected) "✓" "✗")
                   (plist-get r :description))))
  (princ "\n* Interpretation\n\n")
  (princ "`Repair success' means `sexp-cst-repair' returned a\n")
  (princ "`repair-result' envelope (not a typed error).  For the\n")
  (princ "`unfixable' category we *expect* the tool to refuse — so\n")
  (princ "a high `expected-match' there is the right outcome, not a\n")
  (princ "high `repaired' rate.\n\n")
  (princ "`Retry-rate reduction' models the ROI thesis: without the\n")
  (princ "repair tool, every broken case would have forced the LLM\n")
  (princ "to retry; with the tool, only the residual unfixable cases\n")
  (princ "need retry.  That gap is the token / latency saving Phase\n")
  (princ "3a buys the pipeline.\n"))

(let* ((results (mapcar #'anvil-bench-31-3b--run-case
                        anvil-bench-31-3b-cases))
       (tallies (anvil-bench-31-3b--tally results)))
  (anvil-bench-31-3b--print-report results tallies))

;;; run.el ends here
