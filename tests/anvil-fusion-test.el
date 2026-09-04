;;; anvil-fusion-test.el --- ERT for anvil-fusion Phase 1 -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure-layer tests for the Fusion judge prompt builder.  These do NOT
;; load anvil-orchestrator: the prompt layer is self-contained, and the
;; one full-fidelity path that calls into orchestrator is exercised with
;; a cl-letf stub.

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'anvil-fusion)

(defconst anvil-fusion-test--candidates
  '((:id "t1" :provider claude :status done
         :summary "地絡継電器は OCGR ではなく DGR を使う。")
    (:id "t2" :provider gemini :status done
         :summary "方向性を持たせるため DGR(地絡方向継電器) が適切。"))
  "Two healthy candidate slim plists.")

(defconst anvil-fusion-test--dedup-seed
  (let ((shared-head
         (mapconcat (lambda (n)
                      (format "Shared context line %02d: same grounding." n))
                    (number-sequence 1 24)
                    "\n"))
        (shared-tail
         (mapconcat (lambda (n)
                      (format "Shared tail line %02d: same conclusion." n))
                    (number-sequence 25 48)
                    "\n")))
    (mapconcat #'identity
               (list shared-head
                     "Keep this line."
                     shared-tail)
               "\n"))
  "Base text used by dedup rendering tests.")

(defconst anvil-fusion-test--dedup-near
  (replace-regexp-in-string
   "Keep this line\\."
   "Keep this line!"
   anvil-fusion-test--dedup-seed
   t t)
  "Near-duplicate text used by dedup rendering tests.")

(defconst anvil-fusion-test--dedup-different
  "A materially different answer that should remain a singleton cluster."
  "Dissimilar text used by dedup rendering tests.")

;;;; --- candidate text ------------------------------------------------------

(ert-deftest anvil-fusion-test-candidate-text-summary ()
  "Summary fidelity returns the :summary verbatim."
  (should (equal "地絡継電器は OCGR ではなく DGR を使う。"
                 (anvil-fusion--candidate-text
                  (car anvil-fusion-test--candidates) 'summary))))

(ert-deftest anvil-fusion-test-candidate-text-error-fallback ()
  "Empty summary falls back to :error."
  (should (equal "boom"
                 (anvil-fusion--candidate-text
                  '(:id "x" :provider claude :status failed
                        :summary "" :error "boom")
                  'summary))))

(ert-deftest anvil-fusion-test-candidate-text-noinput-fallback ()
  "No summary and no error yields the sentinel."
  (should (equal "(no output)"
                 (anvil-fusion--candidate-text
                  '(:id "x" :provider claude :status failed) 'summary))))

(ert-deftest anvil-fusion-test-candidate-text-full-dispatch ()
  "Full fidelity calls extract-result and uses its untruncated answer."
  (cl-letf (((symbol-function 'anvil-orchestrator-extract-result)
             (lambda (id full)
               (should (equal id "t1"))
               (should full)
               (list :summary "FULL-UNTRUNCATED-ANSWER"))))
    (should (equal "FULL-UNTRUNCATED-ANSWER"
                   (anvil-fusion--candidate-text
                    (car anvil-fusion-test--candidates) 'full)))))

(ert-deftest anvil-fusion-test-candidate-text-full-degrades ()
  "Full fidelity degrades to :summary when extract-result errors."
  (cl-letf (((symbol-function 'anvil-orchestrator-extract-result)
             (lambda (_id _full) (error "no stdout"))))
    (should (equal "地絡継電器は OCGR ではなく DGR を使う。"
                   (anvil-fusion--candidate-text
                    (car anvil-fusion-test--candidates) 'full)))))

;;;; --- candidate formatting ------------------------------------------------

(ert-deftest anvil-fusion-test-format-candidates-numbered ()
  "Candidates are numbered and carry provider + status."
  (let ((block (anvil-fusion--format-candidates
                anvil-fusion-test--candidates 'summary)))
    (should (string-match-p "1\\. \\[provider: claude, status: done\\]" block))
    (should (string-match-p "2\\. \\[provider: gemini, status: done\\]" block))
    (should (string-match-p "DGR" block))))

(ert-deftest anvil-fusion-test-format-candidates-empty ()
  "Empty candidate list does not crash."
  (should (equal "(no candidates)"
                 (anvil-fusion--format-candidates nil 'summary))))

(ert-deftest anvil-fusion-test-format-candidates-default-off-byte-identical ()
  "Default dedup setting preserves the historical candidate block bytes."
  (let ((block (anvil-fusion--format-candidates
                anvil-fusion-test--candidates 'summary)))
    (should
     (equal
      block
      (concat
       "1. [provider: claude, status: done]\n"
       "地絡継電器は OCGR ではなく DGR を使う。\n\n"
       "2. [provider: gemini, status: done]\n"
       "方向性を持たせるため DGR(地絡方向継電器) が適切。")))))

(ert-deftest anvil-fusion-test-dedup-clusters-greedy-order-preserving ()
  "Near-duplicates cluster under the first seed, preserving input order."
  (let* ((clusters (anvil-fusion--dedup-clusters
                    (list (cons "A" anvil-fusion-test--dedup-seed)
                          (cons "B" anvil-fusion-test--dedup-near)
                          (cons "C" anvil-fusion-test--dedup-different))
                    0.9)))
    (should (= (length clusters) 2))
    (should (equal (plist-get (car clusters) :seed)
                   (cons "A" anvil-fusion-test--dedup-seed)))
    (should (equal (plist-get (car clusters) :members)
                   (list (cons "B" anvil-fusion-test--dedup-near))))
    (should (equal (plist-get (cadr clusters) :seed)
                   (cons "C" anvil-fusion-test--dedup-different)))
    (should (equal (plist-get (cadr clusters) :members) nil))))

(ert-deftest anvil-fusion-test-format-candidates-deduped-rendering ()
  "Dedup rendering keeps the seed full, shows delta notes, and preserves singletons."
  (let* ((candidates
          `((:provider claude :status done :summary ,anvil-fusion-test--dedup-seed)
            (:provider gemini :status done :summary ,anvil-fusion-test--dedup-near)
            (:provider mistral :status done :summary ,anvil-fusion-test--dedup-seed)
            (:provider qwen :status done :summary ,anvil-fusion-test--dedup-different)))
         (block (let ((anvil-fusion-judge-dedup-threshold 0.9))
                  (anvil-fusion--format-candidates-deduped candidates 'summary))))
    (should (string-match-p
             (regexp-quote
              (format "1. [provider: claude, status: done]\n%s"
                      anvil-fusion-test--dedup-seed))
             block))
    (should (string-match-p
             (regexp-quote
              "### 2. [provider: gemini, status: done]\n（1. [provider: claude, status: done] とほぼ同一の回答。差分のみ:）")
             block))
    (should (string-match-p "^\\+Keep this line!$" block))
    (should (string-match-p
             (regexp-quote
              "### 3. [provider: mistral, status: done]\n（1. [provider: claude, status: done] とほぼ同一の回答。差分なし・実質同一）")
             block))
    (should (string-match-p
             (regexp-quote
              (format "4. [provider: qwen, status: done]\n%s"
                      anvil-fusion-test--dedup-different))
             block))))

(ert-deftest anvil-fusion-test-format-candidates-deduped-diff-failure-falls-back ()
  "Diff helper errors degrade to full rendering without signaling."
  (let* ((candidates
          `((:provider claude :status done :summary ,anvil-fusion-test--dedup-seed)
            (:provider gemini :status done :summary ,anvil-fusion-test--dedup-near)))
         (block
          (cl-letf (((symbol-function 'anvil-fusion--judge-dedup-diff)
                     (lambda (&rest _) (error "boom"))))
            (anvil-fusion--format-candidates-deduped candidates 'summary))))
    (should (string-match-p
             (regexp-quote
              (format "2. [provider: gemini, status: done]\n%s"
                      anvil-fusion-test--dedup-near))
             block))
    (should-not (string-match-p "差分のみ" block))))

(ert-deftest anvil-fusion-test-format-candidates-deduped-size-cut ()
  "A one-line delta shrinks the judge block substantially."
  (let* ((lines (mapcar (lambda (n)
                          (format "Common line %03d: stable content." n))
                        (number-sequence 1 200)))
         (seed (mapconcat #'identity lines "\n"))
         (near (mapconcat
                #'identity
                (mapcar (lambda (line)
                          (if (equal line "Common line 030: stable content.")
                              "Common line 030: stable content!"
                            line))
                        lines)
                "\n"))
         (candidates `((:provider claude :status done :summary ,seed)
                       (:provider gemini :status done :summary ,near)))
         (full (let ((anvil-fusion-judge-dedup nil))
                 (anvil-fusion--format-candidates candidates 'summary)))
         (dedup (let ((anvil-fusion-judge-dedup t)
                      (anvil-fusion-judge-dedup-threshold 0.9))
                  (anvil-fusion--format-candidates candidates 'summary))))
    (should (< (length dedup) (* 0.6 (length full))))))

;;;; --- judge prompt --------------------------------------------------------

(ert-deftest anvil-fusion-test-build-prompt-structure ()
  "Prompt embeds the question and all five Fusion analysis axes."
  (let ((p (anvil-fusion-build-judge-prompt
            "地絡保護に使う継電器は？" anvil-fusion-test--candidates
            :fidelity 'summary)))
    (should (string-match-p "地絡保護に使う継電器は？" p))
    (dolist (axis '("合意点" "矛盾点" "部分カバー" "独自洞察" "見落とし" "最終回答"))
      (should (string-match-p (regexp-quote axis) p)))
    ;; both candidate answers present
    (should (string-match-p "OCGR ではなく DGR" p))
    (should (string-match-p "地絡方向継電器" p))))

(ert-deftest anvil-fusion-test-build-prompt-extra ()
  "Extra instruction is appended after the rendered prompt."
  (let ((p (anvil-fusion-build-judge-prompt
            "Q" anvil-fusion-test--candidates
            :extra "200字以内で答えること。")))
    (should (string-suffix-p "200字以内で答えること。" p))))

(ert-deftest anvil-fusion-test-build-prompt-extra-empty-ignored ()
  "Empty :extra is ignored (no trailing blank block)."
  (let ((p (anvil-fusion-build-judge-prompt
            "Q" anvil-fusion-test--candidates :extra "")))
    (should-not (string-suffix-p "\n\n" p))))

(ert-deftest anvil-fusion-test-build-prompt-template-override ()
  "A custom two-%s template fully overrides the default."
  (let ((p (anvil-fusion-build-judge-prompt
            "QQ" anvil-fusion-test--candidates
            :template "Q=%s | C=%s")))
    (should (string-prefix-p "Q=QQ | C=" p))
    (should-not (string-match-p "合意点" p))))

(ert-deftest anvil-fusion-test-build-prompt-nil-original ()
  "Nil original prompt renders as empty, no crash."
  (let ((p (anvil-fusion-build-judge-prompt nil nil)))
    (should (stringp p))
    (should (string-match-p "(no candidates)" p))))

(ert-deftest anvil-fusion-test-build-debate-prompt-structure ()
  "Debate prompt includes question, own answer, deduped others, claims, and the refuted-claim ban."
  (let* ((others
          `((:provider claude :status done :summary ,anvil-fusion-test--dedup-seed)
            (:provider gemini :status done :summary ,anvil-fusion-test--dedup-near)
            (:provider qwen :status done :summary ,anvil-fusion-test--dedup-different)))
         (prompt
          (let ((anvil-fusion-judge-dedup-threshold 0.9))
            (anvil-fusion-build-debate-prompt
             "原問テスト"
             "自分の前回回答"
             others
             "1. 主張: DGR を使う\n   VERDICT: refuted"))))
    (should (string-match-p "# 原問\n原問テスト" prompt))
    (should (string-match-p "# あなたの前回の回答\n自分の前回回答" prompt))
    (should (string-match-p "# 他の回答者の回答（要点）" prompt))
    (should (string-match-p "差分のみ" prompt))
    (should (string-match-p "VERDICT: refuted" prompt))
    (should (string-match-p "REFUTED" prompt))))

(ert-deftest anvil-fusion-test-build-debate-prompt-empty-sections-safe ()
  "Empty others/claims render as explicit none markers without crashing."
  (let ((prompt (anvil-fusion-build-debate-prompt "Q" nil nil nil)))
    (should (string-match-p "# あなたの前回の回答\n" prompt))
    (should (string-match-p "# 他の回答者の回答（要点）\n(なし)" prompt))
    (should (string-match-p "# 検証済み主張表\n(なし)" prompt))))

(provide 'anvil-fusion-test)
;;; anvil-fusion-test.el ends here
