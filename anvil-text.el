;;; anvil-text.el --- Text processing for anvil -*- lexical-binding: t; -*-

;; Author: zawatton21
;; Keywords: tools, mcp, claude, text

;;; Commentary:

;; anvil-helpers の姉妹ファイル。Claude Code が emacs-eval 経由で
;; テキスト/org/markdown ファイルからリンク等の情報を抽出する helper。
;;
;; 主要動機:
;;   telegram-url-batch / web-summarizer skill が同じような URL 抽出を
;;   毎回 ad-hoc に書いていたのを統一する。

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-file)

;;;; --- internal -----------------------------------------------------------

(defconst anvil-text--plain-url-regex
  "https?://[^][ \t\n\r<>()'\"`{}|\\^]+"
  "Regex matching plain URLs (no surrounding markup).")

(defconst anvil-text--org-link-regex
  "\\[\\[\\(https?://[^]]+\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]"
  "Regex matching org-mode link `[[url][desc]]' or `[[url]]'.")

(defconst anvil-text--md-link-regex
  "\\[\\([^]]*\\)\\](\\(https?://[^)]+\\))"
  "Regex matching markdown link `[desc](url)'.")

(defun anvil-text--detect-type (path)
  "Guess link type from PATH extension."
  (let ((ext (and path (file-name-extension path))))
    (cond
     ((member ext '("org")) 'org)
     ((member ext '("md" "markdown")) 'markdown)
     (t 'plain))))

(defun anvil-text--scan-buffer (type)
  "Scan current buffer for links of TYPE.
Returns a list of plists (:url :description :line :type)."
  (let (results)
    (save-excursion
      (cond
       ((eq type 'org)
        ;; First pass: org-style [[url][desc]]
        (goto-char (point-min))
        (while (re-search-forward anvil-text--org-link-regex nil t)
          (push (list :url (match-string-no-properties 1)
                      :description (match-string-no-properties 2)
                      :line (line-number-at-pos (match-beginning 0))
                      :type 'org)
                results))
        ;; Second pass: plain URLs not already captured by org links
        (goto-char (point-min))
        (while (re-search-forward anvil-text--plain-url-regex nil t)
          (let ((url (match-string-no-properties 0))
                (start (match-beginning 0)))
            ;; Skip if inside [[...]] (the org link parser already got it)
            (unless (save-excursion
                      (goto-char start)
                      (let ((bol (line-beginning-position)))
                        (and (re-search-backward "\\[\\[" bol t)
                             (not (re-search-forward "\\]\\]"
                                                     start t)))))
              (push (list :url url
                          :description nil
                          :line (line-number-at-pos start)
                          :type 'plain)
                    results)))))
       ((eq type 'markdown)
        (goto-char (point-min))
        (while (re-search-forward anvil-text--md-link-regex nil t)
          (push (list :url (match-string-no-properties 2)
                      :description (match-string-no-properties 1)
                      :line (line-number-at-pos (match-beginning 0))
                      :type 'markdown)
                results))
        (goto-char (point-min))
        (while (re-search-forward anvil-text--plain-url-regex nil t)
          (push (list :url (match-string-no-properties 0)
                      :description nil
                      :line (line-number-at-pos (match-beginning 0))
                      :type 'plain)
                results)))
       (t
        (goto-char (point-min))
        (while (re-search-forward anvil-text--plain-url-regex nil t)
          (push (list :url (match-string-no-properties 0)
                      :description nil
                      :line (line-number-at-pos (match-beginning 0))
                      :type 'plain)
                results)))))
    ;; Sort by line ascending
    (sort (nreverse results)
          (lambda (a b) (< (plist-get a :line) (plist-get b :line))))))

;;;; --- public API ---------------------------------------------------------

(defun anvil-text-extract-links (path &optional type)
  "Extract URLs from text file PATH.

TYPE can be:
  'org       — extracts org `[[url][desc]]' AND plain URLs
  'markdown  — extracts md `[desc](url)' AND plain URLs
  'plain     — only plain URLs
  'auto      — detect from file extension (default)

Returned list is sorted by line ascending. Each element:
  (:url STR :description STR-or-nil :line N :type SYM)

For deduplication by URL, use `cl-delete-duplicates' on the result."
  (let* ((abs (anvil--prepare-path path))
         (resolved-type (if (or (null type) (eq type 'auto))
                            (anvil-text--detect-type abs)
                          type)))
    (with-temp-buffer
      (anvil--insert-file abs)
      (anvil-text--scan-buffer resolved-type))))

;;;; --- discoverability ----------------------------------------------------

(defun anvil-text-helpers-list ()
  "Return a list of all anvil-text* user-facing function names."
  (let (results)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (let ((n (symbol-name sym)))
                    (and (string-prefix-p "anvil-text" n)
                         (not (string-match-p "--" n)))))
         (push sym results))))
    (sort results (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(provide 'anvil-text)
;;; anvil-text.el ends here
