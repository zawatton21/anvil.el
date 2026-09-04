;;; strutil-core.el --- small string utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Tiny string-handling helpers: trimming, ellipsis truncation, and a
;; quoted-CSV line splitter.

;;; Code:

(defun strutil-trim (s)
  "Trim leading and trailing whitespace from S."
  (let ((s (replace-regexp-in-string "\\`[ \t\n\r]+" "" s)))
    (replace-regexp-in-string "[ \t\n\r]+\\'" "" s)))

(defun strutil-truncate (s n)
  "Truncate S to at most N display characters, appending \"...\" if cut.
If S is longer than N characters, the returned string is exactly
N characters long, including the trailing ellipsis.  If S already
fits within N characters, it is returned unchanged."
  (if (<= (length s) n)
      s
    (concat (substring s 0 n) "...")))

(defun strutil-split-csv (line)
  "Split LINE on commas, honoring double-quoted fields.
A field wrapped in double quotes may itself contain commas; a
doubled quote (\"\") inside a quoted field is unescaped to a single
literal quote."
  (let ((fields nil)
        (cur "")
        (in-quotes nil)
        (i 0)
        (len (length line)))
    (while (< i len)
      (let ((c (aref line i)))
        (cond
         ((and in-quotes (eq c ?\") (< (1+ i) len) (eq (aref line (1+ i)) ?\"))
          (setq cur (concat cur "\""))
          (setq i (+ i 2)))
         ((eq c ?\")
          (setq in-quotes (not in-quotes))
          (setq i (1+ i)))
         ((and (eq c ?,) (not in-quotes))
          (push cur fields)
          (setq cur "")
          (setq i (1+ i)))
         (t
          (setq cur (concat cur (char-to-string c)))
          (setq i (1+ i))))))
    (push cur fields)
    (nreverse fields)))

(provide 'strutil-core)
;;; strutil-core.el ends here
