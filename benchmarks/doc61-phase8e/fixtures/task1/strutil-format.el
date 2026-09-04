;;; strutil-format.el --- display-width aware padding -*- lexical-binding: t; -*-

;;; Commentary:
;; Layered on top of strutil-core: computes a "display width" for a
;; string where common full-width (CJK) characters occupy two
;; columns instead of one, and pads a string out to a target display
;; width for aligned plain-text tables.

;;; Code:

(require 'strutil-core)

(defun strutil-wide-char-p (ch)
  "Return non-nil if character CH should occupy two display columns.
Covers the common CJK ranges: Hiragana, Katakana, CJK Unified
Ideographs, and halfwidth/fullwidth forms."
  (or (<= #x3040 ch #x30ff)   ; Hiragana + Katakana
      (<= #x4e00 ch #x9fff)   ; CJK Unified Ideographs
      (<= #xff00 ch #xffef))) ; Halfwidth/Fullwidth Forms

(defun strutil-display-width (s)
  "Return the display width of S, counting wide characters as 2 columns."
  (length s))

(defun strutil-pad-display-width (s width)
  "Pad S on the right with spaces until it reaches display WIDTH.
Uses `strutil-display-width' to measure S; if S is already at or
past WIDTH, it is returned unchanged."
  (let ((gap (- width (strutil-display-width s))))
    (if (> gap 0)
        (concat s (make-string gap ?\s))
      s)))

(provide 'strutil-format)
;;; strutil-format.el ends here
