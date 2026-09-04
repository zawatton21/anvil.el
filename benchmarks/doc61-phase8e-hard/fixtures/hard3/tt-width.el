;;; tt-width.el --- width helpers for text tables -*- lexical-binding: t; -*-

;;; Code:

(defun tt-wide-char-p (ch)
  "Return non-nil if CH should occupy two display columns."
  (or (<= #x3040 ch #x30ff)
      (<= #x4e00 ch #x9fff)
      (<= #xff00 ch #xffef)))

(defun tt-string-width (s)
  "Return the display width of S.
All layout and border code is supposed to route through this helper."
  (length s))

(provide 'tt-width)
;;; tt-width.el ends here
