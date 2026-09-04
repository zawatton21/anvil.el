;;; tt.el --- public API for text tables -*- lexical-binding: t; -*-

;;; Code:

(require 'tt-draw)

(defun tt-render (columns rows)
  "Render COLUMNS and ROWS into a newline-joined text table."
  (mapconcat #'identity (tt-render-lines columns rows) "\n"))

(provide 'tt)
;;; tt.el ends here
