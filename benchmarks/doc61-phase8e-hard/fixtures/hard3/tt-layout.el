;;; tt-layout.el --- layout calculations for text tables -*- lexical-binding: t; -*-

;;; Code:

(require 'tt-width)

(defun tt--cell-string (row key)
  "Return ROW's KEY value as a string."
  (format "%s" (alist-get key row)))

(defun tt-compute-widths (columns rows)
  "Compute per-column widths for COLUMNS and ROWS."
  (mapcar (lambda (column)
            (let* ((key (plist-get column :key))
                   (title (plist-get column :title))
                   (width (length title)))
              (dolist (row rows)
                (setq width (max width (length (tt--cell-string row key)))))
              width))
          columns))

(provide 'tt-layout)
;;; tt-layout.el ends here
