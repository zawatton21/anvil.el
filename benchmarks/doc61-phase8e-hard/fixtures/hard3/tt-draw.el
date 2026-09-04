;;; tt-draw.el --- draw text tables -*- lexical-binding: t; -*-

;;; Code:

(require 'tt-layout)

(defun tt-pad-cell (text width)
  "Pad TEXT on the right to WIDTH."
  (let ((gap (- width (length text))))
    (if (> gap 0)
        (concat text (make-string gap ?\s))
      text)))

(defun tt--border-line (widths)
  "Return a +----+ border line for WIDTHS."
  (concat "+"
          (mapconcat (lambda (width)
                       (make-string (+ width 1) ?-))
                     widths
                     "+")
          "+"))

(defun tt--render-row (columns widths row)
  "Render one ROW for COLUMNS using WIDTHS."
  (concat "| "
          (mapconcat (lambda (pair)
                       (pcase-let ((`(,column . ,width) pair))
                         (tt-pad-cell
                          (tt--cell-string row (plist-get column :key))
                          width)))
                     (cl-mapcar #'cons columns widths)
                     " | ")
          " |"))

(defun tt-render-lines (columns rows)
  "Render table COLUMNS and ROWS as a list of lines."
  (let* ((widths (tt-compute-widths columns rows))
         (border (tt--border-line widths))
         (header-row
          (mapcar (lambda (column)
                    (cons (plist-get column :key)
                          (plist-get column :title)))
                  columns)))
    (append (list border
                  (tt--render-row columns widths header-row)
                  border)
            (mapcar (lambda (row) (tt--render-row columns widths row)) rows)
            (list border))))

(provide 'tt-draw)
;;; tt-draw.el ends here
