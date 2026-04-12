;;; anvil-xlsx.el --- Excel operations for anvil -*- lexical-binding: t; -*-

;; Author: zawatton21
;; Keywords: tools, mcp, claude, xlsx, openpyxl

;;; Commentary:

;; anvil-helpers の姉妹ファイル。Claude Code が emacs-eval 経由で
;; xlsx ファイルを 1 call で読むための openpyxl wrapper。
;;
;; 設計方針:
;;   - python -c インライン script + JSON 経由で値を授受
;;   - utf-8 強制 (Windows 既定 cp932 罠を回避)
;;   - Windows path は forward slash 化 (shell quoting の二重 escape 罠回避)
;;   - openpyxl は read_only=True / data_only=True で開く
;;     (高速化 + formula は計算済値で取得)
;;   - JSON null → nil に変換 (空セル = nil の Lisp 慣習)
;;   - datetime/date はセル → ISO 文字列で返す
;;
;; 必須前提:
;;   - python が exec-path にある
;;   - python から openpyxl が import できる
;;     (無ければ pip install openpyxl --break-system-packages)
;;
;; Public API:
;;   (anvil-xlsx-sheets PATH)
;;   (anvil-xlsx-info PATH)
;;   (anvil-xlsx-read-cells PATH SHEET &optional RANGE)
;;   (anvil-xlsx-cell PATH SHEET ADDRESS)
;;
;; Scope:
;;   read-only API のみ。書き込みは scope 外。

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'anvil-host)

;;;; --- internal -----------------------------------------------------------

(defconst anvil-xlsx--python-script
  "import sys,json,openpyxl,datetime
op=sys.argv[1];path=sys.argv[2]
def cv(c):
    v=c.value
    if isinstance(v,(datetime.datetime,datetime.date)):return v.isoformat()
    return v
wb=openpyxl.load_workbook(path,data_only=True,read_only=True)
if op=='sheets':
    print(json.dumps(wb.sheetnames,ensure_ascii=False))
elif op=='info':
    info={n:{'rows':wb[n].max_row,'cols':wb[n].max_column} for n in wb.sheetnames}
    print(json.dumps({'sheets':wb.sheetnames,'sheet_info':info},ensure_ascii=False))
elif op=='cells':
    sh=sys.argv[3];rng=sys.argv[4];ws=wb[sh]
    if rng=='all':
        cells=[[cv(ws.cell(r,c)) for c in range(1,ws.max_column+1)] for r in range(1,ws.max_row+1)]
    else:
        cells=[[cv(c) for c in row] for row in ws[rng]]
    print(json.dumps(cells,ensure_ascii=False))
elif op=='cell':
    sh=sys.argv[3];addr=sys.argv[4]
    print(json.dumps(cv(wb[sh][addr]),ensure_ascii=False))
"
  "Inline Python script driving anvil-xlsx-* helpers. Run via `python -c'.")

(defun anvil-xlsx--norm-path (path)
  "Return PATH with backslashes converted to forward slashes (after expansion)."
  (replace-regexp-in-string "\\\\" "/" (expand-file-name path)))

(defun anvil-xlsx--python ()
  "Return path to a usable python interpreter, or error."
  (or (executable-find "python")
      (executable-find "python3")
      (error "anvil-xlsx: python not found in exec-path")))

(defun anvil-xlsx--null->nil (val)
  "Recursively convert JSON :null sentinels to nil in VAL.
Vectors stay vectors; alists/plists are not used here (top-level
results are arrays of arrays or plain values)."
  (cond
   ((eq val :null) nil)
   ((vectorp val)
    (let ((len (length val)))
      (dotimes (i len)
        (aset val i (anvil-xlsx--null->nil (aref val i))))
      val))
   (t val)))

(defun anvil-xlsx--run (op path &rest extra)
  "Run the inline python script with OP on PATH (+ EXTRA args).
Return the parsed JSON value (with :null normalized to nil).
Errors with `anvil-xlsx: ...' on python failure."
  (let* ((py (anvil-xlsx--python))
         (npath (anvil-xlsx--norm-path path))
         (parts (append (list (shell-quote-argument py)
                              "-c"
                              (shell-quote-argument anvil-xlsx--python-script)
                              (shell-quote-argument op)
                              (shell-quote-argument npath))
                        (mapcar #'shell-quote-argument extra)))
         (cmd (mapconcat #'identity parts " "))
         (res (anvil-shell cmd '(:max-output 1048576 :timeout 60 :coding utf-8))))
    (unless (eql (plist-get res :exit) 0)
      (error "anvil-xlsx: %s" (string-trim (plist-get res :stderr))))
    (let ((parsed (json-parse-string (plist-get res :stdout)
                                     :object-type 'plist
                                     :array-type 'array
                                     :null-object :null
                                     :false-object nil)))
      (anvil-xlsx--null->nil parsed))))

;;;; --- public API ---------------------------------------------------------

(defun anvil-xlsx-sheets (path)
  "Return the list of sheet names in xlsx file at PATH (as a Lisp list)."
  (let ((res (anvil-xlsx--run "sheets" path)))
    (append res nil)))

(defun anvil-xlsx-info (path)
  "Return a plist describing all sheets in xlsx file at PATH:
  (:sheets (NAME ..)
   :sheet-info ((NAME . (:rows N :cols N)) ..))"
  (let* ((raw (anvil-xlsx--run "info" path))
         (sheets (append (plist-get raw :sheets) nil))
         (info-plist (plist-get raw :sheet_info))
         info-alist)
    ;; info-plist is a plist whose keys are interned sheet-name keywords
    ;; (e.g. :Sheet1). Convert to ((NAME . (:rows N :cols N)) ..).
    (let ((p info-plist))
      (while p
        (let* ((k (car p))
               (v (cadr p))
               (name (substring (symbol-name k) 1)))
          (push (cons name (list :rows (plist-get v :rows)
                                 :cols (plist-get v :cols)))
                info-alist))
        (setq p (cddr p))))
    (list :sheets sheets
          :sheet-info (nreverse info-alist))))

(defun anvil-xlsx-read-cells (path sheet &optional range)
  "Read a 2D block of cells from SHEET in xlsx file at PATH.

RANGE is an A1-style string (\"A1:C10\") or 'all (default).
Returns a vector of vectors (rows × cols). Each cell value is
string / number / nil / ISO datetime string."
  (let ((rng (cond ((null range) "all")
                   ((eq range 'all) "all")
                   ((stringp range) range)
                   (t (error "anvil-xlsx-read-cells: invalid RANGE")))))
    (anvil-xlsx--run "cells" path sheet rng)))

(defun anvil-xlsx-cell (path sheet address)
  "Return the value of a single cell at ADDRESS (\"B5\") in SHEET of PATH.
Value is string / number / nil / ISO datetime string."
  (anvil-xlsx--run "cell" path sheet address))

;;;; --- write support (temp file JSON to avoid shell encoding issues) ------

(defconst anvil-xlsx--write-script
  "import sys,json,openpyxl
with open(sys.argv[1],'r',encoding='utf-8') as f:args=json.loads(f.read())
path=args['path'];op=args['op']
wb=openpyxl.load_workbook(path)
ws=wb[args['sheet']]
if op=='write_cells':
    for item in args['cells']:
        ws[item['addr']]=item['val']
elif op=='write_cell':
    ws[args['addr']]=args['val']
wb.save(path)
print(json.dumps({'ok':True,'op':op,'path':path},ensure_ascii=False))
"
  "Inline Python script for xlsx write ops. Args via temp JSON file (UTF-8).")

(defun anvil-xlsx--write-run (op path args-alist)
  "Run the write Python script with OP on PATH. ARGS-ALIST is extra keys.
Arguments go via temp JSON file (UTF-8) for Japanese path safety."
  (let* ((py (anvil-xlsx--python))
         (npath (anvil-xlsx--norm-path path))
         (payload (json-encode (append `(("op" . ,op) ("path" . ,npath))
                                       args-alist)))
         (tmpfile (make-temp-file "anvil-xlsx-w-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert payload)
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (point-min) (point-max) tmpfile nil 'silent)))
          (let* ((cmd (format "%s -c %s %s"
                              (shell-quote-argument py)
                              (shell-quote-argument anvil-xlsx--write-script)
                              (shell-quote-argument
                               (replace-regexp-in-string "\\\\" "/" tmpfile))))
                 (res (anvil-shell cmd '(:max-output 65536 :timeout 60
                                         :coding utf-8))))
            (unless (eql (plist-get res :exit) 0)
              (error "anvil-xlsx-write: %s"
                     (string-trim (plist-get res :stderr))))
            (json-parse-string (plist-get res :stdout)
                               :object-type 'plist
                               :null-object nil
                               :false-object nil)))
      (ignore-errors (delete-file tmpfile)))))

(defun anvil-xlsx-write-cell (path sheet address value)
  "Write VALUE to cell at ADDRESS in SHEET of xlsx file at PATH.
VALUE can be string, number, or nil (clears cell). Saves the file."
  (anvil-xlsx--write-run "write_cell" path
                          `(("sheet" . ,sheet)
                            ("addr" . ,address)
                            ("val" . ,value))))

(defun anvil-xlsx-write-cells (path sheet cells)
  "Write multiple CELLS to SHEET of xlsx file at PATH. Saves the file.

CELLS is a list of (ADDRESS . VALUE) pairs:
  '((\"A1\" . \"Header\") (\"B1\" . 42) (\"C1\" . nil))"
  (let ((items (mapcar (lambda (c)
                         `(("addr" . ,(car c)) ("val" . ,(cdr c))))
                       cells)))
    (anvil-xlsx--write-run "write_cells" path
                            `(("sheet" . ,sheet)
                              ("cells" . ,items)))))

;;;; --- discoverability ----------------------------------------------------

(defun anvil-xlsx-helpers-list ()
  "Return a list of all anvil-xlsx* user-facing function names."
  (let (results)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (let ((n (symbol-name sym)))
                    (and (string-prefix-p "anvil-xlsx" n)
                         (not (string-match-p "--" n)))))
         (push sym results))))
    (sort results (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(provide 'anvil-xlsx)
;;; anvil-xlsx.el ends here
