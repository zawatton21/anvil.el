;;; anvil-pdf.el --- PDF extraction for anvil -*- lexical-binding: t; -*-

;; Author: zawatton21
;; Keywords: tools, mcp, claude, pdf, pymupdf

;;; Commentary:

;; anvil-helpers の姉妹ファイル。Claude Code が emacs-eval 経由で
;; PDF ファイルのテキスト抽出・メタデータ取得を 1 call で行うための
;; pymupdf (fitz) wrapper。
;;
;; 設計方針:
;;   - python -c インライン script + JSON 経由 (anvil-xlsx と同パターン)
;;   - pymupdf は read_only なので安全
;;   - ScanSnap スキャン文書 / 点検報告書 / 法令規程 PDF を対象
;;   - 重い PDF 処理は eat-dev daemon 経由で呼ぶことを推奨
;;
;; Python 環境:
;;   pymupdf は C 拡張を含むため、全 Python 環境にインストールできるとは限らない。
;;   msys64 Python 3.14 ではビルド失敗。Windows 公式 Python 3.13 では OK。
;;   `anvil-pdf-python' 変数で使用する Python パスを指定可能。
;;
;; Public API:
;;   (anvil-pdf-info PATH)
;;   (anvil-pdf-text PATH &optional PAGES)
;;   (anvil-pdf-search PATH QUERY &optional PAGES)
;;   (anvil-pdf-toc PATH)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'anvil-host)

;;;; --- configuration ------------------------------------------------------

(defvar anvil-pdf-python nil
  "Path to a Python interpreter with pymupdf (fitz) installed.
If nil, `executable-find' is used to locate python/python3.
Set this in init-eat-dev.el when the default python lacks pymupdf.")

;;;; --- internal -----------------------------------------------------------

(defconst anvil-pdf--python-script
  "import sys,json,fitz
with open(sys.argv[1],'r',encoding='utf-8') as f:args=json.loads(f.read())
op=args['op'];path=args['path']
doc=fitz.open(path)
if op=='info':
    m=doc.metadata or {}
    print(json.dumps({'page_count':doc.page_count,'metadata':m},ensure_ascii=False))
elif op=='text':
    pages=args.get('pages')
    result=[]
    for i in range(doc.page_count):
        if pages is None or (i+1) in pages:
            p=doc[i]
            result.append({'page':i+1,'width':p.rect.width,'height':p.rect.height,
                           'text':p.get_text()})
    print(json.dumps(result,ensure_ascii=False))
elif op=='search':
    query=args['query'];pages=args.get('pages')
    result=[]
    for i in range(doc.page_count):
        if pages is None or (i+1) in pages:
            hits=doc[i].search_for(query)
            if hits:
                result.append({'page':i+1,'count':len(hits),
                               'rects':[[r.x0,r.y0,r.x1,r.y1] for r in hits]})
    print(json.dumps(result,ensure_ascii=False))
elif op=='toc':
    print(json.dumps(doc.get_toc(),ensure_ascii=False))
doc.close()
"
  "Inline Python script driving anvil-pdf-* helpers.
Arguments are passed via stdin as JSON to avoid shell encoding issues
with Japanese paths on msys bash → Windows Python.")

(defun anvil-pdf--python ()
  "Return path to a Python with pymupdf, or error."
  (or anvil-pdf-python
      (executable-find "python")
      (executable-find "python3")
      (error "anvil-pdf: python not found")))

(defun anvil-pdf--norm-path (path)
  "Return PATH with backslashes → forward slashes (after expansion)."
  (replace-regexp-in-string "\\\\" "/" (expand-file-name path)))

(defun anvil-pdf--run (op path &optional args-alist)
  "Run inline python script with OP on PATH. ARGS-ALIST is extra keys for the
JSON payload. Arguments go via a temp JSON file to avoid msys bash → Windows
Python shell encoding issues with Japanese paths. Returns parsed JSON."
  (let* ((py (anvil-pdf--python))
         (npath (anvil-pdf--norm-path path))
         (payload (json-encode (append `(("op" . ,op) ("path" . ,npath))
                                       args-alist)))
         (tmpfile (make-temp-file "anvil-pdf-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert payload)
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (point-min) (point-max) tmpfile nil 'silent)))
          (let* ((cmd (format "%s -c %s %s"
                              (shell-quote-argument py)
                              (shell-quote-argument anvil-pdf--python-script)
                              (shell-quote-argument
                               (replace-regexp-in-string "\\\\" "/" tmpfile))))
                 (res (anvil-shell cmd '(:max-output 4194304 :timeout 120
                                         :coding utf-8))))
            (unless (eql (plist-get res :exit) 0)
              (error "anvil-pdf: %s" (string-trim (plist-get res :stderr))))
            (json-parse-string (plist-get res :stdout)
                               :object-type 'plist
                               :array-type 'list
                               :null-object nil
                               :false-object nil)))
      (ignore-errors (delete-file tmpfile)))))

;;;; --- public API ---------------------------------------------------------

(defun anvil-pdf-info (path)
  "Return metadata and page count of PDF at PATH.
Result: (:page-count N :metadata (:title :author :subject :creator ..))"
  (let ((raw (anvil-pdf--run "info" path nil)))
    (list :page-count (plist-get raw :page_count)
          :metadata (plist-get raw :metadata))))

(defun anvil-pdf-text (path &optional pages)
  "Extract text from PDF at PATH.

PAGES is a list of 1-based page numbers (e.g. '(1 2 3)).
nil means all pages.

Each result element: (:page N :width F :height F :text STR)"
  (anvil-pdf--run "text" path
                   (when pages `(("pages" . ,pages)))))

(defun anvil-pdf-search (path query &optional pages)
  "Search for QUERY string in PDF at PATH.

PAGES is a list of 1-based page numbers. nil means all pages.

Each result element: (:page N :count N :rects ((x0 y0 x1 y1) ..))"
  (anvil-pdf--run "search" path
                   (append `(("query" . ,query))
                           (when pages `(("pages" . ,pages))))))

(defun anvil-pdf-toc (path)
  "Return the table of contents of PDF at PATH.
Result: list of (LEVEL TITLE PAGE-NUMBER) entries."
  (anvil-pdf--run "toc" path nil))

;;;; --- discoverability ----------------------------------------------------

(defun anvil-pdf-helpers-list ()
  "Return a list of all anvil-pdf* user-facing function names."
  (let (results)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (let ((n (symbol-name sym)))
                    (and (string-prefix-p "anvil-pdf" n)
                         (not (string-match-p "--" n)))))
         (push sym results))))
    (sort results (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(provide 'anvil-pdf)
;;; anvil-pdf.el ends here
