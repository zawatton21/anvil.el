;;; anvil-data.el --- JSON and CSV data tools for anvil -*- lexical-binding: t; -*-

;; Author: zawatton21
;; Keywords: tools, mcp, claude, json, csv, data

;;; Commentary:

;; anvil-helpers の姉妹ファイル。Claude Code が emacs-eval 経由で
;; JSON 等のデータファイルを 1 call で型安全に読み書きするための helper。
;;
;; 主要動機:
;;   settings.json / report-plan.json / package-lock.json などを
;;   Edit + 手動 parse でなく、構造化された plist で扱う。
;;
;; 特徴:
;;   - UTF-8 強制 (`anvil--insert-file' で raw byte 罠回避)
;;   - 巨大ファイルでも with-temp-buffer + write-region で安全
;;   - JSON null/false は Emacs の :null / :false シンボルで往復可能
;;
;; 注意 (null/false handling):
;;   `json-parse-buffer' のデフォルトでは JSON null → :null シンボル、
;;   false → :false シンボルが返る。Lisp の `nil' は使われない。
;;   この helper はそれを引き継ぐ — 読み書きで往復しても sentinel が
;;   保たれる。Lisp の `nil' を意図的に渡したい場合は読み込み後に
;;   自分で変換すること。

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'anvil-file)

;;;; --- internal -----------------------------------------------------------

(defun anvil-data--require-parent (path)
  "Error if the parent directory of PATH does not exist."
  (let ((parent (file-name-directory (expand-file-name path))))
    (unless (and parent (file-directory-p parent))
      (error "anvil-data: parent directory does not exist: %s" parent))))

;;;; --- public API ---------------------------------------------------------

(defun anvil-data-read-json (path)
  "Read JSON from PATH and return it as a plist (objects) / vector (arrays).

JSON null is returned as the symbol `:null'; false as `:false'.
Numbers, strings, and `t' are returned as themselves. Use these
sentinels back when calling `anvil-data-write-json' for round-trip
fidelity.

Errors with `(error \"anvil-data: ...\")' on file or parse failure."
  (let ((abs (anvil--prepare-path path)))
    (with-temp-buffer
      (anvil--insert-file abs)
      (goto-char (point-min))
      (condition-case err
          (json-parse-buffer :object-type 'plist
                             :array-type 'array
                             :null-object :null
                             :false-object :false)
        (error
         (error "anvil-data: JSON parse error in %s: %s"
                abs (error-message-string err)))))))

(defun anvil-data-write-json (path data &optional pretty)
  "Write DATA to PATH as JSON (UTF-8, LF newlines).

PRETTY non-nil pretty-prints via `json-pretty-print-buffer'.

Round-trip with `anvil-data-read-json' is preserved when DATA uses
the `:null' / `:false' sentinels for JSON null / false; plain Lisp
`nil' inside DATA may be encoded as an empty array / object
depending on context (Emacs `json-serialize' semantics).

Returns (:file PATH :bytes N :pretty BOOL)."
  (anvil-data--require-parent path)
  (let ((abs (expand-file-name path)) bytes)
    (with-temp-buffer
      (insert (json-serialize data
                              :null-object :null
                              :false-object :false))
      (when pretty
        (json-pretty-print-buffer))
      (goto-char (point-max))
      (unless (eq (char-before) ?\n) (insert "\n"))
      (setq bytes (buffer-size))
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max) abs nil 'silent)))
    (list :file abs :bytes bytes :pretty (and pretty t))))

;;;; --- CSV ----------------------------------------------------------------

(defun anvil-data--parse-csv-line (line delim)
  "Parse a single CSV LINE into a list of strings.
Handles double-quoted fields including escaped quotes (\"\")."
  (let ((pos 0)
        (len (length line))
        (fields '())
        (current "")
        (in-quote nil))
    (while (< pos len)
      (let ((ch (aref line pos)))
        (cond
         (in-quote
          (cond
           ((and (= ch ?\")
                 (< (1+ pos) len)
                 (= (aref line (1+ pos)) ?\"))
            ;; Escaped quote
            (setq current (concat current "\""))
            (setq pos (+ pos 2)))
           ((= ch ?\")
            (setq in-quote nil)
            (setq pos (1+ pos)))
           (t
            (setq current (concat current (string ch)))
            (setq pos (1+ pos)))))
         (t
          (cond
           ((= ch ?\")
            (setq in-quote t)
            (setq pos (1+ pos)))
           ((= ch delim)
            (push current fields)
            (setq current "")
            (setq pos (1+ pos)))
           (t
            (setq current (concat current (string ch)))
            (setq pos (1+ pos))))))))
    (push current fields)
    (nreverse fields)))

(defun anvil-data-read-csv (path &optional opts)
  "Read CSV file PATH into a list of rows (each row a list of strings).

OPTS plist:
  :delimiter CHAR    field separator (default ?, comma)
  :encoding SYM      coding system for reading (default 'utf-8)
  :header BOOL       if t, return (:header (..) :rows ((..) ..)) instead
  :skip-rows N       skip N rows from the top before parsing
  :max-rows N        cap returned rows (default nil = all)

Quoted fields with escaped quotes (\"\") are supported. Newlines
inside quoted fields are NOT supported (rare and adds complexity).

Returns either:
  ((row1-field1 row1-field2 ..) (row2-field1 ..) ..)   ; default
  (:header (..) :rows ((..) ..))                        ; with :header t

For Money Forward / 会計ソフト の Shift_JIS CSV, pass :encoding 'cp932."
  (let* ((abs (anvil--prepare-path path))
         (delim (or (plist-get opts :delimiter) ?,))
         (encoding (or (plist-get opts :encoding) 'utf-8))
         (header-p (plist-get opts :header))
         (skip (or (plist-get opts :skip-rows) 0))
         (max-rows (plist-get opts :max-rows))
         (lines '()))
    (with-temp-buffer
      (let ((coding-system-for-read encoding))
        (insert-file-contents abs))
      (goto-char (point-min))
      ;; Skip leading rows
      (dotimes (_ skip)
        (forward-line 1))
      ;; Read remaining lines
      (while (and (not (eobp))
                  (or (null max-rows) (< (length lines) max-rows)))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (unless (string-empty-p line)
            (push (anvil-data--parse-csv-line line delim) lines))
          (forward-line 1))))
    (let ((rows (nreverse lines)))
      (if header-p
          (list :header (car rows) :rows (cdr rows))
        rows))))

;;;; --- discoverability ----------------------------------------------------

(defun anvil-data-helpers-list ()
  "Return a list of all anvil-data* user-facing function names."
  (let (results)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (let ((n (symbol-name sym)))
                    (and (string-prefix-p "anvil-data" n)
                         (not (string-match-p "--" n)))))
         (push sym results))))
    (sort results (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(provide 'anvil-data)
;;; anvil-data.el ends here
