;;; anvil-clipboard.el --- OS clipboard for anvil -*- lexical-binding: t; -*-

;; Author: zawatton21
;; Keywords: tools, mcp, claude, clipboard

;;; Commentary:

;; anvil-helpers の姉妹ファイル。Claude Code が emacs-eval 経由で
;; OS のシステムクリップボードの読み書きを行うための helper。
;;
;; Windows: PowerShell Get-Clipboard / Set-Clipboard (UTF-8 強制)
;; macOS:   pbpaste / pbcopy
;; Linux:   xclip / xsel
;;
;; Public API:
;;   (anvil-clipboard-get)        — read
;;   (anvil-clipboard-set STRING) — write

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-host)

;;;; --- public API ---------------------------------------------------------

(defun anvil-clipboard-get ()
  "Return the current system clipboard content as a string.
Returns nil if clipboard is empty or unavailable."
  (let ((res (pcase system-type
               ('windows-nt
                (anvil-shell
                 "powershell -NoProfile -Command \"[Console]::OutputEncoding=[Text.Encoding]::UTF8; Get-Clipboard\""
                 '(:coding utf-8 :timeout 10)))
               ('darwin
                (anvil-shell "pbpaste" '(:coding utf-8 :timeout 5)))
               ('gnu/linux
                (or (and (executable-find "xclip")
                         (anvil-shell "xclip -selection clipboard -o"
                                      '(:coding utf-8 :timeout 5)))
                    (and (executable-find "xsel")
                         (anvil-shell "xsel --clipboard --output"
                                      '(:coding utf-8 :timeout 5)))
                    (error "anvil-clipboard: xclip or xsel required on Linux")))
               (_ (error "anvil-clipboard: unsupported system-type %s" system-type)))))
    (when (eql (plist-get res :exit) 0)
      (let ((text (plist-get res :stdout)))
        (when (and text (not (string-empty-p text)))
          ;; Remove trailing newline added by Get-Clipboard / pbpaste
          (string-trim-right text "\r?\n"))))))

(defun anvil-clipboard-set (string)
  "Set the system clipboard content to STRING. Returns t on success."
  (let* ((tmpfile (when (eq system-type 'windows-nt)
                    (make-temp-file "anvil-clip-" nil ".txt")))
         (res
          (unwind-protect
              (pcase system-type
                ('windows-nt
                 ;; Write to temp file to avoid shell quoting issues with Japanese
                 (with-temp-buffer
                   (insert string)
                   (let ((coding-system-for-write 'utf-8-unix))
                     (write-region (point-min) (point-max) tmpfile nil 'silent)))
                 (anvil-shell
                  (format "powershell -NoProfile -Command \"Get-Content -Encoding UTF8 '%s' | Set-Clipboard\""
                          (replace-regexp-in-string "\\\\" "/" tmpfile))
                  '(:coding utf-8 :timeout 10)))
                ('darwin
                 (anvil-shell (format "printf '%%s' %s | pbcopy"
                                      (shell-quote-argument string))
                              '(:coding utf-8 :timeout 5)))
                ('gnu/linux
                 (cond
                  ((executable-find "xclip")
                   (anvil-shell (format "printf '%%s' %s | xclip -selection clipboard"
                                        (shell-quote-argument string))
                                '(:coding utf-8 :timeout 5)))
                  ((executable-find "xsel")
                   (anvil-shell (format "printf '%%s' %s | xsel --clipboard --input"
                                        (shell-quote-argument string))
                                '(:coding utf-8 :timeout 5)))
                  (t (error "anvil-clipboard: xclip or xsel required on Linux"))))
                (_ (error "anvil-clipboard: unsupported system-type %s" system-type)))
            (when tmpfile (ignore-errors (delete-file tmpfile))))))
    (eql (plist-get res :exit) 0)))

;;;; --- discoverability ----------------------------------------------------

(defun anvil-clipboard-helpers-list ()
  "Return a list of all anvil-clipboard* user-facing function names."
  (let (results)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (let ((n (symbol-name sym)))
                    (and (string-prefix-p "anvil-clipboard" n)
                         (not (string-match-p "--" n)))))
         (push sym results))))
    (sort results (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(provide 'anvil-clipboard)
;;; anvil-clipboard.el ends here
