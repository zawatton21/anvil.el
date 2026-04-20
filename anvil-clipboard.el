;;; anvil-clipboard.el --- OS clipboard for anvil -*- lexical-binding: t; -*-

;; Author: zawatton
;; Keywords: tools, mcp, claude, clipboard

;;; Commentary:

;; anvil-helpers の姉妹ファイル。Claude Code が emacs-eval 経由で
;; OS のシステムクリップボードの読み書きを行うための helper。
;;
;; Windows: PowerShell Get-Clipboard / Set-Clipboard (UTF-8 強制)
;; macOS:   pbpaste / pbcopy
;; Linux:
;;   - Wayland session (WAYLAND_DISPLAY set):
;;       wl-copy / wl-paste (wl-clipboard) > xclip > xsel
;;   - X11 session:
;;       xclip > xsel
;;
;; Public API:
;;   (anvil-clipboard-get)        — read
;;   (anvil-clipboard-set STRING) — write

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-host)

;;;; --- Linux session detection --------------------------------------------

(defun anvil-clipboard--wayland-session-p ()
  "Return non-nil when the current Linux session is Wayland.
Detection uses `WAYLAND_DISPLAY', which is only exported by a
compositor when a live Wayland socket exists.  `XDG_SESSION_TYPE'
is less reliable (it can be \"wayland\" for an X11 client running
under XWayland) and is intentionally not consulted."
  (let ((wd (getenv "WAYLAND_DISPLAY")))
    (and wd (not (string-empty-p wd)))))

(defun anvil-clipboard--linux-get-command ()
  "Return the shell command string to read the clipboard on Linux.
Returns nil if no supported tool is available."
  (cond
   ((and (anvil-clipboard--wayland-session-p)
         (executable-find "wl-paste"))
    "wl-paste --no-newline")
   ((executable-find "xclip")
    "xclip -selection clipboard -o")
   ((executable-find "xsel")
    "xsel --clipboard --output")))

(defun anvil-clipboard--linux-set-command (string)
  "Return the shell command string to write STRING to the Linux clipboard.
Returns nil if no supported tool is available."
  (let ((q (shell-quote-argument string)))
    (cond
     ((and (anvil-clipboard--wayland-session-p)
           (executable-find "wl-copy"))
      (format "printf '%%s' %s | wl-copy" q))
     ((executable-find "xclip")
      (format "printf '%%s' %s | xclip -selection clipboard" q))
     ((executable-find "xsel")
      (format "printf '%%s' %s | xsel --clipboard --input" q)))))

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
                (let ((cmd (anvil-clipboard--linux-get-command)))
                  (unless cmd
                    (error "anvil-clipboard: wl-clipboard (Wayland), xclip, or xsel required on Linux"))
                  (anvil-shell cmd '(:coding utf-8 :timeout 5))))
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
                 (let ((cmd (anvil-clipboard--linux-set-command string)))
                   (unless cmd
                     (error "anvil-clipboard: wl-clipboard (Wayland), xclip, or xsel required on Linux"))
                   (anvil-shell cmd '(:coding utf-8 :timeout 5))))
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
