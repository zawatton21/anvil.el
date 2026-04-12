;;; anvil-proc.el --- Process management for anvil -*- lexical-binding: t; -*-

;; Author: zawatton21
;; Keywords: tools, mcp, claude, process

;;; Commentary:

;; anvil-host.el の姉妹ファイル。Claude Code が emacs-eval 経由で
;; ホスト PC のプロセス一覧 / 詳細を OS 非依存に問い合わせる helper。
;;
;; 構成:
;;   Layer 1 — `anvil-shell' (anvil-host が所有)
;;   Layer 2 — `anvil-proc-list' / `anvil-proc-find' / `anvil-proc-info'
;;
;; プロセス plist (list 系):
;;   (:pid N :ppid N-or-nil :name STR :user STR-or-nil
;;    :cpu-pct N-or-nil :rss-mb N :vsz-mb N-or-nil :cmdline STR-or-nil)
;;
;; 注意: :cpu-pct のセマンティクスは OS で異なる
;;   Windows : 累積 CPU 秒 (Get-Process .CPU)
;;   Unix    : 瞬時 CPU 使用率 % (ps %cpu)
;; どちらも数値が大きいほど忙しいので sort key としては問題ない。
;;
;; per-OS バックエンド:
;;   Windows  list: PowerShell Get-Process | ConvertTo-Json
;;            info: PowerShell Get-CimInstance Win32_Process
;;   Unix     ps -axo pid,ppid,user,%cpu,rss,vsz,comm,command
;;
;; PowerShell 起動コスト ~300ms を予算に入れる (Windows のみ)。
;; ConvertTo-Json は単一要素を array でなく object にする罠あり。
;;
;; API リファレンス: ~/Cowork/Notes/.claude/anvil-helpers-reference.org

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'anvil-host)

;;;; --- internal: shared helpers -------------------------------------------

(defun anvil-proc--bytes->mb (bytes)
  "Convert BYTES to mebibytes (rounded), or nil if BYTES is nil."
  (and (numberp bytes) (round (/ bytes 1048576.0))))

(defun anvil-proc--kb->mb (kb)
  "Convert KB to mebibytes (rounded), or nil if KB is nil."
  (and (numberp kb) (round (/ kb 1024.0))))

(defun anvil-proc--parse-wmi-date (s)
  "Parse `/Date(ms)/' WMI JSON date S into ISO 8601 (local time)."
  (when (and s (stringp s)
             (string-match "/Date(\\([0-9]+\\))/" s))
    (let ((ms (string-to-number (match-string 1 s))))
      (format-time-string "%Y-%m-%dT%H:%M:%S" (seconds-to-time (/ ms 1000.0))))))

(defun anvil-proc--json-records (output)
  "Parse JSON OUTPUT and return a list of plists.
Handles ConvertTo-Json's quirk where a single element is encoded as
an object instead of an array."
  (when (and output (not (string-empty-p (string-trim output))))
    (let ((parsed (json-parse-string output
                                     :object-type 'plist
                                     :null-object nil
                                     :false-object nil)))
      (cond
       ((vectorp parsed) (append parsed nil))
       ((listp parsed)   (list parsed))
       (t nil)))))

;;;; --- internal: per-OS list ----------------------------------------------

(defun anvil-proc--gather-windows ()
  "Return all processes on Windows as a list of plists.
Uses PowerShell Get-Process; user / cmdline are nil here. Use
`anvil-proc-info' on a single PID to enrich those fields."
  (let* ((cmd (concat "powershell -NoProfile -Command "
                      "\"Get-Process | Select-Object Id,ProcessName,CPU,WorkingSet64 | "
                      "ConvertTo-Json -Compress\""))
         (res (anvil-shell cmd '(:max-output 524288 :timeout 30 :coding utf-8)))
         (records (anvil-proc--json-records (plist-get res :stdout))))
    (mapcar
     (lambda (r)
       (list :pid     (plist-get r :Id)
             :ppid    nil
             :name    (plist-get r :ProcessName)
             :user    nil
             :cpu-pct (plist-get r :CPU)
             :rss-mb  (anvil-proc--bytes->mb (plist-get r :WorkingSet64))
             :vsz-mb  nil
             :cmdline nil))
     records)))

(defun anvil-proc--parse-ps-unix (output)
  "Parse OUTPUT of `ps -axo pid,ppid,user,%cpu,rss,vsz,comm,command'.
Header line is skipped. The 8th column (command) absorbs trailing
whitespace-separated tokens so spaces in the cmdline survive."
  (let (results)
    (dolist (line (cdr (split-string output "\n" t)))
      (let ((fields (split-string (string-trim line) "[ \t]+" nil)))
        (when (>= (length fields) 8)
          (let* ((pid     (string-to-number (nth 0 fields)))
                 (ppid    (string-to-number (nth 1 fields)))
                 (user    (nth 2 fields))
                 (cpu-pct (string-to-number (nth 3 fields)))
                 (rss-kb  (string-to-number (nth 4 fields)))
                 (vsz-kb  (string-to-number (nth 5 fields)))
                 (comm    (nth 6 fields))
                 (cmdline (string-join (nthcdr 7 fields) " ")))
            (push (list :pid pid
                        :ppid ppid
                        :name comm
                        :user user
                        :cpu-pct cpu-pct
                        :rss-mb (anvil-proc--kb->mb rss-kb)
                        :vsz-mb (anvil-proc--kb->mb vsz-kb)
                        :cmdline cmdline)
                  results)))))
    (nreverse results)))

(defun anvil-proc--gather-unix ()
  (let ((res (anvil-shell
              "ps -axo pid,ppid,user,%cpu,rss,vsz,comm,command 2>/dev/null"
              '(:max-output 524288 :timeout 15))))
    (anvil-proc--parse-ps-unix (plist-get res :stdout))))

(defun anvil-proc--gather ()
  "Return all processes on this host as a list of plists."
  (cond ((eq system-type 'windows-nt) (anvil-proc--gather-windows))
        ((or (eq system-type 'darwin) (eq system-type 'gnu/linux))
         (anvil-proc--gather-unix))
        (t (error "anvil-proc: unsupported system-type: %s" system-type))))

;;;; --- internal: filter / sort --------------------------------------------

(defun anvil-proc--sort-key (sort-by)
  "Map SORT-BY ('cpu / 'rss / 'pid) to a plist key."
  (pcase sort-by
    ('cpu :cpu-pct)
    ('rss :rss-mb)
    ('pid :pid)
    (_    (error "anvil-proc: invalid :sort value: %s" sort-by))))

(defun anvil-proc--apply-filter (procs filter)
  "Apply FILTER plist to PROCS. See `anvil-proc-list' for keys."
  (let* ((name-match (plist-get filter :name-match))
         (user       (plist-get filter :user))
         (limit      (if (plist-member filter :limit)
                         (plist-get filter :limit)
                       50))
         (sort-by    (or (plist-get filter :sort) 'cpu))
         (key        (anvil-proc--sort-key sort-by))
         (filtered procs)
         sorted)
    (when name-match
      (setq filtered
            (cl-remove-if-not
             (lambda (p) (and (plist-get p :name)
                              (string-match-p name-match (plist-get p :name))))
             filtered)))
    (when user
      (setq filtered
            (cl-remove-if-not
             (lambda (p) (and (plist-get p :user)
                              (string= user (plist-get p :user))))
             filtered)))
    (setq sorted
          (sort filtered
                (lambda (a b)
                  (let ((ka (or (plist-get a key) 0))
                        (kb (or (plist-get b key) 0)))
                    (if (eq sort-by 'pid)
                        (< ka kb)
                      (> ka kb))))))
    (if (and limit (> (length sorted) limit))
        (seq-take sorted limit)
      sorted)))

;;;; --- internal: per-OS info ----------------------------------------------

(defun anvil-proc--info-windows (pid)
  "Return a detailed plist for PID on Windows, or nil."
  (let* ((cmd (format
               (concat "powershell -NoProfile -Command "
                       "\"Get-CimInstance Win32_Process -Filter 'ProcessId=%d' | "
                       "Select-Object ProcessId,ParentProcessId,Name,CommandLine,"
                       "CreationDate,WorkingSetSize,VirtualSize,UserModeTime,KernelModeTime | "
                       "ConvertTo-Json -Compress\"")
               pid))
         (res (anvil-shell cmd '(:max-output 32768 :timeout 15 :coding utf-8)))
         (records (anvil-proc--json-records (plist-get res :stdout)))
         (rec (car records)))
    (when rec
      (let* ((user-time   (plist-get rec :UserModeTime))
             (kernel-time (plist-get rec :KernelModeTime))
             ;; UserModeTime / KernelModeTime are in 100-ns units.
             (total-secs  (and user-time kernel-time
                               (/ (+ user-time kernel-time) 1.0e7))))
        (list :pid        (plist-get rec :ProcessId)
              :ppid       (plist-get rec :ParentProcessId)
              :name       (plist-get rec :Name)
              :user       nil
              :cpu-pct    total-secs
              :rss-mb     (anvil-proc--bytes->mb (plist-get rec :WorkingSetSize))
              :vsz-mb     (anvil-proc--bytes->mb (plist-get rec :VirtualSize))
              :cmdline    (plist-get rec :CommandLine)
              :start-time (anvil-proc--parse-wmi-date (plist-get rec :CreationDate)))))))

(defun anvil-proc--info-unix (pid)
  "Return a detailed plist for PID on Unix, or nil.
Reuses the unix gather and adds :start-time via a separate ps call."
  (let* ((procs (anvil-proc--gather-unix))
         (rec (cl-find-if (lambda (p) (eql (plist-get p :pid) pid)) procs)))
    (when rec
      (let* ((res (anvil-shell (format "ps -p %d -o lstart= 2>/dev/null" pid)))
             (lstart (string-trim (plist-get res :stdout))))
        (append rec (list :start-time (and (not (string-empty-p lstart)) lstart)))))))

;;;; --- public API ---------------------------------------------------------

(defun anvil-proc-list (&optional filter)
  "Return a list of process plists.

FILTER is an optional plist:
  :name-match REGEXP   substring/regexp on process name
  :user STR            only this user (Unix only; ignored on Windows)
  :limit N             top N (default 50, nil = no limit)
  :sort BY             'cpu (default) | 'rss | 'pid

Each element:
  (:pid N :ppid N-or-nil :name STR :user STR-or-nil
   :cpu-pct N-or-nil :rss-mb N :vsz-mb N-or-nil :cmdline STR-or-nil)

On Windows, :user / :cmdline / :ppid / :vsz-mb are nil — use
`anvil-proc-info' on a single PID to fill them in via WMI."
  (anvil-proc--apply-filter (anvil-proc--gather) filter))

(defun anvil-proc-find (name-or-regexp)
  "Convenience: list processes whose name matches NAME-OR-REGEXP."
  (anvil-proc-list (list :name-match name-or-regexp :limit 50)))

(defun anvil-proc-info (pid)
  "Return a detailed plist for PID, or nil if no such process.
Does not error on missing PID — returns nil instead.

Extra fields beyond `anvil-proc-list': :start-time, and on Windows
also :ppid / :vsz-mb / :cmdline are populated."
  (cond ((eq system-type 'windows-nt) (anvil-proc--info-windows pid))
        ((or (eq system-type 'darwin) (eq system-type 'gnu/linux))
         (anvil-proc--info-unix pid))
        (t (error "anvil-proc: unsupported system-type: %s" system-type))))

;;;; --- anvil-proc-by-port -------------------------------------------------

(declare-function anvil-net-port-listen "anvil-net" (port &optional protocol))

(defun anvil-proc-by-port (port &optional protocol)
  "Resolve which process is listening on PORT into a single plist, or nil.

PROTOCOL: 'tcp (default), 'udp, or 'any.

Convenience composer for `anvil-net-port-listen' + `anvil-proc-info' —
saves a 2-step lookup (especially on Windows where the netstat row
only carries PID, not process name).

Returned plist is the union of net + proc fields:
  (:port :protocol :address :state               ; from net
   :pid :ppid :name :cmdline :rss-mb :vsz-mb     ; from proc
   :start-time)"
  (require 'anvil-net)
  (let ((sock (anvil-net-port-listen port protocol)))
    (when sock
      (let* ((pid (plist-get sock :pid))
             (info (and pid (anvil-proc-info pid))))
        (append
         (list :port     (plist-get sock :port)
               :protocol (plist-get sock :protocol)
               :address  (plist-get sock :address)
               :state    (plist-get sock :state))
         (if info
             (list :pid        (plist-get info :pid)
                   :ppid       (plist-get info :ppid)
                   :name       (plist-get info :name)
                   :cmdline    (plist-get info :cmdline)
                   :rss-mb     (plist-get info :rss-mb)
                   :vsz-mb     (plist-get info :vsz-mb)
                   :start-time (plist-get info :start-time))
           ;; No proc info available (PID 0 / kernel / permission denied)
           (list :pid pid :name nil)))))))

;;;; --- discoverability ----------------------------------------------------

(defun anvil-proc-helpers-list ()
  "Return a list of all anvil-proc* user-facing function names."
  (let (results)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (let ((n (symbol-name sym)))
                    (and (string-prefix-p "anvil-proc" n)
                         (not (string-match-p "--" n)))))
         (push sym results))))
    (sort results (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(provide 'anvil-proc)
;;; anvil-proc.el ends here
