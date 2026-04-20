;;; anvil-host.el --- OS and shell abstraction for anvil -*- lexical-binding: t; -*-

;; Author: zawatton
;; Keywords: tools, mcp, claude, system

;;; Commentary:

;; anvil-helpers.el の姉妹ファイル。Claude Code が emacs-eval
;; (mcp__org-mcp__emacs-eval) 経由で呼び出すための host inspection
;; helper 群。
;;
;; 目的:
;;   1. Emacs server が動いているホスト PC (= 実機 = VM 環境外) の
;;      仕様を OS 非依存 API で取得する
;;   2. OS ごとのコマンド差を helper 内に閉じ込め、Claude のトークン
;;      消費と判断コストを削減
;;   3. wmic / sysctl / /proc 等の冗長な出力を構造化 plist に整形
;;
;; 構成 (2 layer):
;;
;;   Layer 1 — 汎用 shell ランナー
;;     `anvil-shell'        任意 shell コマンド (timeout/coding/truncate 付)
;;     `anvil-shell-by-os'  :windows/:darwin/:linux 分岐版
;;
;;   Layer 2 — 構造化済み host 情報
;;     `anvil-host-info'    カテゴリ指定で plist を返す
;;       (os cpu ram disk gpu network uptime emacs)
;;
;; 戻り値: plist。失敗時は (error "anvil-host: ...") を投げる。
;;
;; API リファレンス: ~/Cowork/Notes/.claude/anvil-helpers-reference.org

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; --- defaults / constants -----------------------------------------------

(defconst anvil-host--default-timeout 30
  "Default timeout (seconds) for `anvil-shell'.")

(defconst anvil-host--default-max-output 16384
  "Default max output bytes captured per stream by `anvil-shell'.")

(defun anvil-host--default-coding ()
  "Return the default coding system for shell I/O on this OS.
Windows defaults to cp932-dos because wmic / systeminfo emit Shift_JIS
on Japanese Windows. Other OSes default to utf-8."
  (if (eq system-type 'windows-nt) 'cp932-dos 'utf-8))

;;;; --- internal: shell run ------------------------------------------------

(defun anvil-host--truncate (str max)
  "Truncate STR to MAX bytes, append a marker if cut."
  (if (and (numberp max) (> (length str) max))
      (concat (substring str 0 max)
              (format "\n...[anvil-host: truncated, %d more bytes]"
                      (- (length str) max)))
    str))

(defun anvil-host--run (command coding cwd timeout)
  "Run COMMAND in shell asynchronously and wait up to TIMEOUT seconds.
Returns (EXIT STDOUT STDERR). Errors on timeout.

`:connection-type' is forced to `pipe' (rather than the Emacs default
PTY).  With a PTY the spawned shell becomes a session leader, and
when the shell exits the kernel sends SIGHUP to every process in
that session — which kills detached / disowned / nohup'd background
descendants that clipboard helpers such as `wl-copy' rely on (see
issue #10).  Pipe mode leaves the shell in Emacs's session; detached
children survive the shell exiting.  `isatty(0/1/2)' becomes false
for the shell and its children, but the helpers we invoke (wmic /
wl-copy / xclip / pbcopy / ...) are all fine with non-tty I/O."
  (let* ((stdout-buf (generate-new-buffer " *anvil-host-stdout*"))
         (stderr-buf (generate-new-buffer " *anvil-host-stderr*"))
         (default-directory (or cwd default-directory))
         (process-coding-system-alist
          (cons (cons "" (cons coding coding))
                process-coding-system-alist))
         proc)
    (unwind-protect
        (progn
          (setq proc
                (make-process
                 :name "anvil-host-shell"
                 :buffer stdout-buf
                 :stderr stderr-buf
                 :noquery t
                 :connection-type 'pipe
                 ;; Emacs' default process sentinel writes a
                 ;; "Process NAME finished" status line into the
                 ;; process buffer on exit, which then shows up in
                 ;; the captured stdout.  Silence it so callers can
                 ;; `string-trim' :stdout and get the actual bytes.
                 :sentinel #'ignore
                 :command (list shell-file-name shell-command-switch command)))
          ;; `make-process' wraps the :stderr buffer in a pipe process
          ;; whose default sentinel ALSO writes a status line.  Nuke
          ;; that too.
          (let ((stderr-proc (get-buffer-process stderr-buf)))
            (when (processp stderr-proc)
              (set-process-sentinel stderr-proc #'ignore)))
          (let ((deadline (+ (float-time) timeout)))
            (while (and (process-live-p proc)
                        (< (float-time) deadline))
              (accept-process-output proc 0.1))
            (when (process-live-p proc)
              (delete-process proc)
              (error "anvil-host: shell timeout after %ss: %s"
                     timeout command)))
          (let ((exit (process-exit-status proc))
                (out  (with-current-buffer stdout-buf (buffer-string)))
                (err  (with-current-buffer stderr-buf (buffer-string))))
            (list exit out err)))
      (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf))
      (when (buffer-live-p stderr-buf) (kill-buffer stderr-buf)))))

;;;; --- Layer 1: anvil-shell -----------------------------------------------

(defun anvil-shell (command &optional opts)
  "Run shell COMMAND on the host and return result as a plist.
OPTS is a plist:
  :timeout    seconds (default 30)
  :max-output bytes per stream (default 16384, nil = no limit)
  :coding     coding-system for I/O (default: cp932-dos on Windows, utf-8 elsewhere)
  :cwd        working directory

Returns:
  (:exit N :stdout STR :stderr STR :command STR :coding SYM
   :truncated BOOL)

Note: this is synchronous from the caller's perspective but uses
`make-process' under the hood, so the Emacs server can still service
other connections via accept-process-output yields."
  (let* ((timeout    (or (plist-get opts :timeout) anvil-host--default-timeout))
         (max-output (if (plist-member opts :max-output)
                         (plist-get opts :max-output)
                       anvil-host--default-max-output))
         (coding     (or (plist-get opts :coding) (anvil-host--default-coding)))
         (cwd        (plist-get opts :cwd))
         (result     (anvil-host--run command coding cwd timeout))
         (exit       (nth 0 result))
         (stdout     (nth 1 result))
         (stderr     (nth 2 result))
         (truncated  (or (and max-output (> (length stdout) max-output))
                         (and max-output (> (length stderr) max-output)))))
    (list :exit exit
          :stdout (if max-output (anvil-host--truncate stdout max-output) stdout)
          :stderr (if max-output (anvil-host--truncate stderr max-output) stderr)
          :command command
          :coding coding
          :truncated (and truncated t))))

(defun anvil-shell-by-os (spec &optional opts)
  "Run an OS-dispatched shell command. SPEC is a plist:
  :windows  command for windows-nt
  :darwin   command for macOS
  :linux    command for gnu/linux

The command matching `system-type' is forwarded to `anvil-shell'
together with OPTS. Errors if the current OS has no entry."
  (let ((cmd (cond ((eq system-type 'windows-nt) (plist-get spec :windows))
                   ((eq system-type 'darwin)     (plist-get spec :darwin))
                   ((eq system-type 'gnu/linux)  (plist-get spec :linux))
                   (t (error "anvil-host: unsupported system-type: %s"
                             system-type)))))
    (unless cmd
      (error "anvil-host: no command provided for %s in spec" system-type))
    (anvil-shell cmd opts)))

;;;; --- internal: parsers --------------------------------------------------

(defun anvil-host--parse-wmic-value (output)
  "Parse `wmic ... /value' OUTPUT into a list of plists, one per record.
Records are separated by blank lines. Keys are downcased and interned
as keywords.

Note: wmic emits \\r\\r\\n line endings on Windows; we split on \\n
only and trim trailing \\r from each line so that intra-line CRs do
not look like record separators."
  (let ((records '())
        (current '()))
    (dolist (raw (split-string output "\n" nil))
      (let ((line (string-trim raw)))
        (cond
         ((string-empty-p line)
          (when current
            (push (nreverse current) records)
            (setq current nil)))
         ((string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" line)
          (let ((k (intern (concat ":" (downcase (match-string 1 line)))))
                (v (string-trim (match-string 2 line))))
            ;; push key first, then value, so after nreverse the
            ;; result is (k v k v ...) — a proper plist.
            (push k current)
            (push v current))))))
    (when current (push (nreverse current) records))
    (nreverse records)))

(defun anvil-host--first-record (output)
  "Return the first plist parsed from wmic /value OUTPUT, or nil."
  (car (anvil-host--parse-wmic-value output)))

(defun anvil-host--num (str)
  "Parse STR as a number, returning nil on failure or empty input."
  (and str
       (let ((s (string-trim str)))
         (and (not (string-empty-p s))
              (string-match-p "\\`-?[0-9]+\\(\\.[0-9]+\\)?\\'" s)
              (string-to-number s)))))

(defun anvil-host--bytes->gb (bytes &optional precision)
  "Convert BYTES (number) to gigabytes, rounded to PRECISION (default 1)."
  (and (numberp bytes)
       (let ((p (or precision 1)))
         (/ (round (* (/ bytes 1073741824.0) (expt 10 p)))
            (float (expt 10 p))))))

(defun anvil-host--parse-wmic-array (s)
  "Parse a wmic array value like {\"a\",\"b\",\"c\"} into a list of strings.
Returns a list of strings, or a single-element list if S is a plain
scalar, or nil for empty input."
  (cond
   ((or (null s) (string-empty-p s)) nil)
   ((string-match-p "\\`{.*}\\'" s)
    (let (results
          (start 0))
      (while (string-match "\"\\([^\"]*\\)\"" s start)
        (push (match-string 1 s) results)
        (setq start (match-end 0)))
      (nreverse results)))
   (t (list s))))

(defun anvil-host--first-ipv4 (strings)
  "Return the first IPv4 address from STRINGS, or nil."
  (cl-find-if (lambda (s) (string-match-p "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\'" s))
              strings))

(defun anvil-host--parse-wmi-datetime (s)
  "Parse a WMI datetime string yyyymmddHHMMSS.ffffff±UTC into ISO 8601."
  (when (and s (string-match
                "\\`\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)"
                s))
    (format "%s-%s-%sT%s:%s:%s"
            (match-string 1 s) (match-string 2 s) (match-string 3 s)
            (match-string 4 s) (match-string 5 s) (match-string 6 s))))

;;;; --- internal: per-OS host queries --------------------------------------

;; Each anvil-host--info-CATEGORY-OS function returns a plist for that
;; category. They never error — on failure they return (:error STR).

;; --- OS ---

(defun anvil-host--info-os-windows ()
  (let* ((res (anvil-shell
               "wmic os get Caption,Version,BuildNumber,OSArchitecture /value"
               '(:max-output 4096)))
         (rec (anvil-host--first-record (plist-get res :stdout))))
    (list :type 'windows-nt
          :name        (plist-get rec :caption)
          :version     (plist-get rec :version)
          :build       (plist-get rec :buildnumber)
          :arch        (plist-get rec :osarchitecture)
          :hostname    (system-name)
          :user        (user-login-name))))

(defun anvil-host--info-os-darwin ()
  (let* ((name    (string-trim (plist-get (anvil-shell "sw_vers -productName") :stdout)))
         (version (string-trim (plist-get (anvil-shell "sw_vers -productVersion") :stdout)))
         (build   (string-trim (plist-get (anvil-shell "sw_vers -buildVersion") :stdout)))
         (arch    (string-trim (plist-get (anvil-shell "uname -m") :stdout))))
    (list :type 'darwin :name name :version version :build build :arch arch
          :hostname (system-name) :user (user-login-name))))

(defun anvil-host--info-os-linux ()
  (let* ((res (anvil-shell "cat /etc/os-release 2>/dev/null"))
         (out (plist-get res :stdout))
         name version)
    (dolist (line (split-string out "\n" t))
      (when (string-match "\\`PRETTY_NAME=\"?\\([^\"]*\\)\"?" line)
        (setq name (match-string 1 line)))
      (when (string-match "\\`VERSION_ID=\"?\\([^\"]*\\)\"?" line)
        (setq version (match-string 1 line))))
    (list :type 'gnu/linux
          :name name
          :version version
          :arch (string-trim (plist-get (anvil-shell "uname -m") :stdout))
          :kernel (string-trim (plist-get (anvil-shell "uname -r") :stdout))
          :hostname (system-name)
          :user (user-login-name))))

;; --- CPU ---

(defun anvil-host--info-cpu-windows ()
  (let* ((res (anvil-shell
               "wmic cpu get Name,NumberOfCores,NumberOfLogicalProcessors,MaxClockSpeed /value"
               '(:max-output 4096)))
         (rec (anvil-host--first-record (plist-get res :stdout))))
    (list :model    (plist-get rec :name)
          :physical (anvil-host--num (plist-get rec :numberofcores))
          :logical  (anvil-host--num (plist-get rec :numberoflogicalprocessors))
          :mhz-max  (anvil-host--num (plist-get rec :maxclockspeed)))))

(defun anvil-host--info-cpu-darwin ()
  (list :model    (string-trim (plist-get (anvil-shell "sysctl -n machdep.cpu.brand_string") :stdout))
        :physical (anvil-host--num (string-trim (plist-get (anvil-shell "sysctl -n hw.physicalcpu") :stdout)))
        :logical  (anvil-host--num (string-trim (plist-get (anvil-shell "sysctl -n hw.logicalcpu") :stdout)))
        :mhz-max  (let ((hz (anvil-host--num
                             (string-trim (plist-get (anvil-shell "sysctl -n hw.cpufrequency_max") :stdout)))))
                    (and hz (round (/ hz 1.0e6))))))

(defun anvil-host--info-cpu-linux ()
  (let* ((res (anvil-shell "cat /proc/cpuinfo 2>/dev/null" '(:max-output 65536)))
         (out (plist-get res :stdout))
         model physical logical mhz)
    (dolist (line (split-string out "\n" t))
      (cond
       ((and (not model) (string-match "\\`model name\\s-*:\\s-*\\(.*\\)" line))
        (setq model (string-trim (match-string 1 line))))
       ((and (not mhz) (string-match "\\`cpu MHz\\s-*:\\s-*\\([0-9.]+\\)" line))
        (setq mhz (round (string-to-number (match-string 1 line)))))))
    (setq logical (anvil-host--num
                   (string-trim (plist-get (anvil-shell "nproc 2>/dev/null") :stdout))))
    (setq physical (anvil-host--num
                    (string-trim
                     (plist-get (anvil-shell "lscpu 2>/dev/null | awk -F: '/^Core\\(s\\) per socket/ {c=$2} /^Socket\\(s\\)/ {s=$2} END {print c*s}'") :stdout))))
    (list :model model :physical physical :logical logical :mhz-max mhz)))

;; --- RAM ---

(defun anvil-host--info-ram-windows ()
  (let* ((res (anvil-shell
               "wmic os get TotalVisibleMemorySize,FreePhysicalMemory /value"
               '(:max-output 4096)))
         (rec (anvil-host--first-record (plist-get res :stdout)))
         (total-kb (anvil-host--num (plist-get rec :totalvisiblememorysize)))
         (free-kb  (anvil-host--num (plist-get rec :freephysicalmemory))))
    (list :total-gb (and total-kb (anvil-host--bytes->gb (* total-kb 1024)))
          :free-gb  (and free-kb  (anvil-host--bytes->gb (* free-kb  1024))))))

(defun anvil-host--info-ram-darwin ()
  (let* ((bytes (anvil-host--num
                 (string-trim (plist-get (anvil-shell "sysctl -n hw.memsize") :stdout)))))
    (list :total-gb (anvil-host--bytes->gb bytes)
          :free-gb  nil))) ; vm_stat parsing omitted; rarely needed

(defun anvil-host--info-ram-linux ()
  (let* ((out (plist-get (anvil-shell "cat /proc/meminfo 2>/dev/null") :stdout))
         total free)
    (dolist (line (split-string out "\n" t))
      (cond
       ((string-match "\\`MemTotal:\\s-+\\([0-9]+\\) kB" line)
        (setq total (string-to-number (match-string 1 line))))
       ((string-match "\\`MemAvailable:\\s-+\\([0-9]+\\) kB" line)
        (setq free  (string-to-number (match-string 1 line))))))
    (list :total-gb (and total (anvil-host--bytes->gb (* total 1024)))
          :free-gb  (and free  (anvil-host--bytes->gb (* free  1024))))))

;; --- DISK ---

(defun anvil-host--info-disk-windows ()
  (let* ((res (anvil-shell
               "wmic logicaldisk where DriveType=3 get Caption,Size,FreeSpace /value"
               '(:max-output 8192)))
         (recs (anvil-host--parse-wmic-value (plist-get res :stdout))))
    (mapcar (lambda (r)
              (let ((size (anvil-host--num (plist-get r :size)))
                    (free (anvil-host--num (plist-get r :freespace))))
                (list :drive (plist-get r :caption)
                      :total-gb (anvil-host--bytes->gb size)
                      :free-gb  (anvil-host--bytes->gb free))))
            recs)))

(defun anvil-host--info-disk-unix ()
  (let* ((res (anvil-shell "df -k -P 2>/dev/null"))
         (out (plist-get res :stdout))
         (lines (cdr (split-string out "\n" t))) ; drop header
         results)
    (dolist (line lines)
      (let ((fields (split-string line)))
        (when (>= (length fields) 6)
          (let ((total-kb (anvil-host--num (nth 1 fields)))
                (free-kb  (anvil-host--num (nth 3 fields)))
                (mount    (nth 5 fields)))
            ;; Skip pseudo filesystems
            (unless (string-match-p "\\`/\\(proc\\|sys\\|dev\\|run\\)" mount)
              (push (list :drive mount
                          :total-gb (and total-kb (anvil-host--bytes->gb (* total-kb 1024)))
                          :free-gb  (and free-kb  (anvil-host--bytes->gb (* free-kb  1024))))
                    results))))))
    (nreverse results)))

;; --- GPU ---

(defun anvil-host--info-gpu-windows ()
  (let* ((res (anvil-shell
               "wmic path Win32_VideoController get Name,AdapterRAM,DriverVersion /value"
               '(:max-output 8192)))
         (recs (anvil-host--parse-wmic-value (plist-get res :stdout))))
    (mapcar (lambda (r)
              (let ((vram (anvil-host--num (plist-get r :adapterram))))
                (list :name (plist-get r :name)
                      :vram-gb (and vram (anvil-host--bytes->gb vram))
                      :driver  (plist-get r :driverversion))))
            recs)))

(defun anvil-host--info-gpu-darwin ()
  (let* ((out (plist-get (anvil-shell "system_profiler SPDisplaysDataType 2>/dev/null"
                                       '(:max-output 8192))
                         :stdout))
         results name vram)
    (dolist (line (split-string out "\n" t))
      (cond
       ((string-match "Chipset Model:\\s-*\\(.*\\)" line)
        (when name
          (push (list :name name :vram-gb vram) results)
          (setq vram nil))
        (setq name (string-trim (match-string 1 line))))
       ((string-match "VRAM[^:]*:\\s-*\\([0-9]+\\)\\s-*\\(MB\\|GB\\)" line)
        (let ((n (string-to-number (match-string 1 line)))
              (u (match-string 2 line)))
          (setq vram (if (string= u "GB") (float n) (/ n 1024.0)))))))
    (when name (push (list :name name :vram-gb vram) results))
    (nreverse results)))

(defun anvil-host--info-gpu-linux ()
  (let* ((out (plist-get (anvil-shell "lspci 2>/dev/null | grep -iE 'vga|3d|display'") :stdout)))
    (mapcar (lambda (line)
              (list :name (replace-regexp-in-string
                           "\\`[^:]+:[^:]+:\\s-*" "" line)
                    :vram-gb nil :driver nil))
            (split-string out "\n" t))))

;; --- NETWORK ---

(defun anvil-host--info-net-windows ()
  (let* ((res (anvil-shell
               "wmic nicconfig where IPEnabled=true get IPAddress,DefaultIPGateway,Description /value"
               '(:max-output 8192)))
         (recs (anvil-host--parse-wmic-value (plist-get res :stdout))))
    (mapcar (lambda (r)
              (let* ((ips      (anvil-host--parse-wmic-array (plist-get r :ipaddress)))
                     (gateways (anvil-host--parse-wmic-array (plist-get r :defaultipgateway))))
                (list :iface    (plist-get r :description)
                      :ipv4     (anvil-host--first-ipv4 ips)
                      :ips      ips
                      :gateway  (or (anvil-host--first-ipv4 gateways) (car gateways)))))
            recs)))

(defun anvil-host--info-net-darwin ()
  (let* ((iface (string-trim (plist-get (anvil-shell "route get default 2>/dev/null | awk '/interface:/ {print $2}'") :stdout)))
         (ip    (string-trim (plist-get (anvil-shell (format "ipconfig getifaddr %s 2>/dev/null" iface)) :stdout)))
         (gw    (string-trim (plist-get (anvil-shell "route get default 2>/dev/null | awk '/gateway:/ {print $2}'") :stdout))))
    (list (list :iface iface :ip ip :gateway gw))))

(defun anvil-host--info-net-linux ()
  (let* ((iface (string-trim (plist-get (anvil-shell "ip route 2>/dev/null | awk '/^default/ {print $5; exit}'") :stdout)))
         (ip    (string-trim (plist-get (anvil-shell (format "ip -4 -o addr show %s 2>/dev/null | awk '{print $4}'" iface)) :stdout)))
         (gw    (string-trim (plist-get (anvil-shell "ip route 2>/dev/null | awk '/^default/ {print $3; exit}'") :stdout))))
    (list (list :iface iface :ip ip :gateway gw))))

;; --- UPTIME ---

(defun anvil-host--info-uptime-windows ()
  (let* ((res (anvil-shell "wmic os get LastBootUpTime /value" '(:max-output 1024)))
         (rec (anvil-host--first-record (plist-get res :stdout)))
         (boot (anvil-host--parse-wmi-datetime (plist-get rec :lastbootuptime))))
    (list :boot-time boot
          :uptime    (and boot
                          (let ((boot-time
                                 (encode-time
                                  (parse-time-string (replace-regexp-in-string "T" " " boot)))))
                            (round (- (float-time) (float-time boot-time))))))))

(defun anvil-host--info-uptime-unix ()
  (let* ((boot (string-trim (plist-get (anvil-shell "uptime -s 2>/dev/null") :stdout))))
    (list :boot-time (and (not (string-empty-p boot)) boot)
          :uptime nil)))

;; --- EMACS (no shell) ---

(defun anvil-host--info-emacs ()
  (list :version              emacs-version
        :system-configuration system-configuration
        :native-comp          (and (fboundp 'native-comp-available-p)
                                   (native-comp-available-p))
        :gui                  (display-graphic-p)
        :daemon               (daemonp)
        :init-file            user-init-file
        :user-emacs-directory user-emacs-directory))

;;;; --- Layer 2: anvil-host-info -------------------------------------------

(defconst anvil-host--all-categories
  '(os cpu ram disk gpu network uptime emacs)
  "Default category set for `anvil-host-info'.")

(defun anvil-host--dispatch (category)
  "Run the per-OS info function for CATEGORY. Returns plist or list."
  (pcase category
    ('os      (cond ((eq system-type 'windows-nt) (anvil-host--info-os-windows))
                    ((eq system-type 'darwin)     (anvil-host--info-os-darwin))
                    ((eq system-type 'gnu/linux)  (anvil-host--info-os-linux))))
    ('cpu     (cond ((eq system-type 'windows-nt) (anvil-host--info-cpu-windows))
                    ((eq system-type 'darwin)     (anvil-host--info-cpu-darwin))
                    ((eq system-type 'gnu/linux)  (anvil-host--info-cpu-linux))))
    ('ram     (cond ((eq system-type 'windows-nt) (anvil-host--info-ram-windows))
                    ((eq system-type 'darwin)     (anvil-host--info-ram-darwin))
                    ((eq system-type 'gnu/linux)  (anvil-host--info-ram-linux))))
    ('disk    (cond ((eq system-type 'windows-nt) (anvil-host--info-disk-windows))
                    (t                            (anvil-host--info-disk-unix))))
    ('gpu     (cond ((eq system-type 'windows-nt) (anvil-host--info-gpu-windows))
                    ((eq system-type 'darwin)     (anvil-host--info-gpu-darwin))
                    ((eq system-type 'gnu/linux)  (anvil-host--info-gpu-linux))))
    ('network (cond ((eq system-type 'windows-nt) (anvil-host--info-net-windows))
                    ((eq system-type 'darwin)     (anvil-host--info-net-darwin))
                    ((eq system-type 'gnu/linux)  (anvil-host--info-net-linux))))
    ('uptime  (cond ((eq system-type 'windows-nt) (anvil-host--info-uptime-windows))
                    (t                            (anvil-host--info-uptime-unix))))
    ('emacs   (anvil-host--info-emacs))
    (_ (error "anvil-host: unknown category: %s" category))))

(defun anvil-host-info (&optional categories)
  "Return host PC info as a nested plist.
CATEGORIES is a list of symbols (subset of `anvil-host--all-categories').
Default = all categories.

Each value is a plist (or list of plists for disk / gpu / network).
On per-category failure, the category value is (:error \"...\")."
  (let ((cats (or categories anvil-host--all-categories))
        result)
    (dolist (cat cats)
      (let ((val (condition-case err
                     (anvil-host--dispatch cat)
                   (error (list :error (error-message-string err))))))
        (setq result (plist-put result
                                (intern (concat ":" (symbol-name cat)))
                                val))))
    result))

;;;; --- Layer 1.5: which / env ---------------------------------------------

(defun anvil-host-which (cmd)
  "Return the absolute path of CMD on the host, or nil.
CMD can be:
  - a string  → returns string-or-nil
  - a list of strings → returns ((NAME . PATH-OR-NIL) ..)

Wraps `executable-find', which respects `exec-path' (the
Emacs-internal PATH) — useful for diagnosing PATH drift between
shell and Emacs."
  (cond
   ((stringp cmd) (executable-find cmd))
   ((listp cmd)
    (let (results)
      (dolist (c cmd)
        (push (cons c (executable-find c)) results))
      (nreverse results)))
   (t (error "anvil-host: which expects string or list, got %s"
             (type-of cmd)))))

(defun anvil-host-env (key-or-list)
  "Return the value of environment variable KEY, or nil.
KEY-OR-LIST can be:
  - a string → returns string-or-nil
  - a list of strings → returns ((NAME . VALUE-OR-NIL) ..)

Wraps `getenv', which reflects the Emacs-process environment
(may differ from a fresh shell if Emacs was started with a
restricted env)."
  (cond
   ((stringp key-or-list) (getenv key-or-list))
   ((listp key-or-list)
    (let (results)
      (dolist (k key-or-list)
        (push (cons k (getenv k)) results))
      (nreverse results)))
   (t (error "anvil-host: env expects string or list, got %s"
             (type-of key-or-list)))))

;;;; --- discoverability ----------------------------------------------------

(defun anvil-host-helpers-list ()
  "Return a list of all anvil-host* user-facing function names."
  (let (results)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (let ((n (symbol-name sym)))
                    (and (or (string-prefix-p "anvil-host" n)
                             (string= n "anvil-shell")
                             (string= n "anvil-shell-by-os"))
                         (not (string-match-p "--" n)))))
         (push sym results))))
    (sort results (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(provide 'anvil-host)
;;; anvil-host.el ends here
