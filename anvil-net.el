;;; anvil-net.el --- Network inspection for anvil -*- lexical-binding: t; -*-

;; Author: zawatton21
;; Keywords: tools, mcp, claude, network

;;; Commentary:

;; anvil-host.el の姉妹ファイル。Claude Code が emacs-eval 経由で
;; ホスト PC の listening port を OS 非依存に問い合わせるための
;; helper 群。
;;
;; 構成:
;;   Layer 1 — `anvil-shell' (anvil-host が所有、ここでは使うだけ)
;;   Layer 2 — `anvil-net-listening' / `anvil-net-port-listen' /
;;             `anvil-net-by-pid'
;;
;; 戻り値: socket plist
;;   (:port N :protocol SYM :pid N :process STR :address STR :state STR)
;;
;; per-OS バックエンド:
;;   Windows  netstat -ano                 (LISTENING TCP / bound UDP)
;;   macOS    lsof -nP -iTCP -sTCP:LISTEN  + lsof -nP -iUDP
;;   Linux    ss -tlnp + ss -ulnp          (fallback: netstat -tunlp)
;;
;; API リファレンス: ~/Cowork/Notes/.claude/anvil-helpers-reference.org

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'anvil-host)

;;;; --- internal: address parser -------------------------------------------

(defun anvil-net--split-addr-port (s)
  "Split S into a (ADDR . PORT) cons.
Handles IPv6 bracket form \"[::]:135\", IPv4 \"0.0.0.0:135\",
wildcard \"*:135\", and lsof's \"localhost:22\".
Returns (S . nil) if no port can be extracted."
  (cond
   ((null s) (cons nil nil))
   ;; [ipv6]:port
   ((string-match "\\`\\[\\(.*\\)\\]:\\([0-9]+\\)\\'" s)
    (cons (match-string 1 s)
          (string-to-number (match-string 2 s))))
   ;; addr:port (greedy on .* lets host:host:port for v6 fall through to nil,
   ;; which is fine — bare v6 without brackets is not used by netstat/ss/lsof
   ;; in listen output)
   ((string-match "\\`\\(.*\\):\\([0-9]+\\)\\'" s)
    (cons (match-string 1 s)
          (string-to-number (match-string 2 s))))
   (t (cons s nil))))

;;;; --- internal: per-OS parsers -------------------------------------------

(defun anvil-net--parse-netstat-windows (output)
  "Parse Windows `netstat -ano' OUTPUT into a list of socket plists.
Only LISTENING TCP and bound UDP are returned. Header lines are
skipped automatically because they fail the proto regex.

Note: Japanese Windows localizes header text (\"アクティブな接続\")
but TCP/UDP/LISTENING and the data columns themselves stay ASCII."
  (let (results)
    (dolist (raw (split-string output "\n" t))
      (let ((line (string-trim raw)))
        (cond
         ;; TCP local foreign STATE pid
         ((string-match
           "\\`TCP\\s-+\\(\\S-+\\)\\s-+\\S-+\\s-+\\(\\S-+\\)\\s-+\\([0-9]+\\)\\'"
           line)
          (let ((local (match-string 1 line))
                (state (match-string 2 line))
                (pid   (string-to-number (match-string 3 line))))
            (when (string= state "LISTENING")
              (let ((parsed (anvil-net--split-addr-port local)))
                (push (list :port (cdr parsed)
                            :protocol 'tcp
                            :pid pid
                            :process nil
                            :address (car parsed)
                            :state "LISTEN")
                      results)))))
         ;; UDP local foreign pid    (no state column)
         ((string-match
           "\\`UDP\\s-+\\(\\S-+\\)\\s-+\\S-+\\s-+\\([0-9]+\\)\\'"
           line)
          (let ((local (match-string 1 line))
                (pid   (string-to-number (match-string 2 line))))
            (let ((parsed (anvil-net--split-addr-port local)))
              (push (list :port (cdr parsed)
                          :protocol 'udp
                          :pid pid
                          :process nil
                          :address (car parsed)
                          :state nil)
                    results)))))))
    (nreverse results)))

(defun anvil-net--parse-lsof (output protocol)
  "Parse `lsof -i' OUTPUT into a list of plists, all tagged as PROTOCOL.
Header line (COMMAND PID USER ...) is skipped. The NAME column is
the last whitespace-delimited field (with optional trailing
\"(LISTEN)\")."
  (let (results)
    (dolist (raw (cdr (split-string output "\n" t)))
      (let ((fields (split-string raw nil t)))
        (when (>= (length fields) 9)
          (let* ((cmd  (nth 0 fields))
                 (pid  (string-to-number (nth 1 fields)))
                 (rest (nthcdr 8 fields))
                 (joined (string-join rest " "))
                 addr-port state)
            (if (string-match "\\`\\(.*?\\)\\s-+(\\([^)]*\\))\\s-*\\'" joined)
                (setq addr-port (match-string 1 joined)
                      state     (match-string 2 joined))
              (setq addr-port (string-trim joined)))
            (let* ((parsed (anvil-net--split-addr-port addr-port))
                   (port   (cdr parsed))
                   (addr   (car parsed)))
              (when port
                (push (list :port port
                            :protocol protocol
                            :pid pid
                            :process cmd
                            :address addr
                            :state state)
                      results)))))))
    (nreverse results)))

(defun anvil-net--parse-ss-linux (output protocol)
  "Parse `ss -tlnp' / `ss -ulnp' OUTPUT into a list of plists.
PROTOCOL ('tcp / 'udp) is stamped onto every record. Header line
is skipped. Process info comes from the trailing
\"users:((\\\"name\\\",pid=N,fd=...))\" column when present."
  (let (results)
    (dolist (raw (cdr (split-string output "\n" t)))
      (let ((fields (split-string raw nil t)))
        (when (>= (length fields) 4)
          (let* ((state (nth 0 fields))
                 ;; ss columns: State Recv-Q Send-Q Local Peer [Process]
                 (local (nth 3 fields))
                 (proc-field (nth 5 fields))
                 (parsed (anvil-net--split-addr-port local))
                 (addr (car parsed))
                 (port (cdr parsed))
                 process pid)
            (when (and proc-field
                       (string-match "(\"\\([^\"]+\\)\",pid=\\([0-9]+\\)" proc-field))
              (setq process (match-string 1 proc-field)
                    pid     (string-to-number (match-string 2 proc-field))))
            (when port
              (push (list :port port
                          :protocol protocol
                          :pid pid
                          :process process
                          :address addr
                          :state state)
                    results))))))
    (nreverse results)))

;;;; --- internal: per-OS gather --------------------------------------------

(defun anvil-net--gather-windows ()
  (let ((res (anvil-shell "netstat -ano" '(:max-output 524288))))
    (anvil-net--parse-netstat-windows (plist-get res :stdout))))

(defun anvil-net--gather-darwin ()
  (let ((tcp (plist-get (anvil-shell "lsof -nP -iTCP -sTCP:LISTEN"
                                     '(:max-output 131072))
                        :stdout))
        (udp (plist-get (anvil-shell "lsof -nP -iUDP"
                                     '(:max-output 131072))
                        :stdout)))
    (append (anvil-net--parse-lsof tcp 'tcp)
            (anvil-net--parse-lsof udp 'udp))))

(defun anvil-net--gather-linux ()
  (let ((tcp (plist-get (anvil-shell "ss -tlnp 2>/dev/null"
                                     '(:max-output 131072))
                        :stdout))
        (udp (plist-get (anvil-shell "ss -ulnp 2>/dev/null"
                                     '(:max-output 131072))
                        :stdout)))
    (append (anvil-net--parse-ss-linux tcp 'tcp)
            (anvil-net--parse-ss-linux udp 'udp))))

(defun anvil-net--gather ()
  "Return all listening sockets on this host as a list of plists."
  (cond ((eq system-type 'windows-nt) (anvil-net--gather-windows))
        ((eq system-type 'darwin)     (anvil-net--gather-darwin))
        ((eq system-type 'gnu/linux)  (anvil-net--gather-linux))
        (t (error "anvil-net: unsupported system-type: %s" system-type))))

;;;; --- internal: filter ---------------------------------------------------

(defun anvil-net--apply-filter (sockets filter)
  "Filter SOCKETS by FILTER plist. See `anvil-net-listening' for keys."
  (let ((proto    (plist-get filter :protocol))
        (pid      (plist-get filter :pid))
        (process  (plist-get filter :process))
        (port-min (plist-get filter :port-min))
        (port-max (plist-get filter :port-max)))
    (cl-remove-if-not
     (lambda (s)
       (and (or (null proto)    (eq proto (plist-get s :protocol)))
            (or (null pid)      (eql pid  (plist-get s :pid)))
            (or (null process)  (and (plist-get s :process)
                                     (string-match-p process (plist-get s :process))))
            (or (null port-min) (and (plist-get s :port)
                                     (>= (plist-get s :port) port-min)))
            (or (null port-max) (and (plist-get s :port)
                                     (<= (plist-get s :port) port-max)))))
     sockets)))

;;;; --- public API ---------------------------------------------------------

(defun anvil-net-listening (&optional filter)
  "Return all listening sockets on this host as a list of plists.

FILTER is an optional plist:
  :protocol  'tcp | 'udp
  :pid       integer
  :process   regexp matched against the process name (when known)
  :port-min  integer (inclusive lower bound)
  :port-max  integer (inclusive upper bound)

Each element:
  (:port N :protocol SYM :pid N :process STR-or-nil
   :address STR :state STR-or-nil)

The :process field is nil on Windows (use `anvil-proc-info' to
resolve PID → name) and populated on macOS / Linux."
  (anvil-net--apply-filter (anvil-net--gather) filter))

(defun anvil-net-port-listen (port &optional protocol)
  "Return the first socket plist listening on PORT, or nil.
PROTOCOL: 'tcp (default), 'udp, or 'any."
  (let ((proto (or protocol 'tcp)))
    (cl-find-if (lambda (s)
                  (and (eql (plist-get s :port) port)
                       (or (eq proto 'any)
                           (eq (plist-get s :protocol) proto))))
                (anvil-net--gather))))

(defun anvil-net-by-pid (pid)
  "Return all listening sockets owned by PID as a list of plists."
  (cl-remove-if-not (lambda (s) (eql (plist-get s :pid) pid))
                    (anvil-net--gather)))

;;;; --- discoverability ----------------------------------------------------

(defun anvil-net-helpers-list ()
  "Return a list of all anvil-net* user-facing function names."
  (let (results)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (let ((n (symbol-name sym)))
                    (and (string-prefix-p "anvil-net" n)
                         (not (string-match-p "--" n)))))
         (push sym results))))
    (sort results (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(provide 'anvil-net)
;;; anvil-net.el ends here
