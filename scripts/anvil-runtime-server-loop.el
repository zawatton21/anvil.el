;;; anvil-runtime-server-loop.el --- UNIX-socket MCP server for standalone NeLisp -*- lexical-binding: t; -*-

;; K2 (= 2026-05-11) — drop-in replacement for the Phase B5 stdio
;; shell-loop driver.  Instead of reading MCP Content-Length frames
;; from stdin and writing responses to stdout (which the FIFO bridge
;; daemon in G v1 had to shim with `dd bs=1' tricks), this loop uses
;; `make-network-process' from the new nelisp-emacs network stack
;; (= `src/emacs-network-ffi.el' + `src/emacs-process-events.el' +
;; `src/emacs-eventloop.el', shipped in nelisp-emacs main `b662671')
;; to bind a UNIX domain socket and dispatch each incoming connection
;; via a filter callback.
;;
;; Lifecycle:
;;   1. Cold-load the substrate (= same chain as `anvil-runtime-shell-loop')
;;   2. Bind a UNIX server socket at `ANVIL_RUNTIME_SOCKET'
;;   3. Enter `(while t (accept-process-output nil 0 1000))'
;;   4. Per-connection filter accumulates bytes, parses MCP frames,
;;      calls `anvil-server-process-jsonrpc', sends encoded response
;;      back via `process-send-string'
;;
;; Multi-bridge concurrency: each accepted child is a separate process
;; in the registry, each with its own buffered MCP parser state stored
;; per-fd in `anvil-mcp--state-by-fd'.  N MCP clients can connect at
;; once and their frames will not interleave — `make-network-process'
;; handles the multiplex.

;;; Code:

;; Pre-declare so diagnostic trace lines can gate on it before
;; `anvil-server.el' loads.  See shell-loop.el for the same pattern.
(defvar anvil-server--debug-trace nil
  "When non-nil, emit `[STDIO]/[PJ]/[VAD]/[JR]/[TL]/[REG-*]/[STEP]'
diagnostic trace lines via `nelisp--write-stderr-line'.  Default
nil for production (silent).")

;; NeLisp v1.2.0: restore anvil's documented standalone marker (see
;; shell-loop.el for the rationale).
(when (and (fboundp 'read-stdin-bytes) (not (featurep 'nelisp)))
  (provide 'nelisp))

(defun anvil-runtime-server--env (name default)
  (let ((val (and (fboundp 'getenv) (getenv name))))
    (if (and val (> (length val) 0)) val default)))

(defvar anvil-mcp--server-id "emacs-eval"
  "Server id the connection filter dispatches to.

A GLOBAL, deliberately.  Every `defun' below is written inside one big
`let*', so each is a closure over that `let*''s bindings -- and on the
NeLisp v1.2.x reader a closure invoked from the process pump does not
reliably see or write those captured bindings: measured 2026-09-04, a
`:filter' closure recording through a captured variable lost every
write, and mutating a captured cons took the process down, while the
same filter recording into a global hash table worked.  That is why the
daemon accepted connections and answered nothing.  Anything the filter
or sentinel needs at dispatch time is therefore read from a global.")

(defvar anvil-runtime-server--primitive-load nil
  "Standalone `load' function saved before stdlib-misc replaces it.")

(defvar anvil-runtime-server--primitive-hash-functions nil
  "Standalone hash functions saved before stdlib-misc replaces them.")

(defvar anvil-runtime-server--skip-load-files nil
  "Absolute source files intentionally skipped by the runtime loader.")

(defun anvil-runtime-server--absolute-file-name-p (file)
  "Return non-nil when FILE is an absolute name on this platform.
`file-name-absolute-p' is missing from some standalone images, and a
leading-slash test alone rejects the Windows reader's own paths: the
launcher hands the bootstrap `C:/...' names, so every absolute load fell
through to `locate-library' and failed (measured 2026-09-04, the daemon
died on emacs-init.el)."
  (and (stringp file)
       (> (length file) 0)
       (if (fboundp 'file-name-absolute-p)
           (file-name-absolute-p file)
         (or (eq (aref file 0) ?/)
             (eq (aref file 0) 92)
             (and (> (length file) 2)
                  (eq (aref file 1) ?:)
                  (or (eq (aref file 2) ?/) (eq (aref file 2) 92)))))))

(defun anvil-runtime-server--compat-load
    (file &optional noerror nomessage _nosuffix _must-suffix)
  "Load FILE with Emacs load context and the standalone native reader."
  (let ((resolved
         (if (anvil-runtime-server--absolute-file-name-p file)
             (cond
              ((file-exists-p file) file)
              ((file-exists-p (concat file ".el")) (concat file ".el"))
              (t nil))
           (locate-library file))))
    (if (null resolved)
        (if noerror nil
          (signal 'file-error (list "Cannot open load file" file)))
      (if (member resolved anvil-runtime-server--skip-load-files)
          t
        (let ((prior-lfn (and (boundp 'load-file-name) load-file-name))
              (prior-dd (and (boundp 'default-directory) default-directory))
              (value nil)
              (err nil))
          (setq load-file-name resolved)
          (setq default-directory (file-name-directory resolved))
          (condition-case caught
              (setq value
                    (funcall anvil-runtime-server--primitive-load
                             resolved noerror nomessage))
            (error (setq err caught)))
          (when (fboundp 'garbage-collect)
            (garbage-collect))
          (setq load-file-name prior-lfn)
          (setq default-directory prior-dd)
          (cond
           ((null err) value)
           (t (signal (car err) (cdr err)))))))))

;; Path resolution — same chain as shell-loop.el §path-resolution.
;; The primary channel is `anvil-runtime-bootstrap-{anvil-el,nelisp-emacs}-dir'
;; set by `bin/anvil-runtime' before loading us; `getenv' is unreliable
;; here because it's polyfilled by `emacs-callproc.el' which this file
;; loads itself via `(load init-el)' below.
(let* ((anvil-el-dir
        (or (and (boundp 'anvil-runtime-bootstrap-anvil-el-dir)
                 anvil-runtime-bootstrap-anvil-el-dir)
            (anvil-runtime-server--env
             "ANVIL_EL_DIR"
             (or (and (boundp 'load-file-name) load-file-name
                      (file-name-directory
                       (directory-file-name
                        (file-name-directory load-file-name))))
                 "/home/madblack-21/Cowork/Notes/dev/anvil.el"))))
       (nelisp-emacs-dir
        (or (and (boundp 'anvil-runtime-bootstrap-nelisp-emacs-dir)
                 anvil-runtime-bootstrap-nelisp-emacs-dir)
            (anvil-runtime-server--env
             "NELISP_EMACS_DIR"
             (concat (file-name-directory
                      (directory-file-name anvil-el-dir))
                     "nelisp-emacs"))))
       (nelisp-lisp-dir
        (or (and (boundp 'anvil-runtime-bootstrap-nelisp-lisp-dir)
                 (> (length anvil-runtime-bootstrap-nelisp-lisp-dir) 0)
                 anvil-runtime-bootstrap-nelisp-lisp-dir)
            (concat (file-name-directory (directory-file-name anvil-el-dir))
                    "nelisp/lisp")))
       (server-id
        (anvil-runtime-server--env "ANVIL_SERVER_ID" "emacs-eval"))
       ;; Publish it before any filter/sentinel closure can be called.
       (_srvid (setq anvil-mcp--server-id server-id))
       (socket-path
        ;; The launcher (`bin/anvil-runtime server PATH') writes a
        ;; bootstrap.el that `setq's `anvil-runtime-bootstrap-socket-path'
        ;; before loading us.  NeLisp Phase 1.6's `getenv' is stubbed to
        ;; nil so env-vars are not a viable channel.
        (or (and (boundp 'anvil-runtime-bootstrap-socket-path)
                 anvil-runtime-bootstrap-socket-path)
            (anvil-runtime-server--env
             "ANVIL_RUNTIME_SOCKET"
             "/tmp/anvil-runtime.sock")))
       ;; State dir (bin/anvil-runtime creates it); same role as in
       ;; shell-loop.el: temporary files and anvil-server's schema cache.
       (state-dir
        (or (and (boundp 'anvil-runtime-bootstrap-state-dir)
                 anvil-runtime-bootstrap-state-dir)
            (anvil-runtime-server--env "ANVIL_RUNTIME_DAEMON_DIR"
                                       "/tmp/anvil-runtime")))
       ;; NeLisp v1.2.0 (2026-09-04): the daemon listens on TCP loopback
       ;; through nelisp's own process adapter (`packages/nelisp-process-
       ;; adapter', Doc 194: `make-network-process :server t' over the
       ;; native `nelisp-socket-*' family, Linux and Windows alike).  The
       ;; K1 UNIX-socket stack (emacs-network-ffi / -process-events /
       ;; -eventloop) spoke the Rust-era `nl-ffi-call LIB FUNC SIG' contract
       ;; and cannot open a socket on this reader ("socket() failed:
       ;; errno=0", measured on WSL).  `bin/anvil-runtime server PORT'
       ;; sets `anvil-runtime-bootstrap-port'; a non-numeric argument keeps
       ;; the legacy socket-path + K1 path for readers that still have it.
       (port
        (or (and (boundp 'anvil-runtime-bootstrap-port)
                 anvil-runtime-bootstrap-port)
            (let ((env (anvil-runtime-server--env "ANVIL_RUNTIME_PORT" nil)))
              (and env (string-to-number env)))))
       (nelisp-home-dir
        (let ((lisp (and (boundp 'anvil-runtime-bootstrap-nelisp-lisp-dir)
                         anvil-runtime-bootstrap-nelisp-lisp-dir)))
          (and (stringp lisp) (> (length lisp) 0)
               (directory-file-name (file-name-directory (directory-file-name lisp))))))
       (adapter-core-el (and nelisp-home-dir
                             (concat nelisp-home-dir
                                     "/packages/nelisp-eventloop/src/nelisp-async-core.el")))
       (adapter-el (and nelisp-home-dir
                        (concat nelisp-home-dir
                                "/packages/nelisp-process-adapter/src/nelisp-process-adapter.el")))
       (init-el (concat nelisp-emacs-dir "/src/emacs-init.el"))
       (stub-el (concat nelisp-emacs-dir "/src/emacs-stub.el"))
       (network-ffi-el (concat nelisp-emacs-dir "/src/emacs-network-ffi.el"))
       (process-events-el (concat nelisp-emacs-dir
                                   "/src/emacs-process-events.el"))
       (eventloop-el (concat nelisp-emacs-dir "/src/emacs-eventloop.el"))
       (metrics-el (concat anvil-el-dir "/anvil-server-metrics.el"))
       (server-el (concat anvil-el-dir "/anvil-server.el"))
       (server-commands-el (concat anvil-el-dir "/anvil-server-commands.el"))
       ;; Resolved HERE, in the binding list, so it is read while `getenv' is
       ;; still the reader's own.  Read after `(load init-el)' it came back
       ;; nil and the daemon silently served the three-module default even
       ;; when ANVIL_TOOL_MODULES asked for six (measured 2026-09-04).
       ;; shell-loop.el resolves its copy pre-init for the same reason.
       (modules-env (anvil-runtime-server--env
                     "ANVIL_TOOL_MODULES"
                     "anvil-discovery,anvil-sqlite,anvil-bench"))
       (stdlib-misc (concat nelisp-lisp-dir "/nelisp-stdlib-misc.el"))
       (bootstrap-skip-features '(nelisp-coding-jis-tables calendar)))

  ;; --- substrate bootstrap (same as shell-loop.el) ---
  ;; emacs-init.el gates its vendor load-path setup on
  ;; `nelisp-emacs-vendor-root'.  Without this, `(require 'subr-x)' and
  ;; the chain `nelisp-coding' → `nelisp-emacs-compat-fileio' →
  ;; `emacs-fileio-builtins' all fail with "feature was not provided".
  (setq nelisp-emacs-vendor-root (concat nelisp-emacs-dir "/vendor"))
  ;; Pre-provide `nelisp-coding-jis-tables' to bypass its 460KB defconst
  ;; load — under the post-2026-05-17 nelisp (Doc 49 Wave 7–13 Phase 47
  ;; native swap), loading that file aborts the process with glibc
  ;; "double free or corruption (!prev)".  MCP / JSON-RPC traffic is
  ;; UTF-8 only and never exercises JIS codec paths, so the tables are
  ;; deadweight for the server-loop.  Remove once the nelisp regression
  ;; is fixed upstream.
  (provide 'nelisp-coding-jis-tables)
  ;; Same headless pre-provides as shell-loop.el (NeLisp v1.2.0): the
  ;; vendor libs the polyfills would otherwise walk, and the editor / UI
  ;; substrate a socket MCP server never opens.
  (dolist (lib '(subr-x seq cl-extra cl-seq benchmark profiler))
    (provide lib))
  (dolist (f '(emacs-syntax-table emacs-elisp-mode emacs-mode emacs-mode-builtins
               emacs-font-lock-builtins emacs-faces-builtins emacs-textmodes-stub
               emacs-redisplay-builtins emacs-tui-event emacs-tui-backend
               emacs-frame-builtins emacs-window-builtins emacs-keymap-builtins
               emacs-command-loop-builtins
               emacs-frame emacs-window keymap emacs-faces emacs-font-lock))
    (provide f))
  (setq load-prefer-newer t)
  (unless (boundp 'temporary-file-directory)
    (defvar temporary-file-directory (concat state-dir "/tmp/")))
  ;; The v1.2.0 reader's `load' never binds `load-file-name', so
  ;; emacs-init.el cannot put its own src/ on load-path; do it here.
  (let ((src-dir (concat nelisp-emacs-dir "/src")))
    (unless (and (boundp 'load-path) (member src-dir load-path))
      (setq load-path (cons src-dir (and (boundp 'load-path) load-path)))))
  ;; Load before emacs-init for the measured `load-file-name' behavior.
  ;; See shell-loop.el for the 2026-08-28 JSON timing and call-count data.
  ;; The stdlib-misc chain below is OPT-IN, default off.
  ;;
  ;; It was written for the pre-v1.2 runtime, where `load' left
  ;; `load-file-name' nil and stdlib-misc supplied the coding-system and
  ;; hash-traversal fixes.  NeLisp v1.2.1's prelude carries those fixes
  ;; itself, and loading stdlib-misc on top of it is not free: measured
  ;; 2026-09-04 on the windows-x86_64 reader, `(load init-el)' never
  ;; returned -- a 600 s stdio run stalled after `[STEP] pre-init' with
  ;; no output, where the same tree without this chain answered
  ;; `tools/list' normally.  Set ANVIL_RUNTIME_STDLIB_MISC=1 to restore
  ;; it for a runtime that still needs it.
  (when (and (boundp 'anvil-runtime-bootstrap-stdlib-misc)
             anvil-runtime-bootstrap-stdlib-misc
             (file-exists-p stdlib-misc))
    (setq anvil-runtime-server--primitive-load (symbol-function 'load))
    (setq anvil-runtime-server--primitive-hash-functions
          (mapcar (lambda (name) (cons name (symbol-function name)))
                  '(maphash hash-table-keys hash-table-values hash-table-count)))
    (load stdlib-misc nil t)
    ;; Retain the working native hash traversal functions; see shell-loop.el
    ;; for the measured missing-`nelisp--hash-pairs' failure.
    (let ((saved anvil-runtime-server--primitive-hash-functions))
      (while saved
        (fset (car (car saved)) (cdr (car saved)))
        (setq saved (cdr saved))))
    ;; Install the same 2026-08-29 measured native-reader compatibility
    ;; layer and exact vendor-calendar skip documented in shell-loop.el.
    (setq anvil-runtime-server--skip-load-files
          (list (concat nelisp-emacs-dir
                        "/vendor/emacs-lisp/calendar/calendar.el")))
    (fset 'load #'anvil-runtime-server--compat-load)
    )
  (dolist (feature bootstrap-skip-features)
    (provide feature))

  (load init-el nil t)
  (load stub-el nil t)

  ;; --- alist-get override (= Doc 98 §98.2 workaround), probe-gated ---
  ;; The old code loaded the whole nelisp-stdlib-misc.el for this, which
  ;; shell-loop.el measured as a ~300x json-read slowdown; inline the one
  ;; defun instead, and only when the running `alist-get' is wrong (the
  ;; v1.2.0 prelude's is correct).
  (unless (and (fboundp 'alist-get)
               (condition-case nil
                   (and (equal (alist-get 'b '((a . 1) (b . 2))) 2)
                        (eq (alist-get 'z '((a . 1)) 'dflt) 'dflt)
                        (equal (alist-get "k" '(("k" . 3)) nil nil #'equal) 3))
                 (error nil)))
    (defun alist-get (key alist &optional default _remove testfn)
      (let ((cur alist) (found nil) (result default))
        (while (and cur (not found))
          (let ((pair (car cur)))
            (cond
             ((not (consp pair)) (setq cur (cdr cur)))
             ((cond
               ((null testfn) (equal (car pair) key))
               ((eq testfn 'eq) (eq (car pair) key))
               ((eq testfn 'equal) (equal (car pair) key))
               ((or (eq testfn 'string=) (eq testfn 'string-equal))
                (and (stringp (car pair)) (stringp key) (equal (car pair) key)))
               (t (funcall testfn (car pair) key)))
              (setq result (cdr pair))
              (setq found t))
             (t (setq cur (cdr cur))))))
        result)))

  ;; Put `anvil-el-dir' on `load-path' so any `(require 'anvil-orchestrator-routing)'
  ;; / `(require 'anvil-orchestrator-presets)' / etc. inside the
  ;; tool-module files below resolve to siblings of `anvil-server.el'.
  ;; Without this `anvil-orchestrator.el' load/enable fails with
  ;; "Cannot open load file anvil-orchestrator-routing".
  (add-to-list 'load-path anvil-el-dir)

  (load metrics-el nil t)
  (load server-el nil t)
  (when (boundp 'anvil-server-schema-cache-file)
    (setq anvil-server-schema-cache-file
          (concat state-dir "/anvil-schema-cache.el")))
  (load server-commands-el nil t)
  ;; User config, as in shell-loop.el (machine-specific DB pins).
  (condition-case err
      (require 'anvil-config nil t)
    (error
     (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
       (nelisp--write-stderr-line
        (concat "[server-loop] anvil-config load ERR: " (format "%S" err))))))

  ;; --- network stack ---
  ;; TCP (v1.2.0): nelisp's process adapter.  Legacy (no port, or no
  ;; adapter on disk): the K1 UNIX-socket stack.
  (defvar anvil-runtime-server--tcp-p
    (and (integerp port) adapter-el (file-exists-p adapter-el))
    "Non-nil when the daemon listens on TCP through nelisp's adapter.")
  (if anvil-runtime-server--tcp-p
      (progn
        (load adapter-core-el nil t)
        (load adapter-el nil t)
        (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
          (nelisp--write-stderr-line
           (format "[server-loop] network stack: nelisp-process-adapter (TCP 127.0.0.1:%d)" port))))
    (load network-ffi-el nil t)
    (load process-events-el nil t)
    (load eventloop-el nil t))

  (defun anvil-mcp--conn-fd (proc)
    "Return the connection key (fd) for PROC on either network stack.
The adapter's process is a vector `[network-process NAME STATUS FD ...]'
and is recognised first: Layer 2's `process-id-fd' exists after
emacs-init and signals `processp' on it (measured on both hosts; it
killed the daemon right after bind)."
    (cond
     ((and (vectorp proc) (> (length proc) 3)
           (eq (aref proc 0) 'network-process)
           (integerp (aref proc 3)))
      (aref proc 3))
     ((fboundp 'process-id-fd)
      (condition-case nil (process-id-fd proc) (error nil)))
     (t nil)))

  (defun anvil-mcp--decode (s)
    "Return the wire bytes S as a decoded (multibyte) string for dispatch.
The parser above slices by byte counts, so only the extracted body is
decoded; see shell-loop.el's `anvil-runtime-shell--multibyte'."
    (cond
     ((not (stringp s)) s)
     ;; Prefer anvil-server's validating decoder (it also covers host
     ;; Emacs); fall back to the lenient path when it rejects the bytes,
     ;; so one malformed frame cannot take the daemon down.
     ((fboundp 'anvil-server--utf8-bytes-to-string)
      (condition-case nil
          (anvil-server--utf8-bytes-to-string s)
        (error (if (fboundp 'string-as-multibyte)
                   (funcall 'string-as-multibyte s)
                 s))))
     ((fboundp 'string-as-multibyte) (funcall 'string-as-multibyte s))
     (t s)))

  ;; --- shared polyfills (cl-loop / to-json-value / register-tools etc) ---
  ;; (migrated from nelisp-emacs/scripts/ to anvil.el/scripts/ 2026-05-14)
  (let ((polyfills-el (concat anvil-el-dir
                              "/scripts/anvil-runtime-polyfills.el")))
    (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
      (nelisp--write-stderr-line
       (concat "[server-loop] loading polyfills " polyfills-el)))
    (condition-case err
        (load polyfills-el nil t)
      (error
       (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
         (nelisp--write-stderr-line
          (concat "[server-loop] polyfills load ERR: "
                  (format "%S" err)))))))

  ;; --- `help-function-arglist' (= same as shell-loop) ---
  (defun help-function-arglist (function &optional _preserve-names)
    (let ((fn (if (symbolp function)
                  (symbol-function function)
                function)))
      (cond
       ((and (consp fn) (eq (car fn) 'closure))
        (car (cdr (cdr fn))))
       ((and (consp fn) (eq (car fn) 'lambda))
        (car (cdr fn)))
       ((and (consp fn) (eq (car fn) 'macro))
        (help-function-arglist (cdr fn) _preserve-names))
       ((and (vectorp fn) (>= (length fn) 1))
        (aref fn 0))
       (t nil))))

  ;; --- anvil-server framing helpers (same as shell-loop) ---
  (defun anvil-server--strip-trailing-cr (s)
    (if (and (stringp s)
             (> (length s) 0)
             (eq (aref s (1- (length s))) ?\r))
        (substring s 0 (1- (length s)))
      s))

  (defun anvil-server-mcp-utf8-byte-length (s)
    "Return the UTF-8 byte length of S.
The MCP `Content-Length' header / `send(2)' both want the byte
count of the UTF-8 encoding (NeLisp `length' returns char count
for multibyte strings).  Standalone NeLisp ships a fast C
primitive `string-bytes' that returns the byte count directly;
host Emacs has the same primitive natively.

Perf (2026-05-12): the prior pure-elisp impl walked every aref
inside an interpreted while loop.  On the 20 kB `tools/list'
response that was ~8 s per call — twice per request (once in
`anvil-server-mcp-frame-encode', once again deeper in
`emacs-network-ffi--send'), 16 s of overhead in the wire path.
`string-bytes' completes the same computation in microseconds."
    (if (fboundp 'string-bytes)
        (string-bytes s)
      ;; Fallback for runtimes without `string-bytes' (e.g. an old
      ;; NeLisp build before the primitive landed).
      (let ((n 0) (i 0) (len (length s)))
        (while (< i len)
          (let ((c (aref s i)))
            (cond
             ((< c #x80)    (setq n (1+ n)))
             ((< c #x800)   (setq n (+ n 2)))
             ((< c #x10000) (setq n (+ n 3)))
             (t             (setq n (+ n 4)))))
          (setq i (1+ i)))
        n)))

  (defun anvil-server-mcp-frame-encode (body)
    "Emit `Content-Length: N\r\n\r\nBODY'.
N is the UTF-8 byte length of BODY (= what the MCP wire expects).
Retained for callers that need a single framed string; the wire
path in this server-loop prefers `anvil-server-mcp-frame-send'
which avoids the BODY-sized concat."
    (let ((byte-len (anvil-server-mcp-utf8-byte-length body)))
      (concat "Content-Length: " (number-to-string byte-len) "\r\n\r\n" body)))

  (defun anvil-server-mcp-frame-send (proc body)
    "Send the MCP `Content-Length: N\\r\\n\\r\\nBODY' frame to PROC.

Perf (2026-05-12): `frame-encode' produces a single string by
concatenating the header + BODY.  In NeLisp standalone `concat'
on a 20 kB BODY takes ~3 s because the runtime walks/copies the
source byte-by-byte.  This split-send variant writes the header
first (tiny string), then BODY (the large existing string —
already in memory, no copy).  Saves the 3 s concat on every wire
response."
    (let* ((byte-len (anvil-server-mcp-utf8-byte-length body))
           (header (concat "Content-Length: "
                           (number-to-string byte-len)
                           "\r\n\r\n")))
      (process-send-string proc header)
      (process-send-string proc body)))

  (defun anvil-server-mcp-ndjson-send (proc body)
    "Send BODY followed by a newline to PROC (= newline-delimited JSON wire).

Claude Code 2.1.138+ emits bare JSON objects terminated by `\\n'
rather than the spec-defined `Content-Length:'-prefixed frame.  We
detect the dialect per connection (= first byte of the first chunk:
`{' -> ndjson, otherwise framed) and route responses through this
helper instead of `frame-send' for those clients."
    (process-send-string proc body)
    (process-send-string proc "\n"))

  ;; --- tool-module load chain (same as shell-loop default) ---
  (progn

    (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
      (nelisp--write-stderr-line
       (concat "[server-loop] ANVIL_TOOL_MODULES=" modules-env)))
    (when (> (length modules-env) 0)
      (dolist (name (split-string modules-env "," t))
        (let* ((trimmed (if (fboundp 'string-trim) (string-trim name) name))
               (file (concat anvil-el-dir "/" trimmed ".el"))
               (enable-sym (intern (concat trimmed "-enable"))))
          (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
            (nelisp--write-stderr-line (concat "[server-loop] loading " file)))
          (condition-case err
              (progn
                (load file nil t)
                (when (fboundp enable-sym) (funcall enable-sym)))
            (error
             (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
               (nelisp--write-stderr-line
                (concat "[server-loop] " trimmed " load/enable ERR: "
                        (format "%S" err))))))))))

  ;; --- post-load patches (= anvil-sqlite regex compat etc) ---
  (when (fboundp 'anvil-runtime-polyfills-apply-post-load-patches)
    (condition-case err
        (anvil-runtime-polyfills-apply-post-load-patches)
      (error
       (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
         (nelisp--write-stderr-line
          (concat "[server-loop] post-load patches ERR: "
                  (format "%S" err)))))))

  ;; --- per-connection MCP framing parser ---
  ;;
  ;; State plist per fd:
  ;;   :buffer   accumulated bytes not yet consumed
  ;;   :phase    `header' (waiting for `\r\n\r\n') or `body'
  ;;             (waiting for N body bytes)
  ;;   :body-len Content-Length value parsed from header
  ;;
  ;; We use a global hash keyed by fd rather than the process plist so
  ;; the parser is self-contained and survives any future change to
  ;; the process vector layout.

  (defvar anvil-mcp--state-by-fd (make-hash-table :test #'eql))

  (defun anvil-mcp--find-headers-end (s)
    "Return (IDX . LEN) where IDX is the start of the header/body
separator in S and LEN is its byte length, or nil when no
separator is present yet.

Accepts both `\\r\\n\\r\\n' (4-byte, MCP spec) and `\\n\\n' (2-byte,
LF-only).  Claude Code 2.1.122 emits LF-only termination while
spec-compliant clients (= older Claude Code, Cursor, others) emit
CRLF; supporting both lets the same daemon serve every MCP host."
    (let ((n (length s))
          (i 0)
          (found nil))
      (while (and (not found) (< i n))
        (cond
         ;; CRLFCRLF (4-byte, MCP spec).
         ((and (< (+ i 3) n)
               (eq (aref s i) ?\r)
               (eq (aref s (+ i 1)) ?\n)
               (eq (aref s (+ i 2)) ?\r)
               (eq (aref s (+ i 3)) ?\n))
          (setq found (cons i 4)))
         ;; LFLF (2-byte, Claude Code 2.1.122 dialect).
         ((and (< (+ i 1) n)
               (eq (aref s i) ?\n)
               (eq (aref s (+ i 1)) ?\n))
          (setq found (cons i 2))))
        (setq i (1+ i)))
      found))

  ;; Backwards-compatible alias for any old caller that still expects
  ;; the CRLF-only signature (returns plain INDEX).  Reuses the new
  ;; finder so we only have one separator-scan implementation.
  (defun anvil-mcp--find-crlfcrlf (s)
    "Return index of \\r\\n\\r\\n (or \\n\\n) in S, or nil."
    (let ((hit (anvil-mcp--find-headers-end s)))
      (and hit (car hit))))

  (defun anvil-mcp--parse-content-length (header-block)
    "Walk HEADER-BLOCK line-by-line, return the Content-Length value
or nil.  Case-insensitive prefix match.  Accepts both CRLF and
LF-only line endings to match the separator dialects handled by
`anvil-mcp--find-headers-end'."
    (let ((lines (split-string header-block "\r?\n"))
          (found nil))
      (while (and lines (not found))
        (let* ((line (anvil-server--strip-trailing-cr (car lines)))
               (line-down (downcase line))
               (prefix "content-length:"))
          (when (and (>= (length line-down) (length prefix))
                     (string= (substring line-down 0 (length prefix))
                              prefix))
            (let* ((rest (substring line (length prefix)))
                   (i 0)
                   (n (length rest)))
              (while (and (< i n)
                          (let ((c (aref rest i)))
                            (or (eq c 32) (eq c ?\t))))
                (setq i (1+ i)))
              (let* ((num-start i)
                     (num-end i))
                (while (and (< num-end n)
                            (let ((c (aref rest num-end)))
                              (and (>= c ?0) (<= c ?9))))
                  (setq num-end (1+ num-end)))
                (when (> num-end num-start)
                  (setq found (string-to-number
                               (substring rest num-start num-end))))))))
        (setq lines (cdr lines)))
      found))

  (defun anvil-mcp--find-lf (s)
    "Return index of the first `\\n' in S, or nil."
    (let ((n (length s))
          (i 0)
          (found nil))
      (while (and (not found) (< i n))
        (when (eq (aref s i) ?\n)
          (setq found i))
        (setq i (1+ i)))
      found))

  (defun anvil-mcp--send-response (proc response mode)
    "Write RESPONSE back to PROC using MODE (`framed' or `ndjson')."
    (cond
     ((eq mode 'ndjson)
      (anvil-server-mcp-ndjson-send proc response))
     (t
      (anvil-server-mcp-frame-send proc response))))

  (defun anvil-mcp-filter (proc chunk)
    "Per-connection entry: `anvil-mcp--filter-1' behind a guard.
A per-connection error (a bad frame, a handler that signals past
anvil-server's own wrapper) must never unwind the event loop and take
the whole daemon down with every other client."
    (condition-case err
        (anvil-mcp--filter-1 proc chunk)
      (error
       (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
         (nelisp--write-stderr-line
          (format "[mcp-filter] fd=%S ERR %S" (anvil-mcp--conn-fd proc) err))))))

  (defun anvil-mcp--filter-1 (proc chunk)
    "Per-connection MCP frame parser.  Accumulates CHUNK, extracts
zero or more complete frames, dispatches each via
`anvil-server-process-jsonrpc', and writes the response back via
`anvil-server-mcp-frame-send' (Content-Length: framed) or
`anvil-server-mcp-ndjson-send' (bare JSON + `\\n').

Wire-format detection (= per-connection, latched on first chunk):
- `Content-Length: ...\\r\\n\\r\\nBODY' framed (= LSP-style MCP spec)
- `{...JSON...}\\n' newline-delimited JSON (= Claude Code 2.1.138+)
The first non-empty buffer's leading byte picks the dialect:
`{' -> `:mode ndjson', anything else -> `:mode framed'.
Responses go back in the matching wire format."
    (let* ((fd (anvil-mcp--conn-fd proc))
           (state (or (gethash fd anvil-mcp--state-by-fd)
                      (puthash fd
                               (list :buffer ""
                                     :phase 'header
                                     :body-len 0
                                     :mode nil)
                               anvil-mcp--state-by-fd))))
      (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
        (nelisp--write-stderr-line
         (format "[mcp-filter] fd=%d chunk=%d bytes phase=%S buflen-before=%d"
                 fd (length chunk) (plist-get state :phase)
                 (length (plist-get state :buffer)))))
      (plist-put state :buffer
                 (concat (plist-get state :buffer) chunk))
      ;; Latch the wire dialect on the first non-empty buffer.
      (when (and (null (plist-get state :mode))
                 (> (length (plist-get state :buffer)) 0))
        (let ((leader (aref (plist-get state :buffer) 0)))
          (cond
           ((eq leader ?{)
            (plist-put state :mode 'ndjson)
            (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
              (nelisp--write-stderr-line
               (format "[mcp-filter] fd=%d wire-format=ndjson" fd))))
           (t
            (plist-put state :mode 'framed)
            (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
              (nelisp--write-stderr-line
               (format "[mcp-filter] fd=%d wire-format=framed" fd)))))))
      (let ((keep-draining t))
        (while keep-draining
          (setq keep-draining nil)
          (cond
           ;; ── NDJSON path ─────────────────────────────────────────
           ((eq (plist-get state :mode) 'ndjson)
            (let* ((buf (plist-get state :buffer))
                   (lf  (anvil-mcp--find-lf buf)))
              (when lf
                (let ((body (substring buf 0 lf))
                      (rest (substring buf (1+ lf))))
                  (plist-put state :buffer rest)
                  (when (and (> (length body) 0))
                    (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
                      (nelisp--write-stderr-line
                       (format "[mcp-filter] fd=%d dispatch body[%d]: %S"
                               fd (length body)
                               (substring body 0 (min 120 (length body))))))
                    (let ((response
                           (condition-case err
                               (anvil-server-process-jsonrpc (anvil-mcp--decode body) anvil-mcp--server-id)
                             (error
                              (format
                               "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32603,\"message\":\"Internal error: %s\"}}"
                               (replace-regexp-in-string
                                "\"" "\\\\\""
                                (format "%S" err)))))))
                      (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
                        (nelisp--write-stderr-line
                         (format "[mcp-filter] fd=%d response stringp=%S len=%d"
                                 fd (stringp response)
                                 (if (stringp response) (length response) -1))))
                      (when (and (stringp response) (> (length response) 0))
                        (anvil-mcp--send-response proc response 'ndjson))))
                  (setq keep-draining t)))))
           ;; ── Framed path ─────────────────────────────────────────
           ((eq (plist-get state :phase) 'header)
            (let* ((buf (plist-get state :buffer))
                   (hit (anvil-mcp--find-headers-end buf)))
              (when hit
                (let* ((idx (car hit))
                       (sep-len (cdr hit))
                       (header (substring buf 0 idx))
                       (rest (substring buf (+ idx sep-len)))
                       (n (anvil-mcp--parse-content-length header)))
                  (cond
                   ((and (integerp n) (>= n 0))
                    (plist-put state :phase 'body)
                    (plist-put state :body-len n)
                    (plist-put state :buffer rest)
                    (setq keep-draining t))
                   (t
                    ;; Malformed header — drop everything, hope to resync
                    (plist-put state :buffer "")
                    (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
                      (nelisp--write-stderr-line
                       (format "[server-loop] bad header on fd=%d, dropping buffer"
                               fd)))))))))
           ((eq (plist-get state :phase) 'body)
            (let* ((buf (plist-get state :buffer))
                   (n (plist-get state :body-len)))
              (when (>= (length buf) n)
                (let ((body (substring buf 0 n))
                      (rest (substring buf n)))
                  (plist-put state :phase 'header)
                  (plist-put state :body-len 0)
                  (plist-put state :buffer rest)
                  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
                    (nelisp--write-stderr-line
                     (format "[mcp-filter] fd=%d dispatch body[%d]: %S"
                             fd n
                             (substring body 0 (min 120 (length body))))))
                  (let ((response
                         (condition-case err
                             (anvil-server-process-jsonrpc (anvil-mcp--decode body) anvil-mcp--server-id)
                           (error
                            (format
                             "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32603,\"message\":\"Internal error: %s\"}}"
                             (replace-regexp-in-string
                              "\"" "\\\\\""
                              (format "%S" err)))))))
                    (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
                      (nelisp--write-stderr-line
                       (format "[mcp-filter] fd=%d response stringp=%S len=%d"
                               fd (stringp response)
                               (if (stringp response) (length response) -1))))
                    (when (and (stringp response) (> (length response) 0))
                      (anvil-mcp--send-response proc response 'framed)))
                  (setq keep-draining t)))))
           (t nil))))))

  (defun anvil-mcp-sentinel (proc msg)
    "Clean up per-connection state when a child closes.
`process-name' is deliberately not called: the reader's own one
signals `processp' on the adapter's network-process vector (measured on
WSL, and it took the daemon down at the first disconnect)."
    (condition-case err
        (progn
          (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
            (nelisp--write-stderr-line
             (format "[server-loop] sentinel fd=%S: %s"
                     (anvil-mcp--conn-fd proc)
                     (if (stringp msg) (replace-regexp-in-string "\n" "" msg) msg))))
          ;; Drop the parser state when the connection ends.
          (let ((fd (anvil-mcp--conn-fd proc)))
            (when (and fd (integerp fd))
              (remhash fd anvil-mcp--state-by-fd))))
      (error
       (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
         (nelisp--write-stderr-line
          (format "[server-loop] sentinel ERR %S" err))))))

  ;; `anvil-server-process-jsonrpc' rejects requests when no MCP
  ;; server is active; `anvil-server-run-batch-stdio' (= the legacy
  ;; stdio loop) starts one as a side effect, but our event loop does
  ;; not call it.  Start the server explicitly so the active-server
  ;; gate is satisfied for the dispatch path.
  (when (fboundp 'anvil-server-start)
    (condition-case err
        (anvil-server-start)
      (error
       (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
         (nelisp--write-stderr-line
          (concat "[server-loop] anvil-server-start ERR: "
                  (format "%S" err)))))))

  ;; --- bind listener + enter event loop ---
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (let ((bucket (and (boundp 'anvil-server--tools)
                       (gethash server-id anvil-server--tools))))
      (nelisp--write-stderr-line
       (concat "[server-loop] pre-loop registry keys="
               (if bucket
                   (format "%S" (hash-table-keys bucket))
                 "<no-bucket>")))))

  ;; Perf (2026-05-11): pre-warm the `tools/list' response cache so the
  ;; first incoming client request returns instantly instead of paying
  ;; the full ~35 s `json-encode' cost (Claude Code's /mcp handshake
  ;; has a 30 s timeout — a cold tools/list would blow past it).  The
  ;; cache lives in `anvil-server--tools-list-cache' (added in
  ;; anvil-server.el alongside `anvil-server--handle-tools-list').
  ;; tools/list pre-warm is intentionally disabled under the
  ;; post-2026-05-17 nelisp runtime — the pure-elisp `json-encode'
  ;; from Doc 49 Wave 7-13 burns ~9.5 GB RSS and 100 % CPU for many
  ;; minutes without producing output, blocking socket bind.  Allow
  ;; opt-in via ANVIL_TOOLS_LIST_PREWARM=1 once the regression is
  ;; addressed upstream.  First incoming `tools/list' pays the cold
  ;; encode cost; MCP clients with > 60 s handshake timeout cope.
  (when (and (anvil-runtime-server--env "ANVIL_TOOLS_LIST_PREWARM" nil)
             (fboundp 'anvil-server--handle-tools-list)
             (boundp 'anvil-server--tools-list-cache))
    (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
      (nelisp--write-stderr-line
       "[server-loop] pre-warming tools/list cache"))
    (condition-case err
        (let ((unused (anvil-server--handle-tools-list 0 server-id)))
          (ignore unused)
          (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
            (nelisp--write-stderr-line
             (format "[server-loop] tools/list cache pre-warmed (%d bytes)"
                     (length (or (gethash server-id
                                          anvil-server--tools-list-cache)
                                 ""))))))
      (error
       (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
         (nelisp--write-stderr-line
          (concat "[server-loop] tools/list pre-warm ERR: "
                  (format "%S" err)))))))

  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line
     (if anvil-runtime-server--tcp-p
         (format "[server-loop] binding TCP 127.0.0.1:%d" port)
       (concat "[server-loop] binding socket " socket-path))))

  ;; Ensure parent dir exists for the socket file (legacy UNIX-socket path).
  (unless anvil-runtime-server--tcp-p
    (let ((dir (file-name-directory socket-path)))
      (when (and dir (not (file-directory-p dir)))
        (make-directory dir t))))

  (let ((server
         (if anvil-runtime-server--tcp-p
             (make-network-process
              :name "anvil-runtime-server"
              :host "127.0.0.1"
              :service port
              :server t
              :filter #'anvil-mcp-filter
              :sentinel #'anvil-mcp-sentinel)
           (make-network-process
            :name "anvil-runtime-server"
            :family 'local
            :service socket-path
            :server t
            :filter #'anvil-mcp-filter
            :sentinel #'anvil-mcp-sentinel))))
    (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
      (nelisp--write-stderr-line
       (format "[server-loop] listening on %s fd=%S"
               (if anvil-runtime-server--tcp-p
                   (format "127.0.0.1:%d" port)
                 socket-path)
               (anvil-mcp--conn-fd server)))))

  ;; Main loop — block in poll for up to 1 second per iteration so
  ;; SIGINT / SIGTERM can interrupt promptly.
  (while t
    (accept-process-output nil 1 0)))

;;; anvil-runtime-server-loop.el ends here
