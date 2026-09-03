;;; anvil-runtime-shell-loop.el --- nelisp-shell replacement for bin/anvil-runtime  -*- lexical-binding: t; -*-

;; Phase B5 Final B Stage 1 (= 2026-05-09)
;;
;; Doc anvil-runtime pure-elisp roadmap: replace the Rust crate
;; `anvil-runtime' (= 5,667 LOC) with a shell launcher that runs this
;; file under standalone NeLisp.  This file:
;;
;;   1. Loads the L2 Emacs C-primitive shims (`emacs-init', `emacs-stub')
;;      and the json / backquote / cl-defstruct fixes shipped in
;;      Phase B2-B5.
;;   2. Loads the anvil-server.el + anvil-server-commands.el +
;;      anvil-server-metrics.el modules from the user's anvil.el
;;      checkout.
;;   3. Activates the `read-from-minibuffer' shim (= line-buffered
;;      stdin from `read-stdin-bytes' bytes) so anvil-server's MCP
;;      Content-Length frame reader has a working line source.
;;   4. Calls `anvil-server-start' to install the active-server
;;      registry then enters `anvil-server-run-batch-stdio' which
;;      blocks reading frames until EOF.
;;
;; Configuration via env vars (matches Rust binary semantics):
;;   ANVIL_EL_DIR — directory containing anvil-server*.el (required;
;;                  default = $HOME/.emacs.d/external-packages/anvil.el).
;;   ANVIL_SERVER_ID — server-id argument passed to `anvil-server-start'
;;                     and `anvil-server-run-batch-stdio' (default
;;                     = "default").
;;   NELISP_EMACS_DIR — directory containing this file's siblings
;;                      `src/emacs-init.el' / `src/emacs-stub.el'
;;                      (= the nelisp-emacs checkout root).
;;   ANVIL_RUNTIME_DAEMON_DIR — state dir for the fast-handshake cache
;;                      and anvil-server's schema cache (bootstrap
;;                      `anvil-runtime-bootstrap-state-dir' wins).
;;
;; The Rust crate `anvil-runtime/' was deleted in Final B Stage 2
;; (2026-05-10); since NeLisp v1.2.0 (2026-09-04) the reader this file
;; runs under is the pure-elisp standalone binary (`target/nelisp' /
;; `target/nelisp.exe', started as `nelisp --load BOOTSTRAP' by
;; bin/anvil-runtime).  Everything below keeps working on both the
;; legacy and the v1.2.0 reader: overrides that the newer prelude made
;; redundant are gated on a functional probe rather than removed.

;;; Code:

;; Pre-declare so `[STEP]' diagnostic trace lines below can gate on it
;; before `anvil-server.el' is loaded.  `defvar' is a no-op when the
;; variable is already bound, so `anvil-server.el's own `defvar' just
;; documents the same name later in the chain.
(defvar anvil-server--debug-trace nil
  "When non-nil, emit `[STDIO]/[PJ]/[VAD]/[JR]/[TL]/[REG-*]/[STEP]'
diagnostic trace lines via `nelisp--write-stderr-line'.  Default
nil for production (silent).")

;; NeLisp v1.2.0: the pure-elisp standalone reader does not `(provide
;; 'nelisp)' the way the retired Rust bootstrap did, yet anvil's runtime
;; detection (`anvil-config-active-p', README "NeLisp standalone" config
;; path) is contractually `(featurep 'nelisp)'.  Restore the marker here,
;; gated on the stdin primitive only the standalone reader ships, so host
;; Emacs loading this file for tests stays untouched.
(when (and (fboundp 'read-stdin-bytes) (not (featurep 'nelisp)))
  (provide 'nelisp))

(defun anvil-runtime-shell--env (name default)
  (let ((val (and (fboundp 'getenv) (getenv name))))
    (if (and val (> (length val) 0)) val default)))

(defvar anvil-runtime-shell--fast-stdin-buffer ""
  "Unread stdin bytes captured by the fast MCP handshake path.")

(defvar anvil-runtime-shell--fast-pending-body nil
  "JSON-RPC body already read by the fast path but not handled there.")

(defvar anvil-runtime-shell--fast-trace nil
  "When non-nil, trace the pre-init fast MCP handshake path.")

(defun anvil-runtime-shell--fast-log (s)
  "Write fast handshake trace S to stderr when tracing is enabled."
  (when (and anvil-runtime-shell--fast-trace
             (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line s)))

(defun anvil-runtime-shell--plist-get (plist prop)
  "Small `plist-get' replacement available before `emacs-init.el'."
  (let ((cur plist)
        (found nil)
        (value nil))
    (while (and cur (not found))
      (if (eq (car cur) prop)
          (progn
            (setq found t)
            (setq value (car (cdr cur))))
        (setq cur (cdr (cdr cur)))))
    value))

(defun anvil-runtime-shell--substr-pos (s needle)
  "Return the first position of NEEDLE in S, or nil."
  (let* ((s-len (length s))
         (n-len (length needle))
         (limit (- s-len n-len))
         (i 0)
         (found nil))
    (while (and (<= i limit) (not found))
      (let ((j 0)
            (ok t))
        (while (and ok (< j n-len))
          (if (eq (aref s (+ i j)) (aref needle j))
              (setq j (1+ j))
            (setq ok nil)))
        (if ok
            (setq found i)
          (setq i (1+ i)))))
    found))

(defun anvil-runtime-shell--scan-json-string (s needle)
  "Return a JSON string value after NEEDLE in S, or nil."
  (let ((pos (anvil-runtime-shell--substr-pos s needle)))
    (when pos
      (let ((i (+ pos (length needle)))
            (n (length s))
            (chars nil))
        (while (and (< i n)
                    (let ((c (aref s i)))
                      (or (eq c ?\s) (eq c ?\t) (eq c ?\n) (eq c ?\r))))
          (setq i (1+ i)))
        (when (and (< i n) (eq (aref s i) ?\"))
          (setq i (1+ i))
          (while (and (< i n) (not (eq (aref s i) ?\")))
            (if (eq (aref s i) ?\\)
                (progn
                  (setq i (1+ i))
                  (when (< i n)
                    (push (aref s i) chars)
                    (setq i (1+ i))))
              (push (aref s i) chars)
              (setq i (1+ i))))
          (apply #'string (nreverse chars)))))))

(defun anvil-runtime-shell--scan-json-id (s)
  "Return a simple JSON-RPC id literal from S, as already encoded JSON."
  (let ((pos (anvil-runtime-shell--substr-pos s "\"id\":")))
    (if (not pos)
        "null"
      (let ((i (+ pos 5))
            (n (length s))
            (chars nil))
        (while (and (< i n)
                    (let ((c (aref s i)))
                      (or (eq c ?\s) (eq c ?\t) (eq c ?\n) (eq c ?\r))))
          (setq i (1+ i)))
        (cond
         ((and (< i n) (eq (aref s i) ?\"))
          (let ((start i))
            (setq i (1+ i))
            (while (and (< i n)
                        (or (not (eq (aref s i) ?\"))
                            (and (> i start)
                                 (eq (aref s (1- i)) ?\\))))
              (setq i (1+ i)))
            (if (< i n) (substring s start (1+ i)) "null")))
         (t
          (while (and (< i n)
                      (let ((c (aref s i)))
                        (or (eq c ?-) (and (>= c ?0) (<= c ?9)))))
            (push (aref s i) chars)
            (setq i (1+ i)))
          (if chars (apply #'string (nreverse chars)) "null")))))))

(defun anvil-runtime-shell--frame (body)
  "Return BODY as a Content-Length MCP frame."
  (let ((n (if (fboundp 'string-bytes) (string-bytes body) (length body))))
    (concat "Content-Length: " (number-to-string n) "\r\n\r\n" body)))

(defun anvil-runtime-shell--stdout (s)
  "Write S to stdout using the standalone byte writer when present."
  (if (and (fboundp 'nelisp--write-stdout-bytes) (stringp s))
      (nelisp--write-stdout-bytes s)
    (princ s)))

(defun anvil-runtime-shell--fast-refill ()
  "Read more stdin into `anvil-runtime-shell--fast-stdin-buffer'."
  (let ((chunk (and (fboundp 'read-stdin-bytes)
                    (read-stdin-bytes 4096))))
    (when (and (stringp chunk) (> (length chunk) 0))
      (setq anvil-runtime-shell--fast-stdin-buffer
            (concat anvil-runtime-shell--fast-stdin-buffer chunk))
      t)))

(defun anvil-runtime-shell--fast-read-frame ()
  "Read one MCP frame body using only standalone primitives."
  (let ((sep-pos nil)
        (done nil))
    (while (and (not done)
                (not (setq sep-pos
                           (anvil-runtime-shell--substr-pos
                            anvil-runtime-shell--fast-stdin-buffer
                            "\r\n\r\n"))))
      (setq done (not (anvil-runtime-shell--fast-refill))))
    (when sep-pos
      (let* ((header (substring anvil-runtime-shell--fast-stdin-buffer
                                0 sep-pos))
             (body-start (+ sep-pos 4))
             (cl-pos (anvil-runtime-shell--substr-pos header "Content-Length:"))
             (n nil))
        (when cl-pos
          (let ((i (+ cl-pos 15))
                (h-len (length header))
                (digits nil))
            (while (and (< i h-len)
                        (let ((c (aref header i)))
                          (or (eq c ?\s) (eq c ?\t))))
              (setq i (1+ i)))
            (while (and (< i h-len)
                        (let ((c (aref header i)))
                          (and (>= c ?0) (<= c ?9))))
              (push (aref header i) digits)
              (setq i (1+ i)))
            (when digits
              (setq n (string-to-number
                       (apply #'string (nreverse digits)))))))
        (when n
          (while (and (< (length anvil-runtime-shell--fast-stdin-buffer)
                         (+ body-start n))
                      (anvil-runtime-shell--fast-refill)))
          (when (>= (length anvil-runtime-shell--fast-stdin-buffer)
                    (+ body-start n))
            (let ((body (substring anvil-runtime-shell--fast-stdin-buffer
                                   body-start (+ body-start n))))
              (setq anvil-runtime-shell--fast-stdin-buffer
                    (substring anvil-runtime-shell--fast-stdin-buffer
                               (+ body-start n)))
              body)))))))

(defun anvil-runtime-shell--fast-tools-result (fast-file)
  "Return a precomputed `tools/list' result JSON from FAST-FILE, or nil."
  (let ((anvil-runtime-shell--fast-tools-json nil))
    (condition-case nil
        (progn
          (anvil-runtime-shell--fast-log "[FAST] load fast tools")
          (load fast-file t t)
          (when (stringp anvil-runtime-shell--fast-tools-json)
            (anvil-runtime-shell--fast-log "[FAST] fast tools loaded")
            anvil-runtime-shell--fast-tools-json))
      (error
       (anvil-runtime-shell--fast-log "[FAST] fast tools unavailable")
       nil))))

(defun anvil-runtime-shell--fast-handshake (fast-file)
  "Serve initialize/tools-list directly from FAST-FILE before full load."
  (let ((tools-json (anvil-runtime-shell--fast-tools-result fast-file))
        (keep-going t)
        (served nil))
    (when tools-json
      (anvil-runtime-shell--fast-log "[FAST] enter loop")
      (while keep-going
        (let ((body (anvil-runtime-shell--fast-read-frame)))
          (if (not body)
              (setq keep-going nil)
            (let ((method
                   (anvil-runtime-shell--scan-json-string body "\"method\":"))
                  (id (anvil-runtime-shell--scan-json-id body)))
              (anvil-runtime-shell--fast-log
               (concat "[FAST] method=" (or method "nil")))
              (cond
               ((equal method "initialize")
                (anvil-runtime-shell--stdout
                 (anvil-runtime-shell--frame
                  (concat "{\"jsonrpc\":\"2.0\",\"id\":" id
                          ",\"result\":{\"protocolVersion\":\"2025-03-26\","
                          "\"serverInfo\":{\"name\":\"anvil\","
                          "\"version\":\"2025-03-26\"},"
                          "\"capabilities\":{\"tools\":{}}}}")))
                (setq served t))
               ((equal method "notifications/initialized")
                (setq served t))
               ((equal method "tools/list")
                (anvil-runtime-shell--stdout
                 (anvil-runtime-shell--frame
                  (concat "{\"jsonrpc\":\"2.0\",\"id\":" id
                          ",\"result\":" tools-json "}")))
                (setq served t)
                (setq keep-going nil))
               (t
                (setq anvil-runtime-shell--fast-pending-body body)
                (setq keep-going nil))))))))
    served))

;; Path resolution.  Priority order:
;;
;;   1. `anvil-runtime-bootstrap-{anvil-el,nelisp-emacs}-dir' set by
;;      `bin/anvil-runtime' before loading us.  This is the primary
;;      channel — env vars cannot work because standalone NeLisp's
;;      `getenv' is itself a polyfill that only binds once
;;      `emacs-callproc.el' is loaded inside `(load init-el)' below.
;;
;;   2. `getenv' fallback — useful when this file is loaded under host
;;      Emacs for tests / interactive development.
;;
;;   3. `load-file-name'-derived sibling-checkout convention
;;      (`<parent>/anvil.el', `<parent>/nelisp-emacs').
;;
;;   4. Hardcoded dev-mirror fallback.
(let* ((anvil-el-dir
        (or (and (boundp 'anvil-runtime-bootstrap-anvil-el-dir)
                 anvil-runtime-bootstrap-anvil-el-dir)
            (anvil-runtime-shell--env
             "ANVIL_EL_DIR"
             (or (and (boundp 'load-file-name) load-file-name
                      (file-name-directory
                       (directory-file-name
                        (file-name-directory load-file-name))))
                 "/home/madblack-21/Cowork/Notes/dev/anvil.el"))))
       (nelisp-emacs-dir
        (or (and (boundp 'anvil-runtime-bootstrap-nelisp-emacs-dir)
                 anvil-runtime-bootstrap-nelisp-emacs-dir)
            (anvil-runtime-shell--env
             "NELISP_EMACS_DIR"
             (concat (file-name-directory
                      (directory-file-name anvil-el-dir))
                     "nelisp-emacs"))))
       (server-id
        ;; Default `emacs-eval' matches the constant used by every
        ;; GREEN-bucket anvil-* module's `--server-id'.  Tools register
        ;; into that bucket, so the dispatch must look them up there.
        ;; Override via ANVIL_SERVER_ID once getenv reads OS env.
        (anvil-runtime-shell--env "ANVIL_SERVER_ID" "emacs-eval"))
       (init-el (concat nelisp-emacs-dir "/src/emacs-init.el"))
       (stub-el (concat nelisp-emacs-dir "/src/emacs-stub.el"))
       (stdio-el (concat nelisp-emacs-dir "/src/emacs-stdio.el"))
       (metrics-el (concat anvil-el-dir "/anvil-server-metrics.el"))
       (server-el (concat anvil-el-dir "/anvil-server.el"))
       (server-commands-el (concat anvil-el-dir "/anvil-server-commands.el"))
       ;; State dir: bin/anvil-runtime creates it (default ~/.anvil-runtime)
       ;; and passes it through the bootstrap.  The old fixed
       ;; "/tmp/anvil-runtime" stays as the last-resort default for
       ;; host-Emacs test loads; it does not exist for the native
       ;; windows-x86_64 reader (NeLisp v1.2.0).
       (state-dir
        (or (and (boundp 'anvil-runtime-bootstrap-state-dir)
                 anvil-runtime-bootstrap-state-dir)
            (anvil-runtime-shell--env "ANVIL_RUNTIME_DAEMON_DIR"
                                      "/tmp/anvil-runtime")))
       (fast-tools-file (concat state-dir "/anvil-fast-tools.el")))

  (when (and (not anvil-server--debug-trace)
             (fboundp 'read-stdin-bytes)
             (fboundp 'nelisp--write-stdout-bytes))
    (anvil-runtime-shell--fast-handshake fast-tools-file))

  ;; Bootstrap layer 2 + json / backquote fixes.
  ;; emacs-init.el gates its vendor load-path setup on
  ;; `nelisp-emacs-vendor-root'.  Without this the subr-x / nelisp-coding
  ;; chain fails with "feature was not provided".  (Same regression as
  ;; server-loop.el; both code paths need this setq.)
  (setq nelisp-emacs-vendor-root (concat nelisp-emacs-dir "/vendor"))
  ;; Pre-provide `nelisp-coding-jis-tables' so its 460 KB defconst is
  ;; never loaded under the post-2026-05-17 nelisp (Doc 49 Wave 7-13
  ;; Phase 47 native swap), which aborts with glibc "double free or
  ;; corruption (!prev)" on that file.  MCP / JSON-RPC is UTF-8 only,
  ;; so the JIS tables are deadweight here.
  (provide 'nelisp-coding-jis-tables)
  ;; Pre-provide the 6 vendor libs that `anvil-runtime-polyfills.el'
  ;; tries to `(load ...)' below.  Under the post-2026-05-17 nelisp the
  ;; pure-elisp interpreter allocates ~1 MB/s while walking those files
  ;; and never reclaims the cons cells, so even the FAILED loads (e.g.
  ;; `seq' / `cl-extra' / `profiler') burn substrate memory unboundedly.
  ;; NeLisp's permissive `require' silently succeeds for already-provided
  ;; features, so downstream `(require 'subr-x)' etc are satisfied
  ;; without ever touching the vendor files.
  (dolist (lib '(subr-x seq cl-extra cl-seq benchmark profiler))
    (provide lib))
  ;; HEADLESS editor/UI skip (standalone NeLisp cold-load ~76s -> ~36s).
  ;; `bin/anvil-runtime mcp serve' is always headless JSON-RPC over stdio: it
  ;; never opens a frame, syntax table, font-lock buffer or TUI event loop.
  ;; Pre-providing these editor/UI substrate features turns emacs-init.el's
  ;; `(require ...)' calls into silent no-ops (NeLisp's permissive `require'
  ;; succeeds for already-provided features), so the pure-elisp interpreter
  ;; never walks ~50s worth of unused defuns.  KEEP loaded: emacs-buffer /
  ;; minibuffer / string / hash / fns / eval / callproc / sqlite — anvil-server
  ;; depends on those.  If a tool handler ever needs one of the skipped
  ;; features, drop it from this list (it will then load on demand).
  (dolist (f '(emacs-syntax-table emacs-elisp-mode emacs-mode emacs-mode-builtins
               emacs-font-lock-builtins emacs-faces-builtins emacs-textmodes-stub
               emacs-redisplay-builtins emacs-tui-event emacs-tui-backend
               emacs-frame-builtins emacs-window-builtins emacs-keymap-builtins
               emacs-command-loop-builtins
               emacs-frame emacs-window keymap emacs-faces emacs-font-lock))
    (provide f))
  ;; Prefer .el over .elc — post-2026-05-17 nelisp's stdlib-misc swaps
  ;; the elisp Reader to a pure-elisp form that cannot parse `#[..]'
  ;; byte-compiled lambdas or `#s(..)' record syntax that .elc files
  ;; contain.  Without this, stale .elc files in nelisp-emacs/src/ and
  ;; anvil.el/ silently override their .el siblings and explode after
  ;; stdlib-misc loads.
  (setq load-prefer-newer t)
  ;; `temporary-file-directory': nelisp-emacs's emacs-vars.el falls back
  ;; to "/tmp/", which does not exist for the native windows-x86_64
  ;; reader, so anvil-server's schema cache could never be written there
  ;; and every start paid the eager schema path.  Bind it under the
  ;; launcher's state dir (bin/anvil-runtime creates `<state>/tmp')
  ;; before emacs-vars.el's `unless boundp' guard runs.  Only when the
  ;; substrate left it unbound — a reader that ships its own value wins.
  (unless (boundp 'temporary-file-directory)
    (defvar temporary-file-directory (concat state-dir "/tmp/")))
  ;; NeLisp v1.2.0: the reader's `load' does not bind `load-file-name'
  ;; for the file it is loading, so emacs-init.el's "put my own src/ on
  ;; load-path" step (which keys on `load-file-name') is a no-op and its
  ;; first `(require 'nelisp-emacs)' dies with file-missing.  Surface
  ;; the Layer 2 src/ dir explicitly; harmless where the reader already
  ;; binds the variable.
  (let ((src-dir (concat nelisp-emacs-dir "/src")))
    (unless (and (boundp 'load-path) (member src-dir load-path))
      (setq load-path (cons src-dir (and (boundp 'load-path) load-path)))))
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[STEP] pre-init"))
  (load init-el nil t)
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[STEP] init-el done"))
  (load stub-el nil t)
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[STEP] stub-el done"))

  ;; --- TEMPORARY alist-get override (= Doc 98 §98.2 workaround) ---
  ;;
  ;; The fixed `alist-get' lives in `nelisp/lisp/nelisp-stdlib-misc.el'
  ;; on the source side, but standalone NeLisp's `.image' baker (= Doc 98
  ;; §98.2) doesn't yet capture elisp-side `defun' / `fset' mutations that
  ;; route through the env-shim, so the alist-get image baked into the
  ;; binary is still the pre-fix version.  Direct-load the .el here so
  ;; the corrected `alist-get' wins over the image-embedded one.
  ;;
  ;; Remove this block once Doc 98 §98.2 (= elisp-complete frozen-heap
  ;; baker) ships and the .image carries the fixed alist-get.
  ;; Inline the alist-get fix from nelisp-stdlib-misc.el (= the only
  ;; piece anvil-server actually needs from that file).  Loading the
  ;; full stdlib-misc redefines `princ' / `error' / `list' / `provide' /
  ;; etc to pure-elisp versions that cause json-read-from-string to
  ;; degrade ~300x on strings > 75 bytes (bisected 2026-05-24).  By
  ;; inlining just this one defun we get the alist-get fix without
  ;; clobbering the Rust-fast primitives the rest of anvil needs.
  ;;
  ;; NeLisp v1.2.0: the prelude ships a correct `alist-get', so the
  ;; override is gated on a functional probe — it only lands when the
  ;; running definition is missing or answers wrong (= the legacy
  ;; baked-image case this block was written for).
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
  ;; / `(require 'anvil-orchestrator-presets)' / etc. inside tool-module
  ;; files resolve to siblings of `anvil-server.el'.  Without this the
  ;; load/enable of those modules fails with "Cannot open load file".
  (add-to-list 'load-path anvil-el-dir)

  ;; anvil-server module load chain.  Order: metrics → server → commands
  ;; (= matches anvil-server.el's `(require 'anvil-server-metrics)' and
  ;; anvil-server-commands.el's `(require 'anvil-server)').
  (load metrics-el nil t)
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[STEP] metrics-el done"))
  (load server-el nil t)
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[STEP] server-el done"))
  ;; Keep the schema cache next to the fast-handshake cache so one state
  ;; dir holds every persisted artefact of a standalone deployment.
  (when (boundp 'anvil-server-schema-cache-file)
    (setq anvil-server-schema-cache-file
          (concat state-dir "/anvil-schema-cache.el")))
  (load server-commands-el nil t)
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[STEP] server-commands-el done"))

  ;; stdin shim — anvil-server-run-batch-stdio reads frames via
  ;; `read-from-minibuffer'; emacs-stdio.el's installer overrides the
  ;; bulk-stub nil binding with a chunked reader backed by libc.read.
  (load stdio-el nil t)
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[STEP] stdio-el done"))
  (when (boundp 'emacs-stdio--buffer)
    (setq emacs-stdio--buffer
          (concat anvil-runtime-shell--fast-stdin-buffer
                  emacs-stdio--buffer))
    (setq anvil-runtime-shell--fast-stdin-buffer ""))
  (when (fboundp 'emacs-stdio-install-stdin-shim)
    (emacs-stdio-install-stdin-shim))

  ;; Substrate polyfills (cl-* gaps + sqlite cursor protocol +
  ;; benchmark/profiler stubs) for anvil-* GREEN-bucket modules.
  ;; Defer this until a real tool module is loaded.  Cached lazy tool
  ;; fragments can advertise tools/list without it, saving cold-connect
  ;; time on the common schema-cache-hit path.
  (defvar anvil-runtime-shell--polyfills-loaded nil)
  (defun anvil-runtime-shell--ensure-polyfills-loaded ()
    (unless anvil-runtime-shell--polyfills-loaded
      (setq anvil-runtime-shell--polyfills-loaded t)
      (let ((polyfills-el (concat anvil-el-dir
                                  "/scripts/anvil-runtime-polyfills.el")))
        (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
          (nelisp--write-stderr-line
           (concat "[shell-loop] loading polyfills " polyfills-el)))
        (condition-case err
            (load polyfills-el nil t)
          (error
           (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
             (nelisp--write-stderr-line
              (concat "[shell-loop] polyfills load ERR: "
                      (format "%S" err))))))
        (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
          (nelisp--write-stderr-line "[STEP] polyfills done")))))

  ;; `help-function-arglist' polyfill — emacs-stub-bulk.el ships a
  ;; nil-returning placeholder, but anvil-server.el's tool dispatcher
  ;; uses the arglist to validate provided params (= every supplied
  ;; param becomes "Unexpected" when the polyfill returns nil).
  ;; NeLisp standalone wraps user defuns as `(closure ENV ARGS BODY)',
  ;; so we extract ARGS by case on the head symbol.  Unconditional
  ;; `defun' overrides whatever bulk wired in.
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

  ;; Phase B5 Final B Stage 1b — regex-free overrides for the framing
  ;; helpers anvil-server.el uses.  Standalone NeLisp ships only
  ;; `string-match-p' and even that is a hand-rolled lookup table (=
  ;; not a real regex engine), so the original `string-match' /
  ;; `replace-regexp-in-string' patterns return nil unconditionally.
  ;; The overrides below cover the exact callsites in anvil-server's
  ;; MCP framing path with string-search / substring-based logic.

  ;; `(replace-regexp-in-string "\r\\'" "" line)' just strips a single
  ;; trailing CR.  Replace with a simple suffix check.
  (defun anvil-server--strip-trailing-cr (s)
    (if (and (stringp s)
             (> (length s) 0)
             (eq (aref s (1- (length s))) ?\r))
        (substring s 0 (1- (length s)))
      s))

  (defun anvil-server-mcp-parse-content-length-header (header-block)
    "Phase B5 Stage 1b override — regex-free Content-Length parser.
Walks HEADER-BLOCK line-by-line searching for `Content-Length:'
case-insensitively, returns the integer value or nil."
    (when (stringp header-block)
      (let ((lines (split-string header-block "\r\n"))
            (found nil))
        (while (and lines (not found))
          (let* ((line (anvil-server--strip-trailing-cr (car lines)))
                 (line-down (downcase line))
                 (prefix "content-length:"))
            (when (and (>= (length line-down) (length prefix))
                       (string= (substring line-down 0 (length prefix)) prefix))
              (let* ((rest (substring line (length prefix)))
                     ;; Skip leading whitespace.
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
        found)))

  (defun anvil-server-mcp-detect-framing-p (initial)
    "Phase B5 Stage 1b override — case-insensitive prefix check for
`Content-Length:' on INITIAL string (= the first line read from stdin)."
    (when (stringp initial)
      (let* ((stripped (anvil-server--strip-trailing-cr initial))
             (down (downcase stripped))
             (prefix "content-length:"))
        (and (>= (length down) (length prefix))
             (string= (substring down 0 (length prefix)) prefix)))))

  (defun anvil-server-mcp-frame-encode (body)
    "Phase B5 Stage 1b override — emit `Content-Length: N\r\n\r\nBODY'.
N is the UTF-8 byte length of BODY."
    (let* ((bytes (if (fboundp 'encode-coding-string)
                      (encode-coding-string body 'utf-8 t)
                    body))
           (n (if (fboundp 'string-bytes)
                  (string-bytes bytes)
                (length bytes))))
      (concat "Content-Length: " (number-to-string n) "\r\n\r\n" body)))

  ;; Override the framed-with-prefix reader entirely.  The original uses
  ;; `replace-regexp-in-string' for trailing-CR strip, which is a no-op
  ;; stub on standalone NeLisp — that caused the header read loop to
  ;; consume the body line as just another header (= the empty `\r' line
  ;; never matched `string-empty-p'), so by the time body collection
  ;; ran, stdin was at EOF.  This regex-free port restores correctness.
  (defun anvil-server--batch-read-framed-with-prefix (first-header-line)
    "Phase B5 Stage 1b override — regex-free framed reader.

Body bytes are pulled via `emacs-stdio-read-bytes' (Phase B6 fix,
2026-05-10) because the MCP wire format does not insert a newline
between body and the next frame's header — line-based reads would
consume past the body's Content-Length boundary and discard the
head of the next header line."
    (let ((header-lines (list (anvil-server--strip-trailing-cr first-header-line)))
          (seen-blank nil))
      (catch 'done
        (while (not seen-blank)
          (let ((line (ignore-errors (read-from-minibuffer ""))))
            (cond
             ((null line) (throw 'done nil))
             ((string-empty-p (anvil-server--strip-trailing-cr line))
              (setq seen-blank t))
             (t (push (anvil-server--strip-trailing-cr line)
                      header-lines))))))
      (let* ((header-block (mapconcat #'identity
                                      (nreverse header-lines) "\r\n"))
             (n (anvil-server-mcp-parse-content-length-header header-block)))
        (when (and n (>= n 0))
          (let ((body (and (fboundp 'emacs-stdio-read-bytes)
                           (emacs-stdio-read-bytes n))))
            (and body (> (length body) 0) body))))))

  (defun anvil-server--batch-read-framed-message ()
    "Phase B5 Stage 1b override — used on subsequent loop iterations.
Reads the first header line, then delegates to the framed-with-prefix
reader."
    (let ((first (ignore-errors (read-from-minibuffer ""))))
      (cond
       ((null first) nil)
       (t (anvil-server--batch-read-framed-with-prefix first)))))

  (defun anvil-server--batch-skip-blank-lines ()
    "Phase B5 Stage 1b override — drain blanks until non-blank line.
Uses CR-strip to recognise lines that are CRLF artefacts as blank."
    (let ((line ""))
      (while (and (stringp line)
                  (string-empty-p (anvil-server--strip-trailing-cr line)))
        (setq line (ignore-errors (read-from-minibuffer ""))))
      line))

  (defun anvil-runtime-shell--module-tool-ids (module-name)
    "Return the default MCP tool ids registered by MODULE-NAME."
    (cond
     ((string= module-name "anvil-discovery")
      '("anvil-tools-by-intent" "anvil-tools-usage-report"))
     ((string= module-name "anvil-sqlite")
      '("sqlite-query"))
     ((string= module-name "anvil-bench")
      '("bench-compare" "bench-profile-expr" "bench-last"))
     (t nil)))

  (defun anvil-runtime-shell--load-tool-module (module-name)
    "Load MODULE-NAME from `anvil-el-dir' and call its enable function."
    (anvil-runtime-shell--ensure-polyfills-loaded)
    (let* ((file (concat anvil-el-dir "/" module-name ".el"))
           (enable-sym (intern (concat module-name "-enable"))))
      (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
        (nelisp--write-stderr-line
         (concat "[shell-loop] loading " file)))
      (load file nil t)
      (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
        (nelisp--write-stderr-line
         (concat "[shell-loop] " (symbol-name enable-sym)
                 " fboundp=" (if (fboundp enable-sym) "t" "nil"))))
      (when (fboundp enable-sym)
        (funcall enable-sym))
      ;; Post-load substrate patches (= anvil-sqlite regex compat etc).
      ;; In lazy mode modules load one at a time, so run the patches after
      ;; each successful module load instead of once after an eager chain.
      (when (fboundp 'anvil-runtime-polyfills-apply-post-load-patches)
        (condition-case err
            (anvil-runtime-polyfills-apply-post-load-patches)
          (error
           (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
             (nelisp--write-stderr-line
              (concat "[shell-loop] post-load patches ERR: "
                      (format "%S" err)))))))))

  ;; Optional tool-module load+enable chain.  `ANVIL_TOOL_MODULES' is a
  ;; comma-separated list of anvil-* module basenames (e.g.
  ;; "anvil-discovery,anvil-sqlite,anvil-bench") that the driver should
  ;; load from ANVIL_EL_DIR and call <name>-enable on, so their MCP tool
  ;; registrations land in the active server registry before the stdio
  ;; loop begins.  GREEN-bucket modules per the standalone gap analysis
  ;; (= no Emacs-only deps) are the safe targets for now.
  ;; Default = GREEN-bucket subset of anvil-* modules verified to
  ;; load+enable on the standalone substrate.  `anvil-bench' was
  ;; originally GREEN per the gap analysis but pulls `subr-x' /
  ;; `benchmark' / `profiler' that the standalone doesn't ship — kept
  ;; out of the default until those polyfill stubs land.
  ;; ANVIL_TOOL_MODULES env var is honoured when NeLisp's
  ;; process-environment stops being a stub-empty (Phase 1.6 keeps it
  ;; nil so getenv returns nil regardless of OS env), but for now the
  ;; default carries the actual list.
  (let* ((modules-env (anvil-runtime-shell--env
                       "ANVIL_TOOL_MODULES"
                       ;; Default trimmed 2026-05-24 from 7 → 3 modules.
                       ;; Under post-2026-05-17 nelisp without built-in
                       ;; sqlite, `anvil-state' / `anvil-memory' /
                       ;; `anvil-worklog' / `anvil-org-index' all fail
                       ;; at their *-enable step with "sqlite not
                       ;; available", registering zero tools.  Loading
                       ;; them anyway burned 63 % of cold-load time
                       ;; (~13 of ~20 min) on the pure-elisp interpreter
                       ;; before failing.  Set ANVIL_TOOL_MODULES
                       ;; explicitly to re-enable any of them once nelisp
                       ;; ships a sqlite primitive.
                       "anvil-discovery,anvil-sqlite,anvil-bench")))
    (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
      (nelisp--write-stderr-line
       (concat "[shell-loop] ANVIL_TOOL_MODULES="
               (if (> (length modules-env) 0) modules-env "<empty>"))))
    (when (> (length modules-env) 0)
      (let ((module-names nil)
            (lazy-loaders nil))
        (dolist (name (split-string modules-env "," t))
          (let ((trimmed (if (fboundp 'string-trim) (string-trim name) name)))
            (push trimmed module-names)
            (dolist (tool-id (anvil-runtime-shell--module-tool-ids trimmed))
              (let ((module-name trimmed))
                (push
                 (cons tool-id
                       (lambda (_tool-name _server-id)
                         (anvil-runtime-shell--load-tool-module module-name)))
                 lazy-loaders)))))
        (setq module-names (nreverse module-names))
        (let ((lazy-count
               (and (fboundp 'anvil-server-register-cached-tool-fragments)
                    (anvil-server-register-cached-tool-fragments
                     server-id lazy-loaders))))
          (cond
           ((and lazy-count (> lazy-count 0))
            (let ((tools-json
                   (and (boundp 'anvil-server--tools-list-cache)
                        (gethash server-id anvil-server--tools-list-cache))))
              (when (and (stringp tools-json)
                         (fboundp 'write-region))
                (condition-case nil
                    (write-region
                     (concat
                      "(setq anvil-runtime-shell--fast-tools-json '"
                      (prin1-to-string tools-json)
                      ")\n")
                     nil fast-tools-file)
                  (error nil))))
            (when (and anvil-server--debug-trace
                       (fboundp 'nelisp--write-stderr-line))
              (nelisp--write-stderr-line
               (format "[shell-loop] lazy cached tool fragments=%d"
                       lazy-count))))
           (t
            ;; Cache missing or empty: fall back to the old eager path so a
            ;; first-ever run can still generate and persist schema cache.
            (dolist (trimmed module-names)
              (condition-case err
                  (anvil-runtime-shell--load-tool-module trimmed)
                (error
                 (when (and anvil-server--debug-trace
                            (fboundp 'nelisp--write-stderr-line))
                   (nelisp--write-stderr-line
                    (concat "[shell-loop] " trimmed
                            " load/enable ERR: "
                            (format "%S" err)))))))))))))

  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (let ((bucket (and (boundp 'anvil-server--tools)
                       (gethash server-id anvil-server--tools))))
      (nelisp--write-stderr-line
       (concat "[shell-loop] pre-loop registry keys="
               (if bucket
                   (format "%S" (hash-table-keys bucket))
                 "<no-bucket>")))))

  ;; `anvil-server-run-batch-stdio' itself calls `anvil-server-start'
  ;; on entry, so we MUST NOT call it here (= duplicate call signals
  ;; `MCP server is already running').  Just enter the loop.
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[STEP] entering MCP stdin loop"))
  (when anvil-runtime-shell--fast-pending-body
    (anvil-server-start)
    (let ((resp
           (anvil-server-process-jsonrpc
            anvil-runtime-shell--fast-pending-body server-id)))
      (anvil-server--batch-emit-response resp t))
    (anvil-server-stop)
    (setq anvil-runtime-shell--fast-pending-body nil))
  ;; NeLisp v1.2.0: the reader has no process-exit primitive (Layer 2's
  ;; `kill-emacs' returns without exiting), and `nelisp --load FILE'
  ;; prints the value of FILE's last form to stdout on return -- a stray
  ;; `t' behind the final MCP frame.  bin/anvil-runtime therefore starts
  ;; the reader as bare `nelisp BOOTSTRAP', where the last form's value is
  ;; the exit status instead, and ends the bootstrap with `0'.  Nothing to
  ;; do here but return.
  (anvil-server-run-batch-stdio server-id))

;;; anvil-runtime-shell-loop.el ends here
