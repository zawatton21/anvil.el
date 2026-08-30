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
;;
;; Once tested end-to-end, the Rust crate `anvil-runtime/' can be
;; deleted in Final B Stage 2 along with the bin/anvil-runtime symlink
;; rewire.

;;; Code:

;; Pre-declare so `[STEP]' diagnostic trace lines below can gate on it
;; before `anvil-server.el' is loaded.  `defvar' is a no-op when the
;; variable is already bound, so `anvil-server.el's own `defvar' just
;; documents the same name later in the chain.
(defvar anvil-server--debug-trace nil
  "When non-nil, emit `[STDIO]/[PJ]/[VAD]/[JR]/[TL]/[REG-*]/[STEP]'
diagnostic trace lines via `nelisp--write-stderr-line'.  Default
nil for production (silent).")

(defun anvil-runtime-shell--env (name default)
  (let ((val (and (fboundp 'getenv) (getenv name))))
    (if (and val (> (length val) 0)) val default)))

(defvar anvil-runtime-shell--fast-handshake-enabled
  (or (and (boundp 'anvil-runtime-bootstrap-fast-handshake-enabled)
           anvil-runtime-bootstrap-fast-handshake-enabled)
      (equal (anvil-runtime-shell--env
              "ANVIL_RUNTIME_FAST_HANDSHAKE" nil)
             "1"))
  "When non-nil, serve the pre-load MCP fast handshake.
The default is nil; set ANVIL_RUNTIME_FAST_HANDSHAKE=1 to opt in.")

(defvar anvil-runtime-shell--primitive-load nil
  "Standalone `load' function saved before stdlib-misc replaces it.")

(defvar anvil-runtime-shell--primitive-hash-functions nil
  "Standalone hash functions saved before stdlib-misc replaces them.")

(defvar anvil-runtime-shell--skip-load-files nil
  "Absolute source files intentionally skipped by the runtime loader.")

(defun anvil-runtime-shell--compat-load
    (file &optional noerror nomessage _nosuffix _must-suffix)
  "Load FILE with Emacs load context and the standalone native reader."
  (let ((resolved
         (if (and (> (length file) 0) (eq (aref file 0) ?/))
             (cond
              ((file-exists-p file) file)
              ((file-exists-p (concat file ".el")) (concat file ".el"))
              (t nil))
           (locate-library file))))
    (if (null resolved)
        (if noerror nil
          (signal 'file-error (list "Cannot open load file" file)))
      (if (member resolved anvil-runtime-shell--skip-load-files)
          t
        (let ((prior-lfn (and (boundp 'load-file-name) load-file-name))
              (prior-dd (and (boundp 'default-directory) default-directory))
              (value nil)
              (err nil))
          (setq load-file-name resolved)
          (setq default-directory (file-name-directory resolved))
          (condition-case caught
              (setq value
                    (funcall anvil-runtime-shell--primitive-load
                             resolved noerror nomessage))
            (error (setq err caught)))
          (when (fboundp 'garbage-collect)
            (garbage-collect))
          (setq load-file-name prior-lfn)
          (setq default-directory prior-dd)
          (cond
           ((null err) value)
           (t (signal (car err) (cdr err)))))))))

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

(defun anvil-runtime-shell--fast-warn (s)
  "Write fast-cache warning S to stderr."
  (when (fboundp 'nelisp--write-stderr-line)
    (nelisp--write-stderr-line (concat "[FAST] " s))))

(defun anvil-runtime-shell--fast-json-space-p (c)
  "Return non-nil when C is JSON whitespace."
  (or (eq c ?\s) (eq c ?\t) (eq c ?\n) (eq c ?\r)))

(defun anvil-runtime-shell--fast-json-skip-space (s i)
  "Return the first non-whitespace index in S at or after I."
  (let ((n (length s)))
    (while (and (< i n)
                (anvil-runtime-shell--fast-json-space-p (aref s i)))
      (setq i (1+ i)))
    i))

(defun anvil-runtime-shell--fast-json-hex-p (c)
  "Return non-nil when C is a JSON hexadecimal digit."
  (or (and (>= c ?0) (<= c ?9))
      (and (>= c ?a) (<= c ?f))
      (and (>= c ?A) (<= c ?F))))

(defun anvil-runtime-shell--fast-json-string-end (s start)
  "Return the index after a JSON string in S at START, or nil."
  (let ((i (1+ start))
        (n (length s))
        (done nil)
        (valid (and (< start (length s)) (eq (aref s start) ?\"))))
    (while (and valid (not done) (< i n))
      (let ((c (aref s i)))
        (cond
         ((eq c ?\")
          (setq i (1+ i))
          (setq done t))
         ((< c 32)
          (setq valid nil))
         ((eq c ?\\)
          (setq i (1+ i))
          (if (>= i n)
              (setq valid nil)
            (let ((escaped (aref s i)))
              (if (eq escaped ?u)
                  (let ((remaining 4))
                    (while (and valid (> remaining 0))
                      (setq i (1+ i))
                      (if (or (>= i n)
                              (not (anvil-runtime-shell--fast-json-hex-p
                                    (aref s i))))
                          (setq valid nil)
                        (setq remaining (1- remaining))))
                    (when valid (setq i (1+ i))))
                (if (or (eq escaped ?\") (eq escaped ?\\)
                        (eq escaped ?/) (eq escaped ?b)
                        (eq escaped ?f) (eq escaped ?n)
                        (eq escaped ?r) (eq escaped ?t))
                    (setq i (1+ i))
                  (setq valid nil))))))
         (t
          (setq i (1+ i))))))
    (and valid done i)))

(defun anvil-runtime-shell--fast-json-number-end (s start)
  "Return the index after a JSON number in S at START, or nil."
  (let ((i start)
        (n (length s))
        (valid t))
    (when (and (< i n) (eq (aref s i) ?-))
      (setq i (1+ i)))
    (cond
     ((>= i n) (setq valid nil))
     ((eq (aref s i) ?0) (setq i (1+ i)))
     ((and (>= (aref s i) ?1) (<= (aref s i) ?9))
      (while (and (< i n) (>= (aref s i) ?0) (<= (aref s i) ?9))
        (setq i (1+ i))))
     (t (setq valid nil)))
    (when (and valid (< i n) (eq (aref s i) ?.))
      (setq i (1+ i))
      (if (or (>= i n) (< (aref s i) ?0) (> (aref s i) ?9))
          (setq valid nil)
        (while (and (< i n) (>= (aref s i) ?0) (<= (aref s i) ?9))
          (setq i (1+ i)))))
    (when (and valid (< i n)
               (or (eq (aref s i) ?e) (eq (aref s i) ?E)))
      (setq i (1+ i))
      (when (and (< i n)
                 (or (eq (aref s i) ?+) (eq (aref s i) ?-)))
        (setq i (1+ i)))
      (if (or (>= i n) (< (aref s i) ?0) (> (aref s i) ?9))
          (setq valid nil)
        (while (and (< i n) (>= (aref s i) ?0) (<= (aref s i) ?9))
          (setq i (1+ i)))))
    (and valid i)))

(defun anvil-runtime-shell--fast-json-literal-end (s start literal)
  "Return the index after LITERAL in S at START, or nil."
  (let ((end (+ start (length literal))))
    (and (<= end (length s))
         (string= (substring s start end) literal)
         end)))

(defun anvil-runtime-shell--fast-json-array-end (s start)
  "Return the index after a JSON array in S at START, or nil."
  (let* ((n (length s))
         (i (anvil-runtime-shell--fast-json-skip-space s (1+ start)))
         (done nil)
         (valid (and (< start n) (eq (aref s start) ?\[))))
    (if (and valid (< i n) (eq (aref s i) ?\]))
        (setq i (1+ i) done t)
      (while (and valid (not done))
        (setq i (anvil-runtime-shell--fast-json-value-end s i))
        (if (not i)
            (setq valid nil)
          (setq i (anvil-runtime-shell--fast-json-skip-space s i))
          (cond
           ((and (< i n) (eq (aref s i) ?,))
            (setq i (anvil-runtime-shell--fast-json-skip-space s (1+ i))))
           ((and (< i n) (eq (aref s i) ?\]))
            (setq i (1+ i) done t))
           (t (setq valid nil))))))
    (and valid done i)))

(defun anvil-runtime-shell--fast-json-object-end (s start)
  "Return the index after a JSON object in S at START, or nil."
  (let* ((n (length s))
         (i (anvil-runtime-shell--fast-json-skip-space s (1+ start)))
         (done nil)
         (valid (and (< start n) (eq (aref s start) ?\{))))
    (if (and valid (< i n) (eq (aref s i) ?\}))
        (setq i (1+ i) done t)
      (while (and valid (not done))
        (setq i (anvil-runtime-shell--fast-json-string-end s i))
        (if (not i)
            (setq valid nil)
          (setq i (anvil-runtime-shell--fast-json-skip-space s i))
          (if (or (>= i n) (not (eq (aref s i) ?:)))
              (setq valid nil)
            (setq i (anvil-runtime-shell--fast-json-skip-space s (1+ i)))
            (setq i (anvil-runtime-shell--fast-json-value-end s i))
            (if (not i)
                (setq valid nil)
              (setq i (anvil-runtime-shell--fast-json-skip-space s i))
              (cond
               ((and (< i n) (eq (aref s i) ?,))
                (setq i
                      (anvil-runtime-shell--fast-json-skip-space s (1+ i))))
               ((and (< i n) (eq (aref s i) ?\}))
                (setq i (1+ i) done t))
               (t (setq valid nil))))))))
    (and valid done i)))

(defun anvil-runtime-shell--fast-json-value-end (s start)
  "Return the index after one JSON value in S at START, or nil."
  (let* ((i (anvil-runtime-shell--fast-json-skip-space s start))
         (n (length s))
         (c (and (< i n) (aref s i))))
    (cond
     ((null c) nil)
     ((eq c ?\") (anvil-runtime-shell--fast-json-string-end s i))
     ((eq c ?\{) (anvil-runtime-shell--fast-json-object-end s i))
     ((eq c ?\[) (anvil-runtime-shell--fast-json-array-end s i))
     ((or (eq c ?-) (and (>= c ?0) (<= c ?9)))
      (anvil-runtime-shell--fast-json-number-end s i))
     ((eq c ?t) (anvil-runtime-shell--fast-json-literal-end s i "true"))
     ((eq c ?f) (anvil-runtime-shell--fast-json-literal-end s i "false"))
     ((eq c ?n) (anvil-runtime-shell--fast-json-literal-end s i "null"))
     (t nil))))

(defun anvil-runtime-shell--fast-tools-json-problem (tools-json)
  "Return nil when TOOLS-JSON is a plausible non-empty tools result.
Otherwise return a short string describing the problem."
  (if (not (stringp tools-json))
      "cache value is not a string"
    (let* ((n (length tools-json))
           (start (anvil-runtime-shell--fast-json-skip-space tools-json 0))
           (end (anvil-runtime-shell--fast-json-value-end tools-json start)))
      (cond
       ((or (not end)
            (/= (anvil-runtime-shell--fast-json-skip-space tools-json end) n))
        "cache value is not valid JSON")
       ((or (>= start n) (not (eq (aref tools-json start) ?\{)))
        "top-level JSON value is not an object")
       (t
        (let ((i (anvil-runtime-shell--fast-json-skip-space
                  tools-json (1+ start)))
              (found nil)
              (problem nil)
              (done nil))
          (if (and (< i n) (eq (aref tools-json i) ?\}))
              (setq done t)
            (while (and (not done) (not problem))
              (let* ((key-start i)
                     (key-end
                      (anvil-runtime-shell--fast-json-string-end
                       tools-json key-start)))
                (setq i (anvil-runtime-shell--fast-json-skip-space
                         tools-json key-end))
                (setq i (anvil-runtime-shell--fast-json-skip-space
                         tools-json (1+ i)))
                (when (string= (substring tools-json key-start key-end)
                               "\"tools\"")
                  (setq found t)
                  (cond
                   ((or (>= i n) (not (eq (aref tools-json i) ?\[)))
                    (setq problem "tools value is not an array"))
                   ((let ((first
                           (anvil-runtime-shell--fast-json-skip-space
                            tools-json (1+ i))))
                      (and (< first n) (eq (aref tools-json first) ?\])))
                    (setq problem "tools array is empty"))))
                (unless problem
                  (setq i (anvil-runtime-shell--fast-json-value-end tools-json i))
                  (setq i (anvil-runtime-shell--fast-json-skip-space
                           tools-json i))
                  (cond
                   ((eq (aref tools-json i) ?,)
                    (setq i (anvil-runtime-shell--fast-json-skip-space
                             tools-json (1+ i))))
                   ((eq (aref tools-json i) ?\})
                    (setq done t)))))))
          (or problem (and (not found) "tools key is missing"))))))))

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
  (let* ((bytes (if (multibyte-string-p body)
                    (if (fboundp 'nelisp--write-stderr-line)
                        (string-as-unibyte body)
                      (encode-coding-string body 'utf-8 t))
                  body))
         (n (length bytes)))
    (concat "Content-Length: " (number-to-string n) "\r\n\r\n" bytes)))

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
      (when (multibyte-string-p chunk)
        (setq chunk (string-as-unibyte chunk)))
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
          (when (file-exists-p fast-file)
            (let ((problem
                   (anvil-runtime-shell--fast-tools-json-problem
                    anvil-runtime-shell--fast-tools-json)))
              (if problem
                  (progn
                    (anvil-runtime-shell--fast-warn
                     (concat "refusing tools cache " fast-file ": " problem))
                    nil)
                (anvil-runtime-shell--fast-log "[FAST] fast tools loaded")
                anvil-runtime-shell--fast-tools-json))))
      (error
       (when (file-exists-p fast-file)
         (anvil-runtime-shell--fast-warn
          (concat "refusing tools cache " fast-file
                  ": cache file could not be loaded")))
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
       (nelisp-lisp-dir
        (or (and (boundp 'anvil-runtime-bootstrap-nelisp-lisp-dir)
                 (> (length anvil-runtime-bootstrap-nelisp-lisp-dir) 0)
                 anvil-runtime-bootstrap-nelisp-lisp-dir)
            (concat (file-name-directory (directory-file-name anvil-el-dir))
                    "nelisp/lisp")))
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
       (stdlib-misc (concat nelisp-lisp-dir "/nelisp-stdlib-misc.el"))
       (bootstrap-skip-features
        '(nelisp-coding-jis-tables calendar
          subr-x seq cl-extra cl-seq benchmark profiler
          emacs-syntax-table emacs-elisp-mode emacs-mode emacs-mode-builtins
          emacs-font-lock-builtins emacs-faces-builtins emacs-textmodes-stub
          emacs-redisplay-builtins emacs-tui-event emacs-tui-backend
          emacs-frame-builtins emacs-window-builtins emacs-keymap-builtins
          emacs-command-loop-builtins
          emacs-frame emacs-window keymap emacs-faces emacs-font-lock))
       (fast-tools-file "/tmp/anvil-runtime/anvil-fast-tools.el"))

  ;; Safety gate.  Measured on this machine on 2026-08-29: stdout was correct
  ;; in every fast-path run (5,208 bytes and six tools), but every process
  ;; terminated with exit 1 or SIGSEGV after 2--20 s, at HEAD db57796 too.
  ;;
  ;; RESOLVED the same day.  The cause was not in this file: two defects in
  ;; the standalone NeLisp GC, both fixed in nelisp's
  ;; `scripts/nelisp-standalone-build.el' -- a char-table/vector slot walk
  ;; that trusted a length it had read out of an uninitialised parse-pool
  ;; slot, and a per-frame parse-pool capacity read from a global word that
  ;; names whichever load is innermost.  The fast handshake never was the
  ;; affected path; with ASLR disabled the ordinary load path below
  ;; segfaulted just as reliably.  See
  ;; `Notes/dev/BUG-nelisp-layout-dependent-crash-2026-08-29.md'.
  ;;
  ;; The re-enable criterion this comment used to state -- repeated plain
  ;; runs exiting 0 with ASLR both enabled and disabled -- is met against a
  ;; NeLisp built at or after that fix: 3/3 with `setarch -R' and 3/3 plain,
  ;; each returning the full 5,208 bytes.  It is NOT met against an older
  ;; binary, which is why this stays opt-in: turning it on is safe only
  ;; if the deployed `target/nelisp' carries the GC fix.
  ;;
  ;; To turn it on, set `anvil-runtime-bootstrap-fast-handshake-enabled' in
  ;; the bootstrap `bin/anvil-runtime' generates.  ANVIL_RUNTIME_FAST_
  ;; HANDSHAKE=1 does NOT work under standalone NeLisp: the `defvar' above
  ;; runs before `emacs-callproc.el' provides `getenv', so the env channel
  ;; is host-Emacs only.
  (when (and anvil-runtime-shell--fast-handshake-enabled
             (not anvil-server--debug-trace)
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
  ;; Pre-provide the 6 vendor libs that `anvil-runtime-polyfills.el'
  ;; tries to `(load ...)' below.  Under the post-2026-05-17 nelisp the
  ;; pure-elisp interpreter allocates ~1 MB/s while walking those files
  ;; and never reclaims the cons cells, so even the FAILED loads (e.g.
  ;; `seq' / `cl-extra' / `profiler') burn substrate memory unboundedly.
  ;; NeLisp's permissive `require' silently succeeds for already-provided
  ;; features, so downstream `(require 'subr-x)' etc are satisfied
  ;; without ever touching the vendor files.
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
  ;; Populate the native feature registry before stdlib-misc replaces
  ;; `provide'.  Measured 2026-08-29: omitting this pre-pass made the
  ;; standalone process exit 139 immediately after `[STEP] pre-init'.
  (dolist (feature bootstrap-skip-features)
    (provide feature))
  ;; Keep the runtime's established source-first loading policy.
  (setq load-prefer-newer t)
  ;; The ~300x JSON slowdown reported on 2026-05-24 was re-measured on
  ;; this machine on 2026-08-28 and did not reproduce: across nine
  ;; 25--10000-byte payload sizes (14 samples each), median with/without
  ;; ratios were 0.9435x--1.0722x.  A 10 KB call-count probe recorded zero
  ;; calls to `list', `error', `princ', `provide', and `load'; the parser
  ;; loaded by anvil-server's `(require 'json)' does not use those replaced
  ;; functions.  That series still measured a separate superlinear parser
  ;; curve from 300--10000 bytes (log-log exponent 1.450; 10 KB median
  ;; 4285.132 ms with stdlib-misc), which this bootstrap does not address.
  ;; Load stdlib-misc before emacs-init: the same-machine probe measured
  ;; `load-file-name' nil inside a primitive load and non-nil with this
  ;; implementation.
  (setq anvil-runtime-shell--primitive-load (symbol-function 'load))
  (setq anvil-runtime-shell--primitive-hash-functions
        (mapcar (lambda (name) (cons name (symbol-function name)))
                '(maphash hash-table-keys hash-table-values hash-table-count)))
  (load stdlib-misc nil t)
  ;; Measured 2026-08-29: stdlib-misc's four hash traversal wrappers call
  ;; `nelisp--hash-pairs', which this standalone image does not define;
  ;; `anvil-server-start' consequently failed in `clrhash'.  Keep the
  ;; image's working native implementations while loading the rest of
  ;; stdlib-misc.
  (let ((saved anvil-runtime-shell--primitive-hash-functions))
    (while saved
      (fset (car (car saved)) (cdr (car saved)))
      (setq saved (cdr saved))))
  ;; Measured 2026-08-29: stdlib-misc's `load' could not resolve an
  ;; existing absolute /tmp source and its incremental reader treated
  ;; emacs-stub.el's `#x3FFFFF' as a variable.  The saved native loader
  ;; parsed that chain while this wrapper supplied non-nil `load-file-name'.
  ;; The same probe reached vendor calendar's cal-loaddefs.el and then
  ;; exited 139, so skip only that exact vendor body; the local calendar
  ;; facade and emacs-time.el had both completed before the failing load.
  (setq anvil-runtime-shell--skip-load-files
        (list (concat nelisp-emacs-dir
                      "/vendor/emacs-lisp/calendar/calendar.el")))
  (fset 'load #'anvil-runtime-shell--compat-load)
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[STEP] stdlib-misc done"))
  ;; Populate stdlib-misc's `features' list with the same set.  The
  ;; 2026-08-29 probe recorded `(featurep 'calendar)' as t here.
  (dolist (feature bootstrap-skip-features)
    (provide feature))
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[STEP] pre-init"))
  (load init-el nil t)
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[STEP] init-el done"))
  (load stub-el nil t)
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[STEP] stub-el done"))

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
  (load server-commands-el nil t)
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[STEP] server-commands-el done"))

  ;; stdin shim — anvil-server-run-batch-stdio reads frames via
  ;; `read-from-minibuffer'; emacs-stdio.el's installer overrides the
  ;; bulk-stub nil binding with a chunked reader backed by libc.read.
  (load stdio-el nil t)
  (when (and anvil-server--debug-trace (fboundp 'nelisp--write-stderr-line))
    (nelisp--write-stderr-line "[STEP] stdio-el done"))
  ;; Measured 2026-08-28 on NeLisp v1.1.0+1: `read-stdin-bytes'
  ;; returned "日本語" as multibyte with length 3 and string-bytes 9.
  ;; Normalize every refill so the stdio reader's length and substring
  ;; arithmetic stays in MCP wire bytes.
  (defun emacs-stdio--refill ()
    "Read stdin into `emacs-stdio--buffer' as unibyte wire bytes."
    (let ((chunk (read-stdin-bytes emacs-stdio--chunk-size)))
      (cond
       ((null chunk) nil)
       ((and (stringp chunk) (= (length chunk) 0)) nil)
       (t
        (when (multibyte-string-p chunk)
          (setq chunk (string-as-unibyte chunk)))
        (setq emacs-stdio--buffer (concat emacs-stdio--buffer chunk))
        t))))
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
    (let* ((bytes (anvil-server--string-to-utf8-bytes body))
           (n (length bytes)))
      (concat "Content-Length: " (number-to-string n) "\r\n\r\n" bytes)))

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
              (let ((problem
                     (anvil-runtime-shell--fast-tools-json-problem tools-json)))
                (if problem
                    (anvil-runtime-shell--fast-warn
                     (concat "refusing to write tools cache " fast-tools-file
                             ": " problem))
                  (when (fboundp 'write-region)
                    (condition-case nil
                        (write-region
                         (concat
                          "(setq anvil-runtime-shell--fast-tools-json '"
                          (prin1-to-string tools-json)
                          ")\n")
                         nil fast-tools-file)
                      (error nil))))))
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
  (anvil-server-run-batch-stdio server-id))

;;; anvil-runtime-shell-loop.el ends here
