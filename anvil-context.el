;;; anvil-context.el --- Reversible context compression for agents -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Headroom-inspired context compression for arbitrary text that is not
;; necessarily shell output.  `anvil-shell-filter' already handles
;; command-specific stdout compression + tee.  This module adds a
;; general ContentRouter + CCR-shaped store for tool outputs, RAG
;; snippets, JSON payloads, diffs, logs, code excerpts, and prose.
;;
;; The first implementation is intentionally local and deterministic:
;; no model download, no external service, and no semantic claims.  It
;; routes by content type, emits a compact view, and stores the original
;; under `anvil-state' so the caller can retrieve it on demand.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'anvil-server)
(require 'anvil-state)

(defgroup anvil-context nil
  "Reversible context compression for anvil."
  :group 'anvil
  :prefix "anvil-context-")

(defconst anvil-context--server-id "emacs-eval"
  "Server id under which context-* MCP tools register.")

(defconst anvil-context--ccr-ns "context-ccr"
  "`anvil-state' namespace storing raw context blobs.")

(defconst anvil-context--gain-ns "context-gain"
  "`anvil-state' namespace storing compression telemetry.")

(defcustom anvil-context-ccr-ttl-sec 86400
  "Seconds raw context is retained for `context-retrieve'."
  :type 'integer
  :group 'anvil-context)

(defcustom anvil-context-default-max-chars 6000
  "Default maximum characters returned by `context-compress'."
  :type 'integer
  :group 'anvil-context)

(defcustom anvil-context-critical-context-lines 2
  "Surrounding lines kept around critical log lines."
  :type 'integer
  :group 'anvil-context)

(defcustom anvil-context-critical-patterns
  '("\\berror\\b"
    "\\bfatal\\b"
    "\\bfailed\\b"
    "Traceback"
    "Exception"
    "AssertionError"
    "\\bpanic\\b"
    "segmentation fault")
  "Case-insensitive regexps treated as important in log/text compression."
  :type '(repeat regexp)
  :group 'anvil-context)

(defcustom anvil-context-token-chars 4.0
  "Approximate characters-per-token used for stats reporting."
  :type 'number
  :group 'anvil-context)

(defun anvil-context--coerce-int (value default)
  "Coerce VALUE to an integer, falling back to DEFAULT."
  (cond
   ((integerp value) value)
   ((and (stringp value) (string-match-p "\\`[0-9]+\\'" value))
    (string-to-number value))
   (t default)))

(defun anvil-context--nonempty-string (value)
  "Return VALUE when it is a non-empty string, otherwise nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun anvil-context--lines (text)
  "Split TEXT into lines without dropping empty interior lines."
  (split-string (or text "") "\n"))

(defun anvil-context--join-lines (lines)
  "Join LINES with newline separators."
  (string-join (delq nil lines) "\n"))

(defun anvil-context--truncate (text max-chars)
  "Return TEXT capped to MAX-CHARS with an omitted-byte footer."
  (let ((cap (max 0 (or max-chars anvil-context-default-max-chars))))
    (if (or (<= cap 0) (<= (length text) cap))
        text
      (concat (substring text 0 cap)
              (format "\n...[anvil-context: truncated, %d more chars]"
                      (- (length text) cap))))))

(defun anvil-context--json-read (raw)
  "Read RAW as JSON using list/alist containers.
Return (t . VALUE) on success, or nil on failure.  The cons wrapper
keeps JSON `null' and empty arrays distinct from parse failure."
  (condition-case nil
      (cons t
            (let ((json-object-type 'alist)
                  (json-array-type 'list)
                  (json-key-type 'string)
                  (json-false :false)
                  (json-null :null))
              (json-read-from-string raw)))
    (error nil)))

(defun anvil-context--json-object-p (value)
  "Return non-nil when VALUE looks like a JSON object alist."
  (and (listp value)
       (or (null value)
           (consp (car value))
           (and (symbolp (car value)) (null (cdr value))))))

(defun anvil-context--json-array-of-objects-p (value)
  "Return non-nil when VALUE is a JSON array mostly made of objects."
  (and (listp value)
       value
       (cl-every #'anvil-context--json-object-p
                 (seq-take value (min 5 (length value))))))

(defun anvil-context--value-shape (value)
  "Return a short shape string for JSON VALUE."
  (cond
   ((stringp value) (format "string[%d]" (length value)))
   ((numberp value) "number")
   ((memq value '(t :false :null)) (format "%s" value))
   ((vectorp value) (format "array[%d]" (length value)))
   ((and (listp value) (anvil-context--json-object-p value))
    (format "object{%d}" (length value)))
   ((listp value) (format "array[%d]" (length value)))
   (t (format "%s" (type-of value)))))

(defun anvil-context--json-key-list (object)
  "Return sorted keys from OBJECT alist."
  (sort (mapcar #'car object) #'string<))

(defun anvil-context--compress-json (raw max-chars)
  "Compress JSON RAW into a schema/sample view capped by MAX-CHARS."
  (let ((parsed (anvil-context--json-read raw)))
    (if (null parsed)
        (anvil-context--truncate raw max-chars)
      (let* ((value (cdr parsed))
             (out
              (cond
               ((anvil-context--json-array-of-objects-p value)
                (let* ((rows value)
                       (sample (seq-take rows (min 3 (length rows))))
                       (keys (sort (delete-dups
                                    (apply #'append
                                           (mapcar #'anvil-context--json-key-list
                                                   sample)))
                                   #'string<)))
                  (concat
                   (format "json array: %d objects\nkeys: %s\n"
                           (length rows) (string-join keys ", "))
                   "sample:\n"
                   (mapconcat
                    (lambda (row)
                      (concat "- "
                              (mapconcat
                               (lambda (k)
                                 (format "%s=%s"
                                         k
                                         (anvil-context--value-shape
                                          (alist-get k row nil nil #'equal))))
                               keys ", ")))
                    sample "\n"))))
               ((anvil-context--json-object-p value)
                (let ((keys (anvil-context--json-key-list value)))
                  (concat
                   (format "json object: %d keys\nkeys: %s\n"
                           (length keys) (string-join keys ", "))
                   "shapes:\n"
                   (mapconcat
                    (lambda (cell)
                      (format "- %s: %s" (car cell)
                              (anvil-context--value-shape (cdr cell))))
                    value "\n"))))
               (t
                (format "json value: %s" (anvil-context--value-shape value))))))
        (anvil-context--truncate out max-chars)))))

(defun anvil-context--compress-diff (raw max-chars)
  "Compress unified diff RAW into file/hunk/stat lines."
  (let ((files nil)
        (hunks nil)
        (adds 0)
        (dels 0))
    (dolist (line (anvil-context--lines raw))
      (cond
       ((string-prefix-p "diff --git " line)
        (push line files))
       ((string-prefix-p "@@ " line)
        (push line hunks))
       ((and (string-prefix-p "+" line)
             (not (string-prefix-p "+++" line)))
        (setq adds (1+ adds)))
       ((and (string-prefix-p "-" line)
             (not (string-prefix-p "---" line)))
        (setq dels (1+ dels)))))
    (anvil-context--truncate
     (anvil-context--join-lines
      (append
       (list (format "diff summary: %d files, %d hunks, +%d -%d"
                     (length files) (length hunks) adds dels))
       (list "files:")
       (or (nreverse files) '("(none detected)"))
       (list "hunks:")
       (seq-take (nreverse hunks) 20)
       (when (> (length hunks) 20)
         (list (format "...%d more hunks" (- (length hunks) 20))))))
     max-chars)))

(defun anvil-context--critical-line-p (line)
  "Return non-nil when LINE matches a critical pattern."
  (let ((case-fold-search t))
    (cl-some (lambda (pat) (string-match-p pat line))
             anvil-context-critical-patterns)))

(defun anvil-context--compress-log (raw max-chars)
  "Compress log RAW to critical lines plus a tail."
  (let* ((lines (anvil-context--lines raw))
         (count (length lines))
         (keep (make-hash-table :test 'eql)))
    (cl-loop for line in lines
             for idx from 0
             when (anvil-context--critical-line-p line)
             do (cl-loop for j from (max 0 (- idx anvil-context-critical-context-lines))
                         to (min (1- count)
                                 (+ idx anvil-context-critical-context-lines))
                         do (puthash j t keep)))
    (cl-loop for j from (max 0 (- count 20)) below count
             do (puthash j t keep))
    (let ((out-lines
           (append
            (list (format "log summary: %d lines, %d selected"
                          count (hash-table-count keep)))
            (cl-loop for line in lines
                     for idx from 0
                     when (gethash idx keep)
                     collect (format "%d:%s" (1+ idx) line)))))
      (anvil-context--truncate (anvil-context--join-lines out-lines)
                               max-chars))))

(defun anvil-context--code-signal-line-p (line)
  "Return non-nil when LINE is a useful code outline signal."
  (or
   (string-match-p
    (rx line-start (* space)
        (or "import " "from " "#include " "require(" "(require "
            "def " "class " "function " "export " "interface " "type "
            "struct " "enum " "impl " "fn " "pub fn "
            "(defun " "(defmacro " "(defcustom " "(defvar " "(cl-defun "))
    line)
   (string-match-p (rx (or "TODO" "FIXME" "BUG" "HACK")) line)))

(defun anvil-context--compress-code (raw max-chars)
  "Compress code RAW into an outline-like signal list."
  (let* ((lines (anvil-context--lines raw))
         (selected
          (cl-loop for line in lines
                   for idx from 1
                   when (anvil-context--code-signal-line-p line)
                   collect (format "%d:%s" idx (string-trim-right line)))))
    (anvil-context--truncate
     (anvil-context--join-lines
      (append
       (list (format "code outline: %d lines, %d signals"
                     (length lines) (length selected)))
       (or selected '("(no obvious definitions/imports found)"))))
     max-chars)))

(defun anvil-context--text-signal-line-p (line)
  "Return non-nil when LINE is likely useful in prose/RAG text."
  (or (string-match-p (rx line-start (* space) (or "#" "*" "-" "+") space) line)
      (string-match-p (rx line-start (* space) (+ digit) (or "." ")") space) line)
      (anvil-context--critical-line-p line)))

(defun anvil-context--compress-text (raw max-chars)
  "Compress prose RAW to headings, bullets, first lines, and tail."
  (let* ((lines (anvil-context--lines raw))
         (count (length lines))
         (signals
          (cl-loop for line in lines
                   for idx from 1
                   when (anvil-context--text-signal-line-p line)
                   collect (format "%d:%s" idx (string-trim-right line))))
         (head (seq-take lines (min 12 count)))
         (tail (if (> count 20) (last lines 8) nil)))
    (anvil-context--truncate
     (anvil-context--join-lines
      (append
       (list (format "text summary: %d lines, %d signal lines" count
                     (length signals))
             "head:")
       head
       (when signals
         (append (list "signals:") (seq-take signals 40)))
       (when tail
         (append (list "tail:") tail))))
     max-chars)))

(defun anvil-context-detect-kind (raw &optional kind)
  "Return content kind for RAW, respecting KIND when non-auto.
Kinds are `json', `diff', `log', `code', and `text'.  KIND may be
a symbol or string; nil / \"auto\" enables detection."
  (let ((hint (cond
               ((symbolp kind) kind)
               ((and (stringp kind) (not (string-empty-p kind)))
                (intern kind))
               (t 'auto))))
    (if (memq hint '(json diff log code text))
        hint
      (let ((trimmed (string-trim-left (or raw ""))))
        (cond
         ((and (string-match-p "\\`[\\[{]" trimmed)
               (anvil-context--json-read trimmed))
          'json)
         ((or (string-match-p "^diff --git " raw)
              (string-match-p "^@@ " raw))
          'diff)
         ((cl-some #'anvil-context--critical-line-p
                   (seq-take (anvil-context--lines raw) 80))
          'log)
         ((cl-some #'anvil-context--code-signal-line-p
                   (seq-take (anvil-context--lines raw) 80))
          'code)
         (t 'text))))))

(cl-defun anvil-context-compress (raw &key kind max-chars store)
  "Compress RAW and optionally STORE the original for retrieval.
Returns a plist with `:kind', `:compressed', size fields, and optional
`:ccr-id'."
  (let* ((raw* (or raw ""))
         (max* (anvil-context--coerce-int max-chars
                                          anvil-context-default-max-chars))
         (kind* (anvil-context-detect-kind raw* kind))
         (compressed
          (pcase kind*
            ('json (anvil-context--compress-json raw* max*))
            ('diff (anvil-context--compress-diff raw* max*))
            ('log  (anvil-context--compress-log raw* max*))
            ('code (anvil-context--compress-code raw* max*))
            (_     (anvil-context--compress-text raw* max*))))
         (raw-size (length raw*))
         (compressed-size (length compressed))
         (ccr-id (when store (anvil-context--ccr-put raw* kind*))))
    (anvil-context--gain-record kind* raw-size compressed-size)
    (list :kind kind*
          :compressed compressed
          :raw-size raw-size
          :compressed-size compressed-size
          :saved-size (max 0 (- raw-size compressed-size))
          :saved-percent (if (> raw-size 0)
                             (round (* 100.0 (/ (float (max 0 (- raw-size compressed-size)))
                                                raw-size)))
                           0)
          :ccr-id ccr-id
          :retrievable (and ccr-id t))))

(defun anvil-context--ccr-put (raw kind)
  "Store RAW under KIND and return a ccr id."
  (let ((id (format "ctx-%s-%s"
                    (format-time-string "%Y%m%d%H%M%S")
                    (substring (secure-hash 'sha1 raw) 0 12))))
    (anvil-state-set id
                     (list :raw raw
                           :kind kind
                           :created-at (truncate (float-time))
                           :raw-size (length raw))
                     :ns anvil-context--ccr-ns
                     :ttl anvil-context-ccr-ttl-sec)
    id))

(defun anvil-context-retrieve (ccr-id)
  "Return original context for CCR-ID, or nil when expired/missing."
  (anvil-state-get ccr-id :ns anvil-context--ccr-ns :default nil))

(defun anvil-context--gain-record (kind raw-size compressed-size)
  "Record compression telemetry for KIND."
  (let ((id (format "%d-%s-%s"
                    (truncate (* 1000 (float-time)))
                    kind
                    (substring (secure-hash 'sha1
                                            (format "%s/%s/%s"
                                                    kind raw-size compressed-size))
                               0 8))))
    (anvil-state-set id
                     (list :kind kind
                           :raw-size raw-size
                           :compressed-size compressed-size
                           :ts (truncate (float-time)))
                     :ns anvil-context--gain-ns
                     :ttl anvil-context-ccr-ttl-sec)))

(defun anvil-context-stats (&optional limit)
  "Return recent compression telemetry, capped by LIMIT entries."
  (let* ((lim (anvil-context--coerce-int limit 100))
         (keys (anvil-state-list-keys :ns anvil-context--gain-ns :limit lim))
         (rows (delq nil
                     (mapcar (lambda (key)
                               (anvil-state-get key
                                                :ns anvil-context--gain-ns
                                                :default nil))
                             keys)))
         (raw (apply #'+ (mapcar (lambda (row)
                                   (or (plist-get row :raw-size) 0))
                                 rows)))
         (cmp (apply #'+ (mapcar (lambda (row)
                                   (or (plist-get row :compressed-size) 0))
                                 rows)))
         (saved (max 0 (- raw cmp))))
    (list :count (length rows)
          :raw-size raw
          :compressed-size cmp
          :saved-size saved
          :saved-percent (if (> raw 0)
                             (round (* 100.0 (/ (float saved) raw)))
                           0)
          :approx-tokens-saved
          (if (> anvil-context-token-chars 0)
              (round (/ saved anvil-context-token-chars))
            0)
          :rows rows)))

;;;; --- MCP tools -----------------------------------------------------------

(defun anvil-context--tool-compress (raw &optional kind max_chars store)
  "Compress RAW with optional KIND hint and CCR storage.

MCP Parameters:
  raw       - Raw context text to compress.  Required.
  kind      - Optional kind: auto, json, diff, log, code, or text.
  max_chars - Optional max characters returned in the compressed view.
  store     - Non-nil stores the original and returns a retrievable ccr-id."
  (anvil-server-with-error-handling
   (anvil-context-compress raw
                           :kind kind
                           :max-chars max_chars
                           :store store)))

(defun anvil-context--tool-retrieve (ccr_id)
  "Retrieve original context stored by `context-compress'.

MCP Parameters:
  ccr_id - Identifier returned by `context-compress'."
  (anvil-server-with-error-handling
   (let ((row (and (anvil-context--nonempty-string ccr_id)
                   (anvil-context-retrieve ccr_id))))
     (list :ccr-id ccr_id
           :found (and row t)
           :kind (plist-get row :kind)
           :raw (or (plist-get row :raw) "")
           :raw-size (or (plist-get row :raw-size) 0)
           :created-at (plist-get row :created-at)))))

(defun anvil-context--tool-stats (&optional limit)
  "Return recent context compression savings.

MCP Parameters:
  limit - Optional maximum number of telemetry rows to aggregate."
  (anvil-server-with-error-handling
   (anvil-context-stats limit)))

(defconst anvil-context--tool-specs
  `((,(anvil-server-encode-handler #'anvil-context--tool-compress)
     :id "context-compress"
     :intent '(compression context rag)
     :layer 'io
     :description
     "Compress arbitrary context text through a local content router
(json/diff/log/code/text).  When STORE is non-nil, saves the raw
original in a CCR store and returns a ccr-id for `context-retrieve'."
     :read-only t)
    (,(anvil-server-encode-handler #'anvil-context--tool-retrieve)
     :id "context-retrieve"
     :intent '(compression context rag)
     :layer 'io
     :description
     "Retrieve raw context previously saved by `context-compress'
under a ccr-id.  Entries expire after `anvil-context-ccr-ttl-sec'."
     :read-only t)
    (,(anvil-server-encode-handler #'anvil-context--tool-stats)
     :id "context-stats"
     :intent '(compression context admin)
     :layer 'io
     :description
     "Summarise recent context-compress raw→compressed savings and
approximate tokens saved."
     :read-only t))
  "Spec list consumed by `anvil-server-register-tools'.")

(defun anvil-context--register-tools ()
  "Register context-* MCP tools."
  (anvil-server-register-tools anvil-context--server-id
                               anvil-context--tool-specs))

(defun anvil-context--unregister-tools ()
  "Unregister context-* MCP tools."
  (anvil-server-unregister-tools anvil-context--server-id
                                 anvil-context--tool-specs))

;;;###autoload
(defun anvil-context-enable ()
  "Enable reversible context compression MCP tools."
  (interactive)
  (anvil-state-enable)
  (anvil-context--register-tools))

;;;###autoload
(defun anvil-context-disable ()
  "Disable reversible context compression MCP tools."
  (interactive)
  (anvil-context--unregister-tools))

(provide 'anvil-context)

;;; anvil-context.el ends here
