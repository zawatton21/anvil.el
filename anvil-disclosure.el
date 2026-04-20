;;; anvil-disclosure.el --- 3-layer progressive disclosure contract -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Official Layer-1 (index) / Layer-2 (search) / Layer-3 (get) contract
;; for anvil's read surface — Doc 28 Phase 1 + Phase 2.  Provides four
;; new MCP tools:
;;
;;   org-index-index   — Layer 1 for org-mode headlines.  Slim pointers
;;                       (:id org://ID :title :path) so up to 50 rows
;;                       fit in ~1 000 tokens.  Handler delegates to
;;                       `anvil-org-index-search' then projects rows.
;;   file-read-snippet — Layer 2 for files.  Returns a bounded window
;;                       around a given line (default 20 lines) with a
;;                       `file://PATH#L<s>-<e>' citation URI reusable
;;                       in Layer 3.  Delegates to `anvil-file-read-region'.
;;   defs-index        — Layer 1 for elisp symbol definitions.  Slim
;;                       pointers (:id defs://0/SYM :symbol :file :kind)
;;                       projected from `anvil-defs-search' (fuzzy).
;;                       SHA segment is "0" in Phase 2 (no row-level
;;                       versioning yet); reserved for a future hash.
;;   disclosure-help   — Returns the 3-layer flow so LLM agents can
;;                       discover the contract at runtime.
;;
;; Existing Layer-2 / Layer-3 tools (org-index-search / org-read-by-id /
;; file-read / file-outline / defs-search / elisp-get-function-definition)
;; keep their handlers untouched; their descriptions are updated in the
;; owning modules to use the Layer-N template language.

;;; Code:

(require 'cl-lib)
(require 'anvil-server)
(require 'anvil-uri)

;; Autoload-style: require lazily so unit tests that stub the search
;; function can run without the real anvil-org-index / anvil-file /
;; anvil-defs backends.
(declare-function anvil-org-index-search "anvil-org-index")
(declare-function anvil-file-read-region "anvil-file")
(declare-function anvil-defs-search "anvil-defs")

;;; Customization

(defgroup anvil-disclosure nil
  "Progressive-disclosure Layer-1 / Layer-2 / Layer-3 contract."
  :group 'anvil
  :prefix "anvil-disclosure-")

(defcustom anvil-disclosure-layer1-default-limit 50
  "Default `:limit' applied to Layer-1 (slim) queries."
  :type 'integer
  :group 'anvil-disclosure)

(defcustom anvil-disclosure-layer1-hard-limit 200
  "Hard upper bound for Layer-1 result counts.
Caps user-supplied limits to protect the token budget."
  :type 'integer
  :group 'anvil-disclosure)

(defcustom anvil-disclosure-layer1-budget-tokens 25
  "Soft token budget per Layer-1 result.
The budget ERT fails when average per-result size exceeds this value,
flagging design drift.  Keep in sync with the design doc."
  :type 'integer
  :group 'anvil-disclosure)

(defcustom anvil-disclosure-snippet-window-default 20
  "Default `window' size for `file-read-snippet'.
The window is centred on the supplied line, i.e. W/2 lines before
and W/2 after (bounded by file edges).  Keep small — Layer 2 snippets
should fit under ~500 tokens per call."
  :type 'integer
  :group 'anvil-disclosure)

(defcustom anvil-disclosure-snippet-window-max 100
  "Hard upper bound for `file-read-snippet' window size."
  :type 'integer
  :group 'anvil-disclosure)

;;; Layer-1 projection helpers

(defun anvil-disclosure--row-to-pointer (row)
  "Project a Layer-2 ROW plist into a Layer-1 pointer plist.
ROW is a single entry from `anvil-org-index-search' :rows — typically
carrying :file :line :title :org-id (+ richer fields).  Returns
(:id URI :title TITLE :path PATH), skipping rows whose :org-id is
nil (they cannot be cited, so they have no stable URI yet)."
  (let* ((id (plist-get row :org-id))
         (title (or (plist-get row :title) ""))
         (path (or (plist-get row :file) "")))
    (when (and id (stringp id) (not (string-empty-p id)))
      (list :id (anvil-uri-org id)
            :title title
            :path path))))

(defun anvil-disclosure--project-rows (rows)
  "Map ROWS (list of plists) to Layer-1 pointers, dropping ones without an ID."
  (delq nil (mapcar #'anvil-disclosure--row-to-pointer rows)))

;;; Layer-1 handler

(defun anvil-disclosure--clamp-limit (raw)
  "Return an integer limit clamped into the Layer-1 budget range."
  (let ((n (cond ((integerp raw) raw)
                 ((and (stringp raw) (not (string-empty-p (string-trim raw))))
                  (string-to-number raw))
                 (t anvil-disclosure-layer1-default-limit))))
    (min (max 1 n) anvil-disclosure-layer1-hard-limit)))

(defun anvil-disclosure--none-empty (s)
  "Return S when non-empty string, else nil."
  (and s (stringp s) (not (string-empty-p (string-trim s))) s))

;;;###autoload
(defun anvil-disclosure-org-index-index (&optional query path limit)
  "Layer-1 slim pointers for org-mode.
Returns (:count N :truncated BOOL :rows ((:id URI :title T :path P) ...))
where each :id is a reusable `org://ID' citation URI — hand it directly
to `org-read-by-id' (Layer 3) for the full body.

QUERY is wrapped as SQL LIKE against headline title.  PATH is a LIKE
against the absolute org-file path.  LIMIT caps the row count
(default `anvil-disclosure-layer1-default-limit', max
`anvil-disclosure-layer1-hard-limit')."
  (require 'anvil-org-index)
  (let* ((lim (anvil-disclosure--clamp-limit limit))
         (q   (anvil-disclosure--none-empty query))
         (p   (anvil-disclosure--none-empty path))
         (res (anvil-org-index-search
               :title-like q
               :file-like  p
               :limit      lim))
         (rows (plist-get res :rows))
         (pointers (anvil-disclosure--project-rows rows)))
    (list :count     (length pointers)
          :truncated (plist-get res :truncated)
          :rows      pointers)))

(defun anvil-disclosure--tool-org-index-index
    (&optional query path limit)
  "MCP wrapper for `anvil-disclosure-org-index-index'.

MCP Parameters:
  query - Optional title substring (SQL LIKE; auto-wrapped with %..%).
  path  - Optional absolute file-path substring (SQL LIKE).
  limit - Optional integer string, default 50, hard-capped at 200."
  (anvil-server-with-error-handling
   (format "%S"
           (anvil-disclosure-org-index-index query path limit))))

;;; Layer-2 file-read-snippet

(defun anvil-disclosure--clamp-window (raw)
  "Clamp RAW to `[1, anvil-disclosure-snippet-window-max]'."
  (let ((n (cond ((integerp raw) raw)
                 ((and (stringp raw) (not (string-empty-p (string-trim raw))))
                  (string-to-number raw))
                 (t anvil-disclosure-snippet-window-default))))
    (min (max 1 n) anvil-disclosure-snippet-window-max)))

(defun anvil-disclosure--snippet-bounds (line window total)
  "Return (START . END) 1-indexed line bounds for a WINDOW centred on LINE.
TOTAL is the line count of the file; bounds are clamped to [1, TOTAL].
When TOTAL is nil the upper bound is not clamped."
  (let* ((half  (max 1 (/ window 2)))
         (start (max 1 (- line half)))
         (end   (+ start (1- window)))
         (end   (if total (min end total) end))
         ;; If we were pushed against the top edge, the file may be
         ;; shorter than the window — widen the start so we still fill.
         (start (if (and total (< (1+ (- end start)) window))
                    (max 1 (- end (1- window)))
                  start)))
    (cons start end)))

;;;###autoload
(defun anvil-disclosure-file-read-snippet (path line &optional window)
  "Layer-2 snippet for a file.
Return a WINDOW-line window of PATH centred on LINE (default
`anvil-disclosure-snippet-window-default').  The returned plist:

  (:id     URI          file:// citation URI scoped to the window
   :path   PATH         absolute path
   :line-start S        1-indexed first line of the returned body
   :line-end   E        1-indexed last line (inclusive)
   :body   STRING       verbatim slice
   :total-lines N       file length
   :truncated BOOL      t when WINDOW was clamped smaller than
                        the requested size)

Delegates to `anvil-file-read-region'."
  (require 'anvil-file)
  (unless (integerp line)
    (error "anvil-disclosure-file-read-snippet: LINE must be integer"))
  (let* ((abs (expand-file-name path))
         (total (when (file-readable-p abs)
                  (with-temp-buffer
                    (insert-file-contents abs)
                    (count-lines (point-min) (point-max)))))
         (w   (anvil-disclosure--clamp-window window))
         (pr  (anvil-disclosure--snippet-bounds line w total))
         (s   (car pr))
         (e   (cdr pr))
         (body (anvil-file-read-region abs s e)))
    (list :id (anvil-uri-file abs s e)
          :path abs
          :line-start s
          :line-end e
          :body body
          :total-lines (or total 0)
          :truncated (< (1+ (- e s)) w))))

(defun anvil-disclosure--tool-file-read-snippet (path line &optional window)
  "MCP wrapper for `anvil-disclosure-file-read-snippet'.

MCP Parameters:
  path   - Absolute path to the file (string).
  line   - 1-indexed line to centre the snippet on (string or int).
  window - Optional line-count window (string or int), default 20,
           hard-capped at 100."
  (anvil-server-with-error-handling
   (let ((ln (cond ((integerp line) line)
                   ((and (stringp line) (not (string-empty-p line)))
                    (string-to-number line))
                   (t (error "line is required")))))
     (format "%S"
             (anvil-disclosure-file-read-snippet path ln window)))))

;;; Layer-1 defs-index

(defconst anvil-disclosure--defs-sha-placeholder "0"
  "SHA segment used in `defs://' URIs before Phase 3 row-hashing.
Reserved so the scheme can be version-bumped later without a
breaking URI change.  The segment is syntactic — parsers must not
reason about it semantically.")

(defun anvil-disclosure--defs-row-to-pointer (row)
  "Project a `anvil-defs-search' ROW plist into a Layer-1 defs pointer.
Returns (:id defs://0/NAME :symbol NAME :file PATH :kind KIND)."
  (let ((name (plist-get row :name))
        (file (plist-get row :file))
        (kind (plist-get row :kind)))
    (when (and name (stringp name) (not (string-empty-p name)))
      (list :id (anvil-uri-defs anvil-disclosure--defs-sha-placeholder
                                name)
            :symbol name
            :file (or file "")
            :kind (or kind "")))))

(defun anvil-disclosure--defs-project-rows (rows)
  "Map ROWS to Layer-1 defs pointers, dropping rows without a :name."
  (delq nil (mapcar #'anvil-disclosure--defs-row-to-pointer rows)))

;;;###autoload
(defun anvil-disclosure-defs-index (query &optional limit)
  "Layer-1 slim pointers for elisp definitions.
QUERY is a substring (fuzzy LIKE match) of the symbol name.
Returns (:count N :rows ((:id URI :symbol S :file F :kind K) ...))
where each :id is a `defs://0/SYM' citation URI reusable in Layer 3
via `elisp-get-function-definition'.  LIMIT defaults to 50 and is
hard-capped at `anvil-disclosure-layer1-hard-limit'."
  (require 'anvil-defs)
  (let* ((q   (anvil-disclosure--none-empty query))
         (lim (anvil-disclosure--clamp-limit limit))
         (rows (and q (anvil-defs-search q :fuzzy t :limit lim)))
         (pointers (anvil-disclosure--defs-project-rows rows)))
    (list :count (length pointers)
          :rows  pointers)))

(defun anvil-disclosure--tool-defs-index (query &optional limit)
  "MCP wrapper for `anvil-disclosure-defs-index'.

MCP Parameters:
  query - Symbol-name substring to match (string, required, fuzzy).
  limit - Optional integer string, default 50, hard-capped at 200."
  (anvil-server-with-error-handling
   (format "%S"
           (anvil-disclosure-defs-index query limit))))

;;; disclosure-help

(defconst anvil-disclosure--help-text
  "anvil progressive-disclosure contract (Doc 28)
=================================================

Use one of three layers *in order* — escalating only when the cheaper
layer matched.  Every layer hands back a citation URI you can feed
directly into Layer 3.

Layer 1  index   — `org-index-index', `file-outline', `defs-index'
                   ~20 tok/result, up to 50 results.  Answers: does
                   anything match, and where?
Layer 2  search  — `org-index-search', `defs-search',
                   `file-read-snippet' (single windowed excerpt)
                   ~80 tok/row (multi-result) or ~500 tok (snippet).
                   Answers: which one should I pick, or what does a
                   known line look like in context?
Layer 3  get     — `org-read-by-id', `file-read', `elisp-get-function-definition'
                   Full body, bounded by the source section / function
                   / file range.  All three Layer-3 tools accept the
                   citation URI emitted at Layer 1/2 directly — no
                   intermediate translation needed.

Citation URIs:

  org://<ID>                       Layer 3: org-read-by-id
  defs://<sha>/<symbol>            Layer 3: elisp-get-function-definition
                                   sha is \"0\" in Phase 2 (reserved
                                   for future row-hash versioning).
  file://<PATH>[#L<n>[-<m>]]       Layer 3: file-read
  journal://<YYYY>/<ID>            Layer 3: org-read-by-id (journal)
  http-cache://<sha256>            Layer 3: http-cache-get (future)

Rule of thumb: start at Layer 1.  If nothing relevant matched, do not
escalate — adjust the query instead.  Never call Layer 3 (e.g.,
`file-read') until you have a citation URI from Layer 1 or 2; reading
a whole org file when org-index-index would have answered in <1 kB of
context is a 5-10x token waste."
  "Static 3-layer flow documentation returned by `disclosure-help'.")

(defun anvil-disclosure-help-handler ()
  "Return the 3-layer progressive-disclosure contract as plain text.

MCP Parameters: (none)"
  (anvil-server-with-error-handling
   anvil-disclosure--help-text))

;;; Lifecycle

(defconst anvil-disclosure--tool-specs
  `((,#'anvil-disclosure--tool-org-index-index
     :id "org-index-index"
     :description
     "Layer 1 of anvil progressive disclosure for org-mode headlines. Use FIRST for any org query. Returns up to 50 slim pointers at ~20 tokens each, each with a citation URI (org://ID) reusable as-is in Layer 3 (`org-read-by-id'). Escalate to Layer 2 (`org-index-search') only when Layer 1 matched and you need snippets to disambiguate. DO NOT use `file-read' for org files — org-index-index is 10x cheaper. Both `query' (title substring) and `path' (file-path substring) are optional; omit to enumerate."
     :read-only t
     :title "Org Index (Layer 1)")
    (,#'anvil-disclosure--tool-file-read-snippet
     :id "file-read-snippet"
     :description
     "Layer 2 of anvil progressive disclosure for files. Returns a bounded window (default 20 lines, max 100) around `line' in `path' so you can read enough context to decide whether to escalate to Layer 3 `file-read'. Emits a `file://PATH#L<s>-<e>' citation URI that can be fed directly back into `file-read'. Cheaper than reading the whole file when you already know roughly where to look (e.g., from `file-outline' Layer 1 output or from `defs-search')."
     :read-only t
     :title "File Read Snippet (Layer 2)")
    (,#'anvil-disclosure--tool-defs-index
     :id "defs-index"
     :description
     "Layer 1 of anvil progressive disclosure for elisp definitions. Use FIRST when asking \"where is X defined?\". Returns up to 50 slim pointers at ~20 tokens each ({:id defs://0/SYM :symbol :file :kind}). Each URI feeds directly into Layer 3 (`elisp-get-function-definition'). Escalate to Layer 2 (`defs-search') only when you need arity / docstring / obsolete-p metadata to disambiguate."
     :read-only t
     :title "Defs Index (Layer 1)")
    (,#'anvil-disclosure-help-handler
     :id "disclosure-help"
     :description
     "Return the anvil 3-layer progressive-disclosure contract (Layer 1 index → Layer 2 search → Layer 3 get) and the citation URI scheme. Call this once at session start if you are unsure which tool to reach for when reading anvil-managed content."
     :read-only t
     :title "Disclosure Help"))
  "MCP tool specs provided by `anvil-disclosure'.")

;;;###autoload
(defun anvil-disclosure-enable ()
  "Register the Layer-1 org-index-index tool and the disclosure-help tool."
  (interactive)
  (anvil-server-register-tools "emacs-eval"
                               anvil-disclosure--tool-specs))

(defun anvil-disclosure-disable ()
  "Unregister the tools installed by `anvil-disclosure-enable'."
  (interactive)
  (anvil-server-unregister-tools "emacs-eval"
                                 anvil-disclosure--tool-specs))

(provide 'anvil-disclosure)
;;; anvil-disclosure.el ends here
