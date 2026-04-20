;;; anvil-disclosure.el --- 3-layer progressive disclosure contract -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Official Layer-1 (index) / Layer-2 (search) / Layer-3 (get) contract
;; for anvil's read surface — Doc 28 Phase 1.  Provides two new MCP
;; tools:
;;
;;   org-index-index  — Layer 1 for org-mode headlines.  Slim pointers
;;                      (:id org://ID :title :path) so up to 50 rows
;;                      fit in ~1 000 tokens.  Handler delegates to
;;                      `anvil-org-index-search' then projects rows.
;;   disclosure-help  — Returns the 3-layer flow so LLM agents can
;;                      discover the contract at runtime.
;;
;; Existing Layer-2 / Layer-3 tools (org-index-search / org-read-by-id /
;; file-read / file-outline) are untouched at runtime; their tool
;; descriptions still carry Phase 0 language.  Description updates are
;; tracked separately in Doc 28 Phase 1 step 5 — see the design doc.

;;; Code:

(require 'cl-lib)
(require 'anvil-server)
(require 'anvil-uri)

;; Autoload-style: require lazily so unit tests that stub the search
;; function can run without the real anvil-org-index backend.
(declare-function anvil-org-index-search "anvil-org-index")

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
Layer 2  search  — `org-index-search', `defs-search'
                   ~80 tok/result with snippet.  Answers: which one
                   should I pick?
Layer 3  get     — `org-read-by-id', `file-read', `elisp-get-function-definition'
                   Full body, bounded by the source section / function
                   / file range.

Citation URIs:

  org://<ID>                       Layer 3: org-read-by-id
  defs://<sha>/<symbol>            Layer 3: elisp-get-function-definition
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
