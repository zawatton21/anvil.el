;;; anvil-uri.el --- Citation URI scheme for progressive disclosure -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Pure string helpers that format and parse anvil citation URIs.  The
;; URI scheme is shared across Doc 28 progressive-disclosure tools so
;; Layer 1 (*-index) output can be handed verbatim to Layer 3
;; (*-read-by-id / *-get) without intermediate translation.
;;
;; Supported schemes (see `anvil-uri-schemes'):
;;
;;   org://<ID>                       — org-mode :ID: property
;;   defs://<sha>/<symbol>            — elisp symbol in anvil-defs DB
;;   file://<PATH>[#L<start>[-<end>]] — file path, optional line range
;;   journal://<YYYY>/<ID>            — journal.org entry ID
;;   http-cache://<sha256>            — anvil-http cached response body
;;
;; Note: the design doc 28-progressive-disclosure.org originally wrote
;; the http-cache prefix as `http://', but that collides with real HTTP
;; URLs that LLMs also emit.  We use `http-cache://' here to keep the
;; scheme unambiguous when parsing; if a future phase decides otherwise
;; the constant below is the single flip point.
;;
;; No anvil-server dependency — this module is a leaf.

;;; Code:

(require 'cl-lib)

;;; Scheme registry

(defconst anvil-uri-schemes
  '((org         . "org://")
    (defs        . "defs://")
    (file        . "file://")
    (journal     . "journal://")
    (http-cache  . "http-cache://"))
  "Alist mapping anvil citation URI KIND symbols to their scheme prefixes.
Use `anvil-uri-scheme' to resolve a kind symbol to its prefix string.
Callers must never hardcode the prefix — route through the registry so
future scheme renames touch a single point.")

(defun anvil-uri-scheme (kind)
  "Return the URI prefix (string) registered for KIND symbol.
Signals `user-error' when KIND is not in `anvil-uri-schemes'."
  (or (cdr (assq kind anvil-uri-schemes))
      (user-error "anvil-uri: unknown kind %S" kind)))

;;; Formatters

(defun anvil-uri-org (id)
  "Return an org:// citation URI for ID (string)."
  (unless (and (stringp id) (not (string-empty-p id)))
    (error "anvil-uri-org: ID must be a non-empty string, got %S" id))
  (concat (anvil-uri-scheme 'org) id))

(defun anvil-uri-defs (sha symbol)
  "Return a defs:// citation URI for SHA/SYMBOL.
SHA is an arbitrary short digest identifying the definition version
(commit sha, file hash, or anvil-defs row hash).  SYMBOL is the
symbol name (string)."
  (unless (and (stringp sha) (not (string-empty-p sha)))
    (error "anvil-uri-defs: SHA must be a non-empty string"))
  (unless (and (stringp symbol) (not (string-empty-p symbol)))
    (error "anvil-uri-defs: SYMBOL must be a non-empty string"))
  (concat (anvil-uri-scheme 'defs) sha "/" symbol))

(defun anvil-uri-file (path &optional line-start line-end)
  "Return a file:// citation URI for PATH.
PATH must be absolute.  Backslashes are normalised to forward slashes
so the same URI is produced on Windows and POSIX.  Optional LINE-START
and LINE-END add an `#L<n>' or `#L<n>-<m>' fragment."
  (unless (and (stringp path) (not (string-empty-p path)))
    (error "anvil-uri-file: PATH must be a non-empty string"))
  (let* ((norm (replace-regexp-in-string "\\\\" "/" path))
         (abs  (if (and (> (length norm) 0) (eq (aref norm 0) ?/))
                   norm
                 (concat "/" norm)))
         (frag (cond
                ((and (integerp line-start) (integerp line-end)
                      (/= line-start line-end))
                 (format "#L%d-%d" line-start line-end))
                ((integerp line-start)
                 (format "#L%d" line-start))
                (t ""))))
    (concat (anvil-uri-scheme 'file) abs frag)))

(defun anvil-uri-journal (year id)
  "Return a journal:// citation URI for YEAR/ID.
YEAR may be an integer or 4-digit string.  ID is the org entry ID."
  (let ((yr (cond ((integerp year) (format "%04d" year))
                  ((stringp year) year)
                  (t (error "anvil-uri-journal: YEAR must be int/string")))))
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "anvil-uri-journal: ID must be a non-empty string"))
    (concat (anvil-uri-scheme 'journal) yr "/" id)))

(defun anvil-uri-http-cache (sha256)
  "Return an http-cache:// citation URI for SHA256 digest."
  (unless (and (stringp sha256) (not (string-empty-p sha256)))
    (error "anvil-uri-http-cache: SHA256 must be a non-empty string"))
  (concat (anvil-uri-scheme 'http-cache) sha256))

;;; Parser

(defun anvil-uri--strip-prefix (uri prefix)
  "Return URI with PREFIX removed, or nil when URI does not start with PREFIX."
  (and (stringp uri)
       (string-prefix-p prefix uri)
       (substring uri (length prefix))))

(defun anvil-uri-kind (uri)
  "Return the KIND symbol for URI, or nil when no scheme matches."
  (cl-loop for (kind . prefix) in anvil-uri-schemes
           when (string-prefix-p prefix uri)
           return kind))

(defun anvil-uri--parse-file-fragment (body)
  "Split file URI BODY into (PATH LINE-START LINE-END).
BODY is everything after the `file://' prefix."
  (if (string-match "\\`\\(.*?\\)#L\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?\\'"
                    body)
      (let* ((path (match-string 1 body))
             (start (string-to-number (match-string 2 body)))
             (end (and (match-string 3 body)
                       (string-to-number (match-string 3 body)))))
        (list path start (or end start)))
    (list body nil nil)))

(defun anvil-uri-parse (uri)
  "Parse URI string and return a plist describing its parts.
Returns nil when URI has no recognised anvil scheme.  Plist shape:

  org://ID                  → (:scheme org :id ID)
  defs://SHA/SYM            → (:scheme defs :sha SHA :symbol SYM)
  file:///PATH[#L1[-2]]     → (:scheme file :path PATH
                                 :line-start S :line-end E)
  journal://YYYY/ID         → (:scheme journal :year YYYY :id ID)
  http-cache://SHA256       → (:scheme http-cache :sha SHA256)"
  (when-let* ((kind (anvil-uri-kind uri))
              (prefix (anvil-uri-scheme kind))
              (body (substring uri (length prefix))))
    (pcase kind
      ('org
       (list :scheme 'org :id body))
      ('defs
       (if (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" body)
           (list :scheme 'defs
                 :sha (match-string 1 body)
                 :symbol (match-string 2 body))
         (list :scheme 'defs :sha body :symbol nil)))
      ('file
       (pcase-let ((`(,path ,start ,end)
                    (anvil-uri--parse-file-fragment body)))
         ;; Drop the leading slash we prepended in `anvil-uri-file' for
         ;; POSIX paths, but leave Windows `C:/..' intact.
         (let ((cleaned
                (cond
                 ((string-match-p "\\`/[A-Za-z]:/" path) (substring path 1))
                 (t path))))
           (list :scheme 'file :path cleaned
                 :line-start start :line-end end))))
      ('journal
       (if (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" body)
           (list :scheme 'journal
                 :year (match-string 1 body)
                 :id (match-string 2 body))
         (list :scheme 'journal :year body :id nil)))
      ('http-cache
       (list :scheme 'http-cache :sha body)))))

(provide 'anvil-uri)
;;; anvil-uri.el ends here
