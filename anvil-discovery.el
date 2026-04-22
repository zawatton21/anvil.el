;;; anvil-discovery.el --- Intent-based MCP tool discovery  -*- lexical-binding: t; -*-

;; Author: zawatton
;; Package-Requires: ((emacs "29.1") (anvil-server "0.1"))
;; Keywords: tools, mcp

;;; Commentary:

;; Doc 34 Phase A — `anvil-tools-by-intent' MCP tool.
;;
;; Discovery layer over the registry kept by `anvil-server': reads
;; `:intent' / `:layer' / `:stability' metadata attached at
;; registration time and answers "which tools match this intent?"
;; queries without the caller having to read CLAUDE.md or walk the
;; module tree by hand.
;;
;; The registry itself is `anvil-server--tools'; this module only
;; reads it.  Tools without metadata fall back to sensible defaults
;; (:intent (general), :layer core, :stability stable) so existing
;; registrations keep working.
;;
;; See docs/design/34-tool-discovery.org for the design rationale.

;;; Code:

(require 'cl-lib)
(require 'anvil-server)

(defconst anvil-discovery--server-id "emacs-eval"
  "Server id under which discovery MCP tools register.")

(defconst anvil-discovery--default-intent '(general)
  "Intent list applied to tools that registered without `:intent'.")

(defconst anvil-discovery--default-layer 'core
  "Layer applied to tools that registered without `:layer'.")

(defconst anvil-discovery--default-stability 'stable
  "Stability applied to tools that registered without `:stability'.")

(defconst anvil-discovery--layer-rank
  '((core . 0) (io . 1) (workflow . 2) (dev . 3))
  "Sort rank for layers; lower first in discovery output.")


;;;; --- metadata accessors -------------------------------------------------

(defun anvil-discovery--tool-intent (tool)
  "Return TOOL's `:intent' list, falling back to the default."
  (or (plist-get tool :intent) anvil-discovery--default-intent))

(defun anvil-discovery--tool-layer (tool)
  "Return TOOL's `:layer' symbol, falling back to the default."
  (or (plist-get tool :layer) anvil-discovery--default-layer))

(defun anvil-discovery--tool-stability (tool)
  "Return TOOL's `:stability' symbol, falling back to the default."
  (or (plist-get tool :stability) anvil-discovery--default-stability))

(defun anvil-discovery--layer-rank (layer)
  "Sort rank for LAYER; unknown layers sort last."
  (or (cdr (assq layer anvil-discovery--layer-rank)) 99))


;;;; --- query ---------------------------------------------------------------

(defun anvil-discovery--parse-intent-arg (arg)
  "Normalise ARG (intent query) to a list of symbols or nil.
ARG may be nil, a symbol, a single string, or a comma-separated
string of intent names."
  (cond
   ((null arg) nil)
   ((symbolp arg) (list arg))
   ((and (stringp arg) (string-empty-p arg)) nil)
   ((stringp arg)
    (mapcar (lambda (s) (intern (string-trim s)))
            (split-string arg "," t)))
   ((listp arg) arg)
   (t (error "anvil-discovery: unsupported intent arg %S" arg))))

(defun anvil-discovery--parse-layer-arg (arg)
  "Normalise ARG to a layer symbol, or nil for `any'."
  (cond
   ((null arg) nil)
   ((symbolp arg) arg)
   ((and (stringp arg) (string-empty-p arg)) nil)
   ((stringp arg) (intern arg))
   (t (error "anvil-discovery: unsupported layer arg %S" arg))))

(defun anvil-discovery--match-p (tool query-re intent-syms layer-sym
                                      include-experimental)
  "Return non-nil when TOOL matches the query parameters.
INTENT-SYMS is the caller's intent list (nil = any); LAYER-SYM is the
layer filter (nil = any); QUERY-RE is a precompiled regexp or nil;
INCLUDE-EXPERIMENTAL is non-nil to keep `:stability experimental'."
  (let ((stability (anvil-discovery--tool-stability tool))
        (layer     (anvil-discovery--tool-layer tool))
        (intents   (anvil-discovery--tool-intent tool)))
    (and
     ;; Stability gate: deprecated always excluded; experimental gated.
     (not (eq stability 'deprecated))
     (or include-experimental (not (eq stability 'experimental)))
     ;; Layer filter.
     (or (null layer-sym) (eq layer-sym layer))
     ;; Intent filter: at least one common symbol.
     (or (null intent-syms)
         (cl-intersection intent-syms intents))
     ;; Regex filter over id + description.
     (or (null query-re)
         (let ((id   (or (plist-get tool :id) ""))
               (desc (or (plist-get tool :description) "")))
           (or (string-match-p query-re id)
               (string-match-p query-re desc)))))))

(defun anvil-discovery--intent-overlap (a b)
  "Count common symbols between intent lists A and B."
  (length (cl-intersection a b)))

(defun anvil-discovery--tool-summary (server-id tool query-intents)
  "Return a plist summarising TOOL for MCP response output.
QUERY-INTENTS is used to compute an overlap score for sorting."
  (let* ((id (or (plist-get tool :id) ""))
         (desc (or (plist-get tool :description) ""))
         (oneline (car (split-string desc "\n")))
         (intents (anvil-discovery--tool-intent tool))
         (layer (anvil-discovery--tool-layer tool))
         (stability (anvil-discovery--tool-stability tool)))
    (list :id id
          :server-id server-id
          :layer layer
          :intent intents
          :stability stability
          :description oneline
          :_layer-rank (anvil-discovery--layer-rank layer)
          :_intent-score (anvil-discovery--intent-overlap
                          intents (or query-intents '())))))

(defun anvil-discovery--collect (intent-syms layer-sym query-re
                                             include-experimental)
  "Walk `anvil-server--tools' and return matching summaries."
  (let (results)
    (when (hash-table-p anvil-server--tools)
      (maphash
       (lambda (server-id tools-table)
         (when (hash-table-p tools-table)
           (maphash
            (lambda (_tool-id tool)
              (when (anvil-discovery--match-p
                     tool query-re intent-syms layer-sym
                     include-experimental)
                (push (anvil-discovery--tool-summary
                       server-id tool intent-syms)
                      results)))
            tools-table)))
       anvil-server--tools))
    results))

(defun anvil-discovery--sort (summaries)
  "Sort SUMMARIES: layer rank asc, intent score desc, id asc.
Strips the internal `_layer-rank' / `_intent-score' fields."
  (let ((sorted
         (sort summaries
               (lambda (a b)
                 (let ((la (plist-get a :_layer-rank))
                       (lb (plist-get b :_layer-rank)))
                   (cond
                    ((/= la lb) (< la lb))
                    ((/= (plist-get a :_intent-score)
                         (plist-get b :_intent-score))
                     (> (plist-get a :_intent-score)
                        (plist-get b :_intent-score)))
                    (t (string-lessp (plist-get a :id)
                                     (plist-get b :id)))))))))
    (mapcar (lambda (s)
              (let ((clean (cl-copy-list s)))
                (setq clean (anvil-discovery--plist-remove clean :_layer-rank))
                (setq clean (anvil-discovery--plist-remove clean :_intent-score))
                clean))
            sorted)))

(defun anvil-discovery--plist-remove (plist key)
  "Return PLIST with KEY/value removed (non-destructive)."
  (let (out (p plist))
    (while p
      (unless (eq (car p) key)
        (push (car p) out)
        (push (cadr p) out))
      (setq p (cddr p)))
    (nreverse out)))


;;;; --- MCP tool handler ---------------------------------------------------

(defun anvil-discovery-tools-by-intent (query intent layer include-experimental)
  "Return registered MCP tools matching the given filters.

MCP Parameters:
  query                - Optional regex over tool id or description.
                         Empty string means no filter.
  intent               - Optional comma-separated intent symbols
                         (e.g. \"file-edit,org-read\"); empty string
                         means no intent filter.
  layer                - Optional layer: \"core\" / \"io\" / \"workflow\"
                         / \"dev\"; empty string means no layer filter.
  include-experimental - If \"t\" / \"true\" / \"1\" the response includes
                         tools flagged :stability experimental.
                         Deprecated tools are always excluded.

Returns a plist (:count N :tools (...)) where TOOLS are sorted by
layer (core first), intent overlap with the query, and id."
  (anvil-server-with-error-handling
    (let* ((query-str (and (stringp query)
                           (not (string-empty-p query))
                           query))
           (query-re (and query-str
                          (condition-case err
                              (progn (string-match-p query-str "") query-str)
                            (invalid-regexp
                             (anvil-server-tool-throw
                              (format "invalid query regexp: %s"
                                      (cadr err)))))))
           (intent-syms (anvil-discovery--parse-intent-arg intent))
           (layer-sym   (anvil-discovery--parse-layer-arg layer))
           (include-exp (and (stringp include-experimental)
                             (member (downcase include-experimental)
                                     '("t" "true" "1" "yes"))))
           (raw (anvil-discovery--collect
                 intent-syms layer-sym query-re include-exp))
           (sorted (anvil-discovery--sort raw)))
      (list :count (length sorted)
            :tools sorted))))


;;;; --- lifecycle ----------------------------------------------------------

(defun anvil-discovery--register-tools ()
  "Register the Doc 34 Phase A discovery MCP tool."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-discovery-tools-by-intent)
   :id "anvil-tools-by-intent"
   :server-id anvil-discovery--server-id
   :description
   "Discover registered MCP tools by intent / layer / regex without
reading CLAUDE.md.  Filters: intent (comma-separated symbols, e.g.
\"file-edit\" or \"org-read,structure\"), layer (core / io / workflow /
dev), query (regex over id and description), include-experimental
(truthy string).  Returns (:count N :tools (...)) sorted by layer
rank, intent overlap, and id."
   :read-only t
   :intent '(meta discovery)
   :layer 'dev
   :stability 'stable))

(defun anvil-discovery--unregister-tools ()
  "Unregister the Doc 34 Phase A discovery tool."
  (ignore-errors
    (anvil-server-unregister-tool "anvil-tools-by-intent"
                                  anvil-discovery--server-id)))

;;;###autoload
(defun anvil-discovery-enable ()
  "Enable Doc 34 Phase A — register `anvil-tools-by-intent'."
  (interactive)
  (anvil-discovery--register-tools))

;;;###autoload
(defun anvil-discovery-disable ()
  "Disable Doc 34 Phase A — unregister `anvil-tools-by-intent'."
  (interactive)
  (anvil-discovery--unregister-tools))

(provide 'anvil-discovery)
;;; anvil-discovery.el ends here
