;;; anvil-http.el --- HTTP client for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Lightweight wrapper around Emacs' built-in `url-retrieve-synchronously'
;; exposing three MCP tools — `http-fetch' (GET), `http-head' (HEAD) and
;; `http-cache-clear' — for LLM clients that need to fetch static HTTP
;; resources without paying for a full browser session.
;;
;; Design doc: docs/design/09-http.org.
;;
;; Name-collision note: `anvil-net' is unrelated — it inspects listening
;; sockets (netstat / ss / lsof wrapper).  HTTP client concerns live here.
;;
;; Phase 1a scope (this file):
;;   * `anvil-http-get' / `anvil-http-head' / `anvil-http-cache-clear'
;;     Elisp API + matching MCP tools
;;   * SQLite-backed cache via `anvil-state' (ns = "http")
;;   * Conditional GET: If-None-Match / If-Modified-Since on revalidate,
;;     304 serves body from cache and refreshes fetched-at
;;   * TTL fast path (`anvil-http-cache-ttl-sec'): within-window fetches
;;     never touch the network
;;   * Retry on 5xx / 408 / 429 with exponential backoff
;;     (`Retry-After' honoured for 429)
;;   * `url-max-redirections' clamped to 5, http/https-only
;;
;; Phase 1b (selector / json-path extract) is live as of the
;; release containing this comment:
;;   * `:selector' kwarg on `anvil-http-get' (and `selector' MCP
;;     parameter on `http-fetch') evaluates a simple CSS-subset
;;     against text/html (and application/xhtml+xml) responses.
;;     libxml + dom.el is the primary path; a regex-subset
;;     fallback handles ports without libxml.
;;   * `:json-path' kwarg / `json_path' MCP parameter walks a
;;     dotted path through `application/json' responses.  Wildcard
;;     segment `[*]' flattens an array.
;;   * When extraction is requested but nothing matches, the full
;;     body is still returned together with `:extract-miss t' so
;;     callers never receive a silent empty payload.
;;
;; Phase 1c (overflow + header-filter) is live as of the release
;; containing this comment:
;;   * `:body-mode' kwarg / `body_mode' MCP parameter: `auto'
;;     (default) spills bodies over `anvil-http-max-inline-body-bytes'
;;     (200KB) to a hashed tempfile and returns only a head slice +
;;     `:body-overflow-path' / `:body-sha256' / `:total-bytes'.
;;     `full' inlines regardless of size, `head-only' drops the tail
;;     without spilling, `meta-only' drops the body entirely.
;;   * `:header-filter' kwarg / `header_filter' MCP parameter:
;;     `minimal' (default) keeps only the 7 keys Claude actually
;;     uses (Content-Type, Content-Length, ETag, Last-Modified,
;;     Content-Encoding, Location, Retry-After); `all' returns the
;;     full response header set.
;;
;; Phase 1e (robots.txt) is live as of the release containing this
;; comment:
;;   * `anvil-http-respect-robots-txt' (default t) gates robots.txt
;;     enforcement.  When non-nil, `anvil-http-get' fetches the
;;     origin's /robots.txt (24h TTL cache in
;;     `anvil-http--robots-state-ns'), parses RFC 9309 Allow /
;;     Disallow rules, picks the longest-matching User-agent group,
;;     and raises `user-error' on a Disallow match before any
;;     network round-trip to the target URL.
;;   * `http-robots-check' MCP tool exposes the same evaluator
;;     standalone — a caller can test a URL without side effects.
;;   * Fail-open: a non-200 or failing robots.txt fetch lets the
;;     request proceed, matching RFC 9309 guidance for missing or
;;     unreadable robots files.
;;
;; Phase 1.5 (POST + auth + retry jitter) is live as of the release
;; containing this comment:
;;   * `anvil-http-post URL &key body content-type headers auth ...':
;;     Elisp API.  `:body' accepts a string (verbatim), an alist of
;;     (KEY . VALUE) (auto-encoded as application/x-www-form-urlencoded),
;;     or a plist (auto-encoded as application/json).  `:auth' takes
;;     `(:bearer TOKEN)' / `(:basic (USER . PASS))' / `(:header (NAME
;;     . VALUE))', or a list thereof.
;;   * `http-post' MCP tool: body string + optional content_type,
;;     bearer_token, headers_json (JSON object of extra headers),
;;     accept, timeout_sec, body_mode, header_filter.  Mirrors
;;     http-fetch's response shape; POSTs are never cached.
;;   * Retry backoff now applies +/-`anvil-http-retry-jitter-ratio'
;;     (default 25%) random jitter; cuts thunder-herd risk when
;;     many clients retry the same upstream simultaneously.  Set the
;;     ratio to 0 to restore deterministic backoff.
;;
;; Phase 3 (LRU eviction + size cap + metrics) is live as of the
;; release containing this comment:
;;   * `anvil-http-cache-size-cap-bytes' (default 50 MB) is a soft
;;     cap on the http cache.  When `--cache-put' would push the
;;     total over the cap, the oldest entries (by `:fetched-at')
;;     are evicted until the total falls back under.  Set to 0 to
;;     disable size-cap eviction (TTL still expires lazily).
;;   * Running byte counter `anvil-http--cache-bytes-counter' is
;;     maintained incrementally by put/delete/clear; recomputed
;;     from SQLite on `anvil-http-enable' so a fresh daemon starts
;;     from truth.
;;   * Metrics gain `:evictions' counter; `http-cache-status' MCP
;;     tool reports entries / bytes / cap / evictions plus the
;;     existing request-side counters for live observability.
;;
;; Phase 2 (offload) is live as of the release containing this
;; comment:
;;   * `:offload t' kwarg on `anvil-http-get' / `anvil-http-post'
;;     (and `offload' MCP parameter on `http-fetch' / `http-post')
;;     dispatches the actual fetch to an `anvil-offload' worker
;;     subprocess and blocks on the future.  The main daemon stays
;;     responsive while the network round-trip runs out of process
;;     — useful for slow APIs, large downloads, or fetches that
;;     would otherwise pin the event loop.
;;   * `:offload-timeout' kwarg / `offload_timeout_sec' MCP param
;;     bounds the await; defaults to
;;     `anvil-http-offload-timeout-sec' (120s).  Worker-side errors
;;     and await timeouts surface as `anvil-server-tool-error' so
;;     existing error handling keeps working.
;;   * Cache reads / writes are skipped on the worker side (the
;;     parent daemon owns the SQLite file); robots.txt is evaluated
;;     in the parent before dispatch, so the worker bypasses it.
;;
;; Phase 1d (batch fetch) is live as of the release containing this
;; comment:
;;   * `anvil-http-get-batch URLS &key concurrency timeout ...'
;;     fetches multiple URLs concurrently and returns the response
;;     plists in input order.  Per-URL failures surface as
;;     `(:url :error)' so one bad entry does not sink the batch.
;;     TTL-fresh cache hits short-circuit the async fan-out
;;     entirely — a mostly-cached batch costs one envelope and zero
;;     network.
;;   * `http-fetch-batch' MCP tool wraps the Elisp API.  The `urls'
;;     parameter accepts a JSON array; shared `selector',
;;     `json_path', `body_mode', `header_filter', `no_cache',
;;     `concurrency' and `timeout_sec' apply to every URL in the
;;     batch (per-URL overrides are out of scope for this phase).
;;   * Under the hood uses `url-queue-retrieve'; concurrency is
;;     bounded to `anvil-http-batch-concurrency' (default 4); URL
;;     count is capped by `anvil-http-batch-max' (default 16).
;;   * `anvil-http--request-async' is the replaceable primitive —
;;     tests stub it to invoke the callback synchronously with
;;     fixture responses.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-parse)
(require 'url-queue)
(require 'url-util)
(require 'dom)
(require 'anvil-server)
(require 'anvil-state)
;; Architecture (2026-04-25 user signoff): anvil-XXX delegates pure
;; helpers / heavy logic to nelisp-XXX low-level libraries so the
;; substrate investment (Doc 24 Phase 6.2 全 9 sub-phase) is reused.
;; nelisp src/ must be on `load-path' (anvil dev daemon adds it via
;; Phase 5-E setup; Stage D launcher bundles nelisp src/ alongside
;; anvil.el).  Soft load — defer to require so byte-compile works in
;; both anvil-only and anvil+nelisp environments.
(require 'nelisp-http nil 'noerror)

;; `anvil-version' is defined in anvil.el; we only read it in the
;; User-Agent default so soft-require it to avoid a load-order dep.
(defvar anvil-version)

;; `anvil-offload' is loaded lazily — only when `:offload t' is used.
;; Declare its surface to keep the byte-compiler happy without a hard
;; dependency at load time.
(declare-function anvil-offload "anvil-offload" (form &rest keys))
(declare-function anvil-future-await "anvil-offload" (future &optional timeout))
(declare-function anvil-future-value "anvil-offload" (future))
(declare-function anvil-future-error "anvil-offload" (future))
(declare-function anvil-future-done-p "anvil-offload" (future))

;;;; --- configuration ------------------------------------------------------

(defgroup anvil-http nil
  "HTTP client MCP tools for anvil."
  :group 'anvil
  :prefix "anvil-http-")

(defconst anvil-http--server-id "emacs-eval"
  "MCP server id that anvil-http tools register under.
Shared with anvil-file / anvil-org / anvil-browser / anvil-sqlite so
the client sees one unified tool list.")

(defconst anvil-http--state-ns "http"
  "Namespace used when storing cached responses in `anvil-state'.")

(defconst anvil-http--robots-state-ns "http-robots"
  "Namespace used to cache parsed robots.txt responses.
Separate from the main response cache so `http-cache-clear' on the
primary namespace does not wipe robots data and vice versa.")

(defcustom anvil-http-user-agent
  (format "anvil.el/%s (+https://github.com/zawatton/anvil.el)"
          (if (boundp 'anvil-version) anvil-version "dev"))
  "Default value of the User-Agent request header."
  :type 'string
  :group 'anvil-http)

(defcustom anvil-http-cache-ttl-sec 300
  "Default freshness TTL (seconds) for `anvil-http-get' responses.
Within this window the cached entry is served without any network
round-trip.  0 disables the fresh-serve path but the cache still
stores validators for conditional revalidation."
  :type 'integer
  :group 'anvil-http)

(defcustom anvil-http-timeout-sec 30
  "Per-call transport timeout in seconds."
  :type 'integer
  :group 'anvil-http)

(defcustom anvil-http-retry-max 3
  "Maximum retry attempts for 5xx / 408 / 429 responses."
  :type 'integer
  :group 'anvil-http)

(defcustom anvil-http-retry-base-ms 200
  "Base backoff milliseconds for retry attempts.
Actual wait is `base * 2^attempt' unless the server sent a
`Retry-After' header (429 only)."
  :type 'integer
  :group 'anvil-http)

(defcustom anvil-http-retry-jitter-ratio 0.25
  "Random jitter applied to retry backoff, expressed as a fraction.
0 disables jitter (deterministic backoff).  0.25 means the actual
wait is uniformly random in `base * 2^attempt * [1 - 0.25, 1 + 0.25]'.
Cuts thunder-herd risk when many clients retry simultaneously."
  :type 'number
  :group 'anvil-http)

(defcustom anvil-http-max-redirections 5
  "Cap on url.el's redirect chain.  Lower than the Emacs default (30)."
  :type 'integer
  :group 'anvil-http)

(defcustom anvil-http-allowed-schemes '("http" "https")
  "Schemes accepted by `anvil-http-get' / `-head'.
Non-listed schemes are refused with a user-error to block SSRF to
file:// / javascript:// etc."
  :type '(repeat string)
  :group 'anvil-http)

(defcustom anvil-http-max-inline-body-bytes 200000
  "Byte threshold above which response bodies are spilled to disk.
Measured via `string-bytes' (UTF-8 encoding).  Larger bodies have
their :body replaced with a head slice and :body-overflow-path
pointing at a temp file the caller can `file-read' when the
remainder is actually needed.  Set to 0 to disable (always
inline, regardless of size)."
  :type 'integer
  :group 'anvil-http)

(defcustom anvil-http-overflow-head-bytes 2048
  "Characters retained in :body when a response overflows.
Character count, not byte count — multibyte bodies will re-encode
slightly smaller than this value suggests.  A small-but-non-trivial
default (2048) lets Claude see a preview before deciding whether to
pull the rest via `file-read'."
  :type 'integer
  :group 'anvil-http)

(defcustom anvil-http-overflow-dir nil
  "Directory where overflow bodies are spilled.
nil (default) uses `temporary-file-directory'.  Files are named
`anvil-http-<sha-prefix>.bin' so identical bodies dedupe
automatically."
  :type '(choice (const :tag "temporary-file-directory" nil) directory)
  :group 'anvil-http)

(defcustom anvil-http-minimal-header-keys
  '(:content-type :content-length :etag :last-modified
    :content-encoding :location :retry-after)
  "Response headers kept when the active header-filter is `minimal'.
The defaults cover the 7 keys Claude actually needs for
content/length decisions, cache validators, redirect debug, and
backoff hints.  Everything else costs tokens for no gain."
  :type '(repeat keyword)
  :group 'anvil-http)

(defcustom anvil-http-header-filter-default 'minimal
  "Default header-filter mode for anvil-http responses.
`minimal' keeps only `anvil-http-minimal-header-keys'.  `all'
returns every response header verbatim.  A per-call
`:header-filter' argument always wins over this default."
  :type '(choice (const minimal) (const all))
  :group 'anvil-http)

(defcustom anvil-http-respect-robots-txt t
  "Whether `anvil-http-get' checks robots.txt before fetching.
When non-nil, the origin's /robots.txt is fetched (24h TTL cache
in `anvil-http--robots-state-ns') and parsed; a Disallow rule
matching the requested URL raises `user-error'.  Fetch failures
(404, network error) fail open — permissive behaviour matches
RFC 9309 guidance when robots.txt is absent or unreadable.

Set to nil for test fixtures or internal networks where robots.txt
is not meaningful."
  :type 'boolean
  :group 'anvil-http)

(defcustom anvil-http-robots-ttl-sec 86400
  "Seconds to cache robots.txt responses before refetching.
RFC 9309 recommends caching for no longer than 24h (86400 seconds)
unless a Cache-Control header says otherwise.  anvil-http uses the
plain TTL and ignores Cache-Control for simplicity."
  :type 'integer
  :group 'anvil-http)

(defcustom anvil-http-batch-concurrency 4
  "Maximum number of simultaneous url-queue requests per batch.
Binds `url-queue-parallel-processes' for the scope of one
`anvil-http-get-batch' call.  4 is a polite default — one handful
of parallel connections per host is roughly what a browser uses
for HTTP/1.1."
  :type 'integer
  :group 'anvil-http)

(defcustom anvil-http-batch-max 16
  "Maximum number of URLs accepted by one `anvil-http-get-batch' call.
Guard against pathological LLM output that would otherwise fan out
hundreds of requests.  Larger batches should be split by the
caller."
  :type 'integer
  :group 'anvil-http)

(defcustom anvil-http-offload-timeout-sec 120
  "Default timeout (seconds) when waiting for an offloaded fetch.
Long-running downloads or slow APIs should pass a per-call
`:offload-timeout' override; this defcustom is the fallback."
  :type 'integer
  :group 'anvil-http)

(defcustom anvil-http-cache-size-cap-bytes 52428800
  "Soft byte cap on the http response cache.
When a fresh response would push the cache over this number of
bytes, the oldest entries (by `:fetched-at') are evicted until the
total falls back under the cap.  0 disables size-cap eviction (TTL
expiry still happens lazily).  The cap is approximate — only the
body size is counted; headers and overhead are ignored."
  :type 'integer
  :group 'anvil-http)

(defvar anvil-http--cache-bytes-counter 0
  "Running approximation of total body bytes in the http cache.
Maintained incrementally by `--cache-put' / `--cache-delete' /
`--cache-clear-all'; recomputed on `anvil-http-enable' via
`--cache-recompute-bytes'.  Soft — external mutations to
`anvil-state' (e.g. `anvil-state-vacuum') may make this drift.")

(defvar anvil-http--metrics
  (list :requests 0 :cache-fresh 0 :cache-revalidated 0
        :network-200 0 :errors 0 :evictions 0 :log nil)
  "Rolling counters and recent-request log.
:log entries are (:url URL :elapsed-ms N :status N :cache SYM :at T).")

(defcustom anvil-http-metrics-log-size 20
  "Number of recent request entries kept in `anvil-http--metrics'."
  :type 'integer
  :group 'anvil-http)

;;;; --- URL validation / normalization -------------------------------------

(defun anvil-http--check-url (url)
  "Validate URL and its scheme.  Signal `user-error' on failure."
  (unless (and (stringp url) (not (string-empty-p url)))
    (user-error "anvil-http: URL must be a non-empty string, got %S" url))
  (let* ((parsed (url-generic-parse-url url))
         (scheme (and (url-type parsed) (downcase (url-type parsed)))))
    (unless (member scheme anvil-http-allowed-schemes)
      (user-error "anvil-http: scheme %S not in `anvil-http-allowed-schemes'" scheme))))

(defun anvil-http--normalize-url (url)
  "Return a canonical form of URL suitable as a cache key.
Lowercases scheme and host, strips fragment, keeps path+query."
  (let ((parsed (url-generic-parse-url url)))
    (when (url-type parsed)
      (setf (url-type parsed) (downcase (url-type parsed))))
    (when (url-host parsed)
      (setf (url-host parsed) (downcase (url-host parsed))))
    (setf (url-target parsed) nil)
    (url-recreate-url parsed)))

;;;; --- cache I/O (anvil-state ns="http") ----------------------------------

(defun anvil-http--cache-get (norm-url)
  "Return cached response plist for NORM-URL or nil."
  (condition-case _err
      (anvil-state-get norm-url :ns anvil-http--state-ns)
    (error nil)))

(defun anvil-http--cache-entry-size (entry)
  "Return the body byte size of cache ENTRY, or 0 when unknown.
Approximate — only the body is counted, headers and SQLite
overhead are ignored.  Used by the running byte counter and by
LRU eviction."
  (let ((body (and entry (plist-get entry :body))))
    (if (stringp body) (string-bytes body) 0)))

(defun anvil-http--cache-recompute-bytes ()
  "Walk the cache and reset the running byte counter to truth.
O(n) over cache entries; called on enable so a fresh daemon
starts from a correct counter even if SQLite has rows from a
previous session."
  (condition-case _err
      (let ((total 0))
        (dolist (key (anvil-state-list-keys :ns anvil-http--state-ns))
          (let ((entry (anvil-state-get key :ns anvil-http--state-ns)))
            (setq total (+ total (anvil-http--cache-entry-size entry)))))
        (setq anvil-http--cache-bytes-counter total))
    (error (setq anvil-http--cache-bytes-counter 0))))

(defun anvil-http--cache-evict-to-cap ()
  "Drop oldest entries (by `:fetched-at') until size is at or below cap.
Returns the number of evictions; bumps the `:evictions' metric."
  (let ((cap anvil-http-cache-size-cap-bytes)
        (evicted 0))
    (when (and (numberp cap) (> cap 0)
               (> anvil-http--cache-bytes-counter cap))
      (let* ((rows
              (condition-case _err
                  (mapcar
                   (lambda (key)
                     (cons key (anvil-state-get
                                key :ns anvil-http--state-ns)))
                   (anvil-state-list-keys :ns anvil-http--state-ns))
                (error nil)))
             (sorted (sort rows
                           (lambda (a b)
                             (< (or (plist-get (cdr a) :fetched-at) 0)
                                (or (plist-get (cdr b) :fetched-at) 0))))))
        (while (and sorted
                    (> anvil-http--cache-bytes-counter cap))
          (let* ((kv (car sorted))
                 (key (car kv))
                 (size (anvil-http--cache-entry-size (cdr kv))))
            (anvil-state-delete key :ns anvil-http--state-ns)
            (setq anvil-http--cache-bytes-counter
                  (max 0 (- anvil-http--cache-bytes-counter size)))
            (cl-incf evicted)
            (setq sorted (cdr sorted))))))
    (when (> evicted 0)
      (anvil-http--metrics-bump :evictions evicted))
    evicted))

(defun anvil-http--cache-put (norm-url entry)
  "Store ENTRY under NORM-URL in `anvil-state' and maintain the
running byte counter.  Triggers `--cache-evict-to-cap' when the
write pushes the total over `anvil-http-cache-size-cap-bytes'."
  (let* ((old (condition-case _err
                  (anvil-state-get norm-url :ns anvil-http--state-ns)
                (error nil)))
         (old-size (anvil-http--cache-entry-size old))
         (new-size (anvil-http--cache-entry-size entry))
         (delta (- new-size old-size))
         (result (condition-case _err
                     (anvil-state-set norm-url entry
                                      :ns anvil-http--state-ns)
                   (error nil))))
    (setq anvil-http--cache-bytes-counter
          (max 0 (+ anvil-http--cache-bytes-counter delta)))
    (anvil-http--cache-evict-to-cap)
    result))

(defun anvil-http--cache-delete (norm-url)
  "Drop NORM-URL from the cache.  Returns t when a row was removed."
  (let* ((old (condition-case _err
                  (anvil-state-get norm-url :ns anvil-http--state-ns)
                (error nil)))
         (size (anvil-http--cache-entry-size old))
         (result (condition-case _err
                     (anvil-state-delete norm-url
                                         :ns anvil-http--state-ns)
                   (error nil))))
    (when old
      (setq anvil-http--cache-bytes-counter
            (max 0 (- anvil-http--cache-bytes-counter size))))
    result))

(defun anvil-http--cache-clear-all ()
  "Wipe every entry in the http namespace.  Returns deleted row count."
  (let ((result (condition-case _err
                    (anvil-state-delete-ns anvil-http--state-ns)
                  (error 0))))
    (setq anvil-http--cache-bytes-counter 0)
    result))

;;;; --- metrics ------------------------------------------------------------

(defun anvil-http--metrics-bump (key &optional delta)
  (let ((n (or (plist-get anvil-http--metrics key) 0)))
    (setq anvil-http--metrics
          (plist-put anvil-http--metrics key (+ n (or delta 1))))))

(defun anvil-http--metrics-log (url elapsed-ms status cache-sym)
  (let* ((log (plist-get anvil-http--metrics :log))
         (entry (list :url url :elapsed-ms elapsed-ms :status status
                      :cache cache-sym :at (float-time)))
         (updated (cons entry log)))
    (when (> (length updated) anvil-http-metrics-log-size)
      (setq updated (cl-subseq updated 0 anvil-http-metrics-log-size)))
    (setq anvil-http--metrics
          (plist-put anvil-http--metrics :log updated))))

;;;; --- request primitive --------------------------------------------------

(defun anvil-http--parse-response (original-url)
  "Parse the current url-http response buffer into a plist.
Expects `url-http-end-of-headers' to be set; returns nil headers and
empty body otherwise.  The buffer is killed by the caller."
  (let* ((end-headers
          (and (boundp 'url-http-end-of-headers)
               (markerp url-http-end-of-headers)
               (marker-position url-http-end-of-headers)))
         (final
          (or (and (boundp 'url-http-target-url)
                   (let ((u (symbol-value 'url-http-target-url)))
                     (cond ((stringp u) u)
                           ((and u (fboundp 'url-recreate-url))
                            (url-recreate-url u))
                           (t nil))))
              original-url))
         status headers)
    (goto-char (point-min))
    (when (re-search-forward "\\`HTTP/[0-9.]+ +\\([0-9]+\\)"
                             (or end-headers (point-max)) t)
      (setq status (string-to-number (match-string 1))))
    (forward-line 1)
    (while (and end-headers (< (point) end-headers))
      (when (looking-at "^\\([^:\r\n]+\\):[ \t]*\\(.*?\\)[ \t]*\r?$")
        (let ((key (downcase (string-trim (match-string 1))))
              (val (match-string 2)))
          (setq headers (plist-put headers
                                   (intern (concat ":" key))
                                   val))))
      (forward-line 1))
    (let ((body (if end-headers
                    (buffer-substring-no-properties end-headers (point-max))
                  "")))
      ;; url-http leaves one CRLF after headers — trim a single leading newline.
      (when (and (> (length body) 0)
                 (or (eq (aref body 0) ?\n)
                     (eq (aref body 0) ?\r)))
        (setq body (replace-regexp-in-string "\\`\r?\n" "" body)))
      (list :status status
            :headers headers
            :body body
            :final-url final))))

(defun anvil-http--request (method url extra-headers timeout
                                   &optional body)
  "Issue one METHOD request to URL with EXTRA-HEADERS and TIMEOUT (seconds).
BODY is an optional request body for POST / PUT / PATCH; passed as
`url-request-data' verbatim and the caller is responsible for any
encoding (the higher-level `anvil-http-post' takes care of this).
Returns a response plist (:status :headers :body :final-url) or
signals `anvil-server-tool-error' on transport failure / timeout."
  (let* ((url-request-method method)
         (url-request-data body)
         (url-request-extra-headers
          (append
           (unless (assoc "User-Agent" extra-headers)
             (list (cons "User-Agent" anvil-http-user-agent)))
           (unless (assoc "Accept-Encoding" extra-headers)
             (list (cons "Accept-Encoding" "gzip")))
           extra-headers))
         (url-show-status nil)
         (url-automatic-caching nil)
         (url-max-redirections anvil-http-max-redirections)
         (buf nil))
    (condition-case err
        (setq buf (url-retrieve-synchronously url t t timeout))
      (error
       (signal 'anvil-server-tool-error
               (list (format "anvil-http: network error on %s: %s"
                             url (error-message-string err))))))
    (unless (buffer-live-p buf)
      (signal 'anvil-server-tool-error
              (list (format "anvil-http: no response (timeout %ds?) for %s"
                            timeout url))))
    (unwind-protect
        (with-current-buffer buf
          (let ((resp (anvil-http--parse-response url)))
            (unless (plist-get resp :status)
              (signal 'anvil-server-tool-error
                      (list (format "anvil-http: malformed response from %s"
                                    url))))
            resp))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun anvil-http--retryable-p (status)
  "Return non-nil when an HTTP STATUS code should be retried."
  (and status
       (or (and (>= status 500) (< status 600))
           (= status 408)
           (= status 429))))

(defun anvil-http--apply-jitter (base-ms)
  "Apply +/-`anvil-http-retry-jitter-ratio' random jitter to BASE-MS.
A ratio of 0 returns BASE-MS unchanged; anything else returns a
uniform sample from `BASE-MS * [1-ratio, 1+ratio]'.  Jitter cuts
thunder-herd risk when many clients retry the same upstream after
a 5xx burst."
  (let ((j (max 0.0 (min 1.0 (or anvil-http-retry-jitter-ratio 0)))))
    (if (= j 0) base-ms
      (let ((delta (- (* 2.0 j (/ (random 10000) 10000.0)) j)))
        (max 0 (round (* base-ms (+ 1.0 delta))))))))

(defun anvil-http--backoff-ms (attempt response)
  "Return the wait time (ms) before retry ATTEMPT.
RESPONSE is the last response plist; honours `Retry-After' for 429,
otherwise applies exponential backoff with random jitter."
  (let* ((status (plist-get response :status))
         (headers (plist-get response :headers))
         (retry-after (and (= (or status 0) 429)
                           (plist-get headers :retry-after)))
         (base (* anvil-http-retry-base-ms (expt 2 attempt))))
    (cond
     ((and retry-after (string-match "\\`[0-9]+\\'" retry-after))
      (* 1000 (string-to-number retry-after)))
     (t (anvil-http--apply-jitter base)))))

(defun anvil-http--request-with-retry (method url extra-headers timeout
                                              &optional body)
  "Run `anvil-http--request' with retry on 5xx / 408 / 429.
BODY is an optional request body for non-GET methods; passed
through verbatim to the inner request."
  (let ((attempt 0)
        (max-total (1+ (max 0 anvil-http-retry-max)))
        (result nil))
    (catch 'done
      (while (< attempt max-total)
        (setq result (anvil-http--request method url extra-headers
                                          timeout body))
        (if (and (anvil-http--retryable-p (plist-get result :status))
                 (< attempt (1- max-total)))
            (let ((ms (anvil-http--backoff-ms attempt result)))
              (sleep-for (/ ms 1000.0))
              (setq attempt (1+ attempt)))
          (throw 'done result))))
    result))

(defun anvil-http--request-async (url extra-headers timeout callback)
  "Issue an async GET for URL and invoke CALLBACK with the response plist.
CALLBACK receives either (:status :headers :body :final-url) on
success or (:error STRING) on transport / parse failure.  Uses
`url-queue-retrieve' so concurrency is bounded by
`url-queue-parallel-processes' (batch callers let-bind this
from `anvil-http-batch-concurrency').

Unlike `anvil-http--request' this primitive does not retry on 5xx
— batch callers treat a per-URL failure as partial (one bad URL
does not sink the whole batch).  Tests stub this function to
invoke CALLBACK synchronously with fixture responses."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         (append
          (unless (assoc "User-Agent" extra-headers)
            `(("User-Agent" . ,anvil-http-user-agent)))
          (unless (assoc "Accept-Encoding" extra-headers)
            '(("Accept-Encoding" . "gzip")))
          extra-headers))
        (url-max-redirections anvil-http-max-redirections)
        (url-queue-timeout (or timeout anvil-http-timeout-sec)))
    (url-queue-retrieve
     url
     (lambda (status &rest _cbargs)
       (funcall
        callback
        (cond
         ((plist-get status :error)
          (list :error (format "%S" (plist-get status :error))))
         (t
          (condition-case err
              (anvil-http--parse-response url)
            (error (list :error (error-message-string err))))))))
     nil
     t)))

;;;; --- conditional-request header assembly --------------------------------

(defun anvil-http--format-http-date (epoch)
  "Format EPOCH (unix seconds) as an RFC 7231 IMF-fixdate string."
  (let ((system-time-locale "C"))
    (format-time-string "%a, %d %b %Y %H:%M:%S GMT" epoch t)))

(defun anvil-http--build-request-headers (user-headers accept cached if-newer-than)
  "Assemble the outgoing request header alist.
USER-HEADERS is the caller-supplied alist (string keys).  CACHED is
a cache entry plist (or nil).  ACCEPT is a short-hand MIME string."
  (let ((h (copy-sequence (or user-headers nil))))
    (when (and accept (not (assoc "Accept" h)))
      (push (cons "Accept" accept) h))
    (when-let* ((etag (and cached (plist-get cached :etag))))
      (unless (assoc "If-None-Match" h)
        (push (cons "If-None-Match" etag) h)))
    (when-let* ((lm (and cached (plist-get cached :last-modified))))
      (unless (assoc "If-Modified-Since" h)
        (push (cons "If-Modified-Since" lm) h)))
    (when (and (integerp if-newer-than)
               (not (assoc "If-Modified-Since" h)))
      (push (cons "If-Modified-Since"
                  (anvil-http--format-http-date if-newer-than))
            h))
    h))

;;;; --- response assembly --------------------------------------------------

(defun anvil-http--cache-entry (status headers body fetched-at final-url)
  (list :status status
        :headers headers
        :body body
        :fetched-at fetched-at
        :final-url final-url
        :etag (plist-get headers :etag)
        :last-modified (plist-get headers :last-modified)))

(defun anvil-http--response-plist (entry from-cache elapsed-ms)
  "Turn a cache-entry plist into the public response plist."
  (list :status (plist-get entry :status)
        :headers (plist-get entry :headers)
        :body (plist-get entry :body)
        :from-cache from-cache
        :cached-at (plist-get entry :fetched-at)
        :final-url (plist-get entry :final-url)
        :elapsed-ms elapsed-ms))

;;;; --- content-extract helpers (Phase 1b) --------------------------------

(defun anvil-http--libxml-p ()
  "Return non-nil when this Emacs build bundles libxml2.
libxml is bundled with most Linux / macOS builds; a handful of
Windows builds ship without it.  The selector pipeline falls back
to the regex subset when this returns nil.

Delegates to `nelisp-http--libxml-p' when available."
  (if (fboundp 'nelisp-http--libxml-p)
      (nelisp-http--libxml-p)
    (fboundp 'libxml-parse-html-region)))

(defun anvil-http--extract-target-from-content-type (ct)
  "Classify Content-Type string CT as `html', `json', `xml', or nil.
Matches the MIME type case-insensitively and ignores parameters
after a semicolon (e.g. charset=utf-8).  XHTML is treated as HTML
so libxml's HTML parser handles it; pure text/xml and
application/xml use the XML parser when libxml is available.

Delegates to `nelisp-http--extract-target-from-content-type' when available."
  (if (fboundp 'nelisp-http--extract-target-from-content-type)
      (nelisp-http--extract-target-from-content-type ct)
    (cond
     ((not (stringp ct)) nil)
     ((string-match-p "\\`\\(?:[[:space:]]*\\)\\(?:text/html\\|application/xhtml\\+xml\\)\\b"
                      (downcase ct))
      'html)
     ((string-match-p "\\`\\(?:[[:space:]]*\\)application/json\\b" (downcase ct))
      'json)
     ((string-match-p "\\`\\(?:[[:space:]]*\\)\\(?:application/xml\\|text/xml\\)\\b"
                      (downcase ct))
      'xml)
     (t nil))))

(defun anvil-http--parse-selector (s)
  "Parse CSS-subset selector S into an internal form.
Returns one of:

  (:tag SYMBOL)
  (:class STRING)
  (:id STRING)
  (:tag-class SYMBOL STRING)
  (:tag-id SYMBOL STRING)

or nil for anything outside the subset (combinators, pseudo
classes, attribute selectors).  Both libxml and fallback engines
share the same subset so test expectations stay backend-agnostic.

Delegates to `nelisp-http--parse-selector' when available."
  (if (fboundp 'nelisp-http--parse-selector)
      (nelisp-http--parse-selector s)
    (let ((s (and (stringp s) (string-trim s))))
      (cond
       ((or (null s) (string-empty-p s)) nil)
       ((string-match "\\`#\\([A-Za-z][A-Za-z0-9_-]*\\)\\'" s)
        (list :id (match-string 1 s)))
       ((string-match "\\`\\.\\([A-Za-z][A-Za-z0-9_-]*\\)\\'" s)
        (list :class (match-string 1 s)))
       ((string-match "\\`\\([A-Za-z][A-Za-z0-9]*\\)#\\([A-Za-z][A-Za-z0-9_-]*\\)\\'" s)
        (list :tag-id (intern (downcase (match-string 1 s))) (match-string 2 s)))
       ((string-match "\\`\\([A-Za-z][A-Za-z0-9]*\\)\\.\\([A-Za-z][A-Za-z0-9_-]*\\)\\'" s)
        (list :tag-class (intern (downcase (match-string 1 s))) (match-string 2 s)))
       ((string-match "\\`[A-Za-z][A-Za-z0-9]*\\'" s)
        (list :tag (intern (downcase s))))
       (t nil)))))

(defun anvil-http--dom-select (dom parts)
  "Return DOM nodes matching selector PARTS (from `--parse-selector').
Uses dom.el primitives; `dom-by-class' / `dom-by-id' take regex
strings, so `\\\\b' word-boundary anchors match the class/id exactly.

Delegates to `nelisp-http--dom-select' when available."
  (if (fboundp 'nelisp-http--dom-select)
      (nelisp-http--dom-select dom parts)
    (pcase parts
      (`(:tag ,tag) (dom-by-tag dom tag))
      (`(:class ,cls) (dom-by-class dom (format "\\b%s\\b" (regexp-quote cls))))
      (`(:id ,id) (dom-by-id dom (format "\\`%s\\'" (regexp-quote id))))
      (`(:tag-class ,tag ,cls)
       (seq-filter (lambda (el) (eq (dom-tag el) tag))
                   (dom-by-class dom (format "\\b%s\\b" (regexp-quote cls)))))
      (`(:tag-id ,tag ,id)
       (seq-filter (lambda (el) (eq (dom-tag el) tag))
                   (dom-by-id dom (format "\\`%s\\'" (regexp-quote id)))))
      (_ nil))))

(defun anvil-http--select-html-libxml (html selector)
  "Return text from HTML matching SELECTOR via libxml + dom.el.
Matches are joined with a blank-line separator.  Returns nil if the
selector is outside the supported subset or nothing matched."
  (let ((parts (anvil-http--parse-selector selector)))
    (when (and parts (fboundp 'libxml-parse-html-region))
      (let* ((dom (with-temp-buffer
                    (insert html)
                    (libxml-parse-html-region (point-min) (point-max))))
             (nodes (and dom (anvil-http--dom-select dom parts))))
        (when nodes
          (let ((texts (delq nil
                             (mapcar (lambda (n)
                                       (let ((s (string-trim (or (dom-text n) ""))))
                                         (and (not (string-empty-p s)) s)))
                                     nodes))))
            (when texts (mapconcat #'identity texts "\n\n"))))))))

(defun anvil-http--strip-tags (html)
  "Return HTML with tags removed and whitespace collapsed.
Enough for the regex-subset fallback; not a substitute for a real
HTML-to-text renderer (no entity decoding beyond amp/lt/gt/quot)."
  (let* ((s (replace-regexp-in-string "<[^>]+>" "" html))
         (s (replace-regexp-in-string "&amp;" "&" s))
         (s (replace-regexp-in-string "&lt;" "<" s))
         (s (replace-regexp-in-string "&gt;" ">" s))
         (s (replace-regexp-in-string "&quot;" "\"" s))
         (s (replace-regexp-in-string "[ \t]+" " " s))
         (s (replace-regexp-in-string "\n[ \t]*\n[ \t\n]*" "\n\n" s)))
    (string-trim s)))

(defun anvil-http--select-html-fallback (html selector)
  "Regex-subset selector for builds without libxml.
Supports the same subset as `--parse-selector' but without
combinators.  Nested tags of the same name can trip the lazy
match; libxml is the recommended path when available."
  (let* ((parts (anvil-http--parse-selector selector))
         (case-fold-search t)
         (rx nil)
         (group 1))
    (pcase parts
      (`(:tag ,tag)
       (setq rx (format "<%s\\b[^>]*>\\(\\(?:.\\|\n\\)*?\\)</%s>"
                        (symbol-name tag) (symbol-name tag))))
      (`(:id ,id)
       (setq rx (format "<\\([A-Za-z][A-Za-z0-9]*\\)\\b[^>]*\\bid=[\"']%s[\"'][^>]*>\\(\\(?:.\\|\n\\)*?\\)</\\1>"
                        (regexp-quote id)))
       (setq group 2))
      (`(:class ,cls)
       (setq rx (format "<\\([A-Za-z][A-Za-z0-9]*\\)\\b[^>]*\\bclass=[\"'][^\"']*\\b%s\\b[^\"']*[\"'][^>]*>\\(\\(?:.\\|\n\\)*?\\)</\\1>"
                        (regexp-quote cls)))
       (setq group 2))
      (`(:tag-class ,tag ,cls)
       (setq rx (format "<%s\\b[^>]*\\bclass=[\"'][^\"']*\\b%s\\b[^\"']*[\"'][^>]*>\\(\\(?:.\\|\n\\)*?\\)</%s>"
                        (symbol-name tag) (regexp-quote cls) (symbol-name tag))))
      (`(:tag-id ,tag ,id)
       (setq rx (format "<%s\\b[^>]*\\bid=[\"']%s[\"'][^>]*>\\(\\(?:.\\|\n\\)*?\\)</%s>"
                        (symbol-name tag) (regexp-quote id) (symbol-name tag)))))
    (when rx
      (let ((pos 0) (acc nil))
        (while (string-match rx html pos)
          (let* ((content (match-string group html))
                 (text (and content (anvil-http--strip-tags content))))
            (when (and text (not (string-empty-p text)))
              (push text acc)))
          (setq pos (match-end 0)))
        (when acc (mapconcat #'identity (nreverse acc) "\n\n"))))))

(defun anvil-http--split-json-path (path)
  "Tokenize dotted-path PATH.
Segments are (:key STRING), (:index INT), or (:wildcard).  Supports
forms like `items[0].title', `data.results[*].id', and plain
`users.0.name' where digits following a dot become an index."
  (let ((i 0) (n (length path)) (tokens nil))
    (while (< i n)
      (let ((ch (aref path i)))
        (cond
         ((eq ch ?.) (cl-incf i))
         ((eq ch ?\[)
          (let* ((end (string-match "\\]" path i))
                 (inner (and end (substring path (1+ i) end))))
            (unless end
              (error "anvil-http: unterminated [] in json-path %S" path))
            (push (cond
                   ((equal inner "*") (list :wildcard))
                   ((string-match-p "\\`-?[0-9]+\\'" inner)
                    (list :index (string-to-number inner)))
                   (t (list :key inner)))
                  tokens)
            (setq i (1+ end))))
         (t
          (let* ((end (or (string-match "[.[]" path i) n))
                 (seg (substring path i end)))
            (push (if (string-match-p "\\`-?[0-9]+\\'" seg)
                      (list :index (string-to-number seg))
                    (list :key seg))
                  tokens)
            (setq i end))))))
    (nreverse tokens)))

(defun anvil-http--json-walk (node segments)
  "Walk NODE by SEGMENTS produced by `--split-json-path'.
NODE uses hash-tables for objects and vectors for arrays.  Returns
the located sub-tree, or nil on a miss.  `(:wildcard)' flattens an
array; following segments are mapped over its elements."
  (cond
   ((null segments) node)
   ((null node) nil)
   (t
    (pcase (car segments)
      (`(:key ,k)
       (when (hash-table-p node)
         (let ((val (gethash k node)))
           (anvil-http--json-walk val (cdr segments)))))
      (`(:index ,idx)
       (when (and (vectorp node)
                  (>= idx 0) (< idx (length node)))
         (anvil-http--json-walk (aref node idx) (cdr segments))))
      (`(:wildcard)
       (when (vectorp node)
         (let ((results
                (delq nil
                      (mapcar (lambda (el)
                                (anvil-http--json-walk el (cdr segments)))
                              (append node nil)))))
           (vconcat results))))
      (_ nil)))))

(defun anvil-http--select-json-dotted (json-string path)
  "Parse JSON-STRING and walk dotted PATH.
Returns the located sub-tree serialized back to JSON (so Claude
sees the same format it sent), or nil on parse error / miss."
  (condition-case _
      (let* ((node (json-parse-string
                    json-string
                    :object-type 'hash-table
                    :array-type 'array
                    :null-object :null
                    :false-object :false))
             (segments (anvil-http--split-json-path path))
             (result (and segments (anvil-http--json-walk node segments))))
        (when result
          (json-serialize result
                          :null-object :null
                          :false-object :false)))
    (error nil)))

(defun anvil-http--plist-put! (plist &rest kvs)
  "Return PLIST with KVS (flat key/value list) applied via `plist-put'."
  (let ((p (copy-sequence plist)))
    (while kvs
      (setq p (plist-put p (car kvs) (cadr kvs)))
      (setq kvs (cddr kvs)))
    p))

(defun anvil-http--apply-extract (response selector json-path)
  "Post-process RESPONSE with SELECTOR / JSON-PATH when supplied.
Both nil → RESPONSE is returned unchanged.  When extraction runs,
the returned plist always carries `:extract-mode' (symbol) and
`:extract-engine' (symbol) for observability; `:body' is replaced
only on a successful match and `:extract-miss t' is set on a
no-match or content-type mismatch so callers never see an empty
string they can't distinguish from \"server returned empty body\"."
  (if (and (null selector) (null json-path))
      response
    (let* ((headers (plist-get response :headers))
           (ct (plist-get headers :content-type))
           (target (anvil-http--extract-target-from-content-type ct))
           (body (plist-get response :body)))
      (cond
       ((and selector (memq target '(html xml)) (stringp body))
        (let* ((engine (if (anvil-http--libxml-p) 'libxml 'regex-subset))
               (extracted
                (if (eq engine 'libxml)
                    (anvil-http--select-html-libxml body selector)
                  (anvil-http--select-html-fallback body selector))))
          (if (and extracted (not (string-empty-p extracted)))
              (anvil-http--plist-put! response
                                      :body extracted
                                      :extract-mode 'selector
                                      :extract-engine engine)
            (anvil-http--plist-put! response
                                    :extract-miss t
                                    :extract-mode 'selector
                                    :extract-engine engine))))
       ((and json-path (eq target 'json) (stringp body))
        (let ((extracted (anvil-http--select-json-dotted body json-path)))
          (if extracted
              (anvil-http--plist-put! response
                                      :body extracted
                                      :extract-mode 'json-path
                                      :extract-engine 'json)
            (anvil-http--plist-put! response
                                    :extract-miss t
                                    :extract-mode 'json-path
                                    :extract-engine 'json))))
       (t
        (anvil-http--plist-put! response
                                :extract-miss t
                                :extract-mode (cond (selector 'selector)
                                                    (json-path 'json-path))
                                :extract-engine 'content-type-mismatch))))))

;;;; --- body-mode / header-filter helpers (Phase 1c) -----------------------

(defun anvil-http--overflow-dir ()
  "Return the effective overflow spool directory.
Falls back to `temporary-file-directory' when the defcustom is
unset, so callers never need to `or'-chain."
  (or anvil-http-overflow-dir temporary-file-directory))

(defun anvil-http--spill-to-disk (body)
  "Write BODY to an overflow file and return (:path :sha256 :total-bytes).
Filename includes a 16-char prefix of the sha256 so identical
payloads dedupe automatically — a second caller with the same body
observes the file already present and skips the write.  The file
is written in binary (`no-conversion') so a round-trip via
`file-read' returns exactly what came off the wire."
  (let* ((sha (secure-hash 'sha256 body))
         (short (substring sha 0 16))
         (dir (anvil-http--overflow-dir))
         (path (expand-file-name
                (format "anvil-http-%s.bin" short) dir)))
    (unless (file-exists-p path)
      (let ((coding-system-for-write 'no-conversion))
        (with-temp-file path
          (set-buffer-multibyte nil)
          (insert body))))
    (list :path path
          :sha256 sha
          :total-bytes (string-bytes body))))

(defun anvil-http--head-slice (body n)
  "Return the first N characters of BODY (or all of BODY if shorter)."
  (if (and (stringp body) (> (length body) n))
      (substring body 0 n)
    body))

(defun anvil-http--apply-body-mode (response mode)
  "Apply body-mode MODE to RESPONSE and return the transformed plist.

MODE values:
  nil / `auto'  Default.  Spill to disk and return a head slice when
                `:body' exceeds `anvil-http-max-inline-body-bytes';
                shorter bodies pass through.
  `full'        Return `:body' unchanged regardless of size.
  `head-only'   Keep the head slice and drop the tail; no spill file
                is created (use when the caller explicitly does not
                want the remainder).
  `meta-only'   Drop `:body' entirely; useful for cheap probes.

When spilling, the response gains :body-truncated t,
:total-bytes INT, :body-overflow-path STR, :body-sha256 HEX, and
:body-mode `overflow' so the caller can tell the response was
overflowed (vs merely short)."
  (let* ((body (plist-get response :body))
         (size (and (stringp body) (string-bytes body))))
    (pcase (or mode 'auto)
      ('full response)
      ('meta-only
       (let ((r (copy-sequence response)))
         (setq r (plist-put r :body nil))
         (plist-put r :body-mode 'meta-only)))
      ('head-only
       (let* ((head (anvil-http--head-slice
                     body anvil-http-overflow-head-bytes))
              (r (copy-sequence response)))
         (setq r (plist-put r :body head))
         (setq r (plist-put r :body-truncated t))
         (setq r (plist-put r :total-bytes size))
         (plist-put r :body-mode 'head-only)))
      (_
       (cond
        ((null body) response)
        ((or (null size)
             (<= anvil-http-max-inline-body-bytes 0)
             (<= size anvil-http-max-inline-body-bytes))
         response)
        (t
         (let* ((spill (anvil-http--spill-to-disk body))
                (head (anvil-http--head-slice
                       body anvil-http-overflow-head-bytes))
                (r (copy-sequence response)))
           (setq r (plist-put r :body head))
           (setq r (plist-put r :body-truncated t))
           (setq r (plist-put r :total-bytes (plist-get spill :total-bytes)))
           (setq r (plist-put r :body-overflow-path (plist-get spill :path)))
           (setq r (plist-put r :body-sha256 (plist-get spill :sha256)))
           (plist-put r :body-mode 'overflow))))))))

(defun anvil-http--apply-header-filter (response filter)
  "Restrict RESPONSE :headers to the minimal set when FILTER is `minimal'.
FILTER nil falls back to `anvil-http-header-filter-default'.  `all'
is a no-op.  When `minimal', only keys in
`anvil-http-minimal-header-keys' that actually have a value are
returned; missing keys are omitted (not nil-valued)."
  (let* ((f (or filter anvil-http-header-filter-default))
         (headers (plist-get response :headers)))
    (pcase f
      ('all response)
      ('minimal
       (let (filtered)
         (dolist (key anvil-http-minimal-header-keys)
           (let ((val (plist-get headers key)))
             (when val
               (setq filtered (plist-put filtered key val)))))
         (let ((r (copy-sequence response)))
           (plist-put r :headers filtered))))
      (_ response))))

(defun anvil-http--coerce-symbol-arg (arg)
  "Accept a symbol or string ARG and return the canonical symbol or nil.
MCP parameters arrive as strings; Elisp callers pass symbols.  An
empty string or nil collapses to nil so callers can just forward
user input without a pre-check."
  (cond
   ((null arg) nil)
   ((symbolp arg) arg)
   ((and (stringp arg) (string-empty-p arg)) nil)
   ((stringp arg) (intern arg))
   (t nil)))

;;;; --- robots.txt (Phase 1e) ----------------------------------------------

(defun anvil-http--url-origin (url)
  "Return scheme://host[:port] for URL, omitting the default port."
  (let* ((u (url-generic-parse-url url))
         (scheme (url-type u))
         (host (url-host u))
         (port (url-port u))
         (default (pcase scheme ("http" 80) ("https" 443) (_ nil))))
    (if (and (numberp port) default (= port default))
        (format "%s://%s" scheme host)
      (format "%s://%s:%s" scheme host port))))

(defun anvil-http--url-path (url)
  "Return path+query of URL, defaulting to `/' when absent."
  (let* ((u (url-generic-parse-url url))
         (filename (url-filename u)))
    (if (or (null filename) (string-empty-p filename))
        "/"
      filename)))

(defun anvil-http--is-robots-url-p (url)
  "Return non-nil when URL itself points at /robots.txt."
  (string-equal "/robots.txt" (anvil-http--url-path url)))

(defun anvil-http--robots-parse (text)
  "Parse robots.txt TEXT into a list of (UA-LIST . RULES).
Each RULES element is `(DIRECTIVE . PATTERN)' where DIRECTIVE is
`allow' or `disallow'.  Comments (`#' through end-of-line),
whitespace-only lines, and unknown directives (Sitemap, Host, etc.)
are ignored.  Consecutive User-agent lines accumulate into one
group until a rule follows; the next User-agent after a rule
starts a new group — matching RFC 9309 group definition.

Delegates to `nelisp-http--robots-parse' when available."
  (if (fboundp 'nelisp-http--robots-parse)
      (nelisp-http--robots-parse text)
    (let ((groups nil)
          (current-uas nil)
          (current-rules nil)
          (in-rules nil))
      (dolist (raw (split-string (or text "") "\n"))
        (let* ((stripped (replace-regexp-in-string "#.*\\'" "" raw))
               (line (string-trim stripped)))
          (unless (string-empty-p line)
            (when (string-match "\\`\\([A-Za-z-]+\\)[ \t]*:[ \t]*\\(.*\\)\\'"
                                line)
              (let ((key (downcase (match-string 1 line)))
                    (val (string-trim (match-string 2 line))))
                (pcase key
                  ("user-agent"
                   (when in-rules
                     (push (cons (nreverse current-uas)
                                 (nreverse current-rules))
                           groups)
                     (setq current-uas nil
                           current-rules nil
                           in-rules nil))
                   (push val current-uas))
                  ("allow"
                   (when current-uas
                     (push (cons 'allow val) current-rules)
                     (setq in-rules t)))
                  ("disallow"
                   (when current-uas
                     (push (cons 'disallow val) current-rules)
                     (setq in-rules t)))
                  (_ nil)))))))
      (when current-uas
        (push (cons (nreverse current-uas)
                    (nreverse current-rules))
              groups))
      (nreverse groups))))

(defun anvil-http--robots-pick-group (groups ua)
  "Return rules for the group best matching UA string.
UA is compared case-insensitively against each group's user-agent
tokens (substring match); the longest matching token wins.  Falls
back to the `*' group when no specific token matches.  Returns nil
when neither a matching nor `*' group exists.

Delegates to `nelisp-http--robots-pick-group' when available."
  (if (fboundp 'nelisp-http--robots-pick-group)
      (nelisp-http--robots-pick-group groups ua)
    (let ((ua-lc (downcase (or ua "")))
          (best-rules nil)
          (best-len -1)
          (star-rules nil))
      (dolist (group groups)
        (let ((uas (car group))
              (rules (cdr group)))
          (dolist (u uas)
            (let ((u-lc (downcase u)))
              (cond
               ((equal u-lc "*")
                (unless star-rules (setq star-rules rules)))
               ((and (not (string-empty-p u-lc))
                     (string-match-p (regexp-quote u-lc) ua-lc)
                     (> (length u-lc) best-len))
                (setq best-rules rules
                      best-len (length u-lc))))))))
      (or best-rules star-rules))))

(defun anvil-http--robots-pattern-to-regex (pattern)
  "Convert a robots.txt PATTERN to an Emacs regex.
Empty or nil patterns return nil (no match, per RFC 9309 §2.2.2).
`*' expands to `.*', trailing `$' anchors to end-of-URL, and every
other character is regex-quoted.  All other special sequences are
literal.

Delegates to `nelisp-http--robots-pattern-to-regex' when available."
  (if (fboundp 'nelisp-http--robots-pattern-to-regex)
      (nelisp-http--robots-pattern-to-regex pattern)
    (when (and pattern (not (string-empty-p pattern)))
      (let ((end-anchor nil)
            (p pattern))
        (when (string-suffix-p "$" p)
          (setq end-anchor t)
          (setq p (substring p 0 (1- (length p)))))
        (let ((chunks (mapcar
                       (lambda (ch)
                         (cond
                          ((eq ch ?*) ".*")
                          (t (regexp-quote (char-to-string ch)))))
                       (string-to-list p))))
          (concat "\\`"
                  (mapconcat #'identity chunks "")
                  (if end-anchor "\\'" "")))))))

(defun anvil-http--robots-match (rules path)
  "Return the winning rule for PATH against RULES, or nil.
Winning rule follows RFC 9309: longest pattern wins; on a tie
Allow beats Disallow.  Return value is (ALLOW-P . PATTERN-LENGTH)
so callers can use the boolean AND surface the specificity for
observability.

Delegates to `nelisp-http--robots-match' when available."
  (if (fboundp 'nelisp-http--robots-match)
      (nelisp-http--robots-match rules path)
    (let ((best nil))
      (dolist (rule rules)
        (let* ((dir (car rule))
               (pat (cdr rule))
               (rx (anvil-http--robots-pattern-to-regex pat)))
          (when (and rx (string-match-p rx path))
            (let* ((len (length pat))
                   (allow-p (eq dir 'allow))
                   (current (cons allow-p len)))
              (cond
               ((null best) (setq best current))
               ((> len (cdr best)) (setq best current))
               ((and (= len (cdr best))
                     allow-p)
                (setq best current)))))))
      best)))

(defun anvil-http--robots-fetch (origin)
  "Fetch ORIGIN/robots.txt and cache it for `anvil-http-robots-ttl-sec'.
Returns `(:body STR-OR-NIL :fetched-at FLOAT)' plist.  A nil body
means the fetch failed (404, network error, non-200) — callers
treat that as fail-open per RFC 9309 guidance.  The robots fetch
itself sets `:skip-robots-check t' to avoid recursion and
`:no-cache t' so it does not pollute the main response cache."
  (let* ((url (concat origin "/robots.txt"))
         (ns anvil-http--robots-state-ns)
         (cached (anvil-state-get url :ns ns))
         (now (float-time))
         (ttl anvil-http-robots-ttl-sec))
    (if (and cached
             (numberp (plist-get cached :fetched-at))
             (< (- now (plist-get cached :fetched-at)) ttl))
        cached
      (let ((entry
             (condition-case _err
                 (let ((resp (anvil-http-get
                              url
                              :skip-robots-check t
                              :no-cache t
                              :header-filter 'all
                              :body-mode 'full)))
                   (if (= 200 (plist-get resp :status))
                       (list :body (plist-get resp :body)
                             :fetched-at now)
                     (list :body nil :fetched-at now)))
               (error (list :body nil :fetched-at now)))))
        (anvil-state-set url entry :ns ns)
        entry))))

(defun anvil-http--robots-evaluate (url ua)
  "Evaluate URL against its origin's robots.txt for UA.
Returns a plist: `(:origin :allowed :rule-length :robots-present)'.
`:allowed' is t when no Disallow matches or robots.txt is absent
(fail open)."
  (let* ((origin (anvil-http--url-origin url))
         (entry (anvil-http--robots-fetch origin))
         (body (plist-get entry :body)))
    (if (not body)
        (list :origin origin
              :allowed t
              :rule-length nil
              :robots-present nil)
      (let* ((groups (anvil-http--robots-parse body))
             (rules (anvil-http--robots-pick-group groups ua))
             (path (anvil-http--url-path url))
             (match (and rules (anvil-http--robots-match rules path))))
        (list :origin origin
              :allowed (if match (car match) t)
              :rule-length (and match (cdr match))
              :robots-present t)))))

(defun anvil-http--robots-check-signal (url)
  "Raise `user-error' if URL is Disallowed by its origin's robots.txt.
Uses `anvil-http-user-agent' as the UA string.  Silent on allow /
fetch-failure."
  (let ((r (anvil-http--robots-evaluate url anvil-http-user-agent)))
    (unless (plist-get r :allowed)
      (user-error
       "anvil-http: %s is Disallowed for %s by robots.txt at %s"
       url anvil-http-user-agent (plist-get r :origin)))))

;;;; --- offload (Phase 2) --------------------------------------------------

(defun anvil-http--offload-form (method url opts)
  "Build the elisp form sent to an offload worker for METHOD URL.
OPTS carries pass-through keywords from the caller; the worker
forces `:no-cache t' (cache concurrency is the main daemon's
responsibility) and `:skip-robots-check t' (robots are evaluated
in the parent before offloading)."
  (cond
   ((string= method "GET")
    `(progn
       (require 'anvil-state)
       (require 'anvil-http)
       (anvil-state-enable)
       (anvil-http-get ,url
                       :headers ',(plist-get opts :headers)
                       :accept ,(plist-get opts :accept)
                       :timeout-sec ,(plist-get opts :timeout-sec)
                       :selector ,(plist-get opts :selector)
                       :json-path ,(plist-get opts :json-path)
                       :body-mode ',(plist-get opts :body-mode)
                       :header-filter ',(plist-get opts :header-filter)
                       :no-cache t
                       :skip-robots-check t)))
   ((string= method "POST")
    `(progn
       (require 'anvil-state)
       (require 'anvil-http)
       (anvil-state-enable)
       (anvil-http-post ,url
                        :body ,(plist-get opts :body)
                        :content-type ,(plist-get opts :content-type)
                        :headers ',(plist-get opts :headers)
                        :accept ,(plist-get opts :accept)
                        :timeout-sec ,(plist-get opts :timeout-sec)
                        :auth ',(plist-get opts :auth)
                        :body-mode ',(plist-get opts :body-mode)
                        :header-filter ',(plist-get opts :header-filter)
                        :skip-robots-check t)))
   (t (error "anvil-http: offload supports GET and POST only, got %S"
             method))))

(defun anvil-http--offload-fetch (method url opts)
  "Run a METHOD request to URL via anvil-offload and block on the result.
The main daemon stays responsive while the actual network round-trip
runs in a worker subprocess; the await timeout defaults to
`anvil-http-offload-timeout-sec' but can be overridden via
`(plist-get OPTS :offload-timeout)'.  Errors on the worker side
surface as `anvil-server-tool-error' so the caller's existing error
handling still works."
  (require 'anvil-offload)
  (let* ((timeout (or (plist-get opts :offload-timeout)
                      anvil-http-offload-timeout-sec))
         (form (anvil-http--offload-form method url opts))
         (future (anvil-offload form :require '(anvil-http))))
    (anvil-future-await future timeout)
    (cond
     ((anvil-future-error future)
      (signal 'anvil-server-tool-error
              (list (format "anvil-http: offload error for %s: %s"
                            url (anvil-future-error future)))))
     ((not (anvil-future-done-p future))
      (signal 'anvil-server-tool-error
              (list (format "anvil-http: offload await timed out (%ds) for %s"
                            timeout url))))
     (t (anvil-future-value future)))))

;;;; --- POST / auth helpers (Phase 1.5) ------------------------------------

;; Phase 6.2 (Doc 24) で nelisp-http に同名 helper が port 済。本実装
;; は nelisp-http が load-path にあれば nelisp 版へ delegate、なければ
;; 自前 fallback (anvil 単体動作保証)。3 helpers は purely functional、
;; cache / I/O 状態を持たないので backward compat 100%。

(defun anvil-http--url-encode-form (alist)
  "Return ALIST encoded as application/x-www-form-urlencoded.
Delegates to `nelisp-http--url-encode-form' when available."
  (if (fboundp 'nelisp-http--url-encode-form)
      (nelisp-http--url-encode-form alist)
    (mapconcat
     (lambda (pair)
       (format "%s=%s"
               (url-hexify-string (format "%s" (car pair)))
               (url-hexify-string (format "%s" (cdr pair)))))
     alist
     "&")))

(defun anvil-http--plist-to-hash (plist)
  "Return PLIST as a JSON-friendly hash table.
Delegates to `nelisp-http--plist-to-hash' when available."
  (if (fboundp 'nelisp-http--plist-to-hash)
      (nelisp-http--plist-to-hash plist)
    (let ((h (make-hash-table :test 'equal)))
      (while plist
        (let ((k (car plist))
              (v (cadr plist)))
          (puthash (cond ((keywordp k) (substring (symbol-name k) 1))
                         ((symbolp k) (symbol-name k))
                         (t (format "%s" k)))
                   v h))
        (setq plist (cddr plist)))
      h)))

(defun anvil-http--alist-of-string-pairs-p (x)
  "Non-nil when X looks like an alist of (KEY . VAL) pairs.
Delegates to `nelisp-http--alist-of-string-pairs-p' when available."
  (if (fboundp 'nelisp-http--alist-of-string-pairs-p)
      (nelisp-http--alist-of-string-pairs-p x)
    (and (consp x)
         (not (keywordp (car x)))
         (consp (car x))
         (not (consp (cdr (car x)))))))

(defun anvil-http--encode-body (body)
  "Encode BODY into (DATA . CONTENT-TYPE).
- nil → (nil . nil)
- string → (BODY . nil) — caller specifies Content-Type via headers.
- alist of (KEY . VAL) → (form-urlencoded-string .
                         \"application/x-www-form-urlencoded\")
- plist (starts with keyword) → (json-string . \"application/json\")

Delegates to `nelisp-http--encode-body' when available.  nelisp's
generic `error' is translated to `anvil-server-tool-error' so callers
keep observing anvil's existing signal type."
  (if (fboundp 'nelisp-http--encode-body)
      (condition-case err
          (nelisp-http--encode-body body)
        (error
         (signal 'anvil-server-tool-error
                 (list (or (cadr err)
                           (format "anvil-http: cannot encode body of type %S"
                                   (type-of body)))))))
    (cond
     ((null body) (cons nil nil))
     ((stringp body) (cons body nil))
     ((anvil-http--alist-of-string-pairs-p body)
      (cons (anvil-http--url-encode-form body)
            "application/x-www-form-urlencoded"))
     ((and (listp body) (keywordp (car body)))
      (cons (json-serialize (anvil-http--plist-to-hash body)
                            :null-object :null
                            :false-object :false)
            "application/json"))
     (t (signal 'anvil-server-tool-error
                (list (format "anvil-http: cannot encode body of type %S"
                              (type-of body))))))))

(defun anvil-http--apply-auth (headers auth)
  "Augment HEADERS alist with credentials from AUTH plist.
AUTH forms (only one keyword recognised at a time):
  (:bearer TOKEN)             → Authorization: Bearer TOKEN
  (:basic (USER . PASS))      → Authorization: Basic base64(USER:PASS)
  (:basic USER PASS)          → same as above (positional)
  (:header (NAME . VALUE))    → custom request header

A list of these forms is also accepted; each is applied in order.
Existing Authorization in HEADERS is overwritten by Bearer/Basic.

Delegates to `nelisp-http--apply-auth' when available."
  (if (fboundp 'nelisp-http--apply-auth)
      (nelisp-http--apply-auth headers auth)
    (cond
     ((null auth) headers)
     ;; multi-spec list: ((:bearer ...) (:header ...) ...)
     ((and (consp auth) (consp (car auth)) (keywordp (caar auth)))
      (cl-reduce (lambda (h spec) (anvil-http--apply-auth h spec))
                 auth :initial-value headers))
     (t
      (pcase (car-safe auth)
        (:bearer
         (cons (cons "Authorization"
                     (format "Bearer %s" (cadr auth)))
               (assq-delete-all "Authorization" (copy-sequence headers))))
        (:basic
         (let* ((tail (cdr auth))
                (user (if (consp (car tail)) (caar tail) (car tail)))
                (pass (if (consp (car tail)) (cdar tail) (cadr tail)))
                (encoded (base64-encode-string
                          (encode-coding-string
                           (format "%s:%s" user pass) 'utf-8)
                          t)))
           (cons (cons "Authorization" (format "Basic %s" encoded))
                 (assq-delete-all "Authorization"
                                  (copy-sequence headers)))))
        (:header
         (let ((pair (cadr auth)))
           (cons (cons (car pair) (cdr pair)) headers)))
        (_ headers))))))

;;;; --- public Elisp API ---------------------------------------------------

;;;###autoload
(cl-defun anvil-http-get (url &key headers accept timeout-sec
                              no-cache cache-ttl-sec if-newer-than
                              selector json-path
                              body-mode header-filter
                              skip-robots-check
                              offload offload-timeout)
  "GET URL and return a response plist.

Keyword args:
  :headers         alist of (STRING . STRING) extra request headers
  :accept          MIME string added as Accept header (short-hand)
  :timeout-sec     override `anvil-http-timeout-sec'
  :no-cache        skip cache reads AND writes
  :cache-ttl-sec   override `anvil-http-cache-ttl-sec'
  :if-newer-than   unix epoch int; sends If-Modified-Since
  :selector        CSS-subset selector (tag, .class, #id, tag.class,
                   tag#id) evaluated against text/html bodies; libxml
                   is the primary engine, a regex-subset fallback runs
                   on Emacs builds without libxml
  :json-path       Dotted path (e.g. `data.results[0].id',
                   `items[*].name') applied to application/json bodies
  :body-mode       nil / `auto' (default: spill to disk + head slice
                   when body exceeds `anvil-http-max-inline-body-bytes'),
                   `full' (always inline), `head-only' (drop tail, no
                   spill), `meta-only' (drop body entirely)
  :header-filter   nil (uses `anvil-http-header-filter-default'),
                   `minimal' (only `anvil-http-minimal-header-keys'),
                   `all' (full response headers)
  :skip-robots-check  Internal — bypass the robots.txt pre-check.
                   Set by `anvil-http--robots-fetch' on its own GET to
                   avoid recursion; callers should normally leave this
                   nil.  robots.txt URLs are auto-detected and skip
                   the check regardless.

Returns (:status :headers :body :from-cache :cached-at :final-url
:elapsed-ms).  When :selector or :json-path is supplied the plist
also carries :extract-mode and :extract-engine; on a no-match or
content-type mismatch :extract-miss t is set and :body is the full
original response.  When :body-mode triggers overflow the plist
also carries :body-truncated, :total-bytes, :body-overflow-path,
and :body-sha256 so callers can `file-read' the remainder only
when actually needed.

When `anvil-http-respect-robots-txt' is non-nil the origin's
robots.txt is fetched once (24h TTL cache) and the requested URL
is matched against its rules for `anvil-http-user-agent'; a
Disallow hit raises `user-error' before any network round-trip."
  (anvil-http--check-url url)
  (anvil-state-enable)
  (when (and anvil-http-respect-robots-txt
             (not skip-robots-check)
             (not (anvil-http--is-robots-url-p url)))
    (anvil-http--robots-check-signal url))
  (when offload
    (cl-return-from anvil-http-get
      (anvil-http--offload-fetch
       "GET" url
       (list :headers headers :accept accept
             :timeout-sec timeout-sec
             :selector selector :json-path json-path
             :body-mode body-mode :header-filter header-filter
             :offload-timeout offload-timeout))))
  (anvil-http--metrics-bump :requests)
  (anvil-http--apply-header-filter
   (anvil-http--apply-body-mode
    (anvil-http--apply-extract
     (let* ((norm-url (anvil-http--normalize-url url))
         (ttl (or cache-ttl-sec anvil-http-cache-ttl-sec))
         (now (float-time))
         (cached (unless no-cache (anvil-http--cache-get norm-url))))
    ;; Fresh TTL hit → zero network.
    (if (and cached
             (numberp ttl) (> ttl 0)
             (numberp (plist-get cached :fetched-at))
             (< (- now (plist-get cached :fetched-at)) ttl))
        (progn
          (anvil-http--metrics-bump :cache-fresh)
          (anvil-http--metrics-log url 0
                                    (plist-get cached :status) 'fresh)
          (anvil-http--response-plist cached t 0))
      ;; Otherwise hit network, possibly conditionally.
      (let* ((extra (anvil-http--build-request-headers
                     headers accept cached if-newer-than))
             (timeout (or timeout-sec anvil-http-timeout-sec))
             (start (float-time))
             (resp (anvil-http--request-with-retry "GET" url extra timeout))
             (elapsed-ms (round (* 1000 (- (float-time) start))))
             (status (plist-get resp :status))
             (headers* (plist-get resp :headers))
             (body (plist-get resp :body))
             (final (plist-get resp :final-url)))
        (cond
         ((= status 304)
          (unless cached
            (anvil-http--metrics-bump :errors)
            (signal 'anvil-server-tool-error
                    (list (format "anvil-http: 304 from %s but no cache entry"
                                  url))))
          (let* ((merged-headers
                  ;; Prefer the new validators / Date but keep cached body.
                  (anvil-http--merge-headers (plist-get cached :headers)
                                             headers*))
                 (refreshed (anvil-http--cache-entry
                             (plist-get cached :status)
                             merged-headers
                             (plist-get cached :body)
                             now
                             (or final (plist-get cached :final-url)))))
            (unless no-cache (anvil-http--cache-put norm-url refreshed))
            (anvil-http--metrics-bump :cache-revalidated)
            (anvil-http--metrics-log url elapsed-ms 304 'revalidated)
            (anvil-http--response-plist refreshed t elapsed-ms)))
         ((and (>= status 200) (< status 300))
          (let ((entry (anvil-http--cache-entry status headers* body now final)))
            (unless no-cache (anvil-http--cache-put norm-url entry))
            (anvil-http--metrics-bump :network-200)
            (anvil-http--metrics-log url elapsed-ms status 'miss)
            (anvil-http--response-plist entry nil elapsed-ms)))
         (t
          (anvil-http--metrics-bump :errors)
          (anvil-http--metrics-log url elapsed-ms status 'error)
          (signal 'anvil-server-tool-error
                  (list (format "anvil-http: HTTP %d for %s" status url))))))))
     selector json-path)
    body-mode)
   header-filter))

(defun anvil-http--merge-headers (base override)
  "Shallow-merge two header plists, OVERRIDE winning on clashes."
  (let ((out (copy-sequence base)))
    (let ((tail override))
      (while tail
        (setq out (plist-put out (car tail) (cadr tail)))
        (setq tail (cddr tail))))
    out))

;;;###autoload
(cl-defun anvil-http-head (url &key headers timeout-sec)
  "HEAD URL and return (:status :headers :final-url :elapsed-ms).
HEAD responses never touch the cache."
  (anvil-http--check-url url)
  (anvil-http--metrics-bump :requests)
  (let* ((extra (anvil-http--build-request-headers headers nil nil nil))
         (timeout (or timeout-sec anvil-http-timeout-sec))
         (start (float-time))
         (resp (anvil-http--request-with-retry "HEAD" url extra timeout))
         (elapsed-ms (round (* 1000 (- (float-time) start))))
         (status (plist-get resp :status)))
    (anvil-http--metrics-log url elapsed-ms status 'head)
    (list :status status
          :headers (plist-get resp :headers)
          :final-url (plist-get resp :final-url)
          :elapsed-ms elapsed-ms)))

;;;###autoload
(cl-defun anvil-http-post (url &key body content-type headers
                               accept timeout-sec auth
                               body-mode header-filter
                               skip-robots-check
                               offload offload-timeout)
  "POST URL with BODY and return a response plist.

Keyword args:
  :body          Request body.  String → sent verbatim (caller sets
                 :content-type or :headers).  Alist of (KEY . VALUE) →
                 form-urlencoded (Content-Type forced to
                 application/x-www-form-urlencoded).  Plist starting
                 with a keyword → JSON serialised (Content-Type forced
                 to application/json).  nil → empty body.
  :content-type  Override Content-Type.  Wins over the auto-derived
                 type from a structured :body but loses to a value
                 already present in :headers.
  :headers       Alist of extra request headers (string keys).
  :accept        MIME string added as Accept header (short-hand).
  :timeout-sec   Override `anvil-http-timeout-sec'.
  :auth          Plist auth spec — `(:bearer TOKEN)' /
                 `(:basic (USER . PASS))' / `(:header (NAME . VALUE))',
                 or a list of such specs.  See
                 `anvil-http--apply-auth' for details.
  :body-mode     Same as `anvil-http-get'.  POST responses are not
                 cached, so cache semantics do not apply.
  :header-filter Same as `anvil-http-get'.
  :skip-robots-check  Internal — bypass robots.txt pre-check.

Returns (:status :headers :body :from-cache nil :final-url
:elapsed-ms).  POSTs are never cached and always retry on the
retryable status set (5xx / 408 / 429), same as GET.  Robots.txt
enforcement applies — a Disallow on the target URL raises
`user-error' before any network round-trip."
  (anvil-http--check-url url)
  (when (and anvil-http-respect-robots-txt
             (not skip-robots-check)
             (not (anvil-http--is-robots-url-p url)))
    (anvil-http--robots-check-signal url))
  (when offload
    (cl-return-from anvil-http-post
      (anvil-http--offload-fetch
       "POST" url
       (list :body body :content-type content-type
             :headers headers :accept accept
             :timeout-sec timeout-sec :auth auth
             :body-mode body-mode :header-filter header-filter
             :offload-timeout offload-timeout))))
  (anvil-http--metrics-bump :requests)
  (let* ((encoded (anvil-http--encode-body body))
         (data (car encoded))
         (auto-ct (cdr encoded))
         (with-auth (anvil-http--apply-auth (or headers nil) auth))
         (final-headers
          (let ((h with-auth))
            ;; Add Accept if requested and not already present.
            (when (and accept (not (assoc "Accept" h)))
              (setq h (cons (cons "Accept" accept) h)))
            ;; Add Content-Type if not already present.
            (let ((ct (or content-type auto-ct)))
              (when (and ct (not (assoc "Content-Type" h)))
                (setq h (cons (cons "Content-Type" ct) h))))
            h))
         (timeout (or timeout-sec anvil-http-timeout-sec))
         (start (float-time))
         (resp (anvil-http--request-with-retry
                "POST" url final-headers timeout data))
         (elapsed-ms (round (* 1000 (- (float-time) start))))
         (status (plist-get resp :status)))
    (anvil-http--metrics-log url elapsed-ms status 'post)
    (cond
     ((and (integerp status) (>= status 200) (< status 300))
      (anvil-http--apply-header-filter
       (anvil-http--apply-body-mode
        (list :status status
              :headers (plist-get resp :headers)
              :body (plist-get resp :body)
              :from-cache nil
              :cached-at nil
              :final-url (plist-get resp :final-url)
              :elapsed-ms elapsed-ms)
        body-mode)
       header-filter))
     (t
      (anvil-http--metrics-bump :errors)
      (signal 'anvil-server-tool-error
              (list (format "anvil-http: HTTP %s for POST %s"
                            status url)))))))

;;;; --- batch fetch (Phase 1d) ---------------------------------------------

(defun anvil-http--post-process (resp selector json-path body-mode header-filter)
  "Apply extract → body-mode → header-filter to RESP in order."
  (anvil-http--apply-header-filter
   (anvil-http--apply-body-mode
    (anvil-http--apply-extract resp selector json-path)
    body-mode)
   header-filter))

(defun anvil-http--batch-handle-network (resp url now norm cached no-cache
                                              selector json-path
                                              body-mode header-filter)
  "Decide what to return for URL given async RESP.
RESP is either `(:status :headers :body :final-url)' from
`anvil-http--request-async' or `(:error STR)' on failure.  CACHED
is the existing cache entry (for 304 revalidation)."
  (cond
   ((plist-get resp :error)
    (list :url url :error (plist-get resp :error)))
   (t
    (let ((status (plist-get resp :status)))
      (cond
       ((eq status 304)
        (if cached
            (let* ((merged (anvil-http--merge-headers
                            (plist-get cached :headers)
                            (plist-get resp :headers)))
                   (refreshed (anvil-http--cache-entry
                               (plist-get cached :status)
                               merged
                               (plist-get cached :body)
                               now
                               (or (plist-get resp :final-url)
                                   (plist-get cached :final-url)))))
              (unless no-cache (anvil-http--cache-put norm refreshed))
              (anvil-http--post-process
               (anvil-http--response-plist refreshed t 0)
               selector json-path body-mode header-filter))
          (list :url url :error "304 without cache")))
       ((and (integerp status) (>= status 200) (< status 300))
        (let ((entry (anvil-http--cache-entry
                      status
                      (plist-get resp :headers)
                      (plist-get resp :body)
                      now
                      (plist-get resp :final-url))))
          (unless no-cache (anvil-http--cache-put norm entry))
          (anvil-http--post-process
           (anvil-http--response-plist entry nil 0)
           selector json-path body-mode header-filter)))
       (t
        (list :url url
              :error (format "HTTP %s" status))))))))

;;;###autoload
(cl-defun anvil-http-get-batch (urls &key concurrency timeout
                                     selector json-path
                                     body-mode header-filter
                                     no-cache cache-ttl-sec
                                     skip-robots-check)
  "Fetch URLS concurrently, return a list of response plists in input order.
Concurrency is bounded by CONCURRENCY (defaulting to
`anvil-http-batch-concurrency').  Each URL is cache-checked first;
TTL-fresh hits short-circuit the async fan-out entirely.  Per-URL
failures surface as (:url URL :error STR) plists so one bad entry
does not sink the whole batch — the caller loops the return value.

Shared keyword args SELECTOR / JSON-PATH / BODY-MODE / HEADER-FILTER
are applied post-process to every successful response; per-URL
overrides are out of scope for Phase 1d.

SKIP-ROBOTS-CHECK bypasses the robots.txt pre-check (same semantics
as `anvil-http-get'); otherwise each URL is robots-evaluated before
any network round-trip."
  (anvil-state-enable)
  (let* ((urls (append urls nil))
         (n (length urls))
         (concurrency (or concurrency anvil-http-batch-concurrency))
         (timeout* (or timeout anvil-http-timeout-sec))
         (ttl (or cache-ttl-sec anvil-http-cache-ttl-sec))
         (now (float-time))
         (results (make-vector n nil))
         (pending 0))
    (when (> n anvil-http-batch-max)
      (user-error
       "anvil-http: batch URL count %d exceeds `anvil-http-batch-max' %d"
       n anvil-http-batch-max))
    (let ((url-queue-parallel-processes concurrency)
          (url-queue-timeout timeout*))
      (cl-loop
       for url in urls
       for i from 0
       do
       (condition-case err
           (progn
             (anvil-http--check-url url)
             (when (and anvil-http-respect-robots-txt
                        (not skip-robots-check)
                        (not (anvil-http--is-robots-url-p url)))
               (anvil-http--robots-check-signal url))
             (let* ((norm (anvil-http--normalize-url url))
                    (cached (unless no-cache
                              (anvil-http--cache-get norm))))
               (cond
                ((and cached (numberp ttl) (> ttl 0)
                      (numberp (plist-get cached :fetched-at))
                      (< (- now (plist-get cached :fetched-at)) ttl))
                 (aset results i
                       (anvil-http--post-process
                        (anvil-http--response-plist cached t 0)
                        selector json-path body-mode header-filter)))
                (t
                 (cl-incf pending)
                 (let* ((idx i)
                        (cb-norm norm)
                        (cb-cached cached)
                        (extra (anvil-http--build-request-headers
                                nil nil cached nil)))
                   (anvil-http--request-async
                    url extra timeout*
                    (lambda (resp)
                      (aset results idx
                            (anvil-http--batch-handle-network
                             resp url now cb-norm cb-cached no-cache
                             selector json-path body-mode header-filter))
                      (setq pending (1- pending)))))))))
         (error
          (aset results i
                (list :url url :error (error-message-string err))))))
      (while (> pending 0)
        (accept-process-output nil 0.1)))
    (append results nil)))

;;;###autoload
(defun anvil-http-cache-clear (&optional url)
  "Remove cache entries.  With URL, drop just that key; nil flushes http ns."
  (anvil-state-enable)
  (if (and (stringp url) (not (string-empty-p url)))
      (if (anvil-http--cache-delete (anvil-http--normalize-url url)) 1 0)
    (or (anvil-http--cache-clear-all) 0)))

(defun anvil-http--url-sha (url)
  "Return sha256(normalised URL) as hex string.
Used as the stable identifier of a cached response for Doc 28
`http-cache://' citation URIs.  Normalisation goes through
`anvil-http--normalize-url' so capitalisation / fragment / default
port variants collapse to a single sha."
  (secure-hash 'sha256 (anvil-http--normalize-url url)))

;;;###autoload
(cl-defun anvil-http-cache-list (&key limit)
  "Return live cache entries as plists, sorted by URL.
Each entry: (:url NORM-URL :sha SHA256 :status S :fetched-at E
             :body-length N :content-type CT).  Bodies are
*excluded* to keep the Layer-1 listing cheap — call
`anvil-http-cache-get' with the sha to pull the body."
  (anvil-state-enable)
  (let ((urls (anvil-state-list-keys :ns anvil-http--state-ns
                                     :limit limit)))
    (delq nil
          (mapcar
           (lambda (url)
             (let ((entry (anvil-http--cache-get url)))
               (when entry
                 (list :url url
                       :sha (anvil-http--url-sha url)
                       :status (plist-get entry :status)
                       :fetched-at (plist-get entry :fetched-at)
                       :body-length (length (or (plist-get entry :body)
                                                ""))
                       :content-type
                       (plist-get (plist-get entry :headers)
                                  :content-type)))))
           urls))))

;;;###autoload
(defun anvil-http-cache-get (sha)
  "Return the cached response whose normalised URL hashes to SHA.
Accepts either a raw sha256 hex string or an `http-cache://SHA'
citation URI.  Returns the full cache-entry plist (including :body,
prefixed with :url and :sha) or nil when SHA does not match any
live entry."
  (anvil-state-enable)
  (let ((target (if (and (stringp sha)
                         (string-prefix-p "http-cache://" sha))
                    (substring sha (length "http-cache://"))
                  sha)))
    (cl-loop for url in (anvil-state-list-keys :ns anvil-http--state-ns)
             when (equal (anvil-http--url-sha url) target)
             return (let ((entry (anvil-http--cache-get url)))
                      (and entry
                           (append (list :url url :sha target)
                                   entry))))))

;;;; --- MCP tool handlers --------------------------------------------------

(defun anvil-http--tool-fetch (url &optional if_newer_than accept
                                   timeout_sec no_cache
                                   selector json_path
                                   body_mode header_filter
                                   offload offload_timeout_sec)
  "GET URL through `anvil-http-get' and return the response plist.

MCP Parameters:
  url          - Absolute http/https URL to fetch.
  if_newer_than - Optional unix epoch seconds; adds If-Modified-Since so
                  the server can return 304 when nothing changed.
  accept       - Optional Accept header value (e.g. \"application/json\").
                  Passed through verbatim.
  timeout_sec  - Optional request timeout override (integer seconds).
  no_cache     - Non-nil skips cache on both read and write (any truthy
                  value).
  selector     - Optional CSS-subset selector (tag, .class, #id,
                  tag.class, tag#id) applied to text/html bodies.
                  Server-side extraction — typical 20-50x token
                  savings vs returning the full page.  libxml + dom.el
                  is the primary engine; a regex-subset fallback runs
                  on Emacs builds without libxml.
  json_path    - Optional dotted JSON path (e.g. `data.results[0].id',
                  `items[*].name') applied to application/json bodies.
                  Wildcard `[*]' flattens arrays.
  body_mode    - Optional body size strategy:
                  `auto' (default) spills >200KB bodies to a temp file
                  and returns a 2KB head slice + :body-overflow-path;
                  `full' inlines regardless of size;
                  `head-only' keeps the head slice only (no spill);
                  `meta-only' drops the body entirely.
  header_filter - Optional header pruning:
                  `minimal' (default) keeps only Content-Type,
                  Content-Length, ETag, Last-Modified, Content-Encoding,
                  Location, Retry-After;
                  `all' returns every response header.
  offload      - Non-nil offloads the actual fetch to an
                  `anvil-offload' worker subprocess so the main
                  daemon stays responsive.  Robots.txt is still
                  evaluated in the parent before dispatch.  Cache
                  reads / writes are skipped on the worker side.
  offload_timeout_sec - Optional integer await timeout for the
                  offloaded fetch (default
                  `anvil-http-offload-timeout-sec', 120).

Returns (:status :headers :body :from-cache :cached-at :final-url
:elapsed-ms).  Same-URL calls within `anvil-http-cache-ttl-sec'
seconds are served from the cache without any network round-trip.
Otherwise conditional GET (If-None-Match / If-Modified-Since) is
performed and a 304 revalidates the cached body with zero body
transfer.

When selector / json_path is supplied the response plist also
carries :extract-mode and :extract-engine; on a no-match or
content-type mismatch :extract-miss t is set and :body is the full
original response, so callers can tell `no match' apart from
`server returned an empty body'.

When body_mode triggers overflow the plist also carries
:body-truncated t, :total-bytes INT, :body-overflow-path STR, and
:body-sha256 HEX — the caller can `file-read' the overflow path
with offset/limit to pull only the slice it actually needs."
  (anvil-server-with-error-handling
   (let ((if-newer (cond ((null if_newer_than) nil)
                         ((integerp if_newer_than) if_newer_than)
                         ((and (stringp if_newer_than)
                               (string-match "\\`[0-9]+\\'" if_newer_than))
                          (string-to-number if_newer_than))
                         (t nil)))
         (timeout (cond ((null timeout_sec) nil)
                        ((integerp timeout_sec) timeout_sec)
                        ((and (stringp timeout_sec)
                              (string-match "\\`[0-9]+\\'" timeout_sec))
                         (string-to-number timeout_sec))
                        (t nil)))
         (accept* (and (stringp accept) (not (string-empty-p accept)) accept))
         (no-c (and no_cache (not (equal no_cache ""))))
         (sel (and (stringp selector) (not (string-empty-p selector))
                   selector))
         (jp (and (stringp json_path) (not (string-empty-p json_path))
                  json_path))
         (bm (anvil-http--coerce-symbol-arg body_mode))
         (hf (anvil-http--coerce-symbol-arg header_filter))
         (off (and offload (not (equal offload ""))))
         (off-timeout
          (cond ((null offload_timeout_sec) nil)
                ((integerp offload_timeout_sec) offload_timeout_sec)
                ((and (stringp offload_timeout_sec)
                      (string-match-p "\\`[0-9]+\\'" offload_timeout_sec))
                 (string-to-number offload_timeout_sec))
                (t nil))))
     (anvil-http-get url
                     :accept accept*
                     :timeout-sec timeout
                     :no-cache no-c
                     :if-newer-than if-newer
                     :selector sel
                     :json-path jp
                     :body-mode bm
                     :header-filter hf
                     :offload off
                     :offload-timeout off-timeout))))

(defun anvil-http--tool-head (url &optional timeout_sec)
  "HEAD URL and return (:status :headers :final-url :elapsed-ms).

MCP Parameters:
  url          - Absolute http/https URL to probe.
  timeout_sec  - Optional request timeout override (integer seconds).

HEAD responses are never cached; the tool exists for cheap metadata
probes (Content-Type, Content-Length, reachability checks)."
  (anvil-server-with-error-handling
   (let ((timeout (cond ((null timeout_sec) nil)
                        ((integerp timeout_sec) timeout_sec)
                        ((and (stringp timeout_sec)
                              (string-match "\\`[0-9]+\\'" timeout_sec))
                         (string-to-number timeout_sec))
                        (t nil))))
     (anvil-http-head url :timeout-sec timeout))))

(defun anvil-http--tool-post (url &optional body content_type
                                   bearer_token headers_json
                                   accept timeout_sec
                                   body_mode header_filter
                                   offload offload_timeout_sec)
  "POST to URL through `anvil-http-post' and return the response.

MCP Parameters:
  url           - Absolute http/https URL to POST to.
  body          - Request body string (caller pre-encodes JSON / form
                  data; anvil-http does not parse).  Empty string =
                  no body.
  content_type  - Optional Content-Type override (e.g. \"application/json\").
  bearer_token  - Optional OAuth-style bearer token; sent as
                  `Authorization: Bearer ...'.
  headers_json  - Optional JSON object string of extra request headers,
                  e.g. {\"X-Api-Key\":\"...\",\"X-Trace\":\"42\"}.
                  Merged with auth-derived headers.
  accept        - Optional Accept header value.
  timeout_sec   - Optional request timeout override.
  body_mode     - Same as `http-fetch' (auto / full / head-only / meta-only).
  header_filter - Same as `http-fetch' (minimal / all).
  offload       - Non-nil offloads the actual POST to an
                  `anvil-offload' worker subprocess; main daemon stays
                  responsive.  Robots.txt evaluated in the parent.
  offload_timeout_sec - Optional integer await timeout (default 120).

Returns (:status :headers :body :from-cache :cached-at :final-url
:elapsed-ms).  POSTs are never cached.  Retries 5xx / 408 / 429
with jittered exponential backoff (Retry-After honoured on 429).
Robots.txt is enforced for the target URL."
  (anvil-server-with-error-handling
   (let* ((body* (and (stringp body) (not (string-empty-p body)) body))
          (ct (and (stringp content_type) (not (string-empty-p content_type))
                   content_type))
          (timeout (cond ((null timeout_sec) nil)
                         ((integerp timeout_sec) timeout_sec)
                         ((and (stringp timeout_sec)
                               (string-match-p "\\`[0-9]+\\'" timeout_sec))
                          (string-to-number timeout_sec))
                         (t nil)))
          (accept* (and (stringp accept) (not (string-empty-p accept))
                        accept))
          (bm (anvil-http--coerce-symbol-arg body_mode))
          (hf (anvil-http--coerce-symbol-arg header_filter))
          (extra-headers
           (when (and (stringp headers_json)
                      (not (string-empty-p headers_json)))
             (let ((parsed (json-parse-string headers_json
                                              :object-type 'alist
                                              :null-object nil
                                              :false-object :false)))
               (mapcar (lambda (kv)
                         (cons (format "%s" (car kv))
                               (format "%s" (cdr kv))))
                       parsed))))
          (auth (and (stringp bearer_token)
                     (not (string-empty-p bearer_token))
                     (list :bearer bearer_token)))
          (off (and offload (not (equal offload ""))))
          (off-timeout
           (cond ((null offload_timeout_sec) nil)
                 ((integerp offload_timeout_sec) offload_timeout_sec)
                 ((and (stringp offload_timeout_sec)
                       (string-match-p "\\`[0-9]+\\'" offload_timeout_sec))
                  (string-to-number offload_timeout_sec))
                 (t nil))))
     (anvil-http-post url
                      :body body*
                      :content-type ct
                      :headers extra-headers
                      :accept accept*
                      :timeout-sec timeout
                      :auth auth
                      :body-mode bm
                      :header-filter hf
                      :offload off
                      :offload-timeout off-timeout))))

(defun anvil-http--tool-fetch-batch (urls &optional concurrency timeout_sec
                                           selector json_path
                                           body_mode header_filter
                                           no_cache)
  "Batch-fetch URLS (array of http/https URLs) in parallel.

MCP Parameters:
  urls          - Array of absolute http/https URLs to fetch.
                  Capped by `anvil-http-batch-max' (default 16).
  concurrency   - Optional parallel limit (default
                  `anvil-http-batch-concurrency', 4).
  timeout_sec   - Optional per-request timeout override (integer).
  selector      - Same as `http-fetch'; applied to every HTML result.
  json_path     - Same as `http-fetch'; applied to every JSON result.
  body_mode     - Same as `http-fetch'; shared body-size strategy.
  header_filter - Same as `http-fetch'; shared header filter.
  no_cache      - Non-nil skips cache on read and write for every URL.

Returns an array of response plists in INPUT ORDER.  Successes use
the same shape as `http-fetch'; per-URL failures surface as
(:url URL :error STR) so one bad entry does not sink the batch.

Uses `url-queue-retrieve' under the hood — concurrency is bounded
to CONCURRENCY simultaneous requests and TTL-fresh cache hits
short-circuit the network entirely."
  (anvil-server-with-error-handling
   (let ((url-list (cond
                    ((vectorp urls) (append urls nil))
                    ((listp urls) urls)
                    (t (user-error
                        "anvil-http: urls must be an array, got %S"
                        (type-of urls)))))
         (conc (cond ((null concurrency) nil)
                     ((integerp concurrency) concurrency)
                     ((and (stringp concurrency)
                           (string-match-p "\\`[0-9]+\\'" concurrency))
                      (string-to-number concurrency))
                     (t nil)))
         (timeout (cond ((null timeout_sec) nil)
                        ((integerp timeout_sec) timeout_sec)
                        ((and (stringp timeout_sec)
                              (string-match-p "\\`[0-9]+\\'" timeout_sec))
                         (string-to-number timeout_sec))
                        (t nil)))
         (sel (and (stringp selector) (not (string-empty-p selector))
                   selector))
         (jp (and (stringp json_path) (not (string-empty-p json_path))
                  json_path))
         (bm (anvil-http--coerce-symbol-arg body_mode))
         (hf (anvil-http--coerce-symbol-arg header_filter))
         (no-c (and no_cache (not (equal no_cache "")))))
     (anvil-http-get-batch url-list
                           :concurrency conc
                           :timeout timeout
                           :selector sel
                           :json-path jp
                           :body-mode bm
                           :header-filter hf
                           :no-cache no-c))))

(defun anvil-http--robots-tool-payload (url ua)
  "Return the `http-robots-check' response plist for URL and UA.
Factored out of the tool body so the tool handler ends on a
function call — the release audit flags tools whose last form is
a literal `(list :KEYWORD ...)' plist (a sentinel for MCP
encoding bugs)."
  (let ((r (anvil-http--robots-evaluate url ua)))
    (list :url url
          :origin (plist-get r :origin)
          :user-agent ua
          :allowed (plist-get r :allowed)
          :rule-length (plist-get r :rule-length)
          :robots-present (plist-get r :robots-present))))

(defun anvil-http--tool-robots-check (url &optional user_agent)
  "Report whether URL is allowed by its origin's robots.txt.

MCP Parameters:
  url          - Absolute http/https URL to test.
  user_agent   - Optional UA string to match (default `anvil-http-user-agent').

Returns (:url :origin :user-agent :allowed :rule-length :robots-present).
`allowed' is t when no Disallow rule matches or the site has no
robots.txt (fail-open per RFC 9309).  `rule-length' is the length
of the winning pattern when a rule matched (for observability).
`robots-present' is nil when the robots.txt fetch returned a
non-200 or failed."
  (anvil-server-with-error-handling
   (let ((ua (if (and (stringp user_agent)
                      (not (string-empty-p user_agent)))
                 user_agent
               anvil-http-user-agent)))
     (anvil-http--robots-tool-payload url ua))))

(defun anvil-http--cache-status-payload ()
  "Return the http-cache-status response plist."
  (anvil-state-enable)
  (list :ns anvil-http--state-ns
        :entries (or (anvil-state-count anvil-http--state-ns) 0)
        :bytes anvil-http--cache-bytes-counter
        :cap anvil-http-cache-size-cap-bytes
        :evictions (or (plist-get anvil-http--metrics :evictions) 0)
        :requests (or (plist-get anvil-http--metrics :requests) 0)
        :cache-fresh (or (plist-get anvil-http--metrics :cache-fresh) 0)
        :cache-revalidated
        (or (plist-get anvil-http--metrics :cache-revalidated) 0)
        :network-200 (or (plist-get anvil-http--metrics :network-200) 0)
        :errors (or (plist-get anvil-http--metrics :errors) 0)))

(defun anvil-http--tool-cache-status ()
  "Return cache size, entry count, eviction counter, and metrics.

MCP Parameters: (none)

Returns (:ns :entries :bytes :cap :evictions :requests :cache-fresh
:cache-revalidated :network-200 :errors).  The byte counter is the
running approximation maintained by `--cache-put' / `--cache-delete';
call `http-cache-clear' or fully reload the module to force a
recompute (see `anvil-http--cache-recompute-bytes')."
  (anvil-server-with-error-handling
   (anvil-http--cache-status-payload)))

(defun anvil-http--tool-cache-clear (&optional url)
  "Remove cached entries from the http namespace.

MCP Parameters:
  url - Optional URL to drop.  When absent or empty, flushes every
        entry under the http namespace.

Returns the number of deleted rows (0 or 1 for single-URL, or the
total removed for a namespace flush)."
  (anvil-server-with-error-handling
   (let ((u (and (stringp url) (not (string-empty-p url)) url)))
     (anvil-http-cache-clear u))))

;;;; --- status / introspection ---------------------------------------------

;;;###autoload
(defun anvil-http-status ()
  "Show request counters and recent-request log in *Anvil HTTP*."
  (interactive)
  (with-help-window "*Anvil HTTP*"
    (princ "Anvil HTTP — url-retrieve wrapper\n")
    (princ (format "User-Agent : %s\n" anvil-http-user-agent))
    (princ (format "Cache TTL  : %s sec\n" anvil-http-cache-ttl-sec))
    (princ (format "Timeout    : %s sec\n" anvil-http-timeout-sec))
    (princ (format "Retry      : max=%d, base=%dms\n"
                   anvil-http-retry-max anvil-http-retry-base-ms))
    (princ (format "Redirect   : max=%d\n" anvil-http-max-redirections))
    (princ (format "Requests   : %d   fresh: %d   revalidated: %d   net-200: %d   errors: %d\n"
                   (or (plist-get anvil-http--metrics :requests) 0)
                   (or (plist-get anvil-http--metrics :cache-fresh) 0)
                   (or (plist-get anvil-http--metrics :cache-revalidated) 0)
                   (or (plist-get anvil-http--metrics :network-200) 0)
                   (or (plist-get anvil-http--metrics :errors) 0)))
    (princ "\nRecent requests:\n")
    (dolist (entry (plist-get anvil-http--metrics :log))
      (princ (format "  %5dms  %3s  %-12s %s\n"
                     (or (plist-get entry :elapsed-ms) 0)
                     (or (plist-get entry :status) "?")
                     (symbol-name (or (plist-get entry :cache) 'unknown))
                     (plist-get entry :url))))))

;;;; --- module lifecycle ---------------------------------------------------

(defun anvil-http--register-tools ()
  "Register http-* MCP tools under `anvil-http--server-id'."
  (anvil-server-register-tool
   #'anvil-http--tool-fetch
   :id "http-fetch"
   :intent '(http)
   :layer 'io
   :server-id anvil-http--server-id
   :description
   "GET an http/https URL and return status, headers and body with
cache awareness.  Within `anvil-http-cache-ttl-sec' the cached entry
is served without any network call; otherwise a conditional GET
revalidates via If-None-Match / If-Modified-Since and a 304 is
served from cache with zero body transfer.  Retries 5xx / 408 / 429
with exponential backoff (honours Retry-After on 429).

Optional `selector' (CSS subset — tag, .class, #id, tag.class,
tag#id) evaluates server-side against text/html bodies; `json_path'
walks a dotted path (supports `[N]' index and `[*]' wildcard) through
application/json bodies.  Either cuts token cost dramatically vs
returning the full body.  On a no-match the full body is still
returned together with `extract_miss: true' so the caller can
distinguish that from an empty payload.

Optional `body_mode' controls large-body handling — the default
`auto' spills bodies over `anvil-http-max-inline-body-bytes'
(200KB) to a temp file and returns a head slice plus
`body_overflow_path'; `full' inlines regardless, `meta-only' drops
the body.  Optional `header_filter' prunes response headers —
`minimal' (default) keeps only the 7 keys Claude actually uses,
`all' returns everything."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-http--tool-head
   :id "http-head"
   :intent '(http)
   :layer 'io
   :server-id anvil-http--server-id
   :description
   "HEAD an http/https URL and return its status + response headers.
Cheap liveness / metadata probe; responses are never cached."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-http--tool-cache-clear
   :id "http-cache-clear"
   :intent '(http admin)
   :layer 'io
   :server-id anvil-http--server-id
   :description
   "Drop cached entries for the http namespace.  With a URL argument
removes just that entry; without, flushes every cached response.")

  (anvil-server-register-tool
   #'anvil-http--tool-cache-status
   :id "http-cache-status"
   :intent '(http diagnostics)
   :layer 'io
   :server-id anvil-http--server-id
   :description
   "Report cache size, entry count, the size cap, the eviction
counter, and request metrics.  Useful for confirming that LRU
eviction is firing under load and that the size cap is being
respected.  Read-only — does not touch the cache."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-http--tool-post
   :id "http-post"
   :intent '(http)
   :layer 'io
   :server-id anvil-http--server-id
   :description
   "POST to an http/https URL.  Body is sent verbatim — caller
pre-encodes JSON / form data.  Optional `bearer_token' adds an
`Authorization: Bearer ...' header; `headers_json' is a JSON
object string of extra headers (merged on top of auth).  Retries
5xx / 408 / 429 with jittered exponential backoff and honours
Retry-After on 429.  POSTs are never cached.  Robots.txt is
enforced (a Disallow on the target raises a user-error before
the network round-trip)."
   :read-only nil)

  (anvil-server-register-tool
   #'anvil-http--tool-fetch-batch
   :id "http-fetch-batch"
   :intent '(http)
   :layer 'io
   :server-id anvil-http--server-id
   :description
   "Fetch an array of http/https URLs concurrently; return the
responses in input order.  Each element of the returned array is
either a normal http-fetch response plist or (:url :error) when
that URL failed.  Concurrency defaults to 4 (tunable), capped at
`anvil-http-batch-max' (16) URLs per call.  TTL-fresh cache hits
short-circuit the async fan-out so a mostly-cached batch costs
one envelope + zero network.  Shared selector / json_path /
body_mode / header_filter apply to every response."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-http--tool-robots-check
   :id "http-robots-check"
   :intent '(http diagnostics)
   :layer 'io
   :server-id anvil-http--server-id
   :description
   "Test a URL against its origin's robots.txt without fetching the
URL itself.  Fetches /robots.txt (24h TTL) the first time, parses
RFC 9309 Allow / Disallow rules, picks the longest-matching UA
group, and returns `allowed' boolean plus observability fields.
Fail-open on missing or unreadable robots.txt."
   :read-only t))

(defun anvil-http--unregister-tools ()
  "Remove every http-* MCP tool from the shared server."
  (dolist (id '("http-fetch" "http-head" "http-cache-clear"
                "http-post" "http-fetch-batch" "http-robots-check"
                "http-cache-status"))
    (anvil-server-unregister-tool id anvil-http--server-id)))

;;;###autoload
(defun anvil-http-enable ()
  "Register http-* MCP tools and open the anvil-state backing store.
Recomputes the cache byte counter from the current SQLite state so
the LRU eviction starts from truth even after a daemon restart.
Also logs whether libxml is available so the HTML-selector engine
is obvious from a daemon startup log — the regex-subset fallback
handles the same selector subset but without combinators /
pseudo-classes."
  (interactive)
  (anvil-state-enable)
  (anvil-http--cache-recompute-bytes)
  (anvil-http--register-tools)
  (message "anvil-http: enabled; HTML selector engine = %s; cache = %s entries / %s bytes"
           (if (anvil-http--libxml-p) "libxml (primary)"
             "regex-subset (libxml not built-in)")
           (or (anvil-state-count anvil-http--state-ns) 0)
           anvil-http--cache-bytes-counter))

(defun anvil-http-disable ()
  "Unregister http-* MCP tools."
  (interactive)
  (anvil-http--unregister-tools))

(provide 'anvil-http)
;;; anvil-http.el ends here
