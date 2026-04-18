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
;; Phase 1b+ (not in this sub-phase): selector / json-path extract,
;; body-overflow to disk, header-filter, batch fetch, robots.txt.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)
(require 'anvil-server)
(require 'anvil-state)

;; `anvil-version' is defined in anvil.el; we only read it in the
;; User-Agent default so soft-require it to avoid a load-order dep.
(defvar anvil-version)

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

(defvar anvil-http--metrics
  (list :requests 0 :cache-fresh 0 :cache-revalidated 0
        :network-200 0 :errors 0 :log nil)
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

(defun anvil-http--cache-put (norm-url entry)
  "Store ENTRY under NORM-URL in `anvil-state'."
  (condition-case _err
      (anvil-state-set norm-url entry :ns anvil-http--state-ns)
    (error nil)))

(defun anvil-http--cache-delete (norm-url)
  "Drop NORM-URL from the cache.  Returns t when a row was removed."
  (condition-case _err
      (anvil-state-delete norm-url :ns anvil-http--state-ns)
    (error nil)))

(defun anvil-http--cache-clear-all ()
  "Wipe every entry in the http namespace.  Returns deleted row count."
  (condition-case _err
      (anvil-state-delete-ns anvil-http--state-ns)
    (error 0)))

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

(defun anvil-http--request (method url extra-headers timeout)
  "Issue one METHOD request to URL with EXTRA-HEADERS and TIMEOUT (seconds).
Returns a response plist (:status :headers :body :final-url) or
signals `anvil-server-tool-error' on transport failure / timeout."
  (let* ((url-request-method method)
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

(defun anvil-http--backoff-ms (attempt response)
  "Return the wait time (ms) before retry ATTEMPT.
RESPONSE is the last response plist; honours `Retry-After' for 429."
  (let* ((status (plist-get response :status))
         (headers (plist-get response :headers))
         (retry-after (and (= (or status 0) 429)
                           (plist-get headers :retry-after)))
         (base (* anvil-http-retry-base-ms (expt 2 attempt))))
    (cond
     ((and retry-after (string-match "\\`[0-9]+\\'" retry-after))
      (* 1000 (string-to-number retry-after)))
     (t base))))

(defun anvil-http--request-with-retry (method url extra-headers timeout)
  "Run `anvil-http--request' with retry on 5xx / 408 / 429."
  (let ((attempt 0)
        (max-total (1+ (max 0 anvil-http-retry-max)))
        (result nil))
    (catch 'done
      (while (< attempt max-total)
        (setq result (anvil-http--request method url extra-headers timeout))
        (if (and (anvil-http--retryable-p (plist-get result :status))
                 (< attempt (1- max-total)))
            (let ((ms (anvil-http--backoff-ms attempt result)))
              (sleep-for (/ ms 1000.0))
              (setq attempt (1+ attempt)))
          (throw 'done result))))
    result))

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

;;;; --- public Elisp API ---------------------------------------------------

;;;###autoload
(cl-defun anvil-http-get (url &key headers accept timeout-sec
                              no-cache cache-ttl-sec if-newer-than)
  "GET URL and return a response plist.

Keyword args:
  :headers         alist of (STRING . STRING) extra request headers
  :accept          MIME string added as Accept header (short-hand)
  :timeout-sec     override `anvil-http-timeout-sec'
  :no-cache        skip cache reads AND writes
  :cache-ttl-sec   override `anvil-http-cache-ttl-sec'
  :if-newer-than   unix epoch int; sends If-Modified-Since

Returns (:status :headers :body :from-cache :cached-at :final-url
:elapsed-ms)."
  (anvil-http--check-url url)
  (anvil-state-enable)
  (anvil-http--metrics-bump :requests)
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
                  (list (format "anvil-http: HTTP %d for %s" status url)))))))))

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
(defun anvil-http-cache-clear (&optional url)
  "Remove cache entries.  With URL, drop just that key; nil flushes http ns."
  (anvil-state-enable)
  (if (and (stringp url) (not (string-empty-p url)))
      (if (anvil-http--cache-delete (anvil-http--normalize-url url)) 1 0)
    (or (anvil-http--cache-clear-all) 0)))

;;;; --- MCP tool handlers --------------------------------------------------

(defun anvil-http--tool-fetch (url &optional if_newer_than accept
                                   timeout_sec no_cache)
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

Returns (:status :headers :body :from-cache :cached-at :final-url
:elapsed-ms).  Same-URL calls within `anvil-http-cache-ttl-sec'
seconds are served from the cache without any network round-trip.
Otherwise conditional GET (If-None-Match / If-Modified-Since) is
performed and a 304 revalidates the cached body with zero body
transfer."
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
         (no-c (and no_cache (not (equal no_cache "")))))
     (anvil-http-get url
                     :accept accept*
                     :timeout-sec timeout
                     :no-cache no-c
                     :if-newer-than if-newer))))

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
   :server-id anvil-http--server-id
   :description
   "GET an http/https URL and return status, headers and body with
cache awareness.  Within `anvil-http-cache-ttl-sec' the cached entry
is served without any network call; otherwise a conditional GET
revalidates via If-None-Match / If-Modified-Since and a 304 is
served from cache with zero body transfer.  Retries 5xx / 408 / 429
with exponential backoff (honours Retry-After on 429)."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-http--tool-head
   :id "http-head"
   :server-id anvil-http--server-id
   :description
   "HEAD an http/https URL and return its status + response headers.
Cheap liveness / metadata probe; responses are never cached."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-http--tool-cache-clear
   :id "http-cache-clear"
   :server-id anvil-http--server-id
   :description
   "Drop cached entries for the http namespace.  With a URL argument
removes just that entry; without, flushes every cached response."))

(defun anvil-http--unregister-tools ()
  "Remove every http-* MCP tool from the shared server."
  (dolist (id '("http-fetch" "http-head" "http-cache-clear"))
    (anvil-server-unregister-tool id anvil-http--server-id)))

;;;###autoload
(defun anvil-http-enable ()
  "Register http-* MCP tools and open the anvil-state backing store."
  (interactive)
  (anvil-state-enable)
  (anvil-http--register-tools))

(defun anvil-http-disable ()
  "Unregister http-* MCP tools."
  (interactive)
  (anvil-http--unregister-tools))

(provide 'anvil-http)
;;; anvil-http.el ends here
