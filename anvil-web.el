;;; anvil-web.el --- Web API wrappers (X/Twitter, Reddit) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Anvil contributors

;; Author: Anvil contributors
;; Keywords: http, twitter, reddit, tools
;; Package-Requires: ((emacs "28.1") (anvil-http "0.1") (anvil-server "0.1"))
;; Version: 0.1.0

;;; Commentary:

;; Small, focused wrappers around a handful of public web endpoints that
;; `anvil-http' alone does not normalise.  The goal is not "a generic web
;; client" - it is to remove a class of recurring tool-use patterns that
;; otherwise show up as curl + jq + parsing loops in agent transcripts.
;;
;; Currently provided:
;;
;;   * `anvil-web-tweet-fetch' - fetch a single tweet by numeric ID via the
;;     public Twitter syndication endpoint (no auth, no token), and return a
;;     normalised plist.  Works for tweets that X WebFetch can't reach.
;;
;;   * `anvil-web-reddit-thread' - fetch a Reddit thread (title + OP body +
;;     top-N comments) from either a canonical `/comments/<id>/...' URL or a
;;     short `/r/<sub>/s/<hash>' share URL, which requires a redirect-resolve
;;     step before appending `.json'.
;;
;; Both tools go through `anvil-http-get' so caching, retry/backoff and the
;; scheme allow-list come for free.  A browser-ish User-Agent is attached by
;; default since Reddit and a few others return 403 to bare
;; `Mozilla-only' / libcurl-style UAs.

;;; Code:

(require 'anvil-http)
(require 'anvil-server)
(require 'cl-lib)
(require 'json)

(defgroup anvil-web nil
  "Normalised wrappers around a few public web endpoints."
  :group 'anvil
  :prefix "anvil-web-")

(defcustom anvil-web-default-user-agent
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"
  "Default User-Agent for `anvil-web' requests.
Reddit and various CDN-fronted endpoints respond 403 to libcurl or
empty UAs, so we present as a generic desktop browser by default.
Callers can override with the :user-agent argument."
  :type 'string
  :group 'anvil-web)

(defcustom anvil-web-default-timeout-sec 15
  "Default per-request timeout (seconds) for `anvil-web' tools."
  :type 'integer
  :group 'anvil-web)

(defcustom anvil-web-reddit-default-top-n 10
  "Default number of top-level comments returned by `anvil-web-reddit-thread'."
  :type 'integer
  :group 'anvil-web)

(defconst anvil-web--server-id "emacs-eval"
  "MCP server id under which `anvil-web' tools register.
Matches the id used by `anvil-http', `anvil-file', etc. so that all
anvil MCP tools end up on the same transport.")

;;;; --- internal HTTP + JSON helpers --------------------------------------

(defun anvil-web--merge-ua (headers user-agent)
  "Return HEADERS alist with a User-Agent entry guaranteed.
USER-AGENT (nil → default) wins over any UA already in HEADERS."
  (let* ((ua (or user-agent anvil-web-default-user-agent))
         (stripped (cl-remove-if
                    (lambda (h)
                      (and (consp h) (stringp (car h))
                           (string-equal-ignore-case (car h) "User-Agent")))
                    headers)))
    (cons (cons "User-Agent" ua) stripped)))

(cl-defun anvil-web--http-json (url &key headers user-agent
                                    (timeout-sec anvil-web-default-timeout-sec)
                                    (no-cache t)
                                    accept)
  "GET URL through `anvil-http-get' and return its body parsed as JSON.
Signals a user-error for non-2xx status or invalid JSON payloads.
Returns whatever `json-parse-string' produces (plists + vectors)."
  (let* ((merged (anvil-web--merge-ua headers user-agent))
         (resp (anvil-http-get url
                               :headers merged
                               :accept (or accept "application/json")
                               :timeout-sec timeout-sec
                               :no-cache no-cache))
         (status (plist-get resp :status))
         (body (plist-get resp :body)))
    (unless (and (integerp status) (>= status 200) (< status 300))
      (user-error "anvil-web: HTTP %s for %s" status url))
    (unless (and (stringp body) (not (string-empty-p body)))
      (user-error "anvil-web: empty body from %s" url))
    (condition-case err
        (json-parse-string body
                           :object-type 'plist
                           :null-object nil
                           :false-object nil)
      (error
       (user-error "anvil-web: JSON parse failed for %s: %s"
                   url (error-message-string err))))))

(defun anvil-web--resolve-redirect (url &optional user-agent)
  "GET URL via `anvil-http-get' and return `:final-url' after redirects.
Used for Reddit `/s/<hash>' share links that only resolve via redirect.
Returns URL unchanged if the server didn't redirect."
  (let* ((resp (anvil-http-get url
                               :headers (anvil-web--merge-ua nil user-agent)
                               :accept "text/html"
                               :timeout-sec anvil-web-default-timeout-sec
                               :no-cache t))
         (final (plist-get resp :final-url)))
    (or (and (stringp final) (not (string-empty-p final)) final)
        url)))

;;;; --- tweet -------------------------------------------------------------

(defconst anvil-web--tweet-syndication-url
  "https://cdn.syndication.twimg.com/tweet-result?id=%s&token=x"
  "Public syndication endpoint for tweet-by-id retrieval.
Requires no OAuth; the `token' query parameter is a deliberate
placeholder the endpoint accepts.  Returns a single-tweet JSON
document (no thread traversal).")

(defun anvil-web--tweet-normalise (json tweet-id)
  "Flatten the syndication response JSON into a stable plist.
Fields that the caller is likely to want are exposed at the top level;
the raw response is returned under `:raw' for anything else."
  (let* ((user (plist-get json :user))
         (quoted (plist-get json :quoted_tweet))
         (quoted-user (and quoted (plist-get quoted :user)))
         (article (plist-get json :article))
         (entities (plist-get json :entities))
         (url-entries (and entities (plist-get entities :urls)))
         (screen (and user (plist-get user :screen_name))))
    (list
     :id tweet-id
     :url (format "https://x.com/%s/status/%s" (or screen "i") tweet-id)
     :text (plist-get json :text)
     :lang (plist-get json :lang)
     :created-at (plist-get json :created_at)
     :favorite-count (plist-get json :favorite_count)
     :conversation-count (plist-get json :conversation_count)
     :possibly-sensitive (plist-get json :possibly_sensitive)
     :user-name (and user (plist-get user :name))
     :screen-name screen
     :is-blue-verified (and user (plist-get user :is_blue_verified))
     :quoted-screen-name (and quoted-user (plist-get quoted-user :screen_name))
     :quoted-text (and quoted (plist-get quoted :text))
     :article-id (and article (plist-get article :rest_id))
     :article-title (and article (plist-get article :title))
     :article-preview-text (and article (plist-get article :preview_text))
     :expanded-urls
     (cl-loop for u across (or url-entries [])
              for expanded = (plist-get u :expanded_url)
              when expanded collect expanded)
     :raw json)))

(defun anvil-web-tweet-fetch (tweet_id)
  "Fetch a tweet by TWEET_ID and return a normalised plist.
Uses the public syndication API: no authentication, no rate-limit
headers, no thread traversal.  The response is a single tweet - for
thread continuations, call the tool once per reply id.

MCP Parameters:
  tweet_id - Numeric Twitter/X status id as a string of digits,
             e.g. \"2046898117241635240\".

Returns a plist with at least:
  :id :url :text :lang :created-at :user-name :screen-name
  :favorite-count :quoted-text :quoted-screen-name :article-title
  :article-preview-text :expanded-urls :raw

The tool bypasses the `anvil-http' cache (syndication is cheap and
near-realtime accuracy is preferred for social-media content)."
  (anvil-server-with-error-handling
   (let ((id (cond ((and (stringp tweet_id)
                         (string-match-p "\\`[0-9]+\\'" tweet_id))
                    tweet_id)
                   ((integerp tweet_id) (number-to-string tweet_id))
                   (t (user-error
                       "anvil-web-tweet-fetch: tweet_id must be digit string, got %S"
                       tweet_id)))))
     (let* ((url (format anvil-web--tweet-syndication-url id))
            (json (anvil-web--http-json url)))
       (anvil-web--tweet-normalise json id)))))

;;;; --- reddit ------------------------------------------------------------

(defun anvil-web--reddit-canonicalize (url)
  "Return a canonical `/r/<sub>/comments/<id>/slug/' URL for URL.
Resolves the `/r/<sub>/s/<hash>' share-link form via one redirect
fetch.  If URL is already canonical (or any other reddit URL with a
`/comments/' segment), it is returned stripped of query parameters."
  (unless (stringp url)
    (user-error "anvil-web-reddit: url must be a string, got %S" url))
  (let* ((base url))
    (when (string-match "\\b\\(reddit\\.com\\)/r/[^/]+/s/" url)
      (setq base (anvil-web--resolve-redirect url)))
    (let* ((no-query (replace-regexp-in-string "[?#].*\\'" "" base)))
      (unless (string-match-p "/comments/[^/]+/" no-query)
        (user-error
         "anvil-web-reddit: could not resolve canonical comments URL from %s"
         url))
      no-query)))

(defun anvil-web--reddit-normalise-comments (com-children top-n)
  "Turn Reddit's `t1' children vector into a list of plists, capped at TOP-N.
Comments with nil :body (deleted / mod-removed / MoreComments sentinels)
are skipped.  Scores appear as reported by the API (may be fuzzed)."
  (let (out)
    (cl-loop for c across (or com-children [])
             for d = (plist-get c :data)
             for body = (and d (plist-get d :body))
             when (and (stringp body) (not (string-empty-p body)))
             do (push (list :author (plist-get d :author)
                            :score (plist-get d :score)
                            :body body
                            :created-utc (plist-get d :created_utc))
                      out))
    (setq out (nreverse out))
    (if (and (integerp top-n) (>= top-n 0))
        (seq-take out top-n)
      out)))

(defun anvil-web-reddit-thread (url &optional top_n)
  "Fetch a Reddit thread (OP + top comments) and return a normalised plist.
URL may be either a canonical `/r/<sub>/comments/<id>/...' permalink
or a `/r/<sub>/s/<hash>' share-link (the latter is resolved via one
extra redirect fetch).

MCP Parameters:
  url   - Reddit thread URL (canonical or `/s/<hash>' share link).
  top_n - Optional maximum number of top-level comments to include.
          Integer or digit-string; default `anvil-web-reddit-default-top-n'.

Returns a plist:
  :title :author :subreddit :selftext :url :permalink :score
  :created-utc :num-comments :comments

where :comments is a list of (:author :score :body :created-utc)
plists for up to top_n entries."
  (anvil-server-with-error-handling
   (let* ((n (cond ((null top_n) anvil-web-reddit-default-top-n)
                   ((integerp top_n) top_n)
                   ((and (stringp top_n)
                         (string-match-p "\\`[0-9]+\\'" top_n))
                    (string-to-number top_n))
                   (t anvil-web-reddit-default-top-n)))
          (canonical (anvil-web--reddit-canonicalize url))
          (json-url (concat (string-trim-right canonical "/") ".json"))
          (arr (anvil-web--http-json json-url)))
     (unless (and (vectorp arr) (>= (length arr) 2))
       (user-error "anvil-web-reddit: unexpected response shape for %s"
                   json-url))
     (let* ((post-listing (aref arr 0))
            (com-listing (aref arr 1))
            (post-children (plist-get (plist-get post-listing :data) :children))
            (com-children (plist-get (plist-get com-listing :data) :children)))
       (unless (and (vectorp post-children) (> (length post-children) 0))
         (user-error "anvil-web-reddit: no post found in %s" json-url))
       (let ((post-data (plist-get (aref post-children 0) :data)))
         (list
          :title (plist-get post-data :title)
          :author (plist-get post-data :author)
          :subreddit (plist-get post-data :subreddit)
          :selftext (plist-get post-data :selftext)
          :url (plist-get post-data :url)
          :permalink (plist-get post-data :permalink)
          :score (plist-get post-data :score)
          :created-utc (plist-get post-data :created_utc)
          :num-comments (plist-get post-data :num_comments)
          :comments (anvil-web--reddit-normalise-comments com-children n)))))))

;;;; --- module lifecycle --------------------------------------------------

(defun anvil-web--register-tools ()
  "Register web-* MCP tools under `anvil-web--server-id'."
  (anvil-server-register-tool
   #'anvil-web-tweet-fetch
   :id "web-tweet-fetch"
   :intent '(web http twitter)
   :layer 'io
   :server-id anvil-web--server-id
   :description
   "Fetch a single tweet by numeric ID through the public X/Twitter
syndication endpoint.  No auth required, no thread traversal (single
tweet only).  Returns a normalised plist with body text, author,
created-at, quoted-tweet text and X-native article preview fields when
the tweet wraps an x.com/i/article/<id>.  Bypasses the HTTP cache."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-web-reddit-thread
   :id "web-reddit-thread"
   :intent '(web http reddit)
   :layer 'io
   :server-id anvil-web--server-id
   :description
   "Fetch a Reddit thread (title + OP selftext + top-N comments) from
either a canonical `/r/<sub>/comments/<id>/slug/' URL or a
`/r/<sub>/s/<hash>' share link (redirects are resolved automatically).
Top-level comments only; `MoreComments' sentinels and removed bodies
are skipped.  Bypasses the HTTP cache."
   :read-only t))

(defun anvil-web--unregister-tools ()
  "Remove every web-* MCP tool from the shared server."
  (dolist (id '("web-tweet-fetch" "web-reddit-thread"))
    (anvil-server-unregister-tool id anvil-web--server-id)))

;;;###autoload
(defun anvil-web-enable ()
  "Register the `anvil-web' MCP tools."
  (interactive)
  (anvil-web--register-tools))

;;;###autoload
(defun anvil-web-disable ()
  "Unregister the `anvil-web' MCP tools."
  (interactive)
  (anvil-web--unregister-tools))

(provide 'anvil-web)

;;; anvil-web.el ends here
