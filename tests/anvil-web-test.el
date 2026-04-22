;;; anvil-web-test.el --- Tests for anvil-web -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for `anvil-web'.  The transport layer (`anvil-http--request')
;; is stubbed with a response queue; no live network traffic happens unless
;; the optional smoke test at the bottom is enabled with ANVIL_ALLOW_LIVE=1.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'anvil-http)
(require 'anvil-state)
(require 'anvil-web)

;;;; --- fixtures -----------------------------------------------------------

(defvar anvil-web-test--calls nil
  "List of `(:method :url :headers :timeout)' recorded by the stub.")

(defvar anvil-web-test--responses nil
  "Queue of fake response plists to hand back.")

(defun anvil-web-test--pop-response ()
  (or (pop anvil-web-test--responses)
      (error "anvil-web-test: response queue exhausted")))

(defun anvil-web-test--response (status &optional headers body final-url)
  (list :status status
        :headers headers
        :body (or body "")
        :final-url (or final-url "https://example.com/")))

(defmacro anvil-web-test--with-stub (responses &rest body)
  "Run BODY with `anvil-http--request' returning successive RESPONSES."
  (declare (indent 1))
  `(let ((anvil-web-test--calls nil)
         (anvil-web-test--responses (copy-sequence ,responses))
         (anvil-state-db-path (make-temp-file "anvil-web-st-" nil ".db"))
         (anvil-state--db nil)
         (anvil-http--metrics
          (list :requests 0 :cache-fresh 0 :cache-revalidated 0
                :network-200 0 :errors 0 :log nil)))
     (unwind-protect
         (progn
           (anvil-state-enable)
           (cl-letf (((symbol-function 'anvil-http--request)
                      (lambda (method url headers timeout)
                        (push (list :method method :url url
                                    :headers headers :timeout timeout)
                              anvil-web-test--calls)
                        (anvil-web-test--pop-response)))
                     ((symbol-function 'sleep-for)
                      (lambda (&rest _) nil)))
             ,@body))
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path)))))

(defun anvil-web-test--json (obj)
  "Serialize OBJ (plist or vector) to JSON text.
`t' → true, `:false' → false, `:null' → null."
  (json-serialize obj :null-object :null :false-object :false))

(defun anvil-web-test--tb (b)
  "Map a test bool B (t or nil) to a JSON-serializable token."
  (if b t :false))

(defun anvil-web-test--tweet-fixture (&optional overrides)
  "Return a minimal syndication tweet response JSON object (plist).
OVERRIDES, if non-nil, is merged onto the defaults."
  (let ((base
         (list :__typename "Tweet"
               :id_str "2046898117241635240"
               :text "hello world"
               :lang "en"
               :created_at "2026-04-22T10:25:15.000Z"
               :favorite_count 42
               :conversation_count 3
               :possibly_sensitive :false
               :user (list :name "Hasan Toor"
                           :screen_name "hasantoxr"
                           :is_blue_verified t)
               :entities (list :urls
                               (vector
                                (list :expanded_url
                                      "https://example.com/article"
                                      :display_url "example.com/article"))))))
    (if overrides
        (let ((merged (copy-sequence base)))
          (cl-loop for (k v) on overrides by #'cddr
                   do (setq merged (plist-put merged k v)))
          merged)
      base)))

(defun anvil-web-test--reddit-post-fixture ()
  (list :kind "t3"
        :data (list :title "Emacs is My Web Browser"
                    :author "joshuablais"
                    :subreddit "emacs"
                    :selftext "Seed body."
                    :url "https://joshblais.com/blog/emacs-as-my-browser/"
                    :permalink "/r/emacs/comments/1sry0vi/emacs_is_my_web_browser/"
                    :score 77
                    :num_comments 10
                    :created_utc 1776799646.0)))

(defun anvil-web-test--reddit-comment (author score body)
  (list :kind "t1"
        :data (list :author author
                    :score score
                    :body body
                    :created_utc 1776800000.0)))

(defun anvil-web-test--reddit-fixture (&optional comments)
  "Return an ARRAY (vector) of two Listings (post + comments).
COMMENTS is a list of plists from `anvil-web-test--reddit-comment';
if nil a small default list is used."
  (let ((com-list
         (or comments
             (list (anvil-web-test--reddit-comment "alice" 15 "first!")
                   (anvil-web-test--reddit-comment "bob" 8 "second.")
                   ;; MoreComments-like sentinel with nil body
                   (list :kind "more" :data (list :author nil :score nil
                                                  :body nil))))))
    (vector
     (list :kind "Listing"
           :data (list :children
                       (vector (anvil-web-test--reddit-post-fixture))))
     (list :kind "Listing"
           :data (list :children (apply #'vector com-list))))))

;;;; --- merge-ua ----------------------------------------------------------

(ert-deftest anvil-web-test-merge-ua-adds-default ()
  (let ((merged (anvil-web--merge-ua nil nil)))
    (should (equal (cdr (assoc "User-Agent" merged))
                   anvil-web-default-user-agent))))

(ert-deftest anvil-web-test-merge-ua-overrides-caller ()
  "An explicit USER-AGENT wins over any UA already in HEADERS."
  (let* ((headers '(("User-Agent" . "libcurl/7.88")
                    ("X-Test" . "1")))
         (merged (anvil-web--merge-ua headers "custom-ua/1.0")))
    (should (equal "custom-ua/1.0" (cdr (assoc "User-Agent" merged))))
    (should (equal "1" (cdr (assoc "X-Test" merged))))
    ;; The libcurl UA must have been removed (case-insensitive).
    (should (= 1 (cl-count-if
                  (lambda (h)
                    (and (stringp (car h))
                         (string-equal-ignore-case (car h) "User-Agent")))
                  merged)))))

(ert-deftest anvil-web-test-merge-ua-strips-case-insensitive ()
  "UA with lowercased key is stripped before adding the default."
  (let* ((headers '(("user-agent" . "bad/1"))) ; lowercase key
         (merged (anvil-web--merge-ua headers nil)))
    (should (equal anvil-web-default-user-agent
                   (cdr (assoc "User-Agent" merged))))
    (should (= 1 (length merged)))))

;;;; --- http-json --------------------------------------------------------

(ert-deftest anvil-web-test-http-json-200 ()
  (anvil-web-test--with-stub
      (list (anvil-web-test--response
             200 nil
             (anvil-web-test--json (list :ok t :n 1))
             "https://api.example.com/"))
    (let ((obj (anvil-web--http-json "https://api.example.com/")))
      (should (eq t (plist-get obj :ok)))
      (should (= 1 (plist-get obj :n))))))

(ert-deftest anvil-web-test-http-json-non2xx-errors ()
  (anvil-web-test--with-stub
      (list (anvil-web-test--response 404 nil "not found"
                                      "https://api.example.com/"))
    (should-error (anvil-web--http-json "https://api.example.com/")
                  :type 'user-error)))

(ert-deftest anvil-web-test-http-json-empty-body-errors ()
  (anvil-web-test--with-stub
      (list (anvil-web-test--response 200 nil ""
                                      "https://api.example.com/"))
    (should-error (anvil-web--http-json "https://api.example.com/")
                  :type 'user-error)))

(ert-deftest anvil-web-test-http-json-invalid-json-errors ()
  (anvil-web-test--with-stub
      (list (anvil-web-test--response 200 nil "not-json-at-all"
                                      "https://api.example.com/"))
    (should-error (anvil-web--http-json "https://api.example.com/")
                  :type 'user-error)))

(ert-deftest anvil-web-test-http-json-sends-ua ()
  (anvil-web-test--with-stub
      (list (anvil-web-test--response 200 nil
                                      (anvil-web-test--json (list :ok t))
                                      "https://api.example.com/"))
    (anvil-web--http-json "https://api.example.com/")
    (let* ((call (car anvil-web-test--calls))
           (headers (plist-get call :headers)))
      (should (assoc "User-Agent" headers))
      (should (equal anvil-web-default-user-agent
                     (cdr (assoc "User-Agent" headers)))))))

;;;; --- tweet-fetch ------------------------------------------------------

(ert-deftest anvil-web-test-tweet-fetch-happy-path ()
  (anvil-web-test--with-stub
      (list (anvil-web-test--response
             200 nil
             (anvil-web-test--json (anvil-web-test--tweet-fixture))
             "https://cdn.syndication.twimg.com/"))
    (let ((out (anvil-web-tweet-fetch "2046898117241635240")))
      (should (equal "2046898117241635240" (plist-get out :id)))
      (should (equal "hello world" (plist-get out :text)))
      (should (equal "hasantoxr" (plist-get out :screen-name)))
      (should (equal "Hasan Toor" (plist-get out :user-name)))
      (should (equal "en" (plist-get out :lang)))
      (should (equal 42 (plist-get out :favorite-count)))
      (should (equal "https://x.com/hasantoxr/status/2046898117241635240"
                     (plist-get out :url)))
      (should (equal '("https://example.com/article")
                     (plist-get out :expanded-urls)))
      (should-not (plist-get out :quoted-text))
      (should-not (plist-get out :article-title)))))

(ert-deftest anvil-web-test-tweet-fetch-accepts-integer-id ()
  (anvil-web-test--with-stub
      (list (anvil-web-test--response
             200 nil
             (anvil-web-test--json (anvil-web-test--tweet-fixture))
             "https://cdn.syndication.twimg.com/"))
    (let ((out (anvil-web-tweet-fetch 2046898117241635240)))
      (should (equal "2046898117241635240" (plist-get out :id))))))

(ert-deftest anvil-web-test-tweet-fetch-rejects-bad-id ()
  (should-error (anvil-web-tweet-fetch "not-a-number") :type 'user-error)
  (should-error (anvil-web-tweet-fetch "") :type 'user-error)
  (should-error (anvil-web-tweet-fetch nil) :type 'user-error))

(ert-deftest anvil-web-test-tweet-fetch-quoted-and-article ()
  (anvil-web-test--with-stub
      (list (anvil-web-test--response
             200 nil
             (anvil-web-test--json
              (anvil-web-test--tweet-fixture
               (list :quoted_tweet
                     (list :text "original thought"
                           :user (list :screen_name "orig_user"))
                     :article
                     (list :rest_id "99999"
                           :title "Top 30 MCP"
                           :preview_text "preview body here"))))
             "https://cdn.syndication.twimg.com/"))
    (let ((out (anvil-web-tweet-fetch "2046898117241635240")))
      (should (equal "original thought" (plist-get out :quoted-text)))
      (should (equal "orig_user" (plist-get out :quoted-screen-name)))
      (should (equal "99999" (plist-get out :article-id)))
      (should (equal "Top 30 MCP" (plist-get out :article-title)))
      (should (equal "preview body here"
                     (plist-get out :article-preview-text))))))

(ert-deftest anvil-web-test-tweet-fetch-hits-syndication-url ()
  (anvil-web-test--with-stub
      (list (anvil-web-test--response
             200 nil
             (anvil-web-test--json (anvil-web-test--tweet-fixture))
             "https://cdn.syndication.twimg.com/"))
    (anvil-web-tweet-fetch "12345")
    (let* ((call (car anvil-web-test--calls))
           (url (plist-get call :url)))
      (should (string-match-p
               "\\`https://cdn\\.syndication\\.twimg\\.com/tweet-result\\?id=12345"
               url)))))

;;;; --- reddit thread ----------------------------------------------------

(ert-deftest anvil-web-test-reddit-thread-canonical ()
  (anvil-web-test--with-stub
      (list (anvil-web-test--response
             200 nil
             (anvil-web-test--json (anvil-web-test--reddit-fixture))
             "https://www.reddit.com/"))
    (let ((out (anvil-web-reddit-thread
                "https://www.reddit.com/r/emacs/comments/1sry0vi/emacs_is_my_web_browser/")))
      (should (equal "Emacs is My Web Browser" (plist-get out :title)))
      (should (equal "joshuablais" (plist-get out :author)))
      (should (equal "emacs" (plist-get out :subreddit)))
      (should (equal "https://joshblais.com/blog/emacs-as-my-browser/"
                     (plist-get out :url)))
      ;; default top-n = 10, deleted comment skipped
      (let ((comments (plist-get out :comments)))
        (should (= 2 (length comments)))
        (should (equal "alice" (plist-get (nth 0 comments) :author)))
        (should (equal "first!" (plist-get (nth 0 comments) :body)))))))

(ert-deftest anvil-web-test-reddit-thread-shortlink-resolves-redirect ()
  (anvil-web-test--with-stub
      (list
       ;; First call: redirect resolve (HEAD-ish GET)
       (anvil-web-test--response
        200 nil "<html>ignored</html>"
        "https://www.reddit.com/r/emacs/comments/1sry0vi/emacs_is_my_web_browser/?share_id=abc")
       ;; Second call: the .json fetch
       (anvil-web-test--response
        200 nil
        (anvil-web-test--json (anvil-web-test--reddit-fixture))
        "https://www.reddit.com/"))
    (let ((out (anvil-web-reddit-thread
                "https://www.reddit.com/r/emacs/s/lVNi77sfno")))
      (should (equal "Emacs is My Web Browser" (plist-get out :title))))
    ;; Verify the JSON call URL ends with `.json' and has no query string.
    (let* ((json-call (car anvil-web-test--calls))
           (url (plist-get json-call :url)))
      (should (string-suffix-p ".json" url))
      (should-not (string-match-p "\\?" url)))))

(ert-deftest anvil-web-test-reddit-thread-top-n-limits ()
  (anvil-web-test--with-stub
      (list (anvil-web-test--response
             200 nil
             (anvil-web-test--json
              (anvil-web-test--reddit-fixture
               (list (anvil-web-test--reddit-comment "a" 5 "one")
                     (anvil-web-test--reddit-comment "b" 4 "two")
                     (anvil-web-test--reddit-comment "c" 3 "three"))))
             "https://www.reddit.com/"))
    (let ((out (anvil-web-reddit-thread
                "https://www.reddit.com/r/emacs/comments/abc/x/"
                2)))
      (should (= 2 (length (plist-get out :comments)))))))

(ert-deftest anvil-web-test-reddit-thread-top-n-string ()
  "top_n accepts digit-strings (MCP transport coerces int→str)."
  (anvil-web-test--with-stub
      (list (anvil-web-test--response
             200 nil
             (anvil-web-test--json
              (anvil-web-test--reddit-fixture
               (list (anvil-web-test--reddit-comment "a" 5 "one")
                     (anvil-web-test--reddit-comment "b" 4 "two")
                     (anvil-web-test--reddit-comment "c" 3 "three"))))
             "https://www.reddit.com/"))
    (let ((out (anvil-web-reddit-thread
                "https://www.reddit.com/r/emacs/comments/abc/x/" "1")))
      (should (= 1 (length (plist-get out :comments)))))))

(ert-deftest anvil-web-test-reddit-thread-rejects-non-comment-url ()
  (should-error
   (anvil-web-reddit-thread "https://www.reddit.com/r/emacs/")
   :type 'user-error))

(ert-deftest anvil-web-test-reddit-thread-strips-query-string ()
  (anvil-web-test--with-stub
      (list (anvil-web-test--response
             200 nil
             (anvil-web-test--json (anvil-web-test--reddit-fixture))
             "https://www.reddit.com/"))
    (anvil-web-reddit-thread
     "https://www.reddit.com/r/emacs/comments/abc/x/?utm_source=share")
    (let* ((call (car anvil-web-test--calls))
           (url (plist-get call :url)))
      (should (string-suffix-p "/comments/abc/x.json" url))
      (should-not (string-match-p "utm_source" url)))))

(ert-deftest anvil-web-test-reddit-thread-bad-response-shape ()
  (anvil-web-test--with-stub
      (list (anvil-web-test--response
             200 nil
             ;; Only one listing instead of [post, comments]
             (anvil-web-test--json (vector (list :kind "Listing")))
             "https://www.reddit.com/"))
    (should-error
     (anvil-web-reddit-thread
      "https://www.reddit.com/r/emacs/comments/abc/x/")
     :type 'user-error)))

;;;; --- module lifecycle -------------------------------------------------

(ert-deftest anvil-web-test-enable-registers-tools ()
  "enable adds both tools, disable removes them."
  (anvil-web-enable)
  (unwind-protect
      (let ((tools (anvil-server--get-server-tools anvil-web--server-id)))
        (should (gethash "web-tweet-fetch" tools))
        (should (gethash "web-reddit-thread" tools)))
    (anvil-web-disable))
  (let ((tools (anvil-server--get-server-tools anvil-web--server-id)))
    (should-not (gethash "web-tweet-fetch" tools))
    (should-not (gethash "web-reddit-thread" tools))))

;;;; --- live smoke (opt-in via ANVIL_ALLOW_LIVE=1) -----------------------

(ert-deftest anvil-web-test-live-tweet-fetch ()
  "Exercises the real syndication endpoint.  Skipped unless
ANVIL_ALLOW_LIVE=1 is set."
  (skip-unless (equal "1" (getenv "ANVIL_ALLOW_LIVE")))
  ;; Jack Dorsey's \"just setting up my twttr\" - stable public tweet.
  (let ((out (anvil-web-tweet-fetch "20")))
    (should (stringp (plist-get out :text)))
    (should (equal "jack" (plist-get out :screen-name)))))

(provide 'anvil-web-test)

;;; anvil-web-test.el ends here
