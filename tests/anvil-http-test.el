;;; anvil-http-test.el --- Tests for anvil-http -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for `anvil-http' that stub `anvil-http--request' so no
;; real network traffic happens.  One live smoke test at the bottom
;; fetches https://example.com when ANVIL_ALLOW_LIVE=1 is set.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-http)
(require 'anvil-state)
(require 'anvil-offload)

;;;; --- fixtures -----------------------------------------------------------

(defvar anvil-http-test--calls nil
  "List of `(:method :url :headers :timeout)' recorded by the stub,
newest first.")

(defvar anvil-http-test--responses nil
  "Queue of fake response plists to hand back to successive calls.")

(defun anvil-http-test--pop-response ()
  (or (pop anvil-http-test--responses)
      (error "anvil-http-test: response queue exhausted")))

(defmacro anvil-http-test--with-stub (responses &rest body)
  "Run BODY with `anvil-http--request' returning successive RESPONSES.
Provides a fresh `anvil-state' DB and zeroed metrics.  Binds
`anvil-http-respect-robots-txt' to nil so unrelated tests do not
pull on a robots.txt response the stub has not queued."
  (declare (indent 1))
  `(let ((anvil-http-test--calls nil)
         (anvil-http-test--responses (copy-sequence ,responses))
         (anvil-state-db-path (make-temp-file "anvil-http-st-" nil ".db"))
         (anvil-state--db nil)
         (anvil-http-respect-robots-txt nil)
         (anvil-http--metrics
          (list :requests 0 :cache-fresh 0 :cache-revalidated 0
                :network-200 0 :errors 0 :log nil)))
     (unwind-protect
         (progn
           (anvil-state-enable)
           (cl-letf (((symbol-function 'anvil-http--request)
                      (lambda (method url headers timeout &optional body)
                        (push (list :method method :url url
                                    :headers headers :timeout timeout
                                    :body body)
                              anvil-http-test--calls)
                        (anvil-http-test--pop-response)))
                     ((symbol-function 'sleep-for)
                      (lambda (&rest _) nil)))
             ,@body))
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path)))))

(defun anvil-http-test--response (status &optional headers body final-url)
  (list :status status
        :headers headers
        :body (or body "")
        :final-url (or final-url "https://example.com/")))

;;;; --- URL validation -----------------------------------------------------

(ert-deftest anvil-http-test-check-url-rejects-empty ()
  "Empty or non-string URL → user-error."
  (should-error (anvil-http-get "") :type 'user-error)
  (should-error (anvil-http-get nil) :type 'user-error))

(ert-deftest anvil-http-test-check-url-rejects-bad-scheme ()
  "file:// / javascript: scheme are refused."
  (should-error (anvil-http-get "file:///etc/passwd") :type 'user-error)
  (should-error (anvil-http-get "javascript:alert(1)") :type 'user-error))

(ert-deftest anvil-http-test-normalize-url-lowercases-and-strips-fragment ()
  (should (equal "https://example.com/path?q=1"
                 (anvil-http--normalize-url
                  "HTTPS://Example.COM/path?q=1#section"))))

;;;; --- happy path GET -----------------------------------------------------

(ert-deftest anvil-http-test-get-returns-body-and-status ()
  "200 response surfaces status/headers/body and records a cache entry."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200
             (list :etag "\"abc\"" :content-type "text/html")
             "<h1>hi</h1>"
             "https://example.com/"))
    (let ((resp (anvil-http-get "https://example.com/" :cache-ttl-sec 0)))
      (should (= 200 (plist-get resp :status)))
      (should (string-match-p "<h1>hi</h1>" (plist-get resp :body)))
      (should-not (plist-get resp :from-cache))
      (let ((entry (anvil-http--cache-get
                    (anvil-http--normalize-url "https://example.com/"))))
        (should entry)
        (should (equal "\"abc\"" (plist-get entry :etag)))))))

;;;; --- cache fresh-serve --------------------------------------------------

(ert-deftest anvil-http-test-ttl-fresh-hit-avoids-network ()
  "Second GET within TTL serves from cache with zero requests."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200 (list :etag "\"x\"") "body-v1"
             "https://example.com/"))
    (anvil-http-get "https://example.com/" :cache-ttl-sec 600)
    (let ((before (length anvil-http-test--calls))
          (resp (anvil-http-get "https://example.com/" :cache-ttl-sec 600)))
      (should (= before (length anvil-http-test--calls)))
      (should (plist-get resp :from-cache))
      (should (string= "body-v1" (plist-get resp :body)))
      (should (= 1 (plist-get anvil-http--metrics :cache-fresh))))))

;;;; --- conditional revalidation / 304 -------------------------------------

(ert-deftest anvil-http-test-304-serves-cached-body ()
  "Expired TTL triggers conditional GET; 304 serves cached body."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200 (list :etag "\"v1\"" :last-modified "Wed, 01 Jan 2025 00:00:00 GMT")
             "first-body" "https://example.com/")
            (anvil-http-test--response 304 nil "" "https://example.com/"))
    ;; Seed the cache with TTL=0 so next request revalidates.
    (anvil-http-get "https://example.com/" :cache-ttl-sec 0)
    (let ((resp (anvil-http-get "https://example.com/" :cache-ttl-sec 0)))
      (should (= 200 (plist-get resp :status)))
      (should (plist-get resp :from-cache))
      (should (string= "first-body" (plist-get resp :body))))
    (let* ((revalidate-call (car anvil-http-test--calls))
           (headers (plist-get revalidate-call :headers)))
      (should (equal "\"v1\"" (cdr (assoc "If-None-Match" headers))))
      (should (assoc "If-Modified-Since" headers)))
    (should (= 1 (plist-get anvil-http--metrics :cache-revalidated)))))

;;;; --- if-newer-than forwarded as If-Modified-Since ------------------------

(ert-deftest anvil-http-test-if-newer-than-sends-ims ()
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 200 nil "body"))
    (anvil-http-get "https://example.com/" :if-newer-than 1700000000)
    (let ((headers (plist-get (car anvil-http-test--calls) :headers)))
      (should (assoc "If-Modified-Since" headers)))))

;;;; --- retry on 5xx --------------------------------------------------------

(ert-deftest anvil-http-test-retry-on-500-then-success ()
  "500 is retried; second attempt's 200 is returned."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 500 nil "")
            (anvil-http-test--response 200 nil "ok"))
    (let ((resp (anvil-http-get "https://example.com/" :cache-ttl-sec 0)))
      (should (= 200 (plist-get resp :status)))
      (should (= 2 (length anvil-http-test--calls))))))

(ert-deftest anvil-http-test-no-retry-on-404 ()
  "4xx (non-408/429) is NOT retried and surfaces as tool-error."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 404 nil ""))
    (should-error (anvil-http-get "https://example.com/404")
                  :type 'anvil-server-tool-error)
    (should (= 1 (length anvil-http-test--calls)))
    (should (= 1 (plist-get anvil-http--metrics :errors)))))

(ert-deftest anvil-http-test-429-retry-after-numeric ()
  "429 with numeric Retry-After is honoured before retry."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 429 (list :retry-after "2") "")
            (anvil-http-test--response 200 nil "ok"))
    (let ((resp (anvil-http-get "https://example.com/rate" :cache-ttl-sec 0)))
      (should (= 200 (plist-get resp :status)))
      (should (= 2 (length anvil-http-test--calls))))))

;;;; --- user-agent / Accept-Encoding injection -----------------------------

(ert-deftest anvil-http-test-user-agent-and-accept-encoding ()
  "Default UA + Accept-Encoding gzip are injected when absent.
The real `anvil-http--request' inserts these — verified by letting it run
in a fake url-retrieve."
  (cl-letf* ((captured nil)
             ((symbol-function 'url-retrieve-synchronously)
              (lambda (&rest _)
                (setq captured url-request-extra-headers)
                ;; Return a buffer shaped like a minimal url-http response.
                (with-current-buffer (generate-new-buffer " *anvil-http-fake*")
                  (insert "HTTP/1.1 200 OK\r\n"
                          "Content-Type: text/plain\r\n"
                          "\r\n"
                          "ok")
                  (goto-char (point-min))
                  (re-search-forward "\r\n\r\n")
                  (setq-local url-http-end-of-headers
                              (copy-marker (match-end 0)))
                  (setq-local url-http-target-url "https://example.com/")
                  (current-buffer)))))
    (let ((anvil-state-db-path (make-temp-file "anvil-http-ua-" nil ".db"))
          (anvil-state--db nil)
          (anvil-http--metrics
           (list :requests 0 :cache-fresh 0 :cache-revalidated 0
                 :network-200 0 :errors 0 :log nil)))
      (unwind-protect
          (progn
            (anvil-state-enable)
            (anvil-http-get "https://example.com/" :cache-ttl-sec 0)
            (should (assoc "User-Agent" captured))
            (should (string-match-p "^anvil\\.el/"
                                    (cdr (assoc "User-Agent" captured))))
            (should (equal "gzip" (cdr (assoc "Accept-Encoding" captured)))))
        (anvil-state-disable)
        (ignore-errors (delete-file anvil-state-db-path))))))

;;;; --- HEAD ---------------------------------------------------------------

(ert-deftest anvil-http-test-head-does-not-cache ()
  "HEAD bypasses the cache on both read and write."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200 (list :content-length "42") ""))
    (let ((resp (anvil-http-head "https://example.com/")))
      (should (= 200 (plist-get resp :status)))
      (should (equal "42" (plist-get (plist-get resp :headers) :content-length)))
      (should-not (anvil-http--cache-get
                   (anvil-http--normalize-url "https://example.com/"))))))

;;;; --- cache clear --------------------------------------------------------

(ert-deftest anvil-http-test-cache-clear-by-url ()
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 200 nil "body"))
    (anvil-http-get "https://example.com/page" :cache-ttl-sec 600)
    (should (anvil-http--cache-get
             (anvil-http--normalize-url "https://example.com/page")))
    (should (= 1 (anvil-http-cache-clear "https://example.com/page")))
    (should-not (anvil-http--cache-get
                 (anvil-http--normalize-url "https://example.com/page")))))

(ert-deftest anvil-http-test-cache-clear-all ()
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 200 nil "a")
            (anvil-http-test--response 200 nil "b"))
    (anvil-http-get "https://example.com/a" :cache-ttl-sec 600)
    (anvil-http-get "https://example.com/b" :cache-ttl-sec 600)
    (should (>= (anvil-http-cache-clear) 2))
    (should-not (anvil-http--cache-get
                 (anvil-http--normalize-url "https://example.com/a")))))

;;;; --- MCP tool argument coercion -----------------------------------------

(ert-deftest anvil-http-test-tool-fetch-coerces-string-timeout ()
  "MCP params arrive as strings; the tool coerces digit strings."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 200 nil "ok"))
    (anvil-http--tool-fetch "https://example.com/" nil nil "42" nil)
    (should (= 42 (plist-get (car anvil-http-test--calls) :timeout)))))

;;;; --- Phase 1b: selector / json-path extract -----------------------------

(defvar anvil-http-test--html-fixture
  "<html><body>
  <h1>Hello</h1>
  <div class=\"greet\">Welcome</div>
  <div id=\"main\">Main content</div>
  <p class=\"note\">alpha</p>
  <p class=\"note\">beta</p>
</body></html>"
  "Minimal HTML payload exercised by the selector tests.")

(defun anvil-http-test--make-resp (ct body)
  "Return a minimal response plist with content-type CT and body BODY."
  (list :status 200
        :headers (list :content-type ct)
        :body body
        :from-cache nil
        :cached-at nil
        :final-url "https://example.com/"
        :elapsed-ms 1))

(ert-deftest anvil-http-test-phase1b-select-html-libxml-tag ()
  "libxml path: `h1' selector extracts the heading text only."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let* ((resp (anvil-http-test--make-resp
                "text/html; charset=utf-8" anvil-http-test--html-fixture))
         (out (anvil-http--apply-extract resp "h1" nil)))
    (should (eq 'selector (plist-get out :extract-mode)))
    (should (eq 'libxml (plist-get out :extract-engine)))
    (should (string-match-p "\\`Hello" (plist-get out :body)))
    (should-not (plist-get out :extract-miss))))

(ert-deftest anvil-http-test-phase1b-select-html-libxml-tag-class ()
  "libxml path: `p.note' picks every paragraph with class note."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let* ((resp (anvil-http-test--make-resp
                "text/html" anvil-http-test--html-fixture))
         (out (anvil-http--apply-extract resp "p.note" nil))
         (body (plist-get out :body)))
    (should (string-match-p "alpha" body))
    (should (string-match-p "beta" body))
    (should-not (string-match-p "Welcome" body))
    (should (eq 'libxml (plist-get out :extract-engine)))))

(ert-deftest anvil-http-test-phase1b-select-html-fallback ()
  "Regex fallback runs when libxml-p reports nil and still matches
the shared subset (`#main')."
  (cl-letf (((symbol-function 'anvil-http--libxml-p)
             (lambda () nil)))
    (let* ((resp (anvil-http-test--make-resp
                  "text/html" anvil-http-test--html-fixture))
           (out (anvil-http--apply-extract resp "#main" nil)))
      (should (eq 'regex-subset (plist-get out :extract-engine)))
      (should (equal "Main content" (plist-get out :body)))
      (should-not (plist-get out :extract-miss)))))

(ert-deftest anvil-http-test-phase1b-select-html-miss ()
  "Selector that matches nothing → `:extract-miss' t, full body preserved."
  (let* ((resp (anvil-http-test--make-resp
                "text/html" anvil-http-test--html-fixture))
         (out (anvil-http--apply-extract resp "nosuch" nil)))
    (should (eq t (plist-get out :extract-miss)))
    (should (eq 'selector (plist-get out :extract-mode)))
    (should (string-match-p "Welcome" (plist-get out :body)))))

(ert-deftest anvil-http-test-phase1b-select-json-nested ()
  "json-path walks nested keys; the extracted node is re-serialized."
  (let* ((body "{\"data\":{\"count\":42,\"label\":\"ok\"}}")
         (resp (anvil-http-test--make-resp "application/json" body))
         (out (anvil-http--apply-extract resp nil "data.count")))
    (should (eq 'json-path (plist-get out :extract-mode)))
    (should (equal "42" (plist-get out :body)))
    (should-not (plist-get out :extract-miss))))

(ert-deftest anvil-http-test-phase1b-select-json-wildcard ()
  "`items[*].id' flattens an array; result round-trips as a JSON array."
  (let* ((body "{\"items\":[{\"id\":\"a\"},{\"id\":\"b\"},{\"id\":\"c\"}]}")
         (resp (anvil-http-test--make-resp "application/json" body))
         (out (anvil-http--apply-extract resp nil "items[*].id"))
         (decoded (json-parse-string (plist-get out :body)
                                     :array-type 'array)))
    (should (equal 3 (length decoded)))
    (should (equal "a" (aref decoded 0)))
    (should (equal "c" (aref decoded 2)))))

(ert-deftest anvil-http-test-phase1b-extract-content-type-mismatch ()
  "Selector supplied but body is JSON → mismatch engine + extract-miss."
  (let* ((resp (anvil-http-test--make-resp
                "application/json" "{\"a\":1}"))
         (out (anvil-http--apply-extract resp "h1" nil)))
    (should (eq t (plist-get out :extract-miss)))
    (should (eq 'selector (plist-get out :extract-mode)))
    (should (eq 'content-type-mismatch
                (plist-get out :extract-engine)))))

;;;; --- Phase 1c: overflow + header-filter ---------------------------------

(defmacro anvil-http-test--with-overflow-dir (dir-var &rest body)
  "Bind DIR-VAR to a fresh temp dir and delete it after BODY."
  (declare (indent 1))
  `(let ((,dir-var (make-temp-file "anvil-http-overflow-" t)))
     (unwind-protect
         (let ((anvil-http-overflow-dir ,dir-var))
           ,@body)
       (ignore-errors (delete-directory ,dir-var t)))))

(ert-deftest anvil-http-test-phase1c-overflow-spill ()
  "Body over `anvil-http-max-inline-body-bytes' spills to a hashed
tempfile; response carries head slice + overflow metadata."
  (anvil-http-test--with-overflow-dir dir
    (anvil-http-test--with-stub
        (list (anvil-http-test--response
               200
               (list :content-type "text/plain")
               (make-string 250000 ?a)))
      (let* ((anvil-http-max-inline-body-bytes 200000)
             (anvil-http-overflow-head-bytes 64)
             (r (anvil-http-get "https://example.com/big")))
        (should (eq t (plist-get r :body-truncated)))
        (should (= 64 (length (plist-get r :body))))
        (should (= 250000 (plist-get r :total-bytes)))
        (should (stringp (plist-get r :body-overflow-path)))
        (should (file-exists-p (plist-get r :body-overflow-path)))
        (should (= 64 (length (make-string 64 ?a))))
        (should (stringp (plist-get r :body-sha256)))
        (should (eq 'overflow (plist-get r :body-mode)))))))

(ert-deftest anvil-http-test-phase1c-body-mode-full ()
  "`body-mode full' inlines the body even when over threshold."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200
             (list :content-type "text/plain")
             (make-string 250000 ?a)))
    (let* ((anvil-http-max-inline-body-bytes 1000)
           (r (anvil-http-get "https://example.com/big" :body-mode 'full)))
      (should-not (plist-get r :body-truncated))
      (should-not (plist-get r :body-overflow-path))
      (should (= 250000 (length (plist-get r :body)))))))

(ert-deftest anvil-http-test-phase1c-body-mode-meta-only ()
  "`body-mode meta-only' drops :body entirely; metadata survives."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200
             (list :content-type "text/plain" :etag "\"w1\"")
             "hello"))
    (let ((r (anvil-http-get "https://example.com/m" :body-mode 'meta-only)))
      (should (null (plist-get r :body)))
      (should (eq 'meta-only (plist-get r :body-mode)))
      (should (= 200 (plist-get r :status)))
      ;; ETag must survive so the caller can pass it back next time.
      (should (equal "\"w1\"" (plist-get (plist-get r :headers) :etag))))))

(ert-deftest anvil-http-test-phase1c-header-filter-minimal ()
  "Default header-filter keeps only `anvil-http-minimal-header-keys'."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200
             (list :content-type "application/json"
                   :etag "\"abc\""
                   :server "nginx/1.19"
                   :x-cache "HIT"
                   :date "Thu, 01 Jan 1970 00:00:00 GMT")
             "{\"x\":1}"))
    (let* ((anvil-http-header-filter-default 'minimal)
           (r (anvil-http-get "https://example.com/j"))
           (h (plist-get r :headers)))
      (should (equal "application/json" (plist-get h :content-type)))
      (should (equal "\"abc\"" (plist-get h :etag)))
      (should (null (plist-get h :server)))
      (should (null (plist-get h :x-cache)))
      (should (null (plist-get h :date))))))

(ert-deftest anvil-http-test-phase1c-header-filter-all ()
  "`header-filter all' opt-out returns every header the server sent."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200
             (list :content-type "application/json"
                   :server "nginx/1.19"
                   :x-ratelimit-limit "60")
             "{}"))
    (let* ((anvil-http-header-filter-default 'minimal)
           (r (anvil-http-get "https://example.com/j"
                              :header-filter 'all))
           (h (plist-get r :headers)))
      (should (equal "nginx/1.19" (plist-get h :server)))
      (should (equal "60" (plist-get h :x-ratelimit-limit))))))

;;;; --- Phase 1e: robots.txt ------------------------------------------------

(ert-deftest anvil-http-test-phase1e-parse-basic ()
  "Parser groups User-agent + Allow + Disallow lines, ignoring comments."
  (let* ((txt "# comment\nUser-agent: *\nDisallow: /private\nAllow: /private/public\n\nUser-agent: BadBot\nDisallow: /\n")
         (groups (anvil-http--robots-parse txt)))
    (should (= 2 (length groups)))
    (let ((star (car groups))
          (badbot (cadr groups)))
      (should (equal '("*") (car star)))
      (should (equal '((disallow . "/private")
                       (allow . "/private/public"))
                     (cdr star)))
      (should (equal '("BadBot") (car badbot)))
      (should (equal '((disallow . "/")) (cdr badbot))))))

(ert-deftest anvil-http-test-phase1e-pick-group-longest-ua ()
  "pick-group picks the most-specific UA match; falls back to `*'."
  (let* ((groups (anvil-http--robots-parse
                  "User-agent: *\nDisallow: /a\n\nUser-agent: Anvil\nDisallow: /b\n")))
    ;; "Anvil/1.0" matches "Anvil" group
    (should (equal '((disallow . "/b"))
                   (anvil-http--robots-pick-group groups "Anvil/1.0")))
    ;; Unknown UA falls back to `*' group
    (should (equal '((disallow . "/a"))
                   (anvil-http--robots-pick-group groups "OtherBot/2.0")))))

(ert-deftest anvil-http-test-phase1e-match-longest-wins ()
  "Longest pattern wins; on a tie Allow beats Disallow (RFC 9309)."
  (let ((rules '((disallow . "/private")
                 (allow . "/private/public"))))
    ;; Longer pattern (allow /private/public) wins on /private/public/index
    (let ((m (anvil-http--robots-match rules "/private/public/index")))
      (should (car m))                 ; allowed
      (should (= 15 (cdr m)))))        ; "/private/public" = 15 chars
  (let ((rules '((disallow . "/path")
                 (allow . "/path"))))
    ;; Tie → allow wins
    (let ((m (anvil-http--robots-match rules "/path")))
      (should (car m)))))

(ert-deftest anvil-http-test-phase1e-pattern-wildcard-and-anchor ()
  "`*' expands to `.*' and trailing `$' anchors to URL end."
  (let ((rx-star (anvil-http--robots-pattern-to-regex "/a/*.pdf"))
        (rx-anchor (anvil-http--robots-pattern-to-regex "/a$")))
    (should (string-match-p rx-star "/a/x/y.pdf"))
    (should (string-match-p rx-star "/a/big.pdf"))
    (should-not (string-match-p rx-star "/b/file.pdf"))
    (should (string-match-p rx-anchor "/a"))
    (should-not (string-match-p rx-anchor "/a/more"))))

(ert-deftest anvil-http-test-phase1e-get-signals-on-disallow ()
  "anvil-http-get raises user-error when robots.txt Disallows the URL."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200 (list :content-type "text/plain")
             "User-agent: *\nDisallow: /secret\n")
            (anvil-http-test--response
             200 (list :content-type "text/plain") "unreached"))
    (let ((anvil-http-respect-robots-txt t))
      (should-error (anvil-http-get "https://example.com/secret/doc")
                    :type 'user-error))))

(ert-deftest anvil-http-test-phase1e-fail-open-on-error ()
  "robots.txt non-200 → fail-open; request proceeds."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 404 nil "Not found")
            (anvil-http-test--response
             200 (list :content-type "text/plain") "payload"))
    (let ((anvil-http-respect-robots-txt t))
      (let ((r (anvil-http-get "https://example.com/anything")))
        (should (= 200 (plist-get r :status)))
        (should (equal "payload" (plist-get r :body)))))))

;;;; --- Phase 1d: batch fetch ----------------------------------------------

(defmacro anvil-http-test--with-async-stub (response-alist &rest body)
  "Stub `anvil-http--request-async' to invoke the callback synchronously.
RESPONSE-ALIST maps URL to either a full response plist
(:status :headers :body :final-url) or an error plist (:error STR).
Binds a fresh `anvil-state' DB and disables robots.txt."
  (declare (indent 1))
  `(let ((anvil-http-test--async-map ,response-alist)
         (anvil-state-db-path (make-temp-file "anvil-http-batch-" nil ".db"))
         (anvil-state--db nil)
         (anvil-http-respect-robots-txt nil)
         (anvil-http--metrics
          (list :requests 0 :cache-fresh 0 :cache-revalidated 0
                :network-200 0 :errors 0 :log nil)))
     (unwind-protect
         (progn
           (anvil-state-enable)
           (cl-letf (((symbol-function 'anvil-http--request-async)
                      (lambda (url _hdrs _timeout cb)
                        (let ((resp (cdr (assoc url anvil-http-test--async-map))))
                          (funcall cb (or resp (list :error "no stub")))))))
             ,@body))
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path)))))

(ert-deftest anvil-http-test-phase1d-batch-all-cache-hits ()
  "Fully-cached batch short-circuits async and returns in input order."
  (anvil-http-test--with-async-stub nil
    (let ((urls '("https://a.test/one" "https://b.test/two"))
          (now (float-time)))
      (dolist (u urls)
        (anvil-state-set
         (anvil-http--normalize-url u)
         (list :status 200
               :headers (list :content-type "text/plain")
               :body (format "cached-%s" u)
               :fetched-at now
               :final-url u
               :etag nil :last-modified nil)
         :ns "http"))
      (let ((r (anvil-http-get-batch urls :cache-ttl-sec 600)))
        (should (= 2 (length r)))
        (should (eq t (plist-get (nth 0 r) :from-cache)))
        (should (eq t (plist-get (nth 1 r) :from-cache)))
        (should (string-match-p "a.test/one" (plist-get (nth 0 r) :body)))
        (should (string-match-p "b.test/two" (plist-get (nth 1 r) :body)))))))

(ert-deftest anvil-http-test-phase1d-batch-preserves-input-order ()
  "Async stub returns per-URL responses; batch output matches input order."
  (anvil-http-test--with-async-stub
      '(("https://x.test/a" . (:status 200
                               :headers (:content-type "text/plain")
                               :body "A"
                               :final-url "https://x.test/a"))
        ("https://x.test/b" . (:status 200
                               :headers (:content-type "text/plain")
                               :body "B"
                               :final-url "https://x.test/b"))
        ("https://x.test/c" . (:status 200
                               :headers (:content-type "text/plain")
                               :body "C"
                               :final-url "https://x.test/c")))
    (let ((r (anvil-http-get-batch
              '("https://x.test/a" "https://x.test/b" "https://x.test/c")
              :cache-ttl-sec 0)))
      (should (equal "A" (plist-get (nth 0 r) :body)))
      (should (equal "B" (plist-get (nth 1 r) :body)))
      (should (equal "C" (plist-get (nth 2 r) :body))))))

(ert-deftest anvil-http-test-phase1d-batch-per-url-error ()
  "Per-URL failure surfaces as (:url :error); other URLs still succeed."
  (anvil-http-test--with-async-stub
      '(("https://ok.test/" . (:status 200
                               :headers (:content-type "text/plain")
                               :body "ok"
                               :final-url "https://ok.test/"))
        ("https://bad.test/" . (:error "connection refused")))
    (let ((r (anvil-http-get-batch
              '("https://ok.test/" "https://bad.test/")
              :cache-ttl-sec 0)))
      (should (equal "ok" (plist-get (nth 0 r) :body)))
      (should (equal "connection refused" (plist-get (nth 1 r) :error)))
      (should (equal "https://bad.test/" (plist-get (nth 1 r) :url))))))

(ert-deftest anvil-http-test-phase1d-batch-exceeds-max-errors ()
  "Batch larger than `anvil-http-batch-max' raises a user-error upfront."
  (let ((anvil-http-batch-max 2))
    (should-error
     (anvil-http-get-batch
      '("https://a.test/" "https://b.test/" "https://c.test/"))
     :type 'user-error)))

;;;; --- Phase 1.5: POST + auth + jitter ------------------------------------

(ert-deftest anvil-http-test-phase1-5-post-form-urlencoded ()
  "Alist body is form-urlencoded with the right Content-Type."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 200 nil "ok"))
    (anvil-http-post "https://api.example.com/v1/x"
                     :body '(("name" . "Alice") ("n" . "42")))
    (let* ((call (car anvil-http-test--calls))
           (hdrs (plist-get call :headers))
           (body (plist-get call :body)))
      (should (equal "POST" (plist-get call :method)))
      (should (member '("Content-Type" . "application/x-www-form-urlencoded")
                      hdrs))
      (should (equal "name=Alice&n=42" body)))))

(ert-deftest anvil-http-test-phase1-5-post-json-plist ()
  "Plist body is JSON-serialised; Content-Type set to application/json."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 200 nil "{}"))
    (anvil-http-post "https://api.example.com/v1/y"
                     :body '(:foo "bar" :n 42))
    (let* ((call (car anvil-http-test--calls))
           (hdrs (plist-get call :headers))
           (body (plist-get call :body))
           (parsed (json-parse-string body :object-type 'alist)))
      (should (member '("Content-Type" . "application/json") hdrs))
      (should (equal "bar" (alist-get 'foo parsed nil nil #'equal)))
      (should (equal 42 (alist-get 'n parsed nil nil #'equal))))))

(ert-deftest anvil-http-test-phase1-5-post-raw-string-explicit-ct ()
  "String body is passed through and caller-supplied Content-Type wins."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 200 nil "ok"))
    (anvil-http-post "https://api.example.com/v1/z"
                     :body "<xml/>"
                     :content-type "application/xml")
    (let* ((call (car anvil-http-test--calls))
           (hdrs (plist-get call :headers))
           (body (plist-get call :body)))
      (should (equal "<xml/>" body))
      (should (member '("Content-Type" . "application/xml") hdrs)))))

(ert-deftest anvil-http-test-phase1-5-auth-bearer ()
  "(:bearer TOKEN) sets Authorization: Bearer TOKEN."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 200 nil "{}"))
    (anvil-http-post "https://api.example.com/secure"
                     :body "{}"
                     :content-type "application/json"
                     :auth '(:bearer "tok-abc"))
    (let* ((call (car anvil-http-test--calls))
           (hdrs (plist-get call :headers)))
      (should (equal "Bearer tok-abc"
                     (cdr (assoc "Authorization" hdrs)))))))

(ert-deftest anvil-http-test-phase1-5-auth-basic ()
  "(:basic (USER . PASS)) base64-encodes USER:PASS into Authorization."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 200 nil "{}"))
    (anvil-http-post "https://api.example.com/private"
                     :body ""
                     :auth '(:basic ("alice" . "s3cret")))
    (let* ((call (car anvil-http-test--calls))
           (hdrs (plist-get call :headers))
           (auth (cdr (assoc "Authorization" hdrs))))
      (should (string-match "\\`Basic \\(.+\\)" auth))
      (let* ((b64 (match-string 1 auth))
             (decoded (base64-decode-string b64)))
        (should (equal "alice:s3cret" decoded))))))

(ert-deftest anvil-http-test-phase1-5-retry-jitter-bounded ()
  "`--apply-jitter' stays inside +/-`retry-jitter-ratio' of base."
  (let ((anvil-http-retry-jitter-ratio 0.25))
    (cl-loop for _ from 1 to 50 do
             (let ((j (anvil-http--apply-jitter 1000)))
               (should (and (>= j 750) (<= j 1250))))))
  (let ((anvil-http-retry-jitter-ratio 0))
    (should (= 1000 (anvil-http--apply-jitter 1000)))))

;;;; --- Phase 2: offload ---------------------------------------------------

(defmacro anvil-http-test--with-offload-stub (success-or-error
                                              &rest body)
  "Run BODY with anvil-offload stubbed.
SUCCESS-OR-ERROR is `(:value PLIST)' / `(:error STR)' / `:timeout'.
Robots check is disabled and a fresh state DB provisioned."
  (declare (indent 1))
  `(let* ((s ,success-or-error)
          (anvil-state-db-path (make-temp-file "anvil-http-off-" nil ".db"))
          (anvil-state--db nil)
          (anvil-http-respect-robots-txt nil))
     (unwind-protect
         (cl-letf* (((symbol-function 'anvil-offload)
                     (lambda (_form &rest _keys) :fake-future))
                    ((symbol-function 'anvil-future-await)
                     (lambda (_future &optional _timeout) nil))
                    ((symbol-function 'anvil-future-done-p)
                     (lambda (_future) (not (eq s :timeout))))
                    ((symbol-function 'anvil-future-error)
                     (lambda (_future)
                       (and (eq (car-safe s) :error) (cadr s))))
                    ((symbol-function 'anvil-future-value)
                     (lambda (_future)
                       (and (eq (car-safe s) :value) (cadr s)))))
           (anvil-state-enable)
           ,@body)
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path)))))

(ert-deftest anvil-http-test-phase2-offload-success ()
  "`:offload t' returns the value the offload future resolved to."
  (anvil-http-test--with-offload-stub
      (list :value (list :status 200
                         :headers (list :content-type "text/plain")
                         :body "from-offload"
                         :from-cache nil
                         :final-url "https://example.com/big"))
    (let ((r (anvil-http-get "https://example.com/big" :offload t)))
      (should (= 200 (plist-get r :status)))
      (should (equal "from-offload" (plist-get r :body))))))

(ert-deftest anvil-http-test-phase2-offload-error-surfaces ()
  "Worker-side error surfaces as `anvil-server-tool-error'."
  (anvil-http-test--with-offload-stub
      (list :error "subprocess crashed: SIGSEGV")
    (should-error
     (anvil-http-get "https://example.com/x" :offload t)
     :type 'anvil-server-tool-error)))

(ert-deftest anvil-http-test-phase2-offload-timeout-surfaces ()
  "Future never reaching `done' becomes a tool-error."
  (anvil-http-test--with-offload-stub :timeout
    (should-error
     (anvil-http-get "https://example.com/slow"
                     :offload t :offload-timeout 1)
     :type 'anvil-server-tool-error)))

;;;; --- Phase 3: cache LRU + size cap + metrics ---------------------------

(defmacro anvil-http-test--with-cache (&rest body)
  "Run BODY with a fresh anvil-state DB and zeroed cache byte counter."
  (declare (indent 0))
  `(let ((anvil-state-db-path
          (make-temp-file "anvil-http-cache-" nil ".db"))
         (anvil-state--db nil)
         (anvil-http--cache-bytes-counter 0)
         (anvil-http--metrics
          (list :requests 0 :cache-fresh 0 :cache-revalidated 0
                :network-200 0 :errors 0 :evictions 0 :log nil)))
     (unwind-protect
         (progn (anvil-state-enable) ,@body)
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path)))))

(ert-deftest anvil-http-test-phase3-cache-put-updates-counter ()
  "`--cache-put' bumps the running byte counter by body length."
  (anvil-http-test--with-cache
    (let ((entry (anvil-http--cache-entry
                  200 (list :content-type "text/plain")
                  (make-string 1000 ?a)
                  (float-time)
                  "https://example.com/x")))
      (anvil-http--cache-put "https://example.com/x" entry)
      (should (= 1000 anvil-http--cache-bytes-counter)))))

(ert-deftest anvil-http-test-phase3-cache-delete-decrements-counter ()
  "`--cache-delete' subtracts the deleted entry's body length."
  (anvil-http-test--with-cache
    (let ((entry (anvil-http--cache-entry
                  200 (list :content-type "text/plain")
                  (make-string 500 ?b)
                  (float-time)
                  "https://example.com/y")))
      (anvil-http--cache-put "https://example.com/y" entry)
      (should (= 500 anvil-http--cache-bytes-counter))
      (anvil-http--cache-delete "https://example.com/y")
      (should (= 0 anvil-http--cache-bytes-counter)))))

(ert-deftest anvil-http-test-phase3-eviction-drops-oldest ()
  "Going over `anvil-http-cache-size-cap-bytes' evicts oldest first."
  (anvil-http-test--with-cache
    (let* ((anvil-http-cache-size-cap-bytes 1700)
           (now (float-time)))
      ;; Three 800-byte entries → 2400 total; cap=1700 leaves room for
      ;; two entries → the oldest is evicted on the third put.
      (dolist (i '(1 2 3))
        (let ((entry (anvil-http--cache-entry
                      200 (list :content-type "text/plain")
                      (make-string 800 ?x)
                      (+ now i)             ; ascending fetched-at
                      (format "https://example.com/%d" i))))
          (anvil-http--cache-put (format "https://example.com/%d" i) entry)))
      ;; The first entry (oldest) should have been evicted.
      (should-not (anvil-http--cache-get "https://example.com/1"))
      (should (anvil-http--cache-get "https://example.com/2"))
      (should (anvil-http--cache-get "https://example.com/3"))
      (should (>= (or (plist-get anvil-http--metrics :evictions) 0) 1))
      (should (<= anvil-http--cache-bytes-counter
                  anvil-http-cache-size-cap-bytes)))))

(ert-deftest anvil-http-test-phase3-cache-status-payload ()
  "`http-cache-status' returns size, entries, cap, metrics."
  (anvil-http-test--with-cache
    (let ((entry (anvil-http--cache-entry
                  200 (list :content-type "text/plain")
                  (make-string 200 ?z)
                  (float-time)
                  "https://example.com/s")))
      (anvil-http--cache-put "https://example.com/s" entry)
      (let ((p (anvil-http--cache-status-payload)))
        (should (equal "http" (plist-get p :ns)))
        (should (= 1 (plist-get p :entries)))
        (should (= 200 (plist-get p :bytes)))
        (should (numberp (plist-get p :cap)))
        (should (= 0 (plist-get p :evictions)))))))

;;;; --- live smoke test ----------------------------------------------------

(ert-deftest anvil-http-test-live-example-com ()
  "Hit the real server when ANVIL_ALLOW_LIVE=1."
  (skip-unless (and (not (getenv "ANVIL_SKIP_LIVE"))
                    (getenv "ANVIL_ALLOW_LIVE")))
  (let ((anvil-state-db-path (make-temp-file "anvil-http-live-" nil ".db"))
        (anvil-state--db nil)
        (anvil-http--metrics
         (list :requests 0 :cache-fresh 0 :cache-revalidated 0
               :network-200 0 :errors 0 :log nil)))
    (unwind-protect
        (progn
          (anvil-state-enable)
          (let ((resp (anvil-http-get "https://example.com/"
                                      :cache-ttl-sec 0)))
            (should (= 200 (plist-get resp :status)))
            (should (string-match-p "Example Domain"
                                    (plist-get resp :body)))))
      (anvil-state-disable)
      (ignore-errors (delete-file anvil-state-db-path)))))

(provide 'anvil-http-test)
;;; anvil-http-test.el ends here
