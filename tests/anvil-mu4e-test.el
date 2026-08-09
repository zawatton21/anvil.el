;;; anvil-mu4e-test.el --- tests for anvil-mu4e -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercises the Phase 1 mu4e read/search tools against the *real* `mu'
;; binary: each test builds a disposable maildir, indexes it into a
;; throwaway muhome, and asserts the JSON the tools return.  Tests
;; `skip-unless' mu is installed, so CI without mu skips rather than fails.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'anvil-mu4e)

(defconst anvil-mu4e-test--msg1
  "From: Alice Example <alice@example.com>
To: Bob User <bob@example.com>
Cc: Carol <carol@example.com>
Subject: Invoice for May
Date: Thu, 18 Jun 2026 09:31:00 +0900
Message-ID: <inv-may-2026@example.com>

Hello Bob,

Please find the invoice for May attached.

Regards,
Alice
")

(defconst anvil-mu4e-test--msg2
  "From: Dave <dave@example.com>
To: Bob User <bob@example.com>
Subject: Re: Invoice for May
Date: Thu, 18 Jun 2026 10:05:00 +0900
Message-ID: <reply1@example.com>
In-Reply-To: <inv-may-2026@example.com>
References: <inv-may-2026@example.com>

Got it, thanks.
")

(defun anvil-mu4e-test--write (path content)
  "Write CONTENT to PATH (creating parents) as utf-8-unix."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path (insert content))))

(defun anvil-mu4e-test--build-index ()
  "Create a throwaway maildir with two messages and index it with mu.
Return a plist (:root ROOT :muhome MUHOME)."
  (let* ((root (make-temp-file "anvil-mu4e-test-" t))
         (md (expand-file-name "Maildir" root))
         (mh (expand-file-name "muhome" root))
         (inbox (expand-file-name "INBOX" md)))
    (dolist (d '("cur" "new" "tmp"))
      (make-directory (expand-file-name d inbox) t))
    (anvil-mu4e-test--write (expand-file-name "cur/msg1:2,S" inbox)
                            anvil-mu4e-test--msg1)
    (anvil-mu4e-test--write (expand-file-name "new/msg2" inbox)
                            anvil-mu4e-test--msg2)
    (unless (= 0 (call-process "mu" nil nil nil "init"
                               (concat "--maildir=" md)
                               (concat "--muhome=" mh)
                               "--my-address=bob@example.com"))
      (error "anvil-mu4e-test: `mu init' failed"))
    (unless (= 0 (call-process "mu" nil nil nil "index"
                               (concat "--muhome=" mh)))
      (error "anvil-mu4e-test: `mu index' failed"))
    (list :root root :muhome mh)))

(defmacro anvil-mu4e-test--with-index (&rest body)
  "Build a throwaway mu index, bind module vars to it, run BODY, clean up."
  (declare (indent 0))
  `(let* ((ctx (anvil-mu4e-test--build-index))
          (anvil-mu4e-muhome (plist-get ctx :muhome))
          (anvil-mu4e-mu-bin (or (executable-find "mu") "mu")))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory (plist-get ctx :root) t)))))

(defun anvil-mu4e-test--parse (wrapped)
  "Parse WRAPPED into alists/lists for assertion (false/null -> nil).
The read/search tools wrap their JSON in an
`<untrusted-email-content>' guard (line 1 = open tag, line 2 =
notice, line 3 = JSON, line 4 = close tag); strip that when present.
compose-draft/send responses are unwrapped JSON and pass through as-is."
  (json-parse-string
   (if (string-prefix-p "<untrusted-email-content>\n" wrapped)
       (nth 2 (split-string wrapped "\n"))
     wrapped)
   :object-type 'alist :array-type 'list
   :false-object nil :null-object nil))

(defmacro anvil-mu4e-test--with-drafts (&rest body)
  "Run BODY with a throwaway drafts maildir and inert send defaults.
Binds `anvil-mu4e-drafts-dir', `anvil-mu4e-from', and resets every send
gate so no test can ever transmit mail unless it opts in explicitly."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "anvil-mu4e-drafts-" t))
          (anvil-mu4e-drafts-dir dir)
          (anvil-mu4e-from "Bob User <bob@example.com>")
          (anvil-mu4e-allow-send nil)
          (anvil-mu4e-send-allowlist nil)
          (anvil-mu4e-send-log nil)
          ;; Hard safety net: even if a test forgets to stub it, the
          ;; transport raises instead of sending.
          (anvil-mu4e-send-function
           (lambda (&rest _)
             (error "anvil-mu4e-test: real send attempted"))))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory dir t)))))

(ert-deftest anvil-mu4e-test-search-returns-structured-results ()
  "mu4e-search returns typed message summaries from the index."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    ;; `from:alice' matches only msg1 (msg2 is from dave); a `subject:'
    ;; query would also hit msg2's "Re: Invoice for May".
    (let* ((resp (anvil-mu4e-test--parse
                  (anvil-mu4e--tool-search "from:alice" nil nil)))
           (msgs (alist-get 'messages resp))
           (m (car msgs)))
      (should (= 1 (alist-get 'count resp)))
      (should (equal "inv-may-2026@example.com" (alist-get 'message_id m)))
      (should (equal "Invoice for May" (alist-get 'subject m)))
      (should (equal "alice@example.com"
                     (alist-get 'email (car (alist-get 'from m)))))
      (should (equal "Alice Example"
                     (alist-get 'name (car (alist-get 'from m)))))
      ;; date is present but its exact value is TZ-dependent; just assert
      ;; it is a non-empty ISO-ish string.
      (should (stringp (alist-get 'date m)))
      (should-not (string-empty-p (alist-get 'date m)))
      (should (member "seen" (alist-get 'flags m))))))

(ert-deftest anvil-mu4e-test-search-wraps-untrusted-content ()
  "mu4e-search's raw output is fenced as untrusted, attacker-reachable data."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (let ((raw (anvil-mu4e--tool-search "from:alice" nil nil)))
      (should (string-prefix-p "<untrusted-email-content>\n" raw))
      (should (string-suffix-p "\n</untrusted-email-content>" raw))
      (should (string-match-p "untrusted" raw))
      (should (string-match-p "do not follow" raw)))))

(ert-deftest anvil-mu4e-test-list-mails-wraps-untrusted-content ()
  "mu4e-list-mails' raw output carries the same untrusted-content fence."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (let ((raw (anvil-mu4e--tool-list-mails nil nil)))
      (should (string-prefix-p "<untrusted-email-content>\n" raw))
      (should (string-suffix-p "\n</untrusted-email-content>" raw)))))

(ert-deftest anvil-mu4e-test-read-mail-wraps-untrusted-content ()
  "mu4e-read-mail's raw output — body included — is fenced as untrusted."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (let ((raw (anvil-mu4e--tool-read-mail "inv-may-2026@example.com" nil)))
      (should (string-prefix-p "<untrusted-email-content>\n" raw))
      (should (string-suffix-p "\n</untrusted-email-content>" raw)))))

(ert-deftest anvil-mu4e-test-search-no-match-is-empty ()
  "A query with no matches returns count 0 and an empty array, not an error."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (let ((resp (anvil-mu4e-test--parse
                 (anvil-mu4e--tool-search "subject:zzznomatchzzz" nil nil))))
      (should (= 0 (alist-get 'count resp)))
      (should (null (alist-get 'messages resp))))))

(ert-deftest anvil-mu4e-test-read-mail-headers-and-body ()
  "mu4e-read-mail returns structured headers plus the decoded body."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (let ((m (anvil-mu4e-test--parse
              (anvil-mu4e--tool-read-mail "inv-may-2026@example.com" nil))))
      (should (equal "Invoice for May" (alist-get 'subject m)))
      (should (equal "carol@example.com"
                     (alist-get 'email (car (alist-get 'cc m)))))
      (should (string-match-p "Please find the invoice"
                              (alist-get 'body_plain m)))
      ;; the header preamble must be stripped from the body
      (should-not (string-match-p "^Subject:" (alist-get 'body_plain m))))))

(ert-deftest anvil-mu4e-test-read-mail-accepts-angle-brackets ()
  "Message-ID lookup tolerates surrounding angle brackets."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (let ((m (anvil-mu4e-test--parse
              (anvil-mu4e--tool-read-mail "<inv-may-2026@example.com>" nil))))
      (should (equal "Invoice for May" (alist-get 'subject m))))))

(ert-deftest anvil-mu4e-test-read-mail-reply-threading ()
  "A reply carries its References back to the parent message."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (let ((m (anvil-mu4e-test--parse
              (anvil-mu4e--tool-read-mail "reply1@example.com" nil))))
      (should (equal "Re: Invoice for May" (alist-get 'subject m)))
      (should (member "inv-may-2026@example.com"
                      (alist-get 'references m))))))

(ert-deftest anvil-mu4e-test-list-mails-defaults-to-inbox ()
  "mu4e-list-mails with no query lists the inbox (both messages)."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (let ((resp (anvil-mu4e-test--parse
                 (anvil-mu4e--tool-list-mails nil nil))))
      (should (= 2 (alist-get 'count resp))))))

(ert-deftest anvil-mu4e-test-max-results-caps-output ()
  "max_results caps the number of returned messages (string-coerced)."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (let ((resp (anvil-mu4e-test--parse
                 ;; pass as a string, as an MCP client would
                 (anvil-mu4e--tool-list-mails "" "1"))))
      (should (= 1 (alist-get 'count resp))))))

(ert-deftest anvil-mu4e-test-missing-mu-binary-errors-actionably ()
  "An absent mu binary yields an actionable error, not a bare failure."
  (let* ((anvil-mu4e-mu-bin "/nonexistent/mu-binary-zzz")
         (err (should-error (anvil-mu4e--tool-search "x" nil nil)
                            :type 'error)))
    (should (string-match-p "mu binary not found"
                            (error-message-string err)))))

;;;; --- Phase 1.1: CJK substring fallback -----------------------------------

(defun anvil-mu4e-test--build-cjk-index ()
  "Create a throwaway maildir with CJK messages and index it with mu.
Return a plist (:root ROOT :muhome MUHOME)."
  (let* ((root (make-temp-file "anvil-mu4e-cjk-" t))
         (md (expand-file-name "Maildir" root))
         (mh (expand-file-name "muhome" root))
         (inbox (expand-file-name "INBOX" md)))
    (dolist (d '("cur" "new" "tmp"))
      (make-directory (expand-file-name d inbox) t))
    (anvil-mu4e-test--write
     (expand-file-name "cur/c1:2,S" inbox)
     "From: Alice Tanaka <alice@example.com>\nTo: You <you@example.com>\nSubject: 5月分の請求書\nDate: Mon, 15 Jun 2026 09:31:00 +0900\nMessage-ID: <c1@example.com>\n\n請求書の件、ご確認ください。\n")
    (anvil-mu4e-test--write
     (expand-file-name "new/c2" inbox)
     "From: 電力会社 <info@power.example>\nTo: You <you@example.com>\nSubject: 定期点検のお知らせ\nDate: Wed, 10 Jun 2026 08:00:00 +0900\nMessage-ID: <c2@power.example>\n\n点検日程をお知らせします。\n")
    (anvil-mu4e-test--write
     (expand-file-name "new/c3" inbox)
     "From: Dave <dave@example.com>\nTo: You <you@example.com>\nSubject: Weekly report\nDate: Tue, 16 Jun 2026 10:00:00 +0900\nMessage-ID: <c3@example.com>\n\n領収書を添付します。\n")
    (anvil-mu4e-test--write
     (expand-file-name "new/c4" inbox)
     "From: Bob <bob@example.com>\nTo: You <you@example.com>\nSubject: Lunch\nDate: Tue, 16 Jun 2026 12:00:00 +0900\nMessage-ID: <c4@example.com>\n\nlunch tomorrow?\n")
    (unless (= 0 (call-process "mu" nil nil nil "init"
                               (concat "--maildir=" md)
                               (concat "--muhome=" mh)
                               "--my-address=you@example.com"))
      (error "anvil-mu4e-test: `mu init' (cjk) failed"))
    (unless (= 0 (call-process "mu" nil nil nil "index"
                               (concat "--muhome=" mh)))
      (error "anvil-mu4e-test: `mu index' (cjk) failed"))
    (list :root root :muhome mh)))

(defmacro anvil-mu4e-test--with-cjk-index (&rest body)
  "Build a throwaway CJK mu index, bind module vars, run BODY, clean up."
  (declare (indent 0))
  `(let* ((ctx (anvil-mu4e-test--build-cjk-index))
          (anvil-mu4e-muhome (plist-get ctx :muhome))
          (anvil-mu4e-mu-bin (or (executable-find "mu") "mu"))
          (anvil-mu4e-cjk-fallback t)
          (anvil-mu4e-cjk-search-body nil))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory (plist-get ctx :root) t)))))

(ert-deftest anvil-mu4e-test-cjk-bare-term-matches-subject ()
  "A bare CJK term mu cannot tokenize is matched via the substring fallback."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-cjk-index
    (let* ((resp (anvil-mu4e-test--parse (anvil-mu4e--tool-search "請求書" nil nil)))
           (subjects (mapcar (lambda (m) (alist-get 'subject m))
                             (alist-get 'messages resp))))
      (should (eq t (alist-get 'cjk_fallback resp)))
      (should (= 1 (alist-get 'count resp)))
      (should (member "5月分の請求書" subjects)))))

(ert-deftest anvil-mu4e-test-cjk-field-term-matches-from ()
  "A field term with a CJK value (from:電力) filters on that field."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-cjk-index
    (let ((resp (anvil-mu4e-test--parse (anvil-mu4e--tool-search "from:電力" nil nil))))
      (should (eq t (alist-get 'cjk_fallback resp)))
      (should (= 1 (alist-get 'count resp)))
      (should (equal "定期点検のお知らせ"
                     (alist-get 'subject (car (alist-get 'messages resp))))))))

(ert-deftest anvil-mu4e-test-cjk-mixed-query-narrows-then-filters ()
  "A mixed query lets mu narrow (from:) and the fallback filter the CJK term."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-cjk-index
    (let ((resp (anvil-mu4e-test--parse
                 (anvil-mu4e--tool-search "from:info@power.example 点検" nil nil))))
      (should (= 1 (alist-get 'count resp)))
      (should (equal "定期点検のお知らせ"
                     (alist-get 'subject (car (alist-get 'messages resp))))))))

(ert-deftest anvil-mu4e-test-cjk-no-match ()
  "A CJK term present nowhere returns an empty result (still via fallback)."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-cjk-index
    (let ((resp (anvil-mu4e-test--parse
                 (anvil-mu4e--tool-search "存在しない語" nil nil))))
      (should (eq t (alist-get 'cjk_fallback resp)))
      (should (= 0 (alist-get 'count resp))))))

(ert-deftest anvil-mu4e-test-cjk-body-scan-is-opt-in ()
  "A CJK term only in the body matches only when body scanning is enabled."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-cjk-index
    ;; 領収書 appears only in c3's body (subject is ASCII "Weekly report").
    (let ((off (anvil-mu4e-test--parse (anvil-mu4e--tool-search "領収書" nil nil))))
      (should (= 0 (alist-get 'count off))))
    (let* ((anvil-mu4e-cjk-search-body t)
           (on (anvil-mu4e-test--parse (anvil-mu4e--tool-search "領収書" nil nil))))
      (should (= 1 (alist-get 'count on)))
      (should (equal "Weekly report"
                     (alist-get 'subject (car (alist-get 'messages on))))))))

(ert-deftest anvil-mu4e-test-ascii-query-skips-fallback ()
  "A pure-ASCII query uses mu directly (no cjk_fallback flag)."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-cjk-index
    (let ((resp (anvil-mu4e-test--parse (anvil-mu4e--tool-search "from:bob" nil nil))))
      (should-not (alist-get 'cjk_fallback resp))
      (should (= 1 (alist-get 'count resp)))
      (should (equal "Lunch"
                     (alist-get 'subject (car (alist-get 'messages resp))))))))

;;;; --- Phase 2: compose / send --------------------------------------------

(ert-deftest anvil-mu4e-test-compose-draft-writes-draft ()
  "mu4e-compose-draft saves a well-formed draft and never sends."
  (anvil-mu4e-test--with-drafts
    (let* ((d (anvil-mu4e-test--parse
               (anvil-mu4e--tool-compose-draft
                "alice@example.com" "carol@example.com" "Hi there"
                "Hello!\n" nil)))
           (path (alist-get 'draft_id d)))
      (should (eq t (alist-get 'saved d)))
      (should-not (alist-get 'sent d))
      (should (file-exists-p path))
      (should (file-in-directory-p path (expand-file-name anvil-mu4e-drafts-dir)))
      (should (string-match-p ":2,D\\'" path))
      (should (equal "Hi there" (anvil-mu4e--header-value path "Subject")))
      (should (equal "alice@example.com" (anvil-mu4e--header-value path "To")))
      (should (equal "carol@example.com" (anvil-mu4e--header-value path "Cc")))
      (should (string-match-p "bob@example.com"
                              (anvil-mu4e--header-value path "From")))
      (should (string-match-p
               "Hello!"
               (with-temp-buffer (insert-file-contents path) (buffer-string)))))))

(ert-deftest anvil-mu4e-test-compose-rescues-literal-newline-escapes ()
  "A body sent with literal \\n / \\t escapes is decoded into real newlines.
Agents frequently pass backslash-n rather than real newlines (issue #50);
the draft must contain real line breaks, not a literal \"\\n\"."
  (anvil-mu4e-test--with-drafts
    (let* ((d (anvil-mu4e-test--parse
               (anvil-mu4e--tool-compose-draft
                "alice@example.com" nil "Esc"
                "Line1\\nLine2\\n\\nSaludos,\\nBob" nil)))
           (path (alist-get 'draft_id d))
           (content (with-temp-buffer
                      (insert-file-contents path) (buffer-string))))
      ;; Real line breaks are present ...
      (should (string-match-p "Line1\nLine2\n\nSaludos,\nBob" content))
      ;; ... and no literal backslash-n leaked into the message.
      (should-not (string-match-p "\\\\n" content)))))

(ert-deftest anvil-mu4e-test-compose-preserves-real-newlines ()
  "A body that already uses real newlines is left untouched (no rescue)."
  (anvil-mu4e-test--with-drafts
    (let* ((d (anvil-mu4e-test--parse
               (anvil-mu4e--tool-compose-draft
                "alice@example.com" nil "Plain" "Para1\n\nPara2" nil)))
           (path (alist-get 'draft_id d))
           (content (with-temp-buffer
                      (insert-file-contents path) (buffer-string))))
      (should (string-match-p "Para1\n\nPara2" content)))))

(ert-deftest anvil-mu4e-test-compose-reply-sets-threading ()
  "A reply sets In-Reply-To and References even when the parent is unknown."
  (anvil-mu4e-test--with-drafts
    (let* ((d (anvil-mu4e-test--parse
               (anvil-mu4e--tool-compose-draft
                "alice@example.com" nil "Re: x" "ok"
                "ghost-msg@nowhere.example")))
           (path (alist-get 'draft_id d)))
      (should (equal "<ghost-msg@nowhere.example>"
                     (anvil-mu4e--header-value path "In-Reply-To")))
      (should (string-match-p "ghost-msg@nowhere.example"
                              (anvil-mu4e--header-value path "References"))))))

(ert-deftest anvil-mu4e-test-compose-reply-to-indexed-message ()
  "Replying to an indexed message inherits its Re: subject and references."
  (skip-unless (executable-find "mu"))
  (anvil-mu4e-test--with-index
    (anvil-mu4e-test--with-drafts
      (let* ((d (anvil-mu4e-test--parse
                 (anvil-mu4e--tool-compose-draft
                  "alice@example.com" nil nil "thanks"
                  "inv-may-2026@example.com")))
             (path (alist-get 'draft_id d)))
        (should (equal "Re: Invoice for May" (alist-get 'subject d)))
        (should (string-match-p "inv-may-2026@example.com"
                                (anvil-mu4e--header-value path "References")))))))

(ert-deftest anvil-mu4e-test-send-previews-without-confirm ()
  "Without confirm=true, mu4e-send previews and does not call the transport."
  (anvil-mu4e-test--with-drafts
    (let* ((calls '())
           (anvil-mu4e-send-function (lambda (p) (push p calls) t))
           (anvil-mu4e-allow-send t)
           (anvil-mu4e-send-allowlist '("@example\\.com\\'"))
           (d (anvil-mu4e-test--parse
               (anvil-mu4e--tool-compose-draft "alice@example.com" nil "Hi" "yo" nil)))
           (did (alist-get 'draft_id d))
           (r (anvil-mu4e-test--parse (anvil-mu4e--tool-send did :json-false))))
      (should-not (alist-get 'sent r))
      (should (string-match-p "confirm" (alist-get 'reason r)))
      (should (null calls)))))

(ert-deftest anvil-mu4e-test-send-refused-when-disabled ()
  "With confirm but `anvil-mu4e-allow-send' nil, send is refused, no transport."
  (anvil-mu4e-test--with-drafts
    (let* ((calls '())
           (anvil-mu4e-send-function (lambda (p) (push p calls) t))
           (anvil-mu4e-allow-send nil)
           (anvil-mu4e-send-allowlist '(".*"))
           (d (anvil-mu4e-test--parse
               (anvil-mu4e--tool-compose-draft "alice@example.com" nil "Hi" "yo" nil)))
           (did (alist-get 'draft_id d))
           (r (anvil-mu4e-test--parse (anvil-mu4e--tool-send did t))))
      (should-not (alist-get 'sent r))
      (should (string-match-p "disabled" (alist-get 'reason r)))
      (should (null calls)))))

(ert-deftest anvil-mu4e-test-send-refused-for-disallowed-recipient ()
  "A recipient outside the allowlist blocks the send."
  (anvil-mu4e-test--with-drafts
    (let* ((calls '())
           (anvil-mu4e-send-function (lambda (p) (push p calls) t))
           (anvil-mu4e-allow-send t)
           (anvil-mu4e-send-allowlist '("@trusted\\.example\\'"))
           (d (anvil-mu4e-test--parse
               (anvil-mu4e--tool-compose-draft "alice@example.com" nil "Hi" "yo" nil)))
           (did (alist-get 'draft_id d))
           (r (anvil-mu4e-test--parse (anvil-mu4e--tool-send did t))))
      (should-not (alist-get 'sent r))
      (should (string-match-p "allowlist" (alist-get 'reason r)))
      (should (null calls)))))

(ert-deftest anvil-mu4e-test-send-succeeds-when-gates-open ()
  "With all three gates satisfied, send calls the transport once and audits."
  (anvil-mu4e-test--with-drafts
    (let* ((calls '())
           (anvil-mu4e-send-function (lambda (p) (push p calls) t))
           (anvil-mu4e-allow-send t)
           (anvil-mu4e-send-allowlist '("@example\\.com\\'"))
           (anvil-mu4e-send-log (expand-file-name "send.log" anvil-mu4e-drafts-dir))
           (d (anvil-mu4e-test--parse
               (anvil-mu4e--tool-compose-draft "alice@example.com" nil "Hi" "yo" nil)))
           (did (alist-get 'draft_id d))
           (r (anvil-mu4e-test--parse (anvil-mu4e--tool-send did t))))
      (should (eq t (alist-get 'sent r)))
      (should (= 1 (length calls)))
      (should (equal did (car calls)))
      (should (file-exists-p anvil-mu4e-send-log))
      (should (string-match-p
               "alice@example.com"
               (with-temp-buffer
                 (insert-file-contents anvil-mu4e-send-log)
                 (buffer-string)))))))

(ert-deftest anvil-mu4e-test-send-rejects-path-outside-drafts ()
  "A draft_id outside the drafts directory is rejected (path-injection guard)."
  (anvil-mu4e-test--with-drafts
    (let ((anvil-mu4e-allow-send t)
          (anvil-mu4e-send-allowlist '(".*"))
          (anvil-mu4e-send-function (lambda (_p) t)))
      (should-error (anvil-mu4e--tool-send "/etc/hostname" t) :type 'error))))

(ert-deftest anvil-mu4e-test-msmtp-sender-invokes-msmtp-with-t ()
  "anvil-mu4e-send-with-msmtp pipes the draft to `msmtp -t' and succeeds."
  (let ((calls '())
        (anvil-mu4e-msmtp-bin "msmtp"))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/msmtp"))
              ((symbol-function 'call-process)
               (lambda (prog infile _buffer _display &rest args)
                 (push (list prog infile args) calls) 0)))
      (should (anvil-mu4e-send-with-msmtp "/tmp/draft.eml"))
      (let ((c (car calls)))
        (should (string-match-p "msmtp" (nth 0 c)))
        (should (equal "/tmp/draft.eml" (nth 1 c)))
        (should (member "-t" (nth 2 c)))))))

(ert-deftest anvil-mu4e-test-msmtp-sender-errors-on-nonzero-exit ()
  "A non-zero msmtp exit raises rather than silently dropping the mail."
  (let ((anvil-mu4e-msmtp-bin "msmtp"))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/msmtp"))
              ((symbol-function 'call-process) (lambda (&rest _) 1)))
      (should-error (anvil-mu4e-send-with-msmtp "/tmp/draft.eml") :type 'error))))

(provide 'anvil-mu4e-test)
;;; anvil-mu4e-test.el ends here
