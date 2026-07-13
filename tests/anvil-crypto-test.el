;;; anvil-crypto-test.el --- Tests for anvil-crypto -*- lexical-binding: t; -*-

(require 'ert)
(require 'anvil-crypto)

;;;; --- HMAC-SHA256 against RFC 4231 vectors -------------------------------

(ert-deftest anvil-crypto-hmac-rfc4231-case1 ()
  (should (string=
           (anvil-crypto-hmac-sha256 (make-string 20 #x0b) "Hi There")
           "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7")))

(ert-deftest anvil-crypto-hmac-rfc4231-case2 ()
  (should (string=
           (anvil-crypto-hmac-sha256 "Jefe" "what do ya want for nothing?")
           "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843")))

(ert-deftest anvil-crypto-hmac-rfc4231-case6-long-key ()
  ;; key longer than the block (131 x 0xaa) is hashed down first; the
  ;; key is a unibyte byte string (a raw HTTP secret would be), not a
  ;; multibyte string -- the byte>=128 distinction the hardening cares
  ;; about
  (should (string=
           (anvil-crypto-hmac-sha256
            (apply #'unibyte-string (make-list 131 #xaa))
            "Test Using Larger Than Block-Size Key - Hash Key First")
           "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54")))

(ert-deftest anvil-crypto-hmac-binary-length ()
  (should (= 32 (length (anvil-crypto-hmac-sha256 "k" "m" t)))))

;;;; --- constant-time compare ---------------------------------------------

(ert-deftest anvil-crypto-constant-time-equal-basic ()
  (should (anvil-crypto-constant-time-equal "abc" "abc"))
  (should-not (anvil-crypto-constant-time-equal "abc" "abd"))
  (should-not (anvil-crypto-constant-time-equal "abc" "abcd"))
  (should-not (anvil-crypto-constant-time-equal "abc" "")))

;;;; --- Stripe webhook verification ---------------------------------------

(defun anvil-crypto-test--stripe-header (payload secret ts)
  "Build a valid Stripe-Signature header for PAYLOAD at TS."
  (let ((sig (anvil-crypto-hmac-sha256 secret (concat ts "." payload))))
    (format "t=%s,v1=%s" ts sig)))

(ert-deftest anvil-crypto-stripe-verify-valid ()
  (let* ((payload "{\"id\":\"evt_1\",\"type\":\"checkout.session.completed\"}")
         (secret "whsec_test_secret")
         (ts "1720900000")
         (header (anvil-crypto-test--stripe-header payload secret ts)))
    (should (anvil-crypto-stripe-verify payload header secret
                                        :now 1720900010))))

(ert-deftest anvil-crypto-stripe-verify-tampered-payload ()
  (let* ((payload "{\"amount\":100}")
         (secret "whsec_test_secret")
         (ts "1720900000")
         (header (anvil-crypto-test--stripe-header payload secret ts)))
    ;; body changed after signing -> reject
    (should-not (anvil-crypto-stripe-verify "{\"amount\":9999}" header secret
                                            :now 1720900010))))

(ert-deftest anvil-crypto-stripe-verify-replay-outside-window ()
  (let* ((payload "{\"id\":\"evt_2\"}")
         (secret "whsec_test_secret")
         (ts "1720900000")
         (header (anvil-crypto-test--stripe-header payload secret ts)))
    ;; correct signature but 10 minutes late -> reject (default 300s)
    (should-not (anvil-crypto-stripe-verify payload header secret
                                            :now 1720900600))
    ;; widen tolerance -> accept
    (should (anvil-crypto-stripe-verify payload header secret
                                        :now 1720900600 :tolerance 900))))

(ert-deftest anvil-crypto-stripe-verify-wrong-secret ()
  (let* ((payload "{\"id\":\"evt_3\"}")
         (ts "1720900000")
         (header (anvil-crypto-test--stripe-header payload "whsec_real" ts)))
    (should-not (anvil-crypto-stripe-verify payload header "whsec_attacker"
                                            :now 1720900010))))

;;;; --- signed tokens -----------------------------------------------------

(ert-deftest anvil-crypto-token-roundtrip ()
  (let* ((secret "sess_secret")
         (tok (anvil-crypto-sign-token "user=42;role=admin" secret
                                       :ttl 3600 :now 1000)))
    (should (string= (anvil-crypto-verify-token tok secret :now 2000)
                     "user=42;role=admin"))))

(ert-deftest anvil-crypto-token-expired ()
  (let* ((secret "sess_secret")
         (tok (anvil-crypto-sign-token "x" secret :ttl 60 :now 1000)))
    (should-not (anvil-crypto-verify-token tok secret :now 2000))))

(ert-deftest anvil-crypto-token-tampered ()
  (let* ((secret "sess_secret")
         (tok (anvil-crypto-sign-token "role=user" secret :ttl 3600 :now 1000))
         ;; flip the payload segment
         (parts (split-string tok "\\."))
         (forged (concat (anvil-crypto--b64url "role=admin") "."
                         (nth 1 parts) "." (nth 2 parts))))
    (should-not (anvil-crypto-verify-token forged secret :now 1500))))

(ert-deftest anvil-crypto-token-wrong-secret ()
  (let ((tok (anvil-crypto-sign-token "x" "real" :ttl 3600 :now 1000)))
    (should-not (anvil-crypto-verify-token tok "attacker" :now 1500))))

;;;; --- CSPRNG ------------------------------------------------------------

(ert-deftest anvil-crypto-random-bytes-length-and-uniqueness ()
  (should (= 32 (length (anvil-crypto-random-bytes 32))))
  ;; two draws must differ (P(collision) negligible)
  (should-not (string= (anvil-crypto-random-bytes 32)
                       (anvil-crypto-random-bytes 32))))

(ert-deftest anvil-crypto-random-token-format ()
  (let ((tok (anvil-crypto-random-token 32)))
    ;; base64url, no padding, no + / =
    (should (string-match-p "\\`[A-Za-z0-9_-]+\\'" tok))))

;;; anvil-crypto-test.el ends here
