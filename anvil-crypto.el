;;; anvil-crypto.el --- web-security primitives (HMAC, tokens, webhooks) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Anvil contributors

;; Author: Anvil contributors
;; Keywords: crypto, hmac, security, web
;; Package-Requires: ((emacs "28.1"))
;; Version: 0.1.0

;;; Commentary:

;; The first framework primitives for serving web apps on the anvil /
;; NeLisp substrate: everything a request handler needs to trust its
;; inputs and its own tokens, built only on the runtime's `secure-hash'
;; (SHA-256), so the module is dual-target (runs under Emacs today and
;; under NeLisp standalone once the HTTP server substrate lands).
;;
;; Provided:
;;
;;   * `anvil-crypto-hmac-sha256'  - RFC 2104 HMAC over SHA-256 (raw or
;;     hex), verified against the RFC 4231 test vectors.
;;   * `anvil-crypto-constant-time-equal' - timing-safe string compare,
;;     for use on every secret/signature comparison so verification does
;;     not leak the position of the first mismatched byte.
;;   * `anvil-crypto-stripe-verify' - verify a `Stripe-Signature' header
;;     (t=<ts>,v1=<hmac>) against the raw request body and the endpoint
;;     signing secret, with a replay-tolerance window.
;;   * `anvil-crypto-sign-token' / `anvil-crypto-verify-token' - stateless
;;     signed session tokens (payload + expiry + HMAC), so sessions need
;;     no server-side store and a tampered/expired token is rejected.
;;   * `anvil-crypto-random-bytes' / `anvil-crypto-random-token' - CSPRNG
;;     bytes from the OS entropy source, for session ids and secrets.
;;
;; Security notes baked in (the point of "harden first, not later"):
;;   - all signature checks go through the constant-time compare;
;;   - Stripe verification rejects outside the tolerance window (replay);
;;   - tokens carry an expiry that verification enforces;
;;   - random-bytes reads the OS CSPRNG, never a PRNG seeded from time.

;;; Code:

(require 'cl-lib)

(defconst anvil-crypto--sha256-block 64
  "HMAC block size for SHA-256, in bytes.")

;;;; --- HMAC-SHA256 --------------------------------------------------------

(defun anvil-crypto-hmac-sha256 (key message &optional binary)
  "Return the RFC 2104 HMAC-SHA256 of MESSAGE under KEY.
KEY and MESSAGE are unibyte strings (raw bytes).  With BINARY non-nil
return the 32 raw bytes, otherwise the 64-char lowercase hex digest."
  (let* ((block anvil-crypto--sha256-block)
         (k (if (> (length key) block)
                (secure-hash 'sha256 key nil nil t)
              key)))
    (when (< (length k) block)
      (setq k (concat k (make-string (- block (length k)) 0))))
    (let ((ki (make-string block 0))
          (ko (make-string block 0)))
      (dotimes (i block)
        (aset ki i (logxor (aref k i) #x36))
        (aset ko i (logxor (aref k i) #x5c)))
      (let ((inner (secure-hash 'sha256 (concat ki message) nil nil t)))
        (secure-hash 'sha256 (concat ko inner) nil nil binary)))))

;;;; --- constant-time compare ---------------------------------------------

(defun anvil-crypto-constant-time-equal (a b)
  "Return non-nil iff strings A and B are equal, in constant time.
The running time depends only on the length of A, not on where the
first differing byte is, so it does not leak signature bytes through
timing.  Different lengths return nil (but still scan A)."
  (let ((diff (if (= (length a) (length b)) 0 1))
        (la (length a))
        (lb (length b)))
    (dotimes (i la)
      (setq diff (logior diff
                         (logxor (aref a i)
                                 (if (< i lb) (aref b i) 0)))))
    (= diff 0)))

;;;; --- Stripe webhook signatures -----------------------------------------

(defun anvil-crypto--parse-stripe-header (header)
  "Parse a `Stripe-Signature' HEADER into (TIMESTAMP . (V1 ...)).
Returns a plist (:t TIMESTAMP-STRING :v1 (SIG ...))."
  (let (ts v1)
    (dolist (part (split-string header "," t "[ \t]+"))
      (let ((kv (split-string part "=")))
        (cond ((string= (car kv) "t") (setq ts (cadr kv)))
              ((string= (car kv) "v1") (push (cadr kv) v1)))))
    (list :t ts :v1 (nreverse v1))))

(cl-defun anvil-crypto-stripe-verify (payload header secret &key (tolerance 300) now)
  "Verify a Stripe webhook.
PAYLOAD is the raw request body string, HEADER the `Stripe-Signature'
header value, SECRET the endpoint signing secret (whsec_...).  Returns
t when a v1 signature matches HMAC-SHA256 of \"TIMESTAMP.PAYLOAD\" and
the timestamp is within TOLERANCE seconds of NOW (default current
time).  Rejects on missing parts, bad signature, or replay outside the
window."
  (let* ((parsed (anvil-crypto--parse-stripe-header header))
         (ts (plist-get parsed :t))
         (sigs (plist-get parsed :v1))
         (now (or now (float-time)))
         (signed (and ts (concat ts "." payload)))
         (expected (and signed (anvil-crypto-hmac-sha256 secret signed))))
    (and ts sigs expected
         ;; timing-safe match against any provided v1 signature
         (cl-some (lambda (s) (anvil-crypto-constant-time-equal s expected)) sigs)
         ;; replay window
         (<= (abs (- now (string-to-number ts))) tolerance)
         t)))

;;;; --- stateless signed tokens -------------------------------------------

(defun anvil-crypto--b64url (bytes)
  "Base64url-encode BYTES with no padding."
  (let ((s (base64-encode-string bytes t)))
    (setq s (replace-regexp-in-string "+" "-" s))
    (setq s (replace-regexp-in-string "/" "_" s))
    (replace-regexp-in-string "=+$" "" s)))

(defun anvil-crypto--b64url-decode (s)
  "Decode base64url string S (no padding)."
  (setq s (replace-regexp-in-string "-" "+" s))
  (setq s (replace-regexp-in-string "_" "/" s))
  (let ((pad (% (length s) 4)))
    (when (> pad 0) (setq s (concat s (make-string (- 4 pad) ?=)))))
  (base64-decode-string s))

(cl-defun anvil-crypto-sign-token (payload secret &key (ttl 3600) now)
  "Return a signed, expiring token carrying PAYLOAD (a string).
The token is \"B64URL(payload).EXPIRY.HMAC\" where EXPIRY is NOW+TTL
seconds and HMAC covers \"B64URL(payload).EXPIRY\".  Stateless: no
server-side session store is needed."
  (let* ((now (or now (float-time)))
         (expiry (number-to-string (truncate (+ now ttl))))
         (body (concat (anvil-crypto--b64url payload) "." expiry))
         (sig (anvil-crypto-hmac-sha256 secret body)))
    (concat body "." sig)))

(cl-defun anvil-crypto-verify-token (token secret &key now)
  "Verify TOKEN produced by `anvil-crypto-sign-token'.
Returns the decoded payload string when the signature is valid (checked
in constant time) and the token has not expired, else nil."
  (let ((parts (split-string token "\\.")))
    (when (= (length parts) 3)
      (let* ((b64 (nth 0 parts))
             (expiry (nth 1 parts))
             (sig (nth 2 parts))
             (body (concat b64 "." expiry))
             (expected (anvil-crypto-hmac-sha256 secret body))
             (now (or now (float-time))))
        (when (and (anvil-crypto-constant-time-equal sig expected)
                   (<= now (string-to-number expiry)))
          (condition-case nil
              (anvil-crypto--b64url-decode b64)
            (error nil)))))))

;;;; --- CSPRNG -------------------------------------------------------------

(defun anvil-crypto-random-bytes (n)
  "Return N cryptographically-random bytes from the OS entropy source.
Reads /dev/urandom, which is non-seekable, so N bytes are streamed via
`head' rather than a byte-range read.  (Under NeLisp standalone this is
the one spot that needs a getrandom syscall / urandom read; every
caller above is substrate-agnostic.)"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'binary)
          (coding-system-for-write 'binary))
      (unless (zerop (call-process "head" "/dev/urandom" t nil
                                   "-c" (number-to-string n)))
        (error "anvil-crypto: could not read %d bytes from /dev/urandom" n)))
    (let ((bytes (buffer-string)))
      (unless (= (length bytes) n)
        (error "anvil-crypto: short CSPRNG read (%d/%d)" (length bytes) n))
      bytes)))

(defun anvil-crypto-random-token (&optional n)
  "Return a base64url random token from N (default 32) CSPRNG bytes."
  (anvil-crypto--b64url (anvil-crypto-random-bytes (or n 32))))

(provide 'anvil-crypto)
;;; anvil-crypto.el ends here
