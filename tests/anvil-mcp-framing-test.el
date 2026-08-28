;;; anvil-mcp-framing-test.el --- ERT for MCP Content-Length framing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 zawatton

;; This file is part of anvil.el.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; T71: MCP stdio Content-Length framing tests.
;;
;; Covers:
;;   - encode roundtrip (helper symmetry)
;;   - parse Content-Length from header block
;;   - frame parse from string buffer (with body, partial, malformed)
;;   - multi-line / pretty-printed JSON body (newlines preserved)
;;   - case-insensitive header
;;   - UTF-8 byte length correctness for multi-byte characters
;;   - framing detection (legacy line-mode fallback)
;;   - frame parse: incomplete header (returns nil)
;;   - frame parse: incomplete body (returns nil)
;;   - frame parse: missing Content-Length (signals error)

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil-server)
(require 'anvil-server-commands)

;;;; --- Encode -----------------------------------------------------------

(ert-deftest anvil-mcp-framing-test-encode-basic ()
  "`anvil-server-mcp-frame-encode' produces Content-Length header + CRLF + body."
  (let* ((body "{\"jsonrpc\":\"2.0\",\"id\":1}")
         (frame (anvil-server-mcp-frame-encode body)))
    (should (stringp frame))
    (should (string-match-p "\\`Content-Length: 24\r\n\r\n" frame))
    (should (string-suffix-p body frame))))

(ert-deftest anvil-mcp-framing-test-encode-utf8-byte-length ()
  "Encode reports byte length, not character length, for multi-byte chars."
  ;; Lithuanian "ą" = 2 UTF-8 bytes; "あ" = 3 bytes.
  (let* ((body "{\"x\":\"ąあ\"}")
         (frame (anvil-server-mcp-frame-encode body))
         ;; 8 ASCII bytes ({"x":"" + "}) + 2 (ą) + 3 (あ) = 13 bytes
         (expected-byte-len (string-bytes body)))
    (should-not (multibyte-string-p frame))
    (should (string-match
             "\\`Content-Length: \\([0-9]+\\)\r\n\r\n"
             frame))
    (should (= expected-byte-len
               (string-to-number (match-string 1 frame))))))

(ert-deftest anvil-mcp-framing-test-utf8-helper-cjk-byte-count ()
  "The runtime-specific UTF-8 helper returns three bytes for one CJK char."
  (let ((bytes (anvil-server--string-to-utf8-bytes "あ")))
    (should-not (multibyte-string-p bytes))
    (should (= 3 (length bytes)))
    (should (equal '(227 129 130) (append bytes nil)))))

(ert-deftest anvil-mcp-framing-test-encode-rejects-non-string ()
  "Encode signals `wrong-type-argument' for non-string input."
  (should-error (anvil-server-mcp-frame-encode 42)
                :type 'wrong-type-argument)
  (should-error (anvil-server-mcp-frame-encode nil)
                :type 'wrong-type-argument))

;;;; --- Header parser ----------------------------------------------------

(ert-deftest anvil-mcp-framing-test-header-parse-basic ()
  "Parse Content-Length integer from header block."
  (should (= 42 (anvil-server-mcp-parse-content-length-header
                 "Content-Length: 42")))
  (should (= 42 (anvil-server-mcp-parse-content-length-header
                 (anvil-server--string-to-utf8-bytes
                  "Content-Length: 42")))))

(ert-deftest anvil-mcp-framing-test-header-parse-case-insensitive ()
  "Header name match is case-insensitive (RFC 7230)."
  (should (= 17 (anvil-server-mcp-parse-content-length-header
                 "content-length: 17")))
  (should (= 99 (anvil-server-mcp-parse-content-length-header
                 "CONTENT-LENGTH: 99"))))

(ert-deftest anvil-mcp-framing-test-header-parse-with-other-headers ()
  "Parse Content-Length when other headers present."
  (let ((block "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\nContent-Length: 128"))
    (should (= 128 (anvil-server-mcp-parse-content-length-header block)))))

(ert-deftest anvil-mcp-framing-test-header-parse-missing ()
  "Returns nil when Content-Length absent."
  (should-not (anvil-server-mcp-parse-content-length-header
               "Content-Type: text/plain")))

;;;; --- Frame string parser ---------------------------------------------

(ert-deftest anvil-mcp-framing-test-frame-parse-roundtrip ()
  "encode → parse roundtrips."
  (let* ((body "{\"jsonrpc\":\"2.0\",\"method\":\"ping\",\"id\":1}")
         (frame (anvil-server-mcp-frame-encode body))
         (parsed (anvil-server-mcp-frame-parse-string frame)))
    (should (plist-get parsed :body))
    (should (equal body (plist-get parsed :body)))
    (should (= (string-bytes frame)
               (plist-get parsed :consumed)))))

(ert-deftest anvil-mcp-framing-test-frame-parse-multiline-body ()
  "Multi-line / pretty-printed JSON body roundtrips byte-perfect."
  (let* ((body "{\n  \"jsonrpc\": \"2.0\",\n  \"id\": 1,\n  \"params\": {\n    \"k\": \"v\"\n  }\n}")
         (frame (anvil-server-mcp-frame-encode body))
         (parsed (anvil-server-mcp-frame-parse-string frame)))
    (should (equal body (plist-get parsed :body)))))

(ert-deftest anvil-mcp-framing-test-frame-parse-utf8-body ()
  "Multi-byte UTF-8 body roundtrips."
  (let* ((body "{\"msg\":\"こんにちはąć\"}")
         (frame (anvil-server-mcp-frame-encode body))
         (parsed (anvil-server-mcp-frame-parse-string frame)))
    (should (equal body (plist-get parsed :body)))))

(ert-deftest anvil-mcp-framing-test-frame-parse-utf8-byte-boundary ()
  "A byte Content-Length stops before trailing data after a CJK body."
  (let* ((body "{\"msg\":\"あ\"}")
         (header (format "Content-Length: %d\r\n\r\n" (string-bytes body)))
         (frame (concat header body "TAIL"))
         (parsed (anvil-server-mcp-frame-parse-string frame)))
    (should (equal body (plist-get parsed :body)))
    (should (= (+ (string-bytes header) (string-bytes body))
               (plist-get parsed :consumed)))))

(ert-deftest anvil-mcp-framing-test-frame-parse-incomplete-header ()
  "Returns nil when CRLFCRLF separator not yet seen."
  (should-not (anvil-server-mcp-frame-parse-string
               "Content-Length: 10\r\n"))
  (should-not (anvil-server-mcp-frame-parse-string ""))
  (should-not (anvil-server-mcp-frame-parse-string
               "Content-Length: 10")))

(ert-deftest anvil-mcp-framing-test-frame-parse-incomplete-body ()
  "Returns nil when body bytes not yet fully buffered."
  (should-not (anvil-server-mcp-frame-parse-string
               "Content-Length: 100\r\n\r\n{\"x\":1}")))

(ert-deftest anvil-mcp-framing-test-frame-parse-missing-content-length ()
  "Signals `anvil-mcp-frame-error' when header section lacks Content-Length."
  (should-error (anvil-server-mcp-frame-parse-string
                 "Foo: bar\r\n\r\n{\"x\":1}")
                :type 'anvil-mcp-frame-error))

(ert-deftest anvil-mcp-framing-test-frame-parse-trailing-data-not-consumed ()
  ":consumed only counts the first message; trailing bytes ignored."
  (let* ((body "{\"a\":1}")
         (frame1 (anvil-server-mcp-frame-encode body))
         (frame2 (anvil-server-mcp-frame-encode "{\"b\":2}"))
         (combined (concat frame1 frame2))
         (parsed (anvil-server-mcp-frame-parse-string combined)))
    (should (equal body (plist-get parsed :body)))
    (should (= (string-bytes frame1)
               (plist-get parsed :consumed)))
    ;; The remainder is exactly frame2.
    (let* ((rest (substring combined (plist-get parsed :consumed)))
           (parsed2 (anvil-server-mcp-frame-parse-string rest)))
      (should (equal "{\"b\":2}" (plist-get parsed2 :body))))))

;;;; --- Framing detection (legacy fallback sniff) ----------------------

(ert-deftest anvil-mcp-framing-test-detect-framed-input ()
  "Detector returns t for `Content-Length:' prefix (any case)."
  (should (anvil-server-mcp-detect-framing-p "Content-Length: 5"))
  (should (anvil-server-mcp-detect-framing-p "content-length: 5"))
  (should (anvil-server-mcp-detect-framing-p "  Content-Length: 5")))

(ert-deftest anvil-mcp-framing-test-detect-line-mode-input ()
  "Detector returns nil for raw JSON input (legacy line mode)."
  (should-not (anvil-server-mcp-detect-framing-p
               "{\"jsonrpc\":\"2.0\",\"id\":1}"))
  (should-not (anvil-server-mcp-detect-framing-p ""))
  (should-not (anvil-server-mcp-detect-framing-p "[1,2,3]")))

;;;; --- Sanity: define-error symbol present -----------------------------

(ert-deftest anvil-mcp-framing-test-frame-error-defined ()
  "`anvil-mcp-frame-error' is registered as an error symbol."
  (should (get 'anvil-mcp-frame-error 'error-conditions)))

(provide 'anvil-mcp-framing-test)
;;; anvil-mcp-framing-test.el ends here
