;;; anvil-file-test.el --- Tests for anvil-file helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for json-object-add, file-ensure-import, and file-batch-across.
;; Uses temporary files; no dependency on anvil-server registration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-file)

(defun anvil-file-test--with-tmp (content fn)
  "Write CONTENT to a temp file, call FN with its path, then clean up."
  (let ((path (make-temp-file "anvil-file-test-" nil ".txt")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region content nil path nil 'silent))
          (funcall fn path))
      (when (file-exists-p path) (delete-file path)))))

(defun anvil-file-test--read (path)
  "Return PATH's contents as a UTF-8 string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents path))
    (buffer-string)))

(defun anvil-file-test--discard-buffer (buf)
  "Kill visited temp BUF without interactive modified-buffer prompts."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (set-buffer-modified-p nil))
    (kill-buffer buf)))

(defmacro anvil-file-test--with-delta-cache (bindings &rest body)
  "Run BODY with a fresh delta cache and optional BINDINGS.
BINDINGS is a `let' binding list for delta-cache defcustoms."
  (declare (indent 1) (debug (sexp body)))
  `(let ((anvil-file--delta-cache (make-hash-table :test 'equal))
         (anvil-file--delta-cache-order nil)
         ,@bindings)
     ,@body))

(defun anvil-file-test--serialize (value)
  "Serialize VALUE with `format' for MCP-size assertions."
  (format "%S" value))

(defun anvil-file-test--apply-unified-diff (old-body diff)
  "Apply unified DIFF to OLD-BODY with external `patch' and return the result."
  (let ((path (make-temp-file "anvil-file-delta-patch-" nil ".txt")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region old-body nil path nil 'silent))
          (with-temp-buffer
            (insert diff)
            (let ((status (call-process-region (point-min) (point-max)
                                               "patch" nil nil nil
                                               "--quiet" path)))
              (should (eq 0 status))))
          (anvil-file-test--read path))
      (when (file-exists-p path) (delete-file path)))))

(defun anvil-file-test--write (path content)
  "Overwrite PATH with CONTENT using UTF-8."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil path nil 'silent)))

(defun anvil-file-test--error-text (thunk)
  "Call THUNK and return the printed condition from its expected error."
  (condition-case err
      (progn
        (funcall thunk)
        (ert-fail "expected an error"))
    (error (format "%S" err))))

;;;; --- json-object-add ------------------------------------------------------

(ert-deftest anvil-file-test-json-add-empty-object ()
  "Add pairs to an empty JSON object."
  (anvil-file-test--with-tmp
   "{}\n"
   (lambda (path)
     (let ((res (anvil-json-object-add
                 path '(("hello" . "world") ("foo" . "bar")))))
       (should (= 2 (plist-get res :added)))
       (let ((parsed (json-parse-string
                      (anvil-file-test--read path)
                      :object-type 'alist)))
         (should (equal "world" (alist-get 'hello parsed)))
         (should (equal "bar" (alist-get 'foo parsed))))))))

(ert-deftest anvil-file-test-json-add-populated-object ()
  "Add pairs to an object that already has entries; trailing comma auto-added."
  (anvil-file-test--with-tmp
   "{\n  \"existing\": \"yes\"\n}\n"
   (lambda (path)
     (let ((res (anvil-json-object-add
                 path '(("new1" . "a") ("new2" . "b")))))
       (should (= 2 (plist-get res :added)))
       (let ((parsed (json-parse-string
                      (anvil-file-test--read path)
                      :object-type 'alist)))
         (should (equal "yes" (alist-get 'existing parsed)))
         (should (equal "a" (alist-get 'new1 parsed)))
         (should (equal "b" (alist-get 'new2 parsed))))))))

(ert-deftest anvil-file-test-json-duplicate-skip ()
  "Duplicate keys are skipped by default."
  (anvil-file-test--with-tmp
   "{\n  \"k\": \"v1\"\n}\n"
   (lambda (path)
     (let ((res (anvil-json-object-add
                 path '(("k" . "v2") ("new" . "x")))))
       (should (= 1 (plist-get res :added)))
       (should (= 1 (plist-get res :skipped)))
       ;; Original value preserved
       (let ((parsed (json-parse-string
                      (anvil-file-test--read path)
                      :object-type 'alist)))
         (should (equal "v1" (alist-get 'k parsed))))))))

(ert-deftest anvil-file-test-json-duplicate-overwrite ()
  "on-duplicate 'overwrite replaces existing values."
  (anvil-file-test--with-tmp
   "{\n  \"k\": \"v1\",\n  \"other\": \"z\"\n}\n"
   (lambda (path)
     (let ((res (anvil-json-object-add
                 path '(("k" . "v2"))
                 '(:on-duplicate overwrite))))
       (should (= 1 (plist-get res :overwritten)))
       (let ((parsed (json-parse-string
                      (anvil-file-test--read path)
                      :object-type 'alist)))
         (should (equal "v2" (alist-get 'k parsed)))
         (should (equal "z" (alist-get 'other parsed))))))))

(ert-deftest anvil-file-test-json-duplicate-error ()
  "on-duplicate 'error raises an error."
  (anvil-file-test--with-tmp
   "{\n  \"k\": \"v\"\n}\n"
   (lambda (path)
     (should-error
      (anvil-json-object-add
       path '(("k" . "v2"))
       '(:on-duplicate error))))))

(ert-deftest anvil-file-test-json-escape ()
  "Keys and values with special chars are JSON-escaped."
  (anvil-file-test--with-tmp
   "{}\n"
   (lambda (path)
     (anvil-json-object-add
      path
      '(("with\"quote" . "has\\backslash and \"quotes\"")
        ("newlines" . "line1\nline2")))
     ;; Use hash-table form so keys can be arbitrary strings
     (let ((parsed (json-parse-string
                    (anvil-file-test--read path)
                    :object-type 'hash-table)))
       (should (equal "has\\backslash and \"quotes\""
                      (gethash "with\"quote" parsed)))
       (should (equal "line1\nline2"
                      (gethash "newlines" parsed)))))))

(ert-deftest anvil-file-test-json-indent-detection ()
  "Indentation is detected from the first existing entry."
  (anvil-file-test--with-tmp
   "{\n    \"a\": \"1\"\n}\n"     ; 4-space indent
   (lambda (path)
     (anvil-json-object-add path '(("b" . "2")))
     (let ((content (anvil-file-test--read path)))
       (should (string-match-p "^    \"b\"" content))))))

;;;; --- file-ensure-import --------------------------------------------------

(ert-deftest anvil-file-test-ensure-import-already-present ()
  "No-op when the line already exists."
  (anvil-file-test--with-tmp
   "import foo from 'foo';\nimport bar from 'bar';\n\nconst x = 1;\n"
   (lambda (path)
     (let ((res (anvil-file-ensure-import
                 path "import foo from 'foo';")))
       (should (plist-get res :already-present))
       (should-not (plist-get res :inserted))))))

(ert-deftest anvil-file-test-ensure-import-insert-after-last ()
  "Insert after the last matching import line."
  (anvil-file-test--with-tmp
   "import a from 'a';\nimport b from 'b';\n\nconst x = 1;\n"
   (lambda (path)
     (let ((res (anvil-file-ensure-import
                 path "import c from 'c';")))
       (should (plist-get res :inserted))
       (let ((content (anvil-file-test--read path)))
         ;; New import appears right after 'import b'
         (should (string-match-p
                  "import b from 'b';\nimport c from 'c';"
                  content)))))))

(ert-deftest anvil-file-test-ensure-import-no-match-inserts-top ()
  "Insert at top when no import lines exist."
  (anvil-file-test--with-tmp
   "const x = 1;\n"
   (lambda (path)
     (let ((res (anvil-file-ensure-import
                 path "import a from 'a';")))
       (should (plist-get res :inserted))
       (should (= 1 (plist-get res :line)))
       (should (string-prefix-p "import a from 'a';\n"
                                (anvil-file-test--read path)))))))

(ert-deftest anvil-file-test-ensure-import-custom-regex ()
  "Custom :after-regex allows non-import section headers."
  (anvil-file-test--with-tmp
   "(require 'cl-lib)\n(require 'subr-x)\n\n(defun foo () nil)\n"
   (lambda (path)
     (anvil-file-ensure-import
      path "(require 'json)"
      '(:after-regex "^(require "))
     (let ((content (anvil-file-test--read path)))
       (should (string-match-p
                "(require 'subr-x)\n(require 'json)"
                content))))))

;;;; --- file-batch-across ---------------------------------------------------

(ert-deftest anvil-file-test-batch-across-basic ()
  "Run simple replace across two files."
  (let ((p1 (make-temp-file "abf-" nil ".txt"))
        (p2 (make-temp-file "abf-" nil ".txt")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region "hello foo\n" nil p1 nil 'silent)
            (write-region "world foo\n" nil p2 nil 'silent))
          (let ((res (anvil-file-batch-across
                      `(((path . ,p1)
                         (operations . (((op . "replace")
                                         (old . "foo")
                                         (new . "BAR")))))
                        ((path . ,p2)
                         (operations . (((op . "replace")
                                         (old . "foo")
                                         (new . "BAZ")))))))))
            (should (= 2 (plist-get res :files)))
            (should (= 2 (plist-get res :succeeded)))
            (should (= 0 (plist-get res :failed)))
            (should (equal "hello BAR\n" (anvil-file-test--read p1)))
            (should (equal "world BAZ\n" (anvil-file-test--read p2)))))
      (when (file-exists-p p1) (delete-file p1))
      (when (file-exists-p p2) (delete-file p2)))))

(ert-deftest anvil-file-test-batch-across-partial-failure ()
  "One file's failure does not block the others."
  (let ((p1 (make-temp-file "abf-" nil ".txt"))
        (p2 (make-temp-file "abf-" nil ".txt")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region "no match here\n" nil p1 nil 'silent)
            (write-region "has token\n" nil p2 nil 'silent))
          (let ((res (anvil-file-batch-across
                      `(((path . ,p1)
                         (operations . (((op . "replace")
                                         (old . "NOPE")
                                         (new . "X")))))
                        ((path . ,p2)
                         (operations . (((op . "replace")
                                         (old . "token")
                                         (new . "DONE")))))))))
            (should (= 1 (plist-get res :succeeded)))
            (should (= 1 (plist-get res :failed)))
            (should (equal "has DONE\n" (anvil-file-test--read p2)))))
      (when (file-exists-p p1) (delete-file p1))
      (when (file-exists-p p2) (delete-file p2)))))

;;;; --- :warnings integration (Doc 05 Phase 2) -----------------------------

(ert-deftest anvil-file-test-unbounded-read-cap ()
  "Unbounded reads reject over-cap regular files without loading their bodies."
  (should (= 1048576 anvil-file-max-inline-read-bytes))
  (let* ((payload (concat "private-" (make-string 9 ?x)))
         (path (make-temp-file "anvil-file-cap-" nil ".txt"))
         (link (concat path "-link"))
         (literal-insert (symbol-function 'insert-file-contents-literally))
         (max-request 0)
         (max-retained 0))
    (should (= 17 (string-bytes payload)))
    (unwind-protect
        (progn
          (anvil-file-test--write path payload)
          (make-symbolic-link path link)
          (let ((anvil-file-max-inline-read-bytes 16))
            (cl-letf (((symbol-function 'anvil--insert-file)
                       (lambda (&rest _)
                         (ert-fail "unbounded full-body loader was called")))
                      ((symbol-function 'insert-file-contents-literally)
                       (lambda (filename &optional visit beg end replace)
                         (setq max-request
                               (max max-request (- (or end 0) (or beg 0))))
                         (prog1
                             (funcall literal-insert filename visit beg end replace)
                           (setq max-retained (max max-retained (buffer-size)))))))
              (dolist (candidate (list path link))
                (dolist (offset '(nil 1))
                  (let ((message
                         (anvil-file-test--error-text
                          (lambda ()
                            (anvil-file-read candidate offset nil)))))
                    (should (string-match-p "maximum 16 bytes" message))
                    (should (string-match-p "offset=0" message))
                    (should (string-match-p "limit=200" message))
                    (should-not (string-match-p (regexp-quote payload) message))
                    (should-not
                     (string-match-p (regexp-quote candidate) message)))))))
          ;; A stale preliminary size must not authorize a cap-plus-one body.
          (let ((anvil-file-max-inline-read-bytes 16))
            (cl-letf (((symbol-function 'anvil--insert-file)
                       (lambda (&rest _)
                         (ert-fail "unbounded full-body loader was called")))
                      ;; Mock Anvil's generation boundary rather than the
                      ;; byte-compiler-inlinable file-attribute accessor.
                      ((symbol-function 'anvil-file--file-generation)
                       (lambda (_target)
                         '(:identity (1 . 2) :size 16 :mtime (0 0 0 0))))
                      ((symbol-function 'insert-file-contents-literally)
                       (lambda (filename &optional visit beg end replace)
                         (setq max-request
                               (max max-request (- (or end 0) (or beg 0))))
                         (prog1
                             (funcall literal-insert filename visit beg end replace)
                           (setq max-retained (max max-retained (buffer-size)))))))
              (let ((message
                     (anvil-file-test--error-text
                      (lambda () (anvil-file-read path)))))
                (should (string-match-p "at least 17 bytes" message))
                (should (string-match-p "offset=0" message))
                (should (string-match-p "limit=200" message))
                (should-not (string-match-p (regexp-quote payload) message)))))
          (should (<= max-request 17))
          (should (<= max-retained 17)))
      (ignore-errors (delete-file link))
      (ignore-errors (delete-file path)))))

(ert-deftest anvil-file-test-read-argument-validation ()
  "Direct and MCP reads accept only valid integer pagination arguments."
  (let ((body-touched nil))
    (cl-letf (((symbol-function 'anvil--prepare-path)
               (lambda (&rest _)
                 (setq body-touched t)
                 (ert-fail "path preparation reached for an invalid range")))
              ((symbol-function 'anvil--insert-file)
               (lambda (&rest _)
                 (setq body-touched t)
                 (ert-fail "file body reached for an invalid range"))))
      (dolist (offset '(0.5 -1 "0"))
        (should-error (anvil-file-read "/unused" offset 1)))
      (dolist (limit '(0.5 0 -1 "1"))
        (should-error (anvil-file-read "/unused" 0 limit)))
      (should-not body-touched)))
  (let ((calls nil))
    (cl-letf (((symbol-function 'anvil-file-read)
               (lambda (path &optional offset limit)
                 (push (list path offset limit) calls)
                 '(:ok t))))
      (dolist (limit '("1.0" "0" "-1" "+1" "1x" "１"))
        (should-error (anvil-file--tool-read "/unused" "0" limit)))
      (dolist (offset '("0.5" "-1" "+0" "0x" "０"))
        (should-error (anvil-file--tool-read "/unused" offset "1")))
      (should-not calls)
      (should (string-match-p
               ":ok t"
               (anvil-file--tool-read "/unused" "0" "1")))
      (should (equal '(("/unused" 0 1)) calls)))))

(ert-deftest anvil-file-test-read-limit-boundaries ()
  "Inline caps and streamed pages preserve boundaries without full loads."
  (should (= 1048576 anvil-file-max-inline-read-bytes))
  (should (= 65536 anvil-file--stream-chunk-bytes))
  (should (= 16 anvil-file--stream-yield-chunks))
  (should (= 0.001 anvil-file--stream-yield-seconds))
  (let ((path (make-temp-file "anvil-file-identity-")))
    (unwind-protect
        (let* ((attributes (file-attributes path 'integer))
               (fallback
                (list (file-attribute-inode-number attributes)
                      (file-attribute-device-number attributes))))
          (should (equal fallback
                         (anvil-file--attribute-identity attributes)))
          (cl-letf (((symbol-function
                      'file-attribute-file-identifier)
                     nil))
            (should (equal fallback
                           (anvil-file--attribute-identity attributes)))))
      (delete-file path)))
  ;; Exact-cap and small UTF-8 bodies survive; cap-plus-one does not.
  (dolist (body '("0123456789abcdef" "hé🙂\n"))
    (anvil-file-test--with-tmp
     body
     (lambda (path)
       (let* ((anvil-file-max-inline-read-bytes 16)
              (result (anvil-file-read path)))
         (should (equal body (plist-get result :content)))))))
  (anvil-file-test--with-tmp
   "0123456789abcdefg"
   (lambda (path)
     (let ((anvil-file-max-inline-read-bytes 16))
       (should-error (anvil-file-read path)))))

  ;; Disabling the cap preserves the legacy loader for bounded and unbounded
  ;; requests alike.
  (anvil-file-test--with-tmp
   "first\nsecond\n"
   (lambda (path)
     (let ((loader (symbol-function 'anvil--insert-file))
           (loads 0))
       (cl-letf (((symbol-function 'anvil--insert-file)
                  (lambda (target)
                    (cl-incf loads)
                    (funcall loader target))))
         (dolist (cap '(nil 0 -1))
           (let ((anvil-file-max-inline-read-bytes cap))
             (should (equal "first\nsecond\n"
                            (plist-get (anvil-file-read path) :content)))
             (should (equal "first\n"
                            (plist-get (anvil-file-read path 0 1) :content)))))
         (should (= 6 loads))))))

  ;; A UTF-8 character straddles the fixed raw-chunk boundary.  Pagination
  ;; still returns exact lines and never requests or retains too much data.
  (let* ((first (concat (make-string (1- anvil-file--stream-chunk-bytes) ?a)
                        "🙂\n"))
         (body (concat first "second\n\nlast"))
         (initial-size (string-bytes body)))
    (anvil-file-test--with-tmp
     body
     (lambda (path)
       (let ((anvil-file-max-inline-read-bytes 70000)
             (literal-insert (symbol-function 'insert-file-contents-literally))
             (buffer-insert (symbol-function 'insert-buffer-substring))
             (max-chunk 0)
             (max-page 0)
             (requests nil))
         (cl-letf (((symbol-function 'anvil--insert-file)
                    (lambda (&rest _)
                      (ert-fail "streamed pagination called the full loader")))
                   ((symbol-function 'insert-file-contents-literally)
                    (lambda (filename &optional visit beg end replace)
                      (push (cons beg end) requests)
                      (prog1
                          (funcall literal-insert filename visit beg end replace)
                        (setq max-chunk (max max-chunk (buffer-size))))))
                   ((symbol-function 'insert-buffer-substring)
                    (lambda (buffer &optional start end)
                      (prog1
                          (funcall buffer-insert buffer start end)
                        (setq max-page (max max-page (buffer-size)))))))
           (let ((whole (anvil-file-read path 0 200))
                 (one (anvil-file-read path 0 1))
                 (empty (anvil-file-read path 2 1))
                 (last (anvil-file-read path 3 1))
                 (past (anvil-file-read path 10 1)))
             (should (equal body (plist-get whole :content)))
             (should (= 4 (plist-get whole :total-lines)))
             (should (= 4 (plist-get whole :lines-returned)))
             (should (equal first (plist-get one :content)))
             (should (= 1 (plist-get one :lines-returned)))
             (should (equal "\n" (plist-get empty :content)))
             (should (= 1 (plist-get empty :lines-returned)))
             (should (equal "last" (plist-get last :content)))
             (should (= 1 (plist-get last :lines-returned)))
             (should (equal "" (plist-get past :content)))
             (should (= 0 (plist-get past :lines-returned))))
           (should requests)
           (dolist (range requests)
             (should (<= (- (cdr range) (car range))
                         anvil-file--stream-chunk-bytes))
             (should (<= (cdr range) initial-size)))
           (should (<= max-chunk anvil-file--stream-chunk-bytes))
           (should (<= max-page (1+ anvil-file-max-inline-read-bytes)))
           (should (<= (+ max-chunk max-page)
                       (+ anvil-file--stream-chunk-bytes
                          1 anvil-file-max-inline-read-bytes))))))))

  ;; A selected page may itself be too large, but its body never enters the
  ;; diagnostic.
  (anvil-file-test--with-tmp
   "SECRETS!\nok\n"
   (lambda (path)
     (let* ((anvil-file-max-inline-read-bytes 8)
            (message
             (anvil-file-test--error-text
              (lambda () (anvil-file-read path 0 1)))))
       (should (string-match-p "lower.*limit" message))
       (should (string-match-p "filtered.*region" message))
       (should-not (string-match-p "SECRETS!" message)))))

  ;; Growth and replacement during a scan are content-free retry failures,
  ;; and the frozen initial size remains the maximum requested byte offset.
  (dolist (change '(grow replace))
    (anvil-file-test--with-tmp
     "original\nbody\n"
     (lambda (path)
       (let* ((anvil-file-max-inline-read-bytes 64)
              (initial-size (file-attribute-size (file-attributes path)))
              (literal-insert (symbol-function 'insert-file-contents-literally))
              (changed nil)
              (max-end 0)
              (message
               (cl-letf (((symbol-function 'insert-file-contents-literally)
                          (lambda (filename &optional visit beg end replace-buffer)
                            (setq max-end (max max-end (or end 0)))
                            (prog1
                                (funcall literal-insert filename visit beg end
                                         replace-buffer)
                              (unless changed
                                (setq changed t)
                                (pcase change
                                  ('grow
                                   (let ((coding-system-for-write 'utf-8-unix))
                                     (write-region "GROWTH-SECRET\n" nil filename
                                                   t 'silent)))
                                  ('replace
                                   (let ((replacement
                                          (make-temp-file
                                           (file-name-nondirectory filename)
                                           nil ".replacement")))
                                     (unwind-protect
                                         (progn
                                           (anvil-file-test--write
                                            replacement "REPLACEMENT-SECRET\n")
                                           (rename-file replacement filename t))
                                       (ignore-errors
                                         (delete-file replacement)))))))))))
                 (anvil-file-test--error-text
                  (lambda () (anvil-file-read path 0 1))))))
         (should (string-match-p "changed.*retry" message))
         (should-not (string-match-p "SECRET" message))
         (should (<= max-end initial-size))))))

  ;; Seventeen fixed chunks force the cooperative yield, which must let an
  ;; already-ready timer run even when the requested page is beyond EOF.
  (anvil-file-test--with-tmp
   (make-string (* 17 anvil-file--stream-chunk-bytes) ?z)
   (lambda (path)
     (let ((anvil-file-max-inline-read-bytes 8)
           (fired nil)
           (timer nil))
       (unwind-protect
           (progn
             (setq timer (run-at-time 0 nil (lambda () (setq fired t))))
             (let ((result (anvil-file-read path 1 1)))
               (should (equal "" (plist-get result :content)))
               (should (= 1 (plist-get result :total-lines))))
             (should fired))
         (when timer (cancel-timer timer)))))))

(ert-deftest anvil-file-test-page-overflow-stops-stream ()
  "A rejected page stops before later chunks or cooperative yields."
  (let ((body (concat "SECRETS!\n"
                      (make-string
                       (* 17 anvil-file--stream-chunk-bytes) ?z))))
    (anvil-file-test--with-tmp
     body
     (lambda (path)
       (let ((anvil-file-max-inline-read-bytes 8)
             (literal-insert
              (symbol-function 'insert-file-contents-literally))
             (generate-buffer (symbol-function 'generate-new-buffer))
             (signal-overflow
              (symbol-function 'anvil-file--signal-page-overflow))
             page-buffer
             chunk-buffer
             retained-at-signal
             (reads 0)
             (yields 0))
         (cl-letf (((symbol-function 'anvil--insert-file)
                    (lambda (&rest _)
                      (ert-fail "page overflow called the full loader")))
                   ((symbol-function 'generate-new-buffer)
                    (lambda (name &rest arguments)
                      (let ((buffer (apply generate-buffer name arguments)))
                        (cond
                         ((equal name " *anvil-file-page*")
                          (setq page-buffer buffer))
                         ((equal name " *anvil-file-chunk*")
                          (setq chunk-buffer buffer)))
                        buffer)))
                   ((symbol-function 'insert-file-contents-literally)
                    (lambda (filename &optional visit beg end replace)
                      (cl-incf reads)
                      (funcall literal-insert
                               filename visit beg end replace)))
                   ((symbol-function 'accept-process-output)
                    (lambda (&rest _)
                      (cl-incf yields)
                      nil))
                   ((symbol-function 'anvil-file--signal-page-overflow)
                    (lambda (cap)
                      (setq retained-at-signal
                            (list
                             (with-current-buffer page-buffer (buffer-size))
                             (with-current-buffer chunk-buffer (buffer-size))))
                      (funcall signal-overflow cap))))
           (let ((message
                  (anvil-file-test--error-text
                   (lambda () (anvil-file-read path 0 1)))))
             (should (string-match-p "lower.*limit" message))
             (should-not (string-match-p "SECRETS!" message))))
         (should (= 1 reads))
         (should (= 0 yields))
         (should (equal '(0 0) retained-at-signal))
         (should-not (buffer-live-p page-buffer))
         (should-not (buffer-live-p chunk-buffer)))))))

(ert-deftest anvil-file-test-page-overflow-prefers-change-race ()
  "A generation race wins over page overflow without scanning onward."
  (let ((body (concat "RACE-CONTENT-SENTINEL\n"
                      (make-string
                       (* 17 anvil-file--stream-chunk-bytes) ?z))))
    (anvil-file-test--with-tmp
     body
     (lambda (path)
       (let ((anvil-file-max-inline-read-bytes 8)
             (literal-insert
              (symbol-function 'insert-file-contents-literally))
             (generate-buffer (symbol-function 'generate-new-buffer))
             (signal-changed
              (symbol-function 'anvil-file--signal-stream-changed))
             page-buffer
             chunk-buffer
             retained-at-signal
             (reads 0)
             (yields 0))
         (cl-letf (((symbol-function 'anvil--insert-file)
                    (lambda (&rest _)
                      (ert-fail "overflow race called the full loader")))
                   ((symbol-function 'generate-new-buffer)
                    (lambda (name &rest arguments)
                      (let ((buffer (apply generate-buffer name arguments)))
                        (cond
                         ((equal name " *anvil-file-page*")
                          (setq page-buffer buffer))
                         ((equal name " *anvil-file-chunk*")
                          (setq chunk-buffer buffer)))
                        buffer)))
                   ((symbol-function 'insert-file-contents-literally)
                    (lambda (filename &optional visit beg end replace)
                      (cl-incf reads)
                      (prog1
                          (funcall literal-insert
                                   filename visit beg end replace)
                        (when (= reads 1)
                          (with-temp-file filename
                            (insert "replacement\n"))))))
                   ((symbol-function 'accept-process-output)
                    (lambda (&rest _)
                      (cl-incf yields)
                      nil))
                   ((symbol-function 'anvil-file--signal-page-overflow)
                    (lambda (&rest _)
                      (ert-fail "overflow won over a generation change")))
                   ((symbol-function 'anvil-file--signal-stream-changed)
                    (lambda ()
                      (setq retained-at-signal
                            (list
                             (with-current-buffer page-buffer (buffer-size))
                             (with-current-buffer chunk-buffer (buffer-size))))
                      (funcall signal-changed))))
           (let ((message
                  (anvil-file-test--error-text
                   (lambda () (anvil-file-read path 0 1)))))
             (should (string-match-p "File changed" message))
             (should-not (string-match-p "RACE-CONTENT-SENTINEL" message))))
         (should (= 1 reads))
         (should (= 0 yields))
         (should (equal '(0 0) retained-at-signal))
         (should-not (buffer-live-p page-buffer))
         (should-not (buffer-live-p chunk-buffer)))))))

(ert-deftest anvil-file-test-read-warnings-empty-without-buffer ()
  "anvil-file-read returns :warnings nil when no buffer visits the file."
  (anvil-file-test--with-tmp
   "hello\n"
   (lambda (path)
     (let ((res (anvil-file-read path)))
       (should (null (plist-get res :warnings)))
       (should (equal "hello\n" (plist-get res :content)))))))

(ert-deftest anvil-file-test-read-warnings-flag-buffer-newer ()
  "anvil-file-read surfaces a warning when a visited buffer is dirty."
  (anvil-file-test--with-tmp
   "hello\n"
   (lambda (path)
     (let ((buf (find-file-noselect path)))
       (unwind-protect
           (progn
             (with-current-buffer buf (insert "UNSAVED"))
             (let* ((res (anvil-file-read path))
                    (ws  (plist-get res :warnings)))
               (should (= 1 (length ws)))
             (should (string-match-p "buffer-newer" (car ws)))
             ;; Disk content unchanged.
             (should (equal "hello\n" (plist-get res :content)))))
         (anvil-file-test--discard-buffer buf))))))

(ert-deftest anvil-file-test-read-delta-first-full-then-unchanged ()
  "First read returns full content; identical re-read returns a tiny unchanged payload."
  (anvil-file-test--with-delta-cache ()
    (anvil-file-test--with-tmp
     "alpha\nbeta\ngamma\n"
     (lambda (path)
       (let* ((full (anvil-file-read-delta path))
              (unchanged (anvil-file-read-delta path)))
         (should (equal "full" (plist-get full :mode)))
         (should (equal "alpha\nbeta\ngamma\n" (plist-get full :content)))
         (should (stringp (plist-get full :hash)))
         (should (equal "unchanged" (plist-get unchanged :mode)))
         (should (equal (plist-get full :hash) (plist-get unchanged :hash)))
         (should (< (length (anvil-file-test--serialize unchanged)) 200)))))))

(ert-deftest anvil-file-test-read-delta-small-edit-returns-applicable-diff ()
  "A small edit returns a unified diff that reconstructs the new content."
  (anvil-file-test--with-delta-cache ()
    (anvil-file-test--with-tmp
     (mapconcat (lambda (n) (format "line-%02d" n)) (number-sequence 1 30) "\n")
     (lambda (path)
       (let* ((full (anvil-file-read-delta path))
              (old-body (plist-get full :content))
              (old-hash (plist-get full :hash))
              (new-lines (mapcar (lambda (n) (format "line-%02d" n))
                                 (number-sequence 1 30)))
              (new-body
               (mapconcat #'identity
                          (append (seq-take new-lines 9)
                                  '("line-10 updated"
                                    "line-11 updated"
                                    "line-12 updated")
                                  (nthcdr 12 new-lines))
                          "\n")))
         (anvil-file-test--write path new-body)
         (let* ((delta (anvil-file-read-delta path))
                (diff (plist-get delta :diff)))
           (should (equal "delta" (plist-get delta :mode)))
           (should (equal old-hash (plist-get delta :base-hash)))
           (should (string-match-p "^--- cached\n" diff))
           (should (string-match-p "^\\+\\+\\+ current\n" diff))
           (should (string-match-p "^-line-10$" diff))
           (should (string-match-p "^\\+line-10 updated$" diff))
           (should (string-match-p "^\\+line-12 updated$" diff))
           (should (equal new-body
                          (anvil-file-test--apply-unified-diff old-body diff)))
           (should (equal (secure-hash 'sha1 new-body)
                          (plist-get delta :hash)))))))))

(ert-deftest anvil-file-test-read-delta-large-rewrite-falls-back-to-full ()
  "Large rewrites do not return a delta when the diff is too expensive."
  (anvil-file-test--with-delta-cache ()
    (anvil-file-test--with-tmp
     (mapconcat (lambda (n) (format "line-%02d" n)) (number-sequence 1 20) "\n")
     (lambda (path)
       (anvil-file-read-delta path)
       (let ((new-body (mapconcat (lambda (n) (format "rewrite-%02d" n))
                                  (number-sequence 1 20) "\n")))
         (anvil-file-test--write path new-body)
         (let ((res (anvil-file-read-delta path)))
           (should (equal "full" (plist-get res :mode)))
           (should (equal new-body (plist-get res :content)))
           (should-not (plist-get res :diff))))))))

(ert-deftest anvil-file-test-read-delta-reset-and-oversized-bypass-cache ()
  "reset=true forces a fresh full baseline; oversized files stay uncached."
  (anvil-file-test--with-delta-cache
      ((anvil-file-delta-cache-max-entry-chars 8))
    (anvil-file-test--with-tmp
     "small\n"
     (lambda (path)
       (let* ((full (anvil-file-read-delta path))
              (wrapped (read (anvil-file--tool-read-delta path "true"))))
         (should (equal "full" (plist-get full :mode)))
         (should (equal "full" (plist-get wrapped :mode)))
         (should (equal (plist-get full :content) (plist-get wrapped :content))))
       (anvil-file-test--write path "0123456789\n")
       (let ((large1 (anvil-file-read-delta path))
             (large2 (anvil-file-read-delta path)))
         (should (equal "full" (plist-get large1 :mode)))
         (should (equal "too large for delta cache" (plist-get large1 :note)))
         (should (equal "full" (plist-get large2 :mode)))
         (should (equal "too large for delta cache" (plist-get large2 :note)))
         (should-not (gethash (expand-file-name path) anvil-file--delta-cache)))))))

(ert-deftest anvil-file-test-read-delta-eviction-is-fifo ()
  "Reading past the entry limit evicts the oldest cached baseline."
  (anvil-file-test--with-delta-cache
      ((anvil-file-delta-cache-max-entries 2))
    (let ((p1 (make-temp-file "anvil-file-delta-a-" nil ".txt"))
          (p2 (make-temp-file "anvil-file-delta-b-" nil ".txt"))
          (p3 (make-temp-file "anvil-file-delta-c-" nil ".txt")))
      (unwind-protect
          (progn
            (anvil-file-test--write p1 "a\n")
            (anvil-file-test--write p2 "b\n")
            (anvil-file-test--write p3 "c\n")
            (anvil-file-read-delta p1)
            (anvil-file-read-delta p2)
            (anvil-file-read-delta p3)
            (should-not (gethash (expand-file-name p1) anvil-file--delta-cache))
            (should (equal "full" (plist-get (anvil-file-read-delta p1) :mode))))
        (dolist (path (list p1 p2 p3))
          (when (file-exists-p path) (delete-file path)))))))

(ert-deftest anvil-file-test-read-delta-unreadable-path-returns-string-and-drops-stale-cache ()
  "Missing/unreadable paths return a string result and clear stale cache state."
  (anvil-file-test--with-delta-cache ()
    (let ((path (make-temp-file "anvil-file-delta-missing-" nil ".txt")))
      (unwind-protect
          (progn
            (anvil-file-test--write path "baseline\n")
            (should (equal "full" (plist-get (anvil-file-read-delta path) :mode)))
            (delete-file path)
            (let ((res (anvil-file-read-delta path)))
              (should (stringp res))
              (should (string-match-p "file-read-delta failed" res))
              (should-not (gethash (expand-file-name path) anvil-file--delta-cache)))
            (anvil-file-test--write path "rebuilt\n")
            (should (equal "full" (plist-get (anvil-file-read-delta path) :mode))))
        (when (file-exists-p path) (delete-file path))))))

(ert-deftest anvil-file-test-replace-string-warnings-empty ()
  "anvil-file-replace-string returns :warnings nil when no buffer visits."
  (anvil-file-test--with-tmp
   "alpha beta gamma\n"
   (lambda (path)
     (let ((res (anvil-file-replace-string path "beta" "BETA")))
       (should (= 1 (plist-get res :replaced)))
       (should (null (plist-get res :warnings)))
       (should (equal "alpha BETA gamma\n"
                      (anvil-file-test--read path)))))))

(ert-deftest anvil-file-test-replace-string-warnings-flag-buffer-newer ()
  "anvil-file-replace-string surfaces divergence but still writes disk."
  (anvil-file-test--with-tmp
   "alpha beta gamma\n"
   (lambda (path)
     (let ((buf (find-file-noselect path)))
       (unwind-protect
           (progn
             (with-current-buffer buf (insert "UNSAVED"))
             (let* ((res (anvil-file-replace-string path "beta" "BETA"))
                    (ws  (plist-get res :warnings)))
               (should (= 1 (plist-get res :replaced)))
               (should (= 1 (length ws)))
               (should (string-match-p "buffer-newer" (car ws)))
               (should (equal "alpha BETA gamma\n"
                              (anvil-file-test--read path)))))
         (anvil-file-test--discard-buffer buf))))))

;;;; --- Phase 2 full: :warnings embedded in all mutating tools --------------

(defmacro anvil-file-test--expect-warning (form)
  "Assert FORM's plist result includes a `buffer-newer' :warnings entry."
  `(let ((ws (plist-get ,form :warnings)))
     (should (= 1 (length ws)))
     (should (string-match-p "buffer-newer" (car ws)))))

(ert-deftest anvil-file-test-phase2-replace-regexp-warnings ()
  (anvil-file-test--with-tmp
   "aaa bbb ccc\n"
   (lambda (path)
     (let ((buf (find-file-noselect path)))
       (unwind-protect
           (progn
             (with-current-buffer buf (insert "UNSAVED"))
             (anvil-file-test--expect-warning
              (anvil-file-replace-regexp path "b+" "BBB")))
         (anvil-file-test--discard-buffer buf))))))

(ert-deftest anvil-file-test-phase2-insert-at-line-warnings ()
  (anvil-file-test--with-tmp
   "one\ntwo\nthree\n"
   (lambda (path)
     (let ((buf (find-file-noselect path)))
       (unwind-protect
           (progn
             (with-current-buffer buf (insert "UNSAVED"))
             (anvil-file-test--expect-warning
              (anvil-file-insert-at-line path 2 "inserted")))
         (anvil-file-test--discard-buffer buf))))))

(ert-deftest anvil-file-test-phase2-delete-lines-warnings ()
  (anvil-file-test--with-tmp
   "one\ntwo\nthree\nfour\n"
   (lambda (path)
     (let ((buf (find-file-noselect path)))
       (unwind-protect
           (progn
             (with-current-buffer buf (insert "UNSAVED"))
             (anvil-file-test--expect-warning
              (anvil-file-delete-lines path 2 3)))
         (anvil-file-test--discard-buffer buf))))))

(ert-deftest anvil-file-test-phase2-append-warnings ()
  (anvil-file-test--with-tmp
   "start\n"
   (lambda (path)
     (let ((buf (find-file-noselect path)))
       (unwind-protect
           (progn
             (with-current-buffer buf (insert "UNSAVED"))
             (anvil-file-test--expect-warning
              (anvil-file-append path "tail\n")))
         (anvil-file-test--discard-buffer buf))))))

(ert-deftest anvil-file-test-phase2-prepend-warnings ()
  (anvil-file-test--with-tmp
   "body\n"
   (lambda (path)
     (let ((buf (find-file-noselect path)))
       (unwind-protect
           (progn
             (with-current-buffer buf (insert "UNSAVED"))
             (anvil-file-test--expect-warning
              (anvil-file-prepend path "head\n")))
         (anvil-file-test--discard-buffer buf))))))

(ert-deftest anvil-file-test-phase2-batch-warnings ()
  (anvil-file-test--with-tmp
   "foo bar\n"
   (lambda (path)
     (let ((buf (find-file-noselect path)))
       (unwind-protect
           (progn
             (with-current-buffer buf (insert "UNSAVED"))
             (anvil-file-test--expect-warning
              (anvil-file-batch
               path
               '(((op . "replace") (old . "foo") (new . "FOO"))))))
         (anvil-file-test--discard-buffer buf))))))

(ert-deftest anvil-file-test-phase2-ensure-import-warnings-insert-path ()
  "ensure-import's insertion branch carries a divergence :warning."
  (anvil-file-test--with-tmp
   "import a\nimport b\n\nbody()\n"
   (lambda (path)
     (let ((buf (find-file-noselect path)))
       (unwind-protect
           (progn
             (with-current-buffer buf (insert "UNSAVED"))
             (anvil-file-test--expect-warning
              (anvil-file-ensure-import path "import c")))
         (anvil-file-test--discard-buffer buf))))))

(ert-deftest anvil-file-test-phase2-ensure-import-warnings-already-present ()
  "ensure-import's already-present branch still surfaces :warnings.
Uses a fixture whose target line is already on disk so no write fires."
  (anvil-file-test--with-tmp
   "import a\nimport c\nbody()\n"
   (lambda (path)
     (let ((buf (find-file-noselect path)))
       (unwind-protect
           (progn
             (with-current-buffer buf (insert "UNSAVED"))
             (let ((res (anvil-file-ensure-import path "import c")))
               (should (eq t (plist-get res :already-present)))
               (should (= 1 (length (plist-get res :warnings))))
               (should (string-match-p
                        "buffer-newer\\|both-modified"
                        (car (plist-get res :warnings))))))
         (anvil-file-test--discard-buffer buf))))))

;;;; --- code-add-field-by-map ----------------------------------------------

(defun anvil-file-test--with-tmp-ts (content fn)
  "Write CONTENT to a .ts temp file, call FN with its path, then clean up."
  (let ((path (make-temp-file "anvil-code-test-" nil ".ts")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region content nil path nil 'silent))
          (funcall fn path))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest anvil-code-test-dry-run-default ()
  "Without :apply, the file is not modified but preview is populated."
  (anvil-file-test--with-tmp-ts
   "name: { ja: \"ピザ\" },\n"
   (lambda (path)
     (let ((res (anvil-code-add-field-by-map
                 path
                 '(:lookup-key "ja" :add-key "en"
                   :map (("ピザ" . "Pizza"))))))
       (should (eq t (plist-get res :dry-run)))
       (should (= 1 (plist-get res :added)))
       (should (= 1 (plist-get res :total-matches)))
       ;; File unchanged.
       (should (equal "name: { ja: \"ピザ\" },\n"
                      (anvil-file-test--read path)))
       ;; Preview shows the would-be diff.
       (let ((preview (plist-get res :preview)))
         (should (= 1 (length preview)))
         (let ((row (car preview)))
           (should (= 1 (nth 0 row)))
           (should (string-match-p "ja: \"ピザ\"" (nth 1 row)))
           (should (string-match-p "en: \"Pizza\"" (nth 2 row)))))))))

(ert-deftest anvil-code-test-apply-single-block ()
  "With :apply t, the file is rewritten with ADD-KEY inserted."
  (anvil-file-test--with-tmp-ts
   "name: { ja: \"ピザ\" },\n"
   (lambda (path)
     (let ((res (anvil-code-add-field-by-map
                 path
                 '(:lookup-key "ja" :add-key "en"
                   :map (("ピザ" . "Pizza"))
                   :apply t))))
       (should (eq nil (plist-get res :dry-run)))
       (should (= 1 (plist-get res :added)))
       (should (equal "name: { ja: \"ピザ\", en: \"Pizza\" },\n"
                      (anvil-file-test--read path)))))))

(ert-deftest anvil-code-test-apply-multiple-blocks ()
  "Multiple blocks across the file get processed in one call."
  (anvil-file-test--with-tmp-ts
   (concat "name: { ja: \"A\" },\n"
           "name: { ja: \"B\" },\n"
           "name: { ja: \"C\" },\n")
   (lambda (path)
     (let ((res (anvil-code-add-field-by-map
                 path
                 '(:lookup-key "ja" :add-key "en"
                   :map (("A" . "Apple") ("B" . "Banana") ("C" . "Cherry"))
                   :apply t))))
       (should (= 3 (plist-get res :added)))
       (should (= 3 (plist-get res :total-matches)))
       (should (equal (concat "name: { ja: \"A\", en: \"Apple\" },\n"
                              "name: { ja: \"B\", en: \"Banana\" },\n"
                              "name: { ja: \"C\", en: \"Cherry\" },\n")
                      (anvil-file-test--read path)))))))

(ert-deftest anvil-code-test-on-existing-error-default ()
  "Default :on-existing 'error stops when ADD-KEY is already present."
  (anvil-file-test--with-tmp-ts
   "name: { ja: \"A\", en: \"Old\" },\n"
   (lambda (path)
     (should-error
      (anvil-code-add-field-by-map
       path
       '(:lookup-key "ja" :add-key "en"
         :map (("A" . "Apple"))
         :apply t))))))

(ert-deftest anvil-code-test-on-existing-skip ()
  "With :on-existing 'skip, blocks already containing ADD-KEY are left alone."
  (anvil-file-test--with-tmp-ts
   (concat "name: { ja: \"A\", en: \"Old\" },\n"
           "name: { ja: \"B\" },\n")
   (lambda (path)
     (let ((res (anvil-code-add-field-by-map
                 path
                 '(:lookup-key "ja" :add-key "en"
                   :map (("A" . "Apple") ("B" . "Banana"))
                   :on-existing skip
                   :apply t))))
       (should (= 1 (plist-get res :added)))
       (should (= 1 (plist-get res :skipped)))
       (should (equal (concat "name: { ja: \"A\", en: \"Old\" },\n"
                              "name: { ja: \"B\", en: \"Banana\" },\n")
                      (anvil-file-test--read path)))))))

(ert-deftest anvil-code-test-on-existing-overwrite ()
  "With :on-existing 'overwrite, the existing ADD-KEY value is replaced."
  (anvil-file-test--with-tmp-ts
   "name: { ja: \"A\", en: \"Old\" },\n"
   (lambda (path)
     (let ((res (anvil-code-add-field-by-map
                 path
                 '(:lookup-key "ja" :add-key "en"
                   :map (("A" . "Apple"))
                   :on-existing overwrite
                   :apply t))))
       (should (= 1 (plist-get res :overwritten)))
       (should (= 0 (plist-get res :added)))
       (should (equal "name: { ja: \"A\", en: \"Apple\" },\n"
                      (anvil-file-test--read path)))))))

(ert-deftest anvil-code-test-on-missing-skip-default ()
  "Lookup values not in MAP are skipped by default and tracked in :missing."
  (anvil-file-test--with-tmp-ts
   (concat "name: { ja: \"A\" },\n"
           "name: { ja: \"Unknown\" },\n")
   (lambda (path)
     (let ((res (anvil-code-add-field-by-map
                 path
                 '(:lookup-key "ja" :add-key "en"
                   :map (("A" . "Apple"))
                   :apply t))))
       (should (= 1 (plist-get res :added)))
       (should (= 1 (plist-get res :skipped)))
       (let ((missing (plist-get res :missing)))
         (should (= 1 (length missing)))
         (should (equal "Unknown" (caar missing)))
         (should (= 1 (cdar missing))))
       (should (equal (concat "name: { ja: \"A\", en: \"Apple\" },\n"
                              "name: { ja: \"Unknown\" },\n")
                      (anvil-file-test--read path)))))))

(ert-deftest anvil-code-test-on-missing-error ()
  "With :on-missing 'error, an unmapped lookup-value aborts."
  (anvil-file-test--with-tmp-ts
   "name: { ja: \"Unknown\" },\n"
   (lambda (path)
     (should-error
      (anvil-code-add-field-by-map
       path
       '(:lookup-key "ja" :add-key "en"
         :map (("A" . "Apple"))
         :on-missing error
         :apply t))))))

(ert-deftest anvil-code-test-scope-regex ()
  "Edits only happen inside substrings matching :scope-regex."
  (anvil-file-test--with-tmp-ts
   (concat "// region:food\n"
           "  name: { ja: \"A\" },\n"
           "  name: { ja: \"B\" },\n"
           "// region:other\n"
           "  name: { ja: \"A\" },\n")
   (lambda (path)
     (let ((res (anvil-code-add-field-by-map
                 path
                 '(:lookup-key "ja" :add-key "en"
                   :map (("A" . "Apple") ("B" . "Banana"))
                   :scope-regex "// region:food[^/]*"
                   :apply t))))
       (should (= 2 (plist-get res :added)))
       (should (= 2 (plist-get res :total-matches)))
       (should (string-match-p
                "  name: { ja: \"A\", en: \"Apple\" },"
                (anvil-file-test--read path)))
       (should (string-match-p
                "  name: { ja: \"B\", en: \"Banana\" },"
                (anvil-file-test--read path)))
       ;; Outside scope unchanged.
       (let ((content (anvil-file-test--read path)))
         (should (string-match-p
                  "// region:other\n  name: { ja: \"A\" },"
                  content)))))))

(ert-deftest anvil-code-test-multiline-block-skipped ()
  "Multi-line `{...}' blocks are not matched (MVP single-line only)."
  (anvil-file-test--with-tmp-ts
   (concat "name: {\n"
           "  ja: \"A\"\n"
           "},\n")
   (lambda (path)
     (let ((res (anvil-code-add-field-by-map
                 path
                 '(:lookup-key "ja" :add-key "en"
                   :map (("A" . "Apple"))
                   :apply t))))
       (should (= 0 (plist-get res :total-matches)))
       (should (equal (concat "name: {\n"
                              "  ja: \"A\"\n"
                              "},\n")
                      (anvil-file-test--read path)))))))

(ert-deftest anvil-code-test-empty-block-insert ()
  "Inserting into an empty `{}' yields no leading separator."
  (anvil-file-test--with-tmp-ts
   "name: { ja: \"A\" },\n"
   (lambda (path)
     ;; First add en with apply, then verify shape was clean.
     (anvil-code-add-field-by-map
      path
      '(:lookup-key "ja" :add-key "en"
        :map (("A" . "Apple"))
        :apply t))
     (should (equal "name: { ja: \"A\", en: \"Apple\" },\n"
                    (anvil-file-test--read path))))))

(ert-deftest anvil-code-test-required-fields ()
  "Missing required spec fields raise."
  (anvil-file-test--with-tmp-ts
   "name: { ja: \"A\" },\n"
   (lambda (path)
     (should-error (anvil-code-add-field-by-map path '(:add-key "en" :map nil)))
     (should-error (anvil-code-add-field-by-map path '(:lookup-key "ja" :map nil)))
     (should-error (anvil-code-add-field-by-map path '(:lookup-key "ja" :add-key "en"))))))

(ert-deftest anvil-code-test-escape-quotes-in-mapped-value ()
  "Mapped values containing `\"' are JSON-escaped on write."
  (anvil-file-test--with-tmp-ts
   "name: { ja: \"A\" },\n"
   (lambda (path)
     (anvil-code-add-field-by-map
      path
      `(:lookup-key "ja" :add-key "en"
        :map (("A" . ,(concat "Quoted " (string ?\") "thing" (string ?\"))))
        :apply t))
     (should (string-match-p
              (regexp-quote (concat "en: \"Quoted "
                                    (string ?\\) (string ?\")
                                    "thing"
                                    (string ?\\) (string ?\")
                                    "\""))
              (anvil-file-test--read path))))))

;;;; --- code-extract-pattern -----------------------------------------------

(ert-deftest anvil-code-extract-test-next-block-start-default ()
  "Default :block-end 'next-block-start splits at successive starts."
  (anvil-file-test--with-tmp-ts
   (concat "ITEM 1\n"
           "  name = \"Apple\"\n"
           "  price = 100\n"
           "ITEM 2\n"
           "  name = \"Banana\"\n"
           "  price = 200\n")
   (lambda (path)
     (let* ((res (anvil-code-extract-pattern
                  path
                  '(:block-start "^ITEM \\([0-9]+\\)"
                    :fields ((:name "name"
                              :regexp "name = \"\\([^\"]*\\)\"")
                             (:name "price"
                              :regexp "price = \\([0-9]+\\)")))))
            (matches (plist-get res :matches)))
       (should (= 2 (plist-get res :total)))
       (should (= 2 (plist-get res :returned)))
       (should (equal "1" (plist-get (nth 0 matches) :id)))
       (should (equal "Apple"
                      (alist-get "name" (plist-get (nth 0 matches) :fields)
                                 nil nil #'equal)))
       (should (equal "100"
                      (alist-get "price" (plist-get (nth 0 matches) :fields)
                                 nil nil #'equal)))
       (should (equal "Banana"
                      (alist-get "name" (plist-get (nth 1 matches) :fields)
                                 nil nil #'equal)))))))

(ert-deftest anvil-code-extract-test-brace-balance ()
  "brace-balance finds matching `}' through nested `{...}'."
  (anvil-file-test--with-tmp-ts
   (concat "if (id == 100) {\n"
           "  name = \"Outer\";\n"
           "  inner = { foo: 1 };\n"
           "  price = 17000;\n"
           "}\n"
           "if (id == 200) {\n"
           "  name = \"Second\";\n"
           "  price = 25000;\n"
           "}\n")
   (lambda (path)
     (let* ((res (anvil-code-extract-pattern
                  path
                  '(:block-start "if (id == \\([0-9]+\\))"
                    :block-end brace-balance
                    :fields ((:name "name"
                              :regexp "name = \"\\([^\"]*\\)\"")
                             (:name "price"
                              :regexp "price = \\([0-9]+\\)")))))
            (matches (plist-get res :matches)))
       (should (= 2 (plist-get res :returned)))
       (should (equal "100" (plist-get (nth 0 matches) :id)))
       (should (equal "Outer"
                      (alist-get "name" (plist-get (nth 0 matches) :fields)
                                 nil nil #'equal)))
       (should (equal "17000"
                      (alist-get "price" (plist-get (nth 0 matches) :fields)
                                 nil nil #'equal)))
       (should (equal "200" (plist-get (nth 1 matches) :id)))
       (should (equal "25000"
                      (alist-get "price" (plist-get (nth 1 matches) :fields)
                                 nil nil #'equal)))))))

(ert-deftest anvil-code-extract-test-brace-balance-header-before-brace ()
  "brace-balance expects :block-start to match the header before `{'."
  (anvil-file-test--with-tmp-ts
   (concat "if (id == 1) {\n"
           "  count = 5;\n"
           "}\n")
   (lambda (path)
     (let* ((res (anvil-code-extract-pattern
                  path
                  '(:block-start "if (id == \\([0-9]+\\))"
                    :block-end brace-balance
                    :fields ((:name "count"
                              :regexp "count = \\([0-9]+\\)")))))
            (matches (plist-get res :matches)))
       (should (= 1 (plist-get res :returned)))
       (should (equal "1" (plist-get (car matches) :id)))
       (should (equal "5"
                      (alist-get "count" (plist-get (car matches) :fields)
                                 nil nil #'equal)))))))

(ert-deftest anvil-code-extract-test-brace-balance-start-must-not-consume-brace ()
  "Document current contract: :block-start must not consume the opening `{'."
  (anvil-file-test--with-tmp-ts
   (concat "if (id == 1) {\n"
           "  count = 5;\n"
           "}\n")
   (lambda (path)
     (let ((res (anvil-code-extract-pattern
                 path
                 '(:block-start "if (id == \\([0-9]+\\)) {"
                   :block-end brace-balance
                   :fields ((:name "count"
                             :regexp "count = \\([0-9]+\\)"))))))
       (should (= 1 (plist-get res :total)))
       (should (= 0 (plist-get res :returned)))))))

(ert-deftest anvil-code-extract-test-brace-balance-skips-string-braces ()
  "brace-balance ignores `{' / `}' that appear inside double-quoted strings."
  (anvil-file-test--with-tmp-ts
   (concat "if (id == 1) {\n"
           "  template = \"hello {world} bye\";\n"
           "  count = 5;\n"
           "}\n")
   (lambda (path)
     (let* ((res (anvil-code-extract-pattern
                  path
                  '(:block-start "if (id == \\([0-9]+\\))"
                    :block-end brace-balance
                    :fields ((:name "count"
                              :regexp "count = \\([0-9]+\\)")))))
            (matches (plist-get res :matches)))
       (should (= 1 (plist-get res :returned)))
       (should (equal "5"
                      (alist-get "count" (plist-get (nth 0 matches) :fields)
                                 nil nil #'equal)))))))

(ert-deftest anvil-code-extract-test-regexp-block-end ()
  "A string :block-end is treated as a regexp ending the block."
  (anvil-file-test--with-tmp-ts
   (concat "BEGIN A\n"
           "  v = 1\n"
           "END\n"
           "BEGIN B\n"
           "  v = 2\n"
           "END\n")
   (lambda (path)
     (let* ((res (anvil-code-extract-pattern
                  path
                  '(:block-start "^BEGIN \\([A-Z]\\)"
                    :block-end "^END$"
                    :fields ((:name "v" :regexp "v = \\([0-9]+\\)")))))
            (matches (plist-get res :matches)))
       (should (= 2 (plist-get res :returned)))
       (should (equal "A" (plist-get (nth 0 matches) :id)))
       (should (equal "1"
                      (alist-get "v" (plist-get (nth 0 matches) :fields)
                                 nil nil #'equal)))
       (should (equal "B" (plist-get (nth 1 matches) :id)))
       (should (equal "2"
                      (alist-get "v" (plist-get (nth 1 matches) :fields)
                                 nil nil #'equal)))))))

(ert-deftest anvil-code-extract-test-required-skip ()
  "Missing :required field with default 'skip-block drops the block."
  (anvil-file-test--with-tmp-ts
   (concat "ITEM 1\n"
           "  name = \"A\"\n"
           "  price = 10\n"
           "ITEM 2\n"
           "  name = \"B\"\n"
           ;; price intentionally missing
           "ITEM 3\n"
           "  name = \"C\"\n"
           "  price = 30\n")
   (lambda (path)
     (let* ((res (anvil-code-extract-pattern
                  path
                  '(:block-start "^ITEM \\([0-9]+\\)"
                    :fields ((:name "name"
                              :regexp "name = \"\\([^\"]*\\)\"")
                             (:name "price"
                              :regexp "price = \\([0-9]+\\)"
                              :required t))))))
       (should (= 3 (plist-get res :total)))
       (should (= 2 (plist-get res :returned)))
       (should (= 1 (plist-get res :skipped)))
       (let ((ids (mapcar (lambda (m) (plist-get m :id))
                          (plist-get res :matches))))
         (should (equal '("1" "3") ids)))))))

(ert-deftest anvil-code-extract-test-tool-required-false-is-optional ()
  "MCP JSON `required: false' must not mark a field as required."
  (anvil-file-test--with-tmp-ts
   "ITEM 1\n  name = \"A\"\n"
   (lambda (path)
     (let* ((spec-json
             (json-serialize
              '((block-start . "^ITEM \\([0-9]+\\)")
                (fields . [((name . "missing")
                             (regexp . "missing = \\([0-9]+\\)")
                             (required . :false))]))
              :false-object :false))
            (res (read (anvil-file--tool-code-extract-pattern path spec-json))))
       (should (= 1 (plist-get res :total)))
       (should (= 1 (plist-get res :returned)))
       (should (= 0 (plist-get res :skipped)))))))

(ert-deftest anvil-code-extract-test-tool-required-true-skips ()
  "MCP JSON `required: true' still skips blocks with missing fields."
  (anvil-file-test--with-tmp-ts
   "ITEM 1\n  name = \"A\"\n"
   (lambda (path)
     (let* ((spec-json
             (json-serialize
              '((block-start . "^ITEM \\([0-9]+\\)")
                (fields . [((name . "missing")
                             (regexp . "missing = \\([0-9]+\\)")
                             (required . t))]))))
            (res (read (anvil-file--tool-code-extract-pattern path spec-json))))
       (should (= 1 (plist-get res :total)))
       (should (= 0 (plist-get res :returned)))
       (should (= 1 (plist-get res :skipped)))))))

(ert-deftest anvil-code-extract-test-required-error ()
  "Missing :required field with :on-missing-required 'error aborts."
  (anvil-file-test--with-tmp-ts
   (concat "ITEM 1\n"
           "  name = \"A\"\n")
   (lambda (path)
     (should-error
      (anvil-code-extract-pattern
       path
       '(:block-start "^ITEM \\([0-9]+\\)"
         :on-missing-required error
         :fields ((:name "price"
                   :regexp "price = \\([0-9]+\\)"
                   :required t))))))))

(ert-deftest anvil-code-extract-test-max-blocks ()
  ":max-blocks caps the number of returned records."
  (anvil-file-test--with-tmp-ts
   (concat "ITEM 1\n  v = 1\n"
           "ITEM 2\n  v = 2\n"
           "ITEM 3\n  v = 3\n"
           "ITEM 4\n  v = 4\n")
   (lambda (path)
     (let ((res (anvil-code-extract-pattern
                 path
                 '(:block-start "^ITEM \\([0-9]+\\)"
                   :max-blocks 2
                   :fields ((:name "v" :regexp "v = \\([0-9]+\\)"))))))
       (should (= 4 (plist-get res :total)))
       (should (= 2 (plist-get res :returned)))))))

(ert-deftest anvil-code-extract-test-id-nil-when-no-group ()
  "When :block-start has no capture group, :id is nil."
  (anvil-file-test--with-tmp-ts
   "MARK\n  v = 9\n"
   (lambda (path)
     (let* ((res (anvil-code-extract-pattern
                  path
                  '(:block-start "^MARK"
                    :fields ((:name "v" :regexp "v = \\([0-9]+\\)")))))
            (m (car (plist-get res :matches))))
       (should (eq nil (plist-get m :id)))
       (should (equal "9"
                      (alist-get "v" (plist-get m :fields)
                                 nil nil #'equal)))))))

(ert-deftest anvil-code-extract-test-no-matches ()
  "No block-start matches returns empty :matches without error."
  (anvil-file-test--with-tmp-ts
   "nothing here\n"
   (lambda (path)
     (let ((res (anvil-code-extract-pattern
                 path
                 '(:block-start "^NEVER"
                   :fields ((:name "v" :regexp "v = \\([0-9]+\\)"))))))
       (should (= 0 (plist-get res :total)))
       (should (= 0 (plist-get res :returned)))
       (should (eq nil (plist-get res :matches)))))))

(ert-deftest anvil-code-extract-test-validation-required-spec ()
  "Missing :block-start or :fields raises."
  (anvil-file-test--with-tmp-ts
   "any\n"
   (lambda (path)
     (should-error
      (anvil-code-extract-pattern path '(:fields ((:name "x" :regexp "x")))))
     (should-error
      (anvil-code-extract-pattern path '(:block-start "^A"))))))

;;;; --- file-create ---------------------------------------------------------

(defun anvil-file-test--with-tmp-dir (fn)
  "Create a fresh temp directory, call FN with its path, then clean up."
  (let ((dir (make-temp-file "anvil-file-create-" t)))
    (unwind-protect
        (funcall fn dir)
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest anvil-file-test-create-new-file ()
  "anvil-file-create writes content to a fresh path."
  (anvil-file-test--with-tmp-dir
   (lambda (dir)
     (let* ((path (expand-file-name "fresh.txt" dir))
            (result (anvil-file-create path "hello\n")))
       (should (equal (plist-get result :created) (expand-file-name path)))
       (should (= (plist-get result :bytes) 6))
       (should (file-exists-p path))
       (should (equal (anvil-file-test--read path) "hello\n"))))))

(ert-deftest anvil-file-test-create-existing-without-overwrite-errors ()
  "anvil-file-create refuses to clobber an existing file by default."
  (anvil-file-test--with-tmp
   "old\n"
   (lambda (path)
     (should-error (anvil-file-create path "new\n"))
     ;; Original content must be preserved.
     (should (equal (anvil-file-test--read path) "old\n")))))

(ert-deftest anvil-file-test-create-existing-with-overwrite-replaces ()
  "anvil-file-create with OVERWRITE replaces existing content."
  (anvil-file-test--with-tmp
   "old\n"
   (lambda (path)
     (let ((result (anvil-file-create path "new\n" t)))
       (should (= (plist-get result :bytes) 4))
       (should (equal (anvil-file-test--read path) "new\n"))))))

(ert-deftest anvil-file-test-create-missing-parent-dir-errors ()
  "anvil-file-create errors when parent directory is absent."
  (anvil-file-test--with-tmp-dir
   (lambda (dir)
     (let ((path (expand-file-name "no-such-subdir/file.txt" dir)))
       (should-error (anvil-file-create path "x"))
       (should-not (file-exists-p path))))))

(ert-deftest anvil-file-test-tool-create-roundtrip ()
  "MCP wrapper handles new file creation with string args."
  (anvil-file-test--with-tmp-dir
   (lambda (dir)
     (let* ((path (expand-file-name "tool.txt" dir))
            (out (anvil-file--tool-create path "via-tool\n")))
       (should (stringp out))
       (should (string-match-p ":bytes 9" out))
       (should (equal (anvil-file-test--read path) "via-tool\n"))))))

(ert-deftest anvil-file-test-tool-create-overwrite-flag ()
  "MCP wrapper treats non-empty overwrite string as truthy."
  (anvil-file-test--with-tmp
   "first\n"
   (lambda (path)
     ;; Empty string -> still refuses.
     (should-error (anvil-file--tool-create path "second\n" ""))
     (should (equal (anvil-file-test--read path) "first\n"))
     ;; Non-empty -> overwrites.
     (let ((out (anvil-file--tool-create path "second\n" "1")))
       (should (string-match-p ":bytes 7" out))
       (should (equal (anvil-file-test--read path) "second\n"))))))

;;; anvil-file-test.el ends here
