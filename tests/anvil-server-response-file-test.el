;;; anvil-server-response-file-test.el --- Authenticated response staging -*- lexical-binding: t; -*-

;;; Commentary:

;; Regressions for the stdio transport response-file handoff.  Every response
;; must remain bound to the bridge-precommitted inode across stateful dispatch,
;; publication, marker loss, and adversarial pathname callbacks.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'anvil-server)

(defmacro anvil-server-response-file-test--with-directory (directory &rest body)
  "Run BODY with DIRECTORY bound to a private temporary directory."
  (declare (indent 1))
  `(let ((,directory
          (file-truename
           (make-temp-file "anvil-response-file-test-" t))))
     (unwind-protect
         (progn
           (set-file-modes ,directory #o700)
           ,@body)
       (ignore-errors (set-file-modes ,directory #o700))
       (ignore-errors (delete-directory ,directory t)))))

(defun anvil-server-response-file-test--stage (directory sequence)
  "Create and describe a safe precommitted stage for SEQUENCE."
  (let* ((path
          (make-temp-file
           (expand-file-name
            (format ".response-tmp.%d." sequence)
            directory)))
         attributes)
    (set-file-modes path #o600)
    (setq attributes (file-attributes path 'integer))
    (list path
          (file-attribute-device-number attributes)
          (file-attribute-inode-number attributes))))

(defun anvil-server-response-file-test--final (directory sequence)
  "Return the final response path for SEQUENCE."
  (expand-file-name (format "response.%d.json" sequence) directory))

(defun anvil-server-response-file-test--proof (directory sequence)
  "Return the response proof path for SEQUENCE."
  (expand-file-name (format "proof.%d.json" sequence) directory))

(defun anvil-server-response-file-test--call
    (json-string server-id stage sequence max-bytes)
  "Invoke the response writer using STAGE's captured identity."
  (anvil-server-process-jsonrpc-to-file
   json-string server-id (nth 0 stage) sequence max-bytes
   (nth 1 stage) (nth 2 stage)))

(defun anvil-server-response-file-test--contents (path)
  "Return PATH's literal bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun anvil-server-response-file-test--assert-pair
    (directory sequence expected-size)
  "Assert SEQUENCE has one exact authenticated pair of EXPECTED-SIZE."
  (let* ((final
          (anvil-server-response-file-test--final directory sequence))
         (proof
          (anvil-server-response-file-test--proof directory sequence))
         (final-attributes (file-attributes final 'integer))
         (proof-attributes (file-attributes proof 'integer)))
    (should final-attributes)
    (should proof-attributes)
    (should-not (file-symlink-p final))
    (should-not (file-symlink-p proof))
    (should (= #o600 (file-modes final)))
    (should (= #o600 (file-modes proof)))
    (should (= 2 (file-attribute-link-number final-attributes)))
    (should (= 2 (file-attribute-link-number proof-attributes)))
    (should (= expected-size (file-attribute-size final-attributes)))
    (should (= expected-size (file-attribute-size proof-attributes)))
    (should
     (equal
      (file-attribute-device-number final-attributes)
      (file-attribute-device-number proof-attributes)))
    (should
     (equal
      (file-attribute-inode-number final-attributes)
      (file-attribute-inode-number proof-attributes)))
    (list final proof)))

(ert-deftest anvil-server-response-file-test-large-response-is-exact ()
  "A multi-megabyte response publishes one exact two-link inode."
  (anvil-server-response-file-test--with-directory directory
    (let* ((stage (anvil-server-response-file-test--stage directory 1))
           (payload (make-string 3000000 ?z))
           (calls 0))
      (let ((write-region-annotate-functions
             (list
              (lambda (&rest _)
                (error "ambient write annotation ran"))))
            (write-region-post-annotation-function
             (lambda (&rest _)
               (error "ambient post-annotation ran"))))
        (cl-letf (((symbol-function 'anvil-server-process-jsonrpc)
                   (lambda (_request _server)
                     (cl-incf calls)
                     payload)))
          (should
           (equal
            "anvil-mcp-response-staged:1:3000000"
            (anvil-server-response-file-test--call
             "{}" "anvil" stage 1 33554432)))))
      (should (= 1 calls))
      (should-not (file-exists-p (car stage)))
      (pcase-let ((`(,final ,proof)
                   (anvil-server-response-file-test--assert-pair
                    directory 1 3000000)))
        (dolist (path (list final proof))
          (should
           (equal (secure-hash 'sha256 payload)
                  (secure-hash
                   'sha256
                   (anvil-server-response-file-test--contents path))))))
      (should-not
       (directory-files directory nil "^\\.response-tmp\\.")))))

(ert-deftest anvil-server-response-file-test-refuses-preexisting-final ()
  "A competitor final is rejected before stateful dispatch and preserved."
  (anvil-server-response-file-test--with-directory directory
    (let* ((stage (anvil-server-response-file-test--stage directory 2))
           (final (anvil-server-response-file-test--final directory 2))
           (proof (anvil-server-response-file-test--proof directory 2))
           (calls 0))
      (with-temp-file final (insert "competitor"))
      (set-file-modes final #o600)
      (cl-letf (((symbol-function 'anvil-server-process-jsonrpc)
                 (lambda (&rest _args)
                   (cl-incf calls)
                   "{}")))
        (should-error
         (anvil-server-response-file-test--call
          "{}" "anvil" stage 2 33554432)))
      (should (= 0 calls))
      (should (equal "competitor"
                     (anvil-server-response-file-test--contents final)))
      (should-not (file-exists-p proof)))))

(ert-deftest anvil-server-response-file-test-rejects-mismatched-generation ()
  "A stage for another sequence is rejected before dispatch."
  (anvil-server-response-file-test--with-directory directory
    (let ((stage (anvil-server-response-file-test--stage directory 1))
          (calls 0))
      (cl-letf (((symbol-function 'anvil-server-process-jsonrpc)
                 (lambda (&rest _args)
                   (cl-incf calls)
                   "{}")))
        (should-error
         (anvil-server-response-file-test--call
          "{}" "anvil" stage 3 33554432)))
      (should (= 0 calls))
      (should (= 0 (file-attribute-size
                    (file-attributes (car stage))))))))

(ert-deftest anvil-server-response-file-test-oversize-is-correlated-error ()
  "An oversized result becomes a bounded error with the same request id."
  (anvil-server-response-file-test--with-directory directory
    (let ((stage (anvil-server-response-file-test--stage directory 4)))
      (cl-letf (((symbol-function 'anvil-server-process-jsonrpc)
                 (lambda (&rest _args) (make-string 2000 ?z))))
        (should
         (string-match-p
          "^anvil-mcp-response-staged:4:[0-9]+$"
          (anvil-server-response-file-test--call
           "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"x\"}"
           "anvil" stage 4 1024))))
      (let* ((final
              (anvil-server-response-file-test--final directory 4))
             (size (file-attribute-size (file-attributes final)))
             (_pair
              (anvil-server-response-file-test--assert-pair
               directory 4 size))
             (json-object-type 'alist)
             (json-key-type 'symbol)
             (document
              (json-read-from-string
               (anvil-server-response-file-test--contents final)))
             (error-object (alist-get 'error document)))
        (should (= 7 (alist-get 'id document)))
        (should (= -32603 (alist-get 'code error-object)))
        (should
         (string-match-p
          "Response exceeded bridge maximum"
          (alist-get 'message error-object)))))))

(ert-deftest anvil-server-response-file-test-substitute-obeys-hard-limit ()
  "A tiny hard limit publishes an authenticated empty pair."
  (anvil-server-response-file-test--with-directory directory
    (let ((stage (anvil-server-response-file-test--stage directory 5)))
      (cl-letf (((symbol-function 'anvil-server-process-jsonrpc)
                 (lambda (&rest _args) (make-string 2000 ?z))))
        (should
         (equal
          "anvil-mcp-response-staged:5:0"
          (anvil-server-response-file-test--call
           "{\"jsonrpc\":\"2.0\",\"id\":8,\"method\":\"x\"}"
           "anvil" stage 5 1))))
      (anvil-server-response-file-test--assert-pair directory 5 0))))

(ert-deftest anvil-server-response-file-test-cancelled-stage-cannot-publish ()
  "Removing the precommitted stage during dispatch cancels publication."
  (anvil-server-response-file-test--with-directory directory
    (let* ((stage (anvil-server-response-file-test--stage directory 6))
           (final (anvil-server-response-file-test--final directory 6))
           (proof (anvil-server-response-file-test--proof directory 6)))
      (cl-letf (((symbol-function 'anvil-server-process-jsonrpc)
                 (lambda (&rest _args)
                   (delete-file (car stage))
                   "{\"jsonrpc\":\"2.0\",\"id\":6,\"result\":\"late\"}")))
        (should-error
         (anvil-server-response-file-test--call
          "{}" "anvil" stage 6 33554432)))
      (should-not (file-exists-p final))
      (should-not (file-exists-p proof)))))

(ert-deftest anvil-server-response-file-test-no-clobber-competitor ()
  "A final competitor created after dispatch is never overwritten or deleted."
  (anvil-server-response-file-test--with-directory directory
    (let* ((stage (anvil-server-response-file-test--stage directory 7))
           (final (anvil-server-response-file-test--final directory 7))
           (proof (anvil-server-response-file-test--proof directory 7)))
      (cl-letf (((symbol-function 'anvil-server-process-jsonrpc)
                 (lambda (&rest _args)
                   (with-temp-file final (insert "competitor"))
                   (set-file-modes final #o600)
                   "{\"jsonrpc\":\"2.0\",\"id\":7,\"result\":\"late\"}")))
        (should-error
         (anvil-server-response-file-test--call
          "{}" "anvil" stage 7 33554432)))
      (should (equal "competitor"
                     (anvil-server-response-file-test--contents final)))
      (should-not (file-exists-p proof))
      (should-not (file-exists-p (car stage))))))

(ert-deftest anvil-server-response-file-test-stage-swap-preserves-competitor ()
  "A callback-swapped stage is rejected and its replacement is preserved."
  (anvil-server-response-file-test--with-directory directory
    (let* ((stage (anvil-server-response-file-test--stage directory 8))
           (stage-path (car stage))
           (final (anvil-server-response-file-test--final directory 8))
           (proof (anvil-server-response-file-test--proof directory 8)))
      (cl-letf (((symbol-function 'anvil-server-process-jsonrpc)
                 (lambda (&rest _args)
                   (delete-file stage-path)
                   (with-temp-file stage-path (insert "replacement"))
                   (set-file-modes stage-path #o600)
                   "{\"jsonrpc\":\"2.0\",\"id\":8,\"result\":\"late\"}")))
        (should-error
         (anvil-server-response-file-test--call
          "{}" "anvil" stage 8 33554432)))
      (should (equal "replacement"
                     (anvil-server-response-file-test--contents stage-path)))
      (should-not (file-exists-p final))
      (should-not (file-exists-p proof)))))

(ert-deftest anvil-server-response-file-test-proof-link-throw-preserves-pair ()
  "A tagged exit after proof linking leaves the exact recoverable pair."
  (anvil-server-response-file-test--with-directory directory
    (let* ((stage (anvil-server-response-file-test--stage directory 9))
           (proof (anvil-server-response-file-test--proof directory 9))
           (real-add (symbol-function 'add-name-to-file))
           outcome)
      (cl-letf (((symbol-function 'anvil-server-process-jsonrpc)
                 (lambda (&rest _args) "{\"jsonrpc\":\"2.0\",\"id\":9}"))
                ((symbol-function 'add-name-to-file)
                 (lambda (old new &optional ok-if-exists)
                   (prog1
                       (funcall real-add old new ok-if-exists)
                     (when (equal new proof)
                       (throw 'anvil-response-proof-exit 'escaped))))))
        (setq outcome
              (catch 'anvil-response-proof-exit
                (anvil-server-response-file-test--call
                 "{}" "anvil" stage 9 33554432)
                'returned)))
      (should (eq 'escaped outcome))
      (should-not (file-exists-p (car stage)))
      (anvil-server-response-file-test--assert-pair
       directory 9
       (string-bytes "{\"jsonrpc\":\"2.0\",\"id\":9}")))))

(ert-deftest anvil-server-response-file-test-parent-mode-change-fails-closed ()
  "A post-link public parent mode removes all owned publication names."
  (anvil-server-response-file-test--with-directory directory
    (let* ((stage (anvil-server-response-file-test--stage directory 10))
           (proof (anvil-server-response-file-test--proof directory 10))
           (final (anvil-server-response-file-test--final directory 10))
           (real-add (symbol-function 'add-name-to-file)))
      (cl-letf (((symbol-function 'anvil-server-process-jsonrpc)
                 (lambda (&rest _args) "{}"))
                ((symbol-function 'add-name-to-file)
                 (lambda (old new &optional ok-if-exists)
                   (prog1
                       (funcall real-add old new ok-if-exists)
                     (when (equal new proof)
                       (set-file-modes directory #o777))))))
        (should-error
         (anvil-server-response-file-test--call
          "{}" "anvil" stage 10 33554432)))
      (set-file-modes directory #o700)
      (should-not (file-exists-p (car stage)))
      (should-not (file-exists-p final))
      (should-not (file-exists-p proof)))))

(ert-deftest anvil-server-response-file-test-revalidates-parent-after-dispatch ()
  "A callback-swapped response directory cannot receive publication."
  (let* ((root
          (file-truename
           (make-temp-file "anvil-response-parent-test-" t)))
         (directory (expand-file-name "transaction" root))
         (moved (expand-file-name "moved" root)))
    (unwind-protect
        (progn
          (make-directory directory)
          (set-file-modes directory #o700)
          (let* ((stage
                  (anvil-server-response-file-test--stage directory 11))
                 (final
                  (anvil-server-response-file-test--final directory 11)))
            (cl-letf (((symbol-function 'anvil-server-process-jsonrpc)
                       (lambda (&rest _args)
                         (rename-file directory moved)
                         (make-directory directory)
                         (set-file-modes directory #o700)
                         "{}")))
              (should-error
               (anvil-server-response-file-test--call
                "{}" "anvil" stage 11 33554432)))
            (should-not (file-exists-p final))
            (should-not
             (directory-files directory nil "^\\(response\\|proof\\)\\."))
            (should-not
             (directory-files moved nil "^\\(response\\|proof\\)\\."))))
      (ignore-errors (delete-directory root t)))))

(provide 'anvil-server-response-file-test)
;;; anvil-server-response-file-test.el ends here
