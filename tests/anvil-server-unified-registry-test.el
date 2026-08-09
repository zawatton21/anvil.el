;;; anvil-server-unified-registry-test.el --- Registry alias tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression coverage for exposing one canonical registry through a stable
;; virtual MCP server id.  Cached tools/list responses must follow canonical
;; registration mutations, and every registry-backed MCP method must resolve
;; the virtual id before lookup.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'anvil-server)

(defmacro anvil-server-unified-registry-test--with-server (&rest body)
  "Run BODY with isolated canonical and aliased Anvil registries."
  (declare (indent 0))
  `(let ((anvil-server--tools (make-hash-table :test #'equal))
         (anvil-server--resources (make-hash-table :test #'equal))
         (anvil-server--resource-templates (make-hash-table :test #'equal))
         (anvil-server--tools-list-cache (make-hash-table :test #'equal))
         (anvil-server--schema-cache nil)
         (anvil-server--schema-cache-loaded t)
         (anvil-server-id-aliases '(("anvil" . "emacs-eval")))
         (anvil-server-tool-filter-function nil)
         (anvil-server-tool-fragment-function nil)
         (anvil-server-tool-dispatch-hook nil)
         (anvil-server-tool-error-hook nil))
     (cl-letf (((symbol-function 'anvil-server--schema-cache-write)
                (lambda () nil)))
       ,@body)))

(defun anvil-server-unified-registry-test--decode-success (json)
  "Decode JSON and return its result after asserting protocol success."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false :false)
        (json-null :null))
    (let ((response (json-read-from-string json)))
      (should (equal "2.0" (alist-get 'jsonrpc response)))
      (should-not (assq 'error response))
      (should (assq 'result response))
      (alist-get 'result response))))

(defun anvil-server-unified-registry-test--dispatch (method &optional params)
  "Dispatch METHOD with PARAMS through the virtual registry and return result."
  (anvil-server-unified-registry-test--decode-success
   (anvil-server--dispatch-jsonrpc-method
    1 method params "anvil")))

(defun anvil-server-unified-registry-test--tool-names ()
  "Return tool names advertised through the virtual registry."
  (mapcar
   (lambda (tool) (alist-get 'name tool))
   (alist-get
    'tools
    (anvil-server-unified-registry-test--dispatch "tools/list"))))

(defun anvil-server-unified-registry-test--content-text (result)
  "Return the first text content value in RESULT."
  (alist-get 'text (car (alist-get 'contents result))))

(ert-deftest anvil-server-unified-registry-test-alias-cache-mutation ()
  "An alias observes canonical register and unregister after cached lists."
  (anvil-server-unified-registry-test--with-server
    (anvil-server-register-tool
     (lambda () "one")
     :id "one" :description "First tool" :server-id "emacs-eval")
    (should (equal '("one")
                   (anvil-server-unified-registry-test--tool-names)))
    (should
     (gethash
      (cons "anvil" "emacs-eval")
      anvil-server--tools-list-cache))

    (anvil-server-register-tool
     (lambda () "two")
     :id "two" :description "Second tool" :server-id "emacs-eval")
    (should (= 0 (hash-table-count anvil-server--tools-list-cache)))
    (should
     (equal '("one" "two")
            (sort
             (anvil-server-unified-registry-test--tool-names)
             #'string<)))
    (should (= 1 (hash-table-count anvil-server--tools-list-cache)))

    (should (anvil-server-unregister-tool "one" "emacs-eval"))
    (should (= 0 (hash-table-count anvil-server--tools-list-cache)))
    (should (equal '("two")
                   (anvil-server-unified-registry-test--tool-names)))))

(ert-deftest anvil-server-unified-registry-test-virtual-dispatch ()
  "Tools, resources, templates, reads, and calls resolve the virtual id."
  (anvil-server-unified-registry-test--with-server
    (anvil-server-register-tool
     (lambda () "pong")
     :id "ping" :description "Return pong" :server-id "emacs-eval")
    (anvil-server-register-resource
     "test://state" (lambda () "resource-body")
     :name "State" :server-id "emacs-eval")
    (anvil-server-register-resource
     "test://item/{name}" (lambda (_params) "template-body")
     :name "Item" :server-id "emacs-eval")

    (should (equal '("ping")
                   (anvil-server-unified-registry-test--tool-names)))

    (let ((resources
           (alist-get
            'resources
            (anvil-server-unified-registry-test--dispatch
             "resources/list"))))
      (should (= 1 (length resources)))
      (should (equal "test://state"
                     (alist-get 'uri (car resources)))))

    (let ((templates
           (alist-get
            'resourceTemplates
            (anvil-server-unified-registry-test--dispatch
             "resources/templates/list"))))
      (should (= 1 (length templates)))
      (should (equal "test://item/{name}"
                     (alist-get 'uriTemplate (car templates)))))

    (should
     (equal
      "resource-body"
      (anvil-server-unified-registry-test--content-text
       (anvil-server-unified-registry-test--dispatch
        "resources/read" '((uri . "test://state"))))))

    (should
     (equal
      "template-body"
      (anvil-server-unified-registry-test--content-text
       (anvil-server-unified-registry-test--dispatch
        "resources/read" '((uri . "test://item/example"))))))

    (let* ((result
            (anvil-server-unified-registry-test--dispatch
             "tools/call" '((name . "ping") (arguments))))
           (content (car (alist-get 'content result))))
      (should (eq :false (alist-get 'isError result)))
      (should (equal "text" (alist-get 'type content)))
      (should (equal "pong" (alist-get 'text content))))))

(provide 'anvil-server-unified-registry-test)
;;; anvil-server-unified-registry-test.el ends here
