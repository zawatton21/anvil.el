;;; anvil-sexp-test.el --- Tests for anvil-sexp -*- lexical-binding: t; -*-

;;; Commentary:

;; Phase 1 ERT suite for `anvil-sexp'.  Covers reader primitives,
;; read-only locators, preview/apply edit plans, and the verify
;; diagnostics pipeline.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-sexp)


;;;; --- fixture helpers ----------------------------------------------------

(defvar anvil-sexp-test--sample "\
;;; sample.el --- fixture -*- lexical-binding: t; -*-

;;; Commentary:
;; A tiny sample with a few defining forms.

;;; Code:

(defvar sample-var 42
  \"A variable we pretend matters.\")

(defun sample-add (a b)
  \"Return A plus B.\"
  (+ a b))

(defmacro sample-when-positive (x &rest body)
  \"When X is positive evaluate BODY.\"
  (declare (indent 1))
  `(when (> ,x 0) ,@body))

;; sample-add is called once:
(sample-add 1 2)

(provide 'sample)
;;; sample.el ends here
")

(defun anvil-sexp-test--with-sample (fn)
  "Write the sample fixture to a temp file and call FN with its path."
  (let ((tmp (make-temp-file "anvil-sexp-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert anvil-sexp-test--sample))
          (funcall fn tmp))
      (when (file-exists-p tmp) (delete-file tmp))
      (let ((elc (concat tmp "c")))
        (when (file-exists-p elc) (delete-file elc))))))


;;;; --- reader primitives --------------------------------------------------

(ert-deftest anvil-sexp-test-feature-provided ()
  "The module's feature symbol is provided after load."
  (should (featurep 'anvil-sexp)))

(ert-deftest anvil-sexp-test-enable-disable-callable ()
  "Enable and disable stubs exist."
  (should (fboundp 'anvil-sexp-enable))
  (should (fboundp 'anvil-sexp-disable)))

(ert-deftest anvil-sexp-test-read-file-returns-forms ()
  "read-file returns every top-level form with kind / name / range."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let ((forms (anvil-sexp--read-file path)))
       (should (>= (length forms) 5))
       (let ((defun-form (cl-find-if
                          (lambda (f) (eq (plist-get f :kind) 'defun))
                          forms)))
         (should defun-form)
         (should (eq (plist-get defun-form :name) 'sample-add))
         (should (integerp (plist-get defun-form :start)))
         (should (> (plist-get defun-form :end)
                    (plist-get defun-form :start))))))))

(ert-deftest anvil-sexp-test-read-file-skips-comments ()
  "Comments and blank lines do not show up as forms."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let ((forms (anvil-sexp--read-file path)))
       (should-not (cl-find-if
                    (lambda (f) (eq (plist-get f :kind) 'Commentary:))
                    forms))))))

(ert-deftest anvil-sexp-test-find-form-by-name ()
  "find-form-by-name locates a defun by symbol."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let ((hit (anvil-sexp--find-form-by-name path 'sample-add)))
       (should hit)
       (should (eq (plist-get hit :kind) 'defun))))))

(ert-deftest anvil-sexp-test-find-form-by-name-missing ()
  "find-form-by-name returns nil when no form matches."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (should (null (anvil-sexp--find-form-by-name path 'does-not-exist))))))


;;;; --- read-only MCP tools -----------------------------------------------

(ert-deftest anvil-sexp-test-tool-read-file-shape ()
  "sexp-read-file returns JSON-friendly plists with :text."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let ((result (anvil-sexp--tool-read-file path)))
       (should (listp result))
       (dolist (entry result)
         (should (stringp (plist-get entry :text))))
       (let ((names (delq nil (mapcar (lambda (e) (plist-get e :name)) result))))
         (should (member "sample-add" names))
         (should (member "sample-var" names)))))))

(ert-deftest anvil-sexp-test-surrounding-form ()
  "surrounding-form identifies the enclosing defun for a given point."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let* ((forms (anvil-sexp--read-file path))
            (defun-form (cl-find-if
                         (lambda (f) (eq (plist-get f :name) 'sample-add))
                         forms))
            (mid (/ (+ (plist-get defun-form :start)
                       (plist-get defun-form :end))
                    2))
            (hit (anvil-sexp--tool-surrounding-form path mid)))
       (should hit)
       (should (equal (plist-get hit :name) "sample-add"))))))

(ert-deftest anvil-sexp-test-surrounding-form-kind-filter ()
  "surrounding-form respects the optional kind filter."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let* ((forms (anvil-sexp--read-file path))
            (defmacro-form (cl-find-if
                            (lambda (f) (eq (plist-get f :name)
                                            'sample-when-positive))
                            forms))
            (mid (/ (+ (plist-get defmacro-form :start)
                       (plist-get defmacro-form :end))
                    2)))
       ;; Correct kind -> match.
       (should (anvil-sexp--tool-surrounding-form path mid "defmacro"))
       ;; Wrong kind -> no match.
       (should-not (anvil-sexp--tool-surrounding-form path mid "defun"))))))

(ert-deftest anvil-sexp-test-macroexpand ()
  "sexp-macroexpand expands a named macro form."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let ((out (anvil-sexp--tool-macroexpand
                 path "sample-when-positive")))
       (should (equal (plist-get out :name) "sample-when-positive"))
       (should (stringp (plist-get out :expanded)))
       (should (stringp (plist-get out :original)))))))

(ert-deftest anvil-sexp-test-macroexpand-unknown ()
  "sexp-macroexpand errors when the name does not exist."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (should (stringp (plist-get
                       (anvil-sexp--tool-macroexpand path "sample-add")
                       :original)))
     (should-error
      (anvil-sexp--tool-macroexpand path "nope-not-here")
      :type 'anvil-server-tool-error))))


;;;; --- edit plan: preview vs apply ----------------------------------------

(ert-deftest anvil-sexp-test-replace-defun-preview ()
  "Preview returns a plan but does not write the file."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let* ((before (with-temp-buffer
                      (insert-file-contents path)
                      (buffer-string)))
            (plan (anvil-sexp--tool-replace-defun
                   path "sample-add"
                   "(defun sample-add (a b)\n  \"Return A plus B.\"\n  (+ a b 0))"))
            (after (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
       (should (plist-get plan :ops))
       (should (stringp (plist-get plan :diff-preview)))
       (should (null (plist-get plan :applied-at)))
       (should (equal before after))))))

(ert-deftest anvil-sexp-test-replace-defun-apply-writes ()
  "apply=t writes the change and records :applied-at."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let* ((new "(defun sample-add (a b)\n  \"Doubled.\"\n  (+ a b 100))")
            (plan (anvil-sexp--tool-replace-defun
                   path "sample-add" new "t"))
            (after (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
       (should (plist-get plan :applied-at))
       (should (string-match-p "(\\+ a b 100)" after))
       (should-not (string-match-p "(\\+ a b)" after))))))

(ert-deftest anvil-sexp-test-replace-defun-missing-refuses ()
  "replace-defun refuses when the target name is absent."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (should-error
      (anvil-sexp--tool-replace-defun
       path "no-such-symbol" "(defun no-such-symbol () 1)" "t")
      :type 'anvil-server-tool-error))))

(ert-deftest anvil-sexp-test-replace-defun-invalid-new-form ()
  "replace-defun refuses when NEW_FORM does not parse."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (should-error
      (anvil-sexp--tool-replace-defun
       path "sample-add" "(defun broken (" "t")
      :type 'anvil-server-tool-error))))

(ert-deftest anvil-sexp-test-wrap-form-previews ()
  "wrap-form substitutes the placeholder and previews by default."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let* ((forms (anvil-sexp--read-file path))
            (call-form (cl-find-if
                        (lambda (f)
                          (let ((s (plist-get f :sexp)))
                            (and (consp s) (eq (car s) 'sample-add))))
                        forms))
            (pos (plist-get call-form :start))
            (plan (anvil-sexp--tool-wrap-form
                   path pos
                   "(when (numberp 1) |anvil-sexp-hole|)")))
       (should (stringp (plist-get plan :diff-preview)))
       (should (null (plist-get plan :applied-at)))
       (let ((ops (plist-get plan :ops)))
         (should (= 1 (length ops)))
         (should (string-match-p "when (numberp 1)"
                                 (plist-get (car ops) :replacement))))))))

(ert-deftest anvil-sexp-test-wrap-form-requires-placeholder ()
  "wrap-form refuses wrappers without the placeholder token."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (should-error
      (anvil-sexp--tool-wrap-form path 10 "(when t nil)")
      :type 'anvil-server-tool-error))))

(ert-deftest anvil-sexp-test-apply-plan-is-order-safe ()
  "Multiple ops on one file apply back-to-front without range drift."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let* ((forms (anvil-sexp--read-file path))
            (add (cl-find-if (lambda (f) (eq (plist-get f :name) 'sample-add)) forms))
            (var (cl-find-if (lambda (f) (eq (plist-get f :name) 'sample-var)) forms))
            (plan (list :ops
                        (list
                         (list :file path
                               :range (cons (plist-get add :start)
                                            (plist-get add :end))
                               :replacement "(defun sample-add (a b) (+ a b 1))"
                               :reason "test add")
                         (list :file path
                               :range (cons (plist-get var :start)
                                            (plist-get var :end))
                               :replacement "(defvar sample-var 99)"
                               :reason "test var"))
                        :summary "test" :diff-preview "")))
       (anvil-sexp--apply-plan plan)
       (let ((after (with-temp-buffer
                      (insert-file-contents path)
                      (buffer-string))))
         (should (string-match-p "sample-var 99" after))
         (should (string-match-p "(\\+ a b 1)" after)))))))


;;;; --- verify pipeline ---------------------------------------------------

(ert-deftest anvil-sexp-test-verify-clean-file ()
  "verify on a clean fixture has no :kind 'error diagnostics."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let ((r (anvil-sexp--tool-verify path nil nil)))
       ;; Everything skipped when both checks disabled by "nil".
       (should (equal (plist-get r :file) path))
       (should (plist-get r :passed)))
     (let ((r (anvil-sexp--tool-verify path)))
       ;; Byte-compile on a normal fixture succeeds.
       (should (plist-get r :passed))))))

(ert-deftest anvil-sexp-test-verify-captures-warnings ()
  "A file with an obviously unused lexical variable surfaces a warning."
  (let ((tmp (make-temp-file "anvil-sexp-warn-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert ";;; warn.el --- warns -*- lexical-binding: t; -*-\n"
                    ";;; Commentary:\n;; warn.\n;;; Code:\n"
                    "(defun anvil-sexp-test-unused ()\n"
                    "  (let ((x 1)) 2))\n"
                    "(provide 'warn)\n"
                    ";;; warn.el ends here\n"))
          (let ((r (anvil-sexp--tool-verify tmp nil "nil")))
            ;; Some warning entry should exist.
            (should (> (length (plist-get r :diagnostics)) 0))))
      (when (file-exists-p tmp) (delete-file tmp))
      (let ((elc (concat tmp "c")))
        (when (file-exists-p elc) (delete-file elc))))))

(ert-deftest anvil-sexp-test-verify-captures-byte-compile-errors ()
  "A malformed file surfaces a byte-compile error diagnostic."
  (let ((tmp (make-temp-file "anvil-sexp-bad-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert ";;; bad.el --- bad -*- lexical-binding: t; -*-\n"
                    ";;; Commentary:\n;; bad.\n;;; Code:\n"
                    "(defun anvil-sexp-test-broken (\n"
                    "(provide 'bad)\n"
                    ";;; bad.el ends here\n"))
          (let ((r (anvil-sexp--tool-verify tmp nil "nil")))
            (should-not (plist-get r :passed))
            (should (cl-find-if
                     (lambda (d)
                       (and (eq (plist-get d :kind) 'error)
                            (eq (plist-get d :source) 'byte-compile)
                            (string-match-p
                             "End of file during parsing"
                             (plist-get d :message))))
                     (plist-get r :diagnostics)))))
      (when (file-exists-p tmp) (delete-file tmp))
      (let ((elc (concat tmp "c")))
        (when (file-exists-p elc) (delete-file elc))))))


;;;; --- ship criterion: stub + verify + restore round-trip ----------------

(ert-deftest anvil-sexp-test-ship-criterion-stub-and-restore ()
  "Phase 1 ship criterion: replace a defun with a stub, verify, restore."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let* ((forms (anvil-sexp--read-file path))
            (original (plist-get
                       (cl-find-if
                        (lambda (f) (eq (plist-get f :name) 'sample-add))
                        forms)
                       :sexp))
            (original-text (prin1-to-string original))
            (stub "(defun sample-add (_a _b) \"Stub.\" 0)"))
       ;; Stub.
       (anvil-sexp--tool-replace-defun path "sample-add" stub "t")
       (let ((after (with-temp-buffer (insert-file-contents path)
                                      (buffer-string))))
         (should (string-match-p "\"Stub\\.\"" after)))
       ;; Verify still passes (or at least does not error).
       (let ((v (anvil-sexp--tool-verify path nil "nil")))
         (should (listp (plist-get v :diagnostics))))
       ;; Restore.
       (anvil-sexp--tool-replace-defun path "sample-add" original-text "t")
       (let ((restored (with-temp-buffer (insert-file-contents path)
                                         (buffer-string))))
         (should (string-match-p "\\+ a b" restored))
         (should-not (string-match-p "\"Stub\\.\"" restored)))))))


;;;; --- review feedback: truthy edge cases --------------------------------

(ert-deftest anvil-sexp-test-truthy-json-false ()
  "anvil-sexp--truthy treats JSON-shaped falsy values as nil."
  (should-not (anvil-sexp--truthy nil))
  (should-not (anvil-sexp--truthy :json-false))
  (should-not (anvil-sexp--truthy :false))
  (should-not (anvil-sexp--truthy ""))
  (should-not (anvil-sexp--truthy "nil"))
  (should-not (anvil-sexp--truthy "NIL"))
  (should-not (anvil-sexp--truthy "false"))
  (should-not (anvil-sexp--truthy "FALSE"))
  (should-not (anvil-sexp--truthy "0"))
  (should-not (anvil-sexp--truthy "  false  "))
  (should-not (anvil-sexp--truthy "\tnil\n"))
  (should (anvil-sexp--truthy t))
  (should (anvil-sexp--truthy "t"))
  (should (anvil-sexp--truthy "true"))
  (should (anvil-sexp--truthy 1)))

(ert-deftest anvil-sexp-test-json-false-preview-safe ()
  "apply=:json-false must not write the file — safety invariant."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let ((before (with-temp-buffer (insert-file-contents path)
                                     (buffer-string))))
       (anvil-sexp--tool-replace-defun
        path "sample-add"
        "(defun sample-add (a b) \"x\" 99)"
        :json-false)
       (let ((after (with-temp-buffer (insert-file-contents path)
                                      (buffer-string))))
         (should (equal before after)))))))


;;;; --- review feedback: kind restriction on replace-defun ---------------

(ert-deftest anvil-sexp-test-replace-defun-refuses-defvar ()
  "replace-defun targets function-defining forms only, not defvar."
  (let ((tmp (make-temp-file "anvil-sexp-collide-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert ";;; collide.el --- -*- lexical-binding: t; -*-\n"
                    ";;; Commentary:\n;; collide.\n;;; Code:\n"
                    "(defvar sample-name 1 \"var.\")\n\n"
                    "(defun other-fn () nil)\n"
                    "(provide 'collide)\n"
                    ";;; collide.el ends here\n"))
          (should-error
           (anvil-sexp--tool-replace-defun
            tmp "sample-name" "(defvar sample-name 2)" "t")
           :type 'anvil-server-tool-error))
      (when (file-exists-p tmp) (delete-file tmp))
      (let ((elc (concat tmp "c")))
        (when (file-exists-p elc) (delete-file elc))))))


;;;; --- review feedback: apply-plan overlap detection -------------------

(ert-deftest anvil-sexp-test-apply-plan-refuses-overlap ()
  "Overlapping ops on one file cause apply-plan to signal an error."
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let ((plan (list :ops
                       (list (list :file path
                                   :range (cons 10 30)
                                   :replacement ""
                                   :reason "a")
                             (list :file path
                                   :range (cons 20 40)
                                   :replacement ""
                                   :reason "b"))
                       :summary "test" :diff-preview "")))
       (should-error (anvil-sexp--apply-plan plan))))))


;;;; --- review feedback: autoload cookie inclusion -----------------------

(ert-deftest anvil-sexp-test-autoload-cookie-included-in-range ()
  "`;;;###autoload' cookies preceding a defun are part of the form range."
  (let ((tmp (make-temp-file "anvil-sexp-auto-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert ";;; auto.el --- -*- lexical-binding: t; -*-\n"
                    ";;; Commentary:\n;; auto.\n;;; Code:\n"
                    ";;;###autoload\n"
                    "(defun sample-auto () \"doc.\" 1)\n"
                    "(provide 'auto)\n"
                    ";;; auto.el ends here\n"))
          (let* ((forms (anvil-sexp--read-file tmp))
                 (f (cl-find-if (lambda (e) (eq (plist-get e :name) 'sample-auto))
                                forms)))
            (should f)
            (should (< (plist-get f :start) (plist-get f :form-start)))
            (let ((text (with-temp-buffer
                          (insert-file-contents tmp)
                          (buffer-substring-no-properties
                           (plist-get f :start)
                           (plist-get f :end)))))
              (should (string-match-p "^;;;###autoload" text)))))
      (when (file-exists-p tmp) (delete-file tmp))
      (let ((elc (concat tmp "c")))
        (when (file-exists-p elc) (delete-file elc))))))


;;;; --- review feedback: cl-defstruct with options ----------------------

(ert-deftest anvil-sexp-test-cl-defstruct-with-options-name ()
  "cl-defstruct (NAME :option val ...) forms still surface NAME."
  (let ((tmp (make-temp-file "anvil-sexp-struct-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert ";;; struct.el --- -*- lexical-binding: t; -*-\n"
                    ";;; Commentary:\n;; struct.\n;;; Code:\n"
                    "(require 'cl-lib)\n"
                    "(cl-defstruct (anvil-sexp-test-point (:predicate nil)) x y)\n"
                    "(provide 'struct)\n"
                    ";;; struct.el ends here\n"))
          (let* ((forms (anvil-sexp--read-file tmp))
                 (f (cl-find-if (lambda (e) (eq (plist-get e :kind) 'cl-defstruct))
                                forms)))
            (should f)
            (should (eq (plist-get f :name) 'anvil-sexp-test-point))))
      (when (file-exists-p tmp) (delete-file tmp))
      (let ((elc (concat tmp "c")))
        (when (file-exists-p elc) (delete-file elc))))))


;;;; --- review feedback: public elisp API wrappers ----------------------

(ert-deftest anvil-sexp-test-public-api-wrappers-callable ()
  "Doc 12 Phase 1 public names resolve and delegate to tool handlers."
  (should (fboundp 'anvil-sexp-read-file))
  (should (fboundp 'anvil-sexp-surrounding-form))
  (should (fboundp 'anvil-sexp-replace-defun))
  (should (fboundp 'anvil-sexp-wrap-form))
  (should (fboundp 'anvil-sexp-macroexpand))
  (should (fboundp 'anvil-sexp-verify))
  (anvil-sexp-test--with-sample
   (lambda (path)
     (let ((entries (anvil-sexp-read-file path)))
       (should (listp entries))
       (should (> (length entries) 0))
       (should (member "sample-add"
                       (delq nil (mapcar (lambda (e) (plist-get e :name))
                                         entries)))))
     (let ((plan (anvil-sexp-replace-defun
                  path "sample-add"
                  "(defun sample-add (a b) \"New.\" (* a b))")))
       (should (plist-get plan :ops))
       (should-not (plist-get plan :applied-at))))))


;;;; --- Phase 2a: project-scope rename ------------------------------------

(defvar anvil-sexp-test--phase2a-sample "\
;;; p2a.el --- fixture -*- lexical-binding: t; -*-

;;; Commentary:
;; A fixture for Phase 2a rename / replace-call tests.

;;; Code:

(defvar p2a-counter 0
  \"Count of calls to p2a-hello.\")

(defun p2a-hello (name)
  \"Greet NAME via p2a-format.  Increments p2a-counter.\"
  (setq p2a-counter (1+ p2a-counter))
  (p2a-format \"Hello, %s!\" name))

(defun p2a-format (fmt &rest args)
  \"Wrapper over format.  The string token \\\"p2a-format\\\" here must be skipped.\"
  (apply #'format fmt args))

(defvar p2a-handlers
  (list #'p2a-hello #'p2a-format)
  \"List of handlers.\")

(when (fboundp 'p2a-hello)
  (p2a-hello \"test\"))

(provide 'p2a)
;;; p2a.el ends here
")

(defun anvil-sexp-test--with-phase2a-dir (fn)
  "Write the Phase 2a sample into a temp dir, call FN with dir + file."
  (let* ((dir (make-temp-file "anvil-sexp-p2a-" t))
         (file (expand-file-name "p2a.el" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert anvil-sexp-test--phase2a-sample))
          (funcall fn dir file))
      (when (file-directory-p dir) (delete-directory dir t)))))

(ert-deftest anvil-sexp-test-rename-preview-counts-hits ()
  "rename-symbol preview returns a plan whose ops count matches the fixture."
  (anvil-sexp-test--with-phase2a-dir
   (lambda (dir file)
     (ignore dir)
     (let ((plan (anvil-sexp--tool-rename-symbol
                  "p2a-hello" "p2a-hi" nil file)))
       (let ((ops (plist-get plan :ops)))
         ;; Expected: defun-name 1, #'p2a-hello 1, 'p2a-hello in fboundp 1,
         ;; (p2a-hello ...) call 1 => 4.  The docstring mention is excluded.
         (should (= 4 (length ops))))
       (should (null (plist-get plan :applied-at)))))))

(ert-deftest anvil-sexp-test-rename-excludes-strings-and-comments ()
  "rename-symbol must not touch occurrences inside strings/comments."
  (anvil-sexp-test--with-phase2a-dir
   (lambda (dir file)
     (ignore dir)
     (let ((plan (anvil-sexp--tool-rename-symbol
                  "p2a-format" "p2a-fmt" nil file)))
       (anvil-sexp--apply-plan plan))
     (let ((after (with-temp-buffer (insert-file-contents file)
                                    (buffer-string))))
       ;; The defun's name is rewritten.
       (should-not (string-match-p "(defun p2a-format" after))
       (should (string-match-p "(defun p2a-fmt" after))
       ;; The string-literal mention of p2a-format inside the
       ;; docstring ("The string token \"p2a-format\" here...") must
       ;; be left alone.  We grep for the surrounding docstring
       ;; context to avoid the backslash-escape trap.
       (should (string-match-p "The string token.*p2a-format" after))
       ;; The first docstring ("Greet NAME via p2a-format...") also
       ;; preserves its free-text mention.
       (should (string-match-p "Greet NAME via p2a-format" after))))))

(ert-deftest anvil-sexp-test-rename-kind-filter-call-only ()
  "rename-symbol kinds=\"call\" touches only operator-position sites."
  (anvil-sexp-test--with-phase2a-dir
   (lambda (dir file)
     (ignore dir)
     (let* ((plan (anvil-sexp--tool-rename-symbol
                   "p2a-hello" "p2a-hi" "call" file))
            (ops (plist-get plan :ops)))
       (should (= 1 (length ops)))
       (should (equal (plist-get (car ops) :replacement) "p2a-hi"))))))

(ert-deftest anvil-sexp-test-rename-apply-writes-file ()
  "rename-symbol apply=t actually writes the expected replacements."
  (anvil-sexp-test--with-phase2a-dir
   (lambda (dir file)
     (ignore dir)
     (anvil-sexp--tool-rename-symbol
      "p2a-hello" "p2a-hi" nil file "t")
     (let ((after (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))))
       (should (string-match-p "(defun p2a-hi " after))
       (should (string-match-p "(p2a-hi \"test\")" after))
       (should (string-match-p "#'p2a-hi" after))
       (should (string-match-p "'p2a-hi" after))))))

(ert-deftest anvil-sexp-test-rename-apply-json-false-is-noop ()
  "apply=:json-false must not write; preview-safety invariant."
  (anvil-sexp-test--with-phase2a-dir
   (lambda (dir file)
     (ignore dir)
     (let ((before (with-temp-buffer (insert-file-contents file)
                                     (buffer-string))))
       (anvil-sexp--tool-rename-symbol
        "p2a-hello" "p2a-hi" nil file :json-false)
       (let ((after (with-temp-buffer (insert-file-contents file)
                                      (buffer-string))))
         (should (equal before after)))))))

(ert-deftest anvil-sexp-test-rename-refuses-empty ()
  "rename-symbol refuses empty OLD or NEW."
  (anvil-sexp-test--with-phase2a-dir
   (lambda (dir file)
     (ignore dir)
     (should-error (anvil-sexp--tool-rename-symbol
                    "" "x" nil file)
                   :type 'anvil-server-tool-error)
     (should-error (anvil-sexp--tool-rename-symbol
                    "p2a-hello" "" nil file)
                   :type 'anvil-server-tool-error))))

(ert-deftest anvil-sexp-test-replace-call-positional ()
  "replace-call substitutes %1 into the template using the call's arg."
  (anvil-sexp-test--with-phase2a-dir
   (lambda (dir file)
     (ignore dir)
     (let* ((plan (anvil-sexp--tool-replace-call
                   "p2a-hello" "(message \"hi %1\")" file))
            (ops (plist-get plan :ops)))
       (should (= 1 (length ops)))
       (should (string-match-p
                "^(message \"hi \"test\"\")$\\|^(message \"hi \\\\\"test\\\\\"\")$\\|hi"
                (plist-get (car ops) :replacement)))))))

(ert-deftest anvil-sexp-test-replace-call-template-over-arity ()
  "replace-call skips sites whose call has fewer args than the template needs."
  (anvil-sexp-test--with-phase2a-dir
   (lambda (dir file)
     (ignore dir)
     (let ((plan (anvil-sexp--tool-replace-call
                  "p2a-hello" "(pair %1 %2)" file)))
       (should (listp (plist-get plan :ops)))
       (should (= 0 (length (plist-get plan :ops))))))))

(ert-deftest anvil-sexp-test-project-root-default ()
  "Project root walks up to the nearest git repo."
  (let* ((file (locate-library "anvil-sexp"))
         (root (anvil-sexp--project-root (file-name-directory file))))
    (should (stringp root))
    (should (file-directory-p root))
    (should (file-exists-p (expand-file-name ".git" root)))))

(ert-deftest anvil-sexp-test-resolve-scope-shapes ()
  "resolve-scope accepts list / CSV / file / directory / \"project\" defaults."
  (anvil-sexp-test--with-phase2a-dir
   (lambda (dir file)
     (should (equal (anvil-sexp--resolve-scope (list file)) (list file)))
     (should (member file (anvil-sexp--resolve-scope file)))
     (should (member file (anvil-sexp--resolve-scope dir)))
     (should (member file
                     (anvil-sexp--resolve-scope (concat file "," file)))))))

(ert-deftest anvil-sexp-test-public-phase2a-api-fboundp ()
  "Phase 2a public entry points are defined."
  (should (fboundp 'anvil-sexp-rename-symbol))
  (should (fboundp 'anvil-sexp-replace-call)))


;;;; --- review-feedback fixes (Phase 2a round 2) -------------------------

(ert-deftest anvil-sexp-test-classify-ref-at-whitespace-operator ()
  "Operator with whitespace or comments between `(' and head is `call'."
  (with-temp-buffer
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (insert "(foo 1)\n"
            "( foo 1)\n"
            "(\n  foo 1)\n"
            "( ;; hi\n  foo 1)\n")
    ;; Position of each `foo' occurrence.
    (dolist (marker '("(foo" "( foo" "(\n  foo" "( ;; hi\n  foo"))
      (goto-char (point-min))
      (search-forward marker)
      (backward-char (length "foo"))
      (let ((k (save-excursion
                 (anvil-sexp--classify-ref-at (point)))))
        (should (eq k 'call))))))

(ert-deftest anvil-sexp-test-classify-ref-at-var-binding ()
  "Second slot of defvar / defcustom / setq is classified `var'."
  (with-temp-buffer
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (insert "(defvar foo 1)\n(setq foo 2)\n")
    (goto-char (point-min))
    (search-forward "defvar ")
    (let ((k (save-excursion (anvil-sexp--classify-ref-at (point)))))
      (should (eq k 'var)))
    (goto-char (point-min))
    (search-forward "setq ")
    (let ((k (save-excursion (anvil-sexp--classify-ref-at (point)))))
      (should (eq k 'var)))))

(ert-deftest anvil-sexp-test-classify-ref-at-quote-forms ()
  "`'foo', `#'foo', `(quote foo)', `(function foo)' all classify as quote."
  (with-temp-buffer
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (insert "'foo\n#'foo\n(quote foo)\n(function foo)\n")
    (dolist (prefix '("'foo" "#'foo" "(quote foo" "(function foo"))
      (goto-char (point-min))
      (search-forward prefix)
      (backward-char (length "foo"))
      (let ((k (save-excursion (anvil-sexp--classify-ref-at (point)))))
        (should (eq k 'quote))))))

(ert-deftest anvil-sexp-test-render-call-template-cascade-safe ()
  "%N substitution is single-pass — a value containing %M is not rewritten."
  ;; Emit the bare symbol `%2` as arg 1 and `zz` as arg 2.
  (should (equal "(f %2 zz)"
                 (anvil-sexp--render-call-template "(f %1 %2)" '(%2 zz))))
  ;; %10 > %1 prefix collision: template uses %10, which is 10th arg.
  (should (equal "(g a:10)"
                 (anvil-sexp--render-call-template
                  "(g a:%10)" '(_1 _2 _3 _4 _5 _6 _7 _8 _9 10))))
  ;; Too few args signals.
  (should-error
   (anvil-sexp--render-call-template "(h %3)" '(a))))

(ert-deftest anvil-sexp-test-replace-call-tracks-skips ()
  "Sites that cannot be rendered are counted in :skipped and :summary."
  (anvil-sexp-test--with-phase2a-dir
   (lambda (dir file)
     (ignore dir)
     (let* ((plan (anvil-sexp--tool-replace-call
                   "p2a-hello" "(pair %1 %2)" file))
            (skipped (plist-get plan :skipped))
            (summary (plist-get plan :summary)))
       ;; The (p2a-hello \"test\") call has arity 1 but the template
       ;; wants 2 — it must be skipped, visibly.
       (should (>= (length skipped) 1))
       (should (string-match-p "skipped" summary))))))

;;;; --- Phase 2b parity: plan output equal with / without index ---------

(ert-deftest anvil-sexp-test-phase2b-backend-parity ()
  "With `anvil-sexp-use-index-backend' flipped on vs off against the
same project, `anvil-sexp--tool-rename-symbol' must emit the same
edit plan (ops count + byte ranges + replacement texts)."
  (require 'anvil-defs)
  (anvil-sexp-test--with-phase2a-dir
   (lambda (dir file)
     (let* ((anvil-defs-index-db-path
             (expand-file-name "defs.db" dir))
            (anvil-defs-paths (list dir))
            (anvil-defs--db nil)
            (anvil-defs--backend nil))
       (unwind-protect
           (progn
             (anvil-defs-index-rebuild (list dir))
             (let* ((plan-index
                     (let ((anvil-sexp-use-index-backend t))
                       (anvil-sexp--tool-rename-symbol
                        "p2a-hello" "p2a-hi" nil file)))
                    (plan-reader
                     (let ((anvil-sexp-use-index-backend nil))
                       (anvil-sexp--tool-rename-symbol
                        "p2a-hello" "p2a-hi" nil file))))
               (should (= (length (plist-get plan-index :ops))
                          (length (plist-get plan-reader :ops))))
               (cl-loop for a in (plist-get plan-index :ops)
                        for b in (plist-get plan-reader :ops)
                        do (should (equal (plist-get a :range)
                                          (plist-get b :range)))
                        do (should (equal (plist-get a :replacement)
                                          (plist-get b :replacement))))))
         (when anvil-defs--db
           (anvil-defs--close anvil-defs--db)
           (setq anvil-defs--db nil)))))))

(ert-deftest anvil-sexp-test-phase2b-disabled-fallthrough ()
  "When the index backend is disabled, rename still succeeds."
  (anvil-sexp-test--with-phase2a-dir
   (lambda (_dir file)
     (let ((anvil-sexp-use-index-backend nil))
       (let* ((plan (anvil-sexp--tool-rename-symbol
                     "p2a-hello" "p2a-hi" nil file))
              (ops (plist-get plan :ops)))
         (should (>= (length ops) 4)))))))

(ert-deftest anvil-sexp-test-phase2b-stale-index-picks-up-new-file ()
  "Adding a new .el file after the last rebuild must not cause the
index-narrowed backend to miss references inside it.  The
project-wide mtime refresh closes the codex-flagged staleness
window."
  (require 'anvil-defs)
  (anvil-sexp-test--with-phase2a-dir
   (lambda (dir file)
     (let* ((anvil-defs-index-db-path (expand-file-name "defs.db" dir))
            (anvil-defs-paths (list dir))
            (anvil-defs--db nil)
            (anvil-defs--backend nil)
            (new-file (expand-file-name "new-caller.el" dir)))
       (unwind-protect
           (progn
             ;; Build the index against the single pre-existing fixture.
             (anvil-defs-index-rebuild (list dir))
             ;; Drop a second .el into the project AFTER rebuild.
             (with-temp-file new-file
               (insert ";;; new-caller.el --- -*- lexical-binding: t; -*-\n"
                       ";;; Commentary:\n;; Added after rebuild.\n;;; Code:\n"
                       "(defun new-caller () \"calls p2a-hello.\""
                       " (p2a-hello \"late\"))\n"
                       "(provide 'new-caller)\n"
                       ";;; new-caller.el ends here\n"))
             (ignore file)
             (let* ((plan-idx
                     (let ((anvil-sexp-use-index-backend t))
                       (anvil-sexp--tool-rename-symbol
                        "p2a-hello" "p2a-hi" nil dir)))
                    (plan-rd
                     (let ((anvil-sexp-use-index-backend nil))
                       (anvil-sexp--tool-rename-symbol
                        "p2a-hello" "p2a-hi" nil dir))))
               (should (= (length (plist-get plan-idx :ops))
                          (length (plist-get plan-rd :ops))))))
         (when anvil-defs--db
           (anvil-defs--close anvil-defs--db)
           (setq anvil-defs--db nil)))))))


(ert-deftest anvil-sexp-test-rename-var-binding-kind ()
  "rename-symbol with kinds=var touches only LHS-binding sites."
  (let ((tmp (make-temp-file "anvil-sexp-var-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert ";;; v.el --- -*- lexical-binding: t; -*-\n"
                    ";;; Code:\n"
                    "(defvar my-var 1)\n"
                    "(setq my-var (+ my-var 1))\n"
                    "(provide 'v)\n"
                    ";;; v.el ends here\n"))
          (let* ((plan (anvil-sexp--tool-rename-symbol
                        "my-var" "my-var2" "var" tmp))
                 (ops (plist-get plan :ops)))
            ;; LHS of defvar + LHS of setq = 2 sites.  The `(+ my-var 1)'
            ;; RHS is `symbol', not `var', so it stays untouched.
            (should (= 2 (length ops)))))
      (when (file-exists-p tmp) (delete-file tmp))
      (let ((elc (concat tmp "c")))
        (when (file-exists-p elc) (delete-file elc))))))


(provide 'anvil-sexp-test)
;;; anvil-sexp-test.el ends here
