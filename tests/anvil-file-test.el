;;; anvil-file-test.el --- Tests for anvil-file helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for json-object-add, file-ensure-import, and file-batch-across.
;; Uses temporary files; no dependency on anvil-server registration.

;;; Code:

(require 'ert)
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

;;; anvil-file-test.el ends here
