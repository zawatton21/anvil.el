;;; anvil-defs-test.el --- Tests for anvil-defs -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for Doc 11 Phase 1 + Phase 2 of `anvil-defs'.  Each test
;; drives a private SQLite DB under a temp directory so the real
;; user-level DB (~/.emacs.d/anvil-defs-index.db) is untouched.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-defs)


;;;; --- fixture helpers ----------------------------------------------------

(defvar anvil-defs-test--fixture "\
;;; fx.el --- test fixture -*- lexical-binding: t; -*-

;;; Commentary:
;; fixture for anvil-defs tests.

;;; Code:

(require 'subr-x)

(defvar fx-counter 0
  \"Count of greetings emitted by fx-greet.\")

(defun fx-greet (name &optional title)
  \"Greet NAME optionally prefixed by TITLE.
Note: this docstring mentions fx-greet but the string token
must not be counted as a reference.\"
  (setq fx-counter (1+ fx-counter))
  (format \"Hello %s%s\" (or title \"\") name))

(defun fx-greet-variadic (name &rest rest)
  \"Variadic greeting.\"
  (apply #'fx-greet name rest))

(defmacro fx-when-greeting (pred &rest body)
  \"Run BODY when PRED is truthy.\"
  (declare (indent 1))
  `(when ,pred ,@body))

(defun fx-call-greet ()
  \"Exercises a call to fx-greet and a quoted reference.\"
  (fx-greet \"world\")
  (let ((handlers (list #'fx-greet 'fx-greet-variadic)))
    (ignore handlers)))

(provide 'fx)
;;; fx.el ends here
")

(defun anvil-defs-test--with-fixture (fn)
  "Create a temp dir with a single fx.el fixture, call FN with (dir file)."
  (let* ((dir (make-temp-file "anvil-defs-test-" t))
         (file (expand-file-name "fx.el" dir))
         (db (expand-file-name "anvil-defs.db" dir))
         (anvil-defs-index-db-path db)
         (anvil-defs-paths (list dir))
         (anvil-defs--db nil)
         (anvil-defs--backend nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert anvil-defs-test--fixture))
          (funcall fn dir file))
      (when anvil-defs--db
        (anvil-defs--close anvil-defs--db)
        (setq anvil-defs--db nil))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(defun anvil-defs-test--rebuild (dir)
  "Run rebuild against DIR, return the stats plist."
  (anvil-defs-index-rebuild (list dir)))


;;;; --- smoke --------------------------------------------------------------

(ert-deftest anvil-defs-test-feature-provided ()
  (should (featurep 'anvil-defs)))

(ert-deftest anvil-defs-test-enable-disable-callable ()
  (should (fboundp 'anvil-defs-enable))
  (should (fboundp 'anvil-defs-disable)))


;;;; --- rebuild ------------------------------------------------------------

(ert-deftest anvil-defs-test-rebuild-empty-dir ()
  "Rebuilding an empty directory yields all-zero counts."
  (let* ((dir (make-temp-file "anvil-defs-empty-" t))
         (db (expand-file-name "db.sqlite" dir))
         (anvil-defs-index-db-path db)
         (anvil-defs-paths (list dir))
         (anvil-defs--db nil)
         (anvil-defs--backend nil))
    (unwind-protect
        (let ((r (anvil-defs-index-rebuild (list dir))))
          (should (= 0 (plist-get r :files)))
          (should (= 0 (plist-get r :defs)))
          (should (= 0 (plist-get r :refs)))
          (should (= 0 (plist-get r :features))))
      (when anvil-defs--db
        (anvil-defs--close anvil-defs--db)
        (setq anvil-defs--db nil))
      (when (file-directory-p dir) (delete-directory dir t)))))

(ert-deftest anvil-defs-test-rebuild-fixture-counts ()
  "Rebuild against fx.el finds the expected kinds of rows."
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (let ((r (anvil-defs-test--rebuild dir)))
       (should (= 1 (plist-get r :files)))
       (should (>= (plist-get r :defs) 4))   ; fx-counter, fx-greet, fx-greet-variadic, fx-when-greeting, fx-call-greet
       (should (>= (plist-get r :refs) 1))
       (should (>= (plist-get r :features) 2)) ; require subr-x + provide fx
       (should (numberp (plist-get r :duration-ms)))))))


;;;; --- search -------------------------------------------------------------

(ert-deftest anvil-defs-test-search-exact ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let ((hits (anvil-defs-search "fx-greet")))
       (should (= 1 (length hits)))
       (should (equal "fx-greet" (plist-get (car hits) :name)))
       (should (equal "defun" (plist-get (car hits) :kind)))))))

(ert-deftest anvil-defs-test-search-fuzzy ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let ((hits (anvil-defs-search "greet" :fuzzy t)))
       (should (>= (length hits) 3))
       (should (member "fx-greet"
                       (mapcar (lambda (h) (plist-get h :name)) hits)))))))

(ert-deftest anvil-defs-test-search-kind-filter ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let ((only-defvars
            (anvil-defs-search "fx" :kind 'defvar :fuzzy t)))
       (should (> (length only-defvars) 0))
       (dolist (h only-defvars)
         (should (equal (plist-get h :kind) "defvar")))))))


;;;; --- references --------------------------------------------------------

(ert-deftest anvil-defs-test-references-covers-call-and-quote ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let* ((hits (anvil-defs-references "fx-greet"))
            (kinds (delete-dups (mapcar (lambda (h) (plist-get h :kind))
                                        hits))))
       (should (member "call" kinds))
       (should (member "quote" kinds))))))

(ert-deftest anvil-defs-test-references-kind-filter ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let ((calls-only (anvil-defs-references "fx-greet" :kind "call"))
           (quotes-only (anvil-defs-references "fx-greet" :kind "quote")))
       (should (> (length calls-only) 0))
       (dolist (h calls-only)
         (should (equal "call" (plist-get h :kind))))
       (dolist (h quotes-only)
         (should (equal "quote" (plist-get h :kind))))))))

(ert-deftest anvil-defs-test-references-excludes-docstring ()
  "The fx-greet token inside the docstring body must not be a ref."
  (anvil-defs-test--with-fixture
   (lambda (dir file)
     (anvil-defs-test--rebuild dir)
     (let* ((hits (anvil-defs-references "fx-greet"))
            (lines (mapcar (lambda (h) (plist-get h :line)) hits)))
       ;; The docstring mention lives on the line carrying "but the"
       ;; — read the fixture to find it and assert no ref is on it.
       (with-temp-buffer
         (insert-file-contents file)
         (goto-char (point-min))
         (when (re-search-forward "mentions fx-greet but" nil t)
           (let ((line (line-number-at-pos (match-beginning 0))))
             (should-not (memq line lines)))))))))


;;;; --- signature --------------------------------------------------------

(ert-deftest anvil-defs-test-signature-optional ()
  "fx-greet has 1 required arg + 1 &optional => (1 . 2)."
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let ((sig (anvil-defs-signature "fx-greet")))
       (should (equal 1 (plist-get sig :arity-min)))
       (should (equal 2 (plist-get sig :arity-max)))
       (should (equal "defun" (plist-get sig :kind)))))))

(ert-deftest anvil-defs-test-signature-rest ()
  "fx-greet-variadic has &rest => arity-max nil."
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let ((sig (anvil-defs-signature "fx-greet-variadic")))
       (should (equal 1 (plist-get sig :arity-min)))
       (should (null (plist-get sig :arity-max)))))))

(ert-deftest anvil-defs-test-signature-unknown ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (should (null (anvil-defs-signature "no-such-symbol-ever"))))))


;;;; --- who-requires -----------------------------------------------------

(ert-deftest anvil-defs-test-who-requires ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (anvil-defs-test--rebuild dir)
     (let ((files (anvil-defs-who-requires "subr-x")))
       (should (= 1 (length files)))
       (should (string-suffix-p "fx.el" (car files)))))))


;;;; --- index-status + refresh-if-stale ---------------------------------

(ert-deftest anvil-defs-test-index-status-matches-rebuild ()
  (anvil-defs-test--with-fixture
   (lambda (dir _file)
     (let ((built (anvil-defs-test--rebuild dir))
           (stat (anvil-defs-index-status)))
       (should (equal (plist-get built :files) (plist-get stat :files)))
       (should (equal (plist-get built :defs) (plist-get stat :defs)))
       (should (equal (plist-get built :refs) (plist-get stat :refs)))))))

(ert-deftest anvil-defs-test-refresh-if-stale-reingests-edit ()
  "Editing a file and calling refresh-if-stale picks the new def up."
  (anvil-defs-test--with-fixture
   (lambda (dir file)
     (anvil-defs-test--rebuild dir)
     (should-not (anvil-defs-signature "fx-brand-new"))
     ;; Edit the file — append a new defun — and advance mtime.
     (with-temp-buffer
       (insert-file-contents file)
       (goto-char (point-max))
       (re-search-backward ";;; fx.el ends here")
       (goto-char (match-beginning 0))
       (insert "(defun fx-brand-new () \"new.\" 42)\n\n")
       (write-region (point-min) (point-max) file nil 'silent))
     ;; `set-file-times' bumps mtime forward so refresh-if-stale sees it.
     (set-file-times file (time-add (current-time) 5))
     (let ((touched (anvil-defs-refresh-if-stale file)))
       (should (= 1 (length touched)))
       (should (anvil-defs-signature "fx-brand-new"))))))


;;;; --- trace-path (call graph) ------------------------------------------

(defvar anvil-defs-test--trace-fixture "\
;;; tc.el --- trace fixture -*- lexical-binding: t; -*-

;;; Commentary:
;; call chain: tc-a -> tc-b -> tc-c (leaf); tc-d -> tc-b (2nd caller).

;;; Code:

(defun tc-c ()
  \"Leaf — only calls a builtin.\"
  (+ 1 2))

(defun tc-b ()
  \"Mid — calls the leaf.\"
  (tc-c))

(defun tc-a ()
  \"Top — calls the mid.\"
  (tc-b))

(defun tc-d ()
  \"Second caller of tc-b.\"
  (tc-b))

(provide 'tc)
;;; tc.el ends here
")

(defun anvil-defs-test--with-trace-fixture (fn)
  "Create a temp dir with tc.el (a small call chain), rebuild, call FN with dir."
  (let* ((dir (make-temp-file "anvil-defs-trace-" t))
         (file (expand-file-name "tc.el" dir))
         (db (expand-file-name "anvil-defs.db" dir))
         (anvil-defs-index-db-path db)
         (anvil-defs-paths (list dir))
         (anvil-defs--db nil)
         (anvil-defs--backend nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert anvil-defs-test--trace-fixture))
          (anvil-defs-index-rebuild (list dir))
          (funcall fn dir))
      (when anvil-defs--db
        (anvil-defs--close anvil-defs--db)
        (setq anvil-defs--db nil))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(defun anvil-defs-test--names (rows)
  "Return the :name strings from trace ROWS."
  (mapcar (lambda (r) (plist-get r :name)) rows))

(defun anvil-defs-test--depth-of (rows name)
  "Return the :depth of the row in ROWS whose :name equals NAME."
  (plist-get (cl-find name rows
                      :key (lambda (r) (plist-get r :name)) :test #'equal)
             :depth))

(ert-deftest anvil-defs-test-trace-callers-direct ()
  "Direct callers of the leaf is the mid only; self is excluded."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let ((names (anvil-defs-test--names
                   (anvil-defs-trace-path "tc-c" :direction 'callers :depth 1))))
       (should (member "tc-b" names))
       (should-not (member "tc-a" names))
       (should-not (member "tc-c" names))))))

(ert-deftest anvil-defs-test-trace-callers-transitive ()
  "Transitive callers of the leaf cover the whole upstream chain with depths."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let* ((rows (anvil-defs-trace-path "tc-c" :direction 'callers :depth 3))
            (names (anvil-defs-test--names rows)))
       (should (member "tc-b" names))
       (should (member "tc-a" names))
       (should (member "tc-d" names))
       (should (= 1 (anvil-defs-test--depth-of rows "tc-b")))
       (should (= 2 (anvil-defs-test--depth-of rows "tc-a")))
       (should (= 2 (anvil-defs-test--depth-of rows "tc-d")))))))

(ert-deftest anvil-defs-test-trace-depth-cap ()
  "Depth 1 does not reach grand-callers."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let ((names (anvil-defs-test--names
                   (anvil-defs-trace-path "tc-c" :direction 'callers :depth 1))))
       (should (member "tc-b" names))
       (should-not (member "tc-a" names))))))

(ert-deftest anvil-defs-test-trace-callees ()
  "Callees of the top reach the mid then the leaf; all are index-defined."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let* ((rows (anvil-defs-trace-path "tc-a" :direction 'callees :depth 3))
            (names (anvil-defs-test--names rows)))
       (should (member "tc-b" names))
       (should (member "tc-c" names))
       (should (cl-every (lambda (r) (plist-get r :defined)) rows))))))

(ert-deftest anvil-defs-test-trace-internal-only-hides-builtins ()
  "internal-only drops builtin callees; nil includes them."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let ((internal (anvil-defs-test--names
                      (anvil-defs-trace-path "tc-c" :direction 'callees
                                             :internal-only t)))
           (raw (anvil-defs-test--names
                 (anvil-defs-trace-path "tc-c" :direction 'callees
                                        :internal-only nil))))
       (should (null internal))           ; tc-c only calls `+'
       (should (member "+" raw))))))

(ert-deftest anvil-defs-test-trace-tool-wrapper ()
  "The MCP wrapper parses string args and returns caller rows."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let ((names (anvil-defs-test--names
                   (anvil-defs--tool-trace-path "tc-b" "callers" "2"))))
       (should (member "tc-a" names))
       (should (member "tc-d" names))))))

(ert-deftest anvil-defs-test-trace-unknown-symbol ()
  "Tracing an unindexed symbol yields nil in both directions."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (should (null (anvil-defs-trace-path "no-such-symbol-xyz"
                                          :direction 'callers)))
     (should (null (anvil-defs-trace-path "no-such-symbol-xyz"
                                          :direction 'callees))))))


;;;; --- detect-changes (blast-radius) ------------------------------------

(defun anvil-defs-test--range-of (name)
  "Return (LINE . END-LINE) of the indexed definition NAME."
  (let ((h (car (anvil-defs-search name))))
    (cons (plist-get h :line) (plist-get h :end-line))))

(ert-deftest anvil-defs-test-detect-changes-maps-and-risks ()
  "A change inside tc-b maps to tc-b, finds its 2 callers, scores high."
  (anvil-defs-test--with-trace-fixture
   (lambda (dir)
     (let* ((file (expand-file-name "tc.el" dir))
            (res (anvil-defs-detect-changes
                  :changes (list (cons file (list (anvil-defs-test--range-of "tc-b"))))))
            (tcb (cl-find "tc-b" (plist-get res :changed)
                          :key (lambda (c) (plist-get c :name)) :test #'equal)))
       (should tcb)
       (should (plist-get tcb :public))
       (should (= 2 (plist-get tcb :direct-callers)))
       (should (equal "high" (plist-get tcb :risk)))   ; public + callers + untested
       (should (member "tc-a" (plist-get tcb :callers-sample)))
       (should (member "tc-d" (plist-get tcb :callers-sample)))
       (should (>= (plist-get res :total-impacted) 2))))))

(ert-deftest anvil-defs-test-detect-changes-blast-depth ()
  "Editing the leaf propagates up the chain at depth 2."
  (anvil-defs-test--with-trace-fixture
   (lambda (dir)
     (let* ((file (expand-file-name "tc.el" dir))
            (res (anvil-defs-detect-changes
                  :changes (list (cons file (list (anvil-defs-test--range-of "tc-c"))))
                  :depth 2))
            (tcc (cl-find "tc-c" (plist-get res :changed)
                          :key (lambda (c) (plist-get c :name)) :test #'equal)))
       (should tcc)
       ;; callers of tc-c within depth 2: tc-b (d1) + tc-a + tc-d (d2).
       (should (>= (plist-get tcc :impacted) 3))
       (should (>= (plist-get res :total-impacted) 3))))))

(ert-deftest anvil-defs-test-detect-changes-no-def-in-range ()
  "A change touching only the header comment maps to no symbol."
  (anvil-defs-test--with-trace-fixture
   (lambda (dir)
     (let* ((file (expand-file-name "tc.el" dir))
            (res (anvil-defs-detect-changes :changes (list (cons file '((1 . 1)))))))
       (should (null (plist-get res :changed)))
       (should (= 0 (plist-get res :total-impacted)))))))

(ert-deftest anvil-defs-test-detect-changes-git ()
  "End-to-end: a real `git diff' maps the edited body to its definition."
  (skip-unless (executable-find "git"))
  (let* ((dir (make-temp-file "anvil-defs-git-" t))
         (file (expand-file-name "tc.el" dir))
         (db (expand-file-name "anvil-defs.db" dir))
         (anvil-defs-index-db-path db)
         (anvil-defs-paths (list dir))
         (anvil-defs--db nil)
         (anvil-defs--backend nil)
         (default-directory (file-name-as-directory dir))
         (process-environment
          (append '("GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t"
                    "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@t")
                  process-environment)))
    (unwind-protect
        (progn
          (with-temp-file file (insert anvil-defs-test--trace-fixture))
          (call-process "git" nil nil nil "init" "-q")
          (call-process "git" nil nil nil
                        "config" "commit.gpgsign" "false")
          ;; Prove the parser is independent of user-configured diff prefixes.
          (call-process "git" nil nil nil
                        "config" "diff.mnemonicPrefix" "true")
          (call-process "git" nil nil nil "add" "tc.el")
          (call-process "git" nil nil nil "commit" "-q" "-m" "init")
          (anvil-defs-index-rebuild (list dir))
          ;; Edit tc-b's body, then bump mtime so refresh re-ingests.
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (re-search-forward "(tc-c)")
            (replace-match "(tc-c) (ignore 1)")
            (write-region (point-min) (point-max) file nil 'silent))
          (set-file-times file (time-add (current-time) 5))
          (let ((names (mapcar (lambda (c) (plist-get c :name))
                               (plist-get (anvil-defs-detect-changes
                                           :repo dir :rev "HEAD")
                                          :changed))))
            (should (member "tc-b" names))))
      (when anvil-defs--db
        (anvil-defs--close anvil-defs--db)
        (setq anvil-defs--db nil))
      (when (file-directory-p dir) (delete-directory dir t)))))


;;;; --- python (tree-sitter, Doc 57 Phase 3) -----------------------------

(defvar anvil-defs-test--py-fixture "\
def leaf():
    return 1

def mid():
    return leaf()

def top():
    return mid()

class Worker:
    def run(self):
        return mid()

def main():
    w = Worker()
    return w.run()
")

(defun anvil-defs-test--with-py-fixture (fn)
  "Index a temp Python fixture (python language enabled), call FN with dir."
  (let* ((dir (make-temp-file "anvil-defs-py-" t))
         (file (expand-file-name "fx.py" dir))
         (db (expand-file-name "anvil-defs.db" dir))
         (anvil-defs-index-db-path db)
         (anvil-defs-paths (list dir))
         (anvil-defs-languages '(python))
         (anvil-defs--db nil)
         (anvil-defs--backend nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert anvil-defs-test--py-fixture))
          (anvil-defs-index-rebuild (list dir))
          (funcall fn dir))
      (when anvil-defs--db
        (anvil-defs--close anvil-defs--db)
        (setq anvil-defs--db nil))
      (when (file-directory-p dir) (delete-directory dir t)))))

(defmacro anvil-defs-test--skip-unless-python ()
  "Inline guard: skip when tree-sitter / the python grammar is absent."
  '(skip-unless (and (fboundp 'treesit-available-p) (treesit-available-p)
                     (fboundp 'treesit-language-available-p)
                     (treesit-language-available-p 'python))))

(ert-deftest anvil-defs-test-py-rebuild-finds-defs ()
  "The python scanner indexes functions, classes and methods by kind."
  (anvil-defs-test--skip-unless-python)
  (anvil-defs-test--with-py-fixture
   (lambda (_dir)
     (should (equal "function" (plist-get (car (anvil-defs-search "mid")) :kind)))
     (should (equal "class" (plist-get (car (anvil-defs-search "Worker")) :kind)))
     (should (equal "method" (plist-get (car (anvil-defs-search "run")) :kind))))))

(ert-deftest anvil-defs-test-py-trace-callers ()
  "Callers of mid are the top-level fn and the method that call it."
  (anvil-defs-test--skip-unless-python)
  (anvil-defs-test--with-py-fixture
   (lambda (_dir)
     (let ((names (anvil-defs-test--names
                   (anvil-defs-trace-path "mid" :direction 'callers :depth 1))))
       (should (member "top" names))
       (should (member "run" names))))))

(ert-deftest anvil-defs-test-py-attribute-call ()
  "An attribute call `w.run()' is attributed to the run method."
  (anvil-defs-test--skip-unless-python)
  (anvil-defs-test--with-py-fixture
   (lambda (_dir)
     (let ((names (anvil-defs-test--names
                   (anvil-defs-trace-path "run" :direction 'callers :depth 1))))
       (should (member "main" names))))))

(ert-deftest anvil-defs-test-py-trace-callees-transitive ()
  "Callees of top reach mid then leaf across the call chain."
  (anvil-defs-test--skip-unless-python)
  (anvil-defs-test--with-py-fixture
   (lambda (_dir)
     (let ((names (anvil-defs-test--names
                   (anvil-defs-trace-path "top" :direction 'callees :depth 2))))
       (should (member "mid" names))
       (should (member "leaf" names))))))


;;;; --- javascript / typescript (tree-sitter, Doc 57 Phase 3 JS/TS) ------

(defmacro anvil-defs-test--skip-unless-grammar (lang)
  "Inline guard: skip the test when tree-sitter LANG grammar is absent."
  `(skip-unless (and (fboundp 'treesit-available-p) (treesit-available-p)
                     (fboundp 'treesit-language-available-p)
                     (treesit-language-available-p ,lang))))

(defun anvil-defs-test--with-src-fixture (filename content languages fn)
  "Index a temp FILENAME holding CONTENT with LANGUAGES enabled; call FN with dir."
  (let* ((dir (make-temp-file "anvil-defs-src-" t))
         (file (expand-file-name filename dir))
         (db (expand-file-name "anvil-defs.db" dir))
         (anvil-defs-index-db-path db)
         (anvil-defs-paths (list dir))
         (anvil-defs-languages languages)
         (anvil-defs--db nil)
         (anvil-defs--backend nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert content))
          (anvil-defs-index-rebuild (list dir))
          (funcall fn dir))
      (when anvil-defs--db
        (anvil-defs--close anvil-defs--db)
        (setq anvil-defs--db nil))
      (when (file-directory-p dir) (delete-directory dir t)))))

(defvar anvil-defs-test--js-fixture "\
function leaf() {
  return 1;
}

const mid = () => {
  return leaf();
};

function top() {
  return mid();
}

class Worker {
  run() {
    return mid();
  }
}

function main() {
  const w = new Worker();
  return w.run();
}
")

(ert-deftest anvil-defs-test-js-rebuild-finds-defs ()
  "The JS scanner indexes declarations, arrow bindings, classes and methods."
  (anvil-defs-test--skip-unless-grammar 'javascript)
  (anvil-defs-test--with-src-fixture
   "fx.js" anvil-defs-test--js-fixture '(javascript)
   (lambda (_dir)
     (should (equal "function" (plist-get (car (anvil-defs-search "leaf")) :kind)))
     (should (equal "function" (plist-get (car (anvil-defs-search "mid")) :kind))) ; arrow const
     (should (equal "class" (plist-get (car (anvil-defs-search "Worker")) :kind)))
     (should (equal "method" (plist-get (car (anvil-defs-search "run")) :kind))))))

(ert-deftest anvil-defs-test-js-trace-callers ()
  "Callers of mid are the top-level fn and the method that call it."
  (anvil-defs-test--skip-unless-grammar 'javascript)
  (anvil-defs-test--with-src-fixture
   "fx.js" anvil-defs-test--js-fixture '(javascript)
   (lambda (_dir)
     (let ((names (anvil-defs-test--names
                   (anvil-defs-trace-path "mid" :direction 'callers :depth 1))))
       (should (member "top" names))
       (should (member "run" names))))))

(ert-deftest anvil-defs-test-js-arrow-context-and-member-call ()
  "Arrow-binding callers and member-expression callees both resolve."
  (anvil-defs-test--skip-unless-grammar 'javascript)
  (anvil-defs-test--with-src-fixture
   "fx.js" anvil-defs-test--js-fixture '(javascript)
   (lambda (_dir)
     ;; leaf is called only from inside the arrow `mid' -> caller "mid".
     (should (member "mid" (anvil-defs-test--names
                            (anvil-defs-trace-path "leaf" :direction 'callers :depth 1))))
     ;; run is reached only via the member call `w.run()' in main.
     (should (member "main" (anvil-defs-test--names
                             (anvil-defs-trace-path "run" :direction 'callers :depth 1)))))))

(ert-deftest anvil-defs-test-js-trace-callees-transitive ()
  "Callees of top reach mid then leaf across the chain."
  (anvil-defs-test--skip-unless-grammar 'javascript)
  (anvil-defs-test--with-src-fixture
   "fx.js" anvil-defs-test--js-fixture '(javascript)
   (lambda (_dir)
     (let ((names (anvil-defs-test--names
                   (anvil-defs-trace-path "top" :direction 'callees :depth 2))))
       (should (member "mid" names))
       (should (member "leaf" names))))))

(ert-deftest anvil-defs-test-ts-dispatch ()
  "A .ts file routes through the typescript grammar and links calls."
  (anvil-defs-test--skip-unless-grammar 'typescript)
  (anvil-defs-test--with-src-fixture
   "fx.ts"
   "function tleaf(): number { return 1; }\nfunction tmid(): number { return tleaf(); }\n"
   '(typescript)
   (lambda (_dir)
     (should (equal "function" (plist-get (car (anvil-defs-search "tleaf")) :kind)))
     (should (member "tmid" (anvil-defs-test--names
                             (anvil-defs-trace-path "tleaf" :direction 'callers :depth 1)))))))


;;;; --- Phase 4: dead-code / caller-rank / import graph ------------------

(ert-deftest anvil-defs-test-dead-code ()
  "Unreferenced defuns are reported; referenced ones are not."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let ((names (anvil-defs-test--names (anvil-defs-dead-code))))
       (should (member "tc-a" names))      ; nothing calls tc-a
       (should (member "tc-d" names))      ; nothing calls tc-d
       (should-not (member "tc-b" names))  ; called by tc-a / tc-d
       (should-not (member "tc-c" names)))))) ; called by tc-b

(ert-deftest anvil-defs-test-dead-code-name-pattern ()
  "A name-pattern narrows dead-code to matching defs."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let ((names (anvil-defs-test--names
                   (anvil-defs-dead-code :name-pattern "tc-a"))))
       (should (member "tc-a" names))
       (should-not (member "tc-d" names))))))

(ert-deftest anvil-defs-test-callers-rank-desc ()
  "Hotspot ranking puts the most-called def first."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let ((top (car (anvil-defs-callers-rank :order 'desc :limit 2))))
       (should (equal "tc-b" (plist-get top :name)))   ; 2 callers
       (should (= 2 (plist-get top :callers)))))))

(ert-deftest anvil-defs-test-callers-rank-bounds ()
  "min / max bound the in-degree window."
  (anvil-defs-test--with-trace-fixture
   (lambda (_dir)
     (let ((min2 (anvil-defs-test--names (anvil-defs-callers-rank :min 2)))
           (zero (anvil-defs-test--names (anvil-defs-callers-rank :max 0))))
       (should (equal '("tc-b") min2))
       (should (member "tc-a" zero))
       (should (member "tc-d" zero))
       (should-not (member "tc-b" zero))))))

(ert-deftest anvil-defs-test-py-import-graph ()
  "Python imports populate `requires' features for who-requires."
  (anvil-defs-test--skip-unless-python)
  (anvil-defs-test--with-src-fixture
   "imp.py"
   "import os\nimport sys as system\nfrom collections import OrderedDict\n"
   '(python)
   (lambda (_dir)
     (should (= 1 (length (anvil-defs-who-requires "os"))))
     (should (= 1 (length (anvil-defs-who-requires "collections")))))))

(ert-deftest anvil-defs-test-js-import-graph ()
  "JS import specifiers populate `requires' features for who-requires."
  (anvil-defs-test--skip-unless-grammar 'javascript)
  (anvil-defs-test--with-src-fixture
   "imp.js"
   "import fs from \"fs\";\nimport { join } from \"path\";\nimport \"./side\";\n"
   '(javascript)
   (lambda (_dir)
     (should (= 1 (length (anvil-defs-who-requires "fs"))))
     (should (= 1 (length (anvil-defs-who-requires "path"))))
     (should (= 1 (length (anvil-defs-who-requires "./side")))))))


;;;; --- Phase 5: clustering / architecture -------------------------------

(defun anvil-defs-test--with-cluster-fixture (fn)
  "Index a temp 6-file fixture forming two file communities; call FN with dir.
Files a1/a2/a3 call each other (triangle); b1/b2/b3 likewise; one bridge
call fa1 -> fb1 (a1.el -> b1.el) joins them."
  (let* ((dir (make-temp-file "anvil-defs-cl-" t))
         (files '(("a1.el" . "(defun fa1 () (fa2) (fa3) (fb1))\n")
                  ("a2.el" . "(defun fa2 () (fa1) (fa3))\n")
                  ("a3.el" . "(defun fa3 () (fa1) (fa2))\n")
                  ("b1.el" . "(defun fb1 () (fb2) (fb3))\n")
                  ("b2.el" . "(defun fb2 () (fb1) (fb3))\n")
                  ("b3.el" . "(defun fb3 () (fb1) (fb2))\n")))
         (db (expand-file-name "anvil-defs.db" dir))
         (anvil-defs-index-db-path db)
         (anvil-defs-paths (list dir))
         (anvil-defs--db nil)
         (anvil-defs--backend nil))
    (unwind-protect
        (progn
          (dolist (f files)
            (with-temp-file (expand-file-name (car f) dir) (insert (cdr f))))
          (anvil-defs-index-rebuild (list dir))
          (funcall fn dir))
      (when anvil-defs--db
        (anvil-defs--close anvil-defs--db)
        (setq anvil-defs--db nil))
      (when (file-directory-p dir) (delete-directory dir t)))))

(defun anvil-defs-test--cluster-of (clusters name)
  "Return the cluster id whose members include NAME, or nil."
  (catch 'hit
    (dolist (cl clusters)
      (when (member name (plist-get cl :members))
        (throw 'hit (plist-get cl :id))))
    nil))

(ert-deftest anvil-defs-test-clusters-separates-communities ()
  "Community detection splits the two triangles despite the bridge."
  (anvil-defs-test--with-cluster-fixture
   (lambda (_dir)
     (let* ((clusters (anvil-defs-clusters :min-size 2))
            (ca (anvil-defs-test--cluster-of clusters "a1.el"))
            (ca2 (anvil-defs-test--cluster-of clusters "a2.el"))
            (cb (anvil-defs-test--cluster-of clusters "b1.el")))
       (should ca)
       (should cb)
       (should (equal ca ca2))          ; a1.el and a2.el cluster together
       (should-not (equal ca cb))))))   ; a-side and b-side are distinct

(ert-deftest anvil-defs-test-clusters-size ()
  "Each detected cluster spans the three files of its triangle."
  (anvil-defs-test--with-cluster-fixture
   (lambda (_dir)
     (let ((clusters (anvil-defs-clusters :min-size 2)))
       (should (= 2 (length clusters)))
       (dolist (cl clusters)
         (should (= 3 (plist-get cl :size))))))))

(ert-deftest anvil-defs-test-architecture-summary ()
  "The architecture overview reports totals, hotspots and >=2 clusters."
  (anvil-defs-test--with-cluster-fixture
   (lambda (_dir)
     (let ((arch (anvil-defs-architecture)))
       (should (>= (plist-get (plist-get arch :totals) :defs) 6))
       (should (>= (plist-get arch :cluster-count) 2))
       (should (>= (length (plist-get arch :hotspots)) 1))
       (should (>= (length (plist-get arch :clusters)) 2))))))


;;;; --- graph-exclude (plumbing skip-set) + auto-roots ------------------

(defvar anvil-defs-test--exclude-fixture
  (concat "(defun when (x) x)\n"          ; self-defined core form (nelisp-like)
          "(defun unless (x) x)\n"        ; self-defined, never called
          "(defun sf-user () (when 1) (sf-helper))\n"
          "(defun sf-helper () (when 2))\n"
          "(defun sf-dead () 42)\n")      ; real unused def
  "Fixture where core forms `when'/`unless' are redefined, as NeLisp does.")

(ert-deftest anvil-defs-test-graph-exclude-callers-rank ()
  "Self-defined plumbing (`when') is excluded from caller ranking."
  (anvil-defs-test--with-src-fixture
   "sf.el" anvil-defs-test--exclude-fixture '(elisp)
   (lambda (_dir)
     (let ((names (anvil-defs-test--names (anvil-defs-callers-rank :order 'desc))))
       (should (member "sf-helper" names))
       (should-not (member "when" names))))))   ; despite having the most callers

(ert-deftest anvil-defs-test-graph-exclude-trace ()
  "Plumbing callees are dropped from trace results."
  (anvil-defs-test--with-src-fixture
   "sf.el" anvil-defs-test--exclude-fixture '(elisp)
   (lambda (_dir)
     (let ((names (anvil-defs-test--names
                   (anvil-defs-trace-path "sf-user" :direction 'callees :depth 1))))
       (should (member "sf-helper" names))
       (should-not (member "when" names))))))

(ert-deftest anvil-defs-test-graph-exclude-dead-code ()
  "Plumbing names are never flagged dead; real unused defs still are."
  (anvil-defs-test--with-src-fixture
   "sf.el" anvil-defs-test--exclude-fixture '(elisp)
   (lambda (_dir)
     (let ((names (anvil-defs-test--names (anvil-defs-dead-code))))
       (should (member "sf-dead" names))
       (should-not (member "unless" names))))))  ; excluded despite being unused

(ert-deftest anvil-defs-test-auto-roots ()
  "auto-roots returns the git repos directly under <project-root>/dev."
  (let* ((base (make-temp-file "anvil-defs-auto-" t))
         (default-directory (file-name-as-directory base)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" base))
          (dolist (r '("repo1" "repo2"))
            (make-directory (expand-file-name (format "dev/%s/.git" r) base) t))
          (make-directory (expand-file-name "dev/notgit" base) t) ; no .git -> skipped
          (make-directory (expand-file-name "dev/repo1.wt-x/.git" base) t) ; backup name -> skipped
          (let ((roots (mapcar (lambda (p)
                                 (file-name-nondirectory (directory-file-name p)))
                               (anvil-defs--auto-roots))))
            (should (member "repo1" roots))
            (should (member "repo2" roots))
            (should-not (member "notgit" roots))
            (should-not (member "repo1.wt-x" roots))))
      (when (file-directory-p base) (delete-directory base t)))))


(provide 'anvil-defs-test)
;;; anvil-defs-test.el ends here
