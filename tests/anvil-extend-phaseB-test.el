;;; anvil-extend-phaseB-test.el --- ERT for Phase B hot-reload (Doc 38 §3.B) -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT coverage for the Phase B transactional hot-reload + filenotify
;; auto-reload surface added on top of the Phase A scaffold generator.
;; Each test rebinds `anvil-extend-storage-dir' and
;; `anvil-extend-staging-root' to fresh temp directories so the suite
;; is hermetic and parallel-safe — Phase A's
;; `anvil-extend-test--with-storage' is reused for the storage dir and
;; a sibling macro covers the staging dir.
;;
;; The byte-compile-failure tests intentionally write hand-crafted
;; broken `<NAME>.el' on top of a freshly scaffolded one so that the
;; production rollback path runs against a real (not stubbed) failed
;; compile.  The load-failure tests do the same for a syntactically
;; valid but semantically broken `(error "boom")' top-level form.
;;
;; The filenotify test relies on backend availability; on a host
;; without a working backend the test reports a skip rather than a
;; spurious failure.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'filenotify)
(require 'anvil-extend)

;; Phase B keeps its fixtures self-contained (no `(require
;; 'anvil-extend-test)') so loading this file does not double-run the
;; Phase A suite when both are passed to a single ERT batch invocation.

;;;; --- fixtures -----------------------------------------------------------

(defmacro anvil-extend-phaseB-test--with-storage (var &rest body)
  "Bind VAR to a fresh storage dir and `let' `anvil-extend-storage-dir'.

The directory is removed at the end of BODY regardless of how it
exits.  Mirrors the Phase A fixture so the two test files stay
hermetic and independent of each other's load order."
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory
                 (make-temp-file "anvil-extend-phaseB-storage-" t)))
          (anvil-extend-storage-dir ,var))
     (unwind-protect
         (progn (ignore ,var) ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defmacro anvil-extend-phaseB-test--with-staging (var &rest body)
  "Bind VAR to a fresh staging dir and `let' `anvil-extend-staging-root' to it.
The directory is removed at the end of the form regardless of
how BODY exits."
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory
                 (make-temp-file "anvil-extend-phaseB-staging-" t)))
          (anvil-extend-staging-root ,var))
     (unwind-protect
         (progn (ignore ,var) ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defmacro anvil-extend-phaseB-test--with-fixture (storage staging &rest body)
  "Run BODY with isolated STORAGE, STAGING, watch, and debounce state."
  (declare (indent 2))
  (let ((handles (make-symbol "handles"))
        (timers (make-symbol "timers"))
        (names (make-symbol "names"))
        (name (make-symbol "name"))
        (handle (make-symbol "handle"))
        (timer (make-symbol "timer")))
    `(anvil-extend-phaseB-test--with-storage ,storage
       (anvil-extend-phaseB-test--with-staging ,staging
         (let ((anvil-extend--watches nil)
               (anvil-extend--reload-pending nil))
           (unwind-protect
               (progn ,@body)
             (let ((,handles
                    (mapcar #'cdr (copy-sequence anvil-extend--watches)))
                   (,timers
                    (mapcar #'cdr
                            (copy-sequence anvil-extend--reload-pending)))
                   (,names
                    (mapcar #'car (copy-sequence anvil-extend--watches))))
               (dolist (,name ,names)
                 (anvil-extend-unwatch ,name))
               (dolist (,timer ,timers)
                 (when (timerp ,timer)
                   (cancel-timer ,timer)))
               (setq anvil-extend--reload-pending nil)
               (dolist (,handle ,handles)
                 (should-not (file-notify-valid-p ,handle)))
               (dolist (,timer ,timers)
                 (when (timerp ,timer)
                   (should-not (memq ,timer timer-list))
                   (should-not (memq ,timer timer-idle-list))))
               (should-not anvil-extend--watches)
               (should-not anvil-extend--reload-pending))))))))

(defun anvil-extend-phaseB-test--cleanup (sym)
  "Best-effort watch teardown + `unload-feature' for SYM."
  (ignore-errors (anvil-extend-unwatch sym))
  (dolist (s (list sym (intern (concat (symbol-name sym) "-test"))))
    (when (featurep s)
      (ignore-errors (unload-feature s t)))))

(cl-defun anvil-extend-phaseB-test--scaffold-and-load (name body &key params)
  "Helper: scaffold NAME with BODY and PARAMS and `anvil-extend-load' it.
Returns the load result."
  (anvil-extend-scaffold name
                         :params params
                         :body body
                         :docstring "phaseB fixture")
  (anvil-extend-load name))

(defun anvil-extend-phaseB-test--write-broken-syntax (name)
  "Overwrite NAME's elisp file with broken parens so byte-compile fails."
  (let* ((paths (anvil-extend--paths name))
         (path  (plist-get paths :elisp-file)))
    (with-temp-file path
      (insert (format ";;; broken --- intentionally unbalanced -*- lexical-binding: t; -*-\n"))
      (insert "(defun ") (insert (symbol-name name)) (insert " (\n")
      ;; deliberately unterminated paren -> read error -> bytecomp fails
      (insert "  ;; missing close\n")
      (insert (format "(provide '%s)\n" (symbol-name name))))))

(defun anvil-extend-phaseB-test--write-load-fail (name)
  "Overwrite NAME's elisp file so it byte-compiles but errors on load."
  (let* ((paths (anvil-extend--paths name))
         (path  (plist-get paths :elisp-file)))
    (with-temp-file path
      (insert (format ";;; %s --- compiles but errors at load time -*- lexical-binding: t; -*-\n"
                      (symbol-name name)))
      (insert ";;; Code:\n")
      (insert (format "(defun %s () \"reload-fail body\" t)\n" (symbol-name name)))
      (insert "(error \"intentional load-time failure for anvil-extend test\")\n")
      (insert (format "(provide '%s)\n" (symbol-name name))))))

;;;; --- staging dir ----------------------------------------------------

(ert-deftest anvil-extend-phaseB-test-staging-paths-isolated ()
  "Staging path lives under `anvil-extend-staging-root', not storage."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (let* ((staging-path (anvil-extend--staging-path 'iso-name))
           (canonical    (anvil-extend--canonical-elc 'iso-name)))
      (should (string-prefix-p staging staging-path))
      (should (string-prefix-p storage canonical))
      (should-not (string= (file-name-directory staging-path)
                           (file-name-directory canonical))))))

;;;; --- happy path -----------------------------------------------------

(ert-deftest anvil-extend-phaseB-test-reload-roundtrip ()
  "Edit `<NAME>.el' on disk → reload → new definition wins."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (unwind-protect
        (progn
          (anvil-extend-phaseB-test--scaffold-and-load
           'reload-roundtrip
           '((concat "v1-" (format "%s" x)))
           :params '(x))
          (should (fboundp 'reload-roundtrip))
          ;; Sanity: the loaded function speaks v1.
          (should (equal "v1-7"
                         (funcall (symbol-function 'reload-roundtrip) 7)))
          ;; Re-emit the scaffold with a v2 body and reload.
          (anvil-extend-scaffold 'reload-roundtrip
                                 :params '(x)
                                 :body '((concat "v2-" (format "%s" x)))
                                 :docstring "v2")
          (let ((res (anvil-extend-reload 'reload-roundtrip)))
            (should (eq :reloaded (plist-get res :status)))
            (should (eq 'reload-roundtrip (plist-get res :name))))
          (should (equal "v2-7"
                         (funcall (symbol-function 'reload-roundtrip) 7))))
      (anvil-extend-phaseB-test--cleanup 'reload-roundtrip))))

(ert-deftest anvil-extend-phaseB-test-reload-missing-file ()
  "Reloading without a prior scaffold returns :status :missing."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (let ((res (anvil-extend-reload 'never-was)))
      (should (eq :missing (plist-get res :status)))
      (should (stringp (plist-get res :reason))))))

(ert-deftest anvil-extend-phaseB-test-reload-rejects-bad-name ()
  "Reload validates NAME via the Phase A regexp."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (should-error (anvil-extend-reload "string-name") :type 'user-error)
    (should-error (anvil-extend-reload 'BadName)      :type 'user-error)))

;;;; --- rollback paths -------------------------------------------------

(ert-deftest anvil-extend-phaseB-test-rollback-on-byte-compile-fail ()
  "Broken syntax → :rolled-back :stage 'compile, old definition kept.

Rollback survival is asserted *behaviourally* (the function still
returns the expected value) rather than by `eq' on the function
object: byte-compile produces a fresh compiled-lambda object on
every load, so pointer equality after a rebuild is not stable."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (unwind-protect
        (progn
          (anvil-extend-phaseB-test--scaffold-and-load
           'rollback-bc
           '((+ 1 1)))
          (should (fboundp 'rollback-bc))
          (should (= 2 (funcall (symbol-function 'rollback-bc))))
          (anvil-extend-phaseB-test--write-broken-syntax 'rollback-bc)
          (let ((res (anvil-extend-reload 'rollback-bc)))
            (should (eq :rolled-back (plist-get res :status)))
            (should (eq 'compile (plist-get res :stage)))
            (should (stringp (plist-get res :reason))))
          ;; Old definition still callable and returns the same value.
          (should (fboundp 'rollback-bc))
          (should (= 2 (funcall (symbol-function 'rollback-bc)))))
      (anvil-extend-phaseB-test--cleanup 'rollback-bc))))

(ert-deftest anvil-extend-phaseB-test-rollback-on-load-fail ()
  "Compiles but errors on load → :rolled-back :stage 'load, old kept.

As with the byte-compile-fail test, behavioural equivalence (the
function still returns 42) is checked rather than `eq' on the
underlying lambda object."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (unwind-protect
        (progn
          (anvil-extend-phaseB-test--scaffold-and-load 'rollback-load '(42))
          (should (fboundp 'rollback-load))
          (should (= 42 (funcall (symbol-function 'rollback-load))))
          (anvil-extend-phaseB-test--write-load-fail 'rollback-load)
          (let ((res (anvil-extend-reload 'rollback-load)))
            (should (eq :rolled-back (plist-get res :status)))
            (should (eq 'load (plist-get res :stage)))
            (should (stringp (plist-get res :reason))))
          ;; Previous (working) definition is back in memory.
          (should (fboundp 'rollback-load))
          (should (= 42 (funcall (symbol-function 'rollback-load)))))
      (anvil-extend-phaseB-test--cleanup 'rollback-load))))

(ert-deftest anvil-extend-phaseB-test-staging-cleared-on-compile-fail ()
  "Staging dir contains no leftover artifact after a failed reload."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (unwind-protect
        (progn
          (anvil-extend-phaseB-test--scaffold-and-load 'staging-clean '(t))
          (anvil-extend-phaseB-test--write-broken-syntax 'staging-clean)
          (anvil-extend-reload 'staging-clean)
          (let ((staging-elc
                 (expand-file-name "staging-clean.elc" staging)))
            (should-not (file-exists-p staging-elc))))
      (anvil-extend-phaseB-test--cleanup 'staging-clean))))

;;;; --- reload-all -----------------------------------------------------

(ert-deftest anvil-extend-phaseB-test-reload-all-returns-per-name-result ()
  "`anvil-extend-reload-all' walks every visible extension."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (unwind-protect
        (progn
          (anvil-extend-phaseB-test--scaffold-and-load 'all-a '((+ 1 1)))
          (anvil-extend-phaseB-test--scaffold-and-load 'all-b '((* 2 2)))
          (let* ((rows (anvil-extend-reload-all))
                 (names (mapcar (lambda (r) (plist-get r :name)) rows))
                 (statuses (mapcar (lambda (r) (plist-get r :status)) rows)))
            (should (= 2 (length rows)))
            (should (memq 'all-a names))
            (should (memq 'all-b names))
            (dolist (s statuses)
              (should (eq :reloaded s)))))
      (anvil-extend-phaseB-test--cleanup 'all-a)
      (anvil-extend-phaseB-test--cleanup 'all-b))))

(ert-deftest anvil-extend-phaseB-test-reload-all-empty-storage ()
  "`anvil-extend-reload-all' on an empty storage dir returns nil."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (should (null (anvil-extend-reload-all)))))

;;;; --- watch / unwatch ------------------------------------------------

(ert-deftest anvil-extend-phaseB-test-watch-rejects-missing-file ()
  "Cannot watch a NAME whose `<NAME>.el' has not been scaffolded."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (should-error (anvil-extend-watch 'never-scaffolded)
                  :type 'user-error)))

(ert-deftest anvil-extend-phaseB-test-watch-records-handle ()
  "Successful watch installs an entry into `anvil-extend--watches'."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (unwind-protect
        (progn
          (anvil-extend-phaseB-test--scaffold-and-load 'watch-handle '(t))
          (progn
            (let ((handle (anvil-extend-watch 'watch-handle)))
              (should handle)
              (should (assq 'watch-handle anvil-extend--watches))
              (should (eq handle (cdr (assq 'watch-handle
                                            anvil-extend--watches)))))))
      (anvil-extend-phaseB-test--cleanup 'watch-handle))))

(ert-deftest anvil-extend-phaseB-test-watch-replaces-prior-handle ()
  "Re-watching the same NAME tears down the old handle first."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (unwind-protect
        (progn
          (anvil-extend-phaseB-test--scaffold-and-load 'watch-replace '(t))
          (anvil-extend-watch 'watch-replace)
          (anvil-extend-watch 'watch-replace)
          ;; Only one entry per NAME, never duplicated.
          (should (= 1 (cl-count 'watch-replace anvil-extend--watches
                                 :key #'car))))
      (anvil-extend-phaseB-test--cleanup 'watch-replace))))

(ert-deftest anvil-extend-phaseB-test-unwatch-by-symbol ()
  "Unwatching by NAME drops the entry and returns t."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (unwind-protect
        (progn
          (anvil-extend-phaseB-test--scaffold-and-load 'unwatch-sym '(t))
          (anvil-extend-watch 'unwatch-sym)
          (should (eq t (anvil-extend-unwatch 'unwatch-sym)))
          (should-not (assq 'unwatch-sym anvil-extend--watches))
          ;; Idempotent — second call is a no-op nil.
          (should-not (anvil-extend-unwatch 'unwatch-sym)))
      (anvil-extend-phaseB-test--cleanup 'unwatch-sym))))

(ert-deftest anvil-extend-phaseB-test-unwatch-by-handle ()
  "Unwatching by handle (not symbol) also drops the entry."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (unwind-protect
        (progn
          (anvil-extend-phaseB-test--scaffold-and-load 'unwatch-handle '(t))
          (let ((handle (anvil-extend-watch 'unwatch-handle)))
            (should (eq t (anvil-extend-unwatch handle)))
            (should-not (assq 'unwatch-handle anvil-extend--watches))))
      (anvil-extend-phaseB-test--cleanup 'unwatch-handle))))

(ert-deftest anvil-extend-phaseB-test-watch-all-walks-storage ()
  "`anvil-extend-watch-all' installs one watch per visible extension."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (unwind-protect
        (progn
          (anvil-extend-phaseB-test--scaffold-and-load 'wall-a '(t))
          (anvil-extend-phaseB-test--scaffold-and-load 'wall-b '(t))
          (let* ((pairs (anvil-extend-watch-all))
                 (names (mapcar #'car pairs)))
            (should (= 2 (length pairs)))
            (should (memq 'wall-a names))
            (should (memq 'wall-b names))
            (should (assq 'wall-a anvil-extend--watches))
            (should (assq 'wall-b anvil-extend--watches))))
      (anvil-extend-phaseB-test--cleanup 'wall-a)
      (anvil-extend-phaseB-test--cleanup 'wall-b))))

;;;; --- debounced reload ----------------------------------------------

(ert-deftest anvil-extend-phaseB-test-schedule-reload-coalesces ()
  "Multiple `--schedule-reload' calls within debounce window keep one timer."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (unwind-protect
        ;; A long debounce window so the timer cannot expire mid-test.
        (let ((anvil-extend-watch-debounce 60.0))
          (anvil-extend-phaseB-test--scaffold-and-load 'debounce-coal '(t))
          (let ((t1 (anvil-extend--schedule-reload 'debounce-coal))
                (t2 (anvil-extend--schedule-reload 'debounce-coal))
                (t3 (anvil-extend--schedule-reload 'debounce-coal)))
            (should (timerp t1))
            (should (timerp t2))
            (should (timerp t3))
            ;; Only the latest timer remains in the pending alist.
            (should (= 1 (cl-count 'debounce-coal
                                   anvil-extend--reload-pending
                                   :key #'car)))
            (should (eq t3 (cdr (assq 'debounce-coal
                                      anvil-extend--reload-pending))))
            ;; Cleanup leftover timer so it does not fire after teardown.
            (cancel-timer t3)
            (setq anvil-extend--reload-pending
                  (assq-delete-all 'debounce-coal
                                   anvil-extend--reload-pending))))
      (anvil-extend-phaseB-test--cleanup 'debounce-coal))))

;;;; --- MCP wrappers ---------------------------------------------------

(ert-deftest anvil-extend-phaseB-test-tool-reload-accepts-string-name ()
  "MCP wrapper accepts a string NAME and prints the reload plist."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (unwind-protect
        (let ((have-server (featurep 'anvil-server)))
          (anvil-extend-phaseB-test--scaffold-and-load 'mcp-reload '(t))
          (let ((out
                 (if have-server
                     (anvil-extend--tool-reload "mcp-reload")
                   (cl-letf (((symbol-function 'anvil-server-with-error-handling)
                              (lambda (&rest body) `(progn ,@body))))
                     (eval `(anvil-extend--tool-reload "mcp-reload"))))))
            (should (stringp out))
            (should (string-match-p ":status :reloaded" out))))
      (anvil-extend-phaseB-test--cleanup 'mcp-reload))))

(ert-deftest anvil-extend-phaseB-test-tool-watch-returns-status-plist ()
  "MCP watch wrapper returns a printed plist (no opaque handle leaks)."
  (anvil-extend-phaseB-test--with-fixture storage staging
    (unwind-protect
        (let ((have-server (featurep 'anvil-server)))
          (anvil-extend-phaseB-test--scaffold-and-load 'mcp-watch '(t))
          (let ((out
                 (if have-server
                     (anvil-extend--tool-watch "mcp-watch")
                   (cl-letf (((symbol-function 'anvil-server-with-error-handling)
                              (lambda (&rest body) `(progn ,@body))))
                     (eval `(anvil-extend--tool-watch "mcp-watch"))))))
            (should (stringp out))
            (should (string-match-p ":status :watching" out))
            (should (string-match-p "mcp-watch" out))
            (should (assq 'mcp-watch anvil-extend--watches))))
      (anvil-extend-phaseB-test--cleanup 'mcp-watch))))

(provide 'anvil-extend-phaseB-test)
;;; anvil-extend-phaseB-test.el ends here
