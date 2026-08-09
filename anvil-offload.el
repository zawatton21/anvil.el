;;; anvil-offload.el --- Offload heavy elisp to a batch subprocess -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 1 of Doc 03 (Offload Framework).  Provides a long-lived
;; `emacs --batch' REPL subprocess and a future-based API to hand
;; heavy elisp off the main daemon.
;;
;; Phase 1 scope: standalone REPL (no worker-pool integration).
;; Phase 2 will route through the batch lane of `anvil-worker'.
;;
;; Public API:
;;   (anvil-offload FORM &rest KEYS)
;;       Returns an `anvil-future'.  FORM evaluates in the REPL.
;;       KEYS :require, :load-path, :isolated, :on-start, :on-settle,
;;       :process-environment, :exec-path, :default-directory,
;;       :shell-file-name, :shell-command-switch, :exact-load-path.
;;   (anvil-future-done-p FUTURE)
;;   (anvil-future-await FUTURE &optional TIMEOUT)
;;   (anvil-future-value FUTURE)
;;   (anvil-future-error FUTURE)
;;   (anvil-future-cancel FUTURE)
;;   (anvil-future-kill FUTURE)          ; Phase 3a
;;   (anvil-future-checkpoint FUTURE)    ; Phase 3b
;;   (anvil-preempt-checkpoint V &optional C)  ; handler-side, Phase 3b
;;
;; Protocol:
;;   request to stdin            : raw sexp `(:id N :payload BASE64(FORM))'
;;   reply / checkpoint to stdout: PREFIX + BASE64(UTF-8(prin1(MSG))) + "\n"
;;     where decoded MSG is:
;;       (:id N :ok VALUE) | (:id N :error MSG)
;;       (:id N :checkpoint (:value V :cursor C))  (Phase 3b)
;;
;; Checkpoints are intermediate, non-settling messages sent by handlers
;; via `anvil-preempt-checkpoint' so the main daemon can return the last
;; known partial state if the call is killed for running over budget.
;;
;; Replies are framed so stray stdout chatter from handlers / `require'
;; does not poison the transport.  The REPL still uses
;; `send-string-to-terminal' because it calls fflush(stdout) in batch
;; mode — a plain `princ' may stay in the C stdio buffer on Windows
;; pipes and never reach the client.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Customization

(defgroup anvil-offload nil
  "Offload heavy elisp to a batch subprocess."
  :group 'anvil
  :prefix "anvil-offload-")

(defcustom anvil-offload-emacs-bin
  (let ((invoked-emacs
         (and (stringp invocation-name)
              (stringp invocation-directory)
              (expand-file-name invocation-name invocation-directory))))
    (or (and invoked-emacs
             (file-executable-p invoked-emacs)
             invoked-emacs)
        (executable-find "emacs")
        "emacs"))
  "Emacs binary used to spawn the offload REPL."
  :type 'file
  :group 'anvil-offload)

(defcustom anvil-offload-default-await-timeout 300
  "Default timeout (seconds) for `anvil-future-await' when omitted."
  :type 'integer
  :group 'anvil-offload)

(defcustom anvil-offload-poll-interval 0.05
  "Seconds to block in `accept-process-output' per await iteration."
  :type 'number
  :group 'anvil-offload)

(defcustom anvil-offload-pool-size 1
  "Number of REPL subprocesses in the offload pool.
Round-robin dispatch spreads offload requests across live slots;
futures bound to distinct slots execute in parallel.  Default 1
is the Phase 1 behaviour.  A size change retires the old pool before
the next dispatch; dispatch fails closed until every old child exits."
  :type 'integer
  :group 'anvil-offload)

(defcustom anvil-offload-max-frame-bytes (* 2 1024 1024)
  "Maximum bytes accepted for one offload protocol frame.
An over-limit frame fails every future owned by that subprocess and
terminates it before base64 decoding or Lisp reading can monopolize
the host Emacs."
  :type 'integer
  :group 'anvil-offload)

(defcustom anvil-offload-init-files nil
  "Files loaded in every offload subprocess before the REPL loop.
Dedicated backends use this to install a controlled environment.  Isolated
children start with `-Q' and do not clone live functions, features, buffers,
or buffer-local state from the root Emacs; list explicit initializer files
here when child expressions require that configuration.  Request-local
environment, directory, executable, shell, and load-path bindings are
transported separately around each submitted form."
  :type '(repeat file)
  :group 'anvil-offload)

(defcustom anvil-offload-spawn-environment-function nil
  "Optional function returning `process-environment' for child spawn.
The function is called immediately before `make-process'.  A nil
return means to copy the caller's current environment.  Request-local
environment bindings are transported separately and never affect
subprocess creation."
  :type '(choice (const :tag "Inherit caller environment" nil) function)
  :group 'anvil-offload)

;;; State

(defvar anvil-offload--pool nil
  "Vector of live REPL processes, or nil before the first dispatch.
Length matches `anvil-offload-pool-size' at the moment the pool
was initialised.  Each slot is either a live `make-process' or nil
\(unspawned / died and not yet respawned).")

(defvar anvil-offload--pool-retiring-p nil
  "Non-nil while old pooled children must not receive new requests.")

(defvar anvil-offload--pool-cleanup-active-p nil
  "Observable mirror of a synchronous pool or slot retirement transaction.
The stable cell read by `anvil-offload--cleanup-active-p' is authoritative.")

(defvar anvil-offload--submission-active-p nil
  "Observable non-nil mirror while a public submission is transactional.")

(defvar anvil-offload--transaction-state (vector nil nil)
  "Stable cleanup and submission transaction state.
Index 0 is the authoritative cleanup-active bit and index 1 is the
authoritative submission-active bit.  Mutating this vector invokes no
variable watcher, so reentrant callbacks observe the guard before the
corresponding observable dynamic binding is established.")

(defun anvil-offload--cleanup-active-p ()
  "Return non-nil while an authoritative cleanup transaction is active."
  (aref anvil-offload--transaction-state 0))

(defun anvil-offload--submission-active-p ()
  "Return non-nil while an authoritative submission transaction is active."
  (aref anvil-offload--transaction-state 1))

(defmacro anvil-offload--with-cleanup-guard (&rest body)
  "Run BODY under the stable cleanup guard and its observable mirror."
  (declare (indent 0) (debug t))
  (let ((state-symbol (make-symbol "anvil-offload-cleanup-state"))
        (inhibit-symbol (make-symbol "anvil-offload-outer-inhibit-quit")))
    `(let ((,state-symbol anvil-offload--transaction-state)
           (,inhibit-symbol inhibit-quit)
           (inhibit-quit t))
       (when (aref ,state-symbol 0)
         (error "anvil-offload: pool cleanup already active"))
       (aset ,state-symbol 0 t)
       (unwind-protect
           (let ((inhibit-quit ,inhibit-symbol)
                 (anvil-offload--pool-cleanup-active-p t))
             ,@body)
         (aset ,state-symbol 0 nil)))))

(defmacro anvil-offload--with-submission-guard (&rest body)
  "Run BODY under the stable submission guard and its observable mirror."
  (declare (indent 0) (debug t))
  (let ((state-symbol (make-symbol "anvil-offload-submission-state"))
        (inhibit-symbol (make-symbol "anvil-offload-outer-inhibit-quit")))
    `(let ((,state-symbol anvil-offload--transaction-state)
           (,inhibit-symbol inhibit-quit)
           (inhibit-quit t))
       (when (aref ,state-symbol 1)
         (error "anvil-offload: submission already active"))
       (aset ,state-symbol 1 t)
       (unwind-protect
           (let ((inhibit-quit ,inhibit-symbol)
                 (anvil-offload--submission-active-p t))
             ,@body)
         (aset ,state-symbol 1 nil)))))

(defvar anvil-offload--stop-retiring-p nil
  "Non-nil while a whole-subsystem stop has not fully converged.")

(defvar anvil-offload--retired-pools nil
  "Captured pool vectors retained until every owned slot is cleared.")

(defvar anvil-offload--ownership-table-registry nil
  "Auxiliary ownership tables retained across callback-driven table swaps.")

(defvar anvil-offload--fallback-processes (make-hash-table :test 'eq)
  "Stable fallback custody established before any watched table assignment.")

(defvar anvil-offload--round-robin 0
  "Rolling index for pool dispatch.")

(defvar anvil-offload--next-id 0
  "Monotonic request ID counter.")

(defvar anvil-offload--pending nil
  "Hash table mapping request-id → `anvil-future'.
Created lazily in `anvil-offload--ensure-pending'.")

(defvar anvil-offload--isolated-processes nil
  "Auxiliary ownership table for one-shot and staged pooled subprocesses.")

(defvar anvil-offload--repl-init-file nil
  "Path to the generated REPL init file, or nil if not yet written.")

(defconst anvil-offload--protocol-version 3
  "Wire-format version spoken by the offload REPL pool.")

(defconst anvil-offload--frame-prefix "ANVIL-OFFLOAD "
  "Line prefix tagging framed stdout messages from the offload REPL.")

(defconst anvil-offload--canonical-obarray obarray
  "Symbol table used for all host-side offload protocol reads.")

(defconst anvil-offload--ignored-junk-prefixes
  '("Lisp expression: ")
  "Known benign stdout prefixes emitted by the batch REPL.")

(defun anvil-offload--capture-primitive (symbol)
  "Return SYMBOL's underlying function, stripping already-installed advice."
  (let ((function (symbol-function symbol)))
    (if (fboundp 'advice--cd*r)
        (advice--cd*r function)
      function)))

(defconst anvil-offload--puthash-primitive (anvil-offload--capture-primitive 'puthash)
  "Unadvised hash insertion primitive for ownership linearization.")

(defconst anvil-offload--gethash-primitive (anvil-offload--capture-primitive 'gethash)
  "Unadvised hash lookup primitive for ownership linearization.")

(defconst anvil-offload--remhash-primitive (anvil-offload--capture-primitive 'remhash)
  "Unadvised hash removal primitive for nonlocal-exit cleanup.")

(defconst anvil-offload--substring-primitive
  (anvil-offload--capture-primitive 'substring)
  "Captured substring primitive for observable linear-space slicing.")

(defconst anvil-offload--run-at-time-function (anvil-offload--capture-primitive 'run-at-time)
  "Captured timer scheduler for preserving an in-flight nonlocal exit.")

(defconst anvil-offload--process-list-primitive (anvil-offload--capture-primitive 'process-list)
  "Unadvised process identity snapshot primitive for spawn custody.")

(defconst anvil-offload--process-put-primitive (anvil-offload--capture-primitive 'process-put)
  "Unadvised process property writer for custody linearization.")

(defconst anvil-offload--process-get-primitive (anvil-offload--capture-primitive 'process-get)
  "Unadvised process property reader for cleanup snapshots.")

(defconst anvil-offload--process-buffer-primitive
  (anvil-offload--capture-primitive 'process-buffer)
  "Unadvised process buffer reader for sentinel cleanup.")

(defconst anvil-offload--process-filter-primitive
  (anvil-offload--capture-primitive 'process-filter)
  "Unadvised process filter reader for constructor validation.")

(defconst anvil-offload--process-name-primitive (anvil-offload--capture-primitive 'process-name)
  "Unadvised process name reader for constructor validation.")

(defconst anvil-offload--make-process-primitive (anvil-offload--capture-primitive 'make-process)
  "Unadvised subprocess constructor captured before runtime configuration.")

(defconst anvil-offload--timer-create-primitive (anvil-offload--capture-primitive 'timer-create)
  "Unadvised timer allocator for fail-closed deferred cleanup.")

(defconst anvil-offload--timer-set-function-primitive
  (anvil-offload--capture-primitive 'timer-set-function)
  "Unadvised timer callback publisher for fail-closed deferred cleanup.")

(defconst anvil-offload--timer-set-time-primitive
  (anvil-offload--capture-primitive 'timer-set-time)
  "Unadvised timer deadline publisher for fail-closed deferred cleanup.")

(defconst anvil-offload--timer-activate-primitive
  (anvil-offload--capture-primitive 'timer-activate)
  "Unadvised timer activation primitive for fail-closed deferred cleanup.")

(defconst anvil-offload--cancel-timer-primitive
  (anvil-offload--capture-primitive 'cancel-timer)
  "Unadvised timer cancellation primitive for exact timer normalization.")

(defun anvil-offload--scheduled-timers ()
  "Return a snapshot of every normal and idle timer identity."
  (append (copy-sequence timer-list)
          (copy-sequence timer-idle-list)))

(defun anvil-offload--timer-scheduled-p (timer)
  "Return non-nil when TIMER is scheduled normally or while Emacs is idle."
  (and (timerp timer)
       (or (memq timer timer-list)
           (memq timer timer-idle-list))))

(defun anvil-offload--exact-zero-delay-timer-scheduled-p (function args)
  "Return non-nil when an exact FUNCTION/ARGS timer is still scheduled."
  (cl-some
   (lambda (timer)
     (and (timerp timer)
          (eq (timer--function timer) function)
          (equal (timer--args timer) args)
          (anvil-offload--timer-scheduled-p timer)))
   (anvil-offload--scheduled-timers)))

(defun anvil-offload--normalize-zero-delay-timer (timer function args)
  "Make exact TIMER run FUNCTION with ARGS promptly and only once."
  (when (anvil-offload--timer-scheduled-p timer)
    (funcall anvil-offload--cancel-timer-primitive timer))
  (funcall anvil-offload--timer-set-function-primitive
           timer function args)
  (funcall anvil-offload--timer-set-time-primitive
           timer (current-time) nil)
  (funcall anvil-offload--timer-activate-primitive timer)
  timer)

(defun anvil-offload--select-zero-delay-timer
    (returned before function args)
  "Select one new exact FUNCTION/ARGS timer or create a direct fallback.
Preexisting and non-callback timers are never modified."
  (let (candidates)
    (dolist (candidate
             (append (anvil-offload--scheduled-timers)
                     (list returned)))
      (when (and (timerp candidate)
                 (not (memq candidate before)))
        (cl-pushnew candidate candidates :test #'eq)))
    (let ((selected
           (or
            (and (memq returned candidates)
                 (eq (timer--function returned) function)
                 (equal (timer--args returned) args)
                 returned)
            (cl-find-if
             (lambda (candidate)
               (and (eq (timer--function candidate) function)
                    (equal (timer--args candidate) args)))
             candidates))))
      (if selected
          (progn
            ;; Every newly created exact callback timer belongs to this
            ;; scheduling transaction.  Normalize one authoritative timer and
            ;; cancel its exact duplicates; unrelated helper timers remain
            ;; untouched.
            (dolist (candidate candidates)
              (when (and (not (eq candidate selected))
                         (eq (timer--function candidate) function)
                         (equal (timer--args candidate) args)
                         (anvil-offload--timer-scheduled-p candidate))
                (funcall anvil-offload--cancel-timer-primitive candidate)))
            (anvil-offload--normalize-zero-delay-timer
             selected function args))
        (apply #'anvil-offload--make-direct-zero-delay-timer
               function args)))))

(defun anvil-offload--make-direct-zero-delay-timer (function &rest args)
  "Create one zero-delay timer for FUNCTION and ARGS without a scheduler."
  (anvil-offload--normalize-zero-delay-timer
   (funcall anvil-offload--timer-create-primitive)
   function args))

(defun anvil-offload--schedule-zero-delay (function &rest args)
  "Schedule FUNCTION with ARGS and retain exact deferred work on every exit.
Only a new timer carrying exact FUNCTION and ARGS is adopted and normalized.
Preexisting or foreign timers are preserved and cause a direct fallback.  A
tagged scheduler exit still propagates, but unwind publication leaves exact
deferred work active."
  (let ((before (anvil-offload--scheduled-timers))
        returned published timer)
    (unwind-protect
        (condition-case nil
            (progn
              (setq returned
                    (apply anvil-offload--run-at-time-function
                           0 nil function args)
                    timer
                    (anvil-offload--select-zero-delay-timer
                     returned before function args)
                    published t)
              timer)
          ((error quit)
           (setq timer
                 (anvil-offload--select-zero-delay-timer
                  returned before function args)
                 published t)
           timer))
      (unless published
        (anvil-offload--select-zero-delay-timer
         returned before function args)))))

(defun anvil-offload--frame-encode-payload (string)
  "Return STRING encoded as a single-line transport payload."
  (base64-encode-string
   (encode-coding-string string 'utf-8-unix)
   t))

(defun anvil-offload--frame-decode-payload (payload)
  "Decode PAYLOAD from the offload transport into a UTF-8 string."
  (decode-coding-string
   (base64-decode-string payload)
   'utf-8-unix))

(defun anvil-offload--print-readably (object)
  "Return a complete, topology-preserving printed form of OBJECT."
  (let ((print-length nil)
        (print-level nil)
        (print-circle t)
        (print-gensym t)
        (print-unreadable-function nil)
        (print-continuous-numbering nil)
        (print-number-table nil)
        (print-symbols-bare nil)
        (print-quoted t)
        (print-integers-as-characters nil)
        (print-escape-control-characters nil)
        (print-escape-multibyte nil)
        (print-escape-newlines nil)
        (print-escape-nonascii nil)
        (print-charset-text-property t)
        (float-output-format nil)
        (read-circle t)
        (read-symbol-shorthands nil)
        (obarray anvil-offload--canonical-obarray))
    (let* ((text (prin1-to-string object))
           (parsed (read-from-string text)))
      (unless (string-match-p
               "\\`[[:space:]]*\\'" (substring text (cdr parsed)))
        (error "trailing data after serialized request"))
      text)))

(defun anvil-offload--line-preview (string)
  "Return a short, single-line preview of STRING for diagnostics."
  (let ((flat (replace-regexp-in-string "[\r\n]+" "\\n" string)))
    (anvil-offload--bounded-ends flat 120)))

(defconst anvil-offload--junk-diagnostic-max-chars 192
  "Maximum combined non-frame diagnostic characters retained per child.")

(defconst anvil-offload--exit-reason-max-chars 255
  "Maximum child-exit reason characters delivered to a future.")

(defun anvil-offload--bounded-ends (string limit)
  "Bound STRING to nonnegative LIMIT characters while retaining both ends."
  (let ((limit (max 0 limit))
        (separator "..."))
    (cond
     ((<= (length string) limit) string)
     ((<= limit (length separator))
      (substring separator 0 limit))
     (t
      (let* ((payload (- limit (length separator)))
             (prefix-length (/ (+ payload 1) 2))
             (suffix-length (- payload prefix-length)))
        (concat
         (substring string 0 prefix-length)
         separator
         (substring string (- (length string) suffix-length))))))))

(defun anvil-offload--remember-junk-reply (proc line)
  "Remember bounded first and last non-frame LINE diagnostics for PROC."
  (let ((preview (anvil-offload--line-preview line)))
    (unless
        (funcall anvil-offload--process-get-primitive
                 proc 'anvil-offload-first-junk-reply)
      (funcall anvil-offload--process-put-primitive
               proc 'anvil-offload-first-junk-reply preview))
    (funcall anvil-offload--process-put-primitive
             proc 'anvil-offload-last-junk-reply preview)
    preview))

(defun anvil-offload--junk-diagnostic (proc)
  "Return PROC's bounded non-frame diagnostic prefix and suffix."
  (let ((first
         (funcall anvil-offload--process-get-primitive
                  proc 'anvil-offload-first-junk-reply))
        (last
         (funcall anvil-offload--process-get-primitive
                  proc 'anvil-offload-last-junk-reply)))
    (cond
     ((and (stringp first) (not (string-empty-p first))
           (stringp last) (not (string-empty-p last))
           (not (equal first last)))
      (anvil-offload--bounded-ends
       (format "%s ... %s" first last)
       anvil-offload--junk-diagnostic-max-chars))
     ((and (stringp first) (not (string-empty-p first))) first)
     ((and (stringp last) (not (string-empty-p last))) last))))

(defun anvil-offload--exit-reason (proc base-reason)
  "Combine BASE-REASON with PROC's latest bounded non-frame diagnostics."
  (condition-case nil
      (if-let ((diagnostic (anvil-offload--junk-diagnostic proc)))
          (let* ((prefix (format "%s (" base-reason))
                 (available
                  (max 0
                       (- anvil-offload--exit-reason-max-chars
                          (length prefix) 1))))
            (if (= available 0)
                (anvil-offload--bounded-ends
                 base-reason anvil-offload--exit-reason-max-chars)
              (format "%s%s)"
                      prefix
                      (anvil-offload--bounded-ends diagnostic available))))
        (anvil-offload--bounded-ends
         base-reason anvil-offload--exit-reason-max-chars))
    ((error quit)
     (anvil-offload--bounded-ends
      base-reason anvil-offload--exit-reason-max-chars))))

(defun anvil-offload--strip-ignored-junk-prefixes (string)
  "Drop known benign stdout prefixes from STRING in linear space."
  (let ((offset 0)
        (limit (length string))
        changed)
    (while
        (progn
          (setq changed nil)
          (dolist (prefix anvil-offload--ignored-junk-prefixes)
            (let* ((prefix-length (length prefix))
                   (end (+ offset prefix-length)))
              (when (and (> prefix-length 0)
                         (<= end limit)
                         (eq t
                             (compare-strings
                              prefix nil nil string offset end)))
                (setq offset end
                      changed t))))
          changed))
    (if (zerop offset)
        string
      (funcall anvil-offload--substring-primitive string offset))))

(defun anvil-offload--ensure-pending ()
  "Return the pending-futures hash, creating it if needed."
  (or anvil-offload--pending
      (setq anvil-offload--pending (make-hash-table :test 'eql))))

(defun anvil-offload--ensure-isolated-processes ()
  "Return and globally register the current auxiliary ownership table."
  (let ((table
         (or (and (hash-table-p anvil-offload--isolated-processes)
                  anvil-offload--isolated-processes)
             (setq anvil-offload--isolated-processes
                   (make-hash-table :test 'eq)))))
    (unless (memq table anvil-offload--ownership-table-registry)
      (push table anvil-offload--ownership-table-registry))
    table))

(defun anvil-offload--mark-retiring (proc)
  "Mark owned process PROC so no dispatch path can reuse it."
  (when (processp proc)
    (funcall anvil-offload--process-put-primitive proc 'anvil-offload-retiring t))
  proc)

(defun anvil-offload--registered-ownership-tables ()
  "Return every globally retained auxiliary ownership table."
  (let ((tables (copy-sequence anvil-offload--ownership-table-registry)))
    (when (hash-table-p anvil-offload--fallback-processes)
      (push anvil-offload--fallback-processes tables))
    (when (hash-table-p anvil-offload--isolated-processes)
      (push anvil-offload--isolated-processes tables))
    (cl-delete-duplicates
     (cl-remove-if-not #'hash-table-p tables)
     :test #'eq)))

(defun anvil-offload--track-process-ownership (proc value &optional table)
  "Record exact PROC in a globally registered ownership TABLE.
VALUE describes the custody reason.  The table is also recorded on PROC
so sentinel cleanup can release it even after callback-driven global
table replacement."
  (unless (processp proc)
    (error "anvil-offload: cannot own non-process %S" proc))
  (let ((owner (or table (anvil-offload--ensure-isolated-processes))))
    (unless (hash-table-p owner)
      (error "anvil-offload: invalid ownership table %S" owner))
    ;; Custody publication must not pass through runtime advice: this is the
    ;; first operation after make-process returns, and an advised hash insert
    ;; that exits nonlocally must not leave the new child unowned.
    (funcall anvil-offload--puthash-primitive proc value owner)
    (funcall anvil-offload--process-put-primitive
     proc 'anvil-offload-ownership-tables
     (cl-adjoin
      owner (funcall anvil-offload--process-get-primitive proc 'anvil-offload-ownership-tables) :test #'eq))
    ;; Avoid even a no-op assignment to this watched global after process
    ;; creation.  If a genuinely new owner still needs registration, exact
    ;; custody is already present in OWNER and on PROC before watcher code can
    ;; run or exit nonlocally.
    (unless (memq owner anvil-offload--ownership-table-registry)
      (push owner anvil-offload--ownership-table-registry))
    owner))

(defun anvil-offload--retain-submission-process (proc table &optional reason)
  "Retain exact PROC in already registered TABLE without advice callbacks.
When REASON is non-nil, publish it as the current custody reason."
  (unless (and (processp proc) (hash-table-p table))
    (error "anvil-offload: invalid submission custody %S %S" proc table))
  (funcall anvil-offload--puthash-primitive
           proc
           (or reason
               (funcall anvil-offload--gethash-primitive proc table)
               :submission-active)
           table)
  (funcall anvil-offload--process-put-primitive
   proc 'anvil-offload-ownership-tables
   (cl-adjoin
    table (funcall anvil-offload--process-get-primitive proc 'anvil-offload-ownership-tables) :test #'eq))
  proc)

(defun anvil-offload--stage-retirement-pools (pools)
  "Retain every exact child in POOLS before any observable transition."
  (unless (hash-table-p anvil-offload--fallback-processes)
    (error "anvil-offload: stable fallback ownership is unavailable"))
  (dolist (pool pools)
    (when (vectorp pool)
      (dotimes (i (length pool))
        (let ((proc (aref pool i)))
          (when (processp proc)
            (anvil-offload--mark-retiring proc)
            (anvil-offload--retain-submission-process
             proc anvil-offload--fallback-processes :pool-retiring))))))
  anvil-offload--fallback-processes)

(defun anvil-offload--fallback-pool-retirement-p ()
  "Return non-nil when stable fallback custody holds a retired pool child."
  (and
   (hash-table-p anvil-offload--fallback-processes)
   (let (found)
     (maphash
      (lambda (_proc reason)
        (when (eq reason :pool-retiring)
          (setq found t)))
      anvil-offload--fallback-processes)
     found)))

(defun anvil-offload--owned-auxiliary-processes ()
  "Return every process in any registered auxiliary ownership table."
  (let (processes)
    (dolist (table (anvil-offload--registered-ownership-tables))
      (maphash (lambda (proc _value) (push proc processes)) table))
    (cl-delete-duplicates processes :test #'eq)))

(defun anvil-offload--stage-stop-auxiliary-processes ()
  "Snapshot and stably retain every auxiliary child for whole-stop cleanup."
  (unless (hash-table-p anvil-offload--fallback-processes)
    (error "anvil-offload: stable fallback ownership is unavailable"))
  (let ((processes (anvil-offload--owned-auxiliary-processes)))
    (dolist (proc processes)
      (when (processp proc)
        (anvil-offload--mark-retiring proc)
        (funcall anvil-offload--process-put-primitive
                 proc 'anvil-offload-stop-retiring t)
        (anvil-offload--retain-submission-process
         proc anvil-offload--fallback-processes
         (or (funcall anvil-offload--gethash-primitive
                      proc anvil-offload--fallback-processes)
             :stop-retiring))))
    processes))

(defun anvil-offload--fallback-stop-retirement-p ()
  "Return non-nil while stable fallback holds a whole-stop child."
  (and
   (hash-table-p anvil-offload--fallback-processes)
   (let (found)
     (maphash
      (lambda (proc _reason)
        (when
            (and
             (processp proc)
             (funcall anvil-offload--process-get-primitive
                      proc 'anvil-offload-stop-retiring))
          (setq found t)))
      anvil-offload--fallback-processes)
     found)))

(defun anvil-offload--stop-incomplete-p ()
  "Return non-nil while a whole-subsystem stop remains fail closed."
  (or anvil-offload--stop-retiring-p
      (anvil-offload--fallback-stop-retirement-p)))

(defun anvil-offload--clear-process-from-pools (proc)
  "Clear exact PROC from the current and every retired pool vector."
  (dolist (pool (cons anvil-offload--pool anvil-offload--retired-pools))
    (when (vectorp pool)
      (dotimes (i (length pool))
        (when (eq proc (aref pool i))
          (aset pool i nil)))))
  (setq anvil-offload--retired-pools
        (cl-remove-if
         (lambda (pool)
           (not (cl-some #'identity (append pool nil))))
         anvil-offload--retired-pools)))

(defun anvil-offload--release-process-ownership (proc)
  "Remove dead PROC from every globally or locally recorded owner."
  (when (processp proc)
    (anvil-offload--clear-process-from-pools proc)
    (let ((tables
           (append
            (anvil-offload--registered-ownership-tables)
            (funcall anvil-offload--process-get-primitive proc 'anvil-offload-ownership-tables))))
      (dolist (table (cl-delete-duplicates tables :test #'eq))
        (when (hash-table-p table)
          (funcall anvil-offload--remhash-primitive proc table))))
    (funcall anvil-offload--process-put-primitive proc 'anvil-offload-ownership-tables nil)
    (funcall anvil-offload--process-put-primitive proc 'anvil-offload-staged-slot nil)
    (funcall anvil-offload--process-put-primitive
             proc 'anvil-offload-stop-retiring nil)))

(defun anvil-offload--terminate-retained-process (proc)
  "Terminate already marked and stably retained PROC without table publication."
  (let (termination-errors)
    (let ((inhibit-quit t))
      (when (process-live-p proc)
        (condition-case err
            (kill-process proc)
          ((error quit)
           (push (format "kill-process: %s" (error-message-string err))
                 termination-errors))))
      (condition-case err
          (delete-process proc)
        ((error quit)
         (push (format "delete-process: %s"
                       (error-message-string err))
               termination-errors))))
    (if (or (process-live-p proc)
            (memq proc (funcall anvil-offload--process-list-primitive)))
        (progn
          (message
           "anvil-offload: child %s remains tracked after failed hard delete%s"
           (funcall anvil-offload--process-name-primitive proc)
           (if termination-errors
               (format ": %s"
                       (string-join (nreverse termination-errors) "; "))
             ""))
          nil)
      (anvil-offload--release-process-ownership proc)
      t)))

(defun anvil-offload--delete-retained-process (proc)
  "Best-effort deferred deletion of exact fallback-owned PROC."
  (condition-case cleanup-error
      (anvil-offload--terminate-retained-process proc)
    ((error quit)
     (message
      "anvil-offload: deferred exact cleanup failed: %s"
      (error-message-string cleanup-error)))))

(defun anvil-offload--defer-hard-delete-preserving-exit (proc)
  "Retain PROC and schedule callback-free deletion after a nonlocal exit."
  (when (processp proc)
    (anvil-offload--mark-retiring proc)
    (if (hash-table-p anvil-offload--fallback-processes)
        (anvil-offload--retain-submission-process
         proc anvil-offload--fallback-processes)
      (message "anvil-offload: stable fallback ownership is unavailable"))
    (condition-case cleanup-error
        (anvil-offload--make-direct-zero-delay-timer
         #'anvil-offload--delete-retained-process proc)
      ((error quit)
       (message
        "anvil-offload: exact cleanup remains globally owned: %s"
        (error-message-string cleanup-error))))))

(defun anvil-offload--hard-delete-process (proc)
  "Hard-delete the exact owned subprocess PROC.
PROC is marked retiring before table creation can invoke variable
watchers, then placed in a globally registered auxiliary table before
the first destructive callback.  Ownership is cleared only after PROC
is absent from the process registry; failure leaves durable custody for
a later retry."
  (when (processp proc)
    ;; Table creation can run variable watchers.  Mark and establish custody
    ;; in the stable fallback first, so a watcher can replace every pool/table
    ;; global and exit nonlocally without orphaning this exact child.
    (anvil-offload--mark-retiring proc)
    (unless (hash-table-p anvil-offload--fallback-processes)
      (error "anvil-offload: stable fallback ownership is unavailable"))
    (anvil-offload--retain-submission-process
     proc anvil-offload--fallback-processes)
    (let* ((ownership (anvil-offload--ensure-isolated-processes))
           (existing (funcall anvil-offload--gethash-primitive proc ownership)))
      (anvil-offload--track-process-ownership
       proc (or existing :retiring) ownership)
      (anvil-offload--terminate-retained-process proc))))

;;; REPL init file

(defconst anvil-offload--repl-body
  (format ";; anvil-offload REPL — auto-generated, do not edit -*- lexical-binding: t; -*-
\(setq coding-system-for-read 'utf-8-unix
      coding-system-for-write 'utf-8-unix)
\(defconst anvil-offload--frame-prefix %S)
\(defconst anvil-offload--canonical-obarray obarray)
\(defun anvil-offload--serialize-frame (msg)
  \"Return a readable printed representation of MSG.
Unreadable result values become a terminal error carrying the same id.\"
  (let ((print-length nil)
        (print-level nil)
        (print-circle t)
        (print-gensym t)
        (print-unreadable-function nil)
        (print-continuous-numbering nil)
        (print-number-table nil)
        (print-symbols-bare nil)
        (print-quoted t)
        (print-integers-as-characters nil)
        (print-escape-control-characters nil)
        (print-escape-multibyte nil)
        (print-escape-newlines nil)
        (print-escape-nonascii nil)
        (print-charset-text-property t)
        (float-output-format nil)
        (read-circle t)
        (read-symbol-shorthands nil)
        (obarray anvil-offload--canonical-obarray))
    (condition-case err
        (let* ((text (prin1-to-string msg))
               (read-circle t)
               (parsed (read-from-string text)))
          (unless (string-match-p
                   \"\\\\`[[:space:]]*\\\\'\" (substring text (cdr parsed)))
            (error \"trailing data after serialized reply\"))
          text)
      (error
       ;; A checkpoint is nonterminal.  Replacing it with a terminal error
       ;; while evaluation continues would make the live worker look idle to
       ;; the host.  Propagate the serialization failure through evaluation
       ;; instead; its surrounding handler then aborts the request and emits
       ;; exactly one terminal error.  Terminal replies themselves still need
       ;; a guaranteed-readable fallback.
       (if (plist-member msg :checkpoint)
           (signal (car err) (cdr err))
         (prin1-to-string
          (list :id (plist-get msg :id)
                :error (format \"unreadable offload reply: %%S\" err))))))))
\(defun anvil-offload--format-condition (err)
  \"Return ERR as complete diagnostic text under canonical printer state.\"
  (let ((print-length nil)
        (print-level nil)
        (print-circle t)
        (print-gensym t)
        (print-unreadable-function nil)
        (print-continuous-numbering nil)
        (print-number-table nil)
        (print-symbols-bare nil)
        (print-quoted t)
        (print-integers-as-characters nil)
        (print-escape-control-characters nil)
        (print-escape-multibyte nil)
        (print-escape-newlines nil)
        (print-escape-nonascii nil)
        (print-charset-text-property t)
        (float-output-format nil))
    (format \"%%S\" err)))
\(defun anvil-offload--emit-frame (msg)
  \"Write MSG as one framed line to stdout.\"
  (send-string-to-terminal
   (concat anvil-offload--frame-prefix
           (base64-encode-string
            (encode-coding-string
             (anvil-offload--serialize-frame msg) 'utf-8-unix)
            t)
           \"\\n\")))
\(defvar anvil-offload--repl-current-id nil
  \"Request id currently being evaluated — tags checkpoint messages.\")
\(defun anvil-preempt-checkpoint (value &optional cursor)
  \"Send an interim (:value VALUE :cursor CURSOR) checkpoint, return VALUE.
Handlers call this periodically during long work so the main daemon
has the latest partial state if the call is killed over budget.\"
  (when anvil-offload--repl-current-id
    (anvil-offload--emit-frame
     (list :id anvil-offload--repl-current-id
           :checkpoint (list :value value :cursor cursor))))
  value)
\(condition-case nil
    (while t
      (let* ((read-circle t)
             (read-symbol-shorthands nil)
             (read-hide-char nil)
             (inhibit-interaction nil)
             (executing-kbd-macro nil)
             (noninteractive t)
             (minibuffer-local-map nil)
             (obarray anvil-offload--canonical-obarray)
             (msg (read t))
             (id (and (listp msg) (plist-get msg :id)))
             (payload (and (listp msg) (plist-get msg :payload)))
             (quit-after (and (listp msg) (plist-get msg :quit-after)))
             (form (and payload
                        (car (read-from-string
                              (decode-coding-string
                               (base64-decode-string payload)
                               'utf-8-unix))))))
        (when id
          (let* ((anvil-offload--repl-current-id id)
                 (_started
                  (anvil-offload--emit-frame (list :id id :started t)))
                 (reply
                  (condition-case err
                      (list :id id
                            :ok
                            (with-temp-buffer
                              (let ((standard-output (current-buffer)))
                                (eval form t))))
                    (error
                     (list :id id :error
                           (anvil-offload--format-condition err))))))
            (anvil-offload--emit-frame reply)
            (when quit-after
              (kill-emacs 0))))))
  (end-of-file (kill-emacs 0)))
" anvil-offload--frame-prefix)
  "Body of the REPL loop written into the subprocess init file.")

(defun anvil-offload--repl-init-file ()
  "Return the path to the REPL init file, rewriting it when stale."
  (unless (and anvil-offload--repl-init-file
               (file-exists-p anvil-offload--repl-init-file)
               (with-temp-buffer
                 (insert-file-contents anvil-offload--repl-init-file)
                 (equal (buffer-string) anvil-offload--repl-body)))
    (let ((file (make-temp-file "anvil-offload-repl-" nil ".el")))
      (with-temp-file file
        (let ((coding-system-for-write 'utf-8-unix))
          (insert anvil-offload--repl-body)))
      (setq anvil-offload--repl-init-file file)))
  anvil-offload--repl-init-file)

;;; Future struct

(cl-defstruct (anvil-future (:conc-name anvil-future--))
  id
  process
  status                 ; 'pending 'done 'error 'cancelled 'killed
  result
  err
  checkpoint             ; latest (:value V :cursor C) from subprocess, or nil
  started-at
  terminal-reason
  isolated-p
  on-start
  on-settle
  (created-at (float-time))
  done-at
  ;; Lifecycle decisions never trust the callback-visible fields above.
  ;; Callbacks receive the future itself for compatibility and can therefore
  ;; mutate those fields (including a sibling future's fields).  These private
  ;; registration slots are captured before publication and remain the
  ;; authoritative request identity and state.
  registration-p
  registered-id
  registered-process
  registered-status
  registered-isolated-p)

(defun anvil-offload--pending-key-for-future (future)
  "Return the pending-table key whose value is FUTURE, or nil."
  (when (hash-table-p anvil-offload--pending)
    (let (found)
      (maphash
       (lambda (id candidate)
         (when (and (null found) (eq candidate future))
           (setq found id)))
       anvil-offload--pending)
      found)))

(defun anvil-offload--ensure-registration
    (future &optional id process isolated)
  "Return FUTURE after ensuring immutable lifecycle registration.
ID, PROCESS, and ISOLATED are authoritative context captured by the caller.
They are used only for the first registration; subsequent calls preserve the
original request identity regardless of callback-visible field mutation."
  (unless (anvil-future--registration-p future)
    (let ((registered-id
           (or id
               (anvil-offload--pending-key-for-future future)
               (anvil-future--id future))))
      (setf (anvil-future--registration-p future) t
            (anvil-future--registered-id future) registered-id
            (anvil-future--registered-process future)
            (or process (anvil-future--process future))
            (anvil-future--registered-status future)
            (or (anvil-future--status future) 'pending)
            (anvil-future--registered-isolated-p future)
            (if (null isolated)
                (and (anvil-future--isolated-p future) t)
              (and isolated t)))))
  future)

(defun anvil-offload--sync-future-registration (future)
  "Restore callback-visible lifecycle fields from FUTURE's registration."
  (when (anvil-future--registration-p future)
    (setf (anvil-future--id future)
          (anvil-future--registered-id future)
          (anvil-future--process future)
          (anvil-future--registered-process future)
          (anvil-future--status future)
          (anvil-future--registered-status future)
          (anvil-future--isolated-p future)
          (anvil-future--registered-isolated-p future)))
  future)

(defun anvil-offload--registered-pending-p (future)
  "Return non-nil when FUTURE's authoritative state is pending."
  (anvil-offload--ensure-registration future)
  (eq 'pending (anvil-future--registered-status future)))

(defun anvil-offload--registered-process (future)
  "Return FUTURE's immutable owner process."
  (anvil-offload--ensure-registration future)
  (anvil-future--registered-process future))

(defun anvil-offload--registered-id (future)
  "Return FUTURE's immutable pending-table key."
  (anvil-offload--ensure-registration future)
  (anvil-future--registered-id future))

(defun anvil-offload--terminalize-silently
    (future id status payload reason)
  "Set FUTURE terminal without invoking callbacks.
ID is captured before arbitrary user code can mutate FUTURE.  STATUS,
PAYLOAD, and REASON match the ordinary settlement fields."
  (anvil-offload--ensure-registration future id)
  (let ((registered-id (anvil-future--registered-id future)))
    (when (and (hash-table-p anvil-offload--pending)
               (eq future (funcall anvil-offload--gethash-primitive registered-id anvil-offload--pending)))
      (funcall anvil-offload--remhash-primitive
               registered-id anvil-offload--pending)))
  (setf (anvil-future--registered-status future) status
        (anvil-future--status future) status
        (anvil-future--terminal-reason future) reason
        (anvil-future--done-at future) (float-time)
        (anvil-future--on-start future) nil
        (anvil-future--on-settle future) nil)
  (pcase status
    ('done (setf (anvil-future--result future) payload))
    ((or 'error 'killed) (setf (anvil-future--err future) payload)))
  (anvil-offload--sync-future-registration future))

(defun anvil-offload--invoke-callback (future callback event)
  "Invoke CALLBACK for FUTURE, containing errors and quits from filters.
EVENT is a diagnostic symbol used only in the error message."
  (when callback
    (condition-case err
        (funcall callback future)
      ((error quit)
       (message "anvil-offload: %s callback failed for %s: %s"
                event (anvil-offload--registered-id future)
                (error-message-string err))))))

(defun anvil-offload--mark-started (future)
  "Mark pending FUTURE started and notify its start callback once.
The request id and exact owner are captured before arbitrary callback
code.  If the callback exits nonlocally, cleanup never rereads mutable
future ownership fields or performs watched table publication."
  (anvil-offload--ensure-registration future)
  (anvil-offload--sync-future-registration future)
  (when (and (anvil-offload--registered-pending-p future)
             (null (anvil-future--started-at future)))
    (setf (anvil-future--started-at future) (float-time))
    (let ((callback (anvil-future--on-start future))
          (owner (anvil-future--registered-process future))
          (id (anvil-future--registered-id future))
          completed)
      (setf (anvil-future--on-start future) nil)
      (unwind-protect
          (progn
            (anvil-offload--invoke-callback future callback 'start)
            (anvil-offload--sync-future-registration future)
            (setq completed t))
        (unless completed
          (anvil-offload--sync-future-registration future)
          (when (anvil-offload--registered-pending-p future)
            (when (processp owner)
              (anvil-offload--mark-retiring owner))
            (anvil-offload--terminalize-silently
             future id 'killed "start callback aborted"
             'start-callback-aborted)
            (when (processp owner)
              (anvil-offload--defer-hard-delete-preserving-exit owner)))))))
  future)

(defun anvil-offload--settle (future status &optional payload reason)
  "Settle pending FUTURE once with STATUS, PAYLOAD, and REASON.
STATUS is one of `done', `error', `cancelled', or `killed'."
  (anvil-offload--ensure-registration future)
  (when (anvil-offload--registered-pending-p future)
    (let ((id (anvil-future--registered-id future))
          (callback (anvil-future--on-settle future)))
      (anvil-offload--terminalize-silently
       future id status payload reason)
      (unwind-protect
          (anvil-offload--invoke-callback future callback 'settle)
        (anvil-offload--sync-future-registration future)))
    t))

(defun anvil-future-status (future)
  "Return FUTURE's status (pending/done/error/cancelled/killed)."
  (anvil-offload--ensure-registration future)
  (anvil-future--registered-status future))

(defun anvil-future-checkpoint (future)
  "Return the latest checkpoint plist for FUTURE, or nil.
The plist has keys `:value' and `:cursor', matching the arguments
last handed to `anvil-preempt-checkpoint' inside the REPL."
  (anvil-future--checkpoint future))

(defun anvil-future-done-p (future)
  "Non-nil when FUTURE has settled (done/error/cancelled/killed)."
  (not (anvil-offload--registered-pending-p future)))

(defun anvil-future-value (future)
  "Return FUTURE's value; signal if errored, killed, cancelled, or pending."
  (pcase (anvil-future-status future)
    ('done      (anvil-future--result future))
    ('error     (error "anvil-offload: remote error: %s"
                       (anvil-future--err future)))
    ('killed    (error "anvil-offload: %s" (anvil-future--err future)))
    ('cancelled (error "anvil-offload: future was cancelled"))
    ('pending   (error "anvil-offload: future still pending"))
    (other      (error "anvil-offload: unknown status %S" other))))

(defun anvil-future-error (future)
  "Return the error payload of FUTURE, or nil if it has no error.
Both remotely errored and locally killed futures retain a diagnostic
payload."
  (and (memq (anvil-future-status future) '(error killed))
       (anvil-future--err future)))

(defun anvil-future-await (future &optional timeout)
  "Block until FUTURE settles or TIMEOUT seconds elapse.
Return non-nil if settled, nil on timeout.

FUTURE's REPL is prioritized during each poll and timers remain eligible,
but Emacs may still dispatch an already-ready foreign process filter.  Hard
containment belongs to the dedicated-daemon bridge watchdog.  If the REPL
dies, settle its pending futures synchronously rather than depending on the
sentinel's zero-delay fallback timer."
  (let* ((limit (or timeout anvil-offload-default-await-timeout))
         (deadline (and limit (+ (float-time) limit)))
         (proc (anvil-offload--registered-process future)))
    ;; A callback can await a sibling whose terminal frame was already
    ;; coalesced behind the current frame.  The outer filter published that
    ;; remainder before invoking the callback, so service it synchronously
    ;; rather than waiting for process output that has already arrived.
    (when (and (not (anvil-future-done-p future))
               (anvil-offload--filter-queue-has-complete-line-p proc))
      (anvil-offload--filter proc ""))
    (while (and (not (anvil-future-done-p future))
                (or (null deadline) (< (float-time) deadline))
                (process-live-p proc))
      (accept-process-output
       proc anvil-offload-poll-interval nil t))
    (when (and (not (anvil-future-done-p future))
               (not (process-live-p proc)))
      ;; Give a final process-filter callback and the sentinel's zero-delay
      ;; fallback timer one bounded event-loop turn.  If neither settles the
      ;; future, do so synchronously; no pending future may outlive its REPL.
      (accept-process-output
       proc anvil-offload-poll-interval nil t)
      (unless (anvil-future-done-p future)
        (anvil-offload--finalize-dead-process
         proc
         (or (funcall anvil-offload--process-get-primitive proc 'anvil-offload-death-reason)
             (format "offload REPL exited: %s" (process-status proc))))))
    (anvil-future-done-p future)))

(defun anvil-future-cancel (future)
  "Drop local tracking for FUTURE and mark it cancelled.
The subprocess keeps running; its eventual reply is silently
discarded.  To hard-stop offload work, call `anvil-future-kill'
\(per-future) or `anvil-offload-stop-repl' (whole pool)."
  (anvil-offload--settle future 'cancelled nil 'cancelled)
  future)

(defun anvil-future-kill (future &optional reason)
  "Hard-kill the subprocess slot owning pending FUTURE.
Unlike anvil-future-cancel, this attempts to terminate the REPL process.
Before settlement or its user callback, the exact child is marked
retiring so reentrant dispatch cannot reuse it.  Pool or auxiliary
ownership is cleared only after the child is observed dead; failed
termination leaves the marked child tracked for a later retry.

The elapsed wall time (seconds since the future's created-at) is stored
in anvil-future--err.  Repeated calls are idempotent.  Return FUTURE."
  (when (anvil-offload--registered-pending-p future)
    (let* ((proc (anvil-offload--registered-process future))
           (elapsed (- (float-time) (anvil-future--created-at future)))
           (message (if reason
                        (format "%s after %.2fs" reason elapsed)
                      (format "killed after %.2fs" elapsed)))
           settled)
      ;; Mark before settlement: anvil-offload--settle invokes arbitrary user
      ;; callbacks, and no callback may reacquire this exact child.
      (anvil-offload--mark-retiring proc)
      ;; Settle locally before signalling the child.  A synchronous
      ;; sentinel/filter callback can no longer race this future into a
      ;; different terminal state.
      (unwind-protect
          (progn
            (anvil-offload--settle
             future 'killed message (or reason 'killed))
            (setq settled t))
        (if settled
            (anvil-offload--hard-delete-process proc)
          (anvil-offload--defer-hard-delete-preserving-exit proc)))))
  future)

(defun anvil-future-elapsed (future)
  "Return seconds between FUTURE's creation and now (or completion)."
  (- (or (anvil-future--done-at future) (float-time))
     (anvil-future--created-at future)))

;;; Process filter / sentinel

(defvar anvil-offload--repl-current-id nil
  "Bound to the request id while a handler runs inside the REPL.
Defined in the daemon as a no-op anchor so `anvil-preempt-checkpoint'
compiles; the subprocess's init file has its own defvar which is what
actually tags outbound checkpoint messages.")

(defun anvil-preempt-checkpoint (value &optional cursor)
  "Record VALUE (and optional CURSOR) as an interim checkpoint.
Inside the offload REPL subprocess this writes a
`(:id N :checkpoint (:value VALUE :cursor CURSOR))' message so the
main daemon can fold it into the `partial' reply on budget exceed.
In the main daemon this is a harmless no-op — handlers can call it
unconditionally.  Returns VALUE."
  (when (and anvil-offload--repl-current-id
             (fboundp 'send-string-to-terminal))
    (send-string-to-terminal
     (concat anvil-offload--frame-prefix
             (anvil-offload--frame-encode-payload
              (prin1-to-string
               (list :id anvil-offload--repl-current-id
                     :checkpoint (list :value value :cursor cursor))))
             "\n")))
  value)

(defun anvil-offload--settle-reply
    (proc future status payload reason)
  "Settle FUTURE from PROC and hard-delete its captured isolated child.
The exact isolated ownership predicate is snapshotted before settlement.
A nonlocal callback exit defers callback-free exact deletion so cleanup
cannot replace the original exit."
  (anvil-offload--ensure-registration future)
  (let ((delete-isolated
         (and (anvil-future--registered-isolated-p future)
              (eq proc (anvil-future--registered-process future))))
        settled
        completed)
    (unwind-protect
        (progn
          (setq settled
                (anvil-offload--settle future status payload reason))
          (setq completed t))
      (when delete-isolated
        (if completed
            (anvil-offload--hard-delete-process proc)
          (anvil-offload--defer-hard-delete-preserving-exit proc))))
    settled))

(defun anvil-offload--dispatch-reply (proc msg)
  "Route decoded reply MSG from PROC to its registered future, if any.
Every started, checkpoint, and terminal frame is accepted only from the
process that owns its request id.  Terminal replies hard-delete an isolated
one-shot child after settlement."
  (when (listp msg)
    (let* ((id (plist-get msg :id))
           (table (anvil-offload--ensure-pending))
           (future (and id (funcall anvil-offload--gethash-primitive id table))))
      (when future
        (anvil-offload--ensure-registration future id)
        (anvil-offload--sync-future-registration future)
        (if (not (eq proc (anvil-future--registered-process future)))
            (unless (funcall anvil-offload--process-get-primitive proc 'anvil-owner-mismatch-logged)
              (funcall anvil-offload--process-put-primitive proc 'anvil-owner-mismatch-logged t)
              (message "anvil-offload: ignored request %s frame from %s"
                       id (funcall anvil-offload--process-name-primitive proc)))
          (cond
           ((not (anvil-offload--registered-pending-p future))
            (when (eq future (funcall anvil-offload--gethash-primitive id table))
              (funcall anvil-offload--remhash-primitive id table)))
           ((plist-member msg :started)
            (anvil-offload--mark-started future))
           ((plist-member msg :checkpoint)
            (setf (anvil-future--checkpoint future)
                  (plist-get msg :checkpoint)))
           ((plist-member msg :ok)
            (anvil-offload--settle-reply
             proc future 'done (plist-get msg :ok) 'done))
           ((plist-member msg :error)
            (anvil-offload--settle-reply
             proc future 'error (plist-get msg :error) 'remote-error))))))))

(defun anvil-offload--drain-filter-remainder (proc)
  "Drain PROC's transactionally retained filter bytes once."
  (funcall anvil-offload--process-put-primitive
           proc 'anvil-offload-drain-timer nil)
  (when (and (processp proc)
             (> (anvil-offload--filter-queue-total-bytes proc) 0))
    (anvil-offload--filter proc "")))

(defun anvil-offload--schedule-filter-drain (proc)
  "Schedule one idempotent replay of PROC's retained filter bytes."
  (unless
      (timerp
       (funcall anvil-offload--process-get-primitive
                proc 'anvil-offload-drain-timer))
    (funcall anvil-offload--process-put-primitive
             proc 'anvil-offload-drain-timer
             (anvil-offload--schedule-zero-delay
              #'anvil-offload--drain-filter-remainder proc))))

(defun anvil-offload--valid-reply-plist-p (value &optional allowed-keys)
  "Return non-nil when VALUE is a finite, even, known-key plist.
ALLOWED-KEYS defaults to the top-level reply protocol keys."
  (let ((tail value)
        (seen (make-hash-table :test #'eq))
        (keys (or allowed-keys '(:id :started :checkpoint :ok :error)))
        seen-keys
        valid)
    (setq valid t)
    (while (and valid (consp tail))
      (if (funcall anvil-offload--gethash-primitive tail seen)
          (setq valid nil)
        (puthash tail t seen)
        (let ((key (pop tail)))
          (if (or (not (memq key keys))
                  (memq key seen-keys)
                  (not (consp tail)))
              (setq valid nil)
            (push key seen-keys)
            (pop tail)))))
    (and valid (null tail))))

(defun anvil-offload--decode-reply-line (line prefix-index)
  "Decode and validate one framed reply LINE beginning at PREFIX-INDEX."
  (let* ((payload
          (substring
           line (+ prefix-index (length anvil-offload--frame-prefix))))
         (decoded (anvil-offload--frame-decode-payload payload))
         (read-circle t)
         (read-symbol-shorthands nil)
         (obarray anvil-offload--canonical-obarray)
         (parsed (read-from-string decoded))
         (message (car parsed))
         (end (cdr parsed))
         (kinds
          (and (anvil-offload--valid-reply-plist-p message)
               (cl-count-if
                (lambda (key) (plist-member message key))
                '(:started :checkpoint :ok :error)))))
    (unless (string-match-p "\\`[[:space:]]*\\'" (substring decoded end))
      (error "trailing data after reply object"))
    (unless (and (anvil-offload--valid-reply-plist-p message)
                 (integerp (plist-get message :id))
                 (= kinds 1)
                 (or (not (plist-member message :started))
                     (eq t (plist-get message :started)))
                 (or (not (plist-member message :checkpoint))
                     (let ((checkpoint (plist-get message :checkpoint)))
                       (and (anvil-offload--valid-reply-plist-p
                             checkpoint '(:value :cursor))
                            (plist-member checkpoint :value)
                            (plist-member checkpoint :cursor))))
                 (or (not (plist-member message :error))
                     (stringp (plist-get message :error))))
      (error "invalid reply shape"))
    message))

(defconst anvil-offload--filter-queue-tag 'anvil-offload-filter-queue
  "Tag stored in process-local incremental filter queue vectors.")

(defun anvil-offload--filter-queue-p (value)
  "Return non-nil when VALUE is an incremental filter queue."
  (and (vectorp value)
       (= (length value) 6)
       (eq (aref value 0) anvil-offload--filter-queue-tag)))

(defun anvil-offload--filter-queue (proc)
  "Return PROC's incremental queue, upgrading a legacy string losslessly."
  (let ((value
         (funcall anvil-offload--process-get-primitive
                  proc 'anvil-pending-bytes)))
    (if (anvil-offload--filter-queue-p value)
        value
      (unless (or (null value) (stringp value))
        (error "anvil-offload: invalid filter queue state"))
      (let* ((base (or value ""))
             (state
              (vector anvil-offload--filter-queue-tag
                      base 0 nil nil (string-bytes base))))
        (funcall anvil-offload--process-put-primitive
                 proc 'anvil-pending-bytes state)
        state))))

(defun anvil-offload--filter-queue-append (state string)
  "Append STRING to STATE without copying previously queued bytes."
  (unless (string-empty-p string)
    (aset state 3 (cons string (aref state 3)))
    (aset state 4
          (or (aref state 4) (string-match-p "\n" string)))
    (aset state 5 (+ (aref state 5) (string-bytes string))))
  state)

(defun anvil-offload--filter-queue-materialize (state)
  "Materialize STATE's unconsumed base and chunks exactly once."
  (let ((base (aref state 1))
        (offset (aref state 2))
        (chunks (aref state 3)))
    (when (or chunks (> offset 0))
      (setq base
            (mapconcat
             #'identity
             (cons (if (zerop offset) base (substring base offset))
                   (nreverse (copy-sequence chunks)))
             ""))
      (aset state 1 base)
      (aset state 2 0)
      (aset state 3 nil)
      (aset state 4 nil)))
  state)

(defun anvil-offload--filter-queue-line-end (state)
  "Return the next newline position in STATE's materialized base, or nil."
  (string-match "\n" (aref state 1) (aref state 2)))

(defun anvil-offload--filter-queue-has-complete-line-p (proc)
  "Return non-nil when PROC's logical queue contains a complete line."
  (let ((state (anvil-offload--filter-queue proc)))
    (or (anvil-offload--filter-queue-line-end state)
        (aref state 4))))

(defun anvil-offload--filter-queue-total-bytes (proc)
  "Return the logical byte count retained for PROC."
  (aref (anvil-offload--filter-queue proc) 5))

(defun anvil-offload--pending-bytes-string (proc)
  "Return PROC's logical pending bytes as a compatibility snapshot."
  (let ((state (anvil-offload--filter-queue proc)))
    (anvil-offload--filter-queue-materialize state)
    (aref state 1)))

(defun anvil-offload--filter-queue-pop-line (proc)
  "Pop and return (LINE . BYTE-COUNT) from PROC, or nil if incomplete."
  (let* ((state (anvil-offload--filter-queue proc))
         (line-end (anvil-offload--filter-queue-line-end state)))
    (when (and (null line-end) (aref state 4))
      (anvil-offload--filter-queue-materialize state)
      (setq line-end (anvil-offload--filter-queue-line-end state)))
    (when line-end
      (let* ((base (aref state 1))
             (offset (aref state 2))
             (line (substring base offset line-end))
             (consumed-bytes (1+ (string-bytes line)))
             (next (1+ line-end)))
        ;; Commit the exact remainder before dispatch can invoke callbacks.
        (aset state 2 next)
        (aset state 5 (- (aref state 5) consumed-bytes))
        (when (= next (length base))
          (aset state 1 "")
          (aset state 2 0))
        (cons line consumed-bytes)))))

(defun anvil-offload--filter-queue-clear (proc)
  "Replace PROC's logical queue with one empty incremental state."
  (funcall anvil-offload--process-put-primitive
           proc 'anvil-pending-bytes
           (vector anvil-offload--filter-queue-tag "" 0 nil nil 0)))

(defun anvil-offload--drain-filter (proc)
  "Drain complete frames from PROC's shared incremental byte queue."
  (let ((prefix-re (regexp-quote anvil-offload--frame-prefix))
        failure
        record)
    (while (and (null failure)
                (setq record
                      (anvil-offload--filter-queue-pop-line proc)))
      (let ((raw-line (car record))
            (frame-bytes (cdr record)))
        (if (> frame-bytes anvil-offload-max-frame-bytes)
            (setq failure
                  (format "offload frame exceeded %d-byte limit"
                          anvil-offload-max-frame-bytes))
          (let* ((line (anvil-offload--strip-ignored-junk-prefixes raw-line))
                 (idx (and (not (string-blank-p line))
                           (string-match prefix-re line))))
            (cond
             ((string-blank-p line))
             ((null idx)
              (let ((preview (anvil-offload--remember-junk-reply proc line)))
                (unless
                    (funcall anvil-offload--process-get-primitive
                             proc 'anvil-junk-reply-logged)
                  (funcall anvil-offload--process-put-primitive
                           proc 'anvil-junk-reply-logged t)
                  (message "anvil-offload: dropped junk reply line: %S"
                           preview))))
             (t
              ;; The queue offset was committed before decoding and dispatch.
              ;; A callback may service or append later frames recursively
              ;; without replaying this one or copying the queued tail.
              (condition-case err
                  (anvil-offload--dispatch-reply
                   proc (anvil-offload--decode-reply-line line idx))
                (error
                 (setq failure
                       (format "unreadable offload reply frame: %s"
                               (error-message-string err)))))))))))
    (when (and (null failure)
               (not (anvil-offload--filter-queue-has-complete-line-p proc))
               (> (anvil-offload--filter-queue-total-bytes proc)
                  anvil-offload-max-frame-bytes))
      (setq failure
            (format "offload frame exceeded %d-byte limit"
                    anvil-offload-max-frame-bytes)))
    (when failure
      (anvil-offload--filter-queue-clear proc)
      (anvil-offload--mark-retiring proc)
      (unwind-protect
          (anvil-offload--finalize-dead-process proc failure)
        (anvil-offload--hard-delete-process proc)))
    (null failure)))

(defun anvil-offload--filter (proc string)
  "Queue STRING on PROC and dispatch complete framed replies.
Incoming chunks are retained without copying prior bytes.  The byte ceiling
applies to each newline-terminated frame and to the one unterminated
remainder, not to a process-filter chunk that coalesces valid frames."
  (anvil-offload--filter-queue-append
   (anvil-offload--filter-queue proc) string)
  (let ((outermost
         (not
          (funcall anvil-offload--process-get-primitive
                   proc 'anvil-offload-filter-active))))
    (when outermost
      (funcall anvil-offload--process-put-primitive
               proc 'anvil-offload-filter-active t))
    (let (completed)
      (unwind-protect
          (progn
            (anvil-offload--drain-filter proc)
            (setq completed t))
        (when outermost
          (funcall anvil-offload--process-put-primitive
                   proc 'anvil-offload-filter-active nil))
        (unless completed
          ;; The current frame was committed before dispatch; later logical
          ;; frames remain available for one idempotently scheduled drain.
          (anvil-offload--schedule-filter-drain proc))))))

(defun anvil-offload--terminalize-dead-process-silently (proc reason)
  "Terminalize every pending future still owned by dead PROC with REASON.
Return callback records after registration state has been synchronized."
  (let ((table (anvil-offload--ensure-pending))
        records)
    (maphash
     (lambda (id future)
       (anvil-offload--ensure-registration future id)
       (when (and (eq proc (anvil-future--registered-process future))
                  (anvil-offload--registered-pending-p future))
         (push (list id future (anvil-future--on-settle future)) records)))
     table)
    (dolist (record records)
      (anvil-offload--terminalize-silently
       (nth 1 record) (nth 0 record) 'error reason 'child-exit))
    (dolist (record records)
      (anvil-offload--sync-future-registration (nth 1 record)))
    records))

(defun anvil-offload--finalize-dead-process (proc reason)
  "Settle pending futures still owned by dead PROC with REASON."
  (let ((records
         (anvil-offload--terminalize-dead-process-silently proc reason)))
    (unwind-protect
        (dolist (record records)
          (anvil-offload--invoke-callback
           (nth 1 record) (nth 2 record) 'settle))
      ;; A tagged nonlocal exit can skip later callbacks, but it cannot leave
      ;; callback-visible sibling fields inconsistent with terminal state.
      (dolist (record records)
        (anvil-offload--sync-future-registration (nth 1 record))))))

(defun anvil-offload--release-dead-process-resources
    (proc isolated buffer owned-buffer)
  "Release exact dead PROC and its exclusively owned BUFFER.
Hard deletion runs after final-frame settlement.  Failed deletion retains
global ownership for a later retry."
  (unwind-protect
      (anvil-offload--hard-delete-process proc)
    (when (and isolated
               (eq buffer owned-buffer)
               (buffer-live-p buffer)
               (not (anvil-offload--buffer-has-live-peer-p proc buffer)))
      (kill-buffer buffer))))

(defun anvil-offload--finalize-dead-process-and-resources
    (proc base-reason isolated buffer owned-buffer)
  "Settle PROC futures, then release its exact process and buffer resources.
BASE-REASON excludes non-frame diagnostics, which are sampled only after the
final filter callback has had an opportunity to run."
  (funcall anvil-offload--process-put-primitive
           proc 'anvil-offload-death-finalizer-active t)
  (unwind-protect
      (let ((reason (anvil-offload--exit-reason proc base-reason)))
        (funcall anvil-offload--process-put-primitive
                 proc 'anvil-offload-death-reason reason)
        (unwind-protect
            (anvil-offload--finalize-dead-process proc reason)
          (anvil-offload--release-dead-process-resources
           proc isolated buffer owned-buffer)))
    (funcall anvil-offload--process-put-primitive
             proc 'anvil-offload-death-finalizer-active nil)))

(defun anvil-offload--buffer-has-live-peer-p (proc buffer)
  "Return non-nil when BUFFER is still owned by a live process other than PROC."
  (and
   (bufferp buffer)
   (cl-some
    (lambda (candidate)
      (and (processp candidate)
           (not (eq candidate proc))
           (process-live-p candidate)
           (eq (funcall anvil-offload--process-buffer-primitive candidate)
               buffer)))
    (funcall anvil-offload--process-list-primitive))))

(defun anvil-offload--sentinel (proc event)
  "Handle death of PROC; fail only pending futures bound to exact PROC.
Filtering by process identity is load-bearing: if the REPL is stopped and a
fresh one spawned before this sentinel runs, the new REPL's futures must not
be settled.  EVENT describes the process status."
  (when (and
         (processp proc)
         (not (process-live-p proc))
         (not
          (funcall anvil-offload--process-get-primitive
                   proc 'anvil-offload-death-finalizer-active)))
    (let (reason isolated owned-buffer buffer scheduled-p finalizer-args)
      ;; The unwind boundary begins before every property, buffer, and timer
      ;; operation.  Scheduler failure terminalizes pending futures silently
      ;; so no request can remain pending without a future callback source.
      (unwind-protect
          (progn
            (setq reason
                  (format "offload REPL exited: %s" (string-trim event))
                  buffer
                  (funcall anvil-offload--process-buffer-primitive proc))
            (setq isolated
                  (eq
                   t
                   (cl-some
                    (lambda (table)
                      (funcall anvil-offload--gethash-primitive proc table))
                    (anvil-offload--registered-ownership-tables))))
            (setq owned-buffer
                  (funcall anvil-offload--process-get-primitive
                           proc 'anvil-offload-owned-buffer))
            (setq isolated
                  (or
                   isolated
                   (funcall anvil-offload--process-get-primitive
                            proc 'anvil-offload-isolated)))
            (funcall anvil-offload--process-put-primitive
                     proc 'anvil-offload-death-reason reason)
            ;; Let any final filter callback drain queued bytes before
            ;; settlement and exact process deletion.  Ownership remains
            ;; durable until this callback completes or unwinds.
            (setq finalizer-args
                  (list proc reason isolated buffer owned-buffer))
            (apply
             #'anvil-offload--schedule-zero-delay
             #'anvil-offload--finalize-dead-process-and-resources
             finalizer-args)
            (setq scheduled-p t))
        ;; A tagged scheduler exit propagates after schedule-zero-delay has
        ;; installed its exact fallback timer.  Observe that publication here
        ;; so unwind cleanup does not recursively delete a process that Emacs
        ;; is still removing inside its sentinel.
        (unless scheduled-p
          (setq scheduled-p
                (and
                 finalizer-args
                 (anvil-offload--exact-zero-delay-timer-scheduled-p
                  #'anvil-offload--finalize-dead-process-and-resources
                  finalizer-args))))
        (unless scheduled-p
          (funcall anvil-offload--process-put-primitive
                   proc 'anvil-offload-death-finalizer-active t)
          (unwind-protect
              (unwind-protect
                  (when reason
                    (condition-case nil
                        (anvil-offload--terminalize-dead-process-silently
                         proc (anvil-offload--exit-reason proc reason))
                      ((error quit) nil)))
                (anvil-offload--release-dead-process-resources
                 proc isolated buffer owned-buffer))
            (funcall anvil-offload--process-put-primitive
                     proc 'anvil-offload-death-finalizer-active nil)))))))

;;; Pool lifecycle

(defun anvil-offload--process-command ()
  "Return the command used for an offload subprocess."
  (append
   (list anvil-offload-emacs-bin "-Q" "--batch")
   (cl-mapcan (lambda (file) (list "-l" file)) anvil-offload-init-files)
   (list "-l" (anvil-offload--repl-init-file))))

(defun anvil-offload--spawn-process-matches-p (proc name buffer)
  "Return non-nil when PROC retains exact requested transport identity."
  (and
   (processp proc)
   (let ((actual-name
          (funcall anvil-offload--process-name-primitive proc)))
     (and
      (stringp actual-name)
      (eq
       0
       (string-match
        (concat
         (regexp-quote name) "\\(?:<[0-9]+>\\)?\\'")
        actual-name))))
   (eq (funcall anvil-offload--process-filter-primitive proc)
       #'anvil-offload--filter)
   (eq (funcall anvil-offload--process-buffer-primitive proc)
       buffer)))

(defun anvil-offload--new-processes-since (prior)
  "Return every process identity created after PRIOR."
  (cl-remove-if
   (lambda (proc) (memq proc prior))
   (funcall anvil-offload--process-list-primitive)))

(defun anvil-offload--cleanup-spawned-processes
    (processes isolated ownership)
  "Own and hard-delete exact constructor PROCESSES.
ISOLATED selects the custody marker and OWNERSHIP is already registered."
  (dolist (created (cl-delete-duplicates processes :test #'eq))
    (when (processp created)
      (condition-case cleanup-error
          (progn
            (anvil-offload--track-process-ownership
             created (if isolated t :slot-staging) ownership)
            (funcall anvil-offload--process-put-primitive created 'anvil-pending-bytes "")
            (funcall anvil-offload--process-put-primitive created 'anvil-offload-protocol-version
                         anvil-offload--protocol-version)
            (when isolated
              (funcall anvil-offload--process-put-primitive created 'anvil-offload-isolated t))
            (anvil-offload--hard-delete-process created))
        ((error quit)
         (message
          "anvil-offload: spawned child cleanup deferred: %s"
          (error-message-string cleanup-error)))))))

(defun anvil-offload--spawn-named-process (name &optional isolated)
  "Spawn and publish an owned REPL subprocess named NAME.
ISOLATED selects one-shot ownership.  Each constructor owns its full delta on
nonlocal or invalid exit, while a successful constructor owns the returned
child plus every new matching transport sibling.  Preexisting peers and
unrelated successful-constructor helpers are never claimed."
  (let* ((inhibit-quit t)
         ;; Create and globally register the table before any child exists, so
         ;; a watcher failure cannot strand a process during first use.
         (ownership (anvil-offload--ensure-isolated-processes))
         (candidate
          (and anvil-offload-spawn-environment-function
               (funcall anvil-offload-spawn-environment-function)))
         (process-environment
          (copy-sequence (or candidate process-environment)))
         (buffer-name (format " *%s*" name))
         (existing-buffer (get-buffer buffer-name))
         (buffer (or existing-buffer (generate-new-buffer buffer-name)))
         (owns-buffer (null existing-buffer))
         (prior-processes
          (funcall anvil-offload--process-list-primitive))
         proc
         constructor-returned-p
         constructor-processes
         invalid-p
         owned-processes
         completed)
    (unwind-protect
        (progn
          (setq proc
                (funcall anvil-offload--make-process-primitive
                 :name name
                 :buffer buffer
                 :command (anvil-offload--process-command)
                 :connection-type 'pipe
                 :coding 'utf-8-unix
                 :noquery t
                 :filter #'anvil-offload--filter
                 :sentinel #'anvil-offload--sentinel))
          (setq constructor-returned-p t)
          ;; Establish callback-free exact custody before any process-list,
          ;; name, filter, or property query can leave nonlocally.
          (when (and (processp proc)
                     (not (memq proc prior-processes)))
            (funcall anvil-offload--puthash-primitive
                     proc :constructor-return ownership)
            (funcall anvil-offload--process-put-primitive
                     proc 'anvil-offload-owned-buffer
                     (and owns-buffer buffer)))
          (setq constructor-processes
                (anvil-offload--new-processes-since prior-processes))
          (unless
              (and (processp proc)
                   (not (memq proc prior-processes))
                   (anvil-offload--spawn-process-matches-p proc name buffer))
            (setq invalid-p t)
            (cond
             ((not (processp proc))
              (error "anvil-offload: constructor returned no process"))
             ((memq proc prior-processes)
              (error
               "anvil-offload: constructor returned a preexisting process"))
             (t
              (error
               "anvil-offload: constructor returned an invalid process"))))
          (setq owned-processes
                (cl-delete-duplicates
                 (cons
                  proc
                  (cl-remove-if-not
                   (lambda (created)
                     (anvil-offload--spawn-process-matches-p created name buffer))
                   constructor-processes))
                 :test #'eq))
          ;; Publish custody for every matching constructor child before the
          ;; first process property or destructive callback.
          (dolist (created owned-processes)
            (anvil-offload--track-process-ownership
             created (if isolated t :slot-staging) ownership))
          ;; Duplicate exact children are never returned or left reusable.
          (dolist (created owned-processes)
            (unless (eq created proc)
              (anvil-offload--hard-delete-process created)))
          (funcall anvil-offload--process-put-primitive proc 'anvil-pending-bytes "")
          (funcall anvil-offload--process-put-primitive proc 'anvil-offload-protocol-version
                       anvil-offload--protocol-version)
          (when isolated
            (funcall anvil-offload--process-put-primitive proc 'anvil-offload-isolated t))
          (setq completed t)
          proc)
      (unless completed
        (let ((created
               (cond
                (invalid-p constructor-processes)
                ((not constructor-returned-p)
                 (anvil-offload--new-processes-since prior-processes))
                (owned-processes owned-processes)
                (t
                 (cl-remove-if-not
                  (lambda (candidate)
                    (anvil-offload--spawn-process-matches-p candidate name buffer))
                  (anvil-offload--new-processes-since prior-processes))))))
          ;; Never include a process that existed before this constructor,
          ;; even when its invalid return was stored in PROC.
          (setq created
                (cl-remove-if
                 (lambda (candidate)
                   (memq candidate prior-processes))
                 created))
          (anvil-offload--cleanup-spawned-processes
           created isolated ownership)
          (when
              (and
               owns-buffer
               (buffer-live-p buffer)
               (let ((buffer-process (get-buffer-process buffer)))
                 (or (null buffer-process)
                     (memq buffer-process created)))
               (not
                (cl-some
                 (lambda (candidate)
                   (and (processp candidate)
                        (process-live-p candidate)))
                 created)))
            (kill-buffer buffer)))))))

(defun anvil-offload--spawn-process (slot-index)
  "Spawn a fresh pooled REPL subprocess tagged for SLOT-INDEX."
  (anvil-offload--spawn-named-process
   (format "anvil-offload-repl-%d" slot-index)))

(defun anvil-offload--spawn-isolated-process (request-id)
  "Spawn a one-shot REPL subprocess for REQUEST-ID."
  (anvil-offload--spawn-named-process
   (format "anvil-offload-job-%d" request-id) t))

(defun anvil-offload--retire-captured-pool (pool)
  "Attempt to retire all captured pool generations, including POOL.
The caller must hold the cleanup-active guard.  Every globally retired
vector is snapshotted and retained before any destructive callback, and
all of their children are marked before the first delete.  Return
non-nil only when POOL is still the primary pool and every retired
generation has converged to empty."
  (unless (anvil-offload--cleanup-active-p)
    (error "anvil-offload: pool cleanup lacks its active guard"))
  (anvil-offload--stage-retirement-pools
   (cons pool (copy-sequence anvil-offload--retired-pools)))
  (when (vectorp pool)
    (cl-pushnew pool anvil-offload--retired-pools :test #'eq))
  (let ((pools
         (cl-delete-duplicates
          (cl-remove-if-not
           #'vectorp (copy-sequence anvil-offload--retired-pools))
          :test #'eq))
        processes)
    (when (hash-table-p anvil-offload--fallback-processes)
      (maphash
       (lambda (proc reason)
         (when (and (processp proc) (eq reason :pool-retiring))
           (cl-pushnew proc processes :test #'eq)))
       anvil-offload--fallback-processes))
    ;; Capture and mark the entire retirement set before the first callback.
    (dolist (retired pools)
      (dotimes (i (length retired))
        (let ((proc (aref retired i)))
          (when (processp proc)
            (cl-pushnew proc processes :test #'eq)
            (anvil-offload--mark-retiring proc)))))
    (dolist (proc processes)
      (anvil-offload--hard-delete-process proc))
    ;; Destructive callbacks may replace the global retired list.  Reconcile
    ;; every locally captured generation from observed process liveness before
    ;; publishing convergence, then union survivors back with any generations
    ;; installed during cleanup.
    (dolist (retired pools)
      (dotimes (i (length retired))
        (let ((proc (aref retired i)))
          (when (and (processp proc)
                     (not (process-live-p proc))
                     (not
                      (memq
                       proc
                       (funcall anvil-offload--process-list-primitive))))
            (aset retired i nil)))))
    (let ((survivors
           (cl-remove-if-not
            (lambda (retired)
              (and (vectorp retired)
                   (cl-some #'identity (append retired nil))))
            pools))
          (current
           (cl-remove-if-not
            (lambda (retired)
              (and (vectorp retired)
                   (cl-some #'identity (append retired nil))))
            anvil-offload--retired-pools)))
      (setq anvil-offload--retired-pools
            (cl-delete-duplicates (append survivors current) :test #'eq)))
    (and (eq pool anvil-offload--pool)
         (not (cl-some
               (lambda (retired)
                 (cl-some #'identity (append retired nil)))
               pools))
         (null anvil-offload--retired-pools)
         (not (anvil-offload--fallback-pool-retirement-p)))))

(defun anvil-offload--ensure-pool-vector ()
  "Return a dispatchable pool sized to `anvil-offload-pool-size'.
A size change retires every captured generation transactionally.
Reentrant dispatch fails while cleanup is active, and dispatch stays
closed after an incomplete whole-subsystem stop.  A signaling watcher
leaves the old primary and fail-closed flags intact; reentrant public
calls observe the active guard."
  (when (anvil-offload--cleanup-active-p)
    (error "anvil-offload: pool cleanup already active"))
  (when (anvil-offload--stop-incomplete-p)
    (error "anvil-offload: stop cleanup still in progress"))
  (let ((requested-size (max 1 anvil-offload-pool-size)))
    (if (and (not anvil-offload--pool-retiring-p)
             (null anvil-offload--retired-pools)
             (not (anvil-offload--fallback-pool-retirement-p))
             anvil-offload--pool
             (= (length anvil-offload--pool) requested-size))
        anvil-offload--pool
      (anvil-offload--with-cleanup-guard
        (let ((pool anvil-offload--pool))
          (anvil-offload--stage-retirement-pools
           (cons pool (copy-sequence anvil-offload--retired-pools)))
          (setq anvil-offload--pool-retiring-p t)
          (unless (anvil-offload--retire-captured-pool pool)
            (error "anvil-offload: pool retirement still in progress"))
          ;; Publish the new primary exactly once.  Variable watchers run before
          ;; assignment on Emacs 30: a signal preserves the old empty primary
          ;; and the retiring flag, while public reentry sees the active guard.
          (let ((new-pool
                 (make-vector (max 1 anvil-offload-pool-size) nil)))
            (setq anvil-offload--pool new-pool)
            (setq anvil-offload--pool-retiring-p nil)
            new-pool))))))

(defun anvil-offload--ensure-slot (idx)
  "Ensure slot IDX holds a live, protocol-compatible REPL; return it.
Protocol and dead-slot replacement is a slot-local transaction: mark the
old process retiring and block reentrant dispatch before deletion, but do
not sweep healthy peer slots or their pending futures.  A new process is
staged in globally registered ownership tables until every publication
step succeeds."
  (anvil-offload--ensure-pool-vector)
  (when (anvil-offload--cleanup-active-p)
    (error "anvil-offload: pool cleanup already active"))
  (let* ((pool anvil-offload--pool)
         (cur (aref pool idx)))
    (if (and (processp cur)
             (process-live-p cur)
             (not (funcall anvil-offload--process-get-primitive cur 'anvil-offload-retiring))
             (eq (funcall anvil-offload--process-get-primitive cur 'anvil-offload-protocol-version)
                 anvil-offload--protocol-version))
        cur
      (anvil-offload--with-cleanup-guard
        (when cur
          (unless (processp cur)
            (error "anvil-offload: invalid child in slot %d" idx))
          (anvil-offload--mark-retiring cur)
          (unless (anvil-offload--hard-delete-process cur)
            (error "anvil-offload: cannot replace live child in slot %d" idx)))
        (unless (and (eq pool anvil-offload--pool)
                     (null (aref pool idx)))
          (error "anvil-offload: slot %d changed during replacement" idx))
        (let* ((inhibit-quit t)
               (initial-staging (anvil-offload--ensure-isolated-processes))
               proc
               completed)
          (unwind-protect
              (progn
                (setq proc (anvil-offload--spawn-process idx))
                (anvil-offload--track-process-ownership
                 proc :slot-staging initial-staging)
                (let ((current-staging
                       (anvil-offload--ensure-isolated-processes)))
                  (anvil-offload--track-process-ownership
                   proc :slot-staging current-staging))
                (funcall anvil-offload--process-put-primitive proc 'anvil-offload-staged-slot idx)
                (unless (and (eq pool anvil-offload--pool)
                             (null (aref pool idx)))
                  (error "anvil-offload: slot %d changed during spawn" idx))
                (aset pool idx proc)
                ;; Staging is removed only after the exact slot assignment.
                (funcall anvil-offload--process-put-primitive proc 'anvil-offload-staged-slot nil)
                (dolist
                    (table
                     (copy-sequence
                      (funcall anvil-offload--process-get-primitive proc 'anvil-offload-ownership-tables)))
                  (when (hash-table-p table)
                    (funcall anvil-offload--remhash-primitive proc table)))
                (funcall anvil-offload--process-put-primitive proc 'anvil-offload-ownership-tables nil)
                (setq completed t)
                proc)
            (when (and (processp proc) (not completed))
              (anvil-offload--hard-delete-process proc))))))))

(defun anvil-offload--pick-worker ()
  "Return a live REPL from the pool via round-robin."
  (anvil-offload--ensure-pool-vector)
  (let ((n (length anvil-offload--pool)))
    (anvil-offload--ensure-slot (mod (cl-incf anvil-offload--round-robin) n))))

;;;###autoload
(defun anvil-offload-stop-repl ()
  "Transactionally stop every pooled, isolated, and staged subprocess.
Stable fallback custody snapshots and marks every child before the first
watched stop publication.  Every retired pool generation and every registered
auxiliary table remains reachable until all children are gone.  Any survivor
or signaling watcher leaves dispatch fail closed for an explicit retry."
  (interactive)
  (when (anvil-offload--submission-active-p)
    (error "anvil-offload: submission already active"))
  (when (anvil-offload--cleanup-active-p)
    (error "anvil-offload: pool cleanup already active"))
  (anvil-offload--with-cleanup-guard
    (let* ((pool anvil-offload--pool)
           (_pool-custody
            (anvil-offload--stage-retirement-pools
             (cons pool (copy-sequence anvil-offload--retired-pools))))
           (captured-auxiliary
            (anvil-offload--stage-stop-auxiliary-processes)))
      (setq anvil-offload--stop-retiring-p t
            anvil-offload--pool-retiring-p t)
      (let* ((pool-converged
              (anvil-offload--retire-captured-pool pool))
             (auxiliary
              (cl-delete-duplicates
               (append
                captured-auxiliary
                (anvil-offload--stage-stop-auxiliary-processes))
               :test #'eq)))
        (dolist (proc auxiliary)
          (anvil-offload--hard-delete-process proc))
        (when (and pool-converged
                   (eq pool anvil-offload--pool)
                   (null anvil-offload--retired-pools)
                   (null (anvil-offload--owned-auxiliary-processes))
                   (not (anvil-offload--fallback-stop-retirement-p)))
          ;; Publish the absent primary exactly once.  On Emacs 30 a watcher
          ;; signal prevents assignment, leaving both retiring flags intact.
          (setq anvil-offload--pool nil)
          (setq anvil-offload--pool-retiring-p nil)
          (setq anvil-offload--stop-retiring-p nil))))))

;;;###autoload
(defun anvil-offload-repl-alive-p ()
  "Non-nil when at least one pooled REPL is alive."
  (and anvil-offload--pool
       (cl-some (lambda (p) (and (processp p) (process-live-p p)))
                (append anvil-offload--pool nil))))

;;;###autoload
(defun anvil-offload-pool-status ()
  "Return a list of slot descriptors describing the pool.
Each element is `(:slot IDX :alive t-or-nil :pid PID-or-nil)'."
  (and anvil-offload--pool
       (cl-loop for i below (length anvil-offload--pool)
                for p = (aref anvil-offload--pool i)
                collect (list :slot i
                              :alive (and (processp p) (process-live-p p) t)
                              :pid (and (processp p) (process-live-p p)
                                        (process-id p))))))

;;; Public entry point

(defun anvil-offload--build-preamble (requires extra-load-path)
  "Build the preamble forms to run before the user FORM in the REPL.
REQUIRES is a feature symbol or list of symbols to (require \\='X).
EXTRA-LOAD-PATH is a list of directories to prepend to `load-path'
in the subprocess.  Returns a list of forms (possibly empty)."
  (let ((features (cond
                   ((null requires) nil)
                   ((symbolp requires) (list requires))
                   ((listp requires) requires)
                   (t (error "anvil-offload :require must be symbol or list, got %S"
                             requires)))))
    (append
     (and extra-load-path
          ;; `add-to-list' prepends, so emit the additions in reverse to
          ;; preserve the caller's left-to-right search precedence.
          (list `(dolist (d ',(reverse extra-load-path))
                   (add-to-list 'load-path d))))
     (mapcar (lambda (f) `(require ',f)) features))))

(defun anvil-offload--bind-context (form keys)
  "Wrap FORM in request-local dynamic bindings selected from KEYS."
  (let (bindings)
    (dolist (entry '((:process-environment . process-environment)
                     (:exec-path . exec-path)
                     (:default-directory . default-directory)
                     (:shell-file-name . shell-file-name)
                     (:shell-command-switch . shell-command-switch)
                     (:exact-load-path . load-path)))
      (when (plist-member keys (car entry))
        (push (list (cdr entry) (list 'quote (plist-get keys (car entry))))
              bindings)))
    (if bindings `(let ,(nreverse bindings) ,form) form)))

(defun anvil-offload--delete-aborted-submission (proc _ownership)
  "Best-effort compatibility wrapper for deferred exact PROC cleanup."
  (anvil-offload--delete-retained-process proc))

(defun anvil-offload--abort-submission (proc future id _ownership)
  "Best-effort cleanup for a nonlocally aborted public submission.
ID was captured before arbitrary callbacks.  Retire exact
PROC and silently remove pending FUTURE immediately.  Destructive cleanup
uses captured timer primitives on a later event-loop turn, so an arbitrary
scheduler cannot replace the original error, quit, or throw."
  (when (processp proc)
    (anvil-offload--mark-retiring proc))
  (when (and (anvil-future-p future)
             (anvil-offload--registered-pending-p future))
    (anvil-offload--terminalize-silently
     future id 'error
     "offload submission aborted" 'submission-aborted))
  (when (processp proc)
    (anvil-offload--defer-hard-delete-preserving-exit proc)))

;;;###autoload
(cl-defun anvil-offload (form &rest keys)
  "Evaluate FORM in an owned offload subprocess; return an `anvil-future'.

FORM is sent as a single S-expression.  The subprocess evaluates its printed
form via a base64 payload inside the request sexp and sends back either
`(:id N :ok VALUE)' or `(:id N :error MSG)'.  The main daemon never blocks.

Keyword arguments:
  :require FEATURES   Symbol or list of symbols to `require' in the
                      subprocess before FORM is evaluated.
  :load-path DIRS     List of directories prepended to `load-path' in the
                      subprocess (applied before :require).
  :isolated BOOL      Use a fresh one-shot subprocess owned only by this
                      future.
  :on-start FN        Called once with the future before evaluation.
  :on-settle FN       Called once with the terminal future.
  :process-environment, :exec-path, :default-directory,
  :shell-file-name, :shell-command-switch
                      Request-local bindings in the subprocess.
  :exact-load-path DIRS
                      Replace `load-path' exactly around FORM.  Isolated async
                      evaluation uses this to preserve request search
                      precedence; ordinary callers normally use :load-path.

Dispatch uses round-robin across the pool (`anvil-offload-pool-size')."
  (when (or (anvil-offload--cleanup-active-p)
            (anvil-offload--stop-incomplete-p)
            (anvil-offload--submission-active-p))
    (error
     "anvil-offload: cleanup already active or submission already active"))
  ;; The stable submission bit is the linearization point.  It is published
  ;; before the observable special binding and remains set through pending
  ;; registration and the successful transport send.
  (anvil-offload--with-submission-guard
    (let* ((requires (plist-get keys :require))
         (extra-load-path (plist-get keys :load-path))
         (preamble (anvil-offload--build-preamble requires extra-load-path))
         ;; Capture a registered table before allocation or arbitrary spawn
         ;; callbacks.  Abort cleanup can then establish exact custody without
         ;; assigning a watched global or depending on callback-mutated state.
         (submission-ownership (anvil-offload--ensure-isolated-processes))
         (id (cl-incf anvil-offload--next-id))
         (isolated (plist-get keys :isolated))
         (context-form (anvil-offload--bind-context form keys))
         (full-form
          (if preamble (cons 'progn (append preamble (list context-form)))
            context-form))
         proc
         future
         submitted)
    (unwind-protect
        (progn
          (let ((inhibit-quit t))
            (setq proc
                  (if isolated
                      (anvil-offload--spawn-isolated-process id)
                    (anvil-offload--pick-worker)))
            ;; Every selected child has durable auxiliary custody before the
            ;; pending table or transport can execute arbitrary advised code.
            (anvil-offload--retain-submission-process
             proc submission-ownership)
            (setq future
                  (make-anvil-future
                   :id id
                   :process proc
                   :status 'pending
                   :isolated-p isolated
                   :on-start (plist-get keys :on-start)
                   :on-settle (plist-get keys :on-settle)
                   :registration-p t
                   :registered-id id
                   :registered-process proc
                   :registered-status 'pending
                   :registered-isolated-p (and isolated t)))
            (puthash id future (anvil-offload--ensure-pending)))
          (process-send-string
           proc
           (concat
            (anvil-offload--print-readably
             (list :id id
                   :quit-after (and isolated t)
                   :payload
                   (anvil-offload--frame-encode-payload
                    (anvil-offload--print-readably full-form))))
            "\n"))
          (setq submitted t)
          future)
      ;; Unwind-protect covers errors, quits, and arbitrary tagged nonlocal
      ;; exits from spawn callbacks, pending publication, or transport advice.
      (unless submitted
        (anvil-offload--abort-submission
         proc future id submission-ownership))))))

;;;###autoload
(cl-defun anvil-offload-isolated (form &rest keys)
  "Evaluate FORM in a fresh one-shot subprocess and return its future."
  (apply #'anvil-offload form :isolated t keys))

;;; Module enable / disable (for `anvil-enable' integration)

;;;###autoload
(defun anvil-offload-enable ()
  "Enable the anvil-offload module.

Does *not* spawn the REPL — spawning is lazy on first
`anvil-offload' call.  Kept as a no-op so module registration
stays uniform across anvil modules."
  (interactive)
  t)

;;;###autoload
(defun anvil-offload-disable ()
  "Disable the anvil-offload module — stops the REPL if running."
  (interactive)
  (anvil-offload-stop-repl))

(provide 'anvil-offload)
;;; anvil-offload.el ends here
