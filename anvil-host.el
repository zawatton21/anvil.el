;;; anvil-host.el --- OS and shell abstraction for anvil -*- lexical-binding: t; -*-

;; Author: zawatton
;; Keywords: tools, mcp, claude, system

;;; Commentary:

;; anvil-helpers.el の姉妹ファイル。Claude Code が emacs-eval
;; (mcp__org-mcp__emacs-eval) 経由で呼び出すための host inspection
;; helper 群。
;;
;; 目的:
;;   1. Emacs server が動いているホスト PC (= 実機 = VM 環境外) の
;;      仕様を OS 非依存 API で取得する
;;   2. OS ごとのコマンド差を helper 内に閉じ込め、Claude のトークン
;;      消費と判断コストを削減
;;   3. wmic / sysctl / /proc 等の冗長な出力を構造化 plist に整形
;;
;; 構成 (2 layer):
;;
;;   Layer 1 — 汎用 shell ランナー
;;     `anvil-shell'        任意 shell コマンド (timeout/coding/truncate 付)
;;     `anvil-shell-by-os'  :windows/:darwin/:linux 分岐版
;;
;;   Layer 2 — 構造化済み host 情報
;;     `anvil-host-info'    カテゴリ指定で plist を返す
;;       (os cpu ram disk gpu network uptime emacs)
;;
;; 戻り値: plist。失敗時は (error "anvil-host: ...") を投げる。
;;
;; API リファレンス: ~/Cowork/Notes/.claude/anvil-helpers-reference.org

;;; Code:

(require 'cl-lib)
(require 'subr-x)
;; Doc 50 — load the user config loader.  Under regular Emacs this is
;; a cheap no-op; under the NeLisp standalone path (= `bin/anvil mcp
;; serve' via the Rust anvil-runtime binary) it resolves
;; `$ANVIL_CONFIG_DIR/config.el' so defcustom overrides land before
;; any other anvil module's init code runs.  See anvil-config.el for
;; the detection contract.
(require 'anvil-config)

(defun anvil-host--capture-primitive (symbol)
  "Return SYMBOL's underlying function, stripping already-installed advice."
  (let ((function (symbol-function symbol)))
    (if (fboundp 'advice--cd*r)
        (advice--cd*r function)
      function)))

(defconst anvil-host--process-list-primitive (anvil-host--capture-primitive 'process-list)
  "Unadvised process identity snapshot primitive for constructor custody.")

(defconst anvil-host--make-process-primitive (anvil-host--capture-primitive 'make-process)
  "Unadvised subprocess constructor captured before runtime configuration.")

(defconst anvil-host--make-pipe-process-primitive
  (anvil-host--capture-primitive 'make-pipe-process)
  "Unadvised pipe constructor captured before runtime configuration.")

(defconst anvil-host--process-send-eof-primitive
  (anvil-host--capture-primitive 'process-send-eof)
  "Unadvised stdin close primitive for private child execution.")

(defconst anvil-host--accept-process-output-primitive
  (anvil-host--capture-primitive 'accept-process-output)
  "Unadvised targeted wait primitive for private child execution.")

(defconst anvil-host--run-at-time-function
  (anvil-host--capture-primitive 'run-at-time)
  "Unadvised one-shot scheduler captured before runtime configuration.")

(defconst anvil-host--process-filter-primitive
  (anvil-host--capture-primitive 'process-filter)
  "Unadvised process filter reader for private-output validation.")

(defconst anvil-host--process-name-primitive (anvil-host--capture-primitive 'process-name)
  "Unadvised process name reader for constructor recovery.")

(defconst anvil-host--set-process-buffer-primitive
  (anvil-host--capture-primitive 'set-process-buffer)
  "Unadvised buffer detach primitive for output secrecy.")

;;;; --- defaults / constants -----------------------------------------------

(defconst anvil-host--default-timeout 30
  "Default timeout (seconds) for `anvil-shell'.")

(defconst anvil-host--default-max-output 16384
  "Default max output bytes captured per stream by `anvil-shell'.")

(defconst anvil-host--stderr-drain-budget-sec 0.2
  "Seconds to drain the stderr pipe-proc after the main proc exits.
Captures late stderr bytes that the kernel pipe still held when
the shell child died.  Capped so a disowned descendant which
inherits fd 2 (e.g. `setsid --fork') cannot extend
`anvil-shell' beyond this budget.")

(defvar anvil-host-cleanup-timeout 1.0
  "Seconds allowed to confirm that an interrupted host child is dead.")

(defvar anvil-host--cleanup-state (cons nil nil)
  "Stable host-child cleanup state.
The car is the unwatchable transaction-active bit.  The cdr lazily becomes
a vector containing the hash-table mirror, callback-free staging list,
authoritative one-shot timer, and submission-active bit.  Minimal Elisp
runtimes can therefore load this file without constructing unsupported
process-custody objects.")

(defvar anvil-host--cleanup-timer nil
  "Timer retrying exact host resources whose cleanup has not converged.")

(defvar anvil-host--cleanup-active nil
  "Observable dynamic cleanup state.
The stable car of anvil-host--cleanup-state, not this watchable variable,
is authoritative for reentrancy exclusion.")

(defvar anvil-host-child-process-environment nil
  "Optional process environment used only while creating a shell child.")

(defvar anvil-host-child-exec-path nil
  "Optional executable path used only while creating a shell child.")

(defvar anvil-host-child-shell-file-name nil
  "Optional shell executable used only while creating a shell child.")

(defvar anvil-host-child-shell-command-switch nil
  "Optional shell command switch used only while creating a shell child.")

(defun anvil-host--default-coding ()
  "Return the default coding system for shell I/O on this OS.
Windows defaults to cp932-dos because wmic / systeminfo emit Shift_JIS
on Japanese Windows. Other OSes default to utf-8."
  (if (eq system-type 'windows-nt) 'cp932-dos 'utf-8))

;;;; --- internal: shell run ------------------------------------------------

(defun anvil-host--truncate (str max &optional total-bytes)
  "Append a truncation marker to bounded STR when TOTAL-BYTES exceeds MAX.
Callers must already have limited the captured unibyte stream to MAX bytes.
When TOTAL-BYTES is nil, derive it from STR for compatibility."
  (let ((total (or total-bytes (string-bytes str))))
    (if (and (numberp max) (> total max))
        (concat str
                (format "\n...[anvil-host: truncated, %d more bytes]"
                        (- total max)))
      str)))

(defun anvil-host--scheduled-timers ()
  "Return a snapshot of every normal and idle timer identity."
  (append (copy-sequence timer-list)
          (copy-sequence timer-idle-list)))

(defun anvil-host--timer-scheduled-p (timer)
  "Return non-nil when TIMER is scheduled normally or while Emacs is idle."
  (and (timerp timer)
       (or (memq timer timer-list)
           (memq timer timer-idle-list))))

(defun anvil-host--resource-state ()
  "Return the stable vector for table, staging, timer, and submission."
  (let* ((tail (cdr anvil-host--cleanup-state))
         (legacy-timer
          (and (timerp anvil-host--cleanup-timer)
               (anvil-host--timer-scheduled-p anvil-host--cleanup-timer)
               anvil-host--cleanup-timer))
         resources)
    (setq resources
          (cond
           ((null tail)
            (vector nil nil legacy-timer nil))
           ((hash-table-p tail)
            (vector tail nil legacy-timer nil))
           ((and (vectorp tail) (= (length tail) 4)) tail)
           ((and (vectorp tail) (= (length tail) 3))
            (vector (aref tail 0) (aref tail 1) (aref tail 2) nil))
           ((consp tail)
            (vector (car tail) (cdr tail) legacy-timer nil))
           (t (error "anvil-host: invalid cleanup state"))))
    (unless (eq tail resources)
      (setcdr anvil-host--cleanup-state resources))
    ;; Reconcile a scheduled legacy mirror after partial live reloads too.
    (when (and legacy-timer
               (let ((current (aref resources 2)))
                 (not (anvil-host--timer-scheduled-p current))))
      (aset resources 2 legacy-timer))
    resources))

(defun anvil-host--retired-table ()
  "Return the lazily initialized retirement-table mirror."
  (let* ((resources (anvil-host--resource-state))
         (table (aref resources 0)))
    (or table
        (progn
          (setq table (make-hash-table :test #'eq))
          (aset resources 0 table)
          table))))

(defun anvil-host--retired-entries ()
  "Return a snapshot of every uniquely owned retirement entry."
  (let* ((resources (anvil-host--resource-state))
         (table (aref resources 0))
         (entries (copy-sequence (aref resources 1))))
    (when (hash-table-p table)
      (maphash
       (lambda (owner metadata)
         (unless (assq owner entries)
           (push (cons owner metadata) entries)))
       table))
    entries))

(defun anvil-host--retired-count ()
  "Return the number of uniquely owned host resource records."
  (length (anvil-host--retired-entries)))

(defun anvil-host--retired-empty-p ()
  "Return non-nil when no host resource remains in global custody."
  (null (anvil-host--retired-entries)))

(defun anvil-host--owned-metadata-p (owner metadata)
  "Return non-nil when OWNER still names exact METADATA globally."
  (let* ((resources (anvil-host--resource-state))
         (entry (assq owner (aref resources 1)))
         (table (aref resources 0)))
    (or (eq (cdr-safe entry) metadata)
        (and (hash-table-p table)
             (eq (gethash owner table) metadata)))))

(defun anvil-host--stage-metadata (metadata)
  "Give METADATA callback-free global custody and return its entry."
  (let* ((resources (anvil-host--resource-state))
         (owner (make-symbol "anvil-host-resource-custody"))
         (entry (cons owner metadata)))
    ;; Mutating the stable resource vector invokes neither variable watchers
    ;; nor hash-table advice.  Publication therefore precedes every
    ;; observable binding and destructive callback.
    (aset resources 1 (cons entry (aref resources 1)))
    entry))

(defun anvil-host--cleanup-active-p ()
  "Return non-nil during an exact-resource cleanup transaction."
  (car anvil-host--cleanup-state))

(defun anvil-host--terminate-exact-process (process)
  "Try to terminate exact PROCESS and return non-nil only after its death."
  (if (not (processp process))
      t
    (let ((deadline (+ (float-time) anvil-host-cleanup-timeout))
          (inhibit-quit t))
      (when (process-live-p process)
        (condition-case nil
            (kill-process process)
          ((error quit) nil)))
      (while (and (process-live-p process)
                  (< (float-time) deadline))
        ;; The integer JUST-THIS-ONE value prioritizes this exact process and
        ;; suppresses ordinary timers.  Emacs may still dispatch an already-
        ;; ready foreign process filter.
        (funcall anvil-host--accept-process-output-primitive process 0.02 nil 1))
      (condition-case nil
          (delete-process process)
        ((error quit) nil))
      (while (and (process-live-p process)
                  (< (float-time) deadline))
        (funcall anvil-host--accept-process-output-primitive process 0.02 nil 1))
      (and (not (process-live-p process))
           (not (memq process
                      (funcall anvil-host--process-list-primitive)))))))

(defun anvil-host--close-exact-pipe (process)
  "Close exact pipe PROCESS without waiting for an inherited descriptor."
  (if (not (processp process))
      t
    (let ((inhibit-quit t))
      (condition-case nil
          (delete-process process)
        ((error quit) nil))
      (and (not (process-live-p process))
           (not (memq process
                      (funcall anvil-host--process-list-primitive)))))))

(defun anvil-host--release-process-resources (owner metadata)
  "Release OWNER only after every exact METADATA resource has converged.
The staged entry is removed last, so hash callbacks and tagged exits retain
global custody for the retry timer."
  (let* ((resources (anvil-host--resource-state))
         (table (aref resources 0))
         (process (plist-get metadata :process))
         (primary-released-p
          (or (not (processp process))
              (and (not (process-live-p process))
                   (not (memq
                         process
                         (funcall anvil-host--process-list-primitive)))))))
    (when (and (anvil-host--owned-metadata-p owner metadata)
               primary-released-p)
      (let* ((stderr-process (plist-get metadata :stderr-process))
             (custody-buffer (plist-get metadata :custody-buffer))
             (stderr-custody-buffer
              (plist-get metadata :stderr-custody-buffer))
             (inhibit-quit t)
             (stderr-released-p
              (or (not (processp stderr-process))
                  (anvil-host--close-exact-pipe stderr-process))))
        (when (and stderr-released-p
                   (buffer-live-p custody-buffer))
          (condition-case nil
              (kill-buffer custody-buffer)
            ((error quit) nil)))
        (when (and stderr-released-p
                   (buffer-live-p stderr-custody-buffer))
          (condition-case nil
              (kill-buffer stderr-custody-buffer)
            ((error quit) nil)))
        (when (and primary-released-p
                   stderr-released-p
                   (not (buffer-live-p custody-buffer))
                   (not (buffer-live-p stderr-custody-buffer))
                   (anvil-host--owned-metadata-p owner metadata))
          ;; Remove the advised hash mirror before the callback-free stage.
          ;; An exit after real `remhash' still leaves the staged entry.
          (when (and (hash-table-p table)
                     (eq (gethash owner table) metadata))
            (remhash owner table))
          (aset resources 1
                (assq-delete-all owner (aref resources 1)))
          t)))))

(defun anvil-host--cleanup-owned-entry (entry)
  "Try to converge the exact resource retirement ENTRY."
  (let ((owner (car entry))
        (metadata (cdr entry)))
    (when (anvil-host--owned-metadata-p owner metadata)
      (let ((process (plist-get metadata :process)))
        (when (anvil-host--terminate-exact-process process)
          (anvil-host--release-process-resources owner metadata))))))

(defun anvil-host--retry-owned-entries ()
  "Converge a snapshot of retained entries under an acquired cleanup guard."
  (let ((entries (anvil-host--retired-entries)))
    (let ((anvil-host-child-process-environment nil)
          (anvil-host-child-exec-path nil)
          (anvil-host-child-shell-file-name nil)
          (anvil-host-child-shell-command-switch nil))
      (dolist (entry entries)
        (let* ((metadata (cdr entry))
               (custody-buffer (plist-get metadata :custody-buffer))
               (context-buffer
                (if (buffer-live-p custody-buffer)
                    custody-buffer
                  (plist-get metadata :stderr-custody-buffer))))
          (if (buffer-live-p context-buffer)
              (with-current-buffer context-buffer
                (anvil-host--cleanup-owned-entry entry))
            (anvil-host--cleanup-owned-entry entry)))))
    (when (anvil-host--retired-empty-p)
      (anvil-host--cancel-cleanup-timer))))

(defun anvil-host--retry-retired-processes ()
  "Retry every exact host resource retained after failed cleanup."
  (unless (anvil-host--cleanup-active-p)
    (setcar anvil-host--cleanup-state t)
    (unwind-protect
        (let ((anvil-host--cleanup-active t))
          (anvil-host--retry-owned-entries))
      (unwind-protect
          (unless (anvil-host--retired-empty-p)
            (anvil-host--ensure-cleanup-timer))
        (setcar anvil-host--cleanup-state nil)))))

(defun anvil-host--cleanup-timer-scheduled-p (&optional timer)
  "Return non-nil when TIMER, or the authoritative timer, is scheduled."
  (let ((candidate
         (or timer (aref (anvil-host--resource-state) 2))))
    (anvil-host--timer-scheduled-p candidate)))

(defun anvil-host--cancel-cleanup-timer ()
  "Cancel and forget the exact authoritative cleanup timer."
  (let* ((resources (anvil-host--resource-state))
         (timer (aref resources 2)))
    ;; Clear the callback-free identity before cancel-timer or mirror watchers
    ;; can reenter.  A one-shot timer which escapes cancellation expires.
    (when (eq (aref resources 2) timer)
      (aset resources 2 nil))
    (when (anvil-host--cleanup-timer-scheduled-p timer)
      (cancel-timer timer))
    (when (or (eq anvil-host--cleanup-timer timer)
              (and (timerp anvil-host--cleanup-timer)
                   (not (anvil-host--timer-scheduled-p
                         anvil-host--cleanup-timer))))
      (condition-case nil
          (setq anvil-host--cleanup-timer nil)
        ((error quit) nil)))))

(defun anvil-host--cleanup-timer-fired (timer)
  "Handle exact one-shot cleanup TIMER under the stable cleanup guard."
  (let* ((state anvil-host--cleanup-state)
         (outer-active (car state))
         (inhibit-quit t))
    (unless outer-active
      (setcar state t))
    (unwind-protect
        (if outer-active
            (let ((resources (anvil-host--resource-state)))
              (when (eq (aref resources 2) timer)
                (aset resources 2 nil))
              (when (and (timerp timer)
                         (eq anvil-host--cleanup-timer timer))
                (condition-case nil
                    (setq anvil-host--cleanup-timer nil)
                  ((error quit) nil))))
          (let ((anvil-host--cleanup-active t)
                (resources (anvil-host--resource-state)))
            (unwind-protect
                (progn
                  (when (eq (aref resources 2) timer)
                    (aset resources 2 nil))
                  (when (and (timerp timer)
                             (eq anvil-host--cleanup-timer timer))
                    (condition-case nil
                        (setq anvil-host--cleanup-timer nil)
                      ((error quit) nil))))
              ;; Watcher errors and tagged exits cannot skip convergence.
              (anvil-host--retry-owned-entries))))
      (unless outer-active
        (unwind-protect
            (unless (anvil-host--retired-empty-p)
              (anvil-host--ensure-cleanup-timer))
          (setcar state nil))))))

(defun anvil-host--discard-cleanup-timer (timer)
  "Cancel exact constructor TIMER without allowing it to repeat."
  (when (timerp timer)
    (condition-case nil
        (progn
          (setf (timer--repeat-delay timer) nil)
          (when (anvil-host--timer-scheduled-p timer)
            (cancel-timer timer)))
      ((error quit) nil))))

(defun anvil-host--normalize-cleanup-timer (timer callback)
  "Make exact TIMER a fresh one-shot invoking CALLBACK with no arguments."
  (unless (timerp timer)
    (error "anvil-host: cleanup timer constructor returned no timer"))
  (when (anvil-host--timer-scheduled-p timer)
    (cancel-timer timer))
  (timer-set-function timer callback nil)
  (timer-set-time timer (timer-relative-time nil 1) nil)
  (timer-activate timer)
  timer)

(defun anvil-host--make-direct-cleanup-timer (callback)
  "Create and activate a fallback one-shot for CALLBACK."
  (anvil-host--normalize-cleanup-timer (timer-create) callback))

(defun anvil-host--select-cleanup-timer
    (returned observed before callback)
  "Return one exact normalized cleanup timer for this constructor.
RETURNED and OBSERVED are accepted only when absent from BEFORE.  A timer with
the unique CALLBACK identity is preferred, but a new returned wrapper is also
constructor-owned.  Duplicate exact-callback timers are canceled while
non-callback helper timers are preserved.  When advice creates no recoverable
timer, construct a direct fallback."
  (let (candidates)
    (dolist (candidate
             (append (anvil-host--scheduled-timers)
                     (list returned observed)))
      (when
          (and
           (timerp candidate)
           (not (memq candidate before))
           (or
            (eq candidate returned)
            (eq candidate observed)
            (eq (timer--function candidate) callback)))
        (cl-pushnew candidate candidates :test #'eq)))
    (let ((selected
           (or
            ;; When several exact callback timers were created, the
            ;; constructor's returned identity is authoritative.
            (and (memq returned candidates)
                 (eq (timer--function returned) callback)
                 returned)
            (and (memq observed candidates)
                 (eq (timer--function observed) callback)
                 observed)
            (cl-find-if
             (lambda (candidate)
               (eq (timer--function candidate) callback))
             candidates)
            (and (memq returned candidates) returned)
            (and (memq observed candidates) observed)
            (car candidates))))
      (if selected
          (progn
            (dolist (candidate candidates)
              (when (and (not (eq candidate selected))
                         (eq (timer--function candidate) callback))
                (anvil-host--discard-cleanup-timer candidate)))
            (anvil-host--normalize-cleanup-timer selected callback))
        (anvil-host--make-direct-cleanup-timer callback)))))

(defun anvil-host--ensure-cleanup-timer ()
  "Ensure failed host-child cleanup has an exact one-shot retry timer."
  (let* ((resources (anvil-host--resource-state))
         (current (aref resources 2)))
    (unless (anvil-host--cleanup-timer-scheduled-p current)
      (let ((inhibit-quit t)
            (before (anvil-host--scheduled-timers))
            callback returned timer published-p)
        (setq callback
              (lambda (&rest _ignored)
                (let ((fired
                       (or
                        (and
                         (boundp 'timer-event-last)
                         (timerp timer-event-last)
                         (not (memq timer-event-last before))
                         (eq (timer--function timer-event-last) callback)
                         timer-event-last)
                        (and (timerp timer) timer))))
                  (when (timerp fired)
                    (setq timer fired))
                  (if (anvil-host--cleanup-active-p)
                      ;; Constructor advice may yield past the deadline before
                      ;; returning.  Retain and rearm the exact executing
                      ;; one-shot so the active transaction cannot lose retry.
                      (when (timerp fired)
                        (aset (anvil-host--resource-state) 2 fired)
                        (condition-case nil
                            (anvil-host--normalize-cleanup-timer
                             fired callback)
                          ((error quit) nil)))
                    (anvil-host--cleanup-timer-fired fired)))))
        (unwind-protect
            (progn
              (setq returned
                    (funcall anvil-host--run-at-time-function
                             1 nil callback)
                    timer
                    (anvil-host--select-cleanup-timer
                     returned timer before callback))
              ;; The stable identity is authoritative and precedes the
              ;; watchable compatibility mirror.
              (aset resources 2 timer)
              (setq published-p t)
              (setq anvil-host--cleanup-timer timer))
          ;; Constructor advice may leave nonlocally after creating the timer.
          ;; Recover its exact callback identity without invoking watchers or
          ;; replacing the original exit.
          (unless published-p
            (let ((recovered
                   (condition-case nil
                       (anvil-host--select-cleanup-timer
                        returned timer before callback)
                     ((error quit) nil))))
              (when recovered
                (setq timer recovered)
                (aset resources 2 timer)
                (setq published-p t)))))))))

(defun anvil-host--retire-resources (metadata-list)
  "Publish and converge every exact resource record in METADATA-LIST."
  (when metadata-list
    (let* ((state anvil-host--cleanup-state)
           (outer-active (car state))
           (inhibit-quit t))
      ;; Acquire the stable guard before any observable binding or advised
      ;; table operation.  Stage every record before deleting the first.
      (unless outer-active
        (setcar state t))
      (unwind-protect
          (let ((entries
                 (mapcar #'anvil-host--stage-metadata metadata-list)))
            (if outer-active
                ;; A destructive callback reentered retirement.  Publish its
                ;; exact records, but let the outer transaction or timer
                ;; converge them without recursive deletion.
                (let ((table (anvil-host--retired-table)))
                  (dolist (entry entries)
                    (puthash (car entry) (cdr entry) table)))
              ;; Entries are globally staged before this watched binding.
              (let ((anvil-host--cleanup-active t)
                    (table (anvil-host--retired-table)))
                (dolist (entry entries)
                  (puthash (car entry) (cdr entry) table))
                (dolist (entry entries)
                  (anvil-host--cleanup-owned-entry entry)))))
        ;; This unwind encloses staging, the watched binding, hash
        ;; publication, and every destructive callback.
        (unwind-protect
            (unless (anvil-host--retired-empty-p)
              (anvil-host--ensure-cleanup-timer))
          (unless outer-active
            (setcar state nil)))))))

(defun anvil-host--retire-process
    (process stderr-process custody-buffer &optional stderr-custody-buffer)
  "Retain and terminate exact PROCESS and its auxiliary resources.
STDERR-PROCESS is retained beside PROCESS.  CUSTODY-BUFFER and
STDERR-CUSTODY-BUFFER support callers that own temporary buffers.
Callback-free staging precedes observable bindings and destructive callbacks."
  (let* ((primary (and (processp process) process))
         (auxiliary (and (processp stderr-process) stderr-process))
         (has-resources
          (or primary auxiliary
              (buffer-live-p custody-buffer)
              (buffer-live-p stderr-custody-buffer))))
    (when has-resources
      (anvil-host--retire-resources
       (list
        (list :process primary
              :stderr-process auxiliary
              :custody-buffer custody-buffer
              :stderr-custody-buffer stderr-custody-buffer))))))

(defun anvil-host--unique-process-name (prefix)
  "Return a process name beginning with PREFIX that is unused in this Emacs."
  (let (name)
    (while
        (progn
          (setq name (symbol-name (gensym prefix)))
          (get-process name)))
    name))

(defun anvil-host--process-matches-constructor-p (process name filter)
  "Return non-nil when PROCESS retains constructor NAME or FILTER identity."
  (and (processp process)
       (or
        (string-match-p
         (concat "\\`" (regexp-quote name) "\\(?:<[0-9]+>\\)?\\'")
         (funcall anvil-host--process-name-primitive process))
        (and filter
             (eq (funcall anvil-host--process-filter-primitive process)
                 filter)))))

(defun anvil-host--new-processes-in-name-family
    (name preexisting &optional filter)
  "Return new transaction processes for NAME, PREEXISTING, and FILTER."
  (cl-remove-if-not
   (lambda (process)
     (and (not (memq process preexisting))
          (anvil-host--process-matches-constructor-p
           process name filter)))
   (funcall anvil-host--process-list-primitive)))

(defun anvil-host--new-processes-since (snapshot)
  "Return process identities created after SNAPSHOT."
  (cl-remove-if
   (lambda (process) (memq process snapshot))
   (funcall anvil-host--process-list-primitive)))

(defun anvil-host--transaction-processes
    (returned name filter constructor-started-p constructor-before
              constructor-complete-p constructor-invalid-p
              constructor-processes)
  "Return exact children created by one constructor transaction.
A full constructor delta is owned only after nonlocal exit or invalid return;
a successful valid constructor may create unrelated helper processes."
  (let ((processes
         (and constructor-invalid-p
              (copy-sequence constructor-processes))))
    (when (and constructor-started-p (not constructor-complete-p))
      (setq processes
            (anvil-host--new-processes-since constructor-before)))
    (dolist (process
             (anvil-host--new-processes-in-name-family
              name constructor-before filter))
      (cl-pushnew process processes :test #'eq))
    (when (and (processp returned)
               (not (memq returned constructor-before)))
      (cl-pushnew returned processes :test #'eq))
    processes))

(defun anvil-host--candidate-metadata (processes stderr-processes)
  "Return retirement records for exact PROCESSES and STDERR-PROCESSES."
  (if (and (= (length processes) 1)
           (= (length stderr-processes) 1))
      (list
       (list :process (car processes)
             :stderr-process (car stderr-processes)
             :custody-buffer nil
             :stderr-custody-buffer nil))
    (append
     (mapcar
      (lambda (process)
        (list :process process :stderr-process nil
              :custody-buffer nil :stderr-custody-buffer nil))
      processes)
     (mapcar
      (lambda (process)
        (list :process nil :stderr-process process
              :custody-buffer nil :stderr-custody-buffer nil))
      stderr-processes))))

(defun anvil-host--scrub-code-conversion-work-buffer ()
  "Erase Emacs's reusable coding scratch buffer after private decoding."
  (when-let ((buffer (get-buffer " *code-conversion-work*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (set-buffer-modified-p nil)))))

(defun anvil-host--decode-output (bytes coding)
  "Decode private unibyte BYTES with CODING, then scrub coding scratch."
  (unwind-protect
      (decode-coding-string bytes coding)
    (anvil-host--scrub-code-conversion-work-buffer)))

(defun anvil-host--detach-constructor-processes (processes)
  "Detach exact constructor PROCESSES from every discoverable output buffer."
  (dolist (process (cl-delete-duplicates processes :test #'eq))
    (when (processp process)
      (funcall anvil-host--set-process-buffer-primitive process nil))))

(defun anvil-host--run (command coding cwd timeout &optional max-output)
  "Run one non-reentrant host-shell submission transaction.
MAX-OUTPUT bounds captured bytes per output stream; nil means unbounded."
  (let* ((resources (anvil-host--resource-state))
         (outer-inhibit-quit inhibit-quit))
    (when (aref resources 3)
      (error "anvil-host: submission already active"))
    ;; Establish the callback-free guard before restoring caller quit policy.
    (let ((inhibit-quit t))
      (aset resources 3 t)
      (unwind-protect
          (let ((inhibit-quit outer-inhibit-quit))
            (anvil-host--run-transaction
             command coding cwd timeout max-output))
        (aset resources 3 nil)))))

(defun anvil-host--run-transaction (command coding cwd timeout max-output)
  "Run shell COMMAND using CODING in CWD, waiting up to TIMEOUT seconds.
Return (EXIT STDOUT STDERR STDOUT-TOTAL STDERR-TOTAL
        STDOUT-SOURCE STDERR-SOURCE), where the final values are exact
captured unibyte source strings.  Signal an error on timeout.

The shell and both output streams use pipes.  This API has no stdin
channel, so the process-input pipe is closed immediately after spawn.
A detached descendant may retain stdout or stderr; bounded waiting and
post-exit draining prevent inherited descriptors from retaining the call.

Output is accumulated in lexical filter state rather than discoverable
buffers.  Request-specific child bindings are present only during spawn.
Waits target this shell for return semantics while servicing all process
output, so Emacs server sockets and helper filters remain responsive.
Secret-bearing child overrides are shadowed during every yield; hard
containment therefore belongs to the dedicated-daemon bridge watchdog.
Nonlocal exits recover every exact post-snapshot constructor child."
  (anvil-host--resource-state)
  ;; Public submission is fail-closed across cleanup snapshots.
  (when (anvil-host--cleanup-active-p)
    (error "anvil-host: cleanup already active"))
  (unless (anvil-host--retired-empty-p)
    (anvil-host--retry-retired-processes))
  (when (or (anvil-host--cleanup-active-p)
            (not (anvil-host--retired-empty-p)))
    (error "anvil-host: retained cleanup has not converged"))
  ;; Cold table construction still precedes every process constructor.
  (anvil-host--retired-table)
  (let* ((shell-process-name
          (anvil-host--unique-process-name "anvil-host-shell-"))
         (stderr-process-name
          (anvil-host--unique-process-name "anvil-host-stderr-"))
         (child-directory (or cwd default-directory))
         (child-process-environment
          (copy-sequence
           (or anvil-host-child-process-environment process-environment)))
         (child-exec-path
          (copy-sequence (or anvil-host-child-exec-path exec-path)))
         (child-shell
          (or anvil-host-child-shell-file-name shell-file-name))
         (child-switch
          (or anvil-host-child-shell-command-switch shell-command-switch))
         stdout-chunks
         stderr-chunks
         (stdout-captured-bytes 0)
         (stderr-captured-bytes 0)
         (stdout-total-bytes 0)
         (stderr-total-bytes 0)
         (stdout-filter
          (lambda (_process chunk)
            (unwind-protect
                (let* ((size (string-bytes chunk))
                       (remaining
                        (and max-output
                             (max 0 (- max-output
                                       stdout-captured-bytes))))
                       (take (if remaining (min size remaining) size)))
                  (setq stdout-total-bytes
                        (+ stdout-total-bytes size))
                  (when (> take 0)
                    ;; Binary process coding makes CHUNK unibyte, so string
                    ;; indices and byte counts are identical here.
                    (push (if (= take size)
                              chunk
                            (substring chunk 0 take))
                          stdout-chunks)
                    (setq stdout-captured-bytes
                          (+ stdout-captured-bytes take))))
              (anvil-host--scrub-code-conversion-work-buffer))))
         (stderr-filter
          (lambda (_process chunk)
            (unwind-protect
                (let* ((size (string-bytes chunk))
                       (remaining
                        (and max-output
                             (max 0 (- max-output
                                       stderr-captured-bytes))))
                       (take (if remaining (min size remaining) size)))
                  (setq stderr-total-bytes
                        (+ stderr-total-bytes size))
                  (when (> take 0)
                    (push (if (= take size)
                              chunk
                            (substring chunk 0 take))
                          stderr-chunks)
                    (setq stderr-captured-bytes
                          (+ stderr-captured-bytes take))))
              (anvil-host--scrub-code-conversion-work-buffer))))
         stderr-constructor-started-p
         stderr-constructor-before
         stderr-constructor-processes
         stderr-constructor-complete-p
         stderr-constructor-invalid-p
         main-constructor-started-p
         main-constructor-before
         main-constructor-processes
         main-constructor-complete-p
         main-constructor-invalid-p
         proc
         stderr-proc)
    (unwind-protect
        (progn
          (setq stderr-constructor-before
                (funcall anvil-host--process-list-primitive)
                stderr-constructor-started-p t)
          (setq stderr-proc
                (funcall anvil-host--make-pipe-process-primitive
                 :name stderr-process-name
                 ;; Unlike `make-process', a nil pipe buffer creates a buffer
                 ;; named after the process.  Use an existing anchor while the
                 ;; lexical filter is installed, then detach it.
                 :buffer (current-buffer)
                 :coding 'binary
                 :noquery t
                 :sentinel #'ignore
                 :filter stderr-filter))
          (setq stderr-constructor-processes
                (anvil-host--new-processes-since
                 stderr-constructor-before)
                stderr-constructor-complete-p t)
          (let ((valid
                 (and
                  (processp stderr-proc)
                  (not (memq stderr-proc stderr-constructor-before))
                  (eq
                   (funcall
                    anvil-host--process-filter-primitive stderr-proc)
                   stderr-filter))))
            (setq stderr-constructor-invalid-p (not valid))
            (anvil-host--detach-constructor-processes
             (if valid
                 (cl-remove-if-not
                  (lambda (candidate)
                    (anvil-host--process-matches-constructor-p
                     candidate stderr-process-name stderr-filter))
                  stderr-constructor-processes)
               stderr-constructor-processes))
            (unless valid
              (if (memq stderr-proc stderr-constructor-before)
                  (error
                   "anvil-host: pipe constructor returned a preexisting process")
                (error
                 "anvil-host: pipe constructor returned an invalid process"))))
          ;; Dedicated backends may supply immutable child bindings.  Their
          ;; dynamic scope ends immediately after process creation.
          (let ((default-directory child-directory)
                (process-environment child-process-environment)
                (exec-path child-exec-path)
                (shell-file-name child-shell)
                (shell-command-switch child-switch))
            (setq main-constructor-before
                  (funcall anvil-host--process-list-primitive)
                  main-constructor-started-p t)
            (setq proc
                  (funcall anvil-host--make-process-primitive
                   :name shell-process-name
                   :buffer nil
                   :stderr stderr-proc
                   :filter stdout-filter
                   :coding '(binary . binary)
                   :noquery t
                   :connection-type 'pipe
                   :sentinel #'ignore
                   :command (list child-shell child-switch command)))
            (setq main-constructor-processes
                  (anvil-host--new-processes-since
                   main-constructor-before)
                  main-constructor-complete-p t))
          (let ((valid
                 (and
                  (processp proc)
                  (not (memq proc main-constructor-before))
                  (eq
                   (funcall anvil-host--process-filter-primitive proc)
                   stdout-filter))))
            (setq main-constructor-invalid-p (not valid))
            (anvil-host--detach-constructor-processes
             (if valid
                 (cl-remove-if-not
                  (lambda (candidate)
                    (anvil-host--process-matches-constructor-p
                     candidate shell-process-name stdout-filter))
                  main-constructor-processes)
               main-constructor-processes))
            (unless valid
              (cond
               ((not (processp proc))
                (error "anvil-host: shell spawn returned no process"))
               ((memq proc main-constructor-before)
                (error
                 "anvil-host: shell constructor returned a preexisting process"))
               (t
                (error
                 "anvil-host: shell constructor returned an invalid process")))))
          ;; Commands that read non-TTY stdin must see EOF instead of retaining
          ;; the root event loop until the tool timeout.
          (condition-case error
              (funcall anvil-host--process-send-eof-primitive proc)
            (error
             (when (process-live-p proc)
               (signal (car error) (cdr error)))))
          ;; Shadow secret-bearing outer advice bindings during every yield.
          ;; Accept foreign process output as well: Emacs server sockets and
          ;; helper filters must remain responsive while the child is running.
          (let ((anvil-host-child-process-environment nil)
                (anvil-host-child-exec-path nil)
                (anvil-host-child-shell-file-name nil)
                (anvil-host-child-shell-command-switch nil))
            (let ((deadline (+ (float-time) timeout)))
              (while (and (process-live-p proc)
                          (< (float-time) deadline))
                (funcall anvil-host--accept-process-output-primitive proc 0.05 nil nil)
                (when (and (processp stderr-proc)
                           (process-live-p stderr-proc))
                  (funcall anvil-host--accept-process-output-primitive stderr-proc 0 nil nil)))
              (when (process-live-p proc)
                (error "anvil-host: shell timeout after %ss: %s"
                       timeout command)))
            ;; A detached descendant may retain stderr; drain only briefly.
            (when (and (processp stderr-proc)
                       (process-live-p stderr-proc))
              (let ((drain-deadline
                     (+ (float-time) anvil-host--stderr-drain-budget-sec)))
                (while (and (process-live-p stderr-proc)
                            (< (float-time) drain-deadline))
                  (funcall anvil-host--accept-process-output-primitive stderr-proc 0.02 nil nil)
                  (funcall anvil-host--accept-process-output-primitive proc 0 nil nil))))
            (funcall anvil-host--accept-process-output-primitive proc 0.01 nil nil))
          (let ((stdout-source
                 (apply #'concat (nreverse stdout-chunks)))
                (stderr-source
                 (apply #'concat (nreverse stderr-chunks))))
            (list
             (process-exit-status proc)
             (anvil-host--decode-output stdout-source coding)
             (anvil-host--decode-output stderr-source coding)
             stdout-total-bytes
             stderr-total-bytes
             stdout-source
             stderr-source)))
      ;; Always derive custody from the pre-call identity snapshot.  A
      ;; constructor may rewrite names, detach buffers, create suffix
      ;; collisions, or return a preexisting peer instead of its new child.
      (let ((inhibit-quit t)
            (anvil-host-child-process-environment nil)
            (anvil-host-child-exec-path nil)
            (anvil-host-child-shell-file-name nil)
            (anvil-host-child-shell-command-switch nil))
        (let* ((processes
                (anvil-host--transaction-processes
                 proc shell-process-name stdout-filter
                 main-constructor-started-p main-constructor-before
                 main-constructor-complete-p
                 main-constructor-invalid-p main-constructor-processes))
               (stderr-processes
                (anvil-host--transaction-processes
                 stderr-proc stderr-process-name stderr-filter
                 stderr-constructor-started-p stderr-constructor-before
                 stderr-constructor-complete-p
                 stderr-constructor-invalid-p
                 stderr-constructor-processes))
               (metadata
                (anvil-host--candidate-metadata
                 processes stderr-processes)))
          (anvil-host--retire-resources metadata))))))

;;;; --- Layer 1: anvil-shell -----------------------------------------------

(defun anvil-shell (command &optional opts)
  "Run shell COMMAND on the host and return result as a plist.
OPTS is a plist:
  :timeout    seconds (default 30)
  :max-output bytes per stream (default 16384, nil = no limit)
  :coding     coding-system for I/O (default: cp932-dos on Windows, utf-8 elsewhere)
  :cwd        working directory
  :include-source-bytes
              include private captured unibyte source strings for callers
              that must preserve source-stream byte accounting

Returns:
  (:exit N :stdout STR :stderr STR :stdout-captured STR
   :stderr-captured STR :stdout-total-bytes N
   :stderr-total-bytes N :command STR :coding SYM :truncated BOOL
   [:stdout-captured-source-bytes UNIBYTE-STR
    :stderr-captured-source-bytes UNIBYTE-STR])

The public stdout/stderr fields carry presentation truncation sentinels.
The captured fields are undecorated, bounded decoded text.  When requested,
the source fields are exact bounded source-stream bytes and are the only
fields suitable for source-encoding byte accounting.

Note: this is synchronous and fail-closed against overlapping or reentrant
host submissions.  Its process waits keep ordinary timers responsive, but
Emacs may dispatch a ready foreign process filter; use a dedicated daemon plus
bridge watchdog for hard containment from arbitrary callback hangs."
  (let* ((timeout    (or (plist-get opts :timeout) anvil-host--default-timeout))
         (max-output (if (plist-member opts :max-output)
                         (plist-get opts :max-output)
                       anvil-host--default-max-output))
         (coding     (or (plist-get opts :coding) (anvil-host--default-coding)))
         (cwd        (plist-get opts :cwd))
         (include-source-bytes
          (plist-get opts :include-source-bytes)))
    (unless (or (null max-output)
                (and (integerp max-output) (>= max-output 0)))
      (error "anvil-host: :max-output must be a nonnegative integer or nil"))
    (let* ((result
            (anvil-host--run command coding cwd timeout max-output))
           (exit (nth 0 result))
           (stdout (nth 1 result))
           (stderr (nth 2 result))
           (stdout-total (or (nth 3 result) (string-bytes stdout)))
           (stderr-total (or (nth 4 result) (string-bytes stderr)))
           (truncated
            (or (and max-output (> stdout-total max-output))
                (and max-output (> stderr-total max-output)))))
      (append
       (list :exit exit
             :stdout (anvil-host--truncate
                      stdout max-output stdout-total)
             :stderr (anvil-host--truncate
                      stderr max-output stderr-total)
             :stdout-captured stdout
             :stderr-captured stderr
             :stdout-total-bytes stdout-total
             :stderr-total-bytes stderr-total
             :command command
             :coding coding
             :truncated (and truncated t))
       (when include-source-bytes
         (list :stdout-captured-source-bytes (nth 5 result)
               :stderr-captured-source-bytes (nth 6 result)))))))

(defun anvil-shell-by-os (spec &optional opts)
  "Run an OS-dispatched shell command. SPEC is a plist:
  :windows  command for windows-nt
  :darwin   command for macOS
  :linux    command for gnu/linux

The command matching `system-type' is forwarded to `anvil-shell'
together with OPTS. Errors if the current OS has no entry."
  (let ((cmd (cond ((eq system-type 'windows-nt) (plist-get spec :windows))
                   ((eq system-type 'darwin)     (plist-get spec :darwin))
                   ((eq system-type 'gnu/linux)  (plist-get spec :linux))
                   (t (error "anvil-host: unsupported system-type: %s"
                             system-type)))))
    (unless cmd
      (error "anvil-host: no command provided for %s in spec" system-type))
    (anvil-shell cmd opts)))

;;;; --- internal: parsers --------------------------------------------------

(defun anvil-host--parse-wmic-value (output)
  "Parse `wmic ... /value' OUTPUT into a list of plists, one per record.
Records are separated by blank lines. Keys are downcased and interned
as keywords.

Note: wmic emits \\r\\r\\n line endings on Windows; we split on \\n
only and trim trailing \\r from each line so that intra-line CRs do
not look like record separators."
  (let ((records '())
        (current '()))
    (dolist (raw (split-string output "\n" nil))
      (let ((line (string-trim raw)))
        (cond
         ((string-empty-p line)
          (when current
            (push (nreverse current) records)
            (setq current nil)))
         ((string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" line)
          (let ((k (intern (concat ":" (downcase (match-string 1 line)))))
                (v (string-trim (match-string 2 line))))
            ;; push key first, then value, so after nreverse the
            ;; result is (k v k v ...) — a proper plist.
            (push k current)
            (push v current))))))
    (when current (push (nreverse current) records))
    (nreverse records)))

(defun anvil-host--first-record (output)
  "Return the first plist parsed from wmic /value OUTPUT, or nil."
  (car (anvil-host--parse-wmic-value output)))

(defun anvil-host--num (str)
  "Parse STR as a number, returning nil on failure or empty input."
  (and str
       (let ((s (string-trim str)))
         (and (not (string-empty-p s))
              (string-match-p "\\`-?[0-9]+\\(\\.[0-9]+\\)?\\'" s)
              (string-to-number s)))))

(defun anvil-host--bytes->gb (bytes &optional precision)
  "Convert BYTES (number) to gigabytes, rounded to PRECISION (default 1)."
  (and (numberp bytes)
       (let ((p (or precision 1)))
         (/ (round (* (/ bytes 1073741824.0) (expt 10 p)))
            (float (expt 10 p))))))

(defun anvil-host--parse-wmic-array (s)
  "Parse a wmic array value like {\"a\",\"b\",\"c\"} into a list of strings.
Returns a list of strings, or a single-element list if S is a plain
scalar, or nil for empty input."
  (cond
   ((or (null s) (string-empty-p s)) nil)
   ((string-match-p "\\`{.*}\\'" s)
    (let (results
          (start 0))
      (while (string-match "\"\\([^\"]*\\)\"" s start)
        (push (match-string 1 s) results)
        (setq start (match-end 0)))
      (nreverse results)))
   (t (list s))))

(defun anvil-host--first-ipv4 (strings)
  "Return the first IPv4 address from STRINGS, or nil."
  (cl-find-if (lambda (s) (string-match-p "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\'" s))
              strings))

(defun anvil-host--parse-wmi-datetime (s)
  "Parse a WMI datetime string yyyymmddHHMMSS.ffffff±UTC into ISO 8601."
  (when (and s (string-match
                "\\`\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)"
                s))
    (format "%s-%s-%sT%s:%s:%s"
            (match-string 1 s) (match-string 2 s) (match-string 3 s)
            (match-string 4 s) (match-string 5 s) (match-string 6 s))))

;;;; --- internal: per-OS host queries --------------------------------------

;; Each anvil-host--info-CATEGORY-OS function returns a plist for that
;; category. They never error — on failure they return (:error STR).

;; --- OS ---

(defun anvil-host--info-os-windows ()
  (let* ((res (anvil-shell
               "wmic os get Caption,Version,BuildNumber,OSArchitecture /value"
               '(:max-output 4096)))
         (rec (anvil-host--first-record (plist-get res :stdout))))
    (list :type 'windows-nt
          :name        (plist-get rec :caption)
          :version     (plist-get rec :version)
          :build       (plist-get rec :buildnumber)
          :arch        (plist-get rec :osarchitecture)
          :hostname    (system-name)
          :user        (user-login-name))))

(defun anvil-host--info-os-darwin ()
  (let* ((name    (string-trim (plist-get (anvil-shell "sw_vers -productName") :stdout)))
         (version (string-trim (plist-get (anvil-shell "sw_vers -productVersion") :stdout)))
         (build   (string-trim (plist-get (anvil-shell "sw_vers -buildVersion") :stdout)))
         (arch    (string-trim (plist-get (anvil-shell "uname -m") :stdout))))
    (list :type 'darwin :name name :version version :build build :arch arch
          :hostname (system-name) :user (user-login-name))))

(defun anvil-host--info-os-linux ()
  (let* ((res (anvil-shell "cat /etc/os-release 2>/dev/null"))
         (out (plist-get res :stdout))
         name version)
    (dolist (line (split-string out "\n" t))
      (when (string-match "\\`PRETTY_NAME=\"?\\([^\"]*\\)\"?" line)
        (setq name (match-string 1 line)))
      (when (string-match "\\`VERSION_ID=\"?\\([^\"]*\\)\"?" line)
        (setq version (match-string 1 line))))
    (list :type 'gnu/linux
          :name name
          :version version
          :arch (string-trim (plist-get (anvil-shell "uname -m") :stdout))
          :kernel (string-trim (plist-get (anvil-shell "uname -r") :stdout))
          :hostname (system-name)
          :user (user-login-name))))

;; --- CPU ---

(defun anvil-host--info-cpu-windows ()
  (let* ((res (anvil-shell
               "wmic cpu get Name,NumberOfCores,NumberOfLogicalProcessors,MaxClockSpeed /value"
               '(:max-output 4096)))
         (rec (anvil-host--first-record (plist-get res :stdout))))
    (list :model    (plist-get rec :name)
          :physical (anvil-host--num (plist-get rec :numberofcores))
          :logical  (anvil-host--num (plist-get rec :numberoflogicalprocessors))
          :mhz-max  (anvil-host--num (plist-get rec :maxclockspeed)))))

(defun anvil-host--info-cpu-darwin ()
  (list :model    (string-trim (plist-get (anvil-shell "sysctl -n machdep.cpu.brand_string") :stdout))
        :physical (anvil-host--num (string-trim (plist-get (anvil-shell "sysctl -n hw.physicalcpu") :stdout)))
        :logical  (anvil-host--num (string-trim (plist-get (anvil-shell "sysctl -n hw.logicalcpu") :stdout)))
        :mhz-max  (let ((hz (anvil-host--num
                             (string-trim (plist-get (anvil-shell "sysctl -n hw.cpufrequency_max") :stdout)))))
                    (and hz (round (/ hz 1.0e6))))))

(defun anvil-host--info-cpu-linux ()
  (let* ((res (anvil-shell "cat /proc/cpuinfo 2>/dev/null" '(:max-output 65536)))
         (out (plist-get res :stdout))
         model physical logical mhz)
    (dolist (line (split-string out "\n" t))
      (cond
       ((and (not model) (string-match "\\`model name\\s-*:\\s-*\\(.*\\)" line))
        (setq model (string-trim (match-string 1 line))))
       ((and (not mhz) (string-match "\\`cpu MHz\\s-*:\\s-*\\([0-9.]+\\)" line))
        (setq mhz (round (string-to-number (match-string 1 line)))))))
    (setq logical (anvil-host--num
                   (string-trim (plist-get (anvil-shell "nproc 2>/dev/null") :stdout))))
    (setq physical (anvil-host--num
                    (string-trim
                     (plist-get (anvil-shell "lscpu 2>/dev/null | awk -F: '/^Core\\(s\\) per socket/ {c=$2} /^Socket\\(s\\)/ {s=$2} END {print c*s}'") :stdout))))
    (list :model model :physical physical :logical logical :mhz-max mhz)))

;; --- RAM ---

(defun anvil-host--info-ram-windows ()
  (let* ((res (anvil-shell
               "wmic os get TotalVisibleMemorySize,FreePhysicalMemory /value"
               '(:max-output 4096)))
         (rec (anvil-host--first-record (plist-get res :stdout)))
         (total-kb (anvil-host--num (plist-get rec :totalvisiblememorysize)))
         (free-kb  (anvil-host--num (plist-get rec :freephysicalmemory))))
    (list :total-gb (and total-kb (anvil-host--bytes->gb (* total-kb 1024)))
          :free-gb  (and free-kb  (anvil-host--bytes->gb (* free-kb  1024))))))

(defun anvil-host--info-ram-darwin ()
  (let* ((bytes (anvil-host--num
                 (string-trim (plist-get (anvil-shell "sysctl -n hw.memsize") :stdout)))))
    (list :total-gb (anvil-host--bytes->gb bytes)
          :free-gb  nil))) ; vm_stat parsing omitted; rarely needed

(defun anvil-host--info-ram-linux ()
  (let* ((out (plist-get (anvil-shell "cat /proc/meminfo 2>/dev/null") :stdout))
         total free)
    (dolist (line (split-string out "\n" t))
      (cond
       ((string-match "\\`MemTotal:\\s-+\\([0-9]+\\) kB" line)
        (setq total (string-to-number (match-string 1 line))))
       ((string-match "\\`MemAvailable:\\s-+\\([0-9]+\\) kB" line)
        (setq free  (string-to-number (match-string 1 line))))))
    (list :total-gb (and total (anvil-host--bytes->gb (* total 1024)))
          :free-gb  (and free  (anvil-host--bytes->gb (* free  1024))))))

;; --- DISK ---

(defun anvil-host--info-disk-windows ()
  (let* ((res (anvil-shell
               "wmic logicaldisk where DriveType=3 get Caption,Size,FreeSpace /value"
               '(:max-output 8192)))
         (recs (anvil-host--parse-wmic-value (plist-get res :stdout))))
    (mapcar (lambda (r)
              (let ((size (anvil-host--num (plist-get r :size)))
                    (free (anvil-host--num (plist-get r :freespace))))
                (list :drive (plist-get r :caption)
                      :total-gb (anvil-host--bytes->gb size)
                      :free-gb  (anvil-host--bytes->gb free))))
            recs)))

(defun anvil-host--info-disk-unix ()
  (let* ((res (anvil-shell "df -k -P 2>/dev/null"))
         (out (plist-get res :stdout))
         (lines (cdr (split-string out "\n" t))) ; drop header
         results)
    (dolist (line lines)
      (let ((fields (split-string line)))
        (when (>= (length fields) 6)
          (let ((total-kb (anvil-host--num (nth 1 fields)))
                (free-kb  (anvil-host--num (nth 3 fields)))
                (mount    (nth 5 fields)))
            ;; Skip pseudo filesystems
            (unless (string-match-p "\\`/\\(proc\\|sys\\|dev\\|run\\)" mount)
              (push (list :drive mount
                          :total-gb (and total-kb (anvil-host--bytes->gb (* total-kb 1024)))
                          :free-gb  (and free-kb  (anvil-host--bytes->gb (* free-kb  1024))))
                    results))))))
    (nreverse results)))

;; --- GPU ---

(defun anvil-host--info-gpu-windows ()
  (let* ((res (anvil-shell
               "wmic path Win32_VideoController get Name,AdapterRAM,DriverVersion /value"
               '(:max-output 8192)))
         (recs (anvil-host--parse-wmic-value (plist-get res :stdout))))
    (mapcar (lambda (r)
              (let ((vram (anvil-host--num (plist-get r :adapterram))))
                (list :name (plist-get r :name)
                      :vram-gb (and vram (anvil-host--bytes->gb vram))
                      :driver  (plist-get r :driverversion))))
            recs)))

(defun anvil-host--info-gpu-darwin ()
  (let* ((out (plist-get (anvil-shell "system_profiler SPDisplaysDataType 2>/dev/null"
                                       '(:max-output 8192))
                         :stdout))
         results name vram)
    (dolist (line (split-string out "\n" t))
      (cond
       ((string-match "Chipset Model:\\s-*\\(.*\\)" line)
        (when name
          (push (list :name name :vram-gb vram) results)
          (setq vram nil))
        (setq name (string-trim (match-string 1 line))))
       ((string-match "VRAM[^:]*:\\s-*\\([0-9]+\\)\\s-*\\(MB\\|GB\\)" line)
        (let ((n (string-to-number (match-string 1 line)))
              (u (match-string 2 line)))
          (setq vram (if (string= u "GB") (float n) (/ n 1024.0)))))))
    (when name (push (list :name name :vram-gb vram) results))
    (nreverse results)))

(defun anvil-host--info-gpu-linux ()
  (let* ((out (plist-get (anvil-shell "lspci 2>/dev/null | grep -iE 'vga|3d|display'") :stdout)))
    (mapcar (lambda (line)
              (list :name (replace-regexp-in-string
                           "\\`[^:]+:[^:]+:\\s-*" "" line)
                    :vram-gb nil :driver nil))
            (split-string out "\n" t))))

;; --- NETWORK ---

(defun anvil-host--info-net-windows ()
  (let* ((res (anvil-shell
               "wmic nicconfig where IPEnabled=true get IPAddress,DefaultIPGateway,Description /value"
               '(:max-output 8192)))
         (recs (anvil-host--parse-wmic-value (plist-get res :stdout))))
    (mapcar (lambda (r)
              (let* ((ips      (anvil-host--parse-wmic-array (plist-get r :ipaddress)))
                     (gateways (anvil-host--parse-wmic-array (plist-get r :defaultipgateway))))
                (list :iface    (plist-get r :description)
                      :ipv4     (anvil-host--first-ipv4 ips)
                      :ips      ips
                      :gateway  (or (anvil-host--first-ipv4 gateways) (car gateways)))))
            recs)))

(defun anvil-host--info-net-darwin ()
  (let* ((iface (string-trim (plist-get (anvil-shell "route get default 2>/dev/null | awk '/interface:/ {print $2}'") :stdout)))
         (ip    (string-trim (plist-get (anvil-shell (format "ipconfig getifaddr %s 2>/dev/null" iface)) :stdout)))
         (gw    (string-trim (plist-get (anvil-shell "route get default 2>/dev/null | awk '/gateway:/ {print $2}'") :stdout))))
    (list (list :iface iface :ip ip :gateway gw))))

(defun anvil-host--info-net-linux ()
  (let* ((iface (string-trim (plist-get (anvil-shell "ip route 2>/dev/null | awk '/^default/ {print $5; exit}'") :stdout)))
         (ip    (string-trim (plist-get (anvil-shell (format "ip -4 -o addr show %s 2>/dev/null | awk '{print $4}'" iface)) :stdout)))
         (gw    (string-trim (plist-get (anvil-shell "ip route 2>/dev/null | awk '/^default/ {print $3; exit}'") :stdout))))
    (list (list :iface iface :ip ip :gateway gw))))

;; --- UPTIME ---

(defun anvil-host--info-uptime-windows ()
  (let* ((res (anvil-shell "wmic os get LastBootUpTime /value" '(:max-output 1024)))
         (rec (anvil-host--first-record (plist-get res :stdout)))
         (boot (anvil-host--parse-wmi-datetime (plist-get rec :lastbootuptime))))
    (list :boot-time boot
          :uptime    (and boot
                          (let ((boot-time
                                 (encode-time
                                  (parse-time-string (replace-regexp-in-string "T" " " boot)))))
                            (round (- (float-time) (float-time boot-time))))))))

(defun anvil-host--info-uptime-unix ()
  (let* ((boot (string-trim (plist-get (anvil-shell "uptime -s 2>/dev/null") :stdout))))
    (list :boot-time (and (not (string-empty-p boot)) boot)
          :uptime nil)))

;; --- EMACS (no shell) ---

(defun anvil-host--info-emacs ()
  (list :version              emacs-version
        :system-configuration system-configuration
        :native-comp          (and (fboundp 'native-comp-available-p)
                                   (native-comp-available-p))
        :gui                  (display-graphic-p)
        :daemon               (daemonp)
        :init-file            user-init-file
        :user-emacs-directory user-emacs-directory))

;;;; --- Layer 2: anvil-host-info -------------------------------------------

(defconst anvil-host--all-categories
  '(os cpu ram disk gpu network uptime emacs)
  "Default category set for `anvil-host-info'.")

(defun anvil-host--dispatch (category)
  "Run the per-OS info function for CATEGORY. Returns plist or list."
  (pcase category
    ('os      (cond ((eq system-type 'windows-nt) (anvil-host--info-os-windows))
                    ((eq system-type 'darwin)     (anvil-host--info-os-darwin))
                    ((eq system-type 'gnu/linux)  (anvil-host--info-os-linux))))
    ('cpu     (cond ((eq system-type 'windows-nt) (anvil-host--info-cpu-windows))
                    ((eq system-type 'darwin)     (anvil-host--info-cpu-darwin))
                    ((eq system-type 'gnu/linux)  (anvil-host--info-cpu-linux))))
    ('ram     (cond ((eq system-type 'windows-nt) (anvil-host--info-ram-windows))
                    ((eq system-type 'darwin)     (anvil-host--info-ram-darwin))
                    ((eq system-type 'gnu/linux)  (anvil-host--info-ram-linux))))
    ('disk    (cond ((eq system-type 'windows-nt) (anvil-host--info-disk-windows))
                    (t                            (anvil-host--info-disk-unix))))
    ('gpu     (cond ((eq system-type 'windows-nt) (anvil-host--info-gpu-windows))
                    ((eq system-type 'darwin)     (anvil-host--info-gpu-darwin))
                    ((eq system-type 'gnu/linux)  (anvil-host--info-gpu-linux))))
    ('network (cond ((eq system-type 'windows-nt) (anvil-host--info-net-windows))
                    ((eq system-type 'darwin)     (anvil-host--info-net-darwin))
                    ((eq system-type 'gnu/linux)  (anvil-host--info-net-linux))))
    ('uptime  (cond ((eq system-type 'windows-nt) (anvil-host--info-uptime-windows))
                    (t                            (anvil-host--info-uptime-unix))))
    ('emacs   (anvil-host--info-emacs))
    (_ (error "anvil-host: unknown category: %s" category))))

(defun anvil-host-info (&optional categories)
  "Return host PC info as a nested plist.
CATEGORIES is a list of symbols (subset of `anvil-host--all-categories').
Default = all categories.

Each value is a plist (or list of plists for disk / gpu / network).
On per-category failure, the category value is (:error \"...\")."
  (let ((cats (or categories anvil-host--all-categories))
        result)
    (dolist (cat cats)
      (let ((val (condition-case err
                     (anvil-host--dispatch cat)
                   (error (list :error (error-message-string err))))))
        (setq result (plist-put result
                                (intern (concat ":" (symbol-name cat)))
                                val))))
    result))

;;;; --- Layer 1.5: which / env ---------------------------------------------

(defun anvil-host-which (cmd)
  "Return the absolute path of CMD on the host, or nil.
CMD can be:
  - a string  → returns string-or-nil
  - a list of strings → returns ((NAME . PATH-OR-NIL) ..)

Wraps `executable-find', which respects `exec-path' (the
Emacs-internal PATH) — useful for diagnosing PATH drift between
shell and Emacs."
  (cond
   ((stringp cmd) (executable-find cmd))
   ((listp cmd)
    (let (results)
      (dolist (c cmd)
        (push (cons c (executable-find c)) results))
      (nreverse results)))
   (t (error "anvil-host: which expects string or list, got %s"
             (type-of cmd)))))

(defun anvil-host-env (key-or-list)
  "Return the value of environment variable KEY, or nil.
KEY-OR-LIST can be:
  - a string → returns string-or-nil
  - a list of strings → returns ((NAME . VALUE-OR-NIL) ..)

Wraps `getenv', which reflects the Emacs-process environment
(may differ from a fresh shell if Emacs was started with a
restricted env)."
  (cond
   ((stringp key-or-list) (getenv key-or-list))
   ((listp key-or-list)
    (let (results)
      (dolist (k key-or-list)
        (push (cons k (getenv k)) results))
      (nreverse results)))
   (t (error "anvil-host: env expects string or list, got %s"
             (type-of key-or-list)))))

;;;; --- discoverability ----------------------------------------------------

(defun anvil-host-helpers-list ()
  "Return a list of all anvil-host* user-facing function names."
  (let (results)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (let ((n (symbol-name sym)))
                    (and (or (string-prefix-p "anvil-host" n)
                             (string= n "anvil-shell")
                             (string= n "anvil-shell-by-os"))
                         (not (string-match-p "--" n)))))
         (push sym results))))
    (sort results (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(provide 'anvil-host)
;;; anvil-host.el ends here
