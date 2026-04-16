;;; anvil-worker.el --- Lane-aware worker pool for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Fujisawa Electric Management Office

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Emacs is the human's tool first.  AI should never freeze the
;; editor.  This module spawns isolated sub-Emacs daemons and routes
;; MCP tool calls through them.
;;
;; v2 ships the Doc 01 "Phase 1 — Lane split" milestone.  Workers
;; are now organised into three lanes:
;;
;;   :read   — read-only or generally side-effect-light tool calls
;;   :write  — mutating tool calls (file affinity to be added in a
;;             later phase; for now treated as a serialised lane)
;;   :batch  — pre-warmed `emacs --batch'-style fire-and-forget
;;             work (Phase 3 will populate; default size 0)
;;
;; The dispatcher accepts a `:kind' keyword (`:read' / `:write' /
;; `:batch' / `:auto').  In Phase 1 `:auto' simply round-robins
;; across whatever lanes have capacity — a regex/intent-based
;; classifier is Phase 2 work.
;;
;; Backwards compatibility:
;;
;;   * `anvil-worker-pool-size' is preserved as an obsolete alias
;;     for `anvil-worker-read-pool-size' so existing init.org
;;     overrides keep working.
;;   * `(anvil-worker-call EXPR TIMEOUT)' — the old positional
;;     form used by `anvil-cron.el' — is still accepted alongside
;;     the new `(anvil-worker-call EXPR :kind ... :timeout ...)'.

;;; Code:

(require 'cl-lib)
(require 'server)

;;; Customization

(defgroup anvil-worker nil
  "Anvil worker pool — isolated execution for AI tool calls."
  :group 'anvil
  :prefix "anvil-worker-")

(defcustom anvil-worker-read-pool-size 2
  "Number of read-lane worker daemons.
Read-lane workers handle tool calls that are read-only or
generally side-effect-light.  Each worker is a separate Emacs
process (~30 MB RAM)."
  :type 'integer
  :group 'anvil-worker)

(defcustom anvil-worker-write-pool-size 1
  "Number of write-lane worker daemons.
Write-lane workers handle mutating tool calls.  Defaults to 1 to
serialise writes; future phases may shard by file path."
  :type 'integer
  :group 'anvil-worker)

(defcustom anvil-worker-batch-pool-size 1
  "Number of batch-lane worker daemons (pre-warmed `emacs --batch' style).
Default is 1 since Doc 01 Phase 3 — set to 0 to disable the lane
entirely (the classifier then silently downgrades batch-flagged
expressions to `:write')."
  :type 'integer
  :group 'anvil-worker)

(defcustom anvil-worker-batch-warmup-expressions
  '("(require 'org)"
    "(require 'cl-lib)"
    "(require 'subr-x)")
  "S-expression strings sent to each freshly-spawned batch worker.
Pre-loading the common libraries the first real `:batch' tool
call is likely to need amortises the cold-start `require' time so
the user does not pay for it on the first dispatch.

Each expression runs via `emacsclient -n -e' (fire-and-forget),
so failures only surface in the worker's own *Messages* buffer."
  :type '(repeat string)
  :group 'anvil-worker)

(defcustom anvil-worker-batch-warmup-delay 2.0
  "Seconds to wait after a batch daemon spawn before sending warmup.
The fresh daemon needs a moment for its server socket to bind;
firing `emacsclient' too soon would simply fail with `not running'."
  :type 'number
  :group 'anvil-worker)

(define-obsolete-variable-alias 'anvil-worker-pool-size
  'anvil-worker-read-pool-size "anvil 0.7"
  "Renamed when Doc 01 introduced read/write/batch lanes.
The old `pool-size' meant a single round-robin pool; the new
`read-pool-size' is the read-lane size only.  Set `read-pool-size'
plus `write-pool-size' (and optionally `batch-pool-size') to size
the v2 pool explicitly.")

(defcustom anvil-worker-emacs-bin (or (executable-find "emacs") "emacs")
  "Path to the Emacs binary used to spawn worker daemons."
  :type 'file
  :group 'anvil-worker)

(defcustom anvil-worker-init-file nil
  "Init file loaded into worker daemons.
If nil, anvil generates a minimal init that registers the eval tool."
  :type '(choice (const :tag "Auto-generate" nil) file)
  :group 'anvil-worker)

(defcustom anvil-worker-health-check-interval 30
  "Seconds between worker health checks."
  :type 'integer
  :group 'anvil-worker)

(defcustom anvil-worker-alive-check-timeout 2.0
  "Seconds to wait for the `emacsclient -e t' liveness probe.
If the probe does not return within this window, the worker is
treated as dead and the probe process is killed.  This guards
against stale server files whose TCP port has been reused by a
process that accepts the connection but never replies to the
Emacs server auth protocol — which would otherwise hang the
probing daemon indefinitely."
  :type 'number
  :group 'anvil-worker)

(defcustom anvil-worker-call-timeout 60
  "Default timeout in seconds for normal worker calls."
  :type 'integer
  :group 'anvil-worker)

(defcustom anvil-worker-connection-method 'emacsclient
  "Transport used by `anvil-worker-call' to reach a worker daemon.

  `emacsclient' (default)    — external process per call.  Reliable
      but pays ~10ms spawn + emacsclient binary load every dispatch.
  `server-eval-at'            — in-process TCP via the Emacs server
      protocol.  Phase 4c experiment: no per-call spawn.
      WARNING: 2026-04-16 reproduced a hard main-daemon deadlock
      when invoked in a tight loop from within an emacsclient-
      initiated eval — `with-timeout' did not fire and the daemon
      had to be force-killed.  Do NOT enable without reproducing
      the failure in a throwaway daemon first."
  :type '(choice (const :tag "External process (emacsclient)" emacsclient)
                 (const :tag "In-process TCP (server-eval-at) — EXPERIMENTAL"
                        server-eval-at))
  :group 'anvil-worker)

(defcustom anvil-worker-heavy-timeout 300
  "Timeout in seconds for heavy operations (byte-compile, etc)."
  :type 'integer
  :group 'anvil-worker)

(defcustom anvil-worker-spawn-wait 5
  "Max seconds to wait for a freshly spawned worker."
  :type 'integer
  :group 'anvil-worker)

(defcustom anvil-worker-lifecycle-log
  (expand-file-name "anvil-worker.log" user-emacs-directory)
  "Append-only log of worker lifecycle events."
  :type 'file
  :group 'anvil-worker)

;;; Heavy-op detection

(defcustom anvil-worker-heavy-patterns
  '(("byte-compile"         . "(byte-compile")
    ("insert-file-contents" . "(insert-file-contents")
    ("call-process"         . "(call-process\\b")
    ("shell-command"        . "(shell-command")
    ("org-babel-tangle"     . "(org-babel-tangle")
    ("elfeed-update"        . "(elfeed-update")
    ("package-install"      . "(package-install")
    ("url-retrieve-sync"    . "(url-retrieve-synchronously")
    ("make-network-process" . "(make-network-process")
    ("find-file"            . "(find-file[- ]"))
  "Alist of (LABEL . REGEX) for heavy operation detection.
Expressions matching these patterns get routed with longer timeout."
  :type '(alist :key-type string :value-type string)
  :group 'anvil-worker)

;;; Classifier (Doc 01 Phase 2)

(defcustom anvil-worker-classify-read-patterns
  '("(\\(?:anvil-\\)?org-read-\\(?:by-id\\|file\\|outline\\|headline\\)\\b"
    "(\\(?:anvil-\\)?org-index-search\\b"
    "(\\(?:anvil-\\)?file-read\\b"
    "(\\(?:anvil-\\)?file-outline\\b"
    "(\\(?:anvil-\\)?buffer-read\\b"
    "(\\(?:anvil-\\)?buffer-list-modified\\b"
    "(\\(?:anvil-\\)?elisp-describe-\\(?:function\\|variable\\)\\b"
    "(\\(?:anvil-\\)?elisp-info-lookup-symbol\\b"
    "(\\(?:anvil-\\)?elisp-read-source-file\\b"
    "(\\(?:anvil-\\)?elisp-get-function-definition\\b"
    "(\\(?:anvil-\\)?sqlite-query\\b"
    "(\\(?:anvil-\\)?org-get-\\(?:allowed-files\\|tag-config\\|todo-config\\)\\b")
  "Regexes that flag EXPRESSION as a read-only `:read'-lane call.
Matched in document order; the first matching pattern wins.  When
no pattern matches the classifier falls back to the write lane —
see `anvil-worker-classify-unknown-fallback'."
  :type '(repeat regexp)
  :group 'anvil-worker)

(defcustom anvil-worker-classify-write-patterns
  '("(\\(?:anvil-\\)?file-replace-\\(?:string\\|regexp\\)\\b"
    "(\\(?:anvil-\\)?file-insert-at-line\\b"
    "(\\(?:anvil-\\)?file-delete-lines\\b"
    "(\\(?:anvil-\\)?file-append\\b"
    "(\\(?:anvil-\\)?file-prepend\\b"
    "(\\(?:anvil-\\)?file-batch\\(?:-across\\)?\\b"
    "(\\(?:anvil-\\)?file-ensure-import\\b"
    "(\\(?:anvil-\\)?json-object-add\\b"
    "(\\(?:anvil-\\)?buffer-save\\b"
    "(\\(?:anvil-\\)?org-edit-body\\b"
    "(\\(?:anvil-\\)?org-add-todo\\b"
    "(\\(?:anvil-\\)?org-rename-headline\\b"
    "(\\(?:anvil-\\)?org-update-todo-state\\b"
    "(save-buffer\\b"
    "(write-region\\b"
    "(write-file\\b"
    "(delete-file\\b"
    "(rename-file\\b"
    "(make-directory\\b")
  "Regexes that flag EXPRESSION as a mutating `:write'-lane call.
Tested *before* the read patterns so an expression that mixes
read+write operations is still routed to write."
  :type '(repeat regexp)
  :group 'anvil-worker)

(defcustom anvil-worker-classify-batch-patterns
  '("(byte-compile\\(?:-file\\)?\\b"
    "(org-babel-tangle\\b"
    "(\\(?:anvil-\\)?elisp-byte-compile-file\\b")
  "Regexes that flag EXPRESSION as a batch-pool candidate.
Routed to `:batch' only when `anvil-worker-batch-pool-size' > 0;
otherwise the classifier silently downgrades to `:write'."
  :type '(repeat regexp)
  :group 'anvil-worker)

(defcustom anvil-worker-classify-unknown-fallback :write
  "Lane chosen by `anvil-worker--classify' when no pattern matches.
Defaults to `:write' (the safe side per Doc 01) so an
unrecognised expression that happens to mutate state never sneaks
into a read replica."
  :type '(choice (const :read) (const :write) (const :batch))
  :group 'anvil-worker)

(defvar anvil-worker--metrics-classify
  (list :read 0 :write 0 :batch 0 :unknown-fallback 0)
  "Histogram of classifier decisions made by `anvil-worker--classify'.
The `:unknown-fallback' bucket counts only the no-match cases; a
positive match for the same lane goes to that lane's bucket.
Reset with `anvil-worker-classify-metrics-reset'.")

(defun anvil-worker-classify-metrics-reset ()
  "Zero out `anvil-worker--metrics-classify'."
  (interactive)
  (setq anvil-worker--metrics-classify
        (list :read 0 :write 0 :batch 0 :unknown-fallback 0)))

(defun anvil-worker-classify-metrics-show ()
  "Echo the classifier histogram."
  (interactive)
  (message "Anvil classifier metrics: %S" anvil-worker--metrics-classify))

(defun anvil-worker--metrics-bump (key)
  "Increment KEY in `anvil-worker--metrics-classify'."
  (setq anvil-worker--metrics-classify
        (plist-put anvil-worker--metrics-classify key
                   (1+ (or (plist-get anvil-worker--metrics-classify key) 0)))))

;;; Latency metrics (Doc 01 Phase 4a — measurement infrastructure)

(defcustom anvil-worker-latency-sample-cap 1000
  "Per-lane ring-buffer capacity for total-ms samples (used by percentiles).
Older samples are dropped once the cap is reached.  Set to 0 to
disable sample retention while keeping the running sums."
  :type 'integer
  :group 'anvil-worker)

(defun anvil-worker--latency-empty-bucket ()
  "Fresh per-lane latency bucket."
  (list :samples 0
        :spawn-ms-sum 0.0
        :wait-ms-sum  0.0
        :total-ms-sum 0.0
        :totals nil))

(defvar anvil-worker--metrics-latency nil
  "Plist `(:read BUCKET :write BUCKET :batch BUCKET)' of latency stats.
Each bucket carries:
  :samples       — completed call count
  :spawn-ms-sum  — sum of (start-process → ready) latency in ms
  :wait-ms-sum   — sum of (ready → completed) latency in ms
  :total-ms-sum  — sum of full call duration in ms
  :totals        — recent total-ms samples (ring, length ≤ cap)
Initialised lazily by `anvil-worker--latency-record'.")

(defun anvil-worker--latency-init ()
  "Allocate (or reset) `anvil-worker--metrics-latency' to empty buckets."
  (setq anvil-worker--metrics-latency
        (list :read  (anvil-worker--latency-empty-bucket)
              :write (anvil-worker--latency-empty-bucket)
              :batch (anvil-worker--latency-empty-bucket))))

(defun anvil-worker--latency-record (lane spawn-ms wait-ms)
  "Record one (SPAWN-MS, WAIT-MS) sample for LANE.
Updates running sums and pushes total-ms onto the per-lane ring."
  (unless anvil-worker--metrics-latency
    (anvil-worker--latency-init))
  (let* ((bucket (plist-get anvil-worker--metrics-latency lane))
         (total-ms (+ spawn-ms wait-ms))
         (totals (cons total-ms (plist-get bucket :totals)))
         (cap    anvil-worker-latency-sample-cap)
         (totals (if (and (> cap 0) (> (length totals) cap))
                     (cl-subseq totals 0 cap)
                   (and (> cap 0) totals))))
    (plist-put bucket :samples
               (1+ (plist-get bucket :samples)))
    (plist-put bucket :spawn-ms-sum
               (+ spawn-ms (plist-get bucket :spawn-ms-sum)))
    (plist-put bucket :wait-ms-sum
               (+ wait-ms  (plist-get bucket :wait-ms-sum)))
    (plist-put bucket :total-ms-sum
               (+ total-ms (plist-get bucket :total-ms-sum)))
    (plist-put bucket :totals totals)
    nil))

(defun anvil-worker--percentile (samples pct)
  "Return the PCT (0-100) percentile of SAMPLES (numbers), or nil if empty."
  (when samples
    (let* ((sorted (sort (copy-sequence samples) #'<))
           (n (length sorted))
           (idx (max 0 (min (1- n)
                            (floor (* (/ pct 100.0) (1- n)))))))
      (nth idx sorted))))

;;;###autoload
(defun anvil-worker-latency-metrics-reset ()
  "Reset per-lane latency stats to empty buckets."
  (interactive)
  (anvil-worker--latency-init))

;;;###autoload
(defun anvil-worker-latency-metrics-show ()
  "Echo per-lane latency stats (n / mean / p50 / p99 in ms)."
  (interactive)
  (if (or (null anvil-worker--metrics-latency)
          (cl-every (lambda (lane)
                      (zerop (plist-get
                              (plist-get anvil-worker--metrics-latency lane)
                              :samples)))
                    anvil-worker--lanes))
      (message "Anvil worker latency: no samples yet")
    (let (lines)
      (dolist (lane anvil-worker--lanes)
        (let* ((b (plist-get anvil-worker--metrics-latency lane))
               (n (plist-get b :samples)))
          (push (if (zerop n)
                    (format "  %-5s: no samples"
                            (anvil-worker--lane-name lane))
                  (format
                   "  %-5s: n=%d mean=%.1fms (spawn=%.1f wait=%.1f) p50=%dms p99=%dms"
                   (anvil-worker--lane-name lane)
                   n
                   (/ (plist-get b :total-ms-sum) n)
                   (/ (plist-get b :spawn-ms-sum) n)
                   (/ (plist-get b :wait-ms-sum) n)
                   (or (anvil-worker--percentile
                        (plist-get b :totals) 50) 0)
                   (or (anvil-worker--percentile
                        (plist-get b :totals) 99) 0)))
                lines)))
      (message "Anvil worker latency:\n%s"
               (mapconcat #'identity (nreverse lines) "\n")))))

(defun anvil-worker--match-any (expression patterns)
  "Return non-nil if EXPRESSION matches any regex in PATTERNS."
  (and (stringp expression)
       (cl-some (lambda (re) (string-match-p re expression)) patterns)))

(defun anvil-worker--classify (expression)
  "Return the lane symbol that should handle EXPRESSION.

Resolution order (first match wins):
  1. `anvil-worker-classify-write-patterns' → `:write'
  2. `anvil-worker-classify-batch-patterns' → `:batch'
     (downgraded to `:write' when batch lane is empty)
  3. `anvil-worker-classify-read-patterns'  → `:read'
  4. otherwise → `anvil-worker-classify-unknown-fallback'

Increments `anvil-worker--metrics-classify' so callers can audit
how often the classifier hits each bucket vs. falls back."
  (let* ((decision
          (cond
           ((anvil-worker--match-any
             expression anvil-worker-classify-write-patterns)
            :write)
           ((anvil-worker--match-any
             expression anvil-worker-classify-batch-patterns)
            (if (> anvil-worker-batch-pool-size 0) :batch :write))
           ((anvil-worker--match-any
             expression anvil-worker-classify-read-patterns)
            :read)
           (t
            (anvil-worker--metrics-bump :unknown-fallback)
            anvil-worker-classify-unknown-fallback))))
    (anvil-worker--metrics-bump decision)
    decision))

;;; Lane plumbing

(defconst anvil-worker--lanes '(:read :write :batch)
  "Lane symbols, in `:auto' priority order (read first, then write, then batch).")

(defvar anvil-worker--pool nil
  "Plist (:read VECTOR :write VECTOR :batch VECTOR) of worker state plists.
Each worker plist carries:
  :lane LANE   :index INT     :name STRING
  :server-file PATH
  :busy  BOOLEAN
  :last-state alive|dead|nil")

(defvar anvil-worker--health-timer nil
  "Repeating timer for pool health checks.")

(defvar anvil-worker--dispatch-index
  (list :read 0 :write 0 :batch 0)
  "Per-lane round-robin index for worker dispatch.")

(defun anvil-worker--lane-name (lane)
  "Return the bare name of LANE keyword (e.g. \"read\")."
  (substring (symbol-name lane) 1))

(defun anvil-worker--lane-size (lane)
  "Return the configured pool size for LANE."
  (pcase lane
    (:read  anvil-worker-read-pool-size)
    (:write anvil-worker-write-pool-size)
    (:batch anvil-worker-batch-pool-size)
    (_ (error "anvil-worker: unknown lane %S" lane))))

(defun anvil-worker--lane-pool (lane)
  "Return the worker vector for LANE, or nil if pool not initialised."
  (plist-get anvil-worker--pool lane))

(defun anvil-worker--worker (lane index)
  "Return worker state plist for LANE/INDEX, or nil if out of range."
  (let ((vec (anvil-worker--lane-pool lane)))
    (and vec (>= index 0) (< index (length vec)) (aref vec index))))

;;; Internal helpers

(defun anvil-worker--name (lane index)
  "Return daemon name for worker LANE/INDEX."
  (format "anvil-worker-%s-%d" (anvil-worker--lane-name lane) (1+ index)))

(defun anvil-worker--server-file (lane index)
  "Return server file path for worker LANE/INDEX."
  (expand-file-name
   (concat "server/" (anvil-worker--name lane index))
   user-emacs-directory))

(defun anvil-worker--log (event &optional details)
  "Append EVENT with optional DETAILS to the lifecycle log."
  (let ((line (format "%s [%s]%s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      event
                      (if details (concat " " details) "")))
        (coding-system-for-write 'utf-8-unix))
    (write-region line nil anvil-worker-lifecycle-log 'append 'no-message)))

(defun anvil-worker--generate-init-file ()
  "Generate a minimal init file for worker daemons.
Always regenerates.  A cached file can go stale when anvil itself
moves on disk (the previous path gets baked into `load-path'), so
we unconditionally rewrite from the current `locate-library'
result.  Signals if `anvil-server' cannot be located."
  (let* ((init-file (expand-file-name "anvil-worker-init.el"
                                      user-emacs-directory))
         (located (locate-library "anvil-server"))
         (anvil-dir (and located (file-name-directory located))))
    (unless anvil-dir
      (error "anvil-worker: cannot locate anvil-server in load-path"))
    (with-temp-buffer
      (insert ";;; anvil-worker-init.el --- Auto-generated -*- lexical-binding: t; -*-\n\n")
      (insert (format "(add-to-list 'load-path %S)\n" anvil-dir))
      (insert "(require 'anvil-server)\n")
      (insert "(require 'anvil-server-commands)\n\n")
      (insert "(defun anvil-worker--eval (expression)\n")
      (insert "  \"Evaluate EXPRESSION on the worker daemon.\n\n")
      (insert "MCP Parameters:\n")
      (insert "  expression - Emacs Lisp expression as a string\"\n")
      (insert "  (anvil-server-with-error-handling\n")
      (insert "    (let ((result (eval (read expression) t)))\n")
      (insert "      (format \"%S\" result))))\n\n")
      ;; Register with a generic server-id; the actual name doesn't matter
      ;; because the worker is reached via emacsclient, not MCP stdio
      (insert "(anvil-server-register-tool #'anvil-worker--eval\n")
      (insert "  :id \"eval\"\n")
      (insert "  :description \"Evaluate Emacs Lisp on the isolated worker\"\n")
      (insert "  :server-id \"worker\")\n\n")
      (insert "(anvil-server-start)\n")
      (insert "(message \"[anvil-worker] ready\")\n")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max) init-file nil 'silent)))
    init-file))

(defun anvil-worker--init-pool ()
  "(Re-)initialise the lane-keyed pool plist from the configured sizes.
Resets the per-lane dispatch indices."
  (setq anvil-worker--pool nil)
  (dolist (lane anvil-worker--lanes)
    (let* ((size (anvil-worker--lane-size lane))
           (vec  (make-vector size nil)))
      (dotimes (i size)
        (aset vec i
              (list :lane lane
                    :index i
                    :name (anvil-worker--name lane i)
                    :server-file (anvil-worker--server-file lane i)
                    :busy nil
                    :last-state nil)))
      (setq anvil-worker--pool
            (plist-put anvil-worker--pool lane vec))))
  (setq anvil-worker--dispatch-index (list :read 0 :write 0 :batch 0)))

;;; Worker lifecycle

(defun anvil-worker--probe-emacsclient (server-file)
  "Return non-nil if `emacsclient -f SERVER-FILE -e t' exits 0 in time.
The probe is run asynchronously via `make-process' and hard-killed
after `anvil-worker-alive-check-timeout' seconds.  This prevents a
stale SERVER-FILE pointing at a reused TCP port from blocking the
caller indefinitely."
  (let* ((buf (generate-new-buffer " *anvil-worker-probe*"))
         (proc (make-process
                :name "anvil-worker-probe"
                :buffer buf
                :command (list "emacsclient" "-f" server-file "-e" "t")
                :noquery t
                :connection-type 'pipe))
         (deadline (+ (float-time) anvil-worker-alive-check-timeout))
         (alive nil))
    (unwind-protect
        (progn
          (while (and (process-live-p proc)
                      (< (float-time) deadline))
            (accept-process-output proc 0.1 nil t))
          (if (process-live-p proc)
              (progn
                (delete-process proc)
                (anvil-worker--log
                 'probe-timeout
                 (format "%s >%.1fs"
                         (file-name-nondirectory server-file)
                         anvil-worker-alive-check-timeout)))
            (setq alive (= 0 (process-exit-status proc)))))
      (when (buffer-live-p buf) (kill-buffer buf)))
    alive))

(defun anvil-worker--server-file-pid (server-file)
  "Return the PID recorded on the first line of SERVER-FILE.
Returns nil if the file cannot be read or the PID cannot be parsed.
Emacs server files start with `HOST:PORT PID' on line one."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents server-file nil 0 256)
      (goto-char (point-min))
      (when (re-search-forward "^\\S-+ \\([0-9]+\\)" (line-end-position) t)
        (string-to-number (match-string 1))))))

(defun anvil-worker--server-file-stale-p (server-file)
  "Return non-nil if SERVER-FILE's recorded PID is no longer running.
A missing PID (unparseable file) is treated as stale too."
  (let ((pid (anvil-worker--server-file-pid server-file)))
    (or (null pid)
        (null (process-attributes pid)))))

(defun anvil-worker--worker-alive-p (worker)
  "Return non-nil if WORKER plist is reachable.
Performs the same three checks as `anvil-worker-alive-p'
(file-exists / PID still alive / bounded `emacsclient' probe)
and deletes a stale server file as a side-effect."
  (let ((server-file (plist-get worker :server-file)))
    (cond
     ((not (file-exists-p server-file)) nil)
     ((anvil-worker--server-file-stale-p server-file)
      (ignore-errors (delete-file server-file))
      (anvil-worker--log
       'stale-server-file
       (file-name-nondirectory server-file))
      nil)
     (t (anvil-worker--probe-emacsclient server-file)))))

(defun anvil-worker-alive-p (&optional index lane)
  "Return non-nil if worker at INDEX (default 0) of LANE (default :read) is alive.
Kept as a public helper for `M-x' and back-compat callers; new
code should pass an explicit LANE."
  (let ((worker (anvil-worker--worker (or lane :read) (or index 0))))
    (and worker (anvil-worker--worker-alive-p worker))))

(defun anvil-worker--send-warmup (worker)
  "Send `anvil-worker-batch-warmup-expressions' to WORKER, fire-and-forget.
Returns the list of expressions actually dispatched (empty when
WORKER has not come alive yet — the caller may re-schedule)."
  (when (anvil-worker--worker-alive-p worker)
    (let ((server-file (plist-get worker :server-file))
          (sent '()))
      (dolist (expr anvil-worker-batch-warmup-expressions)
        (call-process "emacsclient" nil 0 nil
                      "-n" "-f" server-file "-e" expr)
        (push expr sent))
      (anvil-worker--log
       'warmup
       (format "%s sent=%d" (plist-get worker :name) (length sent)))
      (nreverse sent))))

(defun anvil-worker--maybe-schedule-warmup (worker)
  "If WORKER lives on the `:batch' lane, schedule a deferred warmup.
Uses `run-at-time' so the freshly spawned daemon has time to bind
its server socket before we start firing `emacsclient' at it."
  (when (and (eq :batch (plist-get worker :lane))
             anvil-worker-batch-warmup-expressions)
    (run-at-time anvil-worker-batch-warmup-delay nil
                 #'anvil-worker--send-warmup worker)))

(defun anvil-worker--spawn-worker (worker)
  "Spawn WORKER if not already alive.  Returns the start-process or nil.
Batch-lane spawns also schedule a deferred warmup so common
libraries are loaded before the first real dispatch."
  (if (anvil-worker--worker-alive-p worker)
      (progn
        (anvil-worker--log
         'spawn-skipped
         (format "%s already alive" (plist-get worker :name)))
        nil)
    (let* ((init-file (or anvil-worker-init-file
                         (anvil-worker--generate-init-file)))
           (name (plist-get worker :name))
           (proc (start-process
                  (format "anvil-worker-spawn-%s" name)
                  (get-buffer-create (format " *%s*" name))
                  anvil-worker-emacs-bin
                  (concat "--fg-daemon=" name)
                  "-Q"
                  "-l" init-file)))
      (set-process-query-on-exit-flag proc nil)
      (anvil-worker--log
       'spawn
       (format "%s lane=%s pid=%d"
               name
               (anvil-worker--lane-name (plist-get worker :lane))
               (process-id proc)))
      (anvil-worker--maybe-schedule-warmup worker)
      proc)))

(defun anvil-worker--map-pool (fn)
  "Apply FN to every worker plist in the pool, in lane order."
  (dolist (lane anvil-worker--lanes)
    (let ((vec (anvil-worker--lane-pool lane)))
      (when vec
        (dotimes (i (length vec))
          (funcall fn (aref vec i)))))))

;;;###autoload
(defun anvil-worker-spawn ()
  "Spawn every worker in every lane.  Idempotent."
  (interactive)
  (unless anvil-worker--pool
    (anvil-worker--init-pool))
  (anvil-worker--map-pool #'anvil-worker--spawn-worker)
  (message "Anvil worker pool: spawned (read=%d write=%d batch=%d)"
           anvil-worker-read-pool-size
           anvil-worker-write-pool-size
           anvil-worker-batch-pool-size))

;;;###autoload
(defun anvil-worker-kill ()
  "Kill every worker daemon in every lane."
  (interactive)
  (anvil-worker--map-pool
   (lambda (worker)
     (when (anvil-worker--worker-alive-p worker)
       (ignore-errors
         (call-process "emacsclient" nil nil nil
                       "-f" (plist-get worker :server-file)
                       "-e" "(kill-emacs)"))
       (anvil-worker--log 'killed (plist-get worker :name)))))
  (message "Anvil worker pool: all workers killed"))

;;; Dispatch — pick a worker

(defun anvil-worker--pick-in-lane (lane)
  "Pick the next available worker in LANE, or nil if nothing alive.
Round-robin within LANE, preferring non-busy.  Falls back to a
busy-but-alive worker when every lane member is occupied."
  (let* ((vec (anvil-worker--lane-pool lane))
         (size (and vec (length vec)))
         (start (or (plist-get anvil-worker--dispatch-index lane) 0))
         (chosen nil))
    (when (and vec (> size 0))
      ;; Prefer non-busy + alive.
      (dotimes (off size)
        (let* ((idx (% (+ start off) size))
               (worker (aref vec idx)))
          (when (and (not chosen)
                     (not (plist-get worker :busy))
                     (anvil-worker--worker-alive-p worker))
            (setq chosen worker))))
      ;; Fallback: any alive worker (even busy).
      (unless chosen
        (dotimes (off size)
          (let* ((idx (% (+ start off) size))
                 (worker (aref vec idx)))
            (when (and (not chosen)
                       (anvil-worker--worker-alive-p worker))
              (setq chosen worker)))))
      (when chosen
        (setq anvil-worker--dispatch-index
              (plist-put anvil-worker--dispatch-index lane
                         (% (1+ (plist-get chosen :index)) size)))))
    chosen))

(defun anvil-worker--pick-fallback-in-lane (lane)
  "Spawn the first worker in LANE and wait up to `anvil-worker-spawn-wait'.
Returns the worker plist if it came alive, else nil."
  (let ((worker (anvil-worker--worker lane 0)))
    (when worker
      (anvil-worker--spawn-worker worker)
      (let ((deadline (+ (float-time) anvil-worker-spawn-wait)))
        (while (and (not (anvil-worker--worker-alive-p worker))
                    (< (float-time) deadline))
          (sit-for 0.1)))
      (and (anvil-worker--worker-alive-p worker) worker))))

(defun anvil-worker--pick-worker (&optional kind expression)
  "Pick a worker for a tool call.
KIND is `:read' / `:write' / `:batch' / `:auto' (default `:auto').
When KIND is `:auto', EXPRESSION (a string) is fed to
`anvil-worker--classify' to choose the effective lane; the chosen
lane is tried first and the remaining lanes act as fallbacks."
  (unless anvil-worker--pool
    (anvil-worker--init-pool))
  (let* ((kind (or kind :auto))
         (effective (if (eq kind :auto)
                        (anvil-worker--classify expression)
                      kind))
         ;; Try effective lane first, then the rest in canonical order.
         (try-order (cons effective
                          (cl-remove effective anvil-worker--lanes))))
    (or (cl-some #'anvil-worker--pick-in-lane try-order)
        (cl-some #'anvil-worker--pick-fallback-in-lane try-order))))

;;; Heavy-op detection

(defun anvil-worker-detect-heavy (expression)
  "Return list of matched heavy-op labels in EXPRESSION, or nil."
  (when (stringp expression)
    (let (matches)
      (dolist (p anvil-worker-heavy-patterns)
        (when (string-match-p (cdr p) expression)
          (push (car p) matches)))
      (nreverse matches))))

;;; Call worker

(defun anvil-worker--parse-call-args (args)
  "Parse trailing ARGS for `anvil-worker-call'.
Returns a plist `(:kind KIND :timeout TIMEOUT)'.  Accepts both
the legacy positional form `(TIMEOUT)' (number or nil) and the
new keyword form `(:kind ... :timeout ...)'."
  (cond
   ((null args)
    (list :kind :auto :timeout nil))
   ;; Legacy: a single positional value that is nil or a number.
   ((and (= 1 (length args))
         (or (null (car args)) (numberp (car args))))
    (list :kind :auto :timeout (car args)))
   (t
    (let ((kind :auto) (timeout nil))
      (while args
        (let ((k (pop args)))
          (cond
           ((eq k :kind)    (setq kind    (pop args)))
           ((eq k :timeout) (setq timeout (pop args)))
           (t (error "anvil-worker-call: unexpected argument %S" k)))))
      (list :kind kind :timeout timeout)))))

;;;###autoload
(defun anvil-worker-call (expression &rest args)
  "Run EXPRESSION on a worker daemon and return its printed result.

Supported call forms:
  (anvil-worker-call EXPR)                          ; :auto, default timeout
  (anvil-worker-call EXPR TIMEOUT)                  ; legacy positional
  (anvil-worker-call EXPR :kind :read|:write|:batch|:auto
                          :timeout SECONDS)         ; v2 keyword form

Auto-spawns workers if needed.  Signals `error' when no worker in
the requested lane is reachable within `anvil-worker-spawn-wait'."
  (let* ((parsed  (anvil-worker--parse-call-args args))
         (kind    (plist-get parsed :kind))
         (timeout (plist-get parsed :timeout))
         (heavy   (anvil-worker-detect-heavy expression))
         (timeout (or timeout
                      (if heavy anvil-worker-heavy-timeout
                        anvil-worker-call-timeout)))
         (worker  (anvil-worker--pick-worker kind expression)))
    (unless worker
      (error "Anvil: no worker available for kind=%S (read=%d write=%d batch=%d)"
             kind
             anvil-worker-read-pool-size
             anvil-worker-write-pool-size
             anvil-worker-batch-pool-size))
    (let ((server-file (plist-get worker :server-file))
          (name (plist-get worker :name))
          (lane (plist-get worker :lane)))
      (plist-put worker :busy t)
      (anvil-worker--log
       'dispatch
       (format "%s lane=%s kind=%s heavy=%s timeout=%ds"
               name
               (anvil-worker--lane-name lane)
               (anvil-worker--lane-name kind)
               (if heavy (mapconcat #'identity heavy ",") "no")
               timeout))
      (when (eq kind :auto)
        (anvil-worker--log
         'classified
         (format "%s → lane=%s" name (anvil-worker--lane-name lane))))
      (unwind-protect
          (pcase anvil-worker-connection-method
            ('server-eval-at
             (anvil-worker--dispatch-via-server-eval-at
              worker expression timeout))
            ('emacsclient
             (anvil-worker--dispatch-via-emacsclient
              worker expression timeout))
            (m (error "anvil-worker: unknown connection-method %S" m)))
        (plist-put worker :busy nil)))))

(defun anvil-worker--dispatch-via-emacsclient (worker expression timeout)
  "Dispatch EXPRESSION to WORKER by spawning an `emacsclient' process.
Records per-call latency into `anvil-worker--metrics-latency'."
  (let* ((server-file (plist-get worker :server-file))
         (name        (plist-get worker :name))
         (lane        (plist-get worker :lane))
         (buf (get-buffer-create (format " *anvil-worker-call-%s*" name)))
         (t0 (float-time))
         t1 t-end)
    (with-current-buffer buf (erase-buffer))
    (let* ((proc (start-process
                  "anvil-worker-call" buf
                  "emacsclient"
                  "-f" server-file
                  "-e" expression)))
      (setq t1 (float-time))
      (set-process-query-on-exit-flag proc nil)
      (while (and (process-live-p proc)
                  (< (- (float-time) t1) timeout))
        (accept-process-output proc 0.1))
      (setq t-end (float-time))
      (when (process-live-p proc)
        (kill-process proc)
        (error "Anvil worker %s timeout (%ds)" name timeout))
      (anvil-worker--latency-record
       lane
       (* 1000.0 (- t1 t0))
       (* 1000.0 (- t-end t1)))
      (string-trim (with-current-buffer buf (buffer-string))))))

(defun anvil-worker--dispatch-via-server-eval-at (worker expression timeout)
  "Dispatch EXPRESSION to WORKER via `server-eval-at' (no external process).
Records per-call latency; the `:spawn-ms' bucket stays at 0 since no
child process is created, making the emacsclient vs server-eval-at
comparison visible in `anvil-worker-latency-metrics-show'."
  (let* ((name (plist-get worker :name))
         (lane (plist-get worker :lane))
         (t0 (float-time))
         (form (read expression))
         result t-end)
    (with-timeout
        (timeout (error "Anvil worker %s timeout (%ds)" name timeout))
      (condition-case err
          (setq result (prin1-to-string (server-eval-at name form)))
        (error
         (setq result
               (format "*ERROR*: %s" (error-message-string err))))))
    (setq t-end (float-time))
    (anvil-worker--latency-record
     lane 0.0 (* 1000.0 (- t-end t0)))
    result))

;;; Pool status

;;;###autoload
(defun anvil-worker-status ()
  "Display per-lane pool status."
  (interactive)
  (unless anvil-worker--pool
    (anvil-worker--init-pool))
  (let ((lines '()))
    (dolist (lane anvil-worker--lanes)
      (let ((vec (anvil-worker--lane-pool lane)))
        (when (and vec (> (length vec) 0))
          (push (format "[%s]" (anvil-worker--lane-name lane)) lines)
          (dotimes (i (length vec))
            (let* ((worker (aref vec i))
                   (alive (anvil-worker--worker-alive-p worker))
                   (busy  (plist-get worker :busy)))
              (push (format "  %s: %s%s"
                            (plist-get worker :name)
                            (if alive "alive" "dead")
                            (if busy " [busy]" ""))
                    lines))))))
    (message "Anvil worker pool (read=%d write=%d batch=%d):\n%s"
             anvil-worker-read-pool-size
             anvil-worker-write-pool-size
             anvil-worker-batch-pool-size
             (mapconcat #'identity (nreverse lines) "\n"))))

;;; Health check

(defun anvil-worker--health-check-one (worker)
  "Inspect WORKER and respawn it if it transitioned to dead."
  (let ((alive (anvil-worker--worker-alive-p worker))
        (last  (plist-get worker :last-state))
        (name  (plist-get worker :name))
        (lane  (anvil-worker--lane-name (plist-get worker :lane))))
    (cond
     ((null last)
      (plist-put worker :last-state (if alive 'alive 'dead))
      (unless alive
        (anvil-worker--log
         'startup-dead
         (format "%s lane=%s respawning" name lane))
        (anvil-worker--spawn-worker worker)))
     ((and (eq last 'alive) (not alive))
      (anvil-worker--log
       'death (format "%s lane=%s respawning" name lane))
      (plist-put worker :last-state 'dead)
      (anvil-worker--spawn-worker worker))
     ((and (eq last 'dead) alive)
      (anvil-worker--log 'recovered (format "%s lane=%s" name lane))
      (plist-put worker :last-state 'alive)))))

(defun anvil-worker--health-check ()
  "Sweep every lane and respawn dead workers.  Logs only transitions."
  (unless anvil-worker--pool
    (anvil-worker--init-pool))
  (anvil-worker--map-pool #'anvil-worker--health-check-one))

;;;###autoload
(defun anvil-worker-health-timer-start ()
  "Start periodic pool health checks with auto-respawn."
  (interactive)
  (when (timerp anvil-worker--health-timer)
    (cancel-timer anvil-worker--health-timer))
  (setq anvil-worker--health-timer
        (run-with-timer anvil-worker-health-check-interval
                        anvil-worker-health-check-interval
                        #'anvil-worker--health-check))
  (anvil-worker--log
   'timer-started
   (format "interval=%ds read=%d write=%d batch=%d"
           anvil-worker-health-check-interval
           anvil-worker-read-pool-size
           anvil-worker-write-pool-size
           anvil-worker-batch-pool-size))
  (message "Anvil worker health timer started (%ds)"
           anvil-worker-health-check-interval))

(defun anvil-worker-health-timer-stop ()
  "Stop periodic pool health checks."
  (interactive)
  (when (timerp anvil-worker--health-timer)
    (cancel-timer anvil-worker--health-timer)
    (setq anvil-worker--health-timer nil)
    (anvil-worker--log 'timer-stopped)
    (message "Anvil worker health timer stopped")))

;;; Module enable/disable

(defun anvil-worker-enable ()
  "Initialise every lane, spawn workers, start health monitoring."
  (anvil-worker--init-pool)
  (anvil-worker-spawn)
  (anvil-worker-health-timer-start))

(defun anvil-worker-disable ()
  "Stop health monitoring (does not kill running workers)."
  (anvil-worker-health-timer-stop))

(provide 'anvil-worker)
;;; anvil-worker.el ends here
