;;; anvil-browser.el --- Browser automation for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; MCP tool wrapper around the `agent-browser' CLI
;; (https://agent-browser.dev/, `npm install -g agent-browser').
;;
;; agent-browser ships an accessibility-tree + ref interface that
;; compresses web page tokens by up to 25x for LLM consumption.  This
;; module folds the usual four-step Claude workflow (open, snapshot,
;; click, get) into a single MCP tool call so the client does not pay
;; bash round-trip overhead between each step.
;;
;; Design doc: docs/design/07-browser-framework.org (Phase A).
;;
;; Transport: each tool call spawns one `agent-browser batch --json'
;; subprocess via `make-process' with the command list piped on
;; stdin.  This avoids shell quoting issues for URLs / JavaScript and
;; keeps the Emacs server unblocked (accept-process-output yields).
;;
;; Public MCP tools (all under server-id "emacs-eval"):
;;   browser-fetch       url [selector] [session]
;;   browser-interact    url actions-json [session]
;;   browser-capture     url [title] [tags] [session]
;;   browser-screenshot  url [region] [session]
;;   browser-close
;;
;; Cache (in-memory) de-duplicates same-URL fetches within
;; `anvil-browser-cache-ttl-sec' seconds; set to 0 to disable.  The
;; cache is per-Emacs-process and does not persist; Phase B extracts
;; it into anvil-state for cross-session reuse.
;;
;; Hard runtime dependency: the agent-browser CLI must be on
;; `exec-path'.  The module loads without it; calls error clearly.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'anvil-server)
(require 'anvil-state)

;;;; --- configuration ------------------------------------------------------

(defgroup anvil-browser nil
  "Browser automation MCP tools for anvil (via agent-browser CLI)."
  :group 'anvil
  :prefix "anvil-browser-")

(defconst anvil-browser--server-id "emacs-eval"
  "MCP server id that anvil-browser tools register under.
Shared with anvil-file / anvil-org / anvil-sqlite so the client
sees one unified tool list.")

(defcustom anvil-browser-cli "agent-browser"
  "Name (or absolute path) of the agent-browser CLI.
Resolved via `executable-find' when it is not absolute."
  :type 'string
  :group 'anvil-browser)

(defcustom anvil-browser-session-name "anvil-default"
  "Default value for the CLI's --session-name flag.
agent-browser auto-saves cookies and localStorage under this name
so repeated calls share login state.  Callers can override per
tool invocation via the `session' argument."
  :type 'string
  :group 'anvil-browser)

(defcustom anvil-browser-cache-ttl-sec 300
  "Return a cached `browser-fetch' snapshot for the same URL within
this many seconds.  Set to 0 to disable caching entirely."
  :type 'integer
  :group 'anvil-browser)

(defcustom anvil-browser-timeout-sec 60
  "Per-call timeout (seconds) for spawning the agent-browser CLI.
Covers Chrome cold-start plus the full batch of commands."
  :type 'integer
  :group 'anvil-browser)

(defcustom anvil-browser-metrics-log-size 20
  "Number of recent fetch entries kept in `anvil-browser--metrics'.
Surfaced by `anvil-browser-status'."
  :type 'integer
  :group 'anvil-browser)

(defcustom anvil-browser-capture-dir
  (expand-file-name "capture/web" "~/Cowork/Notes")
  "Default directory for `browser-capture' output.
Override per-project for non-Cowork users."
  :type 'directory
  :group 'anvil-browser)

(defcustom anvil-browser-capture-template
  "#+TITLE: %TITLE%
#+DATE: %DATE%
#+PROPERTY: TYPE web_summary
#+PROPERTY: URL %URL%
#+FILETAGS: %TAGS%

%CONTENT%
"
  "Template used by `browser-capture' when generating the captured file.

Either a STRING with %TITLE%, %DATE%, %URL%, %TAGS%, %CONTENT%
placeholders, or a FUNCTION of one plist argument (same keys as
keywords: :title :date :url :tags :content) that returns the
rendered string.  The dual-type pattern mirrors
`org-capture-templates'."
  :type '(choice string function)
  :group 'anvil-browser)

;;;; --- Phase A' auth primitives (login wall bypass) ----------------------

(defcustom anvil-browser-user-agent nil
  "Default value for agent-browser's --user-agent flag.
String or nil (no UA override).  Useful against UA-sniffing
blocks; not sufficient on its own for modern JS challenges."
  :type '(choice (const :tag "Don't override" nil) string)
  :group 'anvil-browser)

(defcustom anvil-browser-profile nil
  "Default value for agent-browser's --profile flag.
String or nil.  A Chrome profile name (e.g. \"Default\") reuses
an already-authenticated profile and is the primary mechanism
for accessing login-walled sites (X, Reddit, private web UIs).
An absolute directory path creates / reuses a persistent custom
profile."
  :type '(choice (const :tag "Fresh profile" nil) string)
  :group 'anvil-browser)

(defcustom anvil-browser-auto-connect nil
  "When non-nil, pass --auto-connect to agent-browser so it
attaches to a running Chrome instance instead of spawning a
fresh one.  Cookies / state are inherited with no profile-lock
contention — the simplest path when the user already has Chrome
open and logged in."
  :type 'boolean
  :group 'anvil-browser)

(defcustom anvil-browser-session-presets nil
  "Alist of named auth presets for `browser-*' tools.

Each entry is (NAME . PLIST) where PLIST may contain any of
:profile, :user-agent, :auto-connect, :session-name.  When a
tool is called with the matching NAME, the preset values
override the corresponding global defcustoms for that call.

Example:
  \\='((\"reddit\" :profile \"Default\")
    (\"x\"      :auto-connect t)
    (\"bot\"    :user-agent \"Mozilla/5.0 (compatible; anvil-bot)\"))"
  :type '(alist :key-type string
                :value-type (plist :value-type sexp))
  :group 'anvil-browser)

;;;; --- internal state -----------------------------------------------------

(defconst anvil-browser--state-ns "browser"
  "Namespace used when storing fetched snapshots in `anvil-state'.")

(defvar anvil-browser--metrics
  (list :fetches 0 :cache-hits 0 :errors 0 :log nil)
  "Rolling counters and recent-fetch log.
:log is a list of (:url URL :elapsed-ms N :cached BOOL :at FLOAT-TIME).")

;;;; --- internal helpers ---------------------------------------------------

(defun anvil-browser--cli-path ()
  "Return an absolute path to the agent-browser CLI or signal an error."
  (or (and (file-name-absolute-p anvil-browser-cli)
           (file-executable-p anvil-browser-cli)
           anvil-browser-cli)
      (executable-find anvil-browser-cli)
      (user-error
       "anvil-browser: '%s' not found on exec-path — install via 'npm install -g agent-browser'"
       anvil-browser-cli)))

(defun anvil-browser--resolve-auth-config (preset)
  "Resolve auth config plist for PRESET, falling back to defcustoms.
PRESET is a string name that must exist in
`anvil-browser-session-presets' (when non-nil / non-empty).
Returns a plist with :profile :user-agent :auto-connect
:session-name keys, each either a string/boolean or nil.
Signals `user-error' on unknown preset name."
  (let* ((preset-plist
          (cond
           ((or (null preset) (and (stringp preset)
                                   (string-empty-p preset)))
            nil)
           ((stringp preset)
            (let ((entry (assoc preset anvil-browser-session-presets)))
              (unless entry
                (user-error
                 "anvil-browser: unknown preset %S (known: %S)"
                 preset
                 (mapcar #'car anvil-browser-session-presets)))
              (cdr entry)))
           (t (user-error
               "anvil-browser: preset must be a string or nil, got %S"
               preset))))
         (resolve (lambda (key default)
                    (if (plist-member preset-plist key)
                        (plist-get preset-plist key)
                      default))))
    (list :profile      (funcall resolve :profile      anvil-browser-profile)
          :user-agent   (funcall resolve :user-agent   anvil-browser-user-agent)
          :auto-connect (funcall resolve :auto-connect anvil-browser-auto-connect)
          :session-name (funcall resolve :session-name nil))))

(defun anvil-browser--build-cli-args (auth session-override)
  "Compose the agent-browser CLI args for AUTH + SESSION-OVERRIDE.
AUTH is the plist returned by `anvil-browser--resolve-auth-config'.
SESSION-OVERRIDE is the tool's `session' argument (highest
priority).  Returns a list of strings ending with the
batch-mode terminator \\='(\"batch\" \"--json\") so the caller
can send JSON on stdin."
  (let* ((session-name (or session-override
                           (plist-get auth :session-name)
                           anvil-browser-session-name))
         (ua      (plist-get auth :user-agent))
         (profile (plist-get auth :profile))
         (auto    (plist-get auth :auto-connect))
         (args    (list "--session-name" session-name)))
    (when (and (stringp ua) (not (string-empty-p ua)))
      (setq args (append args (list "--user-agent" ua))))
    (when (and (stringp profile) (not (string-empty-p profile)))
      (setq args (append args (list "--profile" profile))))
    (when auto
      (setq args (append args (list "--auto-connect"))))
    (append args (list "batch" "--json"))))

(defun anvil-browser--cache-key (url selector &optional auth)
  "Build the `anvil-state' key for URL + SELECTOR (+ optional AUTH).
Serialized as `URL\\0SELECTOR\\0PROFILE\\0UA' so an empty
selector produces a distinct key from any non-empty one, and
authenticated fetches (non-nil profile / UA) do not collide with
anonymous ones.  Session-name is intentionally excluded — it
governs cookie persistence but two sessions with the same
profile should hit the same cache entry."
  (format "%s\0%s\0%s\0%s"
          url
          (or selector "")
          (or (plist-get auth :profile) "")
          (or (plist-get auth :user-agent) "")))

(defun anvil-browser--cache-get (url selector &optional auth)
  "Return the cached snapshot text for URL+SELECTOR (+ AUTH) or nil.
Looks the entry up in `anvil-state' under the \"browser\"
namespace.  TTL is enforced by `anvil-state' via lazy expiry."
  (when (and (numberp anvil-browser-cache-ttl-sec)
             (> anvil-browser-cache-ttl-sec 0))
    (anvil-state-get (anvil-browser--cache-key url selector auth)
                     :ns anvil-browser--state-ns)))

(defun anvil-browser--cache-put (url selector snapshot &optional auth)
  "Store SNAPSHOT text for URL+SELECTOR (+ AUTH) with the configured TTL.
No-op when caching is disabled."
  (when (and (numberp anvil-browser-cache-ttl-sec)
             (> anvil-browser-cache-ttl-sec 0))
    (anvil-state-set (anvil-browser--cache-key url selector auth)
                     snapshot
                     :ns anvil-browser--state-ns
                     :ttl anvil-browser-cache-ttl-sec)))

(defun anvil-browser--cache-clear ()
  "Delete every cached browser snapshot from `anvil-state'."
  (condition-case _err
      (anvil-state-delete-ns anvil-browser--state-ns)
    (error 0)))

(defun anvil-browser--cache-count ()
  "Return the live cached-entry count under the browser namespace."
  (condition-case _err
      (anvil-state-count anvil-browser--state-ns)
    (error 0)))

(defun anvil-browser--metrics-bump (key &optional delta)
  "Increment the counter at KEY in `anvil-browser--metrics' by DELTA (default 1)."
  (let ((n (or (plist-get anvil-browser--metrics key) 0)))
    (setq anvil-browser--metrics
          (plist-put anvil-browser--metrics key (+ n (or delta 1))))))

(defun anvil-browser--metrics-log (url elapsed-ms cached)
  "Append a recent-fetch entry for URL with ELAPSED-MS and CACHED flag.
Trims the log to `anvil-browser-metrics-log-size' entries."
  (let* ((log (plist-get anvil-browser--metrics :log))
         (entry (list :url url :elapsed-ms elapsed-ms
                      :cached (and cached t) :at (float-time)))
         (updated (cons entry log)))
    (when (> (length updated) anvil-browser-metrics-log-size)
      (setq updated (cl-subseq updated 0 anvil-browser-metrics-log-size)))
    (setq anvil-browser--metrics
          (plist-put anvil-browser--metrics :log updated))))

(defun anvil-browser--json-encode-commands (commands)
  "Encode COMMANDS (list of string lists) as a JSON array of arrays.
Each inner list is (CMD ARG...) and becomes one agent-browser
batch command."
  (let ((json-false :json-false)
        (json-null nil))
    (json-encode
     (mapcar (lambda (cmd)
               (unless (and (listp cmd)
                            (cl-every #'stringp cmd))
                 (error "anvil-browser: command must be a list of strings: %S" cmd))
               (vconcat cmd))
             commands))))

(defun anvil-browser--strip-sentinel-noise (text)
  "Strip Emacs' default \"Process ... finished/exited\" lines from TEXT.
`make-process' injects these into the stdout / stderr buffer even
when we pass `:sentinel #\\='ignore'; they corrupt downstream
JSON parsing."
  (replace-regexp-in-string
   "\n*Process [^\n]* \\(finished\\|exited[^\n]*\\)\n*\\'"
   "" text))

(defun anvil-browser--run-batch (commands &optional session auth)
  "Spawn agent-browser with COMMANDS piped as JSON on stdin.

COMMANDS is a list of string lists, e.g.
  ((\"open\" \"https://example.com\") (\"snapshot\" \"-i\" \"-c\")).

SESSION overrides `anvil-browser-session-name'.  AUTH is the
resolved auth-config plist from `anvil-browser--resolve-auth-config'
(nil ⇒ no preset, fall back to defcustoms only).

Returns the parsed JSON output (a list of per-command result
plists).  Non-zero exit is tolerated when the CLI still produced
a parseable JSON array — the batch may have had some commands
fail while others returned useful output (e.g. an `open' blocked
by the browser but a usable error snapshot).  Callers check
per-entry `:success' via `anvil-browser--check-all-success' when
they need strict behavior.  Errors on timeout or when no parseable
JSON comes back."
  (let* ((cli (anvil-browser--cli-path))
         (auth-or-default (or auth (anvil-browser--resolve-auth-config nil)))
         (args (anvil-browser--build-cli-args auth-or-default session))
         (payload (anvil-browser--json-encode-commands commands))
         (stdout-buf (generate-new-buffer " *anvil-browser-stdout*"))
         (stderr-buf (generate-new-buffer " *anvil-browser-stderr*"))
         (process-coding-system-alist
          (cons (cons "" (cons 'utf-8 'utf-8))
                process-coding-system-alist))
         proc)
    (unwind-protect
        (progn
          (setq proc
                (make-process
                 :name "anvil-browser"
                 :buffer stdout-buf
                 :stderr stderr-buf
                 :noquery t
                 :connection-type 'pipe
                 :coding 'utf-8
                 :sentinel #'ignore
                 :command (cons cli args)))
          (process-send-string proc payload)
          (process-send-eof proc)
          (let ((deadline (+ (float-time) anvil-browser-timeout-sec)))
            (while (and (process-live-p proc)
                        (< (float-time) deadline))
              (accept-process-output proc 0.1))
            (when (process-live-p proc)
              (delete-process proc)
              (error "anvil-browser: timeout after %ss (commands=%d)"
                     anvil-browser-timeout-sec (length commands))))
          (let* ((exit (process-exit-status proc))
                 (stdout (anvil-browser--strip-sentinel-noise
                          (with-current-buffer stdout-buf (buffer-string))))
                 (stderr (anvil-browser--strip-sentinel-noise
                          (with-current-buffer stderr-buf (buffer-string)))))
            (condition-case err
                (json-parse-string stdout
                                   :object-type 'plist
                                   :array-type 'list
                                   :null-object nil
                                   :false-object nil)
              (json-parse-error
               (error "anvil-browser: exit %s, could not parse CLI output: %s — stderr=%s stdout=%s"
                      exit (error-message-string err)
                      (string-trim stderr)
                      (string-trim stdout))))))
      (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf))
      (when (buffer-live-p stderr-buf) (kill-buffer stderr-buf)))))

(defun anvil-browser--last-snapshot (results)
  "Return the snapshot text from the last snapshot-style result in RESULTS.
Looks for the rightmost entry whose `:result' plist has a
`:snapshot' key.  Returns nil when none is present."
  (let ((found nil))
    (dolist (r results)
      (let* ((result (plist-get r :result))
             (snap (and (listp result) (plist-get result :snapshot))))
        (when snap (setq found snap))))
    found))

(defun anvil-browser--check-all-success (results)
  "Error if any entry in RESULTS reports `:success' nil.
Returns RESULTS when all succeeded."
  (dolist (r results)
    (unless (plist-get r :success)
      (error "anvil-browser: command %S failed: %s"
             (plist-get r :command)
             (or (plist-get r :error) "(no error message)"))))
  results)

;;;; --- template rendering -------------------------------------------------

(defun anvil-browser--subst-template (template plist)
  "Substitute %TITLE% / %DATE% / %URL% / %TAGS% / %CONTENT% in TEMPLATE.
PLIST supplies the values under the corresponding keywords.
Unknown placeholders are left verbatim."
  (let ((out template))
    (dolist (cell '((:title . "%TITLE%") (:date . "%DATE%")
                    (:url . "%URL%") (:tags . "%TAGS%")
                    (:content . "%CONTENT%")))
      (setq out (replace-regexp-in-string
                 (regexp-quote (cdr cell))
                 (or (plist-get plist (car cell)) "")
                 out t t)))
    out))

(defun anvil-browser--render-capture (plist)
  "Render `anvil-browser-capture-template' against PLIST.
Accepts either a string template or a function of one argument."
  (let ((tmpl anvil-browser-capture-template))
    (cond
     ((functionp tmpl) (funcall tmpl plist))
     ((stringp tmpl)   (anvil-browser--subst-template tmpl plist))
     (t (error "anvil-browser: invalid capture template type: %S" (type-of tmpl))))))

(defun anvil-browser--capture-filename (dir)
  "Return a new capture file path under DIR using web_YYYYMMDD_NN.org.
Increments the counter until an unused path is found."
  (unless (file-directory-p dir) (make-directory dir t))
  (let* ((date (format-time-string "%Y%m%d"))
         (n 1)
         path)
    (while (progn
             (setq path (expand-file-name
                         (format "web_%s_%02d.org" date n) dir))
             (file-exists-p path))
      (setq n (1+ n)))
    path))

(defun anvil-browser--normalize-tags (tags)
  "Normalize TAGS (string or list) into an org :tag1:tag2: style string.
Empty input returns the empty string."
  (let ((items (cond
                ((null tags) nil)
                ((listp tags) tags)
                ((stringp tags)
                 (split-string tags "[[:space:],]+" t)))))
    (if items
        (concat ":" (mapconcat #'identity items ":") ":")
      "")))

;;;; --- MCP tool handlers --------------------------------------------------

(defun anvil-browser--tool-fetch (url &optional selector session preset)
  "Fetch URL and return a compact accessibility-tree snapshot.

MCP Parameters:
  url      - Absolute URL to open (string).
  selector - Optional CSS selector to scope the snapshot to a
             subtree (e.g. \"main article\").  Empty string is
             treated as no selector.
  session  - Optional --session-name override for persisting
             cookies / localStorage separately from the default
             session.
  preset   - Optional auth preset name from
             `anvil-browser-session-presets'.  Overrides the
             global profile / user-agent / auto-connect
             defcustoms for this call.

Returns the accessibility tree text produced by
`agent-browser snapshot -i -c'.  Same-URL calls within
`anvil-browser-cache-ttl-sec' seconds are served from the
in-memory cache.

Token cost is typically 25x smaller than a raw DOM dump."
  (anvil-server-with-error-handling
   (let* ((sel (and (stringp selector) (not (string-empty-p selector)) selector))
          (auth (anvil-browser--resolve-auth-config preset))
          (cached (anvil-browser--cache-get url sel auth))
          (start (float-time)))
     (anvil-browser--metrics-bump :fetches)
     (if cached
         (progn
           (anvil-browser--metrics-bump :cache-hits)
           (anvil-browser--metrics-log url 0 t)
           cached)
       (condition-case err
           (let* ((snap-cmd (append '("snapshot" "-i" "-c")
                                    (and sel (list "-s" sel))))
                  (results (anvil-browser--check-all-success
                            (anvil-browser--run-batch
                             (list (list "open" url) snap-cmd)
                             session auth)))
                  (text (or (anvil-browser--last-snapshot results)
                            (error "anvil-browser: no snapshot in batch output"))))
             (anvil-browser--cache-put url sel text auth)
             (anvil-browser--metrics-log
              url (round (* 1000 (- (float-time) start))) nil)
             text)
         (error
          (anvil-browser--metrics-bump :errors)
          (signal (car err) (cdr err))))))))

(defun anvil-browser--parse-actions (actions-json)
  "Parse ACTIONS-JSON (string) into a list of string-lists.
Each entry is one agent-browser batch command, e.g.
  [[\"click\", \"@e1\"], [\"fill\", \"@e2\", \"hello\"]]"
  (unless (and (stringp actions-json) (not (string-empty-p actions-json)))
    (user-error "anvil-browser: actions must be a non-empty JSON array string"))
  (let* ((parsed (condition-case err
                     (json-parse-string actions-json
                                        :array-type 'list
                                        :null-object nil
                                        :false-object nil)
                   (json-parse-error
                    (user-error "anvil-browser: invalid actions JSON: %s"
                                (error-message-string err))))))
    (unless (listp parsed)
      (user-error "anvil-browser: actions JSON must be an array, got %s"
                  (type-of parsed)))
    (mapcar (lambda (cmd)
              (unless (and (listp cmd) (cl-every #'stringp cmd))
                (user-error
                 "anvil-browser: each action must be an array of strings: %S"
                 cmd))
              cmd)
            parsed)))

(defun anvil-browser--tool-interact (url actions &optional session preset)
  "Open URL, run each action in ACTIONS, and return the final snapshot.

MCP Parameters:
  url     - Absolute URL to open.
  actions - JSON array of command arrays, e.g.
            '[[\"click\",\"@e1\"],[\"fill\",\"@e2\",\"hi\"]]'.
            Commands match the agent-browser CLI verbs
            (click / fill / press / eval / etc.).
  session - Optional --session-name override.
  preset  - Optional auth preset name from
            `anvil-browser-session-presets'.

The implementation prepends an `open' and appends a final
`snapshot -i -c' so the return value is always the post-action
tree.  All actions share one Chrome session to preserve DOM state."
  (anvil-server-with-error-handling
   (let* ((parsed (anvil-browser--parse-actions actions))
          (batch (append (list (list "open" url))
                         parsed
                         (list '("snapshot" "-i" "-c"))))
          (auth (anvil-browser--resolve-auth-config preset))
          (results (anvil-browser--check-all-success
                    (anvil-browser--run-batch batch session auth))))
     (or (anvil-browser--last-snapshot results)
         (error "anvil-browser: interact produced no snapshot")))))

(defun anvil-browser--tool-capture (url &optional title tags session preset)
  "Fetch URL, render `anvil-browser-capture-template', and save to
`anvil-browser-capture-dir'.

MCP Parameters:
  url     - Absolute URL to fetch and archive.
  title   - Optional title for the captured org file.  Falls back
            to the URL when empty.
  tags    - Optional tag string; either space/comma-separated
            words (\"ai automation\") or a ready-made org tag
            string (\":ai:automation:\").
  session - Optional --session-name override.
  preset  - Optional auth preset name from
            `anvil-browser-session-presets'.

Returns the absolute path of the saved file."
  (anvil-server-with-error-handling
   (let* ((snapshot (anvil-browser--tool-fetch url nil session preset))
          (resolved-title (if (and (stringp title)
                                   (not (string-empty-p title)))
                              title
                            url))
          (norm-tags (anvil-browser--normalize-tags tags))
          (plist (list :title resolved-title
                       :date (format-time-string "<%Y-%m-%d %a>")
                       :url url
                       :tags norm-tags
                       :content snapshot))
          (rendered (anvil-browser--render-capture plist))
          (path (anvil-browser--capture-filename anvil-browser-capture-dir)))
     (let ((coding-system-for-write 'utf-8-unix))
       (with-temp-buffer
         (insert rendered)
         (write-region (point-min) (point-max) path nil 'silent)))
     path)))

(defun anvil-browser--tool-screenshot (url &optional region session preset)
  "Open URL and save a screenshot to a temp PNG file.

MCP Parameters:
  url     - Absolute URL to open.
  region  - Optional CSS selector; when provided the screenshot
            is scoped to that element (equivalent to
            `agent-browser screenshot SELECTOR PATH').
  session - Optional --session-name override.
  preset  - Optional auth preset name from
            `anvil-browser-session-presets'.

Returns the absolute path of the PNG file.  Caller owns cleanup."
  (anvil-server-with-error-handling
   (let* ((out (make-temp-file "anvil-browser-" nil ".png"))
          (sel (and (stringp region)
                    (not (string-empty-p region))
                    region))
          (shot-cmd (if sel
                        (list "screenshot" sel out)
                      (list "screenshot" out)))
          (auth (anvil-browser--resolve-auth-config preset))
          (results (anvil-browser--check-all-success
                    (anvil-browser--run-batch
                     (list (list "open" url) shot-cmd)
                     session auth))))
     (ignore results)
     out)))

(defun anvil-browser--tool-close ()
  "Close every live agent-browser session.

Delegates to `agent-browser close --all'.  Safe to call when no
sessions exist; the CLI is a no-op in that case.  Also clears the
in-memory fetch cache."
  (anvil-server-with-error-handling
   (let* ((cli (anvil-browser--cli-path))
          (stdout-buf (generate-new-buffer " *anvil-browser-close*"))
          proc)
     (unwind-protect
         (progn
           (setq proc
                 (make-process
                  :name "anvil-browser-close"
                  :buffer stdout-buf
                  :noquery t
                  :sentinel #'ignore
                  :command (list cli "close" "--all")))
           (let ((deadline (+ (float-time) anvil-browser-timeout-sec)))
             (while (and (process-live-p proc)
                         (< (float-time) deadline))
               (accept-process-output proc 0.1))
             (when (process-live-p proc)
               (delete-process proc)))
           (let ((cleared (anvil-browser--cache-clear)))
             (format "%S"
                     (list :ok t
                           :exit (process-exit-status proc)
                           :cache-cleared cleared))))
       (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf))))))

;;;; --- status / introspection ---------------------------------------------

;;;###autoload
(defun anvil-browser-status ()
  "Show fetch counters and recent-fetch log in *Anvil Browser*."
  (interactive)
  (with-help-window "*Anvil Browser*"
    (princ (format "Anvil Browser — agent-browser wrapper\n"))
    (princ (format "CLI       : %s\n"
                   (or (ignore-errors (anvil-browser--cli-path))
                       "(not found)")))
    (princ (format "Session   : %s\n" anvil-browser-session-name))
    (princ (format "Profile   : %s\n" (or anvil-browser-profile "(none)")))
    (princ (format "User-Agent: %s\n" (or anvil-browser-user-agent "(none)")))
    (princ (format "AutoConn  : %s\n" (if anvil-browser-auto-connect "on" "off")))
    (princ (format "Presets   : %s\n"
                   (or (mapconcat #'car anvil-browser-session-presets ", ")
                       "(none)")))
    (princ (format "Cache TTL : %s sec  (entries: %d)\n"
                   anvil-browser-cache-ttl-sec
                   (anvil-browser--cache-count)))
    (princ (format "Fetches   : %d   cache-hits: %d   errors: %d\n"
                   (or (plist-get anvil-browser--metrics :fetches) 0)
                   (or (plist-get anvil-browser--metrics :cache-hits) 0)
                   (or (plist-get anvil-browser--metrics :errors) 0)))
    (princ "\nRecent fetches:\n")
    (dolist (entry (plist-get anvil-browser--metrics :log))
      (princ (format "  %5dms %s  %s\n"
                     (or (plist-get entry :elapsed-ms) 0)
                     (if (plist-get entry :cached) "HIT " "MISS")
                     (plist-get entry :url))))))

;;;; --- module lifecycle ---------------------------------------------------

(defun anvil-browser--register-tools ()
  "Register all browser-* MCP tools under `anvil-browser--server-id'."
  (anvil-server-register-tool
   #'anvil-browser--tool-fetch
   :id "browser-fetch"
   :server-id anvil-browser--server-id
   :description
   "Open a URL and return a compact accessibility-tree snapshot
via agent-browser.  Typically 25x cheaper in tokens than a raw
DOM dump.  Same-URL repeats within the cache TTL are served
without relaunching Chrome."
   :read-only t)

  (anvil-server-register-tool
   #'anvil-browser--tool-interact
   :id "browser-interact"
   :server-id anvil-browser--server-id
   :description
   "Open a URL and run a JSON array of agent-browser commands
(click / fill / press / eval / …) in a single Chrome session,
returning the final post-action accessibility tree.")

  (anvil-server-register-tool
   #'anvil-browser--tool-capture
   :id "browser-capture"
   :server-id anvil-browser--server-id
   :description
   "Fetch a URL and save it as an org file under
`anvil-browser-capture-dir' using `anvil-browser-capture-template'.
Returns the absolute path of the new file.")

  (anvil-server-register-tool
   #'anvil-browser--tool-screenshot
   :id "browser-screenshot"
   :server-id anvil-browser--server-id
   :description
   "Open a URL and save a PNG screenshot (optionally scoped to a
CSS selector) to a temp file.  Returns the file path; the caller
owns cleanup.")

  (anvil-server-register-tool
   #'anvil-browser--tool-close
   :id "browser-close"
   :server-id anvil-browser--server-id
   :description
   "Close every live agent-browser session and clear the in-memory
fetch cache.  Safe when no sessions exist."))

(defun anvil-browser--unregister-tools ()
  "Remove every browser-* MCP tool from the shared server."
  (dolist (id '("browser-fetch" "browser-interact" "browser-capture"
                "browser-screenshot" "browser-close"))
    (anvil-server-unregister-tool id anvil-browser--server-id)))

;;;###autoload
(defun anvil-browser-enable ()
  "Register browser-* MCP tools.  Does not spawn a browser session.
The first call to `browser-fetch' (or kin) lazily launches
Chrome via agent-browser.  The snapshot cache is backed by
`anvil-state', which is opened here."
  (interactive)
  (anvil-state-enable)
  (anvil-browser--register-tools))

(defun anvil-browser-disable ()
  "Unregister browser-* MCP tools and close any live sessions."
  (interactive)
  (ignore-errors (anvil-browser--tool-close))
  (anvil-browser--unregister-tools))

(provide 'anvil-browser)
;;; anvil-browser.el ends here
