# agent-ide-inline Implementation Plan

> **Superseded 2026-08-23:** the v1 plan below implemented a gptel-rewrite
> style feature. After review, the feature was reworked to gptel-inline
> style interaction (see `docs/superpowers/specs/2026-08-23-agent-ide-inline-design.md`
> v2). The hook wiring in Task 1 remains valid and unchanged; Tasks 2-3
> were redone under TDD in the same files.

---

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add gptel-inline-style in-place region editing to agent-ide, backed by the existing ACP session (pi via pi-acp).

**Architecture:** New consumer file `agent-ide-inline.el` + three additive micro-edits: a chunk hook in `agent-ide-transcript.el`, prompt response/failure hooks in `agent-ide-protocol.el`, and a `require` in `agent-ide.el`. Preview state lives in a global alist `agent-ide-inline--previews` keyed by session (hooks fire in the session buffer, so state must not be buffer-local to the edited buffer).

**Tech Stack:** Emacs Lisp ≥ 29.1, ERT, acp.el, cl-lib, subr-x.

**Spec:** `docs/superpowers/specs/2026-08-23-agent-ide-inline-design.md`

## Global Constraints

- Emacs ≥ 29.1; no new package dependencies.
- All edits to existing files are additive; existing behavior unchanged.
- Lexical binding; docstrings on every defun/defcustom/defvar.
- Prefix all new symbols with `agent-ide-inline-`.
- Tests: ERT, `cl-letf` stubs, fake sessions via `agent-ide--make-session` (see `agent-ide-submit-test.el`).
- Test runner: `emacs -batch -L . -l agent-ide-inline-test.el -f ert-run-tests-batch-and-exit` with elpa load paths appended.

---

### Task 1: Extension hooks (transcript chunk + prompt response/failure)

**Files:**
- Modify: `agent-ide-transcript.el` (~line 670-690, `agent-ide-transcript-handle-notification`)
- Modify: `agent-ide-protocol.el` (`agent-ide-protocol-send-prompt`)
- Test: `agent-ide-inline-test.el` (create)

**Interfaces:**
- Produces:
  - `(defvar agent-ide-message-chunk-functions nil)` — hook called as `(FUNCTION SESSION TEXT)` after each agent message chunk is rendered.
  - `(defvar agent-ide-prompt-response-functions nil)` — hook called as `(FUNCTION SESSION RESPONSE)` at the end of `agent-ide-protocol-send-prompt`'s `:on-success` handler.
  - `(defvar agent-ide-prompt-failure-functions nil)` — hook called as `(FUNCTION SESSION ERROR)` at the end of the `:on-failure` handler.

- [ ] **Step 1: Write the failing test**

Create `agent-ide-inline-test.el`:

```elisp
;;; agent-ide-inline-test.el --- Tests for agent-ide-inline -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-ide-session)

(defun agent-ide-inline-test--session ()
  (agent-ide--make-session
   :directory default-directory
   :buffer (current-buffer)
   :tool-calls (make-hash-table :test 'equal)
   :prompt-history nil
   :acp-session-id "test-session"))

(ert-deftest agent-ide-inline-chunk-hook-fires ()
  "Transcript handler runs `agent-ide-message-chunk-functions' with text."
  (let* ((session (agent-ide-inline-test--session))
         (seen nil)
         (agent-ide-message-chunk-functions
          (list (lambda (s text) (setq seen (list s text))))))
    (cl-letf (((symbol-function 'agent-ide-renderer-append-stream-chunk)
               (lambda (_s _k _t) nil)))
      (agent-ide-transcript-handle-notification
       session
       '((method . "session/update")
         (params (update (sessionUpdate . "agent_message_chunk")
                         (content ((type . "text") (text . "hello"))))))))
    (should (eq (car seen) session))
    (should (equal (cadr seen) "hello"))))

(ert-deftest agent-ide-inline-prompt-response-hook-fires ()
  "Successful prompt runs `agent-ide-prompt-response-functions'."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let* ((session (agent-ide-inline-test--session))
           (seen nil)
           (agent-ide-prompt-response-functions
            (list (lambda (s response) (setq seen (list s response))))))
      (setq-local agent-ide--session session)
      (cl-letf (((symbol-function 'agent-ide-protocol-send-request)
                 (lambda (_s _req &rest args)
                   (funcall (plist-get args :on-success)
                            '((stopReason . "end_turn")))))
                ((symbol-function 'agent-ide-renderer-update-header) #'ignore)
                ((symbol-function 'agent-ide-renderer-reset-stream) #'ignore)
                ((symbol-function 'agent-ide-renderer-finish-stream) #'ignore)
                ((symbol-function 'agent-ide-renderer-follow-input) #'ignore))
        (agent-ide-protocol-send-prompt session "test"))
      (should (eq (car seen) session))
      (should (equal (map-elt (cadr seen) 'stopReason) "end_turn"))
      (should (equal (agent-ide-session-status session) "idle")))))

(ert-deftest agent-ide-inline-prompt-failure-hook-fires ()
  "Failed prompt runs `agent-ide-prompt-failure-functions'."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let* ((session (agent-ide-inline-test--session))
           (seen nil)
           (agent-ide-prompt-failure-functions
            (list (lambda (s error) (setq seen (list s error))))))
      (setq-local agent-ide--session session)
      (cl-letf (((symbol-function 'agent-ide-protocol-send-request)
                 (lambda (_s _req &rest args)
                   (funcall (plist-get args :on-failure)
                            '((message . "boom")))))
                ((symbol-function 'agent-ide-renderer-update-header) #'ignore)
                ((symbol-function 'agent-ide-renderer-reset-stream) #'ignore)
                ((symbol-function 'agent-ide-renderer-follow-input) #'ignore))
        (agent-ide-protocol-send-prompt session "test"))
      (should (eq (car seen) session))
      (should (equal (map-elt (cadr seen) 'message) "boom")))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `emacs -batch -L . -l agent-ide-inline-test.el -f ert-run-tests-batch-and-exit`
Expected: FAIL with `void-variable agent-ide-message-chunk-functions`

- [ ] **Step 3: Write minimal implementation**

In `agent-ide-transcript.el`, near the top after `(require 'agent-ide-protocol)`:

```elisp
(defvar agent-ide-message-chunk-functions nil
  "Hook run for each agent message text chunk.
Called with (SESSION TEXT) after the chunk is rendered in the transcript.")
```

Replace the `agent_message_chunk` branch in `agent-ide-transcript-handle-notification`:

```elisp
	 ("agent_message_chunk"
	  (let ((text (agent-ide-transcript--content-text
	               (map-elt update 'content))))
	    (agent-ide-renderer-append-stream-chunk session 'message text)
	    (run-hook-with-args 'agent-ide-message-chunk-functions session text)))
```

In `agent-ide-protocol.el`, after `(defvar agent-ide-model nil ...)`:

```elisp
(defvar agent-ide-prompt-response-functions nil
  "Hook run after a successful prompt response.
Called with (SESSION RESPONSE) after status is set to idle.")

(defvar agent-ide-prompt-failure-functions nil
  "Hook run after a failed prompt.
Called with (SESSION ERROR) after status is set to idle.")
```

At the end of `agent-ide-protocol-send-prompt`'s `:on-success` lambda (after the `stop-reason` block):

```elisp
                 (run-hook-with-args 'agent-ide-prompt-response-functions
                                     session response)
```

At the end of the `:on-failure` lambda (after `agent-ide-renderer-append-error`):

```elisp
                 (run-hook-with-args 'agent-ide-prompt-failure-functions
                                     session error))
```

- [ ] **Step 4: Run test to verify it passes**

Run the batch command from Step 2. Expected: 3/3 PASS.

- [ ] **Step 5: Commit**

```bash
git add agent-ide-transcript.el agent-ide-protocol.el agent-ide-inline-test.el
git commit -m "feat(inline): transcript chunk and prompt response hooks"
```

---

### Task 2: Inline core (state, preview overlay, accept/reject, helpers)

**Files:**
- Create: `agent-ide-inline.el`
- Test: `agent-ide-inline-test.el` (extend)

**Interfaces:**
- Consumes: the three hooks from Task 1; `agent-ide--make-session` accessors; `agent-ide--format-region-context`; `agent-ide--start-session`; `agent-ide--working-directory`; `agent-ide-protocol-cancel`.
- Produces:
  - `agent-ide-inline--previews` — alist `(SESSION . STATE)`, STATE a plist `(:buffer :overlay :start :end :text :done :instruction)`.
  - `(agent-ide-inline--preview-start SESSION BUFFER START END INSTRUCTION)` → STATE
  - `(agent-ide-inline--preview-update STATE TEXT)`
  - `(agent-ide-inline--teardown SESSION &optional MESSAGE)`
  - `(agent-ide-inline--preview-in-buffer)` → STATE or nil
  - `(agent-ide-inline--strip-fences TEXT)`, `(agent-ide-inline--build-prompt INSTRUCTION CONTEXT)`, `(agent-ide-inline--resolve-session)`
  - `(agent-ide-inline-accept)`, `(agent-ide-inline-reject)` commands
  - minor mode `agent-ide-inline-preview-mode`

- [ ] **Step 1: Write the failing tests** (append to `agent-ide-inline-test.el`)

```elisp
(require 'agent-ide-inline)

(ert-deftest agent-ide-inline-strip-fences-removes-surrounding-fence ()
  (should (equal (agent-ide-inline--strip-fences "```python\nx = 1\n```")
                 "x = 1")))

(ert-deftest agent-ide-inline-strip-fences-leaves-plain-text ()
  (should (equal (agent-ide-inline--strip-fences "  plain text  ")
                 "plain text")))

(ert-deftest agent-ide-inline-build-prompt-fills-template-slots ()
  (let ((agent-ide-inline-prompt-template "%i\n\n%c"))
    (should (equal (agent-ide-inline--build-prompt "fix it" "ctx block")
                   "fix it\n\nctx block"))))

(ert-deftest agent-ide-inline-build-prompt-default-has-constraint ()
  (let ((prompt (agent-ide-inline--build-prompt "i" "c")))
    (should (string-match-p "Do not use tools" prompt))
    (should (string-match-p "replacement text" prompt))))

(ert-deftest agent-ide-inline-resolve-returns-matching-session ()
  (let* ((dir (file-truename default-directory))
         (s1 (agent-ide-inline-test--session))
         (s2 (agent-ide-inline-test--session)))
    (setf (agent-ide-session-directory s1) (file-truename "/somewhere/else")
          (agent-ide-session-directory s2) dir)
    (cl-letf (((symbol-function 'agent-ide--working-directory)
               (lambda () default-directory))
              ((symbol-function 'agent-ide--start-session)
               (lambda (&optional _d) (error "should not start")))
              (agent-ide--sessions (list s1 s2)))
      (should (eq (agent-ide-inline--resolve-session) s2)))))

(ert-deftest agent-ide-inline-resolve-starts-new-session-when-no-match ()
  (let ((started nil))
    (cl-letf (((symbol-function 'agent-ide--working-directory)
               (lambda () "/proj/"))
              ((symbol-function 'agent-ide--start-session)
               (lambda (&optional d) (setq started d) 'new-session))
              (agent-ide--sessions nil))
      (should (eq (agent-ide-inline--resolve-session) 'new-session))
      (should (equal started "/proj/")))))

(ert-deftest agent-ide-inline-accept-replaces-region-and-is-undoable ()
  (with-temp-buffer
    (insert "hello world")
    (let* ((session (agent-ide-inline-test--session))
           (state (agent-ide-inline--preview-start
                   session (current-buffer) 1 6 "rewrite")))
      (agent-ide-inline--preview-update state "goodbye")
      (should (equal (buffer-string) "hello world"))
      (agent-ide-inline-accept)
      (should (equal (buffer-string) "goodbye world"))
      (undo)
      (should (equal (buffer-string) "hello world"))
      (should-not agent-ide-inline-preview-mode)
      (should-not (alist-get session agent-ide-inline--previews)))))

(ert-deftest agent-ide-inline-reject-leaves-buffer-unchanged ()
  (with-temp-buffer
    (insert "hello world")
    (let* ((session (agent-ide-inline-test--session))
           (state (agent-ide-inline--preview-start
                   session (current-buffer) 1 6 "rewrite")))
      (agent-ide-inline--preview-update state "goodbye")
      (agent-ide-inline-reject)
      (should (equal (buffer-string) "hello world"))
      (should-not agent-ide-inline-preview-mode)
      (should-not (alist-get session agent-ide-inline--previews)))))

(ert-deftest agent-ide-inline-external-edit-cancels-preview ()
  (with-temp-buffer
    (insert "hello world")
    (let* ((session (agent-ide-inline-test--session))
           (_state (agent-ide-inline--preview-start
                    session (current-buffer) 1 6 "rewrite")))
      (goto-char (point-max))
      (insert "!")
      (should-not (alist-get session agent-ide-inline--previews))
      (should-not agent-ide-inline-preview-mode))))

(ert-deftest agent-ide-inline-chunks-accumulate-into-overlay ()
  (with-temp-buffer
    (insert "hello world")
    (let* ((session (agent-ide-inline-test--session))
           (state (agent-ide-inline--preview-start
                   session (current-buffer) 1 6 "rewrite")))
      (agent-ide-inline--on-chunk session "goo")
      (agent-ide-inline--on-chunk session "dbye")
      (should (equal (plist-get state :text) "goodbye"))
      (should (equal (overlay-get (plist-get state :overlay) 'display)
                     (propertize "goodbye"
                                 'face 'agent-ide-inline-preview-face))))))

(ert-deftest agent-ide-inline-response-finalizes-and-strips-fences ()
  (with-temp-buffer
    (insert "hello world")
    (let* ((session (agent-ide-inline-test--session))
           (state (agent-ide-inline--preview-start
                   session (current-buffer) 1 6 "rewrite")))
      (agent-ide-inline--on-chunk session "```\nbye")
      (agent-ide-inline--on-response session nil)
      (should (plist-get state :done))
      (should (equal (plist-get state :text) "bye")))))
```

- [ ] **Step 2: Run tests to verify they fail**

Expected: FAIL with `void-function agent-ide-inline--strip-fences` (file not loadable — this is the "feature missing" failure).

- [ ] **Step 3: Write minimal implementation**

Create `agent-ide-inline.el`:

```elisp
;;; agent-ide-inline.el --- In-place region editing with agent-ide -*- lexical-binding: t; -*-

;;; Commentary:

;; gptel-inline-style in-place rewrites backed by an agent-ide session.
;; Select a region, run `agent-ide-inline-rewrite', give an instruction,
;; watch the proposed replacement stream into an overlay, then accept
;; (C-c C-c) or reject (C-c C-k).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agent-ide-core)
(require 'agent-ide-protocol)
(require 'agent-ide-renderer)
(require 'agent-ide-session)

(defgroup agent-ide-inline nil
  "In-place agent rewrites in any buffer."
  :group 'agent-ide
  :prefix "agent-ide-inline-")

(defface agent-ide-inline-preview-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for the proposed inline replacement text.")

(defcustom agent-ide-inline-accept-key "C-c C-c"
  "Key sequence to accept the inline preview."
  :type 'key-sequence
  :group 'agent-ide-inline)

(defcustom agent-ide-inline-reject-key "C-c C-k"
  "Key sequence to reject the inline preview."
  :type 'key-sequence
  :group 'agent-ide-inline)

(defcustom agent-ide-inline-prompt-template
  (concat "%i\n\n"
          "Constraint: Do not use tools and do not modify any files. "
          "Reply with only the replacement text for the region, "
          "without markdown fences or explanation.\n\n"
          "Context:\n%c")
  "Template for inline rewrite prompts.
%i is replaced with the instruction, %c with the region context."
  :type 'string
  :group 'agent-ide-inline)

(defcustom agent-ide-inline-ready-timeout 30
  "Seconds to wait for a newly created session to become ready."
  :type 'integer
  :group 'agent-ide-inline)

(defvar agent-ide-inline-history nil
  "History for inline rewrite instructions.")

(defvar agent-ide-inline--previews nil
  "Alist mapping active sessions to preview state plists.
Each state has keys :buffer :overlay :start :end :text :done
:instruction.  Hooks fire in the session buffer, so state is global.")

(defvar agent-ide-inline-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd agent-ide-inline-accept-key) #'agent-ide-inline-accept)
    (define-key map (kbd agent-ide-inline-reject-key) #'agent-ide-inline-reject)
    map)
  "Keymap for `agent-ide-inline-preview-mode'.")

(define-minor-mode agent-ide-inline-preview-mode
  "Minor mode while an agent-ide inline preview is active."
  :lighter " Inline"
  :keymap agent-ide-inline-preview-mode-map)

(defun agent-ide-inline--strip-fences (text)
  "Return TEXT with a single surrounding markdown code fence removed."
  (let ((s (string-trim text)))
    (when (string-match "\\````[^\n]*\n" s)
      (setq s (substring s (match-end 0))))
    (when (string-match "\n```[ \t]*\\'" s)
      (setq s (substring s 0 (match-beginning 0))))
    (string-trim s)))

(defun agent-ide-inline--build-prompt (instruction context)
  "Build the rewrite prompt from INSTRUCTION and CONTEXT."
  (format-spec agent-ide-inline-prompt-template
               (format-spec-make ?i instruction ?c context)))

(defun agent-ide-inline--resolve-session ()
  "Return the session for the current buffer's project directory.
Strict directory match; start a new session when none matches."
  (let ((dir (file-truename (agent-ide--working-directory))))
    (or (cl-find-if
         (lambda (s)
           (string= dir (file-truename (agent-ide-session-directory s))))
         agent-ide--sessions)
        (agent-ide--start-session (agent-ide--working-directory)))))

(defun agent-ide-inline--region-context (start end)
  "Return a region alist for START..END, like `agent-ide--get-region'."
  `((:file . ,(buffer-file-name))
    (:line-start . ,(line-number-at-pos start))
    (:line-end . ,(line-number-at-pos end))
    (:content . ,(buffer-substring-no-properties start end))))

(defun agent-ide-inline--preview-start (session buffer start end instruction)
  "Begin an inline preview in BUFFER over START..END for SESSION."
  (with-current-buffer buffer
    (let ((ov (make-overlay start end nil t t)))
      (overlay-put ov 'display
                   (propertize "(Working…)" 'face 'agent-ide-inline-preview-face))
      (overlay-put ov 'priority 100)
      (let ((state (list :buffer buffer :overlay ov
                         :start (copy-marker start)
                         :end (copy-marker end t)
                         :text "" :done nil :instruction instruction)))
        (push (cons session state) agent-ide-inline--previews)
        (add-hook 'after-change-functions
                  #'agent-ide-inline--buffer-changed nil t)
        (add-hook 'kill-buffer-hook
                  (lambda () (agent-ide-inline--teardown session)) nil t)
        (agent-ide-inline-preview-mode 1)
        state))))

(defun agent-ide-inline--preview-update (state text)
  "Set preview STATE text and refresh the overlay display string."
  (setf (plist-get state :text) text)
  (when-let* ((ov (plist-get state :overlay))
              ((overlayp ov)))
    (overlay-put ov 'display
                 (propertize text 'face 'agent-ide-inline-preview-face))))

(defun agent-ide-inline--preview-session (state)
  "Return the session owning preview STATE."
  (car (cl-find-if (lambda (entry) (eq (cdr entry) state))
                   agent-ide-inline--previews)))

(defun agent-ide-inline--preview-in-buffer ()
  "Return the preview state for the current buffer, or nil."
  (cl-find-if (lambda (state) (eq (plist-get state :buffer) (current-buffer)))
              (mapcar #'cdr agent-ide-inline--previews)))

(defun agent-ide-inline--teardown (session &optional message)
  "Remove SESSION's preview and restore the edited buffer view."
  (when-let* ((state (alist-get session agent-ide-inline--previews))
              (buffer (plist-get state :buffer)))
    (setq agent-ide-inline--previews
          (assq-delete-all session agent-ide-inline--previews))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (overlayp (plist-get state :overlay))
          (delete-overlay (plist-get state :overlay)))
        (remove-hook 'after-change-functions
                     #'agent-ide-inline--buffer-changed t)
        (when agent-ide-inline-preview-mode
          (agent-ide-inline-preview-mode -1))))
    (let ((m (plist-get state :start)))
      (when (markerp m) (set-marker m nil)))
    (let ((m (plist-get state :end)))
      (when (markerp m) (set-marker m nil))))
  (when message (message "%s" message)))

(defun agent-ide-inline--buffer-changed (&rest _args)
  "Cancel the active preview: the buffer was modified externally."
  (when-let* ((state (agent-ide-inline--preview-in-buffer)))
    (agent-ide-inline--teardown
     (agent-ide-inline--preview-session state)
     "Inline preview cancelled: buffer modified")))

(defun agent-ide-inline--on-chunk (session text)
  "Append chunk TEXT to SESSION's active preview."
  (when-let* ((state (alist-get session agent-ide-inline--previews))
              ((buffer-live-p (plist-get state :buffer)))
              ((not (plist-get state :done))))
    (agent-ide-inline--preview-update
     state (concat (plist-get state :text) text))))

(defun agent-ide-inline--on-response (session _response)
  "Finalize SESSION's active preview from the completed response."
  (when-let* ((state (alist-get session agent-ide-inline--previews))
              ((buffer-live-p (plist-get state :buffer))))
    (let ((final (agent-ide-inline--strip-fences (plist-get state :text))))
      (setf (plist-get state :text) final)
      (setf (plist-get state :done) t)
      (if (string-empty-p final)
          (agent-ide-inline--teardown session "Inline: empty response")
        (agent-ide-inline--preview-update state final)
        (message "Inline ready: %s accept, %s reject"
                 (key-description (kbd agent-ide-inline-accept-key))
                 (key-description (kbd agent-ide-inline-reject-key)))))))

(defun agent-ide-inline--on-failure (session _error)
  "Cancel SESSION's active preview after a failed prompt."
  (when-let* ((state (alist-get session agent-ide-inline--previews)))
    (agent-ide-inline--teardown session "Inline: agent request failed")))

(add-hook 'agent-ide-message-chunk-functions #'agent-ide-inline--on-chunk)
(add-hook 'agent-ide-prompt-response-functions #'agent-ide-inline--on-response)
(add-hook 'agent-ide-prompt-failure-functions #'agent-ide-inline--on-failure)

(defun agent-ide-inline-accept ()
  "Accept the inline preview: replace the region with the proposal."
  (interactive)
  (let ((state (agent-ide-inline--preview-in-buffer)))
    (unless state (user-error "No inline preview"))
    (let ((start (marker-position (plist-get state :start)))
          (end (marker-position (plist-get state :end)))
          (text (plist-get state :text))
          (session (agent-ide-inline--preview-session state)))
      (when (string-empty-p text)
        (agent-ide-inline--teardown session)
        (user-error "No text to accept"))
      (agent-ide-inline--teardown session)
      (atomic-change-group
        (goto-char end)
        (delete-region start end)
        (goto-char start)
        (insert text)))))

(defun agent-ide-inline-reject ()
  "Reject the inline preview, restoring the original text."
  (interactive)
  (let ((state (agent-ide-inline--preview-in-buffer)))
    (unless state (user-error "No inline preview"))
    (let ((session (agent-ide-inline--preview-session state)))
      (when (and (not (plist-get state :done))
                 (member (agent-ide-session-status session)
                         '("running")))
        (ignore-errors (agent-ide-protocol-cancel session)))
      (agent-ide-inline--teardown session)
      (message "Inline preview rejected"))))

(provide 'agent-ide-inline)

;;; agent-ide-inline.el ends here
```

- [ ] **Step 4: Run tests to verify they pass**

Run the batch command. Expected: all Task 2 tests PASS (plus Task 1 tests still green).

- [ ] **Step 5: Commit**

```bash
git add agent-ide-inline.el agent-ide-inline-test.el
git commit -m "feat(inline): preview overlay, accept/reject, chunk accumulation"
```

---

### Task 3: The rewrite command, readiness wait, wiring, docs

**Files:**
- Modify: `agent-ide-inline.el` (add command + send helpers)
- Modify: `agent-ide.el` (add `(require 'agent-ide-inline)` after `(require 'agent-ide-sidebar)`)
- Modify: `README.md` (feature documentation)
- Test: `agent-ide-inline-test.el` (extend)

**Interfaces:**
- Consumes: everything from Task 2; `agent-ide-renderer-append-status`; `agent-ide-protocol-send-prompt`.
- Produces: `(agent-ide-inline-rewrite START END INSTRUCTION)` command (autoload), `(agent-ide-inline--ready-p SESSION)`, `(agent-ide-inline--send-when-ready SESSION DEADLINE PROMPT)`.

- [ ] **Step 1: Write the failing tests**

```elisp
(ert-deftest agent-ide-inline-ready-p-detects-ready-session ()
  (let ((session (agent-ide-inline-test--session)))
    (setf (agent-ide-session-status session) "idle")
    (should (agent-ide-inline--ready-p session))
    (setf (agent-ide-session-status session) "creating-session")
    (should-not (agent-ide-inline--ready-p session))))

(ert-deftest agent-ide-inline-send-when-ready-sends-immediately ()
  (let* ((session (agent-ide-inline-test--session))
         (sent nil))
    (setf (agent-ide-session-status session) "idle")
    (cl-letf (((symbol-function 'agent-ide-protocol-send-prompt)
               (lambda (s p) (setq sent (list s p)))))
      (agent-ide-inline--send-when-ready
       session (time-add (current-time) 10) "prompt"))
    (should (equal sent (list session "prompt")))))

(ert-deftest agent-ide-inline-send-when-ready-waits-when-not-ready ()
  (let* ((session (agent-ide-inline-test--session))
         (timer nil)
         (sent nil))
    (setf (agent-ide-session-acp-session-id session) nil
          (agent-ide-session-status session) "creating-session")
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _rep fn &rest args)
                 (setq timer (cons fn args))))
              ((symbol-function 'agent-ide-protocol-send-prompt)
               (lambda (s p) (setq sent (list s p)))))
      (agent-ide-inline--send-when-ready
       session (time-add (current-time) 10) "prompt"))
    (should-not sent)
    (should (eq (car timer) #'agent-ide-inline--send-when-ready))))

(ert-deftest agent-ide-inline-rewrite-sends-prompt-and-starts-preview ()
  (with-temp-buffer
    (agent-ide-session-mode)
    (insert "hello world")
    (let* ((session (agent-ide-inline-test--session))
           (sent nil)
           (status-line nil))
      (setf (agent-ide-session-status session) "idle")
      (cl-letf (((symbol-function 'agent-ide-inline--resolve-session)
                 (lambda () session))
                ((symbol-function 'agent-ide-protocol-send-prompt)
                 (lambda (s p) (setq sent (list s p))))
                ((symbol-function 'agent-ide-renderer-append-status)
                 (lambda (_s text) (setq status-line text))))
        (agent-ide-inline-rewrite 1 6 "rewrite it"))
      (should (equal (car sent) session))
      (should (string-match-p "rewrite it" (cadr sent)))
      (should (string-match-p "Do not use tools" (cadr sent)))
      (should (string-match-p "hello" (cadr sent)))
      (should (string-match-p "Inline" status-line))
      (should agent-ide-inline-preview-mode)
      (should (alist-get session agent-ide-inline--previews))
      (should (equal (buffer-string) "hello world")))))

(ert-deftest agent-ide-inline-rewrite-errors-when-busy ()
  (with-temp-buffer
    (insert "hello world")
    (let* ((session (agent-ide-inline-test--session)))
      (setf (agent-ide-session-status session) "running")
      (cl-letf (((symbol-function 'agent-ide-inline--resolve-session)
                 (lambda () session))
                ((symbol-function 'agent-ide-protocol-send-prompt)
                 (lambda (_s _p) (error "must not send"))))
        (should-error (agent-ide-inline-rewrite 1 6 "rewrite it")
                      :type 'user-error)
        (should-not agent-ide-inline-preview-mode)))))
```

- [ ] **Step 2: Run tests to verify they fail**

Expected: FAIL with `void-function agent-ide-inline--ready-p`.

- [ ] **Step 3: Write minimal implementation**

Add to `agent-ide-inline.el` (before `(provide ...)`):

```elisp
(defun agent-ide-inline--ready-p (session)
  "Return non-nil when SESSION is initialized and idle."
  (and (agent-ide-session-acp-session-id session)
       (equal (agent-ide-session-status session) "idle")))

(defun agent-ide-inline--send-when-ready (session deadline prompt)
  "Send PROMPT once SESSION is ready, or retry until DEADLINE."
  (cond
   ((agent-ide-inline--ready-p session)
    (agent-ide-protocol-send-prompt session prompt))
   ((equal (agent-ide-session-status session) "failed")
    (agent-ide-inline--teardown session "Inline: agent session failed"))
   ((time-less-p deadline (current-time))
    (agent-ide-inline--teardown session "Inline: timed out waiting for agent"))
   (t
    (run-at-time 0.3 nil #'agent-ide-inline--send-when-ready
                 session deadline prompt))))

;;;###autoload
(defun agent-ide-inline-rewrite (start end instruction)
  "Rewrite the region START..END per INSTRUCTION in place.

Displays the agent's proposal as a streaming overlay over the region.
Accept with `agent-ide-inline-accept', reject with
`agent-ide-inline-reject'."
  (interactive
   (list (region-beginning) (region-end)
         (read-string "Rewrite instruction: " nil
                      'agent-ide-inline-history)))
  (unless (and (use-region-p) (> end start))
    (user-error "No region selected"))
  (let* ((session (agent-ide-inline--resolve-session))
         (context (agent-ide--format-region-context
                   (agent-ide-inline--region-context start end)
                   (agent-ide-session-directory session)))
         (prompt (agent-ide-inline--build-prompt instruction context)))
    (when (equal (agent-ide-session-status session) "running")
      (user-error "Agent busy: interrupt the running turn first"))
    (agent-ide-inline--preview-start session (current-buffer)
                                     start end instruction)
    (agent-ide-renderer-append-status session (format "Inline: %s" instruction))
    (agent-ide-inline--send-when-ready
     session
     (time-add (current-time) agent-ide-inline-ready-timeout)
     prompt)))
```

In `agent-ide.el`, after `(require 'agent-ide-sidebar)` add:

```elisp
(require 'agent-ide-inline)
```

In `README.md`, add a section after "### Session commands":

```markdown
### Inline editing (gptel-inline style)

Select a region in any buffer and run `M-x agent-ide-inline-rewrite`.
The agent's proposed replacement streams into an overlay over the region.
Accept with `C-c C-c` (replaces the region, undoable) or reject with
`C-c C-k` (restores the original text). The turn is visible in the
project's transcript buffer. Keys are configurable via
`agent-ide-inline-accept-key` / `agent-ide-inline-reject-key`.

| Command | Keybinding | Description |
|---|---|---|
| `agent-ide-inline-rewrite` | — | Rewrite the region per an instruction |
| `agent-ide-inline-accept` | `C-c C-c` | Accept the proposed replacement |
| `agent-ide-inline-reject` | `C-c C-k` | Reject and restore the original |
```

- [ ] **Step 4: Run full test suite to verify all pass**

Run: `emacs -batch -L . -l agent-ide-inline-test.el -f ert-run-tests-batch-and-exit`
Expected: all inline tests PASS. Also run the existing suites to confirm no regressions:

```
emacs -batch -L . -l agent-ide-submit-test.el -f ert-run-tests-batch-and-exit
emacs -batch -L . -l agent-ide-session-mode-test.el -f ert-run-tests-batch-and-exit
```

- [ ] **Step 5: Commit**

```bash
git add agent-ide-inline.el agent-ide-inline-test.el agent-ide.el README.md
git commit -m "feat(inline): rewrite command with readiness wait and docs"
```

---

## Self-Review

**Spec coverage:** Spec §requirements 1-7 → Task 3 (command) + Task 2 (overlay/accept/reject) + Task 1 (hooks) + Task 3 (require). Session resolution + busy handling → Task 2 resolve tests + Task 3 busy test. Error handling rows → teardown paths in Task 2/3. Testing section → all three tasks. ✓

**Placeholder scan:** None — all steps carry full code. ✓

**Type consistency:** Hook names, state plist keys (`:buffer :overlay :start :end :text :done :instruction`), and function signatures match across tasks. ✓
