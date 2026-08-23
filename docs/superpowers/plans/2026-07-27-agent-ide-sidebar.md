# Agent IDE Sidebar Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a left side-window listing live Agent IDE sessions (two-line rows: project/status + model/usage) with switch, kill, and new-session actions.

**Architecture:** New `agent-ide-sidebar.el` owns a `*agent-ide-sidebar*` special-mode buffer that projects `agent-ide--sessions`. Lifecycle hooks in core/session/renderer call refresh/show/hide via `fboundp` guards (no circular require). Kill/new reuse existing session APIs.

**Tech Stack:** Emacs Lisp 29+, ERT, existing `agent-ide-*` modules, `display-buffer-in-side-window`.

**Spec:** `docs/superpowers/specs/2026-07-27-agent-ide-sidebar-design.md`

---

## File structure

| File | Role |
|---|---|
| Create: `agent-ide-sidebar.el` | Buffer, mode, render, show/hide, commands |
| Create: `agent-ide-sidebar-test.el` | ERT for render, refresh, dismiss, kill, select |
| Modify: `agent-ide.el` | Customs + `(require 'agent-ide-sidebar)` |
| Modify: `agent-ide-core.el` | Notify sidebar after `agent-ide--set-status` |
| Modify: `agent-ide-session.el` | Show/refresh on create; refresh/hide on cleanup |
| Modify: `agent-ide-renderer.el` | Refresh sidebar from `agent-ide-renderer-update-header` |
| Modify: `agent-ide-hotload.el` | Insert sidebar in load order (before `agent-ide.el`) |
| Modify: `README.md` | Document sidebar usage/keys |

Load order after change: `core → protocol → renderer → session-mode → session → transcript → sidebar → agent-ide`.

---

### Task 1: Scaffold module + wire requires/customs

**Files:**
- Create: `agent-ide-sidebar.el`
- Modify: `agent-ide.el`
- Modify: `agent-ide-hotload.el`
- Test: `agent-ide-sidebar-test.el` (load smoke only in this task)

- [ ] **Step 1: Create `agent-ide-sidebar.el` skeleton**

```elisp
;;; agent-ide-sidebar.el --- Session list side window -*- lexical-binding: t; -*-

;;; Commentary:

;; Left sidebar listing live Agent IDE sessions and their status.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agent-ide-core)
(require 'agent-ide-renderer)

(defgroup agent-ide-sidebar nil
  "Agent IDE session sidebar."
  :group 'agent-ide
  :prefix "agent-ide-sidebar-")

(defcustom agent-ide-sidebar-width 0.22
  "Width of the Agent IDE sidebar side window."
  :type 'number
  :group 'agent-ide-sidebar)

(defcustom agent-ide-sidebar-auto-show t
  "When non-nil, show the sidebar when a session is created."
  :type 'boolean
  :group 'agent-ide-sidebar)

(defcustom agent-ide-sidebar-confirm-kill t
  "When non-nil, confirm before killing a session from the sidebar."
  :type 'boolean
  :group 'agent-ide-sidebar)

(defconst agent-ide-sidebar-buffer-name "*agent-ide-sidebar*"
  "Buffer name for the Agent IDE sidebar.")

(defvar agent-ide-sidebar--user-dismissed nil
  "Non-nil when the user hid the sidebar with `q'.")

(defvar-local agent-ide-sidebar--entries nil
  "List of `agent-ide-session' objects currently shown, in order.")

(defvar agent-ide-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "n") #'agent-ide-sidebar-next)
    (define-key map (kbd "p") #'agent-ide-sidebar-previous)
    (define-key map (kbd "RET") #'agent-ide-sidebar-select)
    (define-key map [mouse-1] #'agent-ide-sidebar-select)
    (define-key map (kbd "k") #'agent-ide-sidebar-kill)
    (define-key map (kbd "+") #'agent-ide-sidebar-new-session)
    (define-key map (kbd "c") #'agent-ide-sidebar-new-session)
    (define-key map (kbd "g") #'agent-ide-sidebar-refresh)
    (define-key map (kbd "q") #'agent-ide-sidebar-quit)
    map)
  "Keymap for `agent-ide-sidebar-mode'.")

(define-derived-mode agent-ide-sidebar-mode special-mode "Agent-IDE-Sidebar"
  "Major mode for the Agent IDE session sidebar."
  (setq truncate-lines t)
  (setq-local buffer-read-only t)
  (setq-local agent-ide-sidebar--entries nil))

(defun agent-ide-sidebar--buffer ()
  "Return the sidebar buffer, creating it if needed."
  (or (get-buffer agent-ide-sidebar-buffer-name)
      (with-current-buffer (get-buffer-create agent-ide-sidebar-buffer-name)
        (agent-ide-sidebar-mode)
        (current-buffer))))

(defun agent-ide-sidebar-refresh ()
  "Redraw the sidebar buffer from `agent-ide--sessions'.
Placeholder until Task 2 implements rendering."
  (agent-ide--cleanup-dead-sessions)
  (when-let* ((buffer (get-buffer agent-ide-sidebar-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "(sidebar stub)\n")
        (setq agent-ide-sidebar--entries nil)))))

;;;###autoload
(defun agent-ide-sidebar ()
  "Show the Agent IDE sidebar and clear the user-dismissed flag."
  (interactive)
  (setq agent-ide-sidebar--user-dismissed nil)
  (agent-ide-sidebar--buffer)
  (agent-ide-sidebar-refresh)
  (display-buffer
   (agent-ide-sidebar--buffer)
   `((display-buffer-in-side-window)
     (side . left)
     (slot . -1)
     (window-width . ,agent-ide-sidebar-width)
     (preserve-size . (t . nil)))))

(defun agent-ide-sidebar-quit ()
  "Hide the sidebar without killing sessions."
  (interactive)
  (setq agent-ide-sidebar--user-dismissed t)
  (when-let* ((window (get-buffer-window agent-ide-sidebar-buffer-name t)))
    (quit-window nil window)))

(defun agent-ide-sidebar-next ()
  "Move to the next sidebar entry."
  (interactive)
  (forward-line 1))

(defun agent-ide-sidebar-previous ()
  "Move to the previous sidebar entry."
  (interactive)
  (forward-line -1))

(defun agent-ide-sidebar-select ()
  "Select the session at point."
  (interactive)
  (user-error "Not implemented"))

(defun agent-ide-sidebar-kill ()
  "Kill the session at point."
  (interactive)
  (user-error "Not implemented"))

(defun agent-ide-sidebar-new-session ()
  "Start a new Agent IDE session."
  (interactive)
  (user-error "Not implemented"))

(provide 'agent-ide-sidebar)

;;; agent-ide-sidebar.el ends here
```

- [ ] **Step 2: Wire `agent-ide.el`**

After existing customs (near `agent-ide-mcp-servers`), add aliases that forward to the sidebar group **or** keep customs only in sidebar file (preferred — already in sidebar). Only add:

```elisp
(require 'agent-ide-sidebar)
```

immediately before `(provide 'agent-ide)`, after `(require 'agent-ide-session)`.

- [ ] **Step 3: Update hotload order**

In `agent-ide-hotload.el`, change `agent-ide-hotload--files` to:

```elisp
(defvar agent-ide-hotload--files
  '("agent-ide-core.el"
    "agent-ide-protocol.el"
    "agent-ide-renderer.el"
    "agent-ide-session-mode.el"
    "agent-ide-session.el"
    "agent-ide-transcript.el"
    "agent-ide-sidebar.el"
    "agent-ide.el")
  "Project source files in load order (respecting dependencies).")
```

- [ ] **Step 4: Smoke-load**

Run:

```bash
cd /Users/zhihu/.emacs.d/agent-ide
emacs -Q --batch \
  -L . \
  --eval "(unless (featurep 'acp) (provide 'acp))" \
  -l agent-ide-core.el \
  -l agent-ide-renderer.el \
  -l agent-ide-sidebar.el \
  --eval "(princ (format \"ok %s\\n\" (fboundp 'agent-ide-sidebar)))"
```

Expected: `ok t`

- [ ] **Step 5: Commit**

```bash
git add agent-ide-sidebar.el agent-ide.el agent-ide-hotload.el
git commit -m "$(cat <<'EOF'
feat(sidebar): scaffold session sidebar module

Add special-mode buffer stub and wire it into package load order.
EOF
)"
```

---

### Task 2: Entry formatting + render (TDD)

**Files:**
- Modify: `agent-ide-sidebar.el`
- Create: `agent-ide-sidebar-test.el`

- [ ] **Step 1: Write failing tests for format helpers**

Create `agent-ide-sidebar-test.el`:

```elisp
;;; agent-ide-sidebar-test.el --- Tests for Agent IDE sidebar -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(unless (featurep 'acp)
  (provide 'acp))

(require 'agent-ide-core)
(require 'agent-ide-renderer)
(require 'agent-ide-sidebar)

(defun agent-ide-sidebar-test--make-session (directory status &rest plist)
  "Make a live test session for DIRECTORY with STATUS.
PLIST may include :models :usage :buffer-name."
  (let* ((buf-name (or (plist-get plist :buffer-name)
                       (format "*agent-test:%s*"
                               (agent-ide--directory-name directory))))
         (buffer (get-buffer-create buf-name))
         (session (agent-ide--make-session
                   :directory (file-name-as-directory (expand-file-name directory))
                   :buffer buffer
                   :status status
                   :tool-calls (make-hash-table :test 'equal)
                   :models (plist-get plist :models)
                   :usage (plist-get plist :usage))))
    (with-current-buffer buffer
      (setq-local default-directory (agent-ide-session-directory session))
      (setq-local agent-ide--session session))
    session))

(defun agent-ide-sidebar-test--teardown ()
  "Kill sidebar and test session buffers; clear global session list."
  (setq agent-ide--sessions nil)
  (setq agent-ide-sidebar--user-dismissed nil)
  (when-let* ((buf (get-buffer agent-ide-sidebar-buffer-name)))
    (kill-buffer buf))
  (dolist (buf (buffer-list))
    (when (string-prefix-p "*agent-test:" (buffer-name buf))
      (kill-buffer buf))))

(ert-deftest agent-ide-sidebar-formats-project-line ()
  "Line 1 includes project name, optional index, and status."
  (unwind-protect
      (let* ((session (agent-ide-sidebar-test--make-session
                       "/tmp/myproject" "running"
                       :buffer-name "*agent:myproject*<2>"))
             (line (agent-ide-sidebar--format-line1 session nil)))
        (should (string-match-p "myproject" line))
        (should (string-match-p "<2>" line))
        (should (string-match-p "running" line)))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-formats-meta-line ()
  "Line 2 includes model and percent usage."
  (unwind-protect
      (let* ((session (agent-ide-sidebar-test--make-session
                       "/tmp/myproject" "idle"
                       :models [((id . "large") (name . "Large") (isDefault . t))]
                       :usage '((used . 42000) (size . 100000))))
             (line (agent-ide-sidebar--format-line2 session)))
        (should (string-match-p "Large" line))
        (should (string-match-p "42%" line)))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-formats-unknown-meta-as-dash ()
  "Unknown model/usage becomes an em dash."
  (unwind-protect
      (let* ((session (agent-ide-sidebar-test--make-session "/tmp/x" "idle"))
             (line (agent-ide-sidebar--format-line2 session)))
        (should (string-match-p "—" line)))
    (agent-ide-sidebar-test--teardown)))

(provide 'agent-ide-sidebar-test)

;;; agent-ide-sidebar-test.el ends here
```

- [ ] **Step 2: Run tests — expect FAIL**

```bash
cd /Users/zhihu/.emacs.d/agent-ide
emacs -Q --batch -L . \
  --eval "(unless (featurep 'acp) (provide 'acp))" \
  -l agent-ide-sidebar-test.el \
  -f ert-run-tests-batch-and-exit
```

Expected: FAIL — `agent-ide-sidebar--format-line1` / `--format-line2` void.

- [ ] **Step 3: Implement format + redraw**

Replace stub `agent-ide-sidebar-refresh` and add helpers in `agent-ide-sidebar.el` (before `provide`):

```elisp
(defface agent-ide-sidebar-status-idle
  '((t :inherit shadow))
  "Face for idle session status."
  :group 'agent-ide-sidebar)

(defface agent-ide-sidebar-status-running
  '((t :inherit success))
  "Face for running/working session status."
  :group 'agent-ide-sidebar)

(defface agent-ide-sidebar-status-failed
  '((t :inherit error))
  "Face for failed session status."
  :group 'agent-ide-sidebar)

(defface agent-ide-sidebar-status-other
  '((t :inherit warning))
  "Face for interrupting/starting and other statuses."
  :group 'agent-ide-sidebar)

(defface agent-ide-sidebar-current
  '((t :inherit highlight))
  "Face for the selected sidebar entry."
  :group 'agent-ide-sidebar)

(defun agent-ide-sidebar--status-face (status)
  "Return face symbol for STATUS string."
  (pcase (downcase (or status ""))
    ((or "idle" "") 'agent-ide-sidebar-status-idle)
    ((or "running" "working") 'agent-ide-sidebar-status-running)
    ("failed" 'agent-ide-sidebar-status-failed)
    (_ 'agent-ide-sidebar-status-other)))

(defun agent-ide-sidebar--buffer-index (session)
  "Return numeric buffer index for SESSION, or nil."
  (when-let* ((buffer (agent-ide-session-buffer session))
              ((buffer-live-p buffer))
              (name (buffer-name buffer)))
    (when (string-match "<\\([0-9]+\\)>\\'" name)
      (string-to-number (match-string 1 name)))))

(defun agent-ide-sidebar--session-visible-p (session)
  "Return non-nil if SESSION transcript is shown in some window."
  (when-let* ((buffer (agent-ide-session-buffer session)))
    (and (buffer-live-p buffer)
         (get-buffer-window buffer t))))

(defun agent-ide-sidebar--usage-percent (session)
  "Return usage percent string for SESSION, or nil."
  (when-let* ((usage (agent-ide-session-usage session))
              (used (agent-ide-renderer--context-used usage))
              (window (agent-ide-renderer--context-window usage))
              ((and (numberp used) (numberp window) (> window 0))))
    (format "%d%%" (round (* 100.0 (/ (float used) window))))))

(defun agent-ide-sidebar--format-line1 (session selected-p)
  "Return propertized first line for SESSION.
SELECTED-P is reserved for callers; visibility uses ●/○."
  (let* ((dot (if (agent-ide-sidebar--session-visible-p session) "●" "○"))
         (project (agent-ide--directory-name
                   (agent-ide-session-directory session)))
         (index (agent-ide-sidebar--buffer-index session))
         (status (or (agent-ide-session-status session) "unknown"))
         (left (concat dot " " project
                       (if index (format " <%d>" index) "")))
         (right status)
         (width (max 20 (window-width (selected-window))))
         (pad (max 1 (- width (string-width left) (string-width right) 1)))
         (line (concat left (make-string pad ?\s) right)))
    (add-text-properties
     0 (length line)
     (list 'agent-ide-session session
           'agent-ide-sidebar-entry t)
     line)
    (add-face-text-property
     (- (length line) (length right)) (length line)
     (agent-ide-sidebar--status-face status) nil line)
    (when selected-p
      (add-face-text-property 0 (length line) 'agent-ide-sidebar-current t line))
    line))

(defun agent-ide-sidebar--format-line2 (session)
  "Return propertized second line for SESSION."
  (let* ((model (or (agent-ide-renderer--model-label session) "—"))
         (usage (or (agent-ide-sidebar--usage-percent session) "—"))
         (text (format "  %s · %s" model usage)))
    (add-text-properties
     0 (length text)
     (list 'agent-ide-session session
           'agent-ide-sidebar-entry t)
     text)
    text))

(defun agent-ide-sidebar--insert-new-button ()
  "Insert the footer new-session button."
  (insert "\n")
  (insert-text-button
   "[+ New]"
   'action (lambda (_button) (agent-ide-sidebar-new-session))
   'follow-link t
   'help-echo "Create a new Agent IDE session"))

(defun agent-ide-sidebar--session-at-point ()
  "Return session text-property at point."
  (get-text-property (point) 'agent-ide-session))

(defun agent-ide-sidebar-refresh ()
  "Redraw the sidebar from live sessions."
  (agent-ide--cleanup-dead-sessions)
  (let* ((buffer (agent-ide-sidebar--buffer))
         (sessions (reverse agent-ide--sessions))
         (old-session
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (agent-ide-sidebar--session-at-point)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (entry-start nil))
        (erase-buffer)
        (setq agent-ide-sidebar--entries sessions)
        (dolist (session sessions)
          (let ((selected (eq session old-session)))
            (insert (agent-ide-sidebar--format-line1 session selected))
            (insert "\n")
            (insert (agent-ide-sidebar--format-line2 session))
            (insert "\n")))
        (agent-ide-sidebar--insert-new-button)
        (goto-char (point-min))
        (when old-session
          (when-let* ((pos (text-property-any
                            (point-min) (point-max)
                            'agent-ide-session old-session)))
            (goto-char pos)))))))
```

Note: `agent-ide-sidebar-new-session` still errors until Task 4 — button will be wired then. For Task 2 tests, only format helpers matter.

Also fix `n`/`p` to move by entry (2 lines) once render exists — update next/previous:

```elisp
(defun agent-ide-sidebar-next ()
  "Move to the next sidebar entry."
  (interactive)
  (when-let* ((session (agent-ide-sidebar--session-at-point)))
    (forward-line 1)
    (while (and (not (eobp))
                (eq (agent-ide-sidebar--session-at-point) session))
      (forward-line 1)))
  (unless (agent-ide-sidebar--session-at-point)
    (goto-char (point-max))
    (when-let* ((pos (previous-single-property-change
                      (point) 'agent-ide-session)))
      (goto-char (max (point-min) (1- pos))))))

(defun agent-ide-sidebar-previous ()
  "Move to the previous sidebar entry."
  (interactive)
  (let ((session (agent-ide-sidebar--session-at-point)))
    (forward-line -1)
    (while (and (not (bobp))
                (or (null (agent-ide-sidebar--session-at-point))
                    (eq (agent-ide-sidebar--session-at-point) session)))
      (forward-line -1))))
```

- [ ] **Step 4: Run format tests — expect PASS**

Same command as Step 2. Expected: 3 tests PASS (others not yet added).

- [ ] **Step 5: Add render integration test**

Append to `agent-ide-sidebar-test.el`:

```elisp
(ert-deftest agent-ide-sidebar-renders-multiple-sessions ()
  "Refresh inserts two lines per session plus footer."
  (unwind-protect
      (let ((a (agent-ide-sidebar-test--make-session "/tmp/a" "idle"))
            (b (agent-ide-sidebar-test--make-session "/tmp/b" "running")))
        (setq agent-ide--sessions (list b a))
        (agent-ide-sidebar-refresh)
        (with-current-buffer agent-ide-sidebar-buffer-name
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "a" text))
            (should (string-match-p "b" text))
            (should (string-match-p "idle" text))
            (should (string-match-p "running" text))
            (should (string-match-p "\\[\\+ New\\]" text))
            (should (= (length agent-ide-sidebar--entries) 2)))))
    (agent-ide-sidebar-test--teardown)))
```

Run ERT again — expect PASS.

- [ ] **Step 6: Commit**

```bash
git add agent-ide-sidebar.el agent-ide-sidebar-test.el
git commit -m "$(cat <<'EOF'
feat(sidebar): render two-line session entries

Format project/status and model/usage rows and redraw the sidebar buffer.
EOF
)"
```

---

### Task 3: Show / hide / dismissed policy + hooks

**Files:**
- Modify: `agent-ide-sidebar.el`
- Modify: `agent-ide-core.el`
- Modify: `agent-ide-session.el`
- Modify: `agent-ide-renderer.el`
- Modify: `agent-ide-sidebar-test.el`

- [ ] **Step 1: Write failing tests for dismiss + auto-show**

Append:

```elisp
(ert-deftest agent-ide-sidebar-quit-sets-dismissed-and-hides ()
  "q marks dismissed and removes the side window."
  (unwind-protect
      (let ((session (agent-ide-sidebar-test--make-session "/tmp/a" "idle")))
        (setq agent-ide--sessions (list session))
        (agent-ide-sidebar)
        (should (get-buffer-window agent-ide-sidebar-buffer-name t))
        (agent-ide-sidebar-quit)
        (should agent-ide-sidebar--user-dismissed)
        (should (null (get-buffer-window agent-ide-sidebar-buffer-name t))))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-refresh-does-not-reshow-when-dismissed ()
  "Status refresh keeps sidebar hidden after user dismiss."
  (unwind-protect
      (let ((session (agent-ide-sidebar-test--make-session "/tmp/a" "idle")))
        (setq agent-ide--sessions (list session))
        (agent-ide-sidebar)
        (agent-ide-sidebar-quit)
        (agent-ide-sidebar-on-sessions-changed)
        (should (null (get-buffer-window agent-ide-sidebar-buffer-name t))))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-new-session-clears-dismissed ()
  "Creating a session shows sidebar again when auto-show is on."
  (unwind-protect
      (let ((agent-ide-sidebar-auto-show t)
            (session (agent-ide-sidebar-test--make-session "/tmp/a" "idle")))
        (setq agent-ide-sidebar--user-dismissed t)
        (setq agent-ide--sessions (list session))
        (agent-ide-sidebar-on-session-created session)
        (should (null agent-ide-sidebar--user-dismissed))
        (should (get-buffer-window agent-ide-sidebar-buffer-name t)))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-hides-when-no-sessions ()
  "Cleanup of last session hides the sidebar."
  (unwind-protect
      (let ((session (agent-ide-sidebar-test--make-session "/tmp/a" "idle")))
        (setq agent-ide--sessions (list session))
        (agent-ide-sidebar)
        (setq agent-ide--sessions nil)
        (agent-ide-sidebar-on-sessions-changed)
        (should (null (get-buffer-window agent-ide-sidebar-buffer-name t))))
    (agent-ide-sidebar-test--teardown)))
```

- [ ] **Step 2: Run — expect FAIL** (`agent-ide-sidebar-on-session-created` void)

- [ ] **Step 3: Implement show/hide API**

Add to `agent-ide-sidebar.el`:

```elisp
(defun agent-ide-sidebar--visible-p ()
  "Return non-nil if the sidebar window is visible."
  (get-buffer-window agent-ide-sidebar-buffer-name t))

(defun agent-ide-sidebar--show (&optional select)
  "Show sidebar side window. SELECT non-nil means select it."
  (let ((buffer (agent-ide-sidebar--buffer)))
    (agent-ide-sidebar-refresh)
    (let ((window
           (display-buffer
            buffer
            `((display-buffer-in-side-window)
              (side . left)
              (slot . -1)
              (window-width . ,agent-ide-sidebar-width)
              (preserve-size . (t . nil))))))
      (when (and select window)
        (select-window window))
      window)))

(defun agent-ide-sidebar--hide ()
  "Hide sidebar window if present."
  (when-let* ((window (get-buffer-window agent-ide-sidebar-buffer-name t)))
    (quit-window nil window)))

(defun agent-ide-sidebar-on-session-created (&optional _session)
  "React to a newly created session."
  (when agent-ide-sidebar-auto-show
    (setq agent-ide-sidebar--user-dismissed nil)
    (agent-ide-sidebar--show nil)))

(defun agent-ide-sidebar-on-sessions-changed ()
  "Refresh or hide sidebar after session list/status changes."
  (agent-ide--cleanup-dead-sessions)
  (cond
   ((null agent-ide--sessions)
    (agent-ide-sidebar--hide))
   ((agent-ide-sidebar--visible-p)
    (agent-ide-sidebar-refresh))
   ((and agent-ide-sidebar-auto-show
         (not agent-ide-sidebar--user-dismissed))
    (agent-ide-sidebar--show nil))
   (t
    ;; Dismissed or auto-show off: refresh buffer contents only if it exists.
    (when (get-buffer agent-ide-sidebar-buffer-name)
      (agent-ide-sidebar-refresh)))))

;;;###autoload
(defun agent-ide-sidebar ()
  "Show the Agent IDE sidebar and clear the user-dismissed flag."
  (interactive)
  (setq agent-ide-sidebar--user-dismissed nil)
  (agent-ide-sidebar--show t))

(defun agent-ide-sidebar-quit ()
  "Hide the sidebar without killing sessions."
  (interactive)
  (setq agent-ide-sidebar--user-dismissed t)
  (agent-ide-sidebar--hide))
```

- [ ] **Step 4: Hook core / session / renderer**

In `agent-ide-core.el`, end of `agent-ide--set-status`:

```elisp
(defun agent-ide--set-status (session status)
  "Set SESSION status to STATUS."
  (when (agent-ide-session-p session)
    (setf (agent-ide-session-status session) status)
    (when-let* ((buffer (agent-ide-session-buffer session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (fboundp 'agent-ide-renderer-refresh-placeholder)
            (agent-ide-renderer-refresh-placeholder session))
          (force-mode-line-update t))))
    (when (fboundp 'agent-ide-sidebar-on-sessions-changed)
      (agent-ide-sidebar-on-sessions-changed))))
```

In `agent-ide-session.el` `agent-ide--create-session`, after `(push session agent-ide--sessions)`:

```elisp
(when (fboundp 'agent-ide-sidebar-on-session-created)
  (agent-ide-sidebar-on-session-created session))
```

In `agent-ide-session.el` `agent-ide--cleanup-session`, after removing from `agent-ide--sessions`:

```elisp
(when (fboundp 'agent-ide-sidebar-on-sessions-changed)
  (agent-ide-sidebar-on-sessions-changed))
```

In `agent-ide-renderer.el` `agent-ide-renderer-update-header`, after updating header-line:

```elisp
(when (fboundp 'agent-ide-sidebar-on-sessions-changed)
  (agent-ide-sidebar-on-sessions-changed))
```

- [ ] **Step 5: Run sidebar ERT — expect PASS**

```bash
cd /Users/zhihu/.emacs.d/agent-ide
emacs -Q --batch -L . \
  --eval "(unless (featurep 'acp) (provide 'acp))" \
  -l agent-ide-sidebar-test.el \
  -f ert-run-tests-batch-and-exit
```

Note: window tests need a frame. In batch Emacs, `display-buffer-in-side-window` often still creates a window on the initial frame. If batch lacks windows, wrap window assertions with:

```elisp
(skip-unless (not noninteractive)) ; only if batch truly cannot create windows
```

Prefer making tests work in batch: call `agent-ide-sidebar--show` and assert `(get-buffer-window ...)` — Emacs 29+ batch usually has a terminal frame. If FAIL due to no window, assert buffer exists + `agent-ide-sidebar--user-dismissed` state instead, and keep one interactive smoke note in README.

- [ ] **Step 6: Commit**

```bash
git add agent-ide-sidebar.el agent-ide-sidebar-test.el \
  agent-ide-core.el agent-ide-session.el agent-ide-renderer.el
git commit -m "$(cat <<'EOF'
feat(sidebar): auto show/hide with dismiss policy

Hook session lifecycle and status updates into the sidebar side window.
EOF
)"
```

---

### Task 4: Select / kill / new commands (TDD)

**Files:**
- Modify: `agent-ide-sidebar.el`
- Modify: `agent-ide-sidebar-test.el`

- [ ] **Step 1: Write failing command tests**

```elisp
(ert-deftest agent-ide-sidebar-select-displays-session-buffer ()
  "RET displays the session buffer."
  (unwind-protect
      (let* ((session (agent-ide-sidebar-test--make-session "/tmp/a" "idle"))
             (shown nil))
        (setq agent-ide--sessions (list session))
        (agent-ide-sidebar-refresh)
        (cl-letf (((symbol-function 'agent-ide--display-buffer)
                   (lambda (buffer)
                     (setq shown buffer)
                     nil)))
          (with-current-buffer agent-ide-sidebar-buffer-name
            (goto-char (point-min))
            (agent-ide-sidebar-select))
          (should (eq shown (agent-ide-session-buffer session)))))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-kill-removes-session ()
  "k cleans up session and removes the entry."
  (unwind-protect
      (let ((session (agent-ide-sidebar-test--make-session "/tmp/a" "idle"))
            (agent-ide-sidebar-confirm-kill nil))
        (setq agent-ide--sessions (list session))
        (agent-ide-sidebar-refresh)
        (cl-letf (((symbol-function 'agent-ide--cleanup-session)
                   (lambda (s)
                     (setq agent-ide--sessions (delq s agent-ide--sessions))))
                  ((symbol-function 'kill-buffer)
                   (lambda (&optional buffer)
                     (when-let* ((buf (or buffer (current-buffer))))
                       (when (buffer-live-p buf)
                         ;; Avoid real kill of unrelated buffers in harness.
                         (when (eq buf (agent-ide-session-buffer session))
                           (set-buffer-modified-p nil)))))))
          (with-current-buffer agent-ide-sidebar-buffer-name
            (goto-char (point-min))
            (agent-ide-sidebar-kill))
          (should (null agent-ide--sessions))))
    (agent-ide-sidebar-test--teardown)))
```

Require `agent-ide-session` for `agent-ide--display-buffer` / cleanup in implementation; tests stub them.

- [ ] **Step 2: Run — expect FAIL** (`Not implemented`)

- [ ] **Step 3: Implement commands**

At top of sidebar file, after renderer require:

```elisp
(require 'agent-ide-session)
```

Implement:

```elisp
(defun agent-ide-sidebar-select ()
  "Display the session at point."
  (interactive)
  (let ((session (or (agent-ide-sidebar--session-at-point)
                     (user-error "No session at point"))))
    (unless (agent-ide--session-live-p session)
      (user-error "Session is dead"))
    (agent-ide--display-buffer (agent-ide-session-buffer session))
    (agent-ide-sidebar-refresh)))

(defun agent-ide-sidebar-kill ()
  "Kill the session at point."
  (interactive)
  (let* ((session (or (agent-ide-sidebar--session-at-point)
                      (user-error "No session at point")))
         (buffer (agent-ide-session-buffer session))
         (was-current
          (and (buffer-live-p buffer)
               (get-buffer-window buffer t))))
    (when (or (not agent-ide-sidebar-confirm-kill)
              (y-or-n-p (format "Kill session %s? "
                                (agent-ide--directory-name
                                 (agent-ide-session-directory session)))))
      (agent-ide--cleanup-session session)
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (agent-ide--cleanup-dead-sessions)
      (cond
       ((null agent-ide--sessions)
        (agent-ide-sidebar--hide))
       (was-current
        (when-let* ((next (car agent-ide--sessions))
                    ((agent-ide--session-live-p next)))
          (agent-ide--display-buffer (agent-ide-session-buffer next)))
        (agent-ide-sidebar-refresh))
       (t
        (agent-ide-sidebar-refresh))))))

(defun agent-ide-sidebar-new-session ()
  "Start a new Agent IDE session."
  (interactive)
  (setq agent-ide-sidebar--user-dismissed nil)
  (agent-ide-new-session))
```

Avoid double-refresh loops: `cleanup-session` already calls `on-sessions-changed`. Prefer kill command calling only cleanup+kill-buffer and letting hooks refresh — simplify kill to:

```elisp
(defun agent-ide-sidebar-kill ()
  "Kill the session at point."
  (interactive)
  (let* ((session (or (agent-ide-sidebar--session-at-point)
                      (user-error "No session at point")))
         (buffer (agent-ide-session-buffer session))
         (was-current
          (and (buffer-live-p buffer)
               (get-buffer-window buffer t))))
    (when (or (not agent-ide-sidebar-confirm-kill)
              (y-or-n-p (format "Kill session %s? "
                                (agent-ide--directory-name
                                 (agent-ide-session-directory session)))))
      (agent-ide--cleanup-session session)
      (when (buffer-live-p buffer)
        (let ((kill-buffer-hook
               (remq #'agent-ide--handle-buffer-killed kill-buffer-hook)))
          ;; kill-buffer-hook on session buffer also cleans up; session already cleaned.
          (kill-buffer buffer)))
      (when (and was-current agent-ide--sessions)
        (when-let* ((next (car agent-ide--sessions))
                    ((agent-ide--session-live-p next)))
          (agent-ide--display-buffer (agent-ide-session-buffer next)))))))
```

Because `kill-buffer-hook` calls `agent-ide--handle-buffer-killed` → cleanup again, either:

1. Only `kill-buffer` (let hook cleanup), or  
2. Only `agent-ide--cleanup-session` then `kill-buffer` with hook temporarily unbound.

**Choose (1) for kill:**

```elisp
(defun agent-ide-sidebar-kill ()
  "Kill the session at point."
  (interactive)
  (let* ((session (or (agent-ide-sidebar--session-at-point)
                      (user-error "No session at point")))
         (buffer (agent-ide-session-buffer session))
         (was-current
          (and (buffer-live-p buffer)
               (get-buffer-window buffer t))))
    (when (or (not agent-ide-sidebar-confirm-kill)
              (y-or-n-p (format "Kill session %s? "
                                (agent-ide--directory-name
                                 (agent-ide-session-directory session)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (and was-current agent-ide--sessions)
        (when-let* ((next (car agent-ide--sessions))
                    ((agent-ide--session-live-p next)))
          (agent-ide--display-buffer (agent-ide-session-buffer next)))))))
```

Update the kill test to stub only `kill-buffer` such that it removes the session via calling real `agent-ide--cleanup-session`, or call the real cleanup path:

```elisp
(ert-deftest agent-ide-sidebar-kill-removes-session ()
  "k kills the session buffer and removes the entry."
  (unwind-protect
      (let ((session (agent-ide-sidebar-test--make-session "/tmp/a" "idle"))
            (agent-ide-sidebar-confirm-kill nil))
        (setq agent-ide--sessions (list session))
        (with-current-buffer (agent-ide-session-buffer session)
          (add-hook 'kill-buffer-hook #'agent-ide--handle-buffer-killed nil t))
        (agent-ide-sidebar-refresh)
        (with-current-buffer agent-ide-sidebar-buffer-name
          (goto-char (point-min))
          (agent-ide-sidebar-kill))
        (should (null agent-ide--sessions))
        (should (not (buffer-live-p (agent-ide-session-buffer session)))))
    (agent-ide-sidebar-test--teardown)))
```

- [ ] **Step 4: Run all sidebar tests — expect PASS**

Also run existing suite:

```bash
emacs -Q --batch -L . \
  --eval "(unless (featurep 'acp) (provide 'acp))" \
  -l agent-ide-session-mode.el \
  -l agent-ide-session-mode-test.el \
  -f ert-run-tests-batch-and-exit
```

Expected: existing tests still PASS.

- [ ] **Step 5: Commit**

```bash
git add agent-ide-sidebar.el agent-ide-sidebar-test.el
git commit -m "$(cat <<'EOF'
feat(sidebar): add select, kill, and new-session commands

Wire RET/k/+ to existing session display and lifecycle helpers.
EOF
)"
```

---

### Task 5: Status-change refresh test + README

**Files:**
- Modify: `agent-ide-sidebar-test.el`
- Modify: `README.md`
- Modify: `README.md` Architecture / Development sections for hotload order

- [ ] **Step 1: Add status refresh test**

```elisp
(ert-deftest agent-ide-sidebar-status-change-updates-text ()
  "agent-ide--set-status refreshes sidebar text."
  (unwind-protect
      (let ((session (agent-ide-sidebar-test--make-session "/tmp/a" "idle")))
        (setq agent-ide--sessions (list session))
        (agent-ide-sidebar-refresh)
        (agent-ide--set-status session "running")
        (with-current-buffer agent-ide-sidebar-buffer-name
          (should (string-match-p
                   "running"
                   (buffer-substring-no-properties (point-min) (point-max))))))
    (agent-ide-sidebar-test--teardown)))
```

Run ERT — PASS.

- [ ] **Step 2: Update README**

In Usage, add section:

```markdown
### Session sidebar

When a session starts, a left sidebar lists live Agent IDE sessions (two-line rows: project/status and model/usage).

| Command | Keybinding | Description |
|---|---|---|
| `agent-ide-sidebar` | — | Show the sidebar |
| Select session | `RET` | Display that session buffer |
| Next / previous | `n` / `p` | Move by session entry |
| Kill session | `k` | Kill session (confirm by default) |
| New session | `+` / `c` | Start `agent-ide-new-session` |
| Refresh | `g` | Redraw the list |
| Quit | `q` | Hide sidebar (keeps sessions; does not auto-reopen until a new session or explicit show) |
```

In Configuration table, add the three `agent-ide-sidebar-*` options.

In Architecture tree, add `agent-ide-sidebar.el`.

In Development load order line, insert `sidebar` before `agent-ide`.

- [ ] **Step 3: Final test run**

```bash
cd /Users/zhihu/.emacs.d/agent-ide
emacs -Q --batch -L . \
  --eval "(unless (featurep 'acp) (provide 'acp))" \
  -l agent-ide-sidebar-test.el \
  -f ert-run-tests-batch-and-exit

emacs -Q --batch -L . \
  --eval "(unless (featurep 'acp) (provide 'acp))" \
  -l agent-ide-session-mode.el \
  -l agent-ide-session-mode-test.el \
  -f ert-run-tests-batch-and-exit
```

Expected: all PASS.

- [ ] **Step 4: Commit**

```bash
git add agent-ide-sidebar-test.el README.md
git commit -m "$(cat <<'EOF'
docs(README): document Agent IDE session sidebar

Cover keys, customs, and updated module load order.
EOF
)"
```

---

## Self-review (plan vs spec)

| Spec requirement | Task |
|---|---|
| Left side-window list | 1, 3 |
| Two-line rows: project/status + model/usage | 2 |
| ●/○ visibility + status faces | 2 |
| Auto-show / auto-hide / `q` dismiss | 3 |
| RET switch, k kill, +/c new, g, n/p | 2 (nav), 4 (commands) |
| Hooks: status, create, cleanup, header/usage | 3 |
| Customs width/auto-show/confirm-kill | 1 |
| No interrupt/restart/sort/history | — (omitted) |
| ERT coverage | 2–5 |
| README | 5 |

No intentional placeholders left in steps. Function names are consistent: `agent-ide-sidebar-on-session-created`, `agent-ide-sidebar-on-sessions-changed`, `agent-ide-sidebar--show/hide`.
