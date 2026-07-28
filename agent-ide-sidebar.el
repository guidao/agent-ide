;;; agent-ide-sidebar.el --- Session list side window -*- lexical-binding: t; -*-

;;; Commentary:

;; Left sidebar listing live Agent IDE sessions and their status.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agent-ide-core)
(require 'agent-ide-renderer)
(require 'agent-ide-session)

(defgroup agent-ide-sidebar nil
  "Agent IDE session sidebar."
  :group 'agent-ide
  :prefix "agent-ide-sidebar-")

(defcustom agent-ide-sidebar-width 0.14
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
    (define-key map (kbd "g") #'agent-ide-sidebar-refresh)
    (define-key map (kbd "q") #'agent-ide-sidebar-quit)
    map)
  "Keymap for `agent-ide-sidebar-mode'.")

(define-derived-mode agent-ide-sidebar-mode special-mode "Agent-IDE-Sidebar"
  "Major mode for the Agent IDE session sidebar."
  (setq truncate-lines t)
  (setq-local buffer-read-only t)
  (setq-local mode-line-format nil)
  (setq-local agent-ide-sidebar--entries nil))

(defun agent-ide-sidebar--buffer ()
  "Return the sidebar buffer, creating it if needed."
  (or (get-buffer agent-ide-sidebar-buffer-name)
      (with-current-buffer (get-buffer-create agent-ide-sidebar-buffer-name)
        (agent-ide-sidebar-mode)
        (current-buffer))))

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
  (let ((normalized (downcase (or status ""))))
    (cond
     ((member normalized '("idle" "")) 'agent-ide-sidebar-status-idle)
     ((member normalized '("running" "working"))
      'agent-ide-sidebar-status-running)
     ((equal normalized "failed") 'agent-ide-sidebar-status-failed)
     (t 'agent-ide-sidebar-status-other))))

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

(defun agent-ide-sidebar--session-window ()
  "Return the sidebar window when visible."
  (or (get-buffer-window (current-buffer) t)
      (get-buffer-window agent-ide-sidebar-buffer-name t)))

(defun agent-ide-sidebar--line1-fits-p (line window)
  "Return non-nil when LINE fits in WINDOW's body.
Uses pixel widths so wide glyphs like ●/○ are measured correctly.
Leaves a one-character margin so the final `]' is not clipped by
window dividers / truncation glyphs."
  (cond
   ((and window
         (display-graphic-p)
         (fboundp 'string-pixel-width)
         (> (window-body-width window t) 0))
    (let ((margin (frame-char-width (window-frame window))))
      (<= (string-pixel-width line)
          (max 0 (- (window-body-width window t) margin)))))
   (t
    ;; Column fallback.  Tiny side-window widths in batch/TTY frames
    ;; are unreliable, so keep a practical minimum for tests/TTY.
    (let ((cols (if window (window-body-width window) 28)))
      (when (< cols 12)
        (setq cols 28))
      (<= (string-width line) (max 1 (- cols 2)))))))

(defun agent-ide-sidebar--truncate-project (project index status-text &optional dot)
  "Truncate PROJECT so line 1 fits the sidebar width.
INDEX is a numeric buffer index or nil.  STATUS-TEXT is like \"[idle]\".
DOT is the visibility marker (●/○).  Ellipsis is \"..\"."
  (let* ((window (agent-ide-sidebar--session-window))
         (dot (or dot "●"))
         (index-text (if index (format " <%d>" index) ""))
         (ellipsis "..")
         (name project)
         (make-line (lambda (n)
                      (concat dot " " n index-text " " status-text))))
    (while (and (not (agent-ide-sidebar--line1-fits-p
                      (funcall make-line name) window))
                (> (string-width name) (string-width ellipsis)))
      (setq name (truncate-string-to-width
                  name
                  (max (string-width ellipsis)
                       (1- (string-width name)))
                  nil nil ellipsis)))
    (unless (agent-ide-sidebar--line1-fits-p (funcall make-line name) window)
      (setq name ellipsis))
    name))

(defun agent-ide-sidebar--pending-permission-p (session)
  "Return non-nil when SESSION has an unanswered permission prompt."
  (let ((found nil))
    (maphash
     (lambda (_key record)
       (when (and (plist-get record :permission)
                  (plist-get record :pending))
         (setq found t)))
     (agent-ide-session-tool-calls session))
    found))

(defun agent-ide-sidebar--active-tool-title (session)
  "Return title of SESSION's in-progress tool call, or nil."
  (let ((active nil)
        (best-pos -1))
    (maphash
     (lambda (_key record)
       (unless (plist-get record :permission)
         (let* ((status (downcase (or (plist-get record :status) "")))
                (title (plist-get record :title))
                (end (plist-get record :end))
                (pos (and (markerp end) (marker-position end))))
           (when (and title
                      (member status '("pending" "in_progress" "in-progress"
                                       "running")))
             (when (or (null active)
                       (and pos (> pos best-pos)))
               (setq active title
                     best-pos (or pos best-pos)))))))
     (agent-ide-session-tool-calls session))
    active))

(defun agent-ide-sidebar--shorten (text max-width)
  "Shorten TEXT to MAX-WIDTH columns with \"..\"."
  (if (<= (string-width text) max-width)
      text
    (truncate-string-to-width text max-width nil nil "..")))

(defun agent-ide-sidebar--display-status (session)
  "Return sidebar status label for SESSION."
  (if (agent-ide-sidebar--pending-permission-p session)
      "ask"
    (or (agent-ide-session-status session) "unknown")))

(defun agent-ide-sidebar--format-line1 (session selected-p)
  "Return propertized first line for SESSION.
When SELECTED-P is non-nil, apply `agent-ide-sidebar-current'.
Visibility indicator uses ●/○ independently.
Long project names are truncated with \"..\" so status stays visible."
  (let* ((dot (if (agent-ide-sidebar--session-visible-p session) "●" "○"))
         (project (agent-ide--directory-name
                   (agent-ide-session-directory session)))
         (index (agent-ide-sidebar--buffer-index session))
         (status (agent-ide-sidebar--display-status session))
         (status-text (format "[%s]" status))
         (project (agent-ide-sidebar--truncate-project
                   project index status-text dot))
         (prefix (concat dot " " project
                         (if index (format " <%d>" index) "")
                         " "))
         (line (concat prefix status-text)))
    (add-text-properties
     0 (length line)
     (list 'agent-ide-session session
           'agent-ide-sidebar-entry t)
     line)
    (add-face-text-property
     (length prefix) (length line)
     (agent-ide-sidebar--status-face
      (if (equal status "ask") "running" status))
     nil line)
    (when selected-p
      (add-face-text-property 0 (length line) 'agent-ide-sidebar-current t line))
    line))

(defun agent-ide-sidebar--format-line2 (session)
  "Return propertized second line for SESSION (model)."
  (let* ((model (or (agent-ide-renderer--model-label session) "—"))
         (text (concat "  " model)))
    (add-text-properties
     0 (length text)
     (list 'agent-ide-session session
           'agent-ide-sidebar-entry t)
     text)
    text))

(defun agent-ide-sidebar--permission-title (session)
  "Return pending permission title for SESSION, or nil."
  (let ((title nil))
    (maphash
     (lambda (_key record)
       (when (and (null title)
                  (plist-get record :permission)
                  (plist-get record :pending))
         (setq title (or (plist-get record :title) "approval"))))
     (agent-ide-session-tool-calls session))
    title))

(defun agent-ide-sidebar--format-line3 (session)
  "Return propertized third line for SESSION tool/approval, or nil."
  (when-let* ((label
               (cond
                ((agent-ide-sidebar--pending-permission-p session)
                 (agent-ide-sidebar--permission-title session))
                (t (agent-ide-sidebar--active-tool-title session))))
              (label (agent-ide-sidebar--shorten label 24))
              (text (concat "  " label)))
    (add-text-properties
     0 (length text)
     (list 'agent-ide-session session
           'agent-ide-sidebar-entry t)
     text)
    text))

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
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq agent-ide-sidebar--entries sessions)
        (dolist (session sessions)
          (let ((selected (eq session old-session))
                (line3 (agent-ide-sidebar--format-line3 session)))
            (insert (agent-ide-sidebar--format-line1 session selected))
            (insert "\n")
            (insert (agent-ide-sidebar--format-line2 session))
            (insert "\n")
            (when line3
              (insert line3)
              (insert "\n"))))
        (goto-char (point-min))
        (when old-session
          (when-let* ((pos (text-property-any
                            (point-min) (point-max)
                            'agent-ide-session old-session)))
            (goto-char pos)))))))

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
  (cond
   ((agent-ide-sidebar--visible-p)
    (when agent-ide-sidebar-auto-show
      (setq agent-ide-sidebar--user-dismissed nil))
    (agent-ide-sidebar-refresh))
   (agent-ide-sidebar-auto-show
    (setq agent-ide-sidebar--user-dismissed nil)
    (agent-ide-sidebar--show nil))))

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
  "Show or focus the Agent IDE sidebar.
If the sidebar is already visible, select its window."
  (interactive)
  (setq agent-ide-sidebar--user-dismissed nil)
  (if-let* ((window (get-buffer-window agent-ide-sidebar-buffer-name t)))
      (progn
        (agent-ide-sidebar-refresh)
        (select-window window))
    (agent-ide-sidebar--show t)))

(defun agent-ide-sidebar-quit ()
  "Hide the sidebar without killing sessions."
  (interactive)
  (setq agent-ide-sidebar--user-dismissed t)
  (agent-ide-sidebar--hide))

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

(defun agent-ide-sidebar--find-session-window ()
  "Return a window showing an Agent IDE session buffer, or nil.
Never returns the sidebar window."
  (cl-loop for window in (window-list-1 nil nil t)
           for buffer = (window-buffer window)
           when (and (buffer-live-p buffer)
                     (not (eq buffer (get-buffer agent-ide-sidebar-buffer-name)))
                     (with-current-buffer buffer
                       (derived-mode-p 'agent-ide-session-mode)))
           return window))

(defun agent-ide-sidebar--display-session (session)
  "Show SESSION transcript.
Prefer reusing/replacing an existing session window instead of
opening another split."
  (let ((buffer (agent-ide-session-buffer session)))
    (unless (buffer-live-p buffer)
      (user-error "Session buffer is dead"))
    (if-let* ((existing (get-buffer-window buffer t)))
        (progn
          (when agent-ide-select-window-on-open
            (select-window existing))
          existing)
      (if-let* ((window (agent-ide-sidebar--find-session-window)))
          (progn
            (set-window-buffer window buffer)
            (when agent-ide-select-window-on-open
              (select-window window))
            window)
        (agent-ide--display-buffer buffer)))))

(defun agent-ide-sidebar-select ()
  "Display the session at point."
  (interactive)
  (let ((session (or (agent-ide-sidebar--session-at-point)
                     (user-error "No session at point"))))
    (unless (agent-ide--session-live-p session)
      (user-error "Session is dead"))
    (agent-ide-sidebar--display-session session)
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
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (and was-current agent-ide--sessions)
        (when-let* ((next (car agent-ide--sessions))
                    ((agent-ide--session-live-p next)))
          (agent-ide-sidebar--display-session next))))))

(provide 'agent-ide-sidebar)

;;; agent-ide-sidebar.el ends here
