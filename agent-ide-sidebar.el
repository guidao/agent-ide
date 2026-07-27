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
      (let ((inhibit-read-only t))
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
