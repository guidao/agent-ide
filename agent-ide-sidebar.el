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
