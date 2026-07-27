;;; agent-ide-session.el --- Session creation and lifecycle -*- lexical-binding: t; -*-

;;; Commentary:

;; User commands and lifecycle management for Agent IDE sessions.

;;; Code:

(require 'acp)
(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'agent-ide-core)
(require 'agent-ide-protocol)
(require 'agent-ide-renderer)
(require 'agent-ide-session-mode)
(require 'agent-ide-transcript)

(defvar agent-ide-command '("cursor-agent" "acp")
  "Command used to start the agent ACP backend.
The first element is the executable and the rest are arguments.")

(defvar agent-ide-environment nil
  "Environment variables added when starting `agent-ide-command'.")

(defvar agent-ide-new-session-split nil
  "Window split direction for new sessions.
Allowed values are nil, `vertical', and `horizontal'.")

(defvar agent-ide-select-window-on-open t
  "Whether opening a Agent IDE buffer selects its window.")

(defun agent-ide--make-client (session)
  "Create an ACP client for SESSION."
  (unless agent-ide-command
    (error "`agent-ide-command' is empty"))
  (acp-make-client
   :context-buffer (agent-ide-session-buffer session)
   :command (car agent-ide-command)
   :command-params (cdr agent-ide-command)
   :environment-variables agent-ide-environment))

(defun agent-ide--subscribe-client (session)
  "Subscribe SESSION to ACP client events."
  (let ((client (agent-ide-session-client session))
        (buffer (agent-ide-session-buffer session)))
    (acp-subscribe-to-notifications
     :client client
     :buffer buffer
     :on-notification (lambda (notification)
                        (agent-ide-transcript-handle-notification
                         session notification)))
    (acp-subscribe-to-requests
     :client client
     :buffer buffer
     :on-request (lambda (request)
                   (agent-ide-transcript-handle-request session request)))
    (acp-subscribe-to-errors
     :client client
     :buffer buffer
     :on-error (lambda (error)
                 (agent-ide-transcript-handle-error session error)))))

(defun agent-ide--display-buffer (buffer)
  "Display Agent IDE BUFFER."
  (let* ((action (pcase agent-ide-new-session-split
                   ('vertical
                    '((display-buffer-reuse-window
                       display-buffer-in-side-window)
                      (side . right)
                      (window-width . 0.42)))
                   ('horizontal
                    '((display-buffer-reuse-window
                       display-buffer-in-side-window)
                      (side . bottom)
                      (window-height . 0.35)))
                   (_
                    '((display-buffer-reuse-window
                       display-buffer-same-window)))))
         (window (display-buffer buffer action)))
    (when (and window agent-ide-select-window-on-open)
      (select-window window))
    window))

(defun agent-ide--cleanup-session (session)
  "Stop and remove SESSION."
  (when (agent-ide-session-p session)
    (when-let* ((client (agent-ide-session-client session)))
      (ignore-errors
        (acp-shutdown :client client)))
    (remhash session agent-ide--session-metadata)
    (setq agent-ide--sessions (delq session agent-ide--sessions))
    (when (fboundp 'agent-ide-sidebar-on-sessions-changed)
      (agent-ide-sidebar-on-sessions-changed))))

(defun agent-ide--handle-buffer-killed ()
  "Clean up session for the killed buffer."
  (when (agent-ide-session-p agent-ide--session)
    (agent-ide--cleanup-session agent-ide--session)))

(defun agent-ide--cleanup-all-sessions ()
  "Terminate all Agent IDE sessions."
  (dolist (session (copy-sequence agent-ide--sessions))
    (agent-ide--cleanup-session session)))

(add-hook 'kill-emacs-hook #'agent-ide--cleanup-all-sessions)

(defun agent-ide--create-session (&optional directory)
  "Create a Agent IDE session for DIRECTORY."
  (let* ((working-dir (file-name-as-directory
                       (expand-file-name
                        (or directory (agent-ide--working-directory)))))
         (buffer (get-buffer-create
                  (agent-ide--session-buffer-name working-dir)))
         (session (agent-ide--make-session
                   :directory working-dir
                   :buffer buffer
                   :status "starting"
                   :created-at (agent-ide--timestamp-now)
                   :tool-calls (make-hash-table :test 'equal)
                   :prompt-history nil
                   :prompt-history-index nil)))
    (with-current-buffer buffer
      (agent-ide-session-mode)
      (setq-local default-directory working-dir)
      (setq-local agent-ide--session session)
      (add-hook 'kill-buffer-hook #'agent-ide--handle-buffer-killed nil t)
      (agent-ide-renderer-initialize-buffer session))
    (setf (agent-ide-session-client session)
          (agent-ide--make-client session))
    (agent-ide--subscribe-client session)
    (push session agent-ide--sessions)
    (when (fboundp 'agent-ide-sidebar-on-session-created)
      (agent-ide-sidebar-on-session-created session))
    session))

(defun agent-ide--start-session (&optional directory)
  "Start a Agent IDE session for DIRECTORY."
  (let ((session (agent-ide--create-session directory)))
    (agent-ide--display-buffer (agent-ide-session-buffer session))
    (agent-ide-protocol-initialize
     session
     (lambda ()
       (agent-ide-protocol-new-session session)))
    session))

;;;###autoload
(defun agent-ide-new-session (&optional directory)
  "Start a new Agent IDE session.
With prefix argument, prompt for DIRECTORY."
  (interactive
   (list (when current-prefix-arg
           (read-directory-name "Agent IDE directory: "))))
  (agent-ide--start-session directory))

;;;###autoload
(defun agent-ide ()
  "Open a Agent IDE session for the current project."
  (interactive)
  (agent-ide--cleanup-dead-sessions)
  (let* ((directory (agent-ide--working-directory))
         (session (cl-find-if
                   (lambda (candidate)
                     (string= (file-truename directory)
                              (file-truename
                               (agent-ide-session-directory candidate))))
                   agent-ide--sessions)))
    (if (and session
             (buffer-live-p (agent-ide-session-buffer session)))
        (agent-ide--display-buffer (agent-ide-session-buffer session))
      (agent-ide--start-session directory))))

;;;###autoload
(defun agent-ide-submit ()
  "Submit the current Agent IDE prompt."
  (interactive)
  (let* ((session (or (agent-ide--session-for-buffer)
                      (user-error "No Agent IDE session")))
         (prompt (agent-ide-renderer-current-input session)))
    (when (string-empty-p (string-trim prompt))
      (user-error "Prompt is empty"))
    (push prompt (agent-ide-session-prompt-history session))
    (setf (agent-ide-session-prompt-history-index session) nil)
    (agent-ide-renderer-freeze-current-input session)
    (agent-ide-renderer-create-prompt session t)
    (agent-ide-protocol-send-prompt session prompt)))

;;;###autoload
(defun agent-ide-interrupt ()
  "Interrupt the active Agent IDE turn."
  (interactive)
  (let ((session (or (agent-ide--session-for-buffer)
                     (user-error "No Agent IDE session"))))
    (agent-ide-protocol-cancel session)))

;;;###autoload
(defun agent-ide-restart ()
  "Restart the current Agent IDE session."
  (interactive)
  (let* ((session (or (agent-ide--session-for-buffer)
                      (user-error "No Agent IDE session")))
         (directory (agent-ide-session-directory session))
         (buffer (agent-ide-session-buffer session)))
    (agent-ide--cleanup-session session)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    (agent-ide--start-session directory)))

;;; Region context

(defun agent-ide--get-region ()
  "Return an alist describing the active region, or nil.
Keys: :file, :line-start, :line-end, :content."
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      `((:file . ,(buffer-file-name))
        (:line-start . ,(line-number-at-pos start))
        (:line-end . ,(line-number-at-pos end))
        (:content . ,(buffer-substring-no-properties start end))))))

(defun agent-ide--format-region-context (region &optional cwd)
  "Format REGION alist as a context block.
Uses CWD to produce relative paths when possible."
  (let* ((file (map-elt region :file))
         (line-start (map-elt region :line-start))
         (line-end (map-elt region :line-end))
         (content (map-elt region :content))
         (max-preview-lines 20)
         (display-path (if (and cwd file (file-in-directory-p file cwd))
                           (file-relative-name file cwd)
                         (or file "<no-file>"))))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (let ((lines nil)
            (current line-start))
        (while (<= current line-end)
          (let ((line-text (buffer-substring
                            (line-beginning-position)
                            (line-end-position))))
            (push (format "  %d: %s" current line-text) lines))
          (forward-line 1)
          (setq current (1+ current)))
        (setq lines (nreverse lines))
        ;; Trim leading empty lines
        (while (and lines (string-match-p "^  [0-9]+: *$" (car lines)))
          (setq lines (cdr lines)))
        ;; Trim trailing empty lines
        (setq lines (nreverse lines))
        (while (and lines (string-match-p "^  [0-9]+: *$" (car lines)))
          (setq lines (cdr lines)))
        (setq lines (nreverse lines))
        ;; Cap at max-preview-lines
        (when (> (length lines) max-preview-lines)
          (setq lines (append (seq-take lines max-preview-lines)
                              (list "  ..."))))
        (concat display-path ":" (number-to-string line-start) "-"
                (number-to-string line-end) "\n\n"
                (string-join lines "\n"))))))

;;;###autoload
(defun agent-ide-set-model (model-id)
  "Switch the current Agent IDE session to MODEL-ID.
Interactively, choose from the available models reported by the agent."
  (interactive
   (list
    (let* ((session (or (agent-ide--find-session-for-buffer)
                        (user-error "No Agent IDE session")))
           (models-response (agent-ide-session-models session))
           (available (agent-ide-renderer--models-list models-response))
           (candidates (agent-ide-renderer--model-completion-alist
                        available)))
      (unless candidates
        (user-error "No models available from agent"))
      (let ((choice (completing-read "Model: " candidates nil t)))
        (or (cdr (assoc choice candidates)) choice)))))
  (let ((session (or (agent-ide--find-session-for-buffer)
                     (user-error "No Agent IDE session"))))
    (agent-ide-protocol-set-model session model-id)))

(defun agent-ide--find-session-for-buffer ()
  "Find a agent-ide session matching the current buffer's project.
Falls back to the most recently created session."
  (agent-ide--cleanup-dead-sessions)
  (or (agent-ide--session-for-buffer)
      (when-let* ((dir (agent-ide--working-directory)))
        (cl-find-if
         (lambda (s)
           (string= (file-truename dir)
                    (file-truename (agent-ide-session-directory s))))
         agent-ide--sessions))
      (car agent-ide--sessions)))

;;;###autoload
(defun agent-ide-yank-region ()
  "Insert the active region as file+line-number context into a agent-ide prompt.

When called from a agent-ide session buffer, inserts directly.
Otherwise finds the session for the current project.
If no session exists, starts one first."
  (interactive)
  (let ((region (agent-ide--get-region)))
    (unless region
      (user-error "No region selected"))
    (unless agent-ide-command
      (user-error "`agent-ide-command' is empty"))
    (let* ((session (or (agent-ide--find-session-for-buffer)
                        (progn
                          (message "Starting agent-ide session...")
                          (agent-ide--start-session
                           (agent-ide--working-directory)))))
           (text (agent-ide--format-region-context
                  region
                  (agent-ide-session-directory session)))
           (buffer (agent-ide-session-buffer session)))
      ;; Ensure the session is visible
      (unless (get-buffer-window buffer t)
        (agent-ide--display-buffer buffer))
      (with-current-buffer buffer
        (when (agent-ide-renderer-input-active-p session)
          (let ((inhibit-read-only t)
                (input-end (agent-ide-session-input-end-marker session)))
            (goto-char (marker-position input-end))
            (unless (bolp)
              (insert "\n"))
            (insert text)
            (unless (bolp)
              (insert "\n"))
            (set-marker input-end (point))
            (agent-ide-renderer-make-input-editable session)
            (agent-ide-renderer-style-input-region session)
            (agent-ide-renderer-follow-input session)))))))

(provide 'agent-ide-session)

;;; agent-ide-session.el ends here
