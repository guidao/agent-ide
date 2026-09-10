;;; agent-ide-session.el --- Session creation and lifecycle -*- lexical-binding: t; -*-

;;; Commentary:

;; User commands and lifecycle management for Agent IDE sessions.

;;; Code:

(require 'acp)
(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'agent-ide-core)
(require 'agent-ide-history)
(require 'agent-ide-protocol)
(require 'agent-ide-renderer)
(require 'agent-ide-session-mode)
(require 'agent-ide-transcript)

(unless (boundp 'agent-ide-pre-submit-functions)
  (defvar agent-ide-pre-submit-functions nil))

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
  (let ((command (or (agent-ide--session-metadata-get session :command)
                     agent-ide-command)))
    (unless command
      (error "`agent-ide-command' is empty"))
    (acp-make-client
     :context-buffer (agent-ide-session-buffer session)
     :command (car command)
     :command-params (cdr command)
     :environment-variables (agent-ide--session-metadata-get session :environment))))

(defun agent-ide--subscribe-client (session)
  "Subscribe SESSION to ACP client events."
  (let ((client (agent-ide-session-client session))
        (buffer (agent-ide-session-buffer session)))
    (acp-subscribe-to-notifications
     :client client
     :buffer buffer
     :on-notification (lambda (notification)
                        (when (eq client (agent-ide-session-client session))
                          (agent-ide-transcript-handle-notification
                           session notification))))
    (acp-subscribe-to-requests
     :client client
     :buffer buffer
     :on-request (lambda (request)
                   (when (eq client (agent-ide-session-client session))
                     (agent-ide-transcript-handle-request session request))))
    (acp-subscribe-to-errors
     :client client
     :buffer buffer
     :on-error (lambda (error)
                 (when (eq client (agent-ide-session-client session))
                   (agent-ide-transcript-handle-error session error))))))

(defun agent-ide--invalidate-connection (session)
  "Invalidate pending requests and old permission actions in SESSION."
  (agent-ide--session-metadata-put
   session :connection-generation
   (1+ (or (agent-ide--session-metadata-get session :connection-generation) 0)))
  (setf (agent-ide-session-initialized session) nil
        (agent-ide-session-active-requests session) nil)
  (maphash
   (lambda (key record)
     (when (plist-get record :permission)
       (setq record (plist-put record :pending nil))
       (setq record (plist-put record :respond-fn nil)))
     (when (member (plist-get record :status) '("pending" "in_progress" "in-progress" "running"))
       (setq record (plist-put record :status "disconnected")))
     (puthash key record (agent-ide-session-tool-calls session)))
   (agent-ide-session-tool-calls session))
  (agent-ide-renderer-reset-stream session))

(defun agent-ide--watch-client (session)
  "Observe SESSION process exit without replacing ACP's own exit handling."
  (let* ((client (agent-ide-session-client session))
         (process (map-elt client :process)))
    (when (and (processp process) (not (process-get process 'agent-ide-watched)))
      (process-put process 'agent-ide-watched t)
      (let ((previous (process-sentinel process)))
        (set-process-sentinel
         process
         (lambda (proc event)
           (unwind-protect
               (when previous (funcall previous proc event))
             (when (and (memq (process-status proc) '(exit signal closed failed))
                        (eq client (agent-ide-session-client session))
                        (buffer-live-p (agent-ide-session-buffer session)))
               (agent-ide--invalidate-connection session)
               (agent-ide--restore-failed
                session `((message . ,(format "Agent disconnected: %s" (string-trim event)))))))))))))

(defun agent-ide--restore-failed (session error)
  "Leave SESSION available for retry after restoration or connection ERROR."
  (agent-ide--session-metadata-put session :restoring nil)
  (agent-ide--session-metadata-put session :loading-history nil)
  (agent-ide--session-metadata-put session :replay-notifications nil)
  (agent-ide--set-status session "disconnected")
  (agent-ide-renderer-update-header session)
  (agent-ide-renderer-append-error
   session (or (map-elt error 'message) (format "%S" error)))
  (unless (agent-ide-renderer-input-active-p session)
    (agent-ide-renderer-create-prompt session))
  (agent-ide-renderer--with-insertion-point
   session
   (lambda ()
     (dolist (action `(("Retry" . ,(lambda () (agent-ide--resume-session session)))
                       ("Choose session" . ,(lambda () (agent-ide-resume-history)))
                       ("New session" . ,(lambda ()
                                           (agent-ide-new-session
                                            (agent-ide-session-directory session))))))
       (let ((fn (cdr action)))
         (insert-text-button (car action) 'follow-link t
                             'action (lambda (_button) (funcall fn)))
         (insert "  ")))
     (insert "\n"))))

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
      ;; Late callbacks from shutdown must not modify the buffer or metadata.
      (setf (agent-ide-session-client session) nil)
      (ignore-errors
        (acp-shutdown :client client)))
    (remhash session agent-ide--session-metadata)
    (setq agent-ide--sessions (delq session agent-ide--sessions))
    (unless agent-ide--sessions
      (agent-ide-renderer--stop-header-icon-animation))
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

(defun agent-ide--create-session (&optional directory defer-client)
  "Create an Agent IDE session for DIRECTORY.
When DEFER-CLIENT is non-nil, restoration will create the ACP client later."
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
    (agent-ide--session-metadata-put session :command (copy-tree agent-ide-command))
    (agent-ide--session-metadata-put session :environment (copy-tree agent-ide-environment))
    (agent-ide--session-metadata-put session :mcp-servers (copy-tree agent-ide-mcp-servers t))
    (with-current-buffer buffer
      (agent-ide-session-mode)
      (setq-local default-directory working-dir)
      (setq-local agent-ide--session session)
      (add-hook 'kill-buffer-hook #'agent-ide--handle-buffer-killed nil t)
      (agent-ide-renderer-initialize-buffer session))
    (unless defer-client
      (setf (agent-ide-session-client session)
            (agent-ide--make-client session))
      (agent-ide--subscribe-client session))
    (push session agent-ide--sessions)
    (agent-ide--touch-session session)
    (agent-ide-renderer--ensure-header-icon-animation)
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

(defun agent-ide-freeze-user-prompt (session prompt)
  "Record PROMPT in SESSION history and freeze it as a submitted user line.
Creates a fresh editable prompt.  Does not send to the agent."
  (agent-ide-renderer-replace-current-input session prompt)
  (push prompt (agent-ide-session-prompt-history session))
  (setf (agent-ide-session-prompt-history-index session) nil)
  (agent-ide-renderer-freeze-current-input session)
  (agent-ide-renderer-create-prompt session t))

(defun agent-ide-deliver-prompt (session prompt)
  "Freeze PROMPT as a user line in SESSION and send it via ACP."
  (agent-ide--assert-ready session)
  (agent-ide-freeze-user-prompt session prompt)
  (agent-ide-protocol-send-prompt session prompt))

;;;###autoload
(defun agent-ide-submit ()
  "Submit the current Agent IDE prompt."
  (interactive)
  (let* ((session (or (agent-ide--session-for-buffer)
                      (user-error "No Agent IDE session")))
         (prompt (agent-ide-renderer-current-input session)))
    (agent-ide--assert-ready session)
    (when (string-empty-p (string-trim prompt))
      (user-error "Prompt is empty"))
    (unless (run-hook-with-args-until-success
             'agent-ide-pre-submit-functions session prompt)
      (agent-ide-deliver-prompt session prompt))))

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

(defun agent-ide--resume-session (session)
  "Reconnect SESSION in its existing buffer, preserving the current draft."
  (unless (agent-ide-session-acp-session-id session)
    (user-error "This session has no saved ID; create a new session"))
  (when (member (agent-ide-session-status session)
                '("running" "interrupting" "initializing" "resuming" "creating-session"))
    (user-error "Session is %s" (agent-ide-session-status session)))
  (unless (file-directory-p (agent-ide-session-directory session))
    (user-error "Session directory no longer exists: %s"
                (agent-ide-session-directory session)))
  (agent-ide--display-buffer (agent-ide-session-buffer session))
  (if (and (equal (agent-ide-session-status session) "idle")
           (process-live-p (map-elt (agent-ide-session-client session) :process)))
      (message "Session is already connected")
    (let ((old-client (agent-ide-session-client session)))
      (setf (agent-ide-session-client session) nil)
      (when old-client (ignore-errors (acp-shutdown :client old-client))))
    (agent-ide--invalidate-connection session)
    (agent-ide--session-metadata-put session :restoring t)
    (agent-ide--session-metadata-put session :available-commands nil)
    (agent-ide--set-status session "resuming")
    (agent-ide-renderer-append-status session "Restoring session...")
    (unless (agent-ide-renderer-input-active-p session)
      (agent-ide-renderer-create-prompt session))
    (condition-case err
        (progn
          (setf (agent-ide-session-client session) (agent-ide--make-client session))
          (agent-ide--subscribe-client session)
          (agent-ide-protocol-initialize
           session (lambda () (agent-ide-protocol-restore-session session))))
      (error (agent-ide--restore-failed
              session `((message . ,(error-message-string err))))))))

(defun agent-ide--resume-entry (entry)
  "Open or restore the historical session described by ENTRY."
  (agent-ide--cleanup-dead-sessions)
  (let* ((id (map-elt entry 'sessionId))
         (directory (map-elt entry 'directory))
         (backend (map-elt entry 'backend))
         (existing
          (cl-find-if
           (lambda (session)
             (and (equal id (agent-ide-session-acp-session-id session))
                  (equal directory (agent-ide-session-directory session))
                  (equal backend
                         (agent-ide-history-backend-key
                          (agent-ide--session-metadata-get session :command)))))
           agent-ide--sessions)))
    (unless (file-directory-p directory)
      (user-error "Session directory no longer exists: %s" directory))
    (unless (or existing
                (equal backend (agent-ide-history-backend-key agent-ide-command)))
      (user-error "Configure agent-ide-command for this session's backend (%s) first"
                  (map-elt entry 'backendName)))
    (let ((session (or existing (agent-ide--create-session directory t))))
      (unless existing
        (setf (agent-ide-session-acp-session-id session) id)
        (unless (equal (map-elt entry 'title) "Untitled")
          (agent-ide--session-metadata-put session :title (map-elt entry 'title)))
        (agent-ide--set-status session "disconnected"))
      (if (and existing
               (not (member (agent-ide-session-status existing) '("disconnected" "failed")))
               (process-live-p (map-elt (agent-ide-session-client existing) :process)))
          (agent-ide--display-buffer (agent-ide-session-buffer existing))
        (agent-ide--resume-session session))
      session)))

;;;###autoload
(defun agent-ide-resume-history (&optional all-projects directory)
  "Choose history for DIRECTORY or this project; a prefix uses ALL-PROJECTS."
  (interactive "P")
  (let* ((directory (unless all-projects
                      (or directory
                          (when-let* ((session (agent-ide--session-for-buffer)))
                            (agent-ide-session-directory session))
                          (agent-ide--working-directory))))
         (candidates (agent-ide-history-candidates directory)))
    (unless candidates
      (user-error "No saved sessions%s"
                  (if directory " in this project; use C-u M-x agent-ide-resume for all projects" "")))
    (agent-ide--resume-entry
     (cdr (assoc (completing-read "Resume session: " candidates nil t) candidates)))))

;;;###autoload
(defun agent-ide-resume (&optional all-projects)
  "Resume this disconnected session, otherwise choose a saved session.
With a prefix argument, always choose from all projects."
  (interactive "P")
  (let ((session (agent-ide--session-for-buffer)))
    (if (and (not all-projects) session
             (member (agent-ide-session-status session) '("disconnected" "failed")))
        (agent-ide--resume-session session)
      (agent-ide-resume-history all-projects))))

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
