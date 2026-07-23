;;; agent-ide-protocol.el --- Agent Communication Protocol bridge -*- lexical-binding: t; -*-

;;; Commentary:

;; Thin wrappers around acp.el for Agent IDE sessions.

;;; Code:

(require 'acp)
(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'agent-ide-core)
(require 'agent-ide-renderer)

(defvar agent-ide-text-file-capabilities t
  "Whether Agent IDE advertises ACP text-file capabilities.")

(defvar agent-ide-mcp-servers []
  "ACP MCP servers passed to `session/new'.")

(defvar agent-ide-model nil
  "Default model ID applied after session creation.
See also `agent-ide-set-model' for interactive switching.")

(defvar agent-ide-client-info
  '((name . "agent-ide")
    (title . "Emacs Agent IDE")
    (version . "0.1.0"))
  "Client info sent in the ACP initialize request.")

(defun agent-ide-protocol--track-request (session request)
  "Track REQUEST as active for SESSION."
  (setf (agent-ide-session-active-requests session)
        (cons request (agent-ide-session-active-requests session))))

(defun agent-ide-protocol--untrack-request (session request)
  "Remove REQUEST from SESSION active requests."
  (setf (agent-ide-session-active-requests session)
        (cl-remove request
                   (agent-ide-session-active-requests session)
                   :test #'equal)))

(cl-defun agent-ide-protocol-send-request
    (session request &key on-success on-failure)
  "Send ACP REQUEST for SESSION."
  (agent-ide-protocol--track-request session request)
  (acp-send-request
   :client (agent-ide-session-client session)
   :request request
   :buffer (agent-ide-session-buffer session)
   :on-success (lambda (response)
                 (agent-ide-protocol--untrack-request session request)
                 (when on-success
                   (funcall on-success response)))
   :on-failure (lambda (error &optional _raw)
                 (agent-ide-protocol--untrack-request session request)
                 (if on-failure
                     (funcall on-failure error)
                   (agent-ide-renderer-append-error
                    session
                    (or (map-elt error 'message)
                        (format "%S" error)))))))

(defun agent-ide-protocol-initialize (session on-ready)
  "Initialize ACP client for SESSION, then call ON-READY."
  (agent-ide--set-status session "initializing")
  (agent-ide-renderer-update-header session)
  (agent-ide-renderer-append-status session "Initializing agent ACP...")
  (agent-ide-protocol-send-request
   session
   (acp-make-initialize-request
    :protocol-version 1
    :client-info agent-ide-client-info
    :read-text-file-capability agent-ide-text-file-capabilities
    :write-text-file-capability agent-ide-text-file-capabilities)
   :on-success (lambda (response)
                 (setf (agent-ide-session-initialized session) t)
                 (setf (agent-ide-session-capabilities session)
                       (or (map-elt response 'agentCapabilities) response))
                 (agent-ide-renderer-append-status session "ACP initialized.")
                 (funcall on-ready))
   :on-failure (lambda (error)
                 (agent-ide--set-status session "failed")
                 (agent-ide-renderer-update-header session)
                 (agent-ide-renderer-append-error
                  session
                  (or (map-elt error 'message)
                      (format "%S" error))))))

(defun agent-ide-protocol-set-model (session model-id &optional silent)
  "Send `session/set_model' for SESSION with MODEL-ID.
When SILENT is non-nil, suppress status messages."
  (unless (agent-ide-session-acp-session-id session)
    (user-error "Agent session is not ready"))
  (agent-ide-protocol-send-request
   session
   (acp-make-session-set-model-request
    :session-id (agent-ide-session-acp-session-id session)
    :model-id model-id)
   :on-success (lambda (_response)
                 ;; Update cached model id in session models data.
                 (let ((models (agent-ide-session-models session)))
                   (when (listp models)
                     (setf (agent-ide-session-models session)
                           (cons (cons 'currentModelId model-id)
                                 (cl-remove 'currentModelId models
                                            :key #'car :test #'eq)))))
                 (agent-ide-renderer-update-header session)
                 (unless silent
                   (agent-ide-renderer-append-status
                    session
                    (format "Model set: %s" model-id))))
   :on-failure (lambda (error)
                 (unless silent
                   (agent-ide-renderer-append-error
                    session
                    (or (map-elt error 'message)
                        (format "set_model failed: %S" error)))))))

(defun agent-ide-protocol-new-session (session)
  "Create a new ACP session for SESSION."
  (agent-ide--set-status session "creating-session")
  (agent-ide-renderer-update-header session)
  (agent-ide-renderer-append-status session "Creating Agent session...")
  (agent-ide-protocol-send-request
   session
   (acp-make-session-new-request
    :cwd (agent-ide-session-directory session)
    :mcp-servers agent-ide-mcp-servers)
   :on-success (lambda (response)
                 (setf (agent-ide-session-acp-session-id session)
                       (map-elt response 'sessionId))
                 (setf (agent-ide-session-modes session)
                       (map-elt response 'modes))
                 (setf (agent-ide-session-models session)
                       (map-elt response 'models))
                 (agent-ide--set-status session "idle")
                 (agent-ide-renderer-update-header session)
                 (agent-ide-renderer-append-status session "Ready.")
                 (agent-ide-renderer-create-prompt session)
                 ;; Apply default model after session is ready.
                 (when agent-ide-model
                   (agent-ide-protocol-set-model
                    session agent-ide-model 'silent)))
   :on-failure (lambda (error)
                 (agent-ide--set-status session "failed")
                 (agent-ide-renderer-update-header session)
                 (agent-ide-renderer-append-error
                  session
                  (or (map-elt error 'message)
                      (format "%S" error))))))

(defun agent-ide-protocol--prompt-content (prompt)
  "Return ACP content blocks for PROMPT."
  (vector `((type . "text")
            (text . ,(substring-no-properties prompt)))))

(defun agent-ide-protocol-send-prompt (session prompt)
  "Send PROMPT to SESSION."
  (unless (agent-ide-session-acp-session-id session)
    (user-error "Agent session is not ready"))
  (agent-ide--set-status session "running")
  (agent-ide-renderer-update-header session)
  (agent-ide-renderer-reset-stream session)
  (agent-ide-protocol-send-request
   session
   (acp-make-session-prompt-request
    :session-id (agent-ide-session-acp-session-id session)
    :prompt (agent-ide-protocol--prompt-content prompt))
   :on-success (lambda (response)
                 (when-let* ((usage (map-elt response 'usage)))
                   (setf (agent-ide-session-usage session) usage))
                 (agent-ide-renderer-finish-stream session)
                 (agent-ide-renderer-reset-stream session)
                 (agent-ide--set-status session "idle")
                 (agent-ide-renderer-update-header session)
                 (agent-ide-renderer-follow-input session)
                 (when-let* ((stop-reason (map-elt response 'stopReason)))
                   (unless (string= stop-reason "end_turn")
                     (agent-ide-renderer-append-status
                      session
                      (format "Stopped: %s" stop-reason)))))
   :on-failure (lambda (error)
                 (agent-ide-renderer-reset-stream session)
                 (agent-ide--set-status session "idle")
                 (agent-ide-renderer-update-header session)
                 (agent-ide-renderer-follow-input session)
                 (agent-ide-renderer-append-error
                  session
                  (or (map-elt error 'message)
                      (format "%S" error))))))

(defun agent-ide-protocol-cancel (session)
  "Cancel active turn for SESSION."
  (unless (agent-ide-session-acp-session-id session)
    (user-error "No active agent ACP session"))
  (acp-send-notification
   :client (agent-ide-session-client session)
   :notification (acp-make-session-cancel-notification
                  :session-id (agent-ide-session-acp-session-id session)
                  :reason "User cancelled"))
  (agent-ide--set-status session "interrupting")
  (agent-ide-renderer-update-header session)
  (agent-ide-renderer-append-status session "Interrupt requested."))

(defun agent-ide-protocol-respond-permission (session request-id option-id)
  "Respond to SESSION permission REQUEST-ID with OPTION-ID.
When OPTION-ID is nil, cancel the request."
  (acp-send-response
   :client (agent-ide-session-client session)
   :response (acp-make-session-request-permission-response
              :request-id request-id
              :option-id option-id
              :cancelled (not option-id))))

(defun agent-ide-protocol--resolve-path (session path)
  "Resolve PATH relative to SESSION directory."
  (expand-file-name path (agent-ide-session-directory session)))

(defun agent-ide-protocol--extract-buffer-text (buffer line limit)
  "Extract text from BUFFER starting at LINE for LIMIT lines."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (when (and line (> line 1))
          (forward-line (1- line)))
        (let ((start (point)))
          (if limit
              (forward-line limit)
            (goto-char (point-max)))
          (buffer-substring-no-properties start (point)))))))

(defun agent-ide-protocol-handle-fs-read (session request)
  "Handle fs/read_text_file REQUEST for SESSION."
  (condition-case err
      (let* ((path (agent-ide-protocol--resolve-path
                    session
                    (agent-ide--get-in request '(params path))))
             (line (or (agent-ide--get-in request '(params line)) 1))
             (limit (agent-ide--get-in request '(params limit)))
             (buffer (find-buffer-visiting path))
             (content (if buffer
                          (agent-ide-protocol--extract-buffer-text buffer line limit)
                        (with-temp-buffer
                          (insert-file-contents path)
                          (agent-ide-protocol--extract-buffer-text
                           (current-buffer) line limit)))))
        (acp-send-response
         :client (agent-ide-session-client session)
         :response (acp-make-fs-read-text-file-response
                    :request-id (map-elt request 'id)
                    :content content)))
    (file-missing
     (acp-send-response
      :client (agent-ide-session-client session)
      :response (acp-make-fs-read-text-file-response
                 :request-id (map-elt request 'id)
                 :error (acp-make-error
                         :code -32002
                         :message "Resource not found"))))
    (error
     (acp-send-response
      :client (agent-ide-session-client session)
      :response (acp-make-fs-read-text-file-response
                 :request-id (map-elt request 'id)
                 :error (acp-make-error
                         :code -32603
                         :message (error-message-string err)))))))

(defun agent-ide-protocol-handle-fs-write (session request)
  "Handle fs/write_text_file REQUEST for SESSION."
  (condition-case err
      (let* ((path (agent-ide-protocol--resolve-path
                    session
                    (agent-ide--get-in request '(params path))))
             (content (or (agent-ide--get-in request '(params content)) ""))
             (dir (file-name-directory path))
             (buffer (or (find-buffer-visiting path)
                         (find-file-noselect path))))
        (when (and dir (not (file-directory-p dir)))
          (make-directory dir t))
        (with-temp-buffer
          (insert content)
          (let ((source (current-buffer)))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert-buffer-substring source)
                (basic-save-buffer)))))
        (acp-send-response
         :client (agent-ide-session-client session)
         :response (acp-make-fs-write-text-file-response
                    :request-id (map-elt request 'id)))
        (agent-ide-renderer-append-status
         session
         (format "Wrote %s" (abbreviate-file-name path))))
    (error
     (acp-send-response
      :client (agent-ide-session-client session)
      :response (acp-make-fs-write-text-file-response
                 :request-id (map-elt request 'id)
                 :error (acp-make-error
                         :code -32603
                         :message (error-message-string err)))))))

(provide 'agent-ide-protocol)

;;; agent-ide-protocol.el ends here
