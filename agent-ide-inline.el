;;; agent-ide-inline.el --- In-place region editing with agent-ide -*- lexical-binding: t; -*-

;;; Commentary:

;; gptel-inline-style in-place rewrites backed by an agent-ide session.
;; Select a region, run `agent-ide-inline-rewrite', give an instruction,
;; watch the proposed replacement stream into an overlay, then accept
;; (C-c C-c) or reject (C-c C-k).

;;; Code:

(require 'cl-lib)
(require 'format-spec)
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
   (progn
     (unless (use-region-p)
       (user-error "No region selected"))
     (list (region-beginning) (region-end)
           (read-string "Rewrite instruction: " nil
                        'agent-ide-inline-history))))
  (unless (> end start)
    (user-error "Invalid region"))
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
      (undo-boundary)
      (goto-char end)
      (delete-region start end)
      (goto-char start)
      (insert text)
      (undo-boundary))))

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
