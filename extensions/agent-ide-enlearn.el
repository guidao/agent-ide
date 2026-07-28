;;; agent-ide-enlearn.el --- English coach pre-submit plugin -*- lexical-binding: t; -*-

;;; Commentary:
;; Optional plugin: translate/polish prompts via gptel, explain in Chinese,
;; then deliver final English through agent-ide-deliver-prompt.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agent-ide-session)

(defgroup agent-ide-enlearn nil
  "English learning pre-submit coach for Agent IDE."
  :group 'agent-ide
  :prefix "agent-ide-enlearn-")

(defun agent-ide-enlearn--mode-for-text (text)
  "Return `translate' if TEXT is mostly Chinese, else `polish'."
  (let* ((han (length (replace-regexp-in-string "[^\u4e00-\u9fff]" "" text)))
         (total (max 1 (length (replace-regexp-in-string "[[:space:]]" "" text)))))
    (if (>= (/ (float han) total) 0.2)
        'translate
      'polish)))

(defun agent-ide-enlearn--parse-response (raw)
  "Parse gptel RAW markdown into plist :final :breakdown :grammar.
:final is nil when the Final section is missing or empty."
  (let* ((final (agent-ide-enlearn--section raw "Final"))
         (breakdown (or (agent-ide-enlearn--section raw "Breakdown") ""))
         (grammar (or (agent-ide-enlearn--section raw "Grammar") "")))
    (list :final (and final (not (string-empty-p (string-trim final)))
                      (string-trim final))
          :breakdown (string-trim breakdown)
          :grammar (string-trim grammar)
          :raw raw)))

;; Prefer portable splitter (Emacs may not like (?s) in string-match):
(defun agent-ide-enlearn--section (raw name)
  "Return body of ## NAME section in RAW, or nil."
  (let ((parts (split-string raw "^## " t))
        found)
    (dolist (part parts found)
      (when (string-match (format "\\`%s[ \t]*\\(?:\n\\|\\'\\)" (regexp-quote name)) part)
        (setq found (string-trim (substring part (match-end 0))))))))

(defcustom agent-ide-enlearn-auto-send nil
  "When non-nil, send Final English without confirmation buttons."
  :type 'boolean
  :group 'agent-ide-enlearn)

(defcustom agent-ide-enlearn-model nil
  "Optional gptel model override for coaching requests."
  :type '(choice (const nil) string)
  :group 'agent-ide-enlearn)

(defcustom agent-ide-enlearn-on-backend-error 'abort
  "Behavior when gptel is unavailable or a request fails."
  :type '(choice (const abort) (const send-original))
  :group 'agent-ide-enlearn)

(defvar-local agent-ide-enlearn--pending nil
  "Plist for in-flight coach turn: :session :original :mode :final ...")

(defvar-local agent-ide-enlearn--request-generation 0
  "Incremented to invalidate in-flight coach gptel callbacks.")

(defvar agent-ide-enlearn--skip-next nil
  "When non-nil, next pre-submit bypasses coaching once.")

(defconst agent-ide-enlearn--system-prompt
  "You help an English learner write coding-agent prompts.
Rules:
1. Output ONLY markdown with headings ## Final, ## Breakdown, ## Grammar.
2. ## Final must be English, imperative, suitable to send to a coding agent.
3. Preserve code fences, file paths, /slash commands, and identifiers unchanged.
4. ## Breakdown and ## Grammar must be in Chinese (medium detail).
5. If input is Chinese, translate intent into Final. If English, polish Final.")

(defun agent-ide-enlearn--build-user-prompt (mode text)
  "Format coaching user message for MODE and TEXT."
  (format "Mode: %s\n\nInput:\n%s" mode text))

(defun agent-ide-enlearn--pending-for (session)
  "Return pending coach plist stored on SESSION's buffer."
  (with-current-buffer (agent-ide-session-buffer session)
    agent-ide-enlearn--pending))

(defun agent-ide-enlearn--set-pending-for (session pending)
  "Store PENDING coach plist on SESSION's buffer."
  (with-current-buffer (agent-ide-session-buffer session)
    (setq agent-ide-enlearn--pending pending)))

(defun agent-ide-enlearn--next-request-id (session)
  "Return a new coach request id for SESSION and bump its generation."
  (with-current-buffer (agent-ide-session-buffer session)
    (cl-incf agent-ide-enlearn--request-generation)))

(defun agent-ide-enlearn--invalidate-pending (session)
  "Drop pending coach state and invalidate in-flight callbacks for SESSION."
  (with-current-buffer (agent-ide-session-buffer session)
    (cl-incf agent-ide-enlearn--request-generation))
  (agent-ide-enlearn--set-pending-for session nil))

(defun agent-ide-enlearn--pending-request-valid-p (session &optional request-id)
  "Return non-nil when SESSION pending matches optional REQUEST-ID."
  (let ((pending (agent-ide-enlearn--pending-for session)))
    (and pending
         (or (null request-id)
             (eq request-id (plist-get pending :request-id))))))

(defun agent-ide-enlearn--pre-submit (session prompt)
  "Pre-submit hook. Return non-nil when handling PROMPT."
  (cond
   ((not agent-ide-enlearn-mode) nil)
   (agent-ide-enlearn--skip-next
    (setq agent-ide-enlearn--skip-next nil)
    nil)
   (t
    (let ((pending (agent-ide-enlearn--pending-for session)))
      (cond
       ((plist-get pending :final)
        (agent-ide-enlearn-send session)
        t)
       (pending
        (user-error "English coach still pending; Send, Edit, Cancel, or interrupt")
        t)
       (t
        (agent-ide-enlearn--begin session prompt)
        t))))))

(defun agent-ide-enlearn--begin (session prompt)
  "Freeze PROMPT and start coaching for SESSION."
  (agent-ide-freeze-user-prompt session prompt)
  (agent-ide--set-status session "coaching")
  (agent-ide-renderer-update-header session)
  (let* ((request-id (agent-ide-enlearn--next-request-id session))
         (pending (list :session session
                        :original prompt
                        :mode (agent-ide-enlearn--mode-for-text prompt)
                        :request-id request-id)))
    (agent-ide-enlearn--set-pending-for session pending)
    (agent-ide-enlearn--request session
                                (plist-get pending :mode)
                                prompt
                                request-id)))

(defun agent-ide-enlearn--backend-unavailable (session)
  "Handle missing gptel for SESSION (Task 5 expands error UI)."
  (agent-ide-enlearn--invalidate-pending session)
  (agent-ide--set-status session "idle")
  (agent-ide-renderer-update-header session)
  (message "gptel unavailable for English coach"))

(cl-defun agent-ide-enlearn--request (session mode text &optional request-id)
  "Ask gptel to coach TEXT for SESSION in MODE."
  (unless (require 'gptel nil t)
    (agent-ide-enlearn--backend-unavailable session)
    (cl-return-from agent-ide-enlearn--request))
  (let* ((pending (agent-ide-enlearn--pending-for session))
         (request-id (or request-id
                          (plist-get pending :request-id)
                          (agent-ide-enlearn--next-request-id session)))
         (prompt (agent-ide-enlearn--build-user-prompt mode text))
         (model (or agent-ide-enlearn-model
                    (when (boundp 'gptel-model) gptel-model))))
    (agent-ide-enlearn--set-pending-for
     session (plist-put (or pending (list :session session))
                        :request-id request-id))
    (gptel-request
     prompt
     :system agent-ide-enlearn--system-prompt
     :model model
     :callback
     (lambda (response _info)
       (when (buffer-live-p (agent-ide-session-buffer session))
         (with-current-buffer (agent-ide-session-buffer session)
           (when (agent-ide-enlearn--pending-request-valid-p session request-id)
             (let ((text (cond ((stringp response) response)
                               ((and (consp response) (stringp (car response)))
                                (mapconcat #'identity response ""))
                               (t ""))))
               (if (string-empty-p (string-trim text))
                   (progn
                     (message "English coach: empty response")
                     (agent-ide-enlearn--invalidate-pending session)
                     (agent-ide--set-status session "idle")
                     (agent-ide-renderer-update-header session))
                 (agent-ide-enlearn--handle-response session text request-id))))))))))

(cl-defun agent-ide-enlearn--handle-response (session raw &optional request-id)
  "Parse RAW gptel reply and render coach block for SESSION."
  (unless (agent-ide-enlearn--pending-request-valid-p session request-id)
    (cl-return-from agent-ide-enlearn--handle-response))
  (let* ((parsed (agent-ide-enlearn--parse-response raw))
         (final (plist-get parsed :final))
         (pending (agent-ide-enlearn--pending-for session)))
    (agent-ide--set-status session "idle")
    (agent-ide-renderer-update-header session)
    (unless final
      (message "English coach: missing Final section")
      (agent-ide-enlearn--invalidate-pending session)
      (cl-return-from agent-ide-enlearn--handle-response))
    (agent-ide-enlearn--set-pending-for
     session (plist-put pending :final final))
    (agent-ide-enlearn--render-coach session parsed)
    (when agent-ide-enlearn-auto-send
      (agent-ide-enlearn-send session))))

(defun agent-ide-enlearn--render-coach (session parsed)
  "Insert English Coach block for PARSED response into SESSION transcript."
  (agent-ide-renderer--with-insertion-point
   session
   (lambda ()
     (let ((start (point)))
       (agent-ide-renderer--insert-read-only
        (format "\n✦ English Coach\nFinal:\n  %s\n\nBreakdown:\n%s\n\nGrammar / collocation:\n%s\n\n"
                (plist-get parsed :final)
                (plist-get parsed :breakdown)
                (plist-get parsed :grammar))
        'face 'agent-ide-muted-face)
       (unless agent-ide-enlearn-auto-send
         (insert-text-button "[Send]" 'follow-link t
                             'keymap agent-ide-action-button-map
                             'action (lambda (_) (agent-ide-enlearn-send session)))
         (insert "  ")
         (insert-text-button "[Edit]" 'follow-link t
                             'keymap agent-ide-action-button-map
                             'action (lambda (_) (agent-ide-enlearn-edit session)))
         (insert "  ")
         (insert-text-button "[Cancel]" 'follow-link t
                             'keymap agent-ide-action-button-map
                             'action (lambda (_) (agent-ide-enlearn-cancel session)))
         (insert "\n"))
       (agent-ide-renderer--freeze-region start (point))))))

(defun agent-ide-enlearn-send (&optional session)
  "Deliver Final English for the pending coach turn."
  (interactive)
  (let* ((session (or session (agent-ide--session-for-buffer)
                        (user-error "No Agent IDE session")))
         (pending (agent-ide-enlearn--pending-for session))
         (final (plist-get pending :final)))
    (unless (and pending final)
      (user-error "No Final English to send"))
    (agent-ide-enlearn--invalidate-pending session)
    (agent-ide-deliver-prompt session final)))

(defun agent-ide-enlearn-edit (&optional session)
  "Put Final into the editable prompt; clear pending without sending."
  (interactive)
  (let* ((session (or session (agent-ide--session-for-buffer)
                        (user-error "No Agent IDE session")))
         (pending (agent-ide-enlearn--pending-for session))
         (final (plist-get pending :final)))
    (unless (and pending final)
      (user-error "No Final English to edit"))
    (agent-ide-enlearn--invalidate-pending session)
    (agent-ide--set-status session "idle")
    (agent-ide-renderer-update-header session)
    (agent-ide-renderer-replace-current-input session final)))

(defun agent-ide-enlearn-cancel (&optional session)
  "Drop pending coach turn without sending."
  (interactive)
  (let ((session (or session (agent-ide--session-for-buffer)
                       (user-error "No Agent IDE session"))))
    (agent-ide--set-status session "idle")
    (agent-ide-renderer-update-header session)
    (agent-ide-enlearn--invalidate-pending session)
    (message "English coach cancelled")))

(defun agent-ide-enlearn--submit-advice (orig &rest _args)
  "Send pending Final when editable prompt is empty."
  (let ((session (agent-ide--session-for-buffer)))
    (if (and session
             (plist-get (agent-ide-enlearn--pending-for session) :final))
        (agent-ide-enlearn-send session)
      (funcall orig))))

;;;###autoload
(define-minor-mode agent-ide-enlearn-mode
  "Translate/polish Agent IDE prompts with gptel before send."
  :global t
  :group 'agent-ide-enlearn
  (if agent-ide-enlearn-mode
      (progn
        (add-hook 'agent-ide-pre-submit-functions
                  #'agent-ide-enlearn--pre-submit)
        (advice-add #'agent-ide-submit :around #'agent-ide-enlearn--submit-advice))
    (progn
      (remove-hook 'agent-ide-pre-submit-functions
                   #'agent-ide-enlearn--pre-submit)
      (advice-remove #'agent-ide-submit #'agent-ide-enlearn--submit-advice))))

;;;###autoload
(defun agent-ide-enlearn-toggle ()
  "Toggle English coach pre-submit mode."
  (interactive)
  (agent-ide-enlearn-mode 'toggle)
  (message "agent-ide-enlearn-mode %s"
           (if agent-ide-enlearn-mode "on" "off")))

;;;###autoload
(defun agent-ide-enlearn-toggle-auto-send ()
  "Toggle auto-send of coached Final English."
  (interactive)
  (setq agent-ide-enlearn-auto-send (not agent-ide-enlearn-auto-send))
  (message "enlearn auto-send %s"
           (if agent-ide-enlearn-auto-send "on" "off")))

;;;###autoload
(defun agent-ide-enlearn-skip-next ()
  "Bypass enlearn for the next submit only."
  (interactive)
  (setq agent-ide-enlearn--skip-next t)
  (message "Next Agent IDE submit skips English coach"))

(when (boundp 'agent-ide-status-placeholder-text-alist)
  (add-to-list 'agent-ide-status-placeholder-text-alist
               '("coaching" . "Coaching...")))

(provide 'agent-ide-enlearn)
