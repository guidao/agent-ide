;;; agent-ide-session-mode.el --- Major mode for agent transcripts -*- lexical-binding: t; -*-

;;; Commentary:

;; Mode and keymaps for Agent IDE transcript buffers.

;;; Code:

(require 'agent-ide-core)
(require 'agent-ide-renderer)
(require 'button)
(require 'seq)
(require 'subr-x)
(require 'thingatpt)
(require 'valign)

(defun agent-ide--valign-guess-table-type (orig-fn)
  "Return 'markdown for `agent-ide-session-mode' buffers."
  (if (derived-mode-p 'agent-ide-session-mode)
      'markdown
    (funcall orig-fn)))

(advice-add 'valign--guess-table-type :around
            #'agent-ide--valign-guess-table-type)

(declare-function agent-ide-submit "agent-ide-session" ())
(declare-function agent-ide-interrupt "agent-ide-session" ())
(declare-function agent-ide-restart "agent-ide-session" ())
(declare-function agent-ide-set-model "agent-ide-session" (model-id))
(declare-function agent-ide-yank-region "agent-ide-session" ())

(defvar agent-ide-session-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'agent-ide-interrupt)
    (define-key map (kbd "C-c C-k") #'agent-ide-interrupt)
    (define-key map (kbd "C-c C-o") #'agent-ide-follow-thing-at-point)
    (define-key map (kbd "C-c C-r") #'agent-ide-restart)
    (define-key map (kbd "C-c C-y") #'agent-ide-yank-region)
    (define-key map (kbd "C-c C-m") #'agent-ide-submit)
    (define-key map (kbd "C-c C-s") #'agent-ide-set-model)
    map)
  "Keymap for `agent-ide-session-mode'.")

(defvar agent-ide-session-prompt-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-p") #'agent-ide-previous-prompt-history)
    (define-key map (kbd "M-n") #'agent-ide-next-prompt-history)
    (define-key map (kbd "/") #'agent-ide-session-mode-insert-slash)
    (define-key map (kbd "TAB") #'completion-at-point)
    map)
  "Keymap active inside the editable prompt.")

(defvar agent-ide-session-command-description-max-width 60
  "Maximum width for slash command descriptions in completion annotations.")

(define-minor-mode agent-ide-session-prompt-minor-mode
  "Minor mode active inside a Agent IDE prompt."
  :lighter " Prompt"
  :keymap agent-ide-session-prompt-minor-mode-map)

(defun agent-ide-session-mode--point-in-prompt-p ()
  "Return non-nil when point is inside the active prompt."
  (when-let* ((session (agent-ide--session-for-buffer)))
    (let ((input-start (agent-ide-session-input-start-marker session))
          (input-end (agent-ide-session-input-end-marker session)))
      (and (agent-ide-renderer-input-active-p session)
           (markerp input-start)
           (markerp input-end)
           (eq (marker-buffer input-start) (current-buffer))
           (eq (marker-buffer input-end) (current-buffer))
           (>= (point) (marker-position input-start))
           (<= (point) (marker-position input-end))))))

(defun agent-ide-session-mode--point-in-active-input-overlay-p
    (session &optional pos)
  "Return non-nil when POS is inside SESSION's active input overlay."
  (setq pos (or pos (point)))
  (let ((overlay (agent-ide-session-input-overlay session)))
    (and (agent-ide-renderer-input-active-p session)
         (overlayp overlay)
         (eq (overlay-buffer overlay) (current-buffer))
         (overlay-start overlay)
         (overlay-end overlay)
         (<= (overlay-start overlay) pos)
         (<= pos (overlay-end overlay)))))

(defun agent-ide-session-mode--input-end-position (session)
  "Return SESSION active editable input end position, if available."
  (let ((input-end (agent-ide-session-input-end-marker session)))
    (when (and (markerp input-end)
               (eq (marker-buffer input-end) (current-buffer)))
      (marker-position input-end))))

(defun agent-ide-session-mode--change-in-prompt-p (session start end)
  "Return non-nil when START..END is inside SESSION's active prompt."
  (let ((input-start (agent-ide-session-input-start-marker session))
        (input-end (agent-ide-session-input-end-marker session)))
    (and (agent-ide-renderer-input-active-p session)
         (markerp input-start)
         (markerp input-end)
         (eq (marker-buffer input-start) (current-buffer))
         (eq (marker-buffer input-end) (current-buffer))
         (>= start (marker-position input-start))
         (<= end (marker-position input-end)))))

(defun agent-ide-session-mode--command-name (command)
  "Return the slash command name from COMMAND."
  (cond
   ((and (listp command)
         (agent-ide-session-mode--alist-object-p command))
    (map-elt command 'name))
   ((stringp command)
    command)))

(defun agent-ide-session-mode--command-description (command)
  "Return the slash command description from COMMAND."
  (when (and (listp command)
             (agent-ide-session-mode--alist-object-p command))
    (map-elt command 'description)))

(defun agent-ide-session-mode--truncate-command-description (description)
  "Return DESCRIPTION shortened for completion annotations."
  (when (stringp description)
    (let* ((single-line (string-trim
                         (replace-regexp-in-string
                          "[[:space:]\n\r\t]+" " " description)))
           (max-width agent-ide-session-command-description-max-width))
      (if (and (integerp max-width)
               (> max-width 0)
               (> (string-width single-line) max-width))
          (truncate-string-to-width single-line max-width nil nil "...")
        single-line))))

(defun agent-ide-session-mode--alist-object-p (value)
  "Return non-nil if VALUE looks like an alist object."
  (and (listp value)
       (or (null value)
           (consp (car value)))))

(defun agent-ide-session-mode--available-command-list (commands)
  "Return COMMANDS as a plain list."
  (cond
   ((vectorp commands) (append commands nil))
   ((and (listp commands)
         (not (agent-ide-session-mode--alist-object-p commands)))
    commands)
   (commands (list commands))))

(defun agent-ide-session-mode--available-command-candidates (session)
  "Return slash command completion candidates for SESSION."
  (let ((commands (agent-ide--session-metadata-get session :available-commands)))
    (delete-dups
     (seq-sort
      #'string-lessp
      (seq-keep (lambda (command)
                  (when-let* ((name (agent-ide-session-mode--command-name
                                      command)))
                    (concat "/" (string-remove-prefix "/" name))))
                (agent-ide-session-mode--available-command-list commands))))))

(defun agent-ide-session-mode--available-command-annotations (session)
  "Return an alist mapping slash command candidates to descriptions for SESSION."
  (seq-keep (lambda (command)
              (when-let* ((name (agent-ide-session-mode--command-name command))
                          (description
                           (agent-ide-session-mode--truncate-command-description
                            (agent-ide-session-mode--command-description command))))
                (cons (concat "/" (string-remove-prefix "/" name))
                      description)))
            (agent-ide-session-mode--available-command-list
             (agent-ide--session-metadata-get session :available-commands))))

(defun agent-ide-session-mode--slash-completion-bounds (session)
  "Return completion bounds for the active slash command in SESSION."
  (let ((input-start (agent-ide-session-input-start-marker session))
        (input-end (agent-ide-session-input-end-marker session)))
    (when (and (markerp input-start)
               (markerp input-end)
               (<= (marker-position input-start) (point))
               (<= (point) (marker-position input-end)))
      (let ((text (buffer-substring-no-properties
                   (marker-position input-start)
                   (point))))
        (when (and (string-prefix-p "/" text)
                   (not (string-match-p "[[:space:]]" text)))
          (cons (marker-position input-start) (point)))))))

(defun agent-ide-session-mode-completion-at-point ()
  "Complete available agent slash commands in the active prompt."
  (when-let* ((session (agent-ide--session-for-buffer))
              (bounds (agent-ide-session-mode--slash-completion-bounds session))
              (candidates
               (agent-ide-session-mode--available-command-candidates session)))
    (let ((annotations
           (agent-ide-session-mode--available-command-annotations session)))
    (list (car bounds)
          (cdr bounds)
          candidates
          :exclusive 'no
          :annotation-function
          (lambda (candidate)
            (if-let* ((description (alist-get candidate annotations nil nil
                                              #'string=)))
                (concat " " description)
              " command"))))))

(defun agent-ide-session-mode-insert-slash ()
  "Insert slash in the prompt.

Slash command candidates are still provided through
`completion-at-point-functions', but inserting slash does not invoke
`completion-at-point' directly.  This lets completion frontends that
already show a postframe/posframe avoid an extra *Completions* buffer."
  (interactive)
  (self-insert-command 1))

(defun agent-ide-session-mode-sync-prompt-minor-mode ()
  "Synchronize prompt minor mode with point."
  (when (derived-mode-p 'agent-ide-session-mode)
    (when-let* ((session (agent-ide--session-for-buffer)))
      (when-let* ((input-end
                   (agent-ide-session-mode--input-end-position session)))
        (when (and (agent-ide-session-mode--point-in-active-input-overlay-p
                    session)
                   (> (point) input-end))
          (goto-char input-end))))
    (let ((inside (agent-ide-session-mode--point-in-prompt-p)))
      (unless (eq inside agent-ide-session-prompt-minor-mode)
        (agent-ide-session-prompt-minor-mode (if inside 1 -1))))))

(defun agent-ide-session-mode-refresh-placeholder (&rest _args)
  "Refresh the active prompt placeholder."
  (when-let* ((session (agent-ide--session-for-buffer)))
    (agent-ide-renderer-refresh-placeholder session)))

(defun agent-ide-session-mode-style-input (start end _length)
  "Apply prompt styling to newly edited text between START and END."
  (when-let* ((session (agent-ide--session-for-buffer)))
    (agent-ide-renderer-make-input-editable session start end)
    (agent-ide-renderer-style-input-region session start end)))

(defun agent-ide-session-mode-protect-transcript (start end)
  "Reject manual edits outside the active prompt between START and END."
  (unless inhibit-read-only
    (let ((session (agent-ide--session-for-buffer)))
      (unless (and session
                   (agent-ide-session-mode--change-in-prompt-p
                    session start end))
        (signal 'buffer-read-only (list (current-buffer)))))))

(defun agent-ide-previous-prompt-history ()
  "Replace prompt with the previous history entry."
  (interactive)
  (agent-ide-session-mode--cycle-history -1))

(defun agent-ide-next-prompt-history ()
  "Replace prompt with the next history entry."
  (interactive)
  (agent-ide-session-mode--cycle-history 1))

(defun agent-ide-follow-thing-at-point ()
  "Open the Markdown link, bare URL, or local path at point.
Full URLs open with `browse-url'; file paths open with `find-file'."
  (interactive)
  (cond
   ((when-let* ((button (button-at (point)))
                (url (button-get button 'agent-ide-url)))
      (agent-ide-renderer-open-url url)
      t))
   ((when-let* ((url (thing-at-point 'url t)))
      (agent-ide-renderer-open-url url)
      t))
   ((when-let* ((file (thing-at-point 'filename t)))
      (when (or (file-name-absolute-p file)
                (file-exists-p file)
                (file-exists-p (expand-file-name file)))
        (find-file file)
        t)))
   (t
    (user-error "Nothing to follow at point"))))

(defun agent-ide-session-mode--cycle-history (delta)
  "Cycle current prompt history by DELTA."
  (let* ((session (or (agent-ide--session-for-buffer)
                      (user-error "No Agent IDE session")))
         (history (agent-ide-session-prompt-history session)))
    (unless history
      (user-error "No prompt history"))
    (let* ((current (or (agent-ide-session-prompt-history-index session)
                        (if (> delta 0) -1 (length history))))
           (next (max 0 (min (1- (length history)) (+ current delta))))
           (text (nth next history))
           (start (agent-ide-session-input-start-marker session))
           (end (agent-ide-session-input-end-marker session)))
      (setf (agent-ide-session-prompt-history-index session) next)
      (with-current-buffer (agent-ide-session-buffer session)
        (let ((inhibit-read-only t))
          (delete-region (marker-position start) (marker-position end))
          (goto-char (marker-position start))
          (insert text)
          (set-marker end (point))
          (agent-ide-renderer-make-input-editable session)
          (agent-ide-renderer-style-input-region session))))))

(define-derived-mode agent-ide-session-mode text-mode "Agent-IDE"
  "Major mode for agent ACP IDE sessions."
  (setq-local truncate-lines nil)
  (setq-local cursor-type 'bar)
  (add-hook 'post-command-hook
            #'agent-ide-session-mode-sync-prompt-minor-mode
            nil t)
  (add-hook 'post-command-hook
            #'agent-ide-session-mode-refresh-placeholder
            nil t)
  (add-hook 'after-change-functions
            #'agent-ide-session-mode-refresh-placeholder
            nil t)
  (add-hook 'after-change-functions
            #'agent-ide-session-mode-style-input
            nil t)
  (add-hook 'before-change-functions
            #'agent-ide-session-mode-protect-transcript
            nil t)
  (add-hook 'completion-at-point-functions
            #'agent-ide-session-mode-completion-at-point
            nil t)
  (valign-mode 1))

(provide 'agent-ide-session-mode)

;;; agent-ide-session-mode.el ends here
