;;; agent-ide-elicitation.el --- ACP structured questions -*- lexical-binding: t; -*-

;;; Code:
(require 'acp)
(require 'agent-ide-core)
(require 'agent-ide-renderer)
(require 'wid-edit)
(require 'seq)

(defun agent-ide-elicitation--table (session)
  "Return SESSION's pending form table."
  (or (agent-ide--session-metadata-get session :elicitations)
      (let ((table (make-hash-table :test 'equal)))
        (agent-ide--session-metadata-put session :elicitations table)
        table)))

(defun agent-ide-elicitation--choices (schema)
  "Return (LABEL . VALUE) pairs for an enum SCHEMA."
  (or (mapcar (lambda (entry) (cons (or (map-elt entry 'title) (map-elt entry 'const))
                                    (map-elt entry 'const)))
              (append (or (map-elt schema 'oneOf) (map-elt schema 'anyOf)) nil))
      (mapcar (lambda (value) (cons value value)) (append (map-elt schema 'enum) nil))))

(defun agent-ide-elicitation--validate-schema (schema)
  "Reject unsupported form SCHEMA before offering a response."
  (unless (equal (map-elt schema 'type) "object")
    (error "Expected a flat object form"))
  (dolist (property (map-elt schema 'properties))
    (let ((field (cdr property)))
      (unless (member (map-elt field 'type) '("string" "number" "integer" "boolean" "array"))
        (error "Unsupported field type: %s" (map-elt field 'type)))
      (when (and (equal (map-elt field 'type) "array")
                 (not (agent-ide-elicitation--choices (map-elt field 'items))))
        (error "Array fields must contain enum choices")))))

(defun agent-ide-elicitation--value (schema raw)
  "Validate and encode widget RAW according to field SCHEMA."
  (let* ((type (map-elt schema 'type))
         (choices (agent-ide-elicitation--choices
                   (if (equal type "array") (map-elt schema 'items) schema)))
         (value
          (pcase type
            ("boolean" (if raw t :false))
            ((or "number" "integer")
             (unless (string-match-p "\\`-?\\(?:0\\|[1-9][0-9]*\\)\\(?:\\.[0-9]+\\)?\\(?:[eE][+-]?[0-9]+\\)?\\'" raw)
               (user-error "Enter a valid number"))
             (let ((n (string-to-number raw)))
               (when (and (equal type "integer") (/= n (truncate n)))
                 (user-error "Enter a whole number"))
               (if (equal type "integer") (truncate n) n)))
            ("array" (vconcat raw))
            (_ raw))))
    (when choices
      (dolist (v (if (equal type "array") (append value nil) (list value)))
        (unless (rassoc v choices) (user-error "Choose a listed value"))))
    (let* ((measure (pcase type ("string" (length value)) ("array" (length value))
			   ((or "number" "integer") value)))
           (min (map-elt schema (pcase type ("string" 'minLength) ("array" 'minItems) (_ 'minimum))))
           (max (map-elt schema (pcase type ("string" 'maxLength) ("array" 'maxItems) (_ 'maximum)))))
      (when (and measure min (< measure min)) (user-error "Minimum is %s" min))
      (when (and measure max (> measure max)) (user-error "Maximum is %s" max)))
    value))

(defun agent-ide-elicitation--pending-p (session record)
  "Whether RECORD still belongs to SESSION's current connection."
  (and (plist-get record :pending)
       (eq (plist-get record :client) (agent-ide-session-client session))
       (eq record (gethash (plist-get record :id) (agent-ide-elicitation--table session)))))

(defun agent-ide-elicitation-finish (session record action &optional content)
  "Reply to RECORD once with ACTION and optional CONTENT."
  (unless (agent-ide-elicitation--pending-p session record)
    (user-error "This question is no longer pending"))
  (setf (plist-get record :pending) nil)
  (condition-case err
      (acp-send-response :client (plist-get record :client)
                         :response `((:request-id . ,(plist-get record :id))
                                     (:result . ((action . ,action)
                                                 ,@(when content `((content . ,content)))))))
    (error
     (when (eq (plist-get record :client) (agent-ide-session-client session))
       (setf (plist-get record :pending) t))
     (signal (car err) (cdr err))))
  (remhash (plist-get record :id) (agent-ide-elicitation--table session))
  (agent-ide-renderer-update-tool session (plist-get record :key)
                                  "Question" (format "Response: %s" action) "completed")
  (when (buffer-live-p (plist-get record :buffer))
    (quit-windows-on (plist-get record :buffer) t)))

(defun agent-ide-elicitation-cancel-all (session &optional request-id)
  "Cancel SESSION's forms, optionally only REQUEST-ID."
  (let (records failure)
    (maphash (lambda (id record)
               (when (or (null request-id) (equal id request-id)) (push record records)))
             (agent-ide-elicitation--table session))
    (dolist (record records)
      (when (agent-ide-elicitation--pending-p session record)
        (condition-case err (agent-ide-elicitation-finish session record "cancel")
          (error (setq failure err)))))
    (when failure (signal (car failure) (cdr failure)))))

(defun agent-ide-elicitation-discard (session)
  "Invalidate SESSION's forms after disconnect, without sending to a new client."
  (maphash (lambda (_id record)
             (setf (plist-get record :pending) nil)
             (when (buffer-live-p (plist-get record :buffer))
               (quit-windows-on (plist-get record :buffer) t)))
           (agent-ide-elicitation--table session))
  (clrhash (agent-ide-elicitation--table session)))

(defun agent-ide-elicitation--submit (session record fields)
  "Collect FIELDS for RECORD, validate, and submit the reviewed values."
  (let ((content (make-hash-table :test 'equal)))
    (dolist (field fields)
      (pcase-let ((`(,name ,schema ,widget ,include) field))
        (when (cond ((eq include t) t)
                    ((functionp include) (funcall include))
                    (t (widget-value include)))
          (condition-case err
              (puthash name (agent-ide-elicitation--value schema (widget-value widget)) content)
            (error (user-error "%s: %s" name (error-message-string err)))))))
    (agent-ide-elicitation-finish session record "accept" content)))

(defun agent-ide-elicitation--custom-fields (schema)
  "Return custom-field/question name pairs for SCHEMA's Other answers.
Recognize the paired fields supplied by the question adapter, leaving
unrelated optional text fields alone."
  (let ((properties (map-elt schema 'properties)) pairs)
    (dolist (property properties)
      (let* ((name (format "%s" (car property)))
             (field (cdr property))
             (custom-name (concat name "_custom"))
             (custom (cdr (seq-find
                           (lambda (entry) (equal (format "%s" (car entry)) custom-name))
                           properties))))
        (when (and (equal (map-elt field 'type) "string")
                   (rassoc "Other (write below)" (agent-ide-elicitation--choices field))
                   (equal (map-elt custom 'type) "string")
                   (not (agent-ide-elicitation--choices custom))
                   (not (member custom-name (append (map-elt schema 'required) nil))))
          (push (cons custom-name name) pairs))))
    pairs))

(defun agent-ide-elicitation--custom-input (choice schema)
  "Create an input for SCHEMA beneath CHOICE, visible only for Other.
Return (WIDGET INCLUDE), retaining the draft when another answer is chosen."
  (let* ((start (point))
         (input (widget-create 'editable-field :size 60 :format "%v\n"
                               (or (map-elt schema 'default) "")))
         (overlay (make-overlay start (point)))
         (include (lambda () (equal (widget-value choice) "Other (write below)")))
         (update (lambda (&optional focus)
                   (if (funcall include)
                       (progn
                         (overlay-put overlay 'invisible nil)
                         (widget-apply input :activate)
                         (when focus (goto-char (widget-field-start input))))
                     (widget-apply input :deactivate)
                     (overlay-put overlay 'invisible t)))))
    (widget-put choice :notify (lambda (&rest _) (funcall update t)))
    (funcall update)
    (list input include)))

(defun agent-ide-elicitation-open (session record)
  "Open an editable review form for pending RECORD from SESSION."
  (unless (agent-ide-elicitation--pending-p session record)
    (user-error "This question is no longer pending"))
  (if (buffer-live-p (plist-get record :buffer))
      (pop-to-buffer (plist-get record :buffer))
    (let* ((buffer (generate-new-buffer "*Agent question*"))
           (schema (plist-get record :schema))
           (custom-fields (agent-ide-elicitation--custom-fields schema)) fields)
      (setf (plist-get record :buffer) buffer)
      (with-current-buffer buffer
        (kill-all-local-variables)
        (widget-insert (format "Agent: %s\nSession: %s\n\n%s\n\n"
                               (or (agent-ide--get-in (agent-ide--session-metadata-get session :agent-info) '(title))
                                   (car (agent-ide--session-metadata-get session :command)) "Agent")
                               (buffer-name (agent-ide-session-buffer session))
                               (plist-get record :message)))
        (dolist (property (seq-remove
                          (lambda (entry) (assoc (format "%s" (car entry)) custom-fields))
                          (map-elt schema 'properties)))
          (let* ((name (if (symbolp (car property)) (symbol-name (car property)) (car property)))
                 (field (cdr property))
                 (type (map-elt field 'type))
                 (required (member name (append (map-elt schema 'required) nil)))
                 (default (map-elt field 'default))
                 (choices (agent-ide-elicitation--choices
                           (if (equal type "array") (map-elt field 'items) field)))
                 (include (if required t
                            (widget-create 'checkbox (map-contains-key field 'default))))
                 widget)
            (widget-insert (format "%s%s\n" (if required "" " Include: ") (or (map-elt field 'title) name)))
            (when-let* ((description (map-elt field 'description)))
              (widget-insert (concat description "\n")))
            (setq widget
                  (cond
                   ((equal type "boolean") (widget-create 'checkbox (eq default t)))
                   ((equal type "array")
                    (apply #'widget-create 'checklist :value (append default nil)
                           (mapcar (lambda (choice) `(const :tag ,(car choice) ,(cdr choice))) choices)))
                   (choices
                    (apply #'widget-create 'radio-button-choice
                           :value (or default (cdar choices))
                           :button-args '(:on "[X]" :off "[ ]"
                                          :on-glyph "checked" :off-glyph "unchecked")
                           (mapcar (lambda (choice) `(const :tag ,(car choice) ,(cdr choice))) choices)))
                   (t (widget-create 'editable-field :size 60
                                     (if (null default) "" (format "%s" default))))))
            (push (list name field widget include) fields)
            (when-let* ((custom-name (car (rassoc name custom-fields)))
                        (custom-schema
                         (cdr (seq-find
                               (lambda (entry) (equal (format "%s" (car entry)) custom-name))
                               (map-elt schema 'properties)))))
              (pcase-let ((`(,input ,selected)
                           (agent-ide-elicitation--custom-input widget custom-schema)))
                (push (list custom-name custom-schema input selected) fields)))
            (widget-insert "\n\n")))
        (widget-create 'push-button :notify (lambda (&rest _) (agent-ide-elicitation--submit session record fields)) "Submit")
        (widget-insert "  ")
        (widget-create 'push-button :notify (lambda (&rest _) (agent-ide-elicitation-finish session record "decline")) "Decline")
        (widget-insert "  ")
        (widget-create 'push-button :notify (lambda (&rest _) (agent-ide-elicitation-finish session record "cancel")) "Cancel")
        (widget-insert "\n\nC-c C-c: Submit    C-c C-k: Cancel\n")
        (use-local-map (copy-keymap widget-keymap))
        (local-set-key (kbd "C-c C-c") (lambda () (interactive) (agent-ide-elicitation--submit session record fields)))
        (local-set-key (kbd "C-c C-k") (lambda () (interactive) (agent-ide-elicitation-finish session record "cancel")))
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (when (agent-ide-elicitation--pending-p session record)
                      ;; Clear the buffer reference to prevent recursive kill.
                      (setf (plist-get record :buffer) nil)
                      (ignore-errors (agent-ide-elicitation-finish session record "cancel")))) nil t)
        (setf (plist-get record :fields) fields)
        (widget-setup)
        (goto-char (point-min)))
      (pop-to-buffer buffer))))

(defun agent-ide-elicitation-handle (session request)
  "Queue a form REQUEST without opening a minibuffer in a process filter."
  (let* ((params (map-elt request 'params))
         (schema (map-elt params 'requestedSchema))
         (id (map-elt request 'id))
         (validation-error
          (condition-case err
              (progn
                (unless (and (equal (map-elt params 'mode) "form")
                             (or (and (map-elt params 'sessionId)
                                      (equal (map-elt params 'sessionId) (agent-ide-session-acp-session-id session)))
                                 (and (not (map-elt params 'sessionId)) (map-contains-key params 'requestId))))
                  (error "Unsupported elicitation mode or scope"))
                (agent-ide-elicitation--validate-schema schema)
                (when (gethash id (agent-ide-elicitation--table session))
                  (error "Duplicate pending question ID"))
                nil)
            (error (error-message-string err)))))
    (if validation-error
        (acp-send-response :client (agent-ide-session-client session)
                           :response `((:request-id . ,id)
                                       (:error . ((code . -32602) (message . ,validation-error)))))
      (let* ((key (format "question-%s-%s" (agent-ide--session-metadata-get session :connection-generation) id))
             (record (list :id id :client (agent-ide-session-client session) :pending t
                           :key key :schema schema :message (map-elt params 'message) :buffer nil :fields nil)))
        (puthash id record (agent-ide-elicitation--table session))
        (agent-ide-renderer-update-tool session key "Question" (plist-get record :message) "pending")
        (agent-ide-renderer--with-insertion-point
         session (lambda ()
                   (insert-text-button "[Answer question]" 'follow-link t
                                       'action (lambda (_) (agent-ide-elicitation-open session record)))
                   (insert "\n")))
        (if (equal (agent-ide-session-status session) "interrupting")
            (agent-ide-elicitation-finish session record "cancel")
          (message "Agent has a question; use C-c C-e to answer"))))))

;;;###autoload
(defun agent-ide-answer-question ()
  "Choose and open a pending structured question in the current session."
  (interactive)
  (let ((session (or (agent-ide--session-for-buffer) (user-error "No Agent IDE session"))) choices)
    (maphash (lambda (id record)
               (when (agent-ide-elicitation--pending-p session record)
                 (push (cons (format "%s [%s]" (plist-get record :message) id) record) choices)))
             (agent-ide-elicitation--table session))
    (unless choices (user-error "No pending questions"))
    (agent-ide-elicitation-open
     session (if (= (length choices) 1) (cdar choices)
               (cdr (assoc (completing-read "Question: " choices nil t) choices))))))

(provide 'agent-ide-elicitation)
;;; agent-ide-elicitation.el ends here
