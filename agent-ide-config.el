;;; agent-ide-config.el --- ACP session configuration -*- lexical-binding: t; -*-

;;; Code:
(require 'agent-ide-core)
(require 'seq)

(declare-function agent-ide-renderer-update-header "agent-ide-renderer" (session))
(declare-function agent-ide-protocol-send-request "agent-ide-protocol" (session request &rest callbacks))
(declare-function agent-ide--find-session-for-buffer "agent-ide-session" ())

(defun agent-ide-config-options (session)
  "Return SESSION's supported configuration options in backend order."
  (seq-filter (lambda (option) (member (map-elt option 'type) '("select" "boolean")))
              (agent-ide--session-metadata-get session :config-options)))

(defun agent-ide-config-category (session category)
  "Return the first option in SESSION with CATEGORY."
  (seq-find (lambda (option) (equal (map-elt option 'category) category))
            (agent-ide-config-options session)))

(defun agent-ide-config-choices (option)
  "Return completion choices for a flat or grouped select OPTION."
  (let (choices)
    (dolist (item (append (map-elt option 'options) nil))
      (dolist (value (if (map-contains-key item 'group)
                         (append (map-elt item 'options) nil) (list item)))
        (push (cons (format "%s%s [%s]%s"
                            (if (map-contains-key item 'group)
                                (concat (map-elt item 'name) " / ") "")
                            (map-elt value 'name) (map-elt value 'value)
                            (if-let* ((description (map-elt value 'description)))
                                (concat " — " description) ""))
                    (map-elt value 'value))
              choices)))
    (nreverse choices)))

(defun agent-ide-config-label (option)
  "Return OPTION's current value label."
  (if (equal (map-elt option 'type) "boolean")
      (if (eq t (map-elt option 'currentValue)) "On" "Off")
    (or (car (rassoc (map-elt option 'currentValue) (agent-ide-config-choices option)))
        (map-elt option 'currentValue))))

(defun agent-ide-config-update (session options)
  "Replace SESSION's complete configuration with OPTIONS."
  (agent-ide--session-metadata-put session :config-options options)
  (agent-ide-renderer-update-header session)
  (when (fboundp 'agent-ide-sidebar-on-sessions-changed)
    (agent-ide-sidebar-on-sessions-changed)))

(defun agent-ide-config-read-value (option)
  "Read a value for OPTION using completion."
  (let* ((boolean (equal (map-elt option 'type) "boolean"))
         (choices (if boolean '(("On" . t) ("Off" . :false))
                    (agent-ide-config-choices option)))
         (current (if boolean
                      (if (eq t (map-elt option 'currentValue)) t :false)
                    (map-elt option 'currentValue))))
    (unless choices (user-error "No values available for %s" (map-elt option 'name)))
    (cdr (assoc (completing-read (format "%s: " (map-elt option 'name))
                                 choices nil t nil nil (car (rassoc current choices)))
                choices))))

(defun agent-ide-config-set (session config-id value)
  "Set SESSION's CONFIG-ID to VALUE; adopt the backend's complete response."
  (unless (and (agent-ide-session-initialized session)
               (member (agent-ide-session-status session) '("idle" "running"))
               (agent-ide-session-acp-session-id session))
    (user-error "Session is not connected and ready for configuration"))
  (let* ((option (seq-find (lambda (o) (equal config-id (map-elt o 'id)))
                           (agent-ide-config-options session)))
         (boolean (equal (map-elt option 'type) "boolean")))
    (unless option (user-error "Unknown session configuration: %s" config-id))
    (unless (if boolean (memq value '(t nil :false))
              (rassoc value (agent-ide-config-choices option)))
      (user-error "Invalid value for %s" config-id))
    (agent-ide-protocol-send-request
     session
     `((:method . "session/set_config_option")
       (:params . ((sessionId . ,(agent-ide-session-acp-session-id session))
                   (configId . ,config-id)
                   ,@(when boolean '((type . "boolean")))
                   (value . ,(if boolean (if (eq value t) t :false) value)))))
     :on-success (lambda (response)
                   (when (map-contains-key response 'configOptions)
                     (agent-ide-config-update session (map-elt response 'configOptions)))))))

;;;###autoload
(defun agent-ide-set-config ()
  "Choose a session setting and value in the minibuffer."
  (interactive)
  (let* ((session (or (agent-ide--find-session-for-buffer) (user-error "No Agent IDE session")))
         (choices (mapcar (lambda (option)
                            (cons (format "%s [%s]: %s%s" (map-elt option 'name)
                                          (map-elt option 'id) (agent-ide-config-label option)
                                          (if-let* ((description (map-elt option 'description)))
                                              (concat " — " description) "")) option))
                          (agent-ide-config-options session))))
    (unless choices
      (user-error "No configuration options reported; reconnect this session if it predates the update"))
    (let ((option (cdr (assoc (completing-read "Session setting: " choices nil t) choices))))
      (agent-ide-config-set session (map-elt option 'id) (agent-ide-config-read-value option)))))

(provide 'agent-ide-config)
;;; agent-ide-config.el ends here
