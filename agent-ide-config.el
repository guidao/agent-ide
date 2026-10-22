;;; agent-ide-config.el --- ACP session configuration -*- lexical-binding: t; -*-

;;; Code:
(require 'agent-ide-core)
(require 'seq)
(require 'subr-x)

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

(defun agent-ide-config--text (text)
  "Return TEXT as a compact single-line label."
  (string-trim (replace-regexp-in-string "[[:space:]]+" " " (format "%s" (or text "")))))

(defun agent-ide-config--labels (rows)
  "Give ROWS unique completion labels, disambiguating names only as needed."
  (let ((counts (make-hash-table :test 'equal)) (seen (make-hash-table :test 'equal)))
    (dolist (row rows)
      (puthash (plist-get row :name) (1+ (gethash (plist-get row :name) counts 0)) counts))
    (mapcar
     (lambda (row)
       (let* ((name (plist-get row :name))
              (base (if (> (gethash name counts) 1)
                        (format "%s [%s]" name (agent-ide-config--text (plist-get row :id))) name))
              (label base) (n 2))
         (while (gethash label seen)
           (setq label (format "%s <%d>" base n) n (1+ n)))
         (puthash label t seen)
         (plist-put row :label label))) rows)))

(defun agent-ide-config--value-rows (option)
  "Return structured completion rows for flat, grouped or boolean OPTION."
  (if (equal (map-elt option 'type) "boolean")
      (list (list :name "On" :label "On" :value t :current (eq t (map-elt option 'currentValue)))
            (list :name "Off" :label "Off" :value :false :current (not (eq t (map-elt option 'currentValue)))))
    (let (rows)
      (dolist (item (append (map-elt option 'options) nil))
        (let ((grouped (map-contains-key item 'group)))
          (dolist (value (if grouped (append (map-elt item 'options) nil) (list item)))
            (push (list :name (agent-ide-config--text (or (map-elt value 'name) (map-elt value 'value)))
                        :id (map-elt value 'value) :value (map-elt value 'value)
                        :current (equal (map-elt value 'value) (map-elt option 'currentValue))
                        :description (agent-ide-config--text (map-elt value 'description))
                        :group (and grouped (agent-ide-config--text (map-elt item 'name)))) rows))))
      (agent-ide-config--labels (nreverse rows)))))

(defun agent-ide-config-choices (option)
  "Return concise, unique completion names and protocol values for OPTION."
  (mapcar (lambda (row) (cons (plist-get row :label) (plist-get row :value)))
          (agent-ide-config--value-rows option)))

(defun agent-ide-config-label (option)
  "Return OPTION's current display name, without IDs or descriptions."
  (or (plist-get (seq-find (lambda (row) (plist-get row :current))
                          (agent-ide-config--value-rows option)) :name)
      (agent-ide-config--text (map-elt option 'currentValue))))

(defun agent-ide-config--setting-rows (session)
  "Return SESSION's settings with separate name, current value and description."
  (agent-ide-config--labels
   (mapcar (lambda (option)
             (list :name (agent-ide-config--text (or (map-elt option 'name) (map-elt option 'id)))
                   :id (map-elt option 'id) :value option :detail (agent-ide-config-label option)
                   :description (agent-ide-config--text (map-elt option 'description))))
           (agent-ide-config-options session))))

(defun agent-ide-config--affix (rows candidates values)
  "Affix CANDIDATES using ROWS; VALUES means show a current-value marker."
  (let* ((window (or (active-minibuffer-window) (selected-window)))
         (width (max 20 (- (window-body-width window) 4)))
         (name-width (min 40 (apply #'max 0 (mapcar (lambda (r) (string-width (plist-get r :label))) rows))))
         (detail-width (min 24 (max 4 (/ width 4))
                            (apply #'max 0 (mapcar (lambda (r) (string-width (or (plist-get r :detail) ""))) rows)))))
    (mapcar
     (lambda (candidate)
       (let* ((row (seq-find (lambda (r) (equal candidate (plist-get r :label))) rows))
              (prefix (if values (if (plist-get row :current) (propertize "* " 'face 'success) "  ") ""))
              (padding (make-string (+ 2 (max 0 (- name-width (string-width candidate)))) ?\s))
              (detail (truncate-string-to-width (or (plist-get row :detail) "") detail-width nil nil "…"))
              (column (unless values
                        (concat (propertize detail 'face 'font-lock-constant-face)
                                (make-string (+ 2 (max 0 (- detail-width (string-width detail)))) ?\s))))
              (available (max 0 (- width (string-width prefix) (string-width candidate)
                                   (string-width padding) (string-width (or column "")))))
              (description (truncate-string-to-width (or (plist-get row :description) "") available nil nil "…")))
         (list candidate prefix
               (concat padding column (propertize description 'face 'shadow 'help-echo (plist-get row :description))))))
     candidates)))

(defun agent-ide-config--read (prompt rows &optional values default)
  "Read a ROWS entry with clean completion metadata and PROMPT.
VALUES marks the current value; DEFAULT is its candidate label."
  (let* ((names (mapcar (lambda (r) (plist-get r :label)) rows))
         (metadata
          `((category . agent-ide-config)
            (display-sort-function . identity)
            (cycle-sort-function . identity)
            (affixation-function . ,(lambda (candidates) (agent-ide-config--affix rows candidates values)))
            ,@(when (seq-some (lambda (r) (plist-get r :group)) rows)
                `((group-function . ,(lambda (candidate transform)
                                       (if transform candidate
                                         (plist-get (seq-find (lambda (r) (equal candidate (plist-get r :label))) rows) :group))))))))
         (table (lambda (string predicate action)
                  (if (eq action 'metadata) (cons 'metadata metadata)
                    (complete-with-action action names string predicate))))
         (choice (completing-read prompt table nil t nil nil default)))
    (plist-get (seq-find (lambda (r) (equal choice (plist-get r :label))) rows) :value)))

(defun agent-ide-config-update (session options)
  "Replace SESSION's complete configuration with OPTIONS."
  (agent-ide--session-metadata-put session :config-options options)
  (agent-ide-renderer-update-header session)
  (when (fboundp 'agent-ide-sidebar-on-sessions-changed)
    (agent-ide-sidebar-on-sessions-changed)))

(defun agent-ide-config-read-value (option)
  "Read a value for OPTION, marking its current choice."
  (let* ((rows (agent-ide-config--value-rows option))
         (current (seq-find (lambda (r) (plist-get r :current)) rows)))
    (unless rows (user-error "No values available for %s" (map-elt option 'name)))
    (agent-ide-config--read (format "%s: " (agent-ide-config--text (map-elt option 'name)))
                            rows t (plist-get current :label))))

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
         (rows (agent-ide-config--setting-rows session)))
    (unless rows
      (user-error "No configuration options reported; reconnect this session if it predates the update"))
    (let ((option (agent-ide-config--read "Session setting: " rows)))
      (agent-ide-config-set session (map-elt option 'id) (agent-ide-config-read-value option)))))

(provide 'agent-ide-config)
;;; agent-ide-config.el ends here
