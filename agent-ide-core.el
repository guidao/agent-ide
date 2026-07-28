;;; agent-ide-core.el --- Core data structures and helpers -*- lexical-binding: t; -*-

;; Author: Feng
;; Keywords: ai, agent, acp

;;; Commentary:

;; Shared session state and small helpers for agent-ide.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'project)
(require 'subr-x)

(defvar agent-ide-buffer-name-prefix "agent"
  "Prefix used when creating Agent IDE session buffers.")

(defvar agent-ide--sessions nil
  "List of live Agent IDE sessions.")

(defvar agent-ide--session-metadata (make-hash-table :test 'eq)
  "Ephemeral metadata keyed by `agent-ide-session' objects.")

(defvar-local agent-ide--session nil
  "Buffer-local Agent IDE session.")

(cl-defstruct (agent-ide-session
               (:constructor agent-ide--make-session))
  directory
  buffer
  client
  acp-session-id
  initialized
  status
  created-at
  input-overlay
  input-prompt-start-marker
  input-start-marker
  input-end-marker
  current-stream-kind
  current-stream-marker
  tool-calls
  active-requests
  capabilities
  modes
  models
  usage
  prompt-history
  prompt-history-index)

(defun agent-ide--timestamp-now ()
  "Return the current timestamp as a display string."
  (format-time-string "%F %T"))

(defun agent-ide--working-directory ()
  "Return the working directory for a new session."
  (file-name-as-directory
   (expand-file-name
    (or (when-let* ((project (project-current nil)))
          (project-root project))
        default-directory))))

(defun agent-ide--directory-name (directory)
  "Return a short display name for DIRECTORY."
  (file-name-nondirectory
   (directory-file-name (expand-file-name directory))))

(defun agent-ide--session-buffer-name (directory)
  "Return a new session buffer name for DIRECTORY."
  (let* ((base (format "*%s:%s*"
                       agent-ide-buffer-name-prefix
                       (agent-ide--directory-name directory)))
         (name base)
         (index 2))
    (while (get-buffer name)
      (setq name (format "%s<%d>" base index)
            index (1+ index)))
    name))

(defun agent-ide--session-for-buffer (&optional buffer)
  "Return Agent IDE session for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (and (boundp 'agent-ide--session)
         (agent-ide-session-p agent-ide--session)
         agent-ide--session)))

(defun agent-ide--current-session ()
  "Return the current Agent IDE session."
  (or (agent-ide--session-for-buffer)
      (cl-find-if (lambda (session)
                    (eq (agent-ide-session-buffer session)
                        (current-buffer)))
                  agent-ide--sessions)))

(defun agent-ide--session-live-p (session)
  "Return non-nil if SESSION still has a live buffer."
  (and (agent-ide-session-p session)
       (buffer-live-p (agent-ide-session-buffer session))))

(defun agent-ide--cleanup-dead-sessions ()
  "Remove dead sessions from `agent-ide--sessions'."
  (setq agent-ide--sessions
        (cl-remove-if-not #'agent-ide--session-live-p agent-ide--sessions)))

(defun agent-ide--session-metadata-get (session key)
  "Return SESSION metadata value for KEY."
  (plist-get (gethash session agent-ide--session-metadata) key))

(defun agent-ide--session-metadata-put (session key value)
  "Set SESSION metadata KEY to VALUE."
  (let ((plist (gethash session agent-ide--session-metadata)))
    (puthash session (plist-put plist key value) agent-ide--session-metadata)
    value))

(defun agent-ide--get-in (object keys)
  "Return nested value from OBJECT following KEYS."
  (let ((value object))
    (while (and keys value)
      (setq value (map-elt value (pop keys))))
    value))

(defun agent-ide--json-string (object)
  "Return OBJECT as a compact JSON string."
  (json-encode object))

(defun agent-ide--touch-session (session)
  "Record that SESSION was active just now."
  (when (agent-ide-session-p session)
    (agent-ide--session-metadata-put session :last-active-at (float-time))))

(defun agent-ide--set-status (session status)
  "Set SESSION status to STATUS."
  (when (agent-ide-session-p session)
    (setf (agent-ide-session-status session) status)
    (agent-ide--touch-session session)
    (when-let* ((buffer (agent-ide-session-buffer session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (fboundp 'agent-ide-renderer-refresh-placeholder)
            (agent-ide-renderer-refresh-placeholder session))
          (force-mode-line-update t))))
    (when (fboundp 'agent-ide-sidebar-on-sessions-changed)
      (agent-ide-sidebar-on-sessions-changed))))

(provide 'agent-ide-core)

;;; agent-ide-core.el ends here
