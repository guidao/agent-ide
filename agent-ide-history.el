;;; agent-ide-history.el --- Backend session discovery -*- lexical-binding: t; -*-

;;; Commentary:
;; Fetch session metadata over ACP; no local session index is read or written.

;;; Code:

(require 'acp)
(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'agent-ide-core)

(defvar agent-ide-client-info)

(defcustom agent-ide-history-request-timeout 30
  "Maximum seconds to wait for each session discovery request.
Waiting processes Emacs events and can be cancelled with \\[keyboard-quit]."
  :type 'number
  :group 'agent-ide)

(defun agent-ide-history--request (client request)
  "Send REQUEST to discovery CLIENT, waiting with a timeout and quit support."
  (let ((deadline (+ (float-time) agent-ide-history-request-timeout))
        done response failure)
    (acp-send-request
     :client client :request request :buffer (current-buffer)
     :on-success (lambda (value) (setq response value done t))
     :on-failure (lambda (value &optional _raw) (setq failure value done 'error)))
    (while (not done)
      (unless (process-live-p (map-elt client :process))
        (user-error "Agent disconnected while requesting %s" (map-elt request :method)))
      (when (>= (float-time) deadline)
        (user-error "Timed out requesting %s" (map-elt request :method)))
      (accept-process-output nil 0.05))
    (when (eq done 'error)
      (user-error "%s failed: %s" (map-elt request :method)
                  (or (map-elt failure 'message) (format "%S" failure))))
    response))

(defun agent-ide-history-fetch (command environment &optional directory)
  "List sessions from COMMAND with ENVIRONMENT, optionally for DIRECTORY.
Use a temporary connection without creating a conversation.  Fetch every page;
never persist cursors or session metadata.  Always shut down on error or quit."
  (unless command (user-error "`agent-ide-command' is empty"))
  (with-temp-buffer
    (let ((client (acp-make-client
                   :context-buffer (current-buffer)
                   :command (car command) :command-params (cdr command)
                   :environment-variables environment)))
      (unwind-protect
          (let* ((response (agent-ide-history--request
                            client (acp-make-initialize-request
                                    :protocol-version 1
                                    :client-info agent-ide-client-info)))
                 (capabilities (map-elt response 'agentCapabilities))
                 (seen (make-hash-table :test 'equal))
                 (ids (make-hash-table :test 'equal))
                 cursor entries done)
            ;; An advertised empty JSON object decodes to nil: test presence.
            (unless (map-contains-key (map-elt capabilities 'sessionCapabilities) 'list)
              (user-error "This backend does not support session/list"))
            (while (not done)
              (let* ((params (append
                              (when directory
                                `((cwd . ,(directory-file-name (expand-file-name directory)))))
                              (when cursor `((cursor . ,cursor)))))
                     (page (agent-ide-history--request
                            client `((:method . "session/list")
                                     (:params . ,(or params (make-hash-table :test 'equal)))))))
                (unless (map-contains-key page 'sessions)
                  (user-error "Invalid session/list response: missing sessions"))
                (mapc
                 (lambda (entry)
                   (let ((id (map-elt entry 'sessionId)) (cwd (map-elt entry 'cwd)))
                     (unless (and (stringp id) (not (string-empty-p id))
                                  (stringp cwd) (file-name-absolute-p cwd))
                       (user-error "Invalid session/list entry"))
                     (unless (gethash id ids)
                       (puthash id t ids)
                       (push entry entries))))
                 (map-elt page 'sessions))
                (setq cursor (map-elt page 'nextCursor))
                (if (not cursor)
                    (setq done t)
                  (unless (and (stringp cursor) (not (gethash cursor seen)))
                    (user-error "Invalid or repeated session/list cursor"))
                  (puthash cursor t seen))))
            (nreverse entries))
        (ignore-errors (acp-shutdown :client client))
        ;; Ensure cleanup even if an older acp.el cannot shut down a partial start.
        (when (process-live-p (map-elt client :process))
          (delete-process (map-elt client :process)))))))

(defun agent-ide-history-candidates (entries)
  "Return completion candidates for backend ENTRIES, newest first."
  (let ((dated (mapcar
                (lambda (entry)
                  (cons (or (ignore-errors
                              (float-time (date-to-time (map-elt entry 'updatedAt))))
                            0)
                        entry))
                entries)))
    (mapcar
     (lambda (item)
       (let* ((entry (cdr item))
              (title (map-elt entry 'title)))
         (cons (format "%s | %s | %s | %s"
                       (if (and (stringp title) (not (string-empty-p title)))
                           (replace-regexp-in-string "[\n\r\t]+" " " title) "Untitled")
                       (abbreviate-file-name (map-elt entry 'cwd))
                       (if (zerop (car item)) "—"
                         (format-time-string "%m-%d %H:%M" (seconds-to-time (car item))))
                       (map-elt entry 'sessionId))
               entry)))
     (cl-stable-sort dated #'> :key #'car))))

(provide 'agent-ide-history)
;;; agent-ide-history.el ends here
