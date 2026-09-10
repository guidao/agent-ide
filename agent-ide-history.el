;;; agent-ide-history.el --- Persistent session index -*- lexical-binding: t; -*-

;;; Commentary:
;; Store session identifiers and labels, never backend credentials or transcripts.

;;; Code:

(require 'agent-ide-core)

(defcustom agent-ide-history-file
  (expand-file-name "agent-ide/sessions.json" user-emacs-directory)
  "File containing the local session index, or nil to disable persistence."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'agent-ide)

(defun agent-ide-history-backend-key (command)
  "Return an identity for backend COMMAND without storing its arguments."
  (secure-hash 'sha256 (prin1-to-string command)))

(defun agent-ide-history--valid-entry-p (entry)
  "Return non-nil if ENTRY is a usable history record."
  (and (listp entry)
       (cl-every (lambda (key) (stringp (map-elt entry key)))
                 '(sessionId directory backend backendName title))
       (not (string-empty-p (map-elt entry 'sessionId)))
       (file-name-absolute-p (map-elt entry 'directory))
       (numberp (map-elt entry 'createdAt))
       (numberp (map-elt entry 'updatedAt))))

(defun agent-ide-history-read ()
  "Read the history index as data; signal on invalid data to avoid overwriting it."
  (when (and agent-ide-history-file (file-exists-p agent-ide-history-file))
    (with-temp-buffer
      (insert-file-contents agent-ide-history-file)
      (let ((entries (json-parse-buffer :object-type 'alist :array-type 'array
                                        :null-object nil :false-object nil)))
        (unless (and (vectorp entries)
                     (cl-every #'agent-ide-history--valid-entry-p entries))
          (error "Invalid Agent IDE session index: %s" agent-ide-history-file))
        (append entries nil)))))

(defun agent-ide-history--same-p (a b)
  "Return non-nil if A and B identify the same backend session and directory."
  (cl-every (lambda (key) (equal (map-elt a key) (map-elt b key)))
            '(sessionId directory backend)))

(defun agent-ide-history-record (session &optional prompt)
  "Persist SESSION metadata, using the first PROMPT as its title.
Index failures are reported without interrupting the conversation."
  (when (and agent-ide-history-file
             (agent-ide-session-acp-session-id session)
             (agent-ide--session-metadata-get session :command))
    (condition-case err
        (let* ((command (agent-ide--session-metadata-get session :command))
               (entries (agent-ide-history-read))
               (identity `((sessionId . ,(agent-ide-session-acp-session-id session))
                           (directory . ,(agent-ide-session-directory session))
                           (backend . ,(agent-ide-history-backend-key command))))
               (old (cl-find identity entries :test #'agent-ide-history--same-p))
               (title (or (agent-ide--session-metadata-get session :title)
                          (and old (not (equal (map-elt old 'title) "Untitled"))
                               (map-elt old 'title))
                          (and prompt
                               (truncate-string-to-width
                                (car (split-string (string-trim prompt) "\n"))
                                80 nil nil "…"))
                          "Untitled"))
               (now (float-time))
               (entry (append identity
                              `((backendName . ,(file-name-nondirectory (car command)))
                                (title . ,title)
                                (createdAt . ,(or (map-elt old 'createdAt) now))
                                (updatedAt . ,now))))
               (file (expand-file-name agent-ide-history-file))
               (directory (file-name-directory file))
               temporary)
          (unless (equal title "Untitled")
            (agent-ide--session-metadata-put session :title title))
          (make-directory directory t)
          (unwind-protect
              (progn
                (setq temporary (make-temp-file (expand-file-name ".sessions-" directory)))
                (set-file-modes temporary #o600)
                (let ((coding-system-for-write 'utf-8-unix)
                      (write-region-annotate-functions nil)
                      (write-region-post-annotation-function nil))
                  (write-region
                   (json-serialize
                    (vconcat (cons entry
                                   (cl-remove identity entries
                                              :test #'agent-ide-history--same-p))))
                   nil temporary nil 'silent))
                (rename-file temporary file t))
            (when (and temporary (file-exists-p temporary))
              (delete-file temporary))))
      (error (message "Agent IDE could not save session index: %s"
                      (error-message-string err))))))

(defun agent-ide-history-candidates (&optional directory)
  "Return completion entries, newest first, optionally restricted to DIRECTORY."
  (let* ((entries (agent-ide-history-read))
         (filtered
          (if directory
              (cl-remove-if-not
               (lambda (entry)
                 (equal (file-truename directory)
                        (file-truename (map-elt entry 'directory))))
               entries)
            entries)))
    (mapcar
     (lambda (entry)
       (cons (format "%s  | %s | %s | %s | %s"
                     (map-elt entry 'title)
                     (map-elt entry 'backendName)
                     (abbreviate-file-name (map-elt entry 'directory))
                     (format-time-string "%m-%d %H:%M" (map-elt entry 'updatedAt))
                     (map-elt entry 'sessionId))
             entry))
     (sort filtered (lambda (a b) (> (map-elt a 'updatedAt)
                                     (map-elt b 'updatedAt)))))))

(provide 'agent-ide-history)
;;; agent-ide-history.el ends here
