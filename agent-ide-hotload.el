;;; agent-ide-hotload.el --- Hot-reload agent-ide project files -*- lexical-binding: t -*-

;;; Commentary:

;; Commands for quick iterative development: load-file one file
;; or reload the entire project without restarting Emacs.

;;; Code:

(defgroup agent-ide-hotload nil
  "Hot-reload support for agent-ide development."
  :prefix "agent-ide-hotload-")

(defcustom agent-ide-hotload-project-dir
  (when load-file-name
    (file-name-directory load-file-name))
  "Root directory of the agent-ide project files."
  :type 'directory
  :group 'agent-ide-hotload)

(defvar agent-ide-hotload--files
  '("agent-ide-core.el"
    "agent-ide-protocol.el"
    "agent-ide-renderer.el"
    "agent-ide-session-mode.el"
    "agent-ide-session.el"
    "agent-ide-transcript.el"
    "agent-ide-sidebar.el"
    "agent-ide.el")
  "Project source files in load order (respecting dependencies).")

(defvar agent-ide-hotload--last-file nil
  "Last file loaded by `agent-ide-load-file'.")

(defun agent-ide-hotload--ensure-dir ()
  "Ensure `agent-ide-hotload-project-dir' is set."
  (unless (and (stringp agent-ide-hotload-project-dir)
               (file-directory-p agent-ide-hotload-project-dir))
    (setq agent-ide-hotload-project-dir
          (read-directory-name "agent-ide project directory: "
                               default-directory nil t))))

;;;###autoload
(defun agent-ide-load-file (filename)
  "Hot-reload FILENAME by evaluating its contents.
Interactively, prompt for a project .el file (autocompletes sources).
Bound to C-c C-l in `agent-ide-session-mode'.

When called from a project buffer, offers completion on all agent-ide
source files.  The file is re-evaluated in full, so redefinitions take
effect immediately — no need to restart Emacs."
  (interactive
   (list
    (progn
      (agent-ide-hotload--ensure-dir)
      (let ((default (when (and agent-ide-hotload--last-file
                                (file-exists-p agent-ide-hotload--last-file))
                       (file-name-nondirectory agent-ide-hotload--last-file))))
        (expand-file-name
         (completing-read
          (format-prompt "Hot-reload file" default)
          (agent-ide-hotload--project-source-files)
          nil t nil nil default)
         agent-ide-hotload-project-dir)))))
  (let ((inhibit-message nil))
    (unless (file-readable-p filename)
      (user-error "Cannot read file: %s" filename))
    (message "Loading %s ..." (file-name-nondirectory filename))
    (condition-case err
        (progn
          (load-file filename)
          (setq agent-ide-hotload--last-file filename)
          (message "Loaded %s ✓" (file-name-nondirectory filename)))
      (error
       (message "Error loading %s: %s"
                (file-name-nondirectory filename)
                (error-message-string err))))))

;;;###autoload
(defun agent-ide-reload-current-file ()
  "Hot-reload the file associated with the current buffer.
Useful when editing a agent-ide source file."
  (interactive)
  (if (and buffer-file-name
           (string-suffix-p ".el" buffer-file-name))
      (agent-ide-load-file buffer-file-name)
    (user-error "Current buffer is not an .el file")))

;;;###autoload
(defun agent-ide-reload-all ()
  "Hot-reload all agent-ide project files in dependency order.
Rebuilds the entire project state without restarting Emacs."
  (interactive)
  (agent-ide-hotload--ensure-dir)
  (let ((count 0)
        (total (length agent-ide-hotload--files)))
    (dolist (file agent-ide-hotload--files)
      (let ((full (expand-file-name file agent-ide-hotload-project-dir)))
        (when (file-readable-p full)
          (message "Loading %s (%d/%d) ..." file (1+ count) total)
          (condition-case err
              (progn
                (load-file full)
                (cl-incf count))
            (error
             (message "Error loading %s: %s"
                      file (error-message-string err)))))))
    (message "Reloaded %d/%d files ✓" count total)))

;;;###autoload
(defun agent-ide-reload-last ()
  "Hot-reload the last file loaded by `agent-ide-load-file'."
  (interactive)
  (if (and agent-ide-hotload--last-file
           (file-readable-p agent-ide-hotload--last-file))
      (agent-ide-load-file agent-ide-hotload--last-file)
    (call-interactively #'agent-ide-load-file)))

(defun agent-ide-hotload--project-source-files ()
  "Return list of .el source files in the project directory."
  (let ((files (directory-files agent-ide-hotload-project-dir t "\\.el\\'")))
    (sort files #'string<)))

(defun agent-ide-hotload--setup-keybinding ()
  "Set up the C-c C-l keybinding in `agent-ide-session-mode'."
  (when (boundp 'agent-ide-session-mode-map)
    (define-key agent-ide-session-mode-map
                (kbd "C-c C-l") #'agent-ide-load-file)))

;;;###autoload
(defun agent-ide-hotload-enable-keybinding ()
  "Enable C-c C-l hot-reload binding in agent-ide session buffers."
  (interactive)
  (agent-ide-hotload--setup-keybinding)
  ;; Add to existing sessions
  (when (and (fboundp 'agent-ide--sessions)
               (fboundp 'agent-ide-session-buffer))
    (dolist (session (agent-ide--sessions))
      (let ((buffer (agent-ide-session-buffer session)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (local-set-key (kbd "C-c C-l") #'agent-ide-load-file))))))

(provide 'agent-ide-hotload)

;;; agent-ide-hotload.el ends here
