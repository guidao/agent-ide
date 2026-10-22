;;; agent-ide-attachment.el --- Image attachments for ACP prompts -*- lexical-binding: t; -*-

;;; Code:
(require 'agent-ide-core)
(require 'agent-ide-renderer)
(require 'seq)

(defcustom agent-ide-attachment-max-bytes (* 20 1024 1024)
  "Maximum size of one image attachment in bytes."
  :type 'integer :group 'agent-ide)

(defun agent-ide-attachment-list (session)
  "Return SESSION's unsent attachments."
  (agent-ide--session-metadata-get session :attachments))

(defun agent-ide-attachment-check (session)
  "Require SESSION's backend to support image prompts."
  (unless (eq t (agent-ide--get-in (agent-ide-session-capabilities session) '(promptCapabilities image)))
    (user-error "This backend does not advertise image input support")))

(defun agent-ide-attachment--mime (data)
  "Recognize the supported image MIME type from binary DATA."
  (cond ((string-prefix-p (unibyte-string 137 80 78 71 13 10 26 10) data) "image/png")
        ((string-prefix-p (unibyte-string 255 216 255) data) "image/jpeg")
        ((or (string-prefix-p "GIF87a" data) (string-prefix-p "GIF89a" data)) "image/gif")
        ((and (>= (length data) 12) (string-prefix-p "RIFF" data)
              (equal (substring data 8 12) "WEBP")) "image/webp")
        (t (user-error "Use a PNG, JPEG, GIF or WebP image"))))

(defun agent-ide-attachment--remove (session item)
  "Remove pending ITEM from SESSION without modifying the text draft."
  (when (memq item (agent-ide-attachment-list session))
    (agent-ide--session-metadata-put
     session :attachments (delq item (agent-ide-attachment-list session)))
    (agent-ide-attachment-refresh session)))

(defun agent-ide-attachment--label (session item)
  "Return a clickable removal label for SESSION's ITEM."
  (let ((map (make-sparse-keymap))
        (remove (lambda (_event) (interactive "e")
                  (agent-ide-attachment--remove session item))))
    (define-key map [mouse-1] remove)
    (define-key map [mouse-2] remove)
    (propertize (format "[Image: %s ×]" (plist-get item :name))
                'face 'font-lock-string-face 'mouse-face 'highlight
                'keymap map 'follow-link t
                'help-echo "Click to remove image; Backspace at the start of the prompt removes the last image")))

(defun agent-ide-attachment-refresh (session)
  "Show SESSION's pending images with clickable removal controls."
  (let ((overlay (agent-ide-session-input-overlay session))
        (items (agent-ide-attachment-list session)))
    (when (overlayp overlay)
      (overlay-put overlay 'before-string
                   (when items
                     (concat (mapconcat (lambda (item) (agent-ide-attachment--label session item))
                                        items " ") "\n"))))))

(defun agent-ide-attachment-backspace (n &optional killflag)
  "Delete text backward N characters, or the last image at the prompt start.
Pass KILLFLAG through to ordinary backward deletion.  A selected text region
is deleted without changing image attachments."
  (interactive "p\nP")
  (let ((session (agent-ide--session-for-buffer)))
    (cond
     ((use-region-p) (delete-region (region-beginning) (region-end)))
     ((and (= n 1) session (agent-ide-renderer-input-active-p session)
           (= (point) (marker-position (agent-ide-session-input-start-marker session)))
           (agent-ide-attachment-list session))
      (agent-ide-attachment--remove session (car (last (agent-ide-attachment-list session)))))
     (t (delete-backward-char n killflag)))))

(defun agent-ide-attachment-add (session data name)
  "Attach binary image DATA with NAME to SESSION's next prompt."
  (agent-ide-attachment-check session)
  (when (> (string-bytes data) agent-ide-attachment-max-bytes)
    (user-error "Image exceeds the %d MiB attachment limit" (/ agent-ide-attachment-max-bytes 1048576)))
  (let ((item (list :name name :content `((type . "image")
                                          (mimeType . ,(agent-ide-attachment--mime data))
                                          (data . ,(base64-encode-string data t))))))
    (agent-ide--session-metadata-put session :attachments
                                     (append (agent-ide-attachment-list session) (list item)))
    (agent-ide-attachment-refresh session)
    item))

;;;###autoload
(defun agent-ide-attach-image (file)
  "Attach image FILE to the current prompt, without sending it yet."
  (interactive "fAttach image: ")
  (let ((session (or (agent-ide--session-for-buffer) (user-error "No Agent IDE session"))))
    (agent-ide-attachment-check session)
    (unless (file-regular-p file) (user-error "Not an image file: %s" file))
    (when (> (file-attribute-size (file-attributes file)) agent-ide-attachment-max-bytes)
      (user-error "Image exceeds the attachment limit"))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (agent-ide-attachment-add session (buffer-string) (file-name-nondirectory file)))))

;;;###autoload
(defun agent-ide-paste-image (&optional remove)
  "Attach a clipboard image to the current prompt.
With prefix REMOVE, remove an attachment instead.
Uses Emacs clipboard support, or pngpaste on macOS when available."
  (interactive "P")
  (if remove
      (agent-ide-remove-attachment)
    (let* ((session (or (agent-ide--session-for-buffer) (user-error "No Agent IDE session")))
           (_ (agent-ide-attachment-check session))
           (data (and (fboundp 'gui-get-selection)
                      (ignore-errors (gui-get-selection 'CLIPBOARD 'image/png)))))
      (unless (and (stringp data) (ignore-errors (agent-ide-attachment--mime data)))
	(setq data
              (when (and (eq system-type 'darwin) (executable-find "pngpaste"))
		(with-temp-buffer
                  (set-buffer-multibyte nil)
                  (let ((coding-system-for-read 'binary) (coding-system-for-write 'binary))
                    (when (zerop (call-process "pngpaste" nil '(t nil) nil "-"))
                      (buffer-string)))))))
      (unless (stringp data)
	(user-error "No clipboard image available; use agent-ide-attach-image to select a file"))
      (agent-ide-attachment-add session data "clipboard.png"))))

;;;###autoload
(defun agent-ide-remove-attachment ()
  "Remove an image from the current prompt."
  (interactive)
  (let* ((session (or (agent-ide--session-for-buffer) (user-error "No Agent IDE session")))
         (items (agent-ide-attachment-list session))
         (choices (cl-loop for item in items for index from 1
                           collect (cons (format "%d: %s" index (plist-get item :name)) item))))
    (unless items (user-error "No image attachments"))
    (let ((item (if (= (length items) 1) (car items)
                  (cdr (assoc (completing-read "Remove attachment: " choices nil t) choices)))))
      (agent-ide-attachment--remove session item))))

(defun agent-ide-attachment-restore (session items)
  "Restore failed submission ITEMS while preserving newer draft attachments."
  (agent-ide--session-metadata-put session :attachments
                                   (cl-delete-duplicates
                                    (append items (agent-ide-attachment-list session)) :test #'eq))
  (agent-ide-attachment-refresh session))

(provide 'agent-ide-attachment)
;;; agent-ide-attachment.el ends here
