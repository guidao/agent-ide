;;; agent-ide-media.el --- Inline images and Canvas media -*- lexical-binding: t; -*-

;;; Commentary:
;; Preserve Markdown source while displaying images and canvas-video players.
;; Downloads and players belong to the buffer and are released with their link.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'browse-url)
(require 'image)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)
(require 'agent-ide-core)

(defcustom agent-ide-media-preview t
  "Whether to preview Markdown images and video links."
  :type 'boolean :group 'agent-ide)
(defcustom agent-ide-media-max-width 640
  "Maximum preview width in pixels."
  :type 'integer :group 'agent-ide)
(defcustom agent-ide-media-max-height 400
  "Maximum preview height in pixels."
  :type 'integer :group 'agent-ide)
(defvar-local agent-ide-media-inline-video t
  "Whether to embed Canvas players, rather than offer playback links.
Hidden inline response viewports use playback links because buffer overlays
cannot be copied into an after-string.")
(defvar-local agent-ide-media-updated-functions nil
  "Hook called after an asynchronous media preview changes.")
(defvar-local agent-ide-media--items nil)
(defvar canvas-video--instances)
(defvar canvas-video-audio-output)
(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(declare-function canvas-video--make-instance "canvas-video" (&rest slots))
(declare-function canvas-video--check-display "canvas-video" (width height))
(declare-function canvas-video--restart "canvas-video" (instance))
(declare-function canvas-video--send "canvas-video" (instance &rest args))
(declare-function canvas-video--dispose "canvas-video" (instance))
(declare-function canvas-video-inline-mode "canvas-video" (&optional arg))
(declare-function canvas-video-open "canvas-video" (file))
(declare-function canvas-video-open-url "canvas-video" (url))
(declare-function agent-ide-renderer-render-markdown-region "agent-ide-renderer" (start end))

(defun agent-ide-media--video-p (source)
  "Return non-nil for a video or animation SOURCE extension."
  (let ((case-fold-search t))
    (string-match-p
     "\\.\\(?:gif\\|apng\\|mp4\\|m4v\\|mov\\|webm\\|mkv\\|avi\\|mpeg\\|mpg\\|ogv\\)\\(?:[?#].*\\)?\\'"
     source)))

(defun agent-ide-media--source (target)
  "Resolve Markdown TARGET relative to the session project."
  (cond
   ((string-match-p "\\`\\(?:https?://\\|data:\\)" target) target)
   ((string-prefix-p "file://" target)
    (let ((url (url-generic-parse-url target)))
      (unless (member (url-host url) '(nil "" "localhost"))
        (error "Only local file URLs are supported"))
      (decode-coding-string (url-unhex-string (url-filename url)) 'utf-8)))
   ((string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" target)
    (error "Unsupported media URL scheme"))
   (t (expand-file-name
       (decode-coding-string (url-unhex-string target) 'utf-8)
       (or (and agent-ide--session
                (agent-ide-session-directory agent-ide--session))
           default-directory)))))

(defun agent-ide-media--intact-p (item)
  "Whether ITEM still covers its original source."
  (and (overlay-buffer item)
       (with-current-buffer (overlay-buffer item)
         (save-restriction
           (widen)
           (equal (overlay-get item 'source-text)
                  (buffer-substring-no-properties
                   (overlay-start item) (overlay-end item)))))))

(defun agent-ide-media--dispose (item)
  "Release ITEM, including downloads, temporary files and its player."
  (when (overlay-buffer item)
    (save-restriction
      (widen)
      (with-silent-modifications
        (remove-text-properties (overlay-start item) (overlay-end item)
                                '(agent-ide-media nil display nil button nil
						  category nil action nil keymap nil
						  follow-link nil help-echo nil)))))
  (when-let* ((instance (overlay-get item 'canvas-video-instance)))
    (canvas-video--dispose instance))
  (when-let* ((request (overlay-get item 'request))
              ((buffer-live-p request)))
    (when-let* ((process (get-buffer-process request))) (delete-process process))
    (kill-buffer request))
  (when-let* ((file (overlay-get item 'temporary-file)))
    (ignore-errors (delete-file file)))
  (delete-overlay item)
  (setq agent-ide-media--items (delq item agent-ide-media--items)))

(defun agent-ide-media--prune (&optional start end _old-length)
  "Release invalid previews intersecting changed START..END.
Without bounds, check all previews.  Include collapsed overlays and boundary
edits so deleting a whole image or replacing its delimiters still disposes it."
  (dolist (item (copy-sequence agent-ide-media--items))
    (when (or (not (overlay-buffer item))
              (null start)
              (and (<= (overlay-start item) end)
                   (>= (overlay-end item) start)))
      (unless (agent-ide-media--intact-p item) (agent-ide-media--dispose item)))))

(defun agent-ide-media--cleanup ()
  "Release all media owned by the current buffer."
  (mapc #'agent-ide-media--dispose (copy-sequence agent-ide-media--items)))

(defun agent-ide-media--dimensions ()
  "Return sensible preview dimensions for the current window."
  (cons (max 1 (min agent-ide-media-max-width
                    (if-let* ((window (get-buffer-window (current-buffer) t)))
                        (max 1 (- (window-body-width window t) 24))
                      agent-ide-media-max-width)))
        (max 1 agent-ide-media-max-height)))

(defun agent-ide-media--animated-p (file)
  "Detect GIF, animated PNG and animated WebP in local FILE."
  (or (agent-ide-media--video-p file)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file nil 0 1048576)
        (or (looking-at "GIF8[79]a")
            (and (looking-at "\211PNG") (search-forward "acTL" nil t))
            (and (looking-at "RIFF....WEBP") (search-forward "ANIM" nil t))))))

(defun agent-ide-media--canvas (item source)
  "Attach a canvas-video player to ITEM without editing Markdown SOURCE.
This small adapter uses canvas-video's instance lifecycle because its public
insertion API inserts a new placeholder into the document."
  (unless (require 'canvas-video nil t)
    (error "Install canvas-video and build its module to play this media"))
  (pcase-let* ((`(,width . ,height) (agent-ide-media--dimensions))
               (width (min width 4096)) (height (min height 4096)))
    (canvas-video--check-display width height)
    (let* ((begin (overlay-start item)) (end (overlay-end item))
           (canvas (list 'image :type 'canvas :id (make-symbol "agent-ide-media")
                         :data-width width :data-height height :scale 1.0))
           (instance (canvas-video--make-instance
                      :buffer (current-buffer) :overlay item :source source
                      :placeholder (overlay-get item 'source-text)
                      :width width :height height :audio canvas-video-audio-output
                      :canvas canvas)))
      (overlay-put item 'canvas-video-instance instance)
      (overlay-put item 'display canvas)
      (push instance canvas-video--instances)
      (canvas-video-inline-mode 1)
      (condition-case err
          (progn
            (canvas-video--restart instance)
            ;; Videos wait for Play; animations loop silently.
            (if (overlay-get item 'animation)
                (progn (canvas-video--send instance "set" "loop-file" "inf")
                       (canvas-video--send instance "set" "mute" "yes"))
              (canvas-video--send instance "set" "pause" "yes")))
        (error
         (canvas-video--dispose instance)
         (move-overlay item begin end (current-buffer))
         (overlay-put item 'display nil)
         (overlay-put item 'after-string nil)
         (overlay-put item 'canvas-video-instance nil)
         (signal (car err) (cdr err)))))))

(defun agent-ide-media--open (button)
  "Open BUTTON's media in Canvas or its original image file."
  (let* ((item (button-get button 'agent-ide-media))
         (source (and item (overlay-get item 'resolved-source))))
    (unless source (user-error "Media source is no longer available"))
    (if (overlay-get item 'video)
        (progn
          (unless (require 'canvas-video nil t) (user-error "canvas-video is unavailable"))
          (if (string-match-p "\\`https?://" source)
              (canvas-video-open-url source)
            (canvas-video-open source)))
      (if (string-match-p "\\`https?://" source) (browse-url source)
        (find-file source)))))

(defun agent-ide-media--status (item message)
  "Show readable fallback MESSAGE on ITEM."
  (when (agent-ide-media--intact-p item)
    (with-current-buffer (overlay-buffer item)
      (save-restriction
        (widen)
        (with-silent-modifications
          (put-text-property (overlay-start item) (overlay-end item) 'display
                             (format "[%s: %s]" message (overlay-get item 'label)))
          (put-text-property (overlay-start item) (overlay-end item) 'help-echo message)))
      (run-hooks 'agent-ide-media-updated-functions))))

(defun agent-ide-media--show (item source)
  "Display resolved local file or video URL SOURCE at ITEM."
  (when (agent-ide-media--intact-p item)
    (with-current-buffer (overlay-buffer item)
      (save-restriction
        (widen)
	(condition-case err
            (let* ((remote (string-match-p "\\`https?://" source))
                   (video (or (overlay-get item 'video)
                              (and (not remote) (agent-ide-media--animated-p source)))))
              (overlay-put item 'resolved-source source)
              (overlay-put item 'video video)
              (when (and video (not (string-match-p
                                     "\\.\\(?:mp4\\|m4v\\|mov\\|webm\\|mkv\\|avi\\|mpeg\\|mpg\\|ogv\\)\\(?:[?#].*\\)?\\'"
                                     (downcase source))))
		(overlay-put item 'animation t))
              (if video
                  (if agent-ide-media-inline-video
                      (agent-ide-media--canvas item source)
                    (agent-ide-media--status item "Play with canvas-video"))
		(pcase-let* ((`(,width . ,height) (agent-ide-media--dimensions))
                             (image (create-image source nil nil
                                                  :max-width width :max-height height
                                                  :ascent 'center)))
                  (unless image (error "Unsupported image format"))
                  (with-silent-modifications
                    (put-text-property (overlay-start item) (overlay-end item)
                                       'display image)))))
          (error (agent-ide-media--status item (error-message-string err))))
	(run-hooks 'agent-ide-media-updated-functions)))))

(defun agent-ide-media--downloaded (status item)
  "Handle a URL response STATUS for ITEM and release the response buffer."
  (let ((response (current-buffer)))
    (unwind-protect
        (when (agent-ide-media--intact-p item)
          (condition-case err
              (progn
                (when (or (plist-get status :error)
                          (and (numberp url-http-response-status)
                               (>= url-http-response-status 400)))
                  (error "Media download failed: %s"
                         (or (plist-get status :error) url-http-response-status)))
                (unless url-http-end-of-headers (error "Invalid media response"))
                (let ((file (make-temp-file "agent-ide-media-")))
                  (overlay-put item 'temporary-file file)
                  (let ((coding-system-for-write 'no-conversion))
                    ;; url-http places this marker on the final header newline.
                    (write-region (1+ url-http-end-of-headers) (point-max) file nil 'silent))
                  (agent-ide-media--show item file)))
            (error (agent-ide-media--status item (error-message-string err)))))
      (overlay-put item 'request nil)
      (when (buffer-live-p response) (kill-buffer response)))))

(defun agent-ide-media--load (item target)
  "Resolve and asynchronously load TARGET for ITEM."
  (condition-case err
      (let ((source (agent-ide-media--source target)))
        (overlay-put item 'resolved-source source)
        (when (file-remote-p source) (error "Remote file paths are not supported"))
        (cond
         ((string-prefix-p "data:" source)
          (unless (string-match "\\`data:image/[^;,]+;base64,\\(.+\\)\\'" source)
            (error "Unsupported media data URL"))
          (let ((data (base64-decode-string (match-string 1 source)))
                (file (make-temp-file "agent-ide-media-")))
            (overlay-put item 'temporary-file file)
            (let ((coding-system-for-write 'no-conversion))
              (write-region data nil file nil 'silent))
            (agent-ide-media--show item file)))
         ((and (string-match-p "\\`https?://" source)
               (not (overlay-get item 'video)))
          (agent-ide-media--status item "Loading image")
          (overlay-put item 'request
                       (url-retrieve source #'agent-ide-media--downloaded (list item) t t)))
         (t (agent-ide-media--show item source))))
    (error (agent-ide-media--status item (error-message-string err)))))

(defun agent-ide-media--escaped-p (position)
  "Whether POSITION follows an odd number of backslashes."
  (let ((n 0))
    (while (and (> position (point-min)) (eq (char-before position) ?\\))
      (setq n (1+ n) position (1- position)))
    (= (% n 2) 1)))

(defun agent-ide-media--destination (limit)
  "Read a link destination at point up to LIMIT.
Point starts after the opening parenthesis.  Return (END TARGET), allowing
balanced parentheses, angle-bracket paths and optional quoted titles."
  (let ((begin (point)) (depth 1) angle quote finish)
    (while (and (< (point) limit) (not finish))
      (let ((char (char-after)))
        (cond
         ((eq char ?\\) (forward-char (min 2 (- limit (point)))))
         ((and quote (eq char quote)) (setq quote nil) (forward-char))
         (quote (forward-char))
         ((eq char ?<) (setq angle t) (forward-char))
         ((and angle (eq char ?>)) (setq angle nil) (forward-char))
         (angle (forward-char))
         ((and (memq char '(?\" ?\')) (memq (char-before) '(?\s ?\t)))
          (setq quote char) (forward-char))
         ((eq char ?\() (setq depth (1+ depth)) (forward-char))
         ((eq char ?\))
          (setq depth (1- depth)) (forward-char)
          (when (= depth 0) (setq finish (point))))
         ((eq char ?\n) (goto-char limit))
         (t (forward-char)))))
    (when finish
      (let* ((body (string-trim (buffer-substring-no-properties begin (1- finish))))
             (target (if (string-match "\\`<\\([^>]+\\)>" body)
                         (match-string 1 body)
                       (replace-regexp-in-string "[ \t]+[\"'].*[\"']\\'" "" body))))
        (unless (string-empty-p target)
          (list finish (replace-regexp-in-string "\\\\\\([\\\\()]\\)" "\\1" target)))))))

(defun agent-ide-media-render (start end)
  "Preview Markdown images and video links in START..END without editing text.
Skip escaped markup and code, including incomplete streaming code fences."
  (when (and agent-ide-media-preview (display-images-p))
    (save-excursion
      (save-match-data
        (agent-ide-media--prune start end)
        (goto-char start)
        (while (re-search-forward "`+\\|~~~+\\|!?\\[\\([^]\n]*\\)\\](" end t)
          (let* ((begin (match-beginning 0)) (token (match-string-no-properties 0))
                 (label (match-string-no-properties 1)))
            (cond
             ((agent-ide-media--escaped-p begin))
             ((and (memq (aref token 0) '(?` ?~))
                   (>= (length token) 3)
                   (string-match-p "\\`[ \t]*\\'"
                                   (buffer-substring-no-properties (line-beginning-position) begin)))
              (forward-line 1)
              (unless (re-search-forward
                       (concat "^[ \t]*" (regexp-quote (substring token 0 1))
                               "\\{" (number-to-string (length token)) ",\\}[ \t]*$") end t)
                (goto-char end)))
             ((eq (aref token 0) ?`) (unless (search-forward token end t) (goto-char end)))
             ((eq (aref token 0) ?~))
             ((get-text-property begin 'agent-ide-latex))
             (t
              (when-let* ((destination (agent-ide-media--destination end)))
                (pcase-let ((`(,finish ,target) destination))
                  (when (or (string-prefix-p "!" token) (agent-ide-media--video-p target))
                    (unless (cl-find-if
                             (lambda (item) (and (= begin (overlay-start item))
                                                 (= finish (overlay-end item))))
                             agent-ide-media--items)
                      (let ((item (make-overlay begin finish nil t nil)))
                        (overlay-put item 'source-text (buffer-substring-no-properties begin finish))
                        (overlay-put item 'label (if (string-empty-p label) "media" label))
                        (overlay-put item 'video (agent-ide-media--video-p target))
                        (push item agent-ide-media--items)
                        (add-hook 'after-change-functions #'agent-ide-media--prune nil t)
                        (add-hook 'kill-buffer-hook #'agent-ide-media--cleanup nil t)
                        (add-hook 'change-major-mode-hook #'agent-ide-media--cleanup nil t)
                        (with-silent-modifications
                          (remove-text-properties begin finish '(display nil))
                          (make-text-button begin finish 'agent-ide-media item
                                            'action #'agent-ide-media--open 'follow-link t
                                            'help-echo target)
                          (put-text-property begin finish 'rear-nonsticky t))
                        (agent-ide-media--load item target))))))))))))))

;;;###autoload
(defun agent-ide-preview-media ()
  "Refresh media in previously rendered Markdown, preserving the input draft.
Use after enabling previews, installing canvas-video or changing image sizes."
  (interactive)
  (let ((inhibit-read-only t)
        (position (point-min)))
    (agent-ide-media--cleanup)
    (save-excursion
      (while (< position (point-max))
        (let ((end (next-single-property-change
                    position 'agent-ide-latex-region nil (point-max))))
          (when (get-text-property position 'agent-ide-latex-region)
            (agent-ide-renderer-render-markdown-region position end))
          (setq position end))))))

(provide 'agent-ide-media)
;;; agent-ide-media.el ends here
