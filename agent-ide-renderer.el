;;; agent-ide-renderer.el --- Transcript buffer rendering -*- lexical-binding: t; -*-

;;; Commentary:

;; Low-level transcript rendering helpers for agent-ide.

;;; Code:

(require 'browse-url)
(require 'button)
(require 'cl-lib)
(require 'color)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'thingatpt)
(require 'url-parse)
(require 'agent-ide-core)

(defface agent-ide-header-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for Agent IDE session headers."
  :group 'agent-ide)

(defface agent-ide-header-model-face
  '((t :inherit bold :foreground "#61AFEF"))
  "Face for model name in the header line."
  :group 'agent-ide)

(defface agent-ide-header-dir-face
  '((t :inherit shadow))
  "Face for project directory in the header line."
  :group 'agent-ide)

(defface agent-ide-header-context-face
  '((t :inherit font-lock-doc-face))
  "Face for context usage in the header line."
  :group 'agent-ide)

(defface agent-ide-header-separator-face
  '((t :inherit shadow :slant normal :weight light))
  "Face for dot separators in the header line."
  :group 'agent-ide)

(defface agent-ide-prompt-placeholder-face
  '((t :inherit shadow :slant italic))
  "Face used for Agent IDE prompt placeholder text."
  :group 'agent-ide)

(defface agent-ide-user-prompt-face
  '((t :inherit default :extend t))
  "Face used for editable and submitted Agent IDE prompts."
  :group 'agent-ide)

(defface agent-ide-prompt-face
  '((t :inherit (agent-ide-prompt-placeholder-face agent-ide-user-prompt-face)))
  "Face used for Agent IDE prompt prefixes."
  :group 'agent-ide)

(defface agent-ide-input-box-face
  '((t :inherit default :extend t))
  "Face used for Agent IDE input area."
  :group 'agent-ide)

(defface agent-ide-muted-face
  '((t :inherit shadow))
  "Face used for muted Agent IDE text."
  :group 'agent-ide)

(defun agent-ide-renderer--muted-foreground ()
  "Return a dimmed foreground color for muted text."
  (let* ((fg (agent-ide-renderer--default-foreground-color))
         (bg (agent-ide-renderer--default-background-color)))
    (agent-ide-renderer--blend-colors fg bg
      (if (agent-ide-renderer--theme-dark-p) 0.45 0.38))))

(defun agent-ide-renderer--blend-colors (color1 color2 amount)
  "Blend COLOR1 toward COLOR2 by AMOUNT (0=color1, 1=color2)."
  (pcase-let ((`(,r1 ,g1 ,b1) (color-values color1))
              (`(,r2 ,g2 ,b2) (color-values color2)))
    (format "#%02x%02x%02x"
            (round (/ (+ (* (- 1 amount) r1) (* amount r2)) 257.0))
            (round (/ (+ (* (- 1 amount) g1) (* amount g2)) 257.0))
            (round (/ (+ (* (- 1 amount) b1) (* amount b2)) 257.0)))))

(defface agent-ide-tool-face
  '((t :inherit font-lock-doc-face))
  "Face used for tool-call headings."
  :group 'agent-ide)

(defface agent-ide-approval-header-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face used for inline approval request headers."
  :group 'agent-ide)

(defface agent-ide-approval-label-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for inline approval field labels."
  :group 'agent-ide)

(defface agent-ide-diff-header-face
  '((t :inherit font-lock-keyword-face))
  "Face used for diff header lines."
  :group 'agent-ide)

(defface agent-ide-diff-hunk-face
  '((t :inherit font-lock-function-name-face))
  "Face used for diff hunk header lines."
  :group 'agent-ide)

(defface agent-ide-diff-added-face
  '((t :inherit diff-added))
  "Face used for added diff lines."
  :group 'agent-ide)

(defface agent-ide-diff-removed-face
  '((t :inherit diff-removed))
  "Face used for removed diff lines."
  :group 'agent-ide)

(defface agent-ide-diff-context-face
  '((t :inherit default))
  "Face used for context diff lines."
  :group 'agent-ide)

(defvar agent-ide-action-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    map)
  "Keymap for Agent IDE action buttons.")

(defvar agent-ide-fold-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "RET") #'agent-ide-renderer-toggle-fold-at-point)
    (define-key map (kbd "<return>") #'agent-ide-renderer-toggle-fold-at-point)
    map)
  "Keymap for foldable Agent IDE block headings.")

(defvar agent-ide-expand-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "RET") #'agent-ide-renderer-toggle-expand-at-point)
    (define-key map (kbd "<return>") #'agent-ide-renderer-toggle-expand-at-point)
    map)
  "Keymap for inline expand buttons.")

(defvar agent-ide-prompt-placeholder-text "Tell Agent what to do..."
  "Placeholder text shown in an empty idle Agent IDE prompt.")

(defvar agent-ide-running-placeholder-text "Working..."
  "Placeholder text shown while the agent is working.")

(defvar agent-ide-placeholder-ellipsis-animation-interval 0.5
  "Seconds between animated trailing ellipsis frames in busy prompt help.
When nil or zero, busy prompt help displays its text unchanged.")

(defvar agent-ide-status-placeholder-text-alist
  '(("interrupting" . "Interrupting...")
    ("creating-session" . "Creating session...")
    ("initializing" . "Initializing..."))
  "Alist mapping Agent IDE statuses to prompt placeholder text.")

(defconst agent-ide-renderer--input-background-mix-light 0.05
  "Amount to blend light-theme input background toward foreground.")

(defconst agent-ide-renderer--input-background-mix-dark 0.12
  "Amount to blend dark-theme input background toward foreground.")

(defmacro agent-ide-renderer--writable (&rest body)
  "Run BODY with read-only text temporarily writable."
  (declare (indent 0) (debug t))
  `(let ((inhibit-read-only t))
     ,@body))

(defun agent-ide-renderer--color-defined-p (color)
  "Return non-nil when COLOR is a defined color."
  (and (stringp color)
       (not (member color '("unspecified-fg" "unspecified-bg")))
       (ignore-errors
         (color-values color))))

(defun agent-ide-renderer--default-background-color ()
  "Return the current default background color, or a safe fallback."
  (let ((background (face-background 'default nil t)))
    (if (agent-ide-renderer--color-defined-p background)
        background
      "#000000")))

(defun agent-ide-renderer--default-foreground-color ()
  "Return the current default foreground color, or a safe fallback."
  (let ((foreground (face-foreground 'default nil t)))
    (if (agent-ide-renderer--color-defined-p foreground)
        foreground
      "#ffffff")))

(defun agent-ide-renderer--theme-dark-p ()
  "Return non-nil when the current default background is dark."
  (pcase-let ((`(,red ,green ,blue)
               (color-values
                (agent-ide-renderer--default-background-color))))
    (< (/ (+ red green blue) 3.0) (/ 65535.0 2))))

(defun agent-ide-renderer--blend-default-colors (amount)
  "Blend the default background toward the default foreground by AMOUNT."
  (let ((background (agent-ide-renderer--default-background-color))
        (foreground (agent-ide-renderer--default-foreground-color)))
    (pcase-let ((`(,fg-red ,fg-green ,fg-blue) (color-values foreground))
                (`(,bg-red ,bg-green ,bg-blue) (color-values background)))
      (format "#%02x%02x%02x"
              (round (/ (+ (* amount fg-red)
                           (* (- 1 amount) bg-red))
                        257.0))
              (round (/ (+ (* amount fg-green)
                           (* (- 1 amount) bg-green))
                        257.0))
              (round (/ (+ (* amount fg-blue)
                           (* (- 1 amount) bg-blue))
                        257.0))))))

(defun agent-ide-renderer--input-box-face-spec ()
  "Return a theme-aware face spec for `agent-ide-input-box-face'."
  `((t :inherit default
       :background ,(agent-ide-renderer--blend-default-colors
                     (if (agent-ide-renderer--theme-dark-p)
                         agent-ide-renderer--input-background-mix-dark
                       agent-ide-renderer--input-background-mix-light))
       :box nil
       :underline nil
       :overline nil
       :extend t)))

(defun agent-ide-renderer-refresh-faces ()
  "Refresh concrete faces used by Agent IDE."
  (face-spec-set 'agent-ide-input-box-face
                 (agent-ide-renderer--input-box-face-spec))
  (face-spec-set 'agent-ide-user-prompt-face
                 '((t :inherit agent-ide-input-box-face :extend t))))

(agent-ide-renderer-refresh-faces)

(defun agent-ide-renderer-refresh-all-theme (&rest _)
  "Refresh all Agent IDE faces and session inputs after theme change."
  (agent-ide-renderer-refresh-faces)
  (dolist (session agent-ide--sessions)
    (agent-ide-renderer-refresh-session-input session)))

(add-hook 'enable-theme-functions #'agent-ide-renderer-refresh-all-theme)

(defun agent-ide-renderer-refresh-session-input (session)
  "Refresh SESSION active input face and overlay."
  (when (agent-ide-renderer-input-active-p session)
    (with-current-buffer (agent-ide-session-buffer session)
      (agent-ide-renderer--writable
        (let ((display-start (agent-ide--session-metadata-get
                              session :input-display-start-marker))
              (input-start (agent-ide-session-input-start-marker session))
              (overlay (agent-ide-session-input-overlay session)))
          (agent-ide-renderer-refresh-faces)
          (when (and (markerp display-start)
                     (eq (marker-buffer display-start) (current-buffer)))
            (add-text-properties (marker-position display-start)
                                 (point-max)
                                 '(face agent-ide-user-prompt-face)))
          (when (and (overlayp overlay)
                     (markerp input-start)
                     (eq (marker-buffer input-start) (current-buffer)))
            (move-overlay overlay
                          (marker-position input-start)
                          (point-max)
                          (current-buffer))
            (overlay-put overlay 'face 'agent-ide-user-prompt-face)))))))

(defun agent-ide-renderer--freeze-region (start end)
  "Make region START to END read-only."
  (when (< start end)
    (remove-text-properties start end
                            '(read-only nil
                              rear-nonsticky nil
                              front-sticky nil))
    (add-text-properties
     start end
     '(read-only t
       rear-nonsticky (read-only)
       front-sticky (read-only)))))

(defun agent-ide-renderer--insert-read-only (text &rest properties)
  "Insert TEXT and make it read-only, applying PROPERTIES."
  (let ((start (point)))
    (insert text)
    (when properties
      (add-text-properties start (point) properties))
    (agent-ide-renderer--freeze-region start (point))))

(defun agent-ide-renderer--insert-input-padding (text)
  "Insert read-only input padding TEXT without sticky read-only boundaries."
  (let ((start (point)))
    (insert text)
    (add-text-properties start (point)
                         '(face agent-ide-user-prompt-face
                           read-only t))))

(defun agent-ide-renderer-style-input-region (session &optional start end)
  "Apply prompt face to SESSION editable input between START and END."
  (when (agent-ide-renderer-input-active-p session)
    (let* ((input-start (agent-ide-session-input-start-marker session))
           (input-end (agent-ide-session-input-end-marker session))
           (region-start (max (or start (marker-position input-start))
                              (marker-position input-start)))
           (region-end (min (or end (marker-position input-end))
                            (marker-position input-end))))
      (when (< region-start region-end)
        (add-text-properties region-start region-end
                             '(face agent-ide-user-prompt-face))))))

(defun agent-ide-renderer-make-input-editable (session &optional start end)
  "Remove read-only properties from SESSION editable input between START and END."
  (when (agent-ide-renderer-input-active-p session)
    (let* ((input-start (agent-ide-session-input-start-marker session))
           (input-end (agent-ide-session-input-end-marker session))
           (region-start (max (or start (marker-position input-start))
                              (marker-position input-start)))
           (region-end (min (or end (marker-position input-end))
                            (marker-position input-end))))
      (when (< region-start region-end)
        (remove-text-properties region-start region-end
                                '(read-only nil
                                  rear-nonsticky nil
                                  front-sticky nil))))))

(defun agent-ide-renderer--insert-position (session)
  "Return the transcript insertion position for SESSION."
  (let ((marker (or (agent-ide--session-metadata-get
                     session :active-input-boundary-marker)
                    (agent-ide--session-metadata-get
                     session :input-display-start-marker)
                    (agent-ide-session-input-prompt-start-marker session))))
    (if (and (markerp marker)
             (marker-buffer marker))
        (marker-position marker)
      (point-max))))

(defvar agent-ide-renderer--preserve-transcript-window-follow-anchor t
  "When non-nil, transcript window restoration may keep following the anchor.")

(defun agent-ide-renderer--input-end-position (session)
  "Return SESSION active editable input end position."
  (or (and (agent-ide-session-input-end-marker session)
           (marker-position (agent-ide-session-input-end-marker session)))
      (point-max)))

(defun agent-ide-renderer--input-point-marker (session)
  "Return a marker preserving point when it is inside SESSION's input."
  (let ((buffer (agent-ide-session-buffer session))
        (prompt-start (agent-ide-session-input-prompt-start-marker session))
        (input-end (agent-ide-renderer--input-end-position session)))
    (when (and (buffer-live-p buffer)
               (eq (current-buffer) buffer)
               (agent-ide-renderer-input-active-p session)
               (markerp prompt-start)
               (eq (marker-buffer prompt-start) buffer)
               input-end
               (or (and (>= (point) (marker-position prompt-start))
                        (<= (point) input-end))
                   (= (point) (point-max))))
      (copy-marker (min (point) input-end)))))

(defun agent-ide-renderer--restore-input-point-marker (marker)
  "Restore point to MARKER and clear it."
  (when (markerp marker)
    (when (marker-buffer marker)
      (goto-char marker))
    (set-marker marker nil)))

(defun agent-ide-renderer--sync-following-window-points (session)
  "Sync following windows to SESSION buffer point."
  (let ((buffer (agent-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((anchor (agent-ide-renderer--transcript-tail-point-position session)))
          (dolist (window (get-buffer-window-list buffer nil t))
            (when (and (window-live-p window)
                       (agent-ide-renderer--transcript-window-follows-anchor-p
                        window anchor))
              (set-window-point window (point)))))))))

(defun agent-ide-renderer--transcript-tail-point-position (session)
  "Return the point position to use when following the transcript tail."
  (if (agent-ide-renderer-input-active-p session)
      (agent-ide-renderer--input-end-position session)
    (point-max)))

(defun agent-ide-renderer--input-edit-point-position (session point-pos)
  "Return POINT-POS when it is an active input edit position.
When point is at the active input end, return nil so transcript tail following
can keep using the current tail position."
  (when (agent-ide-renderer-input-active-p session)
    (let ((input-start (agent-ide-session-input-start-marker session))
          (input-end (agent-ide-renderer--input-end-position session)))
      (when (and (markerp input-start)
                 (eq (marker-buffer input-start) (current-buffer))
                 (>= point-pos (marker-position input-start))
                 (< point-pos input-end))
        point-pos))))

(defun agent-ide-renderer--transcript-window-follows-anchor-p (window anchor-pos)
  "Return non-nil when WINDOW is already following transcript ANCHOR-POS."
  (let ((buffer-end (point-max))
        (window-point-pos (window-point window))
        (window-start-pos (window-start window))
        (window-end-pos (window-end window t)))
    (and (window-live-p window)
         (eq (window-buffer window) (current-buffer))
         (or (>= window-point-pos anchor-pos)
             (>= window-end-pos anchor-pos)
             (and (>= window-end-pos buffer-end)
                  (> window-point-pos window-start-pos))))))

(defun agent-ide-renderer--capture-transcript-window-positions (session anchor-pos)
  "Capture current-buffer window positions relative to transcript ANCHOR-POS."
  (mapcar
   (lambda (window)
     (list :window window
           :follow-anchor
           (and agent-ide-renderer--preserve-transcript-window-follow-anchor
                (agent-ide-renderer--transcript-window-follows-anchor-p
                 window anchor-pos))
           :start-marker (copy-marker (window-start window))
           :point-marker (copy-marker (window-point window))))
   (get-buffer-window-list (agent-ide-session-buffer session) nil t)))

(defun agent-ide-renderer--restore-transcript-window-positions (session states)
  "Restore transcript window positions recorded in STATES."
  (let ((tail-pos (agent-ide-renderer--transcript-tail-point-position session)))
    (dolist (state states)
      (let ((window (plist-get state :window))
            (follow-anchor (plist-get state :follow-anchor))
            (start-marker (plist-get state :start-marker))
            (point-marker (plist-get state :point-marker)))
        (unwind-protect
            (when (and (window-live-p window)
                       (eq (window-buffer window) (agent-ide-session-buffer session))
                       (markerp point-marker)
                       (marker-buffer point-marker))
              (let ((point-pos (marker-position point-marker)))
                (if follow-anchor
                    (set-window-point
                     window
                     (or (agent-ide-renderer--input-edit-point-position
                          session point-pos)
                         tail-pos))
                  (when (and (markerp start-marker)
                             (marker-buffer start-marker))
                    (set-window-start window (marker-position start-marker) t))
                  (let ((input-end (and (agent-ide-renderer-input-active-p session)
                                        (agent-ide-renderer--input-end-position session))))
                    (set-window-point
                     window
                     (if (and input-end (> point-pos input-end))
                         input-end
                       point-pos))))))
          (when (markerp start-marker)
            (set-marker start-marker nil))
          (when (markerp point-marker)
            (set-marker point-marker nil)))))))

(defmacro agent-ide-renderer--maybe-save-transcript-position (session anchor &rest body)
  "Run BODY while preserving non-following transcript windows around ANCHOR."
  (declare (indent 2) (debug (form body)))
  `(let ((window-states
          (with-current-buffer (agent-ide-session-buffer ,session)
            (agent-ide-renderer--capture-transcript-window-positions
             ,session ,anchor))))
     (unwind-protect
         (progn ,@body)
       (with-current-buffer (agent-ide-session-buffer ,session)
         (agent-ide-renderer--restore-transcript-window-positions
          ,session window-states)))))

(defun agent-ide-renderer--with-insertion-point (session thunk)
  "Call THUNK at SESSION transcript insertion point.
Preserve the active input edit point and transcript window positions."
  (with-current-buffer (agent-ide-session-buffer session)
    (let* ((pos (agent-ide-renderer--insert-position session))
           (restore-point (agent-ide-renderer--input-point-marker session)))
      (agent-ide-renderer--maybe-save-transcript-position session pos
        (agent-ide-renderer--writable
          (goto-char pos)
          (funcall thunk))
        (agent-ide-renderer--restore-input-point-marker restore-point)
        (agent-ide-renderer--sync-following-window-points session)))))

(defun agent-ide-renderer-follow-input (session)
  "Move following windows for SESSION to the active input prompt."
  (when-let* ((buffer (agent-ide-session-buffer session))
              ((buffer-live-p buffer))
              (tail (with-current-buffer buffer
                      (agent-ide-renderer--transcript-tail-point-position session))))
    (with-current-buffer buffer
      (goto-char tail))
    (dolist (window (get-buffer-window-list buffer nil t))
      (when (and (window-live-p window)
                 (agent-ide-renderer--transcript-window-follows-anchor-p
                  window tail))
        (set-window-point window tail)))))

(defun agent-ide-renderer-toggle-fold-at-point (&optional button)
  "Toggle the foldable block at point or BUTTON."
  (interactive)
  (let* ((pos (if button (button-start button) (point)))
         (overlay (get-text-property pos 'agent-ide-fold-overlay)))
    (unless (overlayp overlay)
      (user-error "No foldable block here"))
    (let ((hidden (overlay-get overlay 'invisible)))
      (overlay-put overlay 'invisible (not hidden))
      (when-let* ((button (get-text-property pos 'agent-ide-fold-button))
                  (label (button-get button 'agent-ide-label)))
        (let ((inhibit-read-only t))
          (button-put button 'display
                      (if (button-get button 'agent-ide-codex-style)
                          label
                        (format "%s %s"
                                (if hidden "▾" "▸")
                                label))))))))

(defun agent-ide-renderer-toggle-expand-at-point (&optional button)
  "Toggle inline expandable output at point or BUTTON."
  (interactive)
  (let* ((button (or button (button-at (point))))
         (start-marker (and button (button-get button 'agent-ide-expanded-start)))
         (end-marker (and button (button-get button 'agent-ide-expanded-end)))
         (expanded (and (markerp start-marker)
                        (markerp end-marker)
                        (marker-buffer start-marker)
                        (marker-buffer end-marker)))
         (output (and button (button-get button 'agent-ide-expanded-output)))
         (mode (and button (button-get button 'agent-ide-expanded-mode))))
    (unless button
      (user-error "No expand button here"))
    (let ((inhibit-read-only t))
      (if expanded
          (let ((start (marker-position start-marker))
                (end (marker-position end-marker)))
            (goto-char start)
            (delete-region start end)
            (set-marker start-marker nil)
            (set-marker end-marker nil)
            (button-put button 'display "[expand]"))
        (unless (and (stringp output)
                     (not (string-empty-p (string-trim output))))
          (user-error "No output to expand"))
        (save-excursion
          (goto-char (button-end button))
          (end-of-line)
          (let ((start (point)))
            (insert "\n")
            (agent-ide-renderer--insert-expanded-output output mode)
            (insert "\n")
            (agent-ide-renderer--freeze-region start (point))
            (button-put button 'agent-ide-expanded-start
                        (copy-marker start nil))
            (button-put button 'agent-ide-expanded-end
                        (copy-marker (point) nil))
            (button-put button 'display "[collapse]")))))))

(defun agent-ide-renderer--expanded-output-ranges-in-region (start end)
  "Return expanded output ranges referenced by buttons in START..END."
  (let ((pos start)
        ranges)
    (while (< pos end)
      (when-let* ((button (button-at pos))
                  (start-marker (button-get button 'agent-ide-expanded-start))
                  (end-marker (button-get button 'agent-ide-expanded-end))
                  ((markerp start-marker))
                  ((markerp end-marker))
                  ((marker-buffer start-marker))
                  ((marker-buffer end-marker)))
        (push (cons (marker-position start-marker)
                    (marker-position end-marker))
              ranges)
        (button-put button 'agent-ide-expanded-start nil)
        (button-put button 'agent-ide-expanded-end nil))
      (setq pos (or (next-single-property-change pos 'button nil end)
                    end))
      (when (< pos end)
        (setq pos (1+ pos))))
    ranges))

(defun agent-ide-renderer--delete-expanded-output-in-region (start end)
  "Delete expanded output referenced by inline buttons in START..END."
  (dolist (range (sort (agent-ide-renderer--expanded-output-ranges-in-region
                        start end)
                       (lambda (a b) (> (car a) (car b)))))
    (delete-region (car range) (cdr range))))

(defun agent-ide-renderer--diff-line-face (line)
  "Return the face to use for diff LINE."
  (cond
   ((string-prefix-p "@@" line) 'agent-ide-diff-hunk-face)
   ((or (string-prefix-p "diff --git" line)
        (string-prefix-p "--- " line)
        (string-prefix-p "+++ " line)
        (string-prefix-p "index " line))
    'agent-ide-diff-header-face)
   ((string-prefix-p "+" line) 'agent-ide-diff-added-face)
   ((string-prefix-p "-" line) 'agent-ide-diff-removed-face)
   (t 'agent-ide-diff-context-face)))

(defun agent-ide-renderer--insert-diff-output (output)
  "Insert OUTPUT with lightweight diff highlighting."
  (dolist (line (split-string (string-trim-right output) "\n"))
    (let ((start (point)))
      (insert line)
      (add-text-properties start (point)
                           (list 'face (agent-ide-renderer--diff-line-face
                                        line)))
      (insert "\n"))))

(defun agent-ide-renderer--insert-expanded-output (output &optional mode)
  "Insert expandable OUTPUT, using MODE-specific rendering when available."
  (let ((body-start (point)))
    (if (eq mode 'diff-mode)
        (agent-ide-renderer--insert-diff-output output)
      (insert (string-trim-right output))
      (agent-ide-renderer-render-markdown-region body-start (point)))))

(defun agent-ide-renderer--language-mode (language)
  "Return a major mode for fenced code LANGUAGE."
  (pcase (downcase (or language ""))
    ((or "elisp" "emacs-lisp" "emacs-lisp-mode") 'emacs-lisp-mode)
    ((or "lisp" "common-lisp") 'lisp-mode)
    ((or "sh" "shell" "bash" "zsh") 'sh-mode)
    ((or "python" "py") 'python-mode)
    ((or "js" "javascript") 'js-mode)
    ((or "ts" "typescript") (if (fboundp 'typescript-ts-mode)
                                'typescript-ts-mode
                              'js-mode))
    ((or "json") 'json-mode)
    ((or "css") 'css-mode)
    ((or "html") 'html-mode)
    ((or "yaml" "yml") 'yaml-mode)
    ((or "go") 'go-mode)
    ((or "rust" "rs") 'rust-mode)
    ((or "diff" "patch") 'diff-mode)
    (_ nil)))

(defun agent-ide-renderer--apply-fontified-text (start end text)
  "Copy fontified TEXT properties into START..END."
  (let ((pos start)
        next)
    (while (< pos end)
      (setq next (or (next-single-property-change
                      (- pos start) 'face text (length text))
                     (next-single-property-change
                      (- pos start) 'font-lock-face text (length text))))
      (let ((face (or (get-text-property (- pos start) 'face text)
                      (get-text-property (- pos start) 'font-lock-face text))))
        (when face
          (put-text-property pos
                             (min end (+ start next))
                             'face face)))
      (setq pos (min end (+ start next))))))

(defun agent-ide-renderer--fontify-elisp-fallback (start end)
  "Add lightweight Lisp call highlighting in START..END.
`emacs-lisp-mode' does not fontify ordinary function calls such as
`alist-get', so add a transcript-friendly fallback for fenced snippets."
  (save-excursion
    (goto-char start)
    (while (re-search-forward
            "(\\s-*\\([[:alpha:]_+*/<>=!?-][[:alnum:]_+*/<>=!?-]*\\)"
            end
            t)
      (unless (get-text-property (match-beginning 1) 'face)
        (put-text-property (match-beginning 1)
                           (match-end 1)
                           'face
                           'font-lock-function-name-face)))
    (goto-char start)
    (while (re-search-forward
            "'\\([[:alpha:]_+*/<>=!?-][[:alnum:]_+*/<>=!?-]*\\)"
            end
            t)
      (unless (get-text-property (match-beginning 1) 'face)
        (put-text-property (match-beginning 1)
                           (match-end 1)
                           'face
                           'font-lock-constant-face)))))

(defun agent-ide-renderer-fontify-code-fences (start end)
  "Render complete Markdown code fences in START..END.
Fence delimiter lines are hidden via `display', while code bodies keep syntax
highlighting and fixed-pitch text, matching the codex-ide transcript style."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "^[ \t]*```\\([[:alnum:]_+.-]*\\)[ \t]*\n" end t)
      (let* ((fence-start (match-beginning 0))
             (language (match-string-no-properties 1))
             (code-start (point))
             (mode (agent-ide-renderer--language-mode language)))
        (when (re-search-forward "^[ \t]*```[ \t]*$" end t)
          (let* ((closing-start (match-beginning 0))
                 (closing-end (min (if (eq (char-after (match-end 0)) ?\n)
                                       (1+ (match-end 0))
                                     (match-end 0))
                                   end))
                 (code-end closing-start))
            (add-text-properties
             fence-start code-start
             '(display "" agent-ide-markdown t))
            (when (and mode (fboundp mode) (< code-start code-end))
              (let ((code (buffer-substring-no-properties code-start code-end))
                    (target-buffer (current-buffer)))
                (with-temp-buffer
                  (insert code)
                  (funcall mode)
                  (font-lock-mode 1)
                  (font-lock-ensure (point-min) (point-max))
                  (let ((fontified (buffer-string)))
                    (with-current-buffer target-buffer
                      (agent-ide-renderer--apply-fontified-text
                       code-start
                       code-end
                       fontified)
                      (when (memq mode '(emacs-lisp-mode lisp-mode))
                        (agent-ide-renderer--fontify-elisp-fallback
                         code-start
                         code-end)))))))
            (add-text-properties code-start code-end
                                 '(agent-ide-markdown t
                                                       agent-ide-markdown-code-content t))
            (add-face-text-property code-start code-end 'fixed-pitch 'append)
            (add-text-properties
             closing-start closing-end
             '(display "" agent-ide-markdown t))
            (goto-char closing-end)))))))

(defun agent-ide-renderer--markdown-code-content-p (start end)
  "Return non-nil when START..END overlaps rendered fenced code content."
  (let ((pos start)
        found)
    (while (and (< pos end) (not found))
      (setq found (get-text-property pos 'agent-ide-markdown-code-content))
      (setq pos (or (next-single-property-change
                     pos 'agent-ide-markdown-code-content nil end)
                    end)))
    found))

(defun agent-ide-renderer--render-markdown-inline-pattern
    (start end pattern face &optional delimiter-groups)
  "Render inline Markdown PATTERN in START..END using FACE.
DELIMITER-GROUPS is a list of match groups to hide."
  (save-excursion
    (goto-char start)
    (while (re-search-forward pattern end t)
      (let ((content-start (match-beginning 2))
            (content-end (match-end 2)))
        (unless (or (not content-start)
                    (agent-ide-renderer--markdown-code-content-p
                     (match-beginning 0)
                     (match-end 0)))
          (add-face-text-property content-start content-end face 'append)
          (add-text-properties content-start content-end
                               '(agent-ide-markdown t))
          (dolist (group delimiter-groups)
            (when (match-beginning group)
              (add-text-properties (match-beginning group)
                                   (match-end group)
                                   '(display ""
                                             agent-ide-markdown t)))))))))

(defun agent-ide-renderer--render-markdown-inline-code (start end)
  "Render Markdown inline code in START..END."
  (agent-ide-renderer--render-markdown-inline-pattern
   start end "\\(`\\)\\([^`\n]+\\)\\(`\\)" 'font-lock-keyword-face '(1 3)))

(defun agent-ide-renderer--render-markdown-emphasis (start end)
  "Render simple Markdown emphasis in START..END."
  (agent-ide-renderer--render-markdown-inline-pattern
   start end "\\(\\*\\*\\)\\([^*\n][^*\n]*?\\)\\(\\*\\*\\)" 'bold '(1 3))
  (agent-ide-renderer--render-markdown-inline-pattern
   start end "\\(__\\)\\([^_\n][^_\n]*?\\)\\(__\\)" 'bold '(1 3))
  (agent-ide-renderer--render-markdown-inline-pattern
   start end "\\(^\\|[^[:word:]_]\\)\\(\\*\\)\\([^*\n][^*\n]*?\\)\\(\\*\\)"
   'italic '(2 4))
  (agent-ide-renderer--render-markdown-inline-pattern
   start end "\\(^\\|[^[:word:]_]\\)\\(_\\)\\([^_\n][^_\n]*?\\)\\(_\\)\\($\\|[^[:word:]_]\\)"
   'italic '(2 4)))

(defun agent-ide-renderer--render-markdown-headings (start end)
  "Render Markdown headings in START..END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "^\\(#\\{1,6\\}[ \t]+\\)\\(.+\\)$" end t)
      (unless (agent-ide-renderer--markdown-code-content-p
               (match-beginning 0)
               (match-end 0))
        (add-text-properties (match-beginning 1)
                             (match-end 1)
                             '(display "" agent-ide-markdown t))
        (add-face-text-property (match-beginning 2)
                                (match-end 2)
                                'agent-ide-header-face
                                'append)
        (add-text-properties (match-beginning 2)
                             (match-end 2)
                             '(agent-ide-markdown t))))))

(defun agent-ide-renderer-open-url (url)
  "Open URL like markdown-mode: full URLs in a browser, paths via find-file."
  (let* ((struct (url-generic-parse-url url))
         (full (url-fullness struct))
         (file (or (car (url-path-and-query struct)) url)))
    (if full
        (browse-url url)
      (when (and file (> (length file) 0))
        (when (string-match "\\`\\([^#?]+\\)" file)
          (setq file (match-string 1 file)))
        (find-file file)))))

(defun agent-ide-renderer-follow-url-button (button)
  "Follow URL stored on BUTTON."
  (when-let* ((url (button-get button 'agent-ide-url)))
    (agent-ide-renderer-open-url url)))

(defun agent-ide-renderer--make-url-button (start end url)
  "Make text in START..END a button that opens URL."
  (make-text-button start end
                    'follow-link t
                    'keymap agent-ide-action-button-map
                    'face 'link
                    'action #'agent-ide-renderer-follow-url-button
                    'agent-ide-url url
                    'help-echo url
                    'agent-ide-markdown t))

(defun agent-ide-renderer--render-markdown-links (start end)
  "Render simple Markdown links in START..END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\(\\[\\)\\([^]\n]+\\)\\(\\](\\)\\([^) \n]+\\)\\()\\)" end t)
      (unless (agent-ide-renderer--markdown-code-content-p
               (match-beginning 0)
               (match-end 0))
        (let ((url (match-string-no-properties 4)))
          (add-text-properties (match-beginning 1)
                               (match-end 1)
                               '(display "" agent-ide-markdown t))
          (add-text-properties (match-beginning 3)
                               (match-end 3)
                               '(display "" agent-ide-markdown t))
          (add-text-properties (match-beginning 4)
                               (match-end 5)
                               '(display "" agent-ide-markdown t))
          (agent-ide-renderer--make-url-button
           (match-beginning 2)
           (match-end 2)
           url))))))

(defun agent-ide-renderer--render-bare-urls (start end)
  "Render bare URLs in START..END as followable buttons."
  (let ((regexp (or (bound-and-true-p browse-url-button-regexp)
                    (bound-and-true-p thing-at-point-url-regexp))))
    (when regexp
      (save-excursion
        (goto-char start)
        (while (re-search-forward regexp end t)
          (let ((url-start (match-beginning 0))
                (url-end (match-end 0))
                (url (match-string-no-properties 0)))
            (unless (or (button-at url-start)
                        (get-text-property url-start 'display)
                        (agent-ide-renderer--markdown-code-content-p
                         url-start url-end))
              (agent-ide-renderer--make-url-button url-start url-end url))))))))

(defun agent-ide-renderer--render-command-lines (start end)
  "Apply `agent-ide-muted-face' to shell-command lines in START..END.
Lines matching the Codex tool-body pattern `  $ ...' get muted."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "^  \\$ " end t)
      (unless (agent-ide-renderer--markdown-code-content-p
               (line-beginning-position)
               (line-end-position))
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position)))
          (add-text-properties line-start line-end
                               '(face agent-ide-muted-face
                                 agent-ide-markdown t)))))))

(defun agent-ide-renderer-render-markdown-region (start end)
  "Apply lightweight Markdown rendering in START..END.
Table alignment is handled by `valign-mode'."
  (when (< start end)
    (let ((end-marker (copy-marker end t)))
      (agent-ide-renderer-fontify-code-fences start (marker-position end-marker))
      (agent-ide-renderer--render-markdown-headings start (marker-position end-marker))
      (agent-ide-renderer--render-command-lines start (marker-position end-marker))
      (agent-ide-renderer--render-markdown-links start (marker-position end-marker))
      (agent-ide-renderer--render-bare-urls start (marker-position end-marker))
      (agent-ide-renderer--render-markdown-inline-code start (marker-position end-marker))
      (agent-ide-renderer--render-markdown-emphasis start (marker-position end-marker))
      ;; Explicitly align tables via valign after markdown rendering.
      ;; jit-lock may not fire promptly during streaming, so we force
      ;; alignment here to ensure tables in agent output are lined up.
      (when (and (fboundp 'valign-region)
                 (display-graphic-p)
                 valign-mode)
        (condition-case nil
            (valign-region start (marker-position end-marker))
          (error nil)))
      (set-marker end-marker nil))))

(cl-defun agent-ide-renderer--insert-foldable-block
    (title body &key status collapsed face key)
  "Insert a foldable block with TITLE and BODY.
STATUS is optional display status.  COLLAPSED controls initial visibility.
FACE is used for the heading.  KEY is only stored as metadata."
  (let* ((label (string-trim
                 (concat (when status (format "[%s] " status))
                         (or title "Block"))))
         (heading-start (point))
         button
         body-start
         body-end
         overlay)
    (insert "\n")
    (setq label (format "* %s" label))
    (setq button
          (insert-text-button
           label
           'follow-link t
           'keymap agent-ide-fold-button-map
           'face (or face 'agent-ide-tool-face)
           'agent-ide-label label
           'agent-ide-codex-style t
           'agent-ide-key key
           'action #'agent-ide-renderer-toggle-fold-at-point))
    (insert "\n")
    (setq body-start (point))
    (when (and body (not (string-empty-p (string-trim body))))
      (let ((body-text-start (point)))
        (insert (string-trim-right body))
        (agent-ide-renderer-render-markdown-region body-text-start (point)))
      (insert "\n"))
    (setq body-end (point))
    ;; Keep the fold overlay bounded to this block.  Streaming code explicitly
    ;; moves the overlay as chunks arrive; normal tool blocks must not absorb
    ;; later permission/results inserted at the same boundary.
    (setq overlay (make-overlay body-start body-end nil nil nil))
    (overlay-put overlay 'invisible collapsed)
    (put-text-property heading-start body-start
                       'agent-ide-fold-overlay overlay)
    (put-text-property heading-start body-start
                       'agent-ide-fold-button button)
    (agent-ide-renderer--freeze-region heading-start (point))
    (list :button button :overlay overlay :body-start body-start :body-end body-end)))

(defun agent-ide-renderer--insert-expand-button (output &optional mode expand-now)
  "Insert an inline expand button for OUTPUT."
  (let ((button (insert-text-button
                 "[expand]"
                 'follow-link t
                 'keymap agent-ide-expand-button-map
                 'face 'link
                 'agent-ide-expanded-output output
                 'agent-ide-expanded-mode mode
                 'action #'agent-ide-renderer-toggle-expand-at-point)))
    (when expand-now
      (agent-ide-renderer-toggle-expand-at-point button))
    button))

(defun agent-ide-renderer-open-expanded-output-at-point (&optional button)
  "Open expandable output from BUTTON in a dedicated buffer."
  (interactive)
  (let* ((button (or button (button-at (point))))
         (output (and button (button-get button 'agent-ide-expanded-output)))
         (mode (and button (button-get button 'agent-ide-expanded-mode)))
         (buffer-name (or (and button
                               (button-get button 'agent-ide-expanded-buffer-name))
                          "*agent-ide-output*")))
    (unless button
      (user-error "No output button here"))
    (unless (and (stringp output)
                 (not (string-empty-p (string-trim output))))
      (user-error "No output to open"))
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (string-trim-right output))
          (insert "\n"))
        (when (and mode (fboundp mode))
          (funcall mode))
        (setq buffer-read-only t)
        (goto-char (point-min)))
      (pop-to-buffer buffer))))

(defun agent-ide-renderer--insert-open-output-button (output &optional mode buffer-name)
  "Insert an inline button that opens OUTPUT using MODE in BUFFER-NAME."
  (insert-text-button
   "[open diff]"
   'follow-link t
   'keymap agent-ide-action-button-map
   'face 'link
   'agent-ide-expanded-output output
   'agent-ide-expanded-mode mode
   'agent-ide-expanded-buffer-name buffer-name
   'action #'agent-ide-renderer-open-expanded-output-at-point))

(defun agent-ide-renderer--insert-codex-tool-body
    (body expanded-output &optional auto-expand)
  "Insert Codex-like BODY and attach expand button for EXPANDED-OUTPUT."
  (let ((body-start (point))
        auto-expand-button)
    (if (and expanded-output
             (stringp body)
             (string-match "\\[\\(?:expand\\|open diff\\)\\]" body))
        (let ((cursor 0))
          (while (string-match "\\[\\(expand\\|open diff\\)\\]" body cursor)
            (insert (substring body cursor (match-beginning 0)))
            (pcase (match-string 1 body)
              ("expand"
               (let ((button
                      (agent-ide-renderer--insert-expand-button
                       expanded-output
                       (and (string-match-p "\\[open diff\\]" body)
                            'diff-mode))))
                 (unless auto-expand-button
                   (setq auto-expand-button
                         (and auto-expand
                              (string-match-p "\\[open diff\\]" body)
                              button)))))
              ("open diff"
               (agent-ide-renderer--insert-open-output-button
                expanded-output 'diff-mode "*agent-ide-diff*")))
            (setq cursor (match-end 0)))
          (insert (substring body cursor)))
      (insert body))
    (agent-ide-renderer-render-markdown-region body-start (point))
    (when auto-expand-button
      (agent-ide-renderer-toggle-expand-at-point auto-expand-button))))

(defun agent-ide-renderer--insert-codex-tool-block
    (title body &optional key collapsed expanded-output auto-expand)
  "Insert a compact Codex-like tool block with TITLE and BODY.
KEY is stored as metadata on the title line.  COLLAPSED controls body
visibility."
  (let ((heading-start (point))
        button
        body-start
        body-end
        overlay)
    (insert "\n")
    (setq button
          (insert-text-button
           (format "* %s" (or title "Tool call"))
           'follow-link t
           'keymap agent-ide-fold-button-map
           'face 'agent-ide-tool-face
           'agent-ide-label (format "* %s" (or title "Tool call"))
           'agent-ide-codex-style t
           'agent-ide-key key
           'action #'agent-ide-renderer-toggle-fold-at-point))
    (insert "\n")
    (setq body-start (point))
    (when (and body (not (string-empty-p (string-trim body))))
      (agent-ide-renderer--insert-codex-tool-body
       (string-trim-right body)
       expanded-output
       auto-expand)
      (insert "\n"))
    (setq body-end (point))
    (setq overlay (make-overlay body-start body-end nil nil nil))
    (overlay-put overlay 'invisible collapsed)
    (put-text-property heading-start body-start
                       'agent-ide-fold-overlay overlay)
    (put-text-property heading-start body-start
                       'agent-ide-fold-button button)
    (agent-ide-renderer--freeze-region heading-start (point))))

(defun agent-ide-renderer--compact-number (value)
  "Format numeric VALUE compactly."
  (cond
   ((not (numberp value)) nil)
   ((>= value 1000000)
    (format "%.1fM" (/ value 1000000.0)))
   ((>= value 1000)
    (format "%.1fk" (/ value 1000.0)))
   (t
    (number-to-string value))))

(defun agent-ide-renderer--map-elt-any (map keys)
  "Return the first non-nil value in MAP for KEYS."
  (catch 'value
    (dolist (key keys)
      (let ((value (ignore-errors
                     (map-elt map key))))
        (when value
          (throw 'value value))))
    nil))

(defun agent-ide-renderer--models-list (models)
  "Return the concrete model list from MODELS."
  (or (and (listp models)
           (agent-ide-renderer--map-elt-any
            models '(availableModels models data)))
      models))

(defun agent-ide-renderer--model-id (model)
  "Return MODEL identifier, when known."
  (when (listp model)
    (agent-ide-renderer--map-elt-any model '(modelId id model))))

(defun agent-ide-renderer--model-display-name (model)
  "Return display label for MODEL, when known."
  (cond
   ((stringp model) model)
   ((listp model)
    (agent-ide-renderer--map-elt-any
     model '(name title label model id modelId)))))

(defun agent-ide-renderer--model-label (session)
  "Return a model label for SESSION, when known."
  (let* ((models-response (agent-ide-session-models session))
         (current-model-id (and (listp models-response)
                                (agent-ide-renderer--map-elt-any
                                 models-response
                                 '(currentModelId current_model_id))))
         (models (agent-ide-renderer--models-list models-response))
         (model (cond
                 (current-model-id
                  (cond
                   ((vectorp models)
                    (seq-find
                     (lambda (entry)
                       (equal (agent-ide-renderer--model-id entry)
                              current-model-id))
                     models))
                   ((listp models)
                    (seq-find
                     (lambda (entry)
                       (equal (agent-ide-renderer--model-id entry)
                              current-model-id))
                     models))))
                 ((vectorp models)
                  (seq-find
                   (lambda (entry)
                     (or (ignore-errors (map-elt entry 'selected))
                         (ignore-errors (map-elt entry 'default))
                         (ignore-errors (map-elt entry 'isDefault))
                         (equal (ignore-errors (map-elt entry 'modelId))
                                "default")
                         (equal (ignore-errors (map-elt entry 'id))
                                "default")))
                   models))
                 ((listp models)
                  (or (seq-find
                       (lambda (entry)
                         (and (listp entry)
                              (or (ignore-errors (map-elt entry 'selected))
                                  (ignore-errors (map-elt entry 'default))
                                  (ignore-errors (map-elt entry 'isDefault))
                                  (equal (ignore-errors (map-elt entry 'modelId))
                                         "default")
                                  (equal (ignore-errors (map-elt entry 'id))
                                         "default"))))
                      models)
                      (car models))))))
    (cond
     ((and current-model-id model)
      (or (agent-ide-renderer--model-display-name model)
          current-model-id))
     (current-model-id current-model-id)
     (model
      (agent-ide-renderer--model-display-name model))
     ((and (vectorp models) (> (length models) 0))
      (let ((first (aref models 0)))
        (agent-ide-renderer--model-display-name first))))))

(defun agent-ide-renderer--model-completion-alist (models)
  "Return a completion alist from MODELS.
Each entry is a cons of (display . model-id)."
  (let ((entries nil)
        (seen (make-hash-table :test 'equal)))
    (cl-flet ((add (id display)
                (when (and id (not (gethash id seen)))
                  (puthash id t seen)
                  (push (cons (or display id) id) entries))))
      (cond
       ((vectorp models)
        (seq-do (lambda (entry)
                  (add (agent-ide-renderer--model-id entry)
                       (agent-ide-renderer--model-display-name entry)))
                models))
       ((listp models)
        (dolist (entry models)
          (add (agent-ide-renderer--model-id entry)
               (agent-ide-renderer--model-display-name entry))))))
    (nreverse entries)))

(defun agent-ide-renderer--usage-total (usage)
  "Return total token usage block from USAGE."
  (or (map-elt usage 'total)
      (map-elt usage 'totalUsage)
      usage))

(defun agent-ide-renderer--usage-last (usage)
  "Return last turn token usage block from USAGE."
  (or (map-elt usage 'last)
      (map-elt usage 'lastUsage)
      (map-elt usage 'turn)
      (map-elt usage 'turnUsage)
      usage))

(defun agent-ide-renderer--token-total (usage-block)
  "Return total token count from USAGE-BLOCK."
  (agent-ide-renderer--map-elt-any
   usage-block '(totalTokens total tokens inputOutputTokens)))

(defun agent-ide-renderer--input-tokens (usage-block)
  "Return input token count from USAGE-BLOCK."
  (agent-ide-renderer--map-elt-any
   usage-block '(inputTokens input promptTokens prompt)))

(defun agent-ide-renderer--output-tokens (usage-block)
  "Return output token count from USAGE-BLOCK."
  (agent-ide-renderer--map-elt-any
   usage-block '(outputTokens output completionTokens completion)))

(defun agent-ide-renderer--context-used (usage)
  "Return tokens currently in context from USAGE.
Prefers ACP `usage_update' fields (`used'), then legacy total-token blocks."
  (or (agent-ide-renderer--map-elt-any usage '(used))
      (let ((total (agent-ide-renderer--usage-total usage)))
        (and total (agent-ide-renderer--token-total total)))))

(defun agent-ide-renderer--context-window (usage)
  "Return context window size from USAGE.
Prefers ACP `usage_update' `size', then legacy context-window fields."
  (agent-ide-renderer--map-elt-any
   usage '(size modelContextWindow contextWindow maxContextTokens maxInputTokens)))

(defun agent-ide-renderer--context-label (usage)
  "Return context usage label for USAGE."
  (when usage
    (let ((used (agent-ide-renderer--context-used usage))
          (window (agent-ide-renderer--context-window usage)))
      (when (and used window)
        (format "Context: %s/%s"
                (agent-ide-renderer--compact-number used)
                (agent-ide-renderer--compact-number window))))))

(defun agent-ide-renderer--last-usage-label (usage)
  "Return last turn token usage label for USAGE."
  (when usage
    (let* ((last (agent-ide-renderer--usage-last usage))
           (input (and last (agent-ide-renderer--input-tokens last)))
           (output (and last (agent-ide-renderer--output-tokens last)))
           (total (and last (agent-ide-renderer--token-total last))))
      (cond
       ((or input output)
        (format "Last: in %s out %s"
                (or (agent-ide-renderer--compact-number input) "?")
                (or (agent-ide-renderer--compact-number output) "?")))
       (total
        (format "Last: %s"
                (agent-ide-renderer--compact-number total)))))))

(defun agent-ide-renderer--header-summary (session)
  "Return header summary text for SESSION."
  (let* ((model-label (agent-ide-renderer--model-label session))
         (usage (agent-ide-session-usage session))
         (context-str
          (when usage
            (let ((used (agent-ide-renderer--context-used usage))
                  (window (agent-ide-renderer--context-window usage)))
              (when (and used window)
                (format "%s/%s tokens"
                        (agent-ide-renderer--compact-number used)
                        (agent-ide-renderer--compact-number window))))))
         (dir (abbreviate-file-name
               (or (agent-ide-session-directory session)
                   default-directory)))
         (sep (propertize " · " 'face 'agent-ide-header-separator-face))
         (parts nil))
    (when model-label
      (push (propertize model-label 'face 'agent-ide-header-model-face) parts))
    (when dir
      (push (propertize dir 'face 'agent-ide-header-dir-face) parts))
    (when context-str
      (push (propertize context-str 'face 'agent-ide-header-context-face) parts))
    (if parts
        (string-join (nreverse parts) sep)
      (propertize "Agent" 'face 'agent-ide-header-model-face))))

(defun agent-ide-renderer-update-header (session)
  "Update SESSION header line."
  (when-let* ((buffer (agent-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq header-line-format
              (concat " " (agent-ide-renderer--header-summary session)))
        (force-mode-line-update t)
        (agent-ide-renderer-refresh-placeholder session))
      (when (fboundp 'agent-ide-sidebar-on-sessions-changed)
        (agent-ide-sidebar-on-sessions-changed)))))

(defun agent-ide-renderer-initialize-buffer (session)
  "Initialize SESSION transcript buffer."
  (with-current-buffer (agent-ide-session-buffer session)
    (agent-ide-renderer--writable
      (erase-buffer)
      (let ((start (point)))
        (insert (format "Agent IDE\n%s\n\n"
                        (abbreviate-file-name
                         (agent-ide-session-directory session))))
        (add-text-properties start (point) '(face agent-ide-header-face))
        (agent-ide-renderer--freeze-region start (point))))
    (agent-ide-renderer-update-header session)))

(defun agent-ide-renderer-append-status (session text)
  "Append status TEXT to SESSION."
  (agent-ide-renderer--with-insertion-point
   session
   (lambda ()
     (agent-ide-renderer--insert-read-only
      (format "%s\n" text)
      'face 'agent-ide-muted-face))))

(defun agent-ide-renderer-append-error (session text)
  "Append error TEXT to SESSION."
  (agent-ide-renderer--with-insertion-point
   session
   (lambda ()
     (agent-ide-renderer--insert-read-only
      (format "\nError: %s\n" text)
      'face 'error))))

(defun agent-ide-renderer-create-prompt (session &optional separate-output-p)
  "Create a fresh editable prompt for SESSION."
  (with-current-buffer (agent-ide-session-buffer session)
    (agent-ide-renderer--writable
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (agent-ide--session-metadata-put
       session :active-input-boundary-marker nil)
      (let ((active-boundary nil)
            (display-start nil)
            (prompt-start nil))
        (when separate-output-p
          ;; Keep this fixed while creating the prompt.  After the prompt exists,
          ;; switch it to insertion-type t so tool/message chunks append in
          ;; arrival order above the active input box.
          (setq active-boundary (copy-marker (point) nil))
          (insert "\n"))
        (setq prompt-start (copy-marker (point) nil))
        (agent-ide-renderer--insert-read-only "> " 'face 'agent-ide-prompt-face)
        (let ((input-start (copy-marker (point) nil))
              (input-end-pos nil)
              (input-end nil))
          (setq input-end-pos (marker-position input-start))
          (agent-ide-renderer--insert-input-padding "\n\n")
          (setq input-end (copy-marker input-end-pos t))
          (setq display-start (copy-marker (marker-position prompt-start) nil))
          (goto-char prompt-start)
          (agent-ide-renderer--insert-input-padding "\n")
          (set-marker prompt-start (point))
          (goto-char (point-max))
          (when-let* ((old-overlay (agent-ide-session-input-overlay session)))
            (delete-overlay old-overlay))
          (setf (agent-ide-session-input-overlay session) nil)
          (agent-ide--session-metadata-put
           ;; Keep this marker before output inserted at the prompt boundary, so
           ;; streaming text never becomes part of the input padding block.
           session :input-display-start-marker (copy-marker display-start nil))
          (when active-boundary
            (set-marker-insertion-type active-boundary t)
            (agent-ide--session-metadata-put
             session :active-input-boundary-marker active-boundary))
          (setf (agent-ide-session-input-prompt-start-marker session)
                (copy-marker (marker-position prompt-start) t))
          (setf (agent-ide-session-input-start-marker session)
                ;; Keep the marker before text inserted at the prompt.
                (copy-marker (marker-position input-start) nil))
          (setf (agent-ide-session-input-end-marker session)
                input-end)
          (agent-ide-renderer-make-input-editable session)
          (agent-ide-renderer-style-input-region session)
          (let ((overlay (make-overlay (marker-position input-start)
                                       (point-max)
                                       (current-buffer)
                                       nil
                                       t)))
            (overlay-put overlay 'face 'agent-ide-user-prompt-face)
            (overlay-put overlay 'field 'agent-ide-active-input)
            (overlay-put overlay 'read-only nil)
            (setf (agent-ide-session-input-overlay session) overlay))
          (agent-ide-renderer-refresh-placeholder session)
          (goto-char input-end)
          (agent-ide-renderer-follow-input session))))))

(defun agent-ide-renderer--placeholder-text (session)
  "Return placeholder text for SESSION."
  (let ((status (downcase (or (agent-ide-session-status session) ""))))
    (or (alist-get status agent-ide-status-placeholder-text-alist nil nil #'string=)
        (if (member status '("running" "working"))
            agent-ide-running-placeholder-text
          agent-ide-prompt-placeholder-text))))

(defconst agent-ide-renderer--placeholder-ellipsis-frames
  '("." ".." "..." "")
  "Display frames for animated busy prompt placeholders.")

(defun agent-ide-renderer--placeholder-animation-enabled-p ()
  "Return non-nil when busy prompt placeholder animation is enabled."
  (and (numberp agent-ide-placeholder-ellipsis-animation-interval)
       (> agent-ide-placeholder-ellipsis-animation-interval 0)))

(defun agent-ide-renderer--placeholder-busy-p (session)
  "Return non-nil when SESSION placeholder represents active work."
  (member (downcase (or (agent-ide-session-status session) ""))
          '("running" "working")))

(defun agent-ide-renderer--placeholder-animated-text (session text)
  "Return TEXT with its trailing ellipsis frame applied for SESSION."
  (if-let* ((frame (and (agent-ide-renderer--placeholder-busy-p session)
                        (string-suffix-p "..." text)
                        (agent-ide--session-metadata-get
                         session :input-placeholder-ellipsis-frame))))
      (concat (substring text 0 -3)
              (nth (mod frame
                        (length agent-ide-renderer--placeholder-ellipsis-frames))
                   agent-ide-renderer--placeholder-ellipsis-frames))
    text))

(defun agent-ide-renderer--placeholder-display-string (session)
  "Return propertized placeholder display string for SESSION."
  (let ((text (propertize
               (agent-ide-renderer--placeholder-animated-text
                session
                (agent-ide-renderer--placeholder-text session))
               'face
               'agent-ide-prompt-placeholder-face)))
    (unless (string-empty-p text)
      (add-text-properties 0 1 '(cursor t) text))
    text))

(defun agent-ide-renderer--input-empty-p (session)
  "Return non-nil if SESSION prompt is empty."
  (string-empty-p (string-trim (agent-ide-renderer-current-input session))))

(defun agent-ide-renderer--placeholder-visible-p (session)
  "Return non-nil when SESSION placeholder should be visible."
  (and (agent-ide-renderer-input-active-p session)
       (agent-ide-renderer--input-empty-p session)))

(defun agent-ide-renderer--placeholder-should-animate-p (session)
  "Return non-nil when SESSION visible placeholder should animate."
  (let ((buffer (agent-ide-session-buffer session)))
    (and (buffer-live-p buffer)
         (get-buffer-window-list buffer nil t)
         (agent-ide-renderer--placeholder-animation-enabled-p)
         (agent-ide-renderer--placeholder-visible-p session)
         (agent-ide-renderer--placeholder-busy-p session)
         (string-suffix-p "..."
                          (agent-ide-renderer--placeholder-text session)))))

(defun agent-ide-renderer--stop-placeholder-animation (session)
  "Stop SESSION placeholder animation timer, if any."
  (when-let* ((timer (agent-ide--session-metadata-get
                      session :input-placeholder-animation-timer)))
    (when (timerp timer)
      (cancel-timer timer)))
  (agent-ide--session-metadata-put
   session :input-placeholder-animation-timer nil)
  (agent-ide--session-metadata-put
   session :input-placeholder-ellipsis-frame nil))

(defun agent-ide-renderer--advance-placeholder-animation (session)
  "Advance SESSION busy placeholder by one frame."
  (if (agent-ide-renderer--placeholder-should-animate-p session)
      (progn
        (agent-ide--session-metadata-put
         session
         :input-placeholder-ellipsis-frame
         (if-let* ((frame (agent-ide--session-metadata-get
                           session :input-placeholder-ellipsis-frame)))
             (mod (1+ frame)
                  (length agent-ide-renderer--placeholder-ellipsis-frames))
           0))
        (agent-ide-renderer-refresh-placeholder session))
    (agent-ide-renderer--stop-placeholder-animation session)))

(defun agent-ide-renderer--ensure-placeholder-animation (session)
  "Ensure SESSION has a live placeholder animation timer when needed."
  (if (agent-ide-renderer--placeholder-should-animate-p session)
      (unless (timerp (agent-ide--session-metadata-get
                       session :input-placeholder-animation-timer))
        (agent-ide--session-metadata-put
         session
         :input-placeholder-animation-timer
         (run-at-time
          agent-ide-placeholder-ellipsis-animation-interval
          agent-ide-placeholder-ellipsis-animation-interval
          #'agent-ide-renderer--advance-placeholder-animation
          session)))
    (agent-ide-renderer--stop-placeholder-animation session)))

(defun agent-ide-renderer--ensure-placeholder-overlay (session)
  "Ensure SESSION has a placeholder overlay at input start."
  (let* ((buffer (agent-ide-session-buffer session))
         (marker (agent-ide-session-input-start-marker session))
         (overlay (agent-ide--session-metadata-get session :input-placeholder-overlay)))
    (unless (and (overlayp overlay) (overlay-buffer overlay))
      (setq overlay nil))
    (when (and (buffer-live-p buffer)
               (markerp marker)
               (eq (marker-buffer marker) buffer)
               (not overlay))
      (setq overlay (make-overlay (marker-position marker)
                                  (marker-position marker)
                                  buffer nil t))
      (overlay-put overlay 'agent-ide-placeholder t)
      (agent-ide--session-metadata-put session :input-placeholder-overlay overlay))
    (when (and overlay
               (buffer-live-p buffer)
               (markerp marker)
               (eq (marker-buffer marker) buffer))
      (move-overlay overlay
                    (marker-position marker)
                    (marker-position marker)
                    buffer))
    overlay))

(defun agent-ide-renderer-refresh-placeholder (&optional session)
  "Refresh prompt placeholder for SESSION."
  (setq session (or session (agent-ide--session-for-buffer)))
  (when (and session (agent-ide-renderer-input-active-p session))
    (agent-ide-renderer--ensure-placeholder-animation session)
    (let ((overlay (agent-ide-renderer--ensure-placeholder-overlay session)))
      (when (overlayp overlay)
        (overlay-put
         overlay
         'after-string
         (and (agent-ide-renderer--placeholder-visible-p session)
              (agent-ide-renderer--placeholder-display-string session)))))))

(defun agent-ide-renderer-input-active-p (session)
  "Return non-nil when SESSION has an active prompt."
  (let ((buffer (agent-ide-session-buffer session))
        (overlay (agent-ide-session-input-overlay session))
        (start (agent-ide-session-input-start-marker session))
        (end (agent-ide-session-input-end-marker session)))
    (and (buffer-live-p buffer)
         (overlayp overlay)
         (eq (overlay-buffer overlay) buffer)
         (markerp start)
         (markerp end)
         (eq (marker-buffer start) buffer)
         (eq (marker-buffer end) buffer))))

(defun agent-ide-renderer-current-input (session)
  "Return SESSION current prompt text."
  (with-current-buffer (agent-ide-session-buffer session)
    (let ((start (agent-ide-session-input-start-marker session))
          (end (agent-ide-session-input-end-marker session)))
      (if (and (markerp start) (marker-buffer start)
               (markerp end) (marker-buffer end))
          (string-trim-right
           (buffer-substring-no-properties
            (marker-position start)
            (marker-position end)))
        ""))))

(defun agent-ide-renderer-replace-current-input (session text)
  "Replace SESSION editable prompt contents with TEXT."
  (with-current-buffer (agent-ide-session-buffer session)
    (when (agent-ide-renderer-input-active-p session)
      (let* ((input-start (agent-ide-session-input-start-marker session))
             (input-end (agent-ide-session-input-end-marker session))
             (start-pos (marker-position input-start))
             (end-pos (marker-position input-end)))
        (agent-ide-renderer--writable
          (agent-ide-renderer-make-input-editable session)
          (delete-region start-pos end-pos)
          (insert text)
          (set-marker input-end (point))
          (agent-ide-renderer-style-input-region session)
          (agent-ide-renderer-refresh-placeholder session))))))

(defun agent-ide-renderer-freeze-current-input (session)
  "Freeze SESSION current input region."
  (with-current-buffer (agent-ide-session-buffer session)
    (agent-ide-renderer--stop-placeholder-animation session)
    (when-let* ((placeholder (agent-ide--session-metadata-get
                              session :input-placeholder-overlay)))
      (delete-overlay placeholder)
      (agent-ide--session-metadata-put session :input-placeholder-overlay nil))
    (when-let* ((overlay (agent-ide-session-input-overlay session)))
      (delete-overlay overlay))
    (setf (agent-ide-session-input-overlay session) nil)
    (agent-ide--session-metadata-put session :active-input-boundary-marker nil)
    (agent-ide--session-metadata-put session :input-display-start-marker nil)
    (when-let* ((prompt-start (agent-ide-session-input-prompt-start-marker session)))
      (agent-ide-renderer--writable
        (agent-ide-renderer--freeze-region (marker-position prompt-start) (point-max))
        (goto-char (point-max))
        (insert "\n")))
    (setf (agent-ide-session-input-prompt-start-marker session) nil)
    (setf (agent-ide-session-input-start-marker session) nil)
    (setf (agent-ide-session-input-end-marker session) nil)))

(defun agent-ide-renderer--stream-title (kind)
  "Return display title for stream KIND."
  (pcase kind
    ('thought "Thinking")
    ('message "Assistant")
    (_ "Agent")))

(defun agent-ide-renderer-append-stream-chunk (session kind text)
  "Append streaming TEXT of KIND to SESSION."
  (unless (string-empty-p (or text ""))
    (agent-ide-renderer--with-insertion-point
     session
     (lambda ()
       (unless (and (eq (agent-ide-session-current-stream-kind session) kind)
                    (markerp (agent-ide-session-current-stream-marker session))
                    (marker-buffer (agent-ide-session-current-stream-marker session)))
         (let* ((title (agent-ide-renderer--stream-title kind))
                (fold (and (eq kind 'thought)
                           (agent-ide-renderer--insert-foldable-block
                            title "" :collapsed t
                            :face 'agent-ide-muted-face
                            :key "thinking")))
                (marker (copy-marker
                         (if fold
                             (plist-get fold :body-start)
                           (progn
                             (agent-ide-renderer--insert-read-only
                              (format "\n%s\n\n" title)
                              'face 'agent-ide-header-face)
                             (point)))
                         t)))
           (when fold
             (agent-ide--session-metadata-put
              session :current-stream-overlay (plist-get fold :overlay)))
           (agent-ide--session-metadata-put
            session :current-stream-start-marker (copy-marker marker nil))
           (setf (agent-ide-session-current-stream-kind session) kind)
           (setf (agent-ide-session-current-stream-marker session) marker)))
       (goto-char (marker-position (agent-ide-session-current-stream-marker session)))
       (let ((start (point)))
         (insert text)
         (when (eq kind 'thought)
           (add-text-properties start (point) '(face agent-ide-muted-face)))
         (unless (eq kind 'thought)
           (agent-ide-renderer-render-markdown-region
            (marker-position
             (or (agent-ide--session-metadata-get
                  session :current-stream-start-marker)
                 (agent-ide-session-current-stream-marker session)))
            (point)))
         (when-let* ((overlay (and (eq kind 'thought)
                                   (agent-ide--session-metadata-get
                                    session :current-stream-overlay))))
           (move-overlay overlay
                         (overlay-start overlay)
                         (point)))
         (agent-ide-renderer--freeze-region start (point))
         (set-marker (agent-ide-session-current-stream-marker session) (point)))))))

(defun agent-ide-renderer-reset-stream (session)
  "Reset SESSION stream grouping."
  (setf (agent-ide-session-current-stream-kind session) nil)
  (setf (agent-ide-session-current-stream-marker session) nil)
  (agent-ide--session-metadata-put session :current-stream-overlay nil)
  (agent-ide--session-metadata-put session :current-stream-start-marker nil))

(defun agent-ide-renderer-finish-stream (session)
  "Terminate SESSION current stream cleanly before the prompt."
  (when (eq (agent-ide-session-current-stream-kind session) 'message)
    (when-let* ((start-marker (agent-ide--session-metadata-get
                               session :current-stream-start-marker))
                (end-marker (agent-ide-session-current-stream-marker session))
                ((marker-buffer start-marker))
                ((marker-buffer end-marker)))
      (with-current-buffer (agent-ide-session-buffer session)
        (agent-ide-renderer--writable
          (agent-ide-renderer-render-markdown-region
           (marker-position start-marker)
           (marker-position end-marker))))))
  (when-let* ((marker (or (agent-ide--session-metadata-get
                           session :active-input-boundary-marker)
                          (agent-ide--session-metadata-get
                           session :input-display-start-marker)
                          (agent-ide-session-input-prompt-start-marker session)))
              ((marker-buffer marker)))
    (with-current-buffer (agent-ide-session-buffer session)
      (agent-ide-renderer--writable
        (goto-char (marker-position marker))
        (unless (or (bobp)
                    (save-excursion
                      (backward-char 1)
                      (looking-at-p "\n")))
          (insert "\n")
          (agent-ide-renderer--freeze-region (1- (point)) (point)))))
    (agent-ide--session-metadata-put session :active-input-boundary-marker nil)))

(defun agent-ide-renderer--tool-record (session key)
  "Return SESSION tool record for KEY."
  (gethash key (agent-ide-session-tool-calls session)))

(defun agent-ide-renderer--put-tool-record (session key record)
  "Store SESSION tool RECORD under KEY."
  (puthash key record (agent-ide-session-tool-calls session)))

(defun agent-ide-renderer--permission-option-label (option)
  "Return a Codex-style label for permission OPTION."
  (let* ((option-id (agent-ide-renderer--permission-option-id option))
         (label (or (map-elt option 'name)
                    (map-elt option 'title)
                    option-id
                    "select"))
         (downcase-label (downcase (format "%s" label))))
    (cond
     ((or (string= downcase-label "allow")
          (equal option-id "allow"))
      "accept")
     ((or (string-match-p "always" downcase-label)
          (equal option-id "alwaysAllow"))
      label)
     ((or (string= downcase-label "reject")
          (equal option-id "reject"))
      "decline")
     (t label))))

(defun agent-ide-renderer--permission-option-id (option)
  "Return OPTION's response identifier."
  (or (map-elt option 'optionId)
      (map-elt option 'id)
      (map-elt option 'kind)))

(defun agent-ide-renderer-pending-permission (session)
  "Return SESSION's newest pending permission as (KEY . RECORD)."
  (let (latest-key latest-record latest-sequence)
    (maphash
     (lambda (key record)
       (when (and (plist-get record :permission)
                  (plist-get record :pending)
                  (or (not latest-record)
                      (> (or (plist-get record :permission-sequence) 0)
                         latest-sequence)))
         (setq latest-key key
               latest-record record
               latest-sequence
               (or (plist-get record :permission-sequence) 0))))
     (agent-ide-session-tool-calls session))
    (when latest-record
      (cons latest-key latest-record))))

(defun agent-ide-renderer-respond-permission (session key option-id)
  "Respond to SESSION permission KEY with OPTION-ID.
When OPTION-ID is nil, cancel the request.  Reject repeated responses."
  (let* ((record (agent-ide-renderer--tool-record session key))
         (respond-fn (plist-get record :respond-fn)))
    (unless (and (plist-get record :permission)
                 (plist-get record :pending)
                 (functionp respond-fn))
      (user-error "Permission request is no longer pending"))
    ;; Mark it first so a fast repeated key press cannot answer twice.  Restore
    ;; pending state if sending the response fails.
    (setq record (plist-put record :pending nil))
    (agent-ide-renderer--put-tool-record session key record)
    (condition-case err
        (funcall respond-fn option-id)
      (error
       (setq record (plist-put record :pending t))
       (agent-ide-renderer--put-tool-record session key record)
       (signal (car err) (cdr err))))))

(defun agent-ide-renderer--insert-approval-label (label)
  "Insert approval LABEL."
  (insert (propertize label 'face 'agent-ide-approval-label-face)))

(defun agent-ide-renderer-update-tool
    (session key title body &optional status style expanded-output collapsed)
  "Insert or replace a tool block in SESSION.
KEY identifies the block.  TITLE, BODY, and STATUS are display text.
STYLE controls the presentation.  By default, render a compact event block.
When STYLE is `foldable', render the older collapsed foldable block.
When COLLAPSED is non-nil, hide the compact block body by default."
  (agent-ide-renderer-reset-stream session)
  (with-current-buffer (agent-ide-session-buffer session)
    (let* ((insert-pos (agent-ide-renderer--insert-position session))
           (restore-point (agent-ide-renderer--input-point-marker session))
           (record (agent-ide-renderer--tool-record session key)))
      (agent-ide-renderer--maybe-save-transcript-position session insert-pos
        (agent-ide-renderer--writable
          (when record
            (when (not title)
              (setq title (plist-get record :title)))
            (when (or (not body)
                      (and (stringp body)
                           (string-empty-p (string-trim body))))
              (setq body (plist-get record :body)))
            (unless expanded-output
              (setq expanded-output (plist-get record :expanded-output)))
            (when (and expanded-output
                       (stringp body)
                       (not (string-match-p "\\[expand\\]" body)))
              (setq body (concat (string-trim-right body) " [expand]")))
            (unless collapsed
              (setq collapsed (plist-get record :collapsed)))
            (unless style
              (setq style (plist-get record :style))))
          (unless style
            (setq style 'tool))
          (if (and record
                   (markerp (plist-get record :start))
                   (markerp (plist-get record :end))
                   (marker-buffer (plist-get record :start))
                   (marker-buffer (plist-get record :end)))
              (progn
                (goto-char (marker-position (plist-get record :start)))
                (agent-ide-renderer--delete-expanded-output-in-region
                 (marker-position (plist-get record :start))
                 (marker-position (plist-get record :end)))
                (delete-region (marker-position (plist-get record :start))
                               (marker-position (plist-get record :end))))
            (goto-char insert-pos)
            (setq record (list :start (copy-marker (point) nil)
                               :end (copy-marker (point) nil))))
          (let ((start (point)))
            (if (not (eq style 'foldable))
                (let ((auto-expand (and expanded-output
                                        (stringp body)
                                        (string-match-p "\\[open diff\\]" body))))
                  (agent-ide-renderer--insert-codex-tool-block
                   title
                   body
                   key
                   collapsed
                   expanded-output
                   auto-expand))
              (agent-ide-renderer--insert-foldable-block
               (or title "Tool call")
               (or body "")
               :status status
               :collapsed t
               :face 'agent-ide-tool-face
               :key key))
            (set-marker (plist-get record :start) start)
            (set-marker (plist-get record :end) (point))
            (setq record (plist-put record :title title))
            (setq record (plist-put record :body (or body "")))
            (setq record (plist-put record :status status))
            (setq record (plist-put record :style style))
            (setq record (plist-put record :expanded-output expanded-output))
            (setq record (plist-put record :collapsed collapsed))
            (when (plist-get record :permission)
              (setq record (plist-put record :pending nil))
              (setq record (plist-put record :options nil))
              (setq record (plist-put record :respond-fn nil)))
            (agent-ide-renderer--put-tool-record session key record))))
        (agent-ide-renderer--restore-input-point-marker restore-point)
        (agent-ide-renderer--sync-following-window-points session)
        (agent-ide--touch-session session)
        (when (fboundp 'agent-ide-sidebar-on-sessions-changed)
          (agent-ide-sidebar-on-sessions-changed)))))

(defun agent-ide-renderer-insert-permission
    (session key title body options respond-fn)
  "Render a permission request in SESSION.
KEY identifies the block.  TITLE and BODY are text.  OPTIONS are ACP
permission options.  RESPOND-FN receives the chosen option id."
  (agent-ide-renderer-reset-stream session)
  (with-current-buffer (agent-ide-session-buffer session)
    (let* ((insert-pos (agent-ide-renderer--insert-position session))
           (restore-point (agent-ide-renderer--input-point-marker session))
           (record (agent-ide-renderer--tool-record session key))
           (permission-sequence
            (1+ (or (agent-ide--session-metadata-get
                     session :permission-sequence)
                    0))))
      (agent-ide--session-metadata-put
       session :permission-sequence permission-sequence)
      (agent-ide-renderer--maybe-save-transcript-position session insert-pos
        (agent-ide-renderer--writable
          (when (and record
                     (markerp (plist-get record :start))
                     (markerp (plist-get record :end))
                     (marker-buffer (plist-get record :start)))
            (delete-region (marker-position (plist-get record :start))
                           (marker-position (plist-get record :end))))
          (goto-char insert-pos)
          (let ((start (point)))
            (insert "\n")
            (insert (propertize "[Approval required]"
                                'face 'agent-ide-approval-header-face))
            (insert "\n\n")
            (when (and body (not (string-empty-p (string-trim body))))
              (let ((command (or title body)))
                (agent-ide-renderer--insert-approval-label
                 "Run the following command?")
                (insert "\n\n    ")
                (insert (propertize command 'face 'agent-ide-tool-face))
                (insert "\n\n")
                (unless (string= (string-trim body) (string-trim command))
                  (insert (string-trim-right body))
                  (insert "\n\n"))))
              (dolist (option (append options nil))
                (let* ((option-id
                        (agent-ide-renderer--permission-option-id option))
                       (label (agent-ide-renderer--permission-option-label
                               option)))
                  (insert-text-button
                   (format "[%s]" label)
                   'follow-link t
                   'keymap agent-ide-action-button-map
                   'action (lambda (_button)
                             (agent-ide-renderer-respond-permission
                              session key option-id)))
                  (insert "\n")))
              (insert-text-button
               "[cancel]"
               'follow-link t
               'keymap agent-ide-action-button-map
               'action (lambda (_button)
                         (agent-ide-renderer-respond-permission
                          session key nil)))
              (insert "\n")
              (agent-ide-renderer--freeze-region start (point))
              (agent-ide-renderer--put-tool-record
               session key (list :start (copy-marker start nil)
                                 :end (copy-marker (point) nil)
                                 :permission t
                                 :pending t
                                 :permission-sequence permission-sequence
                                 :options (append options nil)
                                 :respond-fn respond-fn
                                 :title (or title "Approval"))))))
        (agent-ide-renderer--restore-input-point-marker restore-point)
        (agent-ide-renderer--sync-following-window-points session)
        (agent-ide--touch-session session)
        (when (fboundp 'agent-ide-sidebar-on-sessions-changed)
          (agent-ide-sidebar-on-sessions-changed))
        (message
         (concat "Approval required: C-c C-a allow; "
                 "C-u C-c C-a always; C-c C-d decline; C-c C-p options")))))

(provide 'agent-ide-renderer)

;;; agent-ide-renderer.el ends here
