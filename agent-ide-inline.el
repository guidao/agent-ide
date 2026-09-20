;;; agent-ide-inline.el --- Persistent agent session that follows you around -*- lexical-binding: t; -*-

;;; Commentary:

;; gptel-inline-style interaction backed by an agent-ide session.
;; `agent-ide-inline' opens a small prompt window in the current buffer's
;; window; the project's persistent agent session runs in the background
;; (its transcript buffer keeps the full conversation).  Responses stream
;; into an overlay viewport at point in the buffer where you invoked the
;; command.  Selected text or surrounding context can be injected as
;; reference material by cycling with C-c SPC.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'thingatpt)
(require 'agent-ide-core)
(require 'agent-ide-protocol)
(require 'agent-ide-renderer)
(require 'agent-ide-session)

(defgroup agent-ide-inline nil
  "Inline prompts and responses backed by an agent-ide session."
  :group 'agent-ide
  :prefix "agent-ide-inline-")

(defcustom agent-ide-inline-buffer-display-action
  '((display-buffer-below-selected)
    (window-height . 0.33)
    (dedicated . t))
  "Display action used to show the inline prompt buffer.
See `display-buffer' for details."
  :type 'sexp
  :group 'agent-ide-inline)

(defcustom agent-ide-inline-response-overlay-height 8
  "Height in lines of the inline response viewport."
  :type 'natnum
  :group 'agent-ide-inline)

(defcustom agent-ide-inline-reference-types
  '((prog-mode region line defun window buffer)
    (text-mode region line sentence window buffer)
    (t region line window buffer))
  "Things at point to offer as reference for `agent-ide-inline'.
An alist mapping a major (derived) mode to a list of objects.  Any
object recognized by `thing-at-point' is valid, plus `region',
`window' (visible text) and `buffer' (entire buffer)."
  :type '(alist :key-type symbol :value-type (repeat symbol))
  :group 'agent-ide-inline)

(defcustom agent-ide-inline-ready-timeout 30
  "Seconds to wait for a newly created session to become ready."
  :type 'integer
  :group 'agent-ide-inline)

(defconst agent-ide-inline--hrule
  (concat "\n" (propertize "\n"
                            'face '(:inherit agent-ide-muted-face
                                     :underline t :extend t)))
  "Horizontal rule shown in response viewports.
A blank line followed by an extended underline, so the rule spans
the full window width and adapts to window resizes.")

(defvar agent-ide-inline-response-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>")
                #'agent-ide-inline--response-overlay-dispatch)
    (define-key map (kbd "<mouse-4>") #'agent-ide-inline--response-overlay-up)
    (define-key map (kbd "<wheel-up>") #'agent-ide-inline--response-overlay-up)
    (define-key map (kbd "<mouse-5>") #'agent-ide-inline--response-overlay-down)
    (define-key map (kbd "<wheel-down>") #'agent-ide-inline--response-overlay-down)
    map)
  "Keymap for mouse actions on response overlays.")

(defvar agent-ide-inline--response-overlay-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-RET") #'agent-ide-inline--response-overlay-dispatch)
    (define-key map (kbd "C-M-n") #'agent-ide-inline--response-overlay-down)
    (define-key map (kbd "C-M-p") #'agent-ide-inline--response-overlay-up)
    (define-key map (kbd "C-M-v")
                (lambda () (interactive)
                  (agent-ide-inline--response-overlay-page
                   (agent-ide-inline--response-overlay-at-point) 1)))
    (define-key map (kbd "C-M-S-v")
                (lambda () (interactive)
                  (agent-ide-inline--response-overlay-page
                   (agent-ide-inline--response-overlay-at-point) -1)))
    map)
  "Keymap active while an inline response overlay is visible.")

(defvar agent-ide-inline--overlays nil
  "Alist mapping sessions to their active streaming response overlays.
Only one streaming overlay per session; completed overlays stay in
their buffers until cleared and are no longer updated.")

(defvar-local agent-ide-inline--session nil
  "Session the inline prompt window talks to.")

(defvar-local agent-ide-inline--origin nil
  "Marker in the origin buffer where the response should appear.")

(defvar-local agent-ide-inline--reference-ov nil
  "Overlay highlighting the reference in the origin buffer.")

(defvar-local agent-ide-inline--reference-type nil
  "Current reference type, a symbol.")

;;; Session resolution and readiness

(defun agent-ide-inline--resolve-session ()
  "Return the session for the current buffer's project directory.
Strict directory match; start a new session when none matches."
  (let ((dir (file-truename (agent-ide--working-directory))))
    (or (cl-find-if
         (lambda (s)
           (string= dir (file-truename (agent-ide-session-directory s))))
         agent-ide--sessions)
        (agent-ide--start-session (agent-ide--working-directory)))))

(defun agent-ide-inline--ready-p (session)
  "Return non-nil when SESSION is initialized and idle."
  (and (agent-ide-session-acp-session-id session)
       (equal (agent-ide-session-status session) "idle")))

(defun agent-ide-inline--send-when-ready (session deadline prompt)
  "Send PROMPT once SESSION is ready, or retry until DEADLINE."
  (cond
   ((agent-ide-inline--ready-p session)
    (agent-ide-protocol-send-prompt session prompt))
   ((member (agent-ide-session-status session) '("failed" "disconnected"))
    (agent-ide-inline--on-failure session "Session disconnected; use agent-ide-resume"))
   ((time-less-p deadline (current-time))
    (agent-ide-inline--on-failure session "timed out waiting for agent"))
   (t
    (run-at-time 0.3 nil #'agent-ide-inline--send-when-ready
                 session deadline prompt))))

;;; Reference cycling

(defun agent-ide-inline--reference-types (&optional buffer)
  "Return reference types for BUFFER's mode.
BUFFER defaults to the origin buffer, then the current buffer."
  (with-current-buffer
      (or buffer
          (and agent-ide-inline--origin
               (marker-buffer agent-ide-inline--origin))
          (current-buffer))
    (append
     (or (cl-some
          (lambda (mode-list)
            (and (or (eq (car mode-list) t)
                     (derived-mode-p (car mode-list)))
                 (cdr mode-list)))
          agent-ide-inline-reference-types)
         '(region line window buffer))
     '(none))))

(defun agent-ide-inline--reference-bounds (origin type)
  "Return (START . END) for reference TYPE at ORIGIN, or nil.
ORIGIN is a marker in the origin buffer."
  (when (and origin (marker-buffer origin)
             (buffer-live-p (marker-buffer origin)))
    (with-current-buffer (marker-buffer origin)
      (pcase type
        ('region
         (and (use-region-p)
              (cons (region-beginning) (region-end))))
        ('buffer
         (cons (point-min) (point-max)))
        ('window
         (when-let* ((win (get-buffer-window (current-buffer))))
           (with-selected-window win
             (cons (window-start) (window-end win t)))))
        (_
         (save-excursion
           (goto-char origin)
           (pcase type
             ('line
              (cons (pos-bol) (pos-eol)))
             ('defun
              (and (fboundp 'beginning-of-defun)
                   (ignore-errors
                     (beginning-of-defun)
                     (let ((start (point)))
                       (end-of-defun)
                       (cons start (point))))))
             ('sentence
              (when-let* ((bounds (bounds-of-thing-at-point 'sentence)))
                (cons (car bounds) (cdr bounds))))
             ('none nil))))))))

(defun agent-ide-inline--reference-overlay-update (bounds)
  "Highlight BOUNDS as the reference in the origin buffer."
  (let ((origin agent-ide-inline--origin))
    (when (and origin (marker-buffer origin)
               (buffer-live-p (marker-buffer origin)))
      (let ((buffer (marker-buffer origin)))
        (if (and bounds (> (cdr bounds) (car bounds)))
            (if (and agent-ide-inline--reference-ov
                     (overlayp agent-ide-inline--reference-ov)
                     (overlay-buffer agent-ide-inline--reference-ov))
                (move-overlay agent-ide-inline--reference-ov
                              (car bounds) (cdr bounds) buffer)
              (let ((ov (make-overlay (car bounds) (cdr bounds) buffer)))
                (overlay-put ov 'face 'secondary-selection)
                (overlay-put ov 'evaporate t)
                (setq agent-ide-inline--reference-ov ov)))
          (when (and agent-ide-inline--reference-ov
                     (overlayp agent-ide-inline--reference-ov))
            (delete-overlay agent-ide-inline--reference-ov))
          (setq agent-ide-inline--reference-ov nil))))))

(defun agent-ide-inline-cycle-reference (&optional origin interactivep)
  "Cycle the reference type for ORIGIN and highlight it.
Interactively, SPC continues cycling and C-g clears."
  (interactive (list nil t))
  (let ((types (agent-ide-inline--reference-types))
        (current agent-ide-inline--reference-type))
    (let ((tail (memq (or current (car types)) types)))
      (setq agent-ide-inline--reference-type
            (or (cadr tail) (car types))))
    (let ((next agent-ide-inline--reference-type)
          (count 0))
      (while (and next (not (agent-ide-inline--reference-bounds
                             (or origin agent-ide-inline--origin)
                             next))
                  (< count (length types)))
        (setq next (or (cadr (memq next types)) (car types))
              count (1+ count)))
      (setq agent-ide-inline--reference-type next))
    (agent-ide-inline--reference-overlay-update
     (agent-ide-inline--reference-bounds
      (or origin agent-ide-inline--origin)
      agent-ide-inline--reference-type))
    (when (fboundp 'agent-ide-inline--update-prompt-header)
      (agent-ide-inline--update-prompt-header))
    (when interactivep
      (set-transient-map
       (define-keymap
         "SPC" #'agent-ide-inline-cycle-reference
         "C-g" (lambda () (interactive)
                 (agent-ide-inline--reference-overlay-update nil)
                 (setq agent-ide-inline--reference-type 'none)
                 (agent-ide-inline--update-prompt-header)))
       nil nil "Repeat reference cycling with SPC or clear with C-g"))))

(defun agent-ide-inline--reference-text (ov)
  "Build a reference context string from reference overlay OV."
  (when (and ov (overlayp ov) (overlay-buffer ov))
    (with-current-buffer (overlay-buffer ov)
      (let* ((beg (overlay-start ov))
             (end (overlay-end ov))
             (file (buffer-file-name))
             (name (buffer-name))
             (lstart (line-number-at-pos beg))
             (lend (line-number-at-pos end))
             (lang (and file (file-name-extension file))))
        (concat
         (format "\n\nIn buffer \"%s\"" name)
         (and file (format " (%s)" file))
         (format ", lines %d-%d:\n" lstart lend)
         "```" (or lang "") "\n"
         (buffer-substring-no-properties beg end)
         "\n```\n")))))

;;; Prompt window

(defun agent-ide-inline--update-prompt-header ()
  "Refresh the prompt window header line."
  (when (derived-mode-p 'agent-ide-inline-prompt-mode)
    (setq header-line-format
          (concat
           (format " Including %s  |  Send: C-c RET, Reference: C-c SPC, "
                   (propertize
                    (symbol-name (or agent-ide-inline--reference-type 'none))
                    'face 'mode-line-emphasis))
           "Help: C-c ?, Quit: C-c C-k  |  Session: "
           (if agent-ide-inline--session
               (buffer-name (agent-ide-session-buffer agent-ide-inline--session))
             "<none>")))))

(defun agent-ide-inline-help ()
  "Show a quick overview of inline window keys."
  (interactive)
  (message
   (substitute-command-keys
    "Send: \\[agent-ide-inline-send], Cycle reference: \\[agent-ide-inline-cycle-reference], Switch session: \\[agent-ide-inline-switch-session], Visit session: \\[agent-ide-inline-visit-session], Quit: \\[agent-ide-inline-quit]")))

(defun agent-ide-inline-switch-session ()
  "Switch the inline prompt window to another live session."
  (interactive)
  (let ((choices
         (mapcar (lambda (s)
                   (cons (format "%s (%s)"
                                 (agent-ide--directory-name
                                  (agent-ide-session-directory s))
                                 (or (agent-ide-session-status s) "?"))
                         s))
                 agent-ide--sessions)))
    (unless choices
      (user-error "No agent-ide sessions"))
    (let ((choice (completing-read "Session: " choices nil t)))
      (setq agent-ide-inline--session (cdr (assoc choice choices)))
      (agent-ide-inline--update-prompt-header))))

(defun agent-ide-inline-visit-session ()
  "Pop to the session buffer the inline window is talking to."
  (interactive)
  (unless agent-ide-inline--session
    (user-error "No session"))
  (agent-ide--display-buffer
   (agent-ide-session-buffer agent-ide-inline--session)))

(defun agent-ide-inline-quit ()
  "Quit the inline prompt window, clearing any reference highlight."
  (interactive)
  (agent-ide-inline--reference-overlay-update nil)
  (let ((buf (current-buffer)))
    (when (derived-mode-p 'agent-ide-inline-prompt-mode)
      (kill-buffer buf))))

(defun agent-ide-inline-send ()
  "Send the inline prompt to the session and show the response at origin."
  (interactive)
  (let* ((session agent-ide-inline--session)
         (prompt (string-trim
                  (buffer-substring-no-properties (point-min) (point-max)))))
    (unless session
      (user-error "No session: run `agent-ide-inline' first"))
    (when (string-empty-p prompt)
      (user-error "Empty prompt"))
    (when (equal (agent-ide-session-status session) "running")
      (user-error "Agent busy: interrupt the running turn first"))
    (when (member (agent-ide-session-status session) '("disconnected" "failed" "resuming"))
      (user-error "Session is %s; use agent-ide-resume and wait until ready"
                  (agent-ide-session-status session)))
    (let* ((origin agent-ide-inline--origin)
           (reference (agent-ide-inline--reference-text
                       agent-ide-inline--reference-ov))
           (message (if reference (concat prompt reference) prompt)))
      (when (and origin (marker-buffer origin)
                 (buffer-live-p (marker-buffer origin)))
        (agent-ide-inline--response-overlay-create
         session (marker-buffer origin) (marker-position origin)))
      (agent-ide-renderer-append-status
       session (format "Inline: %s" (string-limit prompt 60)))
      (agent-ide-inline--send-when-ready
       session
       (time-add (current-time) agent-ide-inline-ready-timeout)
       message))
    (agent-ide-inline-quit)))

(defvar agent-ide-inline-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c RET") #'agent-ide-inline-send)
    (define-key map (kbd "C-c C-m") #'agent-ide-inline-send)
    (define-key map (kbd "C-c SPC") #'agent-ide-inline-cycle-reference)
    (define-key map (kbd "C-c ?") #'agent-ide-inline-help)
    (define-key map (kbd "C-c C-k") #'agent-ide-inline-quit)
    (define-key map (kbd "C-c C-b") #'agent-ide-inline-switch-session)
    (define-key map (kbd "C-c C-v") #'agent-ide-inline-visit-session)
    map)
  "Keymap for `agent-ide-inline-prompt-mode'.")

(defun agent-ide-inline--preview-input ()
  "Update formula previews in the inline prompt."
  (agent-ide-latex-update-input (point-min) (point-max)))

(define-derived-mode agent-ide-inline-prompt-mode text-mode "Agent-Inline"
  "Major mode for the inline prompt window."
  (setq-local header-line-format "")
  (add-hook 'post-command-hook #'agent-ide-inline--preview-input nil t)
  (agent-ide-inline--update-prompt-header))

;;;###autoload
(defun agent-ide-inline ()
  "Open an inline prompt window for the current project's agent session.

The prompt window opens below the current window.  On send, the
response streams into an overlay viewport at point.  C-c SPC cycles
the reference context (region, line, defun, window, buffer)."
  (interactive)
  (let* ((origin (point-marker)) ; capture before session setup switches buffers
         (session (agent-ide-inline--resolve-session))
         (prompt-buf (generate-new-buffer "*agent-ide-inline*")))
    (with-current-buffer prompt-buf
      (agent-ide-inline-prompt-mode)
      (setq-local agent-ide-inline--session session)
      (setq-local agent-ide-inline--origin origin)
      (setq-local agent-ide-inline--reference-ov nil)
      (setq-local agent-ide-inline--reference-type nil)
      (agent-ide-inline-cycle-reference origin)
      (agent-ide-inline--update-prompt-header))
    (pop-to-buffer prompt-buf agent-ide-inline-buffer-display-action)))

;;; Response overlay viewport

(defun agent-ide-inline--response-overlay-height (ov)
  "Return the response viewport height for OV."
  (or (overlay-get ov 'agent-ide-inline-height)
      agent-ide-inline-response-overlay-height))

(defun agent-ide-inline--response-overlay-create (session buffer point)
  "Create a response viewport for SESSION in BUFFER at POINT."
  (let* ((src (generate-new-buffer " *agent-ide-inline-response*"))
         (ov (make-overlay point point buffer nil t)))
    (with-current-buffer src
      (text-mode)
      (valign-mode 1)
      (add-hook 'agent-ide-latex-updated-functions
                (lambda () (agent-ide-inline--response-overlay-render ov)) nil t)
      (buffer-disable-undo))
    (overlay-put ov 'agent-ide-inline
                 (list :session session
                       :session-buffer (agent-ide-session-buffer session)
                       :src src
                       :header "Response"
                       :done nil))
    (overlay-put ov 'agent-ide-inline-height
                 agent-ide-inline-response-overlay-height)
    (overlay-put ov 'agent-ide-inline-scroll-index 0)
    (setf (alist-get session agent-ide-inline--overlays) ov)
    (with-current-buffer buffer
      (agent-ide-inline--response-overlay-render ov)
      (agent-ide-inline--setup-response-overlay-keymap ov)
      (agent-ide-inline--response-overlay-mode 1))
    ov))

(defun agent-ide-inline--response-overlay-append-chunk (ov chunk)
  "Append CHUNK to response overlay OV and refresh its display.
Re-renders the whole response buffer so markdown constructs that span
multiple chunks (fences, emphasis) render once complete, matching the
transcript's streaming behavior."
  (when (and ov (overlayp ov) (overlay-buffer ov))
    (let* ((plist (overlay-get ov 'agent-ide-inline))
           (src (plist-get plist :src)))
      (when (buffer-live-p src)
        (with-current-buffer src
          (goto-char (point-max))
          (insert chunk)
          (save-excursion
            (agent-ide-renderer-render-markdown-region
             (point-min) (point-max)))))
      (agent-ide-inline--response-overlay-render ov))))

(defun agent-ide-inline--response-overlay-render (ov)
  "Render response overlay OV as an after-string viewport slice."
  (when (and ov (overlayp ov) (overlay-buffer ov))
    (let* ((plist (overlay-get ov 'agent-ide-inline))
           (src (plist-get plist :src))
           (height (agent-ide-inline--response-overlay-height ov))
           (index (or (overlay-get ov 'agent-ide-inline-scroll-index) 0))
           (len (if (buffer-live-p src)
                    (with-current-buffer src
                      (line-number-at-pos (point-max)))
                  0))
           (view-string
            (and (buffer-live-p src)
                 (with-current-buffer src
                   (goto-char (point-min))
                   (buffer-substring
                    (pos-bol (1+ index))
                    (pos-eol (+ index height))))))
           (up (if (> index 0) "⬆ " "  "))
           (down (if (< (+ index height) len) "⬇ " "  "))
           (header (format "%s%s %s"
                           up down (or (plist-get plist :header) "Response")))
           (session-name
            (buffer-name (plist-get plist :session-buffer))))
      (overlay-put ov 'agent-ide-inline-scroll-index index)
      (overlay-put
       ov 'after-string
       (propertize
        (concat agent-ide-inline--hrule
                (propertize header 'face 'agent-ide-header-face) "\n"
                (or view-string "")
                agent-ide-inline--hrule
                (propertize " " 'display
                            `(space :align-to
                                    (- right ,(length session-name))))
                (propertize session-name 'face 'agent-ide-muted-face))
        'keymap agent-ide-inline-response-overlay-map
        'pointer 'hand)))))

(defun agent-ide-inline--response-overlay-set-scroll-index (ov index)
  "Set scroll INDEX for OV, clamped to the valid range."
  (let* ((height (agent-ide-inline--response-overlay-height ov))
         (plist (overlay-get ov 'agent-ide-inline))
         (src (plist-get plist :src))
         (len (if (buffer-live-p src)
                  (with-current-buffer src
                    (line-number-at-pos (point-max)))
                0))
         (max-index (max 0 (- len height))))
    (overlay-put ov 'agent-ide-inline-scroll-index
                 (min (max 0 index) max-index))))

(defun agent-ide-inline--response-overlay-scroll-to (index ov)
  "Scroll OV to display line INDEX and re-render."
  (when (and ov (overlayp ov) (overlay-buffer ov))
    (agent-ide-inline--response-overlay-set-scroll-index ov index)
    (agent-ide-inline--response-overlay-render ov)))

(defun agent-ide-inline--response-overlay-down (&optional ov)
  "Scroll response overlay OV down by one line."
  (interactive (list (agent-ide-inline--response-overlay-at-point)))
  (when ov
    (agent-ide-inline--response-overlay-scroll-to
     (1+ (or (overlay-get ov 'agent-ide-inline-scroll-index) 0)) ov)))

(defun agent-ide-inline--response-overlay-up (&optional ov)
  "Scroll response overlay OV up by one line."
  (interactive (list (agent-ide-inline--response-overlay-at-point)))
  (when ov
    (agent-ide-inline--response-overlay-scroll-to
     (1- (or (overlay-get ov 'agent-ide-inline-scroll-index) 0)) ov)))

(defun agent-ide-inline--response-overlay-page (ov delta)
  "Scroll OV by DELTA pages (its full height)."
  (when ov
    (agent-ide-inline--response-overlay-scroll-to
     (+ (or (overlay-get ov 'agent-ide-inline-scroll-index) 0)
        (* delta (agent-ide-inline--response-overlay-height ov)))
     ov)))

(defun agent-ide-inline--response-overlay-resize (ov delta)
  "Resize response overlay OV by DELTA lines; non-numeric resets."
  (interactive (list (agent-ide-inline--response-overlay-at-point)
                     (prefix-numeric-value current-prefix-arg)))
  (when ov
    (if (numberp delta)
        (overlay-put ov 'agent-ide-inline-height
                     (max 2 (+ delta (agent-ide-inline--response-overlay-height ov))))
      (overlay-put ov 'agent-ide-inline-height
                   agent-ide-inline-response-overlay-height))
    (agent-ide-inline--response-overlay-render ov)))

(defun agent-ide-inline--response-overlay-at-point ()
  "Return the inline response overlay visible in the selected window.
Searches the visible window range, so actions work anywhere while the
viewport is on screen."
  (let* ((is-mouse-event (consp last-input-event))
         (win (if is-mouse-event
                  (posn-window (event-start last-input-event))
                (selected-window)))
         (pos (if is-mouse-event
                  (posn-point (event-start last-input-event))
                (point))))
    (when (and win pos)
      (with-selected-window win
        (cl-find-if (lambda (ov) (overlay-get ov 'agent-ide-inline))
                    (nconc (overlays-in pos (window-end))
                           (overlays-in (1- (window-start)) pos)))))))

(define-minor-mode agent-ide-inline--response-overlay-mode
  "Minor mode enabling keyboard actions on inline response overlays."
  :lighter " Inline"
  :keymap agent-ide-inline--response-overlay-mode-map)

(defun agent-ide-inline--setup-response-overlay-keymap (ov)
  "Toggle the response mode for OV based on window visibility."
  (letrec ((toggle
            (lambda (win _win-start)
              (if (and (overlayp ov) (overlay-buffer ov)
                       (eq (overlay-buffer ov) (current-buffer))
                       (pos-visible-in-window-p (overlay-end ov) win))
                  (or agent-ide-inline--response-overlay-mode
                      (agent-ide-inline--response-overlay-mode 1))
                (agent-ide-inline--response-overlay-mode -1)))))
    (with-current-buffer (overlay-buffer ov)
      (add-hook 'window-scroll-functions toggle nil t))))

;;; Response actions

(defun agent-ide-inline--response-visit (ov)
  "Pop to the session buffer associated with OV."
  (when-let* ((plist (overlay-get ov 'agent-ide-inline))
              (session-buffer (plist-get plist :session-buffer))
              ((buffer-live-p session-buffer)))
    (pop-to-buffer session-buffer agent-ide-new-session-split)))

(defun agent-ide-inline--response-reply (ov)
  "Open the inline prompt window again to continue the conversation."
  (when-let* ((plist (overlay-get ov 'agent-ide-inline)))
    (when (overlay-buffer ov)
      (with-current-buffer (overlay-buffer ov)
        (goto-char (overlay-start ov))))
    (call-interactively #'agent-ide-inline)))

(defun agent-ide-inline--response-copy (ov)
  "Copy the full response text of OV to the kill ring."
  (when-let* ((plist (overlay-get ov 'agent-ide-inline))
              (src (plist-get plist :src))
              ((buffer-live-p src)))
    (kill-new (with-current-buffer src (buffer-string)))))

(defun agent-ide-inline-clear-response-overlay (ov &optional abort)
  "Remove response overlay OV.
With prefix argument ABORT, also cancel the session's active turn."
  (interactive (list (agent-ide-inline--response-overlay-at-point)
                     current-prefix-arg))
  (when (and ov (overlayp ov))
    (let ((plist (overlay-get ov 'agent-ide-inline)))
      (when (and abort (plist-get plist :session))
        (ignore-errors
          (agent-ide-protocol-cancel (plist-get plist :session))))
      (setq agent-ide-inline--overlays
            (assq-delete-all (plist-get plist :session)
                             agent-ide-inline--overlays))
      (when (buffer-live-p (plist-get plist :src))
        (kill-buffer (plist-get plist :src)))
      (when (overlay-buffer ov)
        (with-current-buffer (overlay-buffer ov)
          (agent-ide-inline--response-overlay-mode -1)))
      (delete-overlay ov))))

(defun agent-ide-inline--response-overlay-dispatch (ov)
  "Show an action menu for response overlay OV."
  (interactive (list (agent-ide-inline--response-overlay-at-point)))
  (unless (and ov (overlayp ov) (overlay-buffer ov))
    (user-error "No inline response overlay"))
  (pcase-let ((`(,choice . ,_desc)
               (read-multiple-choice
                "Action"
                '((?v "visit") (?r "reply") (?c "clear")
                  (?w "copy") (?+ "height+") (?- "height-")
                  (?q "quit")))))
    (pcase choice
      (?v (agent-ide-inline--response-visit ov))
      (?r (agent-ide-inline--response-reply ov))
      (?c (agent-ide-inline-clear-response-overlay ov))
      (?w (agent-ide-inline--response-copy ov))
      (?+ (agent-ide-inline--response-overlay-resize ov 3))
      (?- (agent-ide-inline--response-overlay-resize ov -3))
      (?q (agent-ide-inline-clear-response-overlay ov)))))

;;; Hook handlers

(defun agent-ide-inline--on-chunk (session text)
  "Append chunk TEXT to SESSION's streaming response overlay."
  (when-let* ((ov (alist-get session agent-ide-inline--overlays))
              ((overlayp ov))
              ((overlay-buffer ov)))
    (agent-ide-inline--response-overlay-append-chunk ov text)))

(defun agent-ide-inline--on-response (session _response)
  "Mark SESSION's streaming response overlay as complete."
  (when-let* ((ov (alist-get session agent-ide-inline--overlays))
              ((overlayp ov))
              ((overlay-buffer ov)))
    (let ((plist (overlay-get ov 'agent-ide-inline)))
      (setf (plist-get plist :done) t)
      (overlay-put ov 'agent-ide-inline plist))
    (agent-ide-inline--response-overlay-render ov)
    (message "Inline response ready (M-RET for actions)")))

(defun agent-ide-inline--on-failure (session error)
  "Show the failure on SESSION's streaming response overlay."
  (when-let* ((ov (alist-get session agent-ide-inline--overlays))
              ((overlayp ov))
              ((overlay-buffer ov)))
    (let ((plist (overlay-get ov 'agent-ide-inline)))
      (setf (plist-get plist :header)
            (format "Error: %s"
                    (or (map-elt error 'message)
                        (and (stringp error) error)
                        (format "%S" error))))
      (setf (plist-get plist :done) t)
      (overlay-put ov 'agent-ide-inline plist))
    (agent-ide-inline--response-overlay-render ov)))

(add-hook 'agent-ide-message-chunk-functions #'agent-ide-inline--on-chunk)
(add-hook 'agent-ide-prompt-response-functions #'agent-ide-inline--on-response)
(add-hook 'agent-ide-prompt-failure-functions #'agent-ide-inline--on-failure)

(provide 'agent-ide-inline)

;;; agent-ide-inline.el ends here
