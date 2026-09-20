;;; agent-ide-inline-test.el --- Tests for agent-ide-inline -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-ide-session)
(require 'agent-ide-inline)

(defun agent-ide-inline-test--session ()
  (agent-ide--make-session
   :directory default-directory
   :buffer (current-buffer)
   :tool-calls (make-hash-table :test 'equal)
   :prompt-history nil
   :acp-session-id "test-session"))

;;; Extension hook wiring (transcript + protocol)

(ert-deftest agent-ide-inline-chunk-hook-fires ()
  "Transcript handler runs `agent-ide-message-chunk-functions' with text."
  (let* ((session (agent-ide-inline-test--session))
         (seen nil)
         (agent-ide-message-chunk-functions
          (list (lambda (s text) (setq seen (list s text))))))
    (cl-letf (((symbol-function 'agent-ide-renderer-append-stream-chunk)
               (lambda (_s _k _t) nil)))
      (agent-ide-transcript-handle-notification
       session
       '((method . "session/update")
         (params (update (sessionUpdate . "agent_message_chunk")
                         (content ((type . "text") (text . "hello"))))))))
    (should (eq (car seen) session))
    (should (equal (cadr seen) "hello"))))

(ert-deftest agent-ide-inline-prompt-response-hook-fires ()
  "Successful prompt runs `agent-ide-prompt-response-functions'."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let* ((session (agent-ide-inline-test--session))
           (seen nil)
           (agent-ide-prompt-response-functions
            (list (lambda (s response) (setq seen (list s response))))))
      (setq-local agent-ide--session session)
      (cl-letf (((symbol-function 'agent-ide-protocol-send-request)
                 (lambda (_s _req &rest args)
                   (funcall (plist-get args :on-success)
                            '((stopReason . "end_turn")))))
                ((symbol-function 'agent-ide-renderer-update-header) #'ignore)
                ((symbol-function 'agent-ide-renderer-reset-stream) #'ignore)
                ((symbol-function 'agent-ide-renderer-finish-stream) #'ignore)
                ((symbol-function 'agent-ide-renderer-follow-input) #'ignore))
        (agent-ide-protocol-send-prompt session "test"))
      (should (eq (car seen) session))
      (should (equal (map-elt (cadr seen) 'stopReason) "end_turn"))
      (should (equal (agent-ide-session-status session) "idle")))))

(ert-deftest agent-ide-inline-prompt-failure-hook-fires ()
  "Failed prompt runs `agent-ide-prompt-failure-functions'."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let* ((session (agent-ide-inline-test--session))
           (seen nil)
           (agent-ide-prompt-failure-functions
            (list (lambda (s error) (setq seen (list s error))))))
      (setq-local agent-ide--session session)
      (cl-letf (((symbol-function 'agent-ide-protocol-send-request)
                 (lambda (_s _req &rest args)
                   (funcall (plist-get args :on-failure)
                            '((message . "boom")))))
                ((symbol-function 'agent-ide-renderer-update-header) #'ignore)
                ((symbol-function 'agent-ide-renderer-reset-stream) #'ignore)
                ((symbol-function 'agent-ide-renderer-follow-input) #'ignore))
        (agent-ide-protocol-send-prompt session "test"))
      (should (eq (car seen) session))
      (should (equal (map-elt (cadr seen) 'message) "boom")))))

;;; Session resolution and readiness

(ert-deftest agent-ide-inline-resolve-returns-matching-session ()
  (let* ((dir (file-truename default-directory))
         (s1 (agent-ide-inline-test--session))
         (s2 (agent-ide-inline-test--session)))
    (setf (agent-ide-session-directory s1) (file-truename "/somewhere/else")
          (agent-ide-session-directory s2) dir)
    (cl-letf (((symbol-function 'agent-ide--working-directory)
               (lambda () default-directory))
              ((symbol-function 'agent-ide--start-session)
               (lambda (&optional _d) (error "should not start")))
              (agent-ide--sessions (list s1 s2)))
      (should (eq (agent-ide-inline--resolve-session) s2)))))

(ert-deftest agent-ide-inline-resolve-starts-new-session-when-no-match ()
  (let ((started nil))
    (cl-letf (((symbol-function 'agent-ide--working-directory)
               (lambda () "/proj/"))
              ((symbol-function 'agent-ide--start-session)
               (lambda (&optional d) (setq started d) 'new-session))
              (agent-ide--sessions nil))
      (should (eq (agent-ide-inline--resolve-session) 'new-session))
      (should (equal started "/proj/")))))

(ert-deftest agent-ide-inline-ready-p-detects-ready-session ()
  (let ((session (agent-ide-inline-test--session)))
    (setf (agent-ide-session-status session) "idle")
    (should (agent-ide-inline--ready-p session))
    (setf (agent-ide-session-status session) "creating-session")
    (should-not (agent-ide-inline--ready-p session))))

(ert-deftest agent-ide-inline-send-when-ready-sends-immediately ()
  (let* ((session (agent-ide-inline-test--session))
         (sent nil))
    (setf (agent-ide-session-status session) "idle")
    (cl-letf (((symbol-function 'agent-ide-protocol-send-prompt)
               (lambda (s p) (setq sent (list s p)))))
      (agent-ide-inline--send-when-ready
       session (time-add (current-time) 10) "prompt"))
    (should (equal sent (list session "prompt")))))

(ert-deftest agent-ide-inline-send-when-ready-waits-when-not-ready ()
  (let* ((session (agent-ide-inline-test--session))
         (timer nil)
         (sent nil))
    (setf (agent-ide-session-acp-session-id session) nil
          (agent-ide-session-status session) "creating-session")
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _rep fn &rest args)
                 (setq timer (cons fn args))))
              ((symbol-function 'agent-ide-protocol-send-prompt)
               (lambda (s p) (setq sent (list s p)))))
      (agent-ide-inline--send-when-ready
       session (time-add (current-time) 10) "prompt"))
    (should-not sent)
    (should (eq (car timer) #'agent-ide-inline--send-when-ready))))

;;; Reference cycling

(ert-deftest agent-ide-inline-reference-region-bounds ()
  (with-temp-buffer
    (transient-mark-mode 1)
    (insert "hello world")
    (push-mark (point-min) t t)
    (goto-char 6)
    (let* ((origin (copy-marker 1))
           (bounds (agent-ide-inline--reference-bounds origin 'region)))
      (should (equal bounds (cons 1 6)))
      (set-marker origin nil))))

(ert-deftest agent-ide-inline-reference-line-bounds ()
  (with-temp-buffer
    (insert "line one\nline two\nline three\n")
    (let* ((origin (copy-marker 12)) ; inside line two
           (bounds (agent-ide-inline--reference-bounds origin 'line)))
      (should (equal bounds (cons 10 18)))
      (set-marker origin nil))))

(ert-deftest agent-ide-inline-reference-none-bounds ()
  (with-temp-buffer
    (insert "hello")
    (let ((origin (copy-marker 1)))
      (should-not (agent-ide-inline--reference-bounds origin 'none))
      (set-marker origin nil))))

(ert-deftest agent-ide-inline-reference-types-match-mode ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (memq 'defun (agent-ide-inline--reference-types)))
    (should (memq 'window (agent-ide-inline--reference-types)))))

(ert-deftest agent-ide-inline-reference-text-fences-content ()
  (with-temp-buffer
    (insert "hello world")
    (let ((ov (make-overlay 1 6)))
      (let ((text (agent-ide-inline--reference-text ov)))
        (should (string-match-p "hello" text))
        (should (string-match-p "```" text))
        (should (string-match-p "In buffer" text))
        (should (string-match-p "lines 1-1" text)))
      (delete-overlay ov))))

;;; Prompt window

(ert-deftest agent-ide-inline-prompt-mode-binds-send-key ()
  (should (eq (lookup-key agent-ide-inline-prompt-mode-map (kbd "C-c RET"))
              #'agent-ide-inline-send))
  (should (eq (lookup-key agent-ide-inline-prompt-mode-map (kbd "C-c SPC"))
              #'agent-ide-inline-cycle-reference))
  (should (eq (lookup-key agent-ide-inline-prompt-mode-map (kbd "C-c C-k"))
              #'agent-ide-inline-quit)))

(ert-deftest agent-ide-inline-captures-origin-before-resolving-session ()
  "Origin must be the invocation buffer even if session setup switches buffers."
  (with-temp-buffer
    (insert "origin buffer")
    (goto-char 5)
    (let* ((origin-buf (current-buffer))
           (session (agent-ide-inline-test--session))
           (displayed nil))
      ;; Resolving a session can switch buffers (new sessions display and
      ;; select their transcript window); origin must be captured before.
      (cl-letf (((symbol-function 'agent-ide-inline--resolve-session)
                 (lambda ()
                   (switch-to-buffer (get-buffer-create "*elsewhere*"))
                   (goto-char (point-max))
                   session))
                ((symbol-function 'pop-to-buffer)
                 (lambda (buf _action) (setq displayed buf))))
        (agent-ide-inline))
      (with-current-buffer displayed
        (should (eq (marker-buffer agent-ide-inline--origin) origin-buf))
        (should (= (marker-position agent-ide-inline--origin) 5)))
      (kill-buffer displayed)
      (kill-buffer (get-buffer "*elsewhere*")))))

(ert-deftest agent-ide-inline-opens-prompt-buffer-with-session ()
  (with-temp-buffer
    (insert "origin buffer")
    (let* ((session (agent-ide-inline-test--session))
           (displayed nil)
           (agent-ide-inline--origin (copy-marker (point-min))))
      (cl-letf (((symbol-function 'agent-ide-inline--resolve-session)
                 (lambda () session))
                ((symbol-function 'pop-to-buffer)
                 (lambda (buf _action) (setq displayed buf))))
        (agent-ide-inline))
      (should (buffer-live-p displayed))
      (with-current-buffer displayed
        (should (eq major-mode 'agent-ide-inline-prompt-mode))
        (should (eq agent-ide-inline--session session))
        (should (string-match-p "Send" (or header-line-format ""))))
      (kill-buffer displayed))))

(ert-deftest agent-ide-inline-send-sends-prompt-and-creates-overlay ()
  (with-temp-buffer
    (insert "origin buffer")
    (let* ((session (agent-ide-inline-test--session))
           (sent nil)
           (origin (copy-marker 1 t)))
      (setf (agent-ide-session-status session) "idle")
      (with-temp-buffer
        (agent-ide-inline-prompt-mode)
        (setq-local agent-ide-inline--session session)
        (setq-local agent-ide-inline--origin origin)
        (setq-local agent-ide-inline--reference-ov nil)
        (insert "explain this")
        (let ((prompt-buf (current-buffer)))
          (cl-letf (((symbol-function 'agent-ide-protocol-send-prompt)
                     (lambda (s p) (setq sent (list s p))))
                    ((symbol-function 'agent-ide-renderer-append-status)
                     (lambda (_s _text) nil)))
            (agent-ide-inline-send))
          (should (equal (car sent) session))
          (should (string-match-p "explain this" (cadr sent)))
          (should (alist-get session agent-ide-inline--overlays))
          (should-not (buffer-live-p prompt-buf))) ; prompt window closed
        (should (alist-get session agent-ide-inline--overlays))))))

(ert-deftest agent-ide-inline-send-errors-when-busy ()
  (with-temp-buffer
    (agent-ide-inline-prompt-mode)
    (let* ((session (agent-ide-inline-test--session)))
      (setf (agent-ide-session-status session) "running")
      (setq-local agent-ide-inline--session session)
      (setq-local agent-ide-inline--origin (copy-marker 1 t))
      (setq-local agent-ide-inline--reference-ov nil)
      (insert "explain this")
      (cl-letf (((symbol-function 'agent-ide-protocol-send-prompt)
                 (lambda (_s _p) (error "must not send"))))
        (should-error (agent-ide-inline-send) :type 'user-error)))))

;;; Response overlay viewport

(ert-deftest agent-ide-inline-hrule-uses-extended-underline ()
  "The viewport rule is an extended underline, spanning the window."
  (let ((rule agent-ide-inline--hrule))
    (should-not (string-match-p "─" rule))
    (should (equal (text-properties-at 1 rule)
                   '(face (:inherit agent-ide-muted-face
                                     :underline t :extend t))))))

(ert-deftest agent-ide-inline-response-overlay-name-right-aligned ()
  "The session buffer name at the bottom is right-aligned."
  (with-temp-buffer
    (insert "origin\n")
    (goto-char (point-min))
    (let* ((session (agent-ide-inline-test--session))
           (ov (agent-ide-inline--response-overlay-create
                session (current-buffer) (point)))
           (after (overlay-get ov 'after-string))
           (name (buffer-name (agent-ide-session-buffer session)))
           (aligned nil))
      (should (string-match-p (regexp-quote (string-trim name)) after))
      (dotimes (i (length after))
        (let ((disp (get-text-property i 'display after)))
          (when (and (eq (car-safe disp) 'space)
                     (memq 'right (caddr disp)))
            (setq aligned t))))
      (should aligned)
      (agent-ide-inline-clear-response-overlay ov))))

(ert-deftest agent-ide-inline-response-overlay-create-enables-actions-mode ()
  "Creating a viewport enables actions mode in the ORIGIN buffer,
not the buffer the command happens to run in (the prompt window)."
  (with-temp-buffer
    (insert "origin\n")
    (goto-char (point-min))
    (let* ((origin-buf (current-buffer))
           (session (agent-ide-inline-test--session))
           (ov nil))
      ;; send runs in the prompt buffer; overlay lives in the origin buffer
      (with-temp-buffer
        (setq ov (agent-ide-inline--response-overlay-create
                  session origin-buf 1)))
      (with-current-buffer origin-buf
        (should agent-ide-inline--response-overlay-mode)
        (agent-ide-inline-clear-response-overlay ov)))))

(ert-deftest agent-ide-inline-response-overlay-at-point-searches-window ()
  "Actions find the viewport anywhere in the visible window."
  (with-temp-buffer
    (insert "line one\nline two\nline three\n")
    (goto-char 2)
    (set-window-buffer (selected-window) (current-buffer))
    (let* ((session (agent-ide-inline-test--session))
           (ov (agent-ide-inline--response-overlay-create
                session (current-buffer) 4)))
      (should (eq (agent-ide-inline--response-overlay-at-point) ov))
      (agent-ide-inline-clear-response-overlay ov))))

(ert-deftest agent-ide-inline-markdown-bold-across-chunks-renders ()
  "Bold spanning multiple chunks is rendered once complete."
  (with-temp-buffer
    (insert "origin\n")
    (goto-char (point-min))
    (let* ((session (agent-ide-inline-test--session))
           (ov (agent-ide-inline--response-overlay-create
                session (current-buffer) (point))))
      (agent-ide-inline--on-chunk session "here is **bo")
      (agent-ide-inline--on-chunk session "ld** text")
      (let* ((plist (overlay-get ov 'agent-ide-inline))
             (src (plist-get plist :src)))
        (with-current-buffer src
          ;; buffer positions are 1-based; string-match-p is 0-based
          (let ((i (1+ (string-match-p "bold" (buffer-string)))))
            (should i)
            (should (member 'bold
                            (ensure-list
                             (get-text-property i 'face))))))
        (agent-ide-inline-clear-response-overlay ov)))))

(ert-deftest agent-ide-inline-markdown-fence-across-chunks-renders ()
  "Code fences spanning multiple chunks hide delimiters and mark content."
  (with-temp-buffer
    (insert "origin\n")
    (goto-char (point-min))
    (let* ((session (agent-ide-inline-test--session))
           (ov (agent-ide-inline--response-overlay-create
                session (current-buffer) (point))))
      (agent-ide-inline--on-chunk session "```elisp\n(progn\n  (message ")
      (agent-ide-inline--on-chunk session "\"hi\"))\n```\n")
      (let* ((plist (overlay-get ov 'agent-ide-inline))
             (src (plist-get plist :src)))
        (with-current-buffer src
          (let* ((s (buffer-string))
                 ;; buffer positions are 1-based; string-match-p is 0-based
                 (fence-pos (1+ (string-match-p "```elisp" s)))
                 (code-pos (1+ (string-match-p "(progn" s))))
            (should fence-pos)
            (should (equal (get-text-property fence-pos 'display) ""))
            (should (get-text-property code-pos
                                       'agent-ide-markdown-code-content))))
        (agent-ide-inline-clear-response-overlay ov)))))

(ert-deftest agent-ide-inline-response-overlay-streams-chunks ()
  (with-temp-buffer
    (insert "origin text\n")
    (goto-char (point-min))
    (let* ((session (agent-ide-inline-test--session))
           (ov (agent-ide-inline--response-overlay-create
                session (current-buffer) (point))))
      (agent-ide-inline--on-chunk session "hello ")
      (agent-ide-inline--on-chunk session "world")
      (let* ((plist (overlay-get ov 'agent-ide-inline))
             (src (plist-get plist :src)))
        (should (buffer-live-p src))
        (with-current-buffer src
          (should (equal (buffer-string) "hello world"))))
      (should (string-match-p "hello world"
                              (or (overlay-get ov 'after-string) "")))
      (agent-ide-inline-clear-response-overlay ov)
      (should-not (alist-get session agent-ide-inline--overlays)))))

(ert-deftest agent-ide-inline-response-overlay-scroll-index-clamps ()
  (with-temp-buffer
    (insert "origin\n")
    (goto-char (point-min))
    (let* ((session (agent-ide-inline-test--session))
           (ov (agent-ide-inline--response-overlay-create
                session (current-buffer) (point))))
      (agent-ide-inline--on-chunk session (make-string 30 ?x))
      (agent-ide-inline--response-overlay-set-scroll-index ov 99)
      (should (<= (overlay-get ov 'agent-ide-inline-scroll-index)
                  (- 30 (agent-ide-inline--response-overlay-height ov))))
      (agent-ide-inline--response-overlay-set-scroll-index ov -5)
      (should (= (overlay-get ov 'agent-ide-inline-scroll-index) 0))
      (agent-ide-inline-clear-response-overlay ov))))

(ert-deftest agent-ide-inline-response-overlay-resize-changes-height ()
  (with-temp-buffer
    (insert "origin\n")
    (goto-char (point-min))
    (let* ((session (agent-ide-inline-test--session))
           (ov (agent-ide-inline--response-overlay-create
                session (current-buffer) (point)))
           (before (overlay-get ov 'agent-ide-inline-height)))
      (agent-ide-inline--response-overlay-resize ov 3)
      (should (= (overlay-get ov 'agent-ide-inline-height) (+ before 3)))
      (agent-ide-inline--response-overlay-resize ov 'reset)
      (should (= (overlay-get ov 'agent-ide-inline-height)
                 (agent-ide-inline--response-overlay-height ov)))
      (agent-ide-inline-clear-response-overlay ov))))

(ert-deftest agent-ide-inline-response-copy-kills-full-text ()
  (with-temp-buffer
    (insert "origin\n")
    (goto-char (point-min))
    (let* ((session (agent-ide-inline-test--session))
           (ov (agent-ide-inline--response-overlay-create
                session (current-buffer) (point)))
           (killed nil))
      (agent-ide-inline--on-chunk session "full response")
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text) (setq killed text))))
        (agent-ide-inline--response-copy ov))
      (should (equal killed "full response"))
      (agent-ide-inline-clear-response-overlay ov))))

(ert-deftest agent-ide-inline-response-clear-removes-overlay-and-src ()
  (with-temp-buffer
    (insert "origin\n")
    (goto-char (point-min))
    (let* ((session (agent-ide-inline-test--session))
           (ov (agent-ide-inline--response-overlay-create
                session (current-buffer) (point)))
           (src (plist-get (overlay-get ov 'agent-ide-inline) :src)))
      (agent-ide-inline--on-chunk session "text")
      (agent-ide-inline-clear-response-overlay ov)
      (should-not (overlay-buffer ov))
      (should-not (buffer-live-p src))
      (should-not (alist-get session agent-ide-inline--overlays)))))

(ert-deftest agent-ide-inline-response-dispatch-clear-choice ()
  (with-temp-buffer
    (insert "origin\n")
    (goto-char (point-min))
    (let* ((session (agent-ide-inline-test--session))
           (ov (agent-ide-inline--response-overlay-create
                session (current-buffer) (point))))
      (agent-ide-inline--on-chunk session "text")
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (_prompt _choices) '(?c "clear"))))
        (agent-ide-inline--response-overlay-dispatch ov))
      (should-not (overlay-buffer ov))
      (should-not (alist-get session agent-ide-inline--overlays)))))

(ert-deftest agent-ide-inline-response-dispatch-visit-choice ()
  (with-temp-buffer
    (insert "origin\n")
    (goto-char (point-min))
    (let* ((session (agent-ide-inline-test--session))
           (visited nil)
           (ov (agent-ide-inline--response-overlay-create
                session (current-buffer) (point))))
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (_prompt _choices) '(?v "visit")))
                ((symbol-function 'pop-to-buffer)
                 (lambda (buf _action) (setq visited buf))))
        (agent-ide-inline--response-overlay-dispatch ov))
      (should (eq visited (agent-ide-session-buffer session)))
      (should (alist-get session agent-ide-inline--overlays)))))

;;; Turn-end finalization

(ert-deftest agent-ide-inline-prompt-previews-editable-formulas ()
  (let* ((agent-ide-latex--cache (make-hash-table :test 'equal))
         (settings '(dvisvgm 1.0 "#000000"))
         (image '(image :type svg :data "inline-input")))
    (puthash (agent-ide-latex--cache-key "$x$" settings)
             (list :status 'done :image image) agent-ide-latex--cache)
    (cl-letf (((symbol-function 'agent-ide-latex--settings) (lambda () settings)))
      (with-temp-buffer
        (agent-ide-inline-prompt-mode)
        (insert "$x$")
        (run-hooks 'post-command-hook)
        (should (eq (get-text-property 1 'display) image))
        (goto-char 2)
        (run-hooks 'post-command-hook)
        (should-not (get-text-property 1 'display))
        (goto-char (point-max))
        (run-hooks 'post-command-hook)
        (should (eq (get-text-property 1 'display) image))
        (should (equal (buffer-substring-no-properties 1 (point-max)) "$x$"))))))

(ert-deftest agent-ide-inline-formula-completion-refreshes-viewport ()
  (with-temp-buffer
    (insert "origin\n")
    (let* ((session (agent-ide-inline-test--session))
           (ov (agent-ide-inline--response-overlay-create session (current-buffer) 1))
           (src (plist-get (overlay-get ov 'agent-ide-inline) :src))
           (image '(image :type svg :data "test")))
      (unwind-protect
          (let ((agent-ide-latex-preview nil))
            (agent-ide-inline--response-overlay-append-chunk ov "$x$")
            (with-current-buffer src
              (put-text-property 1 4 'agent-ide-latex-key "key")
              (agent-ide-latex--apply
               (list (copy-marker 1) (copy-marker 4) "$x$" "key") image nil))
            (let* ((text (overlay-get ov 'after-string))
                   (start (string-match (regexp-quote "$x$") text)))
              (should start)
              (should (equal (get-text-property start 'display text) image))))
        (agent-ide-inline-clear-response-overlay ov)))))

(ert-deftest agent-ide-inline-response-marks-done ()
  (with-temp-buffer
    (insert "origin\n")
    (goto-char (point-min))
    (let* ((session (agent-ide-inline-test--session))
           (ov (agent-ide-inline--response-overlay-create
                session (current-buffer) (point))))
      (agent-ide-inline--on-chunk session "final answer")
      (agent-ide-inline--on-response session nil)
      (should (plist-get (overlay-get ov 'agent-ide-inline) :done))
      (agent-ide-inline-clear-response-overlay ov))))

(ert-deftest agent-ide-inline-failure-sets-header ()
  (with-temp-buffer
    (insert "origin\n")
    (goto-char (point-min))
    (let* ((session (agent-ide-inline-test--session))
           (ov (agent-ide-inline--response-overlay-create
                session (current-buffer) (point))))
      (agent-ide-inline--on-failure session '((message . "boom")))
      (let ((plist (overlay-get ov 'agent-ide-inline)))
        (should (string-match-p "boom"
                                (or (plist-get plist :header) ""))))
      (agent-ide-inline-clear-response-overlay ov))))
