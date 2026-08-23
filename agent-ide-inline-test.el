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

(ert-deftest agent-ide-inline-strip-fences-removes-surrounding-fence ()
  (should (equal (agent-ide-inline--strip-fences "```python\nx = 1\n```")
                 "x = 1")))

(ert-deftest agent-ide-inline-strip-fences-leaves-plain-text ()
  (should (equal (agent-ide-inline--strip-fences "  plain text  ")
                 "plain text")))

(ert-deftest agent-ide-inline-build-prompt-fills-template-slots ()
  (let ((agent-ide-inline-prompt-template "%i\n\n%c"))
    (should (equal (agent-ide-inline--build-prompt "fix it" "ctx block")
                   "fix it\n\nctx block"))))

(ert-deftest agent-ide-inline-build-prompt-default-has-constraint ()
  (let ((prompt (agent-ide-inline--build-prompt "i" "c")))
    (should (string-match-p "Do not use tools" prompt))
    (should (string-match-p "replacement text" prompt))))

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

(ert-deftest agent-ide-inline-accept-replaces-region-and-is-undoable ()
  (with-temp-buffer
    (setq buffer-undo-list nil)
    (insert "hello world")
    (let* ((session (agent-ide-inline-test--session))
           (state (agent-ide-inline--preview-start
                   session (current-buffer) 1 6 "rewrite")))
      (agent-ide-inline--preview-update state "goodbye")
      (should (equal (buffer-string) "hello world"))
      (agent-ide-inline-accept)
      (should (equal (buffer-string) "goodbye world"))
      (undo)
      (should (equal (buffer-string) "hello world"))
      (should-not agent-ide-inline-preview-mode)
      (should-not (alist-get session agent-ide-inline--previews)))))

(ert-deftest agent-ide-inline-reject-leaves-buffer-unchanged ()
  (with-temp-buffer
    (insert "hello world")
    (let* ((session (agent-ide-inline-test--session))
           (state (agent-ide-inline--preview-start
                   session (current-buffer) 1 6 "rewrite")))
      (agent-ide-inline--preview-update state "goodbye")
      (agent-ide-inline-reject)
      (should (equal (buffer-string) "hello world"))
      (should-not agent-ide-inline-preview-mode)
      (should-not (alist-get session agent-ide-inline--previews)))))

(ert-deftest agent-ide-inline-external-edit-cancels-preview ()
  (with-temp-buffer
    (insert "hello world")
    (let* ((session (agent-ide-inline-test--session))
           (_state (agent-ide-inline--preview-start
                    session (current-buffer) 1 6 "rewrite")))
      (goto-char (point-max))
      (insert "!")
      (should-not (alist-get session agent-ide-inline--previews))
      (should-not agent-ide-inline-preview-mode))))

(ert-deftest agent-ide-inline-chunks-accumulate-into-overlay ()
  (with-temp-buffer
    (insert "hello world")
    (let* ((session (agent-ide-inline-test--session))
           (state (agent-ide-inline--preview-start
                   session (current-buffer) 1 6 "rewrite")))
      (agent-ide-inline--on-chunk session "goo")
      (agent-ide-inline--on-chunk session "dbye")
      (should (equal (plist-get state :text) "goodbye"))
      (should (equal (overlay-get (plist-get state :overlay) 'display)
                     (propertize "goodbye"
                                 'face 'agent-ide-inline-preview-face))))))

(ert-deftest agent-ide-inline-response-finalizes-and-strips-fences ()
  (with-temp-buffer
    (insert "hello world")
    (let* ((session (agent-ide-inline-test--session))
           (state (agent-ide-inline--preview-start
                   session (current-buffer) 1 6 "rewrite")))
      (agent-ide-inline--on-chunk session "```\nbye")
      (agent-ide-inline--on-response session nil)
      (should (plist-get state :done))
      (should (equal (plist-get state :text) "bye")))))

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

(ert-deftest agent-ide-inline-rewrite-sends-prompt-and-starts-preview ()
  (with-temp-buffer
    (insert "hello world")
    (let* ((session (agent-ide-inline-test--session))
           (sent nil)
           (status-line nil))
      (setf (agent-ide-session-status session) "idle")
      (cl-letf (((symbol-function 'agent-ide-inline--resolve-session)
                 (lambda () session))
                ((symbol-function 'agent-ide-protocol-send-prompt)
                 (lambda (s p) (setq sent (list s p))))
                ((symbol-function 'agent-ide-renderer-append-status)
                 (lambda (_s text) (setq status-line text))))
        (agent-ide-inline-rewrite 1 6 "rewrite it"))
      (should (equal (car sent) session))
      (should (string-match-p "rewrite it" (cadr sent)))
      (should (string-match-p "Do not use tools" (cadr sent)))
      (should (string-match-p "hello" (cadr sent)))
      (should (string-match-p "Inline" status-line))
      (should agent-ide-inline-preview-mode)
      (should (alist-get session agent-ide-inline--previews))
      (should (equal (buffer-string) "hello world")))))

(ert-deftest agent-ide-inline-rewrite-errors-when-busy ()
  (with-temp-buffer
    (insert "hello world")
    (let* ((session (agent-ide-inline-test--session)))
      (setf (agent-ide-session-status session) "running")
      (cl-letf (((symbol-function 'agent-ide-inline--resolve-session)
                 (lambda () session))
                ((symbol-function 'agent-ide-protocol-send-prompt)
                 (lambda (_s _p) (error "must not send"))))
        (should-error (agent-ide-inline-rewrite 1 6 "rewrite it")
                      :type 'user-error)
        (should-not agent-ide-inline-preview-mode)))))
