;;; agent-ide-resume-test.el --- Session restoration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-ide-session)
(require 'agent-ide-sidebar)

(defconst agent-ide-resume-test--fixture
  (expand-file-name "test/fixtures/acp-resume-agent.py"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun agent-ide-resume-test--wait (predicate)
  "Wait up to five seconds for PREDICATE while processing ACP output."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall predicate))))

(defmacro agent-ide-resume-test--with-session (&rest body)
  "Run BODY with a rendered SESSION and isolated runtime state."
  (declare (indent 0))
  `(with-temp-buffer
     (agent-ide-session-mode)
     (let* ((agent-ide-history-file nil)
            (agent-ide--sessions nil)
            (agent-ide--session-metadata (make-hash-table :test 'eq))
            (session (agent-ide--make-session
                      :directory default-directory :buffer (current-buffer)
                      :acp-session-id "saved-id" :status "disconnected"
                      :client (list (cons :test t))
                      :tool-calls (make-hash-table :test 'equal))))
       (setq-local agent-ide--session session)
       (agent-ide-renderer-initialize-buffer session)
       (agent-ide-renderer-create-prompt session)
       ,@body)))

(defun agent-ide-resume-test--update (kind text &optional id)
  "Create a history notification of KIND containing TEXT and message ID."
  `((method . "session/update")
    (params . ((sessionId . "saved-id")
               (update . ((sessionUpdate . ,kind) (messageId . ,id)
                          (content . ((type . "text") (text . ,text)))))))))

(ert-deftest agent-ide-resume-selects-method-by-capability-and-transcript ()
  (dolist (case '(("{\"sessionCapabilities\":{\"resume\":{}},\"loadSession\":true}" t "session/resume")
                  ("{\"sessionCapabilities\":{\"resume\":{}},\"loadSession\":true}" nil "session/load")
                  ("{\"sessionCapabilities\":{\"resume\":{}}}" nil "session/resume")
                  ("{\"loadSession\":true}" t "session/load")
                  ("{\"loadSession\":false}" t nil)))
    (agent-ide-resume-test--with-session
     (setf (agent-ide-session-capabilities session)
           (json-parse-string (nth 0 case) :object-type 'alist
                              :null-object nil :false-object nil))
     (agent-ide--session-metadata-put session :has-transcript (nth 1 case))
     (let (sent)
       (cl-letf (((symbol-function 'agent-ide-protocol-send-request)
                  (lambda (_session request &rest _callbacks) (setq sent request))))
         (agent-ide-protocol-restore-session session))
       (should (equal (map-elt sent :method) (nth 2 case)))
       (if sent
           (progn
             (should (equal (agent-ide--get-in sent '(:params sessionId)) "saved-id"))
             (should (equal (agent-ide-session-status session) "resuming")))
         (should (equal (agent-ide-session-status session) "disconnected")))))))

(ert-deftest agent-ide-resume-keeps-buffer-draft-id-and-model ()
  (agent-ide-resume-test--with-session
   (agent-ide-renderer-append-status session "Original transcript")
   (agent-ide-renderer-replace-current-input session "unfinished draft")
   (agent-ide--session-metadata-put session :has-transcript t)
   (setf (agent-ide-session-capabilities session) '((sessionCapabilities . ((resume)))))
   (let ((agent-ide-model "new-session-default"))
     (cl-letf (((symbol-function 'agent-ide-protocol-send-request)
                (lambda (_session _request &rest callbacks)
                  (funcall (plist-get callbacks :on-success)
                           '((models . ((currentModelId . "restored-model")))))))
               ((symbol-function 'agent-ide-protocol-set-model)
                (lambda (&rest _) (ert-fail "Must not apply the default model"))))
       (agent-ide-protocol-restore-session session)))
   (should (equal (agent-ide-renderer-current-input session) "unfinished draft"))
   (should (string-match-p "Original transcript" (buffer-string)))
   (should (equal (agent-ide-session-acp-session-id session) "saved-id"))
   (should (equal (agent-ide-session-status session) "idle"))
   (should (equal (map-elt (agent-ide-session-models session) 'currentModelId)
                  "restored-model"))))

(ert-deftest agent-ide-resume-load-replaces-history-only-after-success ()
  (agent-ide-resume-test--with-session
   (setf (agent-ide-session-capabilities session) '((loadSession . t)))
   (agent-ide-renderer-append-status session "Old transcript")
   (agent-ide-renderer-replace-current-input session "my draft")
   (let ((chunks 0)
         (agent-ide-message-chunk-functions (list (lambda (&rest _) (ert-fail "Live hook during replay"))))
         success)
     (cl-letf (((symbol-function 'agent-ide-protocol-send-request)
                (lambda (_session _request &rest callbacks)
                  (setq success (plist-get callbacks :on-success)))))
       (agent-ide-protocol-restore-session session))
     (dolist (notification
              (list (agent-ide-resume-test--update "user_message_chunk" "hello " "u1")
                    (agent-ide-resume-test--update "user_message_chunk" "world" "u1")
                    (agent-ide-resume-test--update "agent_message_chunk" "Historical answer" "a1")))
       (cl-incf chunks)
       (agent-ide-transcript-handle-notification session notification))
     (should (= chunks 3))
     (should (string-match-p "Old transcript" (buffer-string)))
     (should-not (string-match-p "Historical answer" (buffer-string)))
     ;; The user can continue editing while load is in progress.
     (agent-ide-renderer-replace-current-input session "updated draft")
     (funcall success nil)
     (should-not (string-match-p "Old transcript" (buffer-string)))
     (should (string-match-p "hello world" (buffer-string)))
     (should (string-match-p "Historical answer" (buffer-string)))
     (should (equal (agent-ide-session-prompt-history session) '("hello world")))
     (should (equal (agent-ide-renderer-current-input session) "updated draft"))
     (should-not (agent-ide--session-metadata-get session :loading-history))
     (let ((agent-ide-message-chunk-functions (list (lambda (&rest _) (cl-incf chunks)))))
       (agent-ide-transcript-handle-notification
        session (agent-ide-resume-test--update "agent_message_chunk" "Live answer" "a2"))
       (should (= chunks 4))))))

(ert-deftest agent-ide-resume-load-failure-retains-old-history-and-draft ()
  (agent-ide-resume-test--with-session
   (setf (agent-ide-session-capabilities session) '((loadSession . t)))
   (agent-ide-renderer-append-status session "Original transcript")
   (agent-ide-renderer-replace-current-input session "draft")
   (let (failure)
     (cl-letf (((symbol-function 'agent-ide-protocol-send-request)
                (lambda (_session _request &rest callbacks)
                  (setq failure (plist-get callbacks :on-failure)))))
       (agent-ide-protocol-restore-session session))
     (agent-ide-transcript-handle-notification
      session (agent-ide-resume-test--update "agent_message_chunk" "Partial replay"))
     (funcall failure '((message . "Session not found")))
     (should (equal (agent-ide-session-status session) "disconnected"))
     (should (string-match-p "Original transcript" (buffer-string)))
     (should-not (string-match-p "Partial replay" (buffer-string)))
     (should (equal (agent-ide-renderer-current-input session) "draft"))
     (should-not (agent-ide--session-metadata-get session :replay-notifications)))))

(ert-deftest agent-ide-resume-blocks-submit-before-freezing-draft ()
  (dolist (status '("disconnected" "initializing" "resuming" "running"))
    (agent-ide-resume-test--with-session
     (setf (agent-ide-session-status session) status)
     (agent-ide-renderer-replace-current-input session "keep me")
     (should-error (agent-ide-submit) :type 'user-error)
     (should-error (agent-ide-deliver-prompt session "replacement") :type 'user-error)
     (should (equal (agent-ide-renderer-current-input session) "keep me"))
     (should-not (agent-ide-session-prompt-history session)))))

(ert-deftest agent-ide-resume-invalidates-permissions-and-stale-callbacks ()
  (agent-ide-resume-test--with-session
   (puthash "permission-1" '(:permission t :pending t :respond-fn ignore)
            (agent-ide-session-tool-calls session))
   (let (success failure called)
     (cl-letf (((symbol-function 'acp-send-request)
                (lambda (&rest args)
                  (setq success (plist-get args :on-success)
                        failure (plist-get args :on-failure)))))
       (agent-ide-protocol-send-request
        session '((:method . "test"))
        :on-success (lambda (_) (setq called t))
        :on-failure (lambda (_) (setq called t))))
     (setf (agent-ide-session-client session) '((:different . t)))
     (agent-ide--invalidate-connection session)
     (funcall success nil)
     (funcall failure nil)
     (should-not called)
     (should-not (agent-ide-session-active-requests session))
      (should-error (agent-ide-renderer-respond-permission session "permission-1" "allow")
                    :type 'user-error))
    (let (keys)
      (cl-letf (((symbol-function 'agent-ide-renderer-insert-permission)
                 (lambda (_session key &rest _) (push key keys))))
        (let ((request '((id . 1) (params . ((toolCall . ((toolCallId . "reused"))))))))
          (agent-ide-transcript--handle-permission session request)
          (agent-ide--invalidate-connection session)
          (agent-ide-transcript--handle-permission session request)))
      (should-not (equal (car keys) (cadr keys))))))

(ert-deftest agent-ide-resume-history-persists-and-filters-without-credentials ()
  (agent-ide-resume-test--with-session
   (let* ((directory (make-temp-file "agent-ide-history-test-" t))
          (agent-ide-history-file (expand-file-name "sessions.json" directory)))
     (unwind-protect
         (progn
           (agent-ide--session-metadata-put session :command '("agent" "--token" "secret"))
           (agent-ide-history-record session)
           (agent-ide-history-record session "Fix scrolling\nextra content")
           (agent-ide-history-record session "Do not replace the title")
           (let ((entries (agent-ide-history-read)))
             (should (= (length entries) 1))
             (should (equal (map-elt (car entries) 'title) "Fix scrolling"))
             (should (= (length (agent-ide-history-candidates default-directory)) 1))
             (should-not (agent-ide-history-candidates directory)))
           (with-temp-buffer
             (insert-file-contents agent-ide-history-file)
             (should-not (string-match-p "secret\\|extra content" (buffer-string)))))
       (delete-directory directory t)))))

(ert-deftest agent-ide-resume-history-corruption-is-not-overwritten ()
  (agent-ide-resume-test--with-session
   (let ((agent-ide-history-file (make-temp-file "agent-ide-corrupt-" nil nil "broken json")))
     (unwind-protect
         (progn
           (agent-ide--session-metadata-put session :command '("agent"))
           (agent-ide-history-record session)
           (should-error (agent-ide-history-read))
           (with-temp-buffer
             (insert-file-contents agent-ide-history-file)
             (should (equal (buffer-string) "broken json"))))
       (delete-file agent-ide-history-file)))))

(ert-deftest agent-ide-resume-rejects-backend-mismatch-before-spawning ()
  (let ((agent-ide--sessions nil)
        (agent-ide-command '("different-agent")))
    (cl-letf (((symbol-function 'agent-ide--create-session)
               (lambda (&rest _) (ert-fail "Must not spawn the wrong backend"))))
      (should-error
       (agent-ide--resume-entry
        `((sessionId . "saved") (directory . ,default-directory)
          (backend . ,(agent-ide-history-backend-key '("original-agent")))
          (backendName . "original-agent")))
       :type 'user-error))))

(ert-deftest agent-ide-resume-real-process-roundtrip ()
  "Exercise real JSON-RPC transport, idle exit, reconnection and cold load."
  (skip-unless (and (fboundp 'acp-make-client) (executable-find "python3")))
  (dolist (capability '("both" "resume" "load"))
    (let* ((directory (make-temp-file "agent-ide-acp-test-" t))
           (state-file (expand-file-name "state.json" directory))
           (agent-ide-history-file (expand-file-name "sessions.json" directory))
           (agent-ide-command (list (executable-find "python3") "-u"
                                    agent-ide-resume-test--fixture state-file capability))
           (agent-ide-environment nil)
           (agent-ide-model nil)
           (agent-ide--sessions nil)
           (agent-ide--session-metadata (make-hash-table :test 'eq))
           (agent-ide-message-chunk-functions nil)
           (agent-ide-prompt-response-functions nil)
           (agent-ide-prompt-failure-functions nil)
           (agent-ide-sidebar-auto-show nil)
           buffers)
      (unwind-protect
          (cl-letf (((symbol-function 'agent-ide--display-buffer) #'ignore))
            (let ((session (agent-ide--start-session directory)))
              (push (agent-ide-session-buffer session) buffers)
              (agent-ide-resume-test--wait
               (lambda () (equal (agent-ide-session-status session) "idle")))
              (agent-ide-deliver-prompt session "first turn")
              (agent-ide-resume-test--wait
               (lambda () (equal (agent-ide-session-status session) "idle")))
              (agent-ide-renderer-replace-current-input session "draft with spaces  \n")
              ;; Terminate an idle process: ACP has no pending request to fail.
              (delete-process (map-elt (agent-ide-session-client session) :process))
              (agent-ide-resume-test--wait
               (lambda () (equal (agent-ide-session-status session) "disconnected")))
              (agent-ide--resume-session session)
              (agent-ide-resume-test--wait
               (lambda () (equal (agent-ide-session-status session) "idle")))
              (should (equal (agent-ide-renderer-current-input session t)
                             "draft with spaces  \n"))
              (with-current-buffer (agent-ide-session-buffer session)
                (save-excursion
                  (goto-char (point-min))
                  (should (= (how-many "Answer: first turn" (point-min) (point-max)) 1))))
              (agent-ide-deliver-prompt session "second turn")
              (agent-ide-resume-test--wait
               (lambda () (equal (agent-ide-session-status session) "idle")))
              ;; Closing the transcript keeps its persistent index entry.
              (kill-buffer (agent-ide-session-buffer session))
              (should-not agent-ide--sessions)
              (let ((restored (agent-ide--resume-entry (car (agent-ide-history-read)))))
                (push (agent-ide-session-buffer restored) buffers)
                (agent-ide-resume-test--wait
                 (lambda () (equal (agent-ide-session-status restored) "idle")))
                (with-current-buffer (agent-ide-session-buffer restored)
                  (if (equal capability "resume")
                      (should (string-match-p "Earlier messages are not loaded" (buffer-string)))
                    (should (string-match-p "Answer: second turn" (buffer-string)))))
                (agent-ide-deliver-prompt restored "third turn")
                (agent-ide-resume-test--wait
                 (lambda () (equal (agent-ide-session-status restored) "idle"))))
              (with-temp-buffer
                (insert-file-contents state-file)
                (let* ((state (json-parse-buffer :object-type 'alist :array-type 'list))
                       (requests (map-elt state 'requests)))
                  (should (= (cl-count "session/new" requests :test #'equal) 1))
                  (should (= (cl-count "session/prompt" requests :test #'equal) 3))
                  (should (equal (cl-remove-if-not
                                  (lambda (method) (member method '("session/load" "session/resume")))
                                  requests)
                                 (pcase capability
                                   ("both" '("session/resume" "session/load"))
                                   ("resume" '("session/resume" "session/resume"))
                                   ("load" '("session/load" "session/load")))))))))
        (agent-ide--cleanup-all-sessions)
        (dolist (buffer buffers) (when (buffer-live-p buffer) (kill-buffer buffer)))
        (delete-directory directory t)))))

(provide 'agent-ide-resume-test)
;;; agent-ide-resume-test.el ends here
