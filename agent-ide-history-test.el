;;; agent-ide-history-test.el --- Backend discovery tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-ide-session)

(defmacro agent-ide-history-test--with-backend (responses &rest body)
  "Run BODY against sequential RESPONSES, capturing REQUESTS and cleanup."
  (declare (indent 1))
  `(let ((replies ,responses) requests client-options closed)
     (cl-letf (((symbol-function 'acp-make-client)
                (lambda (&rest args) (setq client-options args) '((:fake . t))))
               ((symbol-function 'acp-shutdown)
                (lambda (&rest _) (setq closed t)))
               ((symbol-function 'agent-ide-history--request)
                (lambda (_client request)
                  (push request requests)
                  (let ((reply (pop replies)))
                    (if (functionp reply) (funcall reply) reply)))))
       ,@body)))

(ert-deftest agent-ide-history-lists-all-pages-and-deduplicates ()
  (agent-ide-history-test--with-backend
      '(((agentCapabilities . ((sessionCapabilities . ((list))))))
        ((sessions . [((sessionId . "a") (cwd . "/tmp"))]) (nextCursor . "opaque+/="))
        ((sessions . [((sessionId . "a") (cwd . "/tmp"))
                      ((sessionId . "b") (cwd . "/tmp"))])))
    (let ((entries (agent-ide-history-fetch '("agent" "acp") '("PROFILE=one") "/tmp/")))
      (should (equal (mapcar (lambda (e) (map-elt e 'sessionId)) entries) '("a" "b"))))
    (should closed)
    (should-not (buffer-live-p (plist-get client-options :context-buffer)))
    (should (equal (plist-get client-options :environment-variables) '("PROFILE=one")))
    (should (equal (mapcar (lambda (r) (map-elt r :method)) (reverse requests))
                   '("initialize" "session/list" "session/list")))
    (should (equal (agent-ide--get-in (car requests) '(:params cwd)) "/tmp"))
    (should (equal (agent-ide--get-in (car requests) '(:params cursor)) "opaque+/="))))

(ert-deftest agent-ide-history-all-projects-omits-cwd ()
  (agent-ide-history-test--with-backend
      '(((agentCapabilities . ((sessionCapabilities . ((list)))))) ((sessions . [])))
    (should-not (agent-ide-history-fetch '("agent") nil))
    (should (hash-table-p (map-elt (car requests) :params)))
    (should (zerop (hash-table-count (map-elt (car requests) :params))))
    (should closed)))

(ert-deftest agent-ide-history-unsupported-does-not-send-list ()
  (agent-ide-history-test--with-backend '(((agentCapabilities . ((loadSession . t)))))
    (should-error (agent-ide-history-fetch '("agent") nil) :type 'user-error)
    (should (= (length requests) 1))
    (should closed)))

(ert-deftest agent-ide-history-cleans-up-errors-and-quit ()
  (dolist (condition '(error quit))
    (agent-ide-history-test--with-backend
        (list (lambda () (signal condition '("interrupted"))))
      (should (eq (condition-case nil
                      (agent-ide-history-fetch '("agent") nil)
                    (error 'error) (quit 'quit))
                  condition))
      (should closed)
      (should-not (buffer-live-p (plist-get client-options :context-buffer))))))

(ert-deftest agent-ide-history-rejects-repeated-cursors-and-malformed-entries ()
  (dolist (pages '((((sessions . []) (nextCursor . "same"))
                    ((sessions . []) (nextCursor . "same")))
                   (((sessions . [((sessionId . "a") (cwd . "relative"))])))
                   (((nextCursor . "missing-sessions")))))
    (agent-ide-history-test--with-backend
        (cons '((agentCapabilities . ((sessionCapabilities . ((list)))))) pages)
      (should-error (agent-ide-history-fetch '("agent") nil) :type 'user-error)
      (should closed))))

(ert-deftest agent-ide-history-request-errors-timeouts-and-disconnects ()
  (let ((client '((:fake . t))) (request '((:method . "session/list"))))
    (cl-letf (((symbol-function 'acp-send-request)
               (lambda (&rest args)
                 (funcall (plist-get args :on-failure) '((message . "Not authenticated"))))))
      (should (string-match-p
               "Not authenticated"
               (error-message-string
                (should-error (agent-ide-history--request client request) :type 'user-error)))))
    (cl-letf (((symbol-function 'acp-send-request) #'ignore))
      (should (string-match-p
               "disconnected"
               (error-message-string
                (should-error (agent-ide-history--request client request) :type 'user-error))))
      (let ((agent-ide-history-request-timeout 0))
        (cl-letf (((symbol-function 'process-live-p) (lambda (_) t)))
          (should (string-match-p
                   "Timed out"
                   (error-message-string
                    (should-error (agent-ide-history--request client request) :type 'user-error)))))))))

(ert-deftest agent-ide-history-formats-optional-fields-and-sorts-dates ()
  (let* ((entries '(((sessionId . "missing") (cwd . "/tmp"))
                    ((sessionId . "old") (cwd . "/tmp") (updatedAt . "2026-01-01T00:00:00Z"))
                    ((sessionId . "new") (cwd . "/tmp") (title . "A\nB")
                     (updatedAt . "2026-10-22T00:00:00Z"))))
         (candidates (agent-ide-history-candidates entries)))
    (should (equal (mapcar (lambda (c) (map-elt (cdr c) 'sessionId)) candidates)
                   '("new" "old" "missing")))
    (should (string-prefix-p "A B |" (caar candidates)))
    (should (string-prefix-p "Untitled |" (car (car (last candidates)))))))

(ert-deftest agent-ide-history-picker-uses-source-backend-configuration ()
  (let ((source (agent-ide--make-session :directory default-directory))
        (agent-ide--session-metadata (make-hash-table :test 'eq))
        (agent-ide-command '("wrong-agent"))
        (agent-ide-environment '("WRONG=1"))
        (agent-ide-mcp-servers [])
        selected)
    (agent-ide--session-metadata-put source :command '("original-agent"))
    (agent-ide--session-metadata-put source :environment '("PROFILE=original"))
    (agent-ide--session-metadata-put source :mcp-servers [((name . "original"))])
    (cl-letf (((symbol-function 'agent-ide-history-fetch)
               (lambda (command environment directory)
                 (should (equal command '("original-agent")))
                 (should (equal environment '("PROFILE=original")))
                 (should-not directory)
                 (list `((sessionId . "saved") (cwd . ,default-directory)))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _) (caar collection)))
              ((symbol-function 'agent-ide--resume-entry)
               (lambda (entry) (setq selected entry))))
      (agent-ide-resume-history t nil source))
    (should (equal (map-elt selected 'command) '("original-agent")))
    (should (equal (map-elt selected 'environment) '("PROFILE=original")))
    (should (equal (map-elt selected 'mcpServers) [((name . "original"))]))))

(ert-deftest agent-ide-history-selection-reuses-open-buffer-with-normalized-cwd ()
  (let* ((directory (file-name-as-directory default-directory))
         (session (agent-ide--make-session :buffer (current-buffer) :directory directory
                                          :acp-session-id "saved" :status "running"
                                          :client '((:process . fake))))
         (agent-ide--sessions (list session))
         (agent-ide--session-metadata (make-hash-table :test 'eq))
         displayed)
    (agent-ide--session-metadata-put session :command '("original"))
    (cl-letf (((symbol-function 'agent-ide--cleanup-dead-sessions) #'ignore)
              ((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'agent-ide--display-buffer) (lambda (b) (setq displayed b)))
              ((symbol-function 'agent-ide--create-session)
               (lambda (&rest _) (ert-fail "Must reuse existing session"))))
      (should (eq session (agent-ide--resume-entry
                           `((sessionId . "saved") (cwd . ,(directory-file-name directory))
                             (command . ("original")))))))
    (should (eq displayed (current-buffer)))
    (should (equal (agent-ide-session-status session) "running"))))

(provide 'agent-ide-history-test)
;;; agent-ide-history-test.el ends here
