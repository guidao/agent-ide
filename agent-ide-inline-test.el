;;; agent-ide-inline-test.el --- Tests for agent-ide-inline -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-ide-session)

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
