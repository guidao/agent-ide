;;; agent-ide-submit-test.el --- Tests for submit/deliver hooks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(unless (featurep 'acp)
  (provide 'acp))

;; Stub valign like agent-ide-sidebar-test.el if needed before requiring session:
(unless (featurep 'valign)
  (provide 'valign)
  (defun valign-mode (&rest _))
  (defun valign--guess-table-type (&rest _)))

(require 'agent-ide-session)

(defun agent-ide-submit-test--session ()
  (agent-ide--make-session
   :directory default-directory
   :buffer (current-buffer)
   :tool-calls (make-hash-table :test 'equal)
   :prompt-history nil
   :acp-session-id "test-session"))

(ert-deftest agent-ide-submit-delivers-when-no-hooks ()
  "With empty pre-submit hooks, submit freezes input and sends prompt."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let* ((session (agent-ide-submit-test--session))
           (sent nil)
           (agent-ide-pre-submit-functions nil))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-create-prompt session)
      (goto-char (marker-position (agent-ide-session-input-start-marker session)))
      (insert "hello agent")
      (cl-letf (((symbol-function 'agent-ide-protocol-send-prompt)
                 (lambda (_s prompt) (setq sent prompt))))
        (agent-ide-submit)
        (should (equal sent "hello agent"))
        (should (equal (car (agent-ide-session-prompt-history session))
                       "hello agent"))))))

(ert-deftest agent-ide-submit-defers-when-hook-returns-non-nil ()
  "Non-nil hook return prevents protocol send."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let* ((session (agent-ide-submit-test--session))
           (sent 'unset)
           (seen nil)
           (agent-ide-pre-submit-functions
            (list (lambda (s p)
                    (setq seen (list s p))
                    t))))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-create-prompt session)
      (goto-char (marker-position (agent-ide-session-input-start-marker session)))
      (insert "defer me")
      (cl-letf (((symbol-function 'agent-ide-protocol-send-prompt)
                 (lambda (_s prompt) (setq sent prompt))))
        (agent-ide-submit)
        (should (eq sent 'unset))
        (should (equal (nth 1 seen) "defer me"))))))

(ert-deftest agent-ide-deliver-prompt-sends-and-records-history ()
  "deliver-prompt replaces input, freezes, and sends."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let* ((session (agent-ide-submit-test--session))
           (sent nil))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-create-prompt session)
      (cl-letf (((symbol-function 'agent-ide-protocol-send-prompt)
                 (lambda (_s prompt) (setq sent prompt))))
        (agent-ide-deliver-prompt session "final english")
        (should (equal sent "final english"))
        (should (equal (car (agent-ide-session-prompt-history session))
                       "final english"))
        (should (string-match-p "final english" (buffer-string)))))))

(ert-deftest agent-ide-cancel-rejects-idle-and-non-prompt-requests ()
  (with-temp-buffer
    (let ((session (agent-ide-submit-test--session)))
      (cl-letf (((symbol-function 'acp-send-notification)
                 (lambda (&rest _) (ert-fail "Sent cancel without an active prompt"))))
        (dolist (status '("idle" "initializing" "creating-session" "resuming"
                          "disconnected" "failed" "running"))
          (setf (agent-ide-session-status session) status
                (agent-ide-session-active-requests session)
                '(((:method . "session/set_model"))))
          (should-error (agent-ide-protocol-cancel session) :type 'user-error)
          (should (equal status (agent-ide-session-status session))))))))

(ert-deftest agent-ide-cancel-pending-prompt-is-idempotent ()
  (with-temp-buffer
    (let ((session (agent-ide-submit-test--session)) (sent 0))
      (setf (agent-ide-session-status session) "running"
            (agent-ide-session-active-requests session) '(((:method . "session/prompt"))))
      (cl-letf (((symbol-function 'acp-send-notification)
                 (lambda (&rest _)
                   (should (equal "interrupting" (agent-ide-session-status session)))
                   (cl-incf sent))))
        (agent-ide-protocol-cancel session)
        (agent-ide-protocol-cancel session)
        (should (= sent 1))
        (should (equal "interrupting" (agent-ide-session-status session)))
        (should (agent-ide-session-active-requests session))))))

(ert-deftest agent-ide-cancel-preserves-completion-during-send ()
  (with-temp-buffer
    (let ((session (agent-ide-submit-test--session)))
      (setf (agent-ide-session-status session) "running"
            (agent-ide-session-active-requests session) '(((:method . "session/prompt"))))
      (cl-letf (((symbol-function 'acp-send-notification)
                 (lambda (&rest _)
                   (setf (agent-ide-session-active-requests session) nil)
                   (agent-ide--set-status session "idle"))))
        (agent-ide-protocol-cancel session)
        (should (equal "idle" (agent-ide-session-status session)))
        (agent-ide--assert-ready session)))))

(ert-deftest agent-ide-cancel-send-failure-does-not-stick-interrupting ()
  (dolist (disconnect '(nil t))
    (with-temp-buffer
      (let ((session (agent-ide-submit-test--session)))
        (setf (agent-ide-session-status session) "running"
              (agent-ide-session-active-requests session) '(((:method . "session/prompt"))))
        (cl-letf (((symbol-function 'acp-send-notification)
                   (lambda (&rest _)
                     (when disconnect (agent-ide--set-status session "disconnected"))
                     (error "Send failed"))))
          (should-error (agent-ide-protocol-cancel session))
          (should (equal (if disconnect "disconnected" "running")
                         (agent-ide-session-status session))))))))

(provide 'agent-ide-submit-test)
