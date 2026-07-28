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

(provide 'agent-ide-submit-test)
