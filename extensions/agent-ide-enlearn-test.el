;;; agent-ide-enlearn-test.el --- Tests for enlearn -*- lexical-binding: t; -*-

(require 'ert)

;; Stub deps if required by agent-ide-enlearn → agent-ide-session chain:
(unless (featurep 'acp)
  (provide 'acp))
(unless (featurep 'valign)
  (provide 'valign)
  (defun valign-mode (&rest _))
  (defun valign--guess-table-type (&rest _)))

(require 'agent-ide-enlearn)
(require 'agent-ide-submit-test)

(ert-deftest agent-ide-enlearn-detects-chinese ()
  (should (eq (agent-ide-enlearn--mode-for-text "帮我修这个 bug") 'translate))
  (should (eq (agent-ide-enlearn--mode-for-text "Please fix this bug") 'polish)))

(ert-deftest agent-ide-enlearn-parses-markdown-sections ()
  (let* ((raw "## Final\nFix the nil check in foo.el\n\n## Breakdown\n- 祈使句\n\n## Grammar\n- nil check 搭配\n")
         (parsed (agent-ide-enlearn--parse-response raw)))
    (should (equal (plist-get parsed :final) "Fix the nil check in foo.el"))
    (should (string-match-p "祈使句" (plist-get parsed :breakdown)))
    (should (string-match-p "nil check" (plist-get parsed :grammar)))
    (should (equal (plist-get parsed :raw) raw))))

(ert-deftest agent-ide-enlearn-tolerates-heading-trailing-whitespace ()
  (should (equal (agent-ide-enlearn--section "## Final  \nFix" "Final") "Fix"))
  (should (equal (agent-ide-enlearn--section "## Final\t\nFix" "Final") "Fix")))

(ert-deftest agent-ide-enlearn-empty-final-returns-nil ()
  (let ((parsed (agent-ide-enlearn--parse-response "## Final\n\n## Breakdown\n- foo")))
    (should (null (plist-get parsed :final)))
    (should (string-match-p "foo" (plist-get parsed :breakdown)))))

(ert-deftest agent-ide-enlearn-whitespace-only-final-returns-nil ()
  (let ((parsed (agent-ide-enlearn--parse-response "## Final\n   \n## Breakdown\n- foo")))
    (should (null (plist-get parsed :final)))))

(ert-deftest agent-ide-enlearn-parse-failure-returns-nil-final ()
  (let ((parsed (agent-ide-enlearn--parse-response "sorry I cannot")))
    (should (null (plist-get parsed :final)))
    (should (equal (plist-get parsed :breakdown) ""))
    (should (equal (plist-get parsed :grammar) ""))))

(ert-deftest agent-ide-enlearn-hook-defers-when-enabled ()
  (with-temp-buffer
    (agent-ide-session-mode)
    (let* ((session (agent-ide-submit-test--session))
           (sent 'unset)
           (agent-ide-enlearn-auto-send nil))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-create-prompt session)
      (goto-char (marker-position (agent-ide-session-input-start-marker session)))
      (insert "帮我写测试")
      (agent-ide-enlearn-mode 1)
      (cl-letf (((symbol-function 'agent-ide-protocol-send-prompt)
                 (lambda (_s p) (setq sent p)))
                ((symbol-function 'agent-ide-enlearn--request)
                 (lambda (&rest _) nil)))
        (agent-ide-submit)
        (should (eq sent 'unset))
        (should (string-match-p "帮我写测试" (buffer-string)))
        (should (equal (agent-ide-session-status session) "coaching")))
      (agent-ide-enlearn-mode -1))))

(provide 'agent-ide-enlearn-test)
