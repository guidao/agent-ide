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

(ert-deftest agent-ide-enlearn-detects-chinese ()
  (should (eq (agent-ide-enlearn--mode-for-text "帮我修这个 bug") 'translate))
  (should (eq (agent-ide-enlearn--mode-for-text "Please fix this bug") 'polish)))

(ert-deftest agent-ide-enlearn-parses-markdown-sections ()
  (let* ((raw "## Final\nFix the nil check in foo.el\n\n## Breakdown\n- 祈使句\n\n## Grammar\n- nil check 搭配\n")
         (parsed (agent-ide-enlearn--parse-response raw)))
    (should (equal (plist-get parsed :final) "Fix the nil check in foo.el"))
    (should (string-match-p "祈使句" (plist-get parsed :breakdown)))
    (should (string-match-p "nil check" (plist-get parsed :grammar)))))

(ert-deftest agent-ide-enlearn-parse-failure-returns-nil-final ()
  (let ((parsed (agent-ide-enlearn--parse-response "sorry I cannot")))
    (should (null (plist-get parsed :final)))))

(provide 'agent-ide-enlearn-test)
