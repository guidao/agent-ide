;;; agent-ide-session-mode-test.el --- Tests for Agent IDE session mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for transcript edit protection.

;;; Code:

(require 'ert)
(require 'agent-ide-session-mode)

(unless (featurep 'acp)
  (provide 'acp))
(require 'agent-ide-transcript)

(defun agent-ide-session-mode-test--session ()
  "Return a test session backed by the current buffer."
  (agent-ide--make-session
   :directory default-directory
   :buffer (current-buffer)
   :tool-calls (make-hash-table :test 'equal)))

(ert-deftest agent-ide-session-mode-protects-transcript-output ()
  "Manual text can only be inserted into the active input."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-create-prompt session)
      (agent-ide-renderer-append-stream-chunk session 'message "assistant output")
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (search-forward "assistant output"))
      (should-error (insert "x") :type 'buffer-read-only)
      (goto-char (marker-position (agent-ide-session-input-start-marker session)))
      (insert "prompt")
      (should (equal (agent-ide-renderer-current-input session) "prompt")))))

(ert-deftest agent-ide-session-mode-protects-output-boundary ()
  "Manual text cannot be inserted after output but before the prompt."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-create-prompt session t)
      (agent-ide-renderer-append-stream-chunk session 'message "assistant output")
      (let ((boundary (agent-ide--session-metadata-get
                       session :active-input-boundary-marker)))
        (goto-char (marker-position boundary))
        (should-error (insert "x") :type 'buffer-read-only)))))

(ert-deftest agent-ide-session-mode-input-overlay-covers-input-box ()
  "The active input overlay covers prompt padding through point-max."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-create-prompt session)
      (let ((overlay (agent-ide-session-input-overlay session))
            (prompt-start (agent-ide-session-input-prompt-start-marker session))
            (input-start (agent-ide-session-input-start-marker session)))
        (should (overlayp overlay))
        (should (= (overlay-start overlay) (marker-position input-start)))
        (should (= (overlay-end overlay) (point-max)))
        (should (equal (buffer-substring-no-properties
                        (- (point-max) 2)
                        (point-max))
                       "\n\n"))
        (goto-char (marker-position prompt-start))
        (should-error (insert "x") :type 'buffer-read-only)
        (goto-char (marker-position input-start))
        (insert "prompt")
        (should (equal (agent-ide-renderer-current-input session) "prompt"))))))

(ert-deftest agent-ide-session-mode-follow-input-does-not-recenter ()
  "Following the input should not force the prompt to the window bottom."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-create-prompt session)
      (cl-letf (((symbol-function 'recenter)
                 (lambda (&rest _args)
                   (error "recenter should not be called"))))
        (agent-ide-renderer-follow-input session)))))

(ert-deftest agent-ide-session-mode-clamps-point-in-input-padding ()
  "Point in active input padding is clamped back to the editable input end."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-create-prompt session)
      (let ((input-end (marker-position
                        (agent-ide-session-input-end-marker session))))
        (goto-char (1+ input-end))
        (agent-ide-session-mode-sync-prompt-minor-mode)
        (should (= (point) input-end))))))

(ert-deftest agent-ide-renderer-header-shows-model-and-usage ()
  "Header summary includes known model and token usage information."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setf (agent-ide-session-status session) "idle")
      (setf (agent-ide-session-models session)
            [((id . "small") (name . "Small"))
             ((id . "large") (name . "Large") (isDefault . t))])
      (setf (agent-ide-session-usage session)
            '((total . ((totalTokens . 12345)))
              (last . ((inputTokens . 1000)
                       (outputTokens . 234)))
              (modelContextWindow . 200000)))
      (let ((summary (agent-ide-renderer--header-summary session)))
        (should (string-match-p "Large" summary))
        (should (string-match-p "12\\.3k/200\\.0k tokens" summary))))))

(ert-deftest agent-ide-renderer-header-shows-acp-usage-update ()
  "Header summary understands ACP usage_update used/size fields."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setf (agent-ide-session-usage session)
            '((sessionUpdate . "usage_update")
              (used . 53000)
              (size . 200000)))
      (let ((summary (agent-ide-renderer--header-summary session)))
        (should (string-match-p "53\\.0k/200\\.0k tokens" summary))))))

(ert-deftest agent-ide-renderer-header-handles-available-models ()
  "Header summary uses currentModelId from agent ACP models responses."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setf (agent-ide-session-status session) "idle")
      (setf (agent-ide-session-models session)
            '((currentModelId . "sonnet")
              (availableModels .
               [((modelId . "default")
                 (name . "Default (recommended)"))
                ((modelId . "sonnet")
                 (name . "Sonnet"))])))
      (should (string-match-p
               "Model: Sonnet"
               (agent-ide-renderer--header-summary session))))))

(ert-deftest agent-ide-renderer-header-falls-back-to-current-model-id ()
  "Header summary displays currentModelId when the model list has no match."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setf (agent-ide-session-status session) "idle")
      (setf (agent-ide-session-models session)
            '((currentModelId . "gpt-5.4")
              (availableModels .
               [((modelId . "sonnet")
                 (name . "Sonnet"))])))
      (should (string-match-p
               "Model: gpt-5.4"
               (agent-ide-renderer--header-summary session))))))

(ert-deftest agent-ide-renderer-header-puts-agent-icon-before-model ()
  "The Agent IDE icon appears immediately before the model label."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setf (agent-ide-session-models session)
            [((id . "large") (name . "Large") (isDefault . t))])
      (cl-letf (((symbol-function 'agent-ide-renderer--header-icon-string)
                 (lambda () "AGENT-ICON")))
        (should (string-prefix-p
                 "AGENT-ICON Large"
                 (agent-ide-renderer--header-line-content session)))))))

(ert-deftest agent-ide-renderer-header-icon-advances-for-live-sessions ()
  "The shared animation advances and refreshes live session headers."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let* ((session (agent-ide-session-mode-test--session))
           (agent-ide--sessions (list session))
           (agent-ide-renderer--header-icon-frame 0)
           (agent-ide-renderer--header-icon-timer nil))
      (setq-local agent-ide--session session)
      (cl-letf (((symbol-function 'agent-ide-renderer--header-icon-string)
                 (lambda ()
                   (format "FRAME-%d"
                           agent-ide-renderer--header-icon-frame)))
                ((symbol-function 'agent-ide-renderer--header-summary)
                 (lambda (_session) "Model")))
        (agent-ide-renderer--advance-header-icon)
        (should (= agent-ide-renderer--header-icon-frame 1))
        (should (string-match-p
                 "FRAME-1 Model"
                 (substring-no-properties header-line-format)))))))

(ert-deftest agent-ide-renderer-thought-uses-codex-heading ()
  "Thought blocks use Codex-style headings instead of disclosure prefixes."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-append-stream-chunk session 'thought "reasoning")
      (goto-char (point-min))
      (should (search-forward "* Thinking" nil t))
      (should-not (search-forward "▸ Thinking" nil t))
      (let ((button (button-at (line-beginning-position))))
        (should button)
        (should (button-get button 'agent-ide-codex-style))
        (agent-ide-renderer-toggle-fold-at-point button)
        (should (equal (button-get button 'display) "* Thinking"))))))

(ert-deftest agent-ide-renderer-message-renders-markdown ()
  "Assistant messages render common Markdown markup."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-append-stream-chunk
       session 'message
       "Use `foo` and **bold** in [file](/tmp/foo.el#L3C2).")
      (goto-char (point-min))
      (search-forward "foo")
      (should (get-text-property (1- (point)) 'agent-ide-markdown))
      (should (eq (get-text-property (1- (point)) 'face)
                  'font-lock-keyword-face))
      (search-forward "bold")
      (should (memq 'bold (ensure-list (get-text-property (1- (point))
                                                          'face))))
      (search-forward "file")
      (let ((button (button-at (1- (point)))))
        (should button)
        (should (equal (button-get button 'agent-ide-url)
                       "/tmp/foo.el#L3C2"))))))

(ert-deftest agent-ide-renderer-message-renders-bare-urls ()
  "Assistant messages turn bare URLs into followable buttons."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-append-stream-chunk
       session 'message
       "See https://example.com/path for details.")
      (goto-char (point-min))
      (search-forward "https://example.com/path")
      (let ((button (button-at (1- (point)))))
        (should button)
        (should (equal (button-get button 'agent-ide-url)
                       "https://example.com/path"))))))

(ert-deftest agent-ide-follow-thing-at-point-opens-markdown-link ()
  "C-c C-o opens Markdown link URLs at point."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let* ((session (agent-ide-session-mode-test--session))
           (opened nil))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-append-stream-chunk
       session 'message
       "[docs](https://example.com/docs)")
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _) (setq opened url))))
        (goto-char (point-min))
        (search-forward "docs")
        (backward-char)
        (agent-ide-follow-thing-at-point)
        (should (equal opened "https://example.com/docs"))))))

(ert-deftest agent-ide-follow-thing-at-point-opens-bare-url ()
  "C-c C-o opens bare URLs at point."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let* ((session (agent-ide-session-mode-test--session))
           (opened nil))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-append-stream-chunk
       session 'message
       "Visit https://example.com/bare please.")
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _) (setq opened url))))
        (goto-char (point-min))
        (search-forward "https://example.com/bare")
        (backward-char)
        (agent-ide-follow-thing-at-point)
        (should (equal opened "https://example.com/bare"))))))

(ert-deftest agent-ide-follow-thing-at-point-opens-file-path ()
  "C-c C-o opens local file paths from Markdown links."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let* ((session (agent-ide-session-mode-test--session))
           (opened nil)
           (tmp (make-temp-file "agent-ide-follow-")))
      (unwind-protect
          (progn
            (setq-local agent-ide--session session)
            (agent-ide-renderer-initialize-buffer session)
            (agent-ide-renderer-append-stream-chunk
             session 'message
             (format "[file](%s)" tmp))
            (cl-letf (((symbol-function 'find-file)
                       (lambda (file &rest _) (setq opened file))))
              (goto-char (point-min))
              (search-forward "file")
              (backward-char)
              (agent-ide-follow-thing-at-point)
              (should (equal opened tmp))))
        (delete-file tmp)))))

(ert-deftest agent-ide-renderer-message-renders-indented-code-fence ()
  "Assistant messages render fenced code blocks after leading whitespace."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-append-stream-chunk
       session 'message
       "    ```elisp\n    (defun cursor-test ()\n      :ok)\n    ```")
      (agent-ide-renderer-finish-stream session)
      (goto-char (point-min))
      (search-forward "```elisp")
      (should (equal (get-text-property (line-beginning-position) 'display)
                     ""))
      (search-forward "defun")
      (should (get-text-property (1- (point)) 'agent-ide-markdown-code-content))
      (should (get-text-property (1- (point)) 'face))
      (search-forward "```")
      (should (equal (get-text-property (line-beginning-position) 'display)
                     "")))))

(ert-deftest agent-ide-renderer-message-renders-markdown-table ()
  "Assistant messages align Markdown pipe tables."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-append-stream-chunk
       session 'message
       "| Name | Age |\n|---|---:|\n| Bob | 3 |")
      (agent-ide-renderer-finish-stream session)
      (goto-char (point-min))
      (should (search-forward "| Name | Age  |" nil t))
      (should (search-forward "| Bob  | 3    |" nil t))
      (should (get-text-property (line-beginning-position)
                                 'agent-ide-markdown)))))

(ert-deftest agent-ide-renderer-defers-streaming-markdown-table-rewrite ()
  "Assistant message streaming does not rewrite pipe tables before completion."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-append-stream-chunk
       session 'message
       "| Name | Age |\n|---|---:|\n")
      (goto-char (point-min))
      (should (search-forward "|---|---:|" nil t))
      (should-not (search-forward "| ---" nil t))
      (agent-ide-renderer-append-stream-chunk
       session 'message
       "| Bob | 3 |")
      (agent-ide-renderer-finish-stream session)
      (goto-char (point-min))
      (should (search-forward "| Name | Age  |" nil t))
      (should (search-forward "| Bob  | 3    |" nil t)))))

(ert-deftest agent-ide-renderer-animates-running-placeholder ()
  "Running placeholders cycle their trailing ellipsis frame."
  (with-temp-buffer
    (let ((session (agent-ide-session-mode-test--session)))
      (setf (agent-ide-session-status session) "running")
      (agent-ide--session-metadata-put session :input-placeholder-ellipsis-frame 0)
      (should (equal (agent-ide-renderer--placeholder-animated-text
                      session "Working...")
                     "Working."))
      (agent-ide--session-metadata-put session :input-placeholder-ellipsis-frame 1)
      (should (equal (agent-ide-renderer--placeholder-animated-text
                      session "Working...")
                     "Working.."))
      (agent-ide--session-metadata-put session :input-placeholder-ellipsis-frame 2)
      (should (equal (agent-ide-renderer--placeholder-animated-text
                      session "Working...")
                     "Working..."))
      (agent-ide--session-metadata-put session :input-placeholder-ellipsis-frame 3)
      (should (equal (agent-ide-renderer--placeholder-animated-text
                      session "Working...")
                     "Working")))))

(ert-deftest agent-ide-renderer-does-not-animate-idle-placeholder ()
  "Idle placeholders keep their original text."
  (with-temp-buffer
    (let ((session (agent-ide-session-mode-test--session)))
      (setf (agent-ide-session-status session) "idle")
      (agent-ide--session-metadata-put session :input-placeholder-ellipsis-frame 0)
      (should (equal (agent-ide-renderer--placeholder-animated-text
                      session "Tell Agent what to do...")
                     "Tell Agent what to do...")))))

(ert-deftest agent-ide-session-mode-protects-frozen-input ()
  "Submitted prompts become read-only after a new prompt is created."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-create-prompt session)
      (goto-char (marker-position (agent-ide-session-input-start-marker session)))
      (insert "old")
      (let ((old-input-start (copy-marker
                              (agent-ide-session-input-start-marker session))))
        (agent-ide-renderer-freeze-current-input session)
        (agent-ide-renderer-create-prompt session t)
        (goto-char old-input-start)
        (should-error (insert "x") :type 'buffer-read-only)
        (set-marker old-input-start nil))
      (goto-char (marker-position (agent-ide-session-input-start-marker session)))
      (insert "new")
      (should (equal (agent-ide-renderer-current-input session) "new")))))

(ert-deftest agent-ide-session-mode-completes-available-slash-commands ()
  "Slash command completion uses the latest available commands update."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-create-prompt session)
      (agent-ide--session-metadata-put
       session :available-commands
       [((name . "fixer") (description . "故障恢复建议"))
        ((name . "iaas") (description . "查询 k8s 资源信息"))])
      (goto-char (marker-position (agent-ide-session-input-start-marker session)))
      (insert "/fi")
      (let ((completion (agent-ide-session-mode-completion-at-point)))
        (should (= (nth 0 completion)
                   (marker-position
                    (agent-ide-session-input-start-marker session))))
        (should (= (nth 1 completion) (point)))
        (should (member "/fixer" (nth 2 completion)))
        (should (member "/iaas" (nth 2 completion)))))))

(ert-deftest agent-ide-transcript-available-commands-defaults-collapsed ()
  "Available commands updates render collapsed by default."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-transcript-handle-notification
       session
       '((method . "session/update")
         (params . ((update . ((sessionUpdate . "available_commands_update")
                               (availableCommands . [((name . "fixer"))])))))))
      (goto-char (point-min))
      (should (search-forward "* Available commands" nil t))
      (let ((overlay (get-text-property (line-beginning-position)
                                        'agent-ide-fold-overlay)))
        (should overlay)
        (should (overlay-get overlay 'invisible))))))

(ert-deftest agent-ide-session-mode-truncates-slash-command-descriptions ()
  "Slash command annotations keep long descriptions compact."
  (let ((agent-ide-session-command-description-max-width 10))
    (should (equal (agent-ide-session-mode--truncate-command-description
                    "0123456789abcdef")
                   "0123456...")))
  (let ((agent-ide-session-command-description-max-width 20))
    (should (equal (agent-ide-session-mode--truncate-command-description
                    "foo\nbar\tbaz")
                   "foo bar baz"))))

(ert-deftest agent-ide-renderer-expands-tool-diff-output ()
  "Tool diff summaries attach expanded highlighted diff output."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-update-tool
       session
       "tool-diff"
       "Edit a.txt"
       "  └ diff: a.txt (+1/-1, 1 line) [expand] [open diff]"
       nil
       nil
       "--- a/a.txt\n+++ b/a.txt\n@@ -1,1 +1,1 @@\n-old\n+new")
      (goto-char (point-min))
      (search-forward "[expand]")
      (let ((button (button-at (match-beginning 0))))
        (should button)
        (should (equal (button-get button 'display) "[collapse]")))
      (should (search-forward "[open diff]" nil t))
      (should (button-at (match-beginning 0)))
      (should (search-forward "+new" nil t))
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'agent-ide-diff-added-face))
      (should-not (search-forward "+new" nil t)))))

(ert-deftest agent-ide-transcript-normalizes-diff-text-like-codex-ide ()
  "Diff summaries prefer provided diff text and wrap headerless patches."
  (let* ((diff '((path . "a.txt")
                 (diff . "@@ -1 +1 @@\n-old\n+new")))
         (text (agent-ide-transcript--diff-text diff))
         (stats (agent-ide-transcript--diff-text-stats text)))
    (should (string-prefix-p "diff --git a/a.txt b/a.txt" text))
    (should (string-match-p "\\+new" text))
    (should (equal (plist-get stats :filename) "a.txt"))
    (should (= (plist-get stats :added) 1))
    (should (= (plist-get stats :removed) 1))))

(ert-deftest agent-ide-transcript-computes-diff-from-old-and-new-text ()
  "Diff summaries compute unified diff from oldText/newText when patch is absent."
  (let* ((diff '((path . "a.txt")
                 (oldText . "line1\nline2\nline3")
                 (newText . "line1\nchanged\nline3")))
         (text (agent-ide-transcript--diff-text diff))
         (stats (agent-ide-transcript--diff-text-stats text)))
    (should (string-match-p "--- a/a.txt" text))
    (should (string-match-p "\\+\\+\\+ b/a.txt" text))
    (should (string-match-p "-line2" text))
    (should (string-match-p "\\+changed" text))
    (should-not (string-match-p "-line1" text))
    (should-not (string-match-p "-line3" text))
    (should (= (plist-get stats :added) 1))
    (should (= (plist-get stats :removed) 1))))

(ert-deftest agent-ide-transcript-uses-inner-diffs-for-nested-changes ()
  "Nested change containers do not duplicate combined diff display."
  (let* ((inner '((path . "a.txt")
                  (diff . "@@ -1 +1 @@\n-old\n+new")))
         (outer `((type . "diff")
                  (path . "patch")
                  (changes . [,inner])))
         (diffs (agent-ide-transcript--top-level-diffs (list outer inner))))
    (should (equal diffs (list inner)))))

(ert-deftest agent-ide-renderer-replaces-auto-expanded-tool-diff ()
  "Repeated tool updates replace existing auto-expanded diff output."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session))
          (body "  └ diff: a.txt (+1/-1, 1 line) [expand] [open diff]")
          (diff "--- a/a.txt\n+++ b/a.txt\n@@ -1,1 +1,1 @@\n-old\n+new"))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (dotimes (_ 2)
        (agent-ide-renderer-update-tool
         session "tool-diff" "Edit a.txt" body nil nil diff))
      (goto-char (point-min))
      (let ((count 0))
        (while (search-forward "+new" nil t)
          (setq count (1+ count)))
        (should (= count 1))))))

(ert-deftest agent-ide-transcript-empty-tool-update-keeps-diff-summary ()
  "Empty tool updates do not replace an existing diff summary with fallback text."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-transcript--render-tool
       session
       '((sessionUpdate . "tool_call")
         (toolCallId . "edit-1")
         (content . [((path . "a.txt")
                      (diff . "@@ -1 +1 @@\n-old\n+new"))])))
      (agent-ide-transcript--render-tool-update
       session
       '((sessionUpdate . "tool_call_update")
         (toolCallId . "edit-1")
         (status . "completed")))
      (goto-char (point-min))
      (should (search-forward "* Prepared 1 file change" nil t))
      (should (search-forward "└ diff: a.txt" nil t))
      (should-not (search-forward "└ Tool call" nil t)))))

(ert-deftest agent-ide-transcript-detail-update-keeps-existing-summary ()
  "Detail-only tool updates add expansion without replacing the existing summary."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-transcript--render-tool
       session
       '((sessionUpdate . "tool_call")
         (toolCallId . "search-1")
         (title . "Web search")
         (rawInput . ((query . "北京天气")))))
      (agent-ide-transcript--render-tool-update
       session
       '((sessionUpdate . "tool_call_update")
         (toolCallId . "search-1")
         (content . "Web search results")))
      (goto-char (point-min))
      (should (search-forward "* Web search" nil t))
      (should (search-forward "└ Web search [expand]" nil t))
      (should-not (search-forward "└ Tool call" nil t)))))

(ert-deftest agent-ide-renderer-permission-uses-codex-style ()
  "Permission requests render as Codex-style approval blocks."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session))
          selected)
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-insert-permission
       session
       "permission-1"
       "echo hi"
       "Command: echo hi\n\nDescription: Say hi"
       [((optionId . "allow") (name . "Allow"))
        ((optionId . "reject") (name . "Reject"))]
       (lambda (option-id)
         (setq selected option-id)))
      (goto-char (point-min))
      (should (search-forward "[Approval required]" nil t))
      (should (search-forward "Run the following command?" nil t))
      (should (search-forward "    echo hi" nil t))
      (should (search-forward "[accept]" nil t))
      (let ((button (button-at (match-beginning 0))))
        (should button)
        (push-button button))
      (should (equal selected "allow"))
      (should (search-forward "[decline]" nil t))
      (should (search-forward "[cancel]" nil t)))))

(ert-deftest agent-ide-session-permission-shortcuts-are-bound ()
  "Permission actions have cursor-independent session key bindings."
  (should (eq (lookup-key agent-ide-session-mode-map (kbd "C-c C-a"))
              #'agent-ide-approve-permission))
  (should (eq (lookup-key agent-ide-session-mode-map (kbd "C-c C-d"))
              #'agent-ide-decline-permission))
  (should (eq (lookup-key agent-ide-session-mode-map (kbd "C-c C-p"))
              #'agent-ide-select-permission-option)))

(ert-deftest agent-ide-session-approves-pending-permission-without-moving-point ()
  "The approve command chooses allow-once and preserves point."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session))
          selected)
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (let ((inhibit-message t)
            (message-log-max nil))
        (agent-ide-renderer-insert-permission
         session "permission-1" "echo hi" "Command: echo hi"
         [((kind . "allow_always")
           (optionId . "always")
           (name . "Always allow"))
          ((kind . "allow_once")
           (optionId . "once")
           (name . "Allow"))
          ((kind . "reject_once")
           (optionId . "reject")
           (name . "Reject"))]
         (lambda (option-id) (setq selected option-id))))
      (goto-char (point-min))
      (let ((original-point (point)))
        (agent-ide-approve-permission)
        (should (= (point) original-point)))
      (should (equal selected "once"))
      (should-not (agent-ide-renderer-pending-permission session)))))

(ert-deftest agent-ide-session-prefix-approves-permission-always ()
  "A prefix argument chooses the always-allow option."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session))
          selected)
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (let ((inhibit-message t)
            (message-log-max nil))
        (agent-ide-renderer-insert-permission
         session "permission-1" "echo hi" "Command: echo hi"
         [((optionId . "allow") (name . "Allow"))
          ((optionId . "alwaysAllow") (name . "Always Allow"))]
         (lambda (option-id) (setq selected option-id))))
      (let ((current-prefix-arg '(4)))
        (call-interactively #'agent-ide-approve-permission))
      (should (equal selected "alwaysAllow")))))

(ert-deftest agent-ide-session-declines-newest-pending-permission ()
  "Decline acts on the newest request when more than one is pending."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session))
          first-selected
          second-selected)
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (let ((inhibit-message t)
            (message-log-max nil))
        (agent-ide-renderer-insert-permission
         session "permission-1" "first" "Command: first"
         [((kind . "reject_once")
           (optionId . "first-reject")
           (name . "Reject"))]
         (lambda (option-id) (setq first-selected option-id)))
        (agent-ide-renderer-insert-permission
         session "permission-2" "second" "Command: second"
         [((kind . "reject_once")
           (optionId . "second-reject")
           (name . "Reject"))]
         (lambda (option-id) (setq second-selected option-id))))
      (agent-ide-decline-permission)
      (should-not first-selected)
      (should (equal second-selected "second-reject")))))

(ert-deftest agent-ide-session-permission-picker-can-cancel ()
  "The permission option picker includes cancellation."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session))
          (selected 'not-called))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (let ((inhibit-message t)
            (message-log-max nil))
        (agent-ide-renderer-insert-permission
         session "permission-1" "echo hi" "Command: echo hi"
         [((kind . "allow_once")
           (optionId . "allow")
           (name . "Allow"))]
         (lambda (option-id) (setq selected option-id))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _args) "Cancel")))
        (agent-ide-select-permission-option))
      (should-not selected))))

(ert-deftest agent-ide-transcript-web-search-uses-codex-style ()
  "Web search tools use the generic compact tool summary style."
  (let ((display
         (agent-ide-transcript--codex-tool-display
          '((kind . "web_search")
            (rawInput . ((query . "北京天气 2026年5月24日")))
            (content . "Web search results")))))
    (should (equal (plist-get display :title) "web_search"))
    (should (equal (plist-get display :body)
                   "  └ 北京天气 2026年5月24日 [expand]"))))

(ert-deftest agent-ide-transcript-web-search-result-keeps-codex-style ()
  "Completed web search result text stays in expanded output."
  (let ((display
         (agent-ide-transcript--codex-tool-display
          '((rawInput . ((query . "成都天气 2026年5月24日")))
            (content . "Web search results for query: \"成都天气 2026年5月24日\"")))))
    (should (equal (plist-get display :title) "成都天气 2026年5月24日"))
    (should (equal (plist-get display :body)
                   "  └ 成都天气 2026年5月24日 [expand]"))
    (should (equal (plist-get display :expanded-output)
                   "Web search results for query: \"成都天气 2026年5月24日\""))))

(ert-deftest agent-ide-transcript-renders-web-search-title ()
  "Web search tool calls use the same compact tool display as other tools."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-transcript--render-tool
       session
       '((sessionUpdate . "tool_call")
         (toolCallId . "search-1")
         (kind . "web_search")
         (title . "北京天气 2026年5月24日")
         (rawInput . ((query . "北京天气 2026年5月24日")))))
      (goto-char (point-min))
      (should (search-forward "* 北京天气 2026年5月24日" nil t))
      (should (search-forward "└ 北京天气 2026年5月24日" nil t)))))

(ert-deftest agent-ide-renderer-does-not-guess-web-search-title ()
  "Renderer preserves non-search titles even when title matches a body line."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-update-tool
       session
       "edit-1"
       "Edit"
       "  └ Edit\n  └ diff: a.txt (+1/-1, 1 line)")
      (goto-char (point-min))
      (should (search-forward "* Edit" nil t))
      (should-not (search-forward "* Searched the web" nil t)))))

(ert-deftest agent-ide-transcript-tool-update-keeps-title-and-updates-body ()
  "Tool updates keep the initial title heading while replacing argument/output body."
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide-session-mode-test--session)))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-transcript-handle-notification
       session
       '((method . "session/update")
         (params . ((update . ((sessionUpdate . "tool_call")
                               (toolCallId . "tool-1")
                               (name . "exec_command")
                               (title . "initial args")))))))
      (agent-ide-transcript-handle-notification
       session
       '((method . "session/update")
         (params . ((update . ((sessionUpdate . "tool_call_update")
                               (toolCallId . "tool-1")
                               (title . "ls -la")
                               (content . "total 8")))))))
      (goto-char (point-min))
      (should (search-forward "* initial args" nil t))
      (should (search-forward "└ ls -la [expand]" nil t))
      (should-not (search-forward "total 8" nil t))
      (should-not (search-forward "* ls -la" nil t)))))

(ert-deftest agent-ide-transcript-tool-title-prefers-title-over-kind ()
  "Tool call titles prefer ACP title over generic kind labels."
  (should (equal (agent-ide-transcript--tool-title
                  '((kind . "execute")
                    (title . "Run shell command")))
                 "Run shell command")))

(ert-deftest agent-ide-transcript-tool-update-without-id-uses-fallback-key ()
  "Tool updates without an ACP id still get a non-nil renderer key."
  (let ((tool '((sessionUpdate . "tool_call_update")
                (title . "args")
                (content . "output"))))
    (should (string-prefix-p "tool-" (agent-ide-transcript--tool-id tool)))))

(provide 'agent-ide-session-mode-test)

;;; agent-ide-session-mode-test.el ends here
