;;; agent-ide-stream-test.el --- Streaming performance regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-ide)

(ert-deftest agent-ide-stream-batches-bursts-and-flushes-final-text ()
  (with-temp-buffer
    (let* ((calls nil)
           (agent-ide-stream-render-interval 60)
           (state (agent-ide-stream-create
                   (point-min)
                   (lambda (start end)
                     (push (buffer-substring-no-properties start end) calls)))))
      (dotimes (_ 100)
        (goto-char (point-max))
        (insert "x")
        (agent-ide-stream-queue state (point-max)))
      (should (equal calls '("x")))
      (let ((timer (agent-ide-stream-timer state)))
        (should (memq timer timer-list))
        (agent-ide-stream-flush state t)
        (should-not (memq timer timer-list)))
      (should (equal calls (list (make-string 100 ?x) "x")))
      (agent-ide-stream-flush state t)
      (should (= (length calls) 2)))))

(ert-deftest agent-ide-stream-timer-renders-without-another-chunk ()
  (with-temp-buffer
    (let* ((calls 0)
           (agent-ide-stream-render-interval 0.01)
           (state (agent-ide-stream-create 1 (lambda (&rest _) (cl-incf calls)))))
      (insert "a")
      (agent-ide-stream-queue state (point-max))
      (insert "b")
      (agent-ide-stream-queue state (point-max))
      (let ((deadline (+ (float-time) 2)))
        (while (and (< calls 2) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (= calls 2))
      (should-not (agent-ide-stream-timer state)))))

(ert-deftest agent-ide-stream-bounds-work-for-long-unbroken-responses ()
  ;; Force every refresh: performance must not depend solely on fast bursts.
  (dolist (count '(100 200 400 800))
    (with-temp-buffer
      (let* ((scanned 0)
             (agent-ide-stream-render-interval 0)
             (state (agent-ide-stream-create
                     1 (lambda (start end) (cl-incf scanned (- end start))))))
        (dotimes (_ count)
          (goto-char (point-max))
          (insert (make-string 100 ?x))
          (agent-ide-stream-queue state (point-max)))
        (agent-ide-stream-flush state t)
        (should (< scanned (+ 100000 (* 4 count 100))))))))

(ert-deftest agent-ide-stream-does-not-revisit-completed-paragraphs ()
  (with-temp-buffer
    (let* ((scanned 0)
           (agent-ide-stream-render-interval 0)
           (state (agent-ide-stream-create
                   1 (lambda (start end) (cl-incf scanned (- end start))))))
      (dotimes (_ 400)
        (goto-char (point-max))
        (insert "A complete paragraph.\n\n")
        (agent-ide-stream-queue state (point-max)))
      (should (= scanned (buffer-size)))
      (agent-ide-stream-flush state t)
      (should (= scanned (* 2 (buffer-size)))))))

(ert-deftest agent-ide-stream-keeps-multiline-constructs-together ()
  (dolist (source '("```elisp\n(first)\n\n(second)\n"
                    "~~~text\nfirst\n\nsecond\n"
                    "$$a\n\nb" "\\[a\n\nb" "\\(a\n\nb"
                    "`first\n\nsecond" "``first\n\nsecond"))
    (with-temp-buffer
      (insert "intro\n\n" source)
      (should (= (agent-ide-stream--boundary 1 (point-max)) 8)))))

(ert-deftest agent-ide-stream-reuses-fontified-code-before-later-paragraphs ()
  (with-temp-buffer
    (let* ((agent-ide-stream-render-interval 0)
           (agent-ide-latex-preview nil)
           (agent-ide-media-preview nil)
           (calls 0)
           (mode (symbol-function 'emacs-lisp-mode))
           (state (agent-ide-stream-create
                   1 #'agent-ide-renderer-render-markdown-region)))
      (cl-letf (((symbol-function 'emacs-lisp-mode)
                 (lambda () (cl-incf calls) (funcall mode))))
        (insert "```elisp\n(message \"hello\")\n```\n")
        (agent-ide-stream-queue state (point-max))
        (dotimes (_ 100)
          (goto-char (point-max))
          (insert "More **text**.\n\n")
          (agent-ide-stream-queue state (point-max)))
        (should (= calls 1))
        (agent-ide-stream-flush state t)
        (should (= calls 2))))))

(ert-deftest agent-ide-stream-finalizes-split-markdown-and-math ()
  (dolist (chunks '(("intro\n\n**bo" "ld** and [link](" "target.el)\n\nend")
                    ("intro\n\n```elisp\n(message \"" "hello\")\n\n" "nil\n```\n" "end")
                    ("intro\n\n\\[x" "\n\n+y" "\\]\n\n" "end")
                    ("intro\n\n$$x" "\n\n+y" "$$\n\n" "end")
                    ("intro\n\n| A | B |\n" "|---|---|\n" "| 1 | 2 |\n\nend")))
    (let ((agent-ide-latex-preview nil)
          (agent-ide-media-preview nil)
          (agent-ide-stream-render-interval 0)
          (source (apply #'concat chunks))
          expected)
      (with-temp-buffer
        (insert source)
        (agent-ide-renderer-render-markdown-region 1 (point-max))
        (setq expected (buffer-string)))
      (with-temp-buffer
        (let ((state (agent-ide-stream-create
                      1 #'agent-ide-renderer-render-markdown-region)))
          (dolist (chunk chunks)
            (goto-char (point-max))
            (insert chunk)
            (agent-ide-stream-queue state (point-max)))
          (agent-ide-stream-flush state t)
          (should (equal (buffer-substring-no-properties 1 (point-max)) source))
          (dotimes (i (length source))
            (dolist (property '(display agent-ide-url agent-ide-markdown-code-content
                               agent-ide-latex))
              (should (equal (get-text-property (1+ i) property)
                             (get-text-property i property expected))))))))))

(ert-deftest agent-ide-stream-cancels-work-when-buffer-dies ()
  (let ((buffer (generate-new-buffer " *stream-test*")) state timer)
    (with-current-buffer buffer
      (setq state (agent-ide-stream-create 1 #'ignore))
      (insert "a")
      (agent-ide-stream-queue state (point-max))
      (insert "b")
      (agent-ide-stream-queue state (point-max))
      (setq timer (agent-ide-stream-timer state)))
    (kill-buffer buffer)
    (should-not (memq timer timer-list))
    (should-not (marker-buffer (agent-ide-stream-start state)))
    (agent-ide-stream-flush state)))

(ert-deftest agent-ide-stream-inline-finishes-pending-output-on-success-or-failure ()
  (dolist (failure '(nil t))
    (with-temp-buffer
      (insert "origin\n")
      (let* ((agent-ide-inline--overlays nil)
             (session (agent-ide--make-session :buffer (current-buffer)
                                               :directory default-directory))
             (ov (agent-ide-inline--response-overlay-create session (current-buffer) 1))
             (state (agent-ide-inline--stream-state ov)))
        (unwind-protect
            (progn
              (agent-ide-inline--on-chunk session "**bo")
              (agent-ide-inline--on-chunk session "ld**")
              (let ((timer (agent-ide-stream-timer state)))
                (should (timerp timer))
                (if failure
                    (agent-ide-inline--on-failure session '((message . "interrupted")))
                  (agent-ide-inline--on-response session nil))
                (should-not (memq timer timer-list)))
              (let* ((text (overlay-get ov 'after-string))
                     (pos (string-match "bold" text)))
                (should pos)
                (should (memq 'bold (ensure-list (get-text-property pos 'face text)))))
              (should (plist-get (overlay-get ov 'agent-ide-inline) :done)))
          (agent-ide-inline-clear-response-overlay ov))))))

(ert-deftest agent-ide-stream-refresh-preserves-draft-point-and-undo ()
  (with-temp-buffer
    (agent-ide-session-mode)
    (let ((session (agent-ide--make-session
                    :buffer (current-buffer) :directory default-directory
                    :tool-calls (make-hash-table :test 'equal))))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-create-prompt session)
      (agent-ide-renderer-replace-current-input session "my draft")
      (agent-ide-renderer-append-stream-chunk session 'message "**bo")
      (agent-ide-renderer-append-stream-chunk session 'message "ld**")
      (goto-char (+ 3 (agent-ide-session-input-start-marker session)))
      (let ((before (point)) (undo buffer-undo-list) (modified (buffer-modified-p)))
        (agent-ide-stream-flush (agent-ide--session-metadata-get session :stream-renderer))
        (should (= before (point)))
        (should (eq undo buffer-undo-list))
        (should (eq modified (buffer-modified-p)))
        (should (equal (agent-ide-renderer-current-input session) "my draft"))
        (goto-char (point-min))
        (search-forward "bold")
        (should-error (insert "bad") :type 'buffer-read-only)))))

(ert-deftest agent-ide-stream-flushes-before-tool-output-and-kind-changes ()
  (dolist (next '(tool thought reset))
    (with-temp-buffer
      (agent-ide-session-mode)
      (let ((session (agent-ide--make-session
                      :buffer (current-buffer) :directory default-directory
                      :tool-calls (make-hash-table :test 'equal))))
        (setq-local agent-ide--session session)
        (agent-ide-renderer-initialize-buffer session)
        (agent-ide-renderer-create-prompt session)
        (agent-ide-renderer-append-stream-chunk session 'message "**bo")
        (agent-ide-renderer-append-stream-chunk session 'message "ld**")
        (let* ((state (agent-ide--session-metadata-get session :stream-renderer))
               (timer (agent-ide-stream-timer state)))
          (pcase next
            ('tool (agent-ide-renderer-update-tool session "tool" "Tool" "output"))
            ('thought (agent-ide-renderer-append-stream-chunk session 'thought "thinking"))
            ('reset (agent-ide-renderer-reset-stream session)))
          (should-not (memq timer timer-list))
          (should-not (marker-buffer (agent-ide-stream-start state)))
          (goto-char (point-min))
          (search-forward "bold")
          (should (memq 'bold (ensure-list (get-text-property (1- (point)) 'face)))))))))

(provide 'agent-ide-stream-test)
;;; agent-ide-stream-test.el ends here
