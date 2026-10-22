;;; agent-ide-performance-test.el --- Bounded UI work regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-ide)

(defmacro agent-ide-performance-test--session (&rest body)
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let* ((session (agent-ide--make-session
                      :buffer (current-buffer) :directory default-directory
                      :tool-calls (make-hash-table :test 'equal)))
            (agent-ide--sessions (list session))
            (agent-ide-header-icon-animation-interval nil)
            (agent-ide-placeholder-ellipsis-animation-interval nil)
            (agent-ide-media-preview nil))
       (setq-local agent-ide--session session)
       ,@body)))

(ert-deftest agent-ide-performance-tool-replacement-releases-overlays ()
  (agent-ide-performance-test--session
    (dotimes (i 100)
      (agent-ide-renderer-update-tool session "tool" "Command"
                                      (format "result %d" i) "running"))
    (should (string-match-p "result 99" (buffer-string)))
    (should (= 1 (length (append (car (overlay-lists)) (cdr (overlay-lists)))))))
  (agent-ide-performance-test--session
    (dotimes (i 20)
      (agent-ide-renderer-update-tool session "tool" "Command"
                                      (format "result %d" i) "running" 'foldable))
    (should (= 1 (length (append (car (overlay-lists)) (cdr (overlay-lists))))))))

(ert-deftest agent-ide-performance-status-only-update-preserves-open-output ()
  (agent-ide-performance-test--session
    (agent-ide-renderer-update-tool session "tool" "Command" "output [expand]"
                                    "running" nil "full output")
    (goto-char (point-min))
    (search-forward "[expand]")
    (let* ((button (button-at (1- (point))))
           (fold (get-text-property 2 'agent-ide-fold-overlay)))
      (agent-ide-renderer-toggle-expand-at-point button)
      (let ((tick (buffer-modified-tick))
            (text (buffer-string)))
        (dotimes (_ 20)
          (agent-ide-renderer-update-tool session "tool" nil nil "completed"))
        (should (= tick (buffer-modified-tick)))
        (should (equal text (buffer-string)))
        (should (overlay-buffer fold))
        (should (equal "completed" (plist-get
                                    (agent-ide-renderer--tool-record session "tool") :status)))
        (should (marker-buffer (button-get button 'agent-ide-expanded-start)))))))

(ert-deftest agent-ide-performance-diff-computed-once-per-file-per-update ()
  (let ((calls 0)
        (update '((kind . "edit")
                  (content . [((type . "diff") (path . "file.txt")
                               (oldText . "old\n") (newText . "new\n"))]))))
    (cl-letf (((symbol-function 'agent-ide-transcript--compute-unified-diff-from-text)
               (lambda (&rest _) (cl-incf calls)
                 "--- a/file.txt\n+++ b/file.txt\n@@ -1 +1 @@\n-old\n+new")))
      (let ((display (agent-ide-transcript--codex-tool-display update)))
        (should (= calls 1))
        (should (string-match-p "+1/-1" (plist-get display :body)))
        (should (string-match-p "+new" (plist-get display :expanded-output))))
      ;; The cache is not retained across updates, even for the same object.
      (agent-ide-transcript--codex-tool-display update)
      (should (= calls 2)))))

(ert-deftest agent-ide-performance-active-index-tracks-completion-and-reconnect ()
  (agent-ide-performance-test--session
    (let ((history (agent-ide-session-tool-calls session))
          (map (symbol-function 'maphash)))
      (dotimes (i 1000) (puthash i '(:status "completed") history))
      (should (= 0 (hash-table-count (agent-ide-renderer-active-tools session))))
      (cl-letf (((symbol-function 'maphash)
                 (lambda (fn table)
                   (should-not (eq table history))
                   (funcall map fn table))))
        (agent-ide-renderer--put-tool-record
         session "a" '(:title "Running" :status "running"))
        (should (equal "Running" (agent-ide-sidebar--active-tool-title session)))
        (agent-ide-renderer--put-tool-record
         session "p" '(:permission t :pending t :title "Approval"))
        (should (agent-ide-sidebar--pending-permission-p session))
        (agent-ide-renderer--put-tool-record
         session "a" '(:title "Running" :status "completed"))
        (should-not (agent-ide-sidebar--active-tool-title session)))
      (agent-ide--invalidate-connection session)
      (should (= 0 (hash-table-count (agent-ide-renderer-active-tools session))))
      (setf (agent-ide-session-tool-calls session) (make-hash-table :test 'equal))
      (agent-ide-renderer--put-tool-record session "new" '(:title "New" :status "running"))
      (should (equal "New" (agent-ide-sidebar--active-tool-title session))))))

(ert-deftest agent-ide-performance-sidebar-coalesces-and-defers-hidden-updates ()
  (agent-ide-performance-test--session
    (let ((agent-ide-sidebar--refresh-timer nil)
          (agent-ide-sidebar--dirty nil)
          (agent-ide-sidebar-auto-show nil)
          (visible t) (renders 0))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-ide-sidebar--visible-p) (lambda () visible))
                    ((symbol-function 'agent-ide-sidebar-refresh)
                     (lambda () (cl-incf renders) (setq agent-ide-sidebar--dirty nil))))
            (agent-ide-sidebar-on-sessions-changed)
            (let ((timer agent-ide-sidebar--refresh-timer))
              (dotimes (_ 20) (agent-ide-sidebar-on-sessions-changed))
              (should (eq timer agent-ide-sidebar--refresh-timer)))
            (agent-ide-sidebar--flush-refresh)
            (should (= renders 1))
            (setq visible nil)
            (dotimes (_ 20) (agent-ide-sidebar-on-sessions-changed))
            (should-not agent-ide-sidebar--refresh-timer)
            (should (= renders 1))
            (setq visible t)
            (agent-ide-sidebar--schedule-refresh)
            (agent-ide-sidebar--flush-refresh)
            (should (= renders 2)))
        (agent-ide-sidebar--cancel-refresh)))))

(ert-deftest agent-ide-performance-media-validates-only-affected-source ()
  (with-temp-buffer
    (insert (make-string 100000 ?A))
    (let ((ov (make-overlay 1 (point-max) nil t nil))
          (checks 0)
          (intact (symbol-function 'agent-ide-media--intact-p)))
      (overlay-put ov 'source-text (buffer-string))
      (setq agent-ide-media--items (list ov))
      (insert "\nresponse\n")
      (add-hook 'after-change-functions #'agent-ide-media--prune nil t)
      (cl-letf (((symbol-function 'agent-ide-media--intact-p)
                 (lambda (item) (cl-incf checks) (funcall intact item))))
        (dotimes (_ 100) (insert "more text"))
        (should (= checks 0))
        ;; Removing the full image leaves a zero-length overlay at the edit.
        (delete-region 1 100001)
        (should (= checks 1))
        (should-not (overlay-buffer ov))
        (should-not agent-ide-media--items)))))

(ert-deftest agent-ide-performance-code-cache-invalidates-on-source-change ()
  (with-temp-buffer
    (insert "```elisp\n(message \"first\")\n```\n")
    (let ((calls 0) (hooks 0)
          (mode (symbol-function 'emacs-lisp-mode))
          (emacs-lisp-mode-hook nil))
      (add-hook 'emacs-lisp-mode-hook (lambda () (cl-incf hooks)))
      (cl-letf (((symbol-function 'emacs-lisp-mode)
                 (lambda () (cl-incf calls) (funcall mode))))
        (agent-ide-renderer-render-markdown-region 1 (point-max))
        (agent-ide-renderer-render-markdown-region 1 (point-max))
        (should (= calls 1))
        (should (= hooks 0))
        (goto-char 1) (search-forward "first") (replace-match "second")
        (agent-ide-renderer-render-markdown-region 1 (point-max))
        (should (= calls 2))
        (should (get-text-property (match-beginning 0) 'face))))))

(ert-deftest agent-ide-performance-unchanged-input-does-not-copy-text ()
  (with-temp-buffer
    (insert (make-string 100000 ?x))
    (agent-ide-latex-update-input 1 (point-max))
    (cl-letf (((symbol-function 'buffer-substring-no-properties)
               (lambda (&rest _) (ert-fail "Copied unchanged prompt"))))
      (goto-char 1)
      (dotimes (_ 100)
        (forward-char)
        (agent-ide-latex-update-input 1 (point-max))))
    (insert " $x$ ")
    (agent-ide-latex-update-input 1 (point-max))
    (should (= 1 (length agent-ide-latex--input-fragments)))))

(ert-deftest agent-ide-performance-inline-caches-append-and-recovers-from-edits ()
  (with-temp-buffer
    (let ((src (generate-new-buffer " *perf-inline*"))
          (ov (make-overlay 1 1)))
      (unwind-protect
          (progn
            (overlay-put ov 'agent-ide-inline (list :src src :session-buffer (current-buffer)))
            (overlay-put ov 'agent-ide-inline-height 2)
            (with-current-buffer src (insert "one\ntwo\nthree\n"))
            (agent-ide-inline--response-overlay-render ov)
            (agent-ide-inline--response-overlay-set-scroll-index ov 1)
            (agent-ide-inline--response-overlay-render ov)
            (cl-letf (((symbol-function 'line-number-at-pos)
                       (lambda (&rest _) (ert-fail "Recounted append-only response"))))
              (agent-ide-inline--response-overlay-append-chunk ov "four\n")
              (agent-ide-inline--response-overlay-render ov))
            (should (string-match-p "two\nthree" (overlay-get ov 'after-string)))
            (with-current-buffer src
              (should (= 5 (agent-ide-inline--line-count)))
              (goto-char 1) (insert "zero\n")
              (should (= 6 (agent-ide-inline--line-count))))
            (agent-ide-inline--response-overlay-render ov)
            (should (string-match-p "one\ntwo" (overlay-get ov 'after-string))))
        (kill-buffer src)))))

(ert-deftest agent-ide-performance-tail-follow-avoids-layout-query ()
  (save-window-excursion
    (agent-ide-performance-test--session
      (set-window-buffer (selected-window) (current-buffer))
      (cl-letf (((symbol-function 'window-end)
                 (lambda (&rest _) (ert-fail "Laid out an already-following window"))))
        (dotimes (_ 50) (agent-ide-renderer-append-stream-chunk session 'message "chunk ")))
      (agent-ide-renderer-reset-stream session)
      (should (= (window-point) (point-max))))))

(ert-deftest agent-ide-performance-header-only-animates-visible-buffers ()
  (save-window-excursion
    (agent-ide-performance-test--session
      (let ((summaries 0)
            (agent-ide-renderer--header-icon-timer nil))
        (cl-letf (((symbol-function 'agent-ide-renderer--header-summary)
                   (lambda (_) (cl-incf summaries) "Summary"))
                  ((symbol-function 'agent-ide-renderer--header-icon-string) (lambda () "A")))
          (agent-ide-renderer--advance-header-icon)
          (should (= summaries 0))
          (set-window-buffer (selected-window) (current-buffer))
          (dotimes (_ 10) (agent-ide-renderer--advance-header-icon))
          (should (= summaries 1))
          (agent-ide-renderer--set-header-line session t)
          (should (= summaries 2)))))))

(provide 'agent-ide-performance-test)
;;; agent-ide-performance-test.el ends here
