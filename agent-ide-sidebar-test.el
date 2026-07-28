;;; agent-ide-sidebar-test.el --- Tests for Agent IDE sidebar -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(unless (featurep 'acp)
  (provide 'acp))
(unless (featurep 'valign)
  (provide 'valign)
  (defun valign-mode (&rest _))
  (defun valign--guess-table-type (&rest _)))

(require 'agent-ide-core)
(require 'agent-ide-renderer)
(require 'agent-ide-session)
(require 'agent-ide-sidebar)

(defun agent-ide-sidebar-test--make-session (directory status &rest plist)
  "Make a live test session for DIRECTORY with STATUS.
PLIST may include :models :usage :buffer-name."
  (let* ((buf-name (or (plist-get plist :buffer-name)
                       (format "*agent-test:%s*"
                               (agent-ide--directory-name directory))))
         (buffer (get-buffer-create buf-name))
         (session (agent-ide--make-session
                   :directory (file-name-as-directory (expand-file-name directory))
                   :buffer buffer
                   :status status
                   :tool-calls (make-hash-table :test 'equal)
                   :models (plist-get plist :models)
                   :usage (plist-get plist :usage))))
    (with-current-buffer buffer
      (setq-local default-directory (agent-ide-session-directory session))
      (setq-local agent-ide--session session))
    session))

(defun agent-ide-sidebar-test--teardown ()
  "Kill sidebar and test session buffers; clear global session list."
  (setq agent-ide--sessions nil)
  (setq agent-ide-sidebar--user-dismissed nil)
  (when-let* ((buf (get-buffer agent-ide-sidebar-buffer-name)))
    (kill-buffer buf))
  (dolist (buf (buffer-list))
    (when (string-prefix-p "*agent-test:" (buffer-name buf))
      (kill-buffer buf))))

(ert-deftest agent-ide-sidebar-formats-project-line ()
  "Line 1 includes project name, optional index, and status."
  (unwind-protect
      (let* ((session (agent-ide-sidebar-test--make-session
                       "/tmp/myproject" "running"
                       :buffer-name "*agent:myproject*<2>"))
             (line (agent-ide-sidebar--format-line1 session nil)))
        (should (string-match-p "myproject" line))
        (should (string-match-p "<2>" line))
        (should (string-match-p "\\[running\\]" line)))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-truncates-long-project-name ()
  "Long project names end with .. and keep bracketed status."
  (unwind-protect
      (cl-letf (((symbol-function 'agent-ide-sidebar--line1-fits-p)
                 (lambda (line _window)
                   (<= (string-width line) 18))))
        (let* ((session (agent-ide-sidebar-test--make-session
                         "/tmp/very-long-project-name" "idle"))
               (line (substring-no-properties
                      (agent-ide-sidebar--format-line1 session nil))))
          (should (string-match-p "\\.\\." line))
          (should (string-suffix-p "[idle]" line))
          (should (<= (string-width line) 18))))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-formats-meta-line ()
  "Line 2 includes model."
  (unwind-protect
      (let* ((session (agent-ide-sidebar-test--make-session
                       "/tmp/myproject" "idle"
                       :models [((id . "large") (name . "Large") (isDefault . t))]))
             (line (agent-ide-sidebar--format-line2 session)))
        (should (string-match-p "Large" line)))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-formats-unknown-meta-as-dash ()
  "Unknown model becomes an em dash."
  (unwind-protect
      (let* ((session (agent-ide-sidebar-test--make-session "/tmp/x" "idle"))
             (line (agent-ide-sidebar--format-line2 session)))
        (should (string-match-p "—" line)))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-renders-multiple-sessions ()
  "Refresh inserts two lines per session plus footer."
  (unwind-protect
      (let ((a (agent-ide-sidebar-test--make-session "/tmp/a" "idle"))
            (b (agent-ide-sidebar-test--make-session "/tmp/b" "running")))
        (setq agent-ide--sessions (list b a))
        (agent-ide-sidebar-refresh)
        (with-current-buffer agent-ide-sidebar-buffer-name
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "a" text))
            (should (string-match-p "b" text))
            (should (string-match-p "idle" text))
            (should (string-match-p "running" text))
            (should-not (string-match-p "\\[\\+ New\\]" text))
            (should (= (length agent-ide-sidebar--entries) 2)))))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-quit-sets-dismissed-and-hides ()
  "q marks dismissed and removes the side window."
  (unwind-protect
      (let ((session (agent-ide-sidebar-test--make-session "/tmp/a" "idle")))
        (setq agent-ide--sessions (list session))
        (agent-ide-sidebar)
        (should (get-buffer-window agent-ide-sidebar-buffer-name t))
        (agent-ide-sidebar-quit)
        (should agent-ide-sidebar--user-dismissed)
        (should (null (get-buffer-window agent-ide-sidebar-buffer-name t))))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-refresh-does-not-reshow-when-dismissed ()
  "Status refresh keeps sidebar hidden after user dismiss."
  (unwind-protect
      (let ((session (agent-ide-sidebar-test--make-session "/tmp/a" "idle")))
        (setq agent-ide--sessions (list session))
        (agent-ide-sidebar)
        (agent-ide-sidebar-quit)
        (agent-ide-sidebar-on-sessions-changed)
        (should (null (get-buffer-window agent-ide-sidebar-buffer-name t))))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-new-session-clears-dismissed ()
  "Creating a session shows sidebar again when auto-show is on."
  (unwind-protect
      (let ((agent-ide-sidebar-auto-show t)
            (session (agent-ide-sidebar-test--make-session "/tmp/a" "idle")))
        (setq agent-ide-sidebar--user-dismissed t)
        (setq agent-ide--sessions (list session))
        (agent-ide-sidebar-on-session-created session)
        (should (null agent-ide-sidebar--user-dismissed))
        (should (get-buffer-window agent-ide-sidebar-buffer-name t)))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-create-refreshes-when-visible-even-if-auto-show-off ()
  "Visible sidebar refreshes on create even when auto-show is nil."
  (unwind-protect
      (let ((agent-ide-sidebar-auto-show nil)
            (a (agent-ide-sidebar-test--make-session "/tmp/a" "idle")))
        (setq agent-ide--sessions (list a))
        (agent-ide-sidebar)
        (let ((b (agent-ide-sidebar-test--make-session "/tmp/b" "idle")))
          (setq agent-ide--sessions (list b a))
          (agent-ide-sidebar-on-session-created b)
          (with-current-buffer agent-ide-sidebar-buffer-name
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "b" text))
              (should (get-buffer-window agent-ide-sidebar-buffer-name t))))))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-hides-when-no-sessions ()
  "Cleanup of last session hides the sidebar."
  (unwind-protect
      (let ((session (agent-ide-sidebar-test--make-session "/tmp/a" "idle")))
        (setq agent-ide--sessions (list session))
        (agent-ide-sidebar)
        (setq agent-ide--sessions nil)
        (agent-ide-sidebar-on-sessions-changed)
        (should (null (get-buffer-window agent-ide-sidebar-buffer-name t))))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-select-displays-session-buffer ()
  "RET displays the session buffer."
  (unwind-protect
      (let* ((session (agent-ide-sidebar-test--make-session "/tmp/a" "idle"))
             (shown nil))
        (setq agent-ide--sessions (list session))
        (agent-ide-sidebar-refresh)
        (cl-letf (((symbol-function 'agent-ide--display-buffer)
                   (lambda (buffer)
                     (setq shown buffer)
                     nil)))
          (with-current-buffer agent-ide-sidebar-buffer-name
            (goto-char (point-min))
            (agent-ide-sidebar-select))
          (should (eq shown (agent-ide-session-buffer session)))))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-select-replaces-existing-session-window ()
  "RET swaps buffer in an existing session window instead of splitting."
  (unwind-protect
      (let* ((a (agent-ide-sidebar-test--make-session "/tmp/a" "idle"))
             (b (agent-ide-sidebar-test--make-session "/tmp/b" "idle"))
             (displayed nil)
             (window nil))
        (setq agent-ide--sessions (list b a))
        (with-current-buffer (agent-ide-session-buffer a)
          (agent-ide-session-mode))
        (with-current-buffer (agent-ide-session-buffer b)
          (agent-ide-session-mode))
        (setq window (display-buffer (agent-ide-session-buffer a)
                                     '(display-buffer-same-window)))
        (agent-ide-sidebar-refresh)
        (cl-letf (((symbol-function 'agent-ide--display-buffer)
                   (lambda (buffer)
                     (setq displayed buffer)
                     nil)))
          (with-current-buffer agent-ide-sidebar-buffer-name
            (goto-char (point-min))
            (unless (eq (agent-ide-sidebar--session-at-point) b)
              (goto-char (text-property-any
                          (point-min) (point-max)
                          'agent-ide-session b)))
            (agent-ide-sidebar-select))
          (should (null displayed))
          (should (eq (window-buffer window)
                      (agent-ide-session-buffer b)))))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-kill-removes-session ()
  "k kills the session buffer and removes the entry."
  (unwind-protect
      (let ((session (agent-ide-sidebar-test--make-session "/tmp/a" "idle"))
            (agent-ide-sidebar-confirm-kill nil))
        (setq agent-ide--sessions (list session))
        (with-current-buffer (agent-ide-session-buffer session)
          (add-hook 'kill-buffer-hook #'agent-ide--handle-buffer-killed nil t))
        (agent-ide-sidebar-refresh)
        (with-current-buffer agent-ide-sidebar-buffer-name
          (goto-char (point-min))
          (agent-ide-sidebar-kill))
        (should (null agent-ide--sessions))
        (should (not (buffer-live-p (agent-ide-session-buffer session)))))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-status-change-updates-text ()
  "agent-ide--set-status refreshes sidebar text."
  (unwind-protect
      (let ((session (agent-ide-sidebar-test--make-session "/tmp/a" "idle")))
        (setq agent-ide--sessions (list session))
        (agent-ide-sidebar-refresh)
        (agent-ide--set-status session "running")
        (with-current-buffer agent-ide-sidebar-buffer-name
          (should (string-match-p
                   "running"
                   (buffer-substring-no-properties (point-min) (point-max))))))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-line2-shows-model ()
  "Line 2 includes model, not tool."
  (unwind-protect
      (let ((session (agent-ide-sidebar-test--make-session
                      "/tmp/a" "running"
                      :models [((id . "m") (name . "Model") (isDefault . t))])))
        (puthash "t1"
                 (list :title "Bash" :status "in_progress")
                 (agent-ide-session-tool-calls session))
        (let ((line (substring-no-properties
                     (agent-ide-sidebar--format-line2 session))))
          (should (string-match-p "Model" line))
          (should-not (string-match-p "Bash" line))))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-line3-shows-active-tool ()
  "Line 3 shows the in-progress tool title."
  (unwind-protect
      (let ((session (agent-ide-sidebar-test--make-session "/tmp/a" "running")))
        (puthash "t1"
                 (list :title "Bash" :status "in_progress")
                 (agent-ide-session-tool-calls session))
        (let ((line (substring-no-properties
                     (agent-ide-sidebar--format-line3 session))))
          (should (string-match-p "Bash" line))))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-shows-ask-when-permission-pending ()
  "Pending permission replaces status with [ask]."
  (unwind-protect
      (let ((session (agent-ide-sidebar-test--make-session "/tmp/a" "running")))
        (puthash "permission-1"
                 (list :permission t :pending t :title "Shell")
                 (agent-ide-session-tool-calls session))
        (let ((line (substring-no-properties
                     (agent-ide-sidebar--format-line1 session nil))))
          (should (string-match-p "\\[ask\\]" line))))
    (agent-ide-sidebar-test--teardown)))

(provide 'agent-ide-sidebar-test)

;;; agent-ide-sidebar-test.el ends here
