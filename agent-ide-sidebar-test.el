;;; agent-ide-sidebar-test.el --- Tests for Agent IDE sidebar -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(unless (featurep 'acp)
  (provide 'acp))

(require 'agent-ide-core)
(require 'agent-ide-renderer)
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
        (should (string-match-p "running" line)))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-formats-meta-line ()
  "Line 2 includes model and percent usage."
  (unwind-protect
      (let* ((session (agent-ide-sidebar-test--make-session
                       "/tmp/myproject" "idle"
                       :models [((id . "large") (name . "Large") (isDefault . t))]
                       :usage '((used . 42000) (size . 100000))))
             (line (agent-ide-sidebar--format-line2 session)))
        (should (string-match-p "Large" line))
        (should (string-match-p "42%" line)))
    (agent-ide-sidebar-test--teardown)))

(ert-deftest agent-ide-sidebar-formats-unknown-meta-as-dash ()
  "Unknown model/usage becomes an em dash."
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
            (should (string-match-p "\\[\\+ New\\]" text))
            (should (= (length agent-ide-sidebar--entries) 2)))))
    (agent-ide-sidebar-test--teardown)))

(provide 'agent-ide-sidebar-test)

;;; agent-ide-sidebar-test.el ends here
