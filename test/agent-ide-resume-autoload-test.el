;;; agent-ide-resume-autoload-test.el --- Cold-start resume -*- lexical-binding: t; -*-

;; Run in a fresh batch Emacs with acp and valign on load-path.  Do not load
;; agent-ide or the other session tests before this file.
(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'use-package)

(defvar agent-ide-command)
(defvar agent-ide-history-file)
(defvar agent-ide--sessions)

(defconst agent-ide-autoload-test--root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(ert-deftest agent-ide-resume-autoload-from-project-without-new-session ()
  (skip-unless (executable-find "python3"))
  (should-not (featurep 'agent-ide))
  (eval `(use-package agent-ide
           :load-path ,agent-ide-autoload-test--root
           :commands (agent-ide agent-ide-inline
                      agent-ide-resume agent-ide-resume-history)))
  (should (autoloadp (symbol-function 'agent-ide-resume)))
  (should (commandp 'agent-ide-resume))
  (should (autoloadp (symbol-function 'agent-ide-resume-history)))
  (should-not (featurep 'agent-ide))
  (let* ((directory (file-name-as-directory (make-temp-file "agent-ide-cold-" t)))
         (default-directory directory)
         (state-file (expand-file-name "state.json" directory))
         (agent-ide-history-file (expand-file-name "sessions.json" directory))
         (agent-ide-command
          (list (executable-find "python3") "-u"
                (expand-file-name "test/fixtures/acp-resume-agent.py"
                                  agent-ide-autoload-test--root)
                state-file "both"))
         (agent-ide--sessions nil)
         session)
    (unwind-protect
        (progn
          (with-temp-file state-file
            (insert (json-serialize
                     '((created . t) (requests . [])
                       (messages . [["user_message_chunk" "Earlier question"]
                                    ["agent_message_chunk" "Earlier answer"]])))))
          (with-temp-file agent-ide-history-file
            (insert (json-serialize
                     (vector `((sessionId . "fixture-session")
                               (directory . ,directory)
                               (backend . ,(secure-hash 'sha256 (prin1-to-string agent-ide-command)))
                               (backendName . "fixture") (title . "Existing conversation")
                               (createdAt . 1) (updatedAt . 2))))))
          (with-temp-buffer
            ;; This is an ordinary project buffer, not an Agent IDE buffer.
            (setq default-directory directory)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _) (caar collection))))
              (setq session (call-interactively #'agent-ide-resume))))
          (let ((deadline (+ (float-time) 5)))
            (while (and (not (equal (agent-ide-session-status session) "idle"))
                        (< (float-time) deadline))
              (accept-process-output nil 0.02)))
          (should (featurep 'agent-ide))
          (should (equal (agent-ide-session-status session) "idle"))
          (should (equal agent-ide--sessions (list session)))
          (should (equal (agent-ide-session-acp-session-id session) "fixture-session"))
          (with-current-buffer (agent-ide-session-buffer session)
            (should (string-match-p "Earlier answer" (buffer-string))))
          (with-temp-buffer
            (setq default-directory directory)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _) (caar collection))))
              (should (eq session (call-interactively #'agent-ide-resume)))))
          (should (equal agent-ide--sessions (list session)))
          (with-temp-buffer
            (insert-file-contents state-file)
            (should (equal (map-elt (json-parse-buffer :object-type 'alist :array-type 'list)
                                    'requests)
                           '("initialize" "session/load")))))
      (when session
        (kill-buffer (agent-ide-session-buffer session)))
      (delete-directory directory t))))

;;; agent-ide-resume-autoload-test.el ends here
