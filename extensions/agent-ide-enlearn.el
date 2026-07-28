;;; agent-ide-enlearn.el --- English coach pre-submit plugin -*- lexical-binding: t; -*-

;;; Commentary:
;; Optional plugin: translate/polish prompts via gptel, explain in Chinese,
;; then deliver final English through agent-ide-deliver-prompt.

;;; Code:

(require 'subr-x)
(require 'agent-ide-session)

(defgroup agent-ide-enlearn nil
  "English learning pre-submit coach for Agent IDE."
  :group 'agent-ide
  :prefix "agent-ide-enlearn-")

(defun agent-ide-enlearn--mode-for-text (text)
  "Return `translate' if TEXT is mostly Chinese, else `polish'."
  (let* ((han (length (replace-regexp-in-string "[^\u4e00-\u9fff]" "" text)))
         (total (max 1 (length (replace-regexp-in-string "[[:space:]]" "" text)))))
    (if (>= (/ (float han) total) 0.2)
        'translate
      'polish)))

(defun agent-ide-enlearn--parse-response (raw)
  "Parse gptel RAW markdown into plist :final :breakdown :grammar.
:final is nil when the Final section is missing or empty."
  (let* ((final (agent-ide-enlearn--section raw "Final"))
         (breakdown (or (agent-ide-enlearn--section raw "Breakdown") ""))
         (grammar (or (agent-ide-enlearn--section raw "Grammar") "")))
    (list :final (and final (not (string-empty-p (string-trim final)))
                      (string-trim final))
          :breakdown (string-trim breakdown)
          :grammar (string-trim grammar)
          :raw raw)))

;; Prefer portable splitter (Emacs may not like (?s) in string-match):
(defun agent-ide-enlearn--section (raw name)
  "Return body of ## NAME section in RAW, or nil."
  (let ((parts (split-string raw "^## " t))
        found)
    (dolist (part parts found)
      (when (string-match (format "\\`%s[ \t]*\\(?:\n\\|\\'\\)" (regexp-quote name)) part)
        (setq found (string-trim (substring part (match-end 0))))))))

(provide 'agent-ide-enlearn)
