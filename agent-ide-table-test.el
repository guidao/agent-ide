;;; agent-ide-table-test.el --- Wrapped table regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'agent-ide)

(defun agent-ide-table-test--displays (&optional table window)
  (mapcar (lambda (o) (overlay-get o 'display))
          (sort (seq-filter (lambda (o) (eq (overlay-get o 'window) window))
                            (copy-sequence (overlay-get (or table (car agent-ide-table--tables)) 'views)))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defmacro agent-ide-table-test--with (text width &rest body)
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,text)
     (cl-letf (((symbol-function 'agent-ide-table--window-state) (lambda () (list (cons nil ,width)))))
       (agent-ide-renderer-render-markdown-region (point-min) (point-max))
       ,@body)))

(ert-deftest agent-ide-table-wraps-cjk-and-preserves-source ()
  (let ((source "| 信息 | 说明 |\n|---|---|\n| 中文 | 一段比较长的中文和 English words |\n"))
    (agent-ide-table-test--with source 28
      (should (equal source (buffer-substring-no-properties 1 (point-max))))
      (should (= 1 (length agent-ide-table--tables)))
      (let ((rows (agent-ide-table-test--displays)))
        (should (= 3 (length rows)))
        (should (string-match-p "\n" (nth 2 rows)))
        (let ((widths (mapcar #'string-width (split-string (string-join rows "\n") "\n"))))
          (should (cl-every (lambda (w) (= w 28)) widths)))))))

(ert-deftest agent-ide-table-parses-escapes-code-and-optional-outer-pipes ()
  (should (equal (agent-ide-table--cells "| a\\|b | `x|y` | ``a`|b`` |")
                 '("a\\|b" "`x|y`" "``a`|b``")))
  (agent-ide-table-test--with "Left | Right\n:--- | ---:\na\\|b | `x|y`\n" 40
    (should (= 1 (length agent-ide-table--tables)))
    (let ((last (car (last (agent-ide-table-test--displays)))))
      (should (string-match-p "a|b" last))
      (should (string-match-p "x|y" last))
      (should-not (string-match-p "`" last)))))

(ert-deftest agent-ide-table-wraps-words-and-composed-characters ()
  (should (equal (agent-ide-table--wrap "one two three" 7) '("one two" "three")))
  (should (equal (agent-ide-table--wrap "中文中文" 4) '("中文" "中文")))
  (let ((accent (concat "e" (string #x301))))
    (should (equal (agent-ide-table--wrap (concat accent accent) 1) (list accent accent)))))

(ert-deftest agent-ide-table-aligns-right-and-center ()
  (let ((text (agent-ide-table--row '("a" "b" "c") '(5 5 5) '(left center right) nil)))
    (should (equal text "│ a     │   b   │     c │"))))

(ert-deftest agent-ide-table-keeps-links-actionable ()
  (agent-ide-table-test--with "| Link | Value |\n|---|---|\n| [docs](https://example.test) | **bold** |\n" 40
    (let* ((row (car (last (agent-ide-table-test--displays))))
           (pos (string-match "docs" row))
           (map (get-text-property pos 'keymap row)) opened)
      (should (equal (get-text-property pos 'help-echo row) "https://example.test"))
      (cl-letf (((symbol-function 'agent-ide-renderer-open-url) (lambda (url) (setq opened url))))
        (funcall (lookup-key map [mouse-1]) nil))
      (should (equal opened "https://example.test"))
      (should-not (string-match-p "https://" row))
      (should-not (string-match-p "\\*\\*" row)))))

(ert-deftest agent-ide-table-keeps-cell-code-and-border-fonts-independent ()
  (agent-ide-table-test--with "| 内容 | Style |\n|---|---|\n| 中文 `code` | **bold** |\n" 40
    (let ((agent-ide-table--font-family "LXGW WenKai Mono")
          (agent-ide-table--border-font-family "Sarasa Term SC Nerd"))
      (agent-ide-table--layout (car agent-ide-table--tables) t)
      (let* ((rows (agent-ide-table-test--displays))
             (body (nth 2 rows)))
        (dolist (word '("中文" "code" "bold"))
          (should (equal (plist-get (car (get-text-property (string-match word body) 'face body)) :family)
                         "LXGW WenKai Mono")))
        (dolist (row rows)
          (should (equal (plist-get (car (get-text-property 0 'face row)) :family)
                         "Sarasa Term SC Nerd")))))))

(ert-deftest agent-ide-table-ignores-code-and-nontables ()
  (agent-ide-table-test--with "```text\n| A | B |\n|---|---|\n| 1 | 2 |\n```\n\nshell | pipe\nnot a delimiter\n" 40
    (should-not agent-ide-table--tables))
  (agent-ide-table-test--with "| A | B |\n|---|\n" 40
    (should-not agent-ide-table--tables)))

(ert-deftest agent-ide-table-caches-layout-and-reflows-after-resize ()
  (agent-ide-table-test--with "| A | B |\n|---|---|\n| short | long long long long long |\n" 50
    (let* ((table (car agent-ide-table--tables)) (views (overlay-get table 'views)))
      (agent-ide-renderer-render-markdown-region 1 (point-max))
      (agent-ide-table-refresh)
      (should (eq table (car agent-ide-table--tables)))
      (should (eq views (overlay-get table 'views)))
      (cl-letf (((symbol-function 'agent-ide-table--window-state) (lambda () '((nil . 22)))))
        (agent-ide-table-refresh))
      (should-not (eq views (overlay-get table 'views)))
      (should (cl-every (lambda (o) (null (overlay-buffer o))) views)))))

(ert-deftest agent-ide-table-cleans-replaced-output-and-property-changes ()
  (agent-ide-table-test--with "| A | B |\n|---|---|\n| 1 | 2 |\n" 40
    (let ((table (car agent-ide-table--tables)))
      (put-text-property 1 (point-max) 'face 'bold)
      (should (eq table (car agent-ide-table--tables)))
      (let ((views (overlay-get table 'views)))
        (erase-buffer)
        (should-not agent-ide-table--tables)
        (should (cl-every (lambda (o) (null (overlay-buffer o))) views))))))

(ert-deftest agent-ide-table-streaming-updates-preserve-draft ()
  (with-temp-buffer
    (agent-ide-session-mode)
    (let* ((session (agent-ide--make-session :buffer (current-buffer) :directory default-directory
                                           :tool-calls (make-hash-table :test 'equal)))
           (agent-ide-header-icon-animation-interval nil)
           (agent-ide-placeholder-ellipsis-animation-interval nil))
      (setq-local agent-ide--session session)
      (agent-ide-renderer-initialize-buffer session)
      (agent-ide-renderer-create-prompt session)
      (agent-ide-renderer-replace-current-input session "draft untouched")
      (dolist (chunk '("| A | B |\n" "|---|---|\n" "| long cell " "| 中文 |\n"))
        (agent-ide-renderer-append-stream-chunk session 'message chunk))
      (agent-ide-renderer-finish-stream session)
      (should (= 1 (length agent-ide-table--tables)))
      (should (equal "draft untouched" (agent-ide-renderer-current-input session t)))
      (should (string-match-p "long cell" (car (last (agent-ide-table-test--displays)))))
      (should (equal "| A | B |\n|---|---|\n| long cell | 中文 |"
                     (overlay-get (car agent-ide-table--tables) 'source)))
      (agent-ide-renderer-reset-stream session))))

(ert-deftest agent-ide-table-inline-copy-includes-layout ()
  (agent-ide-table-test--with "| A | B |\n|---|---|\n| 1 | 2 |\n" 40
    (let ((text (agent-ide-table-substring 1 (point-max))))
      (should (string-match-p "│ A │ B │" text))
      (should-not (string-match-p "|---|" text)))))

(ert-deftest agent-ide-table-independent-window-layouts ()
  (save-window-excursion
    (with-temp-buffer
      (insert "| A | B |\n|---|---|\n| long long long long | 中文中文中文中文中文中文中文 |\n")
      (let* ((buffer (current-buffer)) (wide (selected-window))
             (narrow (split-window wide 30 'right)))
        (set-window-buffer wide buffer)
        (set-window-buffer narrow buffer)
        (agent-ide-renderer-render-markdown-region 1 (point-max))
        (let* ((table (car agent-ide-table--tables))
               (first (agent-ide-table-test--displays table wide))
               (second (agent-ide-table-test--displays table narrow)))
          (should (= 3 (length first)))
          (should (= 3 (length second)))
          (should-not (equal first second))
          (should (cl-every (lambda (line) (<= (string-width line) (cdr (assq wide (agent-ide-table--window-state)))))
                            (split-string (string-join first "\n") "\n")))
        (agent-ide-table-cleanup))))))

(provide 'agent-ide-table-test)

(ert-deftest agent-ide-table-graphical-cjk-grid ()
  "Check actual glyph widths, not just string-width's logical columns."
  (skip-unless (display-graphic-p))
  (skip-unless (seq-some (lambda (family) (find-font (font-spec :family family)))
                         '("Sarasa Term SC" "Sarasa Mono SC" "Sarasa Term SC Nerd")))
  (save-window-excursion
    (with-temp-buffer
      (let ((window (selected-window)) display coordinates)
        (insert "| English | 中文标题 |\n|---|---|\n| 中文 | abc |\n")
        (agent-ide-table-render 1 (point-max))
        (setq display (agent-ide-table-substring 1 (point-max)))
        (agent-ide-table-cleanup)
        (erase-buffer)
        (insert display)
        (set-window-buffer window (current-buffer))
        (goto-char (point-min))
        (dotimes (_ 3)
          (let ((beg (point)) row)
            (while (re-search-forward "[│├┼┤]" (line-end-position) t)
              (push (car (window-text-pixel-size window beg (match-beginning 0))) row))
            (push (nreverse row) coordinates))
          (forward-line 1))
        (should (= (length (car coordinates)) 3))
        (should (equal (car coordinates) (cadr coordinates)))
        (should (equal (car coordinates) (caddr coordinates)))))))
