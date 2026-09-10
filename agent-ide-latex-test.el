;;; agent-ide-latex-test.el --- Formula preview tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-ide-renderer)

(defmacro agent-ide-latex-test--isolated (&rest body)
  (declare (indent 0) (debug t))
  `(let ((agent-ide-latex--cache (make-hash-table :test 'equal))
         (agent-ide-latex--queue nil)
         (agent-ide-latex--process nil))
     ,@body))

(defun agent-ide-latex-test--fragments (text)
  (with-temp-buffer
    (insert text)
    (mapcar #'caddr (agent-ide-latex-prepare (point-min) (point-max)))))

(ert-deftest agent-ide-latex-delimiters ()
  (should (equal (agent-ide-latex-test--fragments
                  "Inline $x^2$ and \\(a_b\\); display $$a\n+b$$ or \\[\\frac{1}{2}\\].")
                 '("$x^2$" "\\(a_b\\)" "$$a\n+b$$" "\\[\\frac{1}{2}\\]"))))

(ert-deftest agent-ide-latex-skips-code-currency-and-escapes ()
  (dolist (text '("`$x$`" "`` $x$ ``" "```latex\n$x$\n```"
                  "~~~latex\n$x$\n~~~" "```latex\n$x$" "`$x$"
                  "\\$x\\$" "$5 and $10" "$ x $" "$a\nb$"))
    (should-not (agent-ide-latex-test--fragments text)))
  (should (equal (agent-ide-latex-test--fragments "```latex\n$x$\n```\n$y$")
                 '("$y$"))))

(ert-deftest agent-ide-latex-region-may-end-inside-fence-line ()
  (with-temp-buffer
    (insert "```latex\n$x$\n```")
    (should-not (agent-ide-latex-prepare 1 4))))

(ert-deftest agent-ide-latex-streaming-protects-source-and-deduplicates ()
  (agent-ide-latex-test--isolated
    (cl-letf (((symbol-function 'agent-ide-latex--settings)
               (lambda () '(dvisvgm 1.0 "#000000")))
              ((symbol-function 'agent-ide-latex--start-next) #'ignore))
      (with-temp-buffer
        (insert "\\[x_a_b")
        (agent-ide-renderer-render-markdown-region 1 (point-max))
        (should-not agent-ide-latex--queue)
        (should-not (text-property-not-all 1 (point-max) 'display nil))
        (goto-char (point-max))
        (insert "\\]")
        (agent-ide-renderer-render-markdown-region 1 (point-max))
        (agent-ide-renderer-render-markdown-region 1 (point-max))
        (should (= (length agent-ide-latex--queue) 1))
        (should (= (length (plist-get (gethash (car agent-ide-latex--queue)
                                              agent-ide-latex--cache) :waiters)) 1))
        (should (equal (buffer-substring-no-properties 1 (point-max)) "\\[x_a_b\\]"))
        (should-not (text-property-not-all 1 (point-max) 'display nil))))))

(ert-deftest agent-ide-latex-stale-result-does-not-overwrite-edits ()
  (with-temp-buffer
    (insert "$x$")
    (put-text-property 1 4 'agent-ide-latex-key "key")
    (let ((waiter (list (copy-marker 1) (copy-marker 4) "$x$" "key")))
      (goto-char 2)
      (insert "y")
      (agent-ide-latex--apply waiter '(image :type svg :data "unused") nil)
      (should-not (text-property-not-all 1 (point-max) 'display nil))
      (should-not (marker-buffer (car waiter))))))

(ert-deftest agent-ide-latex-refresh-retains-pending-conversion-and-draft ()
  (agent-ide-latex-test--isolated
    (cl-letf (((symbol-function 'agent-ide-latex--settings)
               (lambda () '(dvisvgm 1.0 "#000000")))
              ((symbol-function 'agent-ide-latex--start-next) #'ignore))
      (with-temp-buffer
        (insert "$x$\nDraft $y$")
        (setq-local agent-ide--session
                    (agent-ide--make-session :buffer (current-buffer)
                     :input-prompt-start-marker (copy-marker 5)))
        (agent-ide-renderer-render-markdown-region 1 5)
        (let ((key (car agent-ide-latex--queue)))
          (agent-ide-preview-latex)
          (should (gethash key agent-ide-latex--cache))
          (should (= (length agent-ide-latex--queue) 1))
          (should-not (get-text-property 11 'agent-ide-latex-key))
          (should (equal (buffer-substring-no-properties 5 (point-max)) "Draft $y$")))))))

(ert-deftest agent-ide-latex-org-worker-integration ()
  "Exercise real Org/TeX conversions, failure recovery and shared images."
  (skip-unless (and (executable-find "latex") (executable-find "dvisvgm")
                    (image-type-available-p 'svg)))
  (agent-ide-latex-test--isolated
    (let ((agent-ide-latex-process 'dvisvgm)
          (agent-ide-latex-preview t)
          (buffer-a (generate-new-buffer " *formula-test-a*"))
          (buffer-b (generate-new-buffer " *formula-test-b*"))
          (updates 0))
      (unwind-protect
          (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
            (dolist (buffer (list buffer-a buffer-b))
              (with-current-buffer buffer
                (insert "$\\notARealCommand$ then $x^2$ draft")
                (add-hook 'agent-ide-latex-updated-functions (lambda () (cl-incf updates)) nil t)
                (agent-ide-renderer-render-markdown-region 1 (point-max))))
            (let ((deadline (+ (float-time) 45)))
              (while (and agent-ide-latex--process (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should-not agent-ide-latex--process)
            (should (= (hash-table-count agent-ide-latex--cache) 2))
            (should (= updates 4))
            (dolist (buffer (list buffer-a buffer-b))
              (with-current-buffer buffer
                (should-not (get-text-property 1 'display))
                (should (string-match-p "failed" (get-text-property 1 'help-echo)))
                (goto-char 1)
                (search-forward "$x^2$")
                (let ((image (get-text-property (1- (point)) 'display)))
                  (should (eq (car-safe image) 'image))
                  (should (eq (plist-get (cdr image) :type) 'svg))
                  (should (string-match-p "<svg" (plist-get (cdr image) :data))))
                (should (equal (buffer-substring-no-properties 1 (point-max))
                               "$\\notARealCommand$ then $x^2$ draft")))))
        (when (process-live-p agent-ide-latex--process)
          (setq agent-ide-latex--queue nil)
          (delete-process agent-ide-latex--process))
        (kill-buffer buffer-a)
        (kill-buffer buffer-b)))))

(ert-deftest agent-ide-latex-completion-preserves-narrowing-and-point ()
  (with-temp-buffer
    (insert "$x$\ndraft")
    (put-text-property 1 4 'agent-ide-latex-key "key")
    (let ((waiter (list (copy-marker 1) (copy-marker 4) "$x$" "key"))
          (image '(image :type svg :data "test")))
      (narrow-to-region 5 (point-max))
      (goto-char (point-max))
      (agent-ide-latex--apply waiter image nil)
      (should (= (point-min) 5))
      (should (= (point) (point-max)))
      (widen)
      (should (equal (get-text-property 1 'display) image)))))

(ert-deftest agent-ide-latex-org-worker-png-integration ()
  (skip-unless (and (executable-find "latex") (executable-find "dvipng")
                    (image-type-available-p 'png)))
  (agent-ide-latex-test--isolated
    (let ((agent-ide-latex-process 'dvipng)
          (agent-ide-latex-preview t))
      (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
        (with-temp-buffer
          (insert "\\[\\frac{1}{2}+\\sum_{i=1}^{n}i\\]")
          (agent-ide-renderer-render-markdown-region 1 (point-max))
          (while agent-ide-latex--process (accept-process-output nil 0.05))
          (let ((image (get-text-property 1 'display)))
            (should (eq (car-safe image) 'image))
            (should (eq (plist-get (cdr image) :type) 'png))))))))

(ert-deftest agent-ide-latex-worker-timeout-cleans-up ()
  (agent-ide-latex-test--isolated
    (let ((agent-ide-latex-timeout 0.05)
          (start-process-function (symbol-function 'make-process))
          directory)
      (cl-letf (((symbol-function 'agent-ide-latex--settings)
                 (lambda () '(dvisvgm 1.0 "#000000")))
                ((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq directory default-directory)
                   (apply start-process-function
                          (plist-put args :command '("/bin/sleep" "10"))))))
        (with-temp-buffer
          (insert "$x$")
          (agent-ide-renderer-render-markdown-region 1 (point-max))
          (while agent-ide-latex--process (accept-process-output nil 0.05))
          (should-not (get-text-property 1 'display))
          (should-not (file-exists-p directory))
          (should (eq (plist-get (gethash (get-text-property 1 'agent-ide-latex-key)
                                          agent-ide-latex--cache) :status) 'failed)))))))

(provide 'agent-ide-latex-test)
;;; agent-ide-latex-test.el ends here
