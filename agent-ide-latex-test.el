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

(defun agent-ide-latex-test--apply-preview (begin end image)
  "Apply a completed preview IMAGE to BEGIN..END."
  (with-silent-modifications
    (put-text-property begin end 'agent-ide-latex-key "test-key"))
  (agent-ide-latex--apply
   (list (copy-marker begin t) (copy-marker end nil)
         (buffer-substring-no-properties begin end) "test-key")
   image nil))

(ert-deftest agent-ide-latex-point-reveals-source-and-restores-preview ()
  "Moving through a read-only formula preserves source, point and undo state."
  (with-temp-buffer
    (insert "a $x^2$ z")
    (let ((image '(image :type svg :data "test")))
      (agent-ide-latex-test--apply-preview 3 8 image)
      (setq buffer-read-only t buffer-undo-list nil)
      (set-buffer-modified-p nil)
      (dolist (position '(3 5 7))
        (goto-char position)
        (run-hooks 'post-command-hook)
        (should-not (text-property-not-all 3 8 'display nil))
        (should (= (point) position)))
      (goto-char 8)
      (run-hooks 'post-command-hook)
      (should (eq (get-text-property 3 'display) image))
      ;; Enter from the right and leave on the left, too.
      (goto-char 7)
      (run-hooks 'post-command-hook)
      (should-not (get-text-property 3 'display))
      (goto-char 2)
      (run-hooks 'post-command-hook)
      (should (eq (get-text-property 3 'display) image))
      (should-not agent-ide-latex--revealed)
      (should-not (buffer-modified-p))
      (should-not buffer-undo-list)
      (should (equal (buffer-substring-no-properties 1 (point-max)) "a $x^2$ z")))))

(ert-deftest agent-ide-latex-point-distinguishes-adjacent-shared-images ()
  "Adjacent identical formulas reveal independently despite sharing an image."
  (with-temp-buffer
    (insert "\\(x\\)\\(x\\)")
    (let ((image '(image :type svg :data "shared")))
      (agent-ide-latex-test--apply-preview 1 6 image)
      (agent-ide-latex-test--apply-preview 6 11 image)
      (goto-char 1)
      (run-hooks 'post-command-hook)
      (should-not (get-text-property 1 'display))
      (should (eq (get-text-property 6 'display) image))
      (goto-char 6)
      (run-hooks 'post-command-hook)
      (should (eq (get-text-property 1 'display) image))
      (should-not (get-text-property 6 'display))
      (goto-char (point-max))
      (run-hooks 'post-command-hook)
      (should (eq (get-text-property 6 'display) image)))))

(ert-deftest agent-ide-latex-completion-at-point-keeps-source-visible ()
  "An asynchronous result at point stays revealed, including replacement images."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "$$x\n+y$$ tail")
      (goto-char 4)
      (dolist (image '((image :type svg :data "first")
                       (image :type svg :data "updated")))
        (agent-ide-latex-test--apply-preview 1 9 image)
        (should-not (text-property-not-all 1 9 'display nil))
        (should (= (point) 4)))
      (goto-char 9)
      (run-hooks 'post-command-hook)
      (should (equal (get-text-property 1 'display)
                     '(image :type svg :data "updated"))))))

(ert-deftest agent-ide-latex-revealed-preview-survives-narrowing ()
  (with-temp-buffer
    (insert "$x$\ndraft")
    (let ((image '(image :type svg :data "test")))
      (agent-ide-latex-test--apply-preview 1 4 image)
      (goto-char 2)
      (run-hooks 'post-command-hook)
      (narrow-to-region 5 (point-max))
      (goto-char (point-max))
      (run-hooks 'post-command-hook)
      (should (= (point-min) 5))
      (should (= (point) (point-max)))
      (widen)
      (should (eq (get-text-property 1 'display) image)))))

(ert-deftest agent-ide-latex-disabling-preview-clears-revealed-state ()
  (with-temp-buffer
    (insert "$x$\ndraft")
    (setq-local agent-ide--session
                (agent-ide--make-session :buffer (current-buffer)
                 :input-prompt-start-marker (copy-marker 5)))
    (agent-ide-latex-test--apply-preview 1 4 '(image :type svg :data "test"))
    (goto-char 2)
    (run-hooks 'post-command-hook)
    (let ((agent-ide-latex-preview nil))
      (agent-ide-preview-latex))
    (goto-char (point-max))
    (run-hooks 'post-command-hook)
    (should-not agent-ide-latex--revealed)
    (should-not (text-property-not-all 1 (point-max) 'display nil))
    (should-not (get-text-property 1 'agent-ide-latex-preview))))

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
                (should (string-match-p "Undefined control sequence" (get-text-property 1 'help-echo)))
                (let ((record (gethash (get-text-property 1 'agent-ide-latex-key)
                                       agent-ide-latex--cache)))
                  (should (string-match-p "notARealCommand" (plist-get record :log))))
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
          (should (string-match-p "timed out" (get-text-property 1 'help-echo)))
          (should (eq (plist-get (gethash (get-text-property 1 'agent-ide-latex-key)
                                          agent-ide-latex--cache) :status) 'failed)))))))

(ert-deftest agent-ide-latex-xelatex-settings-and-font-cache ()
  "XeLaTeX needs the SVG converter, and font changes invalidate cached images."
  (let ((agent-ide-latex-process 'xelatex)
        (agent-ide-latex-preview t))
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (type) (eq type 'svg)))
              ((symbol-function 'executable-find)
               (lambda (name) (member name '("xelatex" "dvisvgm")))))
      (let* ((agent-ide-latex-cjk-font "Font A")
             (first (agent-ide-latex--settings))
             (agent-ide-latex-cjk-font "Font B"))
        (should first)
        (should-not (equal first (agent-ide-latex--settings)))))
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
              ((symbol-function 'executable-find) (lambda (name) (equal name "xelatex"))))
      (should-not (agent-ide-latex--settings)))))

(ert-deftest agent-ide-latex-xelatex-chinese-integration ()
  "Render the reported Chinese formulas, retaining their original source."
  (skip-unless (and (executable-find "xelatex") (executable-find "dvisvgm")
                    (image-type-available-p 'svg)))
  (agent-ide-latex-test--isolated
    (let ((agent-ide-latex-process 'xelatex)
          (agent-ide-latex-preview t)
          (source (concat "对于 **\\(e^{x^y}\\)**，一般不能进一步化简。要区分括号的位置：\n"
                          "\\[\n\\boxed{e^{(x^y)}}\\qquad\\text{先算 }x^y\\text{，再作为 }e\\text{ 的指数};\n\\]\n"
                          "\\[\n\\boxed{(e^x)^y=e^{xy}}\\qquad\\text{幂的乘方，指数相乘}.\n\\]")))
      (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
        (with-temp-buffer
          (insert source)
          (agent-ide-renderer-render-markdown-region 1 (point-max))
          (while agent-ide-latex--process (accept-process-output nil 0.05))
          (should (equal source (buffer-substring-no-properties 1 (point-max))))
          (let ((fragments (agent-ide-latex-prepare 1 (point-max))))
            (should (= (length fragments) 3))
            (dolist (fragment fragments)
              (let ((image (get-text-property (car fragment) 'display)))
                (should (eq (car-safe image) 'image))
                (should (eq (plist-get (cdr image) :type) 'svg))
                (should (string-match-p "<path" (plist-get (cdr image) :data)))))))))))

(ert-deftest agent-ide-latex-xelatex-missing-font-recovery ()
  "Keep font diagnostics and retry with a different font without restarting."
  (skip-unless (and (executable-find "xelatex") (executable-find "dvisvgm")
                    (image-type-available-p 'svg)))
  (agent-ide-latex-test--isolated
    (let ((agent-ide-latex-process 'xelatex)
          (agent-ide-latex-preview t)
          (working-font agent-ide-latex-cjk-font)
          (agent-ide-latex-cjk-font "Agent IDE Missing Font 12345"))
      (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
        (with-temp-buffer
          (insert "$\\text{中文}$")
          (agent-ide-renderer-render-markdown-region 1 (point-max))
          (while agent-ide-latex--process (accept-process-output nil 0.05))
          (should-not (get-text-property 1 'display))
          (should (string-match-p "fontspec" (get-text-property 1 'help-echo)))
          (should (string-match-p "cannot be found" (get-text-property 1 'help-echo)))
          (goto-char 1)
          (save-window-excursion
            (unwind-protect
                (progn
                  (agent-ide-latex-show-error)
                  (with-current-buffer "*Agent IDE Formula Error*"
                    (should (string-match-p "cannot be found" (buffer-string)))))
              (when-let* ((buffer (get-buffer "*Agent IDE Formula Error*")))
                (kill-buffer buffer))))
          (let ((agent-ide-latex-cjk-font working-font))
            (agent-ide-renderer-render-markdown-region 1 (point-max))
            (while agent-ide-latex--process (accept-process-output nil 0.05)))
          (should (eq (car-safe (get-text-property 1 'display)) 'image))
          (should (= (hash-table-count agent-ide-latex--cache) 2)))))))

(ert-deftest agent-ide-latex-refresh-isolates-transcript-markdown-regions ()
  "An unmatched backtick in tool output must not swallow later replies."
  (agent-ide-latex-test--isolated
    (cl-letf (((symbol-function 'agent-ide-latex--settings)
               (lambda () '(xelatex 1.0 "#000000" "Songti SC")))
              ((symbol-function 'agent-ide-latex--start-next) #'ignore))
      (with-temp-buffer
        (insert "Tool output: `unfinished $command\n")
        (agent-ide-renderer-render-markdown-region 1 (point-max))
        (insert "\nAssistant\n\n")
        (let ((start (point)))
          (insert "\\[\\text{中文} + x^2\\]\n")
          (agent-ide-renderer-render-markdown-region start (point)))
        (insert "\nAssistant\n\n")
        (let ((start (point)))
          (insert "$$y^2$$\n")
          (agent-ide-renderer-render-markdown-region start (point)))
        (let ((boundary (point)))
          (insert "Draft $z$")
          (setq-local agent-ide--session
                      (agent-ide--make-session :buffer (current-buffer)
                       :input-prompt-start-marker (copy-marker boundary)))
          (agent-ide-preview-latex)
          (goto-char 1)
          (search-forward "\\[")
          (should (get-text-property (- (point) 2) 'agent-ide-latex-key))
          (search-forward "$$")
          (should (get-text-property (- (point) 2) 'agent-ide-latex-key))
          (should (= (length agent-ide-latex--queue) 2))
          (should-not (text-property-not-all boundary (point-max) 'agent-ide-latex-key nil)))))))

(ert-deftest agent-ide-latex-refresh-upgrades-legacy-buffer ()
  "Hotloaded previews reuse legacy math properties, preserving source and point."
  (agent-ide-latex-test--isolated
    (cl-letf (((symbol-function 'agent-ide-latex--settings)
               (lambda () '(xelatex 1.0 "#000000" "Songti SC")))
              ((symbol-function 'agent-ide-latex--start-next) #'ignore))
      (with-temp-buffer
        (insert "Raw tool output `\n\nAssistant\n\n")
        (let ((start (point)))
          (insert "\\[\\text{中文}\\]")
          (put-text-property start (point) 'agent-ide-latex t)
          (setq-local agent-ide--session
                      (agent-ide--make-session :buffer (current-buffer)
                       :input-prompt-start-marker (copy-marker (point))))
          (let ((source (buffer-string)) (position (point)))
            (agent-ide-preview-latex)
            (should (= position (point)))
            (should (equal (substring-no-properties source)
                           (buffer-substring-no-properties 1 (point-max)))))
          (should (get-text-property start 'agent-ide-latex-key))
          (should (= (length agent-ide-latex--queue) 1)))))))

(defun agent-ide-latex-test--queued-sources ()
  (mapcar (lambda (key) (plist-get (gethash key agent-ide-latex--cache) :source))
          agent-ide-latex--queue))

(ert-deftest agent-ide-latex-live-output-precedes-history-backlog ()
  "New formulas and reused history formulas get priority without duplication."
  (agent-ide-latex-test--isolated
    (cl-letf (((symbol-function 'agent-ide-latex--settings)
               (lambda () '(xelatex 1.0 "#000000" "Songti SC")))
              ((symbol-function 'agent-ide-latex--start-next) #'ignore))
      (with-temp-buffer
        (insert "$oldA$ $shared$ $oldB$")
        (let ((agent-ide-latex--background-render t))
          (agent-ide-renderer-render-markdown-region 1 (point-max))))
      (with-temp-buffer
        (insert "$liveA$ $shared$")
        (agent-ide-renderer-render-markdown-region 1 (point-max))
        ;; Streaming rerenders the existing prefix on every chunk.
        (insert " $liveB$")
        (agent-ide-renderer-render-markdown-region 1 (point-max)))
      (should (equal (agent-ide-latex-test--queued-sources)
                     '("$liveA$" "$shared$" "$liveB$" "$oldA$" "$oldB$")))
      (should (= (hash-table-count agent-ide-latex--cache) 5)))))

(ert-deftest agent-ide-latex-promotes-already-marked-formula ()
  "A cache key installed by refresh must not prevent foreground promotion."
  (agent-ide-latex-test--isolated
    (cl-letf (((symbol-function 'agent-ide-latex--settings)
               (lambda () '(xelatex 1.0 "#000000" "Songti SC")))
              ((symbol-function 'agent-ide-latex--start-next) #'ignore))
      (with-temp-buffer
        (insert "$old$")
        (let ((agent-ide-latex--background-render t))
          (agent-ide-renderer-render-markdown-region 1 (point-max))))
      (with-temp-buffer
        (insert "$new$")
        (let ((agent-ide-latex--background-render t))
          (agent-ide-renderer-render-markdown-region 1 (point-max)))
        (should (get-text-property 1 'agent-ide-latex-key))
        (agent-ide-renderer-render-markdown-region 1 (point-max))
        (should (equal (agent-ide-latex-test--queued-sources) '("$new$" "$old$")))
        (let ((record (gethash (car agent-ide-latex--queue) agent-ide-latex--cache)))
          (should (= (length (plist-get record :waiters)) 1)))))))

(ert-deftest agent-ide-latex-replayed-history-stays-in-background ()
  (agent-ide-latex-test--isolated
    (cl-letf (((symbol-function 'agent-ide-latex--settings)
               (lambda () '(xelatex 1.0 "#000000" "Songti SC")))
              ((symbol-function 'agent-ide-latex--start-next) #'ignore))
      (with-temp-buffer
        (setq-local agent-ide--session (agent-ide--make-session :buffer (current-buffer)))
        (agent-ide--session-metadata-put agent-ide--session :replaying t)
        (insert "$history$")
        (agent-ide-renderer-render-markdown-region 1 (point-max))
        (should-not (plist-get (gethash (car agent-ide-latex--queue) agent-ide-latex--cache)
                               :foreground)))
      (with-temp-buffer
        (insert "$live$")
        (agent-ide-renderer-render-markdown-region 1 (point-max)))
      (should (equal (agent-ide-latex-test--queued-sources) '("$live$" "$history$"))))))

(ert-deftest agent-ide-latex-hex-colors-ignore-terminal-palette ()
  (let ((agent-ide-latex-preview t)
        (agent-ide-latex-process 'xelatex))
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
              ((symbol-function 'executable-find) (lambda (&rest _) t))
              ((symbol-function 'face-foreground) (lambda (&rest _) "#34494a"))
              ((symbol-function 'color-values) (lambda (&rest _) '(0 0 65535))))
      (should (equal (nth 2 (agent-ide-latex--settings)) "#34494a"))))
  (should (equal (agent-ide-latex--hex-rgb "#fff") '(1.0 1.0 1.0)))
  (should (equal (agent-ide-latex--hex-rgb "#000000000000") '(0.0 0.0 0.0)))
  (should (equal (agent-ide-latex--hex-rgb "#abc") (agent-ide-latex--hex-rgb "#aabbcc")))
  (should-not (agent-ide-latex--hex-rgb "Transparent")))

(ert-deftest agent-ide-latex-refresh-migrates-old-monochrome-cache ()
  "Existing blue SVGs acquire the correct color without recompiling history."
  (agent-ide-latex-test--isolated
    (let* ((source "$x^2$")
           (settings '(xelatex 1.0 "#34494a" "Songti SC"))
           (old-key (secure-hash 'sha256 (prin1-to-string (cons source settings))))
           (svg "<svg xmlns='http://www.w3.org/2000/svg'><g fill='#00f'><path d='M0 0'/></g></svg>")
           (old-image `(image :type svg :data ,svg :ascent center)))
      (puthash old-key (list :status 'done :source source :settings settings :image old-image)
               agent-ide-latex--cache)
      (cl-letf (((symbol-function 'agent-ide-latex--settings) (lambda () settings))
                ((symbol-function 'agent-ide-latex--start-next)
                 (lambda () (should-not agent-ide-latex--queue))))
        (with-temp-buffer
          (insert source)
          (add-text-properties 1 (point-max)
                               (list 'agent-ide-latex t 'agent-ide-latex-key old-key 'display old-image))
          (setq-local agent-ide--session
                      (agent-ide--make-session :buffer (current-buffer)
                       :input-prompt-start-marker (copy-marker (point-max))))
          (agent-ide-preview-latex)
          (should (equal (get-text-property 1 'agent-ide-latex-key)
                         (agent-ide-latex--cache-key source settings)))
          (should (string-match-p "fill='#34494a'" (plist-get (cdr (get-text-property 1 'display)) :data)))
          (should (equal (buffer-substring-no-properties 1 (point-max)) source)))
        (should (= (hash-table-count agent-ide-latex--cache) 1))
        (should-not (gethash old-key agent-ide-latex--cache))
        (should (equal (plist-get (cdr old-image) :data) svg))))))

(ert-deftest agent-ide-latex-cache-migration-preserves-explicit-colors ()
  (agent-ide-latex-test--isolated
    (let* ((source "$\\color{blue}x$")
           (settings '(xelatex 1.0 "#34494a" "Songti SC"))
           (old-key (secure-hash 'sha256 (prin1-to-string (cons source settings))))
           (record (list :status 'done :source source :settings settings
                         :image '(image :type svg :data "<svg><g fill='#00f'/></svg>"))))
      (puthash old-key record agent-ide-latex--cache)
      (should (= (agent-ide-latex--upgrade-color-cache) 0))
      (should (eq (gethash old-key agent-ide-latex--cache) record))
      (should-not (gethash (agent-ide-latex--cache-key source settings) agent-ide-latex--cache)))))

(ert-deftest agent-ide-latex-exact-svg-color-integration ()
  "Real batch workers preserve theme RGB and explicitly colored formula parts."
  (skip-unless (and (executable-find "latex") (executable-find "xelatex")
                    (executable-find "dvisvgm") (image-type-available-p 'svg)))
  (agent-ide-latex-test--isolated
    (let ((agent-ide-latex-preview t))
      (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
                ((symbol-function 'face-foreground) (lambda (&rest _) "#34494a")))
        (dolist (agent-ide-latex-process '(dvisvgm xelatex))
          (with-temp-buffer
            (insert "$x+{\\color{red}y}$")
            (agent-ide-renderer-render-markdown-region 1 (point-max))
            (while agent-ide-latex--process (accept-process-output nil 0.05))
            (let ((data (plist-get (cdr (get-text-property 1 'display)) :data)))
              (should (stringp data))
              (should (string-match-p "fill=['\"]#34494a['\"]" data))
              (should (string-match-p "fill=['\"]#f00['\"]" data)))))))))

(provide 'agent-ide-latex-test)
;;; agent-ide-latex-test.el ends here
