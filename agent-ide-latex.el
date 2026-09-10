;;; agent-ide-latex.el --- Org-backed formula previews -*- lexical-binding: t; -*-

;;; Commentary:
;; Recognize Markdown math and render it using Org in an isolated batch Emacs.
;; Display properties preserve source text and work in inline viewport strings.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'image)
(require 'json)
(require 'subr-x)
(require 'agent-ide-core)

(defvar org-preview-latex-process-alist)
(declare-function org-create-formula-image "org")

(defcustom agent-ide-latex-preview t
  "Whether complete math fragments are automatically previewed."
  :type 'boolean :group 'agent-ide)

(defcustom agent-ide-latex-process 'dvisvgm
  "Org preview process used for formulas."
  :type '(choice (const dvisvgm) (const dvipng)) :group 'agent-ide)

(defcustom agent-ide-latex-scale 1.0
  "Scale applied to Org's formula previews."
  :type 'number :group 'agent-ide)

(defcustom agent-ide-latex-timeout 20
  "Maximum seconds allowed for a background formula conversion."
  :type 'number :group 'agent-ide)

(defvar-local agent-ide-latex-updated-functions nil
  "Functions called with no arguments after an asynchronous preview update.
They run in the buffer containing the formula source.")

(defconst agent-ide-latex--library
  (expand-file-name "agent-ide-latex.el"
                    (file-name-directory (or load-file-name buffer-file-name))))
(defvar agent-ide-latex--cache (make-hash-table :test 'equal))
(defvar agent-ide-latex--queue nil)
(defvar agent-ide-latex--process nil)

(defun agent-ide-latex--escaped-p (position)
  "Return non-nil if the delimiter at POSITION is escaped."
  (let ((count 0))
    (while (and (> position (point-min)) (eq (char-before position) ?\\))
      (setq count (1+ count) position (1- position)))
    (cl-oddp count)))

(defun agent-ide-latex--closing-end (delimiter limit single-dollar)
  "Find a closing DELIMITER before LIMIT, applying SINGLE-DOLLAR rules."
  (catch 'found
    (while (search-forward delimiter limit t)
      (let ((start (- (point) (length delimiter))))
        (unless (or (agent-ide-latex--escaped-p start)
                    (and single-dollar
                         (or (memq (char-before start) '(?\s ?\t ?\n ?$))
                             (eq (char-after) ?$)
                             (and (char-after) (<= ?0 (char-after) ?9)))))
          (throw 'found (point)))))))

(defun agent-ide-latex-prepare (start end)
  "Protect math in START..END from Markdown and return complete fragments.
Each fragment is (START END SOURCE).  Skip fenced and inline code, including
unfinished code fences.  Dollar math stays on one line unless it uses $$."
  (save-excursion
    (save-match-data
      (goto-char start)
      (let (fragments)
        (while (re-search-forward
                (rx (or (one-or-more "`") (>= 3 "~") "$$" "$" "\\(" "\\[")) end t)
          (let* ((begin (match-beginning 0))
                 (token (match-string-no-properties 0))
                 (body (point)))
            (cond
             ((agent-ide-latex--escaped-p begin))
             ((and (memq (aref token 0) '(?` ?~))
                   (>= (length token) 3)
                   (string-match-p "\\`[ \t]*\\'"
                                   (buffer-substring-no-properties
                                    (line-beginning-position) begin)))
              (forward-line 1)
              (unless (and (<= (point) end) (re-search-forward
					     (concat "^[ \t]*" (regexp-quote (substring token 0 1))
						     "\\{" (number-to-string (length token)) ",\\}[ \t]*$")
					     end t))
                (goto-char end)))
             ((eq (aref token 0) ?`)
              (unless (search-forward token end t) (goto-char end)))
             ((eq (aref token 0) ?~))
             (t
              (let* ((single (equal token "$"))
                     (closing (pcase token ("\\(" "\\)") ("\\[" "\\]") (_ token)))
                     (limit (if single (min end (line-end-position)) end))
                     (finish
                      (unless (and single (memq (char-after body) '(nil ?\s ?\t ?\n ?$)))
                        (agent-ide-latex--closing-end closing limit single))))
                (when (or finish (not single))
                  (let ((bound (or finish end)) (inhibit-read-only t))
                    ;; Remove Markdown styling from a previously incomplete fragment.
                    (unless (get-text-property begin 'agent-ide-latex-key)
                      (remove-text-properties begin bound '(display nil face nil)))
                    (put-text-property begin bound 'agent-ide-latex t)))
                (if finish
                    (push (list begin finish
                                (buffer-substring-no-properties begin finish)) fragments)
                  (goto-char (if single body end))))))))
        (nreverse fragments)))))

(defun agent-ide-latex--settings ()
  "Return preview settings, or nil if previews cannot be displayed."
  (when (and agent-ide-latex-preview (display-images-p)
             (executable-find "latex")
             (executable-find (symbol-name agent-ide-latex-process))
             (image-type-available-p (if (eq agent-ide-latex-process 'dvisvgm) 'svg 'png)))
    (let ((rgb (or (ignore-errors (color-values (face-foreground 'default nil t)))
                   '(0 0 0))))
      (list agent-ide-latex-process agent-ide-latex-scale
            (apply #'format "#%02x%02x%02x" (mapcar (lambda (v) (/ v 257)) rgb))))))

(defun agent-ide-latex--apply (waiter image error-message)
  "Apply IMAGE or ERROR-MESSAGE to a still-valid WAITER."
  (pcase-let ((`(,begin ,end ,source ,key) waiter))
    (when-let* ((buffer (marker-buffer begin))
                ((eq buffer (marker-buffer end))))
      (with-current-buffer buffer
	(save-restriction
          (widen)
          (when (and (<= begin end)
                     (equal source (buffer-substring-no-properties begin end))
                     (equal key (get-text-property begin 'agent-ide-latex-key)))
            (with-silent-modifications
              (if image
                  (put-text-property begin end 'display image)
		(remove-text-properties begin end '(display nil)))
              (put-text-property begin end 'help-echo
				 (if image source (concat "Formula preview: " error-message))))
            (condition-case err
		(run-hooks 'agent-ide-latex-updated-functions)
              (error (message "Agent IDE preview refresh: %s" (error-message-string err))))))))
    (set-marker begin nil)
    (set-marker end nil)))

(defun agent-ide-latex-render (fragments)
  "Schedule previews of FRAGMENTS, sharing conversions across buffers."
  (when-let* ((settings (agent-ide-latex--settings)))
    (dolist (fragment fragments)
      (pcase-let* ((`(,begin ,end ,source) fragment)
                   (key (secure-hash 'sha256 (prin1-to-string (cons source settings)))))
        (unless (equal key (get-text-property begin 'agent-ide-latex-key))
          (let* ((record (gethash key agent-ide-latex--cache))
                 (waiter (list (copy-marker begin t) (copy-marker end nil) source key)))
            (with-silent-modifications
              (put-text-property begin end 'agent-ide-latex-key key))
            (if (memq (plist-get record :status) '(done failed))
                (agent-ide-latex--apply waiter (plist-get record :image) (plist-get record :error))
              (unless record
                (setq record (list :status 'queued :source source :settings settings))
                (setq agent-ide-latex--queue (nconc agent-ide-latex--queue (list key))))
              (setq record (plist-put record :waiters (cons waiter (plist-get record :waiters))))
              (puthash key record agent-ide-latex--cache))))))
    (agent-ide-latex--start-next)))

(defun agent-ide-latex--complete (key directory output log-buffer success)
  "Finish conversion KEY using OUTPUT and SUCCESS, then clean DIRECTORY."
  (let* ((record (gethash key agent-ide-latex--cache))
         (type (if (eq (car (plist-get record :settings)) 'dvisvgm) 'svg 'png))
         (image (and success (file-exists-p output)
                     (ignore-errors
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally output)
                         (create-image (buffer-string) type t :ascent 'center)))))
         (failure "conversion failed; source retained"))
    (setq record (plist-put record :status (if image 'done 'failed)))
    (setq record (plist-put record :image image))
    (setq record (plist-put record :error failure))
    (dolist (waiter (plist-get record :waiters))
      (agent-ide-latex--apply waiter image failure))
    (setq record (plist-put record :waiters nil))
    (puthash key record agent-ide-latex--cache)
    (when (buffer-live-p log-buffer) (kill-buffer log-buffer))
    (ignore-errors (delete-directory directory t))
    (setq agent-ide-latex--process nil)
    (agent-ide-latex--start-next)))

(defun agent-ide-latex--start-next ()
  "Start the next Org worker, allowing only one conversion at a time."
  (when (and (not agent-ide-latex--process) agent-ide-latex--queue)
    (let* ((key (pop agent-ide-latex--queue))
           (record (gethash key agent-ide-latex--cache))
           (settings (plist-get record :settings))
           (directory (file-truename (make-temp-file "agent-ide-latex-" t)))
           (default-directory (file-name-as-directory directory))
           (input (expand-file-name "request.json" directory))
           (output (expand-file-name (if (eq (car settings) 'dvisvgm) "formula.svg" "formula.png") directory))
           (log-buffer (generate-new-buffer " *agent-ide-latex-worker*"))
           (process-environment
            (cons (concat "PATH=" (mapconcat #'identity (delq nil (copy-sequence exec-path))
                                             path-separator))
                  process-environment))
           timer)
      (condition-case err
          (progn
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region
               (json-serialize `((source . ,(plist-get record :source))
                                 (output . ,output) (process . ,(symbol-name (car settings)))
                                 (scale . ,(nth 1 settings)) (foreground . ,(nth 2 settings))))
               nil input nil 'silent))
            (setq agent-ide-latex--process
                  (make-process
                   :name "agent-ide-latex" :buffer log-buffer :noquery t
                   :connection-type 'pipe
                   :command (list (expand-file-name invocation-name invocation-directory)
                                  "-Q" "--batch" "-L" (file-name-directory agent-ide-latex--library)
                                  "-l" agent-ide-latex--library
                                  "--eval" (format "(agent-ide-latex--worker %S)" input))
                   :sentinel
                   (lambda (process _event)
                     (when (memq (process-status process) '(exit signal))
                       (when timer (cancel-timer timer))
                       (agent-ide-latex--complete
                        key directory output log-buffer (= (process-exit-status process) 0))))))
            (setq timer (run-at-time
                         agent-ide-latex-timeout nil
                         (lambda (process)
                           (when (process-live-p process) (delete-process process)))
                         agent-ide-latex--process)))
        (error
         (message "Agent IDE formula preview: %s" (error-message-string err))
         (agent-ide-latex--complete key directory output log-buffer nil))))))

(defun agent-ide-latex--worker (request-file)
  "Render REQUEST-FILE using Org in a separate, clean Emacs process."
  (require 'org)
  (require 'ox-latex)
  (let* ((request (with-temp-buffer
                    (insert-file-contents request-file)
                    (json-parse-buffer :object-type 'alist)))
         (type (intern (alist-get 'process request)))
         (default-directory (file-name-directory request-file))
         (temporary-file-directory default-directory)
         (shell-file-name "/bin/sh")
         (shell-command-switch "-c")
         (process-environment (append (list "openin_any=p" "openout_any=p" "shell_escape=f"
                                            (concat "TEXMFOUTPUT=" default-directory))
                                      process-environment))
         (org-preview-latex-process-alist (copy-tree org-preview-latex-process-alist))
         (entry (assq type org-preview-latex-process-alist)))
    ;; Reuse Org's preamble, packages, colors, scale and image conversion.
    (unless (memq type '(dvisvgm dvipng)) (error "Unsupported formula preview process"))
    (setcdr entry (plist-put (cdr entry) :latex-compiler
                             '("latex -no-shell-escape -halt-on-error -interaction nonstopmode -output-directory %o %f")))
    (condition-case err
        (org-create-formula-image
         (alist-get 'source request) (alist-get 'output request)
         (list :foreground (alist-get 'foreground request) :background "Transparent"
               :scale (alist-get 'scale request)) t type)
      (error
       (when-let* ((buffer (get-buffer "*Org Preview LaTeX Output*")))
         (princ (with-current-buffer buffer (buffer-string))))
       (signal (car err) (cdr err))))))

;;;###autoload
(defun agent-ide-preview-latex ()
  "Refresh formula previews in this session, excluding its editable prompt."
  (interactive)
  (let* ((session (or (agent-ide--session-for-buffer) (user-error "No Agent IDE session")))
         (end (or (agent-ide-session-input-prompt-start-marker session) (point-max))))
    (unless (or (not agent-ide-latex-preview) (agent-ide-latex--settings))
      (user-error "Formula previews need graphical Emacs, latex and %s" agent-ide-latex-process))
    (with-silent-modifications
      (let ((pos (point-min)))
        (while (< pos end)
          (let ((next (next-single-property-change pos 'agent-ide-latex-key nil end)))
            (when (get-text-property pos 'agent-ide-latex-key)
              (let* ((key (get-text-property pos 'agent-ide-latex-key))
                     (record (gethash key agent-ide-latex--cache)))
                (when (eq (plist-get record :status) 'failed)
                  (remhash key agent-ide-latex--cache)))
              (remove-text-properties pos next '(display nil agent-ide-latex-key nil help-echo nil)))
            (setq pos next))))
      (agent-ide-latex-render (agent-ide-latex-prepare (point-min) end)))))

(provide 'agent-ide-latex)
;;; agent-ide-latex.el ends here
