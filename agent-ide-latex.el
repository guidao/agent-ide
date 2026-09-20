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
(defvar org-format-latex-header)
(defvar org-latex-compiler)
(declare-function org-create-formula-image "org")

(defcustom agent-ide-latex-preview t
  "Whether complete math fragments are automatically previewed."
  :type 'boolean :group 'agent-ide)

(defcustom agent-ide-latex-process 'xelatex
  "Org preview process used for formulas.
The default uses XeLaTeX and xeCJK for Chinese text, then dvisvgm for SVG."
  :type '(choice (const xelatex) (const dvisvgm) (const dvipng)) :group 'agent-ide)

(defcustom agent-ide-latex-cjk-font
  (if (eq system-type 'darwin) "Songti SC" "FandolSong-Regular.otf")
  "Chinese font used by the XeLaTeX preview process.
Use an installed font name or a TeX-discoverable font filename."
  :type 'string :group 'agent-ide)

(defcustom agent-ide-latex-scale 1.3
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
(defvar agent-ide-latex--background-render nil
  "Non-nil while scheduling formula previews for existing history.")
(defvar-local agent-ide-latex--revealed nil
  "Currently revealed formula as (BEGIN-MARKER END-MARKER PREVIEW).")
(defvar-local agent-ide-latex--input-state nil
  "Last input snapshot as (START TEXT PREVIEW-ENABLED MODIFICATION-TICK).")
(defvar-local agent-ide-latex--input-fragments nil
  "Input fragments tracked as (BEGIN-MARKER END-MARKER SOURCE).")

(defun agent-ide-latex-update-input (start end)
  "Preview editable input between START and END, preserving its text and faces.
Call after editing commands.  Retain unchanged fragments and pending workers;
discard stale previews, including properties inherited by newly inserted text."
  (let* ((source (buffer-substring-no-properties start end))
         (state (list start source agent-ide-latex-preview
                      (buffer-chars-modified-tick))))
    (unless (equal state agent-ide-latex--input-state)
      (let* ((fragments
              (when agent-ide-latex-preview
                ;; Recognition protects Markdown by removing faces.  Parse a
                ;; copy so editable prompt styling and undo are untouched.
                (with-temp-buffer
                  (insert source)
                  (mapcar (lambda (fragment)
                            (list (+ start (1- (car fragment)))
                                  (+ start (1- (cadr fragment)))
                                  (caddr fragment)))
                          (agent-ide-latex-prepare (point-min) (point-max))))))
             retained)
        (dolist (old agent-ide-latex--input-fragments)
          (pcase-let ((`(,begin ,finish ,text) old))
            (let ((fragment (list (marker-position begin)
                                  (marker-position finish) text)))
              (when (and (member fragment fragments)
                         (get-text-property begin 'agent-ide-latex-key))
                (push (list begin finish
                            (cl-loop for property in
                                     '(agent-ide-latex-key agent-ide-latex-preview
                                       display help-echo)
                                     append (list property
                                                  (get-text-property begin property))))
                      retained)))))
        (with-silent-modifications
          ;; Only clear properties belonging to formulas, leaving other input
          ;; display properties alone.  This also removes inherited tail images.
          (let ((pos start))
            (while (< pos end)
              (let ((next (next-single-property-change pos 'agent-ide-latex-key nil end)))
                (when (get-text-property pos 'agent-ide-latex-key)
                  (remove-text-properties
                   pos next '(agent-ide-latex-key nil agent-ide-latex-preview nil
                              display nil help-echo nil)))
                (setq pos next))))
          (dolist (entry retained)
            (add-text-properties (car entry) (cadr entry) (caddr entry))))
        (dolist (old agent-ide-latex--input-fragments)
          (set-marker (car old) nil)
          (set-marker (cadr old) nil))
        (setq agent-ide-latex--input-fragments
              (mapcar (lambda (fragment)
                        (list (copy-marker (car fragment) t)
                              (copy-marker (cadr fragment) nil) (caddr fragment)))
                      fragments)
              agent-ide-latex--input-state state)
        (agent-ide-latex-render fragments)))
    (agent-ide-latex--reveal-at-point)))

(defun agent-ide-latex--restore-preview ()
  "Restore the revealed formula if its source and preview are still valid."
  (when agent-ide-latex--revealed
    (pcase-let ((`(,begin ,end ,preview) agent-ide-latex--revealed))
      (save-restriction
        (widen)
        (when (and (marker-position begin) (marker-position end)
                   (< begin end)
                   (eq preview (get-text-property begin 'agent-ide-latex-preview))
                   (equal (cadr preview) (buffer-substring-no-properties begin end)))
          (with-silent-modifications
            (put-text-property begin end 'display (car preview)))))
      (set-marker begin nil)
      (set-marker end nil))
    (setq agent-ide-latex--revealed nil)))

(defun agent-ide-latex--reveal-at-point ()
  "Show formula source at point and restore the preview when point leaves.
Only display properties change; source, undo history and modified state stay
intact.  Each preview has its own identity even when images are shared."
  (let ((preview (get-text-property (point) 'agent-ide-latex-preview)))
    (unless (and agent-ide-latex--revealed
                 (eq preview (nth 2 agent-ide-latex--revealed))
                 (<= (nth 0 agent-ide-latex--revealed) (point))
                 (< (point) (nth 1 agent-ide-latex--revealed)))
      (agent-ide-latex--restore-preview))
    (when (and preview (not agent-ide-latex--revealed))
      (save-restriction
        (widen)
        (let ((begin (or (previous-single-property-change
                          (1+ (point)) 'agent-ide-latex-preview)
                         (point-min)))
              (end (or (next-single-property-change
                        (point) 'agent-ide-latex-preview)
                       (point-max))))
          (setq agent-ide-latex--revealed
                (list (copy-marker begin t) (copy-marker end nil) preview)))))
    (when agent-ide-latex--revealed
      (save-restriction
        (widen)
        (with-silent-modifications
          (remove-text-properties (nth 0 agent-ide-latex--revealed)
                                  (nth 1 agent-ide-latex--revealed)
                                  '(display nil)))))))

(defun agent-ide-latex--hex-rgb (color)
  "Parse hexadecimal COLOR into RGB fractions without a display color lookup."
  (when (and (stringp color)
             (string-match-p "\\`#[[:xdigit:]]+\\'" color)
             (memq (length color) '(4 7 10 13)))
    (let* ((width (/ (1- (length color)) 3))
           (maximum (float (1- (expt 16 width)))))
      (cl-loop for start from 1 below (length color) by width
               collect (/ (string-to-number (substring color start (+ start width)) 16)
                          maximum)))))

(defun agent-ide-latex--cache-key (source settings)
  "Return a cache key for SOURCE and SETTINGS using exact RGB conversion."
  (secure-hash 'sha256 (prin1-to-string (cons 'exact-rgb-v1 (cons source settings)))))

(defun agent-ide-latex--upgrade-color-cache ()
  "Migrate plain monochrome SVGs from the terminal-palette cache.
Only migrate snippets without color commands or custom TeX definitions and
SVGs with a single explicit fill color.  Everything else uses the new cache
key and is regenerated normally, preserving intentional colors and PNGs.
Return the number of migrated entries."
  (let (migrations)
    (maphash
     (lambda (key record)
       (let* ((source (plist-get record :source))
              (settings (plist-get record :settings))
              (image (plist-get record :image))
              (svg (plist-get (cdr image) :data))
              (color (nth 2 settings))
              (case-fold-search t))
         (when (and (eq (plist-get record :status) 'done)
                    (equal key (secure-hash 'sha256 (prin1-to-string (cons source settings))))
                    (eq (plist-get (cdr image) :type) 'svg)
                    (stringp svg) (agent-ide-latex--hex-rgb color)
                    (not (string-match-p "color\\|special\\|input\\|include\\|def\\|command\\|csname" source))
                    (not (string-match-p "stroke=\\|<image\\|Gradient\\|<style" svg)))
           (let ((pattern "\\bfill=['\"]\\(#[[:xdigit:]]+\\)['\"]")
                 (pos 0) colors)
             (while (string-match pattern svg pos)
               (cl-pushnew (match-string 1 svg) colors :test #'equal)
               (setq pos (match-end 0)))
             (when (= (length colors) 1)
               (let* ((data (replace-regexp-in-string pattern (concat "fill='" color "'") svg t t))
                      (updated (copy-sequence record))
                      (spec (copy-sequence image)))
                 (setcdr spec (plist-put (cdr spec) :data data))
                 (setq updated (plist-put updated :image spec))
                 (push (list key (agent-ide-latex--cache-key source settings) updated) migrations)))))))
     agent-ide-latex--cache)
    (dolist (migration migrations)
      (unless (gethash (nth 1 migration) agent-ide-latex--cache)
        (puthash (nth 1 migration) (nth 2 migration) agent-ide-latex--cache))
      (remhash (car migration) agent-ide-latex--cache))
    (length migrations)))

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
  ;; Remember the caller's Markdown boundary.  A transcript also contains
  ;; prompts and raw tool output; it must never be parsed as one document.
  (when (< start end)
    (with-silent-modifications
      (put-text-property start end 'agent-ide-latex-region
                         (or (get-text-property start 'agent-ide-latex-region)
                             (list 'markdown)))))
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

(defun agent-ide-latex--image-type (process)
  "Return the image type produced by PROCESS."
  (if (eq process 'dvipng) 'png 'svg))

(defun agent-ide-latex--programs ()
  "Return the programs required by the selected preview process."
  (pcase agent-ide-latex-process
    ('xelatex '("xelatex" "dvisvgm"))
    ('dvisvgm '("latex" "dvisvgm"))
    ('dvipng '("latex" "dvipng"))
    (_ (error "Unsupported formula preview process: %s" agent-ide-latex-process))))

(defun agent-ide-latex--settings ()
  "Return preview settings, or nil if previews cannot be displayed."
  (when (and agent-ide-latex-preview (display-images-p)
             (cl-every #'executable-find (agent-ide-latex--programs))
             (image-type-available-p (agent-ide-latex--image-type agent-ide-latex-process)))
    (let* ((foreground (face-foreground 'default nil t))
           (rgb (or (agent-ide-latex--hex-rgb foreground)
                    (mapcar (lambda (v) (/ v 65535.0))
                            (ignore-errors (color-values foreground)))
                    '(0.0 0.0 0.0))))
      (list agent-ide-latex-process agent-ide-latex-scale
            (apply #'format "#%02x%02x%02x" (mapcar (lambda (v) (round (* v 255))) rgb))
            (when (eq agent-ide-latex-process 'xelatex) agent-ide-latex-cjk-font)))))

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
              (put-text-property begin end 'agent-ide-latex-preview
                                 (when image (list image source)))
              (if image
                  (put-text-property begin end 'display image)
		(remove-text-properties begin end '(display nil)))
              (put-text-property begin end 'help-echo
				 (if image source (concat "Formula preview: " error-message))))
            (add-hook 'post-command-hook #'agent-ide-latex--reveal-at-point nil t)
            ;; Hidden inline viewport buffers must retain their image properties.
            (when (eq (current-buffer) (window-buffer (selected-window)))
              (agent-ide-latex--reveal-at-point))
            (condition-case err
		(run-hooks 'agent-ide-latex-updated-functions)
              (error (message "Agent IDE preview refresh: %s" (error-message-string err))))))))
    (set-marker begin nil)
    (set-marker end nil)))

(defun agent-ide-latex-render (fragments)
  "Schedule previews of FRAGMENTS, sharing conversions across buffers."
  (when-let* ((settings (agent-ide-latex--settings)))
    (let ((background (or agent-ide-latex--background-render
                          (when-let* ((session (agent-ide--session-for-buffer)))
                            (agent-ide--session-metadata-get session :replaying))))
          promoted)
      (dolist (fragment fragments)
        (pcase-let* ((`(,begin ,end ,source) fragment)
                     (key (agent-ide-latex--cache-key source settings)))
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
                (puthash key record agent-ide-latex--cache))))
          ;; A live reply may reuse a formula already waiting in the history
          ;; queue, including one whose buffer already has its cache key.
          (let ((record (gethash key agent-ide-latex--cache)))
            (when (and (not background) (not (plist-get record :foreground))
                       (member key agent-ide-latex--queue))
              (puthash key (plist-put record :foreground t) agent-ide-latex--cache)
              (push key promoted)))))
      (when promoted
        ;; Keep FIFO order for live requests, including promoted history jobs.
        (let ((foreground-p (lambda (key)
                              (plist-get (gethash key agent-ide-latex--cache) :foreground)))
              (remaining (cl-remove-if (lambda (key) (member key promoted))
                                       agent-ide-latex--queue)))
          (setq agent-ide-latex--queue
                (append (cl-remove-if-not foreground-p remaining)
                        (nreverse promoted)
                        (cl-remove-if foreground-p remaining)))))
      (agent-ide-latex--start-next))))

(defun agent-ide-latex--error-summary (log)
  "Extract the first TeX error from LOG, joining package continuation lines."
  (when (and log (string-match "^! +\\([^\n]+\\(?:\n([^\n)]+)[ \t]+[^\n]+\\)*\\)" log))
    (replace-regexp-in-string "\n([^\n)]+)[ \t]+" " " (match-string 1 log))))

(defun agent-ide-latex--complete (key directory output log-buffer success)
  "Finish conversion KEY using OUTPUT and SUCCESS, then clean DIRECTORY."
  (let* ((record (gethash key agent-ide-latex--cache))
         (type (agent-ide-latex--image-type (car (plist-get record :settings))))
         (image (and success (file-exists-p output)
                     (ignore-errors
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally output)
                         (create-image (buffer-string) type t :ascent 'center)))))
         (log (when (and (not image) (buffer-live-p log-buffer))
                (with-current-buffer log-buffer
                  (buffer-substring-no-properties (point-min) (min (point-max) (+ (point-min) 16000))))))
         (detail (agent-ide-latex--error-summary log))
         (failure (unless image
                    (concat "conversion failed: " (or detail "no image produced")
                            "; M-x agent-ide-latex-show-error for details"))))
    (setq record (plist-put record :status (if image 'done 'failed)))
    (setq record (plist-put record :image image))
    (setq record (plist-put record :error failure))
    (setq record (plist-put record :log log))
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
           (output (expand-file-name
                    (concat "formula." (symbol-name (agent-ide-latex--image-type (car settings))))
                    directory))
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
                                 (scale . ,(nth 1 settings)) (foreground . ,(nth 2 settings))
                                 (cjkFont . ,(nth 3 settings))))
               nil input nil 'silent))
            (setq agent-ide-latex--process
                  (make-process
                   :name "agent-ide-latex" :buffer log-buffer :noquery t
                   :coding 'utf-8-unix
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
                           (when (process-live-p process)
                             (with-current-buffer log-buffer
                               (goto-char (point-max))
                               (insert (format "\n! Conversion timed out after %s seconds.\n"
                                               agent-ide-latex-timeout)))
                             (delete-process process)))
                         agent-ide-latex--process)))
        (error
         (message "Agent IDE formula preview: %s" (error-message-string err))
         (with-current-buffer log-buffer
           (goto-char (point-max))
           (insert "\n! " (error-message-string err) "\n"))
         (agent-ide-latex--complete key directory output log-buffer nil))))))

(defun agent-ide-latex--worker (request-file)
  "Render REQUEST-FILE using Org in a separate, clean Emacs process."
  (require 'org)
  (require 'ox-latex)
  (let* ((request (with-temp-buffer
                    (insert-file-contents request-file)
                    (json-parse-buffer :object-type 'alist)))
         (type (intern (alist-get 'process request)))
         (org-latex-compiler (if (eq type 'xelatex) "xelatex" "pdflatex"))
         (org-format-latex-header
          (if (eq type 'xelatex)
              (concat org-format-latex-header "\n\\usepackage{xeCJK}\n\\setCJKmainfont{"
                      (or (alist-get 'cjkFont request) agent-ide-latex-cjk-font) "}\n")
            org-format-latex-header))
         (default-directory (file-name-directory request-file))
         (temporary-file-directory default-directory)
         (shell-file-name "/bin/sh")
         (shell-command-switch "-c")
         (process-environment (append (list "openin_any=p" "openout_any=p" "shell_escape=f"
                                            (concat "TEXMFOUTPUT=" default-directory))
                                      process-environment))
         (processes (copy-tree org-preview-latex-process-alist))
         ;; Define the XDV pipeline here for Org versions without that entry.
         (org-preview-latex-process-alist
          (if (eq type 'xelatex)
              (cons (cons 'xelatex
                          (plist-put (copy-tree (cdr (assq 'dvisvgm processes)))
                                     :image-input-type "xdv"))
                    processes)
            processes))
         (entry (assq type org-preview-latex-process-alist)))
    ;; Reuse Org's preamble, packages, colors, scale and image conversion.
    (unless (memq type '(xelatex dvisvgm dvipng)) (error "Unsupported formula preview process"))
    (when (eq type 'xelatex)
      (setcdr entry (plist-put (cdr entry) :programs '("xelatex" "dvisvgm"))))
    (setcdr entry (plist-put (cdr entry) :latex-compiler
                            (if (eq type 'xelatex)
                                '("xelatex -no-pdf -no-shell-escape -halt-on-error -interaction nonstopmode -output-directory %o %f")
                              '("latex -no-shell-escape -halt-on-error -interaction nonstopmode -output-directory %o %f"))))
    (condition-case err
        (let ((org-color-format (symbol-function 'org-latex-color-format)))
          ;; Org normally calls `color-values', which approximates even hex
          ;; colors using the terminal palette in a --batch worker.
          (cl-letf (((symbol-function 'org-latex-color-format)
                     (lambda (color)
                       (if-let* ((rgb (agent-ide-latex--hex-rgb color)))
                           (mapconcat (lambda (v) (format "%.8f" v)) rgb ",")
                         (funcall org-color-format color)))))
            (org-create-formula-image
             (alist-get 'source request) (alist-get 'output request)
             (list :foreground (alist-get 'foreground request) :background "Transparent"
                   :scale (alist-get 'scale request)) t type)))
      (error
       (when-let* ((buffer (get-buffer "*Org Preview LaTeX Output*")))
         (princ (with-current-buffer buffer (buffer-string))))
       (princ (concat "\n! " (error-message-string err) "\n"))
       (kill-emacs 1)))))

;;;###autoload
(defun agent-ide-latex-show-error ()
  "Show the retained conversion error for the formula at point."
  (interactive)
  (let* ((key (get-text-property (point) 'agent-ide-latex-key))
         (record (gethash key agent-ide-latex--cache)))
    (unless (eq (plist-get record :status) 'failed)
      (user-error "No failed formula at point"))
    (with-help-window "*Agent IDE Formula Error*"
      (princ (plist-get record :error))
      (princ "\n\nFormula:\n")
      (princ (plist-get record :source))
      (princ "\n\nConversion log (up to 16000 characters):\n")
      (princ (or (plist-get record :log) "No converter output available.")))))

(defun agent-ide-latex--regions (end)
  "Return independently rendered Markdown regions before END.
For buffers rendered before region tracking was added, reuse the math ranges
already recognized during streaming.  Do not scan unrelated transcript text."
  (let ((pos (point-min)) regions)
    (while (< pos end)
      (let ((next (next-single-property-change pos 'agent-ide-latex-region nil end)))
        (if (get-text-property pos 'agent-ide-latex-region)
            (push (cons pos next) regions)
          (let ((legacy-pos pos))
            (while (< legacy-pos next)
              (let ((legacy-end (next-single-property-change legacy-pos 'agent-ide-latex nil next)))
                (when (get-text-property legacy-pos 'agent-ide-latex)
                  (push (cons legacy-pos legacy-end) regions))
                (setq legacy-pos legacy-end)))))
        (setq pos next)))
    (nreverse regions)))

;;;###autoload
(defun agent-ide-preview-latex ()
  "Refresh formula previews in this session, excluding its editable prompt."
  (interactive)
  (agent-ide-latex--upgrade-color-cache)
  (let* ((session (or (agent-ide--session-for-buffer) (user-error "No Agent IDE session")))
         (end (or (agent-ide-session-input-prompt-start-marker session) (point-max)))
         (regions (agent-ide-latex--regions end))
         (agent-ide-latex--background-render t))
    (unless (or (not agent-ide-latex-preview) (agent-ide-latex--settings))
      (user-error "Formula previews need graphical Emacs with %s support and %s"
                  (agent-ide-latex--image-type agent-ide-latex-process)
                  (string-join (agent-ide-latex--programs) ", ")))
    (with-silent-modifications
      (let ((pos (point-min)))
        (while (< pos end)
          (let ((next (next-single-property-change pos 'agent-ide-latex-key nil end)))
            (when (get-text-property pos 'agent-ide-latex-key)
              (let* ((key (get-text-property pos 'agent-ide-latex-key))
                     (record (gethash key agent-ide-latex--cache)))
                (when (eq (plist-get record :status) 'failed)
                  (remhash key agent-ide-latex--cache)))
              (remove-text-properties pos next '(display nil agent-ide-latex-key nil
                                                agent-ide-latex-preview nil help-echo nil)))
            (setq pos next))))
      (agent-ide-latex--restore-preview)
      ;; Restore recent replies first when a long history needs conversion.
      (dolist (region (reverse regions))
        (agent-ide-latex-render (agent-ide-latex-prepare (car region) (cdr region)))))))

(provide 'agent-ide-latex)
;;; agent-ide-latex.el ends here
