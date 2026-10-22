;;; agent-ide-table.el --- Wrapped character-grid Markdown tables -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only table views, inspired by markdown-ts-appear's character-width
;; layout.  Source remains intact; each window gets its own row overlays.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'face-remap)
(require 'image)

(defface agent-ide-table-face
  '((t :inherit fixed-pitch))
  "Monospaced face for tables.  CJK glyphs should occupy two Latin columns."
  :group 'agent-ide)
(defface agent-ide-table-border-face
  '((t :inherit (agent-ide-table-face shadow)))
  "Face for generated table borders; its font can differ from cell text." :group 'agent-ide)
(defface agent-ide-table-header-face
  '((t :inherit agent-ide-table-face :weight bold))
  "Face for table headings." :group 'agent-ide)

(defvar-local agent-ide-table--tables nil)
(defvar-local agent-ide-table--timer nil)
(defvar-local agent-ide-table--enabled nil)
(defvar-local agent-ide-table--changing nil)
(defvar-local agent-ide-table--font-cookie nil)
(defvar-local agent-ide-table--font-family nil)
(defvar-local agent-ide-table--border-font-cookie nil)
(defvar-local agent-ide-table--border-font-family nil)
(defvar-local agent-ide-table-display-window nil
  "Optional host window for tables in hidden inline-response buffers.")
(defvar agent-ide-latex-updated-functions)
(defvar agent-ide-latex--updated-region)
(declare-function agent-ide-renderer-open-url "agent-ide-renderer" (url))

(defun agent-ide-table--cells (line)
  "Split a pipe row LINE, respecting escapes and matched inline code."
  (let ((i 0) (start 0) (len (length line)) parts separators)
    (while (< i len)
      (if (get-text-property i 'agent-ide-latex line)
          (setq i (next-single-property-change i 'agent-ide-latex line len))
        (pcase (aref line i)
        (?\\ (setq i (min len (+ i 2))))
        (?` (let* ((beg i)
                   (end (progn (while (and (< i len) (= (aref line i) ?`))
                                 (cl-incf i)) i))
                   (close (string-match (regexp-quote (substring line beg end)) line end)))
              (when close (setq i (+ close (- end beg))))))
        (?| (push (string-trim (substring line start i)) parts)
            (push i separators)
            (setq i (1+ i) start i))
        (_ (cl-incf i)))))
    (when separators
      (push (string-trim (substring line start)) parts)
      (setq parts (nreverse parts))
      (when (string-empty-p (car parts)) (setq parts (cdr parts)))
      (when (and parts (string-empty-p (car (last parts))))
        (setq parts (butlast parts)))
      parts)))

(defun agent-ide-table--alignments (cells)
  "Return alignment symbols if CELLS form a Markdown delimiter row."
  (when (and cells (cl-every (lambda (s) (string-match-p "\\`:?---+:?\\'" s)) cells))
    (mapcar (lambda (s) (cond ((and (string-prefix-p ":" s) (string-suffix-p ":" s)) 'center)
                             ((string-suffix-p ":" s) 'right)
                             (t 'left))) cells)))

(defun agent-ide-table--visible (cell)
  "Copy visible text of CELL, retaining inline colors and link actions."
  (let ((pos 0) parts)
    (while (< pos (length cell))
      (let* ((preview (get-text-property pos 'agent-ide-latex-preview cell))
             (display (or (car-safe preview) (get-text-property pos 'display cell)))
             (image (and (eq (car-safe display) 'image) display))
             (end (cond (preview (next-single-property-change pos 'agent-ide-latex-preview cell (length cell)))
                        ((or image (and (stringp display) (not (string-empty-p display))))
                         (next-single-property-change pos 'display cell (length cell)))
                        (t (next-property-change pos cell (length cell)))))
             (part (cond (image (propertize "\uFFFC" 'agent-ide-table-image
                                             (list image (substring-no-properties cell pos end))
                                             'help-echo (substring-no-properties cell pos end)))
                         ((stringp display) (copy-sequence display))
                         (t (substring cell pos end))))
             (url (get-text-property pos 'agent-ide-url cell)))
        (remove-text-properties 0 (length part)
                                '(display nil invisible nil read-only nil field nil
                                  button nil category nil keymap nil local-map nil) part)
        (when url
          (let ((map (make-sparse-keymap)))
            (define-key map [mouse-1]
                        (lambda (_event) (interactive "e") (agent-ide-renderer-open-url url)))
            (define-key map [mouse-2]
                        (lambda (_event) (interactive "e") (agent-ide-renderer-open-url url)))
            (add-text-properties 0 (length part)
                                 (list 'keymap map 'mouse-face 'highlight 'help-echo url) part)))
        (push part parts)
        (setq pos end)))
    (let ((text (apply #'concat (nreverse parts))))
      ;; Markdown escapes are literal characters, not layout delimiters.
      (setq text (replace-regexp-in-string "\\\\\\([|\\\\]\\)" "\\1" text))
      (setq text (replace-regexp-in-string "[\t\r\n]" " " text))
      ;; Force the grid font/size, while retaining foregrounds and emphasis.
      (add-face-text-property 0 (length text) 'agent-ide-table-face nil text)
      text)))

(defun agent-ide-table--units (text)
  "Split TEXT into graphemes and indivisible image atoms."
  (let ((pos 0) units)
    (while (< pos (length text))
      (let ((end (next-single-property-change pos 'agent-ide-table-image text (length text))))
        (if (get-text-property pos 'agent-ide-table-image text)
            (push (substring text pos end) units)
          (dolist (glyph (string-glyph-split (substring text pos end))) (push glyph units)))
        (setq pos end)))
    (nreverse units)))

(defun agent-ide-table--text-width (text)
  "Return TEXT's grid width, counting each image atom once."
  (if (not (text-property-not-all 0 (length text) 'agent-ide-table-image nil text))
      (string-width text)
    (cl-loop for unit in (agent-ide-table--units text)
             sum (or (get-text-property 0 'agent-ide-table-columns unit) (string-width unit)))))

(defun agent-ide-table--images (text window &optional limit)
  "Size image atoms in TEXT for WINDOW, fitting within LIMIT columns."
  (if (not (text-property-not-all 0 (length text) 'agent-ide-table-image nil text)) text
    (let ((pos 0) parts)
      (while (< pos (length text))
        (let* ((atom (get-text-property pos 'agent-ide-table-image text))
               (end (next-single-property-change pos 'agent-ide-table-image text (length text))))
          (push
           (if (not atom) (substring text pos end)
             (if (not (display-images-p (and window (window-frame window)))) (cadr atom)
               (let* ((image (copy-sequence (car atom)))
                      (unit (max 1 (window-font-width window 'agent-ide-table-face)))
                      (natural (car (image-size image t (and window (window-frame window)))))
                      (pixels (max 1 (if limit (min natural (* limit unit)) natural)))
                      (columns (ceiling pixels unit))
                      (padding (- (* columns unit) pixels)))
                 ;; Never mutate a shared TeX cache entry when fitting a cell.
                 (setf (image-property image :width) pixels
                       (image-property image :height) nil
                       (image-property image :max-width) nil
                       (image-property image :max-height) nil
                       (image-property image :scale) 1)
                 (propertize
                  (concat (propertize "\uFFFC" 'display image)
                          (propertize "\u200B" 'display `(space :width (,padding))))
                  'agent-ide-table-image atom 'agent-ide-table-columns columns
                  'help-echo (cadr atom))))) parts)
          (setq pos end)))
      (apply #'concat (nreverse parts)))))

(defun agent-ide-table--metrics (rows columns)
  "Measure natural and minimum character widths of ROWS with COLUMNS cells."
  (let ((natural (make-vector columns 1)) (minimum (make-vector columns 1)))
    (dolist (row rows)
      (cl-loop for cell in (nth 2 row) for i from 0 below columns do
               (aset natural i (max (aref natural i) (agent-ide-table--text-width cell)))
               (dolist (glyph (agent-ide-table--units cell))
                 (aset minimum i (max (aref minimum i) (if (get-text-property 0 'agent-ide-table-image glyph) 1 (string-width glyph)))))))
    (cons natural minimum)))

(defun agent-ide-table--widths (metrics available)
  "Allocate METRICS columns within AVAILABLE character cells."
  (let* ((natural (car metrics)) (widths (copy-sequence (cdr metrics)))
         (n (length widths)) (budget (- available (+ (* 3 n) 1) (cl-reduce #'+ widths))))
    ;; Grow narrow columns first; very long cells wrap rather than dominate.
    (while (and (> budget 0) (cl-loop for i below n thereis (< (aref widths i) (aref natural i))))
      (let (best)
        (dotimes (i n)
          (when (and (< (aref widths i) (aref natural i))
                     (or (null best) (< (aref widths i) (aref widths best))))
            (setq best i)))
        (cl-incf (aref widths best))
        (cl-decf budget)))
    (append widths nil)))

(defun agent-ide-table--split-word (text width)
  "Split long TEXT at WIDTH columns, preserving graphemes, faces and links."
  (if (<= (agent-ide-table--text-width text) width) (list text)
    (let ((used 0) pieces lines)
      (dolist (glyph (agent-ide-table--units text))
        (let ((size (agent-ide-table--text-width glyph)))
          (when (and pieces (> (+ used size) width))
            (push (apply #'concat (nreverse pieces)) lines)
            (setq pieces nil used 0))
          (push glyph pieces)
          (cl-incf used size)))
      (when pieces (push (apply #'concat (nreverse pieces)) lines))
      (nreverse lines))))

(defun agent-ide-table--wrap (text width)
  "Wrap TEXT at WIDTH columns, keeping words together when possible."
  (if (<= (agent-ide-table--text-width text) width) (list text)
    (let ((line "") lines)
      (dolist (word (split-string text " +" t))
        (cond
         ((> (agent-ide-table--text-width word) width)
          (unless (string-empty-p line) (push line lines))
          (let ((chunks (agent-ide-table--split-word word width)))
            (dolist (chunk (butlast chunks)) (push chunk lines))
            (setq line (car (last chunks)))))
         ((string-empty-p line) (setq line word))
         ((<= (+ (agent-ide-table--text-width line) 1 (agent-ide-table--text-width word)) width)
          (setq line (concat line " " word)))
         (t (push line lines) (setq line word))))
      (unless (string-empty-p line) (push line lines))
      (or (nreverse lines) (list "")))))

(defun agent-ide-table--row (cells widths aligns header &optional window)
  "Lay out CELLS with WIDTHS and ALIGNS; emphasize HEADER cells."
  (let ((wrapped (cl-mapcar (lambda (cell width) (agent-ide-table--wrap (agent-ide-table--images (or cell "") window width) width))
                            cells widths)) lines)
    (while (seq-some #'identity wrapped)
      (let (parts)
        (cl-loop for rest on wrapped for width in widths for align in aligns do
                 (let* ((text (copy-sequence (or (pop (car rest)) "")))
                        (pad (max 0 (- width (agent-ide-table--text-width text))))
                        (left (pcase align ('right pad) ('center (/ pad 2)) (_ 0))))
                   (when header
                     (add-face-text-property 0 (length text) 'agent-ide-table-header-face nil text))
                   (push (concat (propertize "│ " 'face 'agent-ide-table-border-face 'agent-ide-table-border t)
                                 (make-string left ?\s) text (make-string (- pad left) ?\s) " ") parts)))
        (push (concat (apply #'concat (nreverse parts))
                      (propertize "│" 'face 'agent-ide-table-border-face 'agent-ide-table-border t)) lines)))
    (let ((text (string-join (nreverse lines) "\n")))
      (add-face-text-property 0 (length text) 'agent-ide-table-face t text)
      text)))

(defun agent-ide-table--window-state ()
  "Return window and character capacity pairs, with a hidden-buffer fallback."
  (mapcar (lambda (window)
            (cons window (if (window-live-p window)
                             (max 1 (floor (window-max-chars-per-line window 'agent-ide-table-face)))
                           80)))
          (or (get-buffer-window-list (current-buffer) nil t)
              (list (and (window-live-p agent-ide-table-display-window)
                         agent-ide-table-display-window)))))

(defun agent-ide-table--drop-views (table)
  "Release TABLE's generated row overlays."
  (mapc #'delete-overlay (overlay-get table 'views))
  (overlay-put table 'views nil)
  (overlay-put table 'windows nil))

(defun agent-ide-table--dispose (table)
  "Release TABLE and its display overlays."
  (agent-ide-table--drop-views table)
  (delete-overlay table)
  (setq agent-ide-table--tables (delq table agent-ide-table--tables)))

(defun agent-ide-table--layout (table &optional force)
  "Refresh TABLE only when its window widths change, or FORCE is non-nil."
  (let ((windows (agent-ide-table--window-state)))
    (when (or force (not (equal windows (overlay-get table 'windows))))
      (agent-ide-table--drop-views table)
      (dolist (view windows)
        (let* ((indent (overlay-get table 'indent))
               (rows (overlay-get table 'rows))
               (aligns (overlay-get table 'aligns))
               (metrics (if (overlay-get table 'images)
                            (agent-ide-table--metrics
                             (mapcar (lambda (row)
                                       (list (car row) (cadr row)
                                             (mapcar (lambda (cell) (agent-ide-table--images cell (car view)))
                                                     (nth 2 row)))) rows)
                             (length aligns))
                          (overlay-get table 'metrics)))
               (widths (agent-ide-table--widths metrics (- (cdr view) (string-width indent))))
               (index 0))
          (dolist (row (overlay-get table 'rows))
            (let* ((beg (+ (overlay-start table) (nth 0 row)))
                   (end (+ (overlay-start table) (nth 1 row)))
                   (display
                    (if (= index 1)
                        (propertize
                         (concat "├─" (mapconcat (lambda (w) (make-string w ?─)) widths "─┼─") "─┤")
                         'face 'agent-ide-table-border-face 'agent-ide-table-border t)
                      (agent-ide-table--row (nth 2 row) widths aligns (= index 0) (car view))))
                   (ov (make-overlay beg end nil nil nil)))
              (setq display (concat indent (replace-regexp-in-string "\n" (concat "\n" indent) display t t)))
              (agent-ide-table--apply-fonts display)
              (overlay-put ov 'agent-ide-table-row t)
              ;; Replacement strings do not recursively render display images.
              ;; Insert rich rows before the hidden source instead, so image
              ;; and pixel-padding properties reach Emacs's redisplay engine.
              (if (text-property-not-all 0 (length display) 'agent-ide-table-image nil display)
                  (progn (overlay-put ov 'display "")
                         (overlay-put ov 'before-string display))
                (overlay-put ov 'display display))
              (overlay-put ov 'face 'agent-ide-table-face)
              (overlay-put ov 'priority 20)
              (overlay-put ov 'evaporate t)
              (when (and (car view) (eq (window-buffer (car view)) (current-buffer)))
                (overlay-put ov 'window (car view)))
              (overlay-put table 'views (cons ov (overlay-get table 'views)))
              (cl-incf index)))))
      (overlay-put table 'windows windows))))

(defun agent-ide-table--line (limit)
  "Return cells on the current line up to LIMIT, outside code fences."
  (let ((end (min limit (line-end-position))))
    (unless (or (>= (point) end)
                (get-text-property (point) 'agent-ide-markdown-code-content)
                (looking-at-p "    "))
      (agent-ide-table--cells (buffer-substring (point) end)))))

(defun agent-ide-table-render (start end)
  "Render pipe tables in START..END without modifying Markdown source."
  (agent-ide-table-enable)
  (save-excursion
    (save-match-data
      (goto-char start)
      (beginning-of-line)
      ;; A streamed suffix can begin in the middle of an existing table.
      (while (and (> (point) (point-min))
                  (save-excursion (forward-line -1) (agent-ide-table--line end)))
        (forward-line -1))
      (while (< (point) end)
        (let* ((beg (point)) (header (agent-ide-table--line end))
               (indent (buffer-substring-no-properties beg (progn (skip-chars-forward " \t") (point))))
               (header-end (min (line-end-position) end))
               (delim (progn (forward-line 1) (when (< (point) end) (agent-ide-table--line end))))
               (aligns (agent-ide-table--alignments delim)))
          (when (and header aligns (= (length header) (length aligns)))
            (let* ((columns (length aligns))
                   (rows (list (list (- (point) beg) (- (min end (line-end-position)) beg) nil)
                               (list 0 (- header-end beg) (mapcar #'agent-ide-table--visible header))))
                   (finish (min end (line-end-position))) cells)
              (forward-line 1)
              (while (and (< (point) end) (setq cells (agent-ide-table--line end)))
                (setq cells (cl-subseq (append cells (make-list columns "")) 0 columns))
                (setq finish (min end (line-end-position)))
                (push (list (- (point) beg) (- finish beg) (mapcar #'agent-ide-table--visible cells)) rows)
                (forward-line 1))
              (setq rows (nreverse rows))
              (let* ((source (buffer-substring-no-properties beg finish))
                     (table (seq-find (lambda (o) (and (overlay-get o 'agent-ide-table)
                                                       (= (overlay-start o) beg)))
                                      (overlays-at beg))))
                (unless (and table (= (overlay-end table) finish)
                             (equal source (overlay-get table 'source)))
                  (when table (agent-ide-table--dispose table))
                  (setq table (make-overlay beg finish nil nil nil))
                  (overlay-put table 'agent-ide-table t)
                  (overlay-put table 'evaporate t)
                  (overlay-put table 'source source)
                  (overlay-put table 'rows rows)
                  (overlay-put table 'images
                               (seq-some (lambda (row)
                                           (seq-some (lambda (cell) (text-property-not-all
                                                                    0 (length cell) 'agent-ide-table-image nil cell))
                                                     (nth 2 row))) rows))
                  (overlay-put table 'indent indent)
                  (overlay-put table 'aligns aligns)
                  (overlay-put table 'metrics (agent-ide-table--metrics rows columns))
                  (push table agent-ide-table--tables))
                (agent-ide-table--layout table)))))))))

(defun agent-ide-table--before-change (beg end)
  "Remember layouts intersecting BEG..END before a possible text change."
  (setq agent-ide-table--changing
        (cons (buffer-chars-modified-tick)
              (seq-filter (lambda (o) (overlay-get o 'agent-ide-table))
                          (delete-dups (append (overlays-in beg end) (overlays-at beg)))))))

(defun agent-ide-table--after-change (_beg _end _old-length)
  "Discard changed layouts, ignoring face and read-only property changes."
  (when (and agent-ide-table--changing
             (/= (car agent-ide-table--changing) (buffer-chars-modified-tick)))
    (mapc #'agent-ide-table--dispose (cdr agent-ide-table--changing)))
  (setq agent-ide-table--changing nil))

(defun agent-ide-table-refresh (&optional force)
  "Refresh existing tables for the current window geometry.
With FORCE, also reapply customized content and border fonts."
  (interactive (list t))
  (when force (agent-ide-table--configure-fonts))
  (when (timerp agent-ide-table--timer) (cancel-timer agent-ide-table--timer))
  (setq agent-ide-table--timer nil)
  (dolist (table (copy-sequence agent-ide-table--tables))
    (if (overlay-buffer table) (agent-ide-table--layout table force)
      (agent-ide-table--dispose table))))

(defun agent-ide-table--window-change (_window)
  "Coalesce layout work after the display window changes."
  (when (and agent-ide-table--tables (not (timerp agent-ide-table--timer)))
    (setq agent-ide-table--timer
          (run-at-time 0.15 nil
                       (lambda (buffer)
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer (agent-ide-table-refresh))))
                       (current-buffer)))))

(defun agent-ide-table-cleanup ()
  "Release all table overlays and resize timers in this buffer."
  (when (timerp agent-ide-table--timer) (cancel-timer agent-ide-table--timer))
  (setq agent-ide-table--timer nil)
  (mapc #'agent-ide-table--dispose (copy-sequence agent-ide-table--tables)))

(defun agent-ide-table--configure-fonts ()
  "Resolve independent cell and border fonts, honoring customized faces."
  (when agent-ide-table--font-cookie
    (face-remap-remove-relative agent-ide-table--font-cookie))
  (when agent-ide-table--border-font-cookie
    (face-remap-remove-relative agent-ide-table--border-font-cookie))
  (setq agent-ide-table--font-cookie nil
        agent-ide-table--border-font-cookie nil
        agent-ide-table--font-family nil
        agent-ide-table--border-font-family nil)
  (when (display-graphic-p)
    (let* ((font (face-attribute 'default :font))
           (spacing (and (fontp font) (font-get font :spacing)))
           (grid (seq-find (lambda (name) (find-font (font-spec :family name)))
                           '("Sarasa Term SC" "Sarasa Mono SC" "Sarasa Term SC Nerd")))
           (content (or (and (numberp spacing) (>= spacing 90)
                             (face-attribute 'default :family nil t)) grid)))
      ;; Keep the user's monospaced body font for text, including inline code.
      (when (and content (eq (face-attribute 'agent-ide-table-face :family nil nil) 'unspecified))
        (setq agent-ide-table--font-cookie
              (face-remap-add-relative 'agent-ide-table-face :family content)
              agent-ide-table--font-family content))
      ;; A complete grid font keeps Unicode borders one character wide.
      (when (and (or grid content)
                 (eq (face-attribute 'agent-ide-table-border-face :family nil nil) 'unspecified))
        (setq agent-ide-table--border-font-cookie
              (face-remap-add-relative 'agent-ide-table-border-face :family (or grid content))
              agent-ide-table--border-font-family (or grid content))))))

(defun agent-ide-table--apply-fonts (display)
  "Apply cell and border families separately to DISPLAY.
Explicit families also survive display in an inline viewport's host buffer."
  (let ((pos 0))
    (while (< pos (length display))
      (let* ((border (get-text-property pos 'agent-ide-table-border display))
             (end (next-single-property-change pos 'agent-ide-table-border display (length display)))
             (family (if border
                         (or agent-ide-table--border-font-family
                             (face-attribute 'agent-ide-table-border-face :family nil t))
                       (or agent-ide-table--font-family
                           (face-attribute 'agent-ide-table-face :family nil t)))))
        (when (stringp family)
          (add-face-text-property pos end (list :family family) nil display))
        (setq pos end)))))

(defun agent-ide-table--preview-updated ()
  "Rebuild only tables containing the formula whose preview just changed."
  (when (and (boundp 'agent-ide-latex--updated-region) agent-ide-latex--updated-region)
    (let ((tables (seq-filter (lambda (o) (overlay-get o 'agent-ide-table))
                              (overlays-in (car agent-ide-latex--updated-region)
                                           (cdr agent-ide-latex--updated-region)))))
      (dolist (table tables)
        (let ((start (overlay-start table)) (end (overlay-end table)))
          (agent-ide-table--dispose table)
          (agent-ide-table-render start end))))))

(defun agent-ide-table-enable ()
  "Install buffer-local lifecycle hooks for table views."
  (unless agent-ide-table--enabled
    (setq agent-ide-table--enabled t)
    (agent-ide-table--configure-fonts)
    (add-hook 'agent-ide-latex-updated-functions #'agent-ide-table--preview-updated -10 t)
    (add-hook 'before-change-functions #'agent-ide-table--before-change nil t)
    (add-hook 'after-change-functions #'agent-ide-table--after-change nil t)
    (add-hook 'window-size-change-functions #'agent-ide-table--window-change nil t)
    (add-hook 'window-buffer-change-functions #'agent-ide-table--window-change nil t)
    (add-hook 'kill-buffer-hook #'agent-ide-table-cleanup nil t)
    (add-hook 'change-major-mode-hook #'agent-ide-table-cleanup nil t)))

(defun agent-ide-table-substring (start end)
  "Return START..END with table row views for a hidden inline viewport."
  (let ((pos start) pieces)
    (while (< pos end)
      (if-let* ((row (seq-find (lambda (o) (and (overlay-get o 'agent-ide-table-row)
                                               (= (overlay-start o) pos))) (overlays-at pos))))
          (progn (push (or (overlay-get row 'before-string) (overlay-get row 'display)) pieces)
                 (setq pos (min end (overlay-end row))))
        (let ((next (min end (next-overlay-change pos))))
          (push (buffer-substring pos next) pieces)
          (setq pos next))))
    (apply #'concat (nreverse pieces))))

(provide 'agent-ide-table)
;;; agent-ide-table.el ends here
