;;; agent-ide-stream.el --- Bounded streaming Markdown work -*- lexical-binding: t; -*-

;;; Commentary:
;; Share batching and Markdown boundaries between transcript and inline views.

;;; Code:

(require 'cl-lib)
(require 'agent-ide-latex)

(defcustom agent-ide-stream-render-interval 0.05
  "Minimum delay in seconds between streaming Markdown refreshes.
Text is inserted immediately.  Zero renders on every chunk.  Completed
paragraphs are retained; very long unfinished blocks are refreshed less often.
Finishing a stream always renders its complete Markdown immediately."
  :type 'number :group 'agent-ide)

(cl-defstruct (agent-ide-stream (:constructor agent-ide-stream--make))
  start end pending render updated timer last-size rendered-end finalized)

(defvar-local agent-ide-stream--states nil
  "Streaming renderers owned by this buffer.")

(defun agent-ide-stream-create (start render &optional updated)
  "Track Markdown at START, rendered by RENDER with START and END arguments.
Call UPDATED without arguments after each refresh, including text-only ones."
  (let ((state (agent-ide-stream--make
                :start (copy-marker start) :end (copy-marker start)
                :pending (copy-marker start) :render render :updated updated
                :last-size 0)))
    (push state agent-ide-stream--states)
    (add-hook 'kill-buffer-hook #'agent-ide-stream--cleanup nil t)
    (add-hook 'change-major-mode-hook #'agent-ide-stream--cleanup nil t)
    state))

(defun agent-ide-stream-dispose (state)
  "Release STATE without running more rendering work."
  (when state
    (when (timerp (agent-ide-stream-timer state))
      (cancel-timer (agent-ide-stream-timer state)))
    (setf (agent-ide-stream-timer state) nil)
    (when-let* ((buffer (marker-buffer (agent-ide-stream-start state))))
      (with-current-buffer buffer
        (setq agent-ide-stream--states (delq state agent-ide-stream--states))))
    (dolist (marker (list (agent-ide-stream-start state)
                          (agent-ide-stream-end state)
                          (agent-ide-stream-pending state)))
      (set-marker marker nil))))

(defun agent-ide-stream--cleanup ()
  "Cancel all streaming work belonging to the current buffer."
  (mapc #'agent-ide-stream-dispose (copy-sequence agent-ide-stream--states)))

(defun agent-ide-stream--boundary (start end)
  "Find a complete paragraph boundary between START and END.
Keep blank lines inside fenced code, inline code and multiline math together.
An unfinished delimiter keeps the remaining text in the pending region."
  (save-excursion
    (save-match-data
      (goto-char start)
      (let ((boundary start))
        (while (re-search-forward
                (rx (or (seq "\n" (* (any " \t")) "\n")
                        (+ "`") (>= 3 "~") "$$" "$" "\\(" "\\[")) end t)
          (let ((begin (match-beginning 0))
                (token (match-string-no-properties 0)))
            (cond
             ((eq (aref token 0) ?\n) (setq boundary (point)))
             ((agent-ide-latex--escaped-p begin))
             ((and (memq (aref token 0) '(?` ?~))
                   (>= (length token) 3)
                   (string-match-p "\\`[ \t]*\\'"
                                   (buffer-substring-no-properties
                                    (line-beginning-position) begin)))
              (forward-line 1)
              (if (and (< (point) end)
                       (re-search-forward
                        (concat "^[ \t]*" (regexp-quote (substring token 0 1))
                                "\\{" (number-to-string (length token)) ",\\}[ \t]*\n")
                        end t))
                  (setq boundary (point))
                (goto-char end)))
             ((eq (aref token 0) ?`)
              (unless (search-forward token end t) (goto-char end)))
             ((eq (aref token 0) ?~))
             (t
              (let* ((single (equal token "$"))
                     (body (point))
                     (closing (pcase token ("\\(" "\\)") ("\\[" "\\]") (_ token)))
                     (finish
                      (unless (and single (memq (char-after) '(nil ?\s ?\t ?\n ?$)))
                        (agent-ide-latex--closing-end
                         closing (if single (min end (line-end-position)) end) single))))
                (unless finish (goto-char (if single body end))))))))
        boundary))))

(defun agent-ide-stream-flush (state &optional final)
  "Render pending STATE work now; with FINAL, reconcile the whole stream.
Bound rescans of a long unfinished block by doubling its size between passes.
Ordinary paragraphs are committed at blank lines and never scanned again
until the final pass.  Preserve point, undo history and the modified flag."
  (when (timerp (agent-ide-stream-timer state))
    (cancel-timer (agent-ide-stream-timer state)))
  (setf (agent-ide-stream-timer state) nil)
  (when-let* ((buffer (marker-buffer (agent-ide-stream-start state))))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (let* ((start (marker-position (agent-ide-stream-pending state)))
                 (end (marker-position (agent-ide-stream-end state)))
                 (size (- end start)))
            (when (if final
                      (not (agent-ide-stream-finalized state))
                    (and (not (equal end (agent-ide-stream-rendered-end state)))
                         (or (<= size 4096)
                             (>= size (* 2 (agent-ide-stream-last-size state))))))
              (with-silent-modifications
                (funcall (agent-ide-stream-render state)
                         (if final (marker-position (agent-ide-stream-start state)) start)
                         end))
              (setf (agent-ide-stream-rendered-end state) end
                    (agent-ide-stream-finalized state) final)
              (unless final
                (let ((boundary (agent-ide-stream--boundary start end)))
                  (set-marker (agent-ide-stream-pending state) boundary)
                  (setf (agent-ide-stream-last-size state) (- end boundary))))))
          (when (agent-ide-stream-updated state)
            (funcall (agent-ide-stream-updated state))))))))

(defun agent-ide-stream-queue (state end)
  "Extend STATE to END and schedule one refresh for a burst of chunks."
  (set-marker (agent-ide-stream-end state) end)
  (setf (agent-ide-stream-finalized state) nil)
  (cond
   ((or (null (agent-ide-stream-rendered-end state))
        (<= agent-ide-stream-render-interval 0))
    (agent-ide-stream-flush state))
   ((not (timerp (agent-ide-stream-timer state)))
    (setf (agent-ide-stream-timer state)
          (run-at-time agent-ide-stream-render-interval nil
                       #'agent-ide-stream-flush state)))))

(provide 'agent-ide-stream)
;;; agent-ide-stream.el ends here
