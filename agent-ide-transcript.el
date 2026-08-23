;;; agent-ide-transcript.el --- Agent event handling -*- lexical-binding: t; -*-

;;; Commentary:

;; Interprets ACP notifications/requests and updates Agent IDE buffers.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-ide-core)
(require 'agent-ide-renderer)
(require 'agent-ide-protocol)

(defvar agent-ide-message-chunk-functions nil
  "Hook run for each agent message text chunk.
Called with (SESSION TEXT) after the chunk is rendered in the transcript.")

(defun agent-ide-transcript--alist-object-p (value)
  "Return non-nil when VALUE is an ACP object represented as an alist."
  (and (listp value)
       (proper-list-p value)
       (cl-every (lambda (entry)
                   (and (consp entry)
                        (or (symbolp (car entry))
                            (stringp (car entry)))))
                 value)))

(defun agent-ide-transcript--content-text (content)
  "Return readable text from ACP CONTENT."
  (cond
   ((null content) "")
   ((stringp content) content)
   ((vectorp content)
    (mapconcat #'agent-ide-transcript--content-text (append content nil) "\n"))
   ((and (agent-ide-transcript--alist-object-p content)
         (map-elt content 'text))
    (map-elt content 'text))
   ((and (agent-ide-transcript--alist-object-p content)
         (map-elt content 'content))
    (agent-ide-transcript--content-text (map-elt content 'content)))
   ((agent-ide-transcript--alist-object-p content)
    (agent-ide--json-string content))
   ((listp content)
    (mapconcat #'agent-ide-transcript--content-text content "\n"))
   (t (format "%S" content))))

(defun agent-ide-transcript--tool-title (tool-call)
  "Return display title for TOOL-CALL."
  (or (agent-ide-transcript--nonblank-string (map-elt tool-call 'title))
      (agent-ide-transcript--nonblank-string (map-elt tool-call 'name))
      (agent-ide-transcript--nonblank-string (map-elt tool-call 'toolName))
      (agent-ide-transcript--nonblank-string (map-elt tool-call 'kind))))

(defun agent-ide-transcript--nonblank-string (value)
  "Return VALUE when it is a nonblank string."
  (when (and (stringp value)
             (not (string-empty-p (string-trim value))))
    (string-trim value)))

(defun agent-ide-transcript--command-string (command)
  "Return a readable shell COMMAND string."
  (cond
   ((stringp command)
    (agent-ide-transcript--nonblank-string command))
   ((vectorp command)
    (agent-ide-transcript--command-string (append command nil)))
   ((and (listp command)
         (not (keywordp (car-safe (car-safe command))))
         (cl-every #'stringp command))
    (agent-ide-transcript--nonblank-string
     (combine-and-quote-strings command)))
   ((listp command)
    (agent-ide-transcript--raw-command-string command))
   (t nil)))

(defun agent-ide-transcript--raw-command-string (raw)
  "Extract a command-like string from RAW input."
  (when (listp raw)
    (seq-some
     (lambda (key)
       (agent-ide-transcript--command-string (map-elt raw key)))
     '(command cmd shellCommand commandLine args argv invocation))))

(defun agent-ide-transcript--tool-field-string (tool-call key)
  "Return TOOL-CALL field KEY as a nonblank string."
  (agent-ide-transcript--nonblank-string (map-elt tool-call key)))

(defun agent-ide-transcript--web-search-tool-p (tool-call)
  "Return non-nil when TOOL-CALL looks like a web search."
  (let* ((raw (map-elt tool-call 'rawInput))
         (content (map-elt tool-call 'content))
         (haystack (string-join
                    (delq nil
                          (list (agent-ide-transcript--tool-field-string
                                 tool-call 'kind)
                                (agent-ide-transcript--tool-field-string
                                 tool-call 'title)
                                (agent-ide-transcript--tool-field-string
                                 tool-call 'name)
                                (agent-ide-transcript--tool-field-string
                                 tool-call 'toolName)))
                    " ")))
    (or (string-match-p "\\b\\(?:web\\|search\\|browser\\)\\b"
                        (downcase haystack))
        (and (listp raw)
             (or (map-elt raw 'query)
                 (map-elt raw 'queries))
             (not (seq-some (lambda (key) (map-elt raw key))
                            '(command cmd shellCommand commandLine args argv))))
        (agent-ide-transcript--web-search-result-query content))))

(defun agent-ide-transcript--query-list (value)
  "Return VALUE as a list of nonblank query strings."
  (cond
   ((stringp value)
    (when-let* ((query (agent-ide-transcript--nonblank-string value)))
      (list query)))
   ((vectorp value)
    (seq-mapcat #'agent-ide-transcript--query-list (append value nil)))
   ((and (listp value)
         (not (agent-ide-transcript--alist-object-p value)))
    (seq-mapcat #'agent-ide-transcript--query-list value))))

(defun agent-ide-transcript--web-search-queries (tool-call)
  "Return web search queries from TOOL-CALL."
  (let ((raw (map-elt tool-call 'rawInput)))
    (delete-dups
     (append (agent-ide-transcript--query-list (map-elt raw 'query))
             (agent-ide-transcript--query-list (map-elt raw 'queries))))))

(defun agent-ide-transcript--web-search-result-query (content)
  "Return query from web search result CONTENT text."
  (let ((text (agent-ide-transcript--content-text content)))
    (when (and (stringp text)
               (string-match
                "Web search results for query: \"\\([^\"]+\\)\""
                text))
      (match-string 1 text))))

(defun agent-ide-transcript--content-list (content)
  "Return CONTENT as a list when it represents a sequence."
  (cond
   ((vectorp content) (append content nil))
   ((and (listp content)
         (not (agent-ide-transcript--alist-object-p content)))
    content)
   (content (list content))
   (t nil)))

(defun agent-ide-transcript--diff-content-p (content)
  "Return non-nil when CONTENT looks like a file diff object."
  (and (agent-ide-transcript--alist-object-p content)
       (or (map-elt content 'path)
           (map-elt content 'diff)
           (map-elt content 'patch)
           (map-elt content 'output)
           (map-elt content 'text)
           (map-elt content 'changes)
           (map-elt content 'oldText)
           (map-elt content 'newText))
       (member (map-elt content 'type) '("diff" "create" "update" "delete" nil))))

(defun agent-ide-transcript--top-level-diffs (items)
  "Return top-level diff objects from ITEMS without nested duplicates."
  (let ((diffs (seq-filter #'agent-ide-transcript--diff-content-p items)))
    (or (seq-filter (lambda (diff)
                      (not (map-elt diff 'changes)))
                    diffs)
        diffs)))

(defun agent-ide-transcript--diff-action (diff)
  "Return a short action string for DIFF."
  (or (map-elt diff 'type)
      (cond
       ((and (not (map-elt diff 'oldText))
             (map-elt diff 'newText))
        "create")
       ((and (map-elt diff 'oldText)
             (not (map-elt diff 'newText)))
        "delete")
       (t "diff"))))

(defun agent-ide-transcript--count-lines (text)
  "Return number of lines in TEXT."
  (if (stringp text)
      (1+ (cl-count ?\n text))
    0))

(defun agent-ide-transcript--diff-stat (diff)
  "Return a compact change stat for DIFF."
  (let* ((stats (agent-ide-transcript--diff-text-stats
                 (agent-ide-transcript--diff-text diff)))
         (old-lines (or (plist-get stats :removed)
                        (agent-ide-transcript--count-lines
                         (map-elt diff 'oldText))))
         (new-lines (or (plist-get stats :added)
                        (agent-ide-transcript--count-lines
                         (map-elt diff 'newText))))
         (lines (or (plist-get stats :line-count)
                    (max old-lines new-lines))))
    (format "+%d/-%d, %d %s"
            new-lines
            old-lines
            lines
            (if (= lines 1) "line" "lines"))))

(defun agent-ide-transcript--file-name (path)
  "Return a compact display name for PATH."
  (if (stringp path)
      (file-name-nondirectory (directory-file-name path))
    "file"))

(defun agent-ide-transcript--apply-patch-file-paths (text)
  "Return file paths mentioned by an apply-patch TEXT."
  (when (stringp text)
    (let (paths)
      (dolist (line (split-string text "\n"))
        (when (string-match
               (rx line-start
                   "*** "
                   (or "Add" "Update" "Delete")
                   " File: "
                   (group (+ nonl))
                   line-end)
               line)
          (push (match-string 1 line) paths)))
      (nreverse paths))))

(defun agent-ide-transcript--apply-patch-file-diff-header (kind path move-to)
  "Return unified diff file header for apply-patch KIND PATH MOVE-TO."
  (pcase kind
    ("Add" (format "--- /dev/null\n+++ b/%s" path))
    ("Delete" (format "--- a/%s\n+++ /dev/null" path))
    (_ (format "--- a/%s\n+++ b/%s" path (or move-to path)))))

(defun agent-ide-transcript--apply-patch-diff-text (text)
  "Convert apply-patch TEXT to a unified-diff-like string, or nil."
  (when (and (stringp text)
             (string-match-p (rx line-start "*** Begin Patch") text))
    (let (sections kind path move-to body)
      (cl-labels
          ((flush-section
            ()
            (when path
              (push (string-join
                     (cons (agent-ide-transcript--apply-patch-file-diff-header
                            kind path move-to)
                           (nreverse body))
                     "\n")
                    sections))
            (setq kind nil
                  path nil
                  move-to nil
                  body nil)))
        (dolist (line (split-string text "\n"))
          (cond
           ((string-match
             (rx line-start
                 "*** "
                 (group (or "Add" "Update" "Delete"))
                 " File: "
                 (group (+ nonl))
                 line-end)
             line)
            (flush-section)
            (setq kind (match-string 1 line)
                  path (match-string 2 line)))
           ((and path
                 (string-match
                  (rx line-start "*** Move to: " (group (+ nonl)) line-end)
                  line))
            (setq move-to (match-string 1 line)))
           ((or (string-match-p
                 (rx line-start "*** " (or "Begin Patch" "End Patch") line-end)
                 line)
                (string-match-p
                 (rx line-start "*** End of File" line-end)
                 line))
            nil)
           (path
            (push line body))))
        (flush-section)
        (when sections
          (string-join (nreverse sections) "\n\n"))))))

(defun agent-ide-transcript--diff-text-has-file-header-p (diff path)
  "Return non-nil when DIFF already contains a file header for PATH."
  (or (string-match-p (rx line-start "diff --") diff)
      (and path
           (string-match-p (regexp-quote (format "+++ %s" path)) diff))
      (and path
           (string-match-p (regexp-quote (format "+++ b/%s" path)) diff))
      (and path
           (string-match-p (regexp-quote (format "*** Update File: %s" path))
                           diff))
      (and path
           (string-match-p (regexp-quote (format "*** Add File: %s" path))
                           diff))
      (and path
           (string-match-p (regexp-quote (format "*** Delete File: %s" path))
                           diff))))

(defun agent-ide-transcript--wrap-headerless-diff (path diff)
  "Return DIFF wrapped with standard file headers for PATH."
  (string-join
   (list (format "diff --git a/%s b/%s" path path)
         (format "--- a/%s" path)
         (format "+++ b/%s" path)
         diff)
   "\n"))

(defun agent-ide-transcript--diff-lines (text)
  "Return TEXT split into diff lines without trailing newline markers."
  (if (stringp text)
      (split-string (string-trim-right text "\n") "\n")
    nil))

(defun agent-ide-transcript--diff-file-header (side path text)
  "Return unified diff file header on SIDE for PATH and TEXT."
  (if (or (null text) (string-empty-p text))
      (format "%s /dev/null" side)
    (format "%s %s/%s"
            side
            (if (string= side "---") "a" "b")
            path)))

(defun agent-ide-transcript--normalize-computed-diff (raw path old-text new-text)
  "Normalize RAW unified diff for PATH between OLD-TEXT and NEW-TEXT."
  (let ((old-header (agent-ide-transcript--diff-file-header "---" path old-text))
        (new-header (agent-ide-transcript--diff-file-header "+++" path new-text))
        (result nil)
        (state 'start))
    (dolist (line (split-string raw "\n"))
      (pcase state
        ('start
         (cond
          ((string-prefix-p "--- " line)
           (push old-header result)
           (setq state 'old))
          ((string-prefix-p "Binary files " line)
           (push line result)
           (setq state 'done))))
        ('old
         (when (string-prefix-p "+++ " line)
           (push new-header result)
           (setq state 'body)))
        ('body
         (push line result))
        (_ nil)))
    (string-trim-right (string-join (nreverse result) "\n") "\n")))

(defun agent-ide-transcript--unified-diff-fallback (path old-text new-text)
  "Return naive unified diff when external diff is unavailable."
  (let* ((path (or path "unknown"))
         (old-text (or old-text ""))
         (new-text (or new-text ""))
         (old-lines (agent-ide-transcript--diff-lines old-text))
         (new-lines (agent-ide-transcript--diff-lines new-text))
         (old-count (length old-lines))
         (new-count (length new-lines)))
    (string-join
     (append
      (list (agent-ide-transcript--diff-file-header "---" path old-text)
            (agent-ide-transcript--diff-file-header "+++" path new-text)
            (format "@@ -1,%d +1,%d @@" old-count new-count))
      (mapcar (lambda (line) (concat "-" line)) old-lines)
      (mapcar (lambda (line) (concat "+" line)) new-lines))
     "\n")))

(defun agent-ide-transcript--compute-unified-diff-from-text (path old-text new-text)
  "Return unified diff for PATH comparing OLD-TEXT to NEW-TEXT."
  (let* ((path (or path "unknown"))
         (old-text (or old-text ""))
         (new-text (or new-text "")))
    (cond
     ((string= old-text new-text)
      nil)
     ((not (executable-find "diff"))
      (agent-ide-transcript--unified-diff-fallback path old-text new-text))
     (t
      (let ((old-file (make-temp-file "agent-ide-old"))
            (new-file (make-temp-file "agent-ide-new")))
        (unwind-protect
            (progn
              (with-temp-file old-file (insert old-text))
              (with-temp-file new-file (insert new-text))
              (let ((raw (with-temp-buffer
                           (call-process "diff" nil t nil "-U3" old-file new-file)
                           (buffer-string))))
                (if (string-empty-p (string-trim raw))
                    (agent-ide-transcript--unified-diff-fallback
                     path old-text new-text)
                  (agent-ide-transcript--normalize-computed-diff
                   raw path old-text new-text))))
          (when (file-exists-p old-file) (delete-file old-file))
          (when (file-exists-p new-file) (delete-file new-file))))))))

(defun agent-ide-transcript--unified-diff (diff)
  "Return a compact unified diff string for DIFF."
  (agent-ide-transcript--compute-unified-diff-from-text
   (or (map-elt diff 'path) "unknown")
   (map-elt diff 'oldText)
   (map-elt diff 'newText)))

(defun agent-ide-transcript--unified-diffs (diffs)
  "Return unified diff text for DIFFS."
  (string-join
   (delq nil
         (mapcar #'agent-ide-transcript--diff-text diffs))
   "\n\n"))

(defun agent-ide-transcript--diff-text (diff)
  "Extract normalized diff text from DIFF."
  (let ((item-diff (or (map-elt diff 'diff)
                       (map-elt diff 'patch)
                       (map-elt diff 'output)
                       (map-elt diff 'text)))
        (path (map-elt diff 'path)))
    (cond
     ((and (stringp item-diff)
           (not (string-empty-p (string-trim item-diff))))
      (let ((normalized (or (agent-ide-transcript--apply-patch-diff-text
                             item-diff)
                            item-diff)))
        (if (and path
                 (not (agent-ide-transcript--diff-text-has-file-header-p
                       normalized path)))
            (agent-ide-transcript--wrap-headerless-diff path normalized)
          normalized)))
     ((map-elt diff 'changes)
      (agent-ide-transcript--unified-diffs
       (agent-ide-transcript--content-list (map-elt diff 'changes))))
     ((or (map-elt diff 'oldText)
          (map-elt diff 'newText))
      (agent-ide-transcript--unified-diff diff)))))

(defun agent-ide-transcript--diff-text-stats (diff-text)
  "Return a plist summarizing DIFF-TEXT."
  (when (and (stringp diff-text)
             (not (string-empty-p (string-trim diff-text))))
    (let ((added 0)
          (removed 0)
          filename)
      (dolist (line (split-string diff-text "\n"))
        (cond
         ((and (not filename)
               (string-match
                (rx line-start "diff --git " (? "a/")
                    (group (+ (not (any " \n")))))
                line))
          (setq filename (match-string 1 line)))
         ((string-match (rx line-start "+++" (+ space) (? "b/")
                            (group (+ (not (any " \n")))))
                        line)
          (setq filename (or filename (match-string 1 line))))
         ((and (string-prefix-p "+" line)
               (not (string-prefix-p "+++" line)))
          (setq added (1+ added)))
         ((and (string-prefix-p "-" line)
               (not (string-prefix-p "---" line)))
          (setq removed (1+ removed)))))
      (list :filename (or filename "changes")
            :added added
            :removed removed
            :line-count (agent-ide-transcript--count-lines diff-text)))))

(defun agent-ide-transcript--format-diff-line (diff &optional controls)
  "Return a Codex-like summary line for DIFF.
When CONTROLS is non-nil, append inline diff action buttons."
  (let* ((diff-text (agent-ide-transcript--diff-text diff))
         (stats (agent-ide-transcript--diff-text-stats diff-text))
         (path (or (plist-get stats :filename)
                   (map-elt diff 'path)
                   "unknown"))
         (action (agent-ide-transcript--diff-action diff)))
    (format "  └ ((type . %s)) %s\n  └ diff: %s (%s)%s"
            action
            path
            (agent-ide-transcript--file-name path)
            (agent-ide-transcript--diff-stat diff)
            (if controls " [expand] [open diff]" ""))))

(defun agent-ide-transcript--format-diff-lines (diffs)
  "Return Codex-like summary lines for DIFFS."
  (let ((index 0)
        (count (length diffs)))
    (mapconcat (lambda (diff)
                 (setq index (1+ index))
                 (agent-ide-transcript--format-diff-line diff (= index count)))
               diffs
               "\n")))

(defun agent-ide-transcript--command-output-text (tool-call)
  "Return readable output text for TOOL-CALL."
  (agent-ide-transcript--content-text (map-elt tool-call 'content)))

(defun agent-ide-transcript--line-count-label (text)
  "Return compact line count label for TEXT."
  (let ((lines (agent-ide-transcript--count-lines text)))
    (format "%d %s" lines (if (= lines 1) "line" "lines"))))

(defun agent-ide-transcript--tool-detail-text (tool-call)
  "Return expanded detail text for TOOL-CALL."
  (agent-ide-transcript--nonblank-string
   (agent-ide-transcript--tool-body tool-call)))

(defun agent-ide-transcript--tool-summary-text (tool-call)
  "Return compact summary text for TOOL-CALL."
  (let* ((raw (map-elt tool-call 'rawInput))
         (input (when raw
                  (seq-some
                   (lambda (key)
                     (agent-ide-transcript--command-string (map-elt raw key)))
                   '(query queries pattern path file url)))))
    (or (agent-ide-transcript--nonblank-string (map-elt tool-call 'title))
        input
        (agent-ide-transcript--nonblank-string (map-elt tool-call 'name))
        (agent-ide-transcript--nonblank-string (map-elt tool-call 'toolName))
        (agent-ide-transcript--nonblank-string (map-elt tool-call 'kind)))))

(defun agent-ide-transcript--tool-summary-body (summary detail)
  "Return compact body for SUMMARY and optional DETAIL."
  (format "  └ %s%s"
          (or summary "Tool call")
          (if detail " [expand]" "")))

(defun agent-ide-transcript--codex-tool-display (tool-call &optional update-only)
  "Return plist display data for TOOL-CALL in a Codex-like style.
When UPDATE-ONLY is non-nil, omit fallback titles so the renderer keeps the
existing tool heading."
  (let* ((raw (map-elt tool-call 'rawInput))
         (content (map-elt tool-call 'content))
         (items (agent-ide-transcript--content-list content))
         (diffs (agent-ide-transcript--top-level-diffs items))
         (command (agent-ide-transcript--raw-command-string raw))
         (tool-id (agent-ide-transcript--tool-id tool-call))
         (title (agent-ide-transcript--tool-title tool-call))
         (summary (agent-ide-transcript--tool-summary-text tool-call))
         (detail (agent-ide-transcript--tool-detail-text tool-call)))
    (cond
     (diffs
      (list :title (format "Prepared %d file %s"
                           (length diffs)
                           (if (= (length diffs) 1) "change" "changes"))
            :body (agent-ide-transcript--format-diff-lines diffs)
            :expanded-output (agent-ide-transcript--unified-diffs diffs)))
     (command
      (let ((output (agent-ide-transcript--command-output-text tool-call)))
        (list :title "Ran command"
              :body (string-join
                     (delq nil
                           (list (format "  $ %s" command)
                                 (when (and output
                                            (not (string-empty-p
                                                  (string-trim output))))
                                   (format "  └ output: %s [expand]"
                                           (agent-ide-transcript--line-count-label
                                            output)))))
                     "\n")
              :expanded-output output)))
     (t
      (list :title (and (not update-only)
                        (not (agent-ide-transcript--tool-title-id-p
                              title
                              tool-id))
                        (or title summary "Tool call"))
            :body (when (or summary (and detail (not update-only)))
                    (agent-ide-transcript--tool-summary-body summary detail))
            :expanded-output detail)))))

(defun agent-ide-transcript--tool-title-id-p (title tool-id)
  "Return non-nil when TITLE is just a generated TOOL-ID."
  (and (stringp title)
       (or (equal title tool-id)
           (string-match-p "\\`\\(?:call\\|fc\\)_[[:alnum:]_]+\\'" title))))

(defun agent-ide-transcript--tool-title-cache-key (tool-id)
  "Return metadata key for TOOL-ID title cache."
  (intern (format ":tool-title-%s" tool-id)))

(defun agent-ide-transcript--tool-id (tool-call)
  "Return stable renderer key for TOOL-CALL."
  (or (map-elt tool-call 'toolCallId)
      (map-elt tool-call 'id)
      (format "tool-%s" (sxhash tool-call))))

(defun agent-ide-transcript--format-tool-argument (title)
  "Return TITLE formatted as a compact tool argument line."
  (when-let* ((title (agent-ide-transcript--nonblank-string title)))
    (format "  └ %s" title)))

(defun agent-ide-transcript--tool-output (tool-call)
  "Return output text carried by TOOL-CALL content."
  (let ((content (agent-ide-transcript--content-text
                  (map-elt tool-call 'content))))
    (agent-ide-transcript--nonblank-string content)))

(defun agent-ide-transcript--join-tool-body (&rest parts)
  "Join nonblank PARTS into a compact tool body."
  (string-join
   (delq nil
         (mapcar #'agent-ide-transcript--nonblank-string parts))
   "\n"))

(defun agent-ide-transcript--tool-argument-body (tool-call)
  "Return the argument line for TOOL-CALL.
ACP `tool_call_update' events report arguments in their `title' field."
  (agent-ide-transcript--format-tool-argument
   (map-elt tool-call 'title)))

(defun agent-ide-transcript--tool-body (tool-call)
  "Return body text for TOOL-CALL."
  (let* ((raw (map-elt tool-call 'rawInput))
         (command (agent-ide-transcript--raw-command-string raw))
         (description (map-elt raw 'description))
         (content (agent-ide-transcript--content-text
                   (map-elt tool-call 'content)))
         (parts nil))
    (when command
      (push (format "Command: %s" command) parts))
    (when description
      (push (format "Description: %s" description) parts))
    (when (and content (not (string-empty-p content)))
      (push content parts))
    (string-join (nreverse parts) "\n\n")))

(defun agent-ide-transcript--render-tool (session update)
  "Render ACP tool UPDATE for SESSION."
  (let* ((tool-id (agent-ide-transcript--tool-id update))
         (display (agent-ide-transcript--codex-tool-display update)))
    (agent-ide-renderer-update-tool
     session
     tool-id
     (plist-get display :title)
     (plist-get display :body)
     (map-elt update 'status)
     nil
     (plist-get display :expanded-output))))

(defun agent-ide-transcript--render-tool-update (session update)
  "Render ACP tool UPDATE for SESSION."
  (let* ((tool-id (agent-ide-transcript--tool-id update))
         (display (agent-ide-transcript--codex-tool-display update t)))
    (agent-ide-renderer-update-tool
     session
     tool-id
     (plist-get display :title)
     (plist-get display :body)
     (map-elt update 'status)
     nil
     (plist-get display :expanded-output))))

(defun agent-ide-transcript--format-plan (entries)
  "Return readable plan ENTRIES."
  (cond
   ((vectorp entries)
    (mapconcat (lambda (entry)
                 (format "- %s%s"
                         (or (map-elt entry 'content)
                             (map-elt entry 'text)
                             (format "%S" entry))
                         (if-let* ((status (map-elt entry 'status)))
                             (format " [%s]" status)
                           "")))
               (append entries nil)
               "\n"))
   ((stringp entries) entries)
   (t (format "%S" entries))))

(defun agent-ide-transcript-handle-notification (session notification)
  "Handle ACP NOTIFICATION for SESSION."
  (pcase (map-elt notification 'method)
    ("session/update"
     (let* ((update (agent-ide--get-in notification '(params update)))
            (kind (map-elt update 'sessionUpdate)))
       (pcase kind
         ("agent_message_chunk"
          (let ((text (agent-ide-transcript--content-text
                       (map-elt update 'content))))
            (agent-ide-renderer-append-stream-chunk session 'message text)
            (run-hook-with-args 'agent-ide-message-chunk-functions
                                session text)))
         ("agent_thought_chunk"
          (agent-ide-renderer-append-stream-chunk
           session
           'thought
           (agent-ide-transcript--content-text (map-elt update 'content))))
	 ("tool_call"
	  (agent-ide-transcript--render-tool session update))
         ("tool_call_update"
          (agent-ide-transcript--render-tool-update session update))
         ("plan"
          (agent-ide-renderer-update-tool
           session
           "plan"
           "Plan"
           (agent-ide-transcript--format-plan (map-elt update 'entries))
           nil))
         ("available_commands_update"
          (agent-ide--session-metadata-put
           session :available-commands (map-elt update 'availableCommands))
          (agent-ide-renderer-update-tool
           session
           "available-commands"
           "Available commands"
           (agent-ide-transcript--content-text
            (map-elt update 'availableCommands))
           nil
           nil
           nil
           t))
         ("current_mode_update"
          (setf (agent-ide-session-modes session) update)
          (agent-ide-renderer-update-header session))
         ("usage_update"
          (setf (agent-ide-session-usage session) update)
          (agent-ide-renderer-update-header session))
         (_
          (when acp-logging-enabled
            (agent-ide-renderer-update-tool
             session
             (format "notification-%s" (sxhash notification))
             (format "Unhandled update: %s" kind)
             (agent-ide--json-string notification)
             nil))))))
    (_
     (when acp-logging-enabled
       (agent-ide-renderer-update-tool
        session
        (format "notification-%s" (sxhash notification))
        (format "Unhandled notification: %s" (map-elt notification 'method))
        (agent-ide--json-string notification)
        nil)))))

(defun agent-ide-transcript--permission-options (request)
  "Return permission options from REQUEST as a list."
  (let ((options (agent-ide--get-in request '(params options))))
    (cond
     ((vectorp options) (append options nil))
     ((listp options) options)
     (t nil))))

(defun agent-ide-transcript--handle-permission (session request)
  "Handle session/request_permission REQUEST for SESSION."
  (let* ((tool-call (agent-ide--get-in request '(params toolCall)))
         (tool-id (or (map-elt tool-call 'toolCallId)
                      (format "permission-%s" (map-elt request 'id))))
         (key (format "permission-%s" tool-id))
         (request-id (map-elt request 'id))
         (title (agent-ide-transcript--tool-title tool-call))
         (body (agent-ide-transcript--tool-body tool-call))
         (options (agent-ide-transcript--permission-options request)))
    (agent-ide-renderer-insert-permission
     session
     key
     title
     body
     options
     (lambda (option-id)
       (agent-ide-protocol-respond-permission session request-id option-id)
       (agent-ide-renderer-update-tool
	session
	key
        "[Approval resolved]"
        (if option-id
            (format "Selected: %s" option-id)
          "Cancelled")
        nil)))))

(defun agent-ide-transcript-handle-request (session request)
  "Handle incoming ACP REQUEST for SESSION."
  (pcase (map-elt request 'method)
    ("session/request_permission"
     (agent-ide-transcript--handle-permission session request))
    ("fs/read_text_file"
     (agent-ide-protocol-handle-fs-read session request))
    ("fs/write_text_file"
     (agent-ide-protocol-handle-fs-write session request))
    (_
     (acp-send-response
      :client (agent-ide-session-client session)
      :response `((:request-id . ,(map-elt request 'id))
                  (:error . ,(acp-make-error
                              :code -32601
                              :message (format "Method not found: %s"
                                               (map-elt request 'method))))))
     (agent-ide-renderer-append-error
      session
      (format "Unhandled ACP request: %s" (map-elt request 'method))))))

(defun agent-ide-transcript-handle-error (session error)
  "Handle agent process ERROR for SESSION."
  (agent-ide-renderer-append-error
   session
   (or (map-elt error 'message)
       (format "%S" error))))

(provide 'agent-ide-transcript)

;;; agent-ide-transcript.el ends here
