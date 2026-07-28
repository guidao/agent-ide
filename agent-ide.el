;;; agent-ide.el --- Agent inside Emacs -*- lexical-binding: t; -*-

;; Author: Feng
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (acp "0.11.1") (valign "3.1"))
;; Keywords: ai, agent, acp

;;; Commentary:

;; Native Emacs transcript UI for Agent over ACP.

;;; Code:

;;;###autoload
(defgroup agent-ide nil
  "Agent integration over ACP."
  :group 'tools
  :prefix "agent-ide-")

;;;###autoload
(defcustom agent-ide-command '("cursor-agent" "acp")
  "Command used to start the agent ACP backend.
The first element is the executable and the rest are arguments."
  :type '(repeat string)
  :group 'agent-ide)

;;;###autoload
(defcustom agent-ide-environment nil
  "Environment variables added when starting `agent-ide-command'."
  :type '(repeat string)
  :group 'agent-ide)

;;;###autoload
(defcustom agent-ide-buffer-name-prefix "agent"
  "Prefix used when creating Agent IDE session buffers."
  :type 'string
  :group 'agent-ide)

;;;###autoload
(defcustom agent-ide-new-session-split nil
  "Window split direction for new Agent IDE sessions."
  :type '(choice (const :tag "Current window" nil)
                 (const :tag "Right side" vertical)
                 (const :tag "Bottom side" horizontal))
  :group 'agent-ide)

;;;###autoload
(defcustom agent-ide-select-window-on-open t
  "Whether opening a Agent IDE buffer selects its window."
  :type 'boolean
  :group 'agent-ide)

;;;###autoload
(defcustom agent-ide-text-file-capabilities t
  "Whether Agent IDE advertises ACP text-file capabilities."
  :type 'boolean
  :group 'agent-ide)

(defcustom agent-ide-pre-submit-functions nil
  "Abnormal hook run before delivering a user prompt.
Each function is called as (FUNCTION SESSION PROMPT).
If any function returns non-nil, it has handled the submission and
`agent-ide-submit' must not deliver PROMPT.  If all return nil,
PROMPT is delivered with `agent-ide-deliver-prompt'."
  :type 'hook
  :group 'agent-ide)

(defcustom agent-ide-prompt-placeholder-text "Tell Agent what to do..."
  "Placeholder text shown in an empty idle Agent IDE prompt."
  :type 'string
  :group 'agent-ide)

(defcustom agent-ide-running-placeholder-text "Working..."
  "Placeholder text shown while the agent is working."
  :type 'string
  :group 'agent-ide)

(defcustom agent-ide-status-placeholder-text-alist
  '(("interrupting" . "Interrupting...")
    ("creating-session" . "Creating session...")
    ("initializing" . "Initializing..."))
  "Alist mapping Agent IDE statuses to prompt placeholder text."
  :type '(alist :key-type string :value-type string)
  :group 'agent-ide)

;;;###autoload
(defcustom agent-ide-model nil
  "Default model ID set after session creation via `session/set_model'.
When non-nil, the model is applied immediately after each new session
is created.  Use `agent-ide-set-model' to switch models interactively."
  :type '(choice (const :tag "Agent default" nil)
                 (string :tag "Model ID"))
  :group 'agent-ide)

;;;###autoload
(defcustom agent-ide-mcp-servers []
  "ACP MCP servers passed to `session/new'."
  :type 'sexp
  :group 'agent-ide)

(require 'agent-ide-core)
(require 'agent-ide-renderer)
(require 'agent-ide-session-mode)
(require 'agent-ide-protocol)
(require 'agent-ide-transcript)
(require 'agent-ide-session)
(require 'agent-ide-sidebar)

(provide 'agent-ide)

;;; agent-ide.el ends here
