# agent-ide

> Native Emacs transcript UI for AI agents over the [Agent Client Protocol](https://agentclientprotocol.com/) (ACP).
> UI design inspired by [codex-ide](https://github.com/agnt-gg/codex-ide).

**agent-ide** brings any ACP-compatible coding agent directly into Emacs. It renders agent messages, tool calls, diffs, and permission prompts in a rich, read-only transcript buffer — with a fully editable prompt at the bottom. Think of it as Emacs's answer to agent chat panels, but natively integrated: keyboard-driven, theme-aware, and hackable in Emacs Lisp.

## Requirements

- **Emacs** ≥ 29.1
- **[acp.el](https://github.com/xenodium/acp.el)** ≥ 0.11.1 — ACP client library
- **[valign](https://github.com//casouri/valign)** ≥ 3.1 — visual table alignment

## Installation

### With straight.el

```elisp
(straight-use-package
 '(agent-ide :type git :host github :repo "guidao/agent-ide"))

;; Optional: load it immediately
(require 'agent-ide)
```

### Manual

Clone this repository somewhere on your `load-path`:

```sh
git clone https://github.com/YOUR_USER/agent-ide.git ~/.emacs.d/agent-ide
```

Then in your init file:

```elisp
(add-to-list 'load-path "~/.emacs.d/agent-ide")
(require 'agent-ide)
```

For deferred loading with `use-package`, register the resume commands too:

```elisp
(use-package agent-ide
  :load-path "~/.emacs.d/agent-ide"
  :commands (agent-ide agent-ide-inline
             agent-ide-resume agent-ide-resume-history))
```

You can then run `M-x agent-ide-resume` directly from a project file or Dired
after starting Emacs. Choose the saved conversation; no preliminary new session
is needed. Selecting the same connected conversation again reuses its buffer.

## Quick Start

1. Make sure you have an ACP-compatible agent binary on your `PATH` (e.g. `cursor-agent`, `claude`, or any agent exposing an ACP subcommand).
2. Configure the agent command:

   ```elisp
   (setq agent-ide-command '("cursor-agent" "acp"))
   ```

3. Open a project and run `M-x agent-ide`.

A transcript buffer opens, the agent connects, and you see a prompt: **> Tell Agent what to do…**

Type your prompt and press `C-c C-m` (or `Return` with a configured binding) to submit.

## Usage

### Session commands

| Command | Keybinding | Description |
|---|---|---|
| `agent-ide` | — | Open a session for the current project (reuses existing) |
| `agent-ide-new-session` | `C-u M-x agent-ide` | Start a fresh session, optionally in a different directory |
| `agent-ide-submit` | `C-c RET` | Submit the current prompt |
| `agent-ide-interrupt` | `C-c C-c` / `C-c C-k` | Stop the agent mid-response |
| `agent-ide-restart` | `C-c C-r` | Kill and restart the current session |
| `agent-ide-resume` | `C-c C-z` | Reconnect a disconnected session, otherwise choose project history; prefix shows all projects |
| `agent-ide-resume-history` | — | Always choose from saved sessions, optionally across all projects with a prefix |
| `agent-ide-set-model` | `C-c C-s` | Switch the agent model (completing-read) |
| `agent-ide-yank-region` | `C-c C-y` | Insert the active region as file+line context |
| `agent-ide-sidebar` | `C-c C-b` | Focus the session sidebar |
| `agent-ide-approve-permission` | `C-c C-a` | Allow the newest pending request once; use `C-u` to always allow |
| `agent-ide-decline-permission` | `C-c C-d` | Decline the newest pending request |
| `agent-ide-select-permission-option` | `C-c C-p` | Choose any offered permission response or cancel |

### Continuing a previous conversation

Run `M-x agent-ide-resume` to choose a saved conversation for the current
project. Use `C-u M-x agent-ide-resume` to search all projects. Candidates
show the first prompt's title, backend, directory, last activity and session ID.
Selecting an already connected conversation opens its existing buffer.

When an agent process exits, its transcript stays open with a `disconnected`
status. Press `C-c C-z` to reconnect in that buffer. Existing messages and
the editable draft are preserved. During restoration you can edit the draft;
submission is enabled once the session is ready. `C-c C-r` still starts a
fresh conversation.

Agent IDE selects the supported ACP method automatically:

| Situation | Method |
|---|---|
| Existing transcript and backend supports resume | `session/resume` |
| History is needed, or backend only supports load | `session/load` |
| Backend only supports resume | `session/resume`, with a notice if earlier messages are unavailable |

A successful load replaces the displayed history, preserving the draft and
rebuilding prompt history for `M-p` / `M-n`. Historical messages do not trigger
inline response overlays. Failed loads keep the previous transcript and offer
retry, choose-session and new-session buttons. Restoration never automatically
resends an interrupted prompt or creates a replacement conversation.

The local index defaults to `~/.emacs.d/agent-ide/sessions.json` (relative to
`user-emacs-directory`). It stores session IDs, directories, backend identities,
titles and timestamps, and survives closing transcript buffers and Emacs. It
does not store full conversations or credentials. Set `agent-ide-history-file`
to another path, or to `nil` to disable persistence; in-memory reconnection
still works. New sessions are recorded as they are created; older sessions
without a local index entry are not discovered in this version.

The backend must retain the session and advertise `loadSession` or
`sessionCapabilities.resume`. To restore a closed buffer, configure the same
`agent-ide-command` used to create it; a different backend is rejected before
launch. The current environment and MCP configuration are used after reopening
Emacs; reconnecting an existing buffer uses its original configuration.
Missing session IDs, removed directories and unsupported capabilities are
reported without falling back to a new conversation.

### Inline interaction (gptel-inline style)

`M-x agent-ide-inline` opens a small prompt window below the current
window, bound to the current project's agent session. The session's
transcript buffer runs in the background and keeps the full conversation.
Responses stream into an overlay viewport at point in the buffer where
you invoked the command.

| Key | Action |
|---|---|
| `C-c RET` / `C-c C-m` | Send the prompt; the window closes and the response streams into a viewport at point |
| `C-c SPC` | Cycle the reference context (region, line, defun, window, buffer); `SPC` repeats, `C-g` clears |
| `C-c ?` | Show key help |
| `C-c C-b` | Switch the conversation to another live session |
| `C-c C-v` | Visit the session's transcript buffer |
| `C-c C-k` | Quit the prompt window |

On the response viewport:

| Key / Mouse | Action |
|---|---|
| `M-RET` or `mouse-1` | Action menu: visit / reply / clear / copy / height± / quit |
| `C-M-n` / `C-M-p`, mouse wheel | Scroll the viewport |
| `C-M-v` / `C-M-S-v` | Page up/down |
| `C-c C-u` (prefix) | Clearing with a prefix also aborts the running turn |

### Prompt keys

| Key | Action |
|---|---|
| `C-c RET` | Submit prompt |
| `M-p` / `M-n` | Cycle through prompt history |
| `/` | Insert a slash for agent commands |
| `TAB` | Complete slash commands (with descriptions) |

### Header line

The header shows: an animated Agent IDE icon · **model name** · **project directory** · **context usage** / **last-turn tokens**.  The transparent PNG is cached in four rotated frames for lightweight animation.

### Transcript features

- **Markdown rendering** — code fences (syntax-highlighted), inline code, bold/italic, links, headings, and pipe tables (via `valign`).
- **Streaming messages** — agent text appears word-by-word, markdown-rendered as it arrives.
- **Thinking blocks** — agent reasoning is shown inside foldable blocks (collapsed by default).
- **Tool calls** — each tool invocation gets a compact summary with expandable output; diffs are syntax-highlighted.
- **Permission prompts** — inline buttons plus cursor-independent shortcuts: `C-c C-a` allows once, `C-u C-c C-a` always allows, `C-c C-d` declines, and `C-c C-p` shows every option.
- **Plan rendering** — when the agent produces a plan, entries are listed inline.
- **Read-only transcript** — all agent output is frozen; only the current prompt is editable.

### Session sidebar

A left sidebar lists live sessions. Each entry shows project/`[status]` (or `[ask]` when a permission prompt is waiting), model, and an optional third line for the active tool/approval. From a session buffer, press `C-c C-b` (or `M-x agent-ide-sidebar`) to focus it.

| Key | Action |
|---|---|
| `RET` / mouse-1 | Display that session’s buffer via existing `agent-ide--display-buffer` |
| `n` / `p` | Move by entry (two physical lines per entry) |
| `k` | Kill session (confirm when `agent-ide-sidebar-confirm-kill` is non-nil) |
| `r` | Restore the session at point |
| `h` | Choose saved sessions for the entry's project; prefix shows all projects |
| `g` | Manual refresh |
| `q` | Hide sidebar (set user-dismissed; do not kill sessions) |

## Configuration

All options are under the `agent-ide` customize group (`M-x customize-group RET agent-ide`).

| Option | Default | Description |
|---|---|---|
| `agent-ide-command` | `("cursor-agent" "acp")` | The ACP backend command |
| `agent-ide-environment` | `nil` | Extra environment variables for the agent process |
| `agent-ide-buffer-name-prefix` | `"agent"` | Session buffer name prefix (e.g. `*agent:myproject*`) |
| `agent-ide-new-session-split` | `nil` | Where to open new sessions: `nil` (current window), `vertical` (right side), or `horizontal` (bottom) |
| `agent-ide-select-window-on-open` | `t` | Focus the session window when it opens |
| `agent-ide-text-file-capabilities` | `t` | Advertise ACP file read/write to the agent |
| `agent-ide-model` | `nil` | Default model ID applied after session creation |
| `agent-ide-mcp-servers` | `[]` | MCP servers passed to `session/new` |
| `agent-ide-history-file` | `agent-ide/sessions.json` under `user-emacs-directory` | Local session index; `nil` disables persistence |
| `agent-ide-prompt-placeholder-text` | `"Tell Agent what to do..."` | Empty-prompt placeholder |
| `agent-ide-running-placeholder-text` | `"Working..."` | Placeholder while the agent processes |
| `agent-ide-header-icon-animation-interval` | `0.18` | Seconds between rotations of the Agent IDE header icon; `nil` disables animation |
| `agent-ide-sidebar-width` | `0.14` | Left side-window width |
| `agent-ide-sidebar-auto-show` | `nil` | Auto-show on session create when enabled; ignored for refresh while user-dismissed |
| `agent-ide-sidebar-confirm-kill` | `t` | Confirm before kill |
| `agent-ide-latex-preview` | `t` | Preview complete math fragments in graphical Emacs |
| `agent-ide-latex-process` | `dvisvgm` | Org conversion process: `dvisvgm` (SVG) or `dvipng` (PNG) |
| `agent-ide-latex-scale` | `1.0` | Formula image scale |
| `agent-ide-latex-timeout` | `20` | Maximum seconds per formula conversion |

### Previewing math

Formula previews require graphical Emacs with SVG support, `latex` and `dvisvgm`
available in Emacs's `exec-path`. Alternatively, select `dvipng` with PNG support.
Org's LaTeX preview engine runs in a background Emacs process using its default
preamble and packages. It does not load your Org configuration.

Use `$x^2$` or `\(x^2\)` for inline math, and `$$...$$` or `\[...\]` for
display math. Previews appear after the closing delimiter arrives, including in
inline replies. Fenced code blocks (including `latex` blocks) and inline code
keep their source. Dollar-delimited inline math must stay on one line and have
no spaces immediately inside its delimiters; ordinary `$5 and $10` stays text.

The underlying LaTeX text remains available for copying. Missing tools, invalid
formulas and conversion timeouts leave the source visible. Identical formulas
share cached images during the Emacs session.

Run `M-x agent-ide-preview-latex` in a session to preview existing messages or
refresh after changing the scale or theme. This preserves the editable prompt.
For example:

```elisp
(setq agent-ide-latex-scale 1.3)
;; Then run M-x agent-ide-preview-latex in the session.
```

To turn previews off, set `agent-ide-latex-preview` to `nil`; run the same command
to remove existing previews from the current session.

### Example: Right-side panel

```elisp
(setq agent-ide-new-session-split 'vertical)
```

Opens each session in a right-side window at 42% width.

### Example: Different agent backends

```elisp
;; Cursor Agent
(setq agent-ide-command '("cursor-agent" "acp"))

;; Claude Code (if it exposes an ACP subcommand)
(setq agent-ide-command '("claude" "acp"))

;; Custom agent with extra env
(setq agent-ide-command '("my-agent" "--acp")
      agent-ide-environment '("MY_TOKEN=xxx" "DEBUG=1"))
```

### Example: MCP servers

```elisp
(setq agent-ide-mcp-servers
      '[((name . "filesystem")
         (command . "npx")
         (args . ["-y" "@modelcontextprotocol/server-filesystem" "/tmp"]))])
```

## Development

**agent-ide** includes built-in hot-reload support for hacking on the package itself:

| Command | Keybinding | Description |
|---|---|---|
| `agent-ide-load-file` | `C-c C-l` | Reload a single source file |
| `agent-ide-reload-current-file` | — | Reload the file being visited |
| `agent-ide-reload-all` | — | Reload all project files in dependency order |
| `agent-ide-reload-last` | — | Reload the most recently loaded file |

File load order: `core` → `history` → `protocol` → `latex` → `renderer` → `session-mode` → `session` → `transcript` → `sidebar` → `inline` → `agent-ide`.

Run restoration tests with installed `acp` and `valign` packages:

```sh
emacs -Q --batch --eval '(progn (require (quote package)) (package-initialize))' \
  -L . -l acp -l valign -l agent-ide-resume-test.el \
  -f ert-run-tests-batch-and-exit
```

The process integration test uses Python 3 and a local ACP fixture with temporary
session data. It exercises process exit, reconnection, cold restoration and
continued prompting without contacting an agent service.

Run formula preview tests with:

```sh
emacs -Q --batch -L . -l agent-ide-latex-test.el -f ert-run-tests-batch-and-exit
```

The SVG/PNG integration tests use local Org and TeX tools, and skip when those
tools or image types are unavailable. They do not contact an agent service.

## Architecture

```
agent-ide.el              Entry point, defcustom, require all
├── agent-ide-core.el     Session struct, helpers, buffer management
├── agent-ide-history.el  Persistent session index and completion candidates
├── agent-ide-renderer.el Transcript rendering, markdown, diff, folds, faces
├── agent-ide-latex.el    Math recognition and asynchronous Org formula previews
├── agent-ide-protocol.el ACP bridge (init, prompt, cancel, fs ops)
├── agent-ide-session-mode.el Major mode, keymaps, completion, edit guard
├── agent-ide-transcript.el   ACP event dispatch (notifications, requests)
├── agent-ide-session.el  User commands, lifecycle, yank-region, set-model
└── agent-ide-sidebar.el  Session list side window, switch/kill
```

Plugins may register on `agent-ide-pre-submit-functions` to defer submission
(SESSION PROMPT); return non-nil to handle the prompt and call
`agent-ide-deliver-prompt` later.

## License

[MIT](LICENSE) — feel free to use, modify, and share.
