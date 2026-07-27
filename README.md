# agent-ide

> Native Emacs transcript UI for AI agents over the [Agent Communication Protocol](https://agentcommunicationprotocol.dev/) (ACP).
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
| `agent-ide-set-model` | `C-c C-s` | Switch the agent model (completing-read) |
| `agent-ide-yank-region` | `C-c C-y` | Insert the active region as file+line context |

### Prompt keys

| Key | Action |
|---|---|
| `C-c RET` | Submit prompt |
| `M-p` / `M-n` | Cycle through prompt history |
| `/` | Insert a slash for agent commands |
| `TAB` | Complete slash commands (with descriptions) |

### Header line

The header shows: **model name** · **project directory** · **context usage** / **last-turn tokens**.

### Transcript features

- **Markdown rendering** — code fences (syntax-highlighted), inline code, bold/italic, links, headings, and pipe tables (via `valign`).
- **Streaming messages** — agent text appears word-by-word, markdown-rendered as it arrives.
- **Thinking blocks** — agent reasoning is shown inside foldable blocks (collapsed by default).
- **Tool calls** — each tool invocation gets a compact summary with expandable output; diffs are syntax-highlighted.
- **Permission prompts** — inline `[accept]` / `[decline]` / `[cancel]` buttons when the agent requests approval.
- **Plan rendering** — when the agent produces a plan, entries are listed inline.
- **Read-only transcript** — all agent output is frozen; only the current prompt is editable.

### Session sidebar

A left sidebar lists live sessions (status, model, usage). Open with `M-x agent-ide-sidebar`.

| Key | Action |
|---|---|
| `RET` / mouse-1 | Display that session’s buffer via existing `agent-ide--display-buffer` |
| `n` / `p` | Move by entry (two physical lines per entry) |
| `k` | Kill session (confirm when `agent-ide-sidebar-confirm-kill` is non-nil) |
| `+` / `c` | `agent-ide-new-session` |
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
| `agent-ide-prompt-placeholder-text` | `"Tell Agent what to do..."` | Empty-prompt placeholder |
| `agent-ide-running-placeholder-text` | `"Working..."` | Placeholder while the agent processes |
| `agent-ide-sidebar-width` | `0.22` | Left side-window width |
| `agent-ide-sidebar-auto-show` | `t` | Auto-show on session create; ignored for refresh while user-dismissed |
| `agent-ide-sidebar-confirm-kill` | `t` | Confirm before kill |

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

File load order: `core` → `protocol` → `renderer` → `session-mode` → `session` → `transcript` → `sidebar` → `agent-ide`.

## Architecture

```
agent-ide.el              Entry point, defcustom, require all
├── agent-ide-core.el     Session struct, helpers, buffer management
├── agent-ide-renderer.el Transcript rendering, markdown, diff, folds, faces
├── agent-ide-protocol.el ACP bridge (init, prompt, cancel, fs ops)
├── agent-ide-session-mode.el Major mode, keymaps, completion, edit guard
├── agent-ide-transcript.el   ACP event dispatch (notifications, requests)
├── agent-ide-session.el  User commands, lifecycle, yank-region, set-model
└── agent-ide-sidebar.el  Session list side window, switch/kill/new
```

## License

[MIT](LICENSE) — feel free to use, modify, and share.
