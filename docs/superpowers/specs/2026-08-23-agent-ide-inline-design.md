# agent-ide-inline Design (v2)

Date: 2026-08-23 (v2 revises the same-day v1; v1 implemented the wrong
interaction model — gptel-rewrite style — and was reworked)
Status: approved in chat 2026-08-23

## Goal

gptel-inline-style interaction backed by an agent-ide session: a small
prompt window bound to the project's persistent agent session; responses
stream into an overlay viewport at point in the buffer where the command
was invoked. The transcript buffer runs in the background and keeps the
full conversation. No buffer text is ever rewritten.

Reference: karthink/gptel-inline v0.0.5 (installed locally at
`~/.emacs.d/elpa/gptel-inline/`).

## Requirements

1. `agent-ide-inline` works from any buffer; opens a prompt window below
   the current window (0.33 height, dedicated).
2. Prompt window keys: `C-c RET`/`C-c C-m` send, `C-c SPC` cycle
   reference, `C-c ?` help, `C-c C-b` switch session, `C-c C-v` visit
   transcript, `C-c C-k` quit. Header line shows key hints, reference
   type, and the session buffer name.
3. Reference cycling highlights "things at point" in the origin buffer
   (region → line → defun → window → buffer → none, mode-dependent) with
   the `secondary-selection` face; `SPC` repeats, `C-g` clears. Reference
   text is injected as a markdown fenced block with buffer name and line
   range.
4. On send the prompt window closes; the response streams into an overlay
   viewport at the origin point:
   - after-string viewport: hrule + header (scroll indicators, status) +
     height-slice (default 8 lines) + hrule + session buffer name
   - markdown rendering via `agent-ide-renderer-render-markdown-region`
     on a hidden source buffer (code fences, headings, inline code,
     emphasis, links)
   - scroll: wheel, `C-M-n`/`C-M-p`; page: `C-M-v`/`C-M-S-v`; resize:
     numeric prefix on resize command or menu `+`/`-`; index clamped
   - action menu (`M-RET` or `mouse-1`, `read-multiple-choice`):
     v visit / r reply / c clear / w copy / + / − / q
   - prefix on clear also cancels the running turn
5. Session resolution: strict `file-truename` match on
   `agent-ide--working-directory`; no match → start a new session. Busy
   session → `user-error`. New sessions are waited on with a 0.3s poll
   timer (30s timeout).
6. The turn is visible in the transcript (pollution allowed): the user
   prompt appears as an "Inline: …" status line, the streamed reply as a
   normal assistant message.
7. Chunk/response/failure wiring reuses the three hooks added for v1
   (`agent-ide-message-chunk-functions`,
   `agent-ide-prompt-response-functions`,
   `agent-ide-prompt-failure-functions`) — unchanged.
8. v1 rewrite commands (`agent-ide-inline-rewrite`, accept/reject) are
   removed.

## Architecture

Single consumer file `agent-ide-inline.el` rewritten in place; no changes
to other package files (the Task-1 hooks in `agent-ide-transcript.el` and
`agent-ide-protocol.el` stay as-is). State is global (hooks fire in the
session buffer):

- `agent-ide-inline--overlays` — alist `(SESSION . OVERLAY)`; one
  streaming overlay per session; completed overlays stay visible until
  cleared and are no longer updated.
- Per-overlay plist under `'agent-ide-inline`: `:session`,
  `:session-buffer`, `:src` (hidden render buffer), `:header`, `:done`.
- Prompt-window state is buffer-local (`agent-ide-inline--session`,
  `--origin`, `--reference-ov`, `--reference-type`); the prompt buffer is
  killed on send/quit.

## Data flow

```
agent-ide-inline → resolve session (strict, else start) → prompt window
  (origin marker + reference highlight)
send → prompt + reference text → status line in transcript →
  response overlay at origin → send-when-ready → protocol-send-prompt
chunk hook → append into :src buffer → render-markdown-region →
  viewport re-render (slice, clamped scroll)
response hook → :done t, re-render, message "M-RET for actions"
failure hook → header shows error, :done t
clear → kill :src, delete overlay, drop alist entry (prefix: cancel turn)
```

## Interaction details

- Reference bounds: region/window/buffer cases are origin-independent;
  line/defun/sentence use the origin position.
- Viewport render slices the src buffer via `pos-bol`/`pos-eol` line
  arithmetic; the scroll index is clamped to `[0, len-height]`.
- Markdown chunks are rendered incrementally (like the transcript), so
  fences fontify progressively.
- Overlay keymap is attached to the after-string; a buffer-local minor
  mode (`agent-ide-inline--response-overlay-mode`) supplies keyboard
  actions while the viewport is visible.

## Error handling

| Case | Behavior |
|---|---|
| No session in prompt window / empty prompt | `user-error` |
| Session busy | `user-error`, prompt window stays open |
| New session failed / readiness timeout | error shown in viewport header |
| Prompt request failure | failure hook → header shows error |
| Origin buffer killed before response | response still rendered in transcript; no viewport |

## Testing

ERT (repo conventions). `agent-ide-inline-test.el` rewritten: 25 tests
covering hook wiring (3), session resolution/readiness (5), reference
bounds/types/text (5), prompt window (3), viewport streaming/scroll/
resize/copy/clear (7), dispatch (2), turn-end (2).

## Scope

In V2: prompt window, reference cycling, streaming markdown viewport,
action menu, visit/reply/copy/clear/resize, abort-on-prefix-clear.

Out of scope (follow-ups): mouse drag of the viewport, org-element
references, `gptel-inline-append`-style multi-part prompts, tool-call
confirmation inside the viewport (transcript handles permissions today).
