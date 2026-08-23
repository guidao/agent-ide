# agent-ide-inline Design

Date: 2026-08-23
Status: approved (chat design approved 2026-08-23)

## Goal

Add gptel-inline-style interaction to `agent-ide`: select a region in any
buffer, issue a rewrite instruction, watch the agent's proposed replacement
stream into an in-place overlay, then accept (`C-c C-c`) or reject
(`C-c C-k`). Backend is the existing agent-ide session (pi via `pi-acp`,
ACP protocol).

## Requirements

1. `agent-ide-inline-rewrite` works from any buffer with an active region.
2. Proposed text streams into an overlay that visually replaces the region;
   the original buffer text is untouched until accept.
3. Accept replaces the region in a single undoable modification; reject
   restores the original view with no modification.
4. Session resolution is strict by project directory
   (`file-truename` match). No match → start a new session for the current
   working directory. Never fall back to the most-recent session.
5. Inline turns are visible in the transcript (pollution allowed by design).
6. No per-call process startup: inline reuses the long-lived pi-acp process
   of the project's session.
7. Existing agent-ide behavior is unchanged; all edits to existing files are
   additive.

## Architecture

New file `agent-ide-inline.el` implements the feature as a consumer of the
existing session/protocol/transcript infrastructure. Three additive
micro-edits wire extension points:

| File | Change |
|---|---|
| `agent-ide-transcript.el` | In the `agent_message_chunk` branch of `agent-ide-transcript-handle-notification`, run `agent-ide-message-chunk-functions` with `(session text)` after the renderer call |
| `agent-ide-protocol.el` | In `agent-ide-protocol-send-prompt`, run `agent-ide-prompt-response-functions` with `(session response)` at the end of the `:on-success` handler and `agent-ide-prompt-failure-functions` with `(session error)` at the end of the `:on-failure` handler |
| `agent-ide.el` | `(require 'agent-ide-inline)` after `agent-ide-sidebar` |

Rationale for the protocol hooks (a third micro-edit beyond the two
discussed in chat): the inline flow sends its prompt through the canonical
`agent-ide-protocol-send-prompt` path so that status/stream bookkeeping
(`running` → `idle`, stream reset, header updates) stays in one place.
Inline needs a turn-end signal with success/failure distinction; polling the
session status cannot distinguish failure from success. Hooks are the
minimal additive mechanism.

## Interaction flow

```
region → M-x agent-ide-inline-rewrite → instruction (read-string)
→ overlay replaces region visually, streaming chunks appended
→ C-c C-c accept (region replaced, undoable) / C-c C-k reject (restore)
```

- Overlay: from region start to region end with a `display` string; original
  text stays in the buffer, so positions don't drift while streaming.
- While preview is active, buffer-local minor mode
  `agent-ide-inline-preview-mode` (lighter " Inline") binds `C-c C-c`
  (accept) and `C-c C-k` (reject). Keys are defcustoms.
- Streaming state: chunks accumulate in a buffer-local string; each chunk
  re-sets the overlay display string (propertized with a new face
  `agent-ide-inline-preview-face`, default italic comment coloring).
- Before the first chunk arrives, display shows "(Working…)".
- Any external buffer modification while a preview is active cancels the
  preview (accept itself is exempt via a flag). Buffer kill cleans up.
- Accept: `atomic-change-group` delete-region + insert final text, point at
  end of inserted text, preview torn down. Reject: overlay removed, hooks
  unregistered, buffer untouched.
- Turn-end finalization: strip leading/trailing markdown fences and trim;
  empty response → cancel preview with a message.

## Session resolution and lifecycle

`agent-ide-inline--resolve-session`: strict `file-truename` match against
`agent-ide--sessions` on `agent-ide--working-directory`; no match →
`agent-ide--start-session` (which displays the transcript window; acceptable
per decision 1).

New sessions initialize asynchronously (ACP init → `session/new`). Inline
waits via a 0.3s timer polling for non-nil `acp-session-id` and status
"idle"; status "failed" or a 30s deadline aborts with an error and no buffer
modification.

If the resolved session's status is "running", the command signals
`user-error "Agent busy…"` (one ACP turn at a time; interrupt from the
transcript).

## Data flow

1. Build prompt (defcustom `agent-ide-inline-prompt-template` with `%i`
   instruction / `%c` region context slots):

   - instruction
   - constraint: "Do not use tools, do not modify files. Reply with only the
     replacement text, without markdown fences or explanation."
   - region context via existing `agent-ide--format-region-context`
     (`file:line-start-line-end` + content, capped at 20 lines by the
     existing helper).

2. Register buffer-local handlers on `agent-ide-message-chunk-functions`
   (append to overlay) and `agent-ide-prompt-response-functions` /
   `agent-ide-prompt-failure-functions` (finalize / cancel).

3. Append a status line "Inline: <instruction>" to the transcript
   (`agent-ide-renderer-append-status`) so pollution is readable, then send
   via `agent-ide-protocol-send-prompt`.

4. Chunks render in the transcript as usual (allowed pollution) and are
   mirrored into the overlay via the chunk hook.

Known cosmetic gap (V1): the inline user prompt appears as a status line,
not a full user message line; the assistant's streamed reply renders as a
normal message.

## Error handling

| Case | Behavior |
|---|---|
| No active region | `user-error` |
| Session busy | `user-error "Agent busy…"` |
| New session failed / readiness timeout | error message, no modification |
| Prompt request failure | cancel preview, echo error |
| Empty response at turn end | cancel preview, message |
| External buffer edit during preview | cancel preview |
| Reject while streaming | `agent-ide-protocol-cancel` + teardown; late chunks ignored |

## Testing

ERT, following repo conventions (`with-temp-buffer`, fake sessions via
`agent-ide--make-session`, `cl-letf` stubs). New file
`agent-ide-inline-test.el`:

1. Hook wiring: transcript notification handler runs chunk hook; send-prompt
   success/failure run response/failure hooks.
2. `agent-ide-inline--strip-fences`: fences, no fences, empty.
3. Prompt building: contains instruction, constraint, region context.
4. Session resolution: matching dir returned; no match → start-session
   called with working directory; busy status → user-error.
5. Preview: create/accept/reject on a temp buffer; accept yields exact
   replacement text and is undoable; reject leaves buffer unchanged.
6. External modification cancels preview.

Test runner: `emacs -batch -l agent-ide-inline-test.el -f
ert-run-tests-batch-and-exit` (repo has no Makefile/CI).

## Scope

In V1: `agent-ide-inline-rewrite` + accept/reject preview.

Out of scope (follow-ups): `insert-prompt-here`, multi-variant cycling,
completion at point, diff-style preview, per-project inline model override,
queuing while busy.
