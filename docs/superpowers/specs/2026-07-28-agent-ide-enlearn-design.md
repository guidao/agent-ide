# Agent IDE English Learn (enlearn) Design

Date: 2026-07-28  
Status: Approved for planning

## Goal

When enabled, intercept prompt submission so an English learner can:

1. If the input is Chinese: translate it into English suitable as a coding-agent prompt, and explain the English (sentence breakdown + grammar/collocation tips).
2. If the input is English: polish it into more natural English with the same style of explanation.
3. Show the learning material in the same transcript, then send the **final English** to the ACP agent (after confirm by default, or automatically when configured).

Architecture: **thin pre-submit hook in agent-ide core** + **optional plugin** that uses **gptel**. Core must not depend on gptel.

## User-facing behavior

### Enable / options

| Option | Default | Meaning |
|---|---|---|
| `agent-ide-enlearn-mode` | off | Master switch (global minor mode) |
| `agent-ide-enlearn-auto-send` | `nil` | `nil`: show learning block and wait for confirm; `t`: after learning block is ready, send Final automatically |
| `agent-ide-enlearn-model` | `nil` | Optional gptel model override; `nil` follows gptel’s current model |
| `agent-ide-enlearn-on-backend-error` | `abort` | When gptel missing/fails: `abort` (show Retry / Send original / Cancel) or `send-original` |

Commands: `agent-ide-enlearn-toggle`, `agent-ide-enlearn-toggle-auto-send`, and a one-shot `agent-ide-enlearn-skip-next` (next submit bypasses enlearn).

### Submit flow (enlearn on)

1. User submits non-empty prompt (`C-c RET` / configured binding).
2. Plugin freezes the **original** user text in the transcript (Chinese or draft English preserved).
3. Plugin calls gptel asynchronously; UI shows busy state (`Coaching...`). While a coach request is pending, the session interrupt binding (`C-c C-c` / `C-c C-k`) cancels that gptel request only and does not cancel an in-flight ACP turn (there should be none yet).
4. Plugin inserts a read-only **English Coach** block in the same transcript:

```text
✦ English Coach
Final:
  <final English prompt>

Breakdown:
  - ...

Grammar / collocation:
  - ...

[Send]  [Edit]  [Cancel]
```

5. Confirm mode (`auto-send` nil):
   - **Send** / `C-c C-m` (while pending): deliver Final via `agent-ide-deliver-prompt`.
   - **Edit**: put Final into the editable prompt; user may edit and resubmit. Resubmit runs enlearn again unless `skip-next` was set.
   - **Cancel**: do not send to the agent; leave a fresh editable prompt.
6. Auto-send mode: same learning block, no action buttons; when Final is ready, deliver automatically.
7. After deliver, transcript shows the final English as a normal submitted user prompt (in addition to the earlier original and the coach block), then ACP proceeds as today.

### Language / content policy

- Heuristic: input with substantial Han characters → **translate** mode; otherwise → **polish** mode.
- Always run through gptel when enlearn is on (no skip based on `/` or code-only heuristics in v1).
- System instructions must require: preserve code fences, file paths, `/slash` commands, and identifiers unchanged; only rewrite surrounding natural language.
- Final must be English, imperative/actionable as a coding-agent prompt.
- Breakdown and Grammar sections are written in **Chinese** (learner-facing). Depth is medium: Final + sentence breakdown + grammar/collocation tips (not full bilingual worksheets).

### gptel response shape

Model is asked to return markdown with fixed headings:

```markdown
## Final
...

## Breakdown
- ...

## Grammar
- ...
```

Plugin splits on these headings for rendering. If parsing fails: show the raw reply; treat as error path (Retry / Send original / Cancel) when Final cannot be recovered.

gptel calls are short, standalone requests (not attached to the ACP coding session).

### Errors

| Case | Behavior |
|---|---|
| gptel not installed / not configured | Notify; follow `on-backend-error` |
| Request failure / timeout / empty Final | Coach block shows error + `[Retry] [Send original] [Cancel]` |
| User cancels gptel / Cancel button | Do not send; restore editable prompt |
| enlearn off | Hook returns immediately; default submit path |

## Architecture

### Approach

**Core thin hook + separate plugin** (not inlined into the main package’s hard dependencies; not fragile `advice-add`-only).

```text
User RET
  → agent-ide-submit
  → run agent-ide-pre-submit-functions
  → enlearn (if enabled): defer
       → freeze original
       → gptel
       → English Coach block
       → [confirm | auto-send]
       → agent-ide-deliver-prompt(final English)
  → else: agent-ide-deliver-prompt(original)
```

### Core changes (agent-ide)

| Location | Change |
|---|---|
| `agent-ide.el` | Declare `agent-ide-pre-submit-functions` |
| `agent-ide-session.el` | Split today’s `agent-ide-submit` into: read/validate input → run hooks → default deliver; expose `agent-ide-deliver-prompt` |
| Tests | Hook `continue` vs `defer`; `deliver-prompt` matches previous submit tail behavior |

**Hook contract**

```elisp
(defcustom agent-ide-pre-submit-functions nil
  "Abnormal hooks run before delivering a user prompt.
Each function is called with (SESSION PROMPT).
If a function returns non-nil, it has handled the submission (defer);
`agent-ide-submit' must not deliver. If all return nil, deliver PROMPT.")
```

**Public API for plugins**

- `agent-ide-deliver-prompt (session prompt)` — the post-validation half of today’s submit: push `prompt` onto history, freeze/append it as a submitted user line in the transcript, create the next editable prompt, call `agent-ide-protocol-send-prompt`.
  - Normal path: freeze the current editable input (today’s behavior).
  - Deferred enlearn path: original was already frozen; editable prompt may be empty/placeholder. `deliver-prompt` must still append a frozen user line for `prompt` (the Final English) before sending—do not require the Final text to already sit in the editable region.
- Minimal generic transcript note insertion only if existing renderer APIs cannot host the coach block; core must not hard-code English Coach styling.

Core **Package-Requires** must not add gptel.

### Plugin (`agent-ide-enlearn`)

Suggested layout (same repo under `extensions/` or a separate package repo):

| File | Responsibility |
|---|---|
| `agent-ide-enlearn.el` | Mode/flags, register pre-submit hook, gptel invoke, markdown split, coach UI buttons, auto-send, skip-next |

Dependencies: `agent-ide`, `gptel`.

### Out of scope (v1)

- Using `agent -p` / ACP agent for translation
- Skipping enlearn via code-only / slash-only heuristics
- Persistent learning history outside the transcript
- Multi-language targets other than English
- Changing ACP protocol

## Testing

### Core

- With no hooks: submit behavior unchanged.
- Hook returns non-nil: `protocol-send-prompt` not called; plugin can later call `deliver-prompt`.
- `deliver-prompt` updates history/status/prompt UI like today’s submit tail.

### Plugin (gptel mocked)

- Translate path produces Final + Chinese Breakdown/Grammar sections.
- Polish path same schema.
- Confirm Send delivers Final only (not original) to `deliver-prompt`.
- auto-send delivers without buttons.
- Cancel / gptel cancel does not call `deliver-prompt`.
- Parse failure and backend error expose Retry / Send original / Cancel.
- Code/path/`/slash` preservation is asserted via prompt fixture expectations (instruction present; sample response parsed).

## Success criteria

- With enlearn off, agent-ide behavior is identical to today.
- With enlearn on, Chinese and English drafts become a confirmed (or auto-sent) English agent prompt, with medium-depth Chinese explanations in-transcript.
- gptel remains a plugin-only dependency.
- Pre-submit extension point is reusable by other plugins.
