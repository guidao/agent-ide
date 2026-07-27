# Agent IDE Sidebar Design

Date: 2026-07-27  
Status: Approved for planning

## Goal

Add a left sidebar that lists live Agent IDE sessions with status, and supports switching, killing, and creating sessions. It is a read-only projection of `agent-ide--sessions`, not a second source of truth.

## User-facing behavior

### Purpose

- Session switcher: click / `RET` opens the session transcript buffer.
- Status monitor: each row shows live status (and related metadata).
- Minimal ops: kill session; create new session from the sidebar.

### Visibility

- Auto-show when the first session is created (`agent-ide-sidebar-auto-show`, default `t`).
- Auto-hide when the last live session is cleaned up.
- `q` hides the sidebar without killing sessions and sets a user-dismissed flag. While dismissed, status/refresh must not re-show the sidebar. Clear dismissed and show again only when:
  - a **new** session is created and `agent-ide-sidebar-auto-show` is non-nil, or
  - the user runs explicit `M-x agent-ide-sidebar`.

### Row UI (two lines)

```
● myproject <2>          running
  claude-4 · 42%
○ other                  idle
  gpt-5.3 · 12%
```

- Line 1: status dot + project name + optional index + right-aligned status text.
- Line 2: model · usage summary; unknown values shown as `—`.
- `●` marks a session whose transcript buffer is visible in some window; others use `○`. Selected row highlighting is a separate face and independent of `●`/`○`.
- Same directory, multiple sessions: append `<n>` consistent with buffer naming (`*agent:dir*<n>`).
- Status faces distinguish at least: idle, running/working, interrupting, failed.

Footer: `[+ New]` control for creating a session.

### Actions / keys (`agent-ide-sidebar-mode`)

| Key | Action |
|---|---|
| `RET` / mouse-1 | Display that session’s buffer via existing `agent-ide--display-buffer` |
| `n` / `p` | Move by entry (two physical lines per entry) |
| `k` | Kill session (confirm when `agent-ide-sidebar-confirm-kill` is non-nil) |
| `+` / `c` | `agent-ide-new-session` |
| `g` | Manual refresh |
| `q` | Hide sidebar (set user-dismissed; do not kill sessions) |

Kill of the currently shown session: switch to another live session if any; otherwise hide sidebar.

## Architecture

### New module

`agent-ide-sidebar.el`

Responsibilities:

- Create/maintain `*agent-ide-sidebar*` buffer.
- `agent-ide-sidebar-mode` (derived from `special-mode`).
- Render entries from `agent-ide--sessions`.
- Show/hide left side window.
- Commands: select, kill, new, refresh, quit.
- Track user-dismissed flag for auto-show policy.

### Integration points

| Location | Change |
|---|---|
| `agent-ide-core.el` | After `agent-ide--set-status`, notify sidebar refresh |
| `agent-ide-session.el` | On create/cleanup: refresh; auto show/hide per policy |
| `agent-ide-renderer.el` (or header update path) | When model/usage header data changes, refresh sidebar |
| `agent-ide.el` | `require` sidebar module; expose autoloads / customs |

Sidebar never owns ACP clients or session structs. Kill goes through existing cleanup (`agent-ide--cleanup-session` + `kill-buffer`).

### Window policy

- `display-buffer-in-side-window` with `(side . left)`.
- Width from `agent-ide-sidebar-width` (default `0.22`).
- Dedicated slot (customizable if needed later) so it does not collide with the existing right/bottom transcript side window.
- Showing the sidebar does not steal focus from the transcript by default.

### Refresh

Triggers:

1. `agent-ide--set-status`
2. Session create / cleanup
3. Model / usage updates that already refresh the session header
4. Manual `g`

Implementation: full redraw of the sidebar buffer. Session count is small; incremental updates are out of scope.

Preserve point on the same session object across refresh when possible.

## Configuration (v1)

| Variable | Default | Meaning |
|---|---|---|
| `agent-ide-sidebar-width` | `0.22` | Left side-window width |
| `agent-ide-sidebar-auto-show` | `t` | Auto-show on session create; ignored for refresh while user-dismissed |
| `agent-ide-sidebar-confirm-kill` | `t` | Confirm before kill |

## Edge cases

- External `kill-buffer` on a session buffer: existing `kill-buffer-hook` cleanup runs; sidebar refreshes and may hide.
- Sidebar buffer killed: no session impact; next show recreates it.
- Empty session list: always hide the side window (no empty-state panel in v1).

## Out of scope (v1)

- Interrupt / restart from sidebar
- Sorting, grouping, filtering
- Persisted session history
- Dependencies on treemacs / external sidebar frameworks

## Testing

ERT coverage (same style as `agent-ide-session-mode-test.el`):

1. Render zero / multiple sessions → expected line count and key substrings.
2. Status change → refreshed status text.
3. Kill → entry removed; last kill → side window closed.
4. `q` sets dismissed; subsequent refresh does not re-show while sessions remain.
5. Select entry → target session buffer is displayed.

## Success criteria

- With multiple concurrent agents, user can see status at a glance and jump without `C-x b` hunting.
- Sidebar stays in sync with session lifecycle without focus theft or duplicate state.
- Package remains dependency-free beyond current requirements.
