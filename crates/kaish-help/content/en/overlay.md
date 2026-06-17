# Overlay VFS Mode

## What is overlay mode?

Overlay mode wraps the real filesystem in a copy-on-write layer. Every write
goes into an in-memory upper layer; the lower layer (real disk) is never
touched. You can inspect what changed, commit the changes, or discard them.

Enable it once at session start — it is not a `set -o` option:

```
kaish --overlay                        # interactive REPL
kaish --overlay -c 'script'           # one-shot
KernelConfig::with_overlay(true)      # embedder API
```

## kaish-vfs subcommands

`kaish-vfs status` works in any session: it reports `mode: transaction`
inside an overlay session and `mode: direct` (plus the budget) otherwise, so
you can always ask "what session am I in?" without an error path. The other
subcommands require an active overlay session — without one they exit 1 with
a clear message.

| Subcommand | Description |
|---|---|
| `kaish-vfs status` | Session mode (direct/transaction); when in transaction: dirty?, counts by kind, resident bytes, budget. |
| `kaish-vfs diff [path...]` | Unified diff of pending changes. Exits 1 when changes exist, 0 when clean. With path args, filtered. |
| `kaish-vfs commit` | Write all changes to the real filesystem, then reset. |
| `kaish-vfs reset [path]` | Discard one path's edits, or all of them. |

## Transaction story

An overlay session is a **transaction**: every write is virtual until you run
`kaish-vfs commit`. The commit pre-flights all changes (stale-base check),
then writes them atomically-ish to the target filesystem.

After a successful commit the overlay is clean. A failed commit reports which
paths landed and which did not — use `kaish-vfs status` to see the remaining
dirty set and `kaish-vfs reset` to discard it.

### Partial-commit recovery

If `kaish-vfs commit` fails mid-way, the error output lists which paths were
already written to the real tree. To recover:

1. For each path listed as already committed, run `kaish-vfs reset <path>` —
   those files are now on disk; resetting just removes their overlay entry.
2. Run `kaish-vfs status` to see what remains dirty.
3. Resolve any conflicts in the real tree (e.g. remove a file that caused an
   `AlreadyExists` conflict for an Added overlay entry).
4. Retry `kaish-vfs commit`.

## Per-call semantics (fresh-kernel embedders)

When an embedder runs a fresh kernel per `execute()` call (the common agent
pattern), each call gets a **fresh overlay transaction**. Writes are virtual for
that call only.

**The commit must run in the same call as the writes.** If you write in one
`execute()` call and commit in the next, the transaction from the first call
was already discarded when its kernel was dropped.

## Escape hatches (writes that bypass the overlay)

Under Sandboxed+overlay, the following mounts are **real LocalFs** and escape
the transaction:

- `/tmp` — process interop (mktemp output goes to real `/tmp`)
- XDG runtime dir (e.g. `/run/user/1000`) — socket and spill files

Under Passthrough+overlay, everything routes through the overlay including
`/tmp` — mktemp output is virtual and lands on real `/tmp` only on commit.

External commands (those resolved via PATH) are **unavailable** while cwd is
inside an overlay mount. `OverlayFs::real_path` returns `None` by design —
returning the lower's real path would hand tools a host path that bypasses
the overlay (git and friends write through real paths). The kernel cannot
resolve a cwd with no real path, so external commands fail with exit 127
("command not found: git") from inside an overlay mount.

Under Sandboxed+overlay, `cd /tmp` moves cwd to a real LocalFs mount — from
there, external commands work and see (and write) the real filesystem directly.
That is the intended escape hatch for running compilers; be aware that writes
in `/tmp` are not part of the overlay transaction.

## Structured diff output (`--json`)

`kaish-vfs diff --json` emits a JSON array of change objects. Each entry has:

| Field | Type | Description |
|---|---|---|
| `path` | string | VFS-absolute path of the changed file |
| `kind` | string | `"added"`, `"modified"`, or `"removed"` |
| `base_bytes` | number | Byte size of the lower-layer content (0 for Added) |
| `current_bytes` | number | Byte size of the upper-layer content (0 for Removed) |

Raw file content is omitted from the JSON output; use `cat <path>` or `kaish-vfs diff <path>`
to inspect individual file contents.

Exit codes follow POSIX diff convention: **exit 1** when changes exist, **exit 0** when
the overlay is clean (no changes). This applies to both text and `--json` output.

## Overlay resident vs. budget

`kaish-vfs status` shows two counters:

- **overlay-resident**: base snapshots + upper layer content (this overlay's RAM)
- **budget**: the kernel-wide `vfs-memory` pool used/limit

These are different: the budget covers all MemoryFs mounts plus the overlay
upper and bases; overlay-resident is only this overlay's share.

## NoLocal + overlay

`VfsMountMode::NoLocal` (minimal builds, WASI) combined with `overlay: true`
fails at kernel construction — everything is already virtual, there is no real
lower layer to wrap. The error message says so.

## JobFs and byte counting

JobFs (at `/v/jobs`) is synthesized — its content comes from bounded
BoundedStream ring buffers that are already size-capped. JobFs byte counting
is not included in overlay resident bytes or the vfs-memory budget.
