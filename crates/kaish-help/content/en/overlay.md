# Overlay VFS Mode

## Overview

Overlay mode wraps the real filesystem in a copy-on-write layer. Every write lands in an in-memory upper layer without touching the lower real disk. You can inspect, commit, or discard pending changes.

Enable overlay mode at session start. It is not a `set -o` option.

```sh
kaish --overlay              # interactive REPL
kaish --overlay -c 'script'  # one-shot script
```

Embedders enable overlay mode with `KernelConfig::with_overlay(true)`.

## kaish-vfs Subcommands

`kaish-vfs status` works in any session. It reports `mode: transaction` inside an overlay session and `mode: direct` with the memory budget otherwise.

All other `kaish-vfs` subcommands require an active overlay session. Outside an overlay session, they fail and exit `1`.

| Subcommand | Behavior |
|---|---|
| `kaish-vfs status` | Report session mode, dirty flag, change counts by kind, resident bytes, and budget |
| `kaish-vfs diff [path…]` | Show unified diff of pending changes, filtered when paths are given |
| `kaish-vfs commit` | Write all pending changes to the lower filesystem and reset the overlay |
| `kaish-vfs reset [path]` | Discard edits for one path, or all pending edits when omitted |

`kaish-vfs diff` exits `1` when changes exist and `0` when the overlay is clean.

## Transactions and Commits

- An overlay session is a transaction. Writes stay virtual until `kaish-vfs commit` runs.
- `kaish-vfs commit` validates all changes first with a stale-base check, then writes each file to the target filesystem.
- Each file replacement is atomic. A crash leaves each file whole as either old or new, while the multi-file commit is not atomic.
- A successful commit leaves the overlay clean.
- A failed commit reports which paths landed on disk and which failed.

### Partial-Commit Recovery

When `kaish-vfs commit` fails mid-way, follow these steps:

1. Run `kaish-vfs reset <path>` for each path listed as already written to remove its overlay entry.
2. Run `kaish-vfs status` to inspect the remaining dirty paths.
3. Resolve conflicts on disk, such as removing an existing file that triggered an `AlreadyExists` error for an added entry.
4. Run `kaish-vfs commit` again.

## Per-Call Commit Rule

Embedders often construct a fresh kernel for each `execute()` call. Each call gets a fresh overlay transaction whose writes stay virtual for that call only.

Run `kaish-vfs commit` in the same `execute()` call as the writes. Writes from an earlier call are discarded when that kernel drops.

## Overrides

Under `Sandboxed` mode with overlay enabled, these mounts use real `LocalFs` and bypass the transaction:

- `/tmp` handles process interop, so `mktemp` writes to real `/tmp`.
- The XDG runtime directory, such as `/run/user/1000`, holds sockets and spill files.

Under `Passthrough` mode with overlay enabled, every path including `/tmp` routes through the overlay. `mktemp` writes stay virtual until `kaish-vfs commit` runs.

## External Commands

- External commands resolved via `$PATH` fail with exit `127` when `$PWD` is inside an overlay mount.
- Overlay mounts have no real host path because exposing one lets external binaries bypass the overlay.
- The error message names the missing real cwd and suggests using a builtin, changing to a real path, or committing.
- Under `Sandboxed` mode with overlay enabled, run `cd /tmp` to move `$PWD` to a real `LocalFs` mount.
- From `/tmp`, external commands such as compilers run and write directly to the real filesystem outside the overlay transaction.

## Structured Diff Output

`kaish-vfs diff --json` emits a JSON array of change objects.

| Field | Type | Description |
|---|---|---|
| `path` | string | VFS-absolute path of the changed file |
| `kind` | string | `"added"`, `"modified"`, or `"removed"` |
| `base_bytes` | number | Byte size of lower-layer content, or `0` for added files |
| `current_bytes` | number | Byte size of upper-layer content, or `0` for removed files |

`--json` omits raw file content. Run `cat` or `kaish-vfs diff` on a path to inspect file content.

Both text and `--json` output exit `1` when changes exist and `0` when clean.

## Overlay Resident Bytes and Budget

`kaish-vfs status` reports two separate memory counters:

- **overlay-resident:** Base snapshots plus upper-layer content for this overlay.
- **budget:** Used and total bytes across the kernel-wide `vfs-memory` pool, covering all `MemoryFs` mounts plus overlay storage.

## NoLocal Mode with Overlay

Combining `VfsMountMode::NoLocal` with `overlay: true` fails at kernel construction. There is no real lower layer to wrap because every mount is already virtual.

## JobFs and Byte Counting

`JobFs` at `/v/jobs` synthesizes status and command strings on read from the job table. `JobFs` bytes do not count toward `overlay-resident` or the `vfs-memory` budget.
