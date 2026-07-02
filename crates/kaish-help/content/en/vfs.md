# kaish Virtual Filesystem (VFS)

## Modes

| Mode | Access | Context |
|------|--------|---------|
| **Passthrough** | Full filesystem | REPL (human) |
| **Sandboxed** | `$HOME` + `/tmp` only | agent (embedded) |
| **NoLocal** | Memory only | Tests |

In sandboxed mode, paths look native but access outside `$HOME` fails (except `/tmp`).

## Mount Points

```
/home/user/    real filesystem (sandboxed to $HOME in sandboxed mode)
/tmp/          real /tmp (always accessible, tmpfs on Linux)
/dev/null      sink: discards writes, reads empty
/dev/zero      endless zero bytes (counted reads only — see below)
/dev/urandom   endless cryptographic random bytes (also /dev/random)
/v/blobs/      memory storage for blobs
/v/bin/        read-only listing of builtins (can invoke: /v/bin/echo hello)
/v/jobs/{id}/  live background job state (see below)
```

Git is an ordinary external command (`git status`, `git log`, `git diff`) — it
runs via the `subprocess` capability against your system `git`, not a VFS mount.

## /v/jobs — Job Observability

Each background job gets a directory:

```
/v/jobs/{id}/stdout    live output (ring buffer, 10MB max)
/v/jobs/{id}/stderr    live error stream
/v/jobs/{id}/status    "running" | "done:0" | "failed:N"
/v/jobs/{id}/command   original command string
```

```sh
cargo build &
cat /v/jobs/1/status       # running
cat /v/jobs/1/stdout       # build output so far
jobs --cleanup             # remove completed jobs
```

## /tmp — Interop

`/tmp` is the only path outside `$HOME` accessible in sandboxed mode. Use it for data exchange with external commands.

```sh
write /tmp/data.json '{"key": "value"}'
jq '.key' /tmp/data.json
```

## /dev — Synthetic devices

In sandboxed and memory-only modes the host's `/dev` is unreachable, so kaish
provides software-backed devices:

```sh
cmd > /dev/null                          # discard output (writes are dropped)
cat /dev/null                            # empty
head -c 32 /dev/zero                     # exactly 32 zero bytes
dd if=/dev/urandom of=key.bin bs=16 count=1   # 16 random bytes to a file
dd if=/dev/urandom bs=8 count=1          # 8 random bytes as a Bytes result
```

`/dev/zero`, `/dev/urandom`, and `/dev/random` are *endless* streams, but kaish
reads whole files into memory — it has no infinite streams. So you must read a
fixed count: `head -c N` and `dd … count=N` work, while `cat /dev/urandom` is a
loud error rather than a hang. Raw random bytes are binary, so move them with
`dd` (binary-aware) rather than a text pipe; `dd` with no `of=` yields a binary
result that renders as a hex dump (REPL) or a base64 envelope (`--json`). In
passthrough (REPL) mode `/dev` is the real host `/dev` instead.

## Sandbox Limitations

**External binaries bypass the VFS sandbox.** Sandboxed mode restricts kaish builtins to `$HOME` + `/tmp`, but external commands (anything resolved via PATH), `exec`, and `spawn` access the real filesystem directly.

To block external command execution, set `allow_external_commands=false` in `KernelConfig`:

```rust
KernelConfig::agent().with_allow_external_commands(false)
```

When disabled, PATH lookups return "command not found" and the `exec`/`spawn` builtins return errors. `KernelConfig::isolated()` sets this to `false` by default.

Prefer builtins over external commands — kaish's in-process builtins (grep, sed, jq, etc.) respect VFS boundaries.
