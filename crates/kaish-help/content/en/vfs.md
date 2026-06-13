# kaish Virtual Filesystem (VFS)

## Modes

| Mode | Access | Context |
|------|--------|---------|
| **Passthrough** | Full filesystem | REPL (human) |
| **Sandboxed** | `$HOME` + `/tmp` only | MCP (agent) |
| **NoLocal** | Memory only | Tests |

In sandboxed mode, paths look native but access outside `$HOME` fails (except `/tmp`).

## Mount Points

```
/home/user/    real filesystem (sandboxed to $HOME in MCP mode)
/tmp/          real /tmp (always accessible, tmpfs on Linux)
/dev/null      sink: discards writes, reads empty
/dev/zero      endless zero bytes (counted reads only — see below)
/v/blobs/      memory storage for blobs
/v/bin/        read-only listing of builtins (can invoke: /v/bin/echo hello)
/v/jobs/{id}/  live background job state (see below)
```

Git is the `git` *builtin* (`git status`, `git log`, `git diff`), not a VFS mount.

## /v/jobs — Job Observability

Each background job gets a directory:

```
/v/jobs/{id}/stdout    live output (ring buffer, 10MB max)
/v/jobs/{id}/stderr    live error stream
/v/jobs/{id}/status    "running" | "done:0" | "failed:N"
/v/jobs/{id}/command   original command string
```

```bash
cargo build &
cat /v/jobs/1/status       # running
cat /v/jobs/1/stdout       # build output so far
jobs --cleanup             # remove completed jobs
```

## /tmp — Interop

`/tmp` is the only path outside `$HOME` accessible in sandboxed mode. Use it for data exchange with external commands.

```bash
write /tmp/data.json '{"key": "value"}'
jq '.key' /tmp/data.json
```

## /dev — Synthetic devices

In sandboxed and memory-only modes the host's `/dev` is unreachable, so kaish
provides software-backed devices:

```bash
cmd > /dev/null            # discard output (writes are dropped)
cat /dev/null              # empty
head -c 32 /dev/zero       # exactly 32 zero bytes
```

`/dev/zero` is an *endless* stream, but kaish reads whole files into memory — it
has no infinite streams. So you must read a fixed count: `head -c N /dev/zero`
works, while `cat /dev/zero` is a loud error rather than a hang. In passthrough
(REPL) mode `/dev` is the real host `/dev` instead.

`/dev/urandom` is not yet provided — kaish's pipes are UTF-8 text and can't
carry raw random bytes intact. Use a host command (passthrough mode) for now.

## Sandbox Limitations

**External binaries bypass the VFS sandbox.** Sandboxed mode restricts kaish builtins to `$HOME` + `/tmp`, but external commands (anything resolved via PATH), `exec`, and `spawn` access the real filesystem directly.

To block external command execution, set `allow_external_commands=false` in `KernelConfig`:

```rust
KernelConfig::mcp().with_allow_external_commands(false)
```

When disabled, PATH lookups return "command not found" and the `exec`/`spawn` builtins return errors. `KernelConfig::isolated()` sets this to `false` by default.

Prefer builtins over external commands — kaish's in-process builtins (grep, sed, jq, etc.) respect VFS boundaries.
