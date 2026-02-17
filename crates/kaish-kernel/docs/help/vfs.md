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
/v/blobs/      memory storage for blobs
/v/bin/        read-only listing of builtins (can invoke: /v/bin/echo hello)
/v/jobs/{id}/  live background job state (see below)
/git/          read-only git metadata for cwd
```

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

## /git — Repository Introspection

```bash
cat /git/status            # git status
cat /git/log               # recent commits
cat /git/diff              # current diff
cat /git/blame/path/to/f   # blame for specific file
```

## /tmp — Interop

`/tmp` is the only path outside `$HOME` accessible in sandboxed mode. Use it for data exchange with external commands.

```bash
write /tmp/data.json '{"key": "value"}'
jq '.key' /tmp/data.json
```
