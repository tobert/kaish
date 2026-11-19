# kaish Virtual Filesystem

## Modes

| Mode | Access | Context |
|---|---|---|
| **Passthrough** | Full filesystem | Interactive REPL |
| **Sandboxed** | `$HOME` and `/tmp` only | Embedded agent |
| **NoLocal** | Memory only | Tests and minimal builds |

In sandboxed mode, paths look native. Access outside `$HOME` and `/tmp` fails.

## Mount Points

| Mount | Backing | Behavior |
|---|---|---|
| `/home/user/` | Real filesystem | Sandboxed to `$HOME` in sandboxed mode |
| `/tmp/` | Real `/tmp` | Always accessible, tmpfs on Linux |
| `/dev/null` | Sink | Discards writes and reads empty |
| `/dev/zero` | Endless zero bytes | Requires counted reads |
| `/dev/urandom` | Endless random bytes | Cryptographic bytes, aliased at `/dev/random` |
| `/v/blobs/` | Memory storage | Stores binary blobs |
| `/v/bin/` | Builtin directory | Read-only listing, runnable as `/v/bin/echo hello` |
| `/v/jobs/{id}/` | Job state | Exposes background job files |

External commands such as `git` are not VFS mounts. Commands like `git status` run via the `subprocess` capability against host `git`.

## Walks Stay in One Mount

- `grep -r`, `find`, `ls -R`, `tree`, `glob`, and `**` stay inside the starting mount.
- From `/`, these walks list `/v`, `/dev`, and `/tmp` without descending into them.
- Name a mount to walk it. For example, `grep -r x /v` walks `/v` and nested mounts.
- Pass `--cross-mounts` for one command, or run `set -o crossmounts` for the session.
- A walk that skips mounts keeps its exit code and prints one stderr notice:

```
grep: skipped mounts /dev /tmp /v (use --cross-mounts to enter)
```

## Job Observability under /v/jobs

Each background job gets a directory under `/v/jobs/{id}/`:

| Node | Content |
|---|---|
| `/v/jobs/{id}/status` | `"running"`, `"stopped"`, `"done:0"`, `"killed:N"`, or `"failed:N"` |
| `/v/jobs/{id}/command` | Original command string |
| `/v/jobs/{id}/stdout` | Live job `stdout` while running |
| `/v/jobs/{id}/stderr` | Live job `stderr` while running |

```sh
cargo build 2>&1 &
cat /v/jobs/1/status       # running
cat /v/jobs/1/stdout       # build output so far
jobs --cleanup             # remove completed jobs
```

`stdout` routing to `/v/jobs/{id}/stdout`:

- An external command in the final stage streams live, chunk by chunk.
- A builtin or function in the final stage writes its whole output on return.
- Earlier pipeline stages, `$(…)` output, redirected `stdout`, and scatter workers are excluded.

`stderr` routing to `/v/jobs/{id}/stderr`:

- `stderr` fills from every pipeline stage.
- External commands stream live, chunk by chunk. Builtins and functions write once after their redirects apply.
- `2>file`, `&>file`, and `2>&1` keep that stage's `stderr` out of the node. `>&2` still reaches `stderr`.
- A `$(…)` in a command's arguments runs before its redirects, so its `stderr` always reaches the node.

Job stream retention:

- Drop `| tee` on background jobs because the job node already captures output.
- `cargo build | tee log &` buffers output until completion because `tee` is a builtin.
- Each node holds at most 10MB and evicts its oldest bytes past that limit.
- Redirect to a file like `cargo build > /tmp/build.log 2>&1 &` when the full output matters.

## Interop under /tmp

`/tmp` is the only path outside `$HOME` available in sandboxed mode. Use `/tmp` to exchange data with external commands.

```sh
write /tmp/data.json '{"key": "value"}'
jq '.key' /tmp/data.json
```

## Synthetic Devices under /dev

In sandboxed and memory-only modes, the host `/dev` is unreachable. kaish provides software-backed devices instead.

```sh
cmd > /dev/null                               # discard output
cat /dev/null                                 # empty
head -c 32 /dev/zero                          # exactly 32 zero bytes
dd if=/dev/urandom of=key.bin bs=16 count=1   # 16 random bytes to a file
dd if=/dev/urandom bs=8 count=1               # 8 random bytes as a Bytes result
```

- `/dev/zero`, `/dev/urandom`, and `/dev/random` are endless streams.
- Read a fixed byte count with `head -c N` or `dd … count=N`.
- In sandboxed and memory-only modes, `cat /dev/urandom` fails and names `head -c`.
- Move raw random bytes with binary-aware `dd` instead of a text pipe.
- Running `dd` without `of=` returns a binary result.
- Binary results render as a hex dump in the REPL or a base64 envelope under `--json`.
- In passthrough mode, `/dev` maps to the real host `/dev`.

## Redirects

```sh
mkdir -p /tmp/out
cmd > /tmp/out/log.txt   # a redirect never creates a directory
cmd > /v/jobs/x          # error — read-only mount, redirect under /tmp
```

- Targets resolve against `$PWD` and open through their owning mount before the command runs.
- `>` truncates the target file, and `>>` appends to it.
- A missing parent directory, a read-only mount, or a directory target exits `1` without running the command.
- Reading and writing the same file with `<` and `>` fails. Write to a temporary file, then run `mv` over the original.

## Sandbox Limitations

External binaries bypass the VFS sandbox. Sandboxed mode restricts kaish builtins to `$HOME` and `/tmp`. External commands resolved via `$PATH`, `exec`, `spawn`, and `env CMD` access the real filesystem directly.

Set `allow_unwrapped_commands=false` in `KernelConfig` to block external command execution:

```rust
KernelConfig::agent().with_allow_unwrapped_commands(false)
```

- `KernelConfig::isolated()` sets `allow_unwrapped_commands` to `false` by default.
- When disabled, `$PATH` lookup, `exec`, `spawn`, and `env CMD` exit `127` with `"external commands are disabled on this shell"`.
- Unresolvable command names still report `"command not found"`.
- Prefer kaish builtins such as `grep`, `sed`, and `jq` because they respect VFS boundaries.
