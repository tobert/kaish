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
/v/jobs/{id}/  background job state (see below)
/v/approvals/  the approval ledger, read-only (see below)
```

Git is an ordinary external command (`git status`, `git log`, `git diff`) — it
runs via the `subprocess` capability against your system `git`, not a VFS mount.

## /v/jobs — Job Observability

Each background job gets a directory:

```
/v/jobs/{id}/status    "running" | "stopped" | "done:0" | "gated" | "killed:N" | "failed:N"
/v/jobs/{id}/command   original command string
/v/jobs/{id}/stdout    the job's stdout so far — live while it runs
/v/jobs/{id}/stderr    the job's stderr so far — live while it runs
/v/jobs/{id}/approval  pending approval request (JSON) if gated, else empty
```

```sh
cargo build 2>&1 &
cat /v/jobs/1/status       # running
cat /v/jobs/1/stdout       # build output so far
jobs --cleanup             # remove completed jobs
```

`stdout` and `stderr` fill as an external command emits. A builtin does not
stream: it returns its whole output when it finishes, so `echo hi &` fills
the node in one write at the end — and so does `cargo build | tee log &`,
because `tee` is a builtin. Drop the `| tee`; the job's stream is the log.
Only the last stage of a pipeline reaches `stdout` (an earlier stage's output
is the next stage's stdin); `stderr` takes every stage's.

Each node holds at most 10MB and evicts its oldest bytes past that. Redirect
to a file (`cargo build > /tmp/build.log 2>&1 &`) when the whole output
matters.

A destructive op backgrounded under `set -o approvals` (`rm x &`) gates in the
background: status is `gated`, and `/v/jobs/{id}/approval` carries the JSON
request (operation, resources, and the job it belongs to) so the gate can be
fulfilled. The node never carries a token — approve the request, then re-run
with `--confirm=<token>`, or, from an embedder, `Kernel::confirm`.

## /v/approvals — The Approval Ledger

The requests `set -o approvals` raises, and what was decided about them:

```
/v/approvals/pending      JSON array of requests awaiting a decision
/v/approvals/standing     JSON array of live standing grants
/v/approvals/log          NDJSON of the retained entries, oldest first
/v/approvals/{id}/request the request as JSON
/v/approvals/{id}/state   "requested" | "granted" | "denied" | "expired" | ...
/v/approvals/{id}/attempts JSON array of attempts and their outcomes
/v/approvals/{id}/grant   the grant as JSON, empty until it is decided
```

**Read-only, and every write returns `Unsupported`** — granting by writing a
file would make "the agent can write files" mean "the agent can approve its
own operations". Decide with the `approvals` builtin, which needs approval
authority on the session for `grant`, `deny`, and `revoke`.

**No node carries a token.** The redemption credential lives only in the
kernel and is retrievable through an embedder's `ApproverHandle::token_for`.
A grant carries `token_prefix`, four characters that correlate a rejected key
with the grant it was aimed at — never the key.

```sh
rm precious.txt &                     # gates in the background
cat /v/approvals/pending              # every request, across every job
approvals list                        # the same, as a table
approvals show req_9c1a4f2e_42        # one request, its decision, its attempts
approvals renew req_9c1a4f2e_42       # re-raise it after it expires (60s)
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
