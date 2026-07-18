# Embedding kaish

This guide shows how to embed the kaish kernel in your application: kernel
construction, capability features, per-call execution options, custom tools,
and output capture.

## Stability

kaish is pre-1.0 (currently 0.13.x, MSRV 1.85). The language has settled;
the embedding API may still change between minor versions where it improves
both kaish and its embedders — [kaijutsu](https://github.com/tobert/kaijutsu)
is the reference embedder. Pin a minor version and read release notes when
bumping.

**Panic safety:** kaish makes no panic-unwind guarantees. Errors returned as
`Err(...)` always clean up; a panic mid-execute may leave kernel state (e.g.
a pushed scope frame) behind. Treat a panicking kernel as poisoned.

## Quick Start

```rust
use kaish_kernel::{Kernel, KernelConfig};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Create a kernel with defaults
    let kernel = Kernel::new(KernelConfig::transient())?;

    // execute() returns Ok(ExecResult) even when the script fails
    // (nonzero exit code); Err(_) is reserved for kernel faults.
    let result = kernel.execute("echo 'Hello from kaish!'").await?;
    if result.code != 0 {
        eprintln!("script failed: {}", result.err);
    }
    println!("{}", result.text_out());

    Ok(())
}
```

`ExecResult` exposes stdout via the `text_out()` accessor (it materializes
structured output when a builtin returned a table or tree); `code`, `err`,
and `data` are public fields.

## The result contract

Output is clean text by default — simple commands return plain text, structured
builtins (`ls`, `kaish-mounts`, `kaish-vars`) render readable tab-separated
values, and `--json` on any command emits JSON plus a parsed value (`data`) that
builtins set explicitly — kaish never infers it by sniffing stdout. The exit
code is something agents can branch on:

| `code` | Meaning | Recovery |
|--------|---------|----------|
| 0 | Success | — |
| 1 | Failure | Read `err` |
| 2 | Confirmation required (`set -o latch`) | Re-run with `--confirm="<nonce>"` — embedders read the typed `ExecResult.latch` (or call `Kernel::confirm`); the `To confirm, run:` line shows it for humans |
| 3 | Output truncated by the output limit | `original_code` holds the real exit code. With disk spill the message names the spill file — `cat` it, or narrow the query; memory-spill kernels (`with_backend`, `SpillMode::Memory`) truncate in place with no file |
| 124 | Timeout (`timeout_ms`, default 30 s) | — |
| 130 | Cancelled | — |

Embedders typically run a fresh kernel per request (variables, functions,
aliases, `set -o` options, and `cwd` reset each time) while trash and
confirmation nonces (60 s TTL) persist across calls — share the store with
`KernelConfig::with_nonce_store()` (see
[Destructive-op rails](#destructive-op-rails-inspecting-and-fulfilling-the-latch)).

## Stack size — size your execution threads

The interpreter recurses on the **native stack**: command substitution
(`$(…)`), shell-function calls, and `.kai` script sourcing all re-enter the
statement engine. A runaway or mutually recursive script is caught by a depth
guard ([`MAX_RECURSION_DEPTH`], 48) that returns a loud
`"maximum recursion depth exceeded"` error instead of overflowing the stack —
**but the guard only fires *before* the overflow if the thread has enough
stack.** On the default ~2 MB tokio worker stack, a deep recursion SIGSEGVs
before reaching the cap.

kaish can't set this itself (it doesn't own your runtime), so it exposes the
floor: **[`RECOMMENDED_STACK_SIZE`] (12 MiB)**. The cap and the floor are a
matched pair — the floor is sized so the guard trips before `cap × per-level
stack` can overflow it. Size every thread that drives kaish execution to at
least this:

```rust
// Worker threads (pipeline stages, background jobs, scatter workers run here):
let runtime = tokio::runtime::Builder::new_multi_thread()
    .thread_stack_size(kaish_kernel::RECOMMENDED_STACK_SIZE)
    .enable_all()
    .build()?;

// The block_on / driver thread also runs foreground recursion — tokio doesn't
// own it, so if it's the OS main thread (~8 MB) give it a sized std::thread:
std::thread::Builder::new()
    .stack_size(kaish_kernel::RECOMMENDED_STACK_SIZE)
    .spawn(move || runtime.block_on(async { /* … kernel.execute … */ }))?
    .join().unwrap();
```

Below the floor the guard still bounds *most* recursion, but a deep foreground
recursion on an undersized driver thread can still overflow — the reference
REPL (`kaish-repl`) sizes both its runtime workers and its driver thread to
`RECOMMENDED_STACK_SIZE`, and is the working example.

> **Debug builds pay more per level.** The GH #48 allocation pass cut the
> per-level stack to ~50 KB (release) / ~57 KB (this workspace's debug, which
> builds the interpreter crates at `opt-level = 1`). That profile setting lives
> in kaish's own `Cargo.toml` and does **not** propagate to your build — your
> *debug* build of the kernel pays the full unoptimized ~193 KB/level. The
> 12 MiB floor is deliberately sized against that worst case (48 × 193 KB ≈
> 9.3 MB), so you're covered either way; if you want the smaller debug frames
> too, add `[profile.dev.package.kaish-kernel] opt-level = 1` to your own
> workspace.

## Architecture

kaish separates concerns into layers:

```text
┌─────────────────────────────────────────────────────────┐
│  Your Application (e.g., kaijutsu)                       │
├─────────────────────────────────────────────────────────┤
│  KernelClient trait (kaish-client)                       │
│  - execute / tool_schemas / list_vars / cancel           │
│  - EmbeddedClient wraps an in-process Kernel             │
├─────────────────────────────────────────────────────────┤
│  KernelBackend trait (kaish-tool-api)                    │
│  - resolve_real_path() → maps VFS paths to real paths    │
│  - File operations, tool dispatch, mounts                │
├─────────────────────────────────────────────────────────┤
│  Kernel (kaish-kernel)                                   │
│  - Lexer/Parser/Validator/Interpreter                    │
│  - Tool Registry (builtins + custom tools)               │
│  - VFS Router                                            │
└─────────────────────────────────────────────────────────┘
```

Two ways in:

- **`Kernel` directly** — full surface, in-process.
- **`KernelClient`** (`kaish-client` crate) — the frontend trait the REPL
  drives; implement or reuse `EmbeddedClient::new(kernel)` if your app wants
  a swappable kernel connection. `EmbeddedClient::shutdown()` is a no-op by
  design: the embedder owns the kernel lifecycle.

## Capability Features

The default build is deliberately minimal: real-file I/O and the
copy-on-write overlay, **no** process execution, host introspection,
desktop integration, or tokenizer. Each dangerous surface is a named opt-in
cargo feature on `kaish-kernel`:

| Feature | Gates | Default |
|---------|-------|---------|
| `localfs` | Real local filesystem: `LocalFs`, passthrough/sandboxed VFS modes, spill-to-disk | ✓ |
| `overlay` | Copy-on-write overlay FS (implies `localfs`) | ✓ |
| `subprocess` | External commands: exec/spawn/which/bg/fg/kill, PATH, signals, job control | — |
| `host` | Host introspection: `ps`, `uname --host`, `hostname` | — |
| `os-integration` | Freedesktop trash + XDG base directories | — |
| `tokens` | BPE tokenization (`tokens` builtin) | — |
| `full` | All of the above (`native` is an alias) | — |

Consequences for embedders:

- **External commands need `subprocess`.** Without it, PATH lookup and
  `exec`/`spawn` don't exist. With it, gate at runtime via
  `allow_external_commands` (see [Sandboxing](#sandboxing-and-external-commands)).
  Git is an ordinary external command (`git status`, `git log`): it runs via
  `subprocess` against your system `git`, with no in-tree builtin or backend.
- A read-only agent shell wants the default features plus a custom backend —
  see [with_backend hermeticity](#custom-backend-kernelwith_backend).

## Kernel Construction

### Modes (`KernelConfig`)

```rust
use kaish_kernel::{Kernel, KernelConfig, VfsMountMode};
use std::path::PathBuf;

// Throwaway kernel, sandboxed defaults
let kernel = Kernel::new(KernelConfig::transient())?;

// Named kernel sandboxed to a specific root
let config = KernelConfig::named("my-kernel")
    .with_vfs_mode(VfsMountMode::Sandboxed {
        root: Some(PathBuf::from("/custom/root")),
    })
    .with_cwd(PathBuf::from("/custom/root"));

// Full host filesystem (what the REPL uses)
let config = KernelConfig::repl();

// Memory-only VFS, external commands disabled (tests, untrusted scripts)
let config = KernelConfig::isolated();

// Sandboxed-to-$HOME with a 64 MiB in-memory VFS budget (the agent preset)
let config = KernelConfig::agent();
```

Other builders: `.with_latch(bool)` / `.with_trash(bool)` (destructive-op
rails — see below), `.with_vfs_budget(bytes)` / `.without_vfs_budget()` (cap
in-memory VFS growth), `.with_skip_validation(bool)`, `.with_initial_vars(map)`
(below).

#### Destructive-op rails: inspecting and fulfilling the latch

With `.with_latch(true)`, a destructive op (`rm`'s delete, and the truncating
overwrite behind `tee` / `patch` / `sed -i` / `write` / `cp` / `mv` / `dd of=`)
does not run on first call — it returns an `ExecResult` with **exit code 2** and a
confirmation request. Copying or moving *into* a directory, and recursive
`cp -r`/`mv` of a tree, gate only the named destination, not per-child overwrites.
The output contract:

- **`ExecResult.err`** (which a frontend routes to stderr) carries the
  human-readable prompt;
- **stdout** is empty (nothing happened, so there is no success output);
- **`ExecResult.latch`** carries the request as a first-class typed field —
  `Option<Box<LatchRequest>>`, control-plane and distinct from the data-plane
  `.data`.

`LatchRequest` is the whole inspect+fulfill contract:

```rust
pub struct LatchRequest {
    pub nonce: String,        // pass back as --confirm=<nonce>
    pub command: String,      // display label, e.g. "rm", "kaish-trash empty"
    pub paths: Vec<String>,   // resolved paths the op would touch (for policy)
    pub hint: String,         // human re-run string (display only)
    pub tool: String,         // dispatch name — argv0 for a precise replay
    pub argv: Vec<String>,    // the EXACT captured argv (minus the nonce)
    pub ttl: u64,             // seconds until the nonce expires
}
```

**Inspect** with the typed accessor (works before or after `--json` — `.latch`
survives formatting):

```rust
if let Some(req) = result.latch_request() {
    // apply preapproval policy / model review over (req.command, req.paths),
    // or inspect the exact req.argv that a confirm will replay …
}
```

**Fulfill** with `Kernel::confirm` — the highest-fidelity path. It replays the
*exact captured argv* (`req.tool` + `req.argv`) with `--confirm=<nonce>`
prepended, via the argv door — no re-parsing, so paths with spaces or glob
characters round-trip precisely:

```rust
let gated = kernel.execute("rm 'my notes.txt'").await?;
if let Some(req) = gated.latch_request() {
    if approve(&req) {                       // your policy
        let done = kernel.confirm(&req).await?;   // replays exactly, deletes
    }
}
```

Prefer `confirm` over hand-building the re-run. The `hint` field is a
*human-display* string and does **not** robustly quote paths (`rm --confirm="N"
my notes.txt` re-parses as two paths); `confirm` sidesteps that entirely. If you
must build it yourself, use the argv door: `execute_argv(req.tool, [..req.argv,
"--confirm=<nonce>"])`.

The kernel owns the *mechanism* (issuing/validating the path- and command-scoped
nonce, capturing the argv at the dispatch seam); the embedder owns the
*judgment*. The latch is **never** folded into `.data` — a stdout redirect
(`rm big > log`) clears the data-plane `.data` but can't touch the control-plane
`.latch`, so the gate can't be silently bypassed.

> **Note:** the argv is captured at the kernel's dispatch seam, so it's present
> for every kaish builtin and any tool you register in the kernel's registry
> (the `Kernel::with_backend` tools closure). A tool served *only* by a custom
> `KernelBackend::call_tool` that raises its own latch leaves `tool`/`argv`
> empty; `confirm` then fails loud (exit 2) — fulfill those via the `hint` or a
> manually reconstructed argv.

If you executed with `--json` (`OutputFormat::Json`), the gate is a non-zero exit
with a diagnostic, so the result is wrapped in the standard JSON error envelope
and the request is surfaced under its own `latch` key:
`{ "error": "...", "code": 2, "latch": { "nonce": ..., "tool": ..., "argv":
[...], "paths": [...], "hint": ..., "ttl": 60 } }`. The typed `latch_request()`
accessor works the same either way, so it's the recommended path.

Nonces are scoped to `(command, paths)`, expire after 60s, and are not consumed
on use (idempotent retries). To confirm a nonce issued in one `execute()` call
from a *later* call, share the store with
`KernelConfig::with_nonce_store()` — the default `NonceStore` is fresh per
kernel. See [LANGUAGE.md](LANGUAGE.md) for the full latch/trash semantics.

### Custom Backend (`Kernel::with_backend`)

For full control over file I/O, implement `KernelBackend` (from
`kaish-tool-api`, re-exported by the kernel) and assemble with:

```rust
let kernel = Kernel::with_backend(
    backend,            // Arc<dyn KernelBackend>
    config,             // KernelConfig
    |vfs| {             // mount extra filesystems
        // vfs.mount_arc("/v/docs", docs_fs);
    },
    |tools| {           // register custom tools
        // tools.register(MyTool { ... });
    },
)?;
```

> **Warning:** `with_backend` kernels are **hermetic by construction**:
> kaish mounts no host filesystem (your backend is the only I/O path),
> output spill is forced in-memory (no host temp files), and
> background-job output files are disabled. If your embedder previously
> relied on disk spill or `/v/jobs` persistence, that data now stays in
> memory.

`with_backend` also mounts `/dev` (`DevFs`: `/dev/null`, `/dev/zero`,
`/dev/random`, `/dev/urandom`) unconditionally, kernel-owned, alongside
`/v/jobs` and `/v/blobs` — this holds even if your own backend is read-only,
so `cmd > /dev/null` always discards rather than failing as a filesystem
error.

A `with_backend` kernel owns its VFS, so `KernelConfig::with_vfs_budget`
does not see your mounts — cap them yourself by constructing the backing
`MemoryFs` with `MemoryFs::with_budget(Arc<ByteBudget>)`. Both types are
available through `kaish_kernel::vfs`; no direct `kaish-vfs` dependency
needed:

```rust
use kaish_kernel::vfs::{ByteBudget, MemoryFs};

let budget = Arc::new(ByteBudget::labeled(16 * 1024 * 1024, "scratch"));
vfs.mount("/", MemoryFs::with_budget(budget.clone()));
// budget.used() / budget.remaining() are observable at any time.
```

### Output Limits and Spill Mode (`OutputLimitConfig`)

`KernelConfig::output_limit` caps how much a single command's output can grow
before it's truncated (exit code 3 — see [the result contract](#the-result-contract)).
Independent of the byte cap, `SpillMode` decides *where* the overflow goes:

- **`SpillMode::Disk`** (the default): the full output is written to a spill
  file under `paths::spill_dir()` — `$XDG_RUNTIME_DIR/kaish/spill` (tmpfs on
  systemd systems, cleared on reboot) — and the result carries a head+tail
  preview pointing at it (`cat` it to read the rest).
- **`SpillMode::Memory`**: head+tail truncation only — no disk I/O, no
  recoverable file. Memory stays bounded regardless of how much the command
  produced.

| Construction | `SpillMode` |
|---|---|
| `KernelConfig::agent()` / `.agent_with_root()` / `.named()` / `.transient()` (`Sandboxed`, real host mount) | `Disk` |
| `KernelConfig::repl()` (`Passthrough`, real host mount) | `Disk` in principle, but moot — `repl()`'s `output_limit` is `none()` (unlimited) |
| `KernelConfig::isolated()`, or any config `.with_vfs_mode(VfsMountMode::NoLocal)` | `Memory` — forced at construction, no host mount to spill to |
| `Kernel::with_backend(..)` | `Memory` — forced at construction, the embedder owns the VFS and a kernel-side `std::fs` write would bypass it (see the Warning above) |

Forcing beats an explicit request: setting `SpillMode::Disk` on a config that's
`NoLocal` or headed for `with_backend` is silently overridden to `Memory` in
`Kernel::assemble` — neither kernel shape owns a host mount to write to, so an
explicit `Disk` request there would be nonsensical, not honored.

A **host-backed** kernel (`Sandboxed`/`Passthrough`, built with `Kernel::new`)
defaults to `Disk` because it already has a real filesystem — spilling there is
no different from any other write it does. If you want a host-backed kernel
that nonetheless never touches disk (e.g. the output may hold data you don't
want recoverable from a temp file even though the kernel has host access),
opt in explicitly:

```rust
use kaish_kernel::OutputLimitConfig;

let config = KernelConfig::agent()
    .with_output_limit(OutputLimitConfig::agent().in_memory());
```

There's no equivalent flag to force `Disk` on a `NoLocal`/`with_backend`
kernel — by design, since neither owns a host mount to spill to.

> **v0.13.0:** the public `output_limit::spill_aware_collect` function (and its
> private helpers) is removed — it was dead since external-process capture
> moved onto `BoundedStream`/`drain_to_stream`, with spill applied post-hoc at
> the pipeline level (`Kernel::execute_pipeline` → `spill_if_needed`, both
> internal) instead of inline during capture. `OutputLimitConfig` and the
> disk/memory spill behavior documented above are unaffected; there was no
> embedder-facing replacement to migrate to because the function was never a
> supported extension point, just a capture helper that happened to be `pub`.

## Initial Variables and Hermetic Subprocess Env

The kernel is **hermetic by default** — it never reads `std::env::vars()`,
and external commands launched from inside the kernel see only the
variables kaish has marked as exported. Frontends that want shell-like UX
(the bundled REPL, or an embedder that mirrors the host shell) opt in to
OS-env passthrough by populating `initial_vars`:

```rust
use kaish_kernel::ast::Value;
use std::collections::HashMap;

// Bare embedder kernel: hermetic. Subprocesses see no PATH, HOME, etc.
let kernel = Kernel::new(KernelConfig::named("isolated"))?;

// Embedder that wants its own curated env:
let mut vars = HashMap::new();
vars.insert("PATH".to_string(), Value::String("/usr/bin:/bin".into()));
vars.insert("LANG".to_string(), Value::String("C.UTF-8".into()));
let kernel = Kernel::new(
    KernelConfig::named("curated").with_initial_vars(vars),
)?;

// Shell-like passthrough (what kaish-repl does):
let env: HashMap<String, Value> = std::env::vars()
    .map(|(k, v)| (k, Value::String(v)))
    .collect();
let kernel = Kernel::new(KernelConfig::repl().with_initial_vars(env))?;
```

Builders:

- `with_var(name, value)` — add a single entry
- `with_vars(map)` — extend the existing map (last write wins)
- `with_initial_vars(map)` — replace the entire map

All entries are marked exported when the kernel boots, so they reach
external subprocesses (`printenv`, `cargo`, `git`, …) directly. For
*per-call* variables, use `ExecuteOptions::with_vars` (next section)
instead of mutating kernel state.

## Per-Call Execution: `ExecuteOptions`

`Kernel::execute_with_options` is the canonical per-call surface:

```rust
use kaish_kernel::ExecuteOptions;
use std::time::Duration;

let result = kernel.execute_with_options(
    "build-report $REQUEST_ID",
    ExecuteOptions::new()
        .with_vars(request_vars)                 // function-local overlay
        .with_timeout(Duration::from_secs(30))   // per-call deadline
        .with_cwd("/mnt/repos/kaish".into()),    // per-call working dir
).await?;
```

> **Note:** `ExecuteOptions::with_vars` replaces `Kernel::execute_with_vars`,
> which is **deprecated**.

Fields:

- **`vars`** — per-invocation variable overlay with bash function-local
  semantics: a scope frame is pushed, each var set and marked exported
  (visible to the script and any subprocesses it spawns), and the frame is
  popped on return — inner assignments vanish with it, outer values and
  export bits are restored.
- **`timeout`** — per-call deadline; on expiry the result has exit code
  124. `Some(Duration::ZERO)` is a dry-run: validate and return 124
  without executing. A custom tool that legitimately outlives this
  deadline (a provider call that runs minutes) can suspend it with
  `ctx.patient(budget)` — see [Patient tools](#patient-tools-suspending-the-script-timeout).
- **`cancel_token`** — an embedder-owned
  `tokio_util::sync::CancellationToken`, *raced* against the kernel's
  internal token for the duration of the call (not stored). Cancellation
  cascades to forks and external children (SIGTERM → grace → SIGKILL on
  the process group).
- **`interrupt`** — `with_interrupt(Arc<dyn Fn() -> bool + Send + Sync>)`, a
  polled interrupt check for embedders whose thread can't fire `cancel_token`
  while execution runs — the motivating case is `wasm32-unknown-unknown`:
  single-threaded, so the page's main thread can only flip a
  `SharedArrayBuffer` flag for a Web Worker to poll, never cancel a token from
  outside. The kernel checks the closure at its existing cancellation
  checkpoints; a firing check takes the same exit-130 path as
  `Kernel::cancel()`/`cancel_token`, and session state survives. Scoped to the
  one call and cleared on every exit path — prefer `cancel_token` when your
  embedder's threading model allows it.
- **`cwd`** — per-call working directory override.
- **`stdin`** — standard input for this call as a ready, bytes-typed buffer
  (`impl Into<Vec<u8>>` — a `&str`/`String` or a raw `Vec<u8>` both work),
  consumed by the first top-level command that reads stdin (shell draining
  semantics — a later reader sees nothing). Lets an embedder feed piped input,
  e.g. `printf '…' | kaish -c 'sort'`, binary included — a byte-aware builtin
  (`wc -c`, `cat`, `cmp`, …) sees it intact, while a text-only builtin still
  refuses non-UTF-8 loudly when it asks for text. A redirect (`< file`/heredoc)
  on the command still takes precedence. Eager: the whole buffer must exist
  before the call. For a **lazy** stream — fed only if a command reads stdin,
  so an open process stdin that never sends EOF doesn't block a command that
  never reads it — use `Kernel::execute_with_pipe_stdin(_streaming)` with a
  `PipeReader` instead (this is how the non-interactive `kaish` CLI forwards its
  own process stdin, e.g. `sleep 10 | kaish -c 'echo hi'`). See
  [docs/binary-data.md](binary-data.md) for the full text-vs-bytes design
  behind this (`Value::Bytes`, `read_stdin_to_text` vs `_bytes`, which
  builtins are binary-aware).
- **`traceparent` / `tracestate` / `baggage`** — W3C trace context;
  kaish's execution span parents onto your trace, and baggage merges back
  out through `ExecResult.baggage`.

## Argv-Native Execution: `execute_argv`

`Kernel::execute(&str)` is string-native — it lexes and parses its input. If your
embedder already holds **tokenized** arguments (a structured tool call, a
multicall-style frontend), re-quoting them into a string just to have the lexer
split them apart again is wasteful and **lossy**: `ToolArgs::to_argv()` — the
argv-reconstruction step builtins use internally to feed their clap parsers —
stringifies typed values, so a `Value::Json` record loses its structure in the
round-trip. A `Value::Bytes` blob is worse than lossy: in a **named/flag**
argument it's a loud error (`to_argv()` returns
`Result<Vec<String>, ToolArgvError>`, not a bare `Vec<String>`) rather than
silent corruption; in a **positional** argument it renders as an opaque
`[binary: N bytes]` placeholder without erroring, since a clap-reflected
positional field is a validation-only sink no builtin reads for its value.
`ToolArgs::to_argv_excluding(keys)` is the same reconstruction with given
named keys skipped entirely — for a tool that deliberately reads one of its
own params raw off `args.named` (to preserve a `Value::Bytes` payload past the
argv/text boundary) instead of through the round-trip (`write`'s `content`
param does this). `execute_argv` is the peer door that skips the round-trip
entirely:

```rust
use kaish_kernel::ast::Value;

// Run one command whose arguments are already tokenized.
let result = kernel.execute_argv("grep", &[
    Value::String("--ftype".into()),
    Value::String("rust".into()),
    Value::String("needle".into()),
    Value::String("src".into()),
]).await?;

// Typed values pass straight into ToolArgs.positional — no stringification.
let result = kernel.execute_argv("my-tool", &[Value::Bytes(blob)]).await?;
```

Semantics:

- **Tokens are literal.** No glob expansion, no `$VAR` interpolation, no command
  substitution, no word splitting — the "single-quoted word" rule taken to its
  end. `execute_argv("echo", &[Value::String("*.txt".into())])` emits `*.txt`.
  And no **number coercion**: a `Value::String("00")` stays `"00"` (the string
  door's lexer would coerce the bare word `00` to an integer and print `0`). Pass
  a `Value::Int`/`Value::Float` when you mean a number — the type is yours to
  choose, which is the point of the typed door. **Exception:** a leading `~` is
  expanded against the session `HOME`, matching the string door (kaish expands
  `~` uniformly, even in quotes — so the doors agree); pass a pre-resolved path
  if you need it byte-literal.
- **One simple command only.** Pipelines, `&&`/`||`, control flow, and `$()` have
  no argv encoding — use `execute(&str)` for those. The two are *peers*: argv is
  not a subset that drops expressiveness, it's a different door that converges with
  the string door at the shared dispatch chain.
- **Same tail as the string door.** Command resolution (aliases, user tools,
  `.kai` scripts, externals, backend tools), `--json`, and the confirmation latch
  all apply, so a latched `rm` still returns exit 2 with a request on
  `ExecResult.latch` (see [Destructive-op rails](#destructive-op-rails-reading-the-latch-nonce)).
  The kernel's pre-execution *syntax* validator does not run — argv carries no
  shell syntax — but a tool's own `validate()`/clap parse still does.
- **Typed-passthrough caveat.** Because builtins re-parse their own `to_argv()`
  internally (the two-layer clap model), the un-stringified-value win fully lands
  only for tools that read `args.positional` directly (the documented pattern),
  not those that trust their clap struct after a `to_argv()` round-trip. A
  `Value::Bytes` passed as a **named** argument to such a tool surfaces as the
  tool's own `to_argv()` failure (`ExecResult::failure`), not a silent stringify.

Concurrent callers serialize on the same execute lock as `execute`, and the
kernel's configured `request_timeout` applies (a hung builtin or external is
interrupted at the deadline with exit code 124). There is no per-call options
surface yet — if you need per-call timeout/cancel/vars/cwd, use the string door
(`execute_with_options`) until an `execute_argv_with_options` lands.

## Custom Tools

Register custom builtins using the `configure_tools` callback on
`with_backend()` (or a `ToolRegistry` you pass to your backend). The `Tool`
trait lives in `kaish-tool-api` and is re-exported by the kernel:

```rust
use std::sync::Arc;
use async_trait::async_trait;
use kaish_kernel::{Kernel, Tool};
use kaish_kernel::tools::{ToolArgs, ToolCtx, ToolSchema};
use kaish_types::ExecResult;

struct MyTool {
    state: Arc<MyState>,
}

#[async_trait]
impl Tool for MyTool {
    fn name(&self) -> &str { "my-tool" }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("my-tool", "Does something useful")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        ExecResult::success("hello from my-tool")
    }
}

let kernel = Kernel::with_backend(backend, config, |_| {}, |tools| {
    tools.register(MyTool { state: my_state.clone() });
})?;
```

Custom tools registered this way are available as shell builtins — they
appear in `tools --json`, have help text, and participate in tab
completion.

Notes:

- `ctx` is `&mut dyn ToolCtx` — a capability trait giving VFS access, cwd,
  stdin, and cancellation without depending on kernel internals.
- If your tool renders its own output (including handling `--json`
  itself), mark the schema `.with_owned_output()` — the kernel then passes
  `--json` through instead of re-rendering your `ExecResult`. It also passes
  `--help`/`-h` through: an owned-output tool re-parses its own argv, so the
  kernel's generic whole-tool help router stands aside and lets the tool render
  its own help (including leaf/subcommand help its internal parser knows about).
  This makes `--help`/`-h` handling **your** responsibility — unlike `--json`,
  there is no post-execute safety net. If you re-parse with clap this is
  automatic (clap emits help on `--help`); a hand-rolled parser must handle it
  explicitly, or `--help` will fall into your default action.

### Patient tools: suspending the script timeout

The script timeout (`ExecuteOptions::timeout` / `KernelConfig::request_timeout`)
is one budget for the whole script — sized for shell work, not for a
model-backed tool whose provider call legitimately runs minutes. Stretching
the script budget to minutes would hand a `while true` loop the same minutes,
so the two jobs get separate knobs: a tool declares its own budget with
`ctx.patient`.

```rust
async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
    let cancel = /* clone ctx.cancel via the ExecContext downcast */;

    // While the guard is held, the script clock is frozen and this hold's
    // own budget governs; dropping it resumes the script clock with the
    // remaining time it had at acquire.
    let _guard = ctx.patient(Duration::from_secs(300));

    tokio::select! {
        result = call_provider(args) => to_exec_result(result),
        _ = cancel.cancelled() => ExecResult::failure(130, "interrupted"),
    }
}
```

Semantics:

- **The hold's budget has teeth**: if the tool outlives it, the watchdog
  fires and the script exits 124 — a hung provider call cannot wait forever.
- **Cancellation stays live**: `Kernel::cancel()` and the embedder
  `cancel_token` fire immediately during a hold — only the timer pauses.
  A patient tool must still `select!` its wait against `ctx.cancel`,
  as above.
- **Script code has no path to the guard** — only Rust tool code can be
  patient, so the script-level budget keeps its teeth against shell loops.
- **The `timeout` builtin is not suspended**: `timeout 5 my-tool` is an
  explicit user bound on the command and ignores patient holds.
- With no script timeout configured the guard is inert (nothing to
  suspend); holds nest, and the guard may be held across `.await` points.

## Sandboxing and External Commands

Builtins go through the VFS and respect its mounts; **external commands,
`exec`, and `spawn` access the real filesystem directly** (they're OS
processes). Two gates:

- Compile-time: build without the `subprocess` feature — the capability
  doesn't exist.
- Runtime: `allow_external_commands = false` in `KernelConfig` — PATH
  lookups return "command not found" and `exec`/`spawn` error.
  `KernelConfig::isolated()` sets this by default.

### Preflighting a script for external commands

To gate a script for consent (e.g. block until external commands are approved),
classify each command node *before* executing. Walk the parsed AST and ask the
kernel how it will resolve each name — don't re-derive the rules, or your gate
silently disagrees with what kaish actually runs the day resolution changes:

```rust
use kaish_kernel::{parser, ast::Stmt, CommandKind};

let program = parser::parse(src)              // public parser + AST
    .map_err(|_errors| /* surface parse errors */ ())?;
for stmt in &program.statements {
    if let Stmt::Command(cmd) = stmt {        // walk however your policy needs
        let kind = kernel.classify_command(&cmd.name).await;
        if kind.escapes_kernel() {
            // External or Dynamic — escapes to PATH (or can't be resolved
            // statically). Gate it.
        }
        // Builtin / UserTool / Special run in-process under the VFS and
        // capability model.
    }
}
```

`CommandKind` is `#[non_exhaustive]`, so a `match` needs a wildcard arm — and the
safe default for an unrecognized kind is to gate it. `escapes_kernel()` captures
the two buckets a consent gate scrutinizes without spelling out the variants.

`classify_command` mirrors the interpreter's real resolution order — including
**alias expansion** — so a name like `readonly` (no kaish builtin; resolves to an
external binary) reports `External`, and an `alias cat=/bin/something` makes `cat`
report `External` too, the same thing it would actually run. The safe direction of
any residual imprecision is `External`/`Dynamic` — it never under-reports a `PATH`
escape as internal (`/v/bin/cat` and `.kai`/backend tools over-report as
`External`). The consent UX and the block-the-script loop are embedder policy —
the kernel supplies only the classification.

## Path Composition with XDG Primitives

kaish exports XDG base directory primitives so embedders can compose their
own application-specific paths:

```rust
use kaish_kernel::{
    xdg_data_home,    // ~/.local/share or $XDG_DATA_HOME
    xdg_config_home,  // ~/.config or $XDG_CONFIG_HOME
    xdg_cache_home,   // ~/.cache or $XDG_CACHE_HOME
    xdg_runtime_dir,  // $XDG_RUNTIME_DIR or /tmp
    home_dir,         // ~ or $HOME
};

fn myapp_data_dir() -> PathBuf {
    xdg_data_home().join("myapp")
}
```

For user-facing path handling, use `expand_tilde`:

```rust
use kaish_kernel::expand_tilde;

let path = expand_tilde("~/projects/myrepo");
// → /home/username/projects/myrepo
```

## Programmatic VFS Access

The `Filesystem` trait (from `kaish-vfs`, re-exported as
`kaish_kernel::vfs::Filesystem`) takes `&Path`, not `&str`:

```rust
use std::path::Path;
use kaish_kernel::vfs::Filesystem;

let data = kernel.vfs().read(Path::new("/v/jobs/1/stdout")).await?;
```

## Job Output Capture

kaish provides bounded streams for capturing command output without OOM
risk.

### BoundedStream for Custom Output Capture

```rust
use kaish_kernel::{BoundedStream, drain_to_stream, DEFAULT_STREAM_MAX_SIZE};
use std::sync::Arc;
use tokio::process::Command;

async fn capture_with_bounds() -> anyhow::Result<String> {
    let mut child = Command::new("some-chatty-command")
        .stdout(std::process::Stdio::piped())
        .spawn()?;

    // Create bounded stream (10MB max, oldest data evicted on overflow)
    let stream = Arc::new(BoundedStream::new(DEFAULT_STREAM_MAX_SIZE));

    // Drain stdout into the bounded stream
    if let Some(stdout) = child.stdout.take() {
        let stream_clone = stream.clone();
        tokio::spawn(async move {
            drain_to_stream(stdout, stream_clone).await;
        });
    }

    child.wait().await?;

    // Read captured output (safe even if process wrote gigabytes)
    Ok(stream.read_string().await)
}
```

### JobFs for Background Job Observability

The kernel automatically mounts `JobFs` at `/v/jobs`, exposing background
job state:

```
/v/jobs/
├── 1/
│   ├── stdout    # Captured stdout (bounded)
│   ├── stderr    # Captured stderr (bounded)
│   ├── status    # "running", "done:0", "latched", or "failed:N"
│   ├── command   # Original command string
│   └── latch     # Confirmation-latch request (JSON) if gated, else empty
├── 2/
│   └── ...
```

```sh
# In kaish scripts
sleep 10 &              # Starts job 1
jobs                    # Shows: [1] running  /v/jobs/1/
cat /v/jobs/1/status    # "running"

# After completion
cat /v/jobs/1/stdout    # Job's stdout
cat /v/jobs/1/status    # "done:0" on success, "failed:N" otherwise
```

A destructive op backgrounded under `set -o latch` (`rm x &`) gates in the
background rather than running: `status` is `latched`, `JobInfo.latch` (from
`JobManager::list`/`get`) and `/v/jobs/{id}/latch` (JSON) carry the pending
`LatchRequest`, and `wait` surfaces it on the result's `.latch` field (exit 2).
An embedder fulfills the backgrounded gate with `Kernel::confirm(&latch)` — the
same API as a foreground gate.

The status strings are exactly `running`, `done:0`, and `failed:{code}` —
match on those, not on `completed`.

## Frontend Completion Helpers (`kaish_client::completion`)

Answering Tab in a frontend (a REPL, a browser playground, any custom UI
around the kernel) needs two things: figuring out *what* the cursor is
completing, and turning a live kernel's schemas/vars into candidate
spellings. Both are extracted into `kaish_client::completion` so every
frontend shares one implementation instead of re-deriving it — the bundled
`kaish-repl` and the kaish-extras browser playground both consume this crate
rather than duplicating the logic.

```rust
use kaish_client::completion::{
    detect_completion_context, word_start, current_command, flag_candidates,
    CompletionContext,
};

// What kind of thing is being completed at `pos` in `line`?
match detect_completion_context(line, pos) {
    CompletionContext::Command => { /* complete a tool/alias name */ }
    CompletionContext::Variable => { /* complete a $VAR / ${VAR */ }
    CompletionContext::Path => { /* complete a filesystem path, or a
                                     flag if the word starts with `-` */ }
}

let start = word_start(line, pos); // byte offset the word under the cursor begins at

// Given the governing command and its ToolSchema, offer canonical flag spellings
if let Some((cs, ce)) = current_command(line, pos) {
    let candidates = flag_candidates(&schema.params, &line[cs..ce]);
    // -> canonical "--long" and "-x" spellings; snake_case field-id aliases
    //    stay reachable as input but aren't offered as candidates
}
```

Context detection is pure (no kernel access needed); turning a
`CompletionContext` into actual candidates is the frontend's job — walk
`kernel.tool_schemas()` for commands/flags, `kernel.list_vars()` for
variables, `kernel.vfs()` for paths, as `kaish-repl` and kaish-extras both do.

## Exported Types

The `kaish_kernel` crate root re-exports the embedding surface:

- **Core**: `Kernel`, `KernelConfig`, `VfsMountMode`, `ExecuteOptions`,
  `CommandKind`, `KernelBackend`, `LocalBackend`, `Tool`, `ToolRegistry`,
  `ExecContext`, `OutputLimitConfig`
- **Jobs**: `BoundedStream`, `StreamStats`, `drain_to_stream`,
  `DEFAULT_STREAM_MAX_SIZE`, `JobFs`
- **Paths**: `home_dir`, `xdg_data_home`, `xdg_config_home`,
  `xdg_cache_home`, `xdg_runtime_dir`, `expand_tilde`
- **VFS** (module `kaish_kernel::vfs`): `Filesystem`, `VfsRouter`,
  `MemoryFs`, `LocalFs`, `MountInfo`

Pure data types (`ExecResult`, `OutputData`, `Value`, `ToolSchema`,
`ToolArgs`, …) live in the leaf crate `kaish-types`; the tool author API
(`Tool`, `ToolCtx`, `KernelBackend`) in `kaish-tool-api`. Depend on those
directly if you're writing tools without linking the whole kernel.

## Best Practices

1. **Use `with_backend()` for full control** — implement `KernelBackend`
   and let the hermeticity guarantees keep I/O inside your storage model.

2. **Use `ExecuteOptions` for per-call state** — vars, timeout, cwd,
   cancellation, trace context. Don't mutate kernel state between calls.

3. **Compose paths with XDG primitives** — don't hardcode paths.

4. **Start from the minimal feature set** — add `subprocess`/`host`
   only when the embedder needs them; the attack surface is named, not
   inherited.
