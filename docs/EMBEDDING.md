# Embedding kaish

This guide shows how to embed the kaish kernel in your application: kernel
construction, capability features, per-call execution options, custom tools,
and output capture.

## Stability

kaish is pre-1.0 (currently 0.14.x, MSRV 1.85). The language has settled;
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
| 2 | Usage error, or a refusal that names what to do instead (e.g. `kaish-trash empty` without `--confirm`) | Read `err` |
| 3 | Output truncated by the output limit | `original_code` holds the real exit code. With disk spill the message names the spill file — `cat` it, or narrow the query; memory-spill kernels (`with_backend`, `SpillMode::Memory`) truncate in place with no file |
| 124 | Timeout (`timeout_ms`, default 30 s) | — |
| 130 | Cancelled | — |

**Assert on the code and the kind, never on the wording.** The exit code is
contract, and so is the `std::io::ErrorKind` a VFS refusal carries — a write
to a read-only synthetic mount is `ErrorKind::Unsupported` (not
`PermissionDenied`: no permission would make it work) and the statement exits
**1**. The text in `err` is prose written for a human and improves between
releases without notice, so a boundary test that pins it fails on a wording
change that changed no behavior.

Embedders typically run a fresh kernel per request — variables, functions,
aliases, `set -o` options, and `cwd` reset each time.

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
  a swappable kernel connection. `EmbeddedClient::shutdown()` calls
  `Kernel::shutdown()`: it cancels every background job and waits, bounded —
  see "Teardown" below for the contract.

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
  The old `kaish-tools-git` crate (git builtin + `GitVfs`, removed in 0.9.0) is
  being reinvented as a shallow, safety-first git plugin in
  [kaish-extras](https://github.com/tobert/kaish-extras) — history, autopsy,
  and design intent live in `docs/git.md` there, and that repo is its
  maintained home going forward.
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

Other builders: `.with_trash(bool)` (destructive-op
rails — see below), `.with_vfs_budget(bytes)` / `.without_vfs_budget()` (cap
in-memory VFS growth), `.with_skip_validation(bool)`, `.with_initial_vars(map)`
(below).

#### Deciding what a statement may do

kaish does not decide. It tells you what a statement would do, exactly and
before anything happens, and you decide — see
[Command analysis](#command-analysis-plan_program) below, which is the
mechanism for all of it.

The one exception is `set -o trash`, and it is not a decision: a truncating
overwrite or delete copies the prior content to the trash first so the write
is recoverable. `kaish-trash empty` always asks, because it discards the
recovery net every other operation depends on — `--confirm` there is a flag,
not a policy, takes no token, records nothing, and no session setting turns
it off.

See [LANGUAGE.md](LANGUAGE.md) for the full trash semantics.

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
variables kaish has marked as exported.

> **One exception, and it only ever turns a rail *on*.** Four `KernelConfig`
> presets read `KAISH_TRASH` from the process
> environment at construction (`repl()` and the agent presets — the ones a
> frontend uses). Nothing else in the kernel touches `std::env`, and the
> direction is safe: env can enable the trash, never
> disable one an embedder asked for. The right long-term shape is for the
> *frontend* to read env and pass `KernelConfig`; until then, an embedder that
> needs a guaranteed-hermetic construction builds its own `KernelConfig`
> rather than starting from a preset. Frontends that want shell-like UX
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

### Neither `timeout` nor `cancel_token` bounds a background (`&`) job

`ExecuteOptions::timeout` and `cancel_token` govern the call that started a
`&` job, not the job itself (GH #245). `cmd &` returns `[1]` the instant it's
registered — by the time the deadline or cancellation could fire, the call
that would have been bounded by it has already returned. The backgrounded
pipeline runs on its own fork with its own independent
`tokio_util::sync::CancellationToken`, deliberately detached so it survives
the parent call's cancellation (correct shell semantics — `&` is meant to
outlive the command that started it). `Kernel::cancel()` inherits the same
boundary: it cancels the *current* foreground execution, never a
backgrounded one. The only levers that reach a running `&` job are `kill
%N` from a script, or [`Kernel::cancel_all_jobs`]/[`Kernel::shutdown`] from
the embedder. There is no per-job timeout at all unless the script sets one
itself: `timeout 600 cmd &`.

### `Kernel::reset()` does not touch background jobs

`reset()` clears scope and cwd; jobs are not session state, so a `&`
started before `reset()` keeps running, stays in `jobs`, and the job ID
counter keeps counting up (GH #245). An embedder that treats `reset()` as
"new session" — a fresh MCP conversation reusing one kernel, say — inherits
every job the previous conversation backgrounded. Call
[`Kernel::cancel_all_jobs`] first if that inheritance is not wanted.

### The tokio runtime must outlive any background job you start

`execute_background` spawns onto whatever tokio runtime is current when
`execute()` runs (a bare `tokio::spawn`) — kaish does not capture or manage
its own `Handle` (GH #247). An embedder that builds a short-lived runtime per
call (`Runtime::new()` + `block_on` per request — a common pattern for a
one-request-per-thread server) has every background job it started in that
call die, silently, the instant the runtime drops mid-execution. `Kernel` is
`Send + Sync` and gives no signal that this matters. If your embedding
pattern tears down runtimes between calls, either keep one long-lived
runtime for any kernel that backgrounds work, or avoid `&` entirely on a
per-call runtime.

### A hard-killed kaish process can orphan its external children

`setpgid` + a pidfd kill, and tokio's `kill_on_drop`, all need *your* process
to still be alive and running code. None of them fire on `kill -9`, a
segfault, or an OOM kill — so an external command started under an embedder
that dies that way keeps running, unreparented to anything that will stop it.

`KernelConfig::with_kill_children_on_parent_death(true)` arms Linux's
`PR_SET_PDEATHSIG(SIGKILL)` in each child's `pre_exec`, so the OS kills it the
instant the parent dies, for any reason, with no cleanup path of ours
involved:

```rust
let config = KernelConfig::default().with_kill_children_on_parent_death(true);
```

On by default for `KernelConfig::agent()` and `agent_with_root()`, off
everywhere else — the same "protection on for the agent preset, opt in
elsewhere" split `vfs_budget_bytes` uses. It is not unconditional because it
costs something a human at a REPL may not want: an armed child cannot outlive
its shell at all, and cannot opt out from inside (unlike SIGHUP, which
`nohup`/`disown` exist to escape). A REPL user who backgrounds a long download
and exits expects it to survive; an agent embedder expects the opposite.

**Linux only.** macOS has no `PR_SET_PDEATHSIG`, and no equivalent that works
without a live watcher process (`kqueue`'s `NOTE_EXIT` needs one). The flag is
accepted and has no effect there rather than being faked with something
weaker — a macOS embedder that needs the guarantee supplies it from outside
the process (a process group the supervisor kills, or a container).

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
  `.kai` scripts, externals, backend tools) and `--json`.
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

### Command analysis: `plan_program`

`plan_program(source)` returns one `PlannedStatement` per statement of
`source` — its position (`index`) and its `Plan`: the statement rendered back
to shell text **unexpanded**, its kind, every command it contains (loop
bodies and `$(...)` substitutions included), and its variable analysis —
`free_variables` (session names the statement reads) and `bound_variables`
(names it writes or binds itself).

Nothing executes. A plan is parse information: `${HOME}` and `$(...)` appear
exactly as written, no substitution has run, and no filesystem has been
touched. That is the point — you judge what the statement *asked for*, before
anything it names can happen.

```rust
use kaish_kernel::plan_program;

for planned in plan_program(src).map_err(|_errors| /* parse errors */ ())? {
    for cmd in &planned.plan.commands {
        // cmd.name, cmd.args (redaction-aware), cmd.redirects, cmd.background
    }
    // Decide however your policy needs, keyed by planned.index.
}
```

`Kernel::plan_program(source)` is the same read as a method on a kernel.

**Judging against live state.** `free_variables` names what a statement reads,
so `Kernel::get_var` closes the loop — plan, look up what it depends on, and
decide with the values in hand:

```rust
for planned in kernel.plan_program(script)? {
    for name in &planned.plan.free_variables {
        let value = kernel.get_var(name).await;  // judge with live state in hand
    }
}
```

A name that a statement both reads and writes lands in `bound_variables`,
never `free_variables` — the safe direction, since peeking it would judge the
statement against a value the statement itself replaces.

**What the free set covers.** The read set is complete against the statement's
**lexical** surface — kaish has no `eval` and no indirect expansion, so every
read is visible in the source: every `${x}`, interpolation, `${#x}`,
`${x:-default}`, `[$k]` subscript, and `$((…))` identifier. It does not cover names bound at *runtime* by a builtin
that takes them as arguments — `read`, `export`, `unset`, and `push` write
session variables that argv-level analysis cannot see. A statement like
`read TOKEN && curl -H "Authorization: $TOKEN"` reports `TOKEN` as free, and
the value you peek is the one from *before* the `read`.

**Credentials.** Every literal `--confirm=<key>` is redacted from the plans and
**not** returned — you hold `source` and need no second copy. That is the only
redaction kaish performs: it minted that key and knows it outright. kaish
ships no secret detector, because a shell cannot define what a secret is. Run
your own pass over the plans if you need more.

**What this is not.** A plan tells you what a statement would run, not what it
will produce. `rm $(find / -name '*.tmp')` plans as an `rm` whose argument is
produced by a `find` — you see the shape and the producer, never the resolved
paths. Judge accordingly.

#### Heredoc bodies: `PlannedCommand.heredocs`

An agent that runs another language runs it through a heredoc — `python3
<<'PY'`, `sqlite3 <<SQL`. `PlannedCommand.heredocs` publishes each one with the
shell framing already off, so the body goes straight to whatever reads that
language:

```rust
for planned in kernel.plan_program(script)? {
    for command in &planned.plan.commands {
        for heredoc in &command.heredocs {
            // `python3` + `PY` + the program, no quoting to undo.
            let program = heredoc.body.display();
        }
    }
}
```

`delimiter` is the word as written with quotes removed (`PY`, `SQL`) — the hint
an author picked for the language they were about to write, and a hint only.
`body` is verbatim: no tab stripping, no expansion, no quoting, no
kernel-internal rewriting. `free_variables` names what plugs into *this* body,
scoped to it rather than to the whole statement.

**`literal` decides what the body is worth.** A quoted delimiter (`<<'PY'`)
means the body reaches the command exactly as published. An unquoted one
(`<<PY`) means the shell expands `${…}` and `$(…)` first, so a substitution can
land inside a string literal in the other language and the published text is
what was *asked for*, not what runs.

#### Closing that gap: `expand_fragment`

`Kernel::expand_fragment(source, addr, scope)` resolves an unquoted body
against values you supply, addressed by statement index and the flat
`PlannedHeredoc::index`:

```rust
use kaish_kernel::{Expansion, FragmentAddr};

match kernel.expand_fragment(script, FragmentAddr::new(0, 0), &scope)? {
    Expansion::Complete(text) => { /* exactly what the command reads */ }
    Expansion::Blocked { holes } => {
        for hole in &holes {
            // hole.source is `$(date +%s)`; hole.plans is what it would run.
        }
    }
}
```

Two rules shape it, and both are the embedder-in-control preference:

- **The scope is yours, not the kernel's.** Nothing is read from session state.
  Pair it with `get_var` when the session's values are the ones to judge
  against, and supply different ones when they are not — `read TOKEN` binds at
  runtime, so a peeked value would be stale in exactly the case that matters.
- **A `$(…)` comes back, it does not run.** Running it is a decision with a
  clock and a blast radius, and it is the same decision you are asking about.
  Each substitution is a `Hole` carrying its own `Plan`. Judge it safe and you
  run it in a kernel of your own construction — your capabilities, your
  timeout, your cancellation — then expand again with the answer in scope.

`Complete` means "this is the text the command reads", and only that. A name
the body reads and your scope does not carry expands to the empty string,
because that is what kaish does when it executes; a stricter rule would hand
back a body the command never sees. Check `free_variables` against your scope
first when you need every value accounted for. A body reading `$?`, `$$`, or a
positional is a loud error — a supplied scope cannot carry those, and expanding
them against a fresh session would invent a value.

`Blocked` carries no text at all. Half-expanded source reads as ground truth to
whatever parses it next, so the type cannot represent it.

**What this covers.** Heredocs, not every way a program can arrive: `python3 -c
'…'` puts one in an argument, `echo … | python3` puts it in a pipeline, and
`write /tmp/x.py <<'PY'` followed by a later `python3 /tmp/x.py` splits it
across statements. This raises the quality of analysis on the common case; the
airtight configuration is still the `subprocess` feature off.

#### Why this, and not a gate

kaish used to carry an approval ledger: requests, grants, attempts, an
append-only record, a policy language, and a decision chain the kernel ran
inside its own lock. It was removed before 0.14.0, and what replaced it is the
surface above.

The reason is that the kernel was the wrong place for it. Holding a decision
means owning a task, a clock, and a cancellation policy on the embedder's
behalf — three choices no default gets right, made once for every embedder.
And an interception mechanism the kernel defines is one an embedder has to
adopt whole: kaijutsu had already built its own confirmation flow and was
using kaish's only as a transport for a decision it had already made.

An embedder that can *see* a statement precisely does not need the kernel to
hold state on its behalf. It plans, decides with its own policy in its own
storage, and either runs the statement or does not. That composes with
whatever the embedder already has — a UI prompt, a model review, a queue for a
shift change — none of which need a kernel change, because the kernel never
assumed which one it was serving.

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

let data = kernel.vfs().read(Path::new("/v/jobs/1/status")).await?;
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

### Sharing one JobManager across kernels

Each kernel builds its own `JobManager` unless you supply one. An embedder
that builds a kernel per request loses every `cmd &` job when that kernel
drops — ids, status, and output all live on the manager. Hand the same
manager to every kernel and jobs survive between calls:

```rust
use std::sync::Arc;
use kaish_kernel::scheduler::{JobId, JobManager};
use kaish_kernel::{Kernel, KernelConfig};

// Built once, held for the process's lifetime.
let jobs = Arc::new(JobManager::new());

// Every per-request kernel adopts it.
let kernel = Kernel::new(KernelConfig::agent().with_job_manager(jobs.clone()))?;
kernel.execute("cargo build &").await?;
drop(kernel);

// The next kernel sees job 1 — same table, same id space.
let next = Kernel::new(KernelConfig::agent().with_job_manager(jobs.clone()))?;
assert!(next.jobs().exists(JobId(1)).await);
```

A shared manager carries shared settings: `kill_grace` and
`persist_output_files` are stamped onto it at kernel construction, so the
last kernel built wins for both. A hermetic kernel (`NoLocal`, or any
`with_backend` kernel) turns `persist_output_files` off for every kernel on
that manager. Share a manager between kernels configured alike, or accept
the last writer.

### JobFs for Background Job Observability

The kernel automatically mounts `JobFs` at `/v/jobs`, exposing background
job state:

```
/v/jobs/
├── 1/
│   └── status    # "running", "stopped", "done:0", "killed:N", or "failed:N"
│   ├── command   # Original command string
│   ├── stdout    # Job's stdout so far — live while it runs
│   ├── stderr    # Job's stderr so far — live while it runs
├── 2/
│   └── ...
```

```sh
# In kaish scripts
cargo build 2>&1 &          # Starts job 1, returns immediately
jobs                        # Shows: [1] running  /v/jobs/1/
cat /v/jobs/1/status        # "running"
cat /v/jobs/1/stdout        # Whatever the build has printed so far
```

`stdout` and `stderr` are live for an **external** command run by the job:
its drain task tees each 8 KiB chunk into the node as the child emits it.
GH #240 had removed both nodes because they filled only once, at completion,
while four docs promised a live stream — they are back on the terms the docs
always claimed.

Three limits, stated because an embedder polling these needs to predict them:

- **A builtin is not a live producer.** A kaish builtin returns its whole
  output as a value when it finishes, so `echo hi &` fills the node in one
  write at completion — and so does `cargo build 2>&1 | tee build.log &`,
  because kaish's `tee` is a builtin. Drop the `| tee`: the job's own stream
  *is* the log.
- **Only the last stage of a pipeline reaches `stdout`.** An upstream stage's
  output is the next stage's stdin, not the job's stdout. `stderr` takes every
  stage's, since stderr is not piped. One consequence: in a job mixing
  builtins and externals, once any external has written stderr the
  completion write is skipped, so a builtin stage's stderr stays in the job's
  `ExecResult` and does not reach the node.
- **Each node is a 10 MB ring** that evicts its oldest bytes. A job that
  outruns it loses its head, not its tail; redirect to a file
  (`cmd > /tmp/out.log &`) when the whole output matters.

From Rust, `JobManager::read_stdout(id)` / `read_stderr(id)` return the same
snapshot (`None` for an unknown job, `Some(vec![])` for one that has written
nothing yet). To tail a job without a poll loop, take
`JobManager::streams(id)` and await `BoundedStream::changed_since`:

```rust
let streams = kernel.jobs().streams(id).await.expect("job exists");
let mut seen = 0;
loop {
    let stats = streams.stdout.changed_since(seen).await;
    seen = stats.total_written;
    // ... consume streams.stdout.read().await ...
    if stats.closed {
        break; // the job finished; nothing more is coming
    }
}
```

The streams close when the job's result is in, so `stats.closed` is the
caller's stop condition — no timeout guessing.

The status strings are exactly `running`, `stopped`, `done:0`,
`killed:{code}`, and `failed:{code}` — match on those, not on `completed`.
`killed:{code}` marks a job terminated by `kill %N` (or an embedder cancel):
the job stays tracked with its result and output until reaped, so a killed
job is still distinguishable from one that never existed, and `wait %N` still
returns its result (GH #244). `kill %N` waits for the job to actually unwind
(bounded by `kill_grace` + 3s) before exiting 0; `kill --no-wait %N` returns
at dispatch. The `JobManager` keeps at most 100 finished jobs — enforced at
registration and whenever completion is observed (`list`, `wait`); oldest
evicted first — tune with
`JobManager::set_finished_retention`. A session that registers jobs and then
never calls anything holds what it registered; there is no background
sweeper.

`JobId`/`JobStatus`/`JobInfo` (`kaish-types`) implement `Serialize`/
`Deserialize` (plus `schemars::JsonSchema` behind the `schema` feature), so an
embedder can serialize `JobManager::list()`/`get()` output directly rather
than hand-rolling a mirror struct. `JobStatus`'s wire spelling under
serde is lowercase (`"running"`/`"stopped"`/`"done"`/`"killed"`/`"failed"`),
matching the `/v/jobs/N/status` text vocabulary above — not the capitalized
`Display` impl used for human-facing text (the `jobs` table). `JobInfo` also
carries `exit_code: Option<i64>` (set once the job finishes), `started_at` /
`finished_at: Option<SystemTime>` (acquired via `kaish_types::clock`, so they
work on `wasm32-unknown-unknown` too), and `pgids: Vec<u32>` — the real OS
process groups a background job spawned. `pgids` is the surface to use for
"what is this job actually doing"; `pid` is set only for a Ctrl-Z-stopped
foreground job (a TTY concept an embedder never sees) and is otherwise
`None`. For a finished job's `ExecResult` without blocking, use the
non-blocking `JobManager::try_result(id) -> Option<ExecResult>` instead of
`wait`, which parks until the job completes.

`JobManager::list`/`list_ids` return jobs sorted ascending by `JobId` (GH
#247) — job ids are minted in strictly increasing order, so this is spawn
order. Before this, both iterated the backing `HashMap` directly, so two
jobs could come back as `[2, 1]`: arbitrary, and a flake source for any
caller (an MCP surface handing an agent a job list, a snapshot test) that
depended on the order.

### Shutting down a kernel

`Kernel::shutdown(&self)` (GH #245) cancels every tracked background job
([`Kernel::cancel_all_jobs`], the same lever `kill %N` uses — an in-process
builtin future exits at its next checkpoint, an external child gets
SIGTERM→SIGKILL), then waits up to `kill_grace + 3s` **per job** for it to
actually unwind — mirroring `kill %N`'s own bound. The waits are sequential,
so the worst case is additive: N jobs that all ignore cancellation block
`shutdown` for N × (kill_grace + 3s); jobs that unwind promptly cost only
their own unwind time. A job that has not
unwound by its deadline is logged (`tracing::warn!`) and abandoned: it keeps
running detached until the tokio runtime itself goes away. Before this fix
`shutdown` called `JobManager::wait_all()` with no timeout at all — a single
`sleep 3600 &` blocked it for an hour.

`shutdown` takes `&self`, not owned `self`, specifically so an embedder
holding `Arc<Kernel>` (as `EmbeddedClient` does) can call it without
`Arc::try_unwrap` — the work only touches the shared `Arc<JobManager>`,
never kernel state that would need exclusive ownership. `EmbeddedClient::
shutdown` now calls straight through to it; it used to be a no-op with a
comment claiming the kernel's `Drop` would clean up background jobs. That
was never true: background jobs are detached `tokio::spawn` tasks holding
their own `Arc<Kernel>` **fork** ([`Kernel::fork_for_background`] mints an
independent `Arc`), not a reference back to the parent kernel, so dropping
the parent kernel neither cancels a running job nor waits for it — nor does
`Kernel` implement `Drop` at all. Call `shutdown()` explicitly before
dropping a kernel that may have backgrounded work; there is no other way to
stop it short of `kill %N` on every job.

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
- **Operations** (module `kaish_kernel::operation`): `KernelOperation` — the
  effect classes a builtin declares through `ToolSchema::with_operations`

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
