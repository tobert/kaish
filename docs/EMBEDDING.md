# Embedding kaish

This guide shows how to embed the kaish kernel in your application: kernel
construction, capability features, per-call execution options, custom tools,
and output capture.

## Stability

kaish is pre-1.0 (currently 0.8.x, MSRV 1.85). The language has settled;
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

#### Destructive-op rails: reading the latch nonce

With `.with_latch(true)`, a destructive op (`rm`'s delete, and the truncating
overwrite behind `tee` / `patch` / `sed -i` / `write` / `cp` / `mv` / `dd of=`)
does not run on first call — it returns an `ExecResult` with **exit code 2** and a
confirmation nonce. The re-run is the same argv plus `--confirm=<nonce>` (`dd` uses
its `confirm=<nonce>` key=value idiom). Copying or moving *into* a directory, and
recursive `cp -r`/`mv` of a tree, gate only the named destination, not per-child
overwrites. The output contract:

- **`ExecResult.err`** (which a frontend routes to stderr) carries the
  human-readable prompt;
- **stdout** is empty (nothing happened, so there is no success output);
- **`ExecResult.data`** carries the nonce as structured JSON — read it here
  rather than parsing the `err` text:

  ```json
  { "nonce": "a3f7b2c1", "command": "rm",
    "paths": ["important.dat"], "hint": "...", "ttl": 60 }
  ```

  From Rust, prefer the typed accessor over reaching into `.data` by key:

  ```rust
  // Returns Some(LatchRequest { nonce, command, paths, hint, ttl }) only for a
  // latch gate (exit 2 + nonce payload); None for a plain usage error.
  if let Some(req) = result.latch_request() {
      // apply preapproval policy / model review over (req.command, req.paths) …
      // approve → re-run the same argv with `--confirm=<req.nonce>`.
  }
  ```

  This is the seam to hook embedder-side policy: check a preapproval allowlist or
  ask a model to review the resolved `(command, paths)` before re-running. The
  kernel owns the *mechanism* (issuing/validating the path- and command-scoped
  nonce); the embedder owns the *judgment*. Call it on the raw result, before any
  `--json` formatting.

  If you executed with `--json` (`OutputFormat::Json`), this is a non-zero exit
  with a diagnostic, so the result is wrapped in the standard JSON error envelope
  and the nonce payload is nested one level down, under `data`:
  `{ "error": "...", "code": 2, "data": { "nonce": ... } }`. Reading
  `ExecResult.data` from the struct without `--json` gives you the bare payload.

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
- **`cwd`** — per-call working directory override.
- **`stdin`** — standard input for this call as a ready `String` buffer,
  consumed by the first top-level command that reads stdin (shell draining
  semantics — a later reader sees nothing). Lets an embedder feed piped input,
  e.g. `printf '…' | kaish -c 'sort'`. A redirect (`< file`/heredoc) on the
  command still takes precedence. Eager: the whole buffer must exist before the
  call. For a **lazy or binary** stream — fed only if a command reads stdin,
  byte-clean — use `Kernel::execute_with_pipe_stdin(_streaming)` with a
  `PipeReader` instead (this is how the non-interactive `kaish` CLI forwards its
  open process stdin without blocking a command that never reads it, e.g.
  `sleep 10 | kaish -c 'echo hi'`).
- **`traceparent` / `tracestate` / `baggage`** — W3C trace context;
  kaish's execution span parents onto your trace, and baggage merges back
  out through `ExecResult.baggage`.

## Argv-Native Execution: `execute_argv`

`Kernel::execute(&str)` is string-native — it lexes and parses its input. If your
embedder already holds **tokenized** arguments (a structured tool call, a
multicall-style frontend), re-quoting them into a string just to have the lexer
split them apart again is wasteful and **lossy**: `to_argv()` stringifies typed
values, so a `Value::Bytes` blob or a `Value::Json` record can't survive the
round-trip. `execute_argv` is the peer door that skips it:

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
  all apply, so a latched `rm` still returns exit 2 with a nonce in
  `ExecResult.data` (see [Destructive-op rails](#destructive-op-rails-reading-the-latch-nonce)).
  The kernel's pre-execution *syntax* validator does not run — argv carries no
  shell syntax — but a tool's own `validate()`/clap parse still does.
- **Typed-passthrough caveat.** Because builtins re-parse their own `to_argv()`
  internally (the two-layer clap model), the un-stringified-value win fully lands
  only for tools that read `args.positional` directly (the documented pattern),
  not those that trust their clap struct after a `to_argv()` round-trip.

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
│   ├── status    # "running", "done:0", or "failed:N"
│   └── command   # Original command string
├── 2/
│   └── ...
```

```bash
# In kaish scripts
sleep 10 &              # Starts job 1
jobs                    # Shows: [1] running  /v/jobs/1/
cat /v/jobs/1/status    # "running"

# After completion
cat /v/jobs/1/stdout    # Job's stdout
cat /v/jobs/1/status    # "done:0" on success, "failed:N" otherwise
```

The status strings are exactly `running`, `done:0`, and `failed:{code}` —
match on those, not on `completed`.

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
