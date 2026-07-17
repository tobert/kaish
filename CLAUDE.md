# kaish

**kaish** (会sh) is a predictable shell for AI agents: an embeddable Rust library with a
reference REPL.

## Project Overview

会sh is stable and the language has settled down. There may still be some changes before 1.0
for ergonomics or correctness. The focus is the embeddable kernel/library and a reference REPL
that keeps the interactive use case honest. kaish does **not** ship its own MCP server — that
surface lives in the embedders: [kaibo](https://github.com/tobert/kaibo) (解剖) is the MCP
showcase (a read-only codebase-analysis MCP that drives kaish), and
[kaijutsu](https://github.com/tobert/kaijutsu) embeds kaish behind its own MCP interface. Both
have the same maintainer, so API changes are straightforward where they improve the projects
together.

**Philosophy**: 80% rule applied to POSIX/Bourne/bash shell. Kaish implements a `sh` subset that passes `shellcheck --enable=all`.

**Explicitly dropped features**: process substitution `<(cmd)`, backticks, `eval`, word splitting

## Crate Structure

Eagerly read the `crates/kaish-types/` crate in full.

```
crates/
├── kaish-types/      # Pure-data leaf crate: OutputData, ExecResult, Value, DirEntry, etc.
├── kaish-tool-api/   # Tool author API: Tool, ToolCtx, KernelBackend traits
├── kaish-glob/       # Glob matching and async file walking with gitignore support
├── kaish-vfs/        # Filesystem trait + LocalFs/MemoryFs/OverlayFs backends
├── kaish-help/       # Composable help & instructions content (fragments + recipes); content/en/*.md
├── kaish-kernel/     # Core: lexer, parser, interpreter, tools, VFS router, validator
├── kaish-tools-host/ # Host introspection tools (ps; behind the `host` feature)
├── kaish-client/     # Client implementations (embedded)
├── kaish-repl/       # Interactive REPL with rustyline
└── kaish-wasi/       # WASI target (wasm32-wasip1)
```

## Build Commands

```bash
cargo build                              # Build workspace
cargo build -p kaish-kernel              # Build specific crate
cargo test --all                         # Run all tests
cargo test -p kaish-kernel --test lexer_tests   # Lexer tests only
cargo test -p kaish-kernel --test parser_tests  # Parser tests only
cargo clippy --all --all-targets         # Lint everything incl. tests (must be clean)
cargo insta test                         # Run snapshot tests
cargo insta test --check                 # CI mode (fails on pending snapshots)
cargo insta review                       # Interactive review of pending snapshots
```

CI (`.github/workflows/ci.yml`) runs the gates on every PR and push to `main`:
`cargo test --all --locked`, clippy with `-D warnings`, a committed-`.snap.new`
tripwire, `cargo test -p kaish-kernel --no-default-features --locked` (see the
integration-test feature-gating convention below), and the `kaish-wasi`
wasm32-wasip1 build. When a gate changes, change ci.yml in the same PR. The runners track current
stable Rust, which may be newer than local toolchains — CI clippy can fire
lints local clippy doesn't have yet; fix the code rather than pinning the
toolchain.

The workspace denies `clippy::unwrap_used` and warns `clippy::expect_used` (see
`[workspace.lints]` in the root `Cargo.toml`) to keep production code propagating
errors. `clippy.toml` sets `allow-{unwrap,expect}-in-tests = true` so those
restriction lints don't fire on code inside `#[test]` bodies — but clippy does
**not** treat non-`#[test]` test *helper* functions, integration-test crates, or
`#[cfg(all(test, …))]` modules as test context, so add a file- or module-scoped
`#![allow(clippy::unwrap_used, clippy::expect_used)]` there (a panic on a known-good
fixture IS the test failing). `cargo clippy --all` alone skips test targets — use
`--all-targets` to catch test code too.

## Development Guidelines

### Error Handling

- Use `anyhow::Result` for fallible operations
- Avoid `unwrap()` — propagate with `?`
- Add context: `.context("what we were trying to do")`
- Never discard errors.
   - If an error can never happen in practice it can be hidden, but the program must panic on the outside case.
   - When an error is explicitly ignored, it must have a comment saying so.

### Code Style

- Comments only for non-obvious intent or complex behavior
- Avoid `mod.rs` in new modules — use `src/module_name.rs` (legacy `mod.rs` files remain; don't add more)
- Full words for names, avoid abbreviations
- Tokio for all async. Blocking in async: `tokio::task::block_in_place(|| ...)`

### Version Control

- **`main` is protected — every change lands via PR.** Branch first
  (`git switch -c <type>/<short-desc>`), push, and open a PR with `gh pr create`.
  Nothing is committed directly to `main`. **This includes releases** — the
  `/release` skill bumps the version on a `release/vX.Y.Z` branch and merges it
  via PR like any other change; only the `git tag` and `cargo publish` (neither a
  branch commit) run from `main`, after the bump PR has merged.
- **Have the PR reviewed before merging** — prefer kaibo (`consult`) for the
  review; `/code-review` on the diff or another agent/model also works. A few
  tokens on review goes a long way (this is what we ask outside contributors to do
  too; see README "Contributing").
- **PRs land as merge commits that use the PR title and body** — the convention is
  a merge commit (`gh pr merge --merge`), not squash or rebase, with the merge
  commit's subject and body kept as the PR title/body so the decisions captured in
  the PR description land in the history. A maintainer generally does the merging;
  write the PR title/body to carry the same decision-narrative the commit messages
  do (see below).
- **Always add files by name**
- Before committing, both must be clean:
  - `cargo test --all`
  - `cargo clippy --all --all-targets` — zero errors **and** zero warnings
    (`--all-targets` so test code is linted too; see Build Commands for the
    test-code allow convention)
  CI enforces these (plus the sandbox and WASI legs) on the PR — run them
  locally first anyway; the feedback loop is minutes faster.

### Commit messages

Commit and pull request bodies should usually summarize the decisions behind the
change, **drawn from the conversation with the user**. Commit messages briefly explain
what happened as context for the more important task of explaining the decisions we
made.

## Architecture

The 核 (kaku/kernel) is the unit of execution. Multiple frontends connect to the same kernel:

```
Frontends (REPL, Embedded — e.g. kaibo/kaijutsu)
    ↓ KernelClient trait
        └── EmbeddedClient (direct in-process)
    ↓
Kernel (核)
    ├── Lexer (logos)
    ├── Parser (chumsky)
    ├── Validator (pre-execution checks)
    ├── Interpreter (tokio async)
    ├── Tool Registry (builtins + user tools)
    ├── VFS Router (local, memory, overlay backends)
    └── Job Scheduler (background jobs, scatter/gather)
```

## Testing

Uses **rstest** for parameterized tests and **insta** for snapshot testing.
Tests live in `crates/kaish-kernel/tests/`. Snapshots in `crates/kaish-kernel/tests/snapshots/*.snap`.

## Documentation

- `docs/LANGUAGE.md` — complete language reference
- `docs/EMBEDDING.md` — embedder guide (kernel construction, capability
  features, ExecuteOptions, custom tools)
- `crates/kaish-help/content/en/*.md` — help system content, embedded at compile time
  via the `kaish-help` crate (repo-root `docs/help` symlinks here). Shared by the
  kernel `help` builtin, the REPL, and embedders.
- `crates/kaish-help/src/` — composition surface: `fragments.rs` (the English
  fragment registry, concept-organized) + `compose.rs` (recipes). Design:
  `docs/composable-help.md`.
- `docs/devlog.md` — a durable narrative from the agent's perspective; write your
  story there. Open work lives in GitHub Issues. **Write the devlog
  entry late — just before signoff or opening a PR**, so it carries with the PR
  and reflects the work as actually landed. Don't write it early or mid-flight:
  the decisions aren't settled yet, and an entry written ahead of the change it
  describes goes stale before it ships.

**Keep in sync:** When adding builtins or changing syntax, update the relevant help files.
The builtin list in `help builtins` is generated dynamically from tool schemas.
`syntax.md` is **generated** from the Syntax fragments in `kaish-help/src/fragments.rs` —
edit the fragments, then `cargo run -p kaish-help --example regen_syntax` (a drift test
fails if it's stale). `limits.md` and the deeper `docs/LANGUAGE.md` still need manual updates.

## Changelog

`CHANGELOG.md` follows [Keep a Changelog 1.1.0](https://keepachangelog.com/en/1.1.0/)
and [Semantic Versioning](https://semver.org). While pre-1.0, minor (`0.X.0`)
releases may carry breaking changes.

- **Every user/agent/embedder-facing change adds a bullet** under the top
  `## [Unreleased]` section, in the right group: `Added`, `Changed`,
  `Deprecated`, `Removed`, `Fixed`, `Security`. Omit empty groups. Skip pure
  internal churn (refactors with no observable effect, version bumps).
- **Mark breaking changes** by starting the bullet with `**BREAKING:**`. Anything
  that changes default features, the embedder API, language grammar, exit-code
  contracts, or `--json` shapes is breaking.
- Bullets are concise and scannable — one line each, written for someone reading
  the shell/embedding surface, not the diff.
- **At release** (the `/release` skill owns this): rename `## [Unreleased]` to
  `## [X.Y.Z] - YYYY-MM-DD`, add a fresh empty `## [Unreleased]` above it, and add
  the `[X.Y.Z]` compare link at the bottom of the file.
- **Every version bump gets a git tag** `vX.Y.Z` at the bump commit. One tag per
  released version, no gaps — the changelog and `git tag -l` must agree.

## Contributor conventions & gotchas

Hard-won rules that aren't obvious from the code. Violating these silently breaks things.

- **No legacy dual-representations.** Delete old code the moment it's superseded —
  no compatibility shims, no parallel old/new types. Fix call sites immediately.
- **Defer out-of-scope work to GitHub Issues**, not inline `TODO`s in code or
  scratch notes. New deferrals discovered *outside* an active PR go to
  `gh issue create` (GH is the transparent surface contributors see); scoped
  deferrals found *while building a PR* go in the PR body so they carry with the
  change, then to GH Issues if they outlive it. (`docs/issues.md`, the old backlog
  file, was verified entry-by-entry, migrated to GH Issues #175–#203, and deleted
  on 2026-07-16.)
- **Test builtins through `kernel.execute(...)`, not a builtin's direct
  `.execute()`.** Direct calls skip the dispatch chain (arg binding, `--json`,
  output limits) and pass while the real path is broken.
- **Read clap value flags from the parsed struct, never the raw `ToolArgs` map.**
  The kernel binds kebab-case keys (`-A`/`--after-context`); a snake_case raw-map
  read silently misses them (this was the `grep`/`rg` context-flag bug class).
- **`--json` is a kernel-level concern**, not per-tool: `extract_output_format()`
  strips it before tools run; `apply_output_format()` transforms the `ExecResult`
  after. Builtins emit typed `ExecResult::with_output(OutputData::...)` and never
  format JSON themselves (unless they opt out via `ToolSchema.owns_output`).
- **No real system paths in tests.** Use `tempfile::tempdir()` for real FS and VFS
  paths (`/v/...`) for in-memory; never hardcode `/tmp`, `/home`, `/bin`. Gate
  Linux-only tests (`/proc`, absolute `/bin/`) with `#[cfg(target_os = "linux")]`.
  Trash-related tempdirs must use `CARGO_TARGET_TMPDIR`.
- **Backends that don't override `read_range` are O(n²)** under the streaming
  readers (`wc`/`cat`/`grep`/`cmp`/`checksum` scan in 256 KiB windows). Override it
  on any new `Filesystem`/`KernelBackend` that supports byte ranges.
- **Hermetic env has two spawn sites that must stay in sync:**
  `kernel.rs::try_execute_external` (production) and
  `dispatch.rs::BackendDispatcher::try_external` (test-only). The kernel never reads
  OS env — frontends populate `KernelConfig::initial_vars`.
- **Adding a builtin (clap pattern).** Every builtin parses argv with a private
  `clap::Parser` struct inside `execute()` — copy the nearest existing builtin;
  it's the living reference. The non-obvious parts: derive the `ToolSchema` with
  `schema_from_clap` (params come from the struct; description + examples stay
  hand-written); always `#[command(flatten)] global: GlobalFlags` and call
  `parsed.global.apply(ctx)` so `--json` works; a parse failure returns
  `failure(2, ...)` (POSIX usage). **Read `Value`-typed positionals off
  `args.positional`, not the clap struct** — `to_argv()` stringifies values
  (lossy), so the clap positional field is a `#[arg(hide = true)]` sink for
  validation only. Don't add `trailing_var_arg`/`allow_hyphen_values` normally
  (`to_argv()` already emits `--` before positionals); DO add them on the
  variadic field for passthrough builtins (`timeout`/`exec`). Domain parsing
  (sed expressions, awk programs, find predicates) stays hand-rolled — clap only
  owns the argv layer.
- **clap builtin gotchas:** `with_output` drops the `rich_json` payload — use
  `with_output_and_text` when a builtin needs a custom pipe representation;
  `to_argv()` injects a `--` separator, so don't unit-test clap builtins via raw
  `positional` (route through an `execute_argv`-style entrypoint).
- **Capability features are opt-in axes** (`localfs`, `subprocess`, `host`,
  `os-integration`, `tokens`); default is `["localfs"]`. A localfs-only build does
  not spawn subprocesses. `full`/`native` are aliases for all five. New OS-touching
  code must sit behind the right axis and compile out cleanly without it (the
  `--no-default-features` gates in Build Commands enforce this).
- **Feature-gate integration-test files that need a capability, not just the
  production code.** A test file that constructs a kernel via `KernelConfig::repl()`,
  touches a real host path (`tempfile`, `common::kernel_at`), or mounts `LocalFs`
  directly needs `#![cfg(feature = "localfs")]` at the top (the `external_command_tests.rs`
  pattern: `#![cfg(feature = "subprocess")]`, or `#![cfg(all(feature = "localfs",
  feature = "subprocess"))]` when both apply). Prefer the narrowest gate that
  compiles+passes: if only a few tests in an otherwise-featureless file need it,
  gate just those `#[cfg(feature = "localfs")] #[tokio::test]` functions instead of
  the whole file (`vfs_budget_tests.rs`, `validation_tests.rs` do this). Watch for
  tests that *compile* featureless but *fail at runtime* — e.g. `Kernel::transient()`
  falls back to `KernelConfig::isolated()` (`NoLocal`, cwd `"/"`, no real
  filesystem) without `localfs`, so a test asserting cwd-isolation against a non-`/`
  starting cwd, or reading a real tempfile path, needs the same gate even though
  nothing failed to compile.


