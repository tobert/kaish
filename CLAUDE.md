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

**Philosophy**: 80% rule applied to POSIX/Bourne/bash shell. kaish is *inspired by POSIX
`sh` and bash, informed by ShellCheck's lints* — not a dialect ShellCheck can validate:
`[[ ]]` and `<<<` are bash, and typed data, structured `$()`, and collections are modeled
by no ShellCheck dialect at all. **ShellCheck reports nothing about kaish's extensions.
The kaish validator is the only checker that sees them.**

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
RUSTDOCFLAGS="-D warnings" cargo doc --workspace --no-deps   # Rustdoc gate; broken intra-doc links fail CI
cargo insta test                         # Run snapshot tests
cargo insta test --check                 # CI mode (fails on pending snapshots)
cargo insta review                       # Interactive review of pending snapshots
```

CI (`.github/workflows/ci.yml`) runs the gates on every PR and push to `main`:
`cargo test --all --locked`, clippy with `-D warnings`, rustdoc with
`RUSTDOCFLAGS="-D warnings"` (a broken intra-doc link fails the PR — run the
doc gate locally too; it has cost two PRs a full CI round trip each), a
committed-`.snap.new` tripwire, `cargo test -p kaish-kernel
--no-default-features --locked` (see the integration-test feature-gating
convention below), and the `kaish-wasi` wasm32-wasip1 build. When a gate changes, change ci.yml in the same PR. The runners track current
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
- **`///` on a builtin argument is published to agents** — `params_from_clap` copies it
  into `ParamSchema.description` and the kernel ships it to the model. Describe the
  flag's behavior there; implementation notes go in `//` comments.
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

### The embedder is in control

kaish prefers designs where the **embedder holds the state and the control flow**, and the
kernel supplies the mechanism that makes holding it correct. This is a design preference,
not a rule about one subsystem — reach for it whenever a new seam is being drawn.

The test to apply at a seam: does the kernel *ask* the embedder for an answer, or does it
*run* the embedder's work? Asking is a pure function on the request path — the kernel keeps
control, and the answer is data. Running means the kernel owns a task, a clock, and a
cancellation policy on the embedder's behalf, and those are three decisions per deployment
that no default gets right. When the answer cannot be immediate, **return the question as
data and let the embedder come back**, rather than awaiting a callback the kernel then has
to bound.

Three things follow, and each has a worked example in `docs/approval-ledger.md`:

- **The kernel never waits on the embedder.** A bounded wait is a clock-driven decision; an
  unbounded one is a liveness hazard the kernel cannot cancel correctly. The approval ledger
  returns `ApprovalOutcome::Pending` with a structured `ResumeAction` instead (§C.1, §C.2).
- **The kernel keeps what must be correct under concurrency**, and only that: the
  append-only record, the state machine, the balance rule, the types that make a bypass
  unrepresentable. Inverting *those* would make every embedder re-implement the hard part
  (`docs/approval-ledger.md` §0.1).
- **Helpers compose above the seam, never inside it.** A reusable waiter, a pending queue,
  a retry policy — write them as composable pieces on top of the traits and the record, in
  the REPL or a util crate that is itself an embedder. A convenience that reads a clock or
  parks a decision inside the kernel has moved policy back in through the back door.

The payoff is the point: an embedder that owns the state can do things with it we will not
think of. kaijutsu parks a decision in a UI, kaibo puts it in front of a different model,
someone else queues it for a shift change. None of those shapes need a kernel change,
because the kernel never assumed which one it was serving.

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

## Writing style

kaish keeps a small, predictable subset of `sh`, chosen so muscle memory transfers. Our
prose keeps a small, predictable subset of English, chosen for the same reason. Full
guide: `docs/style.md`. **These are weights, not gates** — there is no linter. Groom the
text you touch; we are not scheduling a rewrite.

- **Subset, not slang** — keep the vocabulary small; this constrains distinct words, not
  length. Avoid metaphors that name a mental act as a physical one ("reach for"). `muscle
  memory`, `footgun`, and `escape hatch` are load-bearing and stay. The list grows only on
  evidence — a candidate must already be in consistent use across the corpus with one
  meaning, never on the argument that it would read well. Prefer the reader's word over a
  tool's private one ("allocations", not dhat's "blocks"). American spelling.
- **One term, one meaning** — a synonym reads as a new concept. Terms that carry a
  guarantee go in the table below. Example labels are imperative. Cross-references take one
  form: ``see `help <topic>` ``, or `docs/LANGUAGE.md`, "Section name".
- **State the number** — exact exit code, size, flag, default, condition. "Spills to a file
  and exits 3", never "fails". Agents act on our numbers.
- **Fail loud** — constraint and consequence at the front of the sentence, no hedging. The
  first sentence must work alone; readers skim and the onboarding spine truncates at 3500
  chars.
- **Keep the why** — `<rule> — <why>`; the clause after the dash is load-bearing. Split a
  tangled sentence rather than dropping the rationale, and never invent a rationale the
  source does not record. There is no word budget. Tables carry the same weights and get
  longer, which is the correct trade.
- **Do not leak the kernel** — the test is whether the reader needs the internal to predict
  behavior, not whether the sentence names one. When you touch a builtin, audit every `///`
  on its clap struct (see Code Style).

Full weight applies to help content, `fragments.rs` bodies, builtin schema text
(`description`, `about`, example labels, `///` argument docs), and **every error and
diagnostic string a builtin or the kernel returns** — an agent reads a failure message
more often than any help topic. Partial weight to `LANGUAGE.md`/`EMBEDDING.md`/`NAMING.md`
and `///` rustdoc on `pub` items. Terms only to `README.md` and the design docs.
`devlog.md`, `signoff.md`, and `designing-syntax-with-llms.md` are exempt — they tell a
story and need a voice. kaibo and kaish-extras adopt this by reference as they evolve;
kaijutsu is exempt.

`CHANGELOG.md` is the one place "Keep the why" does not win: one line per bullet, carrying
the rule and one clause of rationale. The narrative goes in the PR body, which becomes the
merge commit. Three numbers and three reasons means three bullets.

### Terms

Terms that carry a guarantee. **This table is the source**; `README.md` mirrors it for
readers and must be kept in step. The list grows when we find a collision, not in advance —
every entry below was verified to be in real use in the governed prose.

| Term | Part of speech | Meaning |
|---|---|---|
| loud, fail loud | adjective, verb phrase | An error is explicit and immediate. kaish never continues on a wrong assumption. |
| silently | adverb | Used only in the negative, to name behavior kaish refuses. |
| builtin | noun | A tool that runs inside the kernel process. |
| external command | noun | A program the kernel runs through `PATH`. |
| kernel | noun | The execution core. Not the OS kernel. |
| mount | noun, verb | A path prefix bound to a filesystem, or the act of binding one. |
| typed | adjective | A value keeps its JSON type through substitution. It is not stringified. |
| overlay | noun, adjective | Copy-on-write mode. Writes land in a virtual upper layer until committed. |
| trash | noun, verb | Recoverable deletion under `set -o trash`. A trash failure is an error, never a permanent delete. |
| nonce | noun | The confirmation token a latch-gated operation requires. |
| spill | verb, noun | To write oversize output to a file, or the file that results. |
| latch | noun, verb | The confirmation hold that a destructive operation waits on. |
| escape hatch | noun | A documented, supported way past a restriction kaish enforces — `-E` out of the BRE superset, `--lines` out of JSONL rows, single quotes out of expansion. Never a workaround: an escape hatch is part of the design, and every restriction that has one names it. |

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
- **Prefer a small PR over an issue, and ask before filing one.** When the work is
  straightforward, do it — a small focused PR beats an issue describing the same
  thing. Pick the lightest artifact that keeps the thought:
  - **Straightforward?** Fix it in a small PR.
  - **Minor, and you don't want to lose it?** An inline `TODO` is fine. We sweep
    them, and a `TODO` rides in the diff where review can call it out — either
    "just fix that now" or "that's a follow-up". A comment nobody reads is the
    failure mode; one sitting in a diff under review is not.
  - **Out of scope for the PR you're in?** The PR body, so it travels with the
    change. A merged PR is a sufficient record and the merge commit keeps it
    searchable.
  - **Real backlog that outlives the PR?** A GitHub issue — but **agents check
    with the user before opening one.** Unilateral filing produces granular churn
    that costs more attention than it saves.
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


