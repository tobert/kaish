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
- **Merging to `main` is Amy's word, and an agent asks every time.** Default:
  open the PR, get it reviewed, address the review, report it ready — then stop.
  Amy reads the PR herself; that reading is the point of the gate, and a green
  review is evidence for her decision, not a substitute for it. She may hand out
  a merge word for a named piece of work ("fix x, y, z and merge when the kaibo
  review is done and addressed") — that word is **ephemeral**: it covers the PRs
  she named, and it expires with them. Never carry it to the next PR, and never
  infer one from a clean review, a green CI, or a clearance given before.
  Irreversible and outward-facing steps — `git tag` pushes, `cargo publish`,
  release pages, anything posted to a repo we don't own — each need their own
  word; a merge word does not cover them.
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
not a rule about one subsystem — apply it whenever a new boundary between kernel and
embedder is being drawn.

The test to apply at a boundary: does the kernel *ask* the embedder for an answer, or does it
*run* the embedder's work? Asking is a pure function on the request path — the kernel keeps
control, and the answer is data. Running means the kernel owns a task, a clock, and a
cancellation policy on the embedder's behalf, and those are three decisions per deployment
that no default gets right. When the answer cannot be immediate, **return the question as
data and let the embedder come back**, rather than awaiting a callback the kernel then has
to bound.

Three things follow:

- **The kernel never waits on the embedder.** A bounded wait is a clock-driven decision; an
  unbounded one is a liveness hazard the kernel cannot cancel correctly. `plan_program` is
  the shape to copy: it returns every statement's `Plan` as data, and the embedder decides
  on its own time whether to execute.
- **The kernel keeps what must be correct under concurrency**, and only that: the job
  table, the trash contract, the output limits. Inverting *those* would make every
  embedder re-implement the hard part.
- **Helpers compose above the boundary, never inside it.** A reusable waiter, a pending queue,
  a retry policy — write them as composable pieces in the REPL or a util crate that is
  itself an embedder. A convenience that reads a clock or parks a decision inside the
  kernel has moved policy back in through the back door.

The payoff is the point: an embedder that owns the state can do things with it we will not
think of. kaijutsu parks a decision in its own UI, kaibo can put a plan in front of a
different model, someone else queues it for a shift change. None of those shapes need a
kernel change, because the kernel never assumed which one it was serving.

The approval ledger is the receipt: kaish once held approval state and decision flow
inside the kernel, and it was removed before 0.14.0 because every embedder already had
its own — see `docs/EMBEDDING.md`, "Why this, and not a gate".

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

**Keep in sync:** When adding builtins or changing syntax, update the relevant help files.
The builtin list in `help builtins` is generated dynamically from tool schemas.
`syntax.md` is **generated** from the Syntax fragments in `kaish-help/src/fragments.rs` —
edit the fragments, then `cargo run -p kaish-help --example regen_syntax` (a drift test
fails if it's stale). `limits.md` and the deeper `docs/LANGUAGE.md` still need manual updates.

## Writing style

kaish keeps a small, predictable subset of `sh`, so existing shell skills transfer. This
guide keeps a small, predictable subset of English for the same reason. This is meant to
be read before editing prose, comments, and documentation. This guide is loosely based
on Standard Technical English and uses similar prescriptions.

### Vocabulary Choices

Keep the vocabulary small. This limits the number of distinct words, not the length of the
text — familiar words may require a longer sentence.

Use plain words instead of figures of speech. Make the intended meaning available from the
words themselves, including in second-language or partial-context use.

Use an established technical term when kaish gives it one meaning.

| Write | Meaning |
|---|---|
| hazard | A condition with a predictable failure. Name the condition and the fix kaish provides. |
| override | A documented way past a restriction. An override is part of the design, not a workaround; every restriction that has one names it. |
| affordance | A visible cue for the next available action. An error that names its fix affords that fix. |
| familiar syntax | Existing `sh` skill transfers because kaish preserves familiar syntax. |

This table uses the terms it defines: a missing fix is a hazard; a documented way past a
restriction is an override; a visible next action is an affordance. Use the terms this way
until they become ordinary kaish vocabulary.

Terms that carry a behavioral guarantee live in the table in `AGENTS.md`.

Use the public word instead of a tool's private term. For example, `dhat` calls
an allocation a "block"; write "18% fewer allocations," not "18% fewer blocks."

Use American spelling to match the corpus: `modeled`, not `modelled`.

### One term, one meaning

Pick one word for each concept and keep it. Do not vary a word for style.

`dialect` is reserved for a ShellCheck language mode or a regex flavor. Do not use it
about prose.

`surface` can hide the thing it names. In published text, name the tool schema, error
message, help topic, or API.

Write `boundary`, not `seam`. Use a boundary to separate available actions from mechanism
that does not affect those actions.

Example labels are imperative. Write "Send STOP by name," not "Named shorthand." The
label sits next to a command, so it should read like one.

Cross-references take one form: ``see `help <topic>` `` for a help topic, and
`docs/LANGUAGE.md`, "Section name" for the language reference. Link instead of
re-explaining.

This section keeps one term for each concept because one term, one meaning applies to the
guide itself.

### Provide Specific Values

Whenever it's practical, provide the public exit code, size, flag, default, and condition.
This saves round trips to get more information and gives agents clear observations for
updating their model of the world.

> Before: Oversize output fails.
>
> After: Oversize output spills to a file and exits 3.

State the default and condition too, for example: "reads stdin when no files are given"
and "off by default; applies to `-r` only."

### Fast & Informative Failures

Errors, warnings, and failures should be informative and, where possible, instructive.
Lead with consequences, name conditions, and suggest next steps when they are known.

Errors that face users, agents, and models must not leak internals. Internal code names
and references will be unresolvable and should only be exposed for assertions and errors
that indicate a real problem in kaish.

### Published builtin text

A `///` comment on a builtin argument is published to agents. `params_from_clap` copies
it into `ParamSchema.description`, and the kernel exposes it through
`Kernel::tool_schemas()`. Describe the argument's behavior there. Put implementation
notes in `//` comments.

A `///` comment on the clap struct is not published; `schema_from_clap` reads
`cmd.get_about()` instead. Struct docs and `//` comments are safe places for mechanism.

> Before: `/// Unset a variable (-u VAR). Repeatable: -u A -u B. Clap sees a single`
> `/// occurrence via to_argv() ... This field is a validation sink only.`
>
> After: `/// Unset a variable (-u VAR). Repeatable: -u A -u B.`

A blank `///` line also splits clap short help from long help. Everything before the blank
line is published; everything after it is not. Use the split when an implementation note
belongs next to the field.

Do not infer the published text by grepping the source. Read `Kernel::tool_schemas()` or
run the published-prose test. When you touch a builtin, audit every `///` on its clap
struct — the visit supplies the context needed to judge each line.

### Write for model context

Use the same prose in human and model contexts. Assume the context may be truncated. Teach
syntax with examples. Repeat a rule in its error. These instructions strengthen the
weights above; they do not replace them.

### The example is the rule

Show the correct example before explaining it. Continue the correct pattern when the
surrounding prose is missing. Make the example carry the rule by itself.

> Before: **Quote to join.** `$VAR`, `$(cmd)`, and globs are each a separate word unless
> quoted — kaish never pastes adjacent unquoted tokens.
>
> After: `"$dir/file.txt"` — one path. kaish keeps `$VAR`, `$(cmd)`, and globs as
> separate words; quote the whole word to join text with interpolation.

Avoid using incorrect examples. When it does happen, put the correct form first and
the clearly marked error next to it:
`echo "$dir/file.txt"`; `echo $dir/file.txt # error — quote the whole path`.

### Terms

Terms that carry a guarantee. **This table is the source**; `README.md` mirrors it for
readers and must be kept in step. The list grows when we find a collision, not in advance —
every entry below was verified to be in real use in the governed prose.

| Term | Part of speech | Meaning |
|---|---|---|
| fail loudly | adjective, verb phrase | An error is explicit and immediate. kaish never continues on a wrong assumption. |
| silently | adverb | Used only in the negative, to name behavior kaish refuses. |
| builtin | noun | A tool that runs inside the kernel process. |
| external command | noun | A program the kernel runs through `PATH`. |
| kernel | noun | The execution core. Not the OS kernel. |
| mount | noun, verb | A path prefix bound to a filesystem, or the act of binding one. |
| typed | adjective | A value keeps its JSON type through substitution. It is not stringified. |
| overlay | noun, adjective | Copy-on-write mode. Writes land in a virtual upper layer until committed. |
| trash | noun, verb | Recoverable deletion under `set -o trash`. A trash failure is an error, never a permanent delete. |
| spill | verb, noun | To write oversize output to a file, or the file that results. |
| hazard | noun | A condition with a predictable failure. Prose names the hazard and the fix kaish ships for it; neither leads. |
| override | noun | A documented, supported way past a restriction kaish enforces — `-E` out of the BRE superset, `--lines` out of JSONL rows. Never a workaround: an override is part of the design, and every restriction that has one names it. |

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
  contracts, or `--json` shapes is breaking — **when the surface it changes shipped
  in a release**. The marker means "a user or embedder of the last released version
  must act on upgrade." Surface that never shipped carries no marker and gets no
  removal bullet: edit its bullets in place to describe the final state; the
  mid-cycle path lives in `git log`.
- Bullets are concise and scannable — one line each, written for someone reading
  the shell/embedding surface, not the diff.
- **At release** (the `/release` skill owns this): rename `## [Unreleased]` to
  `## [X.Y.Z] - YYYY-MM-DD`, add a fresh empty `## [Unreleased]` above it, and add
  the `[X.Y.Z]` compare link at the bottom of the file.
- **Every version bump gets a git tag** `vX.Y.Z` at the bump commit. One tag per
  released version, no gaps — the changelog and `git tag -l` must agree.

