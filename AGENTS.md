# kaish

**kaish** (会sh) is a predictable shell for AI agents: an embeddable Rust library with a
reference REPL. It is stable; changes before 1.0 are limited to ergonomics and correctness.

- **Language:** inspired by POSIX `sh` and bash, with JSON types and a safer subset.
  `[[ ]]` and `<<<` work as in bash. Dropped on purpose: process substitution `<(cmd)`,
  backticks, `eval`, word splitting.
- **Builtins** cover the common Unix text-processing tools. A hermetic build has only
  builtins and never execs an OS program.
- **No MCP server here.** [kaibo](https://github.com/tobert/kaibo) is the MCP showcase.

## Crate structure

Read `crates/kaish-types/` in full before working on code.

```
crates/
├── kaish-types/      # Pure-data leaf crate: OutputData, ExecResult, Value, DirEntry, etc.
├── kaish-tool-api/   # Tool author API: Tool, ToolCtx, KernelBackend traits
├── kaish-glob/       # Glob matching and async file walking with gitignore support
├── kaish-vfs/        # Filesystem trait + LocalFs/MemoryFs/OverlayFs backends
├── kaish-help/       # Help content (fragments + recipes); content/en/*.md
├── kaish-kernel/     # Core: lexer, parser, interpreter, tools, VFS router, validator
├── kaish-tools-host/ # Host introspection tools (ps; behind the `host` feature)
├── kaish-client/     # Client implementations (embedded)
├── kaish-repl/       # Interactive REPL with rustyline
└── kaish-wasi/       # WASI target (wasm32-wasip1)
```

## Gates

CI runs all five on every PR. Run them before pushing.

```bash
cargo test --all
cargo clippy --all --all-targets -- -D warnings
RUSTDOCFLAGS="-D warnings" cargo doc --workspace --no-deps
KAISH_ALLOW_REDUCED_TESTS=1 cargo test -p kaish-kernel --no-default-features
cargo build -p kaish-wasi --target wasm32-wasip1
```

- `cargo clippy` without `-- -D warnings` exits 0 with warnings. Read the output.
- The rustdoc gate catches broken intra-doc links that test and clippy pass. A
  bracketed link to a private item fails it; use a plain code span.
- Kernel tests behind features need them: `cargo test -p kaish-kernel --features
  subprocess,localfs`. A filtered run that reports 0 tests is not a pass.
- Snapshots: `cargo insta test --check` fails on pending snapshots.
- Give each git worktree its own `CARGO_TARGET_DIR`. Workspace crates hash by
  relative path, so a shared target dir runs another worktree's code.

## Development rules

### Error handling

- Never discard errors. An error that is impossible in practice still panics if it
  occurs. A deliberately ignored error gets a comment that says why.
- No silent fallback. A parse that yields a default on failure (`parse().unwrap_or(0)`,
  `as i64` on a float) is a bug: `printf '%d' 0xff` printing `0` is the shape to refuse.
  Return an error that names the value and the fix.
- `unwrap_used` is denied and `expect_used` warned. `clippy.toml` exempts `#[test]`
  bodies but not integration-test crates, test helper functions, or
  `#[cfg(all(test, …))]` modules; those take a file-scoped
  `#![allow(clippy::unwrap_used, clippy::expect_used)]`.

### Number rules

`docs/LANGUAGE.md`, "Arithmetic" is the contract. `$(( ))` is where kaish reads a number
in another base and does checked 64-bit integer arithmetic.

- Base spellings are bash's: `0xff` and `base#digits` (base 2 to 36). `0b101` and
  `0o17` are errors that name `2#101` and `8#17`. Add a spelling only after a model
  panel shows models writing it.
- A leading zero is text (`007`, `0644`). Where kaish needs a number — `$(( ))`,
  `[[ -eq ]]`, `test`, a loop count, a list index — it is an error naming `8#10`,
  `10`, or `10#$x`. kaish never answers octal or a third number.
- An integer literal must fit in 64 bits; overflow, division by zero, an unset
  variable, an empty `$(( ))`, and assignment inside `$(( ))` are errors, never
  wraps or zeros. A string is a value, never an expression.
- `fromjson` parses JSON and nothing else; it is also the string-to-number coercion
  (`fromjson 1e3`). `printf %x` / `%o` format the other direction; `$(random --max N)`
  replaces `$RANDOM`.

### Test a theory before building it

A claim about what models or users will write is measurable. Before adding syntax, a
spelling, or a shortcut on their behalf, hand a few cheap kaibo casts or subagents the
proposed help text and a task list, and count what they produce. Keep one syntax until
the count says otherwise. The same loop works for an error message: show the message,
ask for the next command, and see whether it lands.

### Code style

- Comments are short and direct. Narrative goes in the commit message.
- **`///` on a builtin argument is published to agents.** `params_from_clap` copies it
  into `ParamSchema.description`. Describe the flag's behavior in simple English;
  implementation notes go in `//`. See `docs/writing-style.md`, "Published builtin text".
- New modules use `src/module_name.rs`, not `mod.rs`.
- Full words for names; avoid abbreviations.
- Tokio for all async. Blocking in async: `tokio::task::block_in_place(|| ...)`.
- Tests use **rstest** for parameterized cases and **insta** for snapshots. Kernel
  tests live in `crates/kaish-kernel/tests/`.

### The embedder is in control

kaish prefers designs where the **embedder holds the state and the control flow**, and
the kernel supplies the mechanism that makes holding it correct. Apply this whenever a
new boundary between kernel and embedder is drawn. When the kernel cannot answer
immediately, **return some data and let the embedder come back**. No callbacks or
awaits on embedder code.

## Version control

- **`main` is protected; every change lands via PR.** Agents may open PRs. Review
  before pushing: kaibo (`consult`) with a different model family, or a different
  model tier.
- **Merging is a human decision, and an agent asks every time.** Default: open a PR,
  address review, report it ready, then stop.
- **PRs land as merge commits** (`gh pr merge --merge`) whose subject and body are the
  PR title and body, so the PR text becomes history. Write it like a commit message.
- **PR bodies:** one long line per paragraph (GitHub re-wraps merge commits at 72);
  every example in a ``` fence (indented blocks get re-wrapped); no `##` headings or
  tables (use a short capitalized line).
- **Add files by name:** `git add <file>`. Never `git add -A` or `git add .`.

### Commit messages

Commit and PR bodies summarize the decisions behind the change, **drawn from the
conversation with the user**. A useful message reminds us how we got to the code; the
code speaks for itself.

Write a clinical engineering narrative: the problem, the evidence, the decision, the
rule now in force. Design stories, plans, review transcripts, and model-panel results
live in agent memory outside the repo; a commit never references them.

## Documentation

- `docs/LANGUAGE.md` — complete language reference.
- `docs/EMBEDDING.md` — embedder guide: kernel construction, capability features,
  `ExecuteOptions`, custom tools.
- `docs/writing-style.md` — the writing style for all published text. Read it before
  writing help, docs, errors, `///` builtin docs, CHANGELOG entries, or PR bodies.
- `crates/kaish-help/content/en/*.md` — help content, embedded at compile time
  (`docs/help` symlinks here). `crates/kaish-help/src/fragments.rs` holds the fragment
  registry; `compose.rs` the recipes; design in `docs/composable-help.md`.

**Keep in sync:** when adding builtins or changing syntax, update the help files.
`help builtins` is generated from tool schemas. `syntax.md` is **generated** from the
Syntax fragments: edit `fragments.rs`, then run
`cargo run -p kaish-help --example regen_syntax` (a drift test fails if it is stale).
`limits.md` and `docs/LANGUAGE.md` are updated by hand.

## Writing style

`docs/writing-style.md` is the full guide; its Terms table is the source for terms with
a behavioral guarantee. The rules that matter most:

- Keep a small vocabulary of plain words. One term per concept; don't vary words for
  style. American spelling.
- Give specific values: exit code, size, flag, default, condition. "Oversize output
  spills to a file and exits 3," not "Oversize output fails."
- Errors are feedback: name the value, the rule in a few words, and the fix. Refuse
  rather than degrade quietly. Don't leak internal names.
- The example is the rule: show the correct example first. An incorrect example comes
  second and is clearly marked.
- Lead with the most important information; the context may be truncated.

## Changelog

`CHANGELOG.md` follows [Keep a Changelog 1.1.0](https://keepachangelog.com/en/1.1.0/)
and [Semantic Versioning](https://semver.org). While pre-1.0, minor (`0.X.0`) releases
may carry breaking changes, marked **BREAKING**. Keep each bullet to 25–40 words.
