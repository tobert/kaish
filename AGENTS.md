# kaish

**kaish** is a predictable shell for AI agents as an embeddable Rust library with a reference REPL.

## Project overview

kaish (会sh) is stable. Changes before 1.0 are limited to ergonomics and correctness. The focus is
the embeddable kernel/library and a reference REPL that tests the kernel against interactive
use. kaish does **not** ship its own MCP server. [kaibo](https://github.com/tobert/kaibo) is the
MCP showcase (models and subagents with kaish powers).

**Philosophy**: 80% rule applied to POSIX/Bourne/bash shell. kaish is *inspired by POSIX
`sh` and bash* but makes some tradeoffs to adopt JSON types and offer a safer subset of
its ancestors. `[[ ]]` and `<<<` are just like bash. kaish also includes builtins for the
most common Unix/Linux command line utilities for text processing. It can be built to run
hermetically, with only builtins available to callers, and no exec to the operating system
at all.

**Explicitly dropped features**: process substitution `<(cmd)`, backticks, `eval`, word splitting

## Crate structure

Read `crates/kaish-types/` in full before working on code.

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

## Build commands

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

## Development guidelines

### Error handling

- Never discard errors.
   - If an error is impossible in practice, the program must still panic if it occurs.
   - When an error is deliberately ignored, a comment must say why.

### Code style

- Comments should be short and direct. Comments are not a space for narratives;
  that goes in the commit messages.
- **`///` on a builtin argument is published to agents** — `params_from_clap` copies it
  into `ParamSchema.description` and the kernel ships it to the model. Describe the
  flag's behavior there succinctly, using simple English. Implementation notes go in `//` comments.
- Avoid `mod.rs` in new modules — use `src/module_name.rs`.
- Full words for names, avoid abbreviations.
- Tokio for all async. Blocking in async: `tokio::task::block_in_place(|| ...)`.

### Version control

- **`main` is protected — every change lands via PR.** Agents may create PRs,
  but they **must** be reviewed by a human before merging.
- **Do code review before pushing** — kaibo (`consult`) with a different model
  family is recommended, or use a different model tier via builtin review tools.
- **Merging to `main` is a human decision, and an agent asks every time.** Default:
  open a PR, review it, address the review, report it ready — then stop.
  A human will read the PR and merge or ask for it to be merged.
- **PRs land as merge commits that use the PR title and body** — the convention is
  a merge commit (`gh pr merge --merge`), not squash or rebase, with the merge
  commit's subject and body kept as the PR title/body so the decisions captured in
  the PR description land in the history. A maintainer generally does the merging;
  write the PR title/body to carry the same decision-narrative the commit messages
  do (see below).
- **Do not wrap PR body prose — write one long line per paragraph.** GitHub
  re-wraps the merge commit body at exactly 72 characters.
- **Put every example in a fenced code block in a PR body.** A \`\`\` fence is
  exempt from re-wrap; a four-space-indented block is not. `##` headings do not
  survive as plain text at all — use a short capitalized line or a paragraph
  break instead.
- **Add files by name**: `git add <file>`. Never use `git add -A` or `git add .`
- Before committing, both must be clean:
  - `cargo test --all`
  - `cargo clippy --all --all-targets` — zero errors **and** zero warnings
    (`--all-targets` so test code is linted too — see the note below)
  CI enforces these (plus the sandbox and WASI legs) on the PR — run them
  locally first anyway; the feedback loop is minutes faster.

`unwrap_used` is denied and `expect_used` warned workspace-wide, so the lints
carry that rule and this guide does not repeat it. What the lints cannot tell
you is the way out: `clippy.toml` exempts `#[test]` bodies, but **not**
integration-test crates, non-`#[test]` helper functions, or
`#[cfg(all(test, …))]` modules. Those need a file-scoped
`#![allow(clippy::unwrap_used, clippy::expect_used)]` — a panic on a known-good
fixture IS the test failing. 137 of 158 test files carry it.

### Commit messages

Commit and pull request bodies should usually summarize the decisions behind the
change, **drawn from the conversation with the user**. The commit message is where
the narrative of agent and user can be persisted. A useful commit message will
remind us how we got to the code it contains. The code can speak for itself.

## Architecture

The kernel is the unit of execution. Multiple frontends can connect to the same kernel:

```
Frontends (REPL, Embedded — e.g. kaibo/kaijutsu)
    ↓ KernelClient trait
        └── EmbeddedClient (direct in-process)
    ↓
Kernel
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
embedder is being drawn. When the kernel cannot answer immediately, **return some data
and let the embedder come back**. No callbacks or awaits on embedder code.

## Testing

Tests use **rstest** for parameterized cases and **insta** for snapshot testing.
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
guide keeps a small, predictable subset of English for the same reason.

### Vocabulary choices

Keep the vocabulary small. This limits the number of distinct words, not the length of the
text — familiar words may require a longer sentence.

Use plain words instead of figures of speech. Make the intended meaning available from the
words themselves, including in second-language or partial-context use.

Use an established technical term when kaish gives it one meaning. For example:

| Write | Meaning |
|---|---|
| affordance | A visible cue for the next available action. An error that names its fix affords that fix. |
| familiar syntax | Existing `sh` skill transfers because kaish preserves familiar syntax. |

`hazard` and `override` belong to this vocabulary too; they carry guarantees, so their
definitions live in the Terms table below, with every other term that carries a
behavioral guarantee.

Use American spelling to match the corpus: `modeled`, not `modelled`.

### One term, one meaning

Pick one word for each concept and keep it. Do not vary a word for style.

`dialect` is reserved for a ShellCheck language mode or a regex flavor. Do not use it
about prose.

`surface` can hide the thing it names. In published text, name the tool schema, error
message, help topic, or API.

Example labels are imperative. Write "Send STOP by name," not "Named shorthand." The
label sits next to a command, so it should read like one.

Cross-references take one form per target: ``see `help <topic>` `` for a help topic, and
`docs/LANGUAGE.md`, "Section name" for the language reference. Link instead of
re-explaining.

This section keeps one term for each concept because one term, one meaning applies to the
guide itself.

### Provide specific values

Whenever it's practical, provide the public exit code, size, flag, default, and condition.
This saves round trips to get more information and gives agents clear observations for
updating their model of the world.

> Before: Oversize output fails.
>
> After: Oversize output spills to a file and exits 3.

State the default and condition too, for example: "reads stdin when no files are given"
and "off by default; applies to `-r` only."

### Fast and informative failures

Make errors, warnings, and failures informative and, where possible, instructive.
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

A blank `///` line splits clap short help from long help. Everything before the blank
line is published; everything after it is not. Use the split when an implementation note
belongs next to the field.

Do not infer the published text by grepping the source. Read `Kernel::tool_schemas()` or
run the published-prose test. When modifying builtins, audit every `///` on its clap
struct to ensure code and documentation stay synchronized.

### Write for model context

Use the same prose in human and model contexts. Assume the context may be truncated and
lead with the most important information. Teach syntax with examples. Repeat important
rules.

### The example is the rule

Show the correct example before explaining it. Continue the correct pattern when the
surrounding prose is missing. Make the example carry the rule by itself.

> Before: **Quote to join.** `$VAR`, `$(cmd)`, and globs are each a separate word unless
> quoted — kaish never pastes adjacent unquoted tokens.
>
> After: `"$dir/file.txt"` — one path. kaish keeps `$VAR`, `$(cmd)`, and globs as
> separate words; quote the whole word to join text with interpolation.

Avoid incorrect examples. When one is necessary, put the correct form first and the
clearly marked error next to it:
`echo "$dir/file.txt"`; `echo $dir/file.txt # error — quote the whole path`.

### Terms

These are the terms that carry a stable definition. **This table is the source.**
The list grows when a collision appears in real prose, not in advance.

| Term | Part of speech | Meaning |
|---|---|---|
| hazard | noun | A condition with a predictable failure. Prose names the hazard and the fix kaish ships for it; neither leads. |
| override | noun | A documented, supported way past a restriction kaish enforces — `-E` out of the BRE superset, `--lines` out of JSONL rows. An override is designed and documented intentionally. |
| fail loudly | adjective, verb phrase | An error is explicit and immediate. kaish never continues on a wrong assumption. |
| builtin | noun | An embedded Unix-like tool that runs inside the kernel process. |
| external command | noun | A program the kernel runs on the underlying system via execve(2) family, often via `$PATH`. |
| kernel | noun | The kaish execution core. Not the OS kernel. |
| mount | noun, verb | A path prefix bound to a filesystem, or the act of binding one. |
| typed | adjective | A value keeps its JSON type through substitution. It is not stringified. |
| overlay | noun, adjective | Copy-on-write mode. Writes land in a virtual upper layer until committed. |
| spill | verb, noun | To write oversize output to a file, or the file that results. |

## Changelog

`CHANGELOG.md` follows [Keep a Changelog 1.1.0](https://keepachangelog.com/en/1.1.0/)
and [Semantic Versioning](https://semver.org). While pre-1.0, minor (`0.X.0`)
releases may carry breaking changes. Try to keep them short.

