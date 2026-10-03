# kaish

**kaish** (会sh) is a predictable shell for AI agents: an embeddable Rust library with a
reference REPL. It is stable; changes before 1.0 are limited to ergonomics and correctness.

- **Language:** inspired by POSIX `sh` and bash, with JSON types and a safer subset.
  `[[ ]]` and `<<<` work as in bash. Dropped on purpose: process substitution `<(cmd)`,
  backticks, `eval`, word splitting.
- **Builtins** cover the common Unix text-processing tools. A hermetic build has only
  builtins and never execs an OS program.
- **No MCP server here.** [kaibo](https://github.com/tobert/kaibo) is the MCP showcase.

## Writing style

Write this way by default: commit messages, PR bodies, code comments, help, docs,
error messages, and `///` on builtin arguments.

kaish keeps a small, predictable subset of `sh`, so existing shell skills transfer. This
guide keeps a small, predictable subset of English for the same reason.

### Vocabulary choices

Keep the vocabulary small. This limits the number of distinct words, not the length of the
text. Using familiar words may require a longer sentence.

Use plain words instead of figures of speech. Make the intended meaning available from the
words themselves, including in second-language or partial-context use.

Use an established technical term when kaish gives it one meaning. For example:

| Write | Meaning |
|---|---|
| affordance | A cue for the next available action. An error that names its fix affords that fix. |
| familiar syntax | Existing `sh` skill transfers because kaish preserves familiar syntax. |

`hazard` and `override` belong to this vocabulary too; they carry guarantees, so their
definitions live in the Terms table below, with every other term that carries a
behavioral guarantee.

Use American spellings.

### One term, one meaning

Pick one word for each concept and keep it. Do not vary a word for style.

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

An error is feedback, not a problem. A model that reads `` write `10#$m` `` gets it right
next turn; a silent coercion teaches nothing. When the choice is between degrading quietly
and refusing with the fix named, refuse. Keep the text clinical: the value, the rule in
a few words, the fix.

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
| override | noun | A documented, supported way past a restriction kaish enforces — `-E` out of GNU BRE, `--lines` out of JSONL rows. An override is designed and documented intentionally. |
| fail loudly | adjective, verb phrase | An error is explicit and immediate. kaish never continues on a wrong assumption. |
| builtin | noun | An embedded Unix-like tool that runs inside the kernel process. |
| external command | noun | A program the kernel runs on the underlying system via execve(2) family, often via `$PATH`. |
| kernel | noun | The kaish execution core. Not the OS kernel. |
| mount | noun, verb | A path prefix bound to a filesystem, or the act of binding one. |
| typed | adjective | A value keeps its JSON type through substitution. It is not stringified. |
| overlay | noun, adjective | Copy-on-write mode. Writes land in a virtual upper layer until committed. |
| spill | verb, noun | To write oversize output to a file, or the file that results. |

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

### Code style

- Comments are short and direct. Narrative goes in the commit message.
- **`///` on a builtin argument is published to agents.** `params_from_clap` copies it
  into `ParamSchema.description`. Describe the flag's behavior in simple English;
  implementation notes go in `//`. See "Published builtin text" below.
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
  every example in a code fence (indented blocks get re-wrapped); no `##` headings or
  tables (use a short capitalized line).
- **Add files by name:** `git add <file>`. Never `git add -A` or `git add .`.

### Commit messages

Commit and PR bodies summarize the decisions behind the change, **drawn from the
conversation with the user**.

Write a clinical engineering narrative: the problem, the evidence, the decision, the
rule now in force. Design stories, plans, review transcripts, and model-panel results
live in agent memory outside the repo. Do not reference agent memories in code or
commits.

## Documentation

- `docs/LANGUAGE.md` — complete language reference.
- `docs/EMBEDDING.md` — embedder guide: kernel construction, capability features,
  `ExecuteOptions`, custom tools.
- `crates/kaish-help/content/en/*.md` — help content, embedded at compile time
  (`docs/help` symlinks here). `crates/kaish-help/src/fragments.rs` holds the fragment
  registry; `compose.rs` the recipes; design in `docs/composable-help.md`.

**Keep in sync:** when adding builtins or changing syntax, update the help files.
`help builtins` is generated from tool schemas. `syntax.md` is **generated** from the
Syntax fragments: edit `fragments.rs`, then run
`cargo run -p kaish-help --example regen_syntax` (a drift test fails if it is stale).
`limits.md` and `docs/LANGUAGE.md` are updated by hand.

## Changelog

`CHANGELOG.md` follows [Keep a Changelog 1.1.0](https://keepachangelog.com/en/1.1.0/)
and [Semantic Versioning](https://semver.org). While pre-1.0, minor (`0.X.0`) releases
may carry breaking changes, marked **BREAKING**. Keep each bullet to 25–40 words.
