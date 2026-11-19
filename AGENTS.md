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

Use this style for runtime help, `///` parameter docs, error
messages, and code comments. Narrative design docs keep their own voice. `CHANGELOG.md`
follows its own bullet rule below.

kaish keeps a small, predictable subset of `sh`. This guide keeps a small, predictable
subset of English so both small and frontier models read rules clearly.

| Surface | Source | Rules |
|---|---|---|
| Always-on spine | `crates/kaish-help/src/fragments.rs` (`Model`, `Foundations`, `Depth::Summary`) | Terse rule and at most one contrast; 3,500-char `BUDGET` |
| Reference syntax | `fragments.rs` (`syntax_section`) → `syntax.md` | Full examples in complete constructs; run `regen_syntax` |
| Topic docs | `crates/kaish-help/content/en/*.md` (except `syntax.md`) | Behavior, defaults, tables; no design history |
| Published builtin text | `crates/kaish-kernel/src/tools/builtin/*.rs` | `///` on fields, `about =` on struct, imperative example labels |
| Language reference | `docs/LANGUAGE.md` | Deeper human reference; keep the eight headings `compose.rs` pins |

Never edit `crates/kaish-help/content/en/syntax.md` by hand. Edit `fragments.rs` and
run `cargo run -p kaish-help --example regen_syntax`.

### Sentence and layout

- **One idea per sentence.** Target 20 words or fewer.
- **Use imperative mood for steps.** Start a procedure with a verb like *Run*, *Quote*,
  or *Pass*.
- **Use active voice for descriptions.** Give every description a clear subject, verb,
  and object.
- **Avoid semicolons and stacked em-dashes.** Split compound thoughts into separate
  short sentences.
- **Prefer tables or bullet lists for three or more variants.** Use a fenced code block
  when runnable syntax teaches better.

### Structural elements

- **H1 headings:** Write a noun phrase, such as `# Overlay VFS Mode`.
- **Code fences:** Tag shell blocks with `sh`. Leave raw text untagged or tag data
  blocks with `jsonl`.
- **Placeholders:** Use the single character `…`, never `...`. In kaish, `...` parses
  as a real string argument.
- **Error markers:** Mark invalid lines with `# error — <fix>`. Quote real shipped error
  messages when practical, or keep paraphrases exact.
- **Restriction tables:** Include a `Use instead` column when listing unsupported flags
  or features.

### Vocabulary choices

Keep the vocabulary small and plain. Avoid figures of speech and idioms. Make the
meaning clear even under truncation or second-language reads.

Keep standard Unix and JSON terms (`stdin`, `stdout`, `stderr`, `exit code`, `glob`,
`symlink`, `subshell`, `parse error`). Write plain English around them.

Use American spellings. Terms with fixed kaish meanings live in the Terms table below.

### One term, one meaning

Pick one word for each concept and keep it. Do not vary words for style.

Write imperative labels on `ToolSchema` examples. Write `"Send STOP by name"`, not
`"Named shorthand"`.

Cross-references take one form per target:

- ``see `help <topic>` `` for a top-level topic (`list_topics()` in `topic.rs` is
  canonical)
- ``see `help syntax` → Section`` or ``see `help <section>` `` for a syntax section
- `docs/LANGUAGE.md`, "Section name" for the language reference

### Provide specific values

State the public exit code, byte limit, flag, default, and condition. This saves tool
calls and prevents guessing.

> Before: Oversize output fails.
>
> After: Oversize output spills to a file and exits 3.

State defaults and scope too. Examples: `"reads stdin when no files are given"` and
`"off by default, applies to -r only"`.

### Fast and informative failures

Lead error messages with the consequence. Name the condition and the fix.

Never leak internal symbol names or module paths in user-facing errors. Keep internal
names in assertions only.

Refuse with the fix named instead of coercing silently. Write `` write `10#$m` ``
instead of guessing octal or zero.

Follow the three-part error shape: the bad value, the rule in a few words, and the fix.

### Published builtin text

Always set `#[command(about = "…")]` on a clap struct. Without `about =`, clap
publishes the struct's `///` comment.

Put behavior in each argument's first `///` paragraph. `params_from_clap` publishes
`get_help()` through `Kernel::tool_schemas()`, falling back to `get_long_help()` if the
first paragraph is empty.

Put implementation notes after a blank `///` line or in `//` comments.

Read `Kernel::tool_schemas()` or run `cargo test -p kaish-kernel --test
published_prose_tests` after modifying a builtin.

### Tiering and model context

Use the same prose for humans and models. Assume truncation and put the highest-impact
rules first.

- **Always-on spine (`Model`, `Foundations`, `Depth::Summary`):** Keep rules terse with
  at most one inline contrast. Insert new rules by `ranked(n)` order and stay within the
  3,500-char `BUDGET` in `compose.rs`.
- **One canonical help location per detail:** Keep each detail in one topic
  (`fragments.rs` for syntax, `content/en/*.md` for topics). Link from `overview.md` and
  `limits.md` instead of copying paragraphs. `docs/LANGUAGE.md` stays the longer human
  reference.
- **Behavior over history in runtime help:** Keep runtime behavior (such as `$$` session
  IDs), defaults, limits, and exit codes. Put design rationale, internal names, and
  retired flags in commit messages or `docs/`.
- **Keep `list_topics()`, `HelpTopic::description()`, and `overview.md` in sync.**

### The example is the rule

Show the working example before explaining it. Make the example teach the rule on its
own.

> Before: **Quote to join.** `$VAR`, `$(cmd)`, and globs are each a separate word unless
> quoted — kaish never pastes adjacent unquoted tokens.
>
> After: `"$dir/file.txt"` is one path. kaish keeps `$VAR`, `$(cmd)`, and globs as
> separate words. Quote the whole word to join text with an expansion.

- **Show complete constructs in reference examples.** Wrap tests inside a full statement
  (`if [[ k in $r ]]; then …; fi`). Never show bare `[[ k in $r ]]` alone. Short inline
  loop heads in the spine (`for x in $(values $c)`) are exempt.
- **Teach the plain form and every variant.** Show `!=` alongside `in`, `xs=[]`, and
  `${u["a key"]}` side by side.
- **Anchor novel forms to known ones.** For example, state that `push` takes a variable
  name like `read` or `unset`.
- **Use positive rules for instructions.** Write *"Quote the whole path"* instead of
  *"Do not leave expansions unquoted."* Keep `never` for guarantees, such as *"kaish
  never splits words."*
- **Break priors with paired contrasts.** Put the valid line first and `# error — <fix>`
  immediately below it. `limits.md` is the one page that leads with missing features and
  their replacements.

```sh
echo "$dir/file.txt"
echo $dir/file.txt     # error — quote the whole path
```

### Verification after help changes

- Run `cargo run -p kaish-help --example regen_syntax` after editing `fragments.rs`.
- Run `cargo test -p kaish-help` to check the 3,500-char `BUDGET`,
  `syntax_md_matches_fragments`, and phrase-pinned tests in `compose.rs`.
- Run `cargo test -p kaish-kernel --test builtin_unknown_flag_tests` after editing
  `limits.md`.
- Re-run the LLM panel from `docs/designing-syntax-with-llms.md` before release when
  `Foundations` or `collections` changes.

### Terms

These terms carry a fixed definition. **This table is the source.** Add a term when a
collision appears in real prose, not in advance.

| Term | Part of speech | Meaning |
|---|---|---|
| builtin | noun | An embedded Unix-like tool that runs inside the kernel process. |
| external command | noun | A program the kernel runs via the `execve(2)` family, often through `$PATH`. |
| fail loud | adjective, verb phrase | An error is explicit and immediate. kaish never continues on a wrong assumption. |
| familiar syntax | noun phrase | Existing `sh` skill transfers because kaish preserves familiar syntax. |
| hazard | noun | A condition with a predictable failure. State behavior first, then the hazard, then the fix. |
| kernel | noun | The kaish execution core. Not the OS kernel. |
| mount | noun, verb | A path prefix bound to a filesystem, or the act of binding one. |
| override | noun | A documented escape hatch past a restriction (`-E`, `--lines`), or a flag that takes precedence over another flag (`-d` over `-R`). |
| overlay | noun, adjective | Copy-on-write mode. Writes land in a virtual upper layer until committed. |
| spill | verb, noun | To write oversize output to a file, or the file that results. |
| typed | adjective | A value keeps its JSON type through substitution. It is not stringified. |

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
- A leading zero is text (`007`, `0644`). Where kaish needs a number (`$(( ))`,
  `[[ -eq ]]`, `test`, a loop count, or a list index), it is an error naming `8#10`,
  `10`, or `10#$x`. kaish never answers octal or a third number.
- An integer literal must fit in 64 bits; overflow, division by zero, an unset
  variable, an empty `$(( ))`, and assignment inside `$(( ))` are errors, never
  wraps or zeros. A string is a value, never an expression.
- `fromjson` parses JSON and nothing else; it is also the string-to-number coercion
  (`fromjson 1e3`). `printf %x` / `%o` format the other direction; `$(random --max N)`
  replaces `$RANDOM`.

### Code style

- Comments are short and direct. Narrative goes in the commit message.
- **Audit published `///` text on builtin arguments.** See "Published builtin text" above.
- New modules use `src/module_name.rs`, not `mod.rs`.
- Full words for names; avoid abbreviations.
- Tokio for all async. Blocking in async: `tokio::task::block_in_place(|| …)`.
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
