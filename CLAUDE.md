# kaish

**kaish** (会sh) is a predictable shell for AI agents, embeddable, and available as an MCP server.

## Project Overview

会sh is stable and the language has settled down. There may still be some changes before 1.0
for ergonomics or correctness. The MCP is mature and will continue to follow rmcp updates and
utilize maximum MCP features. The repl is kept up to date to ensure that use case is possible.
[Kaijutsu](https://github.com/tobert/kaijutsu) is the only known embedder of kaish and has the
same maintainer, so API changes are still straightforward where they improve both projects.

**Philosophy**: 80% rule applied to POSIX/Bourne/bash shell. Kaish implements a `sh` subset that passes `shellcheck --enable=all`.

**Explicitly dropped features**: process substitution `<(cmd)`, backticks, `eval`, word splitting

## Crate Structure

Eagerly read the `crates/kaish-types/` crate in full.

```
crates/
├── kaish-types/     # Pure-data leaf crate: OutputData, ExecResult, Value, DirEntry, etc.
├── kaish-help/      # Composable help & instructions content (fragments + recipes); content/en/*.md
├── kaish-glob/      # Glob matching and async file walking with gitignore support
├── kaish-kernel/    # Core: lexer, parser, interpreter, tools, VFS, validator
├── kaish-mcp/       # MCP server (expose kaish as an MCP tool)
├── kaish-client/    # Client implementations (embedded)
├── kaish-repl/      # Interactive REPL with rustyline
└── kaish-wasi/      # WASI target (wasm32-wasip1)
```

## Build Commands

```bash
cargo build                              # Build workspace
cargo build -p kaish-kernel              # Build specific crate
cargo test --all                         # Run all tests
cargo test -p kaish-kernel --test lexer_tests   # Lexer tests only
cargo test -p kaish-kernel --test parser_tests  # Parser tests only
cargo insta test                         # Run snapshot tests
cargo insta test --check                 # CI mode (fails on pending snapshots)
cargo insta review                       # Interactive review of pending snapshots
```

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
- Avoid `mod.rs` — use `src/module_name.rs`
- Full words for names, avoid abbreviations
- Tokio for all async. Blocking in async: `tokio::task::block_in_place(|| ...)`

### Version Control

- **Always add files by name**
- Run `cargo test` before committing

## Architecture

The 核 (kaku/kernel) is the unit of execution. Multiple frontends connect to the same kernel:

```
Frontends (REPL, Embedded, MCP)
    ↓ KernelClient trait
        └── EmbeddedClient (direct in-process)
    ↓
Kernel (核)
    ├── Lexer (logos)
    ├── Parser (chumsky)
    ├── Validator (pre-execution checks)
    ├── Interpreter (tokio async)
    ├── Tool Registry (builtins + user tools)
    ├── VFS Router (local, memory, git backends)
    └── Job Scheduler (background jobs, scatter/gather)
```

## Testing

Uses **rstest** for parameterized tests and **insta** for snapshot testing.
Tests live in `crates/kaish-kernel/tests/`. Snapshots in `crates/kaish-kernel/tests/snapshots/*.snap`.

## Documentation

- `docs/LANGUAGE.md` — complete language reference
- `crates/kaish-help/content/en/*.md` — help system content, embedded at compile time
  via the `kaish-help` crate (repo-root `docs/help` symlinks here). Shared by the
  kernel `help` builtin, the REPL, the MCP server, and embedders.
- `crates/kaish-help/src/` — composition surface: `fragments.rs` (the English
  fragment registry, concept-organized) + `compose.rs` (recipes). Design:
  `docs/composable-help.md`.

**Keep in sync:** When adding builtins or changing syntax, update the relevant help files.
The builtin list in `help builtins` is generated dynamically from tool schemas, but
`syntax.md` and `limits.md` need manual updates.


