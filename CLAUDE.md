# CLAUDE.md

This file provides guidance to models when working with in this repository.

## Project Overview

**kaish** (会sh — "the gathering shell") is a predictable shell for MCP tool orchestration.

Part of the [Kaijutsu](https://github.com/tobert/kaijutsu) project.

**Status**: Core implementation complete. Lexer, parser, interpreter, 54 builtins, MCP client/server, VFS.

## Philosophy

**80% of a POSIX/Bourne/bash shell, 100% unambiguous.**

- **Bourne-lite** — familiar syntax, no dark corners
- **Predictable over powerful** — if bash has a confusing edge case, kaish doesn't have that feature
- **ShellCheck-clean** — the Bourne subset passes `shellcheck --enable=all`
- **Agent-friendly** — easy to predict and validate
- **Fail fast** — ambiguity is an error. does not guess

### ShellCheck-Clean Design

The Bourne-compatible subset of kaish should pass `shellcheck --enable=all`.
When implementing features, verify they don't introduce constructs ShellCheck
would warn about. Extensions (floats, typed params, MCP tools, scatter/gather)
are outside ShellCheck's scope and clearly marked.

## Build Commands

```bash
cargo build                              # Build workspace
cargo build -p kaish-kernel              # Build specific crate
cargo test --all                         # Run all tests
cargo test -p kaish-kernel --test lexer_tests   # Lexer tests only
cargo test -p kaish-kernel --test parser_tests  # Parser tests only
cargo insta test                         # Run snapshot tests
cargo insta test --check                 # CI mode (fails on pending snapshots)
cargo test --features proptest -- --ignored  # Property tests
cargo tarpaulin --out Html --output-dir coverage/  # Coverage
cargo +nightly fuzz run parser -- -max_len=4096    # Fuzz (nightly)
```

If Cap'n Proto schema changes don't trigger rebuilds:
```bash
cargo clean -p kaish-schema && cargo build -p kaish-schema
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

- Comments only for non-obvious "why"
- Avoid `mod.rs` — use `src/module_name.rs`
- Full words for names, avoid abbreviations
- Prefer newtypes over primitives: `struct JobId(Uuid)` not `Uuid`
- Use enums for states and variants
- Define traits for shared capabilities

### Async Patterns

Everything runs on tokio. For blocking operations in async contexts:
```rust
let state = tokio::task::block_in_place(|| self.state.blocking_write());
```

### Version Control

- **always add files by name**
- Review with `git status` before and after staging
- Use `git diff --staged` before committing
- Run `cargo test` before committing

### Commit Attribution

Models should include attribution for themselves.

```
Co-Authored-By: Claude <claude@anthropic.com>
```

## Architecture

The 核 (kaku/kernel) is the unit of execution. Multiple frontends connect to the same kernel:

```
Frontends (REPL, Script Runner, Embedded)
    ↓ KernelClient trait
        ├── EmbeddedClient (direct in-process)
        └── IpcClient (Unix socket + Cap'n Proto RPC)
    ↓
Kernel (核)
    ├── Lexer (logos)
    ├── Parser (chumsky)
    ├── Validator (pre-execution checks)
    ├── Interpreter (tokio async)
    ├── Tool Registry (54 builtins + MCP)
    ├── VFS Router (local, memory, git backends)
    └── Job Scheduler (background jobs, scatter/gather)
```

### Crate Structure

```
crates/
├── kaish-schema/    # Cap'n Proto codegen from schema/kaish.capnp
├── kaish-kernel/    # Core: lexer, parser, interpreter, tools, VFS, validator
├── kaish-mcp/       # MCP client integration (consuming external MCP tools)
├── kaish-client/    # Client implementations (embedded, IPC)
└── kaish-repl/      # Interactive REPL with rustyline
```

**Note:** The `kaish-mcp` crate provides both MCP server (exposing kaish's execute tool) and MCP client (consuming external MCP tools) functionality.


## Language Key Points

**Bourne-compatible syntax:**

- `VAR=value` — assignment (no spaces around `=`)
- `$VAR` and `${VAR}` — both work for expansion
- `${VAR:-default}` — default values
- `${#VAR}` — string length
- `$0`-`$9`, `$@`, `$#` — positional parameters
- `'literal'` and `"interpolated"` — both quote styles
- `[[ ]]` — test expressions
- `if/elif/else/fi`, `for/do/done`, `while/do/done` — control flow
- `break`, `continue`, `return`, `exit` — control statements
- `set -e` — exit on error mode
- `source file` or `. file` — script sourcing
- `-x`, `--flag` — flag arguments
- `key=value` — named arguments

**Kaish-specific:**

- 散/集 (scatter/gather) for parallel execution
- User-defined tools with typed parameters
- MCP tool integration (client-side, consuming external tools)
- VFS mounts with multiple backends (local, memory, git, jobs)
- Pre-execution validation
- Full arithmetic expressions `$((expr))`

### What's Intentionally Missing

Process substitution `<(cmd)`, backticks, aliases, `eval`, shell-level glob expansion

## Testing Strategy

Uses **rstest** for parameterized tests and **insta** for snapshot testing.

Test files in `crates/kaish-kernel/tests/`:
- `lexer_tests.rs` — rstest parameterized lexer tests (94 tests)
- `parser_tests.rs` — insta snapshot tests for AST output (101 tests)
- `validation_tests.rs` — pre-execution validation tests
- `realworld_builtin_tests.rs` — integration tests from real usage
- `job_stream_tests.rs` — BoundedStream and JobFs integration tests
- `snapshots/*.snap` — insta snapshot files for parser tests

Snapshot workflow:
```bash
cargo insta test           # Run tests, create .snap.new for changes
cargo insta review         # Interactive review of pending snapshots
cargo insta accept         # Accept all pending snapshots
```

## Key Documentation

| File | Purpose |
|------|---------|
| `README.md` | Project overview, quick tour, MCP integration |
| `docs/LANGUAGE.md` | Complete language reference with examples |
| `docs/BUILTINS.md` | Tool compatibility and implementation notes |
| `docs/help/*.md` | MCP help system content (agent-facing) |

### Help System

The `docs/help/` directory contains markdown files that are embedded at compile time
into the kernel and exposed via the MCP `help` tool. These are concise reference cards
for LLM agents using kaish.

**Files:**
- `overview.md` — What kaish is, topic list, quick examples
- `syntax.md` — Variables, quoting, pipes, control flow
- `vfs.md` — Virtual filesystem mounts and paths
- `scatter.md` — Parallel processing (散/集)
- `limits.md` — Known limitations and workarounds

**Keep in sync:** When adding builtins or changing syntax, update the relevant help files.
The builtin list in `help builtins` is generated dynamically from tool schemas, but
`syntax.md` and `limits.md` need manual updates.

## Schema Files

- `schema/kaish.capnp` — Cap'n Proto schema (kernel protocol, types)
