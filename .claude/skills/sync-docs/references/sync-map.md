# Sync Map: Code → Documentation

Detailed mapping of every sync point. Uses function names and section headers
rather than line numbers — these are durable across edits.

## Primary Source: Builtin Registration

**File:** `crates/kaish-kernel/src/tools/builtin/mod.rs`
**Function:** `register_builtins()`

This is the canonical list of all builtins. Every `registry.register(...)` call
adds a tool. Count these to get the total.

Notable details:
- `ps::Ps` is `#[cfg(target_os = "linux")]` — Linux-only
- `test_builtin::Test` and `test_builtin::Bracket` register as separate tools
  (`test` and `[`)
- `true_false::True` and `true_false::False` come from one module
- `introspect::Mounts` and `introspect::Tools` come from one module
- `jq_native::JqNative` registers as `jq` (check the schema name)

## Help Categorization

**File:** `crates/kaish-kernel/src/help.rs`
**Function:** `format_tool_list()`

Hard-coded match statement that sorts tool names into categories. When a new
tool is registered but not added here, it falls into the `_ => other_tools`
catch-all. This is the most common source of drift.

**Categories and their match arms:**
- Text Processing: grep, sed, awk, cut, tr, sort, uniq, wc, head, tail, split, diff
- Files & Directories: cat, ls, tree, cd, pwd, mkdir, rm, cp, mv, touch, ln, readlink, write, glob, find, stat, dirname, basename, realpath, mktemp, patch
- Shell & System: echo, printf, read, sleep, date, env, export, set, unset, true, false, test, [, assert, seq, tee, hostname, uname, which
- JSON: jq
- Parallel: scatter, gather
- Processes & Jobs: exec, spawn, jobs, wait, ps, git
- Introspection: help, validate, vars, mounts, tools, tokens

**Verification:** the union of all match arms should equal the set of tools
from `register_builtins()`. Any difference is a bug.

## README.md

### Builtin Table (under `## Builtins`)

The markdown table listing tools by category. Search for `| Category | Tools |`.

Note: README uses 7 categories vs help.rs's 8. README splits git into its own
row and merges "Processes & Jobs" + "Shell & System" + "Introspection"
differently. **This is intentional — don't unify them.**

### Quick Tour (under `## Quick Tour`)

Code blocks that demonstrate kaish features. Every example should actually run
in kaish. Check for:
- Removed builtins still referenced
- Syntax that changed since the example was written
- Features claimed but not yet implemented

### Builtin Philosophy (under `## Builtins`, before the table)

Prose about design principles. Key claims to verify:
- "in-process, zero-fork" — still true?
- "80/20" principle — still the approach?
- "ERE everywhere" — still consistent?

### Language Features Table (under `## Language Features`)

High-level feature summary table. Verify claims match implementation.

## CLAUDE.md

### Crate Structure (under `### Crate Structure`)

Tree diagram of `crates/`. Verify against `ls crates/` and each crate's
`Cargo.toml` description.

### Language Key Points (under `## Language Key Points`)

Lists syntax features with examples. Cross-check against parser capabilities
and the "What's Intentionally Missing" list.

### Architecture Description (under `## Architecture`)

Kernel diagram and component list. Verify component names and descriptions.

### Build Commands (under `## Build Commands`)

Build and test commands. Verify these still work.

### Documentation Table (under `## Key Documentation`)

Points to docs files. Verify paths resolve.

### Help System Notes (under `### Help System`)

States what's auto-generated vs manual. Keep accurate.

## Help Files (compiled into binary)

All under `crates/kaish-kernel/docs/help/`. Embedded at compile time via
`include_str!` in `help.rs`.

### overview.md

- Topic list: must match available help topics in `help.rs` `list_topics()`
- Quick examples: should use representative, working builtins
- Project description: should match README's positioning

### syntax.md

Complete syntax reference. Cross-check against:
- Lexer token types in `lexer.rs`
- Parser rules
- Interpreter behavior for edge cases

### limits.md

Builtin limitations table. Current entries:

| Builtin | Limitation |
|---------|------------|
| `set`   | Only `-e` (exit on error) is supported |
| `ps`    | Linux-only (reads `/proc`) |
| `git`   | Operates on real filesystem, not VFS |
| `head`/`tail -c` | Counts UTF-8 characters, not bytes |

Update when:
- Tool capabilities expand (remove limitation)
- New limitations are discovered
- Platform support changes

### scatter.md

Scatter/gather documentation with examples. Verify parameter names and
defaults match `scatter.rs` and `gather.rs` schemas.

### vfs.md

VFS mount documentation. Verify:
- Listed mount points match actual VFS router configuration
- Example sessions still work
- Git mount docs match git VFS backend capabilities

## docs/LANGUAGE.md

Complete language reference. Cross-check against parser tests in
`crates/kaish-kernel/tests/parser_tests.rs` and lexer tests in
`crates/kaish-kernel/tests/lexer_tests.rs` for feature coverage.

## Tool Schema Quality Checks

For each `builtin/*.rs` file, `fn schema() -> ToolSchema` provides:
- `name` — the command name users type
- `description` — one-liner shown in `help builtins` and MCP tool listings
- `params` — parameter definitions with types and descriptions
- `examples` — usage examples shown in `help <tool>`

Quality criteria:
- Description should be a clear, complete sentence
- Examples should show the most common use case
- Parameters should document defaults and valid values
- Required vs optional should be accurate
