---
name: sync-docs
description: >
  This skill should be used when the user asks to "sync docs", "update README",
  "update CLAUDE.md", "sync documentation with code", "check if docs are current",
  "refresh builtin table", "update help docs", "improve docs", "review documentation",
  or mentions documentation drift, stale docs, doc quality, or doc/code synchronization.
  Also trigger after adding or removing a builtin tool, changing language syntax,
  or modifying VFS mounts.
---

# Sync & Improve Documentation

Synchronize and continuously improve README.md, CLAUDE.md, help system docs,
and language reference with the actual kaish codebase. The source of truth is
always the code — docs follow. But don't just sync mechanically: apply kaizen.
Every pass should leave docs clearer, more accurate, and more welcoming.

## Document Audiences

Each document has a primary audience. Optimize for that audience:

| Document | Primary Audience | Optimize For |
|----------|-----------------|--------------|
| **README.md** | **Humans** (developers discovering the project) | Clarity, invitation, first impressions. This is the front door. |
| **CLAUDE.md** | **Claude / LLM agents** working in the codebase | Efficient orientation: architecture, conventions, build commands. What an agent needs to be productive immediately. |
| **`docs/help/*.md`** | **LLM agents** consuming kaish via MCP | **Token density.** Every token costs money and context window. Pack maximum useful information per token. No filler, no preamble, no "this document describes...". |
| **`docs/LANGUAGE.md`** | **Both** humans and agents | Complete language reference with working examples. |
| **`help.rs`** | **Runtime** (agents calling `help builtins`) | Correct categorization of every registered tool. |

> **Key distinction:** README.md sells kaish to humans. CLAUDE.md orients
> agents to work *on* kaish. Don't mix these concerns. README doesn't need
> crate internals; CLAUDE.md doesn't need marketing copy.

## Sources of Truth

| What | Canonical Location |
|------|-------------------|
| Registered builtins | `crates/kaish-kernel/src/tools/builtin/mod.rs` → `register_builtins()` |
| Tool schemas (names, params, descriptions) | `crates/kaish-kernel/src/tools/builtin/*.rs` → `fn schema()` |
| Help categorization | `crates/kaish-kernel/src/help.rs` → `format_tool_list()` match arms |
| Language syntax | `crates/kaish-kernel/src/` (lexer, parser, interpreter) |
| VFS mounts | `crates/kaish-kernel/src/vfs/` |
| Help content (compiled in) | `crates/kaish-kernel/docs/help/*.md` |

## Category Mapping — Do Not Unify

> **README.md and help.rs use different category schemes. This is intentional.
> Do NOT try to make them match.** Each serves its audience:

| help.rs Category | README Category | Notes |
|-----------------|-----------------|-------|
| Text Processing | Text | Same tools |
| Files & Directories | Files | Same tools |
| JSON | JSON | Same |
| Processes & Jobs | Git + part of System | README gives git its own row |
| Parallel (散/集) | Parallel | Same |
| Shell & System | System | README merges more here |
| Introspection | Meta | Different label, similar scope |

README groups for human scanning. help.rs groups for agent runtime display.

## Workflow

### Phase 0: Quick Check (do this first!)

Before launching the full workflow, do a fast sanity check:

1. Count `registry.register(...)` calls in `register_builtins()`
2. Count tools in the README builtin table
3. Count match arms in `help.rs` `format_tool_list()`
4. Check `ls crates/` against CLAUDE.md crate structure

**If all counts match and no new crates exist**, skip the deep inventory
(Phase 1 agents) and go directly to Phase 2's Gemini review + typo scan.
Report findings and stop if nothing needs fixing — don't force work that
isn't needed.

### Phase 1: Gather (2 parallel subagents)

Only needed when Phase 0 found mismatches, or after significant code changes.

**Agent 1 — Builtin inventory + categorization** (subagent_type: `Explore`):
Read `register_builtins()` in `mod.rs`. Extract every registered tool name
and count. Also read `format_tool_list()` in `help.rs` to get the current
category match arms. Report any tools not in a category.

**Agent 2 — Doc state** (subagent_type: `Explore`):
Read `README.md` and `CLAUDE.md`. Extract the builtin table, feature claims,
crate structure, examples, and architecture description. Note anything that
could be stale.

**Do NOT re-inventory builtins from help files.** Agent 1 already has the
canonical list. Help file review (Phase 2) checks content quality, not
tool coverage.

### Phase 2: Diff & Review

Compare inventories from Phase 1 and identify:
- Tools in code but missing from README table or help.rs categories
- Tools in README/docs but removed from code
- Stale examples referencing removed tools or outdated syntax
- CLAUDE.md crate list not matching actual `crates/` directory
- Feature claims that no longer match implementation

#### Gemini Review (always run this)

Use `consult_gemini_pro` to review README.md quality. Feed it the file and ask:

> Review this README.md for a shell project called kaish. Critique it as if
> you're a developer seeing it for the first time. Is the value proposition
> clear? Are the examples inviting? Is anything confusing or missing? Would
> you want to try it?

Also useful: feed Gemini the `docs/help/*.md` files and ask whether they'd be
useful to an LLM agent encountering kaish for the first time.

#### Help File Quality Check

Help files are consumed by LLM agents paying per-token. Optimize ruthlessly
for information density. Humans who need help can just ask Claude.

Review each help file for content quality (not builtin coverage — that's
handled by the table sync):
- **Token density:** cut filler, preamble, meta-commentary. Lead with facts.
- Are examples current and working? One clear example > three weak ones.
- Is anything thin, unclear, or missing context?
- Does limits.md reflect actual current limitations?
- Does overview.md topic list match `help.rs` topics?

### Phase 3: Apply Fixes

**If Phase 2 found nothing:** report "docs are in sync" and stop. Don't
manufacture work.

**If fixes are needed**, apply them. For each document, respect its audience:

**README.md** (for humans):
- Update builtin table to match code
- Verify quick tour examples actually run
- Check feature claims match implementation
- Improve clarity for newcomers

**CLAUDE.md** (for agents):
- Update crate structure to match `crates/`
- Verify architecture description is current
- Update build commands if changed
- Check documentation pointers still resolve

**help.rs categorization:**
- Ensure every registered builtin appears in exactly one category match arm
- New tools must not silently fall to "Other"

**Help files (`docs/help/*.md`)** — optimize for token density:
- Verify examples use current syntax
- Cut filler phrases ("In order to", "It should be noted that", "This section describes")
- Lead with the useful information, not meta-commentary about the document
- Prefer tables and structured formats over prose where equivalent
- One good example beats three mediocre ones
- Check limits.md reflects actual limitations
- Ensure overview.md topic list matches help topics

**docs/LANGUAGE.md:**
- Cross-check syntax against parser
- Verify examples work
- Fill gaps for undocumented features

Apply edits to independent files in parallel where possible.

### Phase 4: Verify

Run `cargo test --all` and `cargo clippy --all`. The help system tests catch
mismatches between schemas and categorization.

## Documents That Must Stay in Sync

| Document | Section | What Drifts | Risk |
|----------|---------|-------------|------|
| `README.md` | Builtin table (under `## Builtins`) | Tool names and categories | **HIGH** |
| `README.md` | Quick tour examples | Commands and syntax | **HIGH** |
| `help.rs` | `format_tool_list()` match arms | Category assignments | **HIGH** |
| `docs/help/overview.md` | Topic list | Available help topics | Medium |
| `docs/help/syntax.md` | Syntax reference | Language features | Medium |
| `docs/help/limits.md` | Limitations table | Builtin constraints | Medium |
| `docs/help/scatter.md` | Parameter docs | scatter/gather API | Medium |
| `docs/help/vfs.md` | Mount points | VFS configuration | Medium |
| `CLAUDE.md` | Crate structure | Workspace crates | Medium |
| `CLAUDE.md` | Language key points | Syntax features | Medium |
| `docs/LANGUAGE.md` | Everything | Language reference | Medium |

For detailed mappings, consult `references/sync-map.md`.

## Checklist: Adding a New Builtin

When a new builtin is added, update all of these:

1. `crates/kaish-kernel/src/tools/builtin/mod.rs` — register it
2. `crates/kaish-kernel/src/help.rs` — add to correct category match arm
3. `README.md` — add to builtin table in appropriate category
4. `crates/kaish-kernel/docs/help/limits.md` — if it has known limitations
5. `docs/LANGUAGE.md` — if it introduces new syntax or concepts
6. `CLAUDE.md` — if it changes the language key points or architecture

## Tool Schema Quality

Schemas are agent-facing too — they appear in `help <tool>` and MCP tool
listings. Same token density principle applies.

While reviewing, check tool schemas in `builtin/*.rs`:
- `description` — clear one-liner, no "This tool..." preamble
- `examples` — one strong example showing the most common use case
- Parameter descriptions — precise about types, defaults, valid values
- These drive both `help <tool>` output and MCP tool descriptions

## Additional Resources

- **`references/sync-map.md`** — Detailed mapping of every sync point with
  function names, section headers, and what to check
