# next — pre-0.9 punch list (EPHEMERAL)

**Delete this file before the 0.9 release** once the remaining items are
triaged/landed (the `awk-overhaul.md` burn-down convention). Durable deferrals
graduate to `issues.md`, durable fixes get a test + CHANGELOG bullet. Created
2026-06-17.

Sources: a multi-agent review of the 0.9 surface (five Sonnet area passes + two
independent deepseek/kaibo consults).

---

## P1 + P2 — LANDED 2026-06-17 (`fix/pre-0.9-punch-list`)

All silent-wrong-output and confirmed-bug items are fixed, each with a failing
test first (model-consensus oracle, gawk as a sanity check) and a CHANGELOG
bullet. Tests live in `crates/kaish-kernel/tests/builtin_fidelity_tests.rs`
(and `execute_pipe_stdin_tests.rs` for the binary-stdin loud-error cases).

- ✅ awk POSIX numeric-string (strnum) comparison — `AwkValue::StrNum` variant;
  input-derived values (fields, `split()`, `-v`, array subscripts) compare
  numerically only when they look numeric; string constants never do.
- ✅ awk `length(arr)` returns the element count.
- ✅ awk `exit N` sets the exit code (output + END still run; END's `exit` wins).
- ✅ awk invalid multi-char `FS` / `split()` regex is a loud error (single-char
  `FS` stays a literal separator).
- ✅ awk multiple `-v` all applied — **and the natural unquoted `-v NAME=VALUE`
  form now works at all**: the real fix was in the kernel arg-binder (a
  value-flag now consumes a following `key=value` `WordAssign` token as its
  argument, getopt-style). This was deeper than "only the last `-v` honored".
- ✅ awk `$0 = expr` re-splits the record / resets `NF`.
- ✅ validate `-w`/`--warnings` actually gates warnings (was the `A || B || !A`
  tautology); reads the parsed clap field, not the raw map.
- ✅ jq (file) / head (line-mode stdin) reject non-UTF-8 loudly (no `U+FFFD`
  mangle); head also errors on binary streaming stdin.
- ✅ head on empty pipe emits nothing (was a bare `\n`).

## P3 — graduated to `docs/issues.md`

The low-frequency fidelity gaps (sed `p`/`s///p` double-print, sed `s///0` /
empty `//`, awk `"1e"`/`FILENAME`/`RS=""`/`OFMT`, multi-file head/tail trailing
newline, stdin bridge-thread leak, `PipeStdinGuard`/`BackendDispatcher` silent
skips) now live under **issues.md → P3 → "Pre-0.9 punch-list residuals"**.
`head -n -0` signed-zero was already in issues.md.

## Doc-revamp follow-ups (from the MCP drop) — STILL OPEN

- **`composable-help.md`** still frames the help system around "the MCP server"
  (~19 mentions). The composition design is unchanged (recipes serve embedders);
  reword the consumer from "MCP frontend" to "embedders". Not user-facing.
- **`KernelConfig::mcp()` / `OutputLimitConfig::mcp()` preset constructors** are
  still named `mcp` (they're the sandboxed-agent preset). **DECIDE** for 0.9:
  rename to `agent()`/`sandboxed()` (breaking) or keep. Used by docs
  (`EMBEDDING.md`, `vfs.md`) + tests. *(Owner decision — Amy.)*
- **Historical design docs** (`resonance-2026-06.md`, `reviews.md`,
  `binary-data.md`, `kaish-overlayfs.md`, `date-design.md`, …) still mention MCP
  as a then-consumer. Left as historical record — not rewriting past narratives.
