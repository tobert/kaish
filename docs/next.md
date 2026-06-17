# next — pre-0.9 punch list (EPHEMERAL)

**Delete this file before the 0.9 release** once the items are triaged/landed (the
`awk-overhaul.md` burn-down convention). It is the working list for the pre-release
pass; durable deferrals graduate to `issues.md`, durable fixes get a test +
CHANGELOG bullet. Created 2026-06-17.

Sources: a multi-agent review of the 0.9 surface (five Sonnet area passes + two
independent deepseek/kaibo consults). Findings the two methods agreed on, or that
were re-verified in the code here, are marked ✅. Where a single reviewer proposed a
fix that is wrong/partial, that's noted.

---

## P1 — silent wrong output (kaish's worst class)

### awk `compare_values` uses `||` not `&&` ✅ verified
`awk.rs:2342`. `if l_num || r_num` forces numeric comparison when *either* side is
numeric, so a non-numeric string vs a numeric literal coerces the string to `0.0`:
`"abc" == 0` → **true** (gawk: false, string compare). Silent wrong comparisons.
**The obvious `&&` fix is only partial** — true POSIX needs the *numeric-string*
(strnum) concept (an input-derived `"0"` field compares numeric; the string constant
`"abc"` never does). `&&` fixes the reported cases and most common ones; full
correctness is a deeper change to how `AwkValue` tracks strnum-ness.

### awk `length(arr)` returns 0 ✅ verified
`awk.rs:2366-2372`. An array name is eval'd as a scalar expr → `Uninitialized` →
`""` → length 0, instead of the element count. `length()` (bare, on `$0`) is fine.

---

## P2 — confirmed bugs worth fixing for 0.9

### awk `exit N` exit code is dropped ✅ verified
`awk.rs:1862` (`ControlFlow::Exit(_)`), `execute` always returns `Ok`. `awk '{exit
1}'` → builtin exit **0**. Scripts relying on `exit N` to signal failure silently
succeed. (END blocks still run — only the code is lost.)

### awk invalid FS / `split` separator regex silently falls back to literal split ✅ both reviewers
`awk.rs:1813` and `:2437` — `Err(_) => record.split(&fs)`. An invalid regex (`-F '['`)
silently degrades to literal-string splitting → silently wrong field counts. Violates
no-silent-fallback. Fix: propagate the error.

### awk multiple `-v` — only the last is honored ✅ both reviewers
`AwkArgs.var: Option<String>` (clap keeps last). `awk -v a=1 -v b=2 …` silently drops
`a`. Fix: `Vec<String>`, apply all.

### awk `$0 = expr` doesn't re-split fields ✅ both reviewers
`set_field` guards re-split on `n > 0`, so assigning `$0` leaves `$1..$NF`/`NF` stale
(`sub`/`gsub` work around it; direct assignment doesn't). Fix: re-split on `n == 0`.

### validate `show_warnings` tautology ✅ trivial
`validate.rs:85` — `has_flag("warnings") || has_flag("w") || !flags.contains("warnings")`
≡ always true (`A || B || !A`). The `-w`/`--warnings` flag is inert. Fix: drop the
`!A` term.

### jq file-read + `head` line-mode use `from_utf8_lossy` ✅
`jq_native.rs` (positional/`--path` file read) and `head.rs:167` (line-mode stdin)
silently U+FFFD-corrupt non-UTF-8. The stdin strict-decode fix missed these paths;
`head` is newly reachable with binary stdin via the lazy-pipe feature. Use
`read_stdin_to_text`/`from_utf8` (loud) like the rest.

### `head` on empty pipe input emits a spurious `\n` ✅
`head.rs` stream path does `format!("{}\n", lines.join("\n"))` with no `is_empty`
guard (the buffered path guards correctly). `true | head` → `"\n"` not `""`.

---

## P3 — record to issues.md or fix opportunistically

- **sed `p` / `s///p` in non-quiet mode** suppress the auto-print → single print
  where GNU/POSIX print twice. Invisible in tests (all `p` tests use `-n`). Add a
  non-quiet test at minimum.
- **sed `s///0`** silently treated as first-match (GNU errors); **empty `//`** compiles
  an always-match regex instead of reusing the last pattern. Pin behavior with tests.
- **awk `"1e"`→0** (`parse_awk_number` accepts a partial exponent, then `unwrap_or(0.0)`);
  **`FILENAME` always empty** (never set from the input path); **`RS=""` paragraph mode**
  splits on exactly `"\n\n"` so runs of blank lines yield stray records (`awk.rs:1844`);
  **`OFMT` variable ignored** (format hardcoded `%.6g`). All low-frequency.
- **multi-file `head`/`tail` strip the trailing newline** while single-file emits one —
  inconsistent (`head.rs`/`tail.rs` `*_files`).
- **stdin bridge-thread leak / `block_on`-after-shutdown race** (both reviewers):
  `kaish-repl/src/main.rs` detaches an OS thread blocked in `read(2)`; it can't observe
  `PipeReader::drop` while parked, so it lingers until process exit (CLI: bounded, no
  hang) — but a **long-lived embedder** using `execute_with_pipe_stdin` with a
  never-closing producer would leak a thread permanently. Also a theoretical
  `Handle::block_on`-after-runtime-drop window. Harden with `select!`/cancellation.
- **`PipeStdinGuard` `try_write` silent skip** (`kernel.rs:~1676`) and the **test-only
  `BackendDispatcher::try_external` stdin-task leak** on unreachable error returns
  (`dispatch.rs`, missing the `AbortStdinCopyOnDrop` guard the production path has).
  Both "shouldn't fire" but are silent — add the guard / make it loud.
- **`head -n -0`** signed-zero can't be distinguished (lexer-level; already in issues.md).

---

## Doc-revamp follow-ups (from the MCP drop)

- **`composable-help.md`** still frames the help system around "the MCP server" (~19
  mentions). The composition design is unchanged (recipes serve embedders); reword the
  consumer from "MCP frontend" to "embedders". Not user-facing, deferred.
- **`KernelConfig::mcp()` / `OutputLimitConfig::mcp()` preset constructors** are still
  named `mcp` (they're the sandboxed-agent preset). Kept as-is to avoid an unrequested
  breaking API rename — **decide** whether to rename to `agent()`/`sandboxed()` for 0.9
  (breaking) or keep. Used by docs (`EMBEDDING.md`, `vfs.md`) + tests.
- **Historical design docs** (`resonance-2026-06.md`, `reviews.md`, `binary-data.md`,
  `kaish-overlayfs.md`, `date-design.md`, …) still mention MCP as a then-consumer. Left
  as historical record — not rewriting past narratives.
