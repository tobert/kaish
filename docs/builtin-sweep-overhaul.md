# Builtin Sweep — Overhaul punch list

**Scope:** burn down the builtin-sweep findings (non-awk core utils: cut/tr/sort/
head/tail/tac/uniq/wc/printf/seq/split/jq/grep/cmp/diff/patch/cat/tee/base64/xxd/
dd). Twin of `docs/awk-overhaul.md`: a **temporary, delete-before-release**
issues-style file. When every box is checked, fold lasting notes into
`CHANGELOG.md` + help fragments and **delete this file with all of
`docs/builtin-sweep/`** (PLAN Phase E).

**Oracle:** model-consensus, NOT GNU (`[[model-memory-over-gnu-oracle]]`). Expected
strings are the panel's banked consensus in `expectations-<cluster>.md`;
`docs/builtin-sweep/harness.sh` classifies kaish vs that string
(`MATCH | SILENT-WRONG | LOUD | WRONG-RC`). Re-derive a contested expectation via
the panel (`kaibo`), never gawk.

**Method (hold the line):**
- **TDD, kernel-routed.** Every CODE fix lands with a test asserting `kaish ==
  banked consensus`, through `kernel.execute(...)` not a builtin's `.execute()`.
  Failing test first.
- **Docs ride with the fix.** Same commit updates the relevant
  `crates/kaish-help/content/en/*.md` fragment (+ LANGUAGE.md where syntax) and a
  `CHANGELOG.md` `[Unreleased]` bullet.
- **Hunt target = SILENT-WRONG.** kaish exit 0 + wrong bytes is the data hazard.
  LOUD (kaish refuses) is an acceptable 80%-boundary, but its message must hint
  the fix. Gates green before each commit (`cargo test --all`,
  `cargo clippy --all --all-targets`).

**Phase B result (2026-06-16, harness validated):** clean clusters —
`grep` (incl. no-match rc=1, `-E` alternation, ERE `a{2}`), `cmp`, `diff
identical`, `printf`, `seq`, `sort` core, `tr` core, `tee` (after a runner-quoting
fix). Findings below are the real divergences. Verdicts use the **rc-fixed**
harness (the original `printf X` sentinel clobbered `$?`, mis-filing loud refusals
as SILENT-WRONG — fixed `225e1c5`).

Priorities: **P1** silent corruption / contract break · **P2** fidelity + common-
idiom breakage · **P3** coverage gaps · **P4** doc / format-pin / message quality.

> **STATUS: triaged 2026-06-16. Two items need an Amy decision before coding
> (marked ⛳): the unquoted-comma lexer item (P2.1, touches the no-pasting
> decision) and the trailing-newline policy (P4.1).**

---

## P1 — Silent corruption / contract break

### [x] 1. `cat` corrupts byte concatenation (inserts newlines) — FIXED `995a744`
- **Symptom (CT3 + direct):** `cat A B` where A ends in `\n` emits a **blank line**
  between files; `cat C D` where C is `x` (no trailing `\n`) emits `x\ny\n` instead
  of `xy\n`. kaish `cat` is line-oriented and appends a newline per file/line; it
  must concatenate **bytes verbatim**.
- **Verdict:** SILENT-WRONG, data corruption — the worst class. Any file lacking a
  trailing newline is silently altered; newline-terminated files gain blank lines.
- **Contributing factor:** cat reads/emits line-by-line and re-adds `\n` rather than
  streaming raw bytes (likely the `read_stdin_to_string`/line path, not
  `read_range` byte streaming — see `[[arch_streaming_reads]]`).
- **Fix sketch:** stream bytes through unchanged; never synthesize a separator.
- **Test:** `cat A B` (both `\n`-terminated) == raw concat; `cat C D` (C no trailing
  `\n`) == `xy\n`; single-file `cat` unchanged.

### [x] 0. ⚠ REGRESSION from the stdin fix: `kaish -c` eager-read hangs on open stdin — FIXED 2026-06-17
**Fix landed (lazy, pipe-backed stdin):** the frontend no longer pre-reads. A
detached OS thread copies process stdin → a `PipeReader` that seeds the first
top-level command's `pipe_stdin`; the kernel drains it only if a command reads
stdin, so `echo` returns at once on an open never-EOF pipe (`sleep 10 | kaish -c
'echo hi'` → `hi`, ~11ms, rc 0). Seam (Amy-confirmed shape): frontend builds the
`PipeReader`; kernel exposes `execute_with_pipe_stdin(_streaming)` seeding
`exec_ctx.pipe_stdin` via an RAII guard (mirrors `StdinGuard`), consume-once into
stage 0; `set_stdin` clears `pipe_stdin` so `< file`/heredoc still win. Binary
survives losslessly (byte-clean copy). Kept the eager `with_stdin(String)` path
for embedders with a ready buffer. Tests: `execute_pipe_stdin_tests.rs`
(no-block-on-open-stdin, feeds-reader, first-stage-of-pipeline, redirect-beats,
no-leak-between-calls). Docs: CHANGELOG (amended the draft's Added bullet),
EMBEDDING.md, issues.md MCP follow-up updated.

- **Symptom:** `kaish -c 'echo hi'` **hangs** when stdin is an open, non-TTY pipe
  that never sends EOF (e.g. spawned as a subprocess inheriting an idle pipe — the
  Bash-tool case). bash prints `hi` immediately and never reads stdin; kaish's
  frontend `read_piped_stdin()` (`crates/kaish-repl/src/main.rs`) eagerly
  `read_to_end`s stdin *before* execution, so it blocks on input no command will
  ever consume. Confirmed: `sleep 10 | kaish -c 'echo hi'` → no output, rc 124
  (timeout); `sleep 10 | bash -c 'echo hi'` → prints `hi`.
- **Blast radius:** the standalone `kaish -c`/script CLI only. Embedders
  (kaijutsu/kaibo) feed stdin via the kernel API, unaffected. The sweep harness
  pipes closed stdin (EOF), unaffected — so Phase D can proceed. But it must be
  fixed before PR #7 lands.
- **Introduced by:** `c09ee26` (the eager-String stdin approach). DeepSeek flagged
  eager read-to-EOF as the one hazard; it's worse than "acceptable" because it
  breaks the most common invocation (`kaish -c 'cmd'` with inherited stdin).
- **Fix sketch (lazy, pipe-backed stdin):** don't pre-read in the frontend. Feed
  process stdin to the kernel as a **lazy reader** that seeds `exec_ctx.pipe_stdin`
  (a `PipeReader`), with a detached thread copying process-stdin → pipe writer. A
  command that reads stdin drains the pipe (blocks until data/EOF, correct); a
  command that doesn't (`echo`) never touches it and returns immediately — matching
  shell laziness. Obstacle: `ExecuteOptions::stdin` is `Option<String>` in
  kaish-types, which can't hold a kernel `PipeReader`; needs a new seam (e.g. a
  `Kernel`/`EmbeddedClient` method that takes a boxed `AsyncRead` and builds the
  pipe internally, or a kaish-types reader trait object). Keep the existing
  `ExecuteOptions::stdin` String path for embedders that already have a buffer.
- **Test:** `sleep N | kaish -c 'echo hi'` prints `hi` fast (no hang); `printf data
  | kaish -c 'sort'` still works; binary stdin survives losslessly through the pipe.

### [ ] 2. `tee` / `patch` bypass the latch + trash machinery  (Decision #3 — design)
- **Symptom:** file mutations via `tee`/`patch` go through the VFS (overlay-safe)
  but do **not** honor `set -o latch` (confirmation nonce) or trash-on-overwrite,
  unlike `rm`. Same hazard class as sed's deferred `-i` (`[[sed_ergonomics_pass]]`).
- **NOT a reuse of `rm`'s path** (PLAN review #2): `decide_rm_action →
  {Trash,Delete,Latch}` means "trash IS the op". tee/patch need a *pre-write safety
  copy* then overwrite — a new action. **Design Qs to resolve before coding:**
  `decide_mutation_action → {TrashFirst(path), Latch, Proceed}` (same priority chain
  + `/tmp`,`/v` excludes); tee can create a nonexistent file (no trash, just write)
  where rm requires existence; **overlay semantics** — should latch/trash fire on
  ephemeral overlay writes that only materialize on `kaish-vfs commit`? `--confirm`
  flag name (tee/patch have none today); does `tee -a` (append) gate?
- **⛳ Decisions (Amy, 2026-06-17):**
  - **Latch + trash stay ON consistently, even in overlay mode.** The
    protections are about *agent-operation* safety, not just real-FS data —
    latch guards the user from a dangerous op even in virtual space, and trash
    likewise. So a tee/patch overwrite gates regardless of `--overlay`. (Don't
    bypass gating in overlay; the overlay being reversible is not a reason to
    skip the confirm.)
  - **Truncating overwrite gates (trash + latch); `tee -a` append does NOT gate
    — for now.** Keep the first cut simpler: append is non-destructive, so leave
    it ungated and **leave a note** (here + code comment) that we may add
    latch/trash-on-append once a concrete use case appears. New file (no prior
    content) → just write, no trash.
  - Open impl detail deferred to coding: `decide_mutation_action` shape,
    `--confirm` flag naming, and how trash physically captures prior content in
    overlay mode (overlay preserves the original via `reset`, so trash there may
    be a copy-within-overlay vs real-trash — resolve when building; check back
    if thorny).
- **Test:** `tee`/`patch` over an existing file under `set -o latch` → exit 2 + nonce
  on first call, applies on `--confirm`; trash captures prior content; `/tmp`,`/v`
  excluded; overlay-write behavior pinned per the decision.

---

## P2 — Fidelity & common-idiom breakage

### [ ] ⛳ 1. Unquoted comma in argv splits the word (misleading parse error)
- **Symptom:** `cut -d: -f1,3`, `sort -k2,2n`, `cut … --output-delimiter='|'`, even
  `echo a,b` / `echo 1,3` → shell **parse error** "adjacent words with no space …
  kaish does no token pasting". The user wrote one word; the lexer splits at `,`
  (`Token::Comma`, lexer.rs:348/708) and the no-pasting guard fires.
- **Verdict:** LOUD, but the message is **actively wrong** (nothing was pasted) and
  the idioms are bread-and-butter (`-f1,3`, `-k2,2`, CSV values, coordinates).
- **Contributing factor:** `,` is a standalone `Punctuation` token; bare argv
  positionals/values don't absorb it. Same tokenization class as the awk `-F:` fix
  (which fused `:`/`,` onto **short flags** only — bare positions were left).
  Quoting works (`-f "1,3"`); a glued comma after a short flag does **not** (`-f1,3`
  fails) even though `awk -F,` works.
- **`,` is NOT a vestige and is reserved:** consumed by brace expansion `{a,b,c}`
  (`parser.rs:1317`) and as a bare literal (`parser.rs:2039`), and
  `docs/arrays-and-hashes.md` (lines 252–253) reserves it as an **optional** list/
  record separator (`[1, 2, 3]` ≡ `[1 2 3]`, `{a: 1, b: 2}`). A broad "absorb `,`
  into barewords" lexer change would collide with both current brace expansion and
  the planned arrays/hashes grammar.
- **⛳ Decision (Amy, 2026-06-16): message + docs only — do NOT broaden comma
  lexing.** Keep `,` free for brace expansion + the arrays/hashes future. The fix is
  the error-message hint + help, NOT a grammar change: when an unquoted comma-bearing
  argv word trips the no-pasting guard, teach quoting (`cut -f "1,3"`, `sort -k
  "2,2n"`) instead of the misleading "token pasting" text; document the quote-comma-
  values idiom in the cut/sort help. Respects the 2026-06-08 no-pasting decision and
  the "always quote" rule; doesn't preempt arrays/hashes. **The code work lives in
  P4.3** (this P2 entry is now a pointer — no fidelity fix, kaish is intentionally
  loud here).
- **Test:** `cut -d: -f "1,3"` over `a:b:c` == `a:c` (quoted idiom works — pin it); the
  parse-error hint for an unquoted comma mentions quoting (asserted in P4.3).

### [x] 2. `tail -n +N` (from line N) misread as `tail -n N` — FIXED 2026-06-17
**Fix:** `parse_line_spec` detects the `+`-prefixed String (the `+` survives
lexing; a bare `parse()` dropped it) → from-line-N semantics (skip N-1, `+1` =
whole input). Single-stream + multi-file. Tests in
`builtin_sweep_fidelity_tests.rs`. DeepSeek-reviewed.
**Discovered (DeepSeek, deferred):** the `-c` byte-count path on head/tail has
the SAME unsigned-wrap class — `head -c -1` casts `*i as usize` → `usize::MAX`.
Not in the consensus findings; natural next sweep item if we touch `-c`. Also
`tail +3`/`head +3` POSIX shorthand (without `-n`) lexes `+3` as a String the
shorthand handler skips (it only catches negative Int) — both pre-existing, out
of this fix's scope.

- **Symptom (TL2):** `tail -n +2` on 4 lines yields the **last 2** (`cherry,date`)
  instead of **from line 2** (`banana,cherry,date`). The `+N` "start at line N" form
  is silently treated as plain `N`.
- **Verdict:** SILENT-WRONG. **Fix:** parse a leading `+` as start-offset semantics.
- **Test:** `tail -n +2` and `tail -n +1` (== whole input).

### [x] 3. `head -n -N` (all but last N) unsupported → emits everything — FIXED 2026-06-17
**Fix:** consolidated head's 4 duplicated count-parsers into one
`Head::line_spec → (count, all_but_last)` that treats a negative Int / `-`-prefixed
String as all-but-last-N (was `*i as usize` wrap → giant `take`). Streaming
fast-path falls back to buffered when all_but_last (can't early-terminate).
Tests in `builtin_sweep_fidelity_tests.rs`. DeepSeek-reviewed.

- **Symptom (H2):** `head -n -1` emits all 4 lines instead of the first 3.
- **Verdict:** SILENT-WRONG (negative count ignored). **Fix:** support negative `-N`
  (all but last N), or fail loud if declared out of scope. Pin via panel if contested.
- **Test:** `head -n -1` == first N-1 lines.

### [x] 4. `cut` drops a line with no delimiter (`-f`, no `-s`) — FIXED 2026-06-17
**Fix:** non-delimited line passes through whole (`!line.contains(delim)` →
push line); added `-s`/`--only-delimited` to suppress. Field mode only (`-c`
unaffected). Tests in `builtin_sweep_fidelity_tests.rs`. DeepSeek-reviewed.
*Discovered (deferred):* `cut -d ''` (empty delim) isn't rejected → silently
becomes tab; pre-existing.

- **Symptom (C6):** `cut -d, -f2` on `nodelim` (no comma) emits **empty**; consensus
  passes the line through unchanged (GNU prints non-delimited lines unless `-s`).
- **Verdict:** SILENT-WRONG. **Fix:** print the whole line when the delimiter is
  absent and `-s` is not set.
- **Test:** `cut -d, -f2` over `nodelim\n` == `nodelim\n`; `cut -s …` still suppresses.

### [x] 5. `jq -c` (compact) ignored — pretty-prints — FIXED 2026-06-17
**Fix:** thread `compact` into `execute_filter_json` + `format_compact` (serde
`to_string`); also newline-terminate jq output (P4.1, folded). DeepSeek-reviewed.

- **Symptom (J4):** `jq -c .` emits multi-line pretty JSON (`{\n  "a": 1\n}`) instead
  of `{"a":1}`. The compact flag is not honored.
- **Verdict:** SILENT-WRONG (wrong shape for downstream parsing). **Fix:** honor `-c`.
- **Test:** `jq -c .` over `{"a":1}` == `{"a":1}` (+ trailing-newline pin, P4.1).

### [x] 6. `split --limit=N` off-by-one — FIXED 2026-06-17
**Fix:** `splitn(limit)` not `limit+1` (literal+regex); whitespace loop
`0..limit-1`. So `--limit=N` = at most N fields. Also emits trailing newline
(P4.1, folded here). Legacy direct-`.execute()` unit tests updated. Tests in
`builtin_sweep_fidelity_tests.rs`. DeepSeek-reviewed. *Discovered (deferred):*
`SplitArgs.limit`/`.regex` clap fields are dead (read from raw args) — works but
violates the read-from-parsed convention; pre-existing, separate cleanup.

- **Symptom (SP3):** `split : --limit=2` over `a:b:c:d` yields `a,b,c:d` (3 parts)
  instead of `a,b:c:d` (2 parts — first N-1 splits, remainder intact).
- **Verdict:** SILENT-WRONG. **Fix:** `--limit=N` = at most N fields (N-1 splits).
- **Test:** `split : --limit=2` over `a:b:c:d` == `a\nb:c:d`.

### [x] 7. `wc` output format: leading tab + no trailing newline — FIXED 2026-06-17
**Fix:** single-count = bare unpadded number + `
` (drop leading tab); multi-count
right-justified common-width, space-sep, newline-terminated (`format_counts_text`).
`wc -l` counts newlines (W5: `a
b`→1). DeepSeek-reviewed. One realworld wc test
updated off the old TSV format.

- **Symptom (W1/W2/W3/W5):** `wc -l/-w/-c` emit `\t<n>` (leading tab, no trailing
  `\n`) vs consensus `<n>\n`. Single-count form should be the bare number + newline,
  no leading whitespace.
- **Verdict:** SILENT-WRONG (format materially off). **Fix:** drop the leading tab for
  single-count; emit the trailing newline (see P4.1). Multi-count alignment pinned
  via panel.
- **Test:** `wc -l` over `a\nb\n` == `2\n`; `wc -w`, `wc -c` likewise.

### [x] 8. `tr -c` / `-cd` (complement) rejected by clap — FIXED 2026-06-17
**Fix:** added `-c`/`-C`/`--complement`; `in_set1` predicate inverts membership
across delete/translate/squeeze. `tr -cd '[:digit:]'` keeps only digits.
DeepSeek-reviewed (caught + fixed complement-translate squeeze using full SET2
when only set2.last() is emitted).

- **Symptom (T5/T7):** `tr -cd '[:digit:]'`, `tr -c …` → "unexpected argument '-c'".
  The complement flag is missing; common idiom `tr -cd '[:digit:]'`.
- **Verdict:** LOUD, misleading message. **Fix:** add `-c`/`-C`/`--complement` to tr
  (and ensure `-cd` bundling parses), or fail loud with a complement-specific hint.
- **Test:** `tr -cd '[:digit:]'` over `a1b2` == `12`; `tr -c '[:alnum:]' -d`.

---

## P3 — Coverage gaps

### [ ] 1. `sort -V` (version sort) unimplemented
- **Symptom (A7):** `sort -V` over `v1.10,v1.2,v1.9` → "unexpected argument '-V'".
- **Verdict:** LOUD, reasonable boundary but a reached-for form. **⛳ Decision (Amy,
  2026-06-17): implement version sort** (`-V`/`--version-sort`) — common for
  tags/semver, squarely in the 80%. Standard version comparison (numeric runs
  compared numerically). Pin against banked consensus.
- **Test:** `sort -V` over `v1.10\nv1.2\nv1.9` == `v1.2\nv1.9\nv1.10`.

---

## P4 — Doc / format-pin / message quality

### [ ] 1. Trailing-newline policy — EMIT (decided)
- **Cells:** `tac` (TC1), `wc` (P2.7), `split` (SP1/2/4), `jq` (J1/2/3 + J4),
  `base64` (B1/B3), `xxd -p` (X2), `dd|xxd` (D1/D2) all **omit** a trailing newline
  the consensus expects. Core line tools (head/sort/cut/grep/seq/printf) already emit
  it, so this is per-builtin, not systemic.
- **⛳ Decision (Amy, 2026-06-16): emit the trailing newline** to match consensus and
  kaish's own line tools, and avoid "missing final newline" surprises. Round-trips
  work either way (B4 MATCH), so the change is safe. Fold each fix into its builtin's
  commit (wc → P2.7; jq → P2.5; split → P2.6; tac/base64/xxd/dd get their own
  one-liners here).
- **Test:** one kernel-routed assertion per builtin that output ends in `\n`.

### [ ] 2. `patch` emits an extra "N changes applied" line on stdout
- **Symptom (PT1):** `patch` stdout is `patching file …\nN changes applied` vs
  consensus `patching file …\n`. The summary line is kaish-specific.
- **⛳ Decision (Amy, 2026-06-17): follow `patch(1)`** — 30 years stable. GNU
  patch emits `patching file …` on stdout and no "N changes applied" line, so
  **drop the kaish-specific summary line** (hunk failures still go to stderr as
  patch(1) does).

### [ ] 3. LOUD message quality
- Comma-parse message (P2.1) says "token pasting" when it's a comma — misleading even
  if the fix is declined.
- `tr -c` (P2.8) "unexpected argument '-c'" should hint complement support/scope.
- General: every LOUD boundary must hint the fix (PLAN ground rule #2). Eyeball each.

### [ ] 4. `diff` default = unified  (Decision #2 — doc only)
- Already implemented (`diff.rs`); the model-reflex "ed-style normal default" is a
  documented divergence, not a bug. **Confirm + document**; no code. Do not chase
  richer diff now.

### [ ] 5. Remove `rg`  (Decision #1 — breaking)
- Delete `rg.rs`, deregister, drop from help/`LANGUAGE.md`/README builtin lists;
  remove orphaned `crates/kaish-help/content/en/rg.md`, the `grep_engine.rs`
  "used by both" comment + its `#![allow(dead_code)]` (so newly-dead code surfaces),
  the `grep-pcre2` dep + `pcre2` feature in `kaish-kernel/Cargo.toml`, and
  `Cargo.lock`. Add a `CHANGELOG.md` **Removed** bullet listing the **accepted lost
  features** (`--type`/`--hidden`/`--max-count`/`--files`/`-P`/`--no-ignore`) — do
  NOT port them to grep first. The R1–R5 `(stdin):`-prefix findings are **moot**
  (removal), not fixes.

---

## Done when
Every box checked (or waived with a reason); `rg` removed; tee/patch latch+trash
gated; all CODE fixes landed with kernel-routed tests; corrections in help +
LANGUAGE.md + CHANGELOG shipped with each fix; `docs/builtin-sweep/` + this file
deleted; PR #7 green, ready, reviewed, merged.
