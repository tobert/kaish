# Known Issues & Open Work

Actionable punch list. Narrative context lives in [reviews.md](reviews.md).
Last validation pass: 2026-06-09 (Fable 5 systemic review — 61-agent fleet with
adversarial verification + DeepSeek/Gemini resonance panel; narrative in
[reviews.md](reviews.md), panel report in [resonance-2026-06.md](resonance-2026-06.md)).
Path note: the 0.8.0 crate split moved some cited files — `vfs/local.rs` →
`kaish-vfs`, host `ps` → `kaish-tools-host/src/ps.rs`, Tool/ToolCtx/KernelBackend
→ `kaish-tool-api`.

Priorities follow the convention from `reviews.md`:

- **P1** — high-leverage features and diagnostics
- **P2** — focused refactors
- **P3** — scheduler and infra
- **P4** — eventually

The old P0 from the Feb-2026 integration-test pass (5 concrete bugs:
unknown-command socket error, missing `$0`, alias concatenation,
subprocess capture, arithmetic token leak) **all validate as fixed** on
2026-04-16 and have been removed from this list.

---

## P1 — High-leverage features and diagnostics

### OverlayFs — ALL HUNKS LANDED 2026-06-10; residuals only
Hunk 1 `69c42e3`+`2a62a72` (MemoryFs lift, overlay core), accounting vfs half
`dddc85d` (resident_bytes, ByteBudget), hunk 2 `6fe2225` (inspection API),
hunk 3 + accounting kernel half `d0e0deb` (64 MiB mcp budget "vfs-memory",
MountInfo.resident_bytes + kaish-mounts, --overlay opt-in REPL/MCP/embedder,
kaish-vfs builtin status/diff/commit/reset). Residuals:
- **MCP overlay persistence**: MCP kernels are fresh per execute call, so an
  MCP overlay is a per-call transaction (commit in the same script). Cross-call
  overlays need a session map in kaish-mcp — revisit if a consumer wants it;
  would also unlock the originally-floated default-on for MCP. (P3)
- **External commands under overlay** fail with exit 127 inside the overlay
  mount (real_path=None, accidental but correct guard); cd /tmp escapes. A
  friendlier in-band error naming the overlay would help. (P3)
- **JobFs resident counting skipped deliberately**: synthesized fs over
  JobManager's already-bounded BoundedStream ring buffers — nothing unbounded
  to count. Revisit only if job buffers become configurable/unbounded. (P4)
- **kaish-mounts --json shape changed** (breaking): was a bare array, now
  {mounts, budget?}. Flag in next release notes. (P2 — release-notes item)
- **Punted in core** (revisit if a consumer hits them): cross-layer symlink
  resolution is layer-local; whiteout of a directory is per-file only;
  commit_into doesn't propagate mtimes. `VirtualOverlayBackend`
  (backend/overlay.rs) is unrelated prefix routing, not CoW.

### Silent lossy UTF-8 decodes in text builtins — MOSTLY FIXED 2026-06-13
The in-process text builtins used to `from_utf8_lossy` stdin/files, silently
replacing invalid bytes with `U+FFFD` (corruption that looks like success). Fixed
with a strict `ExecContext::read_stdin_to_text()` (errors on non-UTF-8) wired into
`grep`, `rg`, `sed`, `awk`, `sort`, `uniq`, `cut`, `tr`, `jq`, `tac`, `split`,
`scatter`, `gather`, `read`, `tokens`; strict file reads in `diff`/`patch` (point
to `cmp`); `grep` validates file bytes before its engine; `tail -c` joined the
byte-aware movers. Text tools now loud-error on binary; byte-aware tools
(`cat`/`dd`/`base64`/`xxd`/`checksum`/`wc`/`tee`/`head -c`/`tail -c`/`cmp`) consume
it. Regression tests in `sandbox_mode_tests`.

**External-command binary I/O — FIXED 2026-06-13.** External commands now keep
binary intact in both directions: stdin is forwarded raw (`env`/`spawn` read
`read_stdin_to_bytes`; the pipeline path already streamed raw), and captured
stdout becomes a `Bytes` result (text if valid UTF-8) at all three capture sites
— `kernel.rs::try_execute_external` (`BoundedStream::read()` not `read_string`),
`dispatch.rs` (`spill_aware_collect` returns `Vec<u8>`), and `env`/`spawn`. So
`curl url`, `curl url > file.bin`, `… | gzip`, `printf … | base64` all round-trip.
stderr stays text. The output limiter's binary spill/truncate (already
byte-aware) composes. `grep_engine`'s `from_utf8_lossy` is safe (grep validates
upstream). Residual: buffered-`String` stdin (`take_stdin`, heredoc/here-string)
is still text — binary there would need `ctx.stdin` to become bytes, rare.

**Bug found + fixed in the same pass:** `accumulate_result` (kernel.rs) folded
every top-level statement's result via `push_out(text_out())` — lossy-decoding
*any* `Bytes` final result (so even `cat blob.bin` or `echo ff | xxd -r -p`
standalone came back mangled, independent of external commands). Now concatenates
raw bytes when binary is involved.

**Round-2 DeepSeek + Gemini review (2026-06-13)** caught more silent-corruption
sites, all FIXED: `cat` as a pipeline's last stage (read_stdin_to_string),
`$()`/block/function-body capture (`execute_block_capturing` +
`execute_user_tool` now accumulate raw bytes → `$()` yields `Value::Bytes`),
command-subst inside a string (loud error), `kaish-last` (preserves bytes),
background-job output file (notes binary, doesn't dump lossy), `for` over a
`Bytes` value (loud error), `dd skip*bs` (checked_mul). Regression tests in
`sandbox_mode_tests`.

**Residuals (NOT silent corruption — visible/loud, deferred):**
- **Buffered-`String` stdin** (`ExecContext::stdin: Option<String>`): `< binfile`
  and binary here-strings/heredocs error (`from_utf8`) or stringify a
  `Value::Bytes` to the `[binary: N bytes]` placeholder. Making `ctx.stdin` hold
  bytes is the real fix; rare in practice.
- **Var interpolation** `"$x"` where `x` is `Value::Bytes` → `[binary: N bytes]`
  placeholder (visible marker, not lossy). Command-subst-in-string already
  errors; var-in-string could too if `value_to_string` grew a fallible path.
- **Output-limit spill preview**: the head/tail *preview text* in the spill
  notice still `from_utf8_lossy`s (U+FFFD); the spilled file itself is correct
  raw bytes. Cosmetic — switch the preview to a hex dump.
- **`cmp`/`cat`/… whole-file reads** OOM on huge inputs (general kaish model, not
  binary-specific); add a size cap if it bites.
- **`wc -l`** counts `str::lines()` not `\n` — overcounts by 1 with no trailing
  newline (pre-existing, unrelated to binary). **`wc -m`** on binary over-counts
  via U+FFFD (documented).

### Binary-data path — typed `Bytes`, `dd`, `/dev/urandom` — LANDED (residuals only)
Surfaced 2026-06-13 while adding the synthetic `/dev` (DevFs: `/dev/null`,
`/dev/zero` shipped). kaish is UTF-8 text end to end (`ExecResult.out: String`,
`OutputData` string-shaped, pipe consumed as text), so raw bytes can't transit
intact and `/dev/urandom` can't exist. **Design committed: typed `Bytes`
through pipes, nushell-style** — full plan in
[binary-data.md](binary-data.md). Spine: a `Value::Bytes`/`OutputData::Bytes`
that flows through pipes, **coerces to text iff valid UTF-8 (else loud error)**,
and renders at the boundary (REPL hex dump, `--json`/MCP structured base64).

All phases LANDED (commits `e612d69` → `818a22e`):
1. **Value + boundary** — `Value::Bytes` (single binary type; dead
   `Value::Blob`/`BlobRef` deleted), `ExecResult.out` as `OutputPayload::{Text,
   Bytes}` (wire-compatible serde), coercion rule (`text_out` lossy /
   `try_text_out` loud guard / `success_text_or_bytes` at producers), base64
   envelope + `hex_dump`, boundary rendering (REPL hex dump, `--json` envelope).
2. **Transit** — `read_stdin_to_bytes`; byte-clean pipe-write + `>`/`>>`/`&>`
   redirects; `accumulate_result` concats raw bytes (it was lossy-decoding every
   binary final result). `1>&2` of binary is a loud error.
3. **dd + devices** — `dd` (`if=…` parser fix), `/dev/urandom`/`/dev/random`.
   North-star green.
4. **Movers byte-aware** — cat, head -c, tail -c, base64, xxd, checksum, wc -c,
   tee, `cmp` (new). Text tools (grep/sed/awk/…) loud-error on binary, no lossy.
5. **External commands** — capture → Bytes, stdin forwarded raw (`818a22e`).

**Decision: no generic `encode`/`decode`** (2026-06-13) — `base64` + `xxd`
already bridge text↔bytes; a generic pair would duplicate them and invite a
basenc-style format×flag matrix. Add a flagless `urlencode`/`urldecode` only if
web work needs it. **No `random` builtin** (Amy): `dd if=/dev/urandom` covers it.
Residual: buffered-`String` stdin (`take_stdin`, binary heredocs — rare).

**Definition of done (north-star test):** `dd if=/dev/urandom of=/dev/null
bs=1024 count=10` exits 0 having copied exactly 10240 bytes; the same into a
`/tmp` file verifies an exact `wc -c`; two 16-byte draws have differing
checksums (entropy is real). Home: a NoLocal kernel-routed test beside the
DevFs tests in `tests/sandbox_mode_tests.rs`. DevFs:
`crates/kaish-vfs/src/dev.rs`; range plumbing: `Filesystem::read_range`.

### `rg` parallel walking
The 2026-04-29 rg builtin uses `ignore::WalkBuilder::build()`, which
yields a sequential iterator. `WalkParallel::run()` is a few lines'
diff plus careful synchronization (work-stealing crossbeam deques,
results merged across workers). High value on large trees — rg's
real-world advantage. Skipped this round to ship the feature; revisit
when single-thread perf bites.

### `rg` async stdin streaming
`rg.rs::run_with_matcher` reads stdin via `ctx.read_stdin_to_string()`
into a `String` and uses `Searcher::search_slice` on the bytes. Fine
for small/medium pipes; blocks the runtime worker on large pipes.
A `SyncPipeReader` adapter (sync `std::io::Read` over async
`PipeReader` via `Handle::block_on`) plus `Searcher::search_reader`
would stream chunks. Expected modest payoff; defer until a real
workload pushes against the buffer.

### Output disk-spill — runtime read-only residuals (core fix done 2026-06-06)
**Core fix landed 2026-06-06** (full narrative now in git history). `OutputLimitConfig`
carries a runtime `SpillMode` (`Disk` | `Memory`); `Memory` truncates in memory with
no disk I/O, and `Kernel::assemble` auto-forces `Memory` for `NoLocal` mounts.
Remaining open work:
- `Disk` is still the default for *mode-based* `localfs` kernels built via
  `Kernel::new` (`Sandboxed`/`Passthrough`) — those genuinely own their host
  mounts, so disk spill is legitimate there. The auto-force to `Memory` now also
  fires for **any `with_backend` kernel** (not just `NoLocal`): a custom-backend
  kernel owns no host mounts, so a host spill — or a background-job output file —
  is always a VFS bypass (fixed 2026-06-08, see git history; this closed the kaibo
  gap — kaibo uses `with_backend` and did *not* opt into `Memory`, contrary to a
  prior note here). Residual: a mode-based `Sandboxed` kernel an embedder runs
  read-only via `Kernel::new` without `.in_memory()` still spills; none known in
  the wild, and `with_backend` is the read-only embedder path.
- No runtime switch via the `kaish-output-limit` builtin / `set -o` yet — mode is
  config-only. Add a `memory`/`disk` subcommand if interactive control is wanted.
- `host`'s `/proc` and `/etc` reads still bypass the VFS by design — if
  "read-only" is ever marketed as "no host observation," those need a runtime
  gate too.

---

## P2 — Focused refactors & real bugs

### Streaming file reads — wc/checksum/grep/cmp/cat landed 2026-06-14; residuals
Scan-oriented builtins no longer read whole files into memory. Mechanism
(chosen to respect kaish-vfs's runtime-free trait — no `AsyncRead`/tokio in the
`Filesystem` trait, so WASI stays clean):
- `LocalFs::read_range` does a true positional `seek + take` for byte ranges
  instead of read-whole-then-slice. Bonus: this also fixed `head -c bigfile`,
  which previously slurped the whole file before slicing.
- `ExecContext::read_file_chunked(path, chunk_size, f)` pulls a file forward in
  `STREAM_CHUNK_SIZE` (256 KiB) windows via `read_range(bytes)` and feeds each
  chunk to a sync closure; bounded memory, EOF on first empty chunk. Builtins
  whose sink is async (cmp's lockstep, cat→pipe) can't use the closure form and
  run their own offset loop over `read_range(bytes)` instead.

Landed:
- `wc` — incremental line-buffered `WcCounter` (`a610044`).
- `checksum` — incremental `StreamHasher` (`770766c`).
- `grep` (single-file simple path) — `GrepLineScanner`, incremental UTF-8/binary
  detection, byte-identical to the whole-buffer path incl. rich `--json`
  (`0fce8c4`). Complex flags (-A/-B/-C/-c/-q/-l/-o) stay whole-buffer.
- `cmp` — lockstep two-file streaming with early exit on first difference
  (`dcb806c`); a `-` stdin operand keeps the whole-buffer path.
- `cat` (single-file, piped) — streams raw bytes to `pipe_stdout`, early-exits on
  broken pipe (`e220767`); terminal `cat` still materialises (must, to return).

Each landed conversion has a parity test against the **production whole-buffer
path** (not a self-comparison) across chunk-split points, plus a `RecordingFs`
bounded-read proof. The compare-against-production lesson came from a grep draft
that hardcoded `byte_offset=0`/`path=null` in `--json` and still passed a
text-only self-comparison.

Remaining:
- **`base64`** — deferred. Only a pipe-case win (output grows with input), the
  3-byte/4-char carry is the fiddliest of the set, and big-file base64 is
  unlikely in kaish. Revisit if a workload needs it.
- **Deliberately NOT streamed** (need all input or random access — leave whole-
  buffer): `sort`, `uniq` (global dedup), `diff`, `jq`, anything emitting
  structured `.data`. The `.data`/`OutputData` channel is whole-value by design
  (it powers `--json` and `for x in $(cmd)`); streaming only ever applies to the
  raw-byte path.
- **`read_range` re-opens the file per chunk** on LocalFs (open+seek per 256 KiB
  — affects `read_file_chunked` and the cmp/cat offset loops). Cheap relative to
  the read, but a stateful streaming handle would cut the syscalls. Deferred —
  would mean an `AsyncRead`-returning method, which reintroduces the
  tokio-in-trait problem the chunk approach avoided. Revisit only if a profile
  shows the re-opens matter.

### Kernel-routed port residuals: `read`, `env`, `exec` deep coverage
The realworld port is **done 2026-06-11** (see git history) — 48 tests through
`kernel.execute`, plus a new rg section; the port immediately caught the
grep/rg value-flag binding bug. Still worth dedicated kernel-routed suites:
`read` (pipeline/scope semantics are pure kernel interaction) and `env`/`exec`
(hermetic-env overlay, two-spawn-site sync) — each has a single smoke case in
the `--json` sweep now, but no flag-surface depth.

### Composable help — remaining phases
Phases 1–3 done 2026-06-06: the `kaish-help` crate (concept fragments + `compose`/
recipes + byte-stable `get_help`) is the single source for help content; `syntax.md`
is generated + drift-tested; and MCP `instructions:`, the REPL welcome, the `execute`
tool description, and the MCP prompt set all compose from it (no hand-rolled prose
left in the MCP frontend). Remaining:
- **Phase 4:** publish half **done 2026-06-08** (`kaish-help` 0.8.0 on
  crates.io); remaining: kaijutsu/kaibo adoption (tracked in those repos).
- **Phase 5:** i18n scaffolding + first `ja` fragments.

Full design + resolved decisions: [composable-help.md](composable-help.md).

### Split `kernel.rs::execute_stmt_flow`
`kernel.rs:1463`–~1913 (kernel.rs is now 6,838 lines) is a 16-arm async match.
Each arm reaches into `scope`,
`exec_ctx`, and `user_tools` RwLocks; `For`/`While`/`Case` are 100+
lines apiece. Natural refactor: `mod kernel/exec/{assignment, command,
pipeline, control, …}` with `execute_stmt_flow` reduced to a dispatch
arm-per-module.

### Extract `dispatch_command` ctx-sync helper
The six-field `ExecContext` ↔ kernel-state sync appears near every
call site that fields a fork (`kernel.rs:~4100-4140` and duplicates;
citation refreshed 2026-06-09). One helper, one truth.


### No unquoted token-pasting — adjacent bare lexemes are not one word
kaish's lexer/parser does **no token pasting**: a run of adjacent *unquoted*
lexemes is never concatenated into a single word. `/tmp/$(echo x).txt` lexes as
three tokens (`RelativePath` `/tmp/`, `CommandSubst $(…)`, `DottedIdent .txt`)
and `$dir/out.txt` as two (`SimpleVarRef`, path) — there is no word-assembly
step that joins them. The **quoted** form is the supported idiom and works
everywhere because a double-quoted string lexes as one `Interpolated` token:
`"/tmp/$(echo x).txt"`, `"$dir/out.txt"`. This aligns with the
`shellcheck --enable=all` north star, which already requires `"$var"`/`"$(cmd)"`
quoting (SC2086). **Decision (2026-06-08): keep the quoting requirement; do not
add bare word-pasting.** kaish is not aiming to be a human REPL (bash/zsh/fish
fill that niche) — the goal is to guide agents to write reliable scripts, and
"always quote interpolated words" is a simpler, lint-aligned rule than bash's
implicit pasting. Two surfaces expose the no-pasting behavior differently:

- **Redirect target** (`crates/kaish-kernel/src/parser.rs`, `redirect_parser`)
  binds with a single `primary_expr_parser()`, so `> /tmp/$(echo x).txt` is a
  hard parse error (`found '$(' expected redirect, …`). Bare-subst
  (`> $(echo /tmp/x)`) and quoted (`> "/tmp/$(echo x).txt"`) both work.
  *Polish (P4, deferred):* turn the parse error into a "quote the redirect
  target" hint instead of the generic expected-token list.
- **Argv** now also rejects glued positionals with a "quote the whole word"
  hint (fixed 2026-06-11 — see git history). Residual (P4): the check covers
  positional args *before* `--`; post-`--` positionals and a flag-adjacent-to-
  positional glue are not yet flagged (rare; the pre-`--` positional case is the
  documented bug class).

Surfaced 2026-06-08 while adding command-substitution support to redirect
targets.

---

## P3 — Scheduler and infra

### Code formatting (rustfmt) — considered and declined 2026-06-14
**Decision (Amy): not adopting rustfmt.** The audience for this code is Claude and
other agents, not human reviewers, so rustfmt's payoff (consistent visual style,
stable git blame, smaller PR diffs) is mostly cosmetic here; the compiler enforces
correctness and `cargo clippy --all --all-targets` now enforces the lints that
actually affect clarity. Against that thin benefit, a first `cargo fmt --all`
rewrites **~202 files / ~2127 hunks** (rustfmt 1.8.0-stable, edition 2024) and no
small config shrinks it meaningfully — measured: `use_small_heuristics` Off/Max
both make it *worse* (2261/2214), `max_width=80` far worse (3247), `max_width=120`
only ~8% better (1966) at the cost of wider lines everywhere; the knobs that would
preserve the hand-style (`chain_width`, `imports_granularity`, `group_imports`)
are nightly-only and can't back a stable gate. So **no `rustfmt.toml`** is added
(a dormant one would make an ad-hoc `cargo fmt --check` fail confusingly). If this
is ever revisited, it must be its OWN commit (never bundled with a feature) and
pin `edition = "2024"` (bare `rustfmt` defaults to 2015; `cargo fmt` already reads
2024 from Cargo.toml).

### `GlobPath::walk_match` globstar recursion has no work bound
`walk_match` (and the older `match_segments` it parallels) backtrack on globstar
with no `MAX_MATCH_CALLS` guard — unlike the single-component `match_bounded` in
`glob.rs`, which caps at 100_000 calls for ReDoS safety. Calls are bounded by
`C(n+g-1, g)` for `g` collapsed globstars and `n` path components: polynomial in
real filesystem depth, but an adversarial pattern (`a/**/b/**/c/**/…`) against a
deep path could blow up. Practical risk is low today — the walker only matches
against real FS paths, and this is **not a regression** (`match_segments`/`matches()`
already share the unbounded shape). If kaish ever matches user-supplied patterns
against user-supplied path *strings*, add a call counter to `walk_match`.
Surfaced by DeepSeek review of the dotfile-glob fix (2026-06-14).

### `kill -<sig>` bash shorthand (`kill -9 %1`, `kill -STOP %1`) isn't accepted
`kill` takes the signal via `--signal NAME` / `-s NAME` only; the bash idioms
`kill -9 %1`, `kill -KILL %1`, `kill -STOP %1` fail at clap arg parsing (`-9`
lexes as `Int(-9)`, `-STOP`/`-KILL` as a `ShortFlag`, neither a declared flag).
Job-control-heavy users reach for these constantly. Fix: give `kill` bespoke
argv handling (à la `set.rs`) that strips a leading `-<signum>` / `-<SIGNAME>`
token and maps it to the signal before clap sees the rest. The signal *delivery*
mechanism is already in place (see git history `kill %N` entry); this is purely the
front-door syntax. **Demoted P2→P3 2026-06-11 (Amy):** the `-<signum>` shorthand
leans POSIX-y and starts to touch underlying OS signal-number variance; the
loud `--signal NAME %N` form already covers the need. Defer.

### Process-group kill still has a (small) PID-reuse window
The direct-child kill path now uses `pidfd_send_signal` on Linux (see
`crates/kaish-kernel/src/pidfd.rs`), so the bound generation immunises
us from PID reuse for the spawned child itself. **But** the PG-wide kill
that catches grandchildren still goes through `killpg(pgid, sig)` —
there is no PGID equivalent of pidfd. If the leader is reaped and its
PID/PGID is reused before our `killpg` fires, grandchildren of an
unrelated process group could be signalled. Mitigations: cgroup v2
`cgroup.kill` (Linux ≥ 5.14) for atomic tree kill, or
`PR_SET_CHILD_SUBREAPER` to enumerate descendants and pidfd each. Both
are significant complexity; defer until we see a real failure.

### Non-Linux unix targets (macOS, BSD) keep PID-based kill
`pidfd` is Linux-only. On other unix targets we still send `kill(pid,
sig)` and accept the PID-reuse race for the direct child. macOS has no
direct equivalent (FreeBSD has `procctl`/`pdfork` but the model differs).
Acceptable today since kaish runs predominantly on Linux.

### Sync `build_tool_args` lacks the undeclared-space-flag guard
The async `build_args_async` (kernel.rs) fails loud when an undeclared flag under
a `map_positionals` schema would silently divorce a space-form value (fixed
2026-06-08, see git history). The sync twin `build_tool_args`
(`scheduler/pipeline.rs:705`, `unwrap_or(true)` at `:822`; citation refreshed
2026-06-09) shares the same `unwrap_or(true)` default-to-bool
and has **no** equivalent guard. It can't trigger the production bug today — its
only production callers are scatter/gather option parsing (builtin schemas,
`map_positionals=false`); the backend-tool caller is the `#[cfg(test)]`
`BackendDispatcher`. Applying the guard would require changing the return type to
`Result` across ~22 call sites, so it was deferred. If the sync path ever fields a
`map_positionals` backend tool in production, add the same guard (lift a shared
helper) to keep the two builders in sync.

The two builders have since drifted further: the glued short-flag value handling
(`cut -f1`, `head -c5`) added 2026-06-14 lives only in `build_args_async`, so a
glued short flag on a sync-path builtin (e.g. a hypothetical `scatter -n5`) would
not bind. Same low-impact-today reasoning (sync callers are scatter/gather option
parsing), same fix: lift a shared helper, or fold into the twin-elimination below.

### Eliminate the sync `build_tool_args` twin entirely
Bigger than the guard above: retire the sync arg builder so there's one path to
reason about. Real commands already bind through the async `build_args_async`
(foreground via `execute_command`; in pipelines via `Kernel::dispatch` →
`dispatch_command`). The sync `build_tool_args` survives only in (a) scatter/gather
*option* parsing in `run_scatter_gather` (`scheduler/pipeline.rs`, an `async fn`
that already holds a `&dyn CommandDispatcher`, so it could `await
dispatcher.eval_expr`) and (b) the `#[cfg(test)]` `BackendDispatcher`, plus ~27
in-module test call sites. Eliminating it means giving the scheduler an async
arg-builder (e.g. a `CommandDispatcher::build_args` trait method, or a free
`build_tool_args_async` that routes `$(...)` through `eval_expr`) and converting
those test sites to async. Deferred 2026-06-08 while landing per-subcommand tool
schemas — `select_leaf` lives only in the async builder, so the twin never sees a
subcommand tool and routing isn't at risk; this is purely a "fewer paths to
reason about" cleanup. Doing it would also subsume the guard-parity item above.

### Undeclared space-flag guard covers long flags only (`-t val` still divorces)
The 2026-06-08 fix errors on undeclared `--type value` but not single-char
`-t value`, because short flags followed by a positional (`-r path`, `-f file`)
are overwhelmingly legitimate bool+positional and erroring there would be too
aggressive. Net: an undeclared short flag that *should* take a value still
silently drops it under a `map_positionals` schema. Lower-risk than the long-flag
case (backend/MCP tools rarely expose single-char value flags) and the real fix
is the same — declare the flag in the schema. Revisit if a backend tool surfaces a
`-x VALUE`-style flag.

### `dispatch_command` cancel sync is one-way (in only)
`ec.cancel = ctx.cancel` is synced INTO `self.exec_ctx` at the start of
each dispatch, but there's no `ctx.cancel = ec.cancel` reverse sync at
the end. Today this is fine because the only known mutators of
`ctx.cancel` (the `timeout` builtin) save and restore the original token
themselves. If we add another builtin that wants to propagate a token
swap *outward* through dispatch_command, it would need either explicit
restoration or a reverse sync added.

### Test-effectiveness residuals (2026-06-09 fleet review)
Smaller verified gaps from the systemic review, grouped; the headline test
items (destructive rails, realworld port, `--json` sweep) are all closed as
of 2026-06-11 — see git history. Five smaller bullets also fixed 2026-06-11
(snapshot-isolation race, both tautological asserts, external argv no-split,
kill/wait e2e). Each remaining bullet is independently actionable:
- **`builtin_kernel_tests.rs` happy-path gap — PARTIALLY FIXED 2026-06-14:** added
  6 negative cases (cat/wc/sort missing file → exit 1 + named file, cut missing
  -f/-c → exit 1, head unknown flag → clap exit 2, grep no-match → exit 1), each
  pinning the specific code + err substring. More per-builtin negatives still
  welcome, but the "zero nonzero-exit assertions" state is gone.
- **Bare-glob validation backstop is single-site:** the bare-glob validation
  tests at `validation_tests.rs:321-391` are environment-dependent
  (transient-kernel cwd) — backstopped by the deterministic inline
  `test_bare_glob_no_matches_errors` (`kernel.rs:6135`), but that test only
  covers the builtin-argv site; the `execute_stmt_flow` and `build_args_flat`
  "no matches" sites lack their own, and the inline module is feature-gated
  such that `cargo test -p kaish-kernel --lib` alone skips it.
- **Lexer negative tests assert only `is_err` — FIXED 2026-06-14:** float and
  unterminated-string cases now assert the exact `LexerError` variant via
  `run_lexer_error_variant`; the ambiguous-boolean cases match the
  `AmbiguousBoolean(_)`/`AmbiguousBooleanLike(_)` family via
  `run_lexer_error_matching`. Tightening surfaced a real **diagnostic gap**: an
  unterminated double-quoted string (`"unterminated`) never matches the logos
  `String` regex, so it emits the generic `UnexpectedCharacter`, not the curated
  `UnterminatedString` (which only the complete/interpolated-string helper
  reaches). The test pins the *actual* variant with a comment; improving it to
  `UnterminatedString` needs a logos fallback rule on the string token (P4).
- **7 never-emitted `IssueCode` variants — REMOVED 2026-06-14, then 3 re-added
  with emitters:** E006 sed, E007 jq, E010, E011 diff, W003, W004, W005 were
  deleted from `kaish-tool-api/src/issue.rs` as silent aspiration (advertised
  validations that didn't exist). E011 (diff), E006 (sed), and E007 (jq) have
  since been re-added *with* real `Tool::validate` emitters + tests, honoring the
  no-variant-without-an-emitter rule; E010/W003/W004/W005 stay retired (gaps
  documented in `code()`).
- **bg/fg coverage is PTY-only** (unix-gated, timing-sensitive,
  `pty_job_control.rs`). The `wait`/`kill %1` half of this bullet closed
  2026-06-11 (see git history); bg/fg still have no non-PTY coverage.
- **Timing margins:** `background_job_does_not_block_foreground`
  (`concurrency_tests.rs:159`, 300ms headroom — the file's only hard
  wall-clock bound) and bare 5-10ms registration sleeps in `job.rs` unit
  tests (`:719/:738/:759`) — poll instead.
- **Bash-compat leg never runs automatically:** with `KAISH_BASH_COMPAT=1`,
  71+27 bash-side tests pass (verified) — but nothing sets it and there is no
  CI workflow at all, so the divergence detector depends on someone
  remembering. (CI is deliberately deferred per project memory; recorded here
  so the deferral stays a decision, not a drift.)

### Test gaps around kill discipline
- No test for `kill_grace = Duration::ZERO` (immediate SIGKILL).
- No test that a user-defined tool (`tool name { ... }`) inside a
  scatter worker dispatches correctly under cancellation — current
  scatter test uses an external `bash -c "sleep 60"`.
- No test for the JC inherit-output path being killed via cancel —
  that path requires a real TTY which is awkward in test infra.

### `ExecuteOptions` callback type is awkward
`Option<&mut (dyn FnMut(&ExecResult) + Send)>` is hard to call. A
trait-object alias or a dedicated `Callback` trait would smooth the
ergonomics. Not blocking; do it once we see real downstream pain.

### Long-blocking builtins other than `sleep` may not honor cancellation
`sleep` is done (see git history 2026-06-07). The guidance stands for any *other*
builtin that holds a long time/IO future without yielding through `ctx.cancel`:
audit and apply the same `tokio::select!` pattern where one is found (none
currently known besides the now-fixed `sleep`).

### `JobManager::spawn` busy-waits
Uses `std::hint::spin_loop()` to guarantee immediate visibility. Works,
wastes CPU on contention. Channel-based coordination would be cleaner.

### MCP per-request OS thread + 16MB stack
`crates/kaish-mcp/src/server/execute.rs:192` documents it. Fine at
agent-call rates, will be a problem in a hot loop. Replace with a pool
of LocalSet workers + mpsc channel. Benchmark first to confirm the win.

### MCP resource watcher channel fixed at 256
`subscriptions.rs:33` bounds the file-watch channel; high-churn
environments drop events silently.

### `ToolCtx::backend()` forces a full `KernelBackend` mock for out-of-tree tests
`crates/kaish-tool-api/src/ctx.rs`. `backend()` returns a non-optional
`&Arc<dyn KernelBackend>`, so a third-party tool author who wants to unit-test
their tool must construct a complete `KernelBackend` mock (~16 async methods)
even if the tool never touches I/O. We sidestepped this in-tree by relocating
the git/host tool tests to kernel-routed integration tests — which doesn't help
external authors. Surfaced by the dpal review 2026-06-03. Options: (a) ship a
`#[cfg(feature = "test-util")]` no-op `KernelBackend` + `ToolCtx` harness from
`kaish-tool-api` so any tool crate can spin up a context in one line; (b) make
`backend()` return `Option<&Arc<dyn KernelBackend>>` (kernel always `Some`,
pure-compute/test contexts `None`). (a) is less invasive and keeps the common
path honest. Revisit when the first external tool bundle wants unit tests.

---

## P4 — Eventually

### Control structures inside `$()` are not supported
The `$()` body accepts the full *statement* grammar — pipelines, `&&`/`||`
chains, `;`/newline sequences, `#` comments (landed 2026-06-11, see git history) —
but **not** `if`/`for`/`while`/`case`. The chain/sequence grammar is built
locally in `cmd_subst_parser`; wiring control structures in would require
threading the recursive `stmt` parser through ~17 expression call sites
(`primary_expr_parser`/`expr_parser` and their callers), a much larger parser
refactor. `$(if …; then …; fi)` is exotic in command-substitution position;
revisit if a real workload needs it. Workaround: compute at statement level and
capture the variable.

### Soften the "sh subset that passes shellcheck" framing
CLAUDE.md (and the README) describe kaish as "a `sh` subset that passes
`shellcheck --enable=all`." That framing is now more aspirational than accurate
and undersells what kaish actually is. The skeleton is sh-shaped — `if/then/fi`,
`for…in…do/done`, `case…esac`, `NAME=value`, `$()`, `$(())`, `${VAR:-}`, pipes,
heredocs — but:

- The dominant test form is `[[ ]]` (LANGUAGE.md:173), which is a bash-ism, not
  POSIX: shellcheck flags it **SC3010** under `sh` dialect. kaish then extends it
  past bash too (quoted-regex `=~ "\.rs$"`, `!~`). Here-strings `<<<` are bash
  (SC3011).
- Core shell *semantics* are deliberately dropped: word splitting, `eval`,
  backticks, process substitution.
- Typed data (floats, strict booleans, JSON), structured `$()`, the `split`
  builtin, E-code diagnostics, and the proposed collections
  ([arrays-and-hashes.md](arrays-and-hashes.md)) are modelled by **no** shellcheck
  dialect at all.

Net: kaish is neither a strict POSIX-`sh` subset nor a bash subset. "Passes
`shellcheck --enable=all`" holds only for the genuinely-shared core and only under
`bash` dialect. Recommend reframing the docs to something like *"inspired by POSIX
sh and bash, and informed by shellcheck's lints"* rather than implying compliance.
Corollary worth stating in the same place: shellcheck offers **zero** coverage for
kaish's extensions by construction, so the kaish validator is the sole safety net
for them (e.g. discarded-`append`, `len`/`in` type mismatches). One nice point in
kaish's favour — bare `$arr` = whole value is exactly the bash footgun shellcheck
warns about with SC2128 ("expanding an array without an index"), designed out.

### `mktemp` random suffix has slight modulo bias
`random_suffix` (`tools/builtin/mktemp.rs`) maps random bytes onto a 36-char
alphabet with `byte % 36`. Since `256 % 36 = 4`, bytes 252–255 land on the first
four chars, giving them ~3.1% vs ~2.7% probability. Negligible for temp-file
suffixes — the 36^N search space is dominated by N, not the per-char skew — so
not worth the rejection-sampling loop (which also complicates the
fail-loud-on-no-entropy contract). Flagged by dpal review 2026-06-03; recorded
rather than fixed by deliberate choice.

### `uname -v` discloses build provenance unconditionally
`tools/builtin/uname.rs` formats the version field as
`kaish {version} ({git_hash} {build_date})` from compile-time `option_env!`
values. If an embedder sets `KAISH_GIT_HASH`/`KAISH_BUILD_DATE`, `uname -v`
fingerprints the exact commit and build time even in a minimal build. Not host
info, but a build fingerprint; gate behind a `verbose-identity`-style feature if
a threat model cares. Flagged by dpal review 2026-06-03.

### `mktemp` entropy-failure message is unhelpful on wasm
On `wasm32-wasip1`, a `getrandom::fill` failure surfaces an opaque
`getrandom::Error` whose `Display` may be near-empty, so the
`mktemp: could not obtain system entropy: {e}` message loses its detail. Minor;
add a `cfg!(target_arch = "wasm32")` hint to the message if it ever matters.
Flagged by dpal review 2026-06-03.

### `touch <dir>` on a local mount fails (EISDIR); memory mounts succeed
Hardening on 2026-06-07 routed `touch`-on-existing through the VFS via a new
`set_mtime` op (Filesystem + KernelBackend), removing the old
`resolve_real_path` escape to the host and its silent no-op on virtual mounts
(read-only mounts now reject loudly; `MemoryFs` updates its per-entry mtime).
Residual: `LocalFs::set_mtime` opens the path with `write(true)` (preserving the
old `update_mtime` semantics), so `touch existing_directory/` fails with EISDIR
on a local mount, while `MemoryFs::set_mtime` updates a directory's mtime fine.
POSIX `touch` works on directories. Fix is to bump the time via `utimensat`/an
O_RDONLY fd (works on dir fds; on Linux futimens needs only ownership/write
perm, not a write-mode handle) instead of opening for write — deferred to keep
the cross-platform `File::set_modified` contract unchanged for now.

### Recursive/tree `--json` carries structure but no size/type metadata
`tree --json DIR` and `ls -R --json DIR` (without `-l`) serialize the
directory *shape* but every leaf value is `null` — there is nowhere in the
tree JSON shape (`OutputData::to_json`'s nested-object branch, `name → children`)
to hang per-node metadata, and `tree` never collects sizes in the first place.
So neither can produce a recursive "rip of the filesystem + sizes" on its own.
`ls -lR --json` *does* now carry sizes (the table-with-children path, fixed
2026-06-14 — see git history), but the bare-tree shapes don't. Working alternative
today: `for f in $(find DIR -type f); do stat -c '%s %n' "$f"; done`. Proper
fix is a shape decision — either teach `tree`/recursive-`ls` to emit a
table-with-children (so the metadata-bearing serializer applies) or extend the
tree-JSON branch to carry a metadata slot per node. Surfaced 2026-06-14 while
probing fs-rip ergonomics. (P3)

### `--flag=true` on bool flags rejected for builtins that skip flagify
Sibling of the `spawn --command true` fix above, surfaced by the DeepSeek
review. The explicit `seq --json=true` / `find --type=…`-style `=true` form on a
**bool** flag binds into `named` as `Bool(true)` (the `Arg::Named` branch, which
skips the `LongFlag` bool check), and the ~20 builtins that never call
`flagify_bool_named` (seq, find, stat, cp, mkdir, pwd, basename, …) hand clap a
`--json=true` their plain-`bool` field rejects (`seq --json=true` → exit 2
"unexpected value 'true'"). The bare `seq --json` form works. Two routes: call
`flagify_bool_named(&schema)` in every clap builtin (tedious, easy to forget —
this is exactly how the gap arose), or — better — flagify **once in the kernel**
before dispatch (the kernel already builds the schema at `kernel.rs:~2699`) and
drop the per-builtin calls, giving one path. The global `--json` stripper also
misses the `=true` form because it scans `flags`, not `named`. (P3)

### Extract `skip_quoted_content()` shared by arithmetic/heredoc preprocessing
`preprocess_arithmetic()` (~150 lines) and `preprocess_heredocs()`
(~150 lines) both implement quote/escape tracking. Also a dependency
of the heredoc span-tracking work under P1.

### `parse_interpolated_string` is ~200 lines
Handles `$VAR`, `${VAR}`, `$(…)` with nested paren tracking. Could
split into smaller helpers for maintainability.

### chumsky alpha → 1.0 before kaish 1.0
Updated 2026-06-09: chumsky moved to crates.io `1.0.0-alpha.8` in `229f363`
(release prep) — the old "pin a git commit" option is moot. Residual: upgrade
to a stable chumsky 1.0 when it ships, before kaish 1.0. (The stale
`LANGUAGE.md` "sourced from git" line was fixed 2026-06-11.)

### REPL polish
Syntax highlighting (chumsky already produces structured tokens),
abbreviation expansion, multi-line paste detection. `kaish-repl`
highlighter and hinter are currently no-ops; multi-line paste
re-validates on each keystroke.

### Bash-prior "did-you-mean" validator pass
Agents will write `[ -z "$VAR" ]` (single bracket) when `[[ ]]` is
preferred. The biggest historical bash-prior trap — `for i in $LIST`
silently iterating once — is now caught: bare `$VAR` in for-position
is E012 hard error, and `$(cmd)` in for-position splits on `\n`. What
remains is "soft" guidance for less-common ports; a shellcheck-style
actionable warning without rejection would steer without frustrating.
Optional but moderate leverage now that the loud cases are handled.
