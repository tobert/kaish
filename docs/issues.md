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

### Silent lossy UTF-8 decodes in text builtins — corruption risk
Surfaced 2026-06-13 plumbing binary. ~18 text-processing builtins read stdin via
`read_stdin_to_string` (which does `from_utf8_lossy`) or `from_utf8_lossy` on file
content: `grep`, `rg`, `sed`, `awk`, `sort`, `uniq`, `cut`, `tr`, `jq`, `tac`,
`split`, `scatter`, `gather`, `read`, `diff`, `patch`, `env`, `tokens`. Handed
binary, they **silently replace invalid bytes with `U+FFFD`** — corruption that
looks like success (e.g. `grep x < blob`, `sed … < image.png`). The binary movers
(`cat`/`dd`/`base64`/`xxd`/`checksum`/`wc`/`tee`/`head -c`/`cmp`) were made
byte-aware 2026-06-13; these text tools were deliberately left for a dedicated
sweep. **Fix:** per the coercion rule (`docs/binary-data.md`), a text tool handed
non-UTF-8 input should **loud-error** ("input is binary, not text — pipe through
`base64`/`xxd`"), not lossy-decode. Likely a strict `read_stdin_to_text()` that
errors on invalid UTF-8 (vs the lossy `…_to_string`), plus auditing the
`from_utf8_lossy` file-read sites. Net new helper exists: `read_stdin_to_bytes`.
Worth doing — silent corruption violates "crashing is preferred over data
corruption."

### Binary-data path — typed `Bytes`, `dd`, and `/dev/urandom`
Surfaced 2026-06-13 while adding the synthetic `/dev` (DevFs: `/dev/null`,
`/dev/zero` shipped). kaish is UTF-8 text end to end (`ExecResult.out: String`,
`OutputData` string-shaped, pipe consumed as text), so raw bytes can't transit
intact and `/dev/urandom` can't exist. **Design committed: typed `Bytes`
through pipes, nushell-style** — full plan in
[binary-data.md](binary-data.md). Spine: a `Value::Bytes`/`OutputData::Bytes`
that flows through pipes, **coerces to text iff valid UTF-8 (else loud error)**,
and renders at the boundary (REPL hex dump, `--json`/MCP structured base64).

Phases (each independently shippable):
1. **DONE** — Value + boundary: `Value::Bytes` (single binary type; the dead
   `Value::Blob`/`BlobRef` were deleted), `ExecResult.out` as an
   `OutputPayload::{Text,Bytes}` enum (wire-compatible serde), coercion rule
   (`text_out` infallible + lossy / `try_text_out` loud guard), base64 +
   `hex_dump` helpers, boundary rendering (REPL hex dump, `--json` envelope).
   `OutputData::Bytes` deferred to Phase 2.
2. Transit: the pipe ring buffer is already `VecDeque<u8>` — the work is at
   *consumption*: a `read_stdin_to_bytes` sibling (26 `…_to_string` callers), a
   `bytes_out()` pipe-write path, and killing `from_utf8` in `head`/`cat`/`<`.
3. **dd + urandom DONE** — `dd` builtin (`if=`/`of=`/`bs=`/`count=`/`skip=`,
   256 MiB cap, reads via `read_range`) + `/dev/urandom`/`/dev/random` in `DevFs`
   (`getrandom`). Parser `if=` blocker fixed. North-star green. Remaining:
   `encode`/`decode`, realign `base64`, a `random` builtin.
4. As demand appears: `gzip`/`gunzip`, blob helpers.

**North-star test PASSES** (`sandbox_mode_tests`): `dd if=/dev/urandom
of=/dev/null bs=1024 count=10` copies exactly 10240 B; file round-trip readback
is exact; two draws' checksums differ. Remaining Phase-2 transit work (byte-clean
pipe consumption: `cat blob | xxd`) is still open. DeepSeek review notes
(`OutputPayload`/`Blob`/coercion) in binary-data.md.

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
**Core fix landed 2026-06-06** (full narrative now in Resolved). `OutputLimitConfig`
carries a runtime `SpillMode` (`Disk` | `Memory`); `Memory` truncates in memory with
no disk I/O, and `Kernel::assemble` auto-forces `Memory` for `NoLocal` mounts.
Remaining open work:
- `Disk` is still the default for *mode-based* `localfs` kernels built via
  `Kernel::new` (`Sandboxed`/`Passthrough`) — those genuinely own their host
  mounts, so disk spill is legitimate there. The auto-force to `Memory` now also
  fires for **any `with_backend` kernel** (not just `NoLocal`): a custom-backend
  kernel owns no host mounts, so a host spill — or a background-job output file —
  is always a VFS bypass (fixed 2026-06-08, see Resolved; this closed the kaibo
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

### Kernel-routed port residuals: `read`, `env`, `exec` deep coverage
The realworld port is **done 2026-06-11** (see Resolved) — 48 tests through
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

### Minimal build (`--no-default-features`) — integration test binaries don't compile
(The lib-unit-test regression in this entry is **fixed 2026-06-10** — see
Resolved; the `--lib --no-default-features --no-run` gate is now in the build-
command list. Consequence still standing: `sandbox_no_native_builtins`
(`sandbox_mode_tests.rs:217-229`) only compiles in the minimal config, so it's
exercised only when someone runs the minimal gate, and the sandbox config has no
CI.)
Still open: the *integration* test binaries
(`tests/*.rs`) don't compile minimally. 8 of 23 files use `KernelConfig::repl()` /
the `kernel_at` harness, both `#[cfg(feature = "localfs")]` (real-FS) — so `cargo
test -p kaish-kernel --no-default-features` (which builds the integration binaries
too) fails to compile those. Options: (a) file-level `#![cfg(feature = "localfs")]`
on the inherently real-FS binaries so they're skipped minimally; (b) convert the
ones that don't actually need real FS (e.g. `validation_tests`) to
`KernelConfig::isolated()` (memory VFS) so they *run* minimally — strictly better
coverage. `hermetic_home_tests.rs` already uses `isolated()` and runs in both modes
as the pattern to follow. Surfaced 2026-06-03; partially resolved 2026-06-07. Folds
naturally into the planned `native`→capability-feature split.

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

### Glued short-flag values rejected on clap-based coreutils (`cut -f1`, `head -c5`)
Surfaced 2026-06-13 wiring the `dd`/binary tests: `cut -d ' ' -f1` errors with
`unexpected argument '-1'` — the clap-derived arg layer doesn't accept a value
glued onto a single-char short flag (`-f1`, `-c5`, `-n3`). The separated form
(`-f 1`, `-c 5`) works. This breaks deep coreutils muscle memory — `cut -f1`,
`head -c5`, `tail -c20`, `cut -d, -f1,3` are all idiomatic POSIX and what both
agents *and* humans reflexively type. Likely a clap config gap (short flags need
`num_args`/`require_equals` tuning, or a pre-parse split of `-fVALUE` →
`-f VALUE` in the argv layer). Audit the clap-migrated builtins (`cut`, `head`,
`tail`, `grep -A/-B/-C`, `base64 -w`, …) and either configure clap to accept
glued values or normalize in `to_argv`/`build_args`. Pairs with the existing
`gotcha_clap_parsed_vs_raw_args` lessons. Worth doing — the glued form is the
common idiom, and silently erroring on it is a sharp edge.

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
  hint (fixed 2026-06-11 — see Resolved). Residual (P4): the check covers
  positional args *before* `--`; post-`--` positionals and a flag-adjacent-to-
  positional glue are not yet flagged (rare; the pre-`--` positional case is the
  documented bug class).

Surfaced 2026-06-08 while adding command-substitution support to redirect
targets.

---

## P3 — Scheduler and infra

### `kill -<sig>` bash shorthand (`kill -9 %1`, `kill -STOP %1`) isn't accepted
`kill` takes the signal via `--signal NAME` / `-s NAME` only; the bash idioms
`kill -9 %1`, `kill -KILL %1`, `kill -STOP %1` fail at clap arg parsing (`-9`
lexes as `Int(-9)`, `-STOP`/`-KILL` as a `ShortFlag`, neither a declared flag).
Job-control-heavy users reach for these constantly. Fix: give `kill` bespoke
argv handling (à la `set.rs`) that strips a leading `-<signum>` / `-<SIGNAME>`
token and maps it to the signal before clap sees the rest. The signal *delivery*
mechanism is already in place (see Resolved `kill %N` entry); this is purely the
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
2026-06-08, see Resolved). The sync twin `build_tool_args`
(`scheduler/pipeline.rs:705`, `unwrap_or(true)` at `:822`; citation refreshed
2026-06-09) shares the same `unwrap_or(true)` default-to-bool
and has **no** equivalent guard. It can't trigger the production bug today — its
only production callers are scatter/gather option parsing (builtin schemas,
`map_positionals=false`); the backend-tool caller is the `#[cfg(test)]`
`BackendDispatcher`. Applying the guard would require changing the return type to
`Result` across ~22 call sites, so it was deferred. If the sync path ever fields a
`map_positionals` backend tool in production, add the same guard (lift a shared
helper) to keep the two builders in sync.

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
of 2026-06-11 — see Resolved. Five smaller bullets also fixed 2026-06-11
(snapshot-isolation race, both tautological asserts, external argv no-split,
kill/wait e2e). Each remaining bullet is independently actionable:
- **`builtin_kernel_tests.rs` is 100% happy-path:** 38 tests, zero nonzero-exit
  assertions. Add 1–2 negative cases per builtin (missing file, bad flag)
  asserting the specific code + err substring — agents branch on exit codes.
- **Bare-glob validation backstop is single-site:** the bare-glob validation
  tests at `validation_tests.rs:321-391` are environment-dependent
  (transient-kernel cwd) — backstopped by the deterministic inline
  `test_bare_glob_no_matches_errors` (`kernel.rs:6135`), but that test only
  covers the builtin-argv site; the `execute_stmt_flow` and `build_args_flat`
  "no matches" sites lack their own, and the inline module is feature-gated
  such that `cargo test -p kaish-kernel --lib` alone skips it.
- **Lexer negative tests assert only `is_err`** (`lexer_tests.rs:160`
  `run_lexer_error_test`): the float/ambiguous-boolean/unterminated suites
  would still pass if the curated diagnostics regressed to a generic error.
  Thread an expected variant/substring through the rstest cases (the inline
  `backtick_in_source_is_rejected` shows the pattern).
- **7 of 20 `IssueCode` variants are never emitted** (E006 sed, E007 jq, E010,
  E011 diff, W003, W004, W005 — `kaish-tool-api/src/issue.rs`): the advertised
  validations don't exist, so deleting them fails zero tests. Only grep and
  seq implement `Tool::validate`. Either implement + test per code, or remove
  the dead variants (fail-loud over silent aspiration).
- **bg/fg coverage is PTY-only** (unix-gated, timing-sensitive,
  `pty_job_control.rs`). The `wait`/`kill %1` half of this bullet closed
  2026-06-11 (see Resolved); bg/fg still have no non-PTY coverage.
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
`sleep` is done (see Resolved 2026-06-07). The guidance stands for any *other*
builtin that holds a long time/IO future without yielding through `ctx.cancel`:
audit and apply the same `tokio::select!` pattern where one is found (none
currently known besides the now-fixed `sleep`).

### Job output files in `/tmp/kaish/jobs/` persist indefinitely
`JobManager` only cleans up on explicit `cleanup()` / `remove()`. No
automatic GC of stale files from crashed sessions or old jobs.
Long-running embedders accumulate. Prune on kernel start and on a
configurable interval.

### `JobManager::spawn` busy-waits
Uses `std::hint::spin_loop()` to guarantee immediate visibility. Works,
wastes CPU on contention. Channel-based coordination would be cleaner.

### MCP per-request OS thread + 16MB stack
`crates/kaish-mcp/src/server/execute.rs:192` documents it. Fine at
agent-call rates, will be a problem in a hot loop. Replace with a pool
of LocalSet workers + mpsc channel. Benchmark first to confirm the win.

### MCP resource list always re-traversed
`notify_resource_list_changed` fires unconditionally. Cheap fix;
diff against previous snapshot before notifying.

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

### `ToolCtx::as_any`/`as_any_mut` are a public downcast hatch
`crates/kaish-tool-api/src/ctx.rs`. The escape hatch that lets in-tree builtins
recover the concrete `ExecContext` is exposed on the public trait, so an
out-of-tree tool could in principle downcast to a kernel type — though only if
it deliberately takes a dependency on `kaish-kernel` to name `ExecContext`
(impossible from the leaf API alone), so the practical risk is low. Cheap
hardening flagged by dpal 2026-06-03: mark both methods `#[doc(hidden)]` so they
don't advertise themselves as part of the supported surface. A heavier option (a
kernel-internal extension trait carrying the downcast, keeping `ToolCtx` itself
hatch-free) is more churn than the current need justifies.

---

## P4 — Eventually

### Control structures inside `$()` are not supported
The `$()` body accepts the full *statement* grammar — pipelines, `&&`/`||`
chains, `;`/newline sequences, `#` comments (landed 2026-06-11, see Resolved) —
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

### `--json` is not stripped before `test` parses its operands
`test -n hello --json` errors with `test: unknown binary operator: -json`
(exit 2) — the global-flag extraction that strips `--json` for every other
builtin doesn't fire for `test`'s raw-operand evaluation. The error does
honor the JSON envelope contract, and the sweep pins that behavior
(`json_sweep_tests.rs`, the `test` case). `--json` on an output-less builtin
is near-meaningless, so this is polish: either strip it there too or document
the exception. Surfaced by the `--json` sweep 2026-06-11.

### `jq --json` double-encodes already-JSON stdout
`echo '{"a":1}' | jq '.a' --json` emits the JSON *string* `"1\n"` rather than
the number `1` — jq's stdout is already JSON text, and the text→string rule
wraps it again. The real value rides `.data` (which MCP's
`structured_content` uses), so consumers have a correct path; this is about
the `--json` text surface specifically. Decide: have `apply_output_format`
prefer `.data` when present, or document that `--json` is redundant after
`jq`. Sweep pins current behavior. Surfaced 2026-06-11.

### `spawn --command true` — bool-shaped values vanish from named args
`true`/`false` as a named-arg value lex as `Value::Bool`, and
`flagify_bool_named` converts `Bool(true)` into bare flag presence — so
`spawn --command true` errors with "a value is required for '--command'".
Quoting works (`--command 'true'`). Documented flagify behavior colliding
with a literal command named `true`; a did-you-mean hint (or exempting
declared value-taking params from flagify) would smooth it. Surfaced by the
`--json` sweep probes 2026-06-11.

### Bare `,` / numeric ranges parse oddly — `cut -d,`, `tr -d 0-9` need quoting
Surfaced 2026-05-28 by the kernel-routed builtin tests. Two related
tokenization gaps in the same family as the dot-prefixed-filename entry
above:
- A standalone comma isn't a bareword, so `cut -d, -f2` and `cut -d , -f2`
  both raise a parse error; only `cut -d ',' -f2` works.
- A numeric range like `0-9` lexes as `Int(0)` + `Int(-9)` (the `-` starts
  a negative int), so `tr -d 0-9` is a silent no-op — only `tr -d '0-9'`
  works. Letter ranges (`tr a-z A-Z`) work because `-` is inside the Ident
  char class. (The `tr` help example was fixed 2026-05-28 to quote `'0-9'`.)
Low priority — quoting is a clean workaround and kaish is a POSIX *subset* —
but a "did-you-mean: quote it" diagnostic would smooth bash porting. Decide
whether lone punctuation / digit-ranges that agents reach for as set/delim
arguments should tokenize as barewords.

### Flag injection via glob expansion in `rm`
`rm *` in a directory containing `-rf.txt` expands to `rm -rf.txt …`
and kaish's structured flag parsing then flips `rm` behaviour.
Mitigations already in place: `set -o latch`, `set -o trash`, `rm --
*`. Future fix: the kernel knows which args are `GlobPattern`-sourced
— `rm` could reject flag-shaped positional args from that path.

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

### Multi-file `grep` silently skips unreadable explicit operands
`grep_multiple_files` does `Err(_) => continue` per file — right for the
recursive walk it was written for, but for explicit operands
(`grep p real.txt typo.txt`) a missing/unreadable file should report
`grep: typo.txt: …` and affect the exit code, like POSIX grep. Same
silent-skip shape as the multi-arg `ls` entry below. Low priority.

### Multi-argument `ls` silently skips inaccessible paths
`Ls::render_names` (`ls.rs`) skips any path that fails to `stat` — correct
for the glob race it was written for (a match removed between walk and
stat), but for an explicit multi-arg call like `ls real.txt gone.txt` it
drops `gone.txt` with no error, where bash prints `ls: cannot access`.
Single-arg `ls gone.txt` still fails loudly (separate code path). Low
priority; decide whether multi-arg should accumulate per-path errors.

---

## Resolved since previous lists

Captured here so context from `cleanups-todo.md` / old `issues.md`
isn't lost when those files are deleted.

- **`glob` with an absolute pattern matched nothing through a hidden dir — fixed 2026-06-11.**
  Surfaced by `cargo test --all` (the `subprocess`-gated test module isn't
  compiled by a plain `cargo test`, so it slipped past commit `3a30afe`, the
  one that rewrote these tests to use absolute tempdir paths).
  `kernel::tests::test_for_loop_glob_iterates` —
  `for F in $(glob "/abs/dir/*.txt")` — iterated 0 times. **Actual root cause**
  (my first localization in the now-removed P1 was wrong: `GlobPath::matches`
  is purely segment-based and ignores `anchored`, so matching was never the
  problem): for an anchored pattern the builtin walked from
  `ctx.resolve_path("/")` with the full pattern (the `static_prefix`
  optimization was explicitly punted in a code comment), and the walker
  **skips hidden entries** (`entry_name.starts_with('.')`, `walker.rs:263`).
  `tempfile::tempdir()` creates `/tmp/.tmpXXXX` — a *hidden* dir — so the walk
  never descended into it. (Walking from `/` was also O(filesystem).) MemoryFs
  only *appeared* to work because its non-hidden roots happened to be
  traversable. **Fix:** new `GlobPath::split_static_dir()` peels the literal
  leading components into the walk root, so glob walks from `/.work/data`
  (or `/tmp/.tmpXXXX`) directly — the hidden ancestor is the root, not a
  skipped entry — and matches the remainder (`*.txt`) beneath it; it always
  leaves ≥1 segment so an all-literal `/a/b/c.txt` walks `/a/b` and matches
  `c.txt`. Names are still reported relative to the conventional root
  (`report_root`), so output shape is unchanged. Tests: a `split_static_dir`
  doctest + two MemoryFs regression tests reproducing the bug class without a
  real FS (`test_glob_anchored_through_hidden_dir` — absolute pattern through a
  hidden `.work/`, plus a depth assertion that nested `sub/c.txt` is *not*
  caught by `*.txt`; `test_glob_anchored_exact_literal_file`), and the original
  `test_for_loop_glob_iterates` now passes.

- **0.8.2 swipe: four agent-facing language fixes — done 2026-06-11.** One
  session, TDD throughout (failing test first), kaish+bash compat sides both
  green, clippy clean on default / `--no-default-features` / `subprocess`.
  - **#1 inline env prefix is command-scoped (`FOO=bar cmd`).** New
    `Stmt::EnvScoped { assignments, body }` (parser produces it when ≥1
    `NAME=value` is immediately followed by a command/pipeline; a bare
    `NAME=value` with no command stays a persistent `Assignment`, and
    `FOO=bar && cmd` does not over-capture). The kernel applies the assignments
    as **exported** vars in a fresh scope frame for the body only, then unwinds
    the frame and restores export marks — so the subprocess env sees them and
    nothing leaks. Values evaluate left-to-right (`A=1 B=$A cmd`). Documented
    kaish divergence: the prefixed var is visible to the command's own arg
    expansion (kaish builtins read scope), where bash expands args first. Tests:
    parser shape + disambiguation, `shell_compat` leak/persist/visibility,
    subprocess `env_prefix_reaches_subprocess_then_does_not_leak`.
  - **#2 unquoted glued argv words are a parse error, not a silent splat.**
    `args_list_parser` captures each pre-`--` arg's span and rejects two
    positional args with touching spans (`/tmp/$(echo x).txt`, `$dir/out.txt`,
    `foo$(x)baz`) with a "quote the whole word" `Rich::custom` error — mirroring
    the existing `WordAssign` spaces-around-`=` check. Precise: single-token
    words (`file.txt`, `a.b.c`, `v1.2.3`) are one arg and never trip it (probed
    against the live binary first). Landed as a **parse error** (not the
    originally-floated validator E-code) because spans live in the parser and
    `parse()` already favors unskippable parse-time rejection for dangerous
    ambiguities (`first_ambiguous_stdin`). Residual post-`--`/flag-glue case →
    P4 above.
  - **#3 scatter newline-splits plain-text stdin.** `extract_items` now mirrors
    the for-loop `$(cmd)` contract (split on `\n` only, trim trailing `\n`/`\r`,
    keep interior blanks, whitespace-in-line intact) so `cat items.txt | scatter
    --as ITEM …` runs one worker per line instead of binding the whole stdin to
    one worker. Both scatter files' unit tests + a kernel-routed
    `scatter_plain_text_stdin_fans_out_per_line`. LANGUAGE.md states the
    contract.
  - **#5 `$()` accepts the full statement grammar.** `Expr::CommandSubst` /
    `StringPart::CommandSubst` now hold `Vec<Stmt>` (the old `Pipeline`-only
    representation and the dead `stmt_to_pipeline` helper were **deleted** — one
    representation, no compat shim). `cmd_subst_parser` builds a local
    chain/sequence parser (`&&`/`||`/`;`/newlines/comments); the kernel runs the
    block via a new `execute_block_capturing` (output accumulates with no
    separator, last statement's `.data`/exit ride through, scope/cwd snapshot
    restored). String interpolation `"$(a && b)"` gets the full grammar for free
    (it already routed through `parse()`). The sync `Executor::execute` trait now
    takes `&[Stmt]` (only `NoOpExecutor` implements it). Control structures in
    `$()` are the one boundary → P4 above. Tests: bash-cross-checked
    `cmdsubst_*` (`&&`/`||`/`;`/multiline/comment/string-interp/cd-containment).

- **`kill` job control works in hermetic builds — fixed 2026-06-11.** The whole
  `kill` builtin was gated `#[cfg(all(unix, feature = "subprocess"))]`, so
  `kill %1` returned "not supported on this platform" without `subprocess` —
  even though terminating a kaish background job is pure kernel work (a
  cancellation token, not an OS signal). Restructured: the builtin is always
  active; arg parsing + `%N`-vs-PID dispatch are platform-independent; `kill_job`
  and `kill_pid` have two cfg variants. Hermetic `kill_job` cancels the job via
  its token (terminating signals) or refuses loudly (non-terminating, no process
  group to signal). The `subprocess` variant keeps the full OS-signal fidelity
  (`killpg` of recorded process groups, bare-PID `nix::kill`) byte-for-byte —
  and when external commands are on, `/bin/kill` covers raw PIDs anyway. The
  `kill %N` / `wait %N` builtin-job tests in `background_execution_tests.rs` were
  un-gated from `subprocess` (they assert pure kaish job control) and now pass in
  both builds; the `--json` sweep's `kill %1` case passes in the default build.

- **grep/rg value-bearing flags were silently dropped on the kernel path — fixed 2026-06-11.**
  The realworld-port's first kernel-routed run caught it: `grep -A 5` (and the
  `--after-context=5` equals form) matched but emitted **no context lines**;
  `rg -A`, `rg --max-count`, `--max-depth`, `--max-filesize`, `--type-not`
  were all likewise dropped. Cause: both builtins clap-parse into a struct,
  then **ignore it** and re-read the raw `ToolArgs` named map by snake_case
  id (`args.get("after_context")`) — but kernel binding stores the value
  under the kebab long-flag name (`after-context`), so the lookup missed and
  the option silently defaulted. The inline unit tests passed because they
  inject snake_case keys directly. Fix: grep's context values and the whole
  of `RgOptions` now come from the clap-parsed struct (`from_parsed`), the
  one source both paths share; a non-numeric value (`rg -A bogus`) is now a
  loud exit-2 error naming the flag instead of a silently ignored option.
  `awk` already did this right (clap-first with legacy fallback); audit found
  no other value-flag offenders (`ln`'s snake read is a positional fallback).
  Pinned by `realworld_builtin_tests.rs::rg_realworld` (context, max-count,
  type filter, bad-value error) and the grep `-A` realworld case.

- **`realworld_builtin_tests.rs` ported to the kernel route — done 2026-06-11.**
  All 43 mined-pattern tests now drive their real command strings through
  `kernel.execute()` over a tempdir (`common::kernel_at`/`run`); the pipeline
  patterns that were previously simulated by manually shuttling stdout into
  `ctx.set_stdin` are now actual `|` pipelines. Added a 5-test `rg_realworld`
  section (rg had zero kernel-routed coverage and the largest flag surface) —
  48 tests total. The port found the grep/rg value-flag bug above on its
  first run. File is `#![cfg(feature = "localfs")]`-gated so the minimal
  build skips it cleanly (the integration-binary minimal-compile item).

- **`--json` sweep across the whole registry — done 2026-06-11.**
  New `tests/json_sweep_tests.rs`: one representative invocation per
  registered builtin run with `--json` through `kernel.execute()`, asserting
  exit code + top-level JSON shape (array/object/string/empty/error
  envelope). A drift guard fails the suite when a builtin is registered
  without a sweep case or an explicit documented skip (5 skips: `[`
  unreachable, bg/fg need a PTY, exec replaces the process, kaish-trash
  reads the user's real trash). Found and fixed two real bugs:
  - **`checksum --json` columns were scrambled** — the full text line sat in
    `node.name`, which the table→JSON convention maps to the *first* header,
    shifting every column (HASH=whole line, FILE=hash, ALGO=path, algorithm
    dropped). Both table sites now follow the name-is-first-column rule.
  - **`unset` printed the removed-count** (`X=1; unset X` emitted `1` where
    bash is silent) — polluting `$()` captures and `--json` output with a
    stray number. Now silent on success per POSIX; pinned by the
    bash-cross-checked `unset_is_silent` compat case.
  Three warts recorded as new P4 entries rather than fixed: `test` doesn't
  get `--json` stripped, `jq --json` double-encodes, `spawn --command true`
  bool-flagify collision. Harness note: the sweep builds its kernels via
  `into_arc()` because redispatching builtins (`timeout`) need the kernel's
  `self_weak` dispatcher — a plain `Kernel` returns "no dispatcher
  available".

- **Destructive-op safety rails now have kernel-routed coverage — fixed 2026-06-11.**
  New `tests/latch_trash_tests.rs` (9 tests, all through `kernel.execute`):
  latch flow (`set -o latch` → `rm` exit 2 → re-run the `.data` hint verbatim →
  deleted; bogus nonce → exit 1, file survives; multi-path batch under one
  nonce; latch-off default deletes) and the previously-never-executed
  `RmAction::Trash` arm via a mock backend (small file delegates to backend
  without double-delete; directories trash without `-r`; trash beats latch for
  small files; **trash failure = exit 1, never falls through to permanent
  delete**; backend-absent fails loud). Enabling API: new public
  `Kernel::set_trash_backend(Option<Arc<dyn TrashBackend>>)` — also useful to
  embedders that want a custom trash. Trash tests root their tempdir in
  `CARGO_TARGET_TMPDIR` (not `/tmp` — `decide_rm_action` skips trash there).
  Bonus find by the new `validation_quoted_glob_in_mv` assertion: `mv`'s
  per-source error dropped the operand name (`mv: not found: …`); now
  `mv: <src>: <err>` matching `rm`'s format.

- **Five test-effectiveness residual bullets — fixed 2026-06-11.**
  (1) `background_job_snapshot_isolation` now backgrounds the *delay* too
  (`snap() { sleep 0.2; echo $VAR; }; snap &`), so it fails under a
  shared-scope regression instead of sequencing the mutation after a
  foreground sleep. (2) `sandbox_external_commands_blocked` pins exit 127 +
  "command not found" — a sandbox escape where curl actually runs and fails
  no longer passes. (3) `validation_quoted_glob_in_mv` replaced
  `is_ok() || is_err()` with: validates OK, fails at runtime, error names the
  literal `*.old` (proof the quoted glob wasn't expanded). (4) New Linux-gated
  `external_argv_does_not_split_space_containing_var` pins the no-split
  guarantee on the external spawn path (`/usr/bin/printf "[%s]" $X` → one
  bracket group). (5) `wait`/`kill %1` now appear as commands in integration
  tests: `wait_propagates_background_job_failure` (failed job → exit 1 +
  `[1] Failed`) and `wait_after_kill_reports_job_gone` in
  `background_execution_tests.rs` — no PTY needed.

- **kaibo P1 pair for 0.8.1 — done 2026-06-11.** (1) `ByteBudget` re-exported
  through `kaish_kernel::vfs` so a `with_backend` embedder names the type it
  hands to `MemoryFs::with_budget` without a direct kaish-vfs dep; locked in
  by `vfs_budget_tests::byte_budget_reexport_serves_with_backend_embedders`.
  (2) Watchdog seam (`ctx.patient`): the per-execute timer is now a
  movable-deadline `Watchdog` (`kernel/src/watchdog.rs`, tokio `watch`
  channel) the timer task re-arms against. `ToolCtx::patient(budget)`
  (kaish-tool-api, default inert) returns a `PatientGuard`; the kernel's
  `ExecContext` override acquires a `WatchdogHold` — while held the script
  clock freezes and the hold's own budget governs (overrun fires 124);
  Drop restores the frozen remaining (VarsFrameGuard discipline; out-of-order
  release from forked stages handled). Cancellation stays live throughout —
  only the timer pauses. The `timeout` builtin is deliberately NOT suspended
  (a user-requested bound keeps its teeth; doc comment in timeout.rs).
  Watchdog handle rides `ExecContext.watchdog` alongside `cancel` (execute
  entry sets, restore block clears, `dispatch_command` syncs, forks share).
  Tests: 6 paused-clock unit tests in watchdog.rs + 7 kernel-routed
  integration tests in `tests/patient_watchdog_tests.rs` (failing-first).
  Pulled into 0.8.1 by Amy 2026-06-11 (originally penciled for 0.8.2).

- **Documentation truth slate — fixed 2026-06-11.** Every verified falsehood
  from the 2026-06-09 accuracy sweep corrected, re-probed against the live
  binary first: LANGUAGE.md `[ ]` story unified (docs-only decision —
  `[ expr ]` doesn't parse; `[[ ]]`/`test` are the forms; aliases dropped
  from the intentionally-missing row), VFS table re-trued (no `/git` mount
  anywhere; git is a builtin; `/v/*` mounts documented), `head/tail -c`
  re-trued to deliberate POSIX bytes, chumsky line updated to crates.io
  alpha, latch example matches the `Authorized:`/quoted-nonce format.
  limits.md: `[ ]` line fixed, keyword row narrowed (statement-openers
  `echo if`/`for`/`while`/`case` still fail; closers like `echo done` work
  now), `-c` row fixed, stale functions-can't-run-in-pipelines bullet
  dropped. vfs.md: `/git` mount + section removed. rg.md: `-trust` example
  → `-t rust` + glued-flag caveat, `kaish-jq` → `jq`, dead native/WASI
  caveat replaced (rg deps are unconditional and registration ungated).
  README: embedding snippet `result.out` → `text_out()`; MCP section
  rewritten per the resonance panel's top-3 — single `execute` tool (the
  phantom `help` tool claim removed), `structured_content` envelope +
  exit-code table (0/1/2/3/124/130) with recovery actions, per-call
  lifecycle table (resets vs persists), sandbox section naming
  `allow_external_commands`/capability features/`--overlay`/64 MiB budget,
  and agent-gotcha callouts. Heredoc test headers re-trued (body `\r`
  preserved + CR-tolerant delimiter, unterminated = hard error, body-`$()`
  tests live and un-ignored). New finding recorded as its own P2 entry:
  inline env prefix persists past the command.

- **EMBEDDING.md rewritten + git material split out — fixed 2026-06-11.**
  Full restructure per the resonance recommendation. Core guide now leads
  with a stability/MSRV statement and capability-feature table, then:
  KernelClient/EmbeddedClient surface (incl. `shutdown()` no-op),
  KernelConfig modes/builders, `with_backend` hermeticity guarantees,
  `ExecuteOptions` (vars overlay replacing deprecated `execute_with_vars`,
  `Duration::ZERO` dry-run, cancel_token, W3C trace context), current
  `Tool` trait (`&mut dyn ToolCtx`) + `owns_output`, `Filesystem`/`&Path`
  VFS access, JobFs `"done:0"` status strings. GitVfs/kaijutsu tutorial
  moved to `docs/EMBEDDING-GIT.md` with corrected samples
  (`LocalBackend::new(Arc<VfsRouter>)`, `git` feature callout). Repo
  CLAUDE.md/GEMINI.md crate tree lists all 12 crates, mod.rs rule scoped to
  new modules, VFS Router line de-gitted. Residual: samples are still not
  doc-tested or generated, so drift will recur — reviews.md's
  generate-or-doctest suggestion stands.

- **Minimal-build lib-test regression — fixed 2026-06-10.** `touch.rs`'s
  `test_touch_existing_readonly_rejects` did an ungated `use crate::vfs::LocalFs;`,
  breaking `cargo test -p kaish-kernel --lib --no-default-features` (E0432). The
  documented `cargo check --no-default-features` gate missed it because check
  doesn't build `#[cfg(test)]` code. Gated the test under
  `#[cfg(feature = "localfs")]` (it needs `LocalFs::read_only`); minimal lib
  tests compile and run again (1347 pass). Added
  `cargo test -p kaish-kernel --lib --no-default-features --no-run` to the
  build-command gate list so a test-only import can't slip past `check` again.
  Integration-binary minimal compile is still open (separate, larger; see P2).

- **`kill %N` now signals running background jobs (builtin *and* external) — fixed 2026-06-10.**
  Previously `kill %N` only worked for *stopped* jobs (PGID recorded via Ctrl-Z);
  a running `sleep 5 & kill %1` was "job N not found". Two unifying mechanisms,
  matching kaish's "jobs are tasks, not processes" model:
  - **Per-job cancellation token (Phase 1).** `execute_background` now forks via
    `fork_for_background(cancel, job_id)` and records the fork's `CancellationToken`
    on the `Job` (`JobManager::set_cancel_token`/`cancel`). `kill %N` with a
    terminating signal cancels it — stopping pure-builtin jobs (`sleep &`, no OS
    process group) and cascading SIGTERM→SIGKILL to any external children.
  - **Recorded process groups (Phase 2).** The background fork is stamped with
    `bg_job_id` (propagated through the fork tree); when `try_execute_external`
    spawns a child it records the child's PGID on the job
    (`JobManager::add_pgid`/`job_pgids`). `kill --signal STOP/CONT/USR1/… %N`
    then `killpg`s the real process group(s) for full signal fidelity. A
    non-terminating signal to a pure-builtin job is refused loudly.
  Tests: `JobManager` unit (`test_cancel_token_fires`, `test_pgids_recorded_and_deduped`,
  `test_cancel_without_token_returns_false`), builtin e2e
  (`kill_terminates_builtin_background_job`,
  `kill_nonterminating_signal_on_builtin_job_errors`), external e2e
  (`kill_signals_external_background_job_process_group` — STOP/CONT/TERM).
  LANGUAGE.md:635 re-trued. Residual: the bash `kill -9`/`-STOP` shorthand still
  isn't parsed (separate kill-argv entry, P2); `kill --signal NAME %N` is the form.

- **`wait %N` / `kill %N` jobspec syntax no longer a lexer error — fixed 2026-06-10.**
  `%` had no token, so `wait %1` / `kill %N` were "lexer error: unexpected
  character" despite being documented. Added a `JobSpec` lexer token
  (`%[0-9]+`, keeps the `%` — kill uses it to tell a job from a PID), wired into
  `primary_expr_parser` as a literal string. `wait` now strips the optional `%`
  and waits for **each** positional (the doc's `wait %1 %2` waited only the
  first before); `kill` already stripped `%`. Tests: lexer
  `job_spec_lexes_as_one_token`, `wait` unit (`%N` + multi-job), e2e
  `wait_jobspec_percent_form_end_to_end`. Arithmetic `%` (modulo, preprocessed
  inside `$(())`) is unaffected. Residual: `kill %N` still can't signal a
  running background job — separate PGID-tracking gap, now its own P3 entry.

- **Statement-output joining inserts no separator (`;`/`&&` consistent, bash-matching) — fixed 2026-06-10.**
  `accumulate_result` inserted a newline between outputs that didn't already end
  in one, so `printf "a" && printf "b"` gave `a\nb` while `printf "a"; printf "b"`
  (which bypassed it) gave `ab`, and the doc claimed the `a\nb` form as a feature.
  Decision (maintainer): match bash — drop the artificial separator so both give
  `ab`; a newline appears only when a command emits its own (`echo`). Removed the
  stdout+stderr separator insertion in `accumulate_result` (affects all callers:
  sequences, `&&`/`||` chains, loops, function bodies — all now bash-consistent).
  `LANGUAGE.md` output-model note and `limits.md` row re-trued; inline tests
  updated; 5 bash-cross-checked compat cases (`semicolon_*`, `and_chain_*`,
  `printf_loop_no_separator`, `echo_sequence_keeps_own_newlines`). Residual `$()`
  parse limits split into their own (still-open) entry.

- **`--` now protects dash-words with internal hyphens — fixed 2026-06-10.**
  `echo -- -not-a-flag` printed `-not -a -flag` because the `ShortFlag` lexer
  regex (`-[a-zA-Z][a-zA-Z0-9]*`) excluded internal hyphens, fragmenting one
  shell word into three flag tokens before the parser saw it. Fix (maintainer
  chose the lexer route over a post-`--` parser rejoin): add `-` to the regex
  char class (`-[a-zA-Z][a-zA-Z0-9-]*`) so the whole word is one token; `--`
  stays `DoubleDash` (second char must be a letter) and `-` stays `MinusAlone`.
  The `parser_double_dash_ends_flags` snapshot that blessed the bug was
  re-blessed (now one positional). Tests: lexer
  `short_flag_with_internal_hyphens_is_one_token` (+`--`/`-` guards) and e2e
  `test_double_dash_protects_internal_hyphen_word` /
  `_multiple_dash_words` in shell_bugs_tests.rs. Blast radius: a no-space
  `-x-y` now lexes as one flag `x-y` rather than two; no test or builtin
  depended on the split. Sibling tokenization-gap (`bare ,` / digit-ranges)
  stays P4.

- **`break N` / `continue N` no longer discard pre-signal output — fixed 2026-06-10.**
  A nested loop printing before `break 2` printed nothing: the Break signal
  carried a fresh empty `ExecResult` and *replaced* the loop's accumulated
  output as it propagated up, instead of merging. Fix in `kernel.rs`: two
  helpers (`fold_loop_output_into_flow` when a break/continue propagates to an
  outer loop, `accumulate_flow_output` when a loop finally handles one) applied
  to the For and While break/continue arms. Bash-cross-checked compat tests
  (`break_2_preserves_inner_output`, `break_2_preserves_outer_and_inner_output`,
  `continue_2_preserves_inner_output`, plus the single-level control). Same
  defect fixed for `continue N`, which shared the mechanism.

- **`${VAR:-"default"}` default-word quote removal — fixed 2026-06-10.** The
  default word was passed verbatim to `parse_interpolated_string`, so quote
  characters survived into the value (`${NAME:-"default"}` → `"default"`). The
  quotes are shell syntax, not data. Added `unquote_default_word` in `parser.rs`
  (drops quote delimiters; double quotes keep interpolation, single quotes
  suppress it via the `__KAISH_ESCAPED_DOLLAR__` marker) applied at all three
  default-word sites (`parse_var_expr` + the two interpolated-string parsers).
  Bash-cross-checked compat tests in `shell_compat_tests.rs`
  (`default_word_*`, 7 cases incl. single-quote interpolation suppression and
  value-wins). `LANGUAGE.md:32`'s example now reads true.

- **`[[ ! A || B ]]` precedence — fixed 2026-06-10.** `!` was parsed by
  `ignore_then(compound)` where `compound` is the full or-level parser, so it
  negated the entire rest of the expression (`!(A || B)`) instead of just the
  next term. Empirically `[[ ! -f /nonexistent || -d /etc ]]` returned false
  (should be true). Fix: a nested `recursive` makes the bang arm recurse at the
  unary level only (`(!A) || B`), matching the grammar comment and
  `LANGUAGE.md`. Two parser snapshots (`test_not_with_and`,
  `test_complex_compound`) had blessed the buggy AST and were re-blessed to the
  correct precedence; behavioural regressions in `shell_bugs_tests.rs`
  (`test_bang_binds_tighter_than_or/_and`, `test_bang_single_term_unaffected`).

- **Output-limit runtime control is now reachable from the shell — fixed 2026-06-10.**
  The four-part bug cluster the 2026-06-09 doc-accuracy pass surfaced is closed,
  each with kernel-routed coverage in `tests/output_limit_control_tests.rs`:
  - **`set -o output-limit=SIZE` parses.** The set grammar
    (`parser.rs`, `set_with_flags`) only accepted flag tokens and bare
    identifiers, so the `name`/`=`/`value` token triple was a parse error. Added
    a `set_option_assign` arm that folds the three tokens back into one
    `name=value` positional (the form `set.rs` already parses), plus a
    `set_quoted_arg` arm so `set -o "output-limit=8K"` works too.
  - **`kaish-output-limit set N` parses.** `set` is a keyword token and was
    absent from `keyword_as_bareword` (`parser.rs:primary_expr_parser`), so it
    couldn't be an argument — the `set` subcommand (and `echo set`) were parse
    errors / silently mis-parsed. Added `Token::Set => "set"`; the `set` *builtin*
    is still matched only when the token leads a statement, so nothing is shadowed.
  - **Runtime mutations persist.** `execute_command`'s sync-back (`kernel.rs`)
    synced scope/cwd/aliases/pipes back into `self.exec_ctx` but **not**
    `output_limit`, so a builtin's change was dropped before `dispatch_command`
    could read it back. Added the one missing `ec.output_limit = ctx.output_limit`.
  - **Docs re-trued to the real 8K default** (was 8K in code, 64K in docs):
    `output-limit.md` MCP-limit row + `on` default, `limits.md` set-options row,
    and `LANGUAGE.md`'s set-builtins note (which now lists `-o output-limit`).

- **`touch .hidden.txt` / dot-prefixed filenames — fixed (validated 2026-06-09).**
  The old P4 entry is closed: dot-prefixed names now lex as a single
  `DottedIdent` bareword (`lexer.rs:378-384`, the argv-barewords work,
  `074ad98`). Verified at HEAD: `touch .hidden.txt && ls .hidden.txt &&
  echo .parent` all behave; POSIX `. file` source alias still works (pinned by
  the bare-`.` Resolved entry below). Quoting is no longer needed.

- **grep `-E` no-op — closed as decided 2026-06-09.**
  The old P2 entry's preferred end state (option a + c: keep the accept-and-
  ignore for muscle-memory compatibility, note it in help text) is implemented:
  `grep.rs:74-76` doc comment ("No-op: Rust's regex crate is always extended …
  accepted for POSIX/muscle-memory compatibility") surfaces in `help grep` via
  the clap-derived schema. Nothing left to do unless someone reports surprise.

- **`with_backend` kernels now refuse host side channels — output spill + job-output leaks closed 2026-06-08.**
  Two paths reached the real host filesystem via `std::fs`, bypassing the VFS (and
  any read-only mount): output disk-spill (`paths::spill_dir()`) and background-job
  output files (`Job::write_output_file` → `std::env::temp_dir()/kaish/jobs`). The
  auto-force to `SpillMode::Memory` only triggered on `VfsMountMode::NoLocal`, but the
  read-only embedder path is `Kernel::with_backend` (kaibo, kaijutsu) with a
  `Sandboxed`-flavored config — so neither guard fired and a >limit `cat`/`rg` (8 KB
  in kaibo) spilled project bytes to host `/tmp`, and `cmd & ; wait` wrote job output
  there too. Both bypass the read-only `LocalFs` mount kaibo relies on. Realized the
  "dedicated runtime read-only flag" the `assemble` comment anticipated, structurally:
  `assemble` takes a `no_host_filesystem` bool (`true` from `with_backend`, since a
  custom-backend kernel owns no host mounts so *any* host write is a bypass; `false`
  from `Kernel::new`, where `NoLocal` still triggers via `vfs_mode`). When set it
  forces `SpillMode::Memory` **and** `JobManager::set_persist_output_files(false)`.
  Jobs stamp the flag at registration (`spawn`/`register`/`register_with_streams`);
  `Job::wait` skips the host write when off — live output stays in-memory via the VFS
  streams (`/v/jobs/{id}/stdout`) and the cached `ExecResult`, so nothing is lost
  in-process. Tests: `scheduler::job::tests::test_no_host_output_file_when_persistence_disabled`
  (mechanism) and `background_execution_tests::test_job_output_persistence_disabled_for_hostless_kernels`
  (wiring: `NoLocal` + `with_backend` → off, `Sandboxed` → on). The `host` `/proc`/`/etc`
  *reads* remain a separate, by-design, `host`-gated disclosure (P1).

- **Stdin redirect (`< file`) now reads through the VFS, honors `$PWD`, and fails loud — fixed 2026-06-08.**
  `setup_stdin_redirects` (`scheduler/pipeline.rs`) read the target with
  `std::fs::read_to_string` — bypassing the VFS entirely (host-fs leak in sandbox
  mode, ignored VFS mounts), not resolving against cwd, and silently swallowing a
  missing/unreadable file (`if let Ok(content)`) so the command ran with empty
  stdin instead of erroring. Now the helper is `async`, resolves the target via
  `ctx.resolve_path` and reads via `ctx.backend.read` (same path as `cat`), and a
  missing file or non-UTF-8 content is a hard error (exit 1, `redirect: …`
  message). Both call sites updated: `run_single` returns the failure directly;
  the pipeline-stage path surfaces it from inside the spawned task. Two regression
  tests (`sandbox_stdin_redirect_reads_via_vfs_and_cwd`,
  `sandbox_stdin_redirect_missing_file_fails_loud`). Command substitution in
  redirect targets was a follow-on fix (see next entry).

- **Command substitution now runs in redirect targets and heredoc bodies — fixed 2026-06-08.**
  Redirect-target and heredoc-body evaluation went through the synchronous
  `eval_simple_expr`/`eval_string_parts_sync`, whose `CommandSubst` arm was a
  no-op (no pipeline execution available), so `cat < $(echo x)`,
  `echo x > $(echo f)`, and `$(...)` inside a heredoc body silently dropped the
  substitution. Fix (design chosen with the maintainer): added
  `async fn eval_expr(&self, expr, ctx)` to the `CommandDispatcher` trait — the
  Kernel delegates to the existing `eval_expr_async` (snapshots scope/cwd, runs
  the pipeline, restores; only output escapes), the test `BackendDispatcher`
  keeps its sync-only behavior. `eval_redirect_target` is now async, returns
  `Result`, and routes through `ctx.dispatcher.eval_expr` (already populated on
  every `ExecContext`), falling back to the sync evaluator only when no
  dispatcher is attached. All four output-redirect arms in `apply_redirects` and
  the stdin/heredoc/here-string arms in `setup_stdin_redirects` were converted to
  the async fail-loud path. Un-ignored the two `heredoc_tests` command-subst
  tests; added `sandbox_redirect_target_command_substitution`. **Remaining:**
  command substitution *embedded in* an interpolated redirect path
  (`> /tmp/$(echo x).txt`) still fails to parse — see P2 below; the bare-subst
  form (`> $(echo /tmp/x.txt)`) works.

- **Output redirects now resolve relative targets against `$PWD` — fixed 2026-06-08.**
  `redirect_write`/`redirect_append` (`scheduler/pipeline.rs`) handed the raw
  redirect target straight to `ctx.backend.write` with no cwd resolution, unlike
  every other path operand. The router normalizes a bare relative path by
  prepending `/` (`vfs/router.rs`), so `echo x > f` wrote `/f`, not `$PWD/f`. Two
  symptoms: redirects ignored `cd` (`cd /sub; echo x > f` landed at `/f`), and
  write/read disagreed — `echo x > f` then `cat f` failed because `cat` resolves
  against cwd while the redirect resolved against `/`. Surfaced from kaibo, where
  it made a sandbox test vacuous: a relative `>` into a read-only project landed
  in the ephemeral `MemoryFs` `/` mount instead of being refused. Fix: both
  helpers now call `ctx.resolve_path(path)` (the same cwd-relative, normalized
  resolution `cat`/`cp`/etc. use) before handing the target to the backend;
  absolute targets are unaffected. Two regression tests pin it
  (`sandbox_relative_redirect_honors_cwd`, `sandbox_relative_append_honors_cwd`):
  relative write+read agree at `$PWD/f`, and `cat /f` still fails.

- **Space-form flag values to backend tools no longer silently dropped — fixed 2026-06-08.**
  `kj context create exp --type explorer` (space form) silently lost the value
  while `--type=explorer` (equals form) worked — a **privilege-escalation-by-typo**
  once kaijutsu went deny-by-default (asking for read-only `explorer`, silently
  getting permissive `default`). Root-caused to kaish, not kaijutsu: in
  `build_args_async` (kernel.rs) an *undeclared* flag — one the tool's schema
  doesn't list as a non-bool param — defaults to `is_bool = true`
  (`unwrap_or(true)`), so `--type` became a value-less boolean flag and `explorer`
  was orphaned, then misrouted by `map_positionals` to a different param slot. The
  `=` form survives because `--type=explorer` lexes as `Arg::Named` and bypasses
  schema lookup entirely. Confirmed empirically: `{named:{name:"context"},
  flags:{type}, positional:[create,exp,explorer]}`. Since kaish genuinely cannot
  distinguish `--type explorer` (flag wants value) from `--force file.txt` (bool +
  positional) without a complete schema, the fix **fails loud rather than guessing**
  (crash > corruption): an undeclared long flag under a `map_positionals` schema
  that is immediately followed by an unconsumed positional now errors with a
  copy-pasteable fix — `kj: --type is not a declared flag … Use --type=explorer, or
  have kj declare --type in its schema`. Surfaces like a validation failure (Err →
  exit 1, message to stderr). Six guard tests pin the safe paths (declared flag
  still binds, `=` form binds, bool-at-end ok, flag-before-flag ok, builtins
  unaffected); one red-first test pins the loud error. The real binding fix remains
  for kaijutsu to declare its `kj` flags in the schema, after which the existing
  `consume_flag_positionals` machinery handles space form. Two residuals tracked
  below (sync `build_tool_args`; single-char short flags).

- **Output disk-spill no longer bypasses the VFS — core fix done 2026-06-06.**
  `output_limit.rs` used to write spill files via `paths::spill_dir()` + `tokio::fs`
  directly (not through `ctx.backend`), ignoring the VFS mount mode — exploitable for
  exfil staging, disk-fill DoS, or persistent artifacts with no feature beyond the
  default `localfs`, and invisible to the compile-time capability split (runtime path).
  Surfaced by dpal design review 2026-06-03; live-confirmed from kaibo's read-only
  `run_kaish` 2026-06-06. Fix: `OutputLimitConfig` gained a runtime `SpillMode`
  (`Disk` | `Memory`). `Memory` (builder `OutputLimitConfig::mcp().in_memory()`)
  truncates overflow in memory to a head+tail preview with **no disk I/O** — the
  streaming external path (`drain_in_memory`) keeps only a head + a `tail_bytes` ring
  buffer while draining to EOF, and oversized structured `OutputData` is rendered
  through a `write_canonical` byte budget rather than materialized into a full
  `String`, so memory stays bounded regardless of output size. Memory mode still
  remaps the exit to `3` (`did_spill = true`, real code in `original_code`); callers
  treat `3` as "output capped, not error". Disk vs memory distinguished by the `out`
  message ("truncated in memory … no spill file" vs "full output at <path>").
  `Kernel::assemble` auto-forces `Memory` whenever `vfs_mode == NoLocal` (overriding
  an explicit `Disk`), so a memory-only kernel can never write a host spill file;
  documented on `VfsMountMode::NoLocal`, `SpillMode::Disk`, `OutputLimitConfig::in_memory`.
  Residual follow-ups (localfs default, runtime switch, host /proc/etc) tracked in P1.

- **Dispatcher re-entrancy deadlock resolved — verified/pinned 2026-06-07.**
  `timeout 5 echo works` (and any builtin that re-dispatches through
  `ctx.dispatcher`) no longer deadlocks. The fix — drop the `exec_ctx.write()`
  guard before `tool.execute` via a snapshot-and-release in
  `execute_command_depth` (`kernel.rs` ~L2252) — already landed with the
  cancellation/concurrency rework (`719896f`); the issue text was stale. Audit
  confirms there is exactly one `tool.execute` site and it runs with no
  `exec_ctx` lock held. Hardened coverage:
  - Un-ignored the three `timeout::tests::*` that were quarantined for "lexer
    rejects `5s`/`100ms`" — that's also stale (`5s`/`100ms`/`500ms` lex fine
    now via the NumberIdent/bareword work), so all 7 timeout tests run.
  - Added `test_redispatch_does_not_deadlock`, which wraps the re-dispatch in
    `tokio::time::timeout(10s)` so a *future* regression (holding the guard
    across `tool.execute`) fails cleanly in 10s instead of hanging the suite —
    verified by temporarily re-introducing the guard (failed at exactly 10.00s)
    then reverting.
  - Fixed an adjacent bug surfaced by un-ignoring `test_timeout_builtin_times_out`:
    when the timer fired on a cancellation-aware *builtin* (`sleep` returns
    "sleep: interrupted"), timeout's `if err.is_empty()` guard masked the
    "timed out" reason. Now the authoritative "timed out after N" note is
    always surfaced, *appended* to any inner detail (no data loss).

- **Job output filenames now include the OS pid — flake + cross-process collision fixed 2026-06-07.**
  `write_output_file` named files `session_{session}_job_{id}.txt` in the shared
  `/tmp/kaish/jobs/`. `session_id` is a *process-local* atomic that restarts at
  0, so two kaish processes on one host — or two `cargo test --all` binaries —
  both wrote `session_0_job_1.txt` and clobbered each other. That was a real
  production cross-process collision *and* the source of the
  `test_cleanup_removes_temp_files` flake. Filenames now mix in
  `std::process::id()` (`session_{s}_job_{i}.{pid}.txt`), mirroring
  `output_limit`'s spill convention — unique across processes. Chosen over the
  issue's suggested per-test `tempfile::tempdir()` because pid namespacing fixes
  the production bug too, with a smaller change than threading an injected dir
  through the three `Job` constructors. (The separate "job files persist
  indefinitely" GC item in P3 is unchanged.) `--all` tests green.

- **`MemoryFs` `ensure_parents` TOCTOU closed — fixed 2026-06-07.**
  `ensure_parents` took and released its own write lock, then the caller
  re-locked for the mutation — a window where a concurrent task could remove or
  replace a parent dir between setup and mutation (`write`, `mkdir`, `symlink`,
  `rename`). Replaced it with a sync `ensure_parents_locked(&mut entries, path)`
  that operates on the caller's already-held guard, so each op does
  parent-creation + mutation atomically under one lock. `rename` keeps its
  intentional error-ignore (`let _ =`). The fix is structural (single guard) —
  a deterministic race test isn't feasible without flakiness, which this project
  avoids; the 37 existing `vfs::memory` tests cover the parent-creation
  behavior. clippy + `--all` tests green.

- **Builtin `sleep` honors cancellation — verified/pinned 2026-06-07.**
  `tools/builtin/sleep.rs` already raced `tokio::time::sleep(d)` against
  `ctx.cancel.cancelled()` (returns 130 on cancel) — the issue description was
  stale. Added `sleep::tests::test_sleep_honors_cancellation` (an
  already-cancelled token makes a 1h sleep return 130 near-instantly) so a
  regression to a bare `.await` fails the suite. The general "audit other
  long-blocking builtins" guidance is retained in P3.

- **Kernel no longer reads host `HOME` — hermetic by default — fixed 2026-06-07.**
  The construction-time `std::env::var("HOME")` read in `kernel.rs` is gone:
  `HOME` is owned entirely by the frontend via `initial_vars` (REPL/MCP seed it
  from `std::env::vars()`; a hermetic embedder leaves it empty and gets no
  HOME). Tilde expansion no longer reads the host env either — `expand_tilde`
  and `apply_tilde_expansion` now take `home: Option<&str>`, sourced from the
  kernel scope via a new `Kernel::scope_home()` and threaded through
  `build_args_async` / `build_args_flat` / `consume_flag_positionals`. With no
  HOME in scope, `~`/`~/path` stay literal instead of leaking the host home
  dir. `cd` with no args dropped its `std::env::var("HOME")` fallback (scope
  only, else `/`). `~user` → `/etc/passwd` is unchanged (already gated behind
  `host`). Tests: `interpreter::eval::tests::expand_tilde_*` (incl.
  `expand_tilde_hermetic_no_home_does_not_leak_host`) and the new
  `tests/hermetic_home_tests.rs` (e2e: seeded vs hermetic kernel through
  `kernel.execute`). Gates green in default + `--no-default-features` + WASI.

- **`kaish-tool-api` public types are now `#[non_exhaustive]` — fixed 2026-06-07.**
  Added `#[non_exhaustive]` to the data types out-of-tree tools construct and
  pattern-match: `ExecResult`, `ToolArgs`, `ToolSchema`, `ParamSchema`,
  `OutputData` (all `kaish-types`), `BackendError` (errors should be
  non-exhaustive), and `ValidationIssue` (`kaish-tool-api`). `OutputFormat`
  already carried it. Adding a field/variant in a minor release no longer
  breaks downstream exhaustive matches or struct literals. Blast radius was
  small — the types already routed through constructors; only `ParamSchema`
  needed new builders (`new` + `with_required`/`with_default`/
  `with_description`/`with_positional`), and the clap reflector + a few test
  helpers moved off struct literals onto them.

  **Deliberately not annotated:** the enums kaish matches heavily *in-tree*
  (`EntryType`, `IssueCode`, `Severity`). `#[non_exhaustive]` on those would
  force `_` arms in the kernel's cross-crate matches and *lose* the compiler's
  exhaustiveness check — a worse trade than the external-stability gain for
  enums external tools read but don't extend. Gates green: `--all` tests,
  clippy, `--no-default-features`, WASI.

- **Here-string `<<<` ambiguity now surfaces an actionable parse error — fixed 2026-06-07.**
  Two stdin sources on one command (`cat < a <<< b`, `cat <<< a <<< b`) used to
  fail with the generic "expected '=', or '('" — the error from the competing
  statement-level *assignment* alternative. Empirically confirmed that a
  `try_map` rejection inside `command_parser` cannot win this: chumsky's
  `choice` merge keeps the assignment alternative's error regardless of the
  span our custom error carries (even a valid `cat foo`, forced to error in
  `try_map`, surfaced the assignment message at 4..7, not ours at the command
  span). So the rule moved to a **post-parse structural scan** in
  `parse()` (`first_ambiguous_stdin`): `command_parser` now always builds the
  command, and `parse()` rejects any command with >1 stdin source with a clear
  message. Kept at parse time (not the validator) because validation is
  skippable (`skip_validation`) while `setup_stdin_redirects` is silently
  last-wins — parsing is the non-bypassable gate. Span is best-effort
  (start-of-source; redirects carry no AST span — precise columns would need
  spanning `Redirect`, deferred). Test:
  `parser_tests::ambiguous_stdin_surfaces_actionable_message`.

- **`gather` line-format no longer silently drops failures — fixed 2026-06-07.**
  `scheduler/scatter.rs` line format dropped failed workers via
  `.filter(|r| r.result.ok())` while JSON kept them as rows with an `"ok"`
  field — same data, fewer rows depending on `--json`, and a caller iterating
  `for x in $(gather)` saw fewer items than it scattered (silent data
  corruption). `gather_results` now returns a `GatherOutput { text,
  dropped_failures }`: the line format still renders only successful stdout
  rows (clean iteration data) but reports the failed item names, and `run`
  turns a non-empty `dropped_failures` into a loud non-zero exit (`code 1`)
  with an err naming the failed items, short-circuiting before post-gather so
  the truncated set isn't propagated downstream. JSON is unchanged (it already
  carries failures as rows, so `dropped_failures` stays empty). Tests:
  `scheduler::scatter::tests::{test_gather_results_lines_reports_dropped_failures,
  test_gather_results_json_keeps_failures_as_rows}`.

- **Non-finite `Value::Float` no longer collapses to null — fixed 2026-06-04.**
  `value_to_json` mapped NaN/Infinity to `null` (JSON has no representation for
  them), silently losing data on roundtrip. It now serializes the non-finite
  value to its string form (`"NaN"`, `"inf"`, `"-inf"`) so the information
  survives — favoring visibility over a silent fallback. Finite floats are
  unchanged. Tests: `result.rs` `value_to_json_finite_float_is_number`,
  `value_to_json_non_finite_float_serializes_to_string`.

- **`--json` now holds on the error path — fixed 2026-06-04.**
  `apply_output_format` early-returned when stdout was empty, so a failure with
  a populated `err` (e.g. `grep --json --bogus-flag`) leaked the message as
  plain text despite `--json`. The empty-stdout branch now emits a JSON error
  object `{"error": <err>, "code": <code>}` (and mirrors it onto `.data`) when
  the result is a failure with a non-empty `err`. A clean non-zero exit with no
  message (grep no-match, exit 1) and an empty success both stay empty — only
  diagnostic-bearing failures are wrapped. Tests in `output.rs`:
  `apply_output_format_emits_json_error_object_on_failure`,
  `apply_output_format_leaves_clean_no_match_empty`,
  `apply_output_format_empty_success_stays_empty`.

- **`printf` now cycles its format over extra operands — fixed 2026-06-04.**
  POSIX `printf` reuses the format string until all operands are consumed
  (`printf '%s\n' a b c` → `a\nb\nc\n`); kaish made a single pass and dropped
  the rest. `format_string.rs` now factors the per-pass loop into `format_pass`
  (returns the conversion count) and adds `format_string_cycling`, which repeats
  the format in operand-count chunks. A zero-conversion format prints exactly
  once (matches bash, guards the infinite loop); the final pass may run short
  and defaults missing operands. `printf.rs` calls the cycling variant; awk's
  `sprintf` keeps the single-pass `format_string`. Tests: `format_string`
  `test_cycling_*` (six cases) + `printf::test_printf_cycles_format_over_extra_args`.

- **Trace-context egress + the two minor OTel leftovers — done 2026-06-01.**
  The remaining OTel work landed in one batch:
  1. **Egress baggage merge (kernel).** `Kernel::run_inner` now echoes the
     embedder's incoming `ExecuteOptions.baggage` back onto the returned
     `ExecResult.baggage` via `telemetry::merge_egress_baggage`. Merge
     semantics: **tool-emitted entries win** on key collision (freshest
     execution-time value; the embedder already holds its own copy), and
     non-colliding embedder keys are added so the result is a single complete
     view. Unit tests in `telemetry.rs` + an egress assertion in
     `trace_context_tests.rs`.
  2. **MCP round-trip.** `ExecuteResult` gained a `baggage` field
     (`from_exec_result` carries it, omitted from the wire when empty), and the
     `execute` handler mirrors it onto the response `_meta` as a `baggage`
     object — symmetric with `trace_from_meta`'s ingress lift. Tests:
     `from_exec_result_carries_baggage`,
     `execute_echoes_baggage_onto_response_meta`,
     `execute_without_baggage_leaves_meta_unset`.
  3. **REPL env parity.** `kaish-repl::trace_options_from_env` reads
     `TRACEPARENT` / `TRACESTATE` / `BAGGAGE` (W3C baggage-header format) and
     the non-interactive entry points (`kaish script.kai`, `kaish -c '…'`) now
     route through `execute_with_options_streaming`, so `otel-cli exec -- kaish
     …` traces across the boundary. Interactive REPL still doesn't (no upstream
     trace in interactive use). Pure parser unit-tested via an injected env
     getter. (Exposing these in Kaijutsu's embedding is tracked there, not in
     the kernel.)
  4. **Forked spans nest under the execute span.** `bind_current_context` now
     captures the active execute span's OTel context (via the
     `tracing-opentelemetry` bridge, now a normal kernel dep) instead of the
     ambient `Context::current()`, so forked work (background jobs, scatter
     workers, concurrent pipeline stages) nests under the foreground span
     rather than becoming a sibling under the embedder's remote parent. Baggage
     propagation is preserved (the bridge stores `parent_cx.with_span(span)`);
     falls back to `Context::current()` when no bridge layer is installed.
     `trace_context_tests.rs` asserts workers parent onto a kaish-local span,
     not the remote parent.

- **`grep PATTERN f1 f2 …` searched only the first file — fixed 2026-05-28.**
  Same multi-positional class as `ls`: grep read only `get_string("path", 1)`,
  so `grep match a.txt b.txt` (and `grep p *.rs` after kernel glob
  pre-expansion) silently ignored every file after the first — matches in
  later files vanished from both text and `--json`. Now collects all
  `positional[1..]` operands and routes ≥2 files through the existing
  `grep_multiple_files` renderer (filename-prefixed, `show_filename: true`).
  Single-file and stdin paths unchanged (no prefix, POSIX). Regression tests:
  `builtin_kernel_tests::{grep_searches_all_files_and_prefixes_filenames,
  grep_glob_searches_all_matched_files}`.

- **Bare `.` argument parsed as `source` — fixed 2026-05-28 (parser).**
  Surfaced by the kernel-routed builtin breadth tests: `find .`, `ls .`,
  `echo .`, `wc -l .` all misbehaved — `echo .` printed `source: missing
  filename` instead of `.`. `primary_expr_parser` (every argument position)
  omitted bare `Token::Dot`, so a `.` argument was never consumed; the
  statement parser then started a fresh `source` (`.`) command. `find .`
  became two statements: `find` (defaulting to cwd → an unfiltered recursive
  listing) plus `.` (source, no file). Fix: accept `Token::Dot` as the
  literal `"."` in expression/argument position. The `source` alias is
  unaffected — `command_parser` consumes a *leading* `.` as the command name
  before args are parsed. Regression tests:
  `parser_tests::{bare_dot_argument_is_one_command, leading_dot_is_still_source}`
  and `builtin_kernel_tests::{dot_argument_is_literal_not_source,
  ls_dot_lists_current_directory, find_name_filters_to_matches_recursively,
  source_alias_still_works_in_command_position}`.

- **`cp SRC... DST/` trailing-slash failure — fixed 2026-05-28 (lexer).**
  Surfaced by the new kernel-routed builtin tests: `cp a.txt b.txt dest/`
  failed with `cp: dest: is a directory (use -r to copy)` though `... dest`
  worked. Root cause was the *lexer*, not `cp`: `RelativePath`'s bare form
  (`lexer.rs:367`) required ≥1 char after the slash (`...]+`), so `dest/`
  lexed as `Ident("dest")` + `Path("/")` — turning `cp a b dest/` into a
  4-operand command where `cp` took `/` as the destination and `dest` as a
  source. Changed the quantifier to `*` so a trailing slash stays attached
  (absolute `Path` already allowed it). Same tokenization-gap family as the
  dot-prefixed-filename and bare-`,` P4 entries. Regression tests:
  `lexer_tests::lexer_navigation_tokens` (`dest/`, `src/kaish/`) and
  `builtin_kernel_tests::cp_multiple_sources_into_directory_trailing_slash`.

- **`ls` multi-positional / glob-drops-all-but-first — fixed 2026-05-28.**
  The kernel pre-expands a bare glob into one `positional` per match, but
  `Ls::execute` read only `positional[0]` via `get_string("path", 0)`, so
  `ls crates/*/Cargo.toml` and `ls a.txt b.txt` listed only the first item.
  `execute` now collects all positionals and branches: a single target keeps
  the rich behavior (glob / file-as-name / dir contents / recursion) via a
  new `list_one`; multiple targets render one node per argument (directories
  shown by name, not expanded — the predictable structured-output choice)
  via a new `render_names` shared with `list_glob`. The old direct-`.execute()`
  unit tests never caught this because they passed raw glob strings straight
  to the builtin, bypassing kernel pre-expansion. Fixed alongside a new
  kernel-routed test harness (`tests/common::kernel_at`/`run`) and
  `tests/ls_tests.rs` (13 tests through the full kernel pipeline over a
  tempdir; 3 reproduced the bug red-first).

- **Concurrency — done (2026-04-10).** `execute_lock` + `Kernel::fork`
  land; `BackendDispatcher` demoted to `#[cfg(test)]`. See
  `reviews.md` Finding 1.
- **Scatter workers can run user tools / `.kai` scripts** — resolved
  by the fork refactor. Help text (`scatter.md:33`) updated in
  `7fb3d18`.
- **`ls` `too_many_arguments`** — resolved; schema-driven approach
  avoids the parameter count.
- **Old Feb-2026 integration-test bugs** — all five (socket-bind on
  unknown command, `$0` in script mode, alias-in-script concatenation,
  `$(kaish -c …)` capture, arithmetic token leak in subprocesses)
  validate as fixed on 2026-04-16.
- **Drop redundant `with_output_and_text(text(x), x)` in `tail`/`head`**
  — 2026-04-16. Both call sites (`tail.rs:175`, `head.rs:210`)
  switched to `with_output(OutputData::text(x))`.
- **`Token::is_keyword()` / `Token::starts_statement()` gaps** —
  2026-04-16. `is_keyword` now covers `While`/`Return`/`Break`/
  `Continue`/`Exit`; `starts_statement` now covers `While`. Unit
  tests added in `lexer.rs`.
- **`VfsRouter::read_only()` aggregation** — 2026-04-16. Returns
  `true` iff every mount is read-only; empty router stays `false`.
  Tests added in `vfs::router::tests`.
- **Scatter `limit=N` has no upper bound** — 2026-04-16. Clamped to
  `1..=10_000` (`SCATTER_LIMIT_MAX`) with `tracing::warn` on
  clamp-down. Help doc updated.
- **Adopt `ExecContext::expand_paths` in `ls`** — 2026-04-16.
  `list_glob` now uses the shared helper. Recursive `grep` was
  misidentified in prior reviews — it uses `FileWalker` for recursion,
  not the glob-expansion pattern, so no change is warranted there.
- **`#[ignore]` on pre-existing failing tests** — 2026-04-16.
  `test_exec_builtin` (exec replaces libtest binary) and three
  `timeout::tests::*` (`5s`/`100ms` lexer rejection) now carry
  `#[ignore = "..."]` with the reason. `test_timeout_numeric_duration`
  (distinct failure, P2) and `test_symlink_absolute_target_escape_blocked`
  (security-relevant, P2) were intentionally not silenced.
- **`test_symlink_absolute_target_escape_blocked` — fixed 2026-04-18.**
  `vfs/local.rs::symlink` was calling `self.resolve(target)` to validate
  absolute targets, but `resolve` strips the leading `/` and joins with
  root, so `/etc/passwd` always resolved to `<root>/etc/passwd` and the
  containment check trivially passed. Replaced with a direct
  canonical-target vs canonical-root comparison; non-existent absolute
  targets fall back to the literal path (rejected when outside root).
- **Heredoc unit-test expectations — fixed 2026-04-18.** `098f3fe` fixed
  the heredoc lexer to preserve the trailing newline and updated the
  integration tests in `heredoc_tests.rs`, but missed the seven unit
  tests in `lexer.rs` (`heredoc_simple`, `heredoc_multiline`,
  `heredoc_with_special_chars`, `heredoc_strip_tabs`, `heredoc_in_command`,
  `test_heredoc_preserves_leading_empty_lines`,
  `test_heredoc_quoted_delimiter_sets_literal`). Expected `content` strings
  now include the trailing `\n`.
- **`test_timeout_numeric_duration` — root cause identified, fix deferred
  2026-04-18.** The test was `#[ignore]`'d with a pointer to the P2
  "Dispatcher re-entrancy deadlock" entry, which captured the full topology
  and fix direction. *(Superseded — the deadlock is resolved as of 2026-06-07;
  see the entry at the top of this section.)*
- **Here-string `<<<`** — 2026-04-16. New `Token::HereString`,
  `RedirectKind::HereString`, parser arm in `redirect_parser()`, and
  stdin wiring in `setup_stdin_redirects` (append `\n`, match bash).
  Parser rejects any combination of `<`, `<<`, `<<<` on one command
  rather than "last wins". Snapshot and E2E coverage; see
  `docs/plan-here-string.md`.
- **`${VAR.field}` for JSON variables — decided-against 2026-04-16.**
  Discussion landed on keeping jq as the one field-access path.
  `jq -r '.key' <<< "$R"` is bash-idiomatic, shellcheck-clean, and
  kaish's jq is built in (native jaq, no subprocess). Pointer:
  `docs/plan-here-string.md`.
- **Clap-migration underscore-id leak + multi-positional sweep** —
  2026-05-28. Two clap-migration follow-ups found by post-fix audit.

  (a) **Underscore-id routing bug.** Builtins with `_field` (Rust's
  unused-field convention) leaked `_field` as the clap id, so the
  kernel's flag canonicalization emitted `--_field=value` and clap
  rejected it. Broke `kaish-validate -e EXPR`, `kaish-validate --expr
  VALUE` (space form), `read -p PROMPT`, `kaish-trash --confirm VALUE`
  (space form). Bool flags worked because the short-flag branch inserts
  the short char directly. Fix: added `#[arg(id = "name", ...)]` to ~21
  fields across diff/grep/kaish-trash/mkdir/read/tee/uname/validate.
  Side benefit: `help mkdir` / `help uname` no longer show `_parents`
  / `_s` etc. — schema names are clean. Regression test in
  `clap_schema::tests::id_override_strips_leading_underscore_from_schema_name`
  + integration tests for `kaish-validate -e` / `--expr` round-trip.

  (b) **Multi-positional sweep.** The pre-existing
  `args.get_string("path", 0)` pattern only read positional[0]; `mkdir
  a b c` silently dropped `b` and `c`. Converted mkdir/touch/rm/tee/cut
  /sort/stat/dirname/realpath/readlink to iterate `args.positional` via
  `crate::interpreter::value_to_string`. rm batches multi-path latch
  into a single nonce (NonceScope.paths is a set; one nonce validates
  any subset). cp/mv adopted POSIX `SRC... DST/` semantics: when more
  than one source is given, dest must be an existing directory. basename
  / uniq / validate kept single-positional per POSIX. Integration tests
  cover mkdir / touch / rm / dirname multi-arg behavior.

  Note: `ls <glob>` still has the same `positional[0]`-only shape and
  needs a paired fix that also decides how to render multiple roots —
  see the surviving P2 entry above.

  Workspace 1554 kernel unit + 129 REPL integration tests green; clippy
  clean (--all and --no-default-features); WASI build OK.

- **Schema fidelity + positional-index mismatch (clap migration)** —
  2026-05-28. The paired P2 from 2026-05-27 landed in a single batch:
  1. `ParamSchema` gained a `positional: bool` field (serde-skipped
     when false). New `ParamSchema::positional()` builder for hand-
     written schemas; jq_native's `filter` re-insert now uses it.
  2. `params_from_clap` now exposes hidden *positional* sinks while
     still skipping hidden *flag* args (clap's `is_positional()` is the
     oracle — `get_index()` returns None for derived `Vec<String>`
     positionals). Positional params are tagged accordingly.
  3. `validate_against_schema` (`traits.rs:42-150`) was refactored to
     split params into positional and flag groups and match positionals
     by their order *among positionals only*, never by the struct-field
     index. Required-positional and required-flag checks were split so
     each goes through its own matching path.
  4. `schema_param_lookup` (`scheduler/pipeline.rs:514`) now excludes
     positional params from the flag-name lookup so the kernel doesn't
     mis-route `cat --paths foo.txt` into named[paths]=foo.txt and
     starve `cat`'s positional[0] read.
  5. ~40 builtins' positional sinks were un-hidden with descriptive
     names + user-facing doc comments (`paths`, `pattern`, `command`,
     etc.). `help cat` now shows `paths : string` with description.
     A handful that conflict with same-named flags (mktemp `--template`,
     patch `--file`) or accept no real positionals (scatter, gather,
     pwd, kaish-status, …) stay hidden — documented inline.

  Unit-test coverage: clap_schema tests verify positional/flag
  detection; traits validator tests pin the index-mismatch regression
  (`required_positional_satisfied_when_positional_sits_after_flags`).
  Full workspace 1543 unit + integration suites green; clippy clean
  (--all and --no-default-features); WASI build OK.

- **Clap-migration review fixes** — 2026-05-27. Five findings from a
  code-review pass on commits 800a55b … 93fbd12 (clap sweep + parser
  split) landed in a single batch:
  1. `grep -F` no longer silently routes through regex matching;
     pattern is now `regex::escape`'d when -F is set (was: `grep -F
     "192.168.1.1"` matched `1X168Y1Z1`).
  2. `--json` is honored before `tool.execute()` runs via
     `GlobalFlags::apply_from_args(&tool_args, ctx)` in kernel.rs and
     dispatch.rs, so a clap parse-failure path doesn't drop the format
     on the floor. Per-builtin `parsed.global.apply(ctx)` is now
     idempotent backup.
  3. `validate_user_tool_args` in `validator/walker.rs` now counts
     `Arg::WordAssign` as positional (user tools aren't on the
     WordAssign allowlist, so runtime stringifies WA to positional —
     validator must agree).
  4. `flagify_bool_named()` hoisted from 12 byte-identical inline
     copies to a `ToolArgs` method in kaish-types. Documented behavior:
     `Bool(true)` → flag presence, `Bool(false)` dropped (matches
     clap's "absent ≡ false" model).
  5. Migrated builtins' long-flag names converted from snake to
     kebab (`--ignore_case` → `--ignore-case`); both forms accepted
     via `visible_alias` for back-compat. `params_from_clap` now
     exposes the long-flag name as an alias so the validator and
     kernel `schema_param_lookup` recognise the kebab form.

  Two paired findings (schema fidelity + positional index mismatch)
  and one limitation (`apply_output_format` empty-stdout early-return
  suppressing --json on errors) recorded as new P2 entries.

- **Heredoc span tracking** — 2026-04-16 (commits `c21e09c`
  source, `2490127` tests). Interpolated heredoc bodies now carry
  per-part byte offsets via `SpannedPart` and a new `Expr::HereDocBody`
  AST variant; validator attaches the span to any issue raised inside
  the body. `ParseError::format(source)` renders `line:col` with a
  source snippet. Literal heredocs and double-quoted strings keep the
  spanless paths for now — universal spanning is a separate refactor.
  The `skip_quoted_content()` extraction called out as a dependency
  was not needed for this fix; it remains a standalone P4 cleanup.
