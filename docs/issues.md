# Known Issues & Open Work

Actionable punch list. Narrative context lives in [reviews.md](reviews.md).
Last validation pass: 2026-05-28 (after schema-fidelity / positional-index fix).

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

### Native builtins bypass the kaijutsu capability allow-set
When kaish runs inside a kaijutsu context (via `context_shell` / rc scripts),
the kernel enforces a per-context capability allow-set (deny-by-default;
`ContextToolBinding`). Tool calls routed through `broker.call_tool`
(`builtin.file:read`, MCP-registered tools) ARE gated. But kaish's **own native
builtins** (file ops, `ls`, `rg`, glob, …) execute directly against the backend
and are **not** subject to that allow-set. Net effect: granting a context the
`shell` facade ≈ granting the whole native-kaish surface, so a "read-only"-style
role that is given shell can still mutate via native builtins. Decide the model:
either route native builtins through a capability check, or document that
`shell` is all-or-nothing and roles must withhold it (as `explorer` does today).
Surfaced 2026-06-03 during the kaijutsu deny-by-default capability rework.

### Space-separated flag values to `kj` are dropped (`--flag val` vs `--flag=val`)
`kj context create exp --type explorer` (space form) silently loses the value —
the context is created as `default`, not `explorer`. `kj context create exp
--type=explorer` (equals form) works. So a flag value passed as a separate token
to a `kj` builtin invoked from kaish doesn't reach the builtin's argv; only the
`=` form survives. Reproduced 2026-06-03 against the live kernel (one context per
form, checked `context_type` in the DB). Was invisible until kaijutsu went
deny-by-default: previously every context was permissive regardless of type, so a
dropped `--type` had no observable effect. Now it's **security-relevant** —
asking for a restricted role (e.g. read-only `explorer`) and silently getting a
fully-permissive `default` is a privilege-escalation-by-typo. Likely the kaish
tokenizer/arg-passing for `kj` (the `=` form being one token is the tell);
related to the arg-handling quirks in `gotcha_kaish_kj_args`. Fix in the
kaish→kj arg path, or have `kj` reject/normalize the space form.


<!-- Resolved: see docs/plan-here-string.md. Decided against extending
`${VAR.field}` to regular variables (non-bash, shellcheck-incompatible).
Instead, added here-string `<<<` so the natural agent pattern is
`jq -r '.key' <<< "$RESULT"`. kaish ships a native in-process jq. -->

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

### Output disk-spill bypasses the VFS — defeats a runtime read-only kaish
**Core fix landed 2026-06-06.** `OutputLimitConfig` now carries a runtime
`SpillMode` (`Disk` | `Memory`). `Memory` mode (builder: `OutputLimitConfig::mcp().in_memory()`)
truncates overflow in memory to a head+tail preview with **no disk I/O** — no
`paths::spill_dir()` write, no recoverable file — so a runtime read-only kernel
(kaibo) can no longer stage data-exfil / disk-fill artifacts via large output.
Memory use is bounded regardless of output size: the streaming external-command
path (`drain_in_memory`) keeps only a head + a `tail_bytes` ring buffer while
draining to EOF, and oversized structured `OutputData` is rendered through a
`write_canonical` byte budget instead of being materialized into a full `String`.

Original report (context): `output_limit.rs` wrote spill files via
`paths::spill_dir()` + `tokio::fs` directly, NOT through `ctx.backend`, ignoring
the VFS mount mode — exploitable for exfil staging, disk-fill DoS, or persistent
artifacts with no feature beyond the default `localfs`. The compile-time
capability split can't catch this (runtime path). Surfaced by dpal design review
2026-06-03; live-confirmed from kaibo's read-only `run_kaish` 2026-06-06.

**Design decision (2026-06-06):** Memory mode still remaps the exit code to `3`
(`did_spill = true`, real code in `original_code`) — the caller-facing wrinkle
(a successful big `cat` reading as a "failure") is resolved by callers treating
`3` as "output capped, not error", NOT by preserving the real code. Disk vs
memory is distinguished by the `out` message: memory says "truncated in memory …
no spill file", disk says "full output at <path>".

**Auto-force for NoLocal landed 2026-06-06.** `Kernel::assemble` now forces
`SpillMode::Memory` whenever `vfs_mode == NoLocal` (overriding an explicit
`Disk`), since a memory-only kernel has no host filesystem to spill to. A
`NoLocal` kernel can no longer write a host spill file regardless of caller
config. Documented on `VfsMountMode::NoLocal`, `SpillMode::Disk`, and
`OutputLimitConfig::in_memory`.

**Residual / follow-ups:**
- `Disk` is still the default for `localfs` mounts. A `Sandboxed`/`Passthrough`
  kernel run read-only that does *not* opt into `Memory` mode still spills to
  disk — there is no dedicated runtime read-only flag yet, so `NoLocal` is the
  only auto-trigger. Embedders wanting the guarantee on a `localfs` mount must
  set `.in_memory()` (kaibo does). Widen the auto-force when a read-only flag lands.
- No runtime switch via the `kaish-output-limit` builtin / `set -o` yet — mode is
  config-only. Add a `memory`/`disk` subcommand if interactive control is wanted.
- `host`'s `/proc` and `/etc` reads still bypass the VFS by design — if
  "read-only" is ever marketed as "no host observation," those need a runtime
  gate too.

---

## P2 — Focused refactors & real bugs

### Composable help/instructions library (`kaish-help` crate) — Phases 1–3 done
**Phase 1 landed 2026-06-06:** `kaish-help` crate created (concept fragment model +
`compose`/recipes + byte-stable `get_help` compat surface); help content moved to
`crates/kaish-help/content/en/`; `kaish_kernel::help` is now a shim. Tests/clippy/
WASI green. **Remaining:**
- **Phase 2 (done 2026-06-06, light):** `syntax.md` is now generated from
  `Syntax` fragments (`render_syntax_reference()` + `regen_syntax` example),
  drift-tested; byte-identical to the old file. `LANGUAGE.md` stays hand-authored
  (full decomposition declined) with a coverage test.
- **Phase 3 (done 2026-06-06):** MCP `instructions:`, the REPL welcome, the `execute`
  **tool description**, and the MCP **prompt set** all compose from the canonical
  `kaish-help` corpus — no hand-rolled prose or hand-maintained lists left in the MCP
  frontend.
  - The tool description is now built in the `list_tools` runtime override from
    `compose(Recipe::tool_description, …)` plus MCP-frontend framing (the
    `#[tool(description=…)]` macro literal is reduced to a stable one-line fallback).
  - The prompts dropped the `#[prompt_router]`/`#[prompt]` macros: `list_prompts`/
    `get_prompt` are manual and single-source from `help::list_topics()` (one
    `kaish-<topic>` prompt per topic, rendered via `get_help`). This also exposes the
    previously-omitted `ignore` and `output-limit` topics as prompts (6 → 8), and
    unknown prompt names now fail loudly instead of serving generic help.
- **Phase 4:** publish; kaijutsu/kaibo adopt `kaish-help`.
- **Phase 5:** i18n scaffolding + first `ja` fragments.

Full design + resolved decisions: [composable-help.md](composable-help.md).

### Minimal build (`--no-default-features`) — integration test binaries don't compile
**Lib unit tests fixed 2026-06-07.** The cited blocker (`ignore_config.rs` L582
calling `tokio::fs::write`, which needs tokio's `fs` feature, off in the minimal
build) is resolved — the fixture now uses `std::fs::write`, faithful to the
production read (`build_filter` uses `std::fs::read_to_string`). `cargo test
-p kaish-kernel --lib --no-default-features` now passes (1320 tests).

**Remaining:** the *integration* test binaries (`tests/*.rs`) still don't
compile minimally. 8 of 23 files use `KernelConfig::repl()` / the `kernel_at`
harness, both `#[cfg(feature = "localfs")]` (real-FS) — so `cargo test
-p kaish-kernel --no-default-features` (which builds the integration binaries
too) fails to compile those. Options: (a) file-level `#![cfg(feature =
"localfs")]` on the inherently real-FS binaries so they're skipped minimally;
(b) convert the ones that don't actually need real FS (e.g. `validation_tests`)
to `KernelConfig::isolated()` (memory VFS) so they *run* minimally — strictly
better coverage. `hermetic_home_tests.rs` already uses `isolated()` and runs in
both modes as the pattern to follow. Surfaced 2026-06-03; partially resolved
2026-06-07. Folds naturally into the planned `native`→capability-feature split.

### Split `kernel.rs::execute_stmt_flow`
~L1007–L1443 is an 18-arm async match. Each arm reaches into `scope`,
`exec_ctx`, and `user_tools` RwLocks; `For`/`While`/`Case` are 100+
lines apiece. Natural refactor: `mod kernel/exec/{assignment, command,
pipeline, control, …}` with `execute_stmt_flow` reduced to a dispatch
arm-per-module.

### Extract `dispatch_command` ctx-sync helper
The six-field `ExecContext` ↔ kernel-state sync appears near every
call site that fields a fork (`kernel.rs:3224-3271` and duplicates).
One helper, one truth.

### Dispatcher re-entrancy deadlock (P2, real bug surfaced by timeout)
`timeout 5 echo works` deadlocks in production and the test
`test_timeout_numeric_duration` is `#[ignore]`'d for the same reason.
Root topology:
1. `execute_command` (`kernel.rs:1788`) acquires `self.exec_ctx.write()`
   and holds it across `tool.execute(...)`.
2. `timeout` re-dispatches via `ctx.dispatcher`.
3. The re-dispatch path (`dispatch_command` or `Kernel::fork`) tries to
   acquire `self.exec_ctx` again — deadlock.

Two fixes considered, both rejected for this release:
- Sync `ctx.dispatcher` into `self.exec_ctx` so the tool sees it (trivial
  one-liner in `dispatch_command`'s sync block). Makes the dispatcher
  visible but converts a fast-fail "no dispatcher" error into a silent
  5-second hang, so worse UX.
- Have timeout use `dispatcher.fork().await`. `Kernel::fork` itself
  reads parent `exec_ctx` and deadlocks on the outer write guard.

Real fix: restructure `execute_command` so the `exec_ctx.write()` guard
drops before `tool.execute`, with a snapshot-and-merge pattern like
forks already use elsewhere. Tracked with the `Split execute_stmt_flow`
refactor above since both touch the same lock discipline.

Tool-side blast radius is limited to builtins that re-dispatch —
currently only `timeout`. External-command path and normal pipelines
are unaffected (they don't re-enter the dispatcher from inside a tool).

### grep `-E` is pure no-op
`crates/kaish-kernel/src/tools/builtin/grep.rs:74-78`. Rust's regex
crate is always extended, so `-E` semantically aliases to default.
Currently declared as `_extended: bool` accept-and-ignore. Options:
(a) keep the no-op for POSIX/muscle-memory compatibility (current);
(b) remove the field, let clap reject `-E` so users learn it's
unnecessary; (c) note in the help text. (a) is lowest churn — leave
unless someone reports surprise.

---

## P3 — Scheduler and infra

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

### `dispatch_command` cancel sync is one-way (in only)
`ec.cancel = ctx.cancel` is synced INTO `self.exec_ctx` at the start of
each dispatch, but there's no `ctx.cancel = ec.cancel` reverse sync at
the end. Today this is fine because the only known mutators of
`ctx.cancel` (the `timeout` builtin) save and restore the original token
themselves. If we add another builtin that wants to propagate a token
swap *outward* through dispatch_command, it would need either explicit
restoration or a reverse sync added.

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
`sleep` itself is **done** (see Resolved 2026-06-07): it races
`tokio::time::sleep` against `ctx.cancel.cancelled()` and returns 130 on
cancel, pinned by `sleep::tests::test_sleep_honors_cancellation`. The general
guidance still stands for any *other* builtin that holds a long time/IO future
without yielding through `ctx.cancel` — audit and apply the same `tokio::select!`
pattern where one is found (none currently known besides the now-fixed `sleep`).

### Job output files in `/tmp/kaish/jobs/` persist indefinitely
`JobManager` only cleans up on explicit `cleanup()` / `remove()`. No
automatic GC of stale files from crashed sessions or old jobs.
Long-running embedders accumulate. Prune on kernel start and on a
configurable interval.

### `JobManager::spawn` busy-waits
Uses `std::hint::spin_loop()` to guarantee immediate visibility. Works,
wastes CPU on contention. Channel-based coordination would be cleaner.

### MCP per-request OS thread + 16MB stack
`crates/kaish-mcp/src/server/execute.rs:154-157` documents it. Fine at
agent-call rates, will be a problem in a hot loop. Replace with a pool
of LocalSet workers + mpsc channel. Benchmark first to confirm the win.

### MCP resource list always re-traversed
`notify_resource_list_changed` fires unconditionally. Cheap fix;
diff against previous snapshot before notifying.

### MCP resource watcher channel fixed at 256
`subscriptions.rs:33` bounds the file-watch channel; high-churn
environments drop events silently.

### `scheduler::job::tests::test_cleanup_removes_temp_files` flake
Test reaches into the shared real-FS `/tmp/kaish/jobs/` path and races
parallel test runs. Passes in isolation, fails intermittently under
`cargo test --all`. Same root cause as the "job output files in
`/tmp/kaish/jobs/` persist indefinitely" entry above. Switch the
JobManager test path to `tempfile::tempdir()` (each test gets its own
root) until the GC fix lands.

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

### `touch .hidden.txt` fails — dot-prefixed filenames mis-tokenized
The lexer splits `.hidden.txt` into `Dot` + `Ident("hidden.txt")`
rather than a single filename token. `DotSlashPath` (`./foo`) works;
bare dot-prefixed names don't. Workaround: quote the name.

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

### chumsky from git → crates.io
`LANGUAGE.md` Build/Development note is honest but should resolve
before 1.0. Either wait for chumsky 1.0 or pin a specific git commit.

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

### `EmbeddedClient::shutdown()` is a no-op
Intentional (embedder owns lifecycle) but worth one doc line in
`EMBEDDING.md`.

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
  2026-04-18.** The test is `#[ignore]`'d with a pointer to the P2
  "Dispatcher re-entrancy deadlock" entry above, which captures the full
  topology and fix direction.
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
