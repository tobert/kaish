# kaish reviews

Narrative reviews of the codebase, oldest first. Concrete follow-ups
belong in [issues.md](issues.md) — this file is context for *why*.

---

## Code Review: kaish-kernel (April 2026)

**Date:** 2026-04-10
**Reviewer:** Claude
**Scope:** correctness, failure modes, architectural integrity

### Overview
A thorough review of the `kaish` kernel was conducted, focusing on correctness, failure modes, and architectural integrity. The system is well-structured and follows a "structured data first" philosophy.

**Validation pass (2026-04-10):** Each finding below was re-checked against the current source. Status tags reflect that audit; citations are file:line at the time of validation.

### Key Findings

#### 1. Concurrency & Data Races — **RESOLVED (2026-04-10)**

The `Kernel` struct is auto-`Send + Sync` but was **not safe under concurrent `execute()` calls on the same instance**.

- **State Clobbering — confirmed:** `dispatch_command` (`kernel.rs:3114-3150`) and `execute_pipeline` (`kernel.rs:1332-1408`) both snapshot scope/cwd/aliases out of the kernel's `RwLock`s into a local `ExecContext`, run, and then write the mutated state back. Two concurrent `execute()` calls can each take a snapshot, run, and clobber each other on write-back. Lost-update, not memory-unsafety.
- **Shared Stderr — confirmed:** One `tokio::sync::Mutex<StderrReceiver>` per kernel (`kernel.rs:461`), drained at `kernel.rs:845, 2325, 2382`. Concurrent `execute()` calls would race on `drain_lossy()` and one task can swallow another's stderr.
- **Was mitigated only by call sites:** The MCP server creates a fresh `Kernel` per request (`crates/kaish-mcp/src/server/execute.rs:206`), and the REPL is single-execute-at-a-time. The footgun affected embedders that kept a kernel around and called `execute()` from multiple tasks.

##### Resolution

Two complementary changes in `kaish-kernel`:

1. **Per-kernel `execute_lock`** serialises concurrent foreground `execute()` calls.
   - Field: `execute_lock: tokio::sync::Mutex<()>` on `Kernel`. Tokio's mutex is fair (FIFO) and *is* the queue.
   - Acquisition happens in `Kernel::execute_streaming` (public), which then delegates to a private `execute_streaming_inner`. `Kernel::execute` flows through `execute_streaming`, so the lock covers both entry points without any self-deadlock risk.
   - On contention, `Kernel::acquire_execute_lock` emits `tracing::warn!(target: "kaish::kernel::concurrency", …)` — silent serialisation is observable in logs, matching kaish's "no silent fallbacks" rule.
   - `execute_streaming`'s callback trait bound was relaxed to `+ Send` (`&mut (dyn FnMut(&ExecResult) + Send)`) so that `execute()` futures are `Send` and can be used from `tokio::spawn` — required for any real concurrent usage and for the regression suite below.

2. **`Kernel::fork()` + deletion of `BackendDispatcher`** unlocks *true* parallelism for background jobs, scatter workers, and concurrent pipeline stages.
   - New inherent method `Kernel::fork(&self) -> Arc<Self>`: snapshots per-session state (scope via COW Arc, user_tools, exec_ctx), Arc-shares read-mostly resources (`tools`, `vfs`, `jobs`, optional `terminal_state`), and freshly constructs per-fork resources (`stderr_receiver`, `cancel_token`, `execute_lock`). The returned Arc is `into_arc`'d so the fork's `self_weak` points at itself — nested dispatch through `ctx.dispatcher` (e.g. inside `timeout`) routes through the fork, not the parent.
   - `CommandDispatcher` trait gained an `async fn fork(&self) -> Arc<dyn CommandDispatcher>` method; the `Kernel` impl delegates to the inherent `Kernel::fork` via UFCS.
   - `Kernel::dispatch_command` now (a) populates `ctx.dispatcher` from `self.dispatcher()` at the top of every call, ensuring forks dispatch through themselves, and (b) syncs the streaming pipe endpoints (`pipe_stdin`, `pipe_stdout`, `stderr`) in addition to scope/cwd/aliases, so tools running under a fork see the right I/O wiring.
   - `execute_background` (`kernel.rs:1462`) forks instead of constructing a `BackendDispatcher`; the spawned task dispatches through the fork and therefore has access to user-defined tools, `.kai` scripts, and `$(...)` in arguments — all of which silently failed before.
   - `scheduler::pipeline::run_scatter_gather` and `run_pipeline` each `dispatcher.fork().await` per concurrent worker/stage (a single shared fork would reintroduce the same clobber we're fixing).
   - `scheduler::scatter::ScatterGatherRunner` dropped its `parallel_dispatcher` field. `run_parallel` now forks per worker from the sequential dispatcher. Each worker has independent mutable state.
   - `BackendDispatcher` is gated `#[cfg(test)]` — production code has no uses. The struct, its trait impl, its external-command fallback, and related imports all live only in test builds. The public re-export in `kaish-kernel/src/lib.rs:59` was removed.

- **Regression suite:** `crates/kaish-kernel/tests/concurrency_tests.rs` contains nine tests, all running under `#[tokio::test(flavor = "multi_thread", worker_threads = 4)]`:
  1. `concurrent_cwd_no_clobber` / `concurrent_var_no_clobber` / `concurrent_alias_no_clobber` — 4–8 tasks × 50 iterations each mutate and observe scope/cwd/aliases; must never see another task's values.
  2. `concurrent_stderr_isolation` — 4 tasks each tag their stderr; each `ExecResult` must only contain its own tag.
  3. `background_job_does_not_block_foreground` — the lock releases after `&` returns; a foreground `echo` completes within 300 ms of a 400 ms background sleep.
  4. `background_job_runs_user_function` — a POSIX-function user tool runs inside `&`. Before the fork refactor this silently errored with "command not found".
  5. `background_job_snapshot_isolation` / `parent_does_not_see_background_mutation` — the background fork's scope is a snapshot at spawn time, and mutations inside the fork do not leak back to the parent.
  6. `scatter_parallel_runs_user_function` — scatter parallel workers run a user function (previously impossible).

  **Counter-factual verified:** temporarily commenting out the `execute_lock` acquisition reliably crashes tests 1–3 with `cannot pop the root scope frame` — the pre-existing `pop_frame()` panic at `scope.rs:103` is no longer reachable under the lock.

- **Bonus:** the fork refactor also **fixed six pre-existing scatter tests in `scheduler::pipeline::tests`** that had been broken since commit `1f80be6` (March 19) because they required `ctx.dispatcher` to be set on an `ExecContext` that never had it. Scatter now materialises its own sequential dispatcher by calling `dispatcher.fork().await` on the `&dyn` dispatcher already passed down by `PipelineRunner::run`.

- **Known non-regressions:** the `test_exec_builtin` libtest hangs because `std::os::unix::process::CommandExt::exec` replaces the test binary itself; four `tools::builtin::timeout::tests::*` tests panic on the lexer's rejection of numeric-prefix identifiers like `5s`; and `vfs::local::tests::test_symlink_absolute_target_escape_blocked` fails. All five failures are present on clean `main` (verified via a throwaway worktree) and are untouched by this work.

- **Help text follow-up:** `crates/kaish-kernel/docs/help/scatter.md:33` was subsequently updated (commit `7fb3d18`) to describe the full dispatch chain.

#### 2. Word Splitting (Design Departure) — **VALID**
`kaish` explicitly avoids implicit word splitting.

- **Behavior — confirmed** (`kernel.rs:1016-1030`): the for-loop iterator special-cases JSON arrays (iterates elements) and otherwise treats the value as a single string. Inline comment: `"Strings are ONE value - no splitting! Use $(split "$VAR") for explicit splitting"`.
- **`split` builtin — confirmed** (`crates/kaish-kernel/src/tools/builtin/split.rs:160-168`): builds a `serde_json::Value::Array` and stores it on `result.data`, which is the field the for-loop reads.
- **Documented** in `docs/LANGUAGE.md:186-212`.
- **Note on the nominal "ExecResult.data" field:** `ExecResult` actually has *both* `data: Option<Value>` (parsed-JSON, public) and `output: Option<OutputData>` (structured render model, private). They are complementary; see `crates/kaish-types/src/result.rs:20-30`.

#### 3. Arithmetic Evaluation — **VALID**
- `checked_add/sub/mul` at `arithmetic.rs:147-148, 153-154, 172-173`.
- Division/modulo by zero `bail!` at `arithmetic.rs:178-179, 187-188`.
- Recursive descent parser: `ArithParser` at `arithmetic.rs:37`, precedence climb via `parse_comparison` (89), `parse_expr` (139), `parse_term` (164), `parse_unary` (201), `parse_primary` (217).

#### 4. VFS & Routing — **VALID**
- Longest-prefix matching: `router.rs:117-141` (`find_mount` compares `mount_path.as_os_str().len()`).
- Cross-mount `rename` rejected with `io::ErrorKind::Unsupported`: `router.rs:258-271` (Arc-pointer equality at 263).
- `resolve_real_path` bridges to backend's `real_path()` for external commands: `router.rs:101-104`.

#### 5. Lexer Complexity & Heredoc Spans — **VALID**
- Multi-pass design: `preprocess_arithmetic` (`lexer.rs:1110`) and `preprocess_heredocs` (`lexer.rs:1272`), driver at `lexer.rs:1627-1632`.
- Marker substitution: arithmetic at `lexer.rs:1666-1671` (`__KAISH_ARITH_{id}__`); heredoc at `lexer.rs:1675-1688`.
- **Heredoc span tracking — explicitly skipped:** comment at `lexer.rs:1631` literally states "heredoc span tracking is not implemented for simplicity." Errors inside heredocs report incorrect line/column.

#### 6. Scope COW Implementation — **VALID**
- `frames: Arc<Vec<HashMap<String, Value>>>` at `scope.rs:37`.
- `Arc::make_mut` clones the entire `Vec<HashMap>` on first mutation: `scope.rs:93, 125`.
- `${VAR.field}` only works for `$?`: `scope.rs:332-348`. Comment at 347-348: "For regular variables, only simple access is supported / No nested field access for regular variables." This is a real ergonomic gap given kaish's structured-data philosophy — every `--json` result you stash in a variable becomes opaque.

### Failure Mode Analysis

- **Uncaught Panics — VALID:** Sole panic site at `scope.rs:103` (`pop_frame` on root). Validator pairs `push_frame`/`pop_frame` for every block (`validator/walker.rs:206/215, 246/252, 267/271, 329/345`), so user scripts cannot reach it. Internal logic bugs still can.
- **Hidden Errors via stderr — VALID:** Same root cause as Finding 1; not exposed today because of fresh-kernel-per-execute, but real for shared-kernel embedders.
- **Silent Overflow — INVALID for arithmetic** (checked ops everywhere). Numeric literal parsing uses `i64::from_str` which returns a clean `Err`, so no silent overflow there either.

### Conclusion
The `kaish` kernel is a modern, structured take on the shell. The validation pass found no bogus claims — every finding is real in code.

**Status:**

1. **Concurrency contract — DONE.** Finding 1 is resolved via the per-kernel `execute_lock` plus `Kernel::fork`, with `BackendDispatcher` demoted to a test-only stub. Regression suite in `crates/kaish-kernel/tests/concurrency_tests.rs`.
2. **Heredoc span tracking** — still open. Small, isolated, silently degrades the diagnostic experience.
3. **`${VAR.field}` for regular JSON variables** — still open. Directly supports kaish's structured-data thesis.

---

## kaish review — fresh eyes pass (April 2026)

**Date:** 2026-04-16
**Reviewer:** Claude (Opus 4.7)
**Scope:** proposition · design · code state
**Status:** opinion piece, not a decision

A fresh-eyes pass on 会sh after the v0.5 cleanup, the concurrency
hardening commit (`05f2741`), and the language/scatter-help docs sync
(`7fb3d18`).

### TL;DR

kaish is in better shape than its docs suggest. The proposition is
sharp, the architecture has settled into something a second contributor
can navigate, the test/lint discipline is real (913 tests pass, zero
clippy warnings, zero `unsafe` outside two narrow Unix interop sites),
and the recent fork/execute_lock work closed the largest correctness
hole. The growth edges are now ergonomic — heredoc diagnostics,
`${VAR.field}` for JSON values, and a handful of papercut bugs from
February that didn't make the March cleanup batch. None of those block
1.0; pick the two that hurt agents most and ship.

The single architectural debt worth budgeting time for is
`kernel.rs::execute_stmt_flow` — 18 statement arms in one ~430-line
async match. Not broken, just the place where the next contributor (or
you, in three months) will have the worst time.

### Proposition

> *A predictable shell for AI agents. Embeddable, MCP-native, sh-subset
> that passes `shellcheck --enable=all`.*

This is the right wedge. The competing offerings are:

1. **Bare bash via subprocess.** Trains agents on the shell they've
   already memorised, but every nontrivial pipeline is a foot-shoot
   waiting to happen — `$IFS`, word splitting, exit-code propagation,
   `set -euo pipefail` semantics that surprise even humans.
2. **One-tool-per-MCP-call.** Composable nothing. Agents end up
   reinventing pipelines client-side by chaining tool calls, which is
   slower, lossier, and burns context.
3. **Custom DSLs.** Fight the training distribution. You lose the
   muscle memory bash gives you for free.

kaish takes door (1) and removes the footguns: no implicit word
splitting, structured `$(cmd)` results, strict booleans, pre-execution
validation, `--json` on every builtin, latch/trash gates on `rm`. The
sh subset means an agent's bash priors transfer; the deletions catch
mistakes the agent would otherwise make. That's a defensible thesis,
and the implementation honours it.

Two propositional risks worth flagging:

- **Strictness has to pay back fast.** When kaish rejects `if [ "$X" =
  yes ];`, the error needs to be unambiguous and the fix obvious. The
  validator and `kaish-validate` builtin are on the right track, but
  see the heredoc span finding below — bad spans destroy the leverage
  of pre-validation.
- **The MCP story is still the load-bearing one.** Embedding (kaijutsu)
  is real and EMBEDDING.md is good, but the install path most users
  hit is the MCP server. The per-request `Kernel::new()` + 16MB-stack
  thread is fine at agent-call rates today and will stop being fine if
  anyone runs kaish in a hot loop. Worth a perf budget before someone
  files a bug.

### Architecture — what's working

**Crate layout is correct.** The split that landed in v0.3.1 —
`kaish-types` as a pure-data leaf with no async runtime, kaish-kernel
on top, frontends as thin consumers — paid off. `kaish-types` is 1.4k
lines, exports cleanly, and makes the embedder API small enough to
reason about without pulling 60 transitive deps. This is the kind of
boundary you wish more libraries drew.

**The dispatch chain is one chain.** `CommandDispatcher::dispatch` is
the single execution path: special builtins → aliases → user tools →
builtins → `.kai` scripts → external → backend. `Kernel` is the
production impl; the test-only `BackendDispatcher` (now `#[cfg(test)]`,
correctly demoted) is the only other implementor. Pipeline stages,
scatter workers, and background jobs all go through the trait, so the
"can scatter run user functions?" class of bug is structurally fixed,
not patched.

**`Kernel::fork` is an elegant primitive.** Snapshot mutable
per-session state, Arc-share immutable infrastructure, fresh
streams/tokens/locks. One method that explains the entire concurrency
model. The `dispatch_command` self-rewiring (populating
`ctx.dispatcher` from `self.dispatcher()` so forks self-dispatch) is
exactly the kind of detail that's easy to get wrong and was got right.

**Output model.** `OutputData` (tree-of-tables) is the strongest
abstraction in the codebase. One model produces:
- canonical text for pipes,
- columns/colors for the REPL,
- bare JSON for `--json` and MCP consumers,
- byte-budgeted streaming via `write_canonical(budget)`.

Builtins return data; rendering is a frontend concern. This is the
discipline that lets MCP and REPL share one truth without forking
formatters.

**Discipline signals.** `unsafe_code = "deny"` workspace-wide,
`unwrap_used = "deny"`, `expect_used = "warn"` — and clippy is *clean*.
The 14 `#[allow]` sites in kernel/src are all narrow and locally
justified (cancel-token poison recovery, the async-signal-safe
`pre_exec` block in external command spawn, two `too_many_arguments`
in builtins acknowledged in follow-up work). The codebase walks its own
talk.

### Architecture — where the pressure is

#### `kernel.rs` is becoming a town

5,351 lines, with `Kernel::execute_stmt_flow` (~L1007–L1443) carrying
an 18-arm async match. Each arm reaches into `scope`, `exec_ctx`, and
`user_tools` RwLocks; some are 100+ lines (`For`, `While`, `Case`).
The file isn't broken — it's deliberately the kernel's central nervous
system — but it's the place where:

- adding a new statement type means navigating a long match,
- the `dispatch_command` ↔ `ExecContext` sync dance (L3224–3271) is
  duplicated near every call site that fields a fork,
- onboarding a contributor means a vertical scroll.

The natural refactor is `mod kernel/exec/{assignment, command,
pipeline, control, ...}` with `execute_stmt_flow` reduced to a
dispatch arm-per-module. Not urgent, but every quarter you defer it
costs more.

#### Lexer preprocessing is the diagnostic ceiling

Arithmetic (`$((expr))` → `__KAISH_ARITH_…__`) and heredocs (`<<EOF` →
`__KAISH_HEREDOC_…__`) are substituted *before* lexing. The marker
trick is correct and collision-resistant, but:

- `preprocess_heredocs` (lexer.rs:1272–1424) is ~150 lines of manual
  quote/escape/nesting state machine. `preprocess_arithmetic` (~150
  lines) duplicates the quote-handling subset.
- **Heredoc span tracking is explicitly punted** (lexer.rs:1631
  comment: "not implemented for simplicity"). Errors inside heredocs
  report wrong line/column.

The strictness pitch ("we catch errors before execution!") is only as
good as the diagnostic. A user-visible error pointing at the wrong
line erodes the trust the validator earned. This is the
highest-leverage internal-quality fix on the list.

#### `${VAR.field}` only works for `$?`

`scope.rs:332-348` — JSON results stashed in regular variables become
opaque. This is the gap that most directly contradicts the
structured-data thesis: the agent is told to think in structured
results, then loses access to them on `RESULT=$(some-tool --json)`.
Closing this is one of the highest-payoff small features left.

#### Scheduler — known but real

- No timeout on scatter/gather (a hung worker stalls forever).
- No upper bound on `scatter limit=N` (clamp to e.g. 10000).
- `gather` line-format silently filters failed tasks; JSON includes
  them. Asymmetric semantics on the same data is a footgun even by
  bash standards.
- Job output files in `/tmp/kaish/jobs/` only GC on explicit
  `cleanup()` — long-running embedders accumulate.
- `JobManager::spawn` `spin_loop()` busy-wait — works, wastes CPU on
  contention.

The `gather` filtering asymmetry is the one I'd promote to P1:
"depending on `--json`, the same input gives you different rows" is
the kind of bug that wastes hours.

#### Frontends — minor papercuts

- `kaish-mcp` per-request thread + 16MB stack: documented, fine
  today, will be a problem at scale. Worth a benchmark before it
  becomes a ticket.
- `kaish-mcp` resource list always re-traversed and
  `notify_resource_list_changed` fires unconditionally.
- `kaish-repl` highlighter and hinter are no-ops; multi-line paste
  re-validates on each keystroke.
- `kaish-client::EmbeddedClient::shutdown()` is intentionally a
  no-op — sensible (embedder owns lifecycle) but worth a doc line.

None of these are urgent. They're the kind of polish that earns trust
once the language work is done.

### Test posture

| signal | value |
|---|---|
| `cargo test --all` | **913 passed, 0 failed, 13 ignored** |
| `cargo clippy --all` | **0 warnings** |
| Snapshot files | **138** (insta) |
| `#[test]` declarations | 2,227 (inflated by rstest expansion + parameterised cases) |
| `TODO/FIXME/XXX/HACK` markers | **44 across 7 files** |
| `unsafe` blocks | 2 (terminal init, external-command `pre_exec`) |
| Workspace `unsafe_code` | `deny` |
| Workspace `unwrap_used` | `deny` |

The new `concurrency_tests.rs` suite (9 tests, multi-thread tokio,
50-iteration mutator races) is the right shape for the kind of bug it
guards against. The counter-factual from the 2026-04-10 review
(temporarily disabling `execute_lock` reliably crashes 1–3) is exactly
the test discipline this kind of work needs.

The two known-flaky areas — `test_exec_builtin` (replaces test
binary), and the four `timeout::tests::*` (lexer rejects `5s`) — are
honest pre-existing failures. Worth a one-line `#[ignore = "reason"]`
so they stop being visual noise.

### Documentation posture

`LANGUAGE.md` and `EMBEDDING.md` are exemplary: present-tense, complete,
no aspirational sections. The MCP install snippet in the README is
specific enough to copy.

### Risks the codebase is *not* obviously addressing

- **Bash priors that aren't lexically caught.** Agents will write
  things like `[ -z "$VAR" ]` (single bracket — supported but
  discouraged) or `for i in $LIST` (no split — silently iterates
  once). The validator could grow a "did-you-mean" pass that points
  these out without rejecting them, in the same spirit as
  shellcheck's actionable warnings. Optional but high-leverage.
- **External-command surface area.** `try_execute_external` is the
  escape hatch from kaish's safety guarantees. The
  `allow_external_commands` flag gates it cleanly, but agents in MCP
  contexts where external is on still get the full bash footgun
  surface for any tool they call. There's no clean fix here — it's
  the contract — but worth saying out loud.
- **Heredoc + arithmetic preprocessing as a coupled pair.** Both
  passes share quote-handling logic and both substitute markers; a
  `skip_quoted_content()` extraction would reduce the surface area
  for the heredoc span-tracking work.

### Closing

This is one of the better-disciplined Rust codebases I've read in a
shell-sized project. The concurrency contract is now real, the
dispatch chain is one chain, the type system is small and exported
well, and the test posture is honest. The remaining work is the work
of a project asymptoting on 1.0 — not refactor-the-world stuff, just
the last 10% of polish that converts a sharp idea into a tool people
trust.

The single decision worth making now: **what does 1.0 mean for kaish?**
If it's "MCP server agents can't trip over" the P0/P1 items in
[issues.md](issues.md) deliver that. If it's "embedders can build
kaijutsu-class apps without surprise" the EMBEDDING.md guarantees + a
kaijutsu integration test matter more than the language polish.
Picking one tightens the followup queue considerably.

頑張って！

---

## Systemic review: test effectiveness · doc accuracy · doc resonance (June 2026)

**Date:** 2026-06-09
**Reviewer:** Claude (Fable 5), orchestrating a 61-agent fleet, with
DeepSeek-V4-Pro and Gemini 3 Pro as an external resonance panel
**Scope:** would the tests fail when we're wrong? do the docs tell the truth?
do the docs *land* for their audiences?
**Method:** seven parallel analysis dimensions (3 test, 4 doc); every P1–P3
finding handed to an adversarial verifier instructed to refute it; LANGUAGE.md
claims verified by *executing* them against the v0.8.0 binary; the resonance
panel read five docs cold from two personas (fresh MCP agent, evaluating staff
engineer). 52 findings verified: 49 confirmed, 3 refuted. Concrete follow-ups
are all in [issues.md](issues.md); panel report in
[resonance-2026-06.md](resonance-2026-06.md). This is the tradition pass —
done each model generation, after the April 2026 Opus 4.7 fresh-eyes review.

### TL;DR

kaish's core discipline is real and has *improved*: the builtin coverage gap
flagged in May (55/85 without kernel-routed tests) is down to ~20/89
inline-only, the bash-compat dual-run harness genuinely detects divergence,
SpillMode/cancellation/concurrency are tested at the level the April review
asked for, and 037aa63's host-side-channel refusal is pinned at three layers.
The README builtin table matches the registry exactly and the generated
syntax.md passes its drift test — the *generated* doc pipeline works.

The two systemic weaknesses are mirror images of each other:

1. **Tests guard the layer below the one that changes.** The latch/trash
   safety rails, all 43 "realworld" builtin tests, and rg's 27-flag surface
   are tested by hand-building `ToolArgs` — below the lex→parse→clap-binding
   pipeline that the last three schema commits reworked. The machinery works
   (verified by hand at HEAD); nothing pins it.
2. **Hand-written docs drift; nobody executes them.** Running LANGUAGE.md
   against the binary surfaced ~15 verified falsehoods — including three real
   semantic bugs the doc examples *exposed* (`[[ ! ... || ... ]]` precedence
   inverted from the parser's own grammar comment; `break 2` discarding
   accumulated output; `${NAME:-"default"}` keeping literal quotes). The doc
   wasn't just stale — it was the best bug-finding tool in this review.

### Test effectiveness

**What's strong.** The inline lexer suite pins the preprocessor fixes,
backtick rejection in four contexts, and heredoc contracts. The
`shell_compat!` kaish_eq/bash_eq macro is excellent design — intended
divergence is *recorded*, and the bash leg actually passes when enabled
(71+27 green, verified). Cancellation tests kill real PIDs and verify
SIGTERM→SIGKILL escalation. The flagship guarantees — no word splitting (in
for-loops and builtins), newline-only substitution split, strict-glob errors,
backtick rejection — are all deterministically pinned somewhere.

**Where the suite lies to us.**
- A parser snapshot **blesses a real bug**: `echo -- -not-a-flag` → three
  args, comment acknowledging the lexer split, snapshot approving it. Fixing
  the bug will fail the test. That's the inversion of what a test is for.
- `tests/common` forces latch and trash **off** for every harness kernel, so
  the destructive-op rails — the features that justify "fail loud, not
  silent" — have zero kernel-routed coverage, and the
  `RmAction::Trash`-failure invariant ("never fall through to permanent
  delete") is pinned by nothing.
- `realworld_builtin_tests.rs` is named for the property it doesn't have.
- Tautologies exist (`is_ok() || is_err()`; a sandbox assert whose second
  clause is implied by its first), 7 of 20 advertised validator codes are
  never emitted by any code path, and `background_job_snapshot_isolation`
  backgrounds the echo but not the sleep, so it never exercises its race.
- The minimal build **regressed silently**: an ungated test import broke
  `--lib --no-default-features` and the `cargo check` gate can't see test
  code. The sandbox configuration is currently certified by check/clippy
  only — and there is still no CI to notice.

**Refutations worth keeping** (the verifiers earned their tokens):
- "Strict-glob zero-match is unpinned" — refuted; a deterministic inline test
  (`test_bare_glob_no_matches_errors`) exists, though only for one of three
  "no matches" sites. The surrounding environment-dependent tests are still
  weak, but the guarantee holds.
- "kill has no tests at all" — refuted; a real PTY test kills a stopped job
  end-to-end. The honest residual is that it's PTY-only and unix-gated.
- "printf line-separation was removed" — refuted; `accumulate_result` still
  inserts separators — the `;`-sequence path *bypasses* it, which is a more
  precise (and more interesting) bug than the claim.

### Doc accuracy

Executing the docs found: three semantic bugs (above), four hard parse errors
documented as working syntax (`wait %1`, `kill %N`, `set -o output-limit=8K`,
`[ expr ]`), one feature whose documented runtime control is entirely
unreachable (output-limit: parse error + `set`-keyword collision + a
persistence bug + a wrong default, all in one help topic), scatter's canonical
examples silently degrading to one worker, a `/git/` VFS mount that has never
existed at HEAD documented in two files plus CLAUDE.md, and `limits.md`
contradicting `scatter.md` about what works in workers (limits.md lost —
the fork refactor landed; the help corpus didn't notice).

EMBEDDING.md is the worst single file: it predates the 0.8.0 capability
split, four of its eight samples don't compile (private `result.out`, removed
`Tool::execute` signature, zero-arg `LocalBackend::new()`, `&str`-for-`&Path`),
it promotes a deprecated entry point, and it never mentions cargo features,
`ExecuteOptions`, or `with_backend` hermeticity — the three things a 2026
embedder must know. Meanwhile the *generated* docs passed every spot-check.
The lesson is structural, not moral: **docs that compile from code stay true;
docs that narrate code rot in ~5 weeks.** More of LANGUAGE.md and
EMBEDDING.md should become generated or doc-tested (the `result.out` snippet
would be caught by a single doctest).

issues.md itself validated well: all P1s accurate, every spot-checked
Resolved entry genuinely resolved, four citations drifted (refreshed), two
entries were fixed-but-listed-open (closed), one had regressed (reopened
louder). The punch-list discipline works.

### Doc resonance

Full report in [resonance-2026-06.md](resonance-2026-06.md). The convergent
signal — both model families, independently, ranked first — is that the
README MCP section contradicts itself about JSON output and never states the
`execute()` return contract; the accuracy pass found the same section
documents a `help` tool that doesn't exist. One section, three failure modes,
first thing an integrator reads.

The panel's sharpest observation: kaish's strongest security engineering
(capability features, `allow_external_commands`, `with_backend` hermeticity)
is **invisible** in the docs they read — Gemini reconstructed the external-
command path as an undisclosed sandbox escape. The work is done; only the
telling is missing. Cheapest credibility win available.

What resonates, per both models: the ShellCheck-aligned divergence framing,
quote-to-join's paired explanation, latch/trash nonce lifecycle, the
cancellation cascade, hermetic env. The pattern: the beloved sections state
*contracts* (tables, codes, exhaustive lists); the weak ones narrate
features. DeepSeek's framing is worth adopting as a doc style rule: *"tie
every divergence from bash to a specific ShellCheck code."*

### Closing

The April review said kaish was "asymptoting on 1.0" and the remaining work
was polish. This review sharpens that: the *code* is asymptoting; the
**contract surfaces** — tests that pin the arg-binding layer, docs that state
return shapes and lifecycles — are a release behind. Every finding here is
the same finding at different altitudes: kaish's behavior is better than its
own description of itself. For a shell whose entire thesis is predictability
for agents that can only read the description, closing that gap *is* the 1.0
work.

Kaizen note for the next model-generation pass: the doc-accuracy dimension
(execute every example) and the adversarial-verify stage both paid for
themselves — three findings refuted before they could pollute the punch list,
three bugs found by running documentation. Keep both. Add: run the bash-compat
leg, and `cargo test --lib --no-default-features --no-run`, as part of the
review preflight — both regressions this round were invisible to the default
gates.

また次のモデルで会いましょう。

