# Known Issues & Open Work

Actionable punch list. Narrative context lives in [reviews.md](reviews.md).
Last validation pass: 2026-04-16 (updated after heredoc-span work).

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

### Here-string `<<<` — parse-error message polish
`parser.rs` command_parser `.try_map(...)` emits
"multiple stdin redirects on one command are ambiguous" when two of
`<`, `<<`, `<<<` appear on the same command. Chumsky's alternative
backtracking surfaces a less specific "expected '=', or '('" at the
first redirect instead. Behaviour is correct (loud parse error);
message should be narrowed so the actionable reason is surfaced.

### `gather` line-format silently drops failures
`scheduler/scatter.rs:299` — `.filter(|r| r.result.ok())` drops failed
tasks in line format; JSON format (`:284-290`) includes them with an
`"ok"` field. Same data, different rows depending on `--json`. Either
match JSON behaviour with an explicit failure marker, or fail loudly
when results are dropped.

---

## P2 — Focused refactors & real bugs

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

### `ls <glob>` only lists the first match under kernel pre-expansion
Surfaced by the 2026-04-16 cleanup's smoke check: `ls crates/*/Cargo.toml`
from the CLI returns only `crates/kaish-client/Cargo.toml`, not all 7.
Root cause: the kernel pre-expands `GlobPattern` args into multiple
`tool_args.positional` values (`kernel.rs:1851-1860`), but
`ls.execute` reads only `positional[0]` via `args.get_string("path", 0)`
(`ls.rs:86-88`). Same shape may affect other builtins that only
inspect `positional[0]`. `cat`/`head`/`tail` etc. use
`expand_paths(&args.positional)` so they loop over all positionals;
`ls` needs the same fix plus a decision on how to render multiple
roots. Tests pass because the unit tests pass raw glob strings to
`Ls.execute` directly, bypassing the kernel's pre-expansion.

### Schema fidelity + positional-index mismatch (clap migration, paired)
**Both surfaced by code review on 2026-05-27 of commits 800a55b /
189c8db / a5ea7e1.** Two paired regressions from the clap sweep:

**(a) `schema_from_clap` excludes hidden positional sinks.**
Every migrated builtin uses `#[arg(hide = true)] rest: Vec<String>` (or
`paths`, `words`, etc.) to absorb positionals so clap doesn't reject
them. `params_from_clap` skips hidden args, so the resulting
`ToolSchema.params` contains **zero positional entries** for migrated
builtins. `help cat` no longer documents the `path` parameter; MCP
schema consumers learn nothing about what positionals each builtin
accepts. Pre-sweep hand-written schemas DID declare positionals.

**(b) `validate_against_schema` matches positionals by index in
`schema.params`.** `traits.rs:46-66` walks `params.iter().enumerate()`
and treats each index as both the param slot AND the positional slot.
This worked when hand-written schemas put positionals first. If (a) is
fixed by un-hiding the sinks, positionals now sit AFTER flags in
clap-derived order — `mkdir foo` would falsely error "missing required
positional" because the path field has index 1+ in the struct.

**Right shape:** add `positional: bool` to `ParamSchema` (default
false, serde-skipped). Have `params_from_clap` set it from
`arg.get_index().is_some()`. Stop hiding positional sinks — declare
them with descriptive names + doc comments. Refactor
`validate_against_schema` to split params into positionals and flags;
match positionals by their order among positionals only.

Risk: some current hidden sinks are true sinks (e.g., `rest` in find,
glob, rg) that swallow trailing args without modeling them. Those
should keep the sink semantics but expose a positional param in the
schema (`paths`, `pattern`, etc.) with a short description.

### `apply_output_format` early-returns on empty stdout
`kaish-types/src/output.rs:521-524`:
```
if !result.has_output() && result.text_out().is_empty() {
    return result;
}
```
The doc above the function claims it "serializes regardless of exit
code." It doesn't — when the result is a failure with empty stdout and
populated `err`, the early-return preserves stderr as text and skips
JSON encoding entirely. For `grep --json --bogus-flag` and friends,
the error message goes out as plain text even though `--json` was
requested.

Two ways forward: (1) when format=Json and stdout is empty but
exit≠0, emit a JSON error object `{"error": <err>, "code": <code>}`
to stdout; or (2) document the contract honestly as "--json applies
to successful stdout only." Option (1) is the agent-friendly choice
and matches the existing comment intent. Backward-compat audit
needed for any test asserting err-via-stderr with --json present.

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

### Builtin `sleep` does not honor cancellation
`tools/builtin/sleep.rs` is `tokio::time::sleep(d).await` and does not
check `ctx.cancel`. A `timeout 1 sleep 60` works for *external* `sleep`
because we kill the OS process, but for the builtin it sleeps the full
60s. Make builtins that block on time/IO race their work against
`ctx.cancel.cancelled()`. Same applies to other long-blocking builtins
that hold a future without yielding through cancellation.

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

### `MemoryFs` lock-splitting TOCTOU in `ensure_parents`
`vfs/memory.rs:179` acquires a write lock, drops it, then the actual
mutation acquires a new write lock. Concurrent task can invalidate the
parent setup between the two. Affects `rename`, `write`, `mkdir`,
`symlink`. Fix: hold one lock across both operations, or inline
`ensure_parents`.

### `scheduler::job::tests::test_cleanup_removes_temp_files` flake
Test reaches into the shared real-FS `/tmp/kaish/jobs/` path and races
parallel test runs. Passes in isolation, fails intermittently under
`cargo test --all`. Same root cause as the "job output files in
`/tmp/kaish/jobs/` persist indefinitely" entry above. Switch the
JobManager test path to `tempfile::tempdir()` (each test gets its own
root) until the GC fix lands.

---

## P4 — Eventually

### `touch .hidden.txt` fails — dot-prefixed filenames mis-tokenized
The lexer splits `.hidden.txt` into `Dot` + `Ident("hidden.txt")`
rather than a single filename token. `DotSlashPath` (`./foo`) works;
bare dot-prefixed names don't. Workaround: quote the name.

### Flag injection via glob expansion in `rm`
`rm *` in a directory containing `-rf.txt` expands to `rm -rf.txt …`
and kaish's structured flag parsing then flips `rm` behaviour.
Mitigations already in place: `set -o latch`, `set -o trash`, `rm --
*`. Future fix: the kernel knows which args are `GlobPattern`-sourced
— `rm` could reject flag-shaped positional args from that path.

### `Value::Float` NaN / Infinity silently becomes null
`kaish-types/src/result.rs` uses `serde_json::Number::from_f64(*f).
unwrap_or(Null)`. Roundtrip loses data. Either document as intentional
or serialise non-finite floats to string form.

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

---

## Resolved since previous lists

Captured here so context from `cleanups-todo.md` / old `issues.md`
isn't lost when those files are deleted.

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
