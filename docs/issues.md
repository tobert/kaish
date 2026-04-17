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

### `vfs::local::test_symlink_absolute_target_escape_blocked` fails
`crates/kaish-kernel/src/vfs/local.rs:520`. The test symlinks an
absolute path (`/etc/passwd`) inside the sandbox root and expects
`PermissionDenied`; it currently gets a different error or success.
Security-relevant — the sandbox may not be blocking absolute symlink
targets as the test assumes. Investigate whether the test is wrong or
the sandbox has a hole.

### `test_timeout_numeric_duration` fails
`crates/kaish-kernel/src/tools/builtin/timeout.rs:237`. Runs
`timeout 5 echo works` and expects a successful echo; currently asserts
fail (`result.ok()` is false). Distinct from the three `5s`/`100ms`
tests (those are lexer-limited and have been `#[ignore]`'d). This one
uses a plain numeric duration, so the lexer accepts it — something
else is wrong in the timeout or dispatch path. Investigate.

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

---

## P3 — Scheduler and infra

### Scatter / gather has no timeout
A hung worker stalls remaining results forever. No `--timeout`
parameter on `scatter` / `gather`. Add a per-worker timeout knob.

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
Agents will write `[ -z "$VAR" ]` (single bracket) or `for i in $LIST`
(no split — silently iterates once). A shellcheck-style actionable
warning without rejection would steer without frustrating. Optional
but high-leverage.

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
- **Heredoc span tracking** — 2026-04-16 (commits `c21e09c`
  source, `2490127` tests). Interpolated heredoc bodies now carry
  per-part byte offsets via `SpannedPart` and a new `Expr::HereDocBody`
  AST variant; validator attaches the span to any issue raised inside
  the body. `ParseError::format(source)` renders `line:col` with a
  source snippet. Literal heredocs and double-quoted strings keep the
  spanless paths for now — universal spanning is a separate refactor.
  The `skip_quoted_content()` extraction called out as a dependency
  was not needed for this fix; it remains a standalone P4 cleanup.
