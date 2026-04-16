# Tech Debt & Future Work

From code review 2026-03-18 (6 parallel review agents + Gemini second opinion).
Items here were validated but deferred — too large or too tangential for the cleanup batch.

## Scheduler / Scatter-Gather

### Scatter workers can't run user functions or .kai scripts
`BackendDispatcher` is stateless (required for safety) but limited to builtins + external
commands. `tool my_func { ... }; scatter items my_func` silently fails with "command not found".
**Fix**: Per-worker kernel instances for full dispatch. Requires `Arc<Kernel>` refactor.

### No timeout on scatter/gather
A hung worker blocks all remaining results indefinitely. No `--timeout` parameter exists.

### No upper bound on scatter limit
`scatter items cmd limit=999999` spawns excessive tasks. Should clamp to a sane max (e.g., 10000).

### Silent filtering of failures in gather line format
`gather_results()` filters `.filter(|r| r.result.ok())` in line format, silently dropping
failed tasks. JSON format includes them. Users get no warning about dropped failures.

## Job System

### Job output files persist indefinitely
Temp files in `/tmp/kaish/jobs/` are only cleaned up via explicit `cleanup()` or `remove()`.
No automatic GC of stale files from crashed sessions or old jobs.

### Job spin_loop in spawn()
`JobManager::spawn()` uses `std::hint::spin_loop()` busy-wait to guarantee immediate
visibility. Works but wastes CPU under contention. Consider channel-based coordination.

## Builtins

### Glob expansion boilerplate (~10 duplications)
The pattern `if contains_glob { expand_glob... strip_prefix... } else { push }` is repeated
across `head.rs`, `tail.rs`, `cat.rs`, `wc.rs`, `grep.rs`, and others. Should be extracted
to an `ExecContext::expand_paths()` helper.

### `ls` too_many_arguments
`list_glob()` and `list_single()` suppress `clippy::too_many_arguments`. Refactor to an
`LsOptions` struct.

### Redundant `with_output_and_text` in tail/diff
`ExecResult::with_output_and_text(OutputData::text(x), x)` passes the same text twice.
Should just use `ExecResult::with_output(OutputData::text(x))`.

### `.data` population audit (from 2026-04-16 jq iteration fix)
`for v in $(cmd)` iteration depends on builtins populating `ExecResult.data` (or an
iterable `OutputData` shape) for unambiguous N-item producers. Status after the
jq / cut fixes:

- **`jq`, `cut`, `seq`, `find`, `glob`** — populate `.data = Value::Json(Array([…]))`
  directly. Verified by `tests/jq_tests.rs`.
- **`wc`, `ls` (long form), `grep` (default), `stat`** — emit `OutputData::table(…)`.
  The CommandSubst fallback iterates tables by first column, which is sometimes
  useful (`for path in $(ls -l)`) but often not what you want (`for _ in $(wc -l a b)`
  iterates filenames, not counts). Leaving as-is — table iteration is a separate
  ergonomics question from the "multi-line text footgun" fix.
- **`tac`, `sort`, `uniq`, `sed`, `tr`, `tee`, `head`, `tail`** — line-transforming
  filters. Not audited here; their output is often pipe-consumed rather than
  iterated. If a real use case surfaces, populate `.data` with the same pattern
  (`success_with_data(text, Value::Json(Array(lines)))`). Document at the time
  whether line content is guaranteed newline-clean — if the tool passes through
  user-controlled input verbatim, per-line iteration re-introduces the
  splitting-by-`\n` risk the kaish design calls out.

## VFS / Backend

### MemoryFs lock-splitting TOCTOU
`ensure_parents()` acquires its own write lock (drops it after), then the actual mutation
acquires a new write lock. Between these two lock releases, a concurrent task could
invalidate the parent setup. Affects `rename`, `write`, `mkdir`, `symlink`. Low risk
(cooperative async scheduling), but a design smell. Fix requires holding a single lock
across both operations or inlining ensure_parents.

### VfsRouter.read_only() always returns false
Even if all mounted filesystems are read-only, the router reports writable. Should
aggregate `read_only()` from all mounts.

## MCP Server

### Per-request OS thread in execute
Spawns a new thread + tokio LocalSet with 16MB stack per `execute()` call. Comment in
`execute.rs:154-157` acknowledges this. Fine at current scale, needs monitoring.

### Resource watcher channel fixed at 256
Bounded channel in `subscriptions.rs:33`. High-churn environments could drop file watch
events silently.

## Type System

### Value::Float NaN/Infinity silently becomes null
`value_to_json` maps NaN/Inf to `serde_json::Value::Null` via `from_f64()` returning None.
Roundtrip loses data. Either document as intentional or add special handling (e.g., string
representation for non-finite floats).

## Lexer / Parser

### Arithmetic preprocessing duplicates quote-handling logic
`preprocess_arithmetic()` (~150 lines) and heredoc preprocessing (~150 lines) both
implement quote/escape tracking. Extract `skip_quoted_content()` helper.

### `parse_interpolated_string` is ~200 lines
Handles `$VAR`, `${VAR}`, `$(...)` expansions with nested paren tracking. Could be split
into smaller helpers for maintainability.

### Token predicates incomplete
`Token::is_keyword()` doesn't include `Break`/`Continue`. `Token::starts_statement()`
doesn't include `While`. May not matter if these predicates aren't used for those cases,
but they're misleading.
