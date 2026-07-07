//! Scatter/Gather — Parallel pipeline execution.
//!
//! Scatter splits input into items and runs the pipeline in parallel.
//! Gather collects the parallel results.
//!
//! # Example
//!
//! ```text
//! cat urls.txt | scatter | fetch url=${ITEM} | gather
//! ```
//!
//! This reads URLs, then for each URL runs `fetch` in parallel,
//! then collects all results.

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

use tokio::sync::Semaphore;
use tracing::Instrument;

use crate::ast::{Command, Redirect, Value};
use crate::dispatch::CommandDispatcher;
use crate::duration::parse_duration;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ToolRegistry};

use super::pipeline::{apply_redirects, PipelineRunner};

/// Options for scatter operation.
#[derive(Debug, Clone)]
pub struct ScatterOptions {
    /// Variable name to bind each item to (default: "ITEM").
    pub var_name: String,
    /// Maximum parallelism (default: 8).
    pub limit: usize,
    /// Per-worker timeout. When `Some`, each worker is cancelled after this
    /// duration; the worker's external children get SIGTERM/SIGKILL and the
    /// `ScatterResult.timed_out` flag is set.
    pub timeout: Option<Duration>,
}

/// Options for gather operation.
#[derive(Debug, Clone, Default)]
pub struct GatherOptions {
    /// `--lines`: emit each successful worker's raw `out` in item order instead
    /// of JSONL rows — and HARD-ERROR (exit 123, no partial text) if any worker
    /// failed, since bare lines cannot represent a failure. The text escape
    /// hatch that keeps the old line mode's safety property.
    pub lines: bool,
    /// `--json` (the kernel-wide flag; scatter/gather own their output, so it
    /// reaches the tool): render the result records as ONE JSON array instead
    /// of JSONL rows. Same records, same `.data`.
    pub json: bool,
}

impl Default for ScatterOptions {
    fn default() -> Self {
        Self {
            var_name: "ITEM".to_string(),
            limit: 8,
            timeout: None,
        }
    }
}

/// One typed scatter item: the JSON element that fans out to a worker.
///
/// `json` is the source of truth — the worker binding derives from it via
/// [`json_to_value_no_envelope`](crate::interpreter::json_to_value_no_envelope)
/// (the exact conversion `for v in $(cmd)` uses), and the gather row's `item`
/// field carries it typed. `label` is a char-safe truncated display form for
/// spans and error messages.
#[derive(Debug, Clone)]
pub struct ScatterItem {
    /// The element as JSON (string items are JSON strings).
    pub json: serde_json::Value,
    /// Compact display label for tracing and error text.
    pub label: String,
}

impl ScatterItem {
    fn new(json: serde_json::Value) -> Self {
        let full = match &json {
            serde_json::Value::String(s) => s.clone(),
            other => other.to_string(),
        };
        // Char-safe truncation (a byte slice at 64 can split a UTF-8 char).
        let label = if full.chars().count() > 64 {
            let head: String = full.chars().take(64).collect();
            format!("{head}...")
        } else {
            full
        };
        Self { json, label }
    }

    fn from_text_line(line: &str) -> Self {
        Self::new(serde_json::Value::String(line.to_string()))
    }
}

/// Result from a single scatter worker.
#[derive(Debug, Clone)]
pub struct ScatterResult {
    /// The input item that was processed.
    pub item: ScatterItem,
    /// The execution result.
    pub result: ExecResult,
    /// Whether the worker was cancelled by the per-worker `--timeout`.
    pub timed_out: bool,
}

/// Runs scatter/gather pipelines.
///
/// Uses a single dispatcher for sequential stages (pre_scatter, post_gather),
/// and forks it per parallel worker via [`CommandDispatcher::fork`]. Each
/// worker gets its own subkernel with snapshotted session state so they can
/// run concurrently without racing on scope/cwd/aliases.
pub struct ScatterGatherRunner {
    tools: Arc<ToolRegistry>,
    /// Full dispatch chain for sequential stages (pre_scatter, post_gather).
    /// Parallel workers fork from this dispatcher.
    sequential_dispatcher: Arc<dyn CommandDispatcher>,
}

impl ScatterGatherRunner {
    /// Create a new scatter/gather runner.
    ///
    /// `dispatcher` drives sequential stages directly and serves as the fork
    /// source for parallel workers.
    pub fn new(
        tools: Arc<ToolRegistry>,
        dispatcher: Arc<dyn CommandDispatcher>,
    ) -> Self {
        Self { tools, sequential_dispatcher: dispatcher }
    }

    /// Execute a scatter/gather pipeline.
    ///
    /// The pipeline is split into three parts:
    /// - pre_scatter: commands before scatter
    /// - parallel: commands between scatter and gather
    /// - post_gather: commands after gather
    ///
    /// Returns the final result after all stages complete.
    #[tracing::instrument(level = "info", skip(self, pre_scatter, scatter_opts, parallel, gather_opts, post_gather, ctx), fields(item_count = tracing::field::Empty, parallelism = scatter_opts.limit))]
    #[allow(clippy::too_many_arguments)]
    pub async fn run(
        &self,
        pre_scatter: &[Command],
        scatter_opts: ScatterOptions,
        parallel: &[Command],
        gather_opts: GatherOptions,
        gather_redirects: &[Redirect],
        post_gather: &[Command],
        ctx: &mut ExecContext,
    ) -> ExecResult {
        let runner = PipelineRunner::new(self.tools.clone());

        // Run pre-scatter commands to get input.
        // Uses run_sequential to avoid async recursion (scatter → run → scatter).
        let (text, data) = if pre_scatter.is_empty() {
            // Use existing stdin — structured data, a String buffer, or a lazy
            // `pipe_stdin` (a frontend-seeded process-stdin pipe). `take_stdin`
            // alone would miss the pipe; `read_stdin_to_text` prefers it.
            let data = ctx.take_stdin_data();
            let text = match ctx.read_stdin_to_text().await {
                Ok(s) => s.unwrap_or_default(),
                Err(e) => return ExecResult::failure(2, format!("scatter: {e}")),
            };
            (text, data)
        } else {
            let result = runner.run_sequential(pre_scatter, ctx, &*self.sequential_dispatcher).await;
            if !result.ok() {
                return result;
            }
            (result.text_out().into_owned(), result.data)
        };

        // Extract items from structured data or text
        let items = match extract_items(data.as_ref(), &text) {
            Ok(items) => items,
            Err(msg) => return ExecResult::failure(1, msg),
        };
        if items.is_empty() {
            return ExecResult::success("");
        }

        tracing::Span::current().record("item_count", items.len());

        // Run parallel stage
        let results = self
            .run_parallel(&items, &scatter_opts, parallel, ctx)
            .await;

        // Gather per the GH #73 contract: JSONL result records by default (one
        // row per worker, failures included), or `--lines` raw text. Exit codes
        // are A′: 0 all ok · 123 any worker failed · 2 usage (clap layer).
        let gathered = gather_results(&results, &gather_opts);

        // `gather`'s own trailing redirect (`… | gather > results.jsonl | …`)
        // must apply to gather's OWN result before anything downstream sees
        // it — matching shell semantics where a file redirect on a pipeline
        // stage wins over the pipe (`cmd > file | next` sends cmd's real
        // stdout to the file; `next` reads nothing from cmd). Applying it
        // unconditionally (regardless of exit code) matches a redirect being
        // a file-descriptor operation independent of the command's success —
        // `false > file` still creates the file. Previously this only ran
        // when gather was the pipeline's last command, so a trailing
        // `gather > file | jq` silently skipped the file and let the
        // unredirected rows flow to `jq` instead.
        let gathered = apply_redirects(gathered, gather_redirects, ctx, &*self.sequential_dispatcher).await;

        // Run post-gather commands if any. A failed gather short-circuits —
        // feeding partial/failed output onward would propagate corruption.
        if post_gather.is_empty() || gathered.code != 0 {
            gathered
        } else {
            ctx.set_stdin_with_data(
                gathered.text_out().into_owned(),
                gathered.data.clone(),
            );
            runner.run_sequential(post_gather, ctx, &*self.sequential_dispatcher).await
        }
    }

    /// Run the parallel stage for all items.
    ///
    /// Each worker gets its own forked dispatcher via
    /// [`CommandDispatcher::fork`]. The fork snapshots per-session state
    /// (scope, cwd, aliases, user tools) so workers can run concurrently
    /// without racing. Forks are cheap (Scope is COW, plus a few Arc bumps),
    /// and they unlock the full dispatch chain inside workers — user tools,
    /// `.kai` scripts, and `$(...)` in args all work.
    #[tracing::instrument(level = "debug", skip(self, items, opts, commands, base_ctx), fields(worker_count = items.len()))]
    async fn run_parallel(
        &self,
        items: &[ScatterItem],
        opts: &ScatterOptions,
        commands: &[Command],
        base_ctx: &ExecContext,
    ) -> Vec<ScatterResult> {
        let semaphore = Arc::new(Semaphore::new(opts.limit));
        let tools = self.tools.clone();
        let var_name = opts.var_name.clone();

        // Spawn parallel tasks
        let mut handles = Vec::with_capacity(items.len());

        for item in items.iter().cloned() {
            let permit = semaphore.clone().acquire_owned().await;
            let tools = tools.clone();
            // Fork attached: the worker's cancel token is a child of the
            // parent kernel's, so a parent cancel (request timeout, embedder
            // Kernel::cancel) cascades into the worker and kills its
            // external children via the wait_or_kill discipline.
            let worker_dispatcher = self.sequential_dispatcher.fork_attached().await;
            let commands = commands.to_vec();
            let parent_token = base_ctx.cancel.clone();
            let worker_token = parent_token.child_token();

            // Build the worker context FROM THE PARENT, not from scratch. A
            // from-scratch `ExecContext::with_backend_and_scope` starts
            // `watchdog = None`; `dispatch_command` then syncs that `None` INTO
            // the subkernel (kernel.rs `ec.watchdog = ctx.watchdog.clone()`),
            // clobbering the fork's inherited watchdog — so inside a worker the
            // script clock is gone and any `ctx.patient` hold suspends a
            // *missing* timer, yielding false-positive request timeouts that
            // kill the worker. `child_for_pipeline` clones exactly what a worker
            // needs in one shot — watchdog, vfs_budget, aliases, ignore_config,
            // output_limit, allow_external_commands, backend, cwd, scope,
            // dispatcher — replacing the manual field-copy that was easy to let
            // drift (and that dropped the watchdog). `base_ctx` is a borrow (not
            // `'static`), so the child MUST be built here and MOVED into the
            // spawn — it cannot be constructed inside the closure.
            let mut worker_ctx = base_ctx.child_for_pipeline();
            // Per-worker TYPED binding — the same json→Value conversion the
            // for-loop uses for `$(cmd)` items (GH #73), so a record element
            // subscripts as `${ITEM[k]}`.
            worker_ctx.scope.set(
                &var_name,
                crate::interpreter::json_to_value_no_envelope(item.json.clone()),
            );
            // Per-worker cancel token (a child of the parent's), so the timeout
            // timer and a parent cancel both reach this worker's externals.
            worker_ctx.cancel = worker_token.clone();

            // Per-worker timeout: spawn a delay task that cancels the worker's
            // child token after `opts.timeout`. The cancel cascades into the
            // worker's externals via the fork's cancel link. `timed_out_flag`
            // distinguishes timeout from explicit parent cancellation when
            // tagging ScatterResult.
            let timed_out_flag = Arc::new(AtomicBool::new(false));
            let timer_handle: Option<tokio::task::JoinHandle<()>> = opts.timeout.map(|d| {
                let cancel = worker_token.clone();
                let flag = timed_out_flag.clone();
                tokio::spawn(async move {
                    tokio::time::sleep(d).await;
                    flag.store(true, Ordering::SeqCst);
                    cancel.cancel();
                })
            });
            let timed_out_check = timed_out_flag.clone();

            let worker_span = tracing::debug_span!("scatter_worker", item = %item.label);
            // Propagate the embedder's trace context across the spawn boundary so
            // each worker's spans stay in the same trace. `.instrument` below
            // provides the tracing parent; this provides the OTel parent.
            let handle = tokio::spawn(crate::telemetry::bind_current_context(async move {
                let _permit = permit; // Hold permit until done
                let mut worker_ctx = worker_ctx; // moved in; built from parent above

                // Run through PipelineRunner + dispatcher (full resolution chain).
                // Uses run_sequential to avoid async recursion and infinite future size.
                let runner = PipelineRunner::new(tools);
                let mut result =
                    runner.run_sequential(&commands, &mut worker_ctx, &*worker_dispatcher).await;

                // Per-worker spill boundary. `run_sequential` never reaches the
                // kernel's top-level post-run spill check (kernel.rs:2704), so
                // without this each worker holds its FULL output in memory —
                // N concurrent workers × large output evades the sandbox
                // `output_limit` (10 workers × 1 GB = 10 GB resident before
                // anything spills). Cap here, where the N× multiplication lives;
                // `child_for_pipeline` shares the parent's `output_limit`, so
                // workers cap against the same budget.
                if worker_ctx.output_limit.is_enabled() {
                    let _ = crate::output_limit::spill_if_needed(
                        &mut result,
                        &worker_ctx.output_limit,
                    )
                    .await;
                }

                // Worker finished — abort the timer if still pending so it
                // doesn't fire a now-pointless cancel and idle resources.
                if let Some(h) = timer_handle {
                    h.abort();
                }

                // Completion wins ties (GH #132): a worker whose command
                // finishes right as its timeout timer fires can read the
                // flag AFTER the delay task sets it, even though its own
                // result is a genuine, clean success. This isn't just a
                // stale-read gap — `sleep`'s (and any similarly-built
                // builtin's) own `tokio::select! { operation, cancelled() }`
                // is unbiased: if cancellation has *already* been signaled by
                // the time the operation's own timer also matures, tokio can
                // still pick the operation's branch, so `result.ok()` can be
                // `true` even after the flag was set and `cancel.cancel()`
                // was called. The result's own success is the ground truth
                // the flag can't override — a worker that truly finished
                // successfully must never be reported as timed out, no
                // matter what the racing flag says.
                let timed_out = timed_out_check.load(Ordering::SeqCst) && !result.ok();

                ScatterResult { item, result, timed_out }
            }.instrument(worker_span)));

            handles.push(handle);
        }

        // Collect results
        let mut results = Vec::with_capacity(handles.len());
        for handle in handles {
            match handle.await {
                Ok(result) => results.push(result),
                Err(e) => {
                    results.push(ScatterResult {
                        item: ScatterItem::new(serde_json::Value::String(
                            "<worker panicked>".to_string(),
                        )),
                        result: ExecResult::failure(1, format!("Task panicked: {}", e)),
                        timed_out: false,
                    });
                }
            }
        }

        results
    }
}

/// Extract typed items from structured data or text (GH #73 contract).
///
/// Structured `.data` wins and fans out TYPED: a JSON array yields one item per
/// element with the element's real type (a record element subscripts as
/// `${ITEM[k]}` in the worker; number `1` and string `"1"` stay distinct). A
/// `null` element is a loud error — a worker silently running with a null
/// binding is corruption. A single non-array OBJECT is a loud error with a
/// select-the-array hint (one worker running on the `{"jobs":[…]}` envelope is
/// never what was meant); a single scalar is one item. Binary data is a loud
/// error.
///
/// Plain-text stdin is split on newlines only — one item per line, each a
/// string — matching the for-loop `$(cmd)` contract: trailing newlines trimmed
/// once, each line's trailing `\r` stripped, whitespace within a line never
/// split. Blank lines are SKIPPED (panel-ratified: a worker spawned on `""` is
/// silent corruption of the most common input shape). Empty input yields zero
/// items (the caller exits 0 with no rows).
pub fn extract_items(data: Option<&Value>, text: &str) -> Result<Vec<ScatterItem>, String> {
    // 1. Structured data wins over text (arch_data_iteration contract).
    match data {
        // JSON array — fan out per element, typed (seq/split/glob/find/jq).
        Some(Value::Json(serde_json::Value::Array(arr))) => {
            let mut items = Vec::with_capacity(arr.len());
            for (i, elem) in arr.iter().enumerate() {
                if elem.is_null() {
                    return Err(format!(
                        "scatter: item {i} is null — refusing to bind a worker to null \
                         (filter it out first, e.g. jq 'map(select(. != null))')"
                    ));
                }
                items.push(ScatterItem::new(elem.clone()));
            }
            return Ok(items);
        }
        // Kaish scalars — one typed item each.
        Some(Value::String(s)) => {
            return Ok(vec![ScatterItem::new(serde_json::Value::String(s.clone()))])
        }
        Some(Value::Int(i)) => return Ok(vec![ScatterItem::new(serde_json::json!(i))]),
        Some(Value::Float(f)) => return Ok(vec![ScatterItem::new(serde_json::json!(f))]),
        Some(Value::Bool(b)) => return Ok(vec![ScatterItem::new(serde_json::json!(b))]),
        Some(Value::Null) => {
            return Err("scatter: input is null — nothing to fan out".to_string())
        }
        // A single JSON object is almost always the unselected envelope around
        // the array the caller meant — loud, with the fix in the message.
        Some(Value::Json(serde_json::Value::Object(map))) => {
            let hint = map
                .iter()
                .find(|(_, v)| v.is_array())
                .map(|(k, _)| format!(" (did you mean jq '.{k}'?)"))
                .unwrap_or_default();
            return Err(format!(
                "scatter: input is a single object, not an array — select the array to \
                 fan out over{hint}"
            ));
        }
        Some(Value::Json(serde_json::Value::Null)) => {
            return Err("scatter: input is null — nothing to fan out".to_string())
        }
        // Single JSON scalar — one typed item.
        Some(Value::Json(json)) => return Ok(vec![ScatterItem::new(json.clone())]),
        // Binary can't bind a worker variable meaningfully — loud, never a
        // placeholder string item.
        Some(Value::Bytes(b)) => {
            return Err(format!(
                "scatter: input is binary ({} bytes) — decode it to text or JSON first",
                b.len()
            ))
        }
        // No structured data — fall through to plain-text newline-split.
        None => {}
    }

    // 2. Plain text — newline-split, mirroring kernel.rs for-loop $(cmd)
    // semantics; every text item is a string.
    let trimmed = text.trim_end_matches(['\n', '\r']);
    if trimmed.is_empty() {
        return Ok(vec![]);
    }
    Ok(trimmed
        .split('\n')
        .map(|line| line.trim_end_matches('\r'))
        .filter(|line| !line.is_empty())
        .map(ScatterItem::from_text_line)
        .collect())
}

/// Strip exactly one trailing newline (`\n` or `\r\n`), leaving everything
/// else raw — the GH #73 contract for the row's `out` and `err` fields.
fn strip_one_trailing_newline(s: &str) -> &str {
    let s = s.strip_suffix('\n').unwrap_or(s);
    s.strip_suffix('\r').unwrap_or(s)
}

/// Build one JSONL result record for a worker (GH #73 row schema).
///
/// `{"i":N, "item":<typed>, "ok":bool, "code":N, "out":"…", "err":"…"}` plus
/// `data` (the worker's structured output, typed) when present and
/// `timed_out:true` when it was. `i`/`item`/`ok`/`code`/`out`/`err` are always
/// present (`err` deliberately so — omit-empty on the most-read field would
/// make `${r[err]}` a loud missing-key error on every successful row). A
/// timed-out worker reports `code` 124 (the `timeout(1)` prior) and `ok` false.
///
/// # Binary-output hazard
///
/// A worker's `out` is a `text_out()`-shaped string field, but the worker's
/// `ExecResult` payload can be `OutputPayload::Bytes` — several builtins
/// already produce it (`cat`/`head`/`tail`/`base64 -d`/`xxd -r`/`dd`/`tee`/
/// external commands via `env`/`spawn`), so a worker running e.g. `cat
/// binary.file` is not a hypothetical, it is reachable today. `text_out()`
/// would lossily replace invalid UTF-8 with U+FFFD — silent data corruption
/// riding through the row as if it were the worker's real text output. Per
/// "crash beats corrupt" we go loud at row granularity instead: `try_text_out`
/// catches it, the row is forced `ok:false` with a clear `err` (never a
/// lossily-decoded `out`), and the OTHER rows are unaffected — see
/// `docs/binary-data.md` for the broader binary-data plan.
fn result_row(i: usize, r: &ScatterResult) -> serde_json::Value {
    let mut ok = r.result.ok() && !r.timed_out;
    let mut code = if r.timed_out { 124 } else { r.result.code };

    let (out_text, err_text) = match r.result.try_text_out() {
        Ok(text) => (
            strip_one_trailing_newline(&text).to_string(),
            strip_one_trailing_newline(&r.result.err).to_string(),
        ),
        Err(e) => {
            ok = false;
            if code == 0 {
                code = 1;
            }
            (
                String::new(),
                format!(
                    "binary worker output not representable as text ({} bytes) — \
                     encode it in the worker (base64/xxd)",
                    e.len
                ),
            )
        }
    };

    let mut row = serde_json::Map::new();
    row.insert("i".into(), serde_json::json!(i));
    row.insert("item".into(), r.item.json.clone());
    row.insert("ok".into(), serde_json::json!(ok));
    row.insert("code".into(), serde_json::json!(code));
    row.insert("out".into(), serde_json::json!(out_text));
    row.insert("err".into(), serde_json::json!(err_text));
    if let Some(data) = &r.result.data {
        row.insert("data".into(), kaish_types::value_to_json(data));
    }
    // A latched worker (exit 2 under `set -o latch`) is otherwise
    // indistinguishable from a plain failure in the row — carry the nonce so a
    // caller can act on the gate straight from the row (GH #124 part 3).
    // Infallible: LatchRequest is String/Vec<String>/u64 fields only.
    if let Some(latch) = &r.result.latch
        && let Ok(v) = serde_json::to_value(latch)
    {
        row.insert("latch".into(), v);
    }
    if r.timed_out {
        row.insert("timed_out".into(), serde_json::json!(true));
    }
    serde_json::Value::Object(row)
}

/// Render gathered worker results as an [`ExecResult`] (GH #73 contract).
///
/// Default: JSONL — one compact result record per worker, in item order, EVERY
/// worker including failures. One source, three views: the pipe text is the
/// JSONL, `.data` is the typed record array (so `for r in $(… | gather)`
/// iterates records and post-gather stages see typed stdin), and the kernel
/// `--json` flag renders the same array as one JSON document via `rich_json`.
///
/// `--lines`: each successful worker's raw `out` in item order — and a HARD
/// error (no partial text) if any worker failed, because bare lines cannot
/// represent a failure (the old line mode's safety property, kept as a flag).
///
/// Exit codes (A′): `0` all workers ok · `123` any worker failed, partial or
/// total (timeouts count) — partial-vs-total is distinguished in the rows.
fn gather_results(results: &[ScatterResult], opts: &GatherOptions) -> ExecResult {
    // A worker's binary stdout that can't decode as text is a failure for
    // gather's purposes too — see `result_row`'s hazard doc. Folding it into
    // `failed` here keeps the overall exit code (0 vs 123) honest: a `--lines`
    // or JSONL caller checking `$?` must see non-zero, not a silent "0 all
    // ok" while one row was actually corruption-guarded away.
    let is_unrepresentable = |r: &ScatterResult| r.result.try_text_out().is_err();

    let failed: Vec<&ScatterResult> = results
        .iter()
        .filter(|r| !r.result.ok() || r.timed_out || is_unrepresentable(r))
        .collect();
    let code = if failed.is_empty() { 0 } else { 123 };
    let err = if failed.is_empty() {
        String::new()
    } else {
        let names = failed
            .iter()
            .map(|r| {
                if is_unrepresentable(r) {
                    format!("{} (binary output not representable as text)", r.item.label)
                } else {
                    r.item.label.clone()
                }
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!("gather: {} of {} worker(s) failed: {names}", failed.len(), results.len())
    };

    if opts.lines {
        // Bare lines can't carry a failure — refuse with no partial text
        // rather than silently dropping rows. This also covers binary output
        // (folded into `failed` above): `--lines` is a text-only escape
        // hatch, and a U+FFFD-laden line would be exactly the silent
        // corruption this hardening pass exists to prevent.
        if !failed.is_empty() {
            return ExecResult::failure(code, format!("{err} (drop --lines to get per-worker rows)"));
        }
        let text = results
            .iter()
            .map(|r| strip_one_trailing_newline(&r.result.text_out()).to_string())
            .collect::<Vec<_>>()
            .join("\n");
        return ExecResult::success(text);
    }

    let rows: Vec<serde_json::Value> =
        results.iter().enumerate().map(|(i, r)| result_row(i, r)).collect();
    let text = if opts.json {
        // `--json`: the same records as one JSON document.
        serde_json::to_string_pretty(&rows).unwrap_or_default()
    } else {
        // Default: JSONL — one compact record per line.
        rows.iter().map(|row| row.to_string()).collect::<Vec<_>>().join("\n")
    };
    let array = serde_json::Value::Array(rows);
    ExecResult::from_parts(code, text, err, Some(Value::Json(array)))
}

/// Human-readable repr of a `Value` for a "wrong type" error message —
/// deliberately not `Debug` (whose `String("five")` quoting/enum-tag noise
/// reads badly to a user who just typed `--limit five`).
fn describe_value(v: &Value) -> String {
    match v {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(n) => n.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => format!("{s:?}"),
        Value::Json(j) => j.to_string(),
        Value::Bytes(b) => format!("<{} bytes>", b.len()),
    }
}

/// Parse scatter options from tool args.
///
/// A flag key *present* in `args.named` with a value of the wrong type is a
/// loud `Err`, never a silent fall-back to the default — `scatter --limit
/// five` must not quietly run at the default limit, and `scatter --as 42`
/// must not quietly bind `$ITEM`. (An *absent* or unresolved flag falls back
/// to a bare boolean earlier, at the pipeline arg-binding layer — that lenient
/// path is unrelated and untouched here.)
pub fn parse_scatter_options(args: &crate::tools::ToolArgs) -> Result<ScatterOptions, String> {
    let mut opts = ScatterOptions::default();

    match args.named.get("as") {
        None => {}
        Some(Value::String(name)) => opts.var_name = name.clone(),
        Some(other) => {
            return Err(format!(
                "scatter --as: expected a variable name, got {}",
                describe_value(other)
            ))
        }
    }

    match args.named.get("limit") {
        None => {}
        Some(Value::Int(n)) => opts.limit = clamp_scatter_limit(*n),
        // Values from variables often stringify (`--limit "$n"`) — coerce a
        // numeric string the same as an int.
        Some(Value::String(s)) => match s.trim().parse::<i64>() {
            Ok(n) => opts.limit = clamp_scatter_limit(n),
            Err(_) => {
                return Err(format!(
                    "scatter --limit: expected a positive integer, got {}",
                    describe_value(&Value::String(s.clone()))
                ))
            }
        },
        Some(other) => {
            return Err(format!(
                "scatter --limit: expected a positive integer, got {}",
                describe_value(other)
            ))
        }
    }

    // --timeout DURATION: per-worker timeout. Accepts the same forms as the
    // `timeout` builtin (30, 5s, 500ms, 2m, 1h). A present-but-invalid value
    // is a loud Err — a typo here must not silently disable cancellation.
    match args.named.get("timeout") {
        None => {}
        Some(Value::String(s)) => match parse_duration(s) {
            Some(d) => opts.timeout = Some(d),
            None => {
                return Err(format!(
                    "scatter --timeout: invalid duration {} (try: 30, 5s, 500ms, 2m, 1h)",
                    describe_value(&Value::String(s.clone()))
                ))
            }
        },
        Some(Value::Int(n)) if *n >= 0 => opts.timeout = Some(Duration::from_secs(*n as u64)),
        Some(other) => {
            return Err(format!(
                "scatter --timeout: expected a non-negative duration, got {}",
                describe_value(other)
            ))
        }
    }

    Ok(opts)
}

/// Clamp a requested `--limit` to `[1, SCATTER_LIMIT_MAX]`, warning (not
/// erroring) when the ceiling clamps a value down — this ceiling exists to
/// protect the host, not to reject user input, so it stays a warn+clamp.
fn clamp_scatter_limit(requested: i64) -> usize {
    let clamped = requested.clamp(1, SCATTER_LIMIT_MAX as i64);
    if requested > SCATTER_LIMIT_MAX as i64 {
        tracing::warn!(
            target: "kaish::scatter",
            requested = requested,
            ceiling = SCATTER_LIMIT_MAX,
            "scatter limit clamped to ceiling"
        );
    }
    clamped as usize
}

/// Upper bound on the concurrency `scatter --limit N` accepts. Users who
/// ask for more get a `tracing::warn` and are clamped to this value —
/// silent clamping would violate the "no silent fallbacks" rule.
pub const SCATTER_LIMIT_MAX: usize = 10_000;

/// Parse gather options from tool args.
///
/// Returns `Err` for a present-but-wrong-typed flag value, mirroring
/// [`parse_scatter_options`]. Today gather's only value-carrying flags are
/// boolean (`--lines`/`--json`), so this can't yet fail — the `Result` return
/// keeps the signature symmetric with scatter's and ready for the next
/// value-carrying gather flag.
pub fn parse_gather_options(args: &crate::tools::ToolArgs) -> Result<GatherOptions, String> {
    let mut opts = GatherOptions::default();

    if args.has_flag("lines") {
        opts.lines = true;
    }

    if args.has_flag("json") {
        opts.json = true;
    }

    Ok(opts)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn labels(items: &[ScatterItem]) -> Vec<String> {
        items.iter().map(|i| i.label.clone()).collect()
    }

    fn item(s: &str) -> ScatterItem {
        ScatterItem::new(serde_json::Value::String(s.to_string()))
    }

    #[test]
    fn test_extract_items_structured_json_array() {
        let data = Value::Json(serde_json::json!(["a", "b", "c"]));
        let items = extract_items(Some(&data), "").unwrap();
        assert_eq!(labels(&items), vec!["a", "b", "c"]);
    }

    #[test]
    fn test_extract_items_structured_mixed_types_stay_typed() {
        // GH #73: number 1 and string "1" must remain distinct through the
        // fan-out — the old Vec<String> path conflated them silently.
        let data = Value::Json(serde_json::json!([1, "1", true, {"id": 7}]));
        let items = extract_items(Some(&data), "").unwrap();
        assert_eq!(items[0].json, serde_json::json!(1));
        assert_eq!(items[1].json, serde_json::json!("1"));
        assert_ne!(items[0].json, items[1].json, "1 and \"1\" must not conflate");
        assert_eq!(items[2].json, serde_json::json!(true));
        assert_eq!(items[3].json, serde_json::json!({"id": 7}));
    }

    #[test]
    fn test_extract_items_null_element_is_loud() {
        let data = Value::Json(serde_json::json!(["a", null, "c"]));
        let err = extract_items(Some(&data), "").unwrap_err();
        assert!(err.contains("null"), "should name the problem: {err}");
        assert!(err.contains("item 1"), "should name the position: {err}");
    }

    #[test]
    fn test_extract_items_single_object_is_loud_with_hint() {
        let data = Value::Json(serde_json::json!({"jobs": [1, 2]}));
        let err = extract_items(Some(&data), "").unwrap_err();
        assert!(err.contains("single object"), "{err}");
        assert!(err.contains("jq '.jobs'"), "should hint the array key: {err}");
    }

    #[test]
    fn test_extract_items_binary_is_loud() {
        let data = Value::Bytes(vec![0, 1, 2]);
        let err = extract_items(Some(&data), "").unwrap_err();
        assert!(err.contains("binary"), "{err}");
    }

    #[test]
    fn test_extract_items_structured_string() {
        let data = Value::String("single".into());
        let items = extract_items(Some(&data), "").unwrap();
        assert_eq!(labels(&items), vec!["single"]);
    }

    #[test]
    fn test_extract_items_single_line_text() {
        let items = extract_items(None, "hello").unwrap();
        assert_eq!(labels(&items), vec!["hello"]);
    }

    #[test]
    fn test_extract_items_empty() {
        let items = extract_items(None, "").unwrap();
        assert!(items.is_empty());
    }

    #[test]
    fn test_extract_items_multiline_fans_out_per_line() {
        let items = extract_items(None, "one\ntwo\nthree").unwrap();
        assert_eq!(labels(&items), vec!["one", "two", "three"]);
    }

    #[test]
    fn test_extract_items_trailing_newline_no_phantom_item() {
        let items = extract_items(None, "one\ntwo\n").unwrap();
        assert_eq!(labels(&items), vec!["one", "two"]);
    }

    #[test]
    fn test_extract_items_crlf_per_line() {
        let items = extract_items(None, "one\r\ntwo\r\n").unwrap();
        assert_eq!(labels(&items), vec!["one", "two"]);
    }

    #[test]
    fn test_extract_items_blank_lines_skipped() {
        // GH #73 panel finding: a worker spawned on "" is silent corruption of
        // the most common input shape — blank lines are skipped, not items.
        let items = extract_items(None, "a\n\nb").unwrap();
        assert_eq!(labels(&items), vec!["a", "b"]);
    }

    #[test]
    fn test_extract_items_whitespace_within_line_not_split() {
        let items = extract_items(None, "a b\nc d").unwrap();
        assert_eq!(labels(&items), vec!["a b", "c d"]);
    }

    #[test]
    fn test_extract_items_only_newlines_is_empty() {
        let items = extract_items(None, "\n\n").unwrap();
        assert!(items.is_empty());
    }

    #[test]
    fn test_extract_items_structured_overrides_text() {
        let data = Value::Json(serde_json::json!(["x", "y"]));
        let items = extract_items(Some(&data), "ignored\ntext").unwrap();
        assert_eq!(labels(&items), vec!["x", "y"]);
    }

    #[test]
    fn test_item_label_truncates_on_char_boundary() {
        // 100 multibyte chars — a byte-slice truncation would panic.
        let long: String = "é".repeat(100);
        let it = ScatterItem::new(serde_json::Value::String(long));
        assert!(it.label.ends_with("..."));
        assert_eq!(it.label.chars().count(), 67);
    }

    #[test]
    fn test_gather_results_jsonl_rows_carry_everything() {
        let results = vec![
            ScatterResult {
                item: item("a"),
                result: ExecResult::success("result_a\n"),
                timed_out: false,
            },
            ScatterResult {
                item: item("b"),
                result: ExecResult::failure(7, "boom\n"),
                timed_out: false,
            },
        ];
        let out = gather_results(&results, &GatherOptions::default());
        assert_eq!(out.code, 123, "any failure → 123 (A′)");
        let rows: Vec<serde_json::Value> = out
            .text_out()
            .lines()
            .map(|l| serde_json::from_str(l).unwrap())
            .collect();
        assert_eq!(rows.len(), 2, "every worker gets a row, failures included");
        assert_eq!(rows[0]["i"], 0);
        assert_eq!(rows[0]["item"], "a");
        assert_eq!(rows[0]["ok"], true);
        assert_eq!(rows[0]["out"], "result_a", "trailing newline stripped");
        assert_eq!(rows[0]["err"], "", "err always present");
        assert!(rows[0].get("timed_out").is_none(), "omit-false");
        assert!(rows[0].get("data").is_none(), "omit-empty");
        assert_eq!(rows[1]["i"], 1);
        assert_eq!(rows[1]["ok"], false);
        assert_eq!(rows[1]["code"], 7);
        assert_eq!(rows[1]["err"], "boom");
        // .data carries the typed array for iteration / post-gather.
        assert!(matches!(out.data, Some(Value::Json(serde_json::Value::Array(_)))));
    }

    #[test]
    fn test_gather_results_all_ok_is_zero() {
        let results = vec![ScatterResult {
            item: item("a"),
            result: ExecResult::success("x"),
            timed_out: false,
        }];
        let out = gather_results(&results, &GatherOptions::default());
        assert_eq!(out.code, 0);
        assert!(out.err.is_empty());
    }

    #[test]
    fn test_gather_results_timeout_row_is_124() {
        let results = vec![ScatterResult {
            item: item("slow"),
            result: ExecResult::failure(1, "cancelled"),
            timed_out: true,
        }];
        let out = gather_results(&results, &GatherOptions::default());
        assert_eq!(out.code, 123);
        let row: serde_json::Value = serde_json::from_str(out.text_out().lines().next().unwrap()).unwrap();
        assert_eq!(row["code"], 124, "timeout reports the timeout(1) code");
        assert_eq!(row["ok"], false);
        assert_eq!(row["timed_out"], true);
    }

    #[test]
    fn test_gather_results_typed_record_item_in_row() {
        let results = vec![ScatterResult {
            item: ScatterItem::new(serde_json::json!({"id": 3, "host": "web1"})),
            result: ExecResult::success("ok"),
            timed_out: false,
        }];
        let out = gather_results(&results, &GatherOptions::default());
        let row: serde_json::Value = serde_json::from_str(out.text_out().lines().next().unwrap()).unwrap();
        assert_eq!(row["item"]["id"], 3, "row item is the TYPED value, not a string");
    }

    #[test]
    fn test_gather_results_worker_data_rides_the_row() {
        let mut r = ExecResult::success("text");
        r.data = Some(Value::Json(serde_json::json!({"k": 1})));
        let results = vec![ScatterResult { item: item("a"), result: r, timed_out: false }];
        let out = gather_results(&results, &GatherOptions::default());
        let row: serde_json::Value = serde_json::from_str(out.text_out().lines().next().unwrap()).unwrap();
        assert_eq!(row["data"]["k"], 1, "worker .data lands typed in the row");
        assert_eq!(row["out"], "text", "out stays alongside data");
    }

    #[test]
    fn test_gather_results_worker_latch_rides_the_row() {
        // GH #124 part 3: a latched worker (exit 2 under `set -o latch`) is
        // otherwise indistinguishable from a plain failure in the row — the
        // nonce must ride along so a caller can act on the gate from the row.
        use kaish_types::result::LatchRequest;

        let mut r = ExecResult::failure(2, "rm: confirmation required (latch enabled)");
        r.latch = Some(Box::new(LatchRequest {
            nonce: "a3f7b2c1".to_string(),
            command: "rm".to_string(),
            paths: vec!["precious.txt".to_string()],
            hint: "rm --confirm=\"a3f7b2c1\" precious.txt".to_string(),
            tool: "rm".to_string(),
            argv: vec!["precious.txt".to_string()],
            ttl: 60,
            job_id: None,
        }));
        let results = vec![ScatterResult { item: item("a"), result: r, timed_out: false }];
        let out = gather_results(&results, &GatherOptions::default());
        assert_eq!(out.code, 123, "a latched worker still counts as failed for gather's exit code");
        let row: serde_json::Value = serde_json::from_str(out.text_out().lines().next().unwrap()).unwrap();
        assert_eq!(row["ok"], false);
        assert_eq!(row["code"], 2);
        assert_eq!(
            row["latch"]["nonce"], "a3f7b2c1",
            "the latch nonce must ride the row: {row}"
        );
        assert_eq!(row["latch"]["command"], "rm");
    }

    #[test]
    fn test_gather_results_lines_happy_path() {
        let results = vec![
            ScatterResult { item: item("a"), result: ExecResult::success("result_a\n"), timed_out: false },
            ScatterResult { item: item("b"), result: ExecResult::success("result_b"), timed_out: false },
        ];
        let out = gather_results(&results, &GatherOptions { lines: true, ..Default::default() });
        assert_eq!(out.code, 0);
        assert_eq!(&*out.text_out(), "result_a\nresult_b");
    }

    #[test]
    fn test_gather_results_lines_hard_errors_on_any_failure() {
        // Bare lines can't represent a failure — no partial text, loud 123.
        let results = vec![
            ScatterResult { item: item("a"), result: ExecResult::success("good"), timed_out: false },
            ScatterResult { item: item("b"), result: ExecResult::failure(1, "boom"), timed_out: false },
        ];
        let out = gather_results(&results, &GatherOptions { lines: true, ..Default::default() });
        assert_eq!(out.code, 123);
        assert!(out.text_out().is_empty(), "no partial text on --lines failure");
        assert!(out.err.contains("b"), "names the failed item: {}", out.err);
    }

    #[test]
    fn test_parse_scatter_options() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("as".to_string(), Value::String("URL".to_string()));
        args.named.insert("limit".to_string(), Value::Int(4));

        let opts = parse_scatter_options(&args).unwrap();
        assert_eq!(opts.var_name, "URL");
        assert_eq!(opts.limit, 4);
    }

    #[test]
    fn test_parse_gather_options() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.flags.insert("lines".to_string());

        let opts = parse_gather_options(&args).unwrap();
        assert!(opts.lines);
        assert!(!parse_gather_options(&ToolArgs::new()).unwrap().lines, "default is JSONL");
    }

    #[test]
    fn scatter_limit_clamps_to_ceiling() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("limit".to_string(), Value::Int(999_999));
        let opts = parse_scatter_options(&args).unwrap();
        assert_eq!(opts.limit, SCATTER_LIMIT_MAX);
    }

    #[test]
    fn scatter_limit_raises_zero_to_one() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("limit".to_string(), Value::Int(0));
        let opts = parse_scatter_options(&args).unwrap();
        assert_eq!(opts.limit, 1);
    }

    #[test]
    fn scatter_limit_raises_negative_to_one() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("limit".to_string(), Value::Int(-42));
        let opts = parse_scatter_options(&args).unwrap();
        assert_eq!(opts.limit, 1);
    }

    #[test]
    fn scatter_limit_preserves_valid_values() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("limit".to_string(), Value::Int(500));
        let opts = parse_scatter_options(&args).unwrap();
        assert_eq!(opts.limit, 500);
    }

    // ── FIX A: loud on present-but-wrong-typed flag values ──

    #[test]
    fn scatter_limit_wrong_type_is_loud_error() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("limit".to_string(), Value::String("five".to_string()));
        let err = parse_scatter_options(&args).unwrap_err();
        assert!(err.contains("--limit"), "{err}");
        assert!(err.contains("five"), "{err}");
    }

    #[test]
    fn scatter_limit_bool_is_loud_error() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("limit".to_string(), Value::Bool(true));
        let err = parse_scatter_options(&args).unwrap_err();
        assert!(err.contains("--limit"), "{err}");
    }

    #[test]
    fn scatter_limit_numeric_string_coerces() {
        // Values from variables often stringify: `scatter --limit "$n"`.
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("limit".to_string(), Value::String("5".to_string()));
        let opts = parse_scatter_options(&args).unwrap();
        assert_eq!(opts.limit, 5);
    }

    #[test]
    fn scatter_as_wrong_type_is_loud_error() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("as".to_string(), Value::Int(42));
        let err = parse_scatter_options(&args).unwrap_err();
        assert!(err.contains("--as"), "{err}");
        assert!(err.contains("42"), "{err}");
    }

    #[test]
    fn scatter_timeout_negative_int_is_loud_error() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("timeout".to_string(), Value::Int(-5));
        let err = parse_scatter_options(&args).unwrap_err();
        assert!(err.contains("--timeout"), "{err}");
    }

    #[test]
    fn scatter_timeout_unparseable_string_is_loud_error() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("timeout".to_string(), Value::String("banana".to_string()));
        let err = parse_scatter_options(&args).unwrap_err();
        assert!(err.contains("--timeout"), "{err}");
        assert!(err.contains("banana"), "{err}");
    }

    #[test]
    fn scatter_timeout_valid_duration_string_parses() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("timeout".to_string(), Value::String("5s".to_string()));
        let opts = parse_scatter_options(&args).unwrap();
        assert_eq!(opts.timeout, Some(Duration::from_secs(5)));
    }

    #[test]
    fn scatter_timeout_nonnegative_int_is_seconds() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("timeout".to_string(), Value::Int(30));
        let opts = parse_scatter_options(&args).unwrap();
        assert_eq!(opts.timeout, Some(Duration::from_secs(30)));
    }

    // ── FIX C: binary worker output must not silently corrupt to U+FFFD ──

    fn binary_result(invalid_utf8: Vec<u8>) -> ExecResult {
        ExecResult::success_bytes(invalid_utf8)
    }

    #[test]
    fn gather_row_goes_loud_not_lossy_on_binary_out() {
        // 0xFF is never valid UTF-8 on its own — text_out() would replace it
        // with U+FFFD; try_text_out() must catch it instead.
        let results = vec![ScatterResult {
            item: item("bin"),
            result: binary_result(vec![0xFF, 0xFE, 0x00, 0x01]),
            timed_out: false,
        }];
        let out = gather_results(&results, &GatherOptions::default());
        assert_eq!(out.code, 123, "a binary row flips the overall exit code too");
        let row: serde_json::Value =
            serde_json::from_str(out.text_out().lines().next().unwrap()).unwrap();
        assert_eq!(row["ok"], false, "binary output must not be silently ok:true");
        assert_ne!(row["code"], 0, "must carry a nonzero code");
        assert!(row["out"].as_str().unwrap().is_empty(), "no lossy text in out");
        let err_text = row["err"].as_str().unwrap();
        assert!(err_text.contains("binary"), "{err_text}");
        assert!(!err_text.contains('\u{FFFD}'), "must not carry U+FFFD: {err_text}");
    }

    #[test]
    fn gather_lines_hard_errors_on_binary_out() {
        // --lines is the raw-text escape hatch; binary must hard-error the
        // whole gather rather than emit a U+FFFD-laden line.
        let results = vec![
            ScatterResult { item: item("a"), result: ExecResult::success("good"), timed_out: false },
            ScatterResult {
                item: item("bin"),
                result: binary_result(vec![0xFF, 0xFE]),
                timed_out: false,
            },
        ];
        let out = gather_results(&results, &GatherOptions { lines: true, ..Default::default() });
        assert_eq!(out.code, 123);
        assert!(out.text_out().is_empty(), "no partial/lossy text on binary --lines failure");
        assert!(!out.err.contains('\u{FFFD}'), "must not carry U+FFFD: {}", out.err);
        assert!(out.err.contains("binary") || out.err.contains("bin"), "{}", out.err);
    }

    // ── FIX D: workers must inherit the parent's watchdog ──

    fn ctx_with_memory_fs() -> ExecContext {
        use crate::vfs::{MemoryFs, VfsRouter};
        use std::sync::Arc;
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[test]
    fn worker_ctx_inherits_parent_watchdog() {
        use crate::watchdog::Watchdog;
        use std::sync::Arc;

        let mut parent = ctx_with_memory_fs();
        parent.watchdog = Some(Arc::new(Watchdog::new(Duration::from_secs(30))));

        // The fix: workers are built via `child_for_pipeline`, which clones the
        // parent's watchdog. The old from-scratch `with_backend_and_scope`
        // path (below) dropped it — this test would fail against that path.
        let worker_ctx = parent.child_for_pipeline();
        assert!(
            worker_ctx.watchdog.is_some(),
            "worker must carry the parent's script watchdog, not None"
        );

        // Document the trap the fix closes: the old construction starts with a
        // None watchdog, which dispatch_command then syncs into the subkernel.
        let from_scratch =
            ExecContext::with_backend_and_scope(parent.backend.clone(), parent.scope.clone());
        assert!(
            from_scratch.watchdog.is_none(),
            "the abandoned from-scratch path is exactly why the worker lost its watchdog"
        );
    }

    // ── FIX E: workers cap output against the shared spill budget ──

    #[tokio::test]
    async fn worker_spills_over_the_shared_output_limit() {
        use crate::output_limit::{spill_if_needed, OutputLimitConfig};

        // Small in-memory limit (no disk writes in tests — CLAUDE.md).
        let mut cfg = OutputLimitConfig::agent().in_memory();
        cfg.set_limit(Some(64));

        let mut parent = ctx_with_memory_fs();
        parent.output_limit = cfg;

        // `child_for_pipeline` shares the parent's output_limit, so the worker
        // caps against the same budget — this is exactly what the worker path
        // now reads (`worker_ctx.output_limit`) before building its
        // ScatterResult.
        let worker_ctx = parent.child_for_pipeline();
        assert!(worker_ctx.output_limit.is_enabled(), "budget must reach the worker");

        // Mirror the worker sequence: a large result, then the per-worker spill.
        let mut result = ExecResult::success("x".repeat(4096));
        assert!(worker_ctx.output_limit.is_enabled());
        let _ = spill_if_needed(&mut result, &worker_ctx.output_limit).await;

        assert!(result.did_spill, "worker output over the limit must spill, not stay resident");
        assert!(
            result.text_out().len() < 4096,
            "spilled output must be truncated, not the full payload: {} bytes",
            result.text_out().len()
        );
    }

    // ── GH #132: a worker completing at the timeout boundary was
    // misclassified as timed out — reproduced and fixed ──
    //
    // Confirmed mechanism: the per-worker timer task does, in order,
    // `flag.store(true, SeqCst); cancel.cancel();` — both statements on the
    // SAME task, so by the time `cancel()` runs the flag is already true.
    // `sleep`'s own `tokio::select! { sleep(d) => success, cancelled() =>
    // failure(130) }` is unbiased: if BOTH branches are ready at the same
    // poll (the worker's own timer AND the just-cancelled token), tokio picks
    // between them pseudo-randomly. If it picks the sleep branch,
    // `run_sequential` returns a genuine success — but the flag was already
    // set moments earlier by the same timer task. Before the fix, the worker
    // trusted the flag unconditionally (`timed_out_check.load()`), tagging a
    // truly-successful result `timed_out: true` / code 124. The fix (see
    // `let timed_out = timed_out_check.load(...) && !result.ok();` above)
    // makes the result's own success authoritative: completion wins ties.
    //
    // Repro strategy: tie the worker's own `sleep <D>` EXACTLY to `scatter
    // --timeout <D>` so both timers mature at the identical virtual instant
    // under `start_paused`, then run many iterations. `start_paused` requires
    // the `current_thread` flavor (tokio rejects it combined with
    // `multi_thread`), so there's no genuine OS-thread-scheduling
    // non-determinism here — the variance across iterations comes entirely
    // from `tokio::select!`'s own pseudo-random tie-break (fastrand,
    // advancing per call) when `sleep`'s internal select has both branches
    // ready at once. Verified: this test fails ~45% of iterations against
    // the pre-fix code (a plain flag load) and passes 100% against the fix.
    #[tokio::test(flavor = "current_thread", start_paused = true)]
    async fn worker_completing_at_timeout_boundary_is_not_misclassified() {
        use crate::ast::{Arg, Expr};
        use crate::dispatch::BackendDispatcher;
        use crate::tools::register_builtins;
        use crate::vfs::{MemoryFs, VfsRouter};

        let mut registry = ToolRegistry::new();
        register_builtins(&mut registry);
        let tools = Arc::new(registry);
        let dispatcher: Arc<dyn CommandDispatcher> =
            Arc::new(BackendDispatcher::new(tools.clone()));
        let runner = ScatterGatherRunner::new(tools.clone(), dispatcher);

        // 20ms on both sides — the exact tie the race depends on.
        let commands = vec![Command {
            name: "sleep".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("0.02".to_string())))],
            redirects: vec![],
        }];
        let opts = ScatterOptions {
            timeout: Some(Duration::from_millis(20)),
            ..ScatterOptions::default()
        };

        let mut false_positives = 0;
        let mut genuine_timeouts = 0;
        let mut clean_success = 0;
        let iterations = 300;
        for _ in 0..iterations {
            // `BackendDispatcher::dispatch` routes through `ctx.backend.call_tool`,
            // not the registry directly — `with_vfs_and_tools` wires a
            // `LocalBackend` backed by OUR registry, so `sleep` actually
            // resolves instead of falling through to "command not found".
            let mut vfs = VfsRouter::new();
            vfs.mount("/", MemoryFs::new());
            let ctx = ExecContext::with_vfs_and_tools(Arc::new(vfs), tools.clone());
            let items = vec![item("x")];
            let results = runner.run_parallel(&items, &opts, &commands, &ctx).await;
            assert_eq!(results.len(), 1);
            let r = &results[0];
            match (r.timed_out, r.result.ok()) {
                (true, true) => false_positives += 1,
                (true, false) => genuine_timeouts += 1,
                (false, _) => clean_success += 1,
            }
        }

        eprintln!(
            "worker_completing_at_timeout_boundary: {false_positives} false-positive(s), \
             {genuine_timeouts} genuine timeout(s), {clean_success} clean success(es) out of \
             {iterations} iterations"
        );
        // Silence isn't success: if the tie stopped forming (e.g. a tokio
        // upgrade changes select!'s tie-break behavior), 0 false positives
        // would be meaningless rather than reassuring. Assert the race
        // actually fires both ways, so this test can't quietly stop testing
        // anything.
        assert!(
            genuine_timeouts > 0 && clean_success > 0,
            "the tie never formed (genuine_timeouts={genuine_timeouts}, \
             clean_success={clean_success}) — this test needs the race to actually occur to \
             mean anything; check the tied durations still create a real contest"
        );
        assert_eq!(
            false_positives, 0,
            "GH #132: a worker whose operation genuinely completed (result.ok()) must never \
             be reported timed_out — completion should win the tie"
        );
    }
}
