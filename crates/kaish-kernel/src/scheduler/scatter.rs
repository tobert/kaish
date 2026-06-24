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

use crate::ast::{Command, Value};
use crate::dispatch::CommandDispatcher;
use crate::duration::parse_duration;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ToolRegistry};

use super::pipeline::PipelineRunner;

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
#[derive(Debug, Clone)]
pub struct GatherOptions {
    /// Show progress indicator.
    pub progress: bool,
    /// Take first N results and cancel rest (0 = all).
    pub first: usize,
    /// Output format: "json" or "lines".
    pub format: String,
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

impl Default for GatherOptions {
    fn default() -> Self {
        Self {
            progress: false,
            first: 0,
            format: "lines".to_string(),
        }
    }
}

/// Result from a single scatter worker.
#[derive(Debug, Clone)]
pub struct ScatterResult {
    /// The input item that was processed.
    pub item: String,
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
    pub async fn run(
        &self,
        pre_scatter: &[Command],
        scatter_opts: ScatterOptions,
        parallel: &[Command],
        gather_opts: GatherOptions,
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

        // Gather results
        let GatherOutput {
            text: gathered,
            dropped_failures,
        } = gather_results(&results, &gather_opts);

        // The line format can't carry a failed worker as a row. Rather than
        // silently omit it (data corruption — the caller sees fewer rows than
        // items scattered), fail loud: a non-zero exit plus an err naming the
        // failed items. Feeding the truncated set into post-gather would
        // propagate the corruption, so we short-circuit before running it.
        if !dropped_failures.is_empty() {
            let err = format!(
                "gather: {} task(s) failed and were omitted from line output: {} (use --json to capture per-task status)",
                dropped_failures.len(),
                dropped_failures.join(", ")
            );
            return ExecResult::from_output(1, gathered, err);
        }

        // Run post-gather commands if any
        if post_gather.is_empty() {
            ExecResult::success(gathered)
        } else {
            ctx.set_stdin(gathered);
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
        items: &[String],
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
            let var_name = var_name.clone();
            let base_scope = base_ctx.scope.clone();
            let backend = base_ctx.backend.clone();
            let cwd = base_ctx.cwd.clone();
            let parent_token = base_ctx.cancel.clone();
            let worker_token = parent_token.child_token();

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

            let item_label = if item.len() > 64 {
                format!("{}...", &item[..64])
            } else {
                item.clone()
            };
            let worker_span = tracing::debug_span!("scatter_worker", item = %item_label);
            // Propagate the embedder's trace context across the spawn boundary so
            // each worker's spans stay in the same trace. `.instrument` below
            // provides the tracing parent; this provides the OTel parent.
            let handle = tokio::spawn(crate::telemetry::bind_current_context(async move {
                let _permit = permit; // Hold permit until done

                // Create context for this worker
                let mut scope = base_scope;
                scope.set(&var_name, Value::String(item.clone()));

                let mut ctx = ExecContext::with_backend_and_scope(backend, scope);
                ctx.set_cwd(cwd);
                ctx.cancel = worker_token;

                // Run through PipelineRunner + dispatcher (full resolution chain).
                // Uses run_sequential to avoid async recursion and infinite future size.
                let runner = PipelineRunner::new(tools);
                let result = runner.run_sequential(&commands, &mut ctx, &*worker_dispatcher).await;

                // Worker finished — abort the timer if still pending so it
                // doesn't fire a now-pointless cancel and idle resources.
                if let Some(h) = timer_handle {
                    h.abort();
                }

                let timed_out = timed_out_check.load(Ordering::SeqCst);
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
                        item: String::new(),
                        result: ExecResult::failure(1, format!("Task panicked: {}", e)),
                        timed_out: false,
                    });
                }
            }
        }

        results
    }
}

/// Extract items from structured data or text.
///
/// Structured `.data` (a JSON array from split/seq/glob/find) wins and fans out
/// element-by-element. Plain-text stdin is split on newlines only — one item per
/// line — matching the for-loop `$(cmd)` contract (docs/LANGUAGE.md):
/// trailing newlines are trimmed once (no phantom tail item), each line's trailing
/// `\r` is stripped, interior blank lines are preserved, and whitespace within a
/// line is never split. Empty / newline-only input yields zero items.
pub fn extract_items(data: Option<&Value>, text: &str) -> Result<Vec<String>, String> {
    // 1. Structured data wins over text (arch_data_iteration contract).
    match data {
        // JSON array — fan out per element (seq/split/glob/find).
        Some(Value::Json(serde_json::Value::Array(arr))) => {
            return Ok(arr.iter().map(|v| match v {
                serde_json::Value::String(s) => s.clone(),
                other => other.to_string(),
            }).collect());
        }
        // Kaish String — one item.
        Some(Value::String(s)) => return Ok(vec![s.clone()]),
        // Kaish Int/Float/Bool/Null — one item serialized as its display form.
        Some(Value::Int(i)) => return Ok(vec![i.to_string()]),
        Some(Value::Float(f)) => return Ok(vec![f.to_string()]),
        Some(Value::Bool(b)) => return Ok(vec![b.to_string()]),
        Some(Value::Null) => return Ok(vec!["null".to_string()]),
        // Single JSON non-array value (object, number, string, bool, null) —
        // one item.  Use compact serialization; the caller gets the item as a
        // string and the pretty-printed text is NOT used here (that is the bug:
        // a multi-line pretty-print would be newline-split into N bogus items).
        Some(Value::Json(json)) => return Ok(vec![json.to_string()]),
        // Binary data in a scatter context — treat as one opaque item.
        Some(Value::Bytes(b)) => return Ok(vec![format!("[binary: {} bytes]", b.len())]),
        // No structured data — fall through to plain-text newline-split.
        None => {}
    }

    // 2. Plain text — newline-split, mirroring kernel.rs for-loop $(cmd) semantics.
    let trimmed = text.trim_end_matches(['\n', '\r']);
    if trimmed.is_empty() {
        return Ok(vec![]);
    }
    Ok(trimmed
        .split('\n')
        .map(|line| line.trim_end_matches('\r').to_string())
        .collect())
}

/// Rendered gather output plus the names of any failed tasks that the
/// line format could not represent as a row.
struct GatherOutput {
    text: String,
    /// Items whose worker failed and were omitted from `text`. Only the
    /// line format populates this — the JSON format carries every task as a
    /// row with an explicit `"ok"` field, so nothing is dropped there.
    dropped_failures: Vec<String>,
}

/// Gather results into output string.
///
/// The JSON format emits every task as a row (`"ok"` discriminates success
/// from failure). The line format can only carry stdout, so it returns the
/// successful rows in `text` and reports the failed items in
/// `dropped_failures` — the caller (`run`) turns that into a loud non-zero
/// exit rather than letting the failures vanish (see `docs/issues.md`).
fn gather_results(results: &[ScatterResult], opts: &GatherOptions) -> GatherOutput {
    let results_to_use = if opts.first > 0 && opts.first < results.len() {
        &results[..opts.first]
    } else {
        results
    };

    if opts.format == "json" {
        // Output as JSON array of objects
        let json_results: Vec<serde_json::Value> = results_to_use
            .iter()
            .map(|r| {
                serde_json::json!({
                    "item": r.item,
                    "ok": r.result.ok(),
                    "code": r.result.code,
                    "out": r.result.text_out().trim(),
                    "err": r.result.err.trim(),
                    "timed_out": r.timed_out,
                })
            })
            .collect();

        GatherOutput {
            text: serde_json::to_string_pretty(&json_results).unwrap_or_default(),
            dropped_failures: Vec::new(),
        }
    } else {
        // Output as lines (stdout from each successful worker, separated by
        // newlines). Failed workers can't be represented as a stdout row, so
        // we collect their items and let `run` fail loud instead of dropping
        // them silently.
        let text = results_to_use
            .iter()
            .filter(|r| r.result.ok())
            .map(|r| r.result.text_out())
            .map(|t| t.trim().to_string())
            .collect::<Vec<_>>()
            .join("\n");
        let dropped_failures = results_to_use
            .iter()
            .filter(|r| !r.result.ok())
            .map(|r| r.item.clone())
            .collect();
        GatherOutput {
            text,
            dropped_failures,
        }
    }
}

/// Parse scatter options from tool args.
pub fn parse_scatter_options(args: &crate::tools::ToolArgs) -> ScatterOptions {
    let mut opts = ScatterOptions::default();

    if let Some(Value::String(name)) = args.named.get("as") {
        opts.var_name = name.clone();
    }

    if let Some(Value::Int(n)) = args.named.get("limit") {
        let requested = *n;
        let clamped = requested.clamp(1, SCATTER_LIMIT_MAX as i64);
        if requested > SCATTER_LIMIT_MAX as i64 {
            tracing::warn!(
                target: "kaish::scatter",
                requested = requested,
                ceiling = SCATTER_LIMIT_MAX,
                "scatter limit clamped to ceiling"
            );
        }
        opts.limit = clamped as usize;
    }

    // --timeout DURATION: per-worker timeout. Accepts the same forms as the
    // `timeout` builtin (30, 5s, 500ms, 2m, 1h). Invalid input is ignored
    // with a warn so a typo doesn't silently disable cancellation.
    if let Some(Value::String(s)) = args.named.get("timeout") {
        match parse_duration(s) {
            Some(d) => opts.timeout = Some(d),
            None => tracing::warn!(
                target: "kaish::scatter",
                value = %s,
                "scatter --timeout: invalid duration (try: 30, 5s, 500ms, 2m, 1h)"
            ),
        }
    } else if let Some(Value::Int(n)) = args.named.get("timeout") {
        if *n >= 0 {
            opts.timeout = Some(Duration::from_secs(*n as u64));
        }
    }

    opts
}

/// Upper bound on the concurrency `scatter --limit N` accepts. Users who
/// ask for more get a `tracing::warn` and are clamped to this value —
/// silent clamping would violate the "no silent fallbacks" rule.
pub const SCATTER_LIMIT_MAX: usize = 10_000;

/// Parse gather options from tool args.
pub fn parse_gather_options(args: &crate::tools::ToolArgs) -> GatherOptions {
    let mut opts = GatherOptions::default();

    if args.has_flag("progress") {
        opts.progress = true;
    }

    if let Some(Value::Int(n)) = args.named.get("first") {
        opts.first = (*n).max(0) as usize;
    }

    if let Some(Value::String(fmt)) = args.named.get("format") {
        opts.format = fmt.clone();
    }

    opts
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_items_structured_json_array() {
        let data = Value::Json(serde_json::json!(["a", "b", "c"]));
        let items = extract_items(Some(&data), "").unwrap();
        assert_eq!(items, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_extract_items_structured_mixed_types() {
        let data = Value::Json(serde_json::json!([1, "two", true]));
        let items = extract_items(Some(&data), "").unwrap();
        assert_eq!(items, vec!["1", "two", "true"]);
    }

    #[test]
    fn test_extract_items_structured_string() {
        let data = Value::String("single".into());
        let items = extract_items(Some(&data), "").unwrap();
        assert_eq!(items, vec!["single"]);
    }

    #[test]
    fn test_extract_items_single_line_text() {
        let items = extract_items(None, "hello").unwrap();
        assert_eq!(items, vec!["hello"]);
    }

    #[test]
    fn test_extract_items_empty() {
        let items = extract_items(None, "").unwrap();
        assert!(items.is_empty());
    }

    #[test]
    fn test_extract_items_multiline_fans_out_per_line() {
        // Plain-text stdin splits on newlines, matching for-loop $(cmd)
        // semantics (docs/LANGUAGE.md) — one worker per line.
        let items = extract_items(None, "one\ntwo\nthree").unwrap();
        assert_eq!(items, vec!["one", "two", "three"]);
    }

    #[test]
    fn test_extract_items_trailing_newline_no_phantom_item() {
        // Trailing newline is trimmed once before splitting — no empty tail item.
        let items = extract_items(None, "one\ntwo\n").unwrap();
        assert_eq!(items, vec!["one", "two"]);
    }

    #[test]
    fn test_extract_items_crlf_per_line() {
        // Each line's trailing \r is stripped (CRLF input).
        let items = extract_items(None, "one\r\ntwo\r\n").unwrap();
        assert_eq!(items, vec!["one", "two"]);
    }

    #[test]
    fn test_extract_items_interior_blank_line_preserved() {
        // Interior empty lines are preserved (matches for-loop split('\n')).
        let items = extract_items(None, "a\n\nb").unwrap();
        assert_eq!(items, vec!["a", "", "b"]);
    }

    #[test]
    fn test_extract_items_whitespace_within_line_not_split() {
        // Only newlines split; spaces within a line stay in the item.
        let items = extract_items(None, "a b\nc d").unwrap();
        assert_eq!(items, vec!["a b", "c d"]);
    }

    #[test]
    fn test_extract_items_only_newlines_is_empty() {
        let items = extract_items(None, "\n\n").unwrap();
        assert!(items.is_empty());
    }

    #[test]
    fn test_extract_items_structured_overrides_text() {
        // Structured data takes priority over text
        let data = Value::Json(serde_json::json!(["x", "y"]));
        let items = extract_items(Some(&data), "ignored\ntext").unwrap();
        assert_eq!(items, vec!["x", "y"]);
    }

    #[test]
    fn test_gather_results_lines() {
        let results = vec![
            ScatterResult {
                item: "a".to_string(),
                result: ExecResult::success("result_a"),
                timed_out: false,
            },
            ScatterResult {
                item: "b".to_string(),
                result: ExecResult::success("result_b"),
                timed_out: false,
            },
        ];

        let opts = GatherOptions::default();
        let output = gather_results(&results, &opts);
        assert_eq!(output.text, "result_a\nresult_b");
        assert!(output.dropped_failures.is_empty());
    }

    #[test]
    fn test_gather_results_lines_reports_dropped_failures() {
        // A failed worker must not vanish from line output: it is reported in
        // `dropped_failures` so the caller can fail loud (docs/issues.md).
        let results = vec![
            ScatterResult {
                item: "a".to_string(),
                result: ExecResult::success("result_a"),
                timed_out: false,
            },
            ScatterResult {
                item: "b".to_string(),
                result: ExecResult::failure(1, "boom"),
                timed_out: false,
            },
        ];

        let opts = GatherOptions::default();
        let output = gather_results(&results, &opts);
        // Successful rows still render; the failure is reported, not dropped.
        assert_eq!(output.text, "result_a");
        assert_eq!(output.dropped_failures, vec!["b".to_string()]);
    }

    #[test]
    fn test_gather_results_json_keeps_failures_as_rows() {
        // JSON carries failures as rows (ok: false), so it drops nothing.
        let results = vec![ScatterResult {
            item: "b".to_string(),
            result: ExecResult::failure(2, "boom"),
            timed_out: false,
        }];
        let opts = GatherOptions {
            format: "json".to_string(),
            ..Default::default()
        };
        let output = gather_results(&results, &opts);
        assert!(output.dropped_failures.is_empty());
        assert!(output.text.contains("\"ok\": false"));
        assert!(output.text.contains("\"code\": 2"));
    }

    #[test]
    fn test_gather_results_json() {
        let results = vec![ScatterResult {
            item: "test".to_string(),
            result: ExecResult::success("output"),
            timed_out: false,
        }];

        let opts = GatherOptions {
            format: "json".to_string(),
            ..Default::default()
        };
        let output = gather_results(&results, &opts);
        assert!(output.text.contains("\"item\": \"test\""));
        assert!(output.text.contains("\"ok\": true"));
    }

    #[test]
    fn test_gather_results_first_n() {
        let results = vec![
            ScatterResult {
                item: "a".to_string(),
                result: ExecResult::success("1"),
                timed_out: false,
            },
            ScatterResult {
                item: "b".to_string(),
                result: ExecResult::success("2"),
                timed_out: false,
            },
            ScatterResult {
                item: "c".to_string(),
                result: ExecResult::success("3"),
                timed_out: false,
            },
        ];

        let opts = GatherOptions {
            first: 2,
            ..Default::default()
        };
        let output = gather_results(&results, &opts);
        assert_eq!(output.text, "1\n2");
    }

    #[test]
    fn test_parse_scatter_options() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("as".to_string(), Value::String("URL".to_string()));
        args.named.insert("limit".to_string(), Value::Int(4));

        let opts = parse_scatter_options(&args);
        assert_eq!(opts.var_name, "URL");
        assert_eq!(opts.limit, 4);
    }

    #[test]
    fn test_parse_gather_options() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("first".to_string(), Value::Int(5));
        args.named.insert("format".to_string(), Value::String("json".to_string()));

        let opts = parse_gather_options(&args);
        assert_eq!(opts.first, 5);
        assert_eq!(opts.format, "json");
    }

    #[test]
    fn scatter_limit_clamps_to_ceiling() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("limit".to_string(), Value::Int(999_999));
        let opts = parse_scatter_options(&args);
        assert_eq!(opts.limit, SCATTER_LIMIT_MAX);
    }

    #[test]
    fn scatter_limit_raises_zero_to_one() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("limit".to_string(), Value::Int(0));
        let opts = parse_scatter_options(&args);
        assert_eq!(opts.limit, 1);
    }

    #[test]
    fn scatter_limit_raises_negative_to_one() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("limit".to_string(), Value::Int(-42));
        let opts = parse_scatter_options(&args);
        assert_eq!(opts.limit, 1);
    }

    #[test]
    fn scatter_limit_preserves_valid_values() {
        use crate::tools::ToolArgs;

        let mut args = ToolArgs::new();
        args.named.insert("limit".to_string(), Value::Int(500));
        let opts = parse_scatter_options(&args);
        assert_eq!(opts.limit, 500);
    }
}
