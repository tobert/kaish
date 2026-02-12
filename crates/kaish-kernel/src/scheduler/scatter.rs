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

use tokio::sync::Semaphore;
use tracing::Instrument;

use crate::ast::{Command, Value};
use crate::dispatch::CommandDispatcher;
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
}

impl Default for ScatterOptions {
    fn default() -> Self {
        Self {
            var_name: "ITEM".to_string(),
            limit: 8,
        }
    }
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
}

/// Runs scatter/gather pipelines.
///
/// The dispatcher is used for pre_scatter, post_gather, and parallel worker
/// command execution. This enables scatter blocks to run user tools, scripts,
/// and external commands — not just builtins.
pub struct ScatterGatherRunner {
    tools: Arc<ToolRegistry>,
    dispatcher: Arc<dyn CommandDispatcher>,
}

impl ScatterGatherRunner {
    /// Create a new scatter/gather runner with the given dispatcher.
    ///
    /// The dispatcher handles the full command resolution chain for all
    /// pipeline stages (pre_scatter, parallel workers, post_gather).
    pub fn new(tools: Arc<ToolRegistry>, dispatcher: Arc<dyn CommandDispatcher>) -> Self {
        Self { tools, dispatcher }
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
        let input = if pre_scatter.is_empty() {
            // Use existing stdin
            ctx.take_stdin().unwrap_or_default()
        } else {
            let result = runner.run_sequential(pre_scatter, ctx, &*self.dispatcher).await;
            if !result.ok() {
                return result;
            }
            result.out
        };

        // Split input into items
        let items = split_input(&input);
        if items.is_empty() {
            return ExecResult::success("");
        }

        tracing::Span::current().record("item_count", items.len());

        // Run parallel stage
        let results = self
            .run_parallel(&items, &scatter_opts, parallel, ctx)
            .await;

        // Gather results
        let gathered = gather_results(&results, &gather_opts);

        // Run post-gather commands if any
        if post_gather.is_empty() {
            ExecResult::success(gathered)
        } else {
            ctx.set_stdin(gathered);
            runner.run_sequential(post_gather, ctx, &*self.dispatcher).await
        }
    }

    /// Run the parallel stage for all items.
    ///
    /// # Safety constraint
    ///
    /// Parallel workers share the dispatcher via `Arc`. The dispatcher MUST be
    /// stateless (like `BackendDispatcher`) when used from parallel workers.
    /// Using a stateful dispatcher (like `Kernel`, which writes to `self.scope`)
    /// would cause data races — parallel workers would stomp each other's scope.
    ///
    /// The `Kernel` dispatcher is safe for pre_scatter and post_gather (sequential),
    /// but scatter workers must use `BackendDispatcher` until per-worker kernel
    /// instances are implemented.
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
        let dispatcher = self.dispatcher.clone();
        let var_name = opts.var_name.clone();

        // Spawn parallel tasks
        let mut handles = Vec::with_capacity(items.len());

        for item in items.iter().cloned() {
            let permit = semaphore.clone().acquire_owned().await;
            let tools = tools.clone();
            let dispatcher = dispatcher.clone();
            let commands = commands.to_vec();
            let var_name = var_name.clone();
            let base_scope = base_ctx.scope.clone();
            let backend = base_ctx.backend.clone();
            let cwd = base_ctx.cwd.clone();

            let item_label = if item.len() > 64 {
                format!("{}...", &item[..64])
            } else {
                item.clone()
            };
            let worker_span = tracing::debug_span!("scatter_worker", item = %item_label);
            let handle = tokio::spawn(async move {
                let _permit = permit; // Hold permit until done

                // Create context for this worker
                let mut scope = base_scope;
                scope.set(&var_name, Value::String(item.clone()));

                let mut ctx = ExecContext::with_backend_and_scope(backend, scope);
                ctx.set_cwd(cwd);

                // Run through PipelineRunner + dispatcher (full resolution chain).
                // Uses run_sequential to avoid async recursion and infinite future size.
                let runner = PipelineRunner::new(tools);
                let result = runner.run_sequential(&commands, &mut ctx, &*dispatcher).await;

                ScatterResult { item, result }
            }.instrument(worker_span));

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
                    });
                }
            }
        }

        results
    }
}

/// Split input into items (by newlines or JSON array).
fn split_input(input: &str) -> Vec<String> {
    let trimmed = input.trim();

    // Try to parse as JSON array first
    if trimmed.starts_with('[')
        && let Ok(arr) = serde_json::from_str::<Vec<serde_json::Value>>(trimmed) {
            return arr
                .into_iter()
                .map(|v| match v {
                    serde_json::Value::String(s) => s,
                    other => other.to_string(),
                })
                .collect();
        }

    // Fall back to line splitting
    trimmed
        .lines()
        .map(|s| s.to_string())
        .filter(|s| !s.is_empty())
        .collect()
}

/// Gather results into output string.
fn gather_results(results: &[ScatterResult], opts: &GatherOptions) -> String {
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
                    "out": r.result.out.trim(),
                    "err": r.result.err.trim(),
                })
            })
            .collect();

        serde_json::to_string_pretty(&json_results).unwrap_or_default()
    } else {
        // Output as lines (stdout from each, separated by newlines)
        results_to_use
            .iter()
            .filter(|r| r.result.ok())
            .map(|r| r.result.out.trim())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

/// Parse scatter options from tool args.
pub fn parse_scatter_options(args: &crate::tools::ToolArgs) -> ScatterOptions {
    let mut opts = ScatterOptions::default();

    if let Some(Value::String(name)) = args.named.get("as") {
        opts.var_name = name.clone();
    }

    if let Some(Value::Int(n)) = args.named.get("limit") {
        opts.limit = (*n).max(1) as usize;
    }

    opts
}

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
    fn test_split_input_lines() {
        let input = "one\ntwo\nthree\n";
        let items = split_input(input);
        assert_eq!(items, vec!["one", "two", "three"]);
    }

    #[test]
    fn test_split_input_json_array() {
        let input = r#"["a", "b", "c"]"#;
        let items = split_input(input);
        assert_eq!(items, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_split_input_json_mixed() {
        let input = r#"[1, "two", true]"#;
        let items = split_input(input);
        assert_eq!(items, vec!["1", "two", "true"]);
    }

    #[test]
    fn test_split_input_empty() {
        let input = "";
        let items = split_input(input);
        assert!(items.is_empty());
    }

    #[test]
    fn test_gather_results_lines() {
        let results = vec![
            ScatterResult {
                item: "a".to_string(),
                result: ExecResult::success("result_a"),
            },
            ScatterResult {
                item: "b".to_string(),
                result: ExecResult::success("result_b"),
            },
        ];

        let opts = GatherOptions::default();
        let output = gather_results(&results, &opts);
        assert_eq!(output, "result_a\nresult_b");
    }

    #[test]
    fn test_gather_results_json() {
        let results = vec![ScatterResult {
            item: "test".to_string(),
            result: ExecResult::success("output"),
        }];

        let opts = GatherOptions {
            format: "json".to_string(),
            ..Default::default()
        };
        let output = gather_results(&results, &opts);
        assert!(output.contains("\"item\": \"test\""));
        assert!(output.contains("\"ok\": true"));
    }

    #[test]
    fn test_gather_results_first_n() {
        let results = vec![
            ScatterResult {
                item: "a".to_string(),
                result: ExecResult::success("1"),
            },
            ScatterResult {
                item: "b".to_string(),
                result: ExecResult::success("2"),
            },
            ScatterResult {
                item: "c".to_string(),
                result: ExecResult::success("3"),
            },
        ];

        let opts = GatherOptions {
            first: 2,
            ..Default::default()
        };
        let output = gather_results(&results, &opts);
        assert_eq!(output, "1\n2");
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
}
