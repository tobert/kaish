//! Execute tool implementation.
//!
//! The `execute` tool runs kaish scripts in a fresh, isolated kernel.

use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
use std::time::Duration;

use anyhow::{Context, Result};
use kaish_kernel::interpreter::{value_to_json, OutputData};
use kaish_kernel::nonce::NonceStore;
use kaish_kernel::{ExecuteOptions, Kernel, KernelConfig};
use rmcp::schemars::{self, JsonSchema};
use serde::{Deserialize, Serialize};

/// W3C trace context lifted from the MCP request `_meta`.
///
/// This is transport metadata, not tool input — it rides alongside the call the
/// way `nonce_store` and `init_paths` do, not inside [`ExecuteParams`] (which
/// mirrors the tool's argument schema). Forwarded to the kernel via
/// [`ExecuteOptions`] so an MCP client's trace spans the kaish boundary.
#[derive(Debug, Clone, Default)]
pub struct McpTraceContext {
    pub traceparent: Option<String>,
    pub tracestate: Option<String>,
    pub baggage: BTreeMap<String, String>,
}

/// Parameters for the execute tool.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecuteParams {
    /// The kaish script or command to execute.
    pub script: String,

    /// Initial working directory (default: $HOME).
    #[serde(default)]
    pub cwd: Option<String>,

    /// Environment variables to set.
    #[serde(default)]
    pub env: Option<HashMap<String, String>>,

    /// Timeout in milliseconds (default: 30000).
    #[serde(default)]
    pub timeout_ms: Option<u64>,
}

/// Result from script execution.
///
/// `output` and `data` are complementary, not duplicative:
/// - `output`: structured output model for rendering. Present for builtins, None for
///   external commands and `--json` results (cleared by the sentinel pattern in
///   `apply_output_format`).
/// - `data`: structured data set by the executed tool. Only present when a builtin
///   opts in (e.g. `seq`, `jq`, `cut`, `find`, `glob`) or `--json` was used. kaish
///   never sniffs external-command stdout for JSON — pipe through `jq` to opt in.
/// - When `--json` is used, `output` is None and the JSON is in `stdout`/`data`.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ExecuteResult {
    /// Exit code (0 = success).
    pub code: i64,

    /// Standard output. Omitted from structured_content when `data` carries the same info.
    #[serde(default)]
    pub stdout: String,

    /// Standard error.
    pub stderr: String,

    /// Structured data set by the tool/builtin. Never inferred from stdout —
    /// external commands always have this as None unless their output is piped
    /// through a builtin that populates it (e.g. `jq`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<serde_json::Value>,

    /// Whether the command succeeded (code == 0).
    pub ok: bool,

    /// Structured output for rendering. Present for builtins, None for
    /// external commands and --json results. Carried as the typed kernel struct;
    /// serialization happens at the MCP wire boundary.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub output: Option<OutputData>,

    /// MIME content type hint. When set, MCP handler uses this for response MIME type.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content_type: Option<String>,

    /// W3C baggage carried back out of execution (trace-context egress): the
    /// embedder's incoming baggage merged with any tool-emitted entries
    /// (tool wins on collision). The MCP handler also surfaces this on the
    /// response `_meta` so clients can read trace identifiers off the result.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub baggage: BTreeMap<String, String>,
}

impl ExecuteResult {
    /// Create a success result.
    pub fn success(stdout: String) -> Self {
        Self {
            code: 0,
            stdout,
            stderr: String::new(),
            data: None,
            ok: true,
            output: None,
            content_type: None,
            baggage: BTreeMap::new(),
        }
    }

    /// Create a failure result.
    pub fn failure(code: i64, stderr: String) -> Self {
        Self {
            code,
            stdout: String::new(),
            stderr,
            data: None,
            ok: false,
            output: None,
            content_type: None,
            baggage: BTreeMap::new(),
        }
    }

    /// Create from kernel execution result.
    pub fn from_exec_result(result: &kaish_kernel::interpreter::ExecResult) -> Self {
        let data = result.data.as_ref().map(value_to_json);

        Self {
            code: result.code,
            stdout: result.text_out().into_owned(),
            stderr: result.err.clone(),
            data,
            ok: result.ok(),
            output: result.output().cloned(),
            content_type: result.content_type.clone(),
            baggage: result.baggage.clone(),
        }
    }
}

/// Execute a kaish script in a fresh kernel.
///
/// This function runs the kernel execution in a blocking task because the kaish
/// kernel's execute method returns a non-Send future (due to internal use of
/// Pin<Box<dyn Future>>). The rmcp server handler requires Send futures, so we
/// bridge the gap by running the kernel in a dedicated tokio LocalSet.
///
/// If `init_paths` is non-empty, those .kai scripts are read from disk and
/// prepended to the user script before execution. Files are re-read on each
/// call so edits take effect without restarting the server.
///
/// Future optimization: if init scripts get heavy, snapshot interpreter state
/// after running init and clone for each execute instead of re-parsing.
pub async fn execute(
    params: ExecuteParams,
    default_timeout_ms: u64,
    nonce_store: Option<NonceStore>,
    init_paths: &[PathBuf],
    trace: McpTraceContext,
    overlay: bool,
) -> Result<ExecuteResult> {
    let timeout_ms = params.timeout_ms.unwrap_or(default_timeout_ms);

    // Read init scripts up front (before spawning the thread) so errors
    // propagate cleanly to the caller.
    let mut full_script = String::new();
    for path in init_paths {
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read init script: {}", path.display()))?;
        full_script.push_str(&content);
        full_script.push('\n');
    }
    full_script.push_str(&params.script);

    // Run the kernel execution in a dedicated thread with a large stack.
    // The default tokio worker stack (2MB) isn't enough for deep recursion
    // on large inputs (100KB+ strings). We use 16MB which should handle
    // most realistic inputs.
    //
    // PERF: This spawns a thread + runtime per request. If this becomes a
    // bottleneck, refactor to use a persistent LocalSet worker thread with
    // an mpsc channel for dispatching work. The thread-per-request approach
    // is simpler and works around kaish_kernel returning !Send futures.
    let (tx, rx) = tokio::sync::oneshot::channel();

    // Capture current span so kernel spans are children of the handler span
    let parent_span = tracing::Span::current();

    std::thread::Builder::new()
        .name("kaish-execute".to_string())
        .stack_size(16 * 1024 * 1024) // 16MB stack
        .spawn(move || {
            let _guard = parent_span.entered();

            // Create a new single-threaded runtime for this execution
            let rt = match tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
            {
                Ok(rt) => rt,
                Err(e) => {
                    let _ = tx.send(Err(anyhow::anyhow!("Failed to create runtime: {}", e)));
                    return;
                }
            };

            let result = rt.block_on(async move {
                let timeout = Duration::from_millis(timeout_ms);

                // Create a fresh kernel in sandboxed MCP mode
                // Default cwd is $HOME (from KernelConfig::mcp())
                let mut config = if let Some(cwd) = params.cwd {
                    KernelConfig::mcp().with_cwd(PathBuf::from(cwd))
                } else {
                    KernelConfig::mcp()
                };
                if let Some(store) = nonce_store {
                    config = config.with_nonce_store(store);
                }
                // Apply per-call overlay mode. Each call gets a fresh kernel
                // (and thus a fresh transaction). kaish-vfs commit must run
                // in the same call as writes, or changes are discarded.
                if overlay {
                    config = config.with_overlay(true);
                }

                // Apply kernel-level settings from per-request env before creating the kernel.
                // These must influence KernelConfig, not just shell variables.
                if let Some(ref env) = params.env {
                    if env.get("KAISH_LATCH").is_some_and(|v| v == "1") {
                        config = config.with_latch(true);
                    }
                    if env.get("KAISH_TRASH").is_some_and(|v| v == "1") {
                        config = config.with_trash(true);
                    }
                }

                // Build the initial-vars map: OS env first (so PATH/HOME/etc.
                // reach subprocesses — the kernel itself is hermetic and won't
                // read std::env::vars()), then per-request env overlaid on top
                // (per-request entries win on key collision). All entries are
                // marked exported by the kernel.
                let mut initial_vars: HashMap<String, kaish_kernel::ast::Value> =
                    std::env::vars()
                        .map(|(k, v)| (k, kaish_kernel::ast::Value::String(v)))
                        .collect();
                if let Some(env) = params.env {
                    for (k, v) in env {
                        initial_vars.insert(k, kaish_kernel::ast::Value::String(v));
                    }
                }
                config = config.with_initial_vars(initial_vars);

                let kernel = Kernel::new(config).context("Failed to create kernel")?.into_arc();

                // Execute with timeout. The kernel handles cancellation and
                // child-process kill via SIGTERM/grace/SIGKILL on elapsed,
                // returning exit 124 — no need for an outer tokio::time::timeout.
                //
                // Forward the client's W3C trace context so the kernel's
                // execution span parents onto the client's trace.
                let mut opts = ExecuteOptions::new().with_timeout(timeout);
                if let Some(traceparent) = trace.traceparent {
                    opts = opts.with_traceparent(traceparent);
                }
                if let Some(tracestate) = trace.tracestate {
                    opts = opts.with_tracestate(tracestate);
                }
                if !trace.baggage.is_empty() {
                    opts = opts.with_baggage(trace.baggage);
                }
                let result = kernel.execute_with_options(&full_script, opts).await;

                let exec_result = match result {
                    Ok(exec_result) => ExecuteResult::from_exec_result(&exec_result),
                    Err(e) => ExecuteResult::failure(1, e.to_string()),
                };
                Ok(exec_result)
            });

            let _ = tx.send(result);
        })
        .context("Failed to spawn execution thread")?;

    let result = rx
        .await
        .context("Execution thread terminated unexpectedly")??;

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_execute_echo() {
        let params = ExecuteParams {
            script: "echo hello".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        };

        let result = execute(params, 30_000, None, &[], McpTraceContext::default(), false)
            .await
            .expect("execute failed");
        assert!(result.ok);
        assert_eq!(result.code, 0);
        // Simple text passes through unchanged (no TOON encoding)
        assert_eq!(result.stdout.trim(), "hello");
    }

    #[tokio::test]
    async fn test_execute_with_env() {
        let mut env = HashMap::new();
        env.insert("GREETING".to_string(), "world".to_string());

        let params = ExecuteParams {
            script: r#"echo "hello ${GREETING}""#.to_string(),
            cwd: None,
            env: Some(env),
            timeout_ms: None,
        };

        let result = execute(params, 30_000, None, &[], McpTraceContext::default(), false)
            .await
            .expect("execute failed");
        assert!(result.ok);
        // Simple text passes through unchanged (no TOON encoding)
        assert_eq!(result.stdout.trim(), "hello world");
    }

    #[tokio::test]
    async fn test_execute_failure() {
        let params = ExecuteParams {
            script: "nonexistent_command".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        };

        let result = execute(params, 30_000, None, &[], McpTraceContext::default(), false)
            .await
            .expect("execute failed");
        assert!(!result.ok);
        assert_eq!(result.code, 127);
    }

    #[tokio::test]
    async fn test_execute_echo_json_does_not_auto_parse() {
        // External-command stdout that happens to be JSON is NOT sniffed.
        // To get `.data`, the user must opt in (e.g. by piping through `jq`).
        let params = ExecuteParams {
            script: r#"echo "{\"count\": 42}""#.to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        };

        let result = execute(params, 30_000, None, &[], McpTraceContext::default(), false)
            .await
            .expect("execute failed");
        assert!(result.ok);
        assert!(
            result.data.is_none(),
            "stdout must not be auto-parsed; got data = {:?}",
            result.data
        );
        assert!(result.stdout.contains(r#"{"count": 42}"#));
    }

    #[tokio::test]
    async fn test_execute_jq_populates_data() {
        // Piping through `jq` is the explicit opt-in for structured `.data`.
        let params = ExecuteParams {
            script: r#"echo '{"count": 42}' | jq '.'"#.to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        };

        let result = execute(params, 30_000, None, &[], McpTraceContext::default(), false)
            .await
            .expect("execute failed");
        assert!(
            result.ok,
            "script failed: code={} stderr={:?} stdout={:?}",
            result.code, result.stderr, result.stdout,
        );
        let data = result.data.expect("jq must populate .data");
        assert_eq!(data.get("count"), Some(&serde_json::json!(42)));
    }

    #[tokio::test]
    async fn test_execute_timeout() {
        let params = ExecuteParams {
            script: r#"
                I=0
                while [[ ${I} -lt 1000000 ]]; do
                    I=$((I + 1))
                done
            "#
            .to_string(),
            cwd: None,
            env: None,
            timeout_ms: Some(10), // Very short timeout
        };

        let result = execute(params, 30_000, None, &[], McpTraceContext::default(), false)
            .await
            .expect("execute failed");
        assert!(!result.ok);
        assert_eq!(result.code, 124);
        assert!(result.stderr.contains("timed out"));
    }

    #[tokio::test]
    async fn test_execute_result_from_exec_result() {
        use kaish_kernel::interpreter::ExecResult;

        let exec_result = ExecResult::success("test output");
        let result = ExecuteResult::from_exec_result(&exec_result);
        assert!(result.ok);
        assert_eq!(result.stdout, "test output");

        let exec_result = ExecResult::failure(42, "error message");
        let result = ExecuteResult::from_exec_result(&exec_result);
        assert!(!result.ok);
        assert_eq!(result.code, 42);
        assert_eq!(result.stderr, "error message");
    }

    #[test]
    fn from_exec_result_carries_baggage() {
        use kaish_kernel::interpreter::ExecResult;

        let mut exec_result = ExecResult::success("ok");
        exec_result
            .baggage
            .insert("owner".to_string(), "atobey".to_string());

        let result = ExecuteResult::from_exec_result(&exec_result);
        assert_eq!(result.baggage.get("owner").map(String::as_str), Some("atobey"));
    }

    #[test]
    fn empty_baggage_is_omitted_from_serialization() {
        use kaish_kernel::interpreter::ExecResult;

        let result = ExecuteResult::from_exec_result(&ExecResult::success("ok"));
        let json = serde_json::to_value(&result).expect("serialize");
        assert!(
            json.get("baggage").is_none(),
            "empty baggage must not appear on the wire",
        );
    }

    #[test]
    fn test_from_exec_result_string_stays_string() {
        // Value::String containing JSON must NOT be silently re-parsed into an object.
        // It becomes a JSON string, not a JSON object.
        use kaish_kernel::ast::Value;
        use kaish_kernel::interpreter::ExecResult;

        let mut exec_result = ExecResult::success("test");
        exec_result.data = Some(Value::String(r#"{"x": 1}"#.to_string()));

        let result = ExecuteResult::from_exec_result(&exec_result);
        let data = result.data.expect("data should be present");
        // Must be a JSON string, not an object
        assert!(data.is_string(), "Value::String should become a JSON string, not an object");
        assert_eq!(data.as_str(), Some(r#"{"x": 1}"#));
    }

    #[tokio::test]
    async fn test_execute_explicit_json_flag() {
        // `echo hello --json` applies JSON at the kernel level
        let params = ExecuteParams {
            script: "echo hello --json".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        };

        let result = execute(params, 30_000, None, &[], McpTraceContext::default(), false)
            .await
            .expect("execute failed");
        assert!(result.ok);
        // Should be valid JSON, not TOON-wrapped JSON
        let parsed: serde_json::Value = serde_json::from_str(&result.stdout).expect("valid JSON");
        assert_eq!(parsed, serde_json::json!("hello\n"));
    }

    #[tokio::test]
    async fn test_execute_structured_as_canonical_text() {
        // Structured builtins return readable canonical text by default
        let params = ExecuteParams {
            script: "kaish-mounts".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        };

        let result = execute(params, 30_000, None, &[], McpTraceContext::default(), false)
            .await
            .expect("execute failed");
        assert!(result.ok);
        // Canonical text: tab-separated values, one per line
        assert!(result.stdout.contains("/"), "should contain mount paths");
        assert!(result.stdout.contains("rw"), "should contain mount modes");
    }

    #[tokio::test]
    async fn test_execute_simple_text_not_toon_encoded() {
        // Simple text (echo, cat) should NOT be TOON-encoded
        let params = ExecuteParams {
            script: "echo hello world".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        };

        let result = execute(params, 30_000, None, &[], McpTraceContext::default(), false)
            .await
            .expect("execute failed");
        assert!(result.ok);
        // Plain text, not TOON-quoted
        assert_eq!(result.stdout.trim(), "hello world");
    }

    #[tokio::test]
    async fn test_execute_with_init_script() {
        let dir = tempfile::tempdir().unwrap();
        let init_path = dir.path().join("init.kai");
        std::fs::write(&init_path, "GREETING=hello\n").unwrap();

        let params = ExecuteParams {
            script: "echo $GREETING".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        };
        let result = execute(params, 30_000, None, &[init_path], McpTraceContext::default(), false)
            .await
            .expect("execute failed");
        assert!(result.ok);
        assert_eq!(result.stdout.trim(), "hello");
    }

    #[tokio::test]
    async fn test_execute_multiple_init_scripts() {
        let dir = tempfile::tempdir().unwrap();
        let init1 = dir.path().join("a.kai");
        let init2 = dir.path().join("b.kai");
        std::fs::write(&init1, "VAR1=foo\n").unwrap();
        std::fs::write(&init2, "VAR2=bar\n").unwrap();

        let params = ExecuteParams {
            script: "echo ${VAR1} ${VAR2}".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        };
        let result = execute(params, 30_000, None, &[init1, init2], McpTraceContext::default(), false)
            .await
            .expect("execute failed");
        assert!(result.ok);
        assert_eq!(result.stdout.trim(), "foo bar");
    }

    #[tokio::test]
    async fn test_execute_missing_init_script_fails() {
        let params = ExecuteParams {
            script: "echo hi".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        };
        let result = execute(
            params,
            30_000,
            None,
            &[PathBuf::from("/nonexistent/init.kai")],
            McpTraceContext::default(),
            false,
        )
        .await;
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("Failed to read init script"),
            "error should mention init script: {}",
            err_msg
        );
    }

    #[tokio::test]
    async fn test_execute_init_script_hot_reload() {
        let dir = tempfile::tempdir().unwrap();
        let init_path = dir.path().join("init.kai");

        // First run
        std::fs::write(&init_path, "GREETING=hello\n").unwrap();
        let params = ExecuteParams {
            script: "echo $GREETING".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        };
        let result1 = execute(
            params.clone(),
            30_000,
            None,
            std::slice::from_ref(&init_path),
            McpTraceContext::default(),
            false,
        )
            .await
            .expect("execute failed");
        assert!(result1.ok);
        assert_eq!(result1.stdout.trim(), "hello");

        // Modify file and run again — should pick up the change
        std::fs::write(&init_path, "GREETING=howdy\n").unwrap();
        let result2 = execute(params, 30_000, None, &[init_path], McpTraceContext::default(), false)
            .await
            .expect("execute failed");
        assert!(result2.ok);
        assert_eq!(result2.stdout.trim(), "howdy");
    }

    #[tokio::test]
    async fn test_execute_init_script_syntax_error() {
        let dir = tempfile::tempdir().unwrap();
        let init_path = dir.path().join("init.kai");
        // Missing 'fi' — unclosed if statement
        std::fs::write(&init_path, "if [[ true ]]; then echo hi\n").unwrap();

        let params = ExecuteParams {
            script: "echo main".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        };
        let result = execute(params, 30_000, None, &[init_path], McpTraceContext::default(), false)
            .await
            .expect("execute should return Ok with structured failure");
        assert!(!result.ok);
        assert!(!result.stderr.is_empty());
    }
}
