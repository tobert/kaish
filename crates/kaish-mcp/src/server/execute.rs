//! Execute tool implementation.
//!
//! The `execute` tool runs kaish scripts in a fresh, isolated kernel.

use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;

use anyhow::{Context, Result};
use kaish_kernel::{Kernel, KernelConfig};
use serde::{Deserialize, Serialize};

use super::config::ExternalServerConfig;

/// Parameters for the execute tool.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecuteParams {
    /// The kaish script or command to execute.
    pub script: String,

    /// Initial working directory (VFS path, default: /mnt/local).
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecuteResult {
    /// Exit code (0 = success).
    pub code: i64,

    /// Standard output.
    pub stdout: String,

    /// Standard error.
    pub stderr: String,

    /// Parsed JSON data from stdout, if valid JSON.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<serde_json::Value>,

    /// Whether the command succeeded (code == 0).
    pub ok: bool,
}

impl ExecuteResult {
    /// Create a success result.
    pub fn success(stdout: String) -> Self {
        let data = serde_json::from_str(&stdout).ok();
        Self {
            code: 0,
            stdout,
            stderr: String::new(),
            data,
            ok: true,
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
        }
    }

    /// Create from kernel execution result.
    pub fn from_exec_result(result: &kaish_kernel::interpreter::ExecResult) -> Self {
        let data = result.data.as_ref().and_then(|v| {
            match v {
                kaish_kernel::ast::Value::Null => Some(serde_json::Value::Null),
                kaish_kernel::ast::Value::Bool(b) => Some(serde_json::Value::Bool(*b)),
                kaish_kernel::ast::Value::Int(i) => Some(serde_json::Value::Number((*i).into())),
                kaish_kernel::ast::Value::Float(f) => {
                    serde_json::Number::from_f64(*f).map(serde_json::Value::Number)
                }
                kaish_kernel::ast::Value::String(s) => {
                    // Try to parse as JSON first
                    serde_json::from_str(s).ok()
                        .or_else(|| Some(serde_json::Value::String(s.clone())))
                }
            }
        });

        Self {
            code: result.code,
            stdout: result.out.clone(),
            stderr: result.err.clone(),
            data,
            ok: result.ok(),
        }
    }
}

/// Execute a kaish script in a fresh kernel.
///
/// This function runs the kernel execution in a blocking task because the kaish
/// kernel's execute method returns a non-Send future (due to internal use of
/// Pin<Box<dyn Future>>). The rmcp server handler requires Send futures, so we
/// bridge the gap by running the kernel in a dedicated tokio LocalSet.
pub async fn execute(
    params: ExecuteParams,
    external_servers: &[ExternalServerConfig],
    default_timeout_ms: u64,
) -> Result<ExecuteResult> {
    let timeout_ms = params.timeout_ms.unwrap_or(default_timeout_ms);

    // Register external MCP tools if configured
    // Note: For v1 stateless design, we don't connect to external MCPs per-request
    // as that would be too slow. This is a placeholder for future session-based design.
    if !external_servers.is_empty() {
        tracing::debug!(
            "External MCP servers configured but not connected in stateless mode: {:?}",
            external_servers.iter().map(|s| &s.name).collect::<Vec<_>>()
        );
    }

    // Run the kernel execution in a blocking task with its own runtime
    // because kernel.execute() returns a non-Send future.
    let result = tokio::task::spawn_blocking(move || -> Result<ExecuteResult> {
        // Create a new single-threaded runtime for this execution
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .context("Failed to create runtime")?;

        rt.block_on(async move {
            let timeout = Duration::from_millis(timeout_ms);

            // Create a fresh kernel
            let cwd = params
                .cwd
                .map(PathBuf::from)
                .unwrap_or_else(|| PathBuf::from("/mnt/local"));

            let config = KernelConfig {
                name: "mcp-execute".to_string(),
                mount_local: true,
                local_root: None,
                cwd,
                skip_validation: false,
            };

            let kernel = Kernel::new(config).context("Failed to create kernel")?;

            // Set environment variables if provided
            if let Some(env) = params.env {
                for (key, value) in env {
                    kernel
                        .set_var(&key, kaish_kernel::ast::Value::String(value))
                        .await;
                }
            }

            // Execute with timeout
            let result = tokio::time::timeout(timeout, kernel.execute(&params.script)).await;

            let exec_result = match result {
                Ok(Ok(exec_result)) => ExecuteResult::from_exec_result(&exec_result),
                Ok(Err(e)) => ExecuteResult::failure(1, e.to_string()),
                Err(_) => ExecuteResult::failure(
                    124, // Standard timeout exit code
                    format!("Execution timed out after {}ms", timeout_ms),
                ),
            };
            Ok(exec_result)
        })
    })
    .await
    .context("Kernel execution task panicked")??;

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

        let result = execute(params, &[], 30_000).await.expect("execute failed");
        assert!(result.ok);
        assert_eq!(result.code, 0);
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

        let result = execute(params, &[], 30_000).await.expect("execute failed");
        assert!(result.ok);
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

        let result = execute(params, &[], 30_000).await.expect("execute failed");
        assert!(!result.ok);
        assert_eq!(result.code, 127);
    }

    #[tokio::test]
    async fn test_execute_json_output() {
        let params = ExecuteParams {
            script: r#"echo "{\"count\": 42}""#.to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        };

        let result = execute(params, &[], 30_000).await.expect("execute failed");
        assert!(result.ok);
        assert!(result.data.is_some());

        let data = result.data.expect("expected data");
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

        let result = execute(params, &[], 30_000).await.expect("execute failed");
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
}
