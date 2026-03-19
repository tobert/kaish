//! timeout — Run a command with a time limit.
//!
//! Dispatches the inner command through the full resolution chain via
//! `ctx.dispatcher` (user tools → builtins → .kai scripts → external commands).
//! Returns exit code 124 on timeout (matching coreutils convention).

use async_trait::async_trait;
use std::time::Duration;

use crate::ast::{Arg, Command, Expr, Value};
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Timeout tool: run a command with a deadline.
pub struct Timeout;

#[async_trait]
impl Tool for Timeout {
    fn name(&self) -> &str {
        "timeout"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("timeout", "Run a command with a time limit")
            .param(ParamSchema::required(
                "duration",
                "string",
                "Time limit: 30 (seconds), 30s, 500ms, 5m, 1h",
            ))
            .param(ParamSchema::required(
                "command",
                "string",
                "Command to run",
            ))
            .example("With seconds", "timeout 5 sleep 10")
            .example("With duration suffix", "timeout 500ms curl example.com")
            .example("Minutes", "timeout 2m cargo build")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Need at least duration + command
        if args.positional.len() < 2 {
            return ExecResult::failure(
                1,
                "timeout: usage: timeout DURATION COMMAND [ARGS...]",
            );
        }

        // Parse duration from first positional
        let duration_str = match &args.positional[0] {
            Value::String(s) => s.clone(),
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            other => {
                return ExecResult::failure(
                    1,
                    format!("timeout: invalid duration: {:?}", other),
                )
            }
        };

        let duration = match parse_duration(&duration_str) {
            Some(d) => d,
            None => {
                return ExecResult::failure(
                    1,
                    format!("timeout: invalid duration '{}' (try: 30, 5s, 500ms, 2m, 1h)", duration_str),
                )
            }
        };

        // Command name from second positional
        let cmd_name = match &args.positional[1] {
            Value::String(s) => s.clone(),
            other => {
                return ExecResult::failure(
                    1,
                    format!("timeout: invalid command: {:?}", other),
                )
            }
        };

        // Build an AST Command for the inner command so we can dispatch it
        // through the full resolution chain.
        let inner_args: Vec<Arg> = args.positional[2..]
            .iter()
            .map(|v| Arg::Positional(Expr::Literal(v.clone())))
            .collect();

        let inner_cmd = Command {
            name: cmd_name,
            args: inner_args,
            redirects: vec![],
        };

        // Dispatch through the full chain via ctx.dispatcher
        let Some(dispatcher) = ctx.dispatcher.clone() else {
            return ExecResult::failure(1,
                "timeout: no dispatcher available (Kernel must be created via into_arc())");
        };

        match tokio::time::timeout(duration, dispatcher.dispatch(&inner_cmd, ctx)).await {
            Ok(Ok(result)) => result,
            Ok(Err(e)) => ExecResult::failure(1, format!("timeout: {}", e)),
            Err(_elapsed) => {
                ExecResult::failure(124, format!("timeout: timed out after {}", duration_str))
            }
        }
    }
}

/// Parse a duration string: "30" (seconds), "30s", "500ms", "5m", "1h"
fn parse_duration(s: &str) -> Option<Duration> {
    let s = s.trim();

    // Try pure number (seconds)
    if let Ok(secs) = s.parse::<f64>() {
        return if secs >= 0.0 {
            Some(Duration::from_secs_f64(secs))
        } else {
            None
        };
    }

    // Try with suffix
    if let Some(num) = s.strip_suffix("ms") {
        let ms: u64 = num.trim().parse().ok()?;
        return Some(Duration::from_millis(ms));
    }
    if let Some(num) = s.strip_suffix('s') {
        let secs: f64 = num.trim().parse().ok()?;
        return if secs >= 0.0 {
            Some(Duration::from_secs_f64(secs))
        } else {
            None
        };
    }
    if let Some(num) = s.strip_suffix('m') {
        let mins: f64 = num.trim().parse().ok()?;
        return if mins >= 0.0 {
            Some(Duration::from_secs_f64(mins * 60.0))
        } else {
            None
        };
    }
    if let Some(num) = s.strip_suffix('h') {
        let hours: f64 = num.trim().parse().ok()?;
        return if hours >= 0.0 {
            Some(Duration::from_secs_f64(hours * 3600.0))
        } else {
            None
        };
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::kernel::{Kernel, KernelConfig};

    /// Create a Kernel wrapped in Arc for tests that need full dispatch.
    async fn make_kernel() -> std::sync::Arc<Kernel> {
        Kernel::new(KernelConfig::isolated().with_skip_validation(true))
            .unwrap()
            .into_arc()
    }

    // --- Duration parsing tests ---

    #[test]
    fn test_parse_duration_seconds() {
        assert_eq!(parse_duration("30"), Some(Duration::from_secs(30)));
        assert_eq!(parse_duration("0"), Some(Duration::from_secs(0)));
        assert_eq!(parse_duration("1.5"), Some(Duration::from_secs_f64(1.5)));
    }

    #[test]
    fn test_parse_duration_suffix() {
        assert_eq!(parse_duration("500ms"), Some(Duration::from_millis(500)));
        assert_eq!(parse_duration("5s"), Some(Duration::from_secs(5)));
        assert_eq!(parse_duration("2m"), Some(Duration::from_secs(120)));
        assert_eq!(parse_duration("1h"), Some(Duration::from_secs(3600)));
    }

    #[test]
    fn test_parse_duration_invalid() {
        assert_eq!(parse_duration("abc"), None);
        assert_eq!(parse_duration(""), None);
        assert_eq!(parse_duration("-5"), None);
        assert_eq!(parse_duration("5x"), None);
    }

    // --- Integration tests via Kernel::execute ---

    #[tokio::test]
    async fn test_timeout_missing_args() {
        let kernel = make_kernel().await;
        let result = kernel.execute("timeout").await.unwrap();
        assert!(!result.ok());
        assert!(result.err.contains("usage"));
    }

    #[tokio::test]
    async fn test_timeout_invalid_duration() {
        let kernel = make_kernel().await;
        let result = kernel.execute("timeout abc echo hi").await.unwrap();
        assert!(!result.ok());
        assert!(result.err.contains("invalid duration"));
    }

    #[tokio::test]
    async fn test_timeout_builtin_succeeds() {
        let kernel = make_kernel().await;
        let result = kernel.execute("timeout 5s echo hello").await.unwrap();
        assert!(result.ok());
        assert!(result.text_out().contains("hello"));
    }

    #[tokio::test]
    async fn test_timeout_builtin_times_out() {
        let kernel = make_kernel().await;
        let result = kernel.execute("timeout 100ms sleep 10").await.unwrap();
        assert_eq!(result.code, 124);
        assert!(result.err.contains("timed out"));
    }

    #[tokio::test]
    async fn test_timeout_command_not_found() {
        let kernel = make_kernel().await;
        let result = kernel.execute("timeout 5s not_a_command_xyz_123").await.unwrap();
        assert!(!result.ok());
        assert_eq!(result.code, 127);
    }

    #[tokio::test]
    async fn test_timeout_numeric_duration() {
        let kernel = make_kernel().await;
        let result = kernel.execute("timeout 5 echo works").await.unwrap();
        assert!(result.ok());
        assert!(result.text_out().contains("works"));
    }
}
