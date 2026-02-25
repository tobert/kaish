//! sleep — Delay for a specified time.

use async_trait::async_trait;
use std::time::Duration;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Sleep tool: pause execution for a specified duration.
pub struct Sleep;

#[async_trait]
impl Tool for Sleep {
    fn name(&self) -> &str {
        "sleep"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("sleep", "Delay for a specified time")
            .param(ParamSchema::required(
                "seconds",
                "number",
                "Number of seconds to sleep (supports decimals)",
            ))
            .example("Sleep for 1 second", "sleep 1")
            .example("Sleep for half a second", "sleep 0.5")
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        let seconds = match args.get_positional(0) {
            Some(Value::Int(i)) => *i as f64,
            Some(Value::Float(f)) => *f,
            Some(Value::String(s)) => match parse_duration(s) {
                Ok(d) => d,
                Err(e) => return ExecResult::failure(1, format!("sleep: {}", e)),
            },
            _ => return ExecResult::failure(1, "sleep: missing seconds argument"),
        };

        if seconds < 0.0 {
            return ExecResult::failure(1, "sleep: invalid time interval");
        }

        let duration = Duration::from_secs_f64(seconds);
        tokio::time::sleep(duration).await;

        ExecResult::with_output(OutputData::text(""))
    }
}

/// Parse duration string with optional suffix (s, m, h, d).
fn parse_duration(s: &str) -> Result<f64, String> {
    let s = s.trim();
    if s.is_empty() {
        return Err("missing operand".to_string());
    }

    // Check for suffix
    let (num_str, multiplier) = if let Some(rest) = s.strip_suffix('s') {
        (rest, 1.0)
    } else if let Some(rest) = s.strip_suffix('m') {
        (rest, 60.0)
    } else if let Some(rest) = s.strip_suffix('h') {
        (rest, 3600.0)
    } else if let Some(rest) = s.strip_suffix('d') {
        (rest, 86400.0)
    } else {
        (s, 1.0)
    };

    num_str
        .parse::<f64>()
        .map(|n| n * multiplier)
        .map_err(|_| format!("invalid time interval '{}'", s))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;
    use std::time::Instant;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_sleep_integer() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(0)); // 0 seconds for fast test

        let result = Sleep.execute(args, &mut ctx).await;
        assert!(result.ok());
    }

    #[tokio::test]
    async fn test_sleep_float() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Float(0.05)); // 50ms

        let start = Instant::now();
        let result = Sleep.execute(args, &mut ctx).await;
        let elapsed = start.elapsed();

        assert!(result.ok());
        assert!(elapsed >= Duration::from_millis(40)); // Allow some tolerance
    }

    #[tokio::test]
    async fn test_sleep_string_seconds() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("0.01s".into()));

        let result = Sleep.execute(args, &mut ctx).await;
        assert!(result.ok());
    }

    #[tokio::test]
    async fn test_sleep_negative() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Float(-1.0));

        let result = Sleep.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_sleep_missing_arg() {
        let mut ctx = make_ctx();
        let result = Sleep.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }

    #[test]
    fn test_parse_duration() {
        assert!((parse_duration("1").unwrap() - 1.0).abs() < f64::EPSILON);
        assert!((parse_duration("1.5").unwrap() - 1.5).abs() < f64::EPSILON);
        assert!((parse_duration("1s").unwrap() - 1.0).abs() < f64::EPSILON);
        assert!((parse_duration("1m").unwrap() - 60.0).abs() < f64::EPSILON);
        assert!((parse_duration("1h").unwrap() - 3600.0).abs() < f64::EPSILON);
        assert!((parse_duration("0.5h").unwrap() - 1800.0).abs() < f64::EPSILON);
        assert!(parse_duration("abc").is_err());
    }
}
