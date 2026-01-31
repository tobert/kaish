//! date — Display current date and time.

use async_trait::async_trait;
use chrono::{Local, Utc};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Date tool: display current date/time.
pub struct Date;

#[async_trait]
impl Tool for Date {
    fn name(&self) -> &str {
        "date"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("date", "Display current date and time")
            .param(ParamSchema::optional(
                "format",
                "string",
                Value::String("%Y-%m-%d %H:%M:%S".into()),
                "strftime format string",
            ))
            .param(ParamSchema::optional(
                "utc",
                "bool",
                Value::Bool(false),
                "Use UTC instead of local time (-u)",
            ))
            .param(ParamSchema::optional(
                "iso",
                "bool",
                Value::Bool(false),
                "Output in ISO 8601 format",
            ))
            .param(ParamSchema::optional(
                "unix",
                "bool",
                Value::Bool(false),
                "Output Unix timestamp (seconds since epoch)",
            ))
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        let use_utc = args.has_flag("utc") || args.has_flag("u");
        let use_iso = args.has_flag("iso");
        let use_unix = args.has_flag("unix");

        let output = if use_unix {
            // Unix timestamp
            if use_utc {
                Utc::now().timestamp().to_string()
            } else {
                Local::now().timestamp().to_string()
            }
        } else if use_iso {
            // ISO 8601
            if use_utc {
                Utc::now().to_rfc3339()
            } else {
                Local::now().to_rfc3339()
            }
        } else {
            // Custom format or default
            // Strip leading + for Unix date compatibility (date +%s -> date %s)
            let format = args
                .get_string("format", 0)
                .map(|s| s.strip_prefix('+').unwrap_or(&s).to_string())
                .unwrap_or_else(|| "%Y-%m-%d %H:%M:%S".to_string());

            if use_utc {
                Utc::now().format(&format).to_string()
            } else {
                Local::now().format(&format).to_string()
            }
        };

        ExecResult::success(format!("{}\n", output))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_date_default() {
        let mut ctx = make_ctx();
        let result = Date.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        // Default format: YYYY-MM-DD HH:MM:SS
        assert!(result.out.contains('-')); // Has date separators
        assert!(result.out.contains(':')); // Has time separators
    }

    #[tokio::test]
    async fn test_date_custom_format() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("%Y".into()));

        let result = Date.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should be just the year (4 digits + newline)
        let year = result.out.trim();
        assert_eq!(year.len(), 4);
        assert!(year.chars().all(|c| c.is_ascii_digit()));
    }

    #[tokio::test]
    async fn test_date_iso() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("iso".to_string());

        let result = Date.execute(args, &mut ctx).await;
        assert!(result.ok());
        // ISO 8601 format contains 'T' separator
        assert!(result.out.contains('T'));
    }

    #[tokio::test]
    async fn test_date_unix() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("unix".to_string());

        let result = Date.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should be all digits
        let timestamp = result.out.trim();
        assert!(timestamp.chars().all(|c| c.is_ascii_digit()));
        // Reasonable timestamp (after year 2000)
        let ts: i64 = timestamp.parse().unwrap_or(0);
        assert!(ts > 946684800); // Jan 1, 2000
    }

    #[tokio::test]
    async fn test_date_utc_flag() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("u".to_string());
        args.flags.insert("iso".to_string());

        let result = Date.execute(args, &mut ctx).await;
        assert!(result.ok());
        // UTC ISO format ends with +00:00 or Z
        assert!(result.out.contains("+00:00") || result.out.contains('Z'));
    }

    #[tokio::test]
    async fn test_date_utc_long_flag() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("utc".to_string());
        args.flags.insert("iso".to_string());

        let result = Date.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("+00:00") || result.out.contains('Z'));
    }
}
