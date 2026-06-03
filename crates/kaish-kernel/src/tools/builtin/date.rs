//! date — Display current date and time.

use async_trait::async_trait;
use chrono::{Local, Utc};
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Date tool: display current date/time.
pub struct Date;

/// clap-derived argv layer for date. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "date", about = "Display current date and time")]
struct DateArgs {
    /// Use UTC instead of local time (-u)
    #[arg(short = 'u', long = "utc")]
    utc: bool,

    /// Output in ISO 8601 format
    #[arg(long)]
    iso: bool,

    /// Output Unix timestamp (seconds since epoch)
    #[arg(long)]
    unix: bool,

    /// strftime format string (positional)
    #[arg(long)]
    format: Option<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Optional `+FORMAT` and/or `@TIMESTAMP` arguments.
    args: Vec<String>,
}

#[async_trait]
impl Tool for Date {
    fn name(&self) -> &str {
        "date"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &DateArgs::command(),
            "date",
            "Display current date and time",
            [
                ("Current date and time", "date"),
                ("ISO 8601 format", "date --iso"),
                ("Unix timestamp", "date --unix"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match DateArgs::try_parse_from(
            std::iter::once("date".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("date: {e}")),
        };
        parsed.global.apply(ctx);

        let use_utc = parsed.utc;
        let use_iso = parsed.iso;
        let use_unix = parsed.unix;

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

        ExecResult::with_output(OutputData::text(format!("{}\n", output)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
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
        assert!(result.text_out().contains('-')); // Has date separators
        assert!(result.text_out().contains(':')); // Has time separators
    }

    #[tokio::test]
    async fn test_date_custom_format() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("%Y".into()));

        let result = Date.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should be just the year (4 digits + newline)
        let out = result.text_out();
        let year = out.trim();
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
        assert!(result.text_out().contains('T'));
    }

    #[tokio::test]
    async fn test_date_unix() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("unix".to_string());

        let result = Date.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should be all digits
        let out = result.text_out();
        let timestamp = out.trim();
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
        assert!(result.text_out().contains("+00:00") || result.text_out().contains('Z'));
    }

    #[tokio::test]
    async fn test_date_utc_long_flag() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("utc".to_string());
        args.flags.insert("iso".to_string());

        let result = Date.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("+00:00") || result.text_out().contains('Z'));
    }
}
