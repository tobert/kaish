//! hostname — Print the system hostname.
//!
//! # Examples
//!
//! ```kaish
//! hostname                       # → myhost
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

use super::uname::read_hostname;

/// Hostname tool: print the system hostname.
pub struct Hostname;

/// clap-derived argv layer for hostname.
#[derive(Parser, Debug)]
#[command(name = "hostname", about = "Print the system hostname")]
struct HostnameArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Sink — to_argv() always emits `--` before positionals.
    #[arg(hide = true)]
    rest: Vec<String>,
}

#[async_trait]
impl Tool for Hostname {
    fn name(&self) -> &str {
        "hostname"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &HostnameArgs::command(),
            "hostname",
            "Print the system hostname",
            [("Print hostname", "hostname")],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("hostname: {e}")),
        };
        let parsed = match HostnameArgs::try_parse_from(
            std::iter::once("hostname".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("hostname: {e}")),
        };
        parsed.global.apply(ctx);

        ExecResult::with_output(OutputData::text(read_hostname()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    #[tokio::test]
    async fn test_hostname_returns_nonempty() {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let result = Hostname.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(!result.text_out().is_empty(), "hostname should return a non-empty string");
        // Should not contain trailing newline
        assert!(!result.text_out().contains('\n'));
    }
}
