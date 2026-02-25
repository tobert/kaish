//! true/false — Boolean exit code builtins.
//!
//! These builtins provide shell-compatible boolean semantics:
//! - `true` exits with code 0 (success/truthy)
//! - `false` exits with code 1 (failure/falsy)
//!
//! Used in conditions like `if true; then` or `while false; do`.

use async_trait::async_trait;

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, Tool, ToolArgs, ToolSchema};

/// True builtin: always succeeds (exit code 0).
pub struct True;

#[async_trait]
impl Tool for True {
    fn name(&self) -> &str {
        "true"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("true", "Exit with success (code 0)")
            .example("Always succeeds", "true")
    }

    async fn execute(&self, _args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        ExecResult::with_output(OutputData::text(""))
    }
}

/// False builtin: always fails (exit code 1).
pub struct False;

#[async_trait]
impl Tool for False {
    fn name(&self) -> &str {
        "false"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("false", "Exit with failure (code 1)")
            .example("Always fails", "false")
    }

    async fn execute(&self, _args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        ExecResult::failure(1, "")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    #[tokio::test]
    async fn true_returns_success() {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let result = True.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.code, 0);
    }

    #[tokio::test]
    async fn true_with_args_still_succeeds() {
        // Like real shells, true ignores any arguments
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("ignored".into()));

        let result = True.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.code, 0);
    }

    #[tokio::test]
    async fn false_returns_failure() {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let result = False.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
        assert_eq!(result.code, 1);
    }

    #[tokio::test]
    async fn false_with_args_still_fails() {
        // Like real shells, false ignores any arguments
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("ignored".into()));

        let result = False.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert_eq!(result.code, 1);
    }
}
