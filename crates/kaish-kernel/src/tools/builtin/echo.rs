//! echo — Print arguments to stdout.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, Tool, ToolArgs, ToolSchema, ParamSchema};

/// Echo tool: prints arguments to stdout.
pub struct Echo;

#[async_trait]
impl Tool for Echo {
    fn name(&self) -> &str {
        "echo"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("echo", "Print arguments to standard output")
            .param(ParamSchema::optional(
                "args",
                "any",
                Value::Null,
                "Values to print",
            ))
            .param(ParamSchema::optional(
                "no_newline",
                "bool",
                Value::Bool(false),
                "Do not output trailing newline (-n)",
            ))
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        let no_newline = args.has_flag("no_newline") || args.has_flag("n");

        let parts: Vec<String> = args
            .positional
            .iter()
            .map(|v| value_to_string(v))
            .collect();

        let mut output = parts.join(" ");

        // By default, echo adds a trailing newline (like Unix echo)
        // Use -n to suppress it
        if !no_newline && !output.is_empty() {
            output.push('\n');
        }

        ExecResult::success(output)
    }
}

/// Convert a value to its string representation for echo.
fn value_to_string(value: &Value) -> String {
    match value {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => s.clone(),
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
    async fn test_echo_simple() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));

        let result = Echo.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hello\n");
    }

    #[tokio::test]
    async fn test_echo_multiple() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.positional.push(Value::String("world".into()));

        let result = Echo.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hello world\n");
    }

    #[tokio::test]
    async fn test_echo_types() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(42));
        args.positional.push(Value::Bool(true));
        args.positional.push(Value::Float(3.14));

        let result = Echo.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "42 true 3.14\n");
    }

    #[tokio::test]
    async fn test_echo_empty() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Echo.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, ""); // Empty output, no newline added
    }

    #[tokio::test]
    async fn test_echo_n_no_newline() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.flags.insert("n".to_string());

        let result = Echo.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hello"); // No trailing newline
    }

    #[tokio::test]
    async fn test_echo_no_newline_flag() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("test".into()));
        args.flags.insert("no_newline".to_string());

        let result = Echo.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "test");
    }
}
