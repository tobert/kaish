//! assert — Assert conditions for testing.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Assert tool: verify conditions in tests.
pub struct Assert;

#[async_trait]
impl Tool for Assert {
    fn name(&self) -> &str {
        "assert"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("assert", "Assert a condition is true (for testing)")
            .param(ParamSchema::required(
                "condition",
                "any",
                "Value to test for truthiness",
            ))
            .param(ParamSchema::optional(
                "message",
                "string",
                Value::String("assertion failed".into()),
                "Error message if assertion fails",
            ))
            .example("Assert a value is truthy", "assert $RESULT")
            .example("Assert with custom message", "assert $OK \"deploy failed\"")
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        let condition = match args.get_positional(0) {
            Some(v) => v,
            None => return ExecResult::failure(1, "assert: missing condition argument"),
        };

        let message = args
            .get_string("message", 1)
            .unwrap_or_else(|| "assertion failed".to_string());

        if is_truthy(condition) {
            ExecResult::with_output(OutputData::text(""))
        } else {
            ExecResult::failure(1, format!("assert: {}", message))
        }
    }
}

/// Check if a value is "truthy" (non-false, non-null, non-empty).
fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(i) => *i != 0,
        Value::Float(f) => *f != 0.0,
        Value::String(s) => !s.is_empty() && s != "false" && s != "0",
        Value::Json(json) => match json {
            serde_json::Value::Null => false,
            serde_json::Value::Array(arr) => !arr.is_empty(),
            serde_json::Value::Object(obj) => !obj.is_empty(),
            serde_json::Value::Bool(b) => *b,
            serde_json::Value::Number(n) => n.as_f64().map(|f| f != 0.0).unwrap_or(false),
            serde_json::Value::String(s) => !s.is_empty() && s != "false" && s != "0",
        },
        Value::Blob(_) => true, // Blob references are always truthy
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
    async fn test_assert_true() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Bool(true));

        let result = Assert.execute(args, &mut ctx).await;
        assert!(result.ok());
    }

    #[tokio::test]
    async fn test_assert_false() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Bool(false));

        let result = Assert.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("assertion failed"));
    }

    #[tokio::test]
    async fn test_assert_with_message() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Bool(false));
        args.positional.push(Value::String("custom error".into()));

        let result = Assert.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("custom error"));
    }

    #[tokio::test]
    async fn test_assert_int_truthy() {
        let mut ctx = make_ctx();

        // Non-zero is truthy
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(42));
        let result = Assert.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Zero is falsy
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(0));
        let result = Assert.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_assert_string_truthy() {
        let mut ctx = make_ctx();

        // Non-empty string is truthy
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        let result = Assert.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Empty string is falsy
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("".into()));
        let result = Assert.execute(args, &mut ctx).await;
        assert!(!result.ok());

        // "false" string is falsy
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("false".into()));
        let result = Assert.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_assert_null_falsy() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Null);

        let result = Assert.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_assert_json_string_truthy() {
        let mut ctx = make_ctx();

        // Non-empty JSON string is truthy
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("[1, 2]".into()));
        let result = Assert.execute(args, &mut ctx).await;
        assert!(result.ok());

        // "[]" is a non-empty string so it's truthy
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("[]".into()));
        let result = Assert.execute(args, &mut ctx).await;
        assert!(result.ok());
    }

    #[tokio::test]
    async fn test_assert_missing_condition() {
        let mut ctx = make_ctx();
        let result = Assert.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("missing condition"));
    }
}
