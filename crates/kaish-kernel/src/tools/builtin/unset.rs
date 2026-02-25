//! unset — Remove variables from scope.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Unset tool: removes variables from the current scope.
pub struct Unset;

#[async_trait]
impl Tool for Unset {
    fn name(&self) -> &str {
        "unset"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("unset", "Remove variables from scope")
            .param(ParamSchema::required(
                "names",
                "array",
                "Variable names to unset",
            ))
            .example("Remove a variable", "unset MY_VAR")
            .example("Remove multiple", "unset A B C")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        if args.positional.is_empty() {
            return ExecResult::failure(1, "unset: missing variable name");
        }

        let mut unset_count = 0;
        for arg in &args.positional {
            let name = match arg {
                Value::String(s) => s.clone(),
                other => value_to_string(other),
            };

            if ctx.scope.remove(&name).is_some() {
                unset_count += 1;
            }
        }

        ExecResult::with_output(OutputData::text(format!("{}", unset_count)))
    }
}

fn value_to_string(value: &Value) -> String {
    match value {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => s.clone(),
        Value::Json(json) => json.to_string(),
        Value::Blob(blob) => format!("[blob: {} {}]", blob.formatted_size(), blob.content_type),
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
    async fn test_unset_existing_variable() {
        let mut ctx = make_ctx();
        ctx.scope.set("X", Value::Int(42));
        assert!(ctx.scope.get("X").is_some());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("X".into()));

        let result = Unset.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "1");
        assert!(ctx.scope.get("X").is_none());
    }

    #[tokio::test]
    async fn test_unset_nonexistent_variable() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("MISSING".into()));

        let result = Unset.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "0");
    }

    #[tokio::test]
    async fn test_unset_multiple_variables() {
        let mut ctx = make_ctx();
        ctx.scope.set("A", Value::Int(1));
        ctx.scope.set("B", Value::Int(2));
        ctx.scope.set("C", Value::Int(3));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("A".into()));
        args.positional.push(Value::String("B".into()));
        args.positional.push(Value::String("D".into())); // doesn't exist

        let result = Unset.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "2");
        assert!(ctx.scope.get("A").is_none());
        assert!(ctx.scope.get("B").is_none());
        assert!(ctx.scope.get("C").is_some());
    }

    #[tokio::test]
    async fn test_unset_no_args() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Unset.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }
}
