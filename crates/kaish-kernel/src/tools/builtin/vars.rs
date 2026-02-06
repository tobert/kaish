//! vars — List all variables in scope.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::{value_to_json, ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Vars tool: lists all variables in the current scope.
pub struct Vars;

#[async_trait]
impl Tool for Vars {
    fn name(&self) -> &str {
        "vars"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("vars", "List all variables in the current scope")
            .param(ParamSchema::optional(
                "json",
                "bool",
                Value::Bool(false),
                "Output as JSON array of {name, value} objects",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let json_output = args.has_flag("json");
        let vars = ctx.scope.all();

        if json_output {
            format_json(&vars)
        } else {
            format_plain(&vars)
        }
    }
}

/// Format variables as NAME=value lines.
fn format_plain(vars: &[(String, Value)]) -> ExecResult {
    let mut output = String::new();

    for (name, value) in vars {
        let value_str = format_value(value);
        output.push_str(&format!("{}={}\n", name, value_str));
    }

    ExecResult::with_output(OutputData::text(output))
}

/// Format a value for plain text output.
fn format_value(value: &Value) -> String {
    match value {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => format!("\"{}\"", s.replace('\"', "\\\"")),
        Value::Json(json) => json.to_string(),
        Value::Blob(blob) => format!("[blob: {} {}]", blob.formatted_size(), blob.content_type),
    }
}

/// Format variables as JSON array.
fn format_json(vars: &[(String, Value)]) -> ExecResult {
    let json_vars: Vec<serde_json::Value> = vars
        .iter()
        .map(|(name, value)| {
            serde_json::json!({
                "name": name,
                "value": value_to_json(value)
            })
        })
        .collect();

    match serde_json::to_string_pretty(&json_vars) {
        Ok(json_str) => ExecResult::with_output(OutputData::text(json_str)),
        Err(e) => ExecResult::failure(1, format!("failed to serialize variables: {}", e)),
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
    async fn test_vars_empty() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Vars.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.is_empty());
    }

    #[tokio::test]
    async fn test_vars_default_format() {
        let mut ctx = make_ctx();
        ctx.scope.set("X", Value::Int(42));
        ctx.scope.set("NAME", Value::String("test".into()));

        let args = ToolArgs::new();
        let result = Vars.execute(args, &mut ctx).await;

        assert!(result.ok());
        assert!(result.out.contains("X=42"));
        assert!(result.out.contains("NAME=\"test\""));
    }

    #[tokio::test]
    async fn test_vars_json_format() {
        let mut ctx = make_ctx();
        ctx.scope.set("COUNT", Value::Int(10));
        ctx.scope.set("FLAG", Value::Bool(true));

        let mut args = ToolArgs::new();
        args.flags.insert("json".to_string());

        let result = Vars.execute(args, &mut ctx).await;
        assert!(result.ok());

        let data: Vec<serde_json::Value> = serde_json::from_str(&result.out).expect("should be valid JSON");
        assert_eq!(data.len(), 2);

        let names: Vec<&str> = data
            .iter()
            .filter_map(|v| v.get("name").and_then(|n| n.as_str()))
            .collect();
        assert!(names.contains(&"COUNT"));
        assert!(names.contains(&"FLAG"));
    }

    #[tokio::test]
    async fn test_vars_json_string_values() {
        let mut ctx = make_ctx();
        // JSON objects/arrays are now stored as strings
        ctx.scope.set(
            "OBJ",
            Value::String(r#"{"key": "val"}"#.into()),
        );
        ctx.scope.set(
            "ARR",
            Value::String(r#"[1, 2]"#.into()),
        );

        let args = ToolArgs::new();
        let result = Vars.execute(args, &mut ctx).await;

        assert!(result.ok());
        assert!(result.out.contains("OBJ="));
        assert!(result.out.contains("ARR="));
    }

    #[tokio::test]
    async fn test_vars_with_special_chars() {
        let mut ctx = make_ctx();
        ctx.scope.set("MSG", Value::String("hello \"world\"".into()));

        let args = ToolArgs::new();
        let result = Vars.execute(args, &mut ctx).await;

        assert!(result.ok());
        assert!(result.out.contains("MSG="));
        assert!(result.out.contains("\\\"world\\\""));
    }

    #[tokio::test]
    async fn test_vars_all_types() {
        let mut ctx = make_ctx();
        ctx.scope.set("NULL_VAL", Value::Null);
        ctx.scope.set("BOOL_VAL", Value::Bool(false));
        ctx.scope.set("INT_VAL", Value::Int(-5));
        ctx.scope.set("FLOAT_VAL", Value::Float(3.14));
        ctx.scope.set("STR_VAL", Value::String("hello".into()));

        let args = ToolArgs::new();
        let result = Vars.execute(args, &mut ctx).await;

        assert!(result.ok());
        assert!(result.out.contains("NULL_VAL=null"));
        assert!(result.out.contains("BOOL_VAL=false"));
        assert!(result.out.contains("INT_VAL=-5"));
        assert!(result.out.contains("FLOAT_VAL=3.14"));
        assert!(result.out.contains("STR_VAL=\"hello\""));
    }
}
