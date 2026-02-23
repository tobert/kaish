//! kaish-vars — List all variables in scope.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{ExecContext, Tool, ToolArgs, ToolSchema};

/// Vars tool: lists all variables in the current scope.
pub struct Vars;

#[async_trait]
impl Tool for Vars {
    fn name(&self) -> &str {
        "kaish-vars"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("kaish-vars", "List all variables in the current scope")
    }

    async fn execute(&self, _args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let vars = ctx.scope.all();
        format_table(&vars)
    }
}

/// Format variables as a structured table with NAME, VALUE, TYPE columns.
fn format_table(vars: &[(String, Value)]) -> ExecResult {
    if vars.is_empty() {
        return ExecResult::with_output(OutputData::new());
    }

    let headers = vec![
        "NAME".to_string(),
        "VALUE".to_string(),
        "TYPE".to_string(),
    ];

    let nodes: Vec<OutputNode> = vars
        .iter()
        .map(|(name, value)| {
            let value_str = format_value(value);
            let type_str = value_type_name(value);
            OutputNode::new(name).with_cells(vec![value_str, type_str.to_string()])
        })
        .collect();

    ExecResult::with_output(OutputData::table(headers, nodes))
}

/// Format a value for display.
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

/// Get the type name for a value.
fn value_type_name(value: &Value) -> &'static str {
    match value {
        Value::Null => "null",
        Value::Bool(_) => "bool",
        Value::Int(_) => "int",
        Value::Float(_) => "float",
        Value::String(_) => "string",
        Value::Json(_) => "json",
        Value::Blob(_) => "blob",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::apply_output_format;
    use crate::interpreter::OutputFormat;
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
        // Table output: NAME\tVALUE\tTYPE per row (canonical TSV)
        assert!(result.out.contains("X"));
        assert!(result.out.contains("42"));
        assert!(result.out.contains("NAME"));
    }

    #[tokio::test]
    async fn test_vars_json_via_global_flag() {
        let mut ctx = make_ctx();
        ctx.scope.set("COUNT", Value::Int(10));
        ctx.scope.set("FLAG", Value::Bool(true));

        let args = ToolArgs::new();
        let result = Vars.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Simulate global --json flag (handled by kernel)
        let result = apply_output_format(result, OutputFormat::Json);
        let data: Vec<serde_json::Value> = serde_json::from_str(&result.out).expect("should be valid JSON");
        assert_eq!(data.len(), 2);

        let names: Vec<&str> = data
            .iter()
            .filter_map(|v| v.get("NAME").and_then(|n| n.as_str()))
            .collect();
        assert!(names.contains(&"COUNT"));
        assert!(names.contains(&"FLAG"));
    }

    #[tokio::test]
    async fn test_vars_output_data_table() {
        let mut ctx = make_ctx();
        ctx.scope.set("X", Value::Int(42));

        let args = ToolArgs::new();
        let result = Vars.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Should have structured OutputData with table headers
        let output = result.output.as_ref().expect("should have OutputData");
        let headers = output.headers.as_ref().expect("should have headers");
        assert_eq!(headers, &["NAME", "VALUE", "TYPE"]);
        assert!(!output.root.is_empty());
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
        // Table canonical output includes name + value + type
        assert!(result.out.contains("NULL_VAL"));
        assert!(result.out.contains("null"));
        assert!(result.out.contains("BOOL_VAL"));
        assert!(result.out.contains("false"));
        assert!(result.out.contains("INT_VAL"));
        assert!(result.out.contains("-5"));
        assert!(result.out.contains("FLOAT_VAL"));
        assert!(result.out.contains("3.14"));
        assert!(result.out.contains("STR_VAL"));
    }
}
