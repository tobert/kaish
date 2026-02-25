//! printf — Format and print data.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};
use super::format_string::{self, FormatArg};

/// Printf tool: formatted output.
pub struct Printf;

impl FormatArg for Value {
    fn as_format_string(&self) -> String {
        match self {
            Value::String(s) => s.clone(),
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Null => String::new(),
            Value::Json(json) => json.to_string(),
            Value::Blob(blob) => format!("[blob: {} {}]", blob.formatted_size(), blob.content_type),
        }
    }

    fn as_format_int(&self) -> i64 {
        match self {
            Value::Int(i) => *i,
            Value::Float(f) => *f as i64,
            Value::String(s) => s.parse().unwrap_or(0),
            Value::Bool(b) => if *b { 1 } else { 0 },
            _ => 0,
        }
    }

    fn as_format_float(&self) -> f64 {
        match self {
            Value::Float(f) => *f,
            Value::Int(i) => *i as f64,
            Value::String(s) => s.parse().unwrap_or(0.0),
            _ => 0.0,
        }
    }

    fn as_format_char(&self) -> Option<char> {
        match self {
            Value::String(s) => s.chars().next(),
            Value::Int(i) => char::from_u32(*i as u32),
            _ => None,
        }
    }
}

#[async_trait]
impl Tool for Printf {
    fn name(&self) -> &str {
        "printf"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("printf", "Format and print data")
            .param(ParamSchema::required(
                "format",
                "string",
                "Format string (supports %s, %d, %f, %x, %%)",
            ))
            .param(ParamSchema::optional(
                "args",
                "any",
                Value::Null,
                "Arguments for format string",
            ))
            .example("Formatted output", "printf \"%s is %d\\n\" name 42")
            .example("Zero-padded number", "printf \"%08d\" 42")
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        let format = match args.get_string("format", 0) {
            Some(f) => f,
            None => return ExecResult::failure(1, "printf: missing format argument"),
        };

        let format_args: Vec<&Value> = args.positional.iter().skip(1).collect();
        let output = format_string::format_string(&format, &format_args);

        ExecResult::with_output(OutputData::text(output))
    }
}

/// FormatArg impl for references (used by printf which collects &Value)
impl FormatArg for &Value {
    fn as_format_string(&self) -> String { (*self).as_format_string() }
    fn as_format_int(&self) -> i64 { (*self).as_format_int() }
    fn as_format_float(&self) -> f64 { (*self).as_format_float() }
    fn as_format_char(&self) -> Option<char> { (*self).as_format_char() }
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
    async fn test_printf_string() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("Hello, %s!".into()));
        args.positional.push(Value::String("world".into()));

        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "Hello, world!");
    }

    #[tokio::test]
    async fn test_printf_integer() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("Number: %d".into()));
        args.positional.push(Value::Int(42));

        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "Number: 42");
    }

    #[tokio::test]
    async fn test_printf_float() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("Pi: %f".into()));
        args.positional.push(Value::Float(3.14159));

        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.starts_with("Pi: 3.14159"));
    }

    #[tokio::test]
    async fn test_printf_hex() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("Hex: %x".into()));
        args.positional.push(Value::Int(255));

        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "Hex: ff");
    }

    #[tokio::test]
    async fn test_printf_escape_sequences() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("line1\\nline2".into()));

        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "line1\nline2");
    }

    #[tokio::test]
    async fn test_printf_percent_escape() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("100%%".into()));

        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "100%");
    }

    #[tokio::test]
    async fn test_printf_multiple_args() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("%s is %d years old".into()));
        args.positional.push(Value::String("Alice".into()));
        args.positional.push(Value::Int(30));

        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "Alice is 30 years old");
    }

    #[tokio::test]
    async fn test_printf_missing_format() {
        let mut ctx = make_ctx();
        let result = Printf.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_printf_left_align_width() {
        // Bug 6: "%-16s" should left-align to 16 chars
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("%-16s|\\n".into()));
        args.positional.push(Value::String("Name".into()));
        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "Name            |\n");
    }

    #[tokio::test]
    async fn test_printf_zero_pad_int() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("%08d\\n".into()));
        args.positional.push(Value::String("42".into()));
        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "00000042\n");
    }

    #[tokio::test]
    async fn test_printf_precision_float() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("%.2f\\n".into()));
        args.positional.push(Value::String("3.14159".into()));
        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "3.14\n");
    }

    #[tokio::test]
    async fn test_printf_right_align_width() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("%10s|\\n".into()));
        args.positional.push(Value::String("hello".into()));
        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "     hello|\n");
    }

    #[tokio::test]
    async fn test_printf_width_int() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("%6d\\n".into()));
        args.positional.push(Value::Int(42));
        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "    42\n");
    }

    #[tokio::test]
    async fn test_printf_hex_width() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("%08x\\n".into()));
        args.positional.push(Value::Int(255));
        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "000000ff\n");
    }
}
