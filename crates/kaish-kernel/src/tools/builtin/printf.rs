//! printf — Format and print data.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Printf tool: formatted output.
pub struct Printf;

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
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        let format = match args.get_string("format", 0) {
            Some(f) => f,
            None => return ExecResult::failure(1, "printf: missing format argument"),
        };

        // Collect remaining positional args
        let format_args: Vec<&Value> = args.positional.iter().skip(1).collect();
        let mut arg_index = 0;

        let mut output = String::new();
        let mut chars = format.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '%' {
                match chars.next() {
                    Some('%') => output.push('%'),
                    Some('s') => {
                        let val = format_args.get(arg_index).map(|v| value_to_string(v));
                        output.push_str(&val.unwrap_or_default());
                        arg_index += 1;
                    }
                    Some('d') | Some('i') => {
                        let val = format_args.get(arg_index).map(|v| value_to_int(v));
                        output.push_str(&val.unwrap_or(0).to_string());
                        arg_index += 1;
                    }
                    Some('f') => {
                        let val = format_args.get(arg_index).map(|v| value_to_float(v));
                        output.push_str(&format!("{:.6}", val.unwrap_or(0.0)));
                        arg_index += 1;
                    }
                    Some('x') => {
                        let val = format_args.get(arg_index).map(|v| value_to_int(v));
                        output.push_str(&format!("{:x}", val.unwrap_or(0)));
                        arg_index += 1;
                    }
                    Some('X') => {
                        let val = format_args.get(arg_index).map(|v| value_to_int(v));
                        output.push_str(&format!("{:X}", val.unwrap_or(0)));
                        arg_index += 1;
                    }
                    Some('o') => {
                        let val = format_args.get(arg_index).map(|v| value_to_int(v));
                        output.push_str(&format!("{:o}", val.unwrap_or(0)));
                        arg_index += 1;
                    }
                    Some('c') => {
                        let val = format_args.get(arg_index).and_then(|v| match v {
                            Value::String(s) => s.chars().next(),
                            Value::Int(i) => char::from_u32(*i as u32),
                            _ => None,
                        });
                        if let Some(ch) = val {
                            output.push(ch);
                        }
                        arg_index += 1;
                    }
                    Some(ch) => {
                        output.push('%');
                        output.push(ch);
                    }
                    None => output.push('%'),
                }
            } else if c == '\\' {
                match chars.next() {
                    Some('n') => output.push('\n'),
                    Some('t') => output.push('\t'),
                    Some('r') => output.push('\r'),
                    Some('\\') => output.push('\\'),
                    Some('0') => output.push('\0'),
                    Some(ch) => {
                        output.push('\\');
                        output.push(ch);
                    }
                    None => output.push('\\'),
                }
            } else {
                output.push(c);
            }
        }

        ExecResult::success(output)
    }
}

fn value_to_string(v: &Value) -> String {
    match v {
        Value::String(s) => s.clone(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Null => String::new(),
    }
}

fn value_to_int(v: &Value) -> i64 {
    match v {
        Value::Int(i) => *i,
        Value::Float(f) => *f as i64,
        Value::String(s) => s.parse().unwrap_or(0),
        Value::Bool(b) => {
            if *b {
                1
            } else {
                0
            }
        }
        _ => 0,
    }
}

fn value_to_float(v: &Value) -> f64 {
    match v {
        Value::Float(f) => *f,
        Value::Int(i) => *i as f64,
        Value::String(s) => s.parse().unwrap_or(0.0),
        _ => 0.0,
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
}
