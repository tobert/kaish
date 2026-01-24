//! seq — Print sequences of numbers.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Seq tool: print a sequence of numbers.
pub struct Seq;

#[async_trait]
impl Tool for Seq {
    fn name(&self) -> &str {
        "seq"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("seq", "Print sequences of numbers")
            .param(ParamSchema::required(
                "last",
                "number",
                "Last number (or first if two args)",
            ))
            .param(ParamSchema::optional(
                "first",
                "number",
                Value::Int(1),
                "First number (default 1)",
            ))
            .param(ParamSchema::optional(
                "increment",
                "number",
                Value::Int(1),
                "Increment (default 1)",
            ))
            .param(ParamSchema::optional(
                "separator",
                "string",
                Value::String("\n".into()),
                "Separator between numbers (-s)",
            ))
            .param(ParamSchema::optional(
                "width",
                "bool",
                Value::Bool(false),
                "Equalize width by padding with zeros (-w)",
            ))
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        // Parse the arguments - seq has unusual positional arg handling:
        // seq LAST           -> 1 to LAST
        // seq FIRST LAST     -> FIRST to LAST
        // seq FIRST INC LAST -> FIRST to LAST by INC

        let (first, increment, last) = match (
            args.get_positional(0),
            args.get_positional(1),
            args.get_positional(2),
        ) {
            (Some(v1), None, None) => {
                let last = value_to_f64(v1);
                (1.0, 1.0, last)
            }
            (Some(v1), Some(v2), None) => {
                let first = value_to_f64(v1);
                let last = value_to_f64(v2);
                (first, 1.0, last)
            }
            (Some(v1), Some(v2), Some(v3)) => {
                let first = value_to_f64(v1);
                let increment = value_to_f64(v2);
                let last = value_to_f64(v3);
                (first, increment, last)
            }
            _ => return ExecResult::failure(1, "seq: missing argument"),
        };

        if increment == 0.0 {
            return ExecResult::failure(1, "seq: increment cannot be zero");
        }

        let separator = args
            .get_string("separator", usize::MAX)
            .or_else(|| args.get_string("s", usize::MAX))
            .unwrap_or_else(|| "\n".to_string());

        let pad_width = args.has_flag("width") || args.has_flag("w");

        // Generate sequence
        let mut numbers = Vec::new();
        let mut current = first;

        if increment > 0.0 {
            while current <= last + f64::EPSILON {
                numbers.push(current);
                current += increment;
            }
        } else {
            while current >= last - f64::EPSILON {
                numbers.push(current);
                current += increment;
            }
        }

        if numbers.is_empty() {
            return ExecResult::success("");
        }

        // Format output
        let is_integer = numbers.iter().all(|n| n.fract().abs() < f64::EPSILON);

        let formatted: Vec<String> = if is_integer {
            let max_width = if pad_width {
                numbers
                    .iter()
                    .map(|n| (*n as i64).abs().to_string().len())
                    .max()
                    .unwrap_or(1)
            } else {
                0
            };

            numbers
                .iter()
                .map(|n| {
                    let i = *n as i64;
                    if pad_width {
                        format!("{:0>width$}", i, width = max_width)
                    } else {
                        i.to_string()
                    }
                })
                .collect()
        } else {
            numbers.iter().map(|n| format!("{}", n)).collect()
        };

        let mut output = formatted.join(&separator);
        output.push('\n');

        ExecResult::success(output)
    }
}

fn value_to_f64(v: &Value) -> f64 {
    match v {
        Value::Int(i) => *i as f64,
        Value::Float(f) => *f,
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
    async fn test_seq_single_arg() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(5));

        let result = Seq.execute(args, &mut ctx).await;
        assert!(result.ok());
        let lines: Vec<&str> = result.out.lines().collect();
        assert_eq!(lines, vec!["1", "2", "3", "4", "5"]);
    }

    #[tokio::test]
    async fn test_seq_first_last() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(3));
        args.positional.push(Value::Int(7));

        let result = Seq.execute(args, &mut ctx).await;
        assert!(result.ok());
        let lines: Vec<&str> = result.out.lines().collect();
        assert_eq!(lines, vec!["3", "4", "5", "6", "7"]);
    }

    #[tokio::test]
    async fn test_seq_with_increment() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(1));
        args.positional.push(Value::Int(2));
        args.positional.push(Value::Int(10));

        let result = Seq.execute(args, &mut ctx).await;
        assert!(result.ok());
        let lines: Vec<&str> = result.out.lines().collect();
        assert_eq!(lines, vec!["1", "3", "5", "7", "9"]);
    }

    #[tokio::test]
    async fn test_seq_negative_increment() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(5));
        args.positional.push(Value::Int(-1));
        args.positional.push(Value::Int(1));

        let result = Seq.execute(args, &mut ctx).await;
        assert!(result.ok());
        let lines: Vec<&str> = result.out.lines().collect();
        assert_eq!(lines, vec!["5", "4", "3", "2", "1"]);
    }

    #[tokio::test]
    async fn test_seq_custom_separator() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(3));
        args.named
            .insert("separator".to_string(), Value::String(", ".into()));

        let result = Seq.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "1, 2, 3");
    }

    #[tokio::test]
    async fn test_seq_zero_pad() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(10));
        args.flags.insert("w".to_string());

        let result = Seq.execute(args, &mut ctx).await;
        assert!(result.ok());
        let lines: Vec<&str> = result.out.lines().collect();
        assert_eq!(lines[0], "01");
        assert_eq!(lines[9], "10");
    }

    #[tokio::test]
    async fn test_seq_float() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Float(1.0));
        args.positional.push(Value::Float(0.5));
        args.positional.push(Value::Float(2.0));

        let result = Seq.execute(args, &mut ctx).await;
        assert!(result.ok());
        let lines: Vec<&str> = result.out.lines().collect();
        assert_eq!(lines.len(), 3); // 1.0, 1.5, 2.0
    }

    #[tokio::test]
    async fn test_seq_zero_increment() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(1));
        args.positional.push(Value::Int(0));
        args.positional.push(Value::Int(5));

        let result = Seq.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("zero"));
    }
}
