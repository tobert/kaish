//! printf — Format and print data.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};
use super::format_string::{self, FormatArg};

/// Printf tool: formatted output.
pub struct Printf;

/// clap-derived argv layer for printf.
#[derive(Parser, Debug)]
#[command(name = "printf", about = "Format and print data")]
struct PrintfArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Format string followed by arguments substituted into it.
    format_args: Vec<String>,
}

impl FormatArg for Value {
    fn as_format_string(&self) -> String {
        match self {
            Value::String(s) => s.clone(),
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Null => String::new(),
            Value::Json(json) => json.to_string(),
            // Non-UTF-8 bytes are rejected loud at printf's arg gate before we
            // get here; valid-UTF-8 bytes render as their text.
            Value::Bytes(b) => String::from_utf8_lossy(b).into_owned(),
        }
    }

    fn as_format_int(&self) -> i64 {
        match self {
            Value::Int(i) => *i,
            Value::Float(f) => *f as i64,
            Value::String(s) => s.parse().unwrap_or(0),
            Value::Bool(b) => i64::from(*b),
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
        schema_from_clap(
            &PrintfArgs::command(),
            "printf",
            "Format and print data",
            [
                ("Formatted output", "printf \"%s is %d\\n\" name 42"),
                ("Zero-padded number", "printf \"%08d\" 42"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("printf: {e}")),
        };
        let parsed = match PrintfArgs::try_parse_from(
            std::iter::once("printf".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("printf: {e}")),
        };
        parsed.global.apply(ctx);

        // The format is a text sink like the operands below: a binary
        // (non-UTF-8) format string goes loud rather than `get_string`'s
        // silent `None` (which would misreport it as "missing format
        // argument"). Read the value first so a `Value::Bytes` is caught
        // before it degrades to `None`.
        let format = match args.get("format", 0) {
            Some(v @ Value::Bytes(_)) => {
                match crate::interpreter::value_to_text_sink_named(v, "a printf format string") {
                    Ok(f) => f,
                    Err(e) => return ExecResult::failure(1, format!("printf: {e}")),
                }
            }
            _ => match args.get_string("format", 0) {
                Some(f) => f,
                None => return ExecResult::failure(1, "printf: missing format argument"),
            },
        };

        let format_args: Vec<&Value> = args.positional.iter().skip(1).collect();
        // printf is a text sink: a binary (non-UTF-8) operand is loud, never a
        // silent `[binary: N bytes]` placeholder (kept in sync with `echo` and
        // the interpolation/argv sinks). Valid-UTF-8 bytes coerce below.
        for &v in &format_args {
            if let Err(e) = crate::interpreter::value_to_text_sink(v) {
                return ExecResult::failure(1, format!("printf: {e}"));
            }
        }
        // POSIX printf reuses the format until all operands are consumed.
        let output = format_string::format_string_cycling(&format, &format_args);

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
#[allow(clippy::approx_constant)]
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
        assert_eq!(&*result.text_out(), "Hello, world!");
    }

    #[tokio::test]
    async fn test_printf_integer() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("Number: %d".into()));
        args.positional.push(Value::Int(42));

        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "Number: 42");
    }

    #[tokio::test]
    async fn test_printf_float() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("Pi: %f".into()));
        args.positional.push(Value::Float(3.14159));

        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().starts_with("Pi: 3.14159"));
    }

    #[tokio::test]
    async fn test_printf_hex() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("Hex: %x".into()));
        args.positional.push(Value::Int(255));

        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "Hex: ff");
    }

    #[tokio::test]
    async fn test_printf_escape_sequences() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("line1\\nline2".into()));

        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "line1\nline2");
    }

    #[tokio::test]
    async fn test_printf_percent_escape() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("100%%".into()));

        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "100%");
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
        assert_eq!(&*result.text_out(), "Alice is 30 years old");
    }

    #[tokio::test]
    async fn test_printf_cycles_format_over_extra_args() {
        // POSIX: `printf '%s\n' a b c` → "a\nb\nc\n"
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("%s\\n".into()));
        args.positional.push(Value::String("a".into()));
        args.positional.push(Value::String("b".into()));
        args.positional.push(Value::String("c".into()));
        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "a\nb\nc\n");
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
        assert_eq!(&*result.text_out(), "Name            |\n");
    }

    #[tokio::test]
    async fn test_printf_zero_pad_int() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("%08d\\n".into()));
        args.positional.push(Value::String("42".into()));
        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "00000042\n");
    }

    #[tokio::test]
    async fn test_printf_precision_float() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("%.2f\\n".into()));
        args.positional.push(Value::String("3.14159".into()));
        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "3.14\n");
    }

    #[tokio::test]
    async fn test_printf_right_align_width() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("%10s|\\n".into()));
        args.positional.push(Value::String("hello".into()));
        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "     hello|\n");
    }

    #[tokio::test]
    async fn test_printf_width_int() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("%6d\\n".into()));
        args.positional.push(Value::Int(42));
        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "    42\n");
    }

    #[tokio::test]
    async fn test_printf_hex_width() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("%08x\\n".into()));
        args.positional.push(Value::Int(255));
        let result = Printf.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "000000ff\n");
    }
}
