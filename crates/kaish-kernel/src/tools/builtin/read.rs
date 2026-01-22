//! read — Read a line from standard input into variables.
//!
//! Implements the shell `read` builtin for reading input.
//!
//! Usage:
//!   read VAR                    # Read line into VAR
//!   read -r VAR                 # Raw mode (no backslash processing)
//!   read -p "prompt: " VAR      # Show prompt before reading
//!   read VAR1 VAR2 VAR3         # Split line into multiple variables

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Read tool: reads a line from stdin into variable(s).
pub struct Read;

#[async_trait]
impl Tool for Read {
    fn name(&self) -> &str {
        "read"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("read", "Read a line from standard input into variables")
            .param(ParamSchema::optional(
                "raw",
                "bool",
                Value::Bool(false),
                "Raw mode - do not process backslash escapes (-r)",
            ))
            .param(ParamSchema::optional(
                "prompt",
                "string",
                Value::Null,
                "Prompt to display before reading (-p)",
            ))
            .param(ParamSchema::optional(
                "vars",
                "string",
                Value::Null,
                "Variable names to store input (space-separated if multiple)",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let raw_mode = args.has_flag("r") || args.has_flag("raw");
        let _prompt = args.named.get("prompt").or_else(|| args.named.get("p"));

        // Get variable names from positional arguments
        let var_names: Vec<String> = args
            .positional
            .iter()
            .filter_map(|v| match v {
                Value::String(s) => Some(s.clone()),
                _ => None,
            })
            .collect();

        if var_names.is_empty() {
            return ExecResult::failure(1, "read: missing variable name");
        }

        // Get input from stdin
        let input = match ctx.take_stdin() {
            Some(s) => s,
            None => {
                // No stdin provided - return failure (no data to read)
                return ExecResult::failure(1, "read: no input available");
            }
        };

        // Process the input
        let line = input.lines().next().unwrap_or("");

        // Process backslash escapes unless raw mode
        let processed = if raw_mode {
            line.to_string()
        } else {
            process_escapes(line)
        };

        if var_names.len() == 1 {
            // Single variable: assign entire line
            ctx.scope.set(&var_names[0], Value::String(processed));
        } else {
            // Multiple variables: split on whitespace
            let words: Vec<&str> = processed.split_whitespace().collect();

            for (i, var) in var_names.iter().enumerate() {
                if i < var_names.len() - 1 {
                    // Assign individual words to all but last variable
                    let word = words.get(i).copied().unwrap_or("");
                    ctx.scope.set(var, Value::String(word.to_string()));
                } else {
                    // Last variable gets the rest of the line
                    let rest = if i < words.len() {
                        words[i..].join(" ")
                    } else {
                        String::new()
                    };
                    ctx.scope.set(var, Value::String(rest));
                }
            }
        }

        ExecResult::success("")
    }
}

/// Process backslash escape sequences.
fn process_escapes(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('\\') => result.push('\\'),
                Some(other) => {
                    // Unknown escape - keep both characters
                    result.push('\\');
                    result.push(other);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }

    result
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
    async fn test_read_single_var() {
        let mut ctx = make_ctx();
        ctx.set_stdin("hello world".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("NAME".to_string()));

        let result = Read.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(ctx.scope.get("NAME"), Some(&Value::String("hello world".into())));
    }

    #[tokio::test]
    async fn test_read_multiple_vars() {
        let mut ctx = make_ctx();
        ctx.set_stdin("one two three four".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("A".to_string()));
        args.positional.push(Value::String("B".to_string()));
        args.positional.push(Value::String("C".to_string()));

        let result = Read.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(ctx.scope.get("A"), Some(&Value::String("one".into())));
        assert_eq!(ctx.scope.get("B"), Some(&Value::String("two".into())));
        // C gets "three four" (the rest)
        assert_eq!(ctx.scope.get("C"), Some(&Value::String("three four".into())));
    }

    #[tokio::test]
    async fn test_read_escape_processing() {
        let mut ctx = make_ctx();
        ctx.set_stdin("hello\\nworld".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("NAME".to_string()));

        let result = Read.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Without -r, \n becomes newline
        assert_eq!(ctx.scope.get("NAME"), Some(&Value::String("hello\nworld".into())));
    }

    #[tokio::test]
    async fn test_read_raw_mode() {
        let mut ctx = make_ctx();
        ctx.set_stdin("hello\\nworld".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("NAME".to_string()));
        args.flags.insert("r".to_string());

        let result = Read.execute(args, &mut ctx).await;
        assert!(result.ok());
        // With -r, \n stays as literal
        assert_eq!(ctx.scope.get("NAME"), Some(&Value::String("hello\\nworld".into())));
    }

    #[tokio::test]
    async fn test_read_no_input() {
        let mut ctx = make_ctx();
        // No stdin set

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("NAME".to_string()));

        let result = Read.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("no input"));
    }

    #[tokio::test]
    async fn test_read_no_var_name() {
        let mut ctx = make_ctx();
        ctx.set_stdin("hello".to_string());

        let args = ToolArgs::new();

        let result = Read.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("missing variable name"));
    }

    #[tokio::test]
    async fn test_read_empty_input() {
        let mut ctx = make_ctx();
        ctx.set_stdin("".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("NAME".to_string()));

        let result = Read.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(ctx.scope.get("NAME"), Some(&Value::String("".into())));
    }

    #[tokio::test]
    async fn test_read_multiline_takes_first() {
        let mut ctx = make_ctx();
        ctx.set_stdin("first line\nsecond line".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("NAME".to_string()));

        let result = Read.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Only first line is read
        assert_eq!(ctx.scope.get("NAME"), Some(&Value::String("first line".into())));
    }

    #[test]
    fn test_process_escapes() {
        assert_eq!(process_escapes("hello"), "hello");
        assert_eq!(process_escapes("hello\\nworld"), "hello\nworld");
        assert_eq!(process_escapes("tab\\there"), "tab\there");
        assert_eq!(process_escapes("back\\\\slash"), "back\\slash");
        assert_eq!(process_escapes("end\\"), "end\\");
        assert_eq!(process_escapes("unknown\\x"), "unknown\\x");
    }
}
