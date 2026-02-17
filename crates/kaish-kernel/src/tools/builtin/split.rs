//! split — Split a string into an array.
//!
//! Provides explicit string splitting for iteration in for loops.
//! This is the replacement for implicit word splitting that traditional
//! shells perform on unquoted variables.
//!
//! # Examples
//!
//! ```kaish
//! # Default: split on whitespace
//! for word in $(split "hello world"); do
//!     echo $word
//! done
//!
//! # Split on delimiter
//! for part in $(split "a:b:c" ":"); do
//!     echo $part
//! done
//!
//! # Split on regex
//! for part in $(split "a1b2c3" -r "[0-9]"); do
//!     echo $part
//! done
//!
//! # Limit splits
//! for part in $(split "a:b:c:d" ":" --limit=2); do
//!     echo $part  # outputs: a, b:c:d
//! done
//! ```

use async_trait::async_trait;
use regex::Regex;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Split tool: split a string into an array.
pub struct Split;

#[async_trait]
impl Tool for Split {
    fn name(&self) -> &str {
        "split"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("split", "Split a string into an array")
            .param(ParamSchema::optional(
                "string",
                "string",
                Value::Null,
                "String to split (reads from stdin if omitted)",
            ))
            .param(ParamSchema::optional(
                "delimiter",
                "string",
                Value::Null,
                "Literal delimiter (default: whitespace)",
            ))
            .param(ParamSchema::optional(
                "regex",
                "string",
                Value::Null,
                "Regex pattern to split on (-r)",
            ).with_aliases(["-r"]))
            .param(ParamSchema::optional(
                "limit",
                "int",
                Value::Int(0),
                "Maximum number of splits (0 = unlimited)",
            ))
            .example("Split on whitespace", "split \"a b c\"")
            .example("Split on delimiter", "split \"a:b:c\" \":\"")
            .example("Split on regex", "split \"a1b2c3\" -r \"[0-9]\"")
            .example("Limit splits", "split \"a:b:c:d\" \":\" --limit=2")
            .example("Split stdin", "echo \"a,b,c\" | split \",\"")
            .example("Split stdin on whitespace", "echo \"a b c\" | split")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Determine input string and delimiter position.
        // When stdin is available AND there's no explicit string= named arg,
        // read input from stdin and shift positionals so pos[0] = delimiter.
        let has_named_string = args.named.contains_key("string");
        let stdin = ctx.read_stdin_to_string().await;
        let (input, delim_idx) = if let Some(s) = stdin.filter(|s| !s.is_empty() && !has_named_string) {
            // Stdin mode: input from pipe, positional[0] is the delimiter
            (s.trim_end_matches('\n').to_string(), 0usize)
        } else if let Some(s) = args.get_string("string", 0) {
            // Positional/named mode: first arg is the string
            (s, 1usize)
        } else {
            return ExecResult::failure(1, "split: no input (provide string argument or pipe stdin)");
        };

        // Get optional parameters — delimiter position shifts when using stdin
        let delimiter = args.get_string("delimiter", delim_idx);
        let regex_pat = args.get_string("regex", usize::MAX)
            .or_else(|| args.get_string("r", usize::MAX));
        let limit = args.get("limit", usize::MAX)
            .and_then(|v| match v {
                Value::Int(i) => Some(*i as usize),
                Value::String(s) => s.parse().ok(),
                _ => None,
            })
            .unwrap_or(0);

        // Perform the split
        let parts: Vec<&str> = if let Some(pattern) = regex_pat {
            // Regex split
            let re = match Regex::new(&pattern) {
                Ok(r) => r,
                Err(e) => return ExecResult::failure(1, format!("split: invalid regex: {}", e)),
            };
            if limit > 0 {
                re.splitn(&input, limit + 1).collect()
            } else {
                re.split(&input).collect()
            }
        } else if let Some(d) = delimiter {
            // Literal delimiter split
            if limit > 0 {
                input.splitn(limit + 1, &d).collect()
            } else {
                input.split(&d).collect()
            }
        } else {
            // Default: whitespace (like Python str.split())
            if limit > 0 {
                // splitn with whitespace needs custom handling
                let mut result = Vec::new();
                let mut remaining = input.as_str();
                for _ in 0..limit {
                    remaining = remaining.trim_start();
                    if let Some(pos) = remaining.find(char::is_whitespace) {
                        result.push(&remaining[..pos]);
                        remaining = &remaining[pos..];
                    } else {
                        break;
                    }
                }
                remaining = remaining.trim_start();
                if !remaining.is_empty() {
                    result.push(remaining);
                }
                result
            } else {
                input.split_whitespace().collect()
            }
        };

        // Build OutputData nodes for each part
        let nodes: Vec<OutputNode> = parts
            .iter()
            .map(|s| OutputNode::new(*s))
            .collect();

        // Build JSON array for iteration via $(split ...)
        let json_array: Vec<serde_json::Value> = parts
            .iter()
            .map(|s| serde_json::Value::String((*s).to_string()))
            .collect();

        let mut result = ExecResult::with_output(OutputData::nodes(nodes));
        // Preserve data for `for i in $(split ...)` iteration
        result.data = Some(Value::Json(serde_json::Value::Array(json_array)));
        result
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
    async fn test_split_whitespace() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello world foo".into()));

        let result = Split.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hello\nworld\nfoo");

        // Check structured data
        let data = result.data.unwrap();
        if let Value::Json(serde_json::Value::Array(arr)) = data {
            assert_eq!(arr.len(), 3);
            assert_eq!(arr[0], serde_json::json!("hello"));
            assert_eq!(arr[1], serde_json::json!("world"));
            assert_eq!(arr[2], serde_json::json!("foo"));
        } else {
            panic!("Expected JSON array");
        }
    }

    #[tokio::test]
    async fn test_split_delimiter() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("a:b:c".into()));
        args.positional.push(Value::String(":".into()));

        let result = Split.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "a\nb\nc");
    }

    #[tokio::test]
    async fn test_split_regex() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("a1b2c3".into()));
        args.named.insert("regex".to_string(), Value::String("[0-9]".into()));

        let result = Split.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "a\nb\nc\n");  // trailing empty string from split
    }

    #[tokio::test]
    async fn test_split_limit() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("a:b:c:d".into()));
        args.positional.push(Value::String(":".into()));
        args.named.insert("limit".to_string(), Value::Int(2));

        let result = Split.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "a\nb\nc:d");
    }

    #[tokio::test]
    async fn test_split_empty() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("".into()));

        let result = Split.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "");
    }

    #[tokio::test]
    async fn test_split_invalid_regex() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("test".into()));
        args.named.insert("regex".to_string(), Value::String("[invalid".into()));

        let result = Split.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("invalid regex"));
    }

    #[tokio::test]
    async fn test_split_multiple_spaces() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("  a   b   c  ".into()));

        let result = Split.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Default whitespace split should handle multiple spaces
        assert_eq!(result.out, "a\nb\nc");
    }

    #[tokio::test]
    async fn test_split_stdin_with_delimiter() {
        let mut ctx = make_ctx();
        ctx.set_stdin("a,b,c".to_string());
        let mut args = ToolArgs::new();
        // Delimiter is positional[0] when reading from stdin
        args.positional.push(Value::String(",".into()));

        let result = Split.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "a\nb\nc");
    }

    #[tokio::test]
    async fn test_split_stdin_whitespace() {
        let mut ctx = make_ctx();
        ctx.set_stdin("hello world foo".to_string());
        let args = ToolArgs::new();

        let result = Split.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hello\nworld\nfoo");
    }

    #[tokio::test]
    async fn test_split_no_input_error() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Split.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("no input"));
    }

    #[tokio::test]
    async fn test_split_stdin_trailing_newline() {
        let mut ctx = make_ctx();
        // Pipelines typically produce trailing newlines
        ctx.set_stdin("a,b,c\n".to_string());
        let mut args = ToolArgs::new();
        args.positional.push(Value::String(",".into()));

        let result = Split.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "a\nb\nc");
    }
}
