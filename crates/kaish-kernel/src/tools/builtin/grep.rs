//! grep — Search for patterns in files or stdin.

use async_trait::async_trait;
use regex::{Regex, RegexBuilder};
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};
use crate::vfs::Filesystem;

/// Grep tool: search for patterns in text.
pub struct Grep;

#[async_trait]
impl Tool for Grep {
    fn name(&self) -> &str {
        "grep"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("grep", "Search for patterns in files or stdin")
            .param(ParamSchema::required(
                "pattern",
                "string",
                "Regular expression pattern to search for",
            ))
            .param(ParamSchema::optional(
                "path",
                "string",
                Value::Null,
                "File to search (reads stdin if not provided)",
            ))
            .param(ParamSchema::optional(
                "ignore_case",
                "bool",
                Value::Bool(false),
                "Case-insensitive matching (-i)",
            ))
            .param(ParamSchema::optional(
                "line_number",
                "bool",
                Value::Bool(false),
                "Prefix output with line numbers (-n)",
            ))
            .param(ParamSchema::optional(
                "invert",
                "bool",
                Value::Bool(false),
                "Select non-matching lines (-v)",
            ))
            .param(ParamSchema::optional(
                "count",
                "bool",
                Value::Bool(false),
                "Only print count of matching lines (-c)",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let pattern = match args.get_string("pattern", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "grep: missing pattern argument"),
        };

        let ignore_case = args.has_flag("ignore_case") || args.has_flag("i");
        let line_number = args.has_flag("line_number") || args.has_flag("n");
        let invert = args.has_flag("invert") || args.has_flag("v");
        let count_only = args.has_flag("count") || args.has_flag("c");

        // Build regex
        let regex = match RegexBuilder::new(&pattern)
            .case_insensitive(ignore_case)
            .build()
        {
            Ok(r) => r,
            Err(e) => return ExecResult::failure(1, format!("grep: invalid pattern: {}", e)),
        };

        // Get input: from file or stdin
        let input = match args.get_string("path", 1) {
            Some(path) => {
                let resolved = ctx.resolve_path(&path);
                match ctx.vfs.read(Path::new(&resolved)).await {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => s,
                        Err(_) => {
                            return ExecResult::failure(1, format!("grep: {}: invalid UTF-8", path))
                        }
                    },
                    Err(e) => return ExecResult::failure(1, format!("grep: {}: {}", path, e)),
                }
            }
            None => {
                // Read from stdin
                ctx.take_stdin().unwrap_or_default()
            }
        };

        let (output, match_count) = grep_lines(&input, &regex, line_number, invert);

        if count_only {
            ExecResult::success(format!("{}\n", match_count))
        } else if match_count == 0 {
            // grep returns exit code 1 if no matches (but not an error)
            ExecResult::from_output(1, output, "")
        } else {
            ExecResult::success(output)
        }
    }
}

/// Search lines and return matching output and count.
fn grep_lines(input: &str, regex: &Regex, show_line_numbers: bool, invert: bool) -> (String, usize) {
    let mut output = String::new();
    let mut match_count = 0;

    for (line_num, line) in input.lines().enumerate() {
        let matches = regex.is_match(line);
        let should_include = if invert { !matches } else { matches };

        if should_include {
            match_count += 1;
            if show_line_numbers {
                output.push_str(&format!("{}:{}\n", line_num + 1, line));
            } else {
                output.push_str(line);
                output.push('\n');
            }
        }
    }

    (output, match_count)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("test.txt"), b"hello world\nHELLO WORLD\nfoo bar\nbaz")
            .await
            .unwrap();
        mem.write(Path::new("lines.txt"), b"line one\nline two\nline three\nfour")
            .await
            .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_grep_file() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.positional.push(Value::String("/test.txt".into()));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hello world\n");
    }

    #[tokio::test]
    async fn test_grep_case_insensitive() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.positional.push(Value::String("/test.txt".into()));
        args.flags.insert("i".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("hello world"));
        assert!(result.out.contains("HELLO WORLD"));
    }

    #[tokio::test]
    async fn test_grep_line_numbers() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("line".into()));
        args.positional.push(Value::String("/lines.txt".into()));
        args.flags.insert("n".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("1:line one"));
        assert!(result.out.contains("2:line two"));
        assert!(result.out.contains("3:line three"));
    }

    #[tokio::test]
    async fn test_grep_invert() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("line".into()));
        args.positional.push(Value::String("/lines.txt".into()));
        args.flags.insert("v".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "four\n");
    }

    #[tokio::test]
    async fn test_grep_count() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("line".into()));
        args.positional.push(Value::String("/lines.txt".into()));
        args.flags.insert("c".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "3\n");
    }

    #[tokio::test]
    async fn test_grep_no_match() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("xyz".into()));
        args.positional.push(Value::String("/test.txt".into()));

        let result = Grep.execute(args, &mut ctx).await;
        // Exit code 1 for no matches, but not an error
        assert!(!result.ok());
        assert!(result.err.is_empty());
        assert_eq!(result.code, 1);
    }

    #[tokio::test]
    async fn test_grep_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("apple\nbanana\napricot\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("ap".into()));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("apple"));
        assert!(result.out.contains("apricot"));
        assert!(!result.out.contains("banana"));
    }

    #[tokio::test]
    async fn test_grep_regex() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("^line".into())); // Start of line
        args.positional.push(Value::String("/lines.txt".into()));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("line one"));
        assert!(!result.out.contains("four")); // "four" doesn't start with "line"
    }

    #[tokio::test]
    async fn test_grep_invalid_regex() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("[invalid".into()));
        args.positional.push(Value::String("/test.txt".into()));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("invalid pattern"));
    }

    #[tokio::test]
    async fn test_grep_missing_pattern() {
        let mut ctx = make_ctx().await;
        let result = Grep.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("pattern"));
    }

    #[tokio::test]
    async fn test_grep_file_not_found() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("pattern".into()));
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }
}
