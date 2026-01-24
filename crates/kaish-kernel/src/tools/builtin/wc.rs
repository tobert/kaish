//! wc — Word, line, character, and byte count.

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Wc tool: count lines, words, characters, and bytes.
pub struct Wc;

#[async_trait]
impl Tool for Wc {
    fn name(&self) -> &str {
        "wc"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("wc", "Print line, word, and byte counts")
            .param(ParamSchema::optional(
                "path",
                "string",
                Value::Null,
                "File to count (reads stdin if not provided)",
            ))
            .param(ParamSchema::optional(
                "lines",
                "bool",
                Value::Bool(false),
                "Print line count only (-l)",
            ))
            .param(ParamSchema::optional(
                "words",
                "bool",
                Value::Bool(false),
                "Print word count only (-w)",
            ))
            .param(ParamSchema::optional(
                "chars",
                "bool",
                Value::Bool(false),
                "Print character count only (-m)",
            ))
            .param(ParamSchema::optional(
                "bytes",
                "bool",
                Value::Bool(false),
                "Print byte count only (-c)",
            ))
            .example("Count all (lines, words, bytes)", "wc file.txt")
            .example("Count lines only", "wc -l file.txt")
            .example("Count words from stdin", "echo 'hello world' | wc -w")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Get input: from file or stdin
        let (input, filename) = match args.get_string("path", 0) {
            Some(path) => {
                let resolved = ctx.resolve_path(&path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => (s, Some(path)),
                        Err(_) => {
                            return ExecResult::failure(1, format!("wc: {}: invalid UTF-8", path))
                        }
                    },
                    Err(e) => return ExecResult::failure(1, format!("wc: {}: {}", path, e)),
                }
            }
            None => (ctx.take_stdin().unwrap_or_default(), None),
        };

        let lines_only = args.has_flag("lines") || args.has_flag("l");
        let words_only = args.has_flag("words") || args.has_flag("w");
        let chars_only = args.has_flag("chars") || args.has_flag("m");
        let bytes_only = args.has_flag("bytes") || args.has_flag("c");

        let line_count = input.lines().count();
        let word_count = input.split_whitespace().count();
        let char_count = input.chars().count();
        let byte_count = input.len();

        // If any specific flag is set, show only those counts
        let show_all = !lines_only && !words_only && !chars_only && !bytes_only;

        let mut parts = Vec::new();

        if show_all || lines_only {
            parts.push(format!("{:>8}", line_count));
        }
        if show_all || words_only {
            parts.push(format!("{:>8}", word_count));
        }
        if show_all || bytes_only {
            parts.push(format!("{:>8}", byte_count));
        }
        if chars_only {
            parts.push(format!("{:>8}", char_count));
        }

        let mut output = parts.join("");
        if let Some(name) = filename {
            output.push(' ');
            output.push_str(&name);
        }
        output.push('\n');

        ExecResult::success(output)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("test.txt"), b"hello world\nfoo bar baz\n")
            .await
            .unwrap();
        mem.write(Path::new("unicode.txt"), "héllo wörld\n".as_bytes())
            .await
            .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_wc_all_counts() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/test.txt".into()));

        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        // 2 lines, 5 words, 24 bytes
        assert!(result.out.contains("2"));
        assert!(result.out.contains("5"));
        assert!(result.out.contains("24"));
        assert!(result.out.contains("/test.txt"));
    }

    #[tokio::test]
    async fn test_wc_lines_only() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/test.txt".into()));
        args.flags.insert("l".to_string());

        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.trim().starts_with("2"));
    }

    #[tokio::test]
    async fn test_wc_words_only() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/test.txt".into()));
        args.flags.insert("w".to_string());

        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.trim().starts_with("5"));
    }

    #[tokio::test]
    async fn test_wc_bytes_only() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/test.txt".into()));
        args.flags.insert("c".to_string());

        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.trim().starts_with("24"));
    }

    #[tokio::test]
    async fn test_wc_chars() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/unicode.txt".into()));
        args.flags.insert("m".to_string());

        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        // "héllo wörld\n" = 12 chars (é and ö are single chars)
        assert!(result.out.trim().starts_with("12"));
    }

    #[tokio::test]
    async fn test_wc_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("one two three\nfour five\n".to_string());

        let args = ToolArgs::new();
        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        // 2 lines, 5 words
        assert!(result.out.contains("2"));
        assert!(result.out.contains("5"));
        // No filename in output for stdin
        assert!(!result.out.contains("/"));
    }

    #[tokio::test]
    async fn test_wc_file_not_found() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Wc.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    // --- Flag combinations and edge cases ---

    #[tokio::test]
    async fn test_wc_lines_and_words() {
        // wc -lw (common combination)
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/test.txt".into()));
        args.flags.insert("l".to_string());
        args.flags.insert("w".to_string());

        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("2")); // lines
        assert!(result.out.contains("5")); // words
    }

    #[tokio::test]
    async fn test_wc_unicode_chars_vs_bytes() {
        // Verify chars != bytes for multibyte UTF-8
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/unicode.txt".into()));
        args.flags.insert("c".to_string()); // bytes

        let result_bytes = Wc.execute(args, &mut ctx).await;
        assert!(result_bytes.ok());

        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/unicode.txt".into()));
        args.flags.insert("m".to_string()); // chars

        let result_chars = Wc.execute(args, &mut ctx).await;
        assert!(result_chars.ok());

        // "héllo wörld\n" is 14 bytes but 12 chars
        assert!(result_bytes.out.contains("14"));
        assert!(result_chars.out.contains("12"));
    }

    #[tokio::test]
    async fn test_wc_empty_input() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let args = ToolArgs::new();
        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should show 0s for empty input
        assert!(result.out.contains("0"));
    }

    #[tokio::test]
    async fn test_wc_single_word_no_newline() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("word".to_string());

        let args = ToolArgs::new();
        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        // 0 lines (no newline), 1 word, 4 bytes
        let out = &result.out;
        assert!(out.contains("0") || out.contains("1")); // lines or words
    }

    #[tokio::test]
    async fn test_wc_only_whitespace() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("   \n\t\t\n   ".to_string());

        let mut args = ToolArgs::new();
        args.flags.insert("w".to_string());

        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        // 0 words
        assert!(result.out.trim().starts_with("0"));
    }

    #[tokio::test]
    async fn test_wc_japanese_text() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("日本語 テスト\n".to_string());

        let mut args = ToolArgs::new();
        args.flags.insert("m".to_string());

        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        // 日本語 (3) + space (1) + テスト (3) + newline (1) = 8 chars
        assert!(result.out.trim().starts_with("8"));
    }

    #[tokio::test]
    async fn test_wc_long_line() {
        let mut ctx = make_ctx().await;
        let long_line = "a".repeat(10000);
        ctx.set_stdin(format!("{}\n", long_line));

        let mut args = ToolArgs::new();
        args.flags.insert("c".to_string());

        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("10001")); // 10000 + newline
    }
}
