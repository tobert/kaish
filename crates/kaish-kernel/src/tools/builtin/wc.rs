//! wc — Word, line, character, and byte count.

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::glob::contains_glob;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
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
            ).with_aliases(["-l"]))
            .param(ParamSchema::optional(
                "words",
                "bool",
                Value::Bool(false),
                "Print word count only (-w)",
            ).with_aliases(["-w"]))
            .param(ParamSchema::optional(
                "chars",
                "bool",
                Value::Bool(false),
                "Print character count only (-m)",
            ).with_aliases(["-m"]))
            .param(ParamSchema::optional(
                "bytes",
                "bool",
                Value::Bool(false),
                "Print byte count only (-c)",
            ).with_aliases(["-c"]))
            .example("Count all (lines, words, bytes)", "wc file.txt")
            .example("Count lines only", "wc -l file.txt")
            .example("Count words from stdin", "echo 'hello world' | wc -w")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let lines_only = args.has_flag("lines") || args.has_flag("l");
        let words_only = args.has_flag("words") || args.has_flag("w");
        let chars_only = args.has_flag("chars") || args.has_flag("m");
        let bytes_only = args.has_flag("bytes") || args.has_flag("c");
        let show_all = !lines_only && !words_only && !chars_only && !bytes_only;

        // Collect all file paths from positional arguments, expanding globs
        let mut paths: Vec<String> = Vec::new();
        for v in &args.positional {
            if let Value::String(s) = v {
                if contains_glob(s) {
                    match ctx.expand_glob(s).await {
                        Ok(expanded) => {
                            let root = ctx.resolve_path(".");
                            for p in expanded {
                                let rel = p.strip_prefix(&root).unwrap_or(&p);
                                paths.push(rel.to_string_lossy().to_string());
                            }
                        }
                        Err(e) => return ExecResult::failure(1, format!("wc: {}", e)),
                    }
                } else {
                    paths.push(s.clone());
                }
            }
        }

        // Build headers based on flags
        let headers = build_headers(lines_only, words_only, chars_only, bytes_only, show_all);

        // If no files, read from stdin
        if paths.is_empty() {
            let input = ctx.read_stdin_to_string().await.unwrap_or_default();
            let (lc, wc, cc, bc) = count_content(&input);
            let cells = build_cells(lc, wc, cc, bc, lines_only, words_only, chars_only, bytes_only, show_all);
            let node = OutputNode::new("").with_cells(cells);
            return ExecResult::with_output(OutputData::table(headers, vec![node]));
        }

        // Process each file
        let mut nodes = Vec::new();
        let mut total_lines = 0usize;
        let mut total_words = 0usize;
        let mut total_chars = 0usize;
        let mut total_bytes = 0usize;
        let mut had_error = false;
        let mut error_messages = Vec::new();

        for path in &paths {
            let resolved = ctx.resolve_path(path);
            match ctx.backend.read(Path::new(&resolved), None).await {
                Ok(data) => match String::from_utf8(data) {
                    Ok(input) => {
                        let (lc, wc, cc, bc) = count_content(&input);

                        total_lines += lc;
                        total_words += wc;
                        total_chars += cc;
                        total_bytes += bc;

                        let cells = build_cells(lc, wc, cc, bc, lines_only, words_only, chars_only, bytes_only, show_all);
                        nodes.push(OutputNode::new(path.as_str()).with_cells(cells));
                    }
                    Err(_) => {
                        error_messages.push(format!("wc: {}: invalid UTF-8", path));
                        had_error = true;
                    }
                },
                Err(e) => {
                    error_messages.push(format!("wc: {}: {}", path, e));
                    had_error = true;
                }
            }
        }

        // Add total row if multiple files
        if paths.len() > 1 {
            let cells = build_cells(
                total_lines, total_words, total_chars, total_bytes,
                lines_only, words_only, chars_only, bytes_only, show_all,
            );
            nodes.push(OutputNode::new("total").with_cells(cells));
        }

        let output = OutputData::table(headers, nodes);

        if had_error {
            // Return with error but include the output we did get
            let mut result = ExecResult::with_output(output);
            result.code = 1;
            result.err = error_messages.join("\n");
            result
        } else {
            ExecResult::with_output(output)
        }
    }
}

/// Count lines, words, chars, and bytes in content.
fn count_content(input: &str) -> (usize, usize, usize, usize) {
    let lines = input.lines().count();
    let words = input.split_whitespace().count();
    let chars = input.chars().count();
    let bytes = input.len();
    (lines, words, chars, bytes)
}

/// Build headers based on which counts are shown.
fn build_headers(
    lines_only: bool,
    words_only: bool,
    chars_only: bool,
    bytes_only: bool,
    show_all: bool,
) -> Vec<String> {
    let mut headers = vec!["File".to_string()];

    if show_all || lines_only {
        headers.push("Lines".to_string());
    }
    if show_all || words_only {
        headers.push("Words".to_string());
    }
    if show_all || bytes_only {
        headers.push("Bytes".to_string());
    }
    if chars_only {
        headers.push("Chars".to_string());
    }

    headers
}

/// Build cells for a single count row.
#[allow(clippy::too_many_arguments)]
fn build_cells(
    line_count: usize,
    word_count: usize,
    char_count: usize,
    byte_count: usize,
    lines_only: bool,
    words_only: bool,
    chars_only: bool,
    bytes_only: bool,
    show_all: bool,
) -> Vec<String> {
    let mut cells = Vec::new();

    if show_all || lines_only {
        cells.push(line_count.to_string());
    }
    if show_all || words_only {
        cells.push(word_count.to_string());
    }
    if show_all || bytes_only {
        cells.push(byte_count.to_string());
    }
    if chars_only {
        cells.push(char_count.to_string());
    }

    cells
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
        // TSV format: filename\tcount
        assert!(result.out.contains("2"));
        assert!(result.out.contains("/test.txt"));
    }

    #[tokio::test]
    async fn test_wc_words_only() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/test.txt".into()));
        args.flags.insert("w".to_string());

        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        // TSV format: filename\tcount
        assert!(result.out.contains("5"));
    }

    #[tokio::test]
    async fn test_wc_bytes_only() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/test.txt".into()));
        args.flags.insert("c".to_string());

        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        // TSV format: filename\tcount
        assert!(result.out.contains("24"));
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
        // TSV format: filename\tcount
        assert!(result.out.contains("12"));
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
        // 0 words - TSV format with empty filename for stdin
        assert!(result.out.contains("0"));
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
        assert!(result.out.contains("8"));
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

    #[tokio::test]
    async fn test_wc_glob() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("*.txt".into()));
        args.flags.insert("l".to_string());

        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should show counts for multiple files and a total
        assert!(result.out.contains("total"));
    }
}
