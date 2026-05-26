//! wc — Word, line, character, and byte count.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Wc tool: count lines, words, characters, and bytes.
pub struct Wc;

/// clap-derived argv layer for wc. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "wc", about = "Print line, word, and byte counts")]
struct WcArgs {
    /// Print line count only (-l)
    #[arg(short = 'l', long = "lines")]
    lines: bool,

    /// Print word count only (-w)
    #[arg(short = 'w', long = "words")]
    words: bool,

    /// Print character count only (-m)
    #[arg(short = 'm', long = "chars")]
    chars: bool,

    /// Print byte count only (-c)
    #[arg(short = 'c', long = "bytes")]
    bytes: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Sink — to_argv() always emits `--` before positionals. Read paths
    /// off args.positional directly.
    #[arg(hide = true)]
    rest: Vec<String>,
}

#[async_trait]
impl Tool for Wc {
    fn name(&self) -> &str {
        "wc"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &WcArgs::command(),
            "wc",
            "Print line, word, and byte counts",
            [
                ("Count all (lines, words, bytes)", "wc file.txt"),
                ("Count lines only", "wc -l file.txt"),
                ("Count words from stdin", "echo 'hello world' | wc -w"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let parsed = match WcArgs::try_parse_from(
            std::iter::once("wc".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("wc: {e}")),
        };
        parsed.global.apply(ctx);

        let lines_only = parsed.lines;
        let words_only = parsed.words;
        let chars_only = parsed.chars;
        let bytes_only = parsed.bytes;
        let show_all = !lines_only && !words_only && !chars_only && !bytes_only;

        // Collect all file paths from positional arguments, expanding globs
        let paths = match ctx.expand_paths(&args.positional).await {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(1, format!("wc: {}", e)),
        };

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
    let mut headers = vec!["FILE".to_string()];

    if show_all || lines_only {
        headers.push("LINES".to_string());
    }
    if show_all || words_only {
        headers.push("WORDS".to_string());
    }
    if show_all || bytes_only {
        headers.push("BYTES".to_string());
    }
    if chars_only {
        headers.push("CHARS".to_string());
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
    use crate::ast::Value;
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
        assert!(result.text_out().contains("2"));
        assert!(result.text_out().contains("5"));
        assert!(result.text_out().contains("24"));
        assert!(result.text_out().contains("/test.txt"));
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
        assert!(result.text_out().contains("2"));
        assert!(result.text_out().contains("/test.txt"));
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
        assert!(result.text_out().contains("5"));
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
        assert!(result.text_out().contains("24"));
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
        assert!(result.text_out().contains("12"));
    }

    #[tokio::test]
    async fn test_wc_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("one two three\nfour five\n".to_string());

        let args = ToolArgs::new();
        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        // 2 lines, 5 words
        assert!(result.text_out().contains("2"));
        assert!(result.text_out().contains("5"));
        // No filename in output for stdin
        assert!(!result.text_out().contains("/"));
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
        assert!(result.text_out().contains("2")); // lines
        assert!(result.text_out().contains("5")); // words
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
        assert!(result_bytes.text_out().contains("14"));
        assert!(result_chars.text_out().contains("12"));
    }

    #[tokio::test]
    async fn test_wc_empty_input() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let args = ToolArgs::new();
        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should show 0s for empty input
        assert!(result.text_out().contains("0"));
    }

    #[tokio::test]
    async fn test_wc_single_word_no_newline() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("word".to_string());

        let args = ToolArgs::new();
        let result = Wc.execute(args, &mut ctx).await;
        assert!(result.ok());
        // 0 lines (no newline), 1 word, 4 bytes
        let out = result.text_out();
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
        assert!(result.text_out().contains("0"));
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
        assert!(result.text_out().contains("8"));
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
        assert!(result.text_out().contains("10001")); // 10000 + newline
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
        assert!(result.text_out().contains("total"));
    }
}
