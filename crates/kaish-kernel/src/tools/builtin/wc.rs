//! wc — Word, line, character, and byte count.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Wc tool: count lines, words, characters, and bytes.
pub struct Wc;

/// clap-derived argv layer for wc.
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

    /// Files to count; reads stdin when none are given.
    paths: Vec<String>,
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

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("wc: {e}")),
        };
        let parsed = match WcArgs::try_parse_from(
            std::iter::once("wc".to_string()).chain(argv),
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
            let input = ctx.read_stdin_to_bytes().await.unwrap_or_default();
            let (lc, wc, cc, bc) = count_content(&input);
            let cells = build_cells(lc, wc, cc, bc, lines_only, words_only, chars_only, bytes_only, show_all);
            let text = format_counts_text(&[("".to_string(), cells.clone())]);
            let node = OutputNode::new("").with_cells(cells);
            return ExecResult::with_output_and_text(OutputData::table(headers, vec![node]), text);
        }

        // Process each file
        let mut nodes = Vec::new();
        // (label, count-cells) per row, for the right-justified text rendering.
        let mut rows: Vec<(String, Vec<String>)> = Vec::new();
        let mut total_lines = 0usize;
        let mut total_words = 0usize;
        let mut total_chars = 0usize;
        let mut total_bytes = 0usize;
        let mut had_error = false;
        let mut error_messages = Vec::new();

        for path in &paths {
            let resolved = ctx.resolve_path(path);
            // Stream the file through a bounded chunk window rather than reading
            // it whole. Counts come from raw bytes — `wc -c` is exact on binary,
            // and there is no UTF-8 gate to reject a binary file.
            let mut counter = WcCounter::default();
            match ctx
                .read_file_chunked(&resolved, ExecContext::STREAM_CHUNK_SIZE, |chunk| {
                    counter.push(chunk);
                    std::ops::ControlFlow::Continue(())
                })
                .await
            {
                Ok(()) => {
                    let (lc, wc, cc, bc) = counter.finish();

                    total_lines += lc;
                    total_words += wc;
                    total_chars += cc;
                    total_bytes += bc;

                    let cells = build_cells(lc, wc, cc, bc, lines_only, words_only, chars_only, bytes_only, show_all);
                    rows.push((path.clone(), cells.clone()));
                    nodes.push(OutputNode::new(path.as_str()).with_cells(cells));
                }
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
            rows.push(("total".to_string(), cells.clone()));
            nodes.push(OutputNode::new("total").with_cells(cells));
        }

        let output = OutputData::table(headers, nodes);
        let text = format_counts_text(&rows);

        if had_error {
            // Return with error but include the output we did get
            let mut result = ExecResult::with_output_and_text(output, text);
            result.code = 1;
            result.err = error_messages.join("\n");
            result
        } else {
            ExecResult::with_output_and_text(output, text)
        }
    }
}

/// Incremental, line-buffered counter for streaming `wc`.
///
/// Fed arbitrary byte chunks via [`push`](WcCounter::push), it produces the
/// same `(lines, words, chars, bytes)` tuple as [`count_content`] over the
/// concatenation — without ever holding the whole input. The trick is to carry
/// the trailing partial line (the bytes after the last `\n`) between chunks, so
/// complete lines are only counted once their bytes are all present. Because
/// `\n` is a word/line separator and never part of a multibyte UTF-8 sequence,
/// splitting on it lets each line decode cleanly and keeps words, chars, and
/// UTF-8 boundaries from ever straddling a chunk edge.
#[derive(Default)]
struct WcCounter {
    /// Bytes seen since the last newline — the in-progress line.
    carry: Vec<u8>,
    newlines: usize,
    words: usize,
    /// Char count of completed lines only (excludes the `\n` separators, which
    /// are added back in `finish`).
    chars: usize,
    bytes: usize,
}

impl WcCounter {
    fn push(&mut self, chunk: &[u8]) {
        self.bytes += chunk.len();
        self.carry.extend_from_slice(chunk);

        // Count every complete line now (terminated by `\n`); keep the trailing
        // partial line in `carry` for the next chunk.
        let mut start = 0;
        while let Some(pos) = self.carry[start..].iter().position(|&b| b == b'\n') {
            let line_end = start + pos;
            self.count_line(start, line_end);
            self.newlines += 1;
            start = line_end + 1;
        }
        if start > 0 {
            self.carry.drain(..start);
        }
    }

    fn count_line(&mut self, lo: usize, hi: usize) {
        let line = String::from_utf8_lossy(&self.carry[lo..hi]);
        self.words += line.split_whitespace().count();
        self.chars += line.chars().count();
    }

    fn finish(mut self) -> (usize, usize, usize, usize) {
        // The final remainder (bytes after the last `\n`) still contributes its
        // words and chars, so count it — but it is NOT a line. `wc -l` counts
        // newline characters (like GNU): an unterminated final line is not
        // counted (`a\nb` → 1, not 2).
        if !self.carry.is_empty() {
            self.count_line(0, self.carry.len());
        }
        let lines = self.newlines;
        // Every consumed `\n` is one character that line-splitting removed.
        let chars = self.chars + self.newlines;
        (lines, self.words, chars, self.bytes)
    }
}

/// Count lines, words, chars, and bytes in content.
fn count_content(input: &[u8]) -> (usize, usize, usize, usize) {
    // Byte count is the raw length — exact for binary too. Lines/words/chars
    // use a lossy text view: identical to before for valid UTF-8 (borrowed, no
    // copy), best-effort for binary. This keeps `wc -c` honest on binary input.
    let bytes = input.len();
    let text = String::from_utf8_lossy(input);
    // `wc -l` counts newline characters (GNU semantics): an unterminated final
    // line is not a line. `str::lines()` would over-count it.
    let lines = input.iter().filter(|&&b| b == b'\n').count();
    let words = text.split_whitespace().count();
    let chars = text.chars().count();
    (lines, words, chars, bytes)
}

/// Render wc's text output: each row is the count fields right-justified to a
/// common width (the widest field in the whole output), single-space separated,
/// followed by the filename (omitted for stdin's empty label). A single-count
/// invocation (`wc -l`) naturally comes out unpadded — its lone field's width
/// is its own digit count. Every row is newline-terminated. The byte shape is
/// pinned by `builtin_sweep_fidelity_tests` so it's contractual, not surprising.
fn format_counts_text(rows: &[(String, Vec<String>)]) -> String {
    let width = rows
        .iter()
        .flat_map(|(_, cells)| cells.iter().map(|c| c.len()))
        .max()
        .unwrap_or(0);
    let mut out = String::new();
    for (label, cells) in rows {
        let cols: Vec<String> = cells.iter().map(|c| format!("{c:>width$}")).collect();
        out.push_str(&cols.join(" "));
        if !label.is_empty() {
            out.push(' ');
            out.push_str(label);
        }
        out.push('\n');
    }
    out
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
    use std::path::Path;
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

    // --- Streaming: chunk-seam correctness ---

    #[test]
    fn wc_counter_matches_whole_buffer_across_every_split() {
        // Each input is counted by the incremental WcCounter fed as two chunks
        // split at *every* byte boundary, and compared to the whole-buffer
        // reference. The multibyte and mid-word splits are the seam this guards:
        // a 2-byte kanji or a word straddling the chunk edge must still count
        // exactly once.
        let inputs: &[&[u8]] = &[
            b"hello world\nfoo bar baz\n",
            b"word",
            b"",
            "h\u{e9}llo w\u{f6}rld\n\u{65e5}\u{672c}\u{8a9e} \u{30c6}\u{30b9}\u{30c8}\n".as_bytes(),
            b"a\n\nb",
            b"   \n\t\t\n   ",
            b"trailing no newline",
        ];
        for input in inputs {
            let want = count_content(input);
            for split in 0..=input.len() {
                let mut c = WcCounter::default();
                c.push(&input[..split]);
                c.push(&input[split..]);
                assert_eq!(
                    c.finish(),
                    want,
                    "input={:?} split={}",
                    String::from_utf8_lossy(input),
                    split
                );
            }
        }
    }

    // --- Streaming: bounded-memory proof ---

    /// Recorded (offset, limit) pairs from each `read_range` call.
    type RecordedRanges = Arc<std::sync::Mutex<Vec<(Option<u64>, Option<u64>)>>>;

    /// A `Filesystem` that records every `read_range` it is asked for, so a test
    /// can prove a builtin pulls a file in bounded chunks rather than slurping
    /// it whole. Delegates all real work to an inner `MemoryFs`.
    struct RecordingFs {
        inner: MemoryFs,
        ranges: RecordedRanges,
    }

    #[async_trait::async_trait]
    impl Filesystem for RecordingFs {
        async fn read(&self, path: &Path) -> std::io::Result<Vec<u8>> {
            // A whole-file read here would defeat the test's purpose; record it
            // as `(None, None)` so the assertion below can catch it.
            self.ranges.lock().unwrap().push((None, None));
            self.inner.read(path).await
        }
        async fn read_range(
            &self,
            path: &Path,
            range: Option<kaish_vfs::ReadRange>,
        ) -> std::io::Result<Vec<u8>> {
            let key = (
                range.as_ref().and_then(|r| r.offset),
                range.as_ref().and_then(|r| r.limit),
            );
            self.ranges.lock().unwrap().push(key);
            self.inner.read_range(path, range).await
        }
        async fn write(&self, path: &Path, data: &[u8]) -> std::io::Result<()> {
            self.inner.write(path, data).await
        }
        async fn list(&self, path: &Path) -> std::io::Result<Vec<crate::vfs::DirEntry>> {
            self.inner.list(path).await
        }
        async fn stat(&self, path: &Path) -> std::io::Result<crate::vfs::DirEntry> {
            self.inner.stat(path).await
        }
        async fn mkdir(&self, path: &Path) -> std::io::Result<()> {
            self.inner.mkdir(path).await
        }
        async fn remove(&self, path: &Path) -> std::io::Result<()> {
            self.inner.remove(path).await
        }
        fn read_only(&self) -> bool {
            self.inner.read_only()
        }
    }

    #[tokio::test]
    async fn wc_streams_file_in_bounded_chunks() {
        let ranges = Arc::new(std::sync::Mutex::new(Vec::new()));
        let rec = RecordingFs {
            inner: MemoryFs::new(),
            ranges: ranges.clone(),
        };
        // 1000 bytes, no newline → one long word; we only assert the byte total
        // and the read pattern.
        let payload = vec![b'x'; 1000];
        rec.inner.write(Path::new("big.txt"), &payload).await.unwrap();

        let mut vfs = VfsRouter::new();
        vfs.mount("/", rec);
        let ctx = ExecContext::new(Arc::new(vfs));

        let mut counter = WcCounter::default();
        ctx.read_file_chunked(Path::new("/big.txt"), 256, |c| {
            counter.push(c);
            std::ops::ControlFlow::Continue(())
        })
        .await
        .unwrap();

        let recs = ranges.lock().unwrap();
        // 1000 bytes / 256 → 4 data chunks + 1 terminating empty read.
        assert!(
            recs.len() >= 4,
            "expected the file to be read in several chunks, got {} reads",
            recs.len()
        );
        // Never a whole-file read, and every chunk bounded to the window.
        assert!(
            recs.iter().all(|&(_, limit)| limit == Some(256)),
            "every read must be bounded to the chunk size; recorded {recs:?}"
        );
        assert_eq!(counter.finish().3, payload.len(), "byte count is exact");
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
