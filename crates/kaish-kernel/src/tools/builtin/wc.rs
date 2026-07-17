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
            let (lc, wc, cc, bc, invalid_utf8) = count_content(&input);
            if invalid_utf8 && (chars_only || words_only || show_all) {
                return ExecResult::failure(1, format!("wc: {INVALID_UTF8_HINT}"));
            }
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

        let needs_text = chars_only || words_only || show_all;

        for path in &paths {
            let resolved = ctx.resolve_path(path);
            // Stream the file through a bounded chunk window rather than reading
            // it whole. Byte/line counts come from raw bytes regardless of
            // encoding (`wc -c`/`-l` are exact on binary); char/word counts need
            // a text view, so an invalid-UTF-8 file is a loud per-file error
            // when they're actually requested — never a lossy U+FFFD mangle.
            let mut counter = WcCounter::default();
            match ctx
                .read_file_chunked(&resolved, ExecContext::STREAM_CHUNK_SIZE, |chunk| {
                    counter.push(chunk);
                    std::ops::ControlFlow::Continue(())
                })
                .await
            {
                Ok(()) => {
                    let (lc, wc, cc, bc, invalid_utf8) = counter.finish();

                    if invalid_utf8 && needs_text {
                        error_messages.push(format!("wc: {}: {INVALID_UTF8_HINT}", path));
                        had_error = true;
                        continue;
                    }

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

/// Shared tail of the loud error `wc` emits when `-m`/`-w`/default (any count
/// that needs a text view) meets invalid UTF-8. `-c`/`-l` are pure byte-level
/// operations (exact length, raw `\n` scan) and are unaffected — this only
/// fires when a char/word count was actually requested. Mirrors the rest of
/// the fleet's stance (`grep`/`sed`/`head`'s line mode/…): a loud error, never
/// a lossy `U+FFFD` mangle. See `docs/binary-data.md`.
const INVALID_UTF8_HINT: &str =
    "invalid UTF-8 — char/word counts require text; use -c/-l for byte/line counts, \
     or pipe through base64/xxd";

/// Incremental, line-buffered counter for streaming `wc`.
///
/// Fed arbitrary byte chunks via [`push`](WcCounter::push), it produces the
/// same `(lines, words, chars, bytes, invalid_utf8)` tuple as
/// [`count_content`] over the concatenation — without ever holding the whole
/// input. The trick is to carry the trailing partial line (the bytes after the
/// last `\n`) between chunks, so complete lines are only counted once their
/// bytes are all present. Because `\n` is a word/line separator and never part
/// of a multibyte UTF-8 sequence, splitting on it lets each line decode
/// cleanly and keeps words, chars, and UTF-8 boundaries from ever straddling a
/// chunk edge.
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
    /// Set once a line failed strict UTF-8 decode. Byte/line counts (`bytes`,
    /// `newlines`) stay exact regardless; `words`/`chars` under-count once this
    /// is set (that line's contribution is skipped), so callers that need
    /// char/word counts must check this flag and refuse rather than trust
    /// them.
    invalid_utf8: bool,
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
        // Strict decode, not lossy: a line that isn't valid UTF-8 marks
        // `invalid_utf8` and contributes nothing to words/chars rather than
        // expanding each bad byte run into a counted `U+FFFD`.
        match std::str::from_utf8(&self.carry[lo..hi]) {
            Ok(line) => {
                self.words += line.split_whitespace().count();
                self.chars += line.chars().count();
            }
            Err(_) => self.invalid_utf8 = true,
        }
    }

    fn finish(mut self) -> (usize, usize, usize, usize, bool) {
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
        (lines, self.words, chars, self.bytes, self.invalid_utf8)
    }
}

/// Count lines, words, chars, and bytes in content, plus whether the content
/// failed strict UTF-8 decode.
fn count_content(input: &[u8]) -> (usize, usize, usize, usize, bool) {
    // Byte and line counts are pure byte-level operations — exact for binary
    // too, regardless of UTF-8 validity. `wc -l` counts newline characters
    // (GNU semantics): an unterminated final line is not a line.
    let bytes = input.len();
    let lines = input.iter().filter(|&&b| b == b'\n').count();
    // Char/word counts need a text view: strict decode, not lossy — invalid
    // UTF-8 is reported to the caller instead of being expanded into counted
    // `U+FFFD` replacement characters.
    match std::str::from_utf8(input) {
        Ok(text) => {
            let words = text.split_whitespace().count();
            let chars = text.chars().count();
            (lines, words, chars, bytes, false)
        }
        Err(_) => (lines, 0, 0, bytes, true),
    }
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
        // exactly once. Every fixture here is valid UTF-8 by construction —
        // see `wc_counter_invalid_utf8_flag_is_split_independent` below for the
        // binary-input counterpart. (A mixed-validity buffer would legitimately
        // diverge between the two paths on the *word/char* counts — whole-buffer
        // `count_content` bails to `(0, 0)` the moment any byte is invalid,
        // while the chunked counter still tallies the valid lines before the
        // bad one — but both agree on the `invalid_utf8` flag that gates
        // display, so the divergence is never user-visible.)
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

    #[test]
    fn wc_counter_invalid_utf8_flag_is_split_independent() {
        // The `invalid_utf8` flag is what gates wc -m/-w's loud refusal
        // (GH #176) — if it depended on *where* the streaming reader happened
        // to split the input, `wc -m` could silently succeed on binary
        // depending on chunk size, exactly the class of chunk-boundary bug the
        // line-buffering scheme must not have. Byte and line counts (pure
        // byte-level operations) must also stay exact regardless of split
        // point or validity.
        let inputs: &[&[u8]] = &[
            b"\xff\x00\xfe\x80\x01\xc0kaish\xf5",   // invalid, no newline at all
            b"good line\n\xff\xfe bad line\nmore\n", // invalid line in the middle
            b"\xff\xfestart\ngood\n\xc0end",         // invalid at both ends
        ];
        for input in inputs {
            let (want_lines, _, _, want_bytes, want_invalid) = count_content(input);
            assert!(want_invalid, "fixture must actually be invalid UTF-8: {input:?}");
            for split in 0..=input.len() {
                let mut c = WcCounter::default();
                c.push(&input[..split]);
                c.push(&input[split..]);
                let (lines, _, _, bytes, invalid_utf8) = c.finish();
                assert!(
                    invalid_utf8,
                    "invalid_utf8 must be true regardless of split point: input={input:?} split={split}"
                );
                assert_eq!(bytes, want_bytes, "byte count must stay exact: split={split}");
                assert_eq!(lines, want_lines, "line count must stay exact: split={split}");
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
