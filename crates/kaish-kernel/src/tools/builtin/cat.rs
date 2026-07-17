//! cat — Read and output file contents.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

#[cfg(test)]
use crate::ast::Value;
use crate::backend::ReadRange;
use crate::interpreter::{ExecResult, OutputData};
use crate::scheduler::PipeWriter;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Cat tool: read and output file contents.
pub struct Cat;

/// clap-derived argv layer for cat.
#[derive(Parser, Debug)]
#[command(name = "cat", about = "Read and output file contents")]
struct CatArgs {
    /// Number output lines.
    #[arg(short = 'n', long = "number")]
    number: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Files to read. Reads stdin when no files are given.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Cat {
    fn name(&self) -> &str {
        "cat"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &CatArgs::command(),
            "cat",
            "Read and output file contents",
            [
                ("Read a file", "cat README.md"),
                ("Show line numbers", "cat -n src/main.rs"),
                ("Concatenate files", "cat header.txt body.txt"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("cat: {e}")),
        };
        let parsed = match CatArgs::try_parse_from(
            std::iter::once("cat".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("cat: {e}")),
        };
        parsed.global.apply(ctx);
        let number_lines = parsed.number;

        // If no files specified, read from stdin (like POSIX cat)
        if args.positional.is_empty() {
            // Streaming path: pipe_stdin → pipe_stdout without buffering
            if !number_lines && ctx.pipe_stdin.is_some() && ctx.pipe_stdout.is_some() {
                if let (Some(mut pipe_in), Some(mut pipe_out)) =
                    (ctx.pipe_stdin.take(), ctx.pipe_stdout.take())
                {
                    use tokio::io::{AsyncReadExt, AsyncWriteExt};
                    let mut buf = [0u8; 8192];
                    loop {
                        match pipe_in.read(&mut buf).await {
                            Ok(0) => break,
                            Ok(n) => {
                                if pipe_out.write_all(&buf[..n]).await.is_err() {
                                    break; // broken pipe
                                }
                            }
                            Err(_) => break,
                        }
                    }
                    let _ = pipe_out.shutdown().await;
                    return ExecResult::success("");
                }
            }

            // Buffered path (e.g. `cat` as a pipeline's last stage). With `-n`
            // we need text for line numbering, so binary is a loud error;
            // without it, stay byte-clean so piped binary survives intact
            // (`dd if=/dev/urandom … | cat` → a Bytes result, not a lossy mangle).
            if number_lines {
                let stdin = match ctx.read_stdin_to_text().await {
                    Ok(s) => s.unwrap_or_default(),
                    Err(e) => return ExecResult::failure(2, format!("cat: {e}")),
                };
                if stdin.is_empty() {
                    return ExecResult::with_output(OutputData::text(stdin));
                }
                let trailing_newline = stdin.ends_with('\n');
                let mut numbered = stdin
                    .lines()
                    .enumerate()
                    .map(|(i, line)| format!("{:6}\t{}", i + 1, line))
                    .collect::<Vec<_>>()
                    .join("\n");
                if trailing_newline {
                    numbered.push('\n');
                }
                return ExecResult::with_output(OutputData::text(numbered));
            }
            let stdin = ctx.read_stdin_to_bytes().await.unwrap_or_default();
            return ExecResult::success_text_or_bytes(stdin);
        }
        // Collect paths, expanding any glob patterns
        let paths = match ctx.expand_paths(&args.positional).await {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(1, format!("cat: {}", e)),
        };

        if paths.is_empty() {
            return ExecResult::failure(1, "cat: missing path argument");
        }

        // Binary-capable fast path: a single file with no line numbering. Valid
        // UTF-8 is text as before; anything else becomes a Bytes result (a hex
        // dump in the REPL, a base64 envelope under --json) instead of the old
        // "invalid UTF-8" error. Multi-file / -n stay text-only below — you
        // can't line-number or text-concat binary. See docs/binary-data.md.
        if paths.len() == 1 && !number_lines {
            let resolved = ctx.resolve_path(&paths[0]);
            // Streaming path: when cat feeds a downstream stage, stream the file
            // in bounded chunks rather than reading it whole. This bounds memory
            // and lets downstream consumers (e.g. `head`) early-exit cheaply.
            if let Some(pipe_out) = ctx.pipe_stdout.take() {
                return stream_file_to_pipe(
                    ctx,
                    Path::new(&resolved),
                    &paths[0],
                    pipe_out,
                    ExecContext::STREAM_CHUNK_SIZE,
                )
                .await;
            }
            // Terminal stage (pipe_stdout is None): must materialise the whole
            // file to return it as an ExecResult. Streaming saves nothing here.
            return match ctx.backend.read(Path::new(&resolved), None).await {
                Ok(data) => ExecResult::success_text_or_bytes(data),
                Err(e) => ExecResult::failure(1, format!("cat: {}: {}", paths[0], e)),
            };
        }

        let mut all_content = String::new();
        let mut line_num = 1;
        // Track whether the last file processed ends with a newline so we can
        // restore it after `.lines()` strips it (`.lines()` is newline-agnostic
        // and silently drops the trailing newline — we must add it back).
        let mut last_had_trailing_newline = false;

        for path in paths.iter() {
            let resolved = ctx.resolve_path(path);

            match ctx.backend.read(Path::new(&resolved), None).await {
                Ok(data) => match String::from_utf8(data) {
                    Ok(content) => {
                        if number_lines {
                            last_had_trailing_newline = content.ends_with('\n');
                            for line in content.lines() {
                                if !all_content.is_empty() {
                                    all_content.push('\n');
                                }
                                all_content.push_str(&format!("{:6}\t{}", line_num, line));
                                line_num += 1;
                            }
                        } else {
                            // Byte-verbatim concatenation: never synthesize a
                            // separator. Inserting a newline between files (or
                            // after a file that lacks a trailing one) corrupts
                            // the stream — `cat x y` where x is `a` and y is
                            // `b\n` must be `ab\n`, not `a\nb\n`.
                            all_content.push_str(&content);
                        }
                    }
                    Err(_) => return ExecResult::failure(1, format!("cat: {}: invalid UTF-8", path)),
                },
                Err(e) => return ExecResult::failure(1, format!("cat: {}: {}", path, e)),
            }
        }

        // `.lines()` strips trailing newlines; restore one if the last file
        // ended with `\n` (which it almost always does — this was the bug).
        if number_lines && last_had_trailing_newline {
            all_content.push('\n');
        }

        ExecResult::with_output(OutputData::text(all_content))
    }
}

/// Stream a single file to a pipe in bounded `chunk_size` chunks.
///
/// Exposed as a standalone async function (not a method) so tests can call it
/// directly with an injected `PipeWriter` and a custom chunk size that forces
/// multiple reads on a small file — mirroring the cmp/wc approach.
///
/// Binary integrity: every chunk is written as raw bytes (`write_all(&chunk)`),
/// no UTF-8 decode involved. A non-UTF-8 file arrives byte-for-byte identical
/// at the pipe reader.
///
/// Early exit: if `write_all` returns `Err` (the downstream reader was dropped,
/// e.g. `cat big | head -1`), the loop breaks immediately and returns success —
/// the downstream command already has what it needed.
async fn stream_file_to_pipe(
    ctx: &ExecContext,
    path: &Path,
    display_path: &str,
    mut pipe_out: PipeWriter,
    chunk_size: u64,
) -> ExecResult {
    use tokio::io::AsyncWriteExt;
    let mut offset = 0u64;
    loop {
        let chunk = match ctx
            .backend
            .read(path, Some(ReadRange::bytes(offset, chunk_size)))
            .await
        {
            Ok(c) => c,
            Err(e) => {
                // Shut the pipe down before bailing so the reader sees EOF
                // promptly rather than waiting on `Drop`.
                let _ = pipe_out.shutdown().await;
                return ExecResult::failure(1, format!("cat: {display_path}: {e}"));
            }
        };
        if chunk.is_empty() {
            break; // EOF
        }
        offset += chunk.len() as u64;
        if pipe_out.write_all(&chunk).await.is_err() {
            break; // downstream dropped (e.g. head -1 satisfied) — stop reading
        }
    }
    let _ = pipe_out.shutdown().await;
    ExecResult::success("")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("test.txt"), b"hello world").await.unwrap();
        mem.write(Path::new("dir/nested.txt"), b"nested content").await.unwrap();
        mem.write(Path::new("lines.txt"), b"line1\nline2\nline3").await.unwrap();
        mem.write(Path::new("other.txt"), b"other content").await.unwrap();
        // Non-UTF-8 bytes (0xFF 0xFE … is invalid UTF-8).
        mem.write(Path::new("blob.bin"), &[0u8, 0xff, 0xfe, 0x41, 0x80]).await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_cat_file() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/test.txt".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "hello world");
    }

    #[tokio::test]
    async fn test_cat_nested() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/dir/nested.txt".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "nested content");
    }

    #[tokio::test]
    async fn test_cat_binary_file_yields_bytes() {
        // A single non-UTF-8 file becomes a Bytes result, not an error.
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/blob.bin".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok(), "stderr: {}", result.err);
        assert!(result.is_bytes(), "binary file should produce a Bytes result");
        assert_eq!(result.out_bytes(), Some(&[0u8, 0xff, 0xfe, 0x41, 0x80][..]));
    }

    #[tokio::test]
    async fn test_cat_binary_file_json_envelope() {
        // Under --json, a binary result serializes as the base64 envelope.
        use crate::interpreter::{apply_output_format, OutputFormat};
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/blob.bin".into()));

        let result = Cat.execute(args, &mut ctx).await;
        let formatted = apply_output_format(result, OutputFormat::Json);
        let json: serde_json::Value = serde_json::from_str(&formatted.text_out()).unwrap();
        assert_eq!(json["_type"], "bytes");
        assert_eq!(json["len"], 5);
    }

    #[tokio::test]
    async fn test_cat_not_found() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent.txt".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("not found") || result.err.contains("nonexistent"));
    }

    #[tokio::test]
    async fn test_cat_no_arg_no_stdin() {
        // POSIX: cat with no args and no stdin exits 0 with empty output
        let mut ctx = make_ctx().await;
        let args = ToolArgs::new();

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().is_empty());
    }

    #[tokio::test]
    async fn test_cat_from_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello from stdin".to_string());
        let args = ToolArgs::new();

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "hello from stdin");
    }

    #[tokio::test]
    async fn test_cat_from_stdin_with_line_numbers() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("line1\nline2\nline3".to_string());
        let mut args = ToolArgs::new();
        args.flags.insert("n".to_string());

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("1\tline1"));
        assert!(result.text_out().contains("2\tline2"));
        assert!(result.text_out().contains("3\tline3"));
    }

    #[tokio::test]
    async fn test_cat_multiple_files() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/test.txt".into()));
        args.positional.push(Value::String("/other.txt".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("hello world"));
        assert!(result.text_out().contains("other content"));
    }

    #[tokio::test]
    async fn test_cat_n_line_numbers() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.flags.insert("n".to_string());

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("1\tline1"));
        assert!(result.text_out().contains("2\tline2"));
        assert!(result.text_out().contains("3\tline3"));
    }

    #[tokio::test]
    async fn test_cat_number_line_numbers() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.flags.insert("number".to_string());

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("1\tline1"));
    }

    #[tokio::test]
    async fn test_cat_n_multiple_files_continuous_numbering() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.positional.push(Value::String("/test.txt".into()));
        args.flags.insert("n".to_string());

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        // lines.txt has 3 lines, so test.txt should start at line 4
        assert!(result.text_out().contains("4\thello world"));
    }

    #[tokio::test]
    async fn test_cat_glob() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("*.txt".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should include content from test.txt, lines.txt, other.txt
        assert!(result.text_out().contains("hello world"));
        assert!(result.text_out().contains("other content"));
    }

    #[tokio::test]
    async fn test_cat_glob_no_matches() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("*.nonexistent".into()));

        let result = Cat.execute(args, &mut ctx).await;
        // No matches → missing path
        assert!(!result.ok());
    }

    // ---- Streaming / pipe tests ----
    //
    // RecordingFs wraps a MemoryFs and records every read_range call.
    // (None, None) in the log signals a whole-file read, which the streaming
    // path must never issue.

    /// Recorded (offset, limit) pairs from each `read_range` call.
    type RecordedRanges = Arc<std::sync::Mutex<Vec<(Option<u64>, Option<u64>)>>>;

    struct RecordingFs {
        inner: MemoryFs,
        ranges: RecordedRanges,
    }

    #[async_trait::async_trait]
    impl Filesystem for RecordingFs {
        async fn read(&self, path: &Path) -> std::io::Result<Vec<u8>> {
            // A whole-file read defeats the test; record it as (None, None).
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

    /// Build an ExecContext backed by a RecordingFs, returning the ctx and the
    /// shared range log.  The file at `path` is pre-populated with `data`.
    async fn make_recording_ctx(
        path: &str,
        data: &[u8],
    ) -> (ExecContext, RecordedRanges) {
        let ranges = Arc::new(std::sync::Mutex::new(Vec::new()));
        let rec = RecordingFs {
            inner: MemoryFs::new(),
            ranges: ranges.clone(),
        };
        rec.inner.write(Path::new(path), data).await.unwrap();
        let mut vfs = VfsRouter::new();
        vfs.mount("/", rec);
        (ExecContext::new(Arc::new(vfs)), ranges)
    }

    /// Test (a): bytes arriving on the pipe equal the file contents exactly.
    /// Test (b): reads are issued as bounded ranges (never a whole-file None read).
    ///
    /// We use a 256-byte chunk size (much smaller than STREAM_CHUNK_SIZE) to
    /// force multiple reads on a modestly-sized test file, without needing a
    /// file larger than a few hundred KiB.
    #[tokio::test]
    async fn cat_streams_file_in_bounded_chunks() {
        // Build a file large enough to need multiple 256-byte reads.
        let payload: Vec<u8> = (0u8..=255u8).cycle().take(1000).collect();
        let (ctx, ranges) = make_recording_ctx("big.bin", &payload).await;

        // Set up a pipe pair and call the helper directly with chunk_size=256.
        let (pipe_out, mut pipe_in) = crate::scheduler::pipe_stream(4096);

        // Drain the pipe concurrently while stream_file_to_pipe writes.
        let drain = tokio::spawn(async move {
            use tokio::io::AsyncReadExt;
            let mut collected = Vec::new();
            let mut buf = [0u8; 512];
            loop {
                match pipe_in.read(&mut buf).await {
                    Ok(0) | Err(_) => break,
                    Ok(n) => collected.extend_from_slice(&buf[..n]),
                }
            }
            collected
        });

        let result = stream_file_to_pipe(&ctx, Path::new("/big.bin"), "/big.bin", pipe_out, 256).await;
        assert!(result.ok(), "stream_file_to_pipe failed: {}", result.err);

        let collected = drain.await.expect("drain task panicked");

        // (a) Byte-exact round-trip.
        assert_eq!(
            collected, payload,
            "piped bytes must equal the original file contents exactly"
        );

        // (b) No whole-file reads; all reads carry a limit (bounded).
        let recs = ranges.lock().unwrap();
        assert!(
            recs.len() >= 4,
            "expected multiple bounded reads, got {} reads: {recs:?}",
            recs.len()
        );
        assert!(
            recs.iter().all(|&(_, limit)| limit == Some(256)),
            "every read must be bounded to chunk size 256; recorded {recs:?}"
        );
    }

    /// Binary integrity: a file containing non-UTF-8 bytes streams through
    /// the pipe path byte-for-byte identical — no UTF-8 decode anywhere.
    #[tokio::test]
    async fn cat_streams_binary_file_intact() {
        // Bytes that are not valid UTF-8.
        let payload: Vec<u8> = vec![0x00, 0xff, 0xfe, 0x80, 0x81, 0x82, 0x41, 0x00, 0xff];
        let (ctx, _ranges) = make_recording_ctx("binary.bin", &payload).await;

        let (pipe_out, mut pipe_in) = crate::scheduler::pipe_stream(4096);

        let drain = tokio::spawn(async move {
            use tokio::io::AsyncReadExt;
            let mut collected = Vec::new();
            let mut buf = [0u8; 512];
            loop {
                match pipe_in.read(&mut buf).await {
                    Ok(0) | Err(_) => break,
                    Ok(n) => collected.extend_from_slice(&buf[..n]),
                }
            }
            collected
        });

        // Use a small chunk size to exercise the loop even on a small file.
        let result =
            stream_file_to_pipe(&ctx, Path::new("/binary.bin"), "/binary.bin", pipe_out, 4).await;
        assert!(result.ok(), "stream_file_to_pipe failed: {}", result.err);

        let collected = drain.await.expect("drain task panicked");
        assert_eq!(
            collected, payload,
            "non-UTF-8 bytes must arrive at the pipe reader byte-identical"
        );
    }

    /// Early-exit: drop the pipe reader before draining, confirm the loop
    /// returns success without reading the entire file.
    ///
    /// With chunk_size=256 and a 1000-byte file, an intact reader would trigger
    /// ceil(1000/256)+1 = 5 reads (4 data + 1 EOF probe).  After the reader is
    /// dropped the first write_all fails and the loop breaks, so the read count
    /// stays at 1 (the first chunk read before the write attempt).
    #[tokio::test]
    async fn cat_streaming_early_exit_on_broken_pipe() {
        let payload: Vec<u8> = vec![b'x'; 1000];
        let (ctx, ranges) = make_recording_ctx("big.bin", &payload).await;

        let (pipe_out, pipe_reader) = crate::scheduler::pipe_stream(4096);
        // Drop the reader immediately — the first write_all will fail.
        drop(pipe_reader);

        let result = stream_file_to_pipe(&ctx, Path::new("/big.bin"), "/big.bin", pipe_out, 256).await;
        // Early exit must still return success (not an error).
        assert!(result.ok(), "early-exit path should return success, got: {}", result.err);

        // The loop must have stopped early: we expect very few reads (1 chunk
        // read + the early-exit write failure), not all 5 that a full drain
        // would require.
        let read_count = ranges.lock().unwrap().len();
        assert!(
            read_count < 5,
            "expected early exit to stop reads; got {read_count} reads (expected < 5)"
        );
    }
}
