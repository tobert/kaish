//! cmp — compare two files byte by byte.
//!
//! The binary-aware sibling of `diff`: reads both operands as raw bytes (never
//! UTF-8), so it works on any content. Exit status follows POSIX `cmp`:
//! 0 = identical, 1 = differ, 2 = error. With `-s` it is silent (status only).
//! See `docs/binary-data.md`.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::backend::ReadRange;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

/// cmp tool.
pub struct Cmp;

/// clap-derived argv layer for cmp.
#[derive(Parser, Debug)]
#[command(name = "cmp", about = "Compare two files byte by byte")]
struct CmpArgs {
    /// Silent — no output, only the exit status (-s).
    #[arg(short = 's', long = "silent")]
    silent: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// The two files to compare (use `-` for stdin in one slot).
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Cmp {
    fn name(&self) -> &str {
        "cmp"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &CmpArgs::command(),
            "cmp",
            "Compare two files byte by byte",
            [
                ("Compare two files", "cmp a.bin b.bin"),
                ("Silent (exit status only)", "cmp -s a.bin b.bin"),
                ("Compare stdin to a file", "dd if=/dev/urandom bs=16 count=1 | cmp - saved.bin"),
            ],
        )
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        args.flagify_bool_named(&self.schema());
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("cmp: {e}")),
        };
        let parsed = match CmpArgs::try_parse_from(
            std::iter::once("cmp".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("cmp: {e}")),
        };
        parsed.global.apply(ctx);
        let silent = parsed.silent;

        if parsed.paths.len() != 2 {
            return ExecResult::failure(2, "cmp: expected two file operands");
        }
        // Read the operand VALUES off `args.positional`, not `parsed.paths` —
        // `parsed.paths` came through `to_argv()`'s lossy re-serialization
        // (a `Value::Bytes` operand would already be the `[binary: N bytes]`
        // placeholder by the time clap saw it), so a binary path would
        // otherwise silently name a nonexistent file instead of erroring.
        let (name1, name2) = match crate::interpreter::values_to_text_sink_named(&args.positional, "a path") {
            Ok(paths) if paths.len() == 2 => (paths[0].clone(), paths[1].clone()),
            Ok(_) => return ExecResult::failure(2, "cmp: expected two file operands"),
            Err(e) => return ExecResult::failure(2, format!("cmp: {e}")),
        };
        if name1 == "-" && name2 == "-" {
            return ExecResult::failure(2, "cmp: only one operand may be '-' (stdin)");
        }

        // Fast-path: lockstep streaming when both operands are real file paths.
        // A pipe can't be seeked, so if either operand is `-` we fall back to
        // the whole-buffer path below — stdin is already buffered and small.
        // This split is legitimate (streaming fast-path + general fallback), not
        // a dual-representation smell: the two paths must stay semantically
        // identical, which the parity tests in `#[cfg(test)]` enforce.
        if name1 != "-" && name2 != "-" {
            let path1 = ctx.resolve_path(&name1);
            let path2 = ctx.resolve_path(&name2);
            return cmp_lockstep(
                ctx,
                Path::new(&path1),
                Path::new(&path2),
                &name1,
                &name2,
                silent,
                ExecContext::STREAM_CHUNK_SIZE,
            )
            .await;
        }

        // Fallback: whole-buffer path for stdin (`-`) operands.
        let a = match read_operand(ctx, &name1).await {
            Ok(d) => d,
            Err(e) => return ExecResult::failure(2, format!("cmp: {name1}: {e}")),
        };
        let b = match read_operand(ctx, &name2).await {
            Ok(d) => d,
            Err(e) => return ExecResult::failure(2, format!("cmp: {name2}: {e}")),
        };

        cmp_whole_buffer(&a, &b, &name1, &name2, silent)
    }
}

/// Lockstep streaming comparison for two real file paths.
///
/// Reads both files in parallel `chunk_size` windows, advancing a shared offset.
/// Stops at the first differing byte (EARLY EXIT — does not read the rest of
/// either file). The `chunk_size` parameter is exposed so tests can force tiny
/// chunks (1, 2, 3 bytes) to exercise chunk-boundary seams while production
/// always passes `ExecContext::STREAM_CHUNK_SIZE`.
async fn cmp_lockstep(
    ctx: &ExecContext,
    path_a: &Path,
    path_b: &Path,
    name1: &str,
    name2: &str,
    silent: bool,
    chunk_size: u64,
) -> ExecResult {
    let mut offset = 0u64;
    // Running newline count in file A, used to compute the "line N" in the
    // differing-byte message. Tracks only the bytes *before* the first difference.
    let mut newlines_a: u64 = 0;

    loop {
        let chunk_a = match ctx
            .backend
            .read(path_a, Some(ReadRange::bytes(offset, chunk_size)))
            .await
        {
            Ok(c) => c,
            Err(e) => return ExecResult::failure(2, format!("cmp: {name1}: {e}")),
        };
        let chunk_b = match ctx
            .backend
            .read(path_b, Some(ReadRange::bytes(offset, chunk_size)))
            .await
        {
            Ok(c) => c,
            Err(e) => return ExecResult::failure(2, format!("cmp: {name2}: {e}")),
        };

        // Both chunks empty ⇒ EOF reached simultaneously ⇒ files are identical.
        if chunk_a.is_empty() && chunk_b.is_empty() {
            return ExecResult::success("");
        }

        // Compare the common prefix of the two chunks.
        let common = chunk_a.len().min(chunk_b.len());
        if let Some(diff_pos) = (0..common).find(|&i| chunk_a[i] != chunk_b[i]) {
            // Early exit: stop here, do not read any further.
            if silent {
                return ExecResult::failure(1, "");
            }
            // Count newlines in A up to (not including) the differing byte.
            newlines_a += chunk_a[..diff_pos].iter().filter(|&&c| c == b'\n').count() as u64;
            let byte_number = offset + diff_pos as u64 + 1; // 1-based
            let line_number = newlines_a + 1;
            let msg = format!(
                "{name1} {name2} differ: byte {byte_number}, line {line_number}"
            );
            return ExecResult::success(msg).with_code(1);
        }

        // Common region matched. If the chunk lengths differ, the shorter file
        // hit EOF while the longer file still had bytes — report EOF on shorter.
        if chunk_a.len() != chunk_b.len() {
            if silent {
                return ExecResult::failure(1, "");
            }
            let (shorter, at) = if chunk_a.len() < chunk_b.len() {
                (name1, offset + chunk_a.len() as u64)
            } else {
                (name2, offset + chunk_b.len() as u64)
            };
            let msg = format!("cmp: EOF on {shorter} after byte {at}");
            return ExecResult::success(msg).with_code(1);
        }

        // Both chunks are the same length and identical — advance to the next window.
        // Count newlines in A's chunk for the running line tracker.
        newlines_a += chunk_a.iter().filter(|&&c| c == b'\n').count() as u64;
        offset += chunk_a.len() as u64;
    }
}

/// Whole-buffer comparison. Used only when one operand is `-` (stdin).
///
/// Preserved verbatim from the original implementation so the fallback path
/// continues to pass the existing tests. The parity tests compare this against
/// `cmp_lockstep` for every file-pair case to ensure they stay in sync.
fn cmp_whole_buffer(a: &[u8], b: &[u8], name1: &str, name2: &str, silent: bool) -> ExecResult {
    // First differing byte (1-based), with its line number (newlines before it + 1).
    let common = a.len().min(b.len());
    if let Some(offset) = (0..common).find(|&i| a[i] != b[i]) {
        if silent {
            return ExecResult::failure(1, "");
        }
        let line = a[..offset].iter().filter(|&&c| c == b'\n').count() + 1;
        let msg = format!("{name1} {name2} differ: byte {}, line {}", offset + 1, line);
        return ExecResult::success(msg).with_code(1);
    }

    // Common prefix matches; a length difference is EOF on the shorter file.
    if a.len() != b.len() {
        if silent {
            return ExecResult::failure(1, "");
        }
        let (shorter, at) = if a.len() < b.len() {
            (name1, a.len())
        } else {
            (name2, b.len())
        };
        let msg = format!("cmp: EOF on {shorter} after byte {at}");
        return ExecResult::success(msg).with_code(1);
    }

    // Identical → exit 0, no output.
    ExecResult::success("")
}

/// Read an operand as raw bytes — a path through the VFS, or stdin for `-`.
async fn read_operand(ctx: &mut ExecContext, name: &str) -> std::io::Result<Vec<u8>> {
    if name == "-" {
        Ok(ctx.read_stdin_to_bytes().await.unwrap_or_default())
    } else {
        let resolved = ctx.resolve_path(name);
        ctx.backend
            .read(Path::new(&resolved), None)
            .await
            .map_err(std::io::Error::other)
    }
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
        mem.write(Path::new("a.bin"), &[0u8, 1, 2, 3]).await.unwrap();
        mem.write(Path::new("a2.bin"), &[0u8, 1, 2, 3]).await.unwrap();
        mem.write(Path::new("b.bin"), &[0u8, 1, 9, 3]).await.unwrap();
        mem.write(Path::new("short.bin"), &[0u8, 1]).await.unwrap();
        mem.write(Path::new("lines.txt"), b"ab\ncd").await.unwrap();
        mem.write(Path::new("lines2.txt"), b"ab\ncX").await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    fn args(ops: &[&str]) -> ToolArgs {
        let mut a = ToolArgs::new();
        for o in ops {
            a.positional.push(Value::String((*o).to_string()));
        }
        a
    }

    // ---- Existing tests (must all still pass) ----

    #[tokio::test]
    async fn identical_files_exit_zero() {
        let mut ctx = make_ctx().await;
        let r = Cmp.execute(args(&["/a.bin", "/a2.bin"]), &mut ctx).await;
        assert!(r.ok(), "stderr: {}", r.err);
        assert!(r.text_out().is_empty());
    }

    #[tokio::test]
    async fn differing_byte_reported() {
        let mut ctx = make_ctx().await;
        let r = Cmp.execute(args(&["/a.bin", "/b.bin"]), &mut ctx).await;
        assert_eq!(r.code, 1);
        // First difference is at byte index 2 → byte 3, line 1.
        assert!(r.text_out().contains("byte 3, line 1"), "out: {}", r.text_out());
    }

    #[tokio::test]
    async fn line_number_counts_newlines() {
        let mut ctx = make_ctx().await;
        let r = Cmp.execute(args(&["/lines.txt", "/lines2.txt"]), &mut ctx).await;
        assert_eq!(r.code, 1);
        // Differ at the 5th byte ('d' vs 'X'), which is on line 2.
        assert!(r.text_out().contains("byte 5, line 2"), "out: {}", r.text_out());
    }

    #[tokio::test]
    async fn eof_on_shorter() {
        let mut ctx = make_ctx().await;
        let r = Cmp.execute(args(&["/short.bin", "/a.bin"]), &mut ctx).await;
        assert_eq!(r.code, 1);
        assert!(r.text_out().contains("EOF on /short.bin"), "out: {}", r.text_out());
    }

    #[tokio::test]
    async fn silent_suppresses_output() {
        let mut ctx = make_ctx().await;
        let mut a = args(&["/a.bin", "/b.bin"]);
        a.named.insert("silent".to_string(), Value::Bool(true));
        let r = Cmp.execute(a, &mut ctx).await;
        assert_eq!(r.code, 1);
        assert!(r.text_out().is_empty());
        assert!(r.err.is_empty());
    }

    #[tokio::test]
    async fn missing_operand_errors() {
        let mut ctx = make_ctx().await;
        let r = Cmp.execute(args(&["/a.bin"]), &mut ctx).await;
        assert_eq!(r.code, 2);
    }

    #[tokio::test]
    async fn missing_file_is_error_exit_2() {
        let mut ctx = make_ctx().await;
        let r = Cmp.execute(args(&["/a.bin", "/nope.bin"]), &mut ctx).await;
        assert_eq!(r.code, 2);
    }

    // ---- Parity tests: streaming result == whole-buffer reference ----
    //
    // For each file pair we drive `cmp_lockstep` at several tiny chunk sizes
    // (including 1, 2, 3 bytes) to exercise every possible chunk-boundary
    // alignment, and compare its (code, message) against `cmp_whole_buffer`.
    // An earlier draft that only compared the streamer against itself masked
    // a divergence; comparing against the whole-buffer reference ensures both
    // paths stay in lock-step.

    struct PairResult {
        code: i64,
        text: String,
    }

    fn whole_buffer_result(a: &[u8], b: &[u8], name1: &str, name2: &str) -> PairResult {
        let r = cmp_whole_buffer(a, b, name1, name2, false);
        PairResult {
            code: r.code,
            text: r.text_out().to_string(),
        }
    }

    async fn lockstep_result(
        a: &[u8],
        b: &[u8],
        name1: &str,
        name2: &str,
        chunk_size: u64,
    ) -> PairResult {
        // Build a fresh MemoryFs for each call so paths don't collide.
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("a"), a).await.unwrap();
        mem.write(Path::new("b"), b).await.unwrap();
        vfs.mount("/", mem);
        let ctx = ExecContext::new(Arc::new(vfs));

        let r = cmp_lockstep(
            &ctx,
            Path::new("/a"),
            Path::new("/b"),
            name1,
            name2,
            false,
            chunk_size,
        )
        .await;
        PairResult {
            code: r.code, // i64
            text: r.text_out().to_string(),
        }
    }

    // File pairs used across the parity and chunk-seam tests.
    fn file_pairs() -> Vec<(&'static str, Vec<u8>, Vec<u8>)> {
        vec![
            // identical
            ("identical", b"hello world".to_vec(), b"hello world".to_vec()),
            // differ at byte 1 (very first byte)
            ("differ_first", b"xhello".to_vec(), b"yhello".to_vec()),
            // differ at byte 4 (early but not first)
            ("differ_early", b"abcX".to_vec(), b"abcY".to_vec()),
            // differ in a later chunk (byte > chunk_size when chunk=3)
            ("differ_later", b"aabbccDDee".to_vec(), b"aabbccEEee".to_vec()),
            // file B is a prefix of file A (EOF on b)
            ("prefix_b_shorter", b"abcdef".to_vec(), b"abc".to_vec()),
            // file A is a prefix of file B (EOF on a)
            ("prefix_a_shorter", b"abc".to_vec(), b"abcdef".to_vec()),
            // files with newlines — line number tracking
            ("newlines_same_line", b"ab\ncdXef".to_vec(), b"ab\ncdYef".to_vec()),
            // difference falls exactly on a chunk boundary (byte 3, chunks of 3)
            (
                "diff_on_boundary",
                b"abcXefg".to_vec(),
                b"abcYefg".to_vec(),
            ),
            // empty files (identical)
            ("both_empty", b"".to_vec(), b"".to_vec()),
            // one empty, one not
            ("one_empty", b"".to_vec(), b"x".to_vec()),
            // binary content (not valid UTF-8)
            ("binary", vec![0u8, 1, 2, 3, 4], vec![0u8, 1, 2, 255, 4]),
        ]
    }

    #[tokio::test]
    async fn streaming_parity_with_whole_buffer() {
        for (label, a, b) in file_pairs() {
            let want = whole_buffer_result(&a, &b, "/a", "/b");
            for &chunk_size in &[1u64, 2, 3, 7, 13] {
                let got = lockstep_result(&a, &b, "/a", "/b", chunk_size).await;
                assert_eq!(
                    got.code, want.code,
                    "{label} chunk={chunk_size}: exit code mismatch (got {}, want {})",
                    got.code,
                    want.code
                );
                assert_eq!(
                    got.text, want.text,
                    "{label} chunk={chunk_size}: message mismatch\n  got:  {:?}\n  want: {:?}",
                    got.text, want.text
                );
            }
        }
    }

    // ---- Line-number tracking across chunk seams ----
    //
    // Verify that the running newline counter survives chunk boundaries correctly.
    // A difference on line 3 must report "line 3" regardless of where the chunk
    // edges fall.

    #[tokio::test]
    async fn line_number_survives_chunk_seams() {
        // "aaa\nbbb\ncXd" vs "aaa\nbbb\ncYd" — difference at byte 10, line 3.
        let a = b"aaa\nbbb\ncXd".to_vec();
        let b = b"aaa\nbbb\ncYd".to_vec();
        let want = whole_buffer_result(&a, &b, "/f1", "/f2");
        assert!(want.text.contains("line 3"), "reference said: {}", want.text);

        for &chunk_size in &[1u64, 2, 3, 4, 5] {
            let got = lockstep_result(&a, &b, "/f1", "/f2", chunk_size).await;
            assert_eq!(
                got.code, want.code,
                "chunk={chunk_size}: exit code mismatch"
            );
            assert_eq!(
                got.text, want.text,
                "chunk={chunk_size}: message mismatch\n  got:  {:?}\n  want: {:?}",
                got.text, want.text
            );
        }
    }

    // ---- RecordingFs: prove bounded reads and early exit ----
    //
    // The RecordingFs wraps a MemoryFs and records every `read_range` call.
    // We assert:
    //   (a) The streaming path never issues a whole-file `read(None)`.
    //   (b) All reads are bounded to the chunk size.
    //   (c) On an early difference (byte 1 differs), we stop after ONE chunk
    //       per file — not after reading every chunk.

    /// Recorded (offset, limit) pairs from each `read_range` call.
    type RecordedRanges = Arc<std::sync::Mutex<Vec<(Option<u64>, Option<u64>)>>>;

    struct RecordingFs {
        inner: MemoryFs,
        /// (offset, limit) pairs for each `read_range` call.
        /// `(None, None)` signals a whole-file `read(None)`.
        ranges: RecordedRanges,
    }

    #[async_trait::async_trait]
    impl Filesystem for RecordingFs {
        async fn read(&self, path: &Path) -> std::io::Result<Vec<u8>> {
            // A whole-file read here would defeat the test; record it as
            // (None, None) so the assertion below can catch it.
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

    /// Build an ExecContext backed by a RecordingFs and return the shared range log.
    async fn make_recording_ctx(
        file_a: &[u8],
        file_b: &[u8],
    ) -> (ExecContext, RecordedRanges) {
        let ranges = Arc::new(std::sync::Mutex::new(Vec::new()));
        let rec = RecordingFs {
            inner: MemoryFs::new(),
            ranges: ranges.clone(),
        };
        rec.inner.write(Path::new("a"), file_a).await.unwrap();
        rec.inner.write(Path::new("b"), file_b).await.unwrap();
        let mut vfs = VfsRouter::new();
        vfs.mount("/", rec);
        (ExecContext::new(Arc::new(vfs)), ranges)
    }

    #[tokio::test]
    async fn cmp_streams_in_bounded_chunks() {
        // Two 1000-byte identical files: the loop must issue multiple chunk reads
        // and must never issue a whole-file read.
        let payload = vec![b'x'; 1000];
        let (ctx, ranges) = make_recording_ctx(&payload, &payload).await;

        let result = cmp_lockstep(
            &ctx,
            Path::new("/a"),
            Path::new("/b"),
            "/a",
            "/b",
            false,
            256,
        )
        .await;
        assert_eq!(result.code, 0, "files are identical");

        let recs = ranges.lock().unwrap();
        // 1000 bytes / 256 → 4 data reads + 1 terminating empty read = 5 per file → 10 total.
        assert!(
            recs.len() >= 8,
            "expected multiple bounded reads, got {} reads: {recs:?}",
            recs.len()
        );
        // Never a whole-file read (None, None) — all reads must carry a limit.
        // All reads bounded to the chunk size.
        assert!(
            recs.iter().all(|&(_, limit)| limit == Some(256)),
            "every read must be bounded to chunk size 256; recorded {recs:?}"
        );
    }

    #[tokio::test]
    async fn cmp_early_exit_on_first_byte_difference() {
        // Two 1000-byte files where the FIRST byte differs. The streaming path
        // must stop after reading exactly one chunk from each file (plus the
        // termination probe reads at offset 0 and offset chunk_size would not be
        // reached). In total we expect exactly 2 reads: one for file A at offset 0
        // and one for file B at offset 0 — then the loop exits immediately.
        let mut file_a = vec![b'x'; 1000];
        let file_b = vec![b'y'; 1000];
        file_a[0] = b'X'; // differs at byte 0

        let (ctx, ranges) = make_recording_ctx(&file_a, &file_b).await;

        let result = cmp_lockstep(
            &ctx,
            Path::new("/a"),
            Path::new("/b"),
            "/a",
            "/b",
            false,
            256, // chunk_size — 1000 bytes would take 4+ chunks if not stopped
        )
        .await;
        assert_eq!(result.code, 1, "files differ");
        assert!(
            result.text_out().contains("byte 1"),
            "expected byte 1 report, got: {}",
            result.text_out()
        );

        let recs = ranges.lock().unwrap();
        // Exactly 2 reads: one chunk from each file at offset 0.
        // The loop must NOT have read any further.
        assert_eq!(
            recs.len(),
            2,
            "early exit must stop after 2 reads (1 per file); got {} reads: {recs:?}",
            recs.len()
        );
        // Both reads are at offset 0.
        assert!(
            recs.iter().all(|&(offset, _)| offset == Some(0)),
            "both reads must be at offset 0; recorded {recs:?}"
        );
    }

    #[tokio::test]
    async fn cmp_no_whole_file_reads() {
        // Ensure that even for two identical files, the streaming path never
        // issues a `read(None)` whole-file request — all reads must be ranged.
        let payload = b"hello world, no whole-file reads here".to_vec();
        let (ctx, ranges) = make_recording_ctx(&payload, &payload).await;

        cmp_lockstep(
            &ctx,
            Path::new("/a"),
            Path::new("/b"),
            "/a",
            "/b",
            false,
            ExecContext::STREAM_CHUNK_SIZE,
        )
        .await;

        let recs = ranges.lock().unwrap();
        // (None, None) would indicate a whole-file read — there must be none.
        assert!(
            !recs.contains(&(None, None)),
            "whole-file read(None) detected; recorded {recs:?}"
        );
        // All reads must carry a limit (bounded).
        assert!(
            recs.iter().all(|&(_, limit)| limit.is_some()),
            "every read must be bounded; recorded {recs:?}"
        );
    }
}
