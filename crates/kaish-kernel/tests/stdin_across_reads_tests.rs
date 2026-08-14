//! Spec tests: stdin is a stream, and reading part of it leaves the rest.
//!
//! `read` consumes one line. Whatever follows that line is still there for the
//! next reader — a second `read`, or a builtin that drains the remainder.
//! Before this, the first `read` drained stdin to EOF and kept only line one,
//! so `read x; read y` bound `x` and then failed with "no input available",
//! and everything after the first line was silently gone.
//!
//! This is the machinery `cmd | while read x; do …; done` needs, and it is
//! GH #199 ("piped stdin isn't shared across statements in one `kaish -c`").
//!
//! Notes for the test author:
//! - `ExecuteOptions::with_stdin` is the frontend seam (`printf … | kaish -c`).
//! - Assert on exact strings: a reader that returned the whole stream where a
//!   line was asked for would still `contain` the right text.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

use kaish_kernel::{pipe_stream_default, ExecuteOptions, Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::repl().with_trash(false)).expect("failed to create kernel")
}

/// The pre-read-buffer seam (`ExecuteOptions::with_stdin`).
async fn run_with_stdin(prog: &str, stdin: &str) -> (String, i64) {
    let result = kernel()
        .execute_with_options(prog, ExecuteOptions::new().with_stdin(stdin))
        .await
        .expect("kernel execute");
    (result.text_out().to_string(), result.code)
}

/// The lazy-pipe seam (`Kernel::execute_with_pipe_stdin`) — what the REPL uses
/// for `printf … | kaish -c …`. A different source from the buffer above, and
/// the one where "leave the rest" is hardest: a pipe cannot be un-read.
async fn run_with_pipe_stdin(prog: &str, stdin: &[u8]) -> (String, i64) {
    let (writer, reader) = pipe_stream_default();
    writer.write_bytes(stdin).await.unwrap();
    drop(writer); // EOF
    let result = kernel()
        .execute_with_pipe_stdin(prog, ExecuteOptions::new(), reader)
        .await
        .expect("kernel execute");
    (result.text_out().to_string(), result.code)
}

/// Both seams at once — an embedder handing a peeked prefix (`with_stdin`)
/// and the live remainder of the same stream (`execute_with_pipe_stdin`) in
/// one call. `ExecContext::read_stdin_to_bytes` treats the two as one
/// stream, buffer first: the wiring must carry both to a pipeline's first
/// stage together, not let the buffer block the pipe from riding along.
async fn run_with_stdin_and_pipe(prog: &str, stdin_prefix: &str, pipe_rest: &[u8]) -> (String, i64) {
    let (writer, reader) = pipe_stream_default();
    writer.write_bytes(pipe_rest).await.unwrap();
    drop(writer); // EOF
    let result = kernel()
        .execute_with_pipe_stdin(prog, ExecuteOptions::new().with_stdin(stdin_prefix), reader)
        .await
        .expect("kernel execute");
    (result.text_out().to_string(), result.code)
}

// ---------------------------------------------------------------------------
// The core rule: one `read` takes one line.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn two_reads_take_two_lines() {
    let (out, code) = run_with_stdin(
        r#"read x; read y; echo "x=$x y=$y""#,
        "alpha\nbravo\n",
    )
    .await;
    assert_eq!(code, 0, "both reads should succeed: {out:?}");
    assert_eq!(out.trim(), "x=alpha y=bravo");
}

#[tokio::test]
async fn three_reads_take_three_lines_in_order() {
    let (out, code) = run_with_stdin(
        r#"read a; read b; read c; echo "$a-$b-$c""#,
        "one\ntwo\nthree\n",
    )
    .await;
    assert_eq!(code, 0, "all three reads should succeed: {out:?}");
    assert_eq!(out.trim(), "one-two-three");
}

#[tokio::test]
async fn a_single_read_takes_only_the_first_line() {
    let (out, code) = run_with_stdin(r#"read x; echo "got=$x""#, "first\nsecond\n").await;
    assert_eq!(code, 0);
    assert_eq!(
        out.trim(),
        "got=first",
        "`read` binds one line, not the whole stream"
    );
}

// ---------------------------------------------------------------------------
// What `read` leaves behind is still readable by anything else.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn a_later_builtin_drains_what_read_left() {
    let (out, code) = run_with_stdin("read x; cat", "keep\nrest1\nrest2\n").await;
    assert_eq!(code, 0, "cat should read the remainder: {out:?}");
    assert_eq!(
        out, "rest1\nrest2\n",
        "cat must see everything after the line `read` took, and nothing before it"
    );
}

#[tokio::test]
async fn read_then_wc_counts_only_the_remainder() {
    let (out, code) = run_with_stdin("read x; wc -l", "one\ntwo\nthree\n").await;
    assert_eq!(code, 0, "wc should succeed: {out:?}");
    assert_eq!(out.trim(), "2", "one line consumed, two remain");
}

// ---------------------------------------------------------------------------
// Exhaustion is a loud, ordinary failure — not a silent empty binding.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn reading_past_the_end_fails_rather_than_binding_empty() {
    // No trailing `echo`: a succeeding last statement would reset the exit
    // code and hide the failing `read`.
    let (out, code) = run_with_stdin("read x; read y", "only\n").await;
    assert_ne!(code, 0, "the second read has no line to take: {out:?}");
}

#[tokio::test]
async fn a_final_line_without_a_trailing_newline_still_reads() {
    let (out, code) = run_with_stdin(r#"read x; read y; echo "x=$x y=$y""#, "a\nb").await;
    assert_eq!(code, 0, "an unterminated last line is still a line: {out:?}");
    assert_eq!(out.trim(), "x=a y=b");
}

// ---------------------------------------------------------------------------
// Draining builtins are unaffected: with no prior `read`, they see everything.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn a_draining_builtin_alone_still_sees_the_whole_stream() {
    let (out, code) = run_with_stdin("cat", "a\nb\nc\n").await;
    assert_eq!(code, 0);
    assert_eq!(out, "a\nb\nc\n");
}

// ---------------------------------------------------------------------------
// The same rules over the lazy pipe — a separate source, and the one the REPL
// actually uses. GH #199 is exactly this: `printf … | kaish -c 'a; b'`.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn two_reads_take_two_lines_from_a_pipe() {
    let (out, code) =
        run_with_pipe_stdin(r#"read x; read y; echo "x=$x y=$y""#, b"alpha\nbravo\n").await;
    assert_eq!(code, 0, "both reads should succeed: {out:?}");
    assert_eq!(out.trim(), "x=alpha y=bravo");
}

#[tokio::test]
async fn a_later_builtin_drains_what_read_left_on_a_pipe() {
    let (out, code) = run_with_pipe_stdin("read x; cat", b"keep\nrest1\nrest2\n").await;
    assert_eq!(code, 0, "cat should read the remainder: {out:?}");
    assert_eq!(out, "rest1\nrest2\n");
}

#[tokio::test]
async fn a_read_spanning_more_than_one_pipe_chunk_still_takes_one_line() {
    // The line reader pulls 8 KiB at a time. A first line longer than one
    // chunk must still come back whole, with the remainder intact — otherwise
    // the chunk boundary, not the newline, would decide where a line ends.
    let long = "x".repeat(20_000);
    let input = format!("{long}\nsecond\n");
    let (out, code) =
        run_with_pipe_stdin(r#"read a; read b; echo "len=${#a} b=$b""#, input.as_bytes()).await;
    assert_eq!(code, 0, "both reads should succeed: {out:?}");
    assert_eq!(out.trim(), "len=20000 b=second");
}

// The case that matters most, and the one small inputs hide: a SHORT first
// line leaves bytes unread *in the pipe*, not merely in the buffer. Every test
// above fits in one 8 KiB chunk, so the first `read` happens to drain the whole
// stream and nothing is left in the reader to lose. These do not.

#[tokio::test]
async fn a_short_first_line_does_not_strand_the_rest_of_the_pipe() {
    let rest = "x".repeat(20_000);
    let input = format!("keep\n{rest}\n");
    let (out, code) = run_with_pipe_stdin("read x; wc -c", input.as_bytes()).await;
    assert_eq!(code, 0, "wc should succeed: {out:?}");
    // 20_000 x's + the newline that ended them.
    assert_eq!(
        out.trim(),
        "20001",
        "everything after the first line must survive, including the part still \
         unread in the pipe when `read` returned"
    );
}

#[tokio::test]
async fn a_short_first_line_then_cat_delivers_every_remaining_byte() {
    let rest = "y".repeat(20_000);
    let input = format!("head\n{rest}\n");
    let (out, code) = run_with_pipe_stdin("read x; cat", input.as_bytes()).await;
    assert_eq!(code, 0, "cat should succeed");
    assert_eq!(out.len(), 20_001, "cat saw {} bytes, want 20001", out.len());
    assert!(out.starts_with("yyy"), "and it must start where `read` stopped");
}

// A builtin that streams the pipe directly must not skip the buffered front of
// the stream. `head` takes `pipe_stdin` for early termination
// (`seq 1 1000000 | head -5`); after a `read` the first lines are in the buffer,
// not the pipe, so it has to fall back to the joined path.

#[tokio::test]
async fn head_after_a_read_sees_the_lines_read_left_behind() {
    let (out, code) = run_with_pipe_stdin("read x; head -n 2", b"a\nb\nc\nd\n").await;
    assert_eq!(code, 0, "head should succeed: {out:?}");
    assert_eq!(
        out, "b\nc\n",
        "head must resume where `read` stopped, not skip to the pipe's tail"
    );
}

#[tokio::test]
async fn head_still_terminates_early_when_nothing_was_read_first() {
    // The guard above must not cost `head` its streaming fast path.
    let kernel = kernel();
    let result = kernel
        .execute("seq 1 100000 | head -n 3")
        .await
        .expect("kernel execute");
    assert_eq!(result.text_out(), "1\n2\n3\n");
}

#[tokio::test]
async fn a_pipe_read_does_not_block_a_command_that_never_reads() {
    // The lazy-stdin guarantee must survive the line reader: an open, silent
    // pipe plus a command that never reads stdin returns promptly.
    let (writer, reader) = pipe_stream_default();
    let kernel = kernel();
    let fut = kernel.execute_with_pipe_stdin("echo hi", ExecuteOptions::new(), reader);
    let result = tokio::time::timeout(std::time::Duration::from_secs(5), fut)
        .await
        .expect("echo must not block on unread, never-closed stdin")
        .expect("kernel execute");
    assert_eq!(result.text_out(), "hi\n");
    drop(writer);
}

// ---------------------------------------------------------------------------
// A pipeline does not swallow the session's stdin. Stage 0 is handed the
// stream; whatever it does not consume belongs to the next statement, exactly
// as in bash. The `seq` case is the sharp one — that stage never reads stdin
// at all, so dropping the stream would be pure loss.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn a_pipeline_returns_the_stdin_its_first_stage_did_not_consume() {
    // bash: `printf 'a\nb\nc\n' | { read x | cat; cat; }` prints b, c.
    let (out, code) = run_with_pipe_stdin("read x | cat; cat", b"a\nb\nc\n").await;
    assert_eq!(code, 0, "the trailing cat should succeed: {out:?}");
    assert_eq!(out, "b\nc\n");
}

#[tokio::test]
async fn a_pipeline_whose_first_stage_never_reads_stdin_leaves_it_all() {
    // bash: `printf 'a\nb\nc\n' | { seq 1 2 | cat; cat; }` prints 1,2 then a,b,c.
    let (out, code) = run_with_pipe_stdin("seq 1 2 | cat; cat", b"a\nb\nc\n").await;
    assert_eq!(code, 0, "the trailing cat should succeed: {out:?}");
    assert_eq!(
        out, "1\n2\na\nb\nc\n",
        "a stage that never reads stdin must not consume the session's stream"
    );
}

#[tokio::test]
async fn a_buffered_stdin_survives_a_pipeline_too() {
    let (out, code) = run_with_stdin("seq 1 2 | cat; cat", "a\nb\n").await;
    assert_eq!(code, 0, "the trailing cat should succeed: {out:?}");
    assert_eq!(out, "1\n2\na\nb\n");
}

#[tokio::test]
async fn a_pipeline_stage_with_its_own_redirect_leaves_the_session_stream_alone() {
    // `read x <<< "h"` takes its input from the here-string, so the session's
    // stdin was never handed to the stage and is still the parent's. Returning
    // the redirect's leftover over it would lose the stream AND substitute the
    // wrong bytes. bash prints the session's a/b/c here.
    let (out, code) = run_with_pipe_stdin(r#"read x <<< "h" | cat; cat"#, b"a\nb\nc\n").await;
    assert_eq!(code, 0, "the trailing cat should succeed: {out:?}");
    assert_eq!(out, "a\nb\nc\n");
}

#[tokio::test]
async fn a_pipeline_read_with_a_short_line_returns_the_unread_pipe_too() {
    // The remainder after a partial read lives in two places at once: the
    // over-read bytes in the buffer, and the rest still in the pipe. A short
    // first line is what forces both to be non-empty — with a small input the
    // first read drains everything and a dropped `pipe_stdin` goes unnoticed.
    let rest = "y".repeat(20_000);
    let input = format!("keep\n{rest}\n");
    let (out, code) = run_with_pipe_stdin("read x | cat; cat", input.as_bytes()).await;
    assert_eq!(code, 0, "the trailing cat should succeed");
    assert_eq!(out.len(), 20_001, "trailing cat saw {} bytes, want 20001", out.len());
}

#[tokio::test]
async fn a_pipeline_that_drains_stdin_leaves_nothing_and_repeats_nothing() {
    // The duplication guard: `cat | cat` consumes the whole stream, so the
    // trailing `cat` must see nothing — and the bytes must appear exactly once.
    let (out, code) = run_with_pipe_stdin("cat | cat; cat", b"a\nb\nc\n").await;
    assert_eq!(code, 0);
    assert_eq!(out, "a\nb\nc\n", "each byte exactly once");
}

#[tokio::test]
async fn a_pipeline_carries_a_buffered_prefix_and_its_live_pipe_together() {
    // An embedder that already peeked a prefix off an open process stdin and
    // wants to forward the rest lazily seeds both `with_stdin` (the prefix)
    // and `execute_with_pipe_stdin` (the remainder) on the same call. Stage 0
    // must see the whole combined stream, not just the buffered prefix — a
    // filter that only sees the prefix silently misses matches that live in
    // the live-pipe half, and those bytes leak past the filter to whatever
    // runs next instead of being filtered at all.
    let stdin_prefix = "aaa\n";
    let pipe_rest = b"bbb\npipeword\nccc\n";
    let (out, code) =
        run_with_stdin_and_pipe("grep pipeword | cat; cat", stdin_prefix, pipe_rest).await;
    assert_eq!(code, 0, "the trailing cat should succeed: {out:?}");
    assert_eq!(
        out, "pipeword\n",
        "grep must see (and filter) the whole stream, not just the buffered prefix; \
         a wiring bug that drops the live pipe from stage 0 would instead leak every \
         unfiltered line from the pipe out through the trailing cat: {out:?}"
    );
}
