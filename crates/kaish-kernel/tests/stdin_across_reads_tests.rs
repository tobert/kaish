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
