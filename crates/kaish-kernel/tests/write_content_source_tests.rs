//! `write` must never truncate a file it was given no content for.
//!
//! `write FILE` with no content operand used to write zero bytes and exit 0
//! whenever the process's stdin was an exhausted pipe — which is what `kaish -c`
//! hands the kernel when stdin is `/dev/null`, a closed fd, or an empty pipe.
//! An agent typo (`write notes.md` instead of `write notes.md "$text"`) emptied
//! the file and reported success, and `set -o trash` did not cover it: the file
//! is truncated in place, so there is no delete for trash to intercept.
//!
//! The distinction that matters is who supplied the zero bytes. An upstream
//! pipeline stage or a redirect supplied them deliberately — `printf '' | write
//! f` is a truncate request and stays exit 0. The session's ambient stdin
//! supplied nothing at all, and that is an error.
//!
//! The exit code is not the assertion that matters here; the file's bytes are.
//! An error that still truncates is no better than the bug.

// Test-fixture code: unwrap/expect on known-good setup is the idiom.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::kernel_at;
use kaish_kernel::{pipe_stream_default, ExecuteOptions};
use std::fs;
use tempfile::tempdir;

const ORIGINAL: &[u8] = b"[IMPORTANT DATA]\n";

/// An exhausted pipe — the reader sees EOF immediately. This is exactly what
/// the `-c` frontend's stdin bridge produces when the process's stdin is
/// `/dev/null`, a closed fd, or an empty pipe, and it is the shape that made
/// `read_stdin_to_bytes` answer `Ok(Some(vec![]))` instead of `Ok(None)`.
fn exhausted_pipe() -> kaish_kernel::PipeReader {
    let (writer, reader) = pipe_stream_default();
    drop(writer);
    reader
}

/// The regression. Ambient stdin that yields nothing is a missing operand, not
/// empty content — and the file must still hold every original byte.
#[tokio::test]
async fn write_with_exhausted_session_stdin_refuses_and_leaves_the_file_intact() {
    let dir = tempdir().unwrap();
    let target = dir.path().join("notes.md");
    fs::write(&target, ORIGINAL).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel
        .execute_with_pipe_stdin("write notes.md", ExecuteOptions::new(), exhausted_pipe())
        .await
        .unwrap();

    // Bytes first: the data is the contract. A refusal that still truncated
    // would pass an exit-code check and fail this one, which is the failure
    // worth reading.
    assert_eq!(
        fs::read(&target).unwrap(),
        ORIGINAL,
        "the file must be byte-identical — an error that still truncates is the same bug"
    );
    assert_eq!(result.code, 1, "expected a refusal, got: {}", result.err);
}

/// Same refusal with no stdin plumbing at all (the embedded path, and the
/// interactive REPL, where the bridge is never spawned because stdin is a TTY).
/// This arm was always correct; it is pinned so it stays that way.
#[tokio::test]
async fn write_with_no_stdin_at_all_refuses_and_leaves_the_file_intact() {
    let dir = tempdir().unwrap();
    let target = dir.path().join("notes.md");
    fs::write(&target, ORIGINAL).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("write notes.md").await.unwrap();

    assert_eq!(fs::read(&target).unwrap(), ORIGINAL, "file must be untouched");
    assert_eq!(result.code, 1, "expected a refusal, got: {}", result.err);
}

/// The refusal names the file and says it was left alone — an agent that reads
/// only the message must not have to guess whether the write half-happened.
#[tokio::test]
async fn the_refusal_says_the_file_is_unchanged() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("notes.md"), ORIGINAL).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("write notes.md").await.unwrap();

    assert!(
        result.err.contains("notes.md") && result.err.contains("unchanged"),
        "refusal should name the file and say it is unchanged, got: {}",
        result.err
    );
}

/// An upstream stage that deliberately produced nothing IS a truncate request.
#[tokio::test]
async fn an_empty_upstream_stage_truncates() {
    let dir = tempdir().unwrap();
    let target = dir.path().join("notes.md");
    fs::write(&target, ORIGINAL).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("printf '' | write notes.md").await.unwrap();

    assert_eq!(result.code, 0, "expected a truncate, got: {}", result.err);
    assert!(
        fs::read(&target).unwrap().is_empty(),
        "an explicitly empty pipe must truncate to zero bytes"
    );
}

/// So is a redirect from an empty file — the bytes were pointed at `write` on
/// purpose, even though there are none of them.
#[tokio::test]
async fn a_redirect_from_an_empty_file_truncates() {
    let dir = tempdir().unwrap();
    let target = dir.path().join("notes.md");
    fs::write(&target, ORIGINAL).unwrap();
    fs::write(dir.path().join("empty"), b"").unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("write notes.md < empty").await.unwrap();

    assert_eq!(result.code, 0, "expected a truncate, got: {}", result.err);
    assert!(fs::read(&target).unwrap().is_empty(), "redirect must truncate");
}

/// A content operand is unaffected.
#[tokio::test]
async fn a_content_operand_still_writes() {
    let dir = tempdir().unwrap();
    let target = dir.path().join("notes.md");
    fs::write(&target, ORIGINAL).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("write notes.md 'new content'").await.unwrap();

    assert_eq!(result.code, 0, "err={}", result.err);
    assert_eq!(fs::read(&target).unwrap(), b"new content");
}

/// A content operand still wins even when the session pipe is exhausted — the
/// operand path must not consult stdin at all.
#[tokio::test]
async fn a_content_operand_wins_over_exhausted_session_stdin() {
    let dir = tempdir().unwrap();
    let target = dir.path().join("notes.md");
    fs::write(&target, ORIGINAL).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel
        .execute_with_pipe_stdin(
            "write notes.md 'from operand'",
            ExecuteOptions::new(),
            exhausted_pipe(),
        )
        .await
        .unwrap();

    assert_eq!(result.code, 0, "err={}", result.err);
    assert_eq!(fs::read(&target).unwrap(), b"from operand");
}

/// Piped content is unaffected.
#[tokio::test]
async fn piped_content_still_writes() {
    let dir = tempdir().unwrap();
    let target = dir.path().join("notes.md");
    fs::write(&target, ORIGINAL).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("echo x | write notes.md").await.unwrap();

    assert_eq!(result.code, 0, "err={}", result.err);
    assert_eq!(fs::read(&target).unwrap(), b"x\n");
}

/// Real bytes arriving on the session pipe are written — the refusal keys on
/// "nothing arrived", never on "this came from the session".
#[tokio::test]
async fn session_stdin_carrying_real_bytes_still_writes() {
    let dir = tempdir().unwrap();
    let target = dir.path().join("notes.md");
    fs::write(&target, ORIGINAL).unwrap();
    let kernel = kernel_at(dir.path());

    let (writer, reader) = pipe_stream_default();
    writer.write_bytes(b"from the session pipe").await.unwrap();
    drop(writer);

    let result = kernel
        .execute_with_pipe_stdin("write notes.md", ExecuteOptions::new(), reader)
        .await
        .unwrap();

    assert_eq!(result.code, 0, "err={}", result.err);
    assert_eq!(fs::read(&target).unwrap(), b"from the session pipe");
}
