//! An input redirect belongs to the command it is attached to.
//!
//! Two rules, both bash's:
//!
//! 1. `<`, `<<`, and `<<<` win over the pipe and over structured data
//!    handed down the pipeline. `cmd | jq . < f` reads `f`.
//! 2. When the command finishes, whatever it left unread of the redirect's
//!    input is dropped. The session's stdin is what it was before.

#![cfg(feature = "localfs")]
// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use common::kernel_at;
use kaish_kernel::{pipe_stream_default, ExecuteOptions};

fn write(dir: &tempfile::TempDir, name: &str, body: &str) {
    std::fs::write(dir.path().join(name), body).unwrap();
}

// Rule 1: the redirect wins over structured data from upstream.

/// bash: `seq 1 3 | jq -c length < data.json` prints 5.
#[tokio::test]
async fn last_stage_input_redirect_beats_upstream_data() {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "data.json", "[1,2,3,4,5]\n");
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("seq 1 3 | jq -c length < data.json").await.unwrap();
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "5", "{r:?}");
}

#[tokio::test]
async fn gather_then_input_redirect_reads_the_file() {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "data.json", "[1,2,3,4,5]\n");
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute("seq 1 3 | scatter | echo x | gather | jq -c length < data.json")
        .await
        .unwrap();
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "5", "{r:?}");
}

#[tokio::test]
async fn middle_stage_input_redirect_beats_upstream_data() {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "data.json", "[1,2,3,4,5]\n");
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("seq 1 3 | jq -c length < data.json | cat").await.unwrap();
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "5", "{r:?}");
}

#[tokio::test]
async fn function_body_input_redirect_beats_upstream_data() {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "data.json", "[1,2,3,4,5]\n");
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute("count() { jq -c length < data.json; }; seq 1 3 | count")
        .await
        .unwrap();
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "5", "{r:?}");
}

/// Stage 0 of the commands after `gather` inherits gather's structured rows
/// on the context. Its own `<` wins over them.
#[tokio::test]
async fn post_gather_first_stage_input_redirect_beats_gather_rows() {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "data.json", "[1,2,3,4,5]\n");
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute("seq 1 3 | scatter | echo x | gather | jq -c length < data.json | cat")
        .await
        .unwrap();
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "5", "{r:?}");
}

// Rule 2: unread redirect input does not become the session's stdin.

/// bash, stdin `S1\nS2\n`: `read x < g; cat` prints S1 and S2.
#[tokio::test]
async fn read_from_file_leaves_session_stdin_untouched() {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "g", "a\nb\nc\n");
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute_with_options("read x < g; cat", ExecuteOptions::new().with_stdin(b"S1\nS2\n".to_vec()))
        .await
        .unwrap();
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out(), "S1\nS2\n", "{r:?}");
}

/// The same with a lazy process-stdin pipe: the live reader must survive.
#[tokio::test]
async fn read_from_file_leaves_session_pipe_stdin_untouched() {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "g", "a\nb\nc\n");
    let kernel = kernel_at(dir.path());
    let (mut writer, reader) = pipe_stream_default();
    {
        use tokio::io::AsyncWriteExt;
        writer.write_all(b"S1\nS2\n").await.unwrap();
        writer.shutdown().await.unwrap();
    }

    let r = kernel
        .execute_with_pipe_stdin("read x < g; cat", ExecuteOptions::new(), reader)
        .await
        .unwrap();
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out(), "S1\nS2\n", "{r:?}");
}

/// bash, stdin `S1\nS2\n`: `f() { read x < g; }; f; cat` prints S1 and S2.
#[tokio::test]
async fn function_read_from_file_leaves_session_stdin_untouched() {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "g", "a\nb\nc\n");
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute_with_options(
            "f() { read x < g; }; f; cat",
            ExecuteOptions::new().with_stdin(b"S1\nS2\n".to_vec()),
        )
        .await
        .unwrap();
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out(), "S1\nS2\n", "{r:?}");
}

/// bash, stdin `S1\nS2\n`: `f() { read x < g; }; f | cat; cat` prints S1 and S2.
#[tokio::test]
async fn function_in_pipeline_read_from_file_leaves_session_stdin_untouched() {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "g", "a\nb\nc\n");
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute_with_options(
            "f() { read x < g; }; f | cat; cat",
            ExecuteOptions::new().with_stdin(b"S1\nS2\n".to_vec()),
        )
        .await
        .unwrap();
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out(), "S1\nS2\n", "{r:?}");
}

/// With no session stdin, the file's remainder must not appear as one.
#[tokio::test]
async fn function_in_pipeline_file_remainder_does_not_leak() {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "g", "a\nb\nc\n");
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("f() { read x < g; }; f | cat; cat").await.unwrap();
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out(), "", "{r:?}");
}

/// A here-string is scoped the same way.
#[tokio::test]
async fn read_from_here_string_leaves_session_stdin_untouched() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute_with_options(
            "read x <<< 'p q'; cat",
            ExecuteOptions::new().with_stdin(b"S1\n".to_vec()),
        )
        .await
        .unwrap();
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out(), "S1\n", "{r:?}");
}

/// Control: a redirect on a stage still feeds that stage.
#[tokio::test]
async fn redirect_still_feeds_its_own_command() {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "g", "a\nb\nc\n");
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("read x < g; echo $x").await.unwrap();
    assert_eq!(r.text_out(), "a\n", "{r:?}");
}

// Failed opens, live session pipes, and the commands after `gather`.

/// A lazy session stdin holding `S1\nS2\n`, already closed by the writer.
async fn session_pipe() -> kaish_kernel::PipeReader {
    use tokio::io::AsyncWriteExt;
    let (mut writer, reader) = pipe_stream_default();
    writer.write_all(b"S1\nS2\n").await.unwrap();
    writer.shutdown().await.unwrap();
    reader
}

/// bash, stdin `S1\nS2\n`: `cat < g > nodir/x; cat` fails the first command
/// and the second prints S1 and S2.
#[tokio::test]
async fn failed_redirect_open_leaves_session_stdin_untouched() {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "g", "a\nb\nc\n");
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute_with_options(
            "cat < g > nodir/x; cat",
            ExecuteOptions::new().with_stdin(b"S1\nS2\n".to_vec()),
        )
        .await
        .unwrap();
    assert_eq!(r.text_out(), "S1\nS2\n", "{r:?}");
}

/// bash, stdin `S1\nS2\n`: `cat < g | cat; cat` prints a, b, c, S1, S2.
#[tokio::test]
async fn first_stage_input_redirect_leaves_session_pipe_stdin_untouched() {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "g", "a\nb\nc\n");
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute_with_pipe_stdin("cat < g | cat; cat", ExecuteOptions::new(), session_pipe().await)
        .await
        .unwrap();
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out(), "a\nb\nc\nS1\nS2\n", "{r:?}");
}

/// bash, stdin `S1\nS2\n`: the commands after `gather` read gather's rows,
/// and the next statement reads the session's stdin.
#[tokio::test]
async fn gather_aftermath_leaves_session_stdin_untouched() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute_with_options(
            "seq 1 3 | scatter | echo x | gather | echo done; cat",
            ExecuteOptions::new().with_stdin(b"S1\nS2\n".to_vec()),
        )
        .await
        .unwrap();
    assert_eq!(r.text_out(), "done\nS1\nS2\n", "{r:?}");
}

#[tokio::test]
async fn gather_aftermath_leaves_session_pipe_stdin_untouched() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute_with_pipe_stdin(
            "seq 1 3 | scatter | echo x | gather | echo done; cat",
            ExecuteOptions::new(),
            session_pipe().await,
        )
        .await
        .unwrap();
    assert_eq!(r.text_out(), "done\nS1\nS2\n", "{r:?}");
}

/// The post-gather command reads the gathered rows, not the live session pipe.
#[tokio::test]
async fn post_gather_reader_gets_rows_not_the_session_pipe() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute_with_pipe_stdin(
            "seq 1 3 | scatter | echo x | gather --lines | wc -c; cat",
            ExecuteOptions::new(),
            session_pipe().await,
        )
        .await
        .unwrap();
    assert_eq!(r.text_out(), "5\nS1\nS2\n", "{r:?}");
}

#[tokio::test]
async fn gather_then_redirect_then_session_stdin() {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "data.json", "[1,2,3,4,5]\n");
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute_with_options(
            "seq 1 3 | scatter | echo x | gather | jq -c length < data.json; cat",
            ExecuteOptions::new().with_stdin(b"S1\nS2\n".to_vec()),
        )
        .await
        .unwrap();
    assert_eq!(r.text_out(), "5\nS1\nS2\n", "{r:?}");
}
