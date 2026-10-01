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

#[tokio::test]
async fn nested_pipeline_receives_typed_upstream_input() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("f() { jq -c . | cat; }; seq 1 3 | f").await.unwrap();
    assert_eq!(result.code, 0, "{result:?}");
    assert_eq!(result.text_out().trim(), "[1,2,3]", "{result:?}");
}

#[tokio::test]
async fn nested_scatter_receives_typed_upstream_items() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("f() { scatter | echo x | gather | jq -c '.[0].item | type'; }; seq 1 3 | f").await.unwrap();
    assert_eq!(result.code, 0, "{result:?}");
    assert_eq!(result.text_out().trim(), "\"number\"", "{result:?}");
}

#[rstest::rstest]
#[case(false)]
#[case(true)]
#[tokio::test]
async fn first_stage_redirect_target_reads_session_stdin(#[case] live: bool) {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "g", "file contents\n");
    let kernel = kernel_at(dir.path());
    let script = "cat < $(cat) | cat";
    let result = if live {
        use tokio::io::AsyncWriteExt;
        let (mut writer, reader) = pipe_stream_default();
        writer.write_all(b"g\n").await.unwrap();
        writer.shutdown().await.unwrap();
        kernel.execute_with_pipe_stdin(script, ExecuteOptions::new(), reader).await.unwrap()
    } else {
        kernel.execute_with_options(script, ExecuteOptions::new().with_stdin(b"g\n".to_vec())).await.unwrap()
    };
    assert_eq!(result.code, 0, "{result:?}");
    assert_eq!(result.text_out(), "file contents\n", "{result:?}");
}

#[rstest::rstest]
#[case("read x <<'EOF'\na\nb\nEOF\ncat", "S1\nS2\n")]
#[case("cat < g > nodir/x | cat; cat", "S1\nS2\n")]
#[case("cat < g | cat; cat", "a\nb\nc\nS1\nS2\n")]
#[tokio::test]
async fn redirected_input_restores_buffered_session_stdin(#[case] script: &str, #[case] expected: &str) {
    let dir = tempfile::tempdir().unwrap();
    write(&dir, "g", "a\nb\nc\n");
    let kernel = kernel_at(dir.path());
    let result = kernel.execute_with_options(script, ExecuteOptions::new().with_stdin(b"S1\nS2\n".to_vec())).await.unwrap();
    assert_eq!(result.text_out(), expected, "{result:?}");
}

#[tokio::test]
async fn gather_rows_do_not_replace_next_statement_typed_input() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute_with_options(
        "seq 1 3 | scatter | echo x | gather | echo done; jq -c .",
        ExecuteOptions::new().with_stdin(b"[9]\n".to_vec()),
    ).await.unwrap();
    assert_eq!(result.code, 0, "{result:?}");
    assert_eq!(result.text_out(), "done\n[9]\n", "{result:?}");
}

#[rstest::rstest]
#[case(false)]
#[case(true)]
#[tokio::test]
async fn first_stage_returns_partially_consumed_session_input(#[case] live: bool) {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let script = "read x | cat; cat";
    let result = if live {
        kernel.execute_with_pipe_stdin(script, ExecuteOptions::new(), session_pipe().await).await.unwrap()
    } else {
        kernel.execute_with_options(script, ExecuteOptions::new().with_stdin(b"S1\nS2\n".to_vec())).await.unwrap()
    };
    assert_eq!(result.code, 0, "{result:?}");
    assert_eq!(result.text_out(), "S2\n", "{result:?}");
}

#[tokio::test]
async fn nested_pipeline_drains_input_larger_than_the_pipe_buffer() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = tokio::time::timeout(
        std::time::Duration::from_secs(10),
        kernel.execute("f() { cat | cat; }; seq 1 20000 | f | wc -l"),
    ).await.expect("nested pipeline must drain under backpressure").unwrap();
    assert_eq!(result.code, 0, "{result:?}");
    assert_eq!(result.text_out().trim(), "20000", "{result:?}");
}
