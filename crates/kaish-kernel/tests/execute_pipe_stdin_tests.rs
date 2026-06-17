//! Kernel-routed coverage for the *lazy* stdin seam — `Kernel::execute_with_
//! pipe_stdin(_streaming)`, which hands the kernel a `PipeReader` instead of a
//! pre-read `String`.
//!
//! This is the fix for the P1.0 regression: the original `ExecuteOptions::stdin`
//! (String) path forced the frontend to `read_to_end` process stdin *before*
//! executing, which **hangs** `kaish -c 'echo hi'` when stdin is an open pipe
//! that never sends EOF (the subprocess-with-idle-stdin case). A lazy pipe lets
//! a command that never reads stdin (`echo`) return immediately while the writer
//! is still open — matching bash, which never touches stdin for `echo`.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

use std::time::Duration;

use kaish_kernel::{pipe_stream_default, ExecuteOptions, Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::repl().with_latch(false).with_trash(false))
        .expect("failed to create kernel")
}

#[tokio::test]
async fn lazy_stdin_does_not_block_a_command_that_never_reads() {
    // The regression in one assertion: a never-EOF pipe + a command that does
    // not read stdin must return promptly. If the kernel eagerly drained stdin
    // (the old String path's frontend did), this would hang on the live writer.
    let kernel = kernel();
    let (writer, reader) = pipe_stream_default();

    let fut = kernel.execute_with_pipe_stdin("echo hi", ExecuteOptions::new(), reader);
    let result = tokio::time::timeout(Duration::from_secs(5), fut)
        .await
        .expect("echo must not block on unread, never-closed stdin")
        .expect("kernel execute");

    assert_eq!(result.text_out(), "hi\n");
    // Keep the writer alive across execution: the point is that an *open* pipe
    // does not stall a non-reading command.
    drop(writer);
}

#[tokio::test]
async fn lazy_stdin_feeds_a_reading_command() {
    // A command that reads stdin drains the pipe to EOF and sees the bytes.
    let kernel = kernel();
    let (writer, reader) = pipe_stream_default();
    writer.write_bytes(b"3\n1\n2\n").await.unwrap();
    drop(writer); // EOF

    let result = kernel
        .execute_with_pipe_stdin("sort", ExecuteOptions::new(), reader)
        .await
        .expect("kernel execute");
    assert_eq!(result.text_out(), "1\n2\n3\n");
}

#[tokio::test]
async fn lazy_stdin_feeds_first_stage_of_a_pipeline() {
    // The first stage of a multi-stage pipeline inherits the seeded pipe.
    let kernel = kernel();
    let (writer, reader) = pipe_stream_default();
    writer.write_bytes(b"b\na\nc\n").await.unwrap();
    drop(writer);

    let result = kernel
        .execute_with_pipe_stdin("sort | head -n 2", ExecuteOptions::new(), reader)
        .await
        .expect("kernel execute");
    assert_eq!(result.text_out(), "a\nb\n");
}

#[tokio::test]
async fn redirect_beats_lazy_stdin() {
    // `<<<` (here-string) must win over a seeded pipe: `read_stdin_*` prefers
    // `pipe_stdin`, so `set_stdin` clears it to preserve redirect precedence.
    // The writer stays open and silent; if the pipe leaked through, the command
    // would either hang or print the wrong bytes.
    let kernel = kernel();
    let (writer, reader) = pipe_stream_default();

    let fut = kernel.execute_with_pipe_stdin("cat <<< wins", ExecuteOptions::new(), reader);
    let result = tokio::time::timeout(Duration::from_secs(5), fut)
        .await
        .expect("redirect path must not touch the live pipe")
        .expect("kernel execute");
    assert_eq!(result.text_out(), "wins\n");
    drop(writer);
}

#[cfg(all(target_os = "linux", feature = "subprocess"))]
#[tokio::test]
async fn lazy_stdin_feeds_an_external_command() {
    // External commands take stdin via `try_execute_external`, a different path
    // than builtins' `read_stdin_to_*`. It must also drain a seeded lazy pipe —
    // an absolute path forces the external path (not the `cat` builtin).
    let kernel = Kernel::new(
        KernelConfig::repl()
            .with_latch(false)
            .with_trash(false)
            .with_allow_external_commands(true),
    )
    .expect("failed to create kernel");
    let (writer, reader) = pipe_stream_default();
    writer.write_bytes(b"external sees this\n").await.unwrap();
    drop(writer);

    let result = kernel
        .execute_with_pipe_stdin("/bin/cat", ExecuteOptions::new(), reader)
        .await
        .expect("kernel execute");
    assert_eq!(result.text_out(), "external sees this\n");
}

#[tokio::test]
async fn lazy_stdin_feeds_scatter_with_no_pre_scatter_command() {
    // The scatter/gather runner's no-pre-scatter branch reads stdin directly;
    // it must see a seeded lazy pipe, not just the String buffer.
    let kernel = kernel();
    let (writer, reader) = pipe_stream_default();
    writer.write_bytes(b"a\nb\nc\n").await.unwrap();
    drop(writer);

    let result = kernel
        .execute_with_pipe_stdin(
            r#"scatter | echo "loud-${ITEM}" | gather"#,
            ExecuteOptions::new(),
            reader,
        )
        .await
        .expect("kernel execute");
    // Each line is scattered to a worker (item bound to `$ITEM`) and gathered.
    // Before the fix the runner read only the String buffer, so scatter saw no
    // items and returned empty.
    assert!(result.ok(), "scatter failed: {}", result.err);
    let out = result.text_out();
    assert!(out.contains("loud-a"), "missing loud-a: {out:?}");
    assert!(out.contains("loud-b"), "missing loud-b: {out:?}");
    assert!(out.contains("loud-c"), "missing loud-c: {out:?}");
}

#[tokio::test]
async fn lazy_stdin_reaches_a_builtin_that_reads_the_stdin_field() {
    // Regression: `patch`/`write`/`kaish-validate` read stdin and must see a
    // seeded lazy pipe. They formerly read the `ctx.stdin` String field directly
    // (bypassing `pipe_stdin`), so the lazy-stdin change silently starved them.
    // `kaish-validate` reads stdin with no file setup — a clean probe.
    let kernel = kernel();
    let (writer, reader) = pipe_stream_default();
    writer.write_bytes(b"echo hi | grep x\n").await.unwrap();
    drop(writer);

    let result = kernel
        .execute_with_pipe_stdin("kaish-validate", ExecuteOptions::new(), reader)
        .await
        .expect("kernel execute");
    assert!(result.ok(), "validate should see piped stdin: {}", result.err);
    assert!(
        result.text_out().contains("valid"),
        "expected validation of piped script, got: {:?}",
        result.text_out()
    );
}

#[tokio::test]
async fn lazy_stdin_does_not_leak_between_calls() {
    // The seeded reader is consume-once: a second call with no stdin sees none.
    let kernel = kernel();
    let (writer, reader) = pipe_stream_default();
    writer.write_bytes(b"first\n").await.unwrap();
    drop(writer);

    let first = kernel
        .execute_with_pipe_stdin("cat", ExecuteOptions::new(), reader)
        .await
        .expect("first execute");
    assert_eq!(first.text_out(), "first\n");

    let second = kernel.execute("echo done").await.expect("second execute");
    assert_eq!(second.text_out(), "done\n", "prior pipe must not bleed into a later call");
}
