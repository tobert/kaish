//! A `cmd &` job's stderr stream (`/v/jobs/N/stderr`) holds the job's own
//! stderr: every builtin's and every external's, once, in order, after the
//! command's redirects have decided where the bytes go.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(all(feature = "localfs", feature = "subprocess"))]

use std::collections::HashMap;
use std::path::PathBuf;

use kaish_kernel::ast::Value;
use kaish_kernel::scheduler::JobId;
use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    let mut vars = HashMap::new();
    vars.insert(
        "PATH".to_string(),
        Value::String(std::env::var("PATH").unwrap_or_default()),
    );
    Kernel::new(KernelConfig::repl().with_initial_vars(vars)).expect("failed to create kernel")
}

struct Streams {
    stdout: String,
    stderr: String,
    result_err: String,
}

/// Run `program` as job 1 to completion and return both streams and the
/// job's result stderr.
async fn run_job(kernel: &Kernel, program: &str) -> Streams {
    kernel.execute(program).await.expect("spawn failed");
    let id = JobId(1);
    let result = kernel.jobs().wait(id).await.expect("job result");
    let stdout = kernel.jobs().read_stdout(id).await.expect("job must exist");
    let stderr = kernel.jobs().read_stderr(id).await.expect("job must exist");
    Streams {
        stdout: String::from_utf8_lossy(&stdout).into_owned(),
        stderr: String::from_utf8_lossy(&stderr).into_owned(),
        result_err: result.err,
    }
}

fn temp_path(name: &str) -> PathBuf {
    std::env::temp_dir().join(format!("kaish-job-stderr-{name}-{}.txt", std::process::id()))
}

/// Index of `needle` in `haystack`, or a failure that shows both.
fn position(haystack: &str, needle: &str) -> usize {
    haystack
        .find(needle)
        .unwrap_or_else(|| panic!("{needle:?} missing from stderr stream: {haystack:?}"))
}

#[tokio::test]
async fn builtin_stderr_reaches_the_stream_after_an_external_wrote_stderr() {
    let kernel = kernel();
    let streams = run_job(
        &kernel,
        "if true; then sh -c 'echo external-err >&2'; cat /kaish-no-such-builtin-file; fi &",
    )
    .await;
    assert_eq!(streams.stderr.matches("external-err").count(), 1, "{:?}", streams.stderr);
    assert_eq!(
        streams.stderr.matches("kaish-no-such-builtin-file").count(),
        1,
        "the builtin's stderr must reach the stream: {:?}",
        streams.stderr
    );
    assert!(
        streams.result_err.contains("kaish-no-such-builtin-file"),
        "the job's result keeps its stderr too: {:?}",
        streams.result_err
    );
}

#[tokio::test]
async fn stderr_from_every_producer_arrives_in_order() {
    let kernel = kernel();
    let streams = run_job(
        &kernel,
        "if true; then cat /kaish-order-one; sh -c 'echo order-two >&2'; echo order-three >&2; cat /kaish-order-four; fi &",
    )
    .await;
    let err = &streams.stderr;
    let one = position(err, "kaish-order-one");
    let two = position(err, "order-two");
    let three = position(err, "order-three");
    let four = position(err, "kaish-order-four");
    assert!(one < two && two < three && three < four, "out of order: {err:?}");
    assert_eq!(streams.stdout, "", "no stderr byte belongs on stdout");
}

#[tokio::test]
async fn external_stderr_is_published_exactly_once() {
    for program in [
        "sh -c 'echo once-a >&2' &",
        "sh -c 'echo once-a >&2' | cat &",
        "timeout 5 -- sh -c 'echo once-a >&2' &",
        "if true; then timeout 5 -- sh -c 'echo once-a >&2'; echo out; fi &",
        "if true; then sh -c 'echo once-a >&2' | cat; fi | cat &",
    ] {
        // Each program runs as job 1 on its own kernel.
        let streams = run_job(&kernel(), program).await;
        assert_eq!(
            streams.stderr.matches("once-a").count(),
            1,
            "{program}: stderr stream {:?}",
            streams.stderr
        );
    }
}

#[tokio::test]
async fn builtin_stderr_under_a_wrapper_is_published_exactly_once() {
    let kernel = kernel();
    let streams = run_job(&kernel, "if true; then sh -c 'echo first >&2'; timeout 5 cat /kaish-wrapped-missing; fi &").await;
    assert_eq!(
        streams.stderr.matches("kaish-wrapped-missing").count(),
        1,
        "{:?}",
        streams.stderr
    );
}

#[tokio::test]
async fn stderr_redirected_to_a_file_stays_out_of_the_stream() {
    let kernel = kernel();
    let external_path = temp_path("external-to-file");
    let builtin_path = temp_path("builtin-to-file");
    let program = format!(
        "if true; then sh -c 'echo external-to-file >&2' 2>{}; cat /kaish-builtin-to-file 2>{}; echo done; fi &",
        external_path.display(),
        builtin_path.display()
    );
    let streams = run_job(&kernel, &program).await;
    for (path, expected) in [(&external_path, "external-to-file"), (&builtin_path, "kaish-builtin-to-file")] {
        let written = std::fs::read_to_string(path).expect("redirect target written");
        std::fs::remove_file(path).expect("remove redirect target");
        assert!(written.contains(expected), "the file must receive the bytes: {written:?}");
    }
    assert_eq!(streams.stderr, "", "redirected stderr leaked into the stream");
    assert_eq!(streams.stdout, "done\n");
}

#[tokio::test]
async fn stderr_redirected_around_a_function_call_stays_out() {
    let kernel = kernel();
    kernel
        .execute("inner() { sh -c 'echo inner-external >&2'; cat /kaish-inner-builtin; }")
        .await
        .expect("define inner");
    let path = temp_path("function");
    let program = format!("inner 2>{} &", path.display());
    let streams = run_job(&kernel, &program).await;
    let written = std::fs::read_to_string(&path).expect("redirect target written");
    std::fs::remove_file(&path).expect("remove redirect target");
    assert!(written.contains("inner-external"), "{written:?}");
    assert!(written.contains("kaish-inner-builtin"), "{written:?}");
    assert_eq!(streams.stderr, "", "a 2> on the call must keep inner stderr out of the stream");
}

#[tokio::test]
async fn stderr_merged_into_stdout_reaches_the_stdout_stream() {
    let kernel = kernel();
    let streams = run_job(
        &kernel,
        "if true; then sh -c 'echo external-merged >&2' 2>&1; cat /kaish-builtin-merged 2>&1; fi &",
    )
    .await;
    assert!(streams.stdout.contains("external-merged"), "stdout: {:?}", streams.stdout);
    assert!(streams.stdout.contains("kaish-builtin-merged"), "stdout: {:?}", streams.stdout);
    assert_eq!(streams.stderr, "", "merged stderr must not also reach the stderr stream");
}

#[tokio::test]
async fn stderr_merged_into_a_pipe_is_the_next_stage_input() {
    let kernel = kernel();
    let streams = run_job(&kernel, "sh -c 'echo piped-err >&2' 2>&1 | wc -l &").await;
    assert_eq!(streams.stdout.trim(), "1", "the merged line feeds wc: {:?}", streams.stdout);
    assert_eq!(streams.stderr, "", "merged stderr is pipe data, not job stderr");
}

#[tokio::test]
async fn stdout_sent_to_stderr_reaches_the_stderr_stream() {
    let kernel = kernel();
    let streams = run_job(
        &kernel,
        "if true; then sh -c 'echo external-first >&2'; echo builtin-to-err >&2; sh -c 'echo external-to-err' >&2; fi &",
    )
    .await;
    let err = &streams.stderr;
    let first = position(err, "external-first");
    let builtin = position(err, "builtin-to-err");
    let external = position(err, "external-to-err");
    assert!(first < builtin && builtin < external, "out of order: {err:?}");
    assert_eq!(streams.stdout, "");
}

#[tokio::test]
async fn substitution_stderr_is_job_stderr() {
    let kernel = kernel();
    let streams = run_job(
        &kernel,
        "if true; then x=$(sh -c 'echo sub-external >&2; echo value'); y=$(cat /kaish-sub-builtin); echo \"got $x\"; sh -c 'echo after-sub >&2'; fi &",
    )
    .await;
    let err = &streams.stderr;
    assert_eq!(err.matches("sub-external").count(), 1, "{err:?}");
    assert_eq!(err.matches("kaish-sub-builtin").count(), 1, "{err:?}");
    let sub = position(err, "sub-external");
    let after = position(err, "after-sub");
    assert!(sub < after, "out of order: {err:?}");
    assert_eq!(streams.stdout, "got value\n");
}
