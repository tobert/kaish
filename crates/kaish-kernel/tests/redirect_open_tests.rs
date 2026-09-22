//! Redirect targets open before the command runs.
//!
//! Each target is evaluated and opened left to right first: `>` truncates,
//! `>>` opens for append, and a target that fails to open means the command
//! does not run (exit 1). The same file as `<` input and as an output target
//! is refused before anything runs. A redirect never creates a directory.

#![cfg(feature = "localfs")]
// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use common::kernel_at;

/// A function body's streaming writer must not bypass the function's own
/// stdout redirect into the enclosing pipe.
#[tokio::test]
async fn function_body_output_goes_to_the_redirect_not_the_pipe() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("in.txt"), "hello\n").unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("f() { cat in.txt; }; f > out.txt | wc -c").await.expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "0", "the pipe must get nothing: {r:?}");
    assert_eq!(std::fs::read_to_string(dir.path().join("out.txt")).unwrap(), "hello\n");
}

/// The same, for a plain builtin in the first stage of a pipeline.
#[tokio::test]
async fn pipeline_stage_output_goes_to_the_redirect_not_the_pipe() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("in.txt"), "hello\n").unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("cat in.txt > out.txt | wc -c").await.expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "0", "the pipe must get nothing: {r:?}");
    assert_eq!(std::fs::read_to_string(dir.path().join("out.txt")).unwrap(), "hello\n");
}

/// A missing directory is refused with the path and the fix, once, with no
/// doubled phrase from the backend error.
#[tokio::test]
async fn missing_directory_error_names_the_path_and_the_fix() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("touch ran.txt > nodir/a.txt").await.expect("execute");
    assert_eq!(r.code, 1, "{r:?}");
    assert_eq!(
        r.err.trim_end(),
        "redirect: nodir/a.txt: no such file or directory; create the directory first: mkdir -p nodir",
    );
    assert!(!dir.path().join("ran.txt").exists(), "the command must not run");
    assert!(!dir.path().join("nodir").exists(), "a redirect must not create a directory");
}

/// A missing `<` file names the path once, without the backend's own prefix.
#[tokio::test]
async fn missing_input_error_names_the_path() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("cat < nofile.txt").await.expect("execute");
    assert_eq!(r.code, 1, "{r:?}");
    assert_eq!(r.err.trim_end(), "redirect: nofile.txt: no such file or directory");
}

/// A target that cannot be written is refused before the command runs.
#[tokio::test]
async fn unwritable_target_is_refused_before_the_command_runs() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("touch ran.txt > /v/jobs/x").await.expect("execute");
    assert_eq!(r.code, 1, "{r:?}");
    assert!(r.err.starts_with("redirect: /v/jobs/x: "), "{r:?}");
    assert!(!dir.path().join("ran.txt").exists(), "the command must not run");
}

/// `/dev/null` still opens for both `>` and `>>`.
#[tokio::test]
async fn dev_null_opens_for_write_and_append() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("echo x > /dev/null; echo y >> /dev/null; echo z 2> /dev/null; echo ok").await.expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "z\nok");
}

/// A background job whose target fails to open does not run its command.
#[tokio::test]
async fn background_job_with_a_missing_directory_does_not_run() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    kernel.execute("touch ran.txt > nodir/a.txt &").await.expect("spawn");
    let id = kaish_kernel::scheduler::JobId(1);
    let result = kernel.jobs().wait(id).await.expect("job result");
    assert_eq!(result.code, 1, "{result:?}");
    let stderr = kernel.jobs().read_stderr(id).await.expect("job must exist");
    let stderr = String::from_utf8_lossy(&stderr);
    assert!(stderr.contains("redirect: nodir/a.txt: no such file or directory"), "{stderr:?}");
    assert!(!dir.path().join("ran.txt").exists(), "the job's command must not run");
}

/// gather's own target opens before any worker runs.
#[tokio::test]
async fn gather_target_failure_runs_no_worker() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute(r#"seq 1 3 | scatter | touch "w$ITEM" | gather > nodir/out.jsonl"#)
        .await
        .expect("execute");
    assert_eq!(r.code, 1, "{r:?}");
    assert!(r.err.contains("redirect: nodir/out.jsonl: no such file or directory"), "{r:?}");
    for n in 1..=3 {
        assert!(!dir.path().join(format!("w{n}")).exists(), "worker {n} must not run");
    }
}

/// gather reads its workers' results, never a file.
#[tokio::test]
async fn gather_input_redirect_is_refused() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("in.txt"), "x\n").unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("seq 1 2 | scatter | echo $ITEM | gather < in.txt").await.expect("execute");
    assert_eq!(r.code, 1, "{r:?}");
    assert!(r.err.contains("gather"), "{r:?}");
}

/// Targets computed at runtime are compared as resolved paths, not
/// spellings: `P` and `./P` are one file.
#[tokio::test]
async fn same_file_through_substitution_is_refused() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("P"), "b\na\n").unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("sort < $(echo P) > $(echo ./P)").await.expect("execute");
    assert_eq!(r.code, 1, "{r:?}");
    assert_eq!(
        r.err.trim_end(),
        "redirect: ./P is both input and output; write to a temp file, then mv it over ./P",
    );
    assert_eq!(std::fs::read_to_string(dir.path().join("P")).unwrap(), "b\na\n", "P must be untouched");
}

/// A symlink to the input is the same file.
#[tokio::test]
async fn same_file_through_a_symlink_is_refused() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("P"), "b\na\n").unwrap();
    std::os::unix::fs::symlink("P", dir.path().join("L")).unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("sort > L < P").await.expect("execute");
    assert_eq!(r.code, 1, "{r:?}");
    assert!(r.err.contains("L is both input and output"), "{r:?}");
    assert_eq!(std::fs::read_to_string(dir.path().join("P")).unwrap(), "b\na\n", "P must be untouched");
}

/// Different files on `<` and `>` are fine.
#[tokio::test]
async fn different_input_and_output_files_run() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("P"), "b\na\n").unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("sort < P > Q; cat Q").await.expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "a\nb");
}
