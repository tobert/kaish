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
use kaish_kernel::validator::IssueCode;
use kaish_kernel::KernelError;

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

    let r = kernel.execute("touch ran.txt > /v/bin/x").await.expect("execute");
    assert_eq!(r.code, 1, "{r:?}");
    assert_eq!(r.err.trim_end(), "redirect: /v/bin/x: read-only filesystem");
    assert!(!dir.path().join("ran.txt").exists(), "the command must not run");
}

/// `/dev/null` still opens for both `>` and `>>`.
#[tokio::test]
async fn dev_null_opens_for_write_and_append() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute("echo x > /dev/null; echo y >> /dev/null; ls /kaish-redirect-missing 2> /dev/null; echo ok")
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "ok");
    assert_eq!(r.err, "", "ls's error must go to /dev/null: {r:?}");
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
    assert_eq!(r.code, 2, "{r:?}");
    assert_eq!(
        r.err.trim_end(),
        "gather: takes no input redirect; it reads its workers' results. Remove the <, <<, or <<<",
    );
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
        "redirect: ./P is both input and output (> empties it before it is read); \
         write to a temp file, then mv it over ./P",
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

/// Literal targets are caught by the validator, so `--plan` reports them.
#[tokio::test]
async fn literal_same_file_is_a_validation_error() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("P"), "b\na\n").unwrap();
    let kernel = kernel_at(dir.path());

    let err = kernel.execute("sort < P >> ./P").await.expect_err("must be rejected");
    let KernelError::Validation { issues, .. } = err else {
        panic!("must be KernelError::Validation, not {err:?}");
    };
    let issue = issues
        .iter()
        .find(|i| i.code == IssueCode::RedirectInputIsOutput)
        .unwrap_or_else(|| panic!("expected RedirectInputIsOutput: {issues:?}"));
    assert_eq!(issue.code.code(), "E023");
    assert_eq!(
        issue.message,
        "redirect: ./P is both input and output (>> feeds the command its own output); \
         write to a temp file, then append that to ./P",
    );
    assert_eq!(std::fs::read_to_string(dir.path().join("P")).unwrap(), "b\na\n");
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

/// Appending to the file being read feeds the command its own output:
/// `cat < P >> P` would grow P without end.
#[tokio::test]
async fn reading_a_file_while_appending_to_it_is_refused() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("P"), "a\n").unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("cat < $(echo P) >> $(echo P)").await.expect("execute");
    assert_eq!(r.code, 1, "{r:?}");
    assert_eq!(
        r.err.trim_end(),
        "redirect: P is both input and output (>> feeds the command its own output); \
         write to a temp file, then append that to P",
    );
    assert_eq!(std::fs::read_to_string(dir.path().join("P")).unwrap(), "a\n", "P must be untouched");
}

/// A missing input is its own error, not a same-file hazard, and the
/// output after it is never opened.
#[tokio::test]
async fn missing_input_named_as_output_reports_the_missing_input() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("sort < $(echo nofile) > $(echo nofile)").await.expect("execute");
    assert_eq!(r.code, 1, "{r:?}");
    assert_eq!(r.err.trim_end(), "redirect: nofile: no such file or directory");
    assert!(!dir.path().join("nofile").exists(), "the output after a failed input must not open");
}

/// A dangling symlink into a missing directory names that directory, never
/// one that exists.
#[tokio::test]
async fn dangling_symlink_names_the_missing_directory() {
    let dir = tempfile::tempdir().unwrap();
    std::os::unix::fs::symlink("nodir/x", dir.path().join("L")).unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("echo a > L").await.expect("execute");
    assert_eq!(r.code, 1, "{r:?}");
    assert_eq!(
        r.err.trim_end(),
        "redirect: L: no such file or directory; create the directory first: mkdir -p nodir",
    );
    assert!(!dir.path().join("nodir").exists());
}

/// scatter's own redirects never reach it inside a scatter ... gather
/// pipeline, so they are refused by name rather than dropped.
#[tokio::test]
async fn scatter_redirect_is_refused_in_a_scatter_gather_pipeline() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("list.txt"), "1\n2\n").unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute(r#"scatter < list.txt | touch "w$ITEM" | gather"#)
        .await
        .expect("execute");
    assert_eq!(r.code, 2, "{r:?}");
    assert_eq!(
        r.err.trim_end(),
        "scatter: takes no redirects in a scatter ... gather pipeline; \
         pipe its input in: cat list.txt | scatter | ...",
    );
    assert!(!dir.path().join("w1").exists(), "no worker may run");
}

/// A read-only mount refuses the target by kind, and the command does not
/// run.
#[tokio::test]
async fn read_only_mount_target_is_refused() {
    use kaish_kernel::vfs::{LocalFs, MemoryFs, VfsRouter};
    use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend};
    use std::sync::Arc;

    let dir = tempfile::tempdir().unwrap();
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    vfs.mount("/ro", LocalFs::read_only(dir.path().to_path_buf()));
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    let kernel = Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |_| {}).expect("kernel");

    let r = kernel.execute("touch /ran.txt > /ro/out.txt; echo rc=$?; ls /").await.expect("execute");
    assert_eq!(r.err.trim_end(), "redirect: /ro/out.txt: read-only filesystem");
    assert!(r.text_out().starts_with("rc=1"), "{r:?}");
    assert!(!r.text_out().contains("ran.txt"), "the command must not run: {r:?}");
    assert!(!dir.path().join("out.txt").exists(), "host must be untouched");
}

/// Under an overlay, the truncation and the write both land in the upper
/// layer; the host file keeps its content.
#[cfg(feature = "overlay")]
#[tokio::test]
async fn overlay_target_writes_land_in_the_upper_layer() {
    use kaish_kernel::{Kernel, KernelConfig};

    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("out.txt"), "host\n").unwrap();
    let config = KernelConfig::agent_with_root(dir.path().to_path_buf())
        .with_overlay(true)
        .with_trash(false)
        .with_allow_unwrapped_commands(false);
    let kernel = Kernel::new(config).expect("overlay kernel");

    let r = kernel
        .execute("echo new > out.txt; cat out.txt; echo x > nodir/a.txt; echo rc=$?")
        .await
        .expect("execute");
    assert_eq!(r.text_out().trim(), "new\nrc=1", "{r:?}");
    assert_eq!(std::fs::read_to_string(dir.path().join("out.txt")).unwrap(), "host\n");
    assert!(!dir.path().join("nodir").exists());
}
