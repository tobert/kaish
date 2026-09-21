//! Safety net for the stderr-routing rewrite in `job_stderr_routing_tests.rs`:
//! a broad corpus of scripts, each run twice — once as a `cmd &` job, once in
//! the foreground on a fresh kernel — asserting the job's `/v/jobs/N/stderr`
//! stream equals the foreground run's `result.err` byte for byte. A script
//! here that the targeted test file does not name is the guard against a
//! missed stderr-producing site: every kernel error kind (command not found,
//! a malformed `[[ ]]`/`(( ))` operand, a `source`d script's runtime error, a
//! bad scatter/gather option, a redirect-target failure), plus builtins,
//! externals, functions, pipelines (top-level and nested inside a compound),
//! compound statements, `$(...)`, and each redirect form.

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

fn temp_path(name: &str) -> PathBuf {
    std::env::temp_dir().join(format!("kaish-job-stderr-corpus-{name}-{}.txt", std::process::id()))
}

/// Run `setup` (may be empty) then `program` as job 1 to completion; return
/// the job's stderr stream.
async fn job_stderr(setup: &str, program: &str) -> Vec<u8> {
    let kernel = kernel();
    if !setup.is_empty() {
        kernel.execute(setup).await.expect("setup failed");
    }
    kernel.execute(program).await.expect("spawn failed");
    let id = JobId(1);
    kernel.jobs().wait(id).await.expect("job result");
    kernel.jobs().read_stderr(id).await.expect("job must exist")
}

/// Run `setup` (may be empty) then `program` in the foreground on a fresh
/// kernel; return the statement's own `result.err`.
async fn foreground_stderr(setup: &str, program: &str) -> String {
    let kernel = kernel();
    if !setup.is_empty() {
        kernel.execute(setup).await.expect("setup failed");
    }
    kernel.execute(program).await.expect("execute failed").err
}

/// Run `job_program` (must end in ` &`) as a job, and the same script with
/// that suffix stripped in the foreground; assert the job's stderr stream
/// equals the foreground run's `result.err`, byte for byte.
async fn assert_job_matches_foreground(setup: &str, job_program: &str) {
    let fg_program = job_program
        .strip_suffix(" &")
        .unwrap_or_else(|| panic!("corpus program must end in \" &\": {job_program:?}"));
    let job = job_stderr(setup, job_program).await;
    let fg = foreground_stderr(setup, fg_program).await;
    assert_eq!(
        String::from_utf8_lossy(&job),
        fg,
        "job stderr must equal the foreground run's stderr for {job_program:?}"
    );
}

#[tokio::test]
async fn builtin_error_matches_foreground() {
    assert_job_matches_foreground("", "cat /kaish-corpus-missing &").await;
}

#[tokio::test]
async fn external_error_matches_foreground() {
    assert_job_matches_foreground("", "sh -c 'echo corpus-external >&2; exit 3' &").await;
}

#[tokio::test]
async fn function_error_matches_foreground() {
    assert_job_matches_foreground(
        "f() { cat /kaish-corpus-fn-missing; }",
        "f &",
    )
    .await;
}

#[tokio::test]
async fn top_level_pipeline_middle_stage_error_matches_foreground() {
    assert_job_matches_foreground("", "cat /kaish-corpus-mid-missing | wc -l &").await;
}

/// The drain-into-aggregate case: a `|` pipe's non-last stage flushes its
/// stderr through the kernel's `stderr_receiver` channel (`Kernel::
/// drain_stderr_into`), not directly — this pins that the job stream still
/// gets it exactly once even nested inside a compound.
#[tokio::test]
async fn pipeline_nested_in_a_compound_matches_foreground() {
    assert_job_matches_foreground(
        "",
        "if true; then cat /kaish-corpus-nested-missing | wc -l; fi &",
    )
    .await;
}

#[tokio::test]
async fn compound_mixing_builtin_and_external_matches_foreground() {
    assert_job_matches_foreground(
        "",
        "if true; then sh -c 'echo corpus-mix-ext >&2'; cat /kaish-corpus-mix-builtin; fi &",
    )
    .await;
}

#[tokio::test]
async fn command_substitution_matches_foreground() {
    assert_job_matches_foreground(
        "",
        "if true; then x=$(cat /kaish-corpus-sub-missing); echo \"got $x\"; fi &",
    )
    .await;
}

#[tokio::test]
async fn standalone_test_fault_matches_foreground() {
    // A bare `[[ ]]`/`(( ))` is not itself backgroundable syntax (`&`
    // attaches to a pipeline); wrap it the same way the mixed-publication
    // case below does.
    assert_job_matches_foreground("", "if true; then [[ 1 -eq abc ]]; fi &").await;
}

/// The mixed-publication risk: an unpublished `[[ ]]` fault (it never
/// reaches a pipeline leaf) accumulated ahead of an already-published
/// builtin's stderr in the SAME `if` body. A single boolean "published" flag
/// on the aggregate must not let the builtin's already-sent text suppress a
/// republish, nor must it drop the test's own fault text.
#[tokio::test]
async fn test_fault_ahead_of_a_published_builtin_matches_foreground() {
    let job = job_stderr(
        "",
        "if true; then [[ 1 -eq abc ]]; cat /kaish-corpus-after-test-missing; fi &",
    )
    .await;
    let job = String::from_utf8_lossy(&job);
    assert_eq!(
        job.matches("kaish-corpus-after-test-missing").count(),
        1,
        "the builtin's error must appear exactly once: {job:?}"
    );
    assert!(
        job.to_lowercase().contains("numeric") || job.to_lowercase().contains("operand"),
        "the test fault's own diagnostic must reach the stream too: {job:?}"
    );
    assert_job_matches_foreground(
        "",
        "if true; then [[ 1 -eq abc ]]; cat /kaish-corpus-after-test-missing; fi &",
    )
    .await;
}

#[tokio::test]
async fn arithmetic_fault_matches_foreground() {
    assert_job_matches_foreground("", "if true; then (( 1 / 0 )); fi &").await;
}

#[tokio::test]
async fn for_loop_body_errors_match_foreground() {
    assert_job_matches_foreground(
        "",
        "for f in /kaish-corpus-a /kaish-corpus-b; do cat \"$f\"; done &",
    )
    .await;
}

#[tokio::test]
async fn while_loop_body_errors_match_foreground() {
    assert_job_matches_foreground(
        "",
        "i=0; while [[ $i -lt 2 ]]; do cat /kaish-corpus-while-missing; i=$((i+1)); done &",
    )
    .await;
}

#[tokio::test]
async fn stderr_redirect_to_file_matches_foreground() {
    let path = temp_path("2-redirect");
    let program = format!("cat /kaish-corpus-2redirect-missing 2>{} &", path.display());
    let job = job_stderr("", &program).await;
    assert!(job.is_empty(), "redirected stderr must not reach the stream: {job:?}");
    let written = std::fs::read_to_string(&path).expect("redirect target written");
    std::fs::remove_file(&path).expect("remove redirect target");
    assert!(written.contains("kaish-corpus-2redirect-missing"), "{written:?}");
}

#[tokio::test]
async fn both_streams_redirect_to_file_matches_foreground() {
    let path = temp_path("amp-redirect");
    let program = format!("cat /kaish-corpus-ampredirect-missing &>{} &", path.display());
    let job = job_stderr("", &program).await;
    assert!(job.is_empty(), "&>-redirected stderr must not reach the stream: {job:?}");
    let written = std::fs::read_to_string(&path).expect("redirect target written");
    std::fs::remove_file(&path).expect("remove redirect target");
    assert!(written.contains("kaish-corpus-ampredirect-missing"), "{written:?}");
}

#[tokio::test]
async fn merge_stderr_into_stdout_matches_foreground() {
    assert_job_matches_foreground("", "cat /kaish-corpus-merge-missing 2>&1 &").await;
}

#[tokio::test]
async fn merge_stdout_into_stderr_matches_foreground() {
    assert_job_matches_foreground("", "echo corpus-to-err >&2 &").await;
}

#[tokio::test]
async fn source_script_runtime_error_matches_foreground() {
    let path = temp_path("source-script");
    std::fs::write(&path, "cat /kaish-corpus-sourced-missing\n").expect("write script");
    let job_program = format!("source {} &", path.display());
    let fg_program = format!("source {}", path.display());
    let job = job_stderr("", &job_program).await;
    let fg = foreground_stderr("", &fg_program).await;
    std::fs::remove_file(&path).expect("remove script");
    assert_eq!(String::from_utf8_lossy(&job), fg, "{job_program:?}");
}

#[tokio::test]
async fn scatter_bad_option_matches_foreground() {
    assert_job_matches_foreground(
        "",
        "seq 1 3 | scatter --limit not-a-number | cat | gather &",
    )
    .await;
}

#[tokio::test]
async fn redirect_target_failure_matches_foreground() {
    assert_job_matches_foreground("", "echo hi > /kaish-no-such-dir/no-such-file &").await;
}
