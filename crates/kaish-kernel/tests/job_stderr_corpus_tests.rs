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
use std::sync::Arc;

use kaish_kernel::ast::Value;
use kaish_kernel::scheduler::JobId;
use kaish_kernel::{Kernel, KernelConfig};

/// Arc-wrapped so `timeout` can re-dispatch in the foreground run too.
fn kernel() -> Arc<Kernel> {
    let mut vars = HashMap::new();
    vars.insert(
        "PATH".to_string(),
        Value::String(std::env::var("PATH").unwrap_or_default()),
    );
    Kernel::new(KernelConfig::repl().with_initial_vars(vars))
        .expect("failed to create kernel")
        .into_arc()
}

fn temp_path(name: &str) -> PathBuf {
    std::env::temp_dir().join(format!("kaish-job-stderr-corpus-{name}-{}.txt", std::process::id()))
}

/// What a finished job left behind.
struct Job {
    stderr: String,
    stdout: String,
    result_err: String,
}

/// Run `setup` (may be empty) then `program` as job 1 to completion. The
/// stream must hold exactly the job's own `result.err`, whatever else the
/// caller checks.
async fn run_job(setup: &str, program: &str) -> Job {
    let kernel = kernel();
    if !setup.is_empty() {
        kernel.execute(setup).await.expect("setup failed");
    }
    kernel.execute(program).await.expect("spawn failed");
    let id = JobId(1);
    let result = kernel.jobs().wait(id).await.expect("job result");
    let stderr = kernel.jobs().read_stderr(id).await.expect("job must exist");
    let stdout = kernel.jobs().read_stdout(id).await.expect("job must exist");
    let job = Job {
        stderr: String::from_utf8(stderr).expect("utf-8 stderr"),
        stdout: String::from_utf8(stdout).expect("utf-8 stdout"),
        result_err: result.err,
    };
    assert_eq!(job.stderr, job.result_err, "stream must equal the job's result.err for {program:?}");
    job
}

/// Like `run_job`, for a job whose stream holds the bytes of its `result.err`
/// in the order they were produced rather than the order `err` lists them.
async fn run_job_any_order(setup: &str, program: &str) -> Job {
    let kernel = kernel();
    if !setup.is_empty() {
        kernel.execute(setup).await.expect("setup failed");
    }
    kernel.execute(program).await.expect("spawn failed");
    let id = JobId(1);
    let result = kernel.jobs().wait(id).await.expect("job result");
    let stderr = kernel.jobs().read_stderr(id).await.expect("job must exist");
    let stdout = kernel.jobs().read_stdout(id).await.expect("job must exist");
    let mut stream_bytes = stderr.clone();
    let mut result_bytes = result.err.clone().into_bytes();
    stream_bytes.sort_unstable();
    result_bytes.sort_unstable();
    assert!(stream_bytes == result_bytes, "stream {:?} must hold the bytes of result.err {:?}", String::from_utf8_lossy(&stderr), result.err);
    Job {
        stderr: String::from_utf8(stderr).expect("utf-8 stderr"),
        stdout: String::from_utf8(stdout).expect("utf-8 stdout"),
        result_err: result.err,
    }
}

/// The job's stderr stream after running `program` as job 1.
async fn job_stderr(setup: &str, program: &str) -> Vec<u8> {
    run_job(setup, program).await.stderr.into_bytes()
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

/// A fault that ends a block after earlier stderr was already published: the
/// earlier text is published once, and the fault's diagnostic once.
#[tokio::test]
async fn fault_ending_a_function_after_published_stderr_matches_foreground() {
    assert_job_matches_foreground(
        "f() { cat /kaish-corpus-fn-before-fault; x=$(( kaish_unset + 1 )); }",
        "f &",
    )
    .await;
}

/// The same fault in an `if` body. The foreground run returns it as an error,
/// so the stream is checked against the job's own result and by count.
#[tokio::test]
async fn fault_ending_an_if_body_after_published_stderr_is_published_once() {
    let job = run_job(
        "",
        "if true; then cat /kaish-corpus-if-before-fault; x=$(( kaish_unset + 1 )); fi &",
    )
    .await;
    assert_eq!(job.stderr.matches("kaish-corpus-if-before-fault").count(), 1, "{:?}", job.stderr);
    assert_eq!(job.stderr.matches("is unset;").count(), 1, "{:?}", job.stderr);
}

/// A fault on the left of `&&`/`||` becomes the block's error. Its message is
/// published when that error is rendered, not also when the operand returned.
#[rstest::rstest]
#[case::test_expression_and("f() { [[ 1 -eq abc ]] && echo x; }")]
#[case::arithmetic_or("f() { cat /kaish-corpus-or-before; (( 1 / 0 )) || echo y; }")]
#[case::test_builtin_and("f() { test 1 -eq abc && echo x; }")]
#[tokio::test]
async fn fault_on_the_left_of_a_chain_matches_foreground(#[case] setup: &str) {
    assert_job_matches_foreground(setup, "f &").await;
}

/// An `if` whose last statement faulted is itself a fault on the left of
/// `&&`, and its body already published that stderr as it ran. The error it
/// becomes must not publish the same text again.
#[tokio::test]
async fn published_fault_on_the_left_of_a_chain_matches_foreground() {
    assert_job_matches_foreground(
        "g() { if true; then cat /kaish-corpus-chain-body; [[ 1 -eq abc ]]; fi && echo y; }",
        "g &",
    )
    .await;
}

/// A substitution in a command's arguments runs before the command, so its
/// stderr comes first in the job's result, as in the foreground.
#[tokio::test]
async fn substitution_in_the_arguments_of_the_job_command_matches_foreground() {
    assert_job_matches_foreground("", "cat \"$(cat /kaish-corpus-sub-arg)\" &").await;
}

/// Text without a final newline gets one before drained stderr joins it. The
/// drained text already reached the stream live, so the newline arrives after
/// it there: same bytes, once each, in the order they were produced.
#[tokio::test]
async fn separator_before_drained_stderr_is_published_once() {
    let job = run_job_any_order(
        "",
        "if true; then sh -c 'printf kaish-corpus-no-newline >&2'; x=$(cat /kaish-corpus-after-no-newline); fi &",
    )
    .await;
    assert_eq!(job.stderr.matches('\n').count(), 2, "{:?}", job.stderr);
    let fg = foreground_stderr(
        "",
        "if true; then sh -c 'printf kaish-corpus-no-newline >&2'; x=$(cat /kaish-corpus-after-no-newline); fi",
    )
    .await;
    assert_eq!(job.result_err, fg);
}

/// bash expands a command's words before its redirects apply, so a
/// substitution's stderr is not redirected with the command's own.
#[rstest::rstest]
#[case::to_a_file("echo \"$(cat /kaish-corpus-sub-under-2file)\" 2>/dev/null &")]
#[case::merged("echo \"$(cat /kaish-corpus-sub-under-merge)\" 2>&1 &")]
#[tokio::test]
async fn substitution_under_a_stderr_redirect_matches_foreground(#[case] program: &str) {
    let job = run_job("", program).await;
    assert!(job.stderr.contains("kaish-corpus-sub-under"), "{:?}", job.stderr);
    assert_job_matches_foreground("", program).await;
}

/// A function whose body mixes every kind of stderr producer: a builtin (its
/// leaf publishes), an external (its tee publishes), a `[[ ]]` fault (the
/// statement publishes), and an assignment fault that ends the body (the
/// call renders it).
const MIXED_FUNCTION: &str = "mixed() { cat /kaish-corpus-mixed-builtin; sh -c 'echo kaish-corpus-mixed-external >&2'; [[ 1 -eq abc ]]; x=$(( kaish_unset + 1 )); }";

/// Run `program` in the foreground on a fresh kernel after `setup`; return
/// stdout and stderr.
async fn foreground_streams(setup: &str, program: &str) -> (String, String) {
    let kernel = kernel();
    kernel.execute(setup).await.expect("setup failed");
    let result = kernel.execute(program).await.expect("execute failed");
    (result.text_out().into_owned(), result.err)
}

#[rstest::rstest]
#[case::bare("mixed")]
#[case::under_timeout("timeout 5 mixed")]
#[tokio::test]
async fn mixed_function_stderr_is_exact(#[case] call: &str) {
    let (_, fg_err) = foreground_streams(MIXED_FUNCTION, call).await;
    let job = run_job(MIXED_FUNCTION, &format!("{call} &")).await;
    assert_eq!(job.stderr, fg_err, "{call}");
    for marker in ["kaish-corpus-mixed-builtin", "kaish-corpus-mixed-external", "\"abc\"", "is unset;"] {
        assert_eq!(job.stderr.matches(marker).count(), 1, "{call}: {marker} in {:?}", job.stderr);
    }
}

#[rstest::rstest]
#[case::bare("mixed")]
#[case::under_timeout("timeout 5 mixed")]
#[tokio::test]
async fn mixed_function_with_stderr_to_a_file_keeps_the_stream_empty(#[case] call: &str) {
    let path = temp_path(&format!("mixed-{}", call.replace(' ', "-")));
    let program = format!("{call} 2>{}", path.display());
    foreground_streams(MIXED_FUNCTION, &program).await;
    let fg_file = std::fs::read_to_string(&path).expect("foreground redirect target");
    std::fs::remove_file(&path).expect("remove redirect target");
    let job = run_job(MIXED_FUNCTION, &format!("{program} &")).await;
    let job_file = std::fs::read_to_string(&path).expect("job redirect target");
    std::fs::remove_file(&path).expect("remove redirect target");
    assert_eq!(job.stderr, "", "{call}: redirected stderr leaked into the stream");
    assert_eq!(job_file, fg_file, "{call}");
    assert_eq!(job_file.matches("kaish-corpus-mixed-builtin").count(), 1, "{job_file:?}");
    assert_eq!(job_file.matches("is unset;").count(), 1, "{job_file:?}");
}

#[rstest::rstest]
#[case::bare("mixed 2>&1")]
#[case::under_timeout("timeout 5 mixed 2>&1")]
#[tokio::test]
async fn mixed_function_with_stderr_merged_puts_it_on_stdout_once(#[case] call: &str) {
    let (fg_out, fg_err) = foreground_streams(MIXED_FUNCTION, call).await;
    let job = run_job(MIXED_FUNCTION, &format!("{call} &")).await;
    assert_eq!(job.stderr, fg_err, "{call}");
    assert_eq!(job.stderr, "", "{call}: merged stderr must not reach the stderr stream");
    assert_eq!(job.stdout, fg_out, "{call}");
    assert_eq!(job.stdout.matches("kaish-corpus-mixed-external").count(), 1, "{:?}", job.stdout);
    assert_eq!(job.stdout.matches("is unset;").count(), 1, "{:?}", job.stdout);
}

/// `timeout`'s note leads its `err`, but the inner command's stderr reached
/// the stream first. The stream holds the same lines, each once, in the order
/// they happened.
#[rstest::rstest]
#[case::bare_sleep("timeout 300ms sleep 5 &")]
#[case::function_with_published_stderr("timeout 300ms slow &")]
#[case::external_with_published_stderr("timeout 300ms -- sh -c 'echo kaish-corpus-before-timeout >&2; sleep 5' &")]
#[tokio::test]
async fn timeout_note_is_published_once_after_the_inner_stderr(#[case] program: &str) {
    let setup = "slow() { cat /kaish-corpus-before-timeout; sleep 5; }";
    let kernel = kernel();
    kernel.execute(setup).await.expect("setup failed");
    kernel.execute(program).await.expect("spawn failed");
    let id = JobId(1);
    let result = kernel.jobs().wait(id).await.expect("job result");
    let stream = String::from_utf8(kernel.jobs().read_stderr(id).await.expect("job must exist")).expect("utf-8");
    assert_eq!(result.code, 124, "{program}: {result:?}");
    assert!(result.err.starts_with("timeout: timed out after 300ms\n"), "{:?}", result.err);
    assert!(stream.ends_with("timeout: timed out after 300ms\n"), "{program}: the note is the last thing published: {stream:?}");
    let mut stream_lines: Vec<&str> = stream.lines().collect();
    let mut result_lines: Vec<&str> = result.err.lines().collect();
    stream_lines.sort_unstable();
    result_lines.sort_unstable();
    assert_eq!(stream_lines, result_lines, "{program}: stream {stream:?} vs result {:?}", result.err);
    assert!(stream.matches("kaish-corpus-before-timeout").count() <= 1, "{stream:?}");
    if program.contains("slow") || program.contains("sh -c") {
        assert_eq!(stream.matches("kaish-corpus-before-timeout").count(), 1, "{stream:?}");
    }
}

/// A condition command that faults has no truth value; its diagnostic is the
/// error, printed once — in the foreground and on a job's stream.
#[rstest::rstest]
#[case::if_condition("if test 1 -eq abc; then echo x; fi")]
#[case::while_condition("while test 1 -eq abc; do echo x; done")]
#[tokio::test]
async fn faulting_condition_command_prints_its_diagnostic_once(#[case] body: &str) {
    let setup = format!("f() {{ {body}; }}");
    let fg = foreground_stderr(&setup, "f").await;
    assert_eq!(fg.matches("\"abc\"").count(), 1, "foreground: {fg:?}");
    for program in [format!("{body} &"), "f &".to_string()] {
        let job = run_job(&setup, &program).await;
        assert_eq!(job.stderr.matches("\"abc\"").count(), 1, "{program}: {:?}", job.stderr);
    }
    assert_job_matches_foreground(&setup, "f &").await;
}

/// A negated statement drains the stderr channel itself before flipping its
/// code; a `|` pipe's non-last stage reaches the job stream only through it.
#[tokio::test]
async fn negated_pipeline_nested_in_a_compound_matches_foreground() {
    assert_job_matches_foreground(
        "",
        "if true; then ! cat /kaish-corpus-negated-missing | wc -l; fi &",
    )
    .await;
}

/// A fault under `!` becomes the block's error. Its message is published when
/// that error is rendered, not also when the negated body returned.
#[rstest::rstest]
#[case::test_expression("f() { ! [[ 1 -eq abc ]]; }")]
#[case::published_body("f() { ! if true; then cat /kaish-corpus-negated-body; [[ 1 -eq abc ]]; fi; }")]
#[tokio::test]
async fn negated_fault_matches_foreground(#[case] setup: &str) {
    assert_job_matches_foreground(setup, "f &").await;
}

#[tokio::test]
async fn negated_builtin_error_matches_foreground() {
    assert_job_matches_foreground("", "if true; then ! cat /kaish-corpus-negated-builtin; fi &").await;
}
