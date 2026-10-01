//! The stages before `scatter` write stderr like any other stage: it reaches
//! the statement's stderr (and a job's stderr stream) once, whether one stage
//! or several come before `scatter`.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(all(feature = "localfs", feature = "subprocess"))]

use std::collections::HashMap;
use std::time::{Duration, Instant};

use kaish_kernel::ast::Value;
use kaish_kernel::scheduler::JobId;
use kaish_kernel::{ExecuteOptions, Kernel, KernelConfig};

fn kernel() -> Kernel {
    let mut vars = HashMap::new();
    vars.insert(
        "PATH".to_string(),
        Value::String(std::env::var("PATH").unwrap_or_default()),
    );
    Kernel::new(KernelConfig::repl().with_initial_vars(vars)).expect("failed to create kernel")
}

const SOURCE: &str = "src() { echo item; echo pre-warning >&2; }\n\
src_fail() { echo item; echo pre-warning >&2; return 1; }\n";

/// Cases: (pipeline, stderr marker, exit code). The worker prints `got`;
/// `gather --lines` keeps stdout free of the items, so stdout never holds a
/// marker unless stderr leaked into it.
const WORKERS: &str = "scatter | echo got | gather --lines";

fn cases() -> Vec<(String, &'static str, i64)> {
    vec![
        // A bare builtin: its stderr has no earlier writer than the runner.
        (format!("grep '\\d' <<< \"d\" | {WORKERS}"), "stray", 0),
        (format!("src | {WORKERS}"), "pre-warning", 0),
        (format!("sh -c 'echo item; echo pre-warning >&2' | {WORKERS}"), "pre-warning", 0),
        (format!("src | cat | {WORKERS}"), "pre-warning", 0),
        (format!("src | {WORKERS} | cat"), "pre-warning", 0),
        // A failing stage returns before scatter runs; its stderr arrives once.
        (format!("src_fail | {WORKERS}"), "pre-warning", 1),
        (format!("echo item | src_fail | {WORKERS}"), "pre-warning", 1),
        (format!("cat /pre-missing-file | {WORKERS}"), "pre-missing-file", 1),
    ]
}

#[tokio::test]
async fn pre_scatter_stderr_reaches_the_statement_once() {
    for (pipeline, marker, code) in cases() {
        let kernel = kernel();
        let result = kernel.execute(&format!("{SOURCE}{pipeline}")).await.expect("execute");
        assert_eq!(result.code, code, "{pipeline}: exit code, err {:?}", result.err);
        assert_eq!(result.err.matches(marker).count(), 1, "{pipeline}: stderr {:?}", result.err);
        assert!(!result.text_out().contains(marker), "{pipeline}: stdout {:?}", result.text_out());
        if code == 0 {
            assert_eq!(result.text_out().trim(), "got", "{pipeline}: workers ran");
        }
    }
}

async fn streams(kernel: &Kernel, id: JobId) -> (String, String, kaish_kernel::interpreter::ExecResult) {
    let result = kernel.jobs().wait(id).await.expect("job result");
    let stdout = String::from_utf8_lossy(&kernel.jobs().read_stdout(id).await.expect("job exists")).into_owned();
    let stderr = String::from_utf8_lossy(&kernel.jobs().read_stderr(id).await.expect("job exists")).into_owned();
    (stdout, stderr, result)
}

#[tokio::test]
async fn pre_scatter_stderr_reaches_a_jobs_stream_once() {
    for (pipeline, marker, code) in cases() {
        let kernel = kernel();
        kernel.execute(&format!("{SOURCE}{pipeline} &")).await.expect("spawn");
        let (stdout, stderr, result) = streams(&kernel, JobId(1)).await;
        assert_eq!(result.code, code, "{pipeline}: exit code");
        assert_eq!(stderr.matches(marker).count(), 1, "{pipeline}: stream {stderr:?}");
        assert_eq!(result.err.matches(marker).count(), 1, "{pipeline}: result {:?}", result.err);
        assert!(!stdout.contains(marker), "{pipeline}: stdout {stdout:?}");
    }
}

/// A whole-program job writes stderr per statement, not live.
#[tokio::test]
async fn pre_scatter_stderr_reaches_a_whole_program_jobs_stream_once() {
    for (pipeline, marker, code) in cases() {
        let kernel = kernel();
        let id = kernel
            .execute_background_with_options(&format!("{SOURCE}{pipeline}"), ExecuteOptions::new())
            .await
            .expect("program accepted");
        let (stdout, stderr, result) = streams(&kernel, id).await;
        assert_eq!(result.code, code, "{pipeline}: exit code");
        assert_eq!(stderr.matches(marker).count(), 1, "{pipeline}: stream {stderr:?}");
        assert_eq!(result.err.matches(marker).count(), 1, "{pipeline}: result {:?}", result.err);
        assert!(!stdout.contains(marker), "{pipeline}: stdout {stdout:?}");
    }
}

/// The stage's stderr reaches the job's stream when the stage finishes, not
/// when the whole pipeline does.
#[tokio::test]
async fn pre_scatter_stderr_is_live_while_the_workers_run() {
    for (pre, marker) in [
        ("grep '\\d' <<< \"d\"", "stray"),
        ("src", "pre-warning"),
        ("sh -c 'echo item; echo pre-warning >&2'", "pre-warning"),
    ] {
        let kernel = kernel();
        let program = format!("{SOURCE}{pre} | scatter | sh -c 'sleep 3; echo got' | gather --lines &");
        kernel.execute(&program).await.expect("spawn");
        let id = JobId(1);
        let deadline = Instant::now() + Duration::from_secs(10);
        loop {
            let status = kernel.jobs().get_status_string(id).await.expect("job exists");
            let stderr = String::from_utf8_lossy(&kernel.jobs().read_stderr(id).await.expect("job exists")).into_owned();
            if stderr.contains(marker) {
                assert_eq!(status, "running", "{pre}: stderr arrived only after the job ended");
                assert_eq!(stderr.matches(marker).count(), 1, "{pre}: {stderr:?}");
                break;
            }
            assert_eq!(status, "running", "{pre}: job ended before its stderr reached the stream");
            assert!(Instant::now() < deadline, "{pre}: stderr never reached the stream");
            tokio::time::sleep(Duration::from_millis(20)).await;
        }
        let (_, stderr, result) = streams(&kernel, id).await;
        assert_eq!(result.code, 0);
        assert_eq!(stderr.matches(marker).count(), 1, "{pre}: {stderr:?}");
    }
}

#[tokio::test]
async fn redirected_pre_scatter_stderr_stays_out_of_statement_and_job_streams() {
    for producer in ["src", "sh -c 'echo item; echo pre-warning >&2'"] {
        let pipeline = format!("{producer} 2>/dev/null | {WORKERS}");
        let k = kernel();
        let result = k.execute(&format!("{SOURCE}{pipeline}")).await.expect("execute");
        assert_eq!(result.code, 0);
        assert_eq!(result.text_out().trim(), "got");
        assert!(result.err.is_empty(), "{pipeline}: {:?}", result.err);
        k.execute(&format!("{pipeline} &")).await.expect("spawn");
        let (stdout, stderr, result) = streams(&k, JobId(1)).await;
        assert_eq!(result.code, 0);
        assert_eq!(stdout.trim(), "got");
        assert!(stderr.is_empty(), "{pipeline}: {stderr:?}");
        assert!(result.err.is_empty(), "{pipeline}: {:?}", result.err);
    }
}
