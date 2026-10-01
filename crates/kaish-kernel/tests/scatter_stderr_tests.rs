//! The stages before `scatter` write stderr like any other stage: it reaches
//! the statement's stderr (and a job's stderr stream) once, whether one stage
//! or several come before `scatter`.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(all(feature = "localfs", feature = "subprocess"))]

use std::collections::HashMap;

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

const SOURCE: &str = "src() { echo item; echo pre-warning >&2; }\n";

#[rstest::rstest]
#[case::one_builtin_stage("src | scatter | echo got | gather")]
#[case::one_external_stage("sh -c 'echo item; echo pre-warning >&2' | scatter | echo got | gather")]
#[case::two_stages("src | cat | scatter | echo got | gather")]
#[case::one_stage_and_post_gather("src | scatter | echo got | gather | cat")]
#[tokio::test]
async fn pre_scatter_stderr_reaches_the_statement_once(#[case] pipeline: &str) {
    let kernel = kernel();
    let result = kernel.execute(&format!("{SOURCE}{pipeline}")).await.expect("execute");
    assert_eq!(result.code, 0, "pipeline failed: {:?}", result.err);
    assert!(result.text_out().contains("got"), "workers ran: {:?}", result.text_out());
    assert_eq!(
        result.err.matches("pre-warning").count(),
        1,
        "pre_scatter stderr must appear exactly once: {:?}",
        result.err
    );
}

#[rstest::rstest]
#[case::one_builtin_stage("src | scatter | echo got | gather &")]
#[case::one_external_stage("sh -c 'echo item; echo pre-warning >&2' | scatter | echo got | gather &")]
#[case::two_stages("src | cat | scatter | echo got | gather &")]
#[tokio::test]
async fn pre_scatter_stderr_reaches_a_jobs_stream_once(#[case] pipeline: &str) {
    let kernel = kernel();
    kernel.execute(&format!("{SOURCE}{pipeline}")).await.expect("spawn");
    let id = JobId(1);
    let result = kernel.jobs().wait(id).await.expect("job result");
    let stream = String::from_utf8_lossy(&kernel.jobs().read_stderr(id).await.expect("job exists")).into_owned();
    assert_eq!(stream.matches("pre-warning").count(), 1, "job stream: {stream:?}");
    assert_eq!(result.err.matches("pre-warning").count(), 1, "job result: {:?}", result.err);
}
