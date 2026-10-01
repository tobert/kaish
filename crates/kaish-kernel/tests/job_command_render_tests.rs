//! `/v/jobs/N/command` shows the command with the plan's renderer, so a job
//! reads the same as `plan_program` would render it, and re-lexes to the
//! same words.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{plan_program, Kernel, KernelConfig};
use rstest::rstest;

async fn job_command(source: &str) -> String {
    let kernel = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let started = kernel.execute(source).await.expect("starts");
    assert!(started.ok(), "{source}: {}", started.err);
    let shown = kernel.execute("cat /v/jobs/1/command").await.expect("reads");
    assert!(shown.ok(), "cat failed: {}", shown.err);
    shown.text_out().trim_end().to_string()
}

#[rstest]
#[case::quoted_glob("echo '*.rs' &", "echo '*.rs'")]
#[case::quoted_number("echo \"0\" &", "echo '0'")]
#[case::quote_inside("echo \"it's\" &", "echo 'it'\\''s'")]
#[case::bare_glob("echo *.rs &", "echo *.rs")]
#[case::interpolation("echo \"a$X\" &", "echo \"a${X}\"")]
#[case::named("echo --tail=\"5\" &", "echo --tail='5'")]
#[tokio::test]
async fn a_job_command_is_the_plan_rendering(#[case] source: &str, #[case] expected: &str) {
    assert_eq!(job_command(source).await, expected);
}

/// The job string and the plan's `rendered` come from one renderer.
#[tokio::test]
async fn a_job_command_matches_plan_rendered() {
    let source = "echo 'a b' \"0\" '$x' &";
    let plans = plan_program(source).expect("parses");
    let rendered = plans[0].plan.rendered.trim_end_matches(" &").to_string();
    assert_eq!(job_command(source).await, rendered);
}
