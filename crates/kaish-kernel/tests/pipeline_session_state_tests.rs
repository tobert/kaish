//! A pipeline's last stage returns its session changes to the statement.
//!
//! The last stage acts like a statement run on its own: `echo x | read v`,
//! `echo x | cd /tmp`, and `echo x | alias g=grep` keep their change afterward.
//! The pipeline join returned scope, cwd, prev_cwd, and aliases, but dropped
//! ignore config and output limit, which a lone statement returns.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{IgnoreConfig, Kernel, KernelConfig, OutputLimitConfig};

fn kernel() -> Kernel {
    Kernel::new(
        KernelConfig::isolated()
            .with_output_limit(OutputLimitConfig::agent().in_memory())
            .with_ignore_config(IgnoreConfig::agent()),
    )
    .expect("kernel")
}

/// Control: the last stage's alias change survives the pipeline.
async fn assert_last_stage_alias_survives(kernel: &Kernel) {
    let control = kernel.execute("echo x | alias greet='echo hi'").await.expect("execute");
    assert_eq!(control.code, 0, "{control:?}");
    let greet = kernel.execute("greet").await.expect("execute");
    assert_eq!(greet.text_out(), "hi\n", "control: the last stage's alias must survive the pipeline");
}

#[tokio::test]
async fn last_pipeline_stage_output_limit_change_returns() {
    let kernel = kernel();
    assert_last_stage_alias_survives(&kernel).await;

    kernel.execute("echo x | kaish-output-limit off").await.expect("execute");
    let limit = kernel.execute("kaish-output-limit").await.expect("execute");
    assert!(
        !limit.text_out().contains("8K"),
        "the last stage's output-limit change was dropped while its alias change survived: {}",
        limit.text_out()
    );
}

#[tokio::test]
async fn last_pipeline_stage_ignore_change_returns() {
    let kernel = kernel();
    assert_last_stage_alias_survives(&kernel).await;

    let before = kernel.execute("kaish-ignore").await.expect("execute");
    assert!(before.text_out().contains(".gitignore"), "control: the agent ignore config lists .gitignore: {}", before.text_out());
    kernel.execute("echo x | kaish-ignore clear").await.expect("execute");
    let after = kernel.execute("kaish-ignore").await.expect("execute");
    assert!(
        !after.text_out().contains(".gitignore"),
        "the last stage's ignore change was dropped while its alias change survived: {}",
        after.text_out()
    );
}
