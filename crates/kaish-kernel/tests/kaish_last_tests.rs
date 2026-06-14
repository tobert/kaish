//! End-to-end tests for the `kaish-last` builtin.
//!
//! The builtin reads the previous command's ExecResult and emits either
//! its `.data` (as JSON) or its captured `.out` (raw stdout). It replaces
//! the removed `${?.data}` field access on `$?`.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated().with_skip_validation(true))
        .expect("failed to create kernel")
        .into_arc()
}

#[tokio::test]
async fn emits_data_as_json_from_jq() {
    // Discard jq's stdout so r.text_out() reflects only kaish-last's output.
    let k = setup().await;
    let r = k
        .execute(r#"echo '["a","b","c"]' | jq -r '.[]' > /dev/null; kaish-last"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), r#"["a","b","c"]"#);
}

#[tokio::test]
async fn emits_data_as_json_from_seq() {
    let k = setup().await;
    let r = k
        .execute(r#"seq 1 5 > /dev/null; kaish-last"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "[1,2,3,4,5]");
}

#[tokio::test]
async fn data_can_be_piped_into_jq() {
    // kaish-last as first stage of a pipeline emits .data to stdout;
    // jq consumes it.
    let k = setup().await;
    let r = k
        .execute(r#"seq 1 5 > /dev/null; kaish-last | jq '.[2]'"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "3");
}

#[tokio::test]
async fn capture_via_command_substitution() {
    let k = setup().await;
    let r = k
        .execute(r#"seq 1 3 > /dev/null; DATA=$(kaish-last); echo "$DATA""#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "[1,2,3]");
}

#[tokio::test]
async fn falls_back_to_stdout_when_no_data() {
    // echo doesn't populate .data — kaish-last should emit raw stdout.
    // Both echo and kaish-last write to the same captured stream, so the
    // result holds two copies of the line.
    let k = setup().await;
    let r = k
        .execute(r#"echo hello world; kaish-last"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "hello world\nhello world");
}

#[tokio::test]
async fn exits_one_when_no_output() {
    // `true` succeeds with no .data and no .out.
    let k = setup().await;
    let r = k
        .execute(r#"true; kaish-last"#)
        .await
        .expect("script ran");
    assert!(!r.ok(), "expected non-zero exit; got code: {}", r.code);
    assert_eq!(r.code, 1);
    assert!(r.err.contains("no data"), "stderr: {}", r.err);
}

#[tokio::test]
async fn ignores_prior_exit_code_and_uses_data_or_stdout() {
    // Even after a failed command, kaish-last operates on whatever .data or
    // .out is present. Its own exit code is independent of the prior code.
    let k = setup().await;
    let r = k
        .execute(r#"false; kaish-last"#)
        .await
        .expect("script ran");
    // `false` has no .data and no .out; expect kaish-last to exit 1.
    assert_eq!(r.code, 1);
    assert!(r.err.contains("no data"), "stderr: {}", r.err);
}

#[tokio::test]
async fn refuses_with_piped_stdin() {
    // `seq 1 5 | kaish-last` would silently surface pre-pipeline last_result.
    // Refuse rather than mislead.
    let k = setup().await;
    let r = k
        .execute(r#"seq 1 5 | kaish-last"#)
        .await
        .expect("script ran");
    assert!(!r.ok(), "expected non-zero exit; got code: {}", r.code);
    assert_eq!(r.code, 2);
    assert!(r.err.contains("piped stdin"), "stderr: {}", r.err);
}
