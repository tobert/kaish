//! Every diagnostic kaish itself writes ends its own line.
//!
//! `kaish -c 'cat /nope'` printed `cat: /nope: … (os error 2)` with no trailing
//! newline, so the next thing printed ran into it (#363). The defect is a class,
//! not one builtin: kaish's own failure messages are built without a terminating
//! newline and rendered verbatim, so any following output on the same stream
//! fuses onto the message. GNU tools end each diagnostic line; kaish does too.
//!
//! Scope: this pins kaish's OWN diagnostics — the messages builtins and the
//! kernel mint via `ExecResult::failure`. An external command's stderr is
//! pass-through data and keeps its bytes exactly as produced (a program that
//! dies mid-line stays mid-line, as in bash), so it is not covered here.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

/// A diagnostic ends its own line: exactly one trailing `\n`.
fn assert_terminated(what: &str, err: &str) {
    assert!(
        err.ends_with('\n'),
        "{what}: diagnostic must end with a newline, got {err:?}"
    );
    assert!(
        !err.ends_with("\n\n"),
        "{what}: diagnostic must end with exactly one newline, got {err:?}"
    );
}

async fn kernel() -> std::sync::Arc<Kernel> {
    Kernel::new(KernelConfig::isolated())
        .expect("failed to create kernel")
        .into_arc()
}

#[tokio::test]
async fn builtin_single_operand_error_terminates() {
    let k = kernel().await;
    let r = k.execute("cat /nope").await.expect("execute");
    assert!(!r.ok());
    assert_terminated("cat /nope", &r.err);
}

#[tokio::test]
async fn builtin_multi_operand_join_terminates() {
    // wc joins one message per bad operand with '\n'; the whole block must
    // still end its line.
    let k = kernel().await;
    let r = k.execute("wc /nope /also-nope").await.expect("execute");
    assert!(!r.ok());
    assert_terminated("wc on two missing files", &r.err);
}

#[tokio::test]
async fn stat_last_err_terminates() {
    let k = kernel().await;
    let r = k.execute("stat /nope").await.expect("execute");
    assert!(!r.ok());
    assert_terminated("stat /nope", &r.err);
}

#[tokio::test]
async fn readlink_last_err_terminates() {
    let k = kernel().await;
    let r = k.execute("readlink /nope").await.expect("execute");
    assert!(!r.ok());
    assert_terminated("readlink /nope", &r.err);
}

#[tokio::test]
async fn realpath_last_err_terminates() {
    let k = kernel().await;
    let r = k.execute("realpath /nope").await.expect("execute");
    assert!(!r.ok());
    assert_terminated("realpath /nope", &r.err);
}

#[tokio::test]
async fn command_not_found_terminates() {
    let k = kernel().await;
    let r = k.execute("definitely-not-a-real-command").await.expect("execute");
    assert!(!r.ok());
    assert_terminated("command not found", &r.err);
}

#[tokio::test]
async fn read_no_input_error_terminates() {
    let k = kernel().await;
    let r = k.execute("read x").await.expect("execute");
    assert!(!r.ok());
    assert_terminated("read with no stdin", &r.err);
}

#[tokio::test]
async fn a_failure_keeps_exactly_one_newline_when_it_already_has_one() {
    // ls already terminates each operand message; the result must not grow a
    // second blank line on top.
    let k = kernel().await;
    let r = k.execute("ls /nope").await.expect("execute");
    assert!(!r.ok());
    assert_terminated("ls /nope", &r.err);
}
