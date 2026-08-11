//! A gate raised during expression evaluation surfaces like any other gate
//! (`docs/approval-ledger.md` §I.5): exit 2, the request on
//! `ExecResult.approval`, and **nothing of the enclosing statement runs on
//! the missing result**. Found live: `echo "r: $(rm f)"` under
//! `set -o approvals` held the `rm` (the file survived, the request was
//! recorded) but expanded the substitution to empty, printed `r: `, and
//! exited 0 — a stranded request and a silent wrong-assumption continuation.
//!
//! Every test drives real source through `kernel.execute()`, and every
//! context asserts three things: exit 2, the request surfaced on
//! `.approval`, and the world untouched by anything downstream of the held
//! operation.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

// KernelConfig::repl() mounts the real filesystem.
#![cfg(feature = "localfs")]

use std::path::Path;

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{Kernel, KernelConfig};

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("gate-surfacing-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// A kernel with approvals on and a `victim.txt` fixture in its cwd.
async fn approvals_kernel(dir: &Path) -> Kernel {
    std::fs::write(dir.join("victim.txt"), "keep me").expect("write fixture");
    let (kernel, _authority) = Kernel::build(
        KernelConfig::repl()
            .with_cwd(dir.to_path_buf())
            .with_approvals(false)
            .with_trash(false),
    )
    .expect("kernel");
    kernel
        .execute("set -o approvals")
        .await
        .expect("enabling approvals");
    kernel
}

/// The three assertions every held context must satisfy.
fn assert_held(result: &ExecResult, dir: &Path, context: &str) {
    assert_eq!(
        result.code, 2,
        "{context}: a held statement exits 2, got {} (err: {})",
        result.code, result.err
    );
    assert!(
        result.approval.is_some(),
        "{context}: the pending request must ride ExecResult.approval"
    );
    assert!(
        dir.join("victim.txt").exists(),
        "{context}: the gate must have held the rm"
    );
}

// ── Substitution positions ──────────────────────────────────────────────

#[tokio::test]
async fn a_bare_substitution_surfaces_the_held_request() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    let result = kernel.execute("X=$(rm victim.txt)").await.expect("execute");
    assert_held(&result, dir.path(), "bare $() in assignment");
    // The assignment must not have happened on the empty expansion.
    assert!(kernel.get_var("X").await.is_none(), "X must not be assigned");
}

#[tokio::test]
async fn a_quoted_interpolation_surfaces_the_held_request() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    let result = kernel
        .execute(r#"echo "r: $(rm victim.txt)""#)
        .await
        .expect("execute");
    assert_held(&result, dir.path(), "quoted $() in echo");
    assert!(
        !result.text_out().contains("r:"),
        "echo must not run on the empty expansion: {:?}",
        result.text_out()
    );
}

#[tokio::test]
async fn a_nested_substitution_surfaces_the_held_request() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    let result = kernel
        .execute("echo $(echo $(rm victim.txt))")
        .await
        .expect("execute");
    assert_held(&result, dir.path(), "nested $()");
}

#[tokio::test]
async fn a_redirect_target_substitution_surfaces_the_held_request() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    let result = kernel
        .execute("echo hi > $(rm victim.txt)")
        .await
        .expect("execute");
    assert_held(&result, dir.path(), "$() as redirect target");
}

#[tokio::test]
async fn for_items_substitution_holds_before_the_first_iteration() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    let result = kernel
        .execute("for x in $(rm victim.txt); do touch ran.txt; done")
        .await
        .expect("execute");
    assert_held(&result, dir.path(), "$() in for items");
    assert!(
        !dir.path().join("ran.txt").exists(),
        "no iteration may run on the empty expansion"
    );
}

#[tokio::test]
async fn a_case_scrutinee_substitution_surfaces_the_held_request() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    let result = kernel
        .execute("case $(rm victim.txt) in *) touch matched.txt ;; esac")
        .await
        .expect("execute");
    assert_held(&result, dir.path(), "$() as case scrutinee");
    assert!(
        !dir.path().join("matched.txt").exists(),
        "no case branch may run on the empty expansion"
    );
}

#[tokio::test]
async fn an_env_prefix_substitution_surfaces_the_held_request() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    let result = kernel
        .execute("V=$(rm victim.txt) echo hi")
        .await
        .expect("execute");
    assert_held(&result, dir.path(), "$() in env prefix");
    assert!(
        !result.text_out().contains("hi"),
        "the prefixed command must not run"
    );
}

// ── Conditions ──────────────────────────────────────────────────────────

#[tokio::test]
async fn a_held_if_condition_runs_neither_branch() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    let result = kernel
        .execute("if rm victim.txt; then touch then.txt; else touch else.txt; fi")
        .await
        .expect("execute");
    assert_held(&result, dir.path(), "direct command as if condition");
    assert!(
        !dir.path().join("then.txt").exists() && !dir.path().join("else.txt").exists(),
        "pending is not failure: neither branch may run"
    );
}

#[tokio::test]
async fn a_held_while_condition_runs_no_iteration() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    let result = kernel
        .execute("while rm victim.txt; do touch looped.txt; done")
        .await
        .expect("execute");
    assert_held(&result, dir.path(), "direct command as while condition");
    assert!(!dir.path().join("looped.txt").exists());
}

// ── Chains ──────────────────────────────────────────────────────────────

#[tokio::test]
async fn an_or_chain_does_not_run_the_fallback_on_pending() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    let result = kernel
        .execute("rm victim.txt || touch fallback.txt")
        .await
        .expect("execute");
    assert_held(&result, dir.path(), "|| chain");
    assert!(
        !dir.path().join("fallback.txt").exists(),
        "pending is not failure: the fallback may not run"
    );
}

#[tokio::test]
async fn an_and_chain_does_not_run_the_right_side_on_pending() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    let result = kernel
        .execute("rm victim.txt && touch ok.txt")
        .await
        .expect("execute");
    assert_held(&result, dir.path(), "&& chain");
    assert!(!dir.path().join("ok.txt").exists());
}

// ── Pinned siblings (expected to pass before the fix; they guard the
//    contexts the block executor already carries) ───────────────────────

#[tokio::test]
async fn a_pipeline_mid_stage_gate_still_overrides_the_last_stage() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    let result = kernel.execute("rm victim.txt | cat").await.expect("execute");
    assert_held(&result, dir.path(), "gated mid-pipeline stage");
}

#[tokio::test]
async fn a_user_tool_body_gate_surfaces_at_the_call() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    kernel
        .execute("zap() { rm victim.txt; }")
        .await
        .expect("define tool");
    let result = kernel.execute("zap").await.expect("execute");
    assert_held(&result, dir.path(), "gate inside a user tool body");
}

/// The stash-based chain guard: the gate is inside the left side's `$()`,
/// so its typed error gets stringified by the argument builder and the
/// left result carries no `.approval` — only the stash knows. The fallback
/// still must not run.
#[tokio::test]
async fn an_eval_hold_in_a_chain_left_side_does_not_run_the_fallback() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    let result = kernel
        .execute("echo $(rm victim.txt) || touch fallback.txt")
        .await
        .expect("execute");
    assert_held(&result, dir.path(), "stash-based || guard");
    assert!(
        !dir.path().join("fallback.txt").exists(),
        "pending is not failure: the fallback may not run on a stringified hold"
    );
}

/// The argv door takes the slot too (the review's find): a user tool whose
/// body holds inside `$(…)`, invoked via `execute_argv`, surfaces the held
/// result — and the slot must not strand for the next call to mis-take.
#[tokio::test]
async fn execute_argv_surfaces_a_tool_body_hold_and_strands_nothing() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    kernel
        .execute("zap() { echo $(rm victim.txt); }")
        .await
        .expect("define tool");

    let result = kernel.execute_argv("zap", &[]).await.expect("execute_argv");
    assert_held(&result, dir.path(), "execute_argv over a holding tool body");

    // The slot is empty again: an innocent follow-up statement runs clean.
    let after = kernel.execute("echo fine").await.expect("execute");
    assert_eq!(after.code, 0, "a stale slot must not halt the next call: {}", after.err);
    assert_eq!(after.text_out().trim(), "fine");
}

// ── The stranded-request regression itself ──────────────────────────────

#[tokio::test]
async fn the_original_repro_no_longer_strands_the_request() {
    let dir = tempdir();
    let kernel = approvals_kernel(dir.path()).await;
    let result = kernel
        .execute(r#"echo "r: $(rm victim.txt)"
touch after.txt"#)
        .await
        .expect("execute");
    assert_held(&result, dir.path(), "original repro");
    assert!(
        !dir.path().join("after.txt").exists(),
        "the program halts at the held statement (§I.5)"
    );
}
