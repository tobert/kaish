//! GH #222: `run_scatter_gather`'s own option-parsing failures must honor
//! `--json`.
//!
//! `PipelineRunner::run_scatter_gather` (crates/kaish-kernel/src/scheduler/pipeline.rs)
//! pulls scatter/gather's own args out of the pipeline and parses them
//! directly (`build_tool_args` + `parse_scatter_options`/`parse_gather_options`),
//! returning `ExecResult::failure(...)` straight out of the function on any
//! error. Those returns bypass `Tool::execute()` entirely, so the normal
//! per-command `finalize_output` seam (kernel.rs::execute_command, which
//! applies `--json` after every OTHER builtin's dispatch) never runs on
//! them — `--json` was silently dropped on this one path.
//!
//! Driven through `kernel.execute()` (never a builtin's direct `.execute()`)
//! per AGENTS.md, so the real pipeline split (scatter/gather detection) runs.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("failed to create kernel")
}

/// A `{"error", "code"}` envelope, the same shape `apply_output_format`
/// produces for every other builtin's `--json` error path.
fn assert_json_error_envelope(r: &kaish_kernel::interpreter::ExecResult) {
    assert_ne!(r.code, 0, "must be a genuine failure");
    let parsed: serde_json::Value = serde_json::from_str(&r.text_out()).unwrap_or_else(|e| {
        panic!(
            "expected a JSON envelope under --json, got raw text (parse error: {e}): {:?}",
            r.text_out()
        )
    });
    assert!(parsed.get("error").is_some(), "envelope missing \"error\": {parsed}");
    assert_eq!(parsed["code"], r.code, "envelope \"code\" must match the exit code: {parsed}");
}

// ═══════════════════════════════════════════════════════════════════════
// The issue's own repro
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn scatter_option_parse_failure_honors_json_requested_on_gather() {
    // `scatter --limit nope` fails inside `parse_scatter_options` (a
    // present-but-wrong-typed flag value) — by that point `gather_args` has
    // already been built successfully, so this also exercises the ordinary
    // (non-fallback) path.
    let r = kernel()
        .execute(r#"seq 1 3 | scatter --limit nope | echo $ITEM | gather --json"#)
        .await
        .expect("kernel execute");
    assert_json_error_envelope(&r);
    let parsed: serde_json::Value = serde_json::from_str(&r.text_out()).unwrap();
    assert!(
        parsed["error"].as_str().unwrap_or_default().contains("scatter"),
        "envelope error should name the failing side: {parsed}"
    );
}

#[tokio::test]
async fn scatter_option_parse_failure_without_json_stays_plain_text() {
    // Negative control: without --json the error is still plain text (no
    // regression / no over-eager JSON-wrapping of every error).
    let r = kernel()
        .execute(r#"seq 1 3 | scatter --limit nope | echo $ITEM | gather"#)
        .await
        .expect("kernel execute");
    assert_ne!(r.code, 0);
    assert!(
        serde_json::from_str::<serde_json::Value>(&r.text_out()).is_err(),
        "without --json the failure must stay plain text, not a JSON envelope: {:?}",
        r.text_out()
    );
}

// ═══════════════════════════════════════════════════════════════════════
// The fallback this design decision actually protects: a `build_tool_args`
// failure on scatter's OWN args, before `gather_args` is ever built — so
// there's no `ToolArgs` yet for the ordinary `GlobalFlags::apply_from_args`
// route to have read `--json` off of. Detecting `--json` from the raw AST
// up front (`has_json_flag`) is what makes this case work at all.
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn build_tool_args_failure_on_scatter_before_gather_args_exist_still_honors_json() {
    // `${nope[key]}` — a subscripted access on an entirely undefined root —
    // fails inside `build_tool_args` itself (see
    // scatter_gather_jsonl_tests.rs's `undefined_root_subscript_in_scatter_flag_value_is_loud`),
    // which returns before `gather_args` is ever built.
    let r = kernel()
        .execute(r#"seq 1 2 | scatter --as ${nope[key]} | echo $ITEM | gather --json"#)
        .await
        .expect("kernel execute");
    assert_json_error_envelope(&r);
}

#[tokio::test]
async fn json_flag_on_scatter_side_is_also_honored() {
    // `--json` can be requested on EITHER side of the pipe; a failure
    // building gather's own args must still see it via the scatter_cmd scan.
    let r = kernel()
        .execute(r#"seq 1 2 | scatter --json | echo $ITEM | gather --as=${nope[key]}"#)
        .await
        .expect("kernel execute");
    assert_json_error_envelope(&r);
}
