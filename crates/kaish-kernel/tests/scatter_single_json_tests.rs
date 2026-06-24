//! Regression tests for scatter's handling of single (non-array) JSON `.data`.
//!
//! Bug: `extract_items` ignored non-array JSON structured data and fell through
//! to newline-splitting the pretty-printed text, producing N bogus items instead
//! of 1.  Example: `echo '{"k":1}' | jq . | scatter` produced 3 items (the 3
//! lines of pretty-printed JSON) instead of 1 (the object itself).
//!
//! Standalone scatter is validation-gated (requires a `gather`), so all tests
//! use `KernelConfig::repl().with_skip_validation(true)` to reach the scatter
//! path directly via the kernel dispatch chain (not via `Scatter::execute`
//! directly — per the no-direct-builtin-call convention).

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::repl().with_skip_validation(true)).expect("kernel")
}

// ---------------------------------------------------------------------------
// Single JSON object must produce exactly 1 item (the core regression case)
// ---------------------------------------------------------------------------

/// `echo '{"k":1}' | jq . | scatter` — jq emits a single JSON object as
/// structured data; scatter must treat it as ONE item, not split the
/// pretty-printed lines.
#[tokio::test]
async fn single_json_object_is_one_item() {
    let kernel = kernel();
    let r = kernel
        .execute(r#"echo '{"k":1}' | jq . | scatter"#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "should succeed; err={:?}", r.err);
    assert!(
        r.text_out().contains("1 items"),
        "expected 1 item for a single JSON object, got: {:?}",
        r.text_out()
    );
}

// ---------------------------------------------------------------------------
// Single JSON scalar (number) must produce exactly 1 item
// ---------------------------------------------------------------------------

/// `echo '42' | jq . | scatter` — jq emits a single JSON number.
#[tokio::test]
async fn single_json_number_is_one_item() {
    let kernel = kernel();
    let r = kernel
        .execute(r#"echo '42' | jq . | scatter"#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "should succeed; err={:?}", r.err);
    assert!(
        r.text_out().contains("1 items"),
        "expected 1 item for a single JSON number, got: {:?}",
        r.text_out()
    );
}

// ---------------------------------------------------------------------------
// Single JSON string must produce exactly 1 item
// ---------------------------------------------------------------------------

/// `echo '"hello"' | jq . | scatter` — jq emits a single JSON string.
#[tokio::test]
async fn single_json_string_is_one_item() {
    let kernel = kernel();
    let r = kernel
        .execute(r#"echo '"hello"' | jq . | scatter"#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "should succeed; err={:?}", r.err);
    assert!(
        r.text_out().contains("1 items"),
        "expected 1 item for a single JSON string, got: {:?}",
        r.text_out()
    );
}

// ---------------------------------------------------------------------------
// JSON array must STILL fan out per element (no regression)
// ---------------------------------------------------------------------------

/// `seq 1 3 | scatter` — seq emits a JSON array; scatter must still produce
/// 3 items (one per element).  This is the correct behavior that must not
/// regress.
#[tokio::test]
async fn json_array_fans_out_per_element() {
    let kernel = kernel();
    let r = kernel
        .execute("seq 1 3 | scatter")
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "should succeed; err={:?}", r.err);
    assert!(
        r.text_out().contains("3 items"),
        "expected 3 items for seq 1..3, got: {:?}",
        r.text_out()
    );
}

// ---------------------------------------------------------------------------
// Plain-text (no structured data) still newline-splits
// ---------------------------------------------------------------------------

/// Without structured data, scatter must still newline-split the text (no
/// regression on the plain-text path).
#[tokio::test]
async fn plain_text_pipe_still_newline_splits() {
    let kernel = kernel();
    // printf emits plain text — no structured .data sideband
    let r = kernel
        .execute(r#"printf "a\nb\nc" | scatter"#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "should succeed; err={:?}", r.err);
    assert!(
        r.text_out().contains("3 items"),
        "expected 3 items from 3-line plain text, got: {:?}",
        r.text_out()
    );
}
