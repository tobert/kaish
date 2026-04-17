//! End-to-end tests for the jq builtin's structured-data contract.
//!
//! The contract: when a jq filter emits ≥2 values, `.data` is populated with
//! a JSON array so that `for i in $(jq -r '.[]' …)` iterates naturally. This
//! holds regardless of the `-r` / `-c` rendering flags, which only shape the
//! stdout text. Single-value output keeps `.data` as a plain Value (not a
//! 1-element array), matching existing behavior for `jq '.name'`.
//!
//! Tests use an `iter:` prefix in the echo body so that a single iteration
//! binding the whole newline-separated stdout to one variable is
//! distinguishable from proper per-element iteration. Without the fix, a
//! single iteration with a multi-line value would print the prefix once
//! followed by embedded newlines, not once per element.

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated().with_skip_validation(true))
        .expect("failed to create kernel")
        .into_arc()
}

// ============================================================================
// for-loop iteration over jq output
// ============================================================================

#[tokio::test]
async fn for_loop_iterates_jq_raw_array() {
    let k = setup().await;
    let r = k
        .execute(
            r#"for f in $(echo '["a","b","c"]' | jq -r '.[]'); do echo "iter:$f"; done"#,
        )
        .await
        .expect("script ran");
    assert!(r.ok(), "exit code: {}, err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "iter:a\niter:b\niter:c");
}

#[tokio::test]
async fn for_loop_iterates_jq_compact_array() {
    let k = setup().await;
    let r = k
        .execute(
            r#"for n in $(echo '[1,2,3]' | jq -c '.[]'); do echo "iter:$n"; done"#,
        )
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "iter:1\niter:2\niter:3");
}

#[tokio::test]
async fn for_loop_iterates_jq_default_array() {
    // Even without -r / -c, multi-value output should iterate per element
    // rather than binding the whole pretty-printed stdout as one string.
    let k = setup().await;
    let r = k
        .execute(
            r#"for s in $(echo '["x","y","z"]' | jq '.[]'); do echo "iter:$s"; done"#,
        )
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "iter:x\niter:y\niter:z");
}

// ============================================================================
// Single-value and iteration-count guardrails
// ============================================================================

#[tokio::test]
async fn single_value_raw_is_not_wrapped_in_array() {
    // jq -r '.name' must still behave as a scalar: exactly one iteration.
    let k = setup().await;
    let r = k
        .execute(
            r#"for n in $(echo '{"name":"Alice"}' | jq -r '.name'); do echo "iter:$n"; done"#,
        )
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "iter:Alice");
}

#[tokio::test]
async fn single_value_json_keeps_scalar_data() {
    let k = setup().await;
    // jq '.name' produces quoted JSON "Alice". The for-loop should see a
    // single String("Alice") — stripped of JSON quotes by json_to_value.
    let r = k
        .execute(
            r#"for n in $(echo '{"name":"Alice"}' | jq '.name'); do echo "iter:$n"; done"#,
        )
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "iter:Alice");
}

// ============================================================================
// stdout rendering is unchanged (pipe / human consumption)
// ============================================================================

#[tokio::test]
async fn raw_stdout_unchanged_for_pipe_consumers() {
    // The structured .data is orthogonal to stdout — `-r` still produces
    // newline-separated raw strings.
    let k = setup().await;
    let r = k
        .execute(r#"echo '["a","b","c"]' | jq -r '.[]'"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "a\nb\nc");
}

#[tokio::test]
async fn jq_output_can_feed_next_pipe_stage() {
    let k = setup().await;
    let r = k
        .execute(r#"echo '["a","b","c"]' | jq -r '.[]' | wc -l"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "3");
}

// ============================================================================
// Data field contract — readable via ${?.data}
// ============================================================================

// ============================================================================
// cut — other structured producer covered by the same contract
// ============================================================================

#[tokio::test]
async fn for_loop_iterates_cut_output_per_line() {
    // cut has unambiguous N-item structure (one line out per input line).
    // The fix populates .data with a JSON array so `for v in $(cut …)`
    // iterates per value rather than binding the whole multi-line stdout.
    let k = setup().await;
    let r = k
        .execute(
            r#"for v in $(printf 'alice,25\nbob,30\ncarol,28\n' | cut -d ',' -f 1); do echo "iter:$v"; done"#,
        )
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "iter:alice\niter:bob\niter:carol");
}

#[tokio::test]
async fn data_field_is_array_for_multi_value_output() {
    // After a multi-value jq, ${?.data} should serialize to a JSON array.
    let k = setup().await;
    let r = k
        .execute(r#"echo '["a","b","c"]' | jq -r '.[]'"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    let data = r.data.as_ref().expect("data should be populated");
    match data {
        kaish_kernel::ast::Value::Json(serde_json::Value::Array(arr)) => {
            assert_eq!(arr.len(), 3);
            assert_eq!(arr[0], serde_json::json!("a"));
            assert_eq!(arr[1], serde_json::json!("b"));
            assert_eq!(arr[2], serde_json::json!("c"));
        }
        other => panic!("expected Value::Json(Array), got {:?}", other),
    }
}

// ============================================================================
// --arg / --argjson / -n / --null-input (matches real jq's flag names)
// ============================================================================

#[tokio::test]
async fn jq_null_input_short() {
    let k = setup().await;
    let r = k.execute(r#"jq -n '1 + 2'"#).await.expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "3");
}

#[tokio::test]
async fn jq_null_input_long() {
    let k = setup().await;
    let r = k
        .execute(r#"jq --null-input '1 + 2'"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "3");
}

#[tokio::test]
async fn jq_arg_binds_string() {
    let k = setup().await;
    let r = k
        .execute(r#"jq -n --arg name amy -r '$name'"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "amy");
}

#[tokio::test]
async fn jq_argjson_binds_integer() {
    let k = setup().await;
    let r = k
        .execute(r#"jq -n --argjson x 42 '$x + 1'"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "43");
}

#[tokio::test]
async fn jq_argjson_binds_object() {
    let k = setup().await;
    let r = k
        .execute(r#"jq -n --argjson v '{"x":7}' -r '$v.x'"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "7");
}

#[tokio::test]
async fn jq_multiple_arg_occurrences_accumulate() {
    let k = setup().await;
    let r = k
        .execute(r#"jq -n --arg a one --arg b two -r '$a + "-" + $b'"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "one-two");
}

#[tokio::test]
async fn jq_arg_from_kaish_variable() {
    // The canonical agent pattern: stash JSON in a var, bind via --argjson.
    let k = setup().await;
    let r = k
        .execute(
            r#"
            R='{"name":"amy","id":1}'
            jq -n --argjson r "$R" -r '$r.name'
            "#,
        )
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "amy");
}

#[tokio::test]
async fn jq_arg_missing_second_operand_errors() {
    // `--arg name` with no VALUE must fail loudly, matching real jq.
    let k = setup().await;
    let r = k
        .execute(r#"jq -n --arg name"#)
        .await
        .expect("script ran");
    assert!(!r.ok(), "expected non-zero exit, got ok. out={:?}", r.text_out());
}

#[tokio::test]
async fn jq_argjson_invalid_json_errors() {
    let k = setup().await;
    let r = k
        .execute(r#"jq -n --argjson x 'not-json' '.'"#)
        .await
        .expect("script ran");
    assert!(
        !r.ok(),
        "expected non-zero exit for invalid --argjson. out={:?} err={:?}",
        r.text_out(),
        r.err
    );
}
