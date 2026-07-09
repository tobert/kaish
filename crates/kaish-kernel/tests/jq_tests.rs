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

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

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
// Data field contract — exposed via `kaish-last`
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
    // After a multi-value jq, .data on the ExecResult should be a JSON array.
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
async fn jq_null_input_with_slurp_wraps_null_in_array() {
    // GH #111: real jq always wraps -s/--slurp's input in an array, even the
    // synthetic `null` that -n/--null-input feeds in when there's no stdin
    // document: `jq -n -s '.'` -> `[null]`. kaish's -n branch short-circuited
    // straight to a bare `Value::Null` before the slurp branch was ever
    // consulted, so `.` printed `null` instead of `[null]`.
    let k = setup().await;
    let r = k.execute(r#"jq -n -s -c '.'"#).await.expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "[null]");
}

#[tokio::test]
async fn jq_null_input_with_slurp_long_flags() {
    let k = setup().await;
    let r = k
        .execute(r#"jq --null-input --slurp --compact '.'"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "[null]");
}

#[tokio::test]
async fn jq_null_input_without_slurp_stays_bare_null() {
    // Regression guard: -n without -s must keep feeding a bare null, not an
    // array -- only -s triggers the wrap.
    let k = setup().await;
    let r = k.execute(r#"jq -n -c '.'"#).await.expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "null");
}

#[tokio::test]
async fn jq_null_input_with_slurp_length_is_one() {
    // Complements the array-shape assertions above: `length` on `[null]` is
    // 1, not 0 -- confirms the wrap is a genuine one-element array, not an
    // empty one.
    let k = setup().await;
    let r = k
        .execute(r#"jq -n -s 'length'"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "1");
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

// ============================================================================
// Number formatting — jq canonicalizes integral floats to integers
// ============================================================================
//
// jaq produces a float for `6/2` and for a literal like `1e10`; serde_json
// then renders it with a trailing `.0`. Real jq prints integral numbers
// without a decimal point (`3`, `10000000000`), keeping a fractional value as
// `2.5`. These pin the canonicalization (see val_to_json / format_raw).

#[tokio::test]
async fn jq_integral_division_has_no_decimal() {
    let k = setup().await;
    let r = k.execute("jq -cn '6/2'").await.expect("ran");
    assert_eq!(r.text_out().trim(), "3");
}

#[tokio::test]
async fn jq_large_integral_literal_has_no_decimal() {
    let k = setup().await;
    let r = k.execute("jq -cn '1e10'").await.expect("ran");
    assert_eq!(r.text_out().trim(), "10000000000");
}

#[tokio::test]
async fn jq_fractional_value_keeps_decimal() {
    let k = setup().await;
    let r = k.execute("jq -cn '5/2'").await.expect("ran");
    assert_eq!(r.text_out().trim(), "2.5");
}

#[tokio::test]
async fn jq_raw_integral_float_has_no_decimal() {
    // -r output path.
    let k = setup().await;
    let r = k.execute("jq -rn '6/2'").await.expect("ran");
    assert_eq!(r.text_out().trim(), "3");
}

#[tokio::test]
async fn jq_integral_float_inside_array_has_no_decimal() {
    // Nested through the recursive number-canonicalization path.
    let k = setup().await;
    let r = k.execute("jq -cn '[6/2, 5/2]'").await.expect("ran");
    assert_eq!(r.text_out().trim(), "[3,2.5]");
}

// ============================================================================
// jaq 3.x capabilities — fixed by the jaq-core 3 / jaq-json 2 upgrade
// ============================================================================

#[tokio::test]
async fn jq_indexing_null_returns_null() {
    // Real jq: `null | .a` yields `null` (jaq 2.x errored "cannot use null as
    // iterable"). Fixed by jaq-json 2.0's `index_opt` (null → None → null).
    let k = setup().await;
    let r = k.execute("echo 'null' | jq -c '.a'").await.expect("ran");
    assert!(r.ok(), "null index should not error: {}", r.err);
    assert_eq!(r.text_out().trim(), "null");
}

#[tokio::test]
async fn jq_keys_unsorted_preserves_insertion_order() {
    // `keys_unsorted` must return keys in source order, not sorted. Requires
    // both jaq-json 2.0's IndexMap objects AND serde_json `preserve_order` so
    // order survives the input parse.
    let k = setup().await;
    let r = k
        .execute("echo '{\"b\":1,\"a\":2}' | jq -c 'keys_unsorted'")
        .await
        .expect("ran");
    assert_eq!(r.text_out().trim(), "[\"b\",\"a\"]");
}

#[tokio::test]
async fn jq_object_output_preserves_insertion_order() {
    // A plain object passes through in source order (jq parity), not sorted.
    let k = setup().await;
    let r = k
        .execute("echo '{\"b\":1,\"a\":2}' | jq -c '.'")
        .await
        .expect("ran");
    assert_eq!(r.text_out().trim(), "{\"b\":1,\"a\":2}");
}

#[tokio::test]
async fn jq_keys_sorted_still_sorts() {
    // `keys` (sorted) is unaffected by the insertion-order change.
    let k = setup().await;
    let r = k
        .execute("echo '{\"b\":1,\"a\":2}' | jq -c 'keys'")
        .await
        .expect("ran");
    assert_eq!(r.text_out().trim(), "[\"a\",\"b\"]");
}

#[tokio::test]
async fn jq_two_to_the_53_canonicalizes_to_integer() {
    // 2^53 is exactly f64-representable, so a computed integral result at the
    // boundary prints as an integer (jq: `pow(2;53)` → `9007199254740992`),
    // not `9007199254740992.0`. Pins the inclusive `<=` bound.
    let k = setup().await;
    let r = k.execute("jq -cn 'pow(2;53)'").await.expect("ran");
    assert_eq!(r.text_out().trim(), "9007199254740992");
}

#[tokio::test]
async fn jq_big_integer_is_exact() {
    // jaq-json 2.0's BigInt-backed numbers render a large integer literal
    // exactly, instead of going through lossy f64 (`1e+22`).
    let k = setup().await;
    let r = k
        .execute("jq -cn '9999999999999999999999'")
        .await
        .expect("ran");
    assert_eq!(r.text_out().trim(), "9999999999999999999999");
}

#[tokio::test]
async fn jq_pretty_output_is_two_space_indented() {
    // Default (non -c) output is pretty-printed via jaq's writer.
    let k = setup().await;
    let r = k
        .execute("echo '{\"a\":1}' | jq '.'")
        .await
        .expect("ran");
    assert_eq!(r.text_out(), "{\n  \"a\": 1\n}\n");
}

// ============================================================================
// Value::String .data must not be JSON-sniffed on the way into jq
// ============================================================================
//
// jq's `.data`-path stdin resolution (`ast_value_to_json`, feeding
// `resolve_stdin_json`) converts kaish's typed `Value` into the
// `serde_json::Value` jq operates on. `Value::String(s)` is a *string*, full
// stop — it must become a JSON string, never be re-parsed as a JSON document.
// Re-parsing is the project's banned "JSON-sniffing" (docs/arch_no_json_sniffing
// invariant): `.data` is opt-in structure, never inferred from text content.
// `fromjson '"1"'` is the sharpest reproduction: it produces `Value::String("1")`
// — the *decoded contents* of the JSON string `"1"`, i.e. the one-character
// string `1`, not the bare number 1. Piping that into `jq 'type'` must report
// "string", not "number".

#[tokio::test]
async fn jq_type_does_not_json_sniff_a_data_string() {
    let k = setup().await;
    let r = k
        .execute(r#"fromjson '"1"' | jq 'type'"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "\"string\"");
}

#[tokio::test]
async fn jq_identity_does_not_json_sniff_a_data_string() {
    // Same bug, checked via the identity filter and quoting: the string "1"
    // fed through `jq '.'` must render as the JSON string `"1"`, not the bare
    // JSON number `1`.
    let k = setup().await;
    let r = k
        .execute(r#"fromjson '"1"' | jq '.'"#)
        .await
        .expect("script ran");
    assert!(r.ok(), "exit: {} err: {}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "\"1\"");
}

#[tokio::test]
async fn jq_type_does_not_json_sniff_other_json_looking_strings() {
    // The sniffing bug only fires when the string *happens* to parse as JSON
    // — pin down a few more shapes so a partial fix can't slip through.
    let k = setup().await;
    for (literal, expected) in [
        (r#"'"true"'"#, "\"string\""),
        (r#"'"null"'"#, "\"string\""),
        (r#"'"[1,2]"'"#, "\"string\""),
        (r#"'"8"'"#, "\"string\""),
    ] {
        let script = format!("fromjson {literal} | jq 'type'");
        let r = k.execute(&script).await.expect("script ran");
        assert!(r.ok(), "script: {script}, exit: {}, err: {}", r.code, r.err);
        assert_eq!(
            r.text_out().trim(),
            expected,
            "script: {script}"
        );
    }
}
