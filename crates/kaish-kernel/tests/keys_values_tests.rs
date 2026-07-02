//! End-to-end tests for the native-collections OPS pair: `keys` / `values`.
//!
//! These drive real command strings through `kernel.execute()` (never a
//! builtin's `.execute()` directly) so the full dispatch chain runs, and they
//! use `KernelConfig::isolated()` — no localfs — because the pair is pure data
//! and must work in every capability build. Records are built with `fromjson`
//! (the JSON ingress bridge) since the native `{...}` literal grammar hasn't
//! landed yet — same pattern as `collection_access_tests.rs`.
//!
//! See `docs/arrays-and-hashes.md`, "OPS — keys/values are builtins" and
//! "Implementation notes" ("keys/values dispatch on Value::Json shape").

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated().with_skip_validation(true))
        .expect("failed to create kernel")
        .into_arc()
}

async fn run(k: &Kernel, script: &str) -> (String, i64, String) {
    let r = k.execute(script).await.expect("kernel execute");
    (r.text_out().trim().to_string(), r.code, r.err.clone())
}

// ── keys: order ─────────────────────────────────────────────────────────

#[tokio::test]
async fn keys_of_a_record_preserves_insertion_order() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"u=$(fromjson '{"name":"amy","role":"maintainer","age":40}'); keys $u"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"["name","role","age"]"#);
}

#[tokio::test]
async fn keys_of_an_empty_record_is_an_empty_list() {
    let k = setup().await;
    let (out, code, err) = run(&k, r#"u=$(fromjson '{}'); keys $u"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "[]");
}

// ── values: order + pairwise alignment with keys ──────────────────────────

#[tokio::test]
async fn values_of_a_record_preserves_insertion_order() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"u=$(fromjson '{"name":"amy","role":"maintainer","age":40}'); values $u"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"["amy","maintainer",40]"#);
}

#[tokio::test]
async fn values_of_an_empty_record_is_an_empty_list() {
    let k = setup().await;
    let (out, code, err) = run(&k, r#"u=$(fromjson '{}'); values $u"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "[]");
}

#[tokio::test]
async fn keys_and_values_are_pairwise_aligned() {
    let k = setup().await;
    // Zip keys[i] -> values[i] via bracket access on the same record and
    // confirm each key's value matches the aligned values-list element.
    let (out, code, err) = run(
        &k,
        r#"
u=$(fromjson '{"a":1,"b":2,"c":3}')
ks=$(keys $u)
vs=$(values $u)
i=0
for k in $ks; do
  echo "${u[$k]}=${vs[$i]}"
  i=$((i + 1))
done
"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "1=1\n2=2\n3=3");
}

// ── end-to-end: for-head over $(keys $r) ──────────────────────────────────
//
// Note: `for k in $r` (bare record in a for-head iterating its keys directly)
// is a separate, not-yet-landed feature (see docs/arrays-and-hashes.md,
// "Record iteration" — the for-loop's `Object` arm is still sketched, not
// implemented). This exercises the in-scope idiom: `for k in $(keys $r)`.

#[tokio::test]
async fn for_loop_over_keys_of_captures_same_order() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"
r=$(fromjson '{"web":8080,"api":9000}')
for k in $(keys $r); do
  echo "$k=${r[$k]}"
done
"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "web=8080\napi=9000");
}

// ── lists: keys → indices, values → elements (jq semantics) ───────────────

#[tokio::test]
async fn keys_of_a_list_is_its_indices() {
    let k = setup().await;
    let (out, code, err) = run(&k, r#"xs=$(fromjson '["a","b","c"]'); keys $xs"#).await;
    assert_eq!(code, 0, "err: {err}");
    // Indices are integers 0..N-1, not strings.
    assert_eq!(out, "[0,1,2]");
}

#[tokio::test]
async fn keys_of_an_empty_list_is_an_empty_list() {
    let k = setup().await;
    let (out, code, err) = run(&k, r#"xs=$(fromjson '[]'); keys $xs"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "[]");
}

#[tokio::test]
async fn values_of_a_list_is_its_elements() {
    let k = setup().await;
    let (out, code, err) = run(&k, r#"xs=$(fromjson '["a","b","c"]'); values $xs"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"["a","b","c"]"#);
}

#[tokio::test]
async fn values_of_an_empty_list_is_an_empty_list() {
    let k = setup().await;
    let (out, code, err) = run(&k, r#"xs=$(fromjson '[]'); values $xs"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "[]");
}

#[tokio::test]
async fn for_loop_over_keys_of_a_list_iterates_indices() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"
xs=$(fromjson '["a","b","c"]')
for i in $(keys $xs); do
  echo "$i:${xs[$i]}"
done
"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "0:a\n1:b\n2:c");
}

#[tokio::test]
async fn for_loop_over_values_of_a_list_iterates_elements() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"
xs=$(fromjson '["a","b","c"]')
for x in $(values $xs); do
  echo $x
done
"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "a\nb\nc");
}

// ── loud errors: non-collection args only (scalar/bytes/missing) ──────────

#[tokio::test]
async fn keys_of_a_scalar_is_a_loud_error() {
    let k = setup().await;
    let (_, code, err) = run(&k, r#"keys hello"#).await;
    assert_ne!(code, 0);
    assert!(err.contains("expected a record or list"), "err: {err}");
    assert!(err.contains("a string"), "err: {err}");
}

#[tokio::test]
async fn values_of_a_scalar_is_a_loud_error() {
    let k = setup().await;
    let (_, code, err) = run(&k, r#"values 42"#).await;
    assert_ne!(code, 0);
    assert!(err.contains("expected a record or list"), "err: {err}");
    assert!(err.contains("a number"), "err: {err}");
}

#[tokio::test]
async fn keys_with_no_argument_is_a_loud_error() {
    let k = setup().await;
    let (_, code, err) = run(&k, "keys").await;
    assert_ne!(code, 0);
    assert!(err.contains("no argument"), "err: {err}");
}

#[tokio::test]
async fn values_with_no_argument_is_a_loud_error() {
    let k = setup().await;
    let (_, code, err) = run(&k, "values").await;
    assert_ne!(code, 0);
    assert!(err.contains("no argument"), "err: {err}");
}

// ── --json output shape ───────────────────────────────────────────────────

#[tokio::test]
async fn keys_json_output_is_a_json_array() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"u=$(fromjson '{"name":"amy","role":"maintainer"}'); keys --json $u"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    let parsed: serde_json::Value = serde_json::from_str(&out).expect("valid json");
    assert_eq!(parsed, serde_json::json!(["name", "role"]));
}

#[tokio::test]
async fn values_json_output_is_a_json_array() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"u=$(fromjson '{"name":"amy","role":"maintainer"}'); values --json $u"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    let parsed: serde_json::Value = serde_json::from_str(&out).expect("valid json");
    assert_eq!(parsed, serde_json::json!(["amy", "maintainer"]));
}
