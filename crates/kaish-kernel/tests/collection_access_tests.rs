//! Read-side traversal for native collections: `${xs[0]}`, `${r[key]}`,
//! `${r[$k]}`, `${r["weird-key"]}`, `${xs[-1]}`, `${xs[0:2]}`, `${a[b][c]}`,
//! and `${#…}` length. See docs/arrays-and-hashes.md.
//!
//! Values are constructed with `fromjson` (the JSON ingress bridge) so these
//! tests exercise the ACCESS surface before the literal-construction grammar
//! exists. Kernel-routed via `KernelConfig::isolated()` (pure data, no localfs).

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated().with_skip_validation(true))
        .expect("failed to create kernel")
        .into_arc()
}

/// Run a script; return (trimmed stdout, exit code, stderr).
async fn run(k: &Kernel, script: &str) -> (String, i64, String) {
    let r = k.execute(script).await.expect("kernel execute");
    (r.text_out().trim().to_string(), r.code, r.err.clone())
}

// ── List indexing ──────────────────────────────────────────────────────────

#[tokio::test]
async fn list_index_positive() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, r#"x=$(fromjson '["apple","banana","cherry"]'); echo ${x[0]}"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "apple");
}

#[tokio::test]
async fn list_index_negative() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, r#"x=$(fromjson '["apple","banana","cherry"]'); echo ${x[-1]}"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "cherry");
}

#[tokio::test]
async fn list_slice_yields_list() {
    let k = setup().await;
    // end-exclusive; yields a list, which echo renders as compact JSON.
    let (out, code, err) =
        run(&k, r#"x=$(fromjson '["apple","banana","cherry"]'); echo ${x[0:2]}"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"["apple","banana"]"#);
}

// ── Record keys ────────────────────────────────────────────────────────────

#[tokio::test]
async fn record_bareword_key_is_literal() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, r#"u=$(fromjson '{"name":"amy","role":"maintainer"}'); echo ${u[name]}"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "amy");
}

#[tokio::test]
async fn record_dynamic_key() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"u=$(fromjson '{"name":"amy"}'); k=name; echo ${u[$k]}"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "amy");
}

#[tokio::test]
async fn record_quoted_key() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"r=$(fromjson '{"content-type":"text/plain"}'); echo ${r["content-type"]}"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "text/plain");
}

// ── Nested / chained subscripts ────────────────────────────────────────────

#[tokio::test]
async fn nested_list_in_record() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"n=$(fromjson '{"tags":["a","b","c"]}'); echo ${n[tags][1]}"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "b");
}

#[tokio::test]
async fn nested_record_scalar_unwraps_bool() {
    let k = setup().await;
    // ${n[meta][active]} lands on a JSON bool → unwraps to native, prints "true".
    let (out, code, err) = run(
        &k,
        r#"n=$(fromjson '{"meta":{"active":true}}'); echo ${n[meta][active]}"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "true");
}

#[tokio::test]
async fn deep_record_path() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"s=$(fromjson '{"web":{"port":8080}}'); echo ${s[web][port]}"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "8080");
}

// ── Length ${#…} ───────────────────────────────────────────────────────────

#[tokio::test]
async fn length_of_list_is_element_count() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, r#"x=$(fromjson '["a","b","c"]'); echo ${#x}"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "3");
}

#[tokio::test]
async fn length_of_record_is_key_count() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, r#"u=$(fromjson '{"name":"amy","role":"m","age":40}'); echo ${#u}"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "3");
}

#[tokio::test]
async fn length_of_string_stays_char_count() {
    // Scalars keep today's behavior — ${#s} is string length, not a collection.
    let k = setup().await;
    let (out, code, err) = run(&k, r#"s=hello; echo ${#s}"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "5");
}

// ── Scalar unwrap enables typed ops ────────────────────────────────────────

#[tokio::test]
async fn unwrapped_bool_compares_typed() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"c=$(fromjson '{"healthy":false}'); if [[ ${c[healthy]} == false ]]; then echo down; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "down");
}

#[tokio::test]
async fn unwrapped_number_does_arithmetic() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"p=$(fromjson '{"port":8080}'); echo $(( ${p[port]} + 1 ))"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "8081");
}

// ── In-string interpolation ────────────────────────────────────────────────

#[tokio::test]
async fn access_inside_double_quoted_string() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"u=$(fromjson '{"name":"amy"}'); echo "${u[name]} lives here""#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "amy lives here");
}

// ── Loud errors (brackets-only; no silent surprises) ───────────────────────

#[tokio::test]
async fn dotted_access_is_a_loud_error_with_bracket_hint() {
    let k = setup().await;
    let (_, code, err) =
        run(&k, r#"u=$(fromjson '{"name":"amy"}'); echo ${u.name}"#).await;
    assert_ne!(code, 0, "dotted access must be an error");
    assert!(err.contains("[name]"), "error should suggest the bracket form: {err}");
}

#[tokio::test]
async fn subscript_on_scalar_is_a_loud_error() {
    // Scalars never auto-coerce to collections.
    let k = setup().await;
    let (_, code, err) = run(&k, r#"s=hello; echo ${s[0]}"#).await;
    assert_ne!(code, 0, "subscripting a scalar must be an error");
    assert!(err.contains("not a collection"), "got: {err}");
}

#[tokio::test]
async fn out_of_bounds_index_is_a_loud_error() {
    let k = setup().await;
    let (_, code, err) = run(&k, r#"x=$(fromjson '["a","b"]'); echo ${x[9]}"#).await;
    assert_ne!(code, 0, "out-of-bounds index must be an error");
    assert!(err.contains("out of bounds"), "got: {err}");
}

#[tokio::test]
async fn missing_record_key_is_a_loud_error() {
    // A missing key is loud, not a silent empty (use `[[ k in $r ]]` to test
    // presence). Decision recorded in docs/arrays-and-hashes.md.
    let k = setup().await;
    let (_, code, err) =
        run(&k, r#"u=$(fromjson '{"name":"amy"}'); echo ${u[nope]}"#).await;
    assert_ne!(code, 0, "missing key must be an error");
    assert!(err.contains("no such key"), "got: {err}");
}

#[tokio::test]
async fn string_key_on_a_list_is_a_loud_error() {
    let k = setup().await;
    let (_, code, err) =
        run(&k, r#"x=$(fromjson '["a","b"]'); echo ${x[web]}"#).await;
    assert_ne!(code, 0, "a string key on a list must be an error");
    assert!(err.contains("integer index"), "got: {err}");
}

#[tokio::test]
async fn integer_index_on_a_record_is_a_loud_error() {
    // "integers index lists" — a bare integer subscript on a record is an error.
    let k = setup().await;
    let (_, code, err) =
        run(&k, r#"u=$(fromjson '{"name":"amy"}'); echo ${u[0]}"#).await;
    assert_ne!(code, 0, "an integer index on a record must be an error");
    assert!(err.contains("record keys are strings"), "got: {err}");
}

#[tokio::test]
async fn nested_dynamic_key_through_a_path() {
    // `${services[$k][port]}` — a dynamic key mid-path, then a literal key.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"s=$(fromjson '{"web":{"port":8080},"api":{"port":9000}}'); k=api; echo ${s[$k][port]}"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "9000");
}

#[tokio::test]
async fn undefined_root_in_expression_is_an_error() {
    // Undefined root is soft in strings (empty) but an error in expression
    // position — assert the expression-position behavior here.
    let k = setup().await;
    let (_, code, _) = run(&k, r#"echo ${nope[0]}"#).await;
    assert_ne!(code, 0, "undefined root subscript in expr position must error");
}

#[tokio::test]
async fn undefined_root_in_string_is_empty() {
    // Back-compat: an unset variable expands to empty inside a string.
    let k = setup().await;
    let (out, code, err) = run(&k, r#"echo "x=${nope}y""#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "x=y");
}

// ── Overnight-review fixes (2026-07-02): silent-collection traps ────────────

// `[[ ]]` eval errors surface as `Err` from `kernel.execute()` — the same LOUD
// contract the regex-match fix established (see regex_match_error_tests.rs), not a
// silent false. So these assert on the Err, not an exit code.

#[tokio::test]
async fn collection_vs_scalar_equality_is_a_loud_error() {
    // `[[ $list == banana ]]` must NOT be silently false — it's the trap `in`
    // exists to close. See docs/arrays-and-hashes.md (comparison decision).
    let k = setup().await;
    let result = k
        .execute(r#"x=$(fromjson '["a","b"]'); if [[ $x == banana ]]; then echo hit; fi"#)
        .await;
    assert!(result.is_err(), "comparing a list to a scalar must be a loud error");
    let msg = format!("{:#}", result.unwrap_err());
    assert!(msg.contains("membership") || msg.contains(" in "), "should hint at `in`: {msg}");
}

#[tokio::test]
async fn whole_list_vs_scalar_inequality_is_a_loud_error() {
    // `!=` must be loud too (it's `!(values_equal ?)`, so the error propagates).
    let k = setup().await;
    let result = k
        .execute(r#"x=$(fromjson '["a","b"]'); if [[ $x != banana ]]; then echo hit; fi"#)
        .await;
    assert!(result.is_err(), "!= with a list and a scalar must be a loud error");
    assert!(format!("{:#}", result.unwrap_err()).contains("list"), "should name the list type");
}

#[tokio::test]
async fn whole_record_vs_scalar_is_a_loud_error() {
    let k = setup().await;
    // `$u` alone is the whole record; comparing it to a scalar must be loud.
    let result = k
        .execute(r#"u=$(fromjson '{"name":"amy"}'); if [[ $u == amy ]]; then echo hit; fi"#)
        .await;
    assert!(result.is_err(), "comparing a whole record to a scalar must be a loud error");
    assert!(format!("{:#}", result.unwrap_err()).contains("record"), "should name the record type");
}

#[tokio::test]
async fn negative_slice_end_is_not_mistaken_for_a_default() {
    // `${xs[0:-1]}` — the `:-` is a slice bound, not a `:-default` separator.
    let k = setup().await;
    let (out, code, err) =
        run(&k, r#"x=$(fromjson '["a","b","c"]'); echo ${x[0:-1]}"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"["a","b"]"#);
}

#[tokio::test]
async fn default_on_a_subscripted_path_is_a_loud_error_not_the_default() {
    // `${cfg[port]:-8080}` must not silently return 8080 while a real value
    // exists — length/default on a path isn't wired yet, so it's loud with a
    // "bind first" hint (never silent-wrong-output).
    let k = setup().await;
    let (_, code, err) = run(
        &k,
        r#"cfg=$(fromjson '{"port":9000}'); echo ${cfg[port]:-8080}"#,
    )
    .await;
    assert_ne!(code, 0, "default-on-path must be loud, not silently the default");
    assert!(err.contains("bind it first"), "should hint at binding first: {err}");
}

#[tokio::test]
async fn length_of_a_subscripted_path_is_a_loud_error_not_zero() {
    // `${#u[tags]}` must not silently print 0 (bare-name lookup miss) — loud with
    // a "bind first" hint until nested length lands.
    let k = setup().await;
    let (_, code, err) = run(
        &k,
        r#"u=$(fromjson '{"tags":["a","b"]}'); echo "${#u[tags]}""#,
    )
    .await;
    assert_ne!(code, 0, "length-of-path must be loud, not silent 0");
    assert!(err.contains("bind it first"), "should hint at binding first: {err}");
}

#[tokio::test]
async fn for_loop_over_envelope_shaped_elements_stays_a_record() {
    // A `fromjson` array element that happens to be envelope-shaped is external
    // data — it must NOT be silently re-decoded to binary during iteration.
    let k = setup().await;
    // The `$(fromjson …)` CommandSubst form is the real iteration idiom (a bare
    // `for x in $xs` is rejected by the validator, E012).
    let (out, code, err) = run(
        &k,
        r#"for x in $(fromjson '[{"_type":"bytes","encoding":"base64","data":"AQID","len":3}]'); do echo ${x[_type]}; done"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    // Envelope-free: the element is still a record, so `[_type]` reads "bytes".
    assert_eq!(out, "bytes");
}
