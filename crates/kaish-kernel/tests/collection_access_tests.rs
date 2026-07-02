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
async fn nested_error_names_the_full_path() {
    // A failure deep in a path names the real path (`${s[web][9]}`), not just the
    // root and failing segment — the walker accumulates the prefix.
    let k = setup().await;
    let (_, code, err) = run(
        &k,
        r#"s=$(fromjson '{"web":[80,443]}'); echo ${s[web][9]}"#,
    )
    .await;
    assert_ne!(code, 0, "out-of-bounds must be an error");
    assert!(err.contains("s[web][9]"), "should name the full path: {err}");
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

// ── Path-aware ${#path} and ${path:-default} (decision A) ──────────────────
// The subscripted forms are wired through the shared resolver now (they used to
// be a loud "bind first" placeholder). `:-` defaults on ABSENCE (unset root,
// missing key, out-of-bounds, null, empty string) but never on a falsy-but-
// present value; a SHAPE error stays loud even with `:-`.

#[tokio::test]
async fn default_on_a_subscripted_path_returns_the_present_value() {
    // `${cfg[port]:-8080}` with a real value returns it, not the default.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"cfg=$(fromjson '{"port":9000}'); echo ${cfg[port]:-8080}"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "9000");
}

#[tokio::test]
async fn default_on_a_missing_key_yields_the_default() {
    // A missing key is absence — the default fires (decision A).
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"cfg=$(fromjson '{"port":9000}'); echo ${cfg[host]:-localhost}"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "localhost");
}

#[tokio::test]
async fn default_on_an_out_of_bounds_index_yields_the_default() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, r#"xs=$(fromjson '["a","b"]'); echo ${xs[9]:-none}"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "none");
}

#[tokio::test]
async fn default_does_not_suppress_a_shape_error() {
    // An integer index on a record is misuse, not absence — loud even with `:-`.
    let k = setup().await;
    let (_, code, err) =
        run(&k, r#"cfg=$(fromjson '{"port":9000}'); echo ${cfg[0]:-x}"#).await;
    assert_ne!(code, 0, "a shape error must stay loud despite `:-`");
    assert!(err.contains("record keys are strings"), "got: {err}");
}

#[tokio::test]
async fn bare_default_fires_on_null_but_not_on_false_or_zero() {
    // Decision A at the value level: null/empty default; false/0 are present.
    let k = setup().await;
    let (out_null, _, _) = run(&k, r#"x=$(fromjson 'null'); echo ${x:-fallback}"#).await;
    assert_eq!(out_null, "fallback", "null defaults");
    let (out_false, _, _) = run(&k, r#"x=$(fromjson 'false'); echo ${x:-fallback}"#).await;
    assert_eq!(out_false, "false", "false is present, not absent");
    let (out_zero, _, _) = run(&k, r#"x=$(fromjson '0'); echo ${x:-fallback}"#).await;
    assert_eq!(out_zero, "0", "zero is present, not absent");
}

#[tokio::test]
async fn length_of_a_subscripted_path_is_the_element_count() {
    // `${#u[tags]}` resolves the path and counts — expression position exercises
    // the widened lexer regex; the string form the interpolation path.
    let k = setup().await;
    let (bare, code, err) =
        run(&k, r#"u=$(fromjson '{"tags":["a","b","c"]}'); echo ${#u[tags]}"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(bare, "3");

    let (in_string, code2, err2) =
        run(&k, r#"u=$(fromjson '{"tags":["a","b"]}'); echo "count=${#u[tags]}""#).await;
    assert_eq!(code2, 0, "err: {err2}");
    assert_eq!(in_string, "count=2");
}

#[tokio::test]
async fn length_of_a_missing_key_is_a_loud_error() {
    // Length has no default operator, so absence is loud (like bare `${u[nope]}`).
    let k = setup().await;
    let (_, code, _) =
        run(&k, r#"u=$(fromjson '{"tags":["a"]}'); echo ${#u[nope]}"#).await;
    assert_ne!(code, 0, "length of a missing key must be loud");
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

// ── Membership: `[[ e in $coll ]]` / `[[ e not in $coll ]]` ────────────────
// See docs/arrays-and-hashes.md — "Membership `in` is collection-only": a
// list tests element membership (typed equality), a record tests key
// membership, and a scalar/string RHS is a loud error (mirrors the
// collection-vs-scalar `==`/`!=` traps above, same LOUD contract).

#[tokio::test]
async fn element_in_list_is_true() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"fruits=$(fromjson '["apple","banana","cherry"]'); if [[ banana in $fruits ]]; then echo "have banana"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "have banana");
}

#[tokio::test]
async fn element_not_in_list_is_false() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"fruits=$(fromjson '["apple","banana","cherry"]'); if [[ mango in $fruits ]]; then echo "hit"; else echo "miss"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "miss");
}

#[tokio::test]
async fn key_in_record_is_true() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"user=$(fromjson '{"name":"amy","role":"maintainer"}'); if [[ name in $user ]]; then echo "has name"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "has name");
}

#[tokio::test]
async fn key_not_in_record_is_false() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"user=$(fromjson '{"name":"amy"}'); if [[ email in $user ]]; then echo "hit"; else echo "miss"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "miss");
}

#[tokio::test]
async fn not_in_true_when_record_key_absent() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"services=$(fromjson '{"web":{},"api":{}}'); if [[ tmp not in $services ]]; then echo "no tmp"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "no tmp");
}

#[tokio::test]
async fn not_in_with_command_subst_rhs() {
    // Symmetry with `command_subst_rhs_*`: `not in` over a typed `$()` list.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"if [[ 99 not in $(fromjson '[10,20,30]') ]]; then echo "absent"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "absent");
}

#[tokio::test]
async fn not_in_is_false_when_element_present() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"fruits=$(fromjson '["apple","banana"]'); if [[ banana not in $fruits ]]; then echo "hit"; else echo "miss"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "miss");
}

#[tokio::test]
async fn membership_nested_list_numeric_element() {
    // `[[ 443 in ${servers[web]} ]]` — the doc's headline membership example:
    // typed equality means the JSON number 443 matches, not just the string
    // "443".
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"servers=$(fromjson '{"web":[80,443,8080]}'); if [[ 443 in ${servers[web]} ]]; then echo "found"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "found");
}

#[tokio::test]
async fn membership_inside_a_for_loop() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"xs=$(fromjson '[1,2,3]'); for v in $(fromjson '[1,2,3]'); do if [[ $v in $xs ]]; then echo $v; fi; done"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "1\n2\n3");
}

#[tokio::test]
async fn membership_compound_and() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"fruits=$(fromjson '["apple","banana"]'); services=$(fromjson '{"web":{}}'); if [[ apple in $fruits && web in $services ]]; then echo both; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "both");
}

#[tokio::test]
async fn string_rhs_is_a_loud_error_for_in() {
    // A string/scalar RHS is never silently false — use =~/glob/case for
    // substring tests.
    let k = setup().await;
    let result = k.execute(r#"if [[ x in "hello" ]]; then echo hit; fi"#).await;
    assert!(result.is_err(), "`in` with a string RHS must be a loud error");
    let msg = format!("{:#}", result.unwrap_err());
    assert!(
        msg.contains("list or record"),
        "should name the required RHS shape: {msg}"
    );
}

#[tokio::test]
async fn string_rhs_is_a_loud_error_for_not_in() {
    // The negation must be loud too — never silently true.
    let k = setup().await;
    let result = k.execute(r#"if [[ x not in "hello" ]]; then echo hit; fi"#).await;
    assert!(result.is_err(), "`not in` with a string RHS must be a loud error");
    let msg = format!("{:#}", result.unwrap_err());
    assert!(
        msg.contains("list or record"),
        "should name the required RHS shape: {msg}"
    );
}

#[tokio::test]
async fn int_rhs_is_a_loud_error_for_in() {
    // Non-string scalars must be loud too, not just strings.
    let k = setup().await;
    let result = k.execute(r#"n=5; if [[ 1 in $n ]]; then echo hit; fi"#).await;
    assert!(result.is_err(), "`in` with an int RHS must be a loud error");
}

// ── RHS is a general expression, not just `$var`/`${path}` ────────────────
// `in`/`not in` mirror `==`/`!=`: both operands route through the same
// expression evaluator, so a `$(...)` RHS in expression position resolves to
// its typed `.data` (structured `Value::Json`), not a stringified capture.

#[tokio::test]
async fn command_subst_rhs_element_present() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"if [[ 20 in $(fromjson '[10,20,30]') ]]; then echo "found"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "found");
}

#[tokio::test]
async fn command_subst_rhs_element_absent() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"if [[ 99 in $(fromjson '[10,20,30]') ]]; then echo "hit"; else echo "miss"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "miss");
}

#[tokio::test]
async fn command_subst_rhs_via_jq_pipeline() {
    // A different builtin (jq, not fromjson) producing `.data` through a
    // pipe — pins that `in`'s RHS isn't special-cased to one producer.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"if [[ 20 in $(echo "[10,20,30]" | jq ".") ]]; then echo "found"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "found");
}

// ── Element scan never aborts on a nested-collection element ───────────────
// `values_equal` (which powers `==`/`!=`) errors loudly on collection-vs-
// scalar, but a membership *scan* must treat a nested-collection element as
// simply "not a match" and keep going — the loud error is reserved for the
// whole RHS being a scalar, not for what an individual element happens to be.

#[tokio::test]
async fn scan_skips_nested_object_element_without_erroring() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"xs=$(fromjson '[1,{"a":1},5]'); if [[ 5 in $xs ]]; then echo "found"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "found");
}

#[tokio::test]
async fn scan_skips_nested_array_element_without_erroring() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"xs=$(fromjson '[1,[2,3],5]'); if [[ 5 in $xs ]]; then echo "found"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "found");
}

#[tokio::test]
async fn scan_with_nested_element_and_no_match_is_clean_false() {
    // Not just "doesn't crash when it eventually matches" — a miss over a
    // mixed list must also resolve cleanly to false, not error.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"xs=$(fromjson '[1,{"a":1},5]'); if [[ 9 in $xs ]]; then echo "hit"; else echo "miss"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "miss");
}

// ── Decision C (2026-07-02): pin `in` element equality == the `==` rule ──────

#[tokio::test]
async fn membership_index_in_bounds_via_keys() {
    // The documented in-bounds idiom: `[[ i in $(keys $xs) ]]`. keys → integer
    // indices; the bareword LHS coerces the same way `==` does.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"xs=$(fromjson '[10,20,30]'); if [[ 1 in $(keys $xs) ]]; then echo "in"; fi; if [[ 9 in $(keys $xs) ]]; then echo "oob-hit"; else echo "oob-miss"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "in\noob-miss");
}

#[tokio::test]
async fn membership_numeric_string_coercion_matches_like_eq() {
    // A quoted numeric LHS matches a JSON number element — the same coercion
    // `[[ "443" == 443 ]]` uses. Pins that `in` reuses `==`'s equality.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"nums=$(fromjson '[80,443]'); if [[ "443" in $nums ]]; then echo "match"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "match");
}

#[tokio::test]
async fn membership_bool_element() {
    // Typed bool element membership (elements unwrap; typed compare).
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"flags=$(fromjson '[true,false]'); if [[ true in $flags ]]; then echo "yes"; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "yes");
}

// ── Shape guard: `typeof` / `[[ -list ]]` / `[[ -record ]]` ────────────────
// See docs/arrays-and-hashes.md, decision F: the antidote to the
// keys-on-list footgun and the API-shape-variance trap (Teaching note #12).

#[tokio::test]
async fn typeof_on_a_list() {
    let k = setup().await;
    let (out, code, err) = run(&k, r#"x=$(fromjson '[1,2,3]'); typeof $x"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "list");
}

#[tokio::test]
async fn typeof_on_a_record() {
    let k = setup().await;
    let (out, code, err) = run(&k, r#"x=$(fromjson '{"a":1}'); typeof $x"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "record");
}

#[tokio::test]
async fn typeof_on_a_string() {
    let k = setup().await;
    let (out, code, err) = run(&k, "x=hello; typeof $x").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "string");
}

#[tokio::test]
async fn typeof_on_a_number() {
    // No int/float split — both are "number" (jq/JSON have one numeric type).
    let k = setup().await;
    let (out_int, code_int, err_int) = run(&k, "x=5; typeof $x").await;
    assert_eq!(code_int, 0, "err: {err_int}");
    assert_eq!(out_int, "number");

    let (out_float, code_float, err_float) = run(&k, "x=3.14; typeof $x").await;
    assert_eq!(code_float, 0, "err: {err_float}");
    assert_eq!(out_float, "number");
}

#[tokio::test]
async fn typeof_on_a_bool() {
    let k = setup().await;
    let (out, code, err) = run(&k, "x=true; typeof $x").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "bool");
}

#[tokio::test]
async fn typeof_on_null() {
    let k = setup().await;
    let (out, code, err) = run(&k, r#"x=$(fromjson 'null'); typeof $x"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "null");
}

#[tokio::test]
async fn typeof_on_bytes() {
    // xxd -r produces a raw Value::Bytes with no VFS/subprocess involved —
    // safe under the isolated (NoLocal) kernel config.
    let k = setup().await;
    let (out, code, err) = run(&k, "x=$(echo ff | xxd -r -p); typeof $x").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "bytes");
}

#[tokio::test]
async fn typeof_no_argument_is_a_loud_error() {
    let k = setup().await;
    let (_, code, err) = run(&k, "typeof").await;
    assert_ne!(code, 0, "typeof with no argument must be an error");
    assert!(err.contains("no argument"), "got: {err}");
}

#[tokio::test]
async fn typeof_in_a_case_statement() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"x=$(fromjson '[1,2,3]'); case $(typeof $x) in
  list) echo "it's a list" ;;
  record) echo "it's a record" ;;
  *) echo "other" ;;
esac"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "it's a list");
}

#[tokio::test]
async fn typeof_json_output() {
    let k = setup().await;
    let r = k
        .execute(r#"x=$(fromjson '[1,2,3]'); typeof $x --json"#)
        .await
        .expect("kernel execute");
    assert_eq!(r.code, 0, "err: {}", r.err);
    // --json wraps the plain type name as a JSON string, not double-encoded.
    assert_eq!(r.text_out().trim(), "\"list\"");
}

#[tokio::test]
async fn list_test_operator_true_on_a_list() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"x=$(fromjson '[1,2,3]'); if [[ -list $x ]]; then echo T; else echo F; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "T");
}

#[tokio::test]
async fn list_test_operator_false_on_a_record() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"x=$(fromjson '{"a":1}'); if [[ -list $x ]]; then echo T; else echo F; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "F");
}

#[tokio::test]
async fn record_test_operator_true_on_a_record() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"x=$(fromjson '{"a":1}'); if [[ -record $x ]]; then echo T; else echo F; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "T");
}

#[tokio::test]
async fn record_test_operator_false_on_a_list() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"x=$(fromjson '[1,2,3]'); if [[ -record $x ]]; then echo T; else echo F; fi"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "F");
}

#[tokio::test]
async fn list_test_operator_false_on_a_scalar() {
    let k = setup().await;
    let (out, code, err) = run(&k, "x=hello; if [[ -list $x ]]; then echo T; else echo F; fi").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "F");
}

#[tokio::test]
async fn record_test_operator_false_on_a_scalar() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, "x=hello; if [[ -record $x ]]; then echo T; else echo F; fi").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "F");
}

#[tokio::test]
async fn list_test_operator_on_unset_variable_is_a_loud_error() {
    // A bare `$unset` is an undefined-variable error, consistent with every
    // other bare-`$x` operator (`-z $unset` errors too) — a typo'd variable
    // must not read as a silent `false`. A defined-but-wrong-shaped value is
    // false; only *absence* is loud. The guard is used bare (`[[ -list $data ]]`).
    let k = setup().await;
    let result = k.execute("if [[ -list $unset ]]; then echo T; fi").await;
    assert!(result.is_err(), "-list on an unset variable must be a loud error");
}

#[tokio::test]
async fn record_test_operator_on_unset_variable_is_a_loud_error() {
    let k = setup().await;
    let result = k.execute("if [[ -record $unset ]]; then echo T; fi").await;
    assert!(result.is_err(), "-record on an unset variable must be a loud error");
}

#[tokio::test]
async fn shape_guard_idiom_end_to_end() {
    // The documented guard idiom: check the shape before committing to
    // `keys`/`values`/a `for` loop — the antidote to an API call that
    // sometimes returns a list and sometimes returns a single record.
    let k = setup().await;
    let script = r#"
data=$(fromjson '{"a":1,"b":2}')
if [[ -record $data ]]; then
  for k in $(keys $data); do echo "key:$k"; done
elif [[ -list $data ]]; then
  for x in $(values $data); do echo "val:$x"; done
fi
"#;
    let (out, code, err) = run(&k, script).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "key:a\nkey:b");

    let script_list = r#"
data=$(fromjson '["x","y"]')
if [[ -record $data ]]; then
  for k in $(keys $data); do echo "key:$k"; done
elif [[ -list $data ]]; then
  for x in $(values $data); do echo "val:$x"; done
fi
"#;
    let (out_list, code_list, err_list) = run(&k, script_list).await;
    assert_eq!(code_list, 0, "err: {err_list}");
    assert_eq!(out_list, "val:x\nval:y");
}
