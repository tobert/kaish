//! Collection lvalue WRITES: `xs[0]=9`, `user[email]=x`,
//! `services[web][port]=9000`, and the `push` builtin — bareword
//! (`push xs val`) and bracket-path (`push services[web][tags] val`)
//! targets alike. See `docs/arrays-and-hashes.md`, "Assignment lvalues" and
//! "Append — push".
//!
//! Kernel-routed via `KernelConfig::isolated()` WITH validation ON (unlike
//! the sibling `collection_literals_tests.rs`/`collection_access_tests.rs`,
//! which skip validation) — the validator's E016 (undefined lvalue root) and
//! E017 (dotted assignment target) checks are part of what this file pins.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated())
        .expect("failed to create kernel")
        .into_arc()
}

/// Run a script expected to succeed; return trimmed stdout.
async fn ok(k: &Kernel, script: &str) -> String {
    let r = k.execute(script).await.unwrap_or_else(|e| panic!("kernel execute failed: {e}\nscript: {script}"));
    assert!(r.ok(), "expected success, got exit {}: {}\nscript: {script}", r.code, r.err);
    r.text_out().trim().to_string()
}

/// Run a script expected to fail loudly (parse/validation error OR a runtime
/// error propagated as `Err` from `kernel.execute()`); return the error text.
async fn loud_err(k: &Kernel, script: &str) -> String {
    match k.execute(script).await {
        Err(e) => e.to_string(),
        Ok(r) => {
            assert!(!r.ok(), "expected a loud failure, got success: {}\nscript: {script}", r.text_out());
            r.err.clone()
        }
    }
}

// ── Basic writes ─────────────────────────────────────────────────────────

#[tokio::test]
async fn list_index_update() {
    let k = setup().await;
    let out = ok(&k, "xs=[1 2 3]; xs[0]=9; echo ${xs[0]}").await;
    assert_eq!(out, "9");
}

#[tokio::test]
async fn list_negative_index_update() {
    let k = setup().await;
    let out = ok(&k, "xs=[1 2 3]; xs[-1]=7; echo ${xs[-1]}").await;
    assert_eq!(out, "7");
}

#[tokio::test]
async fn record_key_insert() {
    let k = setup().await;
    let out = ok(&k, "u={port: 8080}; u[host]=localhost; echo ${u[host]}").await;
    assert_eq!(out, "localhost");
}

#[tokio::test]
async fn record_key_update_existing() {
    let k = setup().await;
    let out = ok(&k, "u={port: 8080}; u[port]=9090; echo ${u[port]}").await;
    assert_eq!(out, "9090");
}

#[tokio::test]
async fn glued_nested_index_lvalue_writes() {
    // `a[0][1]=9` — a chained-index lvalue (list of lists). The value comes
    // from fromjson so this pins the lvalue write path, not glued nested
    // literal construction.
    let k = setup().await;
    let out = ok(
        &k,
        r#"a=$(fromjson '[[1,2],[3,4]]'); a[0][1]=9; echo ${a[0][1]}"#,
    )
    .await;
    assert_eq!(out, "9");
}

#[tokio::test]
async fn deep_path_write() {
    let k = setup().await;
    let out = ok(
        &k,
        "s={web: {port: 8080}}; s[web][port]=9000; echo ${s[web][port]}",
    )
    .await;
    assert_eq!(out, "9000");
}

#[tokio::test]
async fn write_does_not_disturb_sibling_keys() {
    let k = setup().await;
    let out = ok(
        &k,
        "s={web: {port: 8080, replicas: 3}}; s[web][port]=9000; echo ${s[web][replicas]}",
    )
    .await;
    assert_eq!(out, "3", "sibling key must survive an unrelated write");
}

// ── Loud errors ──────────────────────────────────────────────────────────

#[tokio::test]
async fn out_of_bounds_index_set_is_loud() {
    let k = setup().await;
    let err = loud_err(&k, "xs=[1 2 3]; xs[9]=x").await;
    assert!(
        err.contains("out of bounds") || err.contains("bounds"),
        "got: {err}"
    );
}

#[tokio::test]
async fn missing_intermediate_is_loud_no_autoviv() {
    let k = setup().await;
    let err = loud_err(&k, "s={web: {port: 8080}}; s[api][port]=1").await;
    assert!(
        err.contains("no such key") || err.contains("autovivification"),
        "got: {err}"
    );
}

#[tokio::test]
async fn scalar_root_is_loud() {
    let k = setup().await;
    let err = loud_err(&k, "y=hi; y[0]=x").await;
    assert!(err.contains("collection"), "got: {err}");
}

#[tokio::test]
async fn undefined_root_is_loud() {
    let k = setup().await;
    let err = loud_err(&k, "z[0]=x").await;
    assert!(
        err.contains("not defined") || err.contains("E016"),
        "got: {err}"
    );
}

#[tokio::test]
async fn slice_lvalue_is_loud() {
    let k = setup().await;
    let err = loud_err(&k, "xs=[1 2 3]; xs[0:2]=x").await;
    assert!(err.contains("slice"), "got: {err}");
}

// ── Validator: dotted assignment target (E017) ────────────────────────────

#[tokio::test]
async fn dotted_assignment_target_is_rejected_with_bracket_suggestion() {
    let k = setup().await;
    let err = loud_err(&k, "user.email=x").await;
    assert!(err.contains("E017"), "got: {err}");
    assert!(
        err.contains("user[email]"),
        "expected the bracket-form suggestion: {err}"
    );
}

#[tokio::test]
async fn dotted_env_prefix_assignment_target_is_also_rejected() {
    // Env-prefix assignments (`NAME=value cmd`) build an `Assignment` the same
    // way a bare assignment does (`Stmt::EnvScoped` validates each entry via
    // the same `validate_assignment`), so a dotted root here must be caught
    // too — not just the bare-statement form.
    let k = setup().await;
    let err = loud_err(&k, "X.Y=v echo hi").await;
    assert!(err.contains("E017"), "got: {err}");
    assert!(
        err.contains("X[Y]"),
        "expected the bracket-form suggestion: {err}"
    );
}

// ── push ───────────────────────────────────────────────────────────────────

#[tokio::test]
async fn push_appends_in_place() {
    let k = setup().await;
    let out = ok(&k, "xs=[a b]; push xs c; echo ${#xs} ${xs[-1]}").await;
    assert_eq!(out, "3 c");
}

#[tokio::test]
async fn push_a_record_value_stays_structured() {
    let k = setup().await;
    let out = ok(
        &k,
        "xs=[]; rec={k: v}; push xs $rec; echo ${xs[-1][k]}",
    )
    .await;
    assert_eq!(out, "v", "push must not stringify a typed Value positional");
}

#[tokio::test]
async fn push_multiple_values_in_one_call() {
    let k = setup().await;
    let out = ok(&k, "xs=[a]; push xs b c; echo ${#xs}").await;
    assert_eq!(out, "3");
}

#[tokio::test]
async fn push_non_list_target_is_loud() {
    let k = setup().await;
    let err = loud_err(&k, "y=hi; push y z").await;
    assert!(err.contains("not a list"), "got: {err}");
}

#[tokio::test]
async fn push_undefined_target_is_loud() {
    let k = setup().await;
    let err = loud_err(&k, "push nope z").await;
    assert!(err.contains("not defined"), "got: {err}");
}

// ── Regression: the `=`-triggered lvalue lexer suppression must not fire on
//    a BARE char-class comparison operand. `[[ [a] = b ]]` starts the
//    suppressible run with `[`, not an Ident, so it stays a glob-vs-string
//    comparison (parses and runs) — it is NOT a subscripted assignment. ──────

#[tokio::test]
async fn bare_char_class_comparison_operand_before_eq_is_not_an_lvalue() {
    let k = setup().await;
    // `[[ [a] = b ]]`: "[a]" (a fused GlobWord as a string literal) != "b",
    // so the test is false → exit 1, but it must PARSE and RUN, not
    // parse-error as a malformed lvalue.
    let r = k.execute("[[ [a] = b ]]").await;
    match r {
        Ok(res) => assert_eq!(res.code, 1, "expected a clean false, got: {res:?}"),
        Err(e) => panic!("must not parse-error as an lvalue: {e}"),
    }

    // And a matching form is true → exit 0, confirming it really ran as a
    // string comparison of the literal "[a]".
    let r2 = k.execute("[[ [a] = [a] ]]").await.expect("must run");
    assert_eq!(r2.code, 0, "‘[a]’ == ‘[a]’ should be true: {r2:?}");
}

// ── Bracket-path push target (GH #183) ──────────────────────────────────
//
// `push services[web][tags] item` walks a nested bracket path the same way
// an assignment lvalue does. The lexer recognizes `push`'s target with its
// own independent trigger (`lexer::PushTarget`) — the target has no
// trailing `=` to key off the way an assignment lvalue does — so
// `services[web][tags]` fuses verbatim into a path instead of glob-expanding
// against the filesystem. See `docs/arrays-and-hashes.md`, "Append — push".

#[tokio::test]
async fn bracket_path_push_target_extends_the_nested_list() {
    let k = setup().await;
    let out = ok(
        &k,
        "services={web: {tags: []}}; push services[web][tags] canary; \
         echo ${#services[web][tags]} ${services[web][tags][-1]}",
    )
    .await;
    assert_eq!(out, "1 canary");
}

#[tokio::test]
async fn bracket_path_push_target_does_not_disturb_sibling_keys() {
    let k = setup().await;
    let out = ok(
        &k,
        "services={web: {tags: [a], port: 8080}}; push services[web][tags] b; \
         echo ${services[web][port]}",
    )
    .await;
    assert_eq!(out, "8080", "sibling key must survive a nested push");
}

#[tokio::test]
async fn bracket_path_push_target_single_index_works() {
    // A single `[sub]` hop (not just chained `[a][b]`) exercises the same
    // lexer trigger with the simplest possible bracket run.
    let k = setup().await;
    let out = ok(
        &k,
        "lists={xs: [1, 2]}; push lists[xs] 3; echo ${lists[xs]}",
    )
    .await;
    assert_eq!(out, "[1,2,3]");
}

#[tokio::test]
async fn bracket_path_push_target_missing_intermediate_is_loud_no_autoviv() {
    let k = setup().await;
    let err = loud_err(
        &k,
        "services={}; push services[web][tags] item",
    )
    .await;
    assert!(!err.is_empty(), "expected a non-empty loud error");
}

#[tokio::test]
async fn bracket_path_push_target_non_list_leaf_is_loud() {
    let k = setup().await;
    let err = loud_err(
        &k,
        "services={web: {port: 8080}}; push services[web][port] item",
    )
    .await;
    assert!(err.contains("not a list"), "got: {err}");
}

#[tokio::test]
async fn bracket_path_push_target_undefined_root_is_loud() {
    let k = setup().await;
    let err = loud_err(&k, "push nope[web][tags] item").await;
    assert!(err.contains("not defined"), "got: {err}");
}

#[tokio::test]
async fn push_as_variable_name_is_unaffected_by_the_target_tracker() {
    // The lexer's `push`-target tracker (`PushTarget`) is independent of
    // the assignment DFA — a variable literally named `push` must keep
    // working as an ordinary scalar assignment.
    let k = setup().await;
    let out = ok(&k, "push=5; echo $push").await;
    assert_eq!(out, "5");
}
