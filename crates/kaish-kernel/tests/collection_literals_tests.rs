//! Native collection LITERALS: `xs=[a b c]` (list), `{port: 8080}` / `{port:8080}`
//! (record), multi-line literals with trailing commas, `[...$xs date]` (spread),
//! nesting. See docs/arrays-and-hashes.md.
//!
//! Kernel-routed via `KernelConfig::isolated()` (pure data, no localfs) — pairs
//! with `collection_access_tests.rs` (read side, already shipped) and
//! `sed_awk_bre_dialect_tests.rs`-style conventions in this crate.

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

// ── List literals ───────────────────────────────────────────────────────────

#[tokio::test]
async fn list_literal_three_elements() {
    let k = setup().await;
    let (out, code, err) = run(&k, "xs=[a b c]; echo $xs").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"["a","b","c"]"#);
}

#[tokio::test]
async fn list_literal_empty() {
    let k = setup().await;
    let (out, code, err) = run(&k, "xs=[]; echo $xs").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "[]");
}

#[tokio::test]
async fn list_literal_single_glued_element() {
    let k = setup().await;
    let (out, code, err) = run(&k, "xs=[dog]; echo $xs").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"["dog"]"#);
}

#[tokio::test]
async fn list_literal_single_int() {
    let k = setup().await;
    let (out, code, err) = run(&k, "xs=[1]; echo $xs").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "[1]");
}

#[tokio::test]
async fn list_literal_commas_optional() {
    let k = setup().await;
    let (out1, code1, _) = run(&k, "xs=[1 2 3]; echo $xs").await;
    let (out2, code2, _) = run(&k, "xs=[1, 2, 3]; echo $xs").await;
    assert_eq!(code1, 0);
    assert_eq!(code2, 0);
    assert_eq!(out1, out2);
    assert_eq!(out1, "[1,2,3]");
}

#[tokio::test]
async fn list_literal_heterogeneous() {
    let k = setup().await;
    let (out, code, err) = run(&k, "xs=[1 two true]; echo $xs").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"[1,"two",true]"#);
}

#[tokio::test]
async fn list_literal_quoted_element_with_space() {
    let k = setup().await;
    let (out, code, err) = run(&k, r#"xs=["green apple" banana]; echo $xs"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"["green apple","banana"]"#);
}

#[tokio::test]
async fn list_literal_element_count() {
    let k = setup().await;
    let (out, code, err) = run(&k, "xs=[1 2 3]; echo ${#xs}").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "3");
}

#[tokio::test]
async fn list_literal_access_rides_shipped_resolver() {
    let k = setup().await;
    let (out0, code0, _) = run(&k, "xs=[1 2 3]; echo ${xs[0]}").await;
    let (out_neg, code_neg, _) = run(&k, "xs=[1 2 3]; echo ${xs[-1]}").await;
    let (out_slice, code_slice, _) = run(&k, "xs=[1 2 3]; echo ${xs[0:2]}").await;
    assert_eq!(code0, 0);
    assert_eq!(out0, "1");
    assert_eq!(code_neg, 0);
    assert_eq!(out_neg, "3");
    assert_eq!(code_slice, 0);
    assert_eq!(out_slice, "[1,2]");
}

// ── Record literals ──────────────────────────────────────────────────────────

#[tokio::test]
async fn record_literal_spaced_colon() {
    let k = setup().await;
    let (out, code, err) = run(&k, "x={port: 8080}; echo $x").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"{"port":8080}"#);
}

#[tokio::test]
async fn record_literal_unspaced_colon_matches_spaced() {
    let k = setup().await;
    let (spaced, code1, _) = run(&k, "x={port: 8080}; echo $x").await;
    let (unspaced, code2, _) = run(&k, "x={port:8080}; echo $x").await;
    assert_eq!(code1, 0);
    assert_eq!(code2, 0);
    assert_eq!(spaced, unspaced, "{{port:8080}} must equal {{port: 8080}}");
}

#[tokio::test]
async fn record_literal_multiple_entries() {
    let k = setup().await;
    let (out, code, err) = run(&k, "u={name: amy, role: maintainer}; echo $u").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"{"name":"amy","role":"maintainer"}"#);
}

#[tokio::test]
async fn record_literal_multiline_with_trailing_comma() {
    let k = setup().await;
    let script = "services={\n  web:    {port: 8080, replicas: 3, healthy: true},\n  api:    {port: 9000, replicas: 2, healthy: false},\n}\necho ${services[web][port]}\necho ${services[api][replicas]}";
    let (out, code, err) = run(&k, script).await;
    assert_eq!(code, 0, "err: {err}");
    let mut lines = out.lines();
    assert_eq!(lines.next(), Some("8080"));
    assert_eq!(lines.next(), Some("2"));
}

#[tokio::test]
async fn record_literal_quoted_key() {
    let k = setup().await;
    let (out, code, err) = run(&k, r#"r={"content-type": x}; echo ${r["content-type"]}"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "x");
}

#[tokio::test]
async fn record_literal_access_by_bareword_key() {
    let k = setup().await;
    let (out, code, err) = run(&k, "u={port: 8080}; echo ${u[port]}").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "8080");
}

#[tokio::test]
async fn record_literal_key_count() {
    let k = setup().await;
    let (out, code, err) = run(&k, "u={a: 1, b: 2, c: 3}; echo ${#u}").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "3");
}

// ── Nesting / spread ─────────────────────────────────────────────────────────

#[tokio::test]
async fn nested_list_in_record() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, "x={tags: [a b], meta: {active: true}}; echo ${x[tags][1]}").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "b");
}

#[tokio::test]
async fn nested_record_in_record() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, "x={tags: [a b], meta: {active: true}}; echo ${x[meta][active]}").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "true");
}

#[tokio::test]
async fn spread_flattens_a_list() {
    let k = setup().await;
    let (out, code, err) = run(&k, "xs=[a b c]; new=[...$xs date]; echo $new").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"["a","b","c","date"]"#);
}

#[tokio::test]
async fn spread_of_two_lists() {
    let k = setup().await;
    let (out, code, err) = run(&k, "a=[1 2]; b=[3 4]; c=[...$a ...$b]; echo $c").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "[1,2,3,4]");
}

#[tokio::test]
async fn bare_variable_in_list_nests_instead_of_splatting() {
    let k = setup().await;
    // A bare $var inside `[ ]` is ONE element (nests) — contrast with spread.
    let (out, code, err) = run(&k, "xs=[1 2]; ys=[0 $xs 4]; echo $ys").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "[0,[1,2],4]");
}

// ── `in`/`not in` RHS literal (value-primary seam) ─────────────────────────

#[tokio::test]
async fn in_rhs_accepts_a_list_literal() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, r#"a=cat; if [[ $a in [cat dog] ]]; then echo hit; fi"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "hit");
}

#[tokio::test]
async fn not_in_rhs_accepts_a_list_literal() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, r#"a=fish; if [[ $a not in [cat dog] ]]; then echo hit; fi"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "hit");
}

// ── Loud errors ──────────────────────────────────────────────────────────────

#[tokio::test]
async fn bracket_leading_glob_at_value_position_is_a_loud_error() {
    let k = setup().await;
    // `[0-9]*.log` at value position must never silently become a glob
    // expansion or a partial list literal — it's a parse error.
    let result = k.execute("logs=[0-9]*.log").await;
    assert!(result.is_err(), "bracket-leading glob assignment must be a loud parse error");
}

#[tokio::test]
async fn multiword_bareword_record_value_is_a_loud_error() {
    let k = setup().await;
    let result = k.execute("x={msg: hello world}").await;
    assert!(result.is_err(), "unquoted multi-word record value must be a loud parse error");
}

#[tokio::test]
async fn nonlist_spread_is_a_loud_runtime_error() {
    let k = setup().await;
    let result = k.execute("s=hello; xs=[...$s]").await;
    assert!(result.is_err(), "spreading a scalar must be a loud error");
}

#[tokio::test]
async fn dotted_access_on_a_record_is_still_a_loud_error() {
    let k = setup().await;
    // Regression guard: dot access was never introduced by literals — brackets
    // stay the only access form. Surfaces as a non-zero exit + message (same
    // shape as the other PathError::Shape cases in collection_access_tests.rs),
    // not a hard `Result::Err` from `kernel.execute()`.
    let (_, code, err) = run(&k, "u={name: amy}; echo ${u.name}").await;
    assert_ne!(code, 0, "${{u.name}} dotted access must stay a loud error");
    assert!(err.contains("[name]"), "error should suggest the bracket form, got: {err}");
}

// ── Invariant guards: argv/for-head globs must be unaffected ────────────────

#[tokio::test]
async fn glob_in_assignment_stays_literal_string() {
    let k = setup().await;
    // Pre-existing invariant (kernel::tests::test_glob_in_assignment_is_literal):
    // a pure Star/Question glob (no brackets) at value position is untouched —
    // only bracket-shaped runs are reinterpreted as list literals.
    let (out, code, err) = run(&k, "x=*.txt; echo $x").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "*.txt");
}

#[tokio::test]
async fn for_head_glued_bracket_glob_is_unaffected() {
    let k = setup().await;
    // `for x in [a]` is argv position, not a literal — the for-loop `in`
    // keyword must not trip the value-position suppression (it shares the
    // same lexer token as membership `in`). `KernelConfig::isolated()` has no
    // real filesystem, so kaish's strict-glob contract (zero matches is an
    // error, see glob_no_match_tests.rs) fires — proof the pattern still
    // reached the glob machinery as `GlobWord("[a]")`, not a list literal
    // (which would instead be a completely different parse/eval error).
    let result = k.execute("for x in [a]; do echo $x; done").await;
    let err = result.expect_err("glob with zero matches must error").to_string();
    assert!(err.contains("no matches: [a]"), "got: {err}");
}

#[tokio::test]
async fn foo_bracket_glob_argv_unaffected() {
    let k = setup().await;
    let (_, code, err) = run(&k, "echo foo[0-9]").await;
    assert_ne!(code, 0, "glob with zero matches must be a loud non-zero exit");
    assert!(err.contains("no matches: foo[0-9]"), "got: {err}");
}

// ── case-head `in` is a statement head, NOT membership (regression) ──────────
//
// `case EXPR in PATTERN)` reuses `Token::In`, but the FIRST case pattern is an
// argv-shaped glob (char-class) pattern, not a value-position literal. If the
// value-context suppression treats the case-head `in` like membership `in`, it
// suppresses the first pattern's glob fusion, forcing it down `case_parser`'s
// primitive `[ … ]` char-class branch (which doesn't accept a DASHNUM like
// `0-9`) → a parse error. Guard the first-pattern char-class here.

#[tokio::test]
async fn case_first_pattern_digit_char_class() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, "case 5 in [0-9]*) echo digit ;; *) echo other ;; esac").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "digit");
}

#[tokio::test]
async fn case_first_pattern_negated_char_class() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, "case dog in [!0-9]*) echo nd ;; *) echo x ;; esac").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "nd");
}

#[tokio::test]
async fn case_first_pattern_alpha_range() {
    let k = setup().await;
    // Already worked (no DASHNUM), but pin it against future suppression drift.
    let (out, code, err) =
        run(&k, "case dog in [a-z]*) echo lc ;; *) echo x ;; esac").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "lc");
}

#[tokio::test]
async fn membership_rhs_literal_still_parses_after_case_fix() {
    let k = setup().await;
    // The statement-head-`in` exclusion must NOT swallow a real membership
    // `in`: `[[ x in [a b] ]]` still gets the RHS list literal.
    let (out, code, err) = run(&k, r#"if [[ a in [a b] ]]; then echo hit; fi"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "hit");
}

#[tokio::test]
async fn for_head_glob_still_a_glob_after_case_fix() {
    let k = setup().await;
    // The general statement-head exclusion still covers `for … in` — a glued
    // char-class glob in a for-head stays a glob (strict-no-match error in the
    // fs-less isolated kernel), never a list literal.
    let result = k.execute("for f in [0-9]*.nomatch; do echo $f; done").await;
    let err = result.expect_err("for-head glob with zero matches must error").to_string();
    assert!(err.contains("no matches: [0-9]*.nomatch"), "got: {err}");
}
