//! Kernel-routed tests for `printf`'s numeric conversions.
//!
//! `printf '%d' 0xff` printing `0` is the silent-fallback shape AGENTS.md
//! names as the one to refuse. These pin the refusals and, just as
//! importantly, the values that must keep converting.
//!
//! `awk` shares the format engine but not the rule: in POSIX awk a
//! non-numeric string IS 0, so the awk tests below are the control that the
//! refusal did not leak across the trait.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::transient().with_skip_validation(true)).expect("kernel")
}

/// Run a script, expect a refusal, and hand back the diagnostic.
async fn refused(script: &str) -> String {
    let k = kernel();
    let r = k.execute(script).await.expect("execute");
    assert_ne!(r.code, 0, "{script:?} must refuse, got out={:?}", r.text_out());
    assert!(
        r.text_out().is_empty(),
        "{script:?} must print nothing when it refuses, got {:?}",
        r.text_out()
    );
    r.err
}

/// Run a script that must succeed, and hand back stdout.
async fn ok(script: &str) -> String {
    let k = kernel();
    let r = k.execute(script).await.expect("execute");
    assert_eq!(r.code, 0, "{script:?} must succeed: {}", r.err);
    r.text_out().to_string()
}

// ---------------------------------------------------------------------------
// Refusals: a value that is not a number does not become one
// ---------------------------------------------------------------------------

#[tokio::test]
async fn hex_spelling_names_the_base_reader_and_the_decimal() {
    // The case AGENTS.md names. `0xff` is a string here: JSON has no hex.
    let msg = refused("printf '%d' 0xff").await;
    assert!(msg.contains("0xff"), "must quote the value: {msg}");
    assert!(msg.contains("$(( 0xff ))"), "must name the base reader: {msg}");
    assert!(msg.contains("255"), "must name the decimal: {msg}");
}

#[tokio::test]
async fn a_leading_zero_names_octal_and_decimal() {
    // 0.17.0's rule: where kaish needs a number, a leading zero is an error.
    // This is the case that used to answer plausibly (7), not visibly (0).
    let msg = refused("printf '%d' 007").await;
    assert!(msg.contains("007"), "must quote the value: {msg}");
    assert!(msg.contains("8#7"), "must name the octal spelling: {msg}");
    assert!(msg.contains("`7`"), "must name the decimal spelling: {msg}");
}

#[tokio::test]
async fn a_negative_leading_zero_keeps_its_sign_in_the_fix() {
    // The sign must survive into both suggestions, or the fix changes the
    // value — the same rule the arithmetic refusal follows.
    let msg = refused("printf '%d' -- -007").await;
    assert!(msg.contains("-8#7"), "octal fix must keep the sign: {msg}");
    assert!(msg.contains("`-7`"), "decimal fix must keep the sign: {msg}");
}

#[tokio::test]
async fn a_non_numeric_string_is_refused_not_zeroed() {
    let msg = refused("printf '%d' abc").await;
    assert!(msg.contains("abc"), "must quote the value: {msg}");
}

#[tokio::test]
async fn a_fractional_value_is_refused_for_an_integer_conversion() {
    let msg = refused("printf '%d' 1.5").await;
    assert!(msg.contains("1.5"), "must quote the value: {msg}");
}

#[tokio::test]
async fn a_float_past_the_integer_range_names_the_limit() {
    // `*f as i64` saturated silently. 1e19 is past i64::MAX.
    let msg = refused("printf '%d' 1e19").await;
    assert!(msg.contains("64-bit"), "must name the limit: {msg}");
}

#[tokio::test]
async fn a_non_numeric_string_is_refused_for_a_float_conversion_too() {
    let msg = refused("printf '%f' abc").await;
    assert!(msg.contains("abc"), "must quote the value: {msg}");
}

#[tokio::test]
async fn a_character_conversion_past_the_range_is_refused() {
    // `*i as u32` truncated a wide integer into some other character.
    let msg = refused("printf '%c' 4294967296").await;
    assert!(msg.contains("4294967296"), "must quote the value: {msg}");
}

// ---------------------------------------------------------------------------
// Controls: what must keep working. A refusal that swallows these is worse
// than the bug it replaced.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn plain_integers_still_convert() {
    assert_eq!(ok("printf '%d' 42").await, "42");
    assert_eq!(ok("printf '%d' -- -5").await, "-5");
    assert_eq!(ok("printf '%d' 0").await, "0");
}

#[tokio::test]
async fn a_missing_operand_is_still_zero() {
    // POSIX: an absent operand converts as 0, and that is NOT the silent
    // fallback being removed here. Only a value that IS present and is not
    // a number now refuses.
    assert_eq!(ok("printf '%d'").await, "0");
    assert_eq!(ok("printf '%d-%d' 7").await, "7-0");
}

#[tokio::test]
async fn exponent_notation_is_a_json_number_and_converts() {
    // `fromjson` reads 1e3; printf reads what fromjson reads.
    assert_eq!(ok("printf '%d' 1e3").await, "1000");
}

#[tokio::test]
async fn a_boolean_still_converts() {
    assert_eq!(ok("printf '%d' true").await, "1");
    assert_eq!(ok("printf '%d' false").await, "0");
}

#[tokio::test]
async fn float_conversions_are_unaffected() {
    assert_eq!(ok("printf '%.1f' 1.5").await, "1.5");
    assert_eq!(ok("printf '%.1f' 2").await, "2.0");
}

#[tokio::test]
async fn a_string_conversion_takes_any_text() {
    // Only the NUMERIC conversions gained a rule. %s still prints anything.
    assert_eq!(ok("printf '%s' 0xff").await, "0xff");
    assert_eq!(ok("printf '%s' 007").await, "007");
    assert_eq!(ok("printf '%s' abc").await, "abc");
}

// ---------------------------------------------------------------------------
// The awk control: same format engine, different and correct rule
// ---------------------------------------------------------------------------

#[tokio::test]
async fn awk_still_coerces_a_non_numeric_string_to_zero() {
    // POSIX awk: a non-numeric string is 0. This is awk being right, not
    // awk being unfixed — if this test starts failing, the printf refusal
    // leaked through `FormatArg` into a language that does not want it.
    assert_eq!(ok(r#"awk 'BEGIN { printf "%d\n", "abc" }'"#).await, "0\n");
}

#[tokio::test]
async fn awk_still_reads_a_leading_zero_as_decimal() {
    // awk has no octal rule for this either; 007 is the number 7.
    assert_eq!(ok(r#"awk 'BEGIN { printf "%d\n", "007" }'"#).await, "7\n");
}
