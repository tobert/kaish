//! Kernel-routed regression tests for awk numeric comparison semantics.
//!
//! Covers POSIX strnum rules: a field that looks like a number compares
//! numerically against a numeric operand, while a string constant always
//! forces a string comparison.
//!
//! The root cause was that `AwkValue::StrNum` was missing before commit 5abecdf.
//! Without it, every input field was treated as `AwkValue::String`, which
//! causes `$1 > 5` (with input "10") to compare as strings ("10" < "9" = LE),
//! and `$1 == 0` (with input "0") to compare strings ("0" == "0" = EQ only
//! accidentally, but "0.0" != "0" would be NE instead of the correct EQ).

#![cfg(feature = "localfs")]
// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{ExecuteOptions, Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::repl().with_latch(false).with_trash(false))
        .expect("failed to create kernel")
}

async fn run(prog: &str, stdin: &str) -> (String, i64) {
    let kernel = kernel();
    let result = kernel
        .execute_with_options(prog, ExecuteOptions::new().with_stdin(stdin))
        .await
        .expect("kernel execute");
    (result.text_out().to_string(), result.code)
}

// ─── $1 > 5 with numeric-looking input ─────────────────────────────────────

/// A numeric-looking field must compare NUMERICALLY against a number literal.
///
/// The key case is "10" > 9: as a string compare "10" < "9" (LE), but as a
/// numeric compare 10 > 9 (GT). The correct answer is GT.
#[tokio::test]
async fn numeric_field_gt_number_literal_is_numeric_compare() {
    let (out, code) = run(r#"awk '{ if ($1 > 9) print "GT"; else print "LE" }'"#, "10\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "GT\n", "$1=10 > 9 must compare numerically (not string where 10<9)");
}

/// Variant with the common `$1 > 5` idiom mentioned in issues.md.
/// Input "6" with threshold 5 — numeric compare: 6>5=true; string compare: "6">"5"=true.
/// Input "10" with threshold 5 — numeric: 10>5=true; string "10">"5"=false (1<5 in ASCII).
#[tokio::test]
async fn numeric_field_gt_five_ten_passes() {
    let (out, code) = run(r#"awk '$1 > 5 { print $1 }'"#, "10\n3\n6\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "10\n6\n", "$1>5 must use numeric compare; string compare would miss 10");
}

/// The canonical distinguishing example: "10" vs 9 must be GT.
///
/// String comparison: "10" < "9" → 0 (LE).
/// Numeric comparison: 10 > 9 → 1 (GT). The correct answer is 1.
#[tokio::test]
async fn strnum_vs_number_literal_uses_numeric_compare() {
    let (out, code) = run(r#"awk '{ print ($1 > 9) }'"#, "10\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "1\n", "strnum 10 > number 9 must be 1 (numeric), not 0 (string)");
}

// ─── $1 == 0 for various zero-like inputs ──────────────────────────────────

/// `$1 == 0` with field "0": strnum "0" looks numeric → numeric compare → 0==0 → EQ (1).
#[tokio::test]
async fn field_zero_string_equals_zero_number() {
    let (out, code) = run(r#"awk '{ print ($1 == 0) }'"#, "0\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "1\n", "$1=0 == 0 must be true (numeric compare)");
}

/// `$1 == 0` with field "0.0": strnum "0.0" looks numeric → numeric compare → 0.0==0 → EQ.
#[tokio::test]
async fn field_zero_point_zero_equals_zero_number() {
    let (out, code) = run(r#"awk '{ print ($1 == 0) }'"#, "0.0\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "1\n", "$1=0.0 == 0 must be true (numeric); string compare would give 0");
}

/// `$1 == 0` with non-numeric input: strnum "abc" is NOT numeric → string compare.
/// "abc" as string != "0" as string → NE (0).
#[tokio::test]
async fn non_numeric_field_vs_zero_is_string_compare() {
    let (out, code) = run(r#"awk '{ print ($1 == 0) }'"#, "abc\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "0\n", "$1=abc (non-numeric strnum) == 0 must be 0 (string compare)");
}

// ─── String constant comparison always uses string semantics ───────────────

/// A string *constant* on either side forces string comparison, even if the
/// field looks numeric. "10" as a string < "9" as a string (lexicographic).
#[tokio::test]
async fn strnum_vs_string_constant_uses_string_compare() {
    let (out, code) = run(r#"awk '{ print ($1 > "9") }'"#, "10\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "0\n", "strnum 10 vs string \"9\" must be string compare (10<9 lexically)");
}

/// String constant "abc" == 0: one side is a string constant → string compare.
/// "abc" as string != "0" as string → NE (0).
#[tokio::test]
async fn string_constant_vs_zero_is_string_compare() {
    let (out, code) = run(r#"awk 'BEGIN { print ("abc" == 0) }'"#, "").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "0\n", "string literal \"abc\" == 0 must be 0 (string compare)");
}

/// Two string constants compare lexicographically.
/// "10" < "9" as strings (lexicographic: "1" < "9").
#[tokio::test]
async fn two_string_constants_compare_lexically() {
    let (out, code) = run(r#"awk 'BEGIN { print ("10" < "9") }'"#, "").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "1\n", "string constants must compare lexically: \"10\" < \"9\"");
}

// ─── Pattern-level numeric comparison ──────────────────────────────────────

/// The `$1 > 5` pattern: fields matching numerically are filtered correctly.
/// Input lines "3", "10", "7", "4": 10 and 7 are > 5 numerically.
#[tokio::test]
async fn pattern_numeric_gt_filters_correctly() {
    let (out, code) = run(r#"awk '$1 > 5 { print $1 }'"#, "3\n10\n7\n4\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "10\n7\n", "$1>5 pattern must use numeric compare");
}

/// Two numeric fields compare numerically: $1 > $2 with "10 9" → GT.
#[tokio::test]
async fn two_numeric_fields_compare_numerically() {
    let (out, code) = run(r#"awk '{ if ($1 > $2) print "GT"; else print "LE" }'"#, "10 9\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "GT\n", "strnum vs strnum numeric compare: 10 > 9");
}

// ─── -v assignment numeric strings ─────────────────────────────────────────

/// `-v NAME=VALUE` assignments are POSIX strnums — they compare numerically
/// when the value looks like a number.
#[tokio::test]
async fn var_v_assignment_compares_numerically() {
    let (out, code) = run(r#"awk -v threshold=5 '$1 > threshold { print $1 }'"#, "3\n10\n7\n4\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "10\n7\n", "-v numeric assignment must allow numeric compare");
}
