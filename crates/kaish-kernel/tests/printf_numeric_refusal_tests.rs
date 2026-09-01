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
async fn an_empty_operand_is_refused_in_its_own_words() {
    // The common shape of an unset variable reaching a number position.
    // Arithmetic already calls an empty operand an error; printf agrees,
    // and says which operand problem it is rather than quoting nothing.
    let msg = refused(r#"printf '%d' """#).await;
    assert!(msg.contains("empty"), "must name the emptiness: {msg}");
}

#[tokio::test]
async fn an_empty_variable_is_refused_the_same_way() {
    let msg = refused(r#"x=""; printf '%d' "$x""#).await;
    assert!(msg.contains("empty"), "must name the emptiness: {msg}");
}

#[tokio::test]
async fn the_integer_bounds_convert_exactly() {
    // i64::MIN is representable in f64 and must not be refused by the
    // range guard; i64::MAX must survive the i64 parse path.
    assert_eq!(ok("printf '%d' -- -9223372036854775808").await, "-9223372036854775808");
    assert_eq!(ok("printf '%d' 9223372036854775807").await, "9223372036854775807");
}

#[tokio::test]
async fn a_string_operand_one_past_the_negative_bound_refuses() {
    // The regression this guard exists for. `-9223372036854775809` is not
    // an i64, and reading it as f64 rounds it to EXACTLY i64::MIN, which
    // the range check then accepts — a silent wrong answer wearing a
    // plausible face. It must refuse before any float ever sees it.
    let msg = refused(r#"printf '%d' '-9223372036854775809'"#).await;
    assert!(msg.contains("64-bit"), "must name the limit: {msg}");

    // The control: one step inside the bound still converts, so the guard
    // cannot pass by refusing the whole neighborhood.
    assert_eq!(
        ok(r#"printf '%d' '-9223372036854775808'"#).await,
        "-9223372036854775808"
    );
}

#[tokio::test]
async fn a_string_operand_past_the_positive_bound_refuses() {
    let msg = refused(r#"printf '%d' '9223372036854775808'"#).await;
    assert!(msg.contains("64-bit"), "must name the limit: {msg}");
    assert_eq!(
        ok(r#"printf '%d' '9223372036854775807'"#).await,
        "9223372036854775807"
    );
}

#[tokio::test]
async fn a_quoted_string_operand_takes_the_json_reading() {
    // Unquoted, `1e3` and `42` arrive already typed, so these are the cases
    // that actually exercise the string reader.
    assert_eq!(ok(r#"printf '%d' '1e3'"#).await, "1000");
    assert_eq!(ok(r#"printf '%d' '42'"#).await, "42");
    assert_eq!(ok(r#"printf '%.1f' '1.5'"#).await, "1.5");
}

#[tokio::test]
async fn an_explicit_plus_is_read_and_does_not_dodge_the_rules() {
    // `is_leading_zero_numeral` knows `-` and not `+`, so `+007` could slip
    // past the leading-zero rule and answer 7.
    assert_eq!(ok(r#"printf '%d' '+7'"#).await, "7");
    let msg = refused(r#"printf '%d' '+007'"#).await;
    assert!(msg.contains("8#7"), "must still name the octal spelling: {msg}");
}

#[tokio::test]
async fn the_non_kaish_base_spellings_name_the_kaish_ones() {
    let binary = refused("printf '%d' 0b101").await;
    assert!(binary.contains("2#101"), "must name the binary spelling: {binary}");
    let octal = refused("printf '%d' 0o17").await;
    assert!(octal.contains("8#17"), "must name the octal spelling: {octal}");
}

#[tokio::test]
async fn a_fractional_leading_zero_is_not_offered_an_octal_fix() {
    // `8#7.5` is not a numeral in any base — offering it would be advice
    // that fails when followed.
    let msg = refused(r#"printf '%d' '007.5'"#).await;
    assert!(msg.contains("7.5"), "must name the decimal: {msg}");
    assert!(!msg.contains("8#7.5"), "must not invent a fractional octal: {msg}");
}

#[tokio::test]
async fn a_magnitude_past_f64_names_the_range_but_a_word_does_not() {
    let big = refused(r#"printf '%d' '1e999'"#).await;
    assert!(big.contains("64-bit"), "must name the range: {big}");
    // `"inf".parse::<f64>()` also yields a non-finite float; it must not
    // borrow the range message, because `inf` is not a numeral at all.
    let word = refused("printf '%d' inf").await;
    assert!(!word.contains("64-bit"), "a word is not a range problem: {word}");
}

// ---------------------------------------------------------------------------
// Nothing is printed before a refusal is discovered
// ---------------------------------------------------------------------------

#[tokio::test]
async fn a_refusal_discards_text_already_formatted_before_it() {
    // Every other refusal test has an empty buffer at the moment it
    // refuses, so none of them can catch a partial write. These do: the
    // literal `x`, and a good operand, are already formatted when the bad
    // operand is reached.
    let msg = refused("printf 'x%d' abc").await;
    assert!(msg.contains("abc"), "{msg}");
    let msg = refused("printf '%d-%d' 1 abc").await;
    assert!(msg.contains("abc"), "{msg}");
}

#[tokio::test]
async fn a_refusal_in_a_later_cycling_pass_discards_the_earlier_passes() {
    // printf reuses the format until the operands run out, so the first
    // pass has already written `1` when the second pass refuses.
    let msg = refused("printf '%d\\n' 1 abc").await;
    assert!(msg.contains("abc"), "{msg}");
}

// ---------------------------------------------------------------------------
// Missing operands default for every numeric conversion, not just %d
// ---------------------------------------------------------------------------

#[tokio::test]
async fn every_numeric_conversion_defaults_a_missing_operand() {
    assert_eq!(ok("printf '%x'").await, "0");
    assert_eq!(ok("printf '%o'").await, "0");
    assert_eq!(ok("printf '%.1f'").await, "0.0");
    // %c with nothing to print emits nothing rather than a NUL.
    assert_eq!(ok("printf '%c'").await, "");
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
