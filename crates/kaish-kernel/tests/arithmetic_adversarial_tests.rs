//! Adversarial `$(( ))` cases two reviewers derived from the spec, meant to
//! catch a fresh implementation's blind spots — overflow at the exact
//! boundary, precedence combinations, and the coercion table's corners.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

mod common;

async fn run(source: &str) -> (i64, String, String) {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let r = k.execute(source).await.expect("kernel execute");
    (r.code, r.text_out().trim().to_string(), r.err.clone())
}

/// Every diagnostic a failing statement produces, however it refused.
async fn err_of(source: &str) -> String {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel").into_arc();
    match k.execute(source).await {
        Ok(r) => {
            assert!(!r.ok(), "{source:?} should fail");
            format!("{}{}", r.text_out(), r.err)
        }
        Err(e) => format!("{e:?}"),
    }
}

async fn ok(source: &str, expected: &str) {
    let (code, out, err) = run(source).await;
    assert_eq!(code, 0, "{source:?} must run: {err:?}");
    assert_eq!(out, expected, "{source:?}");
}

async fn errs(source: &str, needle: &str) {
    let text = err_of(source).await;
    assert!(text.contains(needle), "{source:?}: expected {needle:?} in {text:?}");
}

// ── Bases, signs, and based-expansions ──────────────────────────────────

#[tokio::test]
async fn signed_hex_and_based_literals_combine() {
    ok("echo $(( - 16#Ff + + 0X10 ))", "-239").await;
}

#[tokio::test]
async fn based_expansion_from_a_variable_and_a_command() {
    ok("digits=7f; echo $((16#$digits + 1))", "128").await;
    ok("echo $((8#$(echo 17) + 1))", "16").await;
}

/// The spec's coercion table says a based-expansion's text is "digits only
/// (optional sign)" — a sign IS part of the accepted text, so
/// `16#$digits` with `digits` holding `-ff` is -255, not a refusal. A
/// reviewer expected an error naming `-16#ff` (the LITERAL sign-after-`#`
/// refusal, `16#-ff`), but that rule is about the sign appearing in SOURCE
/// TEXT after `#`; here the sign arrives inside the expansion's VALUE,
/// which the spec explicitly allows. Pinning the spec's answer.
///
/// (Written with a quoted assignment — `digits=-ff` unquoted hits an
/// unrelated, pre-existing parser gap: a bareword assignment value that
/// starts with `-` is misparsed as a command, not this rewrite's doing.)
#[tokio::test]
async fn based_expansion_text_may_carry_a_sign() {
    ok(r#"digits="-ff"; echo $((16#$digits))"#, "-255").await;
}

#[tokio::test]
async fn based_expansion_needs_digits_after_hash() {
    errs("echo $((2# 101))", "digits after").await;
}

#[tokio::test]
async fn leading_zero_names_the_octal_fix() {
    errs("echo $((077))", "8#77").await;
}

#[tokio::test]
async fn whitespace_padded_signed_based_string_variable() {
    ok(r#"x='  -16#F  '; echo "$((x + 16#1))""#, "-14").await;
}

#[tokio::test]
async fn default_expression_can_itself_be_a_base_literal() {
    ok("echo $((${missing:-0Xf} + 1))", "16").await;
}

#[tokio::test]
async fn plain_string_base_spelling_and_signed_whitespace_padded() {
    ok(r#"var="16#10"; echo $(( var + 1 ))"#, "17").await;
    ok(r#"v="  -16#Ff  "; echo $(( v ))"#, "-255").await;
}

#[tokio::test]
async fn command_output_hex_with_sign() {
    ok(r#"echo $(( $(echo "-0x10") * 2 ))"#, "-32").await;
}

// ── i64::MIN / i64::MAX boundary ────────────────────────────────────────

#[tokio::test]
async fn integral_float_at_min_converts() {
    ok("x=$(fromjson -9223372036854775808.0); echo $((x))", "-9223372036854775808").await;
}

#[tokio::test]
async fn min_literal_direct_unary_operand_every_base() {
    ok("echo $(( - 9223372036854775808 ))", "-9223372036854775808").await;
    ok("echo $((-0x8000000000000000))", "-9223372036854775808").await;
}

#[tokio::test]
async fn min_magnitude_positive_is_out_of_range() {
    errs("echo $((0X8000000000000000))", "64-bit").await;
}

#[tokio::test]
async fn parens_break_the_direct_unary_minus_exception() {
    errs("echo $((-(9223372036854775808)))", "64-bit").await;
    errs("echo $((-(-9223372036854775808)))", "64-bit").await;
}

#[tokio::test]
async fn max_literal_via_hex() {
    ok("echo $((16#7fffffffffffffff))", "9223372036854775807").await;
}

#[tokio::test]
async fn min_div_neg_one_overflows() {
    errs("echo $(((-9223372036854775808) / (-1)))", "64-bit").await;
}

/// `i64::checked_rem` returns `None` for `MIN % -1` too — it is defined via
/// the division, which overflows, even though the remainder (0; any
/// divisor of magnitude 1 divides evenly) always fits. This is a real bug
/// this test file caught: `apply_binary`'s `Rem` arm special-cases `r ==
/// -1` now instead of trusting `checked_rem`.
#[tokio::test]
async fn min_rem_neg_one_is_zero() {
    ok("echo $(((-9223372036854775808) % (-1)))", "0").await;
}

#[tokio::test]
async fn power_just_past_the_boundary_overflows() {
    errs("echo $((3037000500 ** 2))", "64-bit").await;
}

#[tokio::test]
async fn negative_two_to_the_63_is_exactly_min() {
    // (-2) ** 63 == i64::MIN exactly; checked_pow must reach it without a
    // spurious intermediate overflow (verified directly against
    // i64::checked_pow before trusting the arithmetic evaluator here).
    ok("echo $((-2 ** 63))", "-9223372036854775808").await;
}

#[tokio::test]
async fn one_past_min_magnitude_is_out_of_range() {
    errs("echo $((-9223372036854775809))", "64-bit").await;
}

#[tokio::test]
async fn min_times_neg_one_overflows() {
    errs("echo $((-9223372036854775808 * -1))", "64-bit").await;
}

#[tokio::test]
async fn min_magnitude_plus_one_literal_is_out_of_range() {
    errs("echo $(( 9223372036854775808 - 1 ))", "64-bit").await;
}

// ── Shifts ───────────────────────────────────────────────────────────────

#[tokio::test]
async fn arithmetic_right_shift_sign_extends() {
    ok("echo $((-1 >> 63))", "-1").await;
}

#[tokio::test]
async fn nested_arithmetic_as_a_shift_count() {
    errs("echo $((1 << $((32 + 32))))", "0..=63").await;
}

// ── Laziness beyond $(...) — the skipped branch must not even error ────

#[tokio::test]
async fn ternary_skipped_branch_does_not_divide_by_zero() {
    ok("echo $(( 0 ? 1/0 : 42 ))", "42").await;
}

#[tokio::test]
async fn and_or_skipped_side_command_substitution_does_not_run() {
    let (code, out, err) = run("echo $((0 && $(echo nope >&2; echo 1)))").await;
    assert_eq!(code, 0, "{err:?}");
    assert_eq!(out, "0");
    assert!(!err.contains("nope"), "the skipped $() must not run: {err:?}");

    let (code, out, err) = run("echo $((1 || $(echo nope >&2; echo 1)))").await;
    assert_eq!(code, 0, "{err:?}");
    assert_eq!(out, "1");
    assert!(!err.contains("nope"), "the skipped $() must not run: {err:?}");
}

#[tokio::test]
async fn nested_ternary_skips_both_unselected_command_substitutions() {
    let (code, out, err) =
        run("echo $((1 ? 0 : 1 ? $(echo nope >&2; echo 1) : $(echo bad >&2; echo 1)))").await;
    assert_eq!(code, 0, "{err:?}");
    assert_eq!(out, "0");
    assert!(!err.contains("nope") && !err.contains("bad"), "{err:?}");
}

// ── Precedence combinations ──────────────────────────────────────────────

#[tokio::test]
async fn power_binds_tighter_than_multiplication_with_nested_arithmetic() {
    ok("echo $((3 * $((1 + 2)) ** 2))", "27").await;
}

#[tokio::test]
async fn bitnot_binds_tighter_than_power() {
    ok("echo $((~1 ** 3))", "-8").await;
}

#[tokio::test]
async fn bitand_tighter_than_xor_tighter_than_or() {
    ok("echo $(( 3 | 5 ^ 6 & 10 ))", "7").await;
}

#[tokio::test]
async fn double_negative_power_towers() {
    ok("echo $((-2 ** 3 ** 2))", "-512").await;
}

#[tokio::test]
async fn division_and_modulo_toward_zero_and_dividend_sign() {
    ok("echo $(( -7 / 3 + -7 % 3 ))", "-3").await;
}

#[tokio::test]
async fn based_literal_in_a_sum() {
    ok("echo $(( 2 + 3#10 ))", "5").await;
}

// ── Not-a-token / structural garbage ─────────────────────────────────────

#[tokio::test]
async fn zero_b_and_zero_o_name_the_kaish_spelling() {
    errs("echo $((0b101))", "2#101").await;
    errs("echo $((0o77))", "8#77").await;
}

#[tokio::test]
async fn based_prefix_alone_has_no_digits() {
    errs("echo $((16#))", "digits after").await;
}

#[tokio::test]
async fn whitespace_splitting_the_hash_operator_is_an_error() {
    assert!(!err_of("echo $((16 # ff))").await.is_empty());
}

#[tokio::test]
async fn a_bare_hash_cannot_start_a_value() {
    assert!(!err_of("echo $((#12))").await.is_empty());
}

#[tokio::test]
async fn hash_is_never_a_comment_inside_arithmetic() {
    assert!(!err_of("echo $((2#10 # comment))").await.is_empty());
}

#[tokio::test]
async fn missing_operands_are_errors() {
    errs("echo $((1 + ))", "has no right operand").await;
    errs("echo $(( + ))", "has no operand").await;
}

#[tokio::test]
async fn trailing_garbage_after_a_numeral_is_an_error() {
    assert!(!err_of("echo $((1 + 2a))").await.is_empty());
    assert!(!err_of("echo $((12abc))").await.is_empty());
}

#[tokio::test]
async fn float_literal_names_integer_only() {
    errs("echo $((12.0))", "integer").await;
}

#[tokio::test]
async fn negative_exponent_names_the_fix() {
    errs("echo $((2 ** -1))", "negative").await;
}

#[tokio::test]
async fn power_overflow_names_the_limit() {
    errs("echo $((2 ** 100))", "64-bit").await;
}

#[tokio::test]
async fn shift_count_out_of_range_both_directions() {
    assert!(!err_of("echo $((1 << -1))").await.is_empty());
    assert!(!err_of("echo $((1 << 64))").await.is_empty());
}

#[tokio::test]
async fn command_output_that_is_an_expression_is_refused() {
    assert!(!err_of(r#"echo $(( $(echo "1 + 2") ))"#).await.is_empty());
}

// ── Bare identifiers are variables, not keywords ─────────────────────────

/// A reviewer expected `true + false` to read as the boolean literals (1 +
/// 0 = 1). Under the spec's grammar, though, a bare word inside `$(( ))`
/// is always `reference = identifier` — a variable name — never a keyword;
/// only a variable that HOLDS a `Bool` value coerces through the
/// Int/Bool/Float/String/Null table. `true` and `false` are builtin
/// COMMANDS in kaish, not auto-bound session variables, so `true` here is
/// simply unset. Pinning the spec's answer (an error naming `true` as
/// unset) over the reviewer's expectation.
#[tokio::test]
async fn bare_true_false_are_variable_names_not_literals() {
    errs("echo $(( true + false ))", "unset").await;
}
