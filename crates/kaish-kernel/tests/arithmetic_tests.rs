//! `$(( ))` and bare `(( ))`: every documented example, every named error
//! fix, and the precedence/coercion tables from `docs/LANGUAGE.md`.
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

// ── docs/LANGUAGE.md "Arithmetic" — every example with its # result ────────

#[tokio::test]
async fn language_md_examples() {
    for (source, expected) in [
        ("echo $((5 + 3 * 2))", "11"),
        ("echo $((10 / 3))", "3"),
        ("echo $((-7 % 3))", "-1"),
        ("echo $((2 ** 10))", "1024"),
        ("echo $((0xff))", "255"),
        ("echo $((16#ff))", "255"),
        ("echo $((8#17))", "15"),
        ("echo $((2#1011))", "11"),
        ("echo $((36#z))", "35"),
        ("echo $((-0xff))", "-255"),
        ("count=4; echo $((count + 1))", "5"),
        ("count=4; echo $(($count + 1))", "5"),
        (r#"mask="0xff"; echo $((mask & 16#0f))"#, "15"),
        ("echo $(( ${limit:-0} + 1 ))", "1"),
        ("echo $((2 ** 3 ** 2))", "512"),
        ("echo $((-2 ** 2))", "4"),
        ("echo $((1 << 2 + 1))", "8"),
        ("echo $((5 & 3 == 3))", "1"),
        ("echo $((5 > 3))", "1"),
        ("echo $(( (8 & 8) != 0 ))", "1"),
    ] {
        ok(source, expected).await;
    }
}

#[tokio::test]
async fn assignment_counter_idiom() {
    ok("x=1; x=$((x + 1)); echo $x", "2").await;
}

#[tokio::test]
async fn a_command_prints_one_integer_operand() {
    ok("echo $(( $(printf 6) * 2 ))", "12").await;
}

#[tokio::test]
async fn ternary_picks_the_larger_value() {
    ok("a=3; b=9; echo $((a > b ? a : b))", "9").await;
}

#[tokio::test]
async fn arithmetic_as_a_condition() {
    ok(
        "i=1; while (( i <= 5 )); do echo $i; i=$((i + 1)); done",
        "1\n2\n3\n4\n5",
    )
    .await;
    ok("n=4; if (( n % 2 == 0 )); then echo even; else echo odd; fi", "even").await;
    ok("n=5; if (( n % 2 == 0 )); then echo even; else echo odd; fi", "odd").await;
}

// ── "A bare number follows JSON rules" — the octal paragraph ───────────────

#[tokio::test]
async fn bare_number_section_examples() {
    let text = err_of("echo $((010 + 1))").await;
    assert!(text.contains("leading zero"));
    let text = err_of("[[ 010 -eq 10 ]]").await;
    assert!(!text.is_empty(), "010 must still be refused as a numeral");
    let text = err_of("x=010; echo $((x))").await;
    assert!(text.contains("10#$x") || text.contains("8#$x"), "{text:?}");
}

// ── Not supported, and what to write ────────────────────────────────────────

#[tokio::test]
async fn not_supported_table() {
    for source in ["echo $((x++))", "echo $((x += 1))", "echo $((x = 5))"] {
        let text = err_of(source).await;
        assert!(text.contains("assigns"), "{source:?}: {text:?}");
    }
    let text = err_of("echo $((a, b))").await;
    assert!(text.contains("one expression"), "{text:?}");
    for source in ["echo $((1.5))", "echo $((1e3))"] {
        let text = err_of(source).await;
        assert!(text.contains("integer-only"), "{source:?}: {text:?}");
    }
    let text = err_of("echo $((1 <<< 2))").await;
    assert!(text.contains("here-string"), "{text:?}");
    let text = err_of("echo $(( ))").await;
    assert!(text.contains("no expression"), "{text:?}");
    for source in ["echo $((0x))", "echo $((16#))"] {
        let text = err_of(source).await;
        assert!(text.contains("no digits"), "{source:?}: {text:?}");
    }
}

/// Round-2 review: these error texts used to print a hardcoded `name`/`rhs`
/// placeholder (and the wrong shape for `+=`/`=`) instead of the tokens the
/// author actually typed.
#[tokio::test]
async fn assignment_errors_name_the_real_source() {
    let text = err_of("echo $((x++))").await;
    assert!(text.contains("`x++`") && text.contains("x=$((x + 1))"), "{text:?}");
    let text = err_of("echo $((++x))").await;
    assert!(text.contains("`++x`") && text.contains("x=$((x + 1))"), "{text:?}");
    let text = err_of("echo $((x--))").await;
    assert!(text.contains("`x--`") && text.contains("x=$((x - 1))"), "{text:?}");
    let text = err_of("echo $((x += 2))").await;
    assert!(text.contains("`x += 2`") && text.contains("x=$((x + 2))"), "{text:?}");
    let text = err_of("echo $((x = 2))").await;
    assert!(text.contains("`x = 2`") && text.contains("write `x=2`, or `==` to compare"), "{text:?}");
}

#[tokio::test]
async fn missing_operand_errors_are_specific() {
    let text = err_of("echo $((1 + ))").await;
    assert!(text.contains("has no right operand"), "{text:?}");
    let text = err_of("echo $(( + ))").await;
    assert!(text.contains("has no operand"), "{text:?}");
}

#[tokio::test]
async fn a_leading_zero_base_is_refused() {
    for source in ["echo $((08#17))", "echo $((010#5))"] {
        let text = err_of(source).await;
        assert!(text.contains("without a leading zero"), "{source:?}: {text:?}");
    }
}

#[tokio::test]
async fn based_expansion_digits_take_no_sign() {
    let text = err_of(r#"d="-ff"; echo $((16#$d))"#).await;
    assert!(text.contains("take no sign") && text.contains("-16#ff"), "{text:?}");
}

// ── Coercion table ──────────────────────────────────────────────────────────

#[tokio::test]
async fn coercion_int_bool_float() {
    ok("x=5; echo $((x))", "5").await;
    ok("x=true; echo $((x))", "1").await;
    ok("x=false; echo $((x))", "0").await;
    ok("x=$(fromjson 100.0); echo $((x + 1))", "101").await;
    ok("x=$(fromjson 1e10); echo $((x))", "10000000000").await;
}

/// Round-5 review: `i64::MAX as f64` rounds up to 2^63 (i64::MAX has no
/// exact f64 representation this close to the limit), so a strict `>`
/// against that rounded bound let a Float holding exactly 2^63 through,
/// and the saturating cast then silently answered i64::MAX.
#[tokio::test]
async fn float_at_the_64_bit_boundary() {
    let text = err_of("x=$(fromjson 9223372036854775808.0); echo $((x))").await;
    assert!(text.contains("64-bit"), "{text:?}");
    ok(
        "x=$(fromjson -9223372036854775808.0); echo $((x))",
        "-9223372036854775808",
    )
    .await;
    ok("x=$(fromjson -0.0); echo $((x))", "0").await;
}

#[tokio::test]
async fn coercion_float_errors() {
    for source in ["x=2.7; echo $((x))", "x=$(fromjson 1e20); echo $((x))"] {
        let text = err_of(source).await;
        assert!(!text.is_empty(), "{source:?} must refuse");
    }
}

#[tokio::test]
async fn coercion_string_forms() {
    ok(r#"x="0xff"; echo $((x))"#, "255").await;
    ok(r#"x="16#ff"; echo $((x))"#, "255").await;
    ok(r#"x=" 5 "; echo $((x))"#, "5").await;
    ok(r#"x="-5"; echo $((x))"#, "-5").await;
}

#[tokio::test]
async fn coercion_leading_zero_string() {
    let text = err_of(r#"x="08"; echo $((x))"#).await;
    assert!(text.contains("leading zero"), "{text:?}");
}

#[tokio::test]
async fn coercion_empty_and_non_numeric_and_expression_strings() {
    let text = err_of(r#"x=""; echo $((x))"#).await;
    assert!(!text.is_empty());
    let text = err_of(r#"x="abc"; echo $((x))"#).await;
    assert!(text.contains("not a number"), "{text:?}");
    let text = err_of(r#"x="1 + 2"; echo $((x))"#).await;
    assert!(text.contains("not an expression"), "{text:?}");
}

#[tokio::test]
async fn coercion_null_and_unset() {
    let text = err_of("x=null; x=$(fromjson null); echo $((x))").await;
    assert!(text.contains("null"), "{text:?}");
    let text = err_of("echo $((missing))").await;
    assert!(text.contains("unset"), "{text:?}");
}

#[tokio::test]
async fn coercion_list_and_record() {
    let text = err_of("x=[1 2]; echo $((x))").await;
    assert!(text.contains("list"), "{text:?}");
    let text = err_of("x={a: 1}; echo $((x))").await;
    assert!(text.contains("record"), "{text:?}");
    // The Bytes arm is unit-tested in arithmetic.rs: no shell command produces
    // Bytes deterministically, and /dev/urandom decodes as UTF-8 often enough
    // to fail this assertion roughly once in twelve runs.
}

// ── Precedence ───────────────────────────────────────────────────────────

#[tokio::test]
async fn precedence_table() {
    ok("echo $((1 << 2 + 1))", "8").await;
    ok("echo $((5 & 3 == 3))", "1").await;
    ok("echo $((2 ** 3 ** 2))", "512").await;
    ok("echo $((-2 ** 2))", "4").await;
    ok("echo $((1 ? 2 : 3 ? 4 : 5))", "2").await;
}

// ── Overflow at each operator ────────────────────────────────────────────

#[tokio::test]
async fn overflow_at_each_operator() {
    for source in [
        "echo $((9223372036854775807 + 1))",
        "echo $((-9223372036854775808 - 1))",
        "echo $((9223372036854775807 * 2))",
        "echo $((-9223372036854775808 / -1))",
        "echo $((2 ** 63))",
        "echo $((1 << 63))",
    ] {
        let text = err_of(source).await;
        assert!(text.contains("does not fit"), "{source:?}: {text:?}");
    }
}

#[tokio::test]
async fn min_literal_only_as_direct_unary_operand() {
    ok("echo $((-9223372036854775808))", "-9223372036854775808").await;
    let text = err_of("echo $((9223372036854775808))").await;
    assert!(text.contains("does not fit"), "{text:?}");
}

// ── Lazy $(...) — the unselected side must not run ──────────────────────

#[tokio::test]
async fn lazy_command_substitution_does_not_run_on_the_skipped_side() {
    let (code, out, err) = run("echo $((1 || $(echo side >&2; echo 1)))").await;
    assert_eq!(code, 0, "{err:?}");
    assert_eq!(out, "1");
    assert!(err.is_empty(), "the skipped $() must not run: {err:?}");

    let (code, out, err) = run("echo $((0 && $(echo side >&2; echo 1)))").await;
    assert_eq!(code, 0, "{err:?}");
    assert_eq!(out, "0");
    assert!(err.is_empty(), "the skipped $() must not run: {err:?}");

    let (code, out, err) = run("echo $((1 ? 2 : $(echo side >&2; echo 3)))").await;
    assert_eq!(code, 0, "{err:?}");
    assert_eq!(out, "2");
    assert!(err.is_empty(), "the skipped ternary branch must not run: {err:?}");
}

#[tokio::test]
async fn selected_side_command_substitution_still_runs() {
    // `||` normalizes to 1/0 like any other comparison-shaped operator — the
    // selected side's $(echo 5) still RUNS (unlike the lazy test above), it
    // just doesn't splice its value in unnormalized the way `?:` does.
    let (code, out, err) = run("echo $((0 || $(echo 5)))").await;
    assert_eq!(code, 0, "{err:?}");
    assert_eq!(out, "1");
}

// ── Based expansions ─────────────────────────────────────────────────────

#[tokio::test]
async fn based_expansion_from_a_variable() {
    ok(r#"BITS="1011"; echo $((2#$BITS))"#, "11").await;
    ok(r#"MODE="755"; echo $((8#$MODE))"#, "493").await;
}

#[tokio::test]
async fn based_expansion_from_a_command() {
    ok(r#"echo $((10#$(printf 08)))"#, "8").await;
}

/// Round-5 review: `${x:-$(cmd)}` took the sync fast path and failed with
/// an internal "needs the async evaluator" message — `contains_command_subst`
/// never looked inside a default's text for a `$(...)`.
#[tokio::test]
async fn a_default_may_hold_a_command_substitution() {
    ok("echo $(( ${x:-$(echo 5)} + 1 ))", "6").await;
    ok("x=3; echo $(( ${x:-$(echo 5)} + 1 ))", "4").await;
    // A based-expansion's default stays in TEXT mode (like `10#$var`
    // already does), not the arithmetic operand's leading-zero refusal —
    // `08` from the fallback command reads as decimal, same as if `m`
    // held it directly.
    ok(r#"echo $((10#${m:-$(echo 08)}))"#, "8").await;
}

// ── Nesting ───────────────────────────────────────────────────────────────

#[tokio::test]
async fn nested_arithmetic() {
    ok("echo $(( $((1+2)) * 4 ))", "12").await;
}

#[tokio::test]
async fn newline_inside_arithmetic() {
    ok("echo $((1 +\n2))", "3").await;
}

#[tokio::test]
async fn comment_after_arithmetic_is_a_shell_comment() {
    ok("echo $((16#ff)) # comment", "255").await;
}

#[tokio::test]
async fn quoted_arithmetic_interpolates() {
    ok(r#"echo "$((0xff))""#, "255").await;
}

// ── `[[ ]]` still refuses a leading zero ────────────────────────────────

#[tokio::test]
async fn test_expr_leading_zero_still_refused() {
    let text = err_of("[[ 010 -eq 8 ]]").await;
    assert!(!text.is_empty());
}

// ── Bare `(( ))` as a command ────────────────────────────────────────────

#[tokio::test]
async fn bare_arith_command_exit_codes() {
    let (code, _, _) = run("(( 3 > 2 ))").await;
    assert_eq!(code, 0);
    let (code, _, _) = run("(( 1 > 2 ))").await;
    assert_eq!(code, 1);
    let (code, out, err) = run("(( 1/0 ))").await;
    assert_eq!(code, 2, "out={out:?} err={err:?}");
    assert!(err.contains("divides by zero"), "{err:?}");
}

/// The bare `(( ))` command form also needs the async evaluator for a
/// `$(...)` operand, not just `$(( ))` used as a value.
#[tokio::test]
async fn bare_arith_command_runs_a_command_substitution() {
    let (code, _, _) = run("(( $(echo 3) > 2 ))").await;
    assert_eq!(code, 0);
}

#[tokio::test]
async fn bare_arith_chains_with_and_or() {
    ok("(( 1 > 0 )) && echo yes", "yes").await;
    ok("(( 0 > 1 )) || echo fallback", "fallback").await;
}

// ── Depth cap ────────────────────────────────────────────────────────────

#[tokio::test]
async fn depth_cap_is_enforced() {
    let mut expr = String::new();
    for _ in 0..300 {
        expr.push('(');
    }
    expr.push('1');
    for _ in 0..300 {
        expr.push(')');
    }
    let text = err_of(&format!("echo $(({expr}))")).await;
    assert!(text.contains("256"), "{text:?}");
}

// ── Plan rendering: $(( )) text is verbatim ──────────────────────────────

#[tokio::test]
async fn plan_renders_arithmetic_text_verbatim() {
    use kaish_kernel::plan_program;
    let plans = plan_program("echo $((  1 +  2  ))").expect("parses");
    let rendered = &plans[0].plan.rendered;
    assert!(rendered.contains("$((  1 +  2  ))"), "{rendered:?}");
}

#[tokio::test]
async fn plan_renders_bare_arith_condition_verbatim() {
    use kaish_kernel::plan_program;
    let plans = plan_program("(( i <= 5 ))").expect("parses");
    let rendered = &plans[0].plan.rendered;
    assert!(rendered.contains("(( i <= 5 ))"), "{rendered:?}");
}

// ── The 5-model panel's 14 tasks (fixed inputs replace date/random) ──────

#[tokio::test]
async fn panel_task_1_hex_string_to_decimal() {
    ok(r#"HEX="0xff"; echo $((HEX))"#, "255").await;
}

#[tokio::test]
async fn panel_task_2_binary_string_to_decimal() {
    ok(r#"BITS="1011"; echo $((2#$BITS))"#, "11").await;
}

#[tokio::test]
async fn panel_task_3_octal_mode_string_to_decimal() {
    ok(r#"MODE="755"; echo $((8#$MODE))"#, "493").await;
}

/// Task 4 (`$RANDOM`) needs the `random` builtin, which ships in a separate
/// PR — this asserts the refusal names that exact fix instead of a value.
#[tokio::test]
async fn panel_task_4_random_names_its_fix() {
    let text = err_of("echo $((RANDOM % 100))").await;
    assert!(text.contains("random --max"), "{text:?}");
}

/// Task 5 (`sleep 1` timing via `$SECONDS`) — asserts the refusal names the
/// `date +%s` idiom instead of running a real timed sleep.
#[tokio::test]
async fn panel_task_5_seconds_names_its_fix() {
    let text = err_of("echo $((SECONDS))").await;
    assert!(text.contains("date +%s"), "{text:?}");
}

#[tokio::test]
async fn panel_task_6_count_with_while() {
    ok(
        "i=1; while (( i <= 5 )); do echo $i; i=$((i + 1)); done",
        "1\n2\n3\n4\n5",
    )
    .await;
}

#[tokio::test]
async fn panel_task_7_integer_percentage() {
    ok("echo $((7 * 100 / 9))", "77").await;
}

#[tokio::test]
async fn panel_task_8_even_or_odd() {
    ok("N=4; if (( N % 2 == 0 )); then echo even; else echo odd; fi", "even").await;
    ok("N=7; if (( N % 2 == 0 )); then echo even; else echo odd; fi", "odd").await;
}

#[tokio::test]
async fn panel_task_9_bit_test() {
    ok("FLAGS=0x0c; echo $(( (FLAGS & 8) != 0 ))", "1").await;
    ok("FLAGS=0x04; echo $(( (FLAGS & 8) != 0 ))", "0").await;
}

/// Task 10 (`date +%m`, next month) — a fixed month string stands in for
/// the live date so the answer is deterministic.
#[tokio::test]
async fn panel_task_10_next_month_from_a_fixed_month() {
    ok(r#"m="08"; echo $((10#$m % 12 + 1))"#, "9").await;
    ok(r#"m="12"; echo $((10#$m % 12 + 1))"#, "1").await;
}

#[tokio::test]
async fn panel_task_11_power_of_two() {
    ok("echo $((2 ** 40))", "1099511627776").await;
}

#[tokio::test]
async fn panel_task_12_larger_of_two() {
    ok("A=3; B=7; echo $((A > B ? A : B))", "7").await;
    ok("A=9; B=2; echo $((A > B ? A : B))", "9").await;
}

#[tokio::test]
async fn panel_task_13_bytes_to_kilobytes() {
    ok("TOTAL_BYTES=1536000; echo $((TOTAL_BYTES / 1024))", "1500").await;
}

#[tokio::test]
async fn panel_task_14_decimal_to_hex() {
    ok("printf '%x' 255", "ff").await;
}
