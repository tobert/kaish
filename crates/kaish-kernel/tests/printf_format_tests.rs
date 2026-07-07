//! Kernel-routed tests for `printf` format flags, precision, and conversions.
//!
//! These test the bugs identified in docs/issues.md P1:
//! - `+`/space/`#` flags ignored
//! - `.precision` ignored for `%s` and `%d`/`%x`/`%o`
//! - precision doesn't override `0` flag
//! - `%E`/`%G`/`%b`/`%u` emit literal `%X`
//! - `\0NNN` octal escape broken
//!
//! Each test routes through `kernel.execute()` so the full pipeline runs.
//! Expected values verified against GNU `printf(1)`.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::transient().with_skip_validation(true)).expect("kernel")
}

// ---------------------------------------------------------------------------
// Sign flags: + and space
// ---------------------------------------------------------------------------

#[tokio::test]
async fn printf_plus_flag_positive() {
    // printf '%+d' 5 → +5
    let k = kernel();
    let result = k.execute("printf '%+d' 5").await.expect("execute");
    assert_eq!(result.text_out(), "+5", "printf '%+d' 5 should produce +5");
}

#[tokio::test]
async fn printf_plus_flag_negative() {
    // printf '%+d' -5 → -5 (minus wins over +)
    let k = kernel();
    let result = k.execute("printf '%+d' -- -5").await.expect("execute");
    assert_eq!(result.text_out(), "-5", "printf '%+d' -5 should produce -5");
}

#[tokio::test]
async fn printf_space_flag_positive() {
    // printf '% d' 5 → " 5"
    let k = kernel();
    let result = k.execute("printf '% d' 5").await.expect("execute");
    assert_eq!(result.text_out(), " 5", "printf '% d' 5 should produce ' 5'");
}

#[tokio::test]
async fn printf_space_flag_negative() {
    // printf '% d' -5 → "-5" (minus wins over space)
    let k = kernel();
    let result = k.execute("printf '% d' -- -5").await.expect("execute");
    assert_eq!(result.text_out(), "-5", "printf '% d' -5 should produce -5");
}

#[tokio::test]
async fn printf_plus_beats_space() {
    // when both + and space are specified, + wins
    let k = kernel();
    let result = k.execute("printf '% +d' 5").await.expect("execute");
    assert_eq!(result.text_out(), "+5", "printf '% +d' 5: + flag should win over space");
}

// ---------------------------------------------------------------------------
// Alternate form (#) flag
// ---------------------------------------------------------------------------

#[tokio::test]
async fn printf_hash_hex_lower() {
    // printf '%#x' 255 → 0xff
    let k = kernel();
    let result = k.execute("printf '%#x' 255").await.expect("execute");
    assert_eq!(result.text_out(), "0xff", "printf '%#x' 255 should produce 0xff");
}

#[tokio::test]
async fn printf_hash_hex_upper() {
    // printf '%#X' 255 → 0XFF
    let k = kernel();
    let result = k.execute("printf '%#X' 255").await.expect("execute");
    assert_eq!(result.text_out(), "0XFF", "printf '%#X' 255 should produce 0XFF");
}

#[tokio::test]
async fn printf_hash_octal() {
    // printf '%#o' 8 → 010
    let k = kernel();
    let result = k.execute("printf '%#o' 8").await.expect("execute");
    assert_eq!(result.text_out(), "010", "printf '%#o' 8 should produce 010");
}

#[tokio::test]
async fn printf_hash_zero_is_zero() {
    // printf '%#x' 0 → 0  (special case: 0 with # stays 0, not 0x0)
    let k = kernel();
    let result = k.execute("printf '%#x' 0").await.expect("execute");
    assert_eq!(result.text_out(), "0", "printf '%#x' 0 should produce 0 not 0x0");
}

// ---------------------------------------------------------------------------
// Precision for %d/%x/%o (zero-pads numeric digits)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn printf_precision_decimal() {
    // printf '%.3d' 5 → 005
    let k = kernel();
    let result = k.execute("printf '%.3d' 5").await.expect("execute");
    assert_eq!(result.text_out(), "005", "printf '%.3d' 5 should produce 005");
}

#[tokio::test]
async fn printf_precision_decimal_zero() {
    // printf '%.5d' 0 → 00000
    let k = kernel();
    let result = k.execute("printf '%.5d' 0").await.expect("execute");
    assert_eq!(result.text_out(), "00000", "printf '%.5d' 0 should produce 00000");
}

#[tokio::test]
async fn printf_precision_hex() {
    // printf '%.4x' 255 → 00ff
    let k = kernel();
    let result = k.execute("printf '%.4x' 255").await.expect("execute");
    assert_eq!(result.text_out(), "00ff", "printf '%.4x' 255 should produce 00ff");
}

#[tokio::test]
async fn printf_precision_overrides_zero_flag() {
    // When precision is specified, 0 flag is ignored for integers
    // printf '%05.3d' 5 → "  005" (width 5, precision 3, no zero-pad because precision given)
    let k = kernel();
    let result = k.execute("printf '%05.3d' 5").await.expect("execute");
    assert_eq!(result.text_out(), "  005", "printf '%05.3d' 5: precision overrides zero flag");
}

// ---------------------------------------------------------------------------
// Precision for %s (truncates string)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn printf_precision_string() {
    // printf '%.3s' abcdef → abc
    let k = kernel();
    let result = k.execute("printf '%.3s' abcdef").await.expect("execute");
    assert_eq!(result.text_out(), "abc", "printf '%.3s' abcdef should produce abc");
}

#[tokio::test]
async fn printf_precision_string_shorter_than_precision() {
    // printf '%.10s' hello → hello (no truncation when string is shorter)
    let k = kernel();
    let result = k.execute("printf '%.10s' hello").await.expect("execute");
    assert_eq!(result.text_out(), "hello", "printf '%.10s' hello should produce hello");
}

// ---------------------------------------------------------------------------
// Precision for %f (already worked but covering for regression)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn printf_precision_float() {
    // printf '%.2f' 3.14159 → 3.14
    let k = kernel();
    let result = k.execute("printf '%.2f' 3.14159").await.expect("execute");
    assert_eq!(result.text_out(), "3.14", "printf '%.2f' 3.14159 should produce 3.14");
}

// ---------------------------------------------------------------------------
// %u unsigned decimal
// ---------------------------------------------------------------------------

#[tokio::test]
async fn printf_u_conversion_positive() {
    // printf '%u' 5 → 5
    let k = kernel();
    let result = k.execute("printf '%u' 5").await.expect("execute");
    assert_eq!(result.text_out(), "5", "printf '%u' 5 should produce 5");
}

// ---------------------------------------------------------------------------
// %E uppercase scientific notation
// ---------------------------------------------------------------------------

#[tokio::test]
async fn printf_big_e_conversion() {
    // printf '%E' 1000 → 1.000000E+03
    let k = kernel();
    let result = k.execute("printf '%E' 1000").await.expect("execute");
    assert_eq!(result.text_out(), "1.000000E+03", "printf '%E' 1000 should produce 1.000000E+03");
}

#[tokio::test]
async fn printf_big_e_negative() {
    // printf '%E' -1000 → -1.000000E+03
    let k = kernel();
    let result = k.execute("printf '%E' -- -1000").await.expect("execute");
    assert_eq!(result.text_out(), "-1.000000E+03", "printf '%E' -1000 should produce -1.000000E+03");
}

#[tokio::test]
async fn printf_big_e_with_precision() {
    // printf '%.2E' 1000 → 1.00E+03
    let k = kernel();
    let result = k.execute("printf '%.2E' 1000").await.expect("execute");
    assert_eq!(result.text_out(), "1.00E+03", "printf '%.2E' 1000 should produce 1.00E+03");
}

// ---------------------------------------------------------------------------
// %G uppercase %g (shorter of %E or %f, remove trailing zeros)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn printf_big_g_large_number() {
    // printf '%G' 1234567 → 1.23457E+06
    let k = kernel();
    let result = k.execute("printf '%G' 1234567").await.expect("execute");
    assert_eq!(result.text_out(), "1.23457E+06", "printf '%G' 1234567 should produce 1.23457E+06");
}

#[tokio::test]
async fn printf_big_g_small_number() {
    // printf '%G' 0.000123 → 0.000123
    let k = kernel();
    let result = k.execute("printf '%G' 0.000123").await.expect("execute");
    assert_eq!(result.text_out(), "0.000123", "printf '%G' 0.000123 should produce 0.000123");
}

#[tokio::test]
async fn printf_big_g_one() {
    // printf '%G' 1.0 → 1
    let k = kernel();
    let result = k.execute("printf '%G' 1.0").await.expect("execute");
    assert_eq!(result.text_out(), "1", "printf '%G' 1.0 should produce 1");
}

// ---------------------------------------------------------------------------
// %b — interpret backslash escapes in the argument
// ---------------------------------------------------------------------------

#[tokio::test]
async fn printf_b_tab_escape() {
    // printf '%b' '\t' → TAB character
    let k = kernel();
    let result = k.execute(r#"printf '%b' '\t'"#).await.expect("execute");
    assert_eq!(result.text_out(), "\t", "printf '%b' '\\t' should produce a tab");
}

#[tokio::test]
async fn printf_b_newline_escape() {
    // printf '%b' 'hello\nworld' → "hello\nworld" with actual newline
    let k = kernel();
    let result = k.execute(r#"printf '%b' 'hello\nworld'"#).await.expect("execute");
    assert_eq!(result.text_out(), "hello\nworld", "printf '%b' 'hello\\nworld' should have a real newline");
}

#[tokio::test]
async fn printf_b_octal_escape() {
    // printf '%b' '\101' → A (octal 101 = 65 = 'A')
    let k = kernel();
    let result = k.execute(r#"printf '%b' '\101'"#).await.expect("execute");
    assert_eq!(result.text_out(), "A", "printf '%b' '\\101' should produce A");
}

#[tokio::test]
async fn printf_b_plain_string_passthrough() {
    // printf '%b' 'hello' → hello (no escapes to interpret)
    let k = kernel();
    let result = k.execute(r#"printf '%b' 'hello'"#).await.expect("execute");
    assert_eq!(result.text_out(), "hello", "printf '%b' 'hello' should produce hello");
}

// ---------------------------------------------------------------------------
// \0NNN octal escape in format string
// ---------------------------------------------------------------------------

#[tokio::test]
async fn printf_format_octal_escape_zero_prefix() {
    // `\0NNN`: the leading `0` is the FIRST of up to 3 octal digits, so at most
    // 2 more follow — matching GNU coreutils, bash, and dash (all verified).
    // `\0101` → octal `010` (BS) then a literal `1`; NOT octal 101 ('A').
    let k = kernel();
    let result = k.execute(r#"printf '\0101'"#).await.expect("execute");
    assert_eq!(
        result.text_out(),
        "\u{8}1",
        "printf '\\0101' → BS + '1' (octal 010, then literal 1), like GNU printf"
    );
    // \012 = octal 12 = newline (2 digits after the 0).
    assert_eq!(
        kernel().execute(r#"printf '\012'"#).await.expect("execute").text_out(),
        "\n",
    );
    // \0377 → octal 037 (0x1f) then a literal '7'.
    assert_eq!(
        kernel().execute(r#"printf '\0377'"#).await.expect("execute").text_out(),
        "\u{1f}7",
    );
    // Bare \NNN (no leading 0) still takes the full 3 digits: \101 = 'A'.
    assert_eq!(
        kernel().execute(r#"printf '\101'"#).await.expect("execute").text_out(),
        "A",
    );
}

#[tokio::test]
async fn printf_format_octal_escape_no_zero_prefix() {
    // printf '\101' → A (same as above but without leading 0)
    let k = kernel();
    let result = k.execute(r#"printf '\101'"#).await.expect("execute");
    assert_eq!(result.text_out(), "A", "printf '\\101' should produce A");
}

// ---------------------------------------------------------------------------
// Width + sign flag combinations
// ---------------------------------------------------------------------------

#[tokio::test]
async fn printf_width_plus_sign() {
    // printf '%+6d' 5 → "    +5"
    let k = kernel();
    let result = k.execute("printf '%+6d' 5").await.expect("execute");
    assert_eq!(result.text_out(), "    +5", "printf '%+6d' 5 should produce '    +5'");
}

#[tokio::test]
async fn printf_zero_pad_plus_sign() {
    // printf '%+06d' 5 → "+00005"
    let k = kernel();
    let result = k.execute("printf '%+06d' 5").await.expect("execute");
    assert_eq!(result.text_out(), "+00005", "printf '%+06d' 5 should produce +00005");
}

#[tokio::test]
async fn printf_left_align_beats_zero_pad() {
    // printf '%0-5d' 42 → "42   " (left align wins over zero pad)
    let k = kernel();
    let result = k.execute("printf '%-05d' 42").await.expect("execute");
    assert_eq!(result.text_out(), "42   ", "printf '%-05d' 42: left-align should win over zero-pad");
}

// ---------------------------------------------------------------------------
// %e lowercase scientific (regression guard — was probably already wrong)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn printf_little_e_conversion() {
    // printf '%e' 1000 → 1.000000e+03
    let k = kernel();
    let result = k.execute("printf '%e' 1000").await.expect("execute");
    assert_eq!(result.text_out(), "1.000000e+03", "printf '%e' 1000 should produce 1.000000e+03");
}

// ---------------------------------------------------------------------------
// %g lowercase (regression guard)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn printf_little_g_large_number() {
    // printf '%g' 1234567 → 1.23457e+06
    let k = kernel();
    let result = k.execute("printf '%g' 1234567").await.expect("execute");
    assert_eq!(result.text_out(), "1.23457e+06", "printf '%g' 1234567 should produce 1.23457e+06");
}

// ---------------------------------------------------------------------------
// Dash-only format operand (GH #137 sibling — same lexer bug as `echo ---`)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn printf_dash_only_literal() {
    // printf --- (unquoted) → literal "---" as the format string, no
    // conversions. Used to print "-" (the lexer's plain `--` literal
    // swallowed the leading two dashes as a spurious end-of-flags marker).
    let k = kernel();
    let result = k.execute("printf ---").await.expect("execute");
    assert_eq!(result.text_out(), "---", "printf --- should produce ---");
}

#[tokio::test]
async fn printf_dash_only_after_double_dash() {
    // printf -- --- : a real `--` end-of-flags marker followed by the
    // dash-only operand. Used to parse-error (a spurious second `--` token
    // fell out of mis-lexing `---`, with no grammar production for it).
    let k = kernel();
    let result = k.execute("printf -- ---").await.expect("execute");
    assert_eq!(result.text_out(), "---", "printf -- --- should produce ---");
}
