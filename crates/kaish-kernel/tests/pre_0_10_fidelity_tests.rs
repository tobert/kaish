//! Kernel-routed regression tests for the pre-0.10 fidelity bundle.
//!
//! Each fix turns a previously *silent* wrong behavior loud or correct
//! (`docs/issues.md`):
//!   - `sed -e <numeric>` was silently dropped → now a loud error.
//!   - `cp -p`/`--preserve` was parsed then discarded → now rejected (the VFS
//!     has no attributes to preserve, so the flag is no longer advertised).
//!   - unterminated `$(( … ` silently evaluated the partial expression → now a
//!     loud lexer error.
//!   - `printf '%c'` ignored width/flags, and `%b`'s `\c` only truncated its
//!     own argument instead of stopping all output.
//!
//! Each routes through `kernel.execute()` so the full pipeline runs.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::transient().with_skip_validation(true)).expect("kernel")
}

// ---------------------------------------------------------------------------
// sed -e <numeric> → loud
// ---------------------------------------------------------------------------

#[tokio::test]
async fn sed_numeric_expression_is_loud_not_dropped() {
    // `sed -e 5` binds `5` as an integer; it must not be silently ignored
    // (which once let the *filename* be parsed as the program).
    let k = kernel();
    let result = k
        .execute("echo abc | sed -e 5 -e 's/a/X/'")
        .await
        .expect("execute");
    assert_eq!(result.code, 2, "numeric -e must fail (POSIX usage), got {result:?}");
    assert!(
        result.err.contains("must be a string"),
        "stderr should explain the non-string expression: {:?}",
        result.err
    );
}

#[tokio::test]
async fn sed_string_expression_still_works() {
    // Positive control: a normal string -e is unaffected.
    let k = kernel();
    let result = k
        .execute("echo abc | sed -e 's/a/X/'")
        .await
        .expect("execute");
    assert_eq!(result.code, 0, "valid sed must succeed: {result:?}");
    assert_eq!(result.text_out().trim_end(), "Xbc");
}

#[tokio::test]
async fn sed_substitute_zero_occurrence_is_loud() {
    // `s///0` ("replace the 0th match") is meaningless — GNU sed errors instead
    // of silently treating it as the first match.
    let k = kernel();
    let result = k
        .execute("echo aaa | sed 's/a/b/0'")
        .await
        .expect("execute");
    assert_ne!(result.code, 0, "s///0 must fail, got {result:?}");
    assert!(
        result.err.contains("may not be zero"),
        "stderr should reject the zero occurrence: {:?}",
        result.err
    );
}

// ---------------------------------------------------------------------------
// cp -p → rejected
// ---------------------------------------------------------------------------

#[tokio::test]
async fn cp_preserve_flag_is_rejected() {
    // The VFS can't preserve mode/mtime/ownership, so `-p` must not be a
    // silent no-op — clap rejects it as an unknown argument.
    let k = kernel();
    let result = k.execute("cp -p a b").await.expect("execute");
    assert_eq!(result.code, 2, "cp -p must be a usage error: {result:?}");
    assert!(
        result.err.contains("-p"),
        "stderr should name the rejected flag: {:?}",
        result.err
    );
}

// ---------------------------------------------------------------------------
// Unterminated $(( → loud
// ---------------------------------------------------------------------------

#[tokio::test]
async fn unterminated_arithmetic_is_loud() {
    // `$(( 1 + 2` (no closing `))`) must not silently evaluate to 3.
    let k = kernel();
    let result = k.execute("echo $(( 1 + 2").await;
    let err = result.expect_err("unterminated arithmetic must error");
    let msg = format!("{err:#}");
    assert!(
        msg.contains("unterminated arithmetic"),
        "error should name the unterminated arithmetic: {msg}"
    );
}

#[tokio::test]
async fn terminated_arithmetic_still_works() {
    // Positive control: the closed form still evaluates.
    let k = kernel();
    let result = k.execute("echo $(( 1 + 2 ))").await.expect("execute");
    assert_eq!(result.text_out().trim_end(), "3");
}

// ---------------------------------------------------------------------------
// printf %c width + %b \c whole-format stop
// ---------------------------------------------------------------------------

#[tokio::test]
async fn printf_percent_c_honors_width() {
    let k = kernel();
    let result = k.execute("printf '[%5c]' x").await.expect("execute");
    assert_eq!(result.text_out(), "[    x]", "%c should right-pad to width 5");
}

#[tokio::test]
async fn printf_percent_c_left_align() {
    let k = kernel();
    let result = k.execute("printf '[%-5c]' x").await.expect("execute");
    assert_eq!(result.text_out(), "[x    ]", "%-5c should left-pad to width 5");
}

#[tokio::test]
async fn printf_percent_b_backslash_c_stops_all_output() {
    // `\c` inside a %b argument stops ALL further output, including the
    // trailing literal `c` of the format — not just the rest of the argument.
    let k = kernel();
    let result = k.execute(r#"printf "a%bc" "X\cY""#).await.expect("execute");
    assert_eq!(result.text_out(), "aX", "%b \\c must stop the whole format");
}

#[tokio::test]
async fn printf_format_literal_backslash_c_stops_output() {
    // `\c` in the format literal itself also stops output (GNU printf).
    let k = kernel();
    let result = k.execute(r#"printf "a\cb""#).await.expect("execute");
    assert_eq!(result.text_out(), "a", "format-literal \\c must stop output");
}
