//! Numeric operand coercion: kaish numbers are JSON numbers.
//!
//! `value_to_num` (`[[ ]]`/`test` numeric ops) and `value_to_exit_code`
//! (`return`/`exit`) both parse a string operand as `i64` then `f64`. JSON
//! has no `inf`/`nan`, and an i64-shaped string that only overflows must
//! name the 64-bit limit rather than round through `f64` or say a generic
//! "numeric argument required".
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

/// `"1e309"` overflows to `f64::INFINITY` and used to compare as a real
/// number; `inf`/`nan` already refused (they are not a `.`/`e`/`E` spelling
/// `value_to_num` recognizes, so they never reach the `f64` parse at all —
/// they fail as a non-numeric string). All three must refuse now, and none
/// of them by way of the unrelated NaN-comparison diagnostic.
#[tokio::test]
async fn non_finite_operands_are_refused_not_compared() {
    for source in [r#"[[ "1e309" -gt 1 ]]"#, "[[ inf -gt 1 ]]", "[[ nan -eq nan ]]"] {
        let text = err_of(source).await;
        assert!(
            !text.contains("NaN comparison"),
            "{source:?} must not reach the NaN-comparison diagnostic: {text:?}"
        );
    }
}

/// A float spelling that overflows names the 64-bit range, the same way an
/// integer-shaped overflow does.
#[tokio::test]
async fn overflowing_float_spelling_names_the_range() {
    let text = err_of(r#"[[ "1e309" -gt 1 ]]"#).await;
    assert!(text.contains("64-bit float range"), "must name the range: {text:?}");
}

/// Ordinary float spellings are unaffected.
#[tokio::test]
async fn ordinary_float_operands_still_compare() {
    let (code, _, err) = run(r#"[[ "1.5" -gt 1 ]]"#).await;
    assert_eq!(code, 0, "must still compare: {err:?}");
    let (code, _, err) = run(r#"[[ "1e3" -eq 1000 ]]"#).await;
    assert_eq!(code, 0, "must still compare: {err:?}");
}

/// `return`/`exit` coerce their operand the same way `[[ ]]` does — an
/// i64-shaped string that only overflows must name the 64-bit limit, not
/// say a generic "numeric argument required" as if the text were not a
/// number at all.
#[tokio::test]
async fn return_of_an_overflowing_string_names_the_limit() {
    let text = err_of(r#"x="9223372036854775808"; f() { return $x; }; f"#).await;
    assert!(text.contains("64-bit"), "must name the limit: {text:?}");
}
