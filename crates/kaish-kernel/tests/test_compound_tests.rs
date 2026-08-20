//! `test` grows the XSI compound operators: `-a`, `-o`, `!`, and `( )`.
//!
//! kaish deliberately omitted them in 0.11 and reported a usage error naming
//! `&&`/`||` as the fix. The error was clear — and invisible where `test`
//! actually gets used:
//!
//! ```text
//! if test a = a -o b = c; then echo yes; else echo no; fi   →   no
//! ```
//!
//! An `if` condition reads the exit code, and exit 2 is not zero, so the
//! diagnostic never reaches the author and the expression reads as false. A
//! shell that omits an operator its users will type had better not fail that
//! quietly, so `test` implements them instead.
//!
//! **Every expected exit code in this file was produced by `bash -c` and
//! copied in.** Precedence is bash's: `!` binds tightest, then `-a`, then
//! `-o`; `( )` groups. `-a` and `-o` are also *unary* operators in the
//! two-operand form (`-a FILE` is "file exists", `-o NAME` is "shell option
//! set"), which is the ambiguity POSIX warns about and the reason the
//! operand-count rules come first.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

async fn code(script: &str) -> i64 {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel");
    k.execute(script).await.expect("kernel execute").code
}

/// `(script, bash's exit code)`.
///
/// Paren cases are spelled with quoted parens because an unquoted `(` is the
/// *shell's* subshell syntax, not an operand — `bash -c 'test ( a = a )'`
/// measures bash's parser, not bash's `test`. Getting that wrong the first
/// time reported exit 2 for every paren row.
#[rstest]
// ── operand-count rules, unchanged ────────────────────────────────
#[case("test", 1)]
#[case("test \"\"", 1)]
#[case("test x", 0)]
#[case("test ! x", 1)]
#[case("test ! \"\"", 0)]
#[case("test a = a", 0)]
#[case("test a = b", 1)]
#[case("test ! a = a", 1)]
#[case("test ! a = b", 0)]
// ── binary OR ─────────────────────────────────────────────────────
#[case("test a = a -o b = c", 0)]
#[case("test a = b -o b = b", 0)]
#[case("test a = b -o b = c", 1)]
#[case("test x -o y", 0)]
#[case("test \"\" -o \"\"", 1)]
#[case("test -z \"\" -o -z x", 0)]
// ── binary AND ────────────────────────────────────────────────────
#[case("test a = a -a b = b", 0)]
#[case("test a = a -a b = c", 1)]
#[case("test a = b -a b = c", 1)]
#[case("test -n x -a -n y", 0)]
#[case("test -n x -a -z y", 1)]
// ── precedence: -a binds tighter than -o ──────────────────────────
#[case("test a = a -o b = b -a c = d", 0)]
#[case("test a = b -a c = d -o e = e", 0)]
// ── `!` binds tighter than -a/-o ──────────────────────────────────
#[case("test ! a = a -o b = b", 0)]
#[case("test ! a = a -a b = b", 1)]
#[case("test ! ! x", 0)]
#[case("test ! ! ! \"\"", 0)]
// ── grouping ──────────────────────────────────────────────────────
#[case("test '(' a = a ')'", 0)]
#[case("test '(' a = a -o b = c ')'", 0)]
#[case("test '(' a = b -o b = b ')' -a c = c", 0)]
#[case("test a = b -o '(' b = b -a c = c ')'", 0)]
#[case("test ! '(' a = a ')'", 1)]
#[case("test '(' x ')'", 0)]
// ── malformed: exit 2, never a surprise true/false ────────────────
#[case("test x -a", 2)]
#[case("test x -o", 2)]
#[case("test a = a -o", 2)]
#[case("test '(' ')'", 2)]
#[case("test '(' a = a", 2)]
#[tokio::test]
async fn matches_bash(#[case] script: &str, #[case] expected: i64) {
    assert_eq!(
        code(script).await,
        expected,
        "`{script}` should exit {expected}, the way bash does"
    );
}

/// Four operands beginning with `!` negate the whole three-operand
/// expression, connective included — `test ! x -o x` is `!(x -o x)`, false,
/// not `(!x) -o x`, true. bash's arity rule beats the precedence that governs
/// longer expressions, and this is the shape where the two disagree.
///
/// Found by a 420-case differential sweep against `bash -c`, not by reading:
/// the hand-written table above had `! a = a -o b = b` (five operands, parser
/// territory) and missed the four-operand case entirely.
#[rstest]
#[case("test ! x -o x", 1)]
#[case("test ! \"\" -o x", 1)]
#[case("test ! x -a \"\"", 0)]
#[case("test ! \"\" -a \"\"", 0)]
#[tokio::test]
async fn four_operands_after_bang_negate_the_whole_expression(
    #[case] script: &str,
    #[case] expected: i64,
) {
    assert_eq!(code(script).await, expected, "`{script}`");
}

/// Operands that look like flags stay operands, at every length — the
/// property the raw-argv binding exists for. `-n = -n` is string equality,
/// not `-n` applied to `=`, which a greedy unary rule got wrong on the first
/// draft of the expression parser.
#[rstest]
#[case("test -n = -n", 0)]
#[case("test -f = -f", 0)]
#[case("test x = -f", 1)]
#[case("test = = =", 0)]
#[case("test -f -f", 1)]
#[case("test ! = x", 1)]
#[tokio::test]
async fn flag_shaped_operands_stay_operands(#[case] script: &str, #[case] expected: i64) {
    assert_eq!(code(script).await, expected, "`{script}`");
}

/// One operand that is an operator stays a **loud error**, which is kaish's
/// one deliberate divergence from bash here.
///
/// bash reads `test -z` as the non-empty string `"-z"` and returns true, which
/// silently turns a forgotten operand into a passing condition — the exact
/// failure mode this whole change exists to remove. kaish keeps the 0.11
/// behavior and says which operand is missing. Zero operands DOES conform
/// (false), because an absent expression cannot be a typo'd one.
#[rstest]
#[case("test -o")]
#[case("test -a")]
#[case("test -z")]
#[case("test !")]
#[tokio::test]
async fn a_lone_operator_is_loud_not_a_string(#[case] script: &str) {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let r = k.execute(script).await.expect("kernel execute");
    assert_eq!(r.code, 2, "`{script}` should be a usage error, not bash's true");
    assert!(
        r.err.contains("needs an operand"),
        "`{script}` should name the missing operand, got: {}",
        r.err
    );
}

/// bash's *unary* `-a`/`-o` are deliberately absent, and that is what keeps
/// the spelling unambiguous.
///
/// bash reads `test -a FILE` as a deprecated synonym for `-e` and
/// `test -o NAME` as "shell option NAME is on". Having a second meaning at
/// two operands is exactly why `EXPR -a EXPR` is ambiguous to parse —
/// coreutils' own man page warns about it. kaish gives each spelling one
/// meaning: `-a`/`-o` connect expressions, `-e` tests existence, and anything
/// else is loud.
#[rstest]
#[case("test -a /dev/null")]
#[case("test -a /nonexistent-xyz")]
#[case("test -o trash")]
#[case("test -o nosuchopt")]
#[case("test ! -a \"\"")]
#[case("test ! -o x")]
#[tokio::test]
async fn unary_dash_a_and_dash_o_are_loud(#[case] script: &str) {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let r = k.execute(script).await.expect("kernel execute");
    assert_eq!(r.code, 2, "`{script}` should be a usage error, not a file/option test");
    assert!(!r.err.is_empty(), "`{script}` must carry a diagnostic");
}

/// The bug that started this: the expression is evaluated, so the `if` takes
/// the branch bash takes rather than falling to `else` on a usage error
/// nobody could see.
#[tokio::test]
async fn the_reported_case() {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let r = k
        .execute("if test a = a -o b = c; then echo yes; else echo no; fi")
        .await
        .expect("kernel execute");
    assert_eq!(r.text_out().trim_end(), "yes");
}

/// A malformed expression inside a condition still reads as false — that is
/// how a shell condition works — so the value of implementing the operators
/// is that the common spelling is no longer malformed at all.
#[tokio::test]
async fn a_real_usage_error_is_still_exit_2() {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let r = k.execute("test a = a -o").await.expect("kernel execute");
    assert_eq!(r.code, 2);
    assert!(!r.err.is_empty(), "exit 2 must carry a diagnostic");
}
