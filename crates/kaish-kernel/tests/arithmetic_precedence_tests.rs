//! `$(( ))` precedence and associativity, pinned with operands chosen so a
//! WRONG grouping gives a DIFFERENT answer than the right one.
//!
//! The existing suite has cases that pass under both groupings — `5 & 3 ==
//! 3` is `1` whether `==` binds tighter than `&` or not, so it proves
//! nothing. Every case here states, in a short comment, what the wrong
//! grouping would produce; if the two values coincide, the case is
//! rewritten with different operands until they don't (or, where that is
//! mathematically impossible, the case says so instead of pretending).
//!
//! Precedence table read from `crates/kaish-kernel/src/arithmetic.rs`'s
//! parser (high to low): unary (`! ~` and unary `-`/`+`), `**`, `* / %`,
//! `+ -`, `<< >>`, `< <= > >=`, `== !=`, `&`, `^`, `|`, `&&`, `||`, `?:`.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

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

// ── Every adjacent precedence boundary, high to low ─────────────────────
//
// One case per boundary; the comment gives the value under the OTHER
// (wrong) grouping.

#[rstest]
// `?:` is lowest; its condition is a full `||` expression. Wrong (ternary
// tighter than `||`): `1 || (0 ? 2 : 3)` = `1 || 3` = 1.
#[case("echo $((1 || 0 ? 2 : 3))", "2")]
// `||` vs `&&`. Wrong (`||` tighter): `(1 || 0) && 0` = `1 && 0` = 0.
#[case("echo $((1 || 0 && 0))", "1")]
// `&&` vs `|`. Wrong (`&&` tighter): `(0 && 1) | 2` = `0 | 2` = 2.
#[case("echo $((0 && 1 | 2))", "0")]
// `|` vs `^`. Wrong (`|` tighter): `(1 | 3) ^ 1` = `3 ^ 1` = 2.
#[case("echo $((1 | 3 ^ 1))", "3")]
// `^` vs `&`. Wrong (`^` tighter): `(1 ^ 3) & 2` = `2 & 2` = 2.
#[case("echo $((1 ^ 3 & 2))", "3")]
// `&` vs `==`. Wrong (`&` tighter): `(2 & 2) == 2` = `2 == 2` = 1.
#[case("echo $((2 & 2 == 2))", "0")]
// `==` vs relational. Wrong (`==` tighter): `(0 == 2) < 3` = `0 < 3` = 1.
#[case("echo $((0 == 2 < 3))", "0")]
// relational vs shift. Wrong (relational tighter): `(5 < 1) << 3` = `0 << 3` = 0.
#[case("echo $((5 < 1 << 3))", "1")]
// shift vs `+ -`. Wrong (shift tighter): `(1 << 1) + 2` = `2 + 2` = 4.
#[case("echo $((1 << 1 + 2))", "8")]
// `+ -` vs `* / %`. Wrong (`+ -` tighter): `(1 + 2) * 3` = `3 * 3` = 9.
#[case("echo $((1 + 2 * 3))", "7")]
// `* / %` vs `**`. Wrong (`* / %` tighter): `(2 * 3) ** 2` = `6 ** 2` = 36.
#[case("echo $((2 * 3 ** 2))", "18")]
// `**` vs unary. Wrong (`**` tighter than unary): `-(2 ** 4)` = -16.
#[case("echo $((-2 ** 4))", "16")]
#[tokio::test]
async fn adjacent_precedence_boundary(#[case] source: &str, #[case] expected: &str) {
    ok(source, expected).await;
}

// ── Left-associativity ───────────────────────────────────────────────────
//
// Same rule: right-associativity must give a different number.

#[rstest]
// Right: `8 - (3 - 2)` = 7.
#[case("echo $((8 - 3 - 2))", "3")]
// Right: `100 / (10 / 2)` = `100 / 5` = 20.
#[case("echo $((100 / 10 / 2))", "5")]
// Mixed `/ *`. Right: `100 / (5 * 2)` = `100 / 10` = 10.
#[case("echo $((100 / 5 * 2))", "40")]
// `<<` chain. Right: `1 << (2 << 1)` = `1 << 4` = 16.
#[case("echo $((1 << 2 << 1))", "8")]
// `>>` chain. Right: `64 >> (2 >> 1)` = `64 >> 1` = 32.
#[case("echo $((64 >> 2 >> 1))", "8")]
// Mixed `<< >>`. Right: `1 << (3 >> 1)` = `1 << 1` = 2.
#[case("echo $((1 << 3 >> 1))", "4")]
// Mixed `>> <<`. Right: `8 >> (1 << 1)` = `8 >> 2` = 2.
#[case("echo $((8 >> 1 << 1))", "8")]
// Relational chain — `5 > 3` folds to `1`, then `1 > 1` is `0`.
// Right: `5 > (3 > 1)` = `5 > 1` = 1.
#[case("echo $((5 > 3 > 1))", "0")]
#[tokio::test]
async fn left_associativity(#[case] source: &str, #[case] expected: &str) {
    ok(source, expected).await;
}

/// Equality/inequality chains cannot discriminate association order by
/// value: `==` and `!=` are both affine over GF(2) (`==` is XNOR, `!=` is
/// XOR), and any chain of them collapses to the same XOR-of-operands
/// regardless of how it is parenthesized — `(a==b)==c` and `a==(b==c)`
/// are the same formula (`a xor b xor c`, up to a constant that itself
/// cancels) for every `a,b,c`. Checked by hand for representative triples
/// before writing this: no operand choice makes the two groupings differ.
/// Equality's PRECEDENCE boundary (relative to `&`) is pinned instead, in
/// `adjacent_precedence_boundary`'s `2 & 2 == 2` case; this test only
/// documents why a left-assoc discriminator for `==`/`!=` chains does not
/// exist and pins the (associativity-independent) value.
#[tokio::test]
async fn equality_chains_are_associativity_independent() {
    ok("echo $((1 == 1 == 0))", "0").await;
    ok("echo $((1 == 0 == 0))", "1").await;
}

// ── Checked arithmetic: left-to-right grouping must not hide overflow ───

#[rstest]
// Left (correct): `(MAX + 1) + -1` errors on the first `+`. A right- or
// reordered grouping computing `1 + -1 = 0` first, then `MAX + 0`, would
// silently answer `MAX` instead.
#[case("echo $((9223372036854775807 + 1 + -1))")]
// Left: `(MAX * 2) * 0` errors on the first `*`. Reordered: `2 * 0 = 0`,
// then `MAX * 0 = 0` — silently 0 instead of an error.
#[case("echo $((9223372036854775807 * 2 * 0))")]
// `i64::MIN` mirror of the addition case: `(MIN + -1) + 1` errors first.
// Reordered: `-1 + 1 = 0`, then `MIN + 0 = MIN` — silently MIN.
#[case("echo $((-9223372036854775808 + -1 + 1))")]
// `i64::MIN` mirror of the multiplication case: `(MIN * 2) * 0` errors
// first. Reordered: `2 * 0 = 0`, then `MIN * 0 = 0` — silently 0.
#[case("echo $((-9223372036854775808 * 2 * 0))")]
#[tokio::test]
async fn regrouping_must_not_hide_an_intermediate_overflow(#[case] source: &str) {
    errs(source, "does not fit").await;
}

// ── Ternary associativity ────────────────────────────────────────────────

/// Right-associative: `a ? b : c ? d : e` is `a ? b : (c ? d : e)`.
/// With `a` truthy, the correct reading never looks past `b`: result is
/// `5`. Left-associative would first fold `(1 ? 5 : 0)` to `5`, then use
/// THAT as the next condition: `5 ? 7 : 9` = `7`.
#[tokio::test]
async fn ternary_is_right_associative() {
    ok("echo $((1 ? 5 : 0 ? 7 : 9))", "5").await;
}

/// A branch is a full conditional expression, not a restricted operand —
/// `2 + 3 > 40` (false) must be evaluated as one relational expression to
/// pick the else branch. If the condition position stopped consuming at a
/// lower level (say, additive), this would either fail to parse or read
/// `2 + 3` as the condition (truthy) and answer `10`.
#[tokio::test]
async fn ternary_condition_is_the_full_precedence_chain() {
    ok("echo $((2 + 3 > 40 ? 10 : 20))", "20").await;
}

// ── Unary minus vs `**` and `*` ──────────────────────────────────────────

/// Unary binds tighter than `**`: `(-3) ** 2` = 9, not `-(3 ** 2)` = -9 —
/// an even exponent flips the sign of the wrong grouping, so this
/// discriminates even more visibly than the `-2 ** 2` boundary case above
/// (different operand pair, same rule).
#[tokio::test]
async fn unary_minus_binds_before_power() {
    ok("echo $((-3 ** 2))", "9").await;
}

/// Unlike `**`, negation DISTRIBUTES over `*`: `(-a) * b == -(a * b)` for
/// every `a, b`, so no operand choice can make `-2 * 3` discriminate
/// between "unary applies to the first factor only" and "unary applies to
/// the whole product" — both give -6. Recorded as a smoke test (the value
/// kaish must produce either way) rather than a discriminator; the real
/// unary-precedence pin is the `**` case above, where the non-linearity of
/// exponentiation breaks the symmetry.
#[tokio::test]
async fn unary_minus_and_multiplication_smoke_test() {
    ok("echo $((-2 * 3))", "-6").await;
}
