//! Kernel-routed tests for the `random` builtin.
//!
//! kaish has no `$RANDOM` variable; `random` is its typed replacement. These
//! tests drive real command strings through `kernel.execute()` so the full
//! pipeline runs (lex → parse → validate → dispatch → builtin → `--json`),
//! not just the bare `Random::execute` entry point.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::{kernel_at, run};
use std::collections::HashSet;
use tempfile::tempdir;

/// 200 draws of `random --max 6` all land in `0..=6`, and the draws are not
/// all the same value — pins both the bound and that it's actually random.
#[tokio::test]
async fn stays_in_bounds_and_varies() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let mut seen: HashSet<i64> = HashSet::new();
    for _ in 0..200 {
        let (out, code) = run(&kernel, "random --max 6").await;
        assert_eq!(code, 0, "random --max 6 should succeed: {out:?}");
        let n: i64 = out.parse().expect("random --max 6 prints an integer");
        assert!((0..=6).contains(&n), "{n} outside 0..=6");
        seen.insert(n);
    }
    assert!(
        seen.len() >= 2,
        "200 draws of random --max 6 produced only {:?} — looks non-random",
        seen
    );
}

/// `--min N --max N` has exactly one legal value.
#[tokio::test]
async fn min_equals_max_prints_that_value() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "random --min 5 --max 5").await;
    assert_eq!(code, 0);
    assert_eq!(out, "5");
}

/// Negative bounds are legal and the draw stays inside them.
#[tokio::test]
async fn negative_range_stays_in_range() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    for _ in 0..50 {
        let (out, code) = run(&kernel, "random --min -5 --max 5").await;
        assert_eq!(code, 0);
        let n: i64 = out.parse().expect("integer output");
        assert!((-5..=5).contains(&n), "{n} outside -5..=5");
    }
}

/// `$(random)` binds a typed number, not a string — `typeof` must say so.
#[tokio::test]
async fn command_substitution_is_typed_as_number() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "typeof $(random)").await;
    assert_eq!(code, 0, "typeof $(random) should succeed: {out:?}");
    assert_eq!(out, "number");
}

/// The typed capture is usable directly in arithmetic.
#[tokio::test]
async fn captured_value_is_arithmetic_ready() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "x=$(random --min 3 --max 3); echo $((x * 2))").await;
    assert_eq!(code, 0, "arithmetic on captured random should succeed: {out:?}");
    assert_eq!(out, "6");
}

/// `--min` greater than `--max` is a curated exit-2 error naming both bounds.
#[tokio::test]
async fn min_greater_than_max_is_an_error() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("random --min 10 --max 5").await.unwrap();
    assert_eq!(result.code, 2, "got: {:?}", result.err);
    assert!(
        result.err.contains("--min 10 is greater than --max 5"),
        "error should name both bounds: {:?}",
        result.err
    );
}

/// A positional argument is refused with a curated error naming the fix.
#[tokio::test]
async fn positional_argument_is_an_error() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("random 100").await.unwrap();
    assert_eq!(result.code, 2, "got: {:?}", result.err);
    assert!(
        result.err.contains("--max 100"),
        "error should name the fix: {:?}",
        result.err
    );
}

/// A non-integer bound is a clap parse error, exit 2.
#[tokio::test]
async fn non_integer_bound_is_an_error() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("random --max abc").await.unwrap();
    assert_eq!(result.code, 2, "got: {:?}", result.err);
}

/// `--json` output is a bare JSON number, not a string or an envelope.
#[tokio::test]
async fn json_output_is_a_number() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("random --max 6 --json").await.unwrap();
    assert!(result.ok(), "random --json should succeed: {:?}", result);
    let parsed: serde_json::Value =
        serde_json::from_str(result.text_out().trim()).expect("random --json is JSON");
    assert!(parsed.is_number(), "random --json should be a JSON number: {parsed}");
}

/// The full i64 span must not panic or overflow the range mapper.
#[tokio::test]
async fn full_i64_span_succeeds() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(
        &kernel,
        "random --min -9223372036854775808 --max 9223372036854775807",
    )
    .await;
    assert_eq!(code, 0, "full i64 span should succeed: {out:?}");
    let n: i64 = out.parse().expect("integer output");
    assert!((i64::MIN..=i64::MAX).contains(&n));
}
