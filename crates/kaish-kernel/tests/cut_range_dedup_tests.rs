//! Kernel-routed tests for `cut` range deduplication and ordering.
//!
//! Bug: overlapping or repeated ranges (e.g. `-f 1-3,2-4`) emitted duplicate
//! fields because `select_indices` accumulated into a plain Vec without
//! deduplication. GNU cut outputs each position at most once, in increasing
//! order, regardless of how the ranges were specified.
//!
//! Tests route through `kernel.execute()` so the full dispatch chain runs.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use common::run;
use kaish_kernel::{Kernel, KernelConfig};

fn transient_kernel() -> Kernel {
    Kernel::new(KernelConfig::transient()).expect("kernel")
}

// --- overlapping field ranges ------------------------------------------------

#[tokio::test]
async fn cut_fields_overlapping_ranges_no_duplicates() {
    // `cut -f 1-3,2-4` on a 5-field tab-separated line.
    // GNU cut: union is fields 1-4, output once each in order.
    let kernel = transient_kernel();
    let (out, code) = run(&kernel, "printf 'a\\tb\\tc\\td\\te' | cut -f \"1-3,2-4\"").await;
    assert_eq!(code, 0, "exit code: {out}");
    assert_eq!(out, "a\tb\tc\td", "overlapping ranges must be unioned, not duplicated");
}

#[tokio::test]
async fn cut_fields_repeated_single_field_no_duplicates() {
    // `cut -f 2,2` — same field listed twice.
    let kernel = transient_kernel();
    let (out, code) = run(&kernel, "printf 'x\\ty\\tz' | cut -f \"2,2\"").await;
    assert_eq!(code, 0, "exit code: {out}");
    assert_eq!(out, "y", "repeated field must appear only once");
}

// --- out-of-order ranges → output in field-number order ---------------------

#[tokio::test]
async fn cut_fields_out_of_order_emits_ascending() {
    // GNU cut: `cut -f 3,1` emits field 1 then field 3, not 3 then 1.
    let kernel = transient_kernel();
    let (out, code) = run(&kernel, "printf 'alpha\\tbeta\\tgamma' | cut -f \"3,1\"").await;
    assert_eq!(code, 0, "exit code: {out}");
    assert_eq!(out, "alpha\tgamma", "fields must be emitted in ascending position order");
}

#[tokio::test]
async fn cut_fields_out_of_order_range_emits_ascending() {
    // `cut -f 4-5,1-2` — two non-overlapping ranges given in reverse order.
    let kernel = transient_kernel();
    let (out, code) = run(&kernel, "printf '1\\t2\\t3\\t4\\t5' | cut -f \"4-5,1-2\"").await;
    assert_eq!(code, 0, "exit code: {out}");
    assert_eq!(out, "1\t2\t4\t5", "reversed range order must still output fields ascending");
}

// --- character mode dedup / order -------------------------------------------

#[tokio::test]
async fn cut_chars_overlapping_ranges_no_duplicates() {
    // `cut -c 1-3,2-4` on "abcde" → "abcd" (union, ascending, no repeats).
    let kernel = transient_kernel();
    let (out, code) = run(&kernel, "printf 'abcde' | cut -c \"1-3,2-4\"").await;
    assert_eq!(code, 0, "exit code: {out}");
    assert_eq!(out, "abcd", "overlapping char ranges must be unioned");
}

#[tokio::test]
async fn cut_chars_out_of_order_emits_ascending() {
    // `cut -c 4,1` → chars at positions 1 then 4 (ascending order).
    let kernel = transient_kernel();
    let (out, code) = run(&kernel, "printf 'abcde' | cut -c \"4,1\"").await;
    assert_eq!(code, 0, "exit code: {out}");
    assert_eq!(out, "ad", "char positions must be emitted in ascending order");
}

// --- smoke: simple non-overlapping cases still work -------------------------

#[tokio::test]
async fn cut_fields_simple_list_unchanged() {
    // Non-overlapping, already-ascending list: must still work correctly.
    let kernel = transient_kernel();
    let (out, code) = run(&kernel, "printf 'a\\tb\\tc\\td\\te' | cut -f \"1,3,5\"").await;
    assert_eq!(code, 0, "exit code: {out}");
    assert_eq!(out, "a\tc\te", "simple non-overlapping list must work");
}

#[tokio::test]
async fn cut_fields_single_range_unchanged() {
    let kernel = transient_kernel();
    let (out, code) = run(&kernel, "printf 'w\\tx\\ty\\tz' | cut -f \"2-3\"").await;
    assert_eq!(code, 0, "exit code: {out}");
    assert_eq!(out, "x\ty", "single range must still work");
}
