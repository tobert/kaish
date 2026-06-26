//! Small independent correctness/surprise fixes batched from `docs/issues.md`:
//!
//! 1. `grep -c` exits 1 on zero matches (GNU parity) — was exit 0.
//! 2. `$(cmd)` trims only trailing newlines, not all trailing whitespace —
//!    a bare `$()` used `.trim_end()` (stripping spaces/tabs too), diverging
//!    from the interpolation and for-loop paths which trim newlines only.
//! 3. `jq '. / 0'` fails loudly instead of silently returning `null` — jaq
//!    evaluates `n/0` to a non-finite float that JSON can't represent, which
//!    `val_to_json` was coercing to `null` (silent-wrong).

#![cfg(feature = "localfs")]

mod common;

use common::{kernel_at, run};

// ─────────────────────────── grep -c exit code ───────────────────────────

#[tokio::test]
async fn grep_c_zero_matches_exits_1() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "echo 'foo' | grep -c bar").await;
    assert_eq!(out, "0", "count text still printed");
    assert_eq!(code, 1, "zero matches must exit 1 (GNU parity)");
}

#[tokio::test]
async fn grep_c_with_matches_exits_0() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "printf 'foo\\nfoo\\nbar\\n' | grep -c foo").await;
    assert_eq!(out, "2");
    assert_eq!(code, 0);
}

#[tokio::test]
async fn grep_c_multifile_exits_1_only_if_no_file_matches() {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(tmp.path().join("a.txt"), "alpha\n").unwrap();
    std::fs::write(tmp.path().join("b.txt"), "beta\n").unwrap();
    let kernel = kernel_at(tmp.path());

    // No file contains "zzz" → exit 1.
    let (_out, code) = run(&kernel, "grep -c zzz a.txt b.txt").await;
    assert_eq!(code, 1, "no match in any file must exit 1");

    // One file matches → exit 0.
    let (_out, code) = run(&kernel, "grep -c alpha a.txt b.txt").await;
    assert_eq!(code, 0, "a match in any file must exit 0");
}

// ───────────────────── $(cmd) trailing-whitespace trim ─────────────────────

#[tokio::test]
async fn cmd_subst_preserves_trailing_spaces() {
    // The trailing spaces sit *inside* the brackets, so the test harness's
    // outer trim can't hide a regression. Before the fix `.trim_end()` ate them.
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "x=$(printf 'a  b  '); echo \"[$x]\"").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "[a  b  ]", "trailing spaces must survive command subst");
}

#[tokio::test]
async fn cmd_subst_still_strips_trailing_newlines() {
    // The trailing-newline strip (POSIX) is unchanged.
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "x=$(printf 'hi\\n\\n'); echo \"[$x]\"").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "[hi]", "trailing newlines must still be stripped");
}

// ─────────────────────────── jq division by zero ───────────────────────────

#[tokio::test]
async fn jq_division_by_zero_is_loud() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (_out, code) = run(&kernel, "echo '6' | jq '. / 0'").await;
    assert_ne!(code, 0, "division by zero must fail loudly, not return null");
}

#[tokio::test]
async fn jq_zero_over_zero_nan_is_loud() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (_out, code) = run(&kernel, "echo '0' | jq '. / 0'").await;
    assert_ne!(code, 0, "0/0 (NaN) must fail loudly, not return null");
}

#[tokio::test]
async fn jq_finite_division_still_works() {
    // Regression guard: ordinary (finite) division is untouched by the
    // non-finite check — exit 0 and a real numeric value, not `null`. The
    // integral result renders as `3` (jq number canonicalization), not `3.0`.
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "echo '6' | jq '. / 2'").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "3");
}
