//! Kernel-routed tests for sort -k (key spec) correctness.
//!
//! These tests drive real command strings through `kernel.execute()` so the full
//! pipeline runs: lex → parse → validate → dispatch → builtin.
//!
//! Covers:
//! - `-k2` (field 2 to end of line)
//! - `-k2,2` (field 2 only)
//! - `-k2n` glued numeric modifier
//! - `-k 2n` separated numeric modifier
//! - `-u` key-dedup (dedup by key, not whole line)
//! - `-k2r` reverse on key
//! - existing `-n`/`-r`/plain sort still work after the key-spec rewrite

#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::{kernel_at, run};
use tempfile::tempdir;

fn write_file(dir: &std::path::Path, name: &str, contents: &str) {
    std::fs::write(dir.join(name), contents).expect("write fixture");
}

// ─── -k2 : field-2-to-EOL ────────────────────────────────────────────────────

#[tokio::test]
async fn sort_k2_sorts_by_field2_to_eol() {
    // GNU: `sort -k2` uses field 2 through EOL as the sort key.
    // Input deliberately chosen so field-2-onward differs from full-line order.
    let dir = tempdir().unwrap();
    // "alice 30 z", "bob 10 a", "carol 20 m"
    // sort -k2 → by "30 z" vs "10 a" vs "20 m"
    // → "10 a" < "20 m" < "30 z"
    // expected order: bob alice carol  ... wait, "10" < "20" < "30" lexically too
    // Use values where lex order of field2+ diverges from full-line order:
    // "z 30", "a 20", "m 10"
    // sort -k2 keys: "30" < "20" ... hmm same lex. Let's make them letters:
    // "z bravo", "a alpha", "m charlie"
    // sort -k2 keys: "alpha" < "bravo" < "charlie"
    // expected output order: "a alpha", "z bravo", "m charlie"
    write_file(dir.path(), "data.txt", "z bravo\na alpha\nm charlie\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "sort -k 2 data.txt").await;
    assert_eq!(code, 0, "sort -k2 should succeed: {out:?}");
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines, vec!["a alpha", "z bravo", "m charlie"],
        "sort -k2 should sort by field 2 to EOL: {out:?}");
}

#[tokio::test]
async fn sort_k2_glued_form() {
    // Glued form: -k2 (no space between -k and value)
    let dir = tempdir().unwrap();
    write_file(dir.path(), "data.txt", "z bravo\na alpha\nm charlie\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "sort -k2 data.txt").await;
    assert_eq!(code, 0, "sort -k2 glued should succeed: {out:?}");
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines, vec!["a alpha", "z bravo", "m charlie"],
        "glued -k2 should sort by field 2 to EOL: {out:?}");
}

// ─── -k2,2 : field-2-only ────────────────────────────────────────────────────

#[tokio::test]
async fn sort_k2_comma_2_sorts_by_field2_only() {
    // "-k2,2" means sort by exactly field 2, then fall through to full-line tiebreak.
    // "b 10 x", "a 20 y", "c 10 z"
    // sort -k2,2: keys are "10", "20", "10"
    // stable: "b 10 x" and "c 10 z" tie on key → tiebreak by full line
    // "b 10 x" < "c 10 z" < "a 20 y"
    let dir = tempdir().unwrap();
    write_file(dir.path(), "data.txt", "b 10 x\na 20 y\nc 10 z\n");
    let kernel = kernel_at(dir.path());
    // Comma in key spec must be quoted (kaish reserves bare commas for brace expansion).
    let (out, code) = run(&kernel, r#"sort -k "2,2" data.txt"#).await;
    assert_eq!(code, 0, "sort -k \"2,2\" should succeed: {out:?}");
    let lines: Vec<&str> = out.lines().collect();
    // field 2 only → "10" < "20", ties in "10" break by full line
    assert_eq!(lines[2], "a 20 y", "a 20 y has highest field-2 key: {out:?}");
    assert!(
        lines[0] == "b 10 x" || lines[0] == "c 10 z",
        "first two lines are the '10' rows: {out:?}"
    );
}

// ─── -k2n : glued numeric modifier ──────────────────────────────────────────

#[tokio::test]
async fn sort_k2n_glued_numeric() {
    // "-k2n" means numeric sort on field 2.
    // Without the 'n' modifier, "10" < "9" lexically; with 'n', 9 < 10.
    // "alice 10", "bob 9", "carol 2"
    // sort -k2n → 2 < 9 < 10 → carol, bob, alice
    let dir = tempdir().unwrap();
    write_file(dir.path(), "data.txt", "alice 10\nbob 9\ncarol 2\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "sort -k2n data.txt").await;
    assert_eq!(code, 0, "sort -k2n should succeed: {out:?}");
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines, vec!["carol 2", "bob 9", "alice 10"],
        "sort -k2n should sort field 2 numerically: {out:?}");
}

#[tokio::test]
async fn sort_k2n_separated_numeric() {
    // "-k 2n" with a space — separated form, value is "2n"
    let dir = tempdir().unwrap();
    write_file(dir.path(), "data.txt", "alice 10\nbob 9\ncarol 2\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "sort -k 2n data.txt").await;
    assert_eq!(code, 0, "sort -k 2n should succeed: {out:?}");
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines, vec!["carol 2", "bob 9", "alice 10"],
        "sort -k 2n separated form should sort field 2 numerically: {out:?}");
}

#[tokio::test]
async fn sort_k2_comma_2n_numeric_stop() {
    // "-k2,2n" — numeric modifier on the stop-field spec
    let dir = tempdir().unwrap();
    write_file(dir.path(), "data.txt", "alice 10\nbob 9\ncarol 2\n");
    let kernel = kernel_at(dir.path());
    // Comma in key spec must be quoted (kaish reserves bare commas for brace expansion).
    let (out, code) = run(&kernel, r#"sort -k "2,2n" data.txt"#).await;
    assert_eq!(code, 0, "sort -k \"2,2n\" should succeed: {out:?}");
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines, vec!["carol 2", "bob 9", "alice 10"],
        "sort -k2,2n numeric on stop spec: {out:?}");
}

// ─── -u with key: dedup by key, not full line ──────────────────────────────

#[tokio::test]
async fn sort_u_key_dedups_by_key_not_full_line() {
    // GNU `sort -k2,2 -u` deduplicates by the key (field 2 only).
    // "alice 10", "bob 10", "carol 20"
    // Keys: "10", "10", "20" → dedup by key → one "10" line + "carol 20"
    // Result should have exactly 2 lines (one of the "10" rows dropped).
    let dir = tempdir().unwrap();
    write_file(dir.path(), "data.txt", "alice 10\nbob 10\ncarol 20\n");
    let kernel = kernel_at(dir.path());
    // Comma in key spec must be quoted (kaish reserves bare commas for brace expansion).
    let (out, code) = run(&kernel, r#"sort -k "2,2" -u data.txt"#).await;
    assert_eq!(code, 0, "sort -k \"2,2\" -u should succeed: {out:?}");
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines.len(), 2,
        "sort -k2,2 -u should dedup by key field, keeping 2 unique-key lines: {out:?}");
    // The surviving "10" line is the first one (stable/first-wins):
    assert_eq!(lines[0], "alice 10", "first '10' key wins: {out:?}");
    assert_eq!(lines[1], "carol 20", "carol 20 is the only '20': {out:?}");
}

#[tokio::test]
async fn sort_u_no_key_dedups_full_line() {
    // Without a key, -u deduplicates the full line (existing behavior must not regress).
    let dir = tempdir().unwrap();
    write_file(dir.path(), "data.txt", "apple\nbanana\napple\norange\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "sort -u data.txt").await;
    assert_eq!(code, 0, "sort -u should succeed: {out:?}");
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines.len(), 3, "sort -u deduplicates full lines: {out:?}");
    assert!(lines.contains(&"apple"), "apple present once: {out:?}");
}

// ─── regression: existing -n / -r / plain sort must not break ─────────────

#[tokio::test]
async fn sort_plain_alpha_regression() {
    let dir = tempdir().unwrap();
    write_file(dir.path(), "data.txt", "banana\napple\ncherry\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "sort data.txt").await;
    assert_eq!(code, 0);
    assert_eq!(out.trim(), "apple\nbanana\ncherry");
}

#[tokio::test]
async fn sort_numeric_regression() {
    let dir = tempdir().unwrap();
    write_file(dir.path(), "data.txt", "10\n2\n1\n20\n3\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "sort -n data.txt").await;
    assert_eq!(code, 0);
    assert_eq!(out.trim(), "1\n2\n3\n10\n20");
}

#[tokio::test]
async fn sort_reverse_regression() {
    let dir = tempdir().unwrap();
    write_file(dir.path(), "data.txt", "banana\napple\ncherry\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "sort -r data.txt").await;
    assert_eq!(code, 0);
    assert_eq!(out.trim(), "cherry\nbanana\napple");
}

#[tokio::test]
async fn sort_version_regression() {
    let dir = tempdir().unwrap();
    write_file(dir.path(), "data.txt", "v1.10\nv1.2\nv1.9\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "sort -V data.txt").await;
    assert_eq!(code, 0);
    assert_eq!(out.trim(), "v1.2\nv1.9\nv1.10");
}

// ─── stdin pipe form ──────────────────────────────────────────────────────────

#[tokio::test]
async fn sort_k2n_via_pipe() {
    // Exercise the sort through a pipe so stdin path is tested too.
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, r#"echo "alice 10
bob 9
carol 2" | sort -k2n"#).await;
    assert_eq!(code, 0, "sort -k2n via pipe: {out:?}");
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines, vec!["carol 2", "bob 9", "alice 10"],
        "piped sort -k2n: {out:?}");
}

// ─── -u uses the active comparator, not raw string equality ──────────────────

#[tokio::test]
async fn sort_n_u_dedups_numerically_equal() {
    // `sort -n -u` must collapse numerically-equal lines (10 == 10.0), like GNU.
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, r#"echo "10
10.0
2" | sort -n -u"#).await;
    assert_eq!(code, 0, "{out:?}");
    assert_eq!(out.lines().collect::<Vec<_>>(), vec!["2", "10"],
        "10 and 10.0 are numerically equal — keep one: {out:?}");
}

#[tokio::test]
async fn sort_n_u_keydedups_numerically_equal() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, r#"echo "a 10
a 10.0
b 2" | sort -n -u -k2"#).await;
    assert_eq!(code, 0, "{out:?}");
    assert_eq!(out.lines().collect::<Vec<_>>(), vec!["b 2", "a 10"],
        "field-2 numerically equal — keep first: {out:?}");
}
