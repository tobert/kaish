//! Kernel-routed tests for the `find` expression grammar: `-o`, `-a`, `!`,
//! and `( )` grouping, with GNU precedence (`!` over `-a` over `-o`).
//!
//! Parentheses are quoted (`'('`) so the tests do not depend on how the lexer
//! spells `\(`.

#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;

use common::kernel_at;
use tempfile::tempdir;

/// a.txt, b.rs, c.md, sub/d.txt, sub/e.rs
fn build_tree(dir: &std::path::Path) {
    fs::create_dir_all(dir.join("sub")).unwrap();
    for name in ["a.txt", "b.rs", "c.md", "sub/d.txt", "sub/e.rs"] {
        fs::write(dir.join(name), "x\n").unwrap();
    }
}

/// Run `script`; return (sorted stdout lines without the bare `.` entry,
/// stderr, exit code).
async fn find(script: &str) -> (Vec<String>, String, i64) {
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(script).await.expect("kernel execute");
    let mut lines: Vec<String> = result
        .text_out()
        .lines()
        .map(str::to_string)
        .filter(|l| l != ".")
        .collect();
    lines.sort();
    (lines, result.err.clone(), result.code)
}

fn expect(lines: &[&str]) -> Vec<String> {
    let mut v: Vec<String> = lines.iter().map(|s| s.to_string()).collect();
    v.sort();
    v
}

#[tokio::test]
async fn find_or_of_types_lists_both() {
    let (out, err, code) = find("find . -type f -o -type d").await;
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(
        out,
        expect(&["./a.txt", "./b.rs", "./c.md", "./sub", "./sub/d.txt", "./sub/e.rs"])
    );
}

#[tokio::test]
async fn find_or_of_names() {
    let (out, err, code) = find("find . -name '*.rs' -o -name '*.md'").await;
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out, expect(&["./b.rs", "./c.md", "./sub/e.rs"]));
}

#[tokio::test]
async fn find_or_word_spelling() {
    let (out, err, code) = find("find . -name '*.rs' -or -name '*.md'").await;
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out, expect(&["./b.rs", "./c.md", "./sub/e.rs"]));
}

#[tokio::test]
async fn find_and_binds_tighter_than_or() {
    // -name '*.txt' -o (-name '*.rs' -a -type d): no directory ends in .rs,
    // so only the .txt files match. Left-to-right evaluation would also
    // return the .rs files.
    let (out, err, code) = find("find . -name '*.txt' -o -name '*.rs' -a -type d").await;
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out, expect(&["./a.txt", "./sub/d.txt"]));
}

#[tokio::test]
async fn find_explicit_and() {
    let (out, err, code) = find("find . -name '*.txt' -a -name 'd*'").await;
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out, expect(&["./sub/d.txt"]));
}

#[tokio::test]
async fn find_group_overrides_precedence() {
    let (out, err, code) =
        find("find . '(' -name '*.txt' -o -name '*.rs' ')' -type f -name 'e*'").await;
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out, expect(&["./sub/e.rs"]));
}

#[tokio::test]
async fn find_group_is_not_just_a_flat_or() {
    let (out, err, code) = find("find . -type f '(' -name '*.md' -o -name '*.rs' ')'").await;
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out, expect(&["./b.rs", "./c.md", "./sub/e.rs"]));
}

#[tokio::test]
async fn find_negation_bang_and_word() {
    for negation in ["'!'", "-not"] {
        let (out, err, code) = find(&format!("find . {negation} -name '*.txt' -type f")).await;
        assert_eq!(code, 0, "{negation}: {err}");
        assert_eq!(out, expect(&["./b.rs", "./c.md", "./sub/e.rs"]), "{negation}");
    }
}

#[tokio::test]
async fn find_negated_group() {
    let (out, err, code) =
        find("find . -type f '!' '(' -name '*.txt' -o -name '*.rs' ')'").await;
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out, expect(&["./c.md"]));
}

#[tokio::test]
async fn find_depth_option_applies_to_the_whole_expression() {
    let (out, err, code) = find("find . -maxdepth 1 -name '*.rs' -o -name '*.md'").await;
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out, expect(&["./b.rs", "./c.md"]));
}

#[tokio::test]
async fn find_explicit_print_limits_output_to_printed_branches() {
    let (out, err, code) = find("find . -name a.txt -print -o -name b.rs").await;
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out, expect(&["./a.txt"]));
}

#[tokio::test]
async fn find_iname_ignores_case() {
    let (out, err, code) = find("find . -iname 'A.TXT'").await;
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out, expect(&["./a.txt"]));
}

#[tokio::test]
async fn find_implicit_and_still_works() {
    let (out, err, code) = find("find . -type f -name '*.rs'").await;
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out, expect(&["./b.rs", "./sub/e.rs"]));
}

#[tokio::test]
async fn find_unbalanced_open_group_is_refused() {
    let (out, err, code) = find("find . '(' -name '*.rs'").await;
    assert_eq!(code, 2, "stdout: {out:?}");
    assert!(out.is_empty(), "no partial results: {out:?}");
    assert!(err.contains("find:") && err.contains("'('"), "stderr: {err}");
}

#[tokio::test]
async fn find_stray_close_group_is_refused() {
    let (_, err, code) = find("find . -name '*.rs' ')'").await;
    assert_eq!(code, 2);
    assert!(err.contains("find:") && err.contains("')'"), "stderr: {err}");
}

#[tokio::test]
async fn find_dangling_or_is_refused() {
    let (_, err, code) = find("find . -name '*.rs' -o").await;
    assert_eq!(code, 2);
    assert!(err.contains("find: -o"), "stderr: {err}");
    assert!(!err.contains("unexpected argument"), "stderr: {err}");
}

#[tokio::test]
async fn find_predicate_without_value_is_refused() {
    let (_, err, code) = find("find . -name").await;
    assert_eq!(code, 2);
    assert!(err.contains("find: -name"), "stderr: {err}");
}

#[tokio::test]
async fn find_unsupported_predicate_says_not_supported() {
    let (out, err, code) = find("find . -delete").await;
    assert_eq!(code, 2, "stdout: {out:?}");
    assert!(err.contains("find: -delete is not supported"), "stderr: {err}");
    assert!(!err.contains("--"), "no clap hint: {err}");
}

#[tokio::test]
async fn find_maxdepth_one_stops_at_direct_children() {
    // GNU depth: ./b.rs is 1, ./sub/e.rs is 2.
    let (out, err, code) = find("find . -maxdepth 1 -name '*.rs'").await;
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out, expect(&["./b.rs"]));
}

#[tokio::test]
async fn find_maxdepth_two_reaches_grandchildren() {
    let (out, err, code) = find("find . -maxdepth 2 -name '*.rs'").await;
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(out, expect(&["./b.rs", "./sub/e.rs"]));
}

#[rstest::rstest]
#[case("-print -print", 2)]
#[case("-print -o -print", 1)]
#[case("! -print", 1)]
#[case("'(' -print -a -print ')'", 2)]
#[tokio::test]
async fn each_evaluated_print_action_emits_a_row(#[case] expression: &str, #[case] rows: usize) {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("a.txt"), "data\n").unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(&format!("find a.txt {expression}")).await.unwrap();
    assert_eq!(result.code, 0, "{}", result.err);
    assert_eq!(result.text_out().lines().collect::<Vec<_>>(), vec!["a.txt"; rows]);
}

#[rstest::rstest]
#[case("--json")]
#[case("--json=false")]
#[case("--json=true")]
#[tokio::test]
async fn a_global_output_flag_consumed_as_a_pattern_stays_data(#[case] pattern: &str) {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join(pattern), "data\n").unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(&format!("find . -name {pattern}")).await.unwrap();
    assert_eq!(result.code, 0, "{}", result.err);
    assert_eq!(result.text_out().lines().collect::<Vec<_>>(), vec![format!("./{pattern}")]);
}
