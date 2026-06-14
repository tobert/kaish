//! Pre-execution arity validation for `diff` (E011 DiffNeedsTwoFiles).
//!
//! `diff` compares exactly two files. Before this check, `diff a` failed loud
//! only at runtime ("missing second file") and `diff a b c` *silently dropped*
//! the third operand. The validator now blocks both up front, with E011, while
//! leaving dynamic operand counts (`diff *.txt`, `diff $a $b`) alone — their
//! runtime expansion is unknowable statically.

mod common;

use common::kernel_at;
use std::fs;

/// Validation rejects too-few/too-many *literal* operands with E011.
#[tokio::test]
async fn diff_one_operand_blocks_with_e011() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let err = kernel
        .execute("diff only.txt")
        .await
        .expect_err("one operand should fail validation");
    let msg = err.to_string();
    assert!(msg.contains("E011"), "should name the code: {msg}");
    assert!(
        msg.contains("two file operands"),
        "should explain the arity: {msg}"
    );
    assert!(msg.contains("got 1"), "should report the actual count: {msg}");
}

#[tokio::test]
async fn diff_three_operands_blocks_with_e011() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    // The silently-dropped-third-operand case: `diff a b c` used to diff a vs b
    // and ignore c. Now it is a loud validation error.
    let err = kernel
        .execute("diff a.txt b.txt c.txt")
        .await
        .expect_err("three operands should fail validation");
    let msg = err.to_string();
    assert!(msg.contains("E011"), "should name the code: {msg}");
    assert!(msg.contains("got 3"), "should report the actual count: {msg}");
}

#[tokio::test]
async fn diff_zero_operands_blocks_with_e011() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let err = kernel
        .execute("diff")
        .await
        .expect_err("zero operands should fail validation");
    assert!(err.to_string().contains("E011"), "got: {err}");
}

/// Two real file operands pass validation and run normally.
#[tokio::test]
async fn diff_two_operands_passes_and_runs() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("a.txt"), "same\n").unwrap();
    fs::write(tmp.path().join("b.txt"), "same\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute("diff a.txt b.txt")
        .await
        .expect("two operands should pass validation and execute");
    assert_eq!(result.code, 0, "identical files diff to exit 0: {result:?}");
}

/// The space-form value flag `-C 3` parks its value in `positional` at
/// validation time; the check must discount it, not miscount it as a third
/// operand.
#[tokio::test]
async fn diff_context_space_flag_does_not_inflate_count() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("a.txt"), "x\n").unwrap();
    fs::write(tmp.path().join("b.txt"), "y\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute("diff -C 3 a.txt b.txt")
        .await
        .expect("`-C 3` plus two files should pass validation");
    // Files differ → exit 1, but crucially not a validation error.
    assert_eq!(result.code, 1, "differing files → exit 1: {result:?}");
}

/// A glob operand has an unknown runtime count — skip the arity check so a
/// legitimate two-file expansion is not rejected.
#[tokio::test]
async fn diff_glob_operand_skips_arity_check() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("a.txt"), "same\n").unwrap();
    fs::write(tmp.path().join("b.txt"), "same\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute("diff *.txt")
        .await
        .expect("glob expanding to two files should pass validation and run");
    assert_eq!(result.code, 0, "identical files diff to exit 0: {result:?}");
}

/// A glob that the validator waved through but expands to 3+ files must still
/// fail loud at runtime, not silently drop the surplus.
#[tokio::test]
async fn diff_glob_expanding_to_three_files_errors_at_runtime() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("a.txt"), "1\n").unwrap();
    fs::write(tmp.path().join("b.txt"), "2\n").unwrap();
    fs::write(tmp.path().join("c.txt"), "3\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute("diff *.txt")
        .await
        .expect("validation skips dynamic; runtime guard fires");
    assert_eq!(result.code, 2, "surplus operands → exit 2: {result:?}");
    assert!(
        result.err.contains("two file operands") && result.err.contains("got 3"),
        "runtime guard should name the arity: {:?}",
        result.err
    );
}

/// Variable operands are `<dynamic>` at validation time — skip the check.
#[tokio::test]
async fn diff_variable_operands_skip_arity_check() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("a.txt"), "same\n").unwrap();
    fs::write(tmp.path().join("b.txt"), "same\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute(r#"A="a.txt"; B="b.txt"; diff $A $B"#)
        .await
        .expect("variable operands should pass validation and run");
    assert_eq!(result.code, 0, "identical files diff to exit 0: {result:?}");
}
