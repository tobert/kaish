//! Kernel-routed coverage for the `test` builtin.
//!
//! `test` follows kaish's `[[` semantics but as a *command*: it receives the
//! true source-ordered argv (via the schema's `raw_argv` opt-in), so operands
//! that look like flags (`test $x = -n`, `test 0 -gt -5`) survive the normal
//! flag/positional split. Numeric comparison is kaish's own (JSON) number
//! semantics — floats are fine, identical to `[[` — NOT POSIX integer-only.
//!
//! Every case runs a real command string through `kernel.execute()` so the
//! full lex → parse → validate → dispatch → builtin path runs, and asserts the
//! exit code (0 true / 1 false / 2 usage-or-type error).

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;

use common::{kernel_at, run};
use tempfile::tempdir;

/// Run `script` and return just the exit code.
async fn code_of(script: &str) -> i64 {
    let dir = tempdir().unwrap();
    // Seed a known file + dir so file tests have something real to stat.
    fs::write(dir.path().join("file.txt"), "hi\n").unwrap();
    fs::create_dir(dir.path().join("sub")).unwrap();
    let kernel = kernel_at(dir.path());
    let (_out, code) = run(&kernel, script).await;
    code
}

// --- string emptiness -------------------------------------------------------

#[tokio::test]
async fn string_emptiness() {
    assert_eq!(code_of(r#"test -z """#).await, 0, "-z empty is true");
    assert_eq!(code_of("test -z x").await, 1, "-z non-empty is false");
    assert_eq!(code_of("test -n x").await, 0, "-n non-empty is true");
    assert_eq!(code_of(r#"test -n """#).await, 1, "-n empty is false");
}

// --- one-argument truthiness (POSIX `test STRING`) --------------------------

#[tokio::test]
async fn one_arg_string_truthiness() {
    assert_eq!(code_of("test x").await, 0, "non-empty single arg is true");
    assert_eq!(code_of(r#"test """#).await, 1, "empty single arg is false");
}

// --- string equality (=, ==, !=) --------------------------------------------

#[tokio::test]
async fn string_equality() {
    assert_eq!(code_of("test a = a").await, 0);
    assert_eq!(code_of("test a = b").await, 1);
    assert_eq!(code_of("test a == a").await, 0, "== is a literal-equality alias");
    assert_eq!(code_of("test a != b").await, 0);
    assert_eq!(code_of("test a != a").await, 1);
}

/// The killer case for the ordered-argv work: an operand that *looks* like a
/// flag (`-n`, `-f`) must survive as a literal operand, not get hoisted into
/// the flag set. A reconstruct-from-ToolArgs `test` would mis-handle these.
#[tokio::test]
async fn flag_shaped_operands_survive() {
    assert_eq!(code_of("test -n = -n").await, 0, r#""-n" equals "-n""#);
    assert_eq!(code_of("test x = -f").await, 1, r#""x" != "-f", -f is an operand"#);
    assert_eq!(code_of("test -f = -f").await, 0, r#""-f" equals "-f""#);
}

// --- numeric comparison: kaish JSON-number semantics (floats OK) ------------

#[tokio::test]
async fn numeric_integer() {
    assert_eq!(code_of("test 5 -eq 5").await, 0);
    assert_eq!(code_of("test 5 -eq 3").await, 1);
    assert_eq!(code_of("test 5 -ne 3").await, 0);
    assert_eq!(code_of("test 5 -gt 3").await, 0);
    assert_eq!(code_of("test 3 -gt 5").await, 1);
    assert_eq!(code_of("test 3 -lt 5").await, 0);
    assert_eq!(code_of("test 5 -ge 5").await, 0);
    assert_eq!(code_of("test 4 -le 5").await, 0);
    assert_eq!(code_of("test 08 -eq 8").await, 0, "leading zero is decimal, not octal");
}

/// Negative numbers as operands (second position is the killer spot — a `-5`
/// there would be eaten as a flag without raw_argv).
#[tokio::test]
async fn numeric_negative_operands() {
    assert_eq!(code_of("test 0 -gt -5").await, 0, "0 > -5, -5 survives as operand");
    assert_eq!(code_of("test -5 -lt 0").await, 0, "-5 < 0");
}

/// Reversed panel decision (Amy): numeric is `[[`-consistent — floats work,
/// because numbers are JSON numbers. POSIX `test` would error on `1.5`.
#[tokio::test]
async fn numeric_floats_allowed() {
    assert_eq!(code_of("test 1.5 -gt 1").await, 0, "float operand compares, not errors");
    assert_eq!(code_of("test 1.0 -eq 1").await, 0, "1.0 equals 1 numerically");
    assert_eq!(code_of("test 2.5 -lt 2").await, 1, "2.5 is not < 2");
}

/// Non-numeric operands to a numeric op are LOUD (exit 2), never silently 0.
#[tokio::test]
async fn numeric_non_numeric_is_loud() {
    assert_eq!(code_of("test abc -eq 5").await, 2, "non-numeric string is a usage error");
}

// --- file tests (VFS-aware) -------------------------------------------------

#[tokio::test]
async fn file_tests() {
    assert_eq!(code_of("test -e file.txt").await, 0, "-e existing");
    assert_eq!(code_of("test -e nope").await, 1, "-e missing");
    assert_eq!(code_of("test -f file.txt").await, 0, "-f regular file");
    assert_eq!(code_of("test -f sub").await, 1, "-f on a dir is false");
    assert_eq!(code_of("test -d sub").await, 0, "-d directory");
    assert_eq!(code_of("test -d file.txt").await, 1, "-d on a file is false");
}

/// Proves the *builtin* (not external `/usr/bin/test`) is in play and it stats
/// through the kernel's VFS: floats work here where GNU test would exit 2.
/// (Kept alongside file tests as a belt-and-suspenders "is the builtin live".)
#[tokio::test]
async fn builtin_shadows_external_test() {
    // GNU /usr/bin/test 1.5 -gt 1 => exit 2 (integer syntax error).
    // Our builtin => 0. If this is 2, we're accidentally shelling out.
    assert_eq!(code_of("test 1.5 -gt 1").await, 0);
}

// --- negation (`test ! <primary>`) -----------------------------------------

#[tokio::test]
async fn negation() {
    assert_eq!(code_of("test ! -f nope").await, 0, "! (nope is not a file) => true");
    assert_eq!(code_of("test ! -f file.txt").await, 1, "! (file.txt is a file) => false");
    assert_eq!(code_of("test ! a = a").await, 1, "! (a==a) => false");
    assert_eq!(code_of("test ! a = b").await, 0, "! (a!=b as ==) => true");
    assert_eq!(code_of("test ! -z x").await, 0, "! (-z x is false) => true");
}

// --- arity / usage errors (loud, per the 'require operands' decision) -------

#[tokio::test]
async fn arity_errors_are_loud() {
    assert_eq!(code_of("test").await, 2, "0-arg test is a usage error");
    assert_eq!(code_of("test -f").await, 2, "operator with missing operand is loud");
    assert_eq!(code_of("test -z").await, 2, "operator with missing operand is loud");
    assert_eq!(code_of("test !").await, 2, "bare ! has no expression");
    assert_eq!(code_of("test a b").await, 2, "two barewords, no operator");
}

// --- compound (-a/-o) is rejected loudly, shell chaining is the path --------

#[tokio::test]
async fn compound_operators_rejected() {
    assert_eq!(code_of("test -f file.txt -a -f sub").await, 2, "-a is not supported");
    assert_eq!(code_of("test a = a -o b = b").await, 2, "-o is not supported");
}

#[tokio::test]
async fn shell_chaining_is_the_compound_path() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("file.txt"), "hi\n").unwrap();
    fs::create_dir(dir.path().join("sub")).unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "test -f file.txt && test -d sub && echo both").await;
    assert_eq!(code, 0, "and-chain of two true tests succeeds: {out:?}");
    assert_eq!(out, "both");

    let (out, code) = run(&kernel, "test -f nope || echo fallback").await;
    assert_eq!(code, 0);
    assert_eq!(out, "fallback", "or-chain runs the fallback on a false test");
}

// --- collection operands are a loud Shape error (Decision E) ----------------

#[tokio::test]
async fn collection_operand_is_loud() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (_out, code) = run(&kernel, "xs=[1 2 3]; test $xs = foo").await;
    assert_eq!(code, 2, "a list operand to = is a loud usage/shape error, not silent");
}

// --- test as an `if` condition (the primary use site) -----------------------

#[tokio::test]
async fn test_as_if_condition() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("file.txt"), "hi\n").unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(
        &kernel,
        r#"if test -f file.txt; then echo present; else echo absent; fi"#,
    )
    .await;
    assert_eq!(code, 0);
    assert_eq!(out, "present");
}
