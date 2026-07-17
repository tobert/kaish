//! Spec tests for the newline-split rule in for-loop `$(cmd)` position.
//!
//! These tests describe the behavior specified in `docs/LANGUAGE.md` (the
//! for-loop `$(cmd)` newline-split rule). Each test is a fact about the spec,
//! not a description of the implementation.
//!
//! The rule (one-line): when a `for` iteration item is `$(cmd)` and the
//! substitution returns `Value::String` containing `\n`, split on `\n`
//! after trimming trailing newlines. Everywhere else strings stay whole.
//!
//! Notes for the test author:
//! - `Kernel::transient()` exercises the full kernel; no special wiring
//!   needed.
//! - Builtins like `printf`, `cat`, `seq`, `echo` are sufficient for
//!   stdin/stdout fixtures; we don't need a portable external command.
//! - Tempfiles use `tempfile::NamedTempFile` per the established pattern
//!   in `shell_bugs_tests.rs`. No hardcoded system paths.

use kaish_kernel::Kernel;
use std::io::Write;

// ---------------------------------------------------------------------------
// Splits per line: the muscle-memory case the rule is built for.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn for_subst_printf_multiline_iterates_per_line() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            N=0
            for line in $(printf 'a\nb\nc\n'); do
                N=$((N + 1))
                echo "got=$line"
            done
            echo "count=$N"
            "#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    let text = result.text_out();
    assert!(text.contains("got=a"), "missing 'got=a' in:\n{text}");
    assert!(text.contains("got=b"), "missing 'got=b' in:\n{text}");
    assert!(text.contains("got=c"), "missing 'got=c' in:\n{text}");
    assert!(text.contains("count=3"), "expected 3 iterations:\n{text}");
}

// The only test in this file that touches a real host path (via
// `tempfile::NamedTempFile` + `cat`) — `Kernel::transient()` only mounts a
// real filesystem under the `localfs` feature; without it, `transient()`
// falls back to `KernelConfig::isolated()` (NoLocal, in-memory `/`), so the
// absolute tempfile path would resolve to nothing. Every other test in this
// file only touches virtual builtins (echo/printf/seq/jq/split) and runs
// featureless.
#[cfg(feature = "localfs")]
#[tokio::test]
async fn for_subst_cat_file_iterates_per_line() {
    let kernel = Kernel::transient().unwrap();

    let mut tmp = tempfile::NamedTempFile::new().unwrap();
    writeln!(tmp, "alpha").unwrap();
    writeln!(tmp, "beta").unwrap();
    writeln!(tmp, "gamma").unwrap();
    tmp.flush().unwrap();
    let path = tmp.path().display();

    let script = format!(
        r#"
        N=0
        for word in $(cat {path}); do
            N=$((N + 1))
            echo "w=$word"
        done
        echo "count=$N"
        "#
    );
    let result = kernel.execute(&script).await.unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    let text = result.text_out();
    assert!(text.contains("w=alpha"), "missing 'w=alpha':\n{text}");
    assert!(text.contains("w=beta"), "missing 'w=beta':\n{text}");
    assert!(text.contains("w=gamma"), "missing 'w=gamma':\n{text}");
    assert!(text.contains("count=3"), "expected 3 iterations:\n{text}");
}

// ---------------------------------------------------------------------------
// Whitespace within a line is NEVER split — the "$VAR with spaces just
// works" promise is preserved.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn for_subst_echo_with_spaces_iterates_once() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            N=0
            for x in $(echo "a b c"); do
                N=$((N + 1))
                echo "got=[$x]"
            done
            echo "count=$N"
            "#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    let text = result.text_out();
    assert!(text.contains("got=[a b c]"), "should preserve whole string:\n{text}");
    assert!(text.contains("count=1"), "expected 1 iteration, got:\n{text}");
}

#[tokio::test]
async fn for_subst_single_line_no_newline_iterates_once() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            N=0
            for x in $(printf 'lonely'); do
                N=$((N + 1))
                echo "got=[$x]"
            done
            echo "count=$N"
            "#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    let text = result.text_out();
    assert!(text.contains("got=[lonely]"), "missing 'got=[lonely]':\n{text}");
    assert!(text.contains("count=1"), "expected 1 iteration:\n{text}");
}

// ---------------------------------------------------------------------------
// Trailing-newline handling.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn for_subst_trailing_newline_does_not_create_phantom_item() {
    // printf 'a\nb\n' must yield exactly 2 iterations, not 3.
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            N=0
            for x in $(printf 'a\nb\n'); do
                N=$((N + 1))
                echo "got=[$x]"
            done
            echo "count=$N"
            "#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    let text = result.text_out();
    assert!(text.contains("got=[a]"), "missing 'got=[a]':\n{text}");
    assert!(text.contains("got=[b]"), "missing 'got=[b]':\n{text}");
    assert!(text.contains("count=2"), "expected 2 iterations, got:\n{text}");
    assert!(!text.contains("got=[]"), "should not have a phantom empty item:\n{text}");
}

#[tokio::test]
async fn for_subst_interior_empty_line_preserved() {
    // printf 'a\n\nb\n' must yield ["a", "", "b"] — only trailing newlines
    // are trimmed; interior empty lines stay.
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            N=0
            for x in $(printf 'a\n\nb\n'); do
                N=$((N + 1))
                echo "got=[$x]"
            done
            echo "count=$N"
            "#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    let text = result.text_out();
    assert!(text.contains("got=[a]"), "missing 'got=[a]':\n{text}");
    assert!(text.contains("got=[]"), "missing empty-line iteration:\n{text}");
    assert!(text.contains("got=[b]"), "missing 'got=[b]':\n{text}");
    assert!(text.contains("count=3"), "expected 3 iterations:\n{text}");
}

#[tokio::test]
async fn for_subst_empty_stdout_zero_iterations() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            N=0
            for x in $(printf ''); do
                N=$((N + 1))
                echo "should-not-print"
            done
            echo "count=$N"
            "#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    let text = result.text_out();
    assert!(text.contains("count=0"), "expected 0 iterations:\n{text}");
    assert!(!text.contains("should-not-print"), "body should not run:\n{text}");
}

// ---------------------------------------------------------------------------
// `.data` precedence: structured-array iteration is untouched.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn for_subst_seq_still_uses_data_array() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            N=0
            for i in $(seq 1 3); do
                N=$((N + 1))
                echo "i=$i"
            done
            echo "count=$N"
            "#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    let text = result.text_out();
    assert!(text.contains("i=1"), "missing i=1:\n{text}");
    assert!(text.contains("i=2"), "missing i=2:\n{text}");
    assert!(text.contains("i=3"), "missing i=3:\n{text}");
    assert!(text.contains("count=3"), "expected 3 iterations:\n{text}");
}

#[tokio::test]
async fn for_subst_jq_extract_still_uses_data_array() {
    // jq emits .data; structured iteration should win over newline split.
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            N=0
            for name in $(echo '["alice","bob","carol"]' | jq -r '.[]'); do
                N=$((N + 1))
                echo "hello-$name"
            done
            echo "count=$N"
            "#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    let text = result.text_out();
    assert!(text.contains("hello-alice"), "missing alice:\n{text}");
    assert!(text.contains("hello-bob"), "missing bob:\n{text}");
    assert!(text.contains("hello-carol"), "missing carol:\n{text}");
    assert!(text.contains("count=3"), "expected 3 iterations:\n{text}");
}

// ---------------------------------------------------------------------------
// Quoting suppresses the new behavior — same as bash's IFS= discipline.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn for_subst_quoted_iterates_once_with_embedded_newlines() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            N=0
            for x in "$(printf 'a\nb\nc')"; do
                N=$((N + 1))
            done
            echo "count=$N"
            "#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    let text = result.text_out();
    assert!(text.contains("count=1"), "quoted subst should iterate once:\n{text}");
}

// ---------------------------------------------------------------------------
// Newline-split is for-iteration-position only — assignment, argv, and
// string interpolation keep the whole string.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn assignment_does_not_split_multiline_subst() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            R=$(printf 'a\nb\nc')
            echo "len=${#R}"
            echo "value=[$R]"
            "#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    let text = result.text_out();
    // "a\nb\nc" is 5 chars (a, \n, b, \n, c). If anyone broke
    // assignment by accidentally splitting, the length would change.
    assert!(text.contains("len=5"), "assignment should preserve full string:\n{text}");
    // The literal newlines should survive in the captured variable.
    assert!(text.contains("value=[a\nb\nc]"), "newlines should be preserved:\n{text}");
}

#[tokio::test]
async fn string_interpolation_does_not_split_multiline_subst() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            echo "before|$(printf 'a\nb')|after"
            "#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    let text = result.text_out();
    // The substituted multi-line value sits inside a single string;
    // the result should still be one logical echo with embedded newline.
    assert!(text.contains("before|a\nb|after"), "interpolation should preserve newlines:\n{text}");
}

#[tokio::test]
async fn argv_does_not_split_multiline_subst() {
    // `echo $(printf 'a\nb')` — kaish treats $() in argv position as one
    // Value, so echo sees a single argument containing the embedded newline.
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            echo "[" $(printf 'a\nb') "]"
            "#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    let text = result.text_out();
    // The exact rendering of `echo` with an embedded newline is
    // intentionally not over-specified — what matters is that echo
    // received the multi-line blob, not two separate args.
    assert!(text.contains("a\nb"), "argv should pass multi-line string:\n{text}");
}

// ---------------------------------------------------------------------------
// CRLF handling: per-line iteration trims trailing \r so Windows-origin
// files don't leak carriage returns.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn for_subst_crlf_trims_carriage_returns() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            N=0
            for x in $(printf 'a\r\nb\r\nc\r\n'); do
                N=$((N + 1))
                echo "len=${#x}"
                echo "got=[$x]"
            done
            echo "count=$N"
            "#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    let text = result.text_out();
    assert!(text.contains("count=3"), "expected 3 iterations:\n{text}");
    // Each line is one char ('a', 'b', 'c') — if \r leaked in, length
    // would be 2 instead of 1.
    assert!(text.contains("len=1"), "each line should be 1 char (no \\r):\n{text}");
    assert!(!text.contains("len=2"), "stray \\r leaked into a line:\n{text}");
}

// ---------------------------------------------------------------------------
// Regression guards: things the rule must NOT change.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn regression_for_bare_var_still_validator_error() {
    // E012 should remain a hard error — the bareword scalar case has no
    // \n-list-stdout signal to lean on. This guards the validator path.
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            ITEMS="a b c"
            for i in $ITEMS; do echo $i; done
            "#,
        )
        .await;
    assert!(result.is_err(), "bare $VAR in for should still error");
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("word splitting") || err.contains("iterate once") || err.contains("E012"),
        "error should still mention E012/word splitting: {err}"
    );
}

#[tokio::test]
async fn regression_while_condition_unchanged() {
    // `while` has no iteration list; the change must not touch it.
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            N=0
            while [[ $N -lt 3 ]]; do
                N=$((N + 1))
                echo "n=$N"
            done
            "#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "while loop should run: err={}", result.err);
    let text = result.text_out();
    assert!(text.contains("n=1"), "missing n=1:\n{text}");
    assert!(text.contains("n=2"), "missing n=2:\n{text}");
    assert!(text.contains("n=3"), "missing n=3:\n{text}");
}

#[tokio::test]
async fn regression_explicit_split_still_works() {
    // Users who already wrote `for x in $(split "$VAR")` should not see
    // a change in semantics or count.
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(
            r#"
            N=0
            for x in $(split "alpha beta gamma"); do
                N=$((N + 1))
                echo "x=$x"
            done
            echo "count=$N"
            "#,
        )
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    let text = result.text_out();
    assert!(text.contains("x=alpha"), "missing alpha:\n{text}");
    assert!(text.contains("x=beta"), "missing beta:\n{text}");
    assert!(text.contains("x=gamma"), "missing gamma:\n{text}");
    assert!(text.contains("count=3"), "split should still produce 3:\n{text}");
}
