//! Two more instances of the same defect `quoted_cmdsubst_error_tests.rs`
//! covers for double-quoted strings: a malformed or unterminated `$(...)`
//! must be a loud parse error, never silently kept as literal text.
//!
//! - An unquoted heredoc body (`parse_interpolated_string_spanned`) had the
//!   same two silent fallbacks (unterminated `$(`, and a body that fails to
//!   parse) with no error in either case.
//! - The `${VAR:-WORD}` default word (`parse_var_expr`) discarded a real
//!   error from `parse_interpolated_string` and kept the default literal.
//!
//! A quoted-delimiter heredoc (`<<'EOF'`) is never expanded at all — its body
//! must stay completely untouched, so it gets its own control test.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated().with_skip_validation(true))
        .expect("failed to create kernel")
        .into_arc()
}

// ============================================================================
// Sibling 1: unquoted heredoc bodies
// ============================================================================

// The reported repro: an unquoted heredoc body with an unterminated `$(`
// used to be accepted verbatim (body carried through with the `$(` left as
// literal text) and exit 0. It must now be a loud parse error naming the
// missing `)`, matching the quoted and unquoted top-level forms.
#[tokio::test]
async fn unterminated_cmdsubst_in_heredoc_is_loud() {
    let k = setup().await;
    let err = k
        .execute("cat <<EOF\npre $(echo hi\nEOF\n")
        .await
        .expect_err("unterminated $( in an unquoted heredoc body must be a loud parse error")
        .to_string();
    assert!(
        err.contains("unterminated command substitution") && err.contains(')'),
        "expected an unterminated-command-substitution message naming the missing `)`, got: {err}"
    );
}

// A malformed but *terminated* `$(...)` body in a heredoc must also be loud
// — pins the second silent fallback (a `parse()` failure inside the body
// used to fall back to keeping the whole thing literal).
#[tokio::test]
async fn malformed_cmdsubst_in_heredoc_is_loud() {
    let k = setup().await;
    let res = k.execute("cat <<EOF\npre $(if true; echo 1; fi)\nEOF\n").await;
    assert!(
        res.is_err(),
        "malformed $(...) in an unquoted heredoc body must be a loud parse error, got: {res:?}"
    );
}

// Control: a well-formed `$(...)` in an unquoted heredoc body still runs.
#[tokio::test]
async fn wellformed_cmdsubst_in_heredoc_runs() {
    let k = setup().await;
    let r = k
        .execute("cat <<EOF\npre $(echo hi) post\nEOF\n")
        .await
        .expect("execute");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out(), "pre hi post\n");
}

// Control: a `$(...)` that closes on a LATER line of the body still runs —
// the scan must not mistake the body's own newlines for the end of input.
#[tokio::test]
async fn multiline_cmdsubst_in_heredoc_runs() {
    let k = setup().await;
    let r = k
        .execute("cat <<EOF\npre $(echo one\necho two\n) post\nEOF\n")
        .await
        .expect("execute");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out(), "pre one\ntwo post\n");
}

// Nested command substitution inside a heredoc body's own `$(...)` still
// works — the scan must not stop at the first inner `)`.
#[tokio::test]
async fn nested_cmdsubst_in_heredoc_runs() {
    let k = setup().await;
    let r = k
        .execute("cat <<EOF\nval: $(echo $(echo inner))\nEOF\n")
        .await
        .expect("execute");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out(), "val: inner\n");
}

// CRITICAL: a quoted delimiter (`<<'EOF'`) is never interpolated at all — its
// body stays completely literal, including an unterminated-looking `$(`.
// This must NOT error; it would break real scripts that pass a `$(` through
// literal heredocs on purpose (e.g. embedding kaish source as data).
#[tokio::test]
async fn quoted_delimiter_heredoc_with_unterminated_looking_paren_stays_literal() {
    let k = setup().await;
    let r = k
        .execute("cat <<'EOF'\npre $(echo hi\nEOF\n")
        .await
        .expect("a literal heredoc body must never be parsed for interpolation");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out(), "pre $(echo hi\n");
}

// ============================================================================
// Sibling 2: the `${VAR:-WORD}` default word
// ============================================================================

// The reported repro: `${x:-$(echo hi}` (missing `)`) used to keep the
// default word as literal text and exit 0.
#[tokio::test]
async fn unterminated_cmdsubst_in_default_word_is_loud() {
    let k = setup().await;
    let err = k
        .execute("echo ${x:-$(echo hi}")
        .await
        .expect_err("unterminated $( in a ${VAR:-WORD} default must be a loud parse error")
        .to_string();
    assert!(
        err.contains("unterminated command substitution") && err.contains(')'),
        "expected an unterminated-command-substitution message naming the missing `)`, got: {err}"
    );
}

// A malformed but terminated `$(...)` in the default word must also be loud.
#[tokio::test]
async fn malformed_cmdsubst_in_default_word_is_loud() {
    let k = setup().await;
    let res = k.execute("echo ${x:-$(if true; echo 1; fi)}").await;
    assert!(
        res.is_err(),
        "malformed $(...) in a ${{VAR:-WORD}} default must be a loud parse error, got: {res:?}"
    );
}

// Control: an ordinary literal default word still works.
#[tokio::test]
async fn literal_default_word_runs() {
    let k = setup().await;
    let r = k.execute("echo ${x:-ok}").await.expect("execute");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out().trim(), "ok");
}

// Control: a well-formed `$(...)` default word still runs and expands.
#[tokio::test]
async fn wellformed_cmdsubst_default_word_runs() {
    let k = setup().await;
    let r = k.execute("echo ${x:-$(echo hi)}").await.expect("execute");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out().trim(), "hi");
}

// Control: a plain `${x}` (no default at all) is unaffected.
#[tokio::test]
async fn plain_varref_runs() {
    let k = setup().await;
    let r = k.execute("x=set; echo ${x}").await.expect("execute");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out().trim(), "set");
}

// Bonus coverage: the same default-word swallow, one level down inside an
// unquoted heredoc body's own `${...}` interpolation.
#[tokio::test]
async fn unterminated_cmdsubst_in_heredoc_default_word_is_loud() {
    let k = setup().await;
    let res = k.execute("cat <<EOF\n${x:-$(echo hi}\nEOF\n").await;
    assert!(
        res.is_err(),
        "unterminated $( in a heredoc-body default word must be a loud parse error, got: {res:?}"
    );
}

// The default-word check lives in `var_expr_parser`'s grammar rule, not a
// pre-parse scan gated to the top level — pin that a malformed default word
// nested inside an unquoted `$(...)` is caught too, not just at top level.
#[tokio::test]
async fn unterminated_cmdsubst_in_nested_default_word_is_loud() {
    let k = setup().await;
    let res = k.execute("echo $(echo ${x:-$(echo hi})").await;
    assert!(
        res.is_err(),
        "unterminated $( in a default word nested inside $(...) must be a loud parse error, got: {res:?}"
    );
}
