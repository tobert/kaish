//! Kaish-specific heredoc contracts that don't translate to bash.
//!
//! Bash-portable heredoc behavior (literal/interpolated bodies, `<<-EOF`
//! tab stripping, `$((..))` expansion, line continuation, escapes,
//! delimiter matching, nesting inside if/for/functions) lives in
//! `heredoc_compat_tests.rs` and runs through both kaish and `bash -c`
//! when `KAISH_BASH_COMPAT=1` is set.
//!
//! This file keeps:
//! - **CRLF normalization** in heredoc bodies. Kaish normalizes CR/LF
//!   line endings; bash preserves `\r` and rejects `EOF\r` as a non-match
//!   for the `EOF` delimiter, so the heredoc never terminates under bash.
//! - **Unterminated heredoc contract**. Kaish uses whatever was collected
//!   up to EOF; bash warns to stderr and appends a trailing newline.
//! - Two pre-existing `#[ignore]`d tests for nested command substitution
//!   inside heredoc bodies — blocked on async redirect-target evaluation.

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

/// Construct a fresh in-memory kernel with validation skipped so tests can
/// drive arbitrary scripts without tripping the validator on synthetic vars.
async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated().with_skip_validation(true))
        .expect("failed to create kernel")
        .into_arc()
}

// ============================================================================
// Line-ending handling — hybrid: CR-tolerant delimiter, verbatim body
// ============================================================================
//
// Diverges slightly from bash. Bash preserves `\r` in the body AND requires a
// literal `EOF` line for the delimiter — so a CRLF-saved script with `EOF\r`
// silently swallows the rest of the file. Kaish treats the line ending as
// purely a line terminator for delimiter matching (so `EOF\r` still matches
// `EOF`) while preserving the original bytes in body content (so the user's
// input is honored verbatim).

#[tokio::test]
async fn crlf_body_preserves_carriage_returns() {
    // CRLF inside the body is preserved byte-for-byte.
    let k = setup().await;
    let r = k.execute("cat <<EOF\r\nhello\r\nEOF").await.expect("ok");
    assert_eq!(r.text_out(), "hello\r\n");
}

#[tokio::test]
async fn bare_cr_body_preserves_carriage_returns() {
    // Bare `\r` line endings (Mac classic) are also preserved verbatim.
    let k = setup().await;
    let r = k.execute("cat <<EOF\rhello\rEOF").await.expect("ok");
    assert_eq!(r.text_out(), "hello\r");
}

#[tokio::test]
async fn crlf_terminated_delimiter_still_matches() {
    // A delimiter line ending with `\r\n` (or bare `\r`) must still match
    // the bare `EOF` delimiter — otherwise CRLF-saved scripts break.
    let k = setup().await;
    let r = k.execute("cat <<EOF\nhello\nEOF\r\n").await.expect("ok");
    assert_eq!(r.text_out(), "hello\n");
}

#[tokio::test]
async fn bare_cr_terminated_delimiter_still_matches() {
    let k = setup().await;
    let r = k.execute("cat <<EOF\nhello\nEOF\r").await.expect("ok");
    assert_eq!(r.text_out(), "hello\n");
}

// ============================================================================
// Unterminated heredoc — error rather than silent truncation
// ============================================================================
//
// Previously, kaish silently used whatever was collected when EOF arrived
// before the closing delimiter. That masked the much-more-likely case where
// the user's input got truncated (paste cut off, missing closing line, etc.)
// — silent fallback on missing data is exactly the failure mode we want to
// surface, not paper over.

#[tokio::test]
async fn unterminated_heredoc_errors() {
    let k = setup().await;
    let err = k.execute("cat <<EOF\nhello").await.expect_err(
        "unterminated heredoc must surface as an error, not silent truncation",
    );
    let msg = err.to_string();
    assert!(
        msg.contains("unterminated heredoc"),
        "error should mention unterminated heredoc; got: {msg}",
    );
    assert!(
        msg.contains("EOF"),
        "error should name the expected delimiter; got: {msg}",
    );
}

#[tokio::test]
async fn unterminated_heredoc_with_dash_form_errors() {
    let k = setup().await;
    let err = k.execute("cat <<-DONE\n\thi").await.expect_err("error expected");
    let msg = err.to_string();
    assert!(msg.contains("unterminated heredoc"), "got: {msg}");
    assert!(msg.contains("DONE"), "got: {msg}");
}

// ============================================================================
// Nested command substitution inside an interpolated heredoc body.
//
// **Known limitation (pre-existing, not heredoc-specific)**: redirect
// target evaluation in `scheduler::pipeline::setup_stdin_redirects` goes
// through the synchronous `eval_simple_expr`, which skips
// `StringPart::CommandSubst` because it cannot execute pipelines. The
// same limitation affects regular stdin redirects like `cmd < $(echo x)`.
// Making this async requires making `setup_stdin_redirects` async and
// threading a dispatcher through — out of scope for the heredoc work.
//
// Ignored until that refactor lands; both tests assert the *desired*
// behaviour (bash-compatible). When the redirect path becomes async,
// remove the `#[ignore]` and these should pass.
// ============================================================================

#[tokio::test]
#[ignore = "blocked on async redirect-target evaluation (see module comment)"]
async fn nested_command_substitution_in_body() {
    let k = setup().await;
    let r = k
        .execute("cat <<EOF\nvia $(echo nested)\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "via nested\n");
}

#[tokio::test]
#[ignore = "blocked on async redirect-target evaluation (see module comment)"]
async fn nested_pipeline_command_substitution_in_body() {
    let k = setup().await;
    let r = k
        .execute("cat <<EOF\n$(echo -n HELLO | tr A-Z a-z) world\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "hello world\n");
}
