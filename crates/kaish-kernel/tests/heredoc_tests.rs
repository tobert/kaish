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
// Line-ending normalization — kaish-specific (bash preserves \r)
// ============================================================================

#[tokio::test]
async fn crlf_body_works() {
    // Windows line endings inside the body should be normalised.
    let k = setup().await;
    let r = k.execute("cat <<EOF\r\nhello\r\nEOF").await.expect("ok");
    assert_eq!(r.text_out(), "hello\n");
}

// ============================================================================
// Unterminated heredoc contract — kaish-specific
// ============================================================================

#[tokio::test]
async fn unterminated_heredoc_uses_remaining_content() {
    // No closing EOF — current contract (lexer.rs:1397-1401) is to use
    // whatever was collected so far. Pin this so the contract doesn't
    // silently change.
    let k = setup().await;
    let r = k.execute("cat <<EOF\nhello").await.expect("ok");
    assert_eq!(r.text_out(), "hello");
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
