//! Kaish-specific heredoc contracts that don't translate to bash.
//!
//! Bash-portable heredoc behavior (literal/interpolated bodies, `<<-EOF`
//! tab stripping, `$((..))` expansion, line continuation, escapes,
//! delimiter matching, nesting inside if/for/functions) lives in
//! `heredoc_compat_tests.rs` and runs through both kaish and `bash -c`
//! when `KAISH_BASH_COMPAT=1` is set.
//!
//! This file keeps:
//! - **Line-ending handling** (hybrid, diverges from bash): `\r` bytes in
//!   heredoc bodies are preserved verbatim, but a CRLF-terminated delimiter
//!   line (`EOF\r\n`) still matches `EOF` so CRLF-saved scripts terminate.
//!   Bash also preserves body `\r` but rejects `EOF\r` as a delimiter match.
//! - **Unterminated heredoc contract**. Kaish errors loudly ("unterminated
//!   heredoc"); bash warns to stderr and tolerates, appending a newline.
//! - **Nested command substitution in bodies** — `$(...)` runs inside
//!   heredoc bodies (these were `#[ignore]`d until async redirect-target
//!   evaluation landed).

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

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
// `<<-` tab stripping happens on the source, not on expanded values
// ============================================================================
//
// POSIX `<<-` strips leading tabs from each *source* line before parameter
// expansion. So a literal leading tab in the heredoc is removed, but a tab
// that arrives via a `$var` value (or `$(cmd)` output) at line start is
// preserved — bash agrees. The old interpreter path materialized the body and
// *then* ran `strip_leading_tabs`, eating tabs that came from a variable.

#[tokio::test]
async fn dash_heredoc_strips_source_tabs_not_var_tabs() {
    let k = setup().await;
    // X holds a leading tab; the heredoc's own leading tabs are literal.
    // Source line 1: <TAB>$X  -> strip the literal tab, keep X's tab.
    // Source line 2: <TAB>after -> strip the literal tab.
    let r = k
        .execute("X=\"\tval\"\ncat <<-EOF\n\t$X\n\tafter\nEOF")
        .await
        .expect("ok");
    assert_eq!(
        r.text_out(),
        "\tval\nafter\n",
        "`<<-` must strip only the literal source tab, not the tab from $X"
    );
}

#[tokio::test]
async fn dash_heredoc_var_at_line_start_terminates_strip_run() {
    let k = setup().await;
    // The source line starts with `$X` (no literal leading tab), so a tab that
    // follows on the same source line is mid-line and must be kept verbatim.
    let r = k
        .execute("X=hi\ncat <<-EOF\n$X\tkept\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "hi\tkept\n");
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
// Redirect target/heredoc-body evaluation routes through
// `CommandDispatcher::eval_expr` (see `scheduler::pipeline::eval_redirect_target`),
// so command substitution (`$(...)`) runs in heredoc bodies and stdin/output
// redirect targets. Fixed 2026-06-08.
// ============================================================================

#[tokio::test]
async fn nested_command_substitution_in_body() {
    let k = setup().await;
    let r = k
        .execute("cat <<EOF\nvia $(echo nested)\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "via nested\n");
}

#[tokio::test]
async fn nested_pipeline_command_substitution_in_body() {
    let k = setup().await;
    let r = k
        .execute("cat <<EOF\n$(echo -n HELLO | tr A-Z a-z) world\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "hello world\n");
}
