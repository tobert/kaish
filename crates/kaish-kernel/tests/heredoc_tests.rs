//! End-to-end heredoc tests covering tab stripping, interpolation,
//! pipeline interaction, and edge cases through the actual kernel.
//!
//! Companion to the in-source lexer tests (`mod tests` in lexer.rs) which
//! cover token shape only. Tests here drive the full lex → parse → validate
//! → execute path so behavioral changes anywhere in the pipeline surface.
//!
//! Designed alongside the heredoc work tracked in
//! `~/.claude/plans/let-s-plan-make-heredocs-precious-puzzle.md`.

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
// Literal vs interpolated bodies
// ============================================================================

#[tokio::test]
async fn literal_body_no_interpolation() {
    let k = setup().await;
    // Single-quoted delimiter → literal body, no expansion
    let r = k.execute("cat <<'EOF'\n$NOT_A_VAR\nEOF").await.expect("ok");
    assert_eq!(r.text_out(), "$NOT_A_VAR\n");
}

#[tokio::test]
async fn double_quoted_delimiter_is_literal() {
    let k = setup().await;
    let r = k.execute("cat <<\"EOF\"\n$NOT_A_VAR\nEOF").await.expect("ok");
    assert_eq!(r.text_out(), "$NOT_A_VAR\n");
}

#[tokio::test]
async fn interpolated_body_var_substitution() {
    let k = setup().await;
    let r = k
        .execute("X=hello\ncat <<EOF\n${X}\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "hello\n");
}

#[tokio::test]
async fn interpolated_body_simple_var() {
    let k = setup().await;
    let r = k
        .execute("X=world\ncat <<EOF\nhello $X\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "hello world\n");
}

#[tokio::test]
async fn empty_body() {
    let k = setup().await;
    let r = k.execute("cat <<EOF\nEOF").await.expect("ok");
    assert_eq!(r.text_out(), "");
}

// ============================================================================
// `<<-EOF` indented form: leading tabs must be stripped per POSIX
// ============================================================================

#[tokio::test]
async fn dash_form_strips_leading_tabs() {
    // Pre-fix: tabs were preserved in body content. Post-fix: stripped.
    let k = setup().await;
    let r = k
        .execute("cat <<-EOF\n\thello\n\tworld\n\tEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "hello\nworld\n");
}

#[tokio::test]
async fn dash_form_does_not_strip_leading_spaces() {
    // POSIX strips tabs only — spaces remain.
    let k = setup().await;
    let r = k.execute("cat <<-EOF\n   spaced\n\tEOF").await.expect("ok");
    assert_eq!(r.text_out(), "   spaced\n");
}

#[tokio::test]
async fn dash_form_with_interpolation_strips_tabs() {
    let k = setup().await;
    let r = k
        .execute("X=value\ncat <<-EOF\n\t${X}\n\tEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "value\n");
}

#[tokio::test]
async fn dash_form_literal_delimiter_strips_tabs() {
    let k = setup().await;
    let r = k.execute("cat <<-'EOF'\n\thi\n\tEOF").await.expect("ok");
    assert_eq!(r.text_out(), "hi\n");
}

#[tokio::test]
async fn dash_form_strips_multiple_leading_tabs() {
    let k = setup().await;
    let r = k
        .execute("cat <<-EOF\n\t\t\tdeep\n\tEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "deep\n");
}

#[tokio::test]
async fn dash_form_preserves_internal_tabs() {
    // Only LEADING tabs are stripped. Tabs in the middle of a line stay.
    let k = setup().await;
    let r = k
        .execute("cat <<-EOF\n\thello\tworld\n\tEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "hello\tworld\n");
}

// ============================================================================
// Pipeline interaction
// ============================================================================

#[tokio::test]
async fn trailing_text_after_delimiter_pipes() {
    // `cat <<EOF | tr a-z A-Z\nfoo\nEOF` — the heredoc must wire to cat,
    // and the pipe to tr must still work (regression for the after-delimiter
    // buffering in preprocess_heredocs).
    let k = setup().await;
    let r = k
        .execute("cat <<EOF | tr a-z A-Z\nfoo\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "FOO\n");
}

#[tokio::test]
async fn heredoc_body_with_arithmetic_expansion() {
    // `$((1+2))` is preprocessed before heredocs but the expansion should
    // still appear in the materialized body.
    let k = setup().await;
    let r = k
        .execute("cat <<EOF\nresult: $((1+2))\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "result: 3\n");
}

// ============================================================================
// Line ending edge cases
// ============================================================================

#[tokio::test]
async fn crlf_body_works() {
    // Windows line endings inside the body should be normalised.
    let k = setup().await;
    let r = k.execute("cat <<EOF\r\nhello\r\nEOF").await.expect("ok");
    assert_eq!(r.text_out(), "hello\n");
}

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
// Multi-line bodies
// ============================================================================

#[tokio::test]
async fn multi_line_body_preserves_newlines() {
    let k = setup().await;
    let r = k
        .execute("cat <<EOF\nline1\nline2\nline3\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "line1\nline2\nline3\n");
}

#[tokio::test]
async fn body_with_blank_line_in_middle() {
    let k = setup().await;
    let r = k
        .execute("cat <<EOF\nfirst\n\nthird\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "first\n\nthird\n");
}

#[tokio::test]
async fn preserves_multiple_trailing_newlines() {
    let k = setup().await;
    let r = k
        .execute("cat <<EOF\nhello\n\n\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "hello\n\n\n");
}

// ============================================================================
// Backslash escape processing inside unquoted heredocs (POSIX semantics)
//
// Per POSIX: in unquoted heredocs, `\$`, `\\`, and `\<newline>` are escape
// sequences. Other backslashes are retained literally. Quoted heredocs
// (`<<'EOF'`) do no escape processing.
//
// kaish preserves the body's trailing newline at preprocessing
// (lexer.rs); these tests therefore use
// exact `assert_eq!` WITH a trailing `\n` to match standard shell behavior.
// ============================================================================

#[tokio::test]
async fn dollar_escape_in_unquoted_heredoc_suppresses_expansion() {
    // POSIX: `\$X` in an unquoted heredoc body is a literal `$X` — no
    // expansion. Pre-fix kaish keeps the `\` AND expands `$X` to `\hello`.
    let k = setup().await;
    let r = k.execute("X=hello\ncat <<EOF\n\\$X\nEOF").await.expect("ok");
    assert_eq!(r.text_out(), "$X\n");
}

#[tokio::test]
async fn dollar_escape_in_quoted_heredoc_is_literal_backslash() {
    // Quoted heredoc: no escape processing. The `\` stays AND `$X` is not
    // expanded (no expansion in quoted heredocs).
    let k = setup().await;
    let r = k
        .execute("X=hello\ncat <<'EOF'\n\\$X\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "\\$X\n");
}

#[tokio::test]
async fn backslash_backslash_escape_collapses_to_one_backslash() {
    // POSIX: `\\` in an unquoted heredoc body becomes a single `\`.
    let k = setup().await;
    let r = k.execute("cat <<EOF\n\\\\\nEOF").await.expect("ok");
    assert_eq!(r.text_out(), "\\\n");
}

#[tokio::test]
async fn backslash_other_char_kept_literally() {
    // POSIX: `\x` (where x is not `$`, `\`, `\n`, or backtick) keeps the `\`.
    let k = setup().await;
    let r = k.execute("cat <<EOF\nfoo\\nbar\nEOF").await.expect("ok");
    // `\n` between literal letters is NOT an ANSI-C escape — keep `\n`
    // verbatim (two chars: backslash and the letter n).
    assert_eq!(r.text_out(), "foo\\nbar\n");
}

#[tokio::test]
async fn backslash_newline_is_line_continuation() {
    // POSIX: `\<newline>` joins lines — the `\` and `\n` are both removed.
    // Source: `<<EOF\nfoo\\\nbar\nEOF` → body lines "foo\" and "bar" with
    // the backslash signalling continuation, output should be "foobar".
    let k = setup().await;
    let r = k.execute("cat <<EOF\nfoo\\\nbar\nEOF").await.expect("ok");
    assert_eq!(r.text_out(), "foobar\n");
}

#[tokio::test]
async fn backslash_dollar_then_unrelated_var_only_escapes_first() {
    // Both `\$X` and `$Y` in the same line: only the escaped one stays
    // literal, the other expands.
    let k = setup().await;
    let r = k
        .execute("X=foo\nY=bar\ncat <<EOF\n\\$X $Y\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "$X bar\n");
}

// ============================================================================
// Heredocs nested inside control flow and functions — exercise the
// after-delimiter buffering against shell keywords like `done`, `fi`.
// ============================================================================

#[tokio::test]
async fn heredoc_inside_if_branch() {
    let k = setup().await;
    let r = k
        .execute("if true; then\ncat <<EOF\nin-then\nEOF\nfi")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "in-then\n");
}

#[tokio::test]
async fn heredoc_inside_for_loop_body() {
    let k = setup().await;
    // The heredoc should run once per iteration. seq produces 1..=2,
    // so we expect "pass 1" and "pass 2" separated by kaish's
    // statement-newline separator (see LANGUAGE.md on output model).
    let r = k
        .execute("for N in $(seq 1 2); do\ncat <<EOF\npass $N\nEOF\ndone")
        .await
        .expect("ok");
    // Output is "pass 1\npass 2\n"
    assert_eq!(r.text_out(), "pass 1\npass 2\n");
}

#[tokio::test]
async fn heredoc_inside_user_function() {
    let k = setup().await;
    let r = k
        .execute("greet() {\ncat <<EOF\nhello from $1\nEOF\n}\ngreet Amy")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "hello from Amy\n");
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

// ============================================================================
// Body oddities: content that LOOKS like kaish internal markers, or that
// contains the heredoc delimiter as a prefix on a longer line.
// ============================================================================

#[tokio::test]
async fn body_line_looking_like_delimiter_but_longer_is_not_delimiter() {
    // "EOFSUFFIX" is not equal to "EOF", so it should NOT terminate.
    let k = setup().await;
    let r = k
        .execute("cat <<EOF\nEOFSUFFIX\ntrue body\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "EOFSUFFIX\ntrue body\n");
}

#[tokio::test]
async fn body_containing_leading_whitespace_before_delimiter_in_plain_form() {
    // Without `<<-`, a delimiter line with leading whitespace is NOT a
    // terminator (strict equality after optional tab strip). `  EOF` stays
    // part of the body when plain `<<EOF` is used.
    let k = setup().await;
    let r = k
        .execute("cat <<EOF\nbody\n  EOF\nEOF")
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "body\n  EOF\n");
}
