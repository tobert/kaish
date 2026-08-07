//! Comma is significant only inside a `[...]`/`{...}` literal or pattern —
//! kernel-execute-level (end-to-end lex → parse → validate → dispatch)
//! coverage for the constructs that don't fit `bareword_comma_tests.rs`,
//! `builtin_fidelity_tests.rs`, or `sort_key_tests.rs`: `${...}`/`$(...)`
//! interpolation, quoted strings, heredoc bodies, `$((...))` arithmetic, and
//! real-filesystem glob brace expansion.
//!
//! Precise token-shape assertions (bracket nesting, stray-bracket depth
//! reset, the mechanism itself) live in `lexer.rs`'s own `#[cfg(test)]`
//! module, next to `run_has_bare_comma` / `compute_bracket_depth`. Nested
//! list/record literal EXECUTION (not just lexing) is already covered
//! extensively by `collection_literals_tests.rs` (`list_literal_commas_optional`,
//! `record_literal_multiline_with_trailing_comma`, etc.) — this file doesn't
//! duplicate that.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
// `common::kernel_at` (used by the real-filesystem glob-brace tests below)
// is `localfs`-gated; gate the whole file rather than split the `common`
// import (matches `bareword_comma_tests.rs`'s convention).
#![cfg(feature = "localfs")]

mod common;

use std::sync::Arc;

use common::{kernel_at, run};
use kaish_kernel::{Kernel, KernelConfig};

/// In-memory kernel for tests that don't touch a real filesystem.
async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated().with_skip_validation(true))
        .expect("failed to create kernel")
        .into_arc()
}

// ── `${...}` / `$(...)` interpolation ───────────────────────────────────────

#[tokio::test]
async fn comma_survives_var_ref_default_value() {
    // `${X:-1,3}` lexes as ONE `VarRef` token (balanced-brace scan) — the
    // comma inside never reaches the fusion passes as a separate token.
    let k = setup().await;
    let r = k.execute("echo ${UNSET_XYZ:-1,3}").await.expect("ok");
    assert_eq!(r.text_out(), "1,3\n");
}

#[tokio::test]
async fn comma_survives_simple_var_interpolated_into_word() {
    let k = setup().await;
    let r = k.execute(r#"X="1,3"; echo "field=$X""#).await.expect("ok");
    assert_eq!(r.text_out(), "field=1,3\n");
}

#[tokio::test]
async fn comma_bearing_cmd_subst_result_prints_whole() {
    let k = setup().await;
    let r = k.execute("echo $(echo 1,3)").await.expect("ok");
    assert_eq!(r.text_out(), "1,3\n");
}

#[tokio::test]
async fn comma_inside_cmd_subst_body_is_one_word() {
    // The comma is INSIDE the substitution's own argv, not just its
    // result — `sed -n 1,3p` inside `$(...)` gets the same bracket-depth
    // treatment as top-level source (see `lexer::tests::
    // comma_inside_cmd_subst_folds_like_top_level` for the token shape).
    let k = setup().await;
    let r = k
        .execute(r#"printf "%s\n" "$(printf 'a\nb\nc\n' | sed -n 1,2p)""#)
        .await
        .expect("ok");
    assert_eq!(r.text_out(), "a\nb\n");
}

// ── Quoted strings ───────────────────────────────────────────────────────────

#[tokio::test]
async fn comma_in_double_quoted_string_is_unaffected() {
    let k = setup().await;
    let r = k.execute(r#"echo "a,b,c""#).await.expect("ok");
    assert_eq!(r.text_out(), "a,b,c\n");
}

#[tokio::test]
async fn comma_in_single_quoted_string_is_unaffected() {
    let k = setup().await;
    let r = k.execute("echo 'a,b,c'").await.expect("ok");
    assert_eq!(r.text_out(), "a,b,c\n");
}

// ── Heredoc bodies ───────────────────────────────────────────────────────────

#[tokio::test]
async fn comma_in_heredoc_body_is_unaffected() {
    // Heredoc bodies are extracted by the scanner BEFORE logos runs at all
    // (see lexer.rs module doc, "Pipeline") — a comma inside never becomes
    // `Token::Comma`, so this fix has no surface here at all. Pinned anyway
    // since the task explicitly calls it out as a risk to check.
    let k = setup().await;
    let r = k.execute("cat <<EOF\n1,3p\na,b,c\nEOF").await.expect("ok");
    assert_eq!(r.text_out(), "1,3p\na,b,c\n");
}

// ── `$((...))` arithmetic ───────────────────────────────────────────────────

#[tokio::test]
async fn arithmetic_expansion_unaffected_by_adjacent_comma_word() {
    // `$((...))` is extracted by the same pre-logos scanner as heredocs
    // (see lexer.rs module doc) — the preprocessor is context-unaware, so
    // this checks the two rewrites (arithmetic extraction, comma folding)
    // don't step on each other when a comma-bearing bareword sits right
    // next to an arithmetic expansion.
    let k = setup().await;
    let r = k.execute("echo $((1+2)),3").await;
    // `$((1+2))` evaluates to "3"; the result is glued to the literal
    // ",3" that follows — same no-token-pasting contract as any other
    // expansion glued to text (see `non_comma_glued_pasting_still_errors`
    // below for the general case). This one happens to route through the
    // Arithmetic expr rather than a bareword fold, so it's worth pinning
    // that it errors the SAME way, not a comma-shaped way.
    assert!(r.is_err(), "adjacent words with no space must still be rejected");
}

#[tokio::test]
async fn arithmetic_expansion_alone_is_unaffected() {
    let k = setup().await;
    let r = k.execute("echo $((1+2))").await.expect("ok");
    assert_eq!(r.text_out(), "3\n");
}

#[tokio::test]
async fn quoted_comma_field_list_next_to_arithmetic_still_works() {
    // A comma-bearing bareword argument in the SAME command as an
    // arithmetic expansion (not glued to it) is unaffected by either
    // rewrite.
    let k = setup().await;
    let r = k
        .execute(r#"echo $((1+2)) $(echo 'a:b:c' | cut -d: -f 1,3)"#)
        .await
        .expect("ok");
    // `$(cmd)` carries structured data (one array element per output line),
    // so the single-line `cut` result renders as a one-element JSON array
    // when interpolated bare — unrelated to this fix; `a:c` is the
    // comma-fixed field-range result underneath the `["..."]` wrapping.
    assert_eq!(r.text_out(), "3 [\"a:c\"]\n");
}

// ── Glob brace expansion against a real filesystem ──────────────────────────

#[tokio::test]
async fn glob_brace_expansion_with_star_still_expands() {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(tmp.path().join("a.js"), b"").unwrap();
    std::fs::write(tmp.path().join("b.ts"), b"").unwrap();
    std::fs::write(tmp.path().join("c.txt"), b"").unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "ls *.{js,ts}").await;
    assert_eq!(code, 0, "got: {out}");
    assert!(out.contains("a.js"), "got: {out}");
    assert!(out.contains("b.ts"), "got: {out}");
    assert!(!out.contains("c.txt"), "got: {out}");
}

#[tokio::test]
async fn brace_glob_inside_a_larger_word_still_expands() {
    // The brace sits mid-word, glued to a path prefix — the whole glued
    // run (containing `*`) still fuses to one `GlobWord` for the glob
    // engine, same as `ls *.{js,ts}` above.
    let tmp = tempfile::tempdir().unwrap();
    std::fs::create_dir(tmp.path().join("src")).unwrap();
    std::fs::write(tmp.path().join("src/lib.rs"), b"").unwrap();
    std::fs::write(tmp.path().join("src/Cargo.toml"), b"").unwrap();
    std::fs::write(tmp.path().join("src/notes.txt"), b"").unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "ls src/*.{rs,toml}").await;
    assert_eq!(code, 0, "got: {out}");
    assert!(out.contains("lib.rs"), "got: {out}");
    assert!(out.contains("Cargo.toml"), "got: {out}");
    assert!(!out.contains("notes.txt"), "got: {out}");
}

// ── The non-comma half of the no-token-pasting guard (GH #189) ─────────────

#[tokio::test]
async fn non_comma_glued_pasting_still_errors() {
    let k = setup().await;
    let result = k.execute(r#"echo /tmp/$(echo x).txt"#).await;
    assert!(result.is_err(), "unquoted interpolation glued to text must still error");
    let msg = format!("{:#}", result.unwrap_err());
    assert!(msg.contains("quote"), "should hint to quote: {msg}");
    assert!(!msg.contains("comma"), "not a comma error: {msg}");
}

#[tokio::test]
async fn non_comma_glued_flag_pasting_still_errors() {
    let k = setup().await;
    let result = k.execute("echo --flag$(echo x)").await;
    assert!(result.is_err(), "a flag glued to a substitution must still error");
    let msg = format!("{:#}", result.unwrap_err());
    assert!(msg.contains("quote"), "should hint to quote: {msg}");
}
