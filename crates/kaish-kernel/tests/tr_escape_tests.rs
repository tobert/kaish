//! Kernel-routed tests for `tr` C-style escape interpretation.
//!
//! GNU tr interprets `\n`, `\t`, `\r`, `\\`, `\f`, `\v`, `\a`, `\b`, and
//! octal `\NNN` in SET1 and SET2. Before this fix, kaish `tr` passed the
//! backslash-letter literally, so `tr '\n' ' '` treated SET1 as the two
//! characters `\` and `n` rather than newline — a ubiquitous idiom that
//! silently produced wrong output.
//!
//! Tests are routed through `kernel.execute()` so the full dispatch chain
//! (lex → parse → validate → glob pre-expansion → dispatch → builtin) runs.
//! Stdin is provided via a here-string (`<<<`) which is safe to use here
//! because it does not involve printf escape handling (a separate known bug).

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::Kernel;

// ---------------------------------------------------------------------------
// \n — the ubiquitous idiom
// ---------------------------------------------------------------------------

/// `tr '\n' ' '` should join lines into one space-separated string.
/// Before the fix this treated SET1 as literal backslash + n, leaving
/// newlines untouched.
#[tokio::test]
async fn tr_escape_newline_to_space() {
    let kernel = Kernel::transient().unwrap();
    // Use a here-string to feed multi-line input without relying on printf escapes.
    let result = kernel
        .execute(r#"printf 'line1\nline2\nline3' | tr '\n' ' '"#)
        .await
        .unwrap();
    assert!(result.ok(), "tr should succeed; err={}", result.err);
    // Newlines should be replaced with spaces.
    let out = result.text_out();
    assert!(
        !out.contains('\n'),
        "tr '\\n' ' ' left newlines in output: {out:?}"
    );
    assert!(
        out.contains("line1 line2 line3"),
        "expected 'line1 line2 line3', got: {out:?}"
    );
}

/// `tr ' ' '\n'` should split on spaces, placing each word on its own line.
#[tokio::test]
async fn tr_escape_space_to_newline() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(r#"printf 'a b c' | tr ' ' '\n'"#)
        .await
        .unwrap();
    assert!(result.ok(), "tr should succeed; err={}", result.err);
    let out = result.text_out();
    // Each word should be on its own line.
    assert!(
        out.contains("a\nb\nc"),
        "expected 'a\\nb\\nc', got: {out:?}"
    );
}

// ---------------------------------------------------------------------------
// \t — tab
// ---------------------------------------------------------------------------

/// `tr '\t' ' '` should replace tab characters with spaces.
#[tokio::test]
async fn tr_escape_tab_to_space() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(r#"printf 'a\tb\tc' | tr '\t' ' '"#)
        .await
        .unwrap();
    assert!(result.ok(), "tr should succeed; err={}", result.err);
    let out = result.text_out();
    assert!(
        !out.contains('\t'),
        "tr '\\t' ' ' left tabs in output: {out:?}"
    );
    assert!(
        out.contains("a b c"),
        "expected 'a b c', got: {out:?}"
    );
}

// ---------------------------------------------------------------------------
// \\ — literal backslash
// ---------------------------------------------------------------------------

/// `tr '\\' '/'` should replace literal backslash with forward slash.
#[tokio::test]
async fn tr_escape_backslash_to_slash() {
    let kernel = Kernel::transient().unwrap();
    // Use printf to inject a literal backslash in the stream.
    let result = kernel
        .execute(r#"printf 'a\\b\\c' | tr '\\' '/'"#)
        .await
        .unwrap();
    assert!(result.ok(), "tr should succeed; err={}", result.err);
    let out = result.text_out();
    assert!(
        out.contains("a/b/c"),
        "expected 'a/b/c', got: {out:?}"
    );
}

// ---------------------------------------------------------------------------
// Octal \NNN
// ---------------------------------------------------------------------------

/// `tr '\012' ' '` — octal 012 is newline (0o12 == 10 == '\n').
/// This covers the octal escape path.
#[tokio::test]
async fn tr_escape_octal_newline_to_space() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(r#"printf 'x\ny\nz' | tr '\012' ' '"#)
        .await
        .unwrap();
    assert!(result.ok(), "tr should succeed; err={}", result.err);
    let out = result.text_out();
    assert!(
        !out.contains('\n'),
        "tr '\\012' ' ' left newlines in output: {out:?}"
    );
    assert!(
        out.contains("x y z"),
        "expected 'x y z', got: {out:?}"
    );
}

// ---------------------------------------------------------------------------
// Escape in SET2 (not just SET1)
// ---------------------------------------------------------------------------

/// SET2 escapes must also be interpreted: `tr ' ' '\t'` should produce tabs.
#[tokio::test]
async fn tr_escape_in_set2() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(r#"printf 'a b c' | tr ' ' '\t'"#)
        .await
        .unwrap();
    assert!(result.ok(), "tr should succeed; err={}", result.err);
    let out = result.text_out();
    assert!(
        out.contains("a\tb\tc"),
        "expected 'a\\tb\\tc', got: {out:?}"
    );
}

// ---------------------------------------------------------------------------
// Existing behaviour — ranges must not regress
// ---------------------------------------------------------------------------

/// `tr 'a-z' 'A-Z'` must still work (ranges are not affected by escape fix).
#[tokio::test]
async fn tr_range_still_works_after_escape_fix() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(r#"printf 'hello' | tr 'a-z' 'A-Z'"#)
        .await
        .unwrap();
    assert!(result.ok(), "tr should succeed; err={}", result.err);
    assert_eq!(
        result.text_out().trim(),
        "HELLO",
        "range translation regressed"
    );
}

/// `tr -d '[:digit:]'` character class deletion must not regress.
#[tokio::test]
async fn tr_char_class_still_works_after_escape_fix() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute(r#"printf 'abc123' | tr -d '[:digit:]'"#)
        .await
        .unwrap();
    assert!(result.ok(), "tr should succeed; err={}", result.err);
    assert_eq!(
        result.text_out().trim(),
        "abc",
        "character-class deletion regressed"
    );
}
