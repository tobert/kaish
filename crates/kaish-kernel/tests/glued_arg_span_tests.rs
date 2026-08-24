//! The glued-argv guard (`reject_glued_args` in `parser.rs`) used to point
//! its error span at an unrelated, earlier word instead of the word that
//! actually needs quoting.
//!
//! The cause was not the lexer's fragment spans — `tokenize()` slices each
//! fragment correctly (verified directly against `lexer::tokenize`). The
//! span was lost downstream: `reject_glued_args`'s `Rich::custom` error is
//! raised deep inside a `try_map` wrapping the whole argv `.repeated()`,
//! and chumsky's `choice`/`try_map` alt-error bookkeeping can merge it with
//! an unrelated, shallower failed alternative tried elsewhere in the
//! grammar for the same statement — `Rich::merge` always keeps the
//! shallower error's span, while `RichReason::flat_merge` always prefers
//! this function's `Custom` message, so the two get stitched together
//! wrong. `validate_glued_args` re-detects the same condition directly from
//! tokens, outside that machinery, the same way `validate_cmd_subst_bodies`
//! already does for a malformed `$(...)` body.
//!
//! Each case here asserts the reported span's own source text, not just
//! that an error occurred — a regression that keeps the right message but
//! the wrong span still fails these.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::parser::parse;

/// The text the argv-glue error's own span covers, after checking it is
/// that error and only that error.
fn glued_span_text(source: &str) -> &str {
    let errors = parse(source).expect_err("must be a parse error");
    assert_eq!(errors.len(), 1, "expected exactly one error: {errors:?}");
    let e = &errors[0];
    assert!(
        e.message.contains("adjacent words with no space between them"),
        "wrong error for {source:?}: {e:?}"
    );
    &source[e.span.start..e.span.end]
}

#[test]
fn colon_glued_path_names_the_whole_word() {
    // Used to point at `show` — an innocent, already-fine word.
    assert_eq!(
        glued_span_text("git show HEAD:training/v9/x.py"),
        "HEAD:training/v9/x.py"
    );
}

#[test]
fn colon_glued_short_path_names_the_whole_word() {
    // Used to point at `fetch`.
    assert_eq!(glued_span_text("git fetch origin a/b:c"), "a/b:c");
}

#[test]
fn word_assign_chain_names_the_whole_word() {
    // Used to point at `DBUS_ADDR` alone. A `key=value=value` chain is three
    // glue-candidate args in a row (`WordAssign`, then each `=`/value as its
    // own `Positional`); the span must cover the whole run, not just the
    // first adjacent pair that trips the check.
    assert_eq!(
        glued_span_text("export DBUS_ADDR=unix:path=/run/x"),
        "DBUS_ADDR=unix:path=/run/x"
    );
}

#[test]
fn doubled_test_operator_names_the_first_run() {
    // Used to point at the SECOND `===`'s `==` half.
    assert_eq!(glued_span_text("echo === step 3 ==="), "===");
}

/// A real, single-fragment paste keeps failing the same way (scope limit:
/// only the reported span moves, not what is accepted).
#[test]
fn cmd_subst_glued_to_suffix_still_rejected() {
    assert_eq!(
        glued_span_text(r#"echo /tmp/$(echo x).txt"#),
        r#"/tmp/$(echo x).txt"#
    );
}

/// The redirect-target glue guard is a separate check (`redirect_parser`,
/// not `reject_glued_args`) with its own, differently-worded message. It
/// already reported the right span before this fix and must not regress —
/// and `validate_glued_args`'s new fallback must not steal it, since a
/// redirect target's tokens (`$DIR` then `/out.txt`, zero gap) look just
/// like an argv paste at the token level.
#[test]
fn redirect_target_glue_keeps_its_own_span_and_message() {
    let source = "cat > $DIR/out.txt";
    let errors = parse(source).expect_err("must be a parse error");
    assert_eq!(errors.len(), 1, "expected exactly one error: {errors:?}");
    let e = &errors[0];
    assert!(
        e.message.contains("redirect target"),
        "must still be the redirect-specific message: {e:?}"
    );
    assert_eq!(&source[e.span.start..e.span.end], "/out.txt");
}
