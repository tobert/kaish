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
/// not `reject_glued_args`) with its own, differently-worded message. A
/// redirect target's tokens (`$DIR` then `/out.txt`, zero gap) look exactly
/// like an argv paste at the token level, so this is the case that first
/// showed the rescan could steal another guard's diagnosis.
///
/// It is guarded structurally now rather than by a carve-out: the redirect
/// guard's `Rich::custom` is the message left standing, so
/// `is_glued_args_error` never lets the rescan run here at all. This test
/// fails if that inversion regresses.
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

/// Every purpose-built parse diagnosis in the grammar, checked to survive
/// the glued-argv rescan intact.
///
/// The rescan in `parse_tokens` re-reports the span of a paste rejection
/// the grammar already made. It must never replace a DIFFERENT diagnosis:
/// those name a different mistake and show its own fix, and the paste
/// advice in their place is both wrong and less useful. `x={msg: hello
/// world}` is the case that proved this — the parser has a guard built for
/// GH #183 that names the unquoted multi-word record value, and an earlier
/// cut of the rescan overwrote it with "quote the whole word" when nothing
/// was being pasted at all.
///
/// Each row pairs an input with a fragment only its own guard produces.
/// The assertion that matters is the negative one: none of them may come
/// back as the paste message.
#[test]
fn purpose_built_diagnoses_are_never_replaced_by_the_paste_message() {
    const PASTE: &str = "adjacent words with no space between them are not joined into one";
    let cases = [
        ("cat > $DIR/out.txt", "redirect target"),
        ("x={msg: hello world}", "record value: unexpected word"),
        ("echo ${x:1:2}", "kaish slices with brackets"),
        ("echo $(foo", "unterminated command substitution"),
        (r#"x={"$(": 1}"#, "unterminated command substitution"),
        (r#"echo "$(""#, "unterminated command substitution"),
        ("echo ${x:-$(}", "unterminated command substitution"),
    ];
    for (source, expected) in cases {
        let errors = parse(source).expect_err("must be a parse error");
        let message = &errors[0].message;
        assert!(
            !message.contains(PASTE),
            "the glued-argv rescan stole this guard's diagnosis for {source:?}: {message}"
        );
        assert!(
            message.contains(expected),
            "expected {expected:?} in the error for {source:?}, got: {message}"
        );
    }
}

/// Canary: the number of `Rich::custom` guards in the parser.
///
/// A tripped assertion here is not a bug. It means someone added (or
/// removed) a purpose-built parse diagnosis, and the glued-argv rescan in
/// `parse_tokens` is the thing that could silently swallow a new one.
///
/// If you added a guard: check that its message survives, by adding an
/// input that triggers it to
/// `purpose_built_diagnoses_are_never_replaced_by_the_paste_message` above,
/// then update this count. `is_glued_args_error` should already protect it
/// — it only lets the rescan run when the standing message is the paste
/// message itself — so the new case is expected to pass on the first try.
/// If it does NOT, the inversion has a hole and that is a real bug: fix it
/// there, never by adding a construct for the rescan to skip.
///
/// If you removed one: drop its row above and update the count.
#[test]
fn parser_custom_guard_count_is_pinned() {
    const PARSER_SOURCE: &str = include_str!("../src/parser.rs");
    const EXPECTED: usize = 12;
    let found = PARSER_SOURCE.matches("Rich::custom(").count();
    assert_eq!(
        found, EXPECTED,
        "parser.rs has {found} `Rich::custom` guards, pinned at {EXPECTED} — \
         read this test's doc comment before changing the number"
    );
}
