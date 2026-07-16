//! Characterization tests for the GH #95 lexer pipeline rewrite.
//!
//! Each section pins the SHOULD-BE behavior of a corruption-class bug the
//! pre-#95 four-stage pipeline had (two string-rewriting preprocessors →
//! logos → marker re-threading → three fusion passes, communicating
//! through in-band marker strings, span adjacency, and pass ordering).
//! The composed-scanner pipeline fixes these by construction; these tests
//! keep them dead.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::lexer::{tokenize, LexerError, Token};
use kaish_kernel::parser::parse;

fn toks(src: &str) -> Vec<Token> {
    tokenize(src)
        .expect("lex should succeed")
        .into_iter()
        .map(|s| s.token)
        .collect()
}

// ═══════════════════════════════════════════════════════════════════
// Bug 1: the arithmetic preprocessor was heredoc-blind
// ═══════════════════════════════════════════════════════════════════

#[test]
fn apostrophe_in_heredoc_body_does_not_poison_later_arithmetic() {
    // The apostrophe in "don't" used to open quote state in the
    // arithmetic preprocessor (which ran before heredoc extraction) and
    // swallow every later `$((..))` in the buffer.
    let t = toks("cat <<EOF\ndon't\nEOF\necho $((1+2))");
    assert!(
        t.contains(&Token::Arithmetic("1+2".to_string())),
        "arithmetic after a heredoc containing an apostrophe must expand, got: {t:?}"
    );
    let heredoc = t.iter().find_map(|t| match t {
        Token::HereDoc(d) => Some(d),
        _ => None,
    });
    assert_eq!(heredoc.expect("heredoc token").content, "don't\n");
}

#[test]
fn literal_heredoc_body_with_stray_arith_introducer_is_prose() {
    // `$((` in a LITERAL (<<'EOF') body is text, never an expression —
    // this used to false-positive UnterminatedArithmetic.
    let t = toks("cat <<'EOF'\nliteral $(( text\nEOF\necho done");
    let heredoc = t.iter().find_map(|t| match t {
        Token::HereDoc(d) => Some(d),
        _ => None,
    });
    let data = heredoc.expect("heredoc token");
    assert!(data.literal);
    assert_eq!(data.content, "literal $(( text\n");
}

#[test]
fn interpolated_heredoc_body_with_unterminated_arith_is_loud() {
    // In an INTERPOLATED body, `$(( ` without `))` is an error (bash
    // would fail the expansion too) — loud, not silently mangled.
    let err = tokenize("cat <<EOF\nbroken $(( text\nEOF\n").expect_err("should error");
    assert!(
        err.iter().any(|e| e.token == LexerError::UnterminatedArithmetic),
        "got: {err:?}"
    );
}

#[test]
fn arithmetic_expands_inside_interpolated_heredoc_body() {
    let t = toks("cat <<EOF\nsum: $((1+2))\nEOF\n");
    let heredoc = t.iter().find_map(|t| match t {
        Token::HereDoc(d) => Some(d),
        _ => None,
    });
    assert_eq!(
        heredoc.expect("heredoc token").content,
        "sum: ${__ARITH:1+2__}\n",
        "interpolated bodies carry the ${{__ARITH:expr__}} contract"
    );
}

// ═══════════════════════════════════════════════════════════════════
// Bug 2: the heredoc preprocessor was quote/comment-blind
// ═══════════════════════════════════════════════════════════════════

#[test]
fn heredoc_operator_inside_double_quotes_is_string_text() {
    // `echo "a << b"` used to misfire heredoc collection and kill the
    // whole buffer with UnterminatedHeredoc(delimiter: `b"`).
    let t = toks("echo \"a << b\"\necho after");
    assert_eq!(
        t,
        vec![
            Token::Ident("echo".into()),
            Token::String("a << b".into()),
            Token::Newline,
            Token::Ident("echo".into()),
            Token::Ident("after".into()),
        ]
    );
}

#[test]
fn heredoc_operator_inside_single_quotes_is_string_text() {
    let t = toks("echo 'a << b'\necho after");
    assert_eq!(
        t,
        vec![
            Token::Ident("echo".into()),
            Token::SingleString("a << b".into()),
            Token::Newline,
            Token::Ident("echo".into()),
            Token::Ident("after".into()),
        ]
    );
}

#[test]
fn heredoc_operator_inside_comment_is_comment_text() {
    let t = toks("# see <<EOF for details\necho after");
    assert_eq!(
        t,
        vec![
            Token::Newline,
            Token::Ident("echo".into()),
            Token::Ident("after".into()),
        ]
    );
}

// ═══════════════════════════════════════════════════════════════════
// Bug 3: glued arithmetic leaked raw marker text
// ═══════════════════════════════════════════════════════════════════

#[test]
fn glued_arithmetic_splits_into_adjacent_tokens() {
    // `$((1+2))abc` used to lex as ONE Ident carrying raw
    // `__KAISH_ARITH_…__abc` marker text all the way to the user.
    // Positional marker resolution splits it: Arithmetic + word,
    // span-adjacent.
    let spanned = tokenize("echo $((1+2))abc").expect("lex ok");
    assert_eq!(spanned.len(), 3, "got: {spanned:?}");
    assert_eq!(spanned[1].token, Token::Arithmetic("1+2".into()));
    assert_eq!(spanned[1].span, 5..13, "Arithmetic spans `$((1+2))` exactly");
    assert_eq!(spanned[2].token, Token::Ident("abc".into()));
    assert_eq!(spanned[2].span, 13..16);
}

#[test]
fn glued_arithmetic_is_rejected_loudly_by_the_parser() {
    // The split tokens are span-adjacent positionals, so the parser's
    // no-token-pasting guard rejects them with a quoting hint — kaish
    // deliberately does NOT interpolate bash's `3abc` here.
    let err = parse("echo $((1+2))abc").expect_err("should be a parse error");
    assert!(
        err[0].message.contains("token pasting"),
        "want the no-pasting hint, got: {}",
        err[0].message
    );
}

#[test]
fn arithmetic_glued_on_both_sides_splits_cleanly() {
    let t = toks("echo foo$((1+2))bar");
    assert_eq!(
        t,
        vec![
            Token::Ident("echo".into()),
            Token::Ident("foo".into()),
            Token::Arithmetic("1+2".into()),
            Token::Ident("bar".into()),
        ]
    );
}

// ═══════════════════════════════════════════════════════════════════
// Bug 4: fusion re-stringified numbers (leading zeros died)
// ═══════════════════════════════════════════════════════════════════

#[test]
fn colon_fusion_preserves_leading_zeros() {
    // Fused text is a verbatim source slice; `a:007` used to become
    // `a:7` via Int(7).to_string().
    assert_eq!(toks("echo a:007"), vec![
        Token::Ident("echo".into()),
        Token::Ident("a:007".into()),
    ]);
}

#[test]
fn glob_fusion_preserves_leading_zeros() {
    // `007*` used to glob as `7*`.
    assert_eq!(toks("ls 007*"), vec![
        Token::Ident("ls".into()),
        Token::GlobWord("007*".into()),
    ]);
}

// ═══════════════════════════════════════════════════════════════════
// Bug 5: every top-level `=` opened value position
// ═══════════════════════════════════════════════════════════════════

#[test]
fn spaced_eq_in_argv_does_not_suppress_glob_fusion() {
    // The `=` here is argv text (grep's pattern), not an assignment —
    // glob fusion must proceed. The statement-head DFA follows the
    // grammar: only an assignment's `=` opens value position.
    assert_eq!(toks("grep -E = [a-z]*"), vec![
        Token::Ident("grep".into()),
        Token::ShortFlag("E".into()),
        Token::Eq,
        Token::GlobWord("[a-z]*".into()),
    ]);
    assert_eq!(toks("cmd a = [x]"), vec![
        Token::Ident("cmd".into()),
        Token::Ident("a".into()),
        Token::Eq,
        Token::GlobWord("[x]".into()),
    ]);
}

#[test]
fn assignment_value_position_still_suppresses_bracket_fusion() {
    // Real assignments — glued, spaced, local, env-prefix chains — keep
    // their list-literal RHS as primitive tokens.
    for src in ["x=[ab]", "x = [ab]", "local x = [ab]", "a=1 x=[ab]"] {
        let t = toks(src);
        assert!(
            t.contains(&Token::LBracket) && t.contains(&Token::RBracket),
            "{src:?}: expected primitive brackets, got {t:?}"
        );
        assert!(
            !t.iter().any(|t| matches!(t, Token::GlobWord(_))),
            "{src:?}: expected no GlobWord, got {t:?}"
        );
    }
    // And they parse as list literals.
    assert!(parse("x = [ab]").is_ok());
    assert!(parse("local x = [ab]").is_ok());
}

#[test]
fn subscripted_lvalue_assignment_still_suppresses() {
    let t = toks("fruits[0]=kiwi");
    assert!(
        !t.iter().any(|t| matches!(t, Token::GlobWord(_))),
        "lvalue subscript must not glob-fuse, got {t:?}"
    );
    assert!(parse("fruits[0]=kiwi").is_ok());
}

// ═══════════════════════════════════════════════════════════════════
// Bug 6: no statement-boundary reset / no scope isolation
// ═══════════════════════════════════════════════════════════════════

#[test]
fn subst_body_gets_fresh_context_inside_test() {
    // `[[ -n $(x=[a]) ]]` — the pre-#95 counter leaked test depth into
    // the `$( )` body, so the inner `=` read as comparison and `[a]`
    // fused into a GlobWord. The Subst frame isolates the body: `x=[a]`
    // is an assignment with a list literal.
    let t = toks("[[ -n $(x=[a]) ]]");
    assert!(
        !t.iter().any(|t| matches!(t, Token::GlobWord(_))),
        "list literal inside $() must stay primitive, got {t:?}"
    );
}

#[test]
fn dangling_literal_state_resets_at_hard_separators() {
    // An unterminated `x=[a b` cannot poison tokenization past a `;`
    // (a `;` is never legal inside a literal). The statement itself
    // still fails at parse — loudly — but the next statement lexes
    // canonically.
    let t = toks("x=[a b; ls [dog]");
    assert!(
        t.contains(&Token::GlobWord("[dog]".into())),
        "post-`;` glob must fuse, got {t:?}"
    );
}

#[test]
fn multiline_list_literals_stay_open_across_newlines() {
    // Newlines are legal inside list/record literals (the parser
    // consumes them as separators), so the context must NOT reset there.
    let t = toks("x=[a\nb\nc]");
    assert!(
        !t.iter().any(|t| matches!(t, Token::GlobWord(_))),
        "multiline literal must stay primitive, got {t:?}"
    );
    let program = parse("x=[a\nb\nc]").expect("multiline list literal parses");
    let rendered = format!("{program:?}");
    assert!(rendered.contains("ListLiteral"), "got: {rendered}");
}

// ═══════════════════════════════════════════════════════════════════
// Heredoc span exactness (the old pipeline recorded no span
// replacements for heredocs; every token after one drifted)
// ═══════════════════════════════════════════════════════════════════

#[test]
fn spans_after_heredoc_are_exact_source_ranges() {
    let src = "cat <<EOF\nhello world\nEOF\necho after";
    let spanned = tokenize(src).expect("lex ok");
    for s in &spanned {
        let slice = src.get(s.span.start..s.span.end);
        assert!(
            slice.is_some(),
            "span {:?} for {:?} must be a valid source range",
            s.span,
            s.token
        );
    }
    let after = spanned
        .iter()
        .find(|s| matches!(&s.token, Token::Ident(i) if i == "after"))
        .expect("`after` token");
    assert_eq!(&src[after.span.start..after.span.end], "after");
    let heredoc = spanned
        .iter()
        .find(|s| matches!(s.token, Token::HereDoc(_)))
        .expect("heredoc token");
    assert_eq!(
        &src[heredoc.span.start..heredoc.span.end],
        "EOF",
        "the HereDoc token spans the delimiter word"
    );
    if let Token::HereDoc(d) = &heredoc.token {
        assert_eq!(d.body_start_offset, 10);
        assert_eq!(&src[d.body_start_offset..d.body_start_offset + 5], "hello");
    }
}

#[test]
fn body_start_offset_is_exact_even_after_arithmetic() {
    // With arithmetic BEFORE the heredoc, the old pipeline's offsets
    // were in preprocessed coordinates (its doc comment admitted the
    // drift). Now they are exact.
    let src = "x=$((1+2))\ncat <<EOF\nbody\nEOF\n";
    let spanned = tokenize(src).expect("lex ok");
    let heredoc = spanned
        .iter()
        .find_map(|s| match &s.token {
            Token::HereDoc(d) => Some(d),
            _ => None,
        })
        .expect("heredoc token");
    assert_eq!(&src[heredoc.body_start_offset..heredoc.body_start_offset + 4], "body");
}

// ═══════════════════════════════════════════════════════════════════
// Nested arithmetic inside command substitution
// ═══════════════════════════════════════════════════════════════════

#[test]
fn arithmetic_inside_command_substitution_works() {
    // The old preprocessor skipped `$( )` bodies on the theory that the
    // subcommand would re-lex them — but substitution bodies are lexed
    // inline, so the inner arithmetic hit logos raw and the whole
    // construct was a parse error. One composed scan extracts it
    // uniformly; bash computes `3` here and now kaish does too.
    let t = toks("echo $(echo $((1+2)))");
    assert!(
        t.contains(&Token::Arithmetic("1+2".to_string())),
        "inner arithmetic must be extracted, got {t:?}"
    );
    assert!(parse("echo $(echo $((1+2)))").is_ok());
}

// ═══════════════════════════════════════════════════════════════════
// Arithmetic inside a bare ${...}: loud, not leaked
// ═══════════════════════════════════════════════════════════════════

#[test]
fn arithmetic_inside_varref_default_is_a_loud_error() {
    // `${X:-$((1+2))}` used to leak raw `__KAISH_ARITH_…__` marker text
    // into the AST as the default value. There is no representation for
    // it (nested `${…}` in a bare reference is unsupported), so the
    // scanner rejects it with a hint instead of corrupting.
    let err = tokenize("echo ${X:-$((1+2))}").expect_err("should error");
    assert_eq!(err[0].token, LexerError::ArithmeticInVarRef);
}

#[test]
fn arithmetic_in_default_word_inside_string_still_works() {
    // Inside a double-quoted string the construct routes through string
    // interpolation and keeps working.
    let t = toks(r#"echo "${X:-$((1+2))}""#);
    assert!(
        matches!(&t[1], Token::String(s) if s.contains("${__ARITH:1+2__}")),
        "got {t:?}"
    );
}

// ═══════════════════════════════════════════════════════════════════
// $# before arithmetic ($ guards the comment scan)
// ═══════════════════════════════════════════════════════════════════

#[test]
fn arg_count_does_not_open_a_comment() {
    // The old preprocessor treated the `#` of `$#` as a comment opener,
    // hiding everything after it from arithmetic extraction.
    let t = toks("echo $# $((1+2))");
    assert_eq!(
        t,
        vec![
            Token::Ident("echo".into()),
            Token::ArgCount,
            Token::Arithmetic("1+2".into()),
        ]
    );
}

// ═══════════════════════════════════════════════════════════════════
// Multiple heredocs on one line (previously the second one vanished)
// ═══════════════════════════════════════════════════════════════════

#[test]
fn two_heredocs_on_one_line_both_collect() {
    let src = "cat <<A <<B\nbody a\nA\nbody b\nB\n";
    let spanned = tokenize(src).expect("lex ok");
    let bodies: Vec<&str> = spanned
        .iter()
        .filter_map(|s| match &s.token {
            Token::HereDoc(d) => Some(d.content.as_str()),
            _ => None,
        })
        .collect();
    assert_eq!(bodies, vec!["body a\n", "body b\n"]);
    // The parser still rejects two stdin sources — but now with its own
    // clean ambiguous-stdin message instead of a lexer mangle.
    assert!(parse(src).is_err());
}
