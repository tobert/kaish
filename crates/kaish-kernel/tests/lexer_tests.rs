//! Lexer tests using rstest for parameterization.
//!
//! These tests replace the custom tokens.txt test file format with native Rust tests.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::lexer::{tokenize, LexerError, Token};
use rstest::rstest;

/// Format a Token into the test format string.
fn format_token(token: &Token) -> String {
    fn escape_for_display(s: &str) -> String {
        s.replace('\n', "\\n")
            .replace('\t', "\\t")
            .replace('\r', "\\r")
    }

    match token {
        // Keywords
        Token::Set => "SET".to_string(),
        Token::Local => "LOCAL".to_string(),
        Token::If => "IF".to_string(),
        Token::Then => "THEN".to_string(),
        Token::Else => "ELSE".to_string(),
        Token::Elif => "ELIF".to_string(),
        Token::Fi => "FI".to_string(),
        Token::For => "FOR".to_string(),
        Token::While => "WHILE".to_string(),
        Token::In => "IN".to_string(),
        Token::Do => "DO".to_string(),
        Token::Done => "DONE".to_string(),
        Token::Case => "CASE".to_string(),
        Token::Esac => "ESAC".to_string(),
        Token::Function => "FUNCTION".to_string(),
        Token::Break => "BREAK".to_string(),
        Token::Continue => "CONTINUE".to_string(),
        Token::Return => "RETURN".to_string(),
        Token::Exit => "EXIT".to_string(),
        Token::True => "BOOL(true)".to_string(),
        Token::False => "BOOL(false)".to_string(),

        // Type keywords
        Token::TypeString => "TYPESTRING".to_string(),
        Token::TypeInt => "TYPEINT".to_string(),
        Token::TypeFloat => "TYPEFLOAT".to_string(),
        Token::TypeBool => "TYPEBOOL".to_string(),

        // Operators
        Token::And => "AMPAMP".to_string(),
        Token::Or => "PIPEPIPE".to_string(),
        Token::EqEq => "EQEQ".to_string(),
        Token::NotEq => "NEQ".to_string(),
        Token::Match => "MATCH".to_string(),
        Token::NotMatch => "NOTMATCH".to_string(),
        Token::GtEq => "GEQ".to_string(),
        Token::LtEq => "LEQ".to_string(),
        Token::GtGt => "REDIR_APPEND".to_string(),
        Token::StderrToStdout => "REDIR_MERGE".to_string(),
        Token::StdoutToStderr => "REDIR_STDOUT_TO_STDERR".to_string(),
        Token::StdoutToStderr2 => "REDIR_STDOUT_TO_STDERR".to_string(),
        Token::Stderr => "REDIR_ERR".to_string(),
        Token::Both => "REDIR_BOTH".to_string(),
        Token::HereDocStart => "HEREDOC_START".to_string(),
        Token::HereString => "HERESTRING".to_string(),
        Token::DoubleSemi => "DOUBLESEMI".to_string(),

        // Single-char operators
        Token::Eq => "EQ".to_string(),
        Token::Pipe => "PIPE".to_string(),
        Token::Amp => "AMP".to_string(),
        Token::Gt => "GT".to_string(),
        Token::Lt => "LT".to_string(),
        Token::Semi => "SEMI".to_string(),
        Token::Colon => "COLON".to_string(),
        Token::Comma => "COMMA".to_string(),
        Token::Dot => "DOT".to_string(),
        Token::DotDot => "DOTDOT".to_string(),
        Token::Tilde => "TILDE".to_string(),
        Token::TildePath(s) => format!("TILDEPATH({})", s),
        Token::RelativePath(s) => format!("RELPATH({})", s),
        Token::DotSlashPath(s) => format!("DOTSLASH({})", s),

        // Brackets
        Token::LBrace => "LBRACE".to_string(),
        Token::RBrace => "RBRACE".to_string(),
        Token::LBracket => "LBRACK".to_string(),
        Token::RBracket => "RBRACK".to_string(),
        Token::LParen => "LPAREN".to_string(),
        Token::RParen => "RPAREN".to_string(),
        Token::Star => "STAR".to_string(),
        Token::Bang => "BANG".to_string(),
        Token::Question => "QUESTION".to_string(),
        Token::GlobWord(s) => format!("GLOB({})", s),

        // Arithmetic and command substitution
        Token::Arithmetic(s) => format!("ARITH({})", s),
        Token::CmdSubstStart => "CMDSUBST".to_string(),

        // Flags
        Token::LongFlag(s) => format!("LONGFLAG({})", s),
        Token::ShortFlag(s) => format!("SHORTFLAG({})", s),
        Token::PlusFlag(s) => format!("PLUSFLAG({})", s),
        Token::DoubleDash => "DOUBLEDASH".to_string(),
        // Bare words starting with + or -
        Token::PlusBare(s) => format!("PLUSBARE({})", s),
        Token::MinusBare(s) => format!("MINUSBARE({})", s),
        Token::JobSpec(s) => format!("JOBSPEC({})", s),
        Token::MinusAlone => "MINUSALONE".to_string(),

        // Literals
        Token::String(s) => format!("STRING({})", escape_for_display(s)),
        Token::SingleString(s) => format!("SINGLESTRING({})", s),
        Token::HereDoc(d) => format!("HEREDOC({}, literal={})", escape_for_display(&d.content), d.literal),
        Token::VarRef(s) => format!("VARREF({})", s),
        Token::SimpleVarRef(s) => format!("SIMPLEVARREF({})", s),
        Token::Positional(n) => format!("POSITIONAL({})", n),
        Token::AllArgs => "ALLARGS".to_string(),
        Token::ArgCount => "ARGCOUNT".to_string(),
        Token::LastExitCode => "LASTEXITCODE".to_string(),
        Token::CurrentPid => "CURRENTPID".to_string(),
        Token::VarLength(s) => format!("VARLENGTH({})", s),
        Token::Int(n) => format!("INT({})", n),
        Token::Float(n) => {
            let s = n.to_string();
            if s.contains('.') {
                format!("FLOAT({})", s)
            } else {
                format!("FLOAT({}.0)", s)
            }
        }

        // Identifiers and paths
        Token::Ident(s) => format!("IDENT({})", s),
        Token::NumberIdent(s) => format!("NUMIDENT({})", s),
        Token::DottedIdent(s) => format!("DOTIDENT({})", s),
        Token::Path(s) => format!("PATH({})", s),

        // Structural
        Token::Comment => "COMMENT".to_string(),
        Token::Newline => "NEWLINE".to_string(),
        Token::LineContinuation => "LINECONT".to_string(),

        // Invalid variants (should never be produced)
        Token::InvalidFloatNoLeading => "INVALID_FLOAT_NO_LEADING".to_string(),
        Token::InvalidFloatNoTrailing => "INVALID_FLOAT_NO_TRAILING".to_string(),
        Token::BacktickRejected => "BACKTICK_REJECTED".to_string(),
    }
}

/// Run a lexer test that expects successful tokenization.
fn run_lexer_test(input: &str, expected: &[&str]) {
    let tokens = tokenize(input).expect("lexing should succeed");
    let actual: Vec<String> = tokens
        .iter()
        .filter(|s| !matches!(s.token, Token::Newline | Token::Comment))
        .map(|s| format_token(&s.token))
        .collect();
    let expected: Vec<String> = expected.iter().map(|s| s.to_string()).collect();
    assert_eq!(actual, expected, "input: {:?}", input);
}

/// Run a lexer test that expects an error.
fn run_lexer_error_test(input: &str) {
    let result = tokenize(input);
    assert!(result.is_err(), "expected error for input: {:?}", input);
}

/// Run a lexer test that expects a *specific* error variant. Asserting only
/// `is_err()` would still pass if a curated diagnostic regressed to the generic
/// `UnexpectedCharacter`, so the negative suites pin the variant they document.
fn run_lexer_error_variant(input: &str, expected: LexerError) {
    let errors = tokenize(input).expect_err(&format!("expected error for input: {input:?}"));
    assert!(
        errors.iter().any(|e| e.token == expected),
        "input {input:?}: expected {expected:?}, got {:?}",
        errors.iter().map(|e| &e.token).collect::<Vec<_>>(),
    );
}

/// Like [`run_lexer_error_variant`] but matches against a predicate, for error
/// variants that carry a payload (e.g. the ambiguous-boolean spellings).
fn run_lexer_error_matching(input: &str, pred: impl Fn(&LexerError) -> bool, what: &str) {
    let errors = tokenize(input).expect_err(&format!("expected error for input: {input:?}"));
    assert!(
        errors.iter().any(|e| pred(&e.token)),
        "input {input:?}: expected {what}, got {:?}",
        errors.iter().map(|e| &e.token).collect::<Vec<_>>(),
    );
}

// =============================================================================
// Keywords
// =============================================================================

#[rstest]
#[case::keyword_set("set", &["SET"])]
#[case::keyword_local("local", &["LOCAL"])]
#[case::keyword_if("if", &["IF"])]
#[case::keyword_then("then", &["THEN"])]
#[case::keyword_else("else", &["ELSE"])]
#[case::keyword_elif("elif", &["ELIF"])]
#[case::keyword_fi("fi", &["FI"])]
#[case::keyword_for("for", &["FOR"])]
#[case::keyword_in("in", &["IN"])]
#[case::keyword_do("do", &["DO"])]
#[case::keyword_done("done", &["DONE"])]
fn lexer_keywords(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Identifiers
// =============================================================================

#[rstest]
#[case::ident_simple("foo", &["IDENT(foo)"])]
#[case::ident_underscore("foo_bar", &["IDENT(foo_bar)"])]
#[case::ident_hyphen("foo-bar", &["IDENT(foo-bar)"])]
#[case::ident_private("_private", &["IDENT(_private)"])]
#[case::ident_with_number("x1", &["IDENT(x1)"])]
fn lexer_identifiers(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// Digit-leading bare words are valid argv tokens: SHA prefixes (019dda1c),
// UUIDs, version-ish identifiers. Lex them as NumberIdent so the parser
// can treat them as bareword strings. (Pure digit sequences still lex as
// Int — at least one alpha character is required to land here.)
#[rstest]
#[case::numident_hex("019dda1c", &["NUMIDENT(019dda1c)"])]
#[case::numident_alpha_after_digit("123abc", &["NUMIDENT(123abc)"])]
#[case::numident_with_dash("019dda1c-5b3f-7000", &["NUMIDENT(019dda1c-5b3f-7000)"])]
#[case::numident_with_dot("019dda1c.commit", &["NUMIDENT(019dda1c.commit)"])]
fn lexer_number_idents(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// Dot-prefixed bare words: `.gitignore`, `.parent`, `.parent.parent`. Must
// lex as a single token, not Dot + Ident, so they are not misparsed as the
// POSIX `.` (source) command followed by an argument.
#[rstest]
#[case::dotident_simple(".parent", &["DOTIDENT(.parent)"])]
#[case::dotident_chained(".parent.parent", &["DOTIDENT(.parent.parent)"])]
#[case::dotident_hidden_file(".gitignore", &["DOTIDENT(.gitignore)"])]
#[case::dotident_with_dash(".foo-bar", &["DOTIDENT(.foo-bar)"])]
fn lexer_dot_idents(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// `. file` (with whitespace) must remain Dot + Ident so the source alias works.
#[rstest]
#[case::source_alias_with_space(". script", &["DOT", "IDENT(script)"])]
#[case::source_alias_with_dotted_file(". script.kai", &["DOT", "IDENT(script.kai)"])]
fn lexer_source_alias_preserved(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Integers
// =============================================================================

#[rstest]
#[case::int_zero("0", &["INT(0)"])]
#[case::int_positive("123", &["INT(123)"])]
#[case::int_negative("-456", &["INT(-456)"])]
#[case::int_large("999999999", &["INT(999999999)"])]
fn lexer_integers(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Floats
// =============================================================================

#[rstest]
#[case::float_zero("0.0", &["FLOAT(0.0)"])]
#[case::float_pi("3.14", &["FLOAT(3.14)"])]
#[case::float_negative("-2.5", &["FLOAT(-2.5)"])]
fn lexer_floats(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

#[rstest]
#[case::float_no_leading(".5", LexerError::InvalidFloatNoLeading)]
#[case::float_no_trailing("5.", LexerError::InvalidFloatNoTrailing)]
fn lexer_float_errors(#[case] input: &str, #[case] expected: LexerError) {
    run_lexer_error_variant(input, expected);
}

// =============================================================================
// Booleans
// =============================================================================

#[rstest]
#[case::bool_true("true", &["BOOL(true)"])]
#[case::bool_false("false", &["BOOL(false)"])]
fn lexer_booleans(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

#[rstest]
#[case::bool_upper_true("TRUE")]
#[case::bool_upper_false("FALSE")]
#[case::bool_mixed_true("True")]
#[case::bool_like_yes("yes")]
#[case::bool_like_no("no")]
#[case::bool_like_yes_upper("YES")]
#[case::bool_like_no_upper("NO")]
fn lexer_ambiguous_boolean_errors(#[case] input: &str) {
    run_lexer_error_matching(
        input,
        |e| {
            matches!(
                e,
                LexerError::AmbiguousBoolean(_) | LexerError::AmbiguousBooleanLike(_)
            )
        },
        "an ambiguous-boolean error",
    );
}

// =============================================================================
// Double-Quoted Strings
// =============================================================================

#[rstest]
#[case::string_simple(r#""hello""#, &["STRING(hello)"])]
#[case::string_with_spaces(r#""hello world""#, &["STRING(hello world)"])]
#[case::string_empty(r#""""#, &["STRING()"])]
#[case::string_newline(r#""line\nbreak""#, &["STRING(line\\nbreak)"])]
#[case::string_tab(r#""tab\there""#, &["STRING(tab\\there)"])]
#[case::string_quote(r#""quote\"here""#, &["STRING(quote\"here)"])]
#[case::string_backslash(r#""slash\\here""#, &["STRING(slash\\here)"])]
#[case::string_unicode(r#""unicode\u0041""#, &["STRING(unicodeA)"])]
fn lexer_double_quoted_strings(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

#[rstest]
#[case::string_unterminated(r#""unterminated"#)]
fn lexer_string_errors(#[case] input: &str) {
    // KNOWN GAP (docs/issues.md): an unterminated double-quoted string never
    // matches the logos `String` token regex, so it surfaces the generic
    // `UnexpectedCharacter` rather than the curated `UnterminatedString` (which
    // only the interpolated/complete-string helper emits). Pinning the *actual*
    // variant keeps the test honest and flags any change for review — improving
    // the diagnostic to `UnterminatedString` should update this assertion.
    run_lexer_error_variant(input, LexerError::UnexpectedCharacter);
}

// =============================================================================
// Single-Quoted Strings
// =============================================================================

#[rstest]
#[case::singlestring_simple("'hello'", &["SINGLESTRING(hello)"])]
#[case::singlestring_with_spaces("'hello world'", &["SINGLESTRING(hello world)"])]
#[case::singlestring_empty("''", &["SINGLESTRING()"])]
#[case::singlestring_no_var("'no $VAR here'", &["SINGLESTRING(no $VAR here)"])]
#[case::singlestring_no_escapes(r"'no escapes: \n'", &[r"SINGLESTRING(no escapes: \n)"])]
fn lexer_single_quoted_strings(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

#[rstest]
#[case::singlestring_unterminated("'unterminated")]
fn lexer_singlestring_errors(#[case] input: &str) {
    run_lexer_error_test(input);
}

// =============================================================================
// Variable References
// =============================================================================

#[rstest]
#[case::varref_braced("${X}", &["VARREF(${X})"])]
#[case::varref_braced_lower("${foo}", &["VARREF(${foo})"])]
#[case::varref_braced_underscore("${foo_bar}", &["VARREF(${foo_bar})"])]
#[case::varref_field("${X.field}", &["VARREF(${X.field})"])]
#[case::varref_index("${X[0]}", &["VARREF(${X[0]})"])]
#[case::varref_path("${X.a.b[0].c}", &["VARREF(${X.a.b[0].c})"])]
fn lexer_braced_varrefs(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

#[rstest]
#[case::varref_unterminated_brace("${")]
#[case::varref_unterminated_name("${X")]
fn lexer_varref_errors(#[case] input: &str) {
    run_lexer_error_test(input);
}

#[rstest]
#[case::simple_varref("$X", &["SIMPLEVARREF(X)"])]
#[case::simple_varref_lower("$foo", &["SIMPLEVARREF(foo)"])]
#[case::simple_varref_underscore("$foo_bar", &["SIMPLEVARREF(foo_bar)"])]
#[case::simple_varref_private("$_private", &["SIMPLEVARREF(_private)"])]
fn lexer_simple_varrefs(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Operators
// =============================================================================

#[rstest]
#[case::op_eq("=", &["EQ"])]
#[case::op_eqeq("==", &["EQEQ"])]
#[case::op_neq("!=", &["NEQ"])]
#[case::op_lt("<", &["LT"])]
#[case::op_gt(">", &["GT"])]
#[case::op_leq("<=", &["LEQ"])]
#[case::op_geq(">=", &["GEQ"])]
#[case::op_pipe("|", &["PIPE"])]
#[case::op_amp("&", &["AMP"])]
#[case::op_ampamp("&&", &["AMPAMP"])]
#[case::op_pipepipe("||", &["PIPEPIPE"])]
fn lexer_operators(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Redirects
// =============================================================================

#[rstest]
#[case::redir_append(">>", &["REDIR_APPEND"])]
#[case::redir_err("2>", &["REDIR_ERR"])]
#[case::redir_both("&>", &["REDIR_BOTH"])]
#[case::redir_merge("2>&1", &["REDIR_MERGE"])]
#[case::redir_merge_in_cmd("cmd 2>&1", &["IDENT(cmd)", "REDIR_MERGE"])]
#[case::redir_merge_pipe("cmd 2>&1 | cat", &["IDENT(cmd)", "REDIR_MERGE", "PIPE", "IDENT(cat)"])]
#[case::herestring_alone("<<<", &["HERESTRING"])]
#[case::herestring_with_word("<<< hi", &["HERESTRING", "IDENT(hi)"])]
#[case::herestring_with_var("<<< \"$R\"", &["HERESTRING", "STRING($R)"])]
#[case::herestring_no_space("<<<hi", &["HERESTRING", "IDENT(hi)"])]
#[case::herestring_in_cmd("cat <<< hi", &["IDENT(cat)", "HERESTRING", "IDENT(hi)"])]
#[case::heredoc_start_still_preprocesses("<< EOF\nEOF", &["HEREDOC_START", "HEREDOC(, literal=false)"])]
#[case::four_lt_greedy_match("<<<<", &["HERESTRING", "LT"])]
fn lexer_redirects(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Brackets
// =============================================================================

#[rstest]
#[case::bracket_lbrace("{", &["LBRACE"])]
#[case::bracket_rbrace("}", &["RBRACE"])]
#[case::bracket_lbrack("[", &["LBRACK"])]
#[case::bracket_rbrack("]", &["RBRACK"])]
#[case::bracket_lparen("(", &["LPAREN"])]
#[case::bracket_rparen(")", &["RPAREN"])]
#[case::bracket_double_lbrack("[[", &["LBRACK", "LBRACK"])]
#[case::bracket_double_rbrack("]]", &["RBRACK", "RBRACK"])]
fn lexer_brackets(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Punctuation
// =============================================================================

#[rstest]
#[case::punct_comma(",", &["COMMA"])]
#[case::punct_colon(":", &["COLON"])]
#[case::punct_semi(";", &["SEMI"])]
#[case::punct_dot(".", &["DOT"])]
#[case::punct_star("*", &["STAR"])]
#[case::punct_question("?", &["QUESTION"])]
#[case::punct_bang("!", &["BANG"])]
fn lexer_punctuation(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Bang operator priority (multi-char operators should take precedence)
// =============================================================================

#[rstest]
#[case::bang_alone("!", &["BANG"])]
#[case::bang_neq_takes_priority("!=", &["NEQ"])]
#[case::bang_notmatch_takes_priority("!~", &["NOTMATCH"])]
#[case::bang_then_eq("! =", &["BANG", "EQ"])]
#[case::bang_with_command("! true", &["BANG", "BOOL(true)"])]
#[case::negated_char_class_pattern("[!a-z]", &["GLOB([!a-z])"])]
fn lexer_bang_operator(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Glob word merging
// =============================================================================

#[rstest]
#[case::star_dot_txt("*.txt", &["GLOB(*.txt)"])]
#[case::double_star_slash_rs("**/*.rs", &["GLOB(**/*.rs)"])]
#[case::path_star_go("src/*.go", &["GLOB(src/*.go)"])]
#[case::question_mark_glob("file?.log", &["GLOB(file?.log)"])]
#[case::bracket_class("[a-z].txt", &["GLOB([a-z].txt)"])]
#[case::star_alone("*", &["STAR"])]
#[case::question_alone("?", &["QUESTION"])]
#[case::quoted_not_glob("\"*.txt\"", &["STRING(*.txt)"])]
#[case::no_glob_chars("foo bar", &["IDENT(foo)", "IDENT(bar)"])]
#[case::star_dot_tar_gz("*.tar.gz", &["GLOB(*.tar.gz)"])]
#[case::dot_star(".*", &["GLOB(.*)"])]
#[case::star_dot_brace_rs_go("*.{rs,go}", &["GLOB(*.{rs,go})"])]
#[case::glob_colon_merge("foo::bar*.txt", &["GLOB(foo::bar*.txt)"])]
#[case::glob_tilde_path("~/src/*.rs", &["GLOB(~/src/*.rs)"])]
#[case::glob_relative_path("../src/*.rs", &["GLOB(../src/*.rs)"])]
#[case::glob_dot_slash_path("./*.rs", &["GLOB(./*.rs)"])]
#[case::glob_adjacent_to_semi("*.txt;", &["GLOB(*.txt)", "SEMI"])]
fn lexer_glob_word(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Whitespace & Comments
// =============================================================================

#[rstest]
#[case::string_with_spaces_preserved(r#""  spaces  ""#, &["STRING(  spaces  )"])]
fn lexer_whitespace_in_strings(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Flags
// =============================================================================

#[rstest]
#[case::shortflag_l("-l", &["SHORTFLAG(l)"])]
#[case::shortflag_a("-a", &["SHORTFLAG(a)"])]
#[case::shortflag_combined("-la", &["SHORTFLAG(la)"])]
#[case::shortflag_triple("-vvv", &["SHORTFLAG(vvv)"])]
fn lexer_short_flags(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

#[rstest]
#[case::longflag_force("--force", &["LONGFLAG(force)"])]
#[case::longflag_verbose("--verbose", &["LONGFLAG(verbose)"])]
#[case::longflag_hyphen("--foo-bar", &["LONGFLAG(foo-bar)"])]
#[case::longflag_message("--message", &["LONGFLAG(message)"])]
fn lexer_long_flags(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

#[rstest]
#[case::plusflag_e("+e", &["PLUSFLAG(e)"])]
#[case::plusflag_x("+x", &["PLUSFLAG(x)"])]
#[case::plusflag_combined("+ex", &["PLUSFLAG(ex)"])]
fn lexer_plus_flags(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

#[rstest]
#[case::doubledash("--", &["DOUBLEDASH"])]
fn lexer_double_dash(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// Note: Single dash "-" is now valid as MinusAlone (for cat - stdin indicator)
// Triple dash "---" is DoubleDash + MinusAlone
#[rstest]
#[case::single_dash("-", &["MINUSALONE"])]
#[case::triple_dash("---", &["DOUBLEDASH", "MINUSALONE"])]
fn lexer_dash_variants(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

#[rstest]
#[case::date_format("+%s", &["PLUSBARE(+%s)"])]
#[case::date_format_complex("+%Y-%m-%d", &["PLUSBARE(+%Y-%m-%d)"])]
fn lexer_plus_bare(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Flags vs Negative Numbers
// =============================================================================

#[rstest]
#[case::negative_123("-123", &["INT(-123)"])]
#[case::negative_1("-1", &["INT(-1)"])]
#[case::flag_l("-l", &["SHORTFLAG(l)"])]
#[case::negative_1_then_ident("-1a", &["INT(-1)", "IDENT(a)"])]
fn lexer_flag_vs_number(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Combined Sequences
// =============================================================================

#[rstest]
#[case::bash_assign("X=5", &["IDENT(X)", "EQ", "INT(5)"])]
#[case::echo_string(r#"echo "hi""#, &["IDENT(echo)", "STRING(hi)"])]
#[case::cmd_named_args("cmd a=1 b=2", &["IDENT(cmd)", "IDENT(a)", "EQ", "INT(1)", "IDENT(b)", "EQ", "INT(2)"])]
#[case::pipe_chain("a | b | c", &["IDENT(a)", "PIPE", "IDENT(b)", "PIPE", "IDENT(c)"])]
#[case::redirect("x > file", &["IDENT(x)", "GT", "IDENT(file)"])]
fn lexer_combined_sequences(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Flag Sequences
// =============================================================================

#[rstest]
#[case::ls_l("ls -l", &["IDENT(ls)", "SHORTFLAG(l)"])]
#[case::ls_la("ls -la", &["IDENT(ls)", "SHORTFLAG(la)"])]
#[case::git_force("git --force", &["IDENT(git)", "LONGFLAG(force)"])]
#[case::git_push_force("git push --force", &["IDENT(git)", "IDENT(push)", "LONGFLAG(force)"])]
#[case::set_e("set -e", &["SET", "SHORTFLAG(e)"])]
#[case::set_plus_e("set +e", &["SET", "PLUSFLAG(e)"])]
#[case::set_multi_flags("set -e -u", &["SET", "SHORTFLAG(e)", "SHORTFLAG(u)"])]
#[case::cmd_doubledash_flag("cmd -- -flag", &["IDENT(cmd)", "DOUBLEDASH", "SHORTFLAG(flag)"])]
fn lexer_flag_sequences(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Flag-metachar merging (awk -F: idiom)
// =============================================================================
//
// Only `:` (Colon) fuses onto a short flag when span-adjacent (no whitespace).
// `;` (Semi) and `|` (Pipe) are shell operators and must NEVER fuse — even when
// typed immediately after a flag with no space — because they are statement
// terminators and pipe operators respectively.  bash also requires quoting for
// those forms: `awk -F';'`, not `awk -F;`.
// Guarded by span-adjacency: a *space*-separated flag and colon must stay
// as separate tokens.

#[rstest]
// Colon glued onto short flag: `awk -F:` idiom — should fuse
#[case::shortflag_colon("-F:", &["SHORTFLAG(F:)"])]
// Double-colon run: `-F::` fuses completely into one token
#[case::shortflag_double_colon("-F::", &["SHORTFLAG(F::)"])]
// Semicolon glued onto short flag: must NOT fuse — `;` is a shell operator
#[case::shortflag_semi("-F;", &["SHORTFLAG(F)", "SEMI"])]
// Pipe glued onto short flag: must NOT fuse — `|` is a shell operator
#[case::shortflag_pipe("-F|", &["SHORTFLAG(F)", "PIPE"])]
// Colon in full command context: awk -F: is one token; the program is another
#[case::awk_colon_full(r"awk -F: '{print $1}'", &["IDENT(awk)", "SHORTFLAG(F:)", "SINGLESTRING({print $1})"])]
// Regression: `ls -l|cat` — the Pipe must survive as a real pipeline operator
#[case::ls_pipe_cat("ls -l|cat", &["IDENT(ls)", "SHORTFLAG(l)", "PIPE", "IDENT(cat)"])]
// Regression: `cmd -x;cmd2` — the Semi must survive as a real statement separator
#[case::cmd_semi_cmd2("cmd -x;cmd2", &["IDENT(cmd)", "SHORTFLAG(x)", "SEMI", "IDENT(cmd2)"])]
// Space-separated: flag and operator MUST stay separate (no merge)
#[case::shortflag_space_colon("-F :", &["SHORTFLAG(F)", "COLON"])]
#[case::shortflag_space_semi("-F ;", &["SHORTFLAG(F)", "SEMI"])]
#[case::shortflag_space_pipe("-F |", &["SHORTFLAG(F)", "PIPE"])]
fn lexer_flag_metachar_merge(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Special Variables
// =============================================================================

#[rstest]
#[case::positional_0("$0", &["POSITIONAL(0)"])]
#[case::positional_1("$1", &["POSITIONAL(1)"])]
#[case::positional_9("$9", &["POSITIONAL(9)"])]
#[case::all_args("$@", &["ALLARGS"])]
#[case::arg_count("$#", &["ARGCOUNT"])]
#[case::last_exit_code("$?", &["LASTEXITCODE"])]
#[case::current_pid("$$", &["CURRENTPID"])]
#[case::var_length("${#NAME}", &["VARLENGTH(NAME)"])]
fn lexer_special_variables(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

#[rstest]
#[case::echo_exit_code("echo $?", &["IDENT(echo)", "LASTEXITCODE"])]
#[case::echo_pid("echo $$", &["IDENT(echo)", "CURRENTPID"])]
#[case::echo_all_args("echo $@", &["IDENT(echo)", "ALLARGS"])]
#[case::echo_arg_count("echo $#", &["IDENT(echo)", "ARGCOUNT"])]
fn lexer_special_variables_in_context(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

/// Arithmetic inside command substitutions must not be preprocessed by the outer lexer.
/// The inner command sub is re-lexed when evaluated, so $((expr)) inside $(...) is
/// handled by the inner lexer, not the outer preprocessing pass.
#[rstest]
#[case::arith_inside_cmd_sub_single_quoted(
    "$(kaish -c 'echo $((2 + 2))')",
    &["CMDSUBST", "IDENT(kaish)", "SHORTFLAG(c)", "SINGLESTRING(echo $((2 + 2)))", "RPAREN"]
)]
#[case::arith_outside_cmd_sub(
    "X=$((1 + 2)); $(echo hello)",
    &["IDENT(X)", "EQ", "ARITH(1 + 2)", "SEMI", "CMDSUBST", "IDENT(echo)", "IDENT(hello)", "RPAREN"]
)]
// Bug A: single quote inside double quotes must not start quote mode
#[case::single_quote_in_double_quotes(
    r#"echo "It's $((1+1))""#,
    &["IDENT(echo)", "STRING(It's ${__ARITH:1+1__})"]
)]
// Arithmetic in simple double-quoted string
#[case::arith_in_double_quote_string(
    r#"echo "$((2+3))""#,
    &["IDENT(echo)", "STRING(${__ARITH:2+3__})"]
)]
// Bug B: paren in string inside command sub shouldn't break skipper
#[case::paren_in_string_inside_cmd_sub(
    r#"$(echo "foo ) bar")"#,
    &["CMDSUBST", "IDENT(echo)", "STRING(foo ) bar)", "RPAREN"]
)]
fn lexer_arithmetic_in_command_substitution(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}

// =============================================================================
// Navigation tokens: .., ~, ~/path, ../path
// =============================================================================

#[rstest]
#[case::dotdot("..", &["DOTDOT"])]
#[case::tilde("~", &["TILDE"])]
#[case::tilde_path("~/foo", &["TILDEPATH(~/foo)"])]
#[case::tilde_path_nested("~/src/kaish", &["TILDEPATH(~/src/kaish)"])]
#[case::relative_path("../foo", &["RELPATH(../foo)"])]
#[case::relative_path_nested("../foo/bar", &["RELPATH(../foo/bar)"])]
#[case::cd_dotdot("cd ..", &["IDENT(cd)", "DOTDOT"])]
#[case::cd_tilde("cd ~", &["IDENT(cd)", "TILDE"])]
#[case::cd_tilde_path("cd ~/foo", &["IDENT(cd)", "TILDEPATH(~/foo)"])]
#[case::cd_relative("cd ../bar", &["IDENT(cd)", "RELPATH(../bar)"])]
#[case::dot_slash("./foo", &["DOTSLASH(./foo)"])]
#[case::dot_slash_nested("./src/main.rs", &["DOTSLASH(./src/main.rs)"])]
#[case::cd_dot_slash("cd ./crates", &["IDENT(cd)", "DOTSLASH(./crates)"])]
// Bare relative paths (no ./ or ../ prefix) — regression test for cd only
// traversing the first path component
#[case::cd_bare_relative("cd src/kaish", &["IDENT(cd)", "RELPATH(src/kaish)"])]
#[case::cd_bare_relative_nested("cd src/kaish/crates", &["IDENT(cd)", "RELPATH(src/kaish/crates)"])]
// Trailing slash must stay attached to the path word. Splitting `dest/` into
// `Ident(dest)` + `Path(/)` silently turned `cp a b dest/` into 4 operands.
#[case::bare_relative_trailing_slash("cp a.txt dest/", &["IDENT(cp)", "IDENT(a.txt)", "RELPATH(dest/)"])]
#[case::bare_relative_nested_trailing_slash("cd src/kaish/", &["IDENT(cd)", "RELPATH(src/kaish/)"])]
fn lexer_navigation_tokens(#[case] input: &str, #[case] expected: &[&str]) {
    run_lexer_test(input, expected);
}
