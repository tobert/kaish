//! Lexer tests using rstest for parameterization.
//!
//! These tests replace the custom tokens.txt test file format with native Rust tests.

use kaish_kernel::lexer::{tokenize, Token};
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
        Token::Stderr => "REDIR_ERR".to_string(),
        Token::Both => "REDIR_BOTH".to_string(),
        Token::HereDocStart => "HEREDOC_START".to_string(),
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

        // Arithmetic and command substitution
        Token::Arithmetic(s) => format!("ARITH({})", s),
        Token::CmdSubstStart => "CMDSUBST".to_string(),

        // Flags
        Token::LongFlag(s) => format!("LONGFLAG({})", s),
        Token::ShortFlag(s) => format!("SHORTFLAG({})", s),
        Token::PlusFlag(s) => format!("PLUSFLAG({})", s),
        Token::DoubleDash => "DOUBLEDASH".to_string(),

        // Literals
        Token::String(s) => format!("STRING({})", escape_for_display(s)),
        Token::SingleString(s) => format!("SINGLESTRING({})", s),
        Token::HereDoc(s) => format!("HEREDOC({})", escape_for_display(s)),
        Token::VarRef(s) => format!("VARREF({})", s),
        Token::SimpleVarRef(s) => format!("SIMPLEVARREF({})", s),
        Token::Positional(n) => format!("POSITIONAL({})", n),
        Token::AllArgs => "ALLARGS".to_string(),
        Token::ArgCount => "ARGCOUNT".to_string(),
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
        Token::Path(s) => format!("PATH({})", s),

        // Structural
        Token::Comment => "COMMENT".to_string(),
        Token::Newline => "NEWLINE".to_string(),
        Token::LineContinuation => "LINECONT".to_string(),

        // Invalid variants (should never be produced)
        Token::InvalidNumberIdent => "INVALID_NUMBER_IDENT".to_string(),
        Token::InvalidFloatNoLeading => "INVALID_FLOAT_NO_LEADING".to_string(),
        Token::InvalidFloatNoTrailing => "INVALID_FLOAT_NO_TRAILING".to_string(),
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

#[rstest]
#[case::number_ident("123abc")]
fn lexer_identifier_errors(#[case] input: &str) {
    run_lexer_error_test(input);
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
#[case::float_no_leading(".5")]
#[case::float_no_trailing("5.")]
fn lexer_float_errors(#[case] input: &str) {
    run_lexer_error_test(input);
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
    run_lexer_error_test(input);
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
    run_lexer_error_test(input);
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
#[case::varref_special("${?.ok}", &["VARREF(${?.ok})"])]
#[case::varref_special_path("${?.data.x}", &["VARREF(${?.data.x})"])]
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
#[case::negated_char_class_pattern("[!a-z]", &["LBRACK", "BANG", "IDENT(a-z)", "RBRACK"])]
fn lexer_bang_operator(#[case] input: &str, #[case] expected: &[&str]) {
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

#[rstest]
#[case::triple_dash("---")]
#[case::single_dash("-")]
fn lexer_flag_errors(#[case] input: &str) {
    run_lexer_error_test(input);
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
