//! Lexer test file parser and runner.
//!
//! Parses the `tests/lexer/tokens.txt` format and runs tests against the kaish lexer.

use crate::{TestResult, TestSummary};
use kaish_kernel::lexer::{tokenize, Token};

/// A single lexer test case.
#[derive(Debug, Clone)]
pub struct LexerTestCase {
    /// Line number in the source file (1-indexed).
    pub line_number: usize,
    /// The input to lex.
    pub input: String,
    /// What we expect from lexing.
    pub expected: LexerExpectation,
}

/// What we expect from lexing an input.
#[derive(Debug, Clone, PartialEq)]
pub enum LexerExpectation {
    /// Expected tokens (as formatted strings).
    Tokens(Vec<String>),
    /// Expected error message.
    Error(String),
}

/// Parse the tokens.txt file format into test cases.
pub fn parse_lexer_tests(content: &str) -> Vec<LexerTestCase> {
    let mut cases = Vec::new();

    for (idx, line) in content.lines().enumerate() {
        let line_number = idx + 1;
        let line = line.trim();

        // Skip comments and empty lines
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        // Parse INPUT | EXPECTED format
        // The test file uses multiple spaces before the separator to distinguish from
        // pipe operators in the input (e.g., "a | b | c           | IDENT(...)").
        // We look for the pattern of multiple spaces followed by "| " as the separator.
        if let Some(sep_idx) = find_test_separator(line) {
            let input = line[..sep_idx].trim().to_string();
            let expected_str = line[sep_idx..].trim_start_matches(|c| c == ' ' || c == '|').trim();

            let expected = if expected_str.starts_with("ERROR:") {
                let msg = expected_str.strip_prefix("ERROR:").unwrap_or("").trim();
                LexerExpectation::Error(msg.to_string())
            } else {
                let tokens = parse_expected_tokens(expected_str);
                LexerExpectation::Tokens(tokens)
            };

            cases.push(LexerTestCase {
                line_number,
                input,
                expected,
            });
        }
    }

    cases
}

/// Find the separator in a test line.
/// Looks for a pattern of multiple spaces (>=2) followed by "| " to distinguish
/// the actual separator from pipe operators in the input.
fn find_test_separator(line: &str) -> Option<usize> {
    // Look for patterns like "  | " (at least 2 spaces before the pipe)
    let bytes = line.as_bytes();
    let len = bytes.len();

    for i in 0..len {
        // Look for "| " pattern
        if i + 1 < len && bytes[i] == b'|' && bytes[i + 1] == b' ' {
            // Check if preceded by at least 2 spaces
            let mut space_count = 0;
            let mut j = i;
            while j > 0 && bytes[j - 1] == b' ' {
                space_count += 1;
                j -= 1;
            }
            if space_count >= 2 {
                return Some(j);
            }
        }
    }

    // Fallback: look for " | " (single space, for simple cases)
    line.find(" | ").map(|idx| idx)
}

/// Parse expected tokens from a space-separated string.
/// Handles tokens like: `SET`, `IDENT(foo)`, `INT(5)`, `STRING(hello world)`
fn parse_expected_tokens(s: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut chars = s.chars().peekable();
    let mut current = String::new();
    let mut paren_depth = 0;

    while let Some(ch) = chars.next() {
        match ch {
            '(' => {
                paren_depth += 1;
                current.push(ch);
            }
            ')' => {
                paren_depth -= 1;
                current.push(ch);
            }
            ' ' | '\t' if paren_depth == 0 => {
                if !current.is_empty() {
                    tokens.push(current.clone());
                    current.clear();
                }
            }
            _ => {
                current.push(ch);
            }
        }
    }

    if !current.is_empty() {
        tokens.push(current);
    }

    tokens
}

/// Escape control characters for display in test output.
/// Only escapes invisible control characters, not printable chars like backslash.
fn escape_string_for_display(s: &str) -> String {
    s.replace('\n', "\\n")
        .replace('\t', "\\t")
        .replace('\r', "\\r")
}

/// Format a Token into the test file format.
pub fn format_token(token: &Token) -> String {
    match token {
        // Keywords
        Token::Set => "SET".to_string(),
        Token::Local => "LOCAL".to_string(),
        Token::Tool => "TOOL".to_string(),
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
        Token::String(s) => format!("STRING({})", escape_string_for_display(s)),
        Token::SingleString(s) => format!("SINGLESTRING({})", s),
        Token::HereDoc(s) => format!("HEREDOC({})", escape_string_for_display(s)),
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

        // Identifiers
        Token::Ident(s) => format!("IDENT({})", s),

        // Structural
        Token::Comment => "COMMENT".to_string(),
        Token::Newline => "NEWLINE".to_string(),
        Token::LineContinuation => "LINECONT".to_string(),

        // These variants should never be produced - their callbacks always return errors
        Token::InvalidNumberIdent => "INVALID_NUMBER_IDENT".to_string(),
        Token::InvalidFloatNoLeading => "INVALID_FLOAT_NO_LEADING".to_string(),
        Token::InvalidFloatNoTrailing => "INVALID_FLOAT_NO_TRAILING".to_string(),
    }
}

impl LexerTestCase {
    /// Run this test case and return the result.
    pub fn run(&self) -> TestResult {
        match tokenize(&self.input) {
            Ok(spanned_tokens) => {
                // Successfully lexed - check against expected
                let actual_tokens: Vec<String> = spanned_tokens
                    .iter()
                    .filter(|s| !matches!(s.token, Token::Newline | Token::Comment))
                    .map(|s| format_token(&s.token))
                    .collect();

                match &self.expected {
                    LexerExpectation::Tokens(expected) => {
                        if &actual_tokens == expected {
                            TestResult::Pass
                        } else {
                            TestResult::Fail {
                                expected: expected.join(" "),
                                actual: actual_tokens.join(" "),
                            }
                        }
                    }
                    LexerExpectation::Error(expected_msg) => {
                        // We expected an error but got success
                        TestResult::Fail {
                            expected: format!("ERROR: {}", expected_msg),
                            actual: actual_tokens.join(" "),
                        }
                    }
                }
            }
            Err(errors) => {
                // Lexer error
                match &self.expected {
                    LexerExpectation::Error(_expected_msg) => {
                        // We expected an error - this is a pass
                        // Note: we could match the error message more strictly
                        TestResult::Pass
                    }
                    LexerExpectation::Tokens(expected) => {
                        // We expected tokens but got an error
                        let error_msg = errors
                            .iter()
                            .map(|e| format!("{:?}", e.token))
                            .collect::<Vec<_>>()
                            .join(", ");
                        TestResult::Fail {
                            expected: expected.join(" "),
                            actual: format!("ERROR: {}", error_msg),
                        }
                    }
                }
            }
        }
    }
}

/// Run all lexer test cases and return a summary.
pub fn run_lexer_tests(cases: &[LexerTestCase]) -> TestSummary {
    let mut summary = TestSummary::new();

    for case in cases {
        let name = format!("lexer:{}", case.input);
        let result = case.run();
        summary.record(&name, case.line_number, result);
    }

    summary
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_test() {
        let content = "set | SET";
        let cases = parse_lexer_tests(content);
        assert_eq!(cases.len(), 1);
        assert_eq!(cases[0].input, "set");
        assert_eq!(cases[0].expected, LexerExpectation::Tokens(vec!["SET".to_string()]));
    }

    #[test]
    fn parse_multi_token_test() {
        let content = "set X = 5 | SET IDENT(X) EQ INT(5)";
        let cases = parse_lexer_tests(content);
        assert_eq!(cases.len(), 1);
        assert_eq!(
            cases[0].expected,
            LexerExpectation::Tokens(vec![
                "SET".to_string(),
                "IDENT(X)".to_string(),
                "EQ".to_string(),
                "INT(5)".to_string(),
            ])
        );
    }

    #[test]
    fn parse_error_test() {
        let content = "bad | ERROR: something went wrong";
        let cases = parse_lexer_tests(content);
        assert_eq!(cases.len(), 1);
        assert_eq!(
            cases[0].expected,
            LexerExpectation::Error("something went wrong".to_string())
        );
    }

    #[test]
    fn skip_comments_and_empty() {
        let content = r#"
# This is a comment
set | SET

# Another comment
        "#;
        let cases = parse_lexer_tests(content);
        assert_eq!(cases.len(), 1);
    }

    #[test]
    fn format_token_keywords() {
        assert_eq!(format_token(&Token::Set), "SET");
        assert_eq!(format_token(&Token::If), "IF");
        assert_eq!(format_token(&Token::True), "BOOL(true)");
    }

    #[test]
    fn format_token_with_values() {
        assert_eq!(format_token(&Token::Ident("foo".to_string())), "IDENT(foo)");
        assert_eq!(format_token(&Token::Int(42)), "INT(42)");
        assert_eq!(format_token(&Token::String("hello".to_string())), "STRING(hello)");
    }

    #[test]
    fn run_passing_test() {
        let case = LexerTestCase {
            line_number: 1,
            input: "set".to_string(),
            expected: LexerExpectation::Tokens(vec!["SET".to_string()]),
        };
        assert!(case.run().is_pass());
    }

    #[test]
    fn run_failing_test() {
        let case = LexerTestCase {
            line_number: 1,
            input: "set".to_string(),
            expected: LexerExpectation::Tokens(vec!["WRONG".to_string()]),
        };
        assert!(case.run().is_fail());
    }

    #[test]
    fn parse_string_with_spaces() {
        let content = r#""hello world" | STRING(hello world)"#;
        let cases = parse_lexer_tests(content);
        assert_eq!(cases.len(), 1);
        assert_eq!(
            cases[0].expected,
            LexerExpectation::Tokens(vec!["STRING(hello world)".to_string()])
        );
    }
}
