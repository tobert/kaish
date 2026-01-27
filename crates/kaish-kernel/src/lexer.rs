//! Lexer for kaish source code.
//!
//! Converts source text into a stream of tokens using the logos lexer generator.
//! The lexer is designed to be unambiguous: every valid input produces exactly
//! one token sequence, and invalid input produces clear errors.
//!
//! # Token Categories
//!
//! - **Keywords**: `set`, `tool`, `if`, `then`, `else`, `fi`, `for`, `in`, `do`, `done`
//! - **Literals**: strings, integers, floats, booleans (`true`/`false`)
//! - **Operators**: `=`, `|`, `&`, `>`, `>>`, `<`, `2>`, `&>`, `&&`, `||`
//! - **Punctuation**: `;`, `:`, `,`, `.`, `{`, `}`, `[`, `]`
//! - **Variable references**: `${...}` with nested path access
//! - **Identifiers**: command names, variable names, parameter names

use logos::{Logos, Span};
use std::fmt;

/// A token with its span in the source text.
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub token: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(token: T, span: Span) -> Self {
        Self { token, span }
    }
}

/// Lexer error types.
#[derive(Debug, Clone, PartialEq, Default)]
pub enum LexerError {
    #[default]
    UnexpectedCharacter,
    UnterminatedString,
    UnterminatedVarRef,
    InvalidEscape,
    InvalidNumber,
    AmbiguousBoolean(String),
    AmbiguousBooleanLike(String),
    InvalidNumberIdent(String),
    InvalidFloatNoLeading,
    InvalidFloatNoTrailing,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexerError::UnexpectedCharacter => write!(f, "unexpected character"),
            LexerError::UnterminatedString => write!(f, "unterminated string"),
            LexerError::UnterminatedVarRef => write!(f, "unterminated variable reference"),
            LexerError::InvalidEscape => write!(f, "invalid escape sequence"),
            LexerError::InvalidNumber => write!(f, "invalid number"),
            LexerError::AmbiguousBoolean(s) => {
                write!(f, "ambiguous boolean, use lowercase '{}'", s.to_lowercase())
            }
            LexerError::AmbiguousBooleanLike(s) => {
                let suggest = if s.eq_ignore_ascii_case("yes") { "true" } else { "false" };
                write!(f, "ambiguous boolean-like '{}', use '{}' or '\"{}\"'", s, suggest, s)
            }
            LexerError::InvalidNumberIdent(s) => {
                write!(f, "identifier cannot start with digit: {}", s)
            }
            LexerError::InvalidFloatNoLeading => write!(f, "float must have leading digit"),
            LexerError::InvalidFloatNoTrailing => write!(f, "float must have trailing digit"),
        }
    }
}

/// Tokens produced by the kaish lexer.
///
/// The order of variants matters for logos priority. More specific patterns
/// (like keywords) should come before more general ones (like identifiers).
///
/// Tokens that carry semantic values (strings, numbers, identifiers) include
/// the parsed value directly. This ensures the parser has access to actual
/// data, not just token types.
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(error = LexerError)]
#[logos(skip r"[ \t]+")]
pub enum Token {
    // ═══════════════════════════════════════════════════════════════════
    // Keywords (must come before Ident for priority)
    // ═══════════════════════════════════════════════════════════════════
    #[token("set")]
    Set,

    #[token("local")]
    Local,

    #[token("if")]
    If,

    #[token("then")]
    Then,

    #[token("else")]
    Else,

    #[token("elif")]
    Elif,

    #[token("fi")]
    Fi,

    #[token("for")]
    For,

    #[token("while")]
    While,

    #[token("in")]
    In,

    #[token("do")]
    Do,

    #[token("done")]
    Done,

    #[token("case")]
    Case,

    #[token("esac")]
    Esac,

    #[token("function")]
    Function,

    #[token("break")]
    Break,

    #[token("continue")]
    Continue,

    #[token("return")]
    Return,

    #[token("exit")]
    Exit,

    #[token("true")]
    True,

    #[token("false")]
    False,

    // ═══════════════════════════════════════════════════════════════════
    // Type keywords (for tool parameters)
    // ═══════════════════════════════════════════════════════════════════
    #[token("string")]
    TypeString,

    #[token("int")]
    TypeInt,

    #[token("float")]
    TypeFloat,

    #[token("bool")]
    TypeBool,

    // ═══════════════════════════════════════════════════════════════════
    // Multi-character operators (must come before single-char versions)
    // ═══════════════════════════════════════════════════════════════════
    #[token("&&")]
    And,

    #[token("||")]
    Or,

    #[token("==")]
    EqEq,

    #[token("!=")]
    NotEq,

    #[token("=~")]
    Match,

    #[token("!~")]
    NotMatch,

    #[token(">=")]
    GtEq,

    #[token("<=")]
    LtEq,

    #[token(">>")]
    GtGt,

    #[token("2>&1")]
    StderrToStdout,

    #[token("2>")]
    Stderr,

    #[token("&>")]
    Both,

    #[token("<<")]
    HereDocStart,

    #[token(";;")]
    DoubleSemi,

    // ═══════════════════════════════════════════════════════════════════
    // Single-character operators and punctuation
    // ═══════════════════════════════════════════════════════════════════
    #[token("=")]
    Eq,

    #[token("|")]
    Pipe,

    #[token("&")]
    Amp,

    #[token(">")]
    Gt,

    #[token("<")]
    Lt,

    #[token(";")]
    Semi,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("*")]
    Star,

    #[token("!")]
    Bang,

    #[token("?")]
    Question,

    // ═══════════════════════════════════════════════════════════════════
    // Command substitution
    // ═══════════════════════════════════════════════════════════════════

    /// Arithmetic expression content: synthesized by preprocessing.
    /// Contains the expression string between `$((` and `))`.
    Arithmetic(String),

    /// Command substitution start: `$(` - begins a command substitution
    #[token("$(")]
    CmdSubstStart,

    // ═══════════════════════════════════════════════════════════════════
    // Flags (must come before Int to win over negative numbers)
    // ═══════════════════════════════════════════════════════════════════

    /// Long flag: `--name` or `--foo-bar`
    #[regex(r"--[a-zA-Z][a-zA-Z0-9-]*", lex_long_flag, priority = 3)]
    LongFlag(String),

    /// Short flag: `-l` or `-la` (combined short flags)
    #[regex(r"-[a-zA-Z][a-zA-Z0-9]*", lex_short_flag, priority = 3)]
    ShortFlag(String),

    /// Plus flag: `+e` or `+x` (for set +e to disable options)
    #[regex(r"\+[a-zA-Z][a-zA-Z0-9]*", lex_plus_flag, priority = 3)]
    PlusFlag(String),

    /// Double dash: `--` alone marks end of flags
    #[token("--")]
    DoubleDash,

    // ═══════════════════════════════════════════════════════════════════
    // Literals (with values)
    // ═══════════════════════════════════════════════════════════════════

    /// Double-quoted string: `"..."` - value is the parsed content (quotes removed, escapes processed)
    #[regex(r#""([^"\\]|\\.)*""#, lex_string)]
    String(String),

    /// Single-quoted string: `'...'` - literal content, no escape processing
    #[regex(r"'[^']*'", lex_single_string)]
    SingleString(String),

    /// Braced variable reference: `${VAR}` or `${VAR.field}` - value is the raw inner content
    #[regex(r"\$\{[^}]+\}", lex_varref)]
    VarRef(String),

    /// Simple variable reference: `$NAME` - just the identifier
    #[regex(r"\$[a-zA-Z_][a-zA-Z0-9_]*", lex_simple_varref)]
    SimpleVarRef(String),

    /// Positional parameter: `$0` through `$9`
    #[regex(r"\$[0-9]", lex_positional)]
    Positional(usize),

    /// All positional parameters: `$@`
    #[token("$@")]
    AllArgs,

    /// Number of positional parameters: `$#`
    #[token("$#")]
    ArgCount,

    /// Variable string length: `${#VAR}`
    #[regex(r"\$\{#[a-zA-Z_][a-zA-Z0-9_]*\}", lex_var_length)]
    VarLength(String),

    /// Here-doc content: synthesized by preprocessing, not directly lexed.
    /// Contains the full content of the here-doc (without the delimiter lines).
    HereDoc(String),

    /// Integer literal - value is the parsed i64
    #[regex(r"-?[0-9]+", lex_int, priority = 2)]
    Int(i64),

    /// Float literal - value is the parsed f64
    #[regex(r"-?[0-9]+\.[0-9]+", lex_float)]
    Float(f64),

    // ═══════════════════════════════════════════════════════════════════
    // Invalid patterns (caught before valid tokens for better errors)
    // ═══════════════════════════════════════════════════════════════════

    /// Invalid: number followed by identifier characters (like 123abc)
    #[regex(r"[0-9]+[a-zA-Z_][a-zA-Z0-9_-]*", lex_invalid_number_ident, priority = 3)]
    InvalidNumberIdent,

    /// Invalid: float without leading digit (like .5)
    #[regex(r"\.[0-9]+", lex_invalid_float_no_leading, priority = 3)]
    InvalidFloatNoLeading,

    /// Invalid: float without trailing digit (like 5.)
    /// Logos uses longest-match, so valid floats like 5.5 will match Float pattern instead
    #[regex(r"[0-9]+\.", lex_invalid_float_no_trailing, priority = 2)]
    InvalidFloatNoTrailing,

    // ═══════════════════════════════════════════════════════════════════
    // Paths (absolute paths starting with /)
    // ═══════════════════════════════════════════════════════════════════

    /// Absolute path: `/tmp/out`, `/etc/hosts`, etc.
    #[regex(r"/[a-zA-Z0-9_./+-]*", lex_path)]
    Path(String),

    // ═══════════════════════════════════════════════════════════════════
    // Identifiers (command names, variable names, etc.)
    // ═══════════════════════════════════════════════════════════════════

    /// Identifier - value is the identifier string
    /// Allows dots for filenames like `script.kai`
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_.-]*", lex_ident)]
    Ident(String),

    // ═══════════════════════════════════════════════════════════════════
    // Structural tokens
    // ═══════════════════════════════════════════════════════════════════

    /// Comment: `# ...` to end of line
    #[regex(r"#[^\n\r]*")]
    Comment,

    /// Newline (significant in kaish - ends statements)
    #[regex(r"\n|\r\n")]
    Newline,

    /// Line continuation: backslash at end of line
    #[regex(r"\\[ \t]*(\n|\r\n)")]
    LineContinuation,
}

/// Lex a double-quoted string literal, processing escape sequences.
fn lex_string(lex: &mut logos::Lexer<Token>) -> Result<String, LexerError> {
    parse_string_literal(lex.slice())
}

/// Lex a single-quoted string literal (no escape processing).
fn lex_single_string(lex: &mut logos::Lexer<Token>) -> String {
    let s = lex.slice();
    // Strip the surrounding single quotes
    s[1..s.len() - 1].to_string()
}

/// Lex a braced variable reference, extracting the inner content.
fn lex_varref(lex: &mut logos::Lexer<Token>) -> String {
    // Keep the full ${...} for later parsing of path segments
    lex.slice().to_string()
}

/// Lex a simple variable reference: `$NAME` → `NAME`
fn lex_simple_varref(lex: &mut logos::Lexer<Token>) -> String {
    // Strip the leading `$`
    lex.slice()[1..].to_string()
}

/// Lex a positional parameter: `$1` → 1
fn lex_positional(lex: &mut logos::Lexer<Token>) -> usize {
    // Strip the leading `$` and parse the digit
    lex.slice()[1..].parse().unwrap_or(0)
}

/// Lex a variable length: `${#VAR}` → "VAR"
fn lex_var_length(lex: &mut logos::Lexer<Token>) -> String {
    // Strip the leading `${#` and trailing `}`
    let s = lex.slice();
    s[3..s.len() - 1].to_string()
}

/// Lex an integer literal.
fn lex_int(lex: &mut logos::Lexer<Token>) -> Result<i64, LexerError> {
    lex.slice().parse().map_err(|_| LexerError::InvalidNumber)
}

/// Lex a float literal.
fn lex_float(lex: &mut logos::Lexer<Token>) -> Result<f64, LexerError> {
    lex.slice().parse().map_err(|_| LexerError::InvalidNumber)
}

/// Lex an invalid number-identifier pattern (like 123abc).
/// Always returns Err to produce a lexer error instead of a token.
fn lex_invalid_number_ident(lex: &mut logos::Lexer<Token>) -> Result<(), LexerError> {
    Err(LexerError::InvalidNumberIdent(lex.slice().to_string()))
}

/// Lex an invalid float without leading digit (like .5).
/// Always returns Err to produce a lexer error instead of a token.
fn lex_invalid_float_no_leading(_lex: &mut logos::Lexer<Token>) -> Result<(), LexerError> {
    Err(LexerError::InvalidFloatNoLeading)
}

/// Lex an invalid float without trailing digit (like 5.).
/// Always returns Err to produce a lexer error instead of a token.
fn lex_invalid_float_no_trailing(_lex: &mut logos::Lexer<Token>) -> Result<(), LexerError> {
    Err(LexerError::InvalidFloatNoTrailing)
}

/// Lex an identifier, rejecting ambiguous boolean-like values.
fn lex_ident(lex: &mut logos::Lexer<Token>) -> Result<String, LexerError> {
    let s = lex.slice();

    // Reject ambiguous boolean variants (TRUE, FALSE, True, etc.)
    // Only lowercase 'true' and 'false' are valid booleans (handled by Token::True/False)
    match s.to_lowercase().as_str() {
        "true" | "false" if s != "true" && s != "false" => {
            return Err(LexerError::AmbiguousBoolean(s.to_string()));
        }
        _ => {}
    }

    // Reject yes/no/YES/NO/Yes/No as ambiguous boolean-like values
    if s.eq_ignore_ascii_case("yes") || s.eq_ignore_ascii_case("no") {
        return Err(LexerError::AmbiguousBooleanLike(s.to_string()));
    }

    Ok(s.to_string())
}

/// Lex a long flag: `--name` → `name`
fn lex_long_flag(lex: &mut logos::Lexer<Token>) -> String {
    // Strip the leading `--`
    lex.slice()[2..].to_string()
}

/// Lex a short flag: `-l` → `l`, `-la` → `la`
fn lex_short_flag(lex: &mut logos::Lexer<Token>) -> String {
    // Strip the leading `-`
    lex.slice()[1..].to_string()
}

/// Lex a plus flag: `+e` → `e`, `+ex` → `ex`
fn lex_plus_flag(lex: &mut logos::Lexer<Token>) -> String {
    // Strip the leading `+`
    lex.slice()[1..].to_string()
}

/// Lex an absolute path: `/tmp/out` → `/tmp/out`
fn lex_path(lex: &mut logos::Lexer<Token>) -> String {
    lex.slice().to_string()
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Set => write!(f, "set"),
            Token::Local => write!(f, "local"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::Elif => write!(f, "elif"),
            Token::Fi => write!(f, "fi"),
            Token::For => write!(f, "for"),
            Token::While => write!(f, "while"),
            Token::In => write!(f, "in"),
            Token::Do => write!(f, "do"),
            Token::Done => write!(f, "done"),
            Token::Case => write!(f, "case"),
            Token::Esac => write!(f, "esac"),
            Token::Function => write!(f, "function"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::Return => write!(f, "return"),
            Token::Exit => write!(f, "exit"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::TypeString => write!(f, "string"),
            Token::TypeInt => write!(f, "int"),
            Token::TypeFloat => write!(f, "float"),
            Token::TypeBool => write!(f, "bool"),
            Token::And => write!(f, "&&"),
            Token::Or => write!(f, "||"),
            Token::EqEq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::Match => write!(f, "=~"),
            Token::NotMatch => write!(f, "!~"),
            Token::GtEq => write!(f, ">="),
            Token::LtEq => write!(f, "<="),
            Token::GtGt => write!(f, ">>"),
            Token::StderrToStdout => write!(f, "2>&1"),
            Token::Stderr => write!(f, "2>"),
            Token::Both => write!(f, "&>"),
            Token::HereDocStart => write!(f, "<<"),
            Token::DoubleSemi => write!(f, ";;"),
            Token::Eq => write!(f, "="),
            Token::Pipe => write!(f, "|"),
            Token::Amp => write!(f, "&"),
            Token::Gt => write!(f, ">"),
            Token::Lt => write!(f, "<"),
            Token::Semi => write!(f, ";"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Star => write!(f, "*"),
            Token::Bang => write!(f, "!"),
            Token::Question => write!(f, "?"),
            Token::Arithmetic(s) => write!(f, "ARITHMETIC({})", s),
            Token::CmdSubstStart => write!(f, "$("),
            Token::LongFlag(s) => write!(f, "--{}", s),
            Token::ShortFlag(s) => write!(f, "-{}", s),
            Token::PlusFlag(s) => write!(f, "+{}", s),
            Token::DoubleDash => write!(f, "--"),
            Token::String(s) => write!(f, "STRING({:?})", s),
            Token::SingleString(s) => write!(f, "SINGLESTRING({:?})", s),
            Token::HereDoc(s) => write!(f, "HEREDOC({:?})", s),
            Token::VarRef(v) => write!(f, "VARREF({})", v),
            Token::SimpleVarRef(v) => write!(f, "SIMPLEVARREF({})", v),
            Token::Positional(n) => write!(f, "${}", n),
            Token::AllArgs => write!(f, "$@"),
            Token::ArgCount => write!(f, "$#"),
            Token::VarLength(v) => write!(f, "${{#{}}}", v),
            Token::Int(n) => write!(f, "INT({})", n),
            Token::Float(n) => write!(f, "FLOAT({})", n),
            Token::Path(s) => write!(f, "PATH({})", s),
            Token::Ident(s) => write!(f, "IDENT({})", s),
            Token::Comment => write!(f, "COMMENT"),
            Token::Newline => write!(f, "NEWLINE"),
            Token::LineContinuation => write!(f, "LINECONT"),
            // These variants should never be produced - their callbacks always return errors
            Token::InvalidNumberIdent => write!(f, "INVALID_NUMBER_IDENT"),
            Token::InvalidFloatNoLeading => write!(f, "INVALID_FLOAT_NO_LEADING"),
            Token::InvalidFloatNoTrailing => write!(f, "INVALID_FLOAT_NO_TRAILING"),
        }
    }
}

impl Token {
    /// Returns true if this token is a keyword.
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            Token::Set
                | Token::Local
                | Token::If
                | Token::Then
                | Token::Else
                | Token::Elif
                | Token::Fi
                | Token::For
                | Token::In
                | Token::Do
                | Token::Done
                | Token::Case
                | Token::Esac
                | Token::Function
                | Token::True
                | Token::False
        )
    }

    /// Returns true if this token is a type keyword.
    pub fn is_type(&self) -> bool {
        matches!(
            self,
            Token::TypeString
                | Token::TypeInt
                | Token::TypeFloat
                | Token::TypeBool
        )
    }

    /// Returns true if this token starts a statement.
    pub fn starts_statement(&self) -> bool {
        matches!(
            self,
            Token::Set | Token::Local | Token::Function | Token::If | Token::For | Token::Case | Token::Ident(_) | Token::LBracket
        )
    }

    /// Returns true if this token can appear in an expression.
    pub fn is_value(&self) -> bool {
        matches!(
            self,
            Token::String(_)
                | Token::SingleString(_)
                | Token::HereDoc(_)
                | Token::Arithmetic(_)
                | Token::Int(_)
                | Token::Float(_)
                | Token::True
                | Token::False
                | Token::VarRef(_)
                | Token::SimpleVarRef(_)
                | Token::CmdSubstStart
                | Token::Path(_)
        )
    }
}

/// Preprocess arithmetic expressions in source code.
///
/// Finds `$((expr))` patterns and replaces them with markers.
/// Returns the preprocessed source and a vector of (marker, content) pairs.
///
/// Example:
///   `X=$((1 + 2))`
/// Becomes:
///   `X=__ARITH_0__`
/// With arithmetic[0] = ("__ARITH_0__", "1 + 2")
fn preprocess_arithmetic(source: &str) -> (String, Vec<(String, String)>) {
    let mut result = String::with_capacity(source.len());
    let mut arithmetics: Vec<(String, String)> = Vec::new();
    let mut chars = source.chars().peekable();
    let mut arith_count = 0;

    while let Some(ch) = chars.next() {
        // Look for $(( (potential arithmetic)
        if ch == '$' && chars.peek() == Some(&'(') {
            // Peek ahead to see if it's $((
            let mut peek_chars = chars.clone();
            peek_chars.next(); // consume first (
            if peek_chars.peek() == Some(&'(') {
                // This is arithmetic $((
                chars.next(); // consume first (
                chars.next(); // consume second (

                // Collect expression until matching ))
                let mut expr = String::new();
                let mut paren_depth = 0;

                loop {
                    match chars.next() {
                        Some('(') => {
                            paren_depth += 1;
                            expr.push('(');
                        }
                        Some(')') => {
                            if paren_depth > 0 {
                                paren_depth -= 1;
                                expr.push(')');
                            } else {
                                // Check for second )
                                if chars.peek() == Some(&')') {
                                    chars.next(); // consume second )
                                    break;
                                } else {
                                    // Single ) inside - keep going
                                    expr.push(')');
                                }
                            }
                        }
                        Some(c) => expr.push(c),
                        None => {
                            // EOF without closing ))
                            break;
                        }
                    }
                }

                // Create a marker for this arithmetic
                let marker = format!("__ARITH_{}__", arith_count);
                arith_count += 1;
                arithmetics.push((marker.clone(), expr));

                // Output marker
                result.push_str(&marker);
            } else {
                // This is command substitution $( - output normally
                result.push(ch);
            }
        } else {
            result.push(ch);
        }
    }

    (result, arithmetics)
}

/// Preprocess here-docs in source code.
///
/// Finds `<<WORD` patterns and collects content until the delimiter line.
/// Returns the preprocessed source and a vector of (marker, content) pairs.
///
/// Example:
///   `cat <<EOF\nhello\nworld\nEOF`
/// Becomes:
///   `cat <<__HEREDOC_0__`
/// With heredocs[0] = ("__HEREDOC_0__", "hello\nworld")
fn preprocess_heredocs(source: &str) -> (String, Vec<(String, String)>) {
    let mut result = String::with_capacity(source.len());
    let mut heredocs: Vec<(String, String)> = Vec::new();
    let mut chars = source.chars().peekable();
    let mut heredoc_count = 0;

    while let Some(ch) = chars.next() {
        // Look for << (potential here-doc)
        if ch == '<' && chars.peek() == Some(&'<') {
            chars.next(); // consume second <

            // Check for optional - (strip leading tabs)
            let strip_tabs = chars.peek() == Some(&'-');
            if strip_tabs {
                chars.next();
            }

            // Skip whitespace before delimiter
            while let Some(&c) = chars.peek() {
                if c == ' ' || c == '\t' {
                    chars.next();
                } else {
                    break;
                }
            }

            // Collect the delimiter word
            let mut delimiter = String::new();
            let quoted = chars.peek() == Some(&'\'') || chars.peek() == Some(&'"');
            let quote_char = if quoted { chars.next() } else { None };

            while let Some(&c) = chars.peek() {
                if quoted {
                    if Some(c) == quote_char {
                        chars.next(); // consume closing quote
                        break;
                    }
                } else if c.is_whitespace() || c == '\n' || c == '\r' {
                    break;
                }
                if let Some(ch) = chars.next() {
                    delimiter.push(ch);
                }
            }

            if delimiter.is_empty() {
                // Not a valid here-doc, output << literally
                result.push_str("<<");
                if strip_tabs {
                    result.push('-');
                }
                continue;
            }

            // Skip to newline
            while let Some(&c) = chars.peek() {
                if c == '\n' {
                    chars.next();
                    break;
                } else if c == '\r' {
                    chars.next();
                    if chars.peek() == Some(&'\n') {
                        chars.next();
                    }
                    break;
                }
                if let Some(ch) = chars.next() {
                    result.push(ch);
                }
            }

            // Collect content until delimiter on its own line
            let mut content = String::new();
            let mut current_line = String::new();

            loop {
                match chars.next() {
                    Some('\n') => {
                        // Check if this line is the delimiter
                        let trimmed = if strip_tabs {
                            current_line.trim_start_matches('\t')
                        } else {
                            &current_line
                        };
                        if trimmed == delimiter {
                            // Found end of here-doc
                            break;
                        }
                        // Add line to content
                        if !content.is_empty() || !current_line.is_empty() {
                            content.push_str(&current_line);
                            content.push('\n');
                        }
                        current_line.clear();
                    }
                    Some('\r') => {
                        // Handle \r\n
                        if chars.peek() == Some(&'\n') {
                            chars.next();
                        }
                        let trimmed = if strip_tabs {
                            current_line.trim_start_matches('\t')
                        } else {
                            &current_line
                        };
                        if trimmed == delimiter {
                            break;
                        }
                        if !content.is_empty() || !current_line.is_empty() {
                            content.push_str(&current_line);
                            content.push('\n');
                        }
                        current_line.clear();
                    }
                    Some(c) => {
                        current_line.push(c);
                    }
                    None => {
                        // EOF - check if current line is the delimiter
                        let trimmed = if strip_tabs {
                            current_line.trim_start_matches('\t')
                        } else {
                            &current_line
                        };
                        if trimmed == delimiter {
                            // Found delimiter at EOF
                            break;
                        }
                        // Not a delimiter - include remaining content
                        if !current_line.is_empty() {
                            content.push_str(&current_line);
                        }
                        break;
                    }
                }
            }

            // Remove trailing newline from content (we'll add it when needed)
            let content = content.trim_end_matches('\n').to_string();

            // Create a marker for this here-doc
            let marker = format!("__HEREDOC_{}__", heredoc_count);
            heredoc_count += 1;
            heredocs.push((marker.clone(), content));

            // Output << and marker
            result.push_str("<<");
            result.push_str(&marker);
            result.push('\n'); // Preserve newline after here-doc
        } else {
            result.push(ch);
        }
    }

    (result, heredocs)
}

/// Tokenize source code into a vector of spanned tokens.
///
/// Skips whitespace and comments (unless you need them for formatting).
/// Returns errors with their positions for nice error messages.
///
/// Handles:
/// - Arithmetic: `$((expr))` becomes `Arithmetic("expr")`
/// - Here-docs: `<<EOF\nhello\nEOF` becomes `HereDocStart` + `HereDoc("hello")`
pub fn tokenize(source: &str) -> Result<Vec<Spanned<Token>>, Vec<Spanned<LexerError>>> {
    // Preprocess arithmetic first (before heredocs because heredoc content might contain $((
    let (preprocessed, arithmetics) = preprocess_arithmetic(source);

    // Then preprocess here-docs
    let (preprocessed, heredocs) = preprocess_heredocs(&preprocessed);

    let lexer = Token::lexer(&preprocessed);
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    for (result, span) in lexer.spanned() {
        match result {
            Ok(token) => {
                // Skip comments and line continuations - they're not needed for parsing
                if !matches!(token, Token::Comment | Token::LineContinuation) {
                    tokens.push(Spanned::new(token, span));
                }
            }
            Err(err) => {
                errors.push(Spanned::new(err, span));
            }
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    // Post-process: replace markers with actual token content
    let mut final_tokens = Vec::with_capacity(tokens.len());
    let mut i = 0;

    while i < tokens.len() {
        // Check for arithmetic marker
        if let Token::Ident(ref name) = tokens[i].token
            && name.starts_with("__ARITH_") && name.ends_with("__")
                && let Some((_, expr)) = arithmetics.iter().find(|(marker, _)| marker == name) {
                    final_tokens.push(Spanned::new(Token::Arithmetic(expr.clone()), tokens[i].span.clone()));
                    i += 1;
                    continue;
                }

        // Check for heredoc
        if matches!(tokens[i].token, Token::HereDocStart) {
            // Check if next token is a heredoc marker
            if i + 1 < tokens.len()
                && let Token::Ident(ref name) = tokens[i + 1].token
                    && name.starts_with("__HEREDOC_") && name.ends_with("__") {
                        // Find the corresponding content
                        if let Some((_, content)) = heredocs.iter().find(|(marker, _)| marker == name) {
                            final_tokens.push(Spanned::new(Token::HereDocStart, tokens[i].span.clone()));
                            final_tokens.push(Spanned::new(Token::HereDoc(content.clone()), tokens[i + 1].span.clone()));
                            i += 2;
                            continue;
                        }
                    }
        }
        final_tokens.push(tokens[i].clone());
        i += 1;
    }

    Ok(final_tokens)
}

/// Tokenize source code, preserving comments.
///
/// Useful for pretty-printing or formatting tools that need to preserve comments.
pub fn tokenize_with_comments(source: &str) -> Result<Vec<Spanned<Token>>, Vec<Spanned<LexerError>>> {
    let lexer = Token::lexer(source);
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    for (result, span) in lexer.spanned() {
        match result {
            Ok(token) => {
                tokens.push(Spanned::new(token, span));
            }
            Err(err) => {
                errors.push(Spanned::new(err, span));
            }
        }
    }

    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(errors)
    }
}

/// Extract the string content from a string token (removes quotes, processes escapes).
pub fn parse_string_literal(source: &str) -> Result<String, LexerError> {
    // Remove surrounding quotes
    if source.len() < 2 || !source.starts_with('"') || !source.ends_with('"') {
        return Err(LexerError::UnterminatedString);
    }

    let inner = &source[1..source.len() - 1];
    let mut result = String::with_capacity(inner.len());
    let mut chars = inner.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some('u') => {
                    // Unicode escape: \uXXXX
                    let mut hex = String::with_capacity(4);
                    for _ in 0..4 {
                        match chars.next() {
                            Some(h) if h.is_ascii_hexdigit() => hex.push(h),
                            _ => return Err(LexerError::InvalidEscape),
                        }
                    }
                    let codepoint = u32::from_str_radix(&hex, 16)
                        .map_err(|_| LexerError::InvalidEscape)?;
                    let ch = char::from_u32(codepoint)
                        .ok_or(LexerError::InvalidEscape)?;
                    result.push(ch);
                }
                // Unknown escapes: preserve the backslash (for regex patterns like `\.`)
                Some(next) => {
                    result.push('\\');
                    result.push(next);
                }
                None => return Err(LexerError::InvalidEscape),
            }
        } else {
            result.push(ch);
        }
    }

    Ok(result)
}

/// Parse a variable reference, extracting the path segments.
/// Input: "${VAR.field[0].nested}" → ["VAR", "field", "[0]", "nested"]
pub fn parse_var_ref(source: &str) -> Result<Vec<String>, LexerError> {
    // Remove ${ and }
    if source.len() < 4 || !source.starts_with("${") || !source.ends_with('}') {
        return Err(LexerError::UnterminatedVarRef);
    }

    let inner = &source[2..source.len() - 1];

    // Special case: $? (last result)
    if inner == "?" {
        return Ok(vec!["?".to_string()]);
    }

    let mut segments = Vec::new();
    let mut current = String::new();
    let mut chars = inner.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            '.' => {
                if !current.is_empty() {
                    segments.push(current.clone());
                    current.clear();
                }
            }
            '[' => {
                if !current.is_empty() {
                    segments.push(current.clone());
                    current.clear();
                }
                // Collect the index
                let mut index = String::from("[");
                while let Some(&c) = chars.peek() {
                    index.push(chars.next().expect("peeked char should exist"));
                    if c == ']' {
                        break;
                    }
                }
                segments.push(index);
            }
            _ => {
                current.push(ch);
            }
        }
    }

    if !current.is_empty() {
        segments.push(current);
    }

    Ok(segments)
}

/// Parse an integer literal.
pub fn parse_int(source: &str) -> Result<i64, LexerError> {
    source.parse().map_err(|_| LexerError::InvalidNumber)
}

/// Parse a float literal.
pub fn parse_float(source: &str) -> Result<f64, LexerError> {
    source.parse().map_err(|_| LexerError::InvalidNumber)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(source: &str) -> Vec<Token> {
        tokenize(source)
            .expect("lexer should succeed")
            .into_iter()
            .map(|s| s.token)
            .collect()
    }

    // ═══════════════════════════════════════════════════════════════════
    // Keyword tests
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn keywords() {
        assert_eq!(lex("set"), vec![Token::Set]);
        assert_eq!(lex("if"), vec![Token::If]);
        assert_eq!(lex("then"), vec![Token::Then]);
        assert_eq!(lex("else"), vec![Token::Else]);
        assert_eq!(lex("elif"), vec![Token::Elif]);
        assert_eq!(lex("fi"), vec![Token::Fi]);
        assert_eq!(lex("for"), vec![Token::For]);
        assert_eq!(lex("in"), vec![Token::In]);
        assert_eq!(lex("do"), vec![Token::Do]);
        assert_eq!(lex("done"), vec![Token::Done]);
        assert_eq!(lex("case"), vec![Token::Case]);
        assert_eq!(lex("esac"), vec![Token::Esac]);
        assert_eq!(lex("function"), vec![Token::Function]);
        assert_eq!(lex("true"), vec![Token::True]);
        assert_eq!(lex("false"), vec![Token::False]);
    }

    #[test]
    fn double_semicolon() {
        assert_eq!(lex(";;"), vec![Token::DoubleSemi]);
        // In case pattern context
        assert_eq!(lex("echo \"hi\";;"), vec![
            Token::Ident("echo".to_string()),
            Token::String("hi".to_string()),
            Token::DoubleSemi,
        ]);
    }

    #[test]
    fn type_keywords() {
        assert_eq!(lex("string"), vec![Token::TypeString]);
        assert_eq!(lex("int"), vec![Token::TypeInt]);
        assert_eq!(lex("float"), vec![Token::TypeFloat]);
        assert_eq!(lex("bool"), vec![Token::TypeBool]);
    }

    // ═══════════════════════════════════════════════════════════════════
    // Operator tests
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn single_char_operators() {
        assert_eq!(lex("="), vec![Token::Eq]);
        assert_eq!(lex("|"), vec![Token::Pipe]);
        assert_eq!(lex("&"), vec![Token::Amp]);
        assert_eq!(lex(">"), vec![Token::Gt]);
        assert_eq!(lex("<"), vec![Token::Lt]);
        assert_eq!(lex(";"), vec![Token::Semi]);
        assert_eq!(lex(":"), vec![Token::Colon]);
        assert_eq!(lex(","), vec![Token::Comma]);
        assert_eq!(lex("."), vec![Token::Dot]);
    }

    #[test]
    fn multi_char_operators() {
        assert_eq!(lex("&&"), vec![Token::And]);
        assert_eq!(lex("||"), vec![Token::Or]);
        assert_eq!(lex("=="), vec![Token::EqEq]);
        assert_eq!(lex("!="), vec![Token::NotEq]);
        assert_eq!(lex("=~"), vec![Token::Match]);
        assert_eq!(lex("!~"), vec![Token::NotMatch]);
        assert_eq!(lex(">="), vec![Token::GtEq]);
        assert_eq!(lex("<="), vec![Token::LtEq]);
        assert_eq!(lex(">>"), vec![Token::GtGt]);
        assert_eq!(lex("2>"), vec![Token::Stderr]);
        assert_eq!(lex("&>"), vec![Token::Both]);
    }

    #[test]
    fn brackets() {
        assert_eq!(lex("{"), vec![Token::LBrace]);
        assert_eq!(lex("}"), vec![Token::RBrace]);
        assert_eq!(lex("["), vec![Token::LBracket]);
        assert_eq!(lex("]"), vec![Token::RBracket]);
        assert_eq!(lex("("), vec![Token::LParen]);
        assert_eq!(lex(")"), vec![Token::RParen]);
    }

    // ═══════════════════════════════════════════════════════════════════
    // Literal tests
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn integers() {
        assert_eq!(lex("0"), vec![Token::Int(0)]);
        assert_eq!(lex("42"), vec![Token::Int(42)]);
        assert_eq!(lex("-1"), vec![Token::Int(-1)]);
        assert_eq!(lex("999999"), vec![Token::Int(999999)]);
    }

    #[test]
    fn floats() {
        assert_eq!(lex("3.14"), vec![Token::Float(3.14)]);
        assert_eq!(lex("-0.5"), vec![Token::Float(-0.5)]);
        assert_eq!(lex("123.456"), vec![Token::Float(123.456)]);
    }

    #[test]
    fn strings() {
        assert_eq!(lex(r#""hello""#), vec![Token::String("hello".to_string())]);
        assert_eq!(lex(r#""hello world""#), vec![Token::String("hello world".to_string())]);
        assert_eq!(lex(r#""""#), vec![Token::String("".to_string())]); // empty string
        assert_eq!(lex(r#""with \"quotes\"""#), vec![Token::String("with \"quotes\"".to_string())]);
        assert_eq!(lex(r#""with\nnewline""#), vec![Token::String("with\nnewline".to_string())]);
    }

    #[test]
    fn var_refs() {
        assert_eq!(lex("${X}"), vec![Token::VarRef("${X}".to_string())]);
        assert_eq!(lex("${VAR}"), vec![Token::VarRef("${VAR}".to_string())]);
        assert_eq!(lex("${VAR.field}"), vec![Token::VarRef("${VAR.field}".to_string())]);
        assert_eq!(lex("${VAR[0]}"), vec![Token::VarRef("${VAR[0]}".to_string())]);
        assert_eq!(lex("${?.ok}"), vec![Token::VarRef("${?.ok}".to_string())]);
    }

    // ═══════════════════════════════════════════════════════════════════
    // Identifier tests
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn identifiers() {
        assert_eq!(lex("foo"), vec![Token::Ident("foo".to_string())]);
        assert_eq!(lex("foo_bar"), vec![Token::Ident("foo_bar".to_string())]);
        assert_eq!(lex("foo-bar"), vec![Token::Ident("foo-bar".to_string())]);
        assert_eq!(lex("_private"), vec![Token::Ident("_private".to_string())]);
        assert_eq!(lex("cmd123"), vec![Token::Ident("cmd123".to_string())]);
    }

    #[test]
    fn keyword_prefix_identifiers() {
        // Identifiers that start with keywords but aren't keywords
        assert_eq!(lex("setup"), vec![Token::Ident("setup".to_string())]);
        assert_eq!(lex("tools"), vec![Token::Ident("tools".to_string())]);
        assert_eq!(lex("iffy"), vec![Token::Ident("iffy".to_string())]);
        assert_eq!(lex("forked"), vec![Token::Ident("forked".to_string())]);
        assert_eq!(lex("done-with-it"), vec![Token::Ident("done-with-it".to_string())]);
    }

    // ═══════════════════════════════════════════════════════════════════
    // Statement tests
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn assignment() {
        assert_eq!(
            lex("set X = 5"),
            vec![Token::Set, Token::Ident("X".to_string()), Token::Eq, Token::Int(5)]
        );
    }

    #[test]
    fn command_simple() {
        assert_eq!(lex("echo"), vec![Token::Ident("echo".to_string())]);
        assert_eq!(
            lex(r#"echo "hello""#),
            vec![Token::Ident("echo".to_string()), Token::String("hello".to_string())]
        );
    }

    #[test]
    fn command_with_args() {
        assert_eq!(
            lex("cmd arg1 arg2"),
            vec![Token::Ident("cmd".to_string()), Token::Ident("arg1".to_string()), Token::Ident("arg2".to_string())]
        );
    }

    #[test]
    fn command_with_named_args() {
        assert_eq!(
            lex("cmd key=value"),
            vec![Token::Ident("cmd".to_string()), Token::Ident("key".to_string()), Token::Eq, Token::Ident("value".to_string())]
        );
    }

    #[test]
    fn pipeline() {
        assert_eq!(
            lex("a | b | c"),
            vec![Token::Ident("a".to_string()), Token::Pipe, Token::Ident("b".to_string()), Token::Pipe, Token::Ident("c".to_string())]
        );
    }

    #[test]
    fn if_statement() {
        assert_eq!(
            lex("if true; then echo; fi"),
            vec![
                Token::If,
                Token::True,
                Token::Semi,
                Token::Then,
                Token::Ident("echo".to_string()),
                Token::Semi,
                Token::Fi
            ]
        );
    }

    #[test]
    fn for_loop() {
        assert_eq!(
            lex("for X in items; do echo; done"),
            vec![
                Token::For,
                Token::Ident("X".to_string()),
                Token::In,
                Token::Ident("items".to_string()),
                Token::Semi,
                Token::Do,
                Token::Ident("echo".to_string()),
                Token::Semi,
                Token::Done
            ]
        );
    }

    // ═══════════════════════════════════════════════════════════════════
    // Whitespace and newlines
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn whitespace_ignored() {
        assert_eq!(lex("   set   X   =   5   "), lex("set X = 5"));
    }

    #[test]
    fn newlines_preserved() {
        let tokens = lex("a\nb");
        assert_eq!(
            tokens,
            vec![Token::Ident("a".to_string()), Token::Newline, Token::Ident("b".to_string())]
        );
    }

    #[test]
    fn multiple_newlines() {
        let tokens = lex("a\n\n\nb");
        assert_eq!(
            tokens,
            vec![Token::Ident("a".to_string()), Token::Newline, Token::Newline, Token::Newline, Token::Ident("b".to_string())]
        );
    }

    // ═══════════════════════════════════════════════════════════════════
    // Comments
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn comments_skipped() {
        assert_eq!(lex("# comment"), vec![]);
        assert_eq!(lex("a # comment"), vec![Token::Ident("a".to_string())]);
        assert_eq!(
            lex("a # comment\nb"),
            vec![Token::Ident("a".to_string()), Token::Newline, Token::Ident("b".to_string())]
        );
    }

    #[test]
    fn comments_preserved_when_requested() {
        let tokens = tokenize_with_comments("a # comment")
            .expect("should succeed")
            .into_iter()
            .map(|s| s.token)
            .collect::<Vec<_>>();
        assert_eq!(tokens, vec![Token::Ident("a".to_string()), Token::Comment]);
    }

    // ═══════════════════════════════════════════════════════════════════
    // String parsing
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn parse_simple_string() {
        assert_eq!(parse_string_literal(r#""hello""#).expect("ok"), "hello");
    }

    #[test]
    fn parse_string_with_escapes() {
        assert_eq!(
            parse_string_literal(r#""hello\nworld""#).expect("ok"),
            "hello\nworld"
        );
        assert_eq!(
            parse_string_literal(r#""tab\there""#).expect("ok"),
            "tab\there"
        );
        assert_eq!(
            parse_string_literal(r#""quote\"here""#).expect("ok"),
            "quote\"here"
        );
    }

    #[test]
    fn parse_string_with_unicode() {
        assert_eq!(
            parse_string_literal(r#""emoji \u2764""#).expect("ok"),
            "emoji ❤"
        );
    }

    // ═══════════════════════════════════════════════════════════════════
    // Variable reference parsing
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn parse_simple_var() {
        assert_eq!(
            parse_var_ref("${X}").expect("ok"),
            vec!["X"]
        );
    }

    #[test]
    fn parse_var_with_field() {
        assert_eq!(
            parse_var_ref("${VAR.field}").expect("ok"),
            vec!["VAR", "field"]
        );
    }

    #[test]
    fn parse_var_with_index() {
        assert_eq!(
            parse_var_ref("${VAR[0]}").expect("ok"),
            vec!["VAR", "[0]"]
        );
    }

    #[test]
    fn parse_var_nested() {
        assert_eq!(
            parse_var_ref("${VAR.field[0].nested}").expect("ok"),
            vec!["VAR", "field", "[0]", "nested"]
        );
    }

    #[test]
    fn parse_last_result() {
        assert_eq!(
            parse_var_ref("${?}").expect("ok"),
            vec!["?"]
        );
        assert_eq!(
            parse_var_ref("${?.ok}").expect("ok"),
            vec!["?", "ok"]
        );
    }

    // ═══════════════════════════════════════════════════════════════════
    // Number parsing
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn parse_integers() {
        assert_eq!(parse_int("0").expect("ok"), 0);
        assert_eq!(parse_int("42").expect("ok"), 42);
        assert_eq!(parse_int("-1").expect("ok"), -1);
    }

    #[test]
    fn parse_floats() {
        assert!((parse_float("3.14").expect("ok") - 3.14).abs() < f64::EPSILON);
        assert!((parse_float("-0.5").expect("ok") - (-0.5)).abs() < f64::EPSILON);
    }

    // ═══════════════════════════════════════════════════════════════════
    // Edge cases and errors
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn empty_input() {
        assert_eq!(lex(""), vec![]);
    }

    #[test]
    fn only_whitespace() {
        assert_eq!(lex("   \t\t   "), vec![]);
    }

    #[test]
    fn json_array() {
        assert_eq!(
            lex(r#"[1, 2, 3]"#),
            vec![
                Token::LBracket,
                Token::Int(1),
                Token::Comma,
                Token::Int(2),
                Token::Comma,
                Token::Int(3),
                Token::RBracket
            ]
        );
    }

    #[test]
    fn json_object() {
        assert_eq!(
            lex(r#"{"key": "value"}"#),
            vec![
                Token::LBrace,
                Token::String("key".to_string()),
                Token::Colon,
                Token::String("value".to_string()),
                Token::RBrace
            ]
        );
    }

    #[test]
    fn redirect_operators() {
        assert_eq!(
            lex("cmd > file"),
            vec![Token::Ident("cmd".to_string()), Token::Gt, Token::Ident("file".to_string())]
        );
        assert_eq!(
            lex("cmd >> file"),
            vec![Token::Ident("cmd".to_string()), Token::GtGt, Token::Ident("file".to_string())]
        );
        assert_eq!(
            lex("cmd 2> err"),
            vec![Token::Ident("cmd".to_string()), Token::Stderr, Token::Ident("err".to_string())]
        );
        assert_eq!(
            lex("cmd &> all"),
            vec![Token::Ident("cmd".to_string()), Token::Both, Token::Ident("all".to_string())]
        );
    }

    #[test]
    fn background_job() {
        assert_eq!(
            lex("cmd &"),
            vec![Token::Ident("cmd".to_string()), Token::Amp]
        );
    }

    #[test]
    fn command_substitution() {
        assert_eq!(
            lex("$(cmd)"),
            vec![Token::CmdSubstStart, Token::Ident("cmd".to_string()), Token::RParen]
        );
        assert_eq!(
            lex("$(cmd arg)"),
            vec![
                Token::CmdSubstStart,
                Token::Ident("cmd".to_string()),
                Token::Ident("arg".to_string()),
                Token::RParen
            ]
        );
        assert_eq!(
            lex("$(a | b)"),
            vec![
                Token::CmdSubstStart,
                Token::Ident("a".to_string()),
                Token::Pipe,
                Token::Ident("b".to_string()),
                Token::RParen
            ]
        );
    }

    #[test]
    fn complex_pipeline() {
        assert_eq!(
            lex(r#"cat file | grep pattern="foo" | head count=10"#),
            vec![
                Token::Ident("cat".to_string()),
                Token::Ident("file".to_string()),
                Token::Pipe,
                Token::Ident("grep".to_string()),
                Token::Ident("pattern".to_string()),
                Token::Eq,
                Token::String("foo".to_string()),
                Token::Pipe,
                Token::Ident("head".to_string()),
                Token::Ident("count".to_string()),
                Token::Eq,
                Token::Int(10),
            ]
        );
    }

    // ═══════════════════════════════════════════════════════════════════
    // Flag tests
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn short_flag() {
        assert_eq!(lex("-l"), vec![Token::ShortFlag("l".to_string())]);
        assert_eq!(lex("-a"), vec![Token::ShortFlag("a".to_string())]);
        assert_eq!(lex("-v"), vec![Token::ShortFlag("v".to_string())]);
    }

    #[test]
    fn short_flag_combined() {
        // Combined short flags like -la
        assert_eq!(lex("-la"), vec![Token::ShortFlag("la".to_string())]);
        assert_eq!(lex("-vvv"), vec![Token::ShortFlag("vvv".to_string())]);
    }

    #[test]
    fn long_flag() {
        assert_eq!(lex("--force"), vec![Token::LongFlag("force".to_string())]);
        assert_eq!(lex("--verbose"), vec![Token::LongFlag("verbose".to_string())]);
        assert_eq!(lex("--foo-bar"), vec![Token::LongFlag("foo-bar".to_string())]);
    }

    #[test]
    fn double_dash() {
        // -- alone marks end of flags
        assert_eq!(lex("--"), vec![Token::DoubleDash]);
    }

    #[test]
    fn flags_vs_negative_numbers() {
        // -123 should be a negative integer, not a flag
        assert_eq!(lex("-123"), vec![Token::Int(-123)]);
        // -l should be a flag
        assert_eq!(lex("-l"), vec![Token::ShortFlag("l".to_string())]);
        // -1a is ambiguous - should be Int(-1) then Ident(a)
        // Actually the regex -[a-zA-Z] won't match -1a since 1 isn't a letter
        assert_eq!(
            lex("-1 a"),
            vec![Token::Int(-1), Token::Ident("a".to_string())]
        );
    }

    #[test]
    fn command_with_flags() {
        assert_eq!(
            lex("ls -l"),
            vec![
                Token::Ident("ls".to_string()),
                Token::ShortFlag("l".to_string()),
            ]
        );
        assert_eq!(
            lex("git commit -m"),
            vec![
                Token::Ident("git".to_string()),
                Token::Ident("commit".to_string()),
                Token::ShortFlag("m".to_string()),
            ]
        );
        assert_eq!(
            lex("git push --force"),
            vec![
                Token::Ident("git".to_string()),
                Token::Ident("push".to_string()),
                Token::LongFlag("force".to_string()),
            ]
        );
    }

    #[test]
    fn flag_with_value() {
        assert_eq!(
            lex(r#"git commit -m "message""#),
            vec![
                Token::Ident("git".to_string()),
                Token::Ident("commit".to_string()),
                Token::ShortFlag("m".to_string()),
                Token::String("message".to_string()),
            ]
        );
        assert_eq!(
            lex(r#"--message="hello""#),
            vec![
                Token::LongFlag("message".to_string()),
                Token::Eq,
                Token::String("hello".to_string()),
            ]
        );
    }

    #[test]
    fn end_of_flags_marker() {
        assert_eq!(
            lex("git checkout -- file"),
            vec![
                Token::Ident("git".to_string()),
                Token::Ident("checkout".to_string()),
                Token::DoubleDash,
                Token::Ident("file".to_string()),
            ]
        );
    }

    // ═══════════════════════════════════════════════════════════════════
    // Bash compatibility tokens
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn local_keyword() {
        assert_eq!(lex("local"), vec![Token::Local]);
        assert_eq!(
            lex("local X = 5"),
            vec![Token::Local, Token::Ident("X".to_string()), Token::Eq, Token::Int(5)]
        );
    }

    #[test]
    fn simple_var_ref() {
        assert_eq!(lex("$X"), vec![Token::SimpleVarRef("X".to_string())]);
        assert_eq!(lex("$foo"), vec![Token::SimpleVarRef("foo".to_string())]);
        assert_eq!(lex("$foo_bar"), vec![Token::SimpleVarRef("foo_bar".to_string())]);
        assert_eq!(lex("$_private"), vec![Token::SimpleVarRef("_private".to_string())]);
    }

    #[test]
    fn simple_var_ref_in_command() {
        assert_eq!(
            lex("echo $NAME"),
            vec![Token::Ident("echo".to_string()), Token::SimpleVarRef("NAME".to_string())]
        );
    }

    #[test]
    fn single_quoted_strings() {
        assert_eq!(lex("'hello'"), vec![Token::SingleString("hello".to_string())]);
        assert_eq!(lex("'hello world'"), vec![Token::SingleString("hello world".to_string())]);
        assert_eq!(lex("''"), vec![Token::SingleString("".to_string())]);
        // Single quotes don't process escapes or variables
        assert_eq!(lex(r"'no $VAR here'"), vec![Token::SingleString("no $VAR here".to_string())]);
        assert_eq!(lex(r"'backslash \n stays'"), vec![Token::SingleString(r"backslash \n stays".to_string())]);
    }

    #[test]
    fn test_brackets() {
        // [[ and ]] are now two separate bracket tokens to avoid conflicts with nested arrays
        assert_eq!(lex("[["), vec![Token::LBracket, Token::LBracket]);
        assert_eq!(lex("]]"), vec![Token::RBracket, Token::RBracket]);
        assert_eq!(
            lex("[[ -f file ]]"),
            vec![
                Token::LBracket,
                Token::LBracket,
                Token::ShortFlag("f".to_string()),
                Token::Ident("file".to_string()),
                Token::RBracket,
                Token::RBracket
            ]
        );
    }

    #[test]
    fn test_expression_syntax() {
        assert_eq!(
            lex(r#"[[ $X == "value" ]]"#),
            vec![
                Token::LBracket,
                Token::LBracket,
                Token::SimpleVarRef("X".to_string()),
                Token::EqEq,
                Token::String("value".to_string()),
                Token::RBracket,
                Token::RBracket
            ]
        );
    }

    #[test]
    fn bash_style_assignment() {
        // NAME="value" (no spaces) - lexer sees IDENT EQ STRING
        assert_eq!(
            lex(r#"NAME="value""#),
            vec![
                Token::Ident("NAME".to_string()),
                Token::Eq,
                Token::String("value".to_string())
            ]
        );
    }

    #[test]
    fn positional_params() {
        assert_eq!(lex("$0"), vec![Token::Positional(0)]);
        assert_eq!(lex("$1"), vec![Token::Positional(1)]);
        assert_eq!(lex("$9"), vec![Token::Positional(9)]);
        assert_eq!(lex("$@"), vec![Token::AllArgs]);
        assert_eq!(lex("$#"), vec![Token::ArgCount]);
    }

    #[test]
    fn positional_in_context() {
        assert_eq!(
            lex("echo $1 $2"),
            vec![
                Token::Ident("echo".to_string()),
                Token::Positional(1),
                Token::Positional(2),
            ]
        );
    }

    #[test]
    fn var_length() {
        assert_eq!(lex("${#X}"), vec![Token::VarLength("X".to_string())]);
        assert_eq!(lex("${#NAME}"), vec![Token::VarLength("NAME".to_string())]);
        assert_eq!(lex("${#foo_bar}"), vec![Token::VarLength("foo_bar".to_string())]);
    }

    #[test]
    fn var_length_in_context() {
        assert_eq!(
            lex("echo ${#NAME}"),
            vec![
                Token::Ident("echo".to_string()),
                Token::VarLength("NAME".to_string()),
            ]
        );
    }

    // ═══════════════════════════════════════════════════════════════════
    // Edge case tests: Flag ambiguities
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn plus_flag() {
        // Plus flags for set +e
        assert_eq!(lex("+e"), vec![Token::PlusFlag("e".to_string())]);
        assert_eq!(lex("+x"), vec![Token::PlusFlag("x".to_string())]);
        assert_eq!(lex("+ex"), vec![Token::PlusFlag("ex".to_string())]);
    }

    #[test]
    fn set_with_plus_flag() {
        assert_eq!(
            lex("set +e"),
            vec![
                Token::Set,
                Token::PlusFlag("e".to_string()),
            ]
        );
    }

    #[test]
    fn set_with_multiple_flags() {
        assert_eq!(
            lex("set -e -u"),
            vec![
                Token::Set,
                Token::ShortFlag("e".to_string()),
                Token::ShortFlag("u".to_string()),
            ]
        );
    }

    #[test]
    fn flags_vs_negative_numbers_edge_cases() {
        // -1a should be negative int followed by ident
        assert_eq!(
            lex("-1 a"),
            vec![Token::Int(-1), Token::Ident("a".to_string())]
        );
        // -l is a flag
        assert_eq!(lex("-l"), vec![Token::ShortFlag("l".to_string())]);
        // -123 is negative number
        assert_eq!(lex("-123"), vec![Token::Int(-123)]);
    }

    #[test]
    fn single_dash_is_ident() {
        // Single dash alone - lexer treats this as error or special
        // In most shells, - means stdin, but our lexer may error
        // Let's see what happens
        let result = tokenize("-");
        // A lone dash doesn't match any pattern, should error
        assert!(result.is_err() || result.unwrap().iter().any(|s| matches!(s.token, Token::ShortFlag(_))));
    }

    #[test]
    fn while_keyword_vs_while_loop() {
        // 'while' as keyword in loop context
        assert_eq!(lex("while"), vec![Token::While]);
        // 'while' at start followed by condition
        assert_eq!(
            lex("while true"),
            vec![Token::While, Token::True]
        );
    }

    #[test]
    fn control_flow_keywords() {
        assert_eq!(lex("break"), vec![Token::Break]);
        assert_eq!(lex("continue"), vec![Token::Continue]);
        assert_eq!(lex("return"), vec![Token::Return]);
        assert_eq!(lex("exit"), vec![Token::Exit]);
    }

    #[test]
    fn control_flow_with_numbers() {
        assert_eq!(
            lex("break 2"),
            vec![Token::Break, Token::Int(2)]
        );
        assert_eq!(
            lex("continue 3"),
            vec![Token::Continue, Token::Int(3)]
        );
        assert_eq!(
            lex("exit 1"),
            vec![Token::Exit, Token::Int(1)]
        );
    }

    // ═══════════════════════════════════════════════════════════════════
    // Here-doc tests
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn heredoc_simple() {
        let source = "cat <<EOF\nhello\nworld\nEOF";
        let tokens = lex(source);
        assert_eq!(tokens, vec![
            Token::Ident("cat".to_string()),
            Token::HereDocStart,
            Token::HereDoc("hello\nworld".to_string()),
            Token::Newline,
        ]);
    }

    #[test]
    fn heredoc_empty() {
        let source = "cat <<EOF\nEOF";
        let tokens = lex(source);
        assert_eq!(tokens, vec![
            Token::Ident("cat".to_string()),
            Token::HereDocStart,
            Token::HereDoc("".to_string()),
            Token::Newline,
        ]);
    }

    #[test]
    fn heredoc_with_special_chars() {
        let source = "cat <<EOF\n$VAR and \"quoted\" 'single'\nEOF";
        let tokens = lex(source);
        assert_eq!(tokens, vec![
            Token::Ident("cat".to_string()),
            Token::HereDocStart,
            Token::HereDoc("$VAR and \"quoted\" 'single'".to_string()),
            Token::Newline,
        ]);
    }

    #[test]
    fn heredoc_multiline() {
        let source = "cat <<END\nline1\nline2\nline3\nEND";
        let tokens = lex(source);
        assert_eq!(tokens, vec![
            Token::Ident("cat".to_string()),
            Token::HereDocStart,
            Token::HereDoc("line1\nline2\nline3".to_string()),
            Token::Newline,
        ]);
    }

    #[test]
    fn heredoc_in_command() {
        let source = "cat <<EOF\nhello\nEOF\necho goodbye";
        let tokens = lex(source);
        assert_eq!(tokens, vec![
            Token::Ident("cat".to_string()),
            Token::HereDocStart,
            Token::HereDoc("hello".to_string()),
            Token::Newline,
            Token::Ident("echo".to_string()),
            Token::Ident("goodbye".to_string()),
        ]);
    }

    #[test]
    fn heredoc_strip_tabs() {
        let source = "cat <<-EOF\n\thello\n\tworld\n\tEOF";
        let tokens = lex(source);
        // Content has tabs preserved, only delimiter matching strips tabs
        assert_eq!(tokens, vec![
            Token::Ident("cat".to_string()),
            Token::HereDocStart,
            Token::HereDoc("\thello\n\tworld".to_string()),
            Token::Newline,
        ]);
    }

    // ═══════════════════════════════════════════════════════════════════
    // Arithmetic expression tests
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn arithmetic_simple() {
        let source = "$((1 + 2))";
        let tokens = lex(source);
        assert_eq!(tokens, vec![Token::Arithmetic("1 + 2".to_string())]);
    }

    #[test]
    fn arithmetic_in_assignment() {
        let source = "X=$((5 * 3))";
        let tokens = lex(source);
        assert_eq!(tokens, vec![
            Token::Ident("X".to_string()),
            Token::Eq,
            Token::Arithmetic("5 * 3".to_string()),
        ]);
    }

    #[test]
    fn arithmetic_with_nested_parens() {
        let source = "$((2 * (3 + 4)))";
        let tokens = lex(source);
        assert_eq!(tokens, vec![Token::Arithmetic("2 * (3 + 4)".to_string())]);
    }

    #[test]
    fn arithmetic_with_variable() {
        let source = "$((X + 1))";
        let tokens = lex(source);
        assert_eq!(tokens, vec![Token::Arithmetic("X + 1".to_string())]);
    }

    #[test]
    fn arithmetic_command_subst_not_confused() {
        // $( should not be treated as arithmetic
        let source = "$(echo hello)";
        let tokens = lex(source);
        assert_eq!(tokens, vec![
            Token::CmdSubstStart,
            Token::Ident("echo".to_string()),
            Token::Ident("hello".to_string()),
            Token::RParen,
        ]);
    }
}
