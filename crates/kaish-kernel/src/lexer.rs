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
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

/// Global counter for generating unique markers across all tokenize calls.
static MARKER_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Maximum nesting depth for parentheses in arithmetic expressions.
/// Prevents stack overflow from pathologically nested inputs like $((((((...
const MAX_PAREN_DEPTH: usize = 256;

/// Tracks a text replacement for span correction.
/// When preprocessing replaces text (like `$((1+2))` with a marker),
/// we need to adjust subsequent spans to account for the length change.
#[derive(Debug, Clone)]
struct SpanReplacement {
    /// Position in the preprocessed text where the marker starts.
    preprocessed_pos: usize,
    /// Length of the marker in preprocessed text.
    marker_len: usize,
    /// Length of the original text that was replaced.
    original_len: usize,
}

/// Corrects a span from preprocessed-text coordinates back to original-text coordinates.
fn correct_span(span: Span, replacements: &[SpanReplacement]) -> Span {
    let mut start_adjustment: isize = 0;
    let mut end_adjustment: isize = 0;

    for r in replacements {
        // Calculate the length difference (positive = original was longer, negative = marker is longer)
        let delta = r.original_len as isize - r.marker_len as isize;

        // If the span starts after this replacement, adjust the start
        if span.start > r.preprocessed_pos + r.marker_len {
            start_adjustment += delta;
        } else if span.start > r.preprocessed_pos {
            // Span starts inside the marker - map to original position
            // (this shouldn't happen often, but handle it gracefully)
            start_adjustment += delta;
        }

        // If the span ends after this replacement, adjust the end
        if span.end > r.preprocessed_pos + r.marker_len {
            end_adjustment += delta;
        } else if span.end > r.preprocessed_pos {
            // Span ends inside the marker - map to end of original
            end_adjustment += delta;
        }
    }

    let new_start = (span.start as isize + start_adjustment).max(0) as usize;
    let new_end = (span.end as isize + end_adjustment).max(new_start as isize) as usize;
    new_start..new_end
}

/// Generate a unique marker ID that's extremely unlikely to collide with user code.
/// Uses a combination of timestamp, counter, and process ID.
fn unique_marker_id() -> String {
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let counter = MARKER_COUNTER.fetch_add(1, Ordering::Relaxed);
    let pid = std::process::id();
    format!("{:x}_{:x}_{:x}", timestamp, counter, pid)
}

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
    /// Nesting depth exceeded (too many nested parentheses in arithmetic).
    NestingTooDeep,
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
            LexerError::NestingTooDeep => write!(f, "nesting depth exceeded (max {})", MAX_PAREN_DEPTH),
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
/// Here-doc content data.
/// `literal` is true when the delimiter was quoted (<<'EOF' or <<"EOF"),
/// meaning no variable expansion should occur.
#[derive(Debug, Clone, PartialEq)]
pub struct HereDocData {
    pub content: String,
    pub literal: bool,
}

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

    #[token("1>&2")]
    StdoutToStderr,

    #[token(">&2")]
    StdoutToStderr2,

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

    #[token("..")]
    DotDot,

    #[token(".")]
    Dot,

    /// Tilde path: `~/foo`, `~user/bar` - value includes the full string
    #[regex(r"~[a-zA-Z0-9_./+-]+", lex_tilde_path, priority = 3)]
    TildePath(String),

    /// Bare tilde: `~` alone (expands to $HOME)
    #[token("~")]
    Tilde,

    /// Relative path starting with `../`: `../foo/bar`
    #[regex(r"\.\./[a-zA-Z0-9_./-]+", lex_relative_path, priority = 3)]
    RelativePath(String),

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

    /// Bare word starting with + followed by non-letter: `+%s`, `+%Y-%m-%d`
    /// For date format strings and similar. Lower priority than PlusFlag.
    #[regex(r"\+[^a-zA-Z\s][^\s]*", lex_plus_bare, priority = 2)]
    PlusBare(String),

    /// Bare word starting with - followed by non-letter/digit/dash: `-%`, etc.
    /// For rare cases. Lower priority than ShortFlag, Int, and DoubleDash.
    /// Excludes - after first - to avoid matching --name patterns.
    #[regex(r"-[^a-zA-Z0-9\s\-][^\s]*", lex_minus_bare, priority = 1)]
    MinusBare(String),

    /// Standalone - (stdin indicator for cat -, diff - -, etc.)
    /// Only matches when followed by whitespace or end.
    /// This is handled specially in the parser as a positional arg.
    #[token("-")]
    MinusAlone,

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

    /// Last exit code: `$?`
    #[token("$?")]
    LastExitCode,

    /// Current shell PID: `$$`
    #[token("$$")]
    CurrentPid,

    /// Variable string length: `${#VAR}`
    #[regex(r"\$\{#[a-zA-Z_][a-zA-Z0-9_]*\}", lex_var_length)]
    VarLength(String),

    /// Here-doc content: synthesized by preprocessing, not directly lexed.
    /// Contains the full content of the here-doc (without the delimiter lines).
    HereDoc(HereDocData),

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
    #[regex(r"#[^\n\r]*", allow_greedy = true)]
    Comment,

    /// Newline (significant in kaish - ends statements)
    #[regex(r"\n|\r\n")]
    Newline,

    /// Line continuation: backslash at end of line
    #[regex(r"\\[ \t]*(\n|\r\n)")]
    LineContinuation,
}

/// Semantic category for syntax highlighting.
///
/// Stable enum that groups tokens by purpose. Consumers match on categories
/// instead of individual tokens, insulating them from lexer evolution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenCategory {
    /// Keywords: if, then, else, for, while, function, return, etc.
    Keyword,
    /// Operators: |, &&, ||, >, >>, 2>&1, =, ==, etc.
    Operator,
    /// String literals: "...", '...', heredocs
    String,
    /// Numeric literals: 123, 3.14, arithmetic expressions
    Number,
    /// Variable references: $foo, ${bar}, $1, $@, $#, $?, $$
    Variable,
    /// Comments: # ...
    Comment,
    /// Punctuation: ; , . ( ) { } [ ]
    Punctuation,
    /// Identifiers in command position
    Command,
    /// Absolute paths: /foo/bar
    Path,
    /// Flags: --long, -s, +x
    Flag,
    /// Invalid tokens
    Error,
}

impl Token {
    /// Returns the semantic category for syntax highlighting.
    pub fn category(&self) -> TokenCategory {
        match self {
            // Keywords
            Token::If
            | Token::Then
            | Token::Else
            | Token::Elif
            | Token::Fi
            | Token::For
            | Token::In
            | Token::Do
            | Token::Done
            | Token::While
            | Token::Case
            | Token::Esac
            | Token::Function
            | Token::Return
            | Token::Break
            | Token::Continue
            | Token::Exit
            | Token::Set
            | Token::Local
            | Token::True
            | Token::False
            | Token::TypeString
            | Token::TypeInt
            | Token::TypeFloat
            | Token::TypeBool => TokenCategory::Keyword,

            // Operators and redirections
            Token::Pipe
            | Token::And
            | Token::Or
            | Token::Amp
            | Token::Eq
            | Token::EqEq
            | Token::NotEq
            | Token::Match
            | Token::NotMatch
            | Token::Lt
            | Token::Gt
            | Token::LtEq
            | Token::GtEq
            | Token::GtGt
            | Token::Stderr
            | Token::Both
            | Token::HereDocStart
            | Token::StderrToStdout
            | Token::StdoutToStderr
            | Token::StdoutToStderr2 => TokenCategory::Operator,

            // Strings
            Token::String(_) | Token::SingleString(_) | Token::HereDoc(_) => TokenCategory::String,

            // Numbers
            Token::Int(_) | Token::Float(_) | Token::Arithmetic(_) => TokenCategory::Number,

            // Variables
            Token::VarRef(_)
            | Token::SimpleVarRef(_)
            | Token::Positional(_)
            | Token::AllArgs
            | Token::ArgCount
            | Token::VarLength(_)
            | Token::LastExitCode
            | Token::CurrentPid => TokenCategory::Variable,

            // Flags
            Token::LongFlag(_)
            | Token::ShortFlag(_)
            | Token::PlusFlag(_)
            | Token::DoubleDash => TokenCategory::Flag,

            // Punctuation
            Token::Semi
            | Token::DoubleSemi
            | Token::Colon
            | Token::Comma
            | Token::Dot
            | Token::LParen
            | Token::RParen
            | Token::LBrace
            | Token::RBrace
            | Token::LBracket
            | Token::RBracket
            | Token::Bang
            | Token::Question
            | Token::Star
            | Token::Newline
            | Token::LineContinuation
            | Token::CmdSubstStart => TokenCategory::Punctuation,

            // Comments
            Token::Comment => TokenCategory::Comment,

            // Paths
            Token::Path(_)
            | Token::TildePath(_)
            | Token::RelativePath(_)
            | Token::Tilde
            | Token::DotDot => TokenCategory::Path,

            // Commands/identifiers (and bare words)
            Token::Ident(_)
            | Token::PlusBare(_)
            | Token::MinusBare(_)
            | Token::MinusAlone => TokenCategory::Command,

            // Errors
            Token::InvalidNumberIdent
            | Token::InvalidFloatNoLeading
            | Token::InvalidFloatNoTrailing => TokenCategory::Error,
        }
    }
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

/// Lex a plus bare word: `+%s` → `+%s` (keep the full string)
fn lex_plus_bare(lex: &mut logos::Lexer<Token>) -> String {
    lex.slice().to_string()
}

/// Lex a minus bare word: `-%` → `-%` (keep the full string)
fn lex_minus_bare(lex: &mut logos::Lexer<Token>) -> String {
    lex.slice().to_string()
}

/// Lex an absolute path: `/tmp/out` → `/tmp/out`
fn lex_path(lex: &mut logos::Lexer<Token>) -> String {
    lex.slice().to_string()
}

/// Lex a tilde path: `~/foo` → `~/foo`
fn lex_tilde_path(lex: &mut logos::Lexer<Token>) -> String {
    lex.slice().to_string()
}

/// Lex a relative path: `../foo` → `../foo`
fn lex_relative_path(lex: &mut logos::Lexer<Token>) -> String {
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
            Token::StdoutToStderr => write!(f, "1>&2"),
            Token::StdoutToStderr2 => write!(f, ">&2"),
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
            Token::DotDot => write!(f, ".."),
            Token::Tilde => write!(f, "~"),
            Token::TildePath(s) => write!(f, "{}", s),
            Token::RelativePath(s) => write!(f, "{}", s),
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
            Token::PlusBare(s) => write!(f, "{}", s),
            Token::MinusBare(s) => write!(f, "{}", s),
            Token::MinusAlone => write!(f, "-"),
            Token::String(s) => write!(f, "STRING({:?})", s),
            Token::SingleString(s) => write!(f, "SINGLESTRING({:?})", s),
            Token::HereDoc(d) => write!(f, "HEREDOC({:?}, literal={})", d.content, d.literal),
            Token::VarRef(v) => write!(f, "VARREF({})", v),
            Token::SimpleVarRef(v) => write!(f, "SIMPLEVARREF({})", v),
            Token::Positional(n) => write!(f, "${}", n),
            Token::AllArgs => write!(f, "$@"),
            Token::ArgCount => write!(f, "$#"),
            Token::LastExitCode => write!(f, "$?"),
            Token::CurrentPid => write!(f, "$$"),
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
                | Token::LastExitCode
                | Token::CurrentPid
        )
    }
}

/// Result of preprocessing arithmetic expressions.
struct ArithmeticPreprocessResult {
    /// Preprocessed source with markers replacing $((expr)).
    text: String,
    /// Vector of (marker, expression_content) pairs.
    arithmetics: Vec<(String, String)>,
    /// Span replacements for correcting token positions.
    replacements: Vec<SpanReplacement>,
}

/// Skip a `$(...)` command substitution with quote-aware paren matching.
///
/// Copies the entire command substitution verbatim to `result`, handling
/// single quotes, double quotes, and backslash escapes inside the sub so
/// that parentheses within strings don't confuse the depth counter.
///
/// On entry, `i` points to the `$` of `$(`. On exit, `i` points past the
/// closing `)`.
fn skip_command_substitution(
    chars: &[char],
    i: &mut usize,
    source_pos: &mut usize,
    result: &mut String,
) {
    // Copy $(
    result.push('$');
    result.push('(');
    *i += 2;
    *source_pos += 2;

    let mut depth: usize = 1;
    let mut in_single_quote = false;
    let mut in_double_quote = false;

    while *i < chars.len() && depth > 0 {
        let c = chars[*i];

        if in_single_quote {
            result.push(c);
            *source_pos += c.len_utf8();
            *i += 1;
            if c == '\'' {
                in_single_quote = false;
            }
            continue;
        }

        if in_double_quote {
            if c == '\\' && *i + 1 < chars.len() {
                let next = chars[*i + 1];
                if next == '"' || next == '\\' || next == '$' || next == '`' {
                    result.push(c);
                    result.push(next);
                    *source_pos += c.len_utf8() + next.len_utf8();
                    *i += 2;
                    continue;
                }
            }
            if c == '"' {
                in_double_quote = false;
            }
            result.push(c);
            *source_pos += c.len_utf8();
            *i += 1;
            continue;
        }

        // Outside quotes
        match c {
            '\'' => {
                in_single_quote = true;
                result.push(c);
                *source_pos += c.len_utf8();
                *i += 1;
            }
            '"' => {
                in_double_quote = true;
                result.push(c);
                *source_pos += c.len_utf8();
                *i += 1;
            }
            '\\' if *i + 1 < chars.len() => {
                result.push(c);
                result.push(chars[*i + 1]);
                *source_pos += c.len_utf8() + chars[*i + 1].len_utf8();
                *i += 2;
            }
            '(' => {
                depth += 1;
                result.push(c);
                *source_pos += c.len_utf8();
                *i += 1;
            }
            ')' => {
                depth -= 1;
                result.push(c);
                *source_pos += c.len_utf8();
                *i += 1;
            }
            _ => {
                result.push(c);
                *source_pos += c.len_utf8();
                *i += 1;
            }
        }
    }
}

/// Preprocess arithmetic expressions in source code.
///
/// Finds `$((expr))` patterns and replaces them with markers.
/// Returns the preprocessed source, arithmetic contents, and span replacement info.
///
/// Example:
///   `X=$((1 + 2))`
/// Becomes:
///   `X=__KAISH_ARITH_{id}__`
/// With arithmetics[0] = ("__KAISH_ARITH_{id}__", "1 + 2")
///
/// # Errors
/// Returns `LexerError::NestingTooDeep` if parentheses are nested beyond MAX_PAREN_DEPTH.
fn preprocess_arithmetic(source: &str) -> Result<ArithmeticPreprocessResult, LexerError> {
    let mut result = String::with_capacity(source.len());
    let mut arithmetics: Vec<(String, String)> = Vec::new();
    let mut replacements: Vec<SpanReplacement> = Vec::new();
    let mut source_pos: usize = 0;
    let chars_vec: Vec<char> = source.chars().collect();
    let mut i = 0;

    // Whether we're currently inside double quotes. Single quotes inside
    // double quotes are literal characters, not quote delimiters.
    let mut in_double_quote = false;

    while i < chars_vec.len() {
        let ch = chars_vec[i];

        // Backslash escape outside quotes — skip both chars verbatim
        if !in_double_quote && ch == '\\' && i + 1 < chars_vec.len() {
            result.push(ch);
            result.push(chars_vec[i + 1]);
            source_pos += ch.len_utf8() + chars_vec[i + 1].len_utf8();
            i += 2;
            continue;
        }

        // Single quote — only starts quote mode when NOT inside double quotes
        if ch == '\'' && !in_double_quote {
            result.push(ch);
            i += 1;
            source_pos += 1;
            while i < chars_vec.len() && chars_vec[i] != '\'' {
                result.push(chars_vec[i]);
                source_pos += chars_vec[i].len_utf8();
                i += 1;
            }
            if i < chars_vec.len() {
                result.push(chars_vec[i]); // closing quote
                source_pos += 1;
                i += 1;
            }
            continue;
        }

        // Double quote — toggle state (arithmetic is still expanded inside)
        if ch == '"' {
            in_double_quote = !in_double_quote;
            result.push(ch);
            i += 1;
            source_pos += 1;
            continue;
        }

        // Backslash escape inside double quotes — only \" and \\ are special
        if in_double_quote && ch == '\\' && i + 1 < chars_vec.len() {
            let next = chars_vec[i + 1];
            if next == '"' || next == '\\' || next == '$' || next == '`' {
                result.push(ch);
                result.push(next);
                source_pos += ch.len_utf8() + next.len_utf8();
                i += 2;
                continue;
            }
        }

        // Skip $(...) command substitutions — inner arithmetic belongs to the subcommand
        if ch == '$' && i + 1 < chars_vec.len() && chars_vec[i + 1] == '('
            && !(i + 2 < chars_vec.len() && chars_vec[i + 2] == '(')
        {
            skip_command_substitution(&chars_vec, &mut i, &mut source_pos, &mut result);
            continue;
        }

        // Look for $(( (potential arithmetic)
        if ch == '$' && i + 2 < chars_vec.len() && chars_vec[i + 1] == '(' && chars_vec[i + 2] == '(' {
            let arith_start_pos = result.len();
            let original_start = source_pos;

            // Skip $((
            i += 3;
            source_pos += 3;

            // Collect expression until matching ))
            let mut expr = String::new();
            let mut paren_depth: usize = 0;

            while i < chars_vec.len() {
                let c = chars_vec[i];
                match c {
                    '(' => {
                        paren_depth += 1;
                        if paren_depth > MAX_PAREN_DEPTH {
                            return Err(LexerError::NestingTooDeep);
                        }
                        expr.push('(');
                        i += 1;
                        source_pos += c.len_utf8();
                    }
                    ')' => {
                        if paren_depth > 0 {
                            paren_depth -= 1;
                            expr.push(')');
                            i += 1;
                            source_pos += 1;
                        } else if i + 1 < chars_vec.len() && chars_vec[i + 1] == ')' {
                            // Found closing ))
                            i += 2;
                            source_pos += 2;
                            break;
                        } else {
                            // Single ) inside - keep going
                            expr.push(')');
                            i += 1;
                            source_pos += 1;
                        }
                    }
                    _ => {
                        expr.push(c);
                        i += 1;
                        source_pos += c.len_utf8();
                    }
                }
            }

            // Calculate original length: from $$(( to ))
            let original_len = source_pos - original_start;

            // Create a unique marker for this arithmetic (collision-resistant)
            let marker = format!("__KAISH_ARITH_{}__", unique_marker_id());
            let marker_len = marker.len();

            // Record the replacement for span correction
            replacements.push(SpanReplacement {
                preprocessed_pos: arith_start_pos,
                marker_len,
                original_len,
            });

            arithmetics.push((marker.clone(), expr));
            result.push_str(&marker);
        } else {
            result.push(ch);
            i += 1;
            source_pos += ch.len_utf8();
        }
    }

    Ok(ArithmeticPreprocessResult {
        text: result,
        arithmetics,
        replacements,
    })
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
fn preprocess_heredocs(source: &str) -> (String, Vec<(String, String, bool)>) {
    let mut result = String::with_capacity(source.len());
    let mut heredocs: Vec<(String, String, bool)> = Vec::new();
    let mut chars = source.chars().peekable();

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

            // Buffer text after delimiter word (e.g., " | jq" in "cat <<EOF | jq")
            // This must be emitted AFTER the heredoc marker, not before.
            let mut after_delimiter = String::new();
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
                    after_delimiter.push(ch);
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
                        // Add line to content (including empty lines)
                        content.push_str(&current_line);
                        content.push('\n');
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
                        content.push_str(&current_line);
                        content.push('\n');
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

            // Create a unique marker for this here-doc (collision-resistant)
            let marker = format!("__KAISH_HEREDOC_{}__", unique_marker_id());
            heredocs.push((marker.clone(), content, quoted));

            // Output <<marker first, then any text that followed the delimiter
            // (e.g., " | jq") so the heredoc attaches to the correct command.
            result.push_str("<<");
            result.push_str(&marker);
            result.push_str(&after_delimiter);
            result.push('\n');
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
    let arith_result = preprocess_arithmetic(source)
        .map_err(|e| vec![Spanned::new(e, 0..source.len())])?;

    // Then preprocess here-docs (heredoc span tracking is not implemented for simplicity)
    let (preprocessed, heredocs) = preprocess_heredocs(&arith_result.text);

    // Combine replacements for span correction (arithmetic only for now)
    let span_replacements = arith_result.replacements;

    let lexer = Token::lexer(&preprocessed);
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    for (result, span) in lexer.spanned() {
        // Correct the span from preprocessed coordinates to original coordinates
        let corrected_span = correct_span(span, &span_replacements);
        match result {
            Ok(token) => {
                // Skip comments and line continuations - they're not needed for parsing
                if !matches!(token, Token::Comment | Token::LineContinuation) {
                    tokens.push(Spanned::new(token, corrected_span));
                }
            }
            Err(err) => {
                errors.push(Spanned::new(err, corrected_span));
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
        // Check for arithmetic marker (unique format: __KAISH_ARITH_{id}__)
        if let Token::Ident(ref name) = tokens[i].token
            && name.starts_with("__KAISH_ARITH_") && name.ends_with("__")
                && let Some((_, expr)) = arith_result.arithmetics.iter().find(|(marker, _)| marker == name) {
                    final_tokens.push(Spanned::new(Token::Arithmetic(expr.clone()), tokens[i].span.clone()));
                    i += 1;
                    continue;
                }

        // Check for heredoc (unique format: __KAISH_HEREDOC_{id}__)
        if matches!(tokens[i].token, Token::HereDocStart) {
            // Check if next token is a heredoc marker
            if i + 1 < tokens.len()
                && let Token::Ident(ref name) = tokens[i + 1].token
                    && name.starts_with("__KAISH_HEREDOC_") && name.ends_with("__") {
                        // Find the corresponding content
                        if let Some((_, content, literal)) = heredocs.iter().find(|(marker, _, _)| marker == name) {
                            final_tokens.push(Spanned::new(Token::HereDocStart, tokens[i].span.clone()));
                            final_tokens.push(Spanned::new(Token::HereDoc(HereDocData { content: content.clone(), literal: *literal }), tokens[i + 1].span.clone()));
                            i += 2;
                            continue;
                        }
                    }
        }

        // Check for arithmetic markers inside string content
        let token = if let Token::String(ref s) = tokens[i].token {
            // Check if string contains any arithmetic markers
            let mut new_content = s.clone();
            for (marker, expr) in &arith_result.arithmetics {
                if new_content.contains(marker) {
                    // Replace marker with the special format that parse_interpolated_string can detect
                    // Use ${__ARITH:expr__} format so it gets parsed as StringPart::Arithmetic
                    new_content = new_content.replace(marker, &format!("${{__ARITH:{}__}}", expr));
                }
            }
            if new_content != *s {
                Spanned::new(Token::String(new_content), tokens[i].span.clone())
            } else {
                tokens[i].clone()
            }
        } else {
            tokens[i].clone()
        };
        final_tokens.push(token);
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
                // Use a unique marker for escaped dollar that won't be re-interpreted
                // parse_interpolated_string will convert this back to $
                Some('$') => result.push_str("__KAISH_ESCAPED_DOLLAR__"),
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
                    if let Some(c) = chars.next() {
                        index.push(c);
                    }
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

    #[test]
    fn parse_string_with_escaped_dollar() {
        // \$ produces a marker that parse_interpolated_string will convert to $
        // The marker __KAISH_ESCAPED_DOLLAR__ is used to prevent re-interpretation
        assert_eq!(
            parse_string_literal(r#""\$VAR""#).expect("ok"),
            "__KAISH_ESCAPED_DOLLAR__VAR"
        );
        assert_eq!(
            parse_string_literal(r#""cost: \$100""#).expect("ok"),
            "cost: __KAISH_ESCAPED_DOLLAR__100"
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
    fn single_dash_is_minus_alone() {
        // Single dash alone - now handled as MinusAlone for `cat -` stdin indicator
        let result = tokenize("-").expect("should lex");
        assert_eq!(result.len(), 1);
        assert!(matches!(result[0].token, Token::MinusAlone));
    }

    #[test]
    fn plus_bare_for_date_format() {
        // `date +%s` - the +%s should be PlusBare
        let result = tokenize("+%s").expect("should lex");
        assert_eq!(result.len(), 1);
        assert!(matches!(result[0].token, Token::PlusBare(ref s) if s == "+%s"));

        // `date +%Y-%m-%d` - format string with dashes
        let result = tokenize("+%Y-%m-%d").expect("should lex");
        assert_eq!(result.len(), 1);
        assert!(matches!(result[0].token, Token::PlusBare(ref s) if s == "+%Y-%m-%d"));
    }

    #[test]
    fn plus_flag_still_works() {
        // `set +e` - should still be PlusFlag
        let result = tokenize("+e").expect("should lex");
        assert_eq!(result.len(), 1);
        assert!(matches!(result[0].token, Token::PlusFlag(ref s) if s == "e"));
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
            Token::HereDoc(HereDocData { content: "hello\nworld".to_string(), literal: false }),
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
            Token::HereDoc(HereDocData { content: "".to_string(), literal: false }),
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
            Token::HereDoc(HereDocData { content: "$VAR and \"quoted\" 'single'".to_string(), literal: false }),
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
            Token::HereDoc(HereDocData { content: "line1\nline2\nline3".to_string(), literal: false }),
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
            Token::HereDoc(HereDocData { content: "hello".to_string(), literal: false }),
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
            Token::HereDoc(HereDocData { content: "\thello\n\tworld".to_string(), literal: false }),
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

    #[test]
    fn arithmetic_nesting_limit() {
        // Create deeply nested parens that exceed MAX_PAREN_DEPTH (256)
        let open_parens = "(".repeat(300);
        let close_parens = ")".repeat(300);
        let source = format!("$(({}1{}))", open_parens, close_parens);
        let result = tokenize(&source);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].token, LexerError::NestingTooDeep);
    }

    #[test]
    fn arithmetic_nesting_within_limit() {
        // Nesting within limit should work
        let source = "$((((1 + 2) * 3)))";
        let tokens = lex(source);
        assert_eq!(tokens, vec![Token::Arithmetic("((1 + 2) * 3)".to_string())]);
    }

    // ═══════════════════════════════════════════════════════════════════
    // Token category tests
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn token_categories() {
        // Keywords
        assert_eq!(Token::If.category(), TokenCategory::Keyword);
        assert_eq!(Token::Then.category(), TokenCategory::Keyword);
        assert_eq!(Token::For.category(), TokenCategory::Keyword);
        assert_eq!(Token::Function.category(), TokenCategory::Keyword);
        assert_eq!(Token::True.category(), TokenCategory::Keyword);
        assert_eq!(Token::TypeString.category(), TokenCategory::Keyword);

        // Operators
        assert_eq!(Token::Pipe.category(), TokenCategory::Operator);
        assert_eq!(Token::And.category(), TokenCategory::Operator);
        assert_eq!(Token::Or.category(), TokenCategory::Operator);
        assert_eq!(Token::StderrToStdout.category(), TokenCategory::Operator);
        assert_eq!(Token::GtGt.category(), TokenCategory::Operator);

        // Strings
        assert_eq!(Token::String("test".to_string()).category(), TokenCategory::String);
        assert_eq!(Token::SingleString("test".to_string()).category(), TokenCategory::String);
        assert_eq!(Token::HereDoc(HereDocData { content: "test".to_string(), literal: false }).category(), TokenCategory::String);

        // Numbers
        assert_eq!(Token::Int(42).category(), TokenCategory::Number);
        assert_eq!(Token::Float(3.14).category(), TokenCategory::Number);
        assert_eq!(Token::Arithmetic("1+2".to_string()).category(), TokenCategory::Number);

        // Variables
        assert_eq!(Token::SimpleVarRef("X".to_string()).category(), TokenCategory::Variable);
        assert_eq!(Token::VarRef("${X}".to_string()).category(), TokenCategory::Variable);
        assert_eq!(Token::Positional(1).category(), TokenCategory::Variable);
        assert_eq!(Token::AllArgs.category(), TokenCategory::Variable);
        assert_eq!(Token::ArgCount.category(), TokenCategory::Variable);
        assert_eq!(Token::LastExitCode.category(), TokenCategory::Variable);
        assert_eq!(Token::CurrentPid.category(), TokenCategory::Variable);

        // Flags
        assert_eq!(Token::ShortFlag("l".to_string()).category(), TokenCategory::Flag);
        assert_eq!(Token::LongFlag("verbose".to_string()).category(), TokenCategory::Flag);
        assert_eq!(Token::PlusFlag("e".to_string()).category(), TokenCategory::Flag);
        assert_eq!(Token::DoubleDash.category(), TokenCategory::Flag);

        // Punctuation
        assert_eq!(Token::Semi.category(), TokenCategory::Punctuation);
        assert_eq!(Token::LParen.category(), TokenCategory::Punctuation);
        assert_eq!(Token::LBracket.category(), TokenCategory::Punctuation);
        assert_eq!(Token::Newline.category(), TokenCategory::Punctuation);

        // Comments
        assert_eq!(Token::Comment.category(), TokenCategory::Comment);

        // Paths
        assert_eq!(Token::Path("/tmp/file".to_string()).category(), TokenCategory::Path);

        // Commands
        assert_eq!(Token::Ident("echo".to_string()).category(), TokenCategory::Command);

        // Errors
        assert_eq!(Token::InvalidNumberIdent.category(), TokenCategory::Error);
        assert_eq!(Token::InvalidFloatNoLeading.category(), TokenCategory::Error);
        assert_eq!(Token::InvalidFloatNoTrailing.category(), TokenCategory::Error);
    }

    #[test]
    fn test_heredoc_piped_to_command() {
        // Bug 4: "cat <<EOF | jq" should produce: cat <<heredoc | jq
        // Not: cat | jq <<heredoc
        let tokens = tokenize("cat <<EOF | jq\n{\"key\": \"val\"}\nEOF").unwrap();
        let heredoc_pos = tokens.iter().position(|t| matches!(t.token, Token::HereDoc(_)));
        let pipe_pos = tokens.iter().position(|t| matches!(t.token, Token::Pipe));
        assert!(heredoc_pos.is_some(), "should have a heredoc token");
        assert!(pipe_pos.is_some(), "should have a pipe token");
        assert!(
            pipe_pos.unwrap() > heredoc_pos.unwrap(),
            "Pipe must come after heredoc, got heredoc at {}, pipe at {}. Tokens: {:?}",
            heredoc_pos.unwrap(), pipe_pos.unwrap(), tokens,
        );
    }

    #[test]
    fn test_heredoc_standalone_still_works() {
        // Regression: standalone heredoc (no pipe) must still work
        let tokens = tokenize("cat <<EOF\nhello\nEOF").unwrap();
        assert!(tokens.iter().any(|t| matches!(t.token, Token::HereDoc(_))));
        assert!(!tokens.iter().any(|t| matches!(t.token, Token::Pipe)));
    }

    #[test]
    fn test_heredoc_preserves_leading_empty_lines() {
        // Bug B: heredoc starting with a blank line must preserve it
        let tokens = tokenize("cat <<EOF\n\nhello\nEOF").unwrap();
        let heredoc = tokens.iter().find_map(|t| {
            if let Token::HereDoc(data) = &t.token {
                Some(data.clone())
            } else {
                None
            }
        });
        assert!(heredoc.is_some(), "should have a heredoc token");
        let data = heredoc.unwrap();
        assert!(data.content.starts_with('\n'), "leading empty line must be preserved, got: {:?}", data.content);
        assert_eq!(data.content, "\nhello");
    }

    #[test]
    fn test_heredoc_quoted_delimiter_sets_literal() {
        // Bug N: quoted delimiter (<<'EOF') should set literal=true
        let tokens = tokenize("cat <<'EOF'\nhello $HOME\nEOF").unwrap();
        let heredoc = tokens.iter().find_map(|t| {
            if let Token::HereDoc(data) = &t.token {
                Some(data.clone())
            } else {
                None
            }
        });
        assert!(heredoc.is_some(), "should have a heredoc token");
        let data = heredoc.unwrap();
        assert!(data.literal, "quoted delimiter should set literal=true");
        assert_eq!(data.content, "hello $HOME");
    }

    #[test]
    fn test_heredoc_unquoted_delimiter_not_literal() {
        // Bug N: unquoted delimiter (<<EOF) should have literal=false
        let tokens = tokenize("cat <<EOF\nhello $HOME\nEOF").unwrap();
        let heredoc = tokens.iter().find_map(|t| {
            if let Token::HereDoc(data) = &t.token {
                Some(data.clone())
            } else {
                None
            }
        });
        assert!(heredoc.is_some(), "should have a heredoc token");
        let data = heredoc.unwrap();
        assert!(!data.literal, "unquoted delimiter should have literal=false");
    }
}
