//! Lexer for kaish source code.
//!
//! Converts source text into a stream of tokens using the logos lexer
//! generator. The lexer is designed to be unambiguous: every valid input
//! produces exactly one token sequence, and invalid input produces clear
//! errors.
//!
//! # Pipeline (GH #95)
//!
//! 1. **Scan** — one composed source-order pass with explicit
//!    quote/escape/comment state extracts heredoc bodies and `$((expr))`
//!    arithmetic, producing a rewritten buffer plus a complete
//!    replacement table (both coordinate systems).
//! 2. **logos** — the regex vocabulary below classifies the rewritten
//!    buffer into tokens.
//! 3. **Marker resolution** — scanner markers become `Arithmetic` /
//!    `HereDoc` tokens, matched POSITIONALLY against the replacement
//!    table (never by fishing identifier text); a word glued onto a
//!    marker is split so the parser can reject it loudly.
//! 4. **Span correction** — every token span maps back to exact
//!    original-source byte ranges via the replacement table.
//! 5. **Fusion** — flag-metachar, colon, and glob merges join
//!    span-adjacent runs, with fused text sliced VERBATIM from the
//!    source; `compute_value_context` (an explicit frame stack plus a
//!    statement-head DFA) decides where fusion is suppressed.
//!
//! # Token Categories
//!
//! - **Keywords**: `set`, `if`, `then`, `else`, `fi`, `for`, `in`, `do`, `done`
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


/// Generate a unique marker ID that's extremely unlikely to collide with user code.
/// Uses a combination of timestamp, counter, and process ID.
fn unique_marker_id() -> String {
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let counter = MARKER_COUNTER.fetch_add(1, Ordering::Relaxed);
    #[cfg(target_os = "wasi")]
    let pid = 0u32;
    #[cfg(not(target_os = "wasi"))]
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
    InvalidFloatNoLeading,
    InvalidFloatNoTrailing,
    /// Nesting depth exceeded (too many nested parentheses in arithmetic).
    NestingTooDeep,
    /// Arithmetic expansion `$((` reached end of input without a closing `))`.
    /// Silently evaluating the partial expression would mask a typo (`$(( 1 + 2`
    /// would compute `3`), so we surface it loudly instead.
    UnterminatedArithmetic,
    /// Heredoc body ended without seeing the closing delimiter on its own line.
    /// The user almost certainly meant to type the delimiter — silently using
    /// whatever was collected up to EOF would mask missing data.
    UnterminatedHeredoc { delimiter: String },
    /// Backtick command substitution. Kaish drops backticks intentionally —
    /// they're listed in `docs/LANGUAGE.md` and the help system as not supported.
    /// We surface this as a dedicated error (rather than `UnexpectedCharacter`)
    /// so the message can point users at the `$(cmd)` replacement.
    BackticksNotSupported,
    /// `$((expr))` inside a bare `${...}` reference (e.g. `${X:-$((1+2))}`).
    /// There is no representation for arithmetic inside a variable
    /// reference — the pre-#95 pipeline silently leaked internal marker
    /// text here — so it is a loud error instead. (Inside double-quoted
    /// strings the same construct works via string interpolation.)
    ArithmeticInVarRef,
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
            LexerError::InvalidFloatNoLeading => write!(f, "float must have leading digit"),
            LexerError::InvalidFloatNoTrailing => write!(f, "float must have trailing digit"),
            LexerError::NestingTooDeep => write!(f, "nesting depth exceeded (max {})", MAX_PAREN_DEPTH),
            LexerError::UnterminatedArithmetic => {
                write!(f, "unterminated arithmetic expansion, expected closing `))`")
            }
            LexerError::UnterminatedHeredoc { delimiter } => {
                write!(f, "unterminated heredoc, expected closing delimiter `{}` on its own line", delimiter)
            }
            LexerError::BackticksNotSupported => {
                write!(f, "backticks are not supported in kaish; use $(cmd) instead")
            }
            LexerError::ArithmeticInVarRef => {
                write!(
                    f,
                    "arithmetic expansion inside ${{...}} is not supported; \
                     assign it to a variable first, e.g. N=$((expr)); ${{X:-$N}}"
                )
            }
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
///
/// - `literal` is true when the delimiter was quoted (`<<'EOF'` or `<<"EOF"`),
///   meaning no variable expansion should occur.
/// - `strip_tabs` is true for the `<<-EOF` form. Per POSIX, leading tabs on
///   each body line are stripped at materialization time. Stripping happens
///   downstream of the parser so byte offsets in `content` stay aligned with
///   their original-source positions for span-tracking purposes.
/// - `body_start_offset` is the exact byte offset of the first character of
///   `content` in the original source passed to `tokenize`. This lets the
///   parser compute absolute spans for parts found inside the body during
///   interpolation. (For interpolated bodies containing `$((..))`, spans of
///   parts AFTER the rewritten expression drift by the rewrite's length
///   difference — the body-local `${__ARITH:expr__}` form is longer than
///   the source text; see `rewrite_body_arithmetic`.)
#[derive(Debug, Clone, PartialEq)]
pub struct HereDocData {
    pub content: String,
    pub literal: bool,
    pub strip_tabs: bool,
    pub body_start_offset: usize,
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

    #[token("<<<")]
    HereString,

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

    /// Spread operator: `[...$xs date]`. Only meaningful inside a list literal
    /// (value context); inert everywhere else. logos resolves the `"..."` vs
    /// `".."` (`DotDot`) ambiguity by longest match, so no explicit priority
    /// is needed here.
    #[token("...")]
    DotDotDot,

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

    /// Relative path: `../foo/bar`, bare `src/kaish` (ident containing `/`),
    /// or a directory reference with a trailing slash like `dest/`. The
    /// trailing-slash form uses `*` (not `+`) after the slash so `dest/`
    /// lexes as one token instead of `Ident("dest")` + `Path("/")` — the
    /// latter split silently turned `cp a b dest/` into a 4-operand command.
    #[regex(r"\.\./[a-zA-Z0-9_./-]+", lex_relative_path, priority = 3)]
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_.-]*/[a-zA-Z0-9_./-]*", lex_relative_path, priority = 3)]
    RelativePath(String),

    /// Dot-slash path: `./foo`, `./script.sh`
    #[regex(r"\./[a-zA-Z0-9_./-]+", lex_dot_slash_path, priority = 3)]
    DotSlashPath(String),

    /// Dot-prefixed bareword: `.parent`, `.gitignore`, `.foo.bar`.
    /// Treated as an opaque string in argv position. Distinct from `Token::Dot`
    /// (the POSIX `.` source alias) which only matches a bare `.` — the source
    /// alias requires whitespace before its file argument (`. script`), so
    /// `.parent` (no space) is unambiguously a single bareword.
    #[regex(r"\.[a-zA-Z_][a-zA-Z0-9_.-]*", lex_dotted_ident, priority = 3)]
    DottedIdent(String),

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

    /// Merged glob word: span-adjacent tokens containing `*`, `?`, or `[...]`.
    /// Synthesized by `merge_glob_adjacent()`, never produced by logos directly.
    GlobWord(String),

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

    /// Short flag: `-l`, `-la` (combined short flags), or a dash-word with
    /// internal hyphens like `-not-a-flag`. Internal hyphens are part of the
    /// single shell word — without them the word fragments into separate flag
    /// tokens, which breaks `echo -- -not-a-flag` and the like. A leading `--`
    /// is still `DoubleDash` (the second char must be a letter here) unless
    /// the third char isn't a letter either, in which case it's
    /// `DoubleDashBare` — see below — and whether the word is a flag or a
    /// literal is the binding layer's call.
    #[regex(r"-[a-zA-Z][a-zA-Z0-9-]*", lex_short_flag, priority = 3)]
    ShortFlag(String),

    /// Plus flag: `+e` or `+x` (for set +e to disable options)
    #[regex(r"\+[a-zA-Z][a-zA-Z0-9]*", lex_plus_flag, priority = 3)]
    PlusFlag(String),

    /// Double dash: `--` alone marks end of flags. Only matches when nothing
    /// else follows (a longer match always wins) — a `--`-prefixed word with
    /// more characters after it either lexes as `LongFlag` (3rd char is a
    /// letter) or `DoubleDashBare` (3rd char is anything else).
    #[token("--")]
    DoubleDash,

    /// Bare word starting with `--` whose continuation isn't a valid
    /// long-flag name: `---`, `----`, `--=x`, `--1`, etc. Without this, the
    /// plain `--` literal above always won the length tie against a lone
    /// `--`, silently truncating a dash-only operand to its trailing
    /// remainder (`echo ---` printed `-` instead of `---` — GH #137). Mirrors
    /// `MinusBare`/`PlusBare` (bare-word fallback for an unrecognized
    /// flag-shaped prefix), just generalized to the `--` prefix. A standalone
    /// `--` (followed by whitespace/EOF) still lexes as `DoubleDash` — this
    /// regex requires at least one more non-whitespace character, so the two
    /// never tie in match length and no priority tiebreak is load-bearing;
    /// `priority = 2` is set for consistency with `PlusBare`'s tier.
    #[regex(r"--[^a-zA-Z\s][^\s]*", lex_double_dash_bare, priority = 2)]
    DoubleDashBare(String),

    /// Bare word starting with + followed by non-letter: `+%s`, `+%Y-%m-%d`
    /// For date format strings and similar. Lower priority than PlusFlag.
    #[regex(r"\+[^a-zA-Z\s][^\s]*", lex_plus_bare, priority = 2)]
    PlusBare(String),

    /// Bare word starting with - followed by non-letter/digit/dash: `-%`, etc.
    /// For rare cases. Lower priority than ShortFlag, Int, and DoubleDash.
    /// Excludes - after first - to avoid matching --name patterns.
    #[regex(r"-[^a-zA-Z0-9\s\-][^\s]*", lex_minus_bare, priority = 1)]
    MinusBare(String),

    /// Job specifier: `%1`, `%2` — the bash idiom for `wait`/`kill` targets.
    /// Keeps the leading `%` (kill uses it to distinguish a job from a PID;
    /// wait strips it). Without this token a bare `%1` is a lexer error.
    #[regex(r"%[0-9]+", lex_job_spec)]
    JobSpec(String),

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

    /// Braced variable reference: `${VAR}`, `${VAR.field}`, or a default
    /// form with a NESTED reference like `${X:-${Y}}` — value is the raw
    /// `${...}` text. The regex matches only the `${` opener; the callback
    /// extends the token to the BALANCED closing brace (GH #173 — a plain
    /// `[^}]+` regex stopped at the first `}` and split nested references).
    /// `${#VAR}` still lexes as `VarLength`: its full regex out-matches this
    /// two-character opener, so logos selects it first.
    #[regex(r"\$\{", lex_varref)]
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

    /// Variable string length: `${#VAR}` or a subscripted path `${#u[tags]}`.
    /// The trailing `(\[[^\]]*\])*` admits chained bracket subscripts so a
    /// length-of-path lexes in expression position, not just inside strings; the
    /// parser turns the captured inner into a `VarPath`.
    #[regex(r"\$\{#[a-zA-Z_][a-zA-Z0-9_]*(\[[^\]]*\])*\}", lex_var_length)]
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

    /// Digit-leading bareword: `019dda1c` (SHA prefix), UUIDs, version-ish
    /// strings. Distinguished from `Int` because at least one alpha character
    /// follows the leading digits — the lexer commits to "this is a string,
    /// not a number." Treated as a bareword string in expression position.
    #[regex(r"[0-9]+[a-zA-Z_][a-zA-Z0-9_.-]*", lex_number_ident, priority = 3)]
    NumberIdent(String),

    /// Numeric word containing an embedded hyphen run, or a minus-led numeric
    /// word with a non-numeric suffix. These are single contiguous shell words
    /// the user typed — ISO dates (`2024-01-02`), `N-M` ranges (`10-20`,
    /// `cut -f 1-3`, `tr -d 0-9`), float-dash forms (`1.5-2`), and `find`
    /// predicate values like `-1k` (smaller than 1k). Without this token they
    /// fragment into adjacent `Int`/`Float`/flag tokens and trip the
    /// no-token-pasting guard. The raw slice is preserved verbatim (so leading
    /// zeros survive). A plain `2024`/`1.5`/`-1` stays `Int`/`Float` — the
    /// digit-hyphen form requires a `-segment`, and the minus-led form requires
    /// an alpha after the digits.
    #[regex(r"[0-9]+(\.[0-9]+)?(-[0-9a-zA-Z._]+)+", lex_slice_word, priority = 3)]
    #[regex(r"-[0-9]+[a-zA-Z_][0-9a-zA-Z._-]*", lex_slice_word, priority = 3)]
    DashNumWord(String),

    /// Leading-`@` bareword: `@scope/pkg` (scoped package), `@0` (epoch in
    /// `date -d @0`), or bare `@`. Mid-word `@` (`user@host`) is handled by
    /// `Ident`; this covers the leading-`@` cases that would otherwise be an
    /// "unexpected character" lexer error.
    #[regex(r"@[a-zA-Z0-9_./@-]*", lex_slice_word, priority = 3)]
    AtWord(String),

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
    /// Allows dots for filenames like `script.kai` and `@` for `user@host`,
    /// `a@b.com` (bare `@` is an ordinary word character, as in bash).
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_.@-]*", lex_ident)]
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

    /// Backtick command substitution — explicitly rejected. Kaish drops
    /// backticks; the callback always errors so users get a dedicated
    /// `BackticksNotSupported` message instead of the generic
    /// `UnexpectedCharacter` they would have hit before. Backticks inside
    /// single/double-quoted strings, heredoc bodies, and comments don't
    /// reach this match — those tokens are matched as a single unit
    /// (strings) or extracted before logos runs (heredocs) or skipped to
    /// EOL (comments).
    #[token("`", reject_backtick)]
    BacktickRejected,
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
            | Token::HereString
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
            | Token::CmdSubstStart
            | Token::DotDotDot => TokenCategory::Punctuation,

            // Glob words (merged tokens containing wildcards)
            Token::GlobWord(_) => TokenCategory::Path,

            // Comments
            Token::Comment => TokenCategory::Comment,

            // Paths
            Token::Path(_)
            | Token::TildePath(_)
            | Token::RelativePath(_)
            | Token::Tilde
            | Token::DotDot
            | Token::DotSlashPath(_) => TokenCategory::Path,

            // Commands/identifiers (and bare words)
            Token::Ident(_)
            | Token::PlusBare(_)
            | Token::MinusBare(_)
            | Token::DoubleDashBare(_)
            | Token::MinusAlone
            | Token::NumberIdent(_)
            | Token::DashNumWord(_)
            | Token::AtWord(_)
            | Token::DottedIdent(_)
            | Token::JobSpec(_) => TokenCategory::Command,

            // Errors
            Token::InvalidFloatNoLeading
            | Token::InvalidFloatNoTrailing
            | Token::BacktickRejected => TokenCategory::Error,
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
/// Extend a `${` match across the remainder to the balanced closing `}`
/// and return the full `${...}` text for later parsing of path segments
/// and default words. Brace depth counts raw `{`/`}` characters, matching
/// the scanner's `${...}` region tracking (quote-blind, like the old
/// first-`}` regex — a quoted `}` inside a default word still closes; see
/// GH #173). An empty `${}` stays an error (as it was when the regex
/// required at least one inner character); a reference that never closes
/// is a loud `UnterminatedVarRef`.
fn lex_varref(lex: &mut logos::Lexer<Token>) -> Result<String, LexerError> {
    let mut depth = 1usize;
    let mut extra = 0usize;
    for c in lex.remainder().chars() {
        extra += c.len_utf8();
        match c {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    if extra == 1 {
                        // `${}` — empty reference, same error class the
                        // old non-matching regex produced.
                        return Err(LexerError::UnexpectedCharacter);
                    }
                    lex.bump(extra);
                    return Ok(lex.slice().to_string());
                }
            }
            _ => {}
        }
    }
    Err(LexerError::UnterminatedVarRef)
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

/// Lex a digit-leading bareword like `019dda1c` or `019dda1c-5b3f-7000`.
/// Distinguished from `Int` because at least one alpha character follows the
/// leading digits — the slice is treated as a string, not a number.
fn lex_number_ident(lex: &mut logos::Lexer<Token>) -> String {
    lex.slice().to_string()
}

/// Lex a dot-prefixed bareword like `.gitignore` or `.parent.parent`.
fn lex_dotted_ident(lex: &mut logos::Lexer<Token>) -> String {
    lex.slice().to_string()
}

/// Lex a bareword by capturing its raw slice verbatim (used by `DashNumWord`
/// and `AtWord`, where exact characters — e.g. leading zeros — must survive).
fn lex_slice_word(lex: &mut logos::Lexer<Token>) -> String {
    lex.slice().to_string()
}

/// Lex an invalid float without leading digit (like .5).
/// Always returns Err to produce a lexer error instead of a token.
fn lex_invalid_float_no_leading(_lex: &mut logos::Lexer<Token>) -> Result<(), LexerError> {
    Err(LexerError::InvalidFloatNoLeading)
}

/// Reject a backtick — kaish doesn't support backtick command substitution.
/// The dedicated error gives the user a `$(cmd)` hint instead of the generic
/// `UnexpectedCharacter` they would have hit otherwise.
fn reject_backtick(_lex: &mut logos::Lexer<Token>) -> Result<(), LexerError> {
    Err(LexerError::BackticksNotSupported)
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

/// Lex a double-dash bare word: `---` → `---`, `--=x` → `--=x` (keep the
/// full string; see GH #137).
fn lex_double_dash_bare(lex: &mut logos::Lexer<Token>) -> String {
    lex.slice().to_string()
}

/// Lex a job specifier: `%1` → `%1` (keep the leading `%`).
fn lex_job_spec(lex: &mut logos::Lexer<Token>) -> String {
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

/// Lex a dot-slash path: `./foo` → `./foo`
fn lex_dot_slash_path(lex: &mut logos::Lexer<Token>) -> String {
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
            Token::HereString => write!(f, "<<<"),
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
            Token::DotDotDot => write!(f, "..."),
            Token::Tilde => write!(f, "~"),
            Token::TildePath(s) => write!(f, "{}", s),
            Token::RelativePath(s) => write!(f, "{}", s),
            Token::DotSlashPath(s) => write!(f, "{}", s),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Star => write!(f, "*"),
            Token::Bang => write!(f, "!"),
            Token::Question => write!(f, "?"),
            Token::GlobWord(s) => write!(f, "GLOB({})", s),
            Token::Arithmetic(s) => write!(f, "ARITHMETIC({})", s),
            Token::CmdSubstStart => write!(f, "$("),
            Token::LongFlag(s) => write!(f, "--{}", s),
            Token::ShortFlag(s) => write!(f, "-{}", s),
            Token::PlusFlag(s) => write!(f, "+{}", s),
            Token::DoubleDash => write!(f, "--"),
            Token::DoubleDashBare(s) => write!(f, "{}", s),
            Token::PlusBare(s) => write!(f, "{}", s),
            Token::MinusBare(s) => write!(f, "{}", s),
            Token::JobSpec(s) => write!(f, "{}", s),
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
            Token::NumberIdent(s) => write!(f, "NUMIDENT({})", s),
            Token::DashNumWord(s) => write!(f, "DASHNUM({})", s),
            Token::AtWord(s) => write!(f, "ATWORD({})", s),
            Token::DottedIdent(s) => write!(f, "DOTIDENT({})", s),
            Token::Comment => write!(f, "COMMENT"),
            Token::Newline => write!(f, "NEWLINE"),
            Token::LineContinuation => write!(f, "LINECONT"),
            // These variants should never be produced — their callbacks always return errors
            Token::InvalidFloatNoLeading => write!(f, "INVALID_FLOAT_NO_LEADING"),
            Token::InvalidFloatNoTrailing => write!(f, "INVALID_FLOAT_NO_TRAILING"),
            Token::BacktickRejected => write!(f, "BACKTICK_REJECTED"),
        }
    }
}

impl Token {
    /// Returns true if this token is a keyword.
    // Must match the Keyword variants in `Token::category()` (minus the
    // TypeX variants, which `is_type()` covers separately). Currently
    // uncalled — kept exhaustive so future callers don't get wrong answers.
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
                | Token::While
                | Token::Case
                | Token::Esac
                | Token::Function
                | Token::Return
                | Token::Break
                | Token::Continue
                | Token::Exit
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
    // Currently uncalled — kept exhaustive so future callers don't get wrong answers.
    pub fn starts_statement(&self) -> bool {
        matches!(
            self,
            Token::Set
                | Token::Local
                | Token::Function
                | Token::If
                | Token::For
                | Token::While
                | Token::Case
                | Token::Ident(_)
                | Token::LBracket
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
                | Token::GlobWord(_)
                | Token::LastExitCode
                | Token::CurrentPid
        )
    }
}

// ═══════════════════════════════════════════════════════════════════
// Lexing pipeline (GH #95 rewrite)
//
// One composed source-order scanner extracts heredocs and arithmetic in
// a single quote/escape/comment-aware pass, producing a rewritten buffer
// plus a COMPLETE replacement table (heredocs included — the pre-#95
// pipeline recorded no replacements for heredocs, so every span after a
// heredoc drifted). logos then lexes the rewritten buffer; markers are
// resolved back to `Arithmetic`/`HereDoc` tokens POSITIONALLY (keyed by
// the replacement table, never by fishing identifier names); and finally
// the fusion passes merge span-adjacent runs using VERBATIM source
// slices (leading zeros survive — `Int(007)` never round-trips through
// `to_string()`).
// ═══════════════════════════════════════════════════════════════════

/// A text replacement performed by the scanner, in both coordinate
/// systems. `orig_*` addresses the original source; `new_*` addresses the
/// rewritten buffer fed to logos. The table is ordered by position and is
/// the single source of truth for span correction and marker resolution.
#[derive(Debug, Clone)]
struct Replacement {
    orig_start: usize,
    orig_len: usize,
    new_start: usize,
    new_len: usize,
    kind: ReplacementKind,
}

#[derive(Debug, Clone, PartialEq)]
enum ReplacementKind {
    /// `$((expr))` → arithmetic marker; index into `ScanOutput::arithmetics`.
    Arith(usize),
    /// Heredoc delimiter word → heredoc marker; index into `ScanOutput::heredocs`.
    HeredocIntro(usize),
    /// Heredoc body + terminating delimiter line, elided from the buffer.
    Elision,
}

/// Map a position from rewritten-buffer coordinates back to original-source
/// coordinates. `is_end` selects the boundary policy for half-open spans:
/// an END sitting exactly on a zero-width elision point must NOT be pushed
/// past the elided region, while a START there must be.
fn map_position(p: usize, is_end: bool, replacements: &[Replacement]) -> usize {
    let mut delta: isize = 0;
    for r in replacements {
        let r_end = r.new_start + r.new_len;
        let past = if is_end {
            p >= r_end && p > r.new_start
        } else {
            p >= r_end
        };
        if past {
            delta += r.orig_len as isize - r.new_len as isize;
        } else if p > r.new_start {
            // Strictly inside a replacement (a token glued onto a marker):
            // clamp into the original range.
            return r.orig_start + (p - r.new_start).min(r.orig_len);
        } else {
            break; // table is ordered; nothing further can affect p
        }
    }
    ((p as isize) + delta).max(0) as usize
}

fn map_span(span: &Span, replacements: &[Replacement]) -> Span {
    let start = map_position(span.start, false, replacements);
    let end = map_position(span.end, true, replacements).max(start);
    start..end
}

/// Per-heredoc data collected by the scanner.
///
/// `body` is the raw body bytes (tab stripping for `<<-` happens at
/// materialization). `body_start_offset` is the byte offset of the first
/// body character **in the original source** — exact, since the scanner
/// records every rewrite in the replacement table.
#[derive(Debug, Clone)]
struct HeredocExtract {
    body: String,
    literal: bool,
    strip_tabs: bool,
    body_start_offset: usize,
}

/// A heredoc whose introducer has been scanned but whose body hasn't been
/// collected yet — bodies start after the next unescaped newline, in
/// introducer order (`cat <<A <<B` queues two).
struct PendingHeredoc {
    delimiter: String,
    literal: bool,
    strip_tabs: bool,
    /// Span of the introducer (`<<` through the delimiter word) in
    /// original coordinates, for unterminated-heredoc errors.
    intro_span: Span,
}

/// Scanner output: the rewritten buffer plus everything needed to resolve
/// markers and correct spans.
struct ScanOutput {
    text: String,
    /// (marker, expression) pairs, indexed by `ReplacementKind::Arith`.
    arithmetics: Vec<(String, String)>,
    /// Heredoc extracts, indexed by `ReplacementKind::HeredocIntro`.
    heredocs: Vec<HeredocExtract>,
    replacements: Vec<Replacement>,
}

/// The one composed scanner: a single pass over the original source with
/// explicit quote/escape/comment state. Extracts `$((expr))` arithmetic
/// and `<<WORD` heredocs; everything else is copied verbatim. Because one
/// state machine owns all the context, the pre-#95 mutual-blindness bugs
/// (apostrophes in heredoc bodies poisoning arithmetic, `<<` inside
/// strings or comments misfiring heredoc collection) are structurally
/// impossible.
fn scan(source: &str) -> Result<ScanOutput, Spanned<LexerError>> {
    let chars: Vec<(usize, char)> = source.char_indices().collect();
    let n = chars.len();
    let total_len = source.len();
    let byte_at = |i: usize| -> usize {
        if i < n { chars[i].0 } else { total_len }
    };

    let mut out = String::with_capacity(source.len());
    let mut arithmetics: Vec<(String, String)> = Vec::new();
    let mut heredocs: Vec<HeredocExtract> = Vec::new();
    let mut replacements: Vec<Replacement> = Vec::new();
    let mut pending: Vec<PendingHeredoc> = Vec::new();

    let mut i = 0;
    // Tracks the previously copied character so `$#` (arg count) isn't
    // mistaken for a comment introducer.
    let mut prev_char: Option<char> = None;

    while i < n {
        let (pos, ch) = chars[i];

        // Backslash escape: copy both characters verbatim. This also
        // covers line continuations (`\` + newline) — the escaped newline
        // does not trigger heredoc body collection, matching how logos
        // treats it as a continuation, not a statement boundary.
        if ch == '\\' && i + 1 < n {
            out.push(ch);
            out.push(chars[i + 1].1);
            prev_char = Some(chars[i + 1].1);
            i += 2;
            continue;
        }

        match ch {
            // Single-quoted string: opaque. No heredocs, no arithmetic,
            // no comments inside.
            '\'' => {
                out.push(ch);
                i += 1;
                while i < n && chars[i].1 != '\'' {
                    out.push(chars[i].1);
                    i += 1;
                }
                if i < n {
                    out.push('\''); // closing quote
                    i += 1;
                }
                prev_char = Some('\'');
            }

            // Double-quoted string: arithmetic still expands inside;
            // heredocs and comments do not.
            '"' => {
                out.push(ch);
                i += 1;
                while i < n {
                    let (dpos, dch) = chars[i];
                    if dch == '\\' && i + 1 < n {
                        let next = chars[i + 1].1;
                        if next == '"' || next == '\\' || next == '$' || next == '`' {
                            out.push(dch);
                            out.push(next);
                            i += 2;
                            continue;
                        }
                    }
                    if dch == '"' {
                        out.push(dch);
                        i += 1;
                        break;
                    }
                    if dch == '$'
                        && i + 2 < n
                        && chars[i + 1].1 == '('
                        && chars[i + 2].1 == '('
                    {
                        extract_arithmetic(
                            &chars,
                            &mut i,
                            dpos,
                            total_len,
                            &mut out,
                            &mut arithmetics,
                            &mut replacements,
                        )?;
                        continue;
                    }
                    out.push(dch);
                    i += 1;
                }
                prev_char = Some('"');
            }

            // Comment: copy verbatim through end-of-line (logos tokenizes
            // and drops it). The `$` guard keeps `$#` (arg count) intact.
            '#' if prev_char != Some('$') => {
                while i < n && chars[i].1 != '\n' && chars[i].1 != '\r' {
                    out.push(chars[i].1);
                    i += 1;
                }
                prev_char = Some('#');
            }

            // `<<<` here-string passes through; `<<` starts a heredoc.
            '<' if i + 1 < n && chars[i + 1].1 == '<' => {
                if i + 2 < n && chars[i + 2].1 == '<' {
                    out.push_str("<<<");
                    i += 3;
                    prev_char = Some('<');
                    continue;
                }
                let heredoc_index = heredocs.len() + pending.len();
                scan_heredoc_introducer(
                    &chars,
                    &mut i,
                    pos,
                    &mut out,
                    &mut pending,
                    &mut replacements,
                    heredoc_index,
                );
                prev_char = Some('_'); // marker text ends with '_'
            }

            // `$((` arithmetic; `${...}` variable reference region.
            '$' if i + 2 < n && chars[i + 1].1 == '(' && chars[i + 2].1 == '(' => {
                extract_arithmetic(
                    &chars,
                    &mut i,
                    pos,
                    total_len,
                    &mut out,
                    &mut arithmetics,
                    &mut replacements,
                )?;
                prev_char = Some('_');
            }
            '$' if i + 1 < n && chars[i + 1].1 == '{' => {
                // Copy the ${...} region verbatim, tracking brace depth.
                // Arithmetic inside a bare ${...} cannot be represented
                // (the marker would leak into the reference text — the
                // pre-#95 pipeline silently corrupted this), so it is a
                // loud error instead.
                out.push('$');
                out.push('{');
                i += 2;
                let mut depth = 1usize;
                while i < n && depth > 0 {
                    let (vpos, vch) = chars[i];
                    if vch == '$'
                        && i + 2 < n
                        && chars[i + 1].1 == '('
                        && chars[i + 2].1 == '('
                    {
                        return Err(Spanned::new(
                            LexerError::ArithmeticInVarRef,
                            vpos..(byte_at(i + 3)),
                        ));
                    }
                    match vch {
                        '{' => depth += 1,
                        '}' => depth -= 1,
                        _ => {}
                    }
                    out.push(vch);
                    i += 1;
                }
                prev_char = Some('}');
            }

            // Unescaped newline: copy it, then collect any pending
            // heredoc bodies (in introducer order).
            '\n' => {
                out.push('\n');
                i += 1;
                prev_char = Some('\n');
                if !pending.is_empty() {
                    collect_heredoc_bodies(
                        &chars,
                        &mut i,
                        total_len,
                        out.len(),
                        &mut pending,
                        &mut heredocs,
                        &mut replacements,
                    )?;
                }
            }

            // Bare `\r` (Mac-classic line ending) terminating a heredoc
            // introducer line: normalize to `\n` (same byte length, so
            // spans are unaffected) so logos sees a real Newline, and
            // collect the pending bodies. A CRLF pair falls through to
            // the `\n` arm via the default copy of `\r`; a stray bare
            // `\r` with no heredoc pending stays verbatim (and stays a
            // lexer error, as before).
            '\r' if !pending.is_empty()
                && chars.get(i + 1).map(|c| c.1) != Some('\n') =>
            {
                out.push('\n');
                i += 1;
                prev_char = Some('\n');
                collect_heredoc_bodies(
                    &chars,
                    &mut i,
                    total_len,
                    out.len(),
                    &mut pending,
                    &mut heredocs,
                    &mut replacements,
                )?;
            }

            _ => {
                out.push(ch);
                i += 1;
                prev_char = Some(ch);
            }
        }
    }

    // EOF with heredoc introducers whose bodies never started (no newline
    // after the introducer line).
    if let Some(p) = pending.first() {
        return Err(Spanned::new(
            LexerError::UnterminatedHeredoc {
                delimiter: p.delimiter.clone(),
            },
            p.intro_span.clone(),
        ));
    }

    Ok(ScanOutput {
        text: out,
        arithmetics,
        heredocs,
        replacements,
    })
}

/// Extract `$((expr))` starting at `chars[*i]` (the `$`). Emits a unique
/// marker into `out` and records the replacement. Single `)` characters
/// inside the expression are kept (only a `))` pair at depth zero closes),
/// matching the pre-#95 collector.
fn extract_arithmetic(
    chars: &[(usize, char)],
    i: &mut usize,
    start_pos: usize,
    total_len: usize,
    out: &mut String,
    arithmetics: &mut Vec<(String, String)>,
    replacements: &mut Vec<Replacement>,
) -> Result<(), Spanned<LexerError>> {
    let n = chars.len();
    *i += 3; // consume `$((`

    let mut expr = String::new();
    let mut depth = 0usize;
    let mut closed = false;

    while *i < n {
        let c = chars[*i].1;
        match c {
            '(' => {
                depth += 1;
                if depth > MAX_PAREN_DEPTH {
                    return Err(Spanned::new(
                        LexerError::NestingTooDeep,
                        start_pos..chars[*i].0,
                    ));
                }
                expr.push('(');
                *i += 1;
            }
            ')' => {
                if depth > 0 {
                    depth -= 1;
                    expr.push(')');
                    *i += 1;
                } else if *i + 1 < n && chars[*i + 1].1 == ')' {
                    *i += 2;
                    closed = true;
                    break;
                } else if *i + 1 == n {
                    // Lone `)` at EOF can never be followed by its pair.
                    break;
                } else {
                    expr.push(')');
                    *i += 1;
                }
            }
            _ => {
                expr.push(c);
                *i += 1;
            }
        }
    }

    if !closed {
        // Don't silently evaluate a partial expression (`$(( 1 + 2` must
        // not become `3`).
        return Err(Spanned::new(
            LexerError::UnterminatedArithmetic,
            start_pos..total_len,
        ));
    }

    let end_pos = if *i < n { chars[*i].0 } else { total_len };
    let marker = format!("__KAISH_ARITH_{}__", unique_marker_id());
    replacements.push(Replacement {
        orig_start: start_pos,
        orig_len: end_pos - start_pos,
        new_start: out.len(),
        new_len: marker.len(),
        kind: ReplacementKind::Arith(arithmetics.len()),
    });
    arithmetics.push((marker.clone(), expr));
    out.push_str(&marker);
    Ok(())
}

/// Scan a heredoc introducer starting at `chars[*i]` (the first `<`).
/// Collects the delimiter word bash-style (whole word, quote removal,
/// `literal` if any part was quoted), emits `<<` plus a unique marker, and
/// queues the body for collection at the next unescaped newline. If no
/// delimiter word follows, `<<` is copied verbatim (logos will surface
/// the syntax error).
fn scan_heredoc_introducer(
    chars: &[(usize, char)],
    i: &mut usize,
    intro_start: usize,
    out: &mut String,
    pending: &mut Vec<PendingHeredoc>,
    replacements: &mut Vec<Replacement>,
    heredoc_index: usize,
) {
    let n = chars.len();
    *i += 2; // consume `<<`

    let strip_tabs = *i < n && chars[*i].1 == '-';
    if strip_tabs {
        *i += 1;
    }

    // Skip horizontal whitespace before the delimiter word.
    while *i < n && (chars[*i].1 == ' ' || chars[*i].1 == '\t') {
        *i += 1;
    }

    // Collect the delimiter word with bash-style quote removal: the word
    // runs until unquoted whitespace; single/double quotes are stripped
    // and any quoting makes the heredoc literal (`<<'EOF'` and `<<EO"F"`
    // both suppress interpolation).
    let mut delimiter = String::new();
    let mut literal = false;
    while *i < n {
        let c = chars[*i].1;
        match c {
            '\'' | '"' => {
                literal = true;
                let quote = c;
                *i += 1;
                while *i < n && chars[*i].1 != quote {
                    delimiter.push(chars[*i].1);
                    *i += 1;
                }
                if *i < n {
                    *i += 1; // closing quote
                }
            }
            c if c.is_whitespace() => break,
            c => {
                delimiter.push(c);
                *i += 1;
            }
        }
    }
    let word_end = if *i < n {
        chars[*i].0
    } else {
        chars
            .last()
            .map(|(pos, c)| pos + c.len_utf8())
            .unwrap_or(intro_start + 2)
    };

    if delimiter.is_empty() {
        // Not a heredoc after all — emit what we consumed verbatim.
        out.push_str("<<");
        if strip_tabs {
            out.push('-');
        }
        return;
    }

    let marker = format!("__KAISH_HEREDOC_{}__", unique_marker_id());
    out.push_str("<<");
    replacements.push(Replacement {
        // The replaced original region runs from just after `<<` (the
        // optional `-` and whitespace included) through the delimiter
        // word; the marker stands in for all of it.
        orig_start: intro_start + 2,
        orig_len: word_end - (intro_start + 2),
        new_start: out.len(),
        new_len: marker.len(),
        kind: ReplacementKind::HeredocIntro(heredoc_index),
    });
    out.push_str(&marker);
    pending.push(PendingHeredoc {
        delimiter,
        literal,
        strip_tabs,
        intro_span: intro_start..word_end,
    });
}

/// Collect the bodies of all pending heredocs, in introducer order,
/// starting at `chars[*i]` (the character after the newline that ended
/// the introducer line). Bodies (and their terminating delimiter lines)
/// are elided from the rewritten buffer; each elision is recorded so
/// spans after the heredoc stay exact.
fn collect_heredoc_bodies(
    chars: &[(usize, char)],
    i: &mut usize,
    total_len: usize,
    out_len: usize,
    pending: &mut Vec<PendingHeredoc>,
    heredocs: &mut Vec<HeredocExtract>,
    replacements: &mut Vec<Replacement>,
) -> Result<(), Spanned<LexerError>> {
    let n = chars.len();

    for p in pending.drain(..) {
        let body_start = if *i < n { chars[*i].0 } else { total_len };
        let mut body = String::new();
        let mut found = false;

        while !found {
            if *i >= n {
                // EOF: a final unterminated line was already checked below;
                // reaching here means the delimiter never appeared.
                return Err(Spanned::new(
                    LexerError::UnterminatedHeredoc {
                        delimiter: p.delimiter.clone(),
                    },
                    p.intro_span.clone(),
                ));
            }

            // Read one line and its terminator (`\n`, `\r\n`, bare `\r`,
            // or EOF). The terminator is preserved verbatim in the body;
            // delimiter comparison strips it.
            let mut line = String::new();
            let mut terminator = "";
            let mut at_eof = false;
            loop {
                if *i >= n {
                    at_eof = true;
                    break;
                }
                let c = chars[*i].1;
                if c == '\n' {
                    *i += 1;
                    terminator = "\n";
                    break;
                }
                if c == '\r' {
                    *i += 1;
                    if *i < n && chars[*i].1 == '\n' {
                        *i += 1;
                        terminator = "\r\n";
                    } else {
                        terminator = "\r";
                    }
                    break;
                }
                line.push(c);
                *i += 1;
            }

            let compare = if p.strip_tabs {
                line.trim_start_matches('\t')
            } else {
                line.as_str()
            };
            if compare == p.delimiter {
                found = true;
            } else if at_eof {
                // The source ended without the closing delimiter. Crash
                // rather than silently using what was collected — missing
                // data is exactly where a silent fallback masks the bug.
                return Err(Spanned::new(
                    LexerError::UnterminatedHeredoc {
                        delimiter: p.delimiter.clone(),
                    },
                    p.intro_span.clone(),
                ));
            } else {
                body.push_str(&line);
                body.push_str(terminator);
            }
        }

        let elide_end = if *i < n { chars[*i].0 } else { total_len };
        replacements.push(Replacement {
            orig_start: body_start,
            orig_len: elide_end - body_start,
            new_start: out_len,
            new_len: 0,
            kind: ReplacementKind::Elision,
        });

        // For interpolated (non-literal) bodies, rewrite arithmetic to the
        // `${__ARITH:expr__}` form the interpolation parser understands
        // (see `parse_interpolated_string`). Literal bodies stay verbatim
        // — a `$((` there is prose, never an expression (this is the
        // pre-#95 false-positive fix). Bash expands `$(( ))` in heredoc
        // bodies regardless of quotes within the body, so the body scan
        // is deliberately quote-blind; `\$((` escapes it.
        let body = if p.literal {
            body
        } else {
            rewrite_body_arithmetic(&body, &p)?
        };

        heredocs.push(HeredocExtract {
            body,
            literal: p.literal,
            strip_tabs: p.strip_tabs,
            body_start_offset: body_start,
        });
    }

    Ok(())
}

/// Rewrite `$((expr))` inside an interpolated heredoc body to
/// `${__ARITH:expr__}`. `\$((` stays literal (minus nothing — the
/// backslash is preserved for the interpolation parser). An unterminated
/// `$((` in the body is a loud error, matching bash (which would fail the
/// expansion) and the shell's crash-over-corrupt stance.
fn rewrite_body_arithmetic(
    body: &str,
    p: &PendingHeredoc,
) -> Result<String, Spanned<LexerError>> {
    if !body.contains("$((") {
        return Ok(body.to_string());
    }
    let chars: Vec<char> = body.chars().collect();
    let n = chars.len();
    let mut out = String::with_capacity(body.len());
    let mut i = 0;
    while i < n {
        if chars[i] == '\\' && i + 1 < n {
            out.push(chars[i]);
            out.push(chars[i + 1]);
            i += 2;
            continue;
        }
        if chars[i] == '$' && i + 2 < n && chars[i + 1] == '(' && chars[i + 2] == '(' {
            i += 3;
            let mut expr = String::new();
            let mut depth = 0usize;
            let mut closed = false;
            while i < n {
                let c = chars[i];
                match c {
                    '(' => {
                        depth += 1;
                        expr.push('(');
                        i += 1;
                    }
                    ')' => {
                        if depth > 0 {
                            depth -= 1;
                            expr.push(')');
                            i += 1;
                        } else if i + 1 < n && chars[i + 1] == ')' {
                            i += 2;
                            closed = true;
                            break;
                        } else {
                            expr.push(')');
                            i += 1;
                        }
                    }
                    _ => {
                        expr.push(c);
                        i += 1;
                    }
                }
            }
            if !closed {
                return Err(Spanned::new(
                    LexerError::UnterminatedArithmetic,
                    p.intro_span.clone(),
                ));
            }
            out.push_str(&format!("${{__ARITH:{}__}}", expr));
            continue;
        }
        out.push(chars[i]);
        i += 1;
    }
    Ok(out)
}

// ═══════════════════════════════════════════════════════════════════
// Marker resolution (positional)
// ═══════════════════════════════════════════════════════════════════

/// Resolve scanner markers back into real tokens, keyed by POSITION in the
/// replacement table (never by matching identifier text):
///
/// - an `Ident` exactly covering an arithmetic marker becomes `Arithmetic`;
/// - a `String` containing marker text (arithmetic inside a double-quoted
///   string) gets the `${__ARITH:expr__}` content swap the interpolation
///   parser understands;
/// - a word token GLUED onto a marker (`$((1+2))abc`) is SPLIT into the
///   `Arithmetic` plus re-lexed word fragments — span-adjacent, so the
///   parser's no-token-pasting guard rejects it loudly with a quoting hint
///   (the pre-#95 pipeline leaked raw marker text here);
/// - the `Ident` after a `HereDocStart` covering a heredoc marker becomes
///   the `HereDoc` token.
///
/// Tokens carry rewritten-buffer spans on entry and exit; the caller maps
/// them to original coordinates afterwards.
fn resolve_markers(
    tokens: Vec<Spanned<Token>>,
    scan: &ScanOutput,
) -> Result<Vec<Spanned<Token>>, Vec<Spanned<LexerError>>> {
    let markers: Vec<&Replacement> = scan
        .replacements
        .iter()
        .filter(|r| !matches!(r.kind, ReplacementKind::Elision))
        .collect();

    let mut result = Vec::with_capacity(tokens.len());
    let mut mi = 0usize;

    for spanned in tokens {
        let span = spanned.span.clone();
        while mi < markers.len() && markers[mi].new_start + markers[mi].new_len <= span.start {
            mi += 1;
        }

        // Collect the markers contained in this token's span.
        let mut contained = Vec::new();
        let mut mj = mi;
        while mj < markers.len() {
            let m = markers[mj];
            if m.new_start >= span.end {
                break;
            }
            if m.new_start >= span.start && m.new_start + m.new_len <= span.end {
                contained.push(m);
            }
            mj += 1;
        }

        if contained.is_empty() {
            result.push(spanned);
            continue;
        }

        match (&spanned.token, contained.as_slice()) {
            // Exact cover by a single arithmetic marker → Arithmetic token.
            (Token::Ident(_), [m])
                if matches!(m.kind, ReplacementKind::Arith(_))
                    && m.new_start == span.start
                    && m.new_start + m.new_len == span.end =>
            {
                let ReplacementKind::Arith(idx) = m.kind else {
                    unreachable!("guarded by matches! above")
                };
                result.push(Spanned::new(
                    Token::Arithmetic(scan.arithmetics[idx].1.clone()),
                    span,
                ));
            }

            // Exact cover by a heredoc marker → HereDoc token (the parser
            // pairs it with the preceding HereDocStart).
            (Token::Ident(_), [m])
                if matches!(m.kind, ReplacementKind::HeredocIntro(_))
                    && m.new_start == span.start
                    && m.new_start + m.new_len == span.end =>
            {
                let ReplacementKind::HeredocIntro(idx) = m.kind else {
                    unreachable!("guarded by matches! above")
                };
                let hd = &scan.heredocs[idx];
                result.push(Spanned::new(
                    Token::HereDoc(HereDocData {
                        content: hd.body.clone(),
                        literal: hd.literal,
                        strip_tabs: hd.strip_tabs,
                        body_start_offset: hd.body_start_offset,
                    }),
                    span,
                ));
            }

            // Arithmetic inside a double-quoted string: swap each marker's
            // text in the CONTENT for the interpolation form. Escape
            // processing never alters marker text (alphanumerics and
            // underscores), so a plain replace is exact.
            (Token::String(s), ms) => {
                let mut content = s.clone();
                for m in ms {
                    let ReplacementKind::Arith(idx) = m.kind else {
                        // A heredoc marker inside a string would mean the
                        // scanner rewrote inside a quoted region — it
                        // never does.
                        unreachable!("heredoc marker inside string content")
                    };
                    let (marker, expr) = &scan.arithmetics[idx];
                    content =
                        content.replacen(marker, &format!("${{__ARITH:{}__}}", expr), 1);
                }
                result.push(Spanned::new(Token::String(content), span));
            }

            // A word token glued onto marker(s): split into fragments and
            // Arithmetic tokens. The fragments are re-lexed so `007` or
            // `-3` keep their real token identities; span adjacency then
            // triggers the parser's no-pasting guard — a loud error where
            // the old pipeline leaked marker text.
            _ => {
                let mut cursor = span.start;
                for m in &contained {
                    if m.new_start > cursor {
                        relex_fragment(
                            &scan.text[cursor..m.new_start],
                            cursor,
                            &mut result,
                        )?;
                    }
                    match m.kind {
                        ReplacementKind::Arith(idx) => {
                            result.push(Spanned::new(
                                Token::Arithmetic(scan.arithmetics[idx].1.clone()),
                                m.new_start..m.new_start + m.new_len,
                            ));
                        }
                        ReplacementKind::HeredocIntro(_) | ReplacementKind::Elision => {
                            // Heredoc markers are always delimited by the
                            // `<<` before them and line layout after; they
                            // can't glue into a larger word.
                            unreachable!("heredoc marker glued into word token")
                        }
                    }
                    cursor = m.new_start + m.new_len;
                }
                if cursor < span.end {
                    relex_fragment(&scan.text[cursor..span.end], cursor, &mut result)?;
                }
            }
        }

        mi = mj;
    }

    Ok(result)
}

/// Re-lex a fragment of a split word token, offsetting spans by `base`.
fn relex_fragment(
    fragment: &str,
    base: usize,
    result: &mut Vec<Spanned<Token>>,
) -> Result<(), Vec<Spanned<LexerError>>> {
    let mut errors = Vec::new();
    for (tok, span) in Token::lexer(fragment).spanned() {
        let span = base + span.start..base + span.end;
        match tok {
            Ok(t) => result.push(Spanned::new(t, span)),
            Err(e) => errors.push(Spanned::new(e, span)),
        }
    }
    if errors.is_empty() { Ok(()) } else { Err(errors) }
}

// ═══════════════════════════════════════════════════════════════════
// Value-context analysis (one pass, explicit stack)
// ═══════════════════════════════════════════════════════════════════

/// Per-token context for the fusion passes: is this token part of a
/// value-position collection literal? Both merge passes suppress fusion
/// there so `x=[dog]` / `{port:8080}` reach the parser as primitive
/// tokens instead of a fused `GlobWord`/`Ident` — see
/// docs/arrays-and-hashes.md ("Implementation notes").
#[derive(Clone, Copy, Default)]
struct ValueContext {
    /// Inside (or opening) a value-position `[`/`{` literal — suppresses
    /// glob-merge bracket-pair fusion.
    in_literal: bool,
    /// Inside a value-position `{` record literal specifically —
    /// suppresses colon-merge fusion. Narrower than `in_literal` on
    /// purpose: a plain scalar assignment `x=foo:bar` must keep fusing.
    in_brace: bool,
}

/// Structural frames tracked by the context walker. The stack replaces the
/// pre-#95 bare counters: mismatched closers become detectable, `$( )`
/// bodies get fresh statement context (no more `[[ -n $(x=[a]) ]]`
/// test-depth leaks), and dangling literal state can be dropped at
/// statement boundaries instead of poisoning the rest of the buffer.
#[derive(Debug, Clone, Copy, PartialEq)]
enum Frame {
    /// `[[ ... ]]` test expression: `=` is comparison, `in` is membership.
    Test,
    /// Value-position `[ ... ]` list literal: bracket fusion suppressed.
    List,
    /// Value-position `{ ... }` record literal: colon fusion suppressed.
    Record,
    /// `$( ... )` command substitution: a fresh statement scope.
    Subst,
    /// Plain `( ... )` grouping (function parameter lists): inert, tracked
    /// only so `)` pops the right frame.
    Paren,
}

/// Statement-head DFA: decides whether an `=` is an ASSIGNMENT (which puts
/// the following tokens at value position — `x = [a b]` is a legal spaced
/// assignment with a list-literal RHS) or an argv-position literal
/// (`grep -E = [a-z]*` must keep glob-fusing). The pre-#95 pipeline
/// treated EVERY `=` outside `[[ ]]` as value-opening; the DFA follows the
/// grammar instead: an assignment's LHS is the first word of a statement
/// (after optional `local`), an identifier root plus optionally glued
/// `[subscript]` groups. After an assignment's value completes the DFA
/// returns to statement-head state, covering env-prefix chains
/// (`A=1 B=2 cmd`).
#[derive(Debug, Clone, Copy, PartialEq)]
enum StmtHead {
    /// At a statement start: the next identifier could be an lvalue root.
    Start,
    /// Consumed `local`; still expecting the lvalue root.
    AfterLocal,
    /// Consumed an identifier root (and any glued subscript groups);
    /// `usize` is the byte offset just past the last consumed token, for
    /// subscript-gluing checks.
    Lvalue(usize),
    /// Inside a glued `[subscript]` group on the LHS; `usize` is bracket
    /// depth within the group.
    LvalueSubscript(usize),
    /// The `=` fired; consuming the RHS value (frames may open and close
    /// during it). Returns to `Start` when the value completes.
    Value,
    /// Past the command word: `=` here is argv text, never an assignment.
    Argv,
}

/// Tokens that terminate a statement (or arm/branch) and reset the
/// statement-head DFA. `Newline` is deliberately absent from the FRAME
/// reset set (multiline list/record literals are legal — the parser
/// consumes interior newlines) but does reset the DFA when no literal is
/// open, and pops dangling `Test` frames (kaish's `[[ ]]` grammar is
/// single-line).
fn is_statement_boundary(token: &Token) -> bool {
    // `LBrace`/`RBrace` also reset the DFA, but they have dedicated match
    // arms (record-literal vs block-brace discrimination) that run before
    // the boundary wildcard, so they are deliberately absent here.
    matches!(
        token,
        Token::Newline
            | Token::Semi
            | Token::DoubleSemi
            | Token::And
            | Token::Or
            | Token::Pipe
            | Token::Amp
            | Token::If
            | Token::Then
            | Token::Elif
            | Token::Else
            | Token::Fi
            | Token::While
            | Token::Do
            | Token::Done
            | Token::For
            | Token::Case
            | Token::Esac
            | Token::In
    )
}

fn compute_value_context(tokens: &[Spanned<Token>]) -> Vec<ValueContext> {
    let mut ctx = vec![ValueContext::default(); tokens.len()];

    // Frame stack plus per-scope statement DFA. `scopes` parallels the
    // Subst frames: scopes[0] is the top-level statement scope; pushing a
    // Subst pushes a fresh scope.
    let mut frames: Vec<Frame> = Vec::new();
    let mut scopes: Vec<StmtHead> = vec![StmtHead::Start];
    let mut expect_value = false;
    // Set after consuming the first token of a `[[`/`]]` pair so the
    // partner bracket has no structural effect.
    let mut skip_paired_bracket = false;

    // Number of frames below the current scope's floor (frames belonging
    // to enclosing scopes, frozen while this scope is active).
    let mut scope_floors: Vec<usize> = vec![0];

    for i in 0..tokens.len() {
        let tok = &tokens[i].token;
        let span = &tokens[i].span;

        let floor = *scope_floors.last().unwrap_or(&0);
        let top = frames.last().copied();
        let in_open_literal = frames.len() > floor
            && matches!(top, Some(Frame::List) | Some(Frame::Record));

        ctx[i] = ValueContext {
            in_literal: expect_value || in_open_literal,
            in_brace: matches!(top, Some(Frame::Record)),
        };

        // `in` membership: value position only inside a `[[ ]]` test in
        // the current scope (a `for`/`case` head `in` sits outside any
        // Test frame and opens nothing).
        let in_test = frames[floor..].contains(&Frame::Test);

        let opens_value = expect_value;
        expect_value = false;

        if skip_paired_bracket {
            skip_paired_bracket = false;
            continue;
        }

        match tok {
            Token::LBracket => {
                let next_adjacent_lbracket = tokens.get(i + 1).is_some_and(|t| {
                    matches!(t.token, Token::LBracket) && t.span.start == span.end
                });
                let dfa = scopes.last_mut().unwrap_or_else(|| unreachable!("scopes never empty"));
                if let StmtHead::Lvalue(end) = *dfa {
                    // A glued `[` after an lvalue root starts a subscript
                    // group, not a literal or a test.
                    if span.start == end {
                        *dfa = StmtHead::LvalueSubscript(1);
                        continue;
                    }
                }
                if let StmtHead::LvalueSubscript(depth) = *dfa {
                    *dfa = StmtHead::LvalueSubscript(depth + 1);
                    continue;
                }
                if opens_value || in_open_literal {
                    frames.push(Frame::List);
                } else if next_adjacent_lbracket {
                    // `[[` opens a test. The value/literal guards above
                    // keep a glued nested list (`x=[[a] [b]]`) as literal
                    // brackets rather than a bogus test.
                    frames.push(Frame::Test);
                    skip_paired_bracket = true;
                }
                // A lone `[` in argv position (`ls [dog]`, a `[0-9]`
                // char-class) has no structural effect.
            }
            Token::RBracket => {
                let dfa = scopes.last_mut().unwrap_or_else(|| unreachable!("scopes never empty"));
                if let StmtHead::LvalueSubscript(depth) = *dfa {
                    *dfa = if depth == 1 {
                        StmtHead::Lvalue(span.end)
                    } else {
                        StmtHead::LvalueSubscript(depth - 1)
                    };
                    continue;
                }
                let next_adjacent_rbracket = tokens.get(i + 1).is_some_and(|t| {
                    matches!(t.token, Token::RBracket) && t.span.start == span.end
                });
                if frames.len() > floor && top == Some(Frame::List) {
                    frames.pop();
                    if frames.len() == floor {
                        // The literal was an assignment's RHS: the value
                        // is complete, back to statement-head state
                        // (`x=[a] y=[b]` chains).
                        let dfa = scopes
                            .last_mut()
                            .unwrap_or_else(|| unreachable!("scopes never empty"));
                        if *dfa == StmtHead::Value {
                            *dfa = StmtHead::Start;
                        }
                    }
                } else if frames.len() > floor
                    && top == Some(Frame::Test)
                    && next_adjacent_rbracket
                {
                    frames.pop();
                    skip_paired_bracket = true;
                }
            }
            Token::LBrace => {
                if opens_value || in_open_literal {
                    frames.push(Frame::Record);
                } else {
                    // Block-open `{` (function bodies): new statement.
                    *scopes.last_mut().unwrap_or_else(|| unreachable!("scopes never empty")) =
                        StmtHead::Start;
                }
            }
            Token::RBrace => {
                if frames.len() > floor && top == Some(Frame::Record) {
                    frames.pop();
                    if frames.len() == floor {
                        let dfa = scopes
                            .last_mut()
                            .unwrap_or_else(|| unreachable!("scopes never empty"));
                        if *dfa == StmtHead::Value {
                            *dfa = StmtHead::Start;
                        }
                    }
                } else {
                    *scopes.last_mut().unwrap_or_else(|| unreachable!("scopes never empty")) =
                        StmtHead::Start;
                }
            }
            Token::CmdSubstStart => {
                frames.push(Frame::Subst);
                scope_floors.push(frames.len());
                scopes.push(StmtHead::Start);
            }
            Token::LParen => {
                frames.push(Frame::Paren);
            }
            Token::RParen => {
                // Pop through any dangling literal/test frames to the
                // nearest Subst/Paren — an unterminated literal inside
                // `$( )` must not leak into the enclosing scope.
                while let Some(f) = frames.last() {
                    match f {
                        Frame::Subst => {
                            frames.pop();
                            scope_floors.pop();
                            scopes.pop();
                            if scopes.is_empty() {
                                scopes.push(StmtHead::Start);
                            }
                            if scope_floors.is_empty() {
                                scope_floors.push(0);
                            }
                            // The substitution may have been an
                            // assignment's RHS in the enclosing scope
                            // (`x=$(cmd) y=2`): its value is complete.
                            let enclosing_floor = *scope_floors.last().unwrap_or(&0);
                            if frames.len() == enclosing_floor {
                                let dfa = scopes
                                    .last_mut()
                                    .unwrap_or_else(|| unreachable!("scopes never empty"));
                                if *dfa == StmtHead::Value {
                                    *dfa = StmtHead::Start;
                                }
                            }
                            break;
                        }
                        Frame::Paren => {
                            frames.pop();
                            break;
                        }
                        _ => {
                            frames.pop();
                        }
                    }
                }
            }
            Token::Eq => {
                let dfa = scopes.last_mut().unwrap_or_else(|| unreachable!("scopes never empty"));
                if matches!(*dfa, StmtHead::Lvalue(_)) && !in_test {
                    // Assignment `=`: the RHS is at value position.
                    expect_value = true;
                    *dfa = StmtHead::Value;
                } else if matches!(*dfa, StmtHead::Value) {
                    // `=` while consuming a value (e.g. `x = a=b`): argv
                    // text from here on.
                    *dfa = StmtHead::Argv;
                }
                // Comparison `=` inside `[[ ]]` and argv `=` open nothing.
            }
            Token::In if in_test => {
                expect_value = true;
            }
            t if is_statement_boundary(t) => {
                // Reset the statement DFA; pop dangling literal frames at
                // hard separators. Newline keeps List/Record open
                // (multiline literals are legal) but closes Test (the
                // `[[ ]]` grammar is single-line).
                *scopes.last_mut().unwrap_or_else(|| unreachable!("scopes never empty")) =
                    StmtHead::Start;
                match t {
                    Token::Newline => {
                        while frames.len() > floor && frames.last() == Some(&Frame::Test) {
                            frames.pop();
                        }
                    }
                    Token::Semi
                    | Token::DoubleSemi
                    | Token::Pipe
                    | Token::Amp
                    | Token::And
                    | Token::Or => {
                        while frames.len() > floor
                            && matches!(
                                frames.last(),
                                Some(Frame::Test) | Some(Frame::List) | Some(Frame::Record)
                            )
                        {
                            frames.pop();
                        }
                    }
                    _ => {}
                }
            }
            _ => {
                // Ordinary token: advance the statement DFA when no
                // literal frame is open in this scope.
                if !in_open_literal {
                    let dfa =
                        scopes.last_mut().unwrap_or_else(|| unreachable!("scopes never empty"));
                    *dfa = match (*dfa, tok) {
                        (StmtHead::Start, Token::Local) => StmtHead::AfterLocal,
                        (StmtHead::Start, Token::Ident(_)) => StmtHead::Lvalue(span.end),
                        (StmtHead::AfterLocal, Token::Ident(_)) => StmtHead::Lvalue(span.end),
                        (StmtHead::LvalueSubscript(d), _) => StmtHead::LvalueSubscript(d),
                        (StmtHead::Value, _) => StmtHead::Start,
                        _ => StmtHead::Argv,
                    };
                }
            }
        }
    }

    ctx
}

// ═══════════════════════════════════════════════════════════════════
// Fusion passes
//
// All fused text is a VERBATIM slice of the original source — never
// rebuilt from token values — so `a:007` stays `a:007` and `007*` globs
// as `007*` (the pre-#95 passes rebuilt through `Int::to_string()` and
// dropped leading zeros). Runs are span-adjacent by construction, so the
// slice is exact; a replacement boundary can never sit inside a run
// because marker-derived tokens (`Arithmetic`, `HereDoc`) are not
// mergeable.
// ═══════════════════════════════════════════════════════════════════

/// True for token types that can participate in colon-adjacent merging.
fn is_colon_mergeable(token: &Token) -> bool {
    matches!(
        token,
        Token::Ident(_)
            | Token::NumberIdent(_)
            | Token::DashNumWord(_)
            | Token::AtWord(_)
            | Token::DottedIdent(_)
            | Token::Colon
            | Token::Int(_)
            | Token::Path(_)
            | Token::Float(_)
    )
}

/// Merge span-adjacent token runs containing `Token::Colon` into single
/// `Ident` tokens.
///
/// In bash, `:` is a regular character in unquoted words. kaish tokenizes
/// it separately, which breaks Rust paths (`foo::bar`), URLs
/// (`host:8080`), etc. This pass fuses span-adjacent mergeable tokens
/// into a single `Ident` when the run contains at least one `Colon`.
/// A run that opens inside a value-position record literal
/// (`{port:8080}`) is exempted — see `compute_value_context` — so the
/// record parser sees the `Colon` as its own token.
fn merge_colon_adjacent(tokens: Vec<Spanned<Token>>, source: &str) -> Vec<Spanned<Token>> {
    if tokens.is_empty() {
        return tokens;
    }

    let value_ctx = compute_value_context(&tokens);
    let mut result = Vec::with_capacity(tokens.len());
    let mut run: Vec<&Spanned<Token>> = Vec::new();
    let mut run_start = 0usize;

    for (idx, token) in tokens.iter().enumerate() {
        if run.is_empty() {
            if is_colon_mergeable(&token.token) {
                run.push(token);
                run_start = idx;
            } else {
                result.push(token.clone());
            }
            continue;
        }

        // Safety: run is non-empty (checked above)
        let Some(last) = run.last() else { unreachable!() };
        let adjacent = last.span.end == token.span.start;

        if adjacent && is_colon_mergeable(&token.token) {
            run.push(token);
        } else {
            flush_colon_run(&mut run, &mut result, value_ctx[run_start].in_brace, source);
            if is_colon_mergeable(&token.token) {
                run.push(token);
                run_start = idx;
            } else {
                result.push(token.clone());
            }
        }
    }

    flush_colon_run(&mut run, &mut result, value_ctx[run_start].in_brace, source);

    result
}

/// Flush a run of colon-mergeable tokens: merge to a single `Ident` (text
/// sliced verbatim from the source) if it contains a colon, otherwise emit
/// individually. `suppress` (true when the run opened inside a
/// value-position record literal) forces individual emission.
fn flush_colon_run(
    run: &mut Vec<&Spanned<Token>>,
    result: &mut Vec<Spanned<Token>>,
    suppress: bool,
    source: &str,
) {
    if run.is_empty() {
        return;
    }

    let has_colon = run.iter().any(|t| matches!(t.token, Token::Colon));

    if !suppress && run.len() >= 2 && has_colon {
        let start = run.first().map(|t| t.span.start).unwrap_or(0);
        let end = run.last().map(|t| t.span.end).unwrap_or(0);
        let text = source.get(start..end).unwrap_or_default().to_string();
        result.push(Spanned::new(Token::Ident(text), start..end));
    } else {
        for t in run.iter() {
            result.push((*t).clone());
        }
    }

    run.clear();
}

/// True for token types that can participate in a glob word.
fn is_glob_mergeable(token: &Token) -> bool {
    matches!(
        token,
        Token::Star
            | Token::Question
            | Token::Dot
            | Token::DotDot
            | Token::Ident(_)
            | Token::NumberIdent(_)
            | Token::DashNumWord(_)
            | Token::AtWord(_)
            | Token::DottedIdent(_)
            | Token::Path(_)
            | Token::Int(_)
            | Token::LBracket
            | Token::RBracket
            | Token::Bang
            | Token::DotSlashPath(_)
            | Token::RelativePath(_)
            | Token::TildePath(_)
            | Token::Tilde
            | Token::LBrace
            | Token::RBrace
            | Token::Comma
    )
}

/// Merge a span-adjacent metacharacter onto a flag token.
///
/// Handles the `awk -F:` idiom: the lexer emits `-F` as `ShortFlag("F")`
/// and `:` as `Token::Colon`. When span-adjacent, the `:` is part of the
/// flag value, not a shell operator, so they fuse into `ShortFlag("F:")`
/// for the arg-binding layer (the same mechanism used for `cut -f1`).
/// Consecutive colons are all absorbed (`-F::` → `ShortFlag("F::")`).
///
/// `;` (Semi) and `|` (Pipe) are shell operators and must NOT be fused
/// even when span-adjacent — in bash, `-F;` and `-F|` require quoting
/// (`-F';'`), and kaish matches that contract. Space-separated `cmd -F :`
/// leaves a span gap and never reaches this merge.
fn merge_flag_metachar_adjacent(tokens: Vec<Spanned<Token>>) -> Vec<Spanned<Token>> {
    if tokens.len() < 2 {
        return tokens;
    }

    let mut result = Vec::with_capacity(tokens.len());
    let mut i = 0;

    while i < tokens.len() {
        let token = &tokens[i];

        if let Token::ShortFlag(flag_name) = &token.token {
            let mut fused = flag_name.clone();
            let mut end_span = token.span.end;
            let mut j = i + 1;

            while let Some(next) = tokens.get(j) {
                if next.span.start == end_span {
                    if let Token::Colon = &next.token {
                        fused.push(':');
                        end_span = next.span.end;
                        j += 1;
                        continue;
                    }
                }
                break;
            }

            if j > i + 1 {
                let span = token.span.start..end_span;
                result.push(Spanned::new(Token::ShortFlag(fused), span));
                i = j;
                continue;
            }
        }

        result.push(token.clone());
        i += 1;
    }

    result
}

/// Merge span-adjacent token runs containing glob metacharacters into
/// `GlobWord` tokens.
///
/// A run is merged when it contains at least one `Star`, `Question`, or a
/// `LBracket`+`RBracket` pair. Runs after colon merge: `foo::bar` stays
/// `Ident("foo::bar")` because colon merge already fused it.
///
/// A run that opens at *value position* (`x=[dog]`, `[[ $a in [dog] ]]`)
/// is exempted from bracket-pair fusion — see `compute_value_context` —
/// so the list-literal parser sees primitive `LBracket`/`RBracket`
/// tokens. Argv-position brackets (`ls [dog]`, `for x in [a]`) fuse as
/// before.
///
/// A SEPARATE trigger suppresses fusion for an **assignment lvalue**
/// (`fruits[0]=kiwi`, `services[web][port]=9090`): a bracket-pair run
/// (no `*`/`?`) led by an `Ident` and immediately followed by `Token::Eq`
/// is a subscripted assignment target, not a glob — see
/// `docs/arrays-and-hashes.md` ("Assignment lvalues").
fn merge_glob_adjacent(tokens: Vec<Spanned<Token>>, source: &str) -> Vec<Spanned<Token>> {
    if tokens.is_empty() {
        return tokens;
    }

    let value_ctx = compute_value_context(&tokens);
    let mut result = Vec::with_capacity(tokens.len());
    let mut run: Vec<&Spanned<Token>> = Vec::new();
    let mut run_start = 0usize;

    for (idx, token) in tokens.iter().enumerate() {
        if run.is_empty() {
            if is_glob_mergeable(&token.token) {
                run.push(token);
                run_start = idx;
            } else {
                result.push(token.clone());
            }
            continue;
        }

        // Safety: run is non-empty (checked at top of loop)
        let Some(last) = run.last() else { unreachable!() };
        let adjacent = last.span.end == token.span.start;

        if adjacent && is_glob_mergeable(&token.token) {
            run.push(token);
        } else {
            // `token` is whatever broke the run — an lvalue's `=` is never
            // glob-mergeable, so it always lands here regardless of
            // whitespace (`fruits[0]=kiwi` and `fruits[0] = kiwi` both).
            let followed_by_eq = matches!(token.token, Token::Eq);
            flush_glob_run(
                &mut run,
                &mut result,
                value_ctx[run_start].in_literal,
                followed_by_eq,
                source,
            );
            if is_glob_mergeable(&token.token) {
                run.push(token);
                run_start = idx;
            } else {
                result.push(token.clone());
            }
        }
    }

    // End of input: no token follows the final run, so it can't be an
    // lvalue (an assignment always has a value after `=`).
    flush_glob_run(
        &mut run,
        &mut result,
        value_ctx[run_start].in_literal,
        false,
        source,
    );

    result
}

/// Flush a run of glob-mergeable tokens: merge to a `GlobWord` (text
/// sliced verbatim from the source) if it contains glob metacharacters.
///
/// `value_position_suppress` (run opened at value position) forces
/// individual emission for bracket-bearing runs, so a `[`-leading run at
/// value position always reaches the parser as primitive tokens for the
/// list-literal grammar. A pure `Star`/`Question` glob with no brackets
/// (`X=*.txt`) keeps fusing — it evaluates to a literal string at value
/// position exactly as before collection literals existed.
///
/// `followed_by_eq` is the SEPARATE lvalue trigger: an `Ident`-led
/// bracket-pair run with no `*`/`?` immediately before `=` is a
/// subscripted assignment target (`fruits[0]=kiwi`), not a glob.
fn flush_glob_run(
    run: &mut Vec<&Spanned<Token>>,
    result: &mut Vec<Spanned<Token>>,
    value_position_suppress: bool,
    followed_by_eq: bool,
    source: &str,
) {
    if run.is_empty() {
        return;
    }

    let has_bracket_pair = run.iter().any(|t| matches!(t.token, Token::LBracket))
        && run.iter().any(|t| matches!(t.token, Token::RBracket));
    let has_star_or_question = run
        .iter()
        .any(|t| matches!(t.token, Token::Star | Token::Question));
    let has_glob = has_star_or_question || has_bracket_pair;

    // An lvalue subscript run is a ROOT IDENTIFIER followed by brackets
    // (`arr[0]=` → run is `arr [ 0 ]`). A bare char-class comparison
    // operand starts with `[` instead (`[[ [a] = b ]]`), so requiring an
    // `Ident`-led run keeps that fusing-and-comparing while still
    // catching every real lvalue.
    let run_starts_with_ident = matches!(run.first().map(|t| &t.token), Some(Token::Ident(_)));
    let lvalue_suppress =
        followed_by_eq && has_bracket_pair && !has_star_or_question && run_starts_with_ident;
    let suppress = (value_position_suppress && has_bracket_pair) || lvalue_suppress;

    if !suppress && run.len() >= 2 && has_glob {
        let start = run.first().map(|t| t.span.start).unwrap_or(0);
        let end = run.last().map(|t| t.span.end).unwrap_or(0);
        let text = source.get(start..end).unwrap_or_default().to_string();
        result.push(Spanned::new(Token::GlobWord(text), start..end));
    } else {
        for t in run.iter() {
            result.push((*t).clone());
        }
    }

    run.clear();
}

// ═══════════════════════════════════════════════════════════════════
// Pipeline entry points
// ═══════════════════════════════════════════════════════════════════

/// Tokenize kaish source into spanned tokens.
///
/// Pipeline: one composed scan (heredocs + arithmetic extracted with full
/// quote/escape/comment awareness, complete replacement table) → logos →
/// positional marker resolution → span correction back to original
/// coordinates → fusion passes (flag-metachar, colon, glob) with
/// verbatim-slice text. All spans — including `HereDoc` tokens and
/// everything after them — are exact original-source byte ranges.
pub fn tokenize(source: &str) -> Result<Vec<Spanned<Token>>, Vec<Spanned<LexerError>>> {
    tokenize_impl(source, false)
}

/// Tokenize, preserving `Comment` and `LineContinuation` tokens.
///
/// Runs the SAME pipeline as `tokenize` (pre-#95 this was a divergent
/// second pipeline with no preprocessing or merges). Useful for
/// pretty-printing and formatting tools.
pub fn tokenize_with_comments(source: &str) -> Result<Vec<Spanned<Token>>, Vec<Spanned<LexerError>>> {
    tokenize_impl(source, true)
}

fn tokenize_impl(
    source: &str,
    keep_comments: bool,
) -> Result<Vec<Spanned<Token>>, Vec<Spanned<LexerError>>> {
    let scan_output = scan(source).map_err(|e| vec![e])?;

    // map_position's early `break` depends on the table being ordered by
    // rewritten-buffer position; the scanner appends in scan order, which
    // guarantees it.
    debug_assert!(
        scan_output
            .replacements
            .windows(2)
            .all(|w| w[0].new_start <= w[1].new_start),
        "replacement table must be ordered by new_start"
    );

    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    for (result, span) in Token::lexer(&scan_output.text).spanned() {
        match result {
            Ok(token) => {
                if !keep_comments
                    && matches!(token, Token::Comment | Token::LineContinuation)
                {
                    continue;
                }
                // Rewritten-buffer spans here; mapped to original
                // coordinates after marker resolution.
                tokens.push(Spanned::new(token, span));
            }
            Err(err) => {
                errors.push(Spanned::new(err, map_span(&span, &scan_output.replacements)));
            }
        }
    }
    if !errors.is_empty() {
        return Err(errors);
    }

    let resolved = resolve_markers(tokens, &scan_output).map_err(|errs| {
        errs.into_iter()
            .map(|e| Spanned::new(e.token, map_span(&e.span, &scan_output.replacements)))
            .collect::<Vec<_>>()
    })?;

    let mapped: Vec<Spanned<Token>> = resolved
        .into_iter()
        .map(|s| {
            let span = map_span(&s.span, &scan_output.replacements);
            Spanned::new(s.token, span)
        })
        .collect();

    Ok(merge_glob_adjacent(
        merge_colon_adjacent(merge_flag_metachar_adjacent(mapped), source),
        source,
    ))
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
#[allow(clippy::approx_constant)]
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
    }

    #[test]
    fn var_ref_nested_default_is_one_token() {
        // GH #173: the balanced-brace callback keeps a nested reference in
        // a default word as ONE VarRef token (the old first-`}` regex split
        // it into VarRef + RBrace).
        assert_eq!(
            lex("${X:-${Y}}"),
            vec![Token::VarRef("${X:-${Y}}".to_string())]
        );
        assert_eq!(
            lex("${A:-${B:-${C}}}"),
            vec![Token::VarRef("${A:-${B:-${C}}}".to_string())]
        );
        // VarLength still out-matches the two-character `${` opener.
        assert_eq!(lex("${#X}"), vec![Token::VarLength("X".to_string())]);
    }

    #[test]
    fn var_ref_unterminated_and_empty_are_errors() {
        assert!(tokenize("${X:-${Y}").is_err(), "unbalanced nesting is loud");
        assert!(tokenize("${a{b}").is_err(), "extra open brace is loud");
        assert!(tokenize("${}").is_err(), "empty reference is loud");
    }

    #[test]
    fn var_ref_closes_at_first_balanced_brace() {
        // Trailing `b}` after the balanced close is separate tokens — the
        // early-close contract (kaibo review, GH #173).
        assert_eq!(
            lex("${a}b}"),
            vec![
                Token::VarRef("${a}".to_string()),
                Token::Ident("b".to_string()),
                Token::RBrace,
            ]
        );
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
        assert_eq!(lex("kaish-tools"), vec![Token::Ident("kaish-tools".to_string())]);
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
    fn job_spec_lexes_as_one_token() {
        // `%N` is the bash jobspec for wait/kill — used to be a lexer error.
        assert_eq!(lex("%1"), vec![Token::JobSpec("%1".to_string())]);
        assert_eq!(lex("%12"), vec![Token::JobSpec("%12".to_string())]);
        assert_eq!(
            lex("wait %1 %2"),
            vec![
                Token::Ident("wait".to_string()),
                Token::JobSpec("%1".to_string()),
                Token::JobSpec("%2".to_string()),
            ]
        );
    }

    #[test]
    fn short_flag_with_internal_hyphens_is_one_token() {
        // A dash-word with internal hyphens is ONE shell word, not three
        // flags — `-not-a-flag` must not fragment into `-not` `-a` `-flag`.
        // (Whether it's a flag or a literal is the binding layer's call.)
        assert_eq!(
            lex("-not-a-flag"),
            vec![Token::ShortFlag("not-a-flag".to_string())]
        );
        // The two-char terminator `--` is still DoubleDash, and a lone `-`
        // is still MinusAlone — the second char must be a letter to start a
        // short flag.
        assert_eq!(lex("--"), vec![Token::DoubleDash]);
        assert_eq!(lex("-"), vec![Token::MinusAlone]);
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
    fn var_length_with_subscript() {
        // The widened regex admits `[...]` subscripts so a length-of-path lexes
        // in expression position; the parser turns the inner into a VarPath.
        assert_eq!(lex("${#u[tags]}"), vec![Token::VarLength("u[tags]".to_string())]);
        assert_eq!(lex("${#a[0]}"), vec![Token::VarLength("a[0]".to_string())]);
        assert_eq!(lex("${#a[b][c]}"), vec![Token::VarLength("a[b][c]".to_string())]);
        assert_eq!(lex("${#r[$k]}"), vec![Token::VarLength("r[$k]".to_string())]);
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
        // body_start_offset = byte offset of 'h' in "hello", i.e. just after "cat <<EOF\n"
        assert_eq!(tokens, vec![
            Token::Ident("cat".to_string()),
            Token::HereDocStart,
            Token::HereDoc(HereDocData {
                content: "hello\nworld\n".to_string(),
                literal: false,
                strip_tabs: false,
                body_start_offset: 10,
            }),
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
            Token::HereDoc(HereDocData {
                content: "".to_string(),
                literal: false,
                strip_tabs: false,
                body_start_offset: 10,
            }),
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
            Token::HereDoc(HereDocData {
                content: "$VAR and \"quoted\" 'single'\n".to_string(),
                literal: false,
                strip_tabs: false,
                body_start_offset: 10,
            }),
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
            Token::HereDoc(HereDocData {
                content: "line1\nline2\nline3\n".to_string(),
                literal: false,
                strip_tabs: false,
                body_start_offset: 10,
            }),
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
            Token::HereDoc(HereDocData {
                content: "hello\n".to_string(),
                literal: false,
                strip_tabs: false,
                body_start_offset: 10,
            }),
            Token::Newline,
            Token::Ident("echo".to_string()),
            Token::Ident("goodbye".to_string()),
        ]);
    }

    #[test]
    fn heredoc_strip_tabs() {
        let source = "cat <<-EOF\n\thello\n\tworld\n\tEOF";
        let tokens = lex(source);
        // Content keeps tabs verbatim — strip_tabs is recorded on the token so
        // the interpreter can apply POSIX leading-tab stripping at materialization
        // without disturbing source byte offsets used for span tracking.
        assert_eq!(tokens, vec![
            Token::Ident("cat".to_string()),
            Token::HereDocStart,
            Token::HereDoc(HereDocData {
                content: "\thello\n\tworld\n".to_string(),
                literal: false,
                strip_tabs: true,
                body_start_offset: 11,
            }),
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
    // Arithmetic preprocessor + comment interaction
    //
    // The preprocessor used to walk raw characters tracking only quote
    // state. An apostrophe inside a `#` comment would open single-quote
    // mode and swallow real `$((..))` later in the file; `$((..))` *inside*
    // a comment would itself be preprocessed into a marker, misplacing
    // tokens. Surfaced from kaijutsu's seed scripts (see gotcha memory
    // `gotcha-kaish-comment-arithmetic`).
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn arithmetic_after_apostrophe_in_comment() {
        // The bare apostrophe in "doesn't" used to open single-quote mode
        // in the preprocessor and swallow the $((..)) below.
        let source = "# this doesn't work\necho $((1+2))";
        let tokens = lex(source);
        assert_eq!(tokens, vec![
            Token::Newline,
            Token::Ident("echo".to_string()),
            Token::Arithmetic("1+2".to_string()),
        ]);
    }

    #[test]
    fn arithmetic_inside_comment_is_not_expanded() {
        // `$((y))` inside a `#` comment must stay comment text.
        let source = "# the $((y)) syntax explained\necho hello";
        let tokens = lex(source);
        assert_eq!(tokens, vec![
            Token::Newline,
            Token::Ident("echo".to_string()),
            Token::Ident("hello".to_string()),
        ]);
    }

    #[test]
    fn backticked_arithmetic_in_comment_is_not_expanded() {
        // The original kaijutsu repro: `$((x))` inside a comment.
        // Backticks-in-comments used to leak the inner $((..)) to the
        // preprocessor; with comment-skip they stay inert.
        let source = "# the `$((x))` syntax explained\necho $((3+4))";
        let tokens = lex(source);
        assert_eq!(tokens, vec![
            Token::Newline,
            Token::Ident("echo".to_string()),
            Token::Arithmetic("3+4".to_string()),
        ]);
    }

    #[test]
    fn arithmetic_still_works_outside_comments() {
        // Regression guard: comment-skip must not shrink the arithmetic
        // preprocessor's scope on normal `$((..))` usages.
        let source = "X=$((1+2)); Y=$((3*4))";
        let tokens = lex(source);
        assert_eq!(tokens, vec![
            Token::Ident("X".to_string()),
            Token::Eq,
            Token::Arithmetic("1+2".to_string()),
            Token::Semi,
            Token::Ident("Y".to_string()),
            Token::Eq,
            Token::Arithmetic("3*4".to_string()),
        ]);
    }

    #[test]
    fn arithmetic_inside_double_quotes_still_expands() {
        // `#` inside a double-quoted string is a literal character, not a
        // comment introducer — arithmetic must still expand around it.
        let source = "echo \"# $((1+2))\"";
        let tokens = lex(source);
        // The string token contains the `#` and the arithmetic marker;
        // the exact post-processing happens at interpret time. What we
        // assert here is that lexing succeeds and produces a String token
        // (i.e. the comment skip didn't trigger inside the string).
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0], Token::Ident(_)));
        assert!(matches!(tokens[1], Token::String(_)));
    }

    // ═══════════════════════════════════════════════════════════════════
    // Backtick rejection
    //
    // Backticks are an explicitly dropped feature (see CLAUDE.md,
    // docs/LANGUAGE.md, help/limits.md, help/overview.md). We surface a
    // dedicated error rather than the generic `UnexpectedCharacter` so
    // users get a hint to use `$(cmd)`. Comments, single-quoted strings,
    // double-quoted strings, and heredoc bodies are all matched as single
    // tokens (or extracted before logos runs), so the rejection only
    // fires on bare backticks in source code.
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn backtick_in_source_is_rejected() {
        let result = tokenize("echo `date`");
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors.iter().any(|e| e.token == LexerError::BackticksNotSupported));
    }

    #[test]
    fn backtick_in_comment_is_just_comment_text() {
        // Backticks are only rejected when they reach the top-level
        // lexer. Inside a comment they're part of the comment body.
        let source = "# use `date` here\necho hi";
        let tokens = lex(source);
        assert_eq!(tokens, vec![
            Token::Newline,
            Token::Ident("echo".to_string()),
            Token::Ident("hi".to_string()),
        ]);
    }

    #[test]
    fn backtick_in_single_quoted_string_is_literal() {
        // Single-quoted strings are matched as one token by logos; the
        // backticks inside never reach the rejecting matcher.
        let source = "echo '`date`'";
        let tokens = lex(source);
        assert_eq!(tokens, vec![
            Token::Ident("echo".to_string()),
            Token::SingleString("`date`".to_string()),
        ]);
    }

    #[test]
    fn backtick_in_double_quoted_string_is_literal() {
        // Kaish does not activate command substitution from backticks
        // inside double-quoted strings either — clear divergence from
        // POSIX but matches the "backticks don't exist" stance. The
        // double-quoted string token absorbs them as literal characters.
        let source = "echo \"`date`\"";
        let tokens = lex(source);
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0], Token::Ident(_)));
        match &tokens[1] {
            Token::String(s) => assert!(s.contains('`')),
            other => panic!("expected Token::String, got {:?}", other),
        }
    }

    #[test]
    fn backtick_in_heredoc_body_is_preserved() {
        // Heredoc bodies are extracted by the scanner before logos
        // runs, so backticks inside them survive as content.
        let source = "cat <<EOF\n`date`\nEOF\n";
        let tokens = lex(source);
        let heredoc = tokens.iter().find(|t| matches!(t, Token::HereDoc(_)));
        assert!(heredoc.is_some(), "expected a HereDoc token");
        if let Some(Token::HereDoc(d)) = heredoc {
            assert!(d.content.contains('`'));
        }
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
        assert_eq!(
            Token::HereDoc(HereDocData {
                content: "test".to_string(),
                literal: false,
                strip_tabs: false,
                body_start_offset: 0,
            }).category(),
            TokenCategory::String,
        );

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
        assert_eq!(Token::NumberIdent("019dda1c".to_string()).category(), TokenCategory::Command);
        assert_eq!(Token::DottedIdent(".gitignore".to_string()).category(), TokenCategory::Command);

        // Errors
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
        assert_eq!(data.content, "\nhello\n");
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
        assert_eq!(data.content, "hello $HOME\n");
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

    // ═══════════════════════════════════════════════════════════════════
    // Colon merge tests
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn colon_double_in_word() {
        assert_eq!(lex("foo::bar"), vec![Token::Ident("foo::bar".into())]);
    }

    #[test]
    fn colon_single_in_word() {
        assert_eq!(lex("a:b:c"), vec![Token::Ident("a:b:c".into())]);
    }

    #[test]
    fn colon_with_port() {
        assert_eq!(lex("host:8080"), vec![Token::Ident("host:8080".into())]);
    }

    #[test]
    fn colon_standalone() {
        assert_eq!(lex(":"), vec![Token::Colon]);
    }

    #[test]
    fn colon_spaced_no_merge() {
        assert_eq!(
            lex("foo : bar"),
            vec![
                Token::Ident("foo".into()),
                Token::Colon,
                Token::Ident("bar".into()),
            ]
        );
    }

    #[test]
    fn colon_in_command_arg() {
        assert_eq!(
            lex("echo foo::bar"),
            vec![
                Token::Ident("echo".into()),
                Token::Ident("foo::bar".into()),
            ]
        );
    }

    #[test]
    fn colon_trailing() {
        // Trailing colon merges with preceding ident
        assert_eq!(lex("foo:"), vec![Token::Ident("foo:".into())]);
    }

    #[test]
    fn colon_leading() {
        // Leading colon merges with following ident
        assert_eq!(lex(":foo"), vec![Token::Ident(":foo".into())]);
    }

    #[test]
    fn colon_with_path() {
        // Path token + colon + int
        assert_eq!(
            lex("/usr/bin:8080"),
            vec![Token::Ident("/usr/bin:8080".into())]
        );
    }

    // ═══════════════════════════════════════════════════════════════════
    // Token predicate coverage (is_keyword / starts_statement)
    // ═══════════════════════════════════════════════════════════════════

    #[test]
    fn is_keyword_covers_control_flow() {
        for t in [
            Token::While,
            Token::Return,
            Token::Break,
            Token::Continue,
            Token::Exit,
        ] {
            assert!(t.is_keyword(), "{t:?} should be a keyword");
        }
    }

    #[test]
    fn starts_statement_covers_while() {
        assert!(Token::While.starts_statement());
    }

    #[test]
    fn is_keyword_rejects_operators() {
        for t in [Token::Pipe, Token::Amp, Token::Eq, Token::LBrace] {
            assert!(!t.is_keyword(), "{t:?} should not be a keyword");
        }
    }
}
