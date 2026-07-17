//! awk — Pattern scanning and text processing language.
//!
//! A Bourne-lite awk implementation focused on the 80% use case.
//! Uses ERE (extended regex) syntax like egrep, consistent with sed. awk has no
//! `-E`/`-r` flag, so the GNU BRE backslash-metas (`\|`, `\+`, `\(…\)`,
//! `\{N,M\}`) are always accepted as a forgiving superset (issue #60).

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use regex::Regex;
use std::collections::HashMap;
use std::path::Path;

#[cfg(test)]
use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::builtin::get_path_string;
use crate::tools::builtin::read_repeatable_strings;
use crate::tools::builtin::regex_dialect::{append_dialect_hint, bre_metas_to_ere};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Compile an awk ERE pattern, first rewriting the GNU BRE backslash-metas to
/// ERE so `\|`/`\(…\)`/`\{N\}` behave as operators (issue #60). awk is ERE-only
/// with no `-E` flag, so the rewrite always applies. Callers with their own
/// error wording (FS, `split()`) rewrite with [`bre_metas_to_ere`] directly.
fn compile_ere(pattern: &str) -> Result<Regex, String> {
    let rewritten = bre_metas_to_ere(pattern);
    let rewrote = rewritten != pattern;
    // awk has no `-E` flag, so the hint offers only the bracket-class spelling.
    Regex::new(&rewritten)
        .map_err(|e| append_dialect_hint(format!("invalid regex: {e}"), rewrote, None))
}

/// Awk tool: pattern-directed scanning and processing.
pub struct Awk;

/// clap-derived argv layer for awk.
///
/// awk's program body is read off `args.positional[0]` directly so the rich
/// AWK syntax stays out of clap. clap only handles argv-level flags like
/// `-F` (field separator) and `-v NAME=VALUE`.
#[derive(Parser, Debug)]
#[command(name = "awk", about = "Pattern scanning and text processing language")]
struct AwkArgs {
    /// Field separator regex (-F).
    #[arg(short = 'F', long = "field-separator", visible_alias = "field_separator")]
    field_separator: Option<String>,

    /// Variable assignment (-v NAME=VALUE). Repeatable; all are applied.
    #[arg(short = 'v', long = "var")]
    var: Vec<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// AWK program body followed by optional input file paths.
    program_and_paths: Vec<String>,
}

#[async_trait]
impl Tool for Awk {
    fn name(&self) -> &str {
        "awk"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &AwkArgs::command(),
            "awk",
            "Pattern scanning and text processing language",
            [
                ("Print a field", "awk '{print $2}' file.txt"),
                ("Field separator", "awk -F: '{print $1}' /etc/passwd"),
                ("Filter on a numeric field", "awk '$3 > 100 {print $1, $3}' data.txt"),
                ("Match a regex", "awk '/error/ {print}' log.txt"),
                ("Sum, grouped by a key", "awk '{sum[$1] += $2} END {for (k in sum) print k, sum[k]}' data.txt"),
                ("Substitute text", "awk '{gsub(/foo/, \"bar\"); print}' notes.txt"),
                ("Split into an array", "awk '{n = split($0, a, \",\"); print a[1], n}' file.csv"),
                ("BEGIN/END", "awk 'BEGIN {print \"head\"} {print} END {print NR}'"),
                ("Range of lines", "awk '/start/,/stop/' file.txt"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("awk: {e}")),
        };
        let parsed = match AwkArgs::try_parse_from(
            std::iter::once("awk".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("awk: {e}")),
        };
        parsed.global.apply(ctx);

        // Get program (first positional or named)
        let program = match args.get_string("program", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "awk: missing program"),
        };

        // Parse program
        let ast = match parse_program(&program) {
            Ok(ast) => ast,
            Err(e) => return ExecResult::failure(1, format!("awk: {}", e)),
        };

        // `-F`/`--field-separator` is a named/flag value only, never
        // positional, so `ToolArgs::to_argv()` now rejects a `Value::Bytes`
        // field separator loudly before `AwkArgs::try_parse_from` ever runs
        // (GH #164, closing the root cause behind this GH #120 fix) —
        // `parsed.field_separator` can no longer silently observe the old
        // `[binary: N bytes]` placeholder (which would otherwise become
        // awk's literal `FS`, matching no real input and silently turning
        // every line into a single field). Reading it directly is safe.
        let field_sep = parsed
            .field_separator
            .clone()
            .or_else(|| args.get_string("field_separator", usize::MAX))
            .or_else(|| args.get_string("F", usize::MAX));

        // Get input. A binary `path` operand goes loud rather than silently
        // falling through to the "no operand" branch (which reads stdin instead
        // of the file the user actually named).
        let file_pos = 1;
        let input = match get_path_string(&args, "path", file_pos) {
            Ok(Some(path)) => {
                let resolved = ctx.resolve_path(&path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => s,
                        Err(_) => {
                            return ExecResult::failure(1, format!("awk: {}: invalid UTF-8", path))
                        }
                    },
                    Err(e) => return ExecResult::failure(1, format!("awk: {}: {}", path, e)),
                }
            }
            Ok(None) => match ctx.read_stdin_to_text().await {
                Ok(s) => s.unwrap_or_default(),
                Err(e) => return ExecResult::failure(2, format!("awk: {e}")),
            },
            Err(e) => return ExecResult::failure(1, format!("awk: {e}")),
        };

        // Build runtime with initial variables
        let mut runtime = AwkRuntime::new();
        if let Some(fs) = field_sep {
            runtime.set_var("FS", AwkValue::String(fs));
        }

        // Handle -v assignments. Read from the *raw* args, not `parsed.var`: the
        // kernel accumulates repeated `-v` into a `Value::Json(Array)`, which
        // `ToolArgs::to_argv()` collapses to one JSON token (it can't tell a
        // repeatable scalar array from a single array value) — same gotcha as
        // sed's `-e`. Each assignment is applied in order; later wins.
        let var_assigns = match collect_vars(&args) {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("awk: -v: {e}")),
        };
        for var_assign in var_assigns {
            if let Some((name, value)) = var_assign.split_once('=') {
                // Command-line assignments are numeric strings (POSIX strnum).
                runtime.set_var(name.trim(), AwkValue::StrNum(value.to_string()));
            }
        }

        // Execute
        match runtime.execute(&ast, &input) {
            Ok((output, exit_code)) => {
                let result = ExecResult::with_output(OutputData::text(output));
                // `exit N` sets the builtin's exit code; output + END still ran.
                match exit_code {
                    Some(code) => result.with_code(code as i64),
                    None => result,
                }
            }
            Err(e) => ExecResult::failure(1, format!("awk: {}", e)),
        }
    }
}

/// Collect every `-v NAME=VALUE` assignment, in order.
///
/// Repeated `-v` flags are accumulated by the kernel into a `Value::Json(Array)`
/// under the canonical `var` key; a single value may arrive as a bare
/// `Value::String`. Mirrors sed's `collect_expressions` — see the repeatable
/// gotcha in `[[arch_repeatable_flags]]`.
///
/// Delegates to [`read_repeatable_strings`] (GH #217) rather than a hand-rolled
/// `filter_map`: a `-v` value that reaches here is either the `key=value`
/// string produced by `consume_flag_positionals`'s WordAssign reassembly (see
/// `awk_dash_v_binary_assignment_is_loud`), or a *bare* substitution with no
/// literal `=` in the source (`-v $x`) — which skips that guard and can still
/// carry a binary envelope. The old `if let Value::String(s) = item` filter
/// silently dropped that entry instead of erroring, so the assignment just
/// vanished and awk ran as if `-v` had never been given.
fn collect_vars(args: &ToolArgs) -> Result<Vec<String>, String> {
    // Both `-v` and `--var` canonicalize to the long name `var`; `v` is
    // defensive insurance in case a future change binds under the short alias.
    // Only one key is ever actually populated, so trying `var` first and
    // falling back to `v` only when it's empty can't double-count or reorder.
    let vars = read_repeatable_strings(args, "var")?;
    if !vars.is_empty() {
        return Ok(vars);
    }
    read_repeatable_strings(args, "v")
}

// ============================================================================
// AST Types
// ============================================================================

/// Complete AWK program: list of rules.
#[derive(Debug, Clone)]
struct AwkProgram {
    rules: Vec<Rule>,
}

/// A single rule: pattern + action.
#[derive(Debug, Clone)]
struct Rule {
    pattern: Pattern,
    action: Block,
}

/// Pattern that determines if a rule fires.
#[derive(Debug, Clone)]
enum Pattern {
    /// Matches all records (empty pattern).
    All,
    /// BEGIN block - runs before processing.
    Begin,
    /// END block - runs after processing.
    End,
    /// Regex pattern: /regex/.
    Regex(String),
    /// Expression pattern: $1 > 100.
    Expr(Expr),
    /// Range pattern: /start/,/end/ — fires from the record matching `start`
    /// through the record matching `end`, inclusive (POSIX semantics).
    /// Neither endpoint may be BEGIN or END.
    Range(Box<Pattern>, Box<Pattern>),
}

/// A block of statements.
type Block = Vec<Stmt>;

/// Statement types.
#[derive(Debug, Clone)]
enum Stmt {
    /// Print statement: print expr, expr, ...
    Print(Vec<Expr>),
    /// Printf statement: printf fmt, expr, ...
    Printf(Vec<Expr>),
    /// Assignment: var = expr or $n = expr.
    Assign(LValue, AssignOp, Expr),
    /// If statement: if (cond) block [else block].
    If(Expr, Block, Option<Block>),
    /// While loop: while (cond) block.
    While(Expr, Block),
    /// For loop: for (init; cond; incr) block.
    For(Option<Box<Stmt>>, Option<Expr>, Option<Box<Stmt>>, Block),
    /// For-in loop: for (var in arr) block.
    ForIn(String, String, Block),
    /// Break statement.
    Break,
    /// Continue statement.
    Continue,
    /// Next statement - skip to next record.
    Next,
    /// Exit statement - stop processing.
    Exit(Option<Expr>),
    /// Delete array element: delete arr[key].
    Delete(String, Expr),
    /// Expression statement (for side effects).
    Expr(Expr),
}

/// L-value for assignments.
#[derive(Debug, Clone)]
enum LValue {
    /// Variable: name.
    Var(String),
    /// Field: $n.
    Field(Box<Expr>),
    /// Array element: arr[key].
    ArrayElem(String, Box<Expr>),
}

/// Assignment operators.
#[derive(Debug, Clone, Copy)]
enum AssignOp {
    Assign,    // =
    AddAssign, // +=
    SubAssign, // -=
    MulAssign, // *=
    DivAssign, // /=
    ModAssign, // %=
}

/// Expression types.
#[derive(Debug, Clone)]
enum Expr {
    /// Number literal.
    Number(f64),
    /// String literal.
    String(String),
    /// Regex literal (for matching).
    Regex(String),
    /// Variable reference.
    Var(String),
    /// Field reference: $n.
    Field(Box<Expr>),
    /// Array access: arr[key].
    ArrayAccess(String, Box<Expr>),
    /// Binary operation.
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    /// Unary operation.
    UnaryOp(UnaryOp, Box<Expr>),
    /// Ternary: cond ? then : else.
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    /// Function call.
    Call(String, Vec<Expr>),
    /// Pre/post increment/decrement.
    Increment(LValue, bool, bool), // lvalue, is_pre, is_increment
    /// Concatenation (implicit).
    Concat(Vec<Expr>),
    /// In expression: key in array.
    In(Box<Expr>, String),
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq)]
enum BinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    // Regex
    Match,    // ~
    NotMatch, // !~
    // Logical
    And,
    Or,
}

/// Unary operators.
#[derive(Debug, Clone, Copy)]
enum UnaryOp {
    Neg,
    Not,
    Pos,
}

// ============================================================================
// Lexer
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
enum Token {
    // Literals
    Number(f64),
    String(String),
    Regex(String),
    Ident(String),

    // Keywords
    Begin,
    End,
    If,
    Else,
    While,
    For,
    In,
    Do,
    Break,
    Continue,
    Next,
    Exit,
    Delete,
    Print,
    Printf,
    Function,
    Return,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,       // ^
    Dollar,      // $
    Eq,          // ==
    Ne,          // !=
    Lt,          // <
    Le,          // <=
    Gt,          // >
    Ge,          // >=
    Match,       // ~
    NotMatch,    // !~
    And,         // &&
    Or,          // ||
    Not,         // !
    Assign,      // =
    AddAssign,   // +=
    SubAssign,   // -=
    MulAssign,   // *=
    DivAssign,   // /=
    ModAssign,   // %=
    Increment,   // ++
    Decrement,   // --
    Question,    // ?
    Colon,       // :
    Comma,       // ,
    Semicolon,   // ;
    LParen,      // (
    RParen,      // )
    LBrace,      // {
    RBrace,      // }
    LBracket,    // [
    RBracket,    // ]
    Newline,     // significant newline
    Eof,
}

struct AwkLexer {
    chars: Vec<char>,
    pos: usize,
    in_regex_context: bool,
}

impl AwkLexer {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
            in_regex_context: true, // Start of program can have regex
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn peek_ahead(&self, n: usize) -> Option<char> {
        self.chars.get(self.pos + n).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.peek();
        if c.is_some() {
            self.pos += 1;
        }
        c
    }

    fn skip_whitespace_and_comments(&mut self) {
        while let Some(c) = self.peek() {
            if c == ' ' || c == '\t' || c == '\r' {
                self.advance();
            } else if c == '#' {
                // Comment until end of line
                while let Some(c) = self.peek() {
                    if c == '\n' {
                        break;
                    }
                    self.advance();
                }
            } else if c == '\\' && self.peek_ahead(1) == Some('\n') {
                // Line continuation
                self.advance();
                self.advance();
            } else {
                break;
            }
        }
    }

    fn next_token(&mut self) -> Result<Token, String> {
        self.skip_whitespace_and_comments();

        let c = match self.peek() {
            Some(c) => c,
            None => return Ok(Token::Eof),
        };

        // Newline is significant in AWK. It also begins a new rule/statement,
        // a position where a leading `/` is a regex, not division.
        if c == '\n' {
            self.advance();
            self.in_regex_context = true;
            return Ok(Token::Newline);
        }

        // Number
        if c.is_ascii_digit() || (c == '.' && self.peek_ahead(1).is_some_and(|c| c.is_ascii_digit()))
        {
            return self.scan_number();
        }

        // String
        if c == '"' {
            return self.scan_string();
        }

        // Regex (context-sensitive)
        if c == '/' && self.in_regex_context {
            return self.scan_regex();
        }

        // Identifier or keyword
        if c.is_ascii_alphabetic() || c == '_' {
            return self.scan_ident();
        }

        // Operators and punctuation
        self.scan_operator()
    }

    fn scan_number(&mut self) -> Result<Token, String> {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() || c == '.' || c == 'e' || c == 'E' {
                s.push(c);
                self.advance();
                // Handle exponent sign
                if (c == 'e' || c == 'E') && self.peek().is_some_and(|c| c == '+' || c == '-')
                    && let Some(sign) = self.advance() {
                        s.push(sign);
                    }
            } else {
                break;
            }
        }
        let n: f64 = s.parse().map_err(|_| format!("invalid number: {}", s))?;
        self.in_regex_context = false;
        Ok(Token::Number(n))
    }

    fn scan_string(&mut self) -> Result<Token, String> {
        self.advance(); // opening "
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if c == '"' {
                self.advance();
                self.in_regex_context = false;
                return Ok(Token::String(s));
            } else if c == '\\' {
                self.advance();
                match self.advance() {
                    Some('n') => s.push('\n'),
                    Some('t') => s.push('\t'),
                    Some('r') => s.push('\r'),
                    Some('\\') => s.push('\\'),
                    Some('"') => s.push('"'),
                    Some('/') => s.push('/'),
                    Some(c) => {
                        s.push('\\');
                        s.push(c);
                    }
                    None => return Err("unterminated string".to_string()),
                }
            } else if c == '\n' {
                return Err("unterminated string".to_string());
            } else {
                s.push(c);
                self.advance();
            }
        }
        Err("unterminated string".to_string())
    }

    fn scan_regex(&mut self) -> Result<Token, String> {
        self.advance(); // opening /
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if c == '/' {
                self.advance();
                self.in_regex_context = false;
                return Ok(Token::Regex(s));
            } else if c == '\\' {
                self.advance();
                if let Some(next) = self.advance() {
                    if next == '/' {
                        s.push('/');
                    } else {
                        s.push('\\');
                        s.push(next);
                    }
                } else {
                    return Err("unterminated regex".to_string());
                }
            } else if c == '\n' {
                return Err("unterminated regex".to_string());
            } else {
                s.push(c);
                self.advance();
            }
        }
        Err("unterminated regex".to_string())
    }

    fn scan_ident(&mut self) -> Result<Token, String> {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                s.push(c);
                self.advance();
            } else {
                break;
            }
        }
        let tok = match s.as_str() {
            "BEGIN" => Token::Begin,
            "END" => Token::End,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "for" => Token::For,
            "in" => Token::In,
            "do" => Token::Do,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "next" => Token::Next,
            "exit" => Token::Exit,
            "delete" => Token::Delete,
            "print" => Token::Print,
            "printf" => Token::Printf,
            "function" => Token::Function,
            "return" => Token::Return,
            _ => Token::Ident(s),
        };
        // An identifier/keyword is a value (or precedes one), so a following `/`
        // is division, not a regex. (scan_ident never yields `~`/`!~`.)
        self.in_regex_context = false;
        Ok(tok)
    }

    fn scan_operator(&mut self) -> Result<Token, String> {
        let Some(c) = self.advance() else {
            return Err("unexpected end of input in operator".to_string());
        };
        let tok = match c {
            '+' => {
                if self.peek() == Some('+') {
                    self.advance();
                    Token::Increment
                } else if self.peek() == Some('=') {
                    self.advance();
                    Token::AddAssign
                } else {
                    Token::Plus
                }
            }
            '-' => {
                if self.peek() == Some('-') {
                    self.advance();
                    Token::Decrement
                } else if self.peek() == Some('=') {
                    self.advance();
                    Token::SubAssign
                } else {
                    Token::Minus
                }
            }
            '*' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Token::MulAssign
                } else {
                    Token::Star
                }
            }
            '/' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Token::DivAssign
                } else {
                    Token::Slash
                }
            }
            '%' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Token::ModAssign
                } else {
                    Token::Percent
                }
            }
            '^' => Token::Caret,
            '$' => Token::Dollar,
            '=' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            '!' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Token::Ne
                } else if self.peek() == Some('~') {
                    self.advance();
                    self.in_regex_context = true;
                    Token::NotMatch
                } else {
                    Token::Not
                }
            }
            '<' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Token::Le
                } else {
                    Token::Lt
                }
            }
            '>' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Token::Ge
                } else {
                    Token::Gt
                }
            }
            '~' => {
                self.in_regex_context = true;
                Token::Match
            }
            '&' => {
                if self.peek() == Some('&') {
                    self.advance();
                    Token::And
                } else {
                    return Err("expected && but found single &".to_string());
                }
            }
            '|' => {
                if self.peek() == Some('|') {
                    self.advance();
                    Token::Or
                } else {
                    return Err("expected || but found single |".to_string());
                }
            }
            '?' => Token::Question,
            ':' => Token::Colon,
            ',' => Token::Comma,
            ';' => {
                // A `;` ends a statement/rule; a `/` after it is a regex.
                self.in_regex_context = true;
                Token::Semicolon
            }
            '(' => {
                self.in_regex_context = true;
                Token::LParen
            }
            ')' => {
                self.in_regex_context = false;
                Token::RParen
            }
            '{' => {
                self.in_regex_context = true;
                Token::LBrace
            }
            '}' => {
                // A `}` ends an action block; the next rule's leading `/` is a
                // regex pattern, not division.
                self.in_regex_context = true;
                Token::RBrace
            }
            '[' => {
                self.in_regex_context = true;
                Token::LBracket
            }
            ']' => {
                self.in_regex_context = false;
                Token::RBracket
            }
            _ => return Err(format!("unexpected character: {}", c)),
        };

        // Most operators are followed by expressions where regex is valid
        if matches!(
            tok,
            Token::Plus
                | Token::Minus
                | Token::Star
                | Token::Slash
                | Token::Percent
                | Token::Eq
                | Token::Ne
                | Token::Lt
                | Token::Le
                | Token::Gt
                | Token::Ge
                | Token::And
                | Token::Or
                | Token::Not
                | Token::Assign
                | Token::AddAssign
                | Token::SubAssign
                | Token::MulAssign
                | Token::DivAssign
                | Token::ModAssign
                | Token::Question
                | Token::Colon
                | Token::Comma
        ) {
            self.in_regex_context = true;
        }

        Ok(tok)
    }

    fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token()?;
            let is_eof = tok == Token::Eof;
            tokens.push(tok);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }
}

// ============================================================================
// Parser
// ============================================================================

struct AwkParser {
    tokens: Vec<Token>,
    pos: usize,
    /// When true, `parse_comparison` must not consume a top-level `>` or `>=`,
    /// because inside `print`/`printf` those introduce output redirection (which
    /// kaish rejects with a clear error rather than silently mis-parsing).
    in_print: bool,
}

impl AwkParser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0, in_print: false }
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token::Eof)
    }

    fn peek_ahead(&self, n: usize) -> &Token {
        self.tokens.get(self.pos + n).unwrap_or(&Token::Eof)
    }

    fn advance(&mut self) -> Token {
        let tok = self.peek().clone();
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
        tok
    }

    fn expect(&mut self, expected: &Token) -> Result<(), String> {
        if self.peek() == expected {
            self.advance();
            Ok(())
        } else {
            Err(format!("expected {:?}, found {:?}", expected, self.peek()))
        }
    }

    fn skip_newlines(&mut self) {
        while self.peek() == &Token::Newline {
            self.advance();
        }
    }

    fn skip_terminators(&mut self) {
        while matches!(self.peek(), Token::Newline | Token::Semicolon) {
            self.advance();
        }
    }

    // program := rule*
    fn parse_program(&mut self) -> Result<AwkProgram, String> {
        let mut rules = Vec::new();
        self.skip_newlines();

        while self.peek() != &Token::Eof {
            // Catch top-level `function` declarations (they appear before a rule, not
            // inside a block, so parse_statement wouldn't see them).
            if self.peek() == &Token::Function {
                return Err(
                    "user-defined functions are not supported (kaish awk is a one-liner subset)"
                        .to_string(),
                );
            }
            rules.push(self.parse_rule()?);
            self.skip_terminators();
        }

        Ok(AwkProgram { rules })
    }

    // rule := pattern? action? (at least one required)
    fn parse_rule(&mut self) -> Result<Rule, String> {
        self.skip_newlines();

        let pattern = self.parse_pattern()?;
        self.skip_newlines();

        let action = if self.peek() == &Token::LBrace {
            self.parse_action()?
        } else if matches!(pattern, Pattern::Begin | Pattern::End) {
            // BEGIN/END require action
            return Err("BEGIN/END require an action block".to_string());
        } else {
            // Default action is {print $0}
            vec![Stmt::Print(vec![Expr::Field(Box::new(Expr::Number(0.0)))])]
        };

        Ok(Rule { pattern, action })
    }

    // pattern := BEGIN | END | /regex/ | expr | pattern,pattern | (empty)
    fn parse_pattern(&mut self) -> Result<Pattern, String> {
        let first = match self.peek() {
            Token::Begin => {
                self.advance();
                Pattern::Begin
            }
            Token::End => {
                self.advance();
                Pattern::End
            }
            Token::Regex(re) => {
                let re = re.clone();
                self.advance();
                Pattern::Regex(re)
            }
            Token::LBrace => return Ok(Pattern::All),
            Token::Eof | Token::Newline => return Ok(Pattern::All),
            _ => {
                let expr = self.parse_expr()?;
                Pattern::Expr(expr)
            }
        };

        // Check for range pattern: `pattern , pattern`.
        // BEGIN/END may not serve as range endpoints.
        if self.peek() == &Token::Comma {
            match &first {
                Pattern::Begin | Pattern::End => {
                    return Err(
                        "BEGIN and END may not be used as range pattern endpoints".to_string(),
                    );
                }
                _ => {}
            }
            self.advance(); // consume ','
            let second = match self.peek() {
                Token::Begin | Token::End => {
                    return Err(
                        "BEGIN and END may not be used as range pattern endpoints".to_string(),
                    );
                }
                Token::Regex(re) => {
                    let re = re.clone();
                    self.advance();
                    Pattern::Regex(re)
                }
                Token::LBrace | Token::Eof | Token::Newline => {
                    return Err("expected pattern after ',' in range pattern".to_string());
                }
                _ => {
                    let expr = self.parse_expr()?;
                    Pattern::Expr(expr)
                }
            };
            return Ok(Pattern::Range(Box::new(first), Box::new(second)));
        }

        Ok(first)
    }

    // action := '{' statement* '}'
    fn parse_action(&mut self) -> Result<Block, String> {
        self.expect(&Token::LBrace)?;
        self.skip_terminators();

        let mut stmts = Vec::new();
        while self.peek() != &Token::RBrace && self.peek() != &Token::Eof {
            stmts.push(self.parse_statement()?);
            self.skip_terminators();
        }

        self.expect(&Token::RBrace)?;
        Ok(stmts)
    }

    // statement := if | while | for | print | printf | break | continue | next | exit | delete | assignment | expr
    fn parse_statement(&mut self) -> Result<Stmt, String> {
        self.skip_newlines();

        match self.peek() {
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::For => self.parse_for(),
            Token::Print => self.parse_print(),
            Token::Printf => self.parse_printf(),
            Token::Break => {
                self.advance();
                Ok(Stmt::Break)
            }
            Token::Continue => {
                self.advance();
                Ok(Stmt::Continue)
            }
            Token::Next => {
                self.advance();
                Ok(Stmt::Next)
            }
            Token::Exit => {
                self.advance();
                let code = if !matches!(
                    self.peek(),
                    Token::Newline | Token::Semicolon | Token::RBrace | Token::Eof
                ) {
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                Ok(Stmt::Exit(code))
            }
            Token::Delete => self.parse_delete(),
            Token::Function => {
                Err("user-defined functions are not supported (kaish awk is a one-liner subset)"
                    .to_string())
            }
            Token::Return => {
                Err("user-defined functions are not supported (kaish awk is a one-liner subset)"
                    .to_string())
            }
            Token::LBrace => {
                let block = self.parse_action()?;
                Ok(Stmt::If(Expr::Number(1.0), block, None)) // Block as always-true if
            }
            _ => self.parse_assignment_or_expr(),
        }
    }

    fn parse_if(&mut self) -> Result<Stmt, String> {
        self.advance(); // if
        self.expect(&Token::LParen)?;
        let cond = self.parse_expr()?;
        self.expect(&Token::RParen)?;
        self.skip_newlines();

        let then_block = if self.peek() == &Token::LBrace {
            self.parse_action()?
        } else {
            vec![self.parse_statement()?]
        };

        // In AWK, semicolons can appear before 'else': if (c) print "a"; else print "b"
        // Skip optional semicolon and newlines before checking for else
        if self.peek() == &Token::Semicolon {
            self.advance();
        }
        self.skip_newlines();

        let else_block = if self.peek() == &Token::Else {
            self.advance();
            self.skip_newlines();
            if self.peek() == &Token::LBrace {
                Some(self.parse_action()?)
            } else {
                Some(vec![self.parse_statement()?])
            }
        } else {
            None
        };

        Ok(Stmt::If(cond, then_block, else_block))
    }

    fn parse_while(&mut self) -> Result<Stmt, String> {
        self.advance(); // while
        self.expect(&Token::LParen)?;
        let cond = self.parse_expr()?;
        self.expect(&Token::RParen)?;
        self.skip_newlines();

        let body = if self.peek() == &Token::LBrace {
            self.parse_action()?
        } else {
            vec![self.parse_statement()?]
        };

        Ok(Stmt::While(cond, body))
    }

    fn parse_for(&mut self) -> Result<Stmt, String> {
        self.advance(); // for
        self.expect(&Token::LParen)?;

        // Check for for-in: for (var in array)
        if let Token::Ident(name) = self.peek().clone()
            && self.peek_ahead(1) == &Token::In
        {
            self.advance(); // var
            self.advance(); // in
            if let Token::Ident(arr) = self.peek().clone() {
                self.advance();
                self.expect(&Token::RParen)?;
                self.skip_newlines();
                let body = if self.peek() == &Token::LBrace {
                    self.parse_action()?
                } else {
                    vec![self.parse_statement()?]
                };
                return Ok(Stmt::ForIn(name, arr, body));
            } else {
                return Err("expected array name after 'in'".to_string());
            }
        }

        // C-style for: for (init; cond; incr)
        let init = if self.peek() != &Token::Semicolon {
            Some(Box::new(self.parse_assignment_or_expr()?))
        } else {
            None
        };
        self.expect(&Token::Semicolon)?;

        let cond = if self.peek() != &Token::Semicolon {
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.expect(&Token::Semicolon)?;

        let incr = if self.peek() != &Token::RParen {
            Some(Box::new(self.parse_assignment_or_expr()?))
        } else {
            None
        };
        self.expect(&Token::RParen)?;
        self.skip_newlines();

        let body = if self.peek() == &Token::LBrace {
            self.parse_action()?
        } else {
            vec![self.parse_statement()?]
        };

        Ok(Stmt::For(init, cond, incr, body))
    }

    fn parse_print(&mut self) -> Result<Stmt, String> {
        self.advance(); // print
        let mut args = Vec::new();

        if !matches!(
            self.peek(),
            Token::Newline | Token::Semicolon | Token::RBrace | Token::Eof
        ) {
            self.in_print = true;
            let first = self.parse_expr();
            self.in_print = false;
            args.push(first?);
            // After the first expr, a leftover bare `>` (Gt) that parse_comparison
            // left unconsumed (because in_print was set) is output redirection.
            // `>=` (Ge) is never left unconsumed — it is always a comparison.
            if self.peek() == &Token::Gt {
                return Err(
                    "output redirection (> >> |) is not supported; \
                     pipe kaish's output downstream instead"
                        .to_string(),
                );
            }
            while self.peek() == &Token::Comma {
                self.advance();
                self.in_print = true;
                let arg = self.parse_expr();
                self.in_print = false;
                args.push(arg?);
                if self.peek() == &Token::Gt {
                    return Err(
                        "output redirection (> >> |) is not supported; \
                         pipe kaish's output downstream instead"
                            .to_string(),
                    );
                }
            }
        }

        Ok(Stmt::Print(args))
    }

    fn parse_printf(&mut self) -> Result<Stmt, String> {
        self.advance(); // printf
        let mut args = Vec::new();

        if !matches!(
            self.peek(),
            Token::Newline | Token::Semicolon | Token::RBrace | Token::Eof
        ) {
            self.in_print = true;
            let first = self.parse_expr();
            self.in_print = false;
            args.push(first?);
            // Same rule as parse_print: only bare `>` (Gt) is redirection; `>=` is comparison.
            if self.peek() == &Token::Gt {
                return Err(
                    "output redirection (> >> |) is not supported; \
                     pipe kaish's output downstream instead"
                        .to_string(),
                );
            }
            while self.peek() == &Token::Comma {
                self.advance();
                self.in_print = true;
                let arg = self.parse_expr();
                self.in_print = false;
                args.push(arg?);
                if self.peek() == &Token::Gt {
                    return Err(
                        "output redirection (> >> |) is not supported; \
                         pipe kaish's output downstream instead"
                            .to_string(),
                    );
                }
            }
        }

        Ok(Stmt::Printf(args))
    }

    fn parse_delete(&mut self) -> Result<Stmt, String> {
        self.advance(); // delete
        if let Token::Ident(name) = self.peek().clone() {
            self.advance();
            self.expect(&Token::LBracket)?;
            let key = self.parse_expr()?;
            self.expect(&Token::RBracket)?;
            Ok(Stmt::Delete(name, key))
        } else {
            Err("expected array name after delete".to_string())
        }
    }

    fn parse_assignment_or_expr(&mut self) -> Result<Stmt, String> {
        let expr = self.parse_expr()?;

        // Check if this is an assignment
        if let Some(op) = self.match_assign_op() {
            let lvalue = self.expr_to_lvalue(&expr)?;
            let rhs = self.parse_expr()?;
            Ok(Stmt::Assign(lvalue, op, rhs))
        } else {
            Ok(Stmt::Expr(expr))
        }
    }

    fn match_assign_op(&mut self) -> Option<AssignOp> {
        let op = match self.peek() {
            Token::Assign => AssignOp::Assign,
            Token::AddAssign => AssignOp::AddAssign,
            Token::SubAssign => AssignOp::SubAssign,
            Token::MulAssign => AssignOp::MulAssign,
            Token::DivAssign => AssignOp::DivAssign,
            Token::ModAssign => AssignOp::ModAssign,
            _ => return None,
        };
        self.advance();
        Some(op)
    }

    fn expr_to_lvalue(&self, expr: &Expr) -> Result<LValue, String> {
        match expr {
            Expr::Var(name) => Ok(LValue::Var(name.clone())),
            Expr::Field(e) => Ok(LValue::Field(e.clone())),
            Expr::ArrayAccess(name, key) => Ok(LValue::ArrayElem(name.clone(), key.clone())),
            _ => Err("invalid assignment target".to_string()),
        }
    }

    // Expression parsing with precedence climbing
    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_ternary()
    }

    fn parse_ternary(&mut self) -> Result<Expr, String> {
        let cond = self.parse_or()?;
        if self.peek() == &Token::Question {
            self.advance();
            // Ternary branches are sub-expressions: a top-level `>` inside them
            // is a comparison, not print redirection (only an unparenthesized
            // `>` directly in print/printf is redirection).
            let saved_in_print = self.in_print;
            self.in_print = false;
            let then_expr = self.parse_expr()?;
            self.expect(&Token::Colon)?;
            let else_expr = self.parse_expr()?;
            self.in_print = saved_in_print;
            Ok(Expr::Ternary(
                Box::new(cond),
                Box::new(then_expr),
                Box::new(else_expr),
            ))
        } else {
            Ok(cond)
        }
    }

    fn parse_or(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_and()?;
        while self.peek() == &Token::Or {
            self.advance();
            let right = self.parse_and()?;
            left = Expr::BinOp(Box::new(left), BinOp::Or, Box::new(right));
        }
        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_in_expr()?;
        while self.peek() == &Token::And {
            self.advance();
            let right = self.parse_in_expr()?;
            left = Expr::BinOp(Box::new(left), BinOp::And, Box::new(right));
        }
        Ok(left)
    }

    fn parse_in_expr(&mut self) -> Result<Expr, String> {
        let left = self.parse_match()?;
        if self.peek() == &Token::In {
            self.advance();
            if let Token::Ident(arr) = self.peek().clone() {
                self.advance();
                return Ok(Expr::In(Box::new(left), arr));
            } else {
                return Err("expected array name after 'in'".to_string());
            }
        }
        Ok(left)
    }

    fn parse_match(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_comparison()?;
        loop {
            let op = match self.peek() {
                Token::Match => BinOp::Match,
                Token::NotMatch => BinOp::NotMatch,
                _ => break,
            };
            self.advance();
            let right = self.parse_comparison()?;
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let left = self.parse_concat()?;
        let op = match self.peek() {
            Token::Eq => BinOp::Eq,
            Token::Ne => BinOp::Ne,
            Token::Lt => BinOp::Lt,
            Token::Le => BinOp::Le,
            // When inside a print/printf arg list, a bare top-level `>` introduces
            // output redirection, not a comparison. Leave the token unconsumed so
            // parse_print / parse_printf can detect and reject it with a clear error.
            // `>=` (Ge) is NOT redirection — it is a normal comparison operator and
            // must be consumed here regardless of in_print.  Inside parentheses
            // in_print is reset to false, so `print ($1>2)` still compares normally.
            Token::Gt if self.in_print => return Ok(left),
            Token::Gt => BinOp::Gt,
            Token::Ge => BinOp::Ge,
            _ => return Ok(left),
        };
        self.advance();
        let right = self.parse_concat()?;
        Ok(Expr::BinOp(Box::new(left), op, Box::new(right)))
    }

    fn parse_concat(&mut self) -> Result<Expr, String> {
        let mut parts = vec![self.parse_additive()?];

        // In AWK, concatenation is implicit - juxtaposed expressions are concatenated
        while self.is_concat_start() {
            parts.push(self.parse_additive()?);
        }

        if parts.len() == 1 {
            Ok(parts.remove(0))
        } else {
            Ok(Expr::Concat(parts))
        }
    }

    fn is_concat_start(&self) -> bool {
        matches!(
            self.peek(),
            Token::Number(_)
                | Token::String(_)
                | Token::Ident(_)
                | Token::Dollar
                | Token::LParen
                | Token::Not
                | Token::Increment
                | Token::Decrement
        )
    }

    fn parse_additive(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_multiplicative()?;
        loop {
            let op = match self.peek() {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_power()?;
        loop {
            let op = match self.peek() {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Percent => BinOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_power()?;
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    fn parse_power(&mut self) -> Result<Expr, String> {
        let base = self.parse_unary()?;
        if self.peek() == &Token::Caret {
            self.advance();
            let exp = self.parse_power()?; // Right associative
            Ok(Expr::BinOp(Box::new(base), BinOp::Pow, Box::new(exp)))
        } else {
            Ok(base)
        }
    }

    fn parse_unary(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Token::Not => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::UnaryOp(UnaryOp::Not, Box::new(expr)))
            }
            Token::Minus => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::UnaryOp(UnaryOp::Neg, Box::new(expr)))
            }
            Token::Plus => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::UnaryOp(UnaryOp::Pos, Box::new(expr)))
            }
            Token::Increment => {
                self.advance();
                let expr = self.parse_postfix()?;
                let lvalue = self.expr_to_lvalue(&expr)?;
                Ok(Expr::Increment(lvalue, true, true))
            }
            Token::Decrement => {
                self.advance();
                let expr = self.parse_postfix()?;
                let lvalue = self.expr_to_lvalue(&expr)?;
                Ok(Expr::Increment(lvalue, true, false))
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.peek() {
                Token::LBracket => {
                    // Array access - only valid for identifiers
                    if let Expr::Var(name) = expr {
                        self.advance();
                        let key = self.parse_expr()?;
                        // Multi-dimensional subscripts a[i,j] are not supported.
                        // Detect the comma after the first subscript expression and
                        // emit a clear, hinted error rather than "expected RBracket".
                        if self.peek() == &Token::Comma {
                            return Err(
                                "multi-dimensional array subscripts (a[i,j]) are not supported; \
                                 build a string key like a[i \",\" j]"
                                    .to_string(),
                            );
                        }
                        self.expect(&Token::RBracket)?;
                        expr = Expr::ArrayAccess(name, Box::new(key));
                    } else {
                        break;
                    }
                }
                Token::Increment => {
                    self.advance();
                    let lvalue = self.expr_to_lvalue(&expr)?;
                    expr = Expr::Increment(lvalue, false, true);
                }
                Token::Decrement => {
                    self.advance();
                    let lvalue = self.expr_to_lvalue(&expr)?;
                    expr = Expr::Increment(lvalue, false, false);
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.peek().clone() {
            Token::Number(n) => {
                self.advance();
                Ok(Expr::Number(n))
            }
            Token::String(s) => {
                self.advance();
                Ok(Expr::String(s))
            }
            Token::Regex(re) => {
                self.advance();
                Ok(Expr::Regex(re))
            }
            Token::Dollar => {
                self.advance();
                let field = self.parse_unary()?;
                Ok(Expr::Field(Box::new(field)))
            }
            Token::Ident(name) => {
                // `getline` is tokenized as Ident (not a keyword). Catch it here
                // before treating it as a variable or function call.
                if name == "getline" {
                    return Err(
                        "getline is not supported; pipe input into awk instead \
                         (e.g. cat f | awk '...')"
                            .to_string(),
                    );
                }
                self.advance();
                // Check for function call
                if self.peek() == &Token::LParen {
                    self.advance();
                    // Call arguments are sub-expressions: a top-level `>` inside
                    // them is a comparison, not print redirection (mirrors the
                    // LParen reset below so `print length(a>b)` works).
                    let saved_in_print = self.in_print;
                    self.in_print = false;
                    let mut args = Vec::new();
                    if self.peek() != &Token::RParen {
                        args.push(self.parse_expr()?);
                        while self.peek() == &Token::Comma {
                            self.advance();
                            args.push(self.parse_expr()?);
                        }
                    }
                    self.in_print = saved_in_print;
                    self.expect(&Token::RParen)?;
                    Ok(Expr::Call(name, args))
                } else {
                    Ok(Expr::Var(name))
                }
            }
            Token::LParen => {
                self.advance();
                // Parentheses reset the print context: `print ($1 > 2)` must
                // parse the comparison, not treat `>` as redirection.
                let saved_in_print = self.in_print;
                self.in_print = false;
                let expr = self.parse_expr();
                self.in_print = saved_in_print;
                self.expect(&Token::RParen)?;
                Ok(expr?)
            }
            _ => Err(format!("unexpected token: {:?}", self.peek())),
        }
    }
}

fn parse_program(input: &str) -> Result<AwkProgram, String> {
    let mut lexer = AwkLexer::new(input);
    let tokens = lexer.tokenize()?;
    let mut parser = AwkParser::new(tokens);
    parser.parse_program()
}

// ============================================================================
// Runtime / Evaluator
// ============================================================================

/// Format a number for `print` / string coercion, matching gawk's default OFMT (%.6g).
///
/// Rules (matching gawk 5.x):
/// - Zero (including -0.0): always `"0"`.
/// - Integral values (`fract() == 0.0`): emit all digits with no decimal point using
///   `format!("{:.0}", n)`.  This handles large magnitudes like 1e20 correctly without
///   casting to i64 (which would overflow or saturate).
/// - Non-integral values: implement C `%.6g` — 6 significant figures, scientific notation
///   when the rounded exponent is < -4 or >= 6, otherwise fixed, trailing zeros stripped.
fn format_awk_number(n: f64) -> String {
    // Non-finite values: replicate gawk 5.x exact spellings.
    // gawk: `BEGIN{print sqrt(-1)}` → `-nan`
    //       `BEGIN{print 2^1024}`   → `+inf`
    //       `BEGIN{print -2^1024}`  → `-inf`
    if n.is_nan() {
        return "-nan".to_string();
    }
    if n.is_infinite() {
        return if n.is_sign_positive() { "+inf" } else { "-inf" }.to_string();
    }
    if n == 0.0 {
        return "0".to_string();
    }
    if n.fract() == 0.0 {
        // Integral: print all significant digits (no cast to i64 — would overflow at 1e20).
        return format!("{:.0}", n);
    }
    format_awk_ofmt(n)
}

/// Implement C `%.6g` for non-integral awk values (OFMT default).
///
/// The trick: format with `%.5e` first to get the *rounded* exponent (5 decimal digits +
/// implicit 1 = 6 significant figures).  Then choose fixed vs. scientific based on that
/// exponent, following the C standard rule: scientific when e < -4 or e >= 6.
fn format_awk_ofmt(n: f64) -> String {
    const P: i32 = 6; // significant figures
    // %.5e gives 6 sig figs in scientific form; read the exponent from the result.
    let sci = format!("{:.5e}", n);
    // Rust's {:e} always includes 'e'; split at the last one.
    let e_pos = sci.rfind('e').unwrap_or(sci.len());
    let exp: i32 = sci.get(e_pos + 1..).and_then(|s| s.parse().ok()).unwrap_or(0);

    if !(-4..P).contains(&exp) {
        // Scientific notation: strip trailing zeros from the mantissa.
        let mantissa = sci[..e_pos].trim_end_matches('0').trim_end_matches('.');
        if exp >= 0 {
            format!("{}e+{:02}", mantissa, exp)
        } else {
            format!("{}e-{:02}", mantissa, -exp)
        }
    } else {
        // Fixed notation: (P-1-exp) decimal places, then strip trailing zeros.
        let dec = ((P - 1 - exp).max(0)) as usize;
        let raw = format!("{:.prec$}", n, prec = dec);
        raw.trim_end_matches('0').trim_end_matches('.').to_string()
    }
}

/// AWK value with string/number coercion.
#[derive(Debug, Clone, Default)]
enum AwkValue {
    /// A string *constant* (program literal, concatenation, function result).
    /// Never compares numerically — `"abc" == 0` is a string compare.
    String(String),
    /// A *numeric string* (POSIX strnum): a value derived from input — a field,
    /// a `split()` element, a `-v`/command-line assignment, `getline`. It
    /// compares numerically *only when it looks like a number* (`"0"` → numeric,
    /// `"abc"` → string), which is the whole point of the strnum concept.
    StrNum(String),
    Number(f64),
    #[default]
    Uninitialized,
}

impl std::fmt::Display for AwkValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AwkValue::String(s) | AwkValue::StrNum(s) => f.write_str(s),
            AwkValue::Number(n) => f.write_str(&format_awk_number(*n)),
            AwkValue::Uninitialized => Ok(()),
        }
    }
}

impl super::format_string::FormatArg for AwkValue {
    fn as_format_string(&self) -> String { self.to_string() }
    fn as_format_int(&self) -> i64 { self.to_number() as i64 }
    fn as_format_float(&self) -> f64 { self.to_number() }
    fn as_format_char(&self) -> Option<char> {
        let n = self.to_number() as u32;
        char::from_u32(n)
    }
}

impl AwkValue {

    fn to_number(&self) -> f64 {
        match self {
            AwkValue::Number(n) => *n,
            AwkValue::String(s) | AwkValue::StrNum(s) => parse_awk_number(s),
            AwkValue::Uninitialized => 0.0,
        }
    }

    fn to_bool(&self) -> bool {
        match self {
            AwkValue::Number(n) => *n != 0.0,
            // A string *constant* is true iff non-empty. A *numeric string*
            // (strnum) follows its numeric value when it looks like a number
            // (`$1 == "0"` is false in a boolean context), else non-emptiness.
            AwkValue::String(s) => !s.is_empty(),
            AwkValue::StrNum(s) => {
                if str_looks_numeric(s) {
                    parse_awk_number(s) != 0.0
                } else {
                    !s.is_empty()
                }
            }
            AwkValue::Uninitialized => false,
        }
    }
}


/// Whether the *whole* (trimmed) string is a valid number — the POSIX strnum
/// test. Stricter than [`parse_awk_number`], which extracts a leading numeric
/// prefix (`"5abc"` → 5 in numeric context, but `"5abc"` is *not* a numeric
/// string). Hex/inf/nan are rejected to match awk (`"0x10" != 16`).
fn str_looks_numeric(s: &str) -> bool {
    let s = s.trim();
    if s.is_empty() {
        return false;
    }
    match s.parse::<f64>() {
        Ok(n) => n.is_finite(),
        Err(_) => false,
    }
}

fn parse_awk_number(s: &str) -> f64 {
    let s = s.trim();
    if s.is_empty() {
        return 0.0;
    }
    // AWK numeric parsing: leading numeric portion
    let mut end = 0;
    let mut saw_dot = false;
    let mut saw_exp = false;
    let chars: Vec<char> = s.chars().collect();

    if end < chars.len() && (chars[end] == '+' || chars[end] == '-') {
        end += 1;
    }

    while end < chars.len() {
        let c = chars[end];
        if c.is_ascii_digit() {
            end += 1;
        } else if c == '.' && !saw_dot && !saw_exp {
            saw_dot = true;
            end += 1;
        } else if (c == 'e' || c == 'E') && !saw_exp && end > 0 {
            saw_exp = true;
            end += 1;
            if end < chars.len() && (chars[end] == '+' || chars[end] == '-') {
                end += 1;
            }
        } else {
            break;
        }
    }

    if end == 0 {
        return 0.0;
    }

    s[..chars[..end].iter().map(|c| c.len_utf8()).sum()].parse().unwrap_or(0.0)
}

/// Control flow signals.
enum ControlFlow {
    Normal,
    Break,
    Continue,
    Next,
    /// `exit [N]`. `None` is a bare `exit`, which keeps the most recently set
    /// exit status (POSIX) rather than resetting it to 0.
    Exit(Option<i32>),
}

/// Runtime state for AWK execution.
struct AwkRuntime {
    vars: HashMap<String, AwkValue>,
    arrays: HashMap<String, HashMap<String, AwkValue>>,
    fields: Vec<AwkValue>,
    output: String,
    nr: i64, // Current record number
    nf: i64, // Number of fields in current record
    /// Per-rule active state for range patterns (`/start/,/end/`).
    /// Indexed by the rule's position in `AwkProgram.rules`.
    /// Non-range rules always read `false` here and never write it.
    range_active: Vec<bool>,
}

impl AwkRuntime {
    fn new() -> Self {
        let mut vars = HashMap::new();
        // Default special variables
        vars.insert("FS".to_string(), AwkValue::String(" ".to_string()));
        vars.insert("RS".to_string(), AwkValue::String("\n".to_string()));
        vars.insert("OFS".to_string(), AwkValue::String(" ".to_string()));
        vars.insert("ORS".to_string(), AwkValue::String("\n".to_string()));
        vars.insert("NR".to_string(), AwkValue::Number(0.0));
        vars.insert("NF".to_string(), AwkValue::Number(0.0));
        vars.insert("FILENAME".to_string(), AwkValue::String(String::new()));

        Self {
            vars,
            arrays: HashMap::new(),
            fields: vec![AwkValue::Uninitialized], // $0
            output: String::new(),
            nr: 0,
            nf: 0,
            range_active: Vec::new(),
        }
    }

    fn set_var(&mut self, name: &str, value: AwkValue) {
        self.vars.insert(name.to_string(), value);
    }

    fn get_var(&self, name: &str) -> AwkValue {
        self.vars.get(name).cloned().unwrap_or(AwkValue::Uninitialized)
    }

    fn get_field(&self, n: i64) -> AwkValue {
        // A referenced-but-absent field is the *string* `""` (not Uninitialized,
        // which is numeric-and-string): gawk compares `$9 == 0` as strings →
        // false, distinct from an unset *variable*. $0 and in-range fields are
        // always present.
        if n < 0 {
            return AwkValue::String(String::new());
        }
        let idx = n as usize;
        self.fields.get(idx).cloned().unwrap_or_else(|| AwkValue::String(String::new()))
    }

    fn set_field(&mut self, n: i64, value: AwkValue) -> Result<(), String> {
        if n < 0 {
            return Ok(());
        }
        // Assigning `$0` re-splits the record into $1..$NF and resets NF — the
        // `n > 0` rebuild path below only covers individual-field edits.
        if n == 0 {
            return self.split_record(&value.to_string());
        }
        let idx = n as usize;
        if idx >= self.fields.len() {
            self.fields.resize(idx + 1, AwkValue::Uninitialized);
        }
        self.fields[idx] = value;

        // Rebuild $0 after an individual-field edit; extend NF if past the end.
        if n > self.nf {
            self.nf = n;
            self.set_var("NF", AwkValue::Number(n as f64));
        }
        self.rebuild_record();
        Ok(())
    }

    fn rebuild_record(&mut self) {
        let ofs = self.get_var("OFS").to_string();
        let parts: Vec<String> = self.fields[1..].iter().map(|f| f.to_string()).collect();
        self.fields[0] = AwkValue::StrNum(parts.join(&ofs));
    }

    fn split_record(&mut self, record: &str) -> Result<(), String> {
        let fs_raw = self.get_var("FS").to_string();
        self.fields = vec![AwkValue::StrNum(record.to_string())];

        // Rewrite BRE metas BEFORE the single-char check: gawk demotes `\|` to
        // plain `|` and a single-char FS is literal (POSIX), so `-F '\|'` /
        // FS="\\|" split on a literal pipe — never the empty-alternation regex
        // `|`, which would silently split between every character.
        let fs = bre_metas_to_ere(&fs_raw);
        let parts: Vec<&str> = if fs == " " {
            // Default: split on whitespace, trim leading/trailing
            record.split_whitespace().collect()
        } else if fs.chars().count() == 1 {
            // A single-character FS is a literal separator (POSIX), never a
            // regex — `-F '['` splits on a literal `[`.
            record.split(&fs).collect()
        } else {
            // Multi-character FS is an ERE. An invalid regex is a loud error,
            // not a silent literal-split fallback (which silently miscounts
            // fields). Matches gawk's fatal behavior. Report the FS as the user
            // wrote it, with the dialect hint when the rewrite changed it.
            let re = Regex::new(&fs).map_err(|e| {
                append_dialect_hint(
                    format!("invalid FS regex {:?}: {}", fs_raw, e),
                    fs != fs_raw,
                    None,
                )
            })?;
            re.split(record).collect()
        };

        for part in &parts {
            self.fields.push(AwkValue::StrNum(part.to_string()));
        }

        self.nf = (self.fields.len() - 1) as i64;
        self.set_var("NF", AwkValue::Number(self.nf as f64));
        Ok(())
    }

    fn execute(&mut self, program: &AwkProgram, input: &str) -> Result<(String, Option<i32>), String> {
        // Size range_active to the number of rules; non-range rules never touch it.
        self.range_active = vec![false; program.rules.len()];

        // The exit code from the most recent `exit N` (None until one fires).
        let mut exit_code: Option<i32> = None;

        // Run BEGIN rules
        for rule in &program.rules {
            if matches!(rule.pattern, Pattern::Begin)
                && let ControlFlow::Exit(code) = self.execute_block(&rule.action)?
            {
                // `exit` in BEGIN skips main + other BEGINs but still runs END.
                // A bare `exit` (None) keeps the current code (0 so far here).
                if let Some(c) = code {
                    exit_code = Some(c);
                }
                return self.run_end_rules(program, exit_code);
            }
        }

        // Process records
        let rs = self.get_var("RS").to_string();
        let records: Vec<&str> = if rs == "\n" {
            input.lines().collect()
        } else if rs.is_empty() {
            // Paragraph mode
            input.split("\n\n").collect()
        } else {
            input.split(&rs).collect()
        };

        'records: for record in records {
            self.nr += 1;
            self.set_var("NR", AwkValue::Number(self.nr as f64));
            self.split_record(record)?;

            for (rule_idx, rule) in program.rules.iter().enumerate() {
                if matches!(rule.pattern, Pattern::Begin | Pattern::End) {
                    continue;
                }

                if self.pattern_matches(rule_idx, &rule.pattern)? {
                    match self.execute_block(&rule.action)? {
                        ControlFlow::Next => continue 'records,
                        ControlFlow::Exit(code) => {
                            // Bare `exit` (None) keeps the current code.
                            if let Some(c) = code {
                                exit_code = Some(c);
                            }
                            break 'records;
                        }
                        ControlFlow::Break | ControlFlow::Continue => {
                            return Err("break/continue outside loop".to_string())
                        }
                        ControlFlow::Normal => {}
                    }
                }
            }
        }

        self.run_end_rules(program, exit_code)
    }

    /// Run all END rules, then return the accumulated output and the final exit
    /// code. An `exit N` inside END overrides any prior code and stops further
    /// END rules; a bare `exit` keeps the current code (POSIX).
    fn run_end_rules(
        &mut self,
        program: &AwkProgram,
        mut exit_code: Option<i32>,
    ) -> Result<(String, Option<i32>), String> {
        for rule in &program.rules {
            if matches!(rule.pattern, Pattern::End)
                && let ControlFlow::Exit(code) = self.execute_block(&rule.action)?
            {
                if let Some(c) = code {
                    exit_code = Some(c);
                }
                break;
            }
        }
        Ok((std::mem::take(&mut self.output), exit_code))
    }

    fn pattern_matches(&mut self, rule_idx: usize, pattern: &Pattern) -> Result<bool, String> {
        match pattern {
            Pattern::All => Ok(true),
            Pattern::Begin | Pattern::End => Ok(false),
            Pattern::Regex(re) => {
                let regex = compile_ere(re)?;
                Ok(regex.is_match(&self.get_field(0).to_string()))
            }
            Pattern::Expr(expr) => {
                let val = self.eval_expr(expr)?;
                Ok(val.to_bool())
            }
            Pattern::Range(start_pat, end_pat) => {
                // POSIX range pattern semantics (matching gawk):
                //
                // When NOT active: test `start`. If it matches, mark active and
                // fire this record. If `end` also matches the same record, mark
                // inactive immediately (one-record range). Either way fire = true.
                //
                // When active: fire this record. Then test `end`; if it matches,
                // mark inactive (end record is still included).
                let active = self.range_active[rule_idx];
                if !active {
                    // Helper to evaluate a simple (non-Range) pattern inline.
                    let start_matches = self.eval_simple_pattern(start_pat)?;
                    if start_matches {
                        // Check if end also matches this same record.
                        let end_matches = self.eval_simple_pattern(end_pat)?;
                        self.range_active[rule_idx] = !end_matches;
                        Ok(true)
                    } else {
                        Ok(false)
                    }
                } else {
                    // We are inside an active range: fire, then check end.
                    let end_matches = self.eval_simple_pattern(end_pat)?;
                    if end_matches {
                        self.range_active[rule_idx] = false;
                    }
                    Ok(true)
                }
            }
        }
    }

    /// Evaluate a non-Range pattern as a boolean (used for range endpoints).
    fn eval_simple_pattern(&mut self, pattern: &Pattern) -> Result<bool, String> {
        match pattern {
            Pattern::Regex(re) => {
                let regex = compile_ere(re)?;
                Ok(regex.is_match(&self.get_field(0).to_string()))
            }
            Pattern::Expr(expr) => {
                let val = self.eval_expr(expr)?;
                Ok(val.to_bool())
            }
            Pattern::All => Ok(true),
            // BEGIN/END are rejected at parse time; this branch is unreachable.
            Pattern::Begin | Pattern::End => {
                Err("BEGIN/END may not be used as range pattern endpoints".to_string())
            }
            Pattern::Range(_, _) => {
                Err("nested range patterns are not supported".to_string())
            }
        }
    }

    fn execute_block(&mut self, block: &Block) -> Result<ControlFlow, String> {
        for stmt in block {
            match self.execute_stmt(stmt)? {
                ControlFlow::Normal => {}
                cf => return Ok(cf),
            }
        }
        Ok(ControlFlow::Normal)
    }

    fn execute_stmt(&mut self, stmt: &Stmt) -> Result<ControlFlow, String> {
        match stmt {
            Stmt::Print(args) => {
                let ofs = self.get_var("OFS").to_string();
                let ors = self.get_var("ORS").to_string();

                if args.is_empty() {
                    self.output.push_str(&self.get_field(0).to_string());
                } else {
                    let parts: Result<Vec<String>, String> =
                        args.iter().map(|e| Ok(self.eval_expr(e)?.to_string())).collect();
                    self.output.push_str(&parts?.join(&ofs));
                }
                self.output.push_str(&ors);
                Ok(ControlFlow::Normal)
            }

            Stmt::Printf(args) => {
                if args.is_empty() {
                    return Ok(ControlFlow::Normal);
                }
                let format = self.eval_expr(&args[0])?.to_string();
                let vals: Result<Vec<AwkValue>, String> =
                    args[1..].iter().map(|e| self.eval_expr(e)).collect();
                let formatted = self.sprintf(&format, &vals?)?;
                self.output.push_str(&formatted);
                Ok(ControlFlow::Normal)
            }

            Stmt::Assign(lvalue, op, expr) => {
                let rhs = self.eval_expr(expr)?;
                let new_val = match op {
                    AssignOp::Assign => rhs,
                    AssignOp::AddAssign => {
                        let lhs = self.get_lvalue(lvalue)?;
                        AwkValue::Number(lhs.to_number() + rhs.to_number())
                    }
                    AssignOp::SubAssign => {
                        let lhs = self.get_lvalue(lvalue)?;
                        AwkValue::Number(lhs.to_number() - rhs.to_number())
                    }
                    AssignOp::MulAssign => {
                        let lhs = self.get_lvalue(lvalue)?;
                        AwkValue::Number(lhs.to_number() * rhs.to_number())
                    }
                    AssignOp::DivAssign => {
                        let lhs = self.get_lvalue(lvalue)?;
                        let divisor = rhs.to_number();
                        if divisor == 0.0 {
                            return Err("division by zero".to_string());
                        }
                        AwkValue::Number(lhs.to_number() / divisor)
                    }
                    AssignOp::ModAssign => {
                        let lhs = self.get_lvalue(lvalue)?;
                        let divisor = rhs.to_number();
                        if divisor == 0.0 {
                            return Err("division by zero".to_string());
                        }
                        AwkValue::Number(lhs.to_number() % divisor)
                    }
                };
                self.set_lvalue(lvalue, new_val)?;
                Ok(ControlFlow::Normal)
            }

            Stmt::If(cond, then_block, else_block) => {
                let cond_val = self.eval_expr(cond)?;
                if cond_val.to_bool() {
                    self.execute_block(then_block)
                } else if let Some(else_b) = else_block {
                    self.execute_block(else_b)
                } else {
                    Ok(ControlFlow::Normal)
                }
            }

            Stmt::While(cond, body) => {
                loop {
                    let cond_val = self.eval_expr(cond)?;
                    if !cond_val.to_bool() {
                        break;
                    }
                    match self.execute_block(body)? {
                        ControlFlow::Break => break,
                        ControlFlow::Continue => continue,
                        ControlFlow::Next => return Ok(ControlFlow::Next),
                        ControlFlow::Exit(code) => return Ok(ControlFlow::Exit(code)),
                        ControlFlow::Normal => {}
                    }
                }
                Ok(ControlFlow::Normal)
            }

            Stmt::For(init, cond, incr, body) => {
                if let Some(init_stmt) = init {
                    self.execute_stmt(init_stmt)?;
                }
                loop {
                    if let Some(cond_expr) = cond {
                        let cond_val = self.eval_expr(cond_expr)?;
                        if !cond_val.to_bool() {
                            break;
                        }
                    }
                    match self.execute_block(body)? {
                        ControlFlow::Break => break,
                        ControlFlow::Continue => {}
                        ControlFlow::Next => return Ok(ControlFlow::Next),
                        ControlFlow::Exit(code) => return Ok(ControlFlow::Exit(code)),
                        ControlFlow::Normal => {}
                    }
                    if let Some(incr_stmt) = incr {
                        self.execute_stmt(incr_stmt)?;
                    }
                }
                Ok(ControlFlow::Normal)
            }

            Stmt::ForIn(var, arr_name, body) => {
                let keys: Vec<String> = self
                    .arrays
                    .get(arr_name)
                    .map(|arr| arr.keys().cloned().collect())
                    .unwrap_or_default();

                for key in keys {
                    // Array subscripts are numeric strings — `for (k in a)` then
                    // `if (k == 1)` compares numerically when the key looks numeric.
                    self.set_var(var, AwkValue::StrNum(key));
                    match self.execute_block(body)? {
                        ControlFlow::Break => break,
                        ControlFlow::Continue => continue,
                        ControlFlow::Next => return Ok(ControlFlow::Next),
                        ControlFlow::Exit(code) => return Ok(ControlFlow::Exit(code)),
                        ControlFlow::Normal => {}
                    }
                }
                Ok(ControlFlow::Normal)
            }

            Stmt::Break => Ok(ControlFlow::Break),
            Stmt::Continue => Ok(ControlFlow::Continue),
            Stmt::Next => Ok(ControlFlow::Next),
            Stmt::Exit(code) => {
                // Bare `exit` (no expression) → None: keep the current status.
                let exit_code = code
                    .as_ref()
                    .map(|e| self.eval_expr(e).map(|v| v.to_number() as i32))
                    .transpose()?;
                Ok(ControlFlow::Exit(exit_code))
            }

            Stmt::Delete(arr_name, key_expr) => {
                let key = self.eval_expr(key_expr)?.to_string();
                if let Some(arr) = self.arrays.get_mut(arr_name) {
                    arr.remove(&key);
                }
                Ok(ControlFlow::Normal)
            }

            Stmt::Expr(expr) => {
                self.eval_expr(expr)?;
                Ok(ControlFlow::Normal)
            }
        }
    }

    fn get_lvalue(&mut self, lvalue: &LValue) -> Result<AwkValue, String> {
        match lvalue {
            LValue::Var(name) => Ok(self.get_var(name)),
            LValue::Field(expr) => {
                let n = self.eval_expr(expr)?.to_number() as i64;
                Ok(self.get_field(n))
            }
            LValue::ArrayElem(arr_name, key_expr) => {
                let key = self.eval_expr(key_expr)?.to_string();
                Ok(self
                    .arrays
                    .get(arr_name)
                    .and_then(|arr| arr.get(&key))
                    .cloned()
                    .unwrap_or(AwkValue::Uninitialized))
            }
        }
    }

    fn set_lvalue(&mut self, lvalue: &LValue, value: AwkValue) -> Result<(), String> {
        match lvalue {
            LValue::Var(name) => {
                self.set_var(name, value);
            }
            LValue::Field(expr) => {
                let n = self.eval_expr(expr)?.to_number() as i64;
                self.set_field(n, value)?;
            }
            LValue::ArrayElem(arr_name, key_expr) => {
                let key = self.eval_expr(key_expr)?.to_string();
                self.arrays
                    .entry(arr_name.clone())
                    .or_default()
                    .insert(key, value);
            }
        }
        Ok(())
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<AwkValue, String> {
        match expr {
            Expr::Number(n) => Ok(AwkValue::Number(*n)),
            Expr::String(s) => Ok(AwkValue::String(s.clone())),
            Expr::Regex(re) => {
                // When regex is used as expression, match against $0
                let regex = compile_ere(re)?;
                let matched = regex.is_match(&self.get_field(0).to_string());
                Ok(AwkValue::Number(if matched { 1.0 } else { 0.0 }))
            }
            Expr::Var(name) => {
                // Bare `length` (no parens) is `length($0)` in awk.
                if name == "length" {
                    let s = self.get_field(0).to_string();
                    return Ok(AwkValue::Number(s.chars().count() as f64));
                }
                Ok(self.get_var(name))
            }
            Expr::Field(n_expr) => {
                let n = self.eval_expr(n_expr)?.to_number() as i64;
                Ok(self.get_field(n))
            }
            Expr::ArrayAccess(arr_name, key_expr) => {
                let key = self.eval_expr(key_expr)?.to_string();
                Ok(self
                    .arrays
                    .get(arr_name)
                    .and_then(|arr| arr.get(&key))
                    .cloned()
                    .unwrap_or(AwkValue::Uninitialized))
            }
            Expr::BinOp(left, op, right) => self.eval_binop(left, *op, right),
            Expr::UnaryOp(op, expr) => {
                let val = self.eval_expr(expr)?;
                match op {
                    UnaryOp::Neg => Ok(AwkValue::Number(-val.to_number())),
                    UnaryOp::Pos => Ok(AwkValue::Number(val.to_number())),
                    UnaryOp::Not => Ok(AwkValue::Number(if val.to_bool() { 0.0 } else { 1.0 })),
                }
            }
            Expr::Ternary(cond, then_expr, else_expr) => {
                let cond_val = self.eval_expr(cond)?;
                if cond_val.to_bool() {
                    self.eval_expr(then_expr)
                } else {
                    self.eval_expr(else_expr)
                }
            }
            Expr::Call(name, args) => self.call_function(name, args),
            Expr::Increment(lvalue, is_pre, is_incr) => {
                self.eval_increment(lvalue, *is_pre, *is_incr)
            }
            Expr::Concat(parts) => {
                let mut result = String::new();
                for part in parts {
                    result.push_str(&self.eval_expr(part)?.to_string());
                }
                Ok(AwkValue::String(result))
            }
            Expr::In(key_expr, arr_name) => {
                let key = self.eval_expr(key_expr)?.to_string();
                let exists = self
                    .arrays
                    .get(arr_name)
                    .map(|arr| arr.contains_key(&key))
                    .unwrap_or(false);
                Ok(AwkValue::Number(if exists { 1.0 } else { 0.0 }))
            }
        }
    }

    fn eval_increment(&mut self, lvalue: &LValue, is_pre: bool, is_incr: bool) -> Result<AwkValue, String> {
        let old_val = self.get_lvalue(lvalue)?;
        let old_num = old_val.to_number();
        let delta = if is_incr { 1.0 } else { -1.0 };
        let new_num = old_num + delta;

        // Perform the mutation
        self.set_lvalue(lvalue, AwkValue::Number(new_num))?;

        if is_pre {
            Ok(AwkValue::Number(new_num))
        } else {
            Ok(AwkValue::Number(old_num))
        }
    }

    fn eval_binop(&mut self, left: &Expr, op: BinOp, right: &Expr) -> Result<AwkValue, String> {
        match op {
            BinOp::And => {
                let l = self.eval_expr(left)?;
                if !l.to_bool() {
                    return Ok(AwkValue::Number(0.0));
                }
                let r = self.eval_expr(right)?;
                Ok(AwkValue::Number(if r.to_bool() { 1.0 } else { 0.0 }))
            }
            BinOp::Or => {
                let l = self.eval_expr(left)?;
                if l.to_bool() {
                    return Ok(AwkValue::Number(1.0));
                }
                let r = self.eval_expr(right)?;
                Ok(AwkValue::Number(if r.to_bool() { 1.0 } else { 0.0 }))
            }
            BinOp::Match | BinOp::NotMatch => {
                let l = self.eval_expr(left)?.to_string();
                // For ~ and !~, get the regex pattern string directly if it's a regex literal
                let pattern = match right {
                    Expr::Regex(re) => re.clone(),
                    _ => self.eval_expr(right)?.to_string(),
                };
                let regex = compile_ere(&pattern)?;
                let matched = regex.is_match(&l);
                let result = if op == BinOp::Match { matched } else { !matched };
                Ok(AwkValue::Number(if result { 1.0 } else { 0.0 }))
            }
            _ => {
                let l = self.eval_expr(left)?;
                let r = self.eval_expr(right)?;

                match op {
                    BinOp::Add => Ok(AwkValue::Number(l.to_number() + r.to_number())),
                    BinOp::Sub => Ok(AwkValue::Number(l.to_number() - r.to_number())),
                    BinOp::Mul => Ok(AwkValue::Number(l.to_number() * r.to_number())),
                    BinOp::Div => {
                        let divisor = r.to_number();
                        if divisor == 0.0 {
                            return Err("division by zero".to_string());
                        }
                        Ok(AwkValue::Number(l.to_number() / divisor))
                    }
                    BinOp::Mod => {
                        let divisor = r.to_number();
                        if divisor == 0.0 {
                            return Err("division by zero".to_string());
                        }
                        Ok(AwkValue::Number(l.to_number() % divisor))
                    }
                    BinOp::Pow => Ok(AwkValue::Number(l.to_number().powf(r.to_number()))),
                    BinOp::Eq => {
                        let result = self.compare_values(&l, &r) == std::cmp::Ordering::Equal;
                        Ok(AwkValue::Number(if result { 1.0 } else { 0.0 }))
                    }
                    BinOp::Ne => {
                        let result = self.compare_values(&l, &r) != std::cmp::Ordering::Equal;
                        Ok(AwkValue::Number(if result { 1.0 } else { 0.0 }))
                    }
                    BinOp::Lt => {
                        let result = self.compare_values(&l, &r) == std::cmp::Ordering::Less;
                        Ok(AwkValue::Number(if result { 1.0 } else { 0.0 }))
                    }
                    BinOp::Le => {
                        let result = self.compare_values(&l, &r) != std::cmp::Ordering::Greater;
                        Ok(AwkValue::Number(if result { 1.0 } else { 0.0 }))
                    }
                    BinOp::Gt => {
                        let result = self.compare_values(&l, &r) == std::cmp::Ordering::Greater;
                        Ok(AwkValue::Number(if result { 1.0 } else { 0.0 }))
                    }
                    BinOp::Ge => {
                        let result = self.compare_values(&l, &r) != std::cmp::Ordering::Less;
                        Ok(AwkValue::Number(if result { 1.0 } else { 0.0 }))
                    }
                    BinOp::And | BinOp::Or | BinOp::Match | BinOp::NotMatch => {
                        unreachable!()
                    }
                }
            }
        }
    }

    fn compare_values(&self, l: &AwkValue, r: &AwkValue) -> std::cmp::Ordering {
        // POSIX comparison rule: compare numerically iff *both* operands are
        // numeric-for-comparison — a Number, an uninitialized value, or a
        // numeric string (strnum) that looks like a number. A string constant
        // is never numeric, so `"abc" == 0` and `$1("abc") == 0` are string
        // compares (not a silent `"abc" → 0.0` coercion, the old `||` bug).
        if Self::numeric_for_compare(l) && Self::numeric_for_compare(r) {
            l.to_number()
                .partial_cmp(&r.to_number())
                .unwrap_or(std::cmp::Ordering::Equal)
        } else {
            l.to_string().cmp(&r.to_string())
        }
    }

    /// Whether a value participates in a *numeric* comparison (see POSIX strnum
    /// rules in [`Self::compare_values`]).
    fn numeric_for_compare(val: &AwkValue) -> bool {
        match val {
            AwkValue::Number(_) => true,
            AwkValue::Uninitialized => true, // behaves as 0
            AwkValue::StrNum(s) => str_looks_numeric(s),
            AwkValue::String(_) => false, // a string constant never compares numerically
        }
    }

    fn call_function(&mut self, name: &str, args: &[Expr]) -> Result<AwkValue, String> {
        match name {
            "length" => {
                // `length(arr)` on an array name is the element count. A bare
                // variable that names an array must not be eval'd as a scalar
                // (that yields Uninitialized → "" → 0).
                if let [Expr::Var(name)] = args
                    && let Some(arr) = self.arrays.get(name)
                {
                    return Ok(AwkValue::Number(arr.len() as f64));
                }
                let s = if args.is_empty() {
                    self.get_field(0).to_string()
                } else {
                    self.eval_expr(&args[0])?.to_string()
                };
                Ok(AwkValue::Number(s.chars().count() as f64))
            }
            "substr" => {
                if args.is_empty() {
                    return Err("substr requires at least 2 arguments".to_string());
                }
                let s = self.eval_expr(&args[0])?.to_string();
                let start = if args.len() > 1 {
                    (self.eval_expr(&args[1])?.to_number() as i64).max(1) as usize - 1
                } else {
                    0
                };
                let len = if args.len() > 2 {
                    Some(self.eval_expr(&args[2])?.to_number() as usize)
                } else {
                    None
                };
                let chars: Vec<char> = s.chars().collect();
                let end = len.map(|l| (start + l).min(chars.len())).unwrap_or(chars.len());
                let result: String = chars[start.min(chars.len())..end].iter().collect();
                Ok(AwkValue::String(result))
            }
            "index" => {
                if args.len() < 2 {
                    return Err("index requires 2 arguments".to_string());
                }
                let s = self.eval_expr(&args[0])?.to_string();
                let t = self.eval_expr(&args[1])?.to_string();
                let pos = s.find(&t).map(|i| {
                    s[..i].chars().count() + 1
                }).unwrap_or(0);
                Ok(AwkValue::Number(pos as f64))
            }
            "split" => {
                if args.len() < 2 {
                    return Err("split requires at least 2 arguments".to_string());
                }
                let s = self.eval_expr(&args[0])?.to_string();
                // Validate that second arg is an array variable — get name before mutating
                let arr_name = match &args[1] {
                    Expr::Var(name) => name.clone(),
                    _ => return Err("split: second argument must be an array".to_string()),
                };
                let sep = if args.len() > 2 {
                    self.eval_expr(&args[2])?.to_string()
                } else {
                    self.get_var("FS").to_string()
                };

                // gawk: empty input string → clear the array and return 0 for all
                // separator modes (Rust's str::split("") yields [""] → count 1, wrong).
                if s.is_empty() {
                    self.arrays.entry(arr_name).or_default().clear();
                    return Ok(AwkValue::Number(0.0));
                }

                // Honor the same separator rules as split_record:
                //   BRE-meta rewrite first (so `"\\|"` → `|` → literal, like gawk)
                //   " " (default FS) → whitespace mode (split on runs, trim ends)
                //   single char → literal split
                //   multi-char → ERE regex
                let sep_raw = sep;
                let sep = bre_metas_to_ere(&sep_raw);
                let parts: Vec<String> = if sep == " " {
                    s.split_whitespace().map(str::to_string).collect()
                } else if sep.chars().count() == 1 {
                    s.split(sep.as_str()).map(str::to_string).collect()
                } else {
                    // Multi-char separator is an ERE; an invalid regex is a loud
                    // error, not a silent literal-split fallback (gawk: fatal).
                    // Report the separator as the user wrote it, with the
                    // dialect hint when the rewrite changed it.
                    let re = Regex::new(&sep).map_err(|e| {
                        append_dialect_hint(
                            format!("invalid split() separator regex {:?}: {}", sep_raw, e),
                            sep != sep_raw,
                            None,
                        )
                    })?;
                    re.split(&s).map(str::to_string).collect()
                };

                let count = parts.len();

                // Clear the target array first (awk semantics: split always empties it),
                // then fill with 1-indexed keys. Elements are numeric strings.
                let arr = self.arrays.entry(arr_name).or_default();
                arr.clear();
                for (i, part) in parts.into_iter().enumerate() {
                    arr.insert((i + 1).to_string(), AwkValue::StrNum(part));
                }

                Ok(AwkValue::Number(count as f64))
            }
            "sprintf" => {
                if args.is_empty() {
                    return Ok(AwkValue::String(String::new()));
                }
                let format = self.eval_expr(&args[0])?.to_string();
                let vals: Result<Vec<AwkValue>, String> =
                    args[1..].iter().map(|e| self.eval_expr(e)).collect();
                let result = self.sprintf(&format, &vals?)?;
                Ok(AwkValue::String(result))
            }
            "tolower" => {
                let s = if args.is_empty() {
                    self.get_field(0).to_string()
                } else {
                    self.eval_expr(&args[0])?.to_string()
                };
                Ok(AwkValue::String(s.to_lowercase()))
            }
            "toupper" => {
                let s = if args.is_empty() {
                    self.get_field(0).to_string()
                } else {
                    self.eval_expr(&args[0])?.to_string()
                };
                Ok(AwkValue::String(s.to_uppercase()))
            }
            "match" => {
                if args.len() < 2 {
                    return Err("match requires 2 arguments".to_string());
                }
                let s = self.eval_expr(&args[0])?.to_string();
                // Regex literal or string — mirror the sub/gsub handling.
                let pattern = match &args[1] {
                    Expr::Regex(re) => re.clone(),
                    other => self.eval_expr(other)?.to_string(),
                };
                let regex = compile_ere(&pattern)?;
                if let Some(m) = regex.find(&s) {
                    let rstart = s[..m.start()].chars().count() as i64 + 1;
                    let rlength = m.as_str().chars().count() as i64;
                    self.set_var("RSTART", AwkValue::Number(rstart as f64));
                    self.set_var("RLENGTH", AwkValue::Number(rlength as f64));
                    Ok(AwkValue::Number(rstart as f64))
                } else {
                    self.set_var("RSTART", AwkValue::Number(0.0));
                    self.set_var("RLENGTH", AwkValue::Number(-1.0));
                    Ok(AwkValue::Number(0.0))
                }
            }
            "sub" | "gsub" => {
                if args.len() < 2 {
                    return Err(format!("{name} requires at least 2 arguments"));
                }

                // Evaluate the regex pattern (arg 0) — regex literal or string.
                let pattern_str = match &args[0] {
                    Expr::Regex(re) => re.clone(),
                    other => self.eval_expr(other)?.to_string(),
                };
                let regex = compile_ere(&pattern_str)?;

                // Evaluate the replacement template string (arg 1).
                let repl_template = self.eval_expr(&args[1])?.to_string();

                // Determine the target lvalue (arg 2 optional; default is $0).
                // Clone the arg expression so we can use &mut self freely afterwards.
                let target_lvalue: LValue = if args.len() >= 3 {
                    let target_expr = args[2].clone();
                    match &target_expr {
                        Expr::Var(name) => LValue::Var(name.clone()),
                        Expr::Field(inner) => {
                            // Evaluate the field index expression to get the field number.
                            let field_num = self.eval_expr(inner)?.to_number() as i64;
                            LValue::Field(Box::new(Expr::Number(field_num as f64)))
                        }
                        Expr::ArrayAccess(arr, key) => {
                            let key_val = self.eval_expr(key)?.to_string();
                            LValue::ArrayElem(arr.clone(), Box::new(Expr::String(key_val)))
                        }
                        _ => {
                            return Err(format!(
                                "{name}: third argument must be an lvalue (var, field, or array element)"
                            ));
                        }
                    }
                } else {
                    // Default target is $0.
                    LValue::Field(Box::new(Expr::Number(0.0)))
                };

                // Read the current string value of the target (owned — no active borrow).
                let target_str = self.get_lvalue(&target_lvalue)?.to_string();

                // Expand replacement template: & → matched text, \& → literal &, \\ → \.
                // We do this by hand — the `regex` crate uses $-syntax which differs.
                fn expand_repl(template: &str, matched: &str) -> String {
                    let mut out = String::new();
                    let mut chars = template.chars().peekable();
                    while let Some(c) = chars.next() {
                        if c == '\\' {
                            match chars.next() {
                                Some('&') => out.push('&'),
                                Some('\\') => out.push('\\'),
                                Some(other) => {
                                    out.push('\\');
                                    out.push(other);
                                }
                                None => out.push('\\'),
                            }
                        } else if c == '&' {
                            out.push_str(matched);
                        } else {
                            out.push(c);
                        }
                    }
                    out
                }

                // Perform substitution (sub = first match only; gsub = all matches).
                let is_global = name == "gsub";
                let mut replacement_count: usize = 0;
                let result_str: String;

                if is_global {
                    // For gsub: iterate over all matches and replace them.
                    let mut out = String::new();
                    let mut last_end = 0;
                    for mat in regex.find_iter(&target_str) {
                        out.push_str(&target_str[last_end..mat.start()]);
                        out.push_str(&expand_repl(&repl_template, mat.as_str()));
                        last_end = mat.end();
                        replacement_count += 1;
                    }
                    out.push_str(&target_str[last_end..]);
                    result_str = out;
                } else {
                    // For sub: replace only the first match.
                    if let Some(mat) = regex.find(&target_str) {
                        let expanded = expand_repl(&repl_template, mat.as_str());
                        result_str = format!(
                            "{}{}{}",
                            &target_str[..mat.start()],
                            expanded,
                            &target_str[mat.end()..]
                        );
                        replacement_count = 1;
                    } else {
                        result_str = target_str;
                    }
                }

                // Write the substituted string back through the lvalue. Fields
                // are numeric strings; `set_field(0, …)` re-splits $0 itself.
                match &target_lvalue {
                    LValue::Field(field_expr) => {
                        let field_num = match field_expr.as_ref() {
                            Expr::Number(n) => *n as i64,
                            // We only ever construct Expr::Number for the field index above.
                            _ => unreachable!(
                                "sub/gsub field lvalue must be Expr::Number; got {:?}",
                                field_expr
                            ),
                        };
                        self.set_field(field_num, AwkValue::StrNum(result_str))?;
                    }
                    LValue::Var(name) => {
                        self.set_var(name, AwkValue::String(result_str));
                    }
                    LValue::ArrayElem(arr_name, key_expr) => {
                        let key = match key_expr.as_ref() {
                            Expr::String(s) => s.clone(),
                            // Key was already evaluated to Expr::String above.
                            _ => unreachable!(
                                "sub/gsub array key lvalue must be Expr::String; got {:?}",
                                key_expr
                            ),
                        };
                        self.arrays
                            .entry(arr_name.clone())
                            .or_default()
                            .insert(key, AwkValue::String(result_str));
                    }
                }

                Ok(AwkValue::Number(replacement_count as f64))
            }
            "int" => {
                if args.is_empty() {
                    return Err("int requires 1 argument".to_string());
                }
                let x = self.eval_expr(&args[0])?.to_number();
                Ok(AwkValue::Number(x.trunc()))
            }
            "sqrt" => {
                if args.is_empty() {
                    return Err("sqrt requires 1 argument".to_string());
                }
                let x = self.eval_expr(&args[0])?.to_number();
                Ok(AwkValue::Number(x.sqrt()))
            }
            "sin" | "cos" | "atan2" | "exp" | "log" | "rand" | "srand" => {
                // Math functions outside the 80/20 text-processing subset — declared
                // unsupported forever (a TEACH boundary; see docs/designing-syntax-with-llms.md). rand/srand also
                // conflict with the hermetic/deterministic stance.
                Err(format!(
                    "{name}() is not supported (kaish awk is a text-processing subset; only int and sqrt are provided)"
                ))
            }
            _ => Err(format!("unknown function: {}", name)),
        }
    }

    fn sprintf(&self, format: &str, args: &[AwkValue]) -> Result<String, String> {
        Ok(super::format_string::format_string(format, args))
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("data.txt"), b"alice 25\nbob 30\ncarol 35")
            .await
            .unwrap();
        mem.write(Path::new("passwd.txt"), b"root:x:0:0\nuser:x:1000:1000")
            .await
            .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    // === Lexer Tests ===

    #[test]
    fn test_lex_basic() {
        let mut lexer = AwkLexer::new("{print $1}");
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(tokens[0], Token::LBrace));
        assert!(matches!(tokens[1], Token::Print));
        assert!(matches!(tokens[2], Token::Dollar));
        assert!(matches!(tokens[3], Token::Number(n) if n == 1.0));
        assert!(matches!(tokens[4], Token::RBrace));
    }

    #[test]
    fn test_lex_regex() {
        let mut lexer = AwkLexer::new("/error/");
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(&tokens[0], Token::Regex(s) if s == "error"));
    }

    #[test]
    fn test_lex_string() {
        let mut lexer = AwkLexer::new("\"hello\\nworld\"");
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(&tokens[0], Token::String(s) if s == "hello\nworld"));
    }

    #[test]
    fn test_lex_operators() {
        let mut lexer = AwkLexer::new("++ -- += == != ~ !~");
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(tokens[0], Token::Increment));
        assert!(matches!(tokens[1], Token::Decrement));
        assert!(matches!(tokens[2], Token::AddAssign));
        assert!(matches!(tokens[3], Token::Eq));
        assert!(matches!(tokens[4], Token::Ne));
        assert!(matches!(tokens[5], Token::Match));
        assert!(matches!(tokens[6], Token::NotMatch));
    }

    // === Parser Tests ===

    #[test]
    fn test_parse_simple_print() {
        let prog = parse_program("{print}").unwrap();
        assert_eq!(prog.rules.len(), 1);
        assert!(matches!(prog.rules[0].pattern, Pattern::All));
    }

    #[test]
    fn test_parse_begin_end() {
        let prog = parse_program("BEGIN {x=1} {print} END {print x}").unwrap();
        assert_eq!(prog.rules.len(), 3);
        assert!(matches!(prog.rules[0].pattern, Pattern::Begin));
        assert!(matches!(prog.rules[1].pattern, Pattern::All));
        assert!(matches!(prog.rules[2].pattern, Pattern::End));
    }

    #[test]
    fn test_parse_regex_pattern() {
        let prog = parse_program("/error/ {print}").unwrap();
        assert!(matches!(&prog.rules[0].pattern, Pattern::Regex(r) if r == "error"));
    }

    #[test]
    fn test_parse_expr_pattern() {
        let prog = parse_program("$1 > 10 {print}").unwrap();
        assert!(matches!(prog.rules[0].pattern, Pattern::Expr(_)));
    }

    #[test]
    fn test_parse_if_else() {
        let prog = parse_program("{if ($1 > 0) print \"pos\"; else print \"neg\"}").unwrap();
        assert!(matches!(&prog.rules[0].action[0], Stmt::If(_, _, Some(_))));
    }

    #[test]
    fn test_parse_for_loop() {
        let prog = parse_program("{for (i=0; i<10; i++) print i}").unwrap();
        assert!(matches!(&prog.rules[0].action[0], Stmt::For(_, _, _, _)));
    }

    #[test]
    fn test_parse_for_in() {
        let prog = parse_program("{for (k in arr) print k}").unwrap();
        assert!(matches!(&prog.rules[0].action[0], Stmt::ForIn(_, _, _)));
    }

    // === Evaluator Tests ===

    #[test]
    fn test_eval_print_fields() {
        let prog = parse_program("{print $2, $1}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello world").unwrap().0;
        assert_eq!(result, "world hello\n");
    }

    #[test]
    fn test_eval_sum() {
        let prog = parse_program("{sum += $1} END {print sum}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "10\n20\n30").unwrap().0;
        assert_eq!(result, "60\n");
    }

    #[test]
    fn test_eval_field_separator() {
        let prog = parse_program("{print $1}").unwrap();
        let mut rt = AwkRuntime::new();
        rt.set_var("FS", AwkValue::String(":".to_string()));
        let result = rt.execute(&prog, "alice:25").unwrap().0;
        assert_eq!(result, "alice\n");
    }

    #[test]
    fn test_eval_regex_pattern() {
        let prog = parse_program("/world/ {print \"found\"}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello\nworld\nfoo").unwrap().0;
        assert_eq!(result, "found\n");
    }

    #[test]
    fn test_eval_comparison() {
        let prog = parse_program("$1 > 20 {print $1}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "10\n25\n30\n15").unwrap().0;
        assert_eq!(result, "25\n30\n");
    }

    #[test]
    fn test_eval_nr_nf() {
        let prog = parse_program("{print NR, NF}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "a b c\nd e").unwrap().0;
        assert_eq!(result, "1 3\n2 2\n");
    }

    #[test]
    fn test_eval_begin_end() {
        let prog = parse_program("BEGIN {print \"start\"} {print} END {print \"end\"}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "middle").unwrap().0;
        assert_eq!(result, "start\nmiddle\nend\n");
    }

    #[test]
    fn test_eval_if_else() {
        let prog = parse_program("{if ($1 > 0) print \"pos\"; else print \"neg\"}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "5\n-3").unwrap().0;
        assert_eq!(result, "pos\nneg\n");
    }

    #[test]
    fn test_eval_for_loop() {
        let prog = parse_program("BEGIN {for (i=1; i<=3; i++) print i}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "1\n2\n3\n");
    }

    #[test]
    fn test_eval_while_loop() {
        let prog = parse_program("BEGIN {i=1; while (i<=3) {print i; i++}}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "1\n2\n3\n");
    }

    #[test]
    fn test_eval_array() {
        let prog = parse_program("{a[$1]=$2} END {print a[\"bob\"]}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "alice 25\nbob 30").unwrap().0;
        assert_eq!(result, "30\n");
    }

    #[test]
    fn test_eval_length() {
        let prog = parse_program("{print length($1)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello").unwrap().0;
        assert_eq!(result, "5\n");
    }

    #[test]
    fn test_eval_substr() {
        let prog = parse_program("{print substr($1, 2, 3)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello").unwrap().0;
        assert_eq!(result, "ell\n");
    }

    #[test]
    fn test_eval_tolower_toupper() {
        let prog = parse_program("{print tolower($1), toupper($2)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "HELLO world").unwrap().0;
        assert_eq!(result, "hello WORLD\n");
    }

    #[test]
    fn test_eval_index() {
        let prog = parse_program("{print index($1, \"ll\")}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello").unwrap().0;
        assert_eq!(result, "3\n");
    }

    #[test]
    fn test_eval_sprintf() {
        let prog = parse_program("{print sprintf(\"%s is %d\", $1, $2)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "answer 42").unwrap().0;
        assert_eq!(result, "answer is 42\n");
    }

    #[test]
    fn test_eval_printf() {
        let prog = parse_program("{printf \"%s: %d\\n\", $1, $2}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "count 42").unwrap().0;
        assert_eq!(result, "count: 42\n");
    }

    #[test]
    fn test_eval_ternary() {
        let prog = parse_program("{print ($1 > 0 ? \"pos\" : \"neg\")}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "5\n-3").unwrap().0;
        assert_eq!(result, "pos\nneg\n");
    }

    #[test]
    fn test_eval_next() {
        let prog = parse_program("$1 == \"skip\" {next} {print}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "keep\nskip\nalso keep").unwrap().0;
        assert_eq!(result, "keep\nalso keep\n");
    }

    #[test]
    fn test_eval_exit() {
        let prog = parse_program("{if (NR == 2) exit; print}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "line1\nline2\nline3").unwrap().0;
        assert_eq!(result, "line1\n");
    }

    #[test]
    fn test_eval_match_operator() {
        let prog = parse_program("$1 ~ /^a/ {print $1}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "alice\nbob\nanna").unwrap().0;
        assert_eq!(result, "alice\nanna\n");
    }

    #[test]
    fn test_eval_not_match_operator() {
        let prog = parse_program("$1 !~ /^a/ {print $1}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "alice\nbob\nanna").unwrap().0;
        assert_eq!(result, "bob\n");
    }

    #[test]
    fn test_eval_logical_and_or() {
        let prog = parse_program("$1 > 0 && $1 < 10 {print \"single digit\"}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "5\n15\n-3").unwrap().0;
        assert_eq!(result, "single digit\n");
    }

    #[test]
    fn test_eval_concatenation() {
        let prog = parse_program("{print $1 $2}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello world").unwrap().0;
        assert_eq!(result, "helloworld\n");
    }

    #[test]
    fn test_eval_ofs() {
        let prog = parse_program("BEGIN {OFS=\",\"} {print $1, $2}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello world").unwrap().0;
        assert_eq!(result, "hello,world\n");
    }

    // === Real-World Pattern Tests ===
    // Based on actual `cargo test` output processing pipelines.

    #[test]
    fn test_eval_accumulate_single_field() {
        // Pattern: cargo test | grep "^test result:" | awk '{sum += $4} END {print "passed:", sum}'
        let prog = parse_program(r#"{sum += $4} END {print "passed:", sum}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let input = "\
test result: ok. 100 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
test result: ok. 50 passed; 1 failed; 2 ignored; 0 measured; 0 filtered out
test result: ok. 629 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out";
        let result = rt.execute(&prog, input).unwrap().0;
        assert_eq!(result, "passed: 779\n");
    }

    #[test]
    fn test_eval_accumulate_multiple_fields() {
        // Pattern: cargo test | grep "^test result:" | awk '{p+=$4; i+=$8} END {print "passed:", p, "ignored:", i, "total:", p+i}'
        let prog = parse_program(
            r#"{p += $4; i += $8} END {print "passed:", p, "ignored:", i, "total:", p+i}"#,
        )
        .unwrap();
        let mut rt = AwkRuntime::new();
        let input = "\
test result: ok. 100 passed; 0 failed; 3 ignored; 0 measured; 0 filtered out
test result: ok. 50 passed; 1 failed; 2 ignored; 0 measured; 0 filtered out
test result: ok. 629 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out";
        let result = rt.execute(&prog, input).unwrap().0;
        assert_eq!(result, "passed: 779 ignored: 5 total: 784\n");
    }

    #[test]
    fn test_eval_high_field_index() {
        // Accessing $10+ works correctly
        let prog = parse_program("{print $10}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "a b c d e f g h i j k l").unwrap().0;
        assert_eq!(result, "j\n");
    }

    #[test]
    fn test_eval_semicolon_separated_stmts() {
        // Multiple statements in one rule separated by ;
        let prog = parse_program("{a = $1; b = $2; print b, a}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello world").unwrap().0;
        assert_eq!(result, "world hello\n");
    }

    #[test]
    fn test_eval_end_arithmetic_expression() {
        // Arithmetic in END print arguments
        let prog =
            parse_program(r#"{a += $1; b += $2} END {print "sum:", a+b, "diff:", a-b}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "10 3\n20 7").unwrap().0;
        assert_eq!(result, "sum: 40 diff: 20\n");
    }

    // === Integration Tests ===

    #[tokio::test]
    async fn test_awk_from_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("10\n20\n30".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("{sum += $1} END {print sum}".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "60\n");
    }

    #[tokio::test]
    async fn test_awk_from_file() {
        let mut ctx = make_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("{print $1}".into()));
        args.positional.push(Value::String("/data.txt".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "alice\nbob\ncarol\n");
    }

    #[tokio::test]
    async fn test_awk_field_separator() {
        let mut ctx = make_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("{print $1}".into()));
        args.positional.push(Value::String("/passwd.txt".into()));
        args.named.insert("F".to_string(), Value::String(":".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "root\nuser\n");
    }

    #[tokio::test]
    async fn test_awk_with_condition() {
        let mut ctx = make_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("$2 > 26 {print $1}".into()));
        args.positional.push(Value::String("/data.txt".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "bob\ncarol\n");
    }

    #[tokio::test]
    async fn test_awk_missing_program() {
        let mut ctx = make_ctx().await;
        let args = ToolArgs::new();

        let result = Awk.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("missing program"));
    }

    #[tokio::test]
    async fn test_awk_invalid_program() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("{print".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_awk_unicode() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("こんにちは 世界".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("{print $2}".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "世界\n");
    }

    #[tokio::test]
    async fn test_awk_printf_width() {
        // Bug 7: awk printf "%10s" should right-align to 10 chars
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("{printf \"%10s|\\n\", $1}".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "     hello|\n");
    }

    #[tokio::test]
    async fn test_awk_printf_left_align() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hi".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("{printf \"%-10s|\", $1}".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "hi        |");
    }

    #[tokio::test]
    async fn test_awk_printf_zero_pad() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("42".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("{printf \"%06d\", $1}".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "000042");
    }

    // =========================================================================
    // P1 #2 — split() populates the array (Bug #2 fix tests)
    // =========================================================================

    /// split with comma separator: count, first, and last element are correct.
    /// gawk: `BEGIN{n=split("a,b,c",x,","); print n, x[1], x[3]}` → `3 a c`
    #[test]
    fn test_split_comma_separator() {
        let prog = parse_program(r#"BEGIN{n=split("a,b,c",x,","); print n, x[1], x[3]}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "3 a c\n");
    }

    /// split with default whitespace (no sep arg → uses FS=" "): trims ends, splits on runs.
    /// gawk: `BEGIN{n=split("  foo  bar  baz  ",x); print n, x[1], x[2]}` → `3 foo bar`
    #[test]
    fn test_split_whitespace_default() {
        let prog =
            parse_program(r#"BEGIN{n=split("  foo  bar  baz  ",x); print n, x[1], x[2]}"#)
                .unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "3 foo bar\n");
    }

    /// split with regex separator.
    /// gawk: `BEGIN{n=split("one12two34three",x,"[0-9]+"); print n, x[1], x[3]}` → `3 one three`
    #[test]
    fn test_split_regex_separator() {
        let prog =
            parse_program(r#"BEGIN{n=split("one12two34three",x,"[0-9]+"); print n, x[1], x[3]}"#)
                .unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "3 one three\n");
    }

    /// split CLEARS prior array contents before filling.
    /// gawk: prior key "99" must not survive; after split("a,b",x,",") only keys 1 and 2 exist.
    #[test]
    fn test_split_clears_prior_array() {
        let prog = parse_program(
            r#"BEGIN{x[99]="stale"; n=split("a,b",x,","); print (99 in x), x[1], x[2]}"#,
        )
        .unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        // (99 in x) must be 0 (false), x[1]="a", x[2]="b"
        assert_eq!(result, "0 a b\n");
    }

    /// split via kernel.execute (not the builtin's .execute()) — verifies dispatch chain.
    /// gawk: `BEGIN{n=split("a,b,c",x,","); print n, x[2]}` → `3 b`
    #[tokio::test]
    async fn test_split_kernel_dispatch() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String(r#"BEGIN{n=split("a,b,c",x,","); print n, x[2]}"#.into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok(), "awk failed: {}", result.err);
        assert_eq!(&*result.text_out(), "3 b\n");
    }

    // =========================================================================
    // P1 #3 — sub()/gsub() actually substitute (Bug #3 fix tests)
    // =========================================================================

    /// sub: first match only, returns 1 on match.
    /// gawk: `BEGIN{s="foo bar foo"; n=sub(/foo/,"baz",s); print n, s}` → `1 baz bar foo`
    #[test]
    fn test_sub_first_match_only() {
        let prog =
            parse_program(r#"BEGIN{s="foo bar foo"; n=sub(/foo/,"baz",s); print n, s}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "1 baz bar foo\n");
    }

    /// sub: no match returns 0 and leaves target unchanged.
    /// gawk: `BEGIN{s="hello"; n=sub(/xyz/,"A",s); print n, s}` → `0 hello`
    #[test]
    fn test_sub_no_match_returns_zero() {
        let prog =
            parse_program(r#"BEGIN{s="hello"; n=sub(/xyz/,"A",s); print n, s}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "0 hello\n");
    }

    /// gsub: returns the replacement count; replaces all occurrences.
    /// gawk: `{n=gsub(/o/,"0"); print n, $0}` over "foo" → `2 f00`
    #[test]
    fn test_gsub_returns_count_and_replaces_all() {
        let prog = parse_program(r#"{n=gsub(/o/,"0"); print n, $0}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "foo").unwrap().0;
        assert_eq!(result, "2 f00\n");
    }

    /// gsub: & in replacement expands to the matched text.
    /// gawk: `BEGIN{s="foo"; gsub(/o/,"[&]",s); print s}` → `f[o][o]`
    #[test]
    fn test_gsub_ampersand_in_replacement() {
        let prog = parse_program(r#"BEGIN{s="foo"; gsub(/o/,"[&]",s); print s}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "f[o][o]\n");
    }

    /// gsub: \& in replacement is a literal & (not the match).
    /// gawk: `BEGIN{s="foo"; gsub(/o/,"\\&",s); print s}` → `f&&`
    #[test]
    fn test_gsub_escaped_ampersand_is_literal() {
        let prog = parse_program(r#"BEGIN{s="foo"; gsub(/o/,"\\&",s); print s}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "f&&\n");
    }

    /// gsub into a named field rebuilds $0.
    /// gawk: `{gsub(/a/,"A",$1); print}` over "alice adam" → `Alice adam`
    /// (Only $1 is substituted; $0 is rebuilt with OFS.)
    #[test]
    fn test_gsub_into_named_field_rebuilds_record() {
        let prog = parse_program(r#"{gsub(/a/,"A",$1); print}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "alice adam").unwrap().0;
        // $1 = "alice" → "Alice" (only first field); $2 stays "adam"; rebuilt $0 = "Alice adam"
        assert_eq!(result, "Alice adam\n");
    }

    /// gsub defaults to $0 when no target arg is given, and modifies in place.
    /// gawk: `{gsub(/o/,"0"); print}` over "foo BAR baz" → `f00 BAR baz`
    #[test]
    fn test_gsub_default_target_is_dollar_zero() {
        let prog = parse_program(r#"{gsub(/o/,"0"); print}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "foo BAR baz").unwrap().0;
        assert_eq!(result, "f00 BAR baz\n");
    }

    /// gsub via kernel.execute (not the builtin's .execute()) — verifies dispatch chain.
    #[tokio::test]
    async fn test_gsub_kernel_dispatch() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("foo BAR baz".to_string());

        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String(r#"{n=gsub(/o/,"0"); print n, $0}"#.into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok(), "awk failed: {}", result.err);
        assert_eq!(&*result.text_out(), "2 f00 BAR baz\n");
    }

    // =========================================================================
    // Finding #1 — split("", a, sep) must return 0 and leave array empty.
    // =========================================================================

    /// split on an empty string returns 0 for ALL separator modes.
    /// gawk: `BEGIN{n=split("",a,","); print n, (1 in a)}` → `0 0`
    #[test]
    fn test_split_empty_string_comma_sep_returns_zero() {
        let prog =
            parse_program(r#"BEGIN{n=split("",a,","); print n, (1 in a)}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "0 0\n");
    }

    /// split on empty string with regex separator also returns 0.
    /// gawk: `BEGIN{n=split("",a,"[,;]"); print n, (1 in a)}` → `0 0`
    #[test]
    fn test_split_empty_string_regex_sep_returns_zero() {
        let prog =
            parse_program(r#"BEGIN{n=split("",a,"[,;]"); print n, (1 in a)}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "0 0\n");
    }

    /// split on empty string with whitespace FS returns 0.
    /// gawk: `BEGIN{n=split("",a); print n, (1 in a)}` → `0 0`
    #[test]
    fn test_split_empty_string_whitespace_fs_returns_zero() {
        let prog = parse_program(r#"BEGIN{n=split("",a); print n, (1 in a)}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "0 0\n");
    }

    // =========================================================================
    // Finding #2 — set_field must raise NF when assigning past it.
    // =========================================================================

    /// Assigning a high field extends NF.
    /// gawk: `echo "a b" | gawk '{$5="X"; print NF}'` → `5`
    #[test]
    fn test_set_field_extends_nf() {
        let prog = parse_program(r#"{$5="X"; print NF}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "a b").unwrap().0;
        assert_eq!(result, "5\n");
    }

    /// gsub on a field beyond current NF also raises NF (empty field gets substituted).
    /// gawk: `echo "a b" | gawk '{gsub(/./,"X",$5); print NF}'` → `5`
    #[test]
    fn test_gsub_on_high_field_extends_nf() {
        let prog = parse_program(r#"{gsub(/./,"X",$5); print NF}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "a b").unwrap().0;
        assert_eq!(result, "5\n");
    }

    /// Assigning within NF does not change NF.
    /// gawk: `echo "a b c" | gawk '{$2="B"; print NF}'` → `3`
    #[test]
    fn test_set_field_within_nf_unchanged() {
        let prog = parse_program(r#"{$2="B"; print NF}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "a b c").unwrap().0;
        assert_eq!(result, "3\n");
    }

    // =========================================================================
    // Finding #5 — edge tests for gsub/sub (locking known-good behaviour).
    // =========================================================================

    /// empty-pattern gsub inserts replacement before and after every character.
    /// gawk: `echo "abc" | gawk '{gsub(//,"-"); print}'` → `-a-b-c-`
    #[test]
    fn test_gsub_empty_pattern() {
        let prog = parse_program(r#"{gsub(//,"-"); print}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "abc").unwrap().0;
        assert_eq!(result, "-a-b-c-\n");
    }

    /// star-empty pattern: gsub(/x*/,"-","abc") must not infinite-loop; count = 4.
    /// gawk: `echo "abc" | gawk '{n=gsub(/x*/,"-"); print n, $0}'` → `4 -a-b-c-`
    #[test]
    fn test_gsub_star_empty_pattern_no_infinite_loop() {
        let prog = parse_program(r#"{n=gsub(/x*/,"-"); print n, $0}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "abc").unwrap().0;
        assert_eq!(result, "4 -a-b-c-\n");
    }

    /// `&&` in replacement expands to matched text twice.
    /// gawk: `echo "foo" | gawk '{sub(/o/,"&&"); print}'` → `fooo`
    #[test]
    fn test_sub_double_ampersand_in_replacement() {
        let prog = parse_program(r#"{sub(/o/,"&&"); print}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "foo").unwrap().0;
        assert_eq!(result, "fooo\n");
    }

    /// empty replacement deletes matched characters.
    /// gawk: `echo "food" | gawk '{gsub(/o/,""); print}'` → `fd`
    #[test]
    fn test_gsub_empty_replacement_deletes() {
        let prog = parse_program(r#"{gsub(/o/,""); print}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "food").unwrap().0;
        assert_eq!(result, "fd\n");
    }

    /// trailing lone backslash in replacement: kept as-is (matching gawk).
    /// gawk: `BEGIN{s="foo"; sub(/o/,"\\",s); print s}` → `f\o`
    #[test]
    fn test_sub_trailing_lone_backslash_in_replacement() {
        // AWK string "\\" is one backslash character — gawk keeps it.
        let prog = parse_program(r#"BEGIN{s="foo"; sub(/o/,"\\",s); print s}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "f\\o\n");
    }

    /// `\\&` in replacement: literal backslash followed by the matched text.
    /// gawk: `BEGIN{s="foo"; sub(/o/,"\\\\&",s); print s}` → `f\oo`
    /// (AWK string "\\\\&" is the two tokens: \\ (one backslash) + & (matched text).)
    #[test]
    fn test_sub_backslash_ampersand_replacement() {
        let prog = parse_program(r#"BEGIN{s="foo"; sub(/o/,"\\\\&",s); print s}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "f\\oo\n");
    }

    /// `\x` (backslash before non-special char) keeps the backslash, matching gawk.
    /// gawk: `BEGIN{s="foo"; sub(/o/,"\\x",s); print s}` → `f\xo`
    /// Note: the DeepSeek review claimed gawk drops the backslash, but gawk 5.4.0
    /// actually KEEPS it. This test locks the current (correct) behaviour.
    #[test]
    fn test_sub_backslash_other_char_keeps_backslash() {
        let prog = parse_program(r#"BEGIN{s="foo"; sub(/o/,"\\x",s); print s}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "f\\xo\n");
    }

    // =========================================================================
    // P2 #4 — match() sets RSTART and RLENGTH
    // =========================================================================

    /// match on a hit: RSTART = 1-based start, RLENGTH = char length of match.
    /// gawk: `BEGIN{match("foobar",/bar/); print RSTART, RLENGTH}` → `4 3`
    #[test]
    fn test_match_sets_rstart_rlength_on_hit() {
        let prog =
            parse_program(r#"BEGIN{match("foobar",/bar/); print RSTART, RLENGTH}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "4 3\n");
    }

    /// match on no hit: RSTART = 0, RLENGTH = -1 (gawk standard).
    /// gawk: `BEGIN{match("foobar",/xyz/); print RSTART, RLENGTH}` → `0 -1`
    #[test]
    fn test_match_sets_rstart_rlength_on_miss() {
        let prog =
            parse_program(r#"BEGIN{match("foobar",/xyz/); print RSTART, RLENGTH}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "0 -1\n");
    }

    /// match return value is the 1-based start position (or 0 on no match).
    /// gawk: `BEGIN{pos=match("foobar",/bar/); print pos}` → `4`
    #[test]
    fn test_match_return_value_is_rstart() {
        let prog = parse_program(r#"BEGIN{pos=match("foobar",/bar/); print pos}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "4\n");
    }

    /// substr($0, RSTART, RLENGTH) extraction idiom works after match().
    /// gawk: `BEGIN{s="hello world"; match(s,/wor/); print substr(s,RSTART,RLENGTH)}` → `wor`
    #[test]
    fn test_match_substr_extraction_idiom() {
        let prog = parse_program(
            r#"BEGIN{s="hello world"; match(s,/wor/); print substr(s,RSTART,RLENGTH)}"#,
        )
        .unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "wor\n");
    }

    /// match() via kernel.execute (not builtin .execute()) — verifies dispatch chain.
    #[tokio::test]
    async fn test_match_kernel_dispatch_rstart_rlength() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(
            r#"BEGIN{match("foobar",/bar/); print RSTART, RLENGTH}"#.into(),
        ));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok(), "awk failed: {}", result.err);
        assert_eq!(&*result.text_out(), "4 3\n");
    }

    // =========================================================================
    // P3 #7 — bare `length` (no parens) means length($0)
    // =========================================================================

    /// bare `length` (no parens) evaluates to the character length of $0.
    /// gawk: `echo "hello" | gawk '{print length}'` → `5`
    #[test]
    fn test_bare_length_is_length_dollar_zero() {
        let prog = parse_program("{print length}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello").unwrap().0;
        assert_eq!(result, "5\n");
    }

    /// bare `length` counts Unicode characters, not bytes.
    /// gawk: `echo "こんにちは" | gawk '{print length}'` → `5`
    #[test]
    fn test_bare_length_counts_unicode_chars() {
        let prog = parse_program("{print length}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "こんにちは").unwrap().0;
        assert_eq!(result, "5\n");
    }

    /// length($1) (explicit arg) still works — bare-length special-case must not break it.
    /// gawk: `echo "hello world" | gawk '{print length($1)}'` → `5`
    #[test]
    fn test_length_explicit_arg_still_works() {
        let prog = parse_program("{print length($1)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello world").unwrap().0;
        assert_eq!(result, "5\n");
    }

    /// length() with empty parens defaults to $0, just like length($0).
    /// gawk: `echo "hello" | gawk '{print length()}'` → `5`
    #[test]
    fn test_length_empty_parens_defaults_to_dollar_zero() {
        let prog = parse_program("{print length()}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello").unwrap().0;
        assert_eq!(result, "5\n");
    }

    /// bare `length` via kernel.execute.
    #[tokio::test]
    async fn test_bare_length_kernel_dispatch() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("{print length}".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok(), "awk failed: {}", result.err);
        assert_eq!(&*result.text_out(), "5\n");
    }

    // =========================================================================
    // P3 #8 — int() and sqrt() implemented; other math functions loud-error
    // =========================================================================

    /// int(3.9) truncates toward zero.
    /// gawk: `BEGIN{print int(3.9)}` → `3`
    #[test]
    fn test_int_positive_truncates_toward_zero() {
        let prog = parse_program("BEGIN{print int(3.9)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "3\n");
    }

    /// int(-3.9) truncates toward zero (not floor).
    /// gawk: `BEGIN{print int(-3.9)}` → `-3`
    #[test]
    fn test_int_negative_truncates_toward_zero() {
        let prog = parse_program("BEGIN{print int(-3.9)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "-3\n");
    }

    /// sqrt(2) matches gawk's output.
    /// gawk: `BEGIN{print sqrt(2)}` → `1.41421`
    #[test]
    fn test_sqrt_two() {
        let prog = parse_program("BEGIN{print sqrt(2)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "1.41421\n");
    }

    /// int() and sqrt() via kernel.execute.
    #[tokio::test]
    async fn test_int_sqrt_kernel_dispatch() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("BEGIN{print int(3.9), int(-3.9), sqrt(4)}".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok(), "awk failed: {}", result.err);
        assert_eq!(&*result.text_out(), "3 -3 2\n");
    }

    /// sin() returns a loud, honest error (not "use dedicated tool").
    #[tokio::test]
    async fn test_sin_returns_loud_unsupported_error() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("BEGIN{print sin(1)}".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(!result.ok(), "expected failure but got success");
        assert!(
            result.err.contains("not supported"),
            "expected 'not supported' in error, got: {}",
            result.err
        );
        assert!(
            !result.err.contains("dedicated tool"),
            "stale 'dedicated tool' message still present: {}",
            result.err
        );
    }

    /// cos() also loud-errors with the accurate subset message.
    #[test]
    fn test_cos_loud_error_message() {
        let prog = parse_program("BEGIN{print cos(0)}").unwrap();
        let mut rt = AwkRuntime::new();
        let err = rt.execute(&prog, "").unwrap_err();
        assert!(err.contains("not supported"), "got: {err}");
        assert!(err.contains("int and sqrt"), "got: {err}");
    }

    // =========================================================================
    // P3 #6 — range patterns `/start/,/end/`
    // =========================================================================

    /// Multi-record span: /bob/,/carol/ fires on bob, dave (between), and carol.
    /// gawk: echo "alice\nbob\ndave\ncarol\neve" | gawk '/bob/,/carol/'
    /// → bob\ndave\ncarol
    #[test]
    fn test_range_pattern_multi_record_span() {
        let prog = parse_program("/bob/,/carol/").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "alice\nbob\ndave\ncarol\neve").unwrap().0;
        assert_eq!(result, "bob\ndave\ncarol\n");
    }

    /// One-record range: start and end match the same record — only that record fires.
    /// gawk: echo "alice\nbobcarol\neve" | gawk '/bob/,/carol/'
    /// → bobcarol  (start and end matched same line → range closes immediately)
    #[test]
    fn test_range_pattern_one_record_range() {
        let prog = parse_program("/bob/,/carol/").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "alice\nbobcarol\neve").unwrap().0;
        assert_eq!(result, "bobcarol\n");
    }

    /// Range re-opens after closing: gawk fires on each span independently.
    /// Input: start1 ... end1 ... start2 ... end2
    /// gawk: echo "bob\ncarol\nbob\ncarol" | gawk '/bob/,/carol/'
    /// → bob\ncarol\nbob\ncarol
    #[test]
    fn test_range_pattern_re_triggers_after_close() {
        let prog = parse_program("/bob/,/carol/").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt
            .execute(&prog, "alice\nbob\ncarol\neve\nbob\nfoo\ncarol\ndone")
            .unwrap()
            .0;
        assert_eq!(result, "bob\ncarol\nbob\nfoo\ncarol\n");
    }

    /// Range with expression endpoints: NR==2,NR==4 fires on lines 2–4 inclusive.
    /// gawk: printf "a\nb\nc\nd\ne\n" | gawk 'NR==2,NR==4'
    /// → b\nc\nd
    ///
    /// Note: NR>=2,NR<=4 would re-open on line 5 (5>=2 fires again), matching gawk;
    /// NR==2,NR==4 uses exact equality so the range closes cleanly after line 4.
    #[test]
    fn test_range_pattern_expr_endpoints() {
        let prog = parse_program("NR==2,NR==4").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "a\nb\nc\nd\ne").unwrap().0;
        assert_eq!(result, "b\nc\nd\n");
    }

    /// Range pattern with action block.
    /// gawk: echo "alice\nbob\ndave\ncarol\neve" | gawk '/bob/,/carol/ {print ">" $0}'
    /// → >bob\n>dave\n>carol
    #[test]
    fn test_range_pattern_with_action() {
        let prog = parse_program("/bob/,/carol/ {print \">\" $0}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "alice\nbob\ndave\ncarol\neve").unwrap().0;
        assert_eq!(result, ">bob\n>dave\n>carol\n");
    }

    /// BEGIN/END rejected as range endpoints with a clear error.
    #[test]
    fn test_range_pattern_begin_endpoint_rejected() {
        let err = parse_program("BEGIN,/foo/").unwrap_err();
        assert!(
            err.contains("BEGIN") || err.contains("END"),
            "expected clear error about BEGIN/END, got: {err}"
        );
    }

    /// Range pattern via kernel.execute (dispatch chain).
    #[tokio::test]
    async fn test_range_pattern_kernel_dispatch() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("alice\nbob\ndave\ncarol\neve".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/bob/,/carol/".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok(), "awk failed: {}", result.err);
        assert_eq!(&*result.text_out(), "bob\ndave\ncarol\n");
    }

    // =========================================================================
    // P2 #5 — print redirection is a loud error, not a silent comparison
    // =========================================================================

    /// `{print $1 > "f"}` must fail with the redirection hint — NOT print `1` per line.
    #[tokio::test]
    async fn test_print_redirect_gt_is_loud_error() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello\nworld".to_string());

        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String(r#"{print $1 > "f"}"#.into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(!result.ok(), "expected failure, got success");
        assert!(
            result.err.contains("redirection") || result.err.contains("not supported"),
            "expected redirection error, got: {}",
            result.err
        );
        // Must NOT have printed `1` per line (the old silent mis-parse behaviour).
        assert!(
            !result.text_out().contains("1\n"),
            "old silent comparison behaviour still active: {}",
            result.text_out()
        );
    }

    /// `{print ($1 > 2)}` — parenthesised comparison inside print still works.
    /// gawk: echo "5" | gawk '{print ($1 > 2)}' → `1`
    #[test]
    fn test_print_parenthesised_comparison_still_works() {
        let prog = parse_program("{print ($1 > 2)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "5\n1").unwrap().0;
        // 5 > 2 → 1; 1 > 2 → 0
        assert_eq!(result, "1\n0\n");
    }

    /// `print $1, $2` (comma-separated) still works correctly.
    #[test]
    fn test_print_comma_sep_args_still_works() {
        let prog = parse_program("{print $1, $2}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello world").unwrap().0;
        assert_eq!(result, "hello world\n");
    }

    /// `print $1 $2` (concat, no comma) still works correctly.
    #[test]
    fn test_print_concat_no_comma_still_works() {
        let prog = parse_program("{print $1 $2}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello world").unwrap().0;
        assert_eq!(result, "helloworld\n");
    }

    /// Pattern comparison `$3 > 100 {print}` outside print is unaffected.
    #[test]
    fn test_pattern_comparison_gt_outside_print_unaffected() {
        let prog = parse_program("$3 > 100 {print $1}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "alice bob 50\nbob carol 200").unwrap().0;
        assert_eq!(result, "bob\n");
    }

    /// `{printf "%s\n", $1 > "f"}` also rejects redirection in printf.
    #[tokio::test]
    async fn test_printf_redirect_gt_is_loud_error() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello".to_string());

        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String(r#"{printf "%s\n", $1 > "f"}"#.into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(!result.ok(), "expected failure, got success");
        assert!(
            result.err.contains("redirection") || result.err.contains("not supported"),
            "expected redirection error, got: {}",
            result.err
        );
    }

    // =========================================================================
    // AwkValue Display — format_awk_number gawk-compatibility matrix
    // =========================================================================

    /// format_awk_number matches gawk's default OFMT (%.6g) for the full
    /// matrix required by the task, plus representative edge cases.
    #[test]
    #[allow(clippy::approx_constant)] // 3.14159265 is deliberately not PI; it is the test input
    fn test_format_awk_number_gawk_matrix() {
        #[rustfmt::skip]
        let cases: &[(f64, &str)] = &[
            // gawk: `BEGIN{print 1/3}` → `0.333333`
            (1.0 / 3.0,                  "0.333333"),
            // gawk: `BEGIN{print 2^10}` → `1024`  (integral)
            (1024.0,                     "1024"),
            // gawk: `BEGIN{print 3.14159265}` → `3.14159`
            (3.141_592_65,               "3.14159"),
            // gawk: `BEGIN{print 0.1+0.2}` → `0.3`  (rounds nicely at 6 sig figs)
            (0.1 + 0.2,                  "0.3"),
            // gawk: `BEGIN{print 100000000000000000}` → `100000000000000000`  (large integral)
            (100_000_000_000_000_000.0,  "100000000000000000"),
            // gawk: `BEGIN{print 1e20}` → `100000000000000000000`
            (1e20,                       "100000000000000000000"),
            // gawk: `BEGIN{print sqrt(2)}` → `1.41421`
            (2.0_f64.sqrt(),             "1.41421"),
            // gawk: `BEGIN{print -1/3}` → `-0.333333`
            (-1.0 / 3.0,                 "-0.333333"),
            // gawk: `BEGIN{print 0.0000001}` → `1e-07`
            (0.000_000_1,                "1e-07"),
            // gawk: `BEGIN{print 123456.789}` → `123457`  (rounds to 6 sig figs in fixed form)
            (123_456.789,                "123457"),
            // gawk: `BEGIN{print 0}` → `0`
            (0.0,                        "0"),
            // gawk: `BEGIN{print 1000000}` → `1000000`  (integral, 7 digits, not scientific)
            (1_000_000.0,                "1000000"),
            // Additional boundary cases
            // e=5, rounds up to e=6: scientific
            (999_999.5,                  "1e+06"),
            // e < -4: scientific
            (0.000_01,                   "1e-05"),
            (0.000_099,                  "9.9e-05"),
            // large non-integral: scientific
            (1_234_567.89,               "1.23457e+06"),
            // negative zero → "0"
            (-0.0_f64,                   "0"),
            // negative scientific
            (-0.000_000_1,               "-1e-07"),
        ];

        for &(n, expected) in cases {
            let got = super::format_awk_number(n);
            assert_eq!(got, expected, "format_awk_number({n}) = {got:?}, want {expected:?}");
        }
    }

    /// Kernel-routed tests for the three classes: large integral, sub-1 fraction, sqrt(2).
    /// These go through the full AwkRuntime.execute path so Display is exercised end-to-end.
    #[tokio::test]
    async fn test_numeric_display_kernel_large_integral_fraction_sqrt() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        // Large integral: must not truncate or switch to scientific
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("BEGIN{print 100000000000000000}".into()));
        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok(), "awk failed: {}", result.err);
        assert_eq!(&*result.text_out(), "100000000000000000\n");

        // Sub-1 fraction: must use scientific at e < -4
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("BEGIN{print 0.0000001}".into()));
        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok(), "awk failed: {}", result.err);
        assert_eq!(&*result.text_out(), "1e-07\n");

        // sqrt(2): 6 significant figures, fixed form
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("BEGIN{print sqrt(2)}".into()));
        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok(), "awk failed: {}", result.err);
        assert_eq!(&*result.text_out(), "1.41421\n");
    }

    // =========================================================================
    // Issue 2 — single `awk:` prefix on unsupported-math errors
    // =========================================================================

    /// The user-visible error for sin() must be `awk: sin() is not supported ...`
    /// (single prefix), not `awk: awk: sin() is not supported ...`.
    #[tokio::test]
    async fn test_sin_error_has_single_awk_prefix() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("BEGIN{print sin(1)}".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(!result.ok(), "expected failure");
        // Must start with exactly "awk: sin()" — not "awk: awk: sin()"
        assert!(
            result.err.starts_with("awk: sin()"),
            "expected single 'awk:' prefix, got: {:?}",
            result.err
        );
        assert!(
            !result.err.contains("awk: awk:"),
            "doubled prefix detected: {:?}",
            result.err
        );
    }

    // =========================================================================
    // P4 #9 — Grammar-boundary loud errors and the >= comparison fix
    // =========================================================================

    /// `print 1>=2` must evaluate to `0` (comparison), not error as redirection.
    /// gawk: `echo x | gawk '{print 1>=2}'` → `0`
    #[test]
    fn test_print_ge_is_comparison_not_redirect() {
        let prog = parse_program("{print 1>=2}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "x").unwrap().0;
        assert_eq!(result, "0\n");
    }

    /// `print 1 >= 2` with spaces also compares, not redirects.
    #[test]
    fn test_print_ge_spaced_is_comparison() {
        let prog = parse_program("{print 1 >= 2}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "x").unwrap().0;
        assert_eq!(result, "0\n");
    }

    /// `print 2>=1` → `1` (true comparison).
    #[test]
    fn test_print_ge_true_comparison() {
        let prog = parse_program("{print 2>=1}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "x").unwrap().0;
        assert_eq!(result, "1\n");
    }

    /// `print 1>2` is output redirection and must error loud.
    /// gawk redirects; kaish doesn't support redirection — must not silently compare.
    #[tokio::test]
    async fn test_print_bare_gt_is_loud_redirect_error() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("x".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(r#"{print 1>2}"#.into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(!result.ok(), "expected failure, got success");
        assert!(
            result.err.contains("redirection") || result.err.contains("not supported"),
            "expected redirection error, got: {}",
            result.err
        );
    }

    /// `print x>>"f"` (append redirection) must also error loud.
    #[tokio::test]
    async fn test_print_append_redirect_is_loud_error() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("x".to_string());

        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String(r#"{print x>>"f"}"#.into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(!result.ok(), "expected failure, got success");
        assert!(
            result.err.contains("redirection") || result.err.contains("not supported"),
            "expected redirection error, got: {}",
            result.err
        );
    }

    /// `print ($1>2)` with parentheses still compares (in_print reset inside parens).
    /// gawk: `echo "5" | gawk '{print ($1>2)}'` → `1`
    #[test]
    fn test_print_paren_gt_is_comparison() {
        let prog = parse_program("{print ($1>2)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "5").unwrap().0;
        assert_eq!(result, "1\n");
    }

    /// A `>` inside a function-call argument in print is a comparison, not
    /// redirection — the print context must reset across call args.
    /// gawk: `echo hello | gawk '{print substr($1,1,2>1)}'` → `h`
    #[test]
    fn test_print_call_arg_gt_is_comparison() {
        let prog = parse_program("{print substr($1, 1, 2>1)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello").unwrap().0;
        assert_eq!(result, "h\n");
    }

    /// A `>` inside a (parenthesized) ternary branch in print is a comparison.
    #[test]
    fn test_print_paren_ternary_gt_is_comparison() {
        let prog = parse_program("{print (1 ? 3>2 : 0)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "x").unwrap().0;
        assert_eq!(result, "1\n");
    }

    /// Two independent range patterns in one program keep separate active state.
    /// gawk: `printf '1\n2\n3\n4\n5\n' | gawk '/1/,/2/{print} /3/,/4/{print}'` → 1 2 3 4
    #[test]
    fn test_two_independent_range_patterns() {
        let prog = parse_program("/1/,/2/{print} /3/,/4/{print}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "1\n2\n3\n4\n5\n").unwrap().0;
        assert_eq!(result, "1\n2\n3\n4\n");
    }

    /// A range whose end never matches stays open through EOF.
    /// gawk: `printf '1\n2\n3\n4\n5\n' | gawk '/2/,/zzz/'` → 2 3 4 5
    #[test]
    fn test_range_end_never_matches_runs_to_eof() {
        let prog = parse_program("/2/,/zzz/").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "1\n2\n3\n4\n5\n").unwrap().0;
        assert_eq!(result, "2\n3\n4\n5\n");
    }

    /// A regex pattern starting a new rule after a `}` lexes as a regex, not
    /// division. `/1/{print} /3/{print}` over 1..5 → `1`, `3`.
    #[test]
    fn test_multiple_regex_rules_lex_after_brace() {
        let prog = parse_program("/1/{print} /3/{print}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "1\n2\n3\n4\n5\n").unwrap().0;
        assert_eq!(result, "1\n3\n");
    }

    /// Division (`/` after a value) must still lex as division, not regex.
    #[test]
    fn test_division_still_lexes_after_value() {
        let prog = parse_program("BEGIN{x=10; print x/2/1}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "5\n");
    }

    /// Division inside a for-header (after `;`) still lexes as division — the
    /// `;`-enters-regex-context fix must not break `i < n/2`.
    #[test]
    fn test_division_in_for_header() {
        let prog = parse_program("BEGIN{ for(i=0; i<10/2; i++) s=s i; print s }").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "01234\n");
    }

    /// A regex expression after `;` lexes as a regex (matches $0), not division.
    #[test]
    fn test_regex_after_semicolon() {
        let prog = parse_program("{ x=1; if (/3/) print \"hit\" }").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "3\n4\n").unwrap().0;
        assert_eq!(result, "hit\n");
    }

    /// A range whose start and end match the same record fires for one record.
    /// gawk: `printf '1\n2\n3\n4\n5\n' | gawk '/3/,/3/'` → 3
    #[test]
    fn test_range_one_record_when_start_equals_end() {
        let prog = parse_program("/3/,/3/").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "1\n2\n3\n4\n5\n").unwrap().0;
        assert_eq!(result, "3\n");
    }

    /// bare `getline` → loud, hinted error.
    #[tokio::test]
    async fn test_getline_bare_is_loud_error() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("x".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("{getline}".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(!result.ok(), "expected failure, got success");
        assert!(
            result.err.contains("getline") && result.err.contains("not supported"),
            "expected getline error, got: {}",
            result.err
        );
    }

    /// `getline x` (getline into a variable) → loud, hinted error.
    #[tokio::test]
    async fn test_getline_into_var_is_loud_error() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("x".to_string());

        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("{getline x}".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(!result.ok(), "expected failure, got success");
        assert!(
            result.err.contains("getline") && result.err.contains("not supported"),
            "expected getline error, got: {}",
            result.err
        );
    }

    /// `function f(){return 1}` → loud error about user-defined functions.
    #[tokio::test]
    async fn test_user_function_is_loud_error() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("function f(){return 1}".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(!result.ok(), "expected failure, got success");
        assert!(
            result.err.contains("user-defined functions") || result.err.contains("not supported"),
            "expected function error, got: {}",
            result.err
        );
    }

    /// `return` inside a block also errors about user-defined functions.
    #[tokio::test]
    async fn test_return_in_block_is_loud_error() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("x".to_string());

        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("{return 1}".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(!result.ok(), "expected failure, got success");
        assert!(
            result.err.contains("user-defined functions") || result.err.contains("not supported"),
            "expected function error, got: {}",
            result.err
        );
    }

    /// `a[1,2]=3` (multi-dimensional subscript) → loud, hinted error.
    #[tokio::test]
    async fn test_multi_dim_subscript_is_loud_error() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("BEGIN{a[1,2]=3; print a[1,2]}".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(!result.ok(), "expected failure, got success");
        assert!(
            result.err.contains("multi-dimensional") || result.err.contains("a[i,j]"),
            "expected multi-dim error, got: {}",
            result.err
        );
    }

    // =========================================================================
    // Non-finite number formatting — matches gawk 5.4.0 exact spelling
    // =========================================================================

    /// sqrt(-1) formats as `-nan`, matching gawk 5.4.0.
    /// gawk: `BEGIN{print sqrt(-1)}` (stderr warning suppressed) → `-nan`
    #[test]
    fn test_sqrt_negative_formats_as_nan() {
        let got = super::format_awk_number(f64::NAN);
        assert_eq!(got, "-nan", "NaN must format as '-nan', got: {got:?}");
    }

    /// Positive infinity formats as `+inf`, matching gawk 5.4.0.
    /// gawk: `BEGIN{print 2^1024}` → `+inf`
    #[test]
    fn test_positive_infinity_formats_as_plus_inf() {
        let got = super::format_awk_number(f64::INFINITY);
        assert_eq!(got, "+inf", "pos inf must format as '+inf', got: {got:?}");
    }

    /// Negative infinity formats as `-inf`, matching gawk 5.4.0.
    /// gawk: `BEGIN{print -2^1024}` → `-inf`
    #[test]
    fn test_negative_infinity_formats_as_minus_inf() {
        let got = super::format_awk_number(f64::NEG_INFINITY);
        assert_eq!(got, "-inf", "neg inf must format as '-inf', got: {got:?}");
    }

    /// sqrt(-1) via AwkRuntime end-to-end: the output string matches gawk.
    #[test]
    fn test_sqrt_negative_runtime_output() {
        let prog = parse_program("BEGIN{print sqrt(-1)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "-nan\n", "sqrt(-1) runtime output must be '-nan\\n'");
    }

    /// 2^1024 via AwkRuntime end-to-end: prints `+inf` like gawk.
    #[test]
    fn test_pow_overflow_runtime_output() {
        let prog = parse_program("BEGIN{print 2^1024}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap().0;
        assert_eq!(result, "+inf\n", "2^1024 must print '+inf\\n'");
    }
}
