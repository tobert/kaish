//! awk — Pattern scanning and text processing language.
//!
//! A Bourne-lite awk implementation focused on the 80% use case.
//! Uses ERE (extended regex) syntax like egrep, consistent with sed.

use async_trait::async_trait;
use regex::Regex;
use std::collections::HashMap;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Awk tool: pattern-directed scanning and processing.
pub struct Awk;

#[async_trait]
impl Tool for Awk {
    fn name(&self) -> &str {
        "awk"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("awk", "Pattern scanning and text processing language")
            .param(ParamSchema::required(
                "program",
                "string",
                "AWK program to execute",
            ))
            .param(ParamSchema::optional(
                "path",
                "string",
                Value::Null,
                "File to process (reads stdin if not provided)",
            ))
            .param(ParamSchema::optional(
                "field_separator",
                "string",
                Value::Null,
                "Field separator regex (-F)",
            ))
            .param(ParamSchema::optional(
                "var",
                "string",
                Value::Null,
                "Variable assignment (-v name=value)",
            ))
            .example("Print second field", "awk '{print $2}' file.txt")
            .example("Sum first column", "awk '{sum += $1} END {print sum}' data.txt")
            .example("Filter by pattern", "awk '/error/ {print $0}' log.txt")
            .example("Custom separator", "awk -F: '{print $1}' /etc/passwd")
            .example("Conditional", "awk '$3 > 100 {print $1, $3}' data.txt")
            .example("BEGIN/END", "awk 'BEGIN {print \"Header\"} {print} END {print \"Footer\"}'")
            .example("Field count", "awk '{print NF, $0}' file.txt")
            .example("Line numbers", "awk '{print NR, $0}' file.txt")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
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

        // Get field separator
        let field_sep = args
            .get_string("field_separator", usize::MAX)
            .or_else(|| args.get_string("F", usize::MAX));

        // Get input
        let file_pos = 1;
        let input = match args.get_string("path", file_pos) {
            Some(path) => {
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
            None => ctx.take_stdin().unwrap_or_default(),
        };

        // Build runtime with initial variables
        let mut runtime = AwkRuntime::new();
        if let Some(fs) = field_sep {
            runtime.set_var("FS", AwkValue::String(fs));
        }

        // Handle -v assignments
        if let Some(var_assign) = args.get_string("var", usize::MAX)
            && let Some((name, value)) = var_assign.split_once('=')
        {
            runtime.set_var(name.trim(), AwkValue::String(value.to_string()));
        }
        if let Some(var_assign) = args.get_string("v", usize::MAX)
            && let Some((name, value)) = var_assign.split_once('=')
        {
            runtime.set_var(name.trim(), AwkValue::String(value.to_string()));
        }

        // Execute
        match runtime.execute(&ast, &input) {
            Ok(output) => ExecResult::with_output(OutputData::text(output)),
            Err(e) => ExecResult::failure(1, format!("awk: {}", e)),
        }
    }
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

        // Newline is significant in AWK
        if c == '\n' {
            self.advance();
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
        self.in_regex_context = matches!(tok, Token::Match | Token::NotMatch);
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
            ';' => Token::Semicolon,
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
                self.in_regex_context = false;
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
}

impl AwkParser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
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

    // pattern := BEGIN | END | /regex/ | expr | (empty)
    fn parse_pattern(&mut self) -> Result<Pattern, String> {
        match self.peek() {
            Token::Begin => {
                self.advance();
                Ok(Pattern::Begin)
            }
            Token::End => {
                self.advance();
                Ok(Pattern::End)
            }
            Token::Regex(re) => {
                let re = re.clone();
                self.advance();
                Ok(Pattern::Regex(re))
            }
            Token::LBrace => Ok(Pattern::All),
            Token::Eof | Token::Newline => Ok(Pattern::All),
            _ => {
                let expr = self.parse_expr()?;
                Ok(Pattern::Expr(expr))
            }
        }
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
            args.push(self.parse_expr()?);
            while self.peek() == &Token::Comma {
                self.advance();
                args.push(self.parse_expr()?);
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
            args.push(self.parse_expr()?);
            while self.peek() == &Token::Comma {
                self.advance();
                args.push(self.parse_expr()?);
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
            let then_expr = self.parse_expr()?;
            self.expect(&Token::Colon)?;
            let else_expr = self.parse_expr()?;
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
                self.advance();
                // Check for function call
                if self.peek() == &Token::LParen {
                    self.advance();
                    let mut args = Vec::new();
                    if self.peek() != &Token::RParen {
                        args.push(self.parse_expr()?);
                        while self.peek() == &Token::Comma {
                            self.advance();
                            args.push(self.parse_expr()?);
                        }
                    }
                    self.expect(&Token::RParen)?;
                    Ok(Expr::Call(name, args))
                } else {
                    Ok(Expr::Var(name))
                }
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(&Token::RParen)?;
                Ok(expr)
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

/// AWK value with string/number coercion.
#[derive(Debug, Clone, Default)]
enum AwkValue {
    String(String),
    Number(f64),
    #[default]
    Uninitialized,
}

impl std::fmt::Display for AwkValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AwkValue::String(s) => f.write_str(s),
            AwkValue::Number(n) => {
                if n.fract() == 0.0 && n.abs() < 1e15 {
                    write!(f, "{}", *n as i64)
                } else {
                    let s = format!("{:.6}", n);
                    f.write_str(s.trim_end_matches('0').trim_end_matches('.'))
                }
            }
            AwkValue::Uninitialized => Ok(()),
        }
    }
}

impl AwkValue {

    fn to_number(&self) -> f64 {
        match self {
            AwkValue::Number(n) => *n,
            AwkValue::String(s) => parse_awk_number(s),
            AwkValue::Uninitialized => 0.0,
        }
    }

    fn to_bool(&self) -> bool {
        match self {
            AwkValue::Number(n) => *n != 0.0,
            AwkValue::String(s) => !s.is_empty(),
            AwkValue::Uninitialized => false,
        }
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
    Exit(i32),
}

/// Runtime state for AWK execution.
struct AwkRuntime {
    vars: HashMap<String, AwkValue>,
    arrays: HashMap<String, HashMap<String, AwkValue>>,
    fields: Vec<AwkValue>,
    output: String,
    nr: i64, // Current record number
    nf: i64, // Number of fields in current record
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
        }
    }

    fn set_var(&mut self, name: &str, value: AwkValue) {
        self.vars.insert(name.to_string(), value);
    }

    fn get_var(&self, name: &str) -> AwkValue {
        self.vars.get(name).cloned().unwrap_or(AwkValue::Uninitialized)
    }

    fn get_field(&self, n: i64) -> AwkValue {
        if n < 0 {
            return AwkValue::Uninitialized;
        }
        let idx = n as usize;
        self.fields.get(idx).cloned().unwrap_or(AwkValue::Uninitialized)
    }

    fn set_field(&mut self, n: i64, value: AwkValue) {
        if n < 0 {
            return;
        }
        let idx = n as usize;
        if idx >= self.fields.len() {
            self.fields.resize(idx + 1, AwkValue::Uninitialized);
        }
        self.fields[idx] = value;

        // Rebuild $0 if setting a field
        if n > 0 {
            self.rebuild_record();
        }
    }

    fn rebuild_record(&mut self) {
        let ofs = self.get_var("OFS").to_string();
        let parts: Vec<String> = self.fields[1..].iter().map(|f| f.to_string()).collect();
        self.fields[0] = AwkValue::String(parts.join(&ofs));
    }

    fn split_record(&mut self, record: &str) {
        let fs = self.get_var("FS").to_string();
        self.fields = vec![AwkValue::String(record.to_string())];

        let parts: Vec<&str> = if fs == " " {
            // Default: split on whitespace, trim leading/trailing
            record.split_whitespace().collect()
        } else if fs.len() == 1 {
            record.split(&fs).collect()
        } else {
            // FS is a regex
            match Regex::new(&fs) {
                Ok(re) => re.split(record).collect(),
                Err(_) => record.split(&fs).collect(),
            }
        };

        for part in &parts {
            self.fields.push(AwkValue::String(part.to_string()));
        }

        self.nf = (self.fields.len() - 1) as i64;
        self.set_var("NF", AwkValue::Number(self.nf as f64));
    }

    fn execute(&mut self, program: &AwkProgram, input: &str) -> Result<String, String> {
        // Run BEGIN rules
        for rule in &program.rules {
            if matches!(rule.pattern, Pattern::Begin) && let ControlFlow::Exit(_) = self.execute_block(&rule.action)? {
                return Ok(std::mem::take(&mut self.output));
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
            self.split_record(record);

            for rule in &program.rules {
                if matches!(rule.pattern, Pattern::Begin | Pattern::End) {
                    continue;
                }

                if self.pattern_matches(&rule.pattern)? {
                    match self.execute_block(&rule.action)? {
                        ControlFlow::Next => continue 'records,
                        ControlFlow::Exit(_) => break 'records,
                        ControlFlow::Break | ControlFlow::Continue => {
                            return Err("break/continue outside loop".to_string())
                        }
                        ControlFlow::Normal => {}
                    }
                }
            }
        }

        // Run END rules
        for rule in &program.rules {
            if matches!(rule.pattern, Pattern::End) && let ControlFlow::Exit(_) = self.execute_block(&rule.action)? {
                break;
            }
        }

        Ok(std::mem::take(&mut self.output))
    }

    fn pattern_matches(&mut self, pattern: &Pattern) -> Result<bool, String> {
        match pattern {
            Pattern::All => Ok(true),
            Pattern::Begin | Pattern::End => Ok(false),
            Pattern::Regex(re) => {
                let regex = Regex::new(re).map_err(|e| format!("invalid regex: {}", e))?;
                Ok(regex.is_match(&self.get_field(0).to_string()))
            }
            Pattern::Expr(expr) => {
                let val = self.eval_expr(expr)?;
                Ok(val.to_bool())
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
                    self.set_var(var, AwkValue::String(key));
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
                let exit_code = code
                    .as_ref()
                    .map(|e| self.eval_expr(e).map(|v| v.to_number() as i32))
                    .transpose()?
                    .unwrap_or(0);
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
                self.set_field(n, value);
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
                let regex = Regex::new(re).map_err(|e| format!("invalid regex: {}", e))?;
                let matched = regex.is_match(&self.get_field(0).to_string());
                Ok(AwkValue::Number(if matched { 1.0 } else { 0.0 }))
            }
            Expr::Var(name) => Ok(self.get_var(name)),
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
                let regex = Regex::new(&pattern).map_err(|e| format!("invalid regex: {}", e))?;
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
        // AWK comparison: if either operand is numeric (or a numeric string),
        // compare numerically. Otherwise compare as strings.
        let l_num = Self::looks_numeric(l);
        let r_num = Self::looks_numeric(r);

        if l_num || r_num {
            // At least one is numeric, compare as numbers
            l.to_number()
                .partial_cmp(&r.to_number())
                .unwrap_or(std::cmp::Ordering::Equal)
        } else {
            // Both are non-numeric strings, compare lexicographically
            l.to_string().cmp(&r.to_string())
        }
    }

    fn looks_numeric(val: &AwkValue) -> bool {
        match val {
            AwkValue::Number(_) => true,
            AwkValue::String(s) => {
                let s = s.trim();
                !s.is_empty() && s.parse::<f64>().is_ok()
            }
            AwkValue::Uninitialized => true, // 0
        }
    }

    fn call_function(&mut self, name: &str, args: &[Expr]) -> Result<AwkValue, String> {
        match name {
            "length" => {
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
                // Validate that second arg is an array variable
                let _arr_name = match &args[1] {
                    Expr::Var(name) => name.clone(),
                    _ => return Err("split: second argument must be an array".to_string()),
                };
                let sep = if args.len() > 2 {
                    self.eval_expr(&args[2])?.to_string()
                } else {
                    self.get_var("FS").to_string()
                };

                let parts: Vec<&str> = if sep == " " {
                    s.split_whitespace().collect()
                } else if sep.len() == 1 {
                    s.split(&sep).collect()
                } else {
                    match Regex::new(&sep) {
                        Ok(re) => re.split(&s).collect(),
                        Err(_) => s.split(&sep).collect(),
                    }
                };

                // Note: split() should populate the array, but that requires mutable
                // access. For now, we just return the count. To fully support split(),
                // it would need to be handled as a special statement, not a function.
                Ok(AwkValue::Number(parts.len() as f64))
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
                let pattern = self.eval_expr(&args[1])?.to_string();
                let regex = Regex::new(&pattern).map_err(|e| format!("invalid regex: {}", e))?;
                let pos = regex.find(&s).map(|m| {
                    s[..m.start()].chars().count() + 1
                }).unwrap_or(0);
                Ok(AwkValue::Number(pos as f64))
            }
            "sub" | "gsub" => {
                // These need mutable access; for now, return 0
                // Full implementation would need to modify the target
                Ok(AwkValue::Number(0.0))
            }
            "sin" | "cos" | "atan2" | "exp" | "log" | "sqrt" | "int" | "rand" | "srand" => {
                // Numeric functions not in the 80%
                Err(format!("function '{}' not implemented (use dedicated tool)", name))
            }
            _ => Err(format!("unknown function: {}", name)),
        }
    }

    fn sprintf(&self, format: &str, args: &[AwkValue]) -> Result<String, String> {
        let mut output = String::new();
        let mut arg_index = 0;
        let mut chars = format.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '%' {
                match chars.next() {
                    Some('%') => output.push('%'),
                    Some('s') => {
                        let val = args.get(arg_index).map(|v| v.to_string()).unwrap_or_default();
                        output.push_str(&val);
                        arg_index += 1;
                    }
                    Some('d') | Some('i') => {
                        let val = args.get(arg_index).map(|v| v.to_number() as i64).unwrap_or(0);
                        output.push_str(&val.to_string());
                        arg_index += 1;
                    }
                    Some('f') | Some('g') | Some('e') => {
                        let val = args.get(arg_index).map(|v| v.to_number()).unwrap_or(0.0);
                        output.push_str(&format!("{:.6}", val));
                        arg_index += 1;
                    }
                    Some('x') => {
                        let val = args.get(arg_index).map(|v| v.to_number() as i64).unwrap_or(0);
                        output.push_str(&format!("{:x}", val));
                        arg_index += 1;
                    }
                    Some('X') => {
                        let val = args.get(arg_index).map(|v| v.to_number() as i64).unwrap_or(0);
                        output.push_str(&format!("{:X}", val));
                        arg_index += 1;
                    }
                    Some('o') => {
                        let val = args.get(arg_index).map(|v| v.to_number() as i64).unwrap_or(0);
                        output.push_str(&format!("{:o}", val));
                        arg_index += 1;
                    }
                    Some('c') => {
                        let val = args.get(arg_index).and_then(|v| {
                            let n = v.to_number() as u32;
                            char::from_u32(n)
                        });
                        if let Some(ch) = val {
                            output.push(ch);
                        }
                        arg_index += 1;
                    }
                    Some(ch) => {
                        output.push('%');
                        output.push(ch);
                    }
                    None => output.push('%'),
                }
            } else if c == '\\' {
                match chars.next() {
                    Some('n') => output.push('\n'),
                    Some('t') => output.push('\t'),
                    Some('r') => output.push('\r'),
                    Some('\\') => output.push('\\'),
                    Some(ch) => {
                        output.push('\\');
                        output.push(ch);
                    }
                    None => output.push('\\'),
                }
            } else {
                output.push(c);
            }
        }

        Ok(output)
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
        let result = rt.execute(&prog, "hello world").unwrap();
        assert_eq!(result, "world hello\n");
    }

    #[test]
    fn test_eval_sum() {
        let prog = parse_program("{sum += $1} END {print sum}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "10\n20\n30").unwrap();
        assert_eq!(result, "60\n");
    }

    #[test]
    fn test_eval_field_separator() {
        let prog = parse_program("{print $1}").unwrap();
        let mut rt = AwkRuntime::new();
        rt.set_var("FS", AwkValue::String(":".to_string()));
        let result = rt.execute(&prog, "alice:25").unwrap();
        assert_eq!(result, "alice\n");
    }

    #[test]
    fn test_eval_regex_pattern() {
        let prog = parse_program("/world/ {print \"found\"}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello\nworld\nfoo").unwrap();
        assert_eq!(result, "found\n");
    }

    #[test]
    fn test_eval_comparison() {
        let prog = parse_program("$1 > 20 {print $1}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "10\n25\n30\n15").unwrap();
        assert_eq!(result, "25\n30\n");
    }

    #[test]
    fn test_eval_nr_nf() {
        let prog = parse_program("{print NR, NF}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "a b c\nd e").unwrap();
        assert_eq!(result, "1 3\n2 2\n");
    }

    #[test]
    fn test_eval_begin_end() {
        let prog = parse_program("BEGIN {print \"start\"} {print} END {print \"end\"}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "middle").unwrap();
        assert_eq!(result, "start\nmiddle\nend\n");
    }

    #[test]
    fn test_eval_if_else() {
        let prog = parse_program("{if ($1 > 0) print \"pos\"; else print \"neg\"}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "5\n-3").unwrap();
        assert_eq!(result, "pos\nneg\n");
    }

    #[test]
    fn test_eval_for_loop() {
        let prog = parse_program("BEGIN {for (i=1; i<=3; i++) print i}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap();
        assert_eq!(result, "1\n2\n3\n");
    }

    #[test]
    fn test_eval_while_loop() {
        let prog = parse_program("BEGIN {i=1; while (i<=3) {print i; i++}}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "").unwrap();
        assert_eq!(result, "1\n2\n3\n");
    }

    #[test]
    fn test_eval_array() {
        let prog = parse_program("{a[$1]=$2} END {print a[\"bob\"]}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "alice 25\nbob 30").unwrap();
        assert_eq!(result, "30\n");
    }

    #[test]
    fn test_eval_length() {
        let prog = parse_program("{print length($1)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello").unwrap();
        assert_eq!(result, "5\n");
    }

    #[test]
    fn test_eval_substr() {
        let prog = parse_program("{print substr($1, 2, 3)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello").unwrap();
        assert_eq!(result, "ell\n");
    }

    #[test]
    fn test_eval_tolower_toupper() {
        let prog = parse_program("{print tolower($1), toupper($2)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "HELLO world").unwrap();
        assert_eq!(result, "hello WORLD\n");
    }

    #[test]
    fn test_eval_index() {
        let prog = parse_program("{print index($1, \"ll\")}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello").unwrap();
        assert_eq!(result, "3\n");
    }

    #[test]
    fn test_eval_sprintf() {
        let prog = parse_program("{print sprintf(\"%s is %d\", $1, $2)}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "answer 42").unwrap();
        assert_eq!(result, "answer is 42\n");
    }

    #[test]
    fn test_eval_printf() {
        let prog = parse_program("{printf \"%s: %d\\n\", $1, $2}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "count 42").unwrap();
        assert_eq!(result, "count: 42\n");
    }

    #[test]
    fn test_eval_ternary() {
        let prog = parse_program("{print ($1 > 0 ? \"pos\" : \"neg\")}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "5\n-3").unwrap();
        assert_eq!(result, "pos\nneg\n");
    }

    #[test]
    fn test_eval_next() {
        let prog = parse_program("$1 == \"skip\" {next} {print}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "keep\nskip\nalso keep").unwrap();
        assert_eq!(result, "keep\nalso keep\n");
    }

    #[test]
    fn test_eval_exit() {
        let prog = parse_program("{if (NR == 2) exit; print}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "line1\nline2\nline3").unwrap();
        assert_eq!(result, "line1\n");
    }

    #[test]
    fn test_eval_match_operator() {
        let prog = parse_program("$1 ~ /^a/ {print $1}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "alice\nbob\nanna").unwrap();
        assert_eq!(result, "alice\nanna\n");
    }

    #[test]
    fn test_eval_not_match_operator() {
        let prog = parse_program("$1 !~ /^a/ {print $1}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "alice\nbob\nanna").unwrap();
        assert_eq!(result, "bob\n");
    }

    #[test]
    fn test_eval_logical_and_or() {
        let prog = parse_program("$1 > 0 && $1 < 10 {print \"single digit\"}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "5\n15\n-3").unwrap();
        assert_eq!(result, "single digit\n");
    }

    #[test]
    fn test_eval_concatenation() {
        let prog = parse_program("{print $1 $2}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello world").unwrap();
        assert_eq!(result, "helloworld\n");
    }

    #[test]
    fn test_eval_ofs() {
        let prog = parse_program("BEGIN {OFS=\",\"} {print $1, $2}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello world").unwrap();
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
        let result = rt.execute(&prog, input).unwrap();
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
        let result = rt.execute(&prog, input).unwrap();
        assert_eq!(result, "passed: 779 ignored: 5 total: 784\n");
    }

    #[test]
    fn test_eval_high_field_index() {
        // Accessing $10+ works correctly
        let prog = parse_program("{print $10}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "a b c d e f g h i j k l").unwrap();
        assert_eq!(result, "j\n");
    }

    #[test]
    fn test_eval_semicolon_separated_stmts() {
        // Multiple statements in one rule separated by ;
        let prog = parse_program("{a = $1; b = $2; print b, a}").unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "hello world").unwrap();
        assert_eq!(result, "world hello\n");
    }

    #[test]
    fn test_eval_end_arithmetic_expression() {
        // Arithmetic in END print arguments
        let prog =
            parse_program(r#"{a += $1; b += $2} END {print "sum:", a+b, "diff:", a-b}"#).unwrap();
        let mut rt = AwkRuntime::new();
        let result = rt.execute(&prog, "10 3\n20 7").unwrap();
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
        assert_eq!(result.out, "60\n");
    }

    #[tokio::test]
    async fn test_awk_from_file() {
        let mut ctx = make_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("{print $1}".into()));
        args.positional.push(Value::String("/data.txt".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "alice\nbob\ncarol\n");
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
        assert_eq!(result.out, "root\nuser\n");
    }

    #[tokio::test]
    async fn test_awk_with_condition() {
        let mut ctx = make_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("$2 > 26 {print $1}".into()));
        args.positional.push(Value::String("/data.txt".into()));

        let result = Awk.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "bob\ncarol\n");
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
        assert_eq!(result.out, "世界\n");
    }
}
