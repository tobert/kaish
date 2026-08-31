//! `$(( ))` — checked 64-bit integer arithmetic and another-base number
//! reading.
//!
//! Three stages, kept separate so each can be tested on its own:
//! `tokenize` (text → `Tok`), `parse` (`Tok` → `ArithExpr`), and
//! evaluation (`eval_sync` for a scope with no `$(...)` reachable, and
//! `Kernel::eval_arith_async` in `kernel.rs` for the general case).
//!
//! Supports: decimal/hex/`base#digits` literals (base 2..=36), the full C
//! precedence table down through `?:`, `$name`/`${...}`/`$(...)`/nested
//! `$((...))` as operands, and bare `(( expr ))` as a condition (see
//! `Stmt::Arith`/`Expr::Arith` in `ast/types.rs`).
//!
//! Diverges from bash on purpose: overflow is an error, never a wrap; a
//! leading-zero numeral is refused, never read as octal; an unset or empty
//! operand is an error, never 0; `$(...)` on the unselected side of
//! `&&`/`||`/`?:` never runs.

use crate::ast::{Stmt, Value, VarPath};
use crate::interpreter::{value_defaults_on_emptiness, value_to_string, PathError, Scope};
use std::ops::Range;

/// An error from tokenizing, parsing, or evaluating `$(( ))`. `message` is
/// the full, final text shown to the caller; `span` is the byte range in the
/// arithmetic source the error concerns, when one exists (evaluation errors
/// over already-resolved values carry `0..0` — the message already names
/// the values).
#[derive(Debug, Clone, PartialEq)]
pub struct ArithError {
    pub message: String,
    pub span: Range<usize>,
}

impl ArithError {
    fn new(message: impl Into<String>, span: Range<usize>) -> Self {
        Self { message: message.into(), span }
    }
}

impl std::fmt::Display for ArithError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for ArithError {}

const MAX_DEPTH: usize = 256;

/// The decimal a leading-zero numeral was probably meant to be — `010`
/// becomes `10`, `-007` becomes `-7`. `None` when the text is not one.
///
/// The suggestion keeps the sign: `-007` is not fixed by writing `7`.
pub(crate) fn leading_zero_decimal(text: &str) -> Option<String> {
    if !crate::lexer::is_leading_zero_numeral(text) {
        return None;
    }
    let sign = if text.starts_with('-') { "-" } else { "" };
    let digits = text.trim_start_matches('-').trim_start_matches('0');
    Some(format!("{sign}{}", if digits.is_empty() { "0" } else { digits }))
}

// ═══════════════════════════════════════════════════════════════════
// Tokens
// ═══════════════════════════════════════════════════════════════════

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum BinOp {
    Add, Sub, Mul, Div, Rem, Pow, Shl, Shr,
    Lt, Le, Gt, Ge, Eq, Ne,
    BitAnd, BitXor, BitOr, And, Or,
}

impl BinOp {
    fn symbol(self) -> &'static str {
        match self {
            BinOp::Add => "+", BinOp::Sub => "-", BinOp::Mul => "*",
            BinOp::Div => "/", BinOp::Rem => "%", BinOp::Pow => "**",
            BinOp::Shl => "<<", BinOp::Shr => ">>",
            BinOp::Lt => "<", BinOp::Le => "<=", BinOp::Gt => ">", BinOp::Ge => ">=",
            BinOp::Eq => "==", BinOp::Ne => "!=",
            BinOp::BitAnd => "&", BinOp::BitXor => "^", BinOp::BitOr => "|",
            BinOp::And => "&&", BinOp::Or => "||",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum UnOp {
    Neg,
    Not,
    BitNot,
}

/// A `$(...)`/`${...}`/`$name`/`$?`/`$$`/`$((...))` operand, still
/// unresolved. Evaluating one is the only place `$(( ))` needs the async
/// evaluator — everything else in `ArithExpr` is pure.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expansion {
    /// Bare `x` or `$x` — the whole value.
    Var(String),
    /// `${root[...]...}` — a literal-key subscript path (the interpolation
    /// reading: brackets hold a KEY, not an expression).
    BracedPath { root: String, brackets: String },
    /// `${root[...]:-default}` — `default` is itself arithmetic source,
    /// evaluated only when `root` is unset or null.
    BracedDefault { root: String, brackets: String, default: String },
    /// `$?`
    LastExitCode,
    /// `$$`
    CurrentPid,
    /// `$(...)` — pre-parsed; running it needs the async evaluator.
    CommandSubst(Vec<Stmt>),
    /// `$((...))` — a nested arithmetic form, evaluated recursively.
    Nested(Box<ArithExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ArithExpr {
    Int(i64),
    Expansion(Expansion),
    /// `xs[i]`, `xs[i][j]` — Decision B: each bracket's contents is a
    /// numeric expression (the opposite of `${xs[i]}`'s literal key).
    Subscript { root: String, indices: Vec<ArithExpr> },
    /// `base#<expansion>` — the expansion's rendered text is read as digits
    /// in `base` (`2#$BITS`, `10#$(date +%m)`).
    BasedExpansion { base: u32, expansion: Box<Expansion> },
    Unary { op: UnOp, operand: Box<ArithExpr> },
    Binary { op: BinOp, left: Box<ArithExpr>, right: Box<ArithExpr> },
    Ternary { cond: Box<ArithExpr>, then_branch: Box<ArithExpr>, else_branch: Box<ArithExpr> },
}

impl ArithExpr {
    /// True when some reachable node is a `$(...)` — used by callers that
    /// want the sync fast path when it is safe.
    pub(crate) fn contains_command_subst(&self) -> bool {
        fn expansion_has(e: &Expansion) -> bool {
            match e {
                Expansion::CommandSubst(_) => true,
                Expansion::Nested(inner) => inner.contains_command_subst(),
                // The default is unparsed text at this point (parsing
                // happens only if it is actually reached, at eval time) —
                // parse it here just to answer the question. A parse
                // failure changes nothing: eval will hit the identical
                // parse error on either path.
                Expansion::BracedDefault { default, .. } => parse(default)
                    .map(|parsed| parsed.contains_command_subst())
                    .unwrap_or(false),
                Expansion::Var(_)
                | Expansion::BracedPath { .. }
                | Expansion::LastExitCode
                | Expansion::CurrentPid => false,
            }
        }
        match self {
            ArithExpr::Int(_) => false,
            ArithExpr::Expansion(e) => expansion_has(e),
            ArithExpr::Subscript { indices, .. } => {
                indices.iter().any(ArithExpr::contains_command_subst)
            }
            ArithExpr::BasedExpansion { expansion, .. } => expansion_has(expansion),
            ArithExpr::Unary { operand, .. } => operand.contains_command_subst(),
            ArithExpr::Binary { left, right, .. } => {
                left.contains_command_subst() || right.contains_command_subst()
            }
            ArithExpr::Ternary { cond, then_branch, else_branch } => {
                cond.contains_command_subst()
                    || then_branch.contains_command_subst()
                    || else_branch.contains_command_subst()
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum TokKind {
    Number(u64),
    BasedExpansion { base: u32, expansion: Box<Expansion> },
    Ident(String),
    Expansion(Expansion),
    LParen,
    RParen,
    LBracket,
    RBracket,
    Question,
    Colon,
    Op(BinOp),
    Bang,
    Tilde,
}

impl std::fmt::Display for TokKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokKind::Number(n) => write!(f, "{n}"),
            TokKind::BasedExpansion { base, .. } => write!(f, "{base}#..."),
            TokKind::Ident(name) => write!(f, "{name}"),
            TokKind::Expansion(_) => write!(f, "$..."),
            TokKind::LParen => write!(f, "("),
            TokKind::RParen => write!(f, ")"),
            TokKind::LBracket => write!(f, "["),
            TokKind::RBracket => write!(f, "]"),
            TokKind::Question => write!(f, "?"),
            TokKind::Colon => write!(f, ":"),
            TokKind::Op(op) => write!(f, "{}", op.symbol()),
            TokKind::Bang => write!(f, "!"),
            TokKind::Tilde => write!(f, "~"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Tok {
    kind: TokKind,
    span: Range<usize>,
}

// ═══════════════════════════════════════════════════════════════════
// Tokenizer
// ═══════════════════════════════════════════════════════════════════

struct Tokenizer<'a> {
    text: &'a str,
    chars: Vec<(usize, char)>,
    pos: usize,
}

impl<'a> Tokenizer<'a> {
    fn new(text: &'a str) -> Self {
        Self { text, chars: text.char_indices().collect(), pos: 0 }
    }

    fn byte_pos(&self) -> usize {
        self.chars.get(self.pos).map(|(b, _)| *b).unwrap_or(self.text.len())
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).map(|(_, c)| *c)
    }

    fn peek_at(&self, n: usize) -> Option<char> {
        self.chars.get(self.pos + n).map(|(_, c)| *c)
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.peek();
        if c.is_some() {
            self.pos += 1;
        }
        c
    }

    fn slice(&self, start: usize, end: usize) -> &'a str {
        let end_byte = self.chars.get(end).map(|(b, _)| *b).unwrap_or(self.text.len());
        let start_byte = self.chars.get(start).map(|(b, _)| *b).unwrap_or(self.text.len());
        &self.text[start_byte..end_byte]
    }

    /// End of the numeral run starting at `from` — digits, letters, and `_`.
    /// An error quotes the literal the user wrote, not the prefix scanned so
    /// far, so `1_000` is not reported as `1_`.
    fn numeral_run_end(&self, from: usize) -> usize {
        let mut end = from;
        while self.chars.get(end).is_some_and(|(_, c)| c.is_ascii_alphanumeric() || *c == '_') {
            end += 1;
        }
        end
    }

    fn skip_ws(&mut self) {
        while matches!(self.peek(), Some(c) if c.is_whitespace()) {
            self.pos += 1;
        }
    }

    fn tokenize(mut self) -> Result<Vec<Tok>, ArithError> {
        let mut out = Vec::new();
        loop {
            self.skip_ws();
            let Some(c) = self.peek() else { break };
            let start = self.pos;
            let start_byte = self.byte_pos();
            let kind = match c {
                '0'..='9' => self.lex_number()?,
                '$' => self.lex_dollar()?,
                c if c.is_ascii_alphabetic() || c == '_' => self.lex_ident(),
                '(' => { self.advance(); TokKind::LParen }
                ')' => { self.advance(); TokKind::RParen }
                '[' => { self.advance(); TokKind::LBracket }
                ']' => { self.advance(); TokKind::RBracket }
                '?' => { self.advance(); TokKind::Question }
                ':' => { self.advance(); TokKind::Colon }
                '~' => { self.advance(); TokKind::Tilde }
                '!' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        TokKind::Op(BinOp::Ne)
                    } else {
                        TokKind::Bang
                    }
                }
                '+' => {
                    self.advance();
                    self.reject_compound_or('+', start_byte, &out)?;
                    TokKind::Op(BinOp::Add)
                }
                '-' => {
                    self.advance();
                    self.reject_compound_or('-', start_byte, &out)?;
                    TokKind::Op(BinOp::Sub)
                }
                '*' => {
                    self.advance();
                    if self.peek() == Some('*') {
                        self.advance();
                        TokKind::Op(BinOp::Pow)
                    } else {
                        TokKind::Op(BinOp::Mul)
                    }
                }
                '/' => { self.advance(); TokKind::Op(BinOp::Div) }
                '%' => { self.advance(); TokKind::Op(BinOp::Rem) }
                '<' => {
                    self.advance();
                    if self.peek() == Some('<') {
                        self.advance();
                        if self.peek() == Some('<') {
                            return Err(ArithError::new(
                                "`<<<` is a here-string, not an operator; write `<<` to shift",
                                start..self.pos + 1,
                            ));
                        }
                        TokKind::Op(BinOp::Shl)
                    } else if self.peek() == Some('=') {
                        self.advance();
                        TokKind::Op(BinOp::Le)
                    } else {
                        TokKind::Op(BinOp::Lt)
                    }
                }
                '>' => {
                    self.advance();
                    if self.peek() == Some('>') {
                        self.advance();
                        if self.peek() == Some('>') {
                            return Err(ArithError::new(
                                "`>>>` is not an operator; write `>>`",
                                start..self.pos + 1,
                            ));
                        }
                        TokKind::Op(BinOp::Shr)
                    } else if self.peek() == Some('=') {
                        self.advance();
                        TokKind::Op(BinOp::Ge)
                    } else {
                        TokKind::Op(BinOp::Gt)
                    }
                }
                '=' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        TokKind::Op(BinOp::Eq)
                    } else {
                        let end = self.text.len();
                        let rhs = self.text[self.byte_pos()..].trim();
                        if let Some((name, name_start)) = Self::preceding_name(&out) {
                            let source = &self.text[name_start..end];
                            return Err(ArithError::new(
                                format!(
                                    "`{source}` assigns inside `$(( ))`; write `{name}={rhs}`, or `==` to compare"
                                ),
                                name_start..end,
                            ));
                        }
                        let source = &self.text[start_byte..end];
                        return Err(ArithError::new(
                            format!(
                                "`{source}` assigns inside `$(( ))`; write `name=rhs`, or `==` to compare"
                            ),
                            start_byte..end,
                        ));
                    }
                }
                '&' => {
                    self.advance();
                    if self.peek() == Some('&') {
                        self.advance();
                        TokKind::Op(BinOp::And)
                    } else {
                        TokKind::Op(BinOp::BitAnd)
                    }
                }
                '|' => {
                    self.advance();
                    if self.peek() == Some('|') {
                        self.advance();
                        TokKind::Op(BinOp::Or)
                    } else {
                        TokKind::Op(BinOp::BitOr)
                    }
                }
                '^' => { self.advance(); TokKind::Op(BinOp::BitXor) }
                ',' => {
                    return Err(ArithError::new(
                        "`,` is not an operator; one expression per `$(( ))`",
                        start..self.pos + 1,
                    ));
                }
                other => {
                    return Err(ArithError::new(
                        format!("`{other}` cannot start a value"),
                        start..self.pos + 1,
                    ));
                }
            };
            out.push(Tok { kind, span: start_byte..self.byte_pos() });
        }
        Ok(out)
    }

    /// The identifier that ends immediately before `op_start_byte` — the
    /// `x` in `x++`/`x += 2`/`x = 2`, read from the token already emitted
    /// (the operator has not been pushed onto `out` yet).
    fn preceding_name(out: &[Tok]) -> Option<(String, usize)> {
        match out.last() {
            Some(Tok { kind: TokKind::Ident(name), span }) => Some((name.clone(), span.start)),
            _ => None,
        }
    }

    /// The identifier starting at the current position — the `x` in
    /// `++x`/`--x`, where the operator precedes the name. Consumes it:
    /// this is only called on a path that is about to return `Err`, so
    /// leaving `self.pos` past it does not affect anything further.
    fn consume_following_name(&mut self) -> Option<(String, usize)> {
        let start = self.pos;
        if !matches!(self.peek(), Some(c) if c.is_ascii_alphabetic() || c == '_') {
            return None;
        }
        while matches!(self.peek(), Some(c) if c.is_ascii_alphanumeric() || c == '_') {
            self.pos += 1;
        }
        Some((self.slice(start, self.pos).to_string(), self.byte_pos()))
    }

    /// After consuming `+`/`-`, refuse `++`/`--`/`+=`/`-=` outright — kaish
    /// has no assignment or increment inside `$(( ))`. Names the real
    /// identifier — from the token just emitted for `x++`/`x+=` (postfix),
    /// or scanned forward for `++x` (prefix) — rather than a placeholder.
    fn reject_compound_or(&mut self, sym: char, op_start_byte: usize, out: &[Tok]) -> Result<(), ArithError> {
        let step = if sym == '+' { "+ 1" } else { "- 1" };
        if self.peek() == Some(sym) {
            self.advance();
            let end = self.byte_pos();
            if let Some((name, name_start)) = Self::preceding_name(out) {
                let source = &self.text[name_start..end];
                return Err(ArithError::new(
                    format!("`{source}` assigns inside `$(( ))`; write `{name}=$(({name} {step}))`"),
                    name_start..end,
                ));
            }
            if let Some((name, name_end)) = self.consume_following_name() {
                let source = &self.text[op_start_byte..name_end];
                return Err(ArithError::new(
                    format!("`{source}` assigns inside `$(( ))`; write `{name}=$(({name} {step}))`"),
                    op_start_byte..name_end,
                ));
            }
            let source = &self.text[op_start_byte..end];
            return Err(ArithError::new(
                format!("`{source}` assigns inside `$(( ))`; write `name=$((name {step}))`"),
                op_start_byte..end,
            ));
        }
        if self.peek() == Some('=') {
            self.advance();
            let end = self.text.len();
            let rhs = self.text[self.byte_pos()..].trim();
            if let Some((name, name_start)) = Self::preceding_name(out) {
                let source = &self.text[name_start..end];
                return Err(ArithError::new(
                    format!("`{source}` assigns inside `$(( ))`; write `{name}=$(({name} {sym} {rhs}))`"),
                    name_start..end,
                ));
            }
            let source = &self.text[op_start_byte..end];
            return Err(ArithError::new(
                format!("`{source}` assigns inside `$(( ))`; write `name=$((name {sym} rhs))`"),
                op_start_byte..end,
            ));
        }
        Ok(())
    }

    fn lex_ident(&mut self) -> TokKind {
        let start = self.pos;
        while matches!(self.peek(), Some(c) if c.is_ascii_alphanumeric() || c == '_') {
            self.pos += 1;
        }
        TokKind::Ident(self.slice(start, self.pos).to_string())
    }

    /// Consume a run of base-`base` digits (case-insensitive letters past
    /// `9`), erroring loud on `_` or a digit too large for `base`. Returns
    /// the checked magnitude and the run's end index (== start if empty).
    fn consume_digits(&mut self, base: u32, lit_start: usize) -> Result<(u64, usize), ArithError> {
        let digits_start = self.pos;
        let mut mag: u64 = 0;
        while let Some(c) = self.peek() {
            if c == '_' {
                return Err(ArithError::new(
                    format!("`{}` contains `_`; remove it", self.slice(lit_start, self.numeral_run_end(self.pos))),
                    lit_start..self.byte_pos() + c.len_utf8(),
                ));
            }
            if !c.is_ascii_alphanumeric() {
                break;
            }
            let digit_val = match c {
                '0'..='9' => c as u32 - '0' as u32,
                'a'..='z' => c as u32 - 'a' as u32 + 10,
                'A'..='Z' => c as u32 - 'A' as u32 + 10,
                _ => unreachable!("ascii_alphanumeric"),
            };
            if digit_val >= base {
                self.pos += 1;
                return Err(ArithError::new(
                    format!(
                        "`{c}` is not a digit in `{}`; use digits valid for base {base}",
                        self.slice(lit_start, self.pos)
                    ),
                    lit_start..self.byte_pos(),
                ));
            }
            mag = mag
                .checked_mul(base as u64)
                .and_then(|m| m.checked_add(digit_val as u64))
                .ok_or_else(|| {
                    ArithError::new(
                        format!("`{}` {INTEGER_OUT_OF_RANGE}", self.slice(lit_start, self.numeral_run_end(self.pos))),
                        lit_start..self.byte_pos(),
                    )
                })?;
            self.pos += 1;
        }
        Ok((mag, digits_start))
    }

    /// Consume a run of plain `0`-`9` digits, erroring loud on `_`. Unlike
    /// `Self::consume_digits`, a non-digit letter (`e`, `x`, …) is a clean
    /// stop, not an error — the base-10 run is used both as a full decimal
    /// literal and as the base number before `#`, and the caller decides
    /// what a trailing `e3`/`.5`/`#` means.
    fn consume_decimal_digits(&mut self, lit_start: usize) -> Result<(u64, usize), ArithError> {
        let digits_start = self.pos;
        let mut mag: u64 = 0;
        while let Some(c) = self.peek() {
            if c == '_' {
                return Err(ArithError::new(
                    format!("`{}` contains `_`; remove it", self.slice(lit_start, self.numeral_run_end(self.pos))),
                    lit_start..self.byte_pos() + c.len_utf8(),
                ));
            }
            if !c.is_ascii_digit() {
                break;
            }
            let digit_val = c as u64 - '0' as u64;
            mag = mag.checked_mul(10).and_then(|m| m.checked_add(digit_val)).ok_or_else(|| {
                ArithError::new(
                    format!("`{}` {INTEGER_OUT_OF_RANGE}", self.slice(lit_start, self.numeral_run_end(self.pos))),
                    lit_start..self.byte_pos(),
                )
            })?;
            self.pos += 1;
        }
        Ok((mag, digits_start))
    }

    fn lex_number(&mut self) -> Result<TokKind, ArithError> {
        let start = self.pos;

        // `0x` / `0X` hex.
        if self.peek() == Some('0') && matches!(self.peek_at(1), Some('x' | 'X')) {
            self.pos += 2;
            let prefix = self.slice(start, self.pos).to_string();
            let (mag, digits_start) = self.consume_digits(16, start)?;
            if digits_start == self.pos {
                return Err(ArithError::new(
                    format!("`{prefix}` has no digits; add digits after `{prefix}`"),
                    start..self.pos,
                ));
            }
            return Ok(TokKind::Number(mag));
        }

        // `0b` / `0o` — not a kaish base spelling.
        if self.peek() == Some('0') && matches!(self.peek_at(1), Some('b' | 'B' | 'o' | 'O')) {
            let kind_char = self.peek_at(1).unwrap_or('b');
            self.pos += 2;
            let digits_start = self.pos;
            while matches!(self.peek(), Some(c) if c.is_ascii_alphanumeric()) {
                self.pos += 1;
            }
            let digits = self.slice(digits_start, self.pos);
            let full = self.slice(start, self.pos);
            let (base, word) = if matches!(kind_char, 'b' | 'B') { (2, "binary") } else { (8, "octal") };
            return Err(ArithError::new(
                format!("`{full}` is not a kaish base spelling; write `{base}#{digits}` for {word}"),
                start..self.pos,
            ));
        }

        // Plain decimal run — either a bare decimal literal, or the base
        // number before `#`.
        let (base_mag, digits_start) = self.consume_decimal_digits(start)?;
        debug_assert!(digits_start == start);

        // Float/exponent shape (`1.5`, `1e3`, `1E-3`): not a kaish spelling
        // — checked before `#` and leading-zero, since a numeral can't be
        // both a based prefix and a float.
        let looks_like_float = (self.peek() == Some('.')
            && matches!(self.peek_at(1), Some(c) if c.is_ascii_digit()))
            || (matches!(self.peek(), Some('e' | 'E'))
                && (matches!(self.peek_at(1), Some(c) if c.is_ascii_digit())
                    || (matches!(self.peek_at(1), Some('+' | '-'))
                        && matches!(self.peek_at(2), Some(c) if c.is_ascii_digit()))));
        if looks_like_float {
            if self.peek() == Some('.') {
                self.pos += 1;
                while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
                    self.pos += 1;
                }
            }
            if matches!(self.peek(), Some('e' | 'E')) {
                self.pos += 1;
                if matches!(self.peek(), Some('+' | '-')) {
                    self.pos += 1;
                }
                while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
                    self.pos += 1;
                }
            }
            let text = self.slice(start, self.pos);
            return Err(ArithError::new(
                format!("`{text}` is not an integer; arithmetic is integer-only"),
                start..self.pos,
            ));
        }

        if self.peek() == Some('#') {
            let base_text = self.slice(start, self.pos);
            if base_text.len() > 1 && base_text.starts_with('0') {
                return Err(ArithError::new(
                    format!("`{base_text}` is not a base spelling; write the base without a leading zero"),
                    start..self.pos,
                ));
            }
            self.advance(); // consume '#'
            // Range-check the full u64 before narrowing: `as u32` on
            // k*2^32 + b (b in 2..=36) truncates to b and passes the
            // check, silently evaluating in base b instead of refusing.
            if !(2..=36).contains(&base_mag) {
                return Err(ArithError::new(
                    format!("base `{base_mag}` is outside 2..=36"),
                    start..self.pos,
                ));
            }
            let base = base_mag as u32;
            if let Some(sign @ ('+' | '-')) = self.peek() {
                let sign_start = self.pos;
                self.pos += 1;
                while matches!(self.peek(), Some(c) if c.is_ascii_alphanumeric()) {
                    self.pos += 1;
                }
                let lit = self.slice(start, self.pos);
                return Err(ArithError::new(
                    format!("`{lit}` puts `{sign}` after `#`; write `{sign}{base}#{}`", self.slice(sign_start + 1, self.pos)),
                    start..self.pos,
                ));
            }
            if self.peek() == Some('$') {
                let expansion = self.lex_expansion_body()?;
                return Ok(TokKind::BasedExpansion { base, expansion: Box::new(expansion) });
            }
            let prefix = self.slice(start, self.pos).to_string();
            let (mag, bdigits_start) = self.consume_digits(base, start)?;
            if bdigits_start == self.pos {
                return Err(ArithError::new(
                    format!("`{prefix}` has no digits; add digits after `{prefix}`"),
                    start..self.pos,
                ));
            }
            return Ok(TokKind::Number(mag));
        }

        let text = self.slice(start, self.pos);
        if crate::lexer::is_leading_zero_numeral(text) {
            let decimal = leading_zero_decimal(text).unwrap_or_else(|| "0".to_string());
            return Err(ArithError::new(
                format!(
                    "`{text}` has a leading zero — kaish reads no octal; write `8#{}` for octal or `{decimal}` for decimal",
                    text.trim_start_matches('0')
                ),
                start..self.pos,
            ));
        }
        Ok(TokKind::Number(base_mag))
    }

    /// `$name`, `$?`, `$$`, `${...}`, `$(...)`, `$((...))` starting at the
    /// current `$`.
    fn lex_dollar(&mut self) -> Result<TokKind, ArithError> {
        Ok(TokKind::Expansion(self.lex_expansion_body()?))
    }

    /// Same as `Self::lex_dollar` but returning the bare `Expansion`,
    /// for `base#$name` and `base#$(...)`.
    fn lex_expansion_body(&mut self) -> Result<Expansion, ArithError> {
        let dollar_start = self.pos;
        self.advance(); // consume '$'
        match self.peek() {
            Some('?') => { self.advance(); Ok(Expansion::LastExitCode) }
            Some('$') => { self.advance(); Ok(Expansion::CurrentPid) }
            Some('(') if self.peek_at(1) == Some('(') => {
                self.pos += 2; // consume both '(' after the '$'
                let inner_start = self.pos;
                let close = self.skip_group(')', true, dollar_start, false)?;
                let inner_text = self.slice(inner_start, close).to_string();
                let inner = parse(&inner_text)?;
                Ok(Expansion::Nested(Box::new(inner)))
            }
            Some('(') => {
                self.advance(); // consume '('
                // `skip_group` finds the close; the general lexer can't
                // re-tokenize arithmetic syntax here. `comments = true`.
                let cmd_start = self.pos;
                let close = self.skip_group(')', false, dollar_start, true)?;
                let cmd_text = self.slice(cmd_start, close).to_string();
                match crate::parser::parse(&cmd_text) {
                    Ok(program) => Ok(Expansion::CommandSubst(program.statements)),
                    Err(_) => Err(ArithError::new(
                        format!("syntax error in command substitution: $({cmd_text})"),
                        dollar_start..self.byte_pos(),
                    )),
                }
            }
            Some('{') => {
                self.advance(); // consume '{'
                let body_start = self.pos;
                let close = self.skip_group('}', false, dollar_start, false)?;
                let body = self.slice(body_start, close).to_string();
                parse_braced_body(&body, dollar_start..self.byte_pos())
            }
            // `$1`, `$2`, … — positional parameters. A leading digit is
            // otherwise not a valid identifier start, so it is unambiguous
            // here: bash allows only a single digit unbraced, but kaish
            // reads the whole run (`${10}` still works too).
            Some(c) if c.is_ascii_alphabetic() || c == '_' || c.is_ascii_digit() => {
                let start = self.pos;
                while matches!(self.peek(), Some(c) if c.is_ascii_alphanumeric() || c == '_') {
                    self.pos += 1;
                }
                Ok(Expansion::Var(self.slice(start, self.pos).to_string()))
            }
            _ => Err(ArithError::new(
                format!("`{}` cannot start a value", self.slice(dollar_start, self.pos + 1)),
                dollar_start..self.byte_pos() + 1,
            )),
        }
    }

    /// Scan a balanced group from just past its opener (`$(`, `$((`, `${`, or
    /// a bare `(`) to its `close`, quote/escape-aware and recursing into
    /// nested `$(…)`, `$((…))`, `${…}`, and `(…)`. Returns the char index of
    /// the close and leaves `self.pos` past it; the body is
    /// `slice(body_start, close)`. `double` closes on two `close` chars
    /// (`$((…))`).
    ///
    /// `comments` true treats `#` as a comment to EOL (command-substitution
    /// bodies only); false leaves it as the base separator (`$((…))`) or a
    /// literal (`${…}`). The word boundary reuses `lexer::opens_a_word`.
    /// `group_start` is the error span for an unterminated group.
    fn skip_group(
        &mut self,
        close: char,
        double: bool,
        group_start: usize,
        comments: bool,
    ) -> Result<usize, ArithError> {
        let open: char = if close == '}' { '{' } else { '(' };
        let mut depth = 1i32;
        loop {
            match self.peek() {
                None => {
                    let close_str = if double { "))" } else if close == '}' { "}" } else { ")" };
                    return Err(ArithError::new(
                        format!("`{}` has no closing `{close_str}`", self.slice(group_start, self.pos)),
                        group_start..self.byte_pos(),
                    ));
                }
                Some('\\') => {
                    self.pos += 1;
                    if self.peek().is_some() {
                        self.pos += 1;
                    }
                }
                Some('\'') => {
                    self.pos += 1;
                    while matches!(self.peek(), Some(c) if c != '\'') {
                        self.pos += 1;
                    }
                    if self.peek() == Some('\'') {
                        self.pos += 1;
                    }
                    // unterminated → `None` errors "no closing …"
                }
                Some('"') => {
                    self.pos += 1;
                    loop {
                        match self.peek() {
                            None => break,
                            Some('\\') => {
                                self.pos += 1;
                                if self.peek().is_some() {
                                    self.pos += 1;
                                }
                            }
                            Some('"') => {
                                self.pos += 1;
                                break;
                            }
                            Some(_) => self.pos += 1,
                        }
                    }
                }
                Some('#') => {
                    if comments {
                        // comment only at a word start (`opens_a_word`);
                        // skip to EOL (`\n`/`\r`, matching the lexer)
                        // so a `)` in it does not close.
                        let prev = self
                            .pos
                            .checked_sub(1)
                            .and_then(|i| self.chars.get(i))
                            .map(|(_, c)| *c);
                        if prev.is_none() || prev.is_some_and(crate::lexer::opens_a_word) {
                            while matches!(self.peek(), Some(c) if c != '\n' && c != '\r') {
                                self.pos += 1;
                            }
                        } else {
                            self.pos += 1; // mid-word `#` is a normal char here
                        }
                    } else {
                        self.pos += 1; // `#` is the base separator / literal
                    }
                }
                Some('$') => {
                    let nested_start = self.pos;
                    match self.peek_at(1) {
                        Some('(') if self.peek_at(2) == Some('(') => {
                            self.pos += 3;
                            self.skip_group(')', true, nested_start, false)?;
                        }
                        Some('(') => {
                            self.pos += 2;
                            self.skip_group(')', false, nested_start, true)?;
                        }
                        Some('{') => {
                            self.pos += 2;
                            self.skip_group('}', false, nested_start, false)?;
                        }
                        _ => self.pos += 1, // `$name` — the `$` is plain here
                    }
                }
                Some('(') => {
                    // bare `(`: recurse so its `)`/`}` does not close the
                    // outer group; `comments` propagates.
                    let nested_start = self.pos;
                    self.pos += 1;
                    self.skip_group(')', false, nested_start, comments)?;
                }
                Some(c) if c == open => {
                    depth += 1;
                    self.pos += 1;
                }
                Some(c) if c == close => {
                    if double {
                        if self.peek_at(1) == Some(close) {
                            let close_pos = self.pos;
                            self.pos += 2;
                            return Ok(close_pos);
                        }
                        // lone `)` in `$((…))`: consume, keep scanning.
                        self.pos += 1;
                    } else {
                        depth -= 1;
                        self.pos += 1;
                        if depth == 0 {
                            return Ok(self.pos - 1);
                        }
                    }
                }
                Some(_) => {
                    self.pos += 1;
                }
            }
        }
    }
}

/// Message for a numeral outside i64 range — shared text with the lexer.
use crate::lexer::INTEGER_OUT_OF_RANGE;

fn split_name_and_brackets(text: &str) -> Option<(String, String)> {
    let bracket_start = text.find('[').unwrap_or(text.len());
    let name = &text[..bracket_start];
    if name.is_empty() || !name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
        || name.chars().next().is_some_and(|c| c.is_ascii_digit())
    {
        return None;
    }
    Some((name.to_string(), text[bracket_start..].to_string()))
}

fn parse_braced_body(body: &str, span: Range<usize>) -> Result<Expansion, ArithError> {
    if body == "?" {
        return Ok(Expansion::LastExitCode);
    }
    if body == "$" {
        return Ok(Expansion::CurrentPid);
    }
    let bytes = body.as_bytes();
    let mut depth = 0i32;
    let mut default_at = None;
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'[' => depth += 1,
            b']' => depth -= 1,
            b':' if depth == 0 && bytes.get(i + 1) == Some(&b'-') => {
                default_at = Some(i);
                break;
            }
            _ => {}
        }
        i += 1;
    }
    if let Some(idx) = default_at {
        let root_and_sub = &body[..idx];
        let default = body[idx + 2..].to_string();
        let Some((root, brackets)) = split_name_and_brackets(root_and_sub) else {
            return Err(ArithError::new(format!("`{{{body}}}` is not valid inside `$(( ))`"), span));
        };
        return Ok(Expansion::BracedDefault { root, brackets, default });
    }
    let Some((root, brackets)) = split_name_and_brackets(body) else {
        return Err(ArithError::new(format!("`{{{body}}}` is not valid inside `$(( ))`"), span));
    };
    if brackets.is_empty() {
        Ok(Expansion::Var(root))
    } else {
        Ok(Expansion::BracedPath { root, brackets })
    }
}

fn tokenize(text: &str) -> Result<Vec<Tok>, ArithError> {
    Tokenizer::new(text).tokenize()
}

// ═══════════════════════════════════════════════════════════════════
// Parser (precedence climbing over the EBNF, high to low: unary, `**`,
// `* / %`, `+ -`, `<< >>`, `< <= > >=`, `== !=`, `&`, `^`, `|`, `&&`,
// `||`, `?:`)
// ═══════════════════════════════════════════════════════════════════

struct Parser {
    toks: Vec<Tok>,
    pos: usize,
    depth: usize,
    end: usize,
    /// The arithmetic source, kept so a "no operand" error can quote the
    /// text consumed so far (`{expr}` in the spec's error table).
    text: String,
}

impl Parser {
    fn new(toks: Vec<Tok>, end: usize, text: &str) -> Self {
        Self { toks, pos: 0, depth: 0, end, text: text.to_string() }
    }

    /// `` `{op}` has no right operand in `{expr}` `` — the operator was the
    /// last token; `expr` is the whole source (trailing whitespace
    /// included, as the spec's own example shows: "1 + " — this only fires
    /// when the operator was the LAST token, so the whole text amounts to
    /// "everything through end of input").
    fn missing_right_operand(&self, op: &str, op_span: &Range<usize>) -> ArithError {
        ArithError::new(
            format!(
                "`{op}` has no right operand in `{}`; add an integer expression after `{op}`",
                self.text
            ),
            op_span.clone(),
        )
    }

    /// `` `{op}` has no operand `` — a unary/power operator with nothing at
    /// all after it (no left operand to show, unlike the binary case).
    fn missing_operand(&self, op: &str, op_span: &Range<usize>) -> ArithError {
        ArithError::new(
            format!("`{op}` has no operand; add an integer expression after `{op}`"),
            op_span.clone(),
        )
    }

    fn peek(&self) -> Option<&TokKind> {
        self.toks.get(self.pos).map(|t| &t.kind)
    }

    fn peek_span(&self) -> Range<usize> {
        self.toks.get(self.pos).map(|t| t.span.clone()).unwrap_or(self.end..self.end)
    }

    fn advance(&mut self) -> Option<Tok> {
        let t = self.toks.get(self.pos).cloned();
        if t.is_some() {
            self.pos += 1;
        }
        t
    }

    fn enter(&mut self) -> Result<(), ArithError> {
        self.depth += 1;
        if self.depth > MAX_DEPTH {
            return Err(ArithError::new("more than 256 nested arithmetic forms", self.peek_span()));
        }
        Ok(())
    }

    fn leave(&mut self) {
        self.depth -= 1;
    }

    fn left_assoc(
        &mut self,
        ops: &[BinOp],
        mut next: impl FnMut(&mut Self) -> Result<ArithExpr, ArithError>,
    ) -> Result<ArithExpr, ArithError> {
        let mut left = next(self)?;
        loop {
            let matched = ops.iter().copied().find(|op| self.peek() == Some(&TokKind::Op(*op)));
            let Some(op) = matched else { break };
            let op_span = self.peek_span();
            self.pos += 1;
            if self.pos >= self.toks.len() {
                return Err(self.missing_right_operand(op.symbol(), &op_span));
            }
            let right = next(self)?;
            left = ArithExpr::Binary { op, left: Box::new(left), right: Box::new(right) };
        }
        Ok(left)
    }

    fn parse_conditional(&mut self) -> Result<ArithExpr, ArithError> {
        self.enter()?;
        let cond = self.parse_logical_or()?;
        let result = if self.peek() == Some(&TokKind::Question) {
            self.pos += 1;
            let then_branch = self.parse_conditional()?;
            match self.peek() {
                Some(&TokKind::Colon) => self.pos += 1,
                _ => {
                    return Err(ArithError::new("`?` has no matching `:`", self.peek_span()));
                }
            }
            let else_branch = self.parse_conditional()?;
            ArithExpr::Ternary {
                cond: Box::new(cond),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            }
        } else {
            cond
        };
        self.leave();
        Ok(result)
    }

    fn parse_logical_or(&mut self) -> Result<ArithExpr, ArithError> {
        self.left_assoc(&[BinOp::Or], Self::parse_logical_and)
    }

    fn parse_logical_and(&mut self) -> Result<ArithExpr, ArithError> {
        self.left_assoc(&[BinOp::And], Self::parse_bitor)
    }

    fn parse_bitor(&mut self) -> Result<ArithExpr, ArithError> {
        self.left_assoc(&[BinOp::BitOr], Self::parse_bitxor)
    }

    fn parse_bitxor(&mut self) -> Result<ArithExpr, ArithError> {
        self.left_assoc(&[BinOp::BitXor], Self::parse_bitand)
    }

    fn parse_bitand(&mut self) -> Result<ArithExpr, ArithError> {
        self.left_assoc(&[BinOp::BitAnd], Self::parse_equality)
    }

    fn parse_equality(&mut self) -> Result<ArithExpr, ArithError> {
        self.left_assoc(&[BinOp::Eq, BinOp::Ne], Self::parse_relational)
    }

    fn parse_relational(&mut self) -> Result<ArithExpr, ArithError> {
        self.left_assoc(&[BinOp::Le, BinOp::Ge, BinOp::Lt, BinOp::Gt], Self::parse_shift)
    }

    fn parse_shift(&mut self) -> Result<ArithExpr, ArithError> {
        self.left_assoc(&[BinOp::Shl, BinOp::Shr], Self::parse_additive)
    }

    fn parse_additive(&mut self) -> Result<ArithExpr, ArithError> {
        self.left_assoc(&[BinOp::Add, BinOp::Sub], Self::parse_multiplicative)
    }

    fn parse_multiplicative(&mut self) -> Result<ArithExpr, ArithError> {
        self.left_assoc(&[BinOp::Mul, BinOp::Div, BinOp::Rem], Self::parse_power)
    }

    fn parse_power(&mut self) -> Result<ArithExpr, ArithError> {
        let base = self.parse_unary()?;
        if self.peek() == Some(&TokKind::Op(BinOp::Pow)) {
            let op_span = self.peek_span();
            self.pos += 1;
            if self.pos >= self.toks.len() {
                return Err(self.missing_right_operand("**", &op_span));
            }
            self.enter()?;
            let exp = self.parse_power()?;
            self.leave();
            Ok(ArithExpr::Binary { op: BinOp::Pow, left: Box::new(base), right: Box::new(exp) })
        } else {
            Ok(base)
        }
    }

    /// `i64::MIN`'s magnitude, `9223372036854775808`, has no representation
    /// as a positive `i64` — it is legal only as the direct operand of a
    /// single unary minus.
    const MIN_MAGNITUDE: u64 = 9_223_372_036_854_775_808;

    fn parse_unary(&mut self) -> Result<ArithExpr, ArithError> {
        self.enter()?;
        let result = match self.peek() {
            Some(&TokKind::Op(BinOp::Sub)) => {
                let op_span = self.peek_span();
                self.pos += 1;
                if self.pos >= self.toks.len() {
                    self.leave();
                    return Err(self.missing_operand("-", &op_span));
                }
                if let Some(&TokKind::Number(mag)) = self.peek() {
                    if mag == Self::MIN_MAGNITUDE {
                        self.pos += 1;
                        self.leave();
                        return Ok(ArithExpr::Int(i64::MIN));
                    }
                }
                let operand = self.parse_unary()?;
                ArithExpr::Unary { op: UnOp::Neg, operand: Box::new(operand) }
            }
            Some(&TokKind::Op(BinOp::Add)) => {
                let op_span = self.peek_span();
                self.pos += 1;
                if self.pos >= self.toks.len() {
                    self.leave();
                    return Err(self.missing_operand("+", &op_span));
                }
                self.parse_unary()?
            }
            Some(&TokKind::Bang) => {
                let op_span = self.peek_span();
                self.pos += 1;
                if self.pos >= self.toks.len() {
                    self.leave();
                    return Err(self.missing_operand("!", &op_span));
                }
                let operand = self.parse_unary()?;
                ArithExpr::Unary { op: UnOp::Not, operand: Box::new(operand) }
            }
            Some(&TokKind::Tilde) => {
                let op_span = self.peek_span();
                self.pos += 1;
                if self.pos >= self.toks.len() {
                    self.leave();
                    return Err(self.missing_operand("~", &op_span));
                }
                let operand = self.parse_unary()?;
                ArithExpr::Unary { op: UnOp::BitNot, operand: Box::new(operand) }
            }
            _ => self.parse_primary()?,
        };
        self.leave();
        Ok(result)
    }

    fn parse_primary(&mut self) -> Result<ArithExpr, ArithError> {
        let span = self.peek_span();
        match self.advance().map(|t| t.kind) {
            Some(TokKind::Number(mag)) => int_from_magnitude(mag, false, span),
            Some(TokKind::BasedExpansion { base, expansion }) => {
                Ok(ArithExpr::BasedExpansion { base, expansion })
            }
            Some(TokKind::Expansion(e)) => Ok(ArithExpr::Expansion(e)),
            Some(TokKind::Ident(name)) => {
                if self.peek() == Some(&TokKind::LBracket) {
                    let mut indices = Vec::new();
                    while self.peek() == Some(&TokKind::LBracket) {
                        self.pos += 1;
                        self.enter()?;
                        let index = self.parse_conditional()?;
                        self.leave();
                        match self.peek() {
                            Some(&TokKind::RBracket) => self.pos += 1,
                            _ => {
                                return Err(ArithError::new(
                                    "`[` has no matching `]`",
                                    self.peek_span(),
                                ));
                            }
                        }
                        indices.push(index);
                    }
                    Ok(ArithExpr::Subscript { root: name, indices })
                } else {
                    Ok(ArithExpr::Expansion(Expansion::Var(name)))
                }
            }
            Some(TokKind::LParen) => {
                self.enter()?;
                if self.peek() == Some(&TokKind::RParen) {
                    self.leave();
                    self.pos += 1;
                    return Err(ArithError::new("`()` has no expression", span));
                }
                let inner = self.parse_conditional()?;
                self.leave();
                match self.peek() {
                    Some(&TokKind::RParen) => {
                        self.pos += 1;
                        Ok(inner)
                    }
                    _ => Err(ArithError::new("`(` has no closing `)`", span)),
                }
            }
            Some(TokKind::RParen) => Err(ArithError::new("`)` has no matching `(`", span)),
            Some(other) => Err(ArithError::new(format!("`{other}` cannot start a value"), span)),
            None => Err(ArithError::new("`$(( ))` has no expression; write a number or an expression", span)),
        }
    }
}

fn int_from_magnitude(mag: u64, negative: bool, span: Range<usize>) -> Result<ArithExpr, ArithError> {
    let max = if negative { Parser::MIN_MAGNITUDE } else { i64::MAX as u64 };
    if mag > max {
        return Err(ArithError::new(format!("`{mag}` {INTEGER_OUT_OF_RANGE}"), span));
    }
    if negative && mag == Parser::MIN_MAGNITUDE {
        return Ok(ArithExpr::Int(i64::MIN));
    }
    Ok(ArithExpr::Int(if negative { -(mag as i64) } else { mag as i64 }))
}

pub(crate) fn parse(text: &str) -> Result<ArithExpr, ArithError> {
    let toks = tokenize(text)?;
    if toks.is_empty() {
        return Err(ArithError::new(
            "`$(( ))` has no expression; write a number or an expression",
            0..text.len(),
        ));
    }
    let end = text.len();
    let mut parser = Parser::new(toks, end, text);
    let expr = parser.parse_conditional()?;
    if let Some(extra) = parser.peek() {
        if extra == &TokKind::RParen {
            return Err(ArithError::new(
                format!("`)` has no matching `(` in `{text}`"),
                parser.peek_span(),
            ));
        }
        return Err(ArithError::new(format!("`{extra}` is not valid inside `$(( ))`"), parser.peek_span()));
    }
    Ok(expr)
}

// ═══════════════════════════════════════════════════════════════════
// Pure operator evaluation — shared by the sync and async walkers
// ═══════════════════════════════════════════════════════════════════

fn shift_count_error(count: i64) -> ArithError {
    ArithError::new(format!("shift count `{count}` is outside 0..=63"), 0..0)
}

fn overflow(l: i64, op: BinOp, r: i64) -> ArithError {
    ArithError::new(format!("`{l} {} {r}` does not fit in a 64-bit integer", op.symbol()), 0..0)
}

pub(crate) fn apply_binary(op: BinOp, l: i64, r: i64) -> Result<i64, ArithError> {
    match op {
        BinOp::Add => l.checked_add(r).ok_or_else(|| overflow(l, op, r)),
        BinOp::Sub => l.checked_sub(r).ok_or_else(|| overflow(l, op, r)),
        BinOp::Mul => l.checked_mul(r).ok_or_else(|| overflow(l, op, r)),
        BinOp::Div => {
            if r == 0 {
                return Err(ArithError::new(format!("`{l} / 0` divides by zero"), 0..0));
            }
            l.checked_div(r).ok_or_else(|| overflow(l, op, r))
        }
        BinOp::Rem => {
            if r == 0 {
                return Err(ArithError::new(format!("`{l} % 0` divides by zero"), 0..0));
            }
            // checked_rem returns None for MIN % -1; the answer is 0.
            if r == -1 {
                return Ok(0);
            }
            l.checked_rem(r).ok_or_else(|| overflow(l, op, r))
        }
        BinOp::Pow => {
            if r < 0 {
                return Err(ArithError::new(format!("exponent `{r}` is negative; use 0 or greater"), 0..0));
            }
            match l {
                0 => Ok(if r == 0 { 1 } else { 0 }),
                1 => Ok(1),
                -1 => Ok(if r % 2 == 0 { 1 } else { -1 }),
                _ => {
                    if r > u32::MAX as i64 {
                        return Err(overflow(l, op, r));
                    }
                    l.checked_pow(r as u32).ok_or_else(|| overflow(l, op, r))
                }
            }
        }
        BinOp::Shl => {
            if !(0..=63).contains(&r) {
                return Err(shift_count_error(r));
            }
            let factor: i128 = 1i128 << r;
            let result = (l as i128) * factor;
            i64::try_from(result).map_err(|_| overflow(l, op, r))
        }
        BinOp::Shr => {
            if !(0..=63).contains(&r) {
                return Err(shift_count_error(r));
            }
            Ok(l >> r)
        }
        BinOp::Lt => Ok((l < r) as i64),
        BinOp::Le => Ok((l <= r) as i64),
        BinOp::Gt => Ok((l > r) as i64),
        BinOp::Ge => Ok((l >= r) as i64),
        BinOp::Eq => Ok((l == r) as i64),
        BinOp::Ne => Ok((l != r) as i64),
        BinOp::BitAnd => Ok(l & r),
        BinOp::BitXor => Ok(l ^ r),
        BinOp::BitOr => Ok(l | r),
        BinOp::And | BinOp::Or => unreachable!("short-circuit ops are handled by the tree walk"),
    }
}

pub(crate) fn apply_unary(op: UnOp, v: i64) -> Result<i64, ArithError> {
    match op {
        UnOp::Neg => v
            .checked_neg()
            .ok_or_else(|| ArithError::new(format!("`-{v}` does not fit in a 64-bit integer"), 0..0)),
        UnOp::Not => Ok(if v == 0 { 1 } else { 0 }),
        UnOp::BitNot => Ok(!v),
    }
}

fn truthy(v: i64) -> bool {
    v != 0
}

// ═══════════════════════════════════════════════════════════════════
// Coercion (Value → i64)
// ═══════════════════════════════════════════════════════════════════

fn expression_like(s: &str) -> bool {
    let bytes = s.as_bytes();
    bytes.iter().enumerate().any(|(i, &b)| match b {
        b'+' | b'-' => i > 0,
        b'*' | b'/' | b'%' | b'<' | b'>' | b'=' | b'&' | b'|' | b'^' | b'!' | b'~' | b'?' | b':' | b'(' | b')' => true,
        _ => false,
    })
}

/// What a piece of text is, as a signed numeral in decimal/hex/`base#`
/// spelling — the core `parse_numeric_string` and the command-output
/// coercion below share, so the sign/leading-zero/tokenize logic exists
/// once.
enum Numeral {
    Ok(i64),
    Empty,
    ExpressionLike,
    LeadingZero,
    NotANumber,
    /// The tokenizer refused with a message that already names a fix
    /// (`0b101` → `2#101`, `1_000` → remove the `_`, `1e3` → integer-only).
    /// Carries that message so the caller can keep it instead of a generic
    /// "is not a number".
    NotANumberWithFix(String),
    OutOfRange,
}

fn read_numeral(text: &str) -> Numeral {
    let trimmed = text.trim();
    if trimmed.is_empty() {
        return Numeral::Empty;
    }
    if expression_like(trimmed) {
        return Numeral::ExpressionLike;
    }
    let (neg, digits) = match trimmed.strip_prefix('-') {
        Some(rest) => (true, rest),
        None => (false, trimmed.strip_prefix('+').unwrap_or(trimmed)),
    };
    if digits.is_empty() {
        return Numeral::NotANumber;
    }
    if crate::lexer::is_leading_zero_numeral(digits) {
        return Numeral::LeadingZero;
    }
    match tokenize(digits) {
        Ok(toks) if toks.len() == 1 => match &toks[0].kind {
            TokKind::Number(mag) => match int_from_magnitude(*mag, neg, 0..0) {
                Ok(ArithExpr::Int(n)) => Numeral::Ok(n),
                Ok(_) => unreachable!("int_from_magnitude only returns Int"),
                Err(_) => Numeral::OutOfRange,
            },
            _ => Numeral::NotANumber,
        },
        Ok(_) => Numeral::NotANumber,
        Err(e) => Numeral::NotANumberWithFix(e.message),
    }
}

fn parse_numeric_string(s: &str, name: &str) -> Result<i64, ArithError> {
    match read_numeral(s) {
        Numeral::Ok(n) => Ok(n),
        Numeral::Empty | Numeral::ExpressionLike => Err(ArithError::new(
            format!(
                "`{name}` holds `{s}`; a variable is a value, not an expression — write it inside `$(( ))`"
            ),
            0..0,
        )),
        Numeral::LeadingZero => Err(ArithError::new(
            format!(
                "`{name}` holds `{s}` (leading zero) — kaish reads no octal; write `10#${name}` for decimal or `8#${name}` for octal"
            ),
            0..0,
        )),
        Numeral::NotANumber => {
            Err(ArithError::new(format!("`{name}` holds `{s}`, which is not a number"), 0..0))
        }
        Numeral::NotANumberWithFix(fix) => {
            Err(ArithError::new(format!("`{name}` holds `{s}`; {fix}"), 0..0))
        }
        Numeral::OutOfRange => {
            Err(ArithError::new(format!("`{name}` holds `{s}`, outside the 64-bit range"), 0..0))
        }
    }
}

/// Coerce a `$(...)` operand's printed text — the command must print exactly
/// one integer.
pub(crate) fn parse_command_output(text: &str, cmd: &str) -> Result<i64, ArithError> {
    match read_numeral(text) {
        Numeral::Ok(n) => Ok(n),
        Numeral::Empty => Err(ArithError::new(
            format!("`{cmd}` printed nothing; the command must print one integer"),
            0..0,
        )),
        Numeral::ExpressionLike
        | Numeral::NotANumber
        | Numeral::NotANumberWithFix(_)
        | Numeral::LeadingZero
        | Numeral::OutOfRange => Err(ArithError::new(
            format!("`{cmd}` printed `{text}`; the command must print one integer"),
            0..0,
        )),
    }
}

pub(crate) fn value_to_arith(value: &Value, name: &str) -> Result<i64, ArithError> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Bool(b) => Ok(if *b { 1 } else { 0 }),
        Value::Float(f) => {
            if !f.is_finite() || f.fract() != 0.0 {
                Err(ArithError::new(format!("`{name}` holds `{f}`; arithmetic is integer-only"), 0..0))
            // The upper bound is the literal 2^63, not `i64::MAX as f64`:
            // i64::MAX (2^63 - 1) has no exact f64 representation at this
            // magnitude, so casting it to f64 ALSO rounds up to 2^63 — a
            // strict `>` against that rounded value let `f == 2^63`
            // through, and the saturating `as i64` below silently
            // returned i64::MAX. A float this large cannot distinguish
            // i64::MAX from one past it, so `>=` refuses the whole
            // ambiguous boundary instead of guessing.
            } else if *f < i64::MIN as f64 || *f >= 9_223_372_036_854_775_808.0 {
                Err(ArithError::new(format!("`{name}` holds `{f}`, outside the 64-bit range"), 0..0))
            } else {
                Ok(*f as i64)
            }
        }
        Value::String(s) => parse_numeric_string(s, name),
        Value::Null => Err(ArithError::new(format!("`{name}` is null; set it to an integer"), 0..0)),
        Value::Json(serde_json::Value::Array(_)) => {
            Err(ArithError::new(format!("`{name}` is a list; index a number field"), 0..0))
        }
        Value::Json(serde_json::Value::Object(_)) => {
            Err(ArithError::new(format!("`{name}` is a record; index a number field"), 0..0))
        }
        Value::Json(_) => Err(ArithError::new(
            format!("`{name}` holds `{}`, which is not a number", value_to_string(value)),
            0..0,
        )),
        Value::Bytes(b) => {
            Err(ArithError::new(format!("`{name}` holds {} bytes; decode them first", b.len()), 0..0))
        }
    }
}

pub(crate) fn unset_error(name: &str) -> ArithError {
    let message = match name {
        "RANDOM" => "`$RANDOM` has no value in kaish; write `$(random --max 100)`".to_string(),
        "SECONDS" => {
            "`$SECONDS` has no value in kaish; write `start=$(date +%s)` and `$(( $(date +%s) - start ))`"
                .to_string()
        }
        _ => format!("`{name}` is unset; set it before `$(( ))` or write `${{{name}:-0}}`"),
    };
    ArithError::new(message, 0..0)
}

pub(crate) fn resolve_var_sync(scope: &Scope, name: &str) -> Result<i64, ArithError> {
    // `$1`, `$2`, … reach the same variable slot bash gives them: text,
    // coerced by the same rules as any other string operand.
    if let Ok(index) = name.parse::<usize>() {
        return match scope.get_positional(index) {
            Some(s) => parse_numeric_string(s, name),
            None => Err(unset_error(name)),
        };
    }
    match scope.get(name) {
        Some(value) => value_to_arith(value, name),
        None => Err(unset_error(name)),
    }
}

pub(crate) fn braced_path_value(scope: &Scope, root: &str, brackets: &str) -> Result<Value, ArithError> {
    let raw = format!("${{{root}{brackets}}}");
    let path: VarPath = crate::parser::parse_varpath(&raw);
    scope.resolve_path(&path).map_err(|e| match e {
        crate::interpreter::PathError::UndefinedRoot(_) => unset_error(root),
        crate::interpreter::PathError::Absence(msg) | crate::interpreter::PathError::Shape(msg) => {
            ArithError::new(msg, 0..0)
        }
    })
}

/// The left operand of `${root[brackets]:-default}` inside `$(( ))`, classified
/// the way ordinary interpolation classifies it (decision A — `resolve_default`
/// in `interpreter/eval.rs`): `Ok(None)` means "select the default" (an unset
/// root, a missing key, an out-of-bounds index, `null`, or an empty string);
/// `Ok(Some(v))` is a present value to use as-is; `Err` is a shape error — a
/// wrong-typed access — that the default must NOT suppress and whose fallback
/// must NOT run.
///
/// The four `BracedDefault` call sites (sync/async × arithmetic-operand/
/// `base#`-text) all resolve through this one function so the contract can't
/// drift between them the way `.ok()` let it drift before.
pub(crate) fn braced_default_operand(
    scope: &Scope,
    root: &str,
    brackets: &str,
) -> Result<Option<Value>, ArithError> {
    let resolved: Result<Value, PathError> = if brackets.is_empty() {
        scope
            .get(root)
            .cloned()
            .ok_or_else(|| PathError::UndefinedRoot(root.to_string()))
    } else {
        let raw = format!("${{{root}{brackets}}}");
        let path: VarPath = crate::parser::parse_varpath(&raw);
        scope.resolve_path(&path)
    };
    match resolved {
        Ok(v) if value_defaults_on_emptiness(&v) => Ok(None),
        Ok(v) => Ok(Some(v)),
        Err(PathError::UndefinedRoot(_)) | Err(PathError::Absence(_)) => Ok(None),
        Err(PathError::Shape(msg)) => Err(ArithError::new(msg, 0..0)),
    }
}

fn subscript_path(root: &str, indices: &[i64]) -> VarPath {
    let mut raw = format!("${{{root}");
    for idx in indices {
        raw.push('[');
        raw.push_str(&idx.to_string());
        raw.push(']');
    }
    raw.push('}');
    crate::parser::parse_varpath(&raw)
}

pub(crate) fn resolve_subscript_sync(scope: &Scope, root: &str, indices: &[i64]) -> Result<i64, ArithError> {
    let path = subscript_path(root, indices);
    let value = scope.resolve_path(&path).map_err(|e| match e {
        crate::interpreter::PathError::UndefinedRoot(_) => unset_error(root),
        crate::interpreter::PathError::Absence(msg) | crate::interpreter::PathError::Shape(msg) => {
            ArithError::new(msg, 0..0)
        }
    })?;
    value_to_arith(&value, root)
}

/// The name and verb an error about `base#<expansion>`'s VALUE uses to
/// describe where the value came from — `` `m` holds `08` `` versus
/// `` `$(...)` printed `08` ``, matching the phrasing the rest of the
/// coercion errors already use for a variable vs. a command's output.
pub(crate) fn expansion_label(e: &Expansion) -> (String, &'static str) {
    match e {
        Expansion::Var(name) => (name.clone(), "holds"),
        Expansion::BracedPath { root, .. } | Expansion::BracedDefault { root, .. } => {
            (root.clone(), "holds")
        }
        Expansion::LastExitCode => ("$?".to_string(), "holds"),
        Expansion::CurrentPid => ("$$".to_string(), "holds"),
        Expansion::CommandSubst(_) => ("$(...)".to_string(), "printed"),
        Expansion::Nested(_) => ("$((...))".to_string(), "holds"),
    }
}

/// Read `text` as digits in `base` — the evaluation half of `base#<expansion>`
/// (`2#$BITS`, `10#$(date +%m)`). `text` is the expansion's rendered VALUE,
/// never re-coerced through the normal numeral rules first: that coercion is
/// exactly what a leading-zero string (`m="08"`) needs `10#$m` to escape, so
/// routing through it here would defeat the form's only purpose.
///
/// A sign in `text` is refused, not applied — the same rule as the literal
/// form (`16#-ff` is refused, naming `-16#ff`): the digits after `#` take
/// no sign, whether the `#` came with the sign in source text or the sign
/// arrived inside an expansion's value. `label`/`verb` name where the value
/// came from (see `expansion_label`) for that refusal's message.
pub(crate) fn based_value(base: u32, text: &str, label: &str, verb: &str) -> Result<i64, ArithError> {
    let trimmed = text.trim();
    if let Some(stripped) = trimmed.strip_prefix('-').or_else(|| trimmed.strip_prefix('+')) {
        let sign = &trimmed[..1];
        return Err(ArithError::new(
            format!(
                "`{label}` {verb} `{text}`; the digits after `#` take no sign — write `{sign}{base}#{stripped}`"
            ),
            0..0,
        ));
    }
    let digits = trimmed;
    if digits.is_empty() {
        return Err(ArithError::new(format!("`{text}` has no digits"), 0..0));
    }
    let mut mag: u64 = 0;
    for c in digits.chars() {
        if !c.is_ascii_alphanumeric() {
            return Err(ArithError::new(format!("`{c}` is not a digit in `{text}`; use digits valid for base {base}"), 0..0));
        }
        let digit_val = match c {
            '0'..='9' => c as u32 - '0' as u32,
            'a'..='z' => c as u32 - 'a' as u32 + 10,
            'A'..='Z' => c as u32 - 'A' as u32 + 10,
            _ => unreachable!(),
        };
        if digit_val >= base {
            return Err(ArithError::new(format!("`{c}` is not a digit in `{text}`; use digits valid for base {base}"), 0..0));
        }
        mag = mag
            .checked_mul(base as u64)
            .and_then(|m| m.checked_add(digit_val as u64))
            .ok_or_else(|| ArithError::new(format!("`{text}` {INTEGER_OUT_OF_RANGE}"), 0..0))?;
    }
    match int_from_magnitude(mag, false, 0..0)? {
        ArithExpr::Int(n) => Ok(n),
        _ => unreachable!(),
    }
}

// ═══════════════════════════════════════════════════════════════════
// Sync evaluator — used where no `$(...)` is reachable. Hits a
// `CommandSubst` leaf only if the walk actually reaches one; the caller
// is expected not to call this when `contains_command_subst()` is true.
// ═══════════════════════════════════════════════════════════════════

/// `contains_command_subst()` routes a tree holding this to the async
/// walker before eval_sync ever runs, so this is reachable only when a
/// caller invokes the sync evaluator directly without that check — the
/// message matches `EvalError::NoExecutor`'s wording for the same
/// situation elsewhere in the interpreter, not an internal name.
fn needs_async(what: &str) -> ArithError {
    ArithError::new(
        format!("`{what}` must be resolved by the async evaluator before sync evaluation"),
        0..0,
    )
}

fn resolve_expansion_sync(e: &Expansion, scope: &Scope) -> Result<i64, ArithError> {
    match e {
        Expansion::Var(name) => resolve_var_sync(scope, name),
        Expansion::BracedPath { root, brackets } => {
            let v = braced_path_value(scope, root, brackets)?;
            value_to_arith(&v, root)
        }
        Expansion::BracedDefault { root, brackets, default } => {
            match braced_default_operand(scope, root, brackets)? {
                None => {
                    let default_expr = parse(default)?;
                    eval_sync(&default_expr, scope)
                }
                Some(v) => value_to_arith(&v, root),
            }
        }
        Expansion::LastExitCode => Ok(scope.last_result().code),
        Expansion::CurrentPid => Ok(scope.pid() as i64),
        Expansion::CommandSubst(_) => Err(needs_async("$(...)")),
        Expansion::Nested(inner) => eval_sync(inner, scope),
    }
}

/// The expansion's rendered VALUE, for `base#<expansion>` — not its
/// arithmetically-coerced number. A `String` value's text passes through
/// untouched (leading zero included); other values render through the same
/// `value_to_string` interpolation uses.
pub(crate) fn expansion_text_sync(e: &Expansion, scope: &Scope) -> Result<String, ArithError> {
    match e {
        Expansion::Var(name) => {
            if let Ok(index) = name.parse::<usize>() {
                return match scope.get_positional(index) {
                    Some(s) => Ok(s.to_string()),
                    None => Err(unset_error(name)),
                };
            }
            match scope.get(name) {
                Some(v) => Ok(value_to_string(v)),
                None => Err(unset_error(name)),
            }
        }
        Expansion::BracedPath { root, brackets } => {
            braced_path_value(scope, root, brackets).map(|v| value_to_string(&v))
        }
        Expansion::BracedDefault { root, brackets, default } => {
            match braced_default_operand(scope, root, brackets)? {
                // A default that is itself a single expansion (`$(cmd)`,
                // `$var`, …) stays in TEXT mode — `10#${m:-$(date +%m)}`
                // needs the same "read raw digits" treatment `10#$m` gets,
                // not the leading-zero refusal a full arithmetic operand
                // would apply. A default with real operators (`1 + 2`) is
                // genuinely an expression and is evaluated as one.
                None => match parse(default)? {
                    ArithExpr::Expansion(e) => expansion_text_sync(&e, scope),
                    default_expr => Ok(eval_sync(&default_expr, scope)?.to_string()),
                },
                Some(v) => Ok(value_to_string(&v)),
            }
        }
        Expansion::LastExitCode => Ok(scope.last_result().code.to_string()),
        Expansion::CurrentPid => Ok(scope.pid().to_string()),
        Expansion::CommandSubst(_) => Err(needs_async("$(...)")),
        Expansion::Nested(inner) => Ok(eval_sync(inner, scope)?.to_string()),
    }
}

fn resolve_based_sync(base: u32, e: &Expansion, scope: &Scope) -> Result<i64, ArithError> {
    let text = expansion_text_sync(e, scope)?;
    let (label, verb) = expansion_label(e);
    based_value(base, &text, &label, verb)
}

pub(crate) fn eval_sync(expr: &ArithExpr, scope: &Scope) -> Result<i64, ArithError> {
    match expr {
        ArithExpr::Int(n) => Ok(*n),
        ArithExpr::Expansion(e) => resolve_expansion_sync(e, scope),
        ArithExpr::Subscript { root, indices } => {
            let mut idx_vals = Vec::with_capacity(indices.len());
            for idx in indices {
                idx_vals.push(eval_sync(idx, scope)?);
            }
            resolve_subscript_sync(scope, root, &idx_vals)
        }
        ArithExpr::BasedExpansion { base, expansion } => resolve_based_sync(*base, expansion, scope),
        ArithExpr::Unary { op, operand } => apply_unary(*op, eval_sync(operand, scope)?),
        ArithExpr::Binary { op: BinOp::And, left, right } => {
            let l = eval_sync(left, scope)?;
            if !truthy(l) { Ok(0) } else { Ok(if truthy(eval_sync(right, scope)?) { 1 } else { 0 }) }
        }
        ArithExpr::Binary { op: BinOp::Or, left, right } => {
            let l = eval_sync(left, scope)?;
            if truthy(l) { Ok(1) } else { Ok(if truthy(eval_sync(right, scope)?) { 1 } else { 0 }) }
        }
        ArithExpr::Binary { op, left, right } => {
            apply_binary(*op, eval_sync(left, scope)?, eval_sync(right, scope)?)
        }
        ArithExpr::Ternary { cond, then_branch, else_branch } => {
            if truthy(eval_sync(cond, scope)?) {
                eval_sync(then_branch, scope)
            } else {
                eval_sync(else_branch, scope)
            }
        }
    }
}

/// Tokenize, parse, and evaluate `text` (the content of `$(( ))`) with no
/// `$(...)` support — the fast path used where an async evaluator isn't
/// available. A reachable `$(...)` errors loudly rather than silently
/// resolving to nothing.
pub fn eval_arithmetic(text: &str, scope: &Scope) -> Result<i64, ArithError> {
    let expr = parse(text)?;
    eval_sync(&expr, scope)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn eval(expr: &str) -> i64 {
        let scope = Scope::new();
        eval_arithmetic(expr, &scope).unwrap_or_else(|e| panic!("eval {expr:?} failed: {e}"))
    }

    fn err(expr: &str) -> String {
        let scope = Scope::new();
        eval_arithmetic(expr, &scope).expect_err("expected an error").message
    }

    fn eval_with(expr: &str, setup: impl FnOnce(&mut Scope)) -> i64 {
        let mut scope = Scope::new();
        setup(&mut scope);
        eval_arithmetic(expr, &scope).unwrap_or_else(|e| panic!("eval {expr:?} failed: {e}"))
    }

    fn err_with(expr: &str, setup: impl FnOnce(&mut Scope)) -> String {
        let mut scope = Scope::new();
        setup(&mut scope);
        eval_arithmetic(expr, &scope).expect_err("expected an error").message
    }

    // ── literals & bases ──
    #[test]
    fn decimal() {
        assert_eq!(eval("42"), 42);
        assert_eq!(eval("0"), 0);
    }

    #[test]
    fn hex() {
        assert_eq!(eval("0xff"), 255);
        assert_eq!(eval("0XFF"), 255);
    }

    #[test]
    fn based() {
        assert_eq!(eval("16#ff"), 255);
        assert_eq!(eval("8#17"), 15);
        assert_eq!(eval("2#1011"), 11);
        assert_eq!(eval("36#z"), 35);
    }

    #[test]
    fn negative_hex_and_based() {
        assert_eq!(eval("-0xff"), -255);
        assert_eq!(eval("- 16#ff"), -255);
    }

    #[test]
    fn sign_after_hash_is_an_error() {
        let msg = err("16#-ff");
        assert!(msg.contains("puts") && msg.contains('#'), "{msg}");
    }

    #[test]
    fn leading_zero_is_an_error() {
        let msg = err("010");
        assert!(msg.contains("leading zero"), "{msg}");
        assert!(msg.contains("8#10"), "{msg}");
        assert!(msg.contains('9') || msg.contains("10"), "{msg}");
    }

    #[test]
    fn zero_alone_is_fine() {
        assert_eq!(eval("0 + 1"), 1);
    }

    #[test]
    fn zero_b_and_zero_o_are_not_kaish_spellings() {
        let msg = err("0b101");
        assert!(msg.contains("2#101"), "{msg}");
        let msg = err("0o17");
        assert!(msg.contains("8#17"), "{msg}");
    }

    #[test]
    fn base_out_of_range() {
        let msg = err("1#5");
        assert!(msg.contains("outside 2..=36"), "{msg}");
        let msg = err("37#5");
        assert!(msg.contains("outside 2..=36"), "{msg}");
    }

    #[test]
    fn bad_digit_for_base() {
        let msg = err("2#5");
        assert!(msg.contains("not a digit"), "{msg}");
    }

    // A base of the form k*2^32 + b (b in 2..=36) used to truncate through
    // `as u32` BEFORE the range check, landing in range and silently
    // computing as base b instead of refusing. Covers the typed literal,
    // the `base#$VAR` expansion form, and a string variable holding the
    // same spelling.
    #[test]
    fn base_out_of_range_survives_u32_truncation() {
        for (expr, true_base) in [
            ("4294967298#10", "4294967298"),
            ("4294967299#10", "4294967299"),
            ("4294967330#10", "4294967330"),
            ("8589934594#10", "8589934594"),
        ] {
            let msg = err(expr);
            assert!(msg.contains("outside 2..=36"), "{expr}: {msg}");
            assert!(msg.contains(true_base), "{expr}: {msg}");
        }
    }

    #[test]
    fn based_expansion_out_of_range_survives_u32_truncation() {
        let msg = err_with("4294967298#$d", |s| s.set("d", Value::String("10".to_string())));
        assert!(msg.contains("outside 2..=36"), "{msg}");
        assert!(msg.contains("4294967298"), "{msg}");
    }

    #[test]
    fn string_variable_based_literal_base_overflow_does_not_compute() {
        let mut scope = Scope::new();
        scope.set("x", Value::String("4294967298#10".to_string()));
        assert!(
            eval_arithmetic("x", &scope).is_err(),
            "a u32-truncated out-of-range base must refuse, not silently compute a value"
        );
    }

    #[test]
    fn no_digits_after_prefix() {
        let msg = err("0x");
        assert!(msg.contains("no digits"), "{msg}");
        let msg = err("16#");
        assert!(msg.contains("no digits"), "{msg}");
    }

    #[test]
    fn underscore_in_literal_quotes_the_whole_literal() {
        // Not `1_`: the message names the literal the user wrote.
        assert!(err("1_000").contains("`1_000`"), "{}", err("1_000"));
        assert!(err("12_345_6").contains("`12_345_6`"), "{}", err("12_345_6"));
        assert!(err("16#f_f").contains("`16#f_f`"), "{}", err("16#f_f"));
    }

    #[test]
    fn out_of_range_literal() {
        let msg = err("9223372036854775808 + 1");
        assert!(msg.contains("does not fit"), "{msg}");
    }

    #[test]
    fn min_literal_only_as_direct_unary_operand() {
        assert_eq!(eval("-9223372036854775808"), i64::MIN);
        let msg = err("9223372036854775808");
        assert!(msg.contains("does not fit"), "{msg}");
        let msg = err("- -9223372036854775808");
        assert!(msg.contains("does not fit"), "{msg}");
    }

    // ── operators & precedence ──
    #[test]
    fn basic_ops() {
        assert_eq!(eval("5 + 3 * 2"), 11);
        assert_eq!(eval("10 / 3"), 3);
        assert_eq!(eval("-7 % 3"), -1);
        assert_eq!(eval("2 ** 10"), 1024);
    }

    #[test]
    fn precedence_examples() {
        assert_eq!(eval("1 << 2 + 1"), 8);
        assert_eq!(eval("5 & 3 == 3"), 1);
        assert_eq!(eval("2 ** 3 ** 2"), 512);
        assert_eq!(eval("-2 ** 2"), 4);
        assert_eq!(eval("1 ? 2 : 3 ? 4 : 5"), 2);
    }

    #[test]
    fn comparisons_return_one_or_zero() {
        assert_eq!(eval("5 > 3"), 1);
        assert_eq!(eval("3 > 5"), 0);
    }

    #[test]
    fn bitwise() {
        assert_eq!(eval("6 & 3"), 2);
        assert_eq!(eval("6 | 1"), 7);
        assert_eq!(eval("6 ^ 3"), 5);
        assert_eq!(eval("~0"), -1);
    }

    #[test]
    fn shifts() {
        assert_eq!(eval("1 << 4"), 16);
        assert_eq!(eval("-8 >> 1"), -4);
    }

    #[test]
    fn shift_count_out_of_range() {
        let msg = err("1 << 64");
        assert!(msg.contains("outside 0..=63"), "{msg}");
        let msg = err("1 << -1");
        assert!(msg.contains("outside 0..=63"), "{msg}");
    }

    #[test]
    fn short_circuit_and_or() {
        assert_eq!(eval("0 && 1"), 0);
        assert_eq!(eval("1 && 1"), 1);
        assert_eq!(eval("1 || 0"), 1);
        assert_eq!(eval("0 || 0"), 0);
    }

    #[test]
    fn ternary_selects_unnormalized_value() {
        assert_eq!(eval("1 ? 42 : 7"), 42);
        assert_eq!(eval("0 ? 42 : 7"), 7);
    }

    // ── overflow ──
    #[test]
    fn overflow_each_op() {
        assert!(err("9223372036854775807 + 1").contains("does not fit"));
        assert!(err("-9223372036854775808 - 1").contains("does not fit"));
        assert!(err("9223372036854775807 * 2").contains("does not fit"));
        assert!(err("-9223372036854775808 / -1").contains("does not fit"));
        assert!(err("2 ** 63").contains("does not fit"));
        assert!(err("1 << 63").contains("does not fit"));
    }

    #[test]
    fn division_and_modulo_by_zero() {
        assert!(err("10 / 0").contains("divides by zero"));
        assert!(err("10 % 0").contains("divides by zero"));
    }

    #[test]
    fn division_truncates_toward_zero() {
        assert_eq!(eval("7 / 2"), 3);
        assert_eq!(eval("-7 / 2"), -3);
    }

    #[test]
    fn negative_exponent() {
        assert!(err("2 ** -1").contains("negative"));
    }

    // ── variables ──
    #[test]
    fn bare_and_dollar_variable() {
        assert_eq!(eval_with("count + 1", |s| s.set("count", Value::Int(4))), 5);
        assert_eq!(eval_with("$count + 1", |s| s.set("count", Value::Int(4))), 5);
    }

    #[test]
    fn unset_variable_is_an_error() {
        let msg = err("missing + 1");
        assert!(msg.contains("unset"), "{msg}");
        assert!(msg.contains(":-0"), "{msg}");
    }

    #[test]
    fn random_and_seconds_name_their_fix() {
        let msg = err("RANDOM % 10");
        assert!(msg.contains("random --max"), "{msg}");
        let msg = err("SECONDS");
        assert!(msg.contains("date +%s"), "{msg}");
    }

    #[test]
    fn null_variable_is_an_error() {
        let msg = err_with("x", |s| s.set("x", Value::Null));
        assert!(msg.contains("null"), "{msg}");
    }

    #[test]
    fn float_variable_errors() {
        let msg = err_with("x", |s| s.set("x", Value::Float(2.7)));
        assert!(msg.contains("integer-only"), "{msg}");
    }

    #[test]
    fn integral_float_coerces() {
        assert_eq!(eval_with("x + 1", |s| s.set("x", Value::Float(100.0))), 101);
    }

    #[test]
    fn float_at_2_63_is_out_of_range() {
        // i64::MAX has no exact f64 representation and rounds UP to 2^63
        // when cast — the same rounding that makes 2^63 itself look like
        // it fits if the bound is compared as `i64::MAX as f64`.
        let msg = err_with("x", |s| s.set("x", Value::Float(9223372036854775808.0)));
        assert!(msg.contains("64-bit"), "{msg}");
    }

    #[test]
    fn float_at_min_still_converts() {
        assert_eq!(eval_with("x", |s| s.set("x", Value::Float(-9223372036854775808.0))), i64::MIN);
    }

    #[test]
    fn negative_zero_float_converts_to_zero() {
        assert_eq!(eval_with("x", |s| s.set("x", Value::Float(-0.0))), 0);
    }

    #[test]
    fn string_value_is_parsed() {
        assert_eq!(eval_with("x", |s| s.set("x", Value::String("0xff".to_string()))), 255);
        assert_eq!(eval_with("mask & 16#0f", |s| s.set("mask", Value::String("0xff".to_string()))), 15);
    }

    #[test]
    fn string_with_leading_zero_names_the_fix() {
        let msg = err_with("x", |s| s.set("x", Value::String("08".to_string())));
        assert!(msg.contains("10#$x") || msg.contains("leading zero"), "{msg}");
    }

    #[test]
    fn string_expression_names_the_fix() {
        let msg = err_with("x", |s| s.set("x", Value::String("1 + 2".to_string())));
        assert!(msg.contains("not an expression"), "{msg}");
    }

    #[test]
    fn string_non_numeric_is_an_error() {
        let msg = err_with("x", |s| s.set("x", Value::String("abc".to_string())));
        assert!(msg.contains("not a number"), "{msg}");
    }

    // `read_numeral` used to flatten every tokenizer `Err` to a generic
    // "is not a number", discarding a fix the tokenizer already named.
    // These three spellings each carry a real fix; `abc` above has none
    // and must keep the generic message.
    #[test]
    fn string_binary_spelling_names_the_fix() {
        let msg = err_with("x", |s| s.set("x", Value::String("0b101".to_string())));
        assert!(msg.contains("`x`") && msg.contains("0b101"), "{msg}");
        assert!(msg.contains("2#101"), "{msg}");
    }

    #[test]
    fn string_underscore_digit_group_names_the_fix() {
        let msg = err_with("x", |s| s.set("x", Value::String("1_000".to_string())));
        assert!(msg.contains("`x`") && msg.contains("1_000"), "{msg}");
        assert!(msg.contains("remove it"), "{msg}");
    }

    #[test]
    fn string_float_spelling_names_the_fix() {
        let msg = err_with("x", |s| s.set("x", Value::String("1e3".to_string())));
        assert!(msg.contains("`x`") && msg.contains("1e3"), "{msg}");
        assert!(msg.contains("integer-only"), "{msg}");
    }

    #[test]
    fn list_record_and_bytes_error() {
        let msg = err_with("x", |s| s.set("x", Value::Json(serde_json::json!([1, 2]))));
        assert!(msg.contains("list"), "{msg}");
        let msg = err_with("x", |s| s.set("x", Value::Json(serde_json::json!({"a": 1}))));
        assert!(msg.contains("record"), "{msg}");
        let msg = err_with("x", |s| s.set("x", Value::Bytes(vec![0xff, 0xfe, 0x00, 0x01])));
        assert!(msg.contains("bytes"), "{msg}");
    }

    #[test]
    fn last_exit_code_and_pid() {
        let mut scope = Scope::new();
        scope.set_last_result(crate::interpreter::ExecResult::success("x").with_code(3));
        assert_eq!(eval_arithmetic("$?", &scope).unwrap(), 3);
        assert_eq!(eval_arithmetic("$$", &scope).unwrap(), scope.pid() as i64);
    }

    // ── subscripts (Decision B: bare `[...]` is an expression) ──
    #[test]
    fn bare_subscript_is_a_variable_expression() {
        let r = eval_with("xs[i]", |s| {
            s.set("xs", Value::Json(serde_json::json!([10, 20, 30])));
            s.set("i", Value::Int(1));
        });
        assert_eq!(r, 20);
    }

    #[test]
    fn bare_subscript_literal_and_expression_index() {
        let r = eval_with("xs[0] + 1", |s| s.set("xs", Value::Json(serde_json::json!([10, 20, 30]))));
        assert_eq!(r, 11);
        let r = eval_with("xs[i + 1]", |s| {
            s.set("xs", Value::Json(serde_json::json!([10, 20, 30])));
            s.set("i", Value::Int(0));
        });
        assert_eq!(r, 20);
    }

    #[test]
    fn braced_path_reads_a_literal_key() {
        let r = eval_with("${c[port]}", |s| s.set("c", Value::Json(serde_json::json!({"port": 8080}))));
        assert_eq!(r, 8080);
    }

    // ── default expansion ──
    #[test]
    fn default_used_when_unset() {
        assert_eq!(eval("${limit:-0} + 1"), 1);
    }

    #[test]
    fn default_not_used_when_set() {
        assert_eq!(eval_with("${limit:-0} + 1", |s| s.set("limit", Value::Int(9))), 10);
    }

    // ── nested arithmetic ──
    #[test]
    fn nested_arithmetic() {
        assert_eq!(eval("$(( 1 + 2 )) * 4"), 12);
    }

    #[test]
    fn newline_inside_is_whitespace() {
        assert_eq!(eval("1 +\n2"), 3);
    }

    // ── structural errors ──
    #[test]
    fn empty_is_an_error() {
        let msg = err("");
        assert!(msg.contains("no expression"), "{msg}");
    }

    #[test]
    fn empty_group_is_an_error() {
        let msg = err("()");
        assert!(msg.contains("no expression"), "{msg}");
    }

    #[test]
    fn missing_close_paren() {
        let msg = err("(1 + 2");
        assert!(msg.contains("closing"), "{msg}");
    }

    #[test]
    fn extra_close_paren() {
        let msg = err("1 + 2)");
        assert!(msg.contains("matching"), "{msg}");
    }

    #[test]
    fn ternary_without_colon() {
        let msg = err("1 ? 2");
        assert!(msg.contains(':'), "{msg}");
    }

    #[test]
    fn not_operators_are_diagnosed() {
        assert!(err("1 <<< 2").contains("here-string"));
        assert!(err("1 >>> 2").contains(">>"));
        assert!(err("x = 5").contains("assigns"));
        assert!(err("x += 1").contains("assigns"));
        assert!(err("x++").contains("assigns"));
        assert!(err("1, 2").contains("one expression"));
    }

    #[test]
    fn assignment_errors_name_the_real_tokens_not_a_placeholder() {
        assert_eq!(err("x++"), "`x++` assigns inside `$(( ))`; write `x=$((x + 1))`");
        assert_eq!(err("++x"), "`++x` assigns inside `$(( ))`; write `x=$((x + 1))`");
        assert_eq!(err("x--"), "`x--` assigns inside `$(( ))`; write `x=$((x - 1))`");
        assert_eq!(err("--x"), "`--x` assigns inside `$(( ))`; write `x=$((x - 1))`");
        assert_eq!(err("x += 2"), "`x += 2` assigns inside `$(( ))`; write `x=$((x + 2))`");
        assert_eq!(err("x -= 3"), "`x -= 3` assigns inside `$(( ))`; write `x=$((x - 3))`");
        assert_eq!(err("x = 2"), "`x = 2` assigns inside `$(( ))`; write `x=2`, or `==` to compare");
    }

    #[test]
    fn missing_operand_names_the_source_consumed_so_far() {
        assert_eq!(
            err("1 + "),
            "`+` has no right operand in `1 + `; add an integer expression after `+`"
        );
        assert_eq!(err(" + "), "`+` has no operand; add an integer expression after `+`");
    }

    #[test]
    fn a_leading_zero_base_is_refused() {
        assert_eq!(err("08#17"), "`08` is not a base spelling; write the base without a leading zero");
        assert_eq!(err("010#5"), "`010` is not a base spelling; write the base without a leading zero");
    }

    #[test]
    fn based_expansion_digits_take_no_sign() {
        let msg = err_with("16#$d", |s| s.set("d", Value::String("-ff".to_string())));
        assert_eq!(msg, "`d` holds `-ff`; the digits after `#` take no sign — write `-16#ff`");
    }

    #[test]
    fn depth_cap() {
        let mut src = String::new();
        for _ in 0..300 {
            src.push('(');
        }
        src.push('1');
        for _ in 0..300 {
            src.push(')');
        }
        let msg = err(&src);
        assert!(msg.contains("256"), "{msg}");
    }
}

