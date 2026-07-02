//! Arithmetic expression evaluation for shell-style `$(( ))` expressions.
//!
//! Supports:
//! - Integer arithmetic: `+`, `-`, `*`, `/`, `%`
//! - Comparison operators: `>`, `<`, `>=`, `<=`, `==`, `!=` (return 1 or 0)
//! - Parentheses for grouping: `(expr)`
//! - Variable references: `$VAR` or bare `VAR`
//! - Integer literals
//!
//! Does NOT support:
//! - Floating point (pipe to `jq` for float math)
//! - Bitwise operations (shell-ism we're skipping)
//! - Assignment within expressions (confusing)

use crate::interpreter::Scope;
use crate::ast::{Value, VarPath, VarSegment};
use anyhow::{bail, Context, Result};

/// Evaluate an arithmetic expression string.
///
/// The expression should be the content between `$((` and `))`.
///
/// # Example
/// ```ignore
/// let scope = Scope::new();
/// scope.set("X", Value::Int(5));
/// let result = eval_arithmetic("X + 3", &scope)?;
/// assert_eq!(result, 8);
/// ```
pub fn eval_arithmetic(expr: &str, scope: &Scope) -> Result<i64> {
    let mut parser = ArithParser::new(expr, scope);
    let result = parser.parse_comparison()?;
    parser.expect_end()?;
    Ok(result)
}

/// Simple recursive descent parser for arithmetic expressions.
struct ArithParser<'a> {
    input: &'a str,
    pos: usize,
    scope: &'a Scope,
}

impl<'a> ArithParser<'a> {
    fn new(input: &'a str, scope: &'a Scope) -> Self {
        Self { input, pos: 0, scope }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len() {
            let ch = self.input.as_bytes()[self.pos];
            if ch == b' ' || ch == b'\t' {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.skip_whitespace();
        self.input[self.pos..].chars().next()
    }

    fn advance(&mut self) -> Option<char> {
        self.skip_whitespace();
        let ch = self.input[self.pos..].chars().next()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    /// Peek at the character n positions ahead (0 = current after whitespace skip).
    fn peek_ahead(&mut self, n: usize) -> Option<char> {
        self.skip_whitespace();
        self.input[self.pos..].chars().nth(n)
    }

    fn expect_end(&mut self) -> Result<()> {
        self.skip_whitespace();
        if self.pos < self.input.len() {
            bail!("unexpected characters at end of arithmetic expression: {:?}",
                  &self.input[self.pos..]);
        }
        Ok(())
    }

    /// Parse comparison operators (lowest precedence): >, <, >=, <=, ==, !=
    /// Returns 1 for true, 0 for false.
    fn parse_comparison(&mut self) -> Result<i64> {
        let mut left = self.parse_expr()?;

        loop {
            self.skip_whitespace();
            match (self.peek_ahead(0), self.peek_ahead(1)) {
                // Two-character operators must be checked first
                (Some('>'), Some('=')) => {
                    self.advance(); // consume '>'
                    self.advance(); // consume '='
                    let right = self.parse_expr()?;
                    left = if left >= right { 1 } else { 0 };
                }
                (Some('<'), Some('=')) => {
                    self.advance(); // consume '<'
                    self.advance(); // consume '='
                    let right = self.parse_expr()?;
                    left = if left <= right { 1 } else { 0 };
                }
                (Some('='), Some('=')) => {
                    self.advance(); // consume '='
                    self.advance(); // consume '='
                    let right = self.parse_expr()?;
                    left = if left == right { 1 } else { 0 };
                }
                (Some('!'), Some('=')) => {
                    self.advance(); // consume '!'
                    self.advance(); // consume '='
                    let right = self.parse_expr()?;
                    left = if left != right { 1 } else { 0 };
                }
                // Single-character operators
                (Some('>'), _) => {
                    self.advance(); // consume '>'
                    let right = self.parse_expr()?;
                    left = if left > right { 1 } else { 0 };
                }
                (Some('<'), _) => {
                    self.advance(); // consume '<'
                    let right = self.parse_expr()?;
                    left = if left < right { 1 } else { 0 };
                }
                _ => break,
            }
        }

        Ok(left)
    }

    /// Parse an expression: handles + and - (lowest precedence)
    fn parse_expr(&mut self) -> Result<i64> {
        let mut left = self.parse_term()?;

        loop {
            match self.peek() {
                Some('+') => {
                    self.advance();
                    let right = self.parse_term()?;
                    left = left.checked_add(right)
                        .context("arithmetic overflow in addition")?;
                }
                Some('-') => {
                    self.advance();
                    let right = self.parse_term()?;
                    left = left.checked_sub(right)
                        .context("arithmetic overflow in subtraction")?;
                }
                _ => break,
            }
        }

        Ok(left)
    }

    /// Parse a term: handles * / % (higher precedence)
    fn parse_term(&mut self) -> Result<i64> {
        let mut left = self.parse_unary()?;

        loop {
            match self.peek() {
                Some('*') => {
                    self.advance();
                    let right = self.parse_unary()?;
                    left = left.checked_mul(right)
                        .context("arithmetic overflow in multiplication")?;
                }
                Some('/') => {
                    self.advance();
                    let right = self.parse_unary()?;
                    if right == 0 {
                        bail!("division by zero");
                    }
                    left = left.checked_div(right)
                        .context("arithmetic overflow in division")?;
                }
                Some('%') => {
                    self.advance();
                    let right = self.parse_unary()?;
                    if right == 0 {
                        bail!("modulo by zero");
                    }
                    left = left.checked_rem(right)
                        .context("arithmetic overflow in modulo")?;
                }
                _ => break,
            }
        }

        Ok(left)
    }

    /// Parse unary operators: + and - prefix
    fn parse_unary(&mut self) -> Result<i64> {
        match self.peek() {
            Some('+') => {
                self.advance();
                self.parse_unary()
            }
            Some('-') => {
                self.advance();
                let val = self.parse_unary()?;
                val.checked_neg().context("arithmetic overflow in negation")
            }
            _ => self.parse_primary(),
        }
    }

    /// Parse primary: numbers, variables, parenthesized expressions
    fn parse_primary(&mut self) -> Result<i64> {
        self.skip_whitespace();

        match self.peek() {
            Some('(') => {
                self.advance(); // consume '('
                let val = self.parse_expr()?;
                match self.peek() {
                    Some(')') => {
                        self.advance();
                        Ok(val)
                    }
                    _ => bail!("expected ')' in arithmetic expression"),
                }
            }
            Some('$') => {
                // $VAR, ${VAR}, $?, $$, ${?}, ${$} syntax
                self.advance(); // consume '$'

                // Special case: $? (last exit code)
                if self.peek() == Some('?') {
                    self.advance(); // consume '?'
                    return Ok(self.scope.last_result().code);
                }

                // Special case: $$ (current PID)
                if self.peek() == Some('$') {
                    self.advance(); // consume second '$'
                    return Ok(self.scope.pid() as i64);
                }

                let var_name = if self.peek() == Some('{') {
                    self.advance(); // consume '{'

                    // Special case: ${?} (last exit code, braced form)
                    if self.peek() == Some('?') {
                        self.advance(); // consume '?'
                        if self.peek() != Some('}') {
                            bail!("expected '}}' after ${{?}} in arithmetic");
                        }
                        self.advance(); // consume '}'
                        return Ok(self.scope.last_result().code);
                    }

                    // Special case: ${$} (current PID, braced form)
                    if self.peek() == Some('$') {
                        self.advance(); // consume '$'
                        if self.peek() != Some('}') {
                            bail!("expected '}}' after ${{$}} in arithmetic");
                        }
                        self.advance(); // consume '}'
                        return Ok(self.scope.pid() as i64);
                    }

                    let name = self.parse_identifier()?;
                    // Collection subscript path: `${p[port]}`, `${a[b][0]}`.
                    if self.peek() == Some('[') {
                        return self.eval_braced_path(&name);
                    }
                    if self.peek() != Some('}') {
                        bail!("expected '}}' after variable name in arithmetic");
                    }
                    self.advance(); // consume '}'
                    name
                } else {
                    self.parse_identifier()?
                };
                self.get_var_value(&var_name)
            }
            Some(c) if c.is_ascii_digit() => {
                self.parse_number()
            }
            Some(c) if c.is_ascii_alphabetic() || c == '_' => {
                // Bare variable name (bash allows this in $(( )))
                let var_name = self.parse_identifier()?;
                if self.peek() == Some('[') {
                    // Bare subscript path `xs[i]` — decision B.
                    return self.eval_bare_subscript_path(&var_name);
                }
                self.get_var_value(&var_name)
            }
            Some(c) => bail!("unexpected character in arithmetic expression: {:?}", c),
            None => bail!("unexpected end of arithmetic expression"),
        }
    }

    fn parse_number(&mut self) -> Result<i64> {
        let start = self.pos;
        while self.pos < self.input.len() {
            let ch = self.input.as_bytes()[self.pos];
            if ch.is_ascii_digit() {
                self.pos += 1;
            } else {
                break;
            }
        }
        let num_str = &self.input[start..self.pos];
        num_str.parse().context("invalid number in arithmetic expression")
    }

    fn parse_identifier(&mut self) -> Result<String> {
        let start = self.pos;
        while self.pos < self.input.len() {
            let ch = self.input.as_bytes()[self.pos];
            if ch.is_ascii_alphanumeric() || ch == b'_' {
                self.pos += 1;
            } else {
                break;
            }
        }
        if start == self.pos {
            bail!("expected identifier in arithmetic expression");
        }
        Ok(self.input[start..self.pos].to_string())
    }

    fn get_var_value(&self, name: &str) -> Result<i64> {
        // Check for positional parameters ($0, $1, $2, ... $9, etc.)
        // Name is just the digits when called from `$1` or `${1}` parsing
        if let Ok(index) = name.parse::<usize>() {
            if let Some(pos_val) = self.scope.get_positional(index) {
                return pos_val.parse().with_context(|| {
                    format!("${} has non-numeric value: {:?}", index, pos_val)
                });
            }
            return Ok(0); // Unset positional defaults to 0
        }

        // Regular variable lookup
        match self.scope.get(name).cloned() {
            Some(value) => self.value_to_arith(&value, name),
            None => Ok(0), // Unset variables default to 0 in arithmetic
        }
    }

    /// Resolve a subscripted variable path (`${p[port]}`) and coerce to an
    /// integer. Reuses the real path resolver, so scalar unwrap and the loud
    /// path errors are identical to `${p[port]}` outside arithmetic.
    fn eval_braced_path(&mut self, root: &str) -> Result<i64> {
        let mut brackets = String::new();
        while self.peek() == Some('[') {
            brackets.push('[');
            self.advance(); // consume '['
            let mut depth = 1;
            while depth > 0 {
                match self.advance() {
                    Some('[') => {
                        depth += 1;
                        brackets.push('[');
                    }
                    Some(']') => {
                        depth -= 1;
                        brackets.push(']');
                    }
                    Some(c) => brackets.push(c),
                    None => bail!("unterminated subscript in arithmetic"),
                }
            }
        }
        if self.peek() != Some('}') {
            bail!("expected '}}' after subscripted variable in arithmetic");
        }
        self.advance(); // consume '}'

        let raw = format!("${{{root}{brackets}}}");
        let path = crate::parser::parse_varpath(&raw);
        let value = self.scope.resolve_path(&path).map_err(|e| match e {
            crate::interpreter::PathError::UndefinedRoot(_) => {
                anyhow::anyhow!("undefined variable in arithmetic: {root}")
            }
            crate::interpreter::PathError::Absence(msg)
            | crate::interpreter::PathError::Shape(msg) => anyhow::anyhow!(msg),
        })?;
        self.value_to_arith(&value, root)
    }

    /// Resolve a BARE subscripted path in arithmetic (`xs[i]`, `xs[0]`,
    /// `xs[i+1]`, `xs[-1]`) and coerce to an integer.
    ///
    /// Decision B: inside `$(( … ))` a bracket's contents are a numeric
    /// expression, so a bareword subscript is the VARIABLE `i` (evaluated here),
    /// NOT the literal key — the exact opposite of the interpolation form
    /// `${xs[i]}`, which stays a literal key via `eval_braced_path`. Each
    /// subscript is evaluated as a nested arithmetic expression to an integer
    /// index; chained subscripts (`grid[i][j]`) walk left to right.
    fn eval_bare_subscript_path(&mut self, root: &str) -> Result<i64> {
        let mut segments = vec![VarSegment::Field(root.to_string())];
        while self.peek() == Some('[') {
            self.advance(); // consume '['
            let index = self.parse_comparison()?; // the inner is a numeric expr
            self.skip_whitespace();
            if self.peek() != Some(']') {
                bail!("expected ']' to close subscript in arithmetic");
            }
            self.advance(); // consume ']'
            segments.push(VarSegment::Index(index));
        }
        let path = VarPath { segments };
        let value = self.scope.resolve_path(&path).map_err(|e| match e {
            crate::interpreter::PathError::UndefinedRoot(_) => {
                anyhow::anyhow!("undefined variable in arithmetic: {root}")
            }
            crate::interpreter::PathError::Absence(msg)
            | crate::interpreter::PathError::Shape(msg) => anyhow::anyhow!(msg),
        })?;
        self.value_to_arith(&value, root)
    }

    /// Coerce a resolved value to an integer for arithmetic.
    fn value_to_arith(&self, value: &Value, name: &str) -> Result<i64> {
        match value {
            Value::Int(n) => Ok(*n),
            Value::String(s) => {
                // Try to parse string as integer
                s.parse().with_context(|| format!(
                    "variable '{}' has non-numeric value: {:?}", name, s
                ))
            }
            Value::Float(f) => Ok(*f as i64),
            Value::Bool(b) => Ok(if *b { 1 } else { 0 }),
            Value::Null => Ok(0), // null coerces to 0 in arithmetic
            Value::Json(_) => anyhow::bail!("variable '{}' is JSON, not a number", name),
            Value::Bytes(_) => anyhow::bail!("variable '{}' is binary data, not a number", name),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn eval(expr: &str) -> i64 {
        let scope = Scope::new();
        eval_arithmetic(expr, &scope).expect("eval should succeed")
    }

    fn eval_with_var(expr: &str, name: &str, value: i64) -> i64 {
        let mut scope = Scope::new();
        scope.set(name, Value::Int(value));
        eval_arithmetic(expr, &scope).expect("eval should succeed")
    }

    #[test]
    fn test_simple_integers() {
        assert_eq!(eval("42"), 42);
        assert_eq!(eval("0"), 0);
        assert_eq!(eval("12345"), 12345);
    }

    // ── Decision B: a bare subscript in arithmetic is a numeric expression ──
    // `$(( xs[i] ))` reads variable `i` (the opposite of `${xs[i]}`, a literal
    // key). Each bracket's inner is evaluated arithmetically to an index.

    fn eval_with_scope(expr: &str, setup: impl FnOnce(&mut Scope)) -> Result<i64> {
        let mut scope = Scope::new();
        setup(&mut scope);
        eval_arithmetic(expr, &scope)
    }

    #[test]
    fn bare_subscript_index_is_a_variable() {
        // xs = [10, 20, 30]; i = 1  →  xs[i] == 20
        let r = eval_with_scope("xs[i]", |s| {
            s.set("xs", Value::Json(serde_json::json!([10, 20, 30])));
            s.set("i", Value::Int(1));
        })
        .expect("bare xs[i] should resolve via variable i");
        assert_eq!(r, 20);
    }

    #[test]
    fn bare_subscript_literal_index() {
        let r = eval_with_scope("xs[0] + 1", |s| {
            s.set("xs", Value::Json(serde_json::json!([10, 20, 30])));
        })
        .expect("xs[0] + 1");
        assert_eq!(r, 11);
    }

    #[test]
    fn bare_subscript_inner_is_an_expression() {
        // xs[i + 1] with i = 0  →  xs[1] == 20
        let r = eval_with_scope("xs[i + 1]", |s| {
            s.set("xs", Value::Json(serde_json::json!([10, 20, 30])));
            s.set("i", Value::Int(0));
        })
        .expect("xs[i + 1]");
        assert_eq!(r, 20);
    }

    #[test]
    fn bare_subscript_negative_index() {
        let r = eval_with_scope("xs[-1]", |s| {
            s.set("xs", Value::Json(serde_json::json!([10, 20, 30])));
        })
        .expect("xs[-1]");
        assert_eq!(r, 30);
    }

    #[test]
    fn bare_subscript_out_of_bounds_is_loud() {
        let r = eval_with_scope("xs[9]", |s| {
            s.set("xs", Value::Json(serde_json::json!([10, 20])));
        });
        assert!(r.is_err(), "out-of-bounds index must be a loud error");
    }

    #[test]
    fn test_addition() {
        assert_eq!(eval("1 + 2"), 3);
        assert_eq!(eval("10 + 20 + 30"), 60);
    }

    #[test]
    fn test_subtraction() {
        assert_eq!(eval("10 - 3"), 7);
        assert_eq!(eval("100 - 50 - 25"), 25);
    }

    #[test]
    fn test_multiplication() {
        assert_eq!(eval("3 * 4"), 12);
        assert_eq!(eval("2 * 3 * 4"), 24);
    }

    #[test]
    fn test_division() {
        assert_eq!(eval("10 / 2"), 5);
        assert_eq!(eval("100 / 10 / 2"), 5);
    }

    #[test]
    fn test_modulo() {
        assert_eq!(eval("10 % 3"), 1);
        assert_eq!(eval("17 % 5"), 2);
    }

    #[test]
    fn test_precedence() {
        assert_eq!(eval("2 + 3 * 4"), 14); // Not 20
        assert_eq!(eval("10 - 6 / 2"), 7); // Not 2
    }

    #[test]
    fn test_parentheses() {
        assert_eq!(eval("(2 + 3) * 4"), 20);
        assert_eq!(eval("((1 + 2) * (3 + 4))"), 21);
    }

    #[test]
    fn test_unary_minus() {
        assert_eq!(eval("-5"), -5);
        assert_eq!(eval("10 + -3"), 7);
        assert_eq!(eval("--5"), 5);
    }

    #[test]
    fn test_unary_plus() {
        assert_eq!(eval("+5"), 5);
        assert_eq!(eval("++5"), 5);
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(eval("  1  +  2  "), 3);
        assert_eq!(eval("1+2"), 3);
    }

    #[test]
    fn test_variable_dollar() {
        assert_eq!(eval_with_var("$X", "X", 10), 10);
        assert_eq!(eval_with_var("$X + 5", "X", 10), 15);
    }

    #[test]
    fn test_variable_dollar_braces() {
        assert_eq!(eval_with_var("${X}", "X", 10), 10);
        assert_eq!(eval_with_var("${X} * 2", "X", 10), 20);
    }

    #[test]
    fn test_variable_bare() {
        assert_eq!(eval_with_var("X", "X", 10), 10);
        assert_eq!(eval_with_var("X + Y", "X", 10), 10); // Y is unset = 0
    }

    #[test]
    fn test_unset_variable() {
        let scope = Scope::new();
        let result = eval_arithmetic("UNDEFINED", &scope).expect("should succeed");
        assert_eq!(result, 0); // Unset variables default to 0
    }

    #[test]
    fn test_division_by_zero() {
        let scope = Scope::new();
        let result = eval_arithmetic("10 / 0", &scope);
        assert!(result.is_err());
    }

    #[test]
    fn test_modulo_by_zero() {
        let scope = Scope::new();
        let result = eval_arithmetic("10 % 0", &scope);
        assert!(result.is_err());
    }

    #[test]
    fn test_complex_expression() {
        assert_eq!(eval("(1 + 2) * (3 + 4) - 5"), 16);
    }

    // Comparison operator tests
    #[test]
    fn test_greater_than() {
        assert_eq!(eval("5 > 3"), 1);
        assert_eq!(eval("3 > 5"), 0);
        assert_eq!(eval("5 > 5"), 0);
    }

    #[test]
    fn test_less_than() {
        assert_eq!(eval("3 < 5"), 1);
        assert_eq!(eval("5 < 3"), 0);
        assert_eq!(eval("5 < 5"), 0);
    }

    #[test]
    fn test_greater_or_equal() {
        assert_eq!(eval("5 >= 3"), 1);
        assert_eq!(eval("5 >= 5"), 1);
        assert_eq!(eval("3 >= 5"), 0);
    }

    #[test]
    fn test_less_or_equal() {
        assert_eq!(eval("3 <= 5"), 1);
        assert_eq!(eval("5 <= 5"), 1);
        assert_eq!(eval("5 <= 3"), 0);
    }

    #[test]
    fn test_equal() {
        assert_eq!(eval("5 == 5"), 1);
        assert_eq!(eval("5 == 3"), 0);
    }

    #[test]
    fn test_not_equal() {
        assert_eq!(eval("5 != 3"), 1);
        assert_eq!(eval("5 != 5"), 0);
    }

    #[test]
    fn test_comparison_with_arithmetic() {
        assert_eq!(eval("(2 + 3) > 4"), 1);
        assert_eq!(eval("10 / 2 == 5"), 1);
        assert_eq!(eval("3 * 4 >= 12"), 1);
        assert_eq!(eval("10 - 5 < 6"), 1);
    }

    #[test]
    fn test_comparison_with_variables() {
        assert_eq!(eval_with_var("X > 5", "X", 10), 1);
        assert_eq!(eval_with_var("X == 10", "X", 10), 1);
        assert_eq!(eval_with_var("X <= 10", "X", 10), 1);
    }

    #[test]
    fn test_chained_comparison() {
        // Note: chained comparisons work left-to-right, not mathematically
        // (5 > 3) > 2 = 1 > 2 = 0
        assert_eq!(eval("5 > 3 > 2"), 0);
        // (5 > 3) == 1 = 1 == 1 = 1
        assert_eq!(eval("5 > 3 == 1"), 1);
    }
}
