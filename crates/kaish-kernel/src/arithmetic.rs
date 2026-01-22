//! Arithmetic expression evaluation for shell-style `$(( ))` expressions.
//!
//! Supports:
//! - Integer arithmetic: `+`, `-`, `*`, `/`, `%`
//! - Parentheses for grouping: `(expr)`
//! - Variable references: `$VAR` or bare `VAR`
//! - Integer literals
//!
//! Does NOT support:
//! - Floating point (use Rhai for that)
//! - Bitwise operations (shell-ism we're skipping)
//! - Assignment within expressions (confusing)

use crate::interpreter::Scope;
use crate::ast::Value;
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
    let result = parser.parse_expr()?;
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

    fn expect_end(&mut self) -> Result<()> {
        self.skip_whitespace();
        if self.pos < self.input.len() {
            bail!("unexpected characters at end of arithmetic expression: {:?}",
                  &self.input[self.pos..]);
        }
        Ok(())
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
                // $VAR or ${VAR} syntax
                self.advance(); // consume '$'
                let var_name = if self.peek() == Some('{') {
                    self.advance(); // consume '{'
                    let name = self.parse_identifier()?;
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
        match self.scope.get(name) {
            Some(Value::Int(n)) => Ok(*n),
            Some(Value::String(s)) => {
                // Try to parse string as integer
                s.parse().with_context(|| format!(
                    "variable '{}' has non-numeric value: {:?}", name, s
                ))
            }
            Some(Value::Float(f)) => Ok(*f as i64),
            Some(Value::Bool(b)) => Ok(if *b { 1 } else { 0 }),
            Some(Value::Null) => Ok(0), // Unset variables default to 0 in arithmetic
            None => Ok(0), // Unset variables default to 0 in arithmetic
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
}
