//! Expression evaluation for kaish.
//!
//! The evaluator takes AST expressions and reduces them to values.
//! Variable references are resolved through the Scope, and string
//! interpolation is expanded.
//!
//! Command substitution (`$(pipeline)`) requires an executor, which is
//! provided by higher layers (L6: Pipes & Jobs).

use std::fmt;

use crate::arithmetic;
use crate::ast::{BinaryOp, Expr, FileTestOp, Pipeline, StringPart, StringTestOp, TestCmpOp, TestExpr, Value, VarPath};
use std::path::Path;

use super::result::ExecResult;
use super::scope::Scope;

/// Errors that can occur during expression evaluation.
#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    /// Variable not found in scope.
    UndefinedVariable(String),
    /// Path resolution failed (bad field/index access).
    InvalidPath(String),
    /// Type mismatch for operation.
    TypeError { expected: &'static str, got: String },
    /// Command substitution failed.
    CommandFailed(String),
    /// No executor available for command substitution.
    NoExecutor,
    /// Division by zero or similar arithmetic error.
    ArithmeticError(String),
    /// Invalid regex pattern.
    RegexError(String),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::UndefinedVariable(name) => write!(f, "undefined variable: {name}"),
            EvalError::InvalidPath(path) => write!(f, "invalid path: {path}"),
            EvalError::TypeError { expected, got } => {
                write!(f, "type error: expected {expected}, got {got}")
            }
            EvalError::CommandFailed(msg) => write!(f, "command failed: {msg}"),
            EvalError::NoExecutor => write!(f, "no executor available for command substitution"),
            EvalError::ArithmeticError(msg) => write!(f, "arithmetic error: {msg}"),
            EvalError::RegexError(msg) => write!(f, "regex error: {msg}"),
        }
    }
}

impl std::error::Error for EvalError {}

/// Result type for evaluation.
pub type EvalResult<T> = Result<T, EvalError>;

/// Trait for executing pipelines (command substitution).
///
/// This is implemented by higher layers (L6: Pipes & Jobs) to provide
/// actual command execution. The evaluator calls this when it encounters
/// a `$(pipeline)` expression.
pub trait Executor {
    /// Execute a pipeline and return its result.
    ///
    /// The executor should:
    /// 1. Parse and execute the pipeline
    /// 2. Capture stdout/stderr
    /// 3. Return an ExecResult with code, output, and parsed data
    fn execute(&mut self, pipeline: &Pipeline, scope: &mut Scope) -> EvalResult<ExecResult>;
}

/// A stub executor that always returns an error.
///
/// Used in L3 before the full executor is available.
pub struct NoOpExecutor;

impl Executor for NoOpExecutor {
    fn execute(&mut self, _pipeline: &Pipeline, _scope: &mut Scope) -> EvalResult<ExecResult> {
        Err(EvalError::NoExecutor)
    }
}

/// Expression evaluator.
///
/// Evaluates AST expressions to values, using the provided scope for
/// variable lookup and the executor for command substitution.
pub struct Evaluator<'a, E: Executor> {
    scope: &'a mut Scope,
    executor: &'a mut E,
}

impl<'a, E: Executor> Evaluator<'a, E> {
    /// Create a new evaluator with the given scope and executor.
    pub fn new(scope: &'a mut Scope, executor: &'a mut E) -> Self {
        Self { scope, executor }
    }

    /// Evaluate an expression to a value.
    pub fn eval(&mut self, expr: &Expr) -> EvalResult<Value> {
        match expr {
            Expr::Literal(value) => self.eval_literal(value),
            Expr::VarRef(path) => self.eval_var_ref(path),
            Expr::Interpolated(parts) => self.eval_interpolated(parts),
            Expr::BinaryOp { left, op, right } => self.eval_binary_op(left, *op, right),
            Expr::CommandSubst(pipeline) => self.eval_command_subst(pipeline),
            Expr::Test(test_expr) => self.eval_test(test_expr),
            Expr::Positional(n) => self.eval_positional(*n),
            Expr::AllArgs => self.eval_all_args(),
            Expr::ArgCount => self.eval_arg_count(),
            Expr::VarLength(name) => self.eval_var_length(name),
            Expr::VarWithDefault { name, default } => self.eval_var_with_default(name, default),
            Expr::Arithmetic(expr_str) => self.eval_arithmetic(expr_str),
            Expr::Command(cmd) => self.eval_command(cmd),
        }
    }

    /// Evaluate a command as a condition (exit code determines truthiness).
    fn eval_command(&mut self, cmd: &crate::ast::Command) -> EvalResult<Value> {
        // Special-case true/false builtins - they have well-known return values
        // and don't need an executor to evaluate. Like real shells, any args are ignored.
        match cmd.name.as_str() {
            "true" => return Ok(Value::Bool(true)),
            "false" => return Ok(Value::Bool(false)),
            _ => {}
        }

        // For other commands, create a single-command pipeline to execute
        let pipeline = crate::ast::Pipeline {
            commands: vec![cmd.clone()],
            background: false,
        };
        let result = self.executor.execute(&pipeline, self.scope)?;
        // Exit code 0 = true, non-zero = false
        Ok(Value::Bool(result.code == 0))
    }

    /// Evaluate arithmetic expansion: `$((expr))`
    fn eval_arithmetic(&mut self, expr_str: &str) -> EvalResult<Value> {
        arithmetic::eval_arithmetic(expr_str, self.scope)
            .map(Value::Int)
            .map_err(|e| EvalError::ArithmeticError(e.to_string()))
    }

    /// Evaluate a test expression `[[ ... ]]` to a boolean value.
    fn eval_test(&mut self, test_expr: &TestExpr) -> EvalResult<Value> {
        let result = match test_expr {
            TestExpr::FileTest { op, path } => {
                let path_value = self.eval(path)?;
                let path_str = value_to_string(&path_value);
                let path = Path::new(&path_str);
                match op {
                    FileTestOp::Exists => path.exists(),
                    FileTestOp::IsFile => path.is_file(),
                    FileTestOp::IsDir => path.is_dir(),
                    FileTestOp::Readable => path.exists() && std::fs::metadata(path).is_ok(),
                    FileTestOp::Writable => {
                        // Check if we can write to the file
                        if path.exists() {
                            std::fs::OpenOptions::new().write(true).open(path).is_ok()
                        } else {
                            false
                        }
                    }
                    FileTestOp::Executable => {
                        #[cfg(unix)]
                        {
                            use std::os::unix::fs::PermissionsExt;
                            path.metadata()
                                .map(|m| m.permissions().mode() & 0o111 != 0)
                                .unwrap_or(false)
                        }
                        #[cfg(not(unix))]
                        {
                            path.exists()
                        }
                    }
                }
            }
            TestExpr::StringTest { op, value } => {
                let val = self.eval(value)?;
                let s = value_to_string(&val);
                match op {
                    StringTestOp::IsEmpty => s.is_empty(),
                    StringTestOp::IsNonEmpty => !s.is_empty(),
                }
            }
            TestExpr::Comparison { left, op, right } => {
                let left_val = self.eval(left)?;
                let right_val = self.eval(right)?;

                match op {
                    TestCmpOp::Eq => values_equal(&left_val, &right_val),
                    TestCmpOp::NotEq => !values_equal(&left_val, &right_val),
                    TestCmpOp::Match => {
                        // Regex match
                        match regex_match(&left_val, &right_val, false) {
                            Ok(Value::Bool(b)) => b,
                            Ok(_) => false,
                            Err(_) => false,
                        }
                    }
                    TestCmpOp::NotMatch => {
                        // Regex not match
                        match regex_match(&left_val, &right_val, true) {
                            Ok(Value::Bool(b)) => b,
                            Ok(_) => true,
                            Err(_) => true,
                        }
                    }
                    TestCmpOp::Gt | TestCmpOp::Lt | TestCmpOp::GtEq | TestCmpOp::LtEq => {
                        // Ordered comparison (works for strings and numbers)
                        // Type mismatches are errors - no silent coercion
                        let ord = compare_values(&left_val, &right_val)?;
                        match op {
                            TestCmpOp::Gt => ord.is_gt(),
                            TestCmpOp::Lt => ord.is_lt(),
                            TestCmpOp::GtEq => ord.is_ge(),
                            TestCmpOp::LtEq => ord.is_le(),
                            _ => unreachable!(),
                        }
                    }
                }
            }
        };
        Ok(Value::Bool(result))
    }

    /// Evaluate a literal value.
    fn eval_literal(&mut self, value: &Value) -> EvalResult<Value> {
        Ok(value.clone())
    }

    /// Evaluate a variable reference.
    fn eval_var_ref(&mut self, path: &VarPath) -> EvalResult<Value> {
        self.scope
            .resolve_path(path)
            .ok_or_else(|| EvalError::InvalidPath(format_path(path)))
    }

    /// Evaluate a positional parameter ($0-$9).
    fn eval_positional(&self, n: usize) -> EvalResult<Value> {
        match self.scope.get_positional(n) {
            Some(s) => Ok(Value::String(s.to_string())),
            None => Ok(Value::String(String::new())), // Unset positional returns empty string
        }
    }

    /// Evaluate all arguments ($@).
    ///
    /// Returns a space-separated string of all positional arguments (POSIX-style).
    fn eval_all_args(&self) -> EvalResult<Value> {
        let args = self.scope.all_args();
        Ok(Value::String(args.join(" ")))
    }

    /// Evaluate argument count ($#).
    fn eval_arg_count(&self) -> EvalResult<Value> {
        Ok(Value::Int(self.scope.arg_count() as i64))
    }

    /// Evaluate variable string length (${#VAR}).
    fn eval_var_length(&self, name: &str) -> EvalResult<Value> {
        match self.scope.get(name) {
            Some(value) => {
                let s = value_to_string(value);
                Ok(Value::Int(s.len() as i64))
            }
            None => Ok(Value::Int(0)), // Unset variable has length 0
        }
    }

    /// Evaluate variable with default (${VAR:-default}).
    /// Returns the variable value if set and non-empty, otherwise the default.
    fn eval_var_with_default(&self, name: &str, default: &str) -> EvalResult<Value> {
        match self.scope.get(name) {
            Some(value) => {
                let s = value_to_string(value);
                if s.is_empty() {
                    // Variable is set but empty, use default
                    Ok(Value::String(default.to_string()))
                } else {
                    Ok(value.clone())
                }
            }
            None => {
                // Variable is unset, use default
                Ok(Value::String(default.to_string()))
            }
        }
    }

    /// Evaluate an interpolated string.
    fn eval_interpolated(&mut self, parts: &[StringPart]) -> EvalResult<Value> {
        let mut result = String::new();
        for part in parts {
            match part {
                StringPart::Literal(s) => result.push_str(s),
                StringPart::Var(path) => {
                    let value = self.scope.resolve_path(path).ok_or_else(|| {
                        EvalError::InvalidPath(format_path(path))
                    })?;
                    result.push_str(&value_to_string(&value));
                }
                StringPart::VarWithDefault { name, default } => {
                    let value = self.eval_var_with_default(name, default)?;
                    result.push_str(&value_to_string(&value));
                }
                StringPart::VarLength(name) => {
                    let value = self.eval_var_length(name)?;
                    result.push_str(&value_to_string(&value));
                }
            }
        }
        Ok(Value::String(result))
    }

    /// Evaluate a binary operation.
    fn eval_binary_op(&mut self, left: &Expr, op: BinaryOp, right: &Expr) -> EvalResult<Value> {
        match op {
            // Short-circuit logical operators
            BinaryOp::And => {
                let left_val = self.eval(left)?;
                if !is_truthy(&left_val) {
                    return Ok(left_val);
                }
                self.eval(right)
            }
            BinaryOp::Or => {
                let left_val = self.eval(left)?;
                if is_truthy(&left_val) {
                    return Ok(left_val);
                }
                self.eval(right)
            }
            // Comparison operators
            BinaryOp::Eq => {
                let left_val = self.eval(left)?;
                let right_val = self.eval(right)?;
                Ok(Value::Bool(values_equal(&left_val, &right_val)))
            }
            BinaryOp::NotEq => {
                let left_val = self.eval(left)?;
                let right_val = self.eval(right)?;
                Ok(Value::Bool(!values_equal(&left_val, &right_val)))
            }
            BinaryOp::Lt => {
                let left_val = self.eval(left)?;
                let right_val = self.eval(right)?;
                compare_values(&left_val, &right_val).map(|ord| Value::Bool(ord.is_lt()))
            }
            BinaryOp::Gt => {
                let left_val = self.eval(left)?;
                let right_val = self.eval(right)?;
                compare_values(&left_val, &right_val).map(|ord| Value::Bool(ord.is_gt()))
            }
            BinaryOp::LtEq => {
                let left_val = self.eval(left)?;
                let right_val = self.eval(right)?;
                compare_values(&left_val, &right_val).map(|ord| Value::Bool(ord.is_le()))
            }
            BinaryOp::GtEq => {
                let left_val = self.eval(left)?;
                let right_val = self.eval(right)?;
                compare_values(&left_val, &right_val).map(|ord| Value::Bool(ord.is_ge()))
            }
            // Regex match operators
            BinaryOp::Match => {
                let left_val = self.eval(left)?;
                let right_val = self.eval(right)?;
                regex_match(&left_val, &right_val, false)
            }
            BinaryOp::NotMatch => {
                let left_val = self.eval(left)?;
                let right_val = self.eval(right)?;
                regex_match(&left_val, &right_val, true)
            }
        }
    }

    /// Evaluate command substitution.
    fn eval_command_subst(&mut self, pipeline: &Pipeline) -> EvalResult<Value> {
        let result = self.executor.execute(pipeline, self.scope)?;

        // Update $? with the result
        self.scope.set_last_result(result.clone());

        // Return the result as a value (the result object itself)
        // The caller can access .ok, .data, etc.
        Ok(result_to_value(&result))
    }
}

/// Convert a Value to its string representation for interpolation.
pub fn value_to_string(value: &Value) -> String {
    match value {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => s.clone(),
    }
}

/// Expand tilde (~) to home directory.
///
/// - `~` alone → `$HOME`
/// - `~/path` → `$HOME/path`
/// - Other strings are returned unchanged.
pub fn expand_tilde(s: &str) -> String {
    if s == "~" {
        std::env::var("HOME").unwrap_or_else(|_| "~".to_string())
    } else if s.starts_with("~/") {
        match std::env::var("HOME") {
            Ok(home) => format!("{}{}", home, &s[1..]),
            Err(_) => s.to_string(),
        }
    } else {
        s.to_string()
    }
}

/// Convert a Value to its string representation, with tilde expansion for paths.
pub fn value_to_string_with_tilde(value: &Value) -> String {
    match value {
        Value::String(s) if s.starts_with('~') => expand_tilde(s),
        _ => value_to_string(value),
    }
}

/// Format a VarPath for error messages.
fn format_path(path: &VarPath) -> String {
    use crate::ast::VarSegment;
    let mut result = String::from("${");
    for (i, seg) in path.segments.iter().enumerate() {
        match seg {
            VarSegment::Field(name) => {
                if i > 0 {
                    result.push('.');
                }
                result.push_str(name);
            }
        }
    }
    result.push('}');
    result
}

/// Check if a value is "truthy" for boolean operations.
///
/// - `null` → false
/// - `false` → false
/// - `0` → false
/// - `""` → false
/// - Everything else → true
fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(i) => *i != 0,
        Value::Float(f) => *f != 0.0,
        Value::String(s) => !s.is_empty(),
    }
}

/// Check if two values are equal.
fn values_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Null, Value::Null) => true,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Int(a), Value::Int(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => (a - b).abs() < f64::EPSILON,
        (Value::Int(a), Value::Float(b)) | (Value::Float(b), Value::Int(a)) => {
            (*a as f64 - b).abs() < f64::EPSILON
        }
        (Value::String(a), Value::String(b)) => a == b,
        _ => false,
    }
}

/// Compare two values for ordering.
fn compare_values(left: &Value, right: &Value) -> EvalResult<std::cmp::Ordering> {
    match (left, right) {
        (Value::Int(a), Value::Int(b)) => Ok(a.cmp(b)),
        (Value::Float(a), Value::Float(b)) => {
            a.partial_cmp(b).ok_or_else(|| EvalError::ArithmeticError("NaN comparison".into()))
        }
        (Value::Int(a), Value::Float(b)) => {
            (*a as f64).partial_cmp(b).ok_or_else(|| EvalError::ArithmeticError("NaN comparison".into()))
        }
        (Value::Float(a), Value::Int(b)) => {
            a.partial_cmp(&(*b as f64)).ok_or_else(|| EvalError::ArithmeticError("NaN comparison".into()))
        }
        (Value::String(a), Value::String(b)) => Ok(a.cmp(b)),
        _ => Err(EvalError::TypeError {
            expected: "comparable types (numbers or strings)",
            got: format!("{:?} vs {:?}", type_name(left), type_name(right)),
        }),
    }
}

/// Get a human-readable type name for a value.
fn type_name(value: &Value) -> &'static str {
    match value {
        Value::Null => "null",
        Value::Bool(_) => "bool",
        Value::Int(_) => "int",
        Value::Float(_) => "float",
        Value::String(_) => "string",
    }
}

/// Convert an ExecResult to a Value for command substitution return.
///
/// Returns the stdout (trimmed) as a string, which is bash-compatible behavior.
/// Access to other result fields (code, err, etc.) is via ${?.field}.
fn result_to_value(result: &ExecResult) -> Value {
    Value::String(result.out.trim_end().to_string())
}

/// Perform regex match or not-match on two values.
///
/// The left operand is the string to match against.
/// The right operand is the regex pattern.
fn regex_match(left: &Value, right: &Value, negate: bool) -> EvalResult<Value> {
    let text = match left {
        Value::String(s) => s.as_str(),
        _ => {
            return Err(EvalError::TypeError {
                expected: "string",
                got: type_name(left).to_string(),
            })
        }
    };

    let pattern = match right {
        Value::String(s) => s.as_str(),
        _ => {
            return Err(EvalError::TypeError {
                expected: "string (regex pattern)",
                got: type_name(right).to_string(),
            })
        }
    };

    let re = regex::Regex::new(pattern).map_err(|e| EvalError::RegexError(e.to_string()))?;
    let matches = re.is_match(text);

    Ok(Value::Bool(if negate { !matches } else { matches }))
}

/// Convenience function to evaluate an expression with a scope.
///
/// Uses NoOpExecutor, so command substitution will fail.
pub fn eval_expr(expr: &Expr, scope: &mut Scope) -> EvalResult<Value> {
    let mut executor = NoOpExecutor;
    let mut evaluator = Evaluator::new(scope, &mut executor);
    evaluator.eval(expr)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::VarSegment;

    // Helper to create a simple variable expression
    fn var_expr(name: &str) -> Expr {
        Expr::VarRef(VarPath::simple(name))
    }

    #[test]
    fn eval_literal_int() {
        let mut scope = Scope::new();
        let expr = Expr::Literal(Value::Int(42));
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Int(42)));
    }

    #[test]
    fn eval_literal_string() {
        let mut scope = Scope::new();
        let expr = Expr::Literal(Value::String("hello".into()));
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::String("hello".into())));
    }

    #[test]
    fn eval_literal_bool() {
        let mut scope = Scope::new();
        assert_eq!(
            eval_expr(&Expr::Literal(Value::Bool(true)), &mut scope),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn eval_literal_null() {
        let mut scope = Scope::new();
        assert_eq!(
            eval_expr(&Expr::Literal(Value::Null), &mut scope),
            Ok(Value::Null)
        );
    }

    #[test]
    fn eval_literal_float() {
        let mut scope = Scope::new();
        let expr = Expr::Literal(Value::Float(3.14));
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Float(3.14)));
    }

    #[test]
    fn eval_variable_ref() {
        let mut scope = Scope::new();
        scope.set("X", Value::Int(100));
        assert_eq!(eval_expr(&var_expr("X"), &mut scope), Ok(Value::Int(100)));
    }

    #[test]
    fn eval_undefined_variable() {
        let mut scope = Scope::new();
        let result = eval_expr(&var_expr("MISSING"), &mut scope);
        assert!(matches!(result, Err(EvalError::InvalidPath(_))));
    }

    #[test]
    fn eval_interpolated_string() {
        let mut scope = Scope::new();
        scope.set("NAME", Value::String("World".into()));

        let expr = Expr::Interpolated(vec![
            StringPart::Literal("Hello, ".into()),
            StringPart::Var(VarPath::simple("NAME")),
            StringPart::Literal("!".into()),
        ]);
        assert_eq!(
            eval_expr(&expr, &mut scope),
            Ok(Value::String("Hello, World!".into()))
        );
    }

    #[test]
    fn eval_interpolated_with_number() {
        let mut scope = Scope::new();
        scope.set("COUNT", Value::Int(42));

        let expr = Expr::Interpolated(vec![
            StringPart::Literal("Count: ".into()),
            StringPart::Var(VarPath::simple("COUNT")),
        ]);
        assert_eq!(
            eval_expr(&expr, &mut scope),
            Ok(Value::String("Count: 42".into()))
        );
    }

    #[test]
    fn eval_and_short_circuit_true() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Bool(true))),
            op: BinaryOp::And,
            right: Box::new(Expr::Literal(Value::Int(42))),
        };
        // true && 42 => 42 (returns right operand)
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Int(42)));
    }

    #[test]
    fn eval_and_short_circuit_false() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Bool(false))),
            op: BinaryOp::And,
            right: Box::new(Expr::Literal(Value::Int(42))),
        };
        // false && 42 => false (returns left operand, short-circuits)
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(false)));
    }

    #[test]
    fn eval_or_short_circuit_true() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Bool(true))),
            op: BinaryOp::Or,
            right: Box::new(Expr::Literal(Value::Int(42))),
        };
        // true || 42 => true (returns left operand, short-circuits)
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_or_short_circuit_false() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Bool(false))),
            op: BinaryOp::Or,
            right: Box::new(Expr::Literal(Value::Int(42))),
        };
        // false || 42 => 42 (returns right operand)
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Int(42)));
    }

    #[test]
    fn eval_equality() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Int(5))),
            op: BinaryOp::Eq,
            right: Box::new(Expr::Literal(Value::Int(5))),
        };
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_inequality() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Int(5))),
            op: BinaryOp::NotEq,
            right: Box::new(Expr::Literal(Value::Int(3))),
        };
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_less_than() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Int(3))),
            op: BinaryOp::Lt,
            right: Box::new(Expr::Literal(Value::Int(5))),
        };
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_greater_than() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Int(5))),
            op: BinaryOp::Gt,
            right: Box::new(Expr::Literal(Value::Int(3))),
        };
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_less_than_or_equal() {
        let mut scope = Scope::new();
        let eq = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Int(5))),
            op: BinaryOp::LtEq,
            right: Box::new(Expr::Literal(Value::Int(5))),
        };
        let lt = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Int(3))),
            op: BinaryOp::LtEq,
            right: Box::new(Expr::Literal(Value::Int(5))),
        };
        assert_eq!(eval_expr(&eq, &mut scope), Ok(Value::Bool(true)));
        assert_eq!(eval_expr(&lt, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_greater_than_or_equal() {
        let mut scope = Scope::new();
        let eq = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Int(5))),
            op: BinaryOp::GtEq,
            right: Box::new(Expr::Literal(Value::Int(5))),
        };
        let gt = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Int(7))),
            op: BinaryOp::GtEq,
            right: Box::new(Expr::Literal(Value::Int(5))),
        };
        assert_eq!(eval_expr(&eq, &mut scope), Ok(Value::Bool(true)));
        assert_eq!(eval_expr(&gt, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_string_comparison() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::String("apple".into()))),
            op: BinaryOp::Lt,
            right: Box::new(Expr::Literal(Value::String("banana".into()))),
        };
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_mixed_int_float_comparison() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Int(3))),
            op: BinaryOp::Lt,
            right: Box::new(Expr::Literal(Value::Float(3.5))),
        };
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_int_float_equality() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Int(5))),
            op: BinaryOp::Eq,
            right: Box::new(Expr::Literal(Value::Float(5.0))),
        };
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_type_mismatch_comparison() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Int(5))),
            op: BinaryOp::Lt,
            right: Box::new(Expr::Literal(Value::String("five".into()))),
        };
        assert!(matches!(eval_expr(&expr, &mut scope), Err(EvalError::TypeError { .. })));
    }

    #[test]
    fn is_truthy_values() {
        assert!(!is_truthy(&Value::Null));
        assert!(!is_truthy(&Value::Bool(false)));
        assert!(is_truthy(&Value::Bool(true)));
        assert!(!is_truthy(&Value::Int(0)));
        assert!(is_truthy(&Value::Int(1)));
        assert!(is_truthy(&Value::Int(-1)));
        assert!(!is_truthy(&Value::Float(0.0)));
        assert!(is_truthy(&Value::Float(0.1)));
        assert!(!is_truthy(&Value::String("".into())));
        assert!(is_truthy(&Value::String("x".into())));
    }

    #[test]
    fn eval_command_subst_fails_without_executor() {
        use crate::ast::{Command, Pipeline};

        let mut scope = Scope::new();
        let pipeline = Pipeline {
            commands: vec![Command {
                name: "echo".into(),
                args: vec![],
                redirects: vec![],
            }],
            background: false,
        };
        let expr = Expr::CommandSubst(Box::new(pipeline));

        assert!(matches!(
            eval_expr(&expr, &mut scope),
            Err(EvalError::NoExecutor)
        ));
    }

    #[test]
    fn eval_last_result_field() {
        let mut scope = Scope::new();
        scope.set_last_result(ExecResult::failure(42, "test error"));

        // ${?.code}
        let expr = Expr::VarRef(VarPath {
            segments: vec![
                VarSegment::Field("?".into()),
                VarSegment::Field("code".into()),
            ],
        });
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Int(42)));

        // ${?.err}
        let expr = Expr::VarRef(VarPath {
            segments: vec![
                VarSegment::Field("?".into()),
                VarSegment::Field("err".into()),
            ],
        });
        assert_eq!(
            eval_expr(&expr, &mut scope),
            Ok(Value::String("test error".into()))
        );
    }

    #[test]
    fn value_to_string_all_types() {
        assert_eq!(value_to_string(&Value::Null), "null");
        assert_eq!(value_to_string(&Value::Bool(true)), "true");
        assert_eq!(value_to_string(&Value::Int(42)), "42");
        assert_eq!(value_to_string(&Value::Float(3.14)), "3.14");
        assert_eq!(value_to_string(&Value::String("hello".into())), "hello");
    }

    // Additional comprehensive tests

    #[test]
    fn eval_negative_int() {
        let mut scope = Scope::new();
        let expr = Expr::Literal(Value::Int(-42));
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Int(-42)));
    }

    #[test]
    fn eval_negative_float() {
        let mut scope = Scope::new();
        let expr = Expr::Literal(Value::Float(-3.14));
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Float(-3.14)));
    }

    #[test]
    fn eval_zero_values() {
        let mut scope = Scope::new();
        assert_eq!(
            eval_expr(&Expr::Literal(Value::Int(0)), &mut scope),
            Ok(Value::Int(0))
        );
        assert_eq!(
            eval_expr(&Expr::Literal(Value::Float(0.0)), &mut scope),
            Ok(Value::Float(0.0))
        );
    }

    #[test]
    fn eval_interpolation_empty_var() {
        let mut scope = Scope::new();
        scope.set("EMPTY", Value::String("".into()));

        let expr = Expr::Interpolated(vec![
            StringPart::Literal("prefix".into()),
            StringPart::Var(VarPath::simple("EMPTY")),
            StringPart::Literal("suffix".into()),
        ]);
        assert_eq!(
            eval_expr(&expr, &mut scope),
            Ok(Value::String("prefixsuffix".into()))
        );
    }

    #[test]
    fn eval_chained_and() {
        let mut scope = Scope::new();
        // true && true && 42
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::Literal(Value::Bool(true))),
                op: BinaryOp::And,
                right: Box::new(Expr::Literal(Value::Bool(true))),
            }),
            op: BinaryOp::And,
            right: Box::new(Expr::Literal(Value::Int(42))),
        };
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Int(42)));
    }

    #[test]
    fn eval_chained_or() {
        let mut scope = Scope::new();
        // false || false || 42
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::Literal(Value::Bool(false))),
                op: BinaryOp::Or,
                right: Box::new(Expr::Literal(Value::Bool(false))),
            }),
            op: BinaryOp::Or,
            right: Box::new(Expr::Literal(Value::Int(42))),
        };
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Int(42)));
    }

    #[test]
    fn eval_mixed_and_or() {
        let mut scope = Scope::new();
        // true || false && false  (and binds tighter, but here we test explicit tree)
        // This tests: (true || false) && true
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::Literal(Value::Bool(true))),
                op: BinaryOp::Or,
                right: Box::new(Expr::Literal(Value::Bool(false))),
            }),
            op: BinaryOp::And,
            right: Box::new(Expr::Literal(Value::Bool(true))),
        };
        // (true || false) = true, true && true = true
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_comparison_with_variables() {
        let mut scope = Scope::new();
        scope.set("X", Value::Int(10));
        scope.set("Y", Value::Int(5));

        let expr = Expr::BinaryOp {
            left: Box::new(var_expr("X")),
            op: BinaryOp::Gt,
            right: Box::new(var_expr("Y")),
        };
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_string_equality() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::String("hello".into()))),
            op: BinaryOp::Eq,
            right: Box::new(Expr::Literal(Value::String("hello".into()))),
        };
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_string_inequality() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::String("hello".into()))),
            op: BinaryOp::NotEq,
            right: Box::new(Expr::Literal(Value::String("world".into()))),
        };
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_null_equality() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Null)),
            op: BinaryOp::Eq,
            right: Box::new(Expr::Literal(Value::Null)),
        };
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_null_not_equal_to_int() {
        let mut scope = Scope::new();
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Null)),
            op: BinaryOp::Eq,
            right: Box::new(Expr::Literal(Value::Int(0))),
        };
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(false)));
    }

    #[test]
    fn eval_float_comparison_boundary() {
        let mut scope = Scope::new();
        // 1.0 == 1.0 (exact)
        let expr = Expr::BinaryOp {
            left: Box::new(Expr::Literal(Value::Float(1.0))),
            op: BinaryOp::Eq,
            right: Box::new(Expr::Literal(Value::Float(1.0))),
        };
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_interpolation_with_bool() {
        let mut scope = Scope::new();
        scope.set("FLAG", Value::Bool(true));

        let expr = Expr::Interpolated(vec![
            StringPart::Literal("enabled: ".into()),
            StringPart::Var(VarPath::simple("FLAG")),
        ]);
        assert_eq!(
            eval_expr(&expr, &mut scope),
            Ok(Value::String("enabled: true".into()))
        );
    }

    #[test]
    fn eval_interpolation_with_null() {
        let mut scope = Scope::new();
        scope.set("VAL", Value::Null);

        let expr = Expr::Interpolated(vec![
            StringPart::Literal("value: ".into()),
            StringPart::Var(VarPath::simple("VAL")),
        ]);
        assert_eq!(
            eval_expr(&expr, &mut scope),
            Ok(Value::String("value: null".into()))
        );
    }

    #[test]
    fn eval_format_path_simple() {
        let path = VarPath::simple("X");
        assert_eq!(format_path(&path), "${X}");
    }

    #[test]
    fn eval_format_path_nested() {
        let path = VarPath {
            segments: vec![
                VarSegment::Field("?".into()),
                VarSegment::Field("code".into()),
            ],
        };
        assert_eq!(format_path(&path), "${?.code}");
    }

    #[test]
    fn type_name_all_types() {
        assert_eq!(type_name(&Value::Null), "null");
        assert_eq!(type_name(&Value::Bool(true)), "bool");
        assert_eq!(type_name(&Value::Int(1)), "int");
        assert_eq!(type_name(&Value::Float(1.0)), "float");
        assert_eq!(type_name(&Value::String("".into())), "string");
    }

    #[test]
    fn expand_tilde_home() {
        // Only test if HOME is set
        if let Ok(home) = std::env::var("HOME") {
            assert_eq!(expand_tilde("~"), home);
            assert_eq!(expand_tilde("~/foo"), format!("{}/foo", home));
            assert_eq!(expand_tilde("~/foo/bar"), format!("{}/foo/bar", home));
        }
    }

    #[test]
    fn expand_tilde_passthrough() {
        // These should not be expanded
        assert_eq!(expand_tilde("/home/user"), "/home/user");
        assert_eq!(expand_tilde("foo~bar"), "foo~bar");
        assert_eq!(expand_tilde("~user"), "~user"); // ~user is not supported
        assert_eq!(expand_tilde(""), "");
    }

    #[test]
    fn value_to_string_with_tilde_expansion() {
        if let Ok(home) = std::env::var("HOME") {
            let val = Value::String("~/test".into());
            assert_eq!(value_to_string_with_tilde(&val), format!("{}/test", home));
        }
    }

    #[test]
    fn eval_positional_param() {
        let mut scope = Scope::new();
        scope.set_positional("my_tool", vec!["hello".into(), "world".into()]);

        // $0 is the tool name
        let expr = Expr::Positional(0);
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::String("my_tool".into()));

        // $1 is the first argument
        let expr = Expr::Positional(1);
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::String("hello".into()));

        // $2 is the second argument
        let expr = Expr::Positional(2);
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::String("world".into()));

        // $3 is not set, returns empty string
        let expr = Expr::Positional(3);
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::String("".into()));
    }

    #[test]
    fn eval_all_args() {
        let mut scope = Scope::new();
        scope.set_positional("test", vec!["a".into(), "b".into(), "c".into()]);

        let expr = Expr::AllArgs;
        let result = eval_expr(&expr, &mut scope).unwrap();

        // $@ returns a space-separated string (POSIX-style)
        assert_eq!(result, Value::String("a b c".into()));
    }

    #[test]
    fn eval_arg_count() {
        let mut scope = Scope::new();
        scope.set_positional("test", vec!["x".into(), "y".into()]);

        let expr = Expr::ArgCount;
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::Int(2));
    }

    #[test]
    fn eval_arg_count_empty() {
        let mut scope = Scope::new();

        let expr = Expr::ArgCount;
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::Int(0));
    }

    #[test]
    fn eval_var_length_string() {
        let mut scope = Scope::new();
        scope.set("NAME", Value::String("hello".into()));

        let expr = Expr::VarLength("NAME".into());
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::Int(5));
    }

    #[test]
    fn eval_var_length_empty_string() {
        let mut scope = Scope::new();
        scope.set("EMPTY", Value::String("".into()));

        let expr = Expr::VarLength("EMPTY".into());
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::Int(0));
    }

    #[test]
    fn eval_var_length_unset() {
        let mut scope = Scope::new();

        // Unset variable has length 0
        let expr = Expr::VarLength("MISSING".into());
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::Int(0));
    }

    #[test]
    fn eval_var_length_int() {
        let mut scope = Scope::new();
        scope.set("NUM", Value::Int(12345));

        // Length of the string representation
        let expr = Expr::VarLength("NUM".into());
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::Int(5)); // "12345" has length 5
    }

    #[test]
    fn eval_var_with_default_set() {
        let mut scope = Scope::new();
        scope.set("NAME", Value::String("Alice".into()));

        // Variable is set, return its value
        let expr = Expr::VarWithDefault {
            name: "NAME".into(),
            default: "default".into(),
        };
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::String("Alice".into()));
    }

    #[test]
    fn eval_var_with_default_unset() {
        let mut scope = Scope::new();

        // Variable is unset, return default
        let expr = Expr::VarWithDefault {
            name: "MISSING".into(),
            default: "fallback".into(),
        };
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::String("fallback".into()));
    }

    #[test]
    fn eval_var_with_default_empty() {
        let mut scope = Scope::new();
        scope.set("EMPTY", Value::String("".into()));

        // Variable is set but empty, return default
        let expr = Expr::VarWithDefault {
            name: "EMPTY".into(),
            default: "not empty".into(),
        };
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::String("not empty".into()));
    }

    #[test]
    fn eval_var_with_default_non_string() {
        let mut scope = Scope::new();
        scope.set("NUM", Value::Int(42));

        // Variable is set to a non-string value, return the value
        let expr = Expr::VarWithDefault {
            name: "NUM".into(),
            default: "default".into(),
        };
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::Int(42));
    }
}
