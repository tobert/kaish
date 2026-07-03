//! Expression evaluation for kaish.
//!
//! The evaluator takes AST expressions and reduces them to values.
//! Variable references are resolved through the Scope, and string
//! interpolation is expanded.
//!
//! Command substitution (`$(pipeline)`) requires an executor, which is
//! provided by higher layers (L6: Pipes & Jobs).

use std::fmt;

use kaish_types::json_to_value_no_envelope;

use crate::arithmetic;
use crate::ast::{BinaryOp, Expr, FileTestOp, Stmt, StringPart, StringTestOp, TestCmpOp, TestExpr, Value, VarPath};
use crate::vfs::DirEntry;
use std::path::Path;

use super::result::ExecResult;
use super::scope::Scope;

/// Strip leading tabs from each line, per POSIX `<<-EOF` heredoc semantics.
///
/// Only tab characters are stripped (not spaces), matching POSIX. Applied at
/// materialization time so source byte offsets in the AST remain aligned with
/// the original source for span-tracking purposes.
pub fn strip_leading_tabs(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut at_line_start = true;
    for ch in s.chars() {
        if at_line_start && ch == '\t' {
            // skip leading tabs at start of line
            continue;
        }
        out.push(ch);
        at_line_start = ch == '\n';
    }
    out
}

/// Assembles a heredoc body part-by-part, applying POSIX `<<-` leading-tab
/// stripping to the **source** rather than to the materialized result.
///
/// Leading tabs that were literal in the heredoc source are stripped; a tab
/// that arrives via an interpolation (`$var` value, `$(cmd)` output) at line
/// start is preserved, because POSIX strips tabs from source lines *before*
/// parameter expansion (bash agrees). Callers feed literal segments through
/// [`push_literal`](Self::push_literal) and interpolated values through
/// [`push_interpolated`](Self::push_interpolated). With `strip_tabs == false`
/// this is a plain concatenation.
///
/// This replaces materialize-then-`strip_leading_tabs`, which ate tabs that
/// came from a variable's value.
pub struct HeredocAssembler {
    out: String,
    strip_tabs: bool,
    at_line_start: bool,
}

impl HeredocAssembler {
    pub fn new(strip_tabs: bool) -> Self {
        Self {
            out: String::new(),
            strip_tabs,
            at_line_start: true,
        }
    }

    /// Append a literal source segment, stripping leading tabs at line starts
    /// when in `<<-` mode.
    pub fn push_literal(&mut self, literal: &str) {
        if !self.strip_tabs {
            self.out.push_str(literal);
            return;
        }
        for ch in literal.chars() {
            match ch {
                '\n' => {
                    self.out.push(ch);
                    self.at_line_start = true;
                }
                '\t' if self.at_line_start => {} // strip a leading source tab
                _ => {
                    self.out.push(ch);
                    self.at_line_start = false;
                }
            }
        }
    }

    /// Append an interpolated value verbatim. The interpolation terminates the
    /// leading-tab run for the current source line — even when it expands to
    /// empty — so a following literal tab on the same source line is mid-line
    /// and kept.
    pub fn push_interpolated(&mut self, value: &str) {
        self.out.push_str(value);
        if self.strip_tabs {
            self.at_line_start = false;
        }
    }

    pub fn into_string(self) -> String {
        self.out
    }
}

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
    /// A collection (list/record) was compared to a scalar with `==`/`!=`, or a
    /// collection form (`${#…}`, `${…:-default}`) was used on a subscripted path
    /// before that path support landed. Carries a full teaching message.
    Unsupported(String),
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
            EvalError::Unsupported(msg) => write!(f, "{msg}"),
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
    /// Execute a command-substitution body — a block of statements (the full
    /// grammar: pipelines, `&&`/`||` chains, `;`/newline sequences) — and return
    /// its combined result.
    ///
    /// The executor should:
    /// 1. Run each statement, accumulating stdout/stderr
    /// 2. Carry the last statement's exit code and structured data through
    /// 3. Return an ExecResult with code, output, and parsed data
    fn execute(&mut self, stmts: &[Stmt], scope: &mut Scope) -> EvalResult<ExecResult>;

    /// Stat a file path through the VFS.
    ///
    /// Returns `Some(entry)` if the path exists, `None` otherwise.
    /// Used by `[[ -d path ]]`, `[[ -f path ]]`, etc.
    ///
    /// Default: falls back to `std::fs::metadata` (bypasses VFS).
    fn file_stat(&self, path: &Path) -> Option<DirEntry> {
        std::fs::metadata(path).ok().map(|meta| {
            if meta.is_dir() {
                DirEntry::directory(path.file_name().unwrap_or_default().to_string_lossy())
            } else {
                #[allow(unused_mut)]
                let mut entry = DirEntry::file(
                    path.file_name().unwrap_or_default().to_string_lossy(),
                    meta.len(),
                );
                #[cfg(unix)]
                {
                    use std::os::unix::fs::PermissionsExt;
                    entry.permissions = Some(meta.permissions().mode());
                }
                entry
            }
        })
    }
}

/// A stub executor that always returns an error.
///
/// Used in L3 before the full executor is available.
pub struct NoOpExecutor;

impl Executor for NoOpExecutor {
    fn execute(&mut self, _stmts: &[Stmt], _scope: &mut Scope) -> EvalResult<ExecResult> {
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
            Expr::HereDocBody { parts, strip_tabs } => {
                // Assemble the body part-by-part so `<<-` tab stripping applies
                // to the literal source, not to tabs that came from a `$var`.
                let mut asm = HeredocAssembler::new(*strip_tabs);
                for sp in parts {
                    match &sp.part {
                        StringPart::Literal(s) => asm.push_literal(s),
                        other => {
                            let value = self.eval_interpolated(std::slice::from_ref(other))?;
                            asm.push_interpolated(&value_to_string(&value));
                        }
                    }
                }
                Ok(Value::String(asm.into_string()))
            }
            Expr::BinaryOp { left, op, right } => self.eval_binary_op(left, *op, right),
            Expr::CommandSubst(stmts) => self.eval_command_subst(stmts),
            Expr::Test(test_expr) => self.eval_test(test_expr),
            Expr::Positional(n) => self.eval_positional(*n),
            Expr::AllArgs => self.eval_all_args(),
            Expr::ArgCount => self.eval_arg_count(),
            Expr::VarLength(path) => self.eval_var_length(path),
            Expr::VarWithDefault { path, default } => self.eval_var_with_default(path, default),
            Expr::Arithmetic(expr_str) => self.eval_arithmetic(expr_str),
            Expr::Command(cmd) => self.eval_command(cmd),
            Expr::LastExitCode => self.eval_last_exit_code(),
            Expr::CurrentPid => self.eval_current_pid(),
            Expr::GlobPattern(s) => Ok(Value::String(s.clone())),
        }
    }

    /// Evaluate last exit code ($?).
    fn eval_last_exit_code(&self) -> EvalResult<Value> {
        Ok(Value::Int(self.scope.last_result().code))
    }

    /// Evaluate current shell PID ($$).
    fn eval_current_pid(&self) -> EvalResult<Value> {
        Ok(Value::Int(self.scope.pid() as i64))
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

        // For other commands, run the command as a one-statement block.
        let block = [Stmt::Command(cmd.clone())];
        let result = self.executor.execute(&block, self.scope)?;
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
                let entry = self.executor.file_stat(path);
                match op {
                    FileTestOp::Exists => entry.is_some(),
                    FileTestOp::IsFile => entry.as_ref().is_some_and(|e| e.is_file()),
                    FileTestOp::IsDir => entry.as_ref().is_some_and(|e| e.is_dir()),
                    FileTestOp::Readable => entry.is_some(),
                    FileTestOp::Writable => entry.as_ref().is_some_and(|e| {
                        e.permissions.is_none_or(|p| p & 0o222 != 0)
                    }),
                    FileTestOp::Executable => entry.as_ref().is_some_and(|e| {
                        e.permissions.is_some_and(|p| p & 0o111 != 0)
                    }),
                }
            }
            TestExpr::StringTest { op, value } => match op {
                StringTestOp::IsEmpty | StringTestOp::IsNonEmpty => {
                    let val = self.eval(value)?;
                    // Decision E: a collection operand is a loud Shape error,
                    // never silently stringified-then-measured (an empty list
                    // `[]` must not read as "-z"-true via its JSON text).
                    let symbol = match op {
                        StringTestOp::IsEmpty => "-z",
                        StringTestOp::IsNonEmpty => "-n",
                        StringTestOp::IsList | StringTestOp::IsRecord => unreachable!(),
                    };
                    if let Some(msg) = scalar_test_operand_error(symbol, &val) {
                        return Err(EvalError::Unsupported(msg));
                    }
                    let s = value_to_string(&val);
                    match op {
                        StringTestOp::IsEmpty => s.is_empty(),
                        StringTestOp::IsNonEmpty => !s.is_empty(),
                        StringTestOp::IsList | StringTestOp::IsRecord => unreachable!(),
                    }
                }
                // Shape guard: inspect the operand's type. Propagates eval
                // errors like -z/-n — a bare `$unset` is an undefined-variable
                // error, not a silent `false` (catching a typo beats reading
                // "not a list"). The guard is meant to be used bare
                // (`[[ -list $data ]]`); a defined-but-wrong-shaped value is
                // simply false.
                StringTestOp::IsList | StringTestOp::IsRecord => {
                    let val = self.eval(value)?;
                    op.matches_shape(&val)
                }
            },
            TestExpr::Comparison { left, op, right } => {
                let left_val = self.eval(left)?;
                let right_val = self.eval(right)?;

                match op {
                    TestCmpOp::Eq => values_equal(&left_val, &right_val)?,
                    TestCmpOp::NotEq => !(values_equal(&left_val, &right_val)?),
                    TestCmpOp::Match => {
                        // Decision E: regex match is scalar-only.
                        guard_scalar_test_operands(op, &left_val, &right_val)?;
                        // Regex match — propagate compile errors loudly (no silent false).
                        match regex_match(&left_val, &right_val, false)? {
                            Value::Bool(b) => b,
                            _ => false,
                        }
                    }
                    TestCmpOp::NotMatch => {
                        guard_scalar_test_operands(op, &left_val, &right_val)?;
                        // Regex not match — propagate compile errors loudly (no silent true).
                        match regex_match(&left_val, &right_val, true)? {
                            Value::Bool(b) => b,
                            _ => true,
                        }
                    }
                    TestCmpOp::Gt | TestCmpOp::Lt | TestCmpOp::GtEq | TestCmpOp::LtEq => {
                        // Decision E: ordering is scalar-only.
                        guard_scalar_test_operands(op, &left_val, &right_val)?;
                        // String comparison: `>` `<` `>=` `<=` use lexicographic ordering.
                        let ord = compare_values(&left_val, &right_val)?;
                        match op {
                            TestCmpOp::Gt => ord.is_gt(),
                            TestCmpOp::Lt => ord.is_lt(),
                            TestCmpOp::GtEq => ord.is_ge(),
                            TestCmpOp::LtEq => ord.is_le(),
                            _ => unreachable!(),
                        }
                    }
                    TestCmpOp::NumEq
                    | TestCmpOp::NumNotEq
                    | TestCmpOp::NumGt
                    | TestCmpOp::NumLt
                    | TestCmpOp::NumGtEq
                    | TestCmpOp::NumLtEq => {
                        // Decision E: numeric comparison is scalar-only.
                        guard_scalar_test_operands(op, &left_val, &right_val)?;
                        // Arithmetic comparison: `-eq` `-ne` `-gt` `-lt` `-ge` `-le`
                        // always coerce operands to numbers. Non-numeric strings error.
                        let ord = numeric_compare(&left_val, &right_val)?;
                        match op {
                            TestCmpOp::NumEq => ord.is_eq(),
                            TestCmpOp::NumNotEq => !ord.is_eq(),
                            TestCmpOp::NumGt => ord.is_gt(),
                            TestCmpOp::NumLt => ord.is_lt(),
                            TestCmpOp::NumGtEq => ord.is_ge(),
                            TestCmpOp::NumLtEq => ord.is_le(),
                            _ => unreachable!(),
                        }
                    }
                }
            }
            TestExpr::And { left, right } => {
                // Short-circuit evaluation: evaluate left first
                let left_result = self.eval_test(left)?;
                if !value_to_bool(&left_result) {
                    false // Short-circuit: left is false, don't evaluate right
                } else {
                    value_to_bool(&self.eval_test(right)?)
                }
            }
            TestExpr::Or { left, right } => {
                // Short-circuit evaluation: evaluate left first
                let left_result = self.eval_test(left)?;
                if value_to_bool(&left_result) {
                    true // Short-circuit: left is true, don't evaluate right
                } else {
                    value_to_bool(&self.eval_test(right)?)
                }
            }
            TestExpr::Not { expr } => {
                let result = self.eval_test(expr)?;
                !value_to_bool(&result)
            }
            TestExpr::In { left, right } => {
                let left_val = self.eval(left)?;
                let right_val = self.eval(right)?;
                eval_membership(&left_val, &right_val)?
            }
            TestExpr::NotIn { left, right } => {
                let left_val = self.eval(left)?;
                let right_val = self.eval(right)?;
                !eval_membership(&left_val, &right_val)?
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
        match self.scope.resolve_path(path) {
            Ok(v) => Ok(v),
            // Undefined root keeps the existing path-shaped error.
            Err(super::scope::PathError::UndefinedRoot(_)) => {
                Err(EvalError::InvalidPath(format_path(path)))
            }
            // A loud path error (absence or shape) carries its own actionable
            // message.
            Err(super::scope::PathError::Absence(msg))
            | Err(super::scope::PathError::Shape(msg)) => Err(EvalError::InvalidPath(msg)),
        }
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

    /// Evaluate variable string length (`${#VAR}` / `${#path[sub]}`).
    fn eval_var_length(&self, path: &VarPath) -> EvalResult<Value> {
        resolve_length(self.scope, path)
            .map(Value::Int)
            .map_err(EvalError::InvalidPath)
    }

    /// Evaluate a variable/path with a default (`${VAR:-default}`).
    /// Yields the value if present and non-empty; on absence or emptiness
    /// evaluates the default parts; a shape error stays loud (decision A).
    fn eval_var_with_default(&mut self, path: &VarPath, default: &[StringPart]) -> EvalResult<Value> {
        match resolve_default(self.scope, path).map_err(EvalError::InvalidPath)? {
            Some(value) => Ok(value),
            None => self.eval_interpolated(default),
        }
    }

    /// Evaluate an interpolated string.
    fn eval_interpolated(&mut self, parts: &[StringPart]) -> EvalResult<Value> {
        let mut result = String::new();
        for part in parts {
            match part {
                StringPart::Literal(s) => result.push_str(s),
                StringPart::Var(path) => {
                    match self.scope.resolve_path(path) {
                        Ok(value) => result.push_str(&value_to_string(&value)),
                        // Unset variables expand to empty string (bash-compatible).
                        Err(super::scope::PathError::UndefinedRoot(_)) => {}
                        // A loud path error (absence or shape) is surfaced, never
                        // swallowed to empty.
                        Err(super::scope::PathError::Absence(msg))
                        | Err(super::scope::PathError::Shape(msg)) => {
                            return Err(EvalError::InvalidPath(msg))
                        }
                    }
                }
                StringPart::VarWithDefault { path, default } => {
                    let value = self.eval_var_with_default(path, default)?;
                    result.push_str(&value_to_string(&value));
                }
                StringPart::VarLength(path) => {
                    let value = self.eval_var_length(path)?;
                    result.push_str(&value_to_string(&value));
                }
                StringPart::Positional(n) => {
                    let value = self.eval_positional(*n)?;
                    result.push_str(&value_to_string(&value));
                }
                StringPart::AllArgs => {
                    let value = self.eval_all_args()?;
                    result.push_str(&value_to_string(&value));
                }
                StringPart::ArgCount => {
                    let value = self.eval_arg_count()?;
                    result.push_str(&value_to_string(&value));
                }
                StringPart::Arithmetic(expr) => {
                    // Parse and evaluate the arithmetic expression
                    let value = self.eval_arithmetic_string(expr)?;
                    result.push_str(&value_to_string(&value));
                }
                StringPart::CommandSubst(stmts) => {
                    // Execute the statement block and capture its output
                    let value = self.eval_command_subst(stmts)?;
                    result.push_str(&value_to_string(&value));
                }
                StringPart::LastExitCode => {
                    result.push_str(&self.scope.last_result().code.to_string());
                }
                StringPart::CurrentPid => {
                    result.push_str(&self.scope.pid().to_string());
                }
            }
        }
        Ok(Value::String(result))
    }

    /// Evaluate an arithmetic string expression (from `$((expr))` in interpolation).
    fn eval_arithmetic_string(&mut self, expr: &str) -> EvalResult<Value> {
        // Use the existing arithmetic evaluator
        arithmetic::eval_arithmetic(expr, self.scope)
            .map(Value::Int)
            .map_err(|e| EvalError::ArithmeticError(e.to_string()))
    }

    /// Evaluate a binary operation. The production parser only emits `&&`/`||`
    /// here; comparisons live on `TestExpr::Comparison` and `BinaryOp` is just
    /// the short-circuit logical chain inside conditions.
    fn eval_binary_op(&mut self, left: &Expr, op: BinaryOp, right: &Expr) -> EvalResult<Value> {
        match op {
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
        }
    }

    /// Evaluate command substitution.
    fn eval_command_subst(&mut self, stmts: &[Stmt]) -> EvalResult<Value> {
        let result = self.executor.execute(stmts, self.scope)?;

        // Update $? with the result
        self.scope.set_last_result(result.clone());

        // Return the result as a value (the result object itself)
        // The caller can access .ok, .data, etc.
        Ok(result_to_value(&result))
    }
}

/// Convert a Value to its string representation for interpolation.
/// Coerce a Value into an exit code (i64) for `return`/`exit`.
///
/// Bash semantics: `return $(echo 42)` works because the captured text "42"
/// is parsed as an integer. Non-numeric strings, `Null`, `Json`, and `Blob`
/// are an error — silently coercing to 0 would mask real bugs.
pub fn value_to_exit_code(value: &Value) -> anyhow::Result<i64> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Bool(b) => Ok(if *b { 0 } else { 1 }),
        Value::Float(f) => Ok(*f as i64),
        Value::String(s) => {
            let trimmed = s.trim();
            trimmed.parse::<i64>().map_err(|_| {
                anyhow::anyhow!("numeric argument required: {:?}", s)
            })
        }
        Value::Null | Value::Json(_) | Value::Bytes(_) => {
            anyhow::bail!("numeric argument required (got {:?})", value)
        }
    }
}

/// Length of a value for `${#…}`: element count for a list, key count for a
/// record, and the byte length of the string form for any scalar (unchanged
/// for non-collections). The single source of truth for the three `${#…}`
/// evaluation sites (sync + the two async ones).
pub fn value_length(value: &Value) -> i64 {
    match value {
        Value::Json(serde_json::Value::Array(a)) => a.len() as i64,
        Value::Json(serde_json::Value::Object(o)) => o.len() as i64,
        // Binary length is the byte count, not the length of the text placeholder.
        Value::Bytes(b) => b.len() as i64,
        other => value_to_string(other).len() as i64,
    }
}

/// Decision A: `${path:-default}` yields the default on *absence or emptiness*,
/// never on falsy values. Fires for a JSON `null` and an empty string; NOT for
/// `false`, `0`, an empty list `[]`, or an empty record `{}` — those are present
/// values, and Python-style truthiness leaking into a shell would be a
/// silent-wrong factory. (Unset roots and missing keys are absence too, but
/// they surface as `PathError` before a value exists — see [`resolve_default`].)
pub fn value_defaults_on_emptiness(value: &Value) -> bool {
    match value {
        Value::Null | Value::Json(serde_json::Value::Null) => true,
        Value::String(s) => s.is_empty(),
        _ => false,
    }
}

/// `${#path}` length semantics over the shared path resolver. An unset *root* is
/// length 0 (bash parity for `${#unset}`); a missing key, out-of-bounds index,
/// or shape error is loud (consistent with bare `${u[nope]}` being loud). The
/// resolver borrows the tree — no whole-root clone.
pub fn resolve_length(scope: &Scope, path: &VarPath) -> Result<i64, String> {
    match scope.resolve_path(path) {
        Ok(value) => Ok(value_length(&value)),
        Err(super::scope::PathError::UndefinedRoot(_)) => Ok(0),
        Err(super::scope::PathError::Absence(msg)) | Err(super::scope::PathError::Shape(msg)) => {
            Err(msg)
        }
    }
}

/// `${path:-default}` decision over the shared path resolver (decision A):
/// `Ok(Some(v))` use the value, `Ok(None)` use the default (absence or
/// emptiness), `Err(msg)` a loud shape error the default does NOT suppress. An
/// unset root, a missing key, and an out-of-bounds index all fold into the
/// default; only a wrong-shaped access shouts.
pub fn resolve_default(scope: &Scope, path: &VarPath) -> Result<Option<Value>, String> {
    match scope.resolve_path(path) {
        Ok(value) if value_defaults_on_emptiness(&value) => Ok(None),
        Ok(value) => Ok(Some(value)),
        Err(super::scope::PathError::UndefinedRoot(_))
        | Err(super::scope::PathError::Absence(_)) => Ok(None),
        Err(super::scope::PathError::Shape(msg)) => Err(msg),
    }
}

/// Reject exporting a structured value into an OS env var. A list/record can't
/// cross the process boundary, and kaish will not silently JSON-serialize it into
/// the child's environment. Returns a loud "serialize first" message naming the
/// offending variable, or `None` if every exported value is a scalar. Both
/// external-spawn sites (`kernel.rs`, `dispatch.rs`) call this before `cmd.env`.
pub fn structured_export_error(vars: &[(String, Value)]) -> Option<String> {
    for (name, value) in vars {
        if let Value::Json(j) = value {
            if matches!(j, serde_json::Value::Array(_) | serde_json::Value::Object(_)) {
                let kind = if j.is_array() { "list" } else { "record" };
                return Some(format!(
                    "cannot export '{name}': it holds a {kind}, which can't be an OS environment variable — serialize it explicitly first, e.g. `export {name}=$(tojson ${name})`"
                ));
            }
        }
    }
    None
}

/// True if `value` is a first-class collection (list/record) rather than a
/// scalar. `Value::Json` also carries JSON scalars (numbers/strings/bool/null
/// unwrapped at the value boundary — see `json_to_value_no_envelope`), so
/// this only matches the two container variants.
pub fn is_collection(value: &Value) -> bool {
    matches!(
        value,
        Value::Json(serde_json::Value::Array(_)) | Value::Json(serde_json::Value::Object(_))
    )
}

/// "list" or "record" for a value that `is_collection`. Callers only reach
/// the fallback arm if they call this without checking `is_collection` first.
fn collection_kind(value: &Value) -> &'static str {
    match value {
        Value::Json(serde_json::Value::Array(_)) => "list",
        Value::Json(serde_json::Value::Object(_)) => "record",
        _ => "collection",
    }
}

/// Decision D: reject a bare collection value crossing a process-boundary
/// sink — an external command's argv element, or a redirect target. String
/// interpolation (`"$c"`) already reduces to a `Value::String` (rendering
/// compact JSON) before reaching either sink, so only a *live*,
/// un-interpolated `Value::Json(Array|Object)` — a bare `$c` — trips this.
/// `sink` names the boundary in the message (e.g. "a command argument",
/// "a redirect target"). `None` for scalars. Callers: `kernel.rs`
/// (`build_args_flat`, `try_execute_external`), `dispatch.rs` (`try_external`,
/// test-only twin), `scheduler/pipeline.rs` (`eval_redirect_target`).
pub fn structured_boundary_error(sink: &str, value: &Value) -> Option<String> {
    if is_collection(value) {
        let kind = collection_kind(value);
        Some(format!(
            "cannot use a {kind} as {sink} — serialize it explicitly first, e.g. `cmd $(tojson $x)`"
        ))
    } else {
        None
    }
}

/// Decision E: scalar-only test operators (`-z`/`-n`, `=~`/`!~`, ordering
/// `<`/`>`/`<=`/`>=`, and numeric `-eq`/`-ne`/`-gt`/`-lt`/`-ge`/`-le`) refuse a
/// collection operand loudly rather than falling through a stringify/silent
/// path. `==`/`!=` already error via `values_equal`; `in`/`not in` are the one
/// operator family that legitimately takes a collection operand and must
/// never call this. `None` for scalars. Shared by the sync `eval_test`
/// (interpreter/eval.rs) and the async `eval_test_async` (kernel.rs) so the
/// two `[[ ]]` evaluators can't drift apart on this guard.
pub fn scalar_test_operand_error(op_symbol: &str, value: &Value) -> Option<String> {
    if is_collection(value) {
        let kind = collection_kind(value);
        Some(format!(
            "`{op_symbol}` needs a scalar; got a {kind} — use `${{#x}}` for length, \
             `-list`/`-record` to test shape, or `in` for membership"
        ))
    } else {
        None
    }
}

pub fn value_to_string(value: &Value) -> String {
    match value {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => s.clone(),
        Value::Json(json) => json.to_string(),
        // Binary in a text context: visible placeholder, not raw bytes. The
        // loud-error guard lands with the Phase-2 arg/sink rework.
        Value::Bytes(b) => format!("[binary: {} bytes]", b.len()),
    }
}

/// Convert a Value to its boolean representation.
///
/// - `Bool(b)` → `b`
/// - `Int(0)` → `false`, other ints → `true`
/// - `String("")` → `false`, non-empty → `true`
/// - `Null` → `false`
/// - `Float(0.0)` → `false`, other floats → `true`
/// - `Json(null)` → `false`, `Json([])` → `false`, `Json({})` → `false`, others → `true`
/// - `Bytes(b)` → `b` non-empty (empty bytes are falsy, like `""`)
pub fn value_to_bool(value: &Value) -> bool {
    match value {
        Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(i) => *i != 0,
        Value::Float(f) => *f != 0.0,
        Value::String(s) => !s.is_empty(),
        Value::Json(json) => match json {
            serde_json::Value::Null => false,
            serde_json::Value::Array(arr) => !arr.is_empty(),
            serde_json::Value::Object(obj) => !obj.is_empty(),
            serde_json::Value::Bool(b) => *b,
            serde_json::Value::Number(n) => n.as_f64().map(|f| f != 0.0).unwrap_or(false),
            serde_json::Value::String(s) => !s.is_empty(),
        },
        Value::Bytes(b) => !b.is_empty(), // empty bytes are falsy, like ""
    }
}

/// Expand tilde (~) to home directory.
///
/// - `~` alone → `home`
/// - `~/path` → `home/path`
/// - `~user` → user's home directory (Unix only, reads /etc/passwd)
/// - `~user/path` → user's home directory + path
/// - Other strings are returned unchanged.
///
/// `home` is the kaish session's `HOME` (from the kernel scope), NOT the host
/// process env — the kernel is hermetic and never reads `std::env::var("HOME")`.
/// When `home` is `None` (no `HOME` in scope, e.g. a hermetic embedder that
/// passed empty `initial_vars`), `~` / `~/path` are left unexpanded rather than
/// leaking the host home directory.
pub fn expand_tilde(s: &str, home: Option<&str>) -> String {
    if s == "~" {
        home.map(|h| h.to_string()).unwrap_or_else(|| "~".to_string())
    } else if s.starts_with("~/") {
        match home {
            Some(home) => format!("{}{}", home, &s[1..]),
            None => s.to_string(),
        }
    } else if s.starts_with('~') {
        // Try ~user expansion
        expand_tilde_user(s)
    } else {
        s.to_string()
    }
}

/// Expand ~user to the user's home directory by reading /etc/passwd.
///
/// Reading the system user database is host introspection, so it requires the
/// `host` capability; without it `~user` is left unexpanded (same as non-Unix).
#[cfg(all(unix, feature = "host"))]
fn expand_tilde_user(s: &str) -> String {
    // Extract username from ~user or ~user/path
    let (username, rest) = if let Some(slash_pos) = s[1..].find('/') {
        (&s[1..slash_pos + 1], &s[slash_pos + 1..])
    } else {
        (&s[1..], "")
    };

    if username.is_empty() {
        return s.to_string();
    }

    // Look up user's home directory by reading /etc/passwd
    // Format: username:x:uid:gid:gecos:home:shell
    let passwd = match std::fs::read_to_string("/etc/passwd") {
        Ok(content) => content,
        Err(_) => return s.to_string(),
    };

    for line in passwd.lines() {
        let fields: Vec<&str> = line.split(':').collect();
        if fields.len() >= 6 && fields[0] == username {
            let home_dir = fields[5];
            return if rest.is_empty() {
                home_dir.to_string()
            } else {
                format!("{}{}", home_dir, rest)
            };
        }
    }

    // User not found, return unchanged
    s.to_string()
}

#[cfg(not(all(unix, feature = "host")))]
fn expand_tilde_user(s: &str) -> String {
    // ~user expansion needs the host user database (/etc/passwd), which is
    // gated behind the `host` capability and only meaningful on Unix.
    s.to_string()
}

/// Convert a Value to its string representation, with tilde expansion for paths.
///
/// `home` is the session `HOME` from the kernel scope (see [`expand_tilde`]);
/// `None` leaves `~`/`~/path` unexpanded rather than reading the host env.
pub fn value_to_string_with_tilde(value: &Value, home: Option<&str>) -> String {
    match value {
        Value::String(s) if s.starts_with('~') => expand_tilde(s, home),
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
            VarSegment::Index(idx) => result.push_str(&format!("[{idx}]")),
            VarSegment::Key(k) => result.push_str(&format!("[{k}]")),
            VarSegment::Dynamic(v) => result.push_str(&format!("[${v}]")),
            VarSegment::Slice(a, b) => {
                let s = a.map(|n| n.to_string()).unwrap_or_default();
                let e = b.map(|n| n.to_string()).unwrap_or_default();
                result.push_str(&format!("[{s}:{e}]"));
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
/// - `Json(null)`, `Json([])`, `Json({})` → false
/// - `Blob(_)` → true
/// - Everything else → true
fn is_truthy(value: &Value) -> bool {
    // Delegate to value_to_bool for consistent behavior
    value_to_bool(value)
}

/// Check if two values are equal under `==` (string equality in `[[ ]]`).
///
/// Same-type comparisons stay typed: Int↔Int, Float↔Float (with epsilon),
/// Int↔Float (numeric across the kaish number axis), Json deep equality,
/// Blob by id. For everything else — including mixed String/Number — we
/// stringify both sides and compare. That matches bash's "everything is a
/// string in `[[ a == b ]]`" model and avoids the prior asymmetry where
/// `[[ "01" == 1 ]]` returned true via parse-as-int while `[[ "01" == "1" ]]`
/// returned false. Users wanting numeric equality across stringified
/// numbers should use `-eq`, which coerces via `numeric_compare`.
fn values_equal(left: &Value, right: &Value) -> EvalResult<bool> {
    match (left, right) {
        (Value::Null, Value::Null) => Ok(true),
        (Value::Bool(a), Value::Bool(b)) => Ok(a == b),
        (Value::Int(a), Value::Int(b)) => Ok(a == b),
        (Value::Float(a), Value::Float(b)) => Ok((a - b).abs() < f64::EPSILON),
        (Value::Int(a), Value::Float(b)) | (Value::Float(b), Value::Int(a)) => {
            Ok((*a as f64 - b).abs() < f64::EPSILON)
        }
        (Value::String(a), Value::String(b)) => Ok(a == b),
        (Value::Json(a), Value::Json(b)) => Ok(a == b),
        (Value::Bytes(a), Value::Bytes(b)) => Ok(a == b),
        // A collection (list/record) compared to a scalar is a loud error, never
        // silently false: brackets-only access means `$list` here is the whole
        // structure. Silent-false is exactly the trap `in` exists to close.
        // (A JSON *scalar* is unwrapped at the value boundary, so it never reaches
        // here as `Json`; only Array/Object do.)
        (Value::Json(j), other) | (other, Value::Json(j))
            if matches!(j, serde_json::Value::Array(_) | serde_json::Value::Object(_)) =>
        {
            let kind = if j.is_array() { "list" } else { "record" };
            Err(EvalError::Unsupported(format!(
                "cannot compare a {kind} to a {other_kind} with ==/!= — test membership with `[[ x in $coll ]]`, or compare structures with `jq`",
                other_kind = type_name(other),
            )))
        }
        // Mixed scalars (most commonly String vs Int/Float from a quoted variable
        // against a numeric literal): fall back to string equality.
        _ => Ok(value_to_string(left) == value_to_string(right)),
    }
}

/// Element-scan equality for `in`: unlike [`values_equal`] (which powers
/// `==`/`!=` and errors loudly on a collection-vs-scalar comparison), a
/// membership scan must never abort partway through a list just because one
/// *element* happens to be a nested collection — that element is simply "not
/// a match," the same as any other non-equal element. Two collections are
/// equal only if they're structurally equal (`==` on the underlying JSON); a
/// collection is never equal to a scalar. The loud error for `in` stays
/// reserved for the whole RHS being a scalar (see [`eval_membership`]).
fn element_matches(needle: &Value, element: &Value) -> bool {
    match (needle, element) {
        (Value::Json(a), Value::Json(b)) => a == b,
        (Value::Json(_), _) | (_, Value::Json(_)) => false,
        // Neither side is a collection here, so `values_equal` can't hit its
        // collection-vs-scalar error arm — this can never actually error.
        _ => values_equal(needle, element).unwrap_or(false),
    }
}

/// Evaluate `[[ e in $coll ]]` membership: shape-dispatch on the RHS.
///
/// A list tests element membership (typed equality — reuses [`values_equal`]
/// via [`element_matches`] so `443 in ${servers[web]}` matches a JSON number
/// 443, not just the string "443"; a nested-collection element is just "not a
/// match," never an abort). A record tests key membership (the LHS is
/// stringified, since record keys are always strings). A scalar/string RHS is
/// a loud error — substring tests use `=~`/glob/`case`, never `in` (see
/// docs/arrays-and-hashes.md).
fn eval_membership(needle: &Value, haystack: &Value) -> EvalResult<bool> {
    match haystack {
        Value::Json(serde_json::Value::Array(items)) => {
            for item in items {
                let element = json_to_value_no_envelope(item.clone());
                if element_matches(needle, &element) {
                    return Ok(true);
                }
            }
            Ok(false)
        }
        Value::Json(serde_json::Value::Object(map)) => {
            Ok(map.contains_key(&value_to_string(needle)))
        }
        other => Err(EvalError::Unsupported(format!(
            "`in` requires a list or record on the right-hand side, got {} — substring tests use `=~`, glob (`[[ $s == *sub* ]]`), or `case`",
            type_name(other),
        ))),
    }
}

/// The literal operator spelling for a `TestCmpOp`, used in Decision E's Shape
/// error message so it names the exact operator the user wrote.
fn cmp_op_symbol(op: &TestCmpOp) -> &'static str {
    match op {
        TestCmpOp::Eq => "==",
        TestCmpOp::NotEq => "!=",
        TestCmpOp::Match => "=~",
        TestCmpOp::NotMatch => "!~",
        TestCmpOp::Gt => ">",
        TestCmpOp::Lt => "<",
        TestCmpOp::GtEq => ">=",
        TestCmpOp::LtEq => "<=",
        TestCmpOp::NumEq => "-eq",
        TestCmpOp::NumNotEq => "-ne",
        TestCmpOp::NumGt => "-gt",
        TestCmpOp::NumLt => "-lt",
        TestCmpOp::NumGtEq => "-ge",
        TestCmpOp::NumLtEq => "-le",
    }
}

/// Decision E guard for every `TestExpr::Comparison` operator except `==`/`!=`
/// (already loud via `values_equal`): a single call point so a new comparison
/// operator can't be added without picking up the collection guard.
fn guard_scalar_test_operands(op: &TestCmpOp, left: &Value, right: &Value) -> EvalResult<()> {
    let symbol = cmp_op_symbol(op);
    if let Some(msg) = scalar_test_operand_error(symbol, left) {
        return Err(EvalError::Unsupported(msg));
    }
    if let Some(msg) = scalar_test_operand_error(symbol, right) {
        return Err(EvalError::Unsupported(msg));
    }
    Ok(())
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

/// Coerce a value to a number for arithmetic test ops (`-eq`/`-gt`/…).
///
/// `String` operands are parsed as `i64` then `f64` (matching POSIX `[[ ]]`
/// arithmetic context). Non-numeric strings and non-numeric types error.
enum Num {
    Int(i64),
    Float(f64),
}

fn value_to_num(value: &Value) -> EvalResult<Num> {
    match value {
        Value::Int(n) => Ok(Num::Int(*n)),
        Value::Float(f) => Ok(Num::Float(*f)),
        Value::String(s) => {
            let t = s.trim();
            if let Ok(n) = t.parse::<i64>() {
                Ok(Num::Int(n))
            } else if let Ok(f) = t.parse::<f64>() {
                Ok(Num::Float(f))
            } else {
                Err(EvalError::TypeError {
                    expected: "numeric operand",
                    got: format!("non-numeric string {:?}", s),
                })
            }
        }
        _ => Err(EvalError::TypeError {
            expected: "numeric operand",
            got: type_name(value).to_string(),
        }),
    }
}

/// Numeric ordering for `[[ -eq ]]`/`-gt`/`-lt`/`-ge`/`-le`/`-ne`.
/// Coerces string operands via `value_to_num`.
fn numeric_compare(left: &Value, right: &Value) -> EvalResult<std::cmp::Ordering> {
    let l = value_to_num(left)?;
    let r = value_to_num(right)?;
    match (l, r) {
        (Num::Int(a), Num::Int(b)) => Ok(a.cmp(&b)),
        (Num::Float(a), Num::Float(b)) => a
            .partial_cmp(&b)
            .ok_or_else(|| EvalError::ArithmeticError("NaN comparison".into())),
        (Num::Int(a), Num::Float(b)) => (a as f64)
            .partial_cmp(&b)
            .ok_or_else(|| EvalError::ArithmeticError("NaN comparison".into())),
        (Num::Float(a), Num::Int(b)) => a
            .partial_cmp(&(b as f64))
            .ok_or_else(|| EvalError::ArithmeticError("NaN comparison".into())),
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
        Value::Json(_) => "json",
        Value::Bytes(_) => "bytes",
    }
}

/// Convert an ExecResult to a Value for command substitution return.
///
/// Prefers structured data if available (for iteration in for loops),
/// otherwise returns stdout (trimmed) as a string. `$?` exposes the exit
/// code as an int; `kaish-last` exposes the previous command's structured
/// data or stdout as text.
fn result_to_value(result: &ExecResult) -> Value {
    // Prefer structured data if available (enables `for i in $(cmd)` iteration)
    if let Some(data) = &result.data {
        return data.clone();
    }
    // Otherwise return stdout as single string (NO implicit splitting).
    // Strip trailing newlines only, not all trailing whitespace — same trim as
    // the async kernel command-subst path (`kernel.rs` Expr::CommandSubst) and
    // the quoted `"$(…)"` interpolation, so this sync evaluator (dead today —
    // it runs under `NoOpExecutor` — but a trap for a future non-async embedder)
    // can't silently diverge.
    Value::String(result.text_out().trim_end_matches('\n').to_string())
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
#[allow(clippy::approx_constant)]
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
        use crate::ast::Command;

        let mut scope = Scope::new();
        let expr = Expr::CommandSubst(vec![Stmt::Command(Command {
            name: "echo".into(),
            args: vec![],
            redirects: vec![],
        })]);

        assert!(matches!(
            eval_expr(&expr, &mut scope),
            Err(EvalError::NoExecutor)
        ));
    }

    #[test]
    fn eval_last_result_bare() {
        // Bare $? returns the exit code as an int (POSIX-shaped).
        // Field access on $? was removed — `kaish-last` covers structured data.
        let mut scope = Scope::new();
        scope.set_last_result(ExecResult::failure(42, "test error"));

        let expr = Expr::VarRef(VarPath {
            segments: vec![VarSegment::Field("?".into())],
        });
        assert_eq!(eval_expr(&expr, &mut scope), Ok(Value::Int(42)));
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
                VarSegment::Field("X".into()),
                VarSegment::Field("field".into()),
            ],
        };
        assert_eq!(format_path(&path), "${X.field}");
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
        // HOME comes from the session scope, not the host env.
        let home = "/home/session";
        assert_eq!(expand_tilde("~", Some(home)), home);
        assert_eq!(expand_tilde("~/foo", Some(home)), format!("{}/foo", home));
        assert_eq!(
            expand_tilde("~/foo/bar", Some(home)),
            format!("{}/foo/bar", home)
        );
    }

    #[test]
    fn expand_tilde_hermetic_no_home_does_not_leak_host() {
        // With no HOME in scope (hermetic embedder), `~` must NOT fall back to
        // the host home directory — it stays literal.
        assert_eq!(expand_tilde("~", None), "~");
        assert_eq!(expand_tilde("~/foo", None), "~/foo");
    }

    #[test]
    fn expand_tilde_passthrough() {
        // These should not be expanded
        assert_eq!(expand_tilde("/home/user", Some("/h")), "/home/user");
        assert_eq!(expand_tilde("foo~bar", Some("/h")), "foo~bar");
        assert_eq!(expand_tilde("", Some("/h")), "");
    }

    #[test]
    #[cfg(all(unix, feature = "host"))]
    fn expand_tilde_user() {
        // Test ~root expansion (root user exists on all Unix systems).
        // `~user` reads /etc/passwd and ignores the session HOME, so pass None.
        let expanded = expand_tilde("~root", None);
        // root's home is typically /root or /var/root (macOS)
        assert!(
            expanded == "/root" || expanded == "/var/root",
            "expected /root or /var/root, got: {}",
            expanded
        );

        // Test ~root/subpath
        let expanded_path = expand_tilde("~root/subdir", None);
        assert!(
            expanded_path == "/root/subdir" || expanded_path == "/var/root/subdir",
            "expected /root/subdir or /var/root/subdir, got: {}",
            expanded_path
        );

        // Nonexistent user should remain unchanged
        let nonexistent = expand_tilde("~nonexistent_user_12345", None);
        assert_eq!(nonexistent, "~nonexistent_user_12345");
    }

    #[test]
    fn value_to_string_with_tilde_expansion() {
        // HOME comes from the session scope, not the host env.
        let val = Value::String("~/test".into());
        assert_eq!(
            value_to_string_with_tilde(&val, Some("/home/session")),
            "/home/session/test"
        );
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

        let expr = Expr::VarLength(VarPath::simple("NAME"));
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::Int(5));
    }

    #[test]
    fn eval_var_length_empty_string() {
        let mut scope = Scope::new();
        scope.set("EMPTY", Value::String("".into()));

        let expr = Expr::VarLength(VarPath::simple("EMPTY"));
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::Int(0));
    }

    #[test]
    fn eval_var_length_unset() {
        let mut scope = Scope::new();

        // Unset variable has length 0
        let expr = Expr::VarLength(VarPath::simple("MISSING"));
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::Int(0));
    }

    #[test]
    fn eval_var_length_int() {
        let mut scope = Scope::new();
        scope.set("NUM", Value::Int(12345));

        // Length of the string representation
        let expr = Expr::VarLength(VarPath::simple("NUM"));
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::Int(5)); // "12345" has length 5
    }

    #[test]
    fn eval_var_with_default_set() {
        let mut scope = Scope::new();
        scope.set("NAME", Value::String("Alice".into()));

        // Variable is set, return its value
        let expr = Expr::VarWithDefault {
            path: VarPath::simple("NAME"),
            default: vec![StringPart::Literal("default".into())],
        };
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::String("Alice".into()));
    }

    #[test]
    fn eval_var_with_default_unset() {
        let mut scope = Scope::new();

        // Variable is unset, return default
        let expr = Expr::VarWithDefault {
            path: VarPath::simple("MISSING"),
            default: vec![StringPart::Literal("fallback".into())],
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
            path: VarPath::simple("EMPTY"),
            default: vec![StringPart::Literal("not empty".into())],
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
            path: VarPath::simple("NUM"),
            default: vec![StringPart::Literal("default".into())],
        };
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn eval_unset_variable_is_empty() {
        let mut scope = Scope::new();
        let parts = vec![
            StringPart::Literal("prefix:".into()),
            StringPart::Var(VarPath::simple("UNSET")),
            StringPart::Literal(":suffix".into()),
        ];
        let expr = Expr::Interpolated(parts);
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::String("prefix::suffix".into()));
    }

    #[test]
    fn eval_unset_variable_multiple() {
        let mut scope = Scope::new();
        scope.set("SET", Value::String("hello".into()));
        let parts = vec![
            StringPart::Var(VarPath::simple("UNSET1")),
            StringPart::Literal("-".into()),
            StringPart::Var(VarPath::simple("SET")),
            StringPart::Literal("-".into()),
            StringPart::Var(VarPath::simple("UNSET2")),
        ];
        let expr = Expr::Interpolated(parts);
        let result = eval_expr(&expr, &mut scope).unwrap();
        assert_eq!(result, Value::String("-hello-".into()));
    }

    // ── Overnight-review fixes (2026-07-02) ────────────────────────────────

    #[test]
    fn values_equal_scalars_still_work() {
        assert_eq!(
            values_equal(&Value::String("x".into()), &Value::String("x".into())),
            Ok(true)
        );
        // Mixed scalar fallthrough (String vs Int) stays string-equality.
        assert_eq!(
            values_equal(&Value::String("42".into()), &Value::Int(42)),
            Ok(true)
        );
    }

    #[test]
    fn values_equal_collection_vs_scalar_is_loud() {
        let list = Value::Json(serde_json::json!(["a", "b"]));
        let record = Value::Json(serde_json::json!({"k": 1}));
        assert!(
            matches!(values_equal(&list, &Value::String("banana".into())), Err(EvalError::Unsupported(_))),
            "list vs scalar must be a loud error, never silently false"
        );
        // Order-independent: scalar on the left too.
        assert!(matches!(
            values_equal(&Value::String("x".into()), &record),
            Err(EvalError::Unsupported(_))
        ));
    }

    #[test]
    fn values_equal_collection_vs_collection_is_structural() {
        // Two collections still compare structurally (records order-insensitive).
        let a = Value::Json(serde_json::json!({"a": 1, "b": 2}));
        let b = Value::Json(serde_json::json!({"b": 2, "a": 1}));
        assert_eq!(values_equal(&a, &b), Ok(true));
    }

    #[test]
    fn value_length_of_bytes_is_byte_count() {
        assert_eq!(value_length(&Value::Bytes(vec![1, 2, 3])), 3);
    }

    #[test]
    fn structured_export_error_flags_collections_passes_scalars() {
        // Scalars are fine.
        let scalars = vec![
            ("A".to_string(), Value::String("x".into())),
            ("B".to_string(), Value::Int(1)),
        ];
        assert!(structured_export_error(&scalars).is_none());
        // A record is refused with a `tojson` hint.
        let with_record = vec![(
            "CFG".to_string(),
            Value::Json(serde_json::json!({"port": 8080})),
        )];
        let msg = structured_export_error(&with_record).expect("record must be refused");
        assert!(msg.contains("CFG") && msg.contains("tojson"), "got: {msg}");
        // A list too.
        let with_list = vec![("XS".to_string(), Value::Json(serde_json::json!([1, 2])))];
        assert!(structured_export_error(&with_list).is_some());
    }

    #[test]
    fn defaults_on_emptiness_matches_decision_a() {
        // Default fires on absence/emptiness (null, empty string) — NEVER on a
        // falsy-but-present value (false, 0, [], {}).
        assert!(value_defaults_on_emptiness(&Value::Null));
        assert!(value_defaults_on_emptiness(&Value::Json(serde_json::Value::Null)));
        assert!(value_defaults_on_emptiness(&Value::String(String::new())));
        assert!(!value_defaults_on_emptiness(&Value::Bool(false)));
        assert!(!value_defaults_on_emptiness(&Value::Int(0)));
        assert!(!value_defaults_on_emptiness(&Value::Json(serde_json::json!([]))));
        assert!(!value_defaults_on_emptiness(&Value::Json(serde_json::json!({}))));
        assert!(!value_defaults_on_emptiness(&Value::String("x".into())));
    }

    #[test]
    fn subscripted_length_and_default_resolve_the_path() {
        // Path-aware length and default via the shared resolver — the old
        // placeholder "bind first" errors are gone; the forms now work.
        let mut scope = Scope::new();
        scope.set("u", Value::Json(serde_json::json!({"tags": ["a", "b"]})));
        let len = eval_expr(
            &Expr::VarLength(crate::parser::parse_varpath("${u[tags]}")),
            &mut scope,
        )
        .unwrap();
        assert_eq!(len, Value::Int(2));

        scope.set("cfg", Value::Json(serde_json::json!({"port": 9000})));
        // A present value wins over the default.
        let val = eval_expr(
            &Expr::VarWithDefault {
                path: crate::parser::parse_varpath("${cfg[port]}"),
                default: vec![StringPart::Literal("8080".into())],
            },
            &mut scope,
        )
        .unwrap();
        assert_eq!(value_to_string(&val), "9000");

        // A missing key falls to the default (absence — decision A).
        let missing = eval_expr(
            &Expr::VarWithDefault {
                path: crate::parser::parse_varpath("${cfg[nope]}"),
                default: vec![StringPart::Literal("8080".into())],
            },
            &mut scope,
        )
        .unwrap();
        assert_eq!(value_to_string(&missing), "8080");

        // A shape error stays loud even with `:-` (an integer index on a record).
        let err = eval_expr(
            &Expr::VarWithDefault {
                path: crate::parser::parse_varpath("${cfg[0]}"),
                default: vec![StringPart::Literal("x".into())],
            },
            &mut scope,
        )
        .unwrap_err();
        assert!(matches!(err, EvalError::InvalidPath(_)), "got: {err}");
    }
}
