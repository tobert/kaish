//! AST walker for pre-execution validation.

use std::collections::HashMap;

use crate::ast::{
    Arg, Assignment, CaseBranch, CaseStmt, Command, Expr, ForLoop, IfStmt, Pipeline, Program,
    Stmt, StringPart, TestExpr, ToolDef, VarPath, VarSegment, WhileLoop, Value,
};
use crate::tools::{ToolArgs, ToolRegistry};

use super::issue::{IssueCode, ValidationIssue};
use super::scope_tracker::ScopeTracker;

/// AST validator that checks for issues before execution.
pub struct Validator<'a> {
    /// Reference to the tool registry.
    registry: &'a ToolRegistry,
    /// User-defined tools.
    user_tools: &'a HashMap<String, ToolDef>,
    /// Variable scope tracker.
    scope: ScopeTracker,
    /// Current loop nesting depth.
    loop_depth: usize,
    /// Current function nesting depth.
    function_depth: usize,
    /// Collected validation issues.
    issues: Vec<ValidationIssue>,
}

impl<'a> Validator<'a> {
    /// Create a new validator.
    pub fn new(registry: &'a ToolRegistry, user_tools: &'a HashMap<String, ToolDef>) -> Self {
        Self {
            registry,
            user_tools,
            scope: ScopeTracker::new(),
            loop_depth: 0,
            function_depth: 0,
            issues: Vec::new(),
        }
    }

    /// Validate a program and return all issues found.
    pub fn validate(mut self, program: &Program) -> Vec<ValidationIssue> {
        for stmt in &program.statements {
            self.validate_stmt(stmt);
        }
        self.issues
    }

    /// Validate a single statement.
    fn validate_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Assignment(assign) => self.validate_assignment(assign),
            Stmt::Command(cmd) => self.validate_command(cmd),
            Stmt::Pipeline(pipe) => self.validate_pipeline(pipe),
            Stmt::If(if_stmt) => self.validate_if(if_stmt),
            Stmt::For(for_loop) => self.validate_for(for_loop),
            Stmt::While(while_loop) => self.validate_while(while_loop),
            Stmt::Case(case_stmt) => self.validate_case(case_stmt),
            Stmt::Break(levels) => self.validate_break(*levels),
            Stmt::Continue(levels) => self.validate_continue(*levels),
            Stmt::Return(expr) => self.validate_return(expr.as_ref()),
            Stmt::Exit(expr) => {
                if let Some(e) = expr {
                    self.validate_expr(e);
                }
            }
            Stmt::ToolDef(tool_def) => self.validate_tool_def(tool_def),
            Stmt::Test(test_expr) => self.validate_test(test_expr),
            Stmt::AndChain { left, right } | Stmt::OrChain { left, right } => {
                self.validate_stmt(left);
                self.validate_stmt(right);
            }
            Stmt::Empty => {}
        }
    }

    /// Validate an assignment statement.
    fn validate_assignment(&mut self, assign: &Assignment) {
        // Validate the value expression
        self.validate_expr(&assign.value);
        // Bind the variable name in scope
        self.scope.bind(&assign.name);
    }

    /// Validate a command invocation.
    fn validate_command(&mut self, cmd: &Command) {
        // Skip source/. commands - they're dynamic
        if cmd.name == "source" || cmd.name == "." {
            return;
        }

        // Skip dynamic command names (variable expansions)
        if !is_static_command_name(&cmd.name) {
            return;
        }

        // Check if command exists
        let is_builtin = self.registry.contains(&cmd.name);
        let is_user_tool = self.user_tools.contains_key(&cmd.name);
        let is_special = is_special_command(&cmd.name);

        if !is_builtin && !is_user_tool && !is_special {
            // Warning only - command might be a script in PATH or external tool
            self.issues.push(ValidationIssue::warning(
                IssueCode::UndefinedCommand,
                format!("command '{}' not found in builtin registry", cmd.name),
            ).with_suggestion("this may be a script in PATH or external command"));
        }

        // Validate arguments expressions
        for arg in &cmd.args {
            self.validate_arg(arg);
        }

        // If we have a schema, validate args against it
        if let Some(tool) = self.registry.get(&cmd.name) {
            let tool_args = build_tool_args_for_validation(&cmd.args);
            let tool_issues = tool.validate(&tool_args);
            self.issues.extend(tool_issues);
        } else if let Some(user_tool) = self.user_tools.get(&cmd.name) {
            // Validate against user-defined tool parameters
            self.validate_user_tool_args(user_tool, &cmd.args);
        }

        // Validate redirects
        for redirect in &cmd.redirects {
            self.validate_expr(&redirect.target);
        }
    }

    /// Validate a command argument.
    fn validate_arg(&mut self, arg: &Arg) {
        match arg {
            Arg::Positional(expr) => self.validate_expr(expr),
            Arg::Named { value, .. } => self.validate_expr(value),
            Arg::ShortFlag(_) | Arg::LongFlag(_) | Arg::DoubleDash => {}
        }
    }

    /// Validate a pipeline.
    fn validate_pipeline(&mut self, pipe: &Pipeline) {
        for cmd in &pipe.commands {
            self.validate_command(cmd);
        }
    }

    /// Validate an if statement.
    fn validate_if(&mut self, if_stmt: &IfStmt) {
        self.validate_expr(&if_stmt.condition);

        self.scope.push_frame();
        for stmt in &if_stmt.then_branch {
            self.validate_stmt(stmt);
        }
        self.scope.pop_frame();

        if let Some(else_branch) = &if_stmt.else_branch {
            self.scope.push_frame();
            for stmt in else_branch {
                self.validate_stmt(stmt);
            }
            self.scope.pop_frame();
        }
    }

    /// Validate a for loop.
    fn validate_for(&mut self, for_loop: &ForLoop) {
        // Validate item expressions
        for item in &for_loop.items {
            self.validate_expr(item);
        }

        self.loop_depth += 1;
        self.scope.push_frame();

        // Bind loop variable
        self.scope.bind(&for_loop.variable);

        for stmt in &for_loop.body {
            self.validate_stmt(stmt);
        }

        self.scope.pop_frame();
        self.loop_depth -= 1;
    }

    /// Validate a while loop.
    fn validate_while(&mut self, while_loop: &WhileLoop) {
        self.validate_expr(&while_loop.condition);

        self.loop_depth += 1;
        self.scope.push_frame();

        for stmt in &while_loop.body {
            self.validate_stmt(stmt);
        }

        self.scope.pop_frame();
        self.loop_depth -= 1;
    }

    /// Validate a case statement.
    fn validate_case(&mut self, case_stmt: &CaseStmt) {
        self.validate_expr(&case_stmt.expr);

        for branch in &case_stmt.branches {
            self.validate_case_branch(branch);
        }
    }

    /// Validate a case branch.
    fn validate_case_branch(&mut self, branch: &CaseBranch) {
        self.scope.push_frame();
        for stmt in &branch.body {
            self.validate_stmt(stmt);
        }
        self.scope.pop_frame();
    }

    /// Validate a break statement.
    fn validate_break(&mut self, levels: Option<usize>) {
        if self.loop_depth == 0 {
            self.issues.push(ValidationIssue::error(
                IssueCode::BreakOutsideLoop,
                "break used outside of a loop",
            ));
        } else if let Some(n) = levels {
            if n > self.loop_depth {
                self.issues.push(ValidationIssue::warning(
                    IssueCode::BreakOutsideLoop,
                    format!(
                        "break {} exceeds loop nesting depth {}",
                        n, self.loop_depth
                    ),
                ));
            }
        }
    }

    /// Validate a continue statement.
    fn validate_continue(&mut self, levels: Option<usize>) {
        if self.loop_depth == 0 {
            self.issues.push(ValidationIssue::error(
                IssueCode::BreakOutsideLoop,
                "continue used outside of a loop",
            ));
        } else if let Some(n) = levels {
            if n > self.loop_depth {
                self.issues.push(ValidationIssue::warning(
                    IssueCode::BreakOutsideLoop,
                    format!(
                        "continue {} exceeds loop nesting depth {}",
                        n, self.loop_depth
                    ),
                ));
            }
        }
    }

    /// Validate a return statement.
    fn validate_return(&mut self, expr: Option<&Box<Expr>>) {
        if let Some(e) = expr {
            self.validate_expr(e);
        }

        if self.function_depth == 0 {
            self.issues.push(ValidationIssue::error(
                IssueCode::ReturnOutsideFunction,
                "return used outside of a function",
            ));
        }
    }

    /// Validate a tool definition.
    fn validate_tool_def(&mut self, tool_def: &ToolDef) {
        self.function_depth += 1;
        self.scope.push_frame();

        // Bind parameters
        for param in &tool_def.params {
            self.scope.bind(&param.name);
            // Validate default expressions
            if let Some(default) = &param.default {
                self.validate_expr(default);
            }
        }

        // Validate body
        for stmt in &tool_def.body {
            self.validate_stmt(stmt);
        }

        self.scope.pop_frame();
        self.function_depth -= 1;
    }

    /// Validate a test expression.
    fn validate_test(&mut self, test: &TestExpr) {
        match test {
            TestExpr::FileTest { path, .. } => self.validate_expr(path),
            TestExpr::StringTest { value, .. } => self.validate_expr(value),
            TestExpr::Comparison { left, right, .. } => {
                self.validate_expr(left);
                self.validate_expr(right);
            }
        }
    }

    /// Validate an expression.
    fn validate_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(_) => {}
            Expr::VarRef(path) => self.validate_var_ref(path),
            Expr::Interpolated(parts) => {
                for part in parts {
                    self.validate_string_part(part);
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                self.validate_expr(left);
                self.validate_expr(right);
            }
            Expr::CommandSubst(pipeline) => self.validate_pipeline(pipeline),
            Expr::Test(test) => self.validate_test(test),
            Expr::Positional(_) | Expr::AllArgs | Expr::ArgCount => {}
            Expr::VarLength(name) => self.check_var_defined(name),
            Expr::VarWithDefault { name, .. } => {
                // Don't warn - default handles undefined case
                let _ = name;
            }
            Expr::Arithmetic(_) => {
                // Arithmetic parsing is done at runtime
            }
            Expr::Command(cmd) => self.validate_command(cmd),
            Expr::LastExitCode | Expr::CurrentPid => {}
        }
    }

    /// Validate a variable reference.
    fn validate_var_ref(&mut self, path: &VarPath) {
        if let Some(VarSegment::Field(name)) = path.segments.first() {
            self.check_var_defined(name);
        }
    }

    /// Validate a string interpolation part.
    fn validate_string_part(&mut self, part: &StringPart) {
        match part {
            StringPart::Literal(_) => {}
            StringPart::Var(path) => self.validate_var_ref(path),
            StringPart::VarWithDefault { .. } => {}
            StringPart::VarLength(name) => self.check_var_defined(name),
            StringPart::Positional(_) | StringPart::AllArgs | StringPart::ArgCount => {}
            StringPart::Arithmetic(_) => {} // Arithmetic expressions are validated at eval time
            StringPart::LastExitCode | StringPart::CurrentPid => {}
        }
    }

    /// Check if a variable is defined and warn if not.
    fn check_var_defined(&mut self, name: &str) {
        // Skip underscore-prefixed vars (external/unchecked convention)
        if ScopeTracker::should_skip_undefined_check(name) {
            return;
        }

        if !self.scope.is_bound(name) {
            self.issues.push(ValidationIssue::warning(
                IssueCode::PossiblyUndefinedVariable,
                format!("variable '{}' may be undefined", name),
            ).with_suggestion(format!("use ${{{}:-default}} if this is intentional", name)));
        }
    }

    /// Validate arguments against a user-defined tool's parameters.
    fn validate_user_tool_args(&mut self, tool_def: &ToolDef, args: &[Arg]) {
        let positional_count = args
            .iter()
            .filter(|a| matches!(a, Arg::Positional(_)))
            .count();

        let required_count = tool_def
            .params
            .iter()
            .filter(|p| p.default.is_none())
            .count();

        if positional_count < required_count {
            self.issues.push(ValidationIssue::error(
                IssueCode::MissingRequiredArg,
                format!(
                    "'{}' requires {} arguments, got {}",
                    tool_def.name, required_count, positional_count
                ),
            ));
        }
    }
}

/// Check if a command name is static (not a variable expansion).
fn is_static_command_name(name: &str) -> bool {
    !name.starts_with('$') && !name.contains("$(")
}

/// Check if a command is a special built-in that we don't validate.
fn is_special_command(name: &str) -> bool {
    matches!(
        name,
        "true" | "false" | ":" | "test" | "[" | "[["
    )
}

/// Build ToolArgs from AST Args for validation purposes.
///
/// This is a simplified version that doesn't evaluate expressions -
/// it uses placeholder values since we only care about argument structure.
pub fn build_tool_args_for_validation(args: &[Arg]) -> ToolArgs {
    let mut tool_args = ToolArgs::new();

    for arg in args {
        match arg {
            Arg::Positional(expr) => {
                tool_args.positional.push(expr_to_placeholder(expr));
            }
            Arg::Named { key, value } => {
                tool_args.named.insert(key.clone(), expr_to_placeholder(value));
            }
            Arg::ShortFlag(flag) => {
                tool_args.flags.insert(flag.clone());
            }
            Arg::LongFlag(flag) => {
                tool_args.flags.insert(flag.clone());
            }
            Arg::DoubleDash => {}
        }
    }

    tool_args
}

/// Convert an expression to a placeholder value for validation.
///
/// For literal values, return the actual value.
/// For dynamic expressions (var refs, command subst), return a placeholder.
fn expr_to_placeholder(expr: &Expr) -> Value {
    match expr {
        Expr::Literal(val) => val.clone(),
        Expr::Interpolated(parts) if parts.len() == 1 => {
            if let StringPart::Literal(s) = &parts[0] {
                Value::String(s.clone())
            } else {
                Value::String("<dynamic>".to_string())
            }
        }
        // For variable refs, command substitution, etc. - use placeholder
        _ => Value::String("<dynamic>".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tools::{register_builtins, ToolRegistry};

    fn make_validator() -> (ToolRegistry, HashMap<String, ToolDef>) {
        let mut registry = ToolRegistry::new();
        register_builtins(&mut registry);
        let user_tools = HashMap::new();
        (registry, user_tools)
    }

    #[test]
    fn validates_undefined_command() {
        let (registry, user_tools) = make_validator();
        let validator = Validator::new(&registry, &user_tools);

        let program = Program {
            statements: vec![Stmt::Command(Command {
                name: "nonexistent_command".to_string(),
                args: vec![],
                redirects: vec![],
            })],
        };

        let issues = validator.validate(&program);
        assert!(!issues.is_empty());
        assert!(issues.iter().any(|i| i.code == IssueCode::UndefinedCommand));
    }

    #[test]
    fn validates_known_command() {
        let (registry, user_tools) = make_validator();
        let validator = Validator::new(&registry, &user_tools);

        let program = Program {
            statements: vec![Stmt::Command(Command {
                name: "echo".to_string(),
                args: vec![Arg::Positional(Expr::Literal(Value::String(
                    "hello".to_string(),
                )))],
                redirects: vec![],
            })],
        };

        let issues = validator.validate(&program);
        // echo should not produce an undefined command error
        assert!(!issues.iter().any(|i| i.code == IssueCode::UndefinedCommand));
    }

    #[test]
    fn validates_break_outside_loop() {
        let (registry, user_tools) = make_validator();
        let validator = Validator::new(&registry, &user_tools);

        let program = Program {
            statements: vec![Stmt::Break(None)],
        };

        let issues = validator.validate(&program);
        assert!(issues.iter().any(|i| i.code == IssueCode::BreakOutsideLoop));
    }

    #[test]
    fn validates_break_inside_loop() {
        let (registry, user_tools) = make_validator();
        let validator = Validator::new(&registry, &user_tools);

        let program = Program {
            statements: vec![Stmt::For(ForLoop {
                variable: "i".to_string(),
                items: vec![Expr::Literal(Value::String("1 2 3".to_string()))],
                body: vec![Stmt::Break(None)],
            })],
        };

        let issues = validator.validate(&program);
        // Break inside loop should NOT produce an error
        assert!(!issues.iter().any(|i| i.code == IssueCode::BreakOutsideLoop));
    }

    #[test]
    fn validates_undefined_variable() {
        let (registry, user_tools) = make_validator();
        let validator = Validator::new(&registry, &user_tools);

        let program = Program {
            statements: vec![Stmt::Command(Command {
                name: "echo".to_string(),
                args: vec![Arg::Positional(Expr::VarRef(VarPath::simple(
                    "UNDEFINED_VAR",
                )))],
                redirects: vec![],
            })],
        };

        let issues = validator.validate(&program);
        assert!(issues
            .iter()
            .any(|i| i.code == IssueCode::PossiblyUndefinedVariable));
    }

    #[test]
    fn validates_defined_variable() {
        let (registry, user_tools) = make_validator();
        let validator = Validator::new(&registry, &user_tools);

        let program = Program {
            statements: vec![
                // First assign the variable
                Stmt::Assignment(Assignment {
                    name: "MY_VAR".to_string(),
                    value: Expr::Literal(Value::String("value".to_string())),
                    local: false,
                }),
                // Then use it
                Stmt::Command(Command {
                    name: "echo".to_string(),
                    args: vec![Arg::Positional(Expr::VarRef(VarPath::simple("MY_VAR")))],
                    redirects: vec![],
                }),
            ],
        };

        let issues = validator.validate(&program);
        // Should NOT warn about MY_VAR
        assert!(!issues
            .iter()
            .any(|i| i.code == IssueCode::PossiblyUndefinedVariable
                && i.message.contains("MY_VAR")));
    }

    #[test]
    fn skips_underscore_prefixed_vars() {
        let (registry, user_tools) = make_validator();
        let validator = Validator::new(&registry, &user_tools);

        let program = Program {
            statements: vec![Stmt::Command(Command {
                name: "echo".to_string(),
                args: vec![Arg::Positional(Expr::VarRef(VarPath::simple("_EXTERNAL")))],
                redirects: vec![],
            })],
        };

        let issues = validator.validate(&program);
        // Should NOT warn about _EXTERNAL
        assert!(!issues
            .iter()
            .any(|i| i.code == IssueCode::PossiblyUndefinedVariable));
    }

    #[test]
    fn builtin_vars_are_defined() {
        let (registry, user_tools) = make_validator();
        let validator = Validator::new(&registry, &user_tools);

        let program = Program {
            statements: vec![Stmt::Command(Command {
                name: "echo".to_string(),
                args: vec![
                    Arg::Positional(Expr::VarRef(VarPath::simple("HOME"))),
                    Arg::Positional(Expr::VarRef(VarPath::simple("PATH"))),
                    Arg::Positional(Expr::VarRef(VarPath::simple("PWD"))),
                ],
                redirects: vec![],
            })],
        };

        let issues = validator.validate(&program);
        // Should NOT warn about HOME, PATH, PWD
        assert!(!issues
            .iter()
            .any(|i| i.code == IssueCode::PossiblyUndefinedVariable));
    }
}
