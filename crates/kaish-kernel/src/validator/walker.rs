//! AST walker for pre-execution validation.

use std::collections::{HashMap, HashSet};

use crate::ast::{
    Arg, Assignment, CaseBranch, CaseStmt, Command, Expr, ForLoop, IfStmt, ListElem, Pipeline,
    PipelineStage, Program, SpannedPart, Stmt, StringPart, TestExpr, ToolDef, VarPath, VarSegment,
    WhileLoop,
    Value,
};
use crate::kernel::{bind_glued_short_value, push_repeatable_value};
use crate::scheduler::{is_bool_type, schema_param_lookup};
use crate::validator::issue::Span;
use crate::tools::{is_global_output_flag, ArgBinding, ToolArgs, ToolRegistry, ToolSchema};
use kaish_types::CommandKind;

use super::issue::{IssueCode, ValidationIssue};
#[cfg(test)]
use super::issue::Severity;
use super::scope_tracker::ScopeTracker;

/// AST validator that checks for issues before execution.
pub struct Validator<'a> {
    /// Reference to the tool registry.
    registry: &'a ToolRegistry,
    /// User-defined tools.
    user_tools: &'a HashMap<String, ToolDef>,
    /// The kernel's name-sorted schema catalog (GH #256); `&[]` when there is
    /// none. `binary_search_by` returns `Ok` only on an equal compare, so an
    /// empty or unsorted catalog can only miss — and a miss falls back to
    /// `tool.schema()`. A *stale* entry is different: it has the right name, so
    /// it hits, and a hit is what skips the fallback. The kernel cannot go
    /// stale — the registry is frozen before `set_tool_schemas` snapshots it —
    /// but an embedder hand-building a catalog can. A hit also assumes
    /// `tool.name() == tool.schema().name`, which nothing enforces.
    catalog: &'a [ToolSchema],
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
    ///
    /// `catalog` is the kernel's name-sorted `Arc<[ToolSchema]>` (see
    /// `ExecContext::tool_schemas`); pass `&[]` when none is available.
    pub fn new(
        registry: &'a ToolRegistry,
        user_tools: &'a HashMap<String, ToolDef>,
        catalog: &'a [ToolSchema],
    ) -> Self {
        Self {
            registry,
            user_tools,
            catalog,
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
            Stmt::Return(expr) => self.validate_return(expr.as_deref()),
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
            Stmt::EnvScoped { assignments, body } => {
                // Validate each prefix assignment (values + bind the name so the
                // body's references resolve), then the command it scopes.
                for assign in assignments {
                    self.validate_assignment(assign);
                }
                self.validate_stmt(body);
            }
            Stmt::Empty => {}
        }
    }

    /// Validate an assignment statement.
    ///
    /// A plain `NAME=value` is checked for a dotted target (`user.email=x`) —
    /// kaish is brackets-only for collection access, and the `Ident` token
    /// admits `.` for other uses (filenames), so a dotted assignment target
    /// is caught here rather than by tightening the lexer regex. A
    /// subscripted lvalue (`x[k]=v`) additionally requires the root to
    /// already be bound — a path-set never autovivifies the root. See
    /// `docs/LANGUAGE.md`, "Assignment — bracket-path lvalues".
    fn validate_assignment(&mut self, assign: &Assignment) {
        // Validate the value expression
        self.validate_expr(&assign.value);

        let name = assign.name();

        // The token-stream scan catches most spellings earlier and with a
        // tighter span, but it tells a target from an argv `key=value` word by
        // what precedes it — and in an env-scoped prefix (`x=1 BAD=2 cmd`) the
        // second target and a command name look identical one token back. Here
        // the tree already knows this is a target, so the rule is exact.
        if let Err(bad) = crate::name::validate(name) {
            // `.` and `#` have their own codes below, which name the corrected
            // spelling. Reporting the general rule as well would hand the
            // author two errors for one mistake.
            if !matches!(bad.ch, '.' | '#') {
                self.issues.push(ValidationIssue::error(
                    IssueCode::InvisibleAssignmentTarget,
                    bad.to_string(),
                ));
            }
        }

        // A name spelled in two scripts binds one variable and reads as
        // another, with exit code 0 either way. A warning, not an error — see
        // `crate::name::mixed_script`.
        if let Some(mixed) = crate::name::mixed_script(name) {
            self.issues.push(
                ValidationIssue::warning(IssueCode::MixedScriptName, mixed.to_string())
                    .with_suggestion(mixed.suggestion()),
            );
        }

        if assign.path.segments.len() == 1 {
            if let Some(dot) = name.find('.') {
                let (root, rest) = (&name[..dot], &name[dot + 1..]);
                self.issues.push(
                    ValidationIssue::error(
                        IssueCode::DottedAssignmentTarget,
                        format!(
                            "'{name}' is not a valid assignment target — kaish uses bracket \
                             access, not dots"
                        ),
                    )
                    .with_suggestion(format!("use `{root}[{rest}]=value`")),
                );
            }
            if name.contains('#') {
                // `#` is an ordinary word character, so `abc#3` is a legal
                // word — but `$abc#3` is a lexer error, so this assignment
                // would bind a variable that can never be read back. Silent
                // creation of an unreachable name is worse than a refusal.
                self.issues.push(
                    ValidationIssue::error(
                        IssueCode::UnreadableAssignmentTarget,
                        format!(
                            "'{name}' is not a valid assignment target — a variable name \
                             cannot contain `#`"
                        ),
                    )
                    .with_suggestion(format!(
                        "drop the `#`, e.g. `{}=value`",
                        name.replace('#', "_")
                    )),
                );
            }
            // Bind the variable name in scope
            self.scope.bind(name);
        } else if !self.scope.is_bound(name) {
            self.issues.push(
                ValidationIssue::error(
                    IssueCode::LvalueUndefinedRoot,
                    format!(
                        "'{name}' is not defined — a subscripted assignment never creates the \
                         root variable"
                    ),
                )
                .with_suggestion(format!("create it first, e.g. `{name}={{}}` or `{name}=[]`")),
            );
            // Bind it anyway so a later reference to the same (still-invalid)
            // root doesn't ALSO trigger a separate undefined-variable warning.
            self.scope.bind(name);
        }
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
            // Warning only - command might be a script in PATH or external tool.
            // (`test` is now a first-class builtin — VFS-aware, validated — so it
            // takes the `is_builtin` path above and never lands here.)
            self.issues.push(ValidationIssue::warning(
                IssueCode::UndefinedCommand,
                format!("command '{}' not found in builtin registry", cmd.name),
            ).with_suggestion("this may be a script in PATH or external command"));
        }

        // Validate arguments expressions
        for arg in &cmd.args {
            self.validate_arg(arg);
        }

        // If we have a schema, validate args against it. Pass the schema so the
        // arg-builder binds glued/value short-flags the same way execute does —
        // otherwise a tool whose validate() reads positionals semantically (sed,
        // awk) misreads them (the schema-blind validation builder).
        if let Some(tool) = self.registry.get(&cmd.name) {
            // Prefer the kernel's schema catalog over `tool.schema()` — same
            // rationale as the dispatch path in kernel.rs (GH #48/#254): for a
            // clap-derived builtin, `schema()` rebuilds the whole clap
            // `Command`, and the catalog already holds exactly that, name-sorted.
            // `owned` covers a catalog miss (empty catalog, or a tool registered
            // after the catalog was built): the fallback calls `schema()` and is
            // equivalent, just not free.
            let owned;
            let schema: &ToolSchema =
                match self.catalog.binary_search_by(|s| s.name.as_str().cmp(cmd.name.as_str())) {
                    Ok(i) => &self.catalog[i],
                    Err(_) => {
                        owned = tool.schema();
                        &owned
                    }
                };
            let tool_args = build_tool_args_for_validation(&cmd.args, Some(schema));
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
            Arg::WordAssign { value, .. } => self.validate_expr(value),
            Arg::ShortFlag(_) | Arg::LongFlag(_) | Arg::DoubleDash => {}
        }
    }

    /// Validate a pipeline.
    fn validate_pipeline(&mut self, pipe: &Pipeline) {
        // Check for scatter without gather
        let named = |name: &str| {
            pipe.stages
                .iter()
                .filter_map(|s| s.as_command())
                .any(|c| c.name == name)
        };
        let has_scatter = named("scatter");
        let has_gather = named("gather");
        if has_scatter && !has_gather {
            self.issues.push(
                ValidationIssue::error(
                    IssueCode::ScatterWithoutGather,
                    "scatter without gather — parallel results would be lost",
                ).with_suggestion("add gather: ... | scatter | cmd | gather")
            );
        }

        for stage in &pipe.stages {
            match stage {
                PipelineStage::Command(cmd) => self.validate_command(cmd),
                PipelineStage::Compound(stmt) => self.validate_stmt(stmt),
            }
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
        // Validate item expressions and check for bare scalar variables
        for item in &for_loop.items {
            self.validate_expr(item);

            // Detect `for i in $VAR` pattern - always a mistake in kaish
            // since we don't do implicit word splitting
            if self.is_bare_scalar_var(item) {
                self.issues.push(
                    ValidationIssue::error(
                        IssueCode::ForLoopScalarVar,
                        "bare variable in for loop iterates once (kaish has no implicit word splitting)",
                    )
                    .with_suggestion(concat!(
                        "wrap it in $(...) — for a collection use keys/values:\n",
                        "    for x in $(values $coll)      # list elements / record values\n",
                        "    for k in $(keys $coll)        # list indices / record keys\n",
                        "    for i in $(split \"$VAR\")       # split a string on whitespace\n",
                        "    for i in $(split \"$VAR\" \":\")   # split a string on a delimiter\n",
                        "    for i in $(seq 1 10)          # iterate numbers\n",
                        "    for i in $(glob \"*.rs\")        # iterate files",
                    )),
                );
            }
        }

        self.loop_depth += 1;
        self.scope.push_frame();

        // `for` binds a name the same way an assignment does, so it carries
        // the same hazard: `for PАTH in …` reads as `PATH` and binds another
        // variable, with exit code 0 either way. Warning, not error — see
        // `crate::name::mixed_script`.
        if let Some(mixed) = crate::name::mixed_script(&for_loop.variable) {
            self.issues.push(
                ValidationIssue::warning(IssueCode::MixedScriptName, mixed.to_string())
                    .with_suggestion(mixed.suggestion()),
            );
        }

        // Bind loop variable
        self.scope.bind(&for_loop.variable);

        for stmt in &for_loop.body {
            self.validate_stmt(stmt);
        }

        self.scope.pop_frame();
        self.loop_depth -= 1;
    }

    /// Extract glob pattern from unquoted literal expressions.
    ///
    /// Check if an expression is a bare scalar variable reference.
    ///
    /// Returns true for `$VAR` or `${VAR}` but not for `$(cmd)` or `"$VAR"`.
    fn is_bare_scalar_var(&self, expr: &Expr) -> bool {
        match expr {
            // Direct variable reference like $VAR or ${VAR}
            Expr::VarRef(_) => true,
            // Variable with default like ${VAR:-default} - also problematic
            Expr::VarWithDefault { .. } => true,
            // NOT a problem: command substitution like $(cmd) - returns structured data
            Expr::CommandSubst(_) => false,
            // NOT a problem: literals are fine
            Expr::Literal(_) => false,
            // NOT a problem: interpolated strings are a single value
            Expr::Interpolated(_) => false,
            // Everything else: not a bare scalar var
            _ => false,
        }
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
        } else if let Some(n) = levels
            && n > self.loop_depth {
                self.issues.push(ValidationIssue::warning(
                    IssueCode::BreakOutsideLoop,
                    format!(
                        "break {} exceeds loop nesting depth {}",
                        n, self.loop_depth
                    ),
                ));
            }
    }

    /// Validate a continue statement.
    fn validate_continue(&mut self, levels: Option<usize>) {
        if self.loop_depth == 0 {
            self.issues.push(ValidationIssue::error(
                IssueCode::BreakOutsideLoop,
                "continue used outside of a loop",
            ));
        } else if let Some(n) = levels
            && n > self.loop_depth {
                self.issues.push(ValidationIssue::warning(
                    IssueCode::BreakOutsideLoop,
                    format!(
                        "continue {} exceeds loop nesting depth {}",
                        n, self.loop_depth
                    ),
                ));
            }
    }

    /// Validate a return statement.
    fn validate_return(&mut self, expr: Option<&Expr>) {
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
            TestExpr::And { left, right } | TestExpr::Or { left, right } => {
                self.validate_test(left);
                self.validate_test(right);
            }
            TestExpr::Not { expr } => self.validate_test(expr),
            TestExpr::In { left, right } | TestExpr::NotIn { left, right } => {
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
            Expr::HereDocBody { parts, .. } => {
                for sp in parts {
                    self.validate_spanned_string_part(sp);
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                self.validate_expr(left);
                self.validate_expr(right);
            }
            Expr::CommandSubst(stmts) => {
                for stmt in stmts {
                    self.validate_stmt(stmt);
                }
            }
            Expr::Test(test) => self.validate_test(test),
            Expr::Positional(_) | Expr::AllArgs | Expr::ArgCount => {}
            Expr::VarLength(path) => {
                if let Some(VarSegment::Field(root)) = path.segments.first() {
                    self.check_var_defined(root);
                }
            }
            Expr::VarWithDefault { .. } => {
                // Don't warn — the default handles the undefined/absent case.
            }
            Expr::Arithmetic(_) => {
                // Arithmetic parsing is done at runtime
            }
            Expr::Command(cmd) => self.validate_command(cmd),
            Expr::LastExitCode | Expr::CurrentPid => {}
            Expr::GlobPattern(_) => {}
            Expr::ListLiteral(elems) => {
                for elem in elems {
                    match elem {
                        ListElem::Item(e) | ListElem::Spread(e) => self.validate_expr(e),
                    }
                }
            }
            Expr::RecordLiteral(entries) => {
                for entry in entries {
                    self.validate_expr(&entry.value);
                }
            }
        }
    }

    /// Validate a variable reference.
    fn validate_var_ref(&mut self, path: &VarPath) {
        if let Some(VarSegment::Field(name)) = path.segments.first() {
            // `${?.field}` is removed — $? is the POSIX integer exit code.
            // Use `kaish-last` to access the previous command's structured data.
            if name == "?" && path.segments.len() > 1 {
                self.issues.push(
                    ValidationIssue::error(
                        IssueCode::LastResultFieldAccess,
                        "${?.field} is removed; $? is the POSIX exit code",
                    )
                    .with_suggestion(
                        "use `kaish-last` to read the previous command's data or stdout",
                    ),
                );
                return;
            }
            self.check_var_defined(name);
        }
    }

    /// Validate a spanned heredoc-body part, attaching the part's span to any
    /// new issues raised during the inner walk. Issues already carrying a span
    /// are left alone (e.g., from a nested validator that already knew better).
    fn validate_spanned_string_part(&mut self, sp: &SpannedPart) {
        let issues_before = self.issues.len();
        self.validate_string_part(&sp.part);
        let span = Span::new(sp.offset, sp.offset + sp.len);
        for issue in &mut self.issues[issues_before..] {
            if issue.span.is_none() {
                issue.span = Some(span);
            }
        }
    }

    /// Validate a string interpolation part.
    fn validate_string_part(&mut self, part: &StringPart) {
        match part {
            StringPart::Literal(_) => {}
            StringPart::Var(path) => self.validate_var_ref(path),
            StringPart::VarWithDefault { default, .. } => {
                // Validate nested parts in the default value
                for p in default {
                    self.validate_string_part(p);
                }
            }
            StringPart::VarLength(path) => {
                if let Some(VarSegment::Field(root)) = path.segments.first() {
                    self.check_var_defined(root);
                }
            }
            StringPart::Positional(_) | StringPart::AllArgs | StringPart::ArgCount => {}
            StringPart::Arithmetic(_) => {} // Arithmetic expressions are validated at eval time
            StringPart::CommandSubst(stmts) => {
                for stmt in stmts {
                    self.validate_stmt(stmt);
                }
            }
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
    ///
    /// Counts bareword `key=value` (`Arg::WordAssign`) as positional: user
    /// tools aren't on `WORD_ASSIGN_BUILTINS`, so at runtime the kernel
    /// stringifies WordAssign into a positional `"key=value"` (matches bash).
    /// Skipping it here would falsely error `mytool foo=bar` when mytool has
    /// one required positional.
    fn validate_user_tool_args(&mut self, tool_def: &ToolDef, args: &[Arg]) {
        let positional_count = args
            .iter()
            .filter(|a| matches!(a, Arg::Positional(_) | Arg::WordAssign { .. }))
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
///
/// The parser only ever produces a literal `Command.name`, so for the AST-walk
/// path this is always true; the `${…}` / `$(…)` guards matter for the
/// string-accepting `Kernel::classify_command` API, where a dynamic name
/// classifies as [`CommandKind::Dynamic`] rather than a misleading `External`.
pub(crate) fn is_static_command_name(name: &str) -> bool {
    !name.starts_with('$') && !name.contains("$(") && !name.contains("${")
}

/// Interpreter special-forms — the command names `execute_command_depth`
/// short-circuits before any alias/registry/`PATH` lookup.
///
/// **Single source of truth, compile-enforced.** `from_name` is the only place a
/// name becomes "special", and the executor matches this enum *exhaustively*, so
/// adding a form is a compile error until both the name mapping here and the
/// execution behavior in `execute_command_depth` are updated — the two cannot
/// silently diverge, and there's no `unreachable!` to panic at runtime.
/// `classify_command` reports every special form as `CommandKind::Special` via
/// [`is_runtime_special_form`].
///
/// This set is deliberately **narrower** than `is_special_command` below: that
/// set is a validator warning heuristic (it suppresses "command not found" for
/// names like `readonly`/`:` that the validator chooses not to flag), whereas
/// this reflects what the interpreter *actually* short-circuits. Keeping them
/// separate avoids `classify_command` mislabelling a name as internal when it
/// would in fact escape to `PATH` (e.g. `readonly` resolves to an external
/// command at runtime). See `Kernel::classify_command`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SpecialForm {
    /// `true` and its other spelling `:` — always succeed (exit 0).
    True,
    /// `false` — always fails (exit 1).
    False,
    /// `source` / `.` — execute a script in the current shell.
    Source,
}

impl SpecialForm {
    /// The single mapping from a command name to a special-form, or `None` if the
    /// name is resolved normally (alias/registry/`PATH`).
    pub(crate) fn from_name(name: &str) -> Option<Self> {
        match name {
            // `:` short-circuits through the same arm as `true`, not through
            // its registered tool. That is what makes them the SAME command
            // rather than two that merely agree: an alias cannot shadow
            // either, and neither clap-parses its operands, so `: -x` exits 0
            // like bash instead of failing on an unknown flag.
            "true" | ":" => Some(Self::True),
            "false" => Some(Self::False),
            "source" | "." => Some(Self::Source),
            _ => None,
        }
    }
}

/// Whether `name` is an interpreter special-form (see [`SpecialForm`]).
pub(crate) fn is_runtime_special_form(name: &str) -> bool {
    SpecialForm::from_name(name).is_some()
}

/// Classify a command name the way the interpreter resolves it, given whether
/// the registry and user-tool table contain it. Shared by the validator's
/// triage and `Kernel::classify_command` so the two never diverge.
pub(crate) fn classify_command_name(
    name: &str,
    is_builtin: bool,
    is_user_tool: bool,
) -> CommandKind {
    if !is_static_command_name(name) {
        return CommandKind::Dynamic;
    }
    if is_runtime_special_form(name) {
        return CommandKind::Special;
    }
    // User functions are checked before builtins in `execute_command_depth`, so
    // a user function shadows a builtin of the same name.
    if is_user_tool {
        return CommandKind::UserTool;
    }
    if is_builtin {
        return CommandKind::Builtin;
    }
    CommandKind::External
}

/// Check if a command is a special built-in that we don't validate.
fn is_special_command(name: &str) -> bool {
    // `test`/`[`/`[[`/`:` are intentionally absent: `test` and `:` are real
    // builtins now (they validate via the registry like any other), and
    // `[`/`[[` parse as `[[ … ]]` test expressions, never reaching here as a
    // command name.
    matches!(name, "true" | "false" | "readonly" | "local")
}

/// Build ToolArgs from AST Args for validation purposes.
///
/// This is a simplified version that doesn't evaluate expressions -
/// it uses placeholder values since we only care about argument structure.
pub fn build_tool_args_for_validation(args: &[Arg], schema: Option<&ToolSchema>) -> ToolArgs {
    let mut tool_args = ToolArgs::new();

    // Validation binds the way execution does — placeholders in source order,
    // into `words` — so the schema checks never judge a decomposition the tool
    // will not receive.
    if schema.is_some_and(|s| matches!(s.arg_binding, ArgBinding::Verbatim)) {
        let mut words = Vec::new();
        let mut past_double_dash = false;
        for arg in args {
            match arg {
                Arg::Positional(expr) => words.push(expr_to_placeholder(expr)),
                Arg::ShortFlag(name) => words.push(Value::String(format!("-{name}"))),
                Arg::LongFlag(name) => {
                    if past_double_dash || !is_global_output_flag(name) {
                        words.push(Value::String(format!("--{name}")));
                    }
                }
                Arg::Named { key, .. } => {
                    if past_double_dash || !is_global_output_flag(key) {
                        words.push(Value::String(format!("--{key}=<value>")));
                    }
                }
                Arg::WordAssign { key, .. } => {
                    words.push(Value::String(format!("{key}=<value>")));
                }
                Arg::DoubleDash => {
                    past_double_dash = true;
                    words.push(Value::String("--".to_string()));
                }
            }
        }
        tool_args.words = Some(words);
        return tool_args;
    }

    // Schema-aware param table: flag name → (canonical, type, consumes, repeatable).
    // Empty when there's no schema, in which case every flag stays a bare flag
    // (the old schema-blind behavior).
    let param_lookup = schema.map(schema_param_lookup).unwrap_or_default();
    let mut consumed: HashSet<usize> = HashSet::new();
    let mut past_double_dash = false;

    for i in 0..args.len() {
        match &args[i] {
            Arg::DoubleDash => past_double_dash = true,
            Arg::Positional(expr) => {
                if !consumed.contains(&i) {
                    tool_args.positional.push(expr_to_placeholder(expr));
                }
            }
            Arg::Named { key, value } => {
                let v = expr_to_placeholder(value);
                match param_lookup.get(key.as_str()) {
                    // Repeatable `--flag=a --flag=b` accumulates (matches execute).
                    Some(&(canonical, _, _, true)) => {
                        let _ = push_repeatable_value(&mut tool_args, key, canonical, v);
                    }
                    Some(&(canonical, ..)) => {
                        tool_args.named.insert(canonical.to_string(), v);
                    }
                    None => {
                        tool_args.named.insert(key.clone(), v);
                    }
                }
            }
            Arg::WordAssign { key, value } => {
                // Validation walker doesn't know which command is receiving;
                // route into named like the legacy behavior so checks stay
                // consistent with previous validator output.
                tool_args.named.insert(key.clone(), expr_to_placeholder(value));
            }
            Arg::ShortFlag(name) => {
                if past_double_dash {
                    tool_args.positional.push(Value::String(format!("-{name}")));
                } else {
                    bind_short_flag_for_validation(
                        name,
                        &param_lookup,
                        args,
                        i,
                        &mut consumed,
                        &mut tool_args,
                    );
                }
            }
            Arg::LongFlag(name) => {
                if past_double_dash {
                    tool_args.positional.push(Value::String(format!("--{name}")));
                } else {
                    match param_lookup.get(name.as_str()) {
                        Some(&(canonical, typ, consumes, repeatable)) if !is_bool_type(typ) => {
                            bind_value_or_flag(
                                &mut tool_args, name, canonical, consumes, repeatable, args, i,
                                &mut consumed,
                            );
                        }
                        Some(&(canonical, ..)) => {
                            tool_args.flags.insert(canonical.to_string());
                        }
                        None => {
                            tool_args.flags.insert(name.clone());
                        }
                    }
                }
            }
        }
    }

    tool_args
}

/// Bind a (possibly glued/combined) short-flag token, schema-aware, mirroring
/// `kernel::build_args_async`: a value-taking first char consumes the rest of the
/// token as its glued value (`-e1d` → e=`1d`) or, if it is the last char, the next
/// positional (`-e d` → e=`d`); bool flags stack (`-la`). Unknown chars stay bare
/// flags so a schemaless tool keeps all-boolean behavior.
fn bind_short_flag_for_validation(
    name: &str,
    param_lookup: &HashMap<String, (&str, &str, usize, bool)>,
    args: &[Arg],
    i: usize,
    consumed: &mut HashSet<usize>,
    tool_args: &mut ToolArgs,
) {
    // Whole-name match first (POSIX `-name value` or a multi-char bool).
    if let Some(&(canonical, typ, consumes, repeatable)) = param_lookup.get(name) {
        if is_bool_type(typ) {
            tool_args.flags.insert(canonical.to_string());
        } else {
            bind_value_or_flag(tool_args, name, canonical, consumes, repeatable, args, i, consumed);
        }
        return;
    }
    // First char is a declared value-taking short flag: the tail is its glued value.
    if let Some(&(canonical, _, consumes, repeatable)) = param_lookup
        .get(&name[..1])
        .filter(|(_, typ, ..)| !is_bool_type(typ))
    {
        let glued = name[1..].to_string();
        if glued.is_empty() {
            bind_value_or_flag(
                tool_args, &name[..1], canonical, consumes, repeatable, args, i, consumed,
            );
        } else {
            let _ =
                bind_glued_short_value(tool_args, &name[..1], canonical, consumes, repeatable, glued);
        }
        return;
    }
    // Combined short flags: bools stack until the first value-taking char, which
    // consumes the rest of the token (or the next positional).
    let bytes = name.as_bytes();
    let mut p = 0;
    while p < bytes.len() {
        let key = &name[p..p + 1];
        match param_lookup.get(key) {
            Some(&(canonical, typ, consumes, repeatable)) if !is_bool_type(typ) => {
                let glued = name[p + 1..].to_string();
                if glued.is_empty() {
                    bind_value_or_flag(
                        tool_args, key, canonical, consumes, repeatable, args, i, consumed,
                    );
                } else {
                    let _ = bind_glued_short_value(
                        tool_args, key, canonical, consumes, repeatable, glued,
                    );
                }
                return;
            }
            _ => {
                tool_args.flags.insert(key.to_string());
                p += 1;
            }
        }
    }
}

/// Bind a bare value-flag by consuming the next `consumes` not-yet-consumed
/// positionals as its value(s) — mirroring `kernel::consume_flag_positionals`:
/// a single-value flag stores a scalar (repeatable → canonical array), a
/// multi-value flag (`jq --arg NAME VALUE`, `consumes==2`) stores an
/// array-of-arrays. A single-value flag may also consume a `key=value`
/// (`awk -v a=1`); multi-value flags take plain positionals only. With nothing
/// to consume it falls back to a bare flag.
#[allow(clippy::too_many_arguments)] // mirrors kernel::consume_flag_positionals
fn bind_value_or_flag(
    tool_args: &mut ToolArgs,
    flag_name: &str,
    canonical: &str,
    consumes: usize,
    repeatable: bool,
    args: &[Arg],
    i: usize,
    consumed: &mut HashSet<usize>,
) {
    let want = consumes.max(1);
    let allow_word_assign = consumes <= 1;
    let mut collected: Vec<Value> = Vec::with_capacity(want);
    for _ in 0..want {
        let found = args[i + 1..].iter().enumerate().find_map(|(off, a)| {
            let idx = i + 1 + off;
            if consumed.contains(&idx) {
                return None;
            }
            match a {
                Arg::Positional(expr) => Some((idx, expr_to_placeholder(expr))),
                Arg::WordAssign { key, value } if allow_word_assign => {
                    let s = crate::interpreter::value_to_string(&expr_to_placeholder(value));
                    Some((idx, Value::String(format!("{key}={s}"))))
                }
                _ => None,
            }
        });
        match found {
            Some((idx, v)) => {
                consumed.insert(idx);
                collected.push(v);
            }
            None => break,
        }
    }

    if collected.is_empty() {
        tool_args.flags.insert(canonical.to_string());
        return;
    }
    if consumes <= 1 {
        if let Some(v) = collected.into_iter().next() {
            if repeatable {
                // Discarding this error is safe ONLY because no value reaching
                // here can be binary. `expr_to_placeholder` passes literals
                // through and renders everything dynamic as `<dynamic>`, and
                // the parser cannot build a `Value::Bytes` literal — bytes only
                // arise at runtime, from a capture the validator never runs. So
                // the binder's binary gate (`flag_value_to_json`, GH #223)
                // cannot fire here, and the only other error is the
                // non-array-entry case, which a fresh `tool_args` cannot reach.
                // Give the validator real evaluated values and this line starts
                // swallowing that gate — propagate it then.
                let _ = push_repeatable_value(tool_args, flag_name, canonical, v);
            } else {
                tool_args.named.insert(canonical.to_string(), v);
            }
        }
        return;
    }
    // Multi-consume: accumulate under named[canonical] as array-of-arrays.
    // Deliberately the raw `value_to_json`, not the binder's `flag_value_to_json`:
    // this is a validation-only twin over placeholders, so there is no binary to
    // gate and nothing to report. Same caveat as above — it diverges from the
    // binder the moment the validator evaluates for real.
    let occ: Vec<serde_json::Value> = collected
        .iter()
        .map(crate::interpreter::value_to_json)
        .collect();
    let entry = tool_args
        .named
        .entry(canonical.to_string())
        .or_insert_with(|| Value::Json(serde_json::Value::Array(Vec::new())));
    if let Value::Json(serde_json::Value::Array(outer)) = entry {
        outer.push(serde_json::Value::Array(occ));
    }
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
        let validator = Validator::new(&registry, &user_tools, &[]);

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

    /// `test` is a first-class builtin now, so it validates through the
    /// registry like any other command — no POSIX-conditional advisory, and no
    /// spurious undefined-command warning.
    #[test]
    fn test_command_is_a_known_builtin() {
        let (registry, user_tools) = make_validator();
        let validator = Validator::new(&registry, &user_tools, &[]);

        let program = Program {
            statements: vec![Stmt::Command(Command {
                name: "test".to_string(),
                args: vec![
                    Arg::Positional(Expr::Literal(Value::String("-n".to_string()))),
                    Arg::Positional(Expr::Literal(Value::String("hi".to_string()))),
                ],
                redirects: vec![],
            })],
        };

        let issues = validator.validate(&program);
        assert!(
            !issues.iter().any(|i| i.code == IssueCode::UndefinedCommand),
            "`test` is a builtin — no undefined-command warning: {issues:?}"
        );
    }

    #[test]
    fn validates_known_command() {
        let (registry, user_tools) = make_validator();
        let validator = Validator::new(&registry, &user_tools, &[]);

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

    /// GH #256's whole premise is that reading the catalog instead of calling
    /// `tool.schema()` doesn't change a single validation outcome. Every other
    /// test in this module passes `&[]` and only ever exercises the fallback
    /// (`tool.schema()`) branch of `validate_command`'s binary search — this
    /// is the one that actually populates the catalog and hits it, then
    /// checks the two branches agree byte-for-byte on a schema-driven issue.
    ///
    /// `jq`'s `filter` is the only *required* param anywhere in the builtin
    /// catalog (checked by dumping every schema's `required` params while
    /// writing this test) and jq's own `Tool::validate` override calls
    /// `validate_against_schema(args, &self.schema())` first thing — so
    /// `jq` with no args at all is guaranteed to trip `MissingRequiredArg`
    /// via a real schema check, identically on both paths, with no need to
    /// fabricate a corrupt schema to make it non-trivial.
    ///
    /// This was manually verified to be a *real*, falsifiable comparison, not
    /// a vacuous one: temporarily corrupting the catalog's `jq` entry so `-r`
    /// (a genuine bool flag) is misdeclared as value-consuming, and changing
    /// the program to `jq -r .` (filter present), makes the two paths
    /// disagree — the catalog-hit path swallows the filter positional `.` as
    /// `-r`'s value and reports `MissingRequiredArg`, while the fallback path
    /// (real schema, `-r` correctly bool) does not — and the `assert_eq!`
    /// below fails as expected. Not committed as a permanent test (it would
    /// pin a corruption, not a correctness invariant); this docstring is the
    /// record of having done it.
    #[test]
    fn catalog_hit_and_fallback_produce_identical_schema_driven_issues() {
        let (registry, user_tools) = make_validator();
        let catalog = registry.schemas();
        assert!(
            catalog.binary_search_by(|s| s.name.as_str().cmp("jq")).is_ok(),
            "fixture assumption: `jq` must be in the catalog for this to be a real hit"
        );

        let program = Program {
            statements: vec![Stmt::Command(Command {
                name: "jq".to_string(),
                args: vec![],
                redirects: vec![],
            })],
        };

        let fallback_issues =
            Validator::new(&registry, &user_tools, &[]).validate(&program);
        let catalog_issues =
            Validator::new(&registry, &user_tools, &catalog).validate(&program);

        // Prove the input is actually discriminating: a program both paths
        // report zero issues on proves nothing about whether they agree.
        assert!(
            fallback_issues.iter().any(|i| i.code == IssueCode::MissingRequiredArg),
            "test input should trip MissingRequiredArg (`filter`) via the fallback path; \
             got {fallback_issues:?}"
        );

        fn render(issues: &[ValidationIssue]) -> Vec<(Severity, IssueCode, &str, Option<&str>)> {
            issues
                .iter()
                .map(|i| (i.severity, i.code, i.message.as_str(), i.suggestion.as_deref()))
                .collect()
        }
        assert_eq!(
            render(&fallback_issues),
            render(&catalog_issues),
            "catalog-hit and tool.schema()-fallback validation must agree exactly \
             (same codes, same messages, same order); fallback={fallback_issues:?} \
             catalog={catalog_issues:?}"
        );
    }

    #[test]
    fn glued_value_flags_dont_false_error_at_validation() {
        // Regression for the schema-blind validation builder:
        // `sed -e1d -e2d FILE` must validate clean. Before schema-aware binding,
        // the glued `-e` values weren't split, so `collect_expressions` fell back
        // to parsing the FILE PATH as the sed program — a path-dependent false
        // E006 (here `file.txt` → its leading `f` is an "unknown command").
        let (registry, user_tools) = make_validator();
        let validator = Validator::new(&registry, &user_tools, &[]);

        let program = Program {
            statements: vec![Stmt::Command(Command {
                name: "sed".to_string(),
                args: vec![
                    Arg::ShortFlag("e1d".to_string()),
                    Arg::ShortFlag("e2d".to_string()),
                    Arg::Positional(Expr::Literal(Value::String("file.txt".to_string()))),
                ],
                redirects: vec![],
            })],
        };

        let issues = validator.validate(&program);
        assert!(
            !issues.iter().any(|i| i.code == IssueCode::InvalidSedExpr),
            "glued -e flags false-errored at validation: {:?}",
            issues.iter().map(|i| &i.message).collect::<Vec<_>>()
        );
    }

    #[test]
    fn validates_break_outside_loop() {
        let (registry, user_tools) = make_validator();
        let validator = Validator::new(&registry, &user_tools, &[]);

        let program = Program {
            statements: vec![Stmt::Break(None)],
        };

        let issues = validator.validate(&program);
        assert!(issues.iter().any(|i| i.code == IssueCode::BreakOutsideLoop));
    }

    #[test]
    fn validates_break_inside_loop() {
        let (registry, user_tools) = make_validator();
        let validator = Validator::new(&registry, &user_tools, &[]);

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
        let validator = Validator::new(&registry, &user_tools, &[]);

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
        let validator = Validator::new(&registry, &user_tools, &[]);

        let program = Program {
            statements: vec![
                // First assign the variable
                Stmt::Assignment(Assignment {
                    path: VarPath::simple("MY_VAR"),
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
        let validator = Validator::new(&registry, &user_tools, &[]);

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
        let validator = Validator::new(&registry, &user_tools, &[]);

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

    #[test]
    fn validates_scatter_without_gather() {
        let (registry, user_tools) = make_validator();
        let validator = Validator::new(&registry, &user_tools, &[]);

        let program = Program {
            statements: vec![Stmt::Pipeline(Pipeline {
                stages: vec![
                    Command { name: "seq".to_string(), args: vec![
                        Arg::Positional(Expr::Literal(Value::String("1".into()))),
                        Arg::Positional(Expr::Literal(Value::String("3".into()))),
                    ], redirects: vec![] },
                    Command { name: "scatter".to_string(), args: vec![], redirects: vec![] },
                    Command { name: "echo".to_string(), args: vec![
                        Arg::Positional(Expr::Literal(Value::String("hi".into()))),
                    ], redirects: vec![] },
                ]
                .into_iter()
                .map(PipelineStage::Command)
                .collect(),
                background: false,
            })],
        };

        let issues = validator.validate(&program);
        assert!(issues.iter().any(|i| i.code == IssueCode::ScatterWithoutGather),
            "should flag scatter without gather: {:?}", issues);
    }

    #[test]
    fn allows_scatter_with_gather() {
        let (registry, user_tools) = make_validator();
        let validator = Validator::new(&registry, &user_tools, &[]);

        let program = Program {
            statements: vec![Stmt::Pipeline(Pipeline {
                stages: vec![
                    Command { name: "seq".to_string(), args: vec![
                        Arg::Positional(Expr::Literal(Value::String("1".into()))),
                        Arg::Positional(Expr::Literal(Value::String("3".into()))),
                    ], redirects: vec![] },
                    Command { name: "scatter".to_string(), args: vec![], redirects: vec![] },
                    Command { name: "echo".to_string(), args: vec![
                        Arg::Positional(Expr::Literal(Value::String("hi".into()))),
                    ], redirects: vec![] },
                    Command { name: "gather".to_string(), args: vec![], redirects: vec![] },
                ]
                .into_iter()
                .map(PipelineStage::Command)
                .collect(),
                background: false,
            })],
        };

        let issues = validator.validate(&program);
        assert!(!issues.iter().any(|i| i.code == IssueCode::ScatterWithoutGather),
            "scatter with gather should pass: {:?}", issues);
    }

    fn make_user_tool_with_required_positional() -> HashMap<String, ToolDef> {
        let mut user_tools = HashMap::new();
        user_tools.insert(
            "mytool".to_string(),
            ToolDef {
                name: "mytool".to_string(),
                params: vec![crate::ast::ParamDef {
                    name: "input".to_string(),
                    param_type: None,
                    default: None,
                }],
                body: vec![],
            },
        );
        user_tools
    }

    /// `mytool foo=bar` should count the bareword `key=value` as a positional
    /// because user tools aren't on the WordAssign allowlist — runtime
    /// stringifies WordAssign to a positional. Validator must agree.
    #[test]
    fn user_tool_wordassign_counts_as_positional() {
        let mut registry = ToolRegistry::new();
        register_builtins(&mut registry);
        let user_tools = make_user_tool_with_required_positional();
        let validator = Validator::new(&registry, &user_tools, &[]);

        let program = Program {
            statements: vec![Stmt::Command(Command {
                name: "mytool".to_string(),
                args: vec![Arg::WordAssign {
                    key: "foo".to_string(),
                    value: Expr::Literal(Value::String("bar".to_string())),
                }],
                redirects: vec![],
            })],
        };

        let issues = validator.validate(&program);
        assert!(
            !issues.iter().any(|i| i.code == IssueCode::MissingRequiredArg),
            "WordAssign should satisfy required positional; got {:?}",
            issues
        );
    }

    /// Missing-required-arg still fires when no positional or WordAssign is
    /// provided — regression guard so the fix doesn't silently skip the check.
    #[test]
    fn user_tool_no_args_still_errors() {
        let mut registry = ToolRegistry::new();
        register_builtins(&mut registry);
        let user_tools = make_user_tool_with_required_positional();
        let validator = Validator::new(&registry, &user_tools, &[]);

        let program = Program {
            statements: vec![Stmt::Command(Command {
                name: "mytool".to_string(),
                args: vec![],
                redirects: vec![],
            })],
        };

        let issues = validator.validate(&program);
        assert!(
            issues.iter().any(|i| i.code == IssueCode::MissingRequiredArg),
            "missing positional should still error; got {:?}",
            issues
        );
    }
}
