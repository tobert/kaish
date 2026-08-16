//! Expanding one heredoc body against a scope the caller supplies.
//!
//! A plan publishes what a command was *asked* to read on stdin. When the
//! delimiter was unquoted the shell expands the body first, so the published
//! text is not the text the command receives — and an analyzer handed the
//! published text would judge a program that never runs.
//!
//! [`expand_fragment`] closes that gap, under two rules that are the whole
//! design:
//!
//! - **The caller supplies the scope.** Nothing is read from session state.
//!   An embedder deciding against values it holds gets the body those values
//!   produce, never one the kernel peeked and never a stale one — `read
//!   TOKEN` binds at runtime, and a plan cannot see it.
//! - **A `$(…)` is returned, not run.** Running it is a decision with a clock
//!   and a blast radius, and it is the same decision the caller is asking
//!   about. Each substitution comes back as a [`Hole`] carrying its plan; a
//!   caller that judges it safe runs it in a kernel of its own construction
//!   and expands again with the answer in scope.
//!
//! Expansion runs the interpreter's own [`Evaluator`], never a second
//! implementation of expansion — a separate one would drift, and the drift
//! *is* the analyzed-text-is-not-executed-text failure this exists to
//! prevent.

use kaish_types::plan::{Expansion, FragmentAddr, Hole};
use kaish_types::Value;

use crate::ast::plan::{plan_statement, render_expr};
use crate::ast::{Arg, Command, Expr, ForLoop, IfStmt, Redirect, RedirectKind, Stmt, StringPart};
use crate::interpreter::Evaluator;
use crate::interpreter::Scope;
use crate::parser::{self, ParseError};

/// Why a fragment could not be expanded. Every variant names what was asked
/// for and what was there instead — an expansion that quietly returned
/// nothing would be read as a body that runs and produces nothing.
#[derive(Debug)]
pub enum FragmentError {
    /// The source did not parse.
    Parse(Vec<ParseError>),
    /// The program has no statement at that index.
    NoSuchStatement { asked: usize, statements: usize },
    /// The statement has no heredoc at that index.
    NoSuchHeredoc { asked: usize, heredocs: usize },
    /// The body reads session state the scope cannot carry — `$?`, `$$`, or
    /// a positional parameter. Expanding against an empty session would
    /// invent values.
    NeedsSessionState { what: String },
    /// Evaluation failed: a variable the body reads was not supplied, or a
    /// value could not become text.
    Eval { message: String },
}

impl std::fmt::Display for FragmentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parse(errors) => {
                write!(f, "source does not parse: {} error(s)", errors.len())
            }
            Self::NoSuchStatement { asked, statements } => write!(
                f,
                "no statement {asked}: the program has {statements}"
            ),
            Self::NoSuchHeredoc { asked, heredocs } => {
                write!(f, "no heredoc {asked}: the statement has {heredocs}")
            }
            Self::NeedsSessionState { what } => write!(
                f,
                "body reads {what}, which a supplied scope cannot carry — expand it in a kernel that holds the session instead"
            ),
            Self::Eval { message } => write!(f, "cannot expand body: {message}"),
        }
    }
}

impl std::error::Error for FragmentError {}

/// Expand one heredoc body against `scope`, without executing anything.
///
/// The address is the statement index and the flat heredoc index a plan
/// publishes as [`PlannedHeredoc::index`].
///
/// [`Expansion::Complete`] means "this is the text the command reads", and
/// that is all it means. A name the body reads and `scope` does not carry
/// expands to the empty string, because that is what kaish does when it
/// executes — expansion follows execution here rather than being stricter
/// than it, since a rule that disagreed with the interpreter would produce a
/// body the command never sees. **A caller that needs every value accounted
/// for checks [`PlannedHeredoc::free_variables`] against its scope before
/// expanding**; the plan publishes exactly that list.
///
/// [`PlannedHeredoc::index`]: kaish_types::plan::PlannedHeredoc::index
/// [`PlannedHeredoc::free_variables`]: kaish_types::plan::PlannedHeredoc::free_variables
pub fn expand_fragment(
    source: &str,
    addr: FragmentAddr,
    scope: &[(String, Value)],
) -> Result<Expansion, FragmentError> {
    let program = parser::parse(source).map_err(FragmentError::Parse)?;
    let statements: Vec<&Stmt> = program
        .statements
        .iter()
        .filter(|s| !matches!(s, Stmt::Empty))
        .collect();
    let stmt = statements
        .get(addr.statement)
        .ok_or(FragmentError::NoSuchStatement {
            asked: addr.statement,
            statements: statements.len(),
        })?;

    let mut found = Vec::new();
    collect_heredoc_targets(stmt, &mut found);
    let count = found.len();
    let target = found
        .into_iter()
        .nth(addr.heredoc)
        .ok_or(FragmentError::NoSuchHeredoc {
            asked: addr.heredoc,
            heredocs: count,
        })?;

    expand_target(target, scope)
}

/// Expand one heredoc's target expression.
fn expand_target(target: &Expr, scope: &[(String, Value)]) -> Result<Expansion, FragmentError> {
    // A `$(…)` anywhere in the body means the text this could produce is not
    // the text that runs. Report every one and expand nothing.
    let mut holes = Vec::new();
    collect_holes(target, &mut holes);
    if !holes.is_empty() {
        return Ok(Expansion::Blocked { holes });
    }
    if let Some(what) = session_state_read(target) {
        return Err(FragmentError::NeedsSessionState { what });
    }

    let mut session = Scope::new();
    for (name, value) in scope {
        session.set(name.clone(), value.clone());
    }
    let value = Evaluator::new(&mut session)
        .eval(target)
        .map_err(|e| FragmentError::Eval {
            message: e.to_string(),
        })?;
    match value {
        Value::String(text) => Ok(Expansion::Complete(text)),
        // Every heredoc target evaluates to a string: a literal body is one,
        // and `Expr::HereDocBody` assembles one. Anything else means the AST
        // shape changed underneath this, which is worth saying out loud.
        other => Err(FragmentError::Eval {
            message: format!("body evaluated to {other:?} instead of text"),
        }),
    }
}

// ───────────────────────── Walking for heredocs ─────────────────────────

/// Every heredoc target in one statement, in the order `ast::plan`'s
/// collection walk reaches them — the order that gives each one the flat
/// index a plan publishes. The two walks must agree; the tests pin it with a
/// heredoc nested inside a loop body and one inside an `if`.
fn collect_heredoc_targets<'a>(stmt: &'a Stmt, out: &mut Vec<&'a Expr>) {
    match stmt {
        Stmt::Command(cmd) => command_heredocs(cmd, out),
        Stmt::Pipeline(p) => {
            for cmd in &p.commands {
                command_heredocs(cmd, out);
            }
        }
        Stmt::Assignment(a) => expr_heredocs(&a.value, out),
        Stmt::If(IfStmt {
            condition,
            then_branch,
            else_branch,
        }) => {
            expr_heredocs(condition, out);
            block_heredocs(then_branch, out);
            if let Some(block) = else_branch {
                block_heredocs(block, out);
            }
        }
        Stmt::For(ForLoop { items, body, .. }) => {
            for item in items {
                expr_heredocs(item, out);
            }
            block_heredocs(body, out);
        }
        Stmt::While(w) => {
            expr_heredocs(&w.condition, out);
            block_heredocs(&w.body, out);
        }
        Stmt::Case(c) => {
            expr_heredocs(&c.expr, out);
            for branch in &c.branches {
                block_heredocs(&branch.body, out);
            }
        }
        Stmt::Return(e) | Stmt::Exit(e) => {
            if let Some(e) = e {
                expr_heredocs(e, out);
            }
        }
        Stmt::ToolDef(def) => block_heredocs(&def.body, out),
        Stmt::AndChain { left, right } | Stmt::OrChain { left, right } => {
            collect_heredoc_targets(left, out);
            collect_heredoc_targets(right, out);
        }
        Stmt::EnvScoped { assignments, body } => {
            for a in assignments {
                expr_heredocs(&a.value, out);
            }
            collect_heredoc_targets(body, out);
        }
        Stmt::Test(_) | Stmt::Break(_) | Stmt::Continue(_) | Stmt::Empty => {}
    }
}

fn block_heredocs<'a>(stmts: &'a [Stmt], out: &mut Vec<&'a Expr>) {
    for stmt in stmts {
        collect_heredoc_targets(stmt, out);
    }
}

fn command_heredocs<'a>(cmd: &'a Command, out: &mut Vec<&'a Expr>) {
    for Redirect { kind, target } in &cmd.redirects {
        if matches!(kind, RedirectKind::HereDoc(_)) {
            out.push(target);
        }
    }
    // A substitution in this command's own argv can carry a heredoc too, and
    // `ast::plan` reaches those after the command itself.
    for arg in &cmd.args {
        match arg {
            Arg::Positional(e)
            | Arg::Named { value: e, .. }
            | Arg::WordAssign { value: e, .. } => expr_heredocs(e, out),
            _ => {}
        }
    }
}

fn expr_heredocs<'a>(expr: &'a Expr, out: &mut Vec<&'a Expr>) {
    match expr {
        Expr::Command(cmd) => command_heredocs(cmd, out),
        Expr::CommandSubst(stmts) => block_heredocs(stmts, out),
        Expr::BinaryOp { left, right, .. } => {
            expr_heredocs(left, out);
            expr_heredocs(right, out);
        }
        _ => {}
    }
}

// ───────────────────────── Holes and session state ────────────────────────

/// Every `$(…)` in a body, in source order — including one nested inside a
/// `${VAR:-default}`. Missing one would expand the body around a hole and
/// call the result complete.
fn collect_holes(expr: &Expr, out: &mut Vec<Hole>) {
    match expr {
        Expr::HereDocBody { parts, .. } => {
            for part in parts {
                part_holes(&part.part, out);
            }
        }
        Expr::Interpolated(parts) => {
            for part in parts {
                part_holes(part, out);
            }
        }
        Expr::CommandSubst(stmts) => out.push(hole(expr, stmts)),
        _ => {}
    }
}

fn part_holes(part: &StringPart, out: &mut Vec<Hole>) {
    match part {
        StringPart::CommandSubst(stmts) => {
            out.push(hole(&Expr::CommandSubst(stmts.clone()), stmts))
        }
        StringPart::VarWithDefault { default, .. } => {
            for part in default {
                part_holes(part, out);
            }
        }
        _ => {}
    }
}

fn hole(expr: &Expr, stmts: &[Stmt]) -> Hole {
    let plans = stmts
        .iter()
        .filter(|s| !matches!(s, Stmt::Empty))
        .map(|s| plan_statement(s).plan)
        .collect();
    Hole::new(render_expr(expr), plans)
}

/// Name the session state a body reads, if any. These resolve from a live
/// session and cannot arrive through a supplied scope, so expanding them
/// against a fresh one would invent a value — `$?` would read 0 whatever the
/// last command did.
fn session_state_read(expr: &Expr) -> Option<String> {
    let parts: &[_] = match expr {
        Expr::HereDocBody { parts, .. } => return parts.iter().find_map(|p| part_state(&p.part)),
        Expr::Interpolated(parts) => parts,
        _ => return None,
    };
    parts.iter().find_map(part_state)
}

fn part_state(part: &StringPart) -> Option<String> {
    match part {
        StringPart::LastExitCode => Some("$?".to_string()),
        StringPart::CurrentPid => Some("$$".to_string()),
        StringPart::Positional(n) => Some(format!("${n}")),
        StringPart::AllArgs => Some("$@".to_string()),
        StringPart::ArgCount => Some("$#".to_string()),
        StringPart::VarWithDefault { default, .. } => default.iter().find_map(part_state),
        _ => None,
    }
}
