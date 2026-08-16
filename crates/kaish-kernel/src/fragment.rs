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

use crate::ast::plan::{heredoc_targets, plan_statement, render_expr};
use crate::ast::{Expr, Stmt, StringPart, VarPath, VarSegment};
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
    // Index the statements as parsed, because that is the position
    // `plan_program` publishes — it numbers before dropping the empty ones, so
    // a blank line or a comment shifts a filtered list out from under every
    // address after it. An empty statement carries no heredoc and falls out as
    // `NoSuchHeredoc` below.
    let stmt = program
        .statements
        .get(addr.statement)
        .ok_or(FragmentError::NoSuchStatement {
            asked: addr.statement,
            statements: program.statements.len(),
        })?;

    // The plan's own walk, so the index that resolves here is the index the
    // plan published. A second walk that had to agree is how an address comes
    // to name a different body than the one it was read from.
    let targets = heredoc_targets(stmt);
    let target = *targets.get(addr.heredoc).ok_or(FragmentError::NoSuchHeredoc {
        asked: addr.heredoc,
        heredocs: targets.len(),
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
        // Both halves: `${?:-fallback}` reads the exit code through its path
        // and never reaches the default, because an exit code is not empty.
        StringPart::VarWithDefault { path, default } => var_path_state(path)
            .or_else(|| default.iter().find_map(part_state)),
        // `$((…))` reads session state through spellings the interpolation
        // parser never turns into a part of its own — the arithmetic
        // evaluator resolves them itself.
        StringPart::Arithmetic(expr) => arithmetic_state(expr),
        // A braced `${?}` is a variable path, not `LastExitCode`, and the
        // scope resolves its root specially. `?`, `$`, and a digit run are
        // not names a caller can supply, so naming them costs no false
        // positive.
        StringPart::Var(path) | StringPart::VarLength(path) => var_path_state(path),
        _ => None,
    }
}

/// The session state a variable path reads, if any.
///
/// `?` and only `?`: the scope resolves that root to the last exit code
/// specially, so a fresh scope would answer 0 whatever the session did. Every
/// other root goes through ordinary lookup — `${$}` and `${1}` are undefined
/// names that expand to empty, exactly as they do when kaish executes, so
/// refusing them would block a body that expands correctly.
fn var_path_state(path: &VarPath) -> Option<String> {
    match path.segments.first()? {
        VarSegment::Field(name) if name == "?" => Some("${?}".to_string()),
        _ => None,
    }
}

/// The session state an arithmetic expression reads, if any.
///
/// `$((…))` is evaluated by `arithmetic.rs`, which resolves `$?`, `$$`, and a
/// positional `$N` itself — none of them ever becomes a `StringPart` this
/// module could see, so the expression text is all there is to go on.
///
/// The rule is deliberately the **complement** of the safe case rather than a
/// list of the unsafe ones: after `$` (and an optional `{`), an ordinary
/// variable name starts with a letter or `_`, and a caller can supply those.
/// Anything else — `?`, `$`, a digit, a spelling nobody has thought of yet —
/// reads something a supplied scope cannot carry, so it refuses. Enumerating
/// the unsafe spellings instead would make every one this misses expand
/// against a fresh session and invent a value.
///
/// Whitespace is skipped at both points because the evaluator's own `peek`
/// skips it, so `$( ( $ ? ) )` reads the exit code exactly as `$(($?))` does.
fn arithmetic_state(expr: &str) -> Option<String> {
    let chars: Vec<char> = expr.chars().collect();
    let skip_spaces = |mut i: usize| {
        while chars.get(i).is_some_and(|c| c.is_whitespace()) {
            i += 1;
        }
        i
    };
    for (i, c) in chars.iter().enumerate() {
        if *c != '$' {
            continue;
        }
        let mut j = skip_spaces(i + 1);
        if chars.get(j) == Some(&'{') {
            j = skip_spaces(j + 1);
        }
        match chars.get(j) {
            // An ordinary name — the caller can supply it.
            Some(c) if c.is_alphabetic() || *c == '_' => {}
            // Nothing at all after the `$` is not a read.
            None => {}
            Some(c) => return Some(format!("${c}")),
        }
    }
    None
}
