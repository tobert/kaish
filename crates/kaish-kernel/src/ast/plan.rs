//! The statement plan: an AST rendered back to shell text, **unexpanded**.
//!
//! A plan is parse information. It is built after validation and before
//! execution, so `${HOME}` and `$(...)` appear exactly as written — an
//! embedder judges what was asked, not what it resolved to, and the
//! substitution that would resolve them has not run.
//!
//! Two products, one AST walk each: `render_stmt` produces the text, and
//! [`planned_commands`] produces one [`PlannedCommand`] per command the
//! statement contains — control-structure bodies, `if` conditions, and
//! command substitutions included, because every one of them is a command
//! this statement would run.
//!
//! The same walk collects the statement's variables: the names it reads
//! (`free_variables`) and the names it writes (`bound_variables`). A name
//! that is both lands bound, never free.

use std::collections::BTreeSet;

use kaish_types::plan::{
    Plan, PlannedCommand, PlannedHeredoc, PlannedRedirect, PlannedValue, PLAN_RENDER_LIMIT,
};
use kaish_types::Value;

use super::types::{
    Arg, Assignment, BinaryOp, CaseStmt, Command, Expr, ForLoop, IfStmt, ListElem, Pipeline,
    PipelineStage, RecordKey, Redirect, RedirectKind, Stmt, StringPart, TestExpr, ToolDef, VarPath,
    VarSegment,
    WhileLoop,
};

/// The kaish version reported alongside a plan document.
///
/// Both JSON emitters — `kaish --plan`/`--plan-file` in kaish-repl and the
/// in-shell `plan` builtin — read this constant for their `kaish_version`
/// field, instead of each reading its own crate's `CARGO_PKG_VERSION`. The
/// kernel is what actually produces the plan, so its version is the honest
/// answer, and an embedder calling [`plan_program`] gets the same string a
/// `kaish --plan` caller does.
///
/// This is the bare semver (`"0.16.0"`), with no `kaish ` prefix and no
/// parenthesized hash/date suffix — a consumer windowing measurements by
/// version compares and sorts this value as-is, with no parsing. The build
/// identity lives in the sibling [`KAISH_GIT_HASH`] and [`KAISH_BUILD_DATE`]
/// fields instead of being folded into this one.
pub const KAISH_VERSION: &str = env!("CARGO_PKG_VERSION");

/// The git commit kaish was built from, short form, or `"unknown"` when none
/// was available at build time — a crates.io tarball build has no `.git` to
/// read, and `kaish-kernel/build.rs` falls back to this literal string.
/// Reported alongside [`KAISH_VERSION`] in every plan document, success or
/// error.
pub const KAISH_GIT_HASH: &str = match option_env!("KAISH_GIT_HASH") {
    Some(hash) => hash,
    None => "unknown",
};

/// The UTC date kaish was built, `YYYY-MM-DD`. Reported alongside
/// [`KAISH_VERSION`] in every plan document, success or error. Unlike
/// [`KAISH_GIT_HASH`], this is always the real build date — it comes from
/// the build script's clock, not from git, so it does not depend on a
/// `.git` checkout being present.
pub const KAISH_BUILD_DATE: &str = match option_env!("KAISH_BUILD_DATE") {
    Some(date) => date,
    None => "unknown",
};

/// One statement's plan.
pub struct StatementPlan {
    /// What the statement was asked to run.
    pub plan: Plan,
}

/// One statement of a planned program: its [`Plan`] and where it sits among
/// the planned statements.
///
/// `index` is the statement's position in the returned list, so an embedder
/// can name which statement it is talking about.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct PlannedStatement {
    /// The statement's position in the returned list, counted from 0 with no
    /// gaps: `plans[i].index == i`, always. Indexing the list by this number
    /// reads the statement it names.
    pub index: usize,
    /// What the statement was asked to run.
    pub plan: Plan,
}

/// Plan every statement of `source` without executing anything.
///
/// A plan is parse information: `${HOME}` and `$(...)` appear exactly as
/// written, no substitution has run, and no filesystem has been touched. That
/// is the point — an embedder judges what the statement *asked for*, before
/// anything it names can happen.
///
/// Each plan carries the statement's rendered text, every command it would
/// run (control-structure bodies, `if` conditions, and `$(...)` bodies
/// included), the variables it reads ([`Plan::free_variables`]) and the ones
/// it writes ([`Plan::bound_variables`]). Reading live session state for the
/// free set with [`Kernel::get_var`](crate::Kernel::get_var) closes the loop:
/// plan a statement, look up what it depends on, and decide with the values
/// in hand.
///
/// # Errors
///
/// Returns the parse errors when `source` does not parse. Each error's
/// [`format`](crate::parser::ParseError::format) renders a diagnostic against
/// the source.
pub fn plan_program(
    source: &str,
) -> Result<Vec<PlannedStatement>, Vec<crate::parser::ParseError>> {
    let program = crate::parser::parse(source)?;
    Ok(program
        .statements
        .iter()
        // An empty statement runs nothing and plans nothing. Dropping it
        // BEFORE numbering is what keeps `index` equal to the position in the
        // returned list: numbering first left a gap whenever the source opened
        // with a comment or a blank line, which is most scripts.
        .filter(|stmt| !matches!(stmt, Stmt::Empty))
        .enumerate()
        .map(|(index, stmt)| PlannedStatement {
            index,
            plan: plan_statement(stmt).plan,
        })
        .collect())
}

/// Build the plan for one top-level statement.
pub(crate) fn plan_statement(stmt: &Stmt) -> StatementPlan {
    let collected = collect(stmt);
    // Free = read and never written in-statement. A name that is both read
    // and written lands in `bound` — the safe direction: an embedder that
    // skips peeking it loses one lookup; one that peeked it would judge the
    // statement against a value the statement itself replaces.
    let free: Vec<String> = collected
        .reads
        .difference(&collected.binds)
        .cloned()
        .collect();
    let bound: Vec<String> = collected.binds.into_iter().collect();
    StatementPlan {
        plan: Plan::new(
            truncate_rendering(render_stmt(stmt)),
            stmt.kind_name(),
            collected.commands,
        )
        .with_variables(free, bound),
    }
}

/// Cut a rendering to [`PLAN_RENDER_LIMIT`] bytes, naming the cut.
///
/// The marker is loud and states the number, because a classifier reading a
/// silently shortened line would judge a statement it cannot see the end of.
/// The structure is not lost with the text — [`Plan::commands`] still names
/// every command.
fn truncate_rendering(rendered: String) -> String {
    if rendered.len() <= PLAN_RENDER_LIMIT {
        return rendered;
    }
    // Back up to a character boundary so the marker lands on valid UTF-8.
    let mut cut = PLAN_RENDER_LIMIT;
    while cut > 0 && !rendered.is_char_boundary(cut) {
        cut -= 1;
    }
    let mut out = rendered[..cut].to_string();
    out.push_str(&format!(
        "… [rendering truncated at {PLAN_RENDER_LIMIT} bytes]"
    ));
    out
}

// ───────────────────────── Command collection ─────────────────────────

/// What one collection walk produces: the statement's commands and its
/// variable analysis.
///
/// Every field is owned, not borrowed — a `$(...)` reached through `$((…))`
/// is parsed from a temporary `Vec<Stmt>` that lives only for the duration
/// of `read_arithmetic`, so nothing this struct holds can borrow from it.
/// That is what lets [`Collected::read_arith_expansion`]'s `CommandSubst` arm
/// walk straight into the SAME collector the rest of the statement uses,
/// instead of building a throwaway one and keeping only its `reads` — the
/// bug this type exists to rule out by construction.
#[derive(Default)]
struct Collected {
    commands: Vec<PlannedCommand>,
    /// Every variable name the statement reads, anywhere — `${x}`, a
    /// `"${x}"` interpolation, `${#x}`, a `[$k]` dynamic subscript, an
    /// identifier inside `$((…))`. kaish has no `eval` and no indirect
    /// expansion, so this set is complete by construction.
    reads: BTreeSet<String>,
    /// Every name the statement writes or binds — an assignment target, a
    /// `for` variable, an env-prefix name, a tool-def parameter.
    binds: BTreeSet<String>,
    /// Every heredoc target the walk has reached, in the order it reached
    /// them — the order that gives each one the flat
    /// [`PlannedHeredoc::index`] a plan publishes, so a heredoc inside a
    /// loop body is addressable without walking structure.
    ///
    /// Kept here rather than re-derived by a second walk. An address that
    /// resolves to a *different* body than the one it published is the worst
    /// failure this surface can have, and two traversals that have to agree
    /// is how you get one — this walk descends into redirect targets,
    /// interpolated strings, AND `$((…))` command substitutions, and a
    /// resolver written to match would have to remember to. `heredoc_targets[i]`
    /// is the target of the heredoc published with `index == i`, by
    /// construction. Cloned at the push site rather than borrowed, so a
    /// heredoc reached only through arithmetic's temporary parse tree still
    /// lands here.
    heredoc_targets: Vec<Expr>,
}

impl Collected {
    /// Publish every heredoc one command declares, numbering them in the
    /// order this walk reaches them.
    fn take_heredocs(&mut self, cmd: &Command) -> Vec<PlannedHeredoc> {
        cmd.redirects
            .iter()
            .filter_map(|r| match &r.kind {
                RedirectKind::HereDoc(meta) => Some((meta, &r.target)),
                _ => None,
            })
            .map(|(meta, target)| {
                let index = self.heredoc_targets.len();
                self.heredoc_targets.push(target.clone());
                // The body's own reads, not the statement's: an embedder
                // asking what plugs into *this* program wants the answer
                // scoped to it. A literal body reads nothing whatever it
                // contains, because nothing in it expands.
                let free = if meta.literal {
                    Vec::new()
                } else {
                    let mut body_reads = Collected::default();
                    collect_expr(target, false, &mut body_reads);
                    body_reads.reads.into_iter().collect()
                };
                PlannedHeredoc::new(
                    index,
                    meta.delimiter.clone(),
                    meta.literal,
                    meta.strip_tabs,
                    PlannedValue::Plain(meta.body.clone()),
                    meta.body_offset,
                )
                .with_free_variables(free)
            })
            .collect()
    }

    /// Record every read a variable path performs: its root name, plus any
    /// `[$k]` dynamic-subscript variable along the path.
    fn read_path(&mut self, path: &VarPath) {
        for (i, segment) in path.segments.iter().enumerate() {
            match segment {
                VarSegment::Field(name) if i == 0 => {
                    self.reads.insert(name.clone());
                }
                VarSegment::Dynamic(v) => {
                    self.reads.insert(v.clone());
                }
                _ => {}
            }
        }
    }

    /// Record the name an assignment path writes (its root), plus the reads
    /// its dynamic subscripts perform — `x[$k]=v` writes `x` and reads `k`.
    fn bind_path(&mut self, path: &VarPath) {
        if let Some(VarSegment::Field(name)) = path.segments.first() {
            self.binds.insert(name.clone());
        }
        for segment in path.segments.iter().skip(1) {
            if let VarSegment::Dynamic(v) = segment {
                self.reads.insert(v.clone());
            }
        }
    }

    /// Record every variable a `$(( ))` (or bare `(( ))`) reads: every
    /// `Ref` name — bare or `$`-prefixed, a bare subscript's root AND its
    /// index expression (`xs[i]` reads both `xs` and `i` — Decision B, the
    /// index is itself arithmetic) — plus a `${...}`/`base#$var`/nested
    /// `$((...))` operand's own reads, and a `$(...)` operand's commands,
    /// binds, and heredocs (via `collect_block`, the same walker a
    /// bare `$(...)` already goes through, so the two agree by construction
    /// rather than by two implementations staying in sync by hand). `$?`/`$$`/a positional
    /// parameter are not session variables, matching every other reader of
    /// them in this file. Parses the text with the real arithmetic parser
    /// rather than scanning for identifier-shaped substrings — the old
    /// scan read `ff` out of `16#ff` and `xff` out of `0xff` as if they
    /// were variables. The shell parser and validator both defer
    /// arithmetic to runtime — an unparsable body is syntactically valid
    /// shell — so a syntax error here reads no variables rather than
    /// failing the plan; the statement itself still fails loudly when it
    /// runs. `background` is the enclosing pipeline's `&`, threaded through
    /// so a `$(...)` reached this way plans backgrounded exactly like a
    /// bare `$(...)` does.
    fn read_arithmetic(&mut self, expr: &str, background: bool) {
        if let Ok(parsed) = crate::arithmetic::parse(expr) {
            self.read_arith_expr(&parsed, background);
        }
    }

    fn read_arith_expr(&mut self, expr: &crate::arithmetic::ArithExpr, background: bool) {
        use crate::arithmetic::ArithExpr;
        match expr {
            ArithExpr::Int(_) => {}
            ArithExpr::Expansion(e) => self.read_arith_expansion(e, background),
            ArithExpr::Subscript { root, indices } => {
                self.reads.insert(root.clone());
                for index in indices {
                    self.read_arith_expr(index, background);
                }
            }
            ArithExpr::BasedExpansion { expansion, .. } => {
                self.read_arith_expansion(expansion, background)
            }
            ArithExpr::Unary { operand, .. } => self.read_arith_expr(operand, background),
            ArithExpr::Binary { left, right, .. } => {
                self.read_arith_expr(left, background);
                self.read_arith_expr(right, background);
            }
            ArithExpr::Ternary { cond, then_branch, else_branch } => {
                self.read_arith_expr(cond, background);
                self.read_arith_expr(then_branch, background);
                self.read_arith_expr(else_branch, background);
            }
        }
    }

    fn read_arith_expansion(&mut self, e: &crate::arithmetic::Expansion, background: bool) {
        use crate::arithmetic::Expansion;
        match e {
            // A bare `$1` is a positional parameter, not a session
            // variable — same exclusion `collect_expr` applies to
            // `Expr::Positional` below.
            Expansion::Var(name) => {
                if name.parse::<usize>().is_err() {
                    self.reads.insert(name.clone());
                }
            }
            Expansion::BracedPath { root, brackets } => {
                let raw = format!("${{{root}{brackets}}}");
                self.read_path(&crate::parser::parse_varpath(&raw));
            }
            Expansion::BracedDefault { root, brackets, default } => {
                let raw = format!("${{{root}{brackets}}}");
                self.read_path(&crate::parser::parse_varpath(&raw));
                if let Ok(parsed) = crate::arithmetic::parse(default) {
                    self.read_arith_expr(&parsed, background);
                }
            }
            Expansion::LastExitCode | Expansion::CurrentPid => {}
            // Walk straight into `self` — commands, binds, and
            // heredocs all land in the one flat walk, not just `reads`. A
            // `$(...)` inside `$((…))` is a command this statement runs,
            // same as a bare `$(...)` in an argument. `background` is the
            // enclosing pipeline's `&`, threaded from the caller rather
            // than hardcoded, so it plans the same as a bare `$(...)`.
            Expansion::CommandSubst(stmts) => collect_block(stmts, background, self),
            Expansion::Nested(inner) => self.read_arith_expr(inner, background),
        }
    }
}

/// Every command the statement contains, in source order, plus any literal
/// redemption key its argv carries.
///
/// A `for` body's commands, an `if` condition's command, and a `$(…)`
/// substitution's commands are all in here: each is a command this statement
/// would run, so each is a `cmd` resource a standing grant has to cover.
fn collect(stmt: &Stmt) -> Collected {
    let mut out = Collected::default();
    collect_stmt(stmt, false, &mut out);
    out
}

/// Every heredoc target the statement contains, indexed by the flat
/// [`PlannedHeredoc::index`] the plan publishes.
///
/// This is the **same walk** that numbers them, not a second one that agrees
/// with it — `heredoc_targets(stmt)[i]` is the target of the heredoc the plan
/// published with `index == i`, by construction rather than by test.
pub(crate) fn heredoc_targets(stmt: &Stmt) -> Vec<Expr> {
    collect(stmt).heredoc_targets
}

/// Every command the statement contains, in source order.
pub fn planned_commands(stmt: &Stmt) -> Vec<PlannedCommand> {
    collect(stmt).commands
}

fn collect_stmt(stmt: &Stmt, background: bool, out: &mut Collected) {
    match stmt {
        Stmt::Assignment(a) => {
            out.bind_path(&a.path);
            collect_expr(&a.value, background, out)
        }
        Stmt::Command(cmd) => collect_command(cmd, background, out),
        Stmt::Pipeline(p) => {
            for stage in &p.stages {
                match stage {
                    PipelineStage::Command(cmd) => {
                        collect_command(cmd, background || p.background, out)
                    }
                    // A compound stage's commands belong to the enclosing
                    // statement, same as a loop body's do.
                    PipelineStage::Compound(stmt) => {
                        collect_stmt(stmt, background || p.background, out)
                    }
                }
            }
        }
        Stmt::If(s) => {
            collect_expr(&s.condition, background, out);
            collect_block(&s.then_branch, background, out);
            if let Some(else_branch) = &s.else_branch {
                collect_block(else_branch, background, out);
            }
        }
        Stmt::For(s) => {
            out.binds.insert(s.variable.clone());
            for item in &s.items {
                collect_expr(item, background, out);
            }
            collect_block(&s.body, background, out);
        }
        Stmt::While(s) => {
            collect_expr(&s.condition, background, out);
            collect_block(&s.body, background, out);
        }
        Stmt::Case(s) => {
            collect_expr(&s.expr, background, out);
            for branch in &s.branches {
                collect_block(&branch.body, background, out);
            }
        }
        Stmt::Return(e) | Stmt::Exit(e) => {
            if let Some(e) = e {
                collect_expr(e, background, out);
            }
        }
        Stmt::ToolDef(def) => {
            for param in &def.params {
                out.binds.insert(param.name.clone());
                if let Some(default) = &param.default {
                    collect_expr(default, background, out);
                }
            }
            collect_block(&def.body, background, out)
        }
        Stmt::Test(t) => collect_test(t, background, out),
        // Same walk as `Expr::Arithmetic`: reads its variables, and any
        // `$(...)` operand inside walks into `PlannedCommand`s, binds,
        // and heredocs too — `(( $(cmd) ))` plans `cmd`.
        Stmt::Arith(expr) => out.read_arithmetic(expr, background),
        Stmt::AndChain { left, right } | Stmt::OrChain { left, right } => {
            collect_stmt(left, background, out);
            collect_stmt(right, background, out);
        }
        Stmt::EnvScoped { assignments, body } => {
            for a in assignments {
                out.bind_path(&a.path);
                collect_expr(&a.value, background, out);
            }
            collect_stmt(body, background, out);
        }
        Stmt::Break(_) | Stmt::Continue(_) | Stmt::Empty => {}
    }
}

fn collect_block(stmts: &[Stmt], background: bool, out: &mut Collected) {
    for stmt in stmts {
        collect_stmt(stmt, background, out);
    }
}

fn collect_command(cmd: &Command, background: bool, out: &mut Collected) {
    let args: Vec<PlannedValue> = cmd.args.iter().map(|arg| plan_arg(arg).1).collect();
    let redirects = cmd
        .redirects
        .iter()
        .map(|r| PlannedRedirect::new(r.kind.to_string(), plan_redirect_target(r)))
        .collect();
    let heredocs = out.take_heredocs(cmd);
    out.commands.push(
        PlannedCommand::new(cmd.name.clone(), args, redirects, background)
            .with_heredocs(heredocs),
    );
    // Substitutions nested inside this command's own arguments and redirect
    // targets are commands too, and they run before it does.
    for arg in &cmd.args {
        match arg {
            Arg::Positional(e) => collect_expr(e, background, out),
            Arg::Named { value, .. } | Arg::WordAssign { value, .. } => {
                collect_expr(value, background, out)
            }
            Arg::ShortFlag(_) | Arg::LongFlag(_) | Arg::DoubleDash => {}
        }
    }
    for redirect in &cmd.redirects {
        collect_expr(&redirect.target, background, out);
    }
}

fn collect_expr(expr: &Expr, background: bool, out: &mut Collected) {
    match expr {
        Expr::Command(cmd) => collect_command(cmd, background, out),
        Expr::CommandSubst(stmts) => collect_block(stmts, background, out),
        // The negation runs its inner command; the plan must show it.
        Expr::Not(inner) => collect_expr(inner, background, out),
        Expr::BinaryOp { left, right, .. } => {
            collect_expr(left, background, out);
            collect_expr(right, background, out);
        }
        Expr::Interpolated(parts) => collect_parts(parts, background, out),
        Expr::HereDocBody { parts, .. } => {
            for part in parts {
                collect_part(&part.part, background, out);
            }
        }
        Expr::Test(t) => collect_test(t, background, out),
        Expr::VarWithDefault { path, default } => {
            out.read_path(path);
            collect_parts(default, background, out)
        }
        Expr::ListLiteral(elems) => {
            for elem in elems {
                match elem {
                    ListElem::Item(e) | ListElem::Spread(e) => collect_expr(e, background, out),
                }
            }
        }
        Expr::RecordLiteral(entries) => {
            for entry in entries {
                if let RecordKey::Interpolated(parts) = &entry.key {
                    collect_parts(parts, background, out);
                }
                collect_expr(&entry.value, background, out);
            }
        }
        Expr::VarRef(path) | Expr::VarLength(path) => out.read_path(path),
        Expr::Arithmetic(e) => out.read_arithmetic(e, background),
        Expr::Arith(e) => out.read_arithmetic(e, background),
        // Special forms ($1, $@, $#, $?, $$) are not session variables; an
        // embedder cannot peek them with `get_var`, so they are not listed.
        Expr::Literal(_)
        | Expr::NumericLiteral { .. }
        | Expr::Positional(_)
        | Expr::AllArgs
        | Expr::ArgCount
        | Expr::LastExitCode
        | Expr::CurrentPid
        | Expr::GlobPattern(_) => {}
    }
}

fn collect_parts(parts: &[StringPart], background: bool, out: &mut Collected) {
    for part in parts {
        collect_part(part, background, out);
    }
}

fn collect_part(part: &StringPart, background: bool, out: &mut Collected) {
    match part {
        StringPart::CommandSubst(stmts) => collect_block(stmts, background, out),
        StringPart::VarWithDefault { path, default } => {
            out.read_path(path);
            collect_parts(default, background, out)
        }
        StringPart::Var(path) | StringPart::VarLength(path) => out.read_path(path),
        StringPart::Arithmetic(e) => out.read_arithmetic(e, background),
        // See the identical special-forms note in `collect_expr`.
        StringPart::Literal(_)
        | StringPart::Positional(_)
        | StringPart::AllArgs
        | StringPart::ArgCount
        | StringPart::LastExitCode
        | StringPart::CurrentPid => {}
    }
}

fn collect_test(test: &TestExpr, background: bool, out: &mut Collected) {
    match test {
        TestExpr::FileTest { path, .. } => collect_expr(path, background, out),
        TestExpr::StringTest { value, .. } => collect_expr(value, background, out),
        TestExpr::Comparison { left, right, .. }
        | TestExpr::In { left, right }
        | TestExpr::NotIn { left, right } => {
            collect_expr(left, background, out);
            collect_expr(right, background, out);
        }
        TestExpr::And { left, right } | TestExpr::Or { left, right } => {
            collect_test(left, background, out);
            collect_test(right, background, out);
        }
        TestExpr::Not { expr } => collect_test(expr, background, out),
    }
}

// ───────────────────────── Rendering ─────────────────────────

/// Render one statement back to shell text, unexpanded.
pub(crate) fn render_stmt(stmt: &Stmt) -> String {
    match stmt {
        Stmt::Assignment(a) => render_assignment(a),
        Stmt::Command(cmd) => render_command(cmd),
        Stmt::Pipeline(p) => render_pipeline(p),
        Stmt::If(s) => render_if(s),
        Stmt::For(s) => render_for(s),
        Stmt::While(s) => render_while(s),
        Stmt::Case(s) => render_case(s),
        Stmt::Break(n) => render_keyword("break", n.map(|n| n.to_string())),
        Stmt::Continue(n) => render_keyword("continue", n.map(|n| n.to_string())),
        Stmt::Return(e) => render_keyword("return", e.as_ref().map(|e| render_expr(e))),
        Stmt::Exit(e) => render_keyword("exit", e.as_ref().map(|e| render_expr(e))),
        Stmt::ToolDef(def) => render_tooldef(def),
        Stmt::Test(t) => format!("[[ {} ]]", render_test(t)),
        Stmt::Arith(e) => format!("(({e}))"),
        Stmt::AndChain { left, right } => {
            format!("{} && {}", render_stmt(left), render_stmt(right))
        }
        Stmt::OrChain { left, right } => {
            format!("{} || {}", render_stmt(left), render_stmt(right))
        }
        Stmt::EnvScoped { assignments, body } => {
            let prefix: Vec<String> = assignments.iter().map(render_assignment).collect();
            format!("{} {}", prefix.join(" "), render_stmt(body))
        }
        Stmt::Empty => String::new(),
    }
}

fn render_keyword(word: &str, operand: Option<String>) -> String {
    match operand {
        Some(operand) => format!("{word} {operand}"),
        None => word.to_string(),
    }
}

fn render_block(stmts: &[Stmt]) -> String {
    stmts
        .iter()
        .filter(|s| !matches!(s, Stmt::Empty))
        .map(render_stmt)
        .collect::<Vec<_>>()
        .join("; ")
}

fn render_assignment(a: &Assignment) -> String {
    let path = render_varpath(&a.path);
    if a.local {
        format!("local {} = {}", path, render_expr(&a.value))
    } else {
        format!("{}={}", path, render_expr(&a.value))
    }
}

/// Render one command: argv0, every argument form, and every redirect.
pub(crate) fn render_command(cmd: &Command) -> String {
    let mut parts = vec![cmd.name.clone()];
    for arg in &cmd.args {
        parts.push(plan_arg(arg).0);
    }
    for redirect in &cmd.redirects {
        parts.push(render_redirect(redirect));
    }
    parts.join(" ")
}

/// Plan one argument: its flat text (for [`render_command`]) and its
/// [`PlannedValue`] (for [`PlannedCommand::args`]), derived together so the
/// two representations cannot disagree about what this argument was.
fn plan_arg(arg: &Arg) -> (String, PlannedValue) {
    let text = match arg {
        Arg::Positional(e) => render_expr(e),
        Arg::Named { key, value } => format!("--{key}={}", render_expr(value)),
        Arg::WordAssign { key, value } => format!("{key}={}", render_expr(value)),
        Arg::ShortFlag(f) => format!("-{f}"),
        Arg::LongFlag(f) => format!("--{f}"),
        Arg::DoubleDash => "--".to_string(),
    };
    (text.clone(), PlannedValue::Plain(text))
}

/// Plan one redirect's target: rendered unexpanded, always plain.
///
/// A heredoc's target is its delimiter word, which is what stands after `<<`
/// in the source. Rendering the *body* here would repeat what
/// [`PlannedCommand::heredocs`] carries structurally, and rendering it from
/// the target expression spells every delimiter `EOF` — the body has lost the
/// word by then.
///
/// [`PlannedCommand::heredocs`]: kaish_types::plan::PlannedCommand::heredocs
fn plan_redirect_target(redirect: &Redirect) -> PlannedValue {
    match &redirect.kind {
        RedirectKind::HereDoc(meta) => {
            let quote = if meta.literal { "'" } else { "" };
            PlannedValue::Plain(format!("{quote}{}{quote}", meta.delimiter))
        }
        _ => PlannedValue::Plain(render_expr(&redirect.target)),
    }
}

fn render_redirect(redirect: &Redirect) -> String {
    // A merge redirect (`2>&1`, `1>&2`) is the whole operator: its target
    // expression is a placeholder, and printing it would invent a filename.
    match &redirect.kind {
        RedirectKind::MergeStderr | RedirectKind::MergeStdout => redirect.kind.to_string(),
        // A heredoc renders back the way it was written — its own delimiter
        // word, its own body. Spelling every delimiter `EOF` would erase the
        // hint the author chose (`PY`, `SQL`) from the one field a classifier
        // reads first.
        RedirectKind::HereDoc(meta) => {
            let dash = if meta.strip_tabs { "-" } else { "" };
            let quote = if meta.literal { "'" } else { "" };
            format!(
                "<<{dash}{quote}{delim}{quote}\n{body}{delim}",
                delim = meta.delimiter,
                body = meta.body,
            )
        }
        _ => format!(
            "{} {}",
            redirect.kind,
            plan_redirect_target(redirect).display()
        ),
    }
}

fn render_pipeline(p: &Pipeline) -> String {
    let body = p
        .stages
        .iter()
        .map(|stage| match stage {
            PipelineStage::Command(cmd) => render_command(cmd),
            PipelineStage::Compound(stmt) => render_stmt(stmt),
        })
        .collect::<Vec<_>>()
        .join(" | ");
    if p.background {
        format!("{body} &")
    } else {
        body
    }
}

fn render_if(s: &IfStmt) -> String {
    let mut out = format!(
        "if {}; then {}",
        render_expr(&s.condition),
        render_block(&s.then_branch)
    );
    if let Some(else_branch) = &s.else_branch {
        let rendered = render_block(else_branch);
        if !rendered.is_empty() {
            out.push_str(&format!("; else {rendered}"));
        }
    }
    out.push_str("; fi");
    out
}

fn render_for(s: &ForLoop) -> String {
    let items: Vec<String> = s.items.iter().map(render_expr).collect();
    format!(
        "for {} in {}; do {}; done",
        s.variable,
        items.join(" "),
        render_block(&s.body)
    )
}

fn render_while(s: &WhileLoop) -> String {
    format!(
        "while {}; do {}; done",
        render_expr(&s.condition),
        render_block(&s.body)
    )
}

fn render_case(s: &CaseStmt) -> String {
    let branches: Vec<String> = s
        .branches
        .iter()
        .map(|b| format!("{}) {} ;;", b.patterns.join("|"), render_block(&b.body)))
        .collect();
    format!("case {} in {} esac", render_expr(&s.expr), branches.join(" "))
}

fn render_tooldef(def: &ToolDef) -> String {
    let params: Vec<String> = def
        .params
        .iter()
        .map(|p| match &p.default {
            Some(default) => format!("{}={}", p.name, render_expr(default)),
            None => p.name.clone(),
        })
        .collect();
    format!(
        "tool {}({}) {{ {} }}",
        def.name,
        params.join(", "),
        render_block(&def.body)
    )
}

/// Render one expression back to shell text, unexpanded: a variable
/// reference stays `${NAME}` and a substitution stays `$(…)`.
pub(crate) fn render_expr(expr: &Expr) -> String {
    match expr {
        Expr::Not(inner) => format!("! {}", render_expr(inner)),
        Expr::Literal(v) => render_literal(v),
        Expr::VarRef(path) => format!("${{{}}}", render_varpath(path)),
        Expr::Interpolated(parts) => format!("\"{}\"", render_parts(parts)),
        Expr::HereDocBody { parts, strip_tabs } => {
            let dash = if *strip_tabs { "-" } else { "" };
            let body: Vec<String> = parts.iter().map(|sp| render_part(&sp.part)).collect();
            format!("<<{dash}EOF\n{}\nEOF", body.join(""))
        }
        Expr::BinaryOp { left, op, right } => {
            let op = match op {
                BinaryOp::And => "&&",
                BinaryOp::Or => "||",
            };
            format!("{} {} {}", render_expr(left), op, render_expr(right))
        }
        Expr::CommandSubst(stmts) => format!("$({})", render_block(stmts)),
        Expr::Test(t) => format!("[[ {} ]]", render_test(t)),
        Expr::Positional(n) => format!("${n}"),
        Expr::AllArgs => "$@".to_string(),
        Expr::ArgCount => "$#".to_string(),
        Expr::VarLength(path) => format!("${{#{}}}", render_varpath(path)),
        Expr::VarWithDefault { path, default } => {
            format!("${{{}:-{}}}", render_varpath(path), render_parts(default))
        }
        Expr::Arithmetic(e) => format!("$(({e}))"),
        Expr::Arith(e) => format!("(({e}))"),
        // Render the source text, not `value`'s canonical form — that is what
        // this variant is for.
        Expr::NumericLiteral { raw, .. } => raw.clone(),
        Expr::Command(cmd) => render_command(cmd),
        Expr::LastExitCode => "$?".to_string(),
        Expr::CurrentPid => "$$".to_string(),
        Expr::GlobPattern(p) => p.clone(),
        Expr::ListLiteral(elems) => {
            let parts: Vec<String> = elems
                .iter()
                .map(|e| match e {
                    ListElem::Item(e) => render_expr(e),
                    ListElem::Spread(e) => format!("...{}", render_expr(e)),
                })
                .collect();
            format!("[{}]", parts.join(" "))
        }
        Expr::RecordLiteral(entries) => {
            let parts: Vec<String> = entries
                .iter()
                .map(|entry| {
                    let key = match &entry.key {
                        RecordKey::Bare(k) => k.clone(),
                        RecordKey::Quoted(k) => format!("\"{k}\""),
                        RecordKey::Interpolated(parts) => format!("\"{}\"", render_parts(parts)),
                    };
                    format!("{key}: {}", render_expr(&entry.value))
                })
                .collect();
            format!("{{{}}}", parts.join(", "))
        }
    }
}

/// A literal, quoted only where a shell reader would need the quotes.
fn render_literal(value: &Value) -> String {
    match value {
        Value::String(s) => quote_word(s),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Null => "null".to_string(),
        Value::Json(j) => j.to_string(),
        // Binary reaches a plan only through `execute_argv`, which takes
        // typed values. Naming the length is honest; printing the bytes
        // would put unreadable data in a stored plan.
        Value::Bytes(b) => format!("<bytes len={}>", b.len()),
    }
}

/// Single-quote a word that a shell reader could not take literally.
fn quote_word(s: &str) -> String {
    let needs_quotes = s.is_empty()
        || s.chars()
            .any(|c| c.is_whitespace() || "\"'$`&|;<>(){}[]*?#!~\\".contains(c));
    if !needs_quotes {
        return s.to_string();
    }
    // `'\''` is the one portable way to put a single quote inside a
    // single-quoted word.
    format!("'{}'", s.replace('\'', "'\\''"))
}

fn render_parts(parts: &[StringPart]) -> String {
    parts.iter().map(render_part).collect::<Vec<_>>().join("")
}

fn render_part(part: &StringPart) -> String {
    match part {
        StringPart::Literal(s) => s.replace('\\', "\\\\").replace('"', "\\\""),
        StringPart::Var(path) => format!("${{{}}}", render_varpath(path)),
        StringPart::VarWithDefault { path, default } => {
            format!("${{{}:-{}}}", render_varpath(path), render_parts(default))
        }
        StringPart::VarLength(path) => format!("${{#{}}}", render_varpath(path)),
        StringPart::Positional(n) => format!("${n}"),
        StringPart::AllArgs => "$@".to_string(),
        StringPart::ArgCount => "$#".to_string(),
        StringPart::Arithmetic(e) => format!("$(({e}))"),
        StringPart::CommandSubst(stmts) => format!("$({})", render_block(stmts)),
        StringPart::LastExitCode => "$?".to_string(),
        StringPart::CurrentPid => "$$".to_string(),
    }
}

fn render_test(test: &TestExpr) -> String {
    match test {
        TestExpr::FileTest { op, path } => format!("{} {}", op, render_expr(path)),
        TestExpr::StringTest { op, value } => format!("{} {}", op, render_expr(value)),
        TestExpr::Comparison { left, op, right } => {
            format!("{} {} {}", render_expr(left), op, render_expr(right))
        }
        TestExpr::And { left, right } => {
            format!("{} && {}", render_test(left), render_test(right))
        }
        TestExpr::Or { left, right } => {
            format!("{} || {}", render_test(left), render_test(right))
        }
        TestExpr::Not { expr } => format!("! {}", render_test(expr)),
        TestExpr::In { left, right } => {
            format!("{} in {}", render_expr(left), render_expr(right))
        }
        TestExpr::NotIn { left, right } => {
            format!("{} not in {}", render_expr(left), render_expr(right))
        }
    }
}

/// Render a variable path in its source form: the root name, then bracket
/// subscripts. Never dotted — kaish access is brackets-only.
fn render_varpath(path: &VarPath) -> String {
    let mut out = String::new();
    for (i, segment) in path.segments.iter().enumerate() {
        match segment {
            VarSegment::Field(name) => {
                if i > 0 {
                    out.push('.');
                }
                out.push_str(name);
            }
            VarSegment::Index(idx) => out.push_str(&format!("[{idx}]")),
            VarSegment::Key(k) => out.push_str(&format!("[{k}]")),
            VarSegment::Dynamic(v) => out.push_str(&format!("[${v}]")),
            VarSegment::Slice(a, b) => out.push_str(&format!(
                "[{}:{}]",
                a.map(|n| n.to_string()).unwrap_or_default(),
                b.map(|n| n.to_string()).unwrap_or_default()
            )),
        }
    }
    out
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;
    use crate::parser::parse;

    fn planned_of(source: &str) -> StatementPlan {
        let program = parse(source).expect("the fixture parses");
        let stmt = program
            .statements
            .into_iter()
            .find(|s| !matches!(s, Stmt::Empty))
            .expect("one statement");
        plan_statement(&stmt)
    }

    fn plan_of(source: &str) -> Plan {
        planned_of(source).plan
    }

    #[test]
    fn a_variable_renders_unexpanded() {
        let plan = plan_of("rm -r \"${HOME}/build\"");
        assert!(
            plan.rendered.contains("${HOME}"),
            "the plan must keep the variable as written: {}",
            plan.rendered
        );
    }

    #[test]
    fn a_substitution_renders_unexpanded_and_plans_its_own_command() {
        let plan = plan_of("rm $(cat list.txt)");
        assert!(
            plan.rendered.contains("$(cat list.txt)"),
            "got: {}",
            plan.rendered
        );
        let names: Vec<&str> = plan.commands.iter().map(|c| c.name.as_str()).collect();
        assert_eq!(names, vec!["rm", "cat"], "the substitution runs too");
    }

    #[test]
    fn a_loop_body_belongs_to_the_enclosing_statement() {
        let plan = plan_of("for f in a b; do rm $f; done");
        assert_eq!(plan.statement_kind, "for");
        let names: Vec<&str> = plan.commands.iter().map(|c| c.name.as_str()).collect();
        assert_eq!(names, vec!["rm"]);
        assert!(plan.rendered.starts_with("for f in a b; do rm"));
    }

    #[test]
    fn every_redirect_form_renders() {
        let plan = plan_of("cmd > out.txt 2> err.txt < in.txt");
        let kinds: Vec<&str> = plan.commands[0]
            .redirects
            .iter()
            .map(|r| r.kind.as_str())
            .collect();
        assert_eq!(kinds, vec![">", "2>", "<"]);
        assert_eq!(
            plan.commands[0].redirects[0].target,
            PlannedValue::Plain("out.txt".to_string())
        );
        assert!(plan.rendered.contains("> out.txt"), "got: {}", plan.rendered);
    }

    #[test]
    fn a_redirect_target_stays_unexpanded() {
        let plan = plan_of("echo hi > ${LOG}");
        assert_eq!(
            plan.commands[0].redirects[0].target,
            PlannedValue::Plain("${LOG}".to_string())
        );
    }

    #[test]
    fn a_merge_redirect_renders_as_its_operator_alone() {
        let plan = plan_of("cmd 2>&1");
        assert!(
            plan.rendered.ends_with("2>&1"),
            "a merge redirect has no filename: {}",
            plan.rendered
        );
    }

    #[test]
    fn every_argument_form_renders() {
        let plan = plan_of("tool -v --force --key=value word -- --after");
        let args = &plan.commands[0].args;
        assert_eq!(
            args,
            &vec![
                PlannedValue::Plain("-v".to_string()),
                PlannedValue::Plain("--force".to_string()),
                PlannedValue::Plain("--key=value".to_string()),
                PlannedValue::Plain("word".to_string()),
                PlannedValue::Plain("--".to_string()),
                PlannedValue::Plain("--after".to_string()),
            ]
        );
    }

    #[test]
    fn a_backgrounded_pipeline_marks_every_command() {
        let plan = plan_of("a | b &");
        assert!(plan.commands.iter().all(|c| c.background));
        assert!(plan.rendered.ends_with('&'), "got: {}", plan.rendered);
    }

    #[test]
    fn a_pipeline_renders_every_stage() {
        let plan = plan_of("cat f | grep x | wc -l");
        let names: Vec<&str> = plan.commands.iter().map(|c| c.name.as_str()).collect();
        assert_eq!(names, vec!["cat", "grep", "wc"]);
        assert_eq!(plan.rendered, "cat f | grep x | wc -l");
    }

    #[test]
    fn an_and_chain_plans_both_sides() {
        let plan = plan_of("mkdir d && rm -r d");
        assert_eq!(plan.statement_kind, "and_chain");
        let names: Vec<&str> = plan.commands.iter().map(|c| c.name.as_str()).collect();
        assert_eq!(names, vec!["mkdir", "rm"]);
    }

    #[test]
    fn an_if_plans_its_condition_and_both_branches() {
        let plan = plan_of("if grep -q x f; then echo hit; else echo miss; fi");
        let names: Vec<&str> = plan.commands.iter().map(|c| c.name.as_str()).collect();
        assert_eq!(names, vec!["grep", "echo", "echo"]);
    }

    #[test]
    fn a_quoted_word_keeps_its_spaces_inside_quotes() {
        let plan = plan_of("echo 'two words'");
        assert_eq!(plan.rendered, "echo 'two words'");
    }

    #[test]
    fn an_interpolated_string_keeps_its_variables() {
        let plan = plan_of("echo \"hello ${NAME}\"");
        assert_eq!(plan.rendered, "echo \"hello ${NAME}\"");
    }

    #[test]
    fn a_bracket_path_renders_with_brackets_not_dots() {
        let plan = plan_of("echo ${servers[web]}");
        assert_eq!(plan.rendered, "echo ${servers[web]}");
    }

    #[test]
    fn rendering_truncates_at_the_limit_with_a_loud_marker() {
        let long = "x".repeat(PLAN_RENDER_LIMIT * 2);
        let plan = plan_of(&format!("echo {long}"));
        assert!(
            plan.rendered.contains("[rendering truncated at 8192 bytes]"),
            "expected the marker, got {} bytes ending in {:?}",
            plan.rendered.len(),
            &plan.rendered[plan.rendered.len().saturating_sub(48)..]
        );
        // The structure survives the cut — that is what a classifier reads.
        assert_eq!(plan.commands.len(), 1);
        assert_eq!(plan.commands[0].name, "echo");
    }

    #[test]
    fn a_short_rendering_carries_no_marker() {
        let plan = plan_of("echo hi");
        assert_eq!(plan.rendered, "echo hi");
    }

    // ── `--confirm=` is an ordinary argument, not a credential ──
    //
    // kaish removed the confirmation latch in 0.14.0; no tool parses
    // `--confirm=<token>` any more. The plan-side lift/redaction that used
    // to treat it as a secret protected a credential that no longer exists,
    // and is gone too — `confirm=` now renders exactly like any other named
    // argument, direct or reached through `$(( ))`.

    #[test]
    fn confirm_renders_as_an_ordinary_argument() {
        let plan = plan_of("rm --confirm=deadbeef target.txt");
        assert_eq!(plan.rendered, "rm --confirm=deadbeef target.txt");
        assert_eq!(
            plan.commands[0].args,
            vec![
                PlannedValue::Plain("--confirm=deadbeef".to_string()),
                PlannedValue::Plain("target.txt".to_string()),
            ]
        );
    }

    #[test]
    fn a_confirm_token_renders_the_same_direct_and_through_arithmetic() {
        // A cross-model review found the arithmetic form of a `$(...)`
        // rendering a confirm token in cleartext while the direct form
        // redacted it — the two disagreeing was the leak. With the
        // redaction mechanism gone, both forms must render identically,
        // in cleartext, since there is nothing left to redact.
        let direct = plan_of("echo $(rm --confirm=secret x)");
        let via_arith = plan_of("echo $((1 + $(rm --confirm=secret x)))");
        assert_eq!(direct.rendered, "echo $(rm --confirm=secret x)");
        assert_eq!(
            via_arith.rendered,
            "echo $((1 + $(rm --confirm=secret x)))"
        );
        assert!(direct.rendered.contains("--confirm=secret"));
        assert!(via_arith.rendered.contains("--confirm=secret"));
    }

    // ── Variable analysis ──

    #[test]
    fn reads_cover_interpolation_length_subscript_and_arithmetic() {
        let plan = plan_of(
            "echo \"${greeting} ${#items} ${servers[$env]}\" $((base + offset))",
        );
        assert_eq!(
            plan.free_variables,
            vec!["base", "env", "greeting", "items", "offset", "servers"],
            "every lexical read is listed, sorted"
        );
        assert!(plan.bound_variables.is_empty());
    }

    // ── Arithmetic reads via the real parser, not a text scan ──
    //
    // The scan used to treat any identifier-shaped substring as a
    // variable, so a base literal's own digits (`ff` in `16#ff`, `xff` in
    // `0xff`) were reported as free variables `get_var` can never resolve
    // — a plan consumer (kaijutsu) reading this to decide what a script
    // needs before running it got a wrong answer, not a cosmetic one.

    #[test]
    fn a_based_literal_reads_nothing() {
        assert!(plan_of("echo $((16#ff + 1))").free_variables.is_empty());
    }

    #[test]
    fn a_hex_literal_reads_nothing_but_a_real_operand_still_does() {
        assert_eq!(plan_of("echo $((0xff + x))").free_variables, vec!["x"]);
    }

    #[test]
    fn based_expansion_reads_the_variable_not_the_base() {
        assert_eq!(plan_of("echo $((10#$m % 12))").free_variables, vec!["m"]);
    }

    #[test]
    fn ternary_reads_both_branches_deduped_and_sorted() {
        assert_eq!(plan_of("echo $((a > b ? a : b))").free_variables, vec!["a", "b"]);
    }

    #[test]
    fn a_bare_subscript_reads_the_root_and_the_index_variable() {
        // Decision B: `xs[i]` reads `i` as a variable (the index is itself
        // arithmetic) as well as `xs` — unlike `${xs[i]}`, where `i` is a
        // literal key and only `xs` is read.
        assert_eq!(plan_of("echo $((xs[i] + 1))").free_variables, vec!["i", "xs"]);
        assert_eq!(plan_of("echo ${xs[i]}").free_variables, vec!["xs"]);
    }

    #[test]
    fn last_exit_code_and_pid_are_not_session_variables() {
        assert!(plan_of("echo $(($? + $$))").free_variables.is_empty());
    }

    #[test]
    fn random_and_seconds_are_free_variables_like_any_other_name() {
        // Planning is static — it cannot know RANDOM/SECONDS will be
        // unset at eval time, and a plan consumer may set them, so they
        // are reported exactly like any other bare name.
        assert_eq!(plan_of("echo $((RANDOM % 10))").free_variables, vec!["RANDOM"]);
    }

    #[test]
    fn command_substitution_inside_arithmetic_contributes_its_own_reads() {
        assert_eq!(plan_of("echo $((1 + $(echo $y)))").free_variables, vec!["y"]);
    }

    #[test]
    fn a_bare_arith_condition_reads_like_any_other_arithmetic() {
        let plan = plan_of("while (( i <= n )); do :; done");
        assert_eq!(plan.free_variables, vec!["i", "n"]);
    }

    #[test]
    fn a_subscripted_assignment_binds_the_root_and_reads_the_subscript() {
        let plan = plan_of("counts[$key]=1");
        assert_eq!(plan.free_variables, vec!["key"]);
        assert_eq!(plan.bound_variables, vec!["counts"]);
    }

    #[test]
    fn an_env_prefix_binds_its_name_for_the_one_command() {
        let plan = plan_of("MODE=fast deploy ${TARGET}");
        assert_eq!(plan.free_variables, vec!["TARGET"]);
        assert_eq!(plan.bound_variables, vec!["MODE"]);
    }

    // ── plan_program: the program-level surface ──

    /// `index` is the position in the returned list, so indexing the list by it
    /// reads the statement it names.
    ///
    /// This used to preserve the gap left by a dropped `Stmt::Empty`, to line
    /// up with `Capture::Statement`'s index. That type was approval-ledger
    /// vocabulary; it was cut in 2481a3f3 and the ledger was deleted whole in
    /// 0c36dba1, before 0.14.0. The correspondence had no remaining consumer,
    /// and what it cost was an off-by-one in every script that opens with a
    /// comment.
    #[test]
    fn plan_program_indexes_are_dense_and_ordered() {
        let source = "echo one\n\n# a comment\necho two && echo three\nX=5";
        let program = parse(source).expect("the fixture parses");
        let expected: Vec<String> = program
            .statements
            .iter()
            .filter(|s| !matches!(s, Stmt::Empty))
            .map(|s| s.kind_name().to_string())
            .collect();
        let plans = plan_program(source).expect("the fixture parses");
        assert_eq!(
            plans.iter().map(|p| p.plan.statement_kind.clone()).collect::<Vec<_>>(),
            expected,
            "every non-empty statement is planned, in source order"
        );
        for (position, planned) in plans.iter().enumerate() {
            assert_eq!(
                planned.index, position,
                "index must be the position in the returned list"
            );
        }
    }

    /// The shape that made the old numbering wrong. A leading comment parses to
    /// a `Stmt::Empty` the plan drops, and numbering before the drop started
    /// every later statement one too high.
    #[test]
    fn a_leading_comment_does_not_shift_the_indexes() {
        let plans = plan_program("# lead\necho a\necho b").expect("parses");
        assert_eq!(plans.len(), 2);
        assert_eq!(plans[0].index, 0, "a leading comment must not shift index");
        assert_eq!(plans[1].index, 1);
    }

    #[test]
    fn plan_program_returns_the_parse_errors_for_a_broken_source() {
        let errors = plan_program("echo 'unclosed").expect_err("must not parse");
        assert!(!errors.is_empty());
    }
}
