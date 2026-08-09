//! The statement plan: an AST rendered back to shell text, **unexpanded**
//! (`docs/approval-ledger.md` §C.6).
//!
//! A plan is parse information. It is built after validation and before
//! execution, so `${HOME}` and `$(...)` appear exactly as written — a
//! classifier judges what was asked, not what it resolved to, and the
//! substitution that would resolve them has not run and must not run before
//! a gate decides.
//!
//! Two products, one AST walk each: [`render_stmt`] produces the text, and
//! [`planned_commands`] produces one [`PlannedCommand`] per command the
//! statement contains — control-structure bodies, `if` conditions, and
//! command substitutions included, because every one of them is a command
//! this statement would run.
//!
//! The collection walk also lifts out any literal `--confirm=<key>` the
//! statement's argv carries ([`StatementPlan::presented_keys`]) — the same
//! two spellings the rendering redacts. One predicate decides all three of
//! lift, redact, and render, so they cannot disagree about what the statement
//! presented.

use kaish_types::approval::{Plan, PlannedCommand, PlannedRedirect, PLAN_RENDER_LIMIT};
use kaish_types::Value;

use super::types::{
    Arg, Assignment, BinaryOp, CaseStmt, Command, Expr, ForLoop, IfStmt, ListElem, Pipeline,
    RecordKey, Redirect, Stmt, StringPart, TestExpr, ToolDef, VarPath, VarSegment, WhileLoop,
};

/// One statement's plan, plus the redemption credentials its argv presented.
pub struct StatementPlan {
    /// What the statement was asked to run, with every credential redacted.
    pub plan: Plan,
    /// Every literal `--confirm=<key>` (or `confirm=<key>`) the statement's
    /// argv carries, in source order.
    ///
    /// **Literal only.** A plan is unexpanded, so `--confirm=${key}` reads as
    /// `${key}` here and nothing is lifted — which is exactly right both
    /// ways: what the record cannot see, the record cannot leak, and what
    /// the gate cannot see, the builtin's own gate still gets, because
    /// nothing is stripped from the argv that executes.
    pub presented_keys: Vec<String>,
}

/// Build the plan for one top-level statement.
pub fn plan_statement(stmt: &Stmt) -> StatementPlan {
    let collected = collect(stmt);
    StatementPlan {
        plan: Plan::new(
            truncate_rendering(render_stmt(stmt)),
            stmt.kind_name(),
            collected.commands,
        ),
        presented_keys: collected.keys,
    }
}

/// Remove every `--confirm=` (or `confirm=`) token from rendered plan text,
/// whatever it carries.
///
/// What a [`PlanBinding`](kaish_types::approval::PlanBinding) digests is the
/// operation that was *judged*, and the credential is not part of that — it
/// is the authorization for it (spec §A.9). Without this, the held statement
/// `rm x` and its re-run `rm --confirm=<redacted> x` would digest
/// differently, and every key presentation would be read as a moved binding
/// and re-asked, which is exactly the loop the binding exists to prevent.
///
/// Unlike [`redact_keys`], this does not need to know the key: it removes the
/// whole token whether it carries a literal credential, the `<redacted>`
/// placeholder a rendered plan shows, or an unexpanded `${key}` the plan
/// could not lift.
pub fn strip_confirm_tokens(rendered: &str) -> String {
    rendered
        .split_whitespace()
        .filter(|word| {
            !word.starts_with(&format!("--{CONFIRM_KEY}=")) && !word.starts_with(&format!("{CONFIRM_KEY}="))
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Remove every one of `keys` from captured source text.
///
/// The capture is what `Kernel::confirm` replays, and it lands in the ledger
/// on the way there — where no entry may carry a credential (spec §A.2). The
/// whole `--confirm=<key>` token goes, not just its value: a replay runs
/// under a redemption correlation and is authorized by that, so a replayed
/// statement re-presenting a spent key would only count a rejection against
/// some request. Leaving `<redacted>` in the argv would do exactly that.
pub fn redact_keys(source: &str, keys: &[String]) -> String {
    let mut out = source.to_string();
    for key in keys {
        for spelling in [format!("--{CONFIRM_KEY}={key}"), format!("{CONFIRM_KEY}={key}")] {
            // Take the separating space with the token so the surrounding
            // words stay one space apart; fall back to the bare token for a
            // spelling that opens its line.
            out = out.replace(&format!(" {spelling}"), "");
            out = out.replace(&spelling, "");
        }
    }
    out
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

/// What one collection walk produces: the statement's commands, and the
/// credentials their argv presented.
#[derive(Default)]
struct Collected {
    commands: Vec<PlannedCommand>,
    keys: Vec<String>,
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

/// Every command the statement contains, in source order.
pub fn planned_commands(stmt: &Stmt) -> Vec<PlannedCommand> {
    collect(stmt).commands
}

fn collect_stmt(stmt: &Stmt, background: bool, out: &mut Collected) {
    match stmt {
        Stmt::Assignment(a) => collect_expr(&a.value, background, out),
        Stmt::Command(cmd) => collect_command(cmd, background, out),
        Stmt::Pipeline(p) => {
            for cmd in &p.commands {
                collect_command(cmd, background || p.background, out);
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
        Stmt::ToolDef(def) => collect_block(&def.body, background, out),
        Stmt::Test(t) => collect_test(t, background, out),
        Stmt::AndChain { left, right } | Stmt::OrChain { left, right } => {
            collect_stmt(left, background, out);
            collect_stmt(right, background, out);
        }
        Stmt::EnvScoped { assignments, body } => {
            for a in assignments {
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
    let mut args = Vec::new();
    for arg in &cmd.args {
        args.push(render_arg(arg));
    }
    let redirects = cmd
        .redirects
        .iter()
        .map(|r| PlannedRedirect::new(r.kind.to_string(), render_expr(&r.target)))
        .collect();
    out.commands.push(PlannedCommand::new(
        cmd.name.clone(),
        args,
        redirects,
        background,
    ));
    // Lift any literal credential out of the argv on the same pass that
    // redacts it from the rendering — one walk, one truth about what this
    // statement presented.
    for arg in &cmd.args {
        if let Some(key) = presented_key(arg) {
            out.keys.push(key);
        }
    }
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
        Expr::VarWithDefault { default, .. } => collect_parts(default, background, out),
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
        Expr::Literal(_)
        | Expr::VarRef(_)
        | Expr::Positional(_)
        | Expr::AllArgs
        | Expr::ArgCount
        | Expr::VarLength(_)
        | Expr::Arithmetic(_)
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
        StringPart::VarWithDefault { default, .. } => collect_parts(default, background, out),
        StringPart::Literal(_)
        | StringPart::Var(_)
        | StringPart::VarLength(_)
        | StringPart::Positional(_)
        | StringPart::AllArgs
        | StringPart::ArgCount
        | StringPart::Arithmetic(_)
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
pub fn render_stmt(stmt: &Stmt) -> String {
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
pub fn render_command(cmd: &Command) -> String {
    let mut parts = vec![cmd.name.clone()];
    for arg in &cmd.args {
        parts.push(render_arg(arg));
    }
    for redirect in &cmd.redirects {
        parts.push(render_redirect(redirect));
    }
    parts.join(" ")
}

/// The one argument whose value never reaches a plan: `--confirm=<token>`
/// carries a redemption credential, and a plan is written straight into the
/// ledger and projected into `/v/approvals` (spec §A.2 — no entry carries a
/// credential).
const CONFIRM_KEY: &str = "confirm";

/// What a redacted credential renders as. Names the flag so the record still
/// shows that a key was presented; shows none of it.
const REDACTED: &str = "<redacted>";

/// The literal credential this argument presents, if it is a `confirm`
/// argument carrying one.
///
/// A non-literal value (`--confirm=${key}`, `--confirm=$(cat key)`) yields
/// `None`: the plan is unexpanded, so the value is not knowable here. That
/// costs nothing — the statement gate does not see the key, the record does
/// not carry it either, and the argv that executes is untouched, so the
/// builtin's own gate still receives whatever it resolves to.
fn presented_key(arg: &Arg) -> Option<String> {
    let value = match arg {
        Arg::Named { key, value } | Arg::WordAssign { key, value } if key == CONFIRM_KEY => value,
        _ => return None,
    };
    match value {
        Expr::Literal(Value::String(s)) => Some(s.clone()),
        _ => None,
    }
}

fn render_arg(arg: &Arg) -> String {
    // A `confirm` argument carrying a *literal* is a credential and is
    // redacted; one carrying `${key}` or `$(…)` is not, and renders as
    // written like every other unexpanded value. One predicate decides all
    // three of lift, redact, and render, so they cannot disagree.
    if presented_key(arg).is_some() {
        return match arg {
            // `dd` takes its operands as bare `key=value`, so `confirm=<key>`
            // is a second spelling of the same credential.
            Arg::WordAssign { key, .. } => format!("{key}={REDACTED}"),
            _ => format!("--{CONFIRM_KEY}={REDACTED}"),
        };
    }
    match arg {
        Arg::Positional(e) => render_expr(e),
        Arg::Named { key, value } => format!("--{}={}", key, render_expr(value)),
        Arg::WordAssign { key, value } => format!("{}={}", key, render_expr(value)),
        Arg::ShortFlag(f) => format!("-{f}"),
        Arg::LongFlag(f) => format!("--{f}"),
        Arg::DoubleDash => "--".to_string(),
    }
}

fn render_redirect(redirect: &Redirect) -> String {
    // A merge redirect (`2>&1`, `1>&2`) is the whole operator: its target
    // expression is a placeholder, and printing it would invent a filename.
    match redirect.kind {
        super::types::RedirectKind::MergeStderr | super::types::RedirectKind::MergeStdout => {
            redirect.kind.to_string()
        }
        _ => format!("{} {}", redirect.kind, render_expr(&redirect.target)),
    }
}

fn render_pipeline(p: &Pipeline) -> String {
    let body = p
        .commands
        .iter()
        .map(render_command)
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
pub fn render_expr(expr: &Expr) -> String {
    match expr {
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
        // would put unreadable data in an audit record.
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
        TestExpr::Or { left, right } => format!("{} || {}", render_test(left), render_test(right)),
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
        assert_eq!(plan.commands[0].redirects[0].target, "out.txt");
        assert!(plan.rendered.contains("> out.txt"), "got: {}", plan.rendered);
    }

    #[test]
    fn a_redirect_target_stays_unexpanded() {
        let plan = plan_of("echo hi > ${LOG}");
        assert_eq!(plan.commands[0].redirects[0].target, "${LOG}");
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
                "-v".to_string(),
                "--force".to_string(),
                "--key=value".to_string(),
                "word".to_string(),
                "--".to_string(),
                "--after".to_string(),
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

    // ── The presented credential (spec §A.2, §C.6) ──

    #[test]
    fn a_literal_key_is_lifted_and_redacted() {
        let planned = planned_of("rm --confirm=deadbeef target.txt");
        assert_eq!(planned.presented_keys, vec!["deadbeef".to_string()]);
        assert_eq!(planned.plan.rendered, "rm --confirm=<redacted> target.txt");
        assert_eq!(
            planned.plan.commands[0].args,
            vec!["--confirm=<redacted>".to_string(), "target.txt".to_string()]
        );
    }

    #[test]
    fn the_bare_word_assign_spelling_is_lifted_too() {
        // `dd` takes its operands as `key=value`, so this is the same
        // credential wearing the other spelling.
        let planned = planned_of("dd if=a of=b confirm=deadbeef");
        assert_eq!(planned.presented_keys, vec!["deadbeef".to_string()]);
        assert!(
            planned.plan.rendered.ends_with("confirm=<redacted>"),
            "got: {}",
            planned.plan.rendered
        );
    }

    #[test]
    fn a_variable_carried_key_is_neither_lifted_nor_redacted() {
        // Nothing to lift and nothing to leak: an unexpanded plan never held
        // the value, so it renders as written like any other variable.
        let planned = planned_of("rm --confirm=${key} target.txt");
        assert!(planned.presented_keys.is_empty());
        assert_eq!(planned.plan.rendered, "rm --confirm=${key} target.txt");
    }

    #[test]
    fn a_key_inside_a_loop_body_is_still_lifted() {
        let planned = planned_of("for f in a b; do rm --confirm=deadbeef $f; done");
        assert_eq!(planned.presented_keys, vec!["deadbeef".to_string()]);
    }

    #[test]
    fn redaction_takes_the_whole_token_and_leaves_one_space() {
        let source = "rm --confirm=deadbeef target.txt";
        assert_eq!(
            redact_keys(source, &["deadbeef".to_string()]),
            "rm target.txt"
        );
    }

    #[test]
    fn redaction_leaves_a_source_that_never_presented_a_key_alone() {
        let source = "rm target.txt";
        assert_eq!(redact_keys(source, &[]), source);
        assert_eq!(redact_keys(source, &["deadbeef".to_string()]), source);
    }

    #[test]
    fn redaction_covers_both_spellings_across_a_multi_statement_source() {
        let source = "echo one\ndd if=a confirm=deadbeef\nrm --confirm=deadbeef x";
        let redacted = redact_keys(source, &["deadbeef".to_string()]);
        assert!(!redacted.contains("deadbeef"), "got: {redacted}");
        assert_eq!(redacted, "echo one\ndd if=a\nrm x");
    }
}
