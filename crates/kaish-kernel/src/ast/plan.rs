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
//! spellings the rendering redacts. One predicate decides all three of lift,
//! redact, and render, so they cannot disagree about what the statement
//! presented.
//!
//! [`plan_statement`] is **the one normalization point** the redaction seam
//! promises (spec §A.8): every argument and redirect target this module
//! plans passes through the same private normalization step exactly once,
//! here, before the `Plan` reaches any of its sinks — the statement
//! classifier, the ledger's `Observed` entry, tracing, and the
//! `/v/approvals` projection all read the same already-decided
//! `Plan::commands`, never the raw AST. The kernel's own confirm-key
//! redaction and an embedder-installed [`Redactor`] both land through this
//! one path, so a sink added later inherits both instead of needing its own
//! fix.
//!
//! **Not normalized here**: [`redact_keys`], which builds
//! `Capture::Statement`'s replay source. `Kernel::confirm` re-parses and
//! re-executes that source verbatim, so it must stay exactly what the user
//! typed — an embedder-redacted value baked into it would replay as the
//! literal marker text instead of the real argument. Only the kernel's own
//! confirm-key token is stripped there, because redemption authorizes
//! through the `ApproverHandle`, never the literal key, so replay needs
//! nothing else out of it.

use kaish_types::approval::{Plan, PlannedCommand, PlannedRedirect, PlannedValue, ValueSite, PLAN_RENDER_LIMIT};
use kaish_types::Value;
use kaish_tool_api::Redactor;

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

/// One statement of a planned program: its [`Plan`] and where it sits in the
/// parsed source.
///
/// `index` is the statement's position in the parsed program — the same
/// number a [`Capture::Statement`](kaish_types::approval::Capture) records
/// and a
/// [`ResumeAction::ConfirmStatement`](kaish_types::approval::ResumeAction)
/// quotes, so a plan built here correlates with any request the kernel later
/// raises for the same statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlannedStatement {
    /// The statement's position in the parsed program.
    pub index: usize,
    /// What the statement was asked to run, with every credential redacted.
    pub plan: Plan,
}

/// Plan every statement of `source` without executing anything.
///
/// A plan is parse information: `${HOME}` and `$(...)` appear exactly as
/// written, no substitution has run, and no filesystem has been touched. The
/// same walk feeds the kernel's own statement gate, so what an embedder reads
/// here is what a [`StatementClassifier`](crate::ledger::StatementClassifier)
/// would judge and what the ledger's `Observed` entries record — one
/// metadata surface, whether or not the kaish ledger is the one consuming it.
///
/// Every literal `--confirm=<key>` is redacted from the plans and **not
/// returned**: the caller holds `source` and can read its own credentials;
/// this function adds no second copy.
///
/// # Errors
///
/// Returns the parse errors when `source` does not parse. Each error's
/// [`format`](crate::parser::ParseError::format) renders a diagnostic against
/// the source.
pub fn plan_program(
    source: &str,
    redactor: Option<&dyn Redactor>,
) -> Result<Vec<PlannedStatement>, Vec<crate::parser::ParseError>> {
    let program = crate::parser::parse(source)?;
    Ok(program
        .statements
        .iter()
        .enumerate()
        // An empty statement runs nothing and plans nothing; skipping it here
        // is why `index` is carried explicitly instead of implied by position.
        .filter(|(_, stmt)| !matches!(stmt, Stmt::Empty))
        .map(|(index, stmt)| PlannedStatement {
            index,
            plan: plan_statement(stmt, redactor).plan,
        })
        .collect())
}

/// Build the plan for one top-level statement. `redactor` is the
/// embedder-installed [`Redactor`] (`KernelConfig::with_redactor`), when one
/// is registered — `None` leaves every non-key value [`PlannedValue::Plain`]
/// (spec §A.8's honest default).
pub fn plan_statement(stmt: &Stmt, redactor: Option<&dyn Redactor>) -> StatementPlan {
    let collected = collect(stmt, redactor);
    StatementPlan {
        plan: Plan::new(
            truncate_rendering(render_stmt(stmt, redactor)),
            stmt.kind_name(),
            collected.commands,
        ),
        presented_keys: collected.keys,
    }
}

/// The one normalization point every argument and redirect target passes
/// through (spec §A.8). `raw` is the value's fully rendered text — already
/// through `render_expr`, so it is exactly what would have reached the sink
/// unmarked before this seam existed.
fn normalize(raw: String, site: ValueSite, redactor: Option<&dyn Redactor>) -> PlannedValue {
    match redactor.and_then(|r| r.redact(&raw, site)) {
        Some(mark) => PlannedValue::redacted(mark.kind, mark.fingerprint),
        None => PlannedValue::Plain(raw),
    }
}

/// Remove every `--confirm=` (or `confirm=`) token from rendered plan text,
/// whatever it carries.
///
/// What a [`PlanBinding`](kaish_types::approval::PlanBinding) digests is the
/// operation that was *judged*, and the credential is not part of that — it
/// is the authorization for it (spec §A.9). Without this, the held statement
/// `rm x` and its re-run `rm --confirm=<confirm-key> x` would digest
/// differently, and every key presentation would be read as a moved binding
/// and re-asked, which is exactly the loop the binding exists to prevent.
///
/// Unlike [`redact_keys`], this does not need to know the key: it removes the
/// whole token whether it carries a literal credential, the `<confirm-key>`
/// marker a rendered plan shows, or an unexpanded `${key}` the plan could not
/// lift.
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
/// some request. Leaving `<confirm-key>` in the argv would do exactly that.
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
fn collect(stmt: &Stmt, redactor: Option<&dyn Redactor>) -> Collected {
    let mut out = Collected::default();
    collect_stmt(stmt, false, &mut out, redactor);
    out
}

/// Every command the statement contains, in source order.
pub fn planned_commands(stmt: &Stmt, redactor: Option<&dyn Redactor>) -> Vec<PlannedCommand> {
    collect(stmt, redactor).commands
}

fn collect_stmt(stmt: &Stmt, background: bool, out: &mut Collected, redactor: Option<&dyn Redactor>) {
    match stmt {
        Stmt::Assignment(a) => collect_expr(&a.value, background, out, redactor),
        Stmt::Command(cmd) => collect_command(cmd, background, out, redactor),
        Stmt::Pipeline(p) => {
            for cmd in &p.commands {
                collect_command(cmd, background || p.background, out, redactor);
            }
        }
        Stmt::If(s) => {
            collect_expr(&s.condition, background, out, redactor);
            collect_block(&s.then_branch, background, out, redactor);
            if let Some(else_branch) = &s.else_branch {
                collect_block(else_branch, background, out, redactor);
            }
        }
        Stmt::For(s) => {
            for item in &s.items {
                collect_expr(item, background, out, redactor);
            }
            collect_block(&s.body, background, out, redactor);
        }
        Stmt::While(s) => {
            collect_expr(&s.condition, background, out, redactor);
            collect_block(&s.body, background, out, redactor);
        }
        Stmt::Case(s) => {
            collect_expr(&s.expr, background, out, redactor);
            for branch in &s.branches {
                collect_block(&branch.body, background, out, redactor);
            }
        }
        Stmt::Return(e) | Stmt::Exit(e) => {
            if let Some(e) = e {
                collect_expr(e, background, out, redactor);
            }
        }
        Stmt::ToolDef(def) => collect_block(&def.body, background, out, redactor),
        Stmt::Test(t) => collect_test(t, background, out, redactor),
        Stmt::AndChain { left, right } | Stmt::OrChain { left, right } => {
            collect_stmt(left, background, out, redactor);
            collect_stmt(right, background, out, redactor);
        }
        Stmt::EnvScoped { assignments, body } => {
            for a in assignments {
                collect_expr(&a.value, background, out, redactor);
            }
            collect_stmt(body, background, out, redactor);
        }
        Stmt::Break(_) | Stmt::Continue(_) | Stmt::Empty => {}
    }
}

fn collect_block(stmts: &[Stmt], background: bool, out: &mut Collected, redactor: Option<&dyn Redactor>) {
    for stmt in stmts {
        collect_stmt(stmt, background, out, redactor);
    }
}

fn collect_command(cmd: &Command, background: bool, out: &mut Collected, redactor: Option<&dyn Redactor>) {
    let args: Vec<PlannedValue> = cmd.args.iter().map(|arg| plan_arg(arg, redactor).1).collect();
    let redirects = cmd
        .redirects
        .iter()
        .map(|r| PlannedRedirect::new(r.kind.to_string(), plan_redirect_target(&r.target, redactor)))
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
            Arg::Positional(e) => collect_expr(e, background, out, redactor),
            Arg::Named { value, .. } | Arg::WordAssign { value, .. } => {
                collect_expr(value, background, out, redactor)
            }
            Arg::ShortFlag(_) | Arg::LongFlag(_) | Arg::DoubleDash => {}
        }
    }
    for redirect in &cmd.redirects {
        collect_expr(&redirect.target, background, out, redactor);
    }
}

fn collect_expr(expr: &Expr, background: bool, out: &mut Collected, redactor: Option<&dyn Redactor>) {
    match expr {
        Expr::Command(cmd) => collect_command(cmd, background, out, redactor),
        Expr::CommandSubst(stmts) => collect_block(stmts, background, out, redactor),
        Expr::BinaryOp { left, right, .. } => {
            collect_expr(left, background, out, redactor);
            collect_expr(right, background, out, redactor);
        }
        Expr::Interpolated(parts) => collect_parts(parts, background, out, redactor),
        Expr::HereDocBody { parts, .. } => {
            for part in parts {
                collect_part(&part.part, background, out, redactor);
            }
        }
        Expr::Test(t) => collect_test(t, background, out, redactor),
        Expr::VarWithDefault { default, .. } => collect_parts(default, background, out, redactor),
        Expr::ListLiteral(elems) => {
            for elem in elems {
                match elem {
                    ListElem::Item(e) | ListElem::Spread(e) => collect_expr(e, background, out, redactor),
                }
            }
        }
        Expr::RecordLiteral(entries) => {
            for entry in entries {
                if let RecordKey::Interpolated(parts) = &entry.key {
                    collect_parts(parts, background, out, redactor);
                }
                collect_expr(&entry.value, background, out, redactor);
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

fn collect_parts(parts: &[StringPart], background: bool, out: &mut Collected, redactor: Option<&dyn Redactor>) {
    for part in parts {
        collect_part(part, background, out, redactor);
    }
}

fn collect_part(part: &StringPart, background: bool, out: &mut Collected, redactor: Option<&dyn Redactor>) {
    match part {
        StringPart::CommandSubst(stmts) => collect_block(stmts, background, out, redactor),
        StringPart::VarWithDefault { default, .. } => collect_parts(default, background, out, redactor),
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

fn collect_test(test: &TestExpr, background: bool, out: &mut Collected, redactor: Option<&dyn Redactor>) {
    match test {
        TestExpr::FileTest { path, .. } => collect_expr(path, background, out, redactor),
        TestExpr::StringTest { value, .. } => collect_expr(value, background, out, redactor),
        TestExpr::Comparison { left, right, .. }
        | TestExpr::In { left, right }
        | TestExpr::NotIn { left, right } => {
            collect_expr(left, background, out, redactor);
            collect_expr(right, background, out, redactor);
        }
        TestExpr::And { left, right } | TestExpr::Or { left, right } => {
            collect_test(left, background, out, redactor);
            collect_test(right, background, out, redactor);
        }
        TestExpr::Not { expr } => collect_test(expr, background, out, redactor),
    }
}

// ───────────────────────── Rendering ─────────────────────────

/// Render one statement back to shell text, unexpanded.
pub fn render_stmt(stmt: &Stmt, redactor: Option<&dyn Redactor>) -> String {
    match stmt {
        Stmt::Assignment(a) => render_assignment(a, redactor),
        Stmt::Command(cmd) => render_command(cmd, redactor),
        Stmt::Pipeline(p) => render_pipeline(p, redactor),
        Stmt::If(s) => render_if(s, redactor),
        Stmt::For(s) => render_for(s, redactor),
        Stmt::While(s) => render_while(s, redactor),
        Stmt::Case(s) => render_case(s, redactor),
        Stmt::Break(n) => render_keyword("break", n.map(|n| n.to_string())),
        Stmt::Continue(n) => render_keyword("continue", n.map(|n| n.to_string())),
        Stmt::Return(e) => render_keyword("return", e.as_ref().map(|e| render_expr(e, redactor))),
        Stmt::Exit(e) => render_keyword("exit", e.as_ref().map(|e| render_expr(e, redactor))),
        Stmt::ToolDef(def) => render_tooldef(def, redactor),
        Stmt::Test(t) => format!("[[ {} ]]", render_test(t, redactor)),
        Stmt::AndChain { left, right } => {
            format!("{} && {}", render_stmt(left, redactor), render_stmt(right, redactor))
        }
        Stmt::OrChain { left, right } => {
            format!("{} || {}", render_stmt(left, redactor), render_stmt(right, redactor))
        }
        Stmt::EnvScoped { assignments, body } => {
            let prefix: Vec<String> = assignments.iter().map(|a| render_assignment(a, redactor)).collect();
            format!("{} {}", prefix.join(" "), render_stmt(body, redactor))
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

fn render_block(stmts: &[Stmt], redactor: Option<&dyn Redactor>) -> String {
    stmts
        .iter()
        .filter(|s| !matches!(s, Stmt::Empty))
        .map(|s| render_stmt(s, redactor))
        .collect::<Vec<_>>()
        .join("; ")
}

fn render_assignment(a: &Assignment, redactor: Option<&dyn Redactor>) -> String {
    let path = render_varpath(&a.path);
    if a.local {
        format!("local {} = {}", path, render_expr(&a.value, redactor))
    } else {
        format!("{}={}", path, render_expr(&a.value, redactor))
    }
}

/// Render one command: argv0, every argument form, and every redirect.
pub fn render_command(cmd: &Command, redactor: Option<&dyn Redactor>) -> String {
    let mut parts = vec![cmd.name.clone()];
    for arg in &cmd.args {
        parts.push(plan_arg(arg, redactor).0);
    }
    for redirect in &cmd.redirects {
        parts.push(render_redirect(redirect, redactor));
    }
    parts.join(" ")
}

/// The one argument whose value never reaches a plan unredacted:
/// `--confirm=<token>` carries a redemption credential, and a plan is
/// written straight into the ledger and projected into `/v/approvals`
/// (spec §A.2 — no entry carries a credential). This is the kernel's own,
/// unconditional redaction (spec §A.8) — it never asks an installed
/// [`Redactor`], because the kernel minted this exact string and knows it
/// outright.
const CONFIRM_KEY: &str = "confirm";

/// The [`PlannedValue::Redacted`] `kind` the kernel's confirm-key redaction
/// marks a value with, so an auditor reading `Plan::commands` can tell the
/// kernel's own redaction apart from an embedder's.
const CONFIRM_KEY_KIND: &str = "confirm-key";

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

/// Plan one argument: its flat text (for [`render_command`]) and its
/// [`PlannedValue`] (for [`PlannedCommand::args`]), both derived from the
/// same one redaction decision (spec §A.8) — so the two representations
/// cannot disagree about what this argument was.
///
/// A `confirm` argument carrying a *literal* is the kernel's own credential
/// and is redacted unconditionally; every other value's flag/key prefix (if
/// any) stays visible even when its value is redacted, so the record still
/// shows *that* a value was presented at that flag, never *what*.
fn plan_arg(arg: &Arg, redactor: Option<&dyn Redactor>) -> (String, PlannedValue) {
    if presented_key(arg).is_some() {
        let value = PlannedValue::redacted(CONFIRM_KEY_KIND, None);
        let text = match arg {
            // `dd` takes its operands as bare `key=value`, so `confirm=<key>`
            // is a second spelling of the same credential.
            Arg::WordAssign { key, .. } => format!("{key}={}", value.display()),
            _ => format!("--{CONFIRM_KEY}={}", value.display()),
        };
        return (text, value);
    }
    let (text, value) = match arg {
        Arg::Positional(e) => {
            let value = normalize(render_expr(e, redactor), ValueSite::Argument, redactor);
            (value.display(), value)
        }
        Arg::Named { key, value: e } => {
            let value = normalize(render_expr(e, redactor), ValueSite::Argument, redactor);
            (format!("--{key}={}", value.display()), value)
        }
        Arg::WordAssign { key, value: e } => {
            let value = normalize(render_expr(e, redactor), ValueSite::Argument, redactor);
            (format!("{key}={}", value.display()), value)
        }
        Arg::ShortFlag(f) => {
            let text = format!("-{f}");
            (text.clone(), PlannedValue::Plain(text))
        }
        Arg::LongFlag(f) => {
            let text = format!("--{f}");
            (text.clone(), PlannedValue::Plain(text))
        }
        Arg::DoubleDash => ("--".to_string(), PlannedValue::Plain("--".to_string())),
    };
    // A redacted `--key=value` loses its `key=` prefix in the *structured*
    // value (`PlannedValue::Redacted` has nowhere to put one) but keeps it
    // in the flat text above; a plain value keeps the full composed text in
    // both, so `PlannedCommand::args` round-trips into `render_command`'s
    // output unless something was actually judged secret.
    let structured = if value.is_redacted() {
        value
    } else {
        PlannedValue::Plain(text.clone())
    };
    (text, structured)
}

/// Plan one redirect's target through the same normalization `plan_arg`
/// applies to arguments (spec §A.8's `ValueSite::RedirectTarget`).
fn plan_redirect_target(target: &Expr, redactor: Option<&dyn Redactor>) -> PlannedValue {
    normalize(render_expr(target, redactor), ValueSite::RedirectTarget, redactor)
}

fn render_redirect(redirect: &Redirect, redactor: Option<&dyn Redactor>) -> String {
    // A merge redirect (`2>&1`, `1>&2`) is the whole operator: its target
    // expression is a placeholder, and printing it would invent a filename.
    match redirect.kind {
        super::types::RedirectKind::MergeStderr | super::types::RedirectKind::MergeStdout => {
            redirect.kind.to_string()
        }
        _ => format!(
            "{} {}",
            redirect.kind,
            plan_redirect_target(&redirect.target, redactor).display()
        ),
    }
}

fn render_pipeline(p: &Pipeline, redactor: Option<&dyn Redactor>) -> String {
    let body = p
        .commands
        .iter()
        .map(|cmd| render_command(cmd, redactor))
        .collect::<Vec<_>>()
        .join(" | ");
    if p.background {
        format!("{body} &")
    } else {
        body
    }
}

fn render_if(s: &IfStmt, redactor: Option<&dyn Redactor>) -> String {
    let mut out = format!(
        "if {}; then {}",
        render_expr(&s.condition, redactor),
        render_block(&s.then_branch, redactor)
    );
    if let Some(else_branch) = &s.else_branch {
        let rendered = render_block(else_branch, redactor);
        if !rendered.is_empty() {
            out.push_str(&format!("; else {rendered}"));
        }
    }
    out.push_str("; fi");
    out
}

fn render_for(s: &ForLoop, redactor: Option<&dyn Redactor>) -> String {
    let items: Vec<String> = s.items.iter().map(|e| render_expr(e, redactor)).collect();
    format!(
        "for {} in {}; do {}; done",
        s.variable,
        items.join(" "),
        render_block(&s.body, redactor)
    )
}

fn render_while(s: &WhileLoop, redactor: Option<&dyn Redactor>) -> String {
    format!(
        "while {}; do {}; done",
        render_expr(&s.condition, redactor),
        render_block(&s.body, redactor)
    )
}

fn render_case(s: &CaseStmt, redactor: Option<&dyn Redactor>) -> String {
    let branches: Vec<String> = s
        .branches
        .iter()
        .map(|b| format!("{}) {} ;;", b.patterns.join("|"), render_block(&b.body, redactor)))
        .collect();
    format!("case {} in {} esac", render_expr(&s.expr, redactor), branches.join(" "))
}

fn render_tooldef(def: &ToolDef, redactor: Option<&dyn Redactor>) -> String {
    let params: Vec<String> = def
        .params
        .iter()
        .map(|p| match &p.default {
            Some(default) => format!("{}={}", p.name, render_expr(default, redactor)),
            None => p.name.clone(),
        })
        .collect();
    format!(
        "tool {}({}) {{ {} }}",
        def.name,
        params.join(", "),
        render_block(&def.body, redactor)
    )
}

/// Render one expression back to shell text, unexpanded: a variable
/// reference stays `${NAME}` and a substitution stays `$(…)`.
pub fn render_expr(expr: &Expr, redactor: Option<&dyn Redactor>) -> String {
    match expr {
        Expr::Literal(v) => render_literal(v),
        Expr::VarRef(path) => format!("${{{}}}", render_varpath(path)),
        Expr::Interpolated(parts) => format!("\"{}\"", render_parts(parts, redactor)),
        Expr::HereDocBody { parts, strip_tabs } => {
            let dash = if *strip_tabs { "-" } else { "" };
            let body: Vec<String> = parts.iter().map(|sp| render_part(&sp.part, redactor)).collect();
            format!("<<{dash}EOF\n{}\nEOF", body.join(""))
        }
        Expr::BinaryOp { left, op, right } => {
            let op = match op {
                BinaryOp::And => "&&",
                BinaryOp::Or => "||",
            };
            format!("{} {} {}", render_expr(left, redactor), op, render_expr(right, redactor))
        }
        Expr::CommandSubst(stmts) => format!("$({})", render_block(stmts, redactor)),
        Expr::Test(t) => format!("[[ {} ]]", render_test(t, redactor)),
        Expr::Positional(n) => format!("${n}"),
        Expr::AllArgs => "$@".to_string(),
        Expr::ArgCount => "$#".to_string(),
        Expr::VarLength(path) => format!("${{#{}}}", render_varpath(path)),
        Expr::VarWithDefault { path, default } => {
            format!("${{{}:-{}}}", render_varpath(path), render_parts(default, redactor))
        }
        Expr::Arithmetic(e) => format!("$(({e}))"),
        Expr::Command(cmd) => render_command(cmd, redactor),
        Expr::LastExitCode => "$?".to_string(),
        Expr::CurrentPid => "$$".to_string(),
        Expr::GlobPattern(p) => p.clone(),
        Expr::ListLiteral(elems) => {
            let parts: Vec<String> = elems
                .iter()
                .map(|e| match e {
                    ListElem::Item(e) => render_expr(e, redactor),
                    ListElem::Spread(e) => format!("...{}", render_expr(e, redactor)),
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
                        RecordKey::Interpolated(parts) => format!("\"{}\"", render_parts(parts, redactor)),
                    };
                    format!("{key}: {}", render_expr(&entry.value, redactor))
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

fn render_parts(parts: &[StringPart], redactor: Option<&dyn Redactor>) -> String {
    parts.iter().map(|p| render_part(p, redactor)).collect::<Vec<_>>().join("")
}

fn render_part(part: &StringPart, redactor: Option<&dyn Redactor>) -> String {
    match part {
        StringPart::Literal(s) => s.replace('\\', "\\\\").replace('"', "\\\""),
        StringPart::Var(path) => format!("${{{}}}", render_varpath(path)),
        StringPart::VarWithDefault { path, default } => {
            format!("${{{}:-{}}}", render_varpath(path), render_parts(default, redactor))
        }
        StringPart::VarLength(path) => format!("${{#{}}}", render_varpath(path)),
        StringPart::Positional(n) => format!("${n}"),
        StringPart::AllArgs => "$@".to_string(),
        StringPart::ArgCount => "$#".to_string(),
        StringPart::Arithmetic(e) => format!("$(({e}))"),
        StringPart::CommandSubst(stmts) => format!("$({})", render_block(stmts, redactor)),
        StringPart::LastExitCode => "$?".to_string(),
        StringPart::CurrentPid => "$$".to_string(),
    }
}

fn render_test(test: &TestExpr, redactor: Option<&dyn Redactor>) -> String {
    match test {
        TestExpr::FileTest { op, path } => format!("{} {}", op, render_expr(path, redactor)),
        TestExpr::StringTest { op, value } => format!("{} {}", op, render_expr(value, redactor)),
        TestExpr::Comparison { left, op, right } => {
            format!("{} {} {}", render_expr(left, redactor), op, render_expr(right, redactor))
        }
        TestExpr::And { left, right } => {
            format!("{} && {}", render_test(left, redactor), render_test(right, redactor))
        }
        TestExpr::Or { left, right } => {
            format!("{} || {}", render_test(left, redactor), render_test(right, redactor))
        }
        TestExpr::Not { expr } => format!("! {}", render_test(expr, redactor)),
        TestExpr::In { left, right } => {
            format!("{} in {}", render_expr(left, redactor), render_expr(right, redactor))
        }
        TestExpr::NotIn { left, right } => {
            format!("{} not in {}", render_expr(left, redactor), render_expr(right, redactor))
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
        plan_statement(&stmt, None)
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

    // ── The presented credential (spec §A.2, §C.6) ──

    #[test]
    fn a_literal_key_is_lifted_and_redacted() {
        let planned = planned_of("rm --confirm=deadbeef target.txt");
        assert_eq!(planned.presented_keys, vec!["deadbeef".to_string()]);
        assert_eq!(planned.plan.rendered, "rm --confirm=<confirm-key> target.txt");
        assert_eq!(
            planned.plan.commands[0].args,
            vec![
                PlannedValue::redacted("confirm-key", None),
                PlannedValue::Plain("target.txt".to_string()),
            ]
        );
    }

    #[test]
    fn the_bare_word_assign_spelling_is_lifted_too() {
        // `dd` takes its operands as `key=value`, so this is the same
        // credential wearing the other spelling.
        let planned = planned_of("dd if=a of=b confirm=deadbeef");
        assert_eq!(planned.presented_keys, vec!["deadbeef".to_string()]);
        assert!(
            planned.plan.rendered.ends_with("confirm=<confirm-key>"),
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

    // ── plan_program: the program-level surface ──

    #[test]
    fn plan_program_indexes_agree_with_the_parsed_program() {
        let source = "echo one\n\n# a comment\necho two && echo three\nX=5";
        let program = parse(source).expect("the fixture parses");
        let expected: Vec<(usize, String)> = program
            .statements
            .iter()
            .enumerate()
            .filter(|(_, s)| !matches!(s, Stmt::Empty))
            .map(|(i, s)| (i, s.kind_name().to_string()))
            .collect();
        let plans = plan_program(source, None).expect("the fixture parses");
        assert_eq!(
            plans
                .iter()
                .map(|p| (p.index, p.plan.statement_kind.clone()))
                .collect::<Vec<_>>(),
            expected,
            "an index here must be usable against Capture::Statement's index"
        );
    }

    #[test]
    fn plan_program_redacts_a_presented_key_and_returns_no_copy_of_it() {
        // `PlannedStatement` has no key field by design — the caller holds
        // the source. What must hold is that the plan itself is redacted.
        let plans = plan_program("rm --confirm=deadbeef x.txt", None).expect("parses");
        assert_eq!(plans[0].plan.rendered, "rm --confirm=<confirm-key> x.txt");
    }

    #[test]
    fn plan_program_returns_the_parse_errors_for_a_broken_source() {
        let errors = plan_program("echo 'unclosed", None).expect_err("must not parse");
        assert!(!errors.is_empty());
    }

    #[test]
    fn redaction_covers_both_spellings_across_a_multi_statement_source() {
        let source = "echo one\ndd if=a confirm=deadbeef\nrm --confirm=deadbeef x";
        let redacted = redact_keys(source, &["deadbeef".to_string()]);
        assert!(!redacted.contains("deadbeef"), "got: {redacted}");
        assert_eq!(redacted, "echo one\ndd if=a\nrm x");
    }
}
