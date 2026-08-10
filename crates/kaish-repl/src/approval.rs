//! The approval prompt — how the REPL fulfils its own gates.
//!
//! The REPL is a plain embedder: it holds an
//! [`ApproverHandle`](kaish_kernel::ledger::ApproverHandle), gets a pending
//! request back from `execute`, and decides in its own read loop
//! (`docs/approval-ledger.md` §C.3). Nothing here is a kernel hook — the
//! prompt happens above the call, after `execute` has already returned, so
//! nothing in the kernel is holding a statement open while a human thinks.
//!
//! Two boundaries this module keeps:
//!
//! - **The prompt is not result output.** The request renders to stderr and
//!   the question is the line editor's prompt; the frontend only asks when
//!   stdin and stdout are both terminals, so an approval prompt can never
//!   land in piped or captured output.
//! - **Silence is denial.** Anything but `y` or `a` — `n`, an empty line,
//!   Ctrl-C, Ctrl-D, a closed terminal — denies. A request is never granted
//!   because an answer was ambiguous.

use std::time::{Duration, SystemTime};

use kaish_types::approval::{
    ApprovalRequestView, OperationPattern, Principal, ResourcePattern, RiskClass, StandingGrant,
    StateClaim, Token, Transition,
};

/// How long a grant made from the prompt stays redeemable: 5 minutes. The
/// REPL redeems it immediately, so this bounds a grant whose replay failed,
/// not the human's thinking time. Every grant is good for exactly one
/// successful settlement whatever the window says (spec §A.1).
pub const GRANT_WINDOW: Duration = Duration::from_secs(300);

/// The channel a REPL grant records itself under, so the log tells a
/// terminal decision from an embedder's out-of-band UI.
pub const GRANT_CHANNEL: &str = "repl-terminal";

/// What the human answered.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Answer {
    /// `y` — grant this request, once.
    Once,
    /// `a` — grant this request and stop asking for this operation on these
    /// resources for the rest of the session.
    Session,
    /// `n`, an empty line, Ctrl-C, Ctrl-D, or anything unrecognized.
    Deny,
}

/// Where the REPL asks a human to decide.
///
/// [`ask`](Self::ask) returns `None` when this frontend has no terminal to
/// ask on — a piped session, `kaish -c`, a script. The caller then leaves the
/// exit-2 result exactly as the kernel produced it, which is the documented
/// non-interactive contract (spec §C.3): no prompt is ever written to a
/// non-terminal.
pub trait ApprovalPrompt {
    /// Show `request` and read one answer. `None` means "cannot ask here".
    fn ask(&mut self, request: &str) -> Option<Answer>;
}

/// The prompt for a frontend with no terminal. Never asks, never writes.
pub struct NoPrompt;

impl ApprovalPrompt for NoPrompt {
    fn ask(&mut self, _request: &str) -> Option<Answer> {
        None
    }
}

/// The prompt a human answers: the request on stderr, the question on the
/// REPL's own line editor.
///
/// **It asks only when stdin and stdout are both terminals**, and that check
/// lives here rather than at the construction site so the rule sits beside
/// the write it governs: a piped or captured session gets no prompt, gets
/// the exit-2 result the kernel produced, and never has an approval question
/// blended into its output (spec §C.3).
///
/// Reading through the line editor is what makes Ctrl-C ordinary input:
/// rustyline holds the terminal, so `^C` at the prompt returns
/// [`ReadlineError::Interrupted`](rustyline::error::ReadlineError) and denies
/// — no signal racing a decision, no request left live.
pub struct TerminalPrompt<'a, H: rustyline::Helper> {
    editor: &'a mut rustyline::Editor<H, rustyline::history::DefaultHistory>,
}

impl<'a, H: rustyline::Helper> TerminalPrompt<'a, H> {
    /// Ask through this line editor.
    pub fn new(editor: &'a mut rustyline::Editor<H, rustyline::history::DefaultHistory>) -> Self {
        Self { editor }
    }
}

impl<H: rustyline::Helper> ApprovalPrompt for TerminalPrompt<'_, H> {
    fn ask(&mut self, request: &str) -> Option<Answer> {
        use std::io::IsTerminal;
        if !std::io::stdin().is_terminal() || !std::io::stdout().is_terminal() {
            return None;
        }
        // stderr, not stdout: the request is a diagnostic about what did not
        // happen, and `kaish > out` must not swallow it or fold it into the
        // data a caller collected.
        eprint!("{request}");
        match self.editor.readline(QUESTION) {
            Ok(line) => Some(parse_answer(&line)),
            // Every read that is not an answer denies. A request left live
            // because the terminal went away is a request nobody will ever
            // close.
            Err(rustyline::error::ReadlineError::Interrupted) => {
                eprintln!("^C");
                Some(Answer::Deny)
            }
            Err(rustyline::error::ReadlineError::Eof) => {
                eprintln!("^D");
                Some(Answer::Deny)
            }
            Err(e) => {
                eprintln!("kaish: could not read an answer ({e}) — denying");
                Some(Answer::Deny)
            }
        }
    }
}

/// The question the prompt puts after the rendered request. Spelled out
/// rather than `[y/N]` alone, because `a` is a standing grant and a reader
/// should not have to guess how wide it is.
pub const QUESTION: &str = "grant? y = once, a = this operation on these resources all session, \
                            anything else denies [y/a/N] ";

/// Read an answer. Anything that is not `y`/`yes` or `a`/`always` denies —
/// including an empty line, so leaning on Enter is a denial and never a
/// grant.
pub fn parse_answer(input: &str) -> Answer {
    match input.trim().to_ascii_lowercase().as_str() {
        "y" | "yes" => Answer::Once,
        "a" | "always" => Answer::Session,
        _ => Answer::Deny,
    }
}

/// The re-run line for a held request: the request's `hint` with its
/// `<token>` placeholder replaced by the real credential.
///
/// **`token` is `Some` only in a session holding an
/// [`ApproverHandle`](kaish_kernel::ledger::ApproverHandle)** — retrieval is
/// authority's one privilege (spec §D.3), and a session without it has no
/// method that returns a credential, so the placeholder stays exactly as the
/// producer wrote it. A hint carrying no placeholder (the statement gate's
/// hint is the rendered line) is returned unchanged: this substitutes a
/// credential, it does not invent a re-run.
pub fn rerun_line(hint: &str, token: Option<&Token>) -> String {
    match token {
        Some(token) => hint.replace(TOKEN_PLACEHOLDER, token.reveal()),
        None => hint.to_string(),
    }
}

/// The placeholder every gate site writes into its hint (`rm
/// --confirm=<token> notes.txt`).
const TOKEN_PLACEHOLDER: &str = "<token>";

/// Render a pending request for a human at a terminal (spec §C.3):
/// operation, risk, principal, reason, every resource with its transition,
/// and the re-run line last and labelled, because a hint is producer-authored
/// text and not something the kernel vouches for.
pub fn render_request(view: &ApprovalRequestView, token: Option<&Token>) -> String {
    let mut text = format!("\napproval required — {}\n", view.id);
    text.push_str(&format!("  operation  {}\n", view.operation));
    text.push_str(&format!("  risk       {}\n", risk_word(view.risk)));
    text.push_str(&format!("  principal  {}\n", view.principal.id));
    if !view.reason.is_empty() {
        text.push_str(&format!("  reason     {}\n", view.reason));
    }
    for resource in &view.resources {
        text.push_str(&format!(
            "  resource   {}:{}{}\n",
            resource.kind,
            resource.id,
            render_transition(resource.transition.as_ref())
        ));
    }
    if let Some(job_id) = view.job_id {
        text.push_str(&format!("  job        /v/jobs/{job_id}/\n"));
    }
    if !view.hint.is_empty() {
        text.push_str(&format!(
            "  re-run     {}   (display only)\n",
            rerun_line(&view.hint, token)
        ));
    }
    text
}

/// `: a1b2c3d → c3d4e5f` for a resource that declares a transition, and
/// nothing at all for one that does not. An unconditioned resource must not
/// read as a claim about state.
fn render_transition(transition: Option<&Transition>) -> String {
    let Some(transition) = transition else {
        return String::new();
    };
    match (claim_word(&transition.from), claim_word(&transition.to)) {
        (None, None) => String::new(),
        (from, to) => format!(
            ": {} → {}",
            from.unwrap_or_else(|| "?".to_string()),
            to.unwrap_or_else(|| "?".to_string())
        ),
    }
}

/// A state claim as a human reads it. `None` for `Unspecified` — it claims
/// nothing, so it prints as nothing rather than as a value.
fn claim_word(claim: &StateClaim) -> Option<String> {
    match claim {
        StateClaim::Absent => Some("absent".to_string()),
        StateClaim::Exact(id) => Some(id.clone()),
        StateClaim::Digest { alg, hex } => Some(format!("{alg}:{}", &hex[..hex.len().min(12)])),
        StateClaim::Unspecified => None,
        // `StateClaim` is `#[non_exhaustive]`: a variant this build does not
        // know still prints as unknown rather than as nothing, because
        // "nothing" is what an unconditioned resource looks like.
        _ => Some("(unrecognized claim)".to_string()),
    }
}

/// The wire spelling of a risk class, so one risk has one name across the
/// prompt, `approvals show`, and `--json`.
fn risk_word(risk: RiskClass) -> &'static str {
    match risk {
        RiskClass::Reversible => "reversible",
        RiskClass::Recoverable => "recoverable",
        RiskClass::Irreversible => "irreversible",
        _ => "unrecognized",
    }
}

/// The standing grant an `a` answer issues (spec §C.3): this operation, these
/// resources, for the rest of the session.
///
/// **Every resource id is escaped, so the rule matches the resources it was
/// issued for and no others.** A standing grant's id is a glob
/// (`kaish-glob`), so a file named `report-*.txt` would otherwise widen the
/// rule to every `report-…` path the session ever names — "always, for this"
/// silently becoming "always, for anything shaped like this".
pub fn session_standing_grant(view: &ApprovalRequestView, issued_by: Principal) -> StandingGrant {
    let operations = vec![OperationPattern::new(view.operation.as_str())];
    let resources = view
        .resources
        .iter()
        .map(|resource| ResourcePattern::new(&resource.kind, escape_glob(&resource.id)))
        .collect();
    StandingGrant::new(
        operations,
        resources,
        None,
        None,
        issued_by,
        format!("approved for the session at the terminal ({})", view.operation),
    )
    .unlimited_uses()
}

/// Backslash-escape every `kaish-glob` metacharacter, so a literal string
/// matches only itself.
fn escape_glob(id: &str) -> String {
    let mut escaped = String::with_capacity(id.len());
    for ch in id.chars() {
        if matches!(ch, '*' | '?' | '[' | ']' | '{' | '}' | '\\') {
            escaped.push('\\');
        }
        escaped.push(ch);
    }
    escaped
}

/// When a grant made now stops being redeemable.
pub fn grant_deadline() -> SystemTime {
    kaish_types::clock::system_now() + GRANT_WINDOW
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;
    use kaish_types::approval::{PrincipalKind, Resource};

    #[test]
    fn every_answer_but_yes_and_always_denies() {
        assert_eq!(parse_answer("y"), Answer::Once);
        assert_eq!(parse_answer("Y\n"), Answer::Once);
        assert_eq!(parse_answer("yes"), Answer::Once);
        assert_eq!(parse_answer("a"), Answer::Session);
        assert_eq!(parse_answer("ALWAYS"), Answer::Session);
        for denial in ["n", "no", "", "  ", "maybe", "yeah", "1"] {
            assert_eq!(parse_answer(denial), Answer::Deny, "{denial:?} must deny");
        }
    }

    #[test]
    fn the_rerun_line_carries_the_token_only_when_one_was_retrieved() {
        let hint = "rm --confirm=<token> notes.txt";
        // No authority: the placeholder is what a session without a handle
        // can render, and it must stay a placeholder.
        assert_eq!(rerun_line(hint, None), hint);
        let token = Token::new("k3f9c1a4");
        assert_eq!(
            rerun_line(hint, Some(&token)),
            "rm --confirm=k3f9c1a4 notes.txt"
        );
    }

    #[test]
    fn a_hint_with_no_placeholder_is_left_alone() {
        // The statement gate's hint is the rendered line, not a re-run
        // template — substituting into it would invent a command.
        let token = Token::new("k3f9c1a4");
        assert_eq!(rerun_line("rm notes.txt", Some(&token)), "rm notes.txt");
    }

    #[test]
    fn a_session_standing_grant_escapes_the_resources_it_names() {
        let view = view_with(vec![Resource::plain("path", "/w/report-*.txt")]);
        let rule = session_standing_grant(&view, Principal::new("amy", PrincipalKind::Human));
        assert_eq!(rule.resources.len(), 1);
        assert_eq!(rule.resources[0].kind, "path");
        assert_eq!(rule.resources[0].pattern, r"/w/report-\*.txt");
        assert!(
            kaish_kernel::glob::glob_match(&rule.resources[0].pattern, "/w/report-*.txt"),
            "the rule must still match the resource it was issued for"
        );
        assert!(
            !kaish_kernel::glob::glob_match(&rule.resources[0].pattern, "/w/report-payroll.txt"),
            "an `a` answer must not widen to every path shaped like this one"
        );
    }

    #[test]
    fn a_session_standing_grant_covers_this_operation_and_repeats() {
        let view = view_with(vec![Resource::plain("path", "/w/notes.txt")]);
        let rule = session_standing_grant(&view, Principal::new("amy", PrincipalKind::Human));
        assert_eq!(rule.operations.len(), 1);
        assert_eq!(rule.operations[0].as_str(), "fs.remove");
        assert_eq!(rule.max_uses, None, "`a` means all session, not once more");
        assert_eq!(rule.expires_at, None);
    }

    #[test]
    fn the_rendering_names_the_operation_risk_and_every_resource() {
        let view = view_with(vec![
            Resource::plain("path", "/w/a.txt"),
            Resource::plain("path", "/w/b.txt"),
        ]);
        let rendered = render_request(&view, None);
        assert!(rendered.contains("fs.remove"), "{rendered}");
        assert!(rendered.contains("irreversible"), "{rendered}");
        assert!(rendered.contains("path:/w/a.txt"), "{rendered}");
        assert!(rendered.contains("path:/w/b.txt"), "{rendered}");
        assert!(rendered.contains("display only"), "{rendered}");
        // Tokenless session: the placeholder, never a credential.
        assert!(rendered.contains("<token>"), "{rendered}");
    }

    #[test]
    fn a_resource_with_no_transition_renders_no_arrow() {
        let view = view_with(vec![Resource::plain("path", "/w/a.txt")]);
        let rendered = render_request(&view, None);
        assert!(!rendered.contains('→'), "{rendered}");
    }

    #[test]
    fn a_declared_transition_renders_both_sides() {
        let resource = Resource::transition(
            "git.ref",
            "refs/heads/main",
            StateClaim::Exact("a1b2c3d".to_string()),
            StateClaim::Exact("c3d4e5f".to_string()),
        );
        let rendered = render_request(&view_with(vec![resource]), None);
        assert!(rendered.contains("a1b2c3d → c3d4e5f"), "{rendered}");
    }

    /// A view built the way the ledger builds one, so the rendering under
    /// test sees the real shape rather than a hand-rolled struct literal.
    fn view_with(resources: Vec<Resource>) -> ApprovalRequestView {
        use kaish_types::approval::{
            ApprovalRequest, ApprovalScope, Capture, Invocation, KernelId, PlanBinding, PlanDigest,
            RequestId, RequestOrigin,
        };
        let draft = resources
            .into_iter()
            .fold(
                ApprovalRequest::builder("fs.remove")
                    .risk(RiskClass::Irreversible)
                    .reason("the fs.* enforce policy is on")
                    .hint("rm --confirm=<token> notes.txt"),
                |builder, resource| builder.resource(resource),
            )
            .build()
            .expect("a well-formed draft");
        let scope = ApprovalScope::kernel(KernelId::new(1));
        draft
            .stamp(
                RequestId::new(0x0badcafe, 1),
                std::time::UNIX_EPOCH,
                RequestOrigin::new(
                    scope.clone(),
                    PlanBinding::new(PlanDigest::new("0badcafe"), "/w", scope),
                    Principal::new("session", PrincipalKind::Agent),
                    Capture::Exact(Invocation {
                        tool: "rm".to_string(),
                        argv: vec!["notes.txt".to_string()],
                    }),
                ),
            )
            .into()
    }
}
