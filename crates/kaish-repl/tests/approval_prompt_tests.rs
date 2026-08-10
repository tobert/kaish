//! The REPL fulfils its own gates (`docs/approval-ledger.md` §C.3, PR 11).
//!
//! These drive the REPL exactly as its read loop does — `process_line` with a
//! prompt — and swap the terminal for a scripted answer, so everything below
//! the keystroke is the real path: the real ledger, the real
//! `ApproverHandle`, the real `Kernel::confirm` replay. The one thing they
//! cannot cover is the keystroke itself; that is `pty_approval.rs`.
//!
//! Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::path::Path;

use kaish_kernel::{Kernel, KernelConfig};
use kaish_repl::approval::{Answer, ApprovalPrompt, NoPrompt};
use kaish_repl::{ProcessResult, Repl};

/// A human who always answers the same way, and remembers what it was shown.
struct Scripted {
    answer: Answer,
    seen: Vec<String>,
}

impl Scripted {
    fn new(answer: Answer) -> Self {
        Self {
            answer,
            seen: Vec::new(),
        }
    }
}

impl ApprovalPrompt for Scripted {
    fn ask(&mut self, request: &str) -> Option<Answer> {
        self.seen.push(request.to_string());
        Some(self.answer)
    }
}

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("kaish-repl-approvals")
        .tempdir()
        .expect("tempdir")
}

/// A REPL session with the authority its own kernel minted — the interactive
/// posture, with the enforce policy on and trash off so a gated `rm` deletes
/// rather than recovers.
fn operator_repl(dir: &Path) -> Repl {
    Repl::with_config(
        kaish_repl::interactive_config()
            .with_cwd(dir.to_path_buf())
            .with_interactive(false)
            .with_approvals(true)
            .with_trash(false),
    )
    .expect("repl")
}

fn text(result: ProcessResult) -> String {
    match result {
        ProcessResult::Output(text) => text,
        ProcessResult::Empty => String::new(),
        ProcessResult::Exit => panic!("the line asked the REPL to exit"),
    }
}

// ============================================================================
// §H acceptance test 1 — a session prompts, grants, and the held operation
// completes.
// ============================================================================

#[test]
fn answering_yes_grants_and_the_held_operation_runs() {
    let dir = tempdir();
    let doomed = dir.path().join("notes.txt");
    std::fs::write(&doomed, "delete me").expect("write");
    let mut repl = operator_repl(dir.path());

    let mut human = Scripted::new(Answer::Once);
    let out = text(repl.process_line("rm notes.txt", &mut human));

    assert_eq!(human.seen.len(), 1, "the gate must reach the human once");
    let shown = &human.seen[0];
    assert!(shown.contains("fs.remove"), "{shown}");
    assert!(shown.contains("notes.txt"), "{shown}");
    assert!(!doomed.exists(), "the granted delete must run: {out}");
}

#[test]
fn the_prompt_shows_the_placeholder_and_never_a_credential() {
    // No credential exists before a decision — the key is minted by the
    // grant — so the re-run line a prompt shows is the producer's template.
    let dir = tempdir();
    std::fs::write(dir.path().join("notes.txt"), "keep").expect("write");
    let mut repl = operator_repl(dir.path());

    let mut human = Scripted::new(Answer::Deny);
    repl.process_line("rm notes.txt", &mut human);

    let shown = &human.seen[0];
    assert!(shown.contains("--confirm=<token>"), "{shown}");
    assert!(shown.contains("display only"), "{shown}");
}

// ============================================================================
// §H acceptance test 3 — a denial (`n`, or Ctrl-C, which parses the same way)
// leaves no live request.
// ============================================================================

#[test]
fn answering_no_denies_and_leaves_no_live_request() {
    let dir = tempdir();
    let kept = dir.path().join("notes.txt");
    std::fs::write(&kept, "keep me").expect("write");
    let mut repl = operator_repl(dir.path());

    let mut human = Scripted::new(Answer::Deny);
    let out = text(repl.process_line("rm notes.txt", &mut human));

    assert!(kept.exists(), "a denied delete must not run");
    assert!(out.contains("denied"), "{out}");
    let pending = text(repl.process_line("approvals list", &mut NoPrompt));
    assert!(
        pending.contains("(no pending approvals)"),
        "a denied request must not stay live: {pending}"
    );
}

/// Ctrl-C at the prompt is the same decision as `n`, reached through
/// rustyline's `Interrupted` rather than a signal (spec §C.3). The keystroke
/// itself is a PTY test; this pins that the decision it maps to closes the
/// request.
#[test]
fn an_interrupted_prompt_denies_like_n() {
    assert_eq!(
        kaish_repl::approval::parse_answer(""),
        Answer::Deny,
        "an empty answer — what an interrupted read yields — denies"
    );
}

// ============================================================================
// §H acceptance test 2 — no prompt, no grant, exit 2 preserved.
// ============================================================================

#[test]
fn a_session_that_cannot_ask_returns_the_gate_untouched() {
    let dir = tempdir();
    let kept = dir.path().join("notes.txt");
    std::fs::write(&kept, "keep me").expect("write");
    let mut repl = operator_repl(dir.path());

    let out = text(repl.process_line("rm notes.txt", &mut NoPrompt));

    assert!(kept.exists(), "nothing may run without a decision");
    assert!(out.contains("pending approval"), "{out}");
    let pending = text(repl.process_line("approvals list", &mut NoPrompt));
    assert!(
        pending.contains("req_"),
        "the request stays live for an operator: {pending}"
    );
}

// ============================================================================
// §H acceptance test 4 — `approvals grant` works at the REPL prompt, and
// nowhere an agent can reach.
// ============================================================================

#[test]
fn approvals_grant_works_in_the_repl_session() {
    let dir = tempdir();
    let doomed = dir.path().join("notes.txt");
    std::fs::write(&doomed, "delete me").expect("write");
    let mut repl = operator_repl(dir.path());

    // Take the gate without answering, then grant it by hand — the path a
    // human takes when they want to look at `approvals show` first.
    let gated = text(repl.process_line("rm notes.txt", &mut NoPrompt));
    let id = request_id(&gated);

    let granted = text(repl.process_line(&format!("approvals grant {id}"), &mut NoPrompt));
    assert!(granted.contains("granted"), "{granted}");

    // Spend it the way §D.3 says a human does: retrieve the key, re-run the
    // command with it. A bare re-run would mint a second request — a grant
    // authorizes a request, not a command.
    let token = repl
        .authority()
        .token_for(&kaish_types::approval::RequestId::parse(&id).expect("a request id"))
        .expect("the REPL session may retrieve the key it granted");
    let done = text(repl.process_line(
        &format!("rm --confirm={} notes.txt", token.reveal()),
        &mut NoPrompt,
    ));
    assert!(!doomed.exists(), "the granted delete must run: {done}");
}

#[tokio::test]
async fn approvals_grant_exits_1_in_an_agent_session() {
    let dir = tempdir();
    std::fs::write(dir.path().join("notes.txt"), "keep me").expect("write");
    // `agent()` never installs an authority — the kernel mints one and the
    // embedder keeps it.
    let (kernel, _authority) = Kernel::build(
        KernelConfig::agent_with_root(dir.path().to_path_buf())
            .with_approvals(true)
            .with_trash(false),
    )
    .expect("kernel");

    let gated = kernel.execute("rm notes.txt").await.expect("execute");
    let id = gated.approval_request().expect("a gated request").id;

    let refused = kernel
        .execute(&format!("approvals grant {id}"))
        .await
        .expect("execute");
    assert_eq!(refused.code, 1, "{}", refused.err);
    assert!(
        refused.err.contains("holds no approval authority"),
        "{}",
        refused.err
    );
    assert!(
        dir.path().join("notes.txt").exists(),
        "the agent must not be able to approve itself"
    );
}

// ============================================================================
// §H acceptance test 5 — the re-run line carries the real token only where
// the session can retrieve one.
// ============================================================================

#[test]
fn the_rerun_line_carries_the_real_token_in_an_authority_holding_session() {
    let dir = tempdir();
    std::fs::write(dir.path().join("notes.txt"), "delete me").expect("write");
    let mut repl = operator_repl(dir.path());

    let gated = text(repl.process_line("rm notes.txt", &mut NoPrompt));
    let id_text = request_id(&gated);
    let id = kaish_types::approval::RequestId::parse(&id_text).expect("a request id");

    // Before a decision there is no key to retrieve, in any session.
    assert!(
        repl.authority().token_for(&id).is_none(),
        "a request's key is minted by its grant, not by asking"
    );

    text(repl.process_line(&format!("approvals grant {id}"), &mut NoPrompt));

    let token = repl
        .authority()
        .token_for(&id)
        .expect("an authority-holding session retrieves the key");
    let view = repl
        .authority()
        .approvals_view()
        .get(&id)
        .expect("the request's chain")
        .request;
    let line = kaish_repl::approval::rerun_line(&view.hint, Some(&token));
    assert!(line.contains(token.reveal()), "{line}");
    assert!(!line.contains("<token>"), "{line}");

    // The same hint, rendered by a session that holds no authority: there is
    // no method that returns a credential, so the placeholder stands.
    assert_eq!(
        kaish_repl::approval::rerun_line(&view.hint, None),
        view.hint,
        "a session without the handle can render only the placeholder"
    );
}

// ============================================================================
// The statement gate at the prompt (§C.6) — the `--gate` classifier the REPL
// ships, and what `confirm` replays.
// ============================================================================

fn gated_repl(dir: &Path, names: &[&str]) -> Repl {
    Repl::with_config(
        kaish_repl::interactive_config()
            .with_cwd(dir.to_path_buf())
            .with_interactive(false)
            .with_approvals(false)
            .with_trash(false)
            .with_statement_classifier(kaish_repl::gate_classifier(
                names.iter().map(|n| n.to_string()).collect(),
            )),
    )
    .expect("repl")
}

#[test]
fn a_gated_statement_runs_on_approval() {
    let dir = tempdir();
    let mut repl = gated_repl(dir.path(), &["echo"]);

    let mut human = Scripted::new(Answer::Once);
    let out = text(repl.process_line("echo held", &mut human));

    assert_eq!(human.seen.len(), 1, "the statement gate must ask");
    assert!(human.seen[0].contains("cmd.execute"), "{}", human.seen[0]);
    assert!(out.contains("held"), "the approved statement must run: {out}");
}

/// **The remainder is not resumed, and the REPL says so.** `Kernel::confirm`
/// replays the held statement and nothing after it (spec §C.2), so a line
/// with more statements on it half-runs — which is correct, and must not be
/// quiet.
#[test]
fn statements_after_the_held_one_do_not_run_and_the_repl_names_them() {
    let dir = tempdir();
    let mut repl = gated_repl(dir.path(), &["echo"]);

    let mut human = Scripted::new(Answer::Once);
    let out = text(repl.process_line("echo one; touch made.txt", &mut human));

    assert!(out.contains("one"), "the approved statement runs: {out}");
    assert!(
        !dir.path().join("made.txt").exists(),
        "the kernel replays one statement, never the rest: {out}"
    );
    // What the human is told about the remainder is a stderr diagnostic, so
    // it is pinned where it is built: `unrun_remainder`'s unit tests.
}

/// `a` issues a standing grant, so the *next* matching statement never
/// reaches the human at all (spec §C.3, §C.4).
#[test]
fn answering_always_stops_asking_for_the_same_operation_and_resources() {
    let dir = tempdir();
    let mut repl = gated_repl(dir.path(), &["echo"]);

    let mut human = Scripted::new(Answer::Session);
    let first = text(repl.process_line("echo twice", &mut human));
    assert!(first.contains("twice"), "{first}");

    let second = text(repl.process_line("echo twice", &mut human));
    assert_eq!(
        human.seen.len(),
        1,
        "the standing grant must answer the second one: {second}"
    );
    assert!(second.contains("twice"), "{second}");

    let listed = text(repl.process_line("approvals list --standing", &mut NoPrompt));
    assert!(
        listed.contains("cmd.execute"),
        "the `always` is a rule an operator can read and revoke: {listed}"
    );
}

/// The request id out of the exit-2 message, which is where a human reads it
/// too.
fn request_id(output: &str) -> String {
    output
        .split_whitespace()
        .find(|word| word.starts_with("req_"))
        .unwrap_or_else(|| panic!("no request id in: {output}"))
        .trim_end_matches(|c: char| !c.is_ascii_alphanumeric())
        .to_string()
}
