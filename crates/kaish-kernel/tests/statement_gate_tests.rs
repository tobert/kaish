//! The statement gate (`docs/approval-ledger.md` §C.6): observe-all at the
//! command level, the classifier, the plan, and `confirm`'s
//! parse-and-execute-index replay.
//!
//! Everything drives real source through `kernel.execute()` or
//! `kernel.execute_argv()`, so the whole path runs — parse, validation, the
//! tap, the decision chain, and the statement machinery.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

// KernelConfig::repl() mounts the real filesystem.
#![cfg(feature = "localfs")]

use std::sync::Arc;

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::ledger::{
    ApproverHandle, ClassificationError, CommandNameClassifier, ExecutionContext, LedgerConfig,
    StatementAssessment, StatementClassificationInput, StatementClassifier, StatementPosture,
};
use kaish_kernel::{Kernel, KernelConfig};
use kaish_types::approval::{
    ApprovalScope, AssessorId, AttemptState, Capture, GrantTerms, KernelId, LedgerEntry,
    OperationPattern, Outcome, Plan, PlannedValue, Principal, PrincipalKind, RequestId, ResourcePattern,
    RiskClass, StandingGrant,
};
use kaish_types::Value;

/// A minimal `ExecutionContext` for classifier calls this file drives
/// directly (outside a real kernel).
fn test_execution_context() -> ExecutionContext {
    ExecutionContext::new("/", ApprovalScope::kernel(KernelId::new(1)))
}

/// The entries inside a ledger's records. These tests assert on entry shape;
/// the [`LedgerRecord`] envelope has its own coverage in `kaish-types` (spec
/// §A.5), and an entry this build does not recognize cannot occur here.
#[allow(dead_code)]
fn entries(records: Vec<kaish_types::approval::LedgerRecord>) -> Vec<LedgerEntry> {
    records
        .into_iter()
        .map(|record| {
            record
                .known()
                .cloned()
                .expect("this build wrote every record it reads back")
        })
        .collect()
}


fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("statement-gate-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// A kernel in its own directory, plus the authority its construction minted.
struct Session {
    kernel: Kernel,
    authority: ApproverHandle,
    root: tempfile::TempDir,
}

impl Session {
    /// A session with no classifier: every statement is `Observe`.
    fn observing() -> Self {
        Self::build(None, None)
    }

    /// A session whose classifier gates every statement planning `rm`.
    fn gating_rm() -> Self {
        Self::build(Some(rm_classifier()), None)
    }

    fn build(
        classifier: Option<Arc<dyn StatementClassifier>>,
        ledger: Option<LedgerConfig>,
    ) -> Self {
        let root = tempdir();
        let mut config = KernelConfig::repl()
            .with_cwd(root.path().to_path_buf())
            .with_approvals(false)
            .with_trash(false)
            .with_principal(Principal::new("test-agent", PrincipalKind::Agent));
        if let Some(classifier) = classifier {
            config = config.with_statement_classifier(classifier);
        }
        if let Some(ledger) = ledger {
            config = config.with_ledger(ledger);
        }
        let (kernel, authority) = Kernel::build(config).expect("kernel");
        Self {
            kernel,
            authority,
            root,
        }
    }

    async fn run(&self, source: &str) -> ExecResult {
        self.kernel.execute(source).await.expect("kernel execute")
    }

    fn path(&self, name: &str) -> std::path::PathBuf {
        self.root.path().join(name)
    }

    fn write(&self, name: &str, contents: &str) {
        std::fs::write(self.path(name), contents).unwrap();
    }

    /// Every statement-tap entry's plan, in commit order.
    fn plans(&self) -> Vec<Plan> {
        entries(self.kernel.approvals().log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items)
            .into_iter()
            .filter_map(|entry| match entry {
                LedgerEntry::Observed {
                    operation, plan, ..
                } if operation.as_str() == "cmd.execute" => plan,
                _ => None,
            })
            .collect()
    }

    /// Every statement-tap entry, in commit order.
    fn taps(&self) -> Vec<LedgerEntry> {
        entries(self.kernel.approvals().log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items)
            .into_iter()
            .filter(|entry| {
                matches!(entry, LedgerEntry::Observed { operation, .. }
                    if operation.as_str() == "cmd.execute")
            })
            .collect()
    }

    async fn grant(&self, id: &RequestId) {
        let view = self
            .kernel
            .approvals()
            .get(id)
            .expect("the request's chain")
            .request;
        self.authority
            .grant(
                id,
                view.revision,
                GrantTerms::once_for_view(
                    &view,
                    std::time::SystemTime::now() + std::time::Duration::from_secs(300),
                ),
            )
            .await
            .expect("the grant must post");
    }

    /// Grant `id` and retrieve its bearer key — what an operator hands back
    /// to whoever re-runs the line.
    async fn grant_and_key(&self, id: &RequestId) -> String {
        self.grant(id).await;
        self.authority
            .token_for(id)
            .expect("a granted request has a key")
            .reveal()
            .to_string()
    }

    /// Every request id the ledger holds, in allocation order.
    fn request_ids(&self) -> Vec<RequestId> {
        let mut ids = self.kernel.approvals().ids();
        ids.sort_by_key(RequestId::seq);
        ids
    }

    /// Everything a reader can reach: the whole ledger log plus every VFS
    /// projection under `/v/approvals`. What a credential scan searches.
    async fn readable_surface(&self, ids: &[RequestId]) -> String {
        let mut surface = serde_json::to_string(&entries(self.kernel.approvals().log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items))
            .expect("the log serializes");
        surface.push_str(&serde_json::to_string(&self.kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items).unwrap());
        for node in ["pending", "standing", "log"] {
            surface.push_str(&self.run(&format!("cat /v/approvals/{node}")).await.text_out());
        }
        for id in ids {
            for node in ["request", "state", "attempts", "grant"] {
                surface.push_str(
                    &self
                        .run(&format!("cat /v/approvals/{id}/{node}"))
                        .await
                        .text_out(),
                );
            }
        }
        surface
    }
}

/// Gates every statement, whatever it plans — including the ones that plan no
/// commands at all.
struct GateEverything;

impl StatementClassifier for GateEverything {
    fn classify(
        &self,
        _input: &StatementClassificationInput<'_>,
    ) -> Result<StatementAssessment, ClassificationError> {
        Ok(StatementAssessment::new(
            StatementPosture::gate("the test gates everything", RiskClass::Reversible),
            AssessorId::new("gate-everything-test-fixture"),
        ))
    }
}

fn rm_classifier() -> Arc<dyn StatementClassifier> {
    Arc::new(CommandNameClassifier::new(
        ["rm"],
        "the statement plans a destructive command",
        RiskClass::Irreversible,
    ))
}

// ============================================================================
// Observe-all: one entry per top-level statement, and no more
// ============================================================================

/// The floor: every executed top-level statement posts exactly one entry,
/// carrying its plan.
#[tokio::test]
async fn every_top_level_statement_posts_exactly_one_entry_with_its_plan() {
    let session = Session::observing();
    let result = session.run("echo one\necho two\necho three").await;
    assert_eq!(result.code, 0, "{}", result.err);

    let plans = session.plans();
    assert_eq!(
        plans.iter().map(|p| p.rendered.as_str()).collect::<Vec<_>>(),
        vec!["echo one", "echo two", "echo three"],
    );
    for plan in &plans {
        assert_eq!(plan.statement_kind, "command");
        assert_eq!(plan.commands.len(), 1);
    }
}

/// The rule the two-site restriction exists for: a 1,000-iteration loop is
/// **one** top-level statement, so it posts one entry. A tap inside
/// `execute_stmt_flow` would post a thousand.
#[tokio::test]
async fn a_thousand_iteration_loop_posts_one_entry() {
    let session = Session::observing();
    let items: Vec<String> = (0..1_000).map(|i| i.to_string()).collect();
    let result = session
        .run(&format!("for i in {}; do echo $i; done", items.join(" ")))
        .await;
    assert_eq!(result.code, 0, "{}", result.err);

    let plans = session.plans();
    assert_eq!(plans.len(), 1, "a loop is one statement, not a thousand");
    assert_eq!(plans[0].statement_kind, "for");
    assert_eq!(
        plans[0].commands.len(),
        1,
        "one planned command — the body's `echo`, planned once"
    );
}

/// The other half of the two-site rule: a nested statement engine — a user
/// tool's body, a `$(…)` block, a sourced script — never taps. Those
/// statements belong to their enclosing top-level statement's plan.
#[tokio::test]
async fn nested_statement_engines_post_nothing_of_their_own() {
    let session = Session::observing();
    let result = session
        .run("two() { echo a; echo b; }\ntwo\nx=$(echo c; echo d)\necho ${x}")
        .await;
    assert_eq!(result.code, 0, "{}", result.err);

    let plans = session.plans();
    assert_eq!(
        plans.iter().map(|p| p.rendered.as_str()).collect::<Vec<_>>(),
        vec![
            "tool two() { echo a; echo b }",
            "two",
            "x=$(echo c; echo d)",
            "echo ${x}",
        ],
        "four top-level statements, four entries"
    );
}

/// A backgrounded statement is a top-level statement: tapped once at the loop
/// site, before the spawn, with no second site in the background machinery.
#[tokio::test]
async fn a_backgrounded_statement_is_tapped_once_at_the_loop_site() {
    let session = Session::observing();
    let result = session.run("echo bg &").await;
    assert_eq!(result.code, 0, "{}", result.err);
    session.run("wait").await;

    let plans = session.plans();
    let backgrounded: Vec<&Plan> = plans
        .iter()
        .filter(|p| p.commands.iter().any(|c| c.background))
        .collect();
    assert_eq!(backgrounded.len(), 1, "{plans:?}");
    assert_eq!(backgrounded[0].rendered, "echo bg &");
}

/// A statement's resources name its commands, so a standing grant and a
/// policy match them with the same machinery every other operation uses.
#[tokio::test]
async fn a_tap_entry_names_one_cmd_resource_per_planned_command() {
    let session = Session::observing();
    session.run("cat a | grep x | wc -l").await;

    let taps = session.taps();
    assert_eq!(taps.len(), 1);
    let LedgerEntry::Observed { resources, .. } = &taps[0] else {
        panic!("expected the tap entry, got {:?}", taps[0]);
    };
    assert_eq!(
        resources
            .iter()
            .map(|r| (r.kind.as_str(), r.id.as_str()))
            .collect::<Vec<_>>(),
        vec![("cmd", "cat"), ("cmd", "grep"), ("cmd", "wc")]
    );
    assert!(
        resources.iter().all(|r| r.subscription.is_none()),
        "`cmd.*` never enters the subscription registry"
    );
}

// ============================================================================
// The gate
// ============================================================================

/// A gate holds the statement before **anything** of it has run: no
/// substitution side effect, no redirect target created.
#[tokio::test]
async fn a_gated_statement_defers_to_exit_2_with_nothing_executed() {
    let session = Session::gating_rm();
    session.write("target.txt", "keep me");

    let result = session
        .run("rm $(echo hi > from-substitution.txt) target.txt > redirect-target.txt")
        .await;

    assert_eq!(result.code, 2, "expected a deferral: {}", result.err);
    let view = result.approval_request().expect("a pending request");
    assert_eq!(view.operation.as_str(), "cmd.execute");
    assert_eq!(view.risk, RiskClass::Irreversible);
    assert!(
        view.plan.is_some(),
        "the request carries the plan it was judged on"
    );

    assert!(
        session.path("target.txt").exists(),
        "the target must survive the gate"
    );
    assert!(
        !session.path("from-substitution.txt").exists(),
        "no substitution may have run"
    );
    assert!(
        !session.path("redirect-target.txt").exists(),
        "no redirect target may have been created"
    );
}

/// The tap records the **ask**, not the execution: a statement that defers
/// and never runs still keeps its tap entry, and that entry precedes the
/// `Requested` one. `Assessed` follows `Requested` — the classifier's own
/// judgment is recorded once the request it explains actually exists (spec
/// §C.7), never ahead of it.
#[tokio::test]
async fn a_deferred_statement_keeps_its_tap_entry_ahead_of_the_request() {
    let session = Session::gating_rm();
    session.write("target.txt", "keep me");
    let result = session.run("rm target.txt").await;
    assert_eq!(result.code, 2, "{}", result.err);

    let kinds: Vec<&str> = entries(session.kernel.approvals().log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items)
        .iter()
        .map(|e| match e {
            LedgerEntry::Observed { .. } => "Observed",
            LedgerEntry::Requested { .. } => "Requested",
            LedgerEntry::Assessed { .. } => "Assessed",
            other => panic!("unexpected entry {other:?}"),
        })
        .collect();
    assert_eq!(kinds, vec!["Observed", "Requested", "Assessed"]);
}

/// A standing grant over `cmd` resources auto-approves, and the statement
/// runs inline — the same §C.4 machinery every other operation uses.
#[tokio::test]
async fn a_standing_grant_over_cmd_resources_auto_approves() {
    let session = Session::gating_rm();
    session.write("target.txt", "delete me");
    session
        .authority
        .grant_standing(StandingGrant::new(
            vec![OperationPattern::new("cmd.execute")],
            vec![ResourcePattern::new("cmd", "rm")],
            None,
            None,
            Principal::new("operator", PrincipalKind::Human),
            "the operator pre-approved deletes",
        ))
        .await
        .expect("the standing grant must issue");

    let result = session.run("rm target.txt").await;
    assert_eq!(result.code, 0, "expected the grant to fire: {}", result.err);
    assert!(!session.path("target.txt").exists(), "the delete must run");
}

/// All-or-nothing: a standing grant covering two of a statement's three
/// commands does not cover the statement.
#[tokio::test]
async fn a_standing_grant_missing_one_command_of_three_defers() {
    let session = Session::gating_rm();
    session.write("target.txt", "keep me");
    session
        .authority
        .grant_standing(StandingGrant::new(
            vec![OperationPattern::new("cmd.execute")],
            vec![
                ResourcePattern::new("cmd", "rm"),
                ResourcePattern::new("cmd", "echo"),
            ],
            None,
            None,
            Principal::new("operator", PrincipalKind::Human),
            "the operator pre-approved rm and echo, and nothing else",
        ))
        .await
        .expect("the standing grant must issue");

    let result = session.run("echo start && rm target.txt && cat target.txt").await;
    assert_eq!(
        result.code, 2,
        "an uncovered `cat` must hold the whole statement: {}",
        result.err
    );
    assert!(session.path("target.txt").exists());
}

// ============================================================================
// Replay
// ============================================================================

/// `confirm` on a `Capture::Statement` re-parses the source and runs exactly
/// the held statement — and the variable an *earlier* statement set is still
/// visible, because earlier statements' effects are session state.
#[tokio::test]
async fn confirm_replays_the_held_statement_with_earlier_variables_visible() {
    let session = Session::gating_rm();
    session.write("chosen.txt", "delete me");

    let held = session.run("target=chosen.txt\nrm ${target}").await;
    assert_eq!(held.code, 2, "{}", held.err);
    let view = held.approval_request().expect("a pending request");
    assert!(
        matches!(&view.capture, Capture::Statement { index, .. } if *index == 1),
        "the capture must name the held statement's index: {:?}",
        view.capture
    );
    assert!(
        session.path("chosen.txt").exists(),
        "nothing ran before the decision"
    );

    session.grant(&view.id).await;
    let replayed = session
        .kernel
        .confirm(&session.authority, &view.id)
        .await
        .expect("confirm");

    assert_eq!(replayed.code, 0, "the replay must run: {}", replayed.err);
    assert!(
        !session.path("chosen.txt").exists(),
        "the replay resolved ${{target}} from statement 0's assignment"
    );
}

/// The replay posts no second tap entry: `confirm` must not record an
/// `Observed` for the statement it replays.
#[tokio::test]
async fn a_replay_posts_no_second_tap_entry() {
    let session = Session::gating_rm();
    session.write("chosen.txt", "delete me");
    let held = session.run("rm chosen.txt").await;
    let view = held.approval_request().expect("a pending request");
    let taps_before = session.taps().len();

    session.grant(&view.id).await;
    let replayed = session
        .kernel
        .confirm(&session.authority, &view.id)
        .await
        .expect("confirm");
    assert_eq!(replayed.code, 0, "{}", replayed.err);
    assert_eq!(
        session.taps().len(),
        taps_before,
        "the replay must not re-record the statement it replays"
    );
}

/// An `fs.*` replay is left alone by the statement site: its correlation
/// belongs to `rm`'s own gate further down, and consuming it there would
/// strand the reservation.
#[tokio::test]
async fn an_fs_replay_passes_through_the_statement_site_untouched() {
    // No classifier: the statement layer observes, the `fs.*` enforce policy
    // gates, and `confirm` replays the captured argv.
    let session = Session::observing();
    session.write("precious.txt", "keep me");
    session.run("set -o approvals").await;

    let gated = session.run("rm precious.txt").await;
    assert_eq!(gated.code, 2, "{}", gated.err);
    let view = gated.approval_request().expect("a gated request");
    assert_eq!(view.operation.as_str(), "fs.remove");

    session.grant(&view.id).await;
    let replayed = session
        .kernel
        .confirm(&session.authority, &view.id)
        .await
        .expect("confirm");
    assert_eq!(
        replayed.code, 0,
        "the fs replay must reach rm's gate: {}",
        replayed.err
    );
    assert!(!session.path("precious.txt").exists());
}

// ============================================================================
// Redeeming a held statement by re-running it with the key
// ============================================================================

/// The other half of `confirm`: an operator hands the key back, the agent
/// re-runs the line with `--confirm=<key>`, and **the original request**
/// redeems. Without the statement gate reading the key off its own plan, the
/// re-run would mint a second request and exit 2 again with the first still
/// pending.
#[tokio::test]
async fn re_running_a_held_statement_with_the_key_redeems_the_original_request() {
    let session = Session::gating_rm();
    session.write("target.txt", "delete me");

    let held = session.run("rm target.txt").await;
    assert_eq!(held.code, 2, "{}", held.err);
    let id = held.approval_request().expect("a pending request").id;
    let key = session.grant_and_key(&id).await;

    let redeemed = session.run(&format!("rm --confirm={key} target.txt")).await;
    assert_eq!(
        redeemed.code, 0,
        "the re-run must redeem, not defer again: {}",
        redeemed.err
    );
    assert!(!session.path("target.txt").exists(), "the statement must run");

    assert_eq!(
        session.request_ids(),
        vec![id.clone()],
        "the re-run must redeem the original request, not mint a second one"
    );
    let chain = session.kernel.approvals().get(&id).expect("the chain");
    assert_eq!(
        chain.attempts.len(),
        1,
        "exactly one attempt, and it belongs to the original request"
    );
    assert_eq!(chain.attempts[0].state, AttemptState::Settled);
    assert_eq!(chain.attempts[0].outcome, Some(Outcome::Exit(0)));
    assert!(
        session.kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items.is_empty(),
        "the chain must be closed, not left pending"
    );
}

/// The §A.2 scan, extended to the statement gate: **no** reader-visible
/// surface carries the issued credential — not the log, not `/v/approvals`,
/// and not `Capture::Statement.source`, which is the raw line the user typed
/// and therefore the one place a re-run's key would land verbatim.
#[tokio::test]
async fn no_readable_surface_carries_a_key_a_re_run_presented() {
    let session = Session::gating_rm();
    session.write("target.txt", "delete me");

    let held = session.run("rm target.txt").await;
    let id = held.approval_request().expect("a pending request").id;
    let key = session.grant_and_key(&id).await;
    session.run(&format!("rm --confirm={key} target.txt")).await;

    // The record still shows that a key was presented — redaction removes the
    // secret, not the fact. Read this before the scan below, whose own `cat`
    // statements are tapped too.
    let re_run = session
        .plans()
        .last()
        .cloned()
        .expect("a plan for the re-run");
    assert_eq!(re_run.rendered, "rm --confirm=<confirm-key> target.txt");

    let ids = session.request_ids();
    let surface = session.readable_surface(&ids).await;
    assert!(
        !surface.contains(&key),
        "a reader-visible surface leaked the credential {key}: {surface}"
    );
}

/// A key one statement presented does not ride along in a **later**
/// statement's capture. `Capture::Statement` records the whole program
/// source, so redacting only the held statement's own key would leave an
/// earlier line's key sitting in it.
#[tokio::test]
async fn a_key_from_an_earlier_statement_is_redacted_from_a_later_capture() {
    let session = Session::gating_rm();
    session.write("first.txt", "delete me");
    session.write("second.txt", "delete me too");

    let held = session.run("rm first.txt").await;
    let id = held.approval_request().expect("a pending request").id;
    let key = session.grant_and_key(&id).await;

    // One program: line 0 redeems with the key, line 1 is held in its turn.
    let second = session
        .run(&format!("rm --confirm={key} first.txt\nrm second.txt"))
        .await;
    assert_eq!(second.code, 2, "the second statement must be held: {}", second.err);
    let view = second.approval_request().expect("a second pending request");
    let Capture::Statement { source, index } = &view.capture else {
        panic!("expected a statement capture, got {:?}", view.capture);
    };
    assert_eq!(*index, 1);
    assert!(
        !source.contains(&key),
        "the earlier statement's key rode along in the capture: {source}"
    );
    assert_eq!(source, "rm first.txt\nrm second.txt");
}

/// The same scan for a key that never redeems: an unmatched presentation
/// posts nothing, and what it does record still carries no credential.
#[tokio::test]
async fn a_key_that_matches_nothing_leaks_nothing() {
    let session = Session::observing();
    let bogus = "0123456789abcdef0123456789abcdef";
    session.run(&format!("rm --confirm={bogus} nothing.txt")).await;

    let surface = session.readable_surface(&session.request_ids()).await;
    assert!(
        !surface.contains(bogus),
        "an unmatched presentation leaked its key: {surface}"
    );
}

/// A key carried by a variable is neither lifted nor redacted, and needs to
/// be neither: the plan is unexpanded, so the statement that presents it
/// records `${key}` and never the value. Redaction covers exactly what the
/// record can see, which is exactly what it could leak.
#[tokio::test]
async fn a_variable_carried_key_renders_as_written_and_carries_no_value() {
    let session = Session::observing();
    let secret = "fedcba9876543210fedcba9876543210";
    session.run(&format!("key={secret}")).await;
    session.run("rm --confirm=${key} nothing.txt").await;

    let plans = session.plans();
    let presenting = plans.last().expect("a plan");
    assert_eq!(presenting.rendered, "rm --confirm=${key} nothing.txt");
    assert!(
        !presenting.rendered.contains(secret)
            && !presenting.commands[0]
                .args
                .contains(&PlannedValue::Plain(secret.to_string())),
        "the statement that presents the key must record no value: {presenting:?}"
    );
}

// ============================================================================
// The argv door
// ============================================================================

/// `execute_argv` bypasses the statement loop, so it carries its own tap —
/// and it posts the entry the statement loop would have posted for the same
/// command.
#[tokio::test]
async fn execute_argv_posts_the_same_entry_the_statement_loop_would() {
    let by_argv = Session::observing();
    by_argv
        .kernel
        .execute_argv("echo", &[Value::String("hi".to_string())])
        .await
        .expect("execute_argv");

    let by_source = Session::observing();
    by_source.run("echo hi").await;

    assert_eq!(by_argv.plans(), by_source.plans());
    assert_eq!(by_argv.plans().len(), 1);
}

/// A gated `execute_argv` captures `Capture::Exact`, not
/// `Capture::Statement` — it already holds a tool name and an argv, and
/// `confirm`'s existing arm replays that form.
#[tokio::test]
async fn a_gated_execute_argv_captures_exact() {
    let session = Session::gating_rm();
    session.write("target.txt", "keep me");

    let result = session
        .kernel
        .execute_argv("rm", &[Value::String("target.txt".to_string())])
        .await
        .expect("execute_argv");
    assert_eq!(result.code, 2, "{}", result.err);
    let view = result.approval_request().expect("a pending request");
    assert!(
        matches!(&view.capture, Capture::Exact(invocation) if invocation.tool == "rm"),
        "expected an exact capture, got {:?}",
        view.capture
    );
    assert!(session.path("target.txt").exists());

    session.grant(&view.id).await;
    let replayed = session
        .kernel
        .confirm(&session.authority, &view.id)
        .await
        .expect("confirm");
    assert_eq!(replayed.code, 0, "{}", replayed.err);
    assert!(!session.path("target.txt").exists());
}

/// A replay whose statement **errors** still settles its attempt. An attempt
/// left `Reserved` fails every later redemption of its grant with
/// `AttemptInFlight` until the sweep abandons it — a grant an operator could
/// no longer use, for a replay that never ran.
#[tokio::test]
async fn a_replay_that_errors_still_settles_its_attempt() {
    // An assignment whose right-hand side cannot be evaluated is one of the
    // few statements that fails as an *error* rather than an exit code, which
    // is exactly the path that used to skip the settlement. It plans no
    // commands, so it needs a classifier that gates on more than a name.
    let session = Session::build(Some(Arc::new(GateEverything)), None);
    let held = session.run("boom=$((1/0))").await;
    assert_eq!(held.code, 2, "{}", held.err);
    let id = held.approval_request().expect("a pending request").id;
    session.grant(&id).await;

    let replayed = session.kernel.confirm(&session.authority, &id).await;
    assert!(
        replayed.is_err(),
        "the fixture must error on replay, or this proves nothing"
    );

    let chain = session.kernel.approvals().get(&id).expect("the chain");
    assert_eq!(chain.attempts.len(), 1, "{:?}", chain.attempts);
    assert_eq!(
        chain.attempts[0].state,
        AttemptState::Settled,
        "an errored replay must not leave its attempt Reserved: {:?}",
        chain.attempts[0]
    );
    assert!(
        matches!(chain.attempts[0].outcome, Some(Outcome::Error(_))),
        "the honest record of an error is Outcome::Error: {:?}",
        chain.attempts[0].outcome
    );
}

// ============================================================================
// The classifier's panic contract
// ============================================================================

/// A classifier that panics gates rather than taking the statement loop down
/// with it (spec §C.6, R4): the tap wraps `classify` in `catch_unwind` and
/// maps a caught panic to `Gate` the same way it maps an `Err` return —
/// `Observe` is a bypass, and a classifier that cannot answer must not be
/// able to turn the statement gate off. This inverts the pre-R4 contract
/// (`Policy::evaluate` still propagates a panic unguarded — see its own doc
/// for why the two hooks diverge): a classifier runs in front of *every*
/// statement, including the ones nobody would ever gate, so its own failure
/// must default to the conservative answer instead of taking the whole
/// program down over one broken rule.
struct PanickingClassifier;

impl StatementClassifier for PanickingClassifier {
    fn classify(
        &self,
        _input: &StatementClassificationInput<'_>,
    ) -> Result<StatementAssessment, ClassificationError> {
        panic!("the embedder's classifier is broken");
    }
}

#[tokio::test]
async fn a_panicking_classifier_gates_rather_than_unwinding_into_the_statement_loop() {
    let session = Session::build(Some(Arc::new(PanickingClassifier)), None);
    let result = session.run("echo hi").await;
    assert_eq!(
        result.code, 2,
        "a panicking classifier must gate (exit 2), not crash the statement loop: {}",
        result.err
    );
    assert!(
        result.approval_request().is_some(),
        "the gate must carry a pending request the caller can act on"
    );
}

/// A classifier returning `Err` gates and does not observe (spec §C.6):
/// `Err` maps to `Gate`, never to `Observe`, and never silently. Distinct
/// from the panic case above — this is the classifier's own honest "I
/// cannot judge this" answer, not a bug taking the process down.
struct ErroringClassifier;

impl StatementClassifier for ErroringClassifier {
    fn classify(
        &self,
        _input: &StatementClassificationInput<'_>,
    ) -> Result<StatementAssessment, ClassificationError> {
        Err(ClassificationError::new("the model backing this classifier is unreachable"))
    }
}

#[tokio::test]
async fn an_erroring_classifier_gates_and_does_not_observe() {
    let session = Session::build(Some(Arc::new(ErroringClassifier)), None);
    let result = session.run("echo hi").await;
    assert_eq!(
        result.code, 2,
        "a classifier Err must gate (exit 2), never silently Observe: {}",
        result.err
    );
    let id = result.approval_request().expect("a pending request").id;
    let taps = session.taps();
    assert_eq!(
        taps.len(),
        1,
        "the statement is still recorded once — Err changes the posture, not whether it is tapped"
    );
    // "Does not observe" means the statement never ran unrecorded and
    // unasked — it is held pending, not silently executed.
    assert_eq!(session.kernel.approvals().get(&id).unwrap().state, kaish_types::approval::RequestState::Requested);
}

// ============================================================================
// The static gate floor (spec §C.6): a classifier can raise, never lower
// ============================================================================

/// A classifier that always answers `Observe`, whatever it is shown — the
/// floor test's whole point is that this classifier's own answer must not
/// win.
struct AlwaysObserve;

impl StatementClassifier for AlwaysObserve {
    fn classify(
        &self,
        _input: &StatementClassificationInput<'_>,
    ) -> Result<StatementAssessment, ClassificationError> {
        Ok(StatementAssessment::new(
            StatementPosture::Observe,
            AssessorId::new("always-observe-test-fixture"),
        ))
    }
}

/// A classifier cannot lower a posture the kernel's own static rules set
/// (spec §C.6). `kaish-trash empty` sits behind the one static floor R4
/// seeded (`KernelOperation::TrashEmpty` is `always_enforced` at the `fs.*`
/// layer too — spec §F.1) — it must still gate even under a classifier that
/// always says `Observe`.
#[tokio::test]
async fn a_classifier_cannot_lower_the_static_floor_on_trash_empty() {
    let session = Session::build(Some(Arc::new(AlwaysObserve)), None);
    let result = session.run("kaish-trash empty").await;
    assert_eq!(
        result.code, 2,
        "the static floor must gate kaish-trash empty regardless of the classifier's Observe: {}",
        result.err
    );
    assert!(
        result.approval_request().is_some(),
        "the gate must carry a pending request"
    );
}

/// The floor never *fires* for a statement it does not name — an
/// `AlwaysObserve` classifier plus an unrelated statement really does
/// observe-and-run, which is what makes the test above meaningful rather
/// than every statement gating regardless of classifier input.
#[tokio::test]
async fn the_static_floor_does_not_gate_unrelated_statements() {
    let session = Session::build(Some(Arc::new(AlwaysObserve)), None);
    let result = session.run("echo hi").await;
    assert_eq!(result.code, 0, "{}", result.err);
    assert_eq!(result.text_out().trim(), "hi");
}

// ============================================================================
// `ExecutionContext` carries no host path (spec §C.6)
// ============================================================================

/// Records every `cwd` it is shown, then observes everything — a spy, not a
/// judgment.
struct RecordingClassifier {
    seen_cwds: std::sync::Mutex<Vec<String>>,
}

impl RecordingClassifier {
    fn new() -> Self {
        Self {
            seen_cwds: std::sync::Mutex::new(Vec::new()),
        }
    }
}

impl StatementClassifier for RecordingClassifier {
    fn classify(
        &self,
        input: &StatementClassificationInput<'_>,
    ) -> Result<StatementAssessment, ClassificationError> {
        self.seen_cwds.lock().unwrap().push(input.context.cwd.clone());
        // Gates, so the same statement also produces a real request with a
        // `PlanBinding` this test can compare `ExecutionContext.cwd`
        // against.
        Ok(StatementAssessment::new(
            StatementPosture::gate("recording fixture always gates", RiskClass::Reversible),
            AssessorId::new("recording-test-fixture"),
        ))
    }
}

/// `ExecutionContext.cwd` is a `String`, not a `PathBuf` or any other
/// host-specific type — the same convention `PlanBinding::cwd` already uses
/// for the identical reason (spec §A.9): kaish has no `VirtualPath`
/// newtype, so "never a host path" is a statement about which *spelling* a
/// consumer gets, not a type that could carry OS-specific quirks (a raw
/// `PathBuf`'s platform separator, a `Path` that borrows a lifetime tied to
/// the kernel's own process). Pinned here by checking that the two seams
/// which both descend from `self.cwd` — the statement's `PlanBinding` (an
/// already-shipped part of the request) and what the classifier is shown —
/// report the identical string for the identical statement, so a future
/// change cannot let one seam start leaking something the other does not.
#[tokio::test]
async fn execution_context_cwd_is_the_same_logical_spelling_plan_binding_uses() {
    let classifier = Arc::new(RecordingClassifier::new());
    let session = Session::build(Some(classifier.clone() as Arc<dyn StatementClassifier>), None);
    let held = session.run("echo hi").await;
    assert_eq!(held.code, 2, "the recording classifier always gates: {}", held.err);

    let id = held.approval_request().expect("a pending request").id;
    let chain = session.kernel.approvals().get(&id).expect("the chain");

    let seen = classifier.seen_cwds.lock().unwrap();
    assert_eq!(seen.len(), 1, "{seen:?}");
    assert_eq!(
        seen[0], chain.request.binding.cwd,
        "ExecutionContext.cwd and PlanBinding.cwd must report the same logical spelling for the same statement"
    );
}

// ============================================================================
// Backpressure: the tap warns, the gate fails closed
// ============================================================================

/// A ledger with no room for a single entry. Ring capacity and sink
/// backpressure surface at the same seam and as the same `LedgerError` class
/// (spec §D.4); this one refuses deterministically, so it is what the two
/// tests below pin.
fn refusing_ledger() -> LedgerConfig {
    LedgerConfig::default().with_retained_entries(0)
}

/// A tap that cannot commit warns, and the statement **still runs**. The tap
/// is a second opinion, not a permission gate, and nobody opted into a
/// completeness guarantee here.
#[tokio::test]
async fn a_tap_the_ledger_refuses_warns_and_the_statement_still_runs() {
    let session = Session::build(None, Some(refusing_ledger()));
    let result = session.run("echo still ran").await;
    assert_eq!(result.code, 0, "the statement must run: {}", result.err);
    assert_eq!(result.text_out().trim(), "still ran");
    assert!(
        session.taps().is_empty(),
        "the entry genuinely did not commit"
    );
}

/// A gate that cannot record its decision fails closed. Every fail-closed
/// rule holds for a gate-classified statement: a decision that cannot be
/// recorded is not made.
#[tokio::test]
async fn a_gate_the_ledger_refuses_still_fails_closed() {
    let session = Session::build(Some(rm_classifier()), Some(refusing_ledger()));
    session.write("target.txt", "keep me");
    let result = session.run("rm target.txt").await;
    assert_eq!(
        result.code, 1,
        "an unrecordable gate must fail closed: {}",
        result.err
    );
    assert!(
        result.err.contains("ledger unavailable"),
        "the failure must name why: {}",
        result.err
    );
    assert!(
        session.path("target.txt").exists(),
        "nothing may run behind an undecidable gate"
    );
}

// ============================================================================
// Rendering
// ============================================================================

/// The rendering is bounded: 8 KiB, with a marker naming the number. The
/// structure survives the cut, because that is what a classifier reads.
#[tokio::test]
async fn rendering_truncates_at_8_kib_with_the_marker() {
    let session = Session::observing();
    let long = "x".repeat(16 * 1024);
    session.run(&format!("echo {long}")).await;

    let plans = session.plans();
    assert_eq!(plans.len(), 1);
    assert!(
        plans[0].rendered.contains("[rendering truncated at 8192 bytes]"),
        "expected the marker on a {}-byte rendering",
        plans[0].rendered.len()
    );
    assert_eq!(plans[0].commands.len(), 1);
    assert_eq!(plans[0].commands[0].name, "echo");
}

/// The plan is unexpanded: `${HOME}` and `$(…)` reach the classifier as
/// written, because it judges what was asked, not what it resolved to.
#[tokio::test]
async fn the_plan_reaches_the_ledger_unexpanded() {
    let session = Session::observing();
    session.run("target=chosen.txt").await;
    session.run("echo \"${target}\" > out.txt").await;

    let plans = session.plans();
    let last = plans.last().expect("a plan");
    assert_eq!(last.rendered, "echo \"${target}\" > out.txt");
    assert_eq!(
        last.commands[0].redirects[0].target,
        PlannedValue::Plain("out.txt".to_string())
    );
}

/// A `--confirm=<token>` value never reaches the ledger (spec §A.2): no
/// entry carries a credential, and the plan is an entry.
#[tokio::test]
async fn a_presented_credential_is_redacted_from_the_plan() {
    let session = Session::observing();
    session.run("rm --confirm=deadbeefdeadbeef nothing.txt").await;

    let plans = session.plans();
    let last = plans.last().expect("a plan");
    assert!(
        !last.rendered.contains("deadbeefdeadbeef"),
        "the plan leaked a credential: {}",
        last.rendered
    );
    assert!(
        last.rendered.contains("--confirm=<confirm-key>"),
        "the record must still show that a key was presented: {}",
        last.rendered
    );
}

// ============================================================================
// The measurement: does the plan discriminate better than the raw line?
// ============================================================================

/// §C.6 asks for a measurement rather than an assertion: does classifying
/// the *plan* discriminate better than classifying the raw line?
///
/// Both classifiers below answer one question — "does this statement run
/// `rm`?" — one from the plan's `commands`, one from the line's whitespace
/// tokens. The corpus is labeled with the truth. The test prints both scores
/// and requires the plan to be at least as accurate, with at least one case
/// separating them; it does not hard-code a percentage, because the number
/// belongs to the corpus and would rot.
#[tokio::test]
async fn the_plan_discriminates_at_least_as_well_as_the_raw_line() {
    // (source, does it actually run `rm`?)
    let corpus: &[(&str, bool)] = &[
        ("rm target.txt", true),
        ("for f in a b; do rm $f; done", true),
        ("echo start && rm target.txt", true),
        ("cat list.txt | rm", true),
        ("echo 'rm target.txt'", false),
        ("grep rm changelog.txt", false),
        ("echo firmware.txt", false),
        ("note=rm", false),
        ("cat rm", false),
    ];

    let session = Session::observing();
    let classifier = CommandNameClassifier::new(
        ["rm"],
        "the statement plans a destructive command",
        RiskClass::Irreversible,
    );

    let mut plan_hits = 0usize;
    let mut raw_hits = 0usize;
    let mut disagreements = 0usize;
    let ctx = test_execution_context();
    for (source, truth) in corpus {
        session.run(source).await;
        let plan = session.plans().last().cloned().expect("a plan per statement");
        let by_plan = classifier
            .classify(&StatementClassificationInput::new(&plan, &ctx))
            .expect("the reference classifier never errors")
            .posture
            .is_gate();
        let by_raw = raw_line_says_rm(source);
        if by_plan == *truth {
            plan_hits += 1;
        }
        if by_raw == *truth {
            raw_hits += 1;
        }
        if by_plan != by_raw {
            disagreements += 1;
        }
    }

    println!(
        "plan-based: {plan_hits}/{} correct; raw-line: {raw_hits}/{} correct; {disagreements} disagreements",
        corpus.len(),
        corpus.len()
    );
    assert!(
        plan_hits >= raw_hits,
        "the plan must not classify worse than the raw line: {plan_hits} vs {raw_hits}"
    );
    assert!(
        disagreements > 0,
        "a corpus where the two never disagree measures nothing"
    );
}

/// The straw the plan is measured against: does any whitespace token of the
/// line, stripped of shell punctuation, equal `rm`? This is the best a
/// classifier can do without a parse, and it cannot tell argv0 from an
/// argument, a quoted string, or the right-hand side of an assignment.
fn raw_line_says_rm(line: &str) -> bool {
    line.split_whitespace()
        .any(|token| token.trim_matches(|c: char| !c.is_alphanumeric()) == "rm")
}
