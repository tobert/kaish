//! `ToolCtx::request_approval` — the kernel's real implementation
//! (`docs/approval-ledger.md` §C.1, §D.1, ledger PR 3).
//!
//! No `#![cfg(feature = ...)]` gate: `ExecContext::new` here mounts a
//! `MemoryFs` (no real filesystem, no `localfs` needed), and the ledger
//! itself has no OS dependency, so this file compiles and passes
//! featureless.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
// `resolvers_with_git_ref` is a plain helper fn, not itself a `#[test]`, so
// clippy's `allow-{unwrap,expect}-in-tests` does not cover its `.expect()`.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;
use std::time::{Duration, SystemTime};

use kaish_kernel::ledger::{ConditionReport, DecisionChain, Ledger, LedgerConfig, ResolverError, StateResolver, StateResolvers, SystemClock};
use kaish_kernel::vfs::{MemoryFs, VfsRouter};
use kaish_kernel::{ExecContext, LedgerAccess};
use kaish_tool_api::{ApprovalOutcome, Tool, ToolArgs, ToolCtx};
use kaish_types::approval::{
    ApprovalRequest, Capture, GrantTerms, Observation, Outcome, Principal, PrincipalKind, RequestState,
    ResourceRef, RiskClass, StateClaim,
};

/// This file's ledger scope (spec §A.7): a fresh kernel id per ledger, and
/// no session — an unscoped ledger is the single-session shape.
#[allow(dead_code)]
fn test_scope() -> kaish_types::approval::ApprovalScope {
    kaish_types::approval::ApprovalScope::kernel(kaish_types::approval::KernelId::mint())
}

/// The origin a request posted by this file is stamped with (spec §A.7,
/// §A.9). One fixed binding: these tests exercise the state machine, not the
/// replay rules.
#[allow(dead_code)]
fn test_origin(principal: kaish_types::approval::Principal) -> kaish_types::approval::RequestOrigin {
    let scope = test_scope();
    kaish_types::approval::RequestOrigin::new(
        scope.clone(),
        kaish_types::approval::PlanBinding::new(
            kaish_types::approval::PlanDigest::new("test"),
            "/",
            scope,
        ),
        principal,
        kaish_types::approval::Capture::DirectExecution)
}


/// Resolves `git.ref` to whatever the fixture's own transition claims as its
/// prior state — just enough to let `present_key`'s redemption-time
/// precondition check (spec §B.4) pass for the confirm-token tests below.
/// Not a real git backend; a real out-of-tree plugin brings its own resolver.
struct FixtureGitRefResolver;

#[async_trait::async_trait]
impl StateResolver for FixtureGitRefResolver {
    fn kind(&self) -> &str {
        "git.ref"
    }

    async fn observe(&self, _id: &str) -> Result<StateClaim, ResolverError> {
        Ok(StateClaim::Exact("a1b2c3d".to_string()))
    }
}

fn resolvers_with_git_ref() -> Arc<StateResolvers> {
    Arc::new(
        StateResolvers::from_registrations(vec![Arc::new(FixtureGitRefResolver)])
            .expect("git.ref is not the reserved path kind"),
    )
}

fn ctx_with_memory_fs() -> ExecContext {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    ExecContext::new(Arc::new(vfs))
}

fn agent(id: &str) -> Principal {
    Principal::new(id, PrincipalKind::Agent)
}

/// A decision chain with no `Policy` installed — stages 1 and 3 only, which
/// is a kernel with no decision hook: no standing rule means Defer means
/// exit 2 (spec §C.2).
fn chain_over(approver: &kaish_kernel::ledger::ApproverHandle) -> std::sync::Arc<DecisionChain> {
    let (_, approvals, authority) = approver.join();
    std::sync::Arc::new(DecisionChain::new(authority, approvals, None))
}

#[tokio::test]
async fn kernel_request_approval_round_trips_a_request_through_the_ledger() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let mut ctx = ctx_with_memory_fs();
    ctx.ledger_access = Some(LedgerAccess {
        requester,
        approvals: approvals.clone(),
        chain: chain_over(&approver),
        principal: agent("agent-1"),
        scope: test_scope(),
        job_id: None,
        resolvers: std::sync::Arc::new(kaish_kernel::ledger::StateResolvers::default()),
        session_authority: None,
    });

    let draft = ApprovalRequest::builder("plugin.dangerous")
        .risk(RiskClass::Irreversible)
        .reason("round-trip test")
        .build()
        .unwrap();

    let outcome = ctx.request_approval(draft, None).await;
    let pending = match outcome {
        ApprovalOutcome::Pending(pending) => *pending,
        other => panic!("nothing decides here — every post must defer to Pending, got {other:?}"),
    };
    let view = pending.request;
    assert_eq!(view.operation.as_str(), "plugin.dangerous");
    assert_eq!(
        view.principal,
        agent("agent-1"),
        "the stamped principal must be the context's own, not left default"
    );
    assert_eq!(
        view.capture,
        Capture::DirectExecution,
        "no dispatch seam ran above this call — it must capture as DirectExecution, not a fabricated Exact"
    );

    // The ledger itself has the request, independent of the view returned
    // to the caller.
    assert_eq!(approvals.state(&view.id), Some(RequestState::Requested));
}

#[tokio::test]
async fn kernel_request_approval_with_no_ledger_wired_is_unsupported() {
    // Today's only production value: no `KernelConfig::with_ledger` exists
    // yet (PR 4), so `ledger_access` is always `None` outside a test.
    let mut ctx = ctx_with_memory_fs();
    assert!(ctx.ledger_access.is_none());

    let draft = ApprovalRequest::builder("plugin.dangerous")
        .risk(RiskClass::Irreversible)
        .build()
        .unwrap();
    let outcome = ctx.request_approval(draft, None).await;
    assert!(matches!(outcome, ApprovalOutcome::Unsupported));
}

/// Fixture tool depending on ONLY `kaish-tool-api` (plus the `kaish-types`
/// vocabulary that crate already re-exports/depends on) — the acceptance
/// criterion for ledger PR 3 (spec §H): "if the fixture needs
/// `kaish-kernel` or `as_any_mut`, the PR is not done." Every `use` inside
/// this module names `kaish_tool_api`/`kaish_types`/`async_trait` only; the
/// surrounding test (which drives a real kernel `ExecContext` and ledger)
/// is not part of the fixture itself.
mod plugin_dangerous {
    use async_trait::async_trait;
    use kaish_tool_api::{ExecResult, Tool, ToolArgs, ToolCtx, ToolSchema};
    use kaish_types::approval::{ApprovalRequest, Resource, RiskClass, StateClaim};
    use kaish_types::Value;

    /// The transition this fixture claims — public so the surrounding test
    /// can build a matching `Observation` at redemption time without
    /// duplicating the literal claim.
    pub fn ref_resource() -> Resource {
        Resource::transition(
            "git.ref",
            "refs/heads/agent/dangerous",
            StateClaim::Exact("a1b2c3d".to_string()),
            StateClaim::Exact("d4e5f6a".to_string()),
        )
    }

    /// A synthetic `plugin.dangerous` operation — stands in for a real
    /// out-of-tree plugin (kaish-git's own gated operations) that has no
    /// `kaish-kernel` dependency at all. Declares one transition-bearing
    /// resource so the end-to-end test exercises §B.4's redemption-time
    /// condition check, not just the trivial no-resources case.
    pub struct PluginDangerous;

    #[async_trait]
    impl Tool for PluginDangerous {
        fn name(&self) -> &str {
            "plugin-dangerous"
        }

        fn schema(&self) -> ToolSchema {
            ToolSchema::new("plugin-dangerous", "fixture: gates a synthetic dangerous operation")
        }

        async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
            let draft = match ApprovalRequest::builder("plugin.dangerous")
                .risk(RiskClass::Irreversible)
                .resource(ref_resource())
                .reason("fixture: gates a synthetic dangerous operation")
                .hint("plugin-dangerous --confirm=<token>")
                .build()
            {
                Ok(draft) => draft,
                Err(e) => return ExecResult::failure(1, e.to_string()),
            };
            // `--confirm=<token>` relayed from argv, exactly the way a real
            // out-of-tree plugin (kaish-git) would read its own flag and pass
            // it through — this is the fixture proving `ToolCtx::request_approval`
            // carries `presented` (ledger PR "spec-gaps" item 1).
            let presented = match args.named.get("confirm") {
                Some(Value::String(token)) => Some(token.as_str()),
                _ => None,
            };
            // The one call pattern (spec §C.1) — the entire gate.
            match ctx.request_approval(draft, presented).await.proceed() {
                Ok(_attempt) => ExecResult::success("dangerous operation performed"),
                Err(result) => result,
            }
        }
    }
}

#[tokio::test]
async fn plugin_dangerous_fixture_gates_end_to_end_through_tool_api_alone() {
    use plugin_dangerous::PluginDangerous;

    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let mut ctx = ctx_with_memory_fs();
    ctx.ledger_access = Some(LedgerAccess {
        requester: requester.clone(),
        approvals: approvals.clone(),
        chain: chain_over(&approver),
        principal: agent("agent-1"),
        scope: test_scope(),
        job_id: None,
        resolvers: std::sync::Arc::new(kaish_kernel::ledger::StateResolvers::default()),
        session_authority: None,
    });
    let tool = PluginDangerous;

    // 1. Request + defer: the fixture's one call posts to the ledger and
    //    returns exit 2. Nothing above this line or in the fixture itself
    //    names `kaish-kernel`.
    let result = tool.execute(ToolArgs::new(), &mut ctx).await;
    assert_eq!(result.code, 2, "a deferred request must be exit 2, not a bare failure");
    let view = result
        .approval_request()
        .expect("Pending must post the view on ExecResult's control-plane field");
    assert_eq!(view.operation.as_str(), "plugin.dangerous");
    assert_eq!(approvals.state(&view.id), Some(RequestState::Requested));

    // 2. Out-of-band grant, via the approval side only — spec §D.3: exactly
    //    one bridge from script/tool code to `grant` exists (the
    //    `approvals` builtin, PR 7), and a plugin has no path to this
    //    `ApproverHandle` at all. `GrantTerms::once_for` needs an
    //    `ApprovalRequest`, not the tokenless view the fixture got back;
    //    re-stamping an equivalent one from the view's own fields — this
    //    MUST include `view.resources`, not just operation/risk, or
    //    `once_for`'s derived conditions silently narrow to nothing and the
    //    real ledger's `find_widened_condition` check (§A.4) would reject
    //    the grant the moment the fixture declares a real resource (a gap a
    //    review round caught: the fixture originally declared none, so the
    //    omission was invisible) — is the legitimate way to reach an
    //    `ApprovalRequest` from outside the ledger crate.
    let mut terms_draft = ApprovalRequest::builder("plugin.dangerous")
        .risk(RiskClass::Irreversible)
        .build()
        .unwrap();
    terms_draft.resources = view.resources.clone();
    let terms_source = terms_draft.stamp(
        view.id.clone(),
        view.requested_at,
        kaish_types::approval::RequestOrigin::new(
            view.scope.clone(),
            view.binding.clone(),
            view.principal.clone(),
            view.capture.clone())
        .with_parent(view.parent.clone())
        .with_context(view.context.clone())
        .with_job_id(view.job_id),
    );
    let not_after = SystemTime::now() + Duration::from_secs(300);
    approver
        .grant(&view.id, view.revision, GrantTerms::once_for(&terms_source, not_after))
        .await
        .unwrap();
    assert_eq!(approvals.state(&view.id), Some(RequestState::Granted));

    // 3. "Confirm-replay": `Kernel::confirm` (the automated replay of the
    //    captured invocation) is ledger PR 5 — it needs the `Capture`-based
    //    draft matcher to correlate a fresh re-invocation back to this
    //    request, which has no gate site to call it from yet (see this
    //    PR's decisions list). Driven directly through the ledger handles
    //    here, standing in for what PR 5 will automate. The observation
    //    proves the redemption-time condition check (§B.4) actually runs
    //    against the fixture's declared transition, not an unconditioned
    //    no-op grant.
    let observed = vec![Observation {
        resource: ResourceRef {
            kind: "git.ref".to_string(),
            id: "refs/heads/agent/dangerous".to_string(),
        },
        claim: StateClaim::Exact("a1b2c3d".to_string()),
        at: SystemTime::now(),
    }];
    let attempt = requester.redeem(&view.id, agent("agent-1"), ConditionReport::observed(observed)).await.unwrap();

    // 4. Settle.
    let appended = requester.settle(&attempt, Outcome::Exit(0)).await.unwrap();
    assert!(appended, "settle must append — this is the attempt's first report");

    let chain = approvals.get(&view.id).expect("chain must still exist after settlement");
    assert_eq!(chain.attempts.len(), 1);
    assert_eq!(chain.attempts[0].outcome, Some(Outcome::Exit(0)));
}

#[tokio::test]
async fn plugin_dangerous_fixture_honors_a_presented_confirm_token() {
    // Ledger PR "spec-gaps" item 1: a plugin tool now has a path to relay
    // its own `--confirm=<token>` the way an in-tree gate site does. Before
    // this PR `ToolCtx::request_approval` took no `presented` parameter at
    // all, so this flow had no way to reach `Authorized` on the same
    // invocation shape as `rm --confirm=<token>` (`execute_argv_tests.rs`).
    use plugin_dangerous::PluginDangerous;

    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let mut ctx = ctx_with_memory_fs();
    ctx.ledger_access = Some(LedgerAccess {
        requester,
        approvals: approvals.clone(),
        chain: chain_over(&approver),
        principal: agent("agent-1"),
        scope: test_scope(),
        job_id: None,
        resolvers: resolvers_with_git_ref(),
        session_authority: None,
    });
    let tool = PluginDangerous;

    // 1. First invocation, no `--confirm`: posts and defers, same as the
    //    plain end-to-end test above.
    let result = tool.execute(ToolArgs::new(), &mut ctx).await;
    assert_eq!(result.code, 2, "a deferred request must be exit 2, not a bare failure");
    let view = result
        .approval_request()
        .expect("Pending must post the view on ExecResult's control-plane field")
        .clone();

    // 2. Grant it, out of band, exactly as an approver would.
    let mut terms_draft = ApprovalRequest::builder("plugin.dangerous")
        .risk(RiskClass::Irreversible)
        .build()
        .unwrap();
    terms_draft.resources = view.resources.clone();
    let terms_source = terms_draft.stamp(
        view.id.clone(),
        view.requested_at,
        kaish_types::approval::RequestOrigin::new(
            view.scope.clone(),
            view.binding.clone(),
            view.principal.clone(),
            view.capture.clone())
        .with_parent(view.parent.clone())
        .with_context(view.context.clone())
        .with_job_id(view.job_id),
    );
    let not_after = SystemTime::now() + Duration::from_secs(300);
    approver
        .grant(&view.id, view.revision, GrantTerms::once_for(&terms_source, not_after))
        .await
        .unwrap();

    // 3. Retrieve the real credential — the thing a frontend would splice
    //    into the printed re-run hint (`plugin-dangerous --confirm=<token>`).
    let token = approver.token_for(&view.id).expect("a granted request has a credential").reveal().to_string();

    // 4. Re-invoke the fixture with `--confirm=<token>` relayed through its
    //    own argv (`args.named["confirm"]`), the way a real out-of-tree
    //    plugin would parse its own flags and hand the value to
    //    `ToolCtx::request_approval`'s new `presented` parameter.
    let mut args = ToolArgs::new();
    args.named.insert("confirm".to_string(), kaish_types::Value::String(token));
    let confirmed = tool.execute(args, &mut ctx).await;
    assert_eq!(
        confirmed.code, 0,
        "a correct presented token must authorize the operation, got: {}",
        confirmed.err
    );
    let chain = approvals.get(&view.id).expect("chain must still exist after redemption");
    assert_eq!(chain.attempts.len(), 1, "the presented token must have reserved exactly one attempt");
}

#[tokio::test]
async fn plugin_dangerous_fixture_rejects_a_wrong_presented_confirm_token() {
    // The other half: a plugin relaying a bad token must fail closed, not
    // silently fall through to the operation.
    use plugin_dangerous::PluginDangerous;

    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let mut ctx = ctx_with_memory_fs();
    ctx.ledger_access = Some(LedgerAccess {
        requester,
        approvals: approvals.clone(),
        chain: chain_over(&approver),
        principal: agent("agent-1"),
        scope: test_scope(),
        job_id: None,
        resolvers: resolvers_with_git_ref(),
        session_authority: None,
    });
    let tool = PluginDangerous;

    let mut args = ToolArgs::new();
    args.named.insert(
        "confirm".to_string(),
        kaish_types::Value::String("0000000000000000000000000000000000000000".to_string()),
    );
    let result = tool.execute(args, &mut ctx).await;
    assert_eq!(result.code, 1, "a wrong presented token must fail closed, not proceed");
}
