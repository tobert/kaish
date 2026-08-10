//! The approval ledger's shell- and VFS-facing surfaces (ledger PR 7,
//! `docs/approval-ledger.md` §D.3): the `/v/approvals` mount, the `approvals`
//! builtin and its authority check, cancellation, and `wait`'s pending
//! count.
//!
//! Everything drives real command strings through `kernel.execute()`, so the
//! full pipeline runs — lex → parse → validate → clap binding → builtin →
//! VFS router. A builtin called directly would skip the arg binding that the
//! subcommand routing depends on, which is exactly the layer these test.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

// KernelConfig::repl() mounts the real filesystem.
#![cfg(feature = "localfs")]

use std::path::Path;
use std::time::Duration;

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::ledger::ApproverHandle;
use kaish_kernel::{Kernel, KernelConfig};
use kaish_types::approval::{GrantTerms, Principal, PrincipalKind, RequestId};

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("approvals-surface-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// A kernel and the authority its construction minted. Whether that authority
/// is *installed on the session* is what the tests below vary — an embedder
/// holding it while the session does not is the agent posture (spec §E.2).
struct Session {
    kernel: Kernel,
    authority: ApproverHandle,
}

impl Session {
    async fn run(&self, script: &str) -> ExecResult {
        self.kernel.execute(script).await.expect("kernel execute")
    }

    async fn out(&self, script: &str) -> String {
        self.run(script).await.text_out().into_owned()
    }

    /// Grant `id` on the terms the request itself declared, for five minutes.
    async fn grant(&self, id: &RequestId) {
        let chain = self.kernel.approvals().get(id).expect("the request's chain");
        let terms = GrantTerms::once_for_view(
            &chain.request,
            std::time::SystemTime::now() + Duration::from_secs(300),
        );
        self.authority
            .grant(id, chain.request.revision, terms)
            .await
            .expect("the grant must post");
    }
}

/// The **agent** posture: the kernel mints an authority, the embedder keeps
/// it, and the session gets none. `approvals grant` must refuse here.
fn agent_session(dir: &Path) -> Session {
    build(
        KernelConfig::repl()
            .with_cwd(dir.to_path_buf())
            .with_approvals(false)
            .with_trash(false),
    )
}

/// The **operator** posture: the same kernel, with the authority installed on
/// the session. `KernelConfig::with_approver_handle` adopts a ledger, so the
/// handle has to come from a first kernel — which is exactly how an embedder
/// hands one session approval rights.
fn operator_session(dir: &Path) -> Session {
    let first = agent_session(dir);
    build(
        KernelConfig::repl()
            .with_cwd(dir.to_path_buf())
            .with_approvals(false)
            .with_trash(false)
            .with_approver_handle(first.authority.clone()),
    )
}

fn build(config: KernelConfig) -> Session {
    let (kernel, authority) = Kernel::build(config).expect("kernel");
    Session { kernel, authority }
}

// ============================================================================
// /v/approvals — the read-only projection
// ============================================================================

/// Spec test 1: `/v/approvals/pending` enumerates gates across *several*
/// background jobs. One `ExecResult` carries one request; the mount is what
/// answers "what else is waiting".
#[tokio::test]
async fn pending_enumerates_gates_across_several_background_jobs() {
    let dir = tempdir();
    let session = agent_session(dir.path());
    for name in ["one.txt", "two.txt", "three.txt"] {
        std::fs::write(dir.path().join(name), "keep me").expect("write");
    }

    session.run("set -o approvals").await;
    for name in ["one.txt", "two.txt", "three.txt"] {
        session.run(&format!("rm {name} &")).await;
        // Let the backgrounded job reach its gate before the next one starts,
        // so all three requests are posted by the time we read the mount.
        session.run("wait").await;
    }

    let body = session.out("cat /v/approvals/pending").await;
    let pending: serde_json::Value = serde_json::from_str(&body).expect("pending is JSON");
    let entries = pending.as_array().expect("pending is an array");
    assert_eq!(entries.len(), 3, "three gated jobs, three pending requests: {body}");

    let jobs: Vec<u64> = entries
        .iter()
        .filter_map(|e| e["job_id"].as_u64())
        .collect();
    assert_eq!(jobs, vec![1, 2, 3], "each request names its own job: {body}");

    // The root lists one directory per request beside the three files.
    let listing = session.out("ls /v/approvals").await;
    for name in ["pending", "standing", "log"] {
        assert!(listing.contains(name), "root must list {name}: {listing}");
    }
    for entry in entries {
        let id = entry["id"].as_str().expect("an id");
        assert!(listing.contains(id), "root must list {id}: {listing}");
    }
}

/// Spec test 2: every write path under `/v/approvals` returns `Unsupported`.
/// Granting by file write would make "the agent can write files" equivalent
/// to "the agent can approve its own operations".
#[tokio::test]
async fn every_write_path_is_unsupported() {
    let dir = tempdir();
    let session = agent_session(dir.path());
    let precious = dir.path().join("precious.txt");
    std::fs::write(&precious, "keep me").expect("write");
    session.run("set -o approvals").await;
    let gated = session.run("rm precious.txt").await;
    let id = gated.approval_request().expect("a gated request").id;

    // The enforce policy off from here on, so what refuses each write is the
    // *mount*, not the gate. With it on, `write /v/approvals/pending` exits 2
    // on an approval request before the VFS is ever reached — the overwrite
    // gate does not know the target sits on a read-only mount. That is a
    // pre-existing shadowing (`/v/jobs` behaves the same way) and out of
    // scope here; see the PR body.
    session.run("set +o approvals").await;

    for script in [
        "write /v/approvals/pending granted".to_string(),
        format!("write /v/approvals/{id}/state granted"),
        format!("write /v/approvals/{id}/grant '{{}}'"),
        "mkdir /v/approvals/req_00000000_1".to_string(),
        "rm /v/approvals/pending".to_string(),
        format!("mv /v/approvals/{id}/request /v/approvals/{id}/grant"),
        format!("touch /v/approvals/{id}/state"),
        "ln -s /v/approvals/pending /v/approvals/link".to_string(),
    ] {
        let result = session.run(&script).await;
        assert_ne!(result.code, 0, "`{script}` must not succeed");
        let said = format!("{} {}", result.err, result.text_out());
        assert!(
            said.contains("read-only") || said.contains("not supported") || said.contains("Unsupported"),
            "`{script}` must refuse loudly and say why, got: {said}"
        );
    }

    // And the refusals changed nothing: the request is still pending and the
    // file is still there.
    let pending = session.out("cat /v/approvals/pending").await;
    assert!(pending.contains(id.as_str()), "the request survives every refused write");
    assert!(precious.exists(), "the gate still holds");
}

/// Spec test 3: no VFS projection contains a credential — asserted by
/// scanning the serialized bytes for the token the ledger actually issued.
/// The public types have no credential field at all (spec §A.2), so this is
/// a guard against a future projection that hand-builds JSON.
#[tokio::test]
async fn no_projection_contains_the_issued_credential() {
    let dir = tempdir();
    let session = operator_session(dir.path());
    let precious = dir.path().join("precious.txt");
    std::fs::write(&precious, "keep me").expect("write");

    session.run("set -o approvals").await;
    let gated = session.run("rm precious.txt").await;
    let id = gated.approval_request().expect("a gated request").id;
    session.grant(&id).await;

    let token = session
        .authority
        .token_for(&id)
        .expect("a credential for a granted request");
    let secret = token.reveal().to_string();
    assert_eq!(secret.len(), 32, "a 128-bit credential in lowercase hex");

    // Redeem it, so `KeyRetrieved`, `Redeemed`, and `Settled` are all on the
    // log — the entries most likely to carry a credential by accident.
    session
        .run(&format!("rm --confirm={secret} precious.txt"))
        .await;

    let mut nodes = vec![
        "/v/approvals/pending".to_string(),
        "/v/approvals/standing".to_string(),
        "/v/approvals/log".to_string(),
    ];
    for suffix in ["request", "state", "attempts", "grant"] {
        nodes.push(format!("/v/approvals/{id}/{suffix}"));
    }
    nodes.push("/v/jobs/1/approval".to_string());

    for node in nodes {
        let body = session.out(&format!("cat {node}")).await;
        assert!(
            !body.contains(&secret),
            "{node} leaked the credential: {body}"
        );
    }

    // The prefix correlator IS on the grant, by design (spec §A.4) — it is
    // four characters, not the key. Pin that it is not the whole thing.
    let grant = session.out(&format!("cat /v/approvals/{id}/grant")).await;
    assert!(grant.contains("token_prefix"), "the grant carries its correlator: {grant}");
    assert!(!grant.contains(&secret));
}

/// The per-request directory answers the four questions §D.3 lists.
#[tokio::test]
async fn a_request_directory_projects_request_state_attempts_and_grant() {
    let dir = tempdir();
    let session = operator_session(dir.path());
    std::fs::write(dir.path().join("precious.txt"), "keep me").expect("write");

    session.run("set -o approvals").await;
    let gated = session.run("rm precious.txt").await;
    let id = gated.approval_request().expect("a gated request").id;

    assert_eq!(
        session.out(&format!("cat /v/approvals/{id}/state")).await.trim(),
        "requested"
    );
    assert!(
        session.out(&format!("cat /v/approvals/{id}/grant")).await.is_empty(),
        "an undecided request projects an empty grant body"
    );
    let request = session.out(&format!("cat /v/approvals/{id}/request")).await;
    assert!(request.contains("fs.remove"), "the request names its operation: {request}");

    session.grant(&id).await;
    assert_eq!(
        session.out(&format!("cat /v/approvals/{id}/state")).await.trim(),
        "granted"
    );
    let grant = session.out(&format!("cat /v/approvals/{id}/grant")).await;
    assert!(!grant.is_empty(), "a decided request projects its grant");
}

/// `log` is NDJSON so a consumer can tail it, and it is seq-ordered.
#[tokio::test]
async fn the_log_node_is_seq_ordered_ndjson() {
    let dir = tempdir();
    let session = operator_session(dir.path());
    std::fs::write(dir.path().join("precious.txt"), "keep me").expect("write");
    session.run("set -o approvals").await;
    let gated = session.run("rm precious.txt").await;
    let id = gated.approval_request().expect("a gated request").id;
    session.grant(&id).await;

    let body = session.out("cat /v/approvals/log").await;
    let mut last = 0u64;
    let mut kinds = Vec::new();
    for line in body.lines().filter(|l| !l.trim().is_empty()) {
        let record: serde_json::Value =
            serde_json::from_str(line).expect("one JSON object per line");
        // Every line is a versioned record (spec §A.5), so a reader knows the
        // schema it is holding and whose scope it belongs to before it looks
        // at the entry.
        assert_eq!(
            record["schema_version"].as_u64(),
            Some(u64::from(kaish_types::approval::LEDGER_SCHEMA_VERSION)),
            "every record names its schema: {line}"
        );
        assert!(record["scope"]["kernel_id"].is_u64(), "every record is scoped: {line}");
        let seq = record["sequence"].as_u64().expect("every record carries a sequence");
        assert!(seq > last, "records are sequence-ordered: {seq} after {last}");
        last = seq;
        let entry = &record["entry"];
        // Every statement also posts the unconditional statement tap (spec
        // §C.6), including the `cat` reading this projection. This test is
        // about the fs chain's ordering, so the taps are filtered out.
        if entry["operation"].as_str() == Some("cmd.execute") {
            continue;
        }
        kinds.push(entry["entry"].as_str().unwrap_or("?").to_string());
    }
    assert_eq!(kinds, vec!["requested", "granted"], "log: {body}");
}

// ============================================================================
// The approvals builtin, and its authority check
// ============================================================================

/// Spec test 4: `approvals grant` is refused with exit 1 in a session without
/// a handle, and permitted in one with it. **The single most important new
/// property** — an agent that can run any shell command cannot approve its
/// own operations.
#[tokio::test]
async fn grant_is_refused_without_authority_and_permitted_with_it() {
    let dir = tempdir();
    let precious = dir.path().join("precious.txt");

    // Without a handle: refused, exit 1, and the file survives.
    std::fs::write(&precious, "keep me").expect("write");
    let agent = agent_session(dir.path());
    agent.run("set -o approvals").await;
    let gated = agent.run("rm precious.txt").await;
    let id = gated.approval_request().expect("a gated request").id;

    let refused = agent.run(&format!("approvals grant {id}")).await;
    assert_eq!(refused.code, 1, "no authority is exit 1, not exit 2: {refused:?}");
    assert!(
        refused.err.contains("no approval authority"),
        "the refusal must name the reason: {}",
        refused.err
    );
    assert_eq!(
        agent.out(&format!("cat /v/approvals/{id}/state")).await.trim(),
        "requested",
        "a refused grant decides nothing"
    );
    assert!(precious.exists());

    // Reading and cancelling stay open to the same session.
    let listed = agent.run("approvals list").await;
    assert_eq!(listed.code, 0, "list needs no authority: {}", listed.err);
    assert!(listed.text_out().contains(id.as_str()));

    // With a handle installed on the session: granted.
    let operator = operator_session(dir.path());
    operator.run("set -o approvals").await;
    let gated = operator.run("rm precious.txt").await;
    let id = gated.approval_request().expect("a gated request").id;
    let granted = operator.run(&format!("approvals grant {id}")).await;
    assert_eq!(granted.code, 0, "an authority-holding session grants: {}", granted.err);
    assert_eq!(
        operator.out(&format!("cat /v/approvals/{id}/state")).await.trim(),
        "granted"
    );
}

/// `deny` and `revoke` are behind the same gate as `grant` — all three are
/// approval-side actions, and a session either has that authority or does not.
#[tokio::test]
async fn deny_and_revoke_need_the_same_authority_grant_does() {
    let dir = tempdir();
    let session = agent_session(dir.path());
    std::fs::write(dir.path().join("precious.txt"), "keep me").expect("write");
    session.run("set -o approvals").await;
    let gated = session.run("rm precious.txt").await;
    let id = gated.approval_request().expect("a gated request").id;

    // The enforce policy off from here on, so what refuses each write is the
    // *mount*, not the gate. With it on, `write /v/approvals/pending` exits 2
    // on an approval request before the VFS is ever reached — the overwrite
    // gate does not know the target sits on a read-only mount. That is a
    // pre-existing shadowing (`/v/jobs` behaves the same way) and out of
    // scope here; see the PR body.
    session.run("set +o approvals").await;

    for script in [format!("approvals deny {id}"), "approvals revoke 1".to_string()] {
        let result = session.run(&script).await;
        assert_eq!(result.code, 1, "`{script}` must exit 1: {result:?}");
        assert!(
            result.err.contains("no approval authority"),
            "`{script}` must name the reason: {}",
            result.err
        );
    }
}

/// Spec §D.3: exactly one builtin bridges to the approval side. Walk the
/// registry and assert `approvals` is the only tool whose schema offers
/// `grant`/`deny`/`revoke` — a second bridge added without this test would
/// silently reopen the hole the authority check closes.
#[tokio::test]
async fn approvals_is_the_only_bridge_to_the_approval_side() {
    let dir = tempdir();
    let session = agent_session(dir.path());

    let mut bridges: Vec<String> = Vec::new();
    for schema in session.kernel.tool_schemas() {
        let offers = schema
            .subcommands
            .iter()
            .any(|sub| matches!(sub.name.as_str(), "grant" | "deny" | "revoke"));
        if offers {
            bridges.push(schema.name.clone());
        }
    }
    assert_eq!(
        bridges,
        vec!["approvals".to_string()],
        "exactly one builtin may reach the approval side"
    );
}

/// `approvals show` reports the decision and the attempt chain, which is what
/// §E asks an auditor to be able to read.
#[tokio::test]
async fn show_reports_the_decision_and_the_attempts() {
    let dir = tempdir();
    let session = operator_session(dir.path());
    std::fs::write(dir.path().join("precious.txt"), "keep me").expect("write");
    session.run("set -o approvals").await;
    let gated = session.run("rm precious.txt").await;
    let id = gated.approval_request().expect("a gated request").id;

    let before = session.out(&format!("approvals show {id}")).await;
    assert!(before.contains("requested"), "{before}");
    assert!(before.contains("fs.remove"), "{before}");
    assert!(before.contains("attempts   none"), "{before}");

    session.grant(&id).await;
    let confirmed = session
        .kernel
        .confirm(&session.authority, &id)
        .await
        .expect("confirm");
    assert_eq!(confirmed.code, 0, "{}", confirmed.err);

    let after = session.out(&format!("approvals show {id}")).await;
    assert!(after.contains("granted by"), "{after}");
    assert!(after.contains("attempt "), "the attempt chain must show: {after}");
}

/// An unknown subcommand and a malformed id are usage errors (exit 2), not
/// failures (exit 1) — the caller mistyped, nothing was attempted.
#[tokio::test]
async fn usage_mistakes_exit_2() {
    let dir = tempdir();
    let session = agent_session(dir.path());
    for script in [
        "approvals",
        "approvals nonesuch",
        "approvals show",
        "approvals show notanid",
        "approvals grant req_9c1a4f2e",
        "approvals list --pending --all",
    ] {
        let result = session.run(script).await;
        assert_eq!(result.code, 2, "`{script}` is a usage error: {result:?}");
    }
}

// ============================================================================
// Cancellation — what replaced expiry (§B.5)
// ============================================================================

/// §B.5: an authority-less session **can** cancel its own request and
/// **cannot** cancel another principal's. Cancellation is a requester action.
#[tokio::test]
async fn a_session_cancels_its_own_request_and_not_another_principals() {
    let dir = tempdir();
    let session = agent_session(dir.path());
    std::fs::write(dir.path().join("precious.txt"), "keep me").expect("write");
    session.run("set -o approvals").await;
    let gated = session.run("rm precious.txt").await;
    let id = gated.approval_request().expect("a gated request").id;
    assert!(
        session.kernel.session_authority().is_none(),
        "this session must hold no approval authority"
    );

    let cancelled = session.run(&format!("approvals cancel {id}")).await;
    assert_eq!(cancelled.code, 0, "a session cancels its own request: {}", cancelled.err);
    assert_eq!(
        session.out(&format!("cat /v/approvals/{id}/state")).await.trim(),
        "cancelled"
    );
    assert!(
        session.kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items.is_empty(),
        "a cancelled request returns its live slot"
    );

    // Now a request raised by someone else, in a session with no authority.
    let other = agent_session(dir.path());
    let other_kernel = Kernel::build(
        KernelConfig::repl()
            .with_cwd(dir.path().to_path_buf())
            .with_approvals(false)
            .with_trash(false)
            .with_approver_handle(other.authority.clone())
            .with_principal(Principal::new("someone-else", PrincipalKind::Agent)),
    )
    .expect("kernel")
    .0;
    std::fs::write(dir.path().join("theirs.txt"), "keep me").expect("write");
    other_kernel.execute("set -o approvals").await.expect("set");
    let theirs = other_kernel.execute("rm theirs.txt").await.expect("rm");
    let theirs_id = theirs.approval_request().expect("a gated request").id;

    // `other` shares the ledger but is the default principal, not
    // "someone-else", and holds no session authority.
    let refused = other.run(&format!("approvals cancel {theirs_id}")).await;
    assert_eq!(
        refused.code, 1,
        "cancelling another principal's request must fail: {refused:?}"
    );
    assert!(
        refused.err.contains("someone-else"),
        "the refusal must name the principal that owns it: {}",
        refused.err
    );
    assert_eq!(
        other.kernel.approvals().state(&theirs_id),
        Some(kaish_types::approval::RequestState::Requested),
        "a refused cancellation closes nothing"
    );
}

/// The other half of the ownership rule: a session **holding this ledger's
/// authority** may cancel any request, not only its own. It could already
/// deny that request, so withholding cancellation from it would be a special
/// case with nothing behind it (spec §B.5).
///
/// Flip `cancel_request`'s `!owned && session_authority.is_none()` to a bare
/// `!owned` and this test exits 1 — it is what pins the authority path, and
/// nothing else does.
#[tokio::test]
async fn an_authority_holding_session_cancels_another_principals_request() {
    let dir = tempdir();
    std::fs::write(dir.path().join("theirs.txt"), "keep me").expect("write");

    // The raiser mints the ledger and asks as "someone-else". It holds no
    // session authority itself — `with_approver_handle` is what installs one,
    // and this kernel is the one being adopted *from*.
    let raiser = build(
        KernelConfig::repl()
            .with_cwd(dir.path().to_path_buf())
            .with_approvals(false)
            .with_trash(false)
            .with_principal(Principal::new("someone-else", PrincipalKind::Agent)),
    );
    raiser.run("set -o approvals").await;
    let gated = raiser.run("rm theirs.txt").await;
    let id = gated.approval_request().expect("a gated request").id;

    let operator = build(
        KernelConfig::repl()
            .with_cwd(dir.path().to_path_buf())
            .with_approvals(false)
            .with_trash(false)
            .with_approver_handle(raiser.authority.clone()),
    );
    assert!(operator.kernel.session_authority().is_some());
    assert_ne!(operator.kernel.principal().id, "someone-else");

    let cancelled = operator.run(&format!("approvals cancel {id}")).await;
    assert_eq!(
        cancelled.code, 0,
        "an authority-holding session cancels another principal's request: {}",
        cancelled.err
    );
    assert_eq!(
        operator.kernel.approvals().state(&id),
        Some(kaish_types::approval::RequestState::Cancelled)
    );
}

/// §B.5: `Cancelled` is terminal for the request and not for the thread of
/// intent. Asking again posts a **new** request with
/// `supersedes: Some(old_id)`, so "this took three attempts" stays legible
/// and the chain stays walkable.
#[tokio::test]
async fn a_cancelled_requests_supersedes_chain_is_walkable() {
    let dir = tempdir();
    let session = agent_session(dir.path());
    std::fs::write(dir.path().join("precious.txt"), "keep me").expect("write");
    session.run("set -o approvals").await;

    // Three asks, each cancelled, each linked to the last.
    let mut ids = Vec::new();
    for _ in 0..3 {
        let gated = session.run("rm precious.txt").await;
        let id = gated.approval_request().expect("a gated request").id;
        if let Some(previous) = ids.last() {
            session
                .kernel
                .approvals()
                .get(&id)
                .expect("chain")
                .request
                .supersedes
                .as_ref()
                .map_or_else(
                    || panic!("the re-ask must link to {previous}"),
                    |linked| assert_eq!(linked, previous),
                );
        }
        session.run(&format!("approvals cancel {id}")).await;
        ids.push(id);
    }

    // Walk it backwards from the newest: every hop lands on a `Cancelled`
    // predecessor, and the walk terminates at the first ask.
    let approvals = session.kernel.approvals();
    let mut cursor = Some(ids[2].clone());
    let mut walked = Vec::new();
    while let Some(id) = cursor {
        let chain = approvals.get(&id).expect("every link is still retained");
        assert_eq!(chain.state, kaish_types::approval::RequestState::Cancelled);
        cursor = chain.request.supersedes.clone();
        walked.push(id);
    }
    assert_eq!(walked, vec![ids[2].clone(), ids[1].clone(), ids[0].clone()]);
}

/// A re-ask is not re-approval: the superseding request starts `Requested`
/// and needs a fresh decision (spec §B.5).
#[tokio::test]
async fn cancelling_a_granted_request_is_refused() {
    let dir = tempdir();
    let session = operator_session(dir.path());
    std::fs::write(dir.path().join("precious.txt"), "keep me").expect("write");
    session.run("set -o approvals").await;
    let gated = session.run("rm precious.txt").await;
    let id = gated.approval_request().expect("a gated request").id;
    session.grant(&id).await;

    let refused = session.run(&format!("approvals cancel {id}")).await;
    assert_eq!(refused.code, 1, "a decided request is not cancellable: {refused:?}");
    assert_eq!(
        session.out(&format!("cat /v/approvals/{id}/state")).await.trim(),
        "granted",
        "the decision stands"
    );
}

/// §C.1: a key presented against a request that is *over* reports the
/// request's own state — `ApprovalOutcome::Closed` — and never
/// `LedgerUnavailable`, which answers a different question ("the ledger
/// could not record this, try again"). Collapsing the two told an embedder
/// to retry a healthy ledger forever.
#[tokio::test]
async fn a_key_presented_against_a_cancelled_request_reports_closed_not_unavailable() {
    let dir = tempdir();
    let session = agent_session(dir.path());
    std::fs::write(dir.path().join("precious.txt"), "keep me").expect("write");
    session.run("set -o approvals").await;
    let gated = session.run("rm precious.txt").await;
    let id = gated.approval_request().expect("a gated request").id;
    session.run(&format!("approvals cancel {id}")).await;

    // 32 lowercase hex, the credential's shape — the key never matters here,
    // because the ledger checks the request's state first.
    let presented = session
        .run("rm precious.txt --confirm=00000000000000000000000000000000")
        .await;

    assert_eq!(presented.code, 1, "a closed request is exit 1, not exit 2: {presented:?}");
    assert!(
        presented.err.contains("Cancelled"),
        "the message must name the request's own state: {}",
        presented.err
    );
    assert!(
        presented.err.contains("ask again"),
        "and say what to do instead: {}",
        presented.err
    );
    assert!(
        !presented.err.contains("ledger unavailable"),
        "nothing is wrong with the ledger — retrying it will never help: {}",
        presented.err
    );
    assert!(
        dir.path().join("precious.txt").exists(),
        "and the operation did not run"
    );
}

// ============================================================================
// wait's pending count
// ============================================================================

/// Spec test 7: `wait` on two gated jobs reports **both** in its message
/// while surfacing **one** on `.approval`.
#[tokio::test]
async fn wait_on_two_gated_jobs_reports_both_and_surfaces_one() {
    let dir = tempdir();
    let session = agent_session(dir.path());
    std::fs::write(dir.path().join("one.txt"), "keep me").expect("write");
    std::fs::write(dir.path().join("two.txt"), "keep me").expect("write");

    session.run("set -o approvals").await;
    session.run("rm one.txt &").await;
    session.run("rm two.txt &").await;

    let waited = session.run("wait").await;
    assert_eq!(waited.code, 2, "two gated jobs wait to exit 2: {waited:?}");
    assert!(
        waited.err.contains("2 approvals pending"),
        "the message must report both: {}",
        waited.err
    );
    assert!(
        waited.err.contains("approvals list"),
        "and point at the surface that enumerates them: {}",
        waited.err
    );

    let surfaced = waited.approval_request().expect("one request on .approval");
    let pending = session.kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items;
    assert_eq!(pending.len(), 2, "both requests are live");
    assert!(
        pending.iter().any(|view| view.id == surfaced.id),
        "the surfaced request is one of the two"
    );

    let listed = session.out("approvals list").await;
    for view in &pending {
        assert!(listed.contains(view.id.as_str()), "list enumerates both: {listed}");
    }
}

/// One gated job says "1 approval pending" — singular, and still pointing at
/// the same surface. A count of one must not read as a different condition.
#[tokio::test]
async fn wait_on_one_gated_job_says_one() {
    let dir = tempdir();
    let session = agent_session(dir.path());
    std::fs::write(dir.path().join("precious.txt"), "keep me").expect("write");
    session.run("set -o approvals").await;
    session.run("rm precious.txt &").await;

    let waited = session.run("wait 1").await;
    assert_eq!(waited.code, 2);
    assert!(
        waited.err.contains("1 approval pending"),
        "singular for one: {}",
        waited.err
    );
}
