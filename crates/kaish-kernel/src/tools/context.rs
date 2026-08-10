//! Execution context for tools.

use std::collections::HashMap;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;

use async_trait::async_trait;
use kaish_types::approval::{
    ApprovalAssessment, ApprovalRequest, ApprovalRequestDraft, ApprovalScope, AssessmentOutcome,
    AssessmentStage, AssessorId, AttemptId, CancelReason, Capture, Condition,
    Invocation, KernelId, Observation, ObservedResource, OperationId, Outcome, Plan, PlanBinding,
    PlanDigest,
    Principal, RequestId, RequestState, Resource, ResourceRef,
};
use sha2::{Digest, Sha256};
use kaish_tool_api::{
    ExecutionContext, StatementAssessment, StatementClassificationInput, StatementClassifier,
    StatementPosture,
};

use crate::ast::Value;
use crate::backend::{KernelBackend, LocalBackend};
use crate::dispatch::PipelinePosition;
use crate::ignore_config::IgnoreConfig;
use crate::interpreter::{ExecResult, Scope};
use crate::ledger::{
    static_gate_floor, Approvals, AttemptGuard, ChainContext, ChainOutcome, ConditionReport,
    DecisionChain, KernelOperation, LedgerError, PathResolver, Posture, Requester, StateResolver,
    StateResolvers, SubscriptionFilter, PATH_KIND,
};
use crate::output_limit::OutputLimitConfig;
use crate::scheduler::{JobManager, PipeReader, PipeWriter, StderrStream};
use crate::tools::ToolRegistry;
use crate::trash::TrashBackend;
use crate::vfs::VfsRouter;
use kaish_vfs::ByteBudget;
use tokio::sync::oneshot;
use tokio_util::sync::CancellationToken;

use crate::interpreter::OutputFormat;

use super::traits::ToolSchema;

/// Output context determines how command output should be formatted.
///
/// Different contexts prefer different output formats:
/// - **Interactive** — Pretty columns, colors, traditional tree (TTY/REPL)
/// - **Piped** — Raw output for pipeline processing
/// - **Model** — Token-efficient compact formats (MCP server / agent context)
/// - **Script** — Non-interactive script execution
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OutputContext {
    /// Interactive TTY/REPL - use human-friendly format with colors.
    #[default]
    Interactive,
    /// Output to another command - use raw output for pipes.
    Piped,
    /// MCP server / agent context - use token-efficient model format.
    Model,
    /// Non-interactive script - use raw output.
    Script,
}

/// Execution context passed to tools.
///
/// Provides access to the backend (for file operations and tool dispatch),
/// scope, and other kernel state.
pub struct ExecContext {
    /// Kernel backend for I/O operations.
    ///
    /// This is the preferred way to access filesystem operations.
    /// Use `backend.read()`, `backend.write()`, etc.
    pub backend: Arc<dyn KernelBackend>,
    /// Variable scope.
    pub scope: Scope,
    /// Current working directory (VFS path).
    pub cwd: PathBuf,
    /// Previous working directory (for `cd -`).
    pub prev_cwd: Option<PathBuf>,
    /// Standard input for the tool (from a redirect, heredoc, here-string, or
    /// `ExecuteOptions::stdin`). Bytes-typed (GH #176) so a `< binfile`
    /// redirect over non-UTF-8 content reaches a byte-aware builtin intact
    /// instead of erroring at redirect setup; a text-only builtin still
    /// refuses it loudly when it calls `read_stdin_to_text`.
    pub stdin: Option<Vec<u8>>,
    /// Structured data from pipeline (pre-parsed JSON from previous command).
    /// Tools can check this before parsing stdin to avoid redundant JSON parsing.
    pub stdin_data: Option<Value>,
    /// Sideband receiver for the previous stage's structured `.data`, set by the
    /// concurrent pipeline runner. Resolved lazily via [`Self::resolve_stdin`]
    /// AFTER the pipe is drained — never pre-read — so a streaming upstream that
    /// only sends its data after writing the pipe can't deadlock a consumer that
    /// awaits it. Non-`Clone`, so it's moved on resolve.
    pub stdin_data_rx: Option<oneshot::Receiver<Option<Value>>>,
    /// Streaming pipe input (set when this command is in a concurrent pipeline).
    pub pipe_stdin: Option<PipeReader>,
    /// Streaming pipe output (set when this command is in a concurrent pipeline).
    pub pipe_stdout: Option<PipeWriter>,
    /// Tool schemas for help command.
    ///
    /// `Arc<[…]>` rather than `Vec`: the full builtin schema catalog (~70
    /// entries, each with its own `Vec`s and `String`s) is snapshotted into a
    /// fresh `ExecContext` at every command dispatch and pipeline/fork child. As
    /// a `Vec` that was a deep clone of the whole catalog per command; as an
    /// `Arc<[…]>` it's a refcount bump (GH #48, item 8). Immutable after the
    /// kernel seeds it, so a shared slice is the right shape.
    pub tool_schemas: Arc<[ToolSchema]>,
    /// Tool registry reference (for tools that need to inspect available tools).
    pub tools: Option<Arc<ToolRegistry>>,
    /// Job manager for background jobs (optional).
    pub job_manager: Option<Arc<JobManager>>,
    /// Kernel stderr stream for real-time error output from pipeline stages.
    ///
    /// When set, pipeline stages write stderr here instead of buffering in
    /// `ExecResult.err`. This allows stderr from all stages to stream to
    /// the terminal (or other sink) concurrently, matching bash behavior.
    pub stderr: Option<StderrStream>,
    /// Position of this command within a pipeline (for stdio decisions).
    pub pipeline_position: PipelinePosition,
    /// Whether we're running in interactive (REPL) mode.
    pub interactive: bool,
    /// Arm `PR_SET_PDEATHSIG(SIGKILL)` on external commands spawned from this
    /// context, so a hard-killed kaish process cannot orphan them.
    ///
    /// Seeded from `KernelConfig::kill_children_on_parent_death` — read that
    /// field for the tradeoff and the macOS gap. It lives here, not on the
    /// `Kernel`, because both external-command spawn sites (`Kernel::
    /// try_execute_external` and `dispatch.rs`'s `BackendDispatcher`) reach an
    /// `ExecContext` and only one of them reaches a `Kernel`; one home keeps
    /// the two `pre_exec` blocks from drifting.
    ///
    /// `false` for a stand-alone `ExecContext` built outside a kernel, which
    /// is the pre-existing behavior.
    pub kill_children_on_parent_death: bool,
    /// Command aliases (name → expansion string).
    pub aliases: HashMap<String, String>,
    /// Ignore file configuration for file-walking tools.
    pub ignore_config: IgnoreConfig,
    /// Output size limit configuration for agent safety.
    pub output_limit: OutputLimitConfig,
    /// Whether external command execution is allowed.
    ///
    /// When `false`, external commands (PATH lookup, `exec`, `spawn`) are blocked.
    /// Only kaish builtins and backend-registered tools (MCP) are available.
    pub allow_external_commands: bool,
    /// Trash backend for safe file deletion.
    ///
    /// Always present when the kernel creates the context (even if `set -o trash`
    /// is off — the backend exists so `kaish-trash list/restore/empty` work
    /// regardless of the trash flag).
    pub trash_backend: Option<Arc<dyn TrashBackend>>,
    /// Terminal state for job control (interactive mode, Unix only).
    #[cfg(all(unix, feature = "subprocess"))]
    pub terminal_state: Option<std::sync::Arc<crate::terminal::TerminalState>>,
    /// Command dispatcher for re-dispatching through the full resolution chain.
    ///
    /// When set (via `Kernel::into_arc()`), builtins like `timeout` can dispatch
    /// inner commands through the full chain (user tools → builtins → .kai scripts
    /// → external commands) instead of being limited to `backend.call_tool()`.
    ///
    /// `None` when the Kernel was not wrapped via `into_arc()`.
    pub dispatcher: Option<Arc<dyn crate::dispatch::CommandDispatcher>>,
    /// Cancellation token for this execution path.
    ///
    /// Populated by the kernel at execute entry, then propagated through pipeline
    /// stages, foreground forks (scatter workers, concurrent pipeline stages,
    /// `$(...)` cmdsubs), and into spawned external children. When the token
    /// fires, externals receive SIGTERM/SIGKILL via the `wait_or_kill` helper.
    ///
    /// Default for stand-alone `ExecContext` constructors is a fresh, never-fired
    /// token so non-kernel test contexts behave as before.
    pub cancel: CancellationToken,
    /// Per-execution output format override set by a builtin's GlobalFlags
    /// flatten (e.g. `--json`). The dispatcher reads this after `tool.execute()`
    /// returns and applies the format via `apply_output_format`.
    ///
    /// Builtins set this via `GlobalFlags::apply(ctx)`; external commands
    /// don't touch it.
    pub output_format: Option<OutputFormat>,

    /// The command currently executing, captured at the dispatch seam as
    /// `(dispatch_name, argv)` where `argv` is the canonical `ToolArgs::to_argv`.
    /// A gate site stamps this into the request's `Capture` so
    /// `Kernel::confirm` can replay the *exact* invocation — no re-parsing of
    /// the human `hint`. `None` for a direct `tool.execute` call in a unit
    /// test (no dispatch seam ran), which records `Capture::DirectExecution`
    /// rather than a silently empty argv.
    ///
    /// Boxed to keep `ExecContext` lean: it is cloned/rebuilt at every recursion
    /// level (pipeline stages, `$(...)`, functions), so an inline `(String,
    /// Vec<String>)` would grow every frame and eat into the interpreter's stack
    /// headroom (see GH #46/#47). The box is allocated once per gated command.
    pub current_invocation: Option<Box<(String, Vec<String>)>>,

    /// Shared VFS memory budget for this kernel's `MemoryFs` mounts.
    ///
    /// `Arc`-cloned from the owning `Kernel` (or its fork parent) so all
    /// concurrent execution paths draw from the same pool. `None` means
    /// unbounded. Populated by `Kernel::assemble` and forwarded through
    /// `child_for_pipeline` / `fork_inner` so background jobs and scatter
    /// workers see the same cap as foreground execution.
    pub vfs_budget: Option<Arc<ByteBudget>>,

    /// The per-execute timeout watchdog, when a script timeout is in effect.
    ///
    /// Populated by the kernel at execute entry (alongside `cancel`) and
    /// shared through `child_for_pipeline` so forks and pipeline stages can
    /// acquire patient holds against the same script clock. `None` when no
    /// timeout is configured — `ToolCtx::patient` then returns an inert guard.
    pub watchdog: Option<Arc<crate::watchdog::Watchdog>>,

    /// Active overlay handle when the kernel was constructed with `overlay: true`.
    ///
    /// `Arc`-cloned so forks and pipeline stages share the same transaction.
    /// `None` when no overlay is active (most kernels).
    #[cfg(all(feature = "localfs", feature = "overlay"))]
    pub overlay_handle: Option<Arc<crate::kernel::OverlayHandle>>,

    /// What this context needs to post approval requests against a real
    /// ledger (`docs/approval-ledger.md`, ledger PR 3). `None` — today's only
    /// value in production — is what makes `ToolCtx::request_approval`
    /// return `Unsupported`: no `KernelConfig::with_ledger` exists yet
    /// (that builder is PR 4), so nothing sets this outside a test.
    pub ledger_access: Option<LedgerAccess>,

    /// Kernel-internal replay correlation (`docs/approval-ledger.md` §B.4).
    /// `Kernel::confirm` reserves the attempt against an already-granted
    /// request and stamps it here before dispatching the captured
    /// invocation; the gate site's fresh draft is then *matched* against
    /// that request rather than posting a second one. Consumed by the first
    /// gate it reaches, so a replayed command with two gates cannot reuse
    /// one authorization twice.
    ///
    /// Never crosses a public API and never reaches a tool.
    pub(crate) redemption: Option<RedemptionContext>,

    /// Why capturing this invocation failed, when it did (spec §B.4). Set at
    /// the dispatch seam when the invocation cannot be captured verbatim;
    /// a request posted with this set records `Capture::CaptureFailed`, and
    /// `confirm` refuses to replay it naming the variant.
    pub(crate) capture_failure: Option<String>,

    /// The request currently being satisfied above this one, if any (spec
    /// §A.7). The statement gate sets it when it authorizes a statement, so
    /// an `fs.*` gate underneath names the statement's request as `parent`
    /// and a UI can render one nested prompt instead of two unrelated ones.
    ///
    /// Propagated to pipeline stages and forks — a nested gate is nested
    /// however deep the execution went to reach it — and cleared when the
    /// statement settles. Parenthood is a display and audit relationship
    /// only: a grant on a parent never authorizes a child (spec §A.7).
    pub(crate) gate_parent: Option<RequestId>,

    /// Attempts reserved during this invocation, each in its drop-safe
    /// guard (spec §C.1). The dispatch seam settles them with the real exit
    /// code when `tool.execute()` returns; dropping this context instead —
    /// a cancelled future, a panic — settles them `Unknown{Cancelled}`,
    /// because a tool that was interrupted may already have written.
    ///
    /// Also the ownership record `settle_with` checks: a tool may report an
    /// outcome for an attempt *this* context reserved and no other.
    pub(crate) attempts: Vec<AttemptGuard>,
}

/// The resource kind a statement's commands are named under (spec §C.6):
/// one `cmd` resource per planned command, matched by a standing grant with
/// the same exact-kind/globbed-id rule everything else uses.
pub(crate) const CMD_KIND: &str = "cmd";

/// What the statement tap decided (spec §C.6). See
/// [`ExecContext::tap_statement`].
pub(crate) enum StatementTap {
    /// Run the statement. `gated` is true when a gate authorized it, and
    /// the caller must settle the reserved attempt with the statement's exit
    /// code once it finishes.
    Proceed {
        /// Whether an attempt is reserved and awaiting settlement.
        gated: bool,
    },
    /// Return this result verbatim. **Nothing of the statement has run** —
    /// no substitution, no redirect opened, no first loop iteration.
    Halt(Box<ExecResult>),
}

/// What a redemption path decided: it produced an outcome, or the operation
/// has moved out of the context its grant was decided in and must ask again
/// (spec §A.9).
///
/// A distinct type rather than a sentinel outcome, because the two are not
/// the same answer: `Authorized` is final and the caller returns it, and
/// `Rebind` means "keep going, post a fresh request" — the difference between
/// refusing an operation and re-asking for it.
enum Rebind {
    /// The redemption path reached a decision. Return it.
    Authorized(kaish_tool_api::ApprovalOutcome),
    /// The binding moved. Nothing was redeemed; post a new request.
    Rebind,
}

/// Kernel-internal correlation between a replay and the request it fulfills
/// (`docs/approval-ledger.md` §B.4). Never crosses a public API, never
/// reaches a tool.
#[derive(Debug, Clone)]
pub(crate) struct RedemptionContext {
    /// The granted request being replayed.
    pub request_id: RequestId,
    /// The attempt `Kernel::confirm` already reserved against it.
    pub attempt_id: AttemptId,
}

/// Everything [`ExecContext`]'s `ToolCtx::request_approval` needs to post a
/// request against a real ledger (`docs/approval-ledger.md` §D.1, ledger PR
/// 3). Grouped into one field rather than four scattered ones so a future
/// `KernelConfig::with_ledger` (PR 4) has a single seam to populate.
#[derive(Clone)]
pub struct LedgerAccess {
    /// The obligations handle — posts `Requested`/`Redeemed`/`Settled`.
    pub requester: Requester,
    /// The read side, for `ToolCtx::approvals`.
    pub approvals: Approvals,
    /// The decision chain (spec §C.2) a fresh request runs
    /// through. Holds the ledger's authority internally; nothing reachable
    /// from script or tool code can get that authority back out of it.
    pub chain: Arc<DecisionChain>,
    /// The backgrounded job this context runs on behalf of, stamped onto
    /// every request it posts. **The one stamping site** — `Job::approval()`
    /// and `wait` both read it back off the record rather than each
    /// re-stamping their own copy.
    pub job_id: Option<u64>,
    /// Who this context's requests are attributed to. Set by
    /// `KernelConfig::with_principal`.
    pub principal: Principal,
    /// Which kernel, session, and actor this context's requests belong to
    /// (spec §A.7). Stamped onto every request and every `Observed` entry
    /// this context posts, and recorded in the [`PlanBinding`] a redemption
    /// must still match. Set by `KernelConfig::with_session`; a
    /// single-session kernel like the REPL carries a kernel id and nothing
    /// else.
    pub scope: ApprovalScope,
    /// The approval authority **this session** holds, if any (spec §D.3).
    /// `None` means the session may not approve anything, and `approvals
    /// grant` exits 1 naming that.
    ///
    /// The `approvals` builtin is the only reader, and the registry test in
    /// `approvals_builtin_tests.rs` is what keeps it that way. This is a
    /// deliberate widening of what a builtin can reach: the type-system tier
    /// of the enforcement ladder (§E.2, tier 1) stops at the crate boundary,
    /// and inside the crate the boundary is the one builtin that reads this
    /// field. The threat model says so explicitly — the ledger does not
    /// defend against hostile Rust compiled into the process (§A.2).
    pub session_authority: Option<crate::ledger::ApproverHandle>,
    /// The embedder- and plugin-registered [`StateResolver`]s a redemption
    /// consults for non-`path` resource kinds (spec §B.4). `path` is not in
    /// here — it is served by a [`PathResolver`] built from *this* context's
    /// backend and cwd, so a request posted inside an overlay is re-observed
    /// through the same overlay.
    pub resolvers: Arc<StateResolvers>,
}

/// What the write-model gate chose for a single truncating overwrite.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MutationAction {
    /// Write now — new file, append, excluded path, or both gates off.
    Proceed,
    /// Snapshot the prior content to trash, then write.
    TrashFirst,
    /// Hold behind an approval request (exit 2 until it is granted).
    Gate,
}

/// What a gated overwrite must still find at the target before it writes —
/// the compare-and-swap expectation `overwrite_checked` enforces.
///
/// Two forms, because the two gate paths hold different things. The trash
/// path already has the prior bytes (it had to copy them to the trash), so it
/// compares bytes. The approval path never holds the content — it only
/// digested it for the ledger condition — so it compares digests, which also
/// bounds its memory: a 10 GiB target costs one 256 KiB window, not 10 GiB.
/// That matters, because the oversize file the trash cannot snapshot is
/// exactly the file that falls through to the approval gate.
#[derive(Debug, Clone)]
pub enum OverwriteExpectation {
    /// The exact prior bytes, from the trash snapshot.
    Bytes(Vec<u8>),
    /// The prior content's digest, re-derived through the backend at write
    /// time. The same claim the ledger holds as the grant's condition.
    Digest(kaish_types::approval::StateClaim),
}

/// What each gated target must still look like when the caller writes it,
/// keyed by resolved path (see `overwrite_checked`).
///
/// Every existing target the gate held appears — trash-snapshotted or
/// approval-gated. A new file, an append, and an excluded or ungated path are
/// absent, because none of them has prior content to lose.
pub type GateExpectations = std::collections::HashMap<PathBuf, OverwriteExpectation>;

/// Real paths the trash gate skips: host scratch under `/tmp`, where
/// snapshotting prior content to trash is pointless. Shared by `rm`'s delete
/// gate (`decide_rm_action`) and the overwrite gate (`decide_mutation_action`)
/// so the exclusion can't drift between them. `Path::starts_with` is
/// component-aware, so `/tmp_file` does not match `/tmp`.
///
/// Note: kaish's own in-memory VFS mounts (e.g. `/v/blobs`) have `real_path ==
/// None`, so they are handled by the no-real-path gating path, not here — there
/// is deliberately no lexical `/v` exclusion. Mount-coverage routing delegates
/// unclaimed `/v/*` to the embedder's backend, whose *real* content under `/v`
/// (a real path like `/v/cas/blob.bin`) must keep the trash/approval safety net; a
/// `/v` prefix exclusion here would silently strip it.
pub(crate) fn is_trash_excluded(real_path: Option<&Path>) -> bool {
    matches!(real_path, Some(rp) if rp.starts_with("/tmp"))
}

/// The resource references a draft names, as the draft matcher compares them.
pub(crate) fn resource_refs(draft: &ApprovalRequestDraft) -> Vec<ResourceRef> {
    let mut refs: Vec<ResourceRef> = draft.resources.iter().map(|r| r.to_ref()).collect();
    refs.sort_by(|a, b| (&a.kind, &a.id).cmp(&(&b.kind, &b.id)));
    refs.dedup();
    refs
}

/// Whether a fresh draft claims the same prior state for each resource as
/// the request that was approved (spec §B.4).
///
/// **The replay path only.** `draft_matches` compares resources through
/// `Resource::to_ref()`, which drops the transition — deliberately, because
/// the credential router (`Approvals::match_draft`) has to find the request a
/// *wrong* presentation was aimed at so the rejection counts against it, and
/// a presentation whose state claim drifted still names the same request. The
/// replay has no such need: `Kernel::confirm` already knows which request it
/// is replaying, so it can afford to be strict — and it must be, because the
/// gate site builds its draft from the world as it stands *now*, after the
/// ledger already observed and reserved. A claim that drifted in that window
/// is the last signal that the approval no longer describes the operation.
///
/// `Err(detail)` names both claims: "this replay is not what was approved" is
/// only actionable if it says how.
fn transitions_match(
    draft: &ApprovalRequestDraft,
    resources: &[Resource],
) -> Result<(), String> {
    for approved in resources {
        let reference = approved.to_ref();
        let Some(presented) = draft
            .resources
            .iter()
            .find(|r| r.to_ref() == reference)
        else {
            // `draft_matches` runs first and compares the reference sets, so
            // this is unreachable in practice. It is still not an occasion to
            // assume a match.
            return Err(format!(
                "it does not name {}:{}, which was approved",
                reference.kind, reference.id
            ));
        };
        if presented.transition != approved.transition {
            return Err(format!(
                "it claims {}:{} is at {} where {} was approved",
                reference.kind,
                reference.id,
                render_claim(presented.transition.as_ref()),
                render_claim(approved.transition.as_ref()),
            ));
        }
    }
    Ok(())
}

/// One resource's prior-state claim, for a diagnostic an operator reads.
fn render_claim(transition: Option<&kaish_types::approval::Transition>) -> String {
    use kaish_types::approval::StateClaim;
    match transition.map(|t| &t.from) {
        None | Some(StateClaim::Unspecified) => "no claimed prior state".to_string(),
        Some(StateClaim::Absent) => "absent".to_string(),
        Some(StateClaim::Exact(id)) => id.clone(),
        Some(StateClaim::Digest { alg, hex }) => format!("{alg}:{hex}"),
        // `StateClaim` is `#[non_exhaustive]`; an unrecognized claim is still
        // a claim, and still has to render as something an operator can read.
        Some(other) => format!("{other:?}"),
    }
}

/// Whether a fresh draft describes the operation and resources that were
/// approved (spec §B.4). Set semantics on resources, matching how a standing
/// grant covers them (§C.4): a duplicate imposes no extra requirement.
///
/// Compares resources by reference (kind + id) only — the transition claim is
/// checked separately by [`transitions_match`], and only on the replay path.
/// See that function for why the two differ.
///
/// `Err(detail)` names the first difference, because "this replay is not what
/// was approved" is only actionable if it says *how*.
pub(crate) fn draft_matches(
    draft: &ApprovalRequestDraft,
    operation: &kaish_types::approval::OperationId,
    resources: &[Resource],
) -> Result<(), String> {
    if draft.operation != *operation {
        return Err(format!(
            "it requests {} where {operation} was approved",
            draft.operation
        ));
    }
    let mut approved: Vec<ResourceRef> = resources.iter().map(|r| r.to_ref()).collect();
    approved.sort_by(|a, b| (&a.kind, &a.id).cmp(&(&b.kind, &b.id)));
    approved.dedup();
    let presented = resource_refs(draft);
    if presented != approved {
        return Err(format!(
            "it touches [{}] where [{}] was approved",
            render_refs(&presented),
            render_refs(&approved)
        ));
    }
    Ok(())
}

fn render_refs(refs: &[ResourceRef]) -> String {
    refs.iter()
        .map(|r| format!("{}:{}", r.kind, r.id))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Extract a human-readable message from a caught panic payload (spec
/// §C.6's `classify_statement` `catch_unwind`). `panic!("literal")` and
/// `panic!("{}", x)` cover the overwhelming majority of panics in practice;
/// anything else is named honestly rather than guessed at.
fn panic_message(payload: &(dyn std::any::Any + Send)) -> String {
    if let Some(s) = payload.downcast_ref::<&str>() {
        (*s).to_string()
    } else if let Some(s) = payload.downcast_ref::<String>() {
        s.clone()
    } else {
        "non-string panic payload".to_string()
    }
}

/// Map a ledger failure at redemption onto the outcome a gate site returns.
/// `AlreadySettled` is the one that matters most: a key presented after a
/// successful settlement reports what already happened instead of running the
/// operation a second time (spec §B.4).
fn approval_error(id: RequestId, err: LedgerError) -> kaish_tool_api::ApprovalOutcome {
    use kaish_tool_api::ApprovalOutcome;
    match err {
        LedgerError::Refused { id, detail } => ApprovalOutcome::Refused { request: id, detail },
        // The request's own state, not the ledger's health (spec §C.1).
        LedgerError::Terminal { id, state, detail } => ApprovalOutcome::Closed {
            request: id,
            state,
            detail: detail.unwrap_or_default(),
        },
        LedgerError::LiveCapacity { .. }
        | LedgerError::LiveCapacityPerPrincipal { .. }
        | LedgerError::RingAtCapacity
        | LedgerError::SinkUnavailable(_)
        | LedgerError::CredentialUnavailable(_) => ApprovalOutcome::LedgerUnavailable {
            reason: err.to_string(),
        },
        other => ApprovalOutcome::Denied {
            request: id,
            reason: other.to_string(),
        },
    }
}

/// Prefix a gate result's diagnostic with the command that raised it, the way
/// every other builtin error reads (`rm: …`), without disturbing the typed
/// control-plane payload riding alongside it.
pub(crate) fn prefix_error(command: &str, mut result: ExecResult) -> ExecResult {
    if !result.err.is_empty() && !result.err.starts_with(command) {
        result.err = format!("{command}: {}", result.err);
    }
    result
}

/// Decide how to gate a truncating overwrite, mirroring `rm`'s trash/approval
/// priority. Pure so the decision table is unit-testable in isolation.
///
/// - A non-existent target or an append has nothing to lose → `Proceed`.
/// - A real path under `/tmp` (host scratch) is excluded (matches `rm`) → `Proceed`.
/// - Trash wins over the gate (trash IS the safety net): `TrashFirst` when
///   trash is on **and** the prior content fits under `trash_max_size` (a
///   file too big to snapshot can't be backed up, so it falls through,
///   exactly like `rm`); else `Gate` when the `fs.*` enforce policy is on;
///   else `Proceed`.
///
/// An overlay/in-memory target has `real_path == None`, so it is *not* excluded
/// and stays gated — the protection is about agent-operation safety, not just
/// real-FS data (Amy, 2026-06-17).
pub(crate) fn decide_mutation_action(
    trash_enabled: bool,
    enforce: bool,
    real_path: Option<&Path>,
    target_exists: bool,
    is_append: bool,
    file_size: u64,
    trash_max_size: u64,
) -> MutationAction {
    if !target_exists || is_append {
        return MutationAction::Proceed;
    }
    if is_trash_excluded(real_path) {
        return MutationAction::Proceed;
    }
    if trash_enabled && file_size <= trash_max_size {
        return MutationAction::TrashFirst;
    }
    if enforce {
        return MutationAction::Gate;
    }
    MutationAction::Proceed
}

/// Overwrite `resolved` with `content`, compare-and-swapping against
/// `expected` first when there is one. The target's current state is
/// re-derived and must match, else a concurrent change is a loud conflict —
/// never a silent clobber. Binary-safe (raw bytes, unlike the `String`-based
/// `PatchOp` CAS). Shared by the byte-oriented gated builtins via
/// `ExecContext::overwrite_checked` (`tee`/`write`/`dd`) and directly by
/// `cp`'s free copy path.
///
/// This is the write-side half of the ledger's precondition check, and it is
/// the half that matters at the mutation: the ledger detects an
/// authorization that went stale between the grant and the redemption, and
/// this catches a change between the redemption and the write. Neither makes
/// the write OS-atomic — a crash mid-write can still truncate (the atomic
/// write-temp-then-rename primitive is a tracked write-model residual).
pub(crate) async fn cas_overwrite(
    backend: &dyn KernelBackend,
    resolved: &Path,
    content: &[u8],
    expected: Option<&OverwriteExpectation>,
) -> Result<(), crate::backend::BackendError> {
    // A re-read or re-digest failure propagates loudly — never
    // `unwrap_or_default()` to empty bytes, which would false-match an empty
    // snapshot (silent overwrite) or report a bogus "file changed" for a real
    // I/O error. A target that vanished since the gate is a change → abort.
    match expected {
        Some(OverwriteExpectation::Bytes(exp)) => {
            let current = backend.read(resolved, None).await?;
            if current != *exp {
                return Err(concurrent_change_error(resolved));
            }
        }
        Some(OverwriteExpectation::Digest(exp)) => {
            let current = crate::ledger::digest_path(backend, resolved)
                .await
                .map_err(|e| {
                    crate::backend::BackendError::InvalidOperation(format!(
                        "{}: cannot re-check the approved content before overwriting it: {e}",
                        resolved.display()
                    ))
                })?;
            if current != *exp {
                return Err(concurrent_change_error(resolved));
            }
        }
        None => {}
    }
    backend
        .write(resolved, content, crate::backend::WriteMode::Overwrite)
        .await
}

/// One wording for "somebody else wrote this while the gate was deciding",
/// whether the expectation was bytes or a digest — a reader should not have
/// to learn which form the gate happened to hold.
fn concurrent_change_error(resolved: &Path) -> crate::backend::BackendError {
    crate::backend::BackendError::InvalidOperation(format!(
        "{}: changed since the write-model gate checked it (concurrent write); \
         aborting overwrite",
        resolved.display()
    ))
}

impl ExecContext {
    /// Create a new execution context with a VFS (uses LocalBackend without tools).
    ///
    /// This constructor is for backward compatibility and tests that don't need tool dispatch.
    /// For full tool support, use `with_vfs_and_tools`.
    pub fn new(vfs: Arc<VfsRouter>) -> Self {
        Self {
            backend: Arc::new(LocalBackend::new(vfs)),
            scope: Scope::new(),
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            stdin_data_rx: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new().into(),
            tools: None,
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            kill_children_on_parent_death: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            current_invocation: None,
            vfs_budget: None,
            watchdog: None,
            #[cfg(all(feature = "localfs", feature = "overlay"))]
            overlay_handle: None,
            ledger_access: None,
            redemption: None,
            capture_failure: None,
            gate_parent: None,
            attempts: Vec::new(),
        }
    }

    /// Create a new execution context with VFS and tool registry.
    ///
    /// This is the preferred constructor for full kaish operation where
    /// tools need to be dispatched through the backend.
    pub fn with_vfs_and_tools(vfs: Arc<VfsRouter>, tools: Arc<ToolRegistry>) -> Self {
        Self {
            backend: Arc::new(LocalBackend::with_tools(vfs, tools.clone())),
            scope: Scope::new(),
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            stdin_data_rx: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new().into(),
            tools: Some(tools),
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            kill_children_on_parent_death: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            current_invocation: None,
            vfs_budget: None,
            watchdog: None,
            #[cfg(all(feature = "localfs", feature = "overlay"))]
            overlay_handle: None,
            ledger_access: None,
            redemption: None,
            capture_failure: None,
            gate_parent: None,
            attempts: Vec::new(),
        }
    }

    /// Create a new execution context with a custom backend.
    pub fn with_backend(backend: Arc<dyn KernelBackend>) -> Self {
        Self {
            backend,
            scope: Scope::new(),
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            stdin_data_rx: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new().into(),
            tools: None,
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            kill_children_on_parent_death: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            current_invocation: None,
            vfs_budget: None,
            watchdog: None,
            #[cfg(all(feature = "localfs", feature = "overlay"))]
            overlay_handle: None,
            ledger_access: None,
            redemption: None,
            capture_failure: None,
            gate_parent: None,
            attempts: Vec::new(),
        }
    }

    /// Create a context with VFS, tools, and a specific scope.
    pub fn with_vfs_tools_and_scope(vfs: Arc<VfsRouter>, tools: Arc<ToolRegistry>, scope: Scope) -> Self {
        Self {
            backend: Arc::new(LocalBackend::with_tools(vfs, tools.clone())),
            scope,
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            stdin_data_rx: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new().into(),
            tools: Some(tools),
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            kill_children_on_parent_death: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            current_invocation: None,
            vfs_budget: None,
            watchdog: None,
            #[cfg(all(feature = "localfs", feature = "overlay"))]
            overlay_handle: None,
            ledger_access: None,
            redemption: None,
            capture_failure: None,
            gate_parent: None,
            attempts: Vec::new(),
        }
    }

    /// Create a context with a specific scope (uses LocalBackend without tools).
    ///
    /// For tests that don't need tool dispatch. For full tool support,
    /// use `with_vfs_tools_and_scope`.
    pub fn with_scope(vfs: Arc<VfsRouter>, scope: Scope) -> Self {
        Self {
            backend: Arc::new(LocalBackend::new(vfs)),
            scope,
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            stdin_data_rx: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new().into(),
            tools: None,
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            kill_children_on_parent_death: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            current_invocation: None,
            vfs_budget: None,
            watchdog: None,
            #[cfg(all(feature = "localfs", feature = "overlay"))]
            overlay_handle: None,
            ledger_access: None,
            redemption: None,
            capture_failure: None,
            gate_parent: None,
            attempts: Vec::new(),
        }
    }

    /// Create a context with a custom backend and scope.
    pub fn with_backend_and_scope(backend: Arc<dyn KernelBackend>, scope: Scope) -> Self {
        Self {
            backend,
            scope,
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            stdin_data_rx: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new().into(),
            tools: None,
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            kill_children_on_parent_death: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            current_invocation: None,
            vfs_budget: None,
            watchdog: None,
            #[cfg(all(feature = "localfs", feature = "overlay"))]
            overlay_handle: None,
            ledger_access: None,
            redemption: None,
            capture_failure: None,
            gate_parent: None,
            attempts: Vec::new(),
        }
    }

    /// Set the available tool schemas (for help command).
    ///
    /// Takes a `Vec` for caller convenience and converts to the shared
    /// `Arc<[…]>` the field stores (see the field docs; GH #48).
    pub fn set_tool_schemas(&mut self, schemas: Vec<ToolSchema>) {
        self.tool_schemas = schemas.into();
    }

    /// Set the tool registry reference.
    pub fn set_tools(&mut self, tools: Arc<ToolRegistry>) {
        self.tools = Some(tools);
    }

    /// Set the job manager for background job tracking.
    pub fn set_job_manager(&mut self, manager: Arc<JobManager>) {
        self.job_manager = Some(manager);
    }

    /// Set the trash backend.
    pub fn set_trash_backend(&mut self, backend: Arc<dyn TrashBackend>) {
        self.trash_backend = Some(backend);
    }

    /// Set stdin for this execution.
    ///
    /// An explicit stdin buffer (`< file`, heredoc, here-string, or a pipeline
    /// hand-off) supersedes any inherited lazy `pipe_stdin`. Since `read_stdin_*`
    /// prefers `pipe_stdin`, clear it here so redirect precedence holds — a
    /// `< file` must beat a frontend-seeded piped stdin. Accepts anything
    /// `Into<Vec<u8>>` — a `String`/`&str` (heredocs, here-strings, most
    /// callers) or a raw `Vec<u8>` (a `< binfile` redirect, GH #176) both work.
    pub fn set_stdin(&mut self, stdin: impl Into<Vec<u8>>) {
        self.stdin = Some(stdin.into());
        self.pipe_stdin = None;
    }

    /// Get stdin, consuming it.
    pub fn take_stdin(&mut self) -> Option<Vec<u8>> {
        self.stdin.take()
    }

    /// Set both text stdin and structured data.
    ///
    /// Use this when passing output through a pipeline where the previous
    /// command produced structured data (e.g., JSON from MCP tools). The text
    /// side is always a genuine `String` here (structured-data hand-off is a
    /// JSON-producing pipeline stage, never binary).
    pub fn set_stdin_with_data(&mut self, text: String, data: Option<Value>) {
        self.stdin = Some(text.into_bytes());
        self.stdin_data = data;
    }

    /// Take structured data if available, consuming it.
    ///
    /// Tools can use this to avoid re-parsing JSON that was already parsed
    /// by a previous command in the pipeline.
    pub fn take_stdin_data(&mut self) -> Option<Value> {
        self.stdin_data.take()
    }

    /// Resolve stdin for a builtin that can consume *either* structured `.data`
    /// or raw text from the previous pipeline stage (jq, scatter, …). Returns
    /// `(Some(data), _)` when the upstream produced structured data, else
    /// `(None, text)`.
    ///
    /// Ordering matters and is the whole point: the pipe is drained to text
    /// FIRST, which runs the upstream producer to completion (it can't be parked
    /// on pipe backpressure), and only THEN is the structured-data sideband
    /// awaited — by which point the producer has definitely sent it (it sends
    /// before writing/closing its pipe). A streaming upstream that emits a lot
    /// of text before sending its (absent) data therefore can't deadlock us, and
    /// a fast structured producer (`seq`) is no longer lost to a startup race
    /// that a one-shot `try_recv` used to drop on the floor.
    pub async fn resolve_stdin(&mut self) -> Result<(Option<Value>, String), String> {
        // Data set directly on the context (not via the pipeline sideband) wins
        // and needs no pipe — e.g. a non-pipeline caller seeded `stdin_data`.
        if let Some(data) = self.stdin_data.take() {
            return Ok((Some(data), String::new()));
        }
        // Drain the pipe (and/or buffered stdin) to text — unblocks the upstream.
        let text = self.read_stdin_to_text().await?.unwrap_or_default();
        // Upstream has now finished; its structured data (if any) is waiting.
        if let Some(rx) = self.stdin_data_rx.take()
            && let Ok(Some(data)) = rx.await
        {
            return Ok((Some(data), text));
        }
        Ok((None, text))
    }

    /// Resolve a path relative to cwd, normalizing `.` and `..` components.
    pub fn resolve_path(&self, path: &str) -> PathBuf {
        let raw = if path.starts_with('/') {
            PathBuf::from(path)
        } else {
            self.cwd.join(path)
        };
        normalize_path(&raw)
    }

    /// Change the current working directory.
    ///
    /// Saves the old directory for `cd -` support.
    pub fn set_cwd(&mut self, path: PathBuf) {
        self.prev_cwd = Some(self.cwd.clone());
        self.cwd = path;
    }

    /// Get the previous working directory (for `cd -`).
    pub fn get_prev_cwd(&self) -> Option<&PathBuf> {
        self.prev_cwd.as_ref()
    }

    /// Read stdin as text, erroring on non-UTF-8 instead of silently
    /// lossy-decoding it (which corrupts binary with `U+FFFD`).
    ///
    /// The strict counterpart to [`Self::read_stdin_to_bytes`], for text-only
    /// builtins (`grep`, `sed`, `awk`, `cut`, `sort`, `jq`, …): a binary stream
    /// is a loud error, not a mangle. Returns `Ok(None)` when there is no stdin
    /// at all. The `Err` is a ready-to-use message; callers prefix their name.
    /// See `docs/binary-data.md` and `docs/issues.md`.
    pub async fn read_stdin_to_text(&mut self) -> Result<Option<String>, String> {
        match self.read_stdin_to_bytes().await {
            None => Ok(None),
            Some(bytes) => String::from_utf8(bytes).map(Some).map_err(|_| {
                "input is not valid UTF-8 (binary data?) — pipe through base64/xxd \
                 or use a binary-aware tool (cat, dd, cmp, wc -c)"
                    .to_string()
            }),
        }
    }

    /// Read all of stdin as raw bytes, preserving binary intact.
    ///
    /// The byte-clean counterpart to [`Self::read_stdin_to_text`], for
    /// binary-aware builtins (`base64`, `xxd`, `checksum`, `wc -c`, `cmp`, …).
    /// Returns `None` when there is no stdin at all (no pipe and no buffer);
    /// an empty pipe yields `Some(vec![])`. The buffered source is already
    /// bytes-typed (GH #176), so this is a plain move, never a re-encode.
    /// See `docs/binary-data.md`.
    pub async fn read_stdin_to_bytes(&mut self) -> Option<Vec<u8>> {
        if let Some(mut reader) = self.pipe_stdin.take() {
            use tokio::io::AsyncReadExt;
            let mut buf = Vec::new();
            reader.read_to_end(&mut buf).await.ok()?;
            Some(buf)
        } else {
            self.stdin.take()
        }
    }

    /// Create a child context for a pipeline stage.
    ///
    /// Shares backend, tools, job_manager, aliases, cwd, and scope
    /// but has independent stdin/stdout pipes.
    pub fn child_for_pipeline(&self) -> Self {
        Self {
            backend: self.backend.clone(),
            scope: self.scope.clone(),
            cwd: self.cwd.clone(),
            prev_cwd: self.prev_cwd.clone(),
            stdin: None,
            stdin_data: None,
            stdin_data_rx: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: self.stderr.clone(),
            tool_schemas: self.tool_schemas.clone(),
            tools: self.tools.clone(),
            job_manager: self.job_manager.clone(),
            pipeline_position: PipelinePosition::Only,
            interactive: self.interactive,
            kill_children_on_parent_death: self.kill_children_on_parent_death,
            aliases: self.aliases.clone(),
            ignore_config: self.ignore_config.clone(),
            output_limit: self.output_limit.clone(),
            allow_external_commands: self.allow_external_commands,
            trash_backend: self.trash_backend.clone(),
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: self.terminal_state.clone(),
            dispatcher: self.dispatcher.clone(),
            cancel: self.cancel.clone(),
            // Output format is per-execution; child pipeline stages start fresh.
            output_format: None,
            // Per-command; each pipeline stage stamps its own at the dispatch seam.
            current_invocation: None,
            // Budget is shared: the child draws from the same pool as the parent.
            vfs_budget: self.vfs_budget.clone(),
            // Watchdog is shared: a patient hold in a pipeline stage or fork
            // suspends the same script clock as foreground execution.
            watchdog: self.watchdog.clone(),
            // Overlay handle is shared: pipeline stages share the same transaction.
            #[cfg(all(feature = "localfs", feature = "overlay"))]
            overlay_handle: self.overlay_handle.clone(),
            // Ledger access is shared: a pipeline stage gates through the
            // same ledger as its parent.
            ledger_access: self.ledger_access.clone(),
            // Per-invocation: a child stage correlates its own replay and
            // owns its own reservations. Copying either would let one gate's
            // authorization travel to another command.
            redemption: None,
            capture_failure: None,
            // Parenthood does travel: a gate reached from inside a gated
            // statement is nested under it however many stages deep the
            // execution went (spec §A.7). It authorizes nothing on its own.
            gate_parent: self.gate_parent.clone(),
            attempts: Vec::new(),
        }
    }

    /// Build an `IgnoreFilter` from the current ignore configuration.
    ///
    /// Returns `None` if no filtering is configured.
    pub async fn build_ignore_filter(&self, root: &std::path::Path) -> Option<crate::walker::IgnoreFilter> {
        use crate::backend_walker_fs::BackendWalkerFs;
        let fs = BackendWalkerFs(self.backend.as_ref());
        self.ignore_config.build_filter(root, &fs).await
    }

    /// Test-only: wire a fresh, standalone ledger onto this context and hand
    /// back its authority, so a unit test can drive a real gate (post →
    /// grant → redeem → settle) without standing up a whole kernel. A
    /// context built by `Kernel::assemble` already has one.
    #[cfg(test)]
    pub(crate) fn wire_test_ledger(&mut self) -> crate::ledger::ApproverHandle {
        let scope = ApprovalScope::kernel(kaish_types::approval::KernelId::mint());
        let (requester, approvals, authority) = crate::ledger::Ledger::build(
            crate::ledger::LedgerConfig::default(),
            scope.clone(),
            None,
            std::sync::Arc::new(crate::ledger::SystemClock),
        )
        .expect("the test ledger must mint an id epoch");
        let chain = Arc::new(crate::ledger::DecisionChain::new(
            authority.clone(),
            approvals.clone(),
            None,
        ));
        self.ledger_access = Some(LedgerAccess {
            requester,
            approvals,
            chain,
            principal: Principal::new("test-session", kaish_types::approval::PrincipalKind::Agent),
            scope,
            job_id: None,
            resolvers: Arc::new(StateResolvers::default()),
            // A wired test context is an authority-holding session: the
            // caller gets the handle back, so the builtin surface must
            // behave the way it does for a REPL that was given one.
            session_authority: Some(authority.clone()),
        });
        authority
    }

    /// Record that capturing this invocation failed, so a gate site posts
    /// `Capture::CaptureFailed` rather than a silently empty argv that
    /// `confirm` would replay as the wrong command (spec §B.4).
    pub(crate) fn set_capture_failed(&mut self, reason: String) {
        self.current_invocation = None;
        self.capture_failure = Some(reason);
    }

    /// Correlate the next gate this context reaches with an attempt already
    /// reserved against `request` (spec §B.4). Kernel-internal —
    /// `Kernel::confirm` is the only caller.
    pub(crate) fn set_redemption(&mut self, request_id: RequestId, attempt_id: AttemptId) {
        self.redemption = Some(RedemptionContext {
            request_id,
            attempt_id,
        });
    }

    /// Drop any replay correlation. Called whatever the replay's outcome: a
    /// stale correlation would let the *next* command adopt an
    /// authorization that was not for it.
    pub(crate) fn clear_redemption(&mut self) {
        self.redemption = None;
    }

    /// Move the replay correlation into the per-command snapshot. Taking
    /// rather than cloning is what keeps one authorization to one dispatch.
    pub(crate) fn take_redemption(&mut self) -> Option<RedemptionContext> {
        self.redemption.take()
    }

    /// Receive a replay correlation moved out of the kernel's own context.
    pub(crate) fn adopt_redemption(&mut self, redemption: Option<RedemptionContext>) {
        self.redemption = redemption;
    }

    /// Which operation the live replay correlation was granted for, if any.
    ///
    /// Peeks without consuming, which is the point: the statement tap has to
    /// tell "this replay is mine" from "this replay is an `fs.remove` on its
    /// way to `rm`'s gate". Taking the correlation to find out would strand
    /// the reservation the inner gate is waiting for.
    pub(crate) fn redemption_operation(&self) -> Option<OperationId> {
        let redemption = self.redemption.as_ref()?;
        let access = self.ledger_access.as_ref()?;
        access
            .approvals
            .get(&redemption.request_id)
            .map(|chain| chain.request.operation)
    }

    /// The filter every `fs.*` gate site classifies its paths with: the
    /// `set -o approvals` enforce policy plus a snapshot of the subscription
    /// registry.
    ///
    /// **Built once per gate call, and free when nothing is subscribed.**
    /// The snapshot is taken only after [`Approvals::any_subscriptions`] —
    /// one relaxed atomic load — says there is something to snapshot, so an
    /// unsubscribed session allocates nothing however many paths the command
    /// names. Call [`SubscriptionFilter::engaged`] on the result and return
    /// early when it is `false`: that early-out is what keeps `rm -rf` over a
    /// large tree from paying a per-path ledger cost nobody asked for.
    pub(crate) fn fs_subscriptions(&self) -> SubscriptionFilter {
        let policy = self.scope.approvals_enabled();
        let subscriptions = match self.ledger_access.as_ref() {
            Some(access) if access.approvals.any_subscriptions() => access.approvals.subscriptions(),
            _ => Vec::new(),
        };
        SubscriptionFilter::new(policy, subscriptions)
    }

    /// Post the `Observed` entry for the paths an `observe` subscription
    /// covers, and let the operation proceed.
    ///
    /// One entry, straight onto the log — no request, no grant, no attempt,
    /// nothing in the live index. The gate site's classification is the
    /// whole decision: each resource carries the display path the command
    /// named, the resolved path the subscription's glob matched, and the
    /// winning subscription's id, so the entry records exactly what was
    /// classified and there is no second matcher to disagree with the
    /// filter. It never defers and never exits 2 — an observe subscription
    /// decides nothing.
    ///
    /// `Err(result)` exits 1: the ledger could not commit the entry (a full
    /// ring, a full sink). Never swallowed — an operator who subscribed
    /// asked for a complete record, and a mutation running outside a record
    /// the operator believes complete is the exact gap a subscription
    /// exists to close.
    pub(crate) async fn record_observed(
        &mut self,
        operation: KernelOperation,
        command: &str,
        resources: Vec<kaish_types::approval::ObservedResource>,
    ) -> Result<(), ExecResult> {
        if resources.is_empty() {
            return Ok(());
        }
        let Some(access) = self.ledger_access.as_ref() else {
            // Classified as observed, but no ledger to record on: fail
            // closed, the same posture `request_approval` takes on a
            // ledgerless context.
            return Err(ExecResult::failure(
                1,
                format!("{command}: an observe subscription covers these paths but this context has no ledger to record on"),
            ));
        };
        let operation_id = operation.id();
        access
            .requester
            .observed(
                operation_id,
                access.scope.clone(),
                access.principal.clone(),
                resources,
                None,
            )
            .await
            .map_err(|e| {
                ExecResult::failure(
                    1,
                    format!("{command}: the observe record could not be committed: {e}"),
                )
            })
    }

    /// Record one top-level statement, and gate it when the classifier says
    /// it must ask first (`docs/approval-ledger.md` §C.6).
    ///
    /// **Called from exactly two sites** — the top-level statement loop and
    /// `execute_argv` — and from nowhere inside the recursion. A tap in
    /// `execute_stmt_flow` or a nested statement loop would post once per
    /// loop iteration, which is the thousand-entry mistake the top-level rule
    /// exists to prevent.
    ///
    /// Three postures come out of it:
    ///
    /// - **Observe** (the default, and the floor): one chainless `Observed`
    ///   entry carrying the plan, then the statement runs. A tap that cannot
    ///   commit warns and the statement **still runs** — the tap is a second
    ///   opinion, not a permission gate, and nobody opted into a completeness
    ///   guarantee. An `fs.*` subscription's operator did, which is why that
    ///   path exits 1 and this one does not.
    /// - **Gate**: the tap entry posts first — it records the *ask*, not the
    ///   execution, so a statement that defers and never runs still keeps its
    ///   tap entry — and then the request runs the same decision chain every
    ///   `fs.*` gate runs. Every fail-closed rule holds there: a decision that
    ///   cannot be recorded is not made.
    /// - **Replay**: under a live redemption correlation for `cmd.execute`,
    ///   no tap posts — `confirm` must not record a second `Observed` for the
    ///   statement it replays — and the gate runs whatever the posture, so the
    ///   draft matcher can check the replay against what was granted. A
    ///   correlation for any *other* operation belongs to a gate site further
    ///   down and is left untouched.
    pub(crate) async fn tap_statement(
        &mut self,
        plan: Plan,
        capture: Capture,
        presented: Option<&str>,
        classifier: Option<&Arc<dyn StatementClassifier>>,
    ) -> StatementTap {
        let replaying = self.redemption_operation();
        let replaying_this_statement = replaying
            .as_ref()
            .is_some_and(|op| op.as_str() == KernelOperation::CmdExecute.as_str());
        if replaying.is_some() && !replaying_this_statement {
            // Someone else's authorization is in flight — an `fs.remove`
            // replay on its way to `rm`'s gate. Neither record it nor consume
            // it.
            return StatementTap::Proceed { gated: false };
        }

        let (posture, assessment) = self.classify_statement(&plan, classifier);

        if !replaying_this_statement {
            self.record_statement(&plan).await;
        }

        let (reason, risk) = match &posture {
            StatementPosture::Gate { reason, risk } => (reason.clone(), *risk),
            StatementPosture::Observe if replaying_this_statement => (
                "replaying a granted statement".to_string(),
                KernelOperation::CmdExecute.risk(),
            ),
            StatementPosture::Observe => return StatementTap::Proceed { gated: false },
            // `StatementPosture` is `#[non_exhaustive]`: an unrecognized
            // future variant gates rather than silently reading as
            // `Observe` — the same fail-closed rule an `Err` classification
            // and a caught panic already follow (spec §C.6).
            other => (
                format!("unrecognized statement posture: {other:?}"),
                KernelOperation::CmdExecute.risk(),
            ),
        };

        let mut builder = ApprovalRequest::builder(KernelOperation::CmdExecute.as_str())
            .risk(risk)
            .reason(reason)
            .hint(plan.rendered.clone())
            .plan(plan.clone());
        for command in &plan.commands {
            builder = builder.resource(Resource::plain(CMD_KIND, command.name.clone()));
        }
        let draft = match builder.build() {
            Ok(draft) => draft,
            // Unreachable — `cmd.execute` is a well-formed dotted id and the
            // risk is always set above — but a build failure must never mean
            // "proceed" for a statement a classifier asked to hold.
            Err(e) => {
                return StatementTap::Halt(Box::new(ExecResult::failure(
                    1,
                    format!("cmd.execute: could not build the approval request: {e}"),
                )))
            }
        };
        // `presented` is the `--confirm=<key>` the statement's own argv
        // carries, lifted out of the plan before it was redacted. Without it
        // a user re-running a held line with the key they were given would
        // never redeem: the gate would see no key, mint a second request, and
        // exit 2 again with the first one still pending (spec §B.4 — one
        // acceptance contract, and the draft matcher is what correlates a
        // presentation to the request it is for).
        let outcome = self.gate(draft, presented, Some(capture)).await;
        // The classifier's own judgment — which may differ from `posture`
        // above when the static floor forced a `Gate` it did not ask for —
        // is recorded once the request it is about actually exists (spec
        // §C.7). Best-effort: a classifier's reasoning is a second opinion,
        // not itself a permission gate, so a capacity failure here warns and
        // never changes what the statement does.
        if let (Some(assessment), Some(request_id)) = (assessment, outcome.request_id()) {
            self.record_assessment(request_id.clone(), assessment).await;
        }
        match outcome.proceed() {
            Ok(attempt) => {
                // Everything this statement reaches is nested under it
                // (spec §A.7). Recorded, never authorizing: a defense-in-depth
                // `fs.*` gate underneath still asks for its own decision.
                self.gate_parent = Some(attempt.request_id().clone());
                StatementTap::Proceed { gated: true }
            }
            Err(result) => StatementTap::Halt(Box::new(result)),
        }
    }

    /// Run the registered classifier, if any, against `plan`, and combine
    /// its answer with the kernel's static gate floor (spec §C.6).
    ///
    /// Returns the posture the tap must act on, plus the classifier's own
    /// raw [`StatementAssessment`] (`None` only when no classifier is
    /// registered at all) — kept separate because the floor can force a
    /// `Gate` the classifier itself did not ask for, and the eventual
    /// `Assessed` entry (spec §C.7) should say what the classifier actually
    /// judged, not the floor-adjusted result.
    ///
    /// **`catch_unwind` here is a deliberate panic boundary** — one of the
    /// few correct ones in this codebase. A statement classifier runs
    /// unconditionally in front of *every* top-level statement, is very
    /// often a call into a model or an embedder-authored rule this kernel
    /// did not write, and — unlike `Policy::evaluate`, which only runs once
    /// a decision is genuinely being asked for — has no scope in which a
    /// panic is unambiguously a kaish bug worth an unwind. Letting it
    /// propagate into the statement loop would crash every later statement
    /// in the same program over one classifier's bug on an unrelated line.
    /// The unwind corrupts nothing it catches: no lock is held across this
    /// call, and a caught panic maps to `Gate` through the exact same path
    /// an `Err` return takes (spec §C.6) — a classifier that cannot answer
    /// must not be able to turn the statement gate off.
    fn classify_statement(
        &self,
        plan: &Plan,
        classifier: Option<&Arc<dyn StatementClassifier>>,
    ) -> (StatementPosture, Option<StatementAssessment>) {
        // The floor only bounds a *classifier's* answer (spec §C.6: "a
        // classifier may raise... it may never lower..." — the rule's
        // subject is the classifier). A kernel with none registered keeps
        // its pre-R4 default exactly: every statement is `Observe` at this
        // layer, and dangerous operations still gate at their own `fs.*` /
        // tool-level site (`kaish-trash empty`'s `trash.empty` gate is
        // `always_enforced` there independent of the statement tap). Firing
        // the floor unconditionally would gate `cmd.execute` in front of a
        // tool-level gate that already covers the same operation, for a
        // kernel that opted into no classification at all.
        let Some(classifier) = classifier else {
            return (StatementPosture::Observe, None);
        };
        let floor = static_gate_floor(plan);

        let execution_context = self.execution_context();
        let input = StatementClassificationInput::new(plan, &execution_context);
        let outcome =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| classifier.classify(&input)));

        let assessment = match outcome {
            Ok(Ok(assessment)) => assessment,
            Ok(Err(err)) => {
                tracing::warn!(
                    reason = %err,
                    "statement classifier returned Err — gating (spec §C.6: Err means Gate, never Observe)"
                );
                StatementAssessment::new(
                    StatementPosture::gate(
                        format!("the statement classifier could not judge this statement: {err}"),
                        KernelOperation::CmdExecute.risk(),
                    ),
                    AssessorId::new("kernel:classifier-error"),
                )
            }
            Err(panic_payload) => {
                let message = panic_message(&*panic_payload);
                tracing::warn!(
                    panic = %message,
                    "statement classifier panicked — gating rather than unwinding into the statement loop (spec §C.6)"
                );
                StatementAssessment::new(
                    StatementPosture::gate(
                        format!("the statement classifier panicked: {message}"),
                        KernelOperation::CmdExecute.risk(),
                    ),
                    AssessorId::new("kernel:classifier-panic"),
                )
            }
        };
        let posture = assessment.posture.clone().at_least(floor);
        (posture, Some(assessment))
    }

    /// What a statement would run against, for the classifier (spec §C.6).
    /// `cwd` is a logical VFS path, never a host path — the same convention
    /// `PlanBinding::cwd` uses (spec §A.9).
    ///
    /// `mounts` is empty: the router has no per-mount `MountClass` registry
    /// yet (a `KernelConfig` seam an embedder assigns classes through is
    /// real follow-up work, not part of this lane's scope — R4 built the
    /// type a classifier can consume, not the registry that would populate
    /// it in the real kernel).
    fn execution_context(&self) -> ExecutionContext {
        let scope = self
            .ledger_access
            .as_ref()
            .map(|access| access.scope.clone())
            .unwrap_or_else(|| ApprovalScope::kernel(KernelId::new(0)));
        ExecutionContext::new(self.cwd.display().to_string(), scope)
    }

    /// Post the classifier's judgment as an `Assessed` entry, once the
    /// request it is about exists (spec §C.7). Best-effort, matching
    /// `record_statement`'s asymmetry: a classifier's reasoning is a second
    /// opinion, not itself a permission gate.
    async fn record_assessment(&self, request: RequestId, assessment: StatementAssessment) {
        let Some(access) = self.ledger_access.as_ref() else {
            return;
        };
        let (outcome, reason, risk) = match &assessment.posture {
            StatementPosture::Gate { reason, risk } => (AssessmentOutcome::Escalate, reason.clone(), Some(*risk)),
            StatementPosture::Observe => (
                AssessmentOutcome::Allow,
                "no gate posture matched".to_string(),
                None,
            ),
            // See the identical `#[non_exhaustive]` note in `tap_statement`.
            other => (
                AssessmentOutcome::Escalate,
                format!("unrecognized statement posture: {other:?}"),
                None,
            ),
        };
        let mut recorded = ApprovalAssessment::new(
            request.clone(),
            assessment.assessor,
            AssessmentStage::Classifier,
            outcome,
            reason,
        );
        if let Some(risk) = risk {
            recorded = recorded.with_risk(risk);
        }
        if let Some(confidence) = assessment.confidence {
            recorded = recorded.with_confidence(confidence);
        }
        if let Some(model) = assessment.model {
            recorded = recorded.with_model(model);
        }
        if let Err(err) = access.requester.assessments().append(recorded).await {
            tracing::warn!(request_id = %request, error = %err, "could not record the statement classifier's assessment");
        }
    }

    /// Post the statement tap's chainless `Observed` entry: one `cmd`
    /// resource per planned command, plus the plan itself (spec §C.6).
    ///
    /// A failure warns and returns. This is the one observability path that
    /// does not fail its operation closed, and the asymmetry is deliberate —
    /// see [`Self::tap_statement`].
    async fn record_statement(&self, plan: &Plan) {
        let Some(access) = self.ledger_access.as_ref() else {
            return;
        };
        let resources = plan
            .commands
            .iter()
            .map(|command| ObservedResource::planned(CMD_KIND, command.name.clone()))
            .collect();
        if let Err(err) = access
            .requester
            .observed(
                KernelOperation::CmdExecute.id(),
                access.scope.clone(),
                access.principal.clone(),
                resources,
                Some(plan.clone()),
            )
            .await
        {
            tracing::warn!(
                error = %err,
                statement = %plan.rendered,
                "the statement tap could not be recorded — the statement still runs"
            );
        }
    }

    /// Request approval for one kernel operation — the single call every
    /// gate site makes (`docs/approval-ledger.md` §C.1).
    ///
    /// `paths` are the resolved paths the operation would touch; they become
    /// the request's `path` resources. `reason` says why the gate fired and
    /// `hint` is the display-only re-run template (untrusted producer text,
    /// spec §C.3 — it never carries a credential). `presented` is the
    /// `--confirm=<token>` value when the caller supplied one.
    ///
    /// `Ok(attempt)` means an attempt is reserved and the caller may perform
    /// the operation; it settles automatically when the invocation returns.
    /// `Err(result)` is what the caller **must return verbatim** — exit 2
    /// with the pending request, or exit 1 naming a denial, a refusal, or a
    /// settled outcome. Never fall through to the operation on `Err`.
    pub(crate) async fn request_gate(
        &mut self,
        operation: KernelOperation,
        resources: Vec<Resource>,
        reason: &str,
        hint: String,
        presented: Option<&str>,
    ) -> Result<kaish_tool_api::AttemptHandle, ExecResult> {
        let mut builder = ApprovalRequest::builder(operation.as_str())
            .risk(operation.risk())
            .reason(reason)
            .hint(hint);
        for resource in resources {
            builder = builder.resource(resource);
        }
        let draft = match builder.build() {
            Ok(draft) => draft,
            // Unreachable in practice — every `KernelOperation` id is
            // well-formed and non-empty (proven in `ledger::operation`'s
            // tests) — but a build failure must never mean "proceed".
            Err(e) => {
                return Err(ExecResult::failure(
                    1,
                    format!("{operation}: could not build the approval request: {e}"),
                ))
            }
        };
        self.gate(draft, presented, None).await.proceed()
    }

    /// The one acceptance contract behind every approval (spec §B.4): a
    /// replay correlated by [`RedemptionContext`], a presented
    /// `--confirm=<token>`, and a fresh request all land here, and the same
    /// draft matcher decides whether the operation in hand is the operation
    /// that was approved.
    ///
    /// `capture` overrides how the invocation is recorded for replay. `None`
    /// reads the dispatch seam, which is what every tool gate site wants; the
    /// statement gate passes `Some(Capture::Statement{…})` because the thing
    /// it would replay is a statement, not an argv (spec §C.6).
    pub(crate) async fn gate(
        &mut self,
        draft: ApprovalRequestDraft,
        presented: Option<&str>,
        capture: Option<Capture>,
    ) -> kaish_tool_api::ApprovalOutcome {
        use kaish_tool_api::ApprovalOutcome;

        let Some(access) = self.ledger_access.clone() else {
            return ApprovalOutcome::Unsupported;
        };

        // The context this operation is being judged in, and the context a
        // redemption of it must still match (spec §A.9).
        let binding = self.binding_for(&draft, &access);

        // ── The replay path ─────────────────────────────────────────
        // `Kernel::confirm` already reserved the attempt; this draft must
        // describe the operation that was granted, or the replay turned into
        // something else on its way here.
        if let Some(redemption) = self.redemption.take() {
            match self.authorize_replay(&access, redemption, &draft, &binding).await {
                Rebind::Authorized(outcome) => return outcome,
                // The replay moved out of the context it was approved in.
                // It is not a redemption of that grant; it is a new ask.
                Rebind::Rebind => {}
            }
        // ── The bearer-key path ─────────────────────────────────────
        // The draft names the request; the key authorizes it. A wrong key
        // still counts against the request the draft describes, which is
        // what gives the rejected-attempt limit somewhere to attach (§F.3).
        } else if let Some(key) = presented {
            match self.present_key(&access, key, &draft, &binding).await {
                Rebind::Authorized(outcome) => return outcome,
                Rebind::Rebind => {}
            }
        }

        // ── A fresh request ─────────────────────────────────────────
        // Asking again after a closed predecessor links to it (spec §B.5):
        // "this took four attempts over two hours" stays legible and the
        // chain stays walkable. Only a *closed-without-running* predecessor
        // counts — cancelled, denied, or past a deadline the embedder set;
        // a successful settlement is a repeat operation, not the same
        // thread of intent.
        let mut draft = draft;
        if draft.supersedes.is_none()
            && let Some(previous) = access.approvals.match_draft(&draft.operation, &resource_refs(&draft))
            && matches!(
                access.approvals.state(&previous),
                Some(RequestState::Cancelled | RequestState::Denied | RequestState::Expired)
            )
        {
            draft.supersedes = Some(previous);
        }
        let capture = capture.unwrap_or_else(|| self.capture());
        let origin = kaish_types::approval::RequestOrigin::new(
            access.scope.clone(),
            binding,
            access.principal.clone(),
            capture)
        .with_parent(self.gate_parent.clone())
        .with_job_id(access.job_id);
        let request = match access.requester.post_request(draft, origin).await {
            Ok(request) => request,
            Err(err) => {
                return ApprovalOutcome::LedgerUnavailable {
                    reason: err.to_string(),
                }
            }
        };

        let outcome = {
            let chain_ctx = ChainContext::new(self.cancel.clone());
            access.chain.decide(&request, &chain_ctx).await
        };
        match outcome {
            Ok(ChainOutcome::Granted { grant, .. }) => {
                // Evaluate the grant's preconditions here, outside the
                // ledger lock, and carry the result in (spec §B.1). A
                // standing grant or a policy hook can decide in the same
                // breath as the request, so this path re-observes too —
                // a grant is never redeemed on an unchecked claim.
                let report = self.observe_conditions(&grant.conditions).await;
                self.reserve(&access, &request.id, report).await
            }
            Ok(ChainOutcome::Denied { reason, .. }) => ApprovalOutcome::Denied {
                request: request.id,
                reason,
            },
            Ok(ChainOutcome::Deferred) => ApprovalOutcome::Pending(Box::new(
                kaish_tool_api::PendingApproval::new(request.into()),
            )),
            // The *execution* was cancelled, not a decision: the chain
            // awaits nobody (spec §C.2), so this is either a token that had
            // already fired when the chain was entered or one that fired in
            // the window between deciding to grant and the grant landing.
            // Either way nothing was granted and nothing will run.
            Ok(ChainOutcome::Cancelled) => ApprovalOutcome::Cancelled {
                request: request.id,
            },
            // A decision the ledger refused: the request closed underneath
            // the chain (cancelled by its owner, past a deadline the
            // embedder set) is `Closed`, and only a ledger condition —
            // capacity, sink backpressure — is `LedgerUnavailable`.
            Err(err) => approval_error(request.id, err),
        }
    }

    /// How this invocation was captured, for a replay by the approval side
    /// (spec §B.4). `DirectExecution` is a `tool.execute` with no dispatch
    /// seam above it — grantable and redeemable by key, but not replayable.
    fn capture(&self) -> Capture {
        if let Some(reason) = &self.capture_failure {
            return Capture::CaptureFailed {
                reason: reason.clone(),
            };
        }
        match self.current_invocation.as_deref() {
            Some((tool, argv)) => Capture::Exact(Invocation {
                tool: tool.clone(),
                argv: argv.clone(),
            }),
            None => Capture::DirectExecution,
        }
    }

    /// The binding this gate call is judged under (spec §A.9): what was
    /// judged, where, and by whom.
    ///
    /// The digest covers the statement's rendered text when the draft
    /// carries a plan, and the operation plus its sorted resource references
    /// otherwise — an `fs.*` gate has no statement behind it, and what it
    /// judged is exactly the operation and the paths it named.
    fn binding_for(&self, draft: &ApprovalRequestDraft, access: &LedgerAccess) -> PlanBinding {
        let judged = match &draft.plan {
            // The credential is the authorization, not part of what was
            // judged — see `strip_confirm_tokens`.
            Some(plan) => crate::ast::plan::strip_confirm_tokens(&plan.rendered),
            None => format!(
                "{}\n{}",
                draft.operation,
                render_refs(&resource_refs(draft))
            ),
        };
        let digest = Sha256::digest(judged.as_bytes());
        PlanBinding::new(
            PlanDigest::new(format!("{digest:x}")),
            self.cwd.display().to_string(),
            access.scope.clone(),
        )
    }

    /// Accept an attempt `Kernel::confirm` already reserved, once the fresh
    /// draft is shown to describe the granted request.
    async fn authorize_replay(
        &mut self,
        access: &LedgerAccess,
        redemption: RedemptionContext,
        draft: &ApprovalRequestDraft,
        binding: &PlanBinding,
    ) -> Rebind {
        use kaish_tool_api::ApprovalOutcome;

        let Some(chain) = access.approvals.get(&redemption.request_id) else {
            return Rebind::Authorized(ApprovalOutcome::LedgerUnavailable {
                reason: LedgerError::NotFound(redemption.request_id).to_string(),
            });
        };
        // A grant is a decision about an operation *in a context* (spec
        // §A.9). A replay that moved out of that context is not a redemption
        // of this grant — it is a new request, so settle the reservation
        // `confirm` made and fall through to posting one.
        if let Some(detail) = binding.mismatch(&chain.request.binding) {
            tracing::warn!(
                request_id = %redemption.request_id,
                detail = %detail,
                "approval binding moved since the grant — posting a fresh request instead of redeeming"
            );
            let _ = access
                .requester
                .settle_by_ids(&redemption.request_id, redemption.attempt_id, Outcome::Exit(1))
                .await;
            return Rebind::Rebind;
        }
        // Two checks, in order: the operation and the resource set, then the
        // prior-state claim on each resource. The second is replay-only —
        // see `transitions_match`.
        let matched = draft_matches(draft, &chain.request.operation, &chain.request.resources)
            .and_then(|()| transitions_match(draft, &chain.request.resources));
        if let Err(detail) = matched {
            // Settle the attempt `confirm` reserved rather than leaving it
            // in flight: a failed attempt does not consume the grant, so the
            // operator can correct the replay and try again inside
            // `not_after` (spec §A.1).
            let _ = access
                .requester
                .settle_by_ids(
                    &redemption.request_id,
                    redemption.attempt_id,
                    Outcome::Exit(1),
                )
                .await;
            return Rebind::Authorized(ApprovalOutcome::Refused {
                request: redemption.request_id.clone(),
                detail: LedgerError::DraftMismatch {
                    request: redemption.request_id,
                    detail,
                }
                .to_string(),
            });
        }
        Rebind::Authorized(ApprovalOutcome::Authorized(self.adopt(
            access,
            redemption.request_id,
            redemption.attempt_id,
        )))
    }

    /// Redeem by presenting a bearer credential, resolving which request the
    /// presentation is for from the draft itself (spec §F.3 item 2).
    async fn present_key(
        &mut self,
        access: &LedgerAccess,
        key: &str,
        draft: &ApprovalRequestDraft,
        binding: &PlanBinding,
    ) -> Rebind {
        use kaish_tool_api::ApprovalOutcome;

        let Some(id) = access.approvals.match_draft(&draft.operation, &resource_refs(draft)) else {
            // A key describing no request kaish has ever seen counts against
            // nothing — a guesser cannot void a request it cannot describe.
            access.requester.reject_unmatched_key();
            return Rebind::Authorized(ApprovalOutcome::Unmatched {
                detail: format!(
                    "no approval request for {} over [{}]",
                    draft.operation,
                    render_refs(&resource_refs(draft))
                ),
            });
        };
        // Same rule the replay path holds (spec §A.9): a key presented from
        // outside the context the grant was decided in redeems nothing. It
        // does not count as a rejected credential either — the key is right,
        // the context is not — so the ledger's rejection counter is
        // untouched and a fresh request is posted instead.
        if let Some(chain) = access.approvals.get(&id) {
            if let Some(detail) = binding.mismatch(&chain.request.binding) {
                tracing::warn!(
                    request_id = %id,
                    detail = %detail,
                    "approval binding moved since the grant — posting a fresh request instead of redeeming"
                );
                return Rebind::Rebind;
            }
        }
        // Observe before presenting, because the observation has to be
        // *inside* the reservation transaction (spec §B.1) and the I/O has
        // to be outside the lock. The ledger still checks the credential
        // first, so a wrong key lands on `TokenRejected` and never reaches
        // the condition check — an invalid presentation cannot void a grant.
        let conditions = access
            .approvals
            .get(&id)
            .and_then(|chain| chain.grant)
            .map(|grant| grant.conditions)
            .unwrap_or_default();
        let report = self.observe_conditions(&conditions).await;
        match access
            .requester
            .redeem_with_token(&id, key, access.principal.clone(), report)
            .await
        {
            Ok(attempt) => {
                let attempt_id = attempt.attempt_id();
                Rebind::Authorized(ApprovalOutcome::Authorized(self.adopt(access, id, attempt_id)))
            }
            Err(err) => Rebind::Authorized(approval_error(id, err)),
        }
    }

    /// The resolver for one resource kind. `path` is the kernel's own,
    /// rebuilt per call from this context's backend and cwd so an overlay or
    /// an in-memory mount is observed exactly the way the gate site resolved
    /// it; every other kind comes from the registry an embedder configured.
    pub(crate) fn state_resolver(&self, kind: &str) -> Option<Arc<dyn StateResolver>> {
        if kind == PATH_KIND {
            return Some(Arc::new(PathResolver::new(
                Arc::clone(&self.backend),
                self.cwd.clone(),
            )));
        }
        self.ledger_access
            .as_ref()
            .and_then(|access| access.resolvers.get(kind).cloned())
    }

    /// Read the current state of every condition that claims a prior state
    /// (spec §B.4), for a redemption to carry into the ledger.
    ///
    /// Runs outside the ledger lock, because it is I/O. An unreadable
    /// resource — or one whose kind has no registered resolver — becomes
    /// [`ConditionReport::Unobservable`], which refuses; it is never a
    /// silent pass.
    ///
    /// **Each observation is stamped from the ledger's own clock**
    /// (`Requester::clock_reading`), not from the system clock, so a
    /// custom-clock embedder reads one timeline inside one `Redeemed` entry
    /// (spec §A.5). The reading is raw: it is taken here, at the moment the
    /// resolver looked, which is earlier than the entry's own latched commit
    /// stamp and is meant to be — that gap is how stale the check was, and
    /// collapsing it would make the record claim the world was observed at
    /// commit time when it was not. The ledger clamps the stamp to its own
    /// view at commit, so an observation can never claim to postdate the
    /// entry carrying it.
    pub(crate) async fn observe_conditions(&self, conditions: &[Condition]) -> ConditionReport {
        let mut observed = Vec::new();
        // A condition that claims nothing has nothing to check, and costs no
        // I/O here (spec §A.3) — see `conditions_to_observe`.
        for condition in crate::ledger::conditions_to_observe(conditions) {
            let resource = condition.resource.clone();
            let Some(resolver) = self.state_resolver(&resource.kind) else {
                return ConditionReport::Unobservable {
                    detail: format!("no state resolver is registered for the '{}' resource kind", resource.kind),
                    resource,
                };
            };
            // A context with no ledger has no grant either, so there are no
            // conditions to observe and this arm cannot be reached with one
            // — but a stamp from the wrong clock is exactly the defect this
            // reading exists to close, so say so rather than substituting a
            // system reading.
            let Some(access) = self.ledger_access.as_ref() else {
                return ConditionReport::Unobservable {
                    detail: "this context has no approval ledger, so there is no clock to stamp \
                             the observation from"
                        .to_string(),
                    resource,
                };
            };
            match resolver.observe(&resource.id).await {
                Ok(claim) => observed.push(Observation {
                    resource,
                    claim,
                    at: access.requester.clock_reading(),
                }),
                Err(err) => {
                    return ConditionReport::Unobservable {
                        detail: err.to_string(),
                        resource,
                    }
                }
            }
        }
        ConditionReport::observed(observed)
    }

    /// Close an undecided request (`docs/approval-ledger.md` §B.5).
    ///
    /// **Cancellation is a requester action, not an approval action.** The
    /// principal that owns the request may close it holding no authority at
    /// all — that is what lets a gated agent withdraw its own request. A
    /// session holding this ledger's authority may cancel any request,
    /// because it could already deny that request; withholding cancellation
    /// from it would be a special case with nothing behind it. Any other
    /// session cancelling another principal's request is refused.
    ///
    /// The originating background job, if there was one, keeps its cached
    /// result; the request it names is now `Cancelled`, which
    /// `approvals show` reports.
    ///
    /// `rev` is the revision the caller's view of the request was at (spec
    /// §B.6) — a caller that read the request moments ago (`approvals
    /// cancel`'s own chain fetch, an embedder's cached `PendingApproval`)
    /// quotes it, and a stale quote is refused and recorded as
    /// `RevisionRejected` rather than applied.
    pub(crate) async fn cancel_request(
        &self,
        id: &RequestId,
        rev: u64,
        reason: CancelReason,
    ) -> Result<(), String> {
        let Some(access) = self.ledger_access.as_ref() else {
            return Err("this session has no approval ledger".to_string());
        };
        let Some(chain) = access.approvals.get(id) else {
            return Err(format!("no approval request {id} in this ledger"));
        };

        let owned = chain.request.principal == access.principal;
        if !owned && access.session_authority.is_none() {
            return Err(format!(
                "{id} was raised by {}, not {} — cancellation is the requester's action, and this \
                 session holds no approval authority over another principal's request",
                chain.request.principal.id, access.principal.id
            ));
        }

        access
            .requester
            .cancel(id, rev, access.principal.clone(), reason)
            .await
            .map_err(|e| format!("{id} cannot be cancelled: {e}"))?;
        Ok(())
    }

    /// Reserve an attempt against a request this execution just had granted.
    async fn reserve(
        &mut self,
        access: &LedgerAccess,
        id: &RequestId,
        report: ConditionReport,
    ) -> kaish_tool_api::ApprovalOutcome {
        use kaish_tool_api::ApprovalOutcome;
        match access
            .requester
            .redeem(id, access.principal.clone(), report)
            .await
        {
            Ok(attempt) => {
                let attempt_id = attempt.attempt_id();
                ApprovalOutcome::Authorized(self.adopt(access, id.clone(), attempt_id))
            }
            Err(err) => approval_error(id.clone(), err),
        }
    }

    /// Take ownership of a reserved attempt: wrap it in its drop-safe guard
    /// (so an interrupted invocation settles `Unknown`, never silently) and
    /// hand the tool a handle naming it.
    fn adopt(
        &mut self,
        access: &LedgerAccess,
        request: RequestId,
        attempt: AttemptId,
    ) -> kaish_tool_api::AttemptHandle {
        self.attempts.push(AttemptGuard::new(
            access.requester.clone(),
            crate::ledger::AttemptHandle::from_reservation(request.clone(), attempt),
        ));
        kaish_tool_api::AttemptHandle::from_reservation(request, attempt)
    }

    /// Settle every attempt this invocation reserved with its real exit code
    /// — the dispatch seam's one call after `tool.execute()` returns (spec
    /// §C.1). Draining the guards here is what makes the normal path a
    /// reported `Exit(code)` rather than the `Unknown{Cancelled}` their
    /// `Drop` would otherwise queue.
    pub(crate) async fn settle_attempts(&mut self, code: i64) {
        for guard in std::mem::take(&mut self.attempts) {
            if let Err(err) = guard.settle(Outcome::Exit(code)).await {
                // Already-terminal is an expected race (the tool reported its
                // own richer outcome through `settle_with`); anything else is
                // worth a trace so it is not silently lost.
                tracing::debug!(error = %err, "settling a reserved attempt did not apply");
            }
        }
    }

    /// Gate a batch of truncating overwrites through the approval ledger +
    /// trash, the way `rm` gates deletes — so `tee`/`patch`/`sed -i` can't
    /// silently clobber a file with no recoverable prior copy and no
    /// approval.
    ///
    /// Each target is `(display_path, is_append)`. A path that doesn't exist
    /// yet or is an append has nothing to lose and passes. For an existing
    /// file under `set -o trash`, the prior content is copied to trash first
    /// (via `trash_bytes`) so it's recoverable; the file is left in place for
    /// the caller to overwrite. Under the `fs.*` enforce policy (and trash
    /// off) the batch needs approval: the first call returns an exit-2 result
    /// with one request covering every gated path.
    ///
    /// `Ok(snapshots)` means every snapshot is done and the caller may write
    /// all targets; `snapshots` maps each trash-snapshotted target's resolved
    /// path to its prior bytes, so a byte-oriented caller can pass them as the
    /// `expected` to `overwrite_checked` for a binary-safe compare-and-swap.
    /// `Err(result)` is what the caller must return verbatim (the pending
    /// request, a rejected key, or a trash failure — never fall through to a
    /// destructive overwrite on error).
    ///
    /// `confirm_hint` builds the re-run command shown in the prompt, given
    /// the space-joined gated paths. Most callers want
    /// `|joined| format!("{command} --confirm=<token> {joined}")`, but a tool
    /// whose argv carries operands the operation can't run without — e.g.
    /// `sed -i`'s expression — must reinject them here, or the advertised
    /// re-run will misbehave (or hang on stdin).
    ///
    /// `operation` is explicit rather than derived from `command`: spec §A.6
    /// wants a new gate site to be a *compile* error until it names its
    /// operation, and a string sniff on the command name would instead pick
    /// a plausible wrong default in silence.
    pub async fn gate_overwrites(
        &mut self,
        operation: KernelOperation,
        command: &str,
        targets: &[(String, bool)],
        confirm: Option<&str>,
        confirm_hint: impl FnOnce(&str) -> String,
    ) -> Result<GateExpectations, ExecResult> {
        let mut expectations = GateExpectations::new();
        let trash_enabled = self.scope.trash_enabled();
        let subscriptions = self.fs_subscriptions();
        // Fast path: nothing is subscribed and nothing is trashed, so this
        // costs one branch and allocates nothing. A large tree must not pay
        // a per-path ledger cost unless an operator asked for one.
        if !trash_enabled && !subscriptions.engaged() {
            return Ok(expectations);
        }
        let trash_max_size = self.scope.trash_max_size();
        let operation_id = operation.id();

        struct Decided {
            display: String,
            resolved: PathBuf,
            action: MutationAction,
            /// The `observe` subscription covering this target, when one
            /// does. Kept beside the gate decision rather than folded into
            /// it: observe records what happened, so it fires for a target
            /// the trash caught and for a new file, neither of which the
            /// gate holds.
            observed: Option<kaish_types::approval::SubscriptionId>,
        }
        // Dedup by resolved path (keep first): a multi-file patch with an
        // explicit target lists the same file once per hunk-group, and we must
        // not snapshot it N times or list it N times in the request.
        let mut seen = std::collections::HashSet::new();
        let mut decided = Vec::with_capacity(targets.len());
        for (display, is_append) in targets {
            let resolved = self.resolve_path(display);
            if !seen.insert(resolved.clone()) {
                continue;
            }
            // `real` is used only for the exclusion decision (/tmp, /v); the
            // snapshot reads bytes through the backend, not the real path.
            let real = self.backend.resolve_real_path(Path::new(&resolved));
            let exists = self.backend.exists(Path::new(&resolved)).await;
            // Prior size decides trash eligibility (a file too big to snapshot
            // can't be backed up). Only stat an existing target.
            let size = if exists {
                self.backend
                    .stat(Path::new(&resolved))
                    .await
                    .map(|e| e.size)
                    .unwrap_or(0)
            } else {
                0
            };
            // Matched on the **resolved** path, recorded under the display
            // path. Matching the resolved path is what makes the glob a
            // scope: `cd /workspace && tee secret` must match
            // `/workspace/**`, and a relative path that escaped the glob
            // would leave the scope meaningless. The record still shows what
            // the command named, because that is the string an auditor
            // reading the log can recognize.
            let posture =
                subscriptions.posture(&operation_id, PATH_KIND, &resolved.to_string_lossy());
            let action = decide_mutation_action(
                trash_enabled,
                posture.enforces(),
                real.as_deref(),
                exists,
                *is_append,
                size,
                trash_max_size,
            );
            decided.push(Decided {
                display: display.clone(),
                resolved,
                action,
                observed: match posture {
                    Posture::Observe(id) => Some(id),
                    _ => None,
                },
            });
        }

        // One request covers every gated path in the batch. Each gated
        // target declares the digest its content has *right now* as the
        // transition's `from`, which becomes a redemption-time condition
        // (spec §B.4): this is `cas_overwrite`'s snapshot-compare, lifted
        // onto the ledger. The ledger stores the digest, never the content —
        // the byte snapshot stays with the trash, where the recovery copy
        // lives.
        let gated: Vec<&Decided> = decided
            .iter()
            .filter(|d| matches!(d.action, MutationAction::Gate))
            .collect();
        if !gated.is_empty() {
            let joined = gated
                .iter()
                .map(|d| d.display.as_str())
                .collect::<Vec<_>>()
                .join(" ");
            let mut resources = Vec::with_capacity(gated.len());
            for d in &gated {
                // A target whose prior content cannot be digested cannot be
                // protected by the condition, so it must not be written
                // under one either — refuse rather than gate on a claim
                // nobody can check later.
                let from = crate::ledger::digest_path(&*self.backend, &d.resolved)
                    .await
                    .map_err(|e| {
                        ExecResult::failure(
                            1,
                            format!("{command}: {}: cannot record the prior state: {e}", d.display),
                        )
                    })?;
                // The same digest becomes this target's write-time
                // expectation, so the gate pays for it once. The ledger
                // catches a file that moved while the operator was deciding;
                // this catches one that moves between the ledger's check and
                // the write itself.
                expectations.insert(
                    d.resolved.clone(),
                    OverwriteExpectation::Digest(from.clone()),
                );
                resources.push(Resource::transition(
                    PATH_KIND,
                    d.display.clone(),
                    from,
                    // The resulting content is not known here — `patch` and
                    // `sed -i` compute it from the prior bytes — and an
                    // unclaimed post-state is exactly what `Unspecified` is
                    // for.
                    kaish_types::approval::StateClaim::Unspecified,
                ));
            }
            self.request_gate(
                operation,
                resources,
                "the fs.* enforce policy is on and this overwrite has no recoverable prior copy",
                confirm_hint(&joined),
                confirm,
            )
            .await
            .map_err(|result| prefix_error(command, result))?;
        }

        // The observe record goes on the log only once the enforce gate has
        // authorized the batch. A batch held at exit 2 never runs, so
        // recording it would claim an operation happened that did not.
        let observed: Vec<kaish_types::approval::ObservedResource> = decided
            .iter()
            .filter_map(|d| {
                d.observed.map(|subscription| {
                    kaish_types::approval::ObservedResource::new(
                        PATH_KIND,
                        d.display.clone(),
                        d.resolved.to_string_lossy(),
                        subscription,
                    )
                })
            })
            .collect();
        self.record_observed(operation, command, observed).await?;

        // Snapshot prior content for every trash-first target before any write,
        // keeping the bytes so a byte-oriented caller can CAS against them.
        for d in &decided {
            if matches!(d.action, MutationAction::TrashFirst) {
                match self.snapshot_for_overwrite(&d.display, &d.resolved).await {
                    Ok(bytes) => {
                        expectations.insert(d.resolved.clone(), OverwriteExpectation::Bytes(bytes));
                    }
                    Err(e) => return Err(ExecResult::failure(1, format!("{command}: {e}"))),
                }
            }
        }
        Ok(expectations)
    }

    /// Copy the prior content of `resolved` into the trash before it's
    /// overwritten, returning those bytes for the caller's compare-and-swap.
    ///
    /// We **copy** (not move): the builtin overwrites the file in place next,
    /// and read-modify-write callers (`patch`, `sed -i`) still need to read it —
    /// the file keeps its identity, only its content changes. (`rm` *moves*
    /// because removal is the op; an overwrite backs up the prior bytes.) Reads
    /// through the backend so a real, overlay, or in-memory file is handled the
    /// same way. A missing trash backend or a trash failure is an error — never
    /// a silent fall-through to a destructive overwrite.
    async fn snapshot_for_overwrite(
        &self,
        display: &str,
        resolved: &Path,
    ) -> Result<Vec<u8>, String> {
        let trash = self
            .trash_backend
            .as_ref()
            .ok_or_else(|| "trash backend not available".to_string())?;
        let bytes = self
            .backend
            .read(resolved, None)
            .await
            .map_err(|e| format!("{display}: {e}"))?;
        trash
            .trash_bytes(Path::new(display), &bytes)
            .await
            .map_err(|e| format!("{display}: trash failed: {e}"))?;
        Ok(bytes)
    }

    /// Overwrite `resolved` with `content`. When `expected` is `Some`, this is a
    /// binary-safe compare-and-swap: the current bytes are re-read and must
    /// equal `expected` (the gate's snapshot), else it errors — a concurrent
    /// change since the gate is a loud conflict, never a silent clobber. Unlike
    /// the `String`-based `PatchOp::Replace` CAS used by `patch`/`sed -i`, this
    /// operates on raw bytes, so binary overwrites (`tee`, `write`, `dd`, `cp`,
    /// `mv`) keep the same protection. It is *not* OS-atomic — a crash mid-write
    /// can still truncate; the atomic write-temp-then-rename primitive remains a
    /// tracked write-model residual.
    pub(crate) async fn overwrite_checked(
        &self,
        resolved: &Path,
        content: &[u8],
        expected: Option<&OverwriteExpectation>,
    ) -> Result<(), String> {
        cas_overwrite(&*self.backend, resolved, content, expected)
            .await
            .map_err(|e| e.to_string())
    }

    /// Expand a glob pattern to matching file paths.
    ///
    /// Returns the matched paths (absolute). Used by builtins that accept glob
    /// patterns in their path arguments (ls, cat, head, tail, wc, etc.).
    pub async fn expand_glob(&self, pattern: &str) -> Result<Vec<PathBuf>, String> {
        use crate::backend_walker_fs::BackendWalkerFs;
        use crate::walker::{EntryTypes, FileWalker, GlobPath, WalkOptions};

        let glob = GlobPath::new(pattern).map_err(|e| format!("invalid pattern: {}", e))?;

        let root = if glob.is_anchored() {
            self.resolve_path("/")
        } else {
            self.resolve_path(".")
        };

        let options = WalkOptions {
            entry_types: EntryTypes::all(),
            respect_gitignore: self.ignore_config.auto_gitignore(),
            ..WalkOptions::default()
        };

        let fs = BackendWalkerFs(self.backend.as_ref());
        let mut walker = FileWalker::new(&fs, &root)
            .with_pattern(glob)
            .with_options(options);

        // Note: if ignore_files contains ".gitignore" AND auto_gitignore is true,
        // the root .gitignore is loaded twice (once here, once by the walker).
        // This is harmless — merge is additive and rules are idempotent.
        if let Some(filter) = self.ignore_config.build_filter(&root, &fs).await {
            walker = walker.with_ignore(filter);
        }

        walker.collect().await.map_err(|e| e.to_string())
    }

    /// Expand positional arguments, resolving glob patterns to relative paths.
    ///
    /// Used by file-processing builtins (cat, head, tail, wc) that accept
    /// glob patterns in their path arguments. Non-string values are converted
    /// to strings (matching shell conventions).
    ///
    /// A `Value::Bytes` operand goes LOUD (GH #93 item 1), and `Value::Json`
    /// (list/record), `Value::Bool`, and `Value::Null` operands go LOUD too
    /// (GH #121) — none is silently dropped by a catch-all anymore. Every
    /// caller here falls back to reading stdin (or a generic "missing path"
    /// error) when the path list comes back empty, so a structured, bool, or
    /// null path used to vanish into a wrong data source instead of erroring.
    /// The match is exhaustive over all 7 `Value` variants on purpose: a
    /// future new variant fails to compile here until handled, rather than
    /// silently falling through a wildcard arm.
    pub async fn expand_paths(&self, positional: &[Value]) -> Result<Vec<String>, String> {
        let mut paths = Vec::new();
        for arg in positional {
            let s = match arg {
                Value::String(s) => s.clone(),
                Value::Int(n) => n.to_string(),
                Value::Float(f) => f.to_string(),
                Value::Bytes(_) => {
                    crate::interpreter::value_to_text_sink_named(arg, "a path").map_err(|e| e.to_string())?
                }
                Value::Json(_) => {
                    return Err(crate::interpreter::structured_boundary_error("a path", arg)
                        .unwrap_or_else(|| "cannot use this value as a path".to_string()));
                }
                Value::Bool(b) => return Err(format!("cannot use a bool ({b}) as a path")),
                Value::Null => return Err("cannot use null as a path".to_string()),
            };
            if crate::glob::contains_glob(&s) {
                let expanded = self.expand_glob(&s).await?;
                let root = self.resolve_path(".");
                for p in expanded {
                    let rel = p.strip_prefix(&root).unwrap_or(&p);
                    paths.push(rel.to_string_lossy().to_string());
                }
            } else {
                paths.push(s);
            }
        }
        Ok(paths)
    }

    /// Default chunk size for forward file scans. Bounds the memory a
    /// scan-oriented builtin holds at once, independent of file size.
    pub const STREAM_CHUNK_SIZE: u64 = 256 * 1024;

    /// Stream a file's bytes forward in `chunk_size` slices, handing each
    /// non-empty chunk to `f`.
    ///
    /// Reads are issued as positional `read_range` requests, so backends slice
    /// without materialising the whole file (LocalFs seeks; MemoryFs/OverlayFs
    /// slice their stored bytes). The loop terminates on the first empty chunk,
    /// which every backend returns once the offset reaches EOF. `f` returns a
    /// [`ControlFlow`](std::ops::ControlFlow): `Break` stops the loop early
    /// (e.g. a consumer that has detected binary content and will discard the
    /// rest), so we don't keep reading a file the caller is done with. This is
    /// the shared engine for scan-oriented builtins (`wc`, `checksum`, `grep`)
    /// that walk a file front-to-back and must not hold it all in memory.
    pub async fn read_file_chunked<F>(
        &self,
        path: &std::path::Path,
        chunk_size: u64,
        mut f: F,
    ) -> kaish_types::backend::BackendResult<()>
    where
        F: FnMut(&[u8]) -> std::ops::ControlFlow<()>,
    {
        use kaish_types::ReadRange;
        let mut offset = 0u64;
        loop {
            let chunk = self
                .backend
                .read(path, Some(ReadRange::bytes(offset, chunk_size)))
                .await?;
            if chunk.is_empty() {
                break;
            }
            offset += chunk.len() as u64;
            if f(&chunk).is_break() {
                break;
            }
        }
        Ok(())
    }
}

/// The kernel's full execution context satisfies the trimmed portable
/// [`ToolCtx`](kaish_tool_api::ToolCtx) contract that out-of-tree tools see.
///
/// Trusted in-tree builtins recover the concrete `ExecContext` (job control,
/// pipes, dispatcher) through
/// [`ToolCtx::as_any_mut`](kaish_tool_api::ToolCtx::as_any_mut).
#[async_trait]
impl kaish_tool_api::ToolCtx for ExecContext {
    fn backend(&self) -> &Arc<dyn KernelBackend> {
        &self.backend
    }

    fn cwd(&self) -> &std::path::Path {
        self.cwd.as_path()
    }

    fn resolve_path(&self, path: &str) -> PathBuf {
        // Inherent methods shadow trait methods in call syntax, so the
        // fully-qualified inherent call here is not recursive.
        ExecContext::resolve_path(self, path)
    }

    fn var(&self, name: &str) -> Option<Value> {
        self.scope.get(name).cloned()
    }

    fn set_var(&mut self, name: &str, value: Value) {
        self.scope.set(name, value);
    }

    fn set_output_format(&mut self, format: OutputFormat) {
        self.output_format = Some(format);
    }

    fn patient(&self, budget: std::time::Duration) -> kaish_tool_api::PatientGuard {
        match &self.watchdog {
            Some(watchdog) => kaish_tool_api::PatientGuard::held(Box::new(watchdog.hold(budget))),
            None => kaish_tool_api::PatientGuard::inert(),
        }
    }

    /// Post an approval request against this context's ledger and run it
    /// through the decision chain (`docs/approval-ledger.md` §C.1, §C.2).
    ///
    /// A context with no ledger returns `Unsupported`, the fail-closed
    /// default. Otherwise the request is posted, decided, and — when a stage
    /// granted — redeemed into a reserved attempt before this returns, so a
    /// tool that gets `Authorized` may proceed immediately.
    ///
    /// `presented` is a plugin's own `--confirm=<token>` value, when its argv
    /// carried one — the same bearer-key path `ExecContext::request_gate`
    /// takes for an in-tree gate site, so both land on the same draft
    /// matcher (`gate`, above).
    async fn request_approval(
        &mut self,
        req: kaish_types::approval::ApprovalRequestDraft,
        presented: Option<&str>,
    ) -> kaish_tool_api::ApprovalOutcome {
        self.gate(req, presented, None).await
    }

    fn approvals(&self) -> kaish_tool_api::Approvals {
        match &self.ledger_access {
            // `PageRequest::default()`'s limit already covers every
            // realistic pending set (spec §D.4's `live_capacity`); a tool
            // reading this snapshot has no cursor of its own to page with.
            Some(access) => kaish_tool_api::Approvals::from_pending(
                access.approvals.pending(kaish_types::approval::PageRequest::default()).items,
            ),
            None => kaish_tool_api::Approvals::empty(),
        }
    }

    /// Report a richer outcome for an attempt **this context reserved**.
    ///
    /// The ownership check is the security boundary, not the handle type:
    /// `settle` has no way to prove who reserved an attempt, so without this
    /// a tool holding any `&mut dyn ToolCtx` could settle another
    /// execution's live attempt by naming its ids. A handle for an attempt
    /// this context did not reserve is refused and traced.
    async fn settle_with(&mut self, attempt: &kaish_tool_api::AttemptHandle, outcome: kaish_types::approval::Outcome) {
        let Some(access) = self.ledger_access.clone() else {
            return;
        };
        let owned = self.attempts.iter().any(|g| {
            g.attempt().request_id() == attempt.request_id()
                && g.attempt().attempt_id() == attempt.attempt_id()
        });
        if !owned {
            tracing::warn!(
                request = %attempt.request_id(),
                attempt = %attempt.attempt_id(),
                "settle_with names an attempt this execution did not reserve — refused"
            );
            return;
        }
        if let Err(err) = access
            .requester
            .settle_by_ids(attempt.request_id(), attempt.attempt_id(), outcome)
            .await
        {
            // Not found / already-terminal are expected races (someone else
            // — the dispatcher's `AttemptGuard` — settled first); anything
            // else is worth a trace so it isn't silently lost.
            tracing::debug!(error = %err, "ToolCtx::settle_with: settle did not apply");
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

/// Normalize a path by resolving `.` and `..` components lexically (no filesystem access).
fn normalize_path(path: &std::path::Path) -> PathBuf {
    let mut parts: Vec<Component> = Vec::new();
    for component in path.components() {
        match component {
            Component::CurDir => {} // skip `.`
            Component::ParentDir => {
                // Pop the last normal component, but don't pop past root
                if let Some(Component::Normal(_)) = parts.last() {
                    parts.pop();
                } else {
                    parts.push(component);
                }
            }
            _ => parts.push(component),
        }
    }
    if parts.is_empty() {
        PathBuf::from("/")
    } else {
        parts.iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::{decide_mutation_action, MutationAction};
    use std::path::Path;

    fn decide(
        trash: bool,
        approvals: bool,
        real: Option<&str>,
        exists: bool,
        append: bool,
    ) -> MutationAction {
        // Default to a small file well under the cap; the size-cap behavior
        // has its own dedicated test below.
        decide_mutation_action(trash, approvals, real.map(Path::new), exists, append, 1, 10_000_000)
    }

    #[test]
    fn new_file_and_append_always_proceed() {
        // Non-existent target: nothing to lose, regardless of gates.
        assert_eq!(decide(true, true, Some("/work/new"), false, false), MutationAction::Proceed);
        // Append to an existing file doesn't destroy prior content.
        assert_eq!(decide(true, true, Some("/work/log"), true, true), MutationAction::Proceed);
    }

    #[test]
    fn trash_wins_over_the_gate_on_an_existing_file() {
        assert_eq!(decide(true, true, Some("/work/f"), true, false), MutationAction::TrashFirst);
        assert_eq!(decide(true, false, Some("/work/f"), true, false), MutationAction::TrashFirst);
    }

    #[test]
    fn the_policy_gates_when_trash_is_off() {
        assert_eq!(decide(false, true, Some("/work/f"), true, false), MutationAction::Gate);
    }

    #[test]
    fn both_gates_off_proceeds() {
        assert_eq!(decide(false, false, Some("/work/f"), true, false), MutationAction::Proceed);
    }

    #[test]
    fn tmp_bypasses_gate_but_real_v_path_stays_gated() {
        // /tmp scratch proceeds even with both gates on (matches rm).
        assert_eq!(decide(true, true, Some("/tmp/scratch"), true, false), MutationAction::Proceed);
        // A *real* path under /v is NOT excluded: mount-coverage routing now
        // delegates unclaimed /v/* to the embedder's backend, so its real
        // content under /v must keep the trash/approval safety net. Trash wins.
        assert_eq!(decide(true, true, Some("/v/cas/blob.bin"), true, false), MutationAction::TrashFirst);
    }

    #[test]
    fn overlay_no_real_path_stays_gated() {
        // No real path (overlay/in-memory) is NOT excluded — still trash-first.
        assert_eq!(decide(true, true, None, true, false), MutationAction::TrashFirst);
        assert_eq!(decide(false, true, None, true, false), MutationAction::Gate);
    }

    #[test]
    fn file_too_big_to_trash_falls_through_like_rm() {
        // Prior content larger than the cap can't be snapshotted, so trash is
        // skipped: the policy gates if on, else the overwrite proceeds unbacked.
        let big = 100u64;
        let cap = 10u64;
        assert_eq!(
            decide_mutation_action(true, true, Some(Path::new("/work/f")), true, false, big, cap),
            MutationAction::Gate
        );
        assert_eq!(
            decide_mutation_action(true, false, Some(Path::new("/work/f")), true, false, big, cap),
            MutationAction::Proceed
        );
        // Exactly at the cap still trashes (inclusive bound, matches rm).
        assert_eq!(
            decide_mutation_action(true, false, Some(Path::new("/work/f")), true, false, cap, cap),
            MutationAction::TrashFirst
        );
    }
}
